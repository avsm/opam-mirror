(*
 * Copyright (c) 2014-2015 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt
open Printf

module PB = Lterm_progress_bar

(** General utility functions for Lwt-based code *)

let open_file =
  function
  | "-" -> return Lwt_io.stdin
  | file -> Lwt_io.open_file ~mode:Lwt_io.input file

let get_exn s =
  Lwt_stream.get s >>= function
  | None -> fail Not_found
  | Some l -> return l

let rec mkdir_p dir =
  match Sys.file_exists dir with
  | true -> return_unit
  | false ->
    mkdir_p (Filename.dirname dir) >>= fun () ->
    (try Unix.mkdir dir 0o700 
     with Unix.Unix_error (Unix.EEXIST,_,_) -> ());
    return_unit

(** Parse the <pkg,uri,checksum> input format *)
let get_urls file =
  open_file file >|=
  Lwt_io.read_lines >>= fun lines ->
  let rec aux acc =
    Lwt_stream.get lines >>= function
    | None -> return acc
    | Some subdir ->
        (get_exn lines >|= Uri.of_string) >>= fun uri ->
        (get_exn lines >|= function "" -> None | c -> Some c) >>= fun csum ->
        aux ((subdir, uri, csum) :: acc)
  in aux []

let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")

let rec fetch ofile uri checksum pbar =
  let url = Uri.to_string uri in
  Cohttp_lwt_unix.Client.get uri >>= fun (resp,body) ->
  let len =
    match Cohttp.Header.get_content_range (Cohttp.Response.headers resp) with
    | None -> 1000000L (* guess length for progress bar *)
    | Some l -> l in
  let lenread = ref 0L in
  match Cohttp.Response.status resp with 
  | `OK -> (*    eprintf "%s %s\n%!" (yellow "GET:") url; *)
    Lwt_io.with_file ~mode:Lwt_io.output ofile
      (fun oc ->
        Cohttp_lwt_body.to_stream body |>
        Lwt_stream.iter_s (fun b ->
          lenread := Int64.add !lenread (Int64.of_int (String.length b));
          let progress = (Int64.to_float !lenread) /. (Int64.to_float len) in
          PB.update ~progress pbar;
          Lwt_io.write oc b
        )
      )
    >>= fun () ->
    begin match checksum with
    | None -> eprintf "%s No checksum for %s\n%!" (yellow "MD5:") ofile
    | Some c ->
      let md5 = Digest.(to_hex (file ofile)) in
      if md5 <> c then
        eprintf "%s Checksum mismatch for %s. Expected %s, got %s\n%!"
          (red "ERR:") ofile c md5
      else (* eprintf "%s %s\n%!" (green "OK: ") ofile;*) ()
    end;
    return_unit
  | `Moved_permanently | `Found -> begin
      match Cohttp.(Header.get (Response.headers resp) "location") with
      | None -> eprintf "%s Bad Found: %s\n" (red "ERR:") url; return_unit
      | Some loc -> fetch ofile (Uri.of_string loc) checksum pbar
    end
  | c ->
    eprintf "%s %s: %s\n" (red "ERR:") (Cohttp.Code.string_of_status c) url;
    return_unit

let run (_,uris) threads odir =
  Sys.chdir odir;
  Lwt_main.run (
    let pbar = PB.make () in
    let pactive = ref [] in
    let add_pbar t = pactive := t :: !pactive in
    let remove_pbar t = pactive := List.filter (fun x -> not (x == t)) !pactive in
    let total_uris = List.length uris in
    let fin_uris = ref 0 in
    let pool = Lwt_pool.create threads (fun () -> return_unit) in
    let _ = Lwt_engine.on_timer 0.1 true (fun _ ->
      let label = Printf.sprintf "%d/%d" !fin_uris total_uris in
      PB.update ~label ~progress:((float !fin_uris) /. (float total_uris)) pbar;
      async (fun () -> (PB.draw (pbar::!pactive)))
    ) in
    Lwt_list.iter_p (fun (subdir, uri, checksum) ->
        Lwt_pool.use pool (fun () ->
            Lwt.catch (fun () ->
                mkdir_p subdir >>= fun () ->
                let fname = Uri.path uri |> Filename.basename in
                let pbar = PB.make ~label:fname () in
                add_pbar pbar;
                fetch (subdir ^ fname) uri checksum pbar >>= fun () ->
                remove_pbar pbar;
                incr fin_uris;
                return_unit
              ) (fun exn ->
                Printf.eprintf "%s: %s %s\n%!" (red "EXC:") (Uri.to_string uri) (Printexc.to_string exn);
                return_unit
              )
          )
      ) uris)

open Cmdliner

let uri =
  let loc =
    let parse s =
      try `Ok ((s, Lwt_main.run (get_urls s)))
      with _ -> `Error (s ^ " is not a valid OPAM repository Git checkout") in
    parse, fun ppf (p,_) -> Format.fprintf ppf "%s" p
  in
  Arg.(required & pos 0 (some loc) None & info [] ~docv:"PLIST"
         ~doc:"Package list generated by opam-mirror-show-urls")

let parallel =
  let doc = "Number of parallel HTTP threads." in
  Arg.(value & opt int 40 & info ["t"; "threads"] ~docv:"THREADS" ~doc)

let odir =
  let doc = "Output directory to place the distfiles subdirectory in." in
  Arg.(value & opt string "." & info ["o"] ~docv:"OUTPUT_DIR" ~doc)

let cmd =
  let doc = "mirror distfiles into an OPAM repository" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) mirrors all the original source distributions tarballs from
        the package list $(i,PLIST) into the $(i,distfiles/) subdirectory.
        The number of parallel HTTP threads can be configured via the
        $(b,-t) option.";
    `P "The $(i,PLIST) is generated by calling the $(b,opam-mirror-show-urls)
        command against an OPAM repository checkout.  You may specify $(i,-)
        as the $(i,PLIST) to read it from the standard input.";
    `S "BUGS";
    `P "Report them to via e-mail to <opam-devel@lists.ocaml.org>, or
        on the issue tracker at <https://github.com/avsm/opam-mirror/issues>";
    `S "SEE ALSO";
    `P "$(b,opam)(1), $(b,opam-mirror-show-urls)(1)" ]
  in
  Term.(pure run $ uri $ parallel $ odir),
  Term.info "opam-mirror" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
