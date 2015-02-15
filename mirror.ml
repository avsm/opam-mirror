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

let get_urls dir =
  Sys.chdir dir;
  let repo = OpamRepository.local (OpamFilename.Dir.of_string dir) in
  let packages = OpamRepository.packages_with_prefixes repo in
  OpamPackage.Map.fold
    (fun nv prefix map ->
      let name = OpamPackage.(Name.to_string (name nv)) in
      let subdir =
        Printf.sprintf "distfiles/%s/%s.%s/" name name
          (OpamPackage.(Version.to_string (version nv))) in
      let url_file = OpamPath.Repository.url repo prefix nv in 
      match OpamFilename.exists url_file with
      | true ->
        let file = OpamFile.URL.read url_file in
        let address = fst (OpamFile.URL.url file) |> Uri.of_string in 
        let checksum = OpamFile.URL.checksum file in
        (subdir, address, checksum) :: map
      | false -> map
    ) packages []

open Lwt
open Printf

let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")

let rec mkdir_p dir =
  match Sys.file_exists dir with
  | true -> return_unit
  | false ->
    mkdir_p (Filename.dirname dir) >>= fun () ->
    (try Unix.mkdir dir 0o700 
     with Unix.Unix_error (Unix.EEXIST,_,_) -> ());
    return_unit

let rec fetch ofile uri checksum =
  let url = Uri.to_string uri in
  Cohttp_lwt_unix.Client.get uri >>= fun (resp,body) ->
  match Cohttp.Response.status resp with 
  | `OK ->
    eprintf "%s %s\n%!" (yellow "GET:") url;
    Lwt_io.with_file ~mode:Lwt_io.output ofile
      (fun oc ->
         Cohttp_lwt_body.to_stream body
         |> Lwt_stream.iter_s (Lwt_io.write oc))
    >>= fun () ->
    begin match checksum with
    | None -> eprintf "%s No checksum for %s\n%!" (yellow "MD5:") ofile
    | Some c ->
      let md5 = Digest.(to_hex (file ofile)) in
      if md5 <> c then
        eprintf "%s Checksum mismatch for %s. Expected %s, got %s\n%!"
          (red "ERR:") ofile c md5
      else
        eprintf "%s %s\n%!" (green "OK: ") ofile;
    end;
    return_unit
  | `Moved_permanently | `Found -> begin
      match Cohttp.(Header.get (Response.headers resp) "location") with
      | None -> eprintf "%s Bad Found: %s\n" (red "ERR:") url; return_unit
      | Some loc -> fetch ofile (Uri.of_string loc) checksum
    end
  | c ->
    eprintf "%s %s: %s\n" (red "ERR:") (Cohttp.Code.string_of_status c) url;
    return_unit

let run (_,uris) threads =
  Lwt_main.run (
    let pool = Lwt_pool.create threads (fun () -> return_unit) in
    Lwt_list.iter_p (fun (subdir, uri, checksum) ->
        Lwt_pool.use pool (fun () ->
            Lwt.catch (fun () ->
                mkdir_p subdir >>= fun () ->
                let fname = Uri.path uri |> Filename.basename in
                fetch (subdir ^ fname) uri checksum
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
      try `Ok ((s, get_urls s))
      with _ -> `Error (s ^ " is not a valid OPAM repository Git checkout") in
    parse, fun ppf (p,_) -> Format.fprintf ppf "%s" p
  in
  Arg.(required & pos 0 (some loc) None & info [] ~docv:"DIR"
         ~doc:"Git directory of OPAM repository to mirror distfiles into")

let parallel =
  let doc = "Number of parallel HTTP threads." in
  Arg.(value & opt int 50 & info ["t"; "threads"] ~docv:"THREADS" ~doc)

let cmd =
  let doc = "mirror distfiles into an OPAM repository" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) mirrors all the original source distributions tarballs for
        the local $(i,DIR) into the $(i,distfiles/) subdirectory.
        The number of parallel HTTP threads can be configured via the
        $(b,-t) option.";
    `S "BUGS";
    `P "Report them to via e-mail to <opam-devel@lists.ocaml.org>, or
        on the issue tracker at <https://github.com/avsm/opam-mirror/issues>";
    `S "SEE ALSO";
    `P "$(b,opam)(1)" ]
  in
  Term.(pure run $ uri $ parallel),
  Term.info "opam-mirror" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
