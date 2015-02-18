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

let run (_,uris) =
  List.iter (fun (subdir, address, csum) ->
    Printf.printf "%s\n%s\n%s\n" subdir
      (Uri.to_string address)
      (match csum with None ->"" | Some c -> c)
  ) uris

let cmd =
  let doc = "print list of upstream distfiles in an OPAM repository" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) prints all the original source distributions tarballs for
        the local $(i,DIR), with one per line.";
    `S "BUGS";
    `P "Report them to via e-mail to <opam-devel@lists.ocaml.org>, or
        on the issue tracker at <https://github.com/avsm/opam-mirror/issues>";
    `S "SEE ALSO";
    `P "$(b,opam)(1)" ]
  in
  Term.(pure run $ uri),
  Term.info "opam-get-mirror-urls" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
