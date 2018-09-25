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
  let repo = OpamRepositoryBackend.local (OpamFilename.Dir.of_string dir) in
  let packages = OpamRepository.packages_with_prefixes repo in
  OpamPackage.Map.fold
    (fun pkg prefix map ->
      let name = OpamPackage.(Name.to_string (name pkg)) in
      let subdir =
        Printf.sprintf "distfiles/%s/%s.%s/" name name
          (OpamPackage.(Version.to_string (version pkg))) in
      let opam_filename = OpamRepositoryPath.opam (OpamFilename.Dir.of_string dir) prefix pkg in
      try
        let opam_file = OpamFile.OPAM.read opam_filename in
        begin match opam_file.url with
          | Some url ->
            let address = OpamFile.URL.url url |> OpamUrl.to_string in
            let checksum =
              try
                let h =
                  (* TODO: handle all kind of hashes *)
                  List.find (fun hash -> OpamHash.kind hash = `MD5)
                    (OpamFile.URL.checksum url) in
                Some (OpamHash.contents h)
              with Not_found -> None
            in
            (subdir, address, checksum) :: map
        | None ->
          map
        end
      with OpamSystem.File_not_found _ ->
        map
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
      address
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
