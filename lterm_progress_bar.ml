(*
 * Copyright (c) 2015 Anil Madhavapeddy <anil@recoil.org>
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

type t = {
  mutable label: string option;
  mutable progress: float;
}

let cols ?(max=58) term =
  LTerm.get_size term >|= fun size ->
  min max size.LTerm_geom.cols 

let progress_bar term {label; progress} =
  let label = match label with None -> "" | Some l -> l in
  cols term >>= fun cols ->
  let progress = min progress 1.0 |> max 0.0 in
  let prog = int_of_float ((float_of_int (cols-1)) *. progress) in
  let open LTerm in
  let open LTerm_text in
  let open LTerm_style in
  let pbuf = Bytes.make prog '=' in
  let pcol = if progress < 0.33 then lred else if progress < 0.66 then lyellow else lgreen in
  let pspace = Bytes.make (cols-prog) ' ' in
  if cols-prog > 0 then Bytes.set pspace (Bytes.length pspace - 1) '|';
  let label = if Bytes.length label > 20 then Bytes.sub label 0 20 else label in
  let label = Printf.sprintf "%-20s" label in
  fprintls term (eval [
      B_reverse true; S label; E_reverse;
      S "|"; B_fg pcol; S pbuf;
      E_fg; B_bold true; S ">"; E_bold;
      S pspace; S "\r"])

let label {label} = label
let progress {progress} = progress

let make ?label ?(progress=0.0) () = {label; progress}

let update ?label ?progress t =
  (match label with None -> () | Some l -> t.label <- Some l);
  (match progress with None -> () | Some p -> t.progress <- p)

let draw ?term tl =
  (match term with None -> Lazy.force LTerm.stdout | Some t -> return t) >>= fun term ->
  Lwt_list.iter_s (progress_bar term) tl >>= fun () ->
  LTerm.move term (List.length tl * (-1)) 0
 
let test () =
  let t =
    Lazy.force LTerm.stdout >>= fun term ->
    let x = ref 0.0 in
    let t1 = make ~label:"foo" () in
    let t2 = make ~label:"bar" () in
    let rec aux t1 t2 =
      cols term >>= fun cols ->
      update ~progress:!x t1;
      update ~progress:(!x+.0.2) t2;
      draw [t1;t2] >>= fun () ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      x := !x +. 0.12;
      aux t1 t2
    in aux t1 t2
  in Lwt_main.run t
