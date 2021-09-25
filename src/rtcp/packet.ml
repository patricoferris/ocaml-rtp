module Header = Header
module App = App
module Bye = Bye
module Rr = Rr
module Sr = Sr
module Sdes = Sdes

type t =
  [ `App of App.t
  | `Bye of Bye.t
  | `Rr of Rr.t
  | `Sr of Sr.t
  | `Sdes of Sdes.t
  | `Raw of Header.t * Cstruct.t ]

let get_header = function
  | `App (t : App.t) -> t.header
  | `Bye (t : Bye.t) -> t.header
  | `Rr (t : Rr.t) -> t.header
  | `Sr (t : Sr.t) -> t.header
  | `Sdes (t : Sdes.t) -> t.header
  | `Raw (header, _) -> header

module Compound = struct
  type nonrec t = t list

  (* Validity: https://datatracker.ietf.org/doc/html/rfc3550#appendix-A.2*)
  let rfc3550 (t : t) =
    let ( >>= ) = Result.bind in
    let check_cname = function
      | `Sdes s ->
          List.exists (fun chunk -> chunk.Sdes.items.typ = `Cname) s.Sdes.chunks
      | _ -> false
    in
    let check_padding = function
      | `Sr s ->
          if s.Sr.header.padding = false then Ok ()
          else Error (`Msg "padding on first packet should be false")
      | `Rr r ->
          if r.Rr.header.padding = false then Ok ()
          else Error (`Msg "padding on first packet should be false")
      | _ -> Error (`Msg "First packet must be receiver or sender report")
    in
    (match t with
    | [] -> Error (`Msg "Empty compound packet")
    | x :: _ -> check_padding x)
    >>= fun () ->
    (if List.for_all (fun t -> (get_header t).version = Rtp.Packet.Two) t then
     Ok ()
    else Error (`Msg "Expected all packets to have version 2"))
    >>= fun () ->
    if List.exists check_cname t then Ok ()
    else Error (`Msg "At least one packet should be a SDES with CNAME")
end
