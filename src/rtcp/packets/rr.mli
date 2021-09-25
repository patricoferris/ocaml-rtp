(* {1 Receiver Report Packet} *)

type t = { header : Header.t; ssrc : Cstruct.uint32; reports : Report.t list }

include S.Packet with type t := t
