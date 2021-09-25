type item =
  [ `Cname
  | `Name
  | `Email
  | `Phone
  | `Loc
  | `Tool
  | `Note
  | `Priv
  | `Other of int ]

val item_to_int : item -> int

val item_of_int : int -> item

type items = { typ : item; length : Cstruct.uint8; payload : Cstruct.t }

type chunk = { ssrc : Cstruct.uint32; items : items }

type t = { header : Header.t; chunks : chunk list }

include S.Packet with type t := t
