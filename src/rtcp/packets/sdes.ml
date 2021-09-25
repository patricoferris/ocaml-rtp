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

let item_to_int = function
  | `Cname -> 1
  | `Name -> 2
  | `Email -> 3
  | `Phone -> 4
  | `Loc -> 5
  | `Tool -> 6
  | `Note -> 7
  | `Priv -> 8
  | `Other i -> i

let item_of_int = function
  | 1 -> `Cname
  | 2 -> `Name
  | 3 -> `Email
  | 4 -> `Phone
  | 5 -> `Loc
  | 6 -> `Tool
  | 7 -> `Note
  | 8 -> `Priv
  | i -> `Other i

type items = { typ : item; length : Cstruct.uint8; payload : Cstruct.t }

let items_equal a b =
  a.typ = b.typ && a.length = b.length && Cstruct.equal a.payload b.payload

type chunk = { ssrc : Cstruct.uint32; items : items }

let chunk_equal a b = a.ssrc = b.ssrc && items_equal a.items b.items

let chunk_of_cstruct buff =
  let ssrc = Cstruct.BE.get_uint32 buff 0 in
  let typ = Cstruct.get_uint8 buff 4 |> item_of_int in
  let length = Cstruct.get_uint8 buff 5 in
  let payload = Cstruct.sub buff 6 length in
  { ssrc; items = { typ; length; payload } }

let chunk_to_cstruct chunk =
  let buff = Cstruct.create 6 in
  Cstruct.BE.set_uint32 buff 0 chunk.ssrc;
  Cstruct.set_uint8 buff 4 (item_to_int chunk.items.typ);
  Cstruct.set_uint8 buff 5 (Cstruct.length chunk.items.payload);
  Cstruct.append buff chunk.items.payload

type t = { header : Header.t; chunks : chunk list }

let packet_type = 202

let of_cstruct buff =
  let header = Header.of_cstruct buff in
  let buff = Cstruct.(sub buff 4 (length buff - 4)) in
  let len = Cstruct.len buff in
  let rec collect_chunks read chunks = function
    | 0 -> List.rev chunks
    | n ->
        let chunk = chunk_of_cstruct (Cstruct.sub buff read (n - read)) in
        let read = read + chunk.items.length + 2 in
        collect_chunks read (chunk :: chunks) (n - read)
  in
  { header; chunks = collect_chunks 0 [] len }

let to_cstruct t =
  List.map chunk_to_cstruct t.chunks
  |> List.fold_left Cstruct.append Cstruct.empty
  |> Cstruct.append (Header.to_cstruct t.header)

let equal a b = List.for_all2 chunk_equal a.chunks b.chunks
