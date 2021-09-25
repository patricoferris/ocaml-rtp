type header
(** H264 header *)

val header_of_cstruct : Cstruct.t -> header

[%%cenum
type typ =
  | SPS_NAL [@id 7]
  | PPS_NAL [@id 8]
  | AUD_NAL [@id 9]
  | ESEQ_NAL [@id 10]
  | ESTREAM_NAL [@id 11]
  | FILLER_NAL [@id 12]
  | STAP_A [@id 24]
  | STAP_B [@id 25]
  | MTAP16 [@id 26]
  | MTAP24 [@id 27]
  | FU_A [@id 28]
  | FU_B [@id 29]
[@@uint8_t]]

type t = { header : header; payload : Cstruct.t }

val nri : header -> int
(** The nal_ref_idc, see
    {{:https://datatracker.ietf.org/doc/html/rfc6184#section-5.3} the specs} for
    the semantics *)

val fzb : header -> int
(** The "forbidden zero bit", should in theory always be zero *)

val typ : header -> typ option * int
(** The type of the NAL unit if we decode it, also returns the integer value *)

val checks : header -> unit
(** Does some ITU checks on the header with assertions *)

val of_cstruct : Cstruct.t -> (t, [ `Msg of string ]) result

val to_cstruct : t -> Cstruct.t

val equal : t -> t -> bool

val pp : t Fmt.t
