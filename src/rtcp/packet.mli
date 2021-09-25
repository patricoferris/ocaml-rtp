(** {1 RTCP Packets} *)

module Header = Header
(** Generic packet headers *)

module App = App
(** Application Specific Packets *)

module Bye = Bye
(** Goodbye Packet *)

module Rr = Rr
(** Receiver Report Packet *)

module Sr = Sr
(** Sender Report Packet *)

module Sdes = Sdes
(** Sender Description Packet *)

type t =
  [ `App of App.t
  | `Bye of Bye.t
  | `Rr of Rr.t
  | `Sr of Sr.t
  | `Sdes of Sdes.t
  | `Raw of Header.t * Cstruct.t ]

val get_header : t -> Header.t

(** {2 Compound Packet}

    A compund packet is actually a collection of RTCP packets transmitted
    together as a single packet over the underlying protocol.

    The first packet must be an {!Sr} or an {!Rr} packet and a {!Sdes} packet
    must be provided with a [`Cname] to begin associating media *)
module Compound : sig
  type nonrec t = t list

  val rfc3550 : t -> (unit, [ `Msg of string ]) result
  (** [rfc3550 t] checks the validity of the compound packet as specified by
      {{:https://datatracker.ietf.org/doc/html/rfc3550#appendix-A.2} appendix
      A.2}*)
end
