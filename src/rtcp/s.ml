module type Packet = sig
  type t

  val of_cstruct : Cstruct.t -> t

  val to_cstruct : t -> Cstruct.t

  val packet_type : int

  val equal : t -> t -> bool
end
