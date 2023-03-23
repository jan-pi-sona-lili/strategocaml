type turn = [ `Blue | `Red ]
val not_turn : turn -> turn

type state = { x:int; y:int; turn:turn; }

module Board : sig
        type t
        val to_img : t -> state -> Notty.image
        val create : unit -> t
end

val draw : state -> Notty_unix.Term.t -> Board.t -> unit
val update : state -> Notty_unix.Term.t -> Board.t -> unit