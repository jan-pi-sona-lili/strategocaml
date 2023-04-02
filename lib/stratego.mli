type turn = [ `Blue | `Red ]
val not_turn : turn -> turn

type state = { x:int; y:int; turn:turn; pressed:(int*int) option; }

module Board : sig
        type t
        val create : unit -> t
end

val draw : state -> Notty_unix.Term.t -> Board.t -> unit
val update : state -> Notty_unix.Term.t -> Board.t -> unit