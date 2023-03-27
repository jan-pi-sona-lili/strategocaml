open Notty_unix
open Stratego

let () = 
        let t = Term.create () in
        let b = Board.create () in

        let init = {x=0; y=0; turn=`Blue; pressed=None} in
        draw init t b