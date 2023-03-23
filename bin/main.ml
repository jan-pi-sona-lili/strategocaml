open Notty
open Notty_unix
open Stratego

let () = 
        let t = Term.create () in
        let b = Board.create () in

        let init = {x=0; y=8; turn=`Blue} in
        Term.image t Infix.(Board.to_img b init <-> I.string A.empty "Space to exit, Enter to switch turns, ←↑↓→ to move");
        update init t b