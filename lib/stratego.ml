open Notty 
open Notty_unix

(* whose turn is it? *)
type turn = [ `Red | `Blue ]
let not_turn = function `Red -> `Blue | `Blue -> `Red

type state = {
        x:int;
        y:int;
        turn:turn;
}

(* include reversal function in Array module *)
module Array = struct 
        include Array

        let rev arr =
                let arr = copy arr in
                let len = length arr in
                for i=0 to len/2 do 
                        let temp = arr.(i) in
                        arr.(i) <- arr.(len-i-1);
                        arr.(len-i-1) <- temp         
                done;
                arr
end


(* describes all pieces *)
module Piece = struct
        type t = Marshal | General | Colonel | Major | Captain | Lieutenant | Sergeant | Miner | Scout | Spy | Bomb | Flag
        [@@warning "-37"]

        let to_string = function
        | Marshal    -> "Mar"
        | General    -> "Gen"
        | Colonel    -> "Col"
        | Major      -> "Maj"
        | Captain    -> "Cap"
        | Lieutenant -> "Ltn"
        | Sergeant   -> "Sgt"
        | Miner      -> "Min"
        | Scout      -> "Sct"
        | Spy        -> "Spy"
        | Bomb       -> "Bom"
        | Flag       -> "Flg"
end

(* describes a tile on the board, which can either be red, blue, water, or empty; plus a function to turn a tile into a Notty image *)
module Tile = struct
        type t = Red of Piece.t | Blue of Piece.t | Water | Empty
        [@@warning "-37"]

        let to_img turn tile invert =
                let top, middle, bottom, style = 
                        match turn, tile with
                        | `Blue, Blue x -> "┌───┐", "│"^Piece.to_string x^"│", "╧═══╧", A.(fg blue)
                        | `Red, Red x   -> "┌───┐", "│"^Piece.to_string x^"│", "╧═══╧", A.(fg red)
                        | `Red, Blue _  -> "╤═══╤", "│   │", "└───┘", A.(fg blue)
                        | `Blue, Red _  -> "╤═══╤", "│   │", "└───┘", A.(fg red)
                        | _, Empty      -> "┌───┐", "│   │", "└───┘", A.empty
                        | _, Water      -> "     ", "     ", "     ", A.(bg blue ++ fg blue)
                in
                let s = I.string (if invert then A.(style ++ st reverse) else style) in
                Infix.(s top <-> s middle <-> s bottom)
end

(* describes the whole board, which is a matrix of tiles *)
module Board = struct
        type t = Tile.t array array

        let to_img board {x;y;turn} =
                let open Array in
                board
                |> mapi (fun y' -> mapi (fun x' e -> Tile.to_img turn e (x=x' && y=y')))
                |> map (fold_left I.(<|>) I.empty)
                |> fold_left I.(<->) I.empty

        let create () =
                let board = Array.make_matrix 10 10 Tile.Empty in
                List.iter (fun (x,y) -> board.(y).(x) <- Water) 
                        [2,4; 3,4;   6,4; 7,4;
                         2,5; 3,5;   6,5; 7,5];
                board
end

let rec draw t b state = Term.image t Infix.(Board.to_img (if state.turn=`Blue then b else Array.rev b) state <-> I.string A.empty "Space to exit, Enter to switch turns, ←↑↓→ to move"); 
                         update t b state
        and update t b ({x;y;turn} as state) =
                match Term.event t with
                | `Key (`ASCII ' ',_)    -> ()
                | `Key (`Enter,_)        -> draw t b {state with turn=not_turn turn}
                | `Key (`Arrow `Left,_)  -> draw t b {state with x=max 0 (x-1)}
                | `Key (`Arrow `Right,_) -> draw t b {state with x=min (x+1) 9}
                | `Key (`Arrow `Down,_)  -> draw t b {state with y=min (y+1) 9}
                | `Key (`Arrow `Up,_)    -> draw t b {state with y=max 0 (y-1)}
                | `Resize _              -> draw t b state
                | _                      -> update t b state
