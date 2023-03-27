open Notty 
open Notty_unix

(* whose turn is it? *)
type turn = [ `Red | `Blue ]
let not_turn = function `Red -> `Blue | `Blue -> `Red

type state = {
        x:int;
        y:int;
        turn:turn;
        pressed:(int*int) option;
}

let rev arr =
        let open Array in
        let arr = copy arr in
        let len = length arr in
        for i=0 to len/2 do 
                let temp = arr.(i) in
                arr.(i) <- arr.(len-i-1);
                arr.(len-i-1) <- temp         
        done;
        arr

(* describes all pieces *)
module Piece = struct
        type t = Marshal | General | Colonel | Major | Captain | Lieutenant | Sergeant | Miner | Scout | Spy | Bomb | Flag
        [@@warning "-37"]

        let to_string = 
                function
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

        let eq_turn turn tile = 
                match turn, tile with
                | `Blue, Blue _ -> true
                | `Red, Red _ -> true
                | _ -> false

        let to_img turn invert tile =
                let top, middle, bottom, style = 
                        match turn, tile with
                        | `Blue, Blue x -> "┌───┐", "│"^Piece.to_string x^"│", "╧═══╧", A.(fg blue)
                        | `Red, Red x   -> "┌───┐", "│"^Piece.to_string x^"│", "╧═══╧", A.(fg red)
                        | `Red, Blue _  -> "╤═══╤", "│   │", "└───┘", A.(fg blue)
                        | `Blue, Red _  -> "╤═══╤", "│   │", "└───┘", A.(fg red)
                        | _, Empty      -> "┌───┐", "│   │", "└───┘", A.empty
                        | _, Water      -> "     ", "     ", "     ", A.(bg blue ++ fg blue)
                in
                let s = if invert then I.string A.(style ++ st reverse) else I.string style in
                I.(s top <-> s middle <-> s bottom)
end

(* describes the whole board, which is a matrix of tiles *)
module Board = struct
        type t = Tile.t array array

        let ( .![;..] ) board arr = let [x;y] = Array.to_list arr in board.(y).(x)
        [@@warning "-8"]

        let ( .![;..]<- ) board arr e = let [x;y] = Array.to_list arr in board.(y).(x) <- e
        [@@warning "-8"]

        let to_img board {x;y;turn;_} =
                let open Array in
                let board_img = map (map (Tile.to_img turn false)) board in
                let () = board_img.![x;y] <- Tile.to_img turn true board.![x;y] in
                board_img
                |> map (fold_left I.(<|>) I.empty)
                |> fold_left I.(<->) I.empty 

        let create () =
                let board = Array.make_matrix 10 10 Tile.Empty in
                List.iter (fun (x,y) -> board.(y).(x) <- Tile.Water) 
                        [2,4; 3,4;   6,4; 7,4;
                         2,5; 3,5;   6,5; 7,5];
                board
end

open Board

let rec draw state t b = 
        Term.image t I.(Board.to_img (if state.turn=`Blue then b else rev b) state <-> I.string A.empty "q to quit, Space to switch turns, ←↑↓→ to move"); 
        update state t b 
and update ({x;y;turn;_} as state) t b =
        (match Term.event t with
        | `Key (`ASCII 'q',_)    -> Fun.(const (const ()))
        | `Key (`ASCII ' ',_)    -> draw {state with turn=not_turn turn}
        | `Key (`Enter, _)       -> draw {state with pressed=if Tile.eq_turn turn b.![x;y] then Some (x,y) else None}
        | `Key (`Arrow `Left,_)  -> draw {state with x=max 0 (x-1)}
        | `Key (`Arrow `Right,_) -> draw {state with x=min (x+1) 9}
        | `Key (`Arrow `Down,_)  -> draw {state with y=min (y+1) 9}
        | `Key (`Arrow `Up,_)    -> draw {state with y=max 0 (y-1)}
        | `Resize _              -> draw state
        | _                      -> update state) t b