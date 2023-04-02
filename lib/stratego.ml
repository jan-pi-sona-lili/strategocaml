open Notty 
open Notty_unix

type turn = [ `Red | `Blue ]
let not_turn = function `Red -> `Blue | `Blue -> `Red

type state = {
        x:int;
        y:int;
        turn:turn;
        pressed:(int*int) option;
}

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

module Tile = struct
        type t = Red of Piece.t | Blue of Piece.t | Water | Empty

        let eq_turn tile (turn:turn)  = 
                match turn, tile with
                | `Blue, Blue _ -> true
                | `Red, Red _ -> true
                | _ -> false

        let unwrap_tile = function
                | Blue x | Red x -> x
                [@@warning "-8"]
end

module Board = struct
        open Tile
        type t = Tile.t array array

        let ( .%() ) board (x,y) = board.(y).(x)
        let ( .%()<- ) board (x,y) e = board.(y).(x) <- e

        let valid_moves board tile x y =
                let open Piece in

                let collides x y = try if board.%(x,y) = Empty then Some (x,y) else None with Invalid_argument _ -> None in

                let rec moves_direction x y dx dy = 
                        let x' = x + dx and y' = y + dy in
                        match collides x' y' with
                        | None -> []
                        | Some x -> x :: moves_direction x' y' dx dy
                in

                let moves_direction' = moves_direction x y in
                match unwrap_tile tile with
                | Scout -> moves_direction' 1 0 @ moves_direction' ~-1 0 @ moves_direction' 0 1 @ moves_direction' 0 ~-1
                | Flag | Bomb -> []
                | _ -> List.concat_map Option.to_list [collides (x+1) y; collides (x-1) y; collides x (y+1); collides x (y-1)]
                        
        let to_img board {x;y;turn;pressed} =
                let to_img turn additionalstyle tile =
                        let top, middle, bottom, style = 
                                match turn, tile with
                                | `Blue, Blue x -> "┌───┐", "│"^Piece.to_string x^"│", "╧═══╧", A.(fg blue)
                                | `Red, Red x   -> "╤═══╤", "│"^Piece.to_string x^"│", "└───┘", A.(fg red)
                                | `Red, Blue _  -> "┌───┐", "│   │", "╧═══╧", A.(fg blue)
                                | `Blue, Red _  -> "╤═══╤", "│   │", "└───┘", A.(fg red)
                                | _, Empty      -> "┌───┐", "│   │", "└───┘", A.(fg lightblack)
                                | _, Water      -> "     ", "     ", "     ", A.(bg blue)
                        in
                        let s = I.string A.(style ++ additionalstyle) in
                        I.(s top <-> s middle <-> s bottom)
                in
                let lightstyle = function 
                        | Red _ -> A.(fg lightred)
                        | Blue _ -> A.(fg lightblue)
                        | Water -> A.(bg blue)
                        | Empty -> A.(fg white)
                in
                let open Array in
                let board_img = map (map (to_img turn A.empty)) board in
                let () = 
                        board_img.%(x,y) <- to_img turn A.(st reverse) board.%(x,y);
                        Option.iter (fun (selx,sely) ->
                                let boardxy = board.%(selx,sely) in
                                let lightstyle = lightstyle boardxy in
                                board_img.%(selx,sely) <- to_img turn lightstyle boardxy;
                                List.iter (fun (x,y) -> board_img.%(x,y) <- to_img turn lightstyle board.%(x,y)) (valid_moves board boardxy selx sely);
                                board_img.%(x,y) <- to_img turn A.(st reverse ++ lightstyle) board.%(x,y)
                        ) pressed
                in
                board_img
                |> map (fold_left I.(<|>) I.empty)
                |> fold_left I.(<->) I.empty 

        let create () =
                let board = Array.make_matrix 10 10 Empty in
                List.iter (fun (x,y) -> board.%(x,y) <- Water) 
                        [2,4; 3,4;   6,4; 7,4;
                         2,5; 3,5;   6,5; 7,5];
                List.iter (fun (x,y,piece) -> board.%(x,y) <- piece) 
                [0,0,Red Piece.Flag; 1,0,Red Piece.Bomb; 2,0,Red Piece.Scout; 0,8,Blue Piece.Sergeant];
                board
end

open Board

let rec draw state t b = 
        Term.image t I.(Board.to_img b state <-> I.string A.empty "q to quit, ←↑↓→ to move, Enter to select the piece" <-> I.string A.empty "Enter again on bright ones to place selected piece"); 
        update state t b 
and update ({x;y;turn;pressed} as state) t b =
        match Term.event t with
        | `Key (`ASCII 'q',_)    -> () 
        | `Key (`Enter, _)       -> 
                let moved = ref false in
                draw {state with pressed=begin
                Option.iter (fun (selx,sely) -> 
                        if List.exists ((=) (x,y)) (valid_moves b b.%(selx,sely) selx sely) 
                        then begin
                                b.%(x,y) <- b.%(selx,sely);
                                b.%(selx,sely) <- Tile.Empty;
                                moved := true
                        end) pressed;
                if Tile.eq_turn b.%(x,y) turn && not !moved then Some (x,y) else None
                end; turn=begin if !moved then not_turn turn else turn end} t b 
        | `Key (`Arrow `Left,_)  -> draw {state with x=max 0 (x-1)} t b
        | `Key (`Arrow `Right,_) -> draw {state with x=min (x+1) 9} t b
        | `Key (`Arrow `Down,_)  -> draw {state with y=min (y+1) 9} t b
        | `Key (`Arrow `Up,_)    -> draw {state with y=max 0 (y-1)} t b
        | `Resize _              -> draw state t b
        | _                      -> update state t b