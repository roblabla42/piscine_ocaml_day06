(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla <robinlambertz+dev@gmail.c>       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 12:51:51 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/24 13:07:08 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end

module Pair : PAIR = struct let pair = (21, 42) end

module type MAKEFST =
    functor (Pair : PAIR) -> VAL

module MakeFst : MAKEFST = functor (Pair : PAIR) ->
    struct
        let (x, _) = Pair.pair
    end

module type MAKESND =
    functor (Pair : PAIR) -> VAL

module MakeSnd = functor (Pair : PAIR) ->
    struct
        let (_, x) = Pair.pair
    end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
