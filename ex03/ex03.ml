(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla <robinlambertz+dev@gmail.c>       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 13:03:37 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/24 15:12:22 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type FIXED = sig
    type t
    val of_float    : float -> t
    val of_int      : int   -> t
    val to_float    : t     -> float
    val to_int      : t     -> int
    val to_string   : t     -> string
    val zero        : t
    val one         : t
    val succ        : t -> t
    val pred        : t -> t
    val min         : t -> t -> t
    val max         : t -> t -> t
    val gth         : t -> t -> bool
    val lth         : t -> t -> bool
    val gte         : t -> t -> bool
    val lte         : t -> t -> bool
    val eqp         : t -> t -> bool (* physical equality *)
    val eqs         : t -> t -> bool (* structural equality *)
    val add         : t -> t -> t
    val sub         : t -> t -> t
    val mul         : t -> t -> t
    val div         : t -> t -> t
    val foreach     : t -> t -> (t -> unit) -> unit
end

module type MAKETYPE = sig
    val bits : int
end

module type MAKE =
    functor (MakeType : MAKETYPE) -> FIXED

module Make = functor (MakeType : MAKETYPE) ->
    struct
        type t                 = int
        let of_float i         =
            let round v = if (ceil v) -. v < v -. (floor v) then ceil v else floor v in
            int_of_float (round (i *. (float_of_int (1 lsl MakeType.bits))))
        let of_int i           = i lsl MakeType.bits
        let to_int i           = i lsr MakeType.bits
        let to_float i         = float_of_int i /. (float_of_int (1 lsl MakeType.bits))
        let to_string i        = string_of_float (to_float i)
        let zero               = 0
        let one                = of_int 1
        let succ               = ( + ) 1
        let pred x             = x - 1
        let min x y            = if x <= y then x else y
        let max x y            = if x >= y then x else y
        let gth                = ( > )
        let lth                = ( < )
        let gte                = ( >= )
        let lte                = ( <= )
        let eqp                = ( == )
        let eqs                = ( = )
        let add                = ( + )
        let sub                = ( - )
        let mul a b            = (a * b + (1 lsl (MakeType.bits - 1))) lsr MakeType.bits
        let div a b            = ((a lsl MakeType.bits) + (b / 2)) / b
        let rec foreach x y fn = if lte x y then (fn x; foreach (succ x) y fn)
    end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8 in
    print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
    print_endline (Fixed8.to_string (Fixed8.mul x8 y8));
    print_endline (Fixed8.to_string (Fixed8.div x8 y8));
    print_endline (Fixed8.to_string (Fixed8.min x8 y8));
    print_endline (Fixed8.to_string (Fixed8.max x8 y8));
    print_endline (Fixed8.to_string (Fixed8.sub x8 y8));
