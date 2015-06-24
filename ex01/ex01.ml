(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla <robinlambertz+dev@gmail.c>       +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 00:25:00 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/24 12:51:00 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringHash = struct
    type t = String.t
    let equal x y = String.compare x y = 0
    let hash x =
        let rec inner hash c =
            (hash lsl 5) + hash + (int_of_char c)
        in
        let rec fold_left_str fn acc str i =
            if i >= (String.length str) then
                acc
            else
                fold_left_str fn (fn acc (String.get str i)) str (i + 1)
        in
        fold_left_str inner 5381 x 0
end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k, v) -> StringHashtbl.add ht k v) pairs;
    StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
