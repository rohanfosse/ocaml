	(** TODO: Ajouter dans le code au debut :OCTET MAGIQUE
	87 ( base 16) = 10000111
	4A ( base 16) = 01001010
	1F ( base 16) = 00011111
	48 ( base 16) = 01001000
DOOOOOOONE 

	**)

	(*Création du type 'tree' ( Leaf signifie feuille) *)
	type tree = Leaf of int * char 
	| Node of int * tree * tree;;

exception FormatfichierIncorrect;;
exception Erreur;;

	(** CREATION DE VARIABLE TEST **)

	let arbre1 = Node(0,Node(0,Node(0,Leaf (3,'c'),Leaf (4,'d')),Leaf (5,'e')),Node(1,Leaf (6,'e'),Leaf (7,'g')));;

	let arbre2 = Node(1,Node(1,Node(0,Leaf (3,'c'),Leaf (4,'d')),Leaf (5,'e')),Node(1,Leaf (6,'e'),Leaf (7,'g')));;

	let liste1 = [arbre1;arbre2];;

	let liste2 = [arbre2;arbre1];;

	let int_list = [0;1;1;0;1;0];;(*retourne 22 avec la fonction int_list*) 

	let bitwise_comprehension a b =
	(* Sort ce qu'il y a en commum entre la valeur binaire des deux nombres*)
	  Printf.printf "a and b: %d\n" (a land b);
	(*convertit les nombres a et b en binaire,et sort comme valeur 1 si a et/ou b = 1 *)
	  Printf.printf "a or b: %d\n" (a lor b);
	(*comme au dessus, sauf que si a=1 et b=1 alors ca renvoit 0 *)
	  Printf.printf "a xor b: %d\n" (a lxor b);
	(* renvoit -a-1 *)
	  Printf.printf "not a: %d\n" (lnot a);
	(*decale le nombre binaire de a vers la droite, en gros ca rajoute b 0 a la fin de a<, assez pratique *)
	  Printf.printf "a lsl b: %d\n" (a lsl b);  (* left shift by two *)
	(* decale de 1 vers la gauche, donc rajoute des 0 devant.*)
	  Printf.printf "a asr b: %d\n" (a asr b);  (* arithmetic right shift *)
	;;

	(* bitwise exemple 
	# bitwise_comprehension 5 9;;
	a and b: 1
	a or b: 13
	a xor b: 12
	not a: -6
	a lsl b: 2560
	a asr b: 0
	- : unit = ()
	*)


	(** ------------------------------------------------------------------**)




	(*val huffman_val_int : tree -> int = <fun> 
	Donne la premiere valeur de l'entier d'un arbre n ( 1 ou 0)*)
	let huffman_val_int n =
	match n with
	Leaf(i, _) -> i
	| Node (i, _, _) -> i;;

	(* val huffman_comparaison : tree -> tree -> int = <fun>
	Comparaison entre les deux vals entier. 
	retourne 0 si ces deux valeurs sont égales.
	-1 si n2>n1 et 1 sinon.*)

	let huffman_comparaison n1 n2 =
	compare (huffman_val_int n1) (huffman_val_int n2);;

	(* val sort_huffman_trees_list : tree list -> tree list = <fun>
	Range les éléments de la liste dans l'ordre croissant
	(permet de ranger les arbres pour faire un parcours infixe*)
	let sort_huffman_trees_list =
	List.sort huffman_comparaison;;

	(* val make_huffman_tree : tree list -> tree = <fun> 
	chaque feuille represente un caractère dans un texte avec sa frequence
	D'une liste d'arbres, on les concatene pour former un seul arbre. *)
	let make_huffman_tree leaves_list =
	let rec maker lst =
	match lst with
	[] -> Leaf(0, '0') (*Ne devrait jamais arriver*)
	| [hd] -> hd
	| hd1::hd2::tl -> let new_node =
	Node((huffman_val_int hd1 + huffman_val_int hd2),
	hd1, hd2) 
	in
	maker (sort_huffman_trees_list (new_node::tl)) 
	in
	maker (sort_huffman_trees_list leaves_list);;

	(* val initial_huffman_trees_list : (unit -> char) -> tree list = <fun>
	this function makes initial leaves with frequencies for each char.
	func_get_next_char est une function qui retourne un nouveau caractère à chaque appel.
	D'un fichier elle nous crée un tableau d'arbre*)
	let initial_huffman_trees_list func_get_next_char =
	let temp = Hashtbl.create 256 in
	(try
	let c = ref '0' in
	while(true) do
	c := func_get_next_char ();
	let a =
	(try
	Hashtbl.find temp !c;
	with
	_ -> 0) in
	Hashtbl.replace temp !c (a + 1)
	done
	with
	End_of_file -> ());
	(* On ressort une liste du Hashtab*)
	let tmp_chars_list = ref [] in
	Hashtbl.iter (fun c freq ->
	tmp_chars_list := ((Leaf(freq, c))::(!tmp_chars_list))
	) temp;
	!tmp_chars_list;;


	(* val list_to_int : int list -> int = <fun>
	Convertit une liste de byte en son entier correspondant en décimal*)
	let list_to_int lst =
	let rec aux l i =
	match l with
	[] -> 0.
	| hd::tl -> (float_of_int hd) *. (2. ** i) +. (aux tl (i +. 1.))
	in
	int_of_float (aux lst 0.);;

	(*val get_huffman_codes : tree -> (char,int * int) Hashtbl.t = <fun>
	retourne un hashtable de la forme
	key = caractère
	value = le chemin du nombre binaire*)
	let get_huffman_codes huffman_tree =
	let hash = Hashtbl.create 31 in
	let acc = [] in
	let rec aux acc huffman_tree =
	match huffman_tree with
	| Leaf(w, c) -> Hashtbl.add hash c ((list_to_int acc), List.length acc)
	| Node(w, l, r) -> aux (0::acc) l; aux (1::acc) r
	in
	aux acc huffman_tree;
	hash;;


	(* val make_binary : int -> int -> int = <fun> 
	retourne (bits8 mod 255) -1 si bits8 > 255, retourne bits8 sinon  *)
	let make_binary buffer most_sig_bit =
	let mask8 = lnot ((lnot 0) lsl 8) in (* donne 255 *)
	let bits8 = buffer lsr (most_sig_bit - 8) in
	bits8 land mask8;;

	(* exemple :
	# make_binary 26 8;;
	- : int = 26
	# make_binary 278 8;;
	- : int = 22
	# make_binary 278 10;;
	- : int = 69
	 *)  

(** A REFAIRE, JUSTE POUR LES TESTS **)
	let insertion_magique out_chan =
	output_byte out_chan 1;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 1;
	output_byte out_chan 1;

	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 0;


	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 1;
	output_byte out_chan 1;
	output_byte out_chan 1;
	output_byte out_chan 1;

	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 1;
	output_byte out_chan 0;
	output_byte out_chan 0;
	output_byte out_chan 0;;


	(* val prochain_carac : in_channel -> unit -> char = <fun>
	retourne un caractère à chaque appel de la chaine chan *)
	let prochain_carac chan () = input_char chan;;

	(* val compression : string -> string -> unit = <fun>
	 Fonction principale de la compression *)
	let compression f_in_name f_out_name =
	let in_chan = open_in_bin f_in_name in
	let out_chan = open_out_bin f_out_name in
	let huffman_trees_list = initial_huffman_trees_list (prochain_carac in_chan) in
	let huffman_tree = make_huffman_tree huffman_trees_list in
	let hash_codes = get_huffman_codes huffman_tree in
	(* On écrit l'arbre et la taille du fichier (en byte) dans le fichier compressé *)
	insertion_magique out_chan;
	output_byte out_chan 0;(* le fameux 0 qui dit que non, on ne fera pas d'extention *)
	Marshal.to_channel out_chan huffman_tree [];
	Marshal.to_channel out_chan (pos_in in_chan) [];
	seek_in in_chan 0;(* on remonte le fichier*)
	let c = ref '0' in
	let i = ref 0 in
	let buffer = ref 0 in
	(try
	while(true) do
	c := input_char in_chan;
	(* on prend un caractère dans le fichier, et on le cherche dans le Hashtbl*)
	let binary_path, length = Hashtbl.find hash_codes !c in
	(* On ajoute le code du caractère courant dans le buffer
	Tout est représenté en binaire
	on incrémente la taille i du buffer *)
	buffer := !buffer lsl length; (*on rajoute la taille du caractère en binaire en 0*)
	buffer := !buffer lor binary_path;(*on ajoute sa valeur dans le buffer *)
	i := !i + length;(*on augmente la taille de i en conséquence *)
	(* Tant que l'on a un byte à écrire*)
	while(!i > 7) do
	output_byte out_chan (make_binary !buffer !i);
	i := !i - 8;
	(* On enlève le byte que l'on vient d'écrire du buffer *)
	let mask = lnot ((lnot 0) lsl (!i )) in
	buffer := !buffer land mask;
	done
	done;
	with
	End_of_file -> (*S'il reste quelquechose sur le buffer*)
	if !i <> 0 then
	begin
	(* On rajoute des 0 jusqu'à ce que l'on obtienne 8 bits *)
	while(!i < 8) do
	buffer := !buffer lsl 1;(* rajoute un 0 a la fin du nombre binaire *)
	incr i;
	done;
	output_byte out_chan (make_binary !buffer 8);(* on ajoute ce buffer au fichier *)
	end
	);
	close_in in_chan;
	close_out out_chan;
	;;


	(* val push_out : out_channel -> tree -> int64 -> int -> int ref -> int = <fun>
	la fonction prend en paramètre un buffer, et décode autant que possible
	buffer attend un Int64
	nb_decompressed_chars est une reference aux nombres de caractères à décompresser
	retourne le nombre de byte actuellement décodé *)
	let push_out chan tree buffer most_significant_bit nb_decompressed_chars=
	let rec aux t buffer most_significant_bit deep tmpdeep =
	match t with
	Leaf(w, c) -> if !nb_decompressed_chars = 0 then 0
	else
	begin
	decr nb_decompressed_chars;
	output_byte chan (int_of_char c);
	(* Et on recommence au début de l'arbre pour un nouveau codage ! *)
	aux tree buffer most_significant_bit (deep + tmpdeep) 0
	end
	|Node (_, l, r) ->
	if most_significant_bit = -1 then
	(* Si on a plus de byte et que l'on a pas atteint de feuille *)
	deep - 1
	else
	begin
	(* gauche/droite?*)
	match ((Int64.to_int (Int64.shift_right buffer most_significant_bit)) land 1) with
	| 0 -> aux l buffer (most_significant_bit - 1) deep (tmpdeep + 1)
	| _ -> aux r buffer (most_significant_bit - 1) deep (tmpdeep + 1)
	end
	in
	aux tree buffer most_significant_bit 0 0;;(* on relance la fonction *)


	(* val nbr_magique : string -> bool = <fun> 
on lit les 30 octets du fichier compressé, s'ils sont égaux au nombre magique, on revoit vrai et on
 continue la decompression, sinon on renvoit faux*)
	let nbr_magique f_in_name =
	let in_chan = open_in_bin f_in_name in
	let buffer = ref Int64.zero in
	let c = ref 0 in
	let i = ref 0 in
	while(!i<30)do
	c := input_byte in_chan;
	(* on ajoute le byte lu au buffer
	on met à jour la taille du buffer *)
	buffer := Int64.shift_left !buffer 1;(* on rajoute 8 0 *)
	buffer := Int64.logor !buffer (Int64.of_int !c);(* on y rajoute la valeur de c *)
	incr i;
	(*Printf.printf "%d\n" (Int64.to_int !buffer);*)
	done;
	if(Int64.to_int(!buffer) == 567445458)
	then  true else false;;


	(* val decompression_aux : string -> string -> unit = <fun>
	Fonction principale de la décompression.
	toutes les opérations de lecture sont supposés être faites sur 8 bytes. *)
	let decompression_aux f_in_name f_out_name =
	let in_chan = open_in_bin f_in_name in
	let out_chan = open_out_bin f_out_name in
	(* On lit le l'arbre et la taille du fichier ( et le 0 magique)*)
	seek_in in_chan 32;
	let carac = input_byte in_chan in
	let h_tree = Marshal.from_channel in_chan in
	let file_size = Marshal.from_channel in_chan in
	(*c est un nombre à 8 bytes*)
	let c = ref 0 in
	(* buffer contient les bits lu du fichier compressé*)
	let buffer = ref Int64.zero in
	let most_significat_bit = ref (-1) in
	let i = ref 0 in
	let rest_bits = ref 0 in
	let nb_decompressed_chars = ref file_size in
	(try
	while(true) do
	c := input_byte in_chan;
	(* on ajoute le byte lu au buffer
	on met à jour la taille du buffer *)
	buffer := Int64.shift_left !buffer 8;(* on rajoute 8 0 *)
	buffer := Int64.logor !buffer (Int64.of_int !c);(* on y rajoute la valeur de c *)
	most_significat_bit := !most_significat_bit + 8;(* on augmente la taille en conséquence *)
	incr i;
	(*si de la forme 32 bytes*)
	if !i>0 && !i mod 4 = 0 then
	begin
	rest_bits := push_out out_chan h_tree !buffer !most_significat_bit nb_decompressed_chars;
	most_significat_bit := !most_significat_bit - !rest_bits - 1;
	(* on enleve le byte que l'on vient de traiter *)
	let mask = ref (Int64.lognot (Int64.zero)) in
	mask := Int64.shift_left !mask (!most_significat_bit + 1);
	mask := Int64.lognot !mask;
	buffer := Int64.logand !buffer !mask;
	i := 0;
	end
	done
	with
	End_of_file -> (*S'il reste quelquechose sur le buffer, on l'ignore*)
	ignore (push_out out_chan h_tree !buffer (!most_significat_bit) nb_decompressed_chars));
	close_in in_chan;
	close_out out_chan;
	;;

let decompression f_in_name f_out_name = 
	if(nbr_magique f_in_name) then
		decompression_aux f_in_name f_out_name
	else
		raise FormatfichierIncorrect;;

let main() = 
let arg1 = Sys.argv.(1) in
let arg2 = Sys.argv.(2) in
let arg3 = Sys.argv.(3) in
if((compare arg1 "c") == 0) then
compression arg2 arg3
else if((compare arg1 "d") == 0) then
decompression arg2 arg3
else
raise Erreur;;

main();;

	(** exemple d'utilisation :

	compression "text1.txt" "text1.hf";;
	decompression_aux "text1.hf" "text2.txt";;
	ET CA MARCHE
	**)
