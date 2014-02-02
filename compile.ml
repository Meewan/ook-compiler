(* Début d'include supplémentaire: On charge la gestion étendu des strings*)
#load "str.cma";;

(* Déclaration des paramètres globaux*)
let c_DEFAULT_SRC = "src.ook";
let c_DEFAULT_DEST_BIN = "a.bin";
let c_DEFAULT_DEST_ASM = "a.asm";
let c_ASM_SHARED_LIB = "sharedlib.asm";

(* On décrit une structure d'énumérative pour décrire le code qui est lue.
Nb: A l'exception de l'instructin fin, cette structure est réutilisable pour n'importe quelle langage de type brainfuck *)
type ast = Incrpoint | Decrpoint | Incroct | Decroct | Sortie | Entree | Instr_bcl | Instr_bcl_fin | Fin;

(* Déclaration des exceptions *)
exception SrcNonTrouvee of string;;

(* Fonction chargée de lire un fichier, et renvoie une liste de string, chaque string correspondant à une ligne*)
let lireFichier fichier_source = 
  (*On commence l'ouverture du document*)
  let res = ref ([]) and descripteurDeFichier = open_in fichier_source in
    try
      (*Tant que le curseur n'est pas à la fin ... (c'est à dire qu'une exception n'est pas lancée) *)
      while true; do
	(* On concatène au résultant*)
	res := ( !res @ [(input_line descripteurDeFichier)]);
      done;
      (* On renvoie la ressource demandé à la toute fin*)
       !res;
     with End_of_file ->
      (* Et si justement on arrive à la fin, alors ...*)
      close_in descripteurDeFichier;
      !res;


(* let lireFichier2 fichier_source = 
  let descripteurDeFichier = open_in fichier_source in
  let rec lireLignes descripteur = 
    let line = input_line (descripteur) in 
      try line :: (lireLignes descripteur) with End_of_file -> [] in 
  lireLignes (descripteurDeFichier);;  close_in descripteurDeFichier ;;*)

(* Fonction convertissant une liste de ligne en listesde mot*)
let rec split_line = function
| [] -> []
| a::b -> (Str.split (Str.regexp_string " ") a)@(split_line b);

(* Fonction parsant la liste de mots en AST*)
let rec ook_to_ast = function
| [] ->  print_string "Att: Fin de programme innatendue"; []
| "Ook."::"Ook?"::b -> Incrpoint::(ook_to_ast b)
| "Ook?"::"Ook."::b -> Decrpoint::(ook_to_ast b)
| "Ook."::"Ook."::b -> Incroct::(ook_to_ast b)
| "Ook!"::"Ook!"::b -> Decroct::(ook_to_ast b)
| "Ook!"::"Ook."::b -> Sortie::(ook_to_ast b)
| "Ook."::"Ook!"::b -> Entree::(ook_to_ast b)
| "Ook!"::"Ook?"::b -> Instr_bcl::(ook_to_ast b)
| "Ook?"::"Ook!"::b -> Instr_bcl_fin::(ook_to_ast b)
| "Ook?"::"Ook?"::[] -> Fin::[]
| _::b -> (ook_to_ast b);;

(* Fonction chargeant le bloc d'instruction ASM commun aux programme en DCPU-16.*)
let get_sharedlib_dcpu16 = lireFichier c_ASM_SHARED_LIB;;

(* Fonction chargeant le nom du fichier d'entrée suivant les paramètres du compilateurs*)
let read_src = if (Array.length Sys.argv) > 1 then Sys.argv.(1) else c_DEFAULT_SRC;;

(* Fonction verifiant l'existance du fichierà compiler*)
let check_src = Sys.file_exists ;;

(* Fonction renvoyant l'emplacmeent du fichier à compiler, ou une exception non-rattrapée en cas d'absence. *)
let get_src = 
  let result = read_src in
    if (check_src result) then
      result
    else
      raise (SrcNonTrouvee result) ;;

(* Fonction auxiliaire assurant la conversion de l'AST en Binaire et ASM: Renvoit un couple de liste de chaine de caractère contenant pour la première le code ASM, pour la seconde le code machines*)      
let rec asm_to_bin_helper mon_ast point id_boucle pronfondeur_boucle = match mon_ast with
| Incrpoint::b -> (* Pas d'équivalent direct en asm*) (asm_to_bin_helper b (point +1) id_boucle pronfondeur_boucle)
| Decrpoint::b -> (* Pas d'équivalent direct en asm*) (asm_to_bin_helper b (point -1) id_boucle pronfondeur_boucle)
| Incroct::b -> let tmp = (asm_to_bin_helper b point id_boucle pronfondeur_boucle) and str_point = (string_of_int point) in (("\n ADD ["^ (str_point)  ^"],1 ;")::(fst tmp),(" 0x8BC2 " ^ str_point )::(snd tmp))
| Decroct::b -> let tmp = (asm_to_bin_helper b point id_boucle pronfondeur_boucle) and str_point = (string_of_int point) in (("\n SUB ["^ (str_point)  ^"],1 ;")::(fst tmp),(" 0x8BC3 " ^ str_point )::(snd tmp))
| Sortie::b -> let tmp = (asm_to_bin_helper b point id_boucle pronfondeur_boucle) and str_point = (string_of_int point) in (("\n SET A, ["^ (str_point)  ^"],1 \n JSR [[putchar]];")::(fst tmp),(" 0x7801 " ^ str_point ^ "0x7820 0x0007" )::(snd tmp)) 
| Entree::b -> let tmp = (asm_to_bin_helper b point id_boucle pronfondeur_boucle) and str_point = (string_of_int point) in (("\n JSR [[getChar]] ; \n set ["^ (str_point)  ^"],A;")::(fst tmp),(" 0x7820 0x0006 0x03C1 " ^ str_point )::(snd tmp)) 
| Instr_bcl::b -> (* binaire non implémenté*) let tmp = (asm_to_bin_helper b point (id_boucle) (pronfondeur_boucle+1)) and str_point = (string_of_int point) in (("\n :loop-begin-"^(string_of_int id_boucle)^"-"^(string_of_int pronfondeur_boucle)^" ; \n IFN "^ str_point ^",0; set PC, loop-end-"^(string_of_int id_boucle)^"-"^(string_of_int pronfondeur_boucle)^" ; ")::(fst tmp),("0x0000 0x0000 ")::(snd tmp))
| Instr_bcl_fin::b -> (* binaire non implémenté*) let tmp = (asm_to_bin_helper b point (id_boucle +1) (pronfondeur_boucle-1)) and str_point = (string_of_int point) in (("\n set PC, loop-begin-"^(string_of_int (id_boucle -1))^"-"^(string_of_int (pronfondeur_boucle - 1))^" ; :loop-end-"^(string_of_int id_boucle)^"-"^(string_of_int pronfondeur_boucle)^";")::(fst tmp),("0x0000 0x0000 ")::(snd tmp))
| Fin::[] -> ("set pc,crash"::[],"0x7f81 0x000A"::[])
| _ -> print_string "Att: interpretation non reconnue lors du parsing de l'ast vers "; ([],[]);;

(* Fonction pour amorcer la conversion d*)
let asm_to_bin mon_ast = asm_to_bin_helper mon_ast 0 0 0;; 

 (* Déclaration de la fonction princpale : le code a été modifier pour assurer une meilleurs lisibilité.*)
 let main = 
 (* On charge la path de la source *)
  let src = get_src in
    (* On charge la liste de mots *)
    let pre_abstract = split_line (lireFichier src) in
      (* On convertie la listes de mots en AST*)
      let abstract = ook_to_ast pre_abstract in
       (* On obtient nos deux listes output*)
       let outputs = asm_to_bin abstract;;
      
 
 (* Lancement effectif de la fonction principale*)
 main;;