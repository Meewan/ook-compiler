;documentation de l'api:
;les arguments des fonctions passent en A
;les valeurs de retours sont passé en A


SET pc, start
;api adresses
:getchar	;recuperation d'une touche frappe au clavier
DAT getchar_function
:putchar	;impression d'un caractere au clavier
DAT putchar_function
;variables de la lib
:screen_number
DAT 0x0
:screen_adress
DAT 0xF000
:last_screen_char
DAT screen_adress
:keyboard_number
DAT 0x0
; fin du programme (le cpu tourne en boucle en ne faisant plus rien)
:crash
set pc, crash


:start	;debut de l'initialisation
;reset des registres
SET a,0
SET b,0
SET c,0
SET i,0
SET j,0
SET x,0
SET y,0
SET z,0


HWN J ;detection du nombre de peripherique
JSR screen
JSR keyboard
set pc, program
;------------------------------------------------------
;initialisation de l'ecran
;------------------------------------------------------
:screen;initialise la boucle pour l'écran
SET I,0
:screen_loop ;boucle de détection d'ecran prend le nombre de peripherique en J et retourne le numero du premier ecran en I
SET Z,0
HWQ I
IFE b,0x7349
  SET Z,1
IFE a,0xf615
	ADD Z,1
IFE Z,2 ;si le peripherique est un ecran
	SET PC,init_screen
ADD I,1
SET PC,screen_loop

:init_screen    ;initialise l'ecran dont l'adresse est contenu dans le registre I
SET A,0
set B, [screen_adress]
HWI I           ;initialisation de la mémoire de l'ecran (de 0xF000 a 0xf100)
SET A,1
SET B, 0
HWI I           ; initialisation de la font avec la font par defaut
SET A,2
SET B,0
HWI I           ;initialise la palette de couleur avec les couleurs par defaut (normalement seul le blanc sur fond noir est utilise)
SET [screen_number], I
SET PC,POP


;---------------------------------------------------------
;initialisation du clavier
;---------------------------------------------------------
:keyboard
SET I,0
:keyboard_loop ;boucle de détection du clavier prend le nombre de peripherique en J et retourne le numero du premier ecran en I
SET Z,0
HWQ I
IFE b,0x30cf
  SET Z,1
IFE a,0x7406
	ADD Z,1
IFE Z,2 ;si le peripherique est un ecran
	SET PC,init_keyboard
ADD I,1
SET PC,keyboard_loop


:init_keyboard
set [keyboard_number],I
SET PC,POP

;-----------------------------------------------------------------------------------------------------------------------------
;implémentation de l'api
;-----------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------
;recuperation d'un frappe au clavier
;---------------------------------------------------------
:getchar_function
SET A,0						;reinitialisation du buffer de clavier
HWI [keyboard_number]
:getchar_function_keyboard_listening_loop	;on attend qu'un caractere soit tappe
SET A,1						
HWI [keyboard_number]						;recuperation du dernier caractere tappe
IFE C,0										;si il est vide on continu a attendre
	set pc,getchar_function_keyboard_listening_loop
SET A,C										;on prepare la valeur de retours
SET PC,POP

;-----------------------------------------------------------
;impression d'un caractere a l'ecran
;-----------------------------------------------------------
:putchar_function
IFE A,10;si on demande un retour a la ligne
	JSR putchar_function_new_line
IFN A,10
	JSR putchar_function_new_char
SET A, [screen_Adress]
ADD A,384
IFG [last_screen_char],A	;si le dernier caractere est hors de l'écran
	JSR putchar_function_scroll
SET PC,POP


;nouvelle ligne
:putchar_function_new_line
set A,[last_screen_char]
sub A,[screen_adress]	;a contient maintenant l'adresse écrite dans l'écran
mod A,32;b contient maintenant l'abscice du dernier caractere écrit
SET B,32
sub B,A
add [last_screen_char],B
set PC,POP

;nouveau caractere
:putchar_function_new_char
ADD [last_screen_char],1
SET [[last_screen_char]], A
set PC,POP

;scroll d'une ligne vers le bas
:putchar_function_scroll
SET J, [screen_adress]
set I,J
ADD I,32
SET c,384
ADD c,[screen_adress]
:putchar_function_scroll_loop
SET [[I]],[[J]]
ifG I,C
	set PC,POP
set PC,putchar_function_scroll_loop
;-----------------------------------------------------------
;programme compilé
;-----------------------------------------------------------
:program
set pc,crash ;placeholder du programme