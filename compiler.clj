;;TODO ajouter un typage fort
;;//en coursTODO ajouter la logique avec la syntaxe type C (et donc les boolean)
;;TODO ajouter les operations ternaires type C

(ns example.core
  (:require [instaparse.core :as insta]))


;definition de la grammaire
;(def parser
;  (insta/parser
;   "expr = number | operation | assignement | test
;    assignement =  varName space ':=' space elt
;    operation = <'('>? (elt|varGet) space arithmeticOperator space (elt|varGet) <')'>?
;    test = <'('>? (elt|varGet) space booleanOperator space (elt|varGet) <')'>?
;    varGet = varName
;    <elt> = number | operation |test
;    arithmeticOperator =  ('*' | '/' |'%') / ('+' | '-')
;    booleanOperator = ('==' | '!=' | '>' | '<' | '>=' | '<=') / ('!')
;    <space> = <#'[ ]+'>
;    letter = #'[a-zA-Z]'
;    varName = #'[a-zA-Z]+'
;    number = #'[0-9]+(.[0-9]+)?'"))

;definition de la grammaire
(def parser
  (insta/parser
   "incr = 'Ook. Ook.'
    decr = 'Ook! Ook!'
    reg+ = 'Ook. Ook?'
    reg- = 'Ook? Ook.'
    input = 'Ook. Ook!'
    output = 'Ook! Ook.'
    startWhile = 'Ook! Ook?'
    endWhile = 'Ook? Ook!'
    end = 'Ook? Ook?'
    "))

(parser "Ook? Ook?")



;assignement d'une valeur a une variable
(defn assign [variable value env]
  (assoc env  variable value)
  )

;logique de parsing
(defn transform-options [env]
  {:number #(Long/parseLong %1)
   :varName keyword
   :arithmeticOperator choose-operator
   :varGet  #(env %1)
   :operation #(%2 %1 %3)
   :assignement #(assign %1 %3 env)
   :expr identity})

(defn parse [input env]
  (->> (parser input) (insta/transform (transform-options env))))

(defn simple-parse [input]
  parse [input {}])

;boucle repl
(defn repl [expression]
  (let [env {}]
    (loop [expr (clojure.string/split  expression #"@" ) ;on coupe sur le retour a la ligne (il faut bien un caractere de démarcation)
           env env]
      (if  (empty? expr) env (
                              recur (rest expr)  (
                                                  let [env (parse  (first expr) env)] (do (print env "\n") env)))

    ))))

;pour tester que ça marche
(parse "3 + 2" {})
(repl "a := 3 + 3@b := 6 - 8@c := a + b"); la sortie est dans la console, la méthode ne retourne rien















