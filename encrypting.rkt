;Devoir Scheme
;Avril 2019
;Bronagh Carolan, Conor Evans (ERASMUS)

; FONCTIONS

; Section 1

(define (decalDroite ch nb)
  ; renvoie une chaîne de caracteres ch, décalé à droite de la valeur de l'entier nb
  ; chaîne de caractères, entier >=0 -> chaîne de caractères
  (let* ((leng (string-length ch))
         (pivot (remainder nb leng)))
    (string-append (substring ch (- leng pivot) leng)
                   (substring ch 0 (- leng pivot)))))


(define (carDeMemeRang c ch1 ch2)
  ; Si le caractère c est trouvé dans le chaîne de caractères ch1, le caractère de même rang dans le chaîne de caractères ch2
  ; est renvoyé en résultat
  ; Si le caractère c n’est pas trouvé dans ch1, il est renvoyé en résultat de la fonction 
  ; caractère, 2 chaînes de caractères (le même longueur) -> caractère
  (if (appartient? c ch1)
      (carDeMemeRangRec c ch1 ch2)
      c))

(define (carDeMemeRangRec c ch1 ch2)
  ; Si le caractère c est trouvé dans le chaîne de caractères ch1, le caractère de même rang dans le chaîne de caractères ch2
  ; est renvoyé en résultat
  ; caractère(qui est dans ch1), 2 chaînes de caractères (le même longueur) -> caractère
  (if (char=? c (string-ref ch1 0))
      (string-ref ch2 0)
      (carDeMemeRangRec c (fin ch1) (fin ch2))))

; SECTION 2 

(define (appartient? c ch)
  ; renvoie vrai ssi c est dans ch
  ; chaîne de caractères, caractère  -> booléen
  (and (not (string=? ch "")) 
       (or (char=? (string-ref ch 0) c)
           (appartient? c (fin ch)))))

(define (supprDoublons ch)
  ; renvoie le chaîne de caractère ch sans qu'il y ait des caractères qui figurent dans ch plus d'une fois
  ; chaîne de caractères -> chaîne de caractères
  (supprDoublRec ch ""))


(define (supprDoublRec ch1 ch2)
  ; fonction auxiliaire/récursive de supprDoublons qui accole le premier caractère du ch1 actuel à ch2 s'il ne figure pas déjà dans ch2,
  ; renvoyant au final la chaine ch2 dès lors que ch1 est équivalente à la chaîne vide.
  ; 2 chaînes de caractères -> chaîne de caractères
  (cond((string=? ch1 "") ch2)
       ((appartient? (string-ref ch1 0) ch2) (supprDoublRec (fin ch1) ch2))
       (else (supprDoublRec (fin ch1) (string-append ch2 (string (string-ref ch1 0)))))))


(define (groupeEnTete ch1 ch2)
  ; renvoie une chaîne de caractères, sans occurrences multiples, correspondant à l’ensemble des caractères présents
  ; dans les deux chaînes donnés dans leur ordre d’apparition dans la première chaîne, puis la deuxième chaîne.
  ; 2 chaînes de caractères -> chaîne de caractère
  (supprDoublons (string-append ch1 ch2)))

; SECTION 3

(define (lireTexte)
  ; fonction qui sert à lire de n, n >= 0, caractères saisis au clavier un par un jusqu'à ce que l'on tombe sur #\$, où l'on
  ; renvoie une chaîne des caractères saisis jusqu'à ce moment-là
  ; -> chaîne de caractères
  (let ((ch (char-downcase (read-char))))
    (if (char=? ch #\$) ""
        (string-append (string ch) (lireTexte)))))

(define (alphaTrans sym ch)
  ; fonction qui renvoie une chaîne crypté selon la technique d'encryption (str). Si aucune technique d'encryption valide
  ; n'est donnée, un message d'erreur est affiché/
  ; symbole, chaîne de caractères -> chaîne de caractères
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz"))
    (cond((symbol=? 'décalage sym) (decalDroite alphabet (string-length ch)))
         ((symbol=? 'permutation sym) (groupeEnTete ch alphabet))
         ((symbol=? 'combiné sym) (decalDroite (groupeEnTete ch alphabet) (string-length ch))))))


(define (traduit texte source cible)
  ; fonction permettant de traduire un texte encrypté selon l'alphabet source et l'alphabet cible
  ; 3 chaînes de caractères -> chaîne de caractères
  (if (string=? texte "")
      ""
      (string-append (traduitCar (string-ref texte 0) source cible) 
                     (traduit (fin texte) source cible))))


(define (traduitCar c source cible)
  ; fonction auxiliaire qui renvoie le caractère de l'alphabet cible (l'alphabet original avant d'encryption)
  ; qui se trouve à l'indice équivalent de c dans l'alphabet source (l'alphabet crypté)
  ; caractère, 2 chaînes de caractères -> chaîne de caractères contenant un seul caractère
  (let ((index (index c source 0)))
    (if (>= index 0)
        (string (string-ref cible index))
        (string c))))


(define (crypte cle texte sym)
  ; fonction qui renvoie une encryption d'un texte selon une clé et une technique d'encryption (sym)
  ; 2 chaînes de caractères, symbole -> chaîne de caratère
  (let (( alph (if (or (symbol=? sym 'décalage) (symbol=? sym 'permutation)(symbol=? sym 'combiné))
                   (alphaTrans sym cle))))
    (traduit texte "abcdefghijklmnopqrstuvwxyz" alph)))

(define (decrypte cle texte sym)
  ; fonction qui renvoie une décryption d'un texte selon une clé et une technique de décryption (sym)
  ; 2 chaînes de caractères, symbole -> chaîne de caratère
  (let (( alph (if (or (symbol=? sym 'décalage) (symbol=? sym 'permutation)(symbol=? sym 'combiné))
                   (alphaTrans sym cle))))
    (traduit texte alph "abcdefghijklmnopqrstuvwxyz")))

; FONCTIONS AUXILIAIRES

(define (index c ch i)
  ; renvoie l'indice d'un caractère c dans une chaine ch. Si c ne figure pas dans ch, -1 est renvoyé.
  ; caractère, chaîne de caractèreS, entier >= 0 -> entier >= -1
  (cond((string=? ch "") -1 )
       ((char=? (string-ref ch 0) c) i)
       (else (index c (fin ch) (+ i 1)))))

(define (fin ch)
  ;renvoie la chaîne de caractères ch sans son premier caractère
  ;chaîne de caractères -> chaîne de caractères
  (substring ch 1 (string-length ch)))


;PROGRAMME

(display "Souhaitez vous [C]rypter ou [D]écrypter un texte ?")
(define input (read))
(display "Tapez le texte à (de)crypter et terminez par un $ :")
(define texte (lireTexte))
(display "Quelle est la clé de cryptage ?")
(define cle (symbol->string (read)))
(display "Quel type de cryptage (décalage, permutation, combiné) ?")
(define typeCryptage (read))

(if (symbol=? input 'C)
    (begin (display "Résultat du cryptage : ")
           (display (crypte cle texte typeCryptage)))
    (if (symbol=? input 'D)
        (begin (display "Résultat du décryptage : ")
               (display (decrypte cle texte typeCryptage)))
        (display "Le (de)cryptage ne marche pas. Verfier vos données svp!"))) 


