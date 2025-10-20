;; ============================================================
;; Fichier : projet_bibliotheque_analyseur_complet.scm
;; Projet : Système de gestion de bibliothèque et mini-analyseur de texte
;; Auteur : Mamadou Yaly
;; Université : Université Numérique Cheikh Amidou Kane
;; Langage : Scheme / Racket
;; Testé sur : OneCompiler (https://onecompiler.com/scheme)
;; ============================================================

;; -----------------------------
;; STRUCTURE D’UN LIVRE
;; -----------------------------
(define (make-livre titre auteur disponible contenu)
  (list (cons 'titre titre)
        (cons 'auteur auteur)
        (cons 'disponible disponible)
        (cons 'contenu contenu)))

(define (titre livre) (cdr (assoc 'titre livre)))
(define (auteur livre) (cdr (assoc 'auteur livre)))
(define (disponible? livre) (cdr (assoc 'disponible livre)))
(define (contenu livre) (cdr (assoc 'contenu livre)))

;; Exemple de bibliothèque
(define bibliotheque
  (list
   (make-livre "Le Chat Mystérieux" "Dupont" #t '(le chat dort sur le canapé))
   (make-livre "Poisson d'Or" "Durand" #f '(le poisson nage dans la mer))
   (make-livre "Regard du Chien" "Dupont" #t '(le chien regarde le chat))
   (make-livre "Forêt Magique" "Diallo" #t '(la forêt est grande et verte))))

;; ============================================================
;; ETAPE 1 : FONCTIONS DE BASE
;; ============================================================

(define (number-of-elements lst)
  (define (aux l acc)
    (if (null? l) acc (aux (cdr l) (+ 1 acc))))
  (aux lst 0))

(define (my-reverse lst)
  (define (aux l acc)
    (if (null? l) acc (aux (cdr l) (cons (car l) acc))))
  (aux lst '()))

(define (member? x lst)
  (cond ((null? lst) #f)
        ((equal? x (car lst)) #t)
        (else (member? x (cdr lst)))))

(define (occur x lst)
  (define (aux l acc)
    (if (null? l)
        acc
        (aux (cdr l)
             (if (equal? x (car l)) (+ 1 acc) acc))))
  (aux lst 0))

;; ============================================================
;; ETAPE 2 : FONCTIONS D'ANALYSE
;; ============================================================

(define (doublons lst)
  (define (aux l acc seen)
    (cond ((null? l) (my-reverse acc))
          (else
           (let ((m (car l)))
             (cond ((member? m seen) (aux (cdr l) acc seen))
                   ((> (occur m lst) 1) (aux (cdr l) (cons m acc) (cons m seen)))
                   (else (aux (cdr l) acc (cons m seen))))))))
  (aux lst '() '()))

(define (unique lst)
  (define (aux l acc)
    (if (null? l)
        (my-reverse acc)
        (let ((m (car l)))
          (if (member? m acc)
              (aux (cdr l) acc)
              (aux (cdr l) (cons m acc))))))
  (aux lst '()))

(define (frequence lst)
  (define (aux l acc)
    (if (null? l)
        (my-reverse acc)
        (let ((m (car l)))
          (let ((found (assoc m acc)))
            (if found
                (aux (cdr l)
                     (map (lambda (p)
                            (if (equal? (car p) m)
                                (cons m (+ 1 (cdr p)))
                                p))
                          acc))
                (aux (cdr l) (cons (cons m 1) acc)))))))
  (aux lst '()))

(define (plus-frequent lst)
  (if (null? lst) ""
      (let ((freqs (frequence lst)))
        (define (aux l max-word max-val)
          (if (null? l) max-word
              (let ((mot (caar l)) (val (cdar l)))
                (if (> val max-val)
                    (aux (cdr l) mot val)
                    (aux (cdr l) max-word max-val)))))
        (aux freqs "" 0))))

;; ============================================================
;; FONCTIONS SUPPLÉMENTAIRES POUR LA BIBLIOTHÈQUE
;; ============================================================

(define (livres-par-auteur auteur-nom biblio)
  (filter (lambda (livre) (equal? (auteur livre) auteur-nom)) biblio))

(define (livres-disponibles biblio)
  (filter (lambda (livre) (disponible? livre)) biblio))

(define (afficher-livre livre)
  (display "Titre      : ") (display (titre livre)) (newline)
  (display "Auteur     : ") (display (auteur livre)) (newline)
  (display "Disponible : ") (display (if (disponible? livre) "Oui" "Non")) (newline)
  (newline))

(define (afficher-biblio biblio)
  (if (null? biblio)
      (display "Aucun livre." )
      (for-each afficher-livre biblio)))

;; ============================================================
;; PROGRAMME PRINCIPAL
;; ============================================================
(define (main)
  (display "=== BIBLIOTHÈQUE ===") (newline) (newline)
  (afficher-biblio bibliotheque)
  (display "=== LIVRES DE L'AUTEUR 'Dupont' ===") (newline) (newline)
  (afficher-biblio (livres-par-auteur "Dupont" bibliotheque))
  (display "=== LIVRES DISPONIBLES ===") (newline) (newline)
  (afficher-biblio (livres-disponibles bibliotheque))
  (newline)
  (display "=== ANALYSE DU PREMIER LIVRE ===") (newline)
  (let ((premier (car bibliotheque)))
    (let ((texte (contenu premier)))
      (display "Titre : ") (display (titre premier)) (newline)
      (display "Texte : ") (display texte) (newline)
      (display "Nombre de mots : ") (display (number-of-elements texte)) (newline)
      (display "Liste inversée : ") (display (my-reverse texte)) (newline)
      (display "Mots doublons : ") (display (doublons texte)) (newline)
      (display "Mots uniques : ") (display (unique texte)) (newline)
      (display "Fréquences : ") (display (frequence texte)) (newline)
      (display "Mot le plus fréquent : ") (display (plus-frequent texte)) (newline)))

  (newline)
  (display "=== FIN DU PROGRAMME ===") (newline))

;; ============================================================
;; EXTENSION : LECTURE DE TEXTE UTILISATEUR
;; ============================================================

(define (split str)
  (define (split-helper s current acc)
    (cond
      ((null? s) (reverse (cons (list->string (reverse current)) acc)))
      ((char-whitespace? (car s))
       (if (null? current)
           (split-helper (cdr s) '() acc)
           (split-helper (cdr s) '() (cons (list->string (reverse current)) acc))))
      (else (split-helper (cdr s) (cons (car s) current) acc))))
  (split-helper (string->list str) '() '()))

(define (analyser-texte-utilisateur)
  (display "Entrez un texte à analyser : ") (flush-output)
  (let* ((input (read-line))
         (mots (split input)))
    (display "Texte analysé : ") (display mots) (newline)
    (display "Nombre de mots : ") (display (number-of-elements mots)) (newline)
    (display "Mots doublons : ") (display (doublons mots)) (newline)
    (display "Mots uniques : ") (display (unique mots)) (newline)
    (display "Fréquences : ") (display (frequence mots)) (newline)
    (display "Mot le plus fréquent : ") (display (plus-frequent mots)) (newline)))

;; ============================================================
;; EXECUTION AUTOMATIQUE
;; ============================================================
(main)
