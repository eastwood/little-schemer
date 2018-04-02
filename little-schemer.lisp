;; Predefined Functions

(defun atom? (x)
  (not (listp x)))

(atom? "s")
(atom? ())

(defun lat? (x)
  (cond
   ((null x) t)
   ((atom? (car x)) (lat? (cdr x)))
   (else nil))) ;; Else statement basically

(defun member? (x lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) x))
    ((member? x (cdr lat)))
    (else nil)))

(null ("tea" "garden"))
(eq (car ("tea")) "tea")
(eq 'tea 'tea) ;; true
(eq "tea" "tea") ;; nil because comparing references? 
(eq (car '(x e a)) 't)

(member? 'tea '(tea garden)) ;; true

(defun rember (x lat)
  (cond
    ((null lat) nil)
    ((eq x (car lat)) (cdr lat)) ;; does the first element match what we're after? then just return the CDR
    ((cons (car lat) (rember x (cdr lat))))))


(rember 'a '(a b))
;; x = a, lat = [a b]
(null '(a b)) ;; nil
(eq a (a)) ;; (b)
;; eval to (b)

(rember 'a '(c b a))
;; x = a, lat = [c b a]
(null '(c b a)) ;; nil
(eq a c) ;; nil
;; next line
(cons c (rember (a (b a)))) ;; cons c (pending)

(rember 'a '(b a))
;; x = a, lat = [b a]
(null '(b a)) ;; nil
(eq a b) ;; false
(cons b (rember (a (a)))) ;; cons b (pending)

(rember 'a '(a))
;; x = a, lat = [a]
;; lat != null
(null (a)) ;; false
(eq a a) ;; true
cdr () ;; ()

;; evaluation up previous call stack
(cons b (rember (a (a)))) ;; (b)
(cons c (rember (a (b a)))) ;; (c b)
(rember 'a '(c b a)) ;; (c b)

;; examples

(rember 'a '(a b c)) ;; (B C)
(rember 'a '(b c a)) ;; (B C)
(rember 'a '(b a c a)) ;; (B C A)
(rember 'a '(a something)) ;; (SOMETHING)

;; wohoo!

(defun firsts (llat)
  (cond
    ((null llat) '())
    ((listp llat)
     (cons (car (car llat))
           (firsts (cdr llat))))))

(firsts '((a b c) (d e f)))
(firsts '(()(a)))


;; insertR
;; Given two atoms (old) and (new) search a list (lat) and if found, insert new after old.

(defun insertR (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat)) (cons old (cons new (cdr lat))))
    (t (cons (car lat) (insertR new old (cdr lat))))))

(insertR 'is 'food '(food good)) ;; food is goo
(insertR 'is 'is '(food good)) ;; food good

(defun insertL (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat)) (cons new lat))
    (t (cons (car lat) (insertL new old (cdr lat))))))

(insertL 'is 'is '(food good)) ;; food good
(insertL 'is 'food '(food good)) ;; is food good
