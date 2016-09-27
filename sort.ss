; sort.ss
; Noah Brackenbury, Joey Long, Xingfan Xia
; CS 251, Fall 2016

; Given a two-argument function and a list, write a function that returns a list containing
; the elements of list sorted according to the given predicate.
; (define sort)

; Helper functions:

; simple mergesort in scheme, implemented by splitting the lists not in
; half, but into odd and even indeces. written with help from online references
(define merge-sort
  (lambda (L)
    (if (null? L)
        L
        (if (null? (cdr L))
            L
            (merge-list
             (merge-sort (odd-indeces L))
             (merge-sort (even-indeces L)))))))

(define merge-list
  (lambda (L1 L2)
    (if (null? L2)
        L1
        (if (null? L1)
            L2
            (if (< (car L1) (car L2))
                (cons (car L1) (merge-list (cdr L1) L2))
                (cons (car L2) (merge-list (cdr L2) L1)))))))

(define odd-indeces
  (lambda (L)
    (if (null? L)
        '()
        (if (null? (cdr L))
              (list (car L))
              (cons (car L) (odd-indeces (cdr (cdr L))))))))

(define even-indeces
    (lambda (L)
      (if (null? L)
          '()
          (if (null? (cdr L))
              '()
              (cons (car (cdr L)) (even-indeces (cdr (cdr L))))))))

(merge-sort '(3 4 5 2 3 8 9 70 34 23 12 3 45 34))