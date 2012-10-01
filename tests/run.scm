(use debug heap test)

(define (figure-6.2)
  (let ((data (list->vector (map list '(16 4 10 14 7 9 3 2 8 1)))))
    (make-max-heap car set-car! cdr data)))

(define (test-figure-6.2 testandum heap)
  (test testandum
        '#((16) (14) (10) (8) (7) (9) (3) (2) (4) (1))
        (heap-data heap)))

(let ((heap (figure-6.2)))
  (heapify!/index heap 1)
  (test-figure-6.2 "heapify!" heap))

(let ((heap (figure-6.2)))
  (build-heap! heap)
  (test-figure-6.2 "build-heap!" heap))

(let ((heap (figure-6.2)))
  (build-heap! heap)
  (test "heap-extremum" '(16)
        (heap-extremum heap))
  (test "heap-extract-extremum! -- extremum" '(16)
        (heap-extract-extremum! heap))
  (test "heap-extract-extremum! -- data"
        '#((14) (8) (10) (4) (7) (9) (3) (2) (1) (1))
        (heap-data heap)))

(define (figure-6.5)
  (let ((data (list->vector (map list '(16 14 10 8 7 9 3 2 4 1)))))
    (make-max-heap car set-car! cdr data)))

(let ((heap (figure-6.5)))
  (heap-change-key!/index heap 8 15)
  (test "heap-change-key!"
        '#((16) (15) (10) (14) (7) (9) (3) (2) (8) (1))
        (heap-data heap)))

(let ((heap (figure-6.5)))
  (heap-insert! heap '(21))
  (test "heap-insert!"
        '#((21)
           (16)
           (10)
           (8)
           (14)
           (9)
           (3)
           (2)
           (4)
           (1)
           (7)
           #f
           #f
           #f
           #f
           #f
           #f
           #f
           #f
           #f)
        (heap-data heap)))

(let ((heap (figure-6.5)))
  (heap-delete!/index heap 4)
  (test "heap-delete!"
        '#((16) (14) (10) (8) (1) (9) (3) (2) (4) (1))
        (heap-data heap)))

(let ((heap (make-max-heap)))
  (heap-insert! heap (cons 1 'a))
  (heap-insert! heap (cons 2 'b))
  (heap-insert! heap (cons 3 'c))
  (test-assert "heap-member?" (heap-member? heap 'a))
  (test "heap-key/datum -- exists" 1 (heap-key/datum heap 'a))
  (test "heap-key/datum -- doesn't exist" #f (heap-key/datum heap 'd))
  (heap-delete!/index heap 0)
  (test-assert "not-heap-member? -- index" (not (heap-member? heap 'c)))
  (heap-delete! heap 'b)
  (test-assert "not-heap-member? -- datum" (not (heap-member? heap 'b))))
