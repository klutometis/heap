(define (parent i)
  (- (inexact->exact (floor (/ (+ i 1) 2))) 1))

(define (left i)
  (+ (* 2 i) 1))

(define (right i)
  (+ (* 2 i) 1 1))

(define-record-and-printer heap
  >?
  =?
  inf
  key
  key-set!
  data
  size)

(define (heap-length heap)
  (vector-length (heap-data heap)))

(define (heap-ref heap i)
  (vector-ref (heap-data heap) i))

(define (heap-set! heap i x)
  (vector-set! (heap-data heap) i x))

(define (heap-swap! heap i j)
  (vector-swap! (heap-data heap) i j))

(define (heapify! heap i)
  (let ((heap->? (heap->? heap))
        (heap-key (heap-key heap)))
    (let ((left (left i))
          (right (right i)))
      (let* ((extremum (if (and (< left (heap-size heap))
                                (heap->?
                                 (heap-key (heap-ref heap left))
                                 (heap-key (heap-ref heap i))))
                           left
                           i))
             (extremum (if (and (< right (heap-size heap))
                                (heap->?
                                 (heap-key (heap-ref heap right))
                                 (heap-key (heap-ref heap extremum))))
                           right
                           extremum)))
        (if (not (= extremum i))
            (begin (heap-swap! heap i extremum)
                   (heapify! heap extremum)))))))

(define initial-heap-size (make-parameter 100))

(define make-max-heap
  (case-lambda
   (()
    (make-max-heap car set-car!))
   ((key key-set!)
    (make-max-heap key key-set! (make-vector (initial-heap-size)) 0))
   ((key key-set! data)
    ;; It's always 0 here, isn't it, unless we're passing in a valid
    ;; heap? In which case: use the constructor directly.
    ;;
    ;; Should we build the heap automatically?
    (make-max-heap key key-set! data (vector-length data)))
   ((key key-set! data size)
    (make-heap > = -inf key key-set! data size))))

(define make-min-heap
  (case-lambda
   (()
    (make-max-heap car set-car!))
   ((key key-set!)
    (make-max-heap key key-set! (make-vector (initial-heap-size)) 0))
   ((key key-set! data)
    ;; It's always 0 here, isn't it, unless we're passing in a valid
    ;; heap? In which case: use the constructor directly.
    ;;
    ;; Should we build the heap automatically?
    (make-max-heap key key-set! data (vector-length data)))
   ((key key-set! data size)
    (make-heap < = +inf key key-set! data size))))

(define (build-heap! heap)
  (heap-size-set! heap (vector-length (heap-data heap)))
  (let ((median (inexact->exact (floor (/ (heap-size heap) 2)))))
    ;; Should be i - 1 here?
    (do ((i (sub1 median) (sub1 i)))
        ((negative? i))
      (heapify! heap i))))

(define (heap-extremum heap)
  (heap-ref heap 0))

(define (heap-extract-extremum! heap)
  (if (zero? (heap-size heap))
      (error "Heap underflow -- HEAP-EXTRACT-EXTREMUM!")
      (let ((extremum (heap-extremum heap)))
        (heap-set! heap 0 (heap-ref heap (- (heap-size heap) 1)))
        (heap-size-set! heap (- (heap-size heap) 1))
        (heapify! heap 0)
        extremum)))

(define (heap-change-key! heap i new-key)
  (let ((heap->? (heap->? heap))
        (heap-=? (heap-=? heap))
        (heap-key (heap-key heap)))
    (let ((old-key (heap-key (heap-ref heap i))))
      (if (or (heap->? new-key old-key)
              (heap-=? new-key old-key))
          (begin
            ((heap-key-set! heap) (heap-ref heap i) new-key)
            (do ((i i (parent i)))
                ;; Do we also need to check for (negative? i)?
                ((or (zero? i)
                     (heap->? (heap-key (heap-ref heap (parent i)))
                              (heap-key (heap-ref heap i)))))
            (heap-swap! heap i (parent i))))
          (error "Key violates heap-gradient -- HEAP-CHANGE-KEY!")))))

(define (heap-insert! heap element)
  (let ((heap-size (heap-size heap)))
    (if (= heap-size (heap-length heap))
        ;; Exponential resizing-strategy
        (heap-data-set! heap (vector-resize (heap-data heap)
                                            (* 2 heap-size))))
    (heap-size-set! heap (+ heap-size 1))
    (let ((key ((heap-key heap) element)))
      ((heap-key-set! heap) element (heap-inf heap))
      (heap-set! heap heap-size element)
      (heap-change-key! heap heap-size key))))


(define (heap-delete! heap i)
  ;; Hypothesis
  (let ((heap-size (- (heap-size heap) 1)))
    (if (negative? heap-size)
        (error "Heap underflow -- HEAP-DELETE!")
        (begin
          (heap-size-set! heap heap-size)
          (heap-set! heap i (heap-ref heap heap-size))
          (heapify! heap i)))))
