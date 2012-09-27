@(title "Heap")
@(description "Mutable heap with priority-queue functions and O(1) membership-testing, which is useful for implementing e.g. A*")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(username "klutometis")

(define (parent i)
  (- (inexact->exact (floor (/ (+ i 1) 2))) 1))

(define (left i)
  (+ (* 2 i) 1))

(define (right i)
  (+ (* 2 i) 1 1))

(define-record-and-printer heap
  @("The heap data-structure"
    (>? "Greater-than relation for keys")
    (=? "Equal-to relation for keys")
    (inf "Infinity w.r.t. the inequality `>?'")
    (key "Key-accessor for heap-elements")
    (key-set! "Key-mutator for heap-elements")
    (data "Vector data-store underlying heap")
    (size "Size of the heap as distinct from size of data")
    (@to "heap"))
  >?
  =?
  inf
  key
  key-set!
  data
  size)

(define (heap-empty? heap)
  @("Is the heap empty?"
    (heap "The heap to check")
    (@to "boolean"))
  (zero? (heap-size heap)))

(define (heap-length heap)
  (vector-length (heap-data heap)))

(define (heap-ref heap i)
  (vector-ref (heap-data heap) i))

(define (heap-set! heap i x)
  (vector-set! (heap-data heap) i x))

(define (heap-swap! heap i j)
  (vector-swap! (heap-data heap) i j))

(define (heapify! heap i)
  @("Given a heap-index, reëstablish the heap-property."
    (heap "The heap in which to heapify")
    (i "The element-index to heapify"))
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

(define initial-heap-size
  @("Initial size of the heap data-store; exponentially resized on
overflow.")
  (make-parameter 100))

(define make-max-heap
  @("Make a max-heap."
    (key "Key-accessor for heap-elements")
    (key-set! "Key-mutator for heap-elements")
    (data "Vector data-store underlying heap")
    (size "Size of the heap as distinct from size of data")
    (@to "max-heap"))
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
  @("Make a min-heap."
    (key "Key-accessor for heap-elements")
    (key-set! "Key-mutator for heap-elements")
    (data "Vector data-store underlying heap")
    (size "Size of the heap as distinct from size of data")
    (@to "min-heap"))
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
  @("Heapify the entire data-store."
    (heap "The heap to heapify"))
  (heap-size-set! heap (vector-length (heap-data heap)))
  (let ((median (inexact->exact (floor (/ (heap-size heap) 2)))))
    ;; Should be i - 1 here?
    (do ((i (sub1 median) (sub1 i)))
        ((negative? i))
      (heapify! heap i))))

(define (heap-extremum heap)
  @("Peak at the heap's extremum (min or max)."
    (heap "The heap at which to peek")
    (@to "extreme element"))
  (heap-ref heap 0))

(define (heap-extract-extremum! heap)
  @("Return and delete the heap's extremum (min or max)."
    (heap "The heap from which to extract")
    (@to "extreme element"))
  (if (zero? (heap-size heap))
      (error "Heap underflow -- HEAP-EXTRACT-EXTREMUM!")
      (let ((extremum (heap-extremum heap)))
        (heap-set! heap 0 (heap-ref heap (- (heap-size heap) 1)))
        (heap-size-set! heap (- (heap-size heap) 1))
        (heapify! heap 0)
        extremum)))

(define (heap-change-key! heap i new-key)
  @("Change the key of the ith element to new-key along the
heap-gradient."
    (heap "The heap in which to change")
    (i "The index of the element whose key to change")
    (new-key "The new key to assign to element i"))
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
  @("Insert a new element into the heap."
    (heap "The heap in which to insert")
    (element "The element to be inserted"))
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
  @("Delete the ith element from the heap"
    (heap "The heap from which to delete")
    (i "The index of the element to delete"))
  ;; Hypothesis
  (let ((heap-size (- (heap-size heap) 1)))
    (if (negative? heap-size)
        (error "Heap underflow -- HEAP-DELETE!")
        (begin
          (heap-size-set! heap heap-size)
          (heap-set! heap i (heap-ref heap heap-size))
          (heapify! heap i)))))
