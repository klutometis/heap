(module heap
  (;; build-heap!
   ;; heap-data
   initial-heap-size
   make-heap
   make-max-heap
   make-min-heap
   heap-change-key!
   heap-delete!
   heap-empty?
   heap-extract-extremum!
   heap-extremum
   heap-insert!
   heap-key
   heap-member?
   ;; heap-membership
   ;; heapify!
   )

  (import chicken scheme)

  (use (only aima define-record-and-printer)
       miscmacros
       srfi-69
       vector-lib)

  (include "heap-core.scm"))
