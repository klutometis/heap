(module heap
  (build-heap!
   heap-data
   initial-heap-size
   make-heap
   make-max-heap
   make-min-heap
   heap-change-key!
   heap-change-key!/index
   heap-delete!
   heap-delete!/index
   heap-empty?
   heap-extract-extremum!
   heap-extremum
   heap-index
   heap-insert!
   heap-member?
   heap-membership
   heapify!
   heapify!/index)

  (import chicken scheme)

  (use (only aima define-record-and-printer)
       miscmacros
       srfi-69
       vector-lib)

  (include "heap-core.scm"))
