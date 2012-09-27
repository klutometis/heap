(module heap
  (build-heap!
   heap-data
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
   heapify!)

  (import chicken scheme)

  (use (only aima define-record-and-printer)
       miscmacros
       vector-lib)

  (include "heap-core.scm"))
