@(title "Heap")
@(description "Mutable heap with O(1) membership-testing")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(username "klutometis")

(module heap
  @("The heap module provides basic priority queue functions with O(1)
membership-testing, which is useful for implementing e.g. A*.")
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
