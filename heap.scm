@(title "Heap")
@(description "Mutable heap with O(1) membership-testing")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(username "klutometis")

(module heap
  @("The heap module provides basic priority queue functions with O(1)
membership-testing, which is useful for implementing e.g. A*.")
  (build-heap!
   make-heap
   heap-adjust-key!
   heap-empty?
   heap-extract-extremum!
   heap-extremum
   heap-insert!
   heap-length
   heap-union!
   heapify!)

  (import chicken scheme)

  (include "heap-core.scm"))
