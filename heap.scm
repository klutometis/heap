(module heap
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
