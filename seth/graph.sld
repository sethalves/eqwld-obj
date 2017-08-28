
(define-library (seth graph)
  (export make-graph
          graph?
          graph-nodes
          graph-edges
          graph-set-nodes!
          make-node
          node?
          node-edges
          node-set-edges!
          node-value
          node-set-value!
          edge?
          edge-start-node
          edge-set-start-node!
          edge-end-node
          edge-set-end-node!
          edge-other-node
          edge-value
          edge-set-value!
          connect-nodes)
  (import (scheme base)
          (snow assert))
  (begin

    (define-record-type <node>
      (make-node~ edges value)
      node?
      (edges node-edges node-set-edges!)
      (value node-value node-set-value!))


    (define-record-type <edge>
      (make-edge~ start-node end-node value)
      edge?
      (start-node edge-start-node edge-set-start-node!)
      (end-node edge-end-node edge-set-end-node!)
      (value edge-value edge-set-value!))


    (define-record-type <graph>
      (make-graph~ nodes edges)
      graph?
      (nodes graph-nodes graph-set-nodes!)
      (edges graph-edges graph-set-edges!))


    (define (make-graph)
      (make-graph~ (list) (list)))


    (define (make-node graph . maybe-value)
      (snow-assert (graph? graph))
      (let* ((value (if (null? maybe-value) #f (car maybe-value)))
             (node (make-node~ (list) value)))
        (graph-set-nodes! graph (cons node (graph-nodes graph)))
        node))


    (define (connect-nodes graph start-node end-node . maybe-value)
      (snow-assert (graph? graph))
      (snow-assert (node? start-node))
      (snow-assert (node? end-node))
      (let* ((value (if (null? maybe-value) #f (car maybe-value)))
             (edge (make-edge~ start-node end-node value)))
        (graph-set-edges! graph (cons edge (graph-edges graph)))
        (node-set-edges! start-node (cons edge (node-edges start-node)))
        (node-set-edges! end-node (cons edge (node-edges end-node)))))

    (define (edge-other-node edge node)
      (if (eq? (edge-start-node edge) node)
          (edge-end-node edge)
          (edge-start-node edge)))

    ))
