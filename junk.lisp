(defun atomically-route-outputs (parts wires)
  (dolist (p parts)
    (dolist (output-mev p)
      (dolist (w wires)
        ;; closed-over vars: parts, wires, p, output-mev, w
        (when (and 
               (eq p (part (sender w))) 
               (eq (port output-mev) (port (sender w))))
          (repurpose-mev-and-enqueue (part (receiver w)) (port (receiver w)) output-mev)))
      (clear-outputs p))))

