(defmacro for-every-part (part-sym parts-list &rest fn)
  `(dolist (,part-sym ,parts-list) ,fn))

(defmacro for-every-output (output-mev-sym part-sym fn)
  `(dolist (,output-mev-sym (output-queue ,part-sym)) ,fn))

(defmacro for-every-wire (wire-sym wires-list fn)
  `(dolist (,wire-sym ,wires-list) ,fn))


