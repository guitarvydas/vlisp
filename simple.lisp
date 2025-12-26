(declaim (optimize (debug 3) (speed 0) (safety 3)))

(load "~/quicklisp/setup.lisp")
(ql:quickload :uiop)
(ql:quickload :cl-json)

(defparameter *top-level-outputs* nil)

(defclass Part ()
  ((name :accessor name :initarg :name)
   (inqueue :accessor inqueue :initform nil)
   (outqueue :accessor outqueue :initform nil)
   (handler :accessor handler :initarg :handler)))

(defmethod clear-outputs ((p Part))
  (setf (outqueue p) nil))

(defmethod has-input-p ((p Part))
  (not (null (inqueue p))))

(defclass Mevent ()
  ((port :accessor port :initarg :port)
   (payload :accessor payload :initarg :payload)))

(defclass Wire ()
  ((sender :accessor sender :initarg :sender)
   (receiver :accessor receiver :initarg :receiver)))

(defun new-wire (sender-ref receiver-ref)
  (make-instance 'Wire :sender sender-ref :receiver receiver-ref))

(defclass PartPort ()
  ((part :accessor part :initarg :part)
   (port :accessor port :initarg :port)))

(defun wref (part-ref port-id)
  (make-instance 'PartPort :part part-ref :port port-id))

(defun send (eh outport outpayload)
  (enqueue-output eh outport outpayload))

(defun Hello (eh mev)
  (declare (ignore mev))
  (send eh "" "Hello"))

(defun World (eh mev)
  (declare (ignore mev))
  (send eh "" "World"))

(defun enqueue-input (eh iport ipayload)
  (setf (inqueue eh) (append (inqueue eh) (list (make-instance 'Mevent :port iport :payload ipayload)))))

(defun dequeue-input (eh)
  (pop (inqueue eh)))

(defun enqueue-output (eh oport opayload)
  (setf (outqueue eh) (append (outqueue eh) (list (make-instance 'Mevent :port oport :payload opayload)))))

(defun dequeue-output (eh)
  (pop (outqueue eh)))

(defun enqueue-top-level-output (oport opayload)
  (setf *top-level-outputs* (append *top-level-outputs* (list (make-instance 'Mevent :port oport :payload opayload)))))


(defparameter *Deracer-state* (list :none :none))

(defun deracer-send-maybe (eh)
  (when (and
	 (not (eq :none (first *Deracer-state*)))
	 (not (eq :none (second *Deracer-state*))))
    (send eh "" (payload (first *Deracer-state*)))
    (send eh "" (payload (second *Deracer-state*)))
    (setf *Deracer-state* (list :none :none))))


(defun Deracer (eh mev)
  (if (eq 1 (port mev))
      (progn
	(setf (first *Deracer-state*) mev)
	(deracer-send-maybe eh))
      (progn
	(setf (second *Deracer-state*) mev)
	(deracer-send-maybe eh))))

(defun dispatcher (parts wires)
  (loop while (some #'has-input-p parts)
	do (dolist (part parts)
             (when (has-input-p part)
               (funcall (handler part) part (dequeue-input part))
               (route-outputs part wires)))))


(defun route-outputs (p wires)
  (dolist (output-mev (outqueue p))
    (dolist (w wires)
      (when (and 
             (equal p (part (sender w))) 
             (equal (port output-mev) (port (sender w))))
        (repurpose-mev-and-enqueue (part (receiver w)) (port (receiver w)) output-mev)))
    (clear-outputs p)))

(defun repurpose-mev-and-enqueue (part port sender-oriented-mev)
  (if (equal "" part)
      (enqueue-top-level-output port (copy-seq (payload sender-oriented-mev)))
    (enqueue-input part port (copy-seq (payload sender-oriented-mev)))))

(defun output-top-level-outputs ()
  (json:encode-json *top-level-outputs* *standard-output*))


(defun main ()
  (let ((part-deracer (make-instance 'part :handler #'Deracer :name "1then2"))
	(part-world (make-instance 'part :handler #'World :name "World"))
	(part-hello (make-instance 'part :handler #'Hello :name "Hello")))
    (let ((parts-list (list part-deracer part-world part-hello)))
      (let ((wires (list
                    ;; LEGO-ness is enhanced by not hard-wiring connections between parts into the parts themselves
                    ;; parts must output only to their own output queues, then the dispatcher routes the mevents to the intended receivers
                    ;;  based on this wiring list
                    ;; Using this kind of wiring scheme, also, reduces namespace issues and encourages isolation between parts
                    ;; wire == ( (sender sender's-port) (receiver receiver's port) )
                    (new-wire (wref part-hello "") (wref part-deracer 1))
                    (new-wire (wref part-world "") (wref part-deracer 2))
                    (new-wire (wref part-deracer "") (wref "" "")))))
        (enqueue-input part-hello "" "")
        (enqueue-input part-world "" "")
        (dispatcher parts-list wires)
        (output-top-level-outputs)))))


