(load "~/quicklisp/setup.lisp")
(ql:quickload :uiop)  ; if not loaded

(defclass Eh ()
  ((inqueue :initform nil :accessor inqueue)
   (outqueue :initform nil :accessor outqueue)))

(defmethod enqueue-input ((part Eh) mev)
  (setf (inqueue part) (append (inqueue part) (list mev))))

(defmethod dequeue-input ((part Eh))
  (pop (inqueue part)))

(defmethod enqueue-output ((part Eh) mev)
  (setf (outqueue part) (append (outqueue part) (list mev))))

(defmethod dequeue-output ((part Eh))
  (pop (outqueue part)))

(defclass Part-Template ()
  ((name :accessor name :initarg :name)
   (handler :initarg :handler :accessor handler)))

(defmethod install ((template Part-Template))
  (setf (gethash *templates* (name template) template)))

(defclass Leaf-Template (Part-Template)
  ())

(defclass Container-Template (part-template)
  ((handler :initform 'container-handler)
   (children :initarg :children :accessor children)
   (wires :initarg :wires :accessor wires)))

(defclass Part (Eh Part-template)
  ())

(defmethod send ((Part eh) (mev Mevent))
  (enqueue-output eh mev))

(defclass Mevent ()
  ((port :initarg :port :accessor port)
   (payload :initarg :payload :accessor payload)))

(defclass Sender ()
  ((part :initarg :part :accessor part)
   (port :initarg :port :accessor port)))

(defclass Receiver ()
  ((part :initarg :part :accessor part)
   (port :initarg :port :accessor port)))

(defclass Wire ()
  ((direction :initarg :direction :accessor direction)
   (sender :initarg :sender :accessor sender)
   (receiver :initarg :receiver :accessor receiver)))

(defun Hello (eh mev)
  (handler-case 
      (progn
	(send eh "" "Hello" mev))
    (error (condition)
      (send eh "✗" (format t "*** error in Hello: ~A~%" condition)))))
  
(defun Hello-instantiator (reg owner name arg template-data)
  (let ((name-with-id (gensymbol "Hello"))
	(instance-state () ))
    (make-leaf name-with-id owner instance-state arg Hello)))

(defun World (eh mev)
  (handler-case 
      (progn
	(send eh "" "World" mev))
    (error (condition)
      (send eh "✗" (format t "*** error in World: ~A~%" condition)))))

(defun World-instantiator (reg owner name arg template-data)
  (let ((name-with-id (gensymbol "World"))
	(instance-state () ))
    (make-leaf name-with-id owner instance-state arg World)))

(defclass 1then2-State ()
  ((in1 :initform :none :accessor in1)
   (in2 :initform :none :accessor in2)))

(defun Deracer (eh mev)
  (handler-case 
      (progn
	(send eh "" "Deracer" mev))
    (error (condition)
      (send eh "✗" (format t "*** error in Deracer: ~A~%" condition)))))

(defun Deracer-instantiator (reg owner name arg template-data)
  (let ((name-with-id (gensymbol "Deracer"))
	(instance-state (make-instance 'Deracer-State) ))
    (make-leaf name-with-id ownder instance-state arg Deracer)))

(defun Deracer-install (reg)
  (let ((template-graph nil))
    (register-template reg mkTemplate "Deracer" template-graph Deracer-instantiator)))


(defun Main ()
  (Hello-install)
  (World-install)
  (Deracer-install)
  (let ((owner (make-instance 'Container)))
    (multiple-value-bind (part-palette env)
                        (initialize_from_files (first (rest (uiop:command-line-arguments)))  ; argv[1]
					       (subseq (uiop:command-line-arguments) 4)) ; argv[4:]
			(let ((parts-list (list (Hello-instantiator reg owner "Hello" "")
                                                (World-instantiator reg owner "World" "")
                                                (Deracer-instantiator reg owner "Deracer" ""))))
                          (Dispatch parts-list)))))

(defun Dispatch (parent parts-list)
  (loop while (some #'ready-p parts-list)
        do (mapc #'(lambda (part)
                     (let ((mev (pop (inqueue part))))
                       (funcall (handler part) mev)
                       (route-outputs parent part)))
                 parts-list)))
  
(defun ready-p (part)
  (not (null (inqueue part))))

(defun has-output (part)
  (not (null (outqueue part))))

(defun route-outputs (parent part)
  (route parent part outs))

(defun route (parent part out-list)
  (unless (null out-list)
    (let ((out-mev (first out-list)))
    (mapc #'(lambda (wire) (activate-wire parent wire part out-mev) (wires parent))
    (route parent part (cdr out-list))))))

(defun activate-wire (parent wire part mev)
  (let ((from (sender wire)))
    (when (sender-match-p from part mev)
      (let ((new-mev (rewrite-mevent-for-receiver (receiver wire) (payload mev))))
        (enqueue-input new-mev (part (receiver wire)))))))
      
(defun sender-match-p (from part mev)
  (let ((from-part (part from))
        (from-port (port from))
        (mev-port (port mev)))
    (and (eq from-part part) (eq from-port mev-port))))

(defmethod rewrite-mevent-for-receiver ((rcv Receiver) val)
  (make-instance 'Mevent :port (port rcv) :payload val))
