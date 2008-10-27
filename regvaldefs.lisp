;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGVALDEFS; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :regvaldefs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-reader)
  (enable-compose-reader))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *spaces* (make-hash-table :test #'equal)))

(defclass space ()
  ((name :accessor space-name :type (or keyword list) :initarg :name)
   (documentation :accessor space-documentation :type string :initarg :documentation)
   (implemented-by :accessor space-implemented-by :type (or space list) :initarg :implemented-by)
   (referrers :accessor space-referrers :initform nil :type list)
   (device# :accessor space-device# :initform (make-hash-table :test 'equal) :type hash-table)
   (devicetype# :accessor space-devicetype# :initform (make-hash-table) :type hash-table)
   (regformat# :accessor space-regformat# :initform (make-hash-table) :type hash-table)
   (reglayout# :accessor space-reglayout# :initform (make-hash-table) :type hash-table)
   (regsetmap# :accessor space-regsetmap# :initform (make-hash-table) :type hash-table)
   (regset# :accessor space-regset# :initform (make-hash-table) :type hash-table)
   (reg# :accessor space-reg# :initform (make-hash-table) :type hash-table)
   (bitfield# :accessor space-bitfield# :initform (make-hash-table) :type hash-table)
   (bitfield-byte# :accessor space-bitfield-byte# :initform (make-hash-table) :type hash-table)))

(defun space-root (space)
  (if (null (space-implemented-by space))
      space
      (space-root (space-implemented-by space))))

(defstruct regformat
  name documentation space bitfields)

(defstruct bitfield
  name doc regformat spec
  (byteval# (make-hash-table) :type hash-table)
  (byterevval# (make-hash-table) :type hash-table))

(defstruct reglayout
  "Maps register names into register structures."
  name documentation space registers)
  
(defstruct reg
  "Defines a formatted register, specified within a reglayout with a selector."
  name documentation layout format selector type ext)

(defstruct (regset (:print-object
		    (lambda (obj stream)
		      (format stream "~@<#<REGSET~; :name ~S :documentation ~S~;>~:@>"
			      (regset-name obj) (regset-documentation obj)))))
  "Augments register layouts with an access method."
  name documentation space layout getter setter pass-register write-only)
  
(defstruct byteval
  name doc value bitfield)

(defclass device ()
  ((id :accessor device-id :type (integer 0))
   (space :accessor device-space :type space)
   (backend :accessor device-backend :type (or null device) :initarg :backend)))

(defun byte-bitmask (byte)
  (dpb -1 byte 0))
  
(defun bytes-bitmask (bytes)
  (reduce (lambda (acc byte) (dpb -1 byte acc)) bytes :initial-value 0))

(defun device-hash-id (device)
  (list (type-of device) (device-id device)))

(defmethod print-object ((space space) stream)
  (format stream "~@<#<SPACE:~;~A ~S implemented-by: ~S regformats: ~S regsets: ~S registers: ~S devices: ~S regsetmap: ~S reglayouts: ~S~;>~:@>"
	  (space-name space)
	  (space-documentation space) (if (listp (space-implemented-by space))
					  (space-implemented-by space)
					  (space-name (space-implemented-by space)))
	  (loop :for x :being :the :hash-values :in (space-regformat# space) :collect (regformat-name x))
	  (loop :for x :being :the :hash-values :in (space-regset# space) :collect (regset-name x))
	  (loop :for x :being :the :hash-values :in (space-reg# space) :collect (reg-name x))
	  (loop :for x :being :the :hash-values :in (space-device# space) :collect (device-hash-id x))
	  (loop :for x :being :the :hash-values :in (space-regsetmap# space)
	     :using (hash-key k) :collect (list k x))
	  (loop :for x :being :the :hash-values :in (space-reglayout# space) :collect (reglayout-name x))))

(defmethod print-object ((device device) stream)
  (labels ((slot (id) (if (slot-boundp device id) (slot-value device id) :unbound-slot)))
    (format stream "~@<#<~;~A-~A backend: ~S~;>~:@>" (type-of device) (slot 'id) (slot 'backend))))

(defun device-insert (space device)
  (lret* ((type (type-of device))
          (id (length (gethash type (space-devicetype# space)))))
    (push device (gethash type (space-devicetype# space)))
    (setf (device-id device) id
          (device-space device) space
          (gethash (list type id) (space-device# space)) device)))

(defun space-device (space type id)
  (let ((bucket (gethash type (space-devicetype# space))))
    (find id bucket :key #'device-id)))

(defmethod initialize-instance :after ((device device) &key space &allow-other-keys)
  (device-insert space device))

(defgeneric space-remove-device (device)
  (:method ((device device))
    (let ((space (device-space device)))
      (remhash (device-hash-id device) (space-device# space))
      (removef (gethash (type-of device) (space-devicetype# space)) device))))

(defun purge-namespace-devices (space)
  (clrhash (space-device# space))
  (clrhash (space-devicetype# space)))

(define-condition bit-notation-condition (error) ())

(define-condition bit-notation-no-space-context-error (bit-notation-condition)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Attempt to use names in a null space context."))))

(define-condition bit-notation-unknown-object-error (bit-notation-condition)
  ((object-type :reader bit-notation-condition-object-type :initarg :object-type)
   (object-name :reader bit-notation-condition-object-name :initarg :object-name)
   (object-container :reader bit-notation-condition-object-container :initarg :object-container))
  (:report (lambda (condition stream)
	     (format stream "Reference to an unknown ~A ~S in space ~S."
		     (string-downcase (string (bit-notation-condition-object-type condition)))
		     (bit-notation-condition-object-name condition)
		     (bit-notation-condition-object-container condition)))))

(defun space (name &key allow-fail)
  "If NAME is non-NIL, try to find a space so named, erring in the case of failure.
     If NAME is NIL, return NIL."
  (let ((space (gethash name *spaces*)))
    (cond ((eq name '*space*)
	   (error 'bit-notation-no-space-context-error))
	  ((and (null space) name (not allow-fail))
	   (error 'bit-notation-unknown-object-error :object-type :space :object-name name :object-container :namespace-root))
	  (t space))))

(defun (setf space) (val name)
  (setf (gethash name *spaces*) val))

(defun list-spaces ()
  (iter (for (k v) in-hashtable *spaces*)
        (collect k)))

(defmacro define-accessor (name hash-accessor object-type container-name-accessor &optional mutator)
  (with-gensyms (object-var container-var name-var)
    `(defun ,name (,container-var ,name-var)
       (let ((,object-var (gethash ,name-var (,hash-accessor ,container-var))))
	 (unless ,object-var
	   (error 'bit-notation-unknown-object-error :object-type ,object-type :object-name ,name-var :object-container (,container-name-accessor ,container-var)))
	 ,(if mutator `(,mutator ,object-var) object-var)))))

(define-accessor regformat space-regformat# :regformat space-name)
(define-accessor register-set space-regset# :register-set space-name)
(define-accessor register-layout space-reglayout# :register-layout space-name)
(define-accessor register space-reg# :register space-name)
(define-accessor bitfield-byte space-bitfield-byte# :bitfield space-name)
(define-accessor bitfield space-bitfield# :bitfield space-name)
(define-accessor bitfield-format space-bitfield# :bitfield space-name bitfield-regformat)
(define-accessor bitfield-documentation space-bitfield# :bitfield space-name bitfield-doc)
(define-accessor byteval bitfield-byteval# :byteval bitfield-name)
  
(defun register-format-field-type (name)
  (format-symbol t "~A-BITFIELD" name))

(defun define-bitfield (regformat name size pos doc &optional bytevals)
  (let* ((spec (byte size pos))
	 (space (regformat-space regformat))
	 (bitfield (make-bitfield :regformat regformat :name name :spec spec :doc doc)))
    (when (gethash name (space-bitfield# space))
      (error "Attempt to redefine as existing bitfield ~S in namespace ~S~%" name (space-name space)))
    (push bitfield (regformat-bitfields regformat))
    (loop :for (value name documentation) :in bytevals
       :do (let ((byteval (make-byteval :name name :doc documentation :bitfield bitfield
								      :value value)))
	     (setf (gethash name (bitfield-byteval# bitfield)) byteval
		   (gethash value (bitfield-byterevval# bitfield)) byteval)))
    (dolist (space (list* space (mapcar #'space (space-referrers space))))
      (setf (gethash name (space-bitfield-byte# space)) spec
	    (gethash name (space-bitfield# space)) bitfield))))

(defun bitfield-wide-p (bitfield)
  (> (car (bitfield-spec bitfield)) 1))

(defun bitfield-enumerated-p (bitfield)
  (plusp (hash-table-count (bitfield-byteval# bitfield))))

;; an ability to pluck in a QUOTE would've been very nice...
(defun define-register-format-notype (space name documentation bitspecs)
  (let ((format (make-regformat :name name :documentation documentation :space space)))
    (mapc [apply [define-bitfield format]] bitspecs)
    (setf (gethash name (space-regformat# space)) format)))

(defmacro define-register-format (&environment env name doc &rest bitspecs)
  `(eval-when (:compile-toplevel :load-toplevel)
     (deftype ,(register-format-field-type name) () `(member ,,@(mapcar #'car bitspecs)))
     (define-register-format-notype (space ,(space-name (space (space-name-context env)))) ',name ,doc ',bitspecs)))
  
(defun define-register (reglayout name selector &key (doc "Undocumented register.") format ext)
  (when (gethash name (space-reg# (reglayout-space reglayout)))
    (error "Attempt to redefine register ~S in namespace ~S."
	   name (space-name (reglayout-space reglayout))))
  (let ((register (make-reg :layout reglayout :name name :selector selector :documentation doc
			    :format (when format (regformat (reglayout-space reglayout) format)) :ext ext
			    :type (register-format-field-type format))))
    (push register (reglayout-registers reglayout))
    (setf (gethash name (space-reg# (reglayout-space reglayout))) register)))

(defun define-register-layout-notype (space name documentation regspecs)
  (let ((reglayout (make-reglayout :name name :space space :documentation documentation)))
    (mapc [apply [define-register reglayout]] regspecs)
    (setf (gethash name (space-reglayout# space)) reglayout)))

(defmacro define-register-layout (&environment env (name doc) &rest defs)
  `(progn
     (deftype ,name () `(member ,,@(mapcar #'first defs)))
     (define-register-layout-notype
	 (space ,(space-name (space (space-name-context env)))) ',name ,doc ',defs)))

(defun register-set-try-claim-register-layout (space regset layout)
  (let* ((regs (loop :for reg :being :the :hash-values :in (space-reg# space)
		  :when (eq (reg-layout reg) layout) :collect reg)))
    (when regs
      (case (gethash (reg-name (first regs)) (space-regsetmap# space))
	((t) nil)
	((nil) (dolist (reg regs)
		 (setf (gethash (reg-name reg) (space-regsetmap# space)) regset)))
	(t (dolist (reg regs)
	     (setf (gethash (reg-name reg) (space-regsetmap# space)) t)))))))
      
(defmacro define-register-set-accessor (&environment env name layout-name accessor doc &key pass-register write-only)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (let* ((space (space ',(space-name (space (space-name-context env)))))
              (layout (register-layout space ',layout-name))
              (home-space (reglayout-space layout))
              (regset (make-regset :name ',name :space home-space :layout layout :documentation ,doc
                                                                                 :write-only ,write-only :pass-register ,pass-register)))
         (setf (gethash ',name (space-regset# home-space)) regset)
         (register-set-try-claim-register-layout home-space regset layout)
         (unless (eq space home-space)
           (setf (gethash ',name (space-regset# space)) regset)
           (register-set-try-claim-register-layout space regset layout))))
     (eval-when (:load-toplevel)
       (let* ((space (space ',(space-name (space (space-name-context env)))))
              (regset (register-set space ',name)))
         (unless (or (fboundp ,accessor)
                     (fboundp (list 'setf ,accessor)))
           (error "No functions available for register set accessor ~S." ',name))
         (when (fboundp ,accessor)
           (setf (regset-getter regset) (fdefinition ,accessor)))
         (when (fboundp (list 'setf ,accessor))
           (setf (regset-setter regset) (fdefinition (list 'setf ,accessor))))))))

(defun register-unambiguous-regset (space regname)
  (let ((spec (gethash regname (space-regsetmap# space))))
    (case spec
      ((nil)
       (when (gethash regname (space-reg# space))
	 (error "Register ~S is hashed in space ~S, but has no associated regset." regname space))
       (error "Unknown register ~S in space ~S." regname space))
      ((t) nil)
      (t spec))))

(defun register-regset-name (space regname disambiguation)
  (declare (type space space) (type symbol regname) (type list disambiguation))
  (regset-name
   (or (register-unambiguous-regset space regname)
       (find (reg-layout (register space regname))
	     (mapcar [register-set space] disambiguation) :key #'regset-layout)
       (error "Ambiguous access method for register ~S space ~S." regname space))))

(defun regset-context (env)
  (multiple-value-bind (val expanded-p) (macroexpand-1 '*regsets* env)
    (when expanded-p val)))

(defmacro with-register-sets (&environment env (&rest rsnames) &body body)
  (if-let ((orphan (find-if-not [register-set (space (space-name-context env))] rsnames)))
	  (error "Reference to an undefined register set ~S." orphan))
  `(symbol-macrolet ((*regsets* ,rsnames))
     ,@body))

(defmacro define-namespace (name &body f)
  (let ((implemented-by (cadr (assoc :implemented-by f)))
	(documentation (cadr (assoc :documentation f))))
    `(eval-when (:compile-toplevel :load-toplevel)
       ,(once-only (name)
		   `(setf (space ,name) (make-instance 'space :documentation ,documentation
						       :name ,name :implemented-by (space ,implemented-by))))

       (symbol-macrolet ((*space* ,name))
	 ,@(mapcar [cons 'define-register-format] (cdr (assoc :register-formats f)))
	 ,@(mapcar [cons 'define-register-layout] (cdr (assoc :register-layouts f)))))))

(defun undefine-space (name)
  (remhash name *spaces*))
  
(defun unify-namespaces (names)
  (let* ((spaces (mapcar #'space names))
	 (unispace (make-instance 'space :name names
					 :documentation (format nil "Unified namespace of:~{ ~S~}."
								(mapcar #'space-documentation spaces))
					 :implemented-by spaces)))
    (labels ((check-import-unispace (accessor-name key val)
	       (let* ((accessor-fn (fdefinition accessor-name))
		      (hash-table (funcall accessor-fn unispace)))
		 (when (gethash key hash-table)
		   (error "Namespace conflict while trying to unify namespaces ~S: accessor ~S, key ~S." names accessor-name key))
		 (setf (gethash key hash-table) val))))
      (dolist (space spaces)
	(pushnew names (space-referrers space) :test #'equal)
	(dolist (accessor-name '(space-regset# space-reg# space-regformat# space-bitfield# space-bitfield-byte#
				 space-reglayout# space-regsetmap#))
	  (let ((checker-importer (curry #'check-import-unispace accessor-name)))
	    (maphash checker-importer (funcall (fdefinition accessor-name) space))))))
    (setf (space names) unispace)))

(defun space-name-context (env)
  (let ((space-name (macroexpand-1 '*space* env)))
    (unless space-name
      (error 'bit-notation-no-space-context-error))
    space-name))

(defmacro with-namespaces ((&rest nsnames) &body body)
  
  (let* ((need-unification (> (length nsnames) 1))
         (name (if need-unification nsnames (first nsnames))))
    (when-let ((orphan (find-if-not #'space nsnames)))
      (error "reference to an undefined namespace ~S" orphan))
    `(progn
       ,@(when need-unification
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (symbol-macrolet ((*space* ,name))
         ,@body))))

(defmacro set-namespace (&rest nsnames)
  (let* ((need-unification (> (length nsnames) 1))
	 (name (if need-unification nsnames (first nsnames))))
    (if-let ((orphan (find-if-not #'space nsnames)))
	    (error "reference to an undefined namespace ~S" orphan))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when need-unification
	       `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (define-symbol-macro *space* ,name))))

(defun bitfield-decode (bitfield value &key (symbolise-unknowns t))
  (declare (type bitfield bitfield) (type (unsigned-byte 32) value))
  (cond ((bitfield-enumerated-p bitfield)
	 (let* ((val (ldb (bitfield-spec bitfield) value)))
	   (if-let ((field (gethash val (bitfield-byterevval# bitfield))))
		   (byteval-name field)
                   (if symbolise-unknowns
                       (format-symbol :keyword "UNKNOWN-VALUE-~B" val)
                       val))))
	((bitfield-wide-p bitfield)
	 (ldb (bitfield-spec bitfield) value))
	(t
	 (ldb-test (bitfield-spec bitfield) value))))

(defun format-decode (format value &key (symbolise-unknowns t))
  (declare (type regformat format) (type (unsigned-byte 32) value))
  (iter (for bitfield in (regformat-bitfields format))
        (collect (cons (bitfield-name bitfield) (bitfield-decode bitfield value :symbolise-unknowns symbolise-unknowns)))))

(defun bytenames-ensure-same-register (space regname bytenames)
  "Deduce register difference from register format difference and signal an error, if any."
  (let ((fmt (if regname (reg-format (register space regname))
		 (bitfield-format space (first bytenames)))))
    (if-let ((stray (find-if-not {[eq fmt] [bitfield-format space]} bytenames)))
	    (error "Ambiguous multiregister access: ~S vs. ~S:~S"
		   (regformat-name fmt) (regformat-name (bitfield-format space stray)) stray))))

(defstruct (environment (:conc-name env-))
  space-name bitfield-name
  (bind# (make-hash-table) :type hash-table))

(defparameter *null-environment* (make-environment))

(defun mkenv (space-name bitfield-name)
  (make-environment :space-name space-name :bitfield-name bitfield-name
		    :bind# (if (or (null space-name) (null bitfield-name)) (make-hash-table)
			       (bitfield-byteval# (bitfield (space space-name) bitfield-name)))))
  
(defun load-time-env (env)
  (let ((env (or env *null-environment*)))
    `(load-time-value (mkenv ',(env-space-name env) ,(env-bitfield-name env)))))

(defun var (env name)
  (declare (type symbol name) (type (or null environment) env))
  (let ((env (or env *null-environment*)))
    (multiple-value-bind (val exist-p) (gethash name (env-bind# env))
      (unless exist-p
	(error "Unbound var ~S in ~S." name env))
      (dpb (byteval-value val) (bitfield-spec (byteval-bitfield val)) 0))))

;; this is _insanely_ error-prone... (though easy to control, since its uses are limited)
(defun val (env val)
  (declare (type (or null environment) env) (type (unsigned-byte 32) val))
  (if (and env (env-space-name env) (env-bitfield-name env))
      (let* ((space (space (env-space-name env)))
	     (bitfield (bitfield space (env-bitfield-name env))))
	(dpb val (bitfield-spec bitfield) 0))
      val))

(defmacro decode-context-with-bitfields ((spacename &optional regset-want space-want bitfield fmtname) regname bytenames env &body body)
  (let ((space (or space-want (gensym))) (regset (and regname regset-want)) (newbytenames (gensym)))
    `(let* ((,spacename (space-name-context ,env)) (,space (space ,spacename))
            (,newbytenames (ensure-list ,bytenames))
            ,@(when bitfield `((,bitfield (bitfield ,space (car ,newbytenames)))))
            ,@(when regset `((,regset (register-regset-name ,space ,regname (regset-context ,env)))))
            ,@(when fmtname `((,fmtname (and ,regname (if-let ((format (reg-format (register ,space ,regname))))
                                                              (regformat-name format)))))))
       (declare (ignorable ,space ,@(when bitfield `(,bitfield)) ,@(when fmtname `(,fmtname)) ,@(when regset `(,regset)) ,newbytenames))
       (bytenames-ensure-same-register ,space ,regname ,newbytenames)
       ,@(when regset
               `((unless (or (register-unambiguous-regset ,space ,regname)
                             (null (regset-context ,env))
                             (member (regset-name (register-set ,space ,regset)) (regset-context ,env)))
                   (error "regset context compilation error: mapped ~S to ~S, while ~S were available"
                          ,regname (regset-name (register-set ,space ,regset)) (regset-context ,env)))))
       ,@body)))

(defmacro decode-context-without-bitfields ((spacename &optional regset-want space-want fmtname) regname env &body body)
  (let ((space (or space-want (gensym))) (regset (and regname regset-want)))
    `(let* ((,spacename (space-name-context ,env)) (,space (space ,spacename))
            ,@(when regset `((,regset (register-regset-name ,space ,regname (regset-context ,env)))))
            ,@(when fmtname `((,fmtname (and ,regname (if-let ((format (reg-format (register ,space ,regname))))
                                                              (regformat-name format)))))))
       (declare (ignorable ,space ,@(when fmtname `(,fmtname)) ,@(when regset `(,regset))))
       ,@(when regset
               `((unless (or (register-unambiguous-regset ,space ,regname)
                             (null (regset-context ,env))
                             (member (regset-name (register-set ,space ,regset)) (regset-context ,env)))
                   (error "regset context compilation error: mapped ~S to ~S, while ~S were available"
                          ,regname (regset-name (register-set ,space ,regset)) (regset-context ,env)))))
       ,@body)))

(defmacro decode-context ((spacename &optional regset-want space-want bitfield fmtname) regname bytenames env &body body)
  (if bytenames
      `(decode-context-with-bitfields (,spacename ,regset-want ,space-want ,bitfield ,fmtname) ,regname ,bytenames ,env ,@body)
      `(decode-context-without-bitfields (,spacename ,regset-want ,space-want ,fmtname) ,regname ,env ,@body)))

(defstruct (early-operator (:conc-name eop-))
  name evalspec associativity identity)

(defparameter *eops* (make-hash-table))

(defun eop (name)
  (if-let ((eop (gethash name *eops*)))
	  (values (eop-evalspec eop) (eop-associativity eop) (eop-identity eop))))

(defun defeop (name evalspec associativity identity)
  (setf (gethash name *eops*) (make-early-operator :name name :evalspec evalspec
						   :associativity associativity :identity identity)))

(mapc [apply #'defeop] '((dpb     (t nil t) nil 0)
                         (logior   t        t   0)
                         (logand   t        t   -1)
                         (logxor   t        t   0)
                         (logeqv   t        t   -1)
                         (lognot   t        nil nil)
                         (logandc1 t        nil nil)
                         (logandc2 t        nil nil)
                         (logiorc1 t        nil nil)
                         (logiorc2 t        nil nil)
                         (lognor   t        nil nil)
                         (lognand  t        nil nil)))

;; possibly makes a 1+-long in the abbreviated case
(defun decode-eval-mask (mask form &optional acc)
  (if (typep mask 'cons)
      (decode-eval-mask (cdr mask) (cdr form) (cons (car mask) acc))
      (nreverse (nconc (make-list (length form) :initial-element mask) acc))))

(defun atom-immediate-p (form)
  (or (typep form 'integer) (typep form 'boolean) (typep form 'keyword)))

(defun constant-p (form)
  (or (atom-immediate-p form)
      (quoted-p form)))

(defun immediate-p (form)
  (or (atom-immediate-p form)
      (and (consp form)
	   (if-let* ((espec (eop (car form))) (evalmask (decode-eval-mask espec form)))
		    (every #'or-p (mapcar #'immediate-p (cdr form)) (mapcar #'null evalmask))))))

(defun eval-atom (form mask env)
  (declare (type (or number boolean keyword) form) (type integer mask)
	   (type (or null environment) env))
  (etypecase form
    (number (logand mask (val env form)))
    (null 0)
    ((eql t) mask)
    (keyword (logand mask (var env form)))))
  
(defun neval (form &optional bitmasks envs)
  (cond
    ((atom form) (let ((imm-p (immediate-p form)) (bitmask (or (car bitmasks) -1)))
		   (values (if imm-p (eval-atom form bitmask (car envs))
			       `(eval-atom ,form ,bitmask ,(load-time-env (car envs))))
			   (not imm-p))))
    ((quoted-p form) form)
    ((null (eop (car form))) (values `(eval-atom ,form ,(or (car bitmasks) -1)
						 ,(load-time-env (car envs))) t))
    (t (multiple-value-bind (present-p assoc-op-p identity) (eop (car form))
	 (declare (ignore present-p))
	 (labels ((iterate (parms bitmasks envs)
		    (let* ((parm (car parms)) (quotedp (quoted-p parm))
			   (bmask (car bitmasks)) (bitmasks (if quotedp bitmasks (cdr bitmasks)))
			   (env (car envs)) (envs (if quotedp envs (cdr envs))))
		      (when parms
			(multiple-value-bind (tail var-p fold) (iterate (cdr parms) bitmasks envs)
			  (multiple-value-bind (stail svar-p) (neval parm `(,bmask) `(,env))
			    (if (and (immediate-p parm) assoc-op-p)
				(values tail (or var-p svar-p) (cons stail fold))
				(values (cons stail tail) (or var-p svar-p) fold))))))))
	   (multiple-value-bind (tail var-p fold) (iterate (cdr form) bitmasks envs)
	     (let ((params (prepend/reduce-equiv fold (fdefinition (car form)) tail
						 :identity identity)))
	       (values (eval-if (not var-p) (list* (car form) params)) var-p))))))))

(defun device-register (device regset register)
  (declare (type device device) (type reg register))
  (funcall (regset-getter regset)
	   device (if (regset-pass-register regset) register (reg-selector register))))

(defun (setf device-register) (value device regset register)
  (declare (type (unsigned-byte 32) value) (type device device) (type reg register))
  (funcall (regset-setter regset)
	   value device (if (regset-pass-register regset) register (reg-selector register))))
    
(defmacro devreg (&environment env device regname)
  (decode-context (space-name regsetname) regname () env
    `(device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
		      (load-time-value (register (space ',space-name) ,regname)))))

(define-setc-expander devreg (&environment env value device regname)
  (decode-context (space-name regsetname) regname () env
    `(setf (device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
			    (load-time-value (register (space ',space-name) ,regname)))
	   ,(neval value))))

(defmacro decode (&environment env fmtname value &key (symbolise-unknowns t))
  (decode-context (space-name) nil () env
    (if (constant-p fmtname) ;; we won't do the same for other obvious cases
        `(format-decode (load-time-value (regformat (space ',space-name) ,fmtname)) ,value :symbolise-unknowns ,symbolise-unknowns)
        `(format-decode (regformat (space ',space-name) ,fmtname) ,value :symbolise-unknowns ,symbolise-unknowns))))

(defmacro devbit-decode (&environment env device regname bytename)
  (decode-context (space-name regsetname space bitfield) regname `(,bytename) env
    `(bitfield-decode (load-time-value (bitfield (space ',space-name) ,bytename))
		      (device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
				       (load-time-value (register (space ',space-name) ,regname))))))

(defmacro devreg-decode (&environment env device regname)
  (decode-context (space-name regsetname space bitfield fmtname) regname () env
    `(format-decode (load-time-value (regformat (space ',space-name) ,fmtname))
		    (device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
				     (load-time-value (register (space ',space-name) ,regname))))))

(defmacro devbit (&environment env device regname bytename)
  (decode-context (space-name regsetname space) regname `(,bytename) env
    `(ldb-test ',(bitfield-byte space bytename)
	       (device-register
		,device (load-time-value (register-set (space ',space-name) ,regsetname))
		(load-time-value (register (space ',space-name) ,regname))))))
  
(defmacro devbit-value (&environment env device regname bytename)
  (decode-context (space-name regsetname space) regname `(,bytename) env
    `(ldb ',(bitfield-byte space bytename)
	  (device-register
	   ,device (load-time-value (register-set (space ',space-name) ,regsetname))
	   (load-time-value (register (space ',space-name) ,regname))))))

(define-setc-expander devbit (&environment env value device regname bytename &key write-only)
  (decode-context (space-name regsetname space bitfield) regname `(,bytename) env
    (let ((wronly (or write-only (regset-write-only (register-set space regsetname))))
	  (mask (byte-bitmask (bitfield-spec bitfield))))
      `(setf (device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
			      (load-time-value (register (space ',space-name) ,regname)))
	     ,(neval
	       `(logior ,value
			,(unless wronly
				 `(device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
						   (load-time-value (register (space ',space-name) ,regname)))))
	       `(,mask ,(lognot mask))
	       `(,(mkenv space-name (bitfield-name bitfield)) nil))))))

(defmacro devbits (&environment env device regname (&rest bytenames))
  (decode-context (space-name regsetname space bitfield) regname bytenames env
    `(values-list 
      (mapcar
       (rcurry #'ldb-test (device-register
			   ,device (load-time-value (register-set (space ',space-name) ,regsetname))
			   (load-time-value (register (space ',space-name) ,regname))))
       ',(mapcar [bitfield-byte space] bytenames)))))

(define-setc-expander devbits (&environment env values device regname (&rest bytenames) &key write-only)
  (decode-context (space-name regsetname space bitfield) regname bytenames env
    (with-gensyms (device-var regset-var reg-var) 
      (let* ((initial (unless (or write-only (regset-write-only (register-set space regsetname)))
			`(device-register ,device-var ,regset-var ,reg-var)))
	     (bytes (mapcar [bitfield-byte space] bytenames)))
	`(let ((,device-var ,device)
	       (,regset-var (load-time-value (register-set (space ',space-name) ,regsetname)))
	       (,reg-var (load-time-value (register (space ',space-name) ,regname))))
	   (setf (device-register ,device-var ,regset-var ,reg-var)
		 ,(neval (list* 'logior initial (ensure-destructurisation bytenames values))
			 (list* (lognot (bytes-bitmask bytes)) (mapcar #'byte-bitmask bytes))
			 (list* nil (mapcar [mkenv space-name] bytenames)))))))))

(defmacro bits (&environment env bytenames &rest bytevals)
  "Combined bytevals of bitfields specified by BYTENAMES/BYTEVALS.
  Bytevals default to T, when left completely unspecified."
  (decode-context (space-name regsetname space) nil bytenames env
    (let* ((bytenames (ensure-list bytenames))
	   (bytevals (or bytevals (make-list (length bytenames) :initial-element t))))
      (neval (list* 'logior bytevals)
	     (mapcar {#'byte-bitmask [bitfield-byte space]} bytenames)
	     (mapcar [mkenv space-name] bytenames)))))

(defmacro test-devbits (&environment env device regname bytenames &rest bytevals)
  "Check if every bitfield among those specified by BYTENAMES is set to a value denoted by
   a corresponding member of BYTEVALS."
  (decode-context (space-name regsetname space bitfield) nil bytenames env
    (let ((bytenames (ensure-list bytenames))
	  (regsetname (register-regset-name space (reg-name (register space regname)) (regset-context env))))
      `(= (logand (device-register ,device (load-time-value (register-set (space ',space-name) ,regsetname))
				   (load-time-value (register (space ',space-name) ,regname)))
		  ,(bytes-bitmask (mapcar [bitfield-byte space] bytenames)))
	  (bits ,bytenames ,@bytevals)))))

(defmacro test-bits (&environment env (&rest bytenames) val)
  "Check if every bitfield specified by BYTENAMES is set to all 1's."
  (let ((mask (bytes-bitmask (mapcar [bitfield-byte (space (space-name-context env))] bytenames))))
    `(= (logand ,val ,mask) ,mask)))

(defmacro bit-value (&environment env value bytename)
  (decode-context (space-name regsetname space) nil `(,bytename) env
    `(ldb ',(bitfield-byte space bytename) ,value)))
