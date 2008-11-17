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

(defclass space ()
  ((name :accessor space-name :type (or keyword list) :initarg :name)
   (documentation :accessor space-documentation :type string :initarg :documentation)
   (implemented-by :accessor space-implemented-by :type (or space list) :initarg :implemented-by)
   (referrers :accessor space-referrers :initform nil :type list)

   (devices :accessor devices :initform (make-hash-table :test 'equal) :type hash-table)
   (devicetypes :accessor devicetypes :initform (make-hash-table) :type hash-table)

   (formats :accessor formats :initform (make-hash-table) :type hash-table)
   (layouts :accessor layouts :initform (make-hash-table) :type hash-table)
   (bankmaps :accessor bankmaps :initform (make-hash-table) :type hash-table)
   (banks :accessor banks :initform (make-hash-table) :type hash-table)
   (registers :accessor registers :initform (make-hash-table) :type hash-table)
   (bitfields :accessor bitfields :initform (make-hash-table) :type hash-table)
   (bitfield-bytes :accessor bitfield-bytes :initform (make-hash-table) :type hash-table)
   )
  (:default-initargs :implemented-by nil))

(defun space-root (space)
  (if (space-implemented-by space)
      (space-root (space-implemented-by space))
      space))

(defstruct (docunamed (:conc-name nil))
  (name nil :type symbol)
  (documentation "" :type string))

(defstruct (spaced (:include docunamed))
  (space nil :type (or null space)))

(defstruct (format (:include spaced))
  bitfields)

(defstruct (bitfield (:include spaced))
  format
  spec
  (bytevals (make-hash-table) :type hash-table)
  (byterevvals (make-hash-table) :type hash-table))

(defstruct (layout (:include spaced))
  "Maps register names into register structures."
  registers)
  
(defstruct (reg (:include spaced))
  "Defines a formatted register, specified within a layout with a selector."
  layout
  format
  selector
  type ext)

(defstruct (bank (:include spaced)
                 (:print-object
                  (lambda (obj stream)
                    (cl:format stream "~@<#<BANK~; :name ~S :documentation ~S~;>~:@>"
                               (name obj) (bank-documentation obj)))))
  "Augments register layouts with an access method."
  layout
  getter setter
  pass-register write-only)
  
(defstruct (byteval (:include spaced))
  byte
  value)

(defclass device ()
  ((id :accessor device-id :type (integer 0))
   (space :accessor device-space :type space)
   (backend :accessor device-backend :type (or null device) :initarg :backend)))

(defun byte-bitmask (byte)
  (dpb -1 byte 0))

(defun flatten-byteval (byteval)
  (declare (type byteval byteval))
  (dpb (byteval-value byteval) (byteval-byte byteval) 0))

(defun bytes-bitmask (bytes)
  (reduce (lambda (acc byte) (dpb -1 byte acc)) bytes :initial-value 0))

(defun device-hash-id (device)
  (list (type-of device) (device-id device)))

(defmethod print-object ((space space) stream)
  (cl:format stream "~@<#<SPACE:~;~A ~S implemented-by: ~S formats: ~S banks: ~S registers: ~S devices: ~S bankmap: ~S layouts: ~S~;>~:@>"
             (space-name space)
             (space-documentation space) (if (listp (space-implemented-by space))
                                             (space-implemented-by space)
                                             (space-name (space-implemented-by space)))
             (loop :for x :being :the :hash-values :in (formats space) :collect (name x))
             (loop :for x :being :the :hash-values :in (banks space) :collect (name x))
             (loop :for x :being :the :hash-values :in (registers space) :collect (name x))
             (loop :for x :being :the :hash-values :in (devices space) :collect (device-hash-id x))
             (loop :for x :being :the :hash-values :in (bankmaps space)
                                                   :using (hash-key k) :collect (list k x))
             (loop :for x :being :the :hash-values :in (layouts space) :collect (name x))))

(defmethod print-object ((device device) stream)
  (labels ((slot (id) (if (slot-boundp device id) (slot-value device id) :unbound-slot)))
    (cl:format stream "~@<#<~;~A-~A backend: ~S~;>~:@>" (type-of device) (slot 'id) (slot 'backend))))

(defun device-insert (space device)
  (lret* ((type (type-of device))
          (id (length (gethash type (devicetypes space)))))
    (push device (gethash type (devicetypes space)))
    (setf (device-id device) id
          (device-space device) space
          (gethash (list type id) (devices space)) device)))

(defun space-device (space type id)
  (let ((bucket (gethash type (devicetypes space))))
    (find id bucket :key #'device-id)))

(defmethod initialize-instance :after ((device device) &key space &allow-other-keys)
  (device-insert space device))

(defgeneric space-remove-device (device)
  (:method ((device device))
    (let ((space (device-space device)))
      (remhash (device-hash-id device) (devices space))
      (removef (gethash (type-of device) (devicetypes space)) device))))

(defun purge-namespace-devices (space)
  (clrhash (devices space))
  (clrhash (devicetypes space)))

(define-condition bit-notation-condition (error) ())

(define-condition bit-notation-no-space-context-error (bit-notation-condition)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (cl:format stream "Attempt to use names in a null space context."))))

(defvar *spaces* (make-hash-table :test #'equal))

(define-container-hash-accessor *spaces* space)
(define-container-hash-accessor :i format :container-transform formats :parametrize-container t)
(define-container-hash-accessor :i layout :container-transform layouts :parametrize-container t)
(define-container-hash-accessor :i bank :container-transform banks :parametrize-container t)
(define-container-hash-accessor :i register :container-transform registers :parametrize-container t)
(define-container-hash-accessor :i bitfield :container-transform bitfields :parametrize-container t)
(define-container-hash-accessor :i bitfield-byte :container-transform bitfield-bytes :parametrize-container t)
(define-container-hash-accessor :i byteval :container-transform bitfield-bytevals :parametrize-container t)

(defun bitfield-format (space bitfield-name)
  "Yield the format of BITFIELD-NAMEd in SPACE"
  (bitfield-format (bitfield space bitfield-name)))

(defun bitfield-documentation (space bitfield-name)
  "Yield the documentation for BITFIELD-NAMEd in SPACE"
  (documentation (bitfield space bitfield-name)))

(defun register-format-field-type (name)
  (format-symbol t "~A-BITFIELD" name))

(defun define-bitfield (format name size pos doc &optional bytevals)
  (let* ((byte (byte size pos))
	 (space (format-space format))
	 (bitfield (make-bitfield :format format :name name :spec byte :documentation doc)))
    (when (gethash name (bitfields space))
      (error "Attempt to redefine as existing bitfield ~S in namespace ~S~%" name (space-name space)))
    (push bitfield (format-bitfields format))
    (loop :for (value name documentation) :in bytevals
                                          :do (let ((byteval (make-byteval :name name :documentation documentation :byte byte :value value)))
                                                (setf (gethash name (bitfield-bytevals bitfield)) byteval
                                                      (gethash value (bitfield-byterevvals bitfield)) byteval)))
    (dolist (space (list* space (mapcar #'space (space-referrers space))))
      (setf (gethash name (bitfield-bytes space)) byte
	    (gethash name (bitfields space)) bitfield))))

;; an ability to pluck in a QUOTE would've been very nice...
(defun define-register-format-notype (space name documentation bitspecs)
  (let ((format (make-format :name name :documentation documentation :space space)))
    (mapc [apply [define-bitfield format]] bitspecs)
    (setf (gethash name (formats space)) format)))

(defmacro define-register-format (&environment env name doc &rest bitspecs)
  `(eval-when (:compile-toplevel :load-toplevel)
     (deftype ,(register-format-field-type name) () `(member ,,@(mapcar #'car bitspecs)))
     (define-register-format-notype (space ,(space-name (space (space-name-context env)))) ',name ,doc ',bitspecs)))
  
(defun define-register (layout name selector &key (doc "Undocumented register.") format ext)
  (when (gethash name (registers (layout-space layout)))
    (error "Attempt to redefine register ~S in namespace ~S."
	   name (space-name (layout-space layout))))
  (let ((register (make-reg :layout layout :name name :selector selector :documentation doc
                                                                         :format (when format (format (layout-space layout) format)) :ext ext
                                                                         :type (register-format-field-type format))))
    (push register (layout-registers layout))
    (setf (gethash name (registers (layout-space layout))) register)))

(defun define-layout-notype (space name documentation registerspecs)
  (let ((layout (make-layout :name name :space space :documentation documentation)))
    (mapc [apply [define-register layout]] registerspecs)
    (setf (gethash name (layouts space)) layout)))

(defmacro define-layout (&environment env (name doc) &rest defs)
  `(progn
     (deftype ,name () `(member ,,@(mapcar #'first defs)))
     (define-layout-notype
	 (space ,(space-name (space (space-name-context env)))) ',name ,doc ',defs)))

(defun bank-try-claim-layout (space bank layout)
  (when-let ((registers (remove-if-not #'(lambda (r) (eq layout (reg-layout r)))
                                       (hash-table-values (registers space)))))
    (case (gethash (name (first registers)) (bankmaps space))
      ((t) nil)
      ((nil) (dolist (reg registers)
               (setf (gethash (name reg) (bankmaps space)) bank)))
      (t (dolist (reg registers)
           (setf (gethash (name reg) (bankmaps space)) t))))))
      
(defmacro define-bank (&environment env name layout-name accessor doc &key pass-register write-only)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (let* ((space (space ',(space-name (space (space-name-context env)))))
              (layout (layout space ',layout-name))
              (home-space (layout-space layout))
              (bank (make-bank :name ',name :space home-space :layout layout :documentation ,doc
                                                                             :write-only ,write-only :pass-register ,pass-register)))
         (setf (gethash ',name (banks home-space)) bank)
         (bank-try-claim-layout home-space bank layout)
         (unless (eq space home-space)
           (setf (gethash ',name (banks space)) bank)
           (bank-try-claim-layout space bank layout))))
     (eval-when (:load-toplevel)
       (let* ((space (space ',(space-name (space (space-name-context env)))))
              (bank (bank space ',name)))
         (unless (or (fboundp ,accessor)
                     (fboundp (list 'setf ,accessor)))
           (error "No functions available for register set accessor ~S." ',name))
         (when (fboundp ,accessor)
           (setf (bank-getter bank) (fdefinition ,accessor)))
         (when (fboundp (list 'setf ,accessor))
           (setf (bank-setter bank) (fdefinition (list 'setf ,accessor))))))))

(defun register-unambiguous-bank (space regname)
  (let ((spec (gethash regname (bankmaps space))))
    (case spec
      ((nil)
       (when (gethash regname (registers space))
	 (error "Register ~S is hashed in space ~S, but has no associated bank." regname space))
       (error "Unknown register ~S in space ~S." regname space))
      ((t) nil)
      (t spec))))

(defun register-bank-name (space regname disambiguation)
  (declare (type space space) (type symbol regname) (type list disambiguation))
  (name (or (register-unambiguous-bank space regname)
            (find (reg-layout (register space regname))
                  (mapcar [bank space] disambiguation) :key #'bank-layout)
            (error "Ambiguous access method for register ~S space ~S." regname space))))

(defun bank-context (env)
  (multiple-value-bind (val expanded-p) (macroexpand-1 '*banks* env)
    (when expanded-p val)))

(defmacro with-banks (&environment env (&rest rsnames) &body body)
  (if-let ((orphan (find-if-not [bank (space (space-name-context env))] rsnames)))
	  (error "Reference to an undefined register set ~S." orphan))
  `(symbol-macrolet ((*banks* ,rsnames))
     ,@body))

;;;
;;;  o  layout templates
;;;  o  register instantiation
;;;;    - what information is deduced from register instances /now/?
;;;;      ... survey
;;;
(defmacro define-namespace (name &body f)
  (let ((implemented-by (cadr (assoc :implemented-by f)))
	(documentation (cadr (assoc :documentation f))))
    `(eval-when (:compile-toplevel :load-toplevel)
       ,(once-only (name)
                   `(setf (space ,name)
                          (make-instance 'space :documentation ,documentation
                                                :name ,name
                                                ,@(when implemented-by
                                                        `(:implemented-by (space ,implemented-by))))))

       (symbol-macrolet ((*space* ,name))
	 ,@(mapcar [cons 'define-register-format] (cdr (assoc :register-formats f)))
	 ,@(mapcar [cons 'define-layout] (cdr (assoc :layouts f)))))))

(defun undefine-space (name)
  (remhash name *spaces*))
  
(defun unify-namespaces (names)
  (let* ((spaces (mapcar #'space names))
	 (unispace (make-instance 'space :name names
					 :documentation (cl:format nil "Unified namespace of:~{ ~S~}."
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
	(dolist (accessor-name '(banks registers formats bitfields bitfield-bytes layouts bankmaps))
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
  (cond ((plusp (hash-table-count (bitfield-bytevals bitfield))) ;; bitfield-enumerated-p?
	 (let* ((val (ldb (bitfield-spec bitfield) value)))
	   (if-let ((field (gethash val (bitfield-byterevvals bitfield))))
		   (name field)
                   (if symbolise-unknowns
                       (format-symbol :keyword "UNKNOWN-VALUE-~B" val)
                       val))))
	((> (car (bitfield-spec bitfield)) 1) ;; bitfield-wide-p?
	 (ldb (bitfield-spec bitfield) value))
	(t
	 (ldb-test (bitfield-spec bitfield) value))))

(defun format-decode (format value &key (symbolise-unknowns t))
  (declare (type format format) (type (unsigned-byte 32) value))
  (iter (for bitfield in (format-bitfields format))
        (collect (cons (name bitfield) (bitfield-decode bitfield value :symbolise-unknowns symbolise-unknowns)))))

;;;;
;;;;
;;;;  F R O N T E N D
;;;;
;;;;
(defun bytenames-ensure-same-register (space regname bytenames)
  "Deduce register difference from register format difference and signal an error, if any."
  (let ((fmt (if regname (reg-format (register space regname))
		 (bitfield-format space (first bytenames)))))
    (if-let ((stray (find-if-not {[eq fmt] [bitfield-format space]} bytenames)))
	    (error "Ambiguous multiregister access: ~S vs. ~S:~S"
		   (name fmt) (name (bitfield-format space stray)) stray))))

(defun mkenv (space-name bitfield-name &aux (bitfield (bitfield (space space-name) bitfield-name)))
  (make-environment
   :byte (bitfield-spec bitfield)
   :bindings
   (if (or (null space-name) (null bitfield-name))
       (make-hash-table)
       (xform-hash-table #'flatten-byteval (bitfield-bytevals bitfield)))))

(defmacro decode-context-with-bitfields ((spacename &optional bank-want space-want bitfield fmtname) regname bytenames env &body body)
  (let ((space (or space-want (gensym))) (bank (and regname bank-want)) (newbytenames (gensym)))
    `(let* ((,spacename (space-name-context ,env)) (,space (space ,spacename))
            (,newbytenames (ensure-list ,bytenames))
            ,@(when bitfield `((,bitfield (bitfield ,space (car ,newbytenames)))))
            ,@(when bank `((,bank (register-bank-name ,space ,regname (bank-context ,env)))))
            ,@(when fmtname `((,fmtname (and ,regname (if-let ((format (reg-format (register ,space ,regname))))
                                                              (name format)))))))
       (declare (ignorable ,space ,@(when bitfield `(,bitfield)) ,@(when fmtname `(,fmtname)) ,@(when bank `(,bank)) ,newbytenames))
       (bytenames-ensure-same-register ,space ,regname ,newbytenames)
       ,@(when bank
               `((unless (or (register-unambiguous-bank ,space ,regname)
                             (null (bank-context ,env))
                             (member (name (bank ,space ,bank)) (bank-context ,env)))
                   (error "bank context compilation error: mapped ~S to ~S, while ~S were available"
                          ,regname (name (bank ,space ,bank)) (bank-context ,env)))))
       ,@body)))

(defmacro decode-context-without-bitfields ((spacename &optional bank-want space-want fmtname) regname env &body body)
  (let ((space (or space-want (gensym))) (bank (and regname bank-want)))
    `(let* ((,spacename (space-name-context ,env)) (,space (space ,spacename))
            ,@(when bank `((,bank (register-bank-name ,space ,regname (bank-context ,env)))))
            ,@(when fmtname `((,fmtname (and ,regname (if-let ((format (reg-format (register ,space ,regname))))
                                                              (name format)))))))
       (declare (ignorable ,space ,@(when fmtname `(,fmtname)) ,@(when bank `(,bank))))
       ,@(when bank
               `((unless (or (register-unambiguous-bank ,space ,regname)
                             (null (bank-context ,env))
                             (member (name (bank ,space ,bank)) (bank-context ,env)))
                   (error "bank context compilation error: mapped ~S to ~S, while ~S were available"
                          ,regname (name (bank ,space ,bank)) (bank-context ,env)))))
       ,@body)))

(defmacro decode-context ((spacename &optional bank-want space-want bitfield fmtname) regname bytenames env &body body)
  (if bytenames
      `(decode-context-with-bitfields (,spacename ,bank-want ,space-want ,bitfield ,fmtname) ,regname ,bytenames ,env ,@body)
      `(decode-context-without-bitfields (,spacename ,bank-want ,space-want ,fmtname) ,regname ,env ,@body)))

(defun device-register (device bank register)
  (declare (type device device) (type reg register))
  (funcall (bank-getter bank)
	   device (if (bank-pass-register bank) register (reg-selector register))))

(defun (setf device-register) (value device bank register)
  (declare (type (unsigned-byte 32) value) (type device device) (type reg register))
  (funcall (bank-setter bank)
	   value device (if (bank-pass-register bank) register (reg-selector register))))
    
(defmacro devreg (&environment env device regname)
  (decode-context (space-name bankname) regname () env
    `(device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
		      (load-time-value (register (space ',space-name) ,regname)))))

(define-setc-expander devreg (&environment env value device regname)
  (decode-context (space-name bankname) regname () env
    `(setf (device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
			    (load-time-value (register (space ',space-name) ,regname)))
	   ,(eeval value))))

(defmacro decode (&environment env fmtname value &key (symbolise-unknowns t))
  (decode-context (space-name) nil () env
    (if (constant-p fmtname) ;; we won't do the same for other obvious cases
        `(format-decode (load-time-value (format (space ',space-name) ,fmtname)) ,value :symbolise-unknowns ,symbolise-unknowns)
        `(format-decode (format (space ',space-name) ,fmtname) ,value :symbolise-unknowns ,symbolise-unknowns))))

(defmacro devbit-decode (&environment env device regname bytename)
  (decode-context (space-name bankname space bitfield) regname `(,bytename) env
    `(bitfield-decode (load-time-value (bitfield (space ',space-name) ,bytename))
		      (device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
				       (load-time-value (register (space ',space-name) ,regname))))))

(defmacro devreg-decode (&environment env device regname)
  (decode-context (space-name bankname space bitfield fmtname) regname () env
    `(format-decode (load-time-value (format (space ',space-name) ,fmtname))
		    (device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
				     (load-time-value (register (space ',space-name) ,regname))))))

(defmacro devbit (&environment env device regname bytename)
  (decode-context (space-name bankname space) regname `(,bytename) env
    `(ldb-test ',(bitfield-byte space bytename)
	       (device-register
		,device (load-time-value (bank (space ',space-name) ,bankname))
		(load-time-value (register (space ',space-name) ,regname))))))
  
(defmacro devbit-value (&environment env device regname bytename)
  (decode-context (space-name bankname space) regname `(,bytename) env
    `(ldb ',(bitfield-byte space bytename)
	  (device-register
	   ,device (load-time-value (bank (space ',space-name) ,bankname))
	   (load-time-value (register (space ',space-name) ,regname))))))

(define-setc-expander devbit (&environment env value device regname bytename &key write-only)
  (decode-context (space-name bankname space bitfield) regname `(,bytename) env
    (let ((wronly (or write-only (bank-write-only (bank space bankname))))
	  (mask (byte-bitmask (bitfield-spec bitfield))))
      `(setf (device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
			      (load-time-value (register (space ',space-name) ,regname)))
	     ,(eeval
	       `(logior ,value
			,(unless wronly
				 `(device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
						   (load-time-value (register (space ',space-name) ,regname)))))
	       `(,mask ,(lognot mask))
	       `(,(mkenv space-name (bitfield-name bitfield)) nil))))))

(defmacro devbits (&environment env device regname (&rest bytenames))
  (decode-context (space-name bankname space bitfield) regname bytenames env
    `(values-list 
      (mapcar
       (rcurry #'ldb-test (device-register
			   ,device (load-time-value (bank (space ',space-name) ,bankname))
			   (load-time-value (register (space ',space-name) ,regname))))
       ',(mapcar [bitfield-byte space] bytenames)))))

(define-setc-expander devbits (&environment env values device regname (&rest bytenames) &key write-only)
  (decode-context (space-name bankname space bitfield) regname bytenames env
    (with-gensyms (device-var bank-var reg-var) 
      (let* ((initial (unless (or write-only (bank-write-only (bank space bankname)))
			`(device-register ,device-var ,bank-var ,reg-var)))
	     (bytes (mapcar [bitfield-byte space] bytenames)))
	`(let ((,device-var ,device)
	       (,bank-var (load-time-value (bank (space ',space-name) ,bankname)))
	       (,reg-var (load-time-value (register (space ',space-name) ,regname))))
	   (setf (device-register ,device-var ,bank-var ,reg-var)
		 ,(eeval (list* 'logior initial (ensure-destructurisation bytenames values))
			 (list* (lognot (bytes-bitmask bytes)) (mapcar #'byte-bitmask bytes))
			 (list* nil (mapcar [mkenv space-name] bytenames)))))))))

(defmacro bits (&environment env bytenames &rest bytevals)
  "Combined bytevals of bitfields specified by BYTENAMES/BYTEVALS.
  Bytevals default to T, when left completely unspecified."
  (decode-context (space-name bankname space) nil bytenames env
    (let* ((bytenames (ensure-list bytenames))
	   (bytevals (or bytevals (make-list (length bytenames) :initial-element t))))
      (eeval (list* 'logior bytevals)
	     (mapcar {#'byte-bitmask [bitfield-byte space]} bytenames)
	     (mapcar [mkenv space-name] bytenames)))))

(defmacro test-devbits (&environment env device regname bytenames &rest bytevals)
  "Check if every bitfield among those specified by BYTENAMES is set to a value denoted by
   a corresponding member of BYTEVALS."
  (decode-context (space-name bankname space bitfield) nil bytenames env
    (let ((bytenames (ensure-list bytenames))
	  (bankname (register-bank-name space (name (register space regname)) (bank-context env))))
      `(= (logand (device-register ,device (load-time-value (bank (space ',space-name) ,bankname))
				   (load-time-value (register (space ',space-name) ,regname)))
		  ,(bytes-bitmask (mapcar [bitfield-byte space] bytenames)))
	  (bits ,bytenames ,@bytevals)))))

(defmacro test-bits (&environment env (&rest bytenames) val)
  "Check if every bitfield specified by BYTENAMES is set to all 1's."
  (let ((mask (bytes-bitmask (mapcar [bitfield-byte (space (space-name-context env))] bytenames))))
    `(= (logand ,val ,mask) ,mask)))

(defmacro bit-value (&environment env value bytename)
  (decode-context (space-name bankname space) nil `(,bytename) env
    `(ldb ',(bitfield-byte space bytename) ,value)))
