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
   (referrers :accessor space-referrers :initform nil :type list)

   (devices :accessor devices :initform (make-hash-table :test 'equal) :type hash-table)
   (layouts :accessor layouts :initform (make-hash-table) :type hash-table)
   (registers :accessor register-dictionary :initform (make-dictionary) :type dictionary)
   (bitfields :accessor bitfields :initform (make-hash-table) :type hash-table)
   (bitfield-bytes :accessor bitfield-bytes :initform (make-hash-table) :type hash-table)
   ))

(defmethod print-object ((space space) stream)
  (cl:format stream "~@<#<SPACE:~;~A ~S registers: ~S devices: ~S layouts: ~S~;>~:@>"
             (space-name space)
             (space-documentation space)
             (map 'list #'name (dictionary::dictionary-ids-to-values (register-dictionary space)))
             (maphash-values #'device-hash-id (devices space))
             (maphash-values #'name (layouts space))))

(defstruct (docunamed (:conc-name nil))
  (name nil :type symbol)
  (documentation "" :type string))

(defstruct (spaced (:include docunamed))
  (space nil :type (or null space)))

(defstruct (format (:include spaced))
  bitfields)

(defstruct (bitfield (:include spaced))
  format%
  spec
  (bytevals (make-hash-table) :type hash-table)
  (byterevvals (make-hash-table) :type hash-table))

(defstruct (layout (:include spaced) (:constructor %make-layout))
  "Maps register names into register structures."
  name-format
  registers
  register-selectors)

(defun make-layout (&rest args &key name-format &allow-other-keys)
  (apply #'%make-layout :name-format (or name-format (cl:format nil "~~A~~^"))
	 (remove-from-plist args :name-format)))

(defstruct (register (:include spaced) (:conc-name reg-))
  "Defines a formatted register, specified within a layout with a selector."
  aliases
  layout
  format
  type ext)

(defstruct (register-instance (:include docunamed) (:conc-name reginstance-))
  "Instance of register."
  device
  register
  selector
  reader writer
  id)
  
(defstruct (byteval (:include spaced))
  byte
  value)

(defun byte-bitmask (byte &optional (acc 0))
  (dpb -1 byte acc))

(defun bytes-bitmask (bytes)
  (reduce #'byte-bitmask bytes :initial-value 0 :from-end t))

(defun flatten-byteval (byteval)
  (dpb (byteval-value byteval) (byteval-byte byteval) 0))

(defvar *spaces* (make-hash-table :test #'equal))
(defvar *device-classes* (make-hash-table :test 'eq))
(defvar *register-formats* (make-hash-table :test 'eq))
(defvar *register-spaces* (make-hash-table :test 'eq))
(defvar *register-instances* (make-hash-table :test #'eq))
(defvar *register-instances-by-id* (make-hash-table :test #'eq))

(define-container-hash-accessor *spaces* space :if-exists :continue)
(define-container-hash-accessor *device-classes* device-class :iterator do-device-classes)
(define-container-hash-accessor *register-formats* format :type format)
(define-container-hash-accessor *register-spaces* register-space :type space :if-exists :error)
(define-container-hash-accessor *register-instances* register-instance :type register-instance :if-exists :error)
(define-container-hash-accessor *register-instances-by-id* register-instance-by-id :type register-instance :if-exists :error)
(define-container-hash-accessor :i device :container-transform devices :parametrize-container t)
(define-container-hash-accessor :i layout :container-transform layouts :parametrize-container t)
(define-container-hash-accessor :i bitfield :container-transform bitfields :parametrize-container t :if-exists :error)
(define-container-hash-accessor :i bitfield-byte :container-transform bitfield-bytes :parametrize-container t :type cons)
(define-container-hash-accessor :i byteval :container-transform bitfield-bytevals :parametrize-container t)

;; This one stands out: going through dictionaries.
(defun register (space name)
  (declare (space space) (keyword name))
  (translation (register-dictionary space) name))

(defun register-id (name)
  (declare (keyword name))
  (symbol-id (register-dictionary (register-space name)) name))

(defclass device-class (standard-class)
  ((register-selectors :accessor device-class-register-selectors :type (vector fixnum))
   (readers :accessor device-class-readers :type (vector function) :documentation "Register ID-indexed reader lookup table.")
   (writers :accessor device-class-writers :type (vector function) :documentation "Register ID-indexed writer lookup table.")
   (space :accessor device-class-space :type space)
   (instances :accessor device-class-instances :type list :initarg :instances)
   (layouts :accessor device-class-layouts :type list)
   (layout-accessors :accessor device-class-layout-accessors :type list :initarg :layouts :documentation "Layout->accessor map."))
  (:default-initargs :instances nil))

(defmacro define-device-class (name space superclasses slots &rest options)
  `(defclass ,name ,(or superclasses '(device))
     ,slots
     (:metaclass device-class)
     (:space . ,space)
     ,@(remove-if (lambda (x) (member x '(:metaclass :space))) options :key #'first)))

(defmethod validate-superclass ((class device-class)
                                (superclass standard-class))
  "DEVICE-CLASS can be a subclass of STANDARD-CLASS."
  t)

(defclass device ()
  ((register-selectors :accessor device-register-selectors :type (vector fixnum) :allocation :class) ; copied over from class
   (readers :accessor device-readers :type (vector function) :allocation :class)                     ; ...
   (writers :accessor device-writers :type (vector function) :allocation :class)                     ; ...
   (id :accessor device-id :type (integer 0))
   (backend :accessor device-backend :type (or null device) :initarg :backend)
   (category :initarg :category) ;; this might go away
   )
  (:metaclass device-class))

(defmethod print-object ((device device) stream)
  (labels ((slot (id) (if (slot-boundp device id) (slot-value device id) :unbound-slot)))
    (cl:format stream "~@<#<~;~A-~A backend: ~S~;>~:@>" (type-of device) (slot 'id) (slot 'backend))))

(defun invalid-register-access-trap (&rest rest)
  (declare (ignore rest))
  (error "~@<Invalid register access.~:@>"))

(defun build-device-class-maps (space layout-specs)
  (let* ((dictionary (register-dictionary space))
         (length (length (dictionary-id-map dictionary)))
         (selector-map (make-array length :element-type 'fixnum :initial-element -1))
         (reader-map (make-array length :element-type 'function :initial-element #'invalid-register-access-trap))
         (writer-map (make-array length :element-type 'function :initial-element #'invalid-register-access-trap)))
    (iter (for (layout-name reader-name writer-name) in layout-specs)
          (let ((layout (layout space layout-name))
                (reader (if reader-name (fdefinition reader-name) #'invalid-register-access-trap))
                (writer (if writer-name (fdefinition writer-name) #'invalid-register-access-trap)))
            (iter (for register in (layout-registers layout))
                  (for register-id = (symbol-id dictionary (name register)))
                  (for selector in (layout-register-selectors layout))
                  (setf (aref selector-map register-id) selector
                        (aref reader-map register-id) reader
                        (aref writer-map register-id) writer))))
    (values selector-map reader-map writer-map)))

;;;
;;; XXX: not pretty: hack around non-&allow-other-keys-able initargs...
;;;
(defmethod initialize-instance ((o device-class) &rest initargs)
  (apply #'shared-initialize o t (remove-from-plist initargs :space)))
(defmethod reinitialize-instance ((o device-class) &rest initargs)
  (apply #'shared-initialize o nil (remove-from-plist initargs :space)))

(defmethod initialize-instance :after ((o device-class) &key space layouts &allow-other-keys)
  (if space
      (let* ((space (space space))
             (layout-instances (mapcar (compose (curry #'layout space) #'first) layouts)))
        (with-slots (register-selectors (space-slot space) (layouts-slot layouts) (readers-slot readers) (writers-slot writers)) o
          (setf (values space-slot layouts-slot) (values space layout-instances)
                (values register-selectors readers-slot writers-slot) (build-device-class-maps space layouts)
                (device-class (class-name o)) o)))
      (when layouts
        (error "~@<During initialization of device class ~S: layouts can not specified without space.~:@>" (class-name o)))))

(defun device-type (device)
  "Return the DEVICE's category, which is supposed to be a \"more
   general type\", independent of flavor variations.

   Used in:
      - runtime device nomenclature (target-device, device-hash-id),
      - device type lookups."
  (slot-value* device 'category (type-of device)))

;;;;
;;;; The decisive question is what factors into the register instance name.
;;;;
(defun device-hash-id (device)
  (list (device-type device) (device-id device)))

(defun find-device (type id)
  (find id (device-class-instances (find-class type)) :key #'device-id))

(defun create-device-register-instances (device &aux (device-class (class-of device)))
  "Walk the DEVICE's layouts and spawn the broodlings."
  (labels ((name-to-reginstance-name (name layout device)
	     (format-symbol :keyword (layout-name-format layout) name (1- (device-id device)))))
    (iter (for layout in (device-class-layouts device-class))
          (for (nil reader-name writer-name) in (device-class-layout-accessors device-class))
      (iter (for register in (layout-registers layout))
            (for selector in (layout-register-selectors layout))
            (let* ((name (name-to-reginstance-name (name register) layout device))
                   (id (1+ (hash-table-count *register-instances-by-id*)))
                   (instance (make-register-instance :name name :register register :device device :selector selector :id id
                                                     :reader (fdefinition reader-name) :writer (fdefinition writer-name))))
              (setf (register-instance-by-id id) instance)
              (iter (for riname in (cons name (mapcar (rcurry #'name-to-reginstance-name layout device)
                                                      (reg-aliases register))))
                    (assert riname)
                    (setf (register-instance riname) instance)))))))

(defmethod initialize-instance :after ((device device) &key &allow-other-keys)
  (let ((device-class (class-of device)))
    (push device (device-class-instances device-class))
    (setf (device-id device) (1- (length (device-class-instances device-class)))
          (device-register-selectors device) (device-class-register-selectors device-class)
          (device-readers device) (device-class-readers device-class)
          (device-writers device) (device-class-writers device-class)
          ;; register within space
          (gethash (device-hash-id device) (devices (device-class-space device-class))) device)
    (create-device-register-instances device)))

(defun space-device-count (space)
  (hash-table-count (devices space)))

(defun space-remove-device (device)
  (let* ((device-class (class-of device))
         (space (device-class-space device-class)))
    (remhash (device-hash-id device) (devices space))
    (removef (device-class-instances device-class) device)))

(defun init-device-model ()
  "Forget all known device and register instances."
  (iter (for (nil space) in-hashtable *spaces*)
        (when (atom (space-name space))
          (clrhash (devices space))
          (do-device-classes (device-class)
            (setf (device-class-instances device-class) nil))))
  (clrhash *register-instances*)
  (clrhash *register-instances-by-id*))

(define-condition bit-notation-condition (error) ())

(define-reported-condition bit-notation-no-space-context-error (bit-notation-condition)
  ()
  (:report () "~@<Attempt to use names in a null space context.~:@>"))

(define-reported-condition bit-notation-conflicting-bitfield-names (bit-notation-condition)
  ((conflicting-bitfield :initarg :conflicting-bitfield)
   (expected-format :initarg :expected-format))
  (:report (conflicting-bitfield expected-format)
           "~@<Encountered unexpected reference to bitfield ~S within context of register format ~S.~:@>" conflicting-bitfield expected-format))

(defun bitfield-format (space bitfield-name)
  "Yield the format of BITFIELD-NAMEd in SPACE"
  (bitfield-format% (bitfield space bitfield-name)))

(defun register-format-field-type (name)
  (format-symbol t "~A-BITFIELD" name))

(defun define-bitfield (format name size pos doc &optional bytevals)
  (let* ((byte (byte size pos))
	 (space (format-space format))
	 (bitfield (make-bitfield :format% format :name name :spec byte :documentation doc)))
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
    (setf (format name) format)))

(defmacro define-register-format (&environment env name doc &rest bitspecs)
  `(eval-when (:compile-toplevel :load-toplevel)
     (deftype ,(register-format-field-type name) () `(member ,,@(mapcar #'car bitspecs)))
     (define-register-format-notype (space ,(space-name (space (environment-space-name-context env)))) ',name ,doc ',bitspecs)))

(defun define-register (layout name &key (doc "Undocumented register.") aliases format ext)
  (lret* ((space (layout-space layout))
          (register (make-register :layout layout :name name :space space :documentation doc
                                   :format (when format (format format)) :ext ext
                                   :aliases aliases
                                   :type (register-format-field-type format))))
    ;; YYY: this is a kludge: a proper EVAL-WHEN somewhere is direly needed...
    (unless (register-space name :if-does-not-exist :continue)
      (setf (register-space name) space))
    (add-symbol (register-dictionary space) name register nil)))

(defun ensure-layout (space name documentation name-format register-specs &aux (selectors (mapcar #'second register-specs)))
  (unless (every (of-type 'fixnum) selectors)
    (error "~@<While defining layout ~S: register selectors must be of type FIXNUM.~:@>" name))
  (lret ((layout (make-layout :name name :space space :documentation documentation
                              :register-selectors selectors :name-format name-format)))
    (setf (gethash name (layouts space)) layout
          (layout-registers layout) (iter (for (name selector . rest) in register-specs)
                                          (collect (apply #'define-register layout name rest))))))

(defmacro define-layout (&environment env (name doc &key name-format) &rest defs)
  `(ensure-layout (space ,(space-name (space (environment-space-name-context env)))) ',name ,doc ,name-format ',defs))

;;;
;;;  o  layout templates
;;;  o  register instantiation
;;;;    - what information is deduced from register instances /now/?
;;;;      ... survey
;;;
(defmacro define-namespace (name &body f)
  `(eval-when (:compile-toplevel :load-toplevel)
     ,(once-only (name)
        `(setf (space ,name) (make-instance 'space :name ,name :documentation ,(cadr (assoc :documentation f)))))
     (symbol-macrolet ((*space* ,name))
       ,@(mapcar [cons 'define-register-format] (cdr (assoc :register-formats f)))
       ,@(mapcar [cons 'define-layout] (cdr (assoc :layouts f))))))

(defun undefine-space (name)
  (remhash name *spaces*))
  
(defun unify-namespaces (names)
  (let* ((spaces (mapcar #'space names))
	 (unispace (make-instance 'space :name names
					 :documentation (cl:format nil "Unified namespace of:~{ ~S~}."
                                                                   (mapcar #'space-documentation spaces)))))
    (labels ((check-import-unispace (accessor-name key val)
	       (let* ((accessor-fn (fdefinition accessor-name))
		      (hash-table (funcall accessor-fn unispace)))
		 (when (gethash key hash-table)
		   (error "Namespace conflict while trying to unify namespaces ~S: accessor ~S, key ~S." names accessor-name key))
		 (setf (gethash key hash-table) val))))
      (dolist (space spaces)
	(pushnew names (space-referrers space) :test #'equal)
	(dolist (accessor-name '(bitfields bitfield-bytes layouts))
	  (let ((checker-importer (curry #'check-import-unispace accessor-name)))
	    (maphash checker-importer (funcall (fdefinition accessor-name) space))))))
    (setf (space names) unispace)))

(defun environment-space-name-context (env)
  (let ((space-name (macroexpand-1 '*space* env)))
    (unless space-name
      (error 'bit-notation-no-space-context-error))
    space-name))

(defmacro space-name-context (&environment env)
  (list 'quote (environment-space-name-context env)))

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
    (when-let ((orphan (find-if-not #'space nsnames)))
      (error "reference to an undefined namespace ~S" orphan))
    `(eval-when (:compile-toplevel :load-toplevel)
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
	    (error 'bit-notation-conflicting-bitfield-names
		   :expected-format (name fmt) ;; (name reg) (name (bitfield-format space stray))
                   :conflicting-bitfield stray))))

(defun mkenv (space-name bitfield-name &aux (bitfield (bitfield (space space-name) bitfield-name)))
  (make-environment
   :byte (bitfield-spec bitfield)
   :bindings
   (if (or (null space-name) (null bitfield-name))
       (make-hash-table)
       (xform-hash-table #'flatten-byteval (bitfield-bytevals bitfield)))))

(defun find-space-with-bytenames (bytenames)
  "Find the first non-compound space containing every bitfield in BYTENAMES."
  (iter (for (name space) in-hashtable *spaces*)
        (when (and (symbolp name)
                   (every (lambda (bf) (bitfield space bf :if-does-not-exist :continue)) bytenames))
          (return space))))

(defmacro decode-context ((spacename &optional space-want bitfield fmtname) regname bytenames &body body)
  (unless (or regname bytenames)
    (error "~@<Impossible to deduce context: neither register name, nor byte names were specified.~:@>"))
  (let ((space (or space-want (gensym))) (newbytenames (gensym)))
    `(let* ((,space ,(if regname `(register-space ,regname) `(find-space-with-bytenames (ensure-list ,bytenames)))) (,spacename (space-name ,space))
            ,@(when bytenames `((,newbytenames (ensure-list ,bytenames))))
            ,@(when (and bitfield bytenames) `((,bitfield (bitfield ,space (car ,newbytenames)))))
            ,@(when fmtname `((,fmtname (and ,regname (xform-if #'identity #'name (reg-format (register ,space ,regname))))))))
       (declare (ignorable ,spacename ,space
                           ,@(when bytenames `(,newbytenames))
                           ,@(when (and bitfield bytenames) `(,bitfield))
                           ,@(when fmtname `(,fmtname))))
       ,@(when bytenames `((bytenames-ensure-same-register ,space ,regname ,newbytenames)))
       ,@body)))

;;;
;;; So, there are two register access schemes.
;;; First way is to access registers via combination of device information
;;; and abstract register name. This is good for debugger-internal code.
;;; The second one derives all information from unique, device-specific
;;; register instances. This is better suited for interactive use.
;;;
(defun device-register (device register-id)
  (declare (type device device) (type fixnum register-id))
  (funcall (aref (device-readers device) register-id) device (aref (device-register-selectors device) register-id)))

(defun set-device-register (device register-id value)
  (declare (type device device) (type (unsigned-byte 32) value))
  (funcall (aref (device-writers device) register-id) value device (aref (device-register-selectors device) register-id)))

(defun reginstance-value (register-instance)
  (declare (type register-instance register-instance))
  (let ((device (reginstance-device register-instance)))
   (funcall (reginstance-reader register-instance) device (reginstance-selector register-instance))))

(defun set-reginstance-value (register-instance value)
  (declare (type register-instance register-instance))
  (let ((device (reginstance-device register-instance)))
    (funcall (reginstance-writer register-instance) value device (reginstance-selector register-instance))))

(defsetf device-register set-device-register)
(defsetf reginstance-value set-reginstance-value)
    
(defmacro devreg (device regname)
  `(device-register ,device (load-time-value (register-id ,regname))))

(define-setc-expander devreg (value device regname)
  `(set-device-register ,device (load-time-value (register-id ,regname)) ,(eeval value)))

(defmacro decode (fmtname value &key (symbolise-unknowns t))
  (if (constant-p fmtname) ;; we won't do the same for other obvious cases
      `(format-decode (load-time-value (format ,fmtname)) ,value :symbolise-unknowns ,symbolise-unknowns)
      `(format-decode (format ,fmtname) ,value :symbolise-unknowns ,symbolise-unknowns)))

(defmacro devbit-decode (device regname bytename)
  (decode-context (space-name space bitfield) regname `(,bytename)
    `(bitfield-decode (load-time-value (bitfield (space ',space-name) ,bytename))
		      (device-register ,device (load-time-value (register-id ,regname))))))

(defmacro devreg-decode (device regname)
  (decode-context (space-name space bitfield fmtname) regname ()
    `(format-decode (load-time-value (format ,fmtname))
		    (device-register ,device (load-time-value (register-id ,regname))))))

(defmacro devbit (device regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    `(ldb-test ',(bitfield-byte space bytename)
	       (device-register ,device (load-time-value (register-id ,regname))))))
  
(defmacro devbit-value (device regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    `(ldb ',(bitfield-byte space bytename)
	  (device-register ,device (load-time-value (register-id ,regname))))))

(define-setc-expander devbit (value device regname bytename &key write-only)
  (decode-context (space-name space bitfield) regname `(,bytename)
    (let ((mask (byte-bitmask (bitfield-spec bitfield))))
      `(setf (device-register ,device (load-time-value (register-id ,regname)))
	     ,(eeval
	       `(logior ,value ,(unless write-only
                                  `(device-register ,device (register-id ,regname))))
	       `(,mask ,(lognot mask))
	       `(,(mkenv space-name (name bitfield)) nil))))))

(defmacro devbits (device regname (&rest bytenames))
  (decode-context (space-name space bitfield) regname bytenames
    `(values-list 
      (mapcar
       (rcurry #'ldb-test (device-register ,device (load-time-value (register-id ,regname))))
       ',(mapcar [bitfield-byte space] bytenames)))))

(define-setc-expander devbits (values device regname (&rest bytenames) &key write-only)
  (decode-context (space-name space bitfield) regname bytenames
    (with-gensyms (device-var reg-id-var) 
      (let* ((initial (unless write-only
                        `(device-register ,device-var ,reg-id-var)))
	     (bytes (mapcar [bitfield-byte space] bytenames)))
	`(let ((,device-var ,device)
	       (,reg-id-var (load-time-value (register-id ,regname))))
	   (setf (device-register ,device-var ,reg-id-var)
		 ,(eeval (list* 'logior initial (ensure-destructurisation bytenames values))
			 (list* (lognot (bytes-bitmask bytes)) (mapcar #'byte-bitmask bytes))
			 (list* nil (mapcar [mkenv space-name] bytenames)))))))))

(defmacro bits (bytenames &rest bytevals)
  "Combined bytevals of bitfields specified by BYTENAMES/BYTEVALS.
  Bytevals default to T, when left completely unspecified."
  (decode-context (space-name space) nil bytenames
    (let* ((bytenames (ensure-list bytenames))
	   (bytevals (or bytevals (make-list (length bytenames) :initial-element t))))
      (eeval (list* 'logior bytevals)
	     (mapcar {#'byte-bitmask [bitfield-byte space]} bytenames)
	     (mapcar [mkenv space-name] bytenames)))))

(defmacro test-devbits (device regname bytenames &rest bytevals)
  "Check if every bitfield among those specified by BYTENAMES is set to a value denoted by
   a corresponding member of BYTEVALS."
  (decode-context (space-name space bitfield) nil bytenames
    (let ((bytenames (ensure-list bytenames)))
      `(= (logand (device-register ,device (load-time-value (register-id ,regname)))
		  ,(bytes-bitmask (mapcar [bitfield-byte space] bytenames)))
	  (bits ,bytenames ,@bytevals)))))

(defmacro test-bits (&environment env (&rest bytenames) val)
  "Check if every bitfield specified by BYTENAMES is set to all 1's."
  (let ((mask (bytes-bitmask (mapcar [bitfield-byte (space (environment-space-name-context env))] bytenames))))
    `(= (logand ,val ,mask) ,mask)))

(defmacro bit-value (value bytename)
  (decode-context (space-name space) nil `(,bytename)
    `(ldb ',(bitfield-byte space bytename) ,value)))
