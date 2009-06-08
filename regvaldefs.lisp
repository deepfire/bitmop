;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGVALDEFS; Base: 10; indent-tabs-mode: nil -*-
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

(defvar *rvd-synchronous-logging-p* t)
(defvar *rvd-log-stream* t)
(defvar *verbose-device-init-p* t)

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
  (formats% nil :type list)
  spec
  (bytevals (make-hash-table) :type hash-table)
  (byterevvals (make-hash-table) :type hash-table))

(defmethod print-object ((o bitfield) stream &aux (byte (bitfield-spec o)))
  (print-unreadable-object (o stream)
    (common-lisp:format stream "~@<BITFIELD ~;~A (byte ~D ~D)~:[~;~:* values:~{ ~D:~A~}~]~:@>"
                        (name o) (byte-size byte) (byte-position byte)
                        (iter (for (name byteval) in-hashtable (bitfield-bytevals o))
                              (collect (byteval-value byteval)) (collect name)))))

(defstruct (layout (:include spaced))
  "Maps register names into register structures."
  force-multi
  registers
  register-selectors)

(defmethod print-object ((o layout) stream)
  (print-unreadable-object (o stream)
    (common-lisp:format stream "~@<LAYOUT~; ~A registers:~{ ~A~}~:@>"
                        (name o) (mapcar #'name (layout-registers o)))))

(defstruct (register (:include spaced) (:conc-name reg-))
  "Defines a formatted register, specified within a layout with a selector."
  aliases
  layout
  format
  type ext)

(defstruct (register-instance (:include docunamed) (:conc-name reginstance-))
  "Instance of register."
  device
  layout
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
(define-container-hash-accessor *device-classes* device-class :type device-class-umbrella :coercer t :iterator do-device-classes)
(define-container-hash-accessor *register-formats* format :type format :if-exists :continue)
(define-container-hash-accessor *register-spaces* register-space :type space :if-exists :error :description "register")
(define-container-hash-accessor *register-instances* register-instance :type register-instance :if-exists :error
                                :iterator do-register-instances :remover remove-register-instance)
(define-container-hash-accessor *register-instances-by-id* register-instance-by-id :type register-instance :type-allow-nil-p t :if-exists :continue)
(define-container-hash-accessor :i device :container-transform devices :parametrize-container t)
(define-container-hash-accessor :i layout :container-transform layouts :parametrize-container t :if-exists :error)
(define-container-hash-accessor :i bitfield :container-transform bitfields :parametrize-container t :if-exists :error)
(define-container-hash-accessor :i bitfield-byte :container-transform bitfield-bytes :parametrize-container t :if-exists :error :type cons)
(define-container-hash-accessor :i byteval :container-transform bitfield-bytevals :parametrize-container t :if-exists :error)
(define-container-hash-accessor :i byterevval :container-transform bitfield-byterevvals :parametrize-container t :if-exists :error :type byteval)

;; This one stands out: going through dictionaries.
(declaim (ftype (function (space symbol) register)))
(defun register (space name)
  (declare (space space) (keyword name))
  (translation (register-dictionary space) name))

(declaim (ftype (function (symbol) fixnum) register-id))
(defun register-id (name)
  (declare (keyword name))
  (symbol-id (register-dictionary (register-space name)) name))

(declaim (ftype (function (space fixnum) register) register-by-id))
(defun register-by-id (space id)
  (declare (space space) (fixnum id))
  (id-value (register-dictionary space) id))

(declaim (ftype (function (symbol symbol) fixnum) register-selector))
(defun register-selector (register-name layout-name)
  (declare (symbol register-name layout-name))
  (let* ((space (register-space register-name))
         (layout (layout space layout-name)))
    (nth (position register-name (layout-registers layout) :key #'name) (layout-register-selectors layout))))

(defparameter *dummy-fixnum-vector* (make-array 0 :element-type 'fixnum))
(defparameter *dummy-function-vector* (make-array 0 :element-type 'function))
(defparameter *dummy-space* (make-instance 'space :name :dummy :documentation "Dummy space."))

(defstruct (struct-device-class (:include docunamed))
  (selectors *dummy-fixnum-vector* :type (vector fixnum))
  (readers *dummy-function-vector* :type (vector function))
  (writers *dummy-function-vector* :type (vector function))
  (space *dummy-space* :type space)
  (instances nil :type list)
  (layouts nil :type list)
  (effective-layout-specs nil :type list)
  (constructor nil :type (function (*) struct-device-class)))

(defparameter *dummy-struct-device-class* (make-struct-device-class :constructor #'make-struct-device-class))

(defclass device-class (standard-class)
  ((selectors :accessor device-class-selectors :type (vector fixnum) :documentation "ID-indexed register selector lookup table.")
   (readers :accessor device-class-readers :type (vector function) :documentation "ID-indexed register reader lookup table.")
   (writers :accessor device-class-writers :type (vector function) :documentation "ID-indexed register writer lookup table.")
   (space :accessor device-class-space :type (or space null) :initarg :sspace)
   (layouts :accessor device-class-layouts :type list)
   (direct-layout-specs :accessor device-class-direct-layout-specs :type list :initform nil :initarg :layouts :documentation "Original layout->accessors alist.")
   (effective-layout-specs :accessor device-class-effective-layout-specs :type list :documentation "Effective layout->accessors alist."))
  (:default-initargs
   :sspace nil))

(deftype device-class-umbrella () `(or device-class struct-device-class))

(defmethod device-class-space ((o struct-device-class)) (struct-device-class-space o))
(defmethod device-class-instances ((o struct-device-class)) (struct-device-class-instances o))
(defmethod device-class-layouts ((o struct-device-class)) (struct-device-class-layouts o)) ;; for COMPUTE-INHERITED-LAYOUTS and C-D-R-I
(defmethod device-class-effective-layout-specs ((o struct-device-class)) (struct-device-class-effective-layout-specs o)) ;; for COMPUTE-INHERITED-LAYOUTS and C-D-R-I

(defclass extended-register-device-class (device-class)
  ((extended-layouts :accessor device-class-extended-layouts :type list :initarg :extended-layouts)
   (extensions :accessor device-class-extensions :type (vector simple-array) :documentation "Selector-indexed storage for extended register information.")))

(defun make-invalid-register-access-read-trap (id format-control)
  (lambda (device selector)
    (declare (ignore selector))
    (error 'invalid-register-read :device device :id id :format-control format-control)))

(defun make-invalid-register-access-write-trap (id format-control)
  (lambda (value device selector)
    (declare (ignore selector))
    (error 'invalid-register-write :value value :device device :id id :format-control format-control)))

(defmethod device-class-register-selector ((o device-class) (i #+sbcl fixnum #-sbcl integer)) (aref (device-class-selectors o) i))
(defmethod device-class-reader ((o device-class) (i #+sbcl fixnum #-sbcl integer)) (aref (device-class-readers o) i))
(defmethod device-class-writer ((o device-class) (i #+sbcl fixnum #-sbcl integer)) (aref (device-class-writers o) i))
(defmethod set-device-class-reader ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn function)) (setf (aref (device-class-readers o) i) fn))
(defmethod set-device-class-writer ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn function)) (setf (aref (device-class-writers o) i) fn))
(defmethod set-device-class-reader ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn null))
  (setf (aref (device-class-readers o) i) (make-invalid-register-access-read-trap i "~@<Disabled register read access for device ~S, register id 0x~X, register ~S.~:@>")))
(defmethod set-device-class-writer ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn null))
  (setf (aref (device-class-writers o) i) (make-invalid-register-access-write-trap i "~@<Disabled register write access of ~8,'0X for device ~S, register id 0x~X, register ~S.~:@>")))
(defsetf device-class-reader set-device-class-reader)
(defsetf device-class-writer set-device-class-writer)

(defun register-id-valid-for-device-class-p (device-class id)
  (not (minusp (device-class-register-selector device-class id))))

(defun register-name-valid-for-device-class-p (device-class register-name)
  (let* ((space (device-class-space device-class)))
    (register-id-valid-for-device-class-p device-class (symbol-id (register-dictionary space) register-name))))

(defun device-class-not-protocol-p (class)
  (not (member (class-name class) '(device extended-register-device))))

(defun device-class-p (class &aux (type (class-name class)))
  (and (subtypep type 'device) (not (member type '(device extended-register-device)))))

(defun remove-if-not-subtype-of (type types)
  (remove-if-not (rcurry #'subtypep type) types))

(defun metaclass-relevant-supers (superclasses)
  (remove-if-not-subtype-of 'device superclasses))

(defun compute-metaclass (provided-metaclass provided-superclasses)
  (let ((default-metaclass (if (first (remove-if-not-subtype-of 'extended-register-device (metaclass-relevant-supers provided-superclasses)))
                               'extended-register-device-class
                               'device-class))) ;; note how this properly defaults to 'device-class when there's no M-R-S
    (or provided-metaclass default-metaclass)))

(defun compute-superclasses (provided-metaclass provided-superclasses)
  (let ((default-superclasses (case provided-metaclass
                                (extended-register-device-class 'extended-register-device)
                                ((device-class nil) 'device))))
    (append provided-superclasses (unless (metaclass-relevant-supers provided-superclasses)
                                    (list default-superclasses)))))

(defmacro define-device-subclass (name space provided-superclasses slots &rest options)
  (let* ((provided-metaclass (second (assoc :metaclass options)))
         (metaclass (compute-metaclass provided-metaclass provided-superclasses)))
    ;; XXX: shouldn't we check for the cases when user specifies E-R-D-C and DEVICE? Would V-S catch that?
    `(progn
       (defclass ,name ,(compute-superclasses provided-metaclass provided-superclasses)
         (,@slots
          (selectors :allocation :class)
          (readers :allocation :class)
          (writers :allocation :class)
          ,@(when (eq metaclass 'extended-register-device-class)
                  `((extensions :allocation :class))))
         (:metaclass ,metaclass)
         (:space . ,space)
         ,@(remove-if (lambda (x) (member x '(:metaclass :space))) options :key #'first)) ;; YYY: REMOVE-FROM-ALIST
       #+ecl
       (eval-when (:compile-toplevel)
         (finalize-inheritance (find-class ',name)))
       (initialize-device-class (find-class ',name) (when ',space (space ',space)) ',(rest (assoc :layouts options))))))

(defmacro define-device-class (name space provided-superclasses slots &rest options)
  `(define-device-subclass ,name ,space (,@provided-superclasses)
     (,@slots
      (instances :accessor instances :type list :initarg :instances :initform nil :allocation :class))
     ,@options))

;; McCLIM-like protocol class stuff :-)
(defmacro define-protocol-device-class (name space provided-superclasses slots &rest options)
  `(progn
     (define-device-class ,name ,space (,@provided-superclasses)
       (,@slots)
       ,@options)
     (let ((the-class (find-class ',name)))
       (defmethod initialize-instance :after ((o ,name) &key &allow-other-keys)
         (when (eq (class-of o) the-class)
           (error 'protocol-class-instantiation :class (class-of o)))))))

(defmethod validate-superclass ((class device-class) (superclass standard-class)) t)

(defmethod validate-superclass ((class extended-register-device-class) (superclass device-class)) t)

(defmacro define-struct-device-class (name space slots &rest options)
  `(progn
     (defstruct (,name (:include struct-device))
       (,@slots))
     (initialize-struct-device-class (make-struct-device-class :name ,name ,@(when-let ((documentation (second (assoc :documentation options))))
                                                                               `(:documentation ,documentation))
                                                               :constructor (function ,(format-symbol (symbol-package name) "MAKE-~A" name)))
                                     (space ,space) ',(rest (assoc :layouts options)))))

(defstruct struct-device
  (class *dummy-struct-device-class* :type struct-device-class)
  (id 0 :type fixnum)
  (backend nil :type (or null struct-device)))

(defclass device ()
  ((selectors :accessor device-selectors :type (vector fixnum) :allocation :class) ; copied over from class
   (readers :accessor device-readers :type (vector function) :allocation :class)                     ; ...
   (writers :accessor device-writers :type (vector function) :allocation :class)                     ; ...
   (id :accessor device-id :type (integer 0))
   (backend :accessor device-backend :type (or null device) :initarg :backend)
   (category :initarg :category) ;; this might go away
   )
  (:metaclass device-class))

(defgeneric class-of-device (device)
  (:documentation "Return the DEVICE's metaclass, or device class structure,
                   depending on its type.")
  (:method ((o struct-device)) (struct-device-class o))
  (:method ((o device)) (class-of o)))

(defmethod device-id ((o struct-device)) (struct-device-id o))
(defmethod instances ((o struct-device)) (struct-device-class-instances (struct-device-class o)))
(defgeneric (setf instances) (value device))

(defmethod device-reader ((device device) register-id) (aref (device-readers device) register-id))
(defmethod device-writer ((device device) register-id) (aref (device-writers device) register-id))
(defmethod set-device-reader ((device device) register-id value) (setf (aref (device-readers device) register-id) value))
(defmethod set-device-writer ((device device) register-id value) (setf (aref (device-writers device) register-id) value))
(defsetf device-reader set-device-reader)
(defsetf device-writer set-device-writer)

(defun print-device-object (device stream)
  (labels ((slot (id) (if (slot-boundp device id) (slot-value device id) :unbound-slot)))
    (cl:format stream "~@<#<~;~A-~A~;>~:@>" (type-of device) (slot 'id))))

(defmethod print-object ((device device) stream)
  (print-device-object device stream))

(defclass slave-device (device)
  ((master :accessor slave-device-master :initarg :master))
  (:metaclass device-class))

(defclass extended-register-device (device)
  ((extensions :accessor device-extensions :type (vector simple-array) :allocation :class)) ; copied over from class
  (:metaclass extended-register-device-class))

(declaim (ftype (function (device fixnum) fixnum) device-register-selector))
(defun device-register-selector (device id)
  (declare (device device) (fixnum id))
  (aref (device-selectors device) id))

(defun class-current-slot-allocation (class slot)
  (if (slot-boundp class slot) (length (slot-value class slot)) 0))

(defgeneric class-pool-boundp (class slot)
  (:documentation "Determine, whether CLASS's SLOT is bound.
                   SLOT must represent a map -- that is, either a selector,
                   a reader or a writer map.")
  (:method ((o struct-device-class) slot-name)
    (not (member (slot-value o slot-name) (list *dummy-fixnum-vector* *dummy-function-vector*))))
  (:method ((o device-class) slot-name)
    (slot-boundp o slot-name)))

(defun ensure-device-class-map-storage (device-class space)
  "Ensure that map storage of DEVICE-CLASS is enough to cover all registers 
   in SPACE."
  (declare (type (or struct-device-class device-class) device-class))
  (let ((required-length (length (dictionary-id-map (register-dictionary space)))))
    (flet ((make-or-extend-pool (type initial-element old-pool)
             (concatenate (list 'vector type)
                          (or old-pool (make-array required-length :element-type type :initial-element initial-element))
                          (unless old-pool
                            (make-list (- required-length (length old-pool)) :initial-element initial-element))))
           (rev-iota (start n)
             (iota n :start (- -1 start) :step -1))
           (irart-iota (start n)
             (mapcar (rcurry #'make-invalid-register-access-read-trap "~@<Undefined register read for device ~S, register id 0x~X, register ~S.~:@>")
                     (iota n :start start)))
           (irawt-iota (start n)
             (mapcar (rcurry #'make-invalid-register-access-write-trap "~@<Undefined register write of ~8,'0X for device ~S, register id 0x~X, register ~S.~:@>")
                     (iota n :start start)))
           (initialise-pool-tail (pool start n initialiser)
             (setf (subseq pool start) (funcall initialiser start n))))
      (iter (for (slot-name type initialiser initial) in `((selectors fixnum ,#'rev-iota 0)
                                                           (readers function ,#'irart-iota #'break)
                                                           (writers function ,#'irawt-iota #'break)))
            (for old-allocation = (class-current-slot-allocation device-class slot-name))
            (setf (slot-value device-class slot-name)
                  (cond ((zerop required-length) (make-array 0))
                        ((>= old-allocation required-length) (slot-value device-class slot-name))
                        (t (lret ((new-pool (make-or-extend-pool
                                             type initial (if (class-pool-boundp device-class slot-name) (slot-value device-class slot-name) nil))))
                                 (initialise-pool-tail new-pool old-allocation (- required-length old-allocation) initialiser)))))))))

(defmacro y (lambda-list &body body)
  "Idiomatic, ignore-saving lambda macro."
  (iter (for var in lambda-list)
        (if var (collect var into binds)
            (let ((var (gensym))) (collect var into binds) (collect var into ignores)))
        (finally (return `(lambda (,@binds) (declare (ignore ,@ignores)) ,@body)))))
(defun mk-f-cdrwalk (s &aux (r s)) (y (nil nil) (prog1 (car r) (setf r (cdr r)))))

(defun mapc-layout-register-ids (fn layout &aux (dictionary (register-dictionary (layout-space layout))))
  (iter (for register in (layout-registers layout))
        (funcall fn (symbol-id dictionary (name register)))))

(defun compute-accessor-function (name reader-p id)
  (if (typep name 'boolean) 
      (if reader-p
          (make-invalid-register-access-read-trap id (if name
                                                         "~@<Not-yet-initialised register read for device ~S, register id 0x~X, register ~S.~:@>"
                                                         "~@<Undefined register read for device ~S, register id 0x~X, register ~S.~:@>"))
          (make-invalid-register-access-write-trap id (if name
                                                         "~@<Not-yet-initialised register write of ~8,'0X for device ~S, register id 0x~X, register ~S.~:@>"
                                                         "~@<Undefined register write of ~8,'0X for device ~S, register id 0x~X, register ~S.~:@>")))
      (fdefinition name)))

(defun map-add-layout-specs (space layout-specs map fn-maker &optional values)
  "Replace sets of entries in MAP, which correspond to successive layout register id sets
   referenced by LAYOUT-SPECS, with result of application of the function
   made by FN-MAKER, accordingly with the per-layout information provided in
   LAYOUT-SPECS and a corresponding member of VALUES, if any.

   The replacement value is computed by application of the per-layout made function
   to the corresponding register id and the old value."
  (iter (for (name reader writer) in layout-specs)
        (let* ((layout (layout space name))
               (fn (funcall fn-maker layout reader writer (pop values))))
          (mapc-layout-register-ids (lambda (id) (setf (aref map id) (funcall fn (aref map id) id))) layout))))

(defun compute-inherited-layouts (direct-layout-instances eligible-parents)
  (values (set-difference (remove-duplicates (apply #'append (mapcar #'device-class-layouts eligible-parents))) 
                          direct-layout-instances)
          (set-difference (remove-duplicates (apply #'append (mapcar #'device-class-effective-layout-specs eligible-parents)) :key #'car) 
                          (mapcar (compose #'list #'name) direct-layout-instances)
                          :key #'car)))

(defgeneric initialize-device-class (device-class space direct-layout-specs)
  (:documentation
   "Initialize DEVICE-CLASS according to SPACE and DIRECT-LAYOUT-SPECS.

   SPACE must be an instance of type SPACE.
   LAYOUT-SPECS is interpreted as a list of three-element sublists, each one
   containing a layout name and two accessor specifications, for both the
   reader and writer to be used for accessing that layout with instances made
   using DEVICE-CLASS.

   Accessor specifications bear one of possible following meanings, 
   with regard to corresponding accessor pools:
      - T, the pool is manually managed,
      - NIL, the pool is disabled, initialized to functions raising an error,
      - any other symbol, or setf function designator, serving as a name of
        a function used to initialize the pool.")
  (:method ((o struct-device-class) space direct-layout-specs)
    (declare (type space space) (type list direct-layout-specs))
    (let ((direct-layout-instances (mapcar (compose (curry #'layout space) #'first) direct-layout-specs)))
      (setf (struct-device-class-layouts o) direct-layout-instances
            (struct-device-class-effective-layout-specs o) direct-layout-specs
            (device-class (struct-device-class-name o)) o)
      ;; allocate storage
      (ensure-device-class-map-storage o (setf (struct-device-class-space o) space))
      ;; compute and patch selector/reader/writer maps
      (with-slots (selectors readers writers) o
        (mapc (curry #'apply #'map-add-layout-specs space direct-layout-specs)
              `((,selectors ,(y (l nil nil nil) (mk-f-cdrwalk (layout-register-selectors l))))
                (,readers   ,(y (nil r nil nil) (y (old id) (if (eq r t) old (compute-accessor-function r t id)))))
                (,writers   ,(y (nil nil w nil) (y (old id) (if (eq w t) old (compute-accessor-function w nil id))))))))))
  (:method ((o device-class) space direct-layout-specs)
    (declare (type (or null space) space) (type list direct-layout-specs))
    #+ecl
    (unless (class-finalized-p o)
      (finalize-inheritance o))
    (if space
        (let ((direct-layout-instances (mapcar (compose (curry #'layout space) #'first) direct-layout-specs))
              (eligible-parents (remove-if-not (lambda (pc) (and (device-class-p pc) (device-class-space pc))) (class-direct-superclasses o))))
          (when-let ((misspaced (remove-if (curry #'eq space) (mapcar #'device-class-space eligible-parents))))
            (error 'cross-space-inheritance
                   :class (class-name o) :required-space (space-name space) :actual-spaces (mapcar #'space-name misspaced)))
          (multiple-value-bind (inherited-layout-instances inherited-layout-specs) (compute-inherited-layouts direct-layout-instances eligible-parents)
            ;; compute effective layouts specs, effective layouts and register device class
            (setf (device-class-effective-layout-specs o) (append inherited-layout-specs direct-layout-specs)
                  (device-class-layouts o) (append inherited-layout-instances direct-layout-instances)
                  (device-class (class-name o)) o)
            ;; allocate selector/reader/writer map storage
            (ensure-device-class-map-storage o (setf (device-class-space o) space))
            ;; compute and patch selector/reader/writer maps
            (with-slots (selectors readers writers) o
              (let ((providing-parents (mapcar (rcurry #'find eligible-parents :key #'device-class-layouts :test #'member) inherited-layout-instances)))
                (mapc (curry #'apply #'map-add-layout-specs space)
                      `((,direct-layout-specs    ,selectors ,(y (l nil nil nil) (mk-f-cdrwalk (layout-register-selectors l))))
                        (,direct-layout-specs    ,readers   ,(y (nil r nil nil) (y (old id) (if (eq r t) old (compute-accessor-function r t id)))))
                        (,direct-layout-specs    ,writers   ,(y (nil nil w nil) (y (old id) (if (eq w t) old (compute-accessor-function w nil id)))))
                        (,inherited-layout-specs ,selectors ,(y (nil nil nil p) (y (nil id) (aref (device-class-selectors p) id))) ,providing-parents)
                        (,inherited-layout-specs ,readers   ,(y (nil nil nil p) (y (nil id) (aref (device-class-readers p) id))) ,providing-parents)
                        (,inherited-layout-specs ,writers   ,(y (nil nil nil p) (y (nil id) (aref (device-class-writers p) id))) ,providing-parents)))))))
        (if direct-layout-specs
            (error 'spaceless-layout-reference :class (class-name o))
            ;; Messing with initargs would be way too painful...
            (with-slots (space layouts direct-layout-specs effective-layout-specs selectors readers writers) o
              (setf (values space layouts direct-layout-specs effective-layout-specs selectors readers writers)
                    (values nil nil nil nil (make-array 0 :element-type 'fixnum)
                            (make-array 0 :element-type 'function :initial-element #'identity) (make-array 0 :element-type 'function :initial-element #'identity))))))))

(defun reinitialize-device-class (device-class)
  "Reinitialize DEVICE-CLASS according to its SPACE and DIRECT-LAYOUT-SPECS slots."
  (initialize-device-class device-class (device-class-space device-class) (device-class-direct-layout-specs device-class)))

(defun device-class-corresponds-to-space-and-layouts-p (device-class space-name layout-specs)
  (when-let ((present-space (if (typep device-class 'struct-device-class)
                                (device-class-space device-class)
                                (slot-value* device-class 'space nil))))
    (and (eq present-space (when space-name (space space-name)))
         (equal (slot-value* device-class 'direct-layout-specs nil) layout-specs))))

(defun maybe-reinitialize-device-class (device-class space-name direct-layout-specs)
  "Reinitialize an already defined DEVICE-CLASS according to SPACE-NAME and 
   DIRECT-LAYOUT-SPECS, if they differ from stored values."
  (unless (device-class-corresponds-to-space-and-layouts-p device-class space-name direct-layout-specs)
    (with-slots (space (direct-layout-specs-slot direct-layout-specs)) device-class
      (setf (values space direct-layout-specs-slot) (values (when space-name (space space-name)) direct-layout-specs)))
    (reinitialize-device-class device-class)))

;;;
;;; XXX: not pretty: hack around non-&allow-other-keys-able initargs...
;;;
(defmethod initialize-instance ((o device-class) &rest initargs)
  (apply #'shared-initialize o t (remove-from-plist initargs :space)))
(defmethod reinitialize-instance ((o device-class) &rest initargs)
  (apply #'shared-initialize o nil (remove-from-plist initargs :space)))
(defmethod initialize-instance :after ((o device-class) &key space &allow-other-keys)
  (declare (ignore space)))

(defun build-device-class-extension-map (space layout-names &aux (length 0))
  (let ((candidate-extensions
         (iter outer
               (for layout-name in layout-names)
               (for layout = (layout space layout-name))
               (iter (for register in (layout-registers layout))
                     (for selector in (layout-register-selectors layout))
                                           
                     (maxf length (1+ selector))
                     (in outer (collect (cons selector (map 'vector #'identity
                                                            (cons register (or (ensure-list (reg-ext register)) (list nil)))))))))))
    (unless (= (length (remove-duplicates candidate-extensions :key #'car))
               (length candidate-extensions))
      (error "~@<Cannot build a register extension map for intersecting layouts: ~S~:@>" layout-names))
    (lret ((extension-map (coerce (make-array length :initial-element (make-array 1 :initial-element nil)) 'vector)))
          (iter (for (selector . extension) in candidate-extensions)
                (setf (aref extension-map selector) extension)))))

(defmethod initialize-device-class :after ((o extended-register-device-class) space direct-layouts)
  (declare (ignore space direct-layouts))
  (let ((extended-layouts (slot-value* o 'extended-layouts nil)))
    (when (device-class-not-protocol-p o)
      (when-let ((missing (remove-if (rcurry #'assoc (device-class-effective-layout-specs o)) extended-layouts)))
        (error "~@<During initialization of extended register device class ~S: unknown layouts were specified to be extended: ~S~:@>"
               (class-name o) missing))
      ;; XXX: no inheritance for these maps... complicated composition. Not unsurmountable, though.
      (setf (device-class-extensions o) (build-device-class-extension-map (device-class-space o) extended-layouts)))))

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

(defun device-register-instance-name (device layout name &aux (namestring (string name)))
  "Complete a register instance name given NAME and LAYOUT of DEVICE."
  (let* ((dot-posn (position #\. namestring))
         (qualify (or (layout-force-multi layout)
                      (> (length (instances device)) 1))))
    (make-keyword (concatenate 'string
                               (cond (dot-posn (subseq namestring 0 dot-posn))
                                     (qualify (string (device-type device))))
                               (when qualify (write-to-string (device-id device)))
                               (when (or dot-posn qualify) ".")
                               namestring))))

(defmacro do-device-class-registers ((layout reader-name writer-name register selector) device-class
                                     &body body &aux (layout-var (or layout (gensym))))
  "Execute BODY with LAYOUT, REGISTER, SELECTOR, READER-NAME and WRITER-NAME
   bound to corresponding values for every register defined in DEVICE-CLASS.

   Any variable name can be specified as NIL, which is intepreted as a
   request to ignore that binding."
  (once-only (device-class)
    `(iter (for ,layout in (device-class-layouts ,device-class))
           ,@(when (or reader-name writer-name)
                   `((for (nil ,reader-name ,writer-name) in (device-class-effective-layout-specs ,device-class))))
           (iter ,@(when register `((for ,register in (layout-registers ,layout-var))))
                 ,@(when selector `((for ,selector in (layout-register-selectors ,layout-var))))
                 ,@body))))

(defun purge-device-register-instances (device)
  "Purge all register instances associated with DEVICE."
  (do-device-class-registers (layout nil nil register nil) (class-of-device device)
    (let* ((ri-name (device-register-instance-name device layout (name register)))
           (ri (register-instance ri-name)))
      (setf (register-instance-by-id (reginstance-id ri)) nil) ; Not REMHASH: that will screw up id allocation.
      (mapc #'remove-register-instance (cons ri-name (mapcar (curry #'device-register-instance-name device layout)
                                                             (reg-aliases register)))))))

(defun create-device-register-instances (device &aux (device-class (class-of-device device)))
  "Walk the DEVICE's layouts and spawn the broodlings."
  (with-retry-restarts ((retry ()
                          :test (lambda (c) (typep c 'bad-redefinition))
                          :report "Purge device register instances and retry their creation."
                          (purge-device-register-instances device)))
    (do-device-class-registers (layout reader-name writer-name register selector) device-class
      (let* ((main-ri-name (device-register-instance-name device layout (name register)))
             (id (1+ (hash-table-count *register-instances-by-id*)))
             (reg-id (register-id (name register)))
             (instance (make-register-instance :name main-ri-name :register register :device device :selector selector :id id :layout layout
                                               :reader (device-class-reader device-class reg-id)
                                               :writer (device-class-writer device-class reg-id))))
        (setf (register-instance-by-id id) instance)
        (iter (for ri-name in (cons main-ri-name (mapcar (curry #'device-register-instance-name device layout)
                                                (reg-aliases register))))
              (assert ri-name)
              (setf (register-instance ri-name) instance))))))

(defun make-struct-device-instance (type &rest initargs)
  (lret* ((class (device-class type))
          (space (struct-device-class-space class))
          (instance (apply (struct-device-class-constructor class) :id (1- (length (struct-device-class-instances class))) initargs)))
    (push instance (struct-device-class-instances class))
    (setf (gethash (device-hash-id instance) (devices space)) instance)
    (create-device-register-instances instance)))

(defmethod initialize-instance :after ((device device) &key &allow-other-keys)
  (let* ((device-class (class-of device))
         (space (device-class-space device-class)))
    (unless space
      (error 'device-type-not-directly-instantiable :type (type-of device)))
    (push device (instances device))
    (setf (device-id device) (1- (length (instances device)))
          (device-selectors device) (device-class-selectors device-class)
          (device-readers device) (device-class-readers device-class)
          (device-writers device) (device-class-writers device-class)
          ;; register within space
          (gethash (device-hash-id device) (devices space)) device)
    ;; (cl:format t "Initialized ~S/~S: ~S, ~S, ~S~%" (class-name (class-of device)) (device-id device) (device-selectors device) (device-readers device) (device-writers device))
    (create-device-register-instances device)))

(defmethod initialize-instance :after ((device extended-register-device) &key &allow-other-keys)
  (setf (device-extensions device) (device-class-extensions (class-of device))))

(defmethod initialize-instance :around ((device device) &key &allow-other-keys)
  (when *verbose-device-init-p*
    (let ((stream (if (eq *rvd-log-stream* t) *standard-output* *rvd-log-stream*)))
      (write (string (type-of device)) :stream stream :escape nil)
      (write-char #\Space stream)
      (when *rvd-synchronous-logging-p*
        (finish-output stream))))
  (handler-bind ((error (lambda (c)
                          (purge-device-register-instances device)
                          (error c))))
    (call-next-method)))

(defun space-device-count (space)
  (hash-table-count (devices space)))

(defun space-remove-device (device)
  (let* ((device-class (class-of device))
         (space (device-class-space device-class)))
    (remhash (device-hash-id device) (devices space))
    (purge-device-register-instances device)
    (removef (instances device) device)))

(defun init-device-model ()
  "Forget all known device and register instances."
  (iter (for (nil space) in-hashtable *spaces*)
        (when (atom (space-name space))
          (clrhash (devices space))))
  (do-device-classes (device-class)
    (unless (class-finalized-p device-class)
      (finalize-inheritance device-class))
    (for prototype = (class-prototype device-class))
    (when (slot-exists-p prototype 'instances)
      (setf (slot-value prototype 'instances) nil)))
  (clrhash *register-instances*)
  (clrhash *register-instances-by-id*))

(define-condition bit-notation-error (error) ())

(define-condition definition-error (bit-notation-error) ())

(define-condition space-definition-error (definition-error)
  ((space :initarg :space)))

(define-condition simple-space-definition-error (space-definition-error simple-error)
  ())

(define-reported-condition bitfields-divergent (space-definition-error)
  ((bitfields :initarg :bitfields))
  (:report (bitfields space) "~@<Unable to find a common format for bitfields ~{ ~A~} in space ~S~:@>" bitfields space))

(define-reported-condition bitfields-unknown (bit-notation-error)
  ((bitfields :initarg :bitfields))
  (:report (bitfields) "~@<Unable to find a space for bitfields: ~{ ~A~}~:@>" bitfields))

(define-reported-condition invalid-register-selectors-in-layout-definition (space-definition-error)
  ((layout :initarg :layout)
   (bad-selectors :initarg :bad-selectors))
  (:report (layout space bad-selectors)
           "~@<In definition of layout ~S in space ~S: register selectors~{ ~A~} must be of type FIXNUM.~:@>" layout space bad-selectors))

(define-reported-condition incompatible-bitfield-redefinition (space-definition-error)
  ((bitfield :initarg :bitfield)
   (to :initarg :to))
  (:report (space bitfield to) "~@<In definition of space ~S: attempt to incompatibly redefine bitfield ~A to ~A.~:@>" space bitfield to))

(define-condition device-class-definition-error (definition-error)
  ((class :initarg :class)))

(define-reported-condition spaceless-layout-reference (device-class-definition-error)
  ()
  (:report (class)
           "~@<During initialization of device class ~S: layouts can not be specified without space.~:@>" class))

(define-reported-condition cross-space-inheritance (device-class-definition-error)
  ((required-space :initarg :required-space)
   (actual-spaces :initarg :actual-spaces))
  (:report (class required-space actual-spaces)
           "~@<During initialization of device class ~S: cannot do cross-space inheritance: ~S vs. ~S.~:@>"
           class required-space actual-spaces))

(define-reported-condition protocol-class-instantiation (bit-notation-error)
  ((class :initarg :class))
  (:report (class) "~@<Protocol device class ~S is not meaned to be directly instantiated.~:@>" class))

(define-reported-condition underspecified-context (bit-notation-error)
  ()
  (:report () "~@<Impossible to deduce context: neither register name, nor byte names were specified.~:@>"))

(define-reported-condition namespace-unification-conflict (bit-notation-error)
  ((namespaces :initarg :namespaces)
   (slot :initarg :slot)
   (key :initarg :key))
  (:report (namespaces slot key) "Conflict during namespace unification. Namespaces ~S, slot ~S, key ~S." namespaces slot key))

(define-reported-condition device-type-not-directly-instantiable (bit-notation-error)
  ((type :initarg :type))
  (:report (type) "~@<Device type ~S is not directly instantiable: it claims no space.~:@>" type))

(define-reported-condition no-space-context (bit-notation-error)
  ()
  (:report () "~@<Attempt to use names in a null space context.~:@>"))

(define-reported-condition conflicting-bitfield-names (bit-notation-error)
  ((conflicting-bitfield :initarg :conflicting-bitfield)
   (expected-format :initarg :expected-format))
  (:report (conflicting-bitfield expected-format)
           "~@<Bitfield ~S does not belong to register format ~S.~:@>" conflicting-bitfield expected-format))

(define-condition invalid-register-access (bit-notation-error)
  ((device :initarg :device)
   (id :initarg :id)
   (format-control :initarg :format-control)))

(define-reported-condition invalid-register-read (invalid-register-access)
  ()
  (:report (device id format-control)
           format-control device id (register-by-id (device-class-space (class-of-device device)) id)))

(define-reported-condition invalid-register-write (invalid-register-access)
  ((value :initarg :value))
  (:report (value device id format-control)
           format-control value device id (register-by-id (device-class-space (class-of-device device)) id)))

(define-reported-condition invalid-device-register (bit-notation-error)
  ((device :initarg :device)
   (register :initarg :register))
  (:report (register device)
           "~@<Register ~S is not referred by layouts of ~S.~:@>" register device))

(defun device-register-layout (device name &key (if-does-not-exist :error))
  "Return the layout of a DEVICE's register, who goes by NAME."
  (or (iter (for layout in (device-class-layouts (class-of-device device)))
            (finding layout such-that (find name (layout-registers layout) :key #'name)))
      (case if-does-not-exist
        (:continue nil)
        (:error (error 'invalid-device-register :device device :register name)))))

(defun device-register-instance (device name)
  "Return the instance of a DEVICE's register, who goes by NAME."
  (register-instance (device-register-instance-name device (device-register-layout device name) name)))

(defun bitfield-formats (space bitfield-name)
  "Yield the format of BITFIELD-NAMEd in SPACE"
  (bitfield-formats% (bitfield space bitfield-name)))

(defun register-format-field-type (name)
  (format-symbol t "~A-BITFIELD" name))

(defun define-byteval (bitfield byte value name &optional (documentation ""))
  (lret ((byteval (make-byteval :name name :byte byte :value value :documentation documentation)))
    (setf (byteval bitfield name) byteval
          (byterevval bitfield value) byteval)))

(defun bytevals-equal-p (b1 b2)
  (or (eq b1 b2)
      (and (eq (name b1) (name b2))
           (equal (byteval-byte b1) (byteval-byte b1))
           (= (byteval-value b1) (byteval-value b2))
           (string= (documentation b1) (documentation b2)))))

(defun bitfields-equal-p (b1 b2)
  (or (eq b1 b2)
      (and (equal (bitfield-spec b1) (bitfield-spec b2))
           (every #'bytevals-equal-p (hash-table-values (bitfield-bytevals b1)) (hash-table-values (bitfield-bytevals b1))))))

(defun ensure-bitfield (format name size pos doc &optional byteval-specs)
  (let* ((byte (byte size pos))
         (space (format-space format))
         (bitfield (make-bitfield :name name :spec byte :documentation doc)))
    (mapc (curry #'apply #'define-byteval bitfield byte) byteval-specs)
    (let ((incumbent (bitfield space name :if-does-not-exist :continue)))
      (if incumbent 
          (unless (bitfields-equal-p bitfield incumbent)
            (error 'incompatible-bitfield-redefinition :space (space-name space) :bitfield incumbent :to bitfield))
          (dolist (space (list* space (mapcar #'space (space-referrers space))))
            (setf (bitfield space name) bitfield
                  (bitfield-byte space name) byte)))
      (lret ((elected-bitfield (or incumbent bitfield)))
        (push format (bitfield-formats% elected-bitfield))))))

;; an ability to pluck in a QUOTE would've been very nice...
(defun define-register-format-notype (space name documentation bitspecs)
  (let ((format (make-format :name name :documentation documentation :space space)))
    (setf (format-bitfields format) (mapcar [apply [ensure-bitfield format]] bitspecs)
          (format name) format)))

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

(defun ensure-layout (space name documentation register-specs force-multi)
  (let ((selectors (mapcar #'second register-specs)))
    (when-let ((bad-selectors (remove-if (of-type 'fixnum) selectors)))
      (error 'invalid-register-selectors-in-layout-definition :space (space-name space) :layout name :bad-selectors bad-selectors))
    (lret ((layout (make-layout :name name :space space :documentation documentation
                                :register-selectors selectors :force-multi force-multi)))
      (setf (layout space name) layout
            (layout-registers layout) (iter (for (name selector . rest) in register-specs)
                                            (collect (apply #'define-register layout name rest)))))))

(defmacro define-layout (&environment env (name doc &key force-multi) &rest defs)
  `(ensure-layout (space ,(space-name (space (environment-space-name-context env)))) ',name ,doc ',defs ,force-multi))

;;;
;;;  o  layout templates
;;;  o  register instantiation
;;;;    - what information is deduced from register instances /now/?
;;;;      ... survey
;;;
(defmacro define-namespace (name &body f)
  (iter (for (clause . nil) in f)
        (unless (member clause '(:documentation :register-formats :layouts))
          (error 'simple-space-definition-error
                 :space name
                 :format-control "~@<in: ~A: unknown clause ~S~:@>"
                 :format-arguments (list 'define-namespace clause))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
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
                   (error 'namespace-unification-conflict :namespaces names :slot accessor-name :key key))
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
      (error 'no-space-context))
    space-name))

(defmacro space-name-context (&environment env)
  (list 'quote (environment-space-name-context env)))

(defmacro with-namespaces ((&rest nsnames) &body body)
  (let* ((need-unification (> (length nsnames) 1))
         (name (if need-unification nsnames (first nsnames))))
    `(progn
       ,@(when need-unification
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (symbol-macrolet ((*space* ,name))
         ,@body))))

(defmacro set-namespace (&rest nsnames)
  (let* ((need-unification (> (length nsnames) 1))
         (name (if need-unification nsnames (first nsnames))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when need-unification
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (define-symbol-macro *space* ,name))))

(defun bitfield-decode (bitfield value &key (symbolise-unknowns t))
  (declare (type bitfield bitfield) (type (unsigned-byte 32) value))
  (cond ((plusp (hash-table-count (bitfield-bytevals bitfield))) ;; bitfield-enumerated-p?
         (let* ((val (ldb (bitfield-spec bitfield) value)))
           (if-let ((field (byterevval bitfield val :if-does-not-exist :continue)))
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
(defun formats-with-bytenames (space bytenames)
  (reduce #'intersection (mapcar (curry #'bitfield-formats space) bytenames)))

(defun bytenames-check-format (format bytenames)
  "Raise an error when any of BYTENAMES do not have FORMAT associated 
   with them."
  (if-let ((stray (find-if-not {[member format] [bitfield-formats (spaced-space format)]} bytenames)))
          (error 'conflicting-bitfield-names :expected-format (name format) :conflicting-bitfield stray)))

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
          (return-from find-space-with-bytenames space)))
  (error 'bitfields-unknown :bitfields bytenames))

(defmacro decode-context ((spacename &optional space-want bitfield fmtname) regname bytenames &body body)
  (unless (or regname bytenames)
    (error 'underspecified-context))
  (let ((space (or space-want (gensym)))
        (newbytenames (gensym)))
    (with-gensyms (format)
      `(let* ((,space ,(if regname
                           `(register-space ,regname)
                           `(find-space-with-bytenames (ensure-list ,bytenames))))
              (,spacename (space-name ,space))
              (,format (if ,regname
                           (reg-format (register ,space ,regname))
                           (if-let ((formats (formats-with-bytenames ,space (ensure-list ,bytenames))))
                             (first formats) ; Bytenames were collated to be equivalent, so ambiguity is harmless.
                             (error 'bitfields-divergent-in-space :bitfields ,bytenames :space ,space))))
              ,@(when fmtname `((,fmtname (name ,format))))
              ,@(when bytenames `((,newbytenames (ensure-list ,bytenames))))
              ,@(when (and bitfield bytenames) `((,bitfield (bitfield ,space (car ,newbytenames))))))
         (declare (ignorable ,spacename ,space ,format
                             ,@(when bytenames `(,newbytenames))
                             ,@(when (and bitfield bytenames) `(,bitfield))))
         ,@(when bytenames `((bytenames-check-format ,format ,newbytenames)))
         ,@body))))

;;;
;;; So, there are two register access schemes.
;;; First way is to access registers via combination of device information
;;; and abstract register name. This is good for debugger-internal code.
;;; The second one derives all information from unique, device-specific
;;; register instances. This is better suited for interactive use.
;;;
(defun device-register (device register-id)
  (declare (type device device) (type fixnum register-id))
  (funcall (aref (device-readers device) register-id) device (aref (device-selectors device) register-id)))

(defun set-device-register (device register-id value)
  (declare (type device device) (type (unsigned-byte 32) value))
  (funcall (aref (device-writers device) register-id) value device (aref (device-selectors device) register-id)))

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

(defmacro place-bit (place regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    `(ldb-test ',(bitfield-byte space bytename) ,place)))
  
(defmacro place-bit-value (place regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    `(ldb ',(bitfield-byte space bytename) ,place)))

(define-setc-expander place-bit (value place regname bytename &key write-only)
  (decode-context (space-name space bitfield) regname `(,bytename)
    (let ((mask (byte-bitmask (bitfield-spec bitfield))))
      `(setf ,place (logior ,@(unless write-only `((logand ,(lognot mask) ,place)))
                            ,(eeval value
                                    `(,mask)
                                    `(,(mkenv space-name (name bitfield)))))))))

(defmacro place-bits (place regname (&rest bytenames))
  (decode-context (space-name space bitfield) regname bytenames
    `(values-list 
      (mapcar (rcurry #'ldb-test ,place) ',(mapcar [bitfield-byte space] bytenames)))))

(define-setc-expander place-bits (values place regname (&rest bytenames) &key write-only)
  (decode-context (space-name space bitfield) regname bytenames
    (let* ((bytes (mapcar [bitfield-byte space] bytenames)))
      `(setf ,place (logior ,@(unless write-only `((logand ,(lognot (bytes-bitmask bytes)) ,place)))
                            ,(eeval (list* 'logior (ensure-destructurisation bytenames values))
                                    (list* (mapcar #'byte-bitmask bytes))
                                    (list* (mapcar [mkenv space-name] bytenames))))))))

(defmacro test-bits (place bytenames &rest bytevals)
  "Check if every bitfield among those specified by BYTENAMES is set to a value denoted by
   a corresponding member of BYTEVALS."
  (decode-context (space-name space) nil bytenames
    (let ((bytenames (ensure-list bytenames)))
      `(= (logand ,place ,(bytes-bitmask (mapcar [bitfield-byte space] bytenames)))
          (bits ,bytenames ,@bytevals)))))

(defmacro test-bits-set (&environment env (&rest bytenames) val)
  "Check if every bitfield specified by BYTENAMES is set to all 1's."
  (let ((mask (bytes-bitmask (mapcar [bitfield-byte (space (environment-space-name-context env))] bytenames))))
    `(= (logand ,val ,mask) ,mask)))

(defmacro bit-value (value bytename)
  (decode-context (space-name space) nil `(,bytename)
    `(ldb ',(bitfield-byte space bytename) ,value)))

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
