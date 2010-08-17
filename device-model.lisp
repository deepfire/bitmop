;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DEVMODEL; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2010 by
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

(in-package :devmodel)

;;;
;;; Logging
;;;
(defvar *devmodel-log-stream* t)
(defvar *devmodel-synchronous-logging-p* t)
(defvar *devmodel-verbose-device-init-p* nil)

;;;
;;; Conditions
;;;
(define-condition device-model-condition () ())
(define-condition device-model-error () ())
(define-simple-error device-model-error)

(define-condition enumeration-pool-condition () 
  ((pool :initarg :pool)))
(define-reported-condition enumeration-pool-class-missing-error (device-model-error enumeration-pool-condition)
  ((class :initarg :class))
  (:report (pool class) "~@<Class ~S missing from ~S.~:@>" class pool))
(define-reported-condition enumeration-pool-id-missing-error (device-model-error enumeration-pool-condition)
  ((class :initarg :class)
   (id :initarg :id))
  (:report (pool class id) "~@<Id ~S missing from class ~S of ~S.~:@>" id class pool))

(define-condition device-class-definition-error (device-model-error)
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

(define-reported-condition protocol-class-instantiation (device-model-error)
  ((class :initarg :class))
  (:report (class) "~@<Protocol device class ~S is not meaned to be directly instantiated.~:@>" class))

(define-reported-condition device-type-not-directly-instantiable (device-model-error)
  ((type :initarg :type))
  (:report (type) "~@<Device type ~S is not directly instantiable: it claims no space.~:@>" type))

(define-condition invalid-register-access (device-model-error)
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

(define-reported-condition invalid-device-register (device-model-error)
  ((device :initarg :device)
   (register :initarg :register))
  (:report (register device)
           "~@<Register ~S is not referred by layouts of ~S.~:@>" register device))

;;;
;;; Enumeration pool
;;;
(defclass enumerated ()
  ((id :reader enumerated-id)
   (class :reader enumerated-class)
   (pool :reader enumerated-pool)))

(defun enumerated-class-name (enumerated)
  (enumclass-name (enumerated-class enumerated)))

(defstruct (enumeration-class (:conc-name enumclass-))
  (name nil :type symbol)
  (pool nil :type enumeration-pool)
  (root (make-hash-table) :type hash-table))

(defstruct (enumeration-pool (:conc-name enumpool-))
  (root (make-hash-table) :type hash-table))

(define-subcontainer enumclass :container-slot root :type enumeration-class :if-exists :error :iterator do-enumpool-classes)
(define-subcontainer enumclass-ref :container-slot root :type t :if-exists :error :iterator do-enumclass-objects)

(defun coerce-to-enumclass (pool enumclass-or-name &key (if-does-not-exist :error))
  (etypecase enumclass-or-name
    (enumeration-class enumclass-or-name)
    (symbol (enumclass pool enumclass-or-name :if-does-not-exist if-does-not-exist))))

(defgeneric enumclass-add (enumeration-class enumerated)
  (:method ((o enumeration-class) (e enumerated))
    (let ((id (hash-table-count (enumclass-root o))))
      (setf (slot-value e 'pool) (enumclass-pool o)
            (slot-value e 'class) o
            (slot-value e 'id) id
            (enumclass-ref o id) e))))

(defgeneric enumclass-remove (enumeration-class enumerated)
  (:method ((o enumeration-class) (id integer))
    (enumclass-remove o (enumclass-ref o id)))
  (:method ((o enumeration-class) (e enumerated))
    (setf (slot-value e 'pool) nil
          (slot-value e 'class) nil
          (slot-value e 'id) nil
          (enumclass-ref o (enumerated-id e)) nil)))

(defun enumclass-count (enumeration-class)
  (hash-table-count (enumclass-root enumeration-class)))

(defun enumpool-add (pool class enumerated &key (if-class-does-not-exist :create))
  (let ((enumclass (or (coerce-to-enumclass pool class :if-does-not-exist :continue)
                       (ecase if-class-does-not-exist
                         (:error (error 'enumeration-pool-class-missing-error :pool pool :class class))
                         (:create (lret ((enumclass (make-enumeration-class :name class :pool pool)))
                                    (let* ((precedence-list (#+ccl ccl:class-precedence-list
                                                             #+sbcl sb-mop:class-precedence-list
                                                             #+ecl clos:class-precedence-list
                                                             (class-of enumerated)))
                                           (precedence-sublist (ldiff precedence-list
                                                                      (member (or (find-class class)
                                                                                  (error "~@<Class ~A does not exist.~:@>" class))
                                                                              precedence-list)))
                                           (enumclass-list (list* class (mapcar #'class-name precedence-sublist))))
                                      (dolist (c enumclass-list)
                                        (setf (enumclass pool c) enumclass)))))))))
    (enumclass-add enumclass enumerated)))

(defun enumpool-remove (pool class enumerated-or-id &key (if-class-does-not-exist :continue)
                      &aux (enumclass (coerce-to-enumclass pool class :if-does-not-exist :continue)))
  (if enumclass
      (enumclass-remove enumclass enumerated-or-id)
      (ecase if-class-does-not-exist
        (:error (error 'enumeration-pool-class-missing-error :pool pool :class class))
        (:continue nil))))

(defun enumpool-ref (pool class id &key (if-class-does-not-exist :error) (if-does-not-exist :error)
                     &aux (enumclass (coerce-to-enumclass pool class :if-does-not-exist :continue)))
  (if enumclass
      (or (enumclass-ref enumclass id :if-does-not-exist :continue)
          (ecase if-does-not-exist
            (:error (error 'enumeration-pool-id-missing-error :pool pool :class class :id id))
            (:continue nil)))
      (ecase if-class-does-not-exist
        (:error (error 'enumeration-pool-class-missing-error :pool pool :class class))
        (:continue nil))))

(defun map-enumpool-type (pool type fn)
  (when-let ((enumclass (coerce-to-enumclass pool type :if-does-not-exist :continue)))
    (do-enumclass-objects (value enumclass)
      (when (typep value type)
        (collect (funcall fn value))))))

(defmacro do-device-class-registers ((layout reader-name writer-name &optional register selector inlayout-register-nr) device-class
                                     &body body &aux (layout-var (or layout (gensym))))
  "Execute BODY with LAYOUT, REGISTER, SELECTOR, READER-NAME and WRITER-NAME
bound to corresponding values for every register defined in DEVICE-CLASS.

Any variable name can be specified as NIL, which is intepreted as a request
to ignore that binding."
  (with-gensyms (top)
    (once-only (device-class)
      `(iter ,top
             (for ,layout in (device-class-layouts ,device-class))
             ,@(when (or reader-name writer-name)
                     `((for (nil ,reader-name ,writer-name) in (device-class-effective-layout-specs ,device-class))))
             (iter ,@(when register `((for ,register in (layout-registers ,layout-var))))
                   ,@(when selector `((for ,selector in (layout-register-selectors ,layout-var))))
                   ,@(when inlayout-register-nr `((for ,inlayout-register-nr from 0)))
                   ,@(butlast body)
                   (in ,top (collect ,(lastcar body))))))))

(defun make-invalid-register-access-read-trap (id format-control)
  (lambda (device selector)
    (declare (ignore selector))
    (error 'invalid-register-read :device device :id id :format-control format-control)))

(defun make-invalid-register-access-write-trap (id format-control)
  (lambda (value device selector)
    (declare (ignore selector))
    (error 'invalid-register-write :value value :device device :id id :format-control format-control)))

(defun device-class-protocol-p (device-class)
  (declare (device-class device-class))
  (member (name device-class) '(device extended-register-device)))
(defun device-class-register-selector (device-class i)
  (aref (device-class-selectors device-class) i))
(defun device-class-reader (device-class i)
  (aref (device-class-readers device-class) i))
(defun device-class-writer (device-class i)
  (aref (device-class-writers device-class) i))
(defun set-device-class-reader (device-class i fn)
  (setf (aref (device-class-readers device-class) i)
        (or fn (make-invalid-register-access-read-trap i "~@<Disabled register read access for device ~S, register id 0x~X, register ~S.~:@>"))))
(defun set-device-class-writer (device-class i fn)
  (setf (aref (device-class-writers device-class) i)
        (or fn (make-invalid-register-access-write-trap i "~@<Disabled register write access of ~8,'0X for device ~S, register id 0x~X, register ~S.~:@>"))))

(defsetf device-class-reader set-device-class-reader)
(defsetf device-class-writer set-device-class-writer)

(defun register-id-valid-for-device-class-p (device-class id)
  (not (minusp (device-class-register-selector device-class id))))

(defun register-name-valid-for-device-class-p (device-class register-name)
  (let* ((space (device-class-space device-class)))
    (register-id-valid-for-device-class-p device-class (symbol-id (space-register-dictionary space) register-name))))

(defclass device (enumerated)
  ((metaclass :reader device-metaclass :allocation :class :initarg :metaclass)
   (selectors :accessor device-selectors :type (vector fixnum) :allocation :class) ; copied over from class
   (readers :accessor device-readers :type (vector function) :allocation :class) ; ...
   (writers :accessor device-writers :type (vector function) :allocation :class) ; ...
   (enumeration-class :type symbol :initarg :enumeration-class)
   (backend :accessor backend :type (or null device) :initarg :backend)))

(eval-when (:compile-toplevel :load-toplevel)
  (setf (device-class 'device) (make-device-class :name 'device)
        (device-class 'extended-register-device) (make-device-class :name 'extended-register-device)))

(defmacro define-device-class (name space provided-superclasses slots &body options)
  ;; XXX: shouldn't we check for the cases when user specifies E-R-D-C and DEVICE? Would V-S catch that?
  (let* ((extendedp (or (some (lambda (x) (subtypep x 'extended-register-device))
                              provided-superclasses)
                        (find :extended-layouts options :key #'car)))
         (type (if extendedp 'extended-register-device 'device))
         (relevant-parents (iter (for name in provided-superclasses)
                                 (when-let ((parent (device-class name :if-does-not-exist :continue)))
                                   (collect parent)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (unless (device-class ',name :if-does-not-exist :continue)
           (setf (device-class ',name)
                 (,(if extendedp
                       'make-extended-register-device-class
                       'make-device-class)
                   :name ',name :space ,(if space `(space ',space) '*dummy-space*)
                   :parents (mapcar #'device-class ',(or (mapcar #'name relevant-parents)
                                                         (list type)))
                   ,@(when-let ((extended-layouts (cdr (assoc :extended-layouts options))))
                               `(:extended-layouts ',extended-layouts))
                   ,@(when-let* ((initargs (cdr (assoc :default-initargs options)))
                                 (enumclass (getf initargs :enumeration-class)))
                                `(:enumeration-class ,enumclass))))))
       (defclass ,name ,(append provided-superclasses
                                (unless relevant-parents
                                  (list type)))
         (,@slots
          (metaclass :allocation :class :initform (device-class ',name))
          (selectors :allocation :class)
          (readers :allocation :class)
          (writers :allocation :class)
          ,@(when extendedp
             `((extensions :allocation :class))))
         ,@(iter (for (opt . body) in options)
                 (unless (member opt '(:enumeration-class :layouts :extended-layouts))
                   (collect (cons opt body)))))
       (initialize-device-class (device-class ',name) ,(when space `(space ',space)) ',(rest (assoc :layouts options))))))

;; McCLIM-like protocol class stuff :-)
(defmacro define-protocol-device-class (name space provided-superclasses slots &body options)
  `(progn
     (define-device-class ,name ,space (,@provided-superclasses)
       (,@slots)
       ,@options)
     (let ((the-class (find-class ',name)))
       (defmethod initialize-instance :after ((o ,name) &key &allow-other-keys)
         (when (eq (class-of o) the-class)
           (error 'protocol-class-instantiation :class (class-of o)))))))

#+nil
(defmacro define-struct-device-class (name space slots &rest options)
  `(progn
     (defstruct (,name (:include struct-device))
       ,@slots)
     (initialize-device-class (make-struct-device-class :name ,name ,@(when-let ((documentation (second (assoc :documentation options))))
                                                                                `(:documentation ,documentation))
                                                        :constructor (function ,(format-symbol (symbol-package name) "MAKE-~A" name)))
                              (space ,space) ',(rest (assoc :layouts options)))))

(defun class-current-slot-allocation (class slot)
  (if (slot-value class slot) (length (slot-value class slot)) 0))

(defgeneric class-pool-boundp (class slot)
  (:documentation "Determine, whether CLASS's SLOT is bound.
                   SLOT must represent a map -- that is, either a selector,
                   a reader or a writer map.")
  (:method ((o struct-device-class) slot-name)
    (not (member (slot-value o slot-name) (list *dummy-fixnum-vector* *dummy-function-vector*))))
  (:method ((o device-class) slot-name)
    (slot-value o slot-name)))

(defun ensure-device-class-map-storage (device-class space)
  "Ensure that map storage of DEVICE-CLASS is enough to cover all registers 
   in SPACE."
  (declare (type (or struct-device-class device-class) device-class))
  (let ((required-length (length (dictionary-id-map (space-register-dictionary space)))))
    (flet ((make-or-extend-pool (type initial-element old-pool)
             (concatenate `(vector ,type)
                          (or old-pool (make-array required-length :element-type type :initial-element initial-element))
                          (make-list (- required-length (length old-pool)) :initial-element initial-element)))
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
                  (cond ((zerop required-length) (make-array 0 :element-type type))
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

(defun mapc-layout-register-ids (fn layout &aux (dictionary (space-register-dictionary (spaced-space layout))))
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
  #+nil
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
    (declare (type (or null space) space) (type list direct-layout-specs) (optimize debug))
    (if space
        (let ((direct-layout-instances (mapcar (compose (curry #'layout space) #'first) direct-layout-specs))
              (eligible-parents (device-class-parents o)))
          (when-let ((misspaced (remove-if (lambda (x) (or (eq x space) (eq x *dummy-space*)))
                                           (mapcar #'device-class-space eligible-parents))))
            (error 'cross-space-inheritance
                   :class (name o) :required-space (space-name space) :actual-spaces (mapcar #'space-name misspaced)))
          (multiple-value-bind (inherited-layout-instances inherited-layout-specs) (compute-inherited-layouts direct-layout-instances eligible-parents)
            ;; compute effective layouts specs, effective layouts and register device class
            (setf (device-class-effective-layout-specs o) (append inherited-layout-specs direct-layout-specs)
                  (device-class-layouts o) (append inherited-layout-instances direct-layout-instances)
                  (device-class (name o)) o)
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
            (error 'spaceless-layout-reference :class (name o))
            ;; Messing with initargs would be way too painful...
            (with-slots (space layouts direct-layout-specs effective-layout-specs selectors readers writers) o
              (setf (values space layouts direct-layout-specs effective-layout-specs selectors readers writers)
                    (values *dummy-space* nil nil nil (make-array 0 :element-type 'fixnum)
                            (make-array 0 :element-type 'function :initial-element #'identity) (make-array 0 :element-type 'function :initial-element #'identity)))))))
  (:method :after ((o extended-register-device-class) space direct-layout-specs)
    (declare (ignore space direct-layout-specs))
    (when (not (device-class-protocol-p o))
      (when-let ((missing (remove-if (rcurry #'assoc (device-class-effective-layout-specs o)) (device-class-extended-layouts o))))
        (error "~@<During initialization of extended register device class ~S: unknown layouts were specified to be extended: ~S~:@>"
               (name o) missing))
      ;; XXX: no inheritance for these maps... complicated composition. Not unsurmountable, though.
      (setf (device-class-extensions o) (build-device-class-extension-map (device-class-space o) (device-class-extended-layouts o))))))

(defun reinitialize-device-class (device-class)
  "Reinitialize DEVICE-CLASS according to its SPACE and DIRECT-LAYOUT-SPECS slots."
  (initialize-device-class device-class (device-class-space device-class) (device-class-direct-layout-specs device-class)))

(defun device-class-corresponds-to-space-and-layouts-p (device-class space-name layout-specs)
  (when-let ((present-space (device-class-space device-class)))
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

;;;;
;;;; Devices
;;;;
;;;  Class precedence list: D > C > B > A
;;;    | a      | b      | c       < Preexisting device of type C enumerated as:
;;;  --+--------+--------+--------
;;;  a | #1       #3       #4
;;;  b | #5       #1       #4
;;;  c | #6       #6       #2
;;;  d | #7       #7       #8
;;;  ^
;;; Query for type:
;;;
;;; Natural constraint: x >= e                                              Methods
;;; 1. device of class X enumd as Z<X, queried as Z           x....qe  e = q
;;; 2. device of class X enumd as X, queried as X               xqe    ...
;;; 3. device of class X enumd as Y<X, queried as Z; Z<Y<X    x..e..q  Q<E, many potential enumeration classes, NIL?
;;; 4. device of class X enumd as X, queried as Y<X           xe....q  ...
;;; 5. device of class X enumd as Z<X, queried as Y; Z<Y<X    x..q..e  e = ec(q), because ENUMPOOL-ADD takes care of that
;;; 6. device of class X enumd as Z<X, queried as X           xq....e  ...
;;; 7. device of class X enumd as Y<X, queried as W; Y<X<W    q..x..e  no enumclass -> NIL
;;; 8. device of class X enumd as X, queried as W; X<W        q....xe  ...

(defmethod initialize-instance :after ((device device) &key &allow-other-keys)
  (let* ((device-class (device-metaclass device))
         (space (device-class-space device-class)))
    (unless space
      (error 'device-type-not-directly-instantiable :type (type-of device)))
    (setf (device-selectors device) (device-class-selectors device-class)
          (device-readers device) (device-class-readers device-class)
          (device-writers device) (device-class-writers device-class))))

(defgeneric class-of-device (device)
  (:documentation "Return the DEVICE's metaclass, or device class structure,
                   depending on its type.")
  (:method ((o device)) (device-metaclass o)))

(defun device-enumeration-class (device)
  (if (typep device 'struct-device)
      (struct-device-class-enumeration-class (struct-device-class device))
      (slot-value* device 'enumeration-class (type-of device))))

(defun enumerate-device (pool device)
  (enumpool-add pool (device-enumeration-class device) device))

(defun device-space (device)
  (declare (type device device))
  (device-class-space (class-of-device device)))

(defgeneric device-reader (device id)
  (:method ((device device) register-id) (aref (device-readers device) register-id)))
(defgeneric device-writer (device id)
  (:method ((device device) register-id) (aref (device-writers device) register-id)))
(defgeneric set-device-reader (device id value)
  (:method ((device device) register-id value) (setf (aref (device-readers device) register-id) value)))
(defgeneric set-device-writer (device id value)
  (:method ((device device) register-id value) (setf (aref (device-writers device) register-id) value)))
(defsetf device-reader set-device-reader)
(defsetf device-writer set-device-writer)

(declaim (ftype (function (device fixnum) fixnum) device-register-selector))
(defun device-register-selector (device id)
  (declare (device device) (fixnum id))
  (aref (device-selectors device) id))

(defun device-register-layout (device name &key (if-does-not-exist :error))
  "Return the layout of a DEVICE's register, who goes by NAME."
  (or (iter (for layout in (device-class-layouts (class-of-device device)))
            (finding layout such-that (find name (layout-registers layout) :key #'name)))
      ;; slow case..
      (iter (for layout in (device-class-layouts (class-of-device device)))
            (finding layout such-that (find name (layout-registers layout) :key #'reg-aliases :test #'member)))
      (case if-does-not-exist
        (:continue nil)
        (:error (error 'invalid-device-register :device device :register name)))))

(defun print-device-object (device stream)
  (format stream "~@<#<~;~A-~A~;>~:@>" (type-of device) (slot-value* device 'id)))

(defmethod print-object ((device device) stream)
  (print-device-object device stream))

(defclass slave-device (device)
  ((master :accessor slave-device-master :initarg :master)))

(defclass master-device (device)
  ((slaves :accessor master-device-slaves :initarg :slaves)))

(defclass extended-register-device (device)
  ((extensions :accessor device-extensions :type (vector simple-array) :allocation :class)))

(defmethod initialize-instance :after ((device extended-register-device) &key &allow-other-keys)
  (setf (device-extensions device) (device-class-extensions (class-of-device device))))

(defstruct struct-device
  (class nil :type struct-device-class)
  (enumpool nil :type (or null enumeration-pool))
  (enumclass nil :type (or null enumeration-class))
  (id nil :type (or null fixnum))
  (backend nil :type (or null struct-device)))

(defun make-struct-device-instance (type &rest initargs)
  (apply (struct-device-class-constructor (device-class type)) initargs))

(defmethod class-of-device ((o struct-device))
  (struct-device-class o))

;;; Make the thing obey the enumerated protocol
(defmethod enumerated-pool ((o struct-device)) (struct-device-enumpool o))
(defmethod enumerated-class ((o struct-device)) (struct-device-enumclass o))
(defmethod enumerated-id ((o struct-device)) (struct-device-id o))
(defmethod enumclass-add ((o enumeration-class) (e struct-device))
  (let ((id (hash-table-count (enumclass-root o))))
    (setf (struct-device-enumpool e) (enumclass-pool o)
          (struct-device-enumclass e) o
          (struct-device-id e) id
          (enumclass-ref o id) e)))
(defmethod enumclass-remove ((o enumeration-class) (e enumerated))
  (setf (struct-device-enumpool e) nil
        (struct-device-enumclass e) nil
        (struct-device-id e) nil
        (enumclass-ref o (enumerated-id e)) nil))

(defun enumerated> (a b)
  (or (string> (type-of a) (type-of b))
      (and (eq (type-of a) (type-of b))
           (> (enumerated-id a) (enumerated-id b)))))

;;;;
;;;; Device access via abstract registers
;;;;
(defun device-register (device register-id)
  (declare (type device device) (type fixnum register-id))
  (funcall (aref (device-readers device) register-id) device (aref (device-selectors device) register-id)))

(defun set-device-register (device register-id value)
  (declare (type device device) (type (unsigned-byte 32) value))
  (funcall (aref (device-writers device) register-id) value device (aref (device-selectors device) register-id)))

(defsetf device-register set-device-register)
    
(defmacro devreg (device regname)
  (once-only (device)
   `(device-register ,device (register-id (device-space ,device) ,regname))))

(define-setc-expander devreg (value device regname)
  (once-only (device)
   `(set-device-register ,device (register-id (device-space ,device) ,regname) ,(eeval value))))

(defmacro devbit-decode (device regname bytename)
  (decode-context (space-name space bitfield) regname `(,bytename)
    (once-only (device)
      `(decode-using-bitfield (load-time-value (bitfield (space ',space-name) ,bytename))
                              (device-register ,device (register-id (device-space ,device) ,regname))))))

(defmacro devreg-decode (device regname)
  (decode-context (space-name space bitfield fmtname) regname ()
    (once-only (device)
     `(decode-using-format (load-time-value (register-format ,fmtname))
                           (device-register ,device (register-id (device-space ,device) ,regname))))))

(defmacro devbit (device regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    (once-only (device)
     `(ldb-test ',(bitfield-byte space bytename)
                (device-register ,device (register-id (device-space ,device) ,regname))))))
  
(defmacro devbit-value (device regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    (once-only (device)
     `(ldb ',(bitfield-byte space bytename)
           (device-register ,device (register-id (device-space ,device) ,regname))))))

(define-setc-expander devbit (value device regname bytename &key write-only)
  (decode-context (space-name space bitfield) regname `(,bytename)
    (let ((mask (byte-bitmask (bitfield-spec bitfield))))
      (once-only (device)
       `(setf (device-register ,device (register-id (device-space ,device) ,regname))
              ,(eeval
                `(logior ,value ,(unless write-only
                                         `(device-register ,device (register-id (device-space ,device) ,regname))))
                `(,mask ,(lognot mask))
                `(,(mkenv space-name (name bitfield)) nil)))))))

(defmacro devbits (device regname (&rest bytenames))
  (decode-context (space-name space bitfield) regname bytenames
    (once-only (device)
     `(values-list
       (mapcar
        (rcurry #'ldb-test (device-register ,device (register-id (device-space ,device) ,regname)))
        ',(mapcar [bitfield-byte space] bytenames))))))

(define-setc-expander devbits (values device regname (&rest bytenames) &key write-only)
  (decode-context (space-name space bitfield) regname bytenames
    (with-gensyms (reg-id-var) 
      (let* ((initial (unless write-only
                        `(device-register ,device ,reg-id-var)))
             (bytes (mapcar [bitfield-byte space] bytenames)))
        (once-only (device)
         `(let ((,reg-id-var (register-id (device-space ,device) ,regname)))
            (setf (device-register ,device ,reg-id-var)
                  ,(eeval (list* 'logior initial (ensure-destructurisation bytenames values))
                          (list* (lognot (bytes-bitmask bytes)) (mapcar #'byte-bitmask bytes))
                          (list* nil (mapcar [mkenv space-name] bytenames))))))))))

(defmacro test-devbits (device regname bytenames &rest bytevals)
  "Check if every bitfield among those specified by BYTENAMES is set to a value denoted by
   a corresponding member of BYTEVALS."
  (decode-context (space-name space bitfield) nil bytenames
    (let ((bytenames (ensure-list bytenames)))
      (once-only (device)
       `(= (logand (device-register ,device (register-id (device-space ,device) ,regname))
                   ,(bytes-bitmask (mapcar [bitfield-byte space] bytenames)))
           (bits ,bytenames ,@bytevals))))))

;;;;
;;;; Register instances
;;;;
(defstruct (register-instance (:include bitmop::docunamed) (:conc-name reginstance-))
  "Instance of register."
  aliases
  device
  layout
  register
  selector
  reader writer
  id)

(defstruct (reginstance-enumeration-pool (:include enumeration-pool) (:conc-name ri-enumpool-))
  (reginstances (make-hash-table :test 'eq) :type hash-table)
  (reginstances-by-id (make-hash-table :test 'eq) :type hash-table))

(define-subcontainer register-instance :container-slot reginstances :type register-instance :if-exists :continue
                     :iterator do-register-instances :remover remove-register-instance)
(define-subcontainer register-instance-by-id :container-slot reginstances-by-id :type register-instance :if-exists :continue :type-allow-nil-p t)

(defun reginstance-value (register-instance)
  (declare (type register-instance register-instance))
  (let ((device (reginstance-device register-instance)))
    (funcall (reginstance-reader register-instance) device (reginstance-selector register-instance))))

(defun set-reginstance-value (register-instance value)
  (declare (type register-instance register-instance))
  (let ((device (reginstance-device register-instance)))
    (funcall (reginstance-writer register-instance) value device (reginstance-selector register-instance))))

(defsetf reginstance-value set-reginstance-value)

(defun device-register-instance (device name &key (if-does-not-exist :error))
  "Return the instance of a DEVICE's register, who goes by NAME."
  (when-let ((layout (device-register-layout device name :if-does-not-exist if-does-not-exist)))
    (register-instance (enumerated-pool device) (device-register-instance-name device layout name))))

(defun device-layout-register-instance-by-selector (device layout selector)
  "Find the register instance of DEVICE, whose abstract register
has SELECTOR within LAYOUT."
  (find-if (lambda (ri) (and (eq  device   (reginstance-device ri))
                             (eq  layout   (reginstance-layout ri))
                             (eql selector (reginstance-selector ri))))
           (hash-table-values (ri-enumpool-reginstances (enumerated-pool device)))))

(defun device-layout-register-instances (device layout)
  "Return all register instances of DEVICE, whose abstract registers
belong to LAYOUT."
  (remove-duplicates (remove-if-not (lambda (ri) (and (eq device (reginstance-device ri))
                                                      (eq layout (reginstance-layout ri))))
                                    (hash-table-values (ri-enumpool-reginstances (enumerated-pool device))))
                     :test #'eq))

(defun device-register-instances (device)
  "Return all register instances of DEVICE."
  (remove-duplicates (remove device (hash-table-values (ri-enumpool-reginstances (enumerated-pool device)))
                             :test-not #'eq :key #'reginstance-device)
                     :test #'eq))

(defun default-register-instance-name (device layout name)
  "Complete a register instance name given NAME and LAYOUT of DEVICE."
  (let* ((dot-posn (position #\. name))
         (qualify (or (layout-force-prefix layout)
                      (layout-force-multi layout)
                      (> (enumclass-count (enumerated-class device)) 1))))
    (concatenate 'string
                 (let ((enumclass-name (string (enumclass-name (enumerated-class device)))))
                   (cond ((layout-force-prefix layout) enumclass-name)
                         (dot-posn (subseq name 0 dot-posn))
                         (qualify enumclass-name)))
                 (when qualify (write-to-string (enumerated-id device) :base 10))
                 (when (or dot-posn qualify) ".")
                 (if (and (not (layout-force-prefix layout)) dot-posn)
                     (subseq name (1+ dot-posn))
                     name))))

(defun simple-register-instance-name (device layout name)
  (declare (ignore layout))
  (concatenate 'string
               (string (enumclass-name (enumerated-class device)))
               (write-to-string (enumerated-id device) :base 10)
               "."
               name))

(defun layout-register-instance-name (device layout name)
  (concatenate 'string
               (string (name layout))
               (write-to-string (enumerated-id device) :base 10)
               "."
               name))

(defun slave-register-instance-name (device layout name &aux
                                     (master (slave-device-master device)))
  (declare (ignore layout))
  (concatenate 'string
               (string (enumclass-name (enumerated-class master)))
               (write-to-string (enumerated-id master) :base 10)
               "."
               name))

(defun device-register-instance-name (device layout name)
  "Complete a register instance name given NAME and LAYOUT of DEVICE."
  (make-keyword (funcall (or (layout-name-fn layout) #'default-register-instance-name)
                         device layout (string name))))

(defun purge-device-register-instances (device &aux (pool (enumerated-pool device)))
  "Purge all register instances associated with DEVICE."
  (do-device-class-registers (layout nil nil register nil) (class-of-device device)
    (let* ((ri-name (device-register-instance-name device layout (name register)))
           (ri (register-instance pool ri-name)))
      ;;
      ;; BUG: note how the line below causes real damage in real life...
      ;; A smarter reginstance ID allocation scheme is required.
      ;;
      (setf (register-instance-by-id pool (reginstance-id ri)) nil)
      (mapc (curry #'remove-register-instance pool) (cons ri-name (mapcar (curry #'device-register-instance-name device layout)
                                                                          (reg-aliases register)))))))

(defgeneric create-device-register-instance (device layout register register-inlayout-nr register-inspace-id selector name aliases id)
  (:method ((o device) layout register register-inlayout-nr register-inspace-id selector name aliases id &aux
            (device-class (class-of-device o)))
    (declare (ignore register-inlayout-nr))
    (make-register-instance :device o :layout layout :register register :selector selector
                            :name name :aliases aliases :id id 
                            :reader (device-class-reader device-class register-inspace-id)
                            :writer (device-class-writer device-class register-inspace-id))))

(defun create-device-register-instances (device &aux (device-class (class-of-device device)) (pool (enumerated-pool device)))
  "Walk the DEVICE's layouts and spawn the broodlings."
  (with-retry-restarts ((retry ()
                          :test (lambda (c) (typep c 'bad-redefinition))
                          :report "Purge device register instances and retry their creation."
                          (purge-device-register-instances device)))
    (do-device-class-registers (layout nil nil register selector inlayout-nr) device-class
      (lret* ((ri-aliases (mapcar (curry #'device-register-instance-name device layout)
                                  (reg-aliases register)))
              (main-ri-name (device-register-instance-name device layout (name register)))
              ;;
              ;; BUG: note how the above BUG relates to this...
              ;;
              (id (1+ (hash-table-count (ri-enumpool-reginstances-by-id pool))))
              (reg-inspace-id (register-id (device-space device) (name register)))
              (instance (create-device-register-instance device layout register inlayout-nr reg-inspace-id selector main-ri-name ri-aliases id)))
        (setf (register-instance-by-id pool id) instance)
        (iter (for ri-name in (cons main-ri-name ri-aliases))
              (assert ri-name)
              (setf (register-instance pool ri-name) instance))))))

(defun define-anonymous-register-instance (pool name reader writer)
  (let* ((free-id (1+ (hash-table-count (ri-enumpool-reginstances-by-id pool))))
         (ri (make-register-instance :name name :id free-id
                                     :device nil :selector nil
                                     :reader reader :writer writer)))
    (setf (register-instance-by-id pool free-id) ri
          (register-instance pool name) ri)))

;;;;
;;;; Das init
;;;;
(defun init-device-model ()
  "Forget all known device and register instances.")
