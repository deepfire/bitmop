;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BITMOP; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2010 by
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

(in-package :bitmop)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-reader)
  (enable-compose-reader))

(defclass space ()
  ((name :accessor space-name :type (or keyword list) :initarg :name)
   (documentation :accessor space-documentation :type (or null string) :initarg :documentation)
   (referrers :accessor space-referrers :initform nil :type list)
   (register-dictionary :accessor space-register-dictionary :initform (make-aliased-dictionary) :type dictionary)
   (layouts :accessor layouts :initform (make-hash-table) :type hash-table)
   (bitfields :accessor bitfields :initform (make-hash-table) :type hash-table)
   (bitfield-bytes :accessor bitfield-bytes :initform (make-hash-table) :type hash-table)))

(defmethod print-object ((space space) stream)
  (format stream "~@<#<SPACE:~;~A ~S registers: ~S layouts: ~S~;>~:@>"
          (space-name space)
          (space-documentation space)
          (map 'list #'name (dictionary::dictionary-ids-to-values (space-register-dictionary space)))
          (maphash-values #'name (layouts space))))

(defstruct (docunamed (:conc-name nil))
  (name nil :type symbol)
  (documentation "" :type string))

(defstruct (spaced (:include docunamed))
  (space nil :type (or null space)))

(defstruct (register-format (:include spaced) (:conc-name format-))
  bitfields)

(defstruct (bitfield (:include spaced))
  (formats% nil :type list)
  spec
  (bytevals (make-hash-table) :type hash-table)
  (byterevvals (make-hash-table) :type hash-table))

(defmethod print-object ((o bitfield) stream &aux (byte (bitfield-spec o)))
  (print-unreadable-object (o stream)
    (format stream "~@<BITFIELD ~;~A (byte ~D ~D)~:[~;~:* values:~{ ~D:~A~}~]~:@>"
            (name o) (byte-size byte) (byte-position byte)
            (iter (for (name byteval) in-hashtable (bitfield-bytevals o))
                  (collect (byteval-value byteval)) (collect name)))))

(defstruct (layout (:include spaced))
  "Maps register names into register structures."
  force-prefix
  force-multi
  name-fn
  registers
  register-selectors)

(defmethod print-object ((o layout) stream)
  (print-unreadable-object (o stream)
    (format stream "~@<LAYOUT~; ~A registers:~{ ~A~}~:@>"
            (name o) (mapcar #'name (layout-registers o)))))

(defstruct (register (:include spaced) (:conc-name reg-))
  "Defines a formatted register, specified within a layout with a selector."
  aliases
  format
  ext)
  
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
(defvar *register-formats* (make-hash-table :test 'eq))
(defvar *register-spaces* (make-hash-table :test 'eq))

(define-root-container *spaces* space :if-exists :continue)
(define-root-container *register-formats* register-format :if-exists :continue)
(define-root-container *register-spaces* register-space :type space :if-exists :error :description "register")
(define-subcontainer layout :container-slot layouts :if-exists :continue)
(define-subcontainer bitfield :container-slot bitfields :if-exists :continue)
(define-subcontainer bitfield-byte :container-slot bitfield-bytes :if-exists :continue :type cons)
(define-subcontainer byteval :container-slot bytevals :if-exists :continue)
(define-subcontainer byterevval :container-slot byterevvals :if-exists :continue :type byteval)

(define-condition bit-notation-error (error) ())

(define-simple-error bit-notation-error)

(define-condition definition-error (bit-notation-error) ())

(define-condition space-definition-error (definition-error)
  ((space :initarg :space)))

(define-simple-error space-definition-error)

(define-reported-condition bitfields-divergent (space-definition-error)
  ((bitfields :initarg :bitfields))
  (:report (bitfields space) "~@<Unable to find a common format for bitfields ~{ ~A~} in space ~S~:@>" bitfields space))

(define-reported-condition bitfields-unknown (bit-notation-error)
  ((bitfields :initarg :bitfields))
  (:report (bitfields) "~@<Unable to find a space for bitfields: ~{ ~A~}~:@>" bitfields))

(define-reported-condition register-not-structured (bit-notation-error)
  ((name :initarg :name))
  (:report (name)
           "~@<Register ~S has no structure.~:@>" name))

(define-reported-condition invalid-register-selectors-in-layout-definition (space-definition-error)
  ((layout :initarg :layout)
   (bad-selectors :initarg :bad-selectors))
  (:report (layout space bad-selectors)
           "~@<In definition of layout ~S in space ~S: register selectors~{ ~A~} must be of type FIXNUM.~:@>" layout space bad-selectors))

(define-reported-condition incompatible-bitfield-redefinition (space-definition-error)
  ((bitfield :initarg :bitfield)
   (to :initarg :to))
  (:report (space bitfield to) "~@<In definition of space ~S: attempt to incompatibly redefine bitfield ~A to ~A.~:@>" space bitfield to))

(define-reported-condition underspecified-context (bit-notation-error)
  ()
  (:report () "~@<Impossible to deduce context: neither register name, nor byte names were specified.~:@>"))

(define-reported-condition namespace-unification-conflict (bit-notation-error)
  ((namespaces :initarg :namespaces)
   (slot :initarg :slot)
   (key :initarg :key))
  (:report (namespaces slot key) "Conflict during namespace unification. Namespaces ~S, slot ~S, key ~S." namespaces slot key))

(define-reported-condition no-space-context (bit-notation-error)
  ()
  (:report () "~@<Attempt to use names in a null space context.~:@>"))

(define-reported-condition conflicting-bitfield-names (bit-notation-error)
  ((conflicting-bitfield :initarg :conflicting-bitfield)
   (expected-format :initarg :expected-format))
  (:report (conflicting-bitfield expected-format)
           "~@<Bitfield ~S does not belong to register format ~S.~:@>" conflicting-bitfield expected-format))

;; This one stands out: going through dictionaries.
(declaim (ftype (function (space symbol) register)))
(defun register (space name)
  (declare (space space) (keyword name))
  (translation (space-register-dictionary space) name))

(declaim (ftype (function (space symbol) fixnum) register-id))
(defun register-id (space name)
  (declare (space space) (keyword name))
  (symbol-id (space-register-dictionary space) name))

(declaim (ftype (function (space fixnum) register) register-by-id))
(defun register-by-id (space id)
  (declare (space space) (fixnum id))
  (id-value (space-register-dictionary space) id))

(declaim (ftype (function (space symbol symbol) fixnum) register-selector))
(defun register-selector (space layout-name register-name)
  (declare (symbol register-name layout-name))
  (let ((layout (layout space layout-name :if-does-not-exist :continue)))
    (unless layout
      (bit-notation-error "~@<No layout ~A within space ~A referred by register ~A while querying for register's selector.~:@>"
                          layout-name (space-name space) register-name))
    (nth (position register-name (layout-registers layout) :key #'name) (layout-register-selectors layout))))

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

(defun bitfield-formats (space bitfield-name)
  "Yield the format of BITFIELD-NAMEd in SPACE"
  (bitfield-formats% (bitfield space bitfield-name)))

(defun define-byteval (bitfield byte value name &optional (documentation ""))
  (lret ((byteval (make-byteval :name name :byte byte :value value :documentation documentation)))
    (setf (byteval bitfield name) byteval
          (byterevval bitfield value) byteval)))

(defun ensure-bitfield (format name size pos &optional (doc "") byteval-specs)
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
(defun do-define-register-format (space name documentation bitspecs)
  (let ((format (make-register-format :name name :documentation documentation :space space)))
    (setf (format-bitfields format) (mapcar [apply [ensure-bitfield format]] bitspecs)
          (register-format name) format)))

(defmacro define-register-format (&environment env name doc &rest bitspecs)
  `(eval-when (:compile-toplevel :load-toplevel)
     (do-define-register-format (space ,(space-name (space (environment-space-name-context env)))) ',name ,doc ',bitspecs)))

(defun define-register (layout name &key (doc "Undocumented register.") aliases format ext)
  (lret* ((space (layout-space layout))
          (register (make-register :name name :space space :documentation doc
                                   :format (when format (register-format format)) :ext ext
                                   :aliases aliases)))
    ;; YYY: this is a kludge: a proper EVAL-WHEN somewhere is direly needed...
    (unless (register-space name :if-does-not-exist :continue)
      (setf (register-space name) space)
      (dolist (a aliases)
        (setf (register-space a) space)))
    (add-symbol (space-register-dictionary space) name register nil)
    (dolist (a aliases)
      (add-alias-unchecked (space-register-dictionary space) name a))))

(defun ensure-layout (space name documentation offset simple-sequence register-specs force-multi force-prefix name-fn)
  (let ((selectors (if simple-sequence
                       (iota (length register-specs) :start offset)
                       (mapcar (compose (curry #'+ offset) #'second) register-specs))))
    (when-let ((bad-selectors (remove-if (of-type 'fixnum) selectors)))
      (error 'invalid-register-selectors-in-layout-definition :space (space-name space) :layout name :bad-selectors bad-selectors))
    (lret ((layout (make-layout :name name :space space :documentation documentation
                                :register-selectors selectors :force-multi force-multi :force-prefix force-prefix :name-fn name-fn)))
      (setf (layout space name) layout
            (layout-registers layout) (iter (for spec in register-specs)
                                            (destructuring-bind (name &optional selector &rest rest) (ensure-list spec)
                                              (declare (ignore selector))
                                              (collect (apply #'define-register layout name rest))))))))

(defmacro define-layout (&environment env (name doc &key (offset 0) simple-sequence force-multi force-prefix name-fn) &rest defs)
  (declare (type integer offset) (type boolean force-multi force-prefix))
  (let ((desugared-name-fn (if (and (consp name-fn) (eq 'lambda (car name-fn)))
                               (rest name-fn)
                               name-fn)))
    `(ensure-layout (space ,(space-name (space (environment-space-name-context env))))
                    ',name ,doc ,offset ,simple-sequence ',defs ,force-multi ,force-prefix ,desugared-name-fn)))

;;;
;;;  o  layout templates
;;;  o  register instantiation
;;;;    - what information is deduced from register instances /now/?
;;;;      ... survey
;;;
(eval-when (:compile-toplevel :load-toplevel)
  (defun check-namespace-clauses (operation name f)
    (iter (for (clause . nil) in f)
          (unless (member clause '(:documentation :register-formats :layouts))
            (error 'simple-space-definition-error
                   :space name
                   :format-control "~@<in: ~A: unknown clause ~S~:@>"
                   :format-arguments (list operation clause))))))

(defmacro define-namespace (name &body f)
  (check-namespace-clauses 'define-namespace name f)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (space ,name) (make-instance 'space :name ,name :documentation ,(cadr (assoc :documentation f))))
     (symbol-macrolet ((*space* ,name))
       ,@(mapcar [cons 'define-register-format] (cdr (assoc :register-formats f)))
       ,@(mapcar [cons 'define-layout] (cdr (assoc :layouts f))))))

(defmacro extend-namespace (name &body f)
  (check-namespace-clauses 'extend-namespace name f)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (space ,name)
     (symbol-macrolet ((*space* ,name))
       ,@(mapcar [cons 'define-register-format] (cdr (assoc :register-formats f)))
       ,@(mapcar [cons 'define-layout] (cdr (assoc :layouts f))))))

(defun undefine-space (name)
  (remhash name *spaces*))
  
(defun unify-namespaces (names)
  (let* ((spaces (mapcar #'space names))
         (unispace (make-instance 'space :name names
                                         :documentation (format nil "Unified namespace of:~{ ~S~}."
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

(defmacro set-namespace (&rest nsnames)
  (let* ((need-unification (> (length nsnames) 1))
         (name (if need-unification nsnames (first nsnames))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when need-unification
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (define-symbol-macro *space* ,name))))

(defun decode-using-bitfield (bitfield value &key (symbolise-unknowns t))
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

(defun decode-using-format (format value &key (symbolise-unknowns t))
  (declare (type register-format format) (type (unsigned-byte 32) value))
  (iter (for bitfield in (format-bitfields format))
        (collect (cons (name bitfield) (decode-using-bitfield bitfield value :symbolise-unknowns symbolise-unknowns)))))

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
                           (or (reg-format (register ,space ,regname))
                               (error 'register-not-structured :name ,regname))
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

(defmacro decode (fmtname value &key (symbolise-unknowns t))
  (if (constant-p fmtname) ;; we won't do the same for other obvious cases
      `(decode-using-format (load-time-value (register-format ,fmtname)) ,value :symbolise-unknowns ,symbolise-unknowns)
      `(decode-using-format (register-format ,fmtname) ,value :symbolise-unknowns ,symbolise-unknowns)))

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

(defun interpret-field-value (field byte value)
  (case value
    ((t)   (dpb -1 byte 0))
    ((nil) 0)
    (t     (ash (if (integerp value)
                    value
                    (byteval-value (byteval field value)))
                (byte-position byte)))))

(defun fbits (bytenames &optional bytevals)
  "Combined bytevals of bitfields specified by BYTENAMES/BYTEVALS.
Bytevals default to T, when left completely unspecified."
  (decode-context (space-name space) nil bytenames
    (let* ((bytenames (ensure-list bytenames))
           (bitfields (mapcar (curry #'bitfield space) bytenames))
           (bytes (mapcar (curry #'bitfield-byte space) bytenames))
           (bytevals (or bytevals (make-list (length bytes) :initial-element t))))
      (labels ((cdrec (acc fields bytes vals)
                 (if fields
                     (cdrec (logior acc (interpret-field-value (car fields) (car bytes) (car vals))) 
                            (rest fields)
                            (rest bytes)
                            (rest vals))
                     acc)))
        (values 
         (cdrec 0 bitfields bytes bytevals)
         (bytes-bitmask bytes))))))

(defmacro bits (bytenames &rest bytevals)
  "Combined bytevals of bitfields specified by BYTENAMES/BYTEVALS.
  Bytevals default to T, when left completely unspecified."
  (decode-context (space-name space) nil bytenames
    (let* ((bytenames (ensure-list bytenames))
           (bytevals (or bytevals (make-list (length bytenames) :initial-element t))))
      (eeval (list* 'logior bytevals)
             (mapcar {#'byte-bitmask [bitfield-byte space]} bytenames)
             (mapcar [mkenv space-name] bytenames)))))