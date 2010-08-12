(defpackage #:bitmop
  (:use :common-lisp :alexandria :pergamum :iterate :early-eval :dictionary :setc #+sbcl :sb-mop #+ecl :clos)
  (:shadow #:space #:documentation)
  (:export
   ;; structure types and accessors
   #:space
   #:space-name
   #:space-documentation
   #:space-register-dictionary
   #:docunamed
   #:name
   #:spaced
   #:spaced-space
   #:register-format
   #:format-bitfields
   #:bitfield
   #:bitfield-spec
   #:bitfield-bytevals
   #:layout
   #:layout-force-prefix
   #:layout-force-multi
   #:layout-name-fn
   #:layout-registers
   #:layout-register-selectors
   #:register
   #:reg-aliases
   #:reg-format
   #:reg-ext
   #:byteval
   #:byteval-byte
   #:byteval-value
   #:byte-bitmask
   #:bytes-bitmask
   #:flatten-byteval
   #:mkenv
   #:regformat
   #:bitfield-byte
   #:byterevval
   ;; trivial queries
   #:register-id
   #:register-by-id
   #:register-selector
   #:bytevals-equal-p
   #:bitfields-equal-p
   #:bitfield-formats
   ;; conditions
   #:bit-notation-error
   #:simple-bit-notation-error
   #:definition-error
   #:space-definition-error
   #:simple-space-definition-error
   #:register-not-structured
   #:bitfields-divergent
   #:bitfields-unknown
   #:invalid-register-selectors-in-layout-definition
   #:incompatible-bitfield-redefinition
   #:underspecified-context
   #:namespace-unification-conflict
   #:no-space-context
   #:conflicting-bitfield-names
   ;; definers
   #:define-byteval
   #:ensure-bitfield
   #:do-define-register-format
   #:define-register-format
   #:define-register
   #:ensure-layout
   #:define-layout
   #:define-namespace
   #:extend-namespace
   ;; space stuff
   #:undefine-space
   #:unify-namespaces
   #:environment-space-name-context
   #:space-name-context
   #:set-namespace
   ;; core API
   #:decode-using-bitfield
   #:decode-using-format
   #:decode
   #:decode-bitfield
   #:decode-context
   #:place-bit
   #:place-bit-value
   #:place-bits
   #:test-bits
   #:test-bits-set
   #:bit-value
   #:interpret-field-value
   #:fbits
   #:bits))

(defpackage #:device-model
  (:nicknames :devmodel)
  (:use :common-lisp :alexandria :iterate :pergamum :early-eval :dictionary :setc :bitmop #+sbcl :sb-mop #+ecl :clos)
  (:shadowing-import-from :bitmop #:space)
  (:export
   ;; logging
   #:*devmodel-log-stream*
   #:*devmodel-synchronous-logging-p*
   ;; conditions
   #:device-model-condition
   #:device-model-error
   #:simple-device-model-error
   #:enumeration-pool-condition
   #:enumeration-pool-class-missing-error
   #:enumeration-pool-id-missing-error
   #:device-class-definition-error
   #:spaceless-layout-reference
   #:cross-space-inheritance
   #:protocol-class-instantiation
   #:device-type-not-directly-instantiable
   #:invalid-register-access
   #:invalid-register-read
   #:invalid-register-write
   #:invalid-device-register
   ;; enumeration
   #:enumerated
   #:id
   #:enumerated-id
   #:enumerated-class
   #:enumerated-pool
   #:enumerated-class-name
   #:do-enumpool-classes
   #:do-enumclass-objects
   #:enumerated>
   #:enumeration-class
   #:enumclass
   #:enumclass-name
   #:enumclass-pool
   #:enumclass-ref
   #:coerce-to-enumclass
   #:enumclass-add
   #:enumclass-remove
   #:enumclass-count
   #:enumeration-pool
   #:enumpool-add
   #:enumpool-remove
   #:enumpool-ref
   #:map-enumpool-type
   ;; device metaclasses
   #:device-class
   #:device-class-space
   #:device-class-enumeration-class
   #:device-class-layouts
   #:struct-device-class
   #:device-class-umbrella
   #:extended-register-device-class
   #:device-class-extensions
   ;; device metaclass queries
   #:do-device-class-registers
   #:device-class-register-selector
   #:device-class-reader
   #:set-device-class-reader
   #:device-class-writer
   #:set-device-class-writer
   #:register-id-valid-for-device-class-p
   #:register-name-valid-for-device-class-p
   #:device-class-p
   #:device-class-protocol-p
   ;; device metaclass definitions
   #:define-device-class
   #:define-protocol-device-class
   #:define-struct-device-class
   ;; devices
   #:device
   #:backend
   #:class-of-device
   #:device-enumeration-class
   #:enumerate-device
   #:device-space
   #:device-reader
   #:device-writer
   #:set-device-reader
   #:set-device-writer
   #:device-register-selector
   #:device-register-layout
   #:print-device-object
   ;; specific devices
   #:slave-device
   #:slave-device-master
   #:master-device
   #:master-device-slaves
   #:extended-register-device
   #:device-extensions
   #:struct-device
   #:make-struct-device-instance
   ;; abstract registers
   #:device-register
   #:set-device-register
   #:devreg
   #:devbit-decode
   #:devreg-decode
   #:devbit
   #:devbit-value
   #:devbits
   #:test-devbits
   ;; register instances
   #:register-instance
   #:reginstance-aliases
   #:reginstance-device
   #:reginstance-layout
   #:reginstance-register
   #:reginstance-selector
   #:reginstance-reader
   #:reginstance-writer
   #:reginstance-id
   #:reginstance-enumeration-pool
   #:make-reginstance-enumeration-pool
   #:register-instance-by-id
   #:reginstance-value
   #:set-reginstance-value
   #:device-register-instance
   #:device-layout-register-instances
   #:device-register-instances
   #:default-register-instance-name
   #:simple-register-instance-name
   #:layout-register-instance-name
   #:slave-register-instance-name
   #:device-register-instance-name
   #:purge-device-register-instances
   #:create-device-register-instance
   #:create-device-register-instances
   #:define-anonymous-register-instance
   ;; final
   #:init-device-model
   ;; sugar.lisp
   #:with-device-bits-toggled
   #:with-logged-device-io
   #:execute-with-maybe-logged-device-io
   #:with-maybe-logged-device-io))