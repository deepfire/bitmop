(defpackage regvaldefs
  (:nicknames :rvd) 
  (:use :common-lisp :alexandria :pergamum :iterate :setc :early-eval :dictionary #+sbcl :sb-mop #+ecl :clos)
  (:shadow #:space #:format #:documentation)
  (:export
   ;; knobs
   #:*rvd-log-stream* #:*verbose-device-init-p* #:*rvd-synchronous-logging-p*
   ;; space
   #:space #:space-name #:environment-space-name-context #:space-name-context #:define-namespace #:undefine-namespace #:with-namespaces #:set-namespace #:unify-namespaces #:space-device-count #:init-device-model #:list-spaces #:undefine-space
   #:space-device #:space-remove-device
   #:name ;; this wants to go elsewhere.
   ;; conditions
   #:bit-notation-error #:simple-bit-notation-error #:protocol-class-instantiation #:underspecified-context #:bitfields-unknown #:bitfields-divergent-in-space #:namespace-unification-conflict
   #:invalid-register-selectors-in-layout-definition #:incompatible-bitfield-redefinition #:device-class-definition-error
   #:spaceless-layout-reference #:cross-space-inheritance #:device-type-not-directly-instantiable #:conflicting-bitfield-names
   #:invalid-register-access #:invalid-register-read #:invalid-register-write
   #:invalid-device-register
   ;; LAYOUTS
   #:layout #:define-layout #:layout-space #:layout-registers #:layout-force-multi #:layout-name-format #:layout-register-selectors
   ;; register stuff
   #:register #:reg-format #:reg-layout #:define-register #:register-decode
   #:register-instance #:register-instance-by-id #:reginstance-id #:reginstance-device #:reginstance-layout #:reginstance-register #:reginstance-value #:set-reginstance-value
   #:reginstance-reader #:reginstance-writer
   #:register-id-valid-for-device-class-p #:register-name-valid-for-device-class-p
   ;; register introspection, SLOW
   #:register-id #:register-by-id #:register-selector
   ;; bitfield, format, byteval
   #:bitfield #:bitfield-byte #:bitfield-format #:bitfield-decode
   #:test-devbits #:test-bits #:test-bits-set #:format-decode
   #:byteval #:byterevval
   ;; device metaclasses
   #:device-class #:device-class-p #:coerce-to-device-class #:define-protocol-device-class #:define-device-class #:define-device-subclass
   #:device-class-space #:device-class-instances #:device-class-layouts #:do-device-class-registers
   #:device-class-register-selector #:device-class-reader #:set-device-class-reader #:device-class-writer #:set-device-class-writer
   #:extended-register-device-class #:device-class-extensions
   #:struct-device-class #:define-struct-device-class
   ;; device classes
   #:device #:id #:device-id #:class-of-device #:device-selectors #:device-register-selector #:backend #:device-backend #:device-register #:instances
   #:print-device-object
   #:slave-device #:slave-device-master
   #:device-type ;; this one is special: goes through category
   #:extended-register-device #:device-extensions
   #:struct-device #:make-struct-device-instance
   #:device-reader #:device-writer #:set-device-reader #:set-device-writer
   #:device-register-layout #:device-register-instance-name #:device-register-instance
   ;; user API
   #:decode-context #:devreg #:place-bit #:place-bits #:decode #:devreg-decode #:devbit #:devbit-decode #:devbit-value #:devbits #:bits #:test-bits #:bit-value
   ;; sugar.lisp
   #:with-device-bits-toggled #:with-logged-device-io))

(in-package :regvaldefs)

;; (define-reloadable :regvaldefs asdf-reloadable (:pergamum :semi-precious)
;;   (:packages :setc :regvaldefs))