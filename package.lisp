(defpackage regvaldefs
  (:nicknames :rvd) 
  (:use :common-lisp :alexandria :pergamum :iterate :setc :early-eval :dictionary #+sbcl sb-mop)
  (:shadow #:space #:format #:documentation)
  (:export
   ;; knobs
   #:*log-stream* #:*verbose-device-init-p*
   ;; space
   #:space #:space-name #:environment-space-name-context #:space-name-context #:define-namespace #:undefine-namespace #:with-namespaces #:set-namespace #:unify-namespaces #:space-device-count #:init-device-model #:list-spaces #:undefine-space
   #:space-device #:space-remove-device
   #:name ;; this wants to go elsewhere.
   ;; LAYOUTS
   #:layout #:define-layout #:layout-space #:layout-registers #:layout-register-selectors
   ;; register stuff
   #:register #:reg-format #:reg-layout #:define-register #:register-decode
   #:register-instance #:register-instance-by-id #:reginstance-id #:reginstance-device #:reginstance-register #:reginstance-bank #:reginstance-value #:set-reginstance-value
   ;; register introspection, SLOW
   #:register-id #:register-by-id #:register-selector
   ;; bitfield, format, byteval
   #:bitfield #:bitfield-byte #:bitfield-format #:bitfield-decode
   #:test-devbits #:test-bits #:format-decode
   #:byteval
   ;; device metaclasses
   #:device-class #:device-class-p #:define-device-class #:define-device-subclass #:device-class-space #:reinitialize-device-class
   #:device-class-register-selector #:device-class-reader #:set-device-class-reader #:device-class-writer #:set-device-class-writer
   #:extended-register-device-class #:device-class-extensions
   ;; device classes
   #:device #:id #:device-id #:device-selectors #:device-register-selector #:backend #:device-backend #:device-register #:instances
   #:device-type ;; this one is special: goes through category
   #:extended-register-device #:device-extensions
   ;; user API
   #:devreg #:decode #:devreg-decode #:devbit #:devbit-decode #:devbit-value #:devbits #:bits #:test-bits #:bit-value))
