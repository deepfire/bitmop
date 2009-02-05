(defpackage regvaldefs
  (:nicknames :rvd) 
  (:use :common-lisp :alexandria :pergamum :iterate :setc :early-eval :dictionary #+sbcl sb-mop)
  (:shadow #:space #:format #:documentation)
  (:export
   #:space #:space-name #:environment-space-name-context #:space-name-context #:define-namespace #:undefine-namespace #:with-namespaces #:set-namespace #:unify-namespaces #:space-device-count #:init-device-model #:list-spaces
   #:name ;; this wants to go elsewhere.
   #:layout #:layout-space #:layout-registers #:define-layout
   #:device-class #:device-class-p #:define-device-class #:device-class-register-selector #:device-class-reader #:set-device-class-reader #:device-class-writer #:set-device-class-writer
   #:register #:register-id #:reg-format #:reg-selector #:reg-layout #:define-register #:register-decode
   #:register-instance #:register-instance-by-id #:reginstance-id #:reginstance-device #:reginstance-register #:reginstance-bank #:reginstance-value #:set-reginstance-value
   #:bitfield #:bitfield-byte #:bitfield-format #:bitfield-decode
   #:byteval
   #:device #:id #:device-id #:device-type #:space-device #:device-space #:backend #:device-backend #:device-register #:space-remove-device
   #:test-devbits #:test-bits #:format-decode #:reg-selector #:reg-ext
   #:devreg #:decode #:devreg-decode #:devbit #:devbit-decode #:devbit-value #:devbits #:bits #:test-bits #:bit-value
   #:undefine-space))
