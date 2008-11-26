(defpackage regvaldefs
  (:nicknames :rvd) 
  (:use :common-lisp :alexandria :pergamum :iterate :setc :early-eval)
  (:shadow #:space #:format #:documentation)
  (:export
   #:space #:space-name #:environment-space-name-context #:space-name-context #:space-root #:define-namespace #:undefine-namespace #:with-namespaces #:set-namespace #:unify-namespaces #:purge-namespace-devices #:list-spaces
   #:name ;; this wants to go elsewhere.
   #:layout #:layout-space #:layout-registers #:define-layout
   #:bank #:bank-space #:define-bank #:with-banks #:register-bank #:bank-layout
   #:devtype #:devtype-banks #:devtype-instances
   #:register #:reg-format #:reg-selector #:reg-layout #:define-register #:register-decode
   #:register-instance #:reginstance-device #:reginstance-register #:reginstance-bank
   #:bitfield #:bitfield-byte #:bitfield-format #:bitfield-decode
   #:byteval
   #:device #:id #:device-id #:device-type #:space-device #:device-space #:backend #:device-backend #:device-register #:space-remove-device
   #:test-devbits #:test-bits #:format-decode #:reg-selector #:reg-ext
   #:devreg #:decode #:devreg-decode #:devbit #:devbit-decode #:devbit-value #:devbits #:bits #:test-bits #:bit-value
   #:undefine-space))
