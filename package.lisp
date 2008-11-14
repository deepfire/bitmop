(defpackage regvaldefs
  (:nicknames :rvd) 
  (:use :common-lisp :alexandria :pergamum :iterate :setc :early-eval)
  (:shadow #:space)
  (:export
   #:space #:space-name #:space-root #:define-namespace #:undefine-namespace #:with-namespaces #:set-namespace #:unify-namespaces #:purge-namespace-devices #:list-spaces
   #:layout #:layout-space #:layout-registers #:define-layout
   #:bank #:regset-space #:define-bank-accessor #:with-banks #:register-regset-name
   #:register #:reg #:reg-name #:reg-format #:reg-selector #:reg-layout #:define-register #:register-decode
   #:bitfield #:bitfield-byte #:bitfield-decode #:bitfield-regformat #:bitfield-documentation
   #:byteval
   #:device #:id #:device-id #:space-device #:device-space #:backend #:device-backend #:device-register #:space-remove-device
   #:test-devbits #:test-bits #:format-decode #:reg-selector #:reg-ext
   #:devreg #:decode #:devreg-decode #:devbit #:devbit-decode #:devbit-value #:devbits #:bits #:test-bits #:bit-value
   #:undefine-space))
