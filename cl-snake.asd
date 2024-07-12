(in-package :asdf)

(defsystem "cl-snake"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("str" "cffi" "bordeaux-threads" "cffi-libffi")
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "ffi")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-snake/tests"))))

(defsystem "cl-snake/tests"
  :author ""
  :license ""
  :depends-on ("cl-snake"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-snake"
  :perform (test-op (op c) (symbol-call :rove :run c)))
