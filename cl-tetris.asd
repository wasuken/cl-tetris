(defsystem "cl-tetris"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-tetris/tests"))))

(defsystem "cl-tetris/tests"
  :author ""
  :license ""
  :depends-on ("cl-tetris"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-tetris"
  :perform (test-op (op c) (symbol-call :rove :run c)))
