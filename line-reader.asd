(defpackage :line-reader-system
  (:use :common-lisp :asdf))

(in-package :line-reader-system)

(defsystem :line-reader
  :name "line-reader"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.1.0"
  :licence "MIT"
  :description "user customizable line stream"
  :serial t
  :components ((:file "line-reader"))
  )
