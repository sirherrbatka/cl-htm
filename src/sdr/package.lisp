(in-package #:cl-user)


(defpackage #:cl-htm.sdr
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:active-neurons
   #:read-active-neurons
   #:clear-all-active
   #:clone
   #:set-active
   #:select-active
   #:sdr))
