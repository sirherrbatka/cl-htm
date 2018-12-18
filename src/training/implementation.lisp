(in-package :cl-htm.training)


(defmethod reset-context ((context basic-context))
  (setf (past-predictive-neurons context) +empty-vector+
        (fill-pointer (active-neurons context)) 0)
  (clrhash (read-extra-data context))
  context)


(defmethod extra-data ((context basic-context) location)
  (gethash location (read-extra-data context)))


(defmethod (setf extra-data) (new-val (context basic-context) location)
  (setf (gethash location (read-extra-data context)) new-val))
