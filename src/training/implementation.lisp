(in-package :cl-htm.training)


(defmethod reset-context ((context basic-training-context))
  (setf (past-predictive-neurons context) +empty-vector+
        (fill-pointer (active-neurons context)) 0)
  context)
