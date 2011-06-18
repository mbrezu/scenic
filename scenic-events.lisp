
(in-package :scenic)

(defgeneric adjust-event-coordinates (event offset-x offset-y))

;;; EVENT class.

(defclass event ()
  ((handled :accessor handled :initarg :handled :initform nil)))

(defmethod adjust-event-coordinates ((event event) offset-x offset-y)
  (declare (ignore event offset-x offset-y)))

;;; MOUSE-EVENT class.

(defclass mouse-event (event)
  ((mouse-x :accessor mouse-x :initarg :mouse-x :initform nil)
   (mouse-y :accessor mouse-y :initarg :mouse-y :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

(defmethod adjust-event-coordinates ((event mouse-event) offset-x offset-y)
  (incf (mouse-x event) offset-x)
  (incf (mouse-y event) offset-y))

(gen-print-object mouse-event (mouse-x mouse-y modifiers))

;;; MOUSE-MOVE-EVENT

(defclass mouse-move-event (mouse-event)
  ((mouse-rel-x :accessor mouse-rel-x :initarg :mouse-rel-x :initform nil)
   (mouse-rel-y :accessor mouse-rel-y :initarg :mouse-rel-y :initform nil)))

(gen-print-object mouse-move-event (mouse-x mouse-y modifiers mouse-rel-x mouse-rel-y))

;;; MOUSE-BUTTON-EVENT

(defclass mouse-button-event (mouse-event)
  ((mouse-button :accessor mouse-button :initarg :mouse-button :initform nil)))

(gen-print-object mouse-button-event (mouse-x mouse-y modifiers mouse-button))

;;; KEY-EVENT class.

(defclass key-event (event)
  ((key :accessor key :initarg :key :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)
   (unicode :accessor unicode :initarg :unicode :initform nil)))

(gen-print-object key-event (key modifiers unicode))

;;; SCROLL-VIEW-MEASURED event.
(defclass scroll-view-measured-event (event)
  ((inner-width :accessor inner-width :initarg :inner-width :initform nil)
   (inner-height :accessor inner-height :initarg :inner-height :initform nil)
   (outer-width :accessor outer-width :initarg :outer-width :initform nil)
   (outer-height :accessor outer-height :initarg :outer-height :initform nil)))

(gen-print-object scroll-view-measured-event
                  (inner-width inner-height outer-width outer-height))


