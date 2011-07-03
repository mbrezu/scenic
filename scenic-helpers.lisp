
(in-package :scenic-helpers)

(defun background (color child)
  (make-instance 'background
                 :fill-color color
                 :child child))

(defun border (color width child)
  (make-instance 'border
                 :stroke-color color
                 :stroke-width width
                 :child child))

(defun scene (width height widget)
  (make-instance 'scene
                 :width width
                 :height height
                 :widget widget))

(defun placeholder (width height)
  (make-instance 'placeholder
                 :width width
                 :height height))

(defun padding (left top right bottom child)
  (make-instance 'padding
                 :left-padding left
                 :top-padding top
                 :right-padding right
                 :bottom-padding bottom
                 :child child))

(defun uniform-padding (padding child)
  (make-instance 'padding
                 :left-padding padding
                 :top-padding padding
                 :right-padding padding
                 :bottom-padding padding
                 :child child))

(defun stack (&rest children)
  (make-instance 'stack
                 :children children))

(defun filler ()
  (make-instance 'filler))

(defun label (text &key
              (face "Courier") (size 12) (color '(0 0 0))
              (slant :normal) (weight :normal))
  (make-instance 'label
                 :text text
                 :font-face face
                 :font-size size
                 :font-color color
                 :font-slant slant
                 :font-weight weight))

(defun button (child)
  (make-instance 'button
                 :child child))

(defun button-text (text)
  (button (uniform-padding 2 (aligner (label text :size 18)))))

(defun toggle (text)
  (make-instance 'toggle-button
                 :state nil
                 :child (uniform-padding 2 (label text :size 18))))

(defun horizontal-slider (min max page)
  (make-instance 'slider
                 :orientation :horizontal
                 :min-value min
                 :max-value max
                 :page-size page
                 :current-min-position min))

(defun vertical-slider (min max page)
  (make-instance 'slider
                 :orientation :vertical
                 :min-value min
                 :max-value max
                 :page-size page
                 :current-min-position min))

(defun sizer (child &key (min-width nil) (min-height nil) (max-width nil) (max-height nil))
  (make-instance 'sizer
                 :child child
                 :min-width min-width
                 :min-height min-height
                 :max-width max-width
                 :max-height max-height))

(defun arrow (direction)
  (make-instance 'arrow :direction direction))

(defun horizontal-scrollbar (min max page)
  (make-instance 'scrollbar
                 :orientation :horizontal
                 :min-value min
                 :max-value max
                 :page-size page
                 :current-min-position min))

(defun vertical-scrollbar (min max page)
  (make-instance 'scrollbar
                 :orientation :vertical
                 :min-value min
                 :max-value max
                 :page-size page
                 :current-min-position min))

(defun image (image-path)
  (make-instance 'image
                 :image-surface (get-image image-path)))

(defun grid (column-layout-options row-layout-options children-descriptions)
  (make-instance 'grid
                 :column-layout-options column-layout-options
                 :row-layout-options row-layout-options
                 :children-descriptions children-descriptions))

(defun vertical-box (space layout-options children)
  (if (= space 0)
      (grid '(:auto) layout-options `((:column ,@(mapcar (lambda (child) (list :cell child))
                                                         children))))
      (grid '(:auto)
            (intersperse layout-options (list space :px))
            `((:column ,@(mapcar (lambda (child) (list :cell child))
                                 (intersperse children
                                              (placeholder 0 space))))))))

(defun horizontal-box (space layout-options children)
  (if (= space 0)
      (grid layout-options '(:auto) `((:row ,@(mapcar (lambda (child) (list :cell child))
                                                      children))))
      (grid (intersperse layout-options (list space :px))
            '(:auto)
            `((:row ,@(mapcar (lambda (child) (list :cell child))
                              (intersperse children
                                           (placeholder space 0))))))))

(defun aligner (child &key (horizontal :center) (vertical :center))
  (make-instance 'aligner
                 :child child
                 :horizontal horizontal
                 :vertical vertical))

(defun clipper (child)
  (make-instance 'clipper
                 :child child))

(defun glass (opacity child)
  (make-instance 'glass
                 :opacity opacity
                 :child child))

(defun henchman (children-locations children)
  (make-instance 'henchman
                 :children children
                 :children-locations children-locations))

(defun scroll-view (child &key (inside-width (expt 10 6)) (inside-height (expt 10 6)))
  (make-instance 'scroll-view
                 :child child
                 :inside-width inside-width
                 :inside-height inside-height))

(defun scroll-view-auto (child &key (inside-width (expt 10 6)) (inside-height (expt 10 6)))
  (let* ((scroll-view (scroll-view child
                                   :inside-width inside-width
                                   :inside-height inside-height))
         (hscroll (horizontal-scrollbar 0 100 10))
         (vscroll (vertical-scrollbar 0 100 10))
         (result (grid '(:auto (1 :auto))
                       '(:auto (1 :auto))
                       `((:row (:cell ,scroll-view)
                               (:cell ,vscroll))
                         (:row (:cell ,hscroll))))))
    (add-event-handler scroll-view :scroll-view-measured :bubble
                       (lambda (o evt)
                         (declare (ignore o))
                         (setf (page-size hscroll) (outer-width evt))
                         (setf (max-value hscroll) (inner-width evt))
                         (setf (page-size vscroll) (outer-height evt))
                         (setf (max-value vscroll) (inner-height evt))))
    (add-event-handler hscroll :position-changed nil
                       (lambda (o evt)
                         (declare (ignore o evt))
                         (setf (horizontal-offset scroll-view)
                               (current-min-position hscroll))
                         (invalidate scroll-view)))
    (add-event-handler vscroll :position-changed nil
                       (lambda (o evt)
                         (declare (ignore o evt))
                         (setf (vertical-offset scroll-view)
                               (current-min-position vscroll))
                         (invalidate scroll-view)))
    (add-event-handler scroll-view :scroll-view-offset-changed nil
                       (lambda (o evt)
                         (declare (ignore o evt))
                         (setf (current-min-position vscroll)
                               (vertical-offset scroll-view))
                         (setf (current-min-position hscroll)
                               (horizontal-offset scroll-view))))
    (values result scroll-view)))

(defun textbox (text cursor-position
                &key (selection-start 0) (caret-color (list 0.0 0.0 0.0))
                (selection-color (list 0.3 0.3 1.0)))
  (make-instance 'textbox
                 :text text
                 :cursor-position cursor-position
                 :selection-start selection-start
                 :caret-color caret-color
                 :selection-color selection-color
                 :background-color (list 1.0 1.0 1.0)
                 :font-face "Courier"
                 :font-size 14))

(defun wrap-stateful-button (stateful-button text)
  (let (label combination)
    (setf combination
          (horizontal-box
           2 '(:auto :auto)
           (list (aligner stateful-button)
                 (setf label
                       (make-instance 'clickable
                                      :child (uniform-padding
                                              2
                                              (aligner (label text :size 18))))))))
    (add-event-handler label :click nil
                       (lambda (o e)
                         (declare (ignore o e))
                         (setf (state stateful-button)
                               (not (state stateful-button)))))
    (values combination stateful-button)))

(defun checkbox (text)
  (wrap-stateful-button (make-instance 'checkbox :state nil)
                        text))

(defun radio-button (text)
  (wrap-stateful-button (make-instance 'radio-button :state nil)
                        text))

(defun group-stateful-buttons (default buttons)
  (labels ((all-unchecked ()
             (every (lambda (btn) (-> btn state not)) buttons)))
    (dolist (b buttons)
      (scenic:add-event-handler b :state-changed nil
                                (lambda (obj e)
                                  (declare (ignore e))
                                  (when (state obj)
                                    (dolist (b buttons)
                                      (unless (eq obj b)
                                        (setf (state b) nil))))
                                  (when (all-unchecked)
                                    (setf (state obj) t)))))
    (setf (state default) t)))