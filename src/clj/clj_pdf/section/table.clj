(ns clj-pdf.section.table
  (:require [clj-pdf.utils :refer [get-color get-alignment split-classes-from-tag]]
            [clj-pdf.section :refer [render make-section]]
            [clj-pdf.section.cell :refer [render-pdf-cell2]])
  (:import [cljpdf.text Cell Element Rectangle Table Document]
           [cljpdf.text.pdf PdfPCell PdfPTable]))


(defn- table-header [^Document doc meta ^Table tbl header cols]
  (when header
    (let [meta?            (map? (first header))
          background-color (get-color (when meta? (:background-color (first header))))
          header-rest      (if meta? (rest header) header)
          header-data      header-rest
          set-bg           #(if background-color
                              (doto ^Cell % (.setBackgroundColor background-color)) %)]
      (if (= 1 (count header-data))
        (let [header               (first header-data)
              ^Element header-text (if (string? header)
                                     (make-section doc meta [:phrase {:style "bold"} header])
                                     (make-section doc meta header))
              header-cell          (doto (new Cell header-text)
                                     (.setHorizontalAlignment 1)
                                     (.setHeader true)
                                     (.setColspan cols))]
          (set-bg header-cell)
          (.addCell tbl header-cell))

        (doseq [h header-data]
          (let [^Element header-text (if (string? h)
                                       (make-section doc meta [:phrase {:style "bold"} h])
                                       (make-section doc meta h))
                ^Cell header-cell    (if (= Cell (type header-text))
                                       header-text
                                       (new Cell header-text))
                header-cell          (doto header-cell (.setHeader true))]
            (when-not (and (string? h)
                           (map? (second h)))
              (when-let [align (:align (second h))]
                (.setHorizontalAlignment header-cell ^int (get-alignment align))))
            (set-bg header-cell)
            (.addCell tbl header-cell)))))
    (.endHeaders tbl)))


(defn- add-table-cell
  [^Document doc ^Table tbl meta content]
  (let [[tag & cx] (when (vector? content)
                     (split-classes-from-tag (first content)))
        element    (cond
                     (= tag :cell)     content
                     (nil? content)    [:cell [:chunk meta ""]]
                     (string? content) [:cell [:chunk meta content]]
                     :else             [:cell content])]
    (.addCell tbl ^Cell (make-section doc meta element))))


(defn- add-pdf-table-cell
  [^Document doc ^PdfPTable tbl meta content]
  (let [tag (when (vector? content)
                    (get content 0))
        element    (cond
                     (= tag :pdf-cell) content
                     (= tag :pdf-cell2) content
                     (nil? content)    [:pdf-cell [:chunk meta ""]]
                     (string? content) [:pdf-cell [:chunk meta content]]
                     :else             [:pdf-cell content])]
    (if (= :pdf-cell2 tag)
      (.addCell tbl ^PdfPCell (render-pdf-cell2 doc (merge meta (get content 1)) (get content 2)))
      (.addCell tbl ^PdfPCell (make-section doc meta element)))))


(defmethod render :table
  [_ ^Document doc {:keys [align
             background-color
             border
             border-width
             cell-border
             header
             no-split-cells?
             num-cols
             offset
             padding
             spacing
             width
             widths]
      :as   meta}
   & rows]

  (when (< (count rows) 1)
    (throw (new Exception "Table must contain rows!")))

  (let [header-cols (cond-> (count header)
                      (map? (first header)) dec)
        cols        (or num-cols (apply max (cons header-cols (map count rows))))
        ^Table tbl  (doto (new Table cols (count rows))
                      (.setWidth (float (or width 100))))]

    (when widths
      (if (= (count widths) cols)
        (.setWidths tbl (int-array widths))
        (throw (new Exception (str "wrong number of columns specified in widths: " widths ", number of columns: " cols)))))

    (if (false? border)
      (.setBorder tbl Rectangle/NO_BORDER)
      (when border-width (.setBorderWidth tbl (float border-width))))

    (when (false? cell-border)
      (.setDefaultCell tbl (doto (new Cell)
                             (.setBorder Rectangle/NO_BORDER))))

    (when-let [background-color (get-color background-color)] (.setBackgroundColor tbl background-color))
    (.setPadding tbl (if padding (float padding) (float 3)))
    (if spacing (.setSpacing tbl (float spacing)))
    (if offset (.setOffset tbl (float offset)))
    (table-header doc meta tbl header cols)

    (.setAlignment tbl ^int (get-alignment align))

    (.setCellsFitPage tbl (boolean no-split-cells?))

    (doseq [row rows]
      (doseq [column row]
        (add-table-cell doc tbl (dissoc meta :header :align :offset :num-cols :width :widths) column)))

    tbl))


(defmethod render :pdf-table
  [_ ^Document doc {:keys [bounding-box
             cell-border
             footer
             header
             horizontal-align
             keep-together?
             no-split-rows?
             no-split-late?
             num-cols
             spacing-after
             spacing-before
             table-events
             width
             width-percent]
      :as   meta}
   widths
   & rows]

  (comment (when (empty? rows)
    (throw (new Exception "Table must contain at least one row"))))
  (let [header-size (if (seq header) (count header))
        footer-size (if (seq footer) (count footer))
        ;; with PdfPTable, the header and footer rows need to go first in the list
        ;; of table rows provided to it
        rows        (concat (if header-size header) (if footer-size footer) rows)]
    (when (and widths
               (not= (count widths)
                 (or num-cols (apply max (map count rows)))))
      (throw (new Exception (str "wrong number of columns specified in widths: " widths ", number of columns: " (or num-cols (apply max (map count rows)))))))

    (let [^int cols (or num-cols (apply max (map count rows)))
          tbl       (new PdfPTable cols)]

      ;; PdfPTable is pretty weird. setHeaderRows needs to be given a number that is
      ;; the sum of the total number of header _and_ footer rows that this table
      ;; will have, while setFooterRows is just the number of footer rows.
      (if (or header-size footer-size)
        (.setHeaderRows tbl (int (+ (or header-size 0) (or footer-size 0)))))
      (if footer-size (.setFooterRows tbl (int footer-size)))

      (when width (.setTotalWidth tbl (float width)))
      (when width-percent (.setWidthPercentage tbl (float width-percent)))
      (if bounding-box
        (if-not widths
          (throw (new Exception "widths must be non-nil when bounding-box is used in a pdf-table"))
          (let [[x y] bounding-box]
            (.setWidthPercentage tbl (float-array widths) (make-section [:rectangle x y]))))
        (if widths
          (.setWidths tbl (float-array widths))))

      (doseq [table-event table-events]
        (.setTableEvent tbl table-event))

      (if spacing-before (.setSpacingBefore tbl (float spacing-before)))
      (if spacing-after (.setSpacingAfter tbl (float spacing-after)))

      (.setHorizontalAlignment tbl ^int (get-alignment horizontal-align))

      (.setKeepTogether tbl (boolean keep-together?))

                                        ; these are inverted so the default if not specified matches
                                        ; PdfPTable default behaviour
      (.setSplitRows tbl (boolean (not no-split-rows?)))
      (.setSplitLate tbl (boolean (not no-split-late?)))
      (.setComplete tbl false)
      (let [it (clojure.lang.RT/iter rows)]
        (loop [i 0]
          (if (.hasNext it)
            (let [it2 (clojure.lang.RT/iter (.next it))]
              (loop []
                (if (.hasNext it2)
                  (do (add-pdf-table-cell doc tbl (merge meta (when (false? cell-border) {:set-border []})) (.next it2))
                      (recur))))
              (do 
                (if (and (= (mod i 5000) 0) (> i (or header-size 0)))
                  (.add doc tbl)) 
                (recur (inc i)))))))
      (.setComplete tbl true)
      tbl)))
