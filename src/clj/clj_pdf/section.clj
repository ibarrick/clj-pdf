(ns clj-pdf.section
  (:require [clj-pdf.utils :refer [split-classes-from-tag get-class-attributes]]))


(declare ^:dynamic *cache*)


(defmulti render (fn [tag doc meta & els] tag))
(defmethod render :default [tag doc meta & els]
  (throw (ex-info (str "invalid tag: " tag) {:meta meta :content els})))


(defn- keywordize [value]
  (if (string? value) (keyword value) value))


(defn make-section
  ([doc element]
   (cond
     (empty? element)             ""
     (every? sequential? element) (doseq [item element]
                                    (make-section doc item))
     element                      (make-section doc {} element)
     :else                        ""))
  ([doc meta element]
   (try
     (cond
       (string? element) element
       (nil? element)    ""
       (number? element) (str element)
       :else
       (let [[tag & content]  element
             tag              (keywordize tag)
             [tag & classes]  (split-classes-from-tag tag)
             class-attrs      (get-class-attributes (:stylesheet meta) classes)
             [attrs elements] (if (map? (first content))
                                [(first content) (rest content)]
                                [nil content])
             new-meta         (cond-> meta
                                class-attrs (merge class-attrs)
                                attrs       (merge attrs))]

         (apply render tag doc new-meta elements)))
     (catch Exception e
       (println e)
       (throw e)
       (comment (prn meta element)
       (throw (ex-info "failed to parse element" {:meta meta :element element} e)))))))


(defn make-section-or [doc if-string meta item]
  (if (string? item)
    (render if-string doc meta item)
    (make-section doc meta item)))


;; that require is here to overcome circular import
(require '[clj-pdf.section core cell chart svg table])
