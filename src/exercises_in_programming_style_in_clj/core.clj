;Author Rigoberto Leander Salgado Reyes <rlsalgado2006 @gmail.com>
;
;Copyright 2016 by Rigoberto Leander Salgado Reyes.
;
;This program is licensed to you under the terms of version 3 of the
;GNU Affero General Public License. This program is distributed WITHOUT
;ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
;MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
;AGPL (http:www.gnu.org/licenses/agpl-3.0.txt) for more details.


(ns exercises-in-programming-style-in-clj.core
  (:require [clojure.string :as s])
  (:gen-class))

(declare infinite-mirror-wf-print infinite-mirror-count
         read-file read-file* split-in-words split-in-words*
         normalize* remove-stop-words* frequencies**
         sort** printme bind)


(def data-file "data.txt")


(defn -main
  ""
  [& args]
  (println "<---------------- INFINITE MIRROR (RECURSION) ---------------->")
  (infinite-mirror-wf-print
   (infinite-mirror-count
    (->> data-file
         slurp
         s/split-lines
         (filter not-empty)
         (mapcat (partial re-seq #"[\wáéíóú]+")))
    #{:a :al :el :en :lo :las :y
      :del :una :su :le :se :la
      :que :fue :los :de :por} {}))

  (println "<---------------- KICK FORWARD (CONTINUATION-PASSING STYLE) ---------------->")
  (read-file data-file split-in-words)

  (println "<---------------- THE ONE (MONADS) ---------------->")
  (->> {:value data-file}
     (bind read-file*)
     (bind split-in-words*)
     (bind normalize*)
     (bind remove-stop-words*)
     (bind frequencies**)
     (bind sort**)
     printme)
  )



;---------------------------------------------------------------------------
; INFINITE MIRROR (RECURSION)
;---------------------------------------------------------------------------


(defn infinite-mirror-count
  ""
  [[first & word-list] stopwords wordfreqs]
  (if (empty? first)
    wordfreqs
    (let[word (->> first s/lower-case keyword)]
      (recur word-list stopwords
             (if (word stopwords)
               wordfreqs
               (update wordfreqs word #(if (nil? %) 1 (inc %))))))))


(defn infinite-mirror-wf-print
  ""
  ([wordfreqs]
   (infinite-mirror-wf-print
    (into
     (sorted-map-by (fn[k1 k2](compare [(get wordfreqs k2) k2][(get wordfreqs k1) k1])))
     wordfreqs) 10))
  ([wordfreqs amount]
   (let[[k v] (first wordfreqs)]
     (when (and (pos? amount) k)
       (printf "%s - %s\n" k v)
       (recur (rest wordfreqs) (dec amount))))))


;; (infinite-mirror-wf-print
;;  (infinite-mirror-count
;;   (->> data-file
;;        slurp
;;        s/split-lines
;;        (filter not-empty)
;;        (mapcat (partial re-seq #"[\wáéíóú]+")))
;;   #{:a :al :el :en :lo :las :y
;;     :del :una :su :le :se :la
;;     :que :fue :los :de :por} {}))


;---------------------------------------------------------------------------
; KICK FORWARD (CONTINUATION-PASSING STYLE)
;---------------------------------------------------------------------------


(declare normalize scan remove-stop-words print-text sort* frequencies*)

(defn read-file
  ""
  [path-to-file func]
  ;split-in-words
  (func (->> path-to-file slurp s/split-lines (filter not-empty)) normalize))


(defn split-in-words
  ""
  [str-data func]
  ;normalize
  (func (mapcat (partial re-seq #"[\wáéíóú]+") str-data) remove-stop-words))


(defn normalize
  ""
  [str-data func]
  ;remove-stop-words
  (func (map #(->> % s/lower-case keyword) str-data) frequencies*))


(defn remove-stop-words
  ""
  [word-list func]
  (let[stop-words #{:a :al :el :en :lo :las :y
                    :del :una :su :le :se :la
                    :que :fue :los :de :por}]
    ;frequencies*
    (func (filter (complement stop-words) word-list) sort*)))


(defn frequencies*
  ""
  [word-list func]
  ;sort*
  (func ((fn[[word & words] freq]
           (if (nil? word)
             freq
             (recur words (update freq word #(if (nil? %) 1 (inc %)))))) word-list {})
        print-text))


(defn sort*
  ""
  [wf func]
  ;print-text
  (func(into
        (sorted-map-by (fn[k1 k2](compare [(get wf k2) k2][(get wf k1) k1])))
        wf) identity))


(defn print-text
  ""
  ([word-freqs func] (print-text word-freqs 10 func))
  ([word-freqs amount func]
   (let[[k v] (first word-freqs)]
     (when (and (pos? amount) k)
       (printf "%s - %s\n" (str k) v)
       (recur (rest word-freqs) (dec amount) func)))))

;; (read-file data-file split-in-words)


;---------------------------------------------------------------------------
; THE ONE (MONADS)
;---------------------------------------------------------------------------


(defn read-file*
  ""
  [path-to-file]
  (->> path-to-file
       slurp
       s/split-lines
       (filter not-empty)))


(defn split-in-words*
  ""
  [str-data]
  (mapcat (partial re-seq #"[\wáéíóú]+") str-data))


(defn normalize*
  ""
  [str-data]
  (map #(->> % s/lower-case keyword) str-data))


(defn remove-stop-words*
  ""
  [word-list]
  (let[stop-words #{:a :al :el :en :lo :las :y
                    :del :una :su :le :se :la
                    :que :fue :los :de :por}]
    (filter (complement stop-words) word-list)))


(defn frequencies**
  ""
  [word-list]
  ((fn[[word & words] freq]
     (if (nil? word)
       freq
       (recur words (update freq word #(if (nil? %) 1 (inc %))))))
   word-list {}))


(defn sort**
  ""
  [wf]
  (into (sorted-map-by (fn[k1 k2](compare [(get wf k2) k2][(get wf k1) k1]))) wf))


(defn bind
  ""
  [func obj]
  (update obj :value func))

(defn printme
  ""
  ([{word-freqs :value}]
   (print-text word-freqs 10))
  ([word-freqs amount]
   (let[[k v] (first word-freqs)]
     (when (and (pos? amount) k)
       (printf "%s - %s\n" (str k) v)
       (recur (rest word-freqs) (dec amount))))))

;; (->> {:value data-file}
;;      (bind read-file)
;;      (bind split-in-words)
;;      (bind normalize)
;;      (bind remove-stop-words)
;;      (bind frequencies*)
;;      (bind sort*)
;;      printme)




















