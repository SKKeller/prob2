(ns syncclient.core
  (:require [goog.Uri :as uri]
            [goog.net.XhrIo :as xhr]
            [clojure.string :as string]
            [cljs.reader :as r]))

(enable-console-print!)

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
nested structure. keys is a sequence of keys. Any empty maps that result
will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn prep-merge [path mergemap]
  (if (seq path) (assoc-in {} path mergemap) mergemap))

(def state (atom {:current -1 :state {}}))

(defmulti mk-fn first)

(defmethod mk-fn :set [[_ path val]] (fn [s] (println :set-fn s path val)
                                       (if (seq path)
                                         (update-in s path (constantly val))
                                         (merge s val))))
(defmethod mk-fn :del-keys [[_ path dkys]]
  (fn [s] (let [x (map #(into path %) dkys)]
           (reduce (fn [s e] (dissoc-in s e)) s dkys))))
(defmethod mk-fn :merge [[_ path add]]
  (fn [s] (merge s (prep-merge path add))))
(defmethod mk-fn :concat [[_ path elems]]
  (fn [s] (update-in s path #(into % elems))))
(defmethod mk-fn :del [[_ path index]]
  (println "Call del" index "@" path)
  (fn [s] (update-in s path
                    (fn [v]
                      (into [] (remove nil?
                                       (map-indexed (fn [i v] (if ((into #{} index) i) nil v)) v)))))))


(defn update-state [updates]
  (map mk-fn updates))

(defn receiver [event]
  (let [response (.getResponseText (.-target event))
    _ (println response)
        [id changes] (r/read-string response)
        _ (println "id" id)
        _ (println "content" changes)
        chg-fkt (apply comp (map mk-fn changes))
        ]
    (swap! state (fn [cs] (let [os (:state cs)
                               ns (chg-fkt os)]
                           (assoc os :current id :state ns))))
    #_(.write js/document text)))

(defn get-state [url]
  (xhr/send url receiver "GET" ""))

(defn ^:extern pp-state []
  (println "ID:" (:current @state))
  (println "State:" (:state @state)))


(defn ^:extern get-updates []
  (let [url (str  "/data?state=" (:current @state))]
    (get-state url)))



