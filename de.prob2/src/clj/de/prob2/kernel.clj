(ns de.prob2.kernel
  (:require [com.stuartsierra.component :as component])
  (:import de.prob.Main
           (de.prob.statespace AnimationSelector Trace IModelChangedListener IAnimationChangeListener StateSpace)))


(defn kebap-case
  ([cls] (kebap-case cls ""))
  ([cls add]
     (keyword
      (str (clojure.string/join
            "-"
            (map (fn [s] (.toLowerCase s))
                 (map second (re-seq #"([A-Z][a-z]*)" (.getSimpleName cls))))) add))))

(defn default-map [v m]
  (into  {:type (kebap-case (class v)) :name (.toString v)} m))


(defprotocol Transform (transform [this]))
(extend-protocol Transform
  de.prob.model.representation.Machine
  (transform [v] (default-map v {}))
  de.prob.model.eventb.Context
  (transform [v] (default-map v {}))
  de.prob.model.representation.BSet
  (transform [v] (default-map v {}))
  de.prob.model.representation.AbstractFormulaElement
  (transform [v] (default-map v {:formula-id (.. v getFormula getFormulaId getUUID)}))
  de.prob.model.classicalb.Operation
  (transform [v] {:type (kebap-case (class v)) :name (.getName v) :output (.getProperty v "output") :parameter (.getProperty v "parameters")})
  de.prob.model.eventb.Event
  (transform [v] {:type (kebap-case (class v)) :name (.getName v)})
  de.prob.model.representation.Action
  (transform [v] (default-map v {}))
  java.lang.Object
  (transform [o] {:type :unknown :class (type o) :object (.toString o)}))

(def exclude #{de.prob.model.eventb.ProofObligation})

(declare extractE)

(defn extractV [x]
  (let [c (.getKey x) lv (.getValue x)]
    [(keyword (.. c getSimpleName toLowerCase)) (if (exclude c) [] (map extractE lv))]))

(defn extractE [absel]
  (let [chd (.getChildren absel)
            tgt (transform absel)]
        (if (seq chd)
          (do (->> chd (map extractV) (into tgt)))
          tgt)))




(defn notify-model-changed [{:keys [clients send-fn!] :as sente} state-space]
  (doseq [c (:any @clients)] (send-fn! c [::model-changed {:space :dude}])))
(defn notify-trace-changed [send-fn trace current?])
(defn notify-animator-busy [busy?])






(defn instantiate [{inj :injector :as prob} cls]
  (.getInstance inj cls))

(defn- install-handlers [sente injector]
  (let [animations (.getInstance injector AnimationSelector)
        listener
        (reify
          IModelChangedListener
          (modelChanged [this state-space] (notify-model-changed sente state-space))
          IAnimationChangeListener
          (traceChange [this trace current] (notify-trace-changed sente trace current))
          (animatorStatus [this busy] (notify-animator-busy busy)))]
    (.registerAnimationChangeListener animations listener)
    (.registerModelChangedListener animations listener)
    listener))

(defrecord ProB [injector listener sente]
  component/Lifecycle
  (start [this]
    (if injector
      this
      (do (println "Preparing ProB 2.0 Kernel")
          (let [injector (Main/getInjector)
                _ (println " -> Got the injector")
                listener (install-handlers sente injector)
                _ (println " -> Installed Listeners")]
            (assoc this :injector injector :listener listener)))))
  (stop [this]
    (if injector (do (println "Shutting down ProB 2.0")
                     (dissoc this :injector :listener))
        this)))

(defn prob []
  (component/using (map->ProB {}) [:sente]))
