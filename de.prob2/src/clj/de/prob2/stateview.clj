(ns de.prob2.stateview
  (:require [de.prob2.kernel :as kernel])
  (:use dorothy.core))

(defn get-selector [prob]
  (kernel/instantiate prob de.prob.statespace.AnimationSelector))

; Funktionsweise dotdot-Macro: (.. t2 getCurrentTransition getRep) für  t2.getCurrentTransition().getRep()
; diese Funktion läd ein Modell, legt einen Trace an, läd diesen als aktuelle Animation und gibt die Values des CurrentStates an
(defn make-animation [prob]
  (let
    [api (kernel/instantiate prob de.prob.scripting.Api)
      m (.b_load api "/home/katharina/Downloads/ProB/examples/LessSimple/phonebook6.mch")
      t (de.prob.statespace.Trace. m)
      t1 (.anyEvent t 0)
      t2 (.anyEvent t1 "add")
      t3 (.anyEvent t2 "add")
      ani (get-selector prob)]
    (.addNewAnimation ani t3)
    (str "test" (.. ani getCurrentTrace getCurrent getTransition getDestination getValues))))

(defn iteration->seq [iteration]
        (seq
          (reify java.lang.Iterable
            (iterator [this]
              (reify java.util.Iterator
                (hasNext [this] (.hasNext iteration))
                (next [this] (.next iteration))
                (remove [this] (.remove iteration)))
                )
            )
          )
)

(defn make-graph1 [valueset]
    (let [
      values (iteration->seq (.. valueset iterator))
      ]
    (-> (digraph [
      (subgraph [
        [:node {:shape :diamond, :style :filled, :color :darkslategray2}]
        :ROOT-NODE
        ])
      ;Knoten
      (for [element values]
        (let [value  (str (.. element getValue))]
          (if (.contains value "\u21A6") ;\u21A6 für das Unicodesymbol des Pfeils
            ;consequence
            (subgraph [
              [:node {:shape :box, :style :filled, :color :darkolivegreen2}]
              (subs value 2 (.indexOf value "\u21A6")) (subs value (+ 1 (.indexOf value "\u21A6")) (.lastIndexOf value "\u0029"))
            ])
              ;alternative
            (subgraph [
                [:node {:shape :box, :style :filled, :color :darkolivegreen2}]
                (subs value 1 (.indexOf value "\u007D"))])
          )
        )
      )
      ;Kanten
      (for [element values]
        (let [value  (str (.. element getValue))]
          (if (.contains value "\u21A6") ;\u21A6 für das Unicodesymbol des Pfeils
            ;consequence
            [(subs value 2 (.indexOf value "\u21A6")) :> (subs value (+ 1 (.indexOf value "\u21A6")) (.lastIndexOf value "\u0029"))      {:label (str (.. element getKey)),:len 1.00}]
              ;alternative
              [(subs value 1 (.indexOf value "\u007D")) :> :ROOT-NODE      {:label (str (.. element getKey)),:len 1.00}]
          )
        )
      )
      ]) dot (save! "outtest.png" {:format :png})
      )
      )
    )

(defn make-graph [valueset]
        (let [
          values (iteration->seq (.. valueset iterator))
          ]
        (-> (digraph [
          (subgraph [
            [:node {:shape :diamond, :style :filled, :color :darkslategray2}]
            :ROOT-NODE
            ])
          ;Knoten
          (for [element values]
            (let [value  (str (.. element getValue))]
              (if (.contains value "\u21A6") ;\u21A6 für das Unicodesymbol des Pfeils
                ;consequence
                (subgraph [
                  [:node {:shape :box, :style :filled, :color :darkolivegreen2}]
                  (loop [index (.indexOf value "\u21A6") erg [] nodes value]
                    (if (= index -1)
                      (if (> (.length nodes) 0)
                        (conj erg (subs nodes 2 (.indexOf nodes "\u21A6")) (subs nodes (+ 1 (.indexOf nodes "\u21A6")) (- (.indexOf nodes "\u007D") 1)) )
                        erg
                      )
                      (recur (.indexOf nodes (.indexOf nodes "\u21A6")) (conj erg (subs nodes (+ 1(.indexOf nodes "\u0028")) (.indexOf nodes "\u21A6")) (subs nodes (+ 1 (.indexOf nodes "\u21A6")) (.indexOf nodes "\u0029"))  ) (subs nodes (+ 1 (.indexOf nodes "\u0029" ))) )
                    )
                  )
                ])
                  ;alternative
                (subgraph [
                    [:node {:shape :box, :style :filled, :color :darkolivegreen2}]
                    (subs value 1 (.indexOf value "\u007D"))])
              )
            )
          )
          ;Kanten
          (for [element values]
            (let [value  (str (.. element getValue))]
              (if (.contains value "\u21A6") ;\u21A6 für das Unicodesymbol des Pfeils
                ;consequence
                ["test" :> :ROOT-NODE {:label (str (.. element getKey)),:len 1.00}]
                ;[(subs value 2 (.indexOf value "\u21A6")) :> (subs value (+ 1 (.indexOf value "\u21A6")) (.lastIndexOf value "\u0029"))      {:label (str (.. element getKey)),:len 1.00}]
                  ;alternative
                  [(subs value 1 (.indexOf value "\u007D")) :> :ROOT-NODE      {:label (str (.. element getKey)),:len 1.00}]
              )
            )
          )
          ]) dot (save! "outtest.png" {:format :png})
          )
          )
        )

(defn get-values [prob]
  (let
    [ani (get-selector prob)
    valuemap (.. ani getCurrentTrace getCurrent getTransition getDestination getValues)
    values (.. valuemap entrySet)
    iter (.. values iterator)]
    (make-graph values)
    (str "test2" valuemap )
  )
)

(defn make-graph2[]
  (-> (digraph [
    ;[:node {:shape :box, :style :filled, :color :lightgrey}]
    ;:Name :Code

    (subgraph :cluster_1 [
      {:style :filled, :color :lightgrey, :shape :box, :label "Name"}
      (node-attrs {:shape :box, :style :filled, :color :darkolivegreen2})
      [:Name1 :name3]
    ])
    (subgraph :cluster_0 [
      {:style :filled, :color :lightgrey, :shape :box, :label "Code"}
      (node-attrs {:shape :box, :style :filled, :color :yellow})
      [:Code5]
      ])
    [:node {:shape :diamond, :style :filled, :color :darkslategray2}]
    :ROOT-NODE

    ;Kanten
    [:Name1 :Code5]
    [:Code5 :ROOT-NODE]
    [:Name1 :ROOT-NODE]

    ]) dot (save! "out2.png" {:format :png})))

(defn create-state-view [prob trace]
  (let [ani (get-selector prob)]
    (str  "My awesomer visualization for " trace " in " ani))
  ;(make-graph2)
  (make-animation prob)
  (get-values prob)
  )
