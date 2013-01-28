(ns flapjax.dom
  (:require
    [hlisp.env  :as hl]
    [jayq.core  :as jq]
    [jayq.util  :as ju]))

(declare ids)

(defn id      [e] (peek (ids e)))
(defn ids     [e] (.-ids e))
(defn id!     [e] (if-not (seq (ids e)) (hl/clone e) e))
(defn is-jq?  [e] (string? (.-jquery e)))

(->
  (jq/$ "body")
  (.on "submit" (fn [event] (.preventDefault event))))

(defn filter-id
  [x]
  (fn [v]
    (< 0 (->
           (jq/$ (.-target v))
           (.parentsUntil "body")
           (.andSelf)
           (.filter (str "[data-hl~='" x "']"))
           (.size)))))

(defn filter-not-disabled
  [v]
  (->
    (jq/$ (.-target v))
    (.is "[data-disabled]")
    not))

(defn find-id
  [x]
  (jq/$ (str "[data-hl~='" x "']")))

(def dom-get (comp find-id id))

(defn delegate-blur!
  [e]  
  (let [x (id e)]
    (when-not (::delegated-blur (meta e))
      (jq/$ (fn []
              (let [ev (.Event js/jQuery "hl-blur")
                    el (find-id x)]
                (set! (.-target ev) (.get el 0))
                (.on el "blur" #(.trigger (jq/$ "body") ev)))) 
            (vary-meta e assoc ::delegated-blur true)))))

(defn- text-val!
  ([e]
   (.val e))
  ([e v]
   (-> e
     (.val v)
     (.trigger "change"))))

(defn- check-val!
  ([e]
   (.is e ":checked"))
  ([e v]
   (-> e
     (.prop "checked" (boolean v))
     (.trigger "change"))))

(defn value!
  [elem & args] 
  (let [e (dom-get elem)]
    (case (.attr e "type")
      "checkbox" (apply check-val! e args)
      (apply text-val! e args))))

(defn attr!
  ([elem k]
   (jq/attr (dom-get elem) k))
  ([elem k v & kvs]
   (let [e (dom-get elem)] 
     (mapv (fn [[k v]]
             (case v
               true   (jq/attr e k k)
               false  (.removeAttr e k)
               (jq/attr e k v)))
           (partition 2 (list* k v kvs)))
     elem)))

(defn remove-attr!
  [elem k & ks]
  (let [e (.removeAttr (dom-get elem) k)]
    (when (seq ks)
      (mapv #(.removeAttr e %) ks))
    elem))

(defn toggle-class!
  ([elem c] 
   (.toggleClass (dom-get elem) c)) 
  ([elem c switch] 
   (.toggleClass (dom-get elem) c switch)))

(defn add-class!
  [elem c & cs]
  (let [e (.addClass (dom-get elem) c)]
    (when (seq cs)
      (mapv #(.addClass e %) cs))
    elem))

(defn remove-class!
  [elem c & cs]
  (let [e (.removeClass (dom-get elem) c)]
    (when (seq cs)
      (mapv #(.removeClass e %) cs))
    elem))

(defn css!
  ([elem k v]
   (.css (dom-get elem) k v)
   elem)
  ([elem o]
   (let [ret (.css (dom-get elem) (ju/clj->js o))]
     (if (is-jq? ret) elem ret))))

(defn toggle!
  [elem v]
  (.toggle (dom-get elem) v))

(defn slide-toggle!
  [elem v]
  (if v
    (.slideDown (.hide (dom-get elem)) "fast")
    (.slideUp (dom-get elem) "fast")))

(defn fade-toggle!
  [elem v]
  (if v
    (.fadeIn (.hide (dom-get elem)) "fast")
    (.fadeOut (dom-get elem) "fast")))

(defn focus!
  [elem _]
  (.focus (dom-get elem)))

(defn select!
  [elem _]
  (.select (dom-get elem)))

(defn text!
  [elem v]
  (.text (dom-get elem) v))

(defn set-nodeValue!
  [node v]
  (set! (.-nodeValue node) v) 
  node)

(defn disabled?
  [elem]
  (.is (dom-get elem) "[data-disabled]"))
