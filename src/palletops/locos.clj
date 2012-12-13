(ns palletops.locos
  "Allow use of declarative rules for specifying configuration options"
  (:refer-clojure :exclude [==])
  (:use
   [clojure.core.logic
    :only [all defc fresh membero partial-map prep project run* s# walk-term
           == >fd <fd]]
   [clojure.tools.logging :only [warnf]]
   [clojure.walk :only [postwalk]]))

(defn deep-merge
  "Recursively merge maps."
  [& ms]
  (letfn [(f [a b]
            (if (and (map? a) (map? b))
              (deep-merge a b)
              b))]
    (apply merge-with f ms)))

;;; A simple quasiquote (no unquoting)
(defn qq-symbol
  "Quasiquote a symbol"
  [sym &env]
  (if (find &env sym)
    sym
    (let [v (resolve sym)]
      (if v
        (symbol (-> v meta :ns ns-name name) (-> v meta :name name))
        sym))))

(defn quasiquote*
  "Quasiquote a form"
  [form &env]
  (list 'quote
        (postwalk
         (fn [x]
           (if (and (symbol? x))
             (qq-symbol x &env)
             x))
         form)))

(defmacro quasiquote
  [form]
  (quasiquote* form &env))

(def ^{:private true :doc "Translate to logic functions"}
  op-map
  {`> >fd
   `< <fd
   '> >fd
   '< <fd})

(defn ->pmap
  "Return a value that will use partial-map unification"
  [x]
  (if (and (map? x)
           (not (instance? clojure.lang.IRecord x))
           (not (instance? clojure.core.logic.PMap x)))
    (partial-map x)
    x))

(defn recursive-partial-map
  "Return a value that will use partial-map unification on sub-maps"
  [x]
  (->pmap (walk-term x ->pmap)))

(defn rule->logic-terms
  "Takes a rule, specified as a pattern, a production and zero or more guards,
   and return logic terms that encode them."
  [rule]
  (let [[pattern production & guards] (prep rule)]
    {:rule (or (-> rule meta :name) (first rule))
     :pattern (recursive-partial-map pattern)
     :production production
     :guards (fn []
               (if (seq guards)
                 (fn [substitutions]
                   (reduce
                    (fn [subs [op & args]]
                      (let [f (or (op-map op) (try @(resolve op)
                                                   (catch Exception e
                                                     (throw
                                                      (Exception.
                                                       (str
                                                        "Couldn't resolve " op)
                                                       e)))))
                            f1 (apply f args)]
                        (f1 subs)))
                    substitutions
                    guards))
                 s#))}))

(defn rules->logic-terms
  "Return a sequence of map of terms to be used in unification of the patterns,
  productions and guards."
  [rules]
  (map rule->logic-terms rules))

(defn ^:internal quote-lvars
  "Quote any lvars in a rule vector"
  [form]
  (postwalk
   (fn [x]
     (if (and (symbol? x) (.startsWith (name x) "?"))
       (list 'quote x)
       x))
   form))

(defn ^:internal quote-rule
  "Quote any lvars in a rule vector's pattern, and quote production and guards."
  [[pattern production & guards :as rule]]
  (with-meta
    (apply vector
           (quote-lvars pattern)
           (list `quasiquote production)
           (map (comp  #(list `quasiquote %)) guards))
    (meta rule)))

(defn ^:internal quote-rules
  "Quote un-evaluated rules"
  [rules]
  (map quote-rule rules))

(defmacro rules
  "Define a set of rules."
  [& rules]
  `(rules->logic-terms ~(vec (quote-rules rules))))

(defmacro defrules
  "Define a named set of rules."
  [name & rules]
  `(def ~name (rules->logic-terms ~(vec (quote-rules rules)))))

;; guarantee that a path of keys does not occur in map x
(defc not-pathc [x path]
  (= (get-in x path ::not-found) ::not-found))

(defc get-c [x s]
  (not= (get s x ::not-found) ::not-found))

(defn matching-productions
  "Takes an expression, and applies rules to it, returning a sequence
   of valid productions."
  [expr rules]
  (run* [q]
    (fresh [pattern production guards rule]
      (membero
       {:pattern pattern :production production :guards guards :rule rule}
       rules)
      (== expr pattern)
      (== q {:production production :rule rule})
      (project [guards] (guards)))))

(defn apply-rule-productions
  "Applies first matching rewrite rule on layer spec."
  [expr rules]
  (if-let [productions (seq (matching-productions expr rules))]
    (do
      (when-let [invalid (seq (remove map? productions))]
        (warnf "Skipping locos productions %s" (vec invalid)))
      (reduce
       (fn reduce-productions [expr {:keys [production rule] :as match}]
         (if (or (.contains (str production) ":clojure.core.logic/not-found")
                 (re-find #"_\.[0-9]+" (str production)))
           (do (warnf "Skipping locos production %s" production)
               expr)
           (let [p (try
                     (eval production)
                     (catch Exception e
                       (throw
                        (Exception.
                         (str "Couldn't eval locos production " production)
                         e))))]
             (-> (deep-merge expr p)
                 (with-meta (update-in (meta expr) [:rules] concat [rule]))))))
       expr
       (filter map? productions)))
    expr))

(defn apply-productions
  "Apply matching productions until no productions match."
  [expr rules]
  (->> expr
       (iterate #(apply-rule-productions % rules))
       (partition 2 1)
       (drop-while #(apply not= %))
       (ffirst)))

(def ^{:doc "Provides a value that will unify with a missing key map"}
  !_ :clojure.core.logic/not-found)
