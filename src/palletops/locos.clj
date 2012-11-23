(ns palletops.locos
  "Allow use of declarative rules for specifying configuration options"
  (:refer-clojure :exclude [==])
  (:use
   [clojure.core.logic
    :only [all fresh membero partial-map prep project run* s# walk-term
           == >fd <fd]]))

(def ^{:private true :doc "Translate to logic functions"}
  op-map
  {`> >fd
   `< <fd
   '> >fd
   '< <fd})

(defn ->pmap
  "Do partial-map unification on all sub-keys"
  [x]
  (if (map? x)
    (partial-map x)
    x))

(defn rule->logic-fns
  "Takes a rule, specified as a pattern, a production and zero or more guards,
   and return logic functions that encode them."
  [rule]
  (let [[pattern production & guards] (prep rule)]
    [(fn [expr] (== expr (->pmap (walk-term pattern ->pmap))))
     (fn [prod] (== prod production))
     (fn []
       (if (seq guards)
         (fn [substitutions]
           (reduce
            (fn [subs [op & args]]
              ((apply (op-map op op) args) subs))
            substitutions
            guards))
         s#))]))

(defn data-rule?
  "Predicate to check for declarative rule."
  [rule]
  (vector? rule))

(defn rules->logic-fns
  "Return a vector of logic functions, that unify the pattern, the production,
   and that apply guard constraints."
  [rules]
  (for [rule rules]
    (if (data-rule? rule)
      (rule->logic-fns rule)
      rule)))

(defmacro defrules
  "Define a named set of rules."
  [name & rules]
  `(def ~name ~(vec (rules->logic-fns rules))))

(defn apply-rules
  "Takes an expression, and applies rules to it, returning a sequence
   of valid productions."
  [expr rules]
  (run* [q]
    (fresh [pattern production guards]
      (membero [pattern production guards] rules)
      (project [pattern production guards]
        (all
         (pattern expr)
         (production q)
         (guards))))))

(defn config
  "Takes an expression and some rules, returning a merge of the eval'd values of
   each matching production."
  [expr rules]
  (reduce #(merge %1 (eval %2)) {} (apply-rules expr rules)))
