(ns palletops.locos-test
  (:use
   clojure.test
   palletops.locos))

(deftest quote-rule-test
  (is (= '[{:item :c :factor '?f} '{:x (* 3 ?f)} '(> ?f 4)]
         (quote-rule '[{:item :c :factor ?f} {:x (* 3 ?f)} (> ?f 4)]))))

(deftest quote-rule-test
  (is (= '[[{:item :c, :factor (quote ?f)}
            (palletops.locos/quasiquote {:x (* 3 ?f)})
            (palletops.locos/quasiquote (> ?f 4))]]
         (quote-rules '[[{:item :c :factor ?f} {:x (* 3 ?f)} (> ?f 4)]]))))

(deftest readme-test
  (let [rules (rules->logic-terms
               '[[{:item :a} {:x 1}]
                 ^{:name :b} [{:item :b} {:x 2}]
                 [{:item :c :factor ?f} {:x (* 2 ?f)}]
                 ^{:name :d} [{:item :c :factor ?f} {:x (* 3 ?f)} (> ?f 4)]])]

    (is (= [{:production {:x 1} :rule {:item :a}}]
           (matching-productions {:item :a} rules)))
    (let [r (apply-rule-productions {:item :a} rules)]
      (is (= {:x 1 :item :a} r))
      (is (= [{:item :a}] (-> r meta :rules))))

    (is (= [{:production {:x 2} :rule :b}]
           (matching-productions {:item :b} rules)))
    (let [r (apply-productions {:item :b} rules)]
      (is (= {:x 2 :item :b} r))
      (is (= [:b] (-> r meta :rules))))

    (is (= {:x 4 :item :c :factor 2}
           (apply-productions {:item :c :factor 2} rules)))

    (let [r (apply-productions {:item :c :factor 5} rules)]
      (is (= {:x 15 :item :c :factor 5} r))
      (is (= [{:item :c :factor '?f} :d] (-> r meta :rules))))

    (let [expr {:item :c :factor 5}]
      (is (= (merge expr {:x 15}) (apply-productions expr rules))))))

(deftest extra-key-test
  (let [rules (rules->logic-terms
               '[[{:item :a} {:x 1}]
                 [{:item :b :factor ?f} {:x (* 2 ?f)}]
                 [{:item :c :d {:factor ?f}} {:x (* 3 ?f)}]])]
    (is (= {:x 1 :item :a :extra :e}
           (apply-productions {:item :a :extra :e} rules)))
    (is (= {:x 1 :item :a :extra {:e 1}}
           (apply-productions {:item :a :extra {:e 1}} rules)))
    (is (= '{:x 4 :item :b :factor 2 :extra {:e 1}}
           (apply-productions {:item :b :factor 2 :extra {:e 1}} rules)))
    (is (= '{:x 9 :item :c :d {:factor 3} :extra {:e 1} :f2 1}
           (apply-productions
            {:item :c :d {:factor 3} :extra {:e 1} :f2 1} rules)))
    (is (= '{:x 9 :item :c :d {:factor 3 :duh 1}}
           (apply-productions {:item :c :d {:factor 3 :duh 1}} rules)))
    (let [expr {:item :c :d {:factor 3} :extra {:e 1} :f2 1}]
      (is (= '{:x 9 :item :c :d {:factor 3} :extra {:e 1} :f2 1}
             (apply-productions expr rules))))))

(deftest missing-key-test
  (let [rules (rules->logic-terms
               `[[{:item ~!_} {:x 1}]
                 [{:item ~!_ :a 1} {:x 2}]])]
    (is (= {:item :b} (apply-productions {:item :b} rules)))
    (is (= {:x 1} (apply-productions {} rules)))
    (is (= {:x 2 :a 1} (apply-productions {:a 1} rules)))))

(deftest guard-test
  (let [rules (rules [{:m ?a} {:x 1} (not-pathc ?a [:c :d])])]
    (let [r (apply-rule-productions {:m {}} rules)]
      (is (= {:m {} :x 1} r)))
    (let [r (apply-rule-productions {:m {:c {:d 1}}} rules)]
      (is (= {:m {:c {:d 1}}} r)))))
