(ns palletops.locos-test
  (:use
   clojure.test
   palletops.locos))

(defrules sizing
  [{:roles #{:name-node} :hardware {:ram ?r }}
   {:namenode-mx (* ?r 0.8)}]
  [{:roles #{:name-node :job-tracker} :hardware {:ram ?r }}
   {:namenode-mx (* ?r 0.4)
    :jobtracker-mx (* ?r 0.3)}]
  [{:roles #{:name-node :job-tracker} :hardware {:ram ?r }}
   {:namenode-mx (* ?r 0.5)}
   [> ?r 1024]]
  [{:roles #{:job-tracker} :hardware {:ram ?r }}
   {:jobtracker-mx (* ?r 0.8)}])

(deftest config-test
  (is (= {:namenode-mx (* 1024 0.8)}
         (config {:roles #{:name-node} :hardware {:ram 1024}} sizing)))
  (is (= {:jobtracker-mx (* 1024 0.3), :namenode-mx (* 1024 0.4)}
         (config
          {:roles #{:name-node :job-tracker} :hardware {:ram 1024}}
          sizing)))
  (is (= {:jobtracker-mx (* 1025 0.3), :namenode-mx (* 1025 0.5)}
         (config
          {:roles #{:name-node :job-tracker} :hardware {:ram 1025}}
          sizing)))
  (is (= {:jobtracker-mx (* 1024 0.8)}
         (config {:roles #{:job-tracker} :hardware {:ram 1024}} sizing))))

(deftest readme-test
  (let [rules (rules->logic-fns
               '[[{:item :a} {:x 1}]
                 [{:item :b} {:x 2}]
                 [{:item :b :factor ?f} {:x (* 2 ?f)}]
                 [{:item :b :factor ?f} {:x (* 3 ?f)} [> ?f 4]]])]
    (is (= {:x 1} (config {:item :a} rules)))
    (is (= {:x 2} (config {:item :b} rules)))
    (is (= {:x 4} (config {:item :b :factor 2} rules)))
    (is (= {:x 15} (config {:item :b :factor 5} rules)))))
