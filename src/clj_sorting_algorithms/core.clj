(ns clj-sorting-algorithms.core)

(defn mergesort [lns]
  (if (empty? lns) lns
    (let [merge-lists
          (fn [l1 l2 res]
            (cond
             (and (empty? l1) (empty? l2)) res
             (empty? l1) (into l2 res)
             (empty? l2) (into l1 res)
             (< (first l1) (first l2))
             (recur (rest l1) l2 (cons (first l1) res))
             :else
             (recur l1 (rest l2) (cons (first l2) res))))
          merge-pairs
          (fn [groups res]
            (if (empty? groups)
              res
              (recur
               (drop 2 groups)
               (cons (merge-lists
                      (first groups)
                      (second groups)
                      '()) res))))]
      (loop [pairs (map #(cons % '()) lns)]
        (if (= 1 (count pairs))
          (first pairs)
          (recur (merge-pairs pairs '())))))))

(defn bubblesort [lns]
  (let [swap
        (fn swap [lns]
          (if (or (= 1 (count lns)) (empty? lns))
            lns
            (cons (min (first lns) (second lns))
                  (swap (cons
                         (max (first lns) (second lns))
                         (drop 2 lns))))))]
    (loop [sns '()
           lns lns]
      (if (empty? lns)
        sns
        (let [s (swap lns)]
          (recur (cons (last s) sns)
                 (butlast s)))))))

(defn insertionsort [lns]
  (let
    [insert
     (fn insert [n lns]
       (if (or (empty? lns) (< n (first lns)))
         (cons n lns)
         (cons (first lns)
               (insert n (rest lns)))))]
    (loop [sns '()
           lns lns]
      (if (empty? lns)
        sns
        (recur
         (insert (first lns) sns)
         (rest lns))))))
