(ns advent2020.day21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(defn parse [input]
  (->> input
       str/split-lines
       (map #(re-matches #"([a-z ]+)\(contains ([a-z, ]+)\)" %))
       (map #(vector (str/split (nth % 1) #" ")
                     (str/split (nth % 2) #", ")))))

(defn group-by-allergen [data]
  (->> (for [item data
             allergen (second item)]
         [allergen (first item)])
       (group-by first)))

(defn count-each-ingredient
  "data is a seq of [allergen [ingredients]], the allergen is discarted, returns a map counting each ingredient"
  [data]
  (->> (for [item data
             ingredient (second item)]
         ingredient)
       frequencies))

(defn allergen-ingredient-frequency [allergen-map]
  (map (fn [[allergen ingredient-list]]
         {:allergen allergen
          :prod-cnt (count ingredient-list)
          :ingredient-freq (count-each-ingredient ingredient-list)})
       allergen-map))

(defn get-possible-allergens [allergen-record]
  (let [{:keys [allergen prod-cnt ingredient-freq]} allergen-record]
    {:allergen allergen
     :possible-ingredients (seq (->> (filter (fn [[k v]] (= v prod-cnt)) ingredient-freq)
                                     (map first)))}))

(defn all-ingredients [data]
  (->> data
       (mapcat first)
       set))


(defn task-1 [data]
  (let [all (all-ingredients data)
        possibly-allergic (->> data
                               group-by-allergen
                               allergen-ingredient-frequency
                               (map get-possible-allergens)
                               (mapcat :possible-ingredients)
                               (into #{}))
        absolutely-clear (set/difference all possibly-allergic)]
    
    (->> data
         (mapcat first)
         (filter absolutely-clear)
         count)
    
    ))

;(print-table [:allergen :possible-ingredients])
;(print-table [:allergen :prod-cnt :ingredient-freq])

(task-1 (parse testdata))

(task-1 (parse (slurp "day21.txt")))

(let [[absolute-allergens possible-allergens]
      (->> (parse (slurp "day21.txt"))
           group-by-allergen
           allergen-ingredient-frequency
           (map get-possible-allergens)
           (sort-by #(count (:possible-ingredients %)))
           (reduce (fn [[absolute-allergens possible-allergens] {:keys [allergen possible-ingredients]}]
                     (let [filtered-candidates (remove #(contains? absolute-allergens %) possible-ingredients)]
                       (if (not= (count filtered-candidates) 1)
                         [absolute-allergens (conj possible-allergens {:allergen allergen :possible-ingredients filtered-candidates})]
                         [(assoc absolute-allergens (first filtered-candidates) allergen) possible-allergens])))
                   [{} []])
           )]
  (print-table [:allergen :ingredient] 
               (sort-by :allergen 
                        (concat 
                         (map (fn [x] {:allergen (:allergen x) :ingredient (seq (:possible-ingredients x))}) possible-allergens)
                         (map (fn [[k v]] {:allergen v :ingredient k}) absolute-allergens)))))

cxsvdm,glf,rsbxb,xbnmzr,txdmlzd,vlblq,mtnh,mptbpz
