(ns data-analysis-tool.core
(:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pprint])
  (:gen-class))



;; Load sales data from the CSV file
(def sales-data-file "/SC2304/data-analysis-tool/sales_team_6.csv"
(def sales-data (map #(zipmap [:name :year-of-birth :departure-city :destination-city :amount] (str/split % #",")) (rest (str/split-lines (slurp sales-data-file)))))

;; 1. Total Revenue by Destination City
(defn total-revenue-by-destination [data]
  (->> data
       (group-by :destination-city)
       (map (fn [[destination transactions]]
              [destination (->> transactions
                                  (map :amount)
                                  (map #(Integer. %))
                                  (reduce +))]))))

;; 2. Average Amount Spent by Age Group
(defn average-amount-by-age-group [data]
  (->> data
       (group-by (fn [record]
                   (let [age-group (if (< (Integer. (:year-of-birth record)) 25) "18-24" "25+")]
                     age-group)))
       (map (fn [[age-group transactions]]
              [age-group (->> transactions
                                (map :amount)
                                (map #(Integer. %))
                                (reduce +)
                                (float)
                                (/ (count transactions)))]))))

;; 3. Most Popular Departure and Destination Pair
(defn most-popular-pair [data]
  (->> data
       (group-by (fn [record]
                   (str (:departure-city record) " -> " (:destination-city record))))
       (map (fn [[pair transactions]]
              [pair (count transactions)]))
       (sort-by second)
       (reverse)
       (first)))

;; 4. Revenue Distribution by Age
(defn revenue-distribution-by-age [data]
  (->> data
       (group-by (fn [record]
                   (let [age-group (if (< (Integer. (:year-of-birth record)) 25) "18-24" "25+")]
                     age-group)))
       (map (fn [[age-group transactions]]
              [age-group (->> transactions
                                (map :amount)
                                (map #(Integer. %))
                                (reduce +))]))))

;; 5. Top Spenders
(defn top-spenders [data n]
  (->> data
       (sort-by :amount)
       (reverse)
       (take n)))

;; Reports from the analysis
(pprint/print-table (total-revenue-by-destination sales-data))
(pprint/print-table (average-amount-by-age-group sales-data))
(println "Most Popular Pair:" (most-popular-pair sales-data))
(pprint/print-table (revenue-distribution-by-age sales-data))
(pprint/print-table (top-spenders sales-data 5))

