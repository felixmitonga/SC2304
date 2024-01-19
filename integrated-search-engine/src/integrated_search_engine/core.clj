(ns integrated-search-engine.core
  (:gen-class))

(def flight-data "Krakov,Warsaw,100,
  Hamburg,Berlin,100,
  Warsaw,Berlin,300,
  Prague,Berlin,200,
  Munich,Berlin,100,
  Munich,Innsbruck,100,
  Vienna,Innsbruck,200,
  Vienna,Budapest,300,
  Warsaw,Budapest,400,
  Zagreb,Budapest,200,
  Vienna,Rome,400,
  Napoli,Rome,200,
  Napoli,Rijeka,100,
  Vienna,Prague,200,
  Vienna,Rijeka,400,
  Rijeka,Zagreb,100,
  Vienna,Zagreb,300,
  Munich,Zagreb,400,
  Innsbruck,Rome,400,
  Budapest,Rome,400,
  Budapest,Berlin,300,
  Prague,Brno,100,
  Prague,Budapest,300")

(defn create-flight-list [flight-data]
  (reduce (fn [flight-list flight]
            (let [parts (clojure.string/split flight #",")]
              (if (>= (count parts) 3)
                (let [departure (first parts)
                      destination (second parts)
                      cost (Integer/parseInt (nth parts 2))]
                  (conj flight-list
                        [departure destination cost]
                        [destination departure cost]))
                flight-list)))
          []
          (clojure.string/split-lines flight-data)))

(defn create-graph [flight-list]
  (reduce (fn [graph [departure destination cost]]
            (update graph departure conj [destination cost]))
          {}
          flight-list))

(defn find-routes [graph start end max-stops max-cost]
  (let [initial-path [{:city start :cost 0}]
        initial-queue [[:initial 0 0]]]
    (loop [queue initial-queue
           routes []]
      (if (empty? queue)
        routes
        (let [[[path-key total-cost stops] & rest-queue] queue
              path (if (= path-key :initial) initial-path path-key)
              current-city (:city (last path))]
          (if (= current-city end)
            (recur rest-queue (conj routes [path total-cost]))
            (let [next-steps (filter (fn [[next-city next-cost]]
                                       (and (<= (+ total-cost next-cost) max-cost)
                                            (not (some #(= next-city (:city %)) path))))
                                     (graph current-city))]
              (recur (reduce (fn [new-queue [next-city next-cost]]
                               (let [new-path (conj path {:city next-city :cost next-cost})
                                     new-total-cost (+ total-cost next-cost)
                                     new-stops (if (= next-city start) 0 (inc stops))]
                                 (conj new-queue [new-path new-total-cost new-stops])))
                             rest-queue
                             next-steps)
                     routes)))))))

(defn prepare-travel-plan [graph departure destination people]
  (let [budget-per-person 1000  ;; Assuming a default budget per person
        max-stops 3  ;; Maximum stops for the travel plan
        routes (find-routes graph departure destination max-stops (* budget-per-person (count people)))]
    (if (empty? routes)
      ["No routes found."]
      (let [sorted-routes (sort-by second routes)]
        (map-indexed (fn [idx [route total-cost]]
                       (str "Option " (inc idx) ":\t "
                            (clojure.string/join " - " (map #(str (:city %) " (" (:cost %) ")") route))
                            " \n total price per person: " (/ total-cost (count people))
                            "\n flights: " (dec (count route))))
                     sorted-routes)))))

(defn main []
  (println "Welcome to your flight search engine !!")
  (let [flight-list (create-flight-list flight-data)
        graph (create-graph flight-list)]
    (println "\nEnter the travel details:")
    (println "Departure city: ")
    (let [departure-city (read-line)]
      (println "Destination city: ")
      (let [destination-city (read-line)]
        (println "Enter customer details (name and year of birth, one per line, type 'done' when finished): ")
        (let [customer-details (loop [details []]
                                 (let [input (read-line)]
                                   (if (= input "done")
                                     details
                                     (recur (conj details (map #(clojure.string/split % #",") (list input)))))))]
          (let [people (map #(list (nth % 0) (Integer/parseInt (nth % 1))) customer-details)]
            (let [travel-plan (prepare-travel-plan graph departure-city destination-city people)]
              (println "\nTravel Plan:")
              (doseq [plan travel-plan]
                (println plan))))))))))

(main)
