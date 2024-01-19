(ns search-engine-tool.core
  (:gen-class))


;; data of flights defined as a string
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

(defn create-flight-list [flight-data] ; created the flight information from flight data
  (reduce (fn [flight-list flight]
            (let [parts (clojure.string/split flight #",")]  ;splits each flight data using a comma as the seperator
              (if (>= (count parts) 3)
                (let [departure (first parts)  ;seperates the information of the cities
                      destination (second parts) ;and prices for flights into parts
                      cost (Integer/parseInt (nth parts 2))]
                  (conj flight-list
                        [departure destination cost]     ;creates the return flights so there are more routes available for clients
                        [destination departure cost]))
                flight-list)))
          []
          (clojure.string/split-lines flight-data)))

(defn create-graph [flight-list]
  (reduce (fn [graph [departure destination cost]]
            (update graph departure conj [destination cost]))
          {}
          flight-list))


; function that uses breadh-first search to find the route from destin. to depart.
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
                     routes))))))))


;creating the travel plan for clients based on their input
(defn prepare-travel-plan [graph departure destination passenger-type]
  (let [budget (if (= passenger-type "f") 700 1000) ; budget for families and groups
        max-stops (if (= passenger-type "f") 2 3)  ; maximum stops for families and groups
        routes (find-routes graph departure destination max-stops budget)]
    (if (empty? routes)
      ["No routes found."]
      (let [sorted-routes (sort-by second routes)]
        (map-indexed (fn [idx [route total-cost]]
                       (str "Option " (inc idx) ":\t "
                            (clojure.string/join " - " (map #(str (:city %)  " (" (:cost %) ")") route))
                            " \n total price per person: " total-cost
                            "\n flights: " (dec (count route))))
                     sorted-routes)))))



;main function for user interaction
(defn main []
  (println "Welcome to your flight search engine !!")
  (loop []
    (println "Enter departure city: ")
    (let [departure-city (read-line)]
      (when-not (or (= departure-city "exit") (= departure-city "restart"))
        (println "Enter destination city: ")
        (let [destination-city (read-line)]
          (when-not (or (= destination-city "exit") (= destination-city "restart"))
            (println "Are you a family or a group? (Enter 'f' for family, 'g' for group): ")
            (let [passenger-type (clojure.string/lower-case (read-line))]
              (when-not (or (= passenger-type "exit") (= passenger-type "restart"))
                (let [flight-list (create-flight-list flight-data)
                      graph (create-graph flight-list)
                      travel-plan (prepare-travel-plan graph
                                                       (clojure.string/capitalize departure-city)
                                                       (clojure.string/capitalize destination-city)
                                                       passenger-type)]
                  (println "\nTravel Plan:")                ;; functionality where typing 'restart' restarts
                  ;; the program and typing 'exit' quits it
                  (doseq [plan travel-plan]                 ;; so the client can see other travel options easily
                    (println plan))
                  (println "Type 'restart' to perform another search or 'exit' to quit: ")
                  (let [command (clojure.string/lower-case (read-line))]
                    (cond
                      (= command "exit") (do (println "Exiting program.") (System/exit 0))
                      (= command "restart") (recur))))))))))))



(main)







