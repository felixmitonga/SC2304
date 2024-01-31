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
            (let [parts (clojure.string/split flight #",")]  ;splits each flight data using a comma as the separator
              (if (>= (count parts) 3)
                (let [departure (first parts)  ;separates the information of the cities
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

(def graph (create-graph (create-flight-list flight-data)))

; function that uses breadth-first search to find the route from destin. to depart.
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


; creating the travel plan for clients based on their input
(defn search_function1 [graph departure destination customers]
  (let [budget-per-passenger (if (= (first customers) "f") 700 1000) ; budget for families and groups
        max-stops (if (= (first customers) "f") 2 3)  ; maximum stops for families and groups
        routes (find-routes graph departure destination max-stops (* budget-per-passenger (count customers)))]
    (if (empty? routes)
      "No routes found."
      (let [sorted-routes (sort-by second routes)
            cheapest-route (first sorted-routes)] ;; Assuming the first route is the cheapest, adjust as needed
        (str "Offer:\n"
             "Travel Plan:\n"
             (clojure.string/join " - " (map #(str (:city %) " (" (:cost %) ")") (first cheapest-route)))
             "\nTotal price per person: " (/ (second cheapest-route) (count customers))
             "\nNumber of flights: " (dec (count (first cheapest-route))))))))


(defn search_function [graph departure destination customers]
  (let [budget-per-passenger (if (= (first customers) "f") 700 1000) ; budget for families and groups
        max-stops (if (= (first customers) "f") 2 3)  ; maximum stops for families and groups
        routes (find-routes graph departure destination max-stops (* budget-per-passenger (count customers)))]
    (if (empty? routes)
      "No routes found."
      (let [sorted-routes (sort-by second routes)
            cheapest-route (first sorted-routes)] ;; Assuming the first route is the cheapest, to be adjusted as needed
        (second cheapest-route)
        ))))

(defn prepare_travel_plan [departure destination people]
  (let [customers (map (fn [[name yob]] {:name name :yob yob}) people)]
    (search_function graph departure destination customers)))

(defn -main []
  (let [departure "Munich"
        destination "Rijeka"
        ;; TESTING OKAY then it will read the sales file from BROKER with data same format, command to load sales data below this line
        people [["John Smith" 1980] ["Mary Smith" 1983] ["Sindy Smith" 2010]]]


    (println "Searching for travel plan...")
    (println "Departure: " departure)
    (println "Destination: " destination)
    (println "Customers: " people)

    (let [result (prepare_travel_plan departure destination people)]
      (if (string? result)
        (println "Result: " result)
        (println "Total cost for the entire group: $" result)))))
