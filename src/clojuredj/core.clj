(ns clojuredj.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.test :refer :all]
            [clansi :refer :all]))
;; EXECUTE THE NAMESPACE FIRST.


;; http://snipplr.com/view/22183/dijkstras-algorithm-in-clojure/

;; EXECUTE THIS 'DO' LINE TO BUILD REQUIRED FUNCTIONS
(do
  (defrecord Parcel [weight location])
  ;;(defrecord Parcel [weight location collection])
  (defrecord Robot [loc parcels parcelLimit weightLimit route])
  (def rbot (Robot. :mainoffice (list) 3 20 nil))

  (defn take-small [layout items]
    (let [ [dist minnodes] (first layout)
    [small pred] (first minnodes)
    others (rest minnodes)]
    [small dist
    (if (empty? others)
      (dissoc layout dist)
      (assoc layout dist others))
    (assoc items small [pred dist])]))

  (defn add-rdist ([layout tile pred dist]
      (if-let [nodes (layout dist)]
        (assoc layout dist (assoc nodes tile pred))
        (assoc layout dist {tile pred})))
      ([layout tile pred dist prevdist]
        (let [neardests (add-rdist layout tile pred dist)
          minnodes (layout prevdist)
          nminnodes (dissoc minnodes tile)]
        (if (empty? nminnodes)
          (dissoc neardests prevdist)
          (assoc neardests prevdist nminnodes)))))

  (defn sections [layout items net tile dist children distance]
    (reduce
      (fn [acc x]
        (let [curdist (+ dist (distance net tile x))
              prevdist (second (items x))
              neardests (first acc)
              npreds (second acc)]
          (if (nil? prevdist)
            [(add-rdist neardests x tile curdist) (assoc npreds x [tile curdist])]
            (if (< curdist prevdist)
              [(add-rdist neardests x tile curdist prevdist)
               (assoc npreds x [tile curdist])]
              [neardests npreds]))))
      [layout items]
      (children net tile)))


  (defn dijkstra [net root dest children distance]
    (loop [layout (sorted-map 0 {root root})
      small root items {root [root 0]} dist 0]
      (if (empty? layout)
        items
      (let [[nminnode ndist neardests npreds] (take-small layout items)
        [nnrdists nnpreds]
        (sections neardests npreds net nminnode ndist children distance)]
          (recur nnrdists nminnode nnpreds ndist)))))

  (defn build-office-path [items root dest]
    (loop [[pred dist] (items dest) path (list dest)]
    (if (nil? pred)
      nil
    (if (= pred root)
      (cons root path)
      (recur (items pred) (cons pred path))
    ))))

  (defn calculatepath
    ([net root dest children distance]
      (let [items (dijkstra net root dest children distance)
      path (build-office-path items root dest)]
      (if (nil? path)
        nil
        (do
          ;;(println (list path))
        (list path ))))))

   (defn calculatedistance
     ([net root dest children distance]
      (let [items (dijkstra net root dest children distance)
 	      path (build-office-path items root dest)]
      (if (nil? path)
   	    nil
        (do
          ;;(println (second (items dest)))
   	    (second (items dest))
        ))))
        ([net root dest children distance x]
         (let [items (dijkstra net root dest children distance)
    	      path (build-office-path items root dest)]
         (if (nil? path)
      	    nil
           (do
      	    (second (items dest))
           )))))

  (defn children [office tile]
   (keys (office tile)))

  (defn distance [office nodesrc dest]
   ((office nodesrc) dest))
(defn ResetOffice[]
  (def office {
    :mainoffice {:mail 2},
    :mail {:ts 1 :o135 2 :mainoffice 2},
    :ts {:mail 1 :a2 2 :o101 1},
    :a1 {:e2 1 :a3 1 :b1 1 :d3 2},
    :a2 {:ts 2 :e2 1 :a3 1},
    :a3 {:a2 1 :a1 1 :o101 2},
    :c1 {:o123 2 :c2 1 :e3 1},
    :c2 {:b1 2 :c1 1 :c3 1},
    :c3 {:b2 2 :c2 1 :e3 1},
    :b1 {:a1 1 :b2 1 :b3 1, :c2 2},
    :b2 {:b1 1 :b4 1 :c3 2},
    :b3 {:b1 1 :b4 1 :o103 2},
    :b4 {:b3 1 :b2 1 :o107 2},
    :d1 {:o129 2 :d2 1 :e1 1},
    :d2 {:o125 2 :d1 1 :d3 1},
    :d3 {:a1 2 :d2 1 :e1 1},
    :e1 {:d1 1 :d3 1},
    :e2 {:a2 1 :a1 1},
    :e3 {:c1 1 :c3 1},
    :o101 {:ts 1 :a3 2 :r101 2 :o103 1},
    :o103 {:o101 1 :b3 2 :r103 2 :o105 1},
    :o105 {:o103 1 :b3 2 :r105 2 :o107 1},
    :o107 {:o105 1 :b4 2 :r107 2 :o109 1},
    :o109 {:o107 1 :r109 2 :o111 1 :o113 1},
    :o111 {:r111 2 :o109 1},
    :o113 {:r113 2 :o109 1 :o115 1},
    :o115 {:r115 2 :o113 1 :o117 1},
    :o117 {:r117 2 :o115 1 :o119 1},
    :o119 {:r119 2 :o117 1 :o121 1 :storage 2},
    :o121 {:r121 2 :o119 1 :o123 1},
    :o123 {:r123 2 :o121 1 :o125 1 :c1 2},
    :o125 {:r125 2 :o123 1 :o127 1 :d2 2},
    :o127 {:r127 2 :o125 1 :o129 1},
    :o129 {:r129 2 :o127 1 :o131 1 :d1 2},
    :o131 {:r131 2 :o129 1 :o133 2},
    :o133 {:o131 2 :o135 2},
    :o135 {:o133 2 :mail 1},
    :r101 {:o101 2},
    :r103 {:o103 2},
    :r105 {:o105 2},
    :r107 {:o107 2},
    :r109 {:o109 2},
    :r111 {:o111 2},
    :r113 {:o113 2},
    :r115 {:o115 2},
    :r117 {:o117 2},
    :r119 {:o119 2},
    :r121 {:o121 2},
    :r123 {:o123 2},
    :r125 {:o125 2},
    :r127 {:o127 2},
    :r129 {:o129 2},
    :r131 {:o131 2},
    :storage {:o119 2},
    })
)

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn map-values [m keys f & args]
  (reduce #(apply update-in %1 [%2] f args) m keys))

(defn UpdateOffice[f amount]
  (def office (into {} (for [item office]
    (hash-map (first item) (map-values (last item) (into [] (keys (last item))) f amount))
  )))
)


  (defn calculateRotationRoute [targets]
    (if (> (count targets) 1)
      (do
        (conj (calculateRotationRoute (rest targets)) (first (calculatepath office (first targets) (first (rest targets)) children distance)))
      )
    )
  )

  (defn WorkoutRoutes [start stops]
    (do
      (for [rotation (combo/permutations stops)]
        (do
          (ResetOffice)
          (flatten (calculateRotationRoute (conj rotation start)))
        )
      )
    )
  )

  (defn FindOrderOfParcels [path]
    (sort-by first (for [x (into [] (-> rbot :parcels))]
      (array-map :index (.indexOf path (:location x)) :location (-> x :location))
    ))
  )

(defn find-first
         [f coll]
         (first (filter f coll)))

(defn RobotWeight[]
  (reduce + (for [x (-> rbot :parcels)]
    (:weight x)
  ))
)

  (defn calculateJourneyCost ([path last]
    (if (> (count path) 0)
      (do
        (conj (calculateJourneyCost (rest path) (first path)) (calculatedistance office last (first path) children distance))
      )
    ))
    ([path last parcels]
      (if (> (count path) 0)
        (do

          ;;(println (first path) (:location (first parcels)))
          (if (= (first path) (:location (first parcels)))
            (do
              ;;(println "true")
              ;;(println (calculatedistance office last (first path) children distance))
              (conj (calculateJourneyCost (rest path) (first path) (rest parcels)) (calculatedistance office last (first path) children distance))
            )
            (do
              (ResetOffice)
              (UpdateOffice + (:weight (find-first #(= (:location %) (:location (first parcels))) (-> rbot :parcels))))
              ;;(println "false")
              ;;(println (calculatedistance office last (first path) children distance))
              (conj (calculateJourneyCost (rest path) (first path) parcels) (calculatedistance office last (first path) children distance))
            )
          )
        )
      )
    )
  )

  (defn CalculateJourneyPath
    ([start stops collection end]
      (flatten (conj (calculatepath office collection end children distance ) (calculatepath office (last (first (sort-by last (WorkoutRoutes start stops)))) collection children distance)  (first (sort-by last (WorkoutRoutes start stops)))))
    )
    ([start stops end]
      (flatten (conj (calculatepath office (last (first (sort-by last (WorkoutRoutes start stops)))) end children distance) (first (sort-by last (WorkoutRoutes start stops)))))
    )
    ([start stops]
        (first (sort-by last (WorkoutRoutes start stops)))
    )
  )

  (defn FormatJourney
    ([start stops collection end]
      (list (CalculateJourneyPath start stops collection end) (reduce + (calculateJourneyCost (CalculateJourneyPath start stops collection end) start)))
    )
    ([start stops end]
      (list (CalculateJourneyPath start stops end) (reduce + (calculateJourneyCost (CalculateJourneyPath start stops end) start)))
    )
    ([start stops]
      (list (CalculateJourneyPath start stops) (reduce + (calculateJourneyCost (CalculateJourneyPath start stops) start)))
    )
  )

  (defn newCalculateJourneyCost [path]
    (if (> (count path) 0)
      (do
        ;;(ResetOffice)
        ;;(println path)
        (calculateJourneyCost (rest path) (first path) (FindOrderOfParcels path))
      )
    )
  )

  (defn CalculatePermutationLengths []
    (for [rot (WorkoutRoutes (-> rbot :loc) (for [x (-> rbot :parcels)] (-> x :location)))]
      (do
        ;;(println (array-map :route rot :cost (reduce + (newCalculateJourneyCost rot))))
        (UpdateOffice + (RobotWeight))
        (array-map :route rot :cost (reduce + (newCalculateJourneyCost rot)))
      )
    )
  )

  (defn CalculateFastestRoute []
    (first (sort-by :cost (CalculatePermutationLengths)))
  )

  (defn ShowAllRoutes []
    (sort-by :cost (CalculatePermutationLengths))
  )

  (defn PrintAllRoutes[]
    (println (style (first (sort-by :cost (CalculatePermutationLengths))) :green))
    (for [x (rest (sort-by :cost (CalculatePermutationLengths)))]
      (println (style x :red))
    )
  )

  (defn MoveToLocation [x]
    (let [result (FormatJourney (-> rbot :loc) (list x))]
    (def rbot (assoc rbot :loc x))
    result)
  )

  (defn AddRoute [route]
    ;;(println route)
    (def rbot (assoc rbot :route (second (first route))))
  )

  (defn AddParcel[parcel]
    (if (< (count (-> rbot :parcels)) (-> rbot :parcelLimit))
      (do
        (if (> (reduce + (for [x (conj (-> rbot :parcels) parcel)] (-> x :weight))) (-> rbot :weightLimit))
          "Unable to add parcel. Robot weight limit reached."
          (do
            (if (= nil (in? (-> rbot :parcels) parcel))
              (do
                (def rbot (assoc rbot :parcels  (conj (:parcels rbot) parcel)))
                (AddRoute (CalculateFastestRoute))
              )
              (def rbot (assoc rbot :parcels  (conj (:parcels rbot) parcel)))
            )
          )
        )
      )
      "Unable to add parcel. Robot carry limit reached."
    )
  )

  (defn RemoveRouteStep [target]
    (if (not= (:location target) (first (-> rbot :route)))
      (do
        (def rbot (assoc rbot :route (rest (-> rbot :route))))
        (RemoveRouteStep target)
      )
      (do
        (if (= (first (-> rbot :route)) (first (-> rbot :route)))
          (def rbot (assoc rbot :route (rest (-> rbot :route))))
        )
      )
    )
  )

  (defn findFirstParcelInRoute []
    (sort-by first (for [x (into [] (-> rbot :parcels))]
      (array-map :index (.indexOf (-> rbot :route) (:location x)) :location (-> x :location))
    ))
  )

  (defn MoveToNextParcel[]
    (do
      (def rbot (assoc rbot :loc (:location (first (findFirstParcelInRoute)))))
      (RemoveRouteStep (first (findFirstParcelInRoute)))
      (def rbot (assoc rbot :parcels (into () (remove (fn[x] (= (:location x) (:location (first (findFirstParcelInRoute))))) (-> rbot :parcels)))))
    )
  )

  (defn DisplayJourney[]
    (-> rbot :route)
  )

;; EXECUTE THIS LINE TO BUILD REQUIRED FUNCTIONS
)

;; Robot has parcels.
;; Parcels have drop points
;; Robot has collection points
;; Robot has end destination

(def parcel (Parcel. 5 :r101))
(def parcel2 (Parcel. 3 :r105))
(def parcel3 (Parcel. 10 :storage))

;; 1. Collect a parcel from the main office and deliver it to R131.
(FormatJourney :mainoffice (list :r131))
;;(def rbot (Robot. :mainoffice (list) 3 20 nil))
;;(def parcelone (Parcel. 5 :r131))
;;(AddParcel parcelone)
;;(DisplayJourney)


;;2. Collect a parcel from the main office and deliver it to R119.
(FormatJourney :mainoffice (list :r119))
;;(def rbot (Robot. :mainoffice (list) 3 20 nil))
;;(def parcelone (Parcel. 5 :r119))
;;(AddParcel parcelone)
;;(DisplayJourney)

;;3. Collect a parcel from R113 and deliver it to room R115.
(FormatJourney :mainoffice (list :r113) :r115)

;; THIS COMMENTED CODE CAN ALSO BE DONE....IT IS MORE DYNAMIC AND USES A ROBOT OBJECT WITH PARCEL
;;(def rbot (Robot. :mainoffice (list) 3 20 nil))
;;(def parcelone (Parcel. 5 :r115))
;;(MoveToLocation :r113)
;;(AddParcel parcelone)
;;(DisplayJourney)


;;4. Collect a parcel from R113 and deliver it to room R129.
(FormatJourney :mainoffice (list :r113) :r129)


;; THIS COMMENTED CODE CAN ALSO BE DONE....IT IS MORE DYNAMIC AND USES A ROBOT OBJECT WITH PARCEL
;;(def rbot (Robot. :mainoffice (list) 3 20 nil))
;;(def parcelone (Parcel. 5 :r129))
;;(MoveToLocation :r113)
;;(AddParcel parcelone)
;;(DisplayJourney)


;;5. Collect a parcel from the main office and deliver it to R131. Collect another parcel from
;;R131 and deliver it to the main office. This should be planned as a single journey.
(FormatJourney :mainoffice (list :r131) :mainoffice)


;;6. Collect two parcels from the main office. Deliver one to R131 and the other to R111. This
;;should be planned as a single journey.
(FormatJourney :mainoffice (list :r131 :r111) :mainoffice)

;; THIS COMMENTED CODE CAN ALSO BE DONE....IT IS MORE DYNAMIC AND USES A ROBOT OBJECT WITH PARCEL
;;(def rbot (Robot. :mainoffice (list) 3 20 nil))
;;(def parcelone (Parcel. 5 :r131))
;;(def parceltwo (Parcel. 5 :r111))
;;(AddParcel parcelone)
;;(AddParcel parceltwo)
;;(DisplayJourney)

;;7. Collect two parcels from the main office. Deliver one to R131 and the other to R111.
;;Collect another parcel from R121 and deliver it to the main office. This should be planned as a single journey.
(FormatJourney :mainoffice (list :r131 :r111) :r121 :mainoffice)

;; THIS COMMENTED CODE CAN ALSO BE DONE....IT IS MORE DYNAMIC AND USES A ROBOT OBJECT WITH PARCEL
;;(def rbot (Robot. :mainoffice (list) 3 20 nil))
;;(def parcelone (Parcel. 5 :r131))
;;(def parceltwo (Parcel. 5 :r111))
;;(AddParcel parcelone)
;;(AddParcel parceltwo)
;;(DisplayJourney)
;;(MoveToNextParcel)
;;(MoveToNextParcel)
;;(MoveToLocation :r121)
;;(def parcelthree (Parcel. 5 :mainoffice))
;;(AddParcel parcelthree)
;;(DisplayJourney)



;;8. Collect two parcels from the main office. Deliver one to R131 and the other to R111.
;;Collect another parcel from R121 and deliver it to the main office. This should be
;;planned as multiple journeys with the route recalculated for the second leg once the
;;first leg has been completed.
;;(FormatJourney :mainoffice (list :r131 :r111) :r121 :mainoffice)

;; ALTER THE PARCEL WEIGHTS AND YOU WILL NOTICE THE ROUTE WILL CHANGE DEPENDING ON THE WEIGHT OF PARCELS.
(-> rbot)
(def rbot (Robot. :mainoffice (list) 3 20 nil))
(def parcelone (Parcel. 10 :r131))
(def parceltwo (Parcel. 1 :r111))
(AddParcel parcelone)
(AddParcel parceltwo)
(DisplayJourney)
(MoveToNextParcel)
(MoveToNextParcel)
(MoveToLocation :r121)
(def parcelthree (Parcel. 5 :mainoffice))
(AddParcel parcelthree)
(DisplayJourney)

;; THIS SHOWS ALL ROUTES - LOOK IN REPL.
(PrintAllRoutes)



;; -----------------------------------------------------------
;; TEST DRIVEN DEVELOPMENT

(do
  (deftest TestRouteCalculation
    (testing "Does route calculation work"
    (def rbot (Robot. :mainoffice (list) 3 20 nil))
    (def parcelone (Parcel. 1 :r131))
    (AddParcel parcelone)
    (is (= (list :mainoffice :mail :o135 :o133 :o131 :r131) (DisplayJourney)))))

  (deftest TestAddingParcel
    (testing "Does AddParcel function work - checks count and element exists in robot parcel list."
    (def rbot (Robot. :mainoffice (list) 3 20 nil))
    (def parcelone (Parcel. 1 :r131))
    (AddParcel parcelone)
    (is (and (= 1 (count (-> rbot :parcels))) (= true (in? (-> rbot :parcels) parcelone))))))

  (deftest TestMovingToNextParcel
    (testing "Does the parcel get removed and the location of robot updated")
    (def rbot (Robot. :mainoffice (list) 3 20 nil))
    (def parcelone (Parcel. 1 :r131))
    (AddParcel parcelone)
    (MoveToNextParcel)
    (is (and (empty? (-> rbot :parcels)) (empty? (-> rbot :route)) (= (-> rbot :loc) :r131))))

  (deftest TestMovingToLocation
    (testing "Does the robot move to the correct location via quickest route?")
    (def rbot (Robot. :mainoffice (list) 3 20 nil))
    (is (= (list (list :mainoffice :mail :o135 :o133 :o131 :o129 :o127 :o125 :o123 :o121) 13) (MoveToLocation :o121)))
  )

  (deftest TestPermutations
    (testing "Does a route calculation check the correct number of potential options")
    (def rbot (Robot. :mainoffice (list) 3 20 nil))
    (def parcelone (Parcel. 1 :r131))
    (def parceltwo (Parcel. 1 :storage))
    (def parcelthree (Parcel. 1 :d3))
    (AddParcel parcelone)
    (AddParcel parceltwo)
    (AddParcel parcelthree)
    (is (= (count (ShowAllRoutes)) (count (combo/permutations (-> rbot :parcels)))))
  )

  (deftest TestResettingOffice
    (testing "Does the office get correctly reset?")
    (ResetOffice)
    (is (= (:o123 office) {:r123 2 :o121 1 :o125 1 :c1 2}))
  )

  (deftest TestAlteringOffice
    (testing "Does the office get correctly altered?")
    (ResetOffice)
    (UpdateOffice + 5)
    (is (= (:o123 office) {:r123 7 :o121 6 :o125 6 :c1 7}))
  )

  (deftest TestShortestRoute
    (testing "Does a route calculation have the shortest route first?")
    (def rbot (Robot. :mainoffice (list) 3 20 nil))
    (def parcelone (Parcel. 1 :r131))
    (def parceltwo (Parcel. 1 :storage))
    (def parcelthree (Parcel. 1 :d3))
    (AddParcel parcelone)
    (AddParcel parceltwo)
    (AddParcel parcelthree)
    (is (= (:cost (first (ShowAllRoutes))) (apply min (for [x (ShowAllRoutes)] (:cost x)))))
  )
)
;; RUN THIS END BRACKET TO BUILD ALL TESTS. RUN TESTS BELOW


(-> rbot)
;; RETURNS NIL = PASS
;; IF FAIL, IT WILL SHOW IN REPL.
(TestRouteCalculation)
(TestAddingParcel)
(TestMovingToNextParcel)
(TestMovingToLocation)
(TestPermutations)
(TestResettingOffice)
(TestAlteringOffice)
(TestShortestRoute)
