(ns overtone-experiments.misc
  (:require
   [clojure.string      :as str]
   [overtone.live       :as o]
   [overtone.inst.piano :as p]
   [overtone.inst.synth :as s]
   [overtone.inst.drum  :as drum]))

(def INSTRUMENT p/piano)
(def TEMPO 120)
(def DECAY 0.25)
(def cur-time (atom nil))

(def hz
  {:C  24
   :C# 25
   :Db 25
   :D  26
   :D# 27
   :Eb 27
   :E  28
   :Fb 28
   :F  29
   :F# 30
   :Gb 30
   :G  31
   :G# 32
   :Ab 32
   :A  33
   :A# 34
   :Bb 34
   :B  35
   :Cb 35
   :R  0})

(defn get-octave
  "Returns octave multiplier"
  [n]
  (* 12 n))

(defn get-duration
  "Returns duration mulitplier"
  [n]
  (let [base (int ( * 1000 (/ 60 TEMPO)))]
  (int (* base (/ 4 n)))))

(defn key-signature? [a]
  (= 2 (count (-> a name (str/split #"-")))))

(defn make-note
  "Convers a note representation (i.e. :C34) into a map of playable data"
  [n acc]
  (let [note  (name n)
        name  (-> note (str/split #"\d") first keyword)
        nums  (last (str/split note #"\D"))
        o     (-> nums first str Integer/parseInt)
        r     (->> nums rest (apply str) Integer/parseInt)
        oct   (get-octave o)
        dur   (get-duration r)
        pitch (+ oct (name hz))
        time  (if (and @cur-time (not (empty? acc))) (+ (:duration (last acc)) @cur-time) (o/now))]
    {:name     name
     :rhythm   r
     :range    o
     :duration dur
     :octave   oct
     :time     time
     :pitch    pitch}))

(defn make-note2
  "Convers a note representation (i.e. :C34) into a map of playable data"
  [n acc]
  (let [note  (str n)
        name  (-> note (str/split #"\d") first keyword)
        nums  (last (str/split note #"\D"))
        o     (-> nums first str Integer/parseInt)
        r     (->> nums rest (apply str) Integer/parseInt)
        oct   (get-octave o)
        dur   (get-duration r)
        pitch (+ oct (name hz))
        time  (if (and @cur-time (not (empty? acc))) (+ (:duration (last acc)) @cur-time) (o/now))]
    {:name     name
     :rhythm   r
     :range    o
     :duration dur
     :octave   oct
     :time     time
     :pitch    pitch}))


(defn read-score
  "takes in a 'score' which is an array with a key-signature (or general config) as the first eleent and a series of
  keyword notes which contain note name, octave, and duration.  Transforms these into pitch and time info to be
  passed into 'play-score fn"
  [score]
  (let [time-sig (first score)
        notes    (filterv #(not (contains? #{:4-4 :3-4} %)) score)
        inst     p/piano]
    (reset! cur-time nil)
    (reduce
     (fn [acc n]
       (case n
         (| ||) acc
         (let [note (make-note2 n acc)]
           (reset! cur-time (:time note))
           (conj acc
                 (merge note 
                        {:time-signature time-sig
                         :tempo          TEMPO})))))
     []
     notes)))

(defn read-score2
  "takes in a 'score' which is an array with a key-signature (or general config) as the first eleent and a series of
  keyword notes which contain note name, octave, and duration.  Transforms these into pitch and time info to be
  passed into 'play-score fn"
  [score]
  (let [time-sig (first score)
        notes    (filterv #(not (contains? #{4/4 3/4} %)) score)
        inst     p/piano]
    (reset! cur-time nil)
    (reduce
     (fn [acc n]
       (case n
         (:| :||) acc
         (let [note (make-note n acc)]
           (reset! cur-time (:time note))
           (conj acc
                 (merge note 
                        {:time-signature time-sig
                         :tempo          TEMPO})))))
     []
     notes)))

(defn play-notes
  "Takes in a vector of maps with time/pitch/instrument instructions and plays them using the 'at macro"
  [notes]
  (reduce
   (fn [acc n]
     (conj acc (o/at (:time n) (INSTRUMENT (:pitch n) :decay DECAY))))
   []
   (if (map? (first notes))
     notes
     (read-score2 notes))))

(defn transpose
  "Adjusts the octave data of each note up or down n octaves"
  [score octaves dir]
  (reduce (fn [acc n]
            (if (and (<= 3 (count (name n)))
                     (not (contains? #{:| :||} n)))
               (let [note     (make-note n [])
                     sn       (name n)
                     txp      (max ((if (= :up dir) + -) (:range note) octaves) 0)
                     new-note (keyword (str (name (:name note)) txp (:rhythm note)))]
                 (conj acc new-note))
               (conj acc n)))
          (conj [] (first score))
          (drop 1 score)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Songs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bassy
  '(4/4 A02 B02 C12 B02))

(def chordy1
  [:4-4 :A31 :G31 :F31 :E31])

(def chordy2
  [:4-4 :E21 :F21 :E21 :D21])

(def chordy3
  [:4-4 :C21 :D21 :A21 :B21])

(do
(play-notes chordy1)
(play-notes chordy2)
(play-notes chordy3)
(play-notes (flatten (repeat 2 bassy)))
)

(play-notes bassy)

(def score2
  [:4-4 :E38 :D#38 :E38 :D#38 :E38 :B28 :D38 :C38 :A28 :||])

(def score3
  [:4-4
   :A116 :C216 :B216 :A116 :|
   :A116 :C216 :B216 :A116 :|
   :A116 :C216 :B216 :A116 :|
   :A116 :C216 :B216 :A116 :|
   ])

(def score
  [:4-4 :C34 :D34 :E34 :D34 :| :E38 :D38 :C38 :B28 :A24 :B24 :||])
  ;; [:4-4 :C34 :D34 :E34 :D34 :| :C38 :C34 :C38 :C34 :||])
(comment
  (read-score score)

  (s/mooger 32)

  (play-notes
    (concat
     score3
     (transpose score3 1 :up)
     (transpose score3 2 :up)
     (transpose score3 1 :up)
     score3
     (transpose score3 1 :down)
     (transpose score3 2 :down)
     ))

  (map #(p/piano %) (range 25 49 12))

  (for [i (range 9 21)] (at (+ (o/now) (* i (rand 2) 50)) (p/piano i)))

  (p/piano 23  0.01)

  (o/stop-all)

  (drum/kick2)
  (drum/snare)
  (drum/open-hat)
  (drum/closed-hat2)

  (defn my-beat []
    (let [time (o/now)]
      (o/at    time       (drum/kick2))
      (o/at (+ time 1000) (drum/snare))
      (o/at (+ time 2000) (drum/kick2))
      (o/at (+ time 2750) (drum/kick2))
      (o/at (+ time 3000) (drum/snare))))

  (defn my-hats []
    (let [time  (o/now)
          times (range 0 8000 125)]
      (mapv (fn [t] (if (= 0 (mod t 1000))
                      (o/at (+ time t) (drum/hat3)) 
                      (o/at (+ time t) (drum/closed-hat2)))) times)))

  (o/at
   (do
     [
      (my-hats)
      (my-beat)
      ]
     (my-hats)))

  (pmap eval
        [my-hats
         my-hats
         my-hats
         my-hats])

  (o/stop-all)

  (def minuet-in-c-bass
    [:4-4
     :C14 :C14 :C14 :C14     :|
     :G#04 :G#04 :G#04 :G#04 :|
     :F04 :F04 :F04 :F04     :|
     :G04 :G04 :G04 :G04     :||])

  (def minuet-in-c-melody
    [:4-4
     :R04 :C52 :R08 :B416 :C516                 :|
     :D58 :C58 :Bb48 :Ab48 :F#48 :G48 :Ab48     :|
     :R04 :G58 :F58 :Eb58 :D58 :C58 :B48 :Ab48  :|
     :C68 :Bb58 :Ab58 :G58 :F58 :Eb58 :D58 :C58 :||])

  [(play-notes (flatten (repeat 2 minuet-in-c-bass)))
   (play-notes (flatten (repeat 2 minuet-in-c-melody)))]

  (play-notes '[C12])

  :end)
