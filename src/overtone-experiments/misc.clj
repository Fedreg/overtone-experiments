(ns overtone-experiments.misc
  (:require
   [clojure.string      :as str]
   [overtone.live       :as o]
   [overtone.inst.synth :as syn]
   [overtone.inst.piano :as p]
   [overtone.inst.drum  :as drum]))

(def hz
  {:C  24
   :C# 25
   :D  26
   :D# 27
   :E  28
   :F  29
   :F# 30
   :G  31
   :G# 32
   :A  33
   :A# 34
   :B  35})


(defn get-octave
  "Returns octave multiplier"
  [n]
  (* 12 n))

(defn get-duration
  "Returns duration mulitplier"
  [n]
  (int (* 1000 (/ 4 n))))


(def score2
  [:4-4 :E38 :D#38 :E38 :D#38 :E38 :B28 :D38 :C38 :A28 :||])

(def score
  [:4-4 :C34 :D34 :E34 :D34 :| :E38 :D38 :C38 :B28 :A24 :B24 :||])
  ;; [:4-4 :C34 :D34 :E34 :D34 :| :C38 :C34 :C38 :C34 :||])

(def cur-time (atom nil))

(defn read-score
  "takes in a 'score' which is an array with a key-signature (or general config) as the first eleent and a series of
  keyword notes which contain note name, octave, and duration.  Transforms these into pitch and time info to be
  passed into 'play-score fn"
  [score]
  (let [time-sig (first score)
        notes    (drop 1 score)
        tempo    60
        beatspm  4
        inst     p/piano]
    (reset! cur-time nil)
    (reduce
     (fn [acc n]
       (case n
         (:| :||) acc
         (let [note  (name n)
               name  (-> note (str/split #"\d") first keyword)
               nums  (last (str/split note #"\D"))
               oct   (-> nums first str Integer/parseInt get-octave)
               dur   (-> nums last str Integer/parseInt get-duration)
               pitch (+ oct (name hz))
               time  (if @cur-time (+ (:duration (last acc)) @cur-time) (o/now))]
           (reset! cur-time time)
           (conj acc
                 {:time-signature time-sig
                  :tempo          tempo
                  :duration       dur
                  :time           time
                  :instrument     inst
                  :pitch          pitch}))))
     []
     notes)))

(defn play-notes
  "Takes in a vector of maps with time/pitch/instrument instructions and plays them using the 'at macro"
  [notes]
  (reduce
   (fn [acc n]
     (conj acc (o/at (:time n) ((:instrument n) (:pitch n)))))
   []
   notes))

(comment
  (read-score score)

 (play-notes (read-score score2))

  (map #(p/piano %) (range 25 49 12))

  (for [i (range 9 21)] (at (+ (o/now) (* i (rand 2) 50)) (p/piano i)))

  (p/piano 6)

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
    (let [time (o/now)]
      (o/at    time       (drum/closed-hat2))
      (o/at (+ time 250)  (drum/closed-hat2))
      (o/at (+ time 500)  (drum/closed-hat2))
      (o/at (+ time 750)  (drum/closed-hat2))
      (o/at (+ time 1000) (drum/closed-hat2))
      (o/at (+ time 1250) (drum/closed-hat2))
      (o/at (+ time 1500) (drum/closed-hat2))
      (o/at (+ time 1750)  (drum/closed-hat2))
      #_(o/at (+ time 2000)  (drum/closed-hat2))))

  (o/at
   (do
     [
      (my-hats)

      (my-beat)
      ]
     (my-hats)))

  :end)
