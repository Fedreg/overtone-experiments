(ns overtone-experiments.core
  (:use overtone.live
        overtone.inst.synth)
  (:require [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst tone
  "Basic synth"
  [note 60 dur 1.01 vol 0.3]
  (let [snd (square (midicps note))
        env (env-gen (perc 0.1 dur))]
    (* env snd vol)))

(def notes
  {:C  21
   :C# 22
   :Cb 20
   :D  23
   :D# 24
   :Db 24
   :E  25
   :E# 26
   :Eb 24
   :F  26
   :F# 27
   :Fb 25
   :G  28
   :G# 29
   :Gb 27
   :A  30
   :A# 31
   :Ab 29
   :B  32
   :B# 33
   :Bb 31})

(def chords
  {:maj  [0 4 7]
   :min  [0 3 7]
   :dim  [0 3 6]
   :aug  [0 4 8]
   :dom7 [0 4 7 10]
   :maj7 [0 4 7 11]
   :min7 [0 3 7 10]})

(set (keys chords))

(defn get-octave
  "Returns octave multiplier"
  [n]
  (* 12 n))

(defn get-duration
  "Returns duration mulitplier"
  [n]
  (/ 4.0 n))

(defn get-duration-ms
  "Returns duration in ms"
  [n]
  (* 250.0 n))

(defn arpeggiate
  "Plays each note of a chord individually"
  [root oct dur mods]
  (let [xs    (first mods)
        ys    (rest mods)
        delay (get-duration-ms dur)]
    (tone (+ (root notes) xs oct) dur)
    (Thread/sleep delay)
    (when-not (empty? ys)
      (arpeggiate root oct dur ys))))

(defn arpeggio-direction
  "Determines the shape of apreggiagion"
  [mods dir]
  (case dir
    :back (reverse mods)
    :ostn [(nth mods 0) (nth mods 1) (nth mods 2) (nth mods 1)]
    :else mods))

(defn get-last-note
  "Gets the final note or chord out of a loop"
  [loop]
  (let [end (-> loop last butlast last last last)]
    end))

(defn -dur
  "Gets the duration out of a note or chord"
  [note]
  (let [-note (if (= 'loop (first note))
                (get-last-note note)
                note)
        nums  (-> -note 
                  second
                  str
                  (str/split #"\D"))
        parser #(-> % second str (Integer/parseInt))
        dur   (if (= 3 (count nums))
                ;; (-> nums last second str (Integer/parseInt))
                (-> nums last parser)
                (->> nums (remove empty?) first parser))]
    dur))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn n! 
  "Breaks note group down to pitch, octave, and duration"
  [note]
  (let [note (name note)
        name (-> note (str/split #"\d") first keyword)
        nums (last (str/split note #"\D"))
        oct  (-> nums first str Integer/parseInt get-octave)
        dur  (-> nums last str Integer/parseInt get-duration)]
    (tone (+ (name notes) oct) dur)))

(defn c! 
  "Breaks note group down to pitch, octave, and duration and retunrs a chord"
  [chord & args]
  (let [note (name chord)
        root (-> note (str/split #"\d") first keyword)
        nums (second (str/split note #"\D"))
        oct  (-> nums first str Integer/parseInt get-octave)
        dur  (-> nums last str Integer/parseInt get-duration)
        func (-> note (str/split #"\d{2}") last keyword)
        mods (func chords)
        arp? (first args)
        dir? (when (= 2 (count args)) (last args))]
    (if arp? 
      (if dir?
        (arpeggiate root oct dur (arpeggio-direction mods dir?))
        (arpeggiate root oct dur mods))
      (pmap #(tone (+ (root notes) % oct) dur) mods))))

(defmacro play!
  "Runs a sequence of notes, chords, or loops (Use a vector of multiple n, c, l for simultaneous playing)"
  [notes]
  (let [xs (first notes)
        ys (rest notes)]
    (if (vector? xs)
      (if (= 'c! (-> xs first first))
        (map eval xs)
        (pmap eval xs))
      (eval xs))
    (when (or (= 'n! (first xs))
              (when (vector? xs)
                (= 'n! (-> xs first first))))
      (Thread/sleep (* 500 (get-duration (-dur xs)))))
    (when-not (empty? `~ys)
      `(play! ~ys))))

(defmacro l-play!
  "Loops around the play macro"
  [reps notes]
  (loop [x reps]
    (when (> x 0)
      (eval `(play! ~notes))
      (recur (dec x)))))

