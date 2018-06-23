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
  (let [snd (saw (midicps note))
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

(defmacro play! [notes]
  (let [xs (first notes)
        ys (rest notes)]
    (when xs
      (eval xs)
      (let [duration (get-duration (-dur xs))]
        (Thread/sleep (* duration 500))
        (when-not (empty? `~ys)
          `(play! ~ys))))))

(comment 

(n! :C48)

(do
  (c! :C31maj )
  (c! :A31min )
  )

(do
(c! :C08maj7 true)
(c! :C18maj7 true)
(c! :C28maj7 true)
(c! :C38maj7 true)
(c! :C48maj7 true)
(c! :C58maj7 true)
(c! :C68maj7 true))

(do 
  (c! :C28maj7 true :back)
  (c! :A28min7 true :ostn)
  (c! :F28maj7 true :back)
  (c! :E28min7 true))

(loop [x 4]
  (when (> x 0)
    (if (even? x)
      (do
        (n! :E11)
        (do 
          (c! :C24maj7 true)
          (c! :A24min7 true)
          (c! :F24maj7 true)
          (c! :E24min7 true)))

      (do 
        (n! :B11)
        (do
          (c! :C24maj7 true :back)
          (c! :A24min7 true)
          (c! :F24maj true)
          (c! :E24min true)
          (n! :D24)
          (Thread/sleep 250)
          (n! :B24)
          (Thread/sleep 250))))
    (recur (- x 1))))

(defn a []
  (loop [x 8]
    (when (> x 0)
      (if (even? x)
        (do
          (n! :A01)
          (n! :C31)
          (n! :E41)
          (n! :A11)
          (c! :A28min7 true))
        (do 
          (n! :E11)
          (c! :A28min7 true :back)))
      (recur (- x 1)))))

(defn b []
  (loop [x 8]
    (when (> x 0)
      (if (even? x)
        (do
          (n! :F01)
          (n! :A31)
          (n! :C41)
          (n! :E11)
          (c! :F28maj7 true))
        (do 
          (n! :F11)
          (c! :F28maj7 true :back)))
      (recur (- x 1)))))

(defn c []
  (loop [x 8]
    (when (> x 0)
      (if (even? x)
        (do
          (n! :D01)
          (n! :F31)
          (n! :A41)
          (n! :Bb11)
          (c! :D28min7 true))
        (do 
          (n! :D11)
          (c! :D28min7 true :back)))
      (recur (- x 1)))))

  (get-last-note '(loop [x 8]
                   (when (> x 0)
                     (if (even? x)
                       (do
                         (n! :D01)
                         (n! :F31)
                         (n! :A41)
                         (n! :Bb11)
                         (c! :D28min7 true))
                       (do 
                         (n! :D11)
                         (c! :D28min7 true :back)))
                     (recur (- x 1)))))
  
(do 
(a)
(b)
(a)
(b)
(c)
(b)
(a)
(b))

(play!
 [(n! :A24)
  (n! :C34)
  (n! :D38)
  (n! :E38)
  (c! :C34maj7 true)])

(-dur '(c! :C34maj7 true))

(-dur '(n! :A34))

:end)
