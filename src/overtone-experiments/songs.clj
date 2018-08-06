(ns overtone-experiments.songs
  (:require
   [overtone-experiments.core :refer [c! n! play! l-play!]]))

(comment 

(n! :C48)

(do
  (c! :C31maj true)
  (c! :A31min true :back)
  )

(def arpy
  (play! [
          (c! :C08maj7 true)
          (c! :C18maj7 true)
          (c! :C28maj7 true)
          (c! :C38maj7 true)
          (c! :C48maj7 true)
          (c! :C58maj7 true)
          (c! :C68maj7 true)]))

(do 
  (c! :C28maj7 true :back)
  (c! :A28min7 true :ostn)
  (c! :F28maj7 true :back)
  (c! :E28min7 true))

:end)

(defn test-song1 []
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
      (recur (- x 1)))))

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

(defn test-song2 []
  (do 
    (a)(b)(a)(b)
    (c)(b)(a)(b)))


(defn counterpoint []
  (play! [
   [(n! :A21) (n! :C21)]
   [(n! :G21) (n! :D21)]
   [(n! :A22) (n! :D22)]
   [(n! :F22) (n! :B22)]
   [(n! :A21) (n! :C21)]
   ]))

(comment

  (play!
   [(n! :A24)
    (n! :C34)
    (n! :D38)
    (n! :E38)
    (c! :C34maj7 true)])

  (play! [(n! :A24) [(n! :A24)(n! :C24)](n! :E24)])

  (test-song1)

 (test-song2)

  (counterpoint)

  (play! [(n! :A32)])

(play!
 2
 [[(c! :A32min7 true) (c! :F22maj7 true)]
  [(c! :A32min7 true :back) (c! :F22maj7 true :back)]])

(l-play! 2 [
        [(n! :A32) (n! :F22)]
        [(n! :G32) (n! :A22)]
        ])

(play!
 [[(c! :A38min7 true)
   (c! :A38min7 true :back)
   (c! :D38min7 true)
   (c! :E38min7 true :back)]

  [(c! :E38min7 true)
   (c! :E38min7 true :back)
   (c! :A38min7 true)
   (c! :E38min7 true :back)]])

(defn bass []
  [(n! :A22)
   (n! :E22)
   (n! :D22)
   (n! :E22)])

(play! [(bass)])

:end)

