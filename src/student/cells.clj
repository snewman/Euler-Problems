(ns student.cells)

(def dim-board [90 90])

(def dim-screen [600 600])

(def dim-scale (vec  (map / dim-screen dim-board)))

(defn new-board
  [width, height]
  (for [x (range width)]
    (for [y (range height)] (rand-nth [:on :off]))))
