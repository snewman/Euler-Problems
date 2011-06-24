(ns student.cells)

(def dim-board [90 90])

(def dim-screen [600 600])

(def dim-scale (vec  (map / dim-screen dim-board)))

(defn new-board
  ([] (new-board dim-screen))
  ( [[ width height]]
      (for [x (range width)]
        (for [y (range height)] (rand-nth [:on :off])))))

(defn row-with-coords
  [row x]
  (for [y (range (count row))] [(nth row y) x y]))

(defn with-coords
  [board]
  (for [x (range (count board))] (row-with-coords (nth board x) x)))

(def state->color
  {:on java.awt.Color/GREEN
   :dying java.awt.Color/RED
   :off java.awt.Color/BLACK})

(defn render-cell [g cell]
  (let [[state x y] cell
        [x-scale y-scale] dim-scale
        x  (inc (* x x-scale))
        y  (inc (* y y-scale))]
    (doto g
      (.setColor (state->color state))
      (.fillRect x y (dec x-scale) (dec y-scale)))))

(defn x
  [[[a b c]]]
  b)


(defn render [graphics img board]
  (let [background-graphics (.getGraphics img)]
    (doto background-graphics
      (.setColor java.awt.Color/BLACK)
      (.fillRect 0 0 (dim-screen 0) (dim-screen 1)))
    (doseq [row (with-coords board)
            cell row]
      (when-not (#{:off} (cell 0))
        (render-cell background-graphics cell)))
    (.drawImage graphics img 0 0 nil)))


(defn launch-1 []
  (let [[screen-x screen-y] dim-screen
        board (atom (new-board))
        frame (javax.swing.JFrame.)
        panel (proxy [javax.swing.JPanel] [])]
    (doto frame
      (.add panel)
      (.pack)
      (.setSize screen-x screen-y)
      (.show)
      (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE))
    board))


(defn launch-2 []
  (let [[screen-x screen-y] dim-screen
        board (atom (new-board))
        frame (javax.swing.JFrame.)
        img   (java.awt.image.BufferedImage. screen-x screen-y java.awt.image.BufferedImage/TYPE_INT_ARGB)
        panel (proxy [javax.swing.JPanel] []
                (paint [g] (render g img @board)))]
    (doto frame
      (.add panel)
      (.pack)
      (.setSize screen-x screen-y)
      (.show)
      (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE))
    board))
