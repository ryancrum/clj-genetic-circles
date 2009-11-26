(import
  '(java.awt Graphics Graphics2D Color Polygon)
  '(java.awt.image BufferedImage PixelGrabber)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(javax.swing JFrame JPanel JFileChooser))

(defstruct image :height :width :pixels)
(defstruct circle :position :color :radius)

(defn position-generator [img]
  [(rand-int (:height img)) (rand-int (:width img))])

(defn color-generator [img]
  (for [i (range 4)]
    (rand-int 255)))

(defn grab-pixels [image-buffer]
  (let [w (.getWidth image-buffer)
        h (.getHeight image-buffer)
        pg (PixelGrabber. image-buffer 0 0 w h true)]
    (if (.grabPixels pg)
      (.getPixels pg)
      nil)))

(defn radius-generator [img]
  (rand-int (max (:height img) (:width img))))

(def *generator* (struct circle position-generator color-generator radius-generator))

(defn generate [generator img]
  (apply struct circle (for [k (keys generator)] ((k generator) img))))

(defn generate-n [generator img n]
  (for [i (range n)]
    (generate generator img)))

(defn mutate [generator img shapes settings]
  (for [shape shapes]
    (if (< (rand) (:mutate-probability settings))
      (generate generator img)
      shape)))

(defn same-length? [& lists]
  (apply = (map count lists)))

(defn any [lst]
     (nth lst (rand (count lst))))

(defn crossover [& mates]
  (if (and (seq mates) (apply same-length? mates))
    (loop [mts mates
           baby []]
      (if (seq (first mts))
        (recur (map rest mts)
               (conj baby (any (map first mts))))
        baby))
    nil))

(defn breed [population generator img fitness-fn settings]
  (let [scores (map fitness-fn population)
        fitnesses (apply sorted-map (interleave scores population))
        elite-count (* (:elitism settings) (count population))
        elites (take elite-count (vals fitnesses))]
    [(first elites)
     (for [i (range (count population))]
       (mutate generator img (crossover (any elites) (any elites)) settings))]))

(defn draw [graphics-obj shape]
  (let [color (:color shape)
        position (:position shape)
        radius (:radius shape)]
    (doto graphics-obj
      (.setColor (Color. (nth color 0)
                         (nth color 1)
                         (nth color 2)
                         (nth color 3)))
      (.fillOval (first position) (second position) radius radius))
    nil))

(defn render [shapes img]
  (let [image-buffer (BufferedImage.
                      (:width img)
                      (:height img)
                      BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics image-buffer)]
    (doseq [shape shapes]
      (draw graphics shape))
    image-buffer))

(defn fitness [img individual]
  (let [indiv-pixels (grab-pixels (render individual img))
        src-pixels (:pixels img)]
    (apply + (map - indiv-pixels src-pixels))))

(def best (ref nil))

(defn run [settings img generator-fn fitness-fn]
  (let [initial-population (for [i (range (:population-size settings))]
                             (generate-n generator-fn img (:shape-count settings)))
        fit (partial fitness-fn img)]
    (loop [population initial-population]
      (let [breeding-results (breed population
                                  generator-fn
                                  img
                                  fit
                                  settings)
            fittest (first breeding-results)
            new-population (second breeding-results)]
        (dosync (ref-set best fittest))
        (recur new-population)))))

