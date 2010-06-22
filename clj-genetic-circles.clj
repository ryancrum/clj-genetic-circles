(import
  '(java.awt Graphics Graphics2D Color Polygon)
  '(java.awt.image BufferedImage PixelGrabber)
  '(java.io File)
  '(java.io FileInputStream)
  '(javax.imageio ImageIO)
  '(javax.swing JFrame JPanel JFileChooser))

(defstruct image :width :height :pixels)
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
    [{:shapes (first elites) :score (first (keys fitnesses))}
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
    (Math/abs (apply + (map - indiv-pixels src-pixels)))))

(defn run [settings fittest-ref counter-ref img generator-fn fitness-fn]
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
        (dosync (ref-set fittest-ref fittest))
        (dosync (alter counter-ref + 1))
        (recur new-population)))))

(def *stop-me* (ref false))

(defn main [filename draw-agent fittest-ref]
  (let [settings {:population-size 25
                  :shape-count 100
                  :elitism 0.4
                  :mutate-probability 0.05}
        selected-image (ImageIO/read (FileInputStream. filename))
        img (struct image
                    (.getWidth selected-image)
                    (.getHeight selected-image)
                    (grab-pixels selected-image))
        fittest fittest-ref
        counter (ref 0)]
    (send-off draw-agent (fn [_] (run settings fittest counter img *generator* fitness)))
    (loop [last-val @counter]
      (let [ctr @counter]
        (if (deref *stop-me*)
          true
          (do
            (if (and (= 0 (mod ctr 2)) (not= ctr last-val))
                (ImageIO/write (render (:shapes @fittest) img) "jpeg" (File. (str "./gen/img_" @counter ".jpg"))))
            (recur ctr)))))))