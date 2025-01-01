(ns fesuku.core
  (:require [seesaw.core :refer :all]
            [seesaw.font :refer [font]]
            [seesaw.color :refer [color]])
  (:gen-class))

;; Modelo del Sudoku y Estado Global
(def empty-board
  (vec (repeat 9 (vec (repeat 9 0)))))

(def initial-state
  {:board empty-board
   :solution nil
   :selected-cell nil
   :game-won false
   :difficulty :medium
   :initial-positions #{}})

(def game-state (atom initial-state))
(def cell-buttons (atom {}))

;; Funciones de Validación del Sudoku
(defn valid-in-row? [board row num]
  (not-any? #(= num %) (nth board row)))

(defn valid-in-col? [board col num]
  (not-any? #(= num (nth % col)) board))

(defn valid-in-box? [board row col num]
  (let [box-row (* 3 (quot row 3))
        box-col (* 3 (quot col 3))]
    (not-any? #(= num %)
              (for [r (range box-row (+ box-row 3))
                    c (range box-col (+ box-col 3))]
                (get-in board [r c])))))

(defn valid-move? [board row col num]
  (and (valid-in-row? board row num)
       (valid-in-col? board col num)
       (valid-in-box? board row col num)
       (not (contains? (:initial-positions @game-state) [row col]))))

;; Actualización de UI
(declare update-all-cells)  ;; Declaración adelantada

(defn update-cell-value [row col value]
  (when-let [btn (get @cell-buttons [row col])]
    (let [{:keys [solution initial-positions]} @game-state
          initial? (contains? initial-positions [row col])]
      (text! btn (if (zero? value) "" (str value)))
      (config! btn :foreground 
              (cond
                initial? (color 0 0 150)  ;; Números iniciales en azul
                (= value (get-in solution [row col])) (color 0 100 0)  ;; Correctos en verde
                :else (color 100 0 0)))))) ;; Incorrectos en rojo

(defn update-all-cells []
  (doseq [row (range 9)
          col (range 9)]
    (update-cell-value row col (get-in (:board @game-state) [row col]))))

;; Generación del Puzzle
(defn find-empty-cell [board]
  (first (for [row (range 9)
               col (range 9)
               :when (zero? (get-in board [row col]))]
           [row col])))

(defn solve-board 
  ([board] 
   (solve-board board 0))
  ([board attempt]
   (if (> attempt 100) 
     nil  ;; Si toma demasiados intentos, reiniciar
     (if-let [[row col] (find-empty-cell board)]
       (let [nums (shuffle (range 1 10))]
         (loop [[n & rest-nums] nums]
           (cond
             (nil? n) nil
             (valid-move? board row col n)
             (if-let [solution (solve-board (assoc-in board [row col] n) attempt)]
               solution
               (recur rest-nums))
             :else (recur rest-nums))))
       board))))

(defn generate-solution []
  (loop []
    (if-let [solution (solve-board empty-board)]
      solution
      (recur))))

(defn remove-numbers [board difficulty]
  (let [cells-to-remove (case difficulty
                         :easy 30
                         :medium 45
                         :hard 55)
        all-positions (for [row (range 9)
                          col (range 9)]
                      [row col])
        positions-to-remove (take cells-to-remove (shuffle all-positions))
        initial-positions (set (for [pos all-positions
                                   :when (not (some #(= pos %) positions-to-remove))]
                               pos))]
    {:board (reduce #(assoc-in %1 %2 0) board positions-to-remove)
     :initial-positions initial-positions}))

;; Verificación de Victoria
(defn board-complete? [board]
  (not-any? zero? (flatten board)))

(defn board-correct? [board solution]
  (= board solution))

(defn check-win []
  (let [{:keys [board solution]} @game-state]
    (when (and (board-complete? board)
               (board-correct? board solution))
      (swap! game-state assoc :game-won true)
      (alert "¡Felicitaciones! Has completado el Sudoku correctamente.")
      true)))

;; Gestión del Juego
(defn new-game [difficulty]
  (let [solution (generate-solution)
        {:keys [board initial-positions]} (remove-numbers solution difficulty)]
    (swap! game-state assoc
           :board board
           :solution solution
           :game-won false
           :difficulty difficulty
           :initial-positions initial-positions
           :selected-cell nil)
    (update-all-cells)))

;; Componentes GUI
(defn create-cell [row col]
  (let [btn (button :text ""
                    :font (font :name "Arial" :size 20 :style :bold)
                    :background (if (zero? (mod (+ (quot row 3) (quot col 3)) 2))
                                (color 240 240 240)
                                (color 220 220 220))
                    :margin 5
                    :size [50 :by 50])]
    (swap! cell-buttons assoc [row col] btn)
    (listen btn :mouse-clicked
            (fn [_]
              (when-not (contains? (:initial-positions @game-state) [row col])
                (swap! game-state assoc :selected-cell [row col])
                (doseq [[_ button] @cell-buttons]
                  (config! button :border nil))
                (config! btn :border (javax.swing.border.LineBorder. 
                                     (color 0 0 255) 2)))))
    btn))

(defn make-move [n]
  (when-let [[row col] (:selected-cell @game-state)]
    (when (valid-move? (:board @game-state) row col n)
      (swap! game-state update :board assoc-in [row col] n)
      (update-cell-value row col n)
      (check-win))))

(defn create-number-pad []
  (horizontal-panel
    :items (for [n (range 1 10)]
            (button :text (str n)
                   :size [50 :by 40]
                   :font (font :size 16 :style :bold)
                   :listen [:action (fn [_] (make-move n))]))))

(defn create-board []
  (border-panel
    :center (grid-panel
             :rows 9 
             :columns 9
             :hgap 1 
             :vgap 1
             :border [5 "Sudoku" :center]
             :items (for [row (range 9)
                         col (range 9)]
                     (create-cell row col)))))

(defn create-difficulty-selector []
  (horizontal-panel
    :items ["Dificultad: "
            (combobox :model [:easy :medium :hard]
                     :selected-item :medium
                     :listen [:selection #(new-game (selection %))])]))

(defn create-controls []
  (vertical-panel
    :items [(horizontal-panel
             :items [(button :text "Nueva partida"
                            :size [120 :by 30]
                            :listen [:action (fn [_] 
                                             (new-game (:difficulty @game-state)))])
                     (button :text "Ver solución"
                            :size [120 :by 30]
                            :listen [:action (fn [_]
                                             (swap! game-state assoc :board (:solution @game-state))
                                             (update-all-cells))])
                     (button :text "Borrar"
                            :size [120 :by 30]
                            :listen [:action (fn [_]
                                             (when-let [[row col] (:selected-cell @game-state)]
                                               (when-not (contains? (:initial-positions @game-state) [row col])
                                                 (swap! game-state update :board assoc-in [row col] 0)
                                                 (update-cell-value row col 0))))])])
            (create-difficulty-selector)
            (create-number-pad)]))

(defn create-game []
  (native!)
  (frame :title "Sudoku"
         :content (border-panel
                   :center (create-board)
                   :south (create-controls)
                   :border 5)
         :on-close :exit
         :size [500 :by 700]))

(defn -main []
  (invoke-later
    (-> (create-game)
        show!)
    (new-game :medium)))
