;; project.clj
(defproject fesuku "0.1.0-SNAPSHOT"
  :description "Sudoku Game"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [seesaw "1.5.0"]]
  :main fesuku.core
  :aot [fesuku.core])
