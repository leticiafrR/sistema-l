(ns tp2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;Lee archivo y lo deja en seq de strings por linea
;ruta es string "ruta/ruta.formato"
(defn read-file [path]
  (with-open [lector (io/reader path)]
    (doall (line-seq lector))))

;devuelve un vector con un string por cada salto de linea en el archivo
(defn read-file [path]
  (let [all-file (slurp path)]
    (str/split-lines all-file)))

;recibe un vector con cada regla como elemento
;devuelve un mapa con pares "predecesor" "sucesor"
(defn create-rules [reglas]
  (as-> reglas
      (mapcat #(str/split % #"\s+") reglas)
      (zipmap (take-nth 2 reglas) (take-nth 2 (rest reglas)))))
;("p s" "p2 s2") =>{"p" "s", "p2" "s2"}


;recive un vec con lineas del file
;reglas es un mapa con pares "predecesor" "sucesor"
(defn create-sistem-l [lines]
  {:angulo (Float/parseFloat (nth lines 0))
   :axioma (nth lines 1)
   :rules (create-rules (nthrest lines 2))})

;devuelve un sistema que posee: int angulo,str axioma y mapa reglas ("predecesor" "sucesor", )
(defn load-sistem-l [input]
  (->> input
       (:r ,,,)
       (str "source/" ,,,)
       (read-file)
       (create-sistem-l )))


;recive un str estado del axioma y le aplica las transformaciones
(defn tranformate-1 [reglas-state]
  (let [reglas (reglas-state 0)
        state (reglas-state 1)
        new-state (->> state
                       (partition 1 ,,,)
                       (apply concat ,,,)
                       (map str ,,,)
                       (replace reglas ,,,)
                       (apply str ,,,))]
    [reglas new-state]))


;recive la cantidad de veces y un vector reglas-state
;aplica la transformación n veces, devuelve el estado final
(defn tranformate [n  reglas-axioma]
  (as-> reglas-axioma
       (nth (iterate tranformate-1 reglas-axioma) n)
       (reglas-axioma 1)))

(defn create-turttle[])

(defn get-input [args]
  {:r (nth args 0) :v (nth args 1) :w (nth args 2)})

(defn -main [& args]
  (let [input (get-input args)
        sistem-l (load-sistem-l input )
        new-axiom (tranformate (input :v) [(sistem-l :rules) (sistem-l :axioma)] )
        tortuga (create-turttle)]))
