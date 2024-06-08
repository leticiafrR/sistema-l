(ns tp2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  )

;Lee archivo y lo deja en seq de strings por linea
;ruta es string "ruta/ruta.formato"
(defn leer_archivo [ruta]

  (with-open [lector (io/reader ruta)]
    (doall (line-seq lector))))

;devuelve el string de la ruta
(defn ruta [nombre]
  (apply str (concat '("resources/") (conj '() nombre))))

;recibe una seq de reglas separadas por regla
;cada regla viena con un espacio entre su predecedor y sucesor
(defn crearReglas [reglas mapa]
  (if (not (empty? reglas))
    (let [regla (seq (.split (first reglas) " "))]
      (merge mapa (crearReglas (rest reglas) {(first regla) (second regla)})))
    mapa))

(defn juntarLinea [linea]
  (apply str linea))

(defn separarLinea [linea]
  (map str (apply concat (partition 1 linea))))

(defn transformacionLinea [linea reglas]
  (juntarLinea (replace reglas (separarLinea linea)))
  )

(defn aplicarIteraciones [pila iteraciones reglas]
  (if (pos? iteraciones)
    (aplicarIteraciones (conj pila (transformacionLinea (first pila) reglas )) (dec iteraciones) reglas)
    pila
    )
  )

(defn -main [& args]
  (if (not (= 3 (count args)))
    (println "Argumentos invalidos")
    (let [r (first args) v (first (next args)) w (first (nnext args))]
      (let [datos (leer_archivo (ruta r))]
        (let [angulo (Integer/parseInt (first datos)) axioma (first (next datos)) reglas (crearReglas (rest (next datos)) nil ) ]
          (println angulo)
          (println axioma)
          (println (transformacionLinea axioma reglas))
          (println (aplicarIteraciones (conj '() axioma) (Integer/parseInt v) reglas))
          (println reglas)))
      (println v)
      (println w))))
