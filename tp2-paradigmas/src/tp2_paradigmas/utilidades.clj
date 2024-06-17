(ns tp2-paradigmas.utilidades
  (:require [clojure.string :as str])
  (:import (java.io FileNotFoundException)))
(java.util.Locale/setDefault java.util.Locale/US)

(defn distancia-imagen
  "calcula la distancia (ancho o largo) de la imagen a crear"
  [x1 x2]
  (Math/abs (- x1 x2)))

(defn angulo-real
  "calcula el angulo real a utilizar (pasa de grados hexadecimales a radianes)"
  [angulo]
  (/ (* Math/PI angulo) 180))

(defn calcular-x
  "calcula la distancia a moverse en x"
  [angulo h]
  (* (Math/cos (angulo-real angulo)) h))

(defn calcular-y
  "calcula la distancia a moverse en y"
  [angulo h]
  (* (Math/sin (angulo-real angulo)) h))

(defn mov-x-y
  "devuelve un vector con la direccion a moverse [dx dy]"
  [angulo h]
  (do [(calcular-x angulo h) (calcular-y angulo h)]))

;recibe un vector con cada regla como elemento
;devuelve {"predecesor" "sucesor"}
(defn crear-reglas
  "crea un mapa donde el predecesor es la clave y el sucesor es el valor"
  [reglas]
  (as-> reglas r
        (mapcat #(map seq (str/split % #"\s+")) r)
        (zipmap (flatten (take-nth 2 r)) (take-nth 2 (rest r)))))

;recibe un vec con lineas del file
;reglas es un mapa {"predecesor" "sucesor"}
(defn crear-sistema-l
  "crea el sistema L pasandole las lineas del archivo que contienen el angulo, axioma y reglas"
  [lineas]
  {:angulo-sist (Float/parseFloat (nth lineas 0))
   :axioma-sist (seq (nth lineas 1))
   :reglas-sist (crear-reglas (nthrest lineas 2))})

;devuelve un vector con un string por cada salto de linea en el archivo
(defn leer-archivo
  "lee el archivo y lo devuelve en una secuencia que contiene cada linea
  si el archivo no se encuentra, imprime un mensaje de error"
  [input ]
  (let [archivo (try (slurp (input :entrada))
                     (catch FileNotFoundException e))
        ERROR "archivo.sl no encontrado"]
    (if (some? archivo)
      (str/split-lines archivo)
      (println ERROR))))


(defn get-linea
  "crea la linea de movimiento de la tortuga a escribir en el archivo"
  [modo tortuga] (let [pos (tortuga :pos)]
                    (format "%c %.2f %.2f \n" modo (pos 0) (pos 1) )))

(defn get-input
  "devuelve un mapa con el nombre del archivo de entrada (str), las iteraciones (int) y el nombre del archivo de salida (str)"
  [ruta-sl n ruta-svg]
  {:entrada     ruta-sl
   :iteraciones (Integer/parseInt n)
   :salida      ruta-svg})

(defn get-cuerpo-inicio
  "Devuelve el primer término del cuerpo de un archivo svg."
  [] "\t<path d=\"M 0 0 ")

(defn get-cuerpo-fin
  "Devuelve el último término del cuerpo de un archivo svg."
  [] "\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>\n")

(defn get-cabecera
  "Devuelve el inicio de un archivo svg (con los margenes indicados)"
  [vmx vmn ]
  (format
    "<svg viewBox=\"%.2f %.2f %.2f %.2f\" xmlns=\"http://www.w3.org/2000/svg\">\n"
    (- (vmn 0) 20.00)
    (- (vmn 1) 20.00)
    (+ (distancia-imagen (vmn 0) (vmx 0)) 50.00)
    (+ (distancia-imagen (vmn 1) (vmx 1)) 50.00)))

(defn get-pie
  "Devuelve el string final de un archivo svg."
  [] "</svg>")