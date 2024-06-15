(ns tp2.core
  (:require [clojure.string :as str])
  (:import (java.util Locale)))

(defn distancia [x1 x2] (Math/abs (- x1 x2)))

(defn get-cabecera [vmx vmin] (str "<svg viewBox=\"" (- (vmin 0) 20) " " (- (vmin 1) 20) " " (+ (distancia (vmin 0) (vmx 0)) 50) " " (+ (distancia (vmin 1) (vmx 1)) 50) "\" xmlns=\"http://www.w3.org/2000/svg\">\n"))

(defn get-cuerpo [round] (str "\t<path d=\"M 0 0 " round "\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>\n"))

(defn get-pie [] (str "</svg>"))

;devuelve un vector con un string por cada salto de linea en el archivo
(defn read-file [path]
  (let [all-file (slurp path)]
    (str/split-lines all-file)))

;recibe un vector con cada regla como elemento
;devuelve un mapa con pares "predecesor" "sucesor"
(defn create-rules [reglas]
  (as-> reglas r
        (mapcat #(str/split % #"\s+") r)
        (zipmap (take-nth 2 r) (take-nth 2 (rest r)))))

;recive un vec con lineas del file
;reglas es un mapa con pares "predecesor" "sucesor"
(defn create-sistem-l [lines]
  {:angulo (Float/parseFloat (nth lines 0))
   :axioma (nth lines 1)
   :rules  (create-rules (nthrest lines 2))})

(defn create-path [name-file] (str "resources/" name-file))

;devuelve un sistema que posee: int angulo,str axioma y mapa reglas ("predecesor" "sucesor", )
(defn load-sistem-l [input]
  (->> input
       (:r,,,)
       (create-path)
       (read-file)
       (create-sistem-l)))

;recive un str estado del axioma y le aplica las transformaciones
(defn tranformate-1 [reglas-state]
  (let [reglas (reglas-state 0)
        state (reglas-state 1)
        new-state (->> state
                       (partition 1,,,)
                       (apply concat,,,)
                       (map str,,,)
                       (replace reglas,,,)
                       (apply str,,,))]
    [reglas new-state]))

;recibe la cantidad de veces y un vector reglas-state
;aplica la transformación n veces, devuelve el estado final
(defn tranformate [n reglas-axioma]
  (as-> reglas-axioma r
        (nth (iterate tranformate-1 r) n)
        (r 1)))

(defn get-input [args]
  {:r (nth args 0) :v (Integer/parseInt (nth args 1)) :w (nth args 2)})

(defn numero-dos-decimales [n] (Double/parseDouble (String/format Locale/US "%.1f" (object-array [(double n)]))))

(defn anguloReal [angulo] (/ (* Math/PI angulo) 180))

(defn calcularX [angulo h] (* (Math/cos (anguloReal angulo)) h))

(defn calcularY [angulo h] (* (Math/sin (anguloReal angulo)) h))

(defn mov-x-y [angulo h] (vec (list (calcularX angulo h) (calcularY angulo h))))

(defn suma-vec [v1 v2] (vec (map numero-dos-decimales (map + v1 v2))))

(defn desapilar-n [pila n]
  (if (empty? pila)
    pila
    (if (pos? n)
      (desapilar-n (pop pila) (dec n))
      pila)))

(defn apilar-n [pila elemento n]
  (if (pos? n)
    (apilar-n (conj pila elemento) elemento (dec n))
    pila))

(defn agrupador-seguidos [code]
  (fn [axioma] (take-while #(= % code) axioma)))

(defn creador-sec-movs [movs secuencia]
  (loop [m movs
         s secuencia]
    (if (empty? m)
      (reverse s)
      (let [agrupador (agrupador-seguidos (first m))
            sec-unica (agrupador m)]
        (recur (nthrest m (count sec-unica)) (conj s sec-unica))))))

(defn filtrar-movs [movimientos alfabeto]
  (filter #(contains? alfabeto %) movimientos))

(defn apply-operations [estado operations]
  ((reduce comp (reverse operations)) estado))



(defn crear-tortuga [datos]
  {:pos     (nth datos 0)
   :anguloT (nth datos 1)
   })

(defn create-estado
  ([l p t x n] {:lineas l :pila p :t-actual t :mx x :mn n})
  ([] (let [new-tortuga (crear-tortuga [[0 0] 0]) ] (create-estado '() '(new-tortuga) new-tortuga [0 0] [0 0]))))

(defn get-line
  ([modo tortuga] (let [x ((tortuga :pos) 0) y ((tortuga :pos) 1)] (do  (get-line modo x y)) ))
  ([modo x y] (str modo " " x " " y)))

(defn update-manejo-pila [new-pila new-line estado]
  (-> estado
      (assoc,,, :lineas (concat (estado :lineas) (list new-line)))
      (assoc,,, :pila new-pila)
      (assoc,,, :t-actual (first new-pila))))


(defn update-avanzar [new-tortuga modo estado]
  (let [new-line (get-line modo new-tortuga)]
    (-> estado
        (assoc,,, :lineas (concat (estado :lineas) (list new-line)))
        (assoc,,, :pila (conj (desapilar-n (estado :pila) 1) new-tortuga))
        (assoc,,, :t-actual new-tortuga)
        (assoc,,, :mx (#(mapv %1 %2 %3) max (new-tortuga :pos) (estado :mx)))
        (assoc,,, :mn (#(mapv %1 %2 %3) min (new-tortuga :pos) (estado :mn))))))

(defn update-girar [new-tortuga estado]
  (-> estado
      (assoc,,, :pila (conj (desapilar-n (estado :pila) 1) new-tortuga))
      (assoc,,, :t-actual new-tortuga)))

;para avanzar y para girar, estado {:lineas lineas :pila pila :t-actual t-actual :mx mx :mn mn}
;recibe un estado y un aplicador a una tortuga, devuelve una función que envuelve esta lógica y actualiza el estado
(defn updater-estado [f type-action & modo]
  (fn [n estado]
    (let [new-tortuga (f (estado :t-actual) n)]
      (case type-action
        :avanzar (update-avanzar new-tortuga (first modo) estado)
        :girar (update-girar new-tortuga estado)))))

(defn create-manejo-pila [apilo?]
  (fn [n estado ] (let [ new-pila (case apilo?
                                    true (apilar-n (estado :pila) (estado :t-actual) n)
                                    false (desapilar-n (estado :pila) n))
                        new-line (as-> new-pila p
                                       ((first p) :pos)
                                       (get-line \M (p 0) (p 1)))]
      (update-manejo-pila new-pila new-line estado))))

(defn create-girar [ang sentido]
  (fn [tortuga n] (as-> (* n sentido ang ) a
                        (+ (tortuga :anguloT) a)
                        (assoc tortuga :anguloT a))))
(defn avanzar [tortuga n]
  (as-> tortuga t
        (suma-vec (t :pos) (mov-x-y (t :anguloT) (* 10 n)))
        (crear-tortuga [t (tortuga :anguloT)])))

(defn keys-alfabeto [] '(\] \[ \I \- \+ \g \f \G \F))

;cada elemento de values-alfabeto espera por n (cantidad de apariciones del código) y el estado
(defn values-alfabeto [sistem]
  (-> ()
      (conj (updater-estado avanzar :avanzar \L))
      (conj (updater-estado avanzar :avanzar \L))
      (conj (updater-estado avanzar :avanzar \M))
      (conj (updater-estado avanzar :avanzar \M))
      (conj (updater-estado (create-girar (sistem :angulo) 1) :girar ))
      (conj (updater-estado (create-girar (sistem :angulo) -1) :girar ))
      (conj (updater-estado (create-girar 180 1) :girar ))
      (conj (create-manejo-pila true))
      (conj (create-manejo-pila false))))

(defn create-alfabeto [sistem]
  (-> ()
      (conj (values-alfabeto sistem))
      (conj (keys-alfabeto))
      (#(reduce zipmap %))))

;devuelve una función que espera por [lineas-pila-tortuga] y devuelve [lineas-pila-tortuga]
(defn convert-to-function [seq-op-codes sistem]
  (as-> seq-op-codes codes
        (first codes)                                       ;primer
        ((create-alfabeto sistem) codes)                    ;función asociada al código
        (partial codes (count seq-op-codes))))


(defn get-operations [input sistem-l alfabeto]
  (as-> (filtrar-movs (tranformate (input :v) [(sistem-l :rules) (sistem-l :axioma)]) alfabeto) mov
        (creador-sec-movs mov [])
        (reverse mov)
        (map #(convert-to-function % sistem-l) mov)))

(defn write-file! [estado path]
  (let [lineas (estado :lineas)
        cabecera (get-cabecera (estado :mx) (estado :mn))
        cuerpo (get-cuerpo (str/join " " lineas))
        fin (get-pie)]
    (spit path (str cabecera cuerpo fin))))

(defn -main [& args]
  (let [input (get-input args)
        sistem-l (load-sistem-l input)
        alfabeto (create-alfabeto sistem-l)
        operations (get-operations input sistem-l alfabeto)]
    (-> (create-estado)
        (apply-operations,,, operations)
        (write-file! ,,, (create-path (input :w))))))
