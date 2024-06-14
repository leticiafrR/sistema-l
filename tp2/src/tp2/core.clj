(ns tp2.core
  (:import (java.util Locale))
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


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
(defn create-path [name-file]
  (str "resources/" name-file)
 )
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


;recive la cantidad de veces y un vector reglas-state
;aplica la transformación n veces, devuelve el estado final
(defn tranformate [n reglas-axioma]
  (as-> reglas-axioma r
        (nth (iterate tranformate-1 r) n)
        (r 1)))

(defn get-input [args]
  {:r (nth args 0) :v (Integer/parseInt (nth args 1)) :w (nth args 2)})

;datos= [ pos ang ]
;       pos=[x y]
(defn crear-tortuga [datos]
  {:pos     (nth datos 0)
   :anguloT (nth datos 1)
   })

(defn numero-dos-decimales [n]
  (Double/parseDouble (String/format Locale/US "%.1f" (object-array [(double n)]))))
(defn anguloReal [angulo]
  (/ (* Math/PI angulo) 180))

(defn calcularX [angulo h]
  (* (Math/cos (anguloReal angulo)) h))

(defn calcularY [angulo h]
  (* (Math/sin (anguloReal angulo)) h))

(defn movXY [angulo h]
  (vec (list (calcularX angulo h) (calcularY angulo h))))

(defn sumaVec [v1 v2]
  (vec (map numero-dos-decimales (map + v1 v2)) )
  )

(defn get-line [modo x y]
  (str modo " " x " " y)
  )

(defn desapilar-n [pila n]
  (if (empty? pila)
    pila
    (if (pos? n)
      (desapilar-n (pop pila) (dec n))
      pila)))
;recibe modo, n y vector: lineas-pila-tortuga
;devuelve vector: lineas-pila-tortuga
(defn avanzar-t [modo n lineas-pila-tortuga-mx-mn]
  (let [
        lineas (lineas-pila-tortuga-mx-mn 0)
        pila (lineas-pila-tortuga-mx-mn 1)
        tortuga (lineas-pila-tortuga-mx-mn 2)
        mx (lineas-pila-tortuga-mx-mn 3)
        mn (lineas-pila-tortuga-mx-mn 4)

        new-pos (sumaVec (tortuga :pos) (movXY (tortuga :anguloT) (* 10 n)))
        new-tortuga (crear-tortuga [new-pos (tortuga :anguloT)])
        new-lines (conj lineas (get-line modo (new-pos 0) (new-pos 1)))
        new-mx (#(mapv %1 %2 %3) max new-pos mx)
        new-mn (#(mapv %1 %2 %3) min new-pos mn)
        new-pila (conj (desapilar-n pila 1) new-tortuga)]
    [new-lines new-pila new-tortuga new-mx new-mn]))

(defn girar-t [horario? ang n lineas-pila-tortuga-mx-mn]
  (let [
        lineas (lineas-pila-tortuga-mx-mn 0)
        pila (lineas-pila-tortuga-mx-mn 1)
        tortuga (lineas-pila-tortuga-mx-mn 2)
        mx (lineas-pila-tortuga-mx-mn 3)
        mn (lineas-pila-tortuga-mx-mn 4)
        new-ang (+ (#(if horario? (- %) % ) (* ang n)) (tortuga :anguloT))
        new-tortuga (crear-tortuga [(tortuga :pos) new-ang])
        new-pila (conj (desapilar-n pila 1) new-tortuga)]
    [lineas new-pila new-tortuga mx mn]))

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

(defn manejo-pila-t [apilo n lineas-pila-tortuga-mx-mn]
  (let [
        lineas (lineas-pila-tortuga-mx-mn 0)
        pila (lineas-pila-tortuga-mx-mn 1)
        tortuga (lineas-pila-tortuga-mx-mn 2)
        mx (lineas-pila-tortuga-mx-mn 3)
        mn (lineas-pila-tortuga-mx-mn 4)
        new-pila (if (true? apilo) (apilar-n pila tortuga n) (desapilar-n pila n))
        new-pos ((first new-pila) :pos)
        new-lines (conj lineas (get-line \M (new-pos 0) (new-pos 1)))
        ]

    [new-lines new-pila (first new-pila) mx mn]))

(defn get-alfabeto [sistem] {
     \F (partial avanzar-t \L,,,), \G (partial avanzar-t \L),
     \f (partial avanzar-t \M), \g (partial avanzar-t \M),
     \+ (partial girar-t false (sistem :angulo)), \- (partial girar-t true (sistem :angulo)),
     \| (partial girar-t true 180), \[ (partial manejo-pila-t true), \] (partial manejo-pila-t false)}) ;recibe tortugra devuelve nueva tortuga

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
  (filter #(contains? alfabeto %) movimientos)
  )

(defn filtrar-movs-rec [movimientos alfabeto ]
  (if (empty? movimientos)
    movimientos
    (let [
          primero (first movimientos)
          resto-movs (rest movimientos)
          ]
      (if (contains? alfabeto primero)
        (conj  (filtrar-movs-rec resto-movs alfabeto) primero)
        (filtrar-movs-rec resto-movs alfabeto)))))

;devuelve una función que espera por [lineas-pila-tortuga] y devuelve [lineas-pila-tortuga]
(defn convert-to-function [seq-op-codes sistem]
  (as-> seq-op-codes codes
        (first codes)                                       ;primer
        ((get-alfabeto sistem) codes)                       ;función asociada al código
        (partial codes (count seq-op-codes))))


(defn get-operations [input sistem-l alfabeto]
  (as-> (filtrar-movs (tranformate (input :v) [(sistem-l :rules) (sistem-l :axioma)]) alfabeto) mov
        (creador-sec-movs mov [])
        (reverse mov)
        (map #(convert-to-function % sistem-l) mov)
        ))


(defn apply-operations [vec operations]
  ((reduce comp (reverse operations)) vec))

(defn get-vec-i []
  (-> []
      (conj (list))
      (conj (list (crear-tortuga [[0 0] 0])))
      (conj (crear-tortuga [[0 0] 0]))
      (conj [0 0])
      (conj [0 0])))


(defn distancia [x1 x2]
  (Math/abs (- x1 x2)))

(defn get-cabecera [vmx vmin]
  (str "<svg viewBox=\"" (- (vmin 0) 20) " " (- (vmin 1) 20) " " (+ (distancia (vmin 0) (vmx 0)) 50) " " (+ (distancia (vmin 1) (vmx 1)) 50) "\" xmlns=\"http://www.w3.org/2000/svg\">\n")
  )

(defn get-cuerpo [round]
  (str "\t<path d=\"M 0 0 " round "\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>\n"))

(defn get-pie []
  (str "</svg>"))


(defn write-file [info-final path]
  (let [
        lineas (reverse (info-final 0))
        vmx    (info-final 3)
        vmn    (info-final 4)
        cabecera (get-cabecera vmx vmn)
        cuerpo (get-cuerpo (str/join " " lineas))
        fin (get-pie)]
    (spit path (str cabecera cuerpo fin))))


(defn -main [& args]
  (let [input (get-input args)
        sistem-l (load-sistem-l input)
        alfabeto (get-alfabeto sistem-l)
        operations (get-operations input sistem-l alfabeto)]
    (-> (get-vec-i)
        (apply-operations ,,, operations )
        (write-file ,,, (create-path (input :w))))
  ))
