(ns tp2-paradigmas.core
  (:require [clojure.java.io :as io]
            [tp2-paradigmas.utilidades :as utilidades]))

(defn crear-tortuga
  "[] crea tortuga con valores iniciales en posicion [0 0] y angulo 0
  [posicion angulo] crea tortuga con la posicion y angulo pasado
  "
  ([] (crear-tortuga [0.00 0.00] 0))
  ([posicion angulo] {:pos posicion :angulo angulo}))

(defn actualizar-avanzar
  "actualiza el estado actual agregando la linea correspondiente con su modo (M o L)
   actualiza los vectores maximos y minimos, comparando con la nueva posicion de la tortuga
  "
  [nueva-tortuga modo estado]
  (-> estado
      (update ,,, :lineas #(doall (concat % (list (utilidades/get-linea modo nueva-tortuga)))))
      (update ,,, :mx #(mapv max (nueva-tortuga :pos) %))
      (update ,,, :mn #(mapv min (nueva-tortuga :pos) %)))
  )

(defn actualizador-estado
  "devuelve una funcion para actualizar el estado"
  [f & modo]
  (fn [estado]
    (let [nueva-tortuga (f (first (estado :pila)))]
      (as-> estado e
            (update e :pila #(conj (pop %) nueva-tortuga))
            (if (not (nil? modo) ) (actualizar-avanzar nueva-tortuga (first modo) e) e)))))

(defn agregar-linea-pila
  "agrega una linea para mover a la tortuga cuando ocurran operaciones de pila"
  [pila lineas]
  (as-> pila p
        (first p)
        (utilidades/get-linea \M p)
        (list p)
        (concat lineas p)))

(defn actualizar-pila
  "actualiza la pila y agrega una linea"
  [funcion estado]
  (as-> estado e
        (update e :pila funcion)
        (update e :lineas (partial agregar-linea-pila (e :pila)))))

(defn crear-actualizar-pila
  "crea una funcion que espera por el estado, que le aplica la funcion correspondiente (apilar o desapilar)"
  [apilo?]
  (fn [estado]
    (let [logica (case apilo?
                   true #(conj % (first %))
                   false #(pop %))]
      (actualizar-pila logica estado))))

(defn crear-girar
  "crea una funcion que espera por la tortuga para hacerla girar en el sentido y angulo pasado"
  [ang sentido]
  (fn [tortuga]
    (as-> (* sentido ang) a
          (update tortuga :angulo #(+ a %)))))

(defn avanzar
  "avanza a la tortuga"
  [tortuga]
  (let [PASO 10]
    (as-> tortuga t
          (mapv + (t :pos) (utilidades/mov-x-y (t :angulo) PASO))
          (crear-tortuga t (tortuga :angulo)))))

(defn keys-alfabeto
  "devuelve el alfabeto disponible"
  [] '(\] \[ \| \- \+ \g \f \G \F))

(defn valores-alfabeto
  "crea una lista de funciones que reciben un \"estado\" y devuelven un \"estado\".
  Todas represenatan las transformaciones posibles del alfabeto."
  [sistema] (-> ()
      (conj (actualizador-estado avanzar \L))
      (conj (actualizador-estado avanzar \L))
      (conj (actualizador-estado avanzar \M))
      (conj (actualizador-estado avanzar \M))
      (conj (actualizador-estado (crear-girar (sistema :angulo-sist) 1)))
      (conj (actualizador-estado (crear-girar (sistema :angulo-sist) -1)))
      (conj (actualizador-estado (crear-girar 180 1)))
      (conj (crear-actualizar-pila true))
      (conj (crear-actualizar-pila false))))

(defn transformar-1
  "Recibe un mapa de reemplazos y aplica 1 transformación a la secuencia a transformar. Devuelve la secuencia con los remplazos incorporados"
  [mapa-remplazos seq-transformar]
  (->> seq-transformar
       (map #(if (contains? mapa-remplazos %) (mapa-remplazos %) %),,,)
       (flatten ,,, )))

(defn transformar
  "Recibe la cantidad de veces a transformar y llama (con las reglas y el axioma) a transformar-1 la cantidad de veces que indica iteraciones."
  [iteraciones reglas axioma]
  (as-> reglas r
        (partial transformar-1 r)
        (nth (iterate r (seq axioma)) iteraciones)))

(defn crear-estado
  "Crea un estado con lineas, pila, maximo y minimo."
  ([l p x n] {:lineas l :pila p :mx x :mn n})
  ([] (let [nueva-tortuga (crear-tortuga)] (crear-estado (lazy-seq) (list nueva-tortuga) [0 0] [0 0]))))


(defn crear-alfabeto
  "Dado el sistema-l crea un alfabeto que asocia cada letra a una función que recibe un estado y devuelve un estado."
  [sistem]
  (-> ()
      (conj (valores-alfabeto sistem))
      (conj (keys-alfabeto))
      (#(reduce zipmap %))))

(defn get-operaciones
  "Transforma n veces (indicado en el input) las reglas de conversion al axioma (indicados en el sistema). Si algún simbolo del axioma no epertence al alfabeto, lo quita.
   Devuelve una secuencia de funciones del alfabeto que representan las transformaciones."
  [input sistem-l alfabeto]
  (->> (transformar (input :iteraciones) (sistem-l :reglas-sist) (sistem-l :axioma-sist))
       (filter #(contains? alfabeto %) ,,,)
       (transformar-1 alfabeto ,,,)))

(defn aplicar-operacion-1
  "Recibe la información de una transformación (un mapa) con un estado y una secuencia de funciones.
   Devuelve la informacion con el estado (despues habersele aplicado una funcion) y el resto de funciones "
  [info-t]
  (-> info-t
      (update :estado-i (first (info-t :funciones-i)))
      (update :funciones-i rest)))

(defn aplicar-operaciones
  "Recibe un estado y una secuencia de funciones que transforman el estado a otro estado.
  Se aplican todas las transformaciones secuencialmente y se devuelve el estado final "
  [estado funciones]
  (let [info-t {:estado-i estado :funciones-i funciones}]   ;info transformacion
    (as-> info-t i
          (:funciones-i i)
          (count i)
          (nth (iterate aplicar-operacion-1 info-t) i)
          (i :estado-i))))

(defn escribir-salida!
  "Escribe en el archivo de la ruta indicada las lineas del estado recibido."
  [estado ruta] (with-open [w (io/writer ruta)]
    (.write w (utilidades/get-cabecera (estado :mx) (estado :mn)))
    (.write w (utilidades/get-cuerpo-inicio))
    (doseq [linea (estado :lineas)]
      (.write w linea))
    (.write w (utilidades/get-cuerpo-fin))
    (.write w (utilidades/get-pie))))

(defn -main [ruta-sl n ruta-svg]
  (let [input (utilidades/get-input ruta-sl n ruta-svg)
        lineas (utilidades/leer-archivo input)]
    (if (some? lineas)
      (let
        [sistem-l (utilidades/crear-sistema-l lineas)
         alfabeto (crear-alfabeto sistem-l)
         operaciones (get-operaciones input sistem-l alfabeto)]
        (-> (crear-estado)
            (aplicar-operaciones ,,, operaciones)
            (escribir-salida! ,,, (input :salida)))))))