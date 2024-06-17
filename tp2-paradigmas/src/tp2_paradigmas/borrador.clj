(ns tp2-paradigmas.borrador
  (import 'java.io.FileNotFoundException)
  (:require [tp2-paradigmas.utilidades :as utilidades]))

(defn crear-tortuga [posicion angulo]
  {:pos    posicion
   :angulo angulo})

(defn actualizar-avanzar [nueva-tortuga modo estado]
  (-> estado
      (update ,,, :lineas #(doall (concat % (list (utilidades/get-linea modo nueva-tortuga)))))
      (update ,,, :mx #(mapv max (nueva-tortuga :pos) %))
      (update ,,, :mn #(mapv min (nueva-tortuga :pos) %)))
  )

;para avanzar y para girar, estado {:lineas lineas :pila pila :t-actual t-actual :mx mx :mn mn}
;recibe un estado y un aplicador a una tortuga, devuelve una función que envuelve esta lógica y actualiza el estado
(defn actualizador-estado [f & modo]
  (fn [estado]
    (do (println " se intenta avnzar ")(let [nueva-tortuga (f (first (estado :pila)))]
                                         (as-> estado e
                                               (update e :pila #(conj (pop %) nueva-tortuga))
                                               (if (any? modo) (actualizar-avanzar nueva-tortuga (first modo) e) e))))
    ))

(defn agregar-linea-pila [pila lineas]
  (as-> pila p
        ((first p) :pos)
        (utilidades/get-linea \M (p 0) (p 1))
        (list p)
        (concat lineas p)))

(defn actualizar-pila [funcion estado]
  (as-> estado e
        (update e :pila funcion)
        (update e :lineas (partial agregar-linea-pila (e :pila)))))

(defn crear-actualizar-pila [apilo?]
  (fn [estado]
    (let [logica (case apilo?
                   true #(conj % (first %))
                   false #(pop %))]
      (actualizar-pila logica estado))))


(defn crear-girar [ang sentido]
  (fn [tortuga]
    (as-> (* sentido ang) a (+ (tortuga :angulo) a)
          (assoc tortuga :angulo a))))

(defn avanzar [tortuga]
  (let [PASO 10.00]
    (as-> tortuga t
          (do (println "llega ")(mapv + (t :pos) (utilidades/mov-x-y (t :angulo) PASO)))
          (do (println "llega ") (crear-tortuga t (tortuga :angulo))))))

(defn keys-alfabeto [] '(\] \[ \| \- \+ \g \f \G \F))




;recibe sistema {:angulo-sist float :axioma-sist str :reglas-sist {str str}}
;siendo estado {:lineas (str) :pila ({:pos [int int] :angulo float}) :mx [int int] :mn [int int]}
;revuelve (f[int estado]->estado)
(defn valores-alfabeto [sistema]
  (-> ()
      (conj (actualizador-estado avanzar \L))
      (conj (actualizador-estado avanzar \L))
      (conj (actualizador-estado avanzar \M))
      (conj (actualizador-estado avanzar \M))
      (conj (actualizador-estado (crear-girar (sistema :angulo-sist) 1)))
      (conj (actualizador-estado (crear-girar (sistema :angulo-sist) -1)))
      (conj (actualizador-estado (crear-girar 180.00 1)))
      (conj (crear-actualizar-pila true))
      (conj (crear-actualizar-pila false))))

;recibe mapa con clave valor con el que se reemplazará, y una secuencia donde se aplicarán los reemplazos si estan en el diccionario de reemplazos
(defn transformar-1 [mapa-remplazos seq-transformar]
  (->> seq-transformar
       (map #(if (contains? mapa-remplazos %) (mapa-remplazos %) %),,,)
       (flatten,,,)))

;recibe repeticiones int, un mapa{\suc (\X \Y)}
;aplica la transformación n veces, devuelve el axioma final
(defn transformar [iteraciones reglas axioma]
  (as-> reglas r
        (partial transformar-1 r)
        (nth (iterate r (seq axioma)) iteraciones)))

(defn crear-estado
  ([l p x n] {:lineas l :pila p :mx x :mn n})
  ([] (let [nueva-tortuga (crear-tortuga [0.00 0.00] 0.00)] (crear-estado (lazy-seq) (list nueva-tortuga) [0.00 0.00] [0.00 0.00]))))

;recibe sistema {:angulo-sist float :axioma-sist str :reglas-sist {"predecesor" "sucesor"}}
;sea estado {:lineas (str) :pila ({:pos [int int] :angulo float}) :mx [int int] :mn [int int]}
;devuelve {char f[int estado]->estado}
(defn crear-alfabeto [sistem]
  (-> ()
      (conj (valores-alfabeto sistem))
      (conj (keys-alfabeto))
      (#(reduce zipmap %))))

(defn get-operaciones [input sistem-l alfabeto]
  (->> (transformar (input :iteraciones) (sistem-l :reglas-sist) (sistem-l :axioma-sist))
       (filter #(contains? alfabeto %) ,,,)
       (transformar-1 alfabeto ,,,)))

(defn aplicar-operacion-1 [info-t]
  (-> info-t
      (update :estado-i (first (info-t :funciones-i)))
      (update :funciones-i rest)))

(defn aplicar-operaciones [estado funciones]
  (let [info-t {:estado-i estado :funciones-i funciones}]   ;info transformacion
    (as-> info-t i
          (:funciones-i i)
          (count i)
          (nth (iterate aplicar-operacion-1 info-t) i)
          (i :estado-i))))

(defn escribir-salida [estado ruta]
  (with-open [w (clojure.java.io/writer ruta)]
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
            (aplicar-operaciones,,, operaciones)
            (escribir-salida ,,, (input :salida)))))))