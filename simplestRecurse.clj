; napisać program, który w dowolnie zagnieżdzonej sekwencji zamienia wszystkie int 5 na "5"

(defn doit-with-x [x y]
	(if (and (integer? y) (= x y)) 
		(str "\"" x "\"")
		y
	)
)


(defn map-if [pred fn coll]
	:doc "Działaj jak map, ale tylko na tych elementach dla których pred zwraca true"
	(defn myfn [x] (if (pred x) (fn x) (identity x)))
	(map myfn coll)
)

(defn do-change [myseq]
	(def tour1 (map (partial doit-with-x 5) myseq))
	(map-if vector? do-change tour1)
)

(def ret
(let [
	dane [3 4 7 8 [899 1332 [33 94329 5 1] 3 5 [[[[[[5 1]]4 6]5]]] ] 5 0 -1324]
]

(trampoline do-change dane)

))

(println ret)
