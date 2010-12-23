; napisać program, który czyni sensowny użytek z reduce
(def dane1 [1 "uncyk" [:h4 :r [ 3 [4]] ] ["oue" {:cyk "bum"}] 9])
(def dane2 [1 2 1394584275 :h] )

(defn count-and-add [x y]
	(defn count-any [a]
		(if (coll? a)
			(count a)
			1
		)
	)
	
	(+ x (count-any y))
)

(defn counter [coll]
	:doc "zlicz elementy w zawarte w coll i dowolnych kolekcjach zawartych w coll (ale bez głębszej rekurencji)"
	(reduce count-and-add 0 coll)
)

(println (counter dane2))
