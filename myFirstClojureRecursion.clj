
(defn policzTAG [tag-name dane]
	:doc "zwraca ilość tagów o podanej nazwie"
    (count (filter #(= tag-name %) dane)))

(defn makeChunks [dane]
	(partition-all 2 (partition-by keyword? dane))
)

(defn start-tag [keyword-str attrs]
	(str 
		"<" (.replace (str keyword-str) ":" "")
		(if attrs
			(str attrs ">")
			">"
		)
	)
)

(defn end-tag [keyword-str]
	(str "</" (.replace (str keyword-str) ":" "") ">")
)

(defn toHTML5 [chunk]
	(if (vector? (ffirst (rest chunk)))
		(cons
			(first chunk)
			(map toHTML5 (makeChunks (ffirst (rest chunk))))
		)
		(let []
			(def node-val (first (rest chunk)))
			(def attrs 
				(if (map? (first node-val))
					(first node-val)
					nil
				))
			(str
				(start-tag (ffirst chunk) attrs)
				(apply str (filter (complement map?) node-val))
				(end-tag (ffirst chunk))
			)
		)
	)
)

(defn find-nested-and-makeChunks [chunk]
	(if (seq? chunk)
		(map toHTML5 (makeChunks (filter (complement seq?) (tree-seq seq? concat chunk))))
		(identity chunk)
	)
)


; ((:div) ([:span fragment... :span [:a {:href http://www.onet.pl/?alfa=true}  bee]]))
(defn toHTML [chunk]
	(def opening-key (first (first chunk)))
	(def node-val    (first (rest  chunk)))
	(def attrs 
		(if (map? (first node-val))
			(first node-val)
			nil
		))
	(str
		(start-tag opening-key attrs)
		(apply str (map toHTML (makeChunks node-val)))
		(end-tag opening-key)
	)
)



(let [
dane 
	[
	:h4 {:class "mini"} "sevilla" :p "oj jakie to biutiful miesce!" :p "realy" :div [:span "fragment..." :span [:a {:href "http://www.onet.pl/?alfa=true"}" bee"]] :h4 {:style "border:1px solid red;"}"nagłówek 2" :p "tekst" :h4 "i znów" :table [:tr [:td "tabelka śliczna, komórka pierwsza" :td "kom2"] :tr [:td "a" :td "f" ]]
	:div [ :div "1" :div [:span [:a "2"]]]
	
	]
]


	(def parts (makeChunks dane))
	(def parts (map toHTML5 parts))
	(def parts (map find-nested-and-makeChunks parts))
	;(println parts)
	;(println (apply str (flatten parts)))
	
	(defn take6 [dane]
		(map toHTML (makeChunks dane))
		
	)
	(println (take6 dane))

	
)

; don't think iterative!!
(defn nth-element [x coll] (first (nthnext coll x)))
(defn replace-nth [x coll replacement]
	(concat 
		(take (- x 1) coll)
		(cons replacement ())
		(drop x coll)
	))
