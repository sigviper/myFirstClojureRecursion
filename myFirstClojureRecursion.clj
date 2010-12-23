(defn debug [& x]
	(if false
		(debug x)
	)
)

(defn policzTAG [tag-name dane]
	:doc "zwraca ilość tagów o podanej nazwie"
    (count (filter #(= tag-name %) dane)))

(defn makeChunks [dane]
	(partition-all 2 (partition-by keyword? dane))
)

(defn properObj [keyword-str]
	(if (seq? keyword-str)
		(str (first keyword-str))
		(str keyword-str)
	)
)

(defn start-tag [keyword-str attrs]
	(str 
		"<" (.replace (properObj keyword-str) ":" "")
		(if attrs
			(str attrs ">")
			">"
		)
	)
)

(defn end-tag [keyword-str]
	(str "</" (.replace (properObj keyword-str) ":" "") ">")
)

(defn map-if [pred fn coll]
	:doc "Działaj jak map, ale tylko na tych elementach dla których pred zwraca true"
	(defn myfn [x] (if (pred x) (fn x) (identity x)))
	(map myfn coll)
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

(defn flatHTML [coll]
	:doc "parses any-depth vector of html tags/attrs/values to a data structure pratitioned and paired by tag"
	(def take1 (makeChunks coll))
	(defn flatInto [vect]
		;((:div) ([:span fragment... :span [:a {:href http://www.onet.pl/?alfa=true}  bee]]))
		(cons (first vect) (flatHTML (ffirst (rest vect))))
	)
	(defn nesting? [elem]
		;(debug "\nconsidering as nesting " (ffirst (rest elem)))
		(vector? (ffirst (rest elem)))
	)
	(map-if nesting? flatInto take1)
)

(defn reduced-html-str [coll]
	:doc "takes a flatHTML output and reduces it to html string"
	;(println "\ngot collection " coll)
	(defn simple-html-block? [eblock]
		(debug "\nconsidering block as simple " eblock)
		(if (char? (first eblock))
			false
			(let []
				(debug (ffirst (rest eblock)) " yelds " (not (sequential? (ffirst (rest eblock)))))
				(not (sequential? (ffirst (rest eblock))))
			)
		)
		;or (char? (ffirst (rest eblock))) 
	)
	(defn nested-html-block? [eblock]
		;(println "\nconsidering block as nested " eblock ", " (rest eblock) ", count " (count (rest eblock)))
		(if (or (char? eblock) (keyword? eblock) (char? (first eblock)))
			false
			(let []
				;(debug (first (rest eblock)) " yelds " (sequential? (first (rest eblock))))
				(sequential? (first (rest eblock)))
			)
		)
	)
	(defn html-reduce [eblock]
		;(debug "\nreducing html of " eblock)
		(def opening-key (first (first eblock)))
		(def node-val    (first (rest  eblock)))
		(def attrs 
			(if (map? (first node-val))
				(first node-val)
				nil
			))
		(list
			(start-tag opening-key attrs)
			(apply str (filter (complement map?) node-val))
			(end-tag opening-key)
		)
	)
	(defn html-reduce-deep [eblock]
		;(println "\nhtml-reduce-deep " eblock)
		
		(if (nested-html-block? eblock)
			(let [tag (first eblock)]
				;(println "nested " eblock)
				(list
				(start-tag tag nil)
				(reduced-html-str (rest eblock))
				(end-tag tag)
				)
				;(map html-reduce-deep (next eblock))
				;(cons (first eblock) (html-reduce-deep (rest eblock)))
			)
			(html-reduce eblock)
		)
	)
	
	(def take1 (map-if simple-html-block? html-reduce coll))
	;(println "take1 " take1)
	;(map-if nested-html-block? reduced-html-str take1)
	(map-if nested-html-block? html-reduce-deep take1)
	
)

(let [
dane1 
	[
	:h4 {:class "mini"} "sevilla" :p "oj jakie to biutiful miesce!" :p "realy" :div [:span "fragment..." :span [:a {:href "http://www.onet.pl/?alfa=true"}" bee"]] :h4 {:style "border:1px solid red;"}"nagłówek 2" :p "tekst" :h4 "i znów" :table [:tr [:td "tabelka śliczna, komórka pierwsza" :td "kom2"] :tr [:td "a" :td "f" ]]
	:div [ :div "1" :div [:span [:a "2"]]]
	
	]
dane2 [:h4 "niewes" :div [:a "blize" :a "dale"]]
dane3 [:div [:h4 "armada" :p "politycznie podejrzani" :h4 "kiszka" :p [:span "makowiec" :a "linki" ]]]
]


;; 	(def parts (makeChunks dane1))
;; 	(def parts (map toHTML5 parts))
;; 	(def parts (map find-nested-and-makeChunks parts))
	;(debug parts)
	;(debug (apply str (flatten parts)))
	
;; 	(defn take6 [dane]
;; 		(map toHTML (makeChunks dane))
;; 		
;; 	)
	;(debug (take6 dane))
	(println (apply str (flatten (reduced-html-str (flatHTML dane1)))))
)

; don't think iterative!!
(defn nth-element [x coll] (first (nthnext coll x)))
(defn replace-nth [x coll replacement]
	(concat 
		(take (- x 1) coll)
		(cons replacement ())
		(drop x coll)
	))
