(ns looping-is-recursion)

(defn power [base exp]
		(let [helper (fn [acc n k]
			(if (zero? k)
				acc
				(recur (* acc n) n (dec k))
			))]
			(helper 1 base exp))
)

(defn last-element [a-seq]
	(let [helper (fn [acc seq]
		(if (empty? seq)
			acc
			(recur (first seq) (rest seq))
		))]
		(helper nil a-seq)
	)
)

(defn seq= [seq1 seq2]
	(let [helper (fn [a-seq b-seq]
		(if (and (empty? a-seq) (empty? b-seq))
			true
			(if (or (not (= (first a-seq) (first b-seq))) (or (empty? a-seq) (empty? b-seq)))
				false
				(recur (rest a-seq) (rest b-seq))
			)
		))]
		(helper seq1 seq2)
	)
)

(defn find-first-index [pred a-seq]
	(loop [index 0 seq a-seq]
		(if (empty? seq)
			nil
			(if (pred (first seq))
				index
				(recur (+ index 1)(rest seq))
			)
		)
	)
)

(defn avg [a-seq]
	(loop [vals 0 sum 0 seq a-seq]
		(if (empty? seq)
			(/ sum vals)
			(recur (+ vals 1) (+ sum (first seq)) (rest seq))
		)
	)
)
(defn toggle [a-set elem]
	(if (contains? a-set elem) 
		(disj a-set elem) 
		(conj a-set elem)
	)
)

(defn parity [a-seq]
	(loop [a #{} lseq a-seq]
		(if (empty? lseq)
			a
			(recur (toggle a (first lseq)) (rest lseq))
		)
	)
)

(defn fast-fibo [n]
	(loop [f1 0 f 1 n n]
		(cond
			(= 0 n) 0
			(= 1 n) f
			:else (recur f (+ f1 f) (dec n))
		)
	)
)

(defn cut-at-repetition [a-seq]
	(if (empty? a-seq)
		a-seq
		(loop [validseq (seq [(first a-seq)]) aseq (rest a-seq)]
			(if (or (empty? aseq) (= (last validseq) (first aseq)))
				(reverse validseq)
				(recur (conj validseq (first aseq)) (rest aseq))
			)
		)
	)
)

