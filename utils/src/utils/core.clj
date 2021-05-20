(ns utils.core
  (:gen-class))

  (use 'clojure.set 'utils.math)

  (defn cartesian
    "computes Cartesian product of A and B"
 
    [A B]
   (set (for [a A b B] [a b]))
   
  )

  (defn powerset 
    "computes the powerset of S"
  
    [S]
    
    (if (empty? S) 
      '#{#{}}
      (let [a (first S) p (powerset (next S))]
        (union 
          p
          (map #(union #{a} %) p)
        )
      )
    )
  )

  (defn dom 
    "computes the domain of a relation"
  
    [R]
    
    (set (for [r R] (first r)))
  )

  (defn rng
    "computes the range of a relation"
 
    [R]
 
    (set (for [r R] (second r)))
  )

  (defn image-of
    "computes the image of the element x under R"
 
    [R x]
    (set (for [r R :when(=(first r ) x ) ] (second r)))
 
  )

  (defn image-of-set 
    "computes the image of the set X under R"
  
    [R X]
  
    (apply union (map #(image-of R %) X))
  )

  (defn compose 
    "computes the composition of S and R"
    
    [S R]
    
    (set (apply union
      (for [r R] (map #(vector (first r) %) (image-of S (second r))))
    ))
  )

  (defn trans-closure
    "Calculates the transitive clojure of a relation given as a set of vectors, 
     e.g. (trans-closure #{[1 2] [2 3]}) yields #{[1 2] [2 3] [1 3]}"
    [e]
    (letfn [(f [x] (for [[a b] x [c d] x :when (= b c)] [a d]))]
           (let [e2 (set (f e))]
             (if (subset? e2 e)
                 e
               (recur (union e e2))))))
  
  (declare closure-rec)

  (defn closure
    "computes the closure of X under the endofunction f
    example: (closure f #{2})"
    
    [f X]

  ;;(if (= (dom f) (rng f))
  (closure-rec f X X)
  ;;(print "Not endofunction")
  ;;)
    
  )

;; This is broken atm
  (defn closure-rec
    "helper function to compute the closure"
  
    [f X Y]
  
    (if (and (subset? X Y) (subset? (image-of-set f Y) Y))
    Y
    (union Y (closure-rec f X (image-of-set f Y)))
    )

    )

  (defn- inv1
    "flips a tuple"
  
    [[a b]]
    
    [b a]
  )

  (defn inverse
    "computes the inverse (aka converse) of a relation"
 
    [R]
    (set (for [a R] (reverse a)))
    
  )

  (defn complement-relation 

    [R A B]
    
    (set (for [a A b B :when (not (contains? R [a b]))] [a b]))
  )

  (defn reflexive?
    "tests whether R is reflexive over A"
 
   [R A]
 
    (every? #(contains? R [% %]) A)
    
  )

  (defn irreflexive? 
    "tests whether R is irreflexive over its domain"
  
    [R]
  
    (every? #(not (contains? R [% %])) (dom R))
  )

  (defn symmetric?
    "tests whether R is symmetric"
 
    [R]
 
    (every? #(contains? R (inv1 %)) R)
    
  )

  (defn transitive?
    "tests whether R is transitive"
 
    [R]
 
    (every? #(subset? (image-of R (second %)) (image-of R (first %))) R)
 
  )

  (defn asymmetric?
    "tests whether R is asymmetric"
  
    [R]
   
    (every?
      #(not (contains? R (inv1 %)))
      R
    )
  )

  (defn antisymmetric? 
    "tests whether R is antisymmetric"
  
    [R]
    
    (every?
      #(if (contains? R (inv1 %)) (= (first %) (second %)) true)
      R
    )
  )

  (defn strict-order? 
    "tests whether R is a strict (partial) order"
  
    [R]
    
    (and (irreflexive? R) (transitive? R))
  )

  (defn non-strict-order?
    "tests whether R is a non-strict (partial) order on A"
  
    [R A]
    
    (and (reflexive? R A) (antisymmetric? R) (transitive? R))
  )

  (defn total?
    "tests whether R is a total on A"
  
    [R A]
    
    (every? (fn [[a b]] (or (= a b) (R [a b]) (R [b a]))) (cartesian A A))
  )

  (defn equivalence?
    "tests whether R is an equivalence relation on A"
  
    [R A]
      
    (and (reflexive? R A) (symmetric? R) (transitive? R))
  )

  (defn isfunction? 
    "determines whether a relation R is a function with domain A"
    
    [R A]
  
    (every? #(= (count (image-of R %)) 1) A)
  )

  (defn injective? 
    "determines whether f is injective"
    
    [f]
  
    (let [g (inverse f)]
      (every?
        #(= 1 (count (image-of g %)))
        (dom g)
      )
    )
  )

  (defn surjective?
    "determines whether f is surjective on codomain B"
 
    [f B]
 
    (= (rng f) B)
    
  )

  (defn bijective? 
    "determines whether f is a bijection from its domain to codomain B"
  
    [f B]
  
    (and (injective? f) (surjective? f B))
  )
  
;;  (defn gcd 
;;    "computes the greatest common divisor of two integers"
;;    [a b]
;;    (if (= b 0)
;;      a
;;      (gcd b (mod a b))
;;    )
;;  )

;;  (defn divides? 
;;    "returns true of a divides b evenly, i.e. without remainder"
;;    [a b] 
;;    (= (rem b a) 0)
;;  )

;;  (defn prime? 
;;    "true, iff n is a prime"
;;    [n]
;;    (and
;;      (> n 1)
;;      (empty? (filter (fn [k] (divides? k n)) (range 2 n)))
;;    )
;;  )

;;  (defn primes-to 
;;    "computes the primes up to n"
;;    [n]
;;    (filter prime? (range 2 (+ n 1)))
;;  )
  
;;  (defn factors-of 
;;    "computes a list of all factors of an integer n"
;;    [n]
;;    (filter (fn [k] (= (rem n k) 0)) (range 1 (+ n 1)))
;;  )
  
;;  (defn prime-factors-of 
;;    "computes a list of all prime factors of an integer n"
;;    [n]
;;    (filter prime? (factors-of n))
;;  )
  
;;  (defn coprime? [a b]
;;    (empty? (intersection (set (prime-factors-of a)) (set (prime-factors-of b))))
;;  )