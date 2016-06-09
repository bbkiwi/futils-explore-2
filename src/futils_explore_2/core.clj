(ns futils-explore-2.core
    (:require [clojure.repl :refer :all]
              [clojure.set :as set]
              [futils.utils :as fu]
              [futils.args :as fa]
              [futils.named :as fn]))


(defn foo
  "I don't do a whole lot."
  [{a :a b :b cc :bill}]
  (println :a " was " a :b " was " b " and " :bill " was " cc))

;TODO Why does trying to put in meta data fail?
;  inspired by clojure.core line 330
(defmacro unnameize
  "Takes a 1-arity function that has argument consisting of  map of 3 named arguments
  Creates a 3-arity function with those named arguments"
  [uf f]
  (let [internmap (ns-interns *ns*)
        var (get internmap f)
        metavar (meta var)
        doc (str "\"" (metavar :doc) "\"")
        argm (first (first (metavar :arglists)))
        ;TODO need to handle not just 3 key case!
        [k1 k2 k3] (keys argm)
        m (merge metavar {:arglists (list (vector k1 k2 k3))})
        ;m (merge metavar {:arglists (list (vector '(quote a) '(quote b) '(quote cc)))})
        iargm (set/map-invert argm)]
    (println  m)
    (list `def (with-meta uf m)
          (with-meta
            (list `fn (list [k1 k2 k3] (list f iargm)))
            {:rettag (:tag m)}))))

(defmacro unnameize-2
  "Takes a 1-arity function that has argument consisting of  map of 3 named arguments
  Creates a 3-arity function with those named arguments"
  [uf f]
  (let [internmap (ns-interns *ns*)
        var (get internmap f)
        metavar (meta var)
        doc (str "\"" (metavar :doc) "\"")
        argm (first (first (metavar :arglists)))
        ;TODO need to handle not just 3 key case!
        [k1 k2 k3] (keys argm)
        m (merge metavar {:arglists (list (vector k1 k2 k3))})
        iargm (set/map-invert argm)]
    (println  m)
    `(def (with-meta uf m)
          (~with-meta
            (fn (~[k1 k2 k3] (~f ~iargm)))
            {:rettag (:tag m)}))))


(defmacro unnameize-no-meta
  "Takes a 1-arity function that has argument consisting of  map of 3 named arguments
  Creates a 3-arity function with those named arguments"
  [uf f]
  (let [internmap (ns-interns *ns*)
        var (get internmap f)
        metavar (meta var)
        doc (str "\"" (metavar :doc) "\"")
        argm (first (first (metavar :arglists)))
        ;TODO need to handle not just 3 key case!
        [k1 k2 k3] (keys argm)
        m (merge metavar {:arglists (list (vector k1 k2 k3))})
        iargm (set/map-invert argm)]
    ;(println  m)
    (list `def uf
          (list `fn (list [k1 k2 k3] (list f iargm))))))


(defmacro unnameize-no-meta-2
  "Takes a 1-arity function that has argument consisting of  map of 3 named arguments
  Creates a 3-arity function with those named arguments"
  [uf f]
  (let [internmap (ns-interns *ns*)
        var (get internmap f)
        metavar (meta var)
        doc (str "\"" (metavar :doc) "\"")
        argm (first (first (metavar :arglists)))
        ;TODO need to handle not just 3 key case!
        [k1 k2 k3] (keys argm)
        m (merge metavar {:arglists (list (vector k1 k2 k3))})
        iargm (set/map-invert argm)]
    ;(println  m)
    `(def ~uf
          (fn (~[k1 k2 k3] (~f ~iargm))))))




