(ns dart2cljd.emit.statements
  "IR statements -> cljd forms (faithful mode, M2 subset).

  `emit-body` turns a Dart statement sequence into a vector of cljd body
  forms, hoisting local declarations into nested `let`s and turning a
  trailing `return` into the body's value. Statement-level for-in loops
  become `doseq` (eager, like Dart); collection `for` elements use `for`."
  (:require [dart2cljd.emit.core :as core]))

(declare emit-body emit-stmt)

(defn- emit-expr [node ctx] (core/emit-expr node ctx))

(defn- do-form [forms]
  (cond
    (empty? forms) nil
    (= 1 (count forms)) (first forms)
    :else (list* 'do forms)))

;; ---------------------------------------------------------------------------
;; bodies
;; ---------------------------------------------------------------------------

(defn emit-body
  "Dart statements -> vector of cljd forms (function/loop/block bodies)."
  [stmts ctx]
  (loop [ss (seq stmts) out []]
    (if-not ss
      out
      (let [s (first ss)]
        (case (:ir s)
          :var-decl
          (let [[decls rest-stmts] (split-with #(= :var-decl (:ir %)) ss)
                bindings (vec (mapcat (fn [d]
                                        (mapcat (fn [v]
                                                  [(symbol (:name v))
                                                   (when (:init v) (emit-expr (:init v) ctx))])
                                                (:vars d)))
                                      decls))
                rest-forms (emit-body rest-stmts ctx)]
            (conj out (if (seq rest-forms)
                        (list* 'let bindings rest-forms)
                        (list 'let bindings nil))))

          :return
          (if (next ss)
            (core/fail! s "early return is not supported yet")
            (conj out (when (:expr s) (emit-expr (:expr s) ctx))))

          (recur (next ss) (conj out (emit-stmt s ctx))))))))

;; ---------------------------------------------------------------------------
;; statements
;; ---------------------------------------------------------------------------

(defn- emit-if [s ctx]
  (let [c (emit-expr (:cond s) ctx)
        then (emit-stmt (:then s) ctx)
        els (:else s)]
    (cond
      (:case s) (core/fail! s "pattern if is not supported yet")

      (nil? els) (list 'when c then)

      (= :if (:ir els))
      (let [clauses (loop [n els acc [c then]]
                      (let [c2 (emit-expr (:cond n) ctx)
                            t2 (emit-stmt (:then n) ctx)
                            acc (conj acc c2 t2)]
                        (if-let [e (:else n)]
                          (if (= :if (:ir e))
                            (recur e acc)
                            (conj acc :else (emit-stmt e ctx)))
                          acc)))]
        (list* 'cond clauses))

      :else (list 'if c then (emit-stmt els ctx)))))

(defn- emit-for [s ctx]
  (let [loop (:loop s)
        body (emit-stmt (:body s) ctx)]
    (case (:ir loop)
      :for-in
      (if-let [v (:var loop)]
        (list 'doseq [(symbol (:name v)) (emit-expr (:iterable loop) ctx)] body)
        (core/fail! loop "pattern for-in is not supported yet"))
      :for-parts (core/fail! loop "C-style for loops are not supported yet")
      (core/fail! loop (str "unsupported for loop " (:ir loop))))))

(defn- unguard [p]
  (if (= :guarded-pattern (:ir p))
    [(:pattern p) (:when p)]
    [p nil]))

(defn- strip-terminator [stmts c]
  (let [v (vec stmts)
        last-s (peek v)]
    (case (:ir last-s)
      :break (if (:label last-s)
               (core/fail! c "labeled break is not supported yet")
               (pop v))
      :return (core/fail! c "return inside switch is not supported yet")
      (:throw :rethrow) v
      (core/fail! c "switch case must end with break/return/throw; fallthrough is unsupported"))))

(defn- emit-switch [s ctx]
  (let [subject (emit-expr (:expr s) ctx)]
    (loop [cs (seq (:cases s)) pending [] pairs [] default nil]
      (if-not cs
        (let [pairs (into pairs (map (fn [l] [l nil]) pending))
              labels (map first pairs)
              literal? (fn [l] (or (number? l) (string? l) (boolean? l)
                                   (nil? l) (keyword? l)))]
          (if (every? literal? labels)
            (list* 'case subject
                   (concat (mapcat (fn [[l b]] [l b]) pairs) [default]))
            (list* 'cond
                   (concat (mapcat (fn [[l b]] [(list '= subject l) b]) pairs)
                           [:else default]))))
        (let [c (first cs)
              ir (:ir c)]
          (case ir
            :switch-pattern-case
            (let [[p w] (unguard (:pattern c))]
              (when w (core/fail! c "switch pattern guards are not supported yet"))
              (when-not (= :const-pattern (:ir p))
                (core/fail! c "only constant switch cases are supported yet"))
              (let [label (emit-expr (:expr p) ctx)
                    stmts (:stmts c)]
                (if (empty? stmts)
                  (recur (next cs) (conj pending label) pairs default)
                  (let [body (do-form (emit-body (strip-terminator stmts c) ctx))]
                    (recur (next cs) []
                           (into pairs (for [l (conj pending label)] [l body]))
                           default)))))

            :switch-case
            (let [label (emit-expr (:expr c) ctx)
                  stmts (:stmts c)]
              (if (empty? stmts)
                (recur (next cs) (conj pending label) pairs default)
                (let [body (do-form (emit-body (strip-terminator stmts c) ctx))]
                  (recur (next cs) []
                         (into pairs (for [l (conj pending label)] [l body]))
                         default))))

            :switch-default
            (let [body (do-form (emit-body (:stmts c) ctx))]
              (recur (next cs) pending pairs body))

            (core/fail! c (str "unsupported switch case " ir))))))))

(defn- emit-catch [c ctx]
  (let [ty (if (:exception-type c)
             (core/type-symbol (:exception-type c) ctx)
             'dynamic)
        e (symbol (or (:exception-var c) "_"))
        st (:stack-var c)
        body (emit-body (:stmts (:body c)) (assoc ctx :catch-exception e))]
    (list* 'catch ty e (concat (when st [(symbol st)]) body))))

(defn- emit-try [s ctx]
  (let [body (emit-body (:stmts (:body s)) ctx)
        catches (map #(emit-catch % ctx) (:catches s))
        fin (:finally s)]
    (list* 'try
           (concat body catches
                   (when fin
                     [(list* 'finally (emit-body (:stmts fin) ctx))])))))

(defn emit-stmt
  "Emit one IR statement as a cljd form."
  [s ctx]
  (case (:ir s)
    :block (do-form (emit-body (:stmts s) ctx))
    :expr-stmt (emit-expr (:expr s) ctx)
    :var-decl (first (emit-body [s] ctx))
    :if (emit-if s ctx)
    :for (emit-for s ctx)
    :switch-stmt (emit-switch s ctx)
    :try (emit-try s ctx)
    :return (core/fail! s "return outside of a function body")
    :throw (list 'throw (emit-expr (:expr s) ctx))
    :rethrow (if-let [e (:catch-exception ctx)]
               (list 'throw e)
               (core/fail! s "rethrow outside of a catch clause"))
    :assert-stmt (if (:message s)
                   (list 'assert (emit-expr (:cond s) ctx) (emit-expr (:message s) ctx))
                   (list 'assert (emit-expr (:cond s) ctx)))
    :empty-stmt nil
    :break (core/fail! s "break outside a supported switch case")
    :continue (core/fail! s "continue is not supported yet")
    :labeled (core/fail! s "labeled statements are not supported yet")
    :yield (core/fail! s "yield is not supported yet")
    :fn-decl (core/fail! s "local function declarations are not supported yet")
    :while (core/fail! s "while loops are not supported yet")
    :do-while (core/fail! s "do-while loops are not supported yet")
    :pattern-var-decl-stmt (core/fail! s "pattern declarations are not supported yet")
    :pattern-assign (core/fail! s "pattern assignments are not supported yet")
    (core/fail! s (str "unsupported statement " (:ir s)))))
