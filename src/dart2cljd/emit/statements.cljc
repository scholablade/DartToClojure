(ns dart2cljd.emit.statements
  "IR statements -> cljd forms (faithful mode).

  Body emission has two shapes:

  * direct mode — used when no `return` needs to escape a nested construct.
    Local declarations are hoisted into `let`s and a trailing `return`
    becomes the body's value. `break`/`continue` loops carry guard flags.
  * guarded mode — used when a function contains early/non-tail returns.
    The function body is wrapped in
      (let [d2c$result (volatile! nil) d2c$done (volatile! false)]
        ... @d2c$result)
    `return` sets both boxes; every following statement is guarded so it is
    skipped once a box is set. Loop `break`/`continue` add their own flags.

  Statement-level for-in loops become `doseq` (eager, like Dart) when no
  jumps escape them; otherwise they become `loop`/`recur`. Collection `for`
  elements stay lazy via dart2cljd.emit.core."
  (:require [dart2cljd.emit.core :as core]
            [dart2cljd.mutable :as mutable]
            [dart2cljd.pattern :as pattern]))

(declare emit-body emit-stmt emit-guarded-seq tail-stmt?)

(defn- fail! [node msg] (core/fail! node msg))
(defn- emit-expr [node ctx] (core/emit-expr node ctx))

(defn- do-form [forms]
  (cond
    (empty? forms) nil
    (= 1 (count forms)) (first forms)
    :else (list* 'do forms)))

(defn- and-form [& xs]
  (let [xs (remove nil? xs)]
    (case (count xs)
      0 true
      1 (first xs)
      (list* 'and xs))))

;; ---------------------------------------------------------------------------
;; return / tail analysis
;; ---------------------------------------------------------------------------

(defn- contains-return? [node]
  (cond
    (map? node)
    (case (:ir node)
      (:fn :fn-decl) false
      :return true
      (boolean (some contains-return?
                     (vals (dissoc node :ir :src :annotations)))))
    (coll? node) (boolean (some contains-return? node))
    :else false))

(defn- all-returns-tail? [stmts]
  (loop [ss (seq stmts)]
    (if-not ss
      true
      (if (next ss)
        (and (not (contains-return? (first ss)))
             (recur (next ss)))
        (tail-stmt? (first ss))))))

(defn- tail-stmt? [s]
  (case (:ir s)
    :return true
    :block (all-returns-tail? (:stmts s))
    :if (and (not (contains-return? (:cond s)))
             (tail-stmt? (:then s))
             (or (nil? (:else s)) (tail-stmt? (:else s))))
    :switch-stmt (every? #(all-returns-tail? (:stmts %)) (:cases s))
    :try (and (all-returns-tail? (:stmts (:body s)))
              (every? #(all-returns-tail? (:stmts (:body %))) (:catches s))
              (or (nil? (:finally s))
                  (not (contains-return? (:finally s)))))
    :labeled (tail-stmt? (:stmt s))
    (not (contains-return? s))))

;; ---------------------------------------------------------------------------
;; guards
;; ---------------------------------------------------------------------------

(defn- guards-form
  "`(not (or (deref g) ...))` for the guard symbols in `guards`, or nil."
  [guards]
  (when (seq guards)
    (list 'not
          (if (= 1 (count guards))
            (list 'deref (first guards))
            (list* 'or (map #(list 'deref %) guards))))))

(defn- guard-test [ctx]
  (guards-form (:guards ctx)))

;; ---------------------------------------------------------------------------
;; declarations
;; ---------------------------------------------------------------------------

(defn- stmt-stmts [s]
  (if (= :block (:ir s)) (:stmts s) [s]))

(defn- decl? [s]
  (contains? #{:var-decl :fn-decl :pattern-var-decl-stmt} (:ir s)))

(defn- pattern-bind-vec
  "Flatten pattern `binds` ([[sym form] ...]) into a `let` binding vector.
  Names the mutability analysis marked are boxed (R1) so reads/writes emit
  `deref`/`vreset!` consistently."
  [binds ctx]
  (vec (mapcat (fn [[n form]]
                 (let [s (core/local-symbol n ctx)
                       nm (if (symbol? n) (name n) (str n))]
                   (if (contains? (:mutable ctx) nm)
                     [s (list (if (contains? (:captured ctx) nm)
                                'atom 'volatile!)
                              form)]
                     [s form])))
               binds)))

(defn- local-fn-form [d ctx]
  (when-not (= :fn (:kind d))
    (fail! d "local getters/setters are not supported"))
  (let [{:keys [params body async rebinds]} (core/emit-callable d ctx)]
    (with-meta (list* 'fn (symbol (:name d)) params (core/wrap-rebinds rebinds body))
      (when async {:async true}))))

(defn- await-init
  "ClojureDart's `no-future` unwraps only a canonical `Future`/`FutureOr`, so a
  local initialized by `await` on a type that merely *implements* Future
  (Supabase's `PostgrestBuilder` chains) is annotated with the pre-await type —
  a Dart analyzer error and a runtime cast failure. The awaited type is not
  knowable from the syntax, so such a local is bound through
  `cljd.core/identity`, which makes ClojureDart infer `dynamic` (conservative,
  and exact for dynamic dispatch)."
  [init form]
  (if (= :await (:ir init))
    (list 'cljd.core/identity form)
    form))

(defn- decl->parts
  "A declaration statement -> {:bindings [...] :checks [form ...]}."
  [d ctx]
  (case (:ir d)
    :var-decl
    {:bindings
     (vec (mapcat (fn [v]
                    (let [n (:name v)
                          init (when (:init v)
                                 (await-init (:init v) (emit-expr (:init v) ctx)))]
                      [(core/local-symbol n ctx)
                       (if (contains? (:mutable ctx) n)
                         (list (if (contains? (:captured ctx) n)
                                 'atom 'volatile!)
                               init)
                         init)]))
                  (:vars d)))
     :checks []}

    :fn-decl
    {:bindings [(core/local-symbol (:name d) ctx) (local-fn-form d ctx)]
     :checks []}

    :pattern-var-decl-stmt
    (let [subj (symbol "d2c$pat")
          alts (pattern/alternatives (:pattern (:decl d)) subj ctx
                                     {:structural-tests? false})]
      (when (not= 1 (count alts))
        (fail! d "or-patterns are not supported in variable declarations"))
      (let [{:keys [tests binds]} (first alts)]
        {:bindings (into [subj (emit-expr (:expr (:decl d)) ctx)]
                         (pattern-bind-vec binds ctx))
         :checks (map #(list 'assert %) tests)}))))

(defn- decl-run-form [decls rest-forms ctx]
  (let [parts (map #(decl->parts % ctx) decls)
        bindings (vec (mapcat :bindings parts))
        checks (vec (mapcat :checks parts))
        body (concat checks rest-forms)]
    (if (seq body)
      (list* 'let bindings body)
      (list 'let bindings nil))))

;; ---------------------------------------------------------------------------
;; direct mode
;; ---------------------------------------------------------------------------

(defn- emit-direct [stmts ctx]
  (loop [ss (seq stmts) out []]
    (if-not ss
      ;; Dart discards the value of a trailing expression statement; ClojureDart
      ;; writes the last form of a *value* position as `return <form>;`, so
      ;; append nil. This applies at every value tail, not only function
      ;; bodies (`return_of_invalid_type` for a void call in an async closure).
      (if (and (:value-tail? ctx) (seq stmts) (= :expr-stmt (:ir (last stmts))))
        (conj out nil)
        out)
      (let [s (first ss)
            last? (nil? (next ss))
            sctx (assoc ctx
                        :value-tail? (and last? (boolean (:value-tail? ctx))))]
        (case (:ir s)
          (:var-decl :fn-decl :pattern-var-decl-stmt)
          (let [[decls more] (split-with decl? ss)]
            (conj out (decl-run-form decls (emit-direct more ctx) ctx)))

          :return
          (if last?
            (conj out (when (:expr s) (emit-expr (:expr s) ctx)))
            (fail! s "early return is not supported here"))

          (recur (next ss) (conj out (emit-stmt s sctx))))))))

;; ---------------------------------------------------------------------------
;; guarded mode
;; ---------------------------------------------------------------------------

(defn- emit-guarded-seq [stmts ctx]
  (if (empty? stmts)
    nil
    (let [s (first stmts)
          rest-stmts (next stmts)
          g (guard-test ctx)
          guard #(if g (list 'when g %) %)
          recur-seq #(emit-guarded-seq % ctx)]
      (case (:ir s)
        (:var-decl :fn-decl :pattern-var-decl-stmt)
        (let [[decls more] (split-with decl? stmts)
              form (decl-run-form decls (when (seq more) [(recur-seq more)]) ctx)]
          (guard form))

        :return
        (let [sets (guard (list 'do
                                (list 'vreset! (:result ctx)
                                      (when (:expr s) (emit-expr (:expr s) ctx)))
                                (list 'vreset! (:done ctx) true)))]
          (if-let [inner (recur-seq rest-stmts)]
            (list 'do sets inner)
            sets))

        :break
        (let [sym (if-let [l (:label s)]
                    (get (:break-syms ctx) l)
                    (:break-sym ctx))]
          (when-not sym (fail! s "break outside a supported loop"))
          (let [form (guard (list 'vreset! sym true))]
            (if-let [inner (recur-seq rest-stmts)]
              (list 'do form inner)
              form)))

        :continue
        (let [sym (if-let [l (:label s)]
                    (get (:continue-syms ctx) l)
                    (:continue-sym ctx))]
          (when-not sym (fail! s "continue outside a supported loop"))
          (let [form (guard (list 'vreset! sym true))]
            (if-let [inner (recur-seq rest-stmts)]
              (list 'do form inner)
              form)))

        (let [form (guard (emit-stmt s (assoc ctx :value-tail? false)))]
          (if-let [inner (recur-seq rest-stmts)]
            (list 'do form inner)
            form))))))

(defn- emit-body-forms [stmts ctx]
  (if (seq (:guards ctx))
    [(or (emit-guarded-seq stmts ctx) nil)]
    (emit-direct stmts ctx)))

(defn emit-body
  "Dart statements -> vector of cljd forms (function/loop/block bodies)."
  [stmts ctx]
  (if (:fn-body? ctx)
    (let [has-ret (boolean (some contains-return? stmts))]
      (if (and has-ret (not (all-returns-tail? stmts)))
        (let [result 'd2c$result
              done 'd2c$done
              ctx' (assoc ctx
                          :result result
                          :done done
                          :guards (conj (vec (:guards ctx)) done))]
          [(list 'let [result (list 'volatile! nil)
                       done (list 'volatile! false)]
                 (or (emit-guarded-seq stmts ctx') nil)
                 (list 'deref result))])
        (emit-direct stmts (assoc ctx :value-tail? true))))
    (emit-body-forms stmts ctx)))

;; ---------------------------------------------------------------------------
;; if / pattern matching
;; ---------------------------------------------------------------------------

(defn- match-form
  "Form evaluating `then` when pattern `p` matches `subject`, else `else`."
  [p subject ctx then else]
  (let [alts (pattern/alternatives p subject ctx)
        guard (pattern/guard-expr p)
        alt-form (fn [a]
                   (let [tests (:tests a)
                         binds (:binds a)
                         inner (if guard
                                 (list 'if (emit-expr guard ctx) then else)
                                 then)
                         inner (if (seq binds)
                                 (list 'let (pattern-bind-vec binds ctx) inner)
                                 inner)]
                     (if (seq tests)
                       (list 'if (list* 'and tests) inner else)
                       inner)))]
    (if (= 1 (count alts))
      (alt-form (first alts))
      (list* 'or (concat (map alt-form alts) [else])))))

(defn- branch-form
  "Emit one if/else branch statement in `ctx`. A *bare* expression statement
  in a value tail appends nil: Dart discards its value, while ClojureDart
  writes the branch's last form as `return <form>;`. (Block branches are
  handled by `emit-direct`, which appends the same nil.)"
  [st ctx]
  (if (seq (:guards ctx))
    (or (emit-guarded-seq (stmt-stmts st) ctx) nil)
    (let [form (emit-stmt st ctx)]
      (if (and (:value-tail? ctx) (= :expr-stmt (:ir st)))
        (list 'do form nil)
        form))))

(defn- emit-if-case [s ctx]
  (let [p (:pattern (:case s))
        subj (symbol "d2c$subj")
        then (branch-form (:then s) ctx)
        else (when (:else s)
               (branch-form (:else s) ctx))]
    (list 'let [subj (emit-expr (:cond s) ctx)]
          (match-form p subj ctx then else))))

(defn- emit-if [s ctx]
  (let [c (emit-expr (:cond s) ctx)
        then-s (:then s)
        els (:else s)
        ;; A branch inherits the if's own tail position: in a value tail its
        ;; last form becomes the returned value.
        branch (fn [st] (branch-form st ctx))]
    (cond
      (:case s) (emit-if-case s ctx)

      (seq (:guards ctx))
      (cond
        (nil? els) (list 'when c (branch then-s))
        (= :if (:ir els))
        (let [clauses (loop [n els acc [c (branch then-s)]]
                        (let [c2 (emit-expr (:cond n) ctx)
                              t2 (branch (:then n))
                              acc (conj acc c2 t2)]
                          (if-let [e (:else n)]
                            (if (= :if (:ir e))
                              (recur e acc)
                              (conj acc :else (branch e)))
                            acc)))]
          (list* 'cond clauses))
        :else (list 'if c (branch then-s) (branch els)))

      :else
      (cond
        (nil? els) (list 'when c (branch then-s))
        (= :if (:ir els))
        (let [clauses (loop [n els acc [c (branch then-s)]]
                        (let [c2 (emit-expr (:cond n) ctx)
                              t2 (branch (:then n))
                              acc (conj acc c2 t2)]
                          (if-let [e (:else n)]
                            (if (= :if (:ir e))
                              (recur e acc)
                              (conj acc :else (branch e)))
                            acc)))]
          (list* 'cond clauses))
        :else (list 'if c (branch then-s) (branch els))))))

;; ---------------------------------------------------------------------------
;; loops
;; ---------------------------------------------------------------------------

(defn- merge-jumps [a b] (merge-with #(or %1 %2) a b))

(defn- jump-info
  "Find break/continue statements that target the loop being emitted.
  `nested?` is true inside a nested loop, where unlabeled jumps target that
  loop instead. Jumps inside nested functions never escape."
  [node labels nested?]
  (cond
    (map? node)
    (case (:ir node)
      (:fn :fn-decl) {}
      :break (if (or (and (nil? (:label node)) (not nested?))
                     (contains? labels (:label node)))
               {:break? true} {})
      :continue (if (or (and (nil? (:label node)) (not nested?))
                        (contains? labels (:label node)))
                  {:continue? true} {})
      (:for :while :do-while) (jump-info (:body node) labels true)
      :labeled (jump-info (:stmt node) (into labels (:labels node)) nested?)
      (reduce (fn [acc v] (merge-jumps acc (jump-info v labels nested?)))
              {} (vals (dissoc node :ir :src :annotations))))
    (coll? node) (reduce (fn [acc v] (merge-jumps acc (jump-info v labels nested?)))
                         {} node)
    :else {}))

(defn- flag-sym [base labels]
  (symbol (str "d2c$" base (when (seq labels) (str "$" (first (sort labels)))))))

(defn- loop-ctx
  "Extend ctx with the guard flags needed by the loop body."
  [s ctx labels]
  (let [{:keys [break? continue?]} (jump-info (:body s) labels false)
        brk (when break? (flag-sym "brk" labels))
        cont (when continue? (flag-sym "cont" labels))
        guards (vec (concat (:guards ctx) (when brk [brk]) (when cont [cont])))]
    {:ctx (assoc ctx
                 :guards guards
                 :break-sym brk
                 :continue-sym cont
                 :break-syms (into (or (:break-syms ctx) {})
                                   (for [l labels] [l brk]))
                 :continue-syms (into (or (:continue-syms ctx) {})
                                      (for [l labels] [l cont])))
     :brk brk
     :cont cont}))

(defn- loop-wrapper [flags form]
  (if (seq flags)
    (list 'let (vec (mapcat (fn [f] [f (list 'volatile! false)]) flags)) form)
    form))

(defn- emit-loop-body [body ctx]
  ;; A loop body's value is discarded (the loop yields the `recur` value), so
  ;; a trailing expression statement needs no nil.
  (do-form (emit-body-forms (stmt-stmts body) (assoc ctx :value-tail? false))))

(defn- recur-form [ctx cont]
  (let [gs (remove #{cont} (:guards ctx))
        g (guards-form gs)]
    (if g (list 'when g (list 'recur)) (list 'recur))))

(defn- emit-while [s ctx labels]
  (let [{:keys [ctx brk cont]} (loop-ctx s ctx labels)
        body (emit-loop-body (:body s) ctx)
        cond (emit-expr (:cond s) ctx)
        form (list* 'loop []
                    (concat (when cont [(list 'vreset! cont false)])
                            [(list 'when (and-form (guards-form (:guards ctx)) cond)
                                   body
                                   (recur-form ctx cont))]))]
    (loop-wrapper (keep identity [brk cont]) form)))

(defn- emit-do-while [s ctx labels]
  (let [{:keys [ctx brk cont]} (loop-ctx s ctx labels)
        body (emit-loop-body (:body s) ctx)
        cond (emit-expr (:cond s) ctx)
        recur-guards (vec (remove #{cont} (:guards ctx)))
        form (list* 'loop []
                    (concat (when cont [(list 'vreset! cont false)])
                            [body
                             (list 'when (and-form (guards-form recur-guards) cond)
                                   (list 'recur))]))]
    (loop-wrapper (keep identity [brk cont]) form)))

(defn- updater-new-value
  "For a C-style for loop: [name new-value-form] for an updater on a loop
  variable, or :expression when the updater is a plain expression."
  [u loop-names ctx]
  (let [target (case (:ir u)
                 (:postfix :prefix) (:operand u)
                 :assign (:lhs u)
                 nil)]
    (cond
      (and (= :id (:ir target)) (contains? loop-names (:name target)))
      (let [n (:name target)]
        [n (case (:ir u)
             :postfix (list (if (= "++" (:op u)) 'inc 'dec) (symbol n))
             :prefix (list (if (= "++" (:op u)) 'inc 'dec) (symbol n))
             :assign (case (:op u)
                       "=" (emit-expr (:rhs u) ctx)
                       "??" (list 'if (list 'nil? (symbol n))
                                  (emit-expr (:rhs u) ctx)
                                  (symbol n))
                       "&&" (list 'and (symbol n) (emit-expr (:rhs u) ctx))
                       "||" (list 'or (symbol n) (emit-expr (:rhs u) ctx))
                       (list (core/compound-fn (:op u)) (symbol n)
                             (emit-expr (:rhs u) ctx))))])

      (empty? loop-names)
      :expression

      :else (fail! u "for-loop updater must assign a loop variable"))))

(defn- emit-for-parts [s ctx labels]
  (let [loop (:loop s)
        init (:init loop)
        decl-vars (when (and init (= :var-decl (:ir init))) (:vars init))]
    (when (and init (not= :var-decl (:ir init)))
      (fail! init "for-loop initializer must be a declaration"))
    (when (and init (nil? decl-vars))
      (fail! init "for-loop pattern declarations are not supported"))
    (let [loop-names (set (map :name decl-vars))
          ctx (-> ctx
                  (update :mutable #(apply disj % loop-names))
                  (update :captured #(apply disj % loop-names)))
          _ (doseq [v decl-vars]
              (when (mutable/assigns? (:body s) (:name v))
                (fail! (:body s) (str "assignment to for-loop variable "
                                      (:name v) " is not supported"))))
          {:keys [ctx brk cont]} (loop-ctx s ctx labels)
          bindings (vec (mapcat (fn [v]
                                  [(core/loop-var-symbol v ctx)
                                   (when (:init v) (emit-expr (:init v) ctx))])
                                decl-vars))
          ;; Each loop variable's new value comes from *its own* updater;
          ;; updaters that are plain expressions (only possible without
          ;; declared variables) are evaluated for effect before the recur.
          per-var (reduce (fn [m u]
                            (let [r (updater-new-value u loop-names ctx)]
                              (if (vector? r)
                                (assoc m (first r) (second r))
                                m)))
                          {}
                          (:updaters loop))
          new-vals (map (fn [v]
                          (get per-var (:name v) (core/local-symbol (:name v) ctx)))
                        decl-vars)
          updaters (keep (fn [u]
                           (let [r (updater-new-value u loop-names ctx)]
                             (when (= :expression r) (emit-expr u ctx))))
                         (:updaters loop))
          body (emit-loop-body (:body s) ctx)
          cond (when (:cond loop) (emit-expr (:cond loop) ctx))
          recur-body (if (seq decl-vars)
                       (list* 'recur new-vals)
                       (do-form (concat updaters [(list 'recur)])))
          recur-form (if-let [g (guards-form (remove #{cont} (:guards ctx)))]
                       (list 'when g recur-body)
                       recur-body)
          form (list* 'loop bindings
                      (concat (when cont [(list 'vreset! cont false)])
                              [(list 'when (and-form (guards-form (:guards ctx)) cond)
                                     body
                                     recur-form)]))]
      (loop-wrapper (keep identity [brk cont]) form))))

(defn- emit-for-in [s ctx labels]
  (let [loop (:loop s)
        v (:var loop)]
    (when (nil? v)
      (fail! loop "pattern for-in loops are not supported"))
    (let [name (:name v)
          ctx (-> ctx
                  (update :mutable disj name)
                  (update :captured disj name))
          _ (when (mutable/assigns? (:body s) name)
              (fail! (:body s) (str "assignment to for-in variable " name
                                    " is not supported")))
          {:keys [break? continue?]} (jump-info (:body s) labels false)
          iterable (emit-expr (:iterable loop) ctx)]
      (if (and (empty? (:guards ctx)) (not break?) (not continue?))
        (list 'doseq [(core/local-symbol name ctx) iterable]
              (do-form (emit-body-forms (stmt-stmts (:body s))
                                        (assoc ctx :value-tail? false))))
        (let [{:keys [ctx brk cont]} (loop-ctx s ctx labels)
              seq-sym (symbol "d2c$seq")
              body (emit-loop-body (:body s) (assoc ctx :guards (:guards ctx)))
              form (list* 'loop [seq-sym (list 'seq iterable)]
                          (concat (when cont [(list 'vreset! cont false)])
                                  [(list 'when (and-form (guards-form (:guards ctx)) seq-sym)
                                         (list 'let [(core/local-symbol name ctx)
                                                     (list 'first seq-sym)]
                                               body
                                               (list 'when (guards-form (remove #{cont} (:guards ctx)))
                                                     (list 'recur (list 'next seq-sym)))))]))]
          (loop-wrapper (keep identity [brk cont]) form))))))

(defn- loop-stmt? [s] (contains? #{:for :while :do-while} (:ir s)))

(defn- emit-loop [s ctx labels]
  (case (:ir s)
    :for (case (:ir (:loop s))
           :for-in (emit-for-in s ctx labels)
           :for-parts (emit-for-parts s ctx labels)
           (fail! s (str "unsupported for loop " (:ir (:loop s)))))
    :while (emit-while s ctx labels)
    :do-while (emit-do-while s ctx labels)
    (fail! s (str "not a loop: " (:ir s)))))

;; ---------------------------------------------------------------------------
;; switch
;; ---------------------------------------------------------------------------

(defn- unguard [p]
  (if (and (map? p) (= :guarded-pattern (:ir p)))
    [(:pattern p) (:when p)]
    [p nil]))

(defn- literal? [l]
  (or (number? l) (string? l) (boolean? l) (nil? l) (keyword? l)))

(defn- case-terminator
  "Strip a trailing switch-terminator `break`. Pattern cases never fall
  through, so they do not require a terminator."
  [stmts _ctx {:keys [last? pattern-case?]}]
  (let [v (vec stmts)
        last-s (peek v)]
    (cond
      (nil? last-s) v
      (= :break (:ir last-s))
      (if (:label last-s)
        (fail! last-s "labeled break as switch terminator is not supported")
        (pop v))
      (contains? #{:return :throw :rethrow :continue} (:ir last-s)) v
      (or last? pattern-case?) v
      :else (fail! last-s "switch case must end with break/return/throw/continue"))))

(defn- find-switch-level-break
  "First unlabeled `break` whose target is the switch being emitted (breaks
  inside nested loops/switches/functions target those), or nil."
  [node]
  (let [found (atom nil)]
    (letfn [(scan [n]
              (when-not @found
                (cond
                  (map? n)
                  (case (:ir n)
                    (:fn :fn-decl) nil
                    (:for :while :do-while :switch-stmt) nil
                    :break (when (nil? (:label n)) (reset! found n))
                    (doseq [v (vals (dissoc n :ir :src :annotations))] (scan v)))
                  (coll? n) (doseq [v n] (scan v)))))]
      (scan node)
      @found)))

(defn- switch-body [stmts ctx opts]
  (let [stmts (case-terminator stmts ctx opts)]
    (when-let [b (find-switch-level-break stmts)]
      (fail! b "break inside a switch case is only supported as the trailing terminator"))
    (do-form (emit-body-forms stmts ctx))))

(defn- switch-clauses
  "Expand switch cases into {:kind :literal|:pattern ...} clauses plus a
  default body. Empty cases attach their labels to the next non-empty case."
  [s ctx]
  (loop [cs (seq (:cases s)) pending [] out [] default nil]
    (if-not cs
      {:clauses (into out (map (fn [l] {:kind :literal :label l :body nil}))
                      pending)
       :default default}
      (let [c (first cs)
            ir (:ir c)]
        (cond
          (contains? #{:switch-case :switch-pattern-case} ir)
          (let [stmts (:stmts c)
                pattern (when (= :switch-pattern-case ir) (:pattern c))
                [p w] (unguard pattern)
                label (if (= :switch-case ir)
                        (emit-expr (:expr c) ctx)
                        (when (= :const-pattern (:ir p))
                          (emit-expr (:expr p) ctx)))
                last? (nil? (next cs))
                body-opts {:last? last? :pattern-case? (= :switch-pattern-case ir)}
                _ (when (and w (empty? stmts))
                    (fail! c "guarded empty switch cases cannot share a body"))]
            (cond
              (empty? stmts)
              (recur (next cs) (conj pending label) out default)

              (= :switch-case ir)
              (recur (next cs) []
                     (into out (for [l (conj pending label)]
                                 {:kind :literal :label l
                                    :body (switch-body stmts ctx body-opts)}))
                     default)

              (and (= :const-pattern (:ir p)) (nil? w))
              (recur (next cs) []
                     (into out (for [l (conj pending label)]
                                 {:kind :literal :label l
                                    :body (switch-body stmts ctx body-opts)}))
                     default)

              :else
              (let [alts (pattern/alternatives p (symbol "d2c$subj") ctx)
                    body (switch-body stmts ctx body-opts)]
                (recur (next cs) []
                       (into out (map (fn [a]
                                        {:kind :pattern
                                         :tests (:tests a)
                                         :binds (:binds a)
                                         :guard w
                                         :body body})
                                      alts))
                       default))))

          (= :switch-default ir)
          (recur (next cs) [] out (switch-body (:stmts c) ctx {:last? (nil? (next cs))}))

          :else
          (fail! c (str "unsupported switch case " ir)))))))

(defn- emit-switch [s ctx]
  (let [{:keys [clauses default]} (switch-clauses s ctx)
        literals? (every? #(= :literal (:kind %)) clauses)
        subject (emit-expr (:expr s) ctx)
        ;; A switch statement without a default that is the tail of a
        ;; non-void function body: Dart accepts it only when the cases are
        ;; exhaustive (enum switches), where the fallback is unreachable —
        ;; an implicit nil would not type-check, so emit a throwing stub
        ;; instead of `nil` (M8.16).
        fallback (if (and (nil? default) (:value-tail? ctx) (:non-void-body? ctx))
                   (list 'throw (list 'UnsupportedError. "unreachable switch"))
                   default)]
    (if (and literals? (every? #(literal? (:label %)) clauses))
      (list* 'case subject
             (concat (mapcat (fn [{:keys [label body]}] [label body]) clauses)
                     [fallback]))
      (let [subj (symbol "d2c$subj")
            clauses' (map (fn [{:keys [kind label tests binds guard body]}]
                            (case kind
                              :literal [(list '= subj label) body]
                              :pattern
                              (let [test (if (seq tests)
                                           (list* 'and tests)
                                           true)
                                    inner (if guard
                                            (list 'if (emit-expr guard ctx) body nil)
                                            body)
                                    inner (if (seq binds)
                                            (list 'let (pattern-bind-vec binds ctx) inner)
                                            inner)]
                                [(if (= true test) :else test) inner])))
                          clauses)]
        (list 'let [subj subject]
              (if (seq clauses')
                (list* 'cond (concat (mapcat identity clauses')
                                     (when fallback [:else fallback])))
                fallback))))))

;; ---------------------------------------------------------------------------
;; try / catch
;; ---------------------------------------------------------------------------

(defn- emit-catch [c ctx]
  ;; A bare Dart `catch` becomes `on Object` (not `on dynamic`): Dart can only
  ;; throw non-null objects, and `dynamic` in an `on` clause is a
  ;; `nullable_type_in_catch_clause` warning.
  (let [ty (if (:exception-type c)
             (core/type-symbol (:exception-type c) ctx)
             'Object)
        e (core/local-symbol (or (:exception-var c) "_") ctx)
        st (when (:stack-var c) (core/local-symbol (:stack-var c) ctx))
        ctx' (assoc ctx :catch-exception e)
        body (emit-body-forms (:stmts (:body c)) ctx')]
    (list* 'catch ty e (concat (when st [st]) body))))

(defn- emit-try [s ctx]
  (let [body (emit-body-forms (:stmts (:body s)) ctx)
        catches (map #(emit-catch % ctx) (:catches s))
        fin (:finally s)]
    (list* 'try
           (concat body catches
                   (when fin
                     ;; A finally block's value is discarded.
                     [(list* 'finally
                             (emit-body-forms (:stmts fin)
                                              (assoc ctx :value-tail? false)))])))))

;; ---------------------------------------------------------------------------
;; pattern statements
;; ---------------------------------------------------------------------------

(defn- emit-pattern-assign [s ctx]
  (let [subj (symbol "d2c$pat")
        alts (pattern/alternatives (:pattern s) subj ctx
                                   {:structural-tests? false})]
    (when (not= 1 (count alts))
      (fail! s "or-patterns are not supported in assignments"))
    (let [{:keys [tests binds]} (first alts)]
      (list 'let (into [subj (emit-expr (:expr s) ctx)] [])
            (do-form
             (concat
              (map (fn [t] (list 'assert t)) tests)
              (map (fn [[n path]]
                     (core/assign-write {:ir :id :name (name n) :src (:src s)}
                                        "=" path ctx))
                   binds)))))))

;; ---------------------------------------------------------------------------
;; switch expressions
;; ---------------------------------------------------------------------------

(defn emit-switch-expr [node ctx]
  (let [subject (emit-expr (:expr node) ctx)
        subj (symbol "d2c$subj")]
    (list
     'let [subj subject]
     (list* 'cond
            (mapcat (fn [c]
                      (let [[p w] (unguard (:pattern c))
                            alts (pattern/alternatives p subj ctx)]
                        (mapcat (fn [a]
                                  (let [tests (:tests a)
                                        binds (:binds a)
                                        test (if (seq tests) (list* 'and tests) true)
                                        body (emit-expr (:expr c) ctx)
                                        inner (if w
                                                (list 'if (emit-expr w ctx) body nil)
                                                body)
                                        inner (if (seq binds)
                                                (list 'let (pattern-bind-vec binds ctx) inner)
                                                inner)]
                                    [(if (= true test) :else test) inner]))
                                alts)))
                    (:cases node))))))

;; ---------------------------------------------------------------------------
;; statement dispatch
;; ---------------------------------------------------------------------------

(defn- labeled-loop [s ctx]
  (let [labels (loop [n s acc #{}]
                 (if (= :labeled (:ir n))
                   (recur (:stmt n) (into acc (:labels n)))
                   [acc n]))
        [labels inner] labels]
    (when-not (loop-stmt? inner)
      (fail! s "labels are only supported on loops"))
    (emit-loop inner ctx labels)))

(defn emit-stmt
  "Emit one IR statement as a cljd form."
  [s ctx]
  (case (:ir s)
    :block (do-form (emit-body-forms (:stmts s) ctx))
    :expr-stmt
    (if (= :pattern-assign (:ir (:expr s)))
      (emit-pattern-assign (:expr s) ctx)
      (core/emit-expr-stmt (:expr s) ctx))
    :var-decl (first (emit-body [s] (assoc ctx :fn-body? false)))
    :fn-decl (first (emit-body [s] (assoc ctx :fn-body? false)))
    :pattern-var-decl-stmt (first (emit-body [s] (assoc ctx :fn-body? false)))
    :if (emit-if s ctx)
    :for (emit-loop s ctx #{})
    :while (emit-loop s ctx #{})
    :do-while (emit-loop s ctx #{})
    :labeled (labeled-loop s ctx)
    :switch-stmt (emit-switch s ctx)
    :try (emit-try s ctx)
    :return (if (:value-tail? ctx)
              (when (:expr s) (emit-expr (:expr s) ctx))
              (fail! s "return outside of a function body"))
    :throw (list 'throw (emit-expr (:expr s) ctx))
    :rethrow (if-let [e (:catch-exception ctx)]
               (list 'throw e)
               (fail! s "rethrow outside of a catch clause"))
    :assert-stmt (if (:message s)
                   (list 'assert (emit-expr (:cond s) ctx)
                         (emit-expr (:message s) ctx))
                   (list 'assert (emit-expr (:cond s) ctx)))
    :pattern-assign (emit-pattern-assign s ctx)
    :empty-stmt nil
    :break (fail! s "break outside a supported loop or switch")
    :continue (fail! s "continue outside a supported loop")
    :yield (fail! s "generators (yield/async*/sync*) are not supported by ClojureDart")
    (fail! s (str "unsupported statement " (:ir s)))))
