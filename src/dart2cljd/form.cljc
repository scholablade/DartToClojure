(ns dart2cljd.form
  "Form constructors for the emitter.

  Emitted code is plain Clojure data (lists, vectors, symbols, strings,
  numbers) plus the records below, which preserve information that plain
  data would lose:

  * MapLit — ordered map literal (plain Clojure maps are unordered for >8
    entries, which would break deterministic output and Dart source order).
  * SetLit — ordered set literal, same reason.
  * Hint  — `^tag form` type hints (`(hint 'String x)` -> `^String x`).
  * RecordLit — Dart record literal: `#dart (1 2 .bar \"hey\")`.

  All constructors are pure; equality is structural (records compare by
  value), so emitted forms can be compared directly in tests.")

(defrecord MapLit [pairs])
(defrecord SetLit [elems])
(defrecord Hint [tag form])
(defrecord RecordLit [fields])

(defn map-lit? [x] (instance? MapLit x))
(defn set-lit? [x] (instance? SetLit x))
(defn hint? [x] (instance? Hint x))
(defn record-lit? [x] (instance? RecordLit x))

(defn map-lit
  "Ordered map literal from alternating key/value arguments."
  [& kvs]
  (->MapLit (vec (partition 2 kvs))))

(defn map-lit*
  "Ordered map literal from a sequence of [k v] pairs."
  [pairs]
  (->MapLit (vec pairs)))

(defn set-lit
  "Ordered set literal."
  [& elems]
  (->SetLit (vec elems)))

(defn hint
  "Type-hinted form: `(hint 'String x)` renders as `^String x`."
  [tag form]
  (->Hint tag form))

(defn record-lit
  "Dart record literal. `fields` is a sequence of [name-or-nil value] pairs
  in source order; nil name means a positional field."
  [fields]
  (->RecordLit (vec fields)))
