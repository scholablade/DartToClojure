(ns dart2cljd.idiomatic-test
  "M4 idiomatic mode: widget classes -> builder fns, State -> atoms +
  f/widget :watch, and f/widget threading of .child chains."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [dart2cljd.alias :as alias]
            [dart2cljd.api :as api]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.idiomatic :as idiomatic]
            [dart2cljd.printer :as printer]))

(def ^:private doc
  (delay (ast/normalize-doc
          (edn/read-string (slurp (io/resource "fixtures/idiomatic.edn"))))))

(def ^:private forms
  (delay (decls/emit-unit (:unit @doc) {:path "corpus/idiomatic.dart" :mode :idiomatic})))

(def ^:private faithful-forms
  (delay (decls/emit-unit (:unit @doc) {:path "corpus/idiomatic.dart"})))

(defn- out [] (printer/print-forms @forms))
(defn- faithful-out [] (printer/print-forms @faithful-forms))

;; ---------------------------------------------------------------------------
;; golden (regenerate: make golden)
;; ---------------------------------------------------------------------------

(deftest idiomatic-golden
  (testing "the reviewed idiomatic translation is byte-stable"
    (is (= (slurp (io/file "corpus/idiomatic.cljd")) (str (out) "\n")))))

;; ---------------------------------------------------------------------------
;; class mapping
;; ---------------------------------------------------------------------------

(deftest widget-classes-become-fns
  (let [o (out)]
    (testing "a StatelessWidget becomes a builder fn; key stays a parameter"
      (is (re-find #"\(defn Greeting \[\.key nil \^String \.name\]" o))
      (is (not (re-find #"deftype Greeting" o))))
    (testing "a StatefulWidget + State pair becomes one fn"
      (is (re-find #"\(defn Counter \[\.key nil \^String \.title\]" o))
      (is (not (re-find #"deftype Counter|deftype CounterState" o)))
      (is (re-find #"\(defn Banner \[\]" o)))
    (testing "widget classes outside the recognized shapes stay faithful"
      (is (re-find #"\(deftype Legacy" o))
      (is (re-find #"\(deftype Plain" o)))))

(deftest key-params-are-threaded
  (let [o (out)]
    (testing "a keyed widget wraps its body in KeyedSubtree"
      (is (re-find #"\(m/KeyedSubtree\s+\.key key" o)))
    (testing "a widget without a key constructor param is not wrapped"
      (is (re-find #"(?s)\(defn Banner \[\]\s+\(f/widget :context context \(m/Center" o)))
    (testing "named key args survive at call sites"
      (is (re-find #"\.home \(Counter \.key nil \.title \"Counter\"\)" o)))))

(deftest state-mapping
  (let [o (out)]
    (testing "state fields are atoms bound in a let"
      (is (re-find #"\(let \[count \(atom 0\)\s+step \(atom 1\)" o)))
    (testing "every watched atom subscribes the widget"
      (is (re-find #":watch \[_ count _ step _ d2c\$tick\]" o)))
    (testing "State methods become local fns"
      (is (re-find #"\(increment \[\] \(do \(swap! count \+ \(deref step\)\) \(swap! d2c\$tick inc\)\) nil\)" o))
      (is (re-find #"\(doubled \[\] \(\* \(deref count\) 2\)\)" o))
      (is (re-find #"\(doubled\)" o)))
    (testing "async State methods keep ^:async"
      (is (re-find #"(?s)\^:async \(ping\s+\[\]\s+\(await" o)))
    (testing "setState callbacks are evaluated inline"
      (is (re-find #"\(fn \[\] \(do \(swap! count \+ 2\) \(swap! d2c\$tick inc\)\)\)" o)))
    (testing "widget.<field> resolves to the builder fn parameter"
      (is (re-find #"\(m/Text title\)" o))
      (is (not (re-find #"\.-title widget" o))))))

(deftest set-state-tick
  (let [o (out)]
    (testing "a setState writing no state atom still bumps the tick"
      (is (re-find #"\(bump \[\] \(do \(\.write \(deref buf\) \"x\"\) \(swap! d2c\$tick inc\)\) nil\)" o)))
    (testing "the tick atom is watched"
      (is (re-find #":watch \[_ buf _ d2c\$tick\]" o)))))

;; ---------------------------------------------------------------------------
;; f/widget threading
;; ---------------------------------------------------------------------------

(deftest widget-threading
  (let [o (out)]
    (testing "a .child chain of 3+ forms becomes one f/widget form"
      (is (re-find #"(?s)\(f/widget\s+:context context\s+\(m/Padding \.padding \(m/EdgeInsets\.all 8\)\)\s+m/Center"
                   o)))
    (testing "threading continues through .body"
      (is (re-find #"(?s)\.body m/Center\s+\(m/Text \(str \"Count: \"" o)))))

(deftest short-chains-are-not-threaded
  (testing "two-form .child chains keep the faithful shape"
    (is (re-find #"\(m/Center \.child \(m/Text \"banner\"\)\)" (out)))))

;; ---------------------------------------------------------------------------
;; operators and mode separation
;; ---------------------------------------------------------------------------

(deftest idiomatic-percent
  (testing "Dart % maps to cljd mod in idiomatic mode"
    (is (re-find #"\(mod a b\)" (out))))
  (testing "faithful mode keeps the Dart % operator"
    (is (re-find #"\(\. a % b\)" (faithful-out)))))

(deftest faithful-mode-unchanged
  (let [o (faithful-out)]
    (is (re-find #"deftype Greeting" o))
    (is (not (re-find #"f/widget" o)))
    (is (not (re-find #"cljd\.flutter" o)))))

;; ---------------------------------------------------------------------------
;; plan / alias details
;; ---------------------------------------------------------------------------

(deftest string-state-fields
  (let [o (out)]
    (testing "a String state field is a watched atom and concatenates via str (M10)"
      (is (re-find #"\(defn Typer \[\.key nil\]" o))
      (is (re-find #"\(swap! text str \"b\"\)" o))
      (is (re-find #"\(m/Text \(str \(deref text\) \"!\"\)\)" o))
      (is (re-find #":watch \[_ text _ d2c\$tick\]" o)))))

(deftest plan-consumes-only-paired-state-classes
  (let [p (idiomatic/plan (:unit @doc))]
    (is (= #{"Greeting" "Counter" "Banner" "Ticker" "Typer" "Editor" "Fader" "Page"}
           (set (keys (:widgets p)))))
    (is (= #{"CounterState" "BannerState" "TickerState" "TyperState"
             "EditorState" "FaderState"}
           (:consumed p)))))

(deftest managed-lifecycle-resources
  (let [o (out)]
    (testing "initState-created fields become :managed resources created once"
      (is (re-find #":managed \[controller\s+\(m/TextEditingController \.text \"hi\"\)\s+:refresh-on :never\s+:dispose \.dispose\]" o)))
    (testing "vsync: this maps to the :vsync directive binding"
      (is (re-find #":vsync d2c\$vsync" o))
      (is (re-find #"(?s)\(m/AnimationController\s+\.vsync d2c\$vsync" o)))
    (testing "managed fields are read in build as plain locals"
      (is (re-find #"\(m/TextField \.controller controller\)" o))
      (is (re-find #"\(m/FadeTransition \.opacity controller" o)))
    (testing "recognized lifecycle members are not emitted as methods"
      (is (not (re-find #"initState|super" o))))))

(deftest no-sugar-without-flutter
  (testing "a StatefulWidget without a paired State class falls back"
    (is (empty? (:widgets (idiomatic/plan
                           {:decls [{:ir :class :name "Orphan" :src {:line 1 :col 1}
                                     :abstract false :modifier nil :type-params []
                                     :with [] :implements []
                                     :extends {:ir :named-type :prefix nil
                                               :name "StatefulWidget"
                                               :type-args [] :nullable false}
                                     :members []}]}))))))

(deftest flutter-alias-fallbacks
  (testing "cljd.flutter keeps an existing alias or takes the first free one"
    (is (= "f" (alias/flutter-alias (alias/context []))))
    (is (= "fl" (alias/flutter-alias
                 (assoc (alias/context []) :prefix->uri {"f" "dart:math"}))))))

;; ---------------------------------------------------------------------------
;; end-to-end (needs the Dart SDK; cached under .dart2cljd-cache)
;; ---------------------------------------------------------------------------

(def ^:private dart-bin
  (or (System/getenv "DART2CLJD_DART")
      "/home/Schola/Projects/develop/flutter/bin/dart"))

(deftest ^:integration snippet-threading
  (if (.exists (io/file dart-bin))
    (is (= "(f/widget m/Foo m/Bar m/Baz (m/Qux))"
           (api/convert "Foo(child: Bar(child: Baz(child: Qux())))"
                        :format :string :mode :idiomatic :dart dart-bin)))
    (is true "dart binary not present; skipped")))

(deftest ^:integration snippet-percent-idiomatic
  (if (.exists (io/file dart-bin))
    (is (= "(mod a b)"
           (api/convert "a % b" :format :string :mode :idiomatic :dart dart-bin)))
    (is true "dart binary not present; skipped")))
