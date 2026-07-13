(ns hive-test.sandbox
  "Disposable on-disk project trees for tests.

   A sandbox SPEC is a plain map {rel-path content}:
     string  -> file written verbatim (parent dirs auto-created)
     fn      -> called with {:dir <abs root> :path <rel-path>}; its return
                value is written as the file body (the plug point for
                generated / parameterized fixtures)
     nil     -> empty directory

   Levers:
     sandbox!      spec [opts]     -> handle {:dir :paths :file :destroy!}
     destroy!      handle          -> recursive delete (handle-gated: only
                                      deletes a root this ns created)
     with-sandbox  [sym spec opts?] & body -> scoped create/always-destroy
     *sandbox*                     -> handle bound by the :sandbox isolation

   Plugs into hive-test.isolation:
     (use-fixtures :each (isolation/with-isolations
                           {:type :sandbox :spec {\"src/a.clj\" \"(ns a)\"}}))
   binds *sandbox* around each test and destroys the tree afterwards."
  (:require [clojure.java.io :as io]
            [hive-test.isolation :as isolation])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:dynamic *sandbox*
  "Handle of the sandbox created by the :sandbox isolation fixture, bound
   around each test body. nil outside that fixture."
  nil)

(defn- render-content
  "Resolve a spec value to the file body string, calling fn values with
   {:dir :path}."
  [content dir rel]
  (if (fn? content)
    (str (content {:dir dir :path rel}))
    (str content)))

(defn- materialize-entry!
  "Write one spec entry under `dir`; returns [rel abs-path]."
  [dir [rel content]]
  (let [f (io/file dir rel)]
    (if (nil? content)
      (.mkdirs f)
      (do (.mkdirs (.getParentFile f))
          (spit f (render-content content dir rel))))
    [rel (str f)]))

(defn destroy!
  "Recursively delete the sandbox tree. Gated on the handle this ns minted —
   arbitrary {:dir ...} maps are refused (returns nil). Idempotent."
  [{:keys [dir] ::keys [created]}]
  (when (and created dir)
    (doseq [^java.io.File f (reverse (file-seq (io/file dir)))]
      (.delete f))
    true))

(defn sandbox!
  "Create a disposable tree from `spec` under a fresh temp root.
   opts: {:prefix str} names the temp dir (default \"hive-sandbox\").

   Returns a handle:
     :dir      absolute root path (str)
     :paths    {rel-path abs-path}
     :file     (fn [rel] abs-path) — resolves ANY rel path under the root,
               present in the spec or not
     :destroy! zero-arg fn, same as (destroy! handle)"
  ([spec] (sandbox! spec {}))
  ([spec {:keys [prefix] :or {prefix "hive-sandbox"}}]
   (let [dir    (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))
         paths  (into {} (map #(materialize-entry! dir %)) spec)
         handle {::created true
                 :dir      dir
                 :paths    paths
                 :file     (fn [rel] (str (io/file dir rel)))}]
     (assoc handle :destroy! (fn [] (destroy! handle))))))

(defmacro with-sandbox
  "binding: [sb spec] or [sb spec opts]. Creates the tree, binds `sb` to the
   handle, always destroys after `body` (throw included)."
  [[sym spec & [opts]] & body]
  `(let [~sym (sandbox! ~spec ~(or opts {}))]
     (try ~@body
          (finally (destroy! ~sym)))))

(defmethod isolation/emit-isolation :sandbox
  [{:keys [spec prefix]}]
  (fn [f]
    (with-sandbox [sb (or spec {}) {:prefix (or prefix "hive-sandbox")}]
      (binding [*sandbox* sb]
        (f)))))
