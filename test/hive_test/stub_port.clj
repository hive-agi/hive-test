(ns hive-test.stub-port)

(defprotocol ExternalPort
  (lookup [this key] [this key fallback])
  (persist [this key value]))

(defprotocol TaggedPort
  "Protocol whose arglists carry return type hints, for the default policy."
  (label ^String [this])
  (open? ^Boolean [this])
  (untagged [this]))
