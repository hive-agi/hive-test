(ns hive-test.stub-port)

(defprotocol ExternalPort
  (lookup [this key] [this key fallback])
  (persist [this key value]))
