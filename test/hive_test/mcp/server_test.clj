(ns hive-test.mcp.server-test
  "Facets for hive-test.mcp.server: the JSON-RPC envelope and tool-schema
   conversion as properties over their contracts, and a golden of a scripted
   stdio session (initialize, tools/list, tools/call, unknown method/tool)
   through run-server! with *in*/*out* bound to strings."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mcp.server :as server]))

(defn- envelope [[id result]] (server/->response id result))

(hst/deftrifecta-from-schema response-wraps-id-and-result
  envelope
  {:in  [:tuple server/Id :any]
   :out server/Response
   :rel (fn [[id result] out] (= {:jsonrpc "2.0" :id id :result result} out))
   :mutation true})

(def ^:private GenParam
  [:map [:name [:string {:min 1 :max 6}]] [:type {:optional true} [:enum :string :array :integer]]
   [:description {:optional true} :string] [:required {:optional true} :boolean]])

(defn- schema-of [params] (server/tool->mcp-schema {:name "t" :description "d" :parameters params}))

(hst/deftrifecta-from-schema tool-schema-lists-exactly-the-required-params
  schema-of
  {:in  [:vector {:max 5} GenParam]
   :out [:map [:inputSchema [:map [:properties :map] [:required [:vector :string]]]]]
   :rel (fn [params {{:keys [properties required]} :inputSchema}]
          (and (= (set (map :name params)) (set (keys properties)))
               (= (mapv :name (filter :required params)) required)))
   :mutation true})

(def ^:private echo-server
  (server/make-server
   {:name "echo" :version "9.9"
    :tools {:echo/say  {:name "echo/say" :description "echo"
                        :parameters [{:name "msg" :type :string :required true}]
                        :handler (fn [{:strs [msg]}] {:said msg})}
            :echo/text {:name "echo/text" :parameters [] :handler (fn [_] "plain")}
            :echo/boom {:name "echo/boom" :parameters [] :handler (fn [_] (throw (Exception. "kaput")))}}}))

(deftest notifications-and-id-less-requests-answer-nothing
  (is (nil? (server/handle-request echo-server {:method "notifications/initialized"})))
  (is (nil? (server/handle-request echo-server {:method "ping"}))))

(defn- session
  "Run the stdio server over `lines`; answer the parsed responses and the
   stderr log. Jackson's parse-error wording is version-dependent, so the
   log keeps only that a parse error was reported.

   The golden pins two KNOWN BUGS (recorded, not fixed): read-message
   keywordizes the JSON (server.clj:93) while tool handlers destructure
   :strs, so arguments never reach them (id 3 echoes null); and an unknown
   method answers inside :result instead of a JSON-RPC :error (server.clj:85)."
  [lines]
  (let [err (java.io.StringWriter.)
        out (with-out-str
              (binding [*in* (java.io.BufferedReader. (java.io.StringReader. (str/join "\n" lines)))
                        *err* err]
                (server/run-server! echo-server)))]
    {:responses (mapv #(json/parse-string % true) (str/split-lines out))
     :log (into [] (comp (remove #(str/starts-with? % " at "))
                         (map #(if (str/starts-with? % "Parse error") "Parse error" %)))
                (str/split-lines (str err)))}))

(deftest-golden stdio-session "test/golden/mcp/server-session.edn"
  (session [(json/generate-string {:jsonrpc "2.0" :id 1 :method "initialize"})
            (json/generate-string {:jsonrpc "2.0" :id 2 :method "tools/list"})
            (json/generate-string {:jsonrpc "2.0" :id 3 :method "tools/call"
                                   :params {:name "echo/say" :arguments {"msg" "hi"}}})
            (json/generate-string {:jsonrpc "2.0" :id 4 :method "tools/call" :params {:name "echo/text"}})
            (json/generate-string {:jsonrpc "2.0" :id 5 :method "tools/call" :params {:name "echo/boom"}})
            (json/generate-string {:jsonrpc "2.0" :id 6 :method "tools/call" :params {:name "nope"}})
            (json/generate-string {:jsonrpc "2.0" :id 7 :method "bogus"})
            (json/generate-string {:jsonrpc "2.0" :method "notifications/initialized"})
            "{not json"]))
