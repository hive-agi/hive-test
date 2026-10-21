(ns hive-test.mcp.server
  "Kaocha MCP server — stdio JSON-RPC server for test execution.
   Follows the clj-kondo-mcp pattern: Babashka MCP server that delegates
   test execution to a target project's nREPL."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [hive-test.mcp.tools :refer [kaocha-tools]]
            [hive-test.contract :as mc]))

;; Protocol version
(def protocol-version "2024-11-05")

;; --- JSON-RPC helpers ---

(defn ->response [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn ->error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

;; --- Tool schema conversion ---

(defn param->json-schema [{:keys [name type description required]}]
  [name (cond-> {:type (clojure.core/name (or type :string))}
          description (assoc :description description))])

(defn tool->mcp-schema [{:keys [name description parameters]}]
  (let [required-params (->> parameters
                             (filter :required)
                             (map :name)
                             vec)
        properties (into {} (map param->json-schema parameters))]
    {:name name
     :description description
     :inputSchema {:type "object"
                   :properties properties
                   :required required-params}}))

;; --- MCP Server ---

(defrecord MCPServer [name version tools])

(defn make-server [{:keys [name version tools]}]
  (->MCPServer name version tools))

(defn handle-initialize [server _params]
  {:protocolVersion protocol-version
   :capabilities {:tools {}}
   :serverInfo {:name (:name server) :version (:version server)}})

(defn handle-tools-list [server _params]
  {:tools (mapv tool->mcp-schema (vals (:tools server)))})

(defn handle-tools-call [server {:keys [name arguments]}]
  (let [tool-key (keyword name)
        tool     (or (get (:tools server) tool-key)
                     ;; Try with namespace
                     (get (:tools server) (keyword (str/replace name "/" ":")))
                     ;; Try exact string match
                     (->> (:tools server) vals
                          (filter #(= (:name %) name))
                          first))]
    (if tool
      (try
        (let [result ((:handler tool) (or arguments {}))]
          {:content [{:type "text"
                      :text (if (string? result)
                              result
                              (json/generate-string result {:pretty true}))}]})
        (catch Exception e
          {:content [{:type "text"
                      :text (str "Error: " (.getMessage e))}]
           :isError true}))
      {:content [{:type "text"
                  :text (str "Unknown tool: " name)}]
       :isError true})))

(defn handle-request [server {:keys [id method params]}]
  (let [result (case method
                 "initialize" (handle-initialize server params)
                 "notifications/initialized" nil
                 "tools/list" (handle-tools-list server params)
                 "tools/call" (handle-tools-call server params)
                 "ping" {}
                 {:error {:code -32601 :message (str "Unknown method: " method)}})]
    (when (and id result)
      (->response id result))))

;; --- Stdio Transport ---

(defn read-message []
  (when-let [line (read-line)]
    (when-not (str/blank? line)
      (try
        (json/parse-string line true)
        (catch Exception e
          (binding [*out* *err*]
            (println "Parse error:" (.getMessage e)))
          nil)))))

(defn write-message [msg]
  (println (json/generate-string msg))
  (flush))

(defn log-stderr [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn run-server! [server]
  (log-stderr "Starting" (:name server) "v" (:version server))
  (loop []
    (when-let [msg (read-message)]
      (log-stderr "Received:" (:method msg))
      (when-let [response (handle-request server msg)]
        (log-stderr "Responding to:" (:method msg))
        (write-message response))
      (recur)))
  (log-stderr "Server shutting down"))

;; --- Entry point ---

(def mcp-server
  (make-server
   {:name "kaocha-mcp"
    :version "0.1.0"
    :tools kaocha-tools}))

(defn -main [& _args]
  (run-server! mcp-server))

;; Contracts: registered only when malli is on the classpath (hive-test.contract).
(def Param [:map [:name :string] [:type {:optional true} [:maybe [:or :keyword :string]]]
            [:description {:optional true} [:maybe :string]] [:required {:optional true} :boolean]])
(def Tool [:map [:name :string] [:description {:optional true} [:maybe :string]]
           [:parameters [:sequential Param]] [:handler {:optional true} ifn?]])
(def Server [:map [:name :string] [:version :string] [:tools [:maybe [:map-of :keyword Tool]]]])
(def Id [:or :int :string])
(def Response [:map [:jsonrpc [:= "2.0"]] [:id :any] [:result :any]])
(def ToolResult [:map [:content [:vector [:map [:type [:= "text"]] [:text :string]]]]
                 [:isError {:optional true} :boolean]])

(mc/=> ->response        [:=> [:cat Id :any] Response])
(mc/=> ->error-response  [:=> [:cat Id :int :string]
                          [:map [:jsonrpc [:= "2.0"]] [:id :any] [:error [:map [:code :int] [:message :string]]]]])
(mc/=> param->json-schema [:=> [:cat Param] [:tuple :string [:map [:type :string] [:description {:optional true} :string]]]])
(mc/=> tool->mcp-schema  [:=> [:cat Tool]
                          [:map [:name :string] [:description :any]
                           [:inputSchema [:map [:type [:= "object"]] [:properties :map] [:required [:vector :string]]]]]])
(mc/=> make-server       [:=> [:cat [:map [:name :string] [:version :string] [:tools [:maybe [:map-of :keyword Tool]]]]] Server])
(mc/=> handle-initialize [:=> [:cat Server :any]
                          [:map [:protocolVersion :string] [:capabilities :map] [:serverInfo [:map [:name :string] [:version :string]]]]])
(mc/=> handle-tools-list [:=> [:cat Server :any] [:map [:tools [:vector :map]]]])
(mc/=> handle-tools-call [:=> [:cat Server [:map [:name :string] [:arguments {:optional true} [:maybe :map]]]] ToolResult])
;; nil for notifications and for a request without an id.
(mc/=> handle-request    [:=> [:cat Server [:map [:id {:optional true} [:maybe Id]] [:method :string] [:params {:optional true} :any]]]
                          [:maybe Response]])

;; The stdio transport: JSON lines in, JSON lines out.
(mc/=> read-message  [:=> [:cat] [:maybe :map]])
(mc/=> write-message [:=> [:cat :map] :nil])
(mc/=> log-stderr    [:=> [:cat [:* :any]] :nil])
(mc/=> run-server!   [:=> [:cat Server] :nil])
(mc/=> -main         [:=> [:cat [:* :string]] :nil])
