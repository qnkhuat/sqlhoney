(ns sqlhoney.core
  (:refer-clojure :exclude [format #_defmethod])
  (:require
   [methodical.core :as m]
   [honey.sql.helpers :as hsql.helpers])
  (:import
   (net.sf.jsqlparser.expression
    DoubleValue
    NullValue
    LongValue
    StringValue)
   (net.sf.jsqlparser.parser CCJSqlParserUtil)
   (net.sf.jsqlparser.schema
    Column
    Table)
   (net.sf.jsqlparser.statement.select
    AllColumns
    SelectItem
    PlainSelect)))

(m/defmulti jsql->honeysql class)

#_(defmacro m/defmethod
    [name dispatch-val method-args & body]
    `(do
      (clojure.core/defmethod ~name ~dispatch-val
        [& args#]
        (when debug (println "IN" args#))
        (let [result# (apply (fn [~@method-args] ~@body) args#)]
          (when debug (println "OUT" result#))
          result#))))


(m/defmethod jsql->honeysql AllColumns
  [^AllColumns obj]
  (str obj))

(defn- maybe-alias
  "Alias hsql-form if `obj` has an alias"
  [hsql-form obj]
  (if-let [the-alias (.getAlias obj)]
    [hsql-form (.getName the-alias)]
    hsql-form))

(m/defmethod jsql->honeysql :default
  [obj]
  (throw (ex-info (clojure.core/format "No implementation for %s" (class obj)) {:obj obj})))

;; net.sf.jsqlparser.statement.select
(m/defmethod jsql->honeysql PlainSelect
  [^PlainSelect obj]
  (merge
   (when-let [selects (.getSelectItems obj)]
     (def selects selects)
     (apply hsql.helpers/select (map jsql->honeysql selects)))
   (when-let [from (.getFromItem obj)]
     (hsql.helpers/from (jsql->honeysql from)))))

(m/defmethod jsql->honeysql AllColumns
  [^AllColumns obj]
  (str obj))

(m/defmethod jsql->honeysql SelectItem
  [^SelectItem obj]
  (maybe-alias (jsql->honeysql (.getExpression obj)) obj))

;; net.sf.jsqlparser.expression

(m/defmethod jsql->honeysql DoubleValue
  [^DoubleValue obj]
  (.getValue obj))

(m/defmethod jsql->honeysql LongValue
  [^LongValue obj]
  (.getValue obj))

(m/defmethod jsql->honeysql StringValue
  [^StringValue obj]
  (let [value (.getValue obj)]
    (case value
      "true" true
      "false" false
      value)))

(m/defmethod jsql->honeysql NullValue
  [^NullValue _obj]
  nil)

;; net.sf.jsqlparser.schema
(m/defmethod jsql->honeysql Column
  [^Column obj]
  (.getFullyQualifiedName obj true))

(m/defmethod jsql->honeysql Table
  [^Table obj]
  (maybe-alias (.getName obj) obj))

(defn format
  [query]
  (jsql->honeysql (CCJSqlParserUtil/parse query)))


(comment
 (use 'clojure.tools.trace)
 (clojure.tools.trace/trace-ns *ns*)
 (remove-all-methods jsql->honeysql)

 (try
  (format "select 1 + 1")
  (catch Exception e
    e)))
