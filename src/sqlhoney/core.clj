(ns sqlhoney.core
  (:refer-clojure :exclude [format #_defmethod])
  (:require
   [honey.sql :as hsql]
   [honey.sql.helpers :as hsql.helpers]
   #_[methodical.core :as m])
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

(defmulti jsql->honeysql class)

#_(defmacro defmethod
    [name dispatch-val method-args & body]
    `(do
      (clojure.core/defmethod ~name ~dispatch-val
        [& args#]
        (when debug (println "IN" args#))
        (let [result# (apply (fn [~@method-args] ~@body) args#)]
          (when debug (println "OUT" result#))
          result#))))


(defmethod jsql->honeysql AllColumns
  [^AllColumns obj]
  (str obj))

(defn- maybe-alias
  "Alias hsql-form if `obj` has an alias"
  [hsql-form obj]
  (if-let [the-alias (.getAlias obj)]
    [hsql-form (.getName the-alias)]
    hsql-form))

(defmethod jsql->honeysql :default
  [obj]
  (throw (ex-info (clojure.core/format "No implementation for %s" (class obj)) {:obj obj})))

;; net.sf.jsqlparser.statement.select
(defmethod jsql->honeysql PlainSelect
  [^PlainSelect obj]
  (merge
   (when-let [selects (.getSelectItems obj)]
     (def selects selects)
     (apply hsql.helpers/select (map jsql->honeysql selects)))
   (when-let [from (.getFromItem obj)]
     (hsql.helpers/from (jsql->honeysql from)))))

(defmethod jsql->honeysql AllColumns
  [^AllColumns obj]
  (str obj))

(defmethod jsql->honeysql SelectItem
  [^SelectItem obj]
  (maybe-alias (jsql->honeysql (.getExpression obj)) obj))

;; net.sf.jsqlparser.expression

(defmethod jsql->honeysql DoubleValue
  [^DoubleValue obj]
  (.getValue obj))

(defmethod jsql->honeysql LongValue
  [^LongValue obj]
  (.getValue obj))

(defmethod jsql->honeysql StringValue
  [^StringValue obj]
  (.getValue obj))

;; net.sf.jsqlparser.schema
(defmethod jsql->honeysql Column
  [^Column obj]
  (.getFullyQualifiedName obj true))

(defmethod jsql->honeysql Table
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
  (format "select 1 as a")
  (catch Exception e
    e)))
