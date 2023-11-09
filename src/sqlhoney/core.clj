(ns sqlhoney.core
  (:refer-clojure :exclude [format])
  (:require
   [honey.sql :as hsql]
   [honey.sql.helpers :as hsql.helpers]
   [methodical.core :as m])
  (:import
   (net.sf.jsqlparser.parser CCJSqlParserUtil)
   (net.sf.jsqlparser.schema
    Column
    Table)
   (net.sf.jsqlparser.statement.select
    AllColumns
    SelectItem
    PlainSelect)))

(m/defmulti jsql->honeysql
  class)

(m/defmethod jsql->honeysql :default
  [obj]
  (throw (ex-info (clojure.core/format "No implementation for %s" (class obj)) {:obj obj})))

(m/defmethod jsql->honeysql PlainSelect
  [^PlainSelect obj]
  (merge
   (when-let [selects (.getSelectItems obj)]
     (apply hsql.helpers/select (map jsql->honeysql selects)))
   (when-let [from (.getFromItem obj)]
     (hsql.helpers/from #p (jsql->honeysql from)))))

(m/defmethod jsql->honeysql SelectItem
  [^SelectItem obj]
  (let [expression (jsql->honeysql (.getExpression obj))]
    (if-let [the-alias (.getAlias obj)]
      [expression (.getName the-alias)]
      expression)))

(m/defmethod jsql->honeysql AllColumns
  [^AllColumns obj]
  (str obj))

(m/defmethod jsql->honeysql Column
  [^Column obj]
  (.getFullyQualifiedName obj true))

(m/defmethod jsql->honeysql Table
  [^Table obj]
  (let [table-name (.getName obj)]
    (if-let [the-alias (.getAlias obj)]
      [table-name (.getName the-alias)]
      table-name)))

(defn format
  [query]
  (#_methodical.util.trace/trace jsql->honeysql (CCJSqlParserUtil/parse query)))

(try
 (format "select id as a from orders")
 (catch Exception e
   e))
