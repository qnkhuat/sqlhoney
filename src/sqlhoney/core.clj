(ns sqlhoney.core
  (:refer-clojure :exclude [format])
  (:require
   [clojure.string :as str]
   [methodical.core :as m]
   [honey.sql.helpers :as hsql.helpers])
  (:import
   (net.sf.jsqlparser.expression
    DoubleValue
    NullValue
    LongValue
    StringValue
    Parenthesis
    JdbcParameter
    NotExpression
    BinaryExpression)
   (net.sf.jsqlparser.expression.operators.arithmetic
    Addition)
   (net.sf.jsqlparser.expression.operators.relational
    ComparisonOperator
    EqualsTo)
   (net.sf.jsqlparser.parser CCJSqlParserUtil)
   (net.sf.jsqlparser.schema
    Column
    Table)
   (net.sf.jsqlparser.statement.select
    AllColumns
    SelectItem
    PlainSelect)))

(def ^:dynamic *debug* false)

(def ^{:dynamic true
       :doc     "Which context the current form is in.
                Could be :select"}
  *context*
  nil)

(m/defmulti jsql->honeysql class)

(defn- maybe-alias
  "Alias hsql-form if `obj` has an alias"
  [hsql-form obj]
  (if-let [the-alias (.getAlias obj)]
    [hsql-form (keyword (.getName the-alias))]
    hsql-form))

(m/defmethod jsql->honeysql :around Object
  [obj]
  (when *debug*
    (println "IN: jsql->honeysql: " (class obj) obj))
  (let [result (next-method obj)]
    (when *debug*
      (println "OUT: jsql->honeysql: " (class obj) obj result))
    result))

(m/defmethod jsql->honeysql :default
  [obj]
  (throw (ex-info (clojure.core/format "No implementation for %s" (class obj)) {:obj obj})))

;; net.sf.jsqlparser.statement.select
(m/defmethod jsql->honeysql PlainSelect
  [^PlainSelect obj]
  (merge
   (when-let [selects (.getSelectItems obj)]
     (binding [*context* :select]
       (apply hsql.helpers/select (map jsql->honeysql selects))))
   (when-let [from (.getFromItem obj)]
     (hsql.helpers/from (jsql->honeysql from)))
   (when-let [where (.getWhere obj)]
     (hsql.helpers/where (jsql->honeysql where)))))

(m/defmethod jsql->honeysql AllColumns
  [^AllColumns _obj]
  :*)

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

(m/defmethod jsql->honeysql BinaryExpression
  [^BinaryExpression obj]
  (cond-> [(keyword (str/lower-case (.getStringExpression obj)))
           (jsql->honeysql (.getLeftExpression obj))
           (jsql->honeysql (.getRightExpression obj))]
    ;; expresison in select clause require 3 level vector
    (= *context* :select)
    vector))

(m/defmethod jsql->honeysql NotExpression
  [^NotExpression obj]
  (if (.isExclamationMark obj)
    [:! (jsql->honeysql (.getExpression obj))]
    [:not (jsql->honeysql (.getExpression obj))]))

(m/defmethod jsql->honeysql Parenthesis
  [^Parenthesis obj]
  (jsql->honeysql (.getExpression obj)))

;; net.sf.jsqlparser.expression.operators.arithmetic

;; net.sf.jsqlparser.expression.operators.relational

;; TODO: not really sure what's the use case here
#_(m/defmethod jsql->honeysql JdbcParameter
    [^JdbcParameter obj]
    "?")

;; net.sf.jsqlparser.schema

(m/defmethod jsql->honeysql Column
  [^Column obj]
  (keyword (.getFullyQualifiedName obj true)))

(m/defmethod jsql->honeysql Table
  [^Table obj]
  (maybe-alias (keyword (.getName obj)) obj))

(defn format
  [query]
  (jsql->honeysql (CCJSqlParserUtil/parse query)))

(comment
 (use 'clojure.tools.trace)
 (clojure.tools.trace/trace-ns *ns*)
 (remove-all-methods jsql->honeysql)

 (try
  (binding [*debug* true]
    (format "select * from u where (id and 1) = 1"))
  (catch Exception e
    e)))
