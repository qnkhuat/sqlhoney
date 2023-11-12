(ns sqlhoney.core
  (:refer-clojure :exclude [format])
  (:require
   [clojure.string :as str]
   [methodical.core :as m]
   [honey.sql :as hsql]
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
    Between
    ExistsExpression)
   (net.sf.jsqlparser.parser CCJSqlParserUtil)
   (net.sf.jsqlparser.schema
    Column
    Table)
   (net.sf.jsqlparser.statement.select
    AllColumns
    Select
    SelectItem
    ParenthesedSelect
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

(defn- maybe-wrap-vector
  "Forms like expression will need to be 3-level vectors in select"
  [hsql-form]
  (cond-> hsql-form
    (= :select *context*)
    vector))

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
(m/defmethod jsql->honeysql Select
  [^Select obj]
  (merge
   (when-let [selects (.getSelectItems obj)]
     (binding [*context* :select]
       (apply hsql.helpers/select (map jsql->honeysql selects))))
   (when-let [from (.getFromItem obj)]
     (hsql.helpers/from (jsql->honeysql from)))
   (when-let [where (.getWhere obj)]
     (hsql.helpers/where (jsql->honeysql where)))))

#_(m/defmethod jsql->honeysql ParenthesedSelect
    [^PlainSelect obj]
    #_(merge
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
  (maybe-wrap-vector [(keyword (str/lower-case (.getStringExpression obj)))
                      (jsql->honeysql (.getLeftExpression obj))
                      (jsql->honeysql (.getRightExpression obj))]))

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

(hsql/register-fn! :not-between
                   (fn [_ [x a b]]
                     (let [[sql-x & params-x] (hsql/format-expr x {:nested true})
                           [sql-a & params-a] (hsql/format-expr a {:nested true})
                           [sql-b & params-b] (hsql/format-expr b {:nested true})]
                       (-> [(str sql-x " NOT BETWEEN " sql-a " AND " sql-b)]
                           (into params-x)
                           (into params-a)
                           (into params-b)))))

(m/defmethod jsql->honeysql Between
  [^Between obj]
  [(if (.isNot obj) :not-between :between) (jsql->honeysql (.getLeftExpression obj))
   (jsql->honeysql (.getBetweenExpressionStart obj)) (jsql->honeysql (.getBetweenExpressionEnd obj))])

(m/defmethod jsql->honeysql ExistsExpression
  [^ExistsExpression obj]
  (maybe-wrap-vector [:exists (jsql->honeysql (.getRightExpression obj))]))



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
 (m/remove-all-methods! #'jsql->honeysql)

 (try
  (binding [*debug* true]
    (format "select * from u where exists (select id from u where id > 10)"))
  (catch Exception e
    e)))
