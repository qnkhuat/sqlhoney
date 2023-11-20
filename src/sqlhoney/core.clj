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
    NotExpression
    BinaryExpression)
   (net.sf.jsqlparser.expression.operators.relational
    Between
    ExistsExpression
    IsBooleanExpression
    InExpression
    IsNullExpression)
   (net.sf.jsqlparser.parser
    CCJSqlParserUtil)
   (net.sf.jsqlparser.schema
    Column
    Table)
   (net.sf.jsqlparser.statement.select
    AllColumns
    AllTableColumns
    Select
    SelectItem
    ParenthesedSelect
    PlainSelect)))

(def ^:dynamic *debug* false)

(def ^{:dynamic true
       :private true
       :doc     "Used to track indentation for debugging purposes"}
  *recursive-level* 0)

(def ^{:dynamic true
       :doc     "Which context the current form is in.
                Could be :select"}
  *context*
  nil)

(m/defmulti jsql->honeysql class)

(defn- maybe-alias
  "Alias hsql-form if `obj` has an alias"
  [hsql-form the-alias]
  (if (some? the-alias)
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
  (if *debug*
    (let [indent (apply str (repeat *recursive-level* "  "))]
      (println indent *recursive-level* "> IN:" obj)
      (let [result (binding  [*recursive-level* (inc *recursive-level*)]
                     (next-method obj))]
        (println indent *recursive-level* "< OUT:" (class obj) result)
        result))
    (next-method obj)))

(m/defmethod jsql->honeysql :default
  [obj]
  (throw (ex-info (clojure.core/format "No implementation for %s" (class obj)) {:obj obj})))

;; net.sf.jsqlparser.statement.select

(m/defmethod jsql->honeysql Select
  [^Select obj]
  ;; make sure binding is nil to starts with for nested cases
  (binding [*context* nil]
    (cond-> {}
      (some? (.getSelectItems obj))
      (#(binding [*context* :select]
          (apply hsql.helpers/select % (map jsql->honeysql (.getSelectItems obj)))))

      (some? (.getFromItem obj))
      (hsql.helpers/from (jsql->honeysql (.getFromItem obj)))

      (some? (.getWhere obj))
      (hsql.helpers/where (jsql->honeysql (.getWhere obj))))))

(m/defmethod jsql->honeysql ParenthesedSelect
  [^PlainSelect obj]
  (maybe-alias (jsql->honeysql (.getSelect obj)) (.getAlias obj)))

(m/defmethod jsql->honeysql AllColumns
  [^AllColumns _obj]
  :*)

(m/defmethod jsql->honeysql AllTableColumns
  [^AllTableColumns obj]
  (keyword (clojure.core/format "%s.*" (.getTable obj))))

(m/defmethod jsql->honeysql SelectItem
  [^SelectItem obj]
  (maybe-alias (jsql->honeysql (.getExpression obj)) (.getAlias obj)))

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

(m/defmethod jsql->honeysql IsBooleanExpression
  [^IsBooleanExpression obj]
  [(if (.isNot obj) :is-not :is) (jsql->honeysql (.getLeftExpression obj)) (.isTrue obj)])

(m/defmethod jsql->honeysql InExpression
  [^InExpression obj]
  [:in (jsql->honeysql (.getLeftExpression obj)) (jsql->honeysql (.getRightExpression obj))])

(m/defmethod jsql->honeysql IsNullExpression
  [^IsNullExpression obj]
  [(if (.isNot obj) :is-not :is) (jsql->honeysql (.getLeftExpression obj)) nil])

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
  (maybe-alias (keyword (.getName obj)) (.getAlias obj)))

(defn format
  ([query]
   (format query false))
  ([query debug?]
   (binding [*debug* debug?]
     (jsql->honeysql (CCJSqlParserUtil/parse query)))))

(comment
 (use 'clojure.tools.trace)
 (clojure.tools.trace/trace-ns *ns*)
 (m/remove-all-methods! #'jsql->honeysql)

 (hsql/format {:select [:*]
               :from   [:u]
               :where  [:is-not :id nil]})
 (try
  (binding [*debug* true]
    (format ""))
  (catch Exception e
    e)))
