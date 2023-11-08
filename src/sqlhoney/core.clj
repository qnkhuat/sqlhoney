(ns sqlhoney.core
  (:refer-clojure :exclude [format])
  (:require
   [honey.sql :as hsql]
   [honey.sql.helpers :as hsql.helpers])
  (:import
   (net.sf.jsqlparser.parser CCJSqlParserUtil)
   (net.sf.jsqlparser.statement.select
    AllColumns
    SelectItem
    PlainSelect)))

#_(.getName (.getFromItem (CCJSqlParserUtil/parse "select * from orders;")))

(.getSelectItems (CCJSqlParserUtil/parse "select * from orders;"))

(defmulti jsql->honeysql
  (fn [obj _query]
    #pp _query
    (class obj)))

(defmethod jsql->honeysql PlainSelect
  [obj query]
  #p obj
  (-> query
      (hsql.helpers/select (map #(jsql->honeysql % query) (.getSelectItems obj)))))

(defmethod jsql->honeysql SelectItem
  [obj query]
  (jsql->honeysql (.getExpression obj) query))

(defmethod jsql->honeysql AllColumns
  [obj _query]
  (str obj))

(defn format
  [query]
  (jsql->honeysql #p (CCJSqlParserUtil/parse query) {}))

(str (.getExpression (first #p (.getSelectItems (CCJSqlParserUtil/parse "select * from orders;")))))
(first (.getSelectItems (CCJSqlParserUtil/parse "select * from orders;")))

(format "select * from orders")
