(ns sqlhoney.core-test
  "The tests should be organized after the class hierarchy from
  https://javadoc.io/static/com.github.jsqlparser/jsqlparser/4.7/overview-tree.html.

  Rambling but I'm thinking, maybe we should structure the deftests after the classes because that's how we extend sqlhoney.

  But From the classes, we should use the grammar graph[1] to define structure for testing.

  [1]: https://jsqlparser.github.io/JSqlParser/syntax_snapshot.html
  "
  (:require
   [clojure.test :refer [deftest is testing]]
   [environ.core :refer [env]]
   [next.jdbc :as jdbc]
   [honey.sql :as hsql]
   [sqlhoney.core :as shoney]))

(def ds (when-let [uri (env :sql-honey-connection-uri-test)]
          (jdbc/get-datasource uri)))

(defn- sql->honey->sql
  [sql]
  (-> sql
      shoney/format
      hsql/format))

(defn- one-or-many
  [x]
  (if ((some-fn sequential? set? nil?) x)
    x
    [x]))

(defmacro ^:private test-format
  ([query expected]
   `(test-format ~query ~expected nil))
  ([query expected sub-paths]
   `(let [query# ~query]
      (testing (format "query: \"%s\" " query#)
        (let [sub-paths#  (one-or-many ~sub-paths)
              hsql-query# (shoney/format query#)
              raw-sql#    (hsql/format hsql-query#)]
          ;; test to see if we can convert from hsql to raw sql
          (is (some? raw-sql#))
          #_(is (some? (jdbc/execute! ds raw-sql#)))
          (is (= ~expected (cond-> hsql-query#
                             (some? sub-paths#)
                             (get-in sub-paths#)))))))))

;; https://javadoc.io/doc/com.github.jsqlparser/jsqlparser/latest/index.html
(deftest jsqlparser.statement.select-test
  (testing "AllColumns"
    (test-format "select *"  [:*] :select)
    (testing "AllTableColumns"
      (test-format "select users.*" [:users.*] :select))
    (testing "ForClause")
    (testing "Function")
    (testing "Join")
    (testing "Limit")
    (testing "Select"
      ;; see to jsqlparser.statement.select.PlainSelect-test
      (testing "PlainSelect"))))

(deftest jsqlparser.statement.select.PlainSelect-test
  (testing "SelectItemsList"
    (testing "SelectItem"
      (test-format "select id" [:id] :select)
      (test-format "select id, first_name as fname" [:id [:first_name :fname]] :select)
      (test-format "select 1" [1] :select)
      (test-format "select 1.5" [1.5] :select)
      (test-format "select 1, 2" [1 2] :select)
      (test-format "select 'a'" ["a"] :select)
      (test-format "select 1 as a" [[1 :a]] :select)
      (test-format "select true" [:true] :select)
      (test-format "select false" [:false] :select)
      (test-format "select null" [nil] :select)
      (testing "with alias"
        (test-format "select id as i" [[:id :i]] :select)))))

(deftest unary-expression-test
  (test-format "select * from u where not id = 1" [:not [:= :id 1]] :where)
  (test-format "select * from u where ! id = 1" [:! [:= :id 1]] :where))

(deftest binary-expression-test ;; net.sf.jsqlparser.expression
  (testing "comparison"
    (test-format "select * from u where id > 1" [:> :id 1] :where)
    (test-format "select * from u where id >= 1" [:>= :id 1] :where)
    (test-format "select * from u where id < 1" [:< :id 1] :where)
    (test-format "select * from u where id <= 1" [:<= :id 1] :where)
    (test-format "select * from u where id = 1" [:= :id 1] :where)
    (test-format "select * from u where id <> 1" [:<> :id 1] :where)
    (test-format "select * from u where id != 1" [:!= :id 1] :where)
    (testing "bitwise"
      (test-format "select * from u where (id & 1) = 1" [:= [:& :id 1] 1] :where)
      (test-format "select * from u where (id | 1) = 1" [:= [:| :id 1] 1] :where)
      (test-format "select * from u where (id ~ 1) = 1" [:= [(keyword "~") :id 1] 1] :where)
      (test-format "select * from u where (id ^ 1) = 1" [:= [(keyword "^") :id 1] 1] :where)
      (test-format "select * from u where (id << 1) = 1" [:= [:<< :id 1] 1] :where)
      (test-format "select * from u where (id >> 1) = 1" [:= [:>> :id 1] 1] :where)))
  (testing "arithmetic"
    ;; net.sf.jsqlparser.expression.operators.arithmetic
    (test-format "select * from u where (id + 1) = 2" [:= [:+ :id 1] 2] :where)
    (test-format "select * from u where (id / 2) = 2" [:= [:/ :id 2] 2] :where)
    (test-format "select * from u where (id * 2) = 2" [:= [:* :id 2] 2] :where)
    (test-format "select * from u where (id % 2) = 2" [:= [:% :id 2] 2] :where))
  (testing "conditional"
    ;; net.sf.jsqlparser.expression.operators.conditional
    (test-format "select * from u where id > 1 and id < 3 " [:and [:> :id 1] [:< :id 3]] :where)
    (test-format "select * from u where id > 1 or id < 3 " [:or [:> :id 1] [:< :id 3]] :where)
    (test-format "select * from u where id > 1 xor id < 3 " [:xor [:> :id 1] [:< :id 3]] :where))
  (testing "relational"
    ;;net.sf.jsqlparser.expression.operators.relational
    (test-format "select * from u where id between 1 and 10" [:between :id 1 10] :where)
    (test-format "select * from u where id not between 1 and 10" [:not-between :id 1 10] :where)
    (is (= ["SELECT * FROM u WHERE id NOT BETWEEN ? AND ?" 1 10]
           (hsql/format (shoney/format "select * from u where id not between 1 and 10"))))
    (test-format "select * from u where exists (select id from u where id > 10)"
                 [:exists {:select [:id] :from [:u] :where [:> :id 10]}] :where)
    (test-format "select * from u where id in (select id from a where id > 3)"
                 [:in :id {:select [:id]
                           :from   [:a]
                           :where  [:> :id 3]}]
                 :where)
    (test-format "select * from u where id is NULL" [:is :id nil] :where)
    (test-format "select * from u where id is not NULL" [:is-not :id nil] :where)
    (test-format "select * from u where id is true" [:is :id true] :where)
    (test-format "select * from u where id is not false" [:is-not :id false] :where))

  (testing "binary expression in select returns 3 layers vector"
    (test-format "select 1 + 1" [[[:+ 1 1]]] :select))
  (testing "string"
    (test-format "select 'sql' + 'honey'" [[[:+ "sql" "honey"]]] :select)
    (test-format "select * from u where name like '%ngoc%'" [:like :name "%ngoc%"] :where)
    (test-format "select * from u where name regexp 'ngoc.*'" [:regexp :name "ngoc.*"] :where)))

(deftest nested-select-test
 (test-format "select * from (select * from u) as s" {:select [:*]
                                                      :from   [[{:select [:*]
                                                                 :from   [:u]} :s]]}))


;; TODO: add more tests for nested select
