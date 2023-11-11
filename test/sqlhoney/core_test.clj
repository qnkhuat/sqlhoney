(ns sqlhoney.core-test
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

(deftest simple-select-test
  (testing "select all"
    (test-format "select * from users"
                 {:select [:*]
                  :from   [:users]})

    (testing "from with alias"
      (test-format "select * from users as u"
                   {:select [:*]
                    :from   [[:users :u]]})))

  (testing "single select"
    (test-format "select id from users"
                 {:select [:id]
                  :from   [:users]})

    (testing "with alias"
      (test-format "select first_name as fname from users"
                   {:select [[:first_name :fname]]
                    :from   [:users]})))

  (testing "multiple select with alias"
    (test-format "select id, first_name as fname from users as u"
                 {:select [:id [:first_name :fname]]
                  :from   [[:users :u]]})))

(deftest select-expression-test
  (testing "simple"
    (test-format "select *" [:*] :select)
    (test-format "select 1" [1] :select)
    (test-format "select 1.5" [1.5] :select)
    (test-format "select 1, 2" [1 2] :select)
    (test-format "select 'a'" ["a"] :select)
    (test-format "select 1 as a" [[1 :a]] :select)
    ;; FIXME
    #_(test-format "select true" [true] :select)
    #_(test-format "select false" [false] :select)
    (test-format "select null" [nil] :select)))

(deftest unary-expression-test
  (test-format "select * from u where not id = 1" [:not [:= :id 1]] :where)
  (test-format "select * from u where ! id = 1" [:! [:= :id 1]] :where))

(deftest binary-expression-test
  (testing "comparision"
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
    (test-format "select * from u where (id + 1) = 2" [:= [:+ :id 1] 2] :where)
    (test-format "select * from u where (id / 2) = 2" [:= [:/ :id 2] 2] :where)
    (test-format "select * from u where (id * 2) = 2" [:= [:* :id 2] 2] :where)
    (test-format "select * from u where (id % 2) = 2" [:= [:% :id 2] 2] :where))
  (testing "conditional"
    (test-format "select * from u where id > 1 and id < 3 " [:and [:> :id 1] [:< :id 3]] :where)
    (test-format "select * from u where id > 1 or id < 3 " [:or [:> :id 1] [:< :id 3]] :where)
    (test-format "select * from u where id > 1 xor id < 3 " [:xor [:> :id 1] [:< :id 3]] :where)))
