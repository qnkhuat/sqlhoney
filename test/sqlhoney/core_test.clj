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

(defn do-test-format
  [query expected sub-keys]
  (let [sub-keys   (one-or-many sub-keys)
        hsql-query (shoney/format query)
        raw-sql    (hsql/format hsql-query)]
    ;; test to see if we can convert from hsql to raw sql
    (testing (format "query: \"%s\" "query)
      (is (some? raw-sql))
      #_(is (some? (jdbc/execute! ds raw-sql)))
      (is (= expected (cond-> hsql-query
                        (some? sub-keys)
                        (get-in sub-keys)))))))

(defmacro ^:private test-format
  ([query expected]
   `(test-format ~query ~expected nil))
  ([query expected sub-keys]
   `(let [query#     ~query]
      (testing (format "query: \"%s\" " query#)
        (let [sub-keys#  (one-or-many ~sub-keys)
              hsql-query# (shoney/format query#)
              raw-sql#    (hsql/format hsql-query#)]
          ;; test to see if we can convert from hsql to raw sql
          (is (some? raw-sql#))
          #_(is (some? (jdbc/execute! ds raw-sql#)))
          (is (= ~expected (cond-> hsql-query#
                             (some? sub-keys#)
                             (get-in sub-keys#)))))))))

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
      (test-format "select * from u where (id >> 1) = 1" [:= [:>> :id 1] 1] :where))
    (testing "logical"
      (test-format "select * from u where (id and 1) = 1" [:= [:and :id 1] 1] :where)
      (test-format "select * from u where (id or 1) = 1" [:= [:or :id 1] 1] :where)
      (test-format "select * from u where (id xor 1) = 1" [:= [:xor :id 1] 1] :where)
      (test-format "select * from u where (id is true)" [:is :id true] :where)
      (test-format "select * from u where (id is not true)" [:is-not :id true] :where)
      (test-format "select * from u where (id is null)" [:is :id nil] :where)
      (test-format "select * from u where (id is not null)" [:is-not :id nil] :where)
      (test-format "select * from u where (id like 'a')" [:like :id "a"] :where)
      (test-format "select * from u where (id not like 'a')" [:not-like :id "a"] :where)
      (test-format "select * from u where (id in (1, 2))" [:in :id [1 2]] :where)
      (test-format "select * from u where (id not in (1, 2))" [:not-in :id [1 2]] :where)
      (test-format "select * from u where (id between 1 and 2)" [:between :id 1 2] :where)
      (test-format "select * from u where (id not between 1 and 2)" [:not-between :id 1 2] :where)
      (test-format "select * from u where (id exists (select 1))" [:exists :id [1]] :where)
      (test-format "select * from u where (id not exists (select 1))" [:not-exists :id [1]] :where)
      (test-format "select * from u where (id sounds like 'a')" [:sounds-like :id "a"] :where))))

(deftest is-it-throw?
  #_(testing "abbbbbbbbbbbbbbbbb"
      (test-format "select * from u where (id is true)" [:is :id true] :where))
  (testing "throw it"
    (throw (ex-info (clojure.core/format "No implementation for %s" 1) {:obj 1}))))

#_(macroexpand-1 '(test-format "select * from u where (id is true)" [:is :id true] :where))
