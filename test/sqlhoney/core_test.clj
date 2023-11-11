(ns sqlhoney.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [environ.core :refer [env]]
   [next.jdbc :as jdbc]
   [honey.sql :as hsql]
   [sqlhoney.core :as shoney]))

(def ds (jdbc/get-datasource (env :sql-honey-connection-uri-test)))

(defn sql->honey->sql
  [sql]
  (-> sql
      shoney/format
      hsql/format))

(defmacro ^:private test-format
  [query expected]
  `(let [query#      ~query
         hsql-query# (shoney/format query#)
         raw-sql#    (hsql/format hsql-query#)]
     ;; test to see if we can convert from hsql to raw sql
     (testing (format "query: %s "query#)
       (is (some? raw-sql#))
       #_(is (some? (jdbc/execute! ds raw-sql#)))
       (is (= ~expected hsql-query#)))))

(deftest simple-select-test
  (testing "select all"
    (test-format "select * from users"
                 {:select ["*"]
                  :from   ["users"]})

    (testing "from with alias"
      (test-format "select * from users as u"
                   {:select ["*"]
                    :from   [["users" "u"]]})))

  (testing "single select"
    (test-format "select id from users"
                 {:select ["id"]
                  :from   ["users"]})

    (testing "with alias"
      (test-format "select first_name as fname from users"
                   {:select [["first_name" "fname"]]
                    :from   ["users"]})))

  (testing "multiple select with alias"
    (test-format "select id, first_name as fname from users as u"
                 {:select ["id" ["first_name" "fname"]]
                  :from   [["users" "u"]]})))

(deftest select-expression-test
  (testing "simple"
    (test-format "select 1"
                 {:select [1]})
    (test-format "select 1.5"
                 {:select [1.5]})
    (test-format "select 1, 2"
                 {:select [1 2]})
    (test-format "select 'a'"
                 {:select ["a"]})
    (test-format "select 1 as a"
                 {:select [[1 "a"]]})
    (test-format "select true"
                 {:select [true]})
    (test-format "select false"
                 {:select [true]})
    (test-format "select null"
                 {:select [nil]})))
