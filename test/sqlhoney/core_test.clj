(ns sqlhoney.core-test
  (:require
   [honey.sql :as hsql]
   [clojure.test :refer [deftest is testing]]
   [sqlhoney.core :as shoney]))

(defmacro ^:private f
  [query]
  `(let [hsql-query# (shoney/format ~query)]
     ;; test to see if we can convert from hsql to raw sql
     (is (some? (hsql/format hsql-query#)))
     hsql-query#))

(deftest simple-select-test
  (testing "select all"
    (is (= {:select ["*"]
            :from   ["users"]}
           (f "select * from users")))

    (testing "from with alias"
      (is (= {:select ["*"]
              :from   [["users" "u"]]}
             (f "select * from users as u")))))

  (testing "single select"
    (is (= {:select ["id"]
            :from   ["users"]}
           (f "select id from users")))

    (testing "with alias"
      (is (= {:select [["first_name" "fname"]]
              :from   ["users"]}
             (f "select first_name as fname from users")))))

  (testing "multiple select with alias"
    (is (= {:select ["id" ["first_name" "fname"]]
            :from   [["users" "u"]]}
           (f "select id, first_name as fname from users as u")))))

(deftest select-expression-test
  (testing "simple"
    (is (= {:select [1]}
           (f "select 1")))
    (is (= {:select [1.5]}
           (f "select 1.5")))
    (is (= {:select [1 2]}
           (f "select 1, 2")))
    (is (= {:select ["a"]}
           (f "select 'a'")))
    (is (= {:select [[1 "a"]]}
           (f "select 1 as a")))))
