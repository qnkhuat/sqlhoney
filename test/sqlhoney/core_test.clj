(ns sqlhoney.core-test
  (:require
   [clojure.test :refer :all]
   [sqlhoney.core :as shoney]))

(deftest simple-select-star-test
  (is (= {:select ["*"] :from ["users"]}
         (shoney/format "select * from users"))))

(deftest select-with-alias-test
  (is (= {:select [["first_name" "fname"]] :from ["users"]}
         (shoney/format "select first_name as fname from users"))))
