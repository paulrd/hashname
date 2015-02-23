(ns hashname.core-test
  (:require [clojure.test :refer :all]
            [hashname.core :refer :all]))

(deftest keys-test
  (testing "should generate from two keys"
    (let [ks {"3a" "eg3fxjnjkz763cjfnhyabeftyf75m2s4gll3gvmuacegax5h6nia"
              "1a" "an7lbl5e6vk4ql6nblznjicn5rmf3lmzlm"}]
      (is (= (from-keys ks) 
             "27ywx5e5ylzxfzxrhptowvwntqrd3jhksyxrfkzi6jfn64d3lwxa")))))

(deftest one-key
  (testing "should generate from one key"
    (let [k {"1a" "vgjz3yjb6cevxjomdleilmzasbj6lcc7"}]
      (is (= (from-keys k)
             "echmb6eke2f6z2mqdwifrt6i6hkkfua7hiisgrms6pwttd6jubiq")))))

(deftest no-keys
  (testing "should fail with no keys"
    (is (thrown-with-msg? Throwable #"should not be empty" (from-keys {})))))

(deftest bad-key
  (testing "should fail with bad key"
    (let [k {"bad" "8jze4merv08q6med3u21y460fjdcphkyuc858538mh48zu8az39t1vxdg9tadzun"}]
      (is (thrown-with-msg? Throwable #"must be a valid hex string" 
                            (from-keys k))))))
