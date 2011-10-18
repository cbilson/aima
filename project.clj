(defproject aima "1.0.0-SNAPSHOT"
  :description "This is me, messing aroud with stuff I am learning from 'Artificial Intelligence - A Modern Approach'"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.apache.commons/commons-math "2.0"]]
  :run-aliases {:knuth aima.ch3.knuth/-main
                :romania aima.ch3.romania/-main})
