(ns build
  (:require [clojure.tools.build.api :as b]))
  
  
(defn compile-java []
  (b/delete {:path "target"})
  (b/javac {:src-dirs ["src/java"]
            :class-dir ["target/classes"]}))

(defn -run [& _]
  (compile-java))
