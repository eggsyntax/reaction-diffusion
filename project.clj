(defproject reactiondiffusion "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 [net.mikera/core.matrix "0.59.0"]
                 ;; [bocko "1.0.0"] ; simple graphics
                 [quil "2.6.0"]]
  :main ^:skip-aot reactiondiffusion.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
