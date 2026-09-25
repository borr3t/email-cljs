(ns email-cljs.dev
  "Node-script entry point for interactive development."
  (:require [email-cljs.util :as util]))

(defn main [& _]
  (println "email-cljs dev runtime loaded. Attach with: npx shadow-cljs cljs-repl dev")
  (println "secret wrapping works:" (pr-str (util/secret "not-a-real-password")))
  (js/setInterval #(println "…dev heartbeat") 60000))
