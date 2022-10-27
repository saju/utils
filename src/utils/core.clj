;;
;; Utility for reporting stats on open PRs for Kong/kong-ee
;;
;; Designed to run from REPL. Has routines to fetch all open PRs
;; and generate frequency counts by authors and maps PRs back to managers
;;
(ns utils.core
  (:require
   [clojure.data.json :as json]
   [org.httpkit.client :as http]))

(def manager-map {:guanlan ["ms2008" "StarlightIbuki" "fffonion"
                            "sumimakito" "outsinre" "windmgc"
                            "vm-001" "catbro666" "mayocream"
                            "ADD-SP" "liverpool8056" "raoxiaoyan"]
                  :rob ["Guaris" "hutchic" "davidma415"
                        "KongSteve"]
                  :tyler ["locao" "tyler-ball" "gszr" "liyangau"
                          "flrgh"]
                  :enrique ["samugi" "jschmid1" "hanshuebner" "Tieske"]})


(defn map-to-manager
  [authors]
  (let [mgrs (keys manager-map)]
    (frequencies
     (replace
      {nil :rob} ;; anything we don't have a mapping for falls to Rob
      (map
       (fn [gh-user]
         (some
          (fn [mgr]
            (let [found? (some #(= gh-user %)
                               (get manager-map mgr))]
              (cond
                found? mgr
                :else false)))
          mgrs))
       authors)))))



(defn load-token-from-disk
  [token-file]
  (slurp token-file))


(defn fetch-open-pr-page!
  ;;
  ;; fetches one page worth of PRs data from the GH pull listing api
  ;;
  [repo gh-tok page-num]
  (let [resp
        @(http/get
          (str "https://api.github.com/repos/"
               repo
               "/pulls")
          {:query-params {:state "open"
                          :per_page 100 ;; this is the max num of PRs returned in a single call
                          :page page-num}
           :headers {"Authorization" (str "Bearer "
                                          gh-tok)
                     "Accept" "application/vnd.github+json"}})]
    (if (not= (:status resp) 200)
      (do
        (println "PR api call failed " (:status resp))
        nil)
      (json/read-str (:body resp)))))
      

(defn fetch-all-open-prs
  ;;
  ;; fetches all open PRs for the given repo by repeatedly calling the GH pull listing api
  ;;
  [repo gh-tok]
  (loop [prs []
         page-num 1]
    (let [op (fetch-open-pr-page!
              repo
              gh-tok
              page-num)]
      (do
        (println (str "page-num " page-num)))
      (if (nil? op)
        prs
        (if (< (count op) 100)
          ;; no more pages to read
          (concat prs op)
          (recur (concat prs op)
                 (inc page-num)))))))

;;
;; from REPL
;; (def gh-tok (load-token-from-disk "/Users/srp/.token"))
;; (def prs (fetch-all-open-prs "Kong/kong-ee" gh-tok))
;; (map #(get (get % "user") "login") prs) -- for authors
;; 


                 

