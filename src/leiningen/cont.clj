(ns leiningen.cont
  "Continuous target task"
  (:use [clojure.string :only (split)]
        [clojure.repl :only (pst)])
  (:require [leiningen.help :as help]
            [watchdog :as watchdog]))

(defn- create-taskmap [task x]
  (let [func-name (second (split (str task) #"\."))]
    (assoc x func-name (str task))))

(defmacro import-task []
  (loop [task help/tasks x {}]
    (if (nil? task)
      `(def ^:dynamic *task* ~x)
      (do
        (require (first task))
        (recur (next task) (create-taskmap (first task) x))))))

(import-task)

(defn- get-lein-tasks [tasks]
  (loop [args tasks result nil]
    (if (nil? args)
      (reverse result)
      (recur (next args)
             (conj result [(get *task* (first args)) (first args)])))))

(defn- call-task [[nm k] project]
  (let [nm-map (ns-interns (symbol nm))
        fun (get nm-map (symbol k))]
    (fn [x]
      (apply fun [project]))))

(defn- create-func [tasks project]
  (fn [x]
    (try
      (println "Change Files" x)
      (loop [nms tasks]
        (if (nil? nms)
          nil
          (do
            ((call-task (first nms) project) x)
            (recur (next nms)))))
      (catch Exception _ ))))

(defn cont [project & args]
  (let [paths (concat [(str (:root project) "/project.clj")]
                    (:source-paths project)
                    (:resource-paths project)
                    (:test-pathes project))
        tasks (get-lein-tasks args)
        task-func (create-func tasks project)]
    (watchdog/set-interval (* 1000))
    (binding [leiningen.test/*exit-after-tests* false]
      (watchdog/watch-start paths task-func))))




