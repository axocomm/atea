(ns tracker.core
  (:require [clojure.string :as string])
  (:import (java.awt AWTException
                     Image
                     MenuItem
                     PopupMenu
                     SystemTray
                     Toolkit
                     TrayIcon)
           (java.awt.event ActionEvent
                           ActionListener)
           (java.net MalformedURLException
                     URL)
           (java.util Date))
  (:gen-class))

(defn resource-path [name]
  (str "resources/" name))

(defn load-icon [name]
  (.getImage (Toolkit/getDefaultToolkit) (resource-path name)))

(defn get-tray []
  (SystemTray/getSystemTray))

(defn create-menu []
  (let [p (PopupMenu.)]
    (.add p (MenuItem. "Stupid"))
    p))

(defn action [f]
  (reify java.awt.event.ActionListener
    (actionPerformed
      [this event] (f))))

; Item management ----------------------------------------------------------

(defn now []
  (.getTime (java.util.Date.)))

(defn to-mins [msec]
  (quot msec 60000))

(defn to-str [mins]
  (format "[%02d:%02d]" (quot mins 60) (mod mins 60)))

(defn to-time [x unit]
  (Math/round (* (Float. x) ({"m" 1 "h" 60 "d" 1440} unit))))

(defn key-task [task]
  (str (:project task) (:description task)))

(defn parition-items [items]
  ; first group by priority into a {pri item} map,
  ; then group the first 10 items of every priority by project
  (reduce (fn [a [pri item]]
            (assoc a pri (group-by :project (take 10 item))))
          {}
          (group-by :priority items)))

(defn update-items [file menu items active actfn deactfn]
  ; project / priority section functions
  (let [add-section
        (fn [add-sep? title sec-items]
          (if add-sep?
            (.addSeparator menu))
          (.add menu title)
          (.addSeparator menu)
          (doseq
            [item sec-items]
            (let [new-item (MenuItem. (if (= (key-task item) (key-task active))
                                        (str "➡ " (:description item))  ;◆✦●
                                        (:description item)))]
              (.addActionListener new-item
                                  (action #(actfn (assoc item :since (now)))))
              (.add menu new-item))))
        add-priority
        (fn [add-sep? priority prjs]
          (add-section
            add-sep?
            (str "Priority " (inc priority) " - " (key (first prjs)))
            (val (first prjs)))
          (doseq [[prj items] (next prjs)] (add-section true prj items)))]

    ; remove old items
    (doseq [index (range (.getItemCount menu))] (.remove menu 0))

    ; add "now working" section
    (when active
      (let [stime (to-mins (- (now) (:since active)))]
        (.add
          menu
          (MenuItem. (str "Session: "
                          (to-str stime)
                          " - Sum: "
                          (to-str (+ (:time active) stime)))))
        (.addSeparator menu)
        (let [stop-item (MenuItem. "Stop work")]
          (.addActionListener stop-item (action #(deactfn)))
          (.add menu stop-item))
        (.addSeparator menu)))

    ; add items sorted by priority and project
    (let [part-items (sort (parition-items items))]
      (if (first part-items)
        (add-priority false (key (first part-items)) (val (first part-items)))
        (.add menu (MenuItem. (str "No tasks in " file))))
      (doseq [[pri prjs] (next part-items)] (add-priority true pri prjs)))

    ; quit menu item
    (.addSeparator menu)
    (let [quit-item (MenuItem. "Quit Atea")]
      (.addActionListener quit-item (action #((deactfn) (System/exit 0))))
      (.add menu quit-item))))

; IO -----------------------------------------------------------------------

(defn escape [s]
  (string/replace s ";" "\\;"))

(defn unescape [s]
  (string/replace s "\\;" ";"))

; tracked tasks
(defn parse-status [line]
  (let [match (re-matches #"# Working on \"\[(.*)\]\" - \"(.*)\" since (\d*) for (\d*)" line)]
    (when match
      {:project (match 1)
       :description (match 2)
       :since (Long. (match 3))
       :time (Long. (match 4))})))

(defn write-status [active]
  (str "# Working on \"[" (:project active)
       "]\" - \"" (:description active)
       "\" since " (:since active)
       " for " (:time active)))

(defn parse-ttask [line]
  (let [match (re-matches #"\[(.*)\];(.*);(\d*);(\d*);(\d*)" line)]
    (when match
      {:project (unescape (match 1))
       :description (unescape (match 2))
       :priority (Long. (match 3))
       :time (Long. (match 4))
       :estimate (Long. (match 5))})))

(defn write-ttask [ttask]
  (apply format "[%s];%s;%d;%d;%d"
         (escape (:project ttask))
         (escape (:description ttask))
         (map ttask [:priority :time :estimate])))

(defn load-ttasks [file]
  (try
    (let [lines (filter (comp not empty?) (string/split-lines (slurp file)))
          status (when (first lines) (parse-status (first lines)))]
      (if status
        {:active status
         :ttasks (map parse-ttask (next lines))}
        {:active nil
         :ttasks (map parse-ttask lines)}))
    (catch java.io.FileNotFoundException e
      {:active nil
       :ttasks []})))

; tasks
(defn parse-task [line]
  ; format is: [project] description - estimate
  ; project and estimate are optional
  (let [match (re-matches #"(\[(.*)\])?\s*(.*?)(\s*-\s*(\d+\.?\d*)([mhd]))?" line)]
    {:project (or (match 2) "Default")
     :description (match 3)
     :estimate (if (match 5) (to-time (match 5) (match 6)) 0)
     :time 0}))

(defn load-tasks [file]
  (try
    ; filter out all non empty lines starting with one or more whitespaces
    (let [lines (filter
                  #(not (re-matches #"\s+[^\s]+.*" %))
                  (string/split-lines (slurp file)))
          ; partition by empty or all-whitespace lines
          pris (filter
                 #(not (re-matches #"\s*" (first %)))
                 (partition-by (partial re-matches #"\s*") lines))
          tasks (zipmap (range (count pris)) pris)]

      ; flatten into maps
      (for [[pri items] tasks
            task items] (into (parse-task task) {:priority pri :time 0})))
    (catch java.io.FileNotFoundException e [])))

(defn key-tasks [tasks]
  (zipmap (map key-task tasks) tasks))

(defn write-ttasks [file tasks ttasks new-active]
  (try
    (let [active (:active ttasks)
          kts (if active
                (update-in (key-tasks (:ttasks ttasks))
                           [(key-task active) :time]
                           #(+ % (to-mins (- (now) (:since active)))))
                (key-tasks (:ttasks ttasks)))

          ; merge textfile tasks and tracked tasks
          tmerged (vals (merge-with (fn [t tt] {:priority (:priority t)
                                                :project (:project t)
                                                :estimate (:estimate t)
                                                :description (:description t)
                                                :time (:time tt)})
                                    (key-tasks tasks)
                                    kts))

          ; get active time
          tactive (when new-active
                    (assoc new-active :time (get-in kts [(key-task new-active) :time] 0)))

          ; write lines
          lines (map write-ttask tmerged)
          content (string/join "\n" (if tactive
                                      (cons (write-status tactive) lines)
                                      lines))]

      (spit file content))
    (catch java.io.FileNotFoundException e nil)))

; Track file updates -------------------------------------------------------

(defn watch-file [filename interval f]
  (let [file (java.io.File. filename)
        timestamp (atom 0)
        listener (reify java.awt.event.ActionListener
                   (actionPerformed
                     [this event]
                     (if (not= @timestamp (.lastModified file))
                       (do
                         (f)
                         (reset! timestamp (.lastModified file))))))
        timer (javax.swing.Timer. interval listener)]
    (.start timer)
    timer))

; main ---------------------------------------------------------------------

(defn rel-to-home [file]
  (str (System/getProperty "user.home")
       (java.io.File/separator)
       file))

(def default-cfg {:file (rel-to-home "tasks.txt")})

(defn write-default-cfg []
  (spit (rel-to-home ".atea") (pr-str default-cfg)))

(defn load-cfg []
  (try
    (load-file (rel-to-home ".atea"))
    (catch Exception e
      (do
        (write-default-cfg)
        default-cfg))))

(defn ttname [tname]
  (let [match (re-matches #"(.+)\..*" tname)]
    (if match
      (str (match 1) "-times.csv")
      (str tname "-times.csv"))))

(defn create-tray-icon [menu]
  (TrayIcon. (load-icon "clock.png") "Menu" menu))

(defn -main []
  (let [old-file (atom nil)
        icon-inactive (load-icon "clock-inactive.png")
        icon-active (load-icon "clock.png")
        menu (create-menu)
        tray-icon (create-tray-icon menu)]
    (.add (get-tray) tray-icon)
    (.setImage tray-icon icon-inactive)
    (letfn [(update-all []
              (let [file (:file (load-cfg))
                             tfile (ttname file)
                             tasks (load-tasks file)
                             ttasks (load-ttasks tfile)]
                         (when (and @old-file (not= @old-file file))
                           (write-ttasks (ttname @old-file)
                                         (load-tasks @old-file)
                                         (load-ttasks (ttname @old-file))
                                         nil))
                         (when tasks
                           (reset! old-file file)
                           (update-items file menu tasks (:active ttasks)
                                         (fn [new-active]
                                           (write-ttasks tfile tasks ttasks new-active)
                                           (.setImage tray-icon icon-active))
                                         (fn []
                                           (write-ttasks tfile tasks ttasks nil)
                                           (.setImage tray-icon icon-inactive)))))
              (doseq [i (range 0 (.getItemCount menu))]
                (.addActionListener (.getItem menu i) (action #(update-all)))))]
      (update-all))
    (Thread/sleep (Long/MAX_VALUE))))
