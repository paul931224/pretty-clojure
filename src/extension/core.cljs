(ns extension.core
 (:require [cljs.reader :refer [read-string]]))

(def vscode (js/require "vscode"))


(defn space-maker [number]
  (apply str (repeat number " ")))


(defn convert [prettify-edn value offset]
 (let [string-val? (string? value)
       map-val?    (map? value)]
  (cond 
    string-val? (str "\"" value "\"")
    map-val?    (prettify-edn (str value) offset)
    :else value)))

(defn prettify-edn [selected-text offset]
  (let [the-map (read-string selected-text)
        is-map? (map? the-map)]
    (when is-map?
     (let [map-keys  (keys the-map)
           keys-lengths (map count (map str map-keys))
           longest-key  (apply max keys-lengths)
           key-column-width (+ longest-key offset)]
      (str (reduce 
            (fn [till-now [now-key now-value]]
             (let [now-key-length (count (str now-key))
                   space-fill-number (- key-column-width now-key-length)
                   maybe-space (if (not= 1 (count till-now)) " " "")] 
                (str till-now 
                     maybe-space 
                     now-key 
                     (space-maker space-fill-number) 
                     (convert prettify-edn now-value offset) 
                     "\n")))
            "{"
            the-map) "}")))))


(defn replace-text []
  (let [editor         (-> vscode .-window .-activeTextEditor)
        selection      (.-selection editor)
        document       (-> editor .-document)
        selected-text  (when editor (.getText document selection))]
   (if editor 
     (.edit ^js editor 
       (fn [editBuilder]
        (let [selection-start  (-> editor .-selection .-start)
              selection-end    (-> editor .-selection .-end)
              delete-range (new (.-Range vscode) selection-start selection-end)]
          (.delete editBuilder delete-range)
          (.replace editBuilder selection-start (prettify-edn selected-text 2))))))))
          

(defn activate
  [context]
  (let [command    (-> vscode .-commands)
        disposable (.registerCommand command "extension.helloWorld" #(replace-text))]

    (.. context.subscriptions (push disposable))))

(defn deactivate [])

(def exports #js {:activate activate
                  :deactivate deactivate})



