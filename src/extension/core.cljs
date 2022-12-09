(ns extension.core
 (:require [cljs.reader :refer [read-string]]))

(def vscode (js/require "vscode"))
(def parinfer (js/require "parinfer"));



(defn run-indent [text]
  (.indentMode parinfer text))

(defn run-smart [text]
 (.smartMode parinfer text))

(defn run-paren [text]
  (.parenMode parinfer text))

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


(defn reformat-all-with-parinfer [editor editBuilder]
   (let [document          (-> editor .-document)         
         last-index        (dec (.-lineCount ^js document))
         first-line        (.lineAt ^js document 0)
         last-line         (.lineAt ^js document last-index)
         start-file        (.-start (.-range first-line))
         end-file          (.-end   (.-range last-line))        
         delete-range      (new (.-Range vscode) start-file end-file)
         ;selection         (.-selection editor)
         og-text           (when editor (.getText ^js document delete-range))
         new-text          (let [smart   (.-text (run-smart  og-text))
                                 indent  (.-text (run-indent smart))
                                 paren   (.-text (run-paren  indent))]
                             smart)
         same?             (= og-text new-text)
         swapping-fn       (fn [text]
                             (.delete editBuilder delete-range)
                             (.insert ^js editBuilder start-file text))]
        (when (not same?) (swapping-fn new-text))))    
     

(defn reformat-selected [editor editBuilder]
 (let [selection-start   (-> editor .-selection .-start)
       selection-end     (-> editor .-selection .-end)
       delete-range      (new (.-Range vscode) selection-start selection-end)
       selection         (.-selection editor)
       document          (-> ^js editor .-document)
       selected-text     (when editor (.getText document ^js selection))]
   (.delete editBuilder delete-range)
   (.replace editBuilder selection-start (prettify-edn selected-text 2))))

(defn format-selected-map! []
  (let [editor         (-> vscode .-window .-activeTextEditor)]                
   (if editor 
     (.edit ^js editor 
       (fn [editBuilder]        
        (reformat-selected editor editBuilder))))))

(defn run-parinfer []
  (let [editor  (-> vscode .-window .-activeTextEditor)]                
   (if editor 
      (.edit ^js editor 
       (fn [editBuilder]
        (reformat-all-with-parinfer editor editBuilder))))))
       


(defn change-listener []
 (.onDidChangeTextDocument 
    (.-workspace vscode)
    (fn [text-document-change-event]
        (run-parinfer)
        (.log js/console text-document-change-event))))        


(defn activate
  [context]
  (let [command    (-> vscode .-commands)
        disposable (.registerCommand command "extension.helloWorld" #(format-selected-map!))]
     (change-listener)
     (.. context.subscriptions (push disposable))))

(defn deactivate [])

(def exports #js {:activate activate
                  :deactivate deactivate})



