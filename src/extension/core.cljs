(ns extension.core
  (:require [cljs.reader :refer [read-string]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.core.async :refer [go]]))

(def vscode (js/require "vscode"))
(def parinfer (js/require "parinfer"));

(def editor  (-> vscode .-window .-activeTextEditor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom formatters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (if editor
    (go (let [edit-ready? (<p! (.edit ^js editor (fn [editBuilder] (reformat-selected editor editBuilder))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom formatters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defn run-indent [text options]
  (.indentMode parinfer text options))

(defn run-smart [text options]
  (.log js/console options)
  (.smartMode parinfer text  options))

(defn run-paren [text options]
  (.parenMode parinfer text options))


(def prev-cursor (atom {:prevCursorLine 0
                        :prevCursorX    0}))


(defn set-prev-cursor! []
  (let [selection-start (-> editor .-selection .-start)]
    (reset! prev-cursor {:prevCursorLine (.-line      ^js selection-start)
                         :prevCursorX    (.-character ^js selection-start)})))

(defn reset-cursor! [selection]
  (set! (.-selection editor) selection))


(defn reformat-all-with-parinfer [editBuilder smart-text]
  (let [document          (-> editor .-document)
        last-index        (dec (.-lineCount ^js document))
        first-line        (.lineAt ^js document 0)
        last-line         (.lineAt ^js document last-index)
        start-file        (.-start (.-range first-line))
        end-file          (.-end   (.-range last-line))
        delete-range      (new (.-Range vscode) start-file end-file)]

    (.delete editBuilder delete-range)
    (.insert ^js editBuilder start-file smart-text)))







(defn run-parinfer []
  (when editor
    (let [document          (-> editor .-document)
          selection-start   (-> editor .-selection .-start)
          selection-end     (-> editor .-selection .-end)
          first-line        (.lineAt ^js document 0)
          last-index        (dec (.-lineCount ^js document))
          last-line         (.lineAt ^js document last-index)
          start-file        (.-start (.-range first-line))
          end-file          (.-end   (.-range last-line))
          delete-range      (new (.-Range vscode) start-file end-file)
          og-text           (when editor (.getText ^js document delete-range))
          smart-result      (run-smart  og-text #js {:prevCursorLine (:prevCursorLine @prev-cursor)
                                                     :prevCursorX    (:prevCursorX    @prev-cursor)
                                                     :partialResult  true
                                                     :cursorLine     (.-line      ^js selection-start)
                                                     :cursorX        (.-character ^js selection-start)})
          smart-text        (.-text smart-result)
          smart-cursor      (let [new-position   (new (.-Position vscode)
                                                      (.-cursorLine smart-result)
                                                      (.-cursorX    smart-result))
                                  new-selection (new (.-Selection vscode) new-position new-position)]
                              new-selection)
          smart-error       (.-message smart-result)
          same?             (= og-text smart-text)
          a (.log js/console
                  smart-error)]
      (set-prev-cursor!)
      (go (let [edit-ready? (<p! (.edit ^js editor (fn [editBuilder]
                                                     (when (not same?)
                                                       (reformat-all-with-parinfer editBuilder smart-text)))))]
            (when edit-ready? (reset-cursor! smart-cursor)))))))

(defn change-listener []
  (.onDidChangeTextDocument
   (.-workspace vscode)
   (fn [text-document-change-event]
     (let [content-changes (.-contentChanges ^js text-document-change-event)
           some-changes?  (not (empty? content-changes))]
       (when some-changes? (run-parinfer))))))

(defn activate
  [context]
  (let [command    (-> vscode .-commands)
        disposable (.registerCommand command "extension.pretty-edn" #(format-selected-map!))]
    (change-listener)
    (.. context.subscriptions (push disposable))))

(defn deactivate [])

(def exports #js {:activate activate
                  :deactivate deactivate})



