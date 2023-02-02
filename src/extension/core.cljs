(ns extension.core
  (:require [cljs.reader :refer [read-string]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.core.async :refer [go]]))

(def vscode (js/require "vscode"))
(def parinfer (js/require "parinfer"));

(def editor  (-> vscode .-window .-activeTextEditor))

(def space? (atom false))

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
  (.smartMode parinfer text  options))

(defn run-paren [text options]
  (.parenMode parinfer text options))


(def prev-cursor (atom {:prevCursorLine 0
                        :prevCursorX    0}))


(defn set-prev-cursor! []
  (let [selection-start (-> editor .-selection .-start)]
    (reset! prev-cursor {:prevCursorLine (.-line      ^js selection-start)
                         :prevCursorX    (.-character ^js selection-start)})))




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





(defn Position--obj [line char]
  (new (.-Position vscode) line char))

(defn Selection--obj [start-Position--obj end-Position--obj]
  (new (.-Selection vscode) start-Position--obj end-Position--obj))


(defn set-cursor! [selection]
  (set! (.-selection editor) selection))

(def set-cursor? (atom false))

(defn run-parinfer [{:keys [changes backspace?]}]
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
          smart-result
          (run-smart
           og-text #js {:changes        ^js changes
                        :prevCursorLine (:prevCursorLine @prev-cursor)
                        :prevCursorX    (:prevCursorX    @prev-cursor)
                        :cursorLine     (.-line      ^js selection-start)
                        :cursorX        (.-character ^js selection-start)})
          smart-text        (.-text smart-result)
          smart-cursor      (let [new-position   (Position--obj (.-cursorLine smart-result)
                                                                (.-cursorX    smart-result))
                                  new-selection  (Selection--obj new-position new-position)]
                              new-selection)
          same?             (= og-text smart-text)]

      (when @set-cursor?
        (do
          (set-cursor! smart-cursor)
          (reset! set-cursor? false)))
      (if (not same?)
        (do
          (.edit ^js editor
                 (fn [editBuilder]
                   (reformat-all-with-parinfer editBuilder smart-text))
                 #js {:options? #js {:undoStopAfter  false
                                     :undoStopBefore false}})
          (reset! set-cursor? true))))))




(defn change-listener []
  (.onDidChangeTextDocument
   (.-workspace vscode)
   (fn [text-document-change-event]
     (let [document                  (-> editor .-document)
           content-changes           (.-contentChanges ^js text-document-change-event)
           some-changes?             (not (empty? content-changes))
           first-content-change      (js->clj (first content-changes))
           range-changed             (get first-content-change "range")
           range-changed-start       (.-start        ^js range-changed)
           range-changed-start-line  (.-line         ^js range-changed-start)
           range-changed-start-ch    (.-character    ^js range-changed-start)
           range-changed-end         (.-end          ^js range-changed)
           range-changed-end-line    (.-line         ^js range-changed-end)
           range-changed-end-ch      (.-character    ^js range-changed-end)
           start-position            (Position--obj range-changed-start-line range-changed-start-ch)
           end-position              (Position--obj range-changed-end-line   range-changed-end-ch)
           selection                 (Selection--obj start-position end-position)
           new-text-in-range         (get first-content-change "text")
           old-text-in-range         (.getText document ^js selection)
           changes                   #js [#js {:lineNo   range-changed-start-line
                                               :x range-changed-start-ch
                                               :oldText old-text-in-range
                                               :newText new-text-in-range}]]

       (when (and (not @space?) some-changes?)
         (do
           (println "faster?")
           (run-parinfer {:changes changes})))
       (reset! space? false)

       (set-prev-cursor!)))))


(defn insert-space []
  (let [selection-start   (-> editor .-selection .-start)]
    (.edit editor
           (fn [edit-builder]
             (.insert ^js edit-builder selection-start " ")))))

(defn activate
  [context]
  (let [command    (-> vscode .-commands)
        disposable-1 (.registerCommand command "extension.pretty-edn" #(format-selected-map!))
        disposable-1 (.registerCommand command "extension.space"      #(do
                                                                         (insert-space)
                                                                         (reset! space? true)
                                                                         (.log js/console "space pressed.")))]
    (change-listener)
    (.addEventListener js/window "keydown" (fn [event] (.log js/console "hello: " event)))
    (.. context.subscriptions (push disposable-1))))

(defn deactivate [])

(def exports #js {:activate activate
                  :deactivate deactivate})



