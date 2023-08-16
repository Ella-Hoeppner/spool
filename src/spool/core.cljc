(ns spool.core)

(defmacro as-> [expr name & forms]
  (let [forms (map (fn [form]
                     (if (seq? form)
                       (if (some #{name} (flatten form))
                         form
                         `(~(first form) ~name ~@(next form)))
                       (list form name)))
                   forms)]
    `(let [~name ~expr
           ~@(interleave (repeat name) (butlast forms))]
       ~(if (empty? forms)
          name
          (last forms)))))

(defmacro as->> [expr name & forms]
  (let [forms (map (fn [form]
                     (if (seq? form)
                       (if (some #{name} (flatten form))
                         form
                         `(~(first form) ~@(next form) ~name))
                       (list form name)))
                   forms)]
    `(let [~name ~expr
           ~@(interleave (repeat name) (butlast forms))]
       ~(if (empty? forms)
          name
          (last forms)))))

(defmacro fn-> [& forms]
  (let [arg-name (gensym)]
    `(~'fn [~arg-name]
           (-> ~arg-name
               ~@forms))))

(defmacro fn-as-> [name & forms]
  (let [arg-name (gensym)]
    `(~'fn [~arg-name]
           (as-> ~arg-name ~name ~@forms))))

(defmacro fn-as->> [name & forms]
  (let [arg-name (gensym)]
    `(~'fn [~arg-name]
           (as->> ~arg-name ~name ~@forms))))
