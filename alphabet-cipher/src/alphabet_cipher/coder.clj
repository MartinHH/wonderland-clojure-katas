(ns alphabet-cipher.coder)

(def min-char \a)
(def max-char \z)

; converts char to a number where \a is 0
(defn char-to-num [c] 
  (- (int c) (int min-char)))

; converts number to char where 0 is \a
(defn num-to-char-mod [n]
  (let [mod-val (+ (char-to-num max-char) 1)
        n-modulo (mod n mod-val)]
    (char (+ (int min-char) n-modulo))))

; encodes 1 char
(defn encode-char [keychar char]
  (num-to-char-mod (+ (char-to-num keychar) (char-to-num char))))

; decodes 1 char
(defn decode-char [keychar char]
  (num-to-char-mod(- (char-to-num char) (char-to-num keychar))))

; helper-function for decipher
(defn find-repitition [word]
  (let [max (count word)]
    (loop [i 1]
      (if (>= i max)
        word (let [candidate (subs word 0 i)
                   repeated (reduce str (take max (cycle candidate)))]
          (if (= repeated word) candidate (recur (inc i)))
        ))))
  )

; converts message via a given char-codec-function
(defn code [keyword message codec]
  (let [cyclic-key (cycle (seq keyword))
        converted (map codec cyclic-key (seq message))]
    (reduce str converted)))

(defn encode [keyword message]
  (code keyword message encode-char))

(defn decode [keyword message]
  (code keyword message decode-char))

(defn decipher [cipher message]
  (let [cycled (decode message cipher)]
    (find-repitition cycled)))
  

