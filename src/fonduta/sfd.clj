(ns fonduta.sfd
  (:require [clojure.string :as string]))

(defn entry [k v]
  (str (name k) ": " v "\n"))

(defn level [n s]
  (if (= n 0) s (level (- n 1) (str " " s))))

(defn segment [s]
  (let [cmd (name (first s))]
    (str (string/join " " (rest s))
         " " cmd " "(if (= cmd "l") 1 0)
         "\n")))

(defn contour [segments]
  (str (segment (first segments))
       (string/join (map (fn [s] (level 1 (segment s))) (rest segments)))))
       
(defn contours [curves]
  (str "Fore\nSplineSet\n"
       (string/join (map contour curves))
       "EndSplineSet\n"))

(defn glyph [[glyph-name code advance & curves]]
  (str (entry :StartChar (name glyph-name))
       (entry :Encoding (string/join " " code))
       (entry :Width (first advance))
       (entry :VWidth (second advance))
       (contours curves)
       "EndChar\n"))

(defn header [head]
  (reduce (fn [a b] (str a (apply entry b))) "" (rest head)))

(defn font [[head & glyphs]]
  (str (header head)
       (entry :BeginChars (str 256 " " (count glyphs)))
       "\n"
       (string/join (map glyph glyphs))
       "EndChars\n"
       "EndSplineFont"))


;; (spit "/Users/daniele/Desktop/abc2.sfd" (font
;; 	      [[:header 
;; 	      [:SplineFontDB 3.0]
;; 	      [:FontName "abc"]
;; 	      [:FullName "abc"]
;; 	      [:FamilyName "abc"]
;; 	      [:Ascent 800]
;; 	      [:Descent 200]
;; 	      [:Encoding "unicode"]
;; 	      [:NameList "Adobe Glyph List"]]
;; 	      [:o [111 111 0] [300 0]
;; 	      [[:m 0 0]
;; 	      [:l 100 0]
;; 	      [:l 100 100]
;; 	      [:l 0 100]
;; 	      [:l 0 0]]]]))