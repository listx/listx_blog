#!/usr/bin/env bb
(require '[clojure.string :as str]
         '[babashka.fs :as fs])

(if (empty? *command-line-args*)
  (binding [*out* *err*]
    (println "Usage: mkpost.sh <WORD_1>...<WORD_N>
    where WORD_1...WORD_N are word(s) separated by whitespace.

Example:
    mkpost.sh foo bar baz")
    (System/exit 1))

  (let [sep "-"
        ;; Handle filename text generation (hyphenated, lowercase)
        joined (->> *command-line-args*
                    (map str/lower-case)
                    (str/join sep))
        date-str (str (java.time.LocalDate/now))
        fname (str date-str "-" joined ".org")

        ;; Handle Org header info (space-separated, original casing)
        title-str (str/join " " *command-line-args*)
        uuid-str (str (java.util.UUID/randomUUID))

        file-content (str ":PROPERTIES:\n"
                          ":ID:       " uuid-str "\n"
                          ":END:\n"
                          "#+title: " title-str "\n\nHello\n")]

    (spit fname file-content)
    (println (str "Created file: " fname))))
