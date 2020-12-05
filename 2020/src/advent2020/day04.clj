(ns advent2020.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def testdata "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def testdata2 "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def testdata3 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(def data (slurp "day04.txt"))

(defn split-passports [str]
  (clojure.string/split str #"\r?\n\r?\n"))

(defn parse-passport [passport]
  (->> (clojure.string/split passport #"[ \r\n]+")
       (map #(clojure.string/split % #":"))
       (into {})))

(def required-passport-fields ["byr"
                               "iyr"
                               "eyr"
                               "hgt"
                               "hcl"
                               "ecl"
                               "pid"
                               ;"cid"
                               ])

(def eye-colors ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])

(defn valid-passport? [passport]
  (every? #(contains? passport %) required-passport-fields))

(defn between [min x max]
  (and (<= min x)
       (<= x max)))

(defn extra-valid-passport? [passport]
  (and (between 1920 (Integer/parseInt (get passport "byr")) 2002)
       (between 2010 (Integer/parseInt (get passport "iyr")) 2020)
       (between 2020 (Integer/parseInt (get passport "eyr")) 2030)
       (let [h (re-matches #"([0-9]+)(in|cm)" (get passport "hgt"))]
         (if (not h) nil
             (let [[_ height unit] h]
               (case unit
                 "cm" (between 150 (Integer/parseInt height) 193)
                 "in" (between 59 (Integer/parseInt height) 76)
                 false))))
       (re-matches #"#[0-9a-f]{6}" (get passport "hcl"))
       (some #(= (get passport "ecl") %) eye-colors)
       (re-matches #"[0-9]{9}" (get passport "pid"))
       ))

(->> data
     split-passports
     (map parse-passport)
     (filter valid-passport?)
     (filter extra-valid-passport?)
     count)


