#! /usr/bin/env janet

(comment import ./args :prefix "")
(comment import ./deval :prefix "")
# deval - defensive eval

(def d/atoms
  (invert [:boolean
           :buffer
           :keyword
           :nil
           :number
           :string
           :symbol]))

(defn d/atom?
  [x]
  (truthy? (get d/atoms (type x))))

(comment

  (d/atom? true)
  # =>
  true

  (d/atom? @"hello")
  # =>
  true

  (d/atom? :smile!)
  # =>
  true

  (d/atom? nil)
  # =>
  true

  (d/atom? 3.1415926535)
  # =>
  true

  (d/atom? ``must have goofed up somewhere``)
  # =>
  true

  (d/atom? 'dolphin)
  # =>
  true

  (d/atom? (fiber/new (fn [_] 1)))
  # =>
  false

  (d/atom? '[:head :end])
  # =>
  false

  (d/atom? (map inc [0 1 2]))
  # =>
  false

  (d/atom? {:a 1})
  # =>
  false

  (d/atom? @{:x 0})
  # =>
  false

  )

(def d/colls
  (invert [:array
           :struct
           :table
           :tuple]))

(defn d/coll?
  [x]
  (truthy? (get d/colls (type x))))

(comment

  (d/coll? '[:head :end])
  # =>
  true

  (d/coll? (map inc [0 1 2]))
  # =>
  true

  (d/coll? {:a 1})
  # =>
  true

  (d/coll? @{:x 0})
  # =>
  true

  (d/coll? true)
  # =>
  false

  (d/coll? @"hello")
  # =>
  false

  (d/coll? :smile!)
  # =>
  false

  (d/coll? nil)
  # =>
  false

  (d/coll? 3.1415926535)
  # =>
  false

  (d/coll? ``must have goofed up somewhere``)
  # =>
  false

  (d/coll? 'dolphin)
  # =>
  false

  (d/coll? (fiber/new (fn [_] 1)))
  # =>
  false

  )

(def d/safe-syms
  (invert 
    ~[% %= * *= + ++ += - -- -= -> ->> -?> -?>> / /= < <= = > >=
      accumulate accumulate2 all and any?
      boolean? buffer? bytes?
      case catseq cfunction? cmp comp complement cond count
      dec deep= deep-not= div do
      empty? even? every?
      false? filter find find-index first flatten flatten-into fn
      from-pairs function?
      get get-in group-by
      has-key? has-value?
      idempotent? identity if if-let if-not in inc indexed? interleave
      interpose invert
      keep keys keyword?
      last length lengthable? let
      map mapcat match math/abs max merge merge-into min mod
      nan? nat? neg? next nil? not not= number?
      odd? one? or
      pairs partial partition partition-by pos? postwalk prewalk product
      put put-in
      quasiquote quote
      range reduce reduce2 reverse reverse!
      scan-number seq set short-fn slice some sort sorted sorted-by
      splice string/find string/find-all string/has-prefix?
      string/has-suffix? string? struct? sum symbol?
      table? tabseq toggle true? truthy? tuple? type
      unless unquote update update-in
      values
      walk when when-let while
      zero? zipcoll]))

(defn d/safe-sym?
  [sym]
  (truthy? (or (get d/safe-syms sym)
               (string/has-prefix? "$" sym))))

(comment

  (d/safe-sym? 'all)
  # =>
  true

  (d/safe-sym? 'os/cd)
  # =>
  false

  )

(defn d/safe?
  [form]
  (truthy?
    (cond
      (and (tuple? form) (= :parens (tuple/type form)))
      (let [head (get form 0)]
        (and (d/safe-sym? head)
             (all d/safe? (tuple/slice form 1))))
      #
      (indexed? form)
      (all d/safe? form)
      #
      (dictionary? form)
      (all |(let [[k v] $]
              (and (d/safe? k) (d/safe? v)))
           (pairs form))
      #
      (symbol? form)
      (d/safe-sym? form)
      #
      (d/atom? form)
      true
      #
      false)))

(comment

  (d/safe? true)
  # =>
  true

  (d/safe? @"hello")
  # =>
  true

  (d/safe? :smile!)
  # =>
  true

  (d/safe? nil)
  # =>
  true

  (d/safe? 3.1415926535)
  # =>
  true

  (d/safe? ``must have goofed up somewhere``)
  # =>
  true

  (d/safe? 'and)
  # =>
  true

  (d/safe? 'dolphin)
  # =>
  false

  (d/safe? '[:head :end])
  # =>
  true

  (d/safe? (map inc [0 1 2]))
  # =>
  true

  (d/safe? '(map inc [0 1 2]))
  # =>
  true

  (d/safe? {:a 1})
  # =>
  true

  (d/safe? @{:x 0})
  # =>
  true

  (d/safe? (fiber/new (fn [_] 1)))
  # =>
  false

  (d/safe? '(fiber/new (fn [_] 1)))
  # =>
  false

  (d/safe? '|(+ $ 80))
  # =>
  true

  (d/safe? '|(os/cwd))
  # =>
  false

  (d/safe? '(map pp [:a :b]))
  # =>
  false

  (d/safe? (peg/compile 1))
  # =>
  false

  )



(defn a/parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break {:show-help true}))
  #
  (when (or (not head) (= head "-v") (= head "--version"))
    (break {:show-version true}))
  #
  (array/remove the-args 0)
  #
  (def [opts file-path]
    (if-not (and (string/has-prefix? "{" head)
                 (string/has-suffix? "}" head))
      [@{} head]
      (let [parsed
            (try (parse (string "@" head))
              ([e] (eprint e)
                   (errorf "failed to parse options: %n" head)))]
        (assertf (and parsed (table? parsed))
                 "expected table but found: %s" (type parsed))
        (def opts parsed)
        (def new-head (get the-args 0))
        (array/remove the-args 0)
        (assertf new-head "expected a command but found none: %n" args)
        [opts new-head])))
  #
  (def input
    (if (= "-" file-path)
      stdin
      (do
        (assertf (= :file (os/stat file-path :mode))
                 "not a file path: %s" file-path)
        #
        (os/realpath file-path))))
  # XXX: improve feedback message?
  (assertf (<= 1 (length the-args))
           "need at least one more argument: %n" the-args)
  #
  (def [path-str] the-args)
  (assertf (parse-all path-str) "could not parse: %n" path-str)
  #
  (var path (parse-all path-str))
  # XXX: using `tuple?` below, but the expected input is a tuple
  #      that represents a short-fn.  things of the form |(...)
  #      can be used, e.g.
  #
  #        |(= (get $ :name) "niche")
  #
  #      because the parser "expands" this to:
  #
  #        (short-fn (= (get $ :name) "niche"))
  #
  (let [first-step (first path)]
    (if (symbol? first-step)
      (assertf (string/has-prefix? "@" first-step)
               "leading symbol item must start with @: %n" first-step)
      (assertf (or (keyword? first-step)
                   (nat? first-step)
                   (tuple? first-step))
               "unexpected type for first step in path: %n" first-step)))
  #
  (assertf (all |(or (keyword? $) (nat? $) (tuple? $))
                (slice path 1))
           "only keywords, natural numbers, tuples allowed: %n"
           (slice path 1))
  #
  (array/remove the-args 0)
  #
  (def top-level-index
    (let [first-step (get path 0)]
      (if (and (symbol? first-step)
               (string/has-prefix? "@" (slice first-step 0 1)))
        (let [scanned (scan-number (slice first-step 1))]
          (set path [;(slice path 1)])
          (if (number? scanned)
            scanned
            # XXX: haven't settled on whether a keyword is better here
            nil))
        0)))
  #
  (assertf (d/safe? path) "path might have unsafe elements: %n" path)
  (def checked-path (eval path))
  #
  (merge opts
         {:input input
          :top-level-index top-level-index
          :path checked-path
          :rest the-args}))


(comment import ./traverse :prefix "")
(comment import ./get :prefix "")
(comment

  (def path-str ":vendored 2 :tag")

  (def value `"7caf81f636bb97104aada6544219733b3c86badf"`)

  # bundle/info.jdn
  (def src
    ``
    {:name "ghost"
     :url "https://github.com/sogaiu/ghost"
     :repo "git+https://github.com/sogaiu/ghost"
     :vendored
     [{:name "some-bundle-bits"
       :url "https://github.com/sogaiu/some-bundle-bits"
       :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
       :paths [["sbb.janet" "bundle/"]]}
      {:name "jell"
       :url "https://github.com/sogaiu/jell"
       :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
       :paths [["jell" "bin/"]]}
      {:name "niche"
       :url "https://github.com/sogaiu/niche"
       :tag "7caf81f636bb97104aada6544219733b3c86badf"
       :paths [["niche.janet" "bin/"]]}]}
    ``)

  (def ds (parse src))

  (def path (parse-all path-str))
  # =>
  @[:vendored 2 :tag]

  (def step-0 (get ds (get path 0)))
  # =>
  [{:name "some-bundle-bits"
    :paths [["sbb.janet" "bundle/"]]
    :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
    :url "https://github.com/sogaiu/some-bundle-bits"}
   {:name "jell"
    :paths [["jell" "bin/"]]
    :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
    :url "https://github.com/sogaiu/jell"}
   {:name "niche"
    :paths [["niche.janet" "bin/"]]
    :tag "7caf81f636bb97104aada6544219733b3c86badf"
    :url "https://github.com/sogaiu/niche"}]

  (def step-1 (get step-0 (get path 1)))
  # =>
  {:name "niche"
   :paths [["niche.janet" "bin/"]]
   :tag "7caf81f636bb97104aada6544219733b3c86badf"
   :url "https://github.com/sogaiu/niche"}

  (def step-2 (get step-1 (get path 2)))
  # =>
  "7caf81f636bb97104aada6544219733b3c86badf"

  )

(defn g/get-via-path
  [ds path]
  (var context ds)
  (def new-path @[])
  (each step path
    (cond
      (or (keyword? step) (nat? step))
      (do
        (def new-context (get context step))
        (assertf new-context "failed to take a step: %n in context: %n"
                 step context)
        (array/push new-path step)
        (set context new-context))
      #
      (function? step)
      (do
        (assertf (indexed? context)
                 "expected indexed, found: %n" context)
        (var capture nil)
        (eachp [i elt] context
          (when (step elt)
            (array/push new-path i)
            (set capture elt)
            (break)))
        (when (nil? capture)
          (errorf "failed to find target item in: %n" context))
        #
        (set context capture))
      #
      (errorf "unexpected value: %n" step)))
  #
  [context new-path])

(comment

  (def src
    ``
    {:name "ghost"
     :url "https://github.com/sogaiu/ghost"
     :repo "git+https://github.com/sogaiu/ghost"
     :vendored
     [{:name "some-bundle-bits"
       :url "https://github.com/sogaiu/some-bundle-bits"
       :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
       :paths [["sbb.janet" "bundle/"]]}
      {:name "jell"
       :url "https://github.com/sogaiu/jell"
       :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
       :paths [["jell" "bin/"]]}
      {:name "niche"
       :url "https://github.com/sogaiu/niche"
       :tag "7caf81f636bb97104aada6544219733b3c86badf"
       :paths [["niche.janet" "bin/"]]}]}
    ``)

  (def ds (parse src))

  (g/get-via-path ds [:vendored 2 :tag])
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  (g/get-via-path ds [:vendored
                    |(= (get $ :name) "niche")
                    :tag])
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  )



(defn t/scan-src
  [src top-level-index path]
  # check source string
  (def [ok? data] (protect (parse-all src)))
  (when (not ok?)
    (errorf "failed to parse content"))
  #
  (var tl-idx nil)
  # determine top-level item to operate on
  (def top-level-item
    (if (nat? top-level-index)
      (get data top-level-index)
      (let [first-step (get path 0)]
        (assertf (function? first-step)
                 "first item of path should be a function: %n"
                 first-step)
        (var capture nil)
        (eachp [i elt] data
          (when (first-step elt)
            (set tl-idx i)
            (set capture elt)
            (break)))
        (when (nil? capture)
          (errorf "failed to find target top-level item: %n"
                  data))
        #
        capture)))
  (assertf top-level-item
           "failed to determine top-level item for: %n" data)
  # adjust path if needed
  (def mod-path (if tl-idx (slice path 1) path))
  # finalize top-level index
  (set tl-idx (cond
                (nat? top-level-index) top-level-index
                (nat? tl-idx) tl-idx
                (errorf "failed to determine top-level index")))
  # non-zipper traversal to learn various things
  [;(g/get-via-path top-level-item mod-path) tl-idx])



(def version "2026-04-09_08-20-17")

(def usage
  `````
  Usage: visito <file> <path>
         visito - <path>

         visito [-h|--help]|[-v|--version]

  Display value reachable via a specific path in JDN content.

  Parameters:

    <file>                 path to `.jdn` file
    <path>                 describes "path" to target
    -                      read JDN content from standard input

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    Show :name's associated value from `bundle/info.jdn`:

    $ visito bundle/info.jdn ':name'

    Show :name's associated value from standard input content:

    $ cat bundle/info.jdn | visito - ':name'

    Show the included path at index 0 from `.niche.jdn`:

    $ visito .niche.jdn ':includes 0'

    Show tag value for a specific vendored dependency in
    `bundle/info.jdn`:

    $ visito bundle/info.jdn \
             ':vendored |(= (get $ :name) "tweake") :tag'

  `````)

########################################################################

(defn visit
  [src top-level-index path]
  (def [found-value _ _]
    (t/scan-src src top-level-index path))
  #
  found-value)

(comment

  (visit `{:key "value"}` 0 [:key])
  # =>
  "value"

  (def src
    ``
    [{:name "alice"
      :value 1}
     {:name "bob"
      :value 2}]
    ``)

  (visit src 0 [0 :value])
  # =>
  1

  (visit src 0 [|(= (get $ :name) "bob") :value])
  # =>
  2

  (def project-janet-src
    ``
    (declare-project
      :name "janet-peg"
      :url "https://github.com/sogaiu/janet-peg")

    (declare-source
      :prefix "janet-peg"
      :source @["lib"])
    ``)

  (visit project-janet-src 1 [2])
  # =>
  "janet-peg"

  (visit project-janet-src 1 [])
  # =>
  '(declare-source
     :prefix "janet-peg"
     :source @["lib"])

  (visit project-janet-src
         nil
         [|(= (get $ 0) 'declare-source)])
  # =>
  '(declare-source
     :prefix "janet-peg"
     :source @["lib"])

  )

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def {:input input
        :path path
        :top-level-index top-level-index} opts)
  #
  (def [ok? src] (protect (if (= input stdin)
                            (file/read input :all)
                            (slurp input))))
  (when (not ok?)
    (errorf "failed to read in: %s" input))
  #
  (def value (visit src top-level-index path))
  #
  (printf "%m" value))

