(import ./args :as a)
(import ./traverse :as t)

(def version "DEVEL")

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
             ':vendored |(= (get $ :name) "niche") :tag'

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

