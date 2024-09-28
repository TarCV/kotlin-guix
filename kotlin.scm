(define-module (guix-packager)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system ant)
  #:use-module (guix git-download)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages java)
  #:use-module (gnu packages protobuf))

;; TODO: enable tests where possible, otherwise mark a package not public
;; TODO: replace vendored sources with downloaded sources
;; TODO: verify output directories
;; TODO: verify quasiquotes and other magic characters
;; TODO: should intellij packages be merged, or their patches be splitted?
;; TODO: compare dart sources with each other
;; TODO: ensure (find-files "*.jar") finds only a single file

(define (link-input-jars target-dir package-names)
  `(lambda* (#:key inputs #:allow-other-keys)
    (mkdir-p ,target-dir)
    (for-each
      (lambda (p)
        (let*
          ((allJars (find-files
                      (assoc-ref inputs p)
                      ;; Exclude javadoc and other variants
                      "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
            (mainJar (if (= 1 (length allJars))
                       (car allJars)
                       (throw 'no-or-multiple-jars-found p))))

          (symlink
            mainJar
            (string-append ,target-dir "/" (basename mainJar)))))
      ,package-names)))

(define ant-contrib
  (package
    (name "ant-contrib")
    (version "1.0b3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.sourceforge.net/ant-contrib/ant-contrib/" version "/ant-contrib-" version "-src.tar.gz"))
        (sha256 (base32 "1mxmhkqc8k7160696alsyh9gq1j9ijsi0m6kw6dzbbl4kkxklfvg"))
        (patches '("patches/ant-contrib.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "lib")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (native-inputs
      (list java-commons-bcel java-commons-httpclient java-junit java-xerces))
    (build-system ant-build-system)
    (arguments
      `(#:make-flags
        ,#~(list (string-append "-Dproject.version=" #$version))
        #:build-target "dist-stage"
        #:tests? #f ;; Has some broken tests
;;        #:test-target "test"
        #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'remove-ivy-dependent-task
              ;; This task implementation depends on old Ivy
              (lambda _
                (delete-file "src/java/net/sf/antcontrib/net/URLImportTask.java")
                ))
            (replace 'install (install-jars "target/stage"))
            (add-after 'install 'install-doc (install-javadoc "target/stage/docs")))))
    (home-page "https://sourceforge.net/projects/ant-contrib/")
    (synopsis "Additional useful tasks and types for Ant")
    (description "Collection of user supplied task (like an <if> task) and a development playground for experimental tasks like a C/C++ compilation task for different compilers.")
    (license license:asl2.0)))

(define java-automaton
  (package
    (name "java-automaton")
    (version "1.12.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/cs-au-dk/dk.brics.automaton.git")
              (commit "75db9df045892a4c59b8994f51e5cbc60be7fdc9")))
        (file-name (git-file-name name version))
        (sha256 (base32 "0a2xndhhb6al26kn77q1i2g9a81pzcybzdckz4818wb3s46p8ayv"))))
    (build-system ant-build-system)
    (arguments
      `(#:build-target "all"
        #:tests? #f ;; This version doesn't have any tests
        #:phases
        (modify-phases %standard-phases
            (replace 'install (install-jars "dist"))
            (add-after 'install 'install-doc (install-javadoc "doc")))))
    (home-page "https://www.brics.dk/automaton/")
    (synopsis "Finite-state automata and regular expressions for Java")
    (description "This Java package contains a DFA/NFA (finite-state automata) implementation with Unicode alphabet (UTF16) and support for the standard regular expression operations (concatenation, union, Kleene star) and a number of non-standard ones (intersection, complement, etc.). In contrast to many other automaton/regexp packages, this package is fast, compact, and implements real, unrestricted regular operations. It uses a symbolic representation based on intervals of Unicode characters.")
    (license license:bsd-3)))

(define-public java-cli-parser
  (package
    (name "java-cli-parser")
    (version "1.1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/spullara/cli-parser.git")
              (commit (string-append "cli-parser-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1f29v4jcnp5nfhhj3kzlryyp0yf97iizbfk1fi8jbhvcpxdajg1w"))))
    (native-inputs
     (list java-junit))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "cli-parser.jar"
        #:source-dir "src/main/java"
        #:test-dir "src/test"
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://github.com/spullara/cli-parser")
    (synopsis "CLI Parser is a tiny (10k jar), super easy to use library for parsing various kinds of command line arguments or property lists.")
    (description "CLI Parser is a tiny (10k jar), super easy to use library for parsing various kinds of command line arguments or property lists. Using annotations on your fields or JavaBean properties you can specify what configuration is available.")
    (license license:asl2.0)))

(define-public protobuf-2.5
  (package
    (inherit protobuf-2)
    (version "2.5.0")
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/google/protobuf/releases/"
                            "download/v" version "/protobuf-"
                            version ".tar.bz2"))
        (sha256
          (base32
          "0xxn9gxhvsgzz2sgmihzf6pf75clr05mqj6218camwrwajpcbgqk"))))))

;; This is the latest version not requiring clients to know about StringLists
(define-public java-protobuf-api-2.5
  (package
    (name "java-protobuf-api")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/google/protobuf/releases/"
                            "download/v" version "/protobuf-"
                            version ".tar.bz2"))
        (sha256
          (base32
          "0xxn9gxhvsgzz2sgmihzf6pf75clr05mqj6218camwrwajpcbgqk"))))
    (native-inputs
     (list java-cglib java-easymock-3.2 java-easymock-class-extension java-junit java-objenesis protobuf-2.5))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "protobuf.jar"
        #:source-dir "java/src/main"
        #:test-dir "java/src/test"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        ,#~(modify-phases %standard-phases
          (add-before 'build 'generate-sources
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "protobuf") "/bin/protoc")
                       "--java_out=java/src/main/java"
                       "--proto_path=src"
                       "src/google/protobuf/descriptor.proto")))
          (add-before 'check 'generate-test-sources
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "protobuf") "/bin/protoc")
                       "--java_out=java/src/test/java"
                       "--proto_path=src"
                       "--proto_path=java/src/test/java"
                       "src/google/protobuf/unittest.proto"
                       "src/google/protobuf/unittest_import.proto"
                       "src/google/protobuf/unittest_import_public.proto"
                       "src/google/protobuf/unittest_mset.proto"
                       "java/src/test/java/com/google/protobuf/multiple_files_test.proto"
                       "java/src/test/java/com/google/protobuf/nested_builders_test.proto"
                       "java/src/test/java/com/google/protobuf/nested_extension.proto"
                       "java/src/test/java/com/google/protobuf/nested_extension_lite.proto"
                       "java/src/test/java/com/google/protobuf/non_nested_extension.proto"
                       "java/src/test/java/com/google/protobuf/non_nested_extension_lite.proto"
                       "java/src/test/java/com/google/protobuf/test_bad_identifiers.proto"
                       "src/google/protobuf/unittest_optimize_for.proto"
                       "src/google/protobuf/unittest_custom_options.proto"
                       "src/google/protobuf/unittest_lite.proto"
                       "src/google/protobuf/unittest_import_lite.proto"
                       "src/google/protobuf/unittest_import_public_lite.proto"
                       "src/google/protobuf/unittest_lite_imports_nonlite.proto"
                       "src/google/protobuf/unittest_enormous_descriptor.proto"
                       "src/google/protobuf/unittest_no_generic_services.proto"))))))
    (home-page "http://protobuf.dev/")
    (synopsis "Protocol Buffers are language-neutral, platform-neutral extensible mechanisms for serializing structured data. This package contains Java API.")
    (description "Protocol buffers are Google’s language-neutral, platform-neutral, extensible mechanism for serializing structured data – think XML, but smaller, faster, and simpler. You define how you want your data to be structured once, then you can use special generated source code to easily write and read your structured data to and from a variety of data streams and using a variety of languages. This package contains Java API.")
    (license license:bsd-3)))

(define java-jetbrains-annotations-java7
  (package
    (inherit java-jetbrains-annotations)
    (arguments
      `(#:make-flags (list "-Dant.build.javac.target=1.7")
      ,@(package-arguments java-jetbrains-annotations)))))

;; IntelliJ 133 vendors ASM 4.0, but we need Java 8 support so use the first major version supporting it
(define java-jetbrains-asm5
  (package
    (inherit java-asm)
    (name "java-jetbrains-asm5")
    (version "5.2")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://gitlab.ow2.org/asm/asm.git")
              (commit (string-append
                        "ASM_"
                        (string-map
                          (lambda (c) (if (char=? c  #\.)  #\_ c))
                          version)))))
        (file-name (git-file-name name version))
        (sha256 (base32 "155xckz8pxdlwf5rn9zhqklxqs2czgfrw6gddsjn70c5lfdmmjxj"))))
    (synopsis "ASM library with patched Java package name")
    (propagated-inputs '())
    ;; Disable tests because base package disables them
    (arguments
      `(#:make-flags (list "-Dant.build.javac.target=1.7")
      ,@(substitute-keyword-arguments (package-arguments java-asm)
        ((#:phases phases)
         `(modify-phases ,phases
            (delete 'remove-bnd-dependency)
            (add-before 'build 'rename-packages
              (lambda _
                (substitute* (find-files "src" "\\.java$")
                  (("([^[:alnum:]])org\\.objectweb\\.asm([^[:alnum:]])" all before after) (string-append before "org.jetbrains.asm4" after))))))))))))

;; Latest version not depending on Java 8 Predicate
(define java-guava-20
  (package
    (inherit java-guava)
    (version "20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/guava/")
                     (commit (string-append "v" version))))
              (file-name (git-file-name "java-guava" version))
              (sha256
               (base32
                "00h5cawdjic1vind3yivzh1f58flvm1yfmhsyqwyvmbvj1vakysp"))))
    ;; Guava tests depend on Truth which has cyclic dependency back on Guava, so tests are disabled for now
    (arguments
      `(#:make-flags (list "-Dant.build.javac.target=1.7")
      ,@(substitute-keyword-arguments (package-arguments java-guava)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'remove-ijrer-imports
              (lambda _
                (substitute*
                  (find-files "." "\\.java$")
                  (("import org.codehaus.mojo.animal_sniffer.IgnoreJRERequirement;") ""))))
            (add-before 'check 'fix-test-target
              (lambda _
                (substitute* "build.xml"
                  (("\\$\\{test\\.home\\}/java") "${test.home}"))))
            (delete 'install-listenablefuture-stub))))))))

(define intellij-util-rt-133
  (package
    (name "intellij-util-rt")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (arguments
      `(#:jar-name "intellij-util-rt.jar"
        #:source-dir "platform/util-rt/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util-rt")
    (description "IntelliJ Platform, util-rt submodule")
    (license license:asl2.0)))

(define intellij-util-rt-134
  (package
    (name "intellij-util-rt")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (arguments
      `(#:jar-name "intellij-util-rt.jar"
        #:source-dir "platform/util-rt/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util-rt")
    (description "IntelliJ Platform, util-rt submodule")
    (license license:asl2.0)))

(define java-jsr166e-seqlock
  (let* ((name "java-jsr166e-seqlock")
        (version "1.7")
        (filename (string-append name "-" version ".java")))
    (package
      (name name)
      (version version)
      (source (origin
          (method url-fetch)
          (uri (string-append "https://gee.cs.oswego.edu/cgi-bin/viewcvs.cgi/jsr166/jsr166/src/jsr166e/extra/SequenceLock.java?revision=" version "&view=co"))
          (file-name filename)
          (sha256 (base32 "1pv9lnj0mb7m50r0q9790jmdrpgnlwg8803ial4z5ip9n3zhnfzh"))))
      (build-system ant-build-system)
      (native-inputs
        (list unzip))
      (arguments
        `(#:jar-name "jsr166e-seqlock.jar"
          #:source-dir "."
          #:tests? #f ;; This class doesn't have tests
          #:phases
          ,#~(modify-phases %standard-phases
            (add-after 'unpack 'fix-file-name
              (lambda _
                (rename-file #$filename "SequenceLock.java"))))))
      (home-page "https://gee.cs.oswego.edu/dl/concurrency-interest/index.html")
      (synopsis "Java implementation of a reentrant mutual exclusion Lock where each lock acquisition/release advances a sequence number")
      (description "Java implementation of a reentrant mutual exclusion Lock in which each lock acquisition or release advances a sequence number. While the implentation source requires only Java6 to compile and run by relying on other jsr166e code, this packages is compiled against JDK 8 having newer implementation of the same dependencies.")
      (license license:cc0))))

(define-public java-picocontainer
  (package
    (name "java-picocontainer")
    (version "1.2")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/picocontainer/PicoContainer1/archive/refs/tags/picocontainer-" version ".tar.gz"))
        (file-name (string-append "picocontainer-" version ".tar.gz"))
        (sha256 (base32 "0xj0981p7zbbx6q7pkpl16bcmddb05qsxsgmwfz1x10xsm3pm892"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jmock-1 java-junit java-xstream))
    (arguments
      `(#:jar-name "picocontainer.jar"
        #:source-dir "container/src/java"
        #:test-dir "container/src/test"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'fix-ant-xml
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}")
                (("<javac " all) (string-append all "encoding=\"iso-8859-1\" "))))))))
    (home-page "http://picocontainer.com/")
    (synopsis "General purpose DI / IOC container")
    (description "General purpose DI / IOC container")
    (license license:bsd-3)))

(define-public java-jetbrains-trove4j
  (let ((commit "29150c19710ef1581c790d0502cf299583db7322")
        (revision "1"))
    (package
      (name "java-jetbrains-trove4j")
      (version (git-version "2016.8.24" revision commit))
      (source (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/JetBrains/intellij-deps-trove4j.git")
                (commit commit)))
          (sha256 (base32 "1bd3bq6i18y0i3bqqkhizqi3cb0x0ja82zk902mqyz84hx8xljwd"))
          (file-name (git-file-name name version))
          (modules '((guix build utils)))
          (snippet '(find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))))
      (native-inputs (list java-junit))
      (build-system ant-build-system)
      (arguments
        `(#:jar-name "jetbrains-trove4j.jar"
          #:source-dir "core/src"
          #:test-dir "test/src"
          #:make-flags (list "-Dant.build.javac.target=1.7")
          #:phases
          (modify-phases %standard-phases
            (add-before 'build 'copy-generated-source
              (lambda _
                ;; TODO: Should the code be regenerated instead?
                (copy-recursively "generated/src"
                                  "core/src")))
              (add-before 'check 'patch-check-target
                (lambda _
                  (substitute* "build.xml"
                    (("\\$\\{test\\.home\\}/java") "")
                    (("<junit.+</junit>") "<java classname=\"gnu.trove.MapTest\" failonerror=\"true\" fork=\"true\"><classpath><pathelement path=\"${env.CLASSPATH}\"/><pathelement path=\"${classes.dir}\"/><pathelement path=\"${test.classes.dir}\"/></classpath></java>")))))))
      (home-page "https://github.com/JetBrains/intellij-deps-trove4j")
      (synopsis "JetBrains fork of the Trove library. The Trove library provides high speed Object and primitive collections for Java.")
      (description "The GNU Trove library has two objectives: 1. Provide \"free\" (as in \"free speech\" and \"free beer\"), fast, lightweight implementations of the java.util Collections API. These implementations are designed to be pluggable replacements for their JDK equivalents. 2. Whenever possible, provide the same collections support for primitive types. This gap in the JDK is often addressed by using the \"wrapper\" classes (java.lang.Integer, java.lang.Float, etc.) with Object-based collections. For most applications, however, collections which store primitives directly will require less space and yield significant performance gains.")
      ;; Some classes are licensed under MIT variant
      (license license:lgpl2.1+))))

(define intellij-boot-133
  (package
    (name "intellij-boot")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (arguments
      `(#:jar-name "intellij-boot.jar"
        #:source-dir "platform/boot/src"
        #:tests? #f
        #:phases
         (modify-phases %standard-phases
           (add-before 'build 'copy-metadata
             (lambda _
               (copy-recursively "platform/boot/src/META-INF" "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Boot")
    (description "IntelliJ Platform, boot submodule")
    (license license:asl2.0)))

(define intellij-boot-134
  (package
    (name "intellij-boot")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (arguments
      `(#:jar-name "intellij-boot.jar"
        #:source-dir "platform/boot/src"
        #:tests? #f
        #:phases
         (modify-phases %standard-phases
           (add-before 'build 'copy-metadata
             (lambda _
               (copy-recursively "platform/boot/src/META-INF" "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Boot")
    (description "IntelliJ Platform, boot submodule")
    (license license:asl2.0)))

(define intellij-compiler-javac2-133
  (package
    (name "intellij-compiler-javac2")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (propagated-inputs
     (list intellij-compiler-instrumentation-util-133))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-compiler-javac2.jar"
        #:source-dir "java/compiler/javac2/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: compiler javac2 module.")
    (description "IntelliJ Platform: compiler javac2 module.")
    (license license:asl2.0)))

(define intellij-compiler-javac2-134
  (package
    (name "intellij-compiler-javac2")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (propagated-inputs
     (list intellij-compiler-instrumentation-util-134))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-compiler-javac2.jar"
        #:source-dir "java/compiler/javac2/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: compiler javac2 module.")
    (description "IntelliJ Platform: compiler javac2 module.")
    (license license:asl2.0)))

(define intellij-compiler-instrumentation-util-133
  (package
    (name "intellij-compiler-instrumentation-util")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (propagated-inputs
     (list java-jetbrains-asm5))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-compiler-instrumentation-util.jar"
        #:source-dir "java/compiler/instrumentation-util/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
          (modify-phases %standard-phases
            (add-before 'build 'patch-for-asm5
              (lambda _
                (substitute*
                  "java/compiler/instrumentation-util/src/com/intellij/compiler/notNullVerification/NotNullVerifyingInstrumenter.java"
                  (("Opcodes\\.ASM4") "Opcodes.ASM5")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: compiler instrumentation-util module.")
    (description "IntelliJ Platform: compiler instrumentation-util module.")
    (license license:asl2.0)))

(define intellij-compiler-instrumentation-util-134
  (package
    (name "intellij-compiler-instrumentation-util")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (propagated-inputs
     (list java-jetbrains-asm5))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-compiler-instrumentation-util.jar"
        #:source-dir "java/compiler/instrumentation-util/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
          (modify-phases %standard-phases
            (add-before 'build 'patch-for-asm5
              (lambda _
                (substitute*
                  "java/compiler/instrumentation-util/src/com/intellij/compiler/notNullVerification/NotNullVerifyingInstrumenter.java"
                  (("Opcodes\\.ASM4") "Opcodes.ASM5")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: compiler instrumentation-util module.")
    (description "IntelliJ Platform: compiler instrumentation-util module.")
    (license license:asl2.0)))

(define intellij-core-api-133
  (package
    (name "intellij-core-api")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list java-automaton intellij-extensions-133))
    (arguments
      `(#:jar-name "intellij-core-api.jar"
        #:source-dir "platform/core-api/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Core API")
    (description "IntelliJ Platform, core-api submodule")
    (license license:asl2.0)))

(define intellij-core-api-134
  (package
    (name "intellij-core-api")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list java-automaton intellij-extensions-134))
    (arguments
      `(#:jar-name "intellij-core-api.jar"
        #:source-dir "platform/core-api/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Core API")
    (description "IntelliJ Platform, core-api submodule")
    (license license:asl2.0)))

(define intellij-core-impl-133
  (package
    (name "intellij-core-impl")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list java-snappy intellij-boot-133 intellij-core-api-133))
    (arguments
      `(#:jar-name "intellij-core-impl.jar"
        #:source-dir "platform/core-impl/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Core implementation")
    (description "IntelliJ Platform, core-impl submodule")
    (license license:asl2.0)))

(define intellij-core-impl-134
  (package
    (name "intellij-core-impl")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list java-snappy intellij-boot-134 intellij-core-api-134))
    (arguments
      `(#:jar-name "intellij-core-impl.jar"
        #:source-dir "platform/core-impl/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
          (modify-phases %standard-phases
            (add-before 'build 'copy-minimal-metadata
              (lambda _
                (copy-recursively "resources-kotlin/src" "build/classes"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Core implementation")
    (description "IntelliJ Platform, core-impl submodule")
    (license license:asl2.0)))

(define intellij-extensions-133
  (package
    (name "intellij-extensions")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations java-jmock-1 java-junit))
    (propagated-inputs
      (list java-xstream intellij-util-133))
    (arguments
      `(#:jar-name "intellij-extensions.jar"
        #:source-dir "platform/extensions/src"
        #:test-dir "platform/extensions/testSrc"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'fix-test-target
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Extensions API")
    (description "IntelliJ Platform, extensions submodule")
    (license license:asl2.0)))

(define intellij-extensions-134
  (package
    (name "intellij-extensions")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations java-jmock-1 java-junit java-hamcrest-all))
    (propagated-inputs
      (list java-xstream intellij-util-134))
    (arguments
      `(#:jar-name "intellij-extensions.jar"
        #:source-dir "platform/extensions/src"
        #:test-dir "platform/extensions/testSrc"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'fix-test-target
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Extensions API")
    (description "IntelliJ Platform, extensions submodule")
    (license license:asl2.0)))

(define intellij-util-133
  (package
    (name "intellij-util")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            ;; Keep "bin/idea.properties" as it is needed for tests
            (rename-file "bin/idea.properties" "idea.properties")
            (delete-file-recursively "bin")
            (mkdir "bin")
            (rename-file "idea.properties" "bin/idea.properties")

            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))

            ;; Delete Mac-only UI classes which are not needed for JPS
            (delete-file "platform/util/src/com/intellij/util/AppleHiDPIScaledImage.java")
            (delete-file "platform/util/src/com/intellij/util/ui/MacUIUtil.java")
            (delete-file-recursively "platform/util/src/com/intellij/ui/mac")
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations java-junit java-hamcrest-all))
    (propagated-inputs
      (list java-cglib java-jakarta-oro java-jdom java-log4j-1.2-api java-native-access java-native-access-platform java-jsr166e-seqlock java-picocontainer intellij-util-rt-133 java-jetbrains-trove4j))
    (arguments
      `(#:jar-name "intellij-util.jar"
        #:source-dir "platform/util/src"
        #:tests? #f ;; TODO: implement additional modules required for the tests
;;         #:test-dir "platform/util/testSrc"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'remove-tests
            (lambda _
              (for-each delete-file
                (list
                  ;; Remove a Mac only test
                  "platform/util/testSrc/com/intellij/util/FoundationTest.java"
                  ;; Remove a UI test
                  "platform/util/testSrc/com/intellij/openapi/ui/SplitterTest.java"
                  ;; Remove tests requiring resources from other modules
                  "platform/util/testSrc/com/intellij/util/diff/DiffTest.java"
                  "platform/util/testSrc/com/intellij/util/io/zip/ReorderJarsTest.java"))))
          (add-before 'build 'fix-test-target
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util")
    (description "IntelliJ Platform, util submodule")
    (license license:asl2.0)))

(define intellij-util-134
  (package
    (name "intellij-util")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            ;; Keep "bin/idea.properties" as it is needed for tests
            (rename-file "bin/idea.properties" "idea.properties")
            (delete-file-recursively "bin")
            (mkdir "bin")
            (rename-file "idea.properties" "bin/idea.properties")

            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))

            ;; Delete Mac-only UI classes which are not needed for JPS
            (delete-file "platform/util/src/com/intellij/util/AppleHiDPIScaledImage.java")
            (delete-file "platform/util/src/com/intellij/util/ui/MacUIUtil.java")
            (delete-file-recursively "platform/util/src/com/intellij/ui/mac")
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations java-junit java-hamcrest-all))
    (propagated-inputs
      (list java-cglib java-jakarta-oro java-jdom java-log4j-1.2-api java-native-access java-native-access-platform java-jsr166e-seqlock java-picocontainer intellij-util-rt-134 java-jetbrains-trove4j))
    (arguments
      `(#:jar-name "intellij-util.jar"
        #:source-dir "platform/util/src"
        #:tests? #f ;; TODO: implement additional modules required for the tests
;;         #:test-dir "platform/util/testSrc"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'remove-tests
            (lambda _
              (for-each delete-file
                (list
                  ;; Remove a Mac only test
                  "platform/util/testSrc/com/intellij/util/FoundationTest.java"
                  ;; Remove a UI test
                  "platform/util/testSrc/com/intellij/openapi/ui/SplitterTest.java"
                  ;; Remove tests requiring resources from other modules
                  "platform/util/testSrc/com/intellij/util/diff/DiffTest.java"
                  "platform/util/testSrc/com/intellij/util/io/zip/ReorderJarsTest.java"))))
          (add-before 'build 'fix-test-target
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util")
    (description "IntelliJ Platform, util submodule")
    (license license:asl2.0)))

(define-public intellij-java-psi-api-133
  (package
    (name "intellij-java-psi-api")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list intellij-core-api-133))
    (arguments
      `(#:jar-name "intellij-java-psi-api.jar"
        #:source-dir "java/java-psi-api/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'copy-messages
            (lambda _
              (mkdir-p "build/classes/messages")
              (copy-recursively "java/java-psi-api/src/messages" "build/classes/messages"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Java PSI API")
    (description "IntelliJ Java psi-api submodule")
    (license license:asl2.0)))

(define-public intellij-java-psi-api-134
  (package
    (name "intellij-java-psi-api")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list intellij-core-api-134))
    (arguments
      `(#:jar-name "intellij-java-psi-api.jar"
        #:source-dir "java/java-psi-api/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'copy-messages
            (lambda _
              (mkdir-p "build/classes/messages")
              (copy-recursively "java/java-psi-api/src/messages" "build/classes/messages"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Java PSI API")
    (description "IntelliJ Java psi-api submodule")
    (license license:asl2.0)))

(define intellij-project-api-134
  (package
    (name "intellij-project-api")
    (version "134")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
              (file-name (string-append "intellij-community-" version ".tar.gz"))
              (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
              (patches '("patches/sdk-134.patch"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                     (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list intellij-core-api-134 intellij-jps-model-api-134))
    (arguments
      `(#:jar-name "intellij-java-impl.jar"
         #:source-dir "platform/projectModel-api/src"
         #:tests? #f ;; This module doesn't have tests
         #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform - Project Model API")
    (description "IntelliJ Platform: projectModel-api submodule")
    (license license:asl2.0)))

(define intellij-java-impl-134
  (package
    (name "intellij-java-impl")
    (version "134")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
              (file-name (string-append "intellij-community-" version ".tar.gz"))
              (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
              (patches '("patches/sdk-134.patch"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                     (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list intellij-project-api-134 intellij-core-api-134))
    (arguments
      `(#:jar-name "intellij-java-impl.jar"
         #:source-dir "java/java-impl/src/com/intellij/psi/impl/compiled"
         #:tests? #f ;; This module doesn't have tests
         #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Java routines")
    (description "IntelliJ Java java-impl submodule")
    (license license:asl2.0)))

(define-public intellij-java-psi-impl-133
  (package
    (name "intellij-java-psi-impl")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list java-jetbrains-asm5 intellij-core-impl-133 intellij-java-psi-api-133))
    (arguments
      `(#:jar-name "intellij-java-psi-impl.jar"
        #:source-dir "java/java-psi-impl/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'copy-messages
            (lambda _
              (mkdir-p "build/classes/messages")
              (copy-recursively "java/java-psi-impl/src/messages" "build/classes/messages"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Java PSI implementation")
    (description "IntelliJ Java psi-impl submodule")
    (license license:asl2.0)))

(define-public intellij-java-psi-impl-134
  (package
    (name "intellij-java-psi-impl")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (propagated-inputs
      (list java-jetbrains-asm5 intellij-core-impl-134 intellij-java-psi-api-134))
    (arguments
      `(#:jar-name "intellij-java-psi-impl.jar"
        #:source-dir "java/java-psi-impl/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'copy-messages
            (lambda _
              (mkdir-p "build/classes/messages")
              (copy-recursively "java/java-psi-impl/src/messages" "build/classes/messages"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Java PSI implementation")
    (description "IntelliJ Java psi-impl submodule")
    (license license:asl2.0)))

(define-public intellij-jps-model-api-133
  (package
    (name "intellij-jps-model-api")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (native-inputs
     (list java-jetbrains-annotations))
    (propagated-inputs
     (list intellij-util-rt-133))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-jps-model-api.jar"
        #:source-dir "jps/model-api/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model API")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains 'model-api' submodule.")
    (license license:asl2.0)))

(define-public intellij-jps-model-api-134
  (package
    (name "intellij-jps-model-api")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (native-inputs
     (list java-jetbrains-annotations))
    (propagated-inputs
     (list intellij-util-rt-134))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-jps-model-api.jar"
        #:source-dir "jps/model-api/src"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model API")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains 'model-api' submodule.")
    (license license:asl2.0)))

(define-public intellij-jps-model-impl-133
  (package
    (name "intellij-jps-model-impl")
    (version "133")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/refs/heads/" version ".tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "18b7k63349xfjqvp0mf8pwpbsdqpbxyg6v25zpmih1w61r1kyf8k"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (native-inputs
     (list java-jetbrains-annotations))
    (propagated-inputs
     (list intellij-jps-model-api-133 intellij-util-133))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-jps-model-impl.jar"
        #:source-dir "jps/model-impl/src"
        #:tests? #f ;; TODO: implement additional modules required for the tests
;;         #:test-dir "jps/model-impl/testSrc"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'fix-test-target
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}"))))
          (add-before 'build 'copy-metadata
            (lambda _
              (copy-recursively "jps/model-impl/src/META-INF" "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model implementation")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains 'model-impl' submodule.")
    (license license:asl2.0)))

(define-public intellij-jps-model-impl-134
  (package
    (name "intellij-jps-model-impl")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (native-inputs
     (list java-jetbrains-annotations))
    (propagated-inputs
     (list intellij-jps-model-api-134 intellij-util-134))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-jps-model-impl.jar"
        #:source-dir "jps/model-impl/src"
        #:tests? #f ;; TODO: implement additional modules required for the tests
;;         #:test-dir "jps/model-impl/testSrc"
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'fix-test-target
            (lambda _
              (substitute* "build.xml"
                (("\\$\\{test\\.home\\}/java") "${test.home}"))))
          (add-before 'build 'copy-metadata
            (lambda _
              (copy-recursively "jps/model-impl/src/META-INF" "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model implementation")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains 'model-impl' submodule.")
    (license license:asl2.0)))

(define intellij-core-kotlin-134
  (package
    (name "intellij-core-kotlin")
    (version "134")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JetBrains/intellij-community/archive/1168c7b8cb4dc8318b8d24037b372141730a0d1f.tar.gz"))
        (file-name (string-append "intellij-community-" version ".tar.gz"))
        (sha256 (base32 "0gw1iihch2hbh61fskp7vqbj7s37z5f19jiaiqxb7wxc2w90cxyz"))
        (patches '("patches/sdk-134.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (native-inputs
      (list unzip))
    (propagated-inputs
      (list java-javax-inject intellij-compiler-javac2-134 intellij-java-psi-impl-134 intellij-jps-model-impl-134))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "intellij-core.jar"
        #:source-dir "empty-src-dir" ;; just jar existing compiled classes, no sources needed
        #:tests? #f
        #:make-flags (list "-Dant.build.javac.target=1.7")
        #:phases
        ,#~(modify-phases %standard-phases
          (add-before 'build 'create-empty-src-dir
            (lambda _
              (mkdir-p "empty-src-dir")))
;          (add-after 'build 'crash
;            (lambda _
;              (invoke "jklfsdfsdfsd")))
          (add-before 'build 'unzip-jars
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "build/classes")
              (for-each
                (lambda (p)
                  (let
                    ((jars (find-files
                              (assoc-ref inputs p)
                              ;; Exclude javadoc and other variants
                              "([[:digit:]]|^[^[:digit:]]+)\\.jar$")))

                      (invoke (string-append #$unzip "/bin/unzip")
                        (if (= 1 (length jars))
                            (car jars)
                            (throw 'multiple-jars-found p))
                        "-d"
                        "build/classes"
                        ;; These files are generated by 'jar' target for each jar file it creates
                        "-x"
                        "META-INF/INDEX.LIST"
                        "META-INF/MANIFEST.MF")))
                (list
                  "java-jdom"
                  "java-javax-inject"

                  "intellij-core-api"
                  "intellij-core-impl"
                  "intellij-extensions"
                  "intellij-java-psi-api"
                  "intellij-java-psi-impl"
                  "intellij-jps-model-api"
                  "intellij-jps-model-impl"
                  "intellij-util"
                  "intellij-util-rt")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ platform: parts required for kotlin")
    (description "This package provides an uberjar consisting of minimal set of modules needed for compiling kotlinc and standard libraries.")
    (license license:asl2.0)))

(define-public kotlin-dart-ast-0.6.786
  (package
    (name "kotlin-dart-ast")
    (version "0.6.786")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "0igjrppp9nd7jb10ydx65xjml96ll9pbbvn8zvidzlbpgyz1mqqi"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./js/js.translator/lib/src/dart-ast-src.jar"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))

              (for-each delete-file
                  (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations unzip))
    (propagated-inputs
      (list intellij-util-133))
    (arguments
      `(#:jar-name "dart-ast.jar"
        #:source-dir "unzipped"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version dart-ast from the Kotlin source code")
    (description "Vendored version dart-ast from the Kotlin source code")
    (license license:bsd-3)))

(define-public kotlin-dart-ast-0.6.1364
  (package
    (name "kotlin-dart-ast")
    (version "0.6.1364")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1xz5q5cf866mcz0i6c24qnx9qldvm6nmfqbdfg8hk0ig2j9ygf15"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./js/js.translator/lib/src/dart-ast-src.jar"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))

              (for-each delete-file
                  (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations unzip))
    (propagated-inputs
      (list intellij-util-133))
    (arguments
      `(#:jar-name "dart-ast.jar"
        #:source-dir "unzipped"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version dart-ast from the Kotlin source code")
    (description "Vendored version dart-ast from the Kotlin source code")
    (license license:bsd-3)))

(define-public kotlin-dart-ast-0.6.1932
  (package
    (name "kotlin-dart-ast")
    (version "0.6.1932")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1r1psqwa5k7klkcyk96rva208zvy76j7nv69v9dfsak2zs1c43ip"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./js/js.translator/lib/src/dart-ast-src.jar"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))

              (for-each delete-file
                  (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations unzip))
    (propagated-inputs
      (list intellij-util-133))
    (arguments
      `(#:jar-name "dart-ast.jar"
        #:source-dir "unzipped"
        #:tests? #f ;; This module doesn't have tests
        #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version dart-ast from the Kotlin source code")
    (description "Vendored version dart-ast from the Kotlin source code")
    (license license:bsd-3)))

(define-public kotlin-0.6.786
  (package
    (name "kotlin")
    (version "0.6.786")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "0igjrppp9nd7jb10ydx65xjml96ll9pbbvn8zvidzlbpgyz1mqqi"))
        (patches '("patches/kotlin-0.6.786.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$")))))
    (native-inputs
      (list ant ant-contrib java-cli-parser java-jline-2 java-guava-20 java-javax-inject java-jetbrains-annotations-java7 java-protobuf-api-2.5 intellij-compiler-javac2-133 intellij-java-psi-impl-133 kotlin-dart-ast-0.6.786 icedtea-7))
    (propagated-inputs '()) ;; TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Dkotlin-home=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             (string-append "-Dbuild.number=" #$version))
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
             ;; Target Java 7 and use its boot classes, because intellij-compiler-javac2 can't instrument classes when they reference any Java 8+ classes (including Object)
             (add-before 'build 'add-bootclasspath
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "build.xml"
                   (("<java classname=\"org\\.jetbrains\\.jet\\.cli\\.jvm\\.K2JVMCompiler\" " all) (string-append all "jvm=\"" (assoc-ref inputs "icedtea") "/bin/java\" ")))))

            (add-before 'build 'prepare-sdk-dir
              (lambda* (#:key inputs #:allow-other-keys)
                ;; build.xml expects exact file names in dependencies directory
                (mkdir-p "dependencies")
                (symlink
                  (string-append
                    (assoc-ref inputs "ant")
                    "/lib/ant.jar")
                  "dependencies/ant.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jline")
                    "/share/java/jline.jar")
                  "dependencies/jline.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-cli-parser")
                    "/share/java/cli-parser.jar")
                  "dependencies/cli-parser-1.1.1.jar")

                (mkdir-p "ideaSDK/core")
                (for-each
                  (lambda (p)
                    (let*
                      ((allJars (find-files
                                (assoc-ref inputs p)
                                ;; Exclude javadoc and other variants
                               "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                        (mainJar (if (= 1 (length allJars))
                                   (car allJars)
                                   (throw 'no-or-multiple-jars-found p))))

                        (symlink
                          mainJar
                          (string-append "ideaSDK/core/" (basename mainJar)))))
                  (list
                    "java-cli-parser"
                    "java-jdom"
                    "java-guava"
                    "java-javax-inject"
                    "java-jetbrains-annotations"
                    "java-jetbrains-asm5"
                    "intellij-core-api"
                    "intellij-core-impl"
                    "intellij-extensions"
                    "intellij-java-psi-api"
                    "intellij-java-psi-impl"
                    "java-picocontainer"
                    "java-jetbrains-trove4j"
                    "intellij-util"
                    "intellij-util-rt"
                  ))

                (mkdir-p "js/js.translator/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "kotlin-dart-ast")
                    "/share/java/dart-ast.jar")
                  "js/js.translator/lib/dart-ast.jar")

                ;; build.xml expects exact file names in ideaSDK/lib
                (mkdir-p "ideaSDK/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-compiler-javac2")
                    "/share/java/intellij-compiler-javac2.jar")
                  "ideaSDK/lib/javac2.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jetbrains-asm5")
                    "/lib/m2/org/ow2/asm/asm/6.0/asm-6.0.jar")
                  "ideaSDK/lib/jetbrains-asm-debug-all-4.0.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-protobuf-api")
                    "/share/java/protobuf.jar")
                  "ideaSDK/lib/protobuf-2.5.0.jar")
                #t))

            (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

(define-public kotlin-0.6.1364
  (package
    (name "kotlin")
    (version "0.6.1364")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1xz5q5cf866mcz0i6c24qnx9qldvm6nmfqbdfg8hk0ig2j9ygf15"))
        (patches '("patches/kotlin-0.6.1364.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$")))))
    (native-inputs
      (list ant ant-contrib java-cli-parser java-jline-2 java-guava-20 java-javax-inject java-jetbrains-annotations-java7 java-protobuf-api-2.5 intellij-compiler-javac2-133 intellij-java-psi-impl-133 kotlin-dart-ast-0.6.1364 icedtea-7 kotlin-0.6.786))
    (propagated-inputs '()) ;; TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Dkotlin-home=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             (string-append "-Dbuild.number=" #$version)
             (string-append "-Dbootstrap.compiler.home=" #$(this-package-native-input "kotlin")))
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
             ;; Target Java 7 and use its boot classes, because intellij-compiler-javac2 can't instrument classes when they reference any Java 8+ classes (including Object)
             (add-before 'build 'add-bootclasspath
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "build.xml"
                  (("<javac2 " all) (string-append all "includeJavaRuntime=\"false\" bootclasspath=\"" (assoc-ref inputs "icedtea") "/lib/rt.jar\" "))
                   (("<java classname=\"org\\.jetbrains\\.jet\\.cli\\.jvm\\.K2JVMCompiler\" " all) (string-append all "jvm=\"" (assoc-ref inputs "icedtea") "/bin/java\" ")))))

            (add-before 'build 'prepare-sdk-dir
              (lambda* (#:key inputs #:allow-other-keys)
                ;; build.xml expects exact file names in dependencies directory
                (mkdir-p "dependencies/ant-1.7/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "ant")
                    "/lib/ant.jar")
                  "dependencies/ant-1.7/lib/ant.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jline")
                    "/share/java/jline.jar")
                  "dependencies/jline.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-cli-parser")
                    "/share/java/cli-parser.jar")
                  "dependencies/cli-parser-1.1.1.jar")

                (mkdir-p "ideaSDK/core")
                (for-each
                  (lambda (p)
                    (let*
                      ((allJars (find-files
                        (assoc-ref inputs p)
                                  ;; Exclude javadoc and other variants
                                  "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                        (mainJar (if (= 1 (length allJars))
                                   (car allJars)
                                   (throw 'no-or-multiple-jars-found p))))

                      (symlink
                        mainJar
                        (string-append "ideaSDK/core/" (basename mainJar)))))
                  (list
                    "java-cli-parser"
                    "java-jdom"
                    "java-guava"
                    "java-javax-inject"
                    "java-jetbrains-annotations"
                    "java-log4j-1.2-api"
                    "java-jetbrains-asm5"
                    "intellij-core-api"
                    "intellij-core-impl"
                    "intellij-extensions"
                    "intellij-java-psi-api"
                    "intellij-java-psi-impl"
                    "java-picocontainer"
                    "java-jetbrains-trove4j"
                    "intellij-util"
                    "intellij-util-rt"
                  ))

                (mkdir-p "js/js.translator/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "kotlin-dart-ast")
                    "/share/java/dart-ast.jar")
                  "js/js.translator/lib/dart-ast.jar")

                ;; build.xml expects exact file names in ideaSDK/lib
                (mkdir-p "ideaSDK/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-compiler-javac2")
                    "/share/java/intellij-compiler-javac2.jar")
                  "ideaSDK/lib/javac2.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jetbrains-asm5")
                    "/lib/m2/org/ow2/asm/asm/6.0/asm-6.0.jar")
                  "ideaSDK/lib/jetbrains-asm-debug-all-4.0.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-protobuf-api")
                    "/share/java/protobuf.jar")
                  "ideaSDK/lib/protobuf-2.5.0.jar")
                #t))

            (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

(define-public kotlin-0.6.1932
  (package
    (name "kotlin")
    (version "0.6.1932")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1r1psqwa5k7klkcyk96rva208zvy76j7nv69v9dfsak2zs1c43ip"))
        (patches '("patches/kotlin-0.6.1932.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$")))))
    (native-inputs
      (list ant ant-contrib java-cli-parser java-jline-2 java-guava-20 java-javax-inject java-jetbrains-annotations-java7 java-protobuf-api-2.5 intellij-compiler-javac2-133 intellij-java-psi-impl-133 intellij-jps-model-impl-133 kotlin-dart-ast-0.6.1932 icedtea-7 kotlin-0.6.1364))
    (propagated-inputs '()) ;; TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Dkotlin-home=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             (string-append "-Dbuild.number=" #$version)
             (string-append "-Dbootstrap.compiler.home=" #$(this-package-native-input "kotlin")))
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
             ;; Target Java 7 and use its boot classes, because intellij-compiler-javac2 can't instrument classes when they reference any Java 8+ classes (including Object)
            (add-before 'build 'add-bootclasspath
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "build.xml"
                  (("<javac2 " all) (string-append all "includeJavaRuntime=\"false\" bootclasspath=\"" (assoc-ref inputs "icedtea") "/lib/rt.jar\" "))
                  (("<java classname=\"org\\.jetbrains\\.jet\\.cli\\.jvm\\.K2JVMCompiler\" " all) (string-append all "jvm=\"" (assoc-ref inputs "icedtea") "/bin/java\" ")))))

            (add-before 'build 'prepare-sdk-dir
              (lambda* (#:key inputs #:allow-other-keys)
                (copy-recursively "runtime/src/org/jetbrains/annotations" "core/util.runtime/src/org/jetbrains")

                ;; build.xml expects exact file names in dependencies directory
                (mkdir-p "dependencies/ant-1.7/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "ant")
                    "/lib/ant.jar")
                  "dependencies/ant-1.7/lib/ant.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jline")
                    "/share/java/jline.jar")
                  "dependencies/jline.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-cli-parser")
                    "/share/java/cli-parser.jar")
                  "dependencies/cli-parser-1.1.1.jar")

                (mkdir-p "ideaSDK/core")
                (for-each
                  (lambda (p)
                    (let*
                      ((allJars (find-files
                                  (assoc-ref inputs p)
                                  ;; Exclude javadoc and other variants
                                  "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                        (mainJar (if (= 1 (length allJars))
                                   (car allJars)
                                   (throw 'no-or-multiple-jars-found p))))

                        (symlink
                        mainJar
                        (string-append "ideaSDK/core/" (basename mainJar)))))
                  (list
                    "java-cli-parser"
                    "java-jdom"
                    "java-guava"
                    "java-javax-inject"
                    "java-jetbrains-annotations"
                    "java-log4j-1.2-api"
                    "java-jetbrains-asm5"
                    "intellij-core-api"
                    "intellij-core-impl"
                    "intellij-extensions"
                    "intellij-java-psi-api"
                    "intellij-java-psi-impl"
                    "intellij-jps-model-impl"
                    "java-picocontainer"
                    "java-jetbrains-trove4j"
                    "intellij-util"
                    "intellij-util-rt"
                  ))

                (mkdir-p "ideaSDK/jps")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-jps-model-impl")
                    "share/java/intellij-jps-model-impl.jar")
                  "ideaSDK/jps/jps-model.jar")

                (mkdir-p "js/js.translator/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "kotlin-dart-ast")
                    "/share/java/dart-ast.jar")
                  "js/js.translator/lib/dart-ast.jar")

                ;; build.xml expects exact file names in ideaSDK/lib
                (mkdir-p "ideaSDK/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-compiler-javac2")
                    "/share/java/intellij-compiler-javac2.jar")
                  "ideaSDK/lib/javac2.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jetbrains-asm5")
                    "/lib/m2/org/ow2/asm/asm/6.0/asm-6.0.jar")
                  "ideaSDK/lib/jetbrains-asm-debug-all-4.0.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-protobuf-api")
                    "/share/java/protobuf.jar")
                  "ideaSDK/lib/protobuf-2.5.0.jar")
                #t))

            (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

(define-public kotlin-0.6.2107
  (package
    (name "kotlin")
    (version "0.6.2107")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1cg1r5xzd0sr8m592j1vyqdqid4plyr3f01lsmg7wal2hfl68mr1"))
        (patches '("patches/kotlin-0.6.2107.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$")))))
    (native-inputs
      (list ant ant-contrib java-cli-parser java-jline-2 java-guava-20 java-javax-inject java-jetbrains-annotations-java7 java-protobuf-api-2.5 intellij-compiler-javac2-133 intellij-java-psi-impl-133 intellij-jps-model-impl-133 icedtea-7 kotlin-0.6.1932))
    (propagated-inputs '()) ;; TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Dkotlin-home=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             (string-append "-Dbuild.number=" #$version)
             (string-append "-Dbootstrap.compiler.home=" #$(this-package-native-input "kotlin"))
             "-verbose")
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
             ;; Target Java 7 and use its boot classes, because intellij-compiler-javac2 can't instrument classes when they reference any Java 8+ classes (including Object)
            (add-before 'build 'add-bootclasspath
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "build.xml"
                  (("<javac2 " all) (string-append all "includeJavaRuntime=\"false\" bootclasspath=\"" (assoc-ref inputs "icedtea") "/lib/rt.jar\" "))
                  (("<java classname=\"org\\.jetbrains\\.jet\\.cli\\.jvm\\.K2JVMCompiler\" " all) (string-append all "jvm=\"" (assoc-ref inputs "icedtea") "/bin/java\" ")))))

            (add-before 'build 'prepare-sdk-dir
              (lambda* (#:key inputs #:allow-other-keys)
                ;; build.xml expects exact file names in dependencies directory
                (mkdir-p "dependencies/ant-1.7/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "ant")
                    "/lib/ant.jar")
                  "dependencies/ant-1.7/lib/ant.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jline")
                    "/share/java/jline.jar")
                  "dependencies/jline.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-cli-parser")
                    "/share/java/cli-parser.jar")
                  "dependencies/cli-parser-1.1.1.jar")

                (mkdir-p "ideaSDK/core")
                (for-each
                  (lambda (p)
                    (let*
                      ((allJars (find-files
                                  (assoc-ref inputs p)
                                  ;; Exclude javadoc and other variants
                                  "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                        (mainJar (if (= 1 (length allJars))
                                   (car allJars)
                                   (throw 'no-or-multiple-jars-found p))))

                        (symlink
                        mainJar
                        (string-append "ideaSDK/core/" (basename mainJar)))))
                  (list
                    "java-cli-parser"
                    "java-jdom"
                    "java-guava"
                    "java-javax-inject"
                    "java-jetbrains-annotations"
                    "java-log4j-1.2-api"
                    "java-jetbrains-asm5"
                    "intellij-core-api"
                    "intellij-core-impl"
                    "intellij-extensions"
                    "intellij-java-psi-api"
                    "intellij-java-psi-impl"
                    "intellij-jps-model-impl"
                    "java-picocontainer"
                    "java-jetbrains-trove4j"
                    "intellij-util"
                    "intellij-util-rt"
                  ))

                (mkdir-p "ideaSDK/jps")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-jps-model-impl")
                    "share/java/intellij-jps-model-impl.jar")
                  "ideaSDK/jps/jps-model.jar")

                ;; build.xml expects exact file names in ideaSDK/lib
                (mkdir-p "ideaSDK/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-compiler-javac2")
                    "/share/java/intellij-compiler-javac2.jar")
                  "ideaSDK/lib/javac2.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jetbrains-asm5")
                    "/lib/m2/org/ow2/asm/asm/6.0/asm-6.0.jar")
                  "ideaSDK/lib/jetbrains-asm-debug-all-4.0.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-protobuf-api")
                    "/share/java/protobuf.jar")
                  "ideaSDK/lib/protobuf-2.5.0.jar")
                #t))

            (add-before 'build 'add-noverify
              (lambda _ (setenv "ANT_OPTS" "-noverify")))

            (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

(define-public kotlin-0.6.2338
  (package
    (name "kotlin")
    (version "0.6.2338")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "16b6z1xw8m0iwizxrqyfg49fii04q2dx9j00fxdvk6rxjyw0l48c"))
        (patches '("patches/kotlin-0.6.2338.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$")))))
    (native-inputs
      (list ant ant-contrib java-cli-parser java-jline-2 java-guava-20 java-javax-inject java-jetbrains-annotations-java7 java-protobuf-api-2.5 intellij-compiler-javac2-133 intellij-java-psi-impl-133 intellij-jps-model-impl-133 icedtea-7 kotlin-0.6.2107))
    (propagated-inputs '()) ;; TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Dkotlin-home=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             (string-append "-Dbuild.number=" #$version)
             (string-append "-Dbootstrap.compiler.home=" #$(this-package-native-input "kotlin"))
             "-verbose")
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
             ;; Target Java 7 and use its boot classes, because intellij-compiler-javac2 can't instrument classes when they reference any Java 8+ classes (including Object)
            (add-before 'build 'add-bootclasspath
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "build.xml"
                  (("<javac2 " all) (string-append all "includeJavaRuntime=\"false\" bootclasspath=\"" (assoc-ref inputs "icedtea") "/lib/rt.jar\" "))
                  (("<java classname=\"org\\.jetbrains\\.jet\\.cli\\.jvm\\.K2JVMCompiler\" " all) (string-append all "jvm=\"" (assoc-ref inputs "icedtea") "/bin/java\" ")))))

            (add-before 'build 'prepare-sdk-dir
              (lambda* (#:key inputs #:allow-other-keys)
                ;; build.xml expects exact file names in dependencies directory
                (mkdir-p "dependencies/ant-1.7/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "ant")
                    "/lib/ant.jar")
                  "dependencies/ant-1.7/lib/ant.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jline")
                    "/share/java/jline.jar")
                  "dependencies/jline.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-cli-parser")
                    "/share/java/cli-parser.jar")
                  "dependencies/cli-parser-1.1.1.jar")

                (mkdir-p "ideaSDK/core")
                (for-each
                  (lambda (p)
                    (let*
                      ((allJars (find-files
                        (assoc-ref inputs p)
                                  ;; Exclude javadoc and other variants
                                  "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                        (mainJar (if (= 1 (length allJars))
                                   (car allJars)
                                   (throw 'no-or-multiple-jars-found p))))

                      (symlink
                        mainJar
                        (string-append "ideaSDK/core/" (basename mainJar)))))
                  (list
                    "java-cli-parser"
                    "java-jdom"
                    "java-guava"
                    "java-javax-inject"
                    "java-jetbrains-annotations"
                    "java-log4j-1.2-api"
                    "java-jetbrains-asm5"
                    "intellij-core-api"
                    "intellij-core-impl"
                    "intellij-extensions"
                    "intellij-java-psi-api"
                    "intellij-java-psi-impl"
                    "intellij-jps-model-impl"
                    "java-picocontainer"
                    "java-jetbrains-trove4j"
                    "intellij-util"
                    "intellij-util-rt"
                  ))

                (mkdir-p "ideaSDK/jps")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-jps-model-impl")
                    "share/java/intellij-jps-model-impl.jar")
                  "ideaSDK/jps/jps-model.jar")

                ;; build.xml expects exact file names in ideaSDK/lib
                (mkdir-p "ideaSDK/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-compiler-javac2")
                    "/share/java/intellij-compiler-javac2.jar")
                  "ideaSDK/lib/javac2.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jetbrains-asm5")
                    "/lib/m2/org/ow2/asm/asm/6.0/asm-6.0.jar")
                  "ideaSDK/lib/jetbrains-asm-debug-all-4.0.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-protobuf-api")
                    "/share/java/protobuf.jar")
                  "ideaSDK/lib/protobuf-2.5.0.jar")
                #t))

            (add-before 'build 'add-noverify
              (lambda _ (setenv "ANT_OPTS" "-noverify")))

            (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

(define-public kotlin-0.6.2451
  (package
    (name "kotlin")
    (version "0.6.2451")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/kotlin.git")
              (commit (string-append "build-" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1ihk7nxdfhird7ai2l3xvjqpb0a717hqvm9g9697w4xq3jil8fla"))
        (patches '("patches/kotlin-0.6.2451.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$")))))
    (native-inputs
      (list ant ant-contrib java-cli-parser java-jline-2 java-guava-20 java-javax-inject java-jetbrains-annotations-java7 java-protobuf-api-2.5 intellij-compiler-javac2-134 intellij-core-kotlin-134 intellij-jps-model-impl-134 icedtea-7 kotlin-0.6.2338))
    (propagated-inputs '()) ;; TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Dkotlin-home=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             (string-append "-Dbuild.number=" #$version)
             (string-append "-Dbootstrap.compiler.home=" #$(this-package-native-input "kotlin"))
             "-Dbootstrap.build.no.tests=true"
             "-verbose")
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
             ;; Target Java 7 and use its boot classes, because intellij-compiler-javac2 can't instrument classes when they reference any Java 8+ classes (including Object)
            (add-before 'build 'add-bootclasspath
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "build.xml"
                  (("<javac2 " all) (string-append all "includeJavaRuntime=\"false\" bootclasspath=\"" (assoc-ref inputs "icedtea") "/lib/rt.jar\" "))
                  (("<java classname=\"org\\.jetbrains\\.jet\\.cli\\.jvm\\.K2JVMCompiler\" " all) (string-append all "jvm=\"" (assoc-ref inputs "icedtea") "/bin/java\" ")))))

            (add-before 'build 'prepare-sdk-dir
              (lambda* (#:key inputs #:allow-other-keys)
                ;; build.xml expects exact file names in dependencies directory
                (mkdir-p "dependencies/ant-1.7/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "ant")
                    "/lib/ant.jar")
                  "dependencies/ant-1.7/lib/ant.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jline")
                    "/share/java/jline.jar")
                  "dependencies/jline.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-cli-parser")
                    "/share/java/cli-parser.jar")
                  "dependencies/cli-parser-1.1.1.jar")

                (mkdir-p "ideaSDK/core")
                (for-each
                  (lambda (p)
                    (let*
                      ((allJars (find-files
                        (assoc-ref inputs p)
                                  ;; Exclude javadoc and other variants
                                  "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                        (mainJar (if (= 1 (length allJars))
                                   (car allJars)
                                   (throw 'no-or-multiple-jars-found p))))

                      (symlink
                        mainJar
                        (string-append "ideaSDK/core/" (basename mainJar)))))
                  (list
                    "java-cli-parser"
                    "java-guava"
                    "java-jetbrains-annotations"
                    "java-log4j-1.2-api"
                    "java-jetbrains-asm5"
                    "intellij-core-kotlin"
                    "java-picocontainer"
                    "java-jetbrains-trove4j"))

                (mkdir-p "ideaSDK/jps")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-jps-model-impl")
                    "share/java/intellij-jps-model-impl.jar")
                  "ideaSDK/jps/jps-model.jar")

                ;; build.xml expects exact file names in ideaSDK/lib
                (mkdir-p "ideaSDK/lib")
                (symlink
                  (string-append
                    (assoc-ref inputs "intellij-compiler-javac2")
                    "/share/java/intellij-compiler-javac2.jar")
                  "ideaSDK/lib/javac2.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-jetbrains-asm5")
                    "/lib/m2/org/ow2/asm/asm/6.0/asm-6.0.jar")
                  "ideaSDK/lib/jetbrains-asm-debug-all-4.0.jar")
                (symlink
                  (string-append
                    (assoc-ref inputs "java-protobuf-api")
                    "/share/java/protobuf.jar")
                  "ideaSDK/lib/protobuf-2.5.0.jar")
                #t))

            (add-before 'build 'add-noverify
              (lambda _ (setenv "ANT_OPTS" "-noverify")))

            (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

kotlin-0.6.2451
