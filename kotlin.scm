(define-module (guix-packager)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system ant)
  #:use-module (guix git-download)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages java)
  #:use-module (gnu packages protobuf))

(define-public ant-contrib
  (package
    (name "ant-contrib")
    (version "r177")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://sourceforge.net/code-snapshots/svn/a/an/ant-contrib/code/ant-contrib-code-" version "-ant-contrib-trunk.zip"))
        (sha256 (base32 "1vgqrqp91sc1213lw7s3pwih169lzkahvb0m4h1h5q82bz9ycsjy"))
        (patches '("patches/ant-contrib.patch"))
        (modules '((guix build utils)))
        (snippet `(for-each delete-file
            (find-files "." ".*\\.jar$|Ivy14Adapter\\.java$")))))
    (native-inputs
      (list java-apache-ivy java-commons-bcel java-commons-httpclient java-xerces unzip))
    (build-system ant-build-system)
    (arguments
      `(#:make-flags
        ,#~(list (string-append "-Ddist.dir=" #$output "/share/java")
             "-Dno-ivy=true"
             (string-append "-Dproject.version=" #$version))
        #:tests? #f))
    (home-page "https://sourceforge.net/projects/ant-contrib/")
    (synopsis "Additional useful tasks and types for Ant")
    (description "Collection of user supplied task (like an <if> task) and a development playground for experimental tasks like a C/C++ compilation task for different compilers.")
    (license license:asl2.0)))

(define-public gant
  (package
    (name "gant")
    (version "1.9.8")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/Gant/Gant.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "1ilcpyqkrz55jzl4avsdy76vpay602hxcw48prr5rf14zssjfv9x"))
        (patches '("patches/gant.patch"))
        (modules '((guix build utils)))
        (snippet
          '(delete-file-recursively "examples"))))
    (native-inputs
     (list ant groovy))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "gant.jar"
        #:source-dir "src/main/groovy"
        #:tests? #f
        #:jdk ,icedtea-8
        #:phases
        (modify-phases %standard-phases
          (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (setenv "CLASSPATH"
                     (string-join
                       (apply append (map (lambda (input)
                                            (find-files (assoc-ref inputs input)
                                                        ".*.jar"))
                                          '("ant" "groovy")))
                       ":"))
             (apply invoke "groovyc" "-d" "build/classes" "-j"
                    (find-files "src/main/" ".*\\.(groovy|java)$"))
             (invoke "ant" "jar")
             #t)))))
    (home-page "https://github.com/Gant/Gant")
    (synopsis "Gant -- Groovy scripting of Ant tasks")
    (description "Gant is a tool for scripting Ant tasks using Groovy instead of XML to specify
the logic. A Gant specification is just a Groovy script and so can bring all
the power of Groovy to bear directly, something not possible with Ant scripts.
Whilst it might be seen as a competitor to Ant, Gant relies on all the Ant
tasks for the complex actions, so it is really an alternative way of doing
builds using Ant, but using a programming language rather than XML to specify
the rules.")
    (license license:asl2.0)))

(define-public java-protobuf-api-2
  (package
    (name "java-protobuf-api")
    (version "2.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/protocolbuffers/protobuf.git")
              (commit (string-append "v" version))))
        (sha256 (base32 "03df8zvx2sry3jz2x4pi3l32qyfqa7w8kj8jdbz30nzy0h7aa070"))))
    (native-inputs
     (list protobuf-2))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "protobuf.jar"
        #:source-dir "java/src/main"
        #:tests? #f
        #:phases
        ,#~(modify-phases %standard-phases
          (add-before 'build 'generate-sources
            (lambda _
              (invoke (string-append #$protobuf-2 "/bin/protoc")
                       "--java_out=java/src/main/java"
                       "--proto_path=src"
                       "src/google/protobuf/descriptor.proto")
         )))))
    (home-page "http://protobuf.dev/")
    (synopsis "Protocol Buffers are language-neutral, platform-neutral extensible mechanisms for serializing structured data. This package contains Java API.")
    (description "Protocol buffers are Google’s language-neutral, platform-neutral, extensible mechanism for serializing structured data – think XML, but smaller, faster, and simpler. You define how you want your data to be structured once, then you can use special generated source code to easily write and read your structured data to and from a variety of data streams and using a variety of languages. This package contains Java API.")
    (license license:bsd-3)))

(define-public intellij-util-rt-133
  (package
    (name "intellij-util-rt")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (arguments
      `(#:jar-name "util-rt.jar"
        #:source-dir "platform/util-rt/src"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util-rt")
    (description "IntelliJ Platform, util-rt submodule")
    (license license:asl2.0)))

(define-public intellij-asm4-133
  (package
    (name "intellij-asm4")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./lib/src/asm4-src.zip"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))
              (substitute* (find-files "unzipped" "\\.java$")
                (("org\\.jetbrains\\.asm\\.") "org.jetbrains.asm4."))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list unzip))
    (arguments
      `(#:jar-name "asm4.jar"
        #:source-dir "unzipped"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version of ASM 4 used in IntelliJ Platform")
    (description "Vendored version of ASM 4 used in IntelliJ Platform")
    (license license:bsd-3)))

(define-public intellij-jsr166e-seqlock-133
  (package
    (name "intellij-jsr166e-seqlock")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./lib/src/jsr166e_src.jar"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))

              (mkdir "src")
              (copy-file "unzipped/jsr166e/extra/SequenceLock.java" "src/SequenceLock.java")
              (delete-file-recursively "unzipped")
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list unzip))
    (arguments
      `(#:jar-name "jsr166e-seqlock.jar"
        #:source-dir "src"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "SequenceLock from the vendored version of jsr166e used in IntelliJ Platform")
    (description "SequenceLock from the vendored version of jsr166e used in IntelliJ Platform")
    (license license:cc0)))

;; (define-public intellij-jzlib-133
;;   (package
;;     (name "intellij-jzlib")
;;     (version "133")
;;     (source (origin
;;         (method git-fetch)
;;         (uri (git-reference
;;               (url "https://github.com/JetBrains/intellij-community.git")
;;               (commit version)))
;;         (file-name (git-file-name name version))
;;         (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
;;         (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
;;         (snippet
;;           #~(begin
;;               (invoke (string-append #$unzip "/bin/unzip")
;;                        "./lib/src/jzlib-1.1.1.zip"
;;                        "-d"
;;                        "unzipped")
;;               ;; Keep only the unzipped source (and ignore current/parent directory links)
;;               (for-each (lambda (f)
;;                           (delete-file-recursively f))
;;                         (filter
;;                           (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
;;                           (scandir ".")))
;;               #t))))
;;     (build-system ant-build-system)
;;     (native-inputs
;;       (list java-protobuf-api-2 unzip))
;;     (arguments
;;       `(#:jar-name "jzlib.jar"
;;         #:source-dir "unzipped/jzlib-1.1.1/src/main/java"
;;         #:tests? #f))
;;     (home-page "https://www.jetbrains.com/opensource/idea/")
;;     (synopsis "Vendored version of JZlib used in IntelliJ Platform")
;;     (description "Vendored version of JZlib used in IntelliJ Platform")
;;     (license license:bsd-3)))

(define-public intellij-netty-133
  (package
    (name "intellij-netty")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./lib/src/netty-all-sources.jar"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))

              ;; Delete channel and codec implementations not used by JPS
              (delete-file-recursively "unzipped/io/netty/channel/rxtx")
              (delete-file-recursively "unzipped/io/netty/channel/udt")
              (delete-file-recursively "unzipped/io/netty/handler/codec/compression")
              (delete-file-recursively "unzipped/io/netty/handler/codec/http")
              (delete-file-recursively "unzipped/io/netty/handler/codec/marshalling")
              (delete-file-recursively "unzipped/io/netty/handler/codec/rtsp")
              (delete-file-recursively "unzipped/io/netty/handler/codec/spdy")

              (delete-file-recursively "unzipped/io/netty/example")
              (delete-file-recursively "unzipped/io/netty/util/internal/chmv8")
              (delete-file "unzipped/io/netty/util/internal/JavassistTypeParameterMatcherGenerator.java")
              (substitute*
                "unzipped/io/netty/util/internal/PlatformDependent.java"
                (("import io\\.netty\\.util\\.internal\\.chmv8\\.ConcurrentHashMapV8;") "")
                (("ConcurrentHashMapV8") "ConcurrentHashMap")
                (("JavassistTypeParameterMatcherGenerator\\.generate\\(Object\\.class, PlatformDependent\\.class\\.getClassLoader\\(\\)\\)") ""))
              (substitute*
                "unzipped/io/netty/util/internal/TypeParameterMatcher.java"
                (("PlatformDependent\\.hasJavassist\\(\\)") "false")
                (("JavassistTypeParameterMatcherGenerator\\.generate\\(parameterType\\)") "null"))
              #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-commons-logging-minimal java-log4j-1.2-api java-protobuf-api-2 java-slf4j-api unzip))
    (arguments
      `(#:jar-name "netty-all.jar"
        #:source-dir "unzipped"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version of Netty used in IntelliJ Platform")
    (description "Vendored version of Netty used in IntelliJ Platform")
    (license license:asl2.0)))

(define-public intellij-picocontainer-133
  (package
    (name "intellij-picocontainer")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./lib/src/picocontainer-src.zip"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))
              (copy-recursively "unzipped/picocontainer-1_2" ".")
              (delete-file-recursively "unzipped")

              (for-each delete-file
                  (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list unzip))
    (arguments
      `(#:jar-name "picocontainer.jar"
        #:source-dir "container/src/java"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version of picocontainer used in IntelliJ Platform")
    (description "Vendored version of picocontainer used in IntelliJ Platform")
    (license license:bsd-3)))

(define-public intellij-trove4j-133
  (package
    (name "intellij-trove4j")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils) (ice-9 ftw) (ice-9 regex)))
        (snippet
          #~(begin
              (invoke (string-append #$unzip "/bin/unzip")
                       "./lib/src/trove4j_src.jar"
                       "-d"
                       "unzipped")
              ;; Keep only the unzipped source (and ignore current/parent directory links)
              (for-each (lambda (f)
                          (delete-file-recursively f))
                        (filter
                          (lambda (n) (not (regexp-match? (string-match "^\\.+$|^unzipped$" n))))
                          (scandir ".")))
              (for-each delete-file
                  (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (build-system ant-build-system)
    (native-inputs
     (list unzip))
    (arguments
      `(#:jar-name "trove4j.jar"
        #:source-dir "unzipped/core/src"
        #:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'build 'copy-generated-source
            (lambda _
              (copy-recursively "unzipped/generated/src"
                                "unzipped/core/src")))
;;           (add-before 'check 'fix-test-path
;;             (lambda _
;;               (substitute* "build.xml" (("\\$\\{test\\.home\\}/java") ""))
;;             #t))
         )))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "Vendored version of trove4j used in IntelliJ Platform")
    (description "Vendored version of trove4j used in IntelliJ Platform")
    (license license:asl2.0)))

(define-public intellij-compiler-javac2-133
  (package
    (name "intellij-compiler-javac2")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (native-inputs
     (list ant intellij-asm4-133 intellij-compiler-instrumentation-133))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "javac2.jar"
        #:source-dir "java/compiler/javac2/src"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: compiler javac2 module.")
    (description "IntelliJ Platform: compiler javac2 module.")
    (license license:asl2.0)))

(define-public intellij-compiler-instrumentation-133
  (package
    (name "intellij-compiler-instrumentation")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (native-inputs
     (list intellij-asm4-133))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "instrumentation-util.jar"
        #:source-dir "java/compiler/instrumentation-util/src"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: compiler instrumentation-util module.")
    (description "IntelliJ Platform: compiler instrumentation-util module.")
    (license license:asl2.0)))

(define-public intellij-util-133
  (package
    (name "intellij-util")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (patches '("patches/sdk-133.patch"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))

            ;; Delete Mac-only UI classes which are not needed for JPS
            (delete-file "platform/util/src/com/intellij/util/AppleHiDPIScaledImage.java")
            (delete-file "platform/util/src/com/intellij/util/ui/MacUIUtil.java")
            (delete-file-recursively "platform/util/src/com/intellij/ui/mac")
            #t))))
    (build-system ant-build-system)
    (native-inputs
      (list java-jetbrains-annotations))
    (inputs
     (list java-asm-3 java-cglib java-jakarta-oro java-jdom java-log4j-1.2-api java-native-access java-native-access-platform intellij-jsr166e-seqlock-133 intellij-picocontainer-133 intellij-util-rt-133 intellij-trove4j-133))
    (arguments
      `(#:jar-name "util.jar"
        #:source-dir "platform/util/src"
        #:jdk ,icedtea-8
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util")
    (description "IntelliJ Platform, util submodule")
    (license license:asl2.0)))

(define-public intellij-jps-model-api-133
  (package
    (name "intellij-jps-model-api")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (native-inputs
     (list java-jetbrains-annotations intellij-util-rt-133))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "jps-model-api.jar"
        #:source-dir "jps/model-api/src"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model API")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains 'model-api' submodule.")
    (license license:asl2.0)))

(define-public intellij-jps-builders-133
  (package
    (name "intellij-jps-builders")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (native-inputs
     (list ant gant java-jetbrains-annotations java-log4j-1.2-api intellij-jps-model-api-133 intellij-util-rt-133 intellij-util-133 intellij-asm4-133 intellij-jsr166e-seqlock-133 intellij-netty-133 intellij-trove4j-133 java-protobuf-api-2 ))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "jps-builders.jar"
        #:source-dir "jps/jps-builders/src"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Builders")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains 'builders' submodule only.")
    (license license:asl2.0)))


(define-public intellij-jps-standalone-builder-133
  (package
    (name "intellij-jps-standalone-builder")
    (version "133")
    (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/JetBrains/intellij-community.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
        (modules '((guix build utils)))
        (snippet
          '(begin
            (delete-file-recursively "bin")
            (delete-file-recursively "lib")
            (delete-file-recursively "plugins")
            (delete-file-recursively "python")
            (for-each delete-file
                (find-files "." ".*\\.(jar|so)$"))
            #t))))
    (native-inputs
     (list ant gant))
    (build-system ant-build-system)
    (arguments
      `(#:jar-name "jps-standalone-builder.jar"
        #:source-dir "jps/standalone-builder"
        #:tests? #f))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System, Standalone builder.")
    (description "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build. This package contains standalone builder only.")
    (license license:asl2.0)))

;; JPS is probably enough for compiling Kotlin, no need to build the sdk platform
;; (define-public intellij-sdk-133
;;   (package
;;     (name "intellij-sdk")
;;     (version "133")
;;     (source (origin
;;         (method git-fetch)
;;         (uri (git-reference
;;               (url "https://github.com/JetBrains/intellij-community.git")
;;               (commit version)))
;;         (file-name (git-file-name name version))
;;         (sha256 (base32 "0k4b1y8dpy3qza7hw5rms4afhjsgr5i8y7qx32fhyf3yybyg8npm"))
;;         (patches '("patches/sdk-133.patch"))
;;         (modules '((guix build utils)))
;;         (snippet
;;           '(begin
;;             (delete-file-recursively "bin")
;;             (delete-file-recursively "lib")
;;             (delete-file-recursively "plugins")
;;             (delete-file-recursively "python")
;;             (for-each delete-file
;;                 (find-files "." ".*\\.(jar|so)$"))
;;             (mkdir-p "lib")
;;             #t))))
;;     (native-inputs
;;      (list ant gant))
;;     (build-system ant-build-system)
;;     (arguments
;;       `(#:build-target "build"
;;         #:tests? #f
;;         #:make-flags
;;         ,#~(list
;;               (string-append "-Ddist.dir=" #$output "/share/java")
;;               (string-append "-Dant-launcher.jar=" #$(this-package-native-input "ant") "/lib/ant-launcher.jar"))))
;;     (home-page "https://www.jetbrains.com/opensource/idea/")
;;     (synopsis "IntelliJ Platform")
;;     (description "IntelliJ Platform")
;;     (license license:asl2.0)))

(define-public kotlin-0-6-786
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
     (list java-jline-2 ant ant-contrib intellij-asm4-133 intellij-compiler-javac2-133 intellij-compiler-instrumentation-133))
    (build-system ant-build-system)
    (arguments
      `(#:build-target "dist"
        #:make-flags
        ,#~(list (string-append "-Doutput=" #$output)
             "-Dgenerate.javadoc=false"
             "-Dshrink=false"
             "-Didea.sdk=ideasdk"
             (string-append "-Dbuild.number=" #$version))
        #:tests? #f
        #:phases
         (modify-phases %standard-phases
           (add-before 'build 'prepare-sdk-dir
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p "ideasdk/lib")
               (symlink
                 (string-append
                   (assoc-ref inputs "intellij-compiler-javac2")
                   "/share/java/javac2.jar")
                 "ideasdk/lib/javac2.jar")
               (symlink
                 (string-append
                   (assoc-ref inputs "intellij-asm4")
                   "/share/java/asm4.jar")
                 "ideasdk/lib/jetbrains-asm-debug-all-4.0.jar")
               #t))
           (delete 'install))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

;; This allows you to run guix shell -f guix-packager.scm.
;; Remove this line if you just want to define a package.
kotlin-0-6-786
