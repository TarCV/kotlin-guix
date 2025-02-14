;;;    Copyright 2024 TarCV
;;;    Copyright 2023 Emmanuel Bourg, Julien Lepiller
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.

(define-module (kotlin)
  #:use-module (guix)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system ant)
  #:use-module (guix git-download)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages java)
  #:use-module (gnu packages protobuf))

;; TODO: should intellij packages be merged, or their patches be splitted?
;; TODO: ensure (find-files "*.jar") finds only a single file
;; TODO: fix manifest configuration https://ant.apache.org/manual/Tasks/manifest.html
;; TODO: improve jar/zip cleanup
;; TODO: update ASM 3.1 to latest minor of release 3
;; TODO: try using latest ASM, IJ, JDK, etc for non-public kotlin packages
;; TODO: add verification that symlink target exists
;; TODO: recheck all hashes with guix download
;; TODO: some intellij versions are not exact, fix them?
;; TODO: delete unused sources before building
;; TODO: try using just the latest intellij, asm, annotations versions
;; TODO: avoid using Java 7+ features in public kotlin packages
;; TODO: change unpack to avoid extracting everything (but how to keep patching?)
;; TODO: how to inherit source too?
;; TODO: remove unused packages
;; TODO: refactor bootstrap chains using folding

(define (link-input-jars target-dir package-names)
  `(lambda* (#:key inputs #:allow-other-keys)
     (mkdir-p ,target-dir)
     (for-each (lambda (p)
                 (let* ((allJars (find-files (assoc-ref inputs p)
                                  ;; Exclude javadoc and other variants
                                  "(^[^[:digit:]]+|[[:digit:]]|4j|api|jsr166e.+)\\.jar$"))
                        (mainJar (if (= 1
                                        (length allJars))
                                     (car allJars)
                                     (throw 'no-or-multiple-jars-found p))))
                   
                   (symlink mainJar
                            (string-append ,target-dir "/"
                                           (basename mainJar)))))
               ,package-names)))

(define (kotlin-make-flags version bootstrap-package)
  #~(list (string-append "-Dkotlin-home="
                         #$output)
          "-Dbootstrap.build.no.tests=true"
          "-Dgenerate.assertions=false"
          "-Dgenerate.javadoc=false"
          "-Dshrink=false"
          (string-append "-Dbuild.number="
                         #$version)
          (string-append "-Dbootstrap.compiler.home="
                         #$bootstrap-package)))

(define (intellij-module-by-branch module-dirs version sha256sum
                                   additional-patches)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
          version ".tar.gz"))
    (file-name (string-append "intellij-community-" version ".tar.gz"))
    (sha256 (base32 sha256sum))
    (patches `(,(string-append "patches/sdk-" version ".patch") ,@additional-patches))
    (modules '((guix build utils)))
    (snippet `(begin
                (delete-file-recursively "build")
                (for-each (lambda (d)
                            (copy-recursively d "module"))
                          (list ,@module-dirs))
                ;; Keep only the combined source (and ignore current/parent directory links)
                (use-modules (ice-9 ftw)
                             (ice-9 regex))
                (for-each (lambda (f)
                            (delete-file-recursively f))
                          (filter (lambda (n)
                                    (not (regexp-match? (string-match
                                                         "^(\\.+|module)$" n))))
                                  (scandir ".")))

                (for-each delete-file
                          (find-files "module"
                                      ".*\\.(a|class|exe|jar|so|zip)$"))))))

(define (kotlin-source-by-tag version sha256sum additional-patches)
  (origin
    (method url-fetch) ;url-fetch instead of git-fetch to avoid spending too much inodes on all kotlin versions
    (uri (string-append
          "https://github.com/JetBrains/kotlin/archive/refs/tags/build-"
          version ".tar.gz"))
    (file-name (string-append "kotlin-" version ".tar.gz"))
    (sha256 (base32 sha256sum))
    (patches `(,(string-append "patches/kotlin-" version ".patch") ,@additional-patches))
    (modules '((guix build utils)))
    (snippet `(for-each delete-file
                        (find-files "." ".*\\.(a|class|exe|jar|so|zip)$")))))

(define (kotlin-source-by-release version sha256sum additional-patches)
  (origin
    (method url-fetch) ;url-fetch instead of git-fetch to avoid spending too much inodes on all kotlin versions
    (uri (string-append
           "https://github.com/JetBrains/kotlin/archive/refs/tags/v"
           version ".tar.gz"))
    (file-name (string-append "kotlin-" version ".tar.gz"))
    (sha256 (base32 sha256sum))
    (patches `(,(string-append "patches/kotlin-" version ".patch") ,@additional-patches))
    (modules '((guix build utils)))
    (snippet `(for-each delete-file
                        (find-files "." ".*\\.(a|class|exe|jar|so|zip)$")))))

(define* (package-by-inheriting-kotlin-package inherited-package
                                               version
                                               sha256sum
                                               additional-patches
                                               #:key (set-native-inputs (lambda 
                                                                                (v)
                                                                          v))
                                               (set-jdk (lambda (v)
                                                          v))
                                               (set-phases (lambda (v)
                                                             v)))
  (package
    (inherit inherited-package)
    (version version)
    (source
     (kotlin-source-by-tag version sha256sum additional-patches))
    (native-inputs `(,@(set-native-inputs (package-native-inputs
                                           inherited-package))))
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments inherited-package)
           ((#:jdk jdk)
            (set-jdk jdk))
           ((#:make-flags make-flags)
            (kotlin-make-flags version inherited-package))
           ((#:phases phases)
            (set-phases phases)))))))

(define* (package-by-inheriting-kotlin-release-package inherited-package
                                                       version
                                                       sha256sum
                                                       additional-patches
                                                       #:key (set-native-inputs (lambda 
                                                                                        (v)
                                                                                  v))
                                                       (set-jdk (lambda (v)
                                                                  v))
                                                       (set-phases (lambda (v)
                                                                     v)))
  (package
    (inherit inherited-package)
    (version version)
    (source
     (kotlin-source-by-release version sha256sum additional-patches))
    (native-inputs `(,@(set-native-inputs (package-native-inputs
                                           inherited-package))))
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments inherited-package)
           ((#:jdk jdk)
            (set-jdk jdk))
           ((#:make-flags make-flags)
            (kotlin-make-flags version inherited-package))
           ((#:phases phases)
            (set-phases phases)))))))

(define ant-contrib
  (package
    (name "ant-contrib")
    (version "1.0b3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.sourceforge.net/ant-contrib/ant-contrib/"
             version "/ant-contrib-" version "-src.tar.gz"))
       (sha256
        (base32 "1mxmhkqc8k7160696alsyh9gq1j9ijsi0m6kw6dzbbl4kkxklfvg"))
       (patches '("patches/ant-contrib.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "lib")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list java-commons-bcel java-commons-httpclient java-junit
                         java-xerces))
    (build-system ant-build-system)
    (arguments
     `(#:make-flags ,#~(list (string-append "-Dproject.version="
                                            #$version))
       #:build-target "dist-stage"
       #:tests? #f ;Has some broken tests
       ;; #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-ivy-dependent-task
                    ;; This task implementation depends on old Ivy
                    (lambda _
                      (delete-file
                       "src/java/net/sf/antcontrib/net/URLImportTask.java")))
                  (replace 'install
                    (install-jars "target/stage"))
                  (add-after 'install 'install-doc
                    (install-javadoc "target/stage/docs")))))
    (home-page "https://sourceforge.net/projects/ant-contrib/")
    (synopsis "Additional useful tasks and types for Ant")
    (description
     "Collection of user supplied task (like an <if> task) and a development playground for experimental tasks like a C/C++ compilation task for different compilers.")
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
       (sha256
        (base32 "0a2xndhhb6al26kn77q1i2g9a81pzcybzdckz4818wb3s46p8ayv"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "all"
       #:tests? #f ;This version doesn't have any tests
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'fix-javadoc
                    (lambda _
                      (substitute* "build.xml"
                        (("additionalparam=\"" all)
                         (string-append all "-notimestamp ")))))
                  (replace 'install
                    (install-jars "dist"))
                  (add-after 'install 'install-doc
                    (install-javadoc "doc")))))
    (home-page "https://www.brics.dk/automaton/")
    (synopsis "Finite-state automata and regular expressions for Java")
    (description
     "This Java package contains a DFA/NFA (finite-state automata) implementation with Unicode alphabet (UTF16) and support for the standard regular expression operations (concatenation, union, Kleene star) and a number of non-standard ones (intersection, complement, etc.).  In contrast to many other automaton/regexp packages, this package is fast, compact, and implements real, unrestricted regular operations.  It uses a symbolic representation based on intervals of Unicode characters.")
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
       (sha256
        (base32 "1f29v4jcnp5nfhhj3kzlryyp0yf97iizbfk1fi8jbhvcpxdajg1w"))))
    (native-inputs (list java-junit))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "cli-parser.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:make-flags (list "-Dant.build.javac.target=1.7")))
    (home-page "https://github.com/spullara/cli-parser")
    (synopsis
     "Tiny (10k jar) library for parsing command line arguments or property lists")
    (description
     "CLI Parser is a tiny (10k jar), super easy to use library for parsing
various kinds of command line arguments or property lists.  Using annotations
on your fields or JavaBean properties you can specify what configuration is
available.")
    (license license:asl2.0)))

;; Kotlinc doesn't require per platform classes/libraries on Linux, so this package doesn't build them
(define java-native-platform
  (package
    (name "java-native-platform")
    (version "0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gradle/native-platform.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r4iism4mwx2y95zawx8jmminlcp1mswn71sqwz73cpsfirq4xha"))))
    (build-system ant-build-system)
    (arguments
     '(#:jar-name "native-platform.jar"
       #:source-dir "src/main/java"
       ;; Tests depend on Groovy Spock
       #:tests? #f))
    (home-page "https://github.com/gradle/native-platform")
    (synopsis "Java bindings for various native APIs (Shared classes only)")
    (description
     "A collection of cross-platform Java APIs for various native APIs.  Currently supports OS X, Linux, Windows and FreeBSD on Intel architectures.")
    (license license:asl2.0)))

;(define-public java-proguard-4.9
;  (package
;    (name "java-proguard")
;    (version "4.9")
;    (source
;      (origin
;        (method git-fetch)
;        (uri (git-reference
;               (url "https://github.com/Guardsquare/proguard.git")
;               (commit (string-append "proguard" version))))
;        (file-name (git-file-name name version))
;        (sha256 (base32 "19wzqm17s6v4hwjam2q0ax16kf0s5ppy5v8z0fyr8imc6sga6zwf"))
;        (patches '("patches/proguard-4.9.patch"))
;        (modules '((guix build utils)))
;        (snippet
;          '(delete-file-recursively "examples"))))
;    (build-system ant-build-system)
;    (arguments
;      `(#:build-target "anttask"
;        #:jdk ,icedtea-7
;        #:make-flags (list "-buildfile" "build")
;        #:tests? #f ;; No tests
;         #:phases
;         (modify-phases %standard-phases
;           (replace 'install (install-jars "lib")))))
;    (home-page "https://www.guardsquare.com/proguard")
;    (synopsis "Java optimizer and obfuscator")
;    (description "ProGuard is a free shrinker, optimizer, obfuscator, and preverifier for Java bytecode:
;     It detects and removes unused classes, fields, methods, and attributes.
;     It optimizes bytecode and removes unused instructions.
;     It renames the remaining classes, fields, and methods using short meaningless names.
;     The resulting applications and libraries are smaller and faster.")
;    (license license:gpl2+)))

(define-public protobuf-2.5
  (package
    (inherit protobuf-2)
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/protocolbuffers/protobuf/releases/"
                           "download/v"
                           version
                           "/protobuf-"
                           version
                           ".tar.bz2"))
       (sha256
        (base32 "0xxn9gxhvsgzz2sgmihzf6pf75clr05mqj6218camwrwajpcbgqk"))))))

;; This is the latest version not requiring clients to know about StringLists
(define-public java-protobuf-api-2.5
  (package
    (name "java-protobuf-api")
    (version "2.5.0")
    (properties '((upstream-name . "protobuf")))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/protocolbuffers/protobuf/releases/"
                           "download/v"
                           version
                           "/protobuf-"
                           version
                           ".tar.bz2"))
       (sha256
        (base32 "0xxn9gxhvsgzz2sgmihzf6pf75clr05mqj6218camwrwajpcbgqk"))))
    (native-inputs (list java-cglib
                         java-easymock-3.2
                         java-easymock-class-extension
                         java-junit
                         java-objenesis
                         protobuf-2.5))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "protobuf.jar"
       #:source-dir "java/src/main"
       #:test-dir "java/src/test"
       #:make-flags (list "-Dant.build.javac.target=1.7")
       #:phases ,#~(modify-phases %standard-phases
                     (add-before 'build 'generate-sources
                       (lambda* (#:key inputs #:allow-other-keys)
                         (invoke (string-append (assoc-ref inputs "protobuf")
                                                "/bin/protoc")
                                 "--java_out=java/src/main/java"
                                 "--proto_path=src"
                                 "src/google/protobuf/descriptor.proto")))
                     ;; must be before 'build, so that it is before 'rename-packages in the inheriting package
                     (add-before 'build 'generate-test-sources
                       (lambda* (#:key inputs #:allow-other-keys)
                         (invoke (string-append (assoc-ref inputs "protobuf")
                                                "/bin/protoc")
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
    (home-page "https://protobuf.dev/")
    (synopsis
     "Java API for Protocol buffers - data encoding for remote procedure calls (RPCs)")
    (description
     "Protocol Buffers are a way of encoding structured data in an efficient
      yet extensible format.  Google uses Protocol Buffers for almost all of its
      internal RPC protocols and file formats.  This package contains Java API.")
    (license license:bsd-3)))

(define java-protobuf-api-2-kotlin
  (package
    (inherit java-protobuf-api-2.5)
    (version "2.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/protocolbuffers/protobuf/releases/"
               "download/v"
               version
               "/protobuf-"
               version
               ".tar.bz2"))
        (sha256
          (base32 "040rcs9fpv4bslhiy43v7dcrzakz4vwwpyqg4jp8bn24sl95ci7f"))))
    (native-inputs (modify-inputs (package-native-inputs java-protobuf-api-2.5)
      (replace "protobuf" protobuf-2)))
    (arguments
      (substitute-keyword-arguments (package-arguments java-protobuf-api-2.5)
        ((#:phases phases)
          `(modify-phases ,phases
             (replace 'generate-test-sources
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; TODO: generate this from java/pom.xml
                 (invoke (string-append (assoc-ref inputs "protobuf")
                           "/bin/protoc")
                   "--java_out=java/src/test/java"
                   "--proto_path=src"
                   "--proto_path=java/src/test/java"
                   "src/google/protobuf/unittest.proto"
                   "src/google/protobuf/unittest_import.proto"
                   "src/google/protobuf/unittest_import_public.proto"
                   "src/google/protobuf/unittest_mset.proto"
                   "java/src/test/java/com/google/protobuf/lazy_fields_lite.proto"
                   "java/src/test/java/com/google/protobuf/lite_equals_and_hash.proto"
                   "java/src/test/java/com/google/protobuf/multiple_files_test.proto"
                   "java/src/test/java/com/google/protobuf/nested_builders_test.proto"
                   "java/src/test/java/com/google/protobuf/nested_extension.proto"
                   "java/src/test/java/com/google/protobuf/nested_extension_lite.proto"
                   "java/src/test/java/com/google/protobuf/non_nested_extension.proto"
                   "java/src/test/java/com/google/protobuf/non_nested_extension_lite.proto"
                   "java/src/test/java/com/google/protobuf/outer_class_name_test.proto"
                   "java/src/test/java/com/google/protobuf/outer_class_name_test2.proto"
                   "java/src/test/java/com/google/protobuf/outer_class_name_test3.proto"
                   "java/src/test/java/com/google/protobuf/test_bad_identifiers.proto"
                   "java/src/test/java/com/google/protobuf/test_check_utf8.proto"
                   "java/src/test/java/com/google/protobuf/test_check_utf8_size.proto"
                   "java/src/test/java/com/google/protobuf/test_custom_options.proto"
                   "src/google/protobuf/unittest_optimize_for.proto"
                   "src/google/protobuf/unittest_custom_options.proto"
                   "src/google/protobuf/unittest_lite.proto"
                   "src/google/protobuf/unittest_import_lite.proto"
                   "src/google/protobuf/unittest_import_public_lite.proto"
                   "src/google/protobuf/unittest_lite_imports_nonlite.proto"
                   "src/google/protobuf/unittest_enormous_descriptor.proto"
                   "src/google/protobuf/unittest_no_generic_services.proto")))
             (add-before 'build 'rename-packages
               (lambda _
                 (substitute* (find-files "java/src" "\\.java$")
                   (("([^[:alnum:]])com\\.google\\.protobuf([^[:alnum:]])" all
                      before after)
                     (string-append before "org.jetbrains.kotlin.protobuf" after)))

                 (mkdir-p "java/src/test/java/org/jetbrains/kotlin")
                 (rename-file "java/src/test/java/com/google/protobuf" "java/src/test/java/org/jetbrains/kotlin/protobuf")))))))))

(define java-jetbrains-asm-4
  (package
    (inherit java-asm)
    (name "java-jetbrains-asm-4")
    (version "4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.ow2.org/asm/asm.git")
             (commit (string-append "ASM_"
                                    (string-map (lambda (c)
                                                  (if (char=? c #\.) #\_ c))
                                                version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hwapag4vxk1vrw30ck82pdldprrfa29vqcp2g2xsdmfpbh6j2qa"))))
    (synopsis "ASM library with patched Java package name")
    (propagated-inputs '())
    ;; Disable tests because base package disables them
    (arguments
     `(#:make-flags (list "-Dant.build.javac.target=1.6")
       ,@(substitute-keyword-arguments (package-arguments java-asm)
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'remove-bnd-dependency)
               (add-before 'build 'rename-packages
                 (lambda _
                   (substitute* (find-files "src" "\\.java$")
                     (("([^[:alnum:]])org\\.objectweb\\.asm([^[:alnum:]])" all
                       before after)
                      (string-append before "org.jetbrains.asm4" after)))))
               (add-after 'build-jars 'fix-jar-name
                 (lambda _
                   (rename-file (string-append "dist/asm-"
                                               ,(package-version java-asm)
                                               ".jar")
                                (string-append "dist/asm-"
                                               ,version ".jar"))))
               (replace 'fix-pom
                 (lambda _
                   (substitute* (find-files "archive" "\\.pom$")
                     (("@product.artifact@")
                      ,version)) #t)))))))))

(define java-jetbrains-asm-5
  (package
    (inherit java-asm)
    (name "java-jetbrains-asm-5")
    (version "5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.ow2.org/asm/asm.git")
             (commit (string-append "ASM_"
                                    (string-map (lambda (c)
                                                  (if (char=? c #\.) #\_ c))
                                                version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155xckz8pxdlwf5rn9zhqklxqs2czgfrw6gddsjn70c5lfdmmjxj"))))
    (synopsis "ASM library with patched Java package name")
    (propagated-inputs '())
    ;; Disable tests because base package disables them
    (arguments
     `(#:make-flags (list "-Dant.build.javac.target=1.6")
       ,@(substitute-keyword-arguments (package-arguments java-asm)
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'remove-bnd-dependency)
               (add-before 'build 'rename-packages
                 (lambda _
                   (substitute* (find-files "src" "\\.java$")
                     (("([^[:alnum:]])org\\.objectweb\\.asm([^[:alnum:]])" all
                       before after)
                      (string-append before "org.jetbrains.org.objectweb.asm"
                                     after)))))
               (add-after 'build-jars 'fix-jar-name
                 (lambda _
                   (rename-file (string-append "dist/asm-"
                                               ,(package-version java-asm)
                                               ".jar")
                                (string-append "dist/asm-"
                                               ,version ".jar"))))
               (replace 'fix-pom
                 (lambda _
                   (substitute* (find-files "archive" "\\.pom$")
                     (("@product.artifact@")
                      ,version)) #t)))))))))

;; Latest version not depending on Java 8 Predicate
(define java-guava-patched-20
  (package
    (inherit java-guava)
    (version "20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/guava/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "java-guava" version))
       (sha256
        (base32 "00h5cawdjic1vind3yivzh1f58flvm1yfmhsyqwyvmbvj1vakysp"))
       (patches '("patches/guava-20-linkedhashset.patch"))))
    ;; Guava tests depend on Truth which has cyclic dependency back on Guava, so tests are disabled for now
    (arguments
     `(#:make-flags (list "-Dant.build.javac.target=1.7")
       ,@(substitute-keyword-arguments (package-arguments java-guava)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-before 'build 'remove-ijrer-imports
                 (lambda _
                   (substitute* (find-files "." "\\.java$")
                     (("import org.codehaus.mojo.animal_sniffer.IgnoreJRERequirement;")
                      ""))))
               (add-before 'check 'fix-test-target
                 (lambda _
                   (substitute* "build.xml"
                     (("\\$\\{test\\.home\\}/java")
                      "${test.home}"))))
               (delete 'install-listenablefuture-stub))))))))

(define java-javax-inject-java6
  (package
    (inherit java-javax-inject)
    (arguments
     `(#:jdk ,icedtea-7
       #:make-flags (list "-Dant.build.javac.target=1.6")
       ,@(package-arguments java-javax-inject)))))

(define intellij-annotations-141
  (package
    (name "intellij-annotations")
    (version "141")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "1s2ys8k7jnr9v05765ql5rk94wnfy18grdiyvynb7pskn96x5jj4"))
       (patches '("patches/sdk-141.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-annotations.jar"
       #:source-dir "platform/annotations/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.5")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Annotations")
    (description "IntelliJ Platform, annotations submodule")
    (license license:asl2.0)))

(define intellij-annotations-143
  (package
    (name "intellij-annotations")
    (version "143")
    (source
     (intellij-module-by-branch (list "platform/annotations") version
      "0n52rkag7iav3lxzmvqbfg3ngw7rymfzhaxj6jshfdw9yw2x45pj"
      '()))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-annotations.jar"
       #:source-dir "module/java5/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.5")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'copy-module-sources
                    (lambda _
                      (copy-recursively "module/common/src" "module/java5/src"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Annotations")
    (description "IntelliJ Platform, annotations submodule")
    (license license:asl2.0)))

(define java-jsr166e-seqlock
  (let* ((name "java-jsr166e-seqlock")
         (version "1.7")
         (filename (string-append name "-" version ".java")))
    (package
      (name name)
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://gee.cs.oswego.edu/cgi-bin/viewcvs.cgi/jsr166/jsr166/src/jsr166e/extra/SequenceLock.java?revision="
               version "&view=co"))
         (file-name filename)
         (sha256
          (base32 "1pv9lnj0mb7m50r0q9790jmdrpgnlwg8803ial4z5ip9n3zhnfzh"))))
      (build-system ant-build-system)
      (arguments
       `(#:jar-name "jsr166e-seqlock.jar"
         #:source-dir "."
         #:tests? #f ;This class doesn't have tests
         #:phases ,#~(modify-phases %standard-phases
                       (add-after 'unpack 'fix-file-name
                         (lambda _
                           (rename-file #$filename "SequenceLock.java"))))))
      (home-page
       "https://gee.cs.oswego.edu/dl/concurrency-interest/index.html")
      (synopsis
       "Java implementation of a reentrant mutual exclusion Lock where each lock acquisition/release advances a sequence number")
      (description
       "Java implementation of a reentrant mutual exclusion Lock in which each lock acquisition or release advances a sequence number.  While the implentation source requires only Java6 to compile and run by relying on other jsr166e code, this packages is compiled against JDK 8 having newer implementation of the same dependencies.")
      (license license:cc0))))

(define-public java-picocontainer
  (package
    (name "java-picocontainer")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/picocontainer/PicoContainer1.git")
              (commit (string-append "picocontainer-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "178sv2qrwr11505rvw9852642xpdcvvdzywc0wl1y8hzqicza6ja"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (ice-9 regex)))))
    (build-system ant-build-system)
    (native-inputs (list java-jmock-1 java-junit java-xstream))
    (arguments
     `(#:jar-name "picocontainer.jar"
       #:source-dir "container/src/java"
       #:test-dir "container/src/test"
       #:make-flags (list "-Dant.build.javac.target=1.7")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'fix-ant-xml
                    (lambda _
                      (substitute* "build.xml"
                        (("\\$\\{test\\.home\\}/java")
                         "${test.home}")
                        (("<javac " all)
                         (string-append all "encoding=\"iso-8859-1\" "))))))))
    (home-page "http://picocontainer.com/")
    (synopsis "General purpose DI / IOC container")
    (description "General purpose DI / IOC container")
    (license license:bsd-3)))

(define java-jetbrains-trove4j
  (let ((commit "29150c19710ef1581c790d0502cf299583db7322")
        (revision "1"))
    (package
      (name "java-jetbrains-trove4j")
      (version (git-version "2016.8.24" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JetBrains/intellij-deps-trove4j.git")
               (commit commit)))
         (sha256
          (base32 "1bd3bq6i18y0i3bqqkhizqi3cb0x0ja82zk902mqyz84hx8xljwd"))
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
         #:phases (modify-phases %standard-phases
                    (add-before 'build 'copy-generated-source
                      (lambda _
                        ;; TODO: Should the code be regenerated instead?
                        (copy-recursively "generated/src" "core/src")))
                    (add-before 'check 'patch-check-target
                      (lambda _
                        (substitute* "build.xml"
                          (("\\$\\{test\\.home\\}/java")
                           "")
                          (("<junit.+</junit>")
                           "<java classname=\"gnu.trove.MapTest\" failonerror=\"true\" fork=\"true\"><classpath><pathelement path=\"${env.CLASSPATH}\"/><pathelement path=\"${classes.dir}\"/><pathelement path=\"${test.classes.dir}\"/></classpath></java>")))))))
      (home-page "https://github.com/JetBrains/intellij-deps-trove4j")
      (synopsis
       "JetBrains fork of the Trove library. The Trove library provides high speed Object and primitive collections for Java")
      (description
       "The GNU Trove library has two objectives: 1. Provide \"free\" (as in \"free speech\" and \"free beer\"), fast, lightweight implementations of the java.util Collections API.  These implementations are designed to be pluggable replacements for their JDK equivalents.  2. Whenever possible, provide the same collections support for primitive types.  This gap in the JDK is often addressed by using the \"wrapper\" classes (java.lang.Integer, java.lang.Float, etc.) with Object-based collections.  For most applications, however, collections which store primitives directly will require less space and yield significant performance gains.")
      ;; Some classes are licensed under MIT variant
      (license license:lgpl2.1+))))

(define intellij-util-rt-139
  (package
    (name "intellij-util-rt")
    (version "139")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/26e72feacf91bfb222bec00b3139ed05aa3084b5.tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "04x2i75kqvxayykniwk2qgs6yh219hxyqjsakmj4brz0g7vjykgq"))
       (patches '("patches/sdk-139.patch"
                  "patches/sdk-util-rt-139-linkedhashmap.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (build-system ant-build-system)
    (native-inputs (list intellij-annotations-141))
    (arguments
     `(#:jar-name "intellij-util-rt.jar"
       #:source-dir "platform/util-rt/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'stub-phase
                    ;; Guix doesn't like if below without this stub
                    (lambda _
                      '())))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util-rt")
    (description "IntelliJ Platform, util-rt submodule")
    (license license:asl2.0)))

(define intellij-util-rt-141
  (package
    (name "intellij-util-rt")
    (version "141")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "1s2ys8k7jnr9v05765ql5rk94wnfy18grdiyvynb7pskn96x5jj4"))
       (patches '("patches/sdk-141.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (build-system ant-build-system)
    (native-inputs (list intellij-annotations-141))
    (arguments
     `(#:jar-name "intellij-util-rt.jar"
       #:source-dir "platform/util-rt/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'stub-phase
                    ;; Guix doesn't like if below without this stub
                    (lambda _
                      '())))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util-rt")
    (description "IntelliJ Platform, util-rt submodule")
    (license license:asl2.0)))

(define intellij-util-rt-143
  (package
    (name "intellij-util-rt")
    (version "143")
    (source
     (intellij-module-by-branch (list "platform/util-rt") version
      "0n52rkag7iav3lxzmvqbfg3ngw7rymfzhaxj6jshfdw9yw2x45pj"
      '()))
    (build-system ant-build-system)
    (native-inputs (list intellij-annotations-143))
    (arguments
     `(#:jar-name "intellij-util-rt.jar"
       #:source-dir "module/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'stub-phase
                    ;; Guix doesn't like if below without this stub
                    (lambda _
                      '())))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util-rt")
    (description "IntelliJ Platform, util-rt submodule")
    (license license:asl2.0)))

(define intellij-util-139
  (package
    (name "intellij-util")
    (version "139")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/26e72feacf91bfb222bec00b3139ed05aa3084b5.tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "04x2i75kqvxayykniwk2qgs6yh219hxyqjsakmj4brz0g7vjykgq"))
       (patches '("patches/sdk-139.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
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
                   (delete-file
                    "platform/util/src/com/intellij/util/AppleHiDPIScaledImage.java")
                   (delete-file
                    "platform/util/src/com/intellij/util/ui/MacUIUtil.java")
                   (delete-file-recursively
                    "platform/util/src/com/intellij/ui/mac")
                   #t))))
    (build-system ant-build-system)
    (native-inputs (list intellij-annotations-141 java-junit java-hamcrest-all))
    (propagated-inputs (list java-cglib
                             java-jakarta-oro
                             java-jdom
                             java-log4j-1.2-api
                             java-native-access
                             java-native-access-platform
                             java-jsr166e-seqlock
                             java-picocontainer
                             intellij-util-rt-139
                             java-jetbrains-trove4j))
    (arguments
     `(#:jar-name "intellij-util.jar"
       #:source-dir "platform/util/src"
       #:test-dir "platform/util/testSrc"
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-tests
                    (lambda _
                      (for-each delete-file
                                (list
                                 ;; Remove a Mac only test
                                 "platform/util/testSrc/com/intellij/util/FoundationTest.java"
                                 ;; Remove tests requiring resources from other modules
                                 "platform/util/testSrc/com/intellij/util/io/zip/ReorderJarsTest.java"))))
                  (add-before 'build 'copy-resources
                    (lambda _
                      (copy-recursively "platform/util/resources"
                                        "build/classes")))
                  (add-before 'build 'copy-resources-for-tests
                    (lambda _
                      (copy-recursively "platform/platform-resources/src"
                                        "build/test-classes")
                      (copy-recursively "platform/platform-resources-en/src"
                                        "build/test-classes")))
                  (add-before 'build 'fix-test-target
                    (lambda _
                      (substitute* "build.xml"
                        (("\\$\\{test\\.home\\}/java")
                         "${test.home}")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util")
    (description "IntelliJ Platform, util submodule")
    (license license:asl2.0)))

(define intellij-util-141
  (package
    (name "intellij-util")
    (version "141")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "1s2ys8k7jnr9v05765ql5rk94wnfy18grdiyvynb7pskn96x5jj4"))
       (patches '("patches/sdk-141.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
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
                   (delete-file
                    "platform/util/src/com/intellij/util/AppleHiDPIScaledImage.java")
                   (delete-file
                    "platform/util/src/com/intellij/util/ui/MacUIUtil.java")
                   (delete-file-recursively
                    "platform/util/src/com/intellij/ui/mac")
                   #t))))
    (build-system ant-build-system)
    (native-inputs (list intellij-annotations-141 java-junit java-hamcrest-all))
    (propagated-inputs (list java-cglib
                             java-jakarta-oro
                             java-jdom
                             java-log4j-1.2-api
                             java-native-access
                             java-native-access-platform
                             java-jsr166e-seqlock
                             java-picocontainer
                             intellij-util-rt-141
                             java-jetbrains-trove4j))
    (arguments
     `(#:jar-name "intellij-util.jar"
       #:source-dir "platform/util/src"
       #:test-dir "platform/util/testSrc"
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-tests
                    (lambda _
                      (for-each delete-file
                                (list
                                 ;; Remove a Mac only test
                                 "platform/util/testSrc/com/intellij/util/FoundationTest.java"
                                 ;; Remove tests requiring resources from other modules
                                 "platform/util/testSrc/com/intellij/util/io/zip/ReorderJarsTest.java"))))
                  (add-before 'build 'copy-resources
                    (lambda _
                      (copy-recursively "platform/util/resources"
                                        "build/classes")))
                  (add-before 'build 'copy-resources-for-tests
                    (lambda _
                      (copy-recursively "platform/platform-resources/src"
                                        "build/test-classes")
                      (copy-recursively "platform/platform-resources-en/src"
                                        "build/test-classes")))
                  (add-before 'build 'fix-test-target
                    (lambda _
                      (substitute* "build.xml"
                        (("\\$\\{test\\.home\\}/java")
                         "${test.home}")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util")
    (description "IntelliJ Platform, util submodule")
    (license license:asl2.0)))

(define intellij-util-143
  (package
    (name "intellij-util")
    (version "143")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "0n52rkag7iav3lxzmvqbfg3ngw7rymfzhaxj6jshfdw9yw2x45pj"))
       (patches '("patches/sdk-143.patch" "patches/sdk-util-143-jdom.patch"
                  "patches/sdk-util-143-remove-imgscalr.patch"
                  "patches/sdk-util-143-tests.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (rename-file "platform/util" "module")
                   ;; Keep "bin/idea.properties" as it is needed for tests
                   (rename-file "bin/idea.properties" "module/idea.properties")

                   (copy-recursively "platform/platform-resources/src"
                                     "module/test-resources")
                   (copy-recursively "platform/platform-resources-en/src"
                                     "module/test-resources")

                   ;; Keep only the module source and a required file from bin
                   (use-modules (ice-9 ftw)
                                (ice-9 regex))
                   (for-each (lambda (f)
                               (delete-file-recursively f))
                             (filter (lambda (n)
                                       (not (regexp-match? (string-match
                                                            "^(\\.+|module)$"
                                                            n))))
                                     (scandir ".")))
                   (mkdir "bin")
                   (rename-file "module/idea.properties" "bin/idea.properties")

                   (for-each delete-file
                             (find-files "module"
                                         ".*\\.(a|class|exe|jar|so|zip)$"))))))
    (build-system ant-build-system)
    (native-inputs (list intellij-annotations-143 java-assertj java-junit
                         java-hamcrest-all))
    (propagated-inputs (list java-cglib
                             java-jakarta-oro
                             java-jdom
                             java-log4j-1.2-api
                             java-native-access
                             java-native-access-platform
                             java-jsr166e-seqlock
                             java-iq80-snappy
                             java-picocontainer
                             intellij-util-rt-143
                             java-jetbrains-trove4j))
    (arguments
     `(#:jar-name "intellij-util.jar"
       #:source-dir "module/src"
       #:test-dir "module/testSrc"
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-mac-code
                    (lambda _
                      (for-each delete-file
                                (list
                                 "module/src/com/intellij/util/AppleHiDPIScaledImage.java"
                                 "module/src/com/intellij/util/ui/MacUIUtil.java"))
                      (delete-file-recursively
                       "module/src/com/intellij/ui/mac")))
                  (add-before 'build 'remove-incompatible-tests
                    (lambda _
                      (for-each delete-file
                                (list
                                 ;; Remove a Mac only test
                                 "module/testSrc/com/intellij/util/FoundationTest.java"
                                 ;; This test crashes and is hard to debug
                                 "module/testSrc/com/intellij/openapi/util/io/FileAttributesReadingTest.java"
                                 ;; This test requires almost a half of IJ SDK to run
                                 "module/testSrc/com/intellij/util/lang/ReorderJarsTest.java"))))
                  (add-before 'build 'copy-resources
                    (lambda _
                      (copy-recursively "module/resources" "build/classes")
                      (copy-recursively "module/test-resources"
                                        "build/test-classes")))
                  (add-before 'build 'fix-test-target
                    (lambda _
                      (substitute* "build.xml"
                        (("\\$\\{test\\.home\\}/java")
                         "${test.home}")
                        (("<junit" all)
                         (string-append all " filtertrace=\"false\""))))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ Platform: Util")
    (description "IntelliJ Platform, util submodule")
    (license license:asl2.0)))

(define intellij-jps-model-api-139
  (package
    (name "intellij-jps-model-api")
    (version "139")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/26e72feacf91bfb222bec00b3139ed05aa3084b5.tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "04x2i75kqvxayykniwk2qgs6yh219hxyqjsakmj4brz0g7vjykgq"))
       (patches '("patches/sdk-139.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list intellij-annotations-141))
    (propagated-inputs (list intellij-util-rt-139))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-jps-model-api.jar"
       #:source-dir "jps/model-api/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.6")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model API")
    (description
     "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build.  This package contains 'model-api' submodule.")
    (license license:asl2.0)))

(define intellij-jps-model-api-141
  (package
    (name "intellij-jps-model-api")
    (version "141")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "1s2ys8k7jnr9v05765ql5rk94wnfy18grdiyvynb7pskn96x5jj4"))
       (patches '("patches/sdk-141.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list intellij-annotations-141))
    (propagated-inputs (list intellij-util-rt-141))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-jps-model-api.jar"
       #:source-dir "jps/model-api/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.6")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model API")
    (description
     "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build.  This package contains 'model-api' submodule.")
    (license license:asl2.0)))

(define intellij-jps-model-api-143
  (package
    (name "intellij-jps-model-api")
    (version "143")
    (source
     (intellij-module-by-branch (list "jps/model-api") version
      "0n52rkag7iav3lxzmvqbfg3ngw7rymfzhaxj6jshfdw9yw2x45pj"
      '()))
    (native-inputs (list intellij-annotations-143))
    (propagated-inputs (list intellij-util-rt-143))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-jps-model-api.jar"
       #:source-dir "module/src"
       #:tests? #f ;This module doesn't have tests
       #:make-flags (list "-Dant.build.javac.target=1.6")))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model API")
    (description
     "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build.  This package contains 'model-api' submodule.")
    (license license:asl2.0)))

(define intellij-jps-model-impl-139
  (package
    (name "intellij-jps-model-impl")
    (version "139")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/26e72feacf91bfb222bec00b3139ed05aa3084b5.tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "04x2i75kqvxayykniwk2qgs6yh219hxyqjsakmj4brz0g7vjykgq"))
       (patches '("patches/sdk-139.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list intellij-annotations-141))
    (propagated-inputs (list intellij-jps-model-api-139 intellij-util-139))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-jps-model-impl.jar"
       #:source-dir "jps/model-impl/src"
       ;; tests require testFramework module that is hard to separate from UI and other things not needed for Kotlin
       #:tests? #f
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'fix-test-target
                    (lambda _
                      (substitute* "build.xml"
                        (("\\$\\{test\\.home\\}/java")
                         "${test.home}"))))
                  (add-after 'build 'copy-metadata
                    (lambda _
                      (copy-recursively "jps/model-impl/src/META-INF"
                                        "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model implementation")
    (description
     "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build.  This package contains 'model-impl' submodule.")
    (license license:asl2.0)))

(define intellij-jps-model-impl-141
  (package
    (name "intellij-jps-model-impl")
    (version "141")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "1s2ys8k7jnr9v05765ql5rk94wnfy18grdiyvynb7pskn96x5jj4"))
       (patches '("patches/sdk-141.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list intellij-annotations-141))
    (propagated-inputs (list intellij-jps-model-api-141 intellij-util-141))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-jps-model-impl.jar"
       #:source-dir "jps/model-impl/src"
       ;; tests require testFramework module that is hard to separate from UI and other things not needed for Kotlin
       #:tests? #f
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'fix-test-target
                    (lambda _
                      (substitute* "build.xml"
                        (("\\$\\{test\\.home\\}/java")
                         "${test.home}"))))
                  (add-after 'build 'copy-metadata
                    (lambda _
                      (copy-recursively "jps/model-impl/src/META-INF"
                                        "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model implementation")
    (description
     "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build.  This package contains 'model-impl' submodule.")
    (license license:asl2.0)))

(define intellij-jps-model-impl-143
  (package
    (name "intellij-jps-model-impl")
    (version "143")
    (source
     (intellij-module-by-branch (list "jps/model-impl") version
      "0n52rkag7iav3lxzmvqbfg3ngw7rymfzhaxj6jshfdw9yw2x45pj"
      '()))
    (native-inputs (list intellij-annotations-143))
    (propagated-inputs (list intellij-jps-model-api-143 intellij-util-143))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-jps-model-impl.jar"
       #:source-dir "module/src"
       ;; tests require testFramework module that is hard to separate from UI and other things not needed for Kotlin
       #:tests? #f
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases (modify-phases %standard-phases
                  (add-after 'build 'copy-metadata
                    (lambda _
                      (copy-recursively "jps/model-impl/src/META-INF"
                                        "build/classes/META-INF"))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "JetBrains Java Project System: Model implementation")
    (description
     "Gant based build framework + dsl, with declarative project structure definition and automatic IntelliJ IDEA projects build.  This package contains 'model-impl' submodule.")
    (license license:asl2.0)))

(define intellij-core-kotlin-139
  (package
    (name "intellij-core-kotlin")
    (version "139")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/26e72feacf91bfb222bec00b3139ed05aa3084b5.tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "04x2i75kqvxayykniwk2qgs6yh219hxyqjsakmj4brz0g7vjykgq"))
       (patches '("patches/sdk-139.patch"
                  "patches/sdk-core-139-compatibility.patch")) ;TODO: is compat patch still needed?
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list intellij-annotations-141 java-jmock-1 java-junit
                         java-hamcrest-all unzip))
    (propagated-inputs (list java-automaton
                             java-javax-inject-java6
                             java-jetbrains-asm-4
                             java-jetbrains-asm-5
                             java-iq80-snappy
                             java-xstream
                             intellij-jps-model-impl-139
                             intellij-util-139))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-core.jar"
       ;; Tests depend on JUnit compiled with default GUIX JDK, so use the same JDK here
       #:source-dir "combined/src"
       #:test-dir "combined/testSrc"
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases ,#~(modify-phases %standard-phases
                     (add-after 'unpack 'copy-module-sources
                       (lambda _
                         (copy-recursively "java/java-psi-api/src"
                                           "combined/src") ;This module doesn't have tests
                         (copy-recursively "java/java-psi-impl/gen"
                                           "combined/src") ;TODO: should these sources be regenerated?
                         (copy-recursively "java/java-psi-impl/src"
                                           "combined/src") ;This module doesn't have tests
                         (copy-recursively "platform/boot/src" "combined/src") ;This module doesn't have tests
                         (copy-recursively "platform/core-api/src"
                                           "combined/src") ;This module doesn't have tests
                         (copy-recursively "platform/core-impl/src"
                                           "combined/src") ;This module doesn't have tests
                         
                         (copy-recursively "platform/extensions/src"
                                           "combined/src")
                         (copy-recursively "platform/extensions/testSrc"
                                           "combined/testSrc")))
                     (add-after 'unpack 'copy-module-messages
                       (lambda _
                         (copy-recursively "java/java-psi-api/src/messages"
                                           "build/classes/messages")
                         (copy-recursively "java/java-psi-impl/src/messages"
                                           "build/classes/messages")))
                     (add-after 'unpack 'copy-module-metadata
                       (lambda _
                         (copy-recursively "platform/boot/src/META-INF"
                                           "build/classes/META-INF")))
                     (add-before 'build 'fix-test-target
                       (lambda _
                         (substitute* "build.xml"
                           (("\\$\\{test\\.home\\}/java")
                            "${test.home}"))))
                     (add-before 'build 'unzip-jars
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "build/classes")
                         (for-each (lambda (p)
                                     (let ((jars (find-files (assoc-ref inputs
                                                              p)
                                                  ;; Exclude javadoc and other variants
                                                  "([[:digit:]]|^[^[:digit:]]+)\\.jar$")))
                                       
                                       (invoke (string-append #$unzip
                                                              "/bin/unzip")
                                               (if (= 1
                                                      (length jars))
                                                   (car jars)
                                                   (throw 'multiple-jars-found
                                                          p))
                                               "-d"
                                               "build/classes"
                                               ;; These files are generated by 'jar' target for each jar file it creates
                                               "-x"
                                               "META-INF/INDEX.LIST"
                                               "META-INF/MANIFEST.MF")))
                                   (list "java-jdom"
                                         "java-javax-inject"

                                         "intellij-jps-model-api"
                                         "intellij-jps-model-impl"
                                         "intellij-util"
                                         "intellij-util-rt")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ platform: parts required for kotlin")
    (description
     "This package provides minimal set of modules needed for compiling kotlinc and standard libraries.")
    (license license:asl2.0)))

(define intellij-core-kotlin-141
  (package
    (name "intellij-core-kotlin")
    (version "141")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JetBrains/intellij-community/archive/refs/heads/"
             version ".tar.gz"))
       (file-name (string-append "intellij-community-" version ".tar.gz"))
       (sha256
        (base32 "1s2ys8k7jnr9v05765ql5rk94wnfy18grdiyvynb7pskn96x5jj4"))
       (patches '("patches/sdk-141.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "bin")
                   (delete-file-recursively "lib")
                   (delete-file-recursively "plugins")
                   (delete-file-recursively "python")
                   (for-each delete-file
                             (find-files "." ".*\\.(a|class|exe|jar|so|zip)$"))
                   #t))))
    (native-inputs (list intellij-annotations-141 java-jmock-1 java-junit
                         java-hamcrest-all unzip))
    (propagated-inputs (list java-automaton
                             java-guava-patched-20
                             java-javax-inject-java6
                             java-jetbrains-asm-4
                             java-jetbrains-asm-5
                             java-iq80-snappy
                             java-xstream
                             intellij-jps-model-impl-141
                             intellij-util-141))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-core.jar"
       ;; Tests depend on JUnit compiled with default GUIX JDK, so use the same JDK here
       #:source-dir "combined/src"
       #:test-dir "combined/testSrc"
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases ,#~(modify-phases %standard-phases
                     (add-after 'unpack 'copy-module-sources
                       (lambda _
                         (copy-recursively "java/java-psi-api/src"
                                           "combined/src") ;This module doesn't have tests
                         (copy-recursively "java/java-psi-impl/gen"
                                           "combined/src") ;TODO: should these sources be regenerated?
                         (copy-recursively "java/java-psi-impl/src"
                                           "combined/src") ;This module doesn't have tests
                         (copy-recursively "platform/boot/src" "combined/src") ;This module doesn't have tests
                         (copy-recursively "platform/core-api/src"
                                           "combined/src") ;This module doesn't have tests
                         (copy-recursively "platform/core-impl/src"
                                           "combined/src") ;This module doesn't have tests
                         
                         (copy-recursively "platform/extensions/src"
                                           "combined/src")
                         (copy-recursively "platform/extensions/testSrc"
                                           "combined/testSrc")))
                     (add-after 'unpack 'copy-module-messages
                       (lambda _
                         (copy-recursively "java/java-psi-api/src/messages"
                                           "build/classes/messages")
                         (copy-recursively "java/java-psi-impl/src/messages"
                                           "build/classes/messages")))
                     (add-after 'unpack 'copy-module-metadata
                       (lambda _
                         (copy-recursively "platform/boot/src/META-INF"
                                           "build/classes/META-INF")))
                     (add-before 'build 'fix-test-target
                       (lambda _
                         (substitute* "build.xml"
                           (("\\$\\{test\\.home\\}/java")
                            "${test.home}"))))
                     (add-before 'build 'unzip-jars
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "build/classes")
                         (for-each (lambda (p)
                                     (let ((jars (find-files (assoc-ref inputs
                                                              p)
                                                  ;; Exclude javadoc and other variants
                                                  "([[:digit:]]|^[^[:digit:]]+)\\.jar$")))
                                       
                                       (invoke (string-append #$unzip
                                                              "/bin/unzip")
                                               (if (= 1
                                                      (length jars))
                                                   (car jars)
                                                   (throw 'multiple-jars-found
                                                          p))
                                               "-d"
                                               "build/classes"
                                               ;; These files are generated by 'jar' target for each jar file it creates
                                               "-x"
                                               "META-INF/INDEX.LIST"
                                               "META-INF/MANIFEST.MF")))
                                   (list "java-jdom"
                                         "java-javax-inject"

                                         "intellij-jps-model-api"
                                         "intellij-jps-model-impl"
                                         "intellij-util"
                                         "intellij-util-rt")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ platform: parts required for kotlin")
    (description
     "This package provides minimal set of modules needed for compiling kotlinc and standard libraries.")
    (license license:asl2.0)))

(define intellij-core-kotlin-143
  (package
    (name "intellij-core-kotlin")
    (version "143")
    (source
     (intellij-module-by-branch (list "java/java-psi-api"
                                      "java/java-psi-impl"
                                      "platform/boot"
                                      "platform/core-api"
                                      "platform/core-impl"
                                      "platform/extensions") version
      "0n52rkag7iav3lxzmvqbfg3ngw7rymfzhaxj6jshfdw9yw2x45pj"
      '("patches/sdk-core-143-jdom.patch"
        "patches/sdk-core-143-remove-mac.patch")))
    (native-inputs (list intellij-annotations-143 java-jmock-1 java-junit
                         java-hamcrest-all unzip))
    (propagated-inputs (list java-automaton
                             java-guava-patched-20
                             java-javax-inject-java6
                             java-jetbrains-asm-4
                             java-jetbrains-asm-5
                             java-iq80-snappy
                             java-xstream
                             intellij-jps-model-impl-143
                             intellij-util-143))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "intellij-core.jar"
       ;; Tests depend on JUnit compiled with default GUIX JDK, so use the same JDK here
       #:source-dir "module/src"
       #:test-dir "module/testSrc"
       #:make-flags (list "-Dant.build.javac.target=1.6")
       #:phases ,#~(modify-phases %standard-phases
                     (add-after 'unpack 'copy-generated-sources
                       (lambda _
                         (copy-recursively "module/gen" "module/src"))) ;TODO: should these sources be regenerated?
                     (add-after 'unpack 'copy-module-messages
                       (lambda _
                         (copy-recursively "module/src/messages"
                                           "build/classes/messages")))
                     (add-after 'unpack 'copy-module-metadata
                       (lambda _
                         (copy-recursively "module/src/META-INF"
                                           "build/classes/META-INF")))
                     (add-before 'build 'fix-test-target
                       (lambda _
                         (substitute* "build.xml"
                           (("\\$\\{test\\.home\\}/java")
                            "${test.home}"))))
                     (add-before 'build 'unzip-jars
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "build/classes")
                         (for-each (lambda (p)
                                     (let ((jars (find-files (assoc-ref inputs
                                                              p)
                                                  ;; Exclude javadoc and other variants
                                                  "([[:digit:]]|^[^[:digit:]]+)\\.jar$")))
                                       
                                       (invoke (string-append #$unzip
                                                              "/bin/unzip")
                                               (if (= 1
                                                      (length jars))
                                                   (car jars)
                                                   (throw 'multiple-jars-found
                                                          p))
                                               "-d"
                                               "build/classes"
                                               ;; These files are generated by 'jar' target for each jar file it creates
                                               "-x"
                                               "META-INF/INDEX.LIST"
                                               "META-INF/MANIFEST.MF")))
                                   (list "java-jdom"
                                         "java-javax-inject"

                                         "intellij-jps-model-api"
                                         "intellij-jps-model-impl"
                                         "intellij-util"
                                         "intellij-util-rt")))))))
    (home-page "https://www.jetbrains.com/opensource/idea/")
    (synopsis "IntelliJ platform: parts required for kotlin")
    (description
     "This package provides minimal set of modules needed for compiling kotlinc and standard libraries.")
    (license license:asl2.0)))

(define kotlin-0.6.786-bootstrap
  (package
    (name "kotlin")
    (version "0.6.786")
    (source
     (kotlin-source-by-tag version
      "1kdylhafh978hwdl2xmis7j9cp7wbma9ghkaci5p4nqzkr4qi383"
      '("patches/kotlin-0.6.786-sdk-139.patch")))
    (native-inputs (list ant
                         ant-contrib
                         java-cli-parser
                         java-jline-2
                         java-guava-patched-20
                         java-javax-inject-java6
                         java-protobuf-api-2.5
                         intellij-annotations-141
                         intellij-core-kotlin-139
                         intellij-jps-model-impl-139))
    (propagated-inputs '()) ;TODO: this means do not propagate anything, right?
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:jdk ,icedtea-7
       #:make-flags ,#~(list (string-append "-Dkotlin-home="
                                            #$output)
                             "-Dgenerate.assertions=false"
                             "-Dgenerate.javadoc=false" "-Dshrink=false"
                             (string-append "-Dbuild.number="
                                            #$version))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-js-compiler-cli
                    (lambda _
                      (delete-file-recursively
                       "compiler/cli/src/org/jetbrains/jet/cli/js")))
                  (add-before 'build 'remove-js-compiler
                    (lambda _
                      (delete-file-recursively "js")
                      (mkdir-p "js/js.translator/lib")
                      (mkdir-p "js/js.translator/src")))
                  (add-before 'build 'fix-jar-task
                    (lambda _
                      (substitute* "build.xml"
                        ;; 'level' should work even for targets enabling 'compress'
                        (("<(jar|jarjar) " all)
                          (string-append all "level=\"0\" modificationtime=\"0\" ")))))
                  (add-before 'build 'remove-targets
                    (lambda _
                      (substitute* "build.xml"
                        ((",android-compiler-plugin(,|\")" _ after)
                         after)
                        ((",androidSdkAnnotations(,|\")" _ after)
                         after)
                        ((",android-sdk-annotations(,|\")" _ after)
                         after)
                        ((",compilerSources(,|\")" _ after)
                         after)
                        ((",compiler-sources(,|\")" _ after)
                         after)
                        ((",daemon-client(,|\")" _ after)
                         after)
                        ((",j2kConverter(,|\")" _ after)
                         after)
                        ((",jslib(,|\")" _ after)
                         after)
                        ((",runtime_sources(,|\")" _ after)
                         after))))
                  (add-before 'build 'fix-value-order
                    ;; fix non-determenistic bytecode generation caused by random iteration order of basic hash maps and sets
                    (lambda _
                      (with-fluids ((%default-port-encoding "ISO-8859-1"))
                                   (substitute* (find-files "."
                                                            "\\.(java|kt)$")
                                     (("([^_[:alnum:]]|new)Hash(Map|Set)" all
                                       prefix suffix)
                                      (string-append prefix "LinkedHash"
                                                     suffix))))

                      (use-modules (ice-9 string-fun))
                      (for-each (lambda (f)
                                  (rename-file f
                                               (string-replace-substring f
                                                "Hash" "LinkedHash")))
                                (find-files "."
                                 "(^|[^_[:alnum:]]|new)Hash(Map|Set)"))))
                  (add-before 'build 'disable-classpath-from-env
                    (lambda _
                      (substitute* "build.xml"
                        (("<project[^>]+>" all)
                         (string-append all
                          "<property name=\"build.sysclasspath\" value=\"ignore\"/>")))))
                  (add-before 'build 'update-asm
                    (lambda _
                      (substitute* (find-files "." "\\.(java|kt)$")
                        (("([^[:alnum:]])org\\.jetbrains\\.asm4([^[:alnum:]])"
                          _ before after)
                         (string-append before
                                        "org.jetbrains.org.objectweb.asm"
                                        after))
                        (("(new InstructionAdapter\\()(.+\\)[[:space:]]+\\{[[:space:]]*)$"
                          _ before after)
                         (string-append before
                          "org.jetbrains.org.objectweb.asm.Opcodes.ASM4, "
                          after))
                        (("(new MethodNode\\()(.+\\)[[:space:]]+\\{[[:space:]]*)$"
                          _ before after)
                         (string-append before
                          "org.jetbrains.org.objectweb.asm.Opcodes.ASM4, "
                          after)))))
                  (add-before 'build 'prepare-ant-lib
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; build.xml expects exact file names in dependencies directory
                      (mkdir-p "dependencies")
                      (symlink (string-append (assoc-ref inputs "ant")
                                              "/lib/ant.jar")
                               "dependencies/ant.jar")))
                  (add-before 'build 'prepare-dependencies
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; build.xml expects exact file names in dependencies directory
                      (mkdir-p "dependencies")
                      (symlink (string-append (assoc-ref inputs "java-jline")
                                              "/share/java/jline.jar")
                               "dependencies/jline.jar")
                      (symlink (string-append (assoc-ref inputs
                                                         "java-cli-parser")
                                              "/share/java/cli-parser.jar")
                               "dependencies/cli-parser-1.1.1.jar")))
                  (add-before 'build 'prepare-idea-core-libs
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; build.xml expects exact file names in dependencies directory
                      (mkdir-p "ideaSDK/core")
                      (for-each (lambda (p)
                                  (let* ((allJars (find-files (assoc-ref
                                                               inputs p)
                                                   ;; Exclude javadoc and other variants
                                                   "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                                         (mainJar (if (= 1
                                                         (length allJars))
                                                      (car allJars)
                                                      (throw 'no-or-multiple-jars-found
                                                             p))))
                                    
                                    (symlink mainJar
                                             (string-append "ideaSDK/core/"
                                                            (basename mainJar)))))
                                (list "java-cli-parser"
                                      "java-guava"
                                      "java-jetbrains-asm-5"
                                      "intellij-annotations"
                                      "intellij-core-kotlin"
                                      "java-picocontainer"
                                      "java-jetbrains-trove4j"))))
                  (add-before 'build 'prepare-protobuf-lib
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; build.xml expects exact file names in ideaSDK/lib
                      (mkdir-p "ideaSDK/lib")
                      (symlink (string-append (assoc-ref inputs
                                                         "java-protobuf-api")
                                              "/share/java/protobuf.jar")
                               "ideaSDK/lib/protobuf-2.5.0.jar") #t))

                  (delete 'install) ;already implemented in build.xml
                  
                  ;; add-after+delete to ease restoring the phase in inheriting packages:
                  ;; TODO: restore all these in the public package
                  ;; distribution jars don't depend on any packages anyway
                  (add-after 'generate-jar-indices 'generate-jar-indices-stub
                    (lambda _
                      #t))
                  (delete 'generate-jar-indices)
                  ;; jar task is configured to be reproducible instead and also no 'generate-jar-indices' phase
                  (add-after 'reorder-jar-content 'reorder-jar-content-stub
                    (lambda _
                      #t))
                  (delete 'reorder-jar-content)
                  ;; not needed when no 'generate-jar-indices' phase
                  (add-after 'strip-jar-timestamps 'strip-jar-timestamps-stub
                    (lambda _
                      #t))
                  (delete 'strip-jar-timestamps))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Kotlin programming language")
    (description "Kotlin programming language")
    (license license:asl2.0)))

(define kotlin-0.6.1364-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.6.786-bootstrap
   "0.6.1364"
   "13swfp7v6rwjfwdjps0bykk15yss68d8c36q7pmbzd0pakh156ch"
   '("patches/kotlin-0.6.786-sdk-139.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (replace 'prepare-ant-lib
                       (lambda* (#:key inputs #:allow-other-keys)
                         ;; build.xml expects exact file names in dependencies directory
                         (mkdir-p "dependencies/ant-1.7/lib")
                         (symlink (string-append (assoc-ref inputs "ant")
                                                 "/lib/ant.jar")
                                  "dependencies/ant-1.7/lib/ant.jar")))
                     (add-after 'prepare-idea-core-libs 'prepare-idea-core-libs-log4j-api
                       (lambda* (#:key inputs #:allow-other-keys)
                         (let* ((p "java-log4j-1.2-api")
                                (allJars (find-files (assoc-ref inputs p)
                                          ;; Exclude javadoc and other variants
                                          "(^[^[:digit:]]+|[[:digit:]]|4j|api)\\.jar$"))
                                (mainJar (if (= 1
                                                (length allJars))
                                             (car allJars)
                                             (throw 'no-or-multiple-jars-found
                                                    p))))
                           (symlink mainJar
                                    (string-append "ideaSDK/core/"
                                                   (basename mainJar))))))))))

(define kotlin-0.6.1932-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.6.1364-bootstrap
   "0.6.1932"
   "0r6dz0frkvg035phi0dsp718gjy8sdmhpkn2h7s89c2a3qb72chr"
   '("patches/kotlin-0.6.786-sdk-139.patch"
     "patches/kotlin-0.6.1932-remove-ant-k2js.patch")
   #:set-native-inputs (lambda (inherited-inputs)
                         (modify-inputs inherited-inputs
                           (append intellij-jps-model-impl-139)))
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-before 'build 'copy-runtime-annotations
                       (lambda* _
                         (copy-recursively
                          "runtime/src/org/jetbrains/annotations"
                          "core/util.runtime/src/org/jetbrains")))
                     (add-before 'build 'remove-k2js
                       (lambda* _
                         (delete-file
                          "build-tools/ant/src/org/jetbrains/jet/buildtools/ant/Kotlin2JsCompilerTask.kt")))
                     (add-before 'build 'prepare-idea-jps-libs
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "ideaSDK/jps")
                         (symlink (string-append (assoc-ref inputs
                                                  "intellij-jps-model-impl")
                                   "/share/java/intellij-jps-model-impl.jar")
                                  "ideaSDK/jps/jps-model.jar")))))))

(define kotlin-0.6.2107-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.6.1932-bootstrap
   "0.6.2107"
   "1aav1hqzqscxi185adcdr4fc2anhfh1jrh77fnvgsrp8nxfv6h20"
   '("patches/kotlin-0.6.786-sdk-139.patch"
     "patches/kotlin-0.6.1932-remove-ant-k2js.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (delete 'copy-runtime-annotations)
                     (add-after 'remove-js-compiler 'remove-js-compiler-fixup-1
                       (lambda _
                         (mkdir-p "js/js.dart-ast/src")))
                     (add-before 'build 'add-noverify
                       ;; TODO: This should not be needed
                       (lambda _
                         (setenv "ANT_OPTS" "-noverify")))))))

(define kotlin-jdk-annotations-patched
  (package
    (name "kotlin-jdk-annotations")
    ;; This is the version from which the annotations are checked out. However, they are patched to match
    ;; Kotlin 0.6.2451+ stdlib
    (version "0.6.2338")
    (source
     (kotlin-source-by-tag version
      "0n61ayxcr75xjcl72227176chp100bndy2n95j5j18q5ksp61mq8"
      '("patches/kotlin-annotations-0.6.2338.patch")))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "kotlin-jdk-annotations.jar"
       #:jdk ,icedtea-7
       #:source-dir "empty-dir"
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'prepare-empty-dir
                    (lambda _
                      (mkdir-p "empty-dir")))
                  (add-before 'build 'add-xmls
                    (lambda _
                      (mkdir-p "empty-dir")
                      (mkdir-p "build/classes")
                      (copy-recursively "jdk-annotations" "build/classes"))))))
    (home-page "https://kotlinlang.org/")
    (synopsis
     "Kotlin programming language: External annotations for earlier compiler versions")
    (description
     "Kotlin required external nullability annotation jars until platform types were introduced.  This package provides external annotations necessary to compile Kotlin standard libraries for those earlier versions.")
    (license license:asl2.0)))

(define kotlin-0.6.2451-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.6.2107-bootstrap
   "0.6.2451"
   "180i65x3pd7l3xbbimrxzscaqqvn6h190amkmvjx3dxzfynw8l6b"
   '("patches/kotlin-0.6.786-sdk-139.patch"
     "patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch"
     "patches/kotlin-0.6.2451-sdk-139.patch")
   #:set-native-inputs (lambda (inherited-inputs)
                         (modify-inputs inherited-inputs
                           (append kotlin-jdk-annotations-patched)))
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-before 'build 'prepare-annotations-lib
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "dependencies/annotations")
                         (symlink (string-append (assoc-ref inputs
                                                  "kotlin-jdk-annotations")
                                   "/share/java/kotlin-jdk-annotations.jar")
                          "dependencies/annotations/kotlin-jdk-annotations.jar")))))))
(define kotlin-0.6.2516-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.6.2451-bootstrap "0.6.2516"
   "0phq1mnmv0xfrmyg6jia4aqknglbizfnyiwfm6fc714kv9vw7pfg"
   '("patches/kotlin-0.6.786-sdk-139.patch"
     "patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch"
     "patches/kotlin-0.6.2451-sdk-139.patch")))
(define kotlin-0.7.333-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.6.2516-bootstrap "0.7.333"
   "12y7rxwy42j3g2lkjwl368glxaqa2iv4rjivbijp8jk70yknf4v6"
   '("patches/kotlin-0.6.786-sdk-139.patch"
     "patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch"
     "patches/kotlin-0.6.2451-sdk-139.patch"
     "patches/kotlin-0.7.333-asm.patch")))

(define kotlin-0.7.638-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.7.333-bootstrap
   "0.7.638"
   "16k9nsac5pdiqya0nb581m5xh6szc8z5xjm9imyw26c9x36q6igs"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (delete 'update-asm)))))
(define kotlin-0.7.1214-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.7.638-bootstrap "0.7.1214"
   "0k3wgai1972jr33bgq0si24xw2gjfpl0ssgsmj76f2jrwvkknx7w"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch")))
(define kotlin-0.8.84-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.7.1214-bootstrap "0.8.84"
   "0610mcyp16q5ykqpjpdnxvsx46h7f8dac7dphc7avgxzl7anb516"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch")))

(define kotlin-0.8.409-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.8.84-bootstrap "0.8.409"
   "0jlp73ka0iwmrblp9f32h3ckv1mx0cbg20cabk1j7vs47s1am0is"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")))
(define kotlin-0.8.418-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.8.409-bootstrap "0.8.418"
   "03rx9n2pcw0crxg4aa9wvgaprvl6yc9dxd12wc2phpq23d3r11ix"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")))
(define kotlin-0.8.422-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.8.418-bootstrap "0.8.422"
   "02n7vqjsqk371apnddi86ckdlpgvhxg8h2yra3g25fh818gagsys"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.6.2451-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")))

(define kotlin-0.8.1444-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.8.422-bootstrap "0.8.1444"
   "1a108mp6v073v86lpbjfj8lznpwfkbv5qd8czlpslbanzq4gwnxc"
   '("patches/kotlin-0.6.1932-remove-ant-k2js.patch"
     "patches/kotlin-0.8.1444-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")))

(define kotlin-0.9.21-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.8.1444-bootstrap
   "0.9.21"
   "0x9hsxaarpzc5fbbx19w738qhc6l72q78fw9lvwpfvgjal447lbk"
   '("patches/kotlin-0.9.21-remove-ant-k2js.patch"
     "patches/kotlin-0.8.1444-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (replace 'remove-k2js
                       (lambda* _
                         (delete-file
                          "build-tools/ant/src/org/jetbrains/jet/buildtools/ant/Kotlin2JsTask.kt")))))))
(define kotlin-0.9.738-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.9.21-bootstrap "0.9.738"
   "0mp0xp9x200mcap46dnkhk6mi8h5cp4f064hy50m5dz6aqn2jrzl"
   '("patches/kotlin-0.9.21-remove-ant-k2js.patch"
     "patches/kotlin-0.8.1444-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")))

(define kotlin-0.9.1204-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.9.738-bootstrap
   "0.9.1204"
   "0n2fijxznjd5w1kkw807pfvr0y5xs598xgvmxdr7b0xipnicwyww"
   '("patches/kotlin-0.9.21-remove-ant-k2js.patch"
     "patches/kotlin-0.8.1444-fix-class-path.patch"
     "patches/kotlin-0.8.409-asm.patch")
   #:set-native-inputs (lambda (inherited-inputs)
                         (modify-inputs inherited-inputs
                           (append kotlin-0.9.738-bootstrap)))))

(define kotlin-0.10.300-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.9.1204-bootstrap
   "0.10.300"
   "0xwi92lm1q09dj2nvd1i7315p7fdw2zzxmlgr5gckzm9g9rr8dnr"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.8.409-asm.patch")
   #:set-native-inputs (lambda (inherited-inputs)
                         (modify-inputs inherited-inputs
                           (replace "kotlin" kotlin-0.9.1204-bootstrap)))
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (replace 'remove-k2js
                       (lambda* _
                         (delete-file
                          "ant/src/org/jetbrains/kotlin/ant/Kotlin2JsTask.kt")))))))

(define kotlin-0.10.823-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.10.300-bootstrap
   "0.10.823"
   "0f8jd17sh1g9q7pzqg81cfxjyzfr1cxl55wixryjbqyi8m2cmr8w"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch")
   #:set-native-inputs (lambda (inherited-inputs)
                         (modify-inputs inherited-inputs
                           (delete "kotlin")))
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (replace 'remove-js-compiler-cli
                       (lambda _
                         (delete-file-recursively
                          "compiler/cli/src/org/jetbrains/kotlin/cli/js")))
                     (add-before 'build 'remove-skip-filter
                       (lambda _
                         (substitute* "build.xml"
                           ;; This filter only makes sense for new javac2 task, that is not used here
                           (("<skip [^>]+/>")
                            ""))))))))

(define kotlin-0.10.1023-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.10.823-bootstrap
   "0.10.1023"
   "0xyw17vdh8h9jvfssb3wkf7vrxp3b3b43mw3yzsfbljnvlhzgvch"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch")
   #:set-native-inputs (lambda _
                         (list ant
                               ant-contrib
                               java-cli-parser
                               java-jline-2
                               java-guava-patched-20
                               java-javax-inject-java6
                               java-protobuf-api-2.5
                               intellij-annotations-141
                               intellij-core-kotlin-141
                               intellij-jps-model-impl-141
                               kotlin-jdk-annotations-patched))))

(define kotlin-0.10.1336-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.10.1023-bootstrap "0.10.1336"
   "0dsffzffanvgabyxs3sl8p7j8gpqbcazg84shbipb9i2p47n5a1c"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch")))

(define kotlin-0.10.1426-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.10.1336-bootstrap
   "0.10.1426"
   "02w1pn3rc3lzr090bxkpz4lvzynaybvxhh96q71s9f1s43jwcz1m"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-after 'prepare-dependencies 'prepare-protobuf-api
                       (lambda* (#:key inputs #:allow-other-keys)
                         ;; build.xml expects exact file names in dependencies directory
                         (mkdir-p "dependencies")
                         (symlink (string-append (assoc-ref inputs
                                                  "java-protobuf-api")
                                                 "/share/java/protobuf.jar")
                                  "dependencies/protobuf-2.5.0-lite.jar")))
                     (add-after 'prepare-dependencies 'prepare-idea-libs
                       (lambda* (#:key inputs #:allow-other-keys)
                         ;; build.xml expects exact file names in dependencies directory
                         (mkdir-p "ideaSDK/lib")
                         (symlink (string-append (assoc-ref inputs
                                                  "java-native-access-platform")
                                   "/share/java/jna-platform.jar")
                                  "ideaSDK/lib/jna-utils.jar")
                         (symlink (search-input-file inputs
                                                     (string-append
                                                      "/share/java/jakarta-oro-"
                                                      ,(package-version
                                                        java-jakarta-oro)
                                                      ".jar"))
                                  "ideaSDK/lib/oromatcher.jar")))
                     (add-after 'prepare-dependencies 'prepare-lib-inject
                       (lambda* (#:key inputs #:allow-other-keys)
                         ;; build.xml expects exact file names in dependencies directory
                         (mkdir-p "lib")
                         (symlink (search-input-file inputs ;TODO: how to refer to a version of the package in inputs?
                                                     ,(let ((p
                                                             java-javax-inject-java6))
                                                        (string-append
                                                         "/lib/m2/javax/inject/javax.inject/"
                                                         (package-version p)
                                                         "/javax.inject-"
                                                         (package-version p)
                                                         ".jar")))
                                  "lib/javax.inject.jar")))))))
(define kotlin-0.10.1464-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.10.1426-bootstrap "0.10.1464"
   "1nbi31whxqskqpcvlm3x5cv5294ily2b9lb8510dyhdf7v5vzpdc"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch")))
(define kotlin-0.11.153-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.10.1464-bootstrap "0.11.153"
   "0j1xmjy9wxmrs6rcfj158pg81q9pcj00r39scr5405y952v3wk1k"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch")))

(define kotlin-0.11.873-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.11.153-bootstrap "0.11.873"
   "15r3b5lfbdvyq9yx0gpfn5wfdsvvnwq239v7yjdynyir3lhn5m94"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.11.992-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.11.873-bootstrap "0.11.992"
   "0dq5i0r4i081gra6zw6sv5s0faxjv1nz7mg1fcfmhcnk1g3xyg7n"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.11.1014-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.11.992-bootstrap "0.11.1014"
   "1as62axvd7c0g3aagcq93y5026r5664rcwb3d39g1qyfg0z8y5wi"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.11.1201-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.11.1014-bootstrap "0.11.1201"
   "1nxx9l71zfwx5jf2w968xrg7n8l9lq9anvxm8nc5959xlgab4wrv"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.11.1393-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.11.1201-bootstrap "0.11.1393"
   "1lcfl1ka8mp16mrk0w3bwlb3g6ag7wvf9i8wpqczhrbmm65ngvaa"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))

(define kotlin-0.12.108-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.11.1393-bootstrap
   "0.12.108"
   "1qql2wn1zfnb7mrpwjpc8kaw1cp3aqmscs4dhanz4yf0zgl7k5wr"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-after 'prepare-dependencies 'prepare-jarjar-dependency
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "dependencies")
                         (symlink (string-append ,java-jarjar
                                                 "/share/java/jarjar-"
                                                 ,(package-version java-jarjar)
                                                 ".jar")
                                  "dependencies/jarjar.jar")))
                     (add-after 'prepare-idea-libs 'prepare-idea-lib-asm
                       (lambda* (#:key inputs #:allow-other-keys)
                         (mkdir-p "ideaSDK/lib")
                         (symlink (string-append ,java-jetbrains-asm-5
                                                 "/lib/m2/org/ow2/asm/asm/"
                                                 ,(package-version
                                                   java-jetbrains-asm-5)
                                                 "/asm-"
                                                 ,(package-version
                                                   java-jetbrains-asm-5)
                                                 ".jar")
                                  "ideaSDK/lib/asm-all.jar")))))))
(define kotlin-0.12.115-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.108-bootstrap "0.12.115"
   "04rzjllifbs731q4hdzldw27pq8lqm50w9k2nvl336apjpd1h64a"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.12.176-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.115-bootstrap "0.12.176"
   "0j2zi95q0ys36n9yjzv4h1hkjcy6v8ickjr19iypnm7337ljllw8"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))

(define kotlin-0.12.470-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.176-bootstrap
   "0.12.470"
   "1w8l0xjxhdsvz29l1jm6w6ddqmi6dnll2lvbv4qyh4nc1whhjk2r"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")
   #:set-jdk (lambda _
               icedtea-8) ;TODO: Try to move this to earlier packages
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (replace 'prepare-ant-lib
                       (lambda _
                         ;; build.xml expects exact file names in dependencies directory
                         (mkdir-p "dependencies/ant-1.8/lib")
                         (symlink (string-append ,ant "/lib/ant.jar")
                                  "dependencies/ant-1.8/lib/ant.jar")))))))

(define kotlin-0.12.1077-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.470-bootstrap
   "0.12.1077"
   "0002sbjalj8f25n974dxadb9ljf65i1bwybcbvr6vnzlya2ykjzb"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")
   #:set-native-inputs (lambda _
                         (list ant
                               ant-contrib
                               java-cli-parser
                               java-jline-2
                               java-guava-patched-20
                               java-javax-inject-java6
                               java-protobuf-api-2.5
                               intellij-annotations-143
                               intellij-core-kotlin-143
                               intellij-jps-model-impl-143
                               kotlin-jdk-annotations-patched))
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-before 'build 'prepare-jansi-libs
                       (lambda _
                         (mkdir-p "dependencies")
                         (symlink (string-append ,java-jansi-1
                                   "/lib/m2/org/fusesource/jansi/jansi/"
                                   ,(package-version java-jansi-1)
                                   "/jansi-"
                                   ,(package-version java-jansi-1)
                                   ".jar") "dependencies/jansi.jar")
                         (symlink (string-append ,java-jansi-native
                                   "/lib/m2/org/fusesource/jansi/jansi-native/"
                                   ,(package-version java-jansi-native)
                                   "/jansi-native-"
                                   ,(package-version java-jansi-native)
                                   ".jar") "dependencies/jansi-native.jar")))
                     (add-after 'prepare-idea-libs 'prepare-idea-libs-jna
                       (lambda _
                         ;; This version of Kotlin updated to a newer version of the jar which has a different file name, while the
                         ;; inherited phases already use that newer version under the old name.
                         (rename-file "ideaSDK/lib/jna-utils.jar"
                                      "ideaSDK/lib/jna-platform.jar")))))))
(define kotlin-0.12.1250-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.1077-bootstrap "0.12.1250"
   "0vangxm9sczxiqbgzlk7faxcm6mmg6ic8k95974j1ddzqkzl39nr"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.12.1306-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.1250-bootstrap "0.12.1306"
   "0sybc5dddzr0fh8rkz78qb27gmqxr9qp6zqs72dhbp9g6hp564ii"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.13.177-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.12.1306-bootstrap "0.13.177"
   "08qglhdb028ik67w2z3l09j9bsn7hf81dxcw5ydamkm2hznahwl4"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.13.791-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.13.177-bootstrap "0.13.791"
   "082h6hfxk4v01ccl2m5f8bqi3flh3ka320xpriizwqrywdkl00hj"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.13.899-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.13.791-bootstrap "0.13.899"
   "1wh6a9vvd6zskr31nqhc9nhqykrfqngv9r48y845cqh0mp2nqc6s"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.13.1118-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.13.899-bootstrap "0.13.1118"
   "0kw00hpalkpy98d0y4nlzrgwb6zbrn6glz6nlixjpkifc04241iz"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.13.1304-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.13.1118-bootstrap "0.13.1304"
   "0xny5qvxag32qz6i80szcbjrr696mq7hfcma5610c774gk8jvskw"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))

(define kotlin-0.14.209-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.13.1304-bootstrap
   "0.14.209"
   "0ilk79b74qdywls9hlc5yllwnvfx32bwiw573m5ykllrzd9lxwzl"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (delete 'fix-value-order)))))
;; TODO: Find the earliest version where it can be deleted
(define kotlin-0.14.398-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.14.209-bootstrap "0.14.398"
   "1w8nwjnfl9am1xljy2agwd9a8gzh200liap0pkbh5m42l5kb4129"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))
(define kotlin-0.15.8-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.14.398-bootstrap "0.15.8"
   "0ad7xq4c9qdj41nh180njyi5g8qf4cc8yglbi7v3gi6pg8aaxiqa"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch")))

(define kotlin-0.15.394-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.15.8-bootstrap
   "0.15.394"
   "1cra0acb52zq92r56svx2w8v62jhqm1xj3dy0lqzp1nxrbynnhn4"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-0.15.394-remove-rmi-k2js.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-after 'prepare-idea-libs 'prepare-idea-libs-native-platform
                       (lambda _
                         (symlink (string-append ,java-native-platform
                                   "/share/java/native-platform.jar")
                                  "dependencies/native-platform-uberjar.jar")))))))
(define kotlin-0.15.541-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.15.394-bootstrap "0.15.541"
   "0mpmq0x8zhfalhl994d95s710yskj071zvw0i72269f1a6h8w5va"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-0.15.394-remove-rmi-k2js.patch")))
(define kotlin-0.15.604-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.15.541-bootstrap "0.15.604"
   "15k8gf3rw82z3h7n88r72pdwma3d4zccbzzssc2255y9yszinqdh"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-0.15.394-remove-rmi-k2js.patch")))
(define kotlin-0.15.723-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.15.604-bootstrap "0.15.723"
   "103nav8fgmcq398sgkyf01nhbspiwa3q39m4j1hsgm183czw70b6"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-0.15.394-remove-rmi-k2js.patch")))
(define kotlin-1.0.0-beta-2055-bootstrap
  (package-by-inheriting-kotlin-package kotlin-0.15.723-bootstrap
   "1.0.0-beta-2055" "1ls5zv0wfqjl6qvd73rhxn4jmcni6w40kivmnqnz8awy53gis1gb"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-0.15.394-remove-rmi-k2js.patch")))
(define kotlin-1.0.0-beta-3070-bootstrap
  (package-by-inheriting-kotlin-package kotlin-1.0.0-beta-2055-bootstrap
   "1.0.0-beta-3070" "03vy6wdn6gvslzlsfb956fsp4k1i1jnmb6b31pqkiwbvhnvnsxfz"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-0.15.394-remove-rmi-k2js.patch")))
(define kotlin-1.0.0-beta-4091-bootstrap
  (package-by-inheriting-kotlin-package kotlin-1.0.0-beta-3070-bootstrap
   "1.0.0-beta-4091" "0zjfa52mhihv10j75yrk1qhx4w0lnajg4pjpzyvq8blc09p60wr5"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-1.0.0-beta-4091-remove-k2js.patch")))

(define kotlin-1.0.0-beta-5010-bootstrap
  (package-by-inheriting-kotlin-package kotlin-1.0.0-beta-4091-bootstrap
   "1.0.0-beta-5010"
   "1sm5jdkrpq3vkqhhj6cfyipzc0ibjhlcnks1kf3s1k53ijadsnn8"
   '("patches/kotlin-0.10.300-remove-ant-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-1.0.0-beta-4091-remove-k2js.patch")
   #:set-phases (lambda (inherited-phases)
                  `(modify-phases ,inherited-phases
                     (add-after 'prepare-idea-libs 'prepare-idea-libs-junit
                       (lambda _
                         (symlink (string-append ,java-junit
                                                 "/lib/m2/junit/junit/"
                                                 ,(package-version java-junit)
                                                 "/junit-"
                                                 ,(package-version java-junit)
                                                 ".jar")
                                  "libraries/lib/junit-4.11.jar")))))))

(define kotlin-1.0.0-beta-5604-bootstrap
  (package-by-inheriting-kotlin-package kotlin-1.0.0-beta-5010-bootstrap
   "1.0.0-beta-5604" "0i1cnwfw2y34pwv96fvcdrdv29p1q5yf1sby98cyga434jxakjjn"
   '("patches/kotlin-1.0.0-beta-5604-remove-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-1.0.0-beta-4091-remove-k2js.patch")))
(define kotlin-1.0.0-dev-162-bootstrap
  (package-by-inheriting-kotlin-package kotlin-1.0.0-beta-5604-bootstrap
   "1.0.0-dev-162" "1nhy6rpx4ksymimpl4dhc2dxr7fn2hwib56ddqrgczjaym29svyv"
   '("patches/kotlin-1.0.0-beta-5604-remove-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-1.0.0-beta-4091-remove-k2js.patch")))
(define kotlin-1.0.0-bootstrap
  (package-by-inheriting-kotlin-package kotlin-1.0.0-dev-162-bootstrap
   "1.0.0" "0api7v7z38scg588w6vn6a2xq625rq1a8p5k9jq9xqh4vv72xpz4"
   '("patches/kotlin-1.0.0-beta-5604-remove-k2js.patch"
     "patches/kotlin-0.10.823-asm.patch"
     "patches/kotlin-0.10.1426-pack-jar.patch"
     "patches/kotlin-0.11.873-javac1.patch"
     "patches/kotlin-1.0.0-beta-4091-remove-k2js.patch")))
;    #:set-native-inputs (lambda (inherited-inputs)
;                          (modify-inputs inherited-inputs
;                            (replace "java-protobuf-api" java-protobuf-api-2-kotlin)))
;    #:set-phases (lambda (inherited-phases)
;                   `(modify-phases ,inherited-phases
;                      (delete 'prepare-protobuf-lib)
;                      (replace 'prepare-protobuf-api ;; TODO: merge with 'prepare-protobuf-lib
;                        (lambda* (#:key inputs #:allow-other-keys)
;                          ;; build.xml expects exact file names in dependencies directory
;                          (mkdir-p "dependencies")
;                          (for-each
;                            (lambda (p)
;                              (symlink (string-append (assoc-ref inputs
;                                                        "java-protobuf-api")
;                                         "/share/java/protobuf.jar")
;                                p))
;                            (list
;                              "dependencies/protobuf-2.6.1.jar"
;                              "dependencies/protobuf-2.6.1-lite.jar"))))))
; (define kotlin-1.1.0-dev-3777-bootstrap
;  (package-by-inheriting-kotlin-package kotlin-1.0.0-bootstrap
;    "1.1.0-dev-3777" "1x15x01m6ff5kqspxyw6h90n3yqbhzwja7v20camjwa9jjay6hs0"
;    '("patches/kotlin-0.10.300-remove-ant-k2js.patch" "patches/kotlin-0.10.1426-pack-jar.patch"
;       "patches/kotlin-0.11.873-javac1.patch")))
;(define kotlin-1.1.0-bootstrap
;  (package-by-inheriting-kotlin-release-package kotlin-1.0.0-bootstrap
;    "1.1" "1wzirri15kmn5g6gnczqjn5pz69pfq7kzibbpfaacbxvjqw2l2pn"
;    '("patches/kotlin-0.10.300-remove-ant-k2js.patch" "patches/kotlin-0.10.1426-pack-jar.patch"
;       "patches/kotlin-0.11.873-javac1.patch")))

kotlin-1.0.0-bootstrap
