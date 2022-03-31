(declare (unit packaging))
(declare (uses util))

(import (chicken file)
	(chicken io)
	(chicken string))
(import srfi-1 srfi-13 srfi-14 srfi-69)
(import matchable)
(import shell)

(define-record pkg
  name version arch build)

(define *pkglist* (make-hash-table equal?))

(define (load-pkglist)
  ;; in case this gets called to reset the package list
  (set! *pkglist* (make-hash-table equal?))
  (print "# Loading installed package list")
  (for-each (lambda (listing)
	      ;; package listing has form pkg-name-...-$version-$arch-$build
              (let* ((tokens (string-split listing "-"))
		     (name (string-join (drop-right tokens 3) "-")))
		
                (match-let (((version arch build) (take-right tokens 3)))
		  (hash-table-set! *pkglist* name
				   (make-pkg name version arch build)))))
	    
	    (directory "/var/log/packages")))

;; version should be like 1.20.300 ...
(define (dotnum-version? str)
  (and (string-any #\. str)
       (not (string-any char-set:letter str))))

(define (compare-version-string ver1 ver2)
  (let ((ver1 (string-join (string-split ver1 "_") "."))
	(ver2 (string-join (string-split ver2 "_") ".")))
    
    (if (every dotnum-version? (list ver1 ver2))
	(let loop ((tokens1 (string-split ver1 "."))
                   (tokens2 (string-split ver2 ".")))
          
          ;; If ver2 has fewer tokens that all match ver1's, then ver1 must be >=
          ;; If ver1 has fewer tokens that all match, then ver1 must be <
          ;; otherwise for each token, if the tokens aren't equal, then the version with
          ;; the higher token has the higher version.
	  ;;(display tokens2) (newline)
          (cond [(null? tokens2) #t]
		[(and (null? tokens1)
                      (not (null? tokens2))) #f]
		[(> (string->number (car tokens1))
                    (string->number (car tokens2))) #t]
		[(< (string->number (car tokens1))
                    (string->number (car tokens2))) #f]
		[else (loop (cdr tokens1) (cdr tokens2))]))
	
	(string>= ver1 ver2))))

(define (installed-pkgs)
  (hash-table-values *pkglist*))

;; used when installing multiple packages in one go.
;; rather than reloading *pkglist* entirely, just add an entry
;; which indicates it was installed.
(define (pkg-add name)
  (hash-table-set! *pkglist* name (make-pkg name "" "" "SBo")))

(define (pkg-installed? name)
  (hash-table-exists? *pkglist* name))

(define (pkg-up-to-date? pkg version)
  (compare-version-string (pkg-version pkg) version))

(define (pkg-is-sb? pkg)
  (string-contains (pkg-build pkg) "SBo"))

(define (install-pkg path)
  (run (installpkg ,path)))

(define (upgrade-pkg path)
  (run (upgradepkg ,path)))
