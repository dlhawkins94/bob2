(declare (unit slackbuilds))
(declare (uses packaging))
(declare (uses util))

(include "macros.scm")

(import (chicken condition)
	(chicken file)
	(chicken format)
	(chicken io)
	(chicken process)
	(chicken process-context)
	(chicken sort)
        (chicken string))

(import srfi-1 srfi-13 srfi-14 srfi-69)
(import http-client)
(import medea)
(import regex)
(import shell)
(import vector-lib)

;; slackbuild record definitions

(define-record sb
  name location files version 
  download download-x86-64 md5sum md5sum-x86-64
  requires short-description)

(define (encode-sb sb)
  `((name . ,(sb-name sb))
    (location . ,(sb-location sb))
    (files . ,(sb-files sb))
    (version . ,(sb-version sb))
    (download . ,(sb-download sb))
    (download-x86-64 . ,(sb-download-x86-64 sb))
    (md5sum . ,(sb-md5sum sb))
    (md5sum-x86-64 . ,(sb-md5sum-x86-64 sb))
    (requires . ,(sb-requires sb))
    (short-description . ,(sb-short-description sb))))

(define (decode-sb data)
  (make-sb (cdr (assoc 'name data))
           (cdr (assoc 'location data))
           (cdr (assoc 'files data))
           (cdr (assoc 'version data))
           (cdr (assoc 'download data))
           (cdr (assoc 'download-x86-64 data))
           (cdr (assoc 'md5sum data))
           (cdr (assoc 'md5sum-x86-64 data))
           (cdr (assoc 'requires data))
           (cdr (assoc 'short-description data))))

;; hash table of slackbuild records, keyed by package name. comes from SLACKBUILDS.TXT
;; stored in lispy data format w/ read, write
(define *sblist* (make-hash-table equal?))

(define (write-sb-list-to-file)
  (with-output-to-file (cat-str +data-root+ "/slackbuilds.json")
    (lambda ()
      (hash-table-for-each *sblist*
                           (lambda (name sb)
                             (write (cons name (encode-sb sb))))))))

(define (load-sb-list-from-file)
  (with-input-from-file (cat-str +data-root+ "/slackbuilds.json")
    (lambda ()
      (let loop ((data (read)))
        (unless (eof-object? data)
          (hash-table-set! *sblist* (car data) (decode-sb (cdr data)))
          (loop (read)))))))

(define-once blacklist
  (with-input-from-file "/etc/bob/blacklist.txt"
    (lambda ()
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '()
            (cons line
                  (loop (read-line))))))))

(define-once forced-libs
  (with-input-from-file "/etc/bob/forced_libs.txt"
    (lambda ()
      (let loop ((line (read-line)))
	(if (eof-object? line)
	    '()
	    (cons line
		  (loop (read-line))))))))

(define-once sbopts
  (with-input-from-file "/etc/bob/sbopts.json"
    (lambda ()
      (alist->hash-table (read-json) equal?))))

;; gets the configuration settings from sbopts.
;; there's a default configuration; if specific configuration settings are specificed,
;; the default conf is updated with them.
;; the final conf is stored in sbopt-cache so this update doesn't happen more than once.
(define *sbopt-cache* (make-hash-table equal?))
(define (get-sb-opts sb)
  (or (hash-table-ref/default *sbopt-cache* (sb-name sb) #f)
      (let ((opts (alist->hash-table (hash-table-ref (sbopts) 'DEFAULT)))
	    (opts-spec (hash-table-ref/default (sbopts) (string->symbol (sb-name sb)) #f)))
	(when opts-spec
	  (for-each (lambda (x)
		      (hash-table-update! opts (car x) (lambda (old) (cdr x))))
		    opts-spec))
	(hash-table-set! *sbopt-cache* (string->symbol (sb-name sb)) (hash-table->alist opts))
	(hash-table-ref *sbopt-cache* (string->symbol (sb-name sb))))))

;; returns true if there is a new changelog, meaning the slackbuild list is out of date
(define (get-changelog)
  (let* ((path (cat-str +data-root+ "/ChangeLog.txt"))
	 (uri (cat-str +repo-url+ "/ChangeLog.txt"))
	 (curr-md5 (get-md5sum path)))

    (run (wget ,uri -O ,path))
    (not (test-md5sum curr-md5 path))))

;; fetches the list of slackbuilds from the server
(define (get-sblist)
  (print "# Getting latest slackbuild list from the server")
  (let* ((data (with-input-from-request (cat-str +repo-url+ "/SLACKBUILDS.TXT") #f read-string))
	 (sb (make-sb "" "" '() "" '() '() '() '() '() "")))
    
    (for-each (lambda (line)
		(if (string-null? line)
		    (begin (hash-table-set! *sblist* (sb-name sb) sb)
			   (set! sb (make-sb "" "" '() "" '() '() '() '() '() "")))

		    (when (string-index (string-drop line 11) #\:)
		      (let* ((line (string-drop line 11))
			     (spl-pos (string-index line #\:))
			     (prop (string->symbol
				    (s-dashed-words
				     (string-downcase (string-take line spl-pos)))))
			     (val (string-drop line (+ spl-pos 2))))
			
			(case prop
			  [(name) (sb-name-set! sb val)]
			  [(location) (sb-location-set! sb val)]
			  [(files) (sb-files-set! sb (string-split val))]
			  [(version) (sb-version-set! sb val)]
			  [(download) (sb-download-set! sb (string-split val))]
			  [(download-x86-64) (sb-download-x86-64-set! sb (string-split val))]
			  [(md5sum) (sb-md5sum-set! sb (string-split val))]
			  [(md5sum-x86-64) (sb-md5sum-x86-64-set! sb (string-split val))]
			  [(requires) (sb-requires-set! sb (string-split val))]
			  [(short-description) (sb-short-description-set! sb val)])))))
	      
	      (string-split data "\n" #t))))

;;; Lists of SBs with specific properties

;; returns the list of installed SB packages that may be upgraded
(define-once new-sbs
  (filter-map (lambda (pkg)
		(and (pkg-is-sb? pkg)
		     (hash-table-exists? *sblist* (pkg-name pkg))
		     (let ((sb (hash-table-ref *sblist* (pkg-name pkg))))
		       (and (not (pkg-up-to-date? pkg (sb-version sb)))
			    sb))))
	      
	      (installed-pkgs)))

(define-once installed-sbs
  (filter-map (lambda (pkg)
                (and (pkg-is-sb? pkg)
		     (hash-table-ref *sblist* (pkg-name pkg))))
              (installed-pkgs)))

;; all the sbs that are in category library.
(define-once installed-sb-libs
  (filter (lambda (sb)
            (equal? "libraries" (second (string-split (sb-location sb) "/"))))
          (installed-sbs)))

;; convenience fn
(define (lib-in? lib libs)
  (member (sb-name lib) (map sb-name libs)))

;; list of every library sb which is not a dependency of any installed package
(define-once unneeded-libraries
  (let* ((libs (installed-sb-libs))
         (deps (make-hash-table)))
    
    ;; for each library, get a list of the installed sbs that depend on it
    (for-each (lambda (sb)
                (for-each (lambda (name)
                            (when (member name (map sb-name libs))
                              (hash-table-update!/default deps name
							  (lambda (l) (cons sb l))
							  '())))
                          (sb-requires sb)))
              (installed-sbs))
    
    ;; single out the libraries who aren't depended on, or who are only depended
    ;; on by other libraries
    (let ((unneeded '()) (needed '()))
      (let loop ((lbs libs))
	(cond [(null? lbs) unneeded]
	      
	      ;; Already processed this lib, move on
	      [(or (lib-in? (car lbs) unneeded)
		   (lib-in? (car lbs) needed))
	       (loop (cdr lbs))]

	      ;; Forced libraries always considered needed
	      [(member (sb-name (car lbs)) (forced-libs))
	       (set! needed (cons (car lbs) needed))
	       (loop (cdr lbs))]
	      
	      ;; loop through this lib's dependents. note that any circular dependencies
	      ;; of libraries will cause this to infinitely loop
	      [else
	       (let check-deps ((dps (hash-table-ref/default deps (sb-name (car lbs)) '())))
		 (cond [(null? dps) ;; no needed deps, so this is unneeded.
			(set! unneeded (cons (car lbs) unneeded))
			(loop (cdr lbs))]
		       
                       ;; If dependent is a library we need to make sure it too is needed.
		       [(lib-in? (car dps) libs)
			;; already found that this dep is unneeded, move on
			(cond [(lib-in? (car dps) unneeded)
			       (check-deps (cdr dps))]
			      
			      ;; already found that this dep is needed, so this lib is needed too.
			      [(lib-in? (car dps) needed)
			       (set! needed (cons (car lbs) needed))
			       (loop (cdr lbs))]
			      
			      ;; haven't processed this dep yet -- do so now
			      [else (loop (cons (car dps) lbs))])]

		       ;; Lib is needed if it has any non-library dependents.
		       [else (set! needed (cons (car lbs) needed))
			     (loop (cdr lbs))]))])))))

(define (get-sources uris md5sums dir)
  ;; src log keeps track of all the sources we've downloaded for easy cleaning
  (let ((src-log (cat-str +data-root+ "/sources.txt")))
    (for-each (lambda (uri md5sum)
		(format #t "# Source: ~a~%" uri)
		(let* ((path (cat-str dir "/" (take-file-name uri)))
		       (src-ok (and (file-exists? path)
				    (test-md5sum md5sum path))))

		  (when (file-exists? path)
		    (if src-ok
			(format #t "# Source already downloaded.~%")
			(begin (format #t "# Source present but bad md5; will try to replace.~%")
			       (delete-file path))))

		  (unless src-ok
		    (run (wget ,uri -O ,path))
		    (run (echo ,path >> ,src-log)) ;; append src path to log
		    (unless (test-md5sum md5sum path)
		      (error `("# md5 mismatch" (,md5sum ,(get-md5sum path))))))))
	      
	      uris md5sums)))

;; retrieves the slackbuild files and package sources
(define (get-sb sb)
  (let* ((sb-uri (cat-str +repo-url+ "/" (sb-location sb) ".tar.gz"))
	 (dir (cat-str +cache-root+ "/" (sb-location sb)))
	 (tmp (cat-str dir ".tar.gz")))

    ;; retrieve & extract the slackbuild tarball
    (run (mkdir -p ,(drop-file-name tmp)))
    (run (wget ,sb-uri -O ,tmp))
    (run (tar xvzf ,tmp -C ,(drop-file-name tmp)))
    (delete-file tmp)
    (newline)

    ;; If there are sources which can't be downloaded automatically,
    ;; warn the user that they need to manually do this, provide links & wait for input.
    (let ((manual-sources (vector->list (cdr (assoc 'manual-sources (get-sb-opts sb))))))
      (when (< 0 (length manual-sources))
	(print "# These sources can't be downloaded automatically:")
	(for-each print manual-sources)
	(format #t "# Place the download sources in ~a then press enter.~%" dir)
	(read-line)))

    ;; If there are separate sources for 32- and 64-bit, pick the 64-bit one.
    ;; Obviously that won't work on 32 bit systems...
    (print "# Getting sources:")
    (get-sources (if (null? (sb-download-x86-64 sb))
		     (sb-download sb)
		     (sb-download-x86-64 sb))
		 
		 (if (null? (sb-md5sum-x86-64 sb))
		     (sb-md5sum sb)
		     (sb-md5sum-x86-64 sb))

		 dir)))

;; generates the env variable string that precedes the SlackBuild call
(define (generate-buildenv-string sb)
  (fold (lambda (envvar str)
	  (cat-str str (symbol->string (car envvar)) "=\"" (cdr envvar) "\" "))
	""
	(cdr (assoc 'buildenv (get-sb-opts sb)))))

;; returns path of built package file
(define (build-sb sb)
  (format #t "# Building ~a~%" (sb-name sb))
  (let ((cwd (current-directory))
	(sb-dir (cat-str +cache-root+ "/" (sb-location sb)))
	(script (cat-str (sb-name sb) ".SlackBuild"))
	(buildenv (generate-buildenv-string sb)))
    
    (change-directory sb-dir)
    (with-input-from-pipe (cat-str buildenv " sh " script)
      (lambda ()
	(let ((package-path #f))
	  (let loop ((line (read-line)))
	    (unless (eof-object? line)
	      (print line)
	      (let ((mtch (string-match "Slackware package (.*) created." line)))
		(when mtch (set! package-path (second mtch))))
	      (loop (read-line))))
	  
	  package-path)))))

;; fetch & build a slackbuild, returns path to built package
(define (prepare-sb sb)
  (let ((opts (get-sb-opts sb)))
    ;; install/upgrade all the dependencies
    (let ((ignore-deps (vector->list (cdr (assoc 'ignore-deps opts)))))
      (for-each (lambda (name)
		  (cond [(member name ignore-deps)
			 (format #t "Ignoring dep \"~a\"~%" name)]
			[(pkg-installed? name)
			 (upgrade-sb (hash-table-ref *sblist* name))]
			[else
			 (format #t "# Need to install ~a~%" name)
			 (install-sb (hash-table-ref *sblist* name))]))
		(sb-requires sb)))
    
    (get-sb sb)
    (let ((package-path (build-sb sb)))
      (when (not package-path)
	(signal
	 (make-property-condition 'exn 'message (sb-name sb))))
      package-path)))

(define (install-sb sb)
  (cond [(pkg-installed? (sb-name sb))
	 (format #t "# ~a already installed, skipping~%" (sb-name sb))]

	[(member (sb-name sb) (blacklist))
	 (format #f "# ~a blacklisted; not installing.~%" (sb-name sb))]

	[else
	 (let ((package-path (prepare-sb sb)))
	   (install-pkg package-path)
	   (delete-file package-path)
	   (pkg-add (sb-name sb)))]))

(define (upgrade-sb sb)
  (cond [(not (pkg-installed? (sb-name sb)))
         (error `("Package not installed" ,(sb-name sb)))]
	
	[(member (sb-name sb) (blacklist))
	 (format #t "# Package blacklisted, not upgrading: ~a~%" (sb-name sb))]
	
	[(compare-version-string (pkg-version (hash-table-ref *pkglist* (sb-name sb)))
                                 (sb-version sb))
         (format #t "# Package ~a is up to date.~%" (sb-name sb))]
	
	[else
         (let ((package-path (prepare-sb sb)))
           (upgrade-pkg package-path)
           (delete-file package-path))]))

;; Deletes all the source archives we've downloaded.
(define (purge-sources)
  (let ((src-log (cat-str +data-root+ "/sources.txt")))
    (if (not (file-exists? src-log))
	(format #t "# No downloaded sources~%")
	(begin
	  (with-input-from-file src-log
	    (lambda ()
	      (let loop ((line (read-line)))
		(unless (eof-object? line)
		  (format #t "# Deleting source file: ~a~%" line)
		  (delete-file line)
		  (loop (read-line))))))
	  (delete-file src-log)))))

