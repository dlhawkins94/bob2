(declare (uses packaging))
(declare (uses sb-info))
(declare (uses slackbuilds))

(import (chicken format)
	(chicken io)
	(chicken process-context))
(import srfi-1 srfi-13 srfi-69)
(import http-client)

;; I don't know what the general expectations for these rules are, but:
;; flags (options w/o args) are preceded by one hyphen.
;; proper options (w/ args) are preceded by two.
;; This way, we can process options & flags before the per-command logic.

(define (flag? arg)
  (and (<= 2 (string-length arg))
       (eq? #\- (string-ref arg 0))
       (not (eq? #\- (string-ref arg 1)))))

(define (option? arg)
  (and (<= 3 (string-length arg))
       (equal? "--" (string-take arg 2))
       (not (eq? #\- (string-ref arg 2)))))

(define (sort-args)
  (when (< (length (argv)) 2)
    (error "Need at least one arg. See bob help."))
  
  (let* ((prog (first (argv)))
	 (cmd (second (argv)))
	 (args '())
	 (flags '())
	 (opts '()))
    
    (let loop ((fields (drop (argv) 2)))
      (cond [(null? fields) '()]
	    
	    [(flag? (car fields))
	     (set! flags
	       (cons (string-drop (car fields) 1)
		     flags))
	     (loop (cdr fields))]

	    [(option? (car fields))
	     (cond [(null? (cdr fields))
		    (error "option parameter missing")]
		   [(or (flag? (cadr fields))
			(option? (cadr fields)))
		    (error "invalid option parameter")])

	     (set! opts
	       (cons (cons (string-drop (car fields) 2)
			   (cadr fields))
		     opts))
	     (loop (cddr fields))]

	    [else
	     (set! args (cons (car fields) args))
	     (loop (cdr fields))]))

    (values prog cmd args flags opts)))

;; main program entry

(max-idle-connections 0)
(client-software '(("lib-curl-agent" 1.0 #f))) ;; some people are just mean
(load-pkglist)

(receive (prog cmd args flags opts) (sort-args)
  (unless (equal? "update" cmd)
    (load-sb-list-from-file))

  (case (string->symbol cmd)
    [(update)
     (let ((update? (get-changelog)))
       (if (or update? (member "force" flags))
	   (begin
	     (unless update?
	       (format #t "# [WRN] Update not needed, but forced~%"))

	     (get-sblist)
	     (write-sb-list-to-file))

	   (load-sb-list-from-file))
       
       (unless (null? (new-sbs))
	 (format #t "# New slackbuilds available:~%")
	 (for-each (lambda (sb)
		     (format #t "~a ~a~%" (sb-name sb) (sb-version sb)))
		   (new-sbs))))]
    
    [(upgrade)
     (for-each upgrade-sb
	       (map (lambda (pkgname)
		      (hash-table-ref *sblist* pkgname))
		    args))]
    
    [(upgrade-all)
     (for-each upgrade-sb (new-sbs))]

    [(fetch)
     (get-sb (hash-table-ref *sblist* (car args)))]

    [(build)
     (build-sb (hash-table-ref *sblist* (car args)))]
    
    [(install)
     (for-each (lambda (name)
		 (install-sb (hash-table-ref *sblist* name)))
	       args)]
    
    [(unneeded-libraries)
     (for-each (lambda (sb)
		 (format #t "~a --- ~a~%" (sb-name sb) (sb-short-description sb)))
	       (unneeded-libraries))]

    [(purge-sources)
     (purge-sources)]
    
    [(info)
     (if (< 1 (length args))
	 (error "Too many args.")
	 (let ((sb (hash-table-ref *sblist* (car args))))
	   (for-each (lambda (field)
		       (if (and (list? (cdr field))
				(< 1 (length (cdr field))))

			   (begin (format #t "~a ::: ~%"
					  (string-upcase (symbol->string (car field))))
				  
				  (for-each (lambda (val)
					      (format #t "    ~a~%" val))
					    (cdr field)))

			   (begin (format #t "~a ::: ~a~%"
					  (string-upcase (symbol->string (car field)))
					  (cdr field)))))
		     (encode-sb sb))

	   (newline)
	   (print-slack-desc sb)))]
    
    [(readme)
     (if (< 1 (length args))
	 (error "Too many args.")
	 (let ((sb (hash-table-ref *sblist* (car args))))
	   (print (with-input-from-request
		   (cat-str +repo-url+ "/" (sb-location sb) "/README")
		   #f read-string))))]
    
    [(search)
     (for-each (lambda (sb)
		 (format #t "~a~a --- ~a~%" 
			 (if (pkg-installed? (sb-name sb))
			     "*** "
			     "")
			 (sb-name sb)
			 (sb-short-description sb)))
	       
	       (apply search-sbs args))]

    [(graph)
     (if (< 1 (length args))
	 (error "Too many args.")
	 (let* ((sb (hash-table-ref *sblist* (car args)))
		(g (if (member "full" flags)
		       (full-dependency-graph sb)
		       (find-dependencies sb))))
	   (print-dependency-graph g)))]))
