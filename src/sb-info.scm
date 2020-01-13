(declare (unit sb-info))
(declare (uses slackbuilds))
(declare (uses util))

(import (chicken format)
	(chicken io)
	(chicken sort))
(import srfi-1 srfi-13 srfi-69)
(import http-client)

;; For some reason there is a long pause after this finishes printing.
;; Maybe it's the EOF detection?
(define (print-slack-desc sb)
  (let ((marker (string-append (sb-name sb) ":")))
    (with-input-from-request
     (cat-str +repo-url+ "/" (sb-location sb) "/slack-desc")
     #f
     (lambda ()
       (let loop ((line (read-line)))
	 (unless (eof-object? line)
	   (when (and (<= (string-length marker)
			  (string-length line))
		      (string= marker (string-take line (string-length marker))))
	     (print (string-drop line (string-length marker))))
	   (loop (read-line))))))))

(define (search-sbs #!rest terms)
  (sort
   (apply lset-intersection
	  (cons (lambda (sb1 sb2)
		  (equal? (sb-name sb1) (sb-name sb2)))
		
		(map (lambda (term)
		       (filter identity
			       (hash-table-map *sblist*
					       (lambda (name sb)
						 (and (string-contains name term) sb)))))
		     terms)))
   
   (lambda (sb1 sb2)
     (string<= (sb-name sb1) (sb-name sb2)))))

;; Find all packages which depend on sb-dep
;; To deal with the circular dep problem, we use visited to keep
;; track of which packages we've already checked
(define (find-dependents sb-dep #!optional (g (make-hash-table)) (visited '()))
  (let ((visited (cons (sb-name sb-dep) visited)))
    (hash-table-for-each *sblist*
			 (lambda (name sb)
			   (when (member (sb-name sb-dep) (sb-requires sb))
			     (unless (member name visited)
			       (find-dependents sb g visited))
			       
			     (hash-table-update!/default
			      g name
			      (lambda (edges)
				(lset-union equal? (list (sb-name sb-dep)) edges))
			      '()))))
    g))

;; Find all packages which sb depends on
(define (find-dependencies sb #!optional (g (make-hash-table)) (visited '()))
  (let ((visited (cons (sb-name sb) visited)))
    (for-each (lambda (name)
		(when (and (not (member name visited))
			   (hash-table-exists? *sblist* name))
		  (find-dependencies (hash-table-ref *sblist* name) g visited))
		  
		(hash-table-update!/default
		 g (sb-name sb)
		 (lambda (edges)
		   (lset-union equal? (list name) edges))
		 '()))
	      (sb-requires sb))
      
    g))
  

;; Find entire dependency tree for this sb: all packages that eventually require
;; it, and all packages that it needs.
;; the output is a directed graph as a hash table of sb-name : adjacency list
(define (full-dependency-graph sb)
  (let ((g (make-hash-table)))
    (find-dependents sb g)
    (find-dependencies sb g)))

(define (graphviz-sanitize name)
  (list->string
   (let loop ((chs (string->list name)))
     (cond [(null? chs) '()]
	   [(eq? #\% (car chs))
	    (append '(#\~ #\%)
		    (loop (cdr chs)))]
	   [else (cons (car chs)
		       (loop (cdr chs)))]))))

(define *digraph-node-attrs*
  `((shape . "box")
    (style . "filled")
    (fillcolor . "#009999")
    (fontcolor . "#ffffff")
    (ranksep . "20 equally")
    (nodesep . "0")))

(define (print-digraph-attrs attrs)
  (format #t " [")
  (let loop ((attrs attrs))
    (format #t "\"~a\" = \"~a\"" (caar attrs) (cdar attrs))
    (unless (null? (cdr attrs))
      (format #t ", ")
      (loop (cdr attrs))))
  (format #t "]"))

(define (print-digraph-node name #!optional attrs)
  (format #t "    \"~a\"" name)
  (when attrs (print-digraph-attrs attrs))
  (format #t ";~%"))

(define (print-dependency-graph g)
  (format #t "digraph dependencies {~%")
  (format #t "    node ")
  (print-digraph-attrs (*digraph-node-attrs*))
  (format #t ";~%")
 
  (let ((visited (make-parameter '())))
    (hash-table-for-each
     g (lambda (name adj-list)
	 (let ((name-gv (graphviz-sanitize name))
	       (sb (hash-table-ref/default *sblist* name #f)))

	   (unless (member name (visited))
	     (visited (cons name (visited)))
	     (print-digraph-node name-gv
				 (cond [(not sb)
					'((fillcolor . "#999900"))]
				       [(not (pkg-installed? name))
					'((fillcolor . "#990000"))]
				       [else #f])))
	   
	   (for-each (lambda (adj)
		       (let ((adj-gv (graphviz-sanitize adj))
			     (sb (hash-table-ref/default *sblist* adj #f)))

			 (unless (member adj (visited))
			   (visited (cons adj (visited)))
			   (print-digraph-node adj-gv
					       (cond [(not sb)
						      '((fillcolor . "#999900"))]
						     [(not (pkg-installed? adj))
						      '((fillcolor . "#990000"))]
						     [else #f])))
			 
			 (format #t "    \"~a\" -> \"~a\";~%" name-gv adj-gv)))
		     adj-list)))))
  
  (format #t "}~%"))



