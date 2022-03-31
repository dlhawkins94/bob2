(declare (unit util))

(import (chicken string))
(import srfi-1 srfi-13)
(import shell)

(define +cache-root+ "/var/cache/bob")
(define +data-root+ "/var/lib/bob")
(define +repo-url+ "https://slackbuilds.org/slackbuilds/15.0")

(define (cat-str #!rest strs)
  (string-concatenate strs))

(define (s-dashed-words str)
  (string-map (lambda (ch)
		(if (or (equal? #\_ ch)
			(equal? #\space ch))
		    #\- ch))
	      str))

(define (take-file-name str)
  (last (string-split str "/")))

(define (drop-file-name str)
  (string-join
   (cons "" (drop-right (string-split str "/") 1))
   "/"))

(define (get-md5sum path)
  (first (string-split (capture (md5sum ,path)) " ")))

(define (test-md5sum md5 path)
  (string= md5 (get-md5sum path)))
