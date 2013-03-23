; -*- coding: utf-8;  -*-

(in-package :common-lisp)

(defpackage :sfw-server
  (:use :cl
	:hunchentoot
	:cl-who)
  ;(:export :start-resty :start-server)
  )

(in-package :sfw-server)


(setq *dispatch-table*
      (list (create-prefix-dispatcher "/" 'root)
	    (create-prefix-dispatcher "/testfw" 'test-sfw)
	    ; (create-regex-dispatcher "^/([a-z0-9-]+)\\.json" 'sfw-json)
	    'dispatch-easy-handlers))

(defvar *ht-server*
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defparameter *reki* nil)

;; imagelle tulee accept:*/*, joten tuossa tapauksessa voinee yrittää
;; tiedostopäätteen avulla tunnistaa, mitä lähettää ylös

(defun test-sfw ()
  (setq *reki* hunchentoot:*request*)
  (log-message* :WARNING "-- ~s :: ~s -."
		(script-name* *reki*) (headers-in* *reki*))
  (format nil
	  "<html><body><h1>hej ~a ~s!</h1><pre>~s</pre><img src=\"http://localhost:4242/test/kala.png\">kala</img></body></html>" 
	  (request-method*)
	  (raw-post-data :force-text t)
	  (headers-in* *reki*)))

(defun sfw ()
  (setq *reki* hunchentoot:*request*)
  (with-html-output (*standard-output* nil :prologue t :indent t)
    (:html (:body "joo-o!"))))

(defun is-json-req (req)
  (multiple-value-bind (whole name)
      (cl-ppcre:scan-to-strings "^/([a-z0-9-]+)\\.json" (request-uri req))
    (and whole (aref name 0))))

(define-easy-handler
    (sfw-jsonr :uri 'is-json-req :default-request-type :GET) (f b)
  (log-message* :WARNING "-- ~s ~s --" f b)
  (with-html-output (*standard-output* nil :prologue t :indent t)
    (:html (:body (:h1 "Pöö!")))))


(defun sfw-json ()
  (setq *reki* hunchentoot:*request*)
  (with-html-output (*standard-output* nil :prologue t :indent t)
    (:html (:body "json!"))))


(defun sf ())
(defun root ()
  (with-html-output (*standard-output* nil :prologue t :indent t)
    (:html (:body (:h1 "Pöö!")))))
