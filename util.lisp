;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:winhttp)

(defmacro with-http ((var) &body body)
  `(let ((,var (http-open)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defmacro with-connect ((var http hostname port) &body body)
  `(let ((,var (http-connect ,http ,hostname ,port)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defmacro with-request ((var hconn &key verb url https-p) &body body)
  `(let ((,var (http-open-request ,hconn
				  :verb ,verb
				  :url ,url
				  :https-p ,https-p)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))
		     
(defun get-content-length (headers)
  (dolist (h headers)
    (destructuring-bind (hname hval) h
      (when (string-equal hname "Content-Length")
	(return-from get-content-length (parse-integer hval)))))
  nil)

(defun http-request (url &key (method :get) 
			   post-data (post-start 0) post-end 
			   rawp headers timeout ignore-certificates-p)
  "Send HTTP request to server. 
URL ::= string in format [http|https://][username:password@]hostname[:port][/url]
METHOD ::= HTTP verb
POST-DATA ::= if specified, is an octet vector sent as post data. Uses region bounded 
by POST-START and POST-DATA.
RAWP ::= if true returns octets otherwise return data is parsed as text.
HEADERS ::= list of (header &optional value)* extra headers to add.
TIMEOUT ::= milliseconds to wait for connection and receive.
IGNORE-CERTIFICATES-P ::= if true will set option flags to ignore certificate errors.

Returns values return-data status-code headers content-length.
"

  (let ((comp (crack-url url)))
    (with-http (hsession)
      (with-connect (hconn hsession (getf comp :hostname) (getf comp :port))
	(with-request (hreq hconn
			    :verb method
			    :url (getf comp :url)
			    :https-p (eq (getf comp :scheme) :https))
	  (let ((user (getf comp :username))
		(pass (getf comp :password)))
	    (when (and user pass)
	      (set-credentials hreq user pass)))
	  (dolist (h headers)
	    (add-request-headers hreq (format nil "~A: ~A"
					      (first h)
					      (or (second h) ""))))
	  (when (and (eq (getf comp :scheme) :https)
		     ignore-certificates-p)
	    (set-ignore-certificates hreq))
	  (when timeout (set-timeouts hreq :connect timeout :recv timeout))
	  (send-request hreq 
			(if (stringp post-data)
			    (babel:string-to-octets post-data)
			    post-data)
			:start post-start :end post-end)
	  (receive-response hreq)
	  (let* ((headers (query-headers hreq))
		 (status (query-status-code hreq))
		 (resp (make-array (* 64 1024)
				   :element-type '(unsigned-byte 8))))
	    (values
	     (cond
	       (rawp
		(flexi-streams:with-output-to-sequence (s)
		  (do ((done nil))
		      (done)
		    (let ((n (read-data hreq resp)))
		      (if (zerop n)
			  (setf done t)
			  (write-sequence resp s :end n))))))
	       (t
		(with-output-to-string (s)
		  (do ((done nil))
		      (done)
		    (let ((n (read-data hreq resp)))
		      (if (zerop n)
			  (setf done t)
			  (format s "~A"
				  (babel:octets-to-string resp
							  :end n
							  :errorp nil))))))))
	     status
	     headers
	     (get-content-length headers))))))))
