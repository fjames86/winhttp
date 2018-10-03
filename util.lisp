;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:winhttp)

(defmacro with-http ((var &optional user-agent) &body body)
  "Evaluate body with VAR bound to a session handle."
  `(let ((,var (http-open ,user-agent)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defmacro with-connect ((var http hostname port) &body body)
  "Evaluate body with VAR bound to a connection handle."
  `(let ((,var (http-connect ,http ,hostname ,port)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defmacro with-request ((var hconn &key verb url https-p) &body body)
  "Evaludate body with VAR bound to a request handle."
  `(let ((,var (http-open-request ,hconn
				  :verb ,verb
				  :url ,url
				  :https-p ,https-p)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))
		     
(defun query-content-length (headers)
  "Returns content length as specified in header."
  (dolist (h headers)
    (destructuring-bind (hname hval) h
      (when (string-equal hname "Content-Length")
	(return-from query-content-length (parse-integer hval)))))
  nil)

(defparameter *status-cb-types*
  '((:RESOLVING-NAME          #x00000001)
    (:NAME-RESOLVED           #x00000002)
    (:CONNECTING-TO-SERVER    #x00000004)
    (:CONNECTED-TO-SERVER     #x00000008)
    (:SENDING-REQUEST         #x00000010)
    (:REQUEST-SENT            #x00000020)
    (:RECEIVING-RESPONSE      #x00000040)
    (:RESPONSE-RECEIVED       #x00000080)
    (:CLOSING-CONNECTION      #x00000100)
    (:CONNECTION-CLOSED       #x00000200)
    (:HANDLE-CREATED          #x00000400)
    (:HANDLE-CLOSING          #x00000800)
    (:DETECTING-PROXY         #x00001000)
    (:REDIRECT                #x00004000)
    (:INTERMEDIATE-RESPONSE   #x00008000)
    (:SECURE-FAILURE          #x00010000)
    (:HEADERS-AVAILABLE       #x00020000)
    (:DATA-AVAILABLE          #x00040000)
    (:READ-COMPLETE           #x00080000)
    (:WRITE-COMPLETE          #x00100000)
    (:REQUEST-ERROR           #x00200000)
    (:SENDREQUEST-COMPLETE    #x00400000)
    (:GETPROXYFORURL-COMPLETE #x01000000)
    (:CLOSE-COMPLETE          #x02000000)
    (:SHUTDOWN-COMPLETE       #x04000000)))

(defmacro define-status-callback (name (hinternet context status infop infolen) &body body)
  "Define a foreign callback function which can be used to receive http request status updates.
See MSDN for information on WinHttpSetStatusCallback function. Use SET-STATUS-CALLBACK to register 
this with a request handle.

HINTERNET ::= handle 
CONTEXT ::= pointer to DWORD context 
STATUS ::= symbol naming status update type 
INFOP ::= pointer to info buffer
INFOLEN ::= length of info buffer
"
  (let ((gstatus (gensym)))
    `(defcallback ,name :void
         ((,hinternet :pointer)
          (,context :pointer)
          (,gstatus :uint32)
          (,infop :pointer)
          (,infolen :uint32))
       (let ((,status (first (find ,gstatus *status-cb-types* :key #'second))))
         ,@body))))

(defun http-request (url &key (method :get) 
			   post-data (post-start 0) post-end 
                           rawp headers timeout ignore-certificates-p
                           statuscb)
  "Send HTTP request to server. 
URL ::= string in format [http|https://][username:password@]hostname[:port][/url]
METHOD ::= HTTP verb
POST-DATA ::= if specified, is an octet vector sent as post data. Uses region bounded 
by POST-START and POST-DATA.
RAWP ::= if true returns octets otherwise return data is parsed as text.
HEADERS ::= list of (header &optional value)* extra headers to add.
TIMEOUT ::= milliseconds to wait for connection and receive.
IGNORE-CERTIFICATES-P ::= if true will set option flags to ignore certificate errors.
<<<<<<< HEAD
STATUSCB ::= if non-nil, is a symbol naming a callback defined using define-status-callback.
This will be invoked to inform various status messages. 
=======
RECV-BUF ::= if provided, is an octet vector that receives the reply body. 
If not supplied a buffer is allocated. 

>>>>>>> f59464e41708630847a9a037c34eb1c27801a8de
Returns values return-data status-code headers content-length.
"

  (let ((comp (crack-url url)))
    (with-http (hsession)
      (when (eq (getf comp :scheme) :https)
        (set-secure-protocols hsession :tls1 t :tls1-1 t :tls1-2 t))
      (with-connect (hconn hsession (getf comp :hostname) (getf comp :port))
	(with-request (hreq hconn
			    :verb method
			    :url (getf comp :url)
			    :https-p (eq (getf comp :scheme) :https))
      
      (when statuscb
        (set-status-callback hreq (get-callback statuscb)))
      
	  (let ((user (getf comp :username))
		(pass (getf comp :password)))
	    (when (and user pass)
	      (set-credentials hreq user pass)))
      
	  (dolist (h headers)
	    (add-request-headers hreq (format nil "~A: ~A"
					      (first h)
					      (or (second h) ""))))
      
	  (when (eq (getf comp :scheme) :https)
        (when ignore-certificates-p
          (set-ignore-certificates hreq)))
      
	  (when timeout (set-timeouts hreq :connect timeout :recv timeout))
      
	  (send-request hreq 
			(if (stringp post-data)
			    (babel:string-to-octets post-data)
			    post-data)
			:start post-start :end post-end)
	  (receive-response hreq)
	  (let* ((headers (query-headers hreq))
		 (status (query-status-code hreq))
		 (len (query-content-length headers))
		 (resp (or recv-buf
			   (make-array len :element-type '(unsigned-byte 8)))))
	    (do ((done nil)
		 (offset 0))
		(done)
	      (let ((n (read-data hreq resp :start offset)))
		(when (zerop n)
		  (setf done t))))
	    (values
	     (if rawp
		 resp
		 (babel:octets-to-string resp :end len))
	     status
	     headers
	     len))))))) 
