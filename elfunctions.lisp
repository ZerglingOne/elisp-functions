(defun index (aList anIndex)
           (cond ((> anIndex 0) (index (cdr aList) (- anIndex 1)))
                 ((< anIndex 0) NIL)
                 ((= anIndex 0) (car aList))))

(defvar currentversions nil)
(defvar servicemethods nil)
(defvar apimethods nil)
(defvar repomethods nil)
(defvar microservicename nil)
(defvar servicename nil)
(defvar reponame nil)
(defvar apiname nil)
(defvar artifactnames nil)
(setq servicemethods "")
(setq repomethods "")
(setq apimethods "")
(setq microservicename "")
(setq empty-char '‎)

(defun clearmethods ()
	(setq servicemethods "")
	(setq repomethods "")
)

(defun feign ()
	(kill-new "import org.springframework.cloud.openfeign.FeignClient;

")
	(kill-append "@FeignClient(value = \"" nil)
	(kill-append (read-string "value = ") nil)
	(kill-append "\", path = \"/api/" nil)
	(kill-append (read-string "path = ") nil)
    (kill-append "\")" nil)
	(alttab-and-paste)
)

(defun restcon ()
	(kill-new "import org.springframework.web.bind.annotation.RequestMapping;
")
	(kill-append "import org.springframework.web.bind.annotation.RestController;
" nil)
	(kill-append "
" nil)
	(kill-append "@RestController
" nil)
	(kill-append "@RequestMapping(value = \"/api/" nil) 
	(kill-append (read-string "value = ") nil)
	(kill-append "\")" nil)
	"Rest controller copied to clipboard"
	(alttab-and-paste)
)

(defun upcopy ()
	(kill-new (upcase (current-kill 0)))
	(alttab-and-paste)
)

(defun downcopy()
	(kill-new (downcase (current-kill 0)))
	(alttab-and-paste)
)

(defun camel ()
	(setq temp (read-string "write your camel: "))
	(kill-new 
		(concat 
			(car (split-string temp)) 
			(eval (append '(concat) (cdr (split-string (capitalize temp)))))
		)
	)
	(makunbound 'temp)
	"copied to clipboard"
)

(defun camelwp(string)
	(concat 
		(car (split-string string)) 
		(eval (append '(concat) (cdr (split-string (capitalize string)))))
	)
)

(defun pascal ()
	(kill-new
		(eval ( append '(concat) (split-string (capitalize (read-string "enter the pascalized string: ")))))
	)
)			
	
(defun kebab ()
	(kill-new
		(replace-regexp-in-string " " "-" (read-string "enter the kebabized string: "))
	)
)

(defun snake ()
	(kill-new
		(replace-regexp-in-string " " "_" (read-string "enter the slithery snake string: "))
	)
)

(defun dragon ()
	(kill-new
		(replace-regexp-in-string " " "_" (upcase (read-string "enter the dragonized string: ")))
	)
)

(defun dragonwp (string)
	(replace-regexp-in-string " " "_" (upcase string))
)

(defun query ()
	(kill-new
		(concat "SELECT * FROM GCDEV." 
			(dragonwp 
				(read-string "type table name spaces instead of underscores: ")
			)
			";"
		)
	)
)



(defun swagger ()
	(w32-shell-execute "open" 
	(concat "http://localhost:" (read-string "port number: ") "/swagger-ui.html"))	
)

(defun reload ()
	(load "elfunctions.lisp")
)

(defun turtle ()
	'(turtle)
)

(defun eval-string (string)
	(eval (car (read-from-string string)))
)

(defvar test)
(setq test 0)
(defun runfuncs ()
	(while (= test 0)
		(setq temp (read-string "function name: "))
		(cond
			((string= temp "runfuncs") (setq temp "turtle"))
			((not (string= temp "quit")) (ignore-errors (eval-string (concat "(" temp ")"))))
			((string= temp "quit") (setq test 1))
		)
	)
	(setq test 0)
	"done!"
)

(defun replace-copy ()
	(kill-new
		(replace-regexp-in-string 
			(read-string "replace: ") 
			(read-string "with: ")
			(current-kill 0)
		)
	)
	(alttab-and-paste)
)

(defun prepare-for-post ()
	(kill-new
		(replace-regexp-in-string
		"\(defun" "    \(defun" (current-kill 0)
		)
	)
	(kill-new
		(replace-regexp-in-string
		"\n\t*)\n" ")" (current-kill 0)
		)
	)
	(kill-new
		(replace-regexp-in-string
		 ")[\n\s\t]*)" "))" (current-kill 0)
		)
	)
	(kill-new
		(replace-regexp-in-string
		 "\t" "      " (current-kill 0)
		)
	)
	(kill-new
		(replace-regexp-in-string
		")\s+)" "))" (current-kill 0)
		)
	)
)

(defun nobody ()
	(kill-new
		(replace-regexp-in-string
		"\s*\n*\s*{\n[^\}]*\}" ";" (current-kill 0)
		)
	)
	(alttab-and-paste)
)

(defun make-web-api ()
	(setq apimethods "")
	(setq servicemethods "")
	(setq repomethods "")
	(setq servicename "")
	(setq reponame "")
	(setq microservicename "")
	(setq microservicename (read-string "micro-service name(e.g. \"<Object Name>\" -> I<Object Name>Repo <Object Name>Service etc.): "))
	(setq servicename (concat microservicename "Service"))
	(setq reponame (concat "I" microservicename "Repo"))
	(setq again "Y")
	(while (string= again "Y")
		(generate-web-methods)
		(setq again (upcase (read-string "add another method? Y/y to continue: ")))
	)
	(setq apimethods (concat "\t@Autowired\n\tprivate " servicename " " (downcase microservicename) "Service;\n" apimethods))
	(setq servicemethods ( concat "\t@Autowired\n\tprivate " reponame " " (downcase microservicename) "Repo;\n" servicemethods))
	(read-string "press enter to put repo methods into clipboard")
	(getrepomethods)
	(alttab-and-paste)
	(read-string "press enter to put service methods into clipboard")
	(getservicemethods)
	(alttab-and-paste)
	(read-string "press enter to put api methods into clipboard")
	(getapimethods)
	(alttab-and-paste)
) 

(defun make-db-api ()
	(setq apimethods "")
	(setq servicemethods "")
	(setq repomethods "")
	(setq servicename "")
	(setq reponame "")
	(setq microservicename "")
	(setq microservicename (read-string "micro-service name(e.g. \"<Object Name>\" -> I<Object Name>Repo <Object Name>Service etc.): "))
	(setq servicename (concat microservicename "Service"))
	(setq reponame (concat "I" microservicename "Repo"))
	(setq again "Y")
	(while (string= again "Y")
		(generate-db-methods)
		(setq again (upcase (read-string "add another method? Y/y to continue: ")))
	)
	(setq apimethods (concat "\t@Autowired\n\tprivate " servicename " " (downcase microservicename) "Service;\n" apimethods))
	(setq servicemethods ( concat "\t@Autowired\n\tprivate " reponame " " (downcase microservicename) "Repo;\n" servicemethods))
	(read-string "press enter to put repo methods into clipboard")
	(getrepomethods)
	(alttab-and-paste)
	(read-string "press enter to put service methods into clipboard")
	(getservicemethods)
	(alttab-and-paste)
	(read-string "press enter to put api methods into clipboard")
	(getapimethods)
	(alttab-and-paste)
)

(defun make-web-methods ()
	(setq apimethods "")
	(setq servicemethods "")
	(setq repomethods "")
	(setq servicename "")
	(setq reponame "")
	(setq microservicename "")
	(setq microservicename (read-string "micro-service name(e.g. \"<Object Name>\" -> I<Object Name>Repo <Object Name>Service etc.): "))
	(setq servicename (concat microservicename "Service"))
	(setq reponame (concat "I" microservicename "Repo"))
	(setq again "Y")
	(while (string= again "Y")
		(generate-web-methods)
		(setq again (upcase (read-string "add another method? Y/y to continue: ")))
	)
	(read-string "press enter to put repo methods into clipboard")
	(getrepomethods)
	(alttab-and-paste)
	(read-string "press enter to put service methods into clipboard")
	(getservicemethods)
	(alttab-and-paste)
	(read-string "press enter to put api methods into clipboard")
	(getapimethods)
	(alttab-and-paste)
)

(defun make-db-methods ()
	(setq apimethods "")
	(setq servicemethods "")
	(setq repomethods "")
	(setq servicename "")
	(setq reponame "")
	(setq microservicename "")
	(setq microservicename (read-string "micro-service name(e.g. \"<Object Name>\" -> I<Object Name>Repo <Object Name>Service etc.): "))
	(setq servicename (concat microservicename "Service"))
	(setq reponame (concat "I" microservicename "Repo"))
	(setq again "Y")
	(while (string= again "Y")
		(generate-db-methods)
		(setq again (upcase (read-string "add another method? Y/y to continue: ")))
	)
	(read-string "press enter to put repo methods into clipboard")
	(getrepomethods)
	(alttab-and-paste)
	(read-string "press enter to put service methods into clipboard")
	(getservicemethods)
	(alttab-and-paste)
	(read-string "press enter to put api methods into clipboard")
	(getapimethods)
	(alttab-and-paste)
)

(defun generate-web-methods ()
	(setq entry "")
	(setq parametertype "")
	(setq passdownparams "")
	(setq parameters "")
	(setq parameterswithtypes "")
	(setq httpmethod (capitalize (read-string "http method: ")))
	(setq path (read-string "path: "))
	(if (not (string= path "")) (setq path (concat "(\"" path "\")")))
	(setq return-type (read-string "return type: "))
	(setq methodname (camelwp(read-string "method name(type lowercase with spaces): ")))
	(setq another "Y")
	(while (string= another "Y")
		(setq parametername (read-string "parameter name?: "))
		(setq entry (read-string "path, param, or body?: "))
			(if (string= (upcase entry) "PATH") (setq parametertype (concat "@PathVariable(\"" parametername "\") ")) nil)
			(if (string= (upcase entry) "PARAM") (setq parametertype (concat "@RequestParam(\"" parametername "\") ")) nil)
			(if (string= (upcase entry) "BODY") (setq parametertype "@RequestBody") nil)
		(setq passdownparams (concat passdownparams parametername))
		(setq entry (read-string "parameter data type: "))
		(setq entry (concat entry " " parametername))		
		(setq another (upcase (read-string "another? y/Y to continue: ")))
			(if(string= another "Y") (setq entry (concat entry ", ")) nil)
			(if(string= another "Y") (setq passdownparams (concat passdownparams ", ")) nil)
		(setq parameters (concat parameters entry))
		(setq parameterswithtypes (concat parameterswithtypes parametertype entry))
	)
	(setq parameters (concat "(" parameters ")"))
	(setq passdownparams (concat "(" passdownparams ")"))
	(setq parameterswithtypes (concat "(" parameterswithtypes ")"))
	(setq apimethods (concat apimethods "\n\t@" httpmethod "Mapping" path "\n"
						"\tpublic " return-type " " methodname parameterswithtypes "{\n\t\t" (downcase microservicename) "Service." methodname passdownparams ";\n\t}")) 
	(setq servicemethods (concat servicemethods "\n\tpublic " return-type " " methodname parameters "{\n\t\t"
							(downcase microservicename) "Repo." methodname passdownparams ";\n\t}"))
	(setq repomethods (concat repomethods "\n\t@" httpmethod "Mapping" path "\n"
						 "\tpublic " return-type " " methodname parameterswithtypes ";"))
	
)

(defun generate-db-methods ()
	(setq entry "")
	(setq parametertype "")
	(setq parameters "")
	(setq passdownparams "")
	(setq parameterswithtypes "")
	(setq httpmethod (capitalize (read-string "http method: ")))
	(setq path (read-string "path: "))
	(if (not (string= path "")) (setq path (concat "(\"" path "\")")))
	(setq return-type (read-string "return type: "))
	(setq repomethodname (camelwp(read-string "repo method name(type lowercase wtih spaces): ")))
	(setq methodname (camelwp(read-string "non-repo method name(type lowercase with spaces): ")))
	(setq another "Y")
	(while (string= another "Y")
		(setq parametername (read-string "parameter name?: "))
		(setq entry (read-string "path, param, or body?: "))
			(if (string= (upcase entry) "PATH") (setq parametertype (concat "@PathVariable(\"" parametername "\") ")) nil)
			(if (string= (upcase entry) "PARAM") (setq parametertype (concat "@RequestParam(\"" parametername "\") ")) nil)
			(if (string= (upcase entry) "BODY") (setq parametertype "@RequestBody") nil)
		(setq entry (read-string "parameter data type: "))
		(setq passdownparams (concat passdownparams parametername))
		(setq entry (concat entry " " parametername))
		(setq another (upcase (read-string "another? y/Y to continue: ")))
			(if(string= another "Y") (setq entry (concat entry ", ")) nil)
			(if(string= another "Y") (setq passdownparams (concat passdownparams ", ")) nil)
		(setq parameters (concat parameters entry))
		(setq parameterswithtypes (concat parameterswithtypes parametertype entry))
	)
	(setq parameters (concat "(" parameters ")"))
	(setq passdownparams (concat "(" passdownparams ")"))
	(setq parameterswithtypes (concat "(" parameterswithtypes ")"))
	(setq apimethods (concat apimethods "\n\t@" httpmethod "Mapping" path "\n"
						"\tpublic " return-type " " methodname parameterswithtypes "{\n\t\t" (downcase microservicename) "Service." methodname passdownparams ";\n\t}")) 
	(setq servicemethods (concat servicemethods "\n\tpublic " return-type " " methodname parameters "{\n\t\t"
							(downcase microservicename) "Repo." methodname passdownparams ";\n\t}"))
	(setq repomethods (concat repomethods "\n\t@" httpmethod "Mapping" path "\n"
						 "\tpublic " return-type " " repomethodname parameters ";"))
)

(defun getapimethods ()
	(kill-new apimethods)
)

(defun getservicemethods ()
	(kill-new servicemethods)
)

(defun getrepomethods ()
	(kill-new repomethods)
)

(defun alttab ()
	(w32-shell-execute "open"
		"C:/Program Files/emacs/share/AHKMacros/emacsalttab.ahk")
)

(defun alttab-and-paste ()
	(w32-shell-execute "open"
		"C:/Program Files/emacs/share/AHKMacros/emacsalttabpaste.ahk")
)

(defun open-macros-folder ()
	(w32-shell-execute "open"
		"C:/Program Files/emacs/share/AHKMacros")
)
	
(defun elfunctions ()
	(w32-shell-execute "open"
		"C:/Program Files/emacs/share/emacs/25.3/site-lisp/elfunctions.lisp"
	)
)

(defun get-all-model-versions ()
	(setq filenamer "")
	(setq filecontents "")
	(setq artifacto "")
	(setq artifactonum "")
	(setq artifactnam nil)
	(setq artifactos nil)
	(w32-shell-execute "open"
		"C:/Program Files/emacs/bin/versionscript.bat")
	(sit-for 1)
	(setq initialfiles (directory-files-recursively "C:/Program Files/emacs/bin/mavens" ""))
	(setq copy-of-files (copy-alist initialfiles))
	(while copy-of-files
		(setf filenamer (pop copy-of-files))
		(setq templist (split-string filenamer "/"))
		(while (not (string-match "-models" (setf artifacto (concat (pop templist) ":")))))
		(setq currentfilecontentsraw (with-temp-buffer (insert-file-contents filenamer) (buffer-string)))
		(setq filelines (split-string currentfilecontentsraw "\n"))
		(while (not (string-match "release" 
			(setf artifactonum (pop filelines)))))
		(setf artifactonum (replace-regexp-in-string "[<>/\t\s]*release[<>/]*" "" artifactonum))
		(setf artifactos (append artifactos (list (concat artifacto artifactonum))))
		(setf artifactnam (append artifactnam (list artifacto)))
	)
	(setq artifactnames (copy-alist artifactnam))
	(setq currentversions (copy-alist artifactos))
	"Got all model versions"
)

(defun change-all-model-versions (filepath)
	(get-all-model-versions)
	(setq models (copy-alist currentversions))
	(setq extension ".gradle")
	(setq allgradles (directory-files-recursively filepath extension))
	(setq gradlecopy (copy-alist allgradles))
	(setq currentfilepath "")
	(setq currentfiledata "")
	(setq currentname "")
	(setq currentmodel "")
	(while gradlecopy
		(setq names (copy-alist artifactnames))
		(setq values (copy-alist currentversions))
		(setf currentfilepath (pop gradlecopy))
		(setf currentfiledata (with-temp-buffer
								(insert-file-contents currentfilepath) (buffer-string)))
		(while names
			(setf currentname (pop names))
			(setf currentmodel (pop values))
			(setf currentfiledata (replace-regexp-in-string (concat currentname "[0-9]+\.[0-9]+") currentmodel currentfiledata)))
		(write-region currentfiledata nil currentfilepath)	
	)
	(makunbound 'models)
	(makunbound 'extension)
	(makunbound 'allgradles)
	(makunbound 'gradlecopy)
	(makunbound 'currentfilepath)
	(makunbound 'currentfiledata)
	(makunbound 'currentname)
	(makunbound 'currentmodel)
	(makunbound 'names)
	(makunbound 'values)
)	
	
(defun eval-string (string)
	(eval (car (read-from-string (format "(progn %s)" string)))))

(defun iorl()
	(cond
		((string= (upcase (current-kill 0)) "I") (print "that is an eye"))
		((string= (upcase (current-kill 0)) "L") (print "that is an ell"))
		((print "that is neither an eye nor an ell"))))
		
(defun swagser ()
	(setq service-name (read-string "service name: "))
	(setq ymls (directory-files-recursively "C:/Users/<User>/Documents/Git/devconfig-local" "yml"))
	(setq potentialpath "")
	(setq top-path nil)
	(setq theport "")
	(while ymls
		(setf potentialpath (pop ymls)) 
		(cond (
				(string-match-p service-name potentialpath)
				(setf top-path potentialpath)
				(setf ymls nil))))
	(cond (top-path
			(setq ymlfile (with-temp-buffer
				(insert-file-contents top-path) (buffer-string)))
				(string-match "server:[ \n\t]*port:[^0-9]*\\([0-9]+\\)" ymlfile)
				(setf theport  (match-string 1 ymlfile))
				(print theport)
				(w32-shell-execute "open" 
				(concat "http://localhost:" theport "/swagger-ui.html")))
		   ("not found"))
					
)

(defun renamers ()
	(setq paths (directory-files-recursively "C:/Users/<User>/Documents/Git" "\\Application.java"))
	(setq shortenedpaths nil)
	(while paths
	 (setq pathchecker (pop paths))
	 (if (string-match-p "/Application\.java" pathchecker) (setf shortenedpaths (append shortenedpaths (list pathchecker))))))

(defun killport(port)
	(w32-shell-execute 
	"open" "powershell"
	(concat "-Command Stop-Process (Get-NetTCPConnection -State Listen -LocalPort " (int-to-string port) ").OwningProcess"))
)

(defun docker-refresh ()
	(w32-shell-execute "open" "powershell" 
	"-Command cd C:\\Users\\<User>\\Documents\ndocker-compose down\nY\nStop-Process -Name \"powershell\" -Force")
	(sit-for 25)
	(w32-shell-execute "open" "powershell" 
	"-Command cd C:\\Users\\<User>\\Documents\nrmdir .\\zk-single-kafka-multiple\\ -Recurse\ndocker-compose up\n"))
	
(defun duplicates (alist)
	(setq bmem nil)
	(setq clist nil)
	(while alist
		(setf bmem (pop alist))
		(if (member bmem alist) (setf clist (append clist (list bmem)))))
	clist
)
		
	
(defun sr ()
	(w32-shell-execute "open" "http://localhost:8761"))

(defun compile-services (&optional stringlist)
	(setq git-path "C:\\Program Files\\Git\\git-cmd.exe")
		(setq options "\"cd C:\\Users\\<User>\\Documents\\Git&")
		(setq temp "")
		(setq top-name "")
		(while stringlist
			(setq services (directory-files "C:/Users/<User>/Documents/Git"))
			(setq currentservice (pop stringlist))
			(setq temp "quit")
			(while services
					(setq servicename (pop services))
					(if (string-match-p currentservice servicename) (progn (setf top-name servicename) (setf services nil))))
					
			(w32-shell-execute "open" git-path (concat options "cd " top-name "&gradlew clean bootjar&exit\"") 6))
			
		(while (not (string= temp "quit"))
			(setq services (directory-files "C:/Users/<User>/Documents/Git"))
			(setq temp (read-string "service name (quit to end): "))
			(cond
				((string= temp "quit") nil)
				((not (string= temp ""))
					(while services
						(setq servicename (pop services))
						(if (string-match-p temp servicename) (progn (setf top-name servicename) (setf services nil))))
		
		(w32-shell-execute "open" git-path (concat options "cd " top-name "&gradlew clean bootjar&exit\"") 6))))
)


(defun run-services (&rest stringlist)
	(setq git-path "C:\\Program Files\\Git\\git-cmd.exe")
	(setq options "\"cd C:\\Users\\<User>\\Documents\\Git&")
	(setq temp "")
	(setq top-name "")
	(while stringlist
		(setq services (directory-files "C:/Users/<User>/Documents/Git"))
		(setq currentservice (pop stringlist))
		(setq temp "quit")
		(while services
				(setq servicename (pop services))
				(if (string-match-p currentservice servicename) (progn (setf top-name servicename) (setf services nil))))
		
		(setq current-path (concat "C:/Users/<User>/Documents/Git/" top-name "/build/libs"))
		(w32-shell-execute "open" git-path (concat options "cd " top-name "&gradlew clean bootjar&cd build/libs&TITLE " top-name
		"&java -jar -Xmx512m " (car (directory-files current-path nil "\.jar")) "\"") 6))
		
	(while (not (string= temp "quit"))
		(setq services (directory-files "C:/Users/<User>/Documents/Git"))
		(setq temp (read-string "service name (quit to end): "))
		(cond
			((string= temp "quit") nil)
			((not (string= temp ""))
				(while services
					(setq servicename (pop services))
					(if (string-match-p temp servicename) (progn (setf top-name servicename) (setf services nil))))
	
	(setq current-path (concat "C:/Users/<User>/Documents/Git/" top-name "/build/libs"))
	(w32-shell-execute "open" git-path (concat options "cd " top-name "&gradlew clean bootjar&cd build/libs&"
		"java -jar -Xmx512m " (car (directory-files current-path nil "\.jar")) "\"") 6))))
)

(defun clear ()
	(comint-clear-buffer) empty-char)

(defun bitbucket ()
	(w32-shell-execute "open" "https://bitbucket.org/dashboard/overview"))
	
(defun taskman ()
	(w32-shell-execute "open" "taskmgr.exe") empty-char)
	
(defun strip-secrets ()
	(kill-new
		(replace-regexp-in-string
			"'\{cipher\}.*"
			"\(cipher stripped via function\)"
			(current-kill 0)
		)
	)
	"Ciphers stripped"
)	

(defun unbind-all ()
	(setq filepathunbind "c:/Program Files/emacs/share/emacs/25.3/site-lisp/elfunctions.lisp")
	(setq filedataunbind (with-temp-buffer (insert-file-contents filepathunbind) (buffer-string)))
	(setq filelinesunbind (split-string filedataunbind "\n"))
	(setq flagor '(thishelpsevaluateastrue))
	(while flagor
		(setq splitor (split-string (pop filelinesunbind) " "))
		(while (string= (car splitor) "") (pop splitor))
		(setq firstone (car splitor))
		(setq secondone (cadr splitor))
		(cond 
			(secondone 
				(if (string-match-p "unbind" secondone) (setf flagor nil))))
		(cond
			((and flagor firstone)
			(if (string-match-p "setq" firstone) (eval-string (concat "(makunbound '" secondone ")"))))))
	(reload)
	(makunbound 'filepathunbind)
	(makunbound 'filedataunbind)
	(makunbound 'filelinesunbind)
	(makunbound 'flagor)
	(makunbound 'splitor)
	(makunbound 'firstone)
	(makunbound 'secondone)
	"done"
)
