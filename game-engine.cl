;; The world inside our game is very simple, containing only three locations.
;; Let's first create a top-level variable, *nodes*, to contain the descriptions
;; of the locations that exist in our game.

(defparameter *nodes* '((living-room (you are in the living-room.
                        a wizard is snoring loudly on the couch.))
                       (garden (you are in a beautiful garden.
                        there is a well in front of you.))
                       (attic (you are in the attic.
                        there is a giant welding torch in the corner.))))

;; We need to create a command to describe a location.
;; To accomplish this, we'll use the assoc function to find the correct item
;; in the list using a key.

(defun describe-location (location nodes)
       (cadr (assoc location nodes)))

;; We need descriptions of paths to other locations as well.
;; We'll create a second variable, *edges*, that contains the paths that
;; players can take to move between places on our map.
;; (We use the term edges because that's the proper math term for the lines
;; connecting nodes in a graph.)

(defparameter *edges* '((living-room (garden west door)
	      	      		     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; Using this structure we create the describe-path function, which builds
;; a textual description of a given edge using our symbols system.

(defun describe-path (edge)
       `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; Since a location may have any number of paths exiting from it, we need
;; a function that can generate descriptions for all edges from a given
;; location by looking up the location from our data structure of edges.

(defun describe-paths (location edges)
       (apply #'append(mapcar #'describe-path (cdr (assoc location edges)))))

;; We need to describe the objects on the floor at a given location, which
;; a player can pick up and use.

(defparameter *objects* '(whiskey bucket frog chain))

;; We need a second variable, *object-locations*, to track the location of
;; each object in the form of an alist.

(defparameter *object-locations* '((whiskey living-room)
	      			   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; A function that lists the objects visible from a given location.

(defun objects-at (loc objs obj-locs)
       (labels ((at-loc-p (obj)
       	       (eq (cadr (assoc obj obj-locs )) loc)))
	       (remove-if-not #'at-loc-p objs)))

;; Write a function to describe the objects visible at a given location.

(defun describe-objects (loc objs obj-loc)
       (labels ((describe-obj (obj)
       	       `(you see a ,obj on the floor.)))
	       (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; We need a variable to track the player's current position.
;; Because the *location* value is intitialized to the living-room symbol, players will
;; find themselves in the living room of the wizard's house at the start of the game. 

(defparameter *location* 'living-room)

;; We tie all of these description functions together into one easy command called look.
;; This will be the actual command players can enter to look around them in the game.

(defun look ()
       (append (describe-location *location* *nodes*)
       	       (describe-paths *location* *edges*)
	       (describe-objects *location* *objects* *object-locations*)))

;; The walk function takes a direction and lets us walk there.

(defun walk (direction)
       (let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
       (if next (progn (setf *location* (car next))
       	   (look))
	   '(you cannot go that way.))))

;; Create a function to pick up objects in our world. 
;; To do so we modify the variable *object-locations* that we're using to 
;; track the location of the objects.

(defun pickup (object)
       (cond ((member object (objects-at *location* *objects* *object-locations*))
       	     (push (list object 'body) *object-locations*)
	     	   `(you are now carrying the ,object))
		   (t '(you cannot get that.))))

;; A function that lets players see an inventory of objects they are carrying.

(defun inventory ()
       (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; Create our own REPL in Lisp.
;; A custom REPL for our game, which lets us call the look command in exactly
;; the same way as the standard REPL did.

;; *The purpose of our game-read function is to fix the two annoyances that make
;; the standard Lisp read function wrong for playing our game
;; *The Lisp read function forces us to put parentheses around our commands.
;; We should be able to just type 'look' without any parentheses.
;; To accomplish this we just call read-line and stick in our own parentheses.
;; *With read we must put a quote in front of any function arguments.
;; We should be able to type 'walk east' without a quote in fron of east.
;; To do this we'll stick a quote in front of the parameters after the fact.

(defun game-read ()
       (let ((cmd (read-from-string
       	    (concatenate 'string "(" (read-line) ")"))))
	    (flet ((quote-it (x)
	    	  (list 'quote x)))
		  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; Improving the eval command.
;; The main problem with eval is that it allows you to call any Lisp command.
;; To help protect our program, we'll create a game-eval function that allows
;; only certain commands to be called.

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
       (if (member (car sexp) *allowed-commands*)
       	   (eval sexp)
	   	 '(i do not know that command)))

;; Improving the print command.
;; One obvious limitation of the print command is that all text descriptions 
;; are printed in uppercase.
;; By writing our own game-print function we can solve this problem.

;; tweak-text function definition.

(defun tweak-text (lst caps lit)
   (when lst
  (let ((item (car lst))
         (rest (cdr lst)))
   (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
         ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
         ((eq item #\") (tweak-text rest caps (not lit)))
           (lit (cons item (tweak-text rest nil lit)))
         ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
         (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;; The game-print function and its helper function are a bit more complicated.
;; The game-print function converts the symbol list (containing the text whose
;; layout we want to fix) into a string with print1-to-string, one of Lisp's
;; many print variants.

;; game-print function definition

  (defun game-print (lst)
   (princ (coerce (tweak-text (coerce (string-trim "() "
                                                    (prin1-to-string lst))
                                       'list)
                               t
                              nil)
                   'string))
    (fresh-line))

		  
;; game-repl function definition

(defun game-repl ()
       (let ((cmd (game-read)))
       	    (unless (eq (car cmd) 'quit)
	    (game-print (game-eval cmd))
	    (game-repl))))

;; function that tweaks our node identifiers to make the appropriate for DOT

(defun dot-name (exp)
  (substitute-if #\_(complement #'alphanumericp) (prin1-to-string exp)))

;; function to generate the label that should appear in the node when it is
;; drawn. the label will consist of the node name and the data linked to the
;; node alist

(defparameter *max-label-length* 30)
(Defun dot-label (exp)
  (if exp
(let ((s (write-to-string exp :pretty nil)))
(if (> (length s) *max-label-length*)
(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
s))
""))