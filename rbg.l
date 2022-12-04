
( setf *limit* 25 )

( defun rbg ( &aux lst l )
  ( setf lst ( list 'R 'B 'G ) )
  ( setf l ( nth ( random ( length lst ) ) lst ) )
  l
)

( defun rbg-list ( n &aux lst )
  ( setf lst '() )
  ( dotimes ( i n ) 
    ( setf lst ( cons ( rbg ) lst ) )
  )
  lst
)

( defun rbg-string ()
  ( rbg-list *limit* )
)

( defmethod mutation ( ( rbg-str list ) &aux p q ) 
  ( setf p ( random ( length rbg-str ) ) )
  ( setf q ( others '( R B G ) ( nth p rbg-str ) ) )
  ( change rbg-str ( pick q ) p )
)

( defmethod others ( ( lst list ) n )
  ( remove n lst )
)

( defun pick ( lst ) 
  ( nth ( random ( length lst ) ) lst )
)

( defmethod change ( ( lst list ) q ( p integer ) )
  ( setf ( nth p lst ) q )
  lst
)

( defmethod crossover ( ( m list ) ( f list ) )
  ( setf pos ( + 1 ( random ( length m ) ) ) )
  ( append ( first-n m pos ) ( rest-n f pos ) )
)

( defun first-n ( ( lst list ) ( n integer ) )
  ( setf first-str ( subseq lst 0 ( - n 1 ) ) )
  first-str
)

( defun rest-n ( ( lst list ) ( n integer ) )
  ( setf rest-str ( subseq lst ( - n 1 ) ) )
  rest-str
)

( defmethod mutation-demo ( &aux s m ) 
  ( setf s ( rbg-string ) )
  ( dotimes ( i 10 )
    ( format t "s = ~A~%" s )
    ( setf m ( mutation s ) )
    ( format t "m = ~A~%~%" m )
  )
)

( defmethod crossover-demo ( &aux m f x ) 
  ( setf m ( rbg-string ) )
  ( setf f ( rbg-string ) )
  ( dotimes ( i 10 )
    ( format t "m = ~A~%" m )
    ( setf x ( crossover m f ) )
    ( format t "x = ~A~%" x )
    ( format t "f = ~A~%~%" f )
  )
)

( defmethod fitness-r ( ( rbg-str list ) )
  ( count 'R rbg-str )
)

( defmethod fitness-b ( ( rbg-str list ) )
  ( count 'B rbg-str )
)

( defmethod fitness-g ( ( rbg-str list ) )
  ( count 'G rbg-str )
)

( defmethod fitness-demo ( &aux x fitness )
  ( setf x ( rbg-string ) )
  ( format t "x = ~A~%" x )
  ( format t "Directly applying the fitness metrics... ~%" )
  ( format t "fitness-r = ~A~%" (fitness-r x ) )
  ( format t "fitness-b = ~A~%" (fitness-b x ) )
  ( format t "fitness-g = ~A~%" (fitness-g x ) )
  ( format t "Indirectly applying the fitness metrics... ~%" )
  ( setf fitness #'fitness-r )
  ( format t "fitness-r = ~A~%" ( funcall fitness x ) )
  ( setf fitness #'fitness-g )
  ( format t "fitness-g = ~A~%" ( funcall fitness x ) )
  ( setf fitness #'fitness-b )
  ( format t "fitness-b = ~A~%" ( funcall fitness x ) )
)

  (
   ( rbg-string :accessor individual-rbg-string :initarg :rbg-string )
   ( fitness :accessor individual-fitness :initarg :fitness )
   ( number :accessor individual-number :initarg :number )
  )
)

( defmethod random-individual ( &aux rbg ) 
  ( setf rbg ( rbg-string ) )
  ( make-instance 'individual
	:rbg-string rbg
	:fitness ( funcall *fitness* rbg )
	:number 0 
  )	
)

( defmethod new-individual ( ( nr number ) ( notes list ) )
  ( make-instance 'individual
    :rbg-string notes
	:fitness ( funcall *fitness* notes )
	:number nr
  )	
)

( defmethod display ( ( i individual ) )
  ( display-nnl i ) ( terpri )
)

( defmethod display-nnl ( ( i individual ) )
  ( prin1 ( individual-number i ) )
  ( princ ( filler ( individual-number i ) ) )
  ( prin1 ( individual-rbg-string i ) )
  ( princ "  " )
  ( prin1 ( individual-fitness i ) )
  ( princ ( filler ( individual-fitness i ) ) )
)

( defmethod filler ( ( n number ) )
  ( cond
	( ( < n 10 ) "     " )
	( ( < n 100 ) "    " )
	( ( < n 1000 ) "   " )
	( ( < n 10000 ) "  " )
	( ( < n 100000 ) " " )
  )
)

( defmethod fitness-b ( ( i individual ) )
  ( fitness-b ( individual-rbg-string i ) )
)

( defmethod fitness-r ( ( i individual ) )
  ( fitness-r ( individual-rbg-string i ) )
)	

( defmethod fitness-g ( ( i individual ) )
  ( fitness-g ( individual-rbg-string i ) )
)

( defmethod individual-demo ( &aux i0 i1 i2 i3 one two three )
  ( setf *fitness* #'fitness-r )
  ( setf i0 ( random-individual) )
  ( display i0 )
  ( setf one ( rbg-string ) )
  ( setf i1 ( new-individual 1 one ) )
  ( display i1 )
  ( setf two ( rbg-string ) )
  ( setf i2 ( new-individual 2 two ) )
  ( display i2 )
  ( setf three (rbg-string ) )
  ( setf i3 ( new-individual 3 three ) )
  ( display i3 )
  ( format t "Fitness of i0 = ~A~%" ( funcall *fitness* i0 ) )
  ( format t "Fitness of i1 = ~A~%" ( funcall *fitness* i1 ) )
  ( format t "Fitness of i2 = ~A~%" ( funcall *fitness* i2 ) )
  ( format t "Fitness of i3 = ~A~%" ( funcall *fitness* i3 ) )
  nil
)


( defconstant *population-size* 100 )
( defconstant *selection-size* 8 )
( setf *fitness* #'fitness-b )

( defclass population ()
  (
	( individuals :accessor population-individuals :initarg :individuals )
	( generation :accessor population-generation :initform 0 )
  )
)

( defmethod size ( ( p population ) )
  ( length ( population-individuals p ) )
)

( defmethod display ( ( p population ) )
  ( terpri ) ( terpri )
  ( princ "Generation " )
  ( prin1 ( population-generation p ) )
  ( princ " population..." )
  ( terpri ) ( terpri )
  ( dolist ( i ( population-individuals p ) )
	( display i )
  )
  ( terpri )
)

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* )
	( push ( new-individual ( + i 1 ) ( rbg-string ) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)	

( defmethod average ( ( p population ) &aux (sum 0) )
  ( loop for i in ( population-individuals p ) 
    do ( setf sum ( + sum ( individual-fitness i ) ) )
  )
  ( float ( / sum ( size p ) ) )
)

( setf *select-demo* nil )

( defmethod select-individual ( ( p population ) &aux i candidates rn )
  ( setf candidates ( select-individuals p ) )
  ( setf mfi ( most-fit-individual candidates ) )
  ( if *select-demo* ( select-demo-helper candidates mfi ) )
  mfi
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates () )
  ( dotimes ( i *selection-size* )
    ( setf rn ( random *population-size* ) )
    ( push ( nth rn individuals ) candidates )
  )
  candidates 
)


( defmethod most-fit-individual ( ( l list ) &aux max-value max-individual )
  ( setf max-value 0 )
  ( dolist ( i l )
	( cond
	  ( ( > ( individual-fitness i ) max-value )
		( setf max-value ( individual-fitness i ) )
		( setf max-individual i )
	  )
	)	
  )
  max-individual
)

( defmethod select-demo-helper ( ( l list ) ( i individual ) )
  ( princ "the sample of individuals..." ) ( terpri )
  ( mapcar #'display l )
  ( terpri )
  ( princ "the most fit of the sample..." ) ( terpri )
  ( display i )
  ( terpri )
  nil
)

( defmethod population-demo ( &aux p )
  ( setf p ( initial-population ) )
  ( display p )
  ( format t "Average fitness = ~A~%~%" ( average p ) )
  ( setf *select-demo* t )
  ( format t "Sampling...~%~%" )
  ( select-individual p ) ( terpri )
  ( format t "Sampling...~%~%" )
  ( select-individual p ) ( terpri )
  ( format t "Sampling...~%~%" )
  ( select-individual p ) ( terpri )
)

( defmethod mutate ( ( i individual ) &aux mutation )
  ( setf mutation ( mutation ( individual-rbg-string i ) ) )
  ( make-instance 'individual
    :number ( individual-number i )
    :rbg-string mutation
    :fitness ( funcall *fitness* mutation )
  )
)

( defconstant *pc-m* 50 )

( defmethod maybe-mutate ( ( i individual ) )
  ( if ( <= ( + 1 ( random 100 ) ) *pc-m* )
    ( mutate i )
    i
  )
)


( defmethod mutate-demo ()
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
	( setf i ( mutate i ) )
	( display i )
  )	
)

( defmethod maybe-mutate-demo ()
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
	( setf n ( maybe-mutate i ) )
	( display-nnl n )
	( if ( not ( equal n i ) ) ( princ " *" ) )
	( terpri )
	( setf i n )
  )	
)

( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-copies ) )
    ( perform-one-copy cp np )
  )
)

( defmethod nr-copies ()
  ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) &aux x m mm new-i )
  ( setf m ( select-individual cp ) )
  ( if *copy-demo* ( format t "Selected individual = ~%" ) )
  ( if *copy-demo* ( display m ) )
  ( setf mm ( maybe-mutate m ) )
  ( if *copy-demo* ( format t "Possibly muted individual = ~&" ) ) 
  ( if *copy-demo* ( display mm ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( if *copy-demo* ( format t "Renumbered individual = ~&" ) ) 
  ( if *copy-demo* ( display mm ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-rbg-string mm ) ) )
  ( setf
    ( population-individuals np )
    ( append ( population-individuals np ) ( list new-i ) )
  )
  nil
)

( defmethod empty-population ( ( cp population ) &aux np )
  ( setf np ( make-instance 'population ) )
  ( setf ( population-individuals np ) () )
  ( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )
  np
)

( defmethod perform-copies-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( terpri ) ( display np ) ( terpri ) ( terpri )
  ( setf *select-demo* t )
  ( setf *copy-demo* t )
  ( dotimes ( i 10 )
	( perform-one-copy cp np )
	( terpri ) ( display np ) ( terpri ) ( terpri )
  )
  ( setf *select-demo* nil )
  ( setf *copy-demo nil )
  nil
)

( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )
 
( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np )
  )
)

( defmethod nr-crossovers ()
  ( * ( / *pc-x* 100 ) *population-size* ) 
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) )
  ( let ( x m mm mother father new-i )
    ( setf mother ( select-individual cp ) )
    ( setf father ( select-individual cp ) )
    ( if *crossover-demo* ( format t "Selected mother = ~%" ) )
    ( if *crossover-demo* ( display mother ) )
    ( if *crossover-demo* ( format t "Selected father = ~%" ) )
    ( if *crossover-demo* ( display father ) )
    ( setf m ( crossover mother father ) )
    ( if *crossover-demo* ( format t "the possibly mutated individual = ~&" ) )
    ( if *crossover-demo* ( display m ) )
    ( setf mm ( maybe-mutate m ) )
    ( if *crossover-demo* ( format t "the possibly mutated individual = ~&" ) )
    ( if *crossover-demo* ( display mm ) )
    ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
    ( if *crossover-demo* ( format t "the renumbered individual = &" ) )
    ( if *crossover-demo* ( display mm ) )
    ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-rbg-string mm ) ) )
    ( setf
      ( population-individuals np )
      ( append ( population-individuals np ) ( list new-i ) )
    )
  )
  nil
)

( defmethod crossover ( ( mother individual ) ( father individual ) &aux mi fi x i )
  ( setf mi ( individual-rbg-string mother ) )
  ( setf fi ( individual-rbg-string father ) )
  ( setf x ( crossover mi fi ) )
  ( setf i ( new-individual 0 x ) )
  i
)

( defmethod perform-crossovers-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( terpri ) ( display np ) ( terpri ) ( terpri )
  ( setf *select-demo* t )
  ( setf *crossover-demo* t )
  ( dotimes ( i 10 )
	( perform-one-crossover cp np )
	( terpri ) ( display np ) ( terpri ) ( terpri )
  )
  ( setf *select-demo* nil )
  ( setf *crossover-demo* nil )
  nil
)

( defconstant *nr-generations* 25 )

( defmethod next-generation ( ( cp population ) &aux np )
  ( setf np ( empty-population cp ) )
  ( perform-copies cp np )
  ( perform-crossovers cp np )
  np
)

( defmethod ga-text-demo ( &aux p )
  ( format t "THE WORLD IS BLUE ~%~%" )
  ( setf *fitness* #'fitness-b ) 
  ( setf p ( initial-population ) ) 
  ( terpri )
  ( summarize p )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  )
  ( terpri )
  ( summarize p )
  ( format t "THE WORLD IS RED ~%~%" )
  ( setf *fitness* #'fitness-r ) 
  ( setf p ( initial-population ) )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  ) 
  ( terpri )
  ( summarize p )
  ( format t "THE WORLD IS GREEN ~%~%" )
  ( setf *fitness* #'fitness-g ) 
  ( setf p ( initial-population ) )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  ) 
  ( terpri )
  ( summarize p )
)

( defmethod summarize ( ( p population ) )
  ( display p )
  ( check-average p )
  ( terpri )
)

( defmethod check-average ( ( p population ) )
  ( format t "average fitness of population ~A = ~A~%" 
    ( population-generation p )
    ( average p )
  )
)
