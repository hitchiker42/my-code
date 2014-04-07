USING: lists math kernel math.private quotations.private combinators ;
IN: misc 
: 1- ( x -- x ) 1 - ;
: 1+ ( x -- x ) 1 + ;
: fact-acc ( x y -- z ) [ 1 <= ] keep [ drop ] [ [ * ] keep 1- fact-acc ] if ;
: fact ( x -- y ) 1 swap fact-acc ;
! (case ((= m 0) (1+ n)) ((and (> m 0) (= n 0)) (ack (1- m) 1))
! ((and (> m 0) (> n 0)) (ack (1- m) (ack m (1- n))))
DEFER: ack
: ack ( m n -- n )
    {
        { [ over zero? ] [ nip 1+ ] }
        { [ 2dup [ 0 > ] [ 0 = ] bi* and ]   [ drop 1- 1 ack ] }
        { [ 2dup [ 0 > ] [ 0 > ] bi* and ] [ dupd 1- [ 1- ] 2dip ack ack ] } } cond ;
! [ n 1- m act ] call m 1- ack
