USING: lists math kernel math.private quotations.private combinators ;
IN: misc 
: 1- ( x -- x ) 1 - ; inline
: 1+ ( x -- x ) 1 + ; inline 
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

: nor ( x y -- x ) bitor bitnot ; inline
: nand ( x y -- x ) bitand bitnot ; inline
: eqv ( x y -- x ) bitxor bitnot ; inline
: not-nor ( x -- x ) dup nor ;
: or-nor ( x y -- x ) nor dup nor ;
: and-nor ( x y -- x ) dup nor swap dup nor nor ;
: nand-nor ( x y -- x ) dup nor swap dup nor nor dup nor ;
! a b -> a b b b-> a b !b -> b !b a -> b !b a a a -> b !b a !a ->
! b a !b !a -> ...
: xor-nor  ( x y -- x ) dup dup nor rot dup dup nor swapd nor -rot nor nor ;
: xnor-nor ( x y -- x ) 2dup nor dup swapd nor -rot nor nor ;
: not-nand ( x y -- x ) nand nand ;
: and-nand  ( x y -- x ) nand dup nand ;
: or-nand ( x y -- x ) dup nand swap dup nand nand ;
: nor-nand ( x y -- x ) dup nand swap dup nand nand dup nand ;
: xor-nand ( x y -- x ) 2dup nand dup swapd nand -rot nand nand ;
: xnor-nand ( x y -- x ) 2dup nand dup swapd nand -rot nand nand dup nand ;
