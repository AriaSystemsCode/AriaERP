*
* Get a timestamp value based on the current date and time.
*
*!*****************************************************************************
*!
*!       Function: STAMPVAL
*!
*!      Called by: CONVERTER          (procedure in TRANSPRT.PRG)
*!
*!          Calls: SHIFTL()           (function  in TRANSPRT.PRG)
*!               : SHIFTR()           (function  in TRANSPRT.PRG)
*!
*!*****************************************************************************
FUNCTION stampval
PRIVATE m.dateval, m.timeval

m.dateval = DAY(DATE()) + ;
   shiftl(MONTH(DATE()), 5) + ;
   shiftl(YEAR(DATE())-1980, 9)

m.timeval = shiftr(VAL(RIGHT(TIME(),2)),1) + ;
   shiftl(VAL(SUBSTR(TIME(),3,2)),5) + ;
   shiftl(VAL(LEFT(TIME(),2)),11)

RETURN shiftl(m.dateval,16)+m.timeval

*
* Shift a value x times to the left.  (This isn't a true match for
* a shift since we keep extending the value without truncating it,
* but it works for us.)
*
*!*****************************************************************************
*!
*!       Function: SHIFTL
*!
*!      Called by: STAMPVAL()         (function  in TRANSPRT.PRG)
*!
*!*****************************************************************************
FUNCTION shiftl
PARAMETER m.value, m.times
PRIVATE m.loop

FOR m.loop = 1 TO m.times
   m.value = m.value * 2
ENDFOR
RETURN m.value

*
* Shift a value x times to the right.  (This isn't a true match for
* a shift since we keep extending the value without truncating it,
* but it works for us.)
*
*!*****************************************************************************
*!
*!       Function: SHIFTR
*!
*!      Called by: STAMPVAL()         (function  in TRANSPRT.PRG)
*!
*!*****************************************************************************
FUNCTION shiftr
PARAMETER m.value, m.times
PRIVATE m.loop

FOR m.loop = 1 TO m.times
   m.value = INT(m.value / 2)
ENDFOR
RETURN m.value

*
* EmptyPlatform - Takes a platform ID and returns .T. if no records for that platform
*       are in the file or .F. if some are present.
*
