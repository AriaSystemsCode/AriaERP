*!*************************************************************
*! Name      : ARGETSCA.PRG
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 05/26/1999
*! Purpose   : Function to Evaluate Invoice FIRST 3 SCALES
*! Returns   : NONE
*!           Due to C101535,1
*!*************************************************************
*! Called from : ARPINVCL.FRX (Dos Format)
*!*************************************************************


PARAMETER lXScales
lXScales =''
IF !EMPTY(lcRpExp)
  lcRpExp = lcRpExp +  " .AND. "
ENDIF  
  lcRpExp = lcRpExp +  ' !EOF("INVLINE") '
_PADVANCE = 'LINEFEED'


FUNCTION lfGetScale
PARAMETER lcDummy
lcDummy = " "
PRIVATE lnNoOscl,laScales,lnCount,lcScale
lnNoOscl = 2
*-- Just to avoid array overwrite
IF lcInvNo == INVHDR.INVOICE
  RETURN
ENDIF


lnAlias = SELECT(0)

SELECT INVLINE
IF EOF() OR BOF()
  lnRecNo = -1
ELSE
  lnRecNo = RECNO()
ENDIF  
=SEEK(INVHDR.INVOICE,'INVLINE')
lnCount = 0
lcScale=""
SCAN WHILE INVOICE = INVHDR.INVOICE AND lnCount < lnNoOscl
  IF !(Scale == lcScale)
    lcScale = Scale
    lnCount = lnCount + 1
    DIMENSION laScales[lnCount,9]

    laScales[lnCount,1] = Scale
    *--There are 32 size fields foe the 4 scales.
    FOR lnCnt = 1 TO 8
      SclFilS = STR(lnCnt,1)
      laScales[lnCount,lnCnt+1] = SCALE.SZ&SclFilS
    ENDFOR
  ENDIF
ENDSCAN
IF lnRecNo <> -1
  GO (lnRecNo)
ENDIF
SELECT (lnAlias)
lcSclHead = ''
IF TYPE("laScales") = "C"
  FOR lnRow = 1 TO ALEN(laScales,1)
    lcSclHead   = lcSclHead + LEFT(laScales[lnRow,1],1) + SPACE(6)
    FOR lnCol = 1 TO 8
      lcSclHead = lcSclHead + ' ' + PADL(ALLTRIM(LEFT(laScales[lnRow,lnCol+1],3)),3)
    ENDFOR
    IF lnRow < ALEN(laScales,1)
      lcSclHead   = lcSclHead + CHR(13)+ CHR(10)
    ENDIF
  ENDFOR
ENDIF  
RETURN 