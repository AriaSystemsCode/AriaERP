*:***************************************************************************
*: Program file  : SOORCNCK.PRG
*: Program desc. : Print Order Confirmation Form for Charies Kirk
*: For Report    : SOORCNCK.FRX
*: System        : Aria Apparel System (A27).
*: Module        : SALES ORDER
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 02/10/2002
*: Reference     : C200280,1
*:***************************************************************************
*: Calls : 
*:    Procedures : None.
*:    Functions  : None.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORCNCK
*:***************************************************************************
*:Modifications
*B606583,1 BWA 12/15/2002 Fix the bug of print the text "Total Qunt." in empty page.
*B606808,1 BWA 12/31/2002 Fix the bug of print the note first preview only and print first 6 line notepad.
*:***************************************************************************

PRIVATE lnAlias
lnAlias = SELECT(0)

IF !USED('BOMVAR')
  =gfOpenFile(gcDataDir+'BOMVAR','BOMVAR','SH')
ELSE
  SELECT(lcTempOrd)
  SET RELATION OFF INTO BOMVAR
ENDIF

IF !USED('ICDESIGN')
  =gfOpenFile(gcDataDir+'ICDESIGN','ICDESIGN','SH')
ELSE
  SELECT BOMVAR
  SET RELATION OFF INTO ICDESIGN
ENDIF

IF !USED('NOTELABEL')
  USE (gcDataDir+'NOTEPAD') IN 0 AGAIN ORDER TAG NOTEPAD ALIAS NOTELABEL
ELSE
  SELECT STYLE
  SET RELATION OFF INTO NOTELABEL
ENDIF

SELECT(lcTempOrd)
SET RELATION TO 'SO'+ORDER+STR(LINENO,6) INTO BOMVAR ADDITIVE

SELECT BOMVAR
SET RELATION TO CDSGNCODE INTO ICDESIGN ADDITIVE

SELECT STYLE
SET RELATION TO 'T'+LABEL_CODE INTO NOTELABEL ADDITIVE
SET RELATION TO 'F'+cStyMajor INTO NOTEPAD ADDITIVE

DECLARE laStyNotes[6]
DECLARE laLabelCod[2]
STORE '' TO laStyNotes,laLabelCod

*B606808,1 BWA 12/31/2002 Reinitail the variable hold the style in the 2nd preview or print.[START]
lcOldStyle = ""
lnstylewid = LEN(gfItemMask('PM'))
*B606808,1 BWA 12/31/2002.[END]

*B606583,1 BWA 12/15/2002 Fix the bug of print the text "Total Qunt." in empty page.[START]
*I use the flag field which is in ver. 2.7 doesn't used to able to solve the bug.
PRIVATE lcRecNFlg , lcOrdFlg , lcStrFlg
STORE SPACE(0) TO lcOrdFlg , lcStrFlg
SELECT(lcTempOrd)
lcRecNFlg = EVAL(KEY())

LOCATE
lcOrdFlg = ORDER
lcStrFlg = STORE
SCAN
  IF lcOrdFlg # ORDER AND !EOF()
    lcOrdFlg = ORDER
    SKIP -1
    REPLACE FLAG WITH "T"
    SKIP
  ELSE
    IF lcOrdFlg = ORDER AND lcStrFlg # STORE AND !EOF()
      SKIP -1
      REPLACE FLAG WITH "T"
      SKIP
      lcStrFlg = STORE
    ENDIF
  ENDIF
ENDSCAN
IF EOF()
  LOCATE
  GOTO BOTTOM
  REPLACE FLAG WITH "T"
ENDIF
=SEEK(lcRecNFlg)

SELECT ORDHDR
SET SKIP TO
SET RELATION TO
SELECT(lcTempOrd)
SET RELATION TO cordtype+ order INTO ORDHDR ADDITIVE
SELECT ORDHDR
SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT(lcTempOrd)
LOCATE
lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'

DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
llSalsOrd = .F.
*B606583,1 BWA 12/15/2002.[END]

SELECT (lnAlias)
RETURN
*-- End Of Option Program.

*:*************************************************************
*: Name      : lfGetArray
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/10/2002
*: Purpose   : Fill the array of style notepad & label code template
*:*************************************************************
*: Calls     : gfItemMask
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : lfGetArray()
*:*************************************************************
*
FUNCTION lfGetArray
PARAMETERS lcDummy

DECLARE laStyNotes[6]
DECLARE laLabelCod[2]
STORE '' TO laStyNotes,laLabelCod

*--  Don't Print the Note if we print the same style
*B606808,1 BWA 12/31/2002 Add the order to the check.[START]
*IF lcOldStyle # LEFT(EVAL(lcTempOrd+'.Style'),lnstylewid)
*  lcOldStyle = LEFT(EVAL(lcTempOrd+'.Style'),lnstylewid)
IF lcOldStyle # LEFT(EVAL(lcTempOrd+'.Style'),lnstylewid) + EVAL(lcTempOrd+'.Order')
  lcOldStyle = LEFT(EVAL(lcTempOrd+'.Style'),lnstylewid) + EVAL(lcTempOrd+'.Order')
*B606808,1 BWA 12/31/2002.[END]

  =lfFillArry('NOTEPAD','laStyNotes',6)
  =lfFillArry('NOTELABEL','laLabelCod',2)
ENDIF

RETURN ''
*--End of lfGetArray.

*:*************************************************************
*: Name      : lfFillArry
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/10/2002
*: Purpose   : Fill the array of style notepad & label code template
*:*************************************************************
*: Calls     : gfItemMask
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : lfFillArry()
*:*************************************************************
*
FUNCTION lfFillArry
PARAMETERS lcAlias,lcArray,lnLen

PRIVATE lnI, lnCount, lcArrayCnt
lnMemoSet = SET('MEMOWIDTH')
SET MEMOWIDTH TO 97

lnCount = 0
*B606808,1 BWA 12/31/2002 Fix the bug of printing the first 6 lines only.[START]
IF lcAlias = 'NOTEPAD'
  FOR lnI = 1 TO 6
    lnCount = lnCount + 1
    lcArrayCnt = lcArray + '[lnCount]'
    &lcArrayCnt. = MLINE(EVALUATE(lcAlias+'.MNOTES'),lnI)
  ENDFOR
ELSE
*B606808,1 BWA 12/31/2002.[END]

  FOR lnI = 1 TO MEMLINES(EVALUATE(lcAlias+'.MNOTES'))
    IF !EMPTY(MLINE(EVALUATE(lcAlias+'.MNOTES'),lnI))
      lnCount = lnCount + 1
      lcArrayCnt = lcArray + '[lnCount]'
      &lcArrayCnt. = MLINE(EVALUATE(lcAlias+'.MNOTES'),lnI)
      IF lnCount = lnLen
        EXIT
      ENDIF
    ENDIF
  ENDFOR
ENDIF

SET MEMOWIDTH TO lnMemoSet
RETURN ''
*--End of lfFillArry.
*:*************************************************************
*: Name      : lfGetColor
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/10/2002
*: Purpose   : Get the color information.
*:*************************************************************
*: Calls     : gfItemMask
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : lfGetColor()
*:*************************************************************
*
FUNCTION lfGetColor
PARAMETERS lcDummy

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

RETURN ''
*--End of lfGetColor.
*:*************************************************************