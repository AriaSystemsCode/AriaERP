*:***************************************************************************
*: Program file  : MFPRCTCK.PRG
*: Program desc. : Print Cutting Tickets for Charies Kirk
*: For Report    : MFPRCTCK.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C200283,1
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*: Modifications :
*: B606854,1 AMH 02/25/2003 Print 20 lies from the style notepad.
*:***************************************************************************

PRIVATE lnAlias
lnAlias = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ELSE
  SELECT ORDHDR
  SET RELATION OFF INTO CUSTOMER
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ELSE
  SELECT CUTPICK
  SET RELATION OFF INTO ORDLINE
ENDIF
IF !USED('BOMVAR')
  =gfOpenFile(gcDataDir+'BOMVAR','BOMVAR','SH')
ELSE
  SELECT ORDLINE
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
SELECT ORDHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
SELECT CUTPICK
SET RELATION TO 'O'+ORDER+CORDLINE INTO ORDLINE ADDITIVE
SELECT ORDLINE
SET RELATION TO 'SO'+ORDER+STR(LINENO,6) INTO BOMVAR ADDITIVE
SELECT BOMVAR
SET RELATION TO CDSGNCODE INTO ICDESIGN ADDITIVE
SELECT CUTTKTH
SET RELATION TO 'F'+STYLE INTO NOTEPAD ADDITIVE
SELECT STYLE
SET RELATION TO 'T'+LABEL_CODE INTO NOTELABEL ADDITIVE
SELECT (lnAlias)
*-- End Of Option Program.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 02/05/2002
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor
PARAMETERS lcDummy

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
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

*!*************************************************************
*! Name      : lfGetArray
*! Developer : Ahmed Maher
*! Date      : 02/05/2002
*! Purpose   : Fill the array of style notepad & label code template
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetArray()
*!*************************************************************
*
FUNCTION lfGetArray
PARAMETERS lcDummy

*B606854,1 AMH Print 20 lies from the style notepad [Start]
*DECLARE laStyNotes[6]
DECLARE laStyNotes[26]
*B606854,1 AMH [End]

DECLARE laLabelCod[3]
STORE SPACE(0) TO laStyNotes,laLabelCod

*B606854,1 AMH Print 20 lies from the style notepad [Start]
*=lfFillArry('NOTEPAD','laStyNotes',6)
=lfFillArry('NOTEPAD','laStyNotes',26)
*B606854,1 AMH [End]

=lfFillArry('NOTELABEL','laLabelCod',3)
RETURN ''
*--End of lfGetArray.

*!*************************************************************
*! Name      : lfFillArry
*! Developer : Ahmed Maher
*! Date      : 02/05/2002
*! Purpose   : Fill the array of style notepad & label code template
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfFillArry()
*!*************************************************************
*
FUNCTION lfFillArry
PARAMETERS lcAlias,lcArray,lnLen

PRIVATE lnI, lnCount, lcArrayCnt, lnMemoSet
lnMemoSet = SET('MEMOWIDTH')
SET MEMOWIDTH TO 97
lnCount = 0
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
SET MEMOWIDTH TO lnMemoSet
RETURN ''
*--End of lfFillArry.