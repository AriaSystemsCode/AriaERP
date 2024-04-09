*:************************************************************************
*: PROGRAM   : ALPKTKF2.PRG (C037972)
*: DESC.     : Customized Sales Order Allocation Picking Ticket Form For FRE20
*: System    : Aria Apparel System    (A27)
*: Module    : SALES ORDER ALLOCATION (AL)
*: Developer : Nader Nabil        - (NNA)
*: DATE      : 05/21/2004
*:************************************************************************
*: Calls : 
*:         FUNCTION  : lfPrnStrn , lfGetLoc , lfContuLoc , lfEvalSegs.
*:         --------------------------------------------------------------
*:         PROCEDURE : lpOldFoter , lpNewFoter , lpPrtSku.
*:************************************************************************
*:Modifications:
*: B124559,1 NNA 09/23/2004 Fix bug that the size Pieces should be distributed on the Prepack 
*: B124559,1 NNA            Sizes not to Multiply with these sizes as it now
*:************************************************************************

*--Reopen the scale file to get the size scale from it.
IF !USED(lcScaleFil)
  = gfOpenFile(gcDataDir+'SCALE' ,gcDataDir+'SCALE', 'SH', @lcScaleFil, .T.)
ENDIF
DIMENSION laScale[64]		&& array to Hold the Scale Sizes for Every Line
STORE '' TO lcSizDsc
STORE 0  TO lnSclRec , lnSizPos , laScale

*--Function to creat Temp. file.
=lfCreatTmp()

SELECT (lcTmpOrdL)
SCAN
  WAIT WINDOW 'Selecting Pick Ticket for printing ...' + &lcTmpOrdL..PikTkt NOWAIT
  lcHoldScal = SPACE(0)    &&Variable hold the accumulated Prepack from the scale file.
  lnChkText = 0            && to print [Case Pack deatials] For every line has 'P' in the Scale
  SCATTER MEMVAR MEMO

  SELECT (lcOrdlinFr)
  M.SortLines = &lcOrdlinFr..SortLines + 1
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE llPrnPp   WITH .F.

  *-- if the &lcTmpOrdL.SCALE have 'P' in the second Chr. and user want to print [Case Pack Details]

  IF SUBSTR(&lcTmpOrdL..Scale,2,1) = 'P' .AND. llRpPCaspk
    lnSclRec = RECNO('SCALE')
    =lfGetPpakQ()
  ENDIF
ENDSCAN
SELECT (lcTmpOrdL)
USE
IF !USED(lcOrdlinFr)
  = gfOpenFile(gcWorkDir+lcOrdlinFr , gcWorkDir+lcTmpOrdL , 'SH', @lcTmpOrdL, .T.)
ELSE
  SELECT (lcOrdlinFr)
  USE
  = gfOpenFile(gcWorkDir+lcOrdlinFr , gcWorkDir+lcOrdlinFr , 'SH', @lcTmpOrdL, .T.)
ENDIF

SELECT (lcTmpOrdL)
LOCATE
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*--IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF 

SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Nader Nabil (NNA)
*! Date          : 05/11/2004
*! Purpose   : Function to Creat the temp. File.
*!*************************************************************
*! Called from : ARPINVFR.PRG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE ALIAS
ALIAS = SELECT(0)

*--Creat table for the trasactions.
SELECT (lcTmpOrdL)
=AFIELDS(laFileStru)

=lfAddField("laFileStru", "SizDsc" , "C",5  ,0)

FOR lnCrtTmp = 1 TO 26
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laFileStru", "CpQty"  + lcNumSiz ,"N",5,0)
  =lfAddField("laFileStru", "SclSiz" + lcNumSiz ,"C",5,0)
ENDFOR

=lfAddField("laFileStru", "lnPrnText" , "N",1  ,0)
=lfAddField("laFileStru", "SortLines" , "N",6  ,0)
=lfAddField("laFileStru", "llPrnPp" , "L",1  ,0)

CREATE TABLE (gcWorkDir+lcOrdlinFr) FROM ARRAY laFileStru
INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) TAG (lcOrdlinFr) OF (gcWorkDir+lcOrdlinFr)
IF gcDevice <> 'SCREEN'
  INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE
ENDIF  

SELECT(ALIAS)
RETURN ''

*-- End of lfCreatTmp.

*!*************************************************************
*! Name      : lfAddField
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!***************************************************************
*! Name      : lfGetPpakQ
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Fill array with the sizes that matches the Scale that have 'P'
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfGetPpakQ()
*!*************************************************************
FUNCTION lfGetPpakQ
PRIVATE lcScalRec
SELECT (lcTmpOrdL)
FOR lnScale = 1 To SCALE.CNT
  lcScale = ALLTRIM(STR(lnscale))
  IF(&lcTmpOrdL..QTY&lcScale > 0)
    GOTO lnSclRec IN SCALE
    lcSizDsc = ALLTRIM(SCALE.SZ&lcScale)          && Get the Scale Size Name i.e (A12 ,15B,Siz3)
    = lfSizNo(lcSizDsc)
    lnPpNo = 1 								 && Variable to hold the Prepack No#

    SELECT SCALE
    =SEEK('P' + SUBSTR(&lcTmpOrdL..Scale,1,1))
    STORE SPACE(3) TO lcScalchk , lcHoldPrpk
    SCAN REST WHILE TYPE + SCALE + PREPAK = "P"
      IF SUBSTR(SCALE,2,1) <> "P" )
        IF lnPpNo = lnSizPos				 && if the Prepak No# Matches the Size No# (First to first..etc)
          IF !(ALLTRIM(SCALE.PREPAK) $ lcHoldScal)
            lcHoldScal = IIF(EMPTY(lcHoldScal) , ALLTRIM(SCALE.PREPAK) , ALLTRIM(lcHoldScal) + ","+  ALLTRIM(SCALE.PREPAK) )
            lcHoldPrpk = ALLTRIM(SCALE.PREPAK)

            =lfgetpack()

            EXIT
          ENDIF
        ENDIF
        lnPpNo = lnPpNo + 1
      ENDIF
    ENDSCAN
  ELSE
    SELECT SCALE
    LOCATE
      SCAN FOR TYPE + LEFT(SCALE,1) + PREPAK = "P"
        IF (LEFT(SCALE,1) == SUBSTR(&lcTmpOrdL..Scale,1,1) AND SUBSTR(SCALE,2,1) <> "P" ) AND !(ALLTRIM(SCALE.PREPAK) $ lcHoldScal)
          lcHoldScal = IIF(EMPTY(lcHoldScal) , ALLTRIM(SCALE.PREPAK) , ALLTRIM(lcHoldScal) + ","+  ALLTRIM(SCALE.PREPAK) )
          EXIT
        ENDIF
      ENDSCAN
  ENDIF
ENDFOR

*--End of lfGetPpakQ.
*!***************************************************************
*! Name      : lfSizNo
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Fill array with the sizes and get its Position
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfSizNo()
*!*************************************************************
FUNCTION lfSizNo
PARAMETER SizNam
SELECT SCALE
=SEEK('S' + SUBSTR(&lcTmpOrdL..Scale,1,1))
lnlvl = 0
SCAN REST WHILE TYPE + SCALE + PREPAK = 'S' + SUBSTR(&lcTmpOrdL..Scale,1,1) FOR ;
          SUBSTR(SCALE.SCALE,2,1) = 'P'
  FOR I = 1 TO SCALE.CNT 
    N  = ALLTRIM(STR(I))
    laScale [I + lnLvl] = ALLTRIM(SCALE.SZ&N)	&& Add to Scale Array to be Accumulated
  NEXT
  lnLvl = lnLvl + ( I - 1 )
ENDSCAN
lnSizPos =  ASCAN(laScale , SizNam)				&& the Size No# 
RETURN lnSizPos
*-- End of lfSizNo.

*!***************************************************************
*! Name      : lfgetpack
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Fill the Temp File CpQty with (Pp_Amount * Temp File Qty For a Size)
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfgetpack()
*!*************************************************************
FUNCTION lfgetpack

SELECT (lcTmpOrdL)
SCATTER MEMVAR MEMO                      &&Same line of the lcTmpOrdL.
SELECT (lcOrdlinFr)
M.SortLines = &lcOrdlinFr..SortLines + 1
APPEND BLANK
GATHER MEMVAR MEMO
REPLACE llPrnPp WITH .T.
REPLACE SizDsc  WITH lcSizDsc

lnChkText = lnChkText + 1
IF(lnChkText = 1)
  REPLACE lnPrnText WITH 1
ENDIF
    
SELECT SCALE
LOCATE
lnFullfld = 0

*B124559,1 NNA 09/23/2004 (Begin) Sum the PPtot for the all Prepacks that related to the 'P' size
*B124559,1 NNA             to get the Retio that I'll distribute Depending on it
lnPackTot = 0
lnRatio   = 0
SCAN FOR TYPE + LEFT(SCALE,1) + PREPAK = "P" + SUBSTR(&lcTmpOrdL..Scale,1,1) + lcHoldPrpk
  IF SUBSTR(SCALE,2,1) <> "P"
    lnPackTot = lnPackTot + SCALE.PPTOT
  ENDIF  
ENDSCAN
lnRatio = (&lcOrdlinFr..Qty&lcScale) / lnPackTot
LOCATE
* B124559,1 NNA (End)

SCAN FOR TYPE + LEFT(SCALE,1) + PREPAK = "P" + SUBSTR(&lcTmpOrdL..Scale,1,1) + lcHoldPrpk
  IF SUBSTR(SCALE,2,1) <> "P"
    FOR lnSize = 1 To 8
      lcSize = ALLTRIM(STR(lnSize))
      IF(SCALE.Pp&lcSize > 0)
        lnFullfld = lnFullfld + 1      && to shift the (Case Pack Details) to print only the none Zero Qty.
        lcPrpck = ALLTRIM(STR(lnFullfld))

        *B124559,1 NNA 09/23/2004 (Begin) Get the 'Case Pack Details Qty.' by get the (Prepack 
        *B124559,1 NNA                    Sizes * Ratio)
        *REPLACE &lcOrdlinFr..CpQty&lcPrpck WITH SCALE.Pp&lcSize * &lcOrdlinFr..Qty&lcScale
        REPLACE &lcOrdlinFr..CpQty&lcPrpck WITH SCALE.Pp&lcSize * lnRatio
        * B124559,1 NNA (End)

        =SEEK("S" + SCALE.SCALE , lcScaleFil)
        REPLACE &lcOrdlinFr..SclSiz&lcPrpck WITH &lcScaleFil..Sz&lcSize
      ENDIF
    ENDFOR
    IF lnFullfld > 26
      EXIT
    ENDIF
  ENDIF
ENDSCAN

*-- End of lfgetpack.
*!***************************************************************
*! Name      : lfPrtNotes
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Function to Evaluate Notes To be Printed 
*! Returns   : Printed Notes
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example   : =lfPrtNotes()
*!*************************************************************
FUNCTION lfPrtNotes
PARAMETER lcReturn

DO CASE
  CASE cGrupDetal<>'D' .AND. llRpOrdNot .AND. SEEK('B'+Order,'NOTEPAD') .AND. ;
       !EMPTY(ALLT(NOTEPAD.mNotes))
    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Order Note Pad'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)

  CASE cGrupDetal='D' .AND. llRpOrdLNt .AND. !EMPTY(ALLT(ORDLINE.Note_Mem))
    lcNotesTtl = 'Line Note Pad'
    lcNotes    = ALLTRIM(ORDLINE.Note_Mem)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*-- End of lfPrtNotes.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the PIKTKT file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup
PARAMETER lcReturn
llEndGroup = .T.

*--IF The Divice is not Screen
IF gcDevice <> 'SCREEN'
  REPLACE PIKTKT.PrtFlag WITH 'P'
ENDIF    && End of IF

RETURN llEndGroup

