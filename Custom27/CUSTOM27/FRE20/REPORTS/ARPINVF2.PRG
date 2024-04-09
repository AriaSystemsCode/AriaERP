*:***************************************************************************
*: Program file  : ARPINVF2.PRG
*: Program desc. : Customized Invoice Form for OPERATION MANAGEMENT SERVICES [FRE20]
*: Date          : 05/11/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Nader Nabil Abd Elmonam (NNA)
*: Tracking Job Number : C037971
*:**************************************************************************
*: Calls : FUNCTIONS  : lfOptMsg   , lfwOldVal  , lfvAccount , lfInvSet
*:		    		  : lfvInvNo   , lfAdrShift , lfSolSpAdr , lfEndGroup  
*:		    		  : lfvSkuPck  , lfCalcTax  , lfSuprSKU  , lfPrtNotes 
*:		    		  : lfPrtLNotes, lfPrtINotes, lfSclSze   , lfEndLine
*:		    		  : lfEndGrRtn , lfGetPath  , lfGetFrx   , lfPrtSku
*:		    		  : lfPrtCasPK
*:**************************************************************************
*: Example : DO ARPINVFR
*:**************************************************************************
*:Modification:
*:B124559,1 NNA 09/23/2004 Fix bug that the size Pieces should be distributed on the Prepack 
*:B124559,1 NNA            Sizes not to Multiply with these sizes as it now
*:C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.
*:**************************************************************************

*--Reopen the scale file to get the size scale from it.
IF !USED(lcScaleFil)
  = gfOpenFile(gcDataDir+'SCALE' ,gcDataDir+'SCALE', 'SH', @lcScaleFil, .T.)
ENDIF

*C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.[START]
STORE '' TO lcUpsNo
STORE IIF(USED(lcArupsshp) , .T. , .F.) TO llFondFil
IF !USED(lcArupsshp)
  llFondFil = gfOpenFile(gcDataDir+'ARUPSSHP' ,gcDataDir+'ARUPSSHP', 'SH', @lcArupsshp, .T.)
ENDIF
*C038785,1 BWA 01/16/2005.[END]

*--Function to creat Temp. file.
STORE '' TO lcSizDsc
STORE 0  TO lnSclRec , lnSizPos
STORE .F. TO llEndLines
=lfCreatTmp()

SELECT INVHDR
SET SKIP TO
SCAN FOR &lcRpExp
  WAIT WINDOW 'Selecting Invoices for printing ...' + INVHDR.invoice NOWAIT
  *lcHoldScal = SPACE(0)    &&Variable hold the accumulated Prepack from the scale file.
  lnChkText = 0

  SELECT INVLINE
  SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
    lnChkText = 0
    lcHoldScal = SPACE(0)    &&Variable hold the accumulated Prepack from the scale file.   
    SCATTER MEMVAR MEMO
    SELECT (lcInvlinFr)
    M.SortLines = &lcInvlinFr..SortLines + 1
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE llPrnPp   WITH .F.

    *-- if the INVLINE.SCALE have 'P' in the second Chr. and user want to print [Case Pack Details]
    IF SUBSTR(InvLine.Scale,2,1) = 'P' .AND. llRpPCaspk
      lnSclRec = RECNO('SCALE')
      =lfGetPpakQ()
    ENDIF
  ENDSCAN
ENDSCAN
               *-- Section break the relatoins --*
SELECT INVHDR
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO INVLINE
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT INVLINE
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*

             *-- Section reCreate the relations --*
SELECT INVHDR
IF llPrntInst .OR. llRpInvNot
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.INVOICE , '*') INTO &lcInvlinFr ADDITIVE
ELSE
  SET RELATION TO INVHDR.INVOICE INTO &lcInvlinFr ADDITIVE
ENDIF

SELECT (lcInvlinFr)
SET RELATION TO EVAL(lcInvlinFr+'.STYLE') INTO STYLE ADDITIVE
SET RELATION TO "S" + EVAL(lcInvlinFr+'.Account') + EVAL(lcInvlinFr+'.STYLE') INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account , 'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , (lcInvlinFr)
ELSE
  SET SKIP TO (lcInvlinFr)
ENDIF
SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR
                       *-- End of the Program --*
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
SELECT InvLine
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

CREATE TABLE (gcWorkDir+lcInvlinFr) FROM ARRAY laFileStru
INDEX ON INVOICE + STR(LineNo,6) + STR(SortLines,6) TAG CasPak OF (gcWorkDir+lcInvlinFr)

SELECT(ALIAS)
RETURN ''

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfAddField
*! Developer : Nader Nabil (NNA)
*! Date      : 05/11/2004
*! Purpose   : Add fields to the array of file structer.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : None
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
SELECT INVLINE
FOR lnScale = 1 To SCALE.CNT
  lcScale = ALLTRIM(STR(lnscale))
  IF(InvLine.QTY&lcScale > 0)
    GOTO lnSclRec IN SCALE
    lcSizDsc = ALLTRIM(SCALE.SZ&lcScale)          && Get the Scale Size Name i.e (A12 ,15B,Siz3)

    = lfSizNo(lcSizDsc)
    lnPpNo = 1
    SELECT SCALE
    =SEEK('P' + SUBSTR(InvLine.Scale,1,1))
    STORE SPACE(3) TO lcScalchk , lcHoldPrpk

    SCAN REST WHILE TYPE + SCALE + PREPAK = 'P'
      IF SUBSTR(SCALE,2,1) <> 'P' )
        IF lnPpNo = lnSizPos
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
        IF (LEFT(SCALE,1) == SUBSTR(InvLine.Scale,1,1) AND SUBSTR(SCALE,2,1) <> "P" ) AND !(ALLTRIM(SCALE.PREPAK) $ lcHoldScal)
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
=SEEK('S' + SUBSTR(INVLINE.SCALE,1,1))
lnlvl = 0
SCAN REST WHILE TYPE + SCALE + PREPAK = 'S' + SUBSTR(INVLINE.SCALE,1,1) FOR ;
          SUBSTR(SCALE.SCALE,2,1) = 'P'
  FOR I = 1 TO SCALE.CNT 
    N  = ALLTRIM(STR(I))
    laScale [I + lnLvl] = ALLTRIM(SCALE.SZ&N)
  NEXT
  lnLvl = lnLvl + ( I - 1 )
ENDSCAN
lnSizPos =  ASCAN(laScale , SizNam)
RETURN lnSizPos
*-- End of lfSizArray.

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

SELECT INVLINE
SCATTER MEMVAR MEMO                      &&Same line of the invline.
SELECT (lcInvlinFr)
M.SortLines = &lcInvlinFr..SortLines + 1
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
SCAN FOR TYPE + LEFT(SCALE,1) + PREPAK = "P" + SUBSTR(InvLine.Scale,1,1) + lcHoldPrpk
  IF SUBSTR(SCALE,2,1) <> "P"
    lnPackTot = lnPackTot + SCALE.PPTOT
  ENDIF  
ENDSCAN
lnRatio = (&lcInvlinFr..Qty&lcScale) / lnPackTot
LOCATE
* B124559,1 NNA (End)

SCAN FOR TYPE + LEFT(SCALE , 1) + PREPAK = "P" + SUBSTR(InvLine.Scale,1,1) + lcHoldPrpk
  IF SUBSTR(SCALE,2,1) <> "P"
    FOR lnSize = 1 To 8
      lcSize = ALLTRIM(STR(lnSize))
      IF(SCALE.Pp&lcSize > 0)
        lnFullfld = lnFullfld + 1      
        lcPrpck = ALLTRIM(STR(lnFullfld))

        *B124559,1 NNA 09/23/2004 (Begin) Get the 'Case Pack Details Qty.' by get the (Prepack 
        *B124559,1 NNA                    Sizes * Ratio)
        *REPLACE &lcInvlinFr..CpQty&lcPrpck WITH SCALE.Pp&lcSize * &lcInvlinFr..Qty&lcScale
        REPLACE &lcInvlinFr..CpQty&lcPrpck WITH SCALE.Pp&lcSize * lnRatio
        * B124559,1 NNA (End)

        =SEEK("S" + SCALE.SCALE , lcScaleFil)
        REPLACE &lcInvlinFr..SclSiz&lcPrpck WITH &lcScaleFil..Sz&lcSize
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

*C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.[START]
STORE '' TO lcUpsNo
IF llFondFil AND EVAL(lcTmpDbt+'.cfile_num')='2'
  PRIVATE lcAlias
  STORE '' TO lcUpsNo
  IF !EMPTY(INVHDR.PIKTKT) .AND. SEEK(INVHDR.PIKTKT , lcArupsshp )
    lcAlias = SELECT(0)
    SELECT (lcArupsshp)
    SCAN REST WHILE PIKTKT = INVHDR.PIKTKT
      lcUpsNo = IIF(EMPTY(lcUpsNo) , "" , SPACE(30) + lcUpsNo ) + &lcArupsshp..CTRACK_NO + CHR(13) + CHR(10)
    ENDSCAN
    SELECT(lcAlias)
  ENDIF
  lcUpsNo = "Ups Track Number:   " + IIF(EMPTY(lcUpsNo) , lcUpsNo , lcUpsNo + REPLICATE('=',50))
ENDIF
*C038785,1 BWA 01/16/2005.[END]

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('C' + INVHDR.Invoice , 'NOTEPAD')

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)

  CASE llRpInvLNt .AND. !EMPTY(&lcInvlinFr..note_mem) .AND. LEFT(ALLTRIM(STRTRAN(&lcInvlinFr..note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(&lcInvlinFr..note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

*C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.[START]
lcNotes = lcNotes + IIF(!EMPTY(lcNotes) , CHR(13) + CHR(10) + REPLICATE('=',50) + CHR(13) + CHR(10) + lcUpsNo , lcUpsNo)
*C038785,1 BWA 01/16/2005.[END]

RETURN !EMPTY(lcNotesTtl)

*-- End of lfPrtNotes.