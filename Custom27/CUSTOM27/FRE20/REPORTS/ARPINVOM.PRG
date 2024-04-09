*:***************************************************************************
*: Program file  : ARPINVOM.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR OPERATIONS MANAGEMENT.
*: Date          : 08/15/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102414
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVOM
*:***************************************************************************
*: Modifications ..
*:***************************************************************************
*:B605977,1 Fix bug in Page no.
*:B606677,1 RAE 12/22/2002 Align the invoice number - in the FRX -, it was 
*:                         printed too high on the top margin.
*:B120198,1 MMM 23/10/2003 Fix Bug of Printing Wrong Page numbers(Page 1 of 2) when the invoice  
*:                         Lines more Than 3 and it still one page [Fix in Prg and FRX].  
*:C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.
*:***************************************************************************
                  *--Section to get the color length --*

*C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.[START]
STORE '' TO lcUpsNo
STORE IIF(USED(lcArupsshp) , .T. , .F.) TO llFondFOpn
IF !USED(lcArupsshp)
  llFondFOpn = gfOpenFile(gcDataDir+'ARUPSSHP' ,gcDataDir+'ARUPSSHP', 'SH', @lcArupsshp, .T.)
ENDIF
*C038785,1 BWA 01/16/2005.[END]

*--THE COLOR LENGTH
STORE 0 TO lnClrLnOm , lnClrPosOm
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnOm  = LEN(laItemSeg[lnCount,3])
    lnClrPosOm = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
                  *--Section to get the color length --*

                  *--Section to print the wanted text --*
lcPhoneOm = TRANSFORM(lcCompPhon , lcPhonPict)
FOR lnLop = 1 TO 6
  IF lcPhoneOm = lacompadd[lnLop]
    lacompadd[lnLop] = "Tel#  : " + lcPhoneOm
    EXIT
  ENDIF
ENDFOR
                  *--Section to print wanted text --*

                  *--Section open the files wanted --*

*-- Open the INVLINE.DBF again to get the count of the lines of the invoice.
IF !USED(lcInvLinOm)
  =gfOpenFile(gcDataDir+"InvLine","InvLine",'SH', @lcInvLinOm, .T.)
ENDIF

IF !USED(lcWarHosOm)
  = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHosOm, .T.)
ENDIF

SELECT INVHDR
SET RELATION TO CWARECODE INTO (lcWarHosOm) ADDITIVE

*C038785,1 BWA 01/16/2005 Add UPS Tracking numbers after the notepad.[START]
SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR
*C038785,1 BWA 01/16/2005.[END]

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfWarhos
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/15/2001
*! Purpose   : Function to Get the Warehous Address.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfWarhos
PARAMETER lfWarhos

DECLARE laCmAdr[5,1]
laCmAdr = ''           && Array to hold the warehous address.
lcWarAddOm = ''

*-- Get the WareHouse addresses
lcWarAddOm = INVHDR.CWARECODE
laCmAdr[1] = gfGetAdr(lcWarHosOm , '' , '' , '' , 1)
laCmAdr[2] = gfGetAdr(lcWarHosOm , '' , '' , '' , 2)
laCmAdr[3] = gfGetAdr(lcWarHosOm , '' , '' , '' , 3)
laCmAdr[4] = gfGetAdr(lcWarHosOm , '' , '' , '' , 4)
laCmAdr[5] = gfGetAdr(lcWarHosOm , '' , '' , '' , 5)
=lfAdrShift('laCmAdr')

FOR lnLopWrh = 1 To 5
  IF !EMPTY(laCmAdr[lnLopWrh])
    lcWarAddOm = lcWarAddOm + SPACE(2) + "-" + SPACE(2) + laCmAdr[lnLopWrh]
  ENDIF
ENDFOR

RETURN ""
*-- End of lfWarhos.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/15/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : lfWarhos()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.
*!*************************************************************
*! Name      : lfPagNoOm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/15/2001
*! Purpose   : Function to get the number of the pages.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPagNoOm
PARAMETER lcPageOm

*-lnContLin >> Hold the number of the invoice lineno in the invline
STORE 0 TO lnContLin , lnPageOm2

*-- The function which we get the last recno for the invoice.
=lfRecCont()

*--The page has 4 records equal 21 line in the notepad.
*-- Case the page has 4 records.

IF MOD(lnContLin,4) = 0
  IF llRpInvNot
    LnNotCont = IIF(SEEK("C" + INVHDR.INVOICE , 'NOTEPAD') , MEMLINES(NOTEPAD.MNOTES) , 0)
    
    *B120198,1 MMM 23/10/2003 Fix Bug of Printing Wrong Page numbers(Page 1 of 2) when the invoice  
    *                         Lines more Than 3 and it still one page. [Start]  
        
    *LnNotCont = LnNotCont - 4
    *lcCheck   = STR(MOD(LnNotCont/21,21),5,2)
    *lnCut     = ATC('.',STR(MOD(LnNotCont/21,21),5,2))
    *lcCheck   = SUBSTR(STR(MOD(LnNotCont/21,21),5,2),lnCut,3)

    *B605977,1 Always force empty notepad to be zero. [Begin]
    *IF VAL(lcCheck) > .81 OR VAL(lcCheck) = 0  
    *IF LnNotCont <= 0
      *LnNotCont = 0
    *B605977,1 Always force empty notepad to be zero. [End]
      
      *LnNotCont = CEILING(MOD(LnNotCont/21,21)/1) + 1
    *ELSE
      *LnNotCont = CEILING(MOD(LnNotCont/21,21)/1)
    *ENDIF
    lnPageOm2  = CEILING(lnContLin / 4) + LnNotCont
     
  *ELSE
    *lnPageOm2   = CEILING(lnContLin / 4)
  *ENDIF
    
    IF LnNotCont <> 0 
      LnNotCont = LnNotCont - 4
      lcCheck   = STR(MOD(LnNotCont/21,21),5,2)
      lnCut     = ATC('.',STR(MOD(LnNotCont/21,21),5,2))
      lcCheck   = SUBSTR(STR(MOD(LnNotCont/21,21),5,2),lnCut,3)

      IF LnNotCont < 0
        LnNotCont = 0
        LnNotCont = CEILING(MOD(LnNotCont/21,21)/1) + 1
      ELSE
        LnNotCont = CEILING(MOD(LnNotCont/21,21)/1)
      ENDIF
    ENDIF
    
     lnPageOm2  = CEILING(lnContLin / 4) +IIF(LnNotCont=0,IIF(INVHDR.Discount<>0,1,0),LnNotCont) 
     
  ELSE
     lnPageOm2   = CEILING(lnContLin / 4) + IIF(INVHDR.Discount<>0,1,0)
  ENDIF
   *B120198,1 MMM. [End]
ELSE
  *-- Case the page has more or less than 4 records.
  IF llRpInvNot
    LnNotCont = IIF(SEEK("C" + INVHDR.INVOICE , 'NOTEPAD') , MEMLINES(NOTEPAD.MNOTES) , 0)
    lnRstPage = MOD(lnContLin , 4)
    lnRstPage = IIF(lnRstPage = 1 , 5 , IIF(lnRstPage = 2 , 9 , IIF(lnRstPage = 3 , 13 , 0 )))
    IF CEILING(lncontlin/4) > 1
      lnAddRow = (CEILING(lncontlin/4) - 1) * 21
      lnRstPage = lnRstPage + lnAddRow
    ENDIF
    LnNotCont = LnNotCont + lnRstPage
    lcCheck   = STR(MOD(LnNotCont/21,21),5,2)
    lnCut     = ATC('.',STR(MOD(LnNotCont/21,21),5,2))
    lcCheck   = SUBSTR(STR(MOD(LnNotCont/21,21),5,2),lnCut,3)
    IF VAL(lcCheck) > .81
      lnPageOm2 = CEILING(MOD(LnNotCont/21,21)/1) + 1
    ELSE
      lnPageOm2 = CEILING(MOD(LnNotCont/21,21)/1)
    ENDIF
  ELSE
    lnPageOm2 = CEILING(lnContLin / 4)
  ENDIF
ENDIF

RETURN ""
*-- End of lfPagNoOm.
*!*************************************************************
*! Name        : lfRecCont
*! Developer   : BASSEM RAFAAT ERNEST (BWA)
*! Date        : 08/15/2001
*! Purpose     : To get the Record count of the InvLine file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : lfRecCont()
*!*************************************************************
FUNCTION lfRecCont
PRIVATE lcAlias

*-- Case get the count of the invoice lines.
lnContLin = 0
lcAlias = ALIAS()

SELECT (lcInvLinOm)
=SEEK(INVHDR.INVOICE , lcInvLinOm )
SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
  lnContLin = lnContLin + 1
ENDSCAN

SELECT (lcAlias)

RETURN ''
*-- End of lfRecNo.
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
IF llFondFOpn AND EVAL(lcTmpDbt+'.cfile_num')='2'
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