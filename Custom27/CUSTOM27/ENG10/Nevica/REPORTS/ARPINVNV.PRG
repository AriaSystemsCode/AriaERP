*:***************************************************************************
*: Program file  : ARPINVNV.PRG
*: Program desc. : Customized Invoice Form Nevica (ENGLAND)
*: Date          : 08/19/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Tracking Job Number: C200379
*:***************************************************************************
*: Example : DO ARPINVNV
*:***************************************************************************
*B606512,1 SSE 10/01/2002 Fix some bugs in Nevica Invoice.
*B608000,1 TMI 03/08/2007 get the note from the invline file 
*C201002,1 NNA 05/14/2008 [T20080428.0004] new modification to Nevica's Form (NV) to split
*C201002,1 NNA            every invoice into two separate forms if user choose split option
*:***************************************************************************

*-- Check for records that meet criteria.
SELECT InvHdr
LOCATE FOR &lcRpExp
SET SKIP TO 

IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.            && used in @ SAY
  SET DEVICE TO SCREEN
  RETURN
ENDIF
PRIVATE lcNewRpExp
SET DEVICE TO PRINT

PRIVATE lcTaxRefr , lnStyleLen , lnStylePos , lnClrLen , lnClrPos
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
lnTax_Rate = gfGetMemVar('M_TAX_RATE')

*-- Get Style Major & Non Major
STORE 0 TO lnStyleLen , lnStylePos , lnClrLen , lnClrPos
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'   && Free (Style)
    lnStyleLen = LEN(laItemSeg[lnCount,3])
    lnStylePos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='C'   && Color
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

DO lpCreateTb    && Procedure to create all temp files needed.

PRIVATE lcInvoice , lnTotQty , lnStyWgt , lnPrice , lnInvHdPos
lnInvHdPos = lfItmPos('INVHDR.INVOICE')
lcNewRpExp = IIF(USED(laOGVrFlt[lnInvHdPos,6]) AND RECCOUNT(laOGVrFlt[lnInvHdPos,6])>0 , ;
             ".T.",IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp))
SELECT IIF(USED(laOGVrFlt[lnInvHdPos,6]) AND RECCOUNT(laOGVrFlt[lnInvHdPos,6])>0 ,(laOGVrFlt[lnInvHdPos,6]),'InvHdr')
SCAN FOR &lcNewRpExp
  lcInvoice = Invoice
  =SEEK(lcInvoice,'InvHdr')
  SELECT InvLine
  SCAN REST WHILE Invoice+STR(LineNo,6) = lcInvoice
    WAIT WINDOW 'Selecting Records For The Report ...' + lcInvoice NOWAIT
    SCATTER MEMVAR MEMO
    lnTotQty = TotQty
    lnStyWgt = Style.nStyWeight
    lnPrice  = Price
    
    SELECT(lcStylIndx)
    *m.cComCode = Style.cComCode
    APPEND BLANK
    GATHER MEMVAR MEMO
    
    *-- Add the notes lines.
    
    IF ALLTRIM(Customer.cCont_Code) == "FR"
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO
      
      *B606512,1 Change Flag field to NewFlag. [Begin]
      *REPLACE Flag     WITH "z"             ,;
      *        llNoTVA  WITH .F.             ,;
      *        cComCode WITH Style.cComCode  ,;
      *        Note_Mem WITH ''
      REPLACE NewFlag  WITH "z"             ,;
              llNoTVA  WITH .F.             ,;
              cComCode WITH Style.cComCode  ,;
              Note_Mem WITH ''
      *B606512,1 Change Flag field to NewFlag. [End]

      IF SEEK(InvLine.Invoice,lcTmpInvLn)
        SELECT(lcTmpInvLn)
        REPLACE TotQty   WITH TotQty + lnTotQty                ,;
                TotFrCom WITH TotFrCom + (lnStyWgt * lnTotQty) ,;
                TotFrVal WITH TotFrVal + (lnTotQty * lnPrice)
      ELSE
        SELECT(lcTmpInvLn)
        APPEND BLANK
        GATHER MEMVAR MEMO
        
        *B606512,1 Change Flag field to NewFlag. [Begin]
        *REPLACE Flag     WITH "z"                     ,;
        *        llNoTVA  WITH .F.                     ,;
        *        Note_Mem WITH ''                      ,;
        *        Style      WITH REPLICATE('z',19)     ,;
        *        TotFrCom   WITH lnStyWgt * lnTotqty   ,;
        *        TotFrVal   WITH lnTotqty * lnPrice    ,;
        *        TotQty     WITH lnTotQty              ,;
        *        cComCode   WITH 'zz'
        REPLACE NewFlag    WITH "z"                   ,;
                llNoTVA    WITH .F.                   ,;
                Note_Mem   WITH ''                    ,;
                Style      WITH REPLICATE('z',19)     ,;
                TotFrCom   WITH lnStyWgt * lnTotqty   ,;
                TotFrVal   WITH lnTotqty * lnPrice    ,;
                TotQty     WITH lnTotQty              ,;
                cComCode   WITH 'zz'
        *B606512,1 Change Flag field to NewFlag. [End]
                
      ENDIF
    ENDIF

    *-- Finally we need to add the color description.
    IF !SEEK(SUBSTR(Style,lnClrPos,lnClrLen), lcTmpColor)
      SELECT (lcTmpColor)
      APPEND BLANK
      REPLACE Color WITH SUBSTR(InvLine.Style,lnClrPos,lnClrLen) ;
              Desc  WITH ALLTRIM(gfCodDes(SUBSTR(InvLine.Style,lnClrPos,lnClrLen) , 'COLOR'))
    ENDIF
    
  ENDSCAN
ENDSCAN

*-- This scan to get the first line and last line after the new index.
SELECT IIF(USED(laOGVrFlt[lnInvHdPos,6]) AND RECCOUNT(laOGVrFlt[lnInvHdPos,6])>0 ,(laOGVrFlt[lnInvHdPos,6]),'InvHdr')
SCAN FOR &lcNewRpExp
  lcInvoice = Invoice
  =SEEK(lcInvoice,'InvHdr')
  =SEEK(lcInvoice,lcStylIndx)       && Get first record
  SELECT (lcStylIndx)
  *REPLACE lFirstSty WITH .T.
  SET ORDER TO TAG InvLine DESCENDING
  =SEEK(lcInvoice+' ')                  && Get last record
  REPLACE lEndLine WITH .T.
  REPLACE llNoTVA WITH .T.
  =lfAllNotes()
ENDSCAN

*-- This scan is made to add a flag whenever style major is changed.
SELECT (lcStylIndx)
SET ORDER TO TAG InvLine1
PRIVATE lcNewStyle
lcNewStyle = ''
SCAN
  IF !(Invoice+SUBSTR(Style,lnStylePos,lnStyleLen) == lcNewStyle)
    REPLACE lFirstSty WITH .T.
    lcNewStyle = Invoice+SUBSTR(Style,lnStylePos,lnStyleLen)
  ENDIF
ENDSCAN
APPEND FROM (gcWorkDir + lcTmpInvLn)

*C201002,1 NNA 09/25/2007 (BEGIN) if split invoice set to yes then call split function
IF llRpSplit
  =lfDoSplit()
ENDIF
*C201002,1 NNA (END)

*-- Section break the relations
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO Customer
SET RELATION OFF INTO OrdHdr
SET RELATION OFF INTO ConsInvL
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO Style
SET RELATION OFF INTO Spck_Lin

SELECT Style
SET RELATION OFF INTO Scale
*-- End Section break the relations --*

*-- Close the invline and open the temp. file with the invline name.
SELECT InvLine
*CLOSE INDEX
USE IN InvLine
USE IN (lcStylIndx)

*C201002,1 NNA 09/25/2007 (BEGIN) if split invoice set to yes then I'll close the standard files and use temp. tables with
*C201002,1 NNA             the same names as standard
IF llRpSplit
  SELECT (lcInvLine)
  CLOSE INDEX
  USE IN (lcInvLine)
  SELECT (lcInvHdr)
  CLOSE INDEX
  USE IN (lcInvHdr)
  USE IN InvHdr
  USE (gcWorkDir+lcInvLine) IN 0 ALIAS InvLine
  USE (gcWorkDir+lcInvHdr) IN 0 ALIAS InvHdr
  SELECT InvHdr
  SET INDEX TO (gcWorkDir+lcInvHdr+'.CDX')
  SET ORDER TO TAG (lcInvHdr)
ELSE
*C201002,1 NNA (END)

  USE (gcWorkDir+lcStylIndx) IN 0 ALIAS InvLine

*C201002,1 NNA 05/14/2008 (BEGIN) End of if statment
ENDIF
*C201002,1 NNA (END)

SELECT InvLine

*C201002,1 NNA 09/25/2007 (BEGIN) set index
IF llRpSplit
  SET INDEX TO (gcWorkDir+lcInvLine+'.CDX')
  SET ORDER TO TAG lcInvLine
ELSE
*C201002,1 NNA (END)

  SET INDEX TO (gcWorkDir+InvLine+'.CDX')
  SET ORDER TO TAG InvLine

*C201002,1 NNA 05/14/2008 (BEGIN) End of if statment
ENDIF
*C201002,1 NNA (END)


*-- Section Create the new relations --*
SELECT InvHdr

*C201002,1 NNA 05/14/2008 (BEGIN) set relation to split field
IF llRpSplit
  SET RELATION TO InvHdr.Invoice+InvHdr.Split INTO InvLine ADDITIVE
ELSE
*C201002,1 NNA (END)

  SET RELATION TO InvHdr.Invoice INTO InvLine ADDITIVE

*C201002,1 NNA 05/14/2008 (BEGIN) End of if statment
ENDIF
*C201002,1 NNA (END)

SELECT InvLine
LOCATE
SET RELATION TO SUBSTR(InvLine.Style,lnClrPos,lnClrLen) INTO (lcTmpColor) ADDITIVE
SET RELATION TO IIF(!EMPTY(InvLine.AltStyle) , InvLine.AltStyle , InvLine.Style) INTO Style ADDITIVE
SET RELATION TO "S" + InvLine.Account + InvLine.Style INTO Spck_Lin ADDITIVE

SELECT Style
SET RELATION TO 'S' + Scale INTO Scale ADDITIVE

SELECT InvHdr
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                  'S' + Account + Store) INTO Customer ADDITIVE
SET RELATION TO 'O' + InvHdr.Order INTO OrdHdr ADDITIVE
SET RELATION TO 'C' + InvHdr.Invoice INTO NotePad ADDITIVE

*IF llPrntInst .OR. llRpInvNot
*  SET SKIP TO (lcTmpDbt) , InvLine
*ELSE
*  SET SKIP TO InvLine
*ENDIF
SET SKIP TO InvLine

SELECT InvHdr

DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')

*C201002,1 NNA 05/14/2008 (BEGIN) close the temporary table and reopen the standard table 
SELECT INVHDR
CLOSE INDEX
USE IN INVHDR
=gfOpenFile(gcDataDir+"InvHdr","InvHdr",'SH')
*C201002,1 NNA (END)

*-- End of custom report.

*!**************************************************************************
*! Name      : lfAllNotes
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/21/2002
*! Purpose   : Fill NotePad and Invoice Note Lines.
*!**************************************************************************
*! Example   : = lfAllNotes()
*!**************************************************************************
*
FUNCTION lfAllNotes
PRIVATE lcReturn , lcAlias
lcAlias = ALIAS()

IF llRpInvNot AND SEEK('C' + INVHDR.Invoice , 'NOTEPAD') AND ;
  !EMPTY(ALLTRIM(NOTEPAD.mNotes)) AND ;
  LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
  
  *B608000,1 TMI [Start] get the note from the invline file 
  m.Note_Mem = INVLINE.NOTE_MEM
  *B608000,1 TMI [End  ] 
  
  SELECT(lcStylIndx)
  IF EMPTY(m.Note_Mem)
    lcNotes    = 'Invoice Notes' + CHR(13) + REPLICATE(CHR(205),13) + CHR(13) + ALLTRIM(NOTEPAD.mNotes)
  ELSE
    lcNotes = m.Note_Mem + CHR(13) + REPLICATE(CHR(205),250) + CHR(13) + 'Invoice Notes' + CHR(13) + REPLICATE(CHR(205),13) + CHR(13)
    lcNotes = lcNotes + ALLTRIM(NOTEPAD.mNotes)
  ENDIF
  REPLACE Note_Mem WITH lcNotes  
ENDIF

IF llRpInvNot AND SEEK("T" + PADR("EXPORT FRANCE",20) , 'NOTEPAD') AND UPPER(ALLTRIM(Customer.cCont_Code))$'FR/CH'
  lcNotes = ''
  PRIVATE lnTotQty , lnTotAmt , lnInvLnRec , lcInvHdNot
  STORE 0 TO lnTotQty , lnTotAmt
  lnInvLnRec = RECNO('InvLine')
  IF SEEK(InvHdr.Invoice,'InvLine')
    PRIVATE lcAlias
    lcAlias = ALIAS()
    SELECT InvLine
    SUM TotQty , TotQty * Price TO lnTotQty , lnTotAmt WHILE Invoice = InvHdr.Invoice
    SELECT (lcAlias)
  ENDIF
  IF BETWEEN(lnInvLnRec,1,RECCOUNT('InvLine'))
    GOTO lnInvLnRec IN InvLine
  ENDIF      
  lcNotes = EVALUATE(lcStylIndx+'.Note_Mem')
  lcNotes = lcNotes + CHR(13) + SPACE(10) + 'Montant H,T.' + SPACE(38) + ;
            STR(lnTotQty,9,2) + SPACE(5) + STR(lnTotAmt,15,2)
  IF InvHdr.Discount <> 0
    lcNotes = lcNotes + CHR(13) + SPACE(10) + 'Rabais Totale' + SPACE(51) + STR(InvHdr.Discount,15,2)
  ENDIF          
  IF llTax AND lcTaxMeth = 'M' AND INVHDR.Tax_Amt > 0
    lcNotes = lcNotes + CHR(13) + SPACE(10) + lcTaxDesc + SPACE(40 - (LEN(lcTaxDesc)-13)) + STR(InvHdr.Tax_Rate,5,2)+'%' + SPACE(5) + STR(InvHdr.Tax_Amt,15,2)  
  ENDIF
  
  IF InvHdr.Freight + InvHdr.Insur + InvHdr.COD > 0
    lcNotes = lcNotes + CHR(13) + SPACE(10) + 'Total Freight' + SPACE(51) + STR(InvHdr.Freight + InvHdr.Insur + InvHdr.COD,15,2)
  ENDIF
  
  IF llTax AND lcTaxMeth = 'A' AND INVHDR.Tax_Amt > 0
    lcNotes = lcNotes + CHR(13) + SPACE(10) + lcTaxDesc + SPACE(40 - (LEN(lcTaxDesc)-13)) + STR(InvHdr.Tax_Rate,5,2)+'%' + SPACE(5) + STR(InvHdr.Tax_Amt,15,2)  
  ENDIF

  lcInvHdNot = IIF(InvHdr.Note1<> '*' , ' '  + InvHdr.Note1 , '') + ' ' + IIF(InvHdr.Note2 <> '*' , InvHdr.Note2 , '')
  IF !EMPTY(lcInvHdNot)
    lcNotes = lcNotes + CHR(13) + IIF(InvHdr.Note1<> '*' , ' '  + InvHdr.Note1 , '') + ' ' + IIF(InvHdr.Note2 <> '*' , InvHdr.Note2 , '')
  ENDIF
  
  IF !EMPTY(InvHdr.Approval) AND UPPER(InvHdr.Approval) <> 'DEC'
    lcNotes = lcNotes + CHR(13) + SPACE(10) + 'Approval: ' + InvHdr.Approval
  ENDIF
  lcNotes = lcNotes + CHR(13) + ALLTRIM(NotePad.mNotes)
  
  IF !EMPTY(lcNotes)
    SELECT (lcStylIndx)  
    REPLACE Note_Mem WITH lcNotes
  ENDIF
ENDIF

SELECT (lcAlias)
*-- End of lfAllNotes.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/21/2002
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGVrFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGVrFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpCreateTb
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/21/2002
*! Purpose   : Create all temp files needed
*!**************************************************************************
*! Example   : DO lpCreateTb
*!**************************************************************************
*
PROCEDURE lpCreateTb
PRIVATE lcAlias
lcAlias = ALIAS()

*--Create table to hold the colors and its description.
CREATE Table (lcTmpColor) (Color C(lnClrLen) , Desc C(30))
INDEX ON Color TAG Color OF (gcWorkDir + lcTmpColor + ".CDX")

*-- start to make the temp file which will be opened as InvLine later.
SELECT InvLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*C201002,1 NNA 05/14/2008 (BEGIN) add new field to the temp file to hold the split section filed
*DIMENSION laFileStru[lnFileStru + 7, 4]
DIMENSION laFileStru[lnFileStru + 8, 4]
*C201002,1 NNA (END)

*--To prevent printing the T.V.A BOX.
laFileStru[lnFileStru + 1 ,1] = 'llNoTVA'
laFileStru[lnFileStru + 1 ,2] = 'L'
laFileStru[lnFileStru + 1 ,3] = 1
laFileStru[lnFileStru + 1 ,4] = 0

*--Field collect in it the total of the last page in case CUSTOMER.CCONT_CODE = "FR"
laFileStru[lnFileStru + 2 ,1] = 'TotFrCom'
laFileStru[lnFileStru + 2 ,2] = 'N'
laFileStru[lnFileStru + 2 ,3] = 10
laFileStru[lnFileStru + 2 ,4] = 2

*--Field collect in it the total of the last page in case CUSTOMER.CCONT_CODE = "FR"
laFileStru[lnFileStru + 3 ,1] = 'TotFrVal'
laFileStru[lnFileStru + 3 ,2] = 'N'
laFileStru[lnFileStru + 3 ,3] = 10
laFileStru[lnFileStru + 3 ,4] = 2

*--To add in it the desc of the Style.Ccomcode to use it in the index.
laFileStru[lnFileStru + 4 ,1] = 'cComCode'
laFileStru[lnFileStru + 4 ,2] = 'C'
laFileStru[lnFileStru + 4 ,3] = 20
laFileStru[lnFileStru + 4 ,4] = 0

*--Check to print this style or not based on the first style major for all colors in this style.
laFileStru[lnFileStru + 5 ,1] = 'lFirstSty'
laFileStru[lnFileStru + 5 ,2] = 'L'
laFileStru[lnFileStru + 5 ,3] = 1
laFileStru[lnFileStru + 5 ,4] = 0

*--check if it's last line in invoice.
laFileStru[lnFileStru + 6 ,1] = 'lEndLine'
laFileStru[lnFileStru + 6 ,2] = 'L'
laFileStru[lnFileStru + 6 ,3] = 1
laFileStru[lnFileStru + 6 ,4] = 0

*--To hold the memo notes
*B606512,1 Add a new fields instead of using the Flag one in InvLine. [Begin]
*laFileStru[lnFileStru + 7 ,1] = 'mNotes'
*laFileStru[lnFileStru + 7 ,2] = 'M'
*laFileStru[lnFileStru + 7 ,3] = 1
*laFileStru[lnFileStru + 7 ,4] = 0
laFileStru[lnFileStru + 7 ,1] = 'NewFlag'
laFileStru[lnFileStru + 7 ,2] = 'C'
laFileStru[lnFileStru + 7 ,3] = 1
laFileStru[lnFileStru + 7 ,4] = 0
*B606512,1 Add a new fields instead of using the Flag one in InvLine. [End]

*C201002,1 NNA 05/14/2008 (BEGIN) the new filed that will hold the split section
*--To hold the Split Section
laFileStru[lnFileStru + 8 ,1] = 'Split'
laFileStru[lnFileStru + 8 ,2] = 'C'
laFileStru[lnFileStru + 8 ,3] = 1
laFileStru[lnFileStru + 8 ,4] = 0
*C201002,1 NNA (END)

CREATE TABLE (gcWorkDir+lcTmpInvLn) FROM ARRAY laFileStru

*C201002,1 NNA 05/14/2008 (BEGIN) create a temp table as same as invline table
IF llRpSplit
  INDEX ON Invoice+SPLIT TAG lcTmpInvLn OF (gcWorkDir + lcTmpInvLn+ '.CDX')
  CREATE TABLE (gcWorkDir+lcInvLine) FROM ARRAY laFileStru
  INDEX ON Invoice + Split+ Style TAG (lcInvLine) OF (gcWorkDir + lcInvLine + '.CDX')
ELSE
*C201002,1 NNA (END)

INDEX ON Invoice TAG lcTmpInvLn OF (gcWorkDir + lcTmpInvLn+ '.CDX')

*C201002,1 NNA 05/14/2008 (BEGIN) End of if statment
ENDIF
*C201002,1 NNA (END)

CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru
*-- The invline is right due to use this name when replace the invline file place the temp. file(gcWorkDir + INVLINE + '.CDX')
*-- Use the flag field to update it with 'Z' for France
INDEX ON Invoice + Style TAG InvLine1 OF (gcWorkDir + InvLine + '.CDX')

*B606512,1 Change Flag field to NewFlag. [Begin]
*INDEX ON Invoice + Flag + cComCode + Style TAG InvLine OF (gcWorkDir + InvLine + '.CDX')
INDEX ON Invoice + NewFlag + cComCode + Style TAG InvLine OF (gcWorkDir + InvLine + '.CDX')
*B606512,1 Change Flag field to NewFlag. [End]

SELECT (lcAlias)
*-- End of lpCreateTb.

*!**************************************************************************
*! Name      : lfEndGrp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/21/2002
*! Purpose   : To detect the end of page
*!**************************************************************************
*! Example   : =lfUsrVlfFn('lfEndGrp','ARPINVNV')
*!**************************************************************************
*This function is not used anymore.
FUNCTION lfEndGrp
PARAMETER llDummy
llDummy  = .T.
PRIVATE lcAlias , lnInvLnRec
llEndGroup = .T.
IF ALLTRIM(UPPER(Customer.cCont_Code)) $ 'FR/CH'
ELSE
ENDIF
RETURN ''
*-- End of lfEndGrp.
*!***************************************************************************
*! Name        : lfDoSplit
*! Developer   : Nader Nabil (NNA)
*! Date        : 05/14/2008
*! Purpose     : Doing the Invoice Split
*!***************************************************************************
*! Called from : ARPINVNV.PRG
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Returns     : None
*!***************************************************************************
*! Example     : =lfDoSplit()
*!***************************************************************************
*-->C201002,1
FUNCTION lfDoSplit
SELECT InvHdr
*--> Create a temp. table and index
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 1, 4]

*--To hold the Split Section
laFileStru[lnFileStru + 1 ,1] = 'Split'
laFileStru[lnFileStru + 1 ,2] = 'C'
laFileStru[lnFileStru + 1 ,3] = 1
laFileStru[lnFileStru + 1 ,4] = 0

CREATE TABLE (gcWorkDir+lcInvHdr) FROM ARRAY laFileStru
INDEX ON Invoice+Split TAG (lcInvHdr) OF (gcWorkDir+lcInvHdr+'.CDX')

SELECT(lcStylIndx)
GO TOP
SCAN REST WHILE !EOF()
  SCATTER MEMVAR MEMO
  lnPrice = m.Price

  *-->First Split
  m.Price = ROUND((lnPrice * lnRpStSplt)/100,2)
  m.Split = '1'
  SELECT(lcInvLine)
  APPEND BLANK
  GATHER MEMVAR MEMO

  *-->Second Split
  m.Price = ROUND((lnPrice * lnRpNdSplt)/100,2)
  m.Split = '2'
  APPEND BLANK
  GATHER MEMVAR MEMO
  **-->Split Invhdr <--**
  IF SEEK(&lcStylIndx..Invoice,'INVHDR')
    SELECT INVHDR
    SCATTER MEMVAR MEMO
    m.Split = '1'
    lnTotChg = m.TotalChg
    lnTaxamt = m.Tax_Amt
    lnDisc   = m.Discount
    lnCharge = m.nCharges
    m.TotalChg = ROUND((lnTotChg * lnRpStSplt)/100,2)
    m.Tax_Amt  = ROUND((lnTaxamt * lnRpStSplt)/100,2)
    m.Discount = ROUND((lnDisc   * lnRpStSplt)/100,2)
    m.nCharges = ROUND((lnCharge * lnRpStSplt)/100,2)

    IF !SEEK(&lcStylIndx..Invoice+m.Split,lcInvHdr)
      SELECT (lcInvHdr)
      APPEND BLANK
      GATHER MEMVAR MEMO        
    ENDIF
    m.Split = '2'
    IF !SEEK(&lcStylIndx..Invoice+m.Split,lcInvHdr)
      m.TotalChg = ROUND((lnTotChg * lnRpNdSplt)/100,2)
      m.Tax_Amt  = ROUND((lnTaxamt * lnRpNdSplt)/100,2)
      m.Discount = ROUND((lnDisc   * lnRpNdSplt)/100,2)
      m.nCharges = ROUND((lnCharge * lnRpNdSplt)/100,2)

      SELECT (lcInvHdr)
      APPEND BLANK
      GATHER MEMVAR MEMO        
    ENDIF
  ENDIF   
ENDSCAN
*--> End of FUNCTION lfDoSplit.



FUNCTION  LFGTSTYDSC
PRIVATE LCFLDLIST, LCDESC, LCFLD

LCFLDLIST = 'CDESC01|CDESC02|CDESC03|CDESC04|CDESC05|CDESC06|CDESC07|CDESC08|CDESC09|CDESC10|'+'CDESC11|CDESC12|CDESC13|CDESC14|CDESC15|CDESC16|CDESC17|CDESC18|CDESC19|CDESC20|'
LCDESC = ALLTRIM(STYLE.DESC)
IF  .NOT. EMPTY(CUSTOMER.CSTYDESC) .AND. UPPER(CUSTOMER.CSTYDESC) $ LCFLDLIST .AND.  .NOT. ;
    EMPTY(EVALUATE('STYLE.'+ALLTRIM(CUSTOMER.CSTYDESC)))
    
   LCDESC = EVALUATE('STYLE.'+ALLTRIM(CUSTOMER.CSTYDESC))
   
ENDIF
RETURN LCDESC
