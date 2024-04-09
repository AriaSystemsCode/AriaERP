*!********************************************************************
*: Program file  : MFCONTX.PRG (REFERE TO C102330)
*: Program desc. : Convert data from Cut & Sold report
*: For screen    : MFCONTX.SCX
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 05/07/2001
*:     Developer : AHMED MAHER (AMH)
*!********************************************************************
*! Modifications :-
*!********************************************************************

*-- Initializing the necessary variables.
STORE SPACE(0)  TO lcTxtFile, lcRegion, lcClass, lcTerms, lcShipVia, lcSpinst, lcScale, lcDesc,;
                   lcDivision, lcInvoice, lcOpenLog
STORE 0         TO lnPrice, lnCost, lnLineInv, lnLineCut, lnLineCut1, lnLogHand
STORE SPACE(6)  TO lcCut, lcCutTran, lcInv
STORE SPACE(12) TO lcStyTran
IF !gfSetup()
  RETURN
ENDIF  

*-- Create Tmp tables.
lcTmpData = gfTempName()
=lfCreatFil()

*-- To select the text file that will be imported.
DO (gcScrDir+gcWinAppl+'\MFCONTX.SPX')

*!*************************************************************
*! Name      : lfvGetFile
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : To allow the user to select the file to be imported.
*!*************************************************************
*! Example            : =lfvGetFile()
*!*************************************************************
*
FUNCTION lfvGetFile

lcTxtFile  = GETFILE('TXT','Select Text File')
SHOW GET lcTxtFile
IF EMPTY(lcTxtFile)
  SHOW GET pbOK DISABLE
ELSE
  SHOW GET pbOK ENABLE
ENDIF
*--end of lfvGetFile.

*!*************************************************************
*! Name      : lfCreatFil
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Create the temporary files.
*!*************************************************************
*! Example            : =lfCreatFil()
*!*************************************************************
*
FUNCTION lfCreatFil

*-- Cursor to collecting Data.
CREATE CURSOR (lcTmpData) (cStyle C(12), cColor C(6), nIntranst1 N(6,0), nIntranst2 N(6,0),;
               nIntranst3 N(6,0), nIntranst4 N(6,0), nIntranst5 N(6,0), nIntranst6 N(6,0),;
               nIntranst7 N(6,0), nIntranst8 N(6,0), nRec1St1 N(6,0), nRec1St2 N(6,0),;
               nRec1St3 N(6,0), nRec1St4 N(6,0), nRec1St5 N(6,0), nRec1St6 N(6,0),;
               nRec1St7 N(6,0), nRec1St8 N(6,0), nRec2Nd1 N(6,0), nRec2Nd2 N(6,0),;
               nRec2Nd3 N(6,0), nRec2Nd4 N(6,0), nRec2Nd5 N(6,0), nRec2Nd6 N(6,0),;
               nRec2Nd7 N(6,0), nRec2Nd8 N(6,0), nShip1 N(6,0), nShip2 N(6,0), nShip3 N(6,0),;
               nShip4 N(6,0), nShip5 N(6,0), nShip6 N(6,0), nShip7 N(6,0), nShip8 N(6,0),;
               nStk1 N(6,0), nStk2 N(6,0), nStk3 N(6,0), nStk4 N(6,0), nStk5 N(6,0),;
               nStk6 N(6,0), nStk7 N(6,0), nStk8 N(6,0))
*-- end of lfCreatFil.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Collect the date from Text file to Temp.
*!*************************************************************
*! Example            : =lfCollect()
*!*************************************************************
*
FUNCTION lfCollect

PRIVATE lnI,lcI

*-- Open the Text file.
lnTextHand = FOPEN(lcTxtFile,0)

*-- If Text file not opened successfully
IF lnTextHand < 0
  *-- Message : <Text file was not opened successfully, Check if this file is already opened and try again!>
  *-- Buttons : <                                    OK                                    >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Text file was not opened successfully, Check if this file is already opened and try again!')
  RETURN .F.
ENDIF

*-- Do While loop till end of text file. 
SELECT STYLE
SET ORDER TO TAG CSTYLE
SELECT (lcTmpData)
WAIT WINDOW 'Collecting data from txt file....' NOWAIT
DO WHILE !FEOF(lnTextHand)
  lcTextLine = FGETS(lnTextHand)
  IF !EMPTY(lcTextLine) .AND. SEEK(SUBSTR(lcTextLine,1,12),'STYLE')
    WAIT WINDOW 'Style : '+ALLTRIM(SUBSTR(lcTextLine,1,12)) NOWAIT
    IF SEEK('S'+STYLE.SCALE,'SCALE')
      APPEND BLANK
      REPLACE CSTYLE WITH SUBSTR(lcTextLine,1,12)
      lcTextLine = FGETS(lnTextHand)
      lcTextLine = FGETS(lnTextHand)
      REPLACE CCOLOR WITH '000' + SUBSTR(lcTextLine,7,3)
      lcTextLine = FGETS(lnTextHand)
      lcTextLine = FGETS(lnTextHand)
      lcTextLine = FGETS(lnTextHand)
      lnI = 0
      FOR lnI = 1 TO SCALE.CNT
        lcI = STR(lnI,1)
        REPLACE NINTRANST&lcI. WITH VAL(SUBSTR(lcTextLine,AT('|',lcTextLine,8)+1,6)) ,;
                NREC1ST&lcI.   WITH VAL(SUBSTR(lcTextLine,AT('|',lcTextLine,9)+1,7)) ,;
                NREC2ND&lcI.   WITH VAL(SUBSTR(lcTextLine,AT('|',lcTextLine,10)+1,6)),;
                NSHIP&lcI.     WITH VAL(SUBSTR(lcTextLine,AT('|',lcTextLine,11)+1,7)),;
                NSTK&lcI.      WITH VAL(SUBSTR(lcTextLine,AT('|',lcTextLine,12)+1,6))
        lcTextLine = FGETS(lnTextHand)
        lcTextLine = FGETS(lnTextHand)
      ENDFOR
    ENDIF
  ENDIF
ENDDO
WAIT CLEAR
=FCLOSE(lnTextHand)
SELECT (lcTmpData)
LOCATE
IF EOF()
  =gfModalGen("TRM000000B00000","DIALOG",'','','No data has been converted.')
  RETURN .F.
ENDIF

*-- end of lfCollect.

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Valid function of the cancel button.
*!*************************************************************
*! Example            : =lfvCancel()
*!*************************************************************
*
FUNCTION lfvCancel

CLEAR READ
*-- end of lfvCancel.

*!*************************************************************
*! Name      : lfvOk
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Valid function of the Ok button.
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
*
FUNCTION lfvOk

*-- Collect data from txt file.
IF !lfCollect()
  lcTxtFile = SPACE(0)
  SHOW GET lcTxtFile
  SHOW GET pbOK DISABLE  
  RETURN
ENDIF

*-- Create Dummy Customer.
=lfAddCust()

*-- Create Dummy Vendor.
=lfAddVend()

*-- Create and Update needed transactions.
SELECT STYLE
SET ORDER TO TAG STYLE
SELECT (lcTmpData)
LOCATE
SCAN
  lcSeason = SUBSTR(CSTYLE,1,3)
  lcStyle  = CSTYLE + '-' + CCOLOR
  IF !SEEK(lcStyle+'DEFWHE'+SPACE(10),'STYDYE')
    IF EMPTY(lcOpenLog)
      IF FILE(gcWorkDir+'MFLOGSTY.TXT')
        lnLogHand = FOPEN(gcWorkDir+'MFLOGSTY.TXT',2)
        IF lnLogHand < 0
          lcOpenLog = 'X'
        ELSE
          lcOpenLog = 'O'
          =FSEEK(lnLogHand,0,2)
          =FPUTS(lnLogHand,'Style : '+ALLTRIM(lcStyle)+;
                           ' does not exist in the STYDYE file.')
        ENDIF
      ELSE
        lnLogHand = FCREATE(gcWorkDir+'MFLOGSTY.TXT',0)
        IF lnLogHand < 0
          lcOpenLog = 'X'
        ELSE
          lcOpenLog = 'O'
          =FPUTS(lnLogHand,'Style : '+ALLTRIM(lcStyle)+;
                           ' does not exist in the STYDYE file.')
        ENDIF
      ENDIF
    ELSE
      IF lcOpenLog = 'O'
        =FPUTS(lnLogHand,'Style : '+ALLTRIM(lcStyle)+;
                         ' does not exist in the STYDYE file.')
      ENDIF
    ENDIF
    LOOP
  ENDIF
  *-- Add record to styinvjl file.
  =lfAddStInv()
  
  *-- Update the stk fields in the style file.
  =lfUpDtStk()
  
  *-- Add the invoice data.
  =lfAddInvoc()
  
  *-- Add the cuttkt data.
  =lfAddCutkt()
  
  *-- Add the intransit data.
  =lfAddIntrn()
ENDSCAN
WAIT CLEAR
IF lcOpenLog = 'O'
  =FCLOSE(lnLogHand)
ENDIF
=gfModalGen("INM000000B00000","DIALOG",'','','Conversion done')
lcTxtFile = SPACE(0)
SHOW GET lcTxtFile
SHOW GET pbOK DISABLE
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfAddCust
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : create dummy customer.
*!*************************************************************
*! Example            : =lfAddCust()
*!*************************************************************
*
FUNCTION lfAddCust

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT CODES
IF SEEK ('D'+'REGION')
  lcRegion = cCode_No
ELSE
  lcRegion = '000000'
ENDIF
IF SEEK ('D'+'CLASS')
  lcClass = cCode_No
ELSE
  lcClass = '000000'
ENDIF
IF SEEK ('D'+ 'CTERMCODE')
  lcTerms = cCode_No
ELSE
  lcTerms = '000000'
ENDIF
IF SEEK ('D'+ 'SHIPVIA')
  lcShipVia = cCode_No
ELSE
  lcShipVia = '000000'
ENDIF
IF SEEK ('D'+ 'SPCINST')
  lcSpinst = cCode_No
ELSE
  lcSpinst = '000000'
ENDIF
SELECT CUSTOMER
IF !SEEK ('M'+'ZZZZZ')
  APPEND BLANK
  REPLACE Type       WITH 'M'             ,;
          Billto     WITH 'M'             ,;
          account    WITH 'ZZZZZ'         ,;
          status     WITH 'A'             ,;
          link_code  WITH 'DEFDEF'        ,;
          Btname     WITH 'Dummy Customer',;
          region     WITH lcregion        ,;
          class      WITH lcclass         ,;
          ctermcode  WITH lcterms         ,;
          shipvia    WITH lcshipvia       ,;
          spcinst    WITH lcspinst        ,;
          ctaxrule   WITH '1'             ,;
          lvatexem   WITH .F.             ,;
          ccont_code WITH 'USA'           ,;
          ccurrcode  WITH 'USD'           ,;
          cslsgllink WITH 'DEF'
ENDIF
SELECT (lnAlias)
*-- end of lfAddCust.

*!*************************************************************
*! Name      : lfAddVend
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : create dummy vendor.
*!*************************************************************
*! Example            : =lfAddVend()
*!*************************************************************
*
FUNCTION lfAddVend

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT APVENDOR
IF !SEEK('ZZZZZZZZ')
  APPEND BLANK
  REPLACE cVendCode  WITH 'ZZZZZZZZ'    ,;
          cvencomp   WITH 'Dummy Vendor',;
          cCont_Code WITH 'USA'         ,;
          cTermCode  WITH lcTerms       ,;
          cCurrCode  WITH 'USD'         ,;
          cVenPrior  WITH '1'           ,;
          cVenPMeth  WITH 'P'           ,;
          cTaxType   WITH 'T'
ENDIF
SELECT (lnAlias)
*-- end of lfAddVend.

*!*************************************************************
*! Name      : lfUpDtStk
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Update stk fields of style file.
*!*************************************************************
*! Example            : =lfUpDtStk()
*!*************************************************************
*
FUNCTION lfUpDtStk

PRIVATE lnAlias, lnI, lcI
lnAlias = SELECT(0)
IF SEEK (lcStyle,'STYLE')
  SELECT STYLE
  REPLACE TOTSTK WITH 0
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE STK&lcI. WITH MAX(EVALUATE(lcTmpData+'.NSTK'+lcI),0)
    REPLACE TOTSTK   WITH TOTSTK + EVALUATE('STK'+lcI)
  ENDFOR
ENDIF
SELECT (lnAlias)
*-- end of lfUpDtStk.

*!*************************************************************
*! Name      : lfAddStInv
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Add styinvJl record.
*!*************************************************************
*! Example            : =lfAddStInv()
*!*************************************************************
*
FUNCTION lfAddStInv

PRIVATE lnAlias
lnAlias = SELECT(0)

DECLARE laOldStk[11]
STORE 0 TO laOldStk

**Calculate the old stock

IF SEEK (lcStyle,'STYLE')
  WAIT WINDOW 'Update Stock value for style : ' + lcStyle NOWAIT
  SELECT STYLE
  lcScale = SCALE
  lnPrice = PRICEA
  lcDesc = DESC
  lcDivision = cDivision
  lnCost  = TOTCOST
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laOldStk[lnI] = EVALUATE('STK'+lcI)
    laOldStk[9]   = laOldStk[9] + laOldStk[lnI]
  ENDFOR
  laOldStk[10] = laOldStk[9]*STYLE.TotCost
  laOldStk[11] = gdSysDate
ENDIF

SELECT InvtAdj
APPEND BLANK
REPLACE Style     WITH lcStyle         ,;
        cReason   WITH "Physical Count",; 
        Date      WITH gdSysDate       ,;
        Type      WITH 'P'             ,;
        cFromWare WITH 'DEFWHE'        ,; 
        Unt_Cost  WITH STYLE.TotCost   ,;
        Old_Cost  WITH STYLE.TotCost

DECLARE laAdjust[9]
STORE 0 TO laAdjust

FOR lnCount=1 TO 8
  lcCount = STR(lnCount,1)
  laAdjust[lnCount] = MAX(EVALUATE(lcTmpData+'.NSTK'+lcCount),0)
  laAdjust[9] = laAdjust[9] + laAdjust[lnCount]
  REPLACE Adj&lcCount    WITH MAX(EVALUATE(lcTmpData+'.NSTK'+lcCount),0),;
          OldQty&lcCount WITH laoldStk(lnCount)                         ,;
          TotAdj         WITH TotAdj + MAX(EVALUATE(lcTmpData+'.NSTK'+lcCount),0)
ENDFOR

REPLACE TotOld WITH laOldStk(9)

*lcSession = gfSequence('GLSESSION')
*--G/L Array difinition and initialization.
DIME laGLDistAr[1,1]
laGLDistAr = ''

lnRet=gfStyCrl('2',lcStyle,'DEFWHE','',gdSysDate,'',@laAdjust,STYLE.TOTCOST,;
               '',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)

SELECT (lnAlias)
*-- end of lfAddStInv.

*!*************************************************************
*! Name      : lfAddInvoc
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Add invoice data.
*!*************************************************************
*! Example            : =lfAddInvoc()
*!*************************************************************
*
FUNCTION lfAddInvoc

PRIVATE lnAlias, lnI, lcI, llNoShip
lnAlias = SELECT(0)
llNoShip = .T.
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF EVALUATE(lcTmpData+'.nShip'+lcI) <> 0
    llNoShip = .F.
    EXIT
  ENDIF
ENDFOR
IF llNoShip
  SELECT (lnAlias)
  RETURN
ENDIF
IF !SEEK('ZZZZZ','INVHDR')
  lcInvoice = gfSequence('INVOICE','','','')
  WAIT WINDOW 'Create invoice # : ' + lcInvoice NOWAIT
  SELECT INVHDR
  APPEND BLANK
  REPLACE INVOICE    WITH lcInvoice ,;
          INVDATE    WITH gdSysDate ,;
          SHIPDATE   WITH gdSysDate ,;
          DUEDATE    WITH gdSysDate ,;
          STATUS     WITH 'C'       ,;
          ACCOUNT    WITH 'ZZZZZ'   ,;
          SEASON     WITH lcSeason  ,;
          cDivision  WITH lcDivision,;
          CTERMCODE  WITH lcTerms   ,;
          SHIPVIA    WITH lcShipVia ,;
          SPCINST    WITH lcSpinst  ,;
          LINK_CODE  WITH 'DEFDEF'  ,;
          DIRECT_INV WITH .T.       ,;
          CCURRCODE  WITH 'USD'     ,;
          NCURRUNIT  WITH 1         ,;
          NEXRATE    WITH 1.0000
ELSE
  lcInvoice = INVHDR.INVOICE
  SELECT INVLINE
  SET ORDER TO TAG INVLINE DESCENDING
  IF SEEK(lcInvoice)
    lnLineInv = LINENO
  ENDIF
  SET ORDER TO TAG INVLINES ASCENDING
ENDIF
SELECT INVLINE
IF !SEEK(lcStyle+lcInvoice)
  WAIT WINDOW 'Add style : ' + ALLTRIM(lcStyle) + ' to invoice # : ' + lcInvoice NOWAIT
  APPEND BLANK
  lnLineInv = lnLineInv + 1
  REPLACE INVOICE    WITH lcInvoice,;
          INVDATE    WITH gdSysDate,;
          LINENO     WITH lnLineInv,;
          ACCOUNT    WITH 'ZZZZZ'  ,;
          STYLE      WITH lcStyle  ,;
          SCALE      WITH lcScale  ,;
          PRICE      WITH lnPrice  ,;
          GROS_PRICE WITH lnPrice  ,;
          GL_SALES   WITH 'DEFDEF' ,;
          GL_COST    WITH 'DEFDEF' ,;
          SEASON     WITH lcSeason ,;
          DESC1      WITH lcDesc
ENDIF
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  REPLACE QTY&lcI. WITH EVALUATE('QTY'+lcI) + EVALUATE(lcTmpData+'.nShip'+lcI)
  REPLACE TOTQTY   WITH TOTQTY + EVALUATE(lcTmpData+'.nShip'+lcI)
ENDFOR
REPLACE INVHDR.ship     WITH INVHDR.SHIP + TOTQTY              ,;
        INVHDR.SHIPAMT  WITH INVHDR.SHIPAMT + (TOTQTY*lnPRICE) ,;
        INVHDR.TOTALCHG WITH INVHDR.TOTALCHG + (TOTQTY*lnPRICE),;
        INVHDR.APPRAMT  WITH INVHDR.APPRAMT + (TOTQTY*lnPRICE)
SELECT (lnAlias)
*-- end of lfAddInvoc.

*!*************************************************************
*! Name      : lfAddCutkt
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Add cuttkt data.
*!*************************************************************
*! Example            : =lfAddCutkt()
*!*************************************************************
*
FUNCTION lfAddCutkt

PRIVATE lnAlias, lnI, lcI, llNoRec1, llNoRec2
lnAlias = SELECT(0)
SELECT (lcTmpData)
STORE .T. TO llNoRec1, llNoRec2
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF llNoRec1 .AND. EVALUATE('nRec1St'+lcI) <> 0
    llNoRec1 = .F.
    IF !llNoRec2
      EXIT
    ENDIF
  ENDIF
  IF llNoRec2 .AND. EVALUATE('nRec2Nd'+lcI) <> 0
    llNoRec2 = .F.
    IF !llNoRec1
      EXIT
    ENDIF
  ENDIF
ENDFOR
IF llNoRec1 .AND. llNoRec2
  SELECT (lnAlias)
  RETURN
ENDIF
SELECT CUTTKTH
IF EMPTY(lcCut) .OR. (SEEK(lcCut) .AND. EVALUATE(lcTmpData+'.CSTYLE') <> STYLE)
  lcCut = gfSequence('CUTTKT','','','')
  WAIT WINDOW 'Create C/T # : ' + lcCut NOWAIT
  APPEND BLANK
  lnLineCut = 0
  REPLACE cuttkt    WITH lcCut                        ,;
          style     WITH EVALUATE(lcTmpData+'.CSTYLE'),;
          season    WITH lcseason                     ,;
          cdivision WITH lcdivision                   ,;
          status    WITH 'C'                          ,;
          entered   WITH gdSysDate                    ,;
          complete  WITH gdSysDate                    ,;
          cowner    WITH 'conversion'
ENDIF
SELECT cuttktl
WAIT WINDOW 'Add style : ' + ALLTRIM(lcStyle) + ' to C/T # : ' + lcCut NOWAIT
APPEND BLANK
lnLineCut = lnLineCut + 1
REPLACE cuttkt WITH lcCut    ,;
        style  WITH lcStyle  ,;
        lineno WITH lnLineCut,;
        trancd WITH '1'
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  REPLACE QTY&lcI. WITH EVALUATE(lcTmpData+'.nRec1St'+lcI) + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  REPLACE TOTQTY   WITH TOTQTY + EVALUATE(lcTmpData+'.nRec1St'+lcI) + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
ENDFOR
REPLACE cuttkth.pcs_bud WITH cuttkth.pcs_bud + totqty
IF !llNoRec1
  APPEND BLANK
  REPLACE cuttkt WITH lcCut    ,;
          style  WITH lcStyle  ,;
          lineno WITH lnLineCut,;
          trancd WITH '2'
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE QTY&lcI. WITH EVALUATE(lcTmpData+'.nRec1St'+lcI)
    REPLACE TOTQTY   WITH TOTQTY + EVALUATE(lcTmpData+'.nRec1St'+lcI)
  ENDFOR
  REPLACE CSTYGRADE WITH '1'
  REPLACE cuttkth.pcs_rec WITH  cuttkth.pcs_rec + totqty
ENDIF
IF !llNoRec2
  APPEND BLANK
  REPLACE cuttkt WITH lcCut    ,;
          style  WITH lcStyle  ,;
          lineno WITH lnLineCut,;
          trancd WITH '3'
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE QTY&lcI. WITH EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
    REPLACE TOTQTY   WITH TOTQTY + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  ENDFOR
  REPLACE cuttkth.pcs_dam WITH cuttkth.pcs_dam+totqty
  *IF !SEEK(lcSeason+'-ZZZZZZZZ-'+EVALUATE(lcTmpData+'.cColor'),'STYLE')
  *  SELECT STYLE
  *  APPEND BLANK
  *  REPLACE STYLE      WITH lcSeason + '-ZZZZZZZZ-' + EVALUATE(lcTmpData+'.cColor'),;
  *          CSTYMAJOR  WITH lcSeason + '-ZZZZZZZZ'                                 ,;
  *          CDYE_FLG   WITH 'N'                                                    ,;
  *          STATUS     WITH 'A'                                                    ,;
  *          DESC       WITH 'Dam. Style'                                           ,;
  *          SEASON     WITH lcSeason                                               ,;
  *          CSTYGRADE  WITH '2'                                                    ,;
  *          LINK_CODE  WITH 'DEFDEF'                                               ,;
  *          MAKE       WITH .T.                                                    ,;
  *          LINVSTY    WITH .T.                                                    ,;
  *          CDEFWARE   WITH 'DEFWHE'                                               ,;
  *          CSLSGLLINK WITH 'DEF'                                                  ,;
  *          SCALE      WITH lcScale
  *  FOR lnI = 1 TO 8
  *    lcI = STR(lnI,1)
  *    REPLACE STK&lcI. WITH EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *    REPLACE TOTSTK   WITH TOTSTK + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *  ENDFOR
  *  SELECT STYDYE
  *  APPEND BLANK
  *  REPLACE STYLE     WITH lcSeason + '-ZZZZZZZZ-' + EVALUATE(lcTmpData+'.cColor'),;
  *          CWARECODE WITH 'DEFWHE'                                               ,;
  *          GL_LINK   WITH 'DEFDEF'
  *  FOR lnI = 1 TO 8
  *    lcI = STR(lnI,1)
  *    REPLACE STK&lcI. WITH EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *    REPLACE TOTSTK   WITH TOTSTK + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *  ENDFOR
  *  DECLARE laAdjust[9]
  *  STORE 0 TO laAdjust

  *  FOR lnCount=1 TO 8
  *    lcCount = STR(lnCount,1)
  *    laAdjust[lnCount] = EVALUATE(lcTmpData+'.nRec2Nd'+lcCount)
  *    laAdjust[9]       = laAdjust[9] + laAdjust[lnCount]
  *  ENDFOR

  *  *--G/L Array difinition and initialization.
  *  DIME laGLDistAr[1,1]
  *  laGLDistAr = ''

  *  lnRet=gfStyCrl('5',lcSeason+'-ZZZZZZZZ-'+EVALUATE(lcTmpData+'.cColor'),'DEFWHE','',;
  *                 gdSysDate,'',@laAdjust,STYLE.TOTCOST,'',.T.,'',0,'','',@laGLDistAr,0,'')
  *ELSE
  *  SELECT STYLE
  *  FOR lnI = 1 TO 8
  *    lcI = STR(lnI,1)
  *    REPLACE STK&lcI. WITH EVALUATE('STK'+lcI) + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *    REPLACE TOTSTK   WITH TOTSTK + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *  ENDFOR
  *  IF SEEK(lcSeason+'-ZZZZZZZZ-'+EVALUATE(lcTmpData+'.cColor')+'DEFWHE','STYDYE')
  *    SELECT STYDYE
  *    FOR lnI = 1 TO 8
  *      lcI = STR(lnI,1)
  *      REPLACE STK&lcI. WITH EVALUATE('STK'+lcI) + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *      REPLACE TOTSTK   WITH TOTSTK + EVALUATE(lcTmpData+'.nRec2Nd'+lcI)
  *    ENDFOR
  *  ENDIF

  *  DECLARE laAdjust[9]
  *  STORE 0 TO laAdjust

  *  FOR lnCount=1 TO 8
  *    lcCount = STR(lnCount,1)
  *    laAdjust[lnCount] = EVALUATE(lcTmpData+'.nRec2Nd'+lcCount)
  *    laAdjust[9]       = laAdjust[9] + laAdjust[lnCount]
  *  ENDFOR

  *  *--G/L Array difinition and initialization.
  *  DIME laGLDistAr[1,1]
  *  laGLDistAr = ''

  *  lnRet=gfStyCrl('5',lcSeason+'-ZZZZZZZZ-'+EVALUATE(lcTmpData+'.cColor'),'DEFWHE','',;
  *                 gdSysDate,'',@laAdjust,STYLE.TOTCOST,'',.T.,'',0,'','',@laGLDistAr,0,'')
  *ENDIF
  *REPLACE CUTTKTL.CSTYGRADE WITH '2'
  *REPLACE CUTTKTL.CRETSTY   WITH STYLE.STYLE
ENDIF
REPLACE CUTTKTH.PCS_OPN WITH CUTTKTH.PCS_BUD - CUTTKTH.PCS_REC - CUTTKTH.PCS_DAM
SELECT (lnAlias)
*-- end of lfAddCutkt.

*!*************************************************************
*! Name      : lfAddIntrn
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Add intransite data.
*!*************************************************************
*! Example            : =lfAddIntrn()
*!*************************************************************
*
FUNCTION lfAddIntrn

PRIVATE lnAlias, lnI, lcI, llNoTran
lnAlias = SELECT(0)
SELECT (lcTmpData)
llNoTran = .T.
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF EVALUATE('nInTranst'+lcI) <> 0
    llNoTran = .F.
    EXIT
  ENDIF
ENDFOR
IF llNoTran
  SELECT (lnAlias)
  RETURN
ENDIF
SELECT TransHdr
IF EMPTY(lcInv) .OR. (SEEK('ZZZZZZZZ'+'R'+lcInv) .AND.;
                      EVALUATE(lcTmpData+'.CSTYLE') <> lcStyTran)
  lcInv = lfGtInvTrn()
  lcStyTran = EVALUATE(lcTmpData+'.CSTYLE')
  WAIT WINDOW 'Creat Intransite invoice # : ' + lcInv NOWAIT
  APPEND BLANK
  REPLACE CCUTVEN    WITH 'ZZZZZZZZ',;
          CSHIP_TYPE WITH 'R'       ,;
          CCUTINV    WITH lcInv     ,;
          STATUS     WITH 'O'       ,;
          ENTERED    WITH gdSysDate ,;
          ETA        WITH gdSysDate
  lcCutTran = gfSequence('CUTTKT','','','')
  SELECT CUTTKTH
  WAIT WINDOW 'Creat C/T # : ' + lcCutTran NOWAIT
  APPEND BLANK
  lnLineCut1 = 0
  REPLACE cuttkt    WITH lcCutTran                    ,;
          style     WITH EVALUATE(lcTmpData+'.CSTYLE'),;
          season    WITH lcseason                     ,;
          cdivision WITH lcdivision                   ,;
          status    WITH 'H'                          ,;
          entered   WITH gdSysDate                    ,;
          complete  WITH gdSysDate
ENDIF
SELECT CUTTKTL
WAIT WINDOW 'Add style : ' + ALLTRIM(lcStyle) + ' to C/T # : ' + lcCutTran NOWAIT
APPEND BLANK
lnLineCut1 = lnLineCut1 + 1
REPLACE CUTTKT WITH lcCutTran ,;
        STYLE  WITH lcStyle   ,;
        LINENO WITH lnLineCut1,;
        TRANCD WITH '1'       ,;
        DATE   WITH gdSysDate
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  REPLACE QTY&lcI. WITH EVALUATE(lcTmpData+'.nInTranst'+lcI)
  REPLACE TOTQTY WITH TOTQTY + EVALUATE(lcTmpData+'.nInTranst'+lcI)
ENDFOR
APPEND BLANK
REPLACE CUTTKT     WITH lcCutTran ,;
        STYLE      WITH lcStyle   ,;
        LINENO     WITH lnLineCut1,;
        TRANCD     WITH '5'       ,;
        CCUTVEN    WITH 'ZZZZZZZZ',;
        CSHIP_TYPE WITH 'R'       ,;
        CCUTINV    WITH lcInv     ,;
        DATE       WITH gdSysDate
lnI = 0
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  REPLACE QTY&lcI. WITH EVALUATE(lcTmpData+'.nInTranst'+lcI)
  REPLACE TOTQTY WITH TOTQTY + EVALUATE(lcTmpData+'.nInTranst'+lcI)
ENDFOR
REPLACE TRANSHDR.TOTQTY WITH TRANSHDR.TOTQTY + TOTQTY
REPLACE CUTTKTH.PCS_BUD WITH CUTTKTH.PCS_BUD + TOTQTY
REPLACE CUTTKTH.PCS_OPN WITH CUTTKTH.PCS_BUD
SELECT (lnAlias)
*-- end of lfAddIntrn.

*!*************************************************************
*! Name      : lfGtInvTrn
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : Get the next avilable invoice # in TRANSHDR file.
*!*************************************************************
*! Example            : =lfGtInvTrn()
*!*************************************************************
*
FUNCTION lfGtInvTrn

IF EMPTY(lcInv)
  lcInv = '000000'
ENDIF
DO WHILE SEEK('ZZZZZZZZR'+lcInv,'TRANSHDR')
  IF lcInv = '999999'
    EXIT
  ENDIF
  lcInv = PADL(ALLTRIM(STR(VAL(lcInv)+1,6)),6,'0')
ENDDO
RETURN lcInv
*-- end of lfGtInvtrn.