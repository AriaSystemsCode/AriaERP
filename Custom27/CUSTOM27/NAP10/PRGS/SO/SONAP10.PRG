*:**************************************************************************
*: Program file  : SONAP10
*: Program desc. : Custom Importing Sales Order For NAP.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 07/05/2001
*: Reference     : C102391
*:**************************************************************************
*: Procedures    : lpCollect, lpTab, lpBackTab, lpRefBrow, lpTrapKey, lpCalComm
*:
*: Functions     : lfvGetFile, lfvSelCan, lfvSelOk, lfOpenFile, lfCreatFil , 
*:                 lfConvTxt, lfvSelect, lfvSelAll, lfvSelNone, lfvInvert,
*:                 lfReadAct, lfDeact, lfHdrBrow, lfwHdrBrow, lfLinBrow,
*:                 lfvCancel, lfvOk, lfChkOrd, lfUpdate
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications ..
*:**************************************************************************
*: B605965,1 RAE 06/23/2002 Fix the Start date in ORDHDR to be 2000 not 
*:                          1900 when the century set to 'ON'.
*:**************************************************************************

*-- Initializing the necessary variables.
PRIVATE lcOrdHdr , lcOrdHdr2 , lmcOrdLine , lcTmpTxtFi , lcErorFil , ;
        lcTxtFile , lcOrders , llProceed , lcBrTitle , lcBrLinTit , ;
        lnMajorLen , lcSeprator , lcGlYear , lcGlPeriod
        
STORE '' TO lcTxtFile , lcOrders , lcGlYear , lcGlPeriod
*B605965,1 RAE Variable to hold the old setting [Start]
STORE '' TO lcCent
ldSysDate = ''
*B605965,1 RAE [End]
llProceed  = .F.
DECLARE laSetups[4,2] , laDRltFld[1,2]
lnMajorLen = LEN(gfItemMask("PM"))
lcSeprator = SUBSTR(gfItemMask("HI"),lnMajorLen+1,1)

lcOrdHdr   = gfTempName()
lcOrdHdr2  = gfTempName()
lcOrdLine  = gfTempName()
lcTmpTxtFi = gfTempName()
lcErorFil  = gfTempName()
lcBrTitle  = 'Orders'
lcBrLinTit = 'Order Lines'
lcOkBmp    = gcBmpHome + "OK.bmp"
lcCanBmp   = gcBmpHome + "CAN.bmp"
lcBtMpCls =  gcBmpHome + "Close1.BMP"

*-- To select the text file that will be imported.
DO (gcScrDir+gcWinAppl+"\SONAP1E.SPX")

*-- If the user presses Cancel button.
IF !llProceed
  RETURN
ENDIF

*-- If the user did not select a file to import.
IF EMPTY(lcTxtFile) 
  *-- Message < You have to select a text file to import. Cannot proceed. >
  *-- Buttons <                            OK                             >
  =gfModalGen("INM000000B00000","DIALOG",'','','You have to select a text file to import. Cannot proceed.')
  RETURN  
ENDIF

*-- Restore the necessary variables.
laDRltFld[1,1] = 'CSLSGLLINK'
laDRltFld[1,2] = 'lcGLSales' 

laSetups[1,1] = 'M_LINK_GL'        && System has been linked to GL
laSetups[2,1] = 'M_STY_COM'        && Edit sales reps commissions at style level
laSetups[3,1] = 'M_DIV_LINK'       && Link code at division level
laSetups[4,1]  = 'M_HOLD_ORD'      && Default new order status to hold
=gfGetMemVar(@laSetups,gcAct_Comp)

llModStyGl = .F.
*llModStyGL = IIF(ALLTRIM(M_LINK_GL) = 'Y' AND TYPE('M_OrdSLnk') = 'L' ;
*                 AND M_OrdSLnk , .T. , .F.)

*-- Opening the necessary files and create the temporary files
IF lfOpenFile() AND lfCreatFil() AND lfConvTxt()
  *-- To append the converted text file into order header and line files.  
  DO lpCollect

  SELECT (lcOrdHdr)
  LOCATE
  DO (gcScrDir+gcWinAppl+"\SONAP10.SPX")
  
  *-- Selecte the rejected files if its not empty then display the screen.
  SELECT(lcErorFil)
  LOCATE
  IF !EMPTY(cErro)
    DO (gcScrDir+gcWinAppl+"\SONAP1F.SPX")
  ENDIF  
ENDIF
*-- End of Program.

*!**************************************************************************
*! Name      : lfvGetFile
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To allow the user to select the file to be imported.
*!**************************************************************************
*! Example   : =lfvGetFile()
*!**************************************************************************
*
FUNCTION lfvGetFile
lcTxtFile  = GETFILE('TXT','Select Text File')
SHOW GET lcTxtFile
IF EMPTY(lcTxtFile)
  SHOW GET pbOK DISABLE
ELSE
  SHOW GET pbOK ENABLE
ENDIF
*-- End of lfvGetFile.

*!**************************************************************************
*! Name      : lfvSelCan
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid function for the cancel button in the first screen.
*!**************************************************************************
*! Example   : =lfvSelCan()
*!**************************************************************************
*
FUNCTION lfvSelCan
llProceed = .F.
CLEAR READ
*-- End of lfvSelCan.

*!**************************************************************************
*! Name      : lfvSelOk
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid function for the Ok button in the first screen.
*!**************************************************************************
*! Example   : =lfvSelOk()
*!**************************************************************************
*
FUNCTION lfvSelOk
llProceed = .T.
CLEAR READ
*-- End of lfvSelOk

*!**************************************************************************
*! Name      : lfOpenFile
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To open the necessary files
*!**************************************************************************
*! Example   : =lfOpenFile()
*!**************************************************************************
*
FUNCTION lfOpenFile

= gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
= gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
= gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
= gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLine','SH')
= gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH')
= gfOpenFile(gcDataDir+'Codes',gcDataDir+'cCode_No','SH')
= gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
= gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
= gfOpenFile(gcDataDir+'NAPOrd',gcDataDir+'NAPOrd','SH')
= gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')

IF laSetups[1,2]='Y'
  = gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH')
  LOCATE
  IF EOF()
    *-- Message < No General Ledger link codes have been setup. Cannot proceed. >
    *-- Buttons <                            OK                             >
    =gfModalGen("TRM000000B00000","DIALOG",'','','No General Ledger link codes have been setup. Cannot proceed.')
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
*-- End of lfOpenFile

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Create the temporary files.
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
SELECT OrdHdr
=AFIELDS(laFStru)
DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cSelect'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 1
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'lReject'
laFStru[ALEN(laFStru,1),2] = 'L'
laFStru[ALEN(laFStru,1),3] = 1
laFStru[ALEN(laFStru,1),4] = 0

CREATE CURSOR (lcOrdHdr) FROM ARRAY laFStru
INDEX ON Order TAG &lcOrdHdr OF (lcOrdHdr)
INDEX ON cSelect+Order TAG &lcOrdHdr2 OF (lcOrdHdr)

SELECT OrdLine
=AFIELDS(laFStru)
CREATE CURSOR (lcOrdLine) FROM ARRAY laFStru
INDEX ON Order TAG &lcOrdLine OF (lcOrdLine)

SELECT (lcOrdLine)
SET ORDER TO lcOrdLine
SELECT (lcOrdHdr)
SET ORDER TO lcOrdHdr
SET RELATION TO Order INTO (lcOrdLine)

*-- Cursor to hold the imported orders from the text file.
CREATE CURSOR (lcTmpTxtFi) (cField1 C(8), cField2 C(14), cField3 C(10),;
              cField4 C(10), cField5 C(8), cField6 C(10), cField7 C(17),;
              cField8 C(12), cField9 C(12), cField10 C(12), cField11 C(10),;
              cField12 C(10), cField13 C(10))

*-- Cursor to hold the rejected orders.
CREATE CURSOR (lcErorFil) (cErro C(110))
*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfConvTxt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Convert the Text fiel to the temporary files.
*!**************************************************************************
*! Example   : =lfConvTxt()
*!**************************************************************************
*
FUNCTION lfConvTxt
SELECT (lcTmpTxtFi)
APPEND FROM &lcTxtFile DELIMITED WITH ,
SELECT (lcTmpTxtFi)
LOCATE
IF EOF()
  RETURN .F.
ENDIF
RETURN .T.
*-- End of lfConvTxt.

*!**************************************************************************
*! Name      : lpCollect
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Collect the date from Text file to Temp Header and temp line.
*!**************************************************************************
*! Example   : DO lpCollect
*!**************************************************************************
*
PROCEDURE lpCollect
PRIVATE lcCustPO , ldStartDat , ldComplDat , ldCanclDat

DIMENSION laQty[8]
laQty = 0
SELECT (lcTmpTxtFi)
LOCATE
SCAN WHILE cField1 <> "LINES"
  lcOrder = STRTRAN(cField1,'"',"")
  lcAcc   = STRTRAN(cField2,'"',"")
  lcStore = STRTRAN(cField3,'"',"")
  lcSeas  = STRTRAN(cField4,'"',"")
  lcDiv   = STRTRAN(cField5,'"',"") 
  lcWare  = STRTRAN(cField6,'"',"")
  lcCustPO   = STRTRAN(cField7,'"',"")

  ldStartDat = STRTRAN(cField8,'"',"")
  *B605965,1 RAE [start]
  *ldStartDat = IIF(LEN(ALLTRIM(ldStartDat))=8,SUBSTR(ldStartDat,1,6)+"20"+SUBSTR(ldStartDat,7,2),ldStartDat)
  ldStartDat = ALLTRIM(ldStartDat)
  ldSysDate = gdSysDate
  ldSysDate = DTOC(ldSysDate)
  IF LEN(ldStartDat) < 8
    *-- If the Start Date is 'M/D/' in text file.
    IF EMPTY(SUBSTR(ldStartDat,AT('/',ldStartDat,2)+1))
      ldStartDat = ldStartDat + IIF(LEN(ldSysDate)=8,SUBSTR(ldSysDate,7,2),SUBSTR(ldSysDate,7,4))
    ELSE
      *-- If the Start Date is 'M/D/YY' in text file.
      IF !EMPTY(SUBSTR(ldStartDat,AT('/',ldStartDat,2)-1))
        ldStartDat = SUBSTR(ldStartDat,1,4)+"20"+SUBSTR(ldStartDat,5,2)
      ELSE
        *-- If the Start Date is 'M/D' in text file.
        IF EMPTY(SUBSTR(ldstartdat,AT('/',ldstartdat,2)))
          ldStartDat = ldStartDat + IIF(LEN(ldSysDate)=8,SUBSTR(ldSysDate,6,3),SUBSTR(ldSysDate,6,5))
        ENDIF              
      ENDIF
    ENDIF
  ELSE
    *-- If the Start Date is 'MM/DD/YYYY' OR 'MM/DD/YY' in text file.
    ldStartDat = IIF(LEN(SUBSTR(ldstartdat,AT('/',ldstartdat,2)+1))<>4,SUBSTR(ldStartDat,1,6)+"20"+SUBSTR(ldStartDat,7,2),ldStartDat)
  ENDIF         
  *B605965,1 RAE [End]

  ldStartDat = IIF(LEN(ALLTRIM(ldStartDat))=0,'',ldStartDat)
  ldComplDat = STRTRAN(cField9,'"',"")
  ldComplDat = IIF(LEN(ALLTRIM(ldComplDat))=8,SUBSTR(ldComplDat,1,6)+"20"+SUBSTR(ldComplDat,7,2),ldComplDat)
  ldComplDat = IIF(LEN(ALLTRIM(ldComplDat))=0,'',ldComplDat)
  ldCanclDat = STRTRAN(cField10,'"',"")
  ldCanclDat = IIF(LEN(ALLTRIM(ldCanclDat))=8,SUBSTR(ldCanclDat,1,6)+"20"+SUBSTR(ldCanclDat,7,2),ldCanclDat)
  ldCanclDat = IIF(LEN(ALLTRIM(ldCanclDat))=0,'',ldCanclDat)
  *B605965,1 RAE Set century 'ON' before updating (lcOrdHdr) [Start]
  *INSERT INTO (lcOrdHdr) (Order,Account,Store,Season,cDivision,cwarecode,CustPo,Start,Complete,Cancelled);
                VALUES   (lcOrder,lcAcc,lcStore,lcSeas,lcDiv,lcWare,lcCustPO,CTOD(ldStartDat),CTOD(ldComplDat),CTOD(ldCanclDat))

  lcCent = SET("CENTURY")
  SET CENTURY ON
  INSERT INTO (lcOrdHdr) (Order,Account,Store,Season,cDivision,cwarecode,CustPo,Start,Complete,Cancelled);
                VALUES   (lcOrder,lcAcc,lcStore,lcSeas,lcDiv,lcWare,lcCustPO,CTOD(ldStartDat),CTOD(ldComplDat),CTOD(ldCanclDat))
  SET CENTURY &lcCent
  *B605965,1 RAE [End]
ENDSCAN
IF cField1 = "LINES"
  SKIP
ENDIF
SCAN REST
  lcOrder = STRTRAN(cField1,'"',"")
  lcStyle = STRTRAN(cField2,'"',"")
  lcColor = STRTRAN(cField3,'"',"")
  lnPrice = VAL(cField4)
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI+4,2))
    lnQty = VAL(cField&lcI)
    laQty[lnI] = lnQty
  ENDFOR
  lnTotQty = VAL(cField13)
  INSERT INTO (lcOrdLine) (Order,Style,Price,TotQty,cWareCode);
                VALUES    (lcOrder,lcStyle+lcSeprator+lcColor,lnPrice,lnTotQty,&lcOrdHdr..cWareCode)
  SELECT (lcOrdLine)
  =SEEK(Style,'Style')
  REPLACE Scale WITH Style.Scale
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE Qty&lcI WITH laQty[lnI]
  ENDFOR
  SELECT (lcTmpTxtFi)
ENDSCAN
*-- End of lpCollect.

*!**************************************************************************
*! Name      : lfvSelect
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid Function of select button
*!**************************************************************************
*! Example   : =lfvSelect()
*!**************************************************************************
*
FUNCTION lfvSelect
PRIVATE lnRecNo

SELECT (lcOrdHdr)
lnRecNo = RECNO()
lcOrdTag = TAG()
SET ORDER TO TAG &lcOrdHdr2
REPLACE cSelect WITH IIF(cSelect="�",'',"�")
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")

IF SEEK("�")
  SHOW GET pbSelNone ENABLE
ELSE
  SHOW GET pbSelNone  DISABLE
ENDIF
IF SEEK(" ")
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF

SET ORDER TO TAG &lcOrdTag
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
*-- End of lfvSelect.

*!**************************************************************************
*! Name      : lfvSelAll
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid Function of select All button
*!**************************************************************************
*! Example   : =lfvSelAll()
*!**************************************************************************
*
FUNCTION lfvSelAll
PRIVATE lnRecNo
SELECT (lcOrdHdr)
lnRecNo = RECNO()
REPLACE ALL cSelect WITH "�"

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")
SHOW GET pbSelAll DISABLE
SHOW GET pbSelNone  ENABLE
*-- End of lfvSelAll.

*!**************************************************************************
*! Name      : lfvSelNone
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid Function of select None button
*!**************************************************************************
*! Example   : =lfvSelNone()
*!**************************************************************************
*
FUNCTION lfvSelNone
PRIVATE lnRecNo
SELECT (lcOrdHdr)
lnRecNo = RECNO()
REPLACE ALL cSelect WITH " "

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")
SHOW GET pbSelAll   ENABLE
SHOW GET pbSelNone  DISABLE
*-- End of lfvSelNone.

*!**************************************************************************
*! Name      : lfvInvert
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid Function of Invert button
*!**************************************************************************
*! Example   : =lfvInvert()
*!**************************************************************************
*
FUNCTION lfvInvert
PRIVATE lnRecNo

SELECT (lcOrdHdr)
lnRecNo = RECNO()
REPLACE ALL cSelect WITH IIF(cSelect="�",'',"�")
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")

lcOrdTag = TAG()
SET ORDER TO TAG &lcOrdHdr2

IF SEEK("�")
  SHOW GET pbSelNone ENABLE
ELSE
  SHOW GET pbSelNone  DISABLE
ENDIF
IF SEEK(" ")
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF

SET ORDER TO TAG &lcOrdTag

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
*-- End of lfvInvert.

*!**************************************************************************
*! Name      : lpTab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To trap the TAB key
*!**************************************************************************
*! Example   : DO lpTab
*!**************************************************************************
*
PROCEDURE lpTab
DO CASE
  *-- When the window on top is Orders
  CASE WONTOP() = lcBrTitle    
    DO lpRefBrow WITH 'H'  
    DO lpTrapKey WITH 'SONAP1C', 'PbSelect', .F.
  *-- When the window on top is Order lines
  CASE WONTOP() = lcBrLinTit
    DO lpTrapKey WITH 'SONAP1D', 'PbOk', .F.
ENDCASE
*-- End of lpTab. 

*!**************************************************************************
*! Name      : lpBackTab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To trap the BACKTAB key
*!**************************************************************************
*! Example   : DO lpBackTab
*!**************************************************************************
*
PROCEDURE lpBackTab
DO CASE
  *-- When the window on top is Orders
  CASE WONTOP() = lcBrTitle
    DO lpTrapKey WITH 'SONAP1D', 'PbCancel', .F.
  *-- When the window on top is Order lines
  CASE WONTOP() = lcBrLinTit
    DO lpTrapKey WITH 'SONAP1C', 'PbInvert', .F.
ENDCASE  
*-- End of lpBackTab.

*!**************************************************************************
*! Name      : lpRefBrow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To activate the necessary browse
*!**************************************************************************
*! Example   : DO lpRefBrow
*!**************************************************************************
*
PROCEDURE lpRefBrow
PARAMETERS lcBrwTit

ON KEY LABEL CTRL+Q    lnDummy = 1
ON KEY LABEL CTRL+W    lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM

IF lcBrwTit = 'H'
  ACTIVATE WINDOW (lcBrLinTit)
  SHOW WINDOW (lcBrLinTit) REFRESH SAME
ELSE
  ACTIVATE WINDOW (lcBrTitle)
  SHOW WINDOW (lcBrTitle) REFRESH SAME
ENDIF
*-- End of lpRefBrow.

*!**************************************************************************
*! Name      : lfReadAct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Uninitializing the keys.
*!**************************************************************************
*! Example   : =lfReadAct()
*!**************************************************************************
*
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 
*-- End of lfReadAct.

*!**************************************************************************
*! Name      : lfDeact
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Traping the Keys
*!**************************************************************************
*! Example   : =lfDeact()
*!**************************************************************************
*
FUNCTION lfDeact
ON KEY LABEL ESC     DO lpTrapKey WITH 'SONAP1D', 'PbCancel', .T.
ON KEY LABEL TAB     DO lpTab
ON KEY LABEL BACKTAB DO lpBackTab
RETURN .F.
*-- End of lfDeact.

*!**************************************************************************
*! Name      : lpTrapKey
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To Trap the Keys.
*!**************************************************************************
*! Example   : =lfDeact()
*!**************************************************************************
*
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF
*-- End of lpTrapKey.

*!**************************************************************************
*! Name      : lfHdrBrow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To browse the orders
*!**************************************************************************
*! Example   : =lfHdrBrow()
*!**************************************************************************
*
FUNCTION lfHdrBrow
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcOrdHdr)
LOCATE
lcBrowFild = "cSelect   :H= '�':2:R,"+;
             "Order     :H = 'Order'      :10:R,"+;
             "Store     :H = 'Store'      :16:R,"+;
             "Account   :H = 'Account'    :16:R,"+;
             "Season    :H = 'Season'     :16:R,"+;
             "cDivision :H = 'Division'   :16:R,"+;
             "cWareCode :H = 'Ware House' :18:R"

BROWSE FIELDS &lcBrowFild ;
         WINDOW    SONAP1A ;
         IN WINDOW SONAP10 ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         NOEDIT            ;
         LOCK 0            ;
         WHEN lfwHdrBrow() ;
         TITLE lcBrTitle
SHOW WINDOW (lcBrTitle) REFRESH SAME

SELECT(lnAlias)
*-- End of lfHdrBrow.

*!**************************************************************************
*! Name      : lfwHdrBrow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To Change the Select button according to the selection
*!**************************************************************************
*! Example   : =lfwHdrBrow()
*!**************************************************************************
*
FUNCTION lfwHdrBrow
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")
*-- End of lfwHdrBrow.

*!**************************************************************************
*! Name      : lfLinBrow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To browse the order lines
*!**************************************************************************
*! Example   : =lfLinBrow()
*!**************************************************************************
*
FUNCTION lfLinBrow
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcOrdLine)
LOCATE
lcBrowLin =  "Order   :H = 'Order'   :08:R,"+;
             "Style   :H = 'Style'   :16:R,"+;
             "Price   :H = 'Price'   :10:R,"+;
             "Qty1    :H = 'Qty1'    :08:R,"+;
             "Qty2    :H = 'Qty2'    :08:R,"+;
             "Qty3    :H = 'Qty3'    :08:R,"+;
             "Qty4    :H = 'Qty4'    :08:R,"+;
             "Qty5    :H = 'Qty5'    :08:R,"+;
             "Qty6    :H = 'Qty6'    :08:R,"+;
             "Qty7    :H = 'Qty7'    :08:R,"+;
             "TotQty  :H = 'TotQty'  :10:R"                                                                                           

BROWSE FIELDS &lcBrowLin ;
         WINDOW    SONAP1B ;
         IN WINDOW SONAP10 ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         NOEDIT            ;
         LOCK 0            ;
         TITLE lcBrLinTit
SHOW WINDOW (lcBrLinTit) REFRESH SAME
SELECT(lnAlias)
*-- End of lfLinBrow.

*!**************************************************************************
*! Name      : lfvCancel
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid function of the cancel button.
*!**************************************************************************
*! Example   : =lfvCancel()
*!**************************************************************************
*
FUNCTION lfvCancel
CLEAR READ
*-- End of lfvCancel.

*!**************************************************************************
*! Name      : lfvOk
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : Valid function of the Ok button to save the selected orders.
*!**************************************************************************
*! Example   : =lfvOk()
*!**************************************************************************
*
FUNCTION lfvOk

*-- To check the whether the order will be imported or rejected.
=lfChkOrd()

*-- Update the selected orders.
=lfUpdate()
IF !EMPTY(lcOrders)
  *-- Message < Generated orders: From 030021 To 030029 >
  *-- Buttons <                   OK                    >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Generated orders: From '+LEFT(lcOrders,6)+' To '+RIGHT(lcOrders,6))
ENDIF
CLEAR READ
*-- End of lfvOk.

*!**************************************************************************
*! Name      : lfChkOrd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To check whether the orders will be rejected or not.
*!**************************************************************************
*! Example   : =lfChkOrd()
*!**************************************************************************
*
FUNCTION lfChkOrd
PRIVATE   lcOrderNo, lcSeason, lcDivision

SELECT (lcOrdHdr)
SET ORDER TO TAG &lcOrdHdr2

IF !SEEK("�")
  *-- Message < No order has been selected. >
  *-- Buttons <              OK             >
  =gfModalGen("INM000000B00000","DIALOG",'','','No order has been selected.')
  CLEAR READ
  RETURN
ENDIF

SCAN REST WHILE cSelect+Order = "�"
  
  *-- Hold the order number, Season and Division
  lcOrderNo  = Order
  lcSeason   = Season
  lcDivision = cDivision

      
  *-- If the order has been imported before.
  IF SEEK(SUBSTR(Order,1,6),'NapOrd')
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' was rejected because it has been imported before.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.
    LOOP
  ENDIF
      
  *-- If empty account
  IF EMPTY(Account)
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected because this order without account.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.    
  ENDIF
  
  *-- If the main account does not found in the customer file.
  IF !SEEK('M'+Account,'Customer')
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected because '+'Account # '+&lcOrdHdr..Account+;
                                ' does not exist in the customer file.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.    
  ENDIF
  
  *-- If status of the customer is not active
  IF SEEK('M'+Account,'Customer') AND Customer.Status <> 'A'
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected because '+'Account # '+&lcOrdHdr..Account+;
                                ' is a non-active account.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.
  ENDIF  

  *-- If Season does not found in the codes file.
  IF !SEEK('NSEASON    ' + Season,'Codes')
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                       'because season : '+ALLTRIM(&lcOrdHdr..Season)+' does not exist in the codes file.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.
  ENDIF  

  *-- If Division does not found in the codes file.
  IF !SEEK('NCDIVISION ' + CDIVISION,'Codes')
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                       'because Division : '+ALLTRIM(&lcOrdHdr..cDivision)+ ' does not exist in the codes file.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.    
  ENDIF
  
  *-- If WareHouse does not found in WareHouse file.
  IF !SEEK(cWareCode,'WareHous')
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                       'because Ware house code : '+ALLTRIM(&lcOrdHdr..cWareCode)+' does not exist in Ware house file.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.    
  ENDIF
  
  *-- If not seek Account+store in the customer file
  IF !EMPTY(Store)
    IF !SEEK('S'+Account+Store,'Customer')
      SELECT (lcErorFil)
      APPEND BLANK
      REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                         'because Account+Store : '+ALLTRIM(&lcOrdHdr..Account)+'+'+;
                         ALLTRIM(&lcOrdHdr..Store)+' does not exist in the customer file.'
      SELECT (lcOrdHdr)
      REPLACE lReject WITH .T.    
    ELSE
      IF Customer.Status <> 'A'
        SELECT (lcErorFil)
        APPEND BLANK
        REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+;
                           ' has been rejected because '+'Account/Store '+;
                            &lcOrdHdr..Account+'/'+ALLTRIM(&lcOrdHdr..Store)+;
                            ' is a non-active account.'
        SELECT (lcOrdHdr)
        REPLACE lReject WITH .T.    
      ENDIF
    ENDIF  
  ENDIF
  
  *-- If there are no lines for the orders
  IF EOF(lcOrdLine)
    SELECT (lcErorFil)
    APPEND BLANK
    REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+;
                       ' has been rejected because there are no lines.'
    SELECT (lcOrdHdr)
    REPLACE lReject WITH .T.    
  ENDIF

  *-- Looping in the order lines.
  SELECT (lcOrdLine)
  SEEK lcOrderNo
  SCAN REST WHILE Order = lcOrderNo
  
    *--If style does not exist in the style file.
    IF !SEEK(Style,'Style')
      SELECT (lcErorFil)
      APPEND BLANK
      REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                         'because Style/Color : '+ALLTRIM(&lcOrdLine..Style)+' does not exist in the style file.'
      REPLACE &lcOrdHdr..lReject WITH .T.    
      SELECT (lcOrdLine)
    ENDIF
      
    *-- If color does not exist in the codes file.  
    IF !SEEK('NCOLOR     '+ALLTRIM(SUBSTR(&lcOrdLine..Style,lnMajorLen+2)),'Codes')  
      SELECT (lcErorFil)
      APPEND BLANK
      REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected because '+;
                         'Color : '+ALLTRIM(SUBSTR(&lcOrdLine..Style,lnMajorLen+2))+' does not exist in the codes file.'

      REPLACE &lcOrdHdr..lReject WITH .T.    
      SELECT (lcOrdLine)
    ENDIF

    *-- If style does not exist in the style file.
    *IF !SEEK(Style,'Style')
    *  SELECT (lcErorFil)
    *  APPEND BLANK
    *  REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
    *                     'because Style/Color : '+ALLTRIM(&lcOrdLine..Style)+;
    *                     ' does not exist in the style file.'
    *  REPLACE &lcOrdHdr..lReject WITH .T.    
    *  SELECT (lcOrdLine)
    *ENDIF

    *-- If style season is different than ordhdr season
    IF SEEK(Style,'Style') AND (Style.Season <> lcSeason)
      SELECT (lcErorFil)
      APPEND BLANK
      REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                         'because Style/Season : '+ALLTRIM(&lcOrdLine..Style)+;
                         '/'+ALLTRIM(Style.Season)+' is different than order season '+lcSeason +'.'

      REPLACE &lcOrdHdr..lReject WITH .T.    
      SELECT (lcOrdLine)
    ENDIF

    *-- If style division is different than ordhdr division
    IF SEEK(Style,'Style') AND (Style.cDivision <> lcDivision)
      SELECT (lcErorFil)
      APPEND BLANK
      REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                         'because Style/Division : '+ALLTRIM(&lcOrdLine..Style)+;
                         '/'+ALLTRIM(Style.cDivision)+' is different than order division '+lcDivision+'.'

      REPLACE &lcOrdHdr..lReject WITH .T.    
      SELECT (lcOrdLine)
    ENDIF
     
    *-- If style is cancelled
    IF SEEK(Style,'Style') AND Style.Status = 'X'
      SELECT (lcErorFil)
      APPEND BLANK
      REPLACE cErro WITH 'Order # '+ALLTRIM(lcOrderNo)+' has been rejected '+;
                         'because Style '+ALLTRIM(&lcOrdLine..Style)+' has a cancelled status.'

      REPLACE &lcOrdHdr..lReject WITH .T.    
      SELECT (lcOrdLine)
    ENDIF
  ENDSCAN
ENDSCAN
*-- End of lfChkOrd.

*!**************************************************************************
*! Name      : lfUpdate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To update the order in the master files
*!**************************************************************************
*! Example   : =lfUpdate()
*!**************************************************************************
*
FUNCTION lfUpdate
PRIVATE lImport
SELECT (lcOrdHdr)
SET ORDER TO TAG &lcOrdHdr2
lImport = .F.
*B605965,1 RAE Change DATE() to gdSysDate [Start]
*IF !CHECKPRD(DATE(),'lcGlYear','lcGlPeriod','',.T.)
IF !CHECKPRD(gdSysDate,'lcGlYear','lcGlPeriod','',.T.)
*B605965,1 RAE [End]
  STORE SPACE(4) TO lcGlYear
  STORE SPACE(2) TO lcGlPeriod
ENDIF

SEEK "�"
SCAN REST WHILE cSelect+Order = "�" FOR !lReject 
  lImport = .T.
  
  *-- Get the division to be passed to the sequence function.
  lcDivCode = cDivision
  =SEEK('NCDIVISION '+lcDivCode,'Codes')
  
  *-- Generate a new SO number.  
  lcOrderNo = gfSequence('ORDER','','',lcDivCode)
  lcOrders  = lcOrders + lcOrderNo
  lcAccount = Account
  lcStore   = Store
  STORE 0 TO lnBook, lnBookAmt
  
  *-- Get Customer information
  =SEEK('M'+Account,'Customer')
  lcFactor = Customer.cFacCode
  
  * Amin
  lcBuyer = Customer.Buyer     
  lcTermCode = Customer.cTermCode 
  lcShipVia   = Customer.ShipVia   
  lcSpcInst   = Customer.SpcInst
  lcInsur    = Customer.cInsur 
  lcPriority  = Customer.Priority
  lcCurrCode = Customer.cCurrCode 
  lcDisc      = Customer.Disc      
  lcPriceLvl = Customer.PriceLvl
  * Amin 

  *amin
  *=IIF(EMPTY(Store),SEEK('M'+Account,'Customer'),SEEK('S'+Account+Store,'Customer'))
  *lcPriceLvl = Customer.PriceLvl
  *amin
  
  *-- If the customer is linked to Gl or not.

  lcStatus   = IIF( !EMPTY(lcFactor) OR laSetups[4,2]='Y', 'H' , 'O' )
  *B605965,1 RAE Change DATE() to gdSysDate [Start]
  *ldEntered  = DATE()
  *ldStart    = IIF(EMPTY(Start),DATE(),Start)
  *ldComplete = IIF(EMPTY(Complete),DATE() + 90,Complete)
  ldEntered  = gdSysDate
  ldStart    = IIF(EMPTY(Start),gdSysDate,Start)
  ldComplete = IIF(EMPTY(Complete),gdSysDate + 90,Complete)
  *B605965,1 RAE [End]
  lcCustPoNo = CustPo
  ldCancel   = Cancelled

  lnComm1    = Customer.Comm
  lnComm2    = Customer.Comm2
  lnHComm1   = lnComm1
  lnHComm2   = lnComm2


  *-- Adding a record to the ord header file
  SELECT OrdHdr
  APPEND BLANK

  REPLACE cOrdType  WITH 'O'                ,;
          Order     WITH lcOrderNo          ,;
          Status    WITH lcStatus           ,;
          Account   WITH &lcOrdHdr..Account ,;
          Store     WITH &lcOrdHdr..Store   ,;
          Season    WITH &lcOrdHdr..Season  ,;
          cDivision WITH &lcOrdHdr..cDivision,;
          Entered   WITH ldEntered          ,;
          Start     WITH ldStart            ,;
          Complete  WITH ldComplete         ,;
          Cancelled WITH ldCancel           ,; 
          Bulk      WITH 'N'                ,;
          Multi     WITH 'N'                ,;
          MultiPO   WITH .F.                ,; 
          cReOrder  WITH 'N'                ,;
          CustPo    WITH lcCustPoNo

  *REPLACE Buyer     WITH Customer.Buyer     ,;
          cTermCode WITH Customer.cTermCode ,;
          ShipVia   WITH Customer.ShipVia   ,;
          SpcInst   WITH Customer.SpcInst   ,;
          cInsur    WITH Customer.cInsur    ,;
          Priority  WITH Customer.Priority  ,;          
          Phone     WITH Customer.Phone1    ,;
          cCurrCode WITH Customer.cCurrCode ,;
          nExRate   WITH 1                  ,;
          nCurrUnit WITH 1                  ,;
          cFacCode  WITH lcFactor           ,;
          Rep1      WITH Customer.SalesRep  ,;
          Comm1     WITH lnComm1            ,;
          Rep2      WITH Customer.Rep2      ,;
          Comm2     WITH lnComm2            ,;
          Disc      WITH Customer.Disc      ,; 
          Note1     WITH Customer.Note      ,;
          cWareCode WITH &lcOrdHdr..cWareCode


 *  Amin
  REPLACE Buyer     WITH lcBuyer     ,;
          cTermCode WITH lcTermCode  ,;
          ShipVia   WITH lcShipVia   ,;
          SpcInst   WITH lcSpcInst   ,;
          cInsur    WITH  lcInsur     ,;
          Priority  WITH lcPriority   ,;          
          Phone     WITH Customer.Phone1    ,;
          cCurrCode WITH Customer.cCurrCode ,;
          nExRate   WITH 1                  ,;
          nCurrUnit WITH 1                  ,;
          cFacCode  WITH lcFactor           ,;
          Rep1      WITH Customer.SalesRep  ,;
          Comm1     WITH lnComm1            ,;
          Rep2      WITH Customer.Rep2      ,;
          Comm2     WITH lnComm2            ,;
          Disc      WITH lcDisc          ,; 
          Note1     WITH Customer.Note      ,;
          cWareCode WITH &lcOrdHdr..cWareCode
 *  Amin

   STORE '' TO lcGLSales
   IF laSetups[3,2]='Y'
     =gfRltFld(&lcOrdHdr..cDivision,@laDRltFld,'CDIVISION')
   ELSE
     lcGLSales = IIF(SEEK('M'+Account,'Customer'),Customer.cSlsGlLink,'')
   ENDIF
   lcGLSales = IIF(EMPTY(lcGLSales),'DEF',lcGLSales)
   REPLACE Gl_Sales WITH lcGLSales

   *IF laSetups[1,2]='Y'
     REPLACE LINK_CODE WITH IIF(SEEK('M'+Account,'Customer') AND EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
   *ENDIF
   
   *-- Calculating the commission
   IF OrdHdr.Disc > 0
      DO lpCalComm WITH OrdHdr.Disc,.F.
    ENDIF

   REPLACE Comm1      WITH lnComm1 , Comm2      WITH lnComm2
   =gfAdd_Info('OrdHdr')
      
  *-- To hold the line numbers in the order line file.
  lnLineNo = 0  
  
  *-- Adding the order lines.
  SELECT (lcOrdLine)
  *SEEK lcOrderNo
  SCAN REST WHILE Order = &lcOrdHdr..Order
    lcStyle = Style
    =SEEK(lcStyle,'Style')
    
    *-- Incrementing the order line.
    lnLineNo = lnLineNo + 1
 
    SELECT OrdLine
    APPEND BLANK 
    *lcGLSales = ALLTRIM(lcGLSales)+Style.cSlsGlLink
    *lcGLSales = IIF(laSetups[1,2]='Y' AND SEEK('02'+ALLTRIM(lcGLSales)+Style.cSlsGlLink,'Gl_Link'),lcGLSales,'DEFDEF')

    REPLACE cOrdType  WITH 'O'          ,;
            Order     WITH lcOrderNo    ,;
            LineNo    WITH lnLineNo     ,;
            Account   WITH lcAccount    ,;
            Store     WITH lcStore      ,;
            Style     WITH lcStyle      ,;
            Scale     WITH Style.Scale  ,;
            PrePak    WITH Style.PrePak ,;
            Start     WITH ldStart      ,;
            Complete  WITH ldComplete   ,;
            GL_Sales  WITH IIF(laSetups[1,2]='Y' AND SEEK('02'+ALLTRIM(lcGLSales)+Style.cSlsGlLink,'Gl_Link'),ALLTRIM(lcGLSales)+Style.cSlsGlLink,'DEFDEF') ,;
            Desc1     WITH Style.Desc1  ,;  
            cWareCode WITH &lcOrdHdr..cWareCode ,;
            Season    WITH &lcOrdHdr..Season
    
    FOR lnCount = 1 TO 8     
      lcCount = STR(lnCount,1)
      REPLACE Qty&lcCount WITH &lcOrdLine..Qty&lcCount
      REPLACE Book&lcCount WITH &lcOrdLine..Qty&lcCount
    ENDFOR
    REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    REPLACE TotBook WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
       
    IF laSetups[2,2] = 'Y' AND Style.Commission
      REPLACE Comm1 WITH lnComm1 , Comm2 WITH lnComm2
    ENDIF
    
    DO CASE
      CASE (lcPriceLvl ='A') OR EMPTY(lcPriceLvl)
        REPLACE Price WITH Style.PriceA , Gros_Price WITH Style.PriceA
      CASE lcPriceLvl = 'B'
        REPLACE Price WITH Style.PriceB , Gros_Price WITH Style.PriceB
      CASE lcPriceLvl = 'C'
        REPLACE Price WITH Style.PriceC , Gros_Price WITH Style.PriceC
      CASE lcPriceLvl = 'Q'
        DO CASE
           CASE Style.nAtQtyC > 0 AND OrdLine.TotQty  > Style.nAtQtyC          
             REPLACE Price WITH Style.PriceC , Gros_Price WITH Style.PriceC

           CASE Style.nAtQtyB > 0 AND OrdLine.TotQty   >Style.nAtQtyB          
             REPLACE Price WITH Style.PriceB , Gros_Price WITH Style.PriceB

          OTHERWISE
            REPLACE Price WITH Style.PriceA , Gros_Price WITH Style.PriceA
        ENDCASE
    ENDCASE
    
    *-- If adding the style gl link code to the order lines.
    IF llModStyGL
      REPLACE GL_Cost WITH IIF(EMPTY(OrdHdr.GL_Cost),Style.Link_Code,OrdHdr.GL_Cost)
    ENDIF
      
    *-- Calculating the order qty in the style file.      
    IF SEEK(lcStyle,'Style')  
      SELECT STYLE
      =RLOCK()
      REPLACE ORD1 WITH ORD1+&lcOrdLine..QTY1,;
              ORD2 WITH ORD2+&lcOrdLine..QTY2,;
              ORD3 WITH ORD3+&lcOrdLine..QTY3,;
              ORD4 WITH ORD4+&lcOrdLine..QTY4,;
              ORD5 WITH ORD5+&lcOrdLine..QTY5,;
              ORD6 WITH ORD6+&lcOrdLine..QTY6,;
              ORD7 WITH ORD7+&lcOrdLine..QTY7,;
              ORD8 WITH ORD8+&lcOrdLine..QTY8,;
              TOTORD WITH TOTORD+&lcOrdLine..TOTQTY
      UNLOCK
    ENDIF

    *-- Calculating the order qty in the style file.
    SELECT StyDye
    IF !SEEK(lcStyle+&lcOrdHdr..cWareCode+SPACE(10))
      APPEND BLANK
      REPLACE Style WITH lcStyle , cWareCode WITH &lcOrdHdr..cWareCode
    ENDIF
    =RLOCK()
    REPLACE ORD1 WITH ORD1+&lcOrdLine..QTY1,;
            ORD2 WITH ORD2+&lcOrdLine..QTY2,;
            ORD3 WITH ORD3+&lcOrdLine..QTY3,;
            ORD4 WITH ORD4+&lcOrdLine..QTY4,;
            ORD5 WITH ORD5+&lcOrdLine..QTY5,;
            ORD6 WITH ORD6+&lcOrdLine..QTY6,;
            ORD7 WITH ORD7+&lcOrdLine..QTY7,;
            ORD8 WITH ORD8+&lcOrdLine..QTY8,;
            TOTORD WITH TOTORD+&lcOrdLine..TOTQTY
    UNLOCK
    
    *-- Canculating the qty and amount.
    SELECT (lcOrdLine)
    lnBook    = lnBook + OrdLine.TotQty
    lnBookAmt = lnBookAmt+(OrdLine.Price * OrdLine.TotQty)
    * AMIN
    *-- Replacing the qty and amount in the ordhdr file

  ENDSCAN

   *-- Replacing the qty and amount in the order header file.
  SELECT OrdHdr
  REPLACE Book     WITH lnBook, BookAmt WITH lnBookAmt,;
          Open     WITH lnBook, OpenAmt WITH lnBookAmt,;
          LastLine WITH lnLineNo

  IF OrdHdr.Status<>'B' AND SEEK(OrdHdr.Account + lcGlYear,'arCusHst')
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + IIF(OrdHdr.Status='B',0,OrdHdr.Open),;
            nOrdQty WITH nOrdQty + IIF(OrdHdr.Status='B',0,OrdHdr.Open),;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnBookAmt,;
            nOrdAmt WITH nOrdAmt + lnBookAmt
    UNLOCK
  ENDIF
   
  SELECT NapOrd
  APPEND BLANK
  REPLACE cOrder WITH &lcOrdHdr..Order
  SELECT (lcOrdHdr)
ENDSCAN

IF !lImport
  *-- Message < No order has been imported. >
  *-- Buttons <              OK               >
  =gfModalGen("INM000000B00000","DIALOG",'','','No order has been imported.')
  RETURN .F.
ENDIF
*-- End of lfUpdate.

*!**************************************************************************
*! Name      : lpCalComm
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/05/2001
*! Purpose   : To calculate the commisions
*!**************************************************************************
*! Example   : DO lpCalComm
*!**************************************************************************
*
PROCEDURE lpCalComm
PARAMETERS lnDisct,llLine,llMsgDisp

=IIF(EMPTY(lcStore),SEEK('M'+Account,'Customer'),SEEK('S'+Account+lcStore,'Customer'))
FOR I=1 TO 2
  Z=STR(I,1)
  XOld_Comm&Z = Comm&Z
  lnComm&Z    = Comm&Z
  
  IF SEEK(OrdHdr.Rep&Z,'SALESREP')
    DO CASE
      CASE lnDisct >= SALESREP.Discnt4
        lnComm&Z = lnComm&Z * (1-SALESREP.Redcom4/100)
      CASE lnDisct >= SALESREP.Discnt3
        lnComm&Z = lnComm&Z * (1-SALESREP.Redcom3/100)
      CASE lnDisct >= SALESREP.Discnt2
        lnComm&Z = lnComm&Z * (1-SALESREP.Redcom2/100)
      CASE lnDisct >= SALESREP.Discnt1
        lnComm&Z = lnComm&Z * (1-SALESREP.Redcom1/100)
    ENDCASE
  ENDIF  
  IF EMPTY(OrdHdr.Rep2)
    EXIT
  ENDIF
ENDFOR
RETURN
*-- End of lpCalComm.


*OCCUR('\',"1\0\")
*SUBSTR("1\0\",AT('\',"1\0\",2)+1)  
