*:***************************************************************************
*: Program file  : POKRA90.PRG
*: Program desc. : Style Shipments and P/O inquire screen for Krazy Kat.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Purchase Order (PO)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Krazy Kat Ltd.
*:***************************************************************************
*: Passed Parameters : 
*:           lcStyle : if the program is called from another inquire screen,
*:                     lcStyle will take a value,else it will be empty 
*:           lcColor : Same as lcStyle           
*:***************************************************************************
*: C101642,1 KHM 10/17/1999
*:***************************************************************************
*: Modifications:
*  B603388,1 ADEL 01/13/2000 Fix the bug of displaying the recieve qty of the whole
*  B603388,1                 P/O instead of the qty of the selected style/color only.
*:***************************************************************************

PARAMETERS lcStyle,lcColor


lcAliasN   = gfTempName()
lcMcompdir = ''
IF !lfCrtSetup()
  RETURN
ENDIF

*-- Initialize the variables
lcIMjrPt   = gfItemMask('PI')
lcMjrPct   = gfItemMask('PM')
lnStyleWid = LEN(lcMjrPct)
lcSepart   = SUBSTR(lcIMjrPt,lnstylewid+1,1)
lcNonMajTl = ''
lcNonMajPi = ''
STORE 0 TO lnFreeLen , lnColorLen , lnMajSeg,lnNonMajSt
lnMajLen = LEN(gfItemMask('PM'))
=lfGetColor()

llbrowse   = .F.
llCalled   = !(TYPE("lcStyle")$"LU" AND TYPE("lcColor")$"LU")
lcStatus   = IIF(llCalled,"DISABLE","ENABLE")
lcNstatus  = IIF(llCalled,"ENABLE","DISABLE")
lcShowP    = "ENABLE"
lcStyle    = IIF(TYPE("lcStyle")$"LU",SPACE(lnMajLen),lcStyle)
lcColor    = IIF(TYPE("lcColor")$"LU",SPACE(lnColorLen),lcColor)
lcColDesc  = SPACE(15)
lcDesc     = SPACE(20)
STORE 0 TO lnBalance,lnTotOrd,lnTotRec,lnMarker
lcPONum    = SPACE(06)
lcBrowTtl  = "P/O lines"
lcShipTtl  = "Shipments Information"
lcWinTitle = "Style Purchase Orders Inquire"  +" (" + gcAct_Comp + ")"
lcNTitle   = 'Discrepency Notes'
llDNote    = .T.
lcNotes    = SPACE(10)

TempPosln  = gfTempName()
TempShipm  = gfTempName()

IF !gfSetup()
  RETURN
ENDIF  

=gfOpenFile(gcDataDir+'NotePad','NotePad','SH')
=gfOpenFile(gcDataDir+"Style","Style","SH")
=gfOpenFile(gcDataDir+"Codes","Codes","SH")

=gfOpenFile(lcmCompDir+"ITLCSHIP",lcmCompDir+"SHIPNUM","SH")
=gfOpenFile(lcmCompDir+"ITHBILLT",lcmCompDir+"HBILLNUM","SH")
=gfOpenFile(lcmCompDir+"ITPOSHIP",lcmCompDir+"lineNo","SH")
=gfOpenFile(lcmCompDir+"ITSETUP"," ","SH")
SELECT ItSetUp
GOTO TOP

=gfOpenFile(gcDataDir+"PoShdr","PoShdr","SH")
=gfOpenFile(gcDataDir+"PoSLn","PoSLnS","SH")
SET RELATION TO cStyType+Po INTO PoSHdr ADDITIVE

SELECT PoSHdr
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cMarker'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
CREATE CURSOR (TEMPPOSLN) FROM ARRAY laFileStru

DO gcScrDir+'PO\POKRA900.SPX'

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Validation for the entry of Style
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lnAlias,lcStyOrder

lnAlias = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

*-- Case the user wants the browse of Styles or type a wrong Style
IF llBrowse OR (!EMPTY(lcStyle) AND !SEEK(lcStyle,"STYLE"))
  lcStyle  = gfStyBrw('M',"","",.F.)
  llBrowse = .F.
ENDIF

IF !EMPTY(lcStyle) AND !EMPTY(lcColor)
  SHOW GET ibStyle  DISABLE
  SHOW GET lcStyle  DISABLE
  SHOW GET ibColor  DISABLE
  SHOW GET lcColor  DISABLE
  SHOW GET pbSNotes ENABLE
  IF llCalled
    SHOW GET pbSelect DISABLE
  ELSE    
    SHOW GET pbSelect ENABLE
  ENDIF
  =lfActBrow()
ENDIF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Validation for the Color key
*!*************************************************************
FUNCTION lfvColor

llbrowse = llbrowse OR '?' $ lcColor
lcColor  = IIF(llbrowse,'?',lcColor)

IF EMPTY(lcStyle) AND EMPTY(lcColor)
  RETURN
ENDIF

IF llBrowse OR !EMPTY(lcColor)
  IF !EMPTY(lcStyle)
    IF !SEEK(LEFT(lcStyle,lnMajLen)+lcSepart+lcColor,'Style')
      lcColor = gfStyBrw('N',lcStyle,@lcColor,.F.)
    ENDIF
  ELSE
    lcItemCd = gfStyBrw('I',lcStyle+lcSepart+lcColor,'',.F.)
    lcStyle  = SUBSTR(lcItemCd ,1,lnstylewid)
    lcColor  = SUBSTR(lcItemCd ,lnNonMajSt,lnColorLen)
  ENDIF
  llBrowse = .F.
ENDIF

IF !EMPTY(lcColor) AND !EMPTY(lcStyle)
  SHOW GET lcColor  DISABLE  
  SHOW GET ibColor  DISABLE  
  SHOW GET lcStyle  DISABLE  
  SHOW GET ibStyle  DISABLE  
  SHOW GET pbSNotes ENABLE
  IF llCalled
    SHOW GET pbSelect DISABLE
  ELSE    
    SHOW GET pbSelect ENABLE
  ENDIF  
  =lfActBrow()
ENDIF

*!*************************************************************
*! Name      : lfActBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : select the required information and browse it
*!*************************************************************
*! Calls       : =lfActBrow()
*!*************************************************************
FUNCTION lfActBrow

IF !EMPTY(lcStyle) AND !EMPTY(lcColor)
  lcDesc    = STYLE.DESC
  lcColDesc = SUBSTR(gfCodDes(lcColor,'COLOR     '),1,15)
  SELECT PoSLn
  IF SEEK(Style.Style)
    *B603388,1 (Begin) Remark the following lines and get the recieved qty from record type "2".
    *SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = Style.Style+'P';
              FOR TranCd = '1'
      *SELECT (TempPosln)
      *APPEND BLANK
      *REPLACE Vendor    WITH PosHdr.Vendor   ,;
              Po        WITH PoSHdr.Po       ,;
              nStyOrder WITH PoSLn.TotQty    ,;
              Receive   WITH PoSHdr.Receive  ,;
              Complete  WITH PosHdr.Complete ,;
              ShipVia   WITH PoShdr.ShipVia  ,;
              Contact   WITH PoShdr.Contact  ,;
              Available WITH PoShdr.Available,; 
              cFob      WITH PoShdr.cFob
    
    SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = Style.Style+'P';
              FOR TranCd  $ "12"
      SELECT (TempPosln)
      IF PoSLn.TranCd = '1'
        APPEND BLANK
        REPLACE Vendor    WITH PosHdr.Vendor   ,;
                Po        WITH PoSHdr.Po       ,;
                nStyOrder WITH PoSLn.TotQty    ,;
                Complete  WITH PosHdr.Complete ,;
                ShipVia   WITH PoShdr.ShipVia  ,;
                Contact   WITH PoShdr.Contact  ,;
                Available WITH PoShdr.Available,; 
                cFob      WITH PoShdr.cFob
      ELSE
        REPLACE Receive   WITH Receive   + PoSLn.TotQty
      ENDIF               
      *B603388,1 (End)       
    ENDSCAN
  ENDIF
ELSE
  STORE '' TO lcDesc,lcColDesc
  SELECT (TempPosln)
  ZAP
ENDIF
SELECT (TempPosln)
GOTO TOP
lcBrFields = "cMarker =IIF(RECNO()=lnMarker,'',' '):H=' ':R:1:W=.F.,"+;
             "Vendor :3:H='Ven', PO :H='PO#',nStyOrder:5:P = '99999':H='Q-Ord',Receive :5:P = '99999':H='Q-Rcd',"+;
             "Balance=(nStyOrder - Receive) :5:P = '99999':H='Blnce',Complete :H='Del-Date',ShipVia :1:H='M',"+;
             "Contact :9:H='Code', Available :H='Sold-dt',cFOB :H='Spl-Note'"

IF !EOF(TempPosln)
  lcShowP = "ENABLE"
  SUM nStyOrder, Receive TO lnTotOrd, lnTotRec
  lnBalance = lnTotOrd - lnTotRec
  GOTO TOP
  lcPONum = &TempPosln..PO
  IF !llCalled
    SHOW GET pbPNotes ENABLE
    SHOW GET pbShip   ENABLE
  ENDIF
ELSE  
  lcShowP = "DISABLE"
  IF !llCalled
    SHOW GET pbPNotes DISABLE
    SHOW GET pbShip   DISABLE
  ENDIF
ENDIF

SELECT(TempPosln)  
GOTO TOP
BROWSE FIELDS &lcBrFields; 
       LOCK 0            ;   
       SAVE              ;
       NOWAIT            ;
       NOMENU            ;
       NODELETE          ;
       NOEDIT            ;
       WHEN lfPONum() AND lfChMarker(TempPosln,lcBrowTtl);
       TITLE lcBrowTtl   ;
       WINDOW POKRA90B IN WINDOW POKRA900
=lfRefresh()

*!*************************************************************
*! Name      : lfChMarker
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : to show the marker for the recored in all browses
*!*************************************************************
*! Calls       : =lfChMarker()
*!*************************************************************
FUNCTION lfChMarker
PARAMETERS lcFile,lcWinName

lnMarker = RECNO(lcFile)
SHOW WINDOW (lcWinName) REFRESH SAME

*!*************************************************************
*! Name      : lfvStyNotes
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To display the notepad for the style
*!*************************************************************
FUNCTION lfvStyNotes

=NOTEPAD('F',lcStyle)

*!*************************************************************
*! Name      : lfPONum
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Get the choosen P/O number form the browse
*!*************************************************************
FUNCTION lfPONum

lcPONum = &TempPosln..PO

*!*************************************************************
*! Name      : lfvPONotes
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To display the notepad for the PO.
*!*************************************************************
FUNCTION lfvPONotes

IF !EMPTY(lcPONum)
  =NOTEPAD('P',lcPONum)
ENDIF  

*!*************************************************************
*! Name      : lfvShipScr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Calling the shipment screen.
*!*************************************************************
FUNCTION lfvShipScr

PUSH KEY
=lfTrapS()
DO gcScrDir+'PO\POKRA90E.SPX'
POP KEY

*!*************************************************************
*! Name      : lfShipSel
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : lfShipSel
*! Purpose   : Select the shipment for the selected P/O
*!*************************************************************
FUNCTION lfShipSel

CREATE CURSOR (TempShipm) ;
              (Serial_No C(6),Qty_Rcd N(7) ,Ctns_Rcd N(4) ,Ctns_Shp  N(4) ,;
               Lot       N(4),HWB     C(15),MWB      C(18),Buyer    C(20) ,;
               ETA_DTE   D   ,Rcd_Dte D    ,mDiscRep M    ,mFliegtbdt M   ,ChBillNum C(15))            

SELECT ItPoShip
IF SEEK(lcPONum)
  *SCAN WHILE Po = lcPONum FOR SUBSTR(Style,1,lnMajLen)+Color = lcStyle+lcColor
  SCAN WHILE Po = lcPONum FOR IIF(ItSetUp.cLinkType='ARIA26',;
                          SUBSTR(Style,1,lnMajLen)+Color = lcStyle+lcColor,;
                          Style=PADR(lcStyle,lnMajLen)+lcSepart+lcColor)
    =SEEK(CSHIPNUM+CHBILLNUM,'ITHBILLT')
    =SEEK(CSHIPNUM,'ITLCSHIP')
    =SEEK(PO,'PoSHdr')
    SELECT &TempShipm
    APPEND BLANK
    REPLACE Serial_No  WITH ItPoShip.cShipNum   ,;
            Qty_Rcd    WITH ItHBillt.nTotalRecv ,;
            Ctns_Rcd   WITH ItHBillt.nCarNumRcv ,;
            Ctns_Shp   WITH ItHBillt.nCartonNum ,;
            Lot        WITH ItLcShip.nCartonNum ,;
            HWB        WITH ItHBillt.cShipNum   ,;
            MWB        WITH ItLcShip.cMasterNum ,;
            Buyer      WITH PosHdr.Contact      ,; 
            ETA_Dte    WITH ItLcShip.dEtaDate   ,;
            Rcd_Dte    WITH IIF(ItLcShip.cStatus='C',ItHBillt.dDelvDate,{}) ,;
            mDiscRep   WITH ItHBillt.mDiscRep   ,;
            mFliegtbdt WITH ItLcShip.mFliegtbdt ,;
            ChBillNum  WITH ItPoShip.ChBillNum
  ENDSCAN
ENDIF
SELECT &TempShipm
GOTO TOP
IF EOF()
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No shipments for '+ALLTRIM(lcStyle)+'/' +ALLTRIM(lcColor)+'| '+lcPONum)
ELSE
  =lfvShipScr()
ENDIF  

*!*************************************************************
*! Name      : lfShipBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Browse the selected shipments
*!*************************************************************
FUNCTION lfShipBrow

lcBrFields = "cMarker =IIF(RECNO()=lnMarker,'',' '):H=' ':R:1:W=.F.:V=lfDispRem():F,"+;
             " Serial_No :H='Serial#':V=lfDispRem():F, Qty_Rcd :H='Qty-Rcd':V=lfDispRem():F,Ctns_Rcd :H='Ctns-Rcd':V=lfDispRem():F,"+;
             "Ctns_Shp:H='Ctns-Shp':V=lfDispRem():F,Lot:H='Lot#':V=lfDispRem():F, "+;
             "Hwb :H='H-AirwayBil':V=lfDispRem():F,Mwb :H='M-AirwayBil':V=lfDispRem():F,Buyer :H='Buyer':V=lfDispRem():F, "+;
             "ETA_Dte :H='ETA_Dte':V=lfDispRem():F,Rcd_Dte :H='Rcd-Date':V=lfDispRem():F"

SELECT(TempShipm)
GOTO TOP
BROWSE FIELDS &lcBrFields; 
       LOCK 0            ;   
       SAVE              ;
       NOWAIT            ;
       NOMENU            ;
       NODELETE          ;
       NOEDIT            ;
       WHEN lfChMarker(TempShipm,lcShipTtl) ;
       TITLE lcShipTtl   ;
       WINDOW POKRA90G IN WINDOW POKRA90E

*!*************************************************************
*! Name      : lfvClose
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To validate the close button.
*!*************************************************************
FUNCTION lfvClose

CLEAR READ

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To validate the select button.
*!*************************************************************
FUNCTION lfvSelect

lcStyle    = SPACE(lnMajLen)
lcColor    = SPACE(lnColorLen)
lcColDesc = SPACE(15)
lcDesc    = SPACE(20)
lnBalance = 0
lnTotOrd  = 0
lnTotRec  = 0
=lfActBrow()
SHOW GET lcStyle  ENABLE
SHOW GET lcColor  ENABLE
SHOW GET ibStyle  ENABLE
SHOW GET ibColor  ENABLE
SHOW GET pbSNotes DISABLE
SHOW GET pbPNotes DISABLE
SHOW GET pbShip   DISABLE  
SHOW GET pbSelect DISABLE  
_CUROBJ = OBJNUM(lcStyle)

*!*************************************************************
*! Name      : lfTrap
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the main screen.
*!*************************************************************
*! Calls       : =lfTrap()
*!*************************************************************
FUNCTION lfTrap

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC        DO lpEsc
ON KEY LABEL CTRL+ENTER DO lpEsc
ON KEY LABEL TAB        DO lpTab
ON KEY LABEL BACKTAB    DO lpSTab

*!*************************************************************
*! Name      : lpTab
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the Tab key in the main screen
*!*************************************************************
*! Calls       : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB lnD = 1

DO CASE
  CASE WONTOP(lcBrowTtl)
    ACTIVATE WINDOW POKra90D
  CASE WONTOP("POKra90D") AND _CUROBJ = OBJNUM(pbClose)
    IF EMPTY(lcStyle) OR EMPTY(lcColor)
      ACTIVATE WINDOW POKra90A
    ELSE
      ACTIVATE WINDOW (lcBrowTtl)
    ENDIF
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE    
 ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpSTab
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the Shift+Tab key in the main screen
*!*************************************************************
*! Calls       : DO lpSTab
*!*************************************************************
PROCEDURE lpSTab

ON KEY LABEL BACKTAB lnD = 1
DO CASE 
  CASE WONTOP(lcBrowTtl)
    IF EMPTY(lcStyle) OR EMPTY(lcColor)
      ACTIVATE WINDOW POKra90A
      IF EMPTY(lcColor)
        _CUROBJ = OBJNUM(lcColor)
      ELSE
        _CUROBJ = OBJNUM(lcStyle)
      ENDIF
    ELSE
      ACTIVATE WINDOW POKra90D
      _CUROBJ = OBJNUM(pbClose)
    ENDIF  
  CASE WONTOP("POKra90D") AND (_CUROBJ = OBJNUM(pbPNotes) OR _CUROBJ = OBJNUM(pbSelect))
    IF !EMPTY(lcStyle) And !EMPTY(lcColor)
      IF _CUROBJ = OBJNUM(pbPNotes)
        ACTIVATE WINDOW (lcBrowTtl)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF
    ELSE
      _CUROBJ = _CUROBJ - 1
    ENDIF
  CASE WONTOP("POKra90D") AND _CUROBJ = OBJNUM(pbSNotes)
    IF EOF(TempPosln)
      ACTIVATE WINDOW (lcBrowTtl)
    ELSE
      _CUROBJ = _CUROBJ - 1
    ENDIF
  CASE WONTOP("POKra90A") AND _CUROBJ = OBJNUM(lcStyle)
    ACTIVATE WINDOW POKra90D
    _CUROBJ = OBJNUM(pbClose)
  CASE WONTOP("POKra90D") AND _CUROBJ = OBJNUM(pbClose)
    IF EMPTY(lcStyle) OR EMPTY(lcColor)
      ACTIVATE WINDOW POKra90A
      IF EMPTY(lcColor)
        _CUROBJ = OBJNUM(lcColor)
      ELSE
        _CUROBJ = OBJNUM(lcStyle)
      ENDIF  
    ELSE
      _CUROBJ = _CUROBJ - 1
    ENDIF
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE    
ON KEY LABEL BACKTAB DO lpSTab

*!*************************************************************
*! Name      : lpEsc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the Esc key in the main screen
*!*************************************************************
*! Calls       : DO lpEsc
*!*************************************************************
PROCEDURE lpEsc

ACTIVATE WINDOW POKra90D
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfTrapS
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for in Shipment screen.
*!*************************************************************
*! Calls       : =lfTrapS()
*!*************************************************************
FUNCTION lfTrapS

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC        DO lpEscS
ON KEY LABEL CTRL+ENTER DO lpEscS
ON KEY LABEL TAB        DO lpTabS
ON KEY LABEL BACKTAB    DO lpSTabS

*!*************************************************************
*! Name      : lpTabS
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the Tab key the Shipment screen
*!*************************************************************
*! Calls       : DO lpTabS
*!*************************************************************
PROCEDURE lpTabS

ON KEY LABEL TAB lnD = 1
DO CASE
  CASE WONTOP(lcShipTtl)
    ACTIVATE WINDOW POKra90H
  CASE WONTOP("POKra90H") AND _CUROBJ = OBJNUM(pbClose)
    ACTIVATE WINDOW (lcShipTtl)
  OTHERWISE
    _CUROBJ =  _CUROBJ + 1
ENDCASE    
ON KEY LABEL TAB DO lpTabS

*!*************************************************************
*! Name      : lpSTabS
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the Shift+Tab key in Shipment screen
*!*************************************************************
*! Calls       : DO lpSTabS
*!*************************************************************
PROCEDURE lpSTabS

ON KEY LABEL BACKTAB lnD = 1
DO CASE 
  CASE WONTOP(lcShipTtl)
    ACTIVATE WINDOW POKra90H
    _CUROBJ = OBJNUM(pbClose)
  CASE WONTOP("POKra90H") AND _CUROBJ=OBJNUM(pbRemark)
    ACTIVATE WINDOW (lcShipTtl)
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE    
ON KEY LABEL BACKTAB DO lpSTabS

*!*************************************************************
*! Name      : lpEscS
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Trapping for the Esc key in Shipment screen
*!*************************************************************
*! Calls       : DO lpEscS
*!*************************************************************
PROCEDURE lpEscS

IF WONTOP("POKra90H") OR WONTOP(lcShipTtl)
  ACTIVATE WINDOW POKra90H
ELSE  
  ACTIVATE WINDOW POKra90N
ENDIF  
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvRemarks
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Prin the remarks for selected shipment
*!*************************************************************
*! Calls       : =lfvRemarks()
*!*************************************************************
FUNCTION lfvRemarks
llDNote = .T.
lcNotes = mDiscRep
lcNTitle = 'Discrepency Notes'
DO gcScrDir+'PO\POKRA90N.SPX'
*DO (QSD+'KRA900N.SPX')  
lcNotes = ''

*!*************************************************************
*! Name      : lfvFBDet
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Print the Flight/Boat details for selected shipment.
*!*************************************************************
*! Calls       : =lfvFBDet()
*!*************************************************************
FUNCTION lfvFBDet

llDNote = .F.
lcNotes = mFliegtbdt
lcNTitle = 'Flight/Boat details'
*DO (QSD+'KRA900N.SPX')  
DO gcScrDir+'PO\POKRA90N.SPX'
lcNotes = ''

*!*************************************************************
*! Name      : lfVNClose
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Valid function for close button for notes screen.
*!*************************************************************
*! Calls       : =lfVNClose()
*!*************************************************************
FUNCTION lfVNClose
IF llDNote 
  IF SEEK(Serial_No+ChBillNum,'ItHBillT')
    REPLACE ItHBillT.mDiscRep WITH lcNotes 
  ENDIF
ELSE
  IF SEEK(Serial_No,'ItLcShip')
    REPLACE ItLcShip.mFliegtbdt WITH lcNotes 
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfDispRem
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : Display the remark for the selected shipment.
*!*************************************************************
*! Calls       : =lfDispRem()
*!*************************************************************
FUNCTION lfDispRem

IF LASTKEY() = 13 AND !MDOWN()
  =lfvRemarks()
ENDIF  

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To get the length to of the color.
*!*************************************************************
*! Calls       : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
lcNonMajPi = ''
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
 *-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen


*!*************************************************************
*! Name      : lfCrtSetup
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To link the company.
*!*************************************************************
*! Calls       : =lfCrtSetup()
*!*************************************************************
FUNCTION lfCrtSetup


IF !FILE(gcDataDir+'KRASETUP.MEM')
  DIMENSION laComp[1,2]
  STORE '' TO laComp , lcMcompdir , lcDir
  DO (gcSCrDir+"KRASETUP.SPX")  && Run The Linked Directry and Selected Company.
ENDIF

IF FILE(gcDataDir+'KRASETUP.MEM')
  RESTORE FROM (gcDataDir+'KRASETUP.MEM') ADDITIVE
  RETURN(.T.)
ELSE
  CLEAR READ
  RETURN(.F.)
ENDIF  

*!*************************************************************
*! Name      : lfVDir
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To validate the selected directory.
*!*************************************************************
*! Calls       : =lfVDir()
*!*************************************************************
FUNCTION lfVDir

lnCount = 0
lcDir = GETDIR()
lcDir = SUBSTR(lcDir,1,AT('\',lcDir,2))
SHOW GET lcDir
IF EMPTY(lcDir)
  RETURN
ENDIF
IF FILE(lcDir+'SYSFILES\SYCCOMP.DBF')
  USE IN (IIF(USED(lcAliasN),lcAliasN,0))
  *=gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',lcAliasN)
  =gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',@lcAliasN,.T.)
  SELECT (lcAliasN)
  COUNT TO lcCompNo FOR !EMPTY(CCOMP_id)
  DIMENSION laComp[lcCompNo,2]
  laComp = ''
  SCAN
    lnCount = lnCount + 1
    laComp[lnCount,1] = cComp_Id + '  ' + ALLTRIM(cCom_Name)
    laComp[lnCount,2] = ccom_ddir
  ENDSCAN  
  SHOW GET PbComp
ENDIF
IF lnCount > 0
  SHOW GET PbComp ENABLE
  SHOW GET PbOk   ENABLE
ENDIF
*-- END OF lfVDir.

*!*************************************************************
*! Name      : lfVAccpOk
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/17/1998
*! Purpose   : To validate OK button.
*!*************************************************************
*! Calls       : =lfVAccpOk()
*!*************************************************************
FUNCTION lfVAccpOk

IF !EMPTY(lcDir)
  lcMcompdir =  laComp[PbComp,2]
  lcMcompdir = ALLTRIM(lcMcompdir)
  IF FILE(lcMcompdir+'ITLCSHIP.DBF') AND FILE(lcMcompdir+'ITHBILLT.DBF') AND;
     FILE(lcMcompdir+'ITPOSHIP.DBF')    
     SAVE ALL LIKE lcMcompdir TO (gcDataDir+'KRASETUP.MEM')
     CLEAR READ
  ELSE
    *-- TEXT : Company files can not be Found
    =gfModalGen('TRM00113B00000','DIALOG','found.') 
    SET DEVICE TO SCREEN
    laComp = ''
    SHOW GET PbComp DISABLE
    SHOW GET PbOk   DISABLE
    RETURN
  ENDIF
ENDIF