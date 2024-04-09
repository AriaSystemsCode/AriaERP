*!*************************************************************
*! Program File : MACMINV.PRG       (*C102762,1 Custom for DRE07)
*! Program Desc : Program to Add, Inquire and Modify Commercial Invoice
*! For Screen   : MACMINV.SPR
*!       System : Aria Apparel Series(A27) 
*!       Module : MATERIAL Module 
*! Developer    : AHMED MAHER (AMH)
*!*************************************************************
*! Calls        :
*!        Procedures :lpShow,lpOptions,lpDelScr
*!        
*!        Functions  : lfActFolder(),lfFillFold(),lfvData1()
*!                     lfvIData1()  ,lfInit()    ,lfPOLines()
*!                     lfBrowLine() ,lfwLine()   ,lfvData2()
*!                     lfvIData2()  ,lfvData3()  ,lfvIData3()
*!                     lfwData22()  ,lfwData2()  ,lfvData9()
*!                     lfwData3()
*!                     lfvNewPL()   ,lfvFabric() ,lfvColor()
*!                     lfvKFabric() ,lfvKColor()
*!*************************************************************
*! Example :
*!        DO MACMINV
*!*************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[26],laKeyField[1,4],laFoldWinds[3,2],laAddress[6,3],laDefProc[10],;
        laVariables[1] 

STORE '' TO lcFolder ,laData,laKeyField,laFoldWinds,laAddress,lcPOHdr
STORE '' TO ldOldEnt, ldOldComp,lcDetTmp,;
            lcFabric,lcOldFab,lcColor,lcOldClr,lcIDesc,lcClrDesc,;
            lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinC31,lcWinC32,lcWinC33,lcDelMesag,;
            lcPOLine,lcPOTran,lcWinC34,laVariables,cMarker,lcContLin,lcPrgtype,;
            lcMenProm,lcUOB

llBrowse = .F.

DECLARE laFromBrow[2]
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcDet_Ttl)

lcWinC30 = ''
lcWinC35 = ''

STORE 0 TO  lnPrice,lnOldPrice,lnAmount,lnOldAmnt,lnUnitQty,lnOldUntQt,lnQty,lnOldQty,lnTotQty,;
            lnLineNo,lnLastLine,lnNewRec,lnLastFold,lnWeight
STORE 1 TO  lnShip,lnItemType
STORE '' TO lcUOM,lcVEnAdd1,lcVEnAdd2,lcVEnAdd3,lcVEnAdd4,lcVEnAdd5,;
            lcWareAdd1,lcWareAdd2,lcWareAdd3,lcWareAdd4,lcWareAdd5,;
            lcOldWare,lcWareDEsc,lcActTmp,lcFiles,lcOldVend
STORE .F. TO llContCls
STORE .F. TO llOpnVend
STORE .F. TO llFromRef

laData[4] = gdSysDate
laData[5] = gdSysDate

llContinue  = .F.
lnSessNo    = gnProgCopy
lnNomOfFo   = 3
llAlowNew   = .T.   && Flag to enable the new button in the control pannel.

ibData11 = 1
ibData12 = 1
ibData13 = 1

lcOldEscTrap  = ON('Key','Esc')
laDefProc[7]  = .F.
laDefProc[9]  = .F.
laDefProc[10] = .F.

lNewLine = .F.
lcScrMode = 'S' 

DIMENSION laShip[1], laItemType[1], laCodInfo [2,10]
STORE "" TO laShip,laItemType,laCodInfo

IF !gfSetup()
  RETURN
ENDIF  

DIMENSION laSetUp[1,2]
laSetUp[1,1] = 'M_WareHouse'
= gfGetMemVar(@laSetUp)
llMultiWH  = ALLTRIM(laSetUp[1,2])= 'Y'
lcSession  = gfsequence('CSESSION')

*-- Checking the existence of at least one warehous (Default) in warehous file
PRIVATE lnWareRec,llWareFond
lnWareRec  = 0
llWareFond = .F.
SELECT WAREHOUS
lnWareRec = RECNO()
LOCATE
IF llMultiWH
  SCAN FOR lMatInv
    llWareFond = .T.
    EXIT
  ENDSCAN
ELSE
  IF EOF()
    llWareFond = .F.
  ELSE 
    llWareFond = .T.
  ENDIF
ENDIF
IF RECCOUNT() >= lnWareRec
  GO lnWareRec
ENDIF

IF !llWareFond
  *-- Locations file is Empty . Cannot proceed.
  *-- <Ok>
  *--Locations file is Empty, Cannot Proceed.
  =gfModalGen('QRM42080B00000','DIALOG','Locations file is Empty')
  glQuitting = .T.
  RETURN
ENDIF  

lcPrgtype  = 'Invoice #'
lcDialog   = 'Invoice'
lcFile_ttl = 'Commercial Invoice'

lcUnsmsPrg   = PADR('MAT_PO',10)
lcwfoldchng  = '=lfActFolder()'   && function to control shows after change the folder
lcfoldpush   = 'pbFolders'        && push button name for the next folder
lnfolderCend = 103.00
lnfolderRend = 2.00

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'COMINVHD'
laKeyField[1,4] = 1

DIMENSION ladata[26]
*----------------------------------------------------------------
lcScFields   = 'INVOICE, CWARECODE, VENDOR, ENTERED, COMPLETE, CCONTAINER,'+;
               'CSEAL, CBOOKING, SHIPVIA,'+;
               'CTOTWEIGHT, NCARTON, CCONTOUT1, CCONTOUT2, CCONTOUT3,'+;
               'CCONTOUT4, CCONTOUT5, NUNITQTY,'+;
               'NQUANTITY1, NQUANTITY2, NQUANTITY3, NQUANTITY4, NQUANTITY5,'+;
               'NFABORDER, POTOTAL, NCARTONS, LASTLINE'
*----------------------------------------------------------------

lcDet_Ttl = "Invoice Lines"
laFromBrow[1]=lcDet_Ttl 
lcRec_Ttl = "Material PO lines"
laFromBrow[2]=lcRec_Ttl

lcMark    = '>'
lcUnMark   = SPACE(1)
lcfoldprnt = gcBaseWind         && window parent name for the folder
llNoShow = .F.
= lfFillFold()
qWareHouse = WAREHOUS.CWARECODE

*----------------------------------------------------------------
IF !WEXIST(gcBaseWind)
  ldOldEnt     = GDSYSDATE
  ldOldComp    = GDSYSDATE
  lcPOHdr      = gfTempName()
  lcWinCh1     = gfTempName()
  lcWinCh2     = gfTempName()
  lcWinCh3     = gfTempName()
  lcWinC31     = gfTempName()
  lcWinC32     = gfTempName()
  lcWinC33     = gfTempName()
  lcWinC34     = gfTempName()
  lcWinC30     = gfTempName()
  lcWinC35     = gfTempName()
  lcWinCh4     = gfTempName()
  lcfolder     = gfTempName()
  llNoShow = .F.
  laVariables[1]  = 'lcScrMode'

  laCodInfo[1,01] = "SHIPVIA"      && Field Name
  laCodInfo[1,02] = "laShip"       && Array Name
  laCodInfo[1,03] = "lnShip"       && Popup Name
  laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = "COMINVHD"     && Alternative File (For default val.)
  laCodInfo[1,08] = "COMINVHD"     && Use this index for the Alternative file.
  laCodInfo[1,09] = "laData[1]"    && Seek this expretion.
  laCodInfo[1,10] = "SHIPVIA"      && Alternative Field Name

  laCodInfo[2,01] = "ITEM_TYPE"    && Field Name
  laCodInfo[2,02] = "laItemType"   && Array Name
  laCodInfo[2,03] = "lnItemType"   && Popup Name
  laCodInfo[2,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[2,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[2,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[2,07] = "FABRIC"       && Alternative File (For default val.)
  laCodInfo[2,08] = "FABRIC"       && Use this index for the Alternative file.
  laCodInfo[2,09] = "lcFabric"     && Seek this expretion.
  laCodInfo[2,10] = "ITEM_TYPE"    && Alternative Field Name

  = gfwCodePop(@laCodInfo, "SHIPVIA" , "N")
  = gfwCodePop(@laCodInfo, "ITEM_TYPE" , "N")
  
  SELECT COMINVHD
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laData[4] = gdSysDate
  laData[5] = gdSysDate
  DIMENSION laFoldWinds[lnNomOfFo,2]
  lcDetTmp  = gfTempName()
  lcPOLine  = gfTempName()
  lcPOTran  = gfTempName()
  lcActTmp  = gfTempName()
*----------------------------------------------------------------
  lnactfolder = 1
  lnlastfold  = lnactfolder
*----------------------------------------------------------------
  IF !USED('ApVendor')
    =gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
    llOpnVend = .T.
  ELSE
    SELECT ApVendor
    lnOldTag = INT(VAL(SYS(21)))
    SET ORDER TO TAG VenCode
    lnRecVend = RECNO("ApVendor")
  ENDIF
  lcFiles = 'lcPOHdr,'+lcPOHdr+','+lcPOHdr+';'+'lcDetTmp,'+lcDetTmp+','+lcDetTmp+';'
ENDIF
*----------------------------------------------------------------

SELECT FABRIC
SET ORDER TO TAG FABRIC

SELECT (lcBaseFile)
PUSH KEY
=lfActPad()
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+B
ON SELECTION PAD _BROWSE OF _MSYSMENU ACTIVATE WINDOW(lcDet_Ttl)
DEFINE PAD _BROWSE1 OF _MSYSMENU PROMPT "" KEY CTRL+R
ON SELECTION PAD _BROWSE1 OF _MSYSMENU DO lpActRec

DO (gcScrDir+gcWinAppl+'\MACMINV.SPX')

POP KEY
RELEASE PAD _Option OF _MSYSMENU
RELEASE PAD _WipOptn OF _MSYSMENU
RELEASE WINDOWS (lcDet_Ttl)
RELEASE PAD _BROWSE  OF _MSYSMENU
RELEASE PAD _BROWSE1 OF _MSYSMENU

*!*************************************************************
*! Name        : lfActFolder
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/17/2002
*!*************************************************************
*! Purpose     : Called when the activ folder is changed
*!               to disable the others folders and show the
*!               gets of the active folder with cases
*!*************************************************************
*! Calls       :
*!*************************************************************
FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC35) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC33) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    SHOW WINDOW (lcWinC32) SAME
    SHOW WINDOW (lcWinCh2) TOP
    IF laScrMode[2] OR laScrMode[1]
      SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinCh2) ENABLE  ONLY
    ENDIF  
    
  CASE lnActFolder = 2
    SHOW WINDOW (lcWinCh3) TOP
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC31) ENABLE  ONLY
    IF laScrMode[2] OR laScrMode[1]
      SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC35) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC33) DISABLE ONLY
      SHOW GET ibFromRec ENABLE
    ELSE
      SHOW GETS WINDOW (lcWinC31) ENABLE  ONLY
      SHOW GETS WINDOW (lcWinC32) ENABLE  ONLY
      SHOW GETS WINDOW (lcWinC33) ENABLE  ONLY
      GOTO TOP IN (lcDetTmp)
      IF EOF(lcDetTmp)
        SHOW GET pbRemove DISABLE
      ELSE
        SHOW GET pbRemove ENABLE
      ENDIF
    ENDIF
    IF laScrMode[3] OR laScrMode[4]
      _CUROBJ = OBJNUM(pbNew)
    ENDIF
    =lfBrowLine()
    =lfwLine()
    SHOW GET ibFromBrow ENABLE
    SHOW GETS WINDOW (lcWinC30) ENABLE ONLY

  CASE lnActFolder = 3
    SHOW WINDOW (lcWinCh4) TOP
    SHOW WINDOW (lcWinC32) SAME
    SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC33) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinC30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC35) DISABLE ONLY
    =lfRefresh()
ENDCASE

*!*************************************************************
*! Name        : lfFillFold
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/17/2002
*!*************************************************************
*! Purpose     : to initialize the folders name and windows
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfFillFold

laFoldWinds[1,1] = "Header"
laFoldWinds[1,2] = "lcWinCh2"

laFoldWinds[2,1] = "Detail"
laFoldWinds[2,2] = "lcWinCh3"

laFoldWinds[3,1] = "Summary"
laFoldWinds[3,2] = "lcWinCh4"

*!*************************************************************
*! Name        : lpShow
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/17/2002
*!*************************************************************
*! Purpose     : show gets enable and disable due to the screen mode
*!*************************************************************
*! Calls       : 
*!*************************************************************
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    IF !USED(lcPoHdr)
      SELECT COMINVHD
      = AFields(laFileStru)
      lnNewFld = ALEN(laFileStru,1)+1
      DIMENSION laFileStru[lnNewFld,4]
      laFileStru[lnNewFld,1] = 'nSteps'
      laFileStru[lnNewFld,2] = 'N'
      laFileStru[lnNewFld,3] = 2
      laFileStru[lnNewFld,4] = 0
      =gfCrtTmp(lcPOHdr,@laFileStru,[INVOICE],lcPOHdr)
      SELECT (lcPOHdr)
      APPEND BLANK
    ENDIF
    
    =lfInit()
    =lfBrowLine()

    = gfwCodePop(@laCodInfo, "SHIPVIA","N")
    = gfwCodePop(@laCodInfo, "ITEM_TYPE","N")
    SHOW GET ibFolder[3] DISABLE  
    SHOW GET ibFolder[2] ENABLE      
    SELECT(lcBaseFile)
    lnlastfold  = lnactfolder
    lnactfolder = 1
    =lfChngFolder(lnActFolder)
    lcVEnAdd1   = SPACE(30)
    lcVEnAdd2   = SPACE(30)
    lcVEnAdd3   = SPACE(30)
    lcVEnAdd4   = SPACE(30)
    lcVEnAdd5   = SPACE(30)
    lcWareAdd1  = SPACE(30)
    lcWareAdd2  = SPACE(30)
    lcWareAdd3  = SPACE(30)
    lcWareAdd4  = SPACE(30)
    lcWareAdd5  = SPACE(30)
    lcFabric    = SPACE(07) 
    lcColor     = SPACE(06) 
    lcRef       = SPACE(10) 
    lcPO        = SPACE(06)
    lcClrDesc   = SPACE(15)
    lcIDesc     = SPACE(20)
    lnQty       = 0
    lnPrice     = 0
    lnAmount    = 0
    lcUOM       = SPACE(3)
    lnWeight    = 0
    lnUnitQty   = 0
    lnTotQty    = 0
    lnLineNo    = 0
    laData[2]   = IIF(llMultiWH,SPACE(06),qWareHouse)
    laData[4]   = GDSYSDATE
    laData[5]   = GDSYSDATE
    SHOW GET pbInvNote DISABLE
    SHOW GET pbNew     DISABLE
    SHOW GET lcFabric  DISABLE
    SHOW GET lcColor   DISABLE
    SHOW GET lcIDesc   DISABLE
    SHOW GET lcClrDesc DISABLE
    SHOW GET lcRef     DISABLE
    SHOW GET lcPO      DISABLE
    SHOW GET lnQty     DISABLE
    SHOW GET lnPrice   DISABLE
    SHOW GET lnAmount  DISABLE
    SHOW GET lcUOM     DISABLE
    SHOW GET lnWeight  DISABLE
    SHOW GET lnUnitQty DISABLE
    
  CASE laScrMode[4]
    =lfInit()
    lcScrMode = 'A'
    llcUpdate = .T.
    IF llContinue
      SELECT (lcDetTmp)
      =lfBrowLine()    
      laCodInfo[1,07] = lcPOhdr 
      laCodInfo[1,08] = lcPOhdr
      laCodInfo[1,09] = "laData[1]"
      =gfwCodePop(@laCodInfo, "SHIPVIA","T")
      laData[9] = laShip[lnShip,2]
    ELSE
      SELECT (lcDetTmp)
      DELETE ALL FOR !EMPTY(POMAT)
      LOCATE
      SELECT(lcBaseFile)
      laData[3]  = IIF(!llMultiWH,qWareHouse,laData[3])
      laData[9]  = IIF(EMPTY(laData[9]),SPACE(06),laData[9])
      lcVEnAdd1  = IIF(EMPTY(lcVEnAdd1),SPACE(30),lcVEnAdd1)
      lcVEnAdd2  = IIF(EMPTY(lcVEnAdd2),SPACE(30),lcVEnAdd2)
      lcVEnAdd3  = IIF(EMPTY(lcVEnAdd3),SPACE(30),lcVEnAdd3)
      lcVEnAdd4  = IIF(EMPTY(lcVEnAdd4),SPACE(30),lcVEnAdd4)
      lcVEnAdd5  = IIF(EMPTY(lcVEnAdd5),SPACE(30),lcVEnAdd5)
      lcWareAdd1 = IIF(EMPTY(lcWareAdd1),SPACE(30),lcWareAdd1)
      lcWareAdd2 = IIF(EMPTY(lcWareAdd2),SPACE(30),lcWareAdd2)
      lcWareAdd3 = IIF(EMPTY(lcWareAdd3),SPACE(30),lcWareAdd3)
      lcWareAdd4 = IIF(EMPTY(lcWareAdd4),SPACE(30),lcWareAdd4)
      lcWareAdd5 = IIF(EMPTY(lcWareAdd5),SPACE(30),lcWareAdd5)
      IF EMPTY(laData[9])
        =gfwCodePop(@laCodInfo, "SHIPVIA","D")
        laData[9] = laShip[lnShip,2]
      ENDIF
      laData[4]  = IIF(EMPTY(laData[4]),gdSysDate,laData[4])
      laData[5]  = IIF(EMPTY(laData[5]),gdSysDate,laData[5])
      lcVEnAdd1  = IIF(EMPTY(laData[3]),SPACE(30),lcVEnAdd1)
      lcVEnAdd2  = IIF(EMPTY(laData[3]),SPACE(30),lcVEnAdd2)
      lcVEnAdd3  = IIF(EMPTY(laData[3]),SPACE(30),lcVEnAdd3)
      lcVEnAdd4  = IIF(EMPTY(laData[3]),SPACE(30),lcVEnAdd4)
      lcVEnAdd5  = IIF(EMPTY(laData[3]),SPACE(30),lcVEnAdd5)
      lcWareAdd1 = IIF(EMPTY(laData[2]),SPACE(30),lcWareAdd1)
      lcWareAdd2 = IIF(EMPTY(laData[2]),SPACE(30),lcWareAdd2)
      lcWareAdd3 = IIF(EMPTY(laData[2]),SPACE(30),lcWareAdd3)
      lcWareAdd4 = IIF(EMPTY(laData[2]),SPACE(30),lcWareAdd4)
      lcWareAdd5 = IIF(EMPTY(laData[2]),SPACE(30),lcWareAdd5)
      lcFabric  = SPACE(07) 
      lcColor   = SPACE(06) 
      lcRef     = SPACE(10) 
      lcPO      = SPACE(06)
      lcClrDesc = SPACE(15)
      lcIDesc   = SPACE(20)
      lnQty       = 0
      lnPrice     = 0
      lnAmount    = 0
      lcUOM       = SPACE(3)
      lnWeight    = 0
      lnUnitQty   = 0
      lnTotQty    = 0
    ENDIF
    SHOW GET laData[3]   ENABLE
    SHOW GET ibData3     ENABLE    
    SHOW GET ibFromBrow  ENABLE
    _CUROBJ = OBJNUM(laData[2])
    SHOW GET ibFolder[3] ENABLE
    
    IF llMultiWH
      SHOW GET laData[2] ENABLE
      SHOW GET ibData2   ENABLE   
    ELSE
      SHOW GET laData[2] DISABLE
      SHOW GET ibData2   DISABLE
    ENDIF  
    GOTO TOP IN (lcDetTmp) 
    IF !EOF(lcDetTmp)
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF     
    SHOW GET pbInvNote DISABLE
    =lfBrowLine()
    lnLastFold  = lnActFolder
    lnActFolder = 1
    =lfChngFolder(lnActFolder)
    
  CASE laScrMode[2]
    lcScrMode = 'V'
    =lfInit()
    IF lnActFolder = 2
      =lfBrowLine()
      =lfwLine()
    ENDIF  
    SELECT (lcPOHdr)
    GATHER FROM laData FIELDS &lcScFields MEMO    
    SELECT(lcBaseFile)
    
    *-- Get the Ship Via Code.
    =gfwCodePop(@laCodInfo,'SHIPVIA','L')
    lnShip= ASCAN('laShip',COMINVHD.SHIPVIA)
    lnShip= IIF(lnShip= 0,1,ASUBSCRIPT(laShip,lnShip,1))
    lnTotQty = laData[18]+laData[19]+laData[20]+laData[21]+laData[22]
    SHOW GET ibFolder[3]  ENABLE    
    SHOW GET ibFolder[2]  ENABLE        
    SHOW GET pbInvNote ENABLE
    
  CASE laScrMode[3]
    llCUpdate  = .T.
    lcScrMode = 'E'
    GOTO TOP IN (lcDetTmp) 
    IF !EOF(lcDetTmp)
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF
    IF llMultiWH
      SHOW GET laData[2] ENABLE
      SHOW GET ibData2   ENABLE
    ELSE
      SHOW GET laData[2] DISABLE
      SHOW GET ibData2   DISABLE
    ENDIF
    SHOW GET pbInvNote ENABLE    
    =lfchngfolder(lnactfolder)
ENDCASE
SHOW GET pbUsrFields DISABLE
=lfActFolder()

*!*************************************************************
*! Name        : lfvData1
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/17/2002
*!*************************************************************
*! Purpose     : Validate the Invoice field and if 
*!               found swithch the screen mode to view mode
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvData1

lnAlias = SELECT()
llMdown = MDOWN()
IF llMdown
  RETURN
ENDIF  
SELECT COMINVHD
IF !EMPTY(laData[1]) AND !laScrMode[4]
  IF !SEEK(laData[1],"COMINVHD")
    DECLARE laBrow[1]
    laBrow=' '
    
    lcBrFields = "Invoice   :R :H='Invoice'   :08,"+;
                 "cWareCode :R :H='WareHouse' :10,"+;
                 "Vendor    :R :H='Vendor'    :10,"+;
                 "Entered   :R :H='Entered'   :08,"+;
                 "Complete  :R :H='Complete'  :08,"+;
                 "NFABORDER :R :H='Tot.Qty.'  :07,"+;	         
                 "POTotal   :R :H='Amount'    :10,"+;
                 "nCartons  :R :H='Carton'    :07"
    llRetValue = gfBrows('',"Invoice","laBrow",'Commercial Invoice')
    IF llRetValue
      laData[1] = laBrow[1]
    ELSE
      laData[1] = SPACE(6)
    ENDIF
  ENDIF
  
  IF !EMPTY(laData[1])
    laScrMode[1] = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ENDIF

*!*************************************************************
*! Name        : lfvIData1
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/17/2002
*!*************************************************************
*! Purpose     : Validate the Invoice field from the key field 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvIData1
laData[1]     = PADR("?",6)
llNothing = lfvData1()

*!*************************************************************
*! Name        : lfInit
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : Initialize the ladata array from the COMINVHD file 
*!               if the screen mode is view mode
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfInit

lnAlias = SELECT(0)
=SEEK(laData[1],"COMINVHD")
laData[3] = COMINVHD.Vendor
=SEEK(laData[3],"APVENDOR")
lcVEnAdd1 =gfGetAdr('ApVendor','','','',1,'')
FOR lnCounter = 2 TO 5
  L=ALLTRIM(STR(lnCounter))
  lcTempVr=''
  FOR lnCount = 1 TO ALEN(laAddress,1)
    IF laAddress[lnCount,1] = lnCounter
      lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
      lcTempVr  = lcTempVr+IIF(EMPTY(lcTempVr).OR.RIGHT(lcTempVr,1)=',' ,'',', ')+lcAddPart
    ENDIF
  ENDFOR
  lcVEnAdd&L = lcTempVr
ENDFOR
laData[2] = COMINVHD.cWareCode
=SEEK(laData[2],"WAREHOUS")
lcWareAdd1 =gfGetAdr('WareHous','','','',1,'')
FOR lnCounter = 2 TO 5
  L=ALLTRIM(STR(lnCounter))
  lcTempVr=''
  FOR lnCount = 1 TO ALEN(laAddress,1)
    IF laAddress[lnCount,1] = lnCounter
      lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
      lcTempVr  = lcTempVr+IIF(EMPTY(lcTempVr).OR.RIGHT(lcTempVr,1)=',' ,'',', ')+lcAddPart
    ENDIF
  ENDFOR
  lcWareAdd&L = lcTempVr
ENDFOR
laData[4]   = COMINVHD.ENTERED
laData[5]   = COMINVHD.COMPLETE
laData[6]   = COMINVHD.CCONTAINER
laData[7]   = COMINVHD.CSEAL
laData[8]   = COMINVHD.CBOOKING
laData[9]   = COMINVHD.SHIPVIA
laData[10]  = COMINVHD.CTOTWEIGHT
laData[11]  = COMINVHD.NCARTON
laData[12]  = COMINVHD.CCONTOUT1
laData[13]  = COMINVHD.CCONTOUT2
laData[14]  = COMINVHD.CCONTOUT3
laData[15]  = COMINVHD.CCONTOUT4
laData[16]  = COMINVHD.CCONTOUT5
laData[17]  = COMINVHD.NUNITQTY
laData[18]  = COMINVHD.NQUANTITY1
laData[19]  = COMINVHD.NQUANTITY2
laData[20]  = COMINVHD.NQUANTITY3
laData[21]  = COMINVHD.NQUANTITY4
laData[22]  = COMINVHD.NQUANTITY5
laData[23]  = COMINVHD.NFABORDER
laData[24]  = COMINVHD.POTOTAL
laData[25]  = COMINVHD.NCARTONS
laData[26]  = COMINVHD.LASTLINE
=lfPOLines()
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfPOLines
*! Developer   : AHMED MAHER
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get all lines from the COMINVLN for the selected Invoice
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfPOLines

IF USED(lcDetTmp) .AND. !EOF()
  SELECT (lcDetTmp)
  DELETE ALL
ELSE
 SELECT COMINVLN
  = AFields(laFileStru)
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'lNew'
  laFileStru[lnNewFld,2] = 'L'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
 
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'nRecno'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 10
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cStatus'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  CREATE table (gcWorkDir+ lcDetTmp) FROM ARRAY laFileStru
  INDEX ON INVOICE+CMATTYPE+POMAT+STR(LINENO,6,0)+FABRIC+COLOR TAG (lcDetTmp) OF (gcWorkDir+lcDetTmp)
ENDIF  

SELECT COMINVLN
IF SEEK(laData[1])
  SCAN REST WHILE INVOICE+CMATTYPE+POMAT+STR(LINENO,6,0)+FABRIC+COLOR = laData[1]
    SCATTER MEMVAR MEMO
    m.cMarker = ' '
    m.lNew    = .F.
    m.cStatus = 'S'
    INSERT INTO (lcDetTmp) FROM MEMVAR 
  ENDSCAN
ENDIF

*!*************************************************************
*! Name        : lfBrowLine
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : browse lines for the selected Invoice
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfBrowLine

lnAlias = SELECT(0)
SELECT (lcDetTmp)
LOCATE
lcBrowFlds = [cMarker    :R :H='',]+;
             [cReference :R :H='Ref. #'      :10,]+;
             [PoMat      :R :H='PO #'        :08,]+;
             [Fabric     :R :H='Item'        :10,]+;
             [Color      :R :H='Color'       :10,]+;
             [cItmDesc   :R :H='Item Desc.'  :20,]+;
             [cClrDesc   :R :H='Color Desc.' :20,]+;
             [nFabTotQty :R :H='Quantity'    :P='9999999.999',]+;
             [Price      :R :H='Price'       :P='999999999.99',]+;
             [lnAmount = NFABTOTQTY*PRICE :R :H='Amount' :P='999999999999999.999',]+;
             [lcItemType = gfCodDes(ITEM_TYPE, 'ITEM_TYPE') :R :H='Item Type' :20,]+;
             [UOM        :R :H='UOM'         :05,]+;
             [NFABWEIGHT :R :H='Weight'      :P='99999.999',]+;
             [nUnitQty   :R :H='Units'       :P='99999']
BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwLine() ;       
       VALID :F lfvBrows() ;
       TITLE lcDet_Ttl    ;
       WINDOW (lcWinC31) IN WINDOW (lcWinCh3)
=lfRefresh('lcWinC33')
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfwLine
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : refresh the fields when changing the record in the browse
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwLine
glFromBrow = .T.
lnAlias = SELECT(0)
SELECT (lcDetTmp)
IF !EOF() AND !BOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
  lcRef       = CREFERENCE
  lcPoMat     = POMAT
  lcFabric    = FABRIC
  lcOldFab    = FABRIC
  lcColor     = COLOR
  lcOldClr    = COLOR
  lcIDesc     = CITMDESC
  lcClrDesc   = CCLRDESC
  lnQty       = NFABTOTQTY
  lnPrice     = PRICE
  lnAmount    = NFABTOTQTY*PRICE
  =gfwCodePop(@laCodInfo, "ITEM_TYPE", "L")
  lnItemType  = ASCAN('laItemType',ITEM_TYPE)
  lnItemType  = IIF(lnItemType = 0,1,ASUBSCRIPT(laItemType,lnItemType,1))
  lcUOM       = UOM
  lnWeight    = NFABWEIGHT
  lnUnitQty   = NUNITQTY
  lnLineNo    = LineNo

  IF laScrMode[3] OR laScrMode[4]
    SHOW GET pbRemove ENABLE
  ENDIF  
ENDIF  

lcShowAble = IIF(laScrMode[2] OR laScrMode[1] OR EOF(lcDetTmp),'DIS','EN') + 'ABLE'
SHOW GET lcRef      &lcShowAble.
SHOW GET lcPoMat    DISABLE
SHOW GET lcFabric   &lcShowAble.
SHOW GET lcColor    &lcShowAble.
SHOW GET ibFabric   &lcShowAble.
SHOW GET ibColor    &lcShowAble.
SHOW GET lcIDesc    &lcShowAble.
SHOW GET lcClrDesc  &lcShowAble.
SHOW GET lnQty      &lcShowAble.
SHOW GET lnPrice    &lcShowAble.
SHOW GET lnAmount   &lcShowAble.
SHOW GET lnItemType &lcShowAble.
SHOW GET lcUOM      &lcShowAble.
SHOW GET lnWeight   &lcShowAble.
SHOW GET lnUnitQty  &lcShowAble.

=lfRefre3()
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvData3
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the vendor field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvData3

*IF MDOWN()
*  RETURN
*ENDIF

IF !(SEEK(laData[3],"ApVendor") AND 'C'$APVENDOR.CVENSUPTYP) AND LASTKEY() <> 9 AND LASTKEY() <> 15
  lcVendor = laData[3]
  =gfApVnBrow (@lcVendor,.F.,'C')
  laData[3] = lcVendor
ENDIF
IF EMPTY(laData[3])
  RETURN
ENDIF 

lcVEnAdd1=gfGetAdr('ApVendor','','','',1,'')
FOR lnCounter = 2 TO 5
  L=ALLTRIM(STR(lnCounter))
  lcTempVr=''
    FOR lnCount = 1 TO ALEN(laAddress,1)
      IF laAddress[lnCount,1] = lnCounter
        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
        lcTempVr = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
      ENDIF
    ENDFOR
    lcVEnAdd&L = lcTempVr
ENDFOR
=lfRefresh()

*!*************************************************************
*! Name        : lfvIData3
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the vendor field from the key field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvIData3

laData[3] = PADR('?',8)
=lfvData3()

*!*************************************************************
*! Name        : lfvData2
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the store field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvData2

PRIVATE lnAlias,lcFltSet
lnAlias = SELECT(0)
SELECT WAREHOUS
lcFltSet = SET('FILTER')
SET FILTER TO LMATINV
IF !SEEK( laData[2],  'WareHous' )
  laData[2] = gfBrowWare( .T. )
  IF EMPTY(laData[2])
    IF EMPTY(lcOldWare)
      =gfModalGen('INM36001B36000','DIALOG')
      _CUROBJ = OBJNUM(laData[2])
      SET FILTER TO &lcFltSet.
      SELECT (lnAlias)
      RETURN
    ELSE
      laData[2] = lcOldWare
    ENDIF
  ENDIF
ENDIF
SET FILTER TO &lcFltSet.
SELECT (lnAlias)

lcWareAdd1 =gfGetAdr('WareHous','','','',1,'')
FOR lnCounter = 2 TO 5
  L=ALLTRIM(STR(lnCounter))
  lcTempVr=''
  FOR lnCount = 1 TO ALEN(laAddress,1)
    IF laAddress[lnCount,1] = lnCounter
      lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
      lcTempVr  = lcTempVr+IIF(EMPTY(lcTempVr).OR.RIGHT(lcTempVr,1)=',' ,'',', ')+lcAddPart
    ENDIF
  ENDFOR
  lcWareAdd&L = lcTempVr
ENDFOR
=lfRefresh()

*!*************************************************************
*! Name        : lfvIData2
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the store field from the key field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvIData2

laData[2] = PADR('?',6)
=lfvData2()

*!*************************************************************
*! Name        : lfvData9
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the ship via field 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvData9
PRIVATE lnAlias

lnAlias = SELECT(0)
laData[9] = laShip[lnShip,2]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvData4
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the entered date  field 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvData4

IF EMPTY(laData[4])
  =gfModalGen('INM36005B36000','DIALOG','entered')
  _CUROBJ = OBJNUM(laData[4])
  laData[4] = ldOldEnt
  RETURN
ENDIF

IF laData[4] > laData[5]
  =gfModalGen('INM36003B36000','DIALOG')
  _CUROBJ = OBJNUM(laData[4])
  laData[4] = ldOldEnt
ENDIF  
SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY

*!*************************************************************
*! Name        : lfvData5
*! Developer   : AHMED MAHER (AMH)
*! Date        : 06/01/97
*!*************************************************************
*! Purpose     : validate the complete date field 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvData5

IF EMPTY(laData[5])
  =gfModalGen('INM36005B36000','DIALOG','complete')
  _CUROBJ = OBJNUM(laData[5])
  laData[5] = ldOldComp
  RETURN
ENDIF

IF laData[4] > laData[5]
  =gfModalGen('INM36003B36000','DIALOG')
  _CUROBJ = OBJNUM(laData[5])
  laData[5] = ldOldComp
ENDIF

*!*************************************************************
*! Name        : lfwData2
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get the old value of the store in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwData2

lcOldWare = laData[2]

*!*************************************************************
*! Name        : lfwData22
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get the old value of the store in a variable from the key field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwData22

=MDOWN()
lcOldWare = laData[2]

*!*************************************************************
*! Name        : lfwData3
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get the old value of the vendor in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwData3

lcOldVend = laData[3]

*!*************************************************************
*! Name        : lfwData32
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get the old value of the vendor in a variable from the key field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwData32

=MDOWN()
lcOldVend = laData[3]

*!*************************************************************
*! Name        : lpDelScr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : Delete invoice
*!*************************************************************
*! Calls       : 
*!*************************************************************
PROCEDURE lpDelScr

SELECT COMINVLN
IF SEEK(laData[1])
  DELETE REST WHILE INVOICE+CMATTYPE+POMAT+STR(LINENO,6,0)+FABRIC+COLOR = laData[1]
ENDIF
SELECT COMINVHD
IF SEEK(laData[1])
  DELETE
ENDIF
=lfRefresh()

*!*************************************************************
*! Name        : lfvNewPL
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : when adding line for the Invoice
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvNewPL

IF EMPTY(laData[2])
  =gfModalGen('INM36001B36000','DIALOG')
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

lcRef      = SPACE(10)
lcPoMat    = SPACE(06)
lcFabric   = SPACE(07)
lcColor    = SPACE(06)
lcIDesc    = SPACE(20) 
lcClrDesc  = SPACE(15) 
lnQty      = 0
lnPrice    = 0
lnAmount   = 0
lnItemType = 1
lcUOM      = SPACE(3)
lnWeight   = 0
lnUnitQty  = 0

SHOW GET lcRef      ENABLE
SHOW GET lcPoMat    DISABLE
SHOW GET lcFabric   ENABLE
SHOW GET lcColor    ENABLE
SHOW GET ibFabric   ENABLE
SHOW GET ibColor    ENABLE
SHOW GET lcIDesc    ENABLE
SHOW GET lcClrDesc  ENABLE
SHOW GET lnQty      ENABLE
SHOW GET lnPrice    ENABLE
SHOW GET lnItemType ENABLE
SHOW GET lcUOM      ENABLE
SHOW GET lnWeight   ENABLE
SHOW GET lnUnitQty  ENABLE

lNewLine = .T.
=lfRefresh()
_CUROBJ = OBJNUM(lcRef)

*!*************************************************************
*! Name        : lfvFabric
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the fabric field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT Fabric
IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
  llBrowse = .F.
  DO FaBrow WITH lcFabric,'*'
ENDIF
SHOW GET lcFabric ENABLE
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvColor
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the color field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT Fabric
IF !EMPTY(lcColor) AND !SEEK(lcFabric+lcColor)
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,6),CHR(240))
  lcOldF   = lcFabric
  SELECT FABRIC
  DO FaBrow WITH lcFabric,lcColor
  IF EMPTY(lcColor)
    lcFabric = lcOldF
    _CUROBJ = OBJNUM(lcColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF

IF !EMPTY(lcFabric) AND !EMPTY(lcColor) AND ;
   ((lcFabric <> lcOldFab) OR (lcColor <> lcOldClr))
  SELECT(lcDetTmp)
  IF lNewLine
    APPEND BLANK
    lnLastLine = lnLastLine + 1 
    lnNewRec = RECNO()
  ELSE  
    GOTO lnNewRec 
  ENDIF
  =SEEK(lcFabric+lcColor,'FABRIC')        
  lcIDesc   = FABRIC.DESC
  lcClrDesc = gfCodDes(lcColor, 'COLOR')
  
  =RLOCK()
  REPLACE INVOICE    WITH laData[1]        ,;
          FABRIC     WITH lcFabric         ,;
          COLOR      WITH lcColor          ,;
          CITMDESC   WITH lcIDesc          ,;
          CCLRDESC   WITH lcClrDesc        ,;
          CREFERENCE WITH lcRef            ,;
          ITEM_TYPE  WITH FABRIC.ITEM_TYPE ,;
          PRICE      WITH FABRIC.NFABCOST  ,;
          UOM        WITH FABRIC.UOMBUY    ,;
          NFABWEIGHT WITH 0                ,;
          LineNo     WITH lnLastLine       ,;
          LNEW       WITH lNewLine         ,;
          cStatus    WITH IIF(cStatus ='A',cStatus ,IIF(EMPTY(cStatus),'A','M'))
  UNLOCK
  =lfwLine()
  SHOW GET pbRemove ENABLE
ELSE
  IF LASTKEY() <> 15 AND EMPTY(lcFabric) AND EMPTY(lcColor)
    =gfModalGen('INM36013B36000','DIALOG')
    _CUROBJ = IIF(EMPTY(lcFabric),OBJNUM(lcFabric),OBJNUM(lcColor))    
    RETURN
  ELSE  
    RETURN
  ENDIF  
ENDIF
SHOW GET lcFabric  ENABLE
SHOW GET lcColor   ENABLE
SHOW GET lcIDesc   ENABLE
SHOW GET lcClrDesc ENABLE
lNewLine = .F.
=lfRefresh()
SHOW GETS WINDOW (lcWinC32) ENABLE ONLY
SHOW WINDOW (lcDet_Ttl) REFRESH SAME
SHOW GET lcPoMat DISABLE
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvKFabric
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the fabric field from the key field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvKFabric

lcFabric = PADR('?',7)
=lfvFabric()
_CUROBJ = OBJNUM(lcColor)

*!*************************************************************
*! Name        : lfvKColor
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the color field from the key field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvKColor

lcColor = PADR('?',6)
=lfvColor()

*!*************************************************************
*! Name        : lfwData5
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old complete date in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwData5

ldOldComp = laData[5]

*!*************************************************************
*! Name        : lfwData4
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old entered date in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwData4

ldOldEnt = laData[4]

*!*************************************************************
*! Name        : lpSavScr
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*!*************************************************************
*! Purpose     : save the modification or a new record 
*!*************************************************************
*! Calls       : 
*!*************************************************************
PROCEDURE lpSavScr

lnAlias = SELECT(0)
IF laScrMode[4] AND laData[23] = 0
  =gfModalGen('INM36101B36000','DIALOG','Commercial Invoice')
  lnlastfold = lnactfolder
  lnactfolder  = 2
  =lfchngfolder(lnactfolder)
  llCSave = .F.
  RETURN
ENDIF
IF EMPTY(laData[2])
  =gfModalGen('INM36002B36000','DIALOG','Warehouse')
  lnlastfold = lnactfolder
  lnactfolder  = 1
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(laData[2])
  llCSave = .F.
  RETURN
ENDIF
IF llMultiWH AND EMPTY(laData[3])
  =gfModalGen('INM36002B36000','DIALOG','Consignee')
  lnlastfold = lnactfolder
  lnactfolder  = 1
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(laData[3])
  llCSave = .F.
  RETURN
ENDIF
IF EMPTY(laData[9])
  laData[9] = laShip[lnShip,2]
ENDIF

SELECT (lcDetTmp)
SCAN
  IF nFabTotQty = 0
    IF gfModalGen('INM36102B36007','DIALOG') = 2
      lnlastfold = lnactfolder
      lnactfolder  = 2
      =lfchngfolder(lnactfolder)
      llCSave = .F.
      RETURN
    ENDIF
    EXIT
  ENDIF
ENDSCAN
DELETE ALL FOR nFabTotQty = 0 AND cStatus <> 'S'
LOCATE
IF EOF()
  =gfModalGen('INM36015B36000','DIALOG')
  lnlastfold = lnactfolder
  lnactfolder  = 2
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(pbNew)
  llCSave = .F.
  RETURN
ENDIF

IF laScrMode[4]
  laData[1] = gfSequence('CCOMINV')
  SELECT (lcDetTmp)
  REPLACE ALL INVOICE WITH laData[1]
ENDIF

=lfUpdLines()
=lfUpdHdr()

=lfUpdUsrFl('CCONTAINER',laData[06])
=lfUpdUsrFl('CSEAL'     ,laData[07])
=lfUpdUsrFl('CBOOKING'  ,laData[08])
=lfUpdUsrFl('CTOTWEIGHT',laData[10])
=lfUpdUsrFl('NCARTON'   ,laData[11])
=lfUpdUsrFl('CCONTOUT1' ,laData[12])
=lfUpdUsrFl('CCONTOUT2' ,laData[13])
=lfUpdUsrFl('CCONTOUT3' ,laData[14])
=lfUpdUsrFl('CCONTOUT4' ,laData[15])
=lfUpdUsrFl('CCONTOUT5' ,laData[16])
=lfUpdUsrFl('NUNITQTY'  ,laData[17])
=lfUpdUsrFl('NQUANTITY1',laData[18])
=lfUpdUsrFl('NQUANTITY2',laData[19])
=lfUpdUsrFl('NQUANTITY3',laData[20])
=lfUpdUsrFl('NQUANTITY4',laData[21])
=lfUpdUsrFl('NQUANTITY5',laData[22])
=lfUpdUsrFl('NCARTONS'  ,laData[25])

IF laScrMode[4]
  SHOW GET laData[1]
  lcMsgTxt='Commercial Invoice'
  =gfModalGen('INM36085B36000','DIALOG',lcMsgTxt+'|'+laData[1])
ENDIF

laScrMode[2] = .T.
laScrMode[1] = .F.
SHOW GET ibFolder[3] ENABLE
lnlastfold  = lnactfolder
lnactfolder = 1
=lfchngfolder(lnactfolder)
SELECT(lcBaseFile)

*!*************************************************************
*! Name        : lfActPad
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : activate a new pad with the option
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Append from Material P/O'      SKIP FOR (lnactfolder <> 2)OR laScrMode[1] OR laScrMode[2]
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Append from Material Shipment' SKIP FOR (lnactfolder <> 2)OR laScrMode[1] OR laScrMode[2]
ON SELECTION POPUP _OPTIONPOP DO lpOptions WITH BAR()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP

*!*************************************************************
*! Name        : lpOptions
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : define the actions done when selecting one of the option bars
*!*************************************************************
*! Calls       : 
*!*************************************************************
PROCEDURE lpOptions
PARAMETERS lnBar

DO CASE
  CASE lnBar = 1
    DO lpPoSele
  CASE lnBar = 2
    DO lpShpSele
ENDCASE    

*!*************************************************************
*! Name        : lfvQty
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the quantity field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvQty

IF lnQty <= 0
  =gfModalGen('INM36014B36000','DIALOG','Quantity')
  lnQty   = lnOldQty
  _CUROBJ = OBJNUM(lnQty)
ELSE
  laData[23] = laData[23] + lnQty - lnOldQty
  laData[24] = laData[24] + ((lnQty - lnOldQty) * lnPrice)
  lnAmount = lnQty * lnPrice
  REPLACE (lcDetTmp+'.nFabTotQty') WITH lnQty
  =lfRefresh()
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name        : lfwQty
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old quantity  in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwQty

lnOldQty = lnQty

*!*************************************************************
*! Name        : lfvPrice
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the Price field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvPrice

IF lnPrice < 0
  =gfModalGen('INM36014B36000','DIALOG','Price')
  lnPrice = lnOldPrice
  _CUROBJ = OBJNUM(lnPrice)
ELSE  
  laData[24] = laData[24] + (lnQty * (lnPrice-lnOldPrice))
  lnAmount = lnQty * lnPrice
  REPLACE (lcDetTmp+'.Price') WITH lnPrice
  =lfRefresh()
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name        : lfwPrice
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old price in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwPrice

lnOldPrice = lnPrice

*!*************************************************************
*! Name        : lfvRef
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the Reference field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvRef

REPLACE (lcDetTmp+'.cReference') WITH lcRef
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*!*************************************************************
*! Name        : lfvUOM
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the UOM field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvUOM

REPLACE (lcDetTmp+'.UOM') WITH lcUOM
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*!*************************************************************
*! Name        : lfvWeight
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/30/2002
*!*************************************************************
*! Purpose     : validate the Weight field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvWeight

REPLACE (lcDetTmp+'.NFABWEIGHT') WITH lnWeight
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*!*************************************************************
*! Name        : lfvUnitQty
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the Units field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvUnitQty

IF lnUnitQty < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Units cannot be less than zero.')
  lnUnitQty   = lnOldUntQt
  _CUROBJ = OBJNUM(lnUnitQty)
ELSE
  laData[25] = laData[25] + lnUnitQty - lnOldUntQt
  REPLACE (lcDetTmp+'.nUnitQty') WITH lnUnitQty
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name        : lfwUnitQty
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old Units in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwUnitQty

lnOldUntQt = lnUnitQty

*!*************************************************************
*! Name        : lfvIDesc
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the Item Description field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvIDesc

REPLACE (lcDetTmp+'.cItmDesc') WITH lcIDesc
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*!*************************************************************
*! Name        : lfvClrDesc
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the Color Description field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvClrDesc

REPLACE (lcDetTmp+'.cClrDesc') WITH lcClrDesc
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*!*************************************************************
*! Name        : lfvItmType
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : validate the Item Type field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvItmType

REPLACE (lcDetTmp+'.Item_Type') WITH laItemType[lnItemType,2]
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*!*************************************************************
*! Name        : lfUpdLines
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : to update COMINVLN file 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfUpdLines

lnAlias = SELECT(0)
IF laScrmode[3]
  SELECT COMINVLN
  IF SEEK(laData[1])
    DELETE REST WHILE INVOICE+CMATTYPE+POMAT+STR(LINENO,6,0)+FABRIC+COLOR = laData[1]
  ENDIF
ENDIF
SELECT COMINVLN
APPEND FROM (gcWorkDir+lcDetTmp)
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfUpdHdr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : to update COMINVHD file 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfUpdHdr

SELECT COMINVHD
IF laScrMode[4]
  APPEND BLANK
ELSE
  SEEK(laData[1])  
ENDIF

=RLOCK()
GATHER FROM laData FIELDS &lcScFields.
UNLOCK

*!*************************************************************
*! Name        : lfvRemLine
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : to remove line from the Invoice lines
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvRemLine

lnAlias = SELECT(0)
SELECT(lcDetTmp)
IF gfModalGen('INM36017B36001','DIALOG') = 1
  ladata[23] = ladata[23] - nFabTotQty
  ladata[24] = ladata[24] - (nFabTotQty*Price)
  laData[25] = laData[25] - nUnitQty
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)
  DELETE
ENDIF
SHOW WINDOW (lcDet_Ttl) REFRESH SAME
LOCATE
IF EOF(lcDetTmp)
  SHOW GET pbRemove DISABLE
  lcFabric    = SPACE(07) 
  lcColor     = SPACE(06) 
  lcRef       = SPACE(10) 
  lcPO        = SPACE(06)
  lcClrDesc   = SPACE(15)
  lcIDesc     = SPACE(20)
  lnQty       = 0
  lnPrice     = 0
  lnAmount    = 0
  lcUOM       = SPACE(3)
  lnWeight    = 0
  lnUnitQty   = 0
  lnTotQty    = 0
  lnLineNo    = 0
ENDIF
=lfwLine()
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfwFabric
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old fabric field in a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwFabric

lcOldFab = lcFabric

*!*************************************************************
*! Name        : lfwColor
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : take the old color fieldin a variable
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwColor

lcOldClr = lcColor

*!*************************************************************
*! Name        : lfvBrows
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : valid function for the lines browse
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvBrows
IF WONTOP() # (lcDet_Ttl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name        : lpPoSele
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : select a P/O to copy lines from
*!*************************************************************
*! Calls       : 
*!*************************************************************
PROCEDURE lpPoSele

PRIVATE laBrow,lcBrTitle,lcBrFields
lnAlias = SELECT(0)
lcPOBrow  = laData[1]
SELECT POFHDR
PRIVATE lnCurRec
lnCurRec = RECNO()
LOCATE FOR CMATTYPE+POMAT = 'P'
IF !FOUND()
  =gfDialog ("I","No records to display.")
  RETURN
ENDIF
DECLARE laBrow[1]
laBrow =' '
lcBrTitle  = 'Purchase Orders'
lcBrFields = "POMAT   :R :H='PO #':8,"+;
             "Status  :R :H='S':2,"+;	         	           	         
             "Vendor  :R :H='Vendor':10,"+;
	         "Complete:R :H='Complete':8,"+;
  	         "NFABORDER  :R :H='Tot.Qty.':7,"+;	         
             "POTotal :R :H='Amount':10,"+;
             "NFBRECEIVE :R :H='Receive':7,"+;
             "NPO_OPEN   :R :H='Open':7"
llRetValue = gfBrows("FOR cMatType ='P' AND STATUS $'HOC'","POMAT","laBrow",lcBrTitle)

IF llRetValue
  lcPOBrow = laBrow[1]
ELSE
  lcPOBrow = SPACE(06)
ENDIF  
IF !EMPTY(lcPOBrow)
  =lfSelPoLin()
  SELECT(lcPOLine)
  LOCATE
  IF EOF()
    =gfModalGen('INM36018B36000','DIALOG',lcPOBrow )
    SELECT (lnAlias)
    RETURN
  ELSE
    PUSH KEY
    ON KEY
    DO (gcScrDir+gcWinAppl+'\POSELE.SPX')
    POP KEY
  ENDIF  
ENDIF

IF lnCurRec <= RECCOUNT('POFHdr')
  GOTO lnCurRec IN POFHdr
ENDIF  
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfSelPoLin
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get all the recieved lines for the selected P/O
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfSelPoLin

lnAlias = SELECT(0)
SELECT *,'' AS cMarker,'' AS cStatus FROM POFLN;
  WHERE cMatType = 'P'      AND;
        POMAT    = lcPOBrow AND;
        TRANCD   = '1';
  INTO DBF (gcWorkDir+lcPOLine)
SELECT(lcPOLine)
INDEX ON cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) TAG lcPOLine
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfApplin
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : append line for the P/O
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfApplin
PRIVATE lnAlias 

lnAlias = SELECT(0)
SELECT(lcPOLine)
SCAN FOR !EMPTY(cStatus)
  SELECT(lcDetTmp) 
  APPEND BLANK
  lnLastLine = lnLastLine + 1
  REPLACE INVOICE  WITH laData[1],;
          CMATTYPE WITH 'S',;
          POMAT    WITH lcPOBrow,;
          FABRIC   WITH EVALUATE(lcPOLine+'.FABRIC'),;
          COLOR    WITH EVALUATE(lcPOLine+'.COLOR'),;
          LineNo   WITH lnLastLine,;
          LNEW     WITH lNewLine,;
          cStatus  WITH 'A'
  
  =SEEK(FABRIC+COLOR,'FABRIC')
  REPLACE NFABTOTQTY WITH EVALUATE(lcPOLine+'.NFABTOTQTY'),;
          PRICE      WITH EVALUATE(lcPOLine+'.NCOST1'),;
          ITEM_TYPE  WITH FABRIC.ITEM_TYPE,;
          CITMDESC   WITH FABRIC.DESC,;
          CCLRDESC   WITH gfCodDes(COLOR, 'COLOR'),;
          UOM        WITH FABRIC.UOMBUY,;
          NFABWEIGHT WITH 0
  laData[23] = laData[23] + NFABTOTQTY
  laData[24] = laData[24] + (NFABTOTQTY*PRICE)
ENDSCAN
SELECT(lcDetTmp)
=lfwLine()
SHOW WINDOW (lcDet_Ttl) REFRESH
=lfRefresh()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpClsScr
*! Purpose   : Close screen P/O.
*!*************************************************************
PROCEDURE lpClsScr

SELECT (lcBaseFile)
IF laScrMode[3] 
  =lfInit()
ENDIF

* --------------------------------------------------------------- 
FUNCTION lfTrapKeys

IF WONTOP() = lcDet_Ttl
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShiftTab
  ON KEY LABEL ESC        DO gfCPClose
ENDIF  
* --------------------------------------------------------------- 
PROCEDURE lpShiftTab

ON KEY LABEL BACKTAB
IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcFolder)
  _CUROBJ = OBJNUM(ibFolder[2])
ENDIF

* --------------------------------------------------------------- 
PROCEDURE lpTab

ON KEY LABEL TAB
IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWinC33)
  _CUROBJ = OBJNUM(pbNew)
ENDIF

* --------------------------------------------------------------- 
FUNCTION lfClearTrap

IF glFromBrow
  =gfStopBrow()
ENDIF

IF WONTOP() <> lcDet_Ttl
  ON KEY LABEL ESC &lcOldEscTrap
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF

*!*************************************************************
*! Name        : lfvSelec
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : select one or all recieved lines
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvSelec
PARAMETER llAll

lnAlias = SELECT(0)
SELECT (lcPOLine)
lnRecno = RECNO()
IF llAll
  REPLACE ALL cStatus WITH "»"
ELSE
  REPLACE cStatus WITH "»"
ENDIF
SHOW WINDOW (lcRec_Ttl) REFRESH SAME
SHOW GET pbUSele  ENABLE 
SHOW GET pbUSeleA ENABLE 
SHOW GET pbSele   DISABLE 
SHOW GET pbSeleA  DISABLE 
GOTO TOP
SCAN
  IF EMPTY(cStatus)
    SHOW GET pbSele  ENABLE   
    SHOW GET pbSeleA ENABLE       
    EXIT
  ENDIF
ENDSCAN
GOTO lnRecno
=lfwPOl()
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfvUSelec
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : unselect line or all recieving lines 
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvUSelec
PARAMETER llAll

lnAlias = SELECT(0)
SELECT (lcPOLine)
IF llAll
  REPLACE ALL cStatus WITH SPACE(01)
ELSE
  REPLACE cStatus WITH SPACE(01)
ENDIF
SHOW WINDOW (lcRec_Ttl) REFRESH
SHOW GET pbSele  ENABLE 
SHOW GET pbSeleA ENABLE 
SHOW GET pbUSele  DISABLE 
SHOW GET pbUSeleA DISABLE 
SCAN
  IF !EMPTY(cStatus)
    SHOW GET pbUSele  ENABLE 
    SHOW GET pbUSeleA ENABLE 
    EXIT
  ENDIF
ENDSCAN
=lfwPOl()
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfBrPOLin
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : browse all lines for the selected P/O
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfBrPOLin

lnAlias = SELECT(0)
lcBrowFlds = [cMarker :H=' ' :R :1 :W=.F.,]+;
             [cStatus :H='',]+;
             [Fabric  :10:H='Item':R,]+;
             [Color   :10:H='Color':R,]+;
             IIF(llMultiWH,[cWareCode:6:H='Ware.':R,],[])+;
             [date:8:H='Date':R,]+;
             [Reference:10:H='Reference':R,]+;
             [nFabTotQty:11:H='Quantity':P='9999999.999':R]
SELECT(lcPOLine)
GOTO TOP
lnMarker = RECNO()
BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwPOl();
       TITLE lcRec_Ttl    ;
       WINDOW POSELE1 IN WINDOW POSELE
ACTIVATE WINDOW (lcRec_Ttl)
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfvTotQty
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : Validate the nquantity1-5 fields
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvTotQty

PRIVATE lnI
lnTotQty = 0
FOR lnI = 18 TO 22
  lnTotQty = lnTotQty + laData[lnI]
ENDFOR
=lfRefresh()

PROCEDURE lpActRec

ACTIVATE WINDOW(lcWinC32)
IF llFromRef
  _CUROBJ = OBJNUM(pbRemove)
  llFromRef = .F.
ELSE
  _CUROBJ = OBJNUM(lcRef)
ENDIF

*!*************************************************************
*! Name        : lfwPOl
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/21/2002
*!*************************************************************
*! Purpose     : when for the recievung lines browse
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfwPOl

PRIVATE lnNewRec 
lnAlias = SELECT(0)
SELECT (lcPOLine)
lnNewRec = RECNO()
REPLACE ALL cMarker WITH lcUnMark
GO IIF(lnNewRec < RECCOUNT(lcPOLine),lnNewRec ,RECCOUNT(lcPOLine))
REPLACE cMarker WITH lcMark
SHOW WINDOW (lcRec_Ttl) REFRESH
SELECT (lnAlias)

*!*************************************************************
*! Name        : lpShpSele
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/21/2002
*!*************************************************************
*! Purpose     : select a Shipment to copy lines from
*!*************************************************************
*! Calls       : 
*!*************************************************************
PROCEDURE lpShpSele

PRIVATE laBrow,lcBrTitle,lcBrFields
lnAlias = SELECT(0)
lcPOBrow  = laData[1]
SELECT MATSHPHD
PRIVATE lnCurRec
lnCurRec = RECNO()
LOCATE
IF !FOUND()
  =gfDialog ("I","No records to display.")
  RETURN
ENDIF
DECLARE laBrow[1]
laBrow =' '
lcBrTitle  = 'Shipments'
lcBrFields = "SHIPNO     :R :H='Shipment #':8,"+;
             "Status     :R :H='S':2,"+;	         	           	         
	         "ENTERED    :R :H='Entered':8,"+;
  	         "NFABTOTQTY :R :H='Tot.Qty.':7,"+;	         
             "RECVSTK    :R :H='Receive':7"
llRetValue = gfBrows("FOR STATUS $'OC'","SHIPNO","laBrow",lcBrTitle)

IF llRetValue
  lcPOBrow = laBrow[1]
ELSE
  lcPOBrow = SPACE(06)
ENDIF  
IF !EMPTY(lcPOBrow)
  =lfSelShpLn()
  SELECT(lcPOLine)
  LOCATE
  IF EOF()
    =gfModalGen('INM36018B36000','DIALOG',lcPOBrow )
    SELECT (lnAlias)
    RETURN
  ELSE
    PUSH KEY
    ON KEY
    DO (gcScrDir+gcWinAppl+'\POSELE.SPX')
    POP KEY
  ENDIF  
ENDIF

IF lnCurRec <= RECCOUNT('POFHdr')
  GOTO lnCurRec IN MATSHPHD
ENDIF  
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfSelShpLn
*! Developer   : AHMED MAHER (AMH)
*! Date        : 12/19/2002
*!*************************************************************
*! Purpose     : get all the recieved lines for the selected Shipment
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfSelShpLn

lnAlias = SELECT(0)
SELECT *,'' AS cMarker,'' AS cStatus FROM MATSHPDT;
  WHERE SHIPNO = lcPOBrow AND;
        TRANCD = '3';
  INTO DBF (gcWorkDir+lcPOLine)
SELECT(lcPOLine)
INDEX ON cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) TAG lcPOLine
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfUpdUsrFl
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/21/2002
*! Purpose   : Updating User defined fields.
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.