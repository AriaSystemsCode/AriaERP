*:***************************************************************************
*: Program file  : ICEIL60.PRG
*: Program desc. : Temporary Inventory Adjustment.
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : EILEEN FISHER
*:***************************************************************************
*: C101399,1 KHM 03/23/99
*:***************************************************************************
*: Modifications:
*: B802929,1 ADEL 01/30/2000 Fix the way the program updates style,stydye, styinvjl, and GL as
*: B802929,1                 it is completely wrong and fix the bug of not updating STYINVJL with dyelot.
*:***************************************************************************

*** Init all program global variables.
lcFields   = 'cBatchNo,cBatDesc,cStatus,lLock,dPostDate'  && Scatter fields.
lcScrMode  = 'V'                      && Screen Mode.
llBrowse   = .F.                      && Do not browse.
lcOldValue = ''                       && Old Value for the when clauses.
llUpdated  = .F.                      && Any modifications done.
lcStatus   = ''                       && Status Desc for the screen.
lcString   = 'OVAP'                   && All avilable status codes.
gcHost     = 'ICEIL600'               && The host screen.
lcTmpFile  = gfTempName()             && The temp work file.
lcTmpStock = gfTempName()             && The temp work file.
lcTmpDyelt = gfTempName()             && The temp work file.
llNewBatch = .F.                      && Processing new batch.
llNewLine  = .F.                      && New Line.
lcStyDesc  = ''                       && Style Desc.
lcScale    = ''                       && Style Scale.
lcWHDesc   = ''                       && WareHouse desc.
lcNewBatch = ''
lcBrowBmp  = gcBmpHome + "ExtKey.BMP"     && Ext Key bit map bath.
llExit     = .F.                            && exit Control flag from line entry.

STORE 0   TO lnstylewid
lcIMjrPt   = gfItemMask('PI')
lcMjrPct   = gfItemMask('PM')
lcNMjrPt   = gfItemMask('PN')
lnStyleWid = LEN(lcMjrPct)
lcSepart   = SUBSTR(lcIMjrPt,lnstylewid+1,1)

*** Array to hold all avilable status desc.
DECLARE laStatus[4]
laStatus[1] = 'Open'                  && Code = 'O'
laStatus[2] = 'Void'                  && Code = 'V'
laStatus[3] = 'Approved'              && Code = 'A'
laStatus[4] = 'Posted'                && Code = 'P'

gnStyleWid = 12
gnColorWid = 6
lcGlFYear  = SPACE(04)
lcGlPeriod = SPACE(02)

IF !gfSetup()
  RETURN
ENDIF  


DIMENSION laSetup[3,2]
laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_LINK_GL'
laSetUp[3,1] = 'M_WAREHOUSE'
=gfGetMemVar(@laSetUp,gcAct_Comp)

llDyelot  = ALLTRIM(laSetUp[1,2]) = 'Y'
llGlLink  = ALLTRIM(laSetUp[2,2]) = 'Y'
llMultiWH = ALLTRIM(laSetUp[3,2]) = 'Y'

*-- Opening all the necessary files.
=gfOpenFile(gcDataDir+'Style','Style'   ,'SH')
=gfOpenFile(gcDataDir+'StyDye','StyDye'  ,'SH')
=gfOpenFile(gcDataDir+'Scale','Scale'   ,'SH')
=gfOpenFile(gcDataDir+'TmpInvH','TmpInvH' ,'SH')
=gfOpenFile(gcDataDir+'TmpInvL','TmpInvLB','SH')
=gfOpenFile(gcDataDir+'TmpStock','TmpStock','SH')
=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
GOTO TOP IN WareHous

SELECT StyDye
SET RELATION TO StyDye.cWareCode INTO WAREHOUS ADDITIVE
GO TOP

IF llGlLink
  *-- OPEN GLDIST FILE TO CALL GENERAL LEDGER DISTRIBUTION PROCEDURE 
  =gfOpenFile(gcDataDir+'GLDIST','GLDISTAC','SH')  
  *** OPEN A TEMP FILE TO BE USED IN CALLING 'GLDIST' PROCEDURE.
  TmpGlDist = gfTempName()
  COPY STRUCTURE TO (gcWorkDir+TmpGlDist)
  =gfOpenFile(gcWorkDir+TmpGlDist,'','EX')
ENDIF  
*B802929,1 (Begin) Prepare laGLDistAr array for updateing gldist.
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = ''
  laGLDistAr[2,1] = ''
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IP'      TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE gdSysDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGlFYear TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE TmpGlDist TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = ''
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF
*B802929,1 (End)


*** Create the temp work file.
SELECT TmpInvL
=AFIELDS(laFileStru)
CREATE DBF(gcWorkDir+lcTmpFile)  FROM ARRAY laFileStru

SELECT TmpStock
=AFIELDS(laFileStru)
CREATE DBF (gcWorkDir+lcTmpStock)  FROM ARRAY laFileStru
INDEX ON cBatchNo+Style+Color+cWareCode+Dyelot TAG (lcTmpStock) OF (lcTmpStock)

CREATE DBF (gcWorkDir+lcTmpDyelt) (Dyelot C(10),nOrg1 N(6,0),nOrg2 N(6,0),nOrg3 N(6,0),;
    nOrg4 N(6,0),nOrg5 N(6,0),nOrg6 N(6,0),nOrg7 N(6,0),nOrg8 N(6,0),nTotOrg N(7,0),;
    nBal1 N(6,0),nBal2 N(6,0),nBal3 N(6,0),nBal4 N(6,0),nBal5 N(6,0),nBal6 N(6,0),nBal7 N(6,0),nBal8 N(6,0),nTotBal N(7,0))

*** Adjust the colors of all screen objects in case of DOS or UNIX 

SELECT TmpInvH
SCATTER FIELDS &lcFields MEMO MEMVAR BLANK

PUSH KEY
PUSH MENU _MSYSMENU
DO gcScrDir+'IC\ICEIL600.SPX'
POP MENU _MSYSMENU
POP KEY

*!*************************************************************
*! Name      : lfShow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The show function for the program.
*!*************************************************************
*! Example            :  =lfShow()
*!*************************************************************
FUNCTION lfShow

SELECT TmpInvH

* View and Modify modes
IF lcScrMode = 'V'
  llNewBatch  = .F.
  SET ORDER TO 0 IN TmpInvL
  SELECT * FROM TmpInvL;
    WHERE cBatchNo+cSheet+Style+Color+cWareCode+Dyelot  = m.cBatchNo INTO DBF (gcWorkDir+lcTmpFile)
          
  SET ORDER TO 0 IN TmpStock
  SELECT * FROM TmpStock;
    WHERE cBatchNo+Style+Color+cWareCode+Dyelot = m.cBatchNo INTO DBF (gcWorkDir+lcTmpStock)
   
  INDEX ON cBatchNo+Style+Color+cWareCode+Dyelot TAG (lcTmpStock) OF (gcWorkDir+lcTmpStock)
  SET ORDER TO TmpStock IN TmpStock
  
  SELECT (lcTmpFile)
  SET RELATION TO cBatchNo+Style+Color+cWareCode+Dyelot INTO (lcTmpStock)
  GO TOP
  SET ORDER TO TmpInvLB IN TmpInvL
    
  RELEASE WINDOW (lcBrowTtl)
  =lfActBrow()
  SELECT TmpInvH
  SCATTER FIELDS &lcFields MEMO MEMVAR
  
  IF !m.lLock
    REPLACE lLock WITH .T.
    m.lLock = .T.
  ELSE
    m.lLock = .F.
  ENDIF
  
  SHOW GETS DISABLE

  IF m.lLock
    IF m.cStatus = 'V'
      SHOW GET pbVoid,1 ENABLE PROMPT 'Un\<void'
      SHOW GET pbNew    ENABLE
    ELSE
      IF !(m.cStatus $ 'AP')
        SHOW GET pbVoid,1 ENABLE PROMPT '\<Void'
      ENDIF
    ENDIF
  
    IF m.cStatus = 'A'
      SHOW GET pbApprove,1 ENABLE PROMPT 'Dis\<approve'
      SHOW GET pbPost      ENABLE
    ELSE
      IF !(m.cStatus $ 'VP')
        SHOW GET pbApprove,1 ENABLE PROMPT '\<Approve'
      ENDIF
    ENDIF
  
    IF m.cStatus = 'P'
      SHOW GET pbNew    ENABLE
    ENDIF
  ENDIF
  
ELSE    
* New and Modify modes
  SELECT TmpInvH
  SCATTER FIELDS &lcFields MEMO MEMVAR BLANK
  
  m.cBatchNo  = lfSequance()
  lcNewBatch  = m.cBatchNo
  m.cStatus   = 'O'
  m.dPostDate = DATE()
  llNewBatch  = .T.
  m.lLock     = .T.
  
  SHOW GETS            DISABLE
  SHOW GET pbVoid,1    ENABLE PROMPT '\<Void'
  SHOW GET pbApprove,1 ENABLE PROMPT '\<Approve'
ENDIF

IF m.cStatus = 'O' .AND. m.lLock
  SHOW GET m.cBatDesc   ENABLE
  SHOW GET pbIns        ENABLE
  SHOW GET pbDel        ENABLE
  SHOW GET pbMod        ENABLE
  SHOW GET pbSave       ENABLE
ENDIF

SHOW GET ibTab          ENABLE
SHOW GET ibBackTab      ENABLE
SHOW GET pbClose        ENABLE

lcStatus = IIF(!EMPTY(m.cStatus),laStatus[AT(m.cStatus,lcString)],'')
=lfRefresh()

*!*************************************************************
*! Name      : lfvcBatch
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Validation function to the batch no field.
*!*************************************************************
*! Example            :  =lfvcBatch()
*!*************************************************************
FUNCTION lfvcBatch

IF !EMPTY(m.cBatchNo) .OR. llBrowse
  IF !SEEK(m.cBatchNo,'TmpInvH')
    *IF llBrowse .OR. ('?' $ m.cBatchNo) .OR. gfDialog('I','Batch number not found.','\<Browse;\<Reenter') = 1
    IF llBrowse .OR. ('?' $ m.cBatchNo) .OR.  gfModalGen('QRM42165B42014','DIALOG' ) = 1
      SELECT TmpInvH
      lcBrFields = [cBatchNo:H="Batch Number",cBatDesc:H="Description",cStatus:H="Status"]
      DECLARE laData[1]
      
      IF ARIABROW('',"Batches",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","cBatchNo","laData")
        m.cBatchNo = laData[1]
      ELSE
        m.cBatchNo = SPACE(6)
        _CUROBJ    = OBJNUM(pbNew)
      ENDIF  
    ELSE  
      m.cBatchNo = SPACE(6)
      _CUROBJ    = OBJNUM(m.cBatchNo)
    ENDIF
  ENDIF
  
  IF !EMPTY(m.cBatchNo)
    lcScrMode = 'V'
    =lfShow()
  ENDIF
  llBrowse = .F.
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the new push botton.
*!*************************************************************
*! Example            :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew

SELECT TmpInvH
GO TOP
LOCATE FOR cStatus $ 'OA'

IF FOUND()
  IF cStatus = 'A'
    =gfModalGen('INM42160B00000','DIALOG' )
  ELSE
     =gfModalGen('INM42161B00000','DIALOG' )               
  ENDIF
  
  _CUROBJ = OBJNUM(m.cBatchNo)
  RETURN
ELSE
  IF !EMPTY(m.cBatchNo) .AND. m.lLock
    =SEEK(m.cBatchNo,'TmpInvH')
    REPLACE TmpInvH.lLock WITH .F.
  ENDIF
ENDIF

m.cStatus = 'O'
lcScrMode = 'N'
=lfShow()

*!*************************************************************
*! Name      : lfvVoid
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function of the void,unvoid push botton.
*!*************************************************************
*! Example            :  =lfvVoid()
*!*************************************************************
FUNCTION lfvVoid
PRIVATE lcOldVal,lnOldRec

lcOldVal = m.cStatus

SELECT TmpInvH
lnOldRec = RECNO()
GO TOP
LOCATE FOR cStatus $ 'OA'

IF !llNewBatch
  GO lnOldRec
ENDIF

IF FOUND() .AND. lcOldVal = 'V'
  m.cStatus = lcOldVal
  =lfvSave()
  =gfModalGen('INM42162B00000','DIALOG' )
ELSE
  m.cStatus = IIF(m.cStatus='V','O','V')
  =lfvSave()
  lcTxtMsg = IIF(lcOldVal $ 'OA','voided.','unvoided.')
  =gfModalGen('INM42163B00000','DIALOG',m.cBatchNo+"|"+lcTxtMsg)
ENDIF

*!*************************************************************
*! Name      : lfvApprove
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the approve pb.
*!*************************************************************
*! Example            :  =lfvApprove()
*!*************************************************************
FUNCTION lfvApprove

lcOldVal  = m.cStatus
m.cStatus = IIF(m.cStatus='A','O','A')
=lfvSave()
lcTxtMsg = IIF(lcOldVal = 'O','approved.','disapproved.')
=gfModalGen('INM42163B00000','DIALOG',m.cBatchNo+"|"+lcTxtMsg)

*!*************************************************************
*! Name      : lfvPost
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the post pb.
*!*************************************************************
*! Example            :  =lfvPost()
*!*************************************************************
FUNCTION lfvPost

IF !CheckPrd(gdSysDate,'lcGlFYear','lcGlPeriod','IP')
  RETURN
ENDIF
*B802929,1 (Begin) Update the fisical year and period var.
IF llGlLink
 STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
 STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
ENDIF  
*B802929,1 (End


m.cStatus = 'P'
=lfvSave()
=lfSv2Mast()  
lcTxtMsg = "posted."
=gfModalGen('INM42163B00000','DIALOG',m.cBatchNo+"|"+lcTxtMsg)

*!*************************************************************
*! Name      : lfvIns
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the inv pb.
*!*************************************************************
*! Example            :  =lfvIns()
*!*************************************************************
FUNCTION lfvIns

DO WHILE .T.
  SELECT (lcTmpFile)
  lcSheet  = cSheet
  lcStyle  = SUBSTR(Style,1,12)
  SCATTER MEMVAR BLANK
  SELECT (lcTmpStock)
  SCATTER MEMVAR BLANK
  DECLARE laOldQty[9]
  STORE 0 TO laOldQty

  m.cBatchNo  = IIF(llNewBatch,lcNewBatch,TmpInvH.cBatchNo)
  m.cWareCode = IIF(llMultiWH ,m.cWareCode, WareHous.cWareCode)
  m.cSheet    = lcSheet
  m.style     = lcStyle
  llNewLine   = .T.
  lcStyDesc   = ''                       && Style Desc.
  lcScale     = ''                       && Style Scale.
  lcWHDesc    = ''                       && WareHouse desc.

  PUSH KEY
  ON KEY
  DO gcScrDir+"IC\ICEil604.SPX"
  POP KEY
  
  IF llExit
    EXIT
  ENDIF
ENDDO

CLEAR TYPEAHEAD  
SHOW WINDOW (lcBrowTtl) REFRESH
ACTIVATE WINDOW (lcBrowTtl) TOP
KEYBOARD "{ALT+B}" CLEAR

*!*************************************************************
*! Name      : lfvcSheet
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the m.cSheet field.
*!*************************************************************
*! Example            :  =lfvcSheet()
*!*************************************************************
FUNCTION lfvcSheet

IF !EMPTY(m.cSheet)
  llExit=.F.
  SHOW GET m.cSheet DISABLE
  SHOW GET ibStyle  ENABLE
  SHOW GET m.Style  ENABLE
ELSE
  llExit=.T.
  SHOW GETS DISABLE
  SHOW GET m.cSheet ENABLE
ENDIF
SHOW GET pbCancel1 ENABLE

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

IF llBrowse OR !EMPTY(m.Style) 
  SELECT STYLE 
   llBrowse = .F.
  IF llBrowse .OR. !SEEK(m.Style)
    m.Style = gfStyBrw('M',"","",.F.)
    m.Style = SUBSTR(m.Style,1,12)
  ENDIF
  
  IF EMPTY(m.Style)   
    m.Style = SPACE(gnStyleWid)
    m.Color = SPACE(gnColorWid)
    _CUROBJ = OBJNUM(m.Style)          
    SHOW GET m.Color DISABLE
    SHOW GET ibColor DISABLE
  ELSE
    SHOW GET m.Color     ENABLE
    SHOW GET ibColor     ENABLE
  ENDIF  
ELSE
  m.Style = SPACE(gnStyleWid)
  m.Color = SPACE(gnColorWid)
  SHOW GET m.Color DISABLE
  SHOW GET ibColor DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function of the color field.
*!*************************************************************
*! Example            :  =lfvColor()
*!*************************************************************
FUNCTION lfvColor

m.Color = IIF(llbrowse,'?',m.Color)
IF llBrowse .OR. !EMPTY(m.Color) 
  SELECT STYLE 
  llBrowse = .F.
  IF llBrowse .OR. !SEEK(LEFT(m.Style,12)+lcSepart+m.Color)    
    m.Color = gfStyBrw('N',m.Style,@m.Color,.F.)
    IF EMPTY(m.Color)
      m.Color = SPACE(6)
      _CUROBJ = OBJNUM(m.Color)
      RETURN
    ENDIF
  ENDIF  
ENDIF

IF !EMPTY(m.Color)
  lcStyDesc = Style.Desc
  lcScale   = 'S'+Style.Scale
  SHOW GET ibStyle DISABLE
  SHOW GET m.Style DISABLE
  SHOW GET m.Color DISABLE
  SHOW GET ibColor DISABLE

  IF llMultiWH
    SHOW GET ibWH        ENABLE
    SHOW GET m.cWareCode ENABLE
  ENDIF
      
  IF llDyeLot .AND. !llMultiWH
    SHOW GET ibDyelot ENABLE
    SHOW GET m.Dyelot ENABLE
  ENDIF
      
  IF !llDyeLot .AND. !llMultiWH
    IF !SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color,lcTmpStock)
      SELECT Style
      SCATTER FIELDS Dyelot,Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,;
                            Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laScat
    ELSE
      SELECT (lcTmpStock)
      SCATTER FIELDS Dyelot,nOrg1,nOrg2,nOrg3,nOrg4,nOrg5,nOrg6,nOrg7,nOrg8,nTotOrg,;
                            nBal1,nBal2,nBal3,nBal4,nBal5,nBal6,nBal7,nBal8,nTotBal TO laScat
    ENDIF
    
    INSERT INTO (lcTmpDyelt) FROM ARRAY laScat    
    SELECT (lcTmpDyelt)
    SCATTER MEMVAR
    ZAP
    
    =SEEK(lcScale,'Scale')
    FOR lnCount = 1 TO  Scale.Cnt
      lcField = 'm.Qty'+ALLTRIM(STR(lnCount))
      SHOW GET &lcField ENABLE
    ENDFOR
    SHOW GET pbOk    ENABLE
  ENDIF
  =lfRefresh1()
ENDIF

*!*************************************************************
*! Name      : lfvWH
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Validation function for the WH field.
*!*************************************************************
*! Example            :  =lfvWH()
*!*************************************************************
FUNCTION lfvWH
PRIVATE lcKey

IF llBrowse .OR. !EMPTY(m.cWareCode)
  lcStyCode = Style.Style
  SELECT StyDye
   IF llBrowse .OR. !SEEK(LEFT(m.Style,12)+lcSepart+m.Color+m.cWareCode+SPACE(10))
    llBrowse = .F.
    lcBrFields = [StyDye.cWareCode:H='Warehouse',warehous.cDesc:H='Description']
    DECLARE laData[2]
    lcKey = lcStyCode
    
    IF ARIABROW('lcKey'+[FOR EMPTY(Dyelot)],"WareHouses",gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"","","cWareCode,cDesc","laData")
      m.cWareCode = laData[1]
      lcWHDesc    = laData[2]                && WareHouse desc.
    ELSE
      m.cWareCode = SPACE(6)
      =gfModalGen('INM42167B00000','DIALOG' )
      CLEAR READ
    ENDIF
  ELSE
    lcWHDesc = warehous.cDesc
  ENDIF  
ENDIF

IF !EMPTY(m.cWareCode)
  SHOW GET ibWH        DISABLE
  SHOW GET m.cWareCode DISABLE

  IF llDyeLot
    SHOW GET ibDyelot ENABLE
    SHOW GET m.Dyelot ENABLE
  ENDIF
      
  IF !llDyeLot
    IF !SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode,lcTmpStock)
      SELECT StyDye
      SCATTER FIELDS Dyelot,Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,;
                            Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laScat
    ELSE
      SELECT (lcTmpStock)
      SCATTER FIELDS Dyelot,nOrg1,nOrg2,nOrg3,nOrg4,nOrg5,nOrg6,nOrg7,nOrg8,nTotOrg,;
                            nBal1,nBal2,nBal3,nBal4,nBal5,nBal6,nBal7,nBal8,nTotBal TO laScat
    ENDIF
    
    INSERT INTO (lcTmpDyelt) FROM ARRAY laScat    
    SELECT (lcTmpDyelt)
    SCATTER MEMVAR
    ZAP

    =SEEK(lcScale,'Scale')
    FOR lnCount = 1 TO  Scale.Cnt
      lcField = 'm.Qty'+ALLTRIM(STR(lnCount))
      SHOW GET &lcField ENABLE
    ENDFOR
    SHOW GET pbOk    ENABLE
  ENDIF
ENDIF

=lfRefresh1()

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Validation finction for the dyelot field.
*!*************************************************************
*! Example            :  =lfvDyelot()
*!*************************************************************
FUNCTION lfvDyelot

SELECT (lcTmpDyelt)
ZAP

SELECT StyDye

IF llBrowse .OR. !EMPTY(m.Dyelot)
  lcStyCod = Style.Style 
  *IF llBrowse .OR. !SEEK(LEFT(m.Style,12)+m.Color+m.cWareCode+m.Dyelot)
  IF llBrowse .OR. !SEEK(lcStyCod+m.cWareCode+m.Dyelot)
    llBrowse   = .F.
    lcBrFields = [Dyelot:H='Dyelot',nBal1:H='Qty1',nBal2:H='Qty2',nBal3:H='Qty3',nBal4:H='Qty4',;
                  nBal5:H='Qty5',nBal6:H='Qty6',nBal7:H='Qty7',nBal8:H='Qty8',nTotBal:H='TotQty']
    DECLARE laData[1]
    =SEEK(lcStyCod+m.cWareCode)
    SCAN REST WHILE Style+cWareCode = lcStyCod+m.cWareCode FOR !EMPTY(Dyelot)
      IF !SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode+StyDye.Dyelot,lcTmpStock)
        SCATTER FIELDS Dyelot,Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,;
                              Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laScat
      ELSE
        SELECT (lcTmpStock)
        SCATTER FIELDS Dyelot,nOrg1,nOrg2,nOrg3,nOrg4,nOrg5,nOrg6,nOrg7,nOrg8,nTotOrg,;
                              nBal1,nBal2,nBal3,nBal4,nBal5,nBal6,nBal7,nBal8,nTotBal TO laScat
      ENDIF

      INSERT INTO (lcTmpDyelt) FROM ARRAY laScat
    ENDSCAN

    SELECT (lcTmpDyelt)
    IF ARIABROW('',"Dyelots",gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,"","","Dyelot","laData")
      m.Dyelot = laData[1]
      SCATTER MEMVAR
    ELSE
      m.Dyelot = SPACE(10)
      =gfModalGen('INM42168B00000','DIALOG' )
      CLEAR READ
    ENDIF  
  ELSE

    IF !SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode+m.Dyelot,lcTmpStock)
      SCATTER FIELDS Dyelot,Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,;
                            Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laScat
    ELSE
      SELECT (lcTmpStock)
      SCATTER FIELDS Dyelot,nOrg1,nOrg2,nOrg3,nOrg4,nOrg5,nOrg6,nOrg7,nOrg8,nTotOrg,;
                            nBal1,nBal2,nBal3,nBal4,nBal5,nBal6,nBal7,nBal8,nTotBal TO laScat
    ENDIF
    
    INSERT INTO (lcTmpDyelt) FROM ARRAY laScat    
    SELECT (lcTmpDyelt)
    SCATTER MEMVAR
  ENDIF  
ENDIF

IF !EMPTY(m.Dyelot)
  SHOW GET ibDyelot DISABLE
  SHOW GET m.Dyelot DISABLE
  =SEEK(lcScale,'Scale')
  FOR lnCount = 1 TO  Scale.Cnt
    lcField = 'm.Qty'+ALLTRIM(STR(lnCount))
    SHOW GET &lcField ENABLE
  ENDFOR
  SHOW GET pbOk    ENABLE
ENDIF

=lfRefresh1()

*!*************************************************************
*! Name      : lfvQty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Valid function for all the edited 8 qtys.
*!*************************************************************
*! Example            :  =lfvQty()
*!*************************************************************
FUNCTION lfvQty
m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
=lfRefresh1()

*!*************************************************************
*! Name      : lfvOk
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the ok pb.
*!*************************************************************
*! Example            :  =lfvOk()
*!*************************************************************
FUNCTION lfvOk

SELECT (lcTmpFile)
IF !llDyeLot
  m.Dyelot = SPACE(10)
ENDIF

IF llNewLine
  APPEND BLANK
  GATHER MEMVAR

  REPLACE nTotOrg  WITH m.nTotBal
  REPLACE nBalance WITH nTotOrg   - TotQty
  llNewLine = .F.
ELSE
  GO lnRecNo IN (lcTmpFile)
  =lfCompute()
ENDIF

SELECT (lcTmpStock)
IF !SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode+m.Dyelot)
  APPEND BLANK
ENDIF

GATHER MEMVAR

REPLACE nBal1   WITH m.nBal1 -  m.Qty1,;
        nBal2   WITH m.nBal2  - m.Qty2,;
        nBal3   WITH m.nBal3  - m.Qty3,;
        nBal4   WITH m.nBal4  - m.Qty4,;
        nBal5   WITH m.nBal5  - m.Qty5,;
        nBal6   WITH m.nBal6  - m.Qty6,;
        nBal7   WITH m.nBal7  - m.Qty7,;
        nBal8   WITH m.nBal8  - m.Qty8,;
        nTotBal WITH m.nTotBal- m.TotQty

CLEAR READ

*!*************************************************************
*! Name      : lfvMod
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation frunction for the modify pb.
*!*************************************************************
*! Example            :  =lfvMod()
*!*************************************************************
FUNCTION lfvMod
PRIVATE lnOldRec
 
SELECT (lcTmpFile)

lnOldRec  = RECNO()
lcPointer = cBatchNo+cSheet+Style+Color+cWareCode+Dyelot 
lnRecNo   = RECNO()
COUNT TO lnTemp

IF lnTemp = 0
  RETURN
ENDIF

SELECT (lcTmpFile)
GO lnRecNo
SCATTER MEMVAR
DECLARE laOldQty[9]
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldQty

SELECT (lcTmpStock)
=SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode+m.Dyelot)
SCATTER MEMVAR
llNewLine = .F.
=SEEK(LEFT(m.Style,12)+lcSepart+m.Color,'Style')
lcStyDesc = Style.Desc               && Style Desc.
lcScale   = 'S'+Style.Scale          && Style Scale.

IF llMultiWH
  =SEEK(m.cWareCode,'warehous')
  lcWHDesc  = warehous.cDesc           && WareHouse desc.
ENDIF

PUSH KEY
ON KEY
DO gcScrDir+"IC\ICEil604.SPX"
POP KEY
CLEAR TYPEAHEAD
SHOW WINDOW (lcBrowTtl)
ACTIVATE WINDOW (lcBrowTtl) TOP
KEYBOARD "{ALT+B}" CLEAR

SELECT (lcTmpFile)
GOTO lnOldRec

*!*************************************************************
*! Name      : lfvDel
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the delete pb.
*!*************************************************************
*! Example            :  =lfvDel()
*!*************************************************************
FUNCTION lfvDel
PRIVATE lnRecNo,lnTemp

SELECT (lcTmpFile)
lnRecNo = RECNO()
COUNT TO lnTemp

IF lnTemp = 0
  RETURN
ENDIF

SELECT (lcTmpFile)
GO lnRecNo
IF gfModalGen('QRM42164B42002','DIALOG') = 1
  SCATTER MEMVAR
  STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
  =lfCompute()
  
  SELECT (lcTmpStock)
  =SEEK(m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode+m.Dyelot)

  REPLACE nBal1   WITH nBal1   - m.Qty1,;
          nBal2   WITH nBal2   - m.Qty2,;
          nBal3   WITH nBal3   - m.Qty3,;
          nBal4   WITH nBal4   - m.Qty4,;
          nBal5   WITH nBal5   - m.Qty5,;
          nBal6   WITH nBal6   - m.Qty6,;
          nBal7   WITH nBal7   - m.Qty7,;
          nBal8   WITH nBal8   - m.Qty8,;
          nTotBal WITH nTotBal - m.TotQty
ENDIF

CLEAR TYPEAHEAD
SHOW WINDOW (lcBrowTtl) REFRESH
*ACTIVATE WINDOW (lcBrowTtl) TOP
*KEYBOARD "{ALT+B}" CLEAR
_CUROBJ = OBJNUM(IbTab)
*!*************************************************************
*! Name      : lfvSave
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : The validation function for the save pb.
*!*************************************************************
*! Example            :  =lfvSave()
*!*************************************************************
FUNCTION lfvSave

*** Naw save header.
SELECT TmpInvH

m.lLock = .F.
IF llNewBatch
  APPEND BLANK
ELSE
  =SEEK(m.cBatchNo)
ENDIF
GATHER MEMVAR

*** Naw save lines.
SELECT TmpInvL
SET ORDER TO 0
BLANK ALL FOR cBatchNo+cSheet+Style+Color+cWareCode+Dyelot = m.cBatchNo
SET ORDER TO TAG TMPINVLB
SELECT (lcTmpFile)
GO TOP
SCAN
  SCATTER MEMVAR
  IF SEEK(SPACE(6),'TmpInvL')
    SELECT TmpInvL
    RECALL
    GATHER MEMVAR
  ELSE
    INSERT INTO TmpInvL FROM MEMVAR
  ENDIF
ENDSCAN

*** Naw save the balances.

SELECT TmpStock
SET ORDER TO 0
BLANK ALL FOR cBatchNo+Style+Color+cWareCode+Dyelot = m.cBatchNo
SET ORDER TO TAG TMPSTOCK

SELECT (lcTmpStock)
SCAN
  SCATTER MEMVAR
  IF SEEK(SPACE(6),'TmpStock')
    SELECT TmpStock
    RECALL
    GATHER MEMVAR
  ELSE
    INSERT INTO TmpStock FROM MEMVAR
  ENDIF
ENDSCAN

lcScrMode = 'V'
=lfShow()

*!*************************************************************
*! Name      : lfCompute
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Compute the qtys from the very begining to avoid mis
*:             calculating the balnaces.
*!*************************************************************
*! Example            :  =lfCompute()
*!*************************************************************
FUNCTION lfCompute
PRIVATE llFirstLin,lnRecNo

SELECT (lcTmpFile)
llFirstLin = .T.
lnRecNo    = RECNO()

SCAN REST WHILE cBatchNo+Style+Color+cWareCode+Dyelot =;
              m.cBatchNo+PADR(m.Style,19,' ')+m.Color+m.cWareCode+m.Dyelot
  IF llFirstLin
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldQtys
    
    REPLACE Qty1    WITH m.Qty1,;
            Qty2    WITH m.Qty2,;
            Qty3    WITH m.Qty3,;
            Qty4    WITH m.Qty4,;
            Qty5    WITH m.Qty5,;
            Qty6    WITH m.Qty6,;
            Qty7    WITH m.Qty7,;
            Qty8    WITH m.Qty8,;
            TotQty  WITH m.TotQty
    REPLACE nBalance WITH nTotOrg - TotQty
    lnNewBal   = nBalance
    llFirstLin = .F.
  ELSE
    REPLACE nTotOrg  WITH lnNewBal,;
            nBalance WITH nTotOrg - TotQty
    lnNewBal   = nBalance
  ENDIF
ENDSCAN

IF m.TotQty = 0
  GO lnRecNo
  DELETE
ENDIF

m.Qty1   = m.Qty1   - laOldQtys[1]
m.Qty2   = m.Qty2   - laOldQtys[2]
m.Qty3   = m.Qty3   - laOldQtys[3]
m.Qty4   = m.Qty4   - laOldQtys[4]
m.Qty5   = m.Qty5   - laOldQtys[5]
m.Qty6   = m.Qty6   - laOldQtys[6]
m.Qty7   = m.Qty7   - laOldQtys[7]
m.Qty8   = m.Qty8   - laOldQtys[8]
m.TotQty = m.TotQty - laOldQtys[9]

*!*************************************************************
*! Name      : lfvClose
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Validation function for the main window close pb.
*!*************************************************************
*! Example            :  =lfvClose()
*!*************************************************************
FUNCTION lfvClose

IF m.lLock
  =SEEK(m.cBatchNo,'TmpInvH')
  SELECT TmpInvH
  REPLACE lLock WITH .F.
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfActBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Browse function to browse the temp file in the screen.
*!*************************************************************
*! Example            :  =lfActBrow()
*!*************************************************************
FUNCTION lfActBrow

SELECT (lcTmpFile)
GO TOP
lcBrfields = "cSheet :H= 'Sheet#' :R," +;
             "lcStyWd = LEFT(Style,12) :R:H='Style',Color :R:H='Color',"+IIF(llDyeLot,"Dyelot :R:H='Dyelot':4,","")+"nTotOrg :R:H='Org Qty',"+;
             "TotQty :R:H='Adj Qty',nBalance :R:H='Balance'"

BROWSE FIELDS &lcBrfields  ;
      NOAPPEND NODELETE    ;
      NOWAIT NOCLEAR  SAVE ;
      TITLE lcBrowTtl      ;
      FONT "System", 10    ;
      WINDOW ICEil602 IN WINDOW (gcHost)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : When deactivating the read.
*!*************************************************************
*! Example            :  =lfReadDeact()
*!*************************************************************
FUNCTION lfReadDeact

IF UPPER(WONTOP()) = UPPER(lcBrowTtl)
  ON KEY LABEL TAB        DO lpTab     WITH 'ICEil61', 'pbNew'
  ON KEY LABEL BACKTAB    DO lpTab     WITH 'ICEil603', 'pbClose'
  ON KEY LABEL CTRL+ENTER DO lpTrapKey WITH 'ICEil603', 'pbClose'
  ON KEY LABEL ESC        DO lpTrapKey WITH 'ICEil603', 'pbClose'
  ON KEY LABEL ALT+C      DO lpTrapKey WITH 'ICEil603', 'pbClose'
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  
  IF m.cStatus = 'O' .AND. m.lLock
    ON KEY LABEL ALT+I      DO lpChTrapKey WITH '=lfvIns()'
    ON KEY LABEL ALT+D      DO lpChTrapKey WITH '=lfvDel()'
    ON KEY LABEL ALT+M      DO lpChTrapKey WITH '=lfvMod()'
    ON KEY LABEL INS        DO lpChTrapKey WITH '=lfvIns()'
    ON KEY LABEL DEL        DO lpChTrapKey WITH '=lfvDel()'
    ON KEY LABEL ENTER      DO lpChTrapKey WITH '=lfvMod()'
    ON KEY LABEL ALT+S      DO lpChTrapKey WITH '=lfvSave()'
  ENDIF
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : When activating the read.
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=gfClearKey()
ON KEY LABEL ALT+B SHOW WINDOW (lcBrowTtl)

*!*************************************************************
*! Name      : lpTab
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Trap the tab key.
*!*************************************************************
*! Example            :  DO lpTab
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName
ACTIVATE WINDOW (lcWindNAme) TOP
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Activate a window and current object = the passed object.
*!*************************************************************
*! Example            :  DO lpTrapKey
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName
ON KEY
ACTIVATE WINDOW (lcWindName) TOP
_CUROBJ = OBJNUM(&lcObjName)

KEYBOARD CHR(13) CLEAR

*!*************************************************************
*! Name      : lpChTrapKey
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : On key label ?? run a function (Trap some keys).
*!*************************************************************
*! Example            :  DO lpChTrapKey
*!*************************************************************
PROCEDURE lpChTrapKey
PARAMETERS lcFuncName
&lcFuncName

*!*************************************************************
*! Name      : lfSequance
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Get the batch sequance number.
*!*************************************************************
*! Example            :  =lfSequance
*!*************************************************************
FUNCTION lfSequance
DECLARE laMax[1]

SELECT MAX(cBatchNo) FROM TmpInvH INTO ARRAY laMax
IF _TALLY = 0
  laMax[1] = '000000'
ENDIF
RETURN PADL(ALLTRIM(STR(VAL(laMax[1])+1,6)),6,'0')


*!*************************************************************
*! Name      : lfSv2Mast
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/23/1999
*! Purpose   : Post to the master files.
*!*************************************************************
*! Example            :  =lfSv2Mast()
*!*************************************************************
*!B802929,1 (Begin) Remark this function and make another one
*FUNCTION lfSv2Mast
FUNCTION lfSv2Mast1
*!B802929,1 (End)

SELECT TmpStock
=SEEK(TmpInvH.cBatchNo)
DECLARE laNewStk[9]
laNewStk = 0
lcStyKey='*'
llNewSty = .F.  
*** Zero the old stock
SCAN REST WHILE cBatchNo+Style+Color+cWareCode+Dyelot = m.cBatchNo
  IF cBatchNo+Style+Color+cWareCode <> m.cBatchNo+lcStyKey
    lcStyKey=Style+Color+cWareCode
    llNewSty = .T.
  ELSE
    llNewSty = .F.  
  ENDIF
  IF llDyeLot
     IF SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode,'StyDye')
      SELECT StyDye
      SCAN REST WHILE Style+cWareCode+Dyelot = ;
                SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode;      
                FOR !EMPTY(StyDye.Dyelot)
                
        GATHER FROM laNewStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk
      ENDSCAN
    ENDIF
  ENDIF

  IF llMultiWH
    SELECT StyDye
    =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode+SPACE(10))
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldWStk
    lnOldWStk = TotStk
    GATHER FROM laNewStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk
  ENDIF
  
  SELECT Style
  =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color)
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldSStk
  lnOldSStk = TotStk  
  GATHER FROM laNewStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk
  *** Update the style inv journal file.
  IF llMultiWH
    DO gpUdtSJl WITH '2',LEFT(TmpStock.Style,12)+lcSepart+TmpStock.Color,;
                     TmpStock.cWareCode,DATE(),' ',StyDye.Ave_Cost,;
                     laOldWStk[1],laOldWStk[2],laOldWStk[3],laOldWStk[4],;
                     laOldWStk[5],laOldWStk[6],laOldWStk[7],laOldWStk[8],;
                     laOldWStk[9],.T.,"StyInvJL",;
                     laOldWStk[1],laOldWStk[2],laOldWStk[3],laOldWStk[4],laOldWStk[5],laOldWStk[6],laOldWStk[7],laOldWStk[8],;
                     laOldWStk[9],StyDye.Ave_Cost
    IF llNewSty
      DO gpUdtSJl WITH '2',LEFT(TmpStock.Style,12)+lcSepart+TmpStock.Color,;
                       TmpStock.cWareCode,DATE(),' ',StyDye.Ave_Cost,;
                       0,0,0,0,0,0,0,0,0,.T.,"StyInvJL",;
                       laOldWStk[1],laOldWStk[2],laOldWStk[3],laOldWStk[4],laOldWStk[5],laOldWStk[6],laOldWStk[7],laOldWStk[8],;
                       laOldWStk[9],StyDye.Ave_Cost
    ENDIF
  ELSE
    DO gpUdtSJl WITH '2',LEFT(TmpStock.Style,12)+lcSepart+TmpStock.Color,;
                     TmpStock.cWareCode,DATE(),' ',Style.Ave_Cost,;
                     laOldSStk[1],laOldSStk[2],laOldSStk[3],laOldSStk[4],laOldSStk[5],laOldSStk[6],laOldSStk[7],laOldSStk[8],;
                     laOldSStk[9],.T.,"StyInvJL",;
                     laOldSStk[1],laOldSStk[2],laOldSStk[3],laOldSStk[4],laOldSStk[5],laOldSStk[6],laOldSStk[7],laOldSStk[8],;
                     laOldSStk[9],Style.Ave_Cost
    IF llNewSty
      DO gpUdtSJl WITH '2',LEFT(TmpStock.Style,12)+lcSepart+TmpStock.Color,;
                       TmpStock.cWareCode,DATE(),' ',Style.Ave_Cost,;
                       0,0,0,0,0,0,0,0,0,.T.,"StyInvJL",;
                       laOldSStk[1],laOldSStk[2],laOldSStk[3],laOldSStk[4],laOldSStk[5],laOldSStk[6],laOldSStk[7],laOldSStk[8],;
                       laOldSStk[9],Style.Ave_Cost
    ENDIF
  ENDIF
  IF llGlLink 
    *-- 1) Inventory control.                
    *-- Category key for "Inventory Control"........=> '006'.
    DO GLDist WITH IIF(llMultiWH,StyDye.Gl_Link,Style.Link_Code),'006',;
    -1*IIF(llMultiWH,lnOldWStk*StyDye.Ave_Cost,lnOldSStk*Style.Ave_Cost),;  
    'IP', '',DATE(), lcGlFYear, lcGlPeriod, '&tmpGlDist'

    *-- 2) Inventory adjustment.           
    *-- Category key for "Inventory Adjustment......=> '007'.
    DO GLDIST WITH IIF(llMultiWH,StyDye.Gl_Link,Style.Link_Code), '007',;
    IIF(llMultiWH,lnOldWStk*StyDye.Ave_Cost,lnOldSStk*Style.Ave_Cost),;    
    'IP', '',DATE(), lcGlFYear, lcGlPeriod, '&tmpGlDist'
  ENDIF  
ENDSCAN

SELECT TmpStock
=SEEK(TmpInvH.cBatchNo)
SCAN REST WHILE cBatchNo+Style+Color+cWareCode+Dyelot = m.cBatchNo
  SELECT TMPINVL
  =SEEK(TmpInvH.cBatchNo)
  SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty ;
  REST WHILE cBatchNo+cSheet+Style+Color+cWareCode+Dyelot = TmpInvH.cBatchNo;
       FOR Style+Color+cWareCode+Dyelot =;
            TmpStock.Style+TmpStock.Color+TmpStock.cWareCode+TmpStock.Dyelot ;
            TO ARRAY laNewStk
  
  IF llDyeLot
    SELECT StyDye
    =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode+TmpStock.Dyelot)
    GATHER FROM laNewStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk
  ENDIF

  IF llMultiWH
    SELECT StyDye
    =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode+SPACE(10))
    
    REPLACE Stk1   WITH Stk1   + laNewStk[1],;
            Stk2   WITH Stk2   + laNewStk[2],;
            Stk3   WITH Stk3   + laNewStk[3],;
            Stk4   WITH Stk4   + laNewStk[4],;
            Stk5   WITH Stk5   + laNewStk[5],;
            Stk6   WITH Stk6   + laNewStk[6],;
            Stk7   WITH Stk7   + laNewStk[7],;
            Stk8   WITH Stk8   + laNewStk[8],;
            TotStk WITH TotStk + laNewStk[9]
  ENDIF
  
  SELECT Style
  =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color)
  REPLACE Stk1   WITH Stk1   + laNewStk[1],;
          Stk2   WITH Stk2   + laNewStk[2],;
          Stk3   WITH Stk3   + laNewStk[3],;
          Stk4   WITH Stk4   + laNewStk[4],;
          Stk5   WITH Stk5   + laNewStk[5],;
          Stk6   WITH Stk6   + laNewStk[6],;
          Stk7   WITH Stk7   + laNewStk[7],;
          Stk8   WITH Stk8   + laNewStk[8],;
          TotStk WITH TotStk + laNewStk[9]

  *** Update the style inv journal file.
  DO gpUdtSJl WITH '2',LEFT(TmpStock.Style,12)+lcSepart+TmpStock.Color,;
                    TmpStock.cWareCode,DATE(),' ',;
                    IIF(llMultiWH,StyDye.Ave_Cost,Style.Ave_Cost),;
                    laNewStk[1],laNewStk[2],laNewStk[3],laNewStk[4],;
                    laNewStk[5],laNewStk[6],laNewStk[7],laNewStk[8],;
                    laNewStk[9],.T.,"StyInvJL",;
                    0,0,0,0,0,0,0,0,0,IIF(llMultiWH,StyDye.Ave_Cost,Style.Ave_Cost)
  IF llGlLink 
    *-- 1) Inventory control.                
    *-- Category key for "Inventory Control"........=> '006'.
    DO GLDist WITH IIF(llMultiWH,StyDye.Gl_Link,Style.Link_Code),'006',;
    laNewStk[9]*IIF(llMultiWH,StyDye.Ave_Cost,Style.Ave_Cost),;  
    'IP', '',DATE(), lcGlFYear, lcGlPeriod, '&tmpGlDist'

    *-- 2) Inventory adjustment.           
    *-- Category key for "Inventory Adjustment......=> '007'.
     DO GLDIST WITH IIF(llMultiWH,StyDye.Gl_Link,Style.Link_Code), '007',;
     -laNewStk[9]*IIF(llMultiWH,StyDye.Ave_Cost,Style.Ave_Cost),;
     'IP', '',DATE(), lcGlFYear, lcGlPeriod, '&tmpGlDist'
   ENDIF  
ENDSCAN

*-- Update distripution master file
IF llGlLink 
  SELECT (tmpGlDist)
  *-- If temp. file not empty
  IF RECCOUNT() <> 0
    *-- Generate a unique session number.
    lcGlSess = gfSEQUENCE('GLSESSION')
    REPLACE ALL GLSESSION WITH lcGlSess
    USE
    SELECT GLDIST  
    APPE FROM (gcWorkDir+tmpGlDist)
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfSv2Mast
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 01/02/2000
*! Purpose   : Post to the master files.
*!*************************************************************
*! Example            :  =lfSv2Mast()
*!*************************************************************
*!B802929,1
FUNCTION lfSv2Mast
*--
SELECT TmpStock
=SEEK(TmpInvH.cBatchNo)
*--This array will hold the new stk for the current record being updated.
DECLARE laNewStk[9]
laNewStk = 0
DO WHILE cBatchNo+Style+Color+cWareCode+Dyelot  = m.cBatchNo
  lcStyClrW = Style+Color+cWareCode
  SELECT STYDYE
  =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode)
  SCAN REST WHILE Style+cWareCode+Dyelot = SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color+TmpStock.cWareCode;
            FOR !EMPTY(StyDye.Dyelot)
    *--Initialize laNewStk to zero out the other dyelots.
    laNewStk = 0
    SELECT TMPSTOCK
    IF SEEK(m.cBatchNo+lcStyClrW+STYDYE.DyeLot)
      SELECT TMPINVL
      =SEEK(TmpInvH.cBatchNo)
      SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty ;
          REST WHILE cBatchNo+cSheet+Style+Color+cWareCode+Dyelot = TmpInvH.cBatchNo;
          FOR Style+Color+cWareCode+Dyelot =;
              TmpStock.Style+TmpStock.Color+TmpStock.cWareCode+TmpStock.Dyelot ;
          TO ARRAY laNewStk
    ENDIF
    *--Get the link code
    =SEEK(SUBSTR(TmpStock.Style,1,12)+lcSepart+TmpStock.Color,'STYLE')
    IF llGlLink
      laGLDistAr[1,1] = IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF'))
      laGLDistAr[2,1] = IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF'))
    ENDIF
    *--Get the key.
    SELECT STYDYE
    lcDyeKey = EVAL(KEY())
    *-- gfStyCrl Parameters are:
    *-- lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn;
    *-- lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,lcLastRSess,lcAdjRef,laLockInfo
    =gfStyCrl('2',Stydye.Style,Stydye.cWareCode,Stydye.Dyelot,gdSysDate,'',@laNewStk,0,'',.T.,'',0,'','',@laGLDistAr,0,'','')
    SELECT STYDYE
    =SEEK(lcDyeKey)
  ENDSCAN
  SELECT TmpStock  
  =SEEK(m.cBatchNo+lcStyClrW)
  SCAN REST WHILE cBatchNo+Style+Color+cWareCode+Dyelot  = m.cBatchNo+lcStyClrW
  ENDSCAN
ENDDO
*-- Update distripution master file
IF llGlLink 
  SELECT (tmpGlDist)
  *-- If temp. file not empty
  IF RECCOUNT() <> 0
    *-- Generate a unique session number.
    lcGlSess = gfSEQUENCE('GLSESSION')
    REPLACE ALL GLSESSION WITH lcGlSess
    USE
    SELECT GLDIST  
    APPEND FROM (gcWorkDir+tmpGlDist)
    ERASE (gcWorkDir+tmpGlDist+'.DBF')    
  ENDIF
ENDIF
