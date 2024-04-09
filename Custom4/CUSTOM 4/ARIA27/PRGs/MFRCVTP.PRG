*:************************************************************************
*:        File: MFRCVTP.PRG                                             :*
*:      System: ARIA APPAREL SYSTEM 2.7                                 :*
*:     Modules: MF,PO                                                   :*
*:     Program: Temporary Receiving.                                    :*
*:      Author: Timour Abdalla Khalil                                   :*
*:        Date: 09/22/97                                                :* 
*:************************************************************************
*E300935,4  MAB 08/25/1998 Add code to deal with dyelots in P/O case 
*E301077,72 MAB 03/09/1999 Update Odded fields using gfAdd_Info function.
*B602828,1  SSE 04/29/1999 adjusting QUANTITY SCREEN to reflect the correct
*                          figures of Both (Budget / Prev Rec) which was 
*                         added by WAM in the screen     
*B602790,1 MAB 05/04/1999 1- Do Accept empty dyelot if style use dyelot.
*B602790,1                2- Pass parameter to message No XXX was done,Cannot update.
*E301220,1 HDM 05/12/1999 We can make temporary receiving by shipment
*B603215,1 AMM 10/19/1999 1- Fix the bug of miscalculation of Original quantity
*B603215,1 AMM            2- Fix the bug of Variable 'CUTTKT' not found
*B802759,1 RAMY 11/10/1999 1-Change the title of the Line Qty screen to 
*B802759,1                   preview the style and the dyelot
*B802759,1                 2-The selected transaction# (CT\PO) is now carried
*B802759,1                   over to the next lines automatically until the 
*B802759,1                   user change it.
*B802759,1                 3-Optimize the Other quality styles browse.
*B603317,1 MAB 12/05/1999 Optimize speed .
*B101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*E301395,1 SSH 09/04/00 Get the last cuttkt as default in case of recieve by carton.
*E500331,1 ABD 06/19/2000 Add more feature to menu option [Add To ann Existing batch].
*E301480,1 NAD 10/17/2000 Enhancment to temporary receive the Inter-Loc. PO.  
*C200152,1 AMH 02/19/2001 Add an option menu popup and tool bar in case
*C200152,1                of inter-location PO batch. In order to create
*C200152,1                new batches, add to existing or add to approved.
*B604344,1 KHM 04/09/2001 Fix the bug of record is out of range.
*C200170,1 AMH 04/08/2001 Issue Inter-Location P/O Batch.
*B604677,1 MHM 07/19/2001 Fix the bug of receving damages styles when having
*B604677,1                conflict scale between the damage and original styles              
*B604646,1 MHM 07/24/2001 Fix the bug not exceed original QTY in case of temperary 
*B604646,1                Inter-Location Purchase Order .   
*B604298,1 AMH 10/28/2001 Fix the bug of adding lines to posted patch.
*B605762,1 KHM 03/28/2002 Saving the carton and dyelot fields without spaces.
*C128583,1 TMI 08/23/2005 Do not show the Asking message when Saving the batch for PAN21
*:************************************************************************
*E301480,1 NAD (Start) Add Parameter to the program to handle the temp inter location po batch
*-- lcType = "P" : Purchase Order
*-- lcType = "N" : Inter-Location Purchase Order
*-- lcType = " " : Cut Tickets
PARAMETERS lcType
*E301480,1 NAD (End)

lcMPara = IIF(gcWinAppl='MF','M','I')

*E301480,1 NAD (Start) Assign value to the new parameter
PRIVATE lcTypBat    && New variable to hold 'N' in case of the Inter Location P/O Batch.
lcType = IIF(lcMPara = "I",IIF(TYPE('lcType')$'UL','P',lcType),' ')
lcTypBat=IIF(lcType='N','N',lcMPara)
*E301480,1 NAD (End)

STORE ''  TO lcWinCh0,lcWinCh1,lcWinCh2,lcTmpLine,lcTemLoc,;
             lcStatus,lcOldValue,lcBlFields,lcBrFields,lcScFields,;
             lcStyHdr,lcStyPict,lcCartTl,lcStyDesc
STORE 0   TO lnstylewid,lnMjrWid,lnWare,lnBrRecNo,lnLastLine,lnLineNo

*STORE .F. TO llDyelot,llWareHous,llWareLoc,llBrowse,llCUpdate,llFromBtch,llLoc 
STORE .F. TO llDyelot,llWareHous,llWareLoc,llBrowse,llCUpdate,llFromBtch,llLoc ,llGenOrNum

STORE .T. TO llAlowNew
STORE ''  TO lcCarton,lcCuttkt,lcStyle,lcWareCode,lcRefer,lcDyelot
llEdtLCost= .F.

*-- E301220,1  HDM Define Shipment No. variable[start]
*laData[9] = ''
*-- E301220,1  HDM[end]

lcMasterH = IIF(lcMPara = 'M','CutTktH','POSHDR')
lcMasterL = IIF(lcMPara = 'M','CutTktL','POSLN')

*B802759,1 RAMY Add new variable used to store the record No and 
*B802759,1 RAMY the current cut tkt [start]
lnLastRec = 0
lcOldTkt  = ''

*B802759,1 RAMY [end]
*--Screen Arrays.
DIMENSION laStatus[1],laWare[1],laSource[1],laTarget[1]
STORE ' ' TO laStatus,laWare,laSource,laTarget

IF !gfSetup()
  RETURN
ENDIF

*C200152,1 AMH define lcTmpPosln cursor name to POSLN rec. will updated as hold for ELS [start]
IF lcType = "N"
  IF ASCAN(laEvntTrig , PADR('DELINE',10)) <> 0
    lcTmpPosln = gfTempName()
  ENDIF
ENDIF
*C200152,1 AMH [End]

*E300935,4 lnPOLine :variable to determine it's the same line [Hold posln.lineno].
lnPOLine = 0

lcBrDtTtl = 'Receiving Lines'
lcTktTl   = IIF(lcMPara='M','Cuttkt','P/O # ')

*lcMesgTl  = IIF(lcMPara='M','Cutting ticket','Purchase order') 
lcMesgTl  = IIF(lcMPara='M','Cutting ticket',IIF(lcType='P','Purchase order','Inter-Location PO')) 
lcWindTitl= lcMesgTl + ' temporary receiving'
lcFile_ttl= lcWindTitl+' numbers'

*--Open needed P/O files.
IF lcMPara='I'
  llPOhdrOpn=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
  llPOLinOpn=gfOpenFile(gcDataDir+'POSLN','POSLN','SH')

*E301077,72 Open Cut Ticket files [Begin]
ELSE  && else run from manufacturing module.
  =gfOpenFile(gcDataDir+'CutTktH','CutTktH','SH')
  =gfOpenFile(gcDataDir+'CutTktL','CutTktL','SH')
*E301077,72 Open Cut Ticket files [End  ]

ENDIF

EXTERNAL ARRAY laDefProc
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)

*-- Define the variable of the system
lcCancel  = gcBmpHome + "trash.bmp"
lcUncan   = gcBmpHome + "untrash.bmp"
lcPromp   = lcCancel
lcDelMesag= 'cancel'
lcMenProm  = PROPER(lcDelMesag)+' batch'
DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.

IF !WEXIST(gcBaseWind)
  
  lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]

  *-- E301220,1  HDM add shipno Field to screen fields [Start]
  *lcScFields = "Tmprcvnum,cDesc,cStatus,Carton,nTotStk,nTotDam,nTotCan,dDate"
  *C200152,4 AMH Add Source location field for inter location Po [Start]
  *lcScFields = "Tmprcvnum,cDesc,cStatus,Carton,nTotStk,nTotDam,nTotCan,dDate,ShipNo"
  IF lcType = "N"
    lcScFields = "Tmprcvnum,cDesc,cStatus,Carton,nTotStk,nTotDam,nTotCan,dDate,ShipNo,Vendor"
  ELSE
    lcScFields = "Tmprcvnum,cDesc,cStatus,Carton,nTotStk,nTotDam,nTotCan,dDate,ShipNo"
  ENDIF
  *C200152,4 AMH [End]
  *-- E301220,1  HDM [End]
  *--Program screen windows.
  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  
  *--Program variables.
  lcStyHdr   = gfItemMask('HI')
  lcStyPict  = gfItemMask('PI')
  lnstylewid = LEN(lcStyPict)
  lnMjrWid   = LEN(gfItemMask('PM'))
  lcCartTl   = 'Carton'


  *E301077,72 Call gfGetMemVar once for all variables. [Begin]
  *B101704,1 [Start]
  *DIMENSION laSetups[3,2]
  DIMENSION laSetups[4,2]
  *B101704,1 [End]
  laSetups[1,1] = 'M_WareHouse'
  laSetups[2,1] = 'M_WareLoc'
  laSetups[3,1] = 'M_Dyelot'
  *B101704,1 [Start]
  laSetups[4,1] = 'M_GenStOrN'
  *B101704,1 [End]
  =gfGetMemVar(@laSetups,gcAct_Comp)
  *B101704,1 [Start]
  llGenOrNum = (UPPER(ALLTRIM(laSetups[4,2])) ='Y')
  *B101704,1 [End]
  *E301077,72 llWareHous = gfGetMemVar('M_WareHouse')='Y'
  *E301077,72 llWareLoc  =  gfGetMemVar('M_WareLoc')  ='Y'
  llWareHous = (laSetups[1,2]='Y')
  llWareLoc  = (laSetups[2,2]='Y')
  

  *E300935,4 Change the following line to add dyelots for imported styles. [begin]
  *E300935,4 llDyelot   = ( gfGetMemVar('M_Dyelot') ='Y' AND lcMPara='M' )
  *E301077,72 llDyelot   = ( gfGetMemVar('M_Dyelot') = 'Y' )
  llDyelot   = (laSetups[3,2]='Y')

  *E300935,4 Change the following line to add dyelots for imported styles. [end]

  *E301077,72 Call gfGetMemVar once for all variables. [End  ]

  *--Fill warehouse array.
  IF llWareHous  
    SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare
  ELSE
    GO TOP IN WAREHOUS
    laWare[1] = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
    lcWareCode= WAREHOUS.cWareCode
  ENDIF

  *--Temp file.
  lcTmpLine= gfTempName()
  SELECT CTKTRCVL
  SET RELA TO Style INTO Style
  =AFIELDS(laFStru)
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+4,4]
  laFStru[lnFStru+1,1] = 'TOTSTK'
  laFStru[lnFStru+2,1] = 'TOTDAM'
  laFStru[lnFStru+3,1] = 'TOTCAN'  
  laFStru[lnFStru+4,1] = 'TOTBAL'
  STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],laFStru[lnFStru+3,2],laFStru[lnFStru+4,2]
  STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],laFStru[lnFStru+3,3],laFStru[lnFStru+4,3]
  STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]
  CREATE DBF (gcWorkDir+lcTmpLine) FROM ARRAY laFStru

  *E300935,4 Change indecies to include nLineNo because we can add the 
  *E300935,4 same line in the same temp. P/O receive.
  *INDEX ON TranCd+cCarton+Cuttkt+Style+Dyelot+STR(LineNo,6) TAG TmpLine1
  *INDEX ON cCarton+Cuttkt+Style+Dyelot+STR(LineNo,6)+TranCd TAG TmpLine2
  INDEX ON TranCd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) TAG TmpLine1  
  INDEX ON cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+TranCd TAG TmpLine2
  
  
  *E300935,4 add new index on temporary file to get change original quantity
  *E300935,4 when add another line or in view mode.
  INDEX ON Cuttkt+STR(nLineNo,6)+STR(LineNo,6)+TranCd TAG TmpLine3
  *E300935,4
  
  SET ORDER TO TAG TmpLine1

  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    =gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
    =AFIELDS(laStru)
    lcTemLoc  = gfTempName()   
    CREATE CURSOR (lcTemLoc) FROM ARRAY laStru
    INDEX ON STYLE+CWARECODE+CLOCATION TAG (lcTemLoc) OF (gcWorkDir+lcTemLoc)
  ENDIF  

  =lfHedInfo(.F.)

ENDIF

SELECT (lcBaseFile)

*B603317,1 Optimize Speed [Begin]
*SET FILTER TO ctype=lcMPara

*E301480,1 NAD (Start)  use the new variable in the expression.
*SET FILTER TO ctype+tmprcvnum=lcMPara  && Set filter to full key expression.
SET FILTER TO ctype+tmprcvnum=lcTypBat  && Set filter to full key expression.
*E301480,1 NAD (End)

*GO TOP
*E301480,1 NAD (Start)  use the new variable in the expression.
*=SEEK(lcMPara)
=SEEK(lcTypBat)
*E301480,1 NAD (End) 
*B603317,1 Optimize Speed [End  ]

*--Call screen.
PUSH KEY
*ON KEY
*ON KEY LABEL ALT+B  ACTIVATE WINDOW (lcBrDtTtl)
DEFINE BAR 100 OF P01PU01 PROMPT '\<Browse' KEY ALT+B , ''
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrDtTtl)

*C200152,1 AMH [Start] Add option menu popup and tool bar in case of inter
*C200152,1 AMH         locatin PO
IF lcType = 'N'
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Generate Batch Tool' SKIP FOR !laScrMode[1]
  ON SELECTION BAR 1 OF _OPTIONPOP DO lpAddBatch
ENDIF
*C200152,1 AMH [End]

DO (gcScrDir+"MFRCVTP.SPX")
RELEASE WINDOW (lcBrDtTtl)

POP KEY
DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.
RELEASE BAR 100 OF P01PU01

*C200152,1 AMH Release option menu popup and tool bar [Start]
IF lcType = 'N'
  RELEASE BAR 1 OF _OPTIONPOP
  RELEASE POPUP _OPTIONPOP
  RELEASE PAD _Option OF _MSYSMENU
ENDIF
*C200152,1 AMH [End]

SELECT (lcBaseFile)
SET FILTER TO 

IF lcMPara='I'
  IF llPOhdrOpn AND USED('POSHDR')
     USE IN POSHDR
  ENDIF 
  IF llPOLinOpn AND USED('POSLN')
     USE IN POSLN
  ENDIF 
ENDIF

*--Normal exit.
IF glQuitting
  SELECT (lcTmpLine)
  USE
  ERASE (gcWorkDir+lcTmpLine+'.DBF')
  ERASE (gcWorkDir+lcTmpLine+'.CDX')
  IF llWareLoc AND USED(lcTemLoc)
    SELECT (lcTemLoc)
    USE
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF
ENDIF

RETURN
*--End...

*:*************************************************************
*! Name    : lpShow
*! Purpose : Show Screen Mode Changes.
*:*************************************************************
FUNCTION lpShow
DO CASE
  *-- S E L E C T  M O D E.
  CASE laScrMode[1]
    llCUpDate = .F.
    =lfHedInfo(.F.)
    =lfActBrow()    
    =lfLinInfo(.F.)
    *E301480,1 NAD (Start)  use the new variable in the expression.  
    *IF lcMPara = 'I'
    IF lcMPara = 'I' AND lcType='P' 
    *E301480,1 NAD (End)
      SHOW GET laData[9] DISABLE
      SHOW GET ibShp     DISABLE
    ENDIF

    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.
    SHOW GET pbKey    DISABLE
    *B603215,1 AMM Reinitialize variable in select mode
    lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]
    *B603215,1 AMM end
  
  *-- V I E W  M O D E.
  CASE laScrMode[2]
    llCUpDate = .F.
    IF !llFromBtch
      =lfHedInfo(.F.)
      =lfHedInfo(.T.)
    ENDIF
    llFromBtch=.F.
    =lfActBrow()
    =lfLinInfo(.T.)    
    SHOW GET pbKey ENABLE
    IF laData[3] = 'X'
      lcPromp    = lcUnCan
      lcDelMesag = 'uncancel'
    ELSE
      lcPromp   = lcCancel
      lcDelMesag = 'cancel'
    ENDIF
    lcMenProm  = PROPER(lcDelMesag)+' batch'
    *C200152,4 AMH Disable cancel button in inter location PO case [start]
    *DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .F.
    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR ;
    IIF((lcType='N' .AND. (lcDelMesag='uncancel' .OR. laData[3]='I')) .OR. laData[3]='P',.T.,.F.)

    *SHOW GET pbDlt,1 PROMPT lcPromp
    IF lcType = "N" .AND. lcPromp = lcUnCan
      SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
    ELSE
      SHOW GET pbDlt,1 PROMPT lcPromp
    ENDIF
    *C200152,4 AMH [End]
    
    *C200170,1 AMH Add case issued batch of inter-location P/O Batch [Start]
    *IF laData[3] $ 'PX'
    IF laData[3] $ 'PX' .OR. (lcType='N' .AND. laData[3]='I')
      SHOW GET pbEdt DISABLE 
      laCtrStat[7]  =  "DISABLE" 
      *IF laData[3] = 'P'
      IF laData[3] $ 'PI'
        SHOW GET pbDlt DISABLE 
      ENDIF
    ENDIF
    *C200170,1 AMH [End]

    *E301480,1 NAD (Start)  Aded type 'P' for the condition to disable the shipment objects in cas of po batch only  
    *IF lcMPara = 'I'
    IF lcMPara = 'I' AND lcType='P' 
    *E301480,1 NAD (End)
      SHOW GET laData[9] DISABLE
      SHOW GET ibShp     DISABLE
    ENDIF

    SHOW GET pbNew    DISABLE 
    SHOW GET pbRemove DISABLE 
    SELECT (lcTmpLine)

    *B603317,1 Optimize Speed [Begin]
    *GO TOP
    LOCATE
    *B603317,1 Optimize Speed [End  ]

    lnBrRecNo = RECNO()
    SHOW WINDOW (lcBrDtTtl) REFRESH

    
  *-- E D I T  M O D E.
  CASE laScrMode[3]
        *-- E301220,1  HDM Control Shipment field dispay [START]
    *E301480,1 NAD (Start) check for type because we added anew type ('N') for inter location po Batch.
    *IF lcMPara = 'I'
    IF lcMPara = 'I' AND lcType='P' 
    *E301480,1 NAD (End) 
      *llByShp = !EMPTY(CTKTRCVH.SHIPNO)
      SHOW GET laData[9] DISABLE
      SHOW GET ibShp     DISABLE
    ENDIF
    *-- HDM [END]

    DIME laStatus[2]
    laStatus[1] = 'Open'
    laStatus[2] = 'Approved'
    llCUpDate = .T.
    SHOW GET laData[4] DISABLE
    SHOW GET lcCarton  DISABLE
    SHOW GET lcCuttkt  DISABLE
    SHOW GET ibtkt     DISABLE
    SHOW GET lcStyle   DISABLE
    SHOW GET lcDyelot  DISABLE

    *C200152,4 AMH Disable Warehouse in edit mode and new button for approved batch [Start]
    IF lcType = "N"
      SHOW GET lnWare  DISABLE
      IF laDAta[3] = 'A'
        SHOW GET pbNew DISABLE
      ENDIF
    ENDIF
    *C200152,4 AMH [End]

    SHOW GET pbEditQ   ENABLE
    SHOW GET pbKey ENABLE
    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.


  *-- A D D  M O D E.
  CASE laScrMode[4]  
    =lfHedInfo(.F.)
    =lfActBrow()    
    DIME laStatus[1]
    laStatus[1] = 'Open'
    laData[3] = 'O'
    laData[8] = gdSysDate
    lcStatus='Open'
    SHOW GET lcStatus DISABLE
    =lfLinInfo(.F.)
    llCUpDate = .T.
    SHOW GET lcCuttkt ENABLE
    SHOW GET ibtkt    ENABLE
    SHOW GET pbKey    DISABLE
    *-- E301220,1  HDM Control Shipment field dispay [START]
    
    *E301480,1 NAD (Start) check for type because we added anew type ('N') for inter location po Batch.
    *IF lcMPara = 'I'
    IF lcMPara = 'I' AND lcType='P' 
    *E301480,1 NAD (End) 
    
      SHOW GET laData[9] ENABLE
      SHOW GET ibShp      ENABLE
    ENDIF
    *-- HDM [End]
    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.
    =lfRefresh(lcWinCh0)
    SHOW GET laData[4] ENABLE
    _CUROBJ=OBJNUM(laData[4])

ENDCASE
RETURN


*:*************************************************************
*! Name    : lfActBrow
*! Purpose : Activate screen browse.
*:*************************************************************
FUNCTION lfActBrow

lnBrRecNo = RECNO()
*E300935,4 Now MAX Function is on the screen and browse
*lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
*             IIF(laData[4],"cCarton :R :H='Carton',","")+;
*             "Cuttkt    :R :H=lcTktTl :8,"+;
*             "Style     :R :22,"+;
*             IIF(llDyelot,"Dyelot :R :H='Dyelot',","")+;
*             IIF(llWareHous,"cWareCode :R :H='Warehs.':10,","")+;
*             "Reference :R :20,"+;
*             "TotQty :R :H='Original':8,"+;
*             "TotStk :R :H='Stock':8,"+;
*             "TotDam :R :H='Other':8,"+;
*             "TotCan :R :H='Cancel':8,"+;
*             "TotBal :R :H='Balance':8"

lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             IIF(laData[4],"cCarton :R :H='Carton',","")+;
             "Cuttkt    :R :H=lcTktTl :8,"+;
             "Style     :R :22,"+;
             IIF(llDyelot,"Dyelot :R :H='Dyelot',","")+;
             IIF(llWareHous,"cWareCode :R :H='Warehs.':10,","")+;
             "Reference :R :20,"+;
             "nTotQty = MAX(TotQty,0) :R :H='Original':8,"+;
             "TotStk :R :H='Stock':8,"+;
             "TotDam :R :H='Other':8,"+;
             "TotCan :R :H='Cancel':8,"+;
             "nTotBal = MAX(TotBal,0) :R :H='Balance':8"

SELECT (lcTmpLine)
BROWSE FIELDS &lcBlfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       KEY '1'  ;
       TITLE (lcBrDtTtl) ;
       WHEN lfwBrow()    ;
       VALID :F lfvBrow();         
       WINDOW (lcWinCh1) ;
       IN WINDOW (gcBaseWind)         
RETURN

*!*************************************************************
*! Name      : lfvBrow
*! Purpose   : When valid function for browse.
*!*************************************************************
FUNCTION lfwBrow
lnBrRecNo = RECNO()
=lfLinInfo(!EOF())
glFromBrow = .T.
RETURN

*!*************************************************************
*! Name      : lfvBrow
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() 
*!             function
*!*************************************************************
FUNCTION lfVBrow
IF WONTOP() # (lcBrDtTtl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name    : lfOldValue
*! Purpose : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfoldvalue
lcOldValue = EVALUATE(SYS(18))
RETURN

*:*************************************************************
*! Name    : lfTrapKy
*! Purpose : Trap key.
*:*************************************************************
FUNCTION lfTrapKy
IF WONTOP() = lcBrDtTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lptab
  ON KEY LABEL backtab DO lpshifttab
  ON KEY LABEL ENTER   llRun=IIF(!EOF(lcTmpLine) AND !EMPTY(&lcTmpLine..Style),lfvEditQty(),.T.)
ENDIF  
RETURN

*!*************************************************************
*! Name    : lfReadAct
*! Purpose : READ Activate
*!*************************************************************
FUNCTION lfReadAct
IF glFromBrow
  =gfStopBrow()
ENDIF
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
RETURN

*!*************************************************************
*! Name      : lpTab
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
PROCEDURE lptab
IF WONTOP()=lcBrDtTtl
  ACTIVATE WINDOW (lcWinCh2)
  DO CASE
    CASE laScrMode[2]
     _CUROBJ=OBJNUM(pbEditQ)
    CASE laScrMode[3] OR (laScrMode[4] AND !EOF(lcTmpLine))
     _CUROBJ=OBJNUM(pbNew)
    CASE laScrMode[4] AND EOF(lcTmpLine)
     _CUROBJ=IIF(laData[4],OBJNUM(lcCarton),OBJNUM(lcCuttkt))
    OTHE
     _CUROBJ=1
  ENDCASE 
ELSE
   ACTIVATE WINDOW (lcBrDtTtl)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpShiftTab
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpshifttab
IF WONTOP()=lcBrDtTtl
  ACTIVATE WINDOW (lcWinCh0)
  DO CASE
    CASE laScrMode[2]
      _CUROBJ=OBJNUM(pbCls)
    CASE laScrMode[3] OR laScrMode[4]
     _CUROBJ=OBJNUM(laData[8])
  ENDCASE 
ELSE
  ACTIVATE WINDOW (lcBrDtTtl)
ENDIF
RETURN

*!*************************************************************
*! Name    : Get or Clear Header information.
*! Purpose : lfHedInfo
*!*************************************************************
FUNCTION lfHedInfo
PARA llGetInfo
lnAlias = SELECT()
SELECT CTKTRCVH
IF llGetInfo
  SCATTER FIELDS &lcScFields MEMO TO laData
  IF laScrMode[2] AND laData[3] $ 'APX'
    DIME laStatus[3]
    laStatus[1] = 'Approved'
    laStatus[2] = 'Cancelled'
    laStatus[3] = 'Posted'
  ELSE
    DIME laStatus[3]
    laStatus[1] = 'Open'
    laStatus[2] = 'Approved'
    laStatus[3] = 'Cancelled'
  ENDIF
  
  *C200170,1 AMH Add case issued batch of inter-location P/O Batch [Start]
  *lcStatus=IIF(laData[3]='A','Approved',IIF(laData[3]='X','Cancelled',IIF(laData[3]='P','Posted','Open')))
  lcStatus=IIF(laData[3]='A','Approved',IIF(laData[3]='X','Cancelled',IIF(laData[3]='P','Posted',IIF(lcType='N' .AND. laData[3]='I','Issued','Open'))))
  *C200170,1 AMH [End]

  lcKey=' '
  lnLastLine=0
  
  SELECT CTKTRCVL
  *E301480,1 NAD (Start) use the new variable in the expression. 
  *SEEK lcMPara+laData[1]
  SEEK lcTypBat+laData[1]
  *E301480,1 NAD (End) 
  *E300935,4
  *SCAN WHILE cType+TmpRcvNum = lcMPara+laData[1]
  
  *E301480,1 NAD (Start) use the new variable in the expression. 
  *SCAN REST WHILE cType+TmpRcvNum = lcMPara+laData[1]
  SCAN REST WHILE cType+TmpRcvNum = lcTypBat+laData[1]
  *E301480,1 NAD (End) 
  
    lnLastLine = MAX(lnLastLine,Lineno)
    SCATTER MEMVAR
    *E300935,4
    m.Flag = 'S' 

    *IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(laData[4],STR(LineNo,6),'') <> lcKey
    IF Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6) <> lcKey

      *SELECT (lcTmpLine)
      *APPEND BLANK
      *GATHER MEMVAR
      *REPLACE cType  WITH lcMPara,;
      *        TranCd WITH '1',;
      *        Flag   WITH 'S'

      *E300935,4
      lcOldTran = m.Trancd
      m.Trancd  = '1' 
      INSERT INTO (lcTmpLine) FROM MEMVAR
      m.Trancd  = lcOldTran            
      *E300935,4

      *E300935,4
      *lcKey=lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(laData[4],STR(LineNo,6),'')
      lcKey=Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6)
    ENDIF

    *E300935,4
    *SELECT (lcTmpLine)
    *APPEND BLANK
    *GATHER MEMVAR
    *REPLACE Flag WITH 'S'
    *E300935,4

    INSERT INTO (lcTmpLine) FROM MEMVAR
  ENDSCAN

  STORE 0  TO lnTStk,lnTDam,lnTCan
  lcVar1=''
  SELECT CTKTRCVL
  
  *E301480,1 NAD (Start)  use the new variable in the expression. 
  *SEEK lcMPara+laData[1]
  SEEK lcTypBat+laData[1]
  *E301480,1 NAD (End) 
  
  *E300935,4
  *lcKey=cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(laData[4],STR(LineNo,6),'')
  lcKey = Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6)
  
  *E301480,1 NAD (Start) use the new variable in the expression. 
  *SCAN WHILE cType+TmpRcvNum=lcMPara+laData[1]
  SCAN WHILE cType+TmpRcvNum=lcTypBat+laData[1]
  *E301480,1 NAD (End) 
  
    *E300935,4
    *IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(laData[4],STR(LineNo,6),'') <> lcKey
    IF Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6) <> lcKey
      SELECT (lcTmpLine)
      SEEK '1'+lcVar1
      REPLACE TotStk WITH lnTStk,;
              TotDam WITH lnTDam,;
              TotCan WITH lnTCan
      STORE 0 TO lnTStk,lnTDam,lnTCan
      SELECT CTKTRCVL
      
      *E300935,4
      *lcKey=cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(laData[4],STR(LineNo,6),'')
      lcKey=Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6)
    ENDIF
  
    *E300935,4
    *lcVar1=cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')
     lcVar1=cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)

    DO CASE
      CASE TranCd='2'
        lnTStk=lnTStk+TotQty
      CASE (lcMPara='I' AND TranCd='4') OR (lcMPara='M' AND TranCd='3')
        lnTDam=lnTDam+TotQty
      CASE (lcMPara='I' AND TranCd='5') OR (lcMPara='M' AND TranCd='4')
        lnTCan=lnTCan+TotQty
    ENDCASE    
  ENDSCAN
  *--Update last record.
  SELECT (lcTmpLine)
  SEEK '1'+lcVar1
  REPLACE TotStk WITH lnTStk,;
          TotDam WITH lnTDam,;
          TotCan WITH lnTCan


  *--Get originals.
  =lfvGetOpen(.T.)

ELSE
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  DIME laStatus[1]
  lcStatus   = ''
  lnLastLine = 0
  SELECT (lcTmpLine)      
  ZAP

ENDIF
SHOW GET lcStatus
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name    : Get or Clear Line information.
*! Purpose : lfLinInfo
*!*************************************************************
FUNCTION lfLinInfo
PARA llGetInfo

lnAlias = SELECT()
SELECT (lcTmpLine)
IF llGetInfo
  lcCarton  = cCarton
  lcCuttkt  = Cuttkt
  lcStyle   = Style
  lcDyelot  = Dyelot
  lcWareCode= cWareCode
  lcRefer   = Reference
  lnWare    = IIF(llWareHous,ASCAN(laWare,lcWareCode,1),1)
  lcStyDesc = IIF(SEEK(lcStyle,'STYLE'),STYLE.Desc1,'')
  SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
  SHOW GET lcCarton DISABLE
  SHOW GET lcCuttkt DISABLE
  SHOW GET ibtkt    DISABLE
  SHOW GET lcStyle  DISABLE
  SHOW GET lcDyelot DISABLE
  IF laScrMode[2]
    SHOW GET pbNew    DISABLE 
    SHOW GET pbRemove DISABLE
    *C200152,1 AMH Disable Warehouse & reference in view mode [Start]
    SHOW GET lnWare  DISABLE
    SHOW GET lcRefer DISABLE
    *C200152,1 AMH [End]
  ENDIF
  *C200152,4 AMH Disable Warehouse in edit mode [Start]
  IF laScrMode[3] .AND. lcType = "N"
    SHOW GET lnWare  DISABLE
  ENDIF
  *C200152,4 AMH [End]
  *-- E301220,1  HDM Control Shipment field dispay [START]
  IF !EMPTY(laData[9])
    SHOW GET laData[9] DISABLE
    SHOW GET ibShp      DISABLE
    SHOW GET laData[4] DISABLE
  ENDIF
  *-- HDM [END]
  
ELSE
  STORE ''  TO lcCarton,lcCuttkt,lcStyle,lcWareCode,lcRefer,lcDyelot,lcStyDesc  
  STORE 0 TO lnWare,lnLineNo
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
ENDIF

*E301480,1 NAD (Start) check for type because we added anew type ('N') for inter location po Batch.
*IF lcMPara = 'I'
IF lcMPara = 'I' AND lcType='P' 
*E301480,1 NAD (End) 

  SHOW GET laData[9] DISABLE
  SHOW GET ibShp     DISABLE
ENDIF

SHOW WINDOW (lcBrDtTtl) REFRESH
=lfRefresh(lcWinCh2)
SELECT(lnAlias)
RETURN


*:*************************************************************
*! Name    : lfvGetOpen
*! Purpose : Get original open qty.
*:*************************************************************
FUNCTION lfvGetOpen
PARA llAllLines
*--Get originals.
DIME laOpnQty[8]
laOpnQty  =0

lcExp = IIF(lcMPara='M',"Cuttkt+Style+Dyelot=lcKey","cStyType+Po+Style+STR(LineNo,6)=lcKey")

SELECT (lcTmpLine)
IF llAllLines
  SEEK '1'
ELSE
  *B604344,1 KHM 04/09/2001 (Begin) Add the checking if EOF.
  IF EOF()
    LOCATE
  ENDIF
  *B604344,1 KHM 04/09/2001 (End)
  
  lnSvRc=RECNO()
ENDIF

SCAN REST WHILE TranCd = '1'

  *E300935,4 Now imported styles have dyelots
  *lcKey=IIF(lcMPara='M','','P')+Cuttkt+Style+IIF(lcMPara='M',Dyelot,'')
  
  *E301480,1 NAD (Start)  Pass the new parameter to Lckey to handle the case of inter location PO Batch.
  lcKey= IIF(lcMPara='M','','P')+Cuttkt+Style+IIF(lcMPara='M',Dyelot,STR(nLineNo,6))
  lcKey= IIF(lcMPara='M','',lcType)+Cuttkt+Style+IIF(lcMPara='M',Dyelot,STR(nLineNo,6))
  *E301480,1 NAD (End)
  
  IF EMPTY(DYELOT)
    lcKey = ALLTRIM(lcKey)
  ENDIF
  
  SELECT (lcMasterL)
  IF SEEK(lcKey,lcMasterL)
    
    *E300935,4 Change to the following because P/O can have the same line,  
    *E300935,4 and also not to include Shipment lines.
    *E300935,4 Style/dyelot.
    *SCAN REST WHILE &lcExp

    *C200170,1 AMH Add case issued batch of inter-location P/O Batch [Start]
    *SCAN REST WHILE &lcExp FOR IIF(lcMPara='M',.T.,TRANCD <>'3')
    SCAN REST WHILE &lcExp FOR IIF(lcMPara='M',.T.,IIF(lcType='N',!(TRANCD$'36'),TRANCD<>'3'))
      FOR I=1 TO 8
        Z=STR(I,1)
        *E300935,4 Now MAX Function is on the screen.
        *laOpnQty[I]=IIF(TranCd='1',laOpnQty[I]+Qty&Z,MAX(laOpnQty[I]-Qty&Z,0))
        laOpnQty[I]=IIF(TranCd='1',laOpnQty[I]+Qty&Z,laOpnQty[I]-Qty&Z)
      ENDFOR 
    ENDSCAN

    *E300935,4 Evaluate remain Open Quantity if there is another lines.[Begin]
    *E300935,4 But this in case of system support dyelots and Imported styles.
    SELECT (lcTmpLine)

    *B602828,1 Sameh Commented out the next line and its endif line 
    *IF !EMPTY(Dyelot) AND lcMPara='I'
    lcCurDyelot = Dyelot
    lcGoAgain   = TranCd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)
    lcSeeked    = Cuttkt+STR(nLineNo,6)
    SET ORDER TO TAG TmpLine3
    = SEEK(lcSeeked)
      
    *B602828,1
    *SCAN REST WHILE Cuttkt+STR(nLineNo,6)+STR(LineNo,6)+Trancd = lcSeeked;
          FOR (Dyelot # lcCurDyelot) AND (Trancd # '1')
    SCAN REST WHILE Cuttkt+STR(nLineNo,6)+STR(LineNo,6)+Trancd = lcSeeked;
          FOR IIF(lcMPara='M',.T.,(Dyelot # lcCurDyelot)) AND (Trancd # '1')
    *B602828,1
	  *B603215,1 AMM Comment out don't include temporary received quantity
      *FOR I=1 TO 8
        *Z=STR(I,1)
        *E300935,4 Now MAX Function is on the screen.
        *laOpnQty[I] = MAX(laOpnQty[I]-Qty&Z,0)
        *laOpnQty[I] = laOpnQty[I]-Qty&Z
      *ENDFOR 
      *B603215,1 AMM end
    ENDSCAN
    SET ORDER TO TAG TmpLine1
    = SEEK(lcGoAgain)
    *ENDIF
      
    *E300935,4 Evaluate remain Open Quantity if there is another lines.[End]
    
    FOR I=1 TO 8
      Z=STR(I,1)
      REPLACE Qty&Z WITH laOpnQty[I]
    ENDFOR 
    
    *E300935,4 Now MAX Function is on the screen and browse
    *REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
    *        TotBal WITH IIF(TotQty-(TotStk+TotDam+TotCan)<0,0,TotQty-(TotStk+TotDam+TotCan)) 
    REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TotBal WITH TotQty-(TotStk+TotDam+TotCan)
    laOpnQty=0
  ENDIF
  IF !llAllLines
    EXIT
  ENDIF
ENDSCAN

SELECT (lcTmpLine)
IF llAllLines

*B603317,1 Optimize Speed [Begin]
*GO TOP
LOCATE
*B603317,1 Optimize Speed [End  ]

ELSE
  *B604344,1 KHM 04/09/2001 (Begin) Check if the saved pointer is between
  *B604344,1                1 and reccount of file.
  *GOTO lnSvRc
  IF BETWEEN(lnSvRc,1,RECCOUNT())
    GOTO lnSvRc
  ENDIF
  *B604344,1 KHM 04/09/2001 (End)  
ENDIF  
RETURN

*:*************************************************************
*! Name    : lfvBatch
*! Purpose : Validate the batch no.
*:*************************************************************
FUNCTION lfvBatch
IF MDOWN()
  RETURN
ENDIF  

*B603317,1 Add lcHdrFlt variable which used in Save filter [Begin]
*PRIVATE lcBrFields
PRIVATE lcBrFields , lcHdrFlt
*B603317,1 Add lcHdrFlt variable which used in Save filter [Begin]

SELECT CTKTRCVH
*E301480,1 NAD (Start)  use the new variable in the expression. 
*IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK(lcMPara+laData[1],'CTKTRCVH') )
IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK(lcTypBat+laData[1],'CTKTRCVH') )
*E301480,1 NAD (End) 

  DIMENSION laTempData[1]
  STORE '' TO laTempData
  lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]

  *B603317,1 browse key instead of direct browse [Begin]
  *=gfBrows(.F.,'TMPRCVNUM','laTempData','Temp. Receive Batchs')
  lcHdrFlt = FILTER()
  SET FILTER TO
  
  *E301480,1 NAD (Start) Use the new variable in the expression. 
  *=gfBrows([lcMPara],'TMPRCVNUM','laTempData','Temp. Receive Batchs')
  =gfBrows([lcTypBat],'TMPRCVNUM','laTempData','Temp. Receive Batchs')
  *E301480,1 NAD (End)
   
  SET FILTER TO &lcHdrFlt
  *B603317,1 

  laData[1]=laTempData[1]
  llBrowse = .F.
  IF EMPTY(laData[1])
    _CUROBJ = OBJNUM(laData[1])
    RETURN
  ENDIF  

ENDIF

IF EMPTY(laData[1])
  RETURN
ENDIF  

*--Get information.
llFromBtch=.T.
laScrMode=.F.
laScrMode[2]=.T.
=lfHedInfo(.T.)

SHOW GETS  
RETURN


*:*************************************************************
*! Name    : lfvStatus
*! Purpose : Validate the Status.
*:*************************************************************
FUNCTION lfvStatus

IF lcStatus=lcOldValue OR laData[3]='X'
  lcStatus=lcOldValue
  SHOW GET lcStatus
  RETURN
ENDIF
laData[3]=SUBSTR(lcStatus,1,1)
laData[3]=IIF(laData[3]='C','X',laData[3])

IF laData[3]='A'

  *B603317,1 Optimize Speed [Begin]
  *GO TOP IN CtKtRcvL
  lnCtAlias = SELECT(0)
  SELECT CtKtRcvL
  LOCATE
  SELECT (lnCtAlias)
  *B603317,1 Optimize Speed [End  ]

  IF EOF('CtKtRcvL')
    *--No lines found in this temporary receiving batch, Cannot approve.
    = gfModalGen('INM34054B34000','DIALOG')
    lcStatus=lcOldValue
    laData[3]=SUBSTR(lcStatus,1,1)
    laData[3]=IIF(laData[3]='C','X',laData[3])
  ENDIF
ENDIF
RETURN


*:*************************************************************
*! Name    : lfvByCrtn
*! Purpose : Validate by carton selection.
*:*************************************************************
FUNCTION lfvByCrtn

=lfActBrow()
=lfRefresh(lcWinCh2)
IF laData[4]
  SHOW GET lcCarton ENABLE
  SHOW GET lcCuttkt  DISABLE
  SHOW GET ibtkt     DISABLE
ELSE
  SHOW GET lcCarton DISABLE
  SHOW GET lcCuttkt  ENABLE
  SHOW GET ibtkt     ENABLE
ENDIF
RETURN


*:*************************************************************
*! Name    : lfvCarton
*! Purpose : Validate carton.
*:*************************************************************
FUNCTION lfvCarton

IF EMPTY(lcCarton)
  RETURN
ENDIF
*E301395,1 SSH 09/04/00 *** Begin *** Refresh lcCuttkt.
SHOW GET lcCuttkt ENABLE
*E301395,1 SSH 09/04/00 *** End   ***
SELECT (lcTmpLine)
LOCATE FOR EMPTY(STYLE)
IF !FOUND()
  APPEND BLANK
ENDIF

*E301480,1 NAD (Start)  Use the new variable in the expression. 
*REPLACE cType   WITH lcMPara,;
        TmpRcvNum WITH laData[1],;
        cCarton WITH lcCarton,;
        TranCd  WITH 'X',;
        Flag    WITH 'A'
        
REPLACE cType   WITH lcTypBat,;
        TmpRcvNum WITH laData[1],;
        cCarton WITH lcCarton,;
        TranCd  WITH 'X',;
        Flag    WITH 'A'        
*E301480,1 NAD (End) 

lnLastLine = lnLastLine + 1
REPLACE LineNo WITH lnLastLine

SHOW GET laData[4] DISABLE
SHOW GET lcCuttkt  ENABLE
SHOW GET ibtkt     ENABLE
SHOW WINDOW (lcBrDtTtl) REFRESH
_CUROBJ=OBJNUM(lcCuttkt)
RETURN


*:*************************************************************
*! Name    : lfvTkt
*! Purpose : Validate c/t and p/o.
*:*************************************************************
FUNCTION lfvTkt
PARA llShCall
lnAlias = SELECT()
llbrowse = llbrowse OR '?' $ lcCuttkt
*IF llBrowse OR (LASTKEY()=13 AND !MDOWN()) 
IF llBrowse OR !MDOWN() OR llShCall
  SELECT (lcMasterH)
  
  *E301480,1 NAD (Start) Seek with the new parameter to handle the case of inter location PO Batch.
  *IF llBrowse OR (!EMPTY(lcCuttkt) AND !SEEK(IIF(lcMPara='M','','P')+lcCuttkt))
  IF llBrowse OR (!EMPTY(lcCuttkt) AND !SEEK(IIF(lcMPara='M','',lcType)+lcCuttkt))
  *E301480,1 NAD (End)
  
    llbrowse = .F.
    IF lcMPara = 'M'
      = CutBrow(@lcCuttkt,'')
      lcStyle=IIF(!EMPTY(lcCuttkt),'??','')
      *B802759,1 RAMY Store the cut tkt no and refrish the variable 
      *B802759,1 RAMY holds the record no [start]
      lcOldTkt = lcCuttkt
      lnLastRec = 0
      *B802759,1 RAMY [end]
    ELSE
    
    *E301480,1 NAD (Start) Pass the new parameter to The PoSBrow to handle the case of inter location PO Batch.
      *DO POSBrow WITH lcCuttkt,'','P'
      DO POSBrow WITH lcCuttkt,'',lcType
    *E301480,1 NAD (End)
    
    ENDIF
    IF EMPTY(lcCuttkt)
       lcCuttkt = SPACE(1)
       _CUROBJ = OBJNUM(lcCuttkt)
       SELECT(lnAlias)
       RETURN
    ENDIF
  ENDIF
  IF !EMPTY(lcCuttkt)

    IF !llShCall
      IF !lfChkCtkt()
         lcCuttkt = SPACE(1)
         _CUROBJ = OBJNUM(lcCuttkt)
         SELECT(lnAlias)
         RETURN 
      ENDIF 
      
    ELSE
      IF lcCuttkt <> lcPo
        lcPo = lcCuttkt
        IF !lfChkCtkt()
           *lcCuttkt = SPACE(1)
           SELECT(lnAlias)
           RETURN .F.
        ENDIF 
      ELSE
        IF llflag
          RETURN .F.
        ENDIF      
      ENDIF          
    ENDIF
    SELECT (lcTmpLine)
    
    
    IF laData[4]
      LOCATE FOR TranCd = 'X' 
      =RLOCK()
      REPLACE Cuttkt WITH lcCuttkt,;
              TranCd WITH '1' 
      UNLOCK
      SHOW WINDOW (lcBrDtTtl) REFRESH
    ELSE
      APPEND BLANK
      *E301480,1 NAD (Start)  Use the new variable .
      *REPLACE cType  WITH lcMPara,;
              TmpRcvNum WITH laData[1],;
              Cuttkt WITH lcCuttkt,;
              TranCd  WITH '1',;
              Flag    WITH 'A'
      
      
      REPLACE cType  WITH lcTypBat,;
              TmpRcvNum WITH laData[1],;
              Cuttkt WITH lcCuttkt,;
              TranCd  WITH '1',;
              Flag    WITH 'A'
  
      *E301480,1 NAD (End) 
      
      SHOW WINDOW (lcBrDtTtl) REFRESH
    ENDIF
    SHOW GET laData[4] DISABLE
    SHOW GET lcCarton  DISABLE
    SHOW GET lcCuttkt  DISABLE
    SHOW GET ibtkt     DISABLE
    SHOW GET pbNew     ENABLE
    SHOW GET pbRemove  ENABLE
    SHOW GET lcStyle   ENABLE
    *B802759,1 RAMY Store the cut tkt no. and refrish the variable that
    *B802759,1 RAMY holds the record no [start]
    IF lcMPara = 'M' AND lcCuttkt <> lcOldTkt
      lcOldTkt = lcCuttkt
      lnLastRec = 0
    ENDIF
    *B802759,1 RAMY [end]
    *-- E301220,1  HDM Control Shipment field dispay [START]
   
    *E301480,1 NAD (Start) check for type because we added anew type ('N') for inter location po Batch.
    *IF lcMPara = 'I'
    IF lcMPara = 'I' AND lcType='P'
    *E301480,1 NAD (End) 
   
      SHOW GET laData[9] DISABLE
      SHOW GET ibShp      DISABLE

    ENDIF
    *-- HDM [END]
    
    =lfvStyle(llShCall)
    _CUROBJ = OBJNUM(lcStyle)
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN

*:*************************************************************
*! Name    : lfChkCtkt
*! Purpose : C/t or p/o needed checks.
*:*************************************************************
FUNCTION lfChkCtkt
PRIVATE lcPrevOrd

SHOW GET lcCuttkt


*-- E301220,1  HDM Validate the selected Po from selected shipment [Start]
*E301480,1 NAD (Start) Validate the selected Po in case of Po batch only.
*IF !llShCall AND lcMPara = 'I' AND !EMPTY(laData[9])
IF !llShCall AND lcMPara = 'I' AND !EMPTY(laData[9]) AND lcType='P'
*E301480,1 NAD (End)
 
  SELECT (lcMasterL)
  lcPrevOrd = ORDER()
  SET ORDER TO Poslnsh
  IF !SEEK(laData[9] + 'P' + lcCuttkt)
    *--XXX Shipment lines not found, Cannot receive it.
    = gfModalGen('INM34160B34000','DIALOG',lcMesgTl+' Shipment')
    SET ORDER TO TAG &lcPrevOrd
    RETURN .F.
  ENDIF
  SET ORDER TO TAG &lcPrevOrd
ENDIF
*-- E301220,1  HDM [End]
DO CASE
  CASE &lcMasterH..Status = 'S'
    *--XXX status is XXX. Therefore,no receivings can be done.
    = gfModalGen('INM34055B34000','DIALOG',lcMesgTl+'|'+'Closed')
    RETURN .F.
  CASE &lcMasterH..Status = 'B'
    *--XXX status is XXX. Therefore,no receivings can be done.
    = gfModalGen('INM34055B34000','DIALOG',lcMesgTl+'|'+'Bid')
    RETURN .F.
  CASE &lcMasterH..Status = 'H'
    *--XXX status is Hold since a cost sheet has not been created yet. Therefore,no receivings can be done.
    = gfModalGen('INM34056B34000','DIALOG',lcMesgTl)
    RETURN .F.
  CASE &lcMasterH..Status = 'X'
    *--XXX has been canceled. Not allowed to receive.
    = gfModalGen('INM34057B34000','DIALOG',lcMesgTl+'|'+'receive')
    RETURN .F.
  CASE &lcMasterH..Status = 'C'
    *--XXX is completely received. Do you wish to continue ?,\<Yes;\<No
    IF gfModalGen('INM34058B34001','DIALOG',lcMesgTl) = 2
      RETURN .F.
    ENDIF
ENDCASE
SELECT (lcMasterL)
*E301480,1 NAD (Start) seek with the parameter to handle the case of inter location PO Batch.
*IF !llShCall AND !SEEK(IIF(lcMPara='M','','P')+lcCuttkt)
IF !llShCall AND !SEEK(IIF(lcMPara='M','',lcType)+lcCuttkt)
*E301480,1 NAD (End)

  *--XXX lines not found, Cannot receive it.
  = gfModalGen('INM34059B34000','DIALOG',lcMesgTl)
  RETURN .F.
ENDIF
RETURN .T.


*:*************************************************************
*! Name    : lfvStyle
*! Purpose : Validate Style.
*:*************************************************************
FUNCTION lfvStyle
PARAMETER llShpCall
PRIVATE lcPrevOrd
lnAlias = SELECT(0)
llBrowse  = EMPTY(lcStyle)

SELECT (lcMasterL)

*E301480,1 NAD (Start) Seek with the new parameter to handle the case of inter location PO Batch.
*IF !llShpCall AND (llBrowse OR ( !EMPTY(lcStyle) AND !SEEK(IIF(lcMPara='M','','P')+lcCuttkt+lcStyle) ))
IF !llShpCall AND (llBrowse OR ( !EMPTY(lcStyle) AND !SEEK(IIF(lcMPara='M','',lcType)+lcCuttkt+lcStyle) ))
*E301480,1 NAD (End)

  DIME laTempData[1]
  STORE '' TO laTempData
  PRIVATE lcBrFields
  lcBrFields = "Style  :R :H='Style #':25,"+;
               IIF(llDyelot,"Dyelot :R :H='Dyelot' :15 ,","")+;
               "TotQty :R :H='Pieces' :9"
  *E301480,1 NAD (Start) Pass the new parameter to Lckey to handle the case of inter location PO Batch.
  *lcKey = IIF(lcMPara='M','','P')+lcCuttkt
  lcKey = IIF(lcMPara='M','',lcType)+lcCuttkt
  *E301480,1 NAD (End)
 
  SEEK lcKey
  *B802759,1 RAMY Add these lines to skip the record pointer to next 
  *B802759,1 RAMY record if browse in the same cut tkt [start]
  IF lcMPara = 'M' AND lcCuttkt = lcOldTkt
    *GO lnLastRec + 1
    IF lnLastRec <> 0
      GO lnLastRec
      SKIP 1
    ENDIF
  ENDIF
  *B802759,1 RAMY [end]
  =ARIABROW([lcKey FOR Trancd='1'],'Temp. Receive Batchs lines',gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,.F.,'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\!\?\<Ok;;','Style','laTempData')
  *B802759,1 RAMY Store the record no of the selected transaction line [start]
  IF lcMPara = 'M'
    IF lcCutTkt = &lcMasterL..CutTkt
      lnLastRec = RECNO(lcMasterL)
    ELSE 
      lnLastRec = 0
      SKIP -1 IN (lcMasterL)
    ENDIF
  ENDIF

  =lfActBrow()
  lnBrRecNo = RECNO()
  *SHOW WINDOW (lcBrDtTtl) REFRESH
  *B802759,1 RAMY [end]

  lcStyle  =IIF(EMPTY(laTempData[1]),&lcMasterL..Style,laTempData[1])
  lcDyelot =IIF(llDyelot,&lcMasterL..Dyelot,SPACE(10))
  lnPOLine  = &lcMasterL..LineNo
ENDIF

llBrowse = .F.
SHOW GET lcStyle
IF !EMPTY(lcStyle)
  IF !llDyelot
    IF !laData[4]
      llRet=.F.
      SELECT (lcTmpLine)
      lnRecNo=RECNO()
      SET ORDER TO TAG TmpLine2

      *cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+TranCd
      *E300935,4 Change previous block to next one. [begin]
      *IF SEEK(SPACE(3)+lcCuttkt+PADR(lcStyle,19))
      *SPACE(3)+lcCuttkt+PADR(lcStyle,19)+lcDyelot+lnPOLine 
      IF SEEK(SPACE(3)+lcCuttkt+PADR(lcStyle,19)+IIF(lcMPara = 'M','',lcDyelot+STR(lnPOLine,6)))
        llRet=.T.
      ENDIF

      SET ORDER TO TAG TmpLine1  
      GOTO lnRecNo
      IF llRet
        *--This line allready exist in this batch.
        = gfModalGen('INM34060B34000','DIALOG')
        REPLACE Flag WITH 'X'
        DELETE
        lcStyle=SPACE(19)
        SHOW GET lcStyle  DISABLE
        SHOW GET lcCuttkt ENABLE
        SHOW GET ibtkt    ENABLE
        SHOW WINDOW (lcBrDtTtl) REFRESH
        _CUROBJ=OBJNUM(lcCuttkt)
        SELECT(lnAlias)
        RETURN
      ENDIF 
    ENDIF 

    SELECT (lcTmpLine)
    REPLACE Style     WITH lcStyle,;
            cWareCode WITH &lcMasterL..cWareCode,;
            Reference WITH &lcMasterL..Reference,;
            nLineNo   WITH &lcMasterL..LineNo,;
            TotStk    WITH 0,;
            TotDam    WITH 0,;
            TotCan    WITH 0

    *--Get originals.

    *-- E301220,1  HDM re-calculate open quantity [START]
    *=lfvGetOpen(.F.)
    IF llShpCall
      =lfvGtShOpn(.F.)
    ELSE
      =lfvGetOpen(.F.)
    ENDIF
    *-- E301220,1  HDM [End]
    
    =lfLinInfo(.T.)

    SHOW GET lcCuttkt DISABLE
    SHOW GET ibtkt    DISABLE
    SHOW GET lcStyle  DISABLE
    SHOW GET pbNew    ENABLE
    SHOW GET pbRemove ENABLE
    SHOW GET lnWare   ENABLE
    SHOW GET pbEditQ  ENABLE      
    SHOW GET lcRefer  ENABLE
    SHOW GET llLoc    ENABLE
  ELSE
    SELECT (lcTmpLine)
    LOCATE FOR TranCd = '1' AND Cuttkt = lcCuttkt AND TmpRcvNum = laData[1] AND cCarton = lcCarton
    =RLOCK()
    REPLACE Style WITH lcStyle
    UNLOCK

    SHOW GET lcStyle  DISABLE
    IF !EMPTY(lcDyelot)
      SHOW GET lcDyelot ENABLE
      _CUROBJ = OBJNUM(lcDyelot)
    ELSE
      =lfvDyelot()
    ENDIF
  ENDIF

  SELECT (lcTmpLine)
  lnBrRecNo=RECNO()
  SHOW WINDOW (lcBrDtTtl) REFRESH
  SHOW GET pbKey ENABLE

ENDIF
SELECT(lnAlias)
RETURN

*:*************************************************************
*! Name    : lfvDyelot
*! Purpose : Validate Dyelot.
*:*************************************************************
FUNCTION lfvDyelot

*B602790,1 Do Accept empty dyelot if style use dyelot [Begin]
*RAMY
*IF EMPTY(lcDyelot) AND (lcDyelot <> lcOldValue)
IF EMPTY(lcDyelot) AND (lcDyelot <> IIF(VARREAD()=UPPER("lcDyelot"),lcOldValue,&lcTmpLine..Dyelot))
  lcDyelot = lcOldValue
  _CUROBJ = OBJNUM(lcDyelot)
  SHOW GET lcDyelot
  RETURN
ENDIF
*B602790,1 Do Accept empty dyelot if style use dyelot [End  ]

lnAlias = SELECT()
IF !laData[4]
  llRet=.F.
  SELECT (lcTmpLine)
  lnRecNo=RECNO()
  SET ORDER TO TAG TmpLine2

  *E300935,4
  *IF SEEK(SPACE(3)+lcCuttkt+PADR(lcStyle,19)+lcDyelot)
  IF SEEK(SPACE(3)+lcCuttkt+PADR(lcStyle,19)+lcDyelot+STR(lnPOLine,6))
    llRet=.T.
  ENDIF 

  SET ORDER TO TAG TmpLine1  
  GOTO lnRecNo
  IF llRet
    *--This line allready exist in this batch.
    = gfModalGen('INM34060B34000','DIALOG')
    REPLACE Flag WITH 'X'
    DELETE
    lcStyle =SPACE(19)
    lcDyelot=SPACE(10)
    SHOW GET lcDyelot DISABLE
    SHOW GET lcStyle  DISABLE
    SHOW GET lcCuttkt ENABLE
    SHOW GET ibtkt    ENABLE
    SHOW WINDOW (lcBrDtTtl) REFRESH
    _CUROBJ=OBJNUM(lcCuttkt)
    SELECT(lnAlias)
    RETURN
  ENDIF 
ENDIF 

SELECT (lcTmpLine)
=RLOCK()
REPLACE Style     WITH lcStyle,;
        Dyelot    WITH lcDyelot,;
        cWareCode WITH &lcMasterL..cWareCode,;
        nLineNo   WITH &lcMasterL..LineNo,;
        Reference WITH &lcMasterL..Reference,;
        TotStk    WITH 0,;
        TotDam    WITH 0,;
        TotCan    WITH 0
UNLOCK
*--Get originals.
*-- E301220,1  HDM re-calculate open quantity [Start]
*=lfvGetOpen(.F.)
IF TYPE('llShpCall')<> 'U' AND llShpCall
  =lfvGtShOpn(.F.)
ELSE
  =lfvGetOpen(.F.)
ENDIF
*-- E301220,1  HDM [End]

=lfLinInfo(.T.)
SELECT(lnAlias)
RETURN


*:*************************************************************
*! Name    : lfvRefer
*! Purpose : Validate Reference.
*:*************************************************************
FUNCTION lfvRefer
SELECT (lcTmpLine)
lnRecNo=RECNO()
lcKey=cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')
SET ORDER TO TAG TmpLine2
SEEK lcKey
REPLACE REST Reference WITH lcRefer ;
       WHILE cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=lcKey
SET ORDER TO TAG TmpLine1
GOTO lnRecNo
SHOW WINDOW (lcBrDtTtl) REFRESH
RETURN

*:*************************************************************
*! Name    : lfvWare
*! Purpose : Validate warehouse.
*:*************************************************************
FUNCTION lfvWare

IF !llWareHous AND llWareLoc
  STORE 1 TO lnWare,lcOldValue
ENDIF

lnWare=IIF(lnWare=0,1,lnWare)
lcWareCode=SUBSTR(laWare[lnWare],1,6)

IF lnWare<>lcOldValue
  IF !SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
    *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcStyle)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH lcStyle,SPACE(10),lcWareCode
    ELSE
      lnWare=lcOldValue
      SHOW GET lnWare
      RETURN
    ENDIF
  ENDIF

  SELECT (lcTmpLine)
  lnRecNo=RECNO()

  *E300935,4 Change Key expression to update warehous changes.
  *lcKey=cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')
  lcKey= ccarton+cuttkt+style+dyelot+STR(nlineno,6)+STR(lineno,6)

  SET ORDER TO TAG TmpLine2
  SEEK lcKey
  *E300935,4 Change replace command to update warehous changes in temp. file.
  *REPLACE REST cWareCode WITH lcWareCode ;
  *       WHILE cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=lcKey

  REPLACE REST cWareCode WITH lcWareCode ;
         WHILE ccarton+cuttkt+style+dyelot+STR(nlineno,6)+STR(lineno,6)=lcKey
  SET ORDER TO TAG TmpLine1
  GOTO lnRecNo
  SHOW WINDOW (lcBrDtTtl) REFRESH
ENDIF

IF llWareLoc
  lcOldAlias = ALIAS() 
  SELECT (lcTemLoc)
  DELETE ALL FOR cWareCode<>lcWareCode AND Style=lcStyle
  SELECT WHSLOC 
  SET ORDER TO TAG WhsLoc
  llFound=SEEK(lcWareCode)
  SET ORDER TO TAG WhsLocSt
  IF llFound
    =lfvLocat()
  ELSE
    *--No locations have been assigned to warehouse XXXXXX .'
    = gfModalGen('TRM42058B42000','DIALOG',ALLTRIM(lcWareCode))
  ENDIF  
  SELECT (lcOldAlias)
  IF !llWareHous
    llLoc = .F.
    SHOW GET llLoc
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvLocat()
*! Purpose   : Do Functions mover to select locations.
*!*************************************************************
FUNCTION lfvLocat

SELECT WHsLoc
SEEK PADR(lcStyle,19)+SPACE(6)+lcWareCode
SCAN WHILE STYLE+COLOR+CWARECODE+CLOCATION=PADR(lcStyle,19)+SPACE(6)+lcWareCode
  SCATTER MEMVAR
  SELECT (lcTemLoc)
  IF !SEEK(PADR(lcStyle,19)+lcWareCode)
    APPEND BLANK
    GATHER MEMVAR 
  ENDIF
ENDSCAN

DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
SELECT cLocation FROM WHSLOC ;
  WHERE Style+Color+cWareCode == SPACE(19)+SPACE(6)+lcWareCode ;
  INTO ARRAY laSource

SELECT cLocation FROM (lcTemLoc) ;
  WHERE Style+cWareCode+cLocation=PADR(lcStyle,19)+lcWareCode;
  INTO ARRAY laTarget

=gfMover(@laSource,@laTarget,"Assign Locations",.T.,'lfvLoc')

SELECT (lcTemLoc)
DELETE FOR Style+cWareCode+cLocation = PADR(lcStyle,19)+lcWareCode

FOR I = 1 TO ALEN(laTarget)
   APPEND BLANK
   REPLACE STYLE     WITH lcStyle;
           CWARECODE WITH lcWareCode;
           CLOCATION WITH laTarget[i]
ENDFOR
RETURN


**************************************************************
* Name : lfvLoc
**************************************************************
FUNCTION lfvLoc
PARAMETERS lnOption

*IF _CUROBJ = OBJNUM(pbMove)
*  RETURN
*ENDIF  
DO CASE 
  CASE lnOption=1
    lcLocatin=laSource[lsSource]
    IF !SEEK(PADR(lcStyle,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
      *--Location XXX is not assigned to '+'style XXX in warehouse XXX,'\!\<Assign;\<Cancel
      IF gfModalGen('TRM42064B42007','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(lcStyle)+'|'+ALLTRIM(lcWareCode)) = 1
        RETURN .T.
      ELSE
        RETURN .F. 
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption=2
    FOR I=1 TO ALEN(laSource,1)
      lcLocatin=laSource[I]
      IF !SEEK(PADR(lcStyle,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
        *--One or more Location(s) are not assigned to '+'style XXX in warehouse XXX,'\!\<Assign;\<Cancel
        IF gfModalGen('TRM42092B42007','DIALOG',ALLTRIM(lcStyle)+'|'+ALLTRIM(lcWareCode)) = 1
          RETURN .T.
        ELSE
          RETURN .F. 
        ENDIF
      ENDIF
    ENDFOR
  CASE (lnOption=3 OR lnOption=4)
ENDCASE
RETURN 

*:*************************************************************
*! Name    : lfvNewLn
*! Purpose : Add new line.
*:*************************************************************
FUNCTION lfvNewLn


*B802759,1 RAMY Add this line to store the currunt selected 
*B802759,1 RAMY transaction # [start]
lcOldCutTkt = lcCuttkt
*B802759,1 RAMY [end]
=lfLinInfo(.F.)

IF laData[4]
  SHOW GET lcCarton ENABLE
  *E301395,1 SSH 09/04/00 *** Begin *** Get the last cuttkt as default.
  lcCuttkt = lcOldCutTkt
  *SHOW GET lcCuttkt DISABLE
  *E301395,1 SSH 09/04/00 *** End   ***
  _CUROBJ=OBJNUM(lcCarton)
ELSE
  *B802759,1 RAMY Add this line to restore the old transaction # [start]
  lcCuttkt = lcOldCutTkt
  *B802759,1 RAMY [end]
  SHOW GET lcCuttkt  ENABLE
  SHOW GET ibtkt     ENABLE
  _CUROBJ=OBJNUM(lcCuttkt)
ENDIF
SHOW GET pbKey ENABLE
RETURN


*:*************************************************************
*! Name    : lfvRemLn
*! Purpose : Remove a line.
*:*************************************************************
FUNCTION lfvRemLn

*-Are you sure you want to delete this line?
IF gfModalGen('QRM34036B34001','DIALOG') = 1
  *E300935,4 declare array laRemoved to have the total removed quantity

  *C200152,1 AMH Custom program to hold the removed lines for ELS [start]
  IF lcType = "N" .AND. lcStatus = 'Approved'
    IF ASCAN(laEvntTrig , PADR('DELINE',10)) <> 0
      =gfDoTriger('MFRCVTP',PADR('DELINE',10))
    ENDIF
  ENDIF
  *C200152,1 AMH [End]

  *B602828,1 Sameh Commented out the next line and its endif line 
  *IF !EMPTY(Dyelot) AND lcMPara='I'
  PRIVATE laRemoved
  DIMENSION laRemoved[8]
  laRemoved = 0
  *ENDIF
  
  SELECT (lcTmpLine)
  
  *E300935,4 Comment out this block because because lcKey  [begin]
  *E300935,4 already have spaces if fields is empty 
  *IF laData[4]
  *  lnLineNo=LineNo
  *  IF !EMPTY(lcCuttkt+lcStyle+lcDyelot)
  *    lcKey=lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnLineNo,6)
  *  ELSE
  *    lcKey=lcCarton+SPACE(35)+STR(lnLineNo,6)
  *  ENDIF
  *ELSE
  *  lcKey=lcCarton+lcCuttkt+lcStyle+lcDyelot
  *ENDIF 
  *E300935,4   Change the previous expressions to the following one.
  *E300935,4 Comment out this block because because lcKey  [end]

  lcKey = cCarton + cuttkt + style + dyelot + STR(nlineno,6) + STR(lineno,6)

  IF SEEK(IIF(lcMPara='M','4','5')+lcKey)
    *E300935,4 Compute removed quantity before delete lines.
    *REPLACE REST Flag WITH IIF(Flag='S','D','X') ;
            *WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=IIF(lcMPara='M','4','5')+lcKey
            *WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=IIF(lcMPara='M','4','5')+lcKey
    SCAN REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=IIF(lcMPara='M','4','5')+lcKey
      REPLACE Flag WITH IIF(Flag='S','D','X')
      
      *B602828,1 Sameh Commented out the next line and its endif line 
      *IF !EMPTY(Dyelot) AND lcMPara='I'
      FOR lnI = 1 TO 8
        lnZ = STR(lnI,1)
        laRemoved[lnI] = laRemoved[lnI] + Qty&lnZ
      ENDFOR
      *ENDIF
    ENDSCAN
   
    =SEEK(IIF(lcMPara='M','4','5')+lcKey)
    *DELETE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=IIF(lcMPara='M','4','5')+lcKey
    DELETE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=IIF(lcMPara='M','4','5')+lcKey

  ENDIF
  
  IF SEEK(IIF(lcMPara='M','3','4')+lcKey)
    *E300935,4 Compute removed quantity before delete lines.
    *REPLACE REST Flag WITH IIF(Flag='S','D','X') ;
            *WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) = IIF(lcMPara='M','3','4')+lcKey
            *WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=IIF(lcMPara='M','3','4')+lcKey
    
    SCAN REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) = IIF(lcMPara='M','3','4')+lcKey
      REPLACE Flag WITH IIF(Flag='S','D','X')
      IF !EMPTY(Dyelot) AND lcMPara='I'
        FOR lnI = 1 TO 8
          lnZ = STR(lnI,1)
          laRemoved[lnI] = laRemoved[lnI] + Qty&lnZ
        ENDFOR
      ENDIF
    ENDSCAN

    =SEEK(IIF(lcMPara='M','3','4')+lcKey)
    *DELETE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=IIF(lcMPara='M','3','4')+lcKey
    DELETE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=IIF(lcMPara='M','3','4')+lcKey
  ENDIF
  
  IF SEEK('2'+lcKey)
    *E300935,4 Compute removed quantity before delete lines.
    *REPLACE REST Flag WITH IIF(Flag='S','D','X') ;
            *WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)='2'+lcKey
            *WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')='2'+lcKey

    SCAN REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)='2'+lcKey
      REPLACE Flag WITH IIF(Flag='S','D','X')
      IF !EMPTY(Dyelot) AND lcMPara='I'
        FOR lnI = 1 TO 8
          lnZ = STR(lnI,1)
          laRemoved[lnI] = laRemoved[lnI] + Qty&lnZ
        ENDFOR
      ENDIF
    ENDSCAN
    
    =SEEK('2'+lcKey)
    *DELETE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')='2'+lcKey
    DELETE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)='2'+lcKey
  ENDIF

  IF SEEK('1'+lcKey)
    *E300935,4 add deleted line quantities to same lines have the another dyelots.[Begin]
    *E300935,4 But this in case of system support dyelots and Imported styles.

    *B602828,1 Sameh Commented out the next line and its endif line 
    *IF !EMPTY(Dyelot) AND lcMPara='I'
    lnCurLine = nLineNo
    *Add removed  quantity to the same lines with another dyelots.

    *B602828,1
    *SCAN FOR Trancd+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) = ;
             '1' + lcCarton + lcCutTkt + lcStyle AND ;
             STR(nLineNo,6) = STR(lnCurLine,6)   AND ;
             DYELOT # lcDyelot
    SCAN FOR Trancd+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) = ;
             '1' + lcCarton + lcCutTkt + lcStyle AND ;
             STR(nLineNo,6) = STR(lnCurLine,6)   AND ;
             IIF(lcMPara='M',.T.,(Dyelot # lcDyelot))
    *B602828,1	

      FOR lnI = 1 TO 8
          lcZ = STR(lnI,1)  
          REPLACE QTY&lcZ WITH QTY&lcZ + laRemoved[lnI] ,;
                  TOTQTY  WITH TOTQTY  + laRemoved[lnI]
      ENDFOR 
      *E300935,4 Now MAX Function is on the screen.
      *REPLACE TOTBAL WITH MAX(TOTQTY - TOTSTK - TOTDAM - TOTCAN,0)
      REPLACE TOTBAL WITH TOTQTY - TOTSTK - TOTDAM - TOTCAN
    ENDSCAN
    = SEEK('1'+lcKey)
    *ENDIF  
    *E300935,4 add deleted line quantities to same lines have the another dyelots.[End]

    laData[5]=laData[5]-TotStk
    laData[6]=laData[6]-TotDam
    laData[7]=laData[7]-TotCan
    REPLACE Flag WITH IIF(Flag='S','D','X')
    DELETE
  ENDIF

  *B603317,1 Optimize Speed [Begin]
  *GO TOP
  LOCATE
  *B603317,1 Optimize Speed [End  ]

  =lfLinInfo(!EOF())    
  =lfRefresh(lcWinCh0)
  IF EOF()
    SHOW GET laData[4] ENABLE
    
    *E301480,1 NAD (Start) check for type because we added anew type ('N') for inter location po Batch.
    *IF lcMPara = 'I' 
    IF lcMPara = 'I' AND lcType='P'
    *E301480,1 NAD (End) 
    
      laData[9] = ''
      SHOW GET laData[9] ENABLE
      SHOW GET ibShp     ENABLE
    ENDIF

    IF laData[4]
      SHOW GET lcCarton ENABLE
      IF lcMPara = 'I'
        _CUROBJ=OBJNUM(laData[9])
      ELSE
        _CUROBJ=OBJNUM(lcCarton)
      ENDIF
    ELSE
      SHOW GET lcCuttkt  ENABLE
      SHOW GET ibtkt     ENABLE
      IF lcMPara = 'I'
        _CUROBJ=OBJNUM(laData[9])
      ELSE
        _CUROBJ=OBJNUM(lcCuttkt)
      ENDIF
    ENDIF
  ENDIF
ENDIF
RETURN
*-- end of lfvRemLn.

*:*************************************************************
*! Name    : lfvEditQty
*! Purpose : Edit line quantity.
*:*************************************************************
FUNCTION lfvEditQty

*E300935,4 declare arrays to hold old output values to update matched lines.

DIMENSION laOldOut[8]
STORE 0 TO laOldOut


DIMENSION laOrg[8],laSok[8],laDam1[8],laDam2[8],laCan[8],laBal[8],laConst[8],laRemain[9]
STORE 0 TO laOrg,laSok,laDam1,laDam2,laCan,laBal,laConst,laRemain

*B602828,1 store STYLE and CUTTKT/PO number from TempLine File [Begin.]
*B602828,1 get BUDGET which appears in QUANTITY SCR from (Cuttktl/Posln) [Begin.]	
SELECT (lcTmpLine)
PRIVATE lcMastKey

*E301480,1 NAD (Start) Pass the new parameter to lcMastKey to handle the case of inter location PO Batch.
*lcMastKey = IIF(lcMPara = 'M','','P') + Cuttkt + Style + IIF(lcMPara = 'M',Dyelot,STR(nlineno,6)) + '1'
lcMastKey = IIF(lcMPara = 'M','',lcType) + Cuttkt + Style + IIF(lcMPara = 'M',Dyelot,STR(nlineno,6)) + '1'
*E301480,1 NAD (End)

IF SEEK(lcMastKey,lcMasterL)
  SELECT (lcMasterL)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laConst
ENDIF

*B602828,1 get BUDGET which appears in QUANTITY SCR from (Cuttktl/Posln) [End.]	
*B602828,1 store STYLE and CUTTKT/PO number from TempLine File [End.]

SELECT (lcTmpLine)
SCATTER MEMVAR
lnLineNo = LineNo
lcWareCode = cWareCode

*E300935,4 Add the following line to adjust index of seek command
lnCutLine = nLineNo
*E300935,4

*E300935,4 Change index to include nLineNo Field [BEGIN]
*=SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
= SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
*E300935,4 Change index [END]

SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOrg
 
=SEEK('S'+STYLE.Scale,'SCALE')
lcSz1=SCALE.Sz1
lcSz2=SCALE.Sz2
lcSz3=SCALE.Sz3
lcSz4=SCALE.Sz4
lcSz5=SCALE.Sz5
lcSz6=SCALE.Sz6
lcSz7=SCALE.Sz7
lcSz8=SCALE.Sz8

STORE '' TO lcRetSty1,lcRetSty2,;
            lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4
lcMStyQlty=STYLE.cStyGrade
DO CASE
  CASE lcMStyQlty='1'
    lcRetSHd ="1st Quality"
    lcRetSHd1="2nd Quality"
    lcRetSHd2="Damaged"
    lcRetSHd3="Second Quality"
    lcRetSHd4="Damaged"
  CASE lcMStyQlty='2'
    lcRetSHd ="2nd Quality"
    lcRetSHd1="1st Quality"
    lcRetSHd2="Damaged"
    lcRetSHd3="First Quality"
    lcRetSHd4="Damaged"
  CASE lcMStyQlty='3'
    lcRetSHd ="Damaged"
    lcRetSHd1="1st Quality"
    lcRetSHd2="2nd Quality"    
    lcRetSHd3="First Quality"
    lcRetSHd4="Second Quality"
ENDCASE

lnOldStk=TotStk
lnOldDam=TotDam
lnOldCan=TotCan

*--Get Stock quantity.
*E300935,4 Change index 
*IF SEEK('2'+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
IF SEEK('2'+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSok
ENDIF

*--Get Damage quantity.
lcRSt1Stat='DISABLE'
lcRSt2Stat='DISABLE'
*E300935,4 Change index 
*IF SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
IF SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  *E300935,4 Change scan expression due to change Index.
  *SCAN WHILE TranCd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=;
  *   IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),'')
  SCAN REST WHILE TranCd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) =;
       IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6)

    IF (lcMStyQlty='1' AND cStyGrade='2') OR cStyGrade='1'
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam1
      lcRetSty1=cRetSty
      lcRSt1Stat='ENABLE'
    ELSE
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam2
      lcRetSty2=cRetSty
      lcRSt2Stat='ENABLE'
    ENDIF
  ENDSCAN
ENDIF

*--Get Cancel quantity.
*E300935,4 Change index 
*IF SEEK(IIF(lcMPara='M','4','5')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
IF SEEK(IIF(lcMPara='M','4','5')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
ENDIF

FOR I=1 TO 8
  *E300935,4 Now MAX Function is on the screen and browse
  *laBal[I]=IIF( laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I])<0, 0,;
  *              laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]) ) 
  laBal[I]= laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I])
ENDFOR

*--Disable edit quantity if view mode.
lcSkMode = IIF(laScrMode[2],'DISABLE','ENABLE')

*C200152,1 AMH Disable dam. & cancel fields in case inter location PO [start]
*lcOtMode = IIF(laScrMode[2],'DISABLE','ENABLE')
IF lcType = 'N'
  lcOtMode = 'DISABLE'
ELSE
  lcOtMode = IIF(laScrMode[2],'DISABLE','ENABLE')
ENDIF
*C200152,1 AMH [End]

lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')

ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*E300935,4 calculate old values of Out quantities from this line.
FOR lnI = 1 TO 8
  laOldOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]

  *B602828,1 get laRemain array which appears in QUANTITY SCREEN [Begin.]
  laRemain[lnI] = laConst[lnI] - laOrg[lnI]
  laRemain[9]   = laRemain[9]  + laRemain[lnI] 
  *B602828,1 get laRemain array which appears in QUANTITY SCREEN [End.]
  
ENDFOR
*E300935,4

*--Call Quantity screen.
*B802759,1 RAMY Add these lines to add the style and the dyelot in the 
*B802759,1 RAMY screen title [start]
IF llDyelot
  lcScrTtl ='Line Quantity for Style: ' + &lcMasterL..Style + IIF(EMPTY(&lcMasterL..Dyelot) , '', '  Dyelot: ' + &lcMasterL..Dyelot)
ELSE
  lcScrTtl ='Line Quantity for Style: ' + &lcMasterL..Style
ENDIF
*B802759,1 RAMY [end]

DO (gcScrDir+"MFRCVQ.SPX")

ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt

*--Nothing changed in view mode.
IF laScrMode[2]
  *E300935,4 Change index 
  *=SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
  = SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  RETURN
ENDIF

lnTStk=laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]
lnTDam=laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]+;
       laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]
lnTCan=laCan[1]+laCan[2]+laCan[3]+laCan[4]+laCan[5]+laCan[6]+laCan[7]+laCan[8]

*--Update stock quantity.
*E300935,4 Change index 
*IF SEEK('2'+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
IF SEEK('2'+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  IF lnTStk<>0
    GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laSok
  ELSE
    REPLACE Flag WITH IIF(Flag='S','D','X')
    DELETE
  ENDIF
ELSE
  IF lnTStk<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH '2',;
            Flag   WITH 'A'
    GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laSok
  ENDIF
ENDIF

*--Update Damaged quantity.
lnStyRecNo=RECNO('STYLE')
IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
  =SEEK(lcRetSty1,'STYLE')
  *E300935,4 Change index 
  *=SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
  =SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  
  *E300935,4 Change index 
  *LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=;
  *   IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),'');
  *   FOR cStyGrade=STYLE.cStyGrade
  LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=;
                    IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6);
                FOR cStyGrade=STYLE.cStyGrade
  IF !FOUND()
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH IIF(lcMPara='M','3','4'),;
            cStyGrade WITH STYLE.cStyGrade,;
            Flag      WITH 'A'
  ENDIF
  GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laDam1  
  REPLACE cRetSty WITH lcRetSty1
ELSE

  *E300935,4 Change index 
  *=SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
  =SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))

  *E300935,4 Change index 
  *LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=;
  *  IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),'') ;
  *  FOR cStyGrade=IIF(lcMStyQlty='1','2','1')
  LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=;
                    IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6) ;
                FOR cStyGrade=IIF(lcMStyQlty='1','2','1')

  IF FOUND()
    REPLACE Flag WITH IIF(Flag='S','D','X')
    DELETE
  ENDIF
ENDIF

IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0      
  =SEEK(lcRetSty2,'STYLE')

  *E300935,4 Change index 
  *=SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
  =SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))

  *E300935,4 Change index
  *LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=;
  *             IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),'') ;
  *             FOR cStyGrade=STYLE.cStyGrade
  LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=;
                    IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6) ;
                FOR cStyGrade=STYLE.cStyGrade

  IF !FOUND()
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH IIF(lcMPara='M','3','4'),;
            cStyGrade WITH STYLE.cStyGrade,;
            Flag      WITH 'A'
  ENDIF
  GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laDam2  
  REPLACE cRetSty WITH lcRetSty2
ELSE

  *E300935,4 Change index 
  *=SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
  =SEEK(IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))

  *E300935,4 Change index 
  *LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+IIF(laData[4],STR(LineNo,6),'')=;
  *              IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),'') ;
  *              FOR cStyGrade=IIF(lcMStyQlty='3','2','3')
  LOCATE REST WHILE Trancd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)=;
                    IIF(lcMPara='M','3','4')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6) ;
                FOR cStyGrade=IIF(lcMStyQlty='3','2','3')

  IF FOUND()
    REPLACE Flag WITH IIF(Flag='S','D','X')
    DELETE
  ENDIF
ENDIF

IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
  GOTO lnStyRecNo IN STYLE
ENDIF  

*--Update cancel quantity.

*E300935,4 Change index 
*IF SEEK(IIF(lcMPara='M','4','5')+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
IF SEEK(IIF(lcMPara='M','4','5')+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
  IF lnTCan<>0
    GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laCan
  ELSE
    REPLACE Flag WITH IIF(Flag='S','D','X')
    DELETE
  ENDIF
ELSE
  IF lnTCan<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH IIF(lcMPara='M','4','5'),;
            Flag   WITH 'A'
    GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FROM laCan
  ENDIF
ENDIF

SELECT (lcTmpLine)
*E300935,4 Change index 
*=SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+IIF(laData[4],STR(lnLineNo,6),''))
=SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))

REPLACE TotStk WITH lnTStk,;
        TotDam WITH lnTDam,;
        TotCan WITH lnTCan,;
        TotBal WITH laBal[1]+laBal[2]+laBal[3]+laBal[4]+laBal[5]+laBal[6]+laBal[7]+laBal[8]

laData[5]=laData[5]+(TotStk-lnOldStk)
laData[6]=laData[6]+(TotDam-lnOldDam)
laData[7]=laData[7]+(TotCan-lnOldCan)

*E300935,4 Evaluate total out quantity and update another lines. [Begin]
*-- Calculate Out Quantity in this Line. [BEGIN]
IF !EMPTY(lcDyelot) AND (lcMPara='I')
  
  DIMENSION laOut[8]
  STORE 0 TO lnTotOut,laOut
  FOR lnI = 1 TO 8
    Z = STR(lnI,1)
    laOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
    *MAB NOW IF laOut[lnI] > Qty&Z
      *MAB NOW laOut[lnI] = Qty&Z
    *MAB NOW ENDIF 
    lnTotOut   = lnTotOut + laOut[lnI]
  ENDFOR
  *-- Calculate Out Quantity in this Line. [BEGIN]

  *Subtract out quantity from the same lines with another dyelots.
  SCAN FOR Trancd+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6) = ;
           '1' + lcCarton + lcCutTkt + lcStyle AND ;
           STR(nLineNo,6) = STR(lnCutLine,6)   AND ;
           DYELOT # lcDyelot
    *MAB NOW IF (TOTQTY  >= TOTSTK+TOTDAM+TOTCAN )
      FOR lnI = 1 TO 8
          lcZ = STR(lnI,1)  
          *E300935,4 Now MAX Function is on the screen.
          *REPLACE QTY&lcZ WITH MAX(QTY&lcZ + laOldOut[lnI] - laOut[lnI],0) ,;
          *        TOTQTY  WITH MAX(TOTQTY  + laOldOut[lnI] - laOut[lnI],0)
          REPLACE QTY&lcZ WITH QTY&lcZ + laOldOut[lnI] - laOut[lnI] ,;
                  TOTQTY  WITH TOTQTY  + laOldOut[lnI] - laOut[lnI]
      ENDFOR 
      *REPLACE TOTBAL WITH MAX(TOTQTY-TOTSTK-TOTDAM-TOTCAN,0)
      REPLACE TOTBAL WITH TOTQTY-TOTSTK-TOTDAM-TOTCAN
    *MAB NOW ENDIF  
  ENDSCAN
  =SEEK('1'+lcCarton+lcCuttkt+lcStyle+lcDyelot+STR(lnCutLine,6)+STR(lnLineNo,6))
ENDIF
*E300935,4 Evaluate total out quantity and update another lines. [End]

=lfRefresh(lcWinCh0)
SHOW WINDOW (lcBrDtTtl) REFRESH
RETURN

*:*************************************************************
*! Name    : lfvQty
*! Purpose : Quantity valid. 
*:*************************************************************
FUNCTION lfvQty
PARA lnCnxt

lnNewBal=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])

*B604646,1 MHM 07/17/2001  Add condition For not exceed  original[Start]
IF  lcType = "N" AND lnNewBal < 0
    =gfModalGen('INM00000B00000','','','','You can not exceed the issued quantity')
    lcCnxt = STR(lnCnxt,1)
    IF _CUROBJ = OBJNUM(laSok[lnCnxt])
      laSok[lnCnxt] = &lcTmpLine..Qty&lcCnxt
      SHOW GET laSok[lnCnxt]
    ENDIF
      
    IF _CUROBJ = OBJNUM(laDam1[lnCnxt])
      laDam1[lnCnxt] = 0
      SHOW GET laDam1[lnCnxt]
    ENDIF

    IF _CUROBJ = OBJNUM(laDam2[lnCnxt])
      laDam2[lnCnxt] =0
      SHOW GET laDam2[lnCnxt]
    ENDIF

    IF _CUROBJ = OBJNUM(laCan[lnCnxt])
      laCan[lnCnxt] = 0
      SHOW GET laCan[lnCnxt]
    ENDIF

   _CUROBJ=_CUROBJ
    RETURN
ENDIF
*B604646,1 MHM 07/17/2001  Add condition For not exceed  original[End]

*C200152,1 AMH Custom trigger to not allow the exceeding the original qty.
*C200152,1     in case of approved batch ELS [start]
IF lcType = "N"
  IF ASCAN(laEvntTrig , PADR('EDITLINE',10)) <> 0
    =gfDoTriger('MFRCVTP',PADR('EDITLINE',10))
  ENDIF
ENDIF
*C200152,1 AMH [End]
IF _CUROBJ<>OBJNUM(laCan[lnCnxt]) AND laCan[lnCnxt] > 0 AND lnNewBal<0
  *--Cancel quantity more that new balance, Therefore it will be redused.
  =gfModalGen('INM34068B34000','DIALOG')
  laBal[lnCnxt]=0
  laCan[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt])
  laCan[lnCnxt]=IIF(laCan[lnCnxt]<0,0,laCan[lnCnxt])
  SHOW GET laCan[lnCnxt]
ELSE
  laBal[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
  laBal[lnCnxt]=IIF(laBal[lnCnxt]<0,0,laBal[lnCnxt])
ENDIF
IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
  IF EMPTY(lcRetSty1)
    lcRetSty1=STYLE.cRetSty
  ENDIF
  SHOW GET lcRetSty1 ENABLE
  SHOW GET ibRtSt1   ENABLE
  *B604677,1 MHM 07/19/2001 Disable the remaning buckets according to the
  *B604677,1                scale [Start]
  IF !EMPTY(lcRetSty1) AND lnCnxt = 1
    =lfChckScal(lcRetSty1,'1')
  ENDIF  
  *B604677,1 MHM 07/19/2001 [End]
  
ELSE
  lcRetSty1=SPACE(19)
  SHOW GET lcRetSty1 DISABLE
  SHOW GET ibRtSt1   DISABLE
ENDIF
IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
  IF EMPTY(lcRetSty2)
    lcRetSty2=STYLE.cRetSty2
  ENDIF
  SHOW GET lcRetSty2 ENABLE
  SHOW GET ibRtSt2   ENABLE
  *B604677,1 MHM 07/19/2001 Disable the remaning buckets according to the
  *B604677,1                scale [Start]
  IF !EMPTY(lcRetSty2) AND lnCnxt = 1
    =lfChckScal(lcRetSty2,'2')
  ENDIF  
  *B604677,1 MHM 07/19/2001 [End]
ELSE
  lcRetSty2=SPACE(19)
  SHOW GET lcRetSty2 DISABLE
  SHOW GET ibRtSt2   DISABLE
ENDIF
=lfRefresh("MFRCVQ")
RETURN

*:*************************************************************
*! Name    : lfvQtyOk
*! Purpose : Quantity Ok pb. valid. 
*:*************************************************************
FUNCTION lfvQtyOk

IF laScrMode[2]
  CLEAR READ
  RETURN
ENDIF  
*-You cannot leave the XXXX style empty since the XXXX quantity was entered.
IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0 AND ;
   EMPTY(lcRetSty1)
   =gfModalGen('TRM34067B34000','DIALOG',lcRetSHd3+'|'+lcRetSHd3)
  _CUROBJ=OBJNUM(lcRetSty1) 
  RETURN
ENDIF
IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0 AND ;
   EMPTY(lcRetSty2)
   =gfModalGen('TRM34067B34000','DIALOG',lcRetSHd4+'|'+lcRetSHd4)
  _CUROBJ=OBJNUM(lcRetSty2) 
  RETURN
ENDIF
IF llWareHous
  IF !EMPTY(lcRetSty1) AND !SEEK(PADR(lcRetSty1,19)+lcWareCode+SPACE(10),'STYDYE')
    *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcRetSty1)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH lcRetSty1,SPACE(10),lcWareCode
    ELSE
      _CUROBJ=OBJNUM(lcRetSty1) 
      RETURN
    ENDIF
  ENDIF
  IF !EMPTY(lcRetSty2) AND !SEEK(PADR(lcRetSty2,19)+lcWareCode+SPACE(10),'STYDYE')
    *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcRetSty2)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH lcRetSty2,SPACE(10),lcWareCode
    ELSE
      _CUROBJ=OBJNUM(lcRetSty2) 
      RETURN
    ENDIF
  ENDIF
ENDIF
CLEAR READ
RETURN

*:*************************************************************
*! Name    : lfvRetSty
*! Purpose : Return style valid. 
*:*************************************************************
FUNCTION lfvRetSty
PARA lcPQualty

IF !llBrowse AND !EMPTY(SUBSTR(lcRetSty&lcPQualty,1,1)) AND;
                 lcRetSty&lcPQualty=IIF(EMPTY(lcOldValue),SPACE(19),lcOldValue)
  RETURN
ENDIF  
DO CASE
  CASE lcMStyQlty='1'
    lcQualty=IIF(lcPQualty='1','2','3')
  CASE lcMStyQlty='2'
    lcQualty=IIF(lcPQualty='1','1','3')
  CASE lcMStyQlty='3'    
    lcQualty=IIF(lcPQualty='1','1','2')
ENDCASE

lnAlias=SELECT()
SELECT STYLE
lnStyRecNo=RECNO('STYLE')
lcStyScl=Scale
LCBrStyle=IIF(lcPQualty='1',lcRetSty1,lcRetSty2)

SET ORDER TO TAG StyQlty
IF llBrowse OR (!EMPTY(LCBrStyle) AND !SEEK(lcQualty+LCBrStyle,'STYLE'))
  llbrowse = .F.
  llMake=(lcMPara='M')
  *B802759,1 Start.
  *SET FILTER TO cStyGrade=lcQualty AND Make=llMake
  llQFound = SEEK(lcQualty)
  IF llQFound
    LOCATE REST WHILE cStyGrade+Style=lcQualty FOR Make=llMake
    llQFound = FOUND()
  ENDIF
  *IF EOF()
  IF !llQFound
  *B802759,1 End.
    *--No styles found for this quality. 
    =gfModalGen('TRM42087B42000','DIALOG')
    LCBrStyle = SPACE(19)
  ELSE
    *B802759,1 Start.
    SET FILTER TO cStyGrade+Style=lcQualty AND Make=llMake
    *B802759,1 End.
    lcHdEsc1= ON('KEY','ESC')
    ON KEY LABEL ESC 
    LCBrStyle = gfStyBrw('I','','',.F.)
    ON KEY LABEL ESC &lcHdEsc1
    SELECT STYLE
    SET FILTER TO 
  ENDIF 
ENDIF
lcRetSty&lcPQualty = LCBrStyle
SET ORDER TO TAG Style
IF !EMPTY(LCBrStyle) AND Scale<>lcStyScl
  *--The selected style has a different size scale.
  SHOW GET lcRetSty&lcPQualty
  *B604677,1 MHM 07/19/2001   Prevent recive to damage/Cancel if the styles have different scales.[start]
  IF !lfChkScal(Scale,PrePak)

    lnReturn = gfModalGen('TRM42089B44009','DIALOG',"","","Selected style has a different size scale. Are you sure you want to proceed")
    IF lnReturn = 1
      IF lcPQualty = '1'
        lnTotal   = laDam1[1] + laDam1[2] + laDam1[3] + laDam1[4] + laDam1[5] + laDam1[6] + laDam1[7] + laDam1[8]
        laBal[1]  = 0
        laDam1[1] = lnTotal
        FOR lnI = 2 TO 8
          laDam1[lnI] = 0
          laBal[lnI]  = laOrg[lnI] - laSok[lnI] - laDam1[lnI] - laDam2[lnI] - laCan[lnI]
        ENDFOR
        SHOW GETS
        *- Disable remaning field if scale is deffrante 
         =lfChckScal(lcRetSty&lcPQualty,'1')
      ELSE
        lnTotal   = laDam2[1] + laDam2[2] + laDam2[3] + laDam2[4] + laDam2[5] + laDam2[6] + laDam2[7] + laDam2[8]
        laBal[1]  = 0
        laDam2[1] = lnTotal
        FOR lnI = 2 TO 8
          laDam2[lnI] = 0
          laBal[lnI]  = laOrg[lnI] - laSok[lnI] - laDam1[lnI] - laDam2[lnI] - laCan[lnI]
        ENDFOR
        SHOW GETS
        *-B604677,1 Disable remaning field if scale is deffrante 
        =lfChckScal(lcRetSty&lcPQualty,'2')
      ENDIF
    ELSE
      lcBrStyle = ''
      lcRetSty&lcPQualty = ''
      SHOW GET lcRetSty&lcPQualty
      IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
        GOTO lnStyRecNo IN STYLE
      ENDIF
      SELECT(lnAlias)
      RETURN
    ENDIF
  ELSE
    =gfModalGen('INM42089B42000','DIALOG')
  ENDIF
  *B604677,1 MHM 07/19/2001 [END]
ENDIF
GOTO lnStyRecNo IN STYLE
SELECT(lnAlias)
RETURN

*:*************************************************************
*! Name    : lpDelScr
*! Purpose : Cancel.uncancel batch.
*:*************************************************************
FUNCTION lpDelScr

IF laData[3] = 'P'
  *-This batch was posted! cannot cancel it.
  =gfModalGen('INM34069B34000','DIALOG','cancel')
  =gfObj_lock(.F.)
  laScrMode = .F.
  laScrMode[2] = .T.
  RETURN
ENDIF

*C200152,1 AMH Custom program to hold the removed lines for ELS [start]
IF lcType = "N"
  IF ASCAN(laEvntTrig , PADR('CANCEL',10)) <> 0
    =gfDoTriger('MFRCVTP',PADR('CANCEL',10))
  ENDIF
ENDIF
*C200152,1 AMH [End]

IF laData[3]<>'X'
  lcStatus = 'Cancelled'
  laData[3]='X'
  lcPromp    = lcUnCan
  lcDelMesag = 'uncancel'
ELSE
  lcStatus = 'Open'
  laData[3]='O'
  lcPromp   = lcCancel
  lcDelMesag = 'cancel'
ENDIF
SHOW GET lcStatus DISABLE
lcMenProm  = PROPER(lcDelMesag)+' batch'
DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm)

*C200152,4 AMH Disable cancel button in inter location PO case [start]
*SHOW GET pbDlt,1 PROMPT lcPromp
IF lcType = "N" .AND. lcPromp = lcUnCan
  SHOW GET pbDlt,1 PROMPT lcPromp DISABLE
ELSE
  SHOW GET pbDlt,1 PROMPT lcPromp
ENDIF
*C200152,4 AMH [End]

SELECT (lcBaseFile)
REPLACE CtKtRcvH.cStatus WITH laData[3]
RETURN

*:*************************************************************
*! Name    : lpSavScr
*! Purpose : Save batch.
*:*************************************************************
FUNCTION lpSavScr

STORE .T. TO llCSave,llCUpdate,llUpdated,llShow
*E500331,1  Define Variable [Begin]

IF TYPE('llBatExist') = "U"
  llBatExist  = .F.
ENDIF
lcTypBat = IIF(TYPE('lcTypBat')$'UL',lcMPara,lcTypBat)

lcBatchNo   = ''
*E500331,1 [End]

GO TOP IN (lcTmpLine)
IF EOF(lcTmpLine)
  *--The lines for this bXXX are missing ! cannot update.
  = gfModalGen('TRM34017B34000','DIALOG','Batch')
  STORE .F. TO llShow,llCSave
  RETURN .F.
ENDIF
SELECT (lcTmpLine)

LOCATE FOR TranCd<>'1'
IF !FOUND()

  *--No Receiving Lines was done,Cannot update.
  *B602790,1 Pass Parameter to message [Begin]
  *= gfModalGen('INM34061B34000','DIALOG')
  = gfModalGen('INM34061B34000','DIALOG','Receiving Lines')
  *B602790,1 Pass Parameter to message [End  ]

  STORE .F. TO llShow,llCSave
  *C200152,4 AMH Go to frist record [start]
  LOCATE
  _CUROBJ = OBJNUM(pbKey)
  *C200152,4 AMH [End]
  RETURN .F.
ENDIF

*C128583,1  TMI [Start] set the variable llBatExist to .F. and let the batch be created with a new sequence
IF ASCAN(laEvntTrig,'APPROVE')<>0 
  =gfDoTriger('PORECPO','APPROVE')
ENDIF
*C128583,1  TMI [End  ] 

*E500331,1 ABD Add To ann Existing batch  [Begin]
*E500331,1 Message No.  : 34170.
*E500331,1 Message TEXT : Do you want to add to an existing batch.
*E500331,1 Button  No.  : 34001.
*E500331,1 Button  TEXT :  \<Yes  - \<No.
SELECT CtKtRcvH
IF llBatExist
  llBatExist  = .F.
  IF laScrMode[4] AND laData[3]<>'A' AND ;
            ( gfModalGen('QRM34170B34001','DIALOG')=1 )
    
    *E301480,1 NAD (Start) Use the new variable in the expression. 
    *LOCATE FOR cStatus = "O" .AND. IIF(lcMPara ='M',Ctype = 'M',Ctype = 'I')
    LOCATE FOR cStatus = "O" .AND. IIF(lcMPara ='M',Ctype = 'M',Ctype = lcTypBat)
    *E301480,1 NAD (End) 
    IF FOUND()
      *-- Call the screen For enter select batch #.
     DO (gcScrDir+"MFBATHNO.SPX")
     llBatExist  = .T.
    ELSE
      *E500331,1 Message No.  : 34171.
      *E500331,1 Message TEXT : You have not an existing open batchs.
      *E500331,1 Button  No.  : 38018. 
      *E500331,1 Button  TEXT : \<OK.
      =gfModalGen('QRM34171B38018','DIALOG')
    ENDIF
  ENDIF
ENDIF
*E500331,1 ABD   [End]

*--Update the master header file.
SELECT CtKtRcvH
IF laScrMode[4]
  *E500331,1 [Begin]
  *laData[1] = gfSequence('TMPRCVNUM','','','')
  laData[1] = IIF(llBatExist,lcBatchNo,gfSequence('TMPRCVNUM','','',''))
  *E500331,1 [Begin]
  
  
  SHOW GET laData[1] DISABLE

  *E500331,1 [Begin]
  IF llBatExist
    
    *E301480,1 NAD (Start) Use the new variable in the expression.   
    *=SEEK(lcMPara+lcBatchNo)
    =SEEK(lcTypBat+lcBatchNo)
    *E301480,1 NAD (End) 
    
    *-- Update the old batch with the Temp receive.
    REPLACE Carton  WITH laData[4]           ,;
            ntotstk WITH ntotstk + laData[5] ,;
            ntotdam WITH ntotdam + laData[6] ,;
            ntotcan WITH ntotcan + laData[7] 
      
  ELSE
    APPEND BLANK
    GATHER FROM laData FIELDS &lcScFields
  ENDIF
  *E500331,1 [Begin]

  *E301077,72 Update Odded fields using gfAdd_Info function [Begin]
  *Odded REPLACE cType     WITH lcMPara,;
  *Odded         Carton    WITH laData[4], ; 
  *Odded         cAdd_User WITH gcUser_id,;
  *Odded         cAdd_Time WITH TIME(),;
  *Odded         dAdd_Date WITH gdSysDate
  
  *E301480,1 NAD (Start) Use the new variable .
  *REPLACE cType     WITH lcMPara,;
          Carton    WITH laData[4]
          
  REPLACE cType     WITH lcTypBat,;
          Carton    WITH laData[4]
  *E301480,1 NAD (End) 
 
 *E301077,72 Update Odded fields using gfAdd_Info function [End  ]

ELSE

  GATHER FROM laData FIELDS &lcScFields

  *E301077,72 Update Odded fields using gfAdd_Info function [Begin]
  *Odded REPLACE Carton     WITH laData[4], ; 
  *Odded         Mod_Userid WITH gcUser_id,;
  *Odded         Mod_Time   WITH TIME(),;
  *Odded         Mod_Date   WITH gdSysDate
  
  REPLACE Carton     WITH laData[4]
  *E301077,72 Update Odded fields using gfAdd_Info function [End  ]

ENDIF

*E301077,72 Update Odded fields using gfAdd_Info function [Begin]
=gfAdd_Info('CTKTRCVH')
*E301077,72 Update Odded fields using gfAdd_Info function [End  ]

*--Do you want to approve this temporary receiving batch?,\<Yes;\<No
*C128583,1  TMI [Start] automatic approve for PAN21
*IF laScrMode[4] AND laData[3]<>'A' AND ;
 ( gfModalGen('QRM34062B34001','DIALOG')=1 )
IF laScrMode[4] AND laData[3]<>'A' AND (ASCAN(laEvntTrig,'APPROVE')<>0 .OR. gfModalGen('QRM34062B34001','DIALOG')=1 )
  *C128583,1  TMI [End  ]   
   lcStatus = 'Approved'
   SHOW GET lcStatus
   laData[3]='A'
   REPLACE CtKtRcvH.cStatus WITH 'A'
ENDIF

*--Update the master lines file.
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine2
IF laScrMode[4]
  REPLACE ALL TmpRcvNum WITH laData[1],;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
ELSE
  REPLACE ALL TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
ENDIF

SELECT (lcTmpLine)
SET DELE OFF
SCAN FOR Flag<>'X' AND TranCd<>'1'
  SCATTER MEMVAR  
  DO CASE
    CASE Flag='A'
      SELECT CTKTRCVL 
      APPEND BLANK
      GATHER MEMVAR  

      *B605762,1 KHM 03/28/2002 (Begin) Replace the carton and dyelot without spaces.      
      REPLACE cCarton WITH ALLTRIM(cCarton),;
              Dyelot  WITH ALLTRIM(Dyelot)             
      *B605762,1 KHM 03/28/2002 (End)
      
      *E301077,72 Update Odded fields using gfAdd_Info function [Begin]
      *Odded REPLACE cAdd_User WITH gcUser_id,;
      *Odded         cAdd_Time WITH TIME(),;
      *Odded         dAdd_Date WITH gdSysDate
      
      =gfAdd_Info('CTKTRCVL')
      *E301077,72 Update Odded fields using gfAdd_Info function [End  ]

    CASE Flag='S'    
      *E300935,4 Change it to new index
      *lcKey=lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(lineno,6)+TranCd
      
      *E301480,1 NAD (Start) Use the new variable in the expression. 
      *lcKey=lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd
      lcKey=lctypBat+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd
      *E301480,1 NAD (End) 
      
      SELECT CTKTRCVL 
      IF SEEK(lcKey)      
        IF Trancd=IIF(lcMPara='M','3','4')
          *E300935,4 Change it to new index
          *LOCATE REST WHILE lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(lineno,6)+TranCd=lcKey ;
          *    FOR cStyGrade = &lcTmpLine..cStyGrade
          
          *E301480,1 NAD (Start) Use the new variable in the expression. 
          *LOCATE REST WHILE lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd=lcKey ;
               FOR cStyGrade = &lcTmpLine..cStyGrade
          LOCATE REST WHILE lcTypBat+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd=lcKey ;
               FOR cStyGrade = &lcTmpLine..cStyGrade
          *E301480,1 NAD (End) 
          
          IF FOUND() 
              =RLOCK()
              GATHER MEMVAR 
        
              *B605762,1 KHM 03/28/2002 (Begin) Replace the carton and dyelot without spaces.
              REPLACE cCarton WITH ALLTRIM(cCarton),;
                      Dyelot  WITH ALLTRIM(Dyelot)             
              *B605762,1 KHM 03/28/2002 (End)

              UNLOCK
          ENDIF     
        ELSE     
          =RLOCK()
          GATHER MEMVAR 
          *B605762,1 KHM 03/28/2002 (Begin) Replace the carton and dyelot without spaces.
          REPLACE cCarton WITH ALLTRIM(cCarton),;
                  Dyelot  WITH ALLTRIM(Dyelot)             
          *B605762,1 KHM 03/28/2002 (End)

          UNLOCK
        ENDIF
      ENDIF
    CASE Flag='D'
      *E300935,4 because it can seek previously deleted data with same values
      *E300935,4 I set deleted on before seek and restore it after seek.
      SET DELETED ON
      *E300935,4 Change it to new index
      *lcKey=lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(lineno,6)+TranCd
      
      
      *E301480,1 NAD (Start) Use the new variable in the expression. 
      *lcKey=lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd
      lcKey=lcTypBat+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd
      *E301480,1 NAD (End) 
      
      SELECT CTKTRCVL 
      IF SEEK(lcKey)      
        IF Trancd=IIF(lcMPara='M','3','4')
          *E300935,4 Change it to new index
          *LOCATE REST WHILE lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(lineno,6)+TranCd=lcKey ;
          *      FOR cStyGrade = &lcTmpLine..cStyGrade
          
          *E301480,1 NAD (Start) Use the new variable in the expression. 
          *LOCATE REST WHILE lcMPara+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd=lcKey ;
               FOR cStyGrade = &lcTmpLine..cStyGrade
          
          LOCATE REST WHILE lcTypBat+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nlineno,6)+STR(lineno,6)+TranCd=lcKey ;
               FOR cStyGrade = &lcTmpLine..cStyGrade
          *E301480,1 NAD (End) 
          
          IF FOUND()
            =RLOCK()
            DELETE
            UNLOCK
          ENDIF     
        ELSE     
          =RLOCK()
          DELETE
          UNLOCK
        ENDIF
      ENDIF
      *E300935,4 because it can seek previously deleted data with same values
      *E300935,4 I set deleted on before seek and restore it after seek.
      SET DELETED OFF
  ENDCASE
ENDSCAN

SELECT (lcTmpLine)
SET DELE ON
SET ORDER TO TAG TmpLine1

*--Update Master file for locations.
IF llWareLoc 
  lcOld=ALIAS()
  SELECT (lcTemLoc)
  GO TOP
  SCAN FOR !EMPTY(cLocation)
    IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
      SCATTER MEMVAR
      INSERT INTO WhsLoc FROM MEMVAR
    ENDIF
  ENDSCAN
  SELECT (lcOld)
ENDIF 

*C200152,1 AMH Custom trigger to update the hold POSLN rec. for ELS [start]
IF !(TYPE('lcType') $ 'UL') .AND. lcType = "N"
  IF ASCAN(laEvntTrig , PADR('SAVELINE',10)) <> 0
    =gfDoTriger('MFRCVTP',PADR('SAVELINE',10))
  ENDIF
ENDIF
*C200152,1 AMH [End]

IF laScrMode[4]
  =gfModalGen('INM42085B42000','DIALOG','Temporary receiving batch'+'|'+laData[1])
ENDIF

*C128583,1  TMI [Start] Create a descripance log file and update the shipment if needed
IF ASCAN(laEvntTrig,'DSCRPNCY')<>0
  =gfDoTriger("PORECPO",PADR("DSCRPNCY",10))
ENDIF  
*C128583,1  TMI [End  ] 

SELECT (lcBaseFile)
RETURN



*!*************************************************************
*! Name    : lpClsScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Cancel/Close entery.
*!*************************************************************
FUNCTION lpClsScr

*--Clear Header.
SELECT (lcBaseFile)
SCATTER MEMVAR MEMO BLANK
*--Clear Line.
=lfLinInfo(.F.)

laScrMode=.F.
laScrMode[1]=.T.
*RAMY
lnLastRec = 0
lcOldTkt  = ''

RETURN

*!*************************************************************
*! Name    : lfvByShp
*! Developer: Hossam El Etreby
*! Date     : 05/12/1999
*! Purpose : Valid function for shipment # field
*!*************************************************************
*! Due to  E301220,1
*!*************************************************************

FUNCTION lfvByShp
PRIVATE lcOldShp , llShpUsed

llbrowse = llbrowse OR '?' $ lcCuttkt
lcOldShp = laData[9]
llShpUsed = gfOpenFile(gcDataDir + 'ShpmtHdr','Shpmthdr')

IF !EMPTY(laData[9]) OR llbrowse
  IF !SEEK(laData[9],'SHPMTHDR') OR llBrowse
  llbrowse = .F.
  SELECT ShpmtHdr
  lcBrFields = [ShipNo :8   :H = 'Shipment #',]+;
               [Status :1   :H = "S",]+;
               [Entered :8  :H = 'Entered',]+;
  	           [Cartons :5  :H = 'Cartons',]+;
	           [AirWayB :15 :H = 'Air-Way Bill #',]+;
    	       [ETA     :8  :H = 'E.T.A.',]+;
    	       [TotQty  :7  :H = 'In-Transit',]+;
    	       [Recv_Stk :6 :H = 'Received',]+;
    	       [Recv_Dam :6 :H = 'Damaged',]+;
    	       [Recv_Can :6 :H = 'Canceled',]+;
    	       [cVessel :30 :H = 'Airline/Vessel',]+;
    	       [Reference   :H = 'Reference']

  SELECT SHPMTHDR
  DIME laTempData[1]
  STORE '' TO laTempData
  =gfBrows(.F.,'ShipNo','laTempData','Shipments')
  IF EMPTY(laTempData[1])
    laData[9] = ''
    IF llShpUsed
      USE IN ShpmtHdr
      llShpUsed = .F.
    ENDIF

    RETURN
  ELSE
    laData[9] = laTempData[1]
  ENDIF
  ENDIF
ELSE
  IF llShpUsed
    USE IN ShpmtHdr
    llShpUsed = .F.
  ENDIF
  RETURN  
ENDIF

IF !laData[4]
  lnRespons = gfModalGen('QRM34158B34015','DIALOG')
  DO CASE
    CASE lnRespons = 1
      IF !EMPTY(laData[9]) AND !laData[4]
        SELECT(lcMasterL)
        lcPrevOrd = ORDER()
        SET ORDER TO Poslnsh
        = SEEK(laData[9])
        lcPo     = SPACE(6)
        llflag   = .F.
        SCAN WHILE SHIPNO = laData[9] FOR TRANCD = '3'
          lcCuttkt = &lcMasterL..PO
          lcStyle =Style
          lcDyelot = IIF(llDyelot,Dyelot,SPACE(10))
          lnPOLine = LineNo
          IF !lfvTkt(.T.)
            llflag = .T.
            LOOP
          ELSE
            llflag = .F.
          ENDIF  
        ENDSCAN
        SET ORDER TO TAG &lcPrevOrd
        SELECT (lcTmpLine)
        lnBrRecNo=RECNO()
        IF llShpUsed
          USE IN ShpmtHdr
          llShpUsed = .F.
        ENDIF
        RETURN
      ENDIF
      SHOW GET laData[9] DISABLE
      SHOW GET ibShp     DISABLE
      SHOW GET laData[4] DISABLE
      CASE lnRespons = 2
        SHOW GET laData[9] DISABLE
        SHOW GET ibShp     DISABLE
        SHOW GET laData[4] DISABLE
      CASE lnRespons = 3
        laData[9] = ''
        SHOW GET laData[9] ENABLE
        SHOW GET ibShp     ENABLE
        SHOW GET laData[4] ENABLE
        _CUROBJ = OBJNUM(laData[9])
    ENDCASE
ENDIF
*SHOW GET laData[9] DISABLE
*SHOW GET ibShp      DISABLE
*SHOW GET laData[4]  DISABLE

IF llShpUsed
  USE IN ShpmtHdr
  llShpUsed = .F.
ENDIF

=lfRefresh(lcWinCh2)


*:*************************************************************
*! Name    : lfvGtShOpn
*! Purpose : Get original open qty.
*! Due to  E301220,1
*:*************************************************************
FUNCTION lfvGtShOpn
PARA llAllLines
PRIVATE lnPointer
*--Get originals.

SELECT (lcTmpLine)
IF llAllLines
  SEEK '1'
ELSE
  *B604344,1 KHM 04/09/2001 (Begin) Add the checking if EOF.
  IF EOF()
    LOCATE
  ENDIF
  *B604344,1 KHM 04/09/2001 (End)
  lnSvRc=RECNO()
ENDIF
SELECT (lcTmpLine)
lcGoAgain   = TranCd+cCarton+Cuttkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine1
= SEEK(lcGoAgain)

SCAN REST WHILE TranCd = '1'
  FOR I=1 TO 8
    Z=STR(I,1)
    REPLACE Qty&Z WITH &lcMasterL..QTY&Z
  ENDFOR
  REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          TotStk WITH TotQty,;
          TotBal WITH TotQty-(TotStk+TotDam+TotCan)
          
  IF !llAllLines
    EXIT
  ENDIF
ENDSCAN
SCAN REST WHILE TranCd = '1'
  lnPointer = RECNO()
  SCATTER MEMVAR MEMO
  M.TRANCD = '2'
  APPEND BLANK
  GATHER MEMVAR MEMO
  FOR I=1 TO 8
    Z=STR(I,1)
    REPLACE Qty&Z WITH &lcMasterL..QTY&Z
  ENDFOR
  REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          TotStk WITH TotQty,;
          TotBal WITH TotQty-(TotStk+TotDam+TotCan)
  GO lnPointer
  IF !llAllLines
    EXIT
  ENDIF
ENDSCAN

SELECT (lcTmpLine)
IF llAllLines

  *B603317,1 Optimize Speed [Begin]
  *GO TOP
  LOCATE
  *B603317,1 Optimize Speed [End  ]

ELSE
  *B604344,1 KHM 04/09/2001 (Begin) Check if the saved pointer is between
  *B604344,1                1 and reccount of file.
  GOTO lnSvRc
  IF BETWEEN(lnSvRc,1,RECCOUNT())
    GOTO lnSvRc
  ENDIF
  *B604344,1 KHM 04/09/2001 (End)  
ENDIF  
RETURN

*B101704,1 [Start]
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture
*-- IF IN PO AND ENTER PO# MANUALLY
IF lcMPara ='I' AND llGenOrNum
  RETURN "@! XXXXXX" 
ELSE
  RETURN "@! X99999"
ENDIF  
*B101704,1 [End]
*!*************************************************************
*! Name      : lfvBatchnm
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Add Lines In temp File for recive lines.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBatchnm()
*!*************************************************************
*E500331
FUNCTION lfvBatchnm

DECLARE laBatchNm[1] && array to get values from browse
STORE '' TO laBatchNm


IF EMPTY(lcBatchNo)
  *E500331,1 Message No.  : 34021.
  *E500331,1 Message TEXT : You cannot leave the "+lcBatchNo+" number empty.
  *E500331,1 Button  No.  : 34000.
  *E500331,1 Button  TEXT : \<OK.
  =gfModalGen('TRM34021B34000','DIALOG','Batch')
   _CUROBJ  = OBJNUM(lcBatchNo)

ENDIF

SELECT CTKTRCVH

*E301480,1 NAD (Start) Use the new variable in the expression. 
*IF llBrowse OR  !SEEK(lcMPara+lcBatchNo)
IF llBrowse OR  !SEEK(lcTypBat+lcBatchNo)
*E301480,1 NAD (End) 

  lcSaveBrow  = lcBrFields
  *-- Set the browse fields.
  lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]

  lcOld_ttl   = lcFile_ttl
  lcFile_ttl  = "Open Temp. Receive Batchs lines"
  *-- call the global browse to browse the available name drops for the current account+store.
  
  *B604298,1 AMH [Start] Fix the browse to display the batches for PO & MF
  *=ARIABROW('',lcFile_ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",;
              "","tmprcvnum","laBatchNm")
    
  =ARIABROW([FOR cType = lcTypBat],lcFile_ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",;
              "","tmprcvnum","laBatchNm")

  *B604298,1 AMH [End]
  
  lcBrFields = lcSaveBrow
  lcFile_ttl = lcOld_ttl
  lcBatchNo  = laBatchNm[1]
  SHOW GET lcBatchNo ENABLE
ENDIF

*E301480,1 NAD (Start) Use the new variable in the expression. 
*IF !EMPTY(lcBatchNo) .AND. SEEK(lcMPara+lcBatchNo)  .AND. cstatus ="A"
IF !EMPTY(lcBatchNo) .AND. SEEK(lcTypBat+lcBatchNo)  .AND. cstatus ="A"
*E301480,1 NAD (End) 

  *E500331,1 Message No.  : 34172.
  *E500331,1 Message TEXT : Cannot add to approved XXXXX .
  *E500331,1 Button  No.  : 34000.
  *E500331,1 Button  TEXT : \<OK.
  =gfModalGen('TRM34172B34000','DIALOG','Batch')
  lcBatchNo = SPACE(6)
   _CUROBJ  = OBJNUM(lcBatchNo)
ENDIF

*B604298,1 AMH Fix the bug of adding lines to posted patchs [Start]
IF !EMPTY(lcBatchNo) .AND. SEEK(lcTypBat+lcBatchNo)  .AND. cstatus ="P"

  *B604298,1 Message No.  : 34179.
  *B604298,1 Message TEXT : Cannot add to Posted Batch.
  *B604298,1 Button  No.  : 34000.
  *B604298,1 Button  TEXT : \<OK.
  =gfModalGen('TRM34179B34000','DIALOG')
  lcBatchNo = SPACE(6)
   _CUROBJ  = OBJNUM(lcBatchNo)
ENDIF
*B604298,1 AMH [End]

IF  laData[4] .AND.  !Carton
  *E500331,1 Message No.  : 34173.
  *E500331,1 Message TEXT : This selected batch is ð  by carton. Cannot proceed.
  *E500331,1 Button  No.  : 34000.
  *E500331,1 Button  TEXT : \<OK.
  =gfModalGen('TRM34173B34000','DIALOG','not')
  lcBatchNo = SPACE(6)
  _CUROBJ  = OBJNUM(lcBatchNo)
ELSE
  IF !laData[4] .AND. Carton
    *E500331,1 Message No.  : 34173.
    *E500331,1 Message TEXT : This selected batch is ð  by carton. Cannot proceed.
    *E500331,1 Button  No.  : 34000.
    *E500331,1 Button  TEXT : \<OK.
    =gfModalGen('TRM34173B34000','DIALOG','')
    lcBatchNo = SPACE(6)
    _CUROBJ  = OBJNUM(lcBatchNo)
  ENDIF

ENDIF

*-- End OF lfvBatchnm.

*!*************************************************************
*! Name      : lpAddBatch
*! Developer : (AMH) Ahmed Maher
*! Date      : 02/19/2001
*! Purpose   : Add Batch for Inter Location POs.
*!*************************************************************
*! Example            :  DO lpAddBatch
*!*************************************************************
*C200152
PROCEDURE lpAddBatch

PRIVATE lcPoFile, llUsePo, lcOrderHdr, lcOrderLn, lcOrdRcvH, lcOrdRcvL, lnAlias, lnCount
PRIVATE lnI, lcForExpr, lcTrancd, lcTmpRcvH, lcTmpRcvL, lcFrist, lcLast, lnSign, lcFrsVend, lcFrsTar
PRIVATE llProcesed
STORE SPACE(0) TO lcRpAddTo, lcForExpr, lcTrancd, lcFrist, lcLast, lcFrsVend, lcFrsTar
llProcesed = .F.
lcTmpRcvH = gfTempName()
lcTmpRcvL = gfTempName()

*-- Declear Source array that be used in Source mover
DECLARE laRpSorSor[1] , laRpTarSor[1]
STORE SPACE(0) TO laRpSorSor , laRpTarSor

*-- Declear Target array that be used in Target mover
DECLARE laRpSorTar[1] , laRpTarTar[1]
STORE SPACE(0) TO laRpSorTar , laRpTarTar

DECLARE laUnAllo[9]
STORE SPACE(0) TO lcRpBatch, lcRpAddTo
STORE 0 TO laUnAllo
llRpAdUnal = .F.
lnAlias = SELECT(0)

SELECT WAREHOUS
LOCATE
lnCount = 0
SCAN
  lnCount = lnCount + 1
  DECLARE laRpSorSor[lnCount], laRpSorTar[lnCount]
  STORE CWARECODE TO laRpSorSor[lnCount], laRpSorTar[lnCount]
ENDSCAN

*-- Call the option grid
llExpr = gfOpGrid('POINTLBT' , .T.)  && Run selection grid.

*-- If the user presses the close button then do nothing.
IF TYPE('llExpr')  = 'L'
  SELECT (lnAlias)
  RETURN
ENDIF

*-- If the user did not select a warehouse in the source option then do not proceed.
IF lcRpAddTo <> 'A' AND EMPTY(laRpTarSor)
  =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Cannot proceed, you must select at least one source location.")
  SELECT (lnAlias)
  RETURN
ENDIF

*-- If the user wants to add to approved batch and did not select a batch#.
IF lcRpAddTo = 'A' 
  IF EMPTY(lcRpBatch)
    =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Cannot proceed, you must select an approved batch.")
    SELECT (lnAlias)
    RETURN
  ELSE 
    SELECT CTKTRCVL
    IF SEEK('N'+lcRpBatch)
      = SEEK('N'+CUTTKT,'POSHDR')
      lcFrsVend = POSHDR.VENDOR
      lcFrsTar = CWARECODE
      SCAN REST WHILE cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+STR(nLineno,6)+;
                      STR(Lineno,6)+Trancd = 'N'+lcRpBatch
        = SEEK('N'+CUTTKT,'POSHDR')
        IF POSHDR.VENDOR <> lcFrsVend .OR. CWARECODE <> lcFrsTar
          =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Cannot proceed, the selected batch has multiple source/target.")
          SELECT (lnAlias)
          RETURN
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDIF

SELECT CTKTRCVH
=AFIELDS(laFStru)
=gfCrtTmp(lcTmpRcvH,@laFStru,'Vendor+cWareCode+cType+TmpRcvNum',lcTmpRcvH)

SELECT CTKTRCVL
=AFIELDS(laFStru)
=gfCrtTmp(lcTmpRcvL,@laFStru,;
          'cType+TmpRcvNum+Cuttkt+Style+Dyelot+STR(nLineno,6)+STR(Lineno,6)+Trancd',lcTmpRcvL)

SELECT CTKTRCVH
lcOrdRcvH = SET('ORDER')
SET ORDER TO TAG CTKTRCVB

SELECT CTKTRCVL
lcOrdRcvL = SET('ORDER')
SET ORDER TO TAG CTKTRCVLC

SELECT POSLN
lcOrderLn = SET('ORDER')
SET ORDER TO TAG POSLNW

SELECT POSHDR
lcOrderHdr = SET('ORDER')
SET ORDER TO TAG POSHDRV

IF lcRpAddTo = 'A'
  DECLARE laRpTarSor[1], laRpTarTar[1]
  laRpTarTar[1] = lcFrsTar
  laRpTarSor[1] = lcFrsVend
ENDIF

IF !EMPTY(laRpTarTar)
  lnI = 0
  FOR lnI = 1 TO ALEN(laRpTarTar)
    lcForExpr = lcForExpr + '|' + PADR(laRpTarTar[lnI],6)
  ENDFOR
ENDIF
lnSequence = 1
lnI = 0
lcTrancd = '16'
FOR lnI = 1 TO ALEN(laRpTarSor)
  IF SEEK(PADR(laRpTarSor[lnI],8)+'N')
    SCAN REST WHILE VENDOR+CSTYTYPE+PO = PADR(laRpTarSor[lnI],8)+'N';
         FOR EVALUATE(llExpr)
      IF SEEK('N'+POSHDR.PO,'POSLN')
        SELECT POSLN
        SCAN REST WHILE CSTYTYPE+PO+CWARECODE = 'N'+POSHDR.PO;
             FOR IIF(EMPTY(laRpTarTar),.T.,CWARECODE$lcForExpr) .AND. TRANCD $ lcTrancd;
                 .AND. REFERENCE <> 'HOLD'

          *C200170,1 AMH Don't add issued or posted batches [Start]
          IF SEEK('N'+PO+STYLE+DYELOT+STR(LINENO,6),'CTKTRCVL') .AND.;
             SEEK(PADR(POSHDR.VENDOR,8)+PADR(POSLN.CWARECODE,6)+'N'+CTKTRCVL.TMPRCVNUM,'CTKTRCVH');
             .AND. CTKTRCVH.CSTATUS $ 'IP'
            LOOP
          ENDIF
          *C200170,1 AMH [End]

          IF lcRpAddTo = 'A'
            IF SEEK('N'+POSLN.PO+POSLN.STYLE+POSLN.DYELOT+STR(POSLN.LINENO,6)+lcRpBatch,'CTKTRCVL')
              LOOP
            ENDIF
          ELSE
            IF SEEK('N'+PO+STYLE+DYELOT+STR(LINENO,6),'CTKTRCVL') .AND.;
               SEEK(PADR(POSHDR.VENDOR,8)+PADR(POSLN.CWARECODE,6)+'N'+CTKTRCVL.TMPRCVNUM,'CTKTRCVH');
               .AND. CTKTRCVH.CSTATUS $ 'OA'
              *C200152,4 AMH Fix duplicated data when add to open batch [Start]
              lcTmpRcvNo = CTKTRCVL.TMPRCVNUM
              *C200152,4 AMH [End]
              IF lcRpAddTo = 'N' .OR.;
                 (lcRpAddTo = 'O' .AND. CTKTRCVH.CSTATUS = 'A' .AND. !llRpAdUnal)
                LOOP
              ENDIF
              *C200152,4 AMH Fix duplicated data when add to open batch [Start]
              IF lcRpAddTo = 'O' .AND. CTKTRCVH.CSTATUS = 'O'
                SELECT CTKTRCVH
                SET ORDER TO TAG CTKTRCVB DESCENDING
                SEEK(PADR(POSHDR.VENDOR,8)+PADR(POSLN.CWARECODE,6)+'N')
                LOCATE REST WHILE VENDOR+CWARECODE+CTYPE+TMPRCVNUM =;
                            PADR(POSHDR.VENDOR,8)+PADR(POSLN.CWARECODE,6)+'N' FOR CSTATUS = 'O'
                IF FOUND() .AND. lcTmpRcvNo <> TMPRCVNUM
                  SELECT CTKTRCVH
                  SET ORDER TO TAG CTKTRCVB ASCENDING
                  SELECT POSLN
                  LOOP
                ENDIF
                SELECT CTKTRCVH
                SET ORDER TO TAG CTKTRCVB ASCENDING
                SELECT POSLN
              ENDIF
              *C200152,4 AMH [End]
              IF lcRpAddTo = 'O' .AND. CTKTRCVH.CSTATUS = 'A' .AND. llRpAdUnal
                laUnAllo[1] = CTKTRCVL.QTY1
                laUnAllo[2] = CTKTRCVL.QTY2
                laUnAllo[3] = CTKTRCVL.QTY3
                laUnAllo[4] = CTKTRCVL.QTY4
                laUnAllo[5] = CTKTRCVL.QTY5
                laUnAllo[6] = CTKTRCVL.QTY6
                laUnAllo[7] = CTKTRCVL.QTY7
                laUnAllo[8] = CTKTRCVL.QTY8
                laUnAllo[9] = CTKTRCVL.TOTQTY
              ENDIF
            ENDIF
          ENDIF
          IF !SEEK(PADR(laRpTarSor[lnI],8)+PADR(CWARECODE,6),lcTmpRcvH)
            =lfAddNewH()
          ENDIF
          IF !SEEK('N'+EVALUATE(lcTmpRcvH+'.TMPRCVNUM')+POSLN.PO+POSLN.STYLE+POSLN.DYELOT+STR(POSLN.LINENO,6),lcTmpRcvL)
            =lfAddNewL()
          ENDIF
          SELECT (lcTmpRcvL)
          lnSign = IIF(POSLN.TRANCD='1',1,IIF(llRpAdUnal,0,-1))
          REPLACE QTY1   WITH QTY1+(POSLN.QTY1*lnSign)-laUnAllo[1],;
                  QTY2   WITH QTY2+(POSLN.QTY2*lnSign)-laUnAllo[2],;
                  QTY3   WITH QTY3+(POSLN.QTY3*lnSign)-laUnAllo[3],;
                  QTY4   WITH QTY4+(POSLN.QTY4*lnSign)-laUnAllo[4],;
                  QTY5   WITH QTY5+(POSLN.QTY5*lnSign)-laUnAllo[5],;
                  QTY6   WITH QTY6+(POSLN.QTY6*lnSign)-laUnAllo[6],;
                  QTY7   WITH QTY7+(POSLN.QTY7*lnSign)-laUnAllo[7],;
                  QTY8   WITH QTY8+(POSLN.QTY8*lnSign)-laUnAllo[8],;
                  TOTQTY WITH TOTQTY+(POSLN.TOTQTY*lnSign)-laUnAllo[9]
          IF TOTQTY <= 0
            DELETE
          ENDIF
          SELECT (lcTmpRcvH)
          *C200152,5 Correct NTOTSTK calculation [Start]
          *REPLACE NTOTSTK WITH NTOTSTK + MAX(EVALUATE(lcTmpRcvL+'.TOTQTY'),0)
          REPLACE NTOTSTK WITH NTOTSTK + (POSLN.TOTQTY*lnSign) - laUnAllo[9]
          IF NTOTSTK <= 0
            DELETE
          ENDIF
          *C200152,5 [End]
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
ENDFOR

SELECT (lcTmpRcvH)
LOCATE
IF EOF()
  IF lcRpAddTo $ 'NO'
    =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"No batches have been processed.")
  ELSE
    =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Batch # "+lcRpBatch+" has not been processed.")
  ENDIF
ELSE
  lnNewBatch = 0
  SCAN
    SCATTER MEMVAR
    SELECT CTKTRCVH
    IF !SEEK(m.Vendor+m.cWareCode+'N'+m.TmpRcvNum)
      APPEND BLANK
      lnNewBatch = lnNewBatch + 1
      DECLARE laNewBatch[lnNewBatch,2]
      laNewBatch[lnNewBatch,1] = m.TmpRcvNum
      m.TmpRcvNum = gfSequence('TMPRCVNUM','','','')
      laNewBatch[lnNewBatch,2] = m.TmpRcvNum
    ENDIF
    GATHER MEMVAR
    lcFrist = IIF(EMPTY(lcFrist),TMPRCVNUM,lcFrist)
    lcLast  = TMPRCVNUM
  ENDSCAN
  SELECT (lcTmpRcvL)
  LOCATE
  SCAN
    SCATTER MEMVAR
    SELECT CTKTRCVL
    IF !SEEK('N'+m.Cuttkt+m.Style+m.Dyelot+STR(m.nLineNo,6)+m.TmpRcvNum)
      APPEND BLANK
      IF TYPE('laNewBatch') # 'U' .AND. ASCAN(laNewBatch,m.TmpRcvNum) <> 0
        m.TmpRcvNum = laNewBatch[ASUBSCRIPT(laNewBatch,ASCAN(laNewBatch,m.TmpRcvNum),1),2]
      ENDIF
    ENDIF
    IF !llProcesed
      lnI = 0
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        IF EVALUATE('QTY'+lcI) <> EVALUATE('m.QTY'+lcI)
          llProcesed = .T.
          EXIT
        ENDIF
      ENDFOR
    ENDIF
    GATHER MEMVAR
  ENDSCAN
  IF llProcesed
    IF lcRpAddTo $ 'NO'
      =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Batches from "+lcFrist+" to "+lcLast+" have been processed.")
    ELSE
      =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Batch # "+lcFrist+" has been processed.")
    ENDIF
  ELSE
    IF lcRpAddTo $ 'NO'
      =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"No batches have been processed.")
    ELSE
      =gfModalGen("TRM00000B00000","DIALOG","Tool",.F.,"Batch # "+lcRpBatch+" has not been processed.")
    ENDIF
  ENDIF
ENDIF

SELECT POSHDR
SET ORDER TO &lcOrderHdr.
SELECT POSLN
SET ORDER TO &lcOrderLn.
SELECT CTKTRCVL
SET ORDER TO &lcOrdRcvL.
SELECT CTKTRCVH
SET ORDER TO &lcOrdRcvH.
SELECT (lnAlias)
glQuitting = .T.  && Rise quit flag because it's modal screen.
*-- end of lpAddBatch.

*!*************************************************************
*! Name      : lfvSource
*! Developer : (AMH) Ahmed Maher
*! Date      : 02/19/2001
*! Purpose   : Valid of Source Location field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSource()
*!*************************************************************
*C200152
FUNCTION lfvSource

= gfMover(@laRpSorSor,@laRpTarSor,'Select Source',.T.,'')  && call mover function.
*-- end of lfvSource.

*!*************************************************************
*! Name      : lfvTarget
*! Developer : (AMH) Ahmed Maher
*! Date      : 02/19/2001
*! Purpose   : Valid of Target Location field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTarget()
*!*************************************************************
*C200152
FUNCTION lfvTarget

= gfMover(@laRpSorTar,@laRpTarTar,'Select Target',.T.,'')  && call mover function.
*-- end of lfvTarget.

*!*************************************************************
*! Name      : lfvAddTo
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/07/2001
*! Purpose   : Valid of Add to Popup.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAddTo()
*!*************************************************************
*C200152
FUNCTION lfvAddTo

CLEAR READ
*-- end of lfvAddTo.

*!*************************************************************
*! Name      : lfvBatNum
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/07/2001
*! Purpose   : Valid of Batch # field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBatNum()
*!*************************************************************
*C200152
FUNCTION lfvBatNum

PRIVATE lcBrFields , lcHdrFlt

SELECT CTKTRCVH
IF !EMPTY(lcRpBatch) AND !SEEK('N'+lcRpBatch,'CTKTRCVH')

  DIMENSION laTempData[1]
  STORE '' TO laTempData
  lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]

  lcHdrFlt = FILTER()
  SET FILTER TO
  
  =gfBrows(['N' FOR CSTATUS='A'],'TMPRCVNUM','laTempData','Temp. Receive Batchs')
   
  SET FILTER TO &lcHdrFlt

  lcRpBatch = laTempData[1]
  IF EMPTY(lcRpBatch)
    _CUROBJ = OBJNUM(lcRpBatch)
    RETURN
  ENDIF  

ENDIF
*-- end of lfvBatNum.

*!*************************************************************
*! Name      : lfAddNewH
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/07/2001
*! Purpose   : Add New Record to lcTmpRcvH
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddNewH()
*!*************************************************************
*C200152
FUNCTION lfAddNewH

SELECT CTKTRCVH
SET ORDER TO TAG CTKTRCVB DESCENDING
IF lcRpAddTo = 'O' .AND. SEEK(PADR(POSHDR.VENDOR,8)+PADR(POSLN.CWARECODE,6)+'N')
  LOCATE REST WHILE VENDOR+CWARECODE+CTYPE+TMPRCVNUM =;
                    PADR(POSHDR.VENDOR,8)+PADR(POSLN.CWARECODE,6)+'N' FOR CSTATUS = 'O'
  IF FOUND()
    SCATTER MEMVAR
    SELECT (lcTmpRcvH)
    APPEND BLANK
    GATHER MEMVAR
  ELSE
    SELECT (lcTmpRcvH)
    APPEND BLANK
    REPLACE CTYPE     WITH 'N',;
            TMPRCVNUM WITH '*'+STR(lnSequence,5),;
            CSTATUS   WITH 'O',;
            NTOTSTK   WITH 0,;
            NTOTDAM   WITH 0,;
            NTOTCAN   WITH 0,;
            DDATE     WITH gdSysDate,;
            Vendor   WITH PADR(laRpTarSor[lnI],8),;
            cWareCode WITH POSLN.CWARECODE
    lnSequence = lnSequence + 1
  ENDIF
ELSE
  IF lcRpAddTo = 'A'
    SELECT CTKTRCVH
    SET ORDER TO TAG CTKTRCVH
    IF SEEK('N'+lcRpBatch)
      SCATTER MEMVAR
      SELECT (lcTmpRcvH)
      APPEND BLANK
      GATHER MEMVAR
    ENDIF
    SELECT CTKTRCVH
    SET ORDER TO TAG CTKTRCVB DESCENDING
  ELSE
    SELECT (lcTmpRcvH)
    APPEND BLANK
    REPLACE CTYPE     WITH 'N',;
            TMPRCVNUM WITH '*'+STR(lnSequence,5),;
            CSTATUS   WITH 'O',;
            NTOTSTK   WITH 0,;
            NTOTDAM   WITH 0,;
            NTOTCAN   WITH 0,;
            DDATE     WITH gdSysDate,;
            Vendor   WITH PADR(laRpTarSor[lnI],8),;
            cWareCode WITH POSLN.CWARECODE
    lnSequence = lnSequence + 1
  ENDIF
ENDIF
SELECT CTKTRCVH
SET ORDER TO TAG CTKTRCVB ASCENDING
*-- end of lfAddNewH.

*!*************************************************************
*! Name      : lfAddNewL
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/07/2001
*! Purpose   : Add New Record to lcTmpRcvL
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddNewL()
*!*************************************************************
*C200152
FUNCTION lfAddNewL

IF lcRpAddTo <> 'N' .AND.;
   SEEK('N'+POSLN.PO+POSLN.STYLE+POSLN.DYELOT+STR(POSLN.LINENO,6)+EVALUATE(lcTmpRcvH+'.TMPRCVNUM'),'CTKTRCVL')
  SELECT (lcTmpRcvH)
  REPLACE NTOTSTK WITH NTOTSTK - CTKTRCVL.TOTQTY
  SELECT CTKTRCVL
  SCATTER MEMVAR
  SELECT (lcTmpRcvL)
  APPEND BLANK
  GATHER MEMVAR FIELDS CTYPE,TMPRCVNUM,CUTTKT,STYLE,DYELOT,CWARECODE,LINENO,TRANCD,NLINENO,FLAG
ELSE
  SELECT (lcTmpRcvL)
  APPEND BLANK
  REPLACE CTYPE     WITH 'N',;
          TMPRCVNUM WITH EVALUATE(lcTmpRcvH+'.TMPRCVNUM'),;
          CUTTKT    WITH POSLN.PO,;
          STYLE     WITH POSLN.STYLE,;
          DYELOT    WITH POSLN.DYELOT,;
          CWARECODE WITH POSLN.CWARECODE,;
          LINENO    WITH 0,;
          TRANCD    WITH '2',;
          NLINENO   WITH POSLN.LINENO;
          FLAG      WITH 'A'
ENDIF
*-- end of lfAddNewL.
*!**************************************************************************
*! Name      : lfChckScal
*! Developer : Mohamed shokry (MHM)
*! Date      : 07/19/2001
*! Purpose   : Function to check damage And secound Quality style 
*!**************************************************************************
*! Parameters: lcStyRet1 : Damage or 2nd Qualtity Style
*!             lnStyNo   : 1--> 2nd Quality 2--> Damage.
*!**************************************************************************
*! Example   :  =lfChckScal()
*!**************************************************************************
*B604677,1 
FUNCTION lfChckScal
PARAMETER lcStyRet1 , lnStyNo
PRIVATE lcCurAls , lnCurRec ,lnStyScal , lnCurRcSty

lcCurAls = SELECT()
SELECT SCALE
lnCurRec = RECNO()
SELECT STYLE
lnCurRcSty = RECNO()
=SEEK(lcStyRet1 ,'STYLE')
=SEEK('S'+STYLE.Scale,'SCALE')
lnScalCnt = SCALE.CNT
SELECT SCALE
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF  
IF !lfChkScal(STYLE.Scale,PrePak)
  FOR lnCount = 1 TO 8 
    IF lnCount =< lnScalCnt 
      IF lnStyNo = '1'
        SHOW GET laDam1[lnCount] ENABLE
      ELSE
        SHOW GET laDam2[lnCount] ENABLE
      ENDIF  
    ELSE
      IF lnStyNo = '1'
        SHOW GET laDam1[lnCount] DISABLE
      ELSE
        SHOW GET laDam2[lnCount] DISABLE
      ENDIF  
    ENDIF
  ENDFOR
ENDIF
SELECT STYLE
IF BETWEEN(lnCurRcSty,1,RECCOUNT())
  GOTO lnCurRcSty 
ENDIF  

SELECT(lcCurAls)

*!**************************************************************************
*! Name      : lfWMfScr
*! Developer : Mohamed shokry (MHM)
*! Date      : 07/22/2001
*! Purpose   : When Function for line quantity screen
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfWMfScr()
*!**************************************************************************
*B604677,1 
FUNCTION lfWMfScr

IF EMPTY(lcRetSty1)  AND EMPTY(lcRetSty2) 
  lcCurAls = SELECT()
  *SELECT (lcTmpLine)
  *LOCATE
  SELECT STYLE
  lnCurRcSty = RECNO()
  lcOldFltr = FILTER()
  SET FILTER TO  
ENDIF

IF !EMPTY(lcRetSty1) 
  =lfChckScal(lcRetSty1,'1')
ELSE
  =SEEK(m.Style,'STYLE')
  IF !EMPTY(STYLE.cretsty)    
    =lfChckScal(STYLE.cretsty,'1')
  ENDIF
ENDIF  

IF !EMPTY(lcRetSty2) 
  =lfChckScal(lcRetSty2,'2')
ELSE
  =SEEK(m.Style,'STYLE')
  IF !EMPTY(STYLE.cretsty2)
    =lfChckScal(STYLE.cretsty2,'2')
  ENDIF
ENDIF  

IF EMPTY(lcRetSty1)  AND EMPTY(lcRetSty2) 
  SELECT STYLE
  IF BETWEEN(lnCurRcSty,1,RECCOUNT())
    GOTO lnCurRcSty 
  ENDIF  
  SET FILTER TO &lcOldFltr 
  SELECT(lcCurAls)
ENDIF
*:*************************************************************
*! Name     : lfChkScal
*! Developer: Mohamed Shokry - (MHM)
*! Date     : 07/22/2001
*! Purpose  : Chek if the style and recieve to cancel/damage have the same scale. 
*:*************************************************************
*B604677,1 
FUNCTION lfChkScal
PARAMETER lcCurScl,lcCurrPre

PRIVATE lnOldAls,lnToRet,lcOldKey,lnSizCnt
lnSizCnt = 0
lnOldAls = SELECT(0)
SELECT SCALE
lnToRet = .T.
lcOldKey = Type+Scale+PrePak
FOR lnInd = 1 TO 8
  lnSizCnt = IIF(laOrg[lnInd]>0,lnSizCnt+1,lnSizCnt)
ENDFOR
=SEEK('S'+lcCurScl+lcCurrPre)
IF lnSizCnt > cNt
  lnToRet = .F.
ENDIF
=SEEK(lcOldKey)
SELECT(lnOldAls)
RETURN lnToRet

