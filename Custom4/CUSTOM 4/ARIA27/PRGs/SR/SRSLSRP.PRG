*:************************************************************************
*: Program file  : SRSLSREP.PRG
*: Program desc. : Sales Representatives Maintenance
*: For screen    : SRSLSRP 
*:         System: ARIA 2.7
*:         Module: SR
*:      Developer: MAN - Mohamed Abdel Salam 
*:************************************************************************
*: Calls :
*:         Procedures : 
*:						lpShow
*:						lpSavScr
*:						lpOptions 
*:						lpDelScr
*:         Functions  : 
*:						lfvRep
*:						lfvPayType
*:						lfwRange
*:						lfvRange
*:						lfvStates 
*:						lfOptions 
*:						lfvNotes 
*:						lfvAccount 
*:						lfvOrders 
*:						lfvCommiss 
*:						lfActBrows 
*:						lfvDivCls 
*:						lfTermnat 
*:						lfvDel 
*:						lfvIns 
*:						lfScrCircl 
*:						lfPushKey 
*:						lfvCom 
*:						lfwCom 
*:						lfBrowse 
*:						lfDivShow 
*:						lfvDvsion 
*:						lpSavScr 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Example : DO srSlsRp
*:*************************************************************
*:E300694,1 MAN 06/22/97 Convert the program to work under the 
*:                       Aria 2.7 Version
*:E300872,1 YMA 04/30/98 Adsded to update the trace file upon
*:E300872,1              saving.
*:E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*:                           as it's now controlled globally
*:B802206,1 BWA 05/03/1999 Fix the bug of not saving the caddress3,
*:B802206,1 BWA            caddress4 , caddress5
*:C200071,1 BWA 05/10/1999 Add a new fields named(comm) and another field(name Comm_rate)
*:C200071,1 BWA            in the salesrep field and it`s name in the screen is
*:C200071,1 BWA            split commission and default commission    
*:B602867,1 BWA 05/20/1999 Fix the bug of the link to from the options menu
*:B602548,1 ADEL 03/08/2000 Fix the bug of not deleting a sales rep. having cancelled orders.
*:B803745,1 MHM 10/25/2000  Fix the bug of divisions description for Salesreps does not work
*:E500433,1 AME 09/11/2001 ADD email address field to the salesrep screen and table
*:C200597,1 TMI 08/19/2004 Call a customized tracekey function for Bong Wha
*:B123036,1 NNA 09/21/2004 Fix bug that the State code filed always browse even the code is valid
*:B038431,1 ASH 10/05/2004 1 - Don't open the division screen if Salesrep commission not based on division.
*:B038431,1 ASH            2 - Fix bug that if we browse in the codes file like (State Field in the SR Screen)
*:B038431,1 ASH                we'll find that the Soft Seek not working
*:**********************************************************************************************

DECLARE laKeyField[1,4], laCodInfo[3,10], laTerms[1], laRegion[1], laDiv[1,2], laTmpStruc[1],;
        laPayType[2,2] 

lcdiv =''
*llNoShow = .T.
laDefProc[7]  = .F.     && Void procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)

laTmpStruc = " "
laPayType[1,1] = "Running Balance"
laPayType[1,2] = "R"
laPayType[2,1] = "Manual"
laPayType[2,2] = "M"

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'SALESREP'
laKeyField[1,4] = 1

laCodInfo[1,01] = "CTERMCODE" 	
laCodInfo[1,02] = "laTerms"     
laCodInfo[1,03] = "lnTerms"     
laCodInfo[1,04] = ""            
laCodInfo[1,05] = .F.           
laCodInfo[1,06] = .F.           
laCodInfo[1,07] = "SALESREP"    
laCodInfo[1,08] = "SALESREP"    
laCodInfo[1,09] = "laData[1]"   
laCodInfo[1,10] = "CTERMCODE"   

laCodInfo[2,01] = "REGION"
laCodInfo[2,02] = "laRegion"
laCodInfo[2,03] = "lnRegion"
laCodInfo[2,04] = ""
laCodInfo[2,05] = .F.
laCodInfo[2,06] = .F.
laCodInfo[2,07] = "SALESREP"    
laCodInfo[2,08] = "SALESREP"	
laCodInfo[2,09] = "laData[1]" 	
laCodInfo[2,10] = "Region"		

laCodInfo[3,01] = "CDIVISION" 	
laCodInfo[3,02] = "laDiv"     
laCodInfo[3,03] = "lnDiv"     
laCodInfo[3,04] = ""            
laCodInfo[3,05] = .F.           
laCodInfo[3,06] = .F.           
laCodInfo[3,07] = ""    
laCodInfo[3,08] = ""    
laCodInfo[3,09] = ""   
laCodInfo[3,10] = ""   
lnRegion=1
lnDiv=1
lnTerms=1
*:E301176,1 HDM [Start]03/22/1999 Prevent programs from displaying notepad icon
*:                           as it's now controlled globally

*DIMENSION laPanelObj(6,3)    && Array to add new buttons to the control panel
DIMENSION laPanelObj(5,3)    && Array to add new buttons to the control panel

*laPanelObj[1,1] = 'pbNotes'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.bmp'
*laPanelObj[1,3] = 'VALID lfvNotes()' 

*laPanelObj[2,1] = 'pbOrder'
*laPanelObj[2,2] = gcBmpHome+'orders.bmp'
*laPanelObj[2,3] = 'VALID lfvOrders()' 

*laPanelObj[3,1] = 'pbComm'
*laPanelObj[3,2] = gcBmpHome+'comm.bmp'
*laPanelObj[3,3] = 'VALID lfvCommiss()'

*laPanelObj[4,1] = 'pbAct'
*laPanelObj[4,2] = gcBmpHome+'accts.bmp'
*laPanelObj[4,3] = 'VALID lfvAccount()'

*laPanelObj[5,1] = 'pbDiv'
*laPanelObj[5,2] = gcBmpHome+'division.bmp'
*laPanelObj[5,3] = 'VALID lfvDvsion()'

*laPanelObj[6,1] = 'pbObj'
*laPanelObj[6,2] = gcBmpHome+'relate.bmp'
*laPanelObj[6,3] = 'VALID GetObj("P",laData[1])'

laPanelObj[1,1] = 'pbOrder'
laPanelObj[1,2] = gcBmpHome+'orders.bmp'
laPanelObj[1,3] = 'VALID lfvOrders()' 

laPanelObj[2,1] = 'pbComm'
laPanelObj[2,2] = gcBmpHome+'comm.bmp'
laPanelObj[2,3] = 'VALID lfvCommiss()'

laPanelObj[3,1] = 'pbAct'
laPanelObj[3,2] = gcBmpHome+'accts.bmp'
laPanelObj[3,3] = 'VALID lfvAccount()'

laPanelObj[4,1] = 'pbDiv'
laPanelObj[4,2] = gcBmpHome+'division.bmp'
laPanelObj[4,3] = 'VALID lfvDvsion()'

laPanelObj[5,1] = 'pbObj'
laPanelObj[5,2] = gcBmpHome+'relate.bmp'
laPanelObj[5,3] = 'VALID GetObj("P",laData[1])'

*:E301176,1 HDM [End]


STORE .F. TO llBrowse , llCommWhen , llDIV_COMM

STORE " " TO lcScFields, lcOldRep   , lcSSN1    , lcSSN2 , lcSSN3 ,;
             lcOldTerm  , lcOldRegn , lcPayType , lcTempDiv , lcObjStat, lcAddHed1

STORE 0   TO lcCurrent , lcAge30 , lcAge60 , lcAge90 , lcAge120 , lcBALANCE ,;
             lnOldVal , puPayType ,lnDivRec, lnDivRec

lcKey      = gcBmpHome + "ExtKey.BMP"

lnLastMode = 1
IF ! gfSetup()
  RETURN
ENDIF
llNoShow=.F.
IF !WEXIST(gcBaseWind)
  lnLastMode = 1
  *lcScFields = "REPCODE,NAME,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6,;
                CITY,STATE,ZIP,PHONE,FAX,SSN,REGION,CTERMCODE,DISCNT1,;
                REDCOM1,DISCNT2,REDCOM2,DISCNT3,REDCOM3,DISCNT4,;
                REDCOM4,PAY_TYPE,cCont_Code"
                
  *B802206,1 BWA Replace cAddress3,cAddress4,cAddress5 fields by age30,age60,age90 
  *B802206,1 BWA not to duplicate fields
  *lcScFields = "REPCODE,NAME,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6,;
                cAddress3,cAddress4,cAddress5,PHONE,FAX,SSN,REGION,CTERMCODE,DISCNT1,;
                REDCOM1,DISCNT2,REDCOM2,DISCNT3,REDCOM3,DISCNT4,"
  lcScFields = "REPCODE,NAME,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6,;
                age30,age60,age90,PHONE,FAX,SSN,REGION,CTERMCODE,DISCNT1,;
                REDCOM1,DISCNT2,REDCOM2,DISCNT3,REDCOM3,DISCNT4,"

  *B802206,1 BWA [END]
  
  *C200071,1 BWA  add the Field (Comm) to the screen fields.
  *lcScFields =  lcScFields + "REDCOM4,PAY_TYPE,cCont_Code"                
  *lcScFields =  lcScFields + "REDCOM4,PAY_TYPE,cCont_Code,Comm"
  *E500433,1 AME [Start] add the email address field
  *lcScFields =  lcScFields + "REDCOM4,PAY_TYPE,cCont_Code,Comm,COMM_RATE"
  lcScFields =  lcScFields + "REDCOM4,PAY_TYPE,cCont_Code,Comm,COMM_RATE,CEMAIL_ADD"
  *E500433,1 AME [End]
  *C200071,1 BWA [END]

  SCATTER FIELDS &lcScFields TO laData BLANK

  lnCurObj   = 1            && Var. hold the current object no.
  lcTempDiv  = gfTempName() && Temp file to hold division commissions.
  llCommWhen = .T.          && Flag to prevent entering the commission before the division.
  lcObjStat  = "DISABLE"
  lcAddHed1  = PROP(LOOKUP(SYCINT.CPARt1LAB,ALLTRIM(gcContCode),sycint.ccont_code,'CCONTCODE'))
  llDIV_COMM = gfGetMemVar('M_REP_COMM')='D'
  IF llDIV_COMM
    SELECT REP_DIV
    =AFIELDS(laTmpStruc)
    CREATE CURSOR (lcTempDiv) FROM ARRAY laTmpStruc
    INDEX ON REPCODE+CDIVISION TAG &lcTempDiv  OF (gcWorkDir+lcTempDiv)
  ENDIF
ENDIF



SELECT SALESREP

DO (gcScrDir + gcWinAppl + '\SRSLSRP.SPR')

IF glQuitting 
  IF USED(lcTempDiv)
    USE IN (lcTempDiv)
  ENDIF
  RELEASE PAD _OPTIONS OF _MSYSMENU
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Local Show for srSlsRp Screen
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             				gfwCodePop
*!             Functions  :   
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None  
*!*************************************************************
*! Example            : =lpShow ()
*!*************************************************************
*
PROCEDURE lpShow
SELECT SALESREP


DO CASE
  CASE laScrMode[1]
    lnLastMode = 1 
    IF llDIV_COMM
      SELECT (lcTempDiv)
      ZAP
    ENDIF

    STORE " " TO lcOldRep , lcSSN1 , lcSSN2 , lcSSN3 ,;
                 lcOldTerm  , lcOldRegn  , lcPayType
    STORE 0   TO lcCurrent , lcAge30 , lcAge60 , lcAge90 , lcAge120 , lcBALANCE ,;
                 lnOldVal , puPayType

    =gfwCodePop ( @laCodInfo, "CTERMCODE" ,"N" )
    =gfwCodePop ( @laCodInfo, "REGION"    ,"N" )
  
    SHOW GET pbComm  DISABLE
    SHOW GET pbAct   DISABLE
    SHOW GET pbDiv   DISABLE
    SHOW GET pbObj   DISABLE
    SHOW GET pbOrder DISABLE
    SHOW GET pbNotes DISABLE
    laData[26] = gcContcode 
    lcAddHed1  = PROP(LOOKUP(SYCINT.CPARt1LAB,laData[26],sycint.ccont_code,'CCONTCODE')) 
    laData[8]  = PROP(LOOKUP(SYCINT.CCONT_DESC,laData[26],sycint.ccont_code,'CCONTCODE')) 
  CASE laScrMode[2]
    lnLastMode = 2
    lcPayType  = IIF(AT(laData[25],"RM")>0,laPayType[AT(laData[25],"RM"),1]," ")
    puPayType  = AT(laData[25],"RM")
    lcCurrent  = CURRENT
    lcAge30    = AGE30
    lcAge60    = AGE60 
    lcAge90    = AGE90
    lcAge120   = AGE120
    lcBALANCE  = BALANCE
  
    lcSSN1     = SUBSTR(laData[14],1,3)
    lcSSN2     = SUBSTR(laData[14],4,2)
    lcSSN3     = SUBSTR(laData[14],6,4)

    IF llDIV_COMM
      SELECT REP_DIV
      USE
      SELECT (lcTempDiv)
      ZAP
      APPEND FROM (gcDataDir+"REP_DIV") FOR REPCODE = laData[1]
      lnDivRec = RECCOUNT(lcTempDiv)
    
      SELECT 0
      USE (gcDataDir+"REP_DIV") SHARED
      SET ORDER TO TAG REP_DIV
      SELECT SALESREP
      SHOW GET pbDiv ENABLE 
    ELSE
      SHOW GET pbDiv DISABLE  
    ENDIF
    =gfwCodePop ( @laCodInfo, "CTERMCODE" ,"T" )
    =gfwCodePop ( @laCodInfo, "REGION"    ,"T" )
  
    SHOW GET pbObj   ENABLE
    SHOW GET pbAct   ENABLE
    SHOW GET pbComm  ENABLE  
    SHOW GET pbOrder ENABLE
    SHOW GET pbNotes ENABLE

  CASE laScrMode[3] 
    lnLastMode = 3
    *B038431,1 ASH 10/05/2004 (Begin) Don't open the division screen if Salesrep commission not based on division.
    *SHOW GET pbDiv ENABLE
    IF llDIV_COMM
      SHOW GET pbDiv ENABLE
    ELSE
      SHOW GET pbDiv DISABLE  
    ENDIF
    *B038431,1 ASH 10/05/2004 (End)
    SHOW GET pbComm ENABLE 
    SHOW GET pbAct  ENABLE
    SHOW GET pbObj  ENABLE 
    SHOW GET pbOrder ENABLE
    SHOW GET pbNotes ENABLE

  CASE laScrMode[4] .AND. lnLastMode <> 4
    lnLastMode = 4
    STORE " " TO lcOldRep , lcSSN1 , lcSSN2 , lcSSN3 ,;
                 lcOldTerm  , lcOldRegn
    STORE 0   TO lcCurrent , lcAge30 , lcAge60 , lcAge90 , lcAge120 , lcBALANCE ,;
                 lnOldVal
    puPayType  = 1
    lcPayType  = laPayType[1,1]
    laData[25] = laPayType[1,2]
  
    IF llDIV_COMM
      SELECT (lcTempDiv)
      ZAP
      lnDivRec = 0
      SELECT SALESREP 
    ENDIF
    =gfwCodePop ( @laCodInfo, "CTERMCODE" ,"D" )
    =gfwCodePop ( @laCodInfo, "REGION"    ,"D" )
    laData[16]=laTerms[lnTerms,2]
    laData[15]=laRegion[lnRegion,2]
    IF llDIV_COMM
      SHOW GET pbDiv   ENABLE  
    ELSE
      SHOW GET pbDiv   DISABLE
    ENDIF
    SHOW GET pbComm  DISABLE 
    SHOW GET pbAct   DISABLE
    SHOW GET pbObj   DISABLE
    SHOW GET pbOrder DISABLE
    SHOW GET pbNotes ENABLE
  
ENDCASE

lcAddHed1 = PROP(LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[26]),ALLTRIM(gcContCode),laData[26]),sycint.ccont_code,'CCONTCODE'))
SHOW GET lcAddHed1

*!*************************************************************
*! Name      : lfvRep
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Valid function for the get field laData[1]
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!							gfSeekRec
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvRep()
*!*************************************************************
*
FUNCTION lfvRep
*PRIVATE lcCurAlias

SELECT SALESREP
IF ATC("?",laData[1]) > 0
  =gfBrows()  
ELSE
  IF !EMPTY(laData[1]) .AND. LASTKEY()= 13 
    =gfSeekRec()
  ENDIF
ENDIF

llBrowse  = .F.

*!*************************************************************
*! Name      : lfvPayType
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Valid function for the invisiable button that
*!           : open a popup have the pay type methods.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            : =lfvPayType() 
*!*************************************************************
*
FUNCTION lfvPayType

laData[25] = laPayType[puPayType,2]
SHOW GET puPayType


*!*************************************************************
*! Name      : lfwRange
*! Developer : RAM - Reham Alallamy
*! Date      : 04/03/1995
*! Purpose   : When function of all order discount & commesion objects
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfwRange
lnOldVal = EVALUATE(VARREAD())

*!*************************************************************
*! Name      : lfvRange
*! Developer : RAM - Reham Alallamy
*! Date      : 04/03/1995
*! Purpose   : Valid function of all order discount & commesion objects
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvRange

lcCurObj = VARREAD()
IF lnOldVal <> EVALUATE(VARREAD())
  DO CASE
    CASE EVALUATE(VARREAD()) > 100
      *-- 'I','Value cannot exceed 100%.','\<Ok')
      =gfModalGen("INM30000B00000","DIALOG") 
      &lcCurObj = lnOldVal
      _CUROBJ   = _CUROBJ
    CASE EVALUATE(VARREAD()) < 0
      &lcCurObj = lnOldVal
      _CUROBJ   = _CUROBJ
  ENDCASE
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : RAM - Reham Alallamy
*! Date      : 04/02/1995
*! Purpose   : Valid function of the push button < Accept >
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
PROCEDURE lpSavScr

IF EMPTY(laData[15]) .OR. EMPTY(laData[16])
  * -- You have to select payment terms, and region codes.
   =gfModalGen("INM30001B00000","DIALOG")
  _CUROBJ = IIF(EMPTY(laData[15]),OBJNUM(laData[15]),OBJNUM(laData[16]))
  llcSave = .F.
  RETURN
ENDIF

laData[14] = lcSSN1 + lcSSN2 + lcSSN3

SELECT SALESREP

IF laScrMode[4]     
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info('SALESREP')

*E300872,1 YMA 04/30/98 Update the trace file with the added record.
= gfTraceKey("SALESREP", SALESREP.REPCODE, "A")
*C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
IF ASCAN(laEvntTrig , PADR('SRSLSRP_A',10)) <> 0
  =gfDoTriger('SRSLSRP',PADR('SRSLSRP_A',10))
ENDIF
*C200597,1  TMI [End  ] 
*E300872,1 YMA 04/30/98 (End).

IF llDIV_COMM
  SELECT (lcTempDiv)
  lcSetDel = SET('DELETE')
  SET DELETED OFF
  SCAN FOR !EMPTY(CDIVISION)
    SELECT REP_DIV
    IF SEEK(&lcTempDiv..REPCODE+&lcTempDiv..CDIVISION)
      IF &lcTempDiv..FLAG <> 'D'
        IF DELETED()
          SCATTER TO laTemp MEMO BLANK
          laTemp[1] = &lcTempDiv..REPCODE
          laTemp[2] = &lcTempDiv..CDIVISION
          RECALL
          =RLOCK()
          GATHER FROM laTemp MEMO
          *E300872,1 YMA 04/30/98 Define the action that is done on the 
          *E300872,1 YMA 04/30/98 record (Added).
          lcStat = "A"
          *E300872,1 YMA 04/30/98 End.
        ELSE
          =RLOCK()
          =gfAdd_Info('REP_DIV')
          *E300872,1 YMA 04/30/98 Define the action that is done on the 
          *E300872,1 YMA 04/30/98 record (Modified).
          lcStat = "M"
          *E300872,1 YMA 04/30/98 End.
        ENDIF
        REPLACE COMM_RATE  WITH &lcTempDiv..COMM_RATE
        = gfAdd_Info('REP_DIV')
        *E300872,1 YMA 04/30/98 Update the trace file with the added/Modified record.
        = gfTraceKey("REP_DIV", REP_DIV.REPCODE+REP_DIV.CDIVISION, lcStat)
        *E300872,1 YMA 04/30/98 (End).
      ELSE
        *E300872,1 YMA 04/30/98 Update the trace file with the Deleted record.
        = gfTraceKey("REP_DIV", REP_DIV.REPCODE+REP_DIV.CDIVISION, "D")
        *E300872,1 YMA 04/30/98 (End).
        DELETE
      ENDIF
      UNLOCK
    ELSE
      IF &lcTempDiv..FLAG <> 'D' .AND. !DELETED(lcTempDiv)
        APPEND BLANK
        =RLOCK()
        REPLACE REPCODE    WITH &lcTempDiv..REPCODE ;
                CDIVISION  WITH &lcTempDiv..CDIVISION ;
                COMM_RATE  WITH &lcTempDiv..COMM_RATE 
                =gfAdd_Info('REP_DIV')
        UNLOCK
        *E300872,1 YMA 04/30/98 Update the trace file with the Deleted record.
        = gfTraceKey("REP_DIV", REP_DIV.REPCODE+REP_DIV.CDIVISION, "A")
        *E300872,1 YMA 04/30/98 (End).
      ENDIF
      SELECT (lcTempDiv)
    ENDIF
  ENDSCAN
  SET DELETED &lcSetDel
  SELECT (lcTempDiv)
  ZAP
ENDIF
SELECT SALESREP

*!*************************************************************
*! Name      : lfvDvsion
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : Valid function of the push button < Division >
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvDvsion 
PRIVATE lcFile_Ttl, lcBrfields

IF laScrMode[2]  
  SELECT REP_DIV
  IF SEEK(laData[1])
    lcFile_Ttl = 'Division Commissions'
    *:B803745,1 MHM 10/25/2000 [start] 
    *lcBrfields = "DIVNAME=LOOKUP(CODES.cDiscrep,gcAct_Comp+REP_DIV.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES'):H='Division'," +;
                 "COMM_RATE :H='Commission'"
                 
    lcBrfields = "DIVNAME=LOOKUP(CODES.cDiscrep,'N' + REP_DIV.CDIVISION,CODES.cCode_No,'CODES') :H='Division'," +;
                 "COMM_RATE :H='Commission'"
    *:B803745,1 MHM 10/25/2000 [end]
    =AriaBrow("laData[1]",lcFile_Ttl,gnBrhSRow1, gnBrhSCol1, gnBrhSRow2, gnBrhSCol2,;
              '',';;;;;\!\?\<Ok',"",'laTemp')
  ELSE
    *-- No division for this salesrep.
    =gfModalGen("INM30002B00000","DIALOG")
  ENDIF
ELSE
  lcBrowTitl = "Divisions"
  lnOldCom   = 0
  =lfPushKey()
  DO (gcScrDir + gcWinAppl + '\SRDIV.SPR')
  RELEASE WINDOW (lcBrowTitl)
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY
  POP KEY
ENDIF
SELECT SALESREP

*!*************************************************************
*! Name      : lfDivShow
*! Developer : RAM - Reham Alallamy
*! Date      : 04/27/1995
*! Purpose   : Division show function
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfDivShow 
IF lnDivRec = 0
  SHOW GET pbDel DISABLE
ELSE
  SHOW GET pbDel ENABLE
ENDIF

*!*************************************************************
*! Name      : lfBrowse
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : Browse function from file temp divisions
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfBrowse 
SELECT (lcTempDiv)
GO TOP
*:B803745,1 MHM 10/25/2000 [start] 
*BROWSE FIELDS lcDummy=LOOKUP(CODES.cDiscrep,gcAct_Comp+cDivision+'N'+'CDIVISION',CODES.cCode_No,'CODES') :26 :H='Division ' :W=.F. ,;
              comm_rate :12 :H="Commission" :V=lfvCom() :W= lfwCom();
       WINDOW DIVISN_0;
       IN WINDOW AWDDIVISN ;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       NOMENU;
       SAVE;
       TITLE lcBrowTitl
BROWSE FIELDS lcDummy=LOOKUP(CODES.cDiscrep,'N' + &lcTempDiv..cDivision,CODES.cCode_No,'CODES') :26 :H='Division ' :W=.F. ,;
              comm_rate :12 :H="Commission" :V=lfvCom() :W= lfwCom();
       WINDOW DIVISN_0;
       IN WINDOW AWDDIVISN ;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       NOMENU;
       SAVE;
       TITLE lcBrowTitl
*:B803745,1 MHM 10/25/2000 [end]

*!*************************************************************
*! Name      : lfwCom
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : When function of the commission rate in the browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfwCom 

lnOldCom = &lcTempDiv..comm_rate

IF !llCommWhen
  llCommWhen =.T.
  KEYBOARD "{ENTER}"
  RETURN .F.  
ENDIF

*!*************************************************************
*! Name      : lfvCom
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : Valid function of the commission rate in the browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvCom 

IF lnOldCom <> &lcTempDiv..comm_rate
  IF !BETWEEN(&lcTempDiv..comm_rate,0,99.99)
    *-- Commission rate cannot be less than zero. 
    =gfModalGen("INM30003B00000","DIALOG")
    SELECT (lcTempDiv)
    REPLACE comm_rate WITH lnOldCom
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfPushKey
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : Function to trap some keys in the division screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfPushKey 

PUSH KEY

*ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTitl) TOP

ON KEY LABEL TAB        DO lfScrCircl
ON KEY LABEL BACKTAB    DO lfScrCircl

ON KEY LABEL Ctrl+ENTER DO lfTermnat
ON KEY LABEL ESC        DO lfTermnat
ON KEY LABEL Ctrl+W     lnDummy = 1
ON KEY LABEL Ctrl+Q     lnDummy = 1
ON KEY LABEL Ctrl+HOME  lnDummy = 1

*!*************************************************************
*! Name      : lfScrCircl
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : Trap function of the TAB & SHIFT+TAB
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfScrCircl 

DO CASE
  CASE LASTKEY() = 9
    DO CASE
      CASE WONTOP("DIVISN_1")
        IF _CUROBJ = OBJNUM(pbCls)
          ACTIVATE WINDOW (lcBrowTitl)
        ELSE
          _CUROBJ = _CUROBJ + 1
        ENDIF
      CASE WONTOP(lcBrowTitl)
        ACTIVATE WINDOW DIVISN_1
        _CUROBJ = OBJNUM(pbIns)
    ENDCASE
  CASE LASTKEY() = 15
    DO CASE
      CASE WONTOP("DIVISN_1")
        IF _CUROBJ = OBJNUM(pbIns)
          ACTIVATE WINDOW (lcBrowTitl)
        ELSE
          _CUROBJ = _CUROBJ - 1
        ENDIF
      CASE WONTOP(lcBrowTitl)
        ACTIVATE WINDOW DIVISN_1
        _CUROBJ = OBJNUM(pbCls)
    ENDCASE
ENDCASE

*!*************************************************************
*! Name      : lfvIns
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Valid function of the insert push button in the
*!             division screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvIns 
PRIVATE lnRecNo,lnEmptyRec

lnRecNo=RECNO()

llCommWhen =.F.

SELECT (lcTempDiv)

LOCATE FOR EMPTY(CDIVISION)
IF !FOUND()
  APPEND BLANK
  REPLACE REPCODE    WITH laData[1] ;
          COMM_RATE  WITH 0.00 
ENDIF
lnEmptyRec = RECNO()
SHOW WINDOW (lcBrowTitl) REFRESH
lnDiv=1

DEFINE WINDOW _rld19fkcp ;
	AT  0.000, 0.000  ;
	SIZE 7.500,42.000 ;
	FONT "MS Sans Serif", 8 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(,,,192,192,192) IN WINDOW AWDDIVISN

MOVE WINDOW _rld19fkcp CENTER
MOVE WINDOW _rld19fkcp BY -7,-7
ACTIVATE WINDOW _rld19fkcp TOP

lcDiv=gfActPop(-1,0,8,26,'laDiv',2,1,@lcDiv,'_rld19fkcp')

RELEASE WINDOW _rld19fkcp
IF ! SEEK(laData[1]+lcDiv)
  GOTO lnEmptyRec
  IF !EMPTY(lcDiv)
    lnDivRec = lnDivRec + 1
    SHOW GET pbDel ENABLE

    REPLACE CDIVISION WITH lcDiv
    KEYBOARD '{ALT+B}'
  ELSE
    DELETE
    GOTO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),1,lnRecNo)
    SHOW WINDOW (lcBrowTitl) REFRESH
  ENDIF
ELSE
    GOTO lnEmptyRec
    DELETE  
    =SEEK(laData[1]+lcDiv)
    SHOW WINDOW (lcBrowTitl) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvDel
*! Developer : RAM - Reham Alallamy
*! Date      : 04/04/1995
*! Purpose   : Valid function of the delete push button in the
*!             division screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvDel 

*-- 'Are you sure you want to delete this record?','\!\<Yes;\?\<No'
IF gfModalGen('QRM00002B36001',"DIALOG","delete") = 1
  lnDivRec = IIF(lnDivRec - 1 > 0,lnDivRec - 1,0)
  IF lnDivRec = 0
    SHOW GET pbDel DISABLE
  ENDIF
  SELECT (lcTempDiv)
  REPLACE FLAG WITH 'D'
  DELETE
  GO TOP
  SHOW WINDOW (lcBrowTitl) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfTermnat
*! Developer : RAM - Reham Alallamy
*! Date      : 04/06/1995
*! Purpose   : Function to terminate the program.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfTermnat 

ACTIVATE WINDOW DIVISN_1
_CUROBJ = OBJNUM(pbCls)
KEYBOARD "{ENTER}"

*!*************************************************************
*! Name      : lfvDivCls
*! Developer : RAM - Reham Alallamy
*! Date      : 04/19/1995
*! Purpose   : Valid function for the close push button.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvDivCls 

IF EMPTY(&lcTempDiv..CDIVISION)
  lnDivRec = IIF(lnDivRec - 1 > 0,lnDivRec - 1,0)
  SHOW GET pbDel DISABLE
  DELETE
  GO TOP
ENDIF
RELEASE PAD _BROWSE OF _MSYSMENU


*!*************************************************************
*! Name      : lfActBrows
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Valid function for the close push button.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfActBrows 

ACTIVATE WINDOW (lcBrowTitl) 

*!*************************************************************
*! Name      : lpDelScr
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Valid for the delete push button.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
PROCEDURE lpDelScr

SELECT OrdHdr
*B602548,1 (Begin) Remark the following line and change status to 'x'.
*LOCATE FOR OrdHdr.Status <> 'C' .AND. (OrdHdr.Rep1 = laData[1] .OR. OrdHdr.Rep2 = laData[1])
LOCATE FOR OrdHdr.Status <> 'X' .AND. (OrdHdr.Rep1 = laData[1] .OR. OrdHdr.Rep2 = laData[1])
*B602548,1 (End)
IF FOUND()

  *---  This Sales Representitive has Sales Orders.
  *---  Cannot delete !
  =gfModalGen("INM30017B00000","DIALOG") 
  SELECT SALESREP
  =gfObj_lock(.F.)
  RETURN
  
ENDIF

SELECT SALESREP

WAIT WINDOW  'Deleting Salesrep and all associated commissions.....' NOWAIT
SELE REPCOMM
IF SEEK(laData[1])
  DELETE REST WHILE REPCODE=laData[1]
ENDIF

*-- DELETE THE SALESREP NOTEPAD 
WAIT WINDOW 'Deleting the notepad for salesrep &laData[1]' NOWAIT
SELECT NOTEPAD
IF SEEK('J'+laData[1])
  DELETE REST WHILE TYPE+KEY='J'+laData[1]
ENDIF

** DELETE THE COMMISSIONS FROM THE REP_dIV FILE. FAR 03/06/94
WAIT WINDOW 'Deleting the division commissions for salesrep &laData[1]' NOWAIT
SELE REP_DIV
IF SEEK(laData[1])
  DELETE REST WHILE REPCODE=laData[1]
ENDIF
 
SELECT SALESREP  &&DM12/19/91 - Put after 'Select Repcomm' if deleting

SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 
laScrMode        = .F.
laScrMode[1]     = .T.

*!*************************************************************
*! Name      : lfvCommiss
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Commis. Inquiry 
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvCommiss 
PRIVATE lcFile_Ttl, lcBrfields
IF SEEK(laData[1],'REPCOMM')
  SELECT REPCOMM
  lcFile_Ttl = 'Commissions for Salesrep # ' + laData[1] 
  
  lcBrfields = "TRAN    :8:H='Tran#',"+;
               "STATUS  :2:H='S',"+;
               "DATE    :8:H='Date',"+;
               "DESC    :H='Description':19 ,"+;
               "CUSTPO  :H='Reference',"+;
               "ACCOUNT :7:H='Acct',"+;
               "STORE   :10:H='Store',"+;
               "ORDER   :H='Order',"+;
               "AMOUNT  :H='Amount'"
  =AriaBrow("laData[1]",lcFile_Ttl,gnBrHSRow1, ;
           gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'';
           ,'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',"",'laTemp')
  SELECT SALESREP        
ELSE
  *-- No commissions for this salesrep.
  =gfModalGen('INM30004B00000',"DIALOG")
ENDIF

*!*************************************************************
*! Name      : lfvOrders 
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Orders Inquiry 
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvOrders 
PRIVATE lcFile_Ttl, lcBrfields

SELECT ORDHDR
SET FILTER TO REP1 = laData[1] OR REP2 = laData[1]
GO TOP

IF EOF()
  *-- No orders for this salesrep. 
  =gfModalGen("INM30005B00000","DIALOG")
ELSE
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER
  lcFile_Ttl = 'Orders for Salesrep # ' + laData[1]
  
  lcBrfields = "ORDER   :8:H='Order#'         ,"+;
               "DUMMY=IIF(cOrdType='O','S.Order','Contract') :8:H='Type',"+;
               "STATUS  :2:H='S'              ,"+;
               "ACCOUNT :7:H='Acct'           ,"+;
               "STORE   :10:H='Store'          ,"+;
               "CUSTOMER.BTNAME:H='Name':26 ,"+;
               "COMPLETE:H='Complete'       ,"+;
               "SHIPAMT :H='Shipped'        ,"+;
               "OPENAMT :H='Open'            "

  =AriaBrow('',lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
           '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',"",'laTemp')
ENDIF  

SELECT ORDHDR
SET FILTER TO 
SET RELATION TO
SELECT SALESREP

*!*************************************************************
*! Name      : lfvAccount 
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Accounts Inquiry 
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvAccount 
PRIVATE lcFile_Ttl, lcBrfields

SELECT CUSTOMER
SET FILTER TO SALESREP = laData[1]
LOCATE 

IF EOF()
  *-- 'No accounts for this salesrep.
  =gfModalGen("INM30006B00000","DIALOG")
ELSE
  lcFile_Ttl = 'Accounts for Salesrep # ' + laData[1]
  
*  lcBrfields = "ACCOUNT  :7:H='Acct',"+;
               "STORE    :10:H='Store',"+;
               "BTNAME   :H='Name':30,"+;
               "BTSTATE  :H=PADR(PROP(LOOKUP(SYCINT.CPARt5LAB,ALLTRIM(gcContCode),sycint.ccont_code,'CCONTCODE')),5,' '),"+;
               "BTCITY   :H='City':20,"+;
               "PHONE1   :20:H='Telephone'"
  lcBrfields = "ACCOUNT    :7:H='Acct',"+;
               "STORE      :10:H='Store',"+;
               "BTNAME     :30:H='Name':30,"+;
               "cAddress42 :10:H=PADR(PROP(LOOKUP(SYCINT.CPARt4LAB,ALLTRIM(gcContCode),sycint.ccont_code,'CCONTCODE')),8,' '),"+;
               "cAddress32 :20:H=PADR(PROP(LOOKUP(SYCINT.CPARt3LAB,ALLTRIM(gcContCode),sycint.ccont_code,'CCONTCODE')),8,' '),"+;
               "PHONE1     :18:H='Telephone':P=GFPHONETEM()"               
  =AriaBrow('',lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,;
           '','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',"",'laTemp')
ENDIF  

SELECT CUSTOMER
SET FILTER TO 
SELECT SALESREP        

*!*************************************************************
*! Name      : lfvNotes 
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Notes
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvNotes 

=NOTEPAD('J',laData[1])
SHOW GET pbNotes ENABLE


*!*************************************************************
*! Name      : lfOptions
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 07/01/1996
*! Purpose   : Maintaion menu pad [Options]
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcSnipet
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOptions('ACTIVATE')
*!*************************************************************
FUNCTION lfOptions 
PARAMETERS lcSnipet

IF UPPER(lcSnipet) = 'ACTIVATE'
  DEFINE PAD _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P
  SET SKIP OF PAD _OPTIONS OF _MSYSMENU (laScrMode[1])
  ON PAD _OPTIONS OF _msysmenu ACTIVATE POPUP _OPTIONPOP

  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  *B602867,1 BWA   Take out from the popup the notes item because now it's globalfrom the system
  *DEFINE BAR 1  OF _OPTIONPOP PROMPT '\<Notes'        SKIP FOR laScrMode[1]
  *DEFINE BAR 2  OF _OPTIONPOP PROMPT '\<Sales Orders' SKIP FOR laScrMode[1] .OR. laScrMode[4] 
  *DEFINE BAR 3  OF _OPTIONPOP PROMPT '\<Commissions'    SKIP FOR laScrMode[1] .OR. laScrMode[4]
  *DEFINE BAR 4  OF _OPTIONPOP PROMPT '\<Accounts'       SKIP FOR laScrMode[1] .OR. laScrMode[4] 
  *DEFINE BAR 5  OF _OPTIONPOP PROMPT '\<Divisions'      SKIP FOR laScrMode[1] .OR. !llDIV_COMM
  *DEFINE BAR 6  OF _OPTIONPOP PROMPT 'Link To'        SKIP FOR laScrMode[1] .OR. laScrMode[4]

  DEFINE BAR 1  OF _OPTIONPOP PROMPT '\<Sales Orders' SKIP FOR laScrMode[1] .OR. laScrMode[4] 
  DEFINE BAR 2  OF _OPTIONPOP PROMPT '\<Commissions'    SKIP FOR laScrMode[1] .OR. laScrMode[4]
  DEFINE BAR 3  OF _OPTIONPOP PROMPT '\<Accounts'       SKIP FOR laScrMode[1] .OR. laScrMode[4] 
  DEFINE BAR 4  OF _OPTIONPOP PROMPT '\<Divisions'      SKIP FOR laScrMode[1] .OR. !llDIV_COMM
  DEFINE BAR 5  OF _OPTIONPOP PROMPT 'Link To'        SKIP FOR laScrMode[1] .OR. laScrMode[4]
  *B602867,1 [END]

  ON SELECTION POPUP _OPTIONPOP DO lpOptions WITH BAR()
ELSE
  IF WPARENT(WONTOP()) <> gcBaseWind
     RELEASE PAD _OPTIONS OF _MSYSMENU
  ENDIF   
ENDIF

PROCEDURE lpOptions 
PARAMETERS lnBar
lcCommand = ALLTRIM(STRTRAN(laPanelObj[lnBar,3],'VALID','='))
&lcCommand.

*!*************************************************************
*! Name      : lfvStates 
*! Developer : MAN - Mohamed Abdel Salam
*! Date      : 06/20/97
*! Purpose   : Validate the state code
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfvStates()
*!*************************************************************

FUNCTION lfvStates 

PRIVATE lcFile_Ttl, lcBrfields

IF ALLTRIM(laData[26])='USA' OR ALLTRIM(laData[26])='CANADA'
  
  *B123036,1 NNA 09/21/2004 (Begin) Correct the Seek Expression because the Width of Codes.Ccode_No
  *B123036,1 NNA             = (6) not (10) and to make this Solution Dynamic ,I get the Width of the
  *B123036,1 NNA             Filed to use it in the Sentence of the (PADR) FUNCTION
  *IF !EMPTY(laData[6]) .AND. ;  
      !SEEK("N"+PADR(ALLTRIM(laData[6]),10)+'N'+'STATE','CODES')
  lnFldSize = FSIZE('CCODE_NO','CODES')  && Get width of the CODES.CCODE_NO field 
  IF !EMPTY(laData[6]) .AND. ;  
     !SEEK("N"+PADR(ALLTRIM(laData[6]),lnFldSize)+'N'+'STATE','CODES')
  *B123036,1 NNA (End)

       lnCurAlias = SELECT(0)
       SELECT CODES
       DECLARE laCodeFld[2]
       lcBrowTitl = 'State Codes'
       lcBrFields = "cCode_No  :R :H=lcBrowTitl, "+;
                 "cDiscrep  :R :H='Name'"
       
       *B038431,1 ASH 10/05/2004 (Begin) Add Type "N" to the Browse to Seek with 'N' + SalesRep. ID
       *IF gfBrows('FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
         "N"+"STATE" AND ;
         cRltField="N"','cCode_No,cDiscrep','laCodeFld')
       IF gfBrows(' "N" FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
         "N"+"STATE" AND cRltField="N"','cCode_No,cDiscrep','laCodeFld')
       *B038431,1 ASH (End)

         laData[6] = laCodeFld[1]
     
       *B123036,1 NNA 09/21/2004 (Begin) if the browse window Opened then you press Escape you'll
       *B123036,1 NNA            find that the Pointer move to the next field even the Code isn't Valid 
       *B123036,1 NNA            so I stop it on the state filed until user input or choose a valid code
       ELSE
         _CUROBJ = _CUROBJ
       *B123036,1 NNA (End)
     
       ENDIF
       SELECT (lnCurAlias)
  ENDIF    
ENDIF