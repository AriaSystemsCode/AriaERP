********************************************************************
*: Program file  : SMUTILSC.PRG 
*: Program desc. : Schedule File Utilities.
*: For screen    : ROB4900
*:                 ROB4910
*:                 ROB4920
*:                 lMover
*:         System: ARIA APPAREL SERIES
*:         Module: SM
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*: Refer to      : (E301516)
*:     Date      : 17/01/2001
*!********************************************************************
*: Modification :
*:B604531,1 AME      Fix the bugs of 1)Data type mismatch .
*:B604531,1 AME                      2)Invalid subscript.   
*:B607307,1 AMH 06/25/2003 Fix the bug of don't check if rebalance program selected when modify.
*!********************************************************************
*-----------------Intiliaze needed variables
*--laScrMode && Screen Modes.
*B604531,4  AME [Start] Variable to clear read
lcRblVld = ''
*B604531,4  AME [End]
lcSchTitl='Schedule'
llModify=.F.
llDayChange=.F.
lcWState='DISABLE'
lcDState='DISABLE'
lcMState='ENABLE'
lcRState='ENABLE'
lcDayState='DISABLE'
lcDateStat='ENABLE'
DIMENSION laScrMode[4]
STORE .F. TO laScrMode
laScrMode[2] = .T.
STORE .T. TO llAlowNew
laWobjects  =.F.
llNowTmp = .F.
llUsrSlct= .F.
lnOldRec = 1
*llAllComp   = .F.
DIMEN laPrinters[1],laDevice[1],lafoldwinds[2,2],laStatus[1,2],laServer[1,2]
STORE '' TO lcCompName,lcCompDesc,lcDefa_print,ladevice,laPrinters,lcDefPrint,;
            lafoldwinds,laStatus,laServer,laOutput,laDisplay
laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
DIMENSION laRpCodMod[1,2],laRpSorMod[1],laRpTarMod[1]
STORE '' TO laRpCodMod,lcSelMod
IF !gfSetup()
  RETURN
ENDIF

*SELECT SYUSTATC
*LOCATE FOR cObj_name ='OLDVARS' .AND. syuStatc.cStation <> gcStation
*** If at least one user have a record in the static files
*** keep locating for all users
*DO WHILE FOUND()
*  lcUser    = syuStatc.cUser_ID        
*  lcStion   = syuStatc.cStation
*  IF lcUser = gcUser_Id .AND. lcStion = gcStation
*    CONTINUE
*  ELSE
*    lcOldRep = SET('REPROCESS')
*    SET REPROCESS TO 1
*    IF RLOCK('SYUSTATC')
*      UNLOCK IN ALIAS('SYUSTATC')
*      SET REPROCESS TO lcOldRep
*      CONTINUE
*    ELSE
*      SET REPROCESS TO lcOldRep
*      =gfModalGen ('INM00025B00000','Alert')  
*      glQuitting = .T.
*      RETURN .T.
*    ENDIF
*  ENDIF
*ENDDO


lcTemp=gfTempName()
CREATE DBF (gcWorkDir+lcTemp) (SCPRG C(250),TIMESCH C(30),RECNU N(2))
=gfOpenFile (gcSysHome+'syubsrvj',gcSysHome+'Userid','SH')
=gfOpenFile (gcSysHome+'syubserv',gcSysHome+'Serverid','SH')
=gfOpenFile(gcSysHome+'SYDAPPL','CAPP_ID')
=gfOpenFile (gcSysHome+'Syccomp',gcSysHome+'Ccomp_id','SH')
*=gfOpenFile (gcSysHome+'SYUBSERV','','SH')
*SELECT SyuBtSrv
SELECT syubsrvj
*SEEK('ROB4800')
*SCAN WHILE CREPUID='ROB4800' FOR cStatus='O'
*SEEK('SMUTILS')
*SCAN WHILE CREPUID='SMUTILS' FOR cStatus='O'
SCAN FOR CREP_ID='SMDOUTL' AND cStatus='O'
  lcSched=''
  lcTimeSch=''
  lnRecNo=RECNO()
  FOR I = 1 TO MEMLINE(mschedinfo)
    lcSched=lcSched+' '+(MLINE(mSchedInfo,I))
    IF ATC('**',MLINE(mSchedInfo,I))>0
      EXIT
    ENDIF
  ENDFOR
  lcSched=SUBSTR(ALLTRIM(STRTRAN(lcSched,'**')),1,250)
  lcTimeSch=MLINE(mSchedInfo,MEMLINE(mSchedInfo))
  SELECT (lcTemp)
  APPEND BLANK
  REPLACE SCPRG WITH lcSched,TimeSch WITH lcTimeSch,RecNu WITH lnRecNo
ENDSCAN
LOCATE
SELECT (lcTemp)
GO TOP
IF EOF()
  lcMState='DISABLE'
  lcRState='DISABLE'
ENDIF
lcFObject   = "PBRUN"
lcLObject   = "PBCLOSE"
PUSH KEY
=gfClearKey()
ON KEY LABEL TAB        llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL BACKTAB    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL CTRL+W     llDummi=1  
ON KEY LABEL CTRL+Q     llDummi=1  
*B604531,4  AME [Start] fix the bug of not showing the screen in case of press
*B604531,4  AME         Control + insert
ON KEY LABEL CTRL+INS DO GFVCPNEW
ON KEY LABEL CTRL+E  DO GFCPEDIT
*B604531,4  AME [End]
IF _DOS OR _UNIX
  =gfSetClrSet() && Set the Color set of the screen
ENDIF
IF !FILE(gcWorkDir+'AutoSchd.MEM')
  lcLogvFile = '&gcWorkDir.AutoSchd.txt'
  SAVE ALL LIKE lcLogvFile* TO (gcWorkDir+'AutoSchd.MEM')
ELSE
  RESTORE FROM (gcWorkDir+'AutoSchd.MEM') ADDI
ENDIF
ckLog = 1
llOvWrt = .F.
ON KEY LABEL ESC DO lpclose
DO (gcScrDir + gcWinAppl + '\SMUTILSC.SPX')
*B604531,4  AME [Start]
IF !EMPTY(gcAct_Comp)
  DO CASE
   CASE 'IC' $ gcPrmtMdls
     =gpChngModl('IC')
   CASE 'PS' $ gcPrmtMdls
     =gpChngModl('PS')
   CASE 'GL' $ gcPrmtMdls
     =gpChngModl('GL')
   CASE 'AR' $ gcPrmtMdls
     =gpChngModl('AR')
   CASE 'AP' $ gcPrmtMdls
     =gpChngModl('AP')
  ENDCASE   
  =gpChngModl('SM')
ENDIF
*B604531,4  AME [End]
ON KEY
POP KEY

*!*************************************************************
*! Name      : lfSchBrows
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : To browse the scheduled programs.
*!***********************************************************************
*! Calls     :
*!***********************************************************************
*! Returns            : Nothing
*!***********************************************************************
*! Example            : =lfSchBrows()
*!***********************************************************************

FUNCTION lfSchBrows
SELECT (lcTemp)
GO TOP
lnMarker =RECNO()
DO CASE
  CASE _DOS .OR. _UNIX
    BROWSE FIELDS SCPRG :H='Schedule programs':40,TimeSch:H='Time Scheduled to run':25 ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       COLOR SCHEME 10   ;
       WINDOW    ROB4901 ;
       IN WINDOW ROB4900 ;
       TITLE lcSchTitl
  CASE _WINDOWS .OR. _MAC
    BROWSE FIELDS SCPRG :H='Schedule programs':50,TimeSch:H='Time Scheduled to run':30;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       WINDOW    ROB4901 ;
       IN WINDOW ROB4900 ;
       TITLE lcSchTitl
ENDCASE

SHOW WINDOW (lcSchTitl) REFRESH SAME
DO GPCTRLSHOW
return .f.
*!*************************************************************
*! Name      : lfVClose
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the Close button.
*!***********************************************************************
*! Calls     :
*!***********************************************************************
*! Returns            : Nothing
*!***********************************************************************
*! Example            : =lfVClose()
*!***********************************************************************

FUNCTION lfVClose

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL CTRL+W     
ON KEY LABEL CTRL+Q     
ACTIVATE WINDOW (WOUTPUT())
CLEA READ 
SHOW GETS
RETURN

*!*************************************************************
*! Name      : lfKeyTrap
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Trapping keys.
*!***********************************************************************
*! Calls     :
*!***********************************************************************
*! Returns            : Nothing
*!***********************************************************************
*! Example            : =lfKeyTrap()
*!***********************************************************************

FUNCTION lfKeyTrap
PARAMETERS lcDirection,lcFirstObj,lcLastObj

*** Adjest the names of objects to ask for and where to go
lcObjChek = IIF(lcDirection = "FORWORD",UPPER(lcLastObj) ,UPPER(lcFirstObj))
lcObjJump = IIF(lcDirection = "FORWORD",UPPER(lcFirstObj),UPPER(lcLastObj) )
lnFactor  = IIF(lcDirection = "FORWORD",1,-1)

*** If key pressed from browse direct control to the right object
*** and if key pressed comming from button go to the browse window
IF WONTOP(WOUTPUT()) .AND. VARREAD()=lcObjChek
  ACTIVATE WINDOW (lcSchTitl)
ELSE
  _CUROBJ =IIF(WONTOP(WOUTPUT()),_CUROBJ+(1*lnFactor),OBJNUM(&lcObjJump))
  ACTIVATE WINDOW (WOUTPUT())
ENDIF  

*!*************************************************************
*! Name      : lfBroActiv
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfBroActiv()
*!*************************************************************
FUNCTION lfBroActiv

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
*ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpclose
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 

*!*************************************************************
*! Name      : lfBroDeact
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Deactivate the browse.
*!***********************************************************************
*! Calls     : lpTrapKey
*!***********************************************************************
*! Returns            : Nothing
*!***********************************************************************
*! Example            : =lfBroDeact()
*!***********************************************************************

FUNCTION lfBroDeact
IF WONTOP() = lcSchTitl
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTrapKey WITH 'ROB4902', 'PBRUN'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'ROB4902', 'pbClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : gfvCpNew
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the New button.
*!***********************************************************************
*! Calls     :
*!***********************************************************************
*! Returns            : Nothing
*!***********************************************************************
*! Example            : =gfvCpNew()
*!***********************************************************************
FUNCTION gfvCpNew

llNowTmp = .F.
ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL CTRL+W     
ON KEY LABEL CTRL+Q     

RBONCE=1
RBDAILY=0
RBWEEK=0
RBMONTH=0
lnDay=1
lcSelComp= ' '
LCTIME   = lfGetTime()
lcDate=Date()
cbXStandard=0
cbXCustom=0
cbOStandard=0
cbOCustom=0
STORE 0 TO CB1,CB2,CB3,CB4,CB5,CB6,CB7,CB8,CB9,CB10,CB11,CB12,CB13,CB14,CB15,CB16,CB17
CBMonday   =1
CBTuesday  =1
CBWednesday=1
CBThursday =1
CBFriday   =1
CBSaturday =1
CBSunday   =1
SHOW GET PBWHICH DISABLE
SHOW GET PODAYS  DISABLE
=lfwGetServ()
DO (gcScrDir + gcWinAppl + '\SMUTILSA.SPX')
*B604531,4  AME [Start] Recall the Sceen Depending onrebalance valid variable
DO WHILE !EMPTY(lcRblVld)
  * Hold The Value of lctemp to restore it agian after back from OG.
  lcTTemp = lcTemp
  DO &lcRblVld
  lcRblVld = ''
  lcTemp = lcTTemp  && restore the old value of lctemp
  IF USED('lcTempDtl')
    USE IN lcTempDtl
  ENDIF
  IF USED('lcTempHdr')
    USE IN lcTempHdr
  ENDIF
  DO (gcScrDir + gcWinAppl + '\SMUTILSA.SPX')
ENDDO
*B604531,4  AME [End] 

*!*************************************************************
*! Name      : lfVModify
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function for Modify.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVModify()
*!*************************************************************

*FUNCTION lfVModify
FUNCTION gfCPEdit

llNowTmp = .F.
llModify=.T.
GOTO &lcTemp..RecNu IN SYUBSRVJ
lcSProg=&lcTemp..SCPrg
lcTSch=&lcTemp..TimeSch
CBSelComp=1
lnSubStr=ATC(' ',lcSPROG)
lcSelComp=SUBSTR(lcSPROG,1,lnSubStr-1)
PRIVATE laSource,laTarget
DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
*llAllComp = (ALEN(laTarget,1) = ALEN(laSource,1))
lnReind=ATC('REINDEX',UPPER(lcSProg))
lnReOrg=ATC('REORGANIZE',UPPER(lcSProg))
lnReBal=ATC('REBALANCE',UPPER(lcSProg))
IF lnReind=0
  cbXStandard=0
  cbXCustom=0
ELSE
  cbXStandard=IIF(SUBSTR(lcSProg,lnReind+8,1)='S',1,0)  
  cbXCustom  =IIF(SUBSTR(lcSProg,lnReind+9,1)='C' OR cbXStandard=0,1,0)
ENDIF
IF lnReOrg=0
  cbOStandard=0
  cbOCustom=0
ELSE
  cbOStandard=IIF(SUBSTR(lcSProg,lnReOrg+11,1)='S',1,0)  
  cbOCustom  =IIF(SUBSTR(lcSProg,lnReOrg+12,1)='C' OR cbOStandard=0,1,0)
ENDIF
IF .F.
IF lnReBal=0
  *cb1=0
  * cb2=0
  * cb3=0
  * cb4=0
  * cb5=0
  * cb6=0
  STORE 0 TO CB1,CB2,CB3,CB4,CB5,CB6,CB7,CB8,CB9,CB10,CB11,CB12,CB13,CB14,CB15,CB16,CB17
ELSE
  *cb1=IIF(ATC('Cut & Sold - W.I.P.',lcSProg)>0,1,0)
  *cb2=IIF(ATC('Cut & Sold - Shipped',lcSProg)>0,1,0)
  *cb3=IIF(ATC('Cut & Sold - Orders',lcSProg)>0,1,0)
  *cb4=IIF(ATC('Style Returns',lcSProg)>0,1,0)
  *cb5=IIF(ATC('Cut & Sold - Allocated',lcSProg)>0,1,0)
  *cb6=IIF(ATC('Material On Order',lcSProg)>0,1,0)
  cb1  = IIF(ATC('GL rebalance',lcSProg)>0,1,0)
  cb2  = IIF(ATC('AP rebalance',lcSProg)>0,1,0)
  cb3  = IIF(ATC('Material on-hand',lcSProg)>0,1,0)
  cb4  = IIF(ATC('Material on-order',lcSProg)>0,1,0)
  cb5  = IIF(ATC('Style on-hand',lcSProg)>0,1,0)
  cb6  = IIF(ATC('Style in transit',lcSProg)>0,1,0)
  cb7  = IIF(ATC('Style work orders',lcSProg)>0,1,0)
  cb8  = IIF(ATC('Style work in process',lcSProg)>0,1,0)
  cb9  = IIF(ATC('Shipped Quantity',lcSProg)>0,1,0)
  cb10 = IIF(ATC('Allocated Quantity',lcSProg)>0,1,0)
  cb11 = IIF(ATC('Ordered Quantity',lcSProg)>0,1,0)
  cb12 = IIF(ATC('Return Authorization',lcSProg)>0,1,0)
  cb13 = IIF(ATC('Return Credit Memo',lcSProg)>0,1,0)
  cb14 = IIF(ATC('AR Invoice rebalance',lcSProg)>0,1,0)
  cb15 = IIF(ATC('Sales Order rebalance',lcSProg)>0,1,0)
  cb16 = IIF(ATC('C/T rebalance',lcSProg)>0,1,0)
  cb17 = IIF(ATC('P/O rebalance',lcSProg)>0,1,0)
ENDIF
ENDIF

*B607307,1 AMH Check if rebalance program selected or not [Start]
llUsrSlct = !EMPTY(SYUBSRVJ.mRepHdFlt) .OR. !EMPTY(SYUBSRVJ.mRepFxFlt) .OR.;
            !EMPTY(SYUBSRVJ.mRepVrFlt) .OR. !EMPTY(SYUBSRVJ.mUsrdVar)
*B607307,1 AMH [End]

lnOnce=ATC('ONCE',UPPER(lcTsch))
lnWeek=ATC('WEEKLY',UPPER(lcTsch))
lnDaily=ATC('DAILY',UPPER(lcTsch))
lnMonthly=ATC('MONTHLY',UPPER(lcTsch))
rbOnce=IIF(lnOnce>0,1,0)
rbWeek=IIF(lnWeek>0,1,0)
rbDaily=IIF(lnDaily>0,1,0)
rbMonth=IIF(lnMonthly>0,1,0)
IF rbOnce=1
  lcTime=SUBSTR(lcTsch,lnOnce+5,11)
  
  *B607307,1 AMH get the full length date [Start]
  *lcDate=CTOD(SUBSTR(lcTsch,lnOnce+17,8))
  lcDate=CTOD(SUBSTR(lcTsch,lnOnce+17,10))
  *B607307,1 AMH [End]
  
  lcDateStat='ENABLE'
  lcWState='DISABLE'
  lcDState='DISABLE'
  lcDayState='DISABLE'  
ENDIF
IF rbWeek=1
  lcTime=SUBSTR(lcTsch,lnWeek+7,11)
  PODAYS=ALLTRIM(SUBSTR(lcTsch,lnWeek+19,11))
  SHOW GET lcDate DISABLE
  lcDateStat='DISABLE'
  lcWState='DISABLE'
  lcDState='ENABLE'
  lcDayState='DISABLE'  
ENDIF
IF rbDaily=1
  lcTime=SUBSTR(lcTsch,lnDaily+6,11)
  lcDays=ALLTRIM(SUBSTR(lcTsch,lnDaily+18,7))
  cbMonday=IIF('M'$ lcDays,1,0)
  cbTuesday=IIF('T'$ lcDays,1,0)
  cbWednesday=IIF('W'$ lcDays,1,0)
  cbThursday=IIF('H'$ lcDays,1,0)
  cbFriday=IIF('F'$ lcDays,1,0)
  cbSaturday=IIF('S'$ lcDays,1,0)
  cbSunday=IIF('U'$ lcDays,1,0)
  lcDateStat='DISABLE'
  lcWState='ENABLE'
  lcDState='DISABLE'
  lcDayState='DISABLE'
ELSE
  cbMonday   =1
  cbTuesday  =1
  cbWednesday=1
  cbThursday =1
  cbFriday   =1
  cbSaturday =1
  cbSunday   =1
ENDIF
IF rbMonth=1
  lcTime=SUBSTR(lcTsch,lnMonthly+8,11)
  lnDay=EVAL(SUBSTR(lcTsch,lnMonthly+20,2))
  lcDayState='ENABLE'
  lcDateStat='DISABLE'
  lcWState='DISABLE'
  lcDState='DISABLE'
ENDIF
ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL CTRL+W     
ON KEY LABEL CTRL+Q     

=lfwGetServ()
DO (gcScrDir + gcWinAppl+'\SMUTILSA.SPX')
*B604531,4  AME [Start] Recall The Sceen Depending onrebalance valid variable.
DO WHILE !EMPTY(lcRblVld)
  * Hold The Value of lctemp to restore it agian after back from OG.
  lcTTemp = lcTemp
  DO &lcRblVld
  lcRblVld = ''
  lcTemp = lcTTemp  && restore the old value of lctemp
  IF USED('lcTempDtl')
    USE IN lcTempDtl
  ENDIF
  IF USED('lcTempHdr')
    USE IN lcTempHdr
  ENDIF
  DO (gcScrDir + gcWinAppl + '\SMUTILSA.SPX')
ENDDO
*B604531,4  AME [End] 
llModify=.F.

*!*************************************************************
*! Name      : lfVRemove
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the Remove button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVRemove()
*!*************************************************************

FUNCTION lfVRemove
IF gfDialog('!','Are you sure you want to remove this record ?','\<Remove;\<Cancel') = 1
  SELECT syubsrvj
  GOTO &lcTemp..RecNu
  DELETE
  SELECT (lcTemp)
  DELETE
  GO TOP
  IF EOF()
    SHOW GET pbModify DISABLE
    SHOW GET pbRemove DISABLE
  ENDIF
  SHOW WINDOW (lcSchTitl) REFRESH  
ENDIF


*!*************************************************************
*! Name      : lpTrapKey
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  DO lpTrapKey WITH 'ROB4902', 'PBRUN'
*!*************************************************************

PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfVDaily
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of daily radio button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVDaily()
*!*************************************************************

FUNCTION lfVDaily
RBONCE=0
RBMONTH=0
RBWEEK=0
SHOW GET RBONCE
SHOW GET RBMONTH
SHOW GET RBWEEK
SHOW GET lcTime ENABLE
SHOW GET lnDay  DISABLE
SHOW GET lcDate DISABLE
SHOW GET PODAYS DISABLE
SHOW GET PBWHICH ENABLE
IF llModify
  llDayChange=.T.
ENDIF


*!*************************************************************
*! Name      : lfVOnce
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of once radio button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVOnce()
*!*************************************************************

FUNCTION lfVOnce
RBDAILY=0
RBMONTH=0
RBWEEK=0
SHOW GET RBDAILY
SHOW GET RBMONTH
SHOW GET RBWEEK
SHOW GET lcTime ENABLE
SHOW GET lcDate ENABLE
SHOW GET lnDay  DISABLE
SHOW GET PODAYS DISABLE
SHOW GET PBWHICH DISABLE
IF llModify
  llDayChange=.T.
ENDIF

*!*************************************************************
*! Name      : lfVMonthly
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of monthly radio button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVMonthly()
*!*************************************************************

FUNCTION lfVMonthly
RBONCE=0
RBDAILY=0
RBWEEK=0
SHOW GET RBONCE
SHOW GET RBDAILY
SHOW GET RBWEEK
SHOW GET lcTime ENABLE
SHOW GET lnDay  ENABLE
SHOW GET lcDate DISABLE
SHOW GET PODAYS DISABLE
SHOW GET PBWHICH DISABLE
IF llModify
  llDayChange=.T.
ENDIF

*!*************************************************************
*! Name      : lfVWeekly
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of weekly radio button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVWeekly()
*!*************************************************************

FUNCTION lfVWeekly
RBONCE=0
RBMONTH=0
RBDAILY=0
SHOW GET RBONCE
SHOW GET RBDAILY
SHOW GET RBMONTH
SHOW GET lcTime ENABLE
SHOW GET lnDay  DISABLE
SHOW GET lcDate DISABLE
SHOW GET PODAYS ENABLE
SHOW GET PBWHICH DISABLE
IF llModify
  llDayChange=.T.
ENDIF


*!*************************************************************
*! Name      : lfVSelComp
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of Selecr Company radio button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : lfMover()
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVSelComp()
*!*************************************************************

FUNCTION lfVSelComp

CBSELCOMP=1
SHOW GET CBSELCOMP
DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
IF !EMPTY(lcSelComp)
  SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name;
    WHERE cComp_ID $ lcSelComp ;
    FROM SycComp INTO ARRAY laTarget;
    ORDER BY SycComp.cComp_ID
  lcSelComp=' '
ENDIF
SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name;
      FROM SycComp INTO ARRAY laSource;
      ORDER BY SycComp.cComp_ID
=gfMover(@laSource,@laTarget,"Select Companies",.T.)
IF ALEN(laTarget) > 0 AND !EMPTY(laTarget)
  FOR I = 1 TO ALEN(laTarget)
    lcSelComp=lcSelComp+SUBSTR(laTarget[I],1,2)+','
  ENDFOR
  lcSelComp=ALLTRIM(lcSelComp)
  lcSelComp=SUBSTR(lcSelComp,1,LEN(lcSelComp)-1)
  SHOW GET lcSelComp
  *-- If no modules are linked, uncheck the 'Selceted modules'.
  IF EMPTY(laRpSorMod)
    cbSelMod = 0
  ELSE
    cbSelMod = 1
  ENDIF
  SHOW GET cbSelMod
ELSE
  CBSELCOMP=0
  lcSelComp=''
  SHOW GET CBSELCOMP
  SHOW GET lcSelComp
  cbSelMod = 0
  SHOW GET cbSelMod DISABLE
ENDIF
*llAllComp = (ALEN(laTarget,1) = ALEN(laSource,1))

*!*************************************************************
*! Name      : lfVDay
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function for Day.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVDay()
*!*************************************************************
FUNCTION lfVDay
IF llModify
  llDayChange=.T.
ENDIF

IF !BETWEEN(lnDay,1,31)
  WAIT WINDOW 'INVALID DAY '
  _CUROBJ=_CUROBJ
ENDIF

*!*************************************************************
*! Name      : lfvTime
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function for time.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfvTime()
*!*************************************************************

FUNCTION lfvTime
IF llModify
  llDayChange=.T.
ENDIF
IF !BETWEEN(EVAL(SUBSTR(lcTime,1,2)),0,12) OR ;
   !BETWEEN(EVAL(SUBSTR(lcTime,4,2)),0,59) OR ;
   !BETWEEN(EVAL(SUBSTR(lcTime,7,2)),0,59) OR ;
   !(UPPER(SUBSTR(lcTime,10,2)) ='AM' OR UPPER(SUBSTR(lcTime,10,2))='PM')
   
  WAIT WINDOW 'INVALID TIME'
  _CUROBJ=_CUROBJ
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfVSOK
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function for Ok button.
*!*************************************************************
*! Passed Parameters  :  llRun
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVSOK(.F.)
*!*************************************************************

FUNCTION lfVSOK
PARAMETER llRun

llPSelect=.T.
IF EMPTY(lcSelComp)
  =gfModalGen('TRM00379B00000','ALERT')
  RETURN
ENDIF
lcSchInfo=lcSelComp
*--If user selected to Re-index Standard or Custom
IF cbXStandard=1 OR cbXCustom=1
  llPSelect=.T.
  lcSchInfo=lcSchInfo+' Reindex '
  IF cbXStandard=1
    lcSchInfo=lcSchInfo+'S'
  ENDIF
  IF cbXCustom=1
    lcSchInfo=lcSchInfo+'C'
  ENDIF
ELSE
  llPSelect=.F.
ENDIF
*--If user selected to Reorganize Standard or Custom
IF cbOStandard=1 OR cbOCustom=1
  llPSelect=.T.
  lcSchInfo=lcSchInfo+' Reorganize '  
  IF cbOStandard=1
    lcSchInfo=lcSchInfo+'S' 
  ENDIF
  IF cbOCustom=1
    lcSchInfo=lcSchInfo+'C' 
  ENDIF
ELSE
  llPSelect=IIF(llPSelect,.T.,.F.)  
ENDIF
IF llUsrSlct
  SELECT syubsrvj
  IF !EMPTY(MUSRDVAR)
    *--If user entered any options which aren't LIKE L?RP* restore them.
    RESTORE FROM MEMO MUSRDVAR ADDI
  ENDIF  
  llPSelect=.T.  
  lcSchInfo=lcSchInfo+' Rebalance '
  IF lcRpItem1 <> 'I'
    lcSchInfo=lcSchInfo+'GL rebalance '
  ENDIF
  IF lcRpItem2 <> 'I'
    lcSchInfo=lcSchInfo+'AP rebalance '
  ENDIF
  IF lcRpItem3 <> 'I'
    lcSchInfo=lcSchInfo+'Material on-hand '
  ENDIF
  IF lcRpItem4 <> 'I'
    lcSchInfo=lcSchInfo+'Material on-order '
  ENDIF
  IF lcRpItem5 <> 'I'
    lcSchInfo=lcSchInfo+'Style on-hand '
  ENDIF
  IF lcRpItem6 <> 'I'
    lcSchInfo=lcSchInfo+'Style in transit '
  ENDIF
  IF lcRpItem7 <> 'I'
    lcSchInfo=lcSchInfo+'Style work orders '
  ENDIF
  IF lcRpItem8 <> 'I'
    lcSchInfo=lcSchInfo+'Style work in process '
  ENDIF
  IF lcRpItem9 <> 'I'
    lcSchInfo=lcSchInfo+'Shipped Quantity '
  ENDIF
  IF lcRpItem10 <> 'I'
    lcSchInfo=lcSchInfo+'Allocated Quantity '
  ENDIF
  IF lcRpItem11 <> 'I'
    lcSchInfo=lcSchInfo+'Ordered Quantity '
  ENDIF
  IF lcRpItem12 <> 'I'
    lcSchInfo=lcSchInfo+'Return Authorization '
  ENDIF
  IF lcRpItem13 <> 'I'
    lcSchInfo=lcSchInfo+'Return Credit Memo '
  ENDIF
  IF lcRpItem14 <> 'I'
    lcSchInfo=lcSchInfo+'AR Invoice rebalance '
  ENDIF
  IF lcRpItem15 <> 'I'
    lcSchInfo=lcSchInfo+'Sales Order rebalance '
  ENDIF
  IF lcRpItem16 <> 'I'
    lcSchInfo=lcSchInfo+'C/T rebalance '
  ENDIF
  IF lcRpItem17 <> 'I'
    lcSchInfo=lcSchInfo+'P/O rebalance '
  ENDIF
ELSE
  llPSelect=IIF(llPSelect,.T.,.F.)    
ENDIF
*==================================================================
IF !llPSelect
  =gfModalGen('TRM00380B00000','ALERT')
  RETURN
ENDIF
SELECT syubsrvj
lcTimeInfo=''
lcNextDate={}
lcNextTime=lcTime
lcNewTime=lfTime(lcNextTime)
DO CASE 
  CASE rbOnce=1
    lcTimeInfo=lcTimeInfo+'Once '+lcTime+' '+DTOC(lcDate)
    lcNextDate=lcDate
  CASE rbWeek=1
    lcTimeInfo=lcTimeInfo+'Weekly '+lcTime+' '+PoDays
    lcToDay  =DOW(DATE())
    lcNextDay=lfDayNo(PoDays)
    IF lcToDay=lcNextDay AND lcNewTime>TIME()
      lcNextDate=DATE()
    ELSE
      *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27
      *lcNextDate=IIF(lcToDay<lcNextDay,;
                     IIF(llDayChange,dInDate,DATE())+lcNextDay-lcToDay,;
                     IIF(llDayChange,dInDate,DATE())+lcNextDay-lcToDay+7)
      lcNextDate=IIF(lcToDay<lcNextDay,;
                     IIF(llDayChange,dAdd_Date,DATE())+lcNextDay-lcToDay,;
                     IIF(llDayChange,dAdd_Date,DATE())+lcNextDay-lcToDay+7)
      *B604531,4 AME [End]
    ENDIF
  CASE rbDaily=1
    lcTimeInfo=lcTimeInfo+'Daily '+lcTime
    lcTDays=''
    IF cbMonday=1
      lcTDays=lcTDays+'M'
    ENDIF  
    IF cbTuesday=1  
      lcTDays=lcTDays+'T'
    ENDIF  
    IF cbWednesday=1
      lcTDays=lcTDays+'W'
    ENDIF  
    IF cbThursday=1
      lcTDays=lcTDays+'H'
    ENDIF  
    IF cbFriday=1
      lcTDays=lcTDays+'F'
    ENDIF  
    IF cbSaturday=1
      lcTDays=lcTDays+'S'
    ENDIF  
    IF cbSunday=1
      lcTDays=lcTDays+'U'
    ENDIF  
    lcTimeInfo=lcTimeInfo+' '+lcTDays
    llExit=.F.
    *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27
    *lcN=lfToday(UPPER(CDOW(IIF(llDayChange,dInDate,DATE()))))  
    lcN=lfToday(UPPER(CDOW(IIF(llDayChange,dAdd_Date,DATE()))))      
    *B604531,4 AME [End]

    IF lcN $ lcTDays AND lcNewTime>TIME()
      lcNextDate=DATE()
    ELSE
      FOR I = 1 TO LEN(lcTDays)
        FOR J = 1 TO 7
          *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27          
          *lcN=lfToday(UPPER(CDOW(IIF(llDayChange,dInDate,DATE()+J))))  
          lcN=lfToday(UPPER(CDOW(IIF(llDayChange,dAdd_Date,DATE()+J))))  
          *B604531,4 AME [End]
          IF lcN $ lcTDays
            *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27          
            *lcNextDate=IIF(llDayChange,dInDate,DATE())+J
            lcNextDate=IIF(llDayChange,dAdd_Date,DATE())+J            
            *B604531,4 AME [End]
            llExit=.T.
            EXIT
          ENDIF
        ENDFOR
        IF llExit
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  CASE rbMonth=1
    lcTimeInfo=lcTimeInfo+'Monthly '+lcTime+' '+ALLTRIM(STR(lnDay))
    *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27
    *lnToDay=DAY(IIF(llDayChange,dInDate,DATE()))
    lnToDay=DAY(IIF(llDayChange,dAdd_Date,DATE()))
    *B604531,4 AME [End]
    IF lnDay=lnToDay AND lcNewTime>TIME()
      lcNextDate=DATE()
    ELSE
      IF lnDay>lnToDay
        *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27
        *lcNextDate=IIF(llDayChange,dInDate,DATE())+lnDay-lnToDay
        lcNextDate=IIF(llDayChange,dAdd_Date,DATE())+lnDay-lnToDay
        *B604531,4 AME [End]
      ELSE
        *B604531,4 AME [Start] Dindate in A26 coverted to Dadd_Date in A27
        *lcNextMonth=IIF(MONTH(IIF(llDayChange,dInDate,DATE()))=12,1,;
                        MONTH(IIF(llDayChange,dInDate,DATE()))+1)
        *lcNextYear =IIF(MONTH(IIF(llDayChange,dInDate,DATE()))=12,;
                        YEAR(IIF(llDayChange,dInDate,DATE()))+1,;
                        YEAR(IIF(llDayChange,dInDate,DATE())))
        lcNextMonth=IIF(MONTH(IIF(llDayChange,dAdd_Date,DATE()))=12,1,;
                        MONTH(IIF(llDayChange,dAdd_Date,DATE()))+1)
        lcNextYear =IIF(MONTH(IIF(llDayChange,dAdd_Date,DATE()))=12,;
                        YEAR(IIF(llDayChange,dAdd_Date,DATE()))+1,;
                        YEAR(IIF(llDayChange,dAdd_Date,DATE())))
        *B604531,4 AME [End]
        lcNextDay  =ALLTRIM(STR(lnDay))
        lcNextMonth=ALLTRIM(STR(lcNextMonth))
        lcNextYear =ALLTRIM(STR(lcNextYear))
        lcNextDate =CTOD(lcNextMonth+'/'+lcNextDay+'/'+lcNextYear)
      ENDIF
    ENDIF
    lcNextDate = IIF (EMPTY(lcNextDate),DATE(),lcNextDate)
ENDCASE

lnNoOfComp=INT(LEN(ALLTRIM(STRTRAN(lcSelComp,',')))/2)
DO CASE
  CASE cbXStandard=1 AND cbXCustom=1
    lcXV='B'
  CASE cbXStandard=1 AND cbXCustom=0
    lcXV='S'
  CASE cbXStandard=0 AND cbXCustom=1
    lcXV='C'
  OTHER
    lcXV=''
ENDCASE

DO CASE
  CASE cbOStandard=1 AND cbOCustom=1
    lcOV='B'
  CASE cbOStandard=1 AND cbOCustom=0
    lcOV='S'
  CASE cbOStandard=0 AND cbOCustom=1
    lcOV='C'
  OTHER
    lcOV=''
ENDCASE
lcBV = IIF(llUsrSlct,'Y','')
STORE '' TO lcRepH,lcFxFl,lcVrFlt,lcUsrVr
IF !EMPTY(lcBV)
  lcRepH  = SYUBSRVJ.mRepHdFlt
  lcFxFl  = SYUBSRVJ.mRepFxFlt 
  lcVrFlt = SYUBSRVJ.mRepVrFlt
  lcUsrVr = SYUBSRVJ.mUsrdVar
ENDIF
IF llNowTmp
  llNowTmp = .F.
  USE IN SYUBSRVJ
  =gfOpenFile (gcSysHome+'syubsrvj',gcSysHome+'Userid','SH')
  IF llModify
    GOTO &lcTemp..RecNu
  ENDIF  
ENDIF
llOpenRep  = .F.
llOnlyReb  = .T.
lcFilHandl = ''
IF !llRun
  SELECT syubsrvj
  IF !llModify
     APPEND BLANK
     *B604531,1 AME [Start] priority is Char. not an integer.
     *REPLACE cpServerId WITH laServer[puServer,2],;
             cRep_Id    WITH 'SMDOUTL'           ,;
             cPriority  WITH 1                   ,;
             cStatus    WITH 'O'                 ,;
             Net_Loc    WITH 'S'                 ,;
             mRepHdFlt  WITH lcRepH              ,;
             mRepFxFlt  WITH lcFxFl              ,;
             mRepVrFlt  WITH lcVrFlt             ,;
             mUsrdVar   WITH lcUsrVr             ,;
             cUser_Id   WITH gcUser_id

     REPLACE cpServerId WITH laServer[puServer,2],;
             cRep_Id    WITH 'SMDOUTL'           ,;
             cPriority  WITH '1'                 ,;
             cStatus    WITH 'O'                 ,;
             Net_Loc    WITH 'S'                 ,;
             mRepHdFlt  WITH lcRepH              ,;
             mRepFxFlt  WITH lcFxFl              ,;
             mRepVrFlt  WITH lcVrFlt             ,;
             mUsrdVar   WITH lcUsrVr             ,;
             cUser_Id   WITH gcUser_id
    *B604531,1 AME [END]             
    =gfAdd_info()                      
  ELSE
    GOTO &lcTemp..RECNU
    REPLACE  cpServerId WITH laServer[puServer,2],;
             mRepoVars  WITH ''                  ,;
             mRepHdFlt  WITH lcRepH              ,;
             mRepFxFlt  WITH lcFxFl              ,;
             mRepVrFlt  WITH lcVrFlt             ,;
             mUsrdVar   WITH lcUsrVr
  ENDIF
  REPLACE mSchedInfo WITH lcSchInfo+'**'+CHR(13)+lcTimeInfo
  REPLACE NextDate WITH lcNextDate,NextTime WITH lcNewTime
  FOR I=1 TO lnNoOfComp
    lcCompNo=SUBSTR(STRTRAN(lcSelComp,','),(I*2)-1,2)
    lcCommand='DO gcAppHome+gcAct_Appl+"\SMDOUTL" WITH '+ '"'+lcCompNo+'","'+lcXV+'","'+lcOV+'","'+lcBV+'","'+lcLogvFile+'"'
    REPLACE mRepoVars WITH mRepoVars+lcCommand+CHR(13)
  ENDFOR
ELSE
   IF USED('syubsrvj')
    SELECT syubsrvj
    lnRecNo=RECNO()
    USE
  ENDIF
  *--Display the Successful process message once if there's more than one company.
  FOR I=1 TO lnNoOfComp
    lcCompNo=SUBSTR(STRTRAN(lcSelComp,','),(I*2)-1,2)
    lcCommand='DO gcAppHome+gcAct_Appl+"\SMDOUTL" WITH '+ '"'+lcCompNo+'","'+lcXV+'","'+lcOV+'","'+lcBV+'","'+lcLogvFile+'"'
    &lcCommand.
  ENDFOR
  =gfOpenFile (gcSysHome+'syubsrvj',gcSysHome+'Userid','SH')
  SELECT syubsrvj
  *B605445,1 WAB (Start)- Record out of range 
  *IF lnRecNo<>0  
  IF lnRecNo<>0  .AND. RECCOUNT() <> 0
  *B605445,1 WAB (End)
  
    GOTO IIF(BETWEEN(lnRecNo,0,RECCOUNT()),lnRecNo,RECCOUNT())
  ENDIF
ENDIF
ON KEY LABEL TAB        llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL BACKTAB    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL CTRL+W     llDummi=1  
ON KEY LABEL CTRL+Q     llDummi=1  

IF !llRun
  lcSched=''
  lcTimeSch=''
  lnRecNo=RECNO()
  FOR I = 1 TO MEMLINE(mschedinfo)
    lcSched=lcSched+' '+(MLINE(mschedinfo,I))
    IF ATC('**',MLINE(mschedinfo,I))>0
      EXIT
    ENDIF
  ENDFOR
  lcSched=SUBSTR(ALLTRIM(STRTRAN(lcSched,'**')),1,250)
  lcTimeSch=MLINE(mschedinfo,MEMLINE(mschedinfo))
  SELECT (lcTemp)
  IF !llModify
    APPEND BLANK
  ENDIF
  REPLACE SCPrg WITH lcSched,TimeSch WITH lcTimeSch,RecNu WITH lnRecNo
  GO TOP
  IF !EOF()
    SHOW GET pbModify ENABLE LEVEL RDLEVEL()-1
    SHOW GET pbRemove ENABLE LEVEL RDLEVEL()-1
  ENDIF
  ACTIVATE WINDOW (WOUTPUT())
  RELEASE WINDOW FSCHEDULE
  DO lpShow
  SHOW WINDOW (lcSchTitl) REFRESH
ENDIF
llUsrSlct = .F.
RETURN

*!*************************************************************
*! Name      : lfvWhich
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the WHICH DAYS button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfvWhich()
*!*************************************************************

FUNCTION lfvWhich
*DO (QSD+'ROB4920.SPX')
DO (gcScrDir + gcWinAppl + '\SMUTILSB.SPX')

*!*************************************************************
*! Name      : lfvSDate
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the Date.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfvSDate()
*!*************************************************************
FUNCTION lfvSDate
IF llModify
  llDayChange=.T.
ENDIF

*!*************************************************************
*! Name      : lfvPDays
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the days POPUP.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfvPDays()
*!*************************************************************
FUNCTION lfvPDays
IF llModify
  llDayChange=.T.
ENDIF

*!*************************************************************
*! Name      : lfVSCancel
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid function of the Cancel button.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfVSCancel()
*!*************************************************************

FUNCTION lfVSCancel
ON KEY LABEL TAB        llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL BACKTAB    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL CTRL+W     llDummi=1  
ON KEY LABEL CTRL+Q     llDummi=1  

ACTIVATE WINDOW (WOUTPUT())
CLEA READ 
SHOW GETS
IF llNowTmp
  llNowTmp = .F.
  USE IN SYUBSRVJ
  =gfOpenFile (gcSysHome+'syubsrvj',gcSysHome+'Userid','SH')
  IF llModify
    GO lnOldRec
  ENDIF  
ENDIF
llUsrSlct = .F.
RETURN

*!*************************************************************
*! Name      : lfGetTime
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Valid the time entered.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  lcCurrTime.
*!*************************************************************
*! Example            :  =lfGetTime()
*!*************************************************************

FUNCTION lfGetTime

lcCurrHour = IIF(VAL(SUBSTR(TIME(),1,2))=12 OR VAL(SUBSTR(TIME(),1,2))=0,;
             '12',ALLTRIM(STR(VAL(SUBSTR(TIME(),1,2))%12)))
             
lcCurrTime = IIF(VAL(lcCurrHour)<10,'0','')+lcCurrHour+;
             SUBSTR(TIME(),3)+IIF(VAL(SUBSTR(TIME(),1,2))>=12,' pm',' am')
             
RETURN (lcCurrTime)             

*!*************************************************************
*! Name      : lfDayNo
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Function to return the no of day in the week.
*!*************************************************************
*! Passed Parameters  :  lcSDay
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  No of day in the week..
*!*************************************************************
*! Example            :  =lfDayNo(PoDays)
*!*************************************************************

FUNCTION lfDayNo
PARAMETER lcSDay
DO CASE
  CASE lcSDay='SUNDAY'
    RETURN 1
  CASE lcSDay='MONDAY'
    RETURN 2
  CASE lcSDay='TUESDAY'
    RETURN 3
  CASE lcSDay='WEDNESDAY'
    RETURN 4
  CASE lcSDay='THURSDAY'
    RETURN 5
  CASE lcSDay='FRIDAY'
    RETURN 6
  CASE lcSDay='SATURDAY'
    RETURN 7
ENDCASE    

*!*************************************************************
*! Name      : lfToday
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 11/27/1997
*! Purpose   : Function to return a letter that indicate the day.
*!*************************************************************
*! Passed Parameters  :  lcToDay.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  A character the indicate the day.
*!*************************************************************
*! Example            :  =lfToday(UPPER(CDOW(IIF(llDayChange,dInDate,DATE()))))  
*!*************************************************************

FUNCTION lfToday
PARAM lcToDay
DO CASE
  CASE lcToDay='FRIDAY'
    RETURN('F')
  CASE lcToDay='SATURDAY'
    RETURN('S')
  CASE lcToDay='SUNDAY'
    RETURN('U')
  CASE lcToDay='MONDAY'
    RETURN('M')
  CASE lcToDay='TUESDAY'
    RETURN('T')
  CASE lcToDay='WEDNESDAY'
    RETURN('W')
  CASE lcToDay='THURSDAY'
    RETURN('H')
ENDCASE   

FUNCTION lfTime
PARAM lcT
IF UPPER(SUBSTR(lcT,10,2))='PM'
  lcHour =SUBSTR(lcT,1,2)
  lcHour1=IIF(lcHour='12',lcHour,ALLTRIM(STR(EVAL(lcHour)+12)))
  lcT1=STRTRAN(lcT,lcHour,lcHour1)
  RETURN(STRTRAN(lcT1,'PM',''))
ELSE
  RETURN lcT  
ENDIF

*:***************************************************************************
*: Name       : lpShow
*: Developer  : Adel Mohammed El Gazzar (ADEL)
*: Date       : 17/01/2001
*: Purpose    : Screen modes controlling procedure.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : DO lpShow
*:***************************************************************************
PROCEDURE lpShow

*-- V I E W   M O D E
IF laScrMode[2]
  *IF EOF('syubserv')
  *    SHOW GET pbCpNew DISABLE
  *ELSE
  *    SHOW GET pbCpNew ENABLE
  *ENDIF
  *SHOW GET pbCpNew ENABLE
  SHOW GET pbSLCT  DISABLE
  SELECT (lcTemp)
  GO TOP
  IF EOF()
    SHOW GET pbEdt DISABLE
    SHOW GET pbDlt DISABLE
    SHOW GET PBRUN DISABLE
  ELSE
    SHOW GET pbEdt ENABLE LEVEL RDLEVEL()-1
    SHOW GET pbDlt ENABLE LEVEL RDLEVEL()-1
    SHOW GET PBRUN ENABLE LEVEL RDLEVEL()-1
  ENDIF
ENDIF
SHOW GET pbUsrField DISABLE

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr

*SELECT SyuBtSrv
SELECT syubsrvj
GOTO &lcTemp..RecNu
DELETE
SELECT (lcTemp)
DELETE
DO GPCTRLSHOW
SHOW WINDOW (lcSchTitl) REFRESH  


FUNCTION lfvSelMod

IF EMPTY(laRpTarMod)
  * Copy module array to source module array.
  lnModLen = ALEN(laRpCodMod,1)
  FOR lnI = 1  TO  lnModLen 
    DECLARE laRpSorMod[lnI] , laRpTarMod[lnI]  
    laRpSorMod[lnI]  =  laRpCodMod[lnI , 2] 
    laRpTarMod[lnI]  =  laRpCodMod[lnI , 2] 
  ENDFOR
ENDIF  
= gfMover(@laRpSorMod,@laRpTarMod,"Transactions",.T.,.F.,.F.,.T.)
IF !EMPTY(laRpTarMod)
  lcSelMod = ''
  FOR lnCount = 1 TO ALEN(laRpTarMod,1)
    lcSelMod = lcSelMod + laRpTarMod[lnCount,1]+IIF(lnCount<ALEN(laRpTarMod,1),',','')
  ENDFOR
ENDIF
cbSelMod = IIF(ALEN(laRpTarMod,1)=1 AND EMPTY(laRpTarMod[1]),0,1)
SHOW GET cbSelMod
SHOW GET lcSelMod 


*!******************************************************************************************
*! Name      : lfGetModul
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/11/2000
*! Purpose   : Fill the modules mover with only installed and linked modules.
*!******************************************************************************************
*! Refer to  : B603504,1
*!******************************************************************************************
FUNCTION lfGetModul
*PARAMETERS llFirst

* Save prevoius area , order
PRIVATE lcAreaNo ,  lnAreaOrd , lcAreaFlt
lcAreaNo   = SELECT(0)
lnAreaOrd  = ORDER()
DECLARE laRpCodMod[1,2]
PRIVATE  lcModId  , lnModlNo 
SELECT SYCCOMP
GO TOP
SCAN WHILE !EOF()
  * Get & assign all modules to string varibale
  lcComp_mdl = ALLTRIM(SYCCOMP.mComp_mdl)
  * If no module has been defined in this record skip to next record
  IF EMPTY(ALLTRIM(lcComp_mdl))
    SKIP
    LOOP
  ENDIF
  * Variable used as a counter to redclare the module array.
  lnModlNo =  0 
  DO WHILE !EMPTY(lcComp_mdl)
    * Get position of '|' character 
    lnPosition =   ATC (  '|' , lcComp_mdl  )
    * Accumulate no. of modules that found in SYCCOMP  
    lnModlNo =  lnModlNo  + 1
    * Declare module code array
    IF lnModlNo  = 1 
	  * Split module id from whole module string
      lcModId    =  SUBSTR(lcComp_mdl , 1 ,  2  )  
      * Remove module ID from modules string       
      lcComp_mdl = SUBSTR(lcComp_mdl , 3    )          
    ELSE
	  * Split module id from whole module string   	
      lcModId    =  SUBSTR(lcComp_mdl , 2 ,  2  )  
      * Remove module ID from modules string 
      lcComp_mdl = SUBSTR(lcComp_mdl , 4    )  
    ENDIF
    * Check duplicated module id , If this module id was inserted before in 
    * module array, skip it. 
    IF ASCAN(laRpCodMod , lcModId  ) = 0  AND !(lcModId $ 'SUEBPSRGSP')
      * Get module name , then svae module id & module name.  
	  IF SEEK( lcModId, 'SYDAPPL')
        * Declare module code array
        IF EMPTY(laRpCodMod) 
           DECLARE laRpCodMod[1,2]           
        ELSE
   	      DECLARE laRpCodMod[ALEN(laRpCodMod,1)+1,2]           
        ENDIF
        laRpCodMod[ALEN(laRpCodMod,1),1]  = lcModId  
        laRpCodMod[ALEN(laRpCodMod,1),2]  = ALLTRIM(SYDAPPL.cApp_Name)
        lcSelMod = lcSelMod + IIF(LEN(lcSelMod) < 38,IIF(ATC(lcModId,lcSelMod)=0 ,IIF(EMPTY(lcSelMod),'',',')+lcModId,''),'..')
	  ENDIF
    ENDIF 
  ENDDO   
ENDSCAN
IF !EMPTY(laRpCodMod)
  =ASORT(laRpCodMod,2)
  cbSelMod = 1
ENDIF
SHOW GET lcSelMod
SHOW GET cbSelMod 
* Return to prevoius area , order and filter 
SELECT (lcAreaNo)  
SET ORDER TO &lnAreaOrd
*-- end of lfvGetModl


FUNCTION lfGetAllCo

cbSelComp = 1
SHOW GET cbSelComp
DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name;
      FROM SycComp INTO ARRAY laTarget;
      ORDER BY SycComp.cComp_ID
IF ALEN(laTarget) > 0 AND !EMPTY(laTarget)
  FOR I = 1 TO ALEN(laTarget)
    lcSelComp=lcSelComp+SUBSTR(laTarget[I],1,2)+','
  ENDFOR
  lcSelComp=ALLTRIM(lcSelComp)
  lcSelComp=SUBSTR(lcSelComp,1,LEN(lcSelComp)-1)
  SHOW GET lcSelComp
  *-- If no modules are linked, uncheck the 'Selceted modules'.
  IF EMPTY(laRpSorMod)
    cbSelMod = 0
  ELSE
    cbSelMod = 1
  ENDIF
  SHOW GET cbSelMod
ELSE
  CBSELCOMP=0
  lcSelComp=''
  SHOW GET CBSELCOMP
  SHOW GET lcSelComp
  cbSelMod = 0
  SHOW GET cbSelMod DISABLE
ENDIF
*-----
FUNCTION lfNewWhen

IF EOF('syubserv')
  SHOW GET puServer DISABLE
ELSE
  SHOW GET puServer ENABLE
ENDIF

*------
PROCEDURE lpclose
=gfCPClose()

*------
FUNCTION lfvRun

PRIVATE lnSelect
lnSelect = SELECT()
*SELECT SyuBtSrv
SELECT syubsrvj
*B604531,4  AME [Start] var to take record no
lnRecNo = RECNO()
IF !EMPTY(MUSRDVAR)
  RESTORE FROM MEMO MUSRDVAR ADDI
ENDIF  
*B604531,4  AME [End]
GOTO &lcTemp..RECNU   
lnMemoWdth = SET('MEMOWIDTH')
SET MEMOWIDTH TO 150
*B604531,4  AME [Start] line no of memo field initialy 1
*lnMemoLnNo = 0
lnMemoLnNo = 1
*B604531,4  AME [End]
lnMemolins = MEMLINES(mRepoVars)
IF lnMemolins > 0
  *B604531,4  AME [start] line no of memo field initialy 1
  *B604531,4  AME          while condition should be <= instead of <>.
  *DO WHILE lnMemoLnNo <> lnMemolins
  DO WHILE lnMemoLnNo <= lnMemolins
  *B604531,4  AME [End]
    lcCommand = MLINE(mRepoVars,lnMemoLnNo)
    &lcCommand.
    lnMemoLnNo = lnMemoLnNo + 1
  ENDDO        
ENDIF
SET MEMOWIDTH TO (lnMemoWdth)
*B604531,4  AME [Start] reselect the file to deal with excpected 
*B604531,4  AME         bugs in aliases in the program that was run. 
SELECT syubsrvj
*B604531,4  AME [End]
*B605445,1 WAB (Start)- Record out of range 
*IF lnRecNo<>0  
IF lnRecNo<>0  .AND. RECCOUNT() <> 0
*B605445,1 WAB (End)
  GOTO IIF(BETWEEN(lnRecNo,0,RECCOUNT()),lnRecNo,RECCOUNT())
ENDIF
SELECT (lnSelect)

FUNCTION lfGetServ

PRIVATE lnRecNo,lnRePro,lcMultiLocks,lcExtended,lnCurArea
lnCurArea = SELECT()
DIMENSION laServer[1,2]
STORE '' TO laServer
SELECT SYUBSERV
lnRecNo = RECNO()
lnRePro = SET('REPRO')
SET REPROCESS TO 5
GO TOP
SCAN
  IF !EMPTY(laServer[1,1])
    DIMENSION laServer[ALEN(laServer,1)+1,2]  
  ENDIF  
  lcExtended = ''
  IF RECNO() <> lnRecNo
    UNLOCK
    lcExtended = ' Idle Server'
  ELSE  
    lcExtended = ' Current Server'
  ENDIF  
  laServer[ALEN(laServer,1),1] = ALLT(cpServerId)+' '+ALLT(cserverdes)+lcExtended
  laServer[ALEN(laServer,1),2] = ALLTR(cpServerId)
ENDSCAN
SET REPROCESS TO  lnRePro
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF  
SELECT (lnCurArea)

FUNCTION lfwGetServ
=lfGetServ()
*B604531,1 AME [Start] 
*puserver = ALLT(SYUBSERV.cpServerId)+' '+ALLT(SYUBSERV.cserverdes)+ ' Current Server'
*B604531,1 AME [End] 
SHOW GET PUSERVER

FUNCTION lfvGetName

lcFileN = lcLogvFile
llOvWrt = .F.
DO (gcScrDir + gcWinAppl + '\SMUTILSD.SPX')
ckLog = 1
SHOW GET ckLog

FUNCTION lfvGetFile

lcName  = GETFILE("TXT","Select file for Log report","Select",0) && Save the Curr.
lcFileN = IIF(EMPTY(lcName),lcLogvFile,lcName)           && Get New File Name
SHOW GETS

FUNCTION lfWhenLog

_CUROBJ = OBJNUM(lcFileN)


FUNCTION lfvLogOk

IF !EMPTY(lcFileN)
  IF FILE(lcFileN)        && check if the file name is exist 
    lnOption = gfModalGen("QRM00048B00010","DIALOGE")  && Display warrning msg.
    DO CASE
      CASE lnOption = 1      && user select OverWrite the file option
        lcLogvFile = ALLTRIM(lcFileN)
        llOvWrt    = .T.
      CASE lnOption = 2       && user select Append to the file option
        lcLogvFile = ALLTRIM(lcFileN)
      OTHERWISE    && user Cancel
        _CUROBJ = OBJNUM(lcFileN)
        RETURN
    ENDCASE
  ELSE              &&  if file does not exist
    IF OCCURS("\",lcFileN)>0   
      IF !gfValdPath(SUBSTR(lcFileN,1,RAT("\",lcFileN)-1)) && Check if the file have a valid path
        =gfModalGen("INM00049B00000","DIALOGE")
        _CUROBJ = OBJNUM(lcFileN)
        RETURN
      ELSE
        lcLogvFile = ALLTRIM(lcFileN)
      ENDIF
    ELSE && Case the file name is without a path
      lcLogvFile = FULLPATH('')+ALLTRIM(lcFileN)+'.TXT'
    ENDIF
  ENDIF  
ELSE
  =gfModalGen("INM00047B00000","DIALOGE")  
  _CUROBJ = OBJNUM(lcFileN)
  RETURN
ENDIF
SAVE ALL LIKE lcLogvFile* TO (gcWorkDir+'AutoSchd.MEM')
CLEAR READ     

FUNCTION lfvRebal

PRIVATE lnAlias
lnAlias = SELECT()

*--Initialize this variable to determine whether the user enters Run or Close from rebalance OG.
llExpr1 = .F.
*--Send to batch when the user enters Run.
gcDevice = 'BATCH'
SELECT SYUBSRVJ
IF !llNowTmp
  IF llModify
    lnOldRec = RECNO()
    SCATT MEMVAR MEMO
  ENDIF
  DIMENSION laFields[1,4]
  = AFIELDS(laFields)
  USE IN SYUBSRVJ
  CREATE CURSOR SYUBSRVJ FROM ARRAY laFields
  llNowTmp = .T.
  APPE BLAN
  IF llModify
    GATHER MEMVAR MEMO
  ENDIF
  *--Clear cRep_id for lfvRunRep in OG program to update it.
  REPLACE CREP_ID WITH ''
ELSE
  *--Clear cRep_id for lfvRunRep in OG program to update it.
  REPLACE CREP_ID WITH ''
ENDIF  
DO (gcAppHome+"SM\SMREBAL.FXP") WITH '','',.F.,.T.
lcRepH  = SYUBSRVJ.mRepHdFlt
lcFxFl  = SYUBSRVJ.mRepFxFlt 
lcVrFlt = SYUBSRVJ.mRepVrFlt
lcUsrVr = SYUBSRVJ.mUsrdVar
llUsrSlct = (TYPE('llExpr1')  <> 'L' ) OR ;
            (!EMPTY(lcRepH) OR !EMPTY(lcFxFl) OR !EMPTY(lcVrFlt) OR !EMPTY(lcUsrVr))