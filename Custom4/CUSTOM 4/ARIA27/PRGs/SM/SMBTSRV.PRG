*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E301516,1 ADEL 29/01/20001 Perform Sheduled Reindex, Reorganize, and Rebalance records.
*B604531,4 AME 06/27/2001 fix the bug invslid subscript ref. in case of change status popup.
*B607284,1 AMH 06/24/2003 Fix the bug of preview and print buttons enabled when select utilities program.
*B607307,1 AMH 06/25/2003 Fix the bug of daily , weekly and monthly run only once times.
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*******************************************************************************************

DIMEN laPrinters[1],laDevice[1],lafoldwinds[2,2],laStatus[1,2],laServer[1,2],;
      laOutput[1],laDisplay[1,2]
STORE '' TO lcCompName,lcCompDesc,lcDefa_print,ladevice,laPrinters,lcDefPrint,;
            lafoldwinds,laStatus,laServer,laOutput,laDisplay
STORE 0 TO lnJobNo,lnPrtNumber,lnTotal,lnQueue,lnComplete,lnRefresh
STORE '' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcFolder,lcBrTitle;
            ,lcFilter
STORE .F. TO cbDelete,cbComplete,cbCancel,llResume
PRIVATE ALL LIKE LBT*
lbtPrnt_LAN = glPrnt_Lan
lbtPrntPort = gcPrnt_Port
lbtPdSetup  = _PDSETUP
lbtPrnt_wd = gnPrint_Wd
lnActFolder = 2

IF !gfSetup()
  EXIT
ENDIF



IF !lfGetCompInf(@lcCompName,@lcCompDesc) OR EMPTY(lcCompName)
*  COMP (gcScrDir+gcWinAppl+'\smgetcom.spr')
  DO (gcScrDir+gcWinAppl+'\smgetcom.spx')
ENDIF
IF !EMPTY(lcCompName)
  SELECT SYUBSERV
  lnRePro = SET('REPRO')
  SET REPROCESS TO 5
  FOR lnCount = 0 TO 99
    lcComNumber = IIF(lnCount=0,'',ALLT(STR(lnCount)))
    IF !SEEK(PADR(lcCompName+' '+lcComNumber,18))
      APPEND BLANK
    ENDIF
    IF RLOCK()
      lcCompName = lcCompName+' '+lcComNumber
      REPLACE CPSERVERID WITH lcCompName,;
              cserverdes WITH lcCompDesc,;
              MOUTPUT    WITH lfRetPrint(),;
              cservertyp WITH 'P'
      =gfAdd_Info()
      EXIT        
    ENDIF  
  ENDFOR  
  SET REPRO TO lnRePro
  =lfGetServ()
  puserver = ALLT(SYUBSERV.cpServerId)+' '+ALLT(SYUBSERV.cserverdes)+ ' Current Server'
  =gfSubStr(ALLTRIM(MOUTPUT),@laOutput,CHR(27))
  SELECT SYUBSRVJ
  
  SET RELATION TO CREP_ID INTO SYDREPRT
    
  DIMENSION laDisplay[6,2] 

  laDisplay[1,1] = 'All jobs'
  laDisplay[1,2] = ''

  laDisplay[2,1] = 'Completed jobs'
  laDisplay[2,2] = 'C'

  laDisplay[3,1] = 'Canceled jobs'
  laDisplay[3,2] = 'X'


  laDisplay[4,1] = 'Open jobs'
  laDisplay[4,2] = 'O'

  laDisplay[5,1] = 'Hold jobs'
  laDisplay[5,2] = 'H'


  laDisplay[6,1] = 'Deleted jobs'
  laDisplay[6,2] = 'D'

  puDisplay = 4
  lcFilter = 'O'

  DIMENSION laStatus[7,2] 

  laStatus[1,1] = 'Deleted  '
  laStatus[1,2] = 'D'

  laStatus[2,1] = 'Completed'
  laStatus[2,2] = 'C'

  laStatus[3,1] = 'Canceled  '
  laStatus[3,2] = 'X'


  laStatus[4,1] = 'Open     '
  laStatus[4,2] = 'O'

  laStatus[5,1] = 'On Hold  '
  laStatus[5,2] = 'H'


  laStatus[6,1] = 'In work  '
  laStatus[6,2] = 'W'

  laStatus[7,1] = 'Error    '
  laStatus[7,2] = 'E'



  lcSetLib = SET('LIBR')
  KEYBOARD "{ENTER}"
  lcDefPrint=PrtWin(SYS(2015))

  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder

  lnFolderCEnd= 35.500            && Folder window end column
  lnfolderrend=   2.00            && Folder window end row


  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()  
  lcFolder = gfTempName()


  lafoldwinds[1,1] = 'Administrator'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Scope'
  lafoldwinds[2,2] = lcWinCh2
  
  lcBrTitle  = 'Jobs'
  lcWindTitl = ALLT(lcCompName)+IIF(EMPTY(lcCompDesc),'','_')+ALLT(lcCompDesc)+' ('+lcWindTitl+')'
*  COMP (gcScrDir+gcWinAppl+'\smbtsrv.spr')
  =lfCountJobjs()
  lnRefresh = SET('REFRESH')
  SET REFRESH TO 5
  *E301516,1 (Begin) Create cursor to hold the description of each program.
  
  *B607307,1 AMH Don't use this cursor to display the filed fo report/program name [Start]
  *CREATE CURSOR PROGNAME (NAME C(250))
  *APPEND BLANK
  *REPLACE NAME WITH 'Sheduled Reindex, Reorganize, and Rebalance'
  *B607307,1 AMH [End]
  
  SELECT SYUBSRVJ
  *E301516,1 (End)
  DO (gcScrDir+gcWinAppl+'\smbtsrv.spx')
  *E301516,1 (Begin) Close cursor
  
  *B607307,1 AMH Don't use this cursor to display the filed fo report/program name [Start]
  *USE IN PROGNAME 
  *B607307,1 AMH [End]
  
  *E301516,1 (End)
  SET REFRESH TO lnRefresh
  SELECT SYUBSRVJ
  
  *B607307,1 AMH Delete required job items after close the screen [Start]
  lcDelJobs = ''
  lcDelJobs = lcDelJobs + IIF(cbDelete  ,'D','')
  lcDelJobs = lcDelJobs + IIF(cbComplete,'C','')
  lcDelJobs = lcDelJobs + IIF(cbCancel  ,'X','')
  DELETE FOR CSTATUS $ lcDelJobs
  *B607307,1 AMH [End]
  
  SET RELATION TO
  SELECT SYUBSERV
  DELETE
  RELEASE WINDOW (lcBrTitle)
  glQuitting = .T.

  KEYBOARD "{ENTER}"
  =PrtWin(lcDefPrint)
  CLEAR TYPEAHEAD
  SET LIBR TO &lcSetLib  
ENDIF
=lfRsetPrint()





FUNCTION lfGetCompInf
PARAMETERS lcComputer,lcDescription
PRIVATE lcSysComp,llError,lcError,llReturn
llReturn = .T.
lcError = ON('ERROR')
llError = .F.
ON ERROR llError = .T.
CREATE CURSOR XX (MSYSTEM M)
APPEN BLANK
APPEN MEMO MSYSTEM FROM (GETENV('winbootdir')+'\system.dat')
IF !llError
  lcSysComp= SUBSTR(msystem,ATC('COMPUTERNAME',MSYSTEM),255)
  lcComputer = SUBSTR(SUBST(lcSysComp,ATC('COMPUTERNAME',lcSysComp)+12),1,atc(chr(1),SUBST(lcSysComp,ATC('computername',lcSysComp)+12))-1)
  lcDescription = SUBSTR(SUBST(lcSysComp,ATC('COMMENT',lcSysComp)+7),1,atc(chr(1),SUBST(lcSysComp,ATC('COMMENT',lcSysComp)+7))-1)
  llReturn = !EMPTY(lcComputer)
ELSE  
  STORE '' TO lcComputer,lcDescription
ENDIF  
USE IN XX
ON ERROR &lcError
RETURN llReturn






PROCEDURE getprint 
* Query WIN.INI to get data on the installed and default printers. 
  
PARAMETER laDevice, lnnumber, lcdfltprnt 
* Usage: 
*    DIMENSION laDevice[1] 
*    numprinters = 0 
*    defaultprnt = "" 
*    DO GETPRINT WITH laDevice, numprinters, defaultprnt 
* 
* The "laDevice" array will be populated with the names and 
* parameters for all installed print devices. 
* 
* The "laDevice" array has this structure: 
* Col 1: Printer name 
* Col 2: Parameter for [laDevice] section 
* Col 3: Parameter for [PrinterPort] section (includes time-out 
* parameters) 
* 
* The contents of "laDevice" might look something like this after 
* GETPRINT is called: 
*   Col 1                      Col 2           Col 3 
*   -------------------------  --------------  ---------------- 
*   Apple LaserWriter II NTX   pscript,LPT2:   pscript,LPT2:,15,90 
*   Generic / Text Only=TTY    fred.prn        fred.prn,15,45 
*   HP LaserJets(Level 5)      HPPCL5MS,LPT1:  HPPCL5MS,LPT1:,15,45 
* 
* "numprinters" in this case would be 3 
* "defaultprnt" might be this: 
*    HP LaserJets(Level 5),HPPCL5MS,LPT1: 
  
#DEFINE lnbuflen 2048 
PRIVATE lcin_talk, lndcount, lcretbuf, lnbytes, lcthisdevice,lcSetLib 
  
IF PARAMETERS() < 3 
  WAIT WINDOW "This procedure requires 3 parameters" 
  RETURN 
ENDIF 
lcSetLib = SET('LIBR')  
IF FILE(SYS(2004)+"FOXTOOLS.FLL") 
  SET LIBRARY TO (SYS(2004)+"foxtools.fll") ADDITIVE 
ELSE 
  WAIT WINDOW "GETPRINT requires the FoxTools library." 
  RETURN 
ENDIF 
  
IF SET("TALK") = "ON" 
  SET TALK OFF 
  lcin_talk = "ON" 
ELSE 
  lcin_talk = "OFF" 
ENDIF 
  
* Fill in the first column of the array with installed laDevice 
* names. 
lcretbuf = REPLICATE(CHR(0),lnbuflen) 
lnbytes = getprostrg("devices",0,CHR(0),@lcretbuf,lnbuflen) 
* The second argument of 0 to GetProfileString() returns the contents 
* of the entire section, with each entry separated by a null terminator 
* (CHR(0)). 
lndcount = 0 
lcretbuf = LEFT(lcretbuf,lnbytes) 
DO WHILE CHR(0) $ lcretbuf 
  lcthisdevice = LEFT(lcretbuf,AT(CHR(0),lcretbuf)-1) 
  IF LEFT(lcthisdevice,1) <> CHR(0) 
    lndcount = lndcount + 1 
    DIMENSION laDevice[lndcount,4] 
    laDevice[lndcount,1] = lcthisdevice 
  ENDIF 
  lcretbuf = SUBSTR(lcretbuf,AT(CHR(0),lcretbuf)+1) 
ENDDO 
  
* Fill in the second and third columns of the laDevice array with the 
* parameters of each installed device from the [devices] section 
* (column 2) and the [PrinterPorts] section (column 3). 
FOR lnCount = 1 TO lndcount 
  lcretbuf = REPLICATE(CHR(0),256) 
  lnbytes = getprostrg("devices",laDevice[lnCount,1],CHR(0),@lcretbuf,256) 
  lcretbuf = LEFT(lcretbuf,lnbytes) 
  laDevice[lnCount,2] = lcretbuf 
  lcretbuf = REPLICATE(CHR(0),256) 
  lnbytes = getprostrg("PrinterPorts",laDevice[lnCount,1],CHR(0),@lcretbuf,256) 
  lcretbuf = LEFT(lcretbuf,lnbytes) 
  laDevice[lnCount,3] = lcretbuf 
ENDFOR 
  
* Store the number of installed devices. 
lnnumber = lndcount 
  
* Now get the default printer. 
lcretbuf = REPLICATE(CHR(0),256) 
lnbytes = getprostrg("windows","device",CHR(0),@lcretbuf,256) 
lcretbuf = LEFT(lcretbuf,lnbytes) 
lcdfltprnt = lcretbuf 
  
SET TALK &lcin_talk 
*RELEASE LIBRARY (SYS(2004)+"foxtools.fll")  
SET LIBR TO &lcSetLib

FUNCTION getprostrg 
PARAMETER SECTION, entry, dflt, buffer, blen 
fn = regfn("GETPROFILESTRING","CCC@CI","I") 
RETURN callfn(fn,SECTION,entry,dflt,@buffer,blen) 
  
FUNCTION getport 
PARAMETER lcpstrg, lnpnum 
* Get the first "port" from a printer string. 

* First get the printer driver name (e.g., pscript) and the comma. 
lcretstrg = SUBSTR(lcpstrg,1,AT(',',lcpstrg)) 
  
* Now get the port designation (e.g., LPT1: or FILE:) 
  
* Check if the port passed is greater than the ports available. 
IF OCCURS(',',lcpstrg) >= lnpnum 
  lcportstrg = SUBSTR(lcpstrg,AT(',',lcpstrg,lnpnum)+1) 
ELSE 
  lcportstrg = SUBSTR(lcpstrg,AT(',',lcpstrg,1)+1) 
ENDIF 
  
IF AT(',',lcportstrg) > 0 
  lcportstrg = LEFT(lcportstrg,AT(',',lcportstrg)-1) 
ENDIF 
  
RETURN lcretstrg + lcportstrg 


FUNCTION lfRetPrint
STORE '' TO lcDefa_print,ladevice,laPrinters
lnPrtNumber = 0 
DO GETPRINT WITH laPrinters, lnPrtNumber, lcDefa_print 
lcPrinters = ''
FOR lnCount = 1 TO ALEN(laPrinters,1)
  IF UPPER(SUBSTR(laPrinters[lnCount,2],ATC(',',laPrinters[lnCount,2])+1)) <> 'FILE:'
    lcPrinters = lcPrinters+IIF(lnCount>1,CHR(27),'')+laPrinters[lnCount,1]
    IF EMPTY(laPrinters[lnCount,4])
      laPrinters[lnCount,4] =LOOKUP(SYCPRINT.cprint_id,UPPER(laPrinters[lnCount,1]),SYCPRINT.cprint_id,'cprint_id')
    ENDIF  
  ENDIF  
ENDFOR
RETURN lcPrinters


FUNCTION lfActFolder
IF lnActFolder = 1
  SHOW GETS WINDOW (lcWinch1) ENABLE
  
  *B607284,1 AMH preview and print buttons disabled when select utilities program [Start]
  =lfwBrow()
  *B607284,1 AMH [End]
  
ELSE
  SHOW GETS WINDOW (lcWinch2) ENABLE
ENDIF  


FUNCTION lfBrowJob
SELECT SYUBSRVJ

*E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
*SET FILTER TO cpserverid+cuser_id=PADR(lcCompName,18)  &&AND CSTATUS = (lcFilter)
 SET FILTER TO cpserverid+cuser_id=PADR(lcCompName,18) .AND. SYDREPRT.Cver<>"A40" &&AND CSTATUS = (lcFilter)
   
*E038033,1 HMA [END]



*E301516,1 (Begin) Browse program description if the record is a Scheduled record.
*lcBrFields = [nRecPoint=IIF(RECNO()=lnJobNo,'>',' '):H='',cUser_id:H='User',sydreprt.crep_name]+;
             [:H='Report':30,cStatus=lfGetStat(cStatus):H='Status',Device=ALLT(MOUTPUT):H='Output']

*B607307,1 AMH Don't use this cursor to display the filed fo report/program name [Start]
*lcFiName = IIF(NET_LOC = 'S','PROGNAME.name','sydreprt.crep_name')
*lcBrFields = [nRecPoint=IIF(RECNO()=lnJobNo,'>',' '):H='',cUser_id:H='User',(lcFiName)]+;
             [:H='Report' :30 :W=lfgetstr() ,cStatus=lfGetStat(cStatus):H='Status',Device=ALLT(MOUTPUT):H='Output']



lcBrFields = [nRecPoint=IIF(RECNO()=lnJobNo,'>',' '):H='',cUser_id:H='User',]+;
             [lcFiName = IIF(NET_LOC = 'S',lfGetStr(),sydreprt.crep_name)]+;
             [:H='Report':30,cStatus=lfGetStat(cStatus):H='Status',Device=ALLT(MOUTPUT):H='Output']


*B607307,1 AMH [End]

*E301516,1 (End)
             
BROWSE FIELDS &lcBrFields WINDOW (lcWinCh0) IN WINDOW (gcBaseWind);
       FOR CSTATUS = (lcFilter);
       Title lcBrTitle;
       WHEN lfwBrow();
       Valid :F lfvBrow();
       NOMENU NOMODIFY NOAPPEND NODELETE NOWAIT NOCLEAR
       

FUNCTION lfwBrow
glFromBrow = .T.
lnJobNo = RECNO()
*B604531,4 AME [Start] pustatus to be numeric not character
*puStatus = lfGetStat(CSTATUS)
puStatus = lfGetStat(CSTATUS,'N')
*B604531,4 AME [End]
puserver = ALLT(SYUBSERV.cpServerId)+' '+ALLT(SYUBSERV.cserverdes)+ ' Current Server'
*B604531,4 AME [Start] pop up is numeric value
*puOutput = ALLT(SYUBSRVJ.MOUTPUT)
puOutput = ASCAN(laOutPut,ALLT(SYUBSRVJ.MOUTPUT))
*B604531,4 AME [End] 
SHOW GET puoutput
SHOW GET PUSTATUS

*B607284,1 AMH preview and print buttons disabled when select utilities program [Start]
*IF SYUBSRVJ.cStatus $ 'OHC' AND !llResume
IF !EOF('SYUBSRVJ') .AND. SYUBSRVJ.cStatus $ 'OHC' AND !llResume AND SYUBSRVJ.NET_LOC <> 'S'
*B607284,1 AMH [End]

  SHOW GET PBSHOW ENABLE
  SHOW GET pbPrtJob ENABLE
ELSE
  SHOW GET PBSHOW DISABLE
  SHOW GET pbPrtJob DISABLE
ENDIF  
SHOW WINDOW (lcBrTitle) REFRESH SAME

FUNCTION lfvBrow
IF !WVISIBLE(gcBaseWind)
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
       
       
       
FUNCTION lfvFilter
IF puDisplay>0
  lcFilter = laDisplay[puDisplay,2]
  SHOW WINDOW (lcBrTitle) REFRESH SAME
ENDIF       

FUNCTION lfGetStat
PARAMETERS lcStatus,lcDirection
lcDirection = IIF(TYPE('lcDirection')#'C','C',lcDirection)
lnStatus = ASCAN(laStatus,lcStatus)
IF lnStatus > 0
  RETURN IIF(lcDirection='C',laStatus[ASUBSCR(laStatus,lnStatus,1),1],;
             ASUBSCR(laStatus,lnStatus,1)) 
ELSE
  RETURN IIF(lcDirection='C',PADR('N/A',9),0)
ENDIF

FUNCTION lfvStatus
*B604531,4 AME [Start] the popUp value is numeric so we'll use it without calling
*B604531,4 AME         function  lfGetStat.
*IF !EMPTY(puStatus) AND laStatus[lfGetStat(puStatus,'N'),2]<>'W' AND;
   laStatus[lfGetStat(puStatus,'N'),2]<>SYUBSRVJ.cStatus 
IF !EMPTY(puStatus) AND laStatus[puStatus,2]<>'W' AND;
   laStatus[puStatus,2]<>SYUBSRVJ.cStatus 
*B604531,4 [End]
  SELECT SYUBSRVJ
  *B604531,4 AME [Start] the popUp value is numeric so we'll use it without calling
  *B604531,4 AME         function  lfGetStat.
  *REPLACE cStatus WITH laStatus[lfGetStat(puStatus,'N'),2]
  REPLACE cStatus WITH laStatus[puStatus,2]
  *B604531,4 [End]  
  =lfCountJobjs()
  =lfRefresh()
  SHOW WINDOW (lcBrTitle) REFRESH SAME
ELSE
  *B604531,4 AME [Start]pustatus to be numeric not character
  *puStatus = lfGetStat(SYUBSRVJ.cStatus)
  puStatus = lfGetStat(SYUBSRVJ.cStatus,'N')
  *B604531,4 AME [End]
  SHOW GET PUSTATUS
ENDIF  
  
FUNCTION lfvSort
SELECT SYUBSRVJ
DO CASE
  CASE rbSort = 1
   SET ORDER TO TAG USERID
  CASE rbSort = 2
   SET ORDER TO TAG PERIORITY
  CASE rbSort = 3
   SET ORDER TO TAG STATUS
  CASE rbSort = 4      
   SET ORDER TO TAG CTIME
ENDCASE
SHOW WINDOW (lcBrTitle) REFRESH SAME


FUNCTION lfwGetServ
=lfGetServ()
puserver = ALLT(SYUBSERV.cpServerId)+' '+ALLT(SYUBSERV.cserverdes)+ ' Current Server'
SHOW GET PUSERVER


FUNCTION lfvGetServ
IF puServer <> ALLT(SYUBSERV.cpServerId)+' '+ALLT(SYUBSERV.cserverdes)+ ' Current Server'
  lnSrvNo = ASCAN(laServer,puServer)
  IF lnSrvNo>0
    DO (gcScrDir+gcWinAppl+'\smgetSrv.spx') WITH puServer,laServer[ASUB(laServer,lnSrvNo,1),2]
    =lfCountJobjs()
    =lfRefresh()
    SHOW WINDOW (lcBrTitle) REFRESH SAME
  ENDIF  
ENDIF


FUNCTION lfvOutPut
*B604531,4 AME [Start]
*IF PUOUTPUT<>ALLT(SYUBSRVJ.MOUTPUT)
IF laOutPut[PUOUTPUT]<>ALLT(SYUBSRVJ.MOUTPUT)
*B604531,4 AME [End] 
  SELECT SYUBSRVJ
  *B604531,4 AME [Start] The popup takes numeric value.
  *REPLACE MOUTPUT WITH ALLT(PUOUTPUT)
  REPLACE MOUTPUT WITH ALLT(laOutPut[PUOUTPUT])
  *B604531,4 AME [End]
  SHOW WINDOW (lcBrTitle) REFRESH SAME
ENDIF


FUNCTION lfGetServ
PRIVATE lnRecNo,lnRePro,lcMultiLocks,lcExtended,lnCurArea
lnCurArea = SELECT()
DIMENSION laServer[1,2]
STORE '' TO laServer
SELECT SYUBSERV
lnRecNo = RECNO()
lnRePro = SET('REPRO')
SET REPROCESS TO 5
*lcMultiLocks = SET('MULTILOCKS')
*SET MULTILOCKS OFF
GO TOP
SCAN
  IF !EMPTY(laServer[1,1])
    DIMENSION laServer[ALEN(laServer,1)+1,2]  
  ENDIF  
  lcExtended = ''
  IF RLOCK()
    IF RECNO() <> lnRecNo
      UNLOCK
*      =LOCK(STR(lnRecNo),'SYUBSERV')
      lcExtended = ' Idle Server'
    ELSE  
      lcExtended = ' Current Server'
    ENDIF  
  ENDIF  
  laServer[ALEN(laServer,1),1] = ALLT(cpServerId)+' '+ALLT(cserverdes)+lcExtended
  laServer[ALEN(laServer,1),2] = ALLTR(cpServerId)
ENDSCAN
*SET MULTILOCKS &lcMultiLocks
SET REPROCESS TO  lnRePro
GO lnRecNo
=RLOCK()
SELECT (lnCurArea)




FUNCTION lfvReadCycl
PARAMETERS llFromButt

llFromButt = IIF(TYPE('llFromButt')='U',.F.,llFromButt)
IF READKEY(1) = 6 OR llFromButt
  *E301516,1 (Begin) Do Scgeduled program.
  IF llResume AND NET_LOC = 'S'
  
    *B607307,1 AMH Run the first open program [Start]
    *IF DATE()>=NextDate AND TIME()>=NextTime AND CSTATUS <> 'C'
    SELECT SYUBSRVJ
    LOCATE FOR CPSERVERID+CSTATUS = PADR(lcCompName,18)+'O' .AND.;
               DATE() >= NextDate .AND. TIME() >= NextTime
    IF FOUND()
    *B607307,1 AMH [End]
    
      lnOldMemW = SET("MEMOWIDTH")
      SET MEMOWIDTH TO 100
      ** A variable to hold the time of running the program.
      lcTSch=MLINE(mSchedInfo,MEMLINE(mSchedInfo))      
      lnOnce=ATC('ONCE',UPPER(lcTSch))
      lnWeek=ATC('WEEKLY',UPPER(lcTSch))
      lnDaily=ATC('DAILY',UPPER(lcTSch))
      lnMonthly=ATC('MONTHLY',UPPER(lcTSch))
      IF USED('Prsonlty')
        SELECT Prsonlty
        USE
      ENDIF
      IF USED('Printers')
        SELECT Printers
        USE
      ENDIF
      *SELECT SyuBtSrv
      SELECT syubsrvj
      *lnRecNo=RECNO()
      lnJobRecNo = RECNO()
      *B604531,4  AME [Start] restor OG var for rebalance.
      *--If user entered any options  restore them.
      IF !EMPTY(MUSRDVAR)
        RESTORE FROM MEMO MUSRDVAR ADDI
      ENDIF  
      *B604531,4  AME [End]
      
      *B607307,1 AMH Save the lcCmopName Varible before run the program [Start]
      lcSaveComp = lcCompName
      lcSaveFltr = lcFilter
      *B607307,1 AMH [End]
      
      FOR I = 1 TO MEMLINES(mRepoVars)
        lcCommand=' '+ALLTRIM(MLINE(mRepoVars,I))
        *USE
        &lcCommand.
        =gfOpenFile (gcSysHome+'syubsrvj',gcSysHome+'Userid','SH')
        IF lnJobRecNo>RECCOUNT()
          GO BOTTOM
        ELSE
          GOTO lnJobRecNo
        ENDIF
      ENDFOR   
      
      *B607307,1 AMH Restore the lcCmopName Varible after run the program [Start]
      lcCompName = lcSaveComp
      lcFilter   = lcSaveFltr
      *B607307,1 AMH [End]
      
      DO CASE
        CASE lnOnce>0
          REPLACE NextTime WITH SPACE(11),cStatus WITH 'C'
          lcNextDate={}
        CASE lnWeek>0
          lcNextDate=NextDate+7
        CASE lnDaily>0
          lcDays=ALLTRIM(SUBSTR(lcTsch,lnDaily+18,7))
          FOR I=1 TO 7
            lcAprDay=lfDay(UPPER(CDOW(IIF(EMPTY(NextDate),DATE(),NextDate)+I)))
            IF lcAprDay $ lcDays
              lcNextDate=NextDate+I
              EXIT
            ENDIF
          ENDFOR
          *B604531,4 AME [Start] in case of not choosing any day.
          IF TYPE('lcNextDate') = 'U'
            lcNextDate = {}
          ENDIF
          *B604531,4 AME [End]
        CASE lnMonthly>0
          lcNextMonth=IIF(MONTH(NextDate)=12,1,MONTH(NextDate)+1)
          lcNextYear =IIF(MONTH(NextDate)=12,YEAR(NextDate)+1,YEAR(NextDate))
          lcNextDay  =ALLTRIM(STR(DAY(NextDate)))
          lcNextMonth=ALLTRIM(STR(lcNextMonth))
          lcNextYear =ALLTRIM(STR(lcNextYear))
          lcNextDate =CTOD(lcNextMonth+'/'+lcNextDay+'/'+lcNextYear)
      ENDCASE
      REPLACE NextDate WITH lcNextDate
      SET MEMOWIDTH TO lnOldMemW
      
      *B607307,1 AMH Don't complete the job since can run again in the next date/time [Start]
      *REPLACE CSTATUS WITH 'C'
      *B607307,1 AMH [End]
      
      IF !WEXIST(lcBrTitle)
        =lfBrowJob()
      ENDIF
    ENDIF
  ELSE
  *E301516,1 (End)
    IF llResume OR llFromButt
      SELECT SYUBSRVJ
      IF !llFromButt
        LOCATE FOR CPSERVERID+CSTATUS = PADR(lcCompName,18)+'O'
      ENDIF
      IF FOUND() OR llFromButt
        lnJobRecNo = RECNO()
        lcActComp = cComp_ID
        lcActMod  = cApp_ID
        lcReport  = cRep_ID
        IF gcAct_Appl <> 'SM'
          =gpChngModl('SM')
        ENDIF
        IF gcAct_Comp <> lcActComp
          lcOldProg = lcProgName
          lcProgName = ''
          =gpChangCom(lcActComp)
          lcProgNam =lcOldProg
        ENDIF
        IF gcAct_Appl <> lcActMod
          =gpChngModl(lcActMod)      
        ENDIF  
        =lfSetPrint(ALLT(SYUBSRVJ.MOUTPUT))
        LBTCError = ON('ERROR')
        LBTERROR = .F.
        ON ERROR DO SMBTERR WITH gcRepHome+gcWinAppl+"REPORT",ERROR(),MESSAGE()
        lcMdRports = gcRepHome+gcWinAppl+"REPORT"
        DO &lcMdRports WITH lcReport,.F.,'BP'
        ON ERROR &LBTCError
        IF !LBTERROR
          SELECT SYUBSRVJ
          GO lnJobRecNo
          REPLACE CSTATUS WITH 'C'
        ENDIF  
        IF !WEXIST(lcBrTitle)
          =lfBrowJob()
        ENDIF
      ENDIF
    ENDIF
  *E301516,1 (Begin) Do Scgeduled program.
  ENDIF
  *E301516,1 (End)
   
  =lfCountJobjs()
  =lfRefresh()
  SHOW WINDOW (lcBrTitle) REFRESH SAME  
  RETURN .F.
ENDIF

FUNCTION lfCountJobjs
SELECT SYUBSRVJ
PRIVATE lnCurRec 
lnCurRec = RECNO()
COUNT FOR CPSERVERID+CSTATUS = PADR(lcCompName,18) TO lnTotal
COUNT FOR CPSERVERID+CSTATUS = PADR(lcCompName,18)+'C' TO lnCompleted
COUNT FOR CPSERVERID+CSTATUS = PADR(lcCompName,18)+'O' TO lnQueue
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GO lnCurRec
ELSE
  GO TOP  
ENDIF


FUNCTION lfvPrinters

*B604531,4 AME [Start] the screen is not exist at all.
*DO (gcScrDir+gcWinAppl+'\smsrvprt.spx')
=SYS(1037)
*B604531,4 AME [End]

FUNCTION lfvShow
lcActComp = cComp_ID
lcActMod  = cApp_ID
lcReport  = cRep_ID
IF gcAct_Appl <> 'SM'
  =gpChngModl('SM')
ENDIF
IF gcAct_Comp <> lcActComp
  lcOldProg = lcProgName
  lcProgName = ''
  =gpChangCom(lcActComp)
  lcProgNam =lcOldProg
ENDIF
IF gcAct_Appl <> lcActMod
  =gpChngModl(lcActMod)      
ENDIF  
LBTError = ON('ERROR')
ON ERROR RETURN TO gcRepHome+gcWinAppl+"REPORT"
lcMdRports = gcRepHome+gcWinAppl+"REPORT"
DO &lcMdRports WITH lcReport,.F.,'BV'
ON ERROR &lbtError
SELECT SYUBSRVJ
IF !WEXIST(lcBrTitle)
  =lfBrowJob()
ENDIF



FUNCTION lfSetPrint
PARAMETERS lcWPrinter
lnPrtPos = ASCAN(laPrinters,lcWPrinter)
KEYBOARD "{ENTER}"+"{ENTER}"
=PRTWIN(lcWPrinter)
lcWPrinter = PRTWIN(lcWPrinter)
CLEAR TYPEAHEAD
glPrnt_Lan = .F.
IF lnPrtPos >0
  lnPrtPos = ASUBS(laPrinters,lnPrtPos,1)
  _PDSETUP = laPrinters[lnPrtPos,4]
  gnPrint_Wd = LOOKUP(SYCPRINT.NPrint_Wid,UPPER(laPrinters[lnPrtPos,4]),SYCPRINT.cprint_id,'cprint_id')
  gnPrint_Wd = MAX(gnPrint_Wd,80)
ENDIF  
gcPrnt_Port = STRTRAN(SUBSTR(lcWPrinter,ATC(' ON ',lcWPrinter)+4),':')


FUNCTION lfRsetPrint
_PDSETUP = lbtPdSetup   
glPrnt_Lan = lbtPrnt_LAN 
gcPrnt_Port = lbtPrntPort
gnPrint_Wd = lbtPrnt_wd


FUNCTION SMBTERR
PARAMETERS lcReturnTo,lnError,lcMessage
ON ERROR
LBTERROR = .T.
SELECT SYUBSRVJ
GO lnJobRecNo
REPLACE CSTATUS WITH 'E',;
        MMESSAGE WITH 'Error #'+ALLT(STR(lnError)) ADDI,;
        MMESSAGE WITH lcMessage ADDI
RETURN TO gcRepHome+gcWinAppl+"REPORT"




FUNCTION gfModalGen
PARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg
PRIVATE lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcCurrDbf,lnButtons,lcDlgMessg
DECLARE laVarsStr[1]         && Array to hold variabels to be replaced
                             && in the messag

PUSH KEY
ON KEY
lcCurrDbf   = ALIAS()
lcDlgID     = IIF(TYPE('lcDlgID'   ) $ "UL",'',lcDlgID   )
lcDlgTyp    = IIF(TYPE('lcDlgTyp'  ) $ "UL",'D',lcDlgTyp )
lcVarsStr   = IIF(TYPE('lcVarsStr' ) $ "UL",'',lcVarsStr )
lcDlgValid  = IIF(TYPE('lcDlgValid') $ "UL",'',lcDlgValid)
lcDlgMessg  = IIF(TYPE('lcDlgMessg') $ "UL",'',lcDlgMessg)
lcMsgCatgry = SUBSTR(lcDlgID,1,2)
lnOldRec    = 0
lnTextStrt  = IIF(EMPTY(lcDlgMessg),7,2)

IF EMPTY(lcDlgMessg)
  *** Open dialog file if it is not opend
  *E301077,78 Hesham (Start)
  *IF !USED('SydDlObj')
  *  SELECT 0
  *  USE (gcSysHome+"SydDlObj") 
  *ENDIF
  *SET ORDER TO TAG CDLOBJID IN ALIAS('SydDlObj')  
  =gfSysOpen(gcSysHome+'SydDlObj',"CDLOBJID")
  *E301077,78 Hesham (End)

  lnOldRec = RECNO('SydDlObj')

  *** Get Dialog messag
  IF SEEK(SUBSTR(lcDlgID,3,6),'SydDlObj')
    lcMessag    = ALLTRIM(SydDlObj.mDlObj)
    lcMsgCatgry = IIF(EMPTY(SydDlObj.cMsgCatgry),lcMsgCatgry,SydDlObj.cMsgCatgry)
  ELSE
    *** If not found set default message to null
    lcMessag = ""
  ENDIF


  *** Collect variables to be replaced from string to array
  =gfSubStr(lcVarsStr,@laVarsStr,'|')

  *** Replace each ð mark with variabe sent
  FOR lnVarsStr = 1  TO ALEN(laVarsStr,1)
    lcMessag = STRTRAN(lcMessag,'ð',laVarsStr[lnVarsStr],1,1)
  ENDFOR  
ELSE
  lcMessag   = ALLTRIM (lcDlgMessg)
ENDIF

  
*** When the read is activated fox will activate the window 
IF lnOldRec > 0  .AND. lnOldRec <= RECCOUNT('SydDlObj')
  GO lnOldRec IN ALIAS('SydDlObj')
ENDIF

*E301516,1 (Begin) Open syubsrvj if SMUTILS.PRG closes it when  excutiving a program.
=gfOpenFile (gcSysHome+'syubsrvj',gcSysHome+'Userid','SH')
*E301516,1 (End)
SELECT SYUBSRVJ
GO lnJobRecNo
REPLACE MMESSAGE WITH lcMessag ADDI,;
        CSTATUS WITH 'E'

IF !EMPTY(lcCurrDbf)
  SELECT (lcCurrDbf)
ENDIF
POP KEY
RETURN TO gcRepHome+gcWinAppl+"REPORT"



FUNCTION gfDialog
PARAMETER lcDlgTyp,lcMsg,lcButton
=gfModalGen(.f.,.f.,.f.,.f.,lcMsg)

*!*************************************************************
*! Name      : lfGetStr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 29/01/2001
*! Purpose   : Update program description
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfGetStr()
*!*************************************************************
*E301516,1
FUNCTION lfGetStr

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

*B607307,1 AMH Don't use this cursor to display the filed fo report/program name [Start]
*REPLACE PROGNAME.name WITH lcSched
RETURN lcSched
*B607307,1 AMH [End]

*!*************************************************************
*! Name      : lfDay
*! Developer : Ahmed Mohamed ElAnwer (AME)
*! Date      : 07/09/2001
*! Purpose   : Function to return a letter that indicate the day.
*!*************************************************************
*! Passed Parameters  :  lcToDay.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  A character the indicate the day.
*!*************************************************************
*! Example            :  =lfDay(UPPER(CDOW(DATE()+I)))
*!*************************************************************
*B604531,4 AME 
FUNCTION lfDay
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