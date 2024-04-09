*************************************************************************
*:Modifications  :
*************************************************************************
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B301269,1 Reham On 06/21/1999
*B301269,1 Add the customer file in the files to be open to add 
*B301269,1 the customer name in the browse.
*B803387,1 BWA 07/04/2000 1)Change the sorting of the data in the tasklist 
*B803387,1                  screen due to the trandate. 
*B803387,1 RAMY 07/24/2000 Fix some bugs in the screen and add new buttons to
* 						   1- Open the Customer or the vendor screnn due to the entry type
* 						   2- Complete the task
* 						   3- Cancel the task
*B603731,1 RAMY 07/24/2000 When the browse is activated and we open any screen we found the 
*                          objects of that screen are disabled
*B604251,1 HBG 21/02/2001  Fix bugs :
*     							1 - Window has not been define
*     							2 - If scap when you are in the browse , window still exist
*C200249,7 AMH 01/29/2002 Make the todo folder to be the default folder for MBI.
*C200376,1 AMH 08/12/2002 Make the todo folder to be the default folder for PUF.
*B606063,1 SSE 07/16/2002 Fix bug of not closing screen from control box on title bar on the screen.
*B606063,1                All modifications in screen set.  
*N000341,1 HBG 01/01/2002 Use Workflow folder to track the tasks of the user
*E301869,2 AMH 11/19/2002 Create a production schedule project according to a setting.
*B606922,1 ASH 01/29/2002 Fix problem of displaying all user lists including the completed one.
*************************************************************************

DIMEN lafoldwinds[5,2],laUsers[1]
STORE '' TO lafoldwinds,lcUserID,lcUserName,laUsers,lctrantype,lcOnEsc
STORE 0 TO lnTotal,lnQueue,lnComplete,lnLastFold,lnActFolder
STORE '' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcFolder,lcBrTitle;
            ,lcFilter,laUsers

*-B803387,1 Ramy [start]
STORE '' TO lcWinCh4
*-B803387,1 Ramy [end]
*B803387,1 BWA 07/04/2000 1)Change the sorting of the data in the tasklist 
*B803387,1                  screen due to the trandate.[START]
PRIVATE lcOrder
*B803387,1 [END]

llNoContrl = .T.

*N000341,1 HBG 01/01/2002 Variabels of Redirect Screen[Begin] 
DIMENSION laOper[1,2]
laOper[1,1] = ' '
STORE '' TO lcOper,lcoprt_dsc,lcoprt_res,m.mNotes
STORE 0 TO lnPerct,lnRemin
lc_PrjAudt = ''
glUpdated  = .T.
lnStart = 0
lcUsrStat = ''
*N000341,1 [End]

IF !gfSetup()
  EXIT
ENDIF


IF !WEXIST(gcBaseWind)
  STORE 5 TO lnLastFold,lnActFolder
  
  *C200249,7 AMH Make the todo folder to be the default folder for MBI [Start]
  IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
    STORE 3 TO lnLastFold,lnActFolde 
  ENDIF     
  *C200249,7 AMH [End]
  
  SELECT DIST CUSER_ID ;
    FROM (gcSysHome+'syuuser');
    INTO ARRAY laUsers
  IF EMPTY(laUsers[1])  
    RETURN
  ENDIF

  lcUserID = gcUser_ID
  lcUserName = LOOKUP(syuuser.cusr_name,lcUserID,syuuser.cuser_id,'CUSER_ID')

  IF EMPTY(lcUserName)
    lcUserID = laUsers[1]
    lcUserName = LOOKUP(syuuser.cusr_name,lcUserID,syuuser.cuser_id,'CUSER_ID')    
  ENDIF



  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()  
  lcFolder = gfTempName()
  *-B803387,1 Ramy [start]
  lcWinCh4 = gfTempName()  
  *-B803387,1 Ramy [end]
  
  *N000341,1 HBG 01/01/2002 Add file foer Audit trail[Begin]
  lc_PrjAudt   = gfTempName()
  
  =gfOpenFile(gcDataDir+'PMPRJDT','PMPRJDTS')  
  SELECT PMPRJDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru, 1)
  DIMENSION laFileStru(lnFileStru + 2, 4)
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cStatus'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nRecNo'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 10
  laFileStru[lnFileStru ,4] = 0
  CREATE CURSOR (lc_PrjAudt) FROM ARRAY laFileStru
  INDEX ON cOprt_Ctg + cOprt_ID TAG PMPRJDT OF (lc_PrjAudt)
  *N000341,1 [End]
  
  
  IF SEEK(gcUser_ID,'SYUUSER') AND SYUUSER.cUsr_Levl = 'O'
    lcUsrStat = 'DISABLE'
  ELSE
    lcUsrStat = 'ENABLE'  
  ENDIF  

ENDIF  
DIMEN lafoldwinds[5,2]
lcfoldprnt  = gcBaseWind          && window parent name for the folder
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder

lnFolderCEnd= 98.000            && Folder window end column
lnfolderrend=   2.00            && Folder window end row

lctrantype = 'CATW '
lafoldwinds[1,1] = 'Calls'
lafoldwinds[1,2] = lcBrTitle
lafoldwinds[2,1] = 'Appointments'
lafoldwinds[2,2] = lcBrTitle
lafoldwinds[3,1] = 'ToDo'
lafoldwinds[3,2] = lcBrTitle
lafoldwinds[4,1] = 'Workflow'
lafoldwinds[4,2] = lcWinCh2
lafoldwinds[5,1] = 'All'
lafoldwinds[5,2] = lcBrTitle
lcBrTitle  = lafoldwinds[1,1]




LACTRSTAT[10] = "DISABLE"


DIMEN laField_H[5,1],laField_N[5,1]

laField_H[1,1] = 'Transaction Date'
laField_H[2,1] = 'Transaction Time'
laField_H[3,1] = 'Status'
laField_H[4,1] = 'Completion Date'
laField_H[5,1] = 'ID'

laField_N[1,1] = 'DTRANDATE'
laField_N[2,1] = 'CTRANTIME'
laField_N[3,1] = 'CCOMPLETED'
laField_N[4,1] = 'DCMPLTDATE'
laField_N[5,1] = 'CCONT_ID'


SELECT syschdul

*B803387,1 BWA 07/04/2000 1)Change the sorting of the data in the tasklist 
*B803387,1                  screen due to the trandate.[START]
lcOrder = ORDER()
SET ORDER TO SCHUSER ASCENDING
*B803387,1 [END]

*-B803387,1 Ramy move this line to the browse function [start]
*SET FILTER TO cUser_ID = (lcUserID) AND CTRANTYPE = (ALLT(SUBSTR(lctrantype,lnActFolder,1))) ;
             AND CCOMPLETED = 'N'
*-B803387,1 Ramy [end]

lcOnEsc = ON('KEY','ESCAPE')
ON KEY LABEL ESCAPE DO lfTermRead

*-B803387,1 Ramy [start]
*N000341,1 HBG 01/01/2002 Change the prompt of the buttons in case of dealing with workflow[Begin] 
*lcPrompt = IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor')
lcPrompt1 = IIF(CTRANTYPE='W','\<Apply',IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor'))
lcPrompt2 = IIF(CTRANTYPE='W','\<Redirect','\<Cancel')
*N000341,1 [End]
*-B803387,1 Ramy [end]

DO (gcScrDir+gcWinAppl+'\TASKLIST.spx')
ON KEY LABEL ESCAPE &lcOnEsc

RELEASE WINDOW (lafoldwinds[lnActFolder,1])
*B803387,1 BWA 07/04/2000 1)Change the sorting of the data in the tasklist 
*B803387,1                  screen due to the trandate.[START]
SET ORDER TO &lcOrder IN SYSCHDUL
*B803387,1 [END]


IF glQuitting
    RELEASE WINDOW (lcBaseWind)
ENDIF
*

SELECT syschdul
SET FILTER TO




*-------------------------------------------------------------
FUNCTION lfActFolder

*N000341,1 HBG 01/01/2002 Change the prompt of the buttons in case of dealing with workflow
*N000341,1                and enable or disable it[Begin] 
DO CASE
  CASE lnActFolder = 4
    lcPrompt1 = '\<Apply'
    lcPrompt2 = '\<Redirect'
    IF COPERSTAT = 'O'
      lnStart = 0
      SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
      SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
      SHOW GET pbComplt,1   DISABLE
      SHOW GET lnPerct DISABLE
      SHOW GET lnRemin DISABLE
      SHOW GET lnStart ENABLE
    ELSE
      lnStart = 1
      SHOW GET lnStart DISABLE
      IF lnRemin = 0 OR COPERSTAT = 'H' OR nEstDur = 0
        SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
      ELSE
        SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
      ENDIF
      IF COPERSTAT = 'H' OR nEstDur  = 0
        SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
        SHOW GET lnPerct DISABLE
        SHOW GET lnRemin DISABLE
      ELSE
        SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
        SHOW GET lnPerct ENABLE
        SHOW GET lnRemin ENABLE
      ENDIF
      IF COPERSTAT = 'H'
        SHOW GET pbComplt,1   DISABLE
      ELSE
        SHOW GET pbComplt,1   ENABLE
      ENDIF
    ENDIF  
  OTHERWISE
    lcPrompt1 = IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor')
    lcPrompt2 = '\<Cancel'
    SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
    SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
    SHOW GET pbComplt,1   ENABLE  
    SHOW GET lnPerct DISABLE
    SHOW GET lnRemin DISABLE  
    lnStart = 0
    SHOW GET lnStart DISABLE
ENDCASE
*N000341,1[End]

*IF !glquitting
  =lfBrowse()
*ENDIF
=lfRefObj()



*-------------------------------------------------------------
FUNCTION lfvUserID
lcUserName = LOOKUP(syuuser.cusr_name,lcUserID,syuuser.cuser_id,'CUSER_ID')
=lfRefresh()
SHOW WINDOW (lafoldwinds[lnActFolder,1]) REFRESH SAME


*-------------------------------------------------------------
FUNCTION lfBrowse
PRIVATE lcBrFields
*lcBrFields = [ccont_id:H='ID',contact:16,cphone:H="Phone",csubject:H="Subject":30,]+;
             [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
             [cTranTime:H='Time',Dcmpltdate:H='Completion Date']
             
*E300789,1 Hesham (Start)
*lcBrFields = [ccont_id:H='ID',contact:16,cphone:H="Phone",csubject:H="Subject":30,]+;
             [F=LOOKUP(CODES.cDiscrep,gcAct_Comp+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
             [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
             [cTranTime:H='Time',Dcmpltdate:H='Completion Date']

*B301269,1 Reham On 06/21/1999   *** Begin ***
*B301269,1 Add the customer name in the browse.
*lcBrFields = [ccont_id:H='ID',contact:16,cphone:H="Phone",csubject:H="Subject":30,]+;
             [F=LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
             [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
             [cTranTime:H='Time',Dcmpltdate:H='Completion Date']
*N000341,1 HBG 01/01/2002 Change the browse fields in case of dealing with workflow[Begin] 
*lcBrFields = [ccont_id:H='ID',Cust=LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER") :H="Name":22,contact:16,cphone:H="Phone",csubject:H="Subject":30,]+;
             [F=LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
             [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
             [cTranTime:H='Time',Dcmpltdate:H='Completion Date']
*N000341,1 [End]            
*B301269,1 Reham On 06/21/1999   *** End   ***



*E300789,1 Hesham (End)

*N000341,1 HBG 01/01/2002 Change the browse fields in case of dealing with workflow[Begin] 
*IF lnActFolde  = ALEN(laFoldWind,1)
*  lcBrFields = [ctype=laFoldWind(ATC(syschdul.CTRANTYPE,lctrantype),1):H='Type',]+lcBrFields
*ENDIF
*N000341,1 [End]

SELECT syschdul

*-B803387,1 Ramy [start]

*BROWSE FIELDS &lcBrFields WINDOW (lcWinCh1) IN WINDOW (gcBaseWind) TITLE lafoldwinds[lnActFolder,1];
       WHEN lfwBrow();
       Valid :F lfvBrow() ;
       NOMODIFY NOMENU NOAPPEND NODELETE NOCLEAR NOWAIT 

SET FILTER TO
*N000341,1 HBG 01/01/2002 Change the filter in case of dealing with workflow[Begin] 
*SET FILTER TO cUser_ID = (lcUserID)  AND CTRANTYPE = (ALLT(SUBSTR(lctrantype,lnActFolde,1))) ;
             AND CCOMPLETED = 'N'

*BROWSE FIELDS &lcBrFields WINDOW (lcWinCh1) IN WINDOW (gcBaseWind) TITLE laFoldWind[lnActFolde,1];
       WHEN lfwBrow();
       Valid :F lfvBrow() ;
       NOMODIFY NOMENU NOAPPEND NODELETE NOCLEAR NOWAIT ;
       FOR DtranDate =< gdSysDate
       
IF SEEK(lcUserID,'syuuser')
  SELECT SYSchdul
  lcOldKey = ORDER()
  SET ORDER TO 
  lcFilterExp = "(cuser_id+DTOS(dtrandate)+ccompleted+cconttype+ccont_id+store+ctrantime+"+;
                "cseqnumber = PADR(syuuser.cusr_grup,10) .OR."+;
                "cuser_id+DTOS(dtrandate)+ccompleted+cconttype+ccont_id+store+ctrantime+"+;
                "cseqnumber = PADR(lcUserId,10))"
  *B606922,1 ASH 01/29/2002 (Begin) Display all user lists in WORKFLOW folder only.
  *SET FILTER TO &lcFilterExp .AND. CTRANTYPE = ALLTRIM(SUBSTR(lctrantype,lnActFolde,1)) .AND. ;
                !(COPERSTAT $ 'CX') .AND. IIF(syschdul.CTRANTYPE = 'W',LPREDCOMP,.T.)
  
  IF lnActFolde = 4                
    SET FILTER TO &lcFilterExp .AND. CTRANTYPE = ALLTRIM(SUBSTR(lctrantype,lnActFolde,1)) .AND. ;
                  !(COPERSTAT $ 'CX') .AND. IIF(syschdul.CTRANTYPE = 'W',LPREDCOMP,.T.)
  ELSE
    SET FILTER TO &lcFilterExp .AND. CTRANTYPE = ALLTRIM(SUBSTR(lctrantype,lnActFolde,1)) .AND. ;
                  !(COPERSTAT $ 'CX') .AND. IIF(syschdul.CTRANTYPE = 'W',LPREDCOMP,.T.) .AND. CCOMPLETED = 'N'
  ENDIF                
  *B606922,1 ASH 01/29/2002 (End)
  LOCATE
  IF !EMPTY(lcOldKey)
    SET ORDER TO &lcOldKey
  ENDIF
ENDIF
lcPrvOrd = ORDER('SYSchdul')
IF lnActFolde = 4
  SET ORDER TO Coprusr IN SYSchdul
  *lcBrFields = [CSEQNUMBER:H='Project ID':12,ccont_id:H='Task ID':14,CSUBJECT:H='Task Name':30,]+;
               [DtranDate:H='Start Date':12,Dcmpltdate:H='Complete Date':14,NEstDur:H='Duration':12,]+;
               [COPERSTAT=IIF(COPERSTAT='O','Open',IIF(COPERSTAT='P','In Work',IIF(COPERSTAT='H','Hold','In Work'))):H='Task Status':17]
  lcBrFields = [cContType=IIF(cContType = 'C','C/T',]+;
               [IIF(cContType = 'P','P/O',IIF(cContType = 'A','Adorment P/O',]+;
               [IIF(cContType = 'D','Dye P/O',IIF(cContType = 'N','Inter-Location P/O',]+;
               [IIF(cContType = 'R','Return P/O',IIF(cContType = 'B','Contract P/O',]+;
               [IIF(cContType = 'O','S/O',IIF(cContType = 'T','Edi S/O',]+;
               [IIF(cContType = 'S','Contract S/O','')))))))))):H='Project Type':20,]+;
               [CSEQNUMBER:H='Project ID':12,ccont_id:H='Task ID':14,CSUBJECT:H='Task Name':30,]+;
               [DtranDate:H='Start Date':12,Dcmpltdate:H='Complete Date':14,NEstDur:H='Duration':12,]+;
               [COPERSTAT=IIF(COPERSTAT='O','Open',IIF(COPERSTAT='P','In Work',IIF(COPERSTAT='H','Hold','In Work'))):H='Task Status':17]               
ELSE
  SET ORDER TO &lcPrvOrd IN SYSchdul
  IF lnActFolde  = ALEN(laFoldWind,1)
    *lcBrFields = [CSEQNUMBER=IIF(syschdul.CTRANTYPE='W',CSEQNUMBER,''):H='Project ID':12,ccont_id:H='Task ID/ID':16,]+;
                 [Cust=LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER"):H="Name":22,]+;
                 [contact:16,cphone:H="Phone",csubject:H="Task Name/Subject":30,]+;
                 [F=LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
                 [Status=IIF(syschdul.CTRANTYPE='W','',IIF(cCompleted='Y','Completed','Open')),DtranDate:H='Start Date/Date',]+;
                 [cTranTime:H='Time',Dcmpltdate:H='Completion Date',NEstDur:H='Duration':12,]+;
                 [COPERSTAT=IIF(syschdul.CTRANTYPE='W',IIF(COPERSTAT='O','Open',]+;
                 [IIF(COPERSTAT='P','In Work',IIF(COPERSTAT='H','Hold','In Work'))),''):H='Task Status':17]
    *B606922,1 ASH 01/29/2002 (Begin) Display the project type and project ID in the last browse columns as per omar.
    *lcBrFields = [cContType=IIF(syschdul.CTRANTYPE='W',IIF(cContType = 'C','C/T',]+;
                 [IIF(cContType = 'P','P/O',IIF(cContType = 'A','Adorment P/O',]+;
                 [IIF(cContType = 'D','Dye P/O',IIF(cContType = 'N','Inter-Location P/O',]+;
                 [IIF(cContType = 'R','Return P/O',IIF(cContType = 'B','Contract P/O',]+;
                 [IIF(cContType = 'O','S/O',IIF(cContType = 'T','Edi S/O',]+;
                 [IIF(cContType = 'S','Contract S/O','')))))))))),''):H='Project Type':20,]

    *lcBrFields = lcBrFields + [CSEQNUMBER=IIF(syschdul.CTRANTYPE='W',CSEQNUMBER,''):H='Project ID':12,ccont_id:H='Task ID/ID':16,]+;
                 [Cust=LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER"):H="Name":22,]+;
                 [contact:16,cphone:H="Phone",csubject:H="Task Name/Subject":30,]+;
                 [F=LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
                 [Status=IIF(syschdul.CTRANTYPE='W','',IIF(cCompleted='Y','Completed','Open')),DtranDate:H='Start Date/Date',]+;
                 [cTranTime:H='Time',Dcmpltdate:H='Completion Date',NEstDur:H='Duration':12,]+;
                 [COPERSTAT=IIF(syschdul.CTRANTYPE='W',IIF(COPERSTAT='O','Open',]+;
                 [IIF(COPERSTAT='P','In Work',IIF(COPERSTAT='H','Hold','In Work'))),''):H='Task Status':17]                 
   

    lcBrFields = [ccont_id:H=IIF(syschdul.CTRANTYPE='W','Task ID/ID','ID'):16,]+;
                 [Cust=LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER"):H="Name":22,]+;
                 [contact:16,cphone:H="Phone",csubject:H="Task Name/Subject":30,]+;
                 [F=LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
                 [Status=IIF(syschdul.CTRANTYPE='W','',IIF(cCompleted='Y','Completed','Open')),DtranDate:H='Start Date/Date',]+;
                 [cTranTime:H='Time',Dcmpltdate:H='Completion Date',NEstDur:H='Duration':12,]+;
                 [COPERSTAT=IIF(syschdul.CTRANTYPE='W',IIF(COPERSTAT='O','Open',]+;
                 [IIF(COPERSTAT='P','In Work',IIF(COPERSTAT='H','Hold','In Work'))),''):H='Task Status':17,] 
   
    lcBrFields = lcBrFields + [cContType=IIF(syschdul.CTRANTYPE='W',IIF(cContType = 'C','C/T',]+;
                 [IIF(cContType = 'P','P/O',IIF(cContType = 'A','Adorment P/O',]+;
                 [IIF(cContType = 'D','Dye P/O',IIF(cContType = 'N','Inter-Location P/O',]+;
                 [IIF(cContType = 'R','Return P/O',IIF(cContType = 'B','Contract P/O',]+;
                 [IIF(cContType = 'O','S/O',IIF(cContType = 'T','Edi S/O',]+;
                 [IIF(cContType = 'S','Contract S/O','')))))))))),''):H='Project Type':20,]+;
                 [CSEQNUMBER=IIF(syschdul.CTRANTYPE='W',CSEQNUMBER,''):H='Project ID':12]              
    *B606922,1 ASH 01/29/2002 (End)                 
    lcBrFields = [ctype=laFoldWind(ATC(syschdul.CTRANTYPE,lctrantype),1):H='Type':10,]+lcBrFields
  ELSE
    lcBrFields = [ccont_id:H='ID',Cust=LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER") :H="Name":22,contact:16,cphone:H="Phone",csubject:H="Subject":30,]+;
                 [F=LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"):H="Reason",]+;
                 [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
                 [cTranTime:H='Time',Dcmpltdate:H='Completion Date']
    
    *C200249,7 AMH Browse spicefic files for MBI [Start]
    IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
      lcBrFields = [ccont_id:H='ID',po:H='Tran. ID',Cust=IIF(EMPTY(PO),LOOKUP(CUSTOMER.BtName,"M"+cCont_Id,CUSTOMER.ACCOUNT,"CUSTOMER"),STYLE) :H="Name":22,]+;
                   [contact:16,cphone:H="Phone",lcSubject=IIF(EMPTY(PO),csubject,CCRPTHOPR):H="Subject":30,]+;
                   [F=IIF(EMPTY(PO),LOOKUP(CODES.cDiscrep,'N'+'CTRANRESON'+CTRANRESON,CODES.cCode_No,"CCODE_NO"),CTASKRESN):H="Reason",]+;
                   [Status=IIF(cCompleted='Y','Completed','Open     '),DtranDate:H='Date',]+;
                   [cTranTime:H='Time',Dcmpltdate:H='Completion Date']
    ENDIF
    *C200249,7 AMH [End]
    *N000341,1 HBG 01/01/2002 Disable the check box of start n other folder than workflow or all[Begin] 
    SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
    SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
    SHOW GET pbComplt,1   ENABLE  
    SHOW GET lnPerct DISABLE
    SHOW GET lnRemin DISABLE  
    lnStart = 0
    SHOW GET lnStart DISABLE
    *N000341,1 [End]
  ENDIF         
ENDIF         

*C200376,1 AMH Browse spicefic files for PUF [Start]
IF ASCAN(laEvntTrig , PADR('TODOPUF',10)) <> 0
  =gfDoTriger('TASKLST',PADR('TODOPUF',10))
ENDIF
*C200376,1 AMH [End]

BROWSE FIELDS &lcBrFields WINDOW (lcWinCh1) IN WINDOW (gcBaseWind) TITLE laFoldWind[lnActFolde,1];
       WHEN lfwBrow();
       Valid :F lfvBrow() ;
       NOMODIFY NOMENU NOAPPEND NODELETE NOCLEAR NOWAIT ;
       FOR DtranDate =< gdSysDate

*N000341,1 [End]

=lfRefObj()
*-B803387,1 Ramy [end]
*-------------------------------------------------
FUNCTION lfwBrow
glFromBrow = .T.
*-B803387,1 Ramy [start]
*N000341,1 HBG 01/01/2002 Change the prompt of the buttons in case of dealing with workflow
*N000341,1                and enable or disable it[Begin] 
*lcPrompt = IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor')
*SHOW GET pbCustVend,1 PROMPT lcPrompt ENABLE
lcPrompt1 = IIF(CTRANTYPE='W','\<Apply',IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor'))
lcPrompt2 = IIF(CTRANTYPE='W','\<Redirect','\<Cancel')
IF CTRANTYPE ='W' 
  IF COPERSTAT = 'O'
    lnStart = 0
    SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
    SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
    SHOW GET pbComplt,1   DISABLE
    SHOW GET lnPerct DISABLE
    SHOW GET lnRemin DISABLE
    SHOW GET lnStart ENABLE
  ELSE
    lnStart = 1
    SHOW GET lnStart DISABLE
    IF lnRemin = 0 OR COPERSTAT = 'H' OR nEstDur = 0
      SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
    ELSE
      SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
    ENDIF
    IF COPERSTAT = 'H' OR nEstDur = 0
      SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
      SHOW GET lnPerct DISABLE
      SHOW GET lnRemin DISABLE
    ELSE
      SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
      SHOW GET lnPerct ENABLE
      SHOW GET lnRemin ENABLE
    ENDIF
    IF COPERSTAT = 'H'
      SHOW GET pbComplt,1   DISABLE
    ELSE
      SHOW GET pbComplt,1   ENABLE
    ENDIF
  ENDIF  
ELSE
  SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
  SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
  SHOW GET pbComplt,1   ENABLE  
  SHOW GET lnPerct DISABLE
  SHOW GET lnRemin DISABLE  
  lnStart = 0
  SHOW GET lnStart DISABLE
ENDIF
*N000341,1 [End]
*B604251,1 HBG 21/02/2001 Show the correct window,to fix bug of Window has not been define [Begin]
*SHOW WINDOW (lcWinCh4) REFRESH SAME
SHOW WINDOW (lcWinCh1) REFRESH SAME
*B604251,1 [End]
=lfRefObj()
*-B803387,1 Ramy [end]
*-------------------------------------------------
FUNCTION lfvBrow

*B603731,1 Ramy [start]
IF !WVISIBLE(lcBaseWind)
  RELEASE WINDOW (lcBaseWind)
  glquitting = .T.
  CLEAR READ
  RETURN TO TASKLST.PRG
ELSE  
  IF !WONTOP(lafoldwinds[lnActFolder,1])
    IF !(WPARENT(WONTOP()) = lcBaseWind) AND WONTOP()<>lcBaseWind AND !WREAD()
      DEACT WINDOW (lafoldwinds[lnActFolder,1])
      =gfStatic()
      CLEAR READ
      RETURN TO TASKLST.PRG
    ELSE
      =gfStopBrow()
      glFromBrow = .F.
    ENDIF  
  ENDIF
ENDIF
*B603731,1 Ramy [end]

*B803387,1 Ramy [start]
*N000341,1 HBG 01/01/2002 Change the prompt of the buttons in case of dealing with workflow
*N000341,1                and enable or disable it[Begin] 
*lcPrompt = IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor')
*SHOW GET pbCustVend,1 PROMPT lcPrompt ENABLE
lcPrompt1 = IIF(CTRANTYPE='W','\<Apply',IIF(cContType = 'C' , '\<Customer' ,  '\<Vendor'))
lcPrompt2 = IIF(CTRANTYPE='W','\<Redirect','\<Cancel')
IF CTRANTYPE ='W' 
  IF lnRemin = 0 OR COPERSTAT = 'H' OR nEstDur = 0
    SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
  ELSE
    SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
  ENDIF
  IF COPERSTAT = 'H' OR nEstDur  = 0
    SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
    SHOW GET lnPerct DISABLE
    SHOW GET lnRemin DISABLE
  ELSE
    SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
    SHOW GET lnPerct ENABLE
    SHOW GET lnRemin ENABLE
  ENDIF
  IF COPERSTAT = 'H'
    SHOW GET pbComplt,1   DISABLE
  ELSE
    SHOW GET pbComplt,1   ENABLE
  ENDIF
ELSE
  SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
  SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
  SHOW GET pbComplt,1   ENABLE  
  SHOW GET lnPerct DISABLE
  SHOW GET lnRemin DISABLE  
ENDIF
*N000341,1 [End]
*B604251,1 HBG 21/02/2001 Show the correct window,to fix bug of Window has not been define [Begin]
*SHOW WINDOW (lcWinCh4) REFRESH SAME
SHOW WINDOW (lcWinCh1) REFRESH SAME
*B604251,1 [End]
=lfRefObj()
*B803387,1 Ramy [end]

*-------------------------------------------------
*FUNCTION lfTermRead
*KEYBOARD "{CTRL+F4}"
*=GFESCAP()

FUNCTION lfTermRead
*KEYBOARD "{CTRL+F4}"
ACTIVATE WINDOW (lcWinCh4)
=gfEscap()

*-------------------------------------------------
FUNCTION lfClrRead
*KEYBOARD "{CTRL+F4}"
ACTIVATE WINDOW (lcWinCh4)
=gfEscap()


*-----------------------------------------------------------------------------
*! Name      : lfvGetInfo
*! Developer : RAMY MABROUK (RAM)
*! Date      : 07/24/2000
*! Purpose   : Open the Customer or the Vendor screen due to the current task
*-----------------------------------------------------------------------------
*! Calls       : gpDoProg().
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfvGetInfo()
*-----------------------------------------------------------------------------
*B803387,1 RAMY

FUNCTION lfvGetInfo

*N000341,1 HBG 01/01/2002 IF This is a workflow record, excute Apply function[Begin]
IF CTRANTYPE='W'
  =lfApplay()
ELSE
*N000341,1 [End]
  DO CASE
    CASE cContType = 'C'
      lcParameter = "'" + cCont_Id + "'" + IIF(EMPTY(STORE) , '' , " , '" + STORE + "'")
      =gfstatic()
      DO gpDoProg WITH 'AWRARCUST' , .F. , 'AR' , lcParameter

    CASE cContType = 'V'
      lcParameter = "'" + ccont_id + "'"
      =gfstatic()
      DO gpDoProg WITH 'AWRAPVENDR' , .F. , 'AP' , lcParameter
  ENDCASE
*N000341,1 HBG 01/01/2002 End if this is workflow record[Begin]   
ENDIF  
*N000341,1 [End]

*-----------------------------------------------------------------------------
*! Name      : lfvCncl
*! Developer : RAMY MABROUK (RAM)
*! Date      : 07/24/2000
*! Purpose   : Cancel the current task
*-----------------------------------------------------------------------------
*! Calls       : gfModalGen().
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfvCncl()
*-----------------------------------------------------------------------------
*B803387,1 RAMY
FUNCTION lfvCncl

*N000341,1 HBG 01/01/2002 IF This is a workflow record, excute Redirect function[Begin]
IF CTRANTYPE='W'
  =lfRedirect()
ELSE
*N000341,1 [End]
  *--Message : 40062
  *--Are you sure you want to delete the current open activity from pending?
  *--Button : 40000
  *--                                  Yes No
  IF gfModalGen('QRM40062B40000','ALERT','open|pending') = 1
    SELECT syschdul
    DELETE
  ENDIF
*N000341,1 HBG 01/01/2002 End IF This is a workflow record[Begin]
ENDIF
*N000341,1 [End]

=lfBrowse()
=lfRefObj()

*-----------------------------------------------------------------------------
*! Name      : lfvCmplt
*! Developer : RAMY MABROUK (RAM)
*! Date      : 07/24/2000
*! Purpose   : Complete the current task
*-----------------------------------------------------------------------------
*! Calls       : TASKCOM.PRG.
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfvCmplt()
*-----------------------------------------------------------------------------
*B803387,1 RAMY
FUNCTION lfvCmplt

*N000341,1 HBG 01/01/2002 IF This is a workflow record, excute Complete function[Begin]
IF CTRANTYPE='W'
  =lfComplete()
ELSE
*N000341,1 [End]
  lnTranType = IIF(cTranType = 'C' , 1 , IIF(cTranType = 'A' , 2 , 3))
  llComplete = .F.
  llFromTask = .T.
  llNewTask  = .F.
  lcTskTypWnd = "TASKTYP"

  DO (gcAppHome+gcWinAppl+'\TASKCOM.PRG') WITH 'M' , cSeqNumber , lnTranType

  DO CASE
    CASE llComplete .AND. !llNewTask
      llClrRead = .T.
      RELEASE WINDOW (lcTskTypWnd)
    CASE llComplete .AND. llNewTask
      llClrRead = .F.
      lcMode = "A"
      lcTskDat   = DATE()
      IF VAL(SUBSTR(TIME(),1,2)) < 12
        lcTskTim   = SUBSTR(TIME(),1,5) + " AM"
      ELSE
        lcHour     = IIF(EVAL(SUBSTR(TIME(),1,2))-12=0,;
                         SUBSTR(TIME(),1,2),ALLTRIM(STR(EVAL(SUBSTR(TIME(),1,2))-12)))
        lcTskTim   = IIF(LEN(lcHour)=1,'0','') + lcHour + SUBSTR(TIME(),3,3)+" PM"
      ENDIF
    
      lnTskDur   = 0
      lcComplete = "N"
  ENDCASE
*N000341,1 HBG 01/01/2002 End IF This is a workflow record[Begin]
ENDIF
*N000341,1 [End]

=lfBrowse()
=lfRefObj()



*-----------------------------------------------------------------------------
*! Name      : lfRefObj
*! Developer : RAMY MABROUK (RAM)
*! Date      : 07/24/2000
*! Purpose   : Refresh the enableing or the disableing of the last screen
*-----------------------------------------------------------------------------
*! Calls       : None.
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfRefObj()
*-----------------------------------------------------------------------------
*B803387,1 RAMY
FUNCTION lfRefObj

IF EOF('syschdul')
  SHOW GET pbCustVend,1 DISABLE
  SHOW GET pbComplt,1   DISABLE
  SHOW GET pbCancl,1    DISABLE
ELSE
  *N000341,1 HBG 01/01/2002 Change the prompt of the buttons in case of dealing with workflow
  *N000341,1                and enable or disable it[Begin] 
  *SHOW GET pbCustVend,1 ENABLE
  *SHOW GET pbComplt,1   ENABLE
  *SHOW GET pbComplt,1   ENABLE
  IF CTRANTYPE ='W' 
    IF COPERSTAT = 'O'
      lnStart = 0
      SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
      SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
      SHOW GET pbComplt,1   DISABLE
      SHOW GET lnPerct DISABLE
      SHOW GET lnRemin DISABLE
      SHOW GET lnStart ENABLE
    ELSE
      lnStart = 1
      SHOW GET lnStart DISABLE
      IF lnRemin = 0 OR COPERSTAT = 'H' OR nEstDur = 0
        SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
      ELSE
        SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
      ENDIF
      IF COPERSTAT = 'H' OR nEstDur  = 0
        SHOW GET pbCancl,1 PROMPT lcPrompt2 DISABLE
        SHOW GET lnPerct DISABLE
        SHOW GET lnRemin DISABLE
      ELSE
        SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
        SHOW GET lnPerct ENABLE
        SHOW GET lnRemin ENABLE
      ENDIF
      IF COPERSTAT = 'H'
        SHOW GET pbComplt,1   DISABLE
      ELSE
        SHOW GET pbComplt,1   ENABLE
      ENDIF
    ENDIF  
  ELSE
    SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
    SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
    SHOW GET pbComplt,1   ENABLE  
    SHOW GET lnPerct DISABLE
    SHOW GET lnRemin DISABLE  
    lnStart = 0
    SHOW GET lnStart DISABLE

  ENDIF
  *N000341,1 [End]
ENDIF

*!*************************************************************
*! Name      : lfvDeact
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*B604251,1

*-- Set the flag From browes to True if the browes is active , to
*-- stop this browes and when press scap

FUNCTION lfvDeact
IF WONTOP() = lcWinCh1
  glFromBrow = .T.
ELSE
  glFromBrow = .F. 
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lfApplay
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Valid function for Apply button
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfApplay()
*!*************************************************************
*N000341,1
FUNCTION lfApplay
PARAMETERS lcConttype,lcseqnumbr,lcCont_ID,lcUser_Id,lnEstDur,ldTranDate,lnRemining,;
           lcDate,lcDir,lcSDir,lcAppHome,lcTempName

IF TYPE('lcConttype') <> 'C' OR TYPE('lcseqnumbr') <> 'C' OR TYPE('lcCont_ID') <> 'C' OR;
   TYPE('lcUser_Id') <> 'C' OR TYPE('lnEstDur') <> 'N' OR;
   TYPE('ldTranDate') <> 'D' OR TYPE('lnRemining') <> 'N' OR TYPE('lcDate') <> 'D' OR;
   TYPE('lcDir') <> 'C' OR  TYPE('lcSDir') <> 'C' OR  TYPE('lcAppHome') <> 'C' 
  lcConttype  = SYSCHDUL.cConttype   
  lcseqnumbr  = SYSCHDUL.cSeqNumber
  lcCont_ID   = SYSCHDUL.cCont_ID
  lcUser_ID   = lcUserID
  lnEstDur    = SYSCHDUL.nEstDur
  ldTranDate  = SYSCHDUL.dTranDate
  lnRemining  = lnRemin
  lcDataDir   = gcDataDir
  lcSysDir    = gcSysHome
  lcDate      = gdSysDate
  lcAppHome   = gcAppHome
  lcTempName  = gfTempName()
ELSE
  lcDataDir   = lcDir
  lcSysDir    = lcSDir
  =lfOpenFile(lcSysDir,'SYSCHDUL','Coprusr')
ENDIF   

=lfOpenFile(lcDataDir,'PMPRJHD','PMPRJHD')
=lfOpenFile(lcDataDir,'PMPRJDT','PMPRJDTS')
=lfOpenFile(lcDataDir,'PMPRJRL','PMPRJRL')
=lfOpenFile(lcDataDir,'PMCALDT','PMCALDT')
=lfOpenFile(lcDataDir,'PMCALHD','PMCALHD')

lnWEnd = 0

SELECT PMPRJDT
SET ORDER TO PMPRJUSR
IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') 
  LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
                 lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
  IF !FOUND()
    =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
  ENDIF   
  
  lnEstDur   = PMPRJDT.nEst_Dur
  ldTranDate = IIF(EMPTY(PMPRJDT.dclc_strt),PMPRJDT.dEst_strt,PMPRJDT.dclc_strt)
  lnDur      = IIF(lcDate = ldTranDate,1,(lcDate - ldTranDate)+1)
  
  DECLARE laHolidays[1]
  laHolidays = ''
  lnRows  = 0

  IF SEEK(PMPRJDT.cCal_id,'PMCALDT')
    SELECT PMCALDT
    SCAN
      ldCal_HFrm = dCal_HFrm
      DO WHILE ldCal_HFrm <= dCal_HTo
        lnRows  = lnRows  + 1
        DIMENSION laHolidays[lnRows]
        laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
        ldCal_HFrm = ldCal_HFrm + 1
      ENDDO  
    ENDSCAN
  ENDIF
  
  lnWEnd = 0
  IF SEEK(PMPRJDT.cCal_id,'PMCALHD')
    ldOprtFsh = SYSCHDUL.dTranDate
    FOR lnI = 1 TO lnDur
       ldOprtFsh = ldOprtFsh + 1
       IF ldOprtFsh > lcDate
         EXIT
       ENDIF
       lnWEnd = lnWEnd + IIF(STR(DOW(ldOprtFsh),1) $ PMCALHD.cCal_WEnd .OR. ;
                     ASCAN(laHolidays,PMPRJDT.cCal_id+ DTOC(ldOprtFsh)) > 0 , 1 , 0)
    ENDFOR
  ENDIF
ENDIF

lnDay    = IIF(lcDate = ldTranDate,1,(lcDate - ldTranDate)+1-lnWEnd)
lnEstDur = (lnDay + lnRemining) - 1

SELECT SYSCHDUL
lcCurIndx = TAG()
SET ORDER TO Coprusr

IF SEEK(lcconttype+lcseqnumbr+lccont_id+'O','SYSCHDUL')
  REPLACE SYSCHDUL.cuser_id  WITH lcUser_id
ENDIF  


SELECT PMPRJDT
SET ORDER TO PMPRJUSR

lcStyle = SPACE(LEN(PMPRJDT.cStyle))
llUpdEstdr = .F.
IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
  LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
                 lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
  IF !FOUND()
    =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
  ENDIF   
  IF EMPTY(PMPRJDT.cOprt_Res)
    REPLACE PMPRJDT.cOprt_Res WITH lcUser_ID,;
            PMPRJDT.cGroup_ID WITH ' ' 
  ENDIF
  REPLACE PMPRJDT.nRem_Dur WITH lnEstDur
  IF PMPRJDT.nRem_Dur = PMPRJDT.nEst_Dur
    llUpdEstdr = .T.
    lnPrvEstDr = PMPRJDT.nEst_Dur
    REPLACE PMPRJDT.nEst_Dur WITH 0
  ENDIF
  lcStyle = PMPRJDT.cStyle
ENDIF

SELECT SYSCHDUL
lcPrvFlt = FILTER()
SET FILTER TO

SELECT PMPRJDT
SET ORDER TO PMPRJDTS

DO lfSchedule IN lcAppHome+"MFPROJ" WITH lcconttype,lcseqnumbr,lcStyle,ldTranDate,lcSysDir,lcTempName

SELECT SYSCHDUL
SET ORDER TO Coprusr

IF SEEK(lcconttype+lcseqnumbr+lccont_id+'O','SYSCHDUL')
  REPLACE SYSCHDUL.cOperStat WITH 'P'
ENDIF  


*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*--Update Audit trail
*DO CASE
*  CASE lcConttype = 'C'     && CT
*    lcEvent = 'RESCHDCT'
*  CASE lcConttype = 'P'      && PO
*    lcEvent = 'RESCHDPO'
*  CASE lcConttype = 'A'      && Adorment PO Order
*    lcEvent = 'RESCHDPA'      
*  CASE lcConttype = 'D'      && Dye PO Order 
*    lcEvent = 'RESCHDPD'      
*  CASE lcConttype = 'N'      && Inter-Location PO
*    lcEvent = 'RESCHDPN'      
*  CASE lcConttype = 'R'      && Return PO
*    lcEvent = 'RESCHDPR'      
*  CASE lcConttype = 'O'      && SO
*    lcEvent = 'RESCHDSO'
*  CASE lcConttype = 'T'      && EDI Sales Order
*    lcEvent = 'RESCHDST'      
*ENDCASE
*HBG [End]

SET ORDER TO Coprusr IN SYSCHDUL
SELECT PMPRJDT
SET ORDER TO PMPRJUSR
=SEEK(lcConttype+lcseqnumbr+lcCont_ID)
LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
               lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
IF !FOUND()
  =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
ENDIF   

IF llUpdEstdr
  REPLACE PMPRJDT.nEst_Dur WITH lnPrvEstDr
ENDIF
REPLACE PMPRJDT.nRem_Dur WITH lnRemining


SCATTER MEMVAR MEMO
ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,m.nAct_dur)
*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
lcInform = ""
*HBG [End]
DO lfUpdAdTrl IN lcAppHome+"MFPROJ" WITH lcconttype
SELECT (lc_PrjAudt)
*HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
*IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
*  =gfDoTriger('TASKLST',PADR(lcEvent,10))
*ENDIF
DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
*HBG [End]

SELECT SYSCHDUL
SET ORDER TO (lcCurIndx)
SET FILTER TO &lcPrvFlt

lnRemin = 0
lnPerct = 0
IF lnActFolde = 4  OR (lnActFolde  = ALEN(laFoldWind,1) AND CTRANTYPE='W')
  SHOW GET lnRemin ENABLE
  SHOW GET lnPerct ENABLE
ELSE
  SHOW GET lnRemin DISABLE
  SHOW GET lnPerct DISABLE  
ENDIF
SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE

*!*************************************************************
*! Name      : lfComplete
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Valid function for Complete button
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfComplete()
*!*************************************************************
*N000341,1
FUNCTION lfComplete
PARAMETERS lcConttype,lcseqnumbr,lcCont_ID,lcUser_Id,ldTranDate,lcDate,lcDir,lcSDir,;
           lcAppHome,lcTempName

IF TYPE('lcConttype') <> 'C' OR TYPE('lcseqnumbr') <> 'C' OR TYPE('lcCont_ID') <> 'C' OR;
   TYPE('lcUser_Id') <> 'C' OR  TYPE('ldTranDate') <> 'D' OR TYPE('lcDate') <> 'D' OR;
   TYPE('lcDir') <> 'C' OR  TYPE('lcSDir') <> 'C' OR  TYPE('lcAppHome') <> 'C'
  lcConttype  = SYSCHDUL.cConttype   
  lcseqnumbr  = SYSCHDUL.cSeqNumber
  lcCont_ID   = SYSCHDUL.cCont_ID
  lcUser_ID   = lcUserID

  ldTranDate  = SYSCHDUL.dTranDate
  lcDataDir   = gcDataDir
  lcSysDir    = gcSysHome
  lcDate      = gdSysDate
  lcAppHome   = gcAppHome
  lcTempName  = gfTempName()  
ELSE
  lcDataDir  = lcDir
  lcSysDir   = lcSDir
  =lfOpenFile(lcSysDir,'SYSCHDUL','Coprusr')
ENDIF   

=lfOpenFile(lcDataDir,'PMPRJHD','PMPRJHD')
=lfOpenFile(lcDataDir,'PMPRJDT','PMPRJDTS')
=lfOpenFile(lcDataDir,'PMPRJRL','PMPRJRL')
=lfOpenFile(lcDataDir,'PMCALDT','PMCALDT')
=lfOpenFile(lcDataDir,'PMCALHD','PMCALHD')

lnWEnd = 0

SELECT PMPRJDT
SET ORDER TO PMPRJUSR
IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') 
  LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
                 lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
  IF !FOUND()
    =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
  ENDIF   
  
  lnWEnd = 0 
  =lfGetWkEnd()
ENDIF

SELECT SYSCHDUL
SET FILTER TO
lcCurIndx = TAG()
SET ORDER TO Coprusr

IF SEEK(lcconttype+lcseqnumbr+lccont_id,'SYSCHDUL')
  IF SYSCHDUL.cOperStat = 'C'
    SELECT SYSCHDUL
    LOCATE REST FOR  cconttype+cseqnumber+ccont_id+coperstat+cuser_id = ;
                     lcconttype+lcseqnumbr+lccont_id AND SYSCHDUL.cOperStat <> 'C'
  ENDIF  
  REPLACE SYSCHDUL.cuser_id   WITH lcUser_id,;
          SYSCHDUL.cCompleted WITH 'Y'
ENDIF  

SELECT PMPRJDT
SET ORDER TO PMPRJUSR
lcStyle = SPACE(LEN(PMPRJDT.cStyle))
IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') 
  LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
                 lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
  IF !FOUND()
    =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
  ENDIF                    
  IF EMPTY(PMPRJDT.cOprt_Res)
    REPLACE PMPRJDT.cOprt_Res WITH lcUser_ID,;
            PMPRJDT.cGroup_ID WITH ' ' 
  ENDIF
  IF PMPRJDT.nEst_dur <> 0
    REPLACE PMPRJDT.nAct_dur WITH IIF(lcDate = ldTranDate,1,(lcDate - ldTranDate) + 1 - lnWEnd)
  ENDIF  
  lcStyle = PMPRJDT.cStyle
ENDIF
SELECT SYSCHDUL
lcPrvFlt = FILTER()
SET FILTER TO
SELECT PMPRJDT
SET ORDER TO PMPRJDTS
DO lfSchedule IN lcAppHome+"MFPROJ" WITH lcconttype,lcseqnumbr,lcStyle,ldTranDate,lcSysDir,lcTempName
SET ORDER TO Coprusr IN SYSCHDUL
SET ORDER TO PMPRJUSR IN PMPRJDT

IF SEEK(lcconttype+lcseqnumbr+lccont_id,'SYSCHDUL')
  IF SYSCHDUL.cOperStat = 'C'
    SELECT SYSCHDUL
    LOCATE REST FOR  cconttype+cseqnumber+ccont_id+coperstat+cuser_id = ;
                     lcconttype+lcseqnumbr+lccont_id AND SYSCHDUL.cOperStat <> 'C'
  ENDIF  
  REPLACE SYSCHDUL.cOperStat  WITH 'C'
  lnActDur = SYSCHDUL.nActDur 
  REPLACE SYSCHDUL.nActDur WITH 0
  
  IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') AND PMPRJDT.nEst_dur <> 0 
    IF PMPRJDT.lOrginal
      SELECT PMPRJDT
      LOCATE REST FOR  cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res = ;
                     lcconttype+lcseqnumbr+lccont_id AND !lOrginal
    ENDIF
    IF !FOUND()
      =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
    ENDIF
 
    REPLACE PMPRJDT.dAct_strt  WITH PMPRJDT.dClc_strt,;
            PMPRJDT.dAct_Fnsh  WITH PMPRJDT.dClc_Fnsh,;
            PMPRJDT.nAct_Dur   WITH IIF(lcDate = ldTranDate,1,(lcDate - ldTranDate) + 1 - lnWEnd) + lnActDur 
    IF lnActDur <> 0
      REPLACE PMPRJDT.dAct_strt  WITH (PMPRJDT.dAct_strt - (PMPRJDT.nAct_Dur)) + 1,;
              PMPRJDT.dClc_strt  WITH (PMPRJDT.dClc_strt - (PMPRJDT.nAct_Dur)) + 1
      REPLACE PMPRJDT.nAct_Dur   WITH (PMPRJDT.dAct_Fnsh - PMPRJDT.dAct_strt) + 1
      lnWEnd = 0 
      =lfGetWkEnd()
      IF lnWEnd > 0
        REPLACE PMPRJDT.dAct_strt  WITH (PMPRJDT.dAct_strt - lnWEnd) + 1,;
                PMPRJDT.dClc_strt  WITH (PMPRJDT.dClc_strt - lnWEnd) + 1
        REPLACE PMPRJDT.nAct_Dur   WITH (PMPRJDT.dAct_Fnsh - PMPRJDT.dAct_strt) + 1 - lnWEnd
      ENDIF  
    ENDIF          
                    
  ENDIF
ENDIF  

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*DO CASE
*  CASE lcConttype = 'C'     && CT
*    lcEvent = 'RESCHDCT'
*  CASE lcConttype = 'P'      && PO
*    lcEvent = 'RESCHDPO'
*  CASE lcConttype = 'A'      && Adorment PO Order
*    lcEvent = 'RESCHDPA'      
*  CASE lcConttype = 'D'      && Dye PO Order 
*    lcEvent = 'RESCHDPD'      
*  CASE lcConttype = 'N'      && Inter-Location PO
*    lcEvent = 'RESCHDPN'      
*  CASE lcConttype = 'R'      && Return PO
*    lcEvent = 'RESCHDPR'      
*  CASE lcConttype = 'O'      && SO
*    lcEvent = 'RESCHDSO'
*  CASE lcConttype = 'T'      && EDI Sales Order
*    lcEvent = 'RESCHDST'      
*ENDCASE
*HBG [End]

SET ORDER TO Coprusr IN SYSCHDUL
llComplete = .T.
SELECT PMPRJDT
*AMH
*SET ORDER TO PMPRJUSR
SET ORDER TO PMPRJUSR ASCENDING
*AMH
IF SEEK(lcConttype+lcseqnumbr+lcCont_ID)
  lcKey1 = PMPRJDT.cprj_typ+PMPRJDT.cprj_id+PMPRJDT.cStyle
  SCAN REST WHILE  cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res = lcConttype+lcseqnumbr
    IF EMPTY(dAct_fnsh)
      llComplete = .F.
      EXIT
    ENDIF
  ENDSCAN
ENDIF

IF llComplete
  SELECT PMPRJHD
  SET ORDER TO PMPRJHD
  IF SEEK(lcKey1)
    REPLACE dAct_strt WITH dClc_strt,;
            dAct_Fnsh WITH dClc_Fnsh,;
            cPrj_Stts WITH 'C'
  ENDIF           
ENDIF

SELECT PMPRJDT
=SEEK(lcConttype+lcseqnumbr+lcCont_ID)
LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
               lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
IF !FOUND()
  =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
ENDIF                
REPLACE PMPRJDT.LAUDTUPD WITH .T.,;
        PMPRJDT.nRem_Dur WITH 0    
SCATTER MEMVAR MEMO
ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
lnrem_dur  = 0
*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
lcInform = ""
*HBG [End]
DO lfUpdAdTrl IN lcAppHome+"MFPROJ" WITH lcconttype
SELECT (lc_PrjAudt)
*HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
*IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
*  =gfDoTriger('TASKLST',PADR(lcEvent,10))
*ENDIF
DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
*HBG [End]

DIMENSION lANCont_ID[1]
lANCont_ID[1] =""
SET ORDER TO PMPRJRLP IN PMPRJRL
IF SEEK(lcconttype + lcseqnumbr + lcStyle + lcCont_Id,'PMPRJRL')
  lnI = 0
  SELECT PMPRJRL
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+cprd_ctg+cprd_id = ;
                  lcconttype + lcseqnumbr + lcStyle + lcCont_Id
    DIMENSION lANCont_ID[lnI+1]
    lnI = ALEN(lANCont_ID,1)
    lANCont_ID[lnI] =PMPRJRL.cOprt_Ctg+PMPRJRL.cOprt_ID
  ENDSCAN
ENDIF

IF !EMPTY(lANCont_ID[1])
  FOR lnI = 1 TO ALEN(lANCont_ID,1)
    DIMENSION laNPrd_ID[1]
    STORE "" TO laNPrd_ID
    
    SET ORDER TO PMPRJRL IN PMPRJRL
    IF SEEK(lcConttype+lcseqnumbr+lcStyle+lANCont_ID[lnI],'PMPRJRL')
      lnK = 0
      SELECT PMPRJRL
      SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = lcConttype+;
                       lcseqnumbr+lcStyle+lANCont_ID[lnI]
        DIMENSION laNPrd_ID[lnK +1]
        lnK = ALEN(laNPrd_ID,1)
        laNPrd_ID[lnK] =PMPRJRL.cPrd_Ctg+PMPRJRL.cPrd_ID              
      ENDSCAN        
    ENDIF
    
    llLoop = .F.
    FOR lnK = 1 TO ALEN(laNPrd_ID,1)
      IF SEEK(lcconttype+lcseqnumbr+laNPrd_ID[lnK],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C' 
        llLoop = .T.
        EXIT
      ENDIF  
    ENDFOR
    IF llLoop 
      LOOP
    ENDIF
      
    IF SEEK(lcconttype+lcseqnumbr+lANCont_ID[lnI],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
      SELECT SYSCHDUL
      REPLACE SYSCHDUL.lPredComp WITH .T.
    ENDIF
    IF SEEK(lcconttype+lcseqnumbr+lANCont_ID[lnI],'SYSCHDUL') AND SYSCHDUL.cOperStat = 'H' 
      SELECT SYSCHDUL
      REPLACE SYSCHDUL.cOperStat  WITH 'O'
    ENDIF
    SELECT PMPRJDT
    SET ORDER TO PMPRJUSR
    =SEEK(lcConttype+lcseqnumbr+lANCont_ID[lnI])
    SCATTER MEMVAR MEMO
    ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
    lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,m.nAct_dur)

    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcInform = ""
    *HBG [End]
    DO lfUpdAdTrl IN lcAppHome+"MFPROJ" WITH lcconttype
    SELECT (lc_PrjAudt)
    *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
    *  =gfDoTriger('TASKLST',PADR(lcEvent,10))
    *ENDIF
    DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
    *HBG [End]
  ENDFOR
ENDIF

SELECT SYSCHDUL
SET ORDER TO (lcCurIndx)
SET FILTER TO &lcPrvFlt
LOCATE

*!*************************************************************
*! Name      : lfRedirect
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Valid function for Rediredct button
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfRedirect()
*!*************************************************************
*N000341,1
FUNCTION lfRedirect

llRedirect = .F.

DIMEN laOper[1,2]
laOper[1,1] = 'N/A'
laOper[1,2] = ' '

=lfOpenFile(gcDataDir,'PMPRJRL','PMPRJRL')
=lfOpenFile(gcDataDir,'PMPRJDT','PMPRJUSR')

SELECT PMPRJDT
SET ORDER TO PMPRJUSR

lcStyle = SPACE(LEN(PMPRJDT.cStyle))
IF SEEK(SYSCHDUL.cconttype+SYSCHDUL.cseqnumber+SYSCHDUL.cCont_ID) 
  lcStyle = PMPRJDT.cStyle
ENDIF

SELECT PMPRJRL
SET ORDER TO PMPRJRL

IF SEEK(SYSCHDUL.cconttype+SYSCHDUL.cseqnumber+lcStyle+SYSCHDUL.cCont_ID)
  lnI = 1
  SCAN REST WHILE cprj_typ + cprj_id + cstyle + coprt_ctg + coprt_id =;
                  SYSCHDUL.cconttype+SYSCHDUL.cseqnumber+lcStyle+SYSCHDUL.cCont_ID
    DIMENSION laOper[lnI+1,2]
    lnI = ALEN(laOper,1)
    laOper[lnI,1] = PMPRJRL.cPrd_ctg+PMPRJRL.cPrd_id
    =SEEK(SYSCHDUL.cconttype+SYSCHDUL.cseqnumber+PMPRJRL.cPrd_ctg+PMPRJRL.cPrd_id,'PMPRJDT') 
    laOper[lnI,2] = PMPRJDT.coprt_dsc
  ENDSCAN
ENDIF

STORE 1 To lnOper
lcOper = laOper[lnOper,2]
STORE '' TO lcoprt_dsc,lcoprt_res,m.mNotes

SET ORDER TO PMPRJUSR IN PMPRJDT

IF ALEN(laOper,1) = 1
  =gfModalgen("TRM00387B00000","DIALOG")
ELSE
  DO (gcScrDir+gcWinAppl+'\PRJREDIR.spx')
ENDIF
IF llRedirect
  =lfRedir()
ENDIF

*!*************************************************************
*! Name      : lfRedir
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Valid function for Rediredct button
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfRedir()
*!*************************************************************
*N000341,1
FUNCTION lfRedir
PARAMETERS lcContType,lcseqnumbr,lcCont_ID,lcUser_ID,lcOpertion,lcOperName,lcUser,lcReason,lcDate,;
           lcDir,lcSDir,lcAppHome,lcTempName

llFromWeb  = .F.

IF TYPE('lcContType') <> 'C' OR TYPE('lcseqnumbr') <> 'C' OR TYPE('lcCont_ID') <> 'C' OR ;
   TYPE('lcUser_ID') <> 'C' OR TYPE('lcOpertion') <> 'C' OR TYPE('lcOperName') <> 'C' OR ;
   TYPE('lcUser') <> 'C' OR TYPE('lcReason') <> 'C'OR TYPE('lcDate') <> 'D' OR;
   TYPE('lcDir') <> 'C' OR  TYPE('lcSDir') <> 'C' OR  TYPE('lcAppHome') <> 'C'
  lcConttype  = SYSCHDUL.cConttype   
  lcseqnumbr  = SYSCHDUL.cSeqNumber
  lcCont_ID   = SYSCHDUL.cCont_ID
  lcUser_ID   = SYSCHDUL.cUser_Id   
  lcDataDir  = gcDataDir
  lcSysDir   = gcSysHome
  lcDate     = gdSysDate
  lcAppHome  = gcAppHome
  lcTempName = gfTempName()
ELSE
  llFromWeb  = .T.
  lcDataDir  = lcDir
  lcSysDir   = lcSDir
  lcOper     = lcOpertion  
  lcoprt_dsc = lcOperName
  lcoprt_res = lcUser
  m.mNotes   = lcReason
  =lfOpenFile(gcDataDir,'PMPRJDT','PMPRJUSR')
  =lfOpenFile(lcDataDir,'PMPRJRL','PMPRJRL')
  =lfOpenFile(lcSysDir,'SYSCHDUL','Coprusr')
ENDIF   

=lfOpenFile(lcDataDir,'PMPRJHD','PMPRJHD')
=lfOpenFile(lcDataDir,'PMCALDT','PMCALDT')
=lfOpenFile(lcDataDir,'PMCALHD','PMCALHD')


IF lcOper <> 'N/A'
  SELECT SYSCHDUL 
  IF llFromWeb
    =SEEK(lcconttype+lcseqnumbr+lcCont_ID+'O')
  ENDIF  
  REPLACE cuser_id  WITH lcUser_ID,;
          cOperStat WITH 'H'

  IF !llFromWeb
    lcContType = SYSCHDUL.cconttype
    lcseqnumbr = SYSCHDUL.cseqnumber
    lcCont_ID  = SYSCHDUL.cCont_ID 
  ENDIF
  
  
  IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') 
    IF EMPTY(PMPRJDT.cOprt_Res)
      REPLACE PMPRJDT.cOprt_Res WITH lcUser_ID,;
              PMPRJDT.cGroup_ID WITH ' ' 
    ENDIF
    
    lnlastDur = gdSysDate - PMPRJDT.dClc_Strt + 1
    SELECT SYSCHDUL 
    SET ORDER TO Coprusr IN SYSCHDUL
    =SEEK(lcconttype+lcseqnumbr+lcCont_ID+'H')
    REPLACE SYSCHDUL.nActDur WITH lnlastDur
    
  ENDIF
  
  IF SEEK(lcContType+lcseqnumbr+lcOper+lcoprt_res,'PMPRJDT')    
    lcOprt_Seq  = PMPRJDT.cOprt_Seq
    lnest_dur   = PMPRJDT.nest_dur
    lcOprt_ID   = PMPRJDT.cOprt_ID
    lcOprt_Ctg  = PMPRJDT.cOprt_Ctg
    lcStyle     = PMPRJDT.cStyle
    llShw2Cust  = PMPRJDT.lShw2Cust
    m.mOprt_Com = PMPRJDT.mOprt_Com
    lcUpdtMthd  = PMPRJDT.cUpdtMthd 
    m.mNotify   = PMPRJDT.mNotify    
    lcCtg_Seq   = PMPRJDT.cCtg_Seq
    lcCal_id    = PMPRJDT.cCal_id
  ENDIF


  DECLARE laHolidays[1]
  laHolidays = ''
  
  *E301869,2 AMH Fix bug of lnRows not found [Start]
  lnRows = 0
  *E301869,2 AMH [End]

  IF SEEK(PMPRJDT.cCal_id,'PMCALDT')
    SELECT PMCALDT
    SCAN
      ldCal_HFrm = dCal_HFrm
      DO WHILE dCal_HTo >= ldCal_HFrm
        lnRows  = lnRows  + 1
        DIMENSION laHolidays[lnRows]
        laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
        ldCal_HFrm = ldCal_HFrm + 1
      ENDDO  
    ENDSCAN
  ENDIF
  
  lnWEnd = 0
  IF SEEK(PMPRJDT.cCal_id,'PMCALHD')
    ldOprtFsh = lcDate
    FOR lnI = 1 TO lnest_dur
       ldOprtFsh = ldOprtFsh + 1
       lnWEnd = lnWEnd + IIF(STR(DOW(ldOprtFsh),1) $ PMCALHD.cCal_WEnd .OR. ;
                     ASCAN(laHolidays,PMPRJDT.cCal_id+ DTOC(ldOprtFsh)) > 0 , 1 , 0)
    ENDFOR
  ENDIF
  IF lnWEnd <> 0
    lnest_dur = (lnest_dur + lnWEnd) - 1
  ENDIF

  INSERT INTO SYSCHDUL;
         (cconttype , cseqnumber , ccont_id , csubject,ctrantype,nestdur,ccompleted,;
         cUser_Id,COPERSTAT,mNotes,dtrandate,dcmpltdate,lPredComp);
  VALUES(lcContType,lcseqnumbr,lcOper,lcoprt_dsc,'W',lnest_dur,'N',;
         lcoprt_res,'O',m.mNotes,lcDate,lcDate+lnest_dur,.T.)
       
  INSERT INTO PMPRJDT;
         (  cPrj_Typ ,   cPrj_ID  ,  cOprt_ID  ,   cOprt_Dsc,;
            nest_dur ,   nrem_dur ,  cOprt_Ctg ,   cOprt_Res,;   
            lShw2Cust,   mOprt_Com,  cUpdtMthd ,   mNotify  ,;  
            cAdd_User,   dAdd_Date,  cAdd_Time ,   cStyle   ,;
            cCtg_Seq ,   cOprt_Seq,  lOrginal  ,   cCal_id  ,;
            dEst_strt,   dEst_fnsh,  lVoid);
  VALUES(   lcContType , lcseqnumbr     , lcOprt_ID   , lcoprt_dsc,;
            lnest_dur  , lnest_dur      , lcOprt_Ctg  , lcoprt_res,;
            llShw2Cust , m.mOprt_Com    , lcUpdtMthd  , m.mNotify ,;    
            gcUser_ID  , lcDate         , TIME()      , lcStyle   ,; 
            lcCtg_Seq  , lcOprt_Seq     , .F.         ,lcCal_id   ,;
            lcDate     ,lcDate+lnest_dur-1,.F.)
  
  *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
  =lfUpdVar()
  *DO CASE
  *  CASE lcConttype = 'C'     && CT
  *    lcEvent = 'RESCHDCT'
  *  CASE lcConttype = 'P'      && PO
  *    lcEvent = 'RESCHDPO'
  *  CASE lcConttype = 'A'      && Adorment PO Order
  *    lcEvent = 'RESCHDPA'      
  *  CASE lcConttype = 'D'      && Dye PO Order 
  *    lcEvent = 'RESCHDPD'      
  *  CASE lcConttype = 'N'      && Inter-Location PO
  *    lcEvent = 'RESCHDPN'      
  *  CASE lcConttype = 'R'      && Return PO
  *    lcEvent = 'RESCHDPR'      
  *  CASE lcConttype = 'O'      && SO
  *    lcEvent = 'RESCHDSO'
  *  CASE lcConttype = 'T'      && EDI Sales Order
  *    lcEvent = 'RESCHDST'      
  *ENDCASE
  *HBG [End]
  
  SELECT PMPRJDT
  SET ORDER TO PMPRJDTS
  SCATTER MEMVAR MEMO
  SET ORDER TO Coprusr IN SYSCHDUL
  ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
  lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,m.nAct_dur)
  *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  lcInform = ""
  *HBG [End]
  DO lfUpdAdTrl IN lcAppHome+"MFPROJ" WITH lcconttype
  SELECT (lc_PrjAudt)
  *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
  *  =gfDoTriger('TASKLST',PADR(lcEvent,10))
  *ENDIF
  DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
  *HBG [End]
ENDIF

SELECT SYSCHDUL
lcPrvFlt = FILTER()
SET FILTER TO

SELECT PMPRJDT
SET ORDER TO PMPRJDTS

DO lfSchedule IN lcAppHome+"MFPROJ" WITH lcconttype,lcseqnumbr,lcStyle,lcDate,lcSysDir,lcTempName

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*DO CASE
*  CASE lcConttype = 'C'     && CT
*    lcEvent = 'RESCHDCT'
*  CASE lcConttype = 'P'      && PO
*    lcEvent = 'RESCHDPO'
*  CASE lcConttype = 'A'      && Adorment PO Order
*    lcEvent = 'RESCHDPA'      
*  CASE lcConttype = 'D'      && Dye PO Order 
*    lcEvent = 'RESCHDPD'      
*  CASE lcConttype = 'N'      && Inter-Location PO
*    lcEvent = 'RESCHDPN'      
*  CASE lcConttype = 'R'      && Return PO
*    lcEvent = 'RESCHDPR'      
*  CASE lcConttype = 'O'      && SO
*    lcEvent = 'RESCHDSO'
*  CASE lcConttype = 'T'      && EDI Sales Order
*    lcEvent = 'RESCHDST'      
*ENDCASE
*HBG [End]

SET ORDER TO Coprusr IN SYSCHDUL
SELECT PMPRJDT
SET ORDER TO PMPRJUSR
=SEEK(lcConttype+lcseqnumbr+lcCont_ID)
LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
               lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
IF !FOUND()
  =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
ENDIF   

SCATTER MEMVAR MEMO
ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,m.nAct_dur)
*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
lcInform = ""
*HBG [End]
DO lfUpdAdTrl IN lcAppHome+"MFPROJ" WITH lcconttype
SELECT (lc_PrjAudt)
*HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
*IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
*  =gfDoTriger('TASKLST',PADR(lcEvent,10))
*ENDIF
DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
*HBG [End]
SELECT SYSCHDUL
SET FILTER TO &lcPrvFlt
LOCATE

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Open the needed files
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfRedir()
*!*************************************************************
*N000341,1
FUNCTION lfOpenFile
PARAMETERS lcpath,lcFile,lcTag

IF !USED(lcFile)
  USE lcpath+lcFile+".DBF" SHARED IN 0
  SET ORDER TO (lcTag) IN (lcFile)
ENDIF

*!*************************************************************
*! Name      : lfvRemin
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Validation of remaining button
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvRemin()
*!*************************************************************
*N000341,1
FUNCTION lfvRemin
PARAMETERS lcRemFld

=lfOpenFile(gcDataDir,'PMCALDT','PMCALDT')
=lfOpenFile(gcDataDir,'PMCALHD','PMCALHD')

DECLARE laHolidays[1]
laHolidays = ''
lnRows  = 0

SELECT PMPRJDT

*E301869,2 AMH Set the order descending to get the last record of this task [Start]
*SET ORDER TO PMPRJUSR
SET ORDER TO PMPRJUSR DESCENDING
*E301869,2 AMH [End]

IF SEEK(SYSCHDUL.cConttype+SYSCHDUL.cSeqNumber+SYSCHDUL.cCont_ID,'PMPRJDT') 
  ldTranDate = IIF(EMPTY(PMPRJDT.dclc_strt),PMPRJDT.dEst_strt,PMPRJDT.dclc_strt)
  lnDur = IIF(gdSysDate = ldTranDate,1,(gdSysDate - ldTranDate)+1)
  lnDur = lnDur + lnRemin
  IF SEEK(PMPRJDT.cCal_id,'PMCALDT')
    SELECT PMCALDT
    SCAN
      ldCal_HFrm = dCal_HFrm
      DO WHILE ldCal_HFrm <= dCal_HTo
        lnRows  = lnRows  + 1
        DIMENSION laHolidays[lnRows]
        laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
        ldCal_HFrm = ldCal_HFrm + 1
      ENDDO  
    ENDSCAN
  ENDIF
  lnWEnd = 0
  IF SEEK(PMPRJDT.cCal_id,'PMCALHD')
    ldOprtFsh = SYSCHDUL.dTranDate
    FOR lnI = 1 TO lnDur
      ldOprtFsh = ldOprtFsh + 1
      IF ldOprtFsh > gdSysDate 
        EXIT
      ENDIF
      lnWEnd = lnWEnd + IIF(STR(DOW(ldOprtFsh),1) $ PMCALHD.cCal_WEnd .OR. ;
                      ASCAN(laHolidays,PMPRJDT.cCal_id+ DTOC(ldOprtFsh)) > 0 , 1 , 0)
    ENDFOR
  ENDIF
ENDIF

DO CASE
  CASE lcRemFld = 'P' 
    SELECT PMPRJDT
    SET ORDER TO PMPRJUSR
    IF SEEK(SYSCHDUL.cConttype+SYSCHDUL.cSeqNumber+SYSCHDUL.cCont_ID,'PMPRJDT') 
      lnRemin = ((nEst_Dur-lnWEnd)  * lnPerct)/100    
      lnRemin = IIF(lnRemin < 1,1,lnRemin)
      IF MOD(lnRemin,1) <> 0
        lnRemin = lnRemin - MOD(lnRemin,1)
      ENDIF
    ENDIF  
  CASE lcRemFld = 'R' 
    SELECT PMPRJDT
    SET ORDER TO PMPRJUSR
    IF SEEK(SYSCHDUL.cConttype+SYSCHDUL.cSeqNumber+SYSCHDUL.cCont_ID,'PMPRJDT') 
      ldTranDate = IIF(EMPTY(PMPRJDT.dclc_strt),PMPRJDT.dEst_strt,PMPRJDT.dclc_strt)
      lnDur = lnDur - lnWEnd 
      lnPerct = (lnRemin / lnDur) * 100
    ENDIF  
ENDCASE
SELECT SYSCHDUL

SHOW GET lnRemin
SHOW GET lnPerct

IF lnRemin = 0 OR nEstDur = 0
  SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
ELSE
  SHOW GET pbCustVend,1 PROMPT lcPrompt1 ENABLE
ENDIF

=lfRefresh()

*!*************************************************************
*! Name      : lfvOper
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Validation of redirect operation 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvRemin()
*!*************************************************************
*N000341,1
FUNCTION lfvOper

lcoprt_dsc = laOper[lnOper,2]

IF SEEK(SYSCHDUL.cconttype+SYSCHDUL.cseqnumber+lcOper,'PMPRJDT')
  lcOprt_res = PMPRJDT.cOprt_Res
ENDIF

SHOW GET lcoprt_dsc
SHOW GET lcoprt_res

=lfRefresh()

*!*************************************************************
*! Name      : lfvStart
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Validation of Start check box
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStart()
*!*************************************************************
*N000341,1
FUNCTION lfvStart

lcCurAls = SELECT(0)
lcConttype  = SYSCHDUL.cConttype   
lcseqnumbr  = SYSCHDUL.cSeqNumber
lcCont_ID   = SYSCHDUL.cCont_ID
lcSysDir    = gcSysHome
lcAppHome   = gcAppHome
lcTempName  = gfTempName()

=lfOpenFile(gcDataDir,'PMPRJHD','PMPRJHD')
=lfOpenFile(gcDataDir,'PMPRJDT','PMPRJDTS')
=lfOpenFile(gcDataDir,'PMPRJRL','PMPRJRL')
=lfOpenFile(gcDataDir,'PMCALDT','PMCALDT')
=lfOpenFile(gcDataDir,'PMCALHD','PMCALHD')

lcPrvOrd = ORDER('SYSchdul')
SET ORDER TO Coprusr IN SYSchdul

IF SEEK(lcconttype+lcseqnumbr+lccont_id,'SYSCHDUL')
  IF SYSCHDUL.cOperStat = 'C'
    SELECT SYSCHDUL
    LOCATE REST FOR  cconttype+cseqnumber+ccont_id+coperstat+cuser_id = ;
                     lcconttype+lcseqnumbr+lccont_id AND SYSCHDUL.cOperStat <> 'C'
  ENDIF  
  REPLACE SYSCHDUL.cuser_id   WITH lcUserID,;
          SYSCHDUL.cOperStat  WITH 'P'
ENDIF  

lcPrvAls = ALIAS()
SELECT PMPRJHD
lcPrvFlt = FILTER()
SET FILTER TO 
=SEEK(lcConttype+lcseqnumbr,'PMPRJHD') 
SELECT (lcPrvAls)

lcStyle  = ""

*E301869,2 AMH Set order to descending to get the last record of this task [Start]
*SET ORDER TO Pmprjusr IN PMPRJDT
SET ORDER TO Pmprjusr DESCENDING IN PMPRJDT
IF SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') 
  SELECT PMPRJDT
  *LOCATE REST FOR cprj_typ+cprj_id+coprt_ctg+coprt_id+coprt_res =;
                 lcConttype+lcseqnumbr+lcCont_ID AND !lOrginal
  *IF !FOUND()
  *  =SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT')
  *ENDIF                    
*E301869,2 AMH [End]

  lcStyle = PMPRJDT.cStyle
  REPLACE PMPRJDT.dclc_strt WITH gdSysDate
  IF EMPTY(PMPRJDT.cOprt_Res)
    REPLACE PMPRJDT.cOprt_Res WITH lcUserID,;
            PMPRJDT.cGroup_ID WITH ' ' 
  ENDIF
ENDIF

DO lfSchedule IN lcAppHome+"MFPROJ" WITH lcconttype,lcseqnumbr,lcStyle,gdSysDate,lcSysDir,lcTempName

*E301869,2 AMH Set order to descending to get the last record of this task [Start]
*SET ORDER TO Pmprjusr IN PMPRJDT
SET ORDER TO Pmprjusr DESCENDING IN PMPRJDT
*E301869,2 AMH [End]

=SEEK(lcConttype+lcseqnumbr+lcCont_ID,'PMPRJDT') 

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*--Update Audit trail
*DO CASE
*  CASE lcConttype = 'C'     && CT
*    lcEvent = 'RESCHDCT'
*  CASE lcConttype = 'P'      && PO
*    lcEvent = 'RESCHDPO'
*  CASE lcConttype = 'A'      && Adorment PO Order
*    lcEvent = 'RESCHDPA'      
*  CASE lcConttype = 'D'      && Dye PO Order 
*    lcEvent = 'RESCHDPD'      
*  CASE lcConttype = 'N'      && Inter-Location PO
*    lcEvent = 'RESCHDPN'      
*  CASE lcConttype = 'R'      && Return PO
*    lcEvent = 'RESCHDPR'      
*  CASE lcConttype = 'O'      && SO
*    lcEvent = 'RESCHDSO'
*  CASE lcConttype = 'T'      && EDI Sales Order
*    lcEvent = 'RESCHDST'      
*ENDCASE
*HBG [End]
SET ORDER TO Coprusr IN SYSCHDUL
SELECT PMPRJDT
SCATTER MEMVAR MEMO
ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,m.nAct_dur)
*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
lcInform = ""
*HBG [End]
DO lfUpdAdTrl IN gcAppHome+"MFPROJ" WITH lcConttype 
SELECT (lc_PrjAudt)
*HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
*IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
*  =gfDoTriger('TASKLST',PADR(lcEvent,10))
*ENDIF
DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
*HBG [End]
SHOW GET pbCustVend,1 PROMPT lcPrompt1 DISABLE
SHOW GET pbCancl,1 PROMPT lcPrompt2 ENABLE
SHOW GET pbComplt,1 ENABLE
SHOW GET lnPerct ENABLE
SHOW GET lnRemin ENABLE
SHOW GET lnStart DISABLE
SET ORDER TO &lcPrvOrd IN SYSchdul
SELECT (lcCurAls)

*!*************************************************************
*! Name      : lfvOper
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Validation of redirect operation 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvRemin()
*!*************************************************************
*N000341,1
FUNCTION lfGetWkEnd

DECLARE laHolidays[1]
laHolidays = ''
lnRows  = 0
ldTranDate  = IIF(EMPTY(PMPRJDT.dclc_strt),PMPRJDT.dEst_strt,PMPRJDT.dclc_strt)
lnDur = IIF(lcDate = ldTranDate,1,(lcDate - ldTranDate)+1)
IF SEEK(PMPRJDT.cCal_id,'PMCALDT')
  SELECT PMCALDT
  SCAN
    ldCal_HFrm = dCal_HFrm
    DO WHILE dCal_HTo >= ldCal_HFrm
      lnRows  = lnRows  + 1
      DIMENSION laHolidays[lnRows]
      laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
      ldCal_HFrm = ldCal_HFrm + 1
    ENDDO  
  ENDSCAN
ENDIF
  
lnWEnd = 0
IF SEEK(PMPRJDT.cCal_id,'PMCALHD')
  ldOprtFsh = IIF(EMPTY(PMPRJDT.dclc_strt),PMPRJDT.dEst_strt,PMPRJDT.dclc_strt)
  FOR lnI = 1 TO lnDur
     ldOprtFsh = ldOprtFsh + 1
     IF ldOprtFsh > lcDate
       EXIT
     ENDIF
     lnWEnd = lnWEnd + IIF(STR(DOW(ldOprtFsh),1) $ PMCALHD.cCal_WEnd .OR. ;
                   ASCAN(laHolidays,PMPRJDT.cCal_id+ DTOC(ldOprtFsh)) > 0 , 1 , 0)
  ENDFOR
ENDIF

*!*************************************************************
*! Name      : lfUpdVar
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdVar()
*!*************************************************************
FUNCTION lfUpdVar


lcApObjNam = 'TASKLST'

DO CASE
  CASE lcConttype = 'C'     && CT
    lcEvent = 'RESCHDCT'
    lcProg  = 'MFCUTKT'
    lcKey   = lcseqnumbr
  CASE lcConttype = 'P'      && PO
    lcEvent = 'RESCHDPO'
    lcProg  = 'POSTY'
    lcKey   = lcconttype+lcseqnumbr
  CASE lcConttype = 'A'      && Adorment PO Order
    lcEvent = 'RESCHDPA' 
    lcProg  = 'MFADPO'  
    lcKey   = lcconttype+lcseqnumbr     
  CASE lcConttype = 'D'      && Dye PO Order 
    lcEvent = 'RESCHDPD'
    lcProg  = 'MFDPO'      
    lcKey   = lcconttype+lcseqnumbr
  CASE lcConttype = 'N'      && Inter-Location PO
    lcEvent = 'RESCHDPN' 
    lcProg  = 'POINTRC'     
    lcKey   = lcconttype+lcseqnumbr
  CASE lcConttype = 'R'      && Return PO
    lcEvent = 'RESCHDPR'  
    lcProg  = 'RETPO'      
    lcKey   = lcconttype+lcseqnumbr
  CASE lcConttype = 'O'      && SO
    lcEvent = 'RESCHDSO'
    lcProg  = 'SOORD'
    lcKey   = lcconttype+lcseqnumbr
  CASE lcConttype = 'T'      && EDI Sales Order
    lcEvent = 'RESCHDST' 
    lcProg  = 'SOEDORD'      
    lcKey   = lcconttype+lcseqnumbr
ENDCASE

