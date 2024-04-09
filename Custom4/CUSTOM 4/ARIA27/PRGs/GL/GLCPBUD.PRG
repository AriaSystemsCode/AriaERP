*:************************************************************************
*:
*: Procedure file: GLCPBUD.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified: 1/26/94
*:
*:  Procs & Fncts:
*:
*:    FUNCTION  lfwPopup    ***  When popup function                ***
*:    FUNCTION  lfvPopup    ***  Valid popup function               ***
*:    FUNCTION  lfwList     ***  When list function                 ***
*:    FUNCTION  lfvView     ***  Valid list function                ***
*:    FUNCTION  lfvSelOne   ***  Function to select one budget      ***
*:    FUNCTION  lfvSelAll   ***  Function to select all budgets     ***
*:    FUNCTION  lfvSelNon   ***  Function to unselect all budgets   ***
*:    FUNCTION  lfvInvert   ***  Function to invert select status   ***
*:    FUNCTION  lfvCopy     ***  Function to copy selected budget(s)***
*:    FUNCTION  lfChkDt     ***  Function to check budget details   ***
*:    FUNCTION  lfCopyHdDt  ***  Function to copy budget header+det.***
*:    FUNCTION  lfvPrint    ***  Function to print copy stat.report ***
*:    FUNCTION  lfvClose    ***  Close                              ***
*:    FUNCTION  lfSamAcode  ***  To check if same account structure ***
*:               
*:      Documented 1/26/1994
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*E301077,71 Walid (WAM) 03/03/99 Open GLACCHAR file with gfOpenFile()
*

DECLARE laWndObj   [2,3]    && Have screen name & first & last obj. for each screen.
DECLARE laFileStru [1]
DECLARE laComLst  [1]  && array of companys list

** laWndObj   : Have screen name & first & last obj. for each screen.
** laFileStru :
** laComLst   : Array of companys list

** lcOldCom   : Variable to store old company id     
** lcComp_ID  : Variable to store new company id  
** lcCompany  : Variable to return selected (company id,name) from popup
** lcComPath  : Variable to store the path for existing source cgmpany
** lcAccSeg   : Variable to hold account segment
** lc_SelBuHd : Temp file name for selected budgets 
** lc_SelBuDt : Temp file name for budget lines
** lc_ViewDt  : Temp file name for view budget details
** lcAlBudHd  : Alias name for budget header file
** lcAlBudDt  : Alias name for budget detail file 
** lcAlAcCha  : Alias name for accounts chart file
** lcBud      : Variable to hold list string
** lcList_Str : Variable to hold view list string
** lc_BudCode : Variable to hold budget code
** lc_BudYear : Variable to hold budget year

** lcBut1Stat : 
** laCtrStat  : To disable the browse pad in the menu

** lcPrompt   :
** lc_Title   :
** lcEmptyTxt :

** lnReCopied : Variable to hold number of copied records.
** lnlistLine : Variable to hold record pointer in the list
** lsBud      : Variable to hold list pointer
** lnHdRecNo  : Variable to hold record number in the header file
** lnSelected :
** lnTotalRec :

** llNoContrl : No control screen
** llValdCopy : Variable to hold copy status


STORE " "       TO lcOldCom   , lcComp_ID  , lcCompany  , lcComPath ,;
                   lcAccSeg   , lc_SelBuHd , lc_SelBuDt , lc_ViewDt ,;
                   lcAlBudHd  , lcAlBudDt  , lcAlAcCha  , lcBud     ,;
                   lcList_Str , lc_BudCode , lc_BudYear

STORE "DISABLE" TO lcBut1Stat , laCtrStat

STORE 0         TO lnReCopied , lnlistLine , lsBud      , lnHdRecNo ,;
                   lnSelected , lnTotalRec

STORE .T.       TO llNoContrl , llValdCopy

lcPrompt    = "\<Select"
lc_Title    = "View details for current budget"
lcEmptyTxt  = 'No company Selected'

IF !gfSetup()    
  RETURN
ENDIF  

*E301077,71 Walid (WAM) [BEGIN]
=gfOpenFile(gcDataDir+'GLACCHAR',gcDataDir+'Acctcode','SH')
*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE

IF EOF()
  * The chart of accounts is empty. You have to create
  * the chart of accounts first.
  * < Ok > 
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF

laWndObj [1,1] = gcBaseWind
laWndObj [1,3] = "PBCLOSE"

DO CASE
  CASE _DOS
    laWndObj [1,2] = "IBPOPCOM"
  CASE _WINDOWS
    laWndObj [1,2] = "PUPOPCOM"
ENDCASE

laWndObj [2,1] = "CWRGLDETAL"
laWndObj [2,2] = "LSDETAIL"
laWndObj [2,3] = "PBDTCLOSE"

IF !WEXIST(gcBaseWind)

  *** Func. in the GL.PRG to see if there is other companies ***
  *** have the same account code strucure.

  IF !lfSamAcode()
    glQuitting=.T.  
    RETURN
  ENDIF

  *** redimension array laComLst (increase the number of rows by one)
  *** and insert a new row in array laComLst with null values.
  DIMENSION laComLst[ALEN(laComLst,1)+1,2]
  =AINS(laComLst,1)
  lcCompany     = lcEmptyTxt
  laComLst[1,1] = lcEmptyTxt
  laComLst[1,2] = ' '

  *** Create the Name for the temporary file
  lc_ViewDt   = gfTempName()  && Temp file name for view budget details
  lc_SelBuHd  = gfTempName()  && Temp file name for budget header
  lc_SelBuDt  = gfTempName()  && Temp file name for budget details 
  lcAlBudDt   = gfTempName()  && Alias name for budget details file
  lcAlAcCha   = gfTempName()  && Alias name for accounts chart file
  lcAlBudHd   = gfTempName()  && Alias name for budget header  file
  
  *** create empty temp file (lc_SelBuHd) 
  *** with the same structure of GLBudHd + Inc, CopyStat.
  SELECT GLBudHd

  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'Inc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CopyStat'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 15
  laFileStru[lnFileStru+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_SelBuHd FROM ARRAY laFileStru    
   
  *** create index file on budget code + budget year
  INDEX ON  CBUDCODE+CBUDYEAR TAG CODEYEAR
  SET ORDER TO TAG CODEYEAR

  *** create empty temp file (lc_ViewDt) 
  *** with the same structure of GLBudDt + account description.
  SELECT GLBudDt

  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]

  laFileStru[lnFileStru+1,1] = 'caccnldes'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 40
  laFileStru[lnFileStru+1,4] = 0

  CREATE TABLE &gcWorkDir.&lc_ViewDt FROM ARRAY laFileStru    
   
  DO CASE
    CASE _DOS
      lcBud      = "Inc+' '+cBudCode+'   '+cBudYear+'   '+SUBSTR(cBudDes,1,31)+'   '+CopyStat"
      lcList_Str = "SUBSTR(&lc_ViewDt..cAcctCode,1,lnAcsSegSz)+'   '+"+;
      "&lc_ViewDt..caccnldes"

    CASE _WINDOWS
      lcBud      = "Inc+' '+cBudCode+'  '+cBudYear+' '+SUBSTR(cBudDes,1,32)+' '+CopyStat"
      lcList_Str = "SUBSTR(&lc_ViewDt..cAcctCode,1,lnAcsSegSz)+'  '+"+;
      "SUBSTR(&lc_ViewDt..caccnldes,1,40-lnAcsSegSz)"
  ENDCASE

 
ENDIF

SELECT (lc_SelBuHd)  
*E300683,5 Call *.SPR from screens directory
* DO GlCpBud.SPR 
DO (gcScrDir + gcWinAppl + '\GlCpBud.SPR')
*E300683,5 end   
*** case quit this program ...
IF glQuitting 

  IF WEXIST("CWRGLDETAL")
    RELEASE WINDOW CWRGLDETAL
  ENDIF

  *** close file (lcAlAcCha) . 
  IF USED(lcAlAcCha)
    SELECT (lcAlAcCha)
    USE 
  ENDIF

  *** close file (lcAlBudDt). 
  IF USED(lcAlBudDt)
    SELECT (lcAlBudDt)
    USE 
  ENDIF

  *** close file (lcAlBudHd) . 
  IF USED(lcAlBudHd)
    SELECT (lcAlBudHd)
    USE 
  ENDIF

  *** close temp file (lc_SelBuHd) then erase this file. 
  IF USED(lc_SelBuHd)
    SELECT(lc_SelBuHd)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_SelBuHd..DBF
  ERASE &gcWorkdir.&lc_SelBuHd..CDX

  *** close temp file (lc_SelBuDt) then erase this file.
  IF USED(lc_SelBuDt)
    SELECT(lc_SelBuDt)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_SelBuDt..DBF
  ERASE &gcWorkdir.&lc_SelBuDt..CDX


  *** close temp file (lc_ViewDt) then erase this file.
  IF USED(lc_ViewDt)
    SELECT(lc_ViewDt)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_ViewDt..DBF
  ERASE &gcWorkdir.&lc_ViewDt..CDX

ENDIF
  
*!**************************************************************************
*!
*!      Function:  lfwPopup
*!
*!**************************************************************************
*
FUNCTION  lfwPopup

*** returns 2 characters from lcCompany that contains 
*** company_id + ' ' + company_name
DO CASE
  CASE _DOS
    lcOldCom = LEFT(lcCompany,2)
    lcColor1 = SCHEME(1,6)
    lcColor2 = SCHEME(1,2)
    *** Show get all hand made popups with the appropirate color. ***
    SHOW GET ibPopCom  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
  CASE _WINDOWS
    lcOldCom  = LEFT(laComLst[puPopCom,1],2)
ENDCASE

*!**************************************************************************
*!
*!      Function:  lfvPopup
*!
*!**************************************************************************
*
FUNCTION  lfvPopup

*** calling (gfActPop)  to activate the popup to change the source 
*** company from the list popup menu.
*** and return the path for selected company
*** returns 2 characters from lcCompany that contains 
*** company_id + ' ' + company_name

DO CASE
  CASE _DOS
    lcComPath  = ALLTRIM(gfActPop(1,3,IIF(ALEN(laComLst,1) > 8,9;
    ,ALEN(laComLst,1)+3),35,'laComLst',2,1,@lcCompany))  
    lcComp_ID  = LEFT(lcCompany,2)
    lcColor1 = SCHEME(1,6)
    lcColor2 = SCHEME(1,2)
    *** Show get all hand made popups with the appropirate color. ***
    SHOW GET ibPopCom  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
  CASE _WINDOWS
    lcComPath  = ALLTRIM(laComLst[puPopCom,2])
    lcComp_ID  = LEFT(laComLst[puPopCom,1],2)
ENDCASE


*** check if the user change the source company
IF lcComp_ID <> lcOldCom

  IF WVISIBLE('CWRGLDETAL') 
    =gfChClose('CWRGLDETAL')
  ENDIF

  =lfRefresh()    && refresh says filed on the screen

  SELECT (lc_SelBuHd)
  
  *** check if the the first row in the array contains empty
  *** in case if this is the first time that change active company.
  IF EMPTY(laComLst[1,2])
     *** delete the first raw in the array and redimension the array.
     =ADEL(laComLst,1)
     DIMENSION laComLst[Alen(lacomlst,1)-1,2]
       IF _WINDOWS
         puPopCom = puPopCom - 1
         SHOW GET puPopCom ENABLE
       ENDIF
  ELSE
    *** select (lc_SelBuHd) and removes all records.
    *** and refresh the list window
    SELECT (lc_SelBuHd)
    lsBud   = 1     
    SHOW GET lsBud  
    ZAP       
    SHOW GET lsBud  

    *** select (lc_ViewDt) and removes all records.
    *** and refresh the list window
    SELECT (lc_ViewDt)
    lsDetail   = 1     
    SHOW GET lsDetail
    ZAP       
    SHOW GET lsDetail

  ENDIF
  
  
  IF USED(lcAlBudHd)
    *** if used then select this area 
    SELECT (lcAlBudHd)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  

  *** use GLBudHd from the source company in the path lcComPath
  *** and set index to order tag BDCODYR
  USE &lcComPath.GLBudHd AGAIN ALIAS &lcAlBudHd ORDER TAG BDCODYR

  *** check if this there no budgets in budget header file 
  *** if empty ----> display message 
  ***                "There are no budgets to copy from this company."
  *** and then close budget header file and disable all objects.
  GO TOP
  IF EOF()
    USE
    SELECT (lc_SelBuHd)
    =gfModalGen("TRM02119B00000","Dialog","bugdets")          
    lcBut1Stat     = "DISABLE"
    SHOW GET pbSelOne DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbView   DISABLE
    SHOW GET pbCopy   DISABLE
    SHOW GET lsBud  
    RETURN
  ENDIF

  *** case there is at least one budget in the header file 
  *** in this case select all budgets from source company 
  *** into lc_SelBuHd and set relation between lc_SelBuHd
  *** and master header file (lcAlBudHd) to check if every
  *** budget still exist in the master file.
  
  SELECT *,'   ' AS 'Inc' ,SPACE(15) AS 'CopyStat';
         FROM &lcComPath.GLBudHd ;
         INTO DBF &gcWorkDir.&lc_SelBuHd 

  SELECT(lc_SelBuHd)
  SET RELATION TO &lc_SelBuHd..cBudCode+&lc_SelBuHd..cBudYear INTO &lcAlBudHd

  IF USED(lcAlBudDt)
    *** if used then select this area 
    SELECT (lcAlBudDt)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  

  *** use GLBudDt from the source company in the path lcComPath
  *** and set index to order tag CDYRPRACC
  USE &lcComPath.GLBudDt AGAIN ALIAS &lcAlBudDt ORDER TAG CDYRPRACC


  IF USED(lcAlAcCha)
    *** if used then select this area 
    SELECT (lcAlAcCha)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  

  *** use GLAccHar from the source company in the path lcComPath
  USE &lcComPath.GLAccHar AGAIN ALIAS &lcAlAcCha

*E300692,1 Change fil ename from sycaccod to accod
  *SELECT SYCACCOD
  SELECT ACCOD
*E300692,1 end
*E300789,7 [BEGIN]
*  SET ORDER TO TAG COMPID
*  IF SEEK(lcComp_ID)
*    lcAccSeg = cAcSegDes
*  ELSE
*    lcAccSeg = ""
*  ENDIF
  GO TOP
  IF !EOF()
    lcAccSeg = cAcSegDes
  ELSE
    lcAccSeg = ""
  ENDIF
*E300789,7 [END..]  
  SELECT (lc_SelBuHd)
  lcBut1Stat     = "ENABLE"
  SHOW GET pbSelOne ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbCopy   ENABLE
  SHOW GET pbView   ENABLE
  lsBud   = 1     
  SHOW GET lsBud  
  
ENDIF

SHOW GET ibBgDuma1  ENABLE
SHOW GET ibEndDumi1 ENABLE
SHOW GET ibBgDumi2  ENABLE
SHOW GET ibEndDumi2 ENABLE
SHOW GET pbDtClose  ENABLE

*!**************************************************************************
*!
*!      Function:  lfvView
*!
*!**************************************************************************
*
FUNCTION  lfvView

*** activate window CWRGLDETAL to display budget details 
=gfActWind('CWRGLDETAL',lc_Title)
SHOW GET lcAccSeg ENABLE
SELECT (lcAlBudDt)

lc_BudCode = &lc_SelBuHd..CBUDCODE
lc_BudYear = &lc_SelBuHd..CBUDYEAR

*** select account code and description from source company
*** where budget code = lc_BudCode
*** and   budget year = lc_BudYear  (active budget)

SELECT DISTINCT &lcAlBudDt..cAcctCode,&lcAlAcCha..cAccnlDes;
   FROM &lcAlBudDt,&lcAlAcCha;
   INTO DBF &gcWorkDir.&lc_ViewDt ;
   WHERE lc_BudCode            = &lcAlBudDt..CBUDCODE ;
   .AND. lc_BudYear            = &lcAlBudDt..CBUDYEAR ;
   .AND. &lcAlBudDt..cAcctCode = &lcAlAcCha..cAcctCode

SELECT (lc_ViewDt)
SHOW GETS WINDOW CWRGLDETAL

*!**************************************************************************
*!
*!      Function:  lfwList
*!
*!**************************************************************************
*
FUNCTION  lfwList
*** check if the number of budgets in the list greater than 1 

IF RECCOUNT(lc_SelBuHd) > 0 
  IF !EMPTY(&lc_SelBuHd..CopyStat)
    lcPrompt = "Select"
    SHOW GET pbSelOne,1 PROMPT 'Select' DISABLE
  ELSE 
    *** check the status for (Inc) field 
    IF !EMPTY(&lc_SelBuHd..Inc)
      *** case the selected .. change the prompt to 'unselect'
      lcPrompt = "\<Unselect"
      SHOW GET pbSelOne,1 PROMPT '\<Unselect'
    ELSE
      *** case the unselected .. change the prompt to 'select'
      lcPrompt = "\<Select"
      SHOW GET pbSelOne,1 PROMPT '\<Select' 
    ENDIF
  ENDIF  

  *** check if winodw CWRGLDETAL visible...
  *** if yes select budget details into lc_ViewDt 
  *** and refresh the list view window
  IF WVISIBLE("CWRGLDETAL")

    lc_BudCode = &lc_SelBuHd..CBUDCODE
    lc_BudYear = &lc_SelBuHd..CBUDYEAR
 
    SELECT DISTINCT &lcAlBudDt..cAcctCode,&lcAlAcCha..cAccnlDes;
      FROM &lcAlBudDt,&lcAlAcCha;
      INTO DBF &gcWorkDir.&lc_ViewDt ;
      WHERE lc_BudCode            = &lcAlBudDt..CBUDCODE ;
      .AND. lc_BudYear            = &lcAlBudDt..CBUDYEAR ;
      .AND. &lcAlBudDt..cAcctCode = &lcAlAcCha..cAcctCode
  
    SELECT (lc_ViewDt)
    lsDetail = 1 
    SHOW GETS WINDOW CWRGLDETAL
  ENDIF
  SELECT (lc_SelBuHd)
ENDIF


*!**************************************************************************
*!					
*!      Function:  lfvSelOne
*!
*!**************************************************************************
*
FUNCTION  lfvSelOne
*** check the status for the field (Inc) 
*** and change status from select   to unselect
***                or from unselect to select
SELECT (lc_SelBuHd)
IF EMPTY(CopyStat)
  REPLACE  Inc WITH IIF(Inc=' û','  ',' û')
  *** refresh the list and change prompt according to new status.
  SHOW GET lsBud
  =lfwList()
ENDIF


*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*

FUNCTION  lfvSelAll 
*** check the field status (Inc) for all the file (lc_SelBuHd)
*** and change status from unselect to select

SELECT (lc_SelBuHd)
*** store current position in the list
lnlistLine = lsBud
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),' û','')
*** refresh the list and change prompt according to new status.
*** restore current position in the list
lsBud = lnlistLine 
SHOW GET lsBud
=lfwList()

*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 
*** check the field status (Inc) for all the file (lc_SelBuHd)
*** and change status from select   to unselect

SELECT (lc_SelBuHd)
*** store current position in the list
lnlistLine = lsBud
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),'  ','')

*** refresh the list and change prompt according to new status.
*** restore current position in the list
lsBud = lnlistLine 
SHOW GET lsBud
=lfwList()

*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!
*!**************************************************************************
*
FUNCTION  lfvInvert 
*** check the status field (Inc) for all the file (lc_SelBuHd)
*** and change status from select   to unselect
***                or from unselect to select

SELECT (lc_SelBuHd)
*** store current position in the list
lnlistLine = lsBud
REPLACE  ALL Inc WITH  IIF(EMPTY(CopyStat),IIF(Inc=' û','  ',' û'),'')
*** refresh the list and change prompt according to new status.
*** restore current position in the list
lsBud = lnlistLine 
SHOW GET lsBud
=lfwList()

*!*************************************************************************
*!
*!     Function:  lfvCopy 
*!
*!**************************************************************************
*
FUNCTION  lfvCopy
*** start copy selected budgets
SELECT (lc_SelBuHd)
lnlistLine = lsBud      && store current position in the list
lnCurRec   = 0          && init. the current budget variable.
lnReCopied = 0          && init. the no of copied records.
lnHdRecNo = RECNO(lc_SelBuHd)

*** count the number of included budgets to the variable lnTotRecs
COUNT FOR !EMPTY(Inc) TO lnTotRecs

*** check if there is any selected budgets to copyied
*** case there is no selected budgets to copyied
IF lnTotRecs = 0 
  *** Message "No included budgets to copy."
  *** and change the current object to (Select) 
  *** then exit from this function.
  =gfModalGen("TRM02121B00000","DIALOG")   
  _CUROBJ=OBJNUM(pbSelOne)
  SELECT (lc_SelBuHd)
  lsBud = lnlistLine     && restore current position in the list

  IF lnHdRecNo <= RECCOUNT()
    GOTO lnHdRecNo
  ENDIF  

  RETURN
ENDIF

*** select all budget details information and the type for 
*** every account from source company into lc_SelBuDt
lnTotRecs  = lnTotRecs + 1
lnSelected = 0 
lnTotalRec = RECCOUNT(lcAlBudDt)
=gfThermo(lnTotalRec,1,'Collecting budget lines...','')

SELECT &lcAlBudDt..*,SPACE(20) AS 'DetalStat',&lcAlAcCha..cTypeCode,lfTherm();
  FROM &lcAlBudDt,&lc_SelBuHd,&lcAlAcCha                ;
  WHERE  !EMPTY(&lc_SelBuHd..Inc)                       ;
  .AND.  &lc_SelBuHd..CBUDCODE  = &lcAlBudDt..CBUDCODE  ;
  .AND.  &lc_SelBuHd..CBUDYEAR  = &lcAlBudDt..CBUDYEAR  ;
  .AND.  &lcAlBudDt..cAcctCode  = &lcAlAcCha..cAcctCode ;
  INTO DBF &gcWorkDir.&lc_SelBuDt

IF lnTotalRec <> lnSelected
  FOR lnSelected=lnSelected TO lnTotalRec
    =gfThermo(lnTotalRec,lnSelected,'Copying budget ...',' ')
  ENDFOR
ENDIF

SELECT (lc_SelBuHd)
SCAN FOR !EMPTY(Inc)
  lnHdRecNo = RECNO(lc_SelBuHd)   && store CP --> lc_SelBuHd 
  lnCurRec = lnCurRec + 1         
  =gfThermo(lnTotRecs,lnCurRec,'Copying budget ...',;
      ALLTRIM(&lc_SelBuHd..cBudCode)+'\' +ALLTRIM(&lc_SelBuHd..cBudYear))      
  SELECT (lcAlBudHd)
  IF EOF()     && check if this master filer (GLBUDHD) 
               && if this budget still exist in the maseter file.
               && if this budget deleted....
               && replace lc_SelBuHd..CopyStat WITH 'Source deleted'

    SELECT (lc_SelBuHd)
    
    IF lnHdRecNo <= RECCOUNT()
      GOTO lnHdRecNo
    ENDIF
      
    REPLACE &lc_SelBuHd..CopyStat WITH 'Source deleted'
    REPLACE &lc_SelBuHd..Inc WITH '  ' 

  ELSE

    SELECT GLBUDHD
    SET ORDER TO TAG BDCODYR
    *** check if this budget exist in the source company
    IF SEEK (&lc_SelBuHd..cBudCode+&lc_SelBuHd..cBudYear)
      *** if this budget exist display message  
      *** budget Already exists do you want to overwrite 
      *** < Overwrite >  <Resume>  <Cancel>
      lnAction=gfModalGen("TRM02114B02015","Dialog",;
      "Budget"+' ' +ALLTRIM(&lc_SelBuHd..cBudCode)+;
               '\' +ALLTRIM(&lc_SelBuHd..cBudYear) + ' ')      

      DO CASE
        CASE lnAction=1   &&  Overwrite this budget

          IF lfChkDt()  && check details for this budget
                        && if valid to copy ..
                        && in this case delete old budget form
                        && active company ( Budget header + Details)

            SELECT glBudHd
            IF gfObj_Lock(.T.)
              DELETE FOR glBudHd.cBudCode     + glBudHd.cBudYear=;
                         &lc_SelBuHd..cBudCode + &lc_SelBuHd..cBudYear
 
              SELECT glBudDt
              DELETE FOR glBudHd.cBudCode     + glBudHd.cBudYear=;
                         &lc_SelBuHd..cBudCode + &lc_SelBuHd..cBudYear
                       
              =lfCopyHdDt()  && Copy cureent budget header and detail
                             && to the actice copmany..   
              SELECT glBudHd
              =gfObj_Lock(.F.)
                              
            ELSE
              SELECT (lc_SelBuHd)
              
              IF lnHdRecNo <= RECCOUNT()
                GOTO lnHdRecNo
              ENDIF
                
              REPLACE &lc_SelBuHd..CopyStat WITH 'Overwrite err.'
              REPLACE &lc_SelBuHd..Inc WITH '  ' 
            ENDIF
          ELSE
            *** if this budget not valid to copy to active company.
            *** in this case repalce copy status with 'Copy Error'
            SELECT (lc_SelBuHd)
            
            IF lnHdRecNo <= RECCOUNT()
              GOTO lnHdRecNo
            ENDIF
              
            REPLACE &lc_SelBuHd..CopyStat WITH 'Copy Error'
            REPLACE &lc_SelBuHd..Inc WITH '  ' 
          ENDIF
          
        CASE lnAction=2      &&  <Resume>
          *** in this case repalce copy status with 'Already exists'
          SELECT (lc_SelBuHd)
          
          IF lnHdRecNo <= RECCOUNT()
            GOTO lnHdRecNo
          ENDIF
            
          REPLACE &lc_SelBuHd..CopyStat WITH 'Already exists'
          REPLACE &lc_SelBuHd..Inc WITH '  ' 

        CASE lnAction=3      &&  <Cancel>
          *** in this case repalce copy status with 'Already exists'
          *** and terminat copy procedure.
          SELECT (lc_SelBuHd)
          
          IF lnHdRecNo <= RECCOUNT()
            GOTO lnHdRecNo
          ENDIF
            
          REPLACE &lc_SelBuHd..CopyStat WITH 'Already exists'
          REPLACE &lc_SelBuHd..Inc WITH '  ' 
          EXIT
       ENDCASE

    ELSE 
      && case active budget not exist in active company
      && in this case check budget details ..
      && and if return .T. then copy budget header and details
      && to the active company. 
      IF lfChkDt()
        =lfCopyHdDt()
      ELSE
        SELECT (lc_SelBuHd)
 
        IF lnHdRecNo <= RECCOUNT()
          GOTO lnHdRecNo
        ENDIF
          
        *** repalce copy status with 'Copy Error'        
        REPLACE &lc_SelBuHd..CopyStat WITH 'Copy Error'       
        REPLACE &lc_SelBuHd..Inc WITH '  ' 
      ENDIF

    ENDIF
  
  ENDIF

  SELECT (lc_SelBuHd)
  
  IF lnHdRecNo <= RECCOUNT()
    GOTO lnHdRecNo
  ENDIF
    
ENDSCAN

IF lnTotRecs <> lnCurRec
  FOR lnCounter = lnCurRec TO lnTotRecs
    =gfThermo(lnTotRecs,lnCounter,'Copying budget ...',' ')
  ENDFOR
ENDIF

*WAIT ALLTRIM(STR(lnReCopied))+" Budgets copied from source company..." ;
   WINDOW NOWAIT
=gfWait("02236","NOWAIT",ALLTRIM(STR(lnReCopied)))

SELECT (lc_SelBuHd)
*** restore current position in the list
lsBud = lnlistLine 
SHOW GET lsBud
=lfwList()

*!**************************************************************************
*!
*!      Function:  lfChkDt
*!
*!**************************************************************************
*

FUNCTION  lfChkDt

*** check every account belong to active budget..
llValdCopy = .T.
SELECT (lc_SelBuDt)

SCAN FOR &lc_SelBuDt..cBudCode+&lc_SelBuDt..cBudYear+&lc_SelBuDt..cBudPrd = ;
         &lc_SelBuHd..cBudCode+&lc_SelBuHd..cBudYear+'01'

  SELECT GLACCHAR
  *** check if this account exist in the target company
  IF !SEEK(&lc_SelBuDt..cAcctCode)

    SELECT (lc_SelBuDt)
    ***  repalce detail status with 'Account not found'        
    REPLACE &lc_SelBuDt..DetalStat WITH 'Account not found'
    llValdCopy = .F.
    LOOP

  ELSE 

    SELECT GLTYPES
    *** check if the type for active account
    *** exist in the target company (GLTYPES)
    IF SEEK(&lc_SelBuDt..cTypeCode)
      *** case found the type in the dbf (GLTYPES)
      *** check if this type between the values of
      *** ( lower limit       , upper limit       ) 
      *** GLTYPES.cTyplacno , GLTYPES.cTypuacno

      LOCATE FOR BETWEEN( VAL(&lc_SelBuDt..cAcctCode) , VAL(glTypes.cTypLacno) , VAL(glTypes.cTypUacno) ) 

      IF ! FOUND() 
        SELECT (lc_SelBuDt)
        *** case the type is out of range ... 
        *** repalce detail status with 'Out of range'        
        REPLACE &lc_SelBuDt..DetalStat WITH 'Out of range'
        llValdCopy = .F.
        LOOP
      ENDIF

    ELSE

      SELECT (lc_SelBuDt)
      ***  repalce detail status with 'Type not found        
      REPLACE &lc_SelBuDt..DetalStat WITH 'Type not found'
      llValdCopy = .F.
      LOOP
        
    ENDIF  

  ENDIF      
SELECT (lc_SelBuDt)
ENDSCAN 

IF llValdCopy 
  Return .T.    && this budget details valid to copy 
ELSE
  Return .F.    && this budget details not valid to copy 
ENDIF   


*!**************************************************************************
*!
*!      Function:  lfCopyHdDt
*!
*!**************************************************************************
*
FUNCTION  lfCopyHdDt

*** copy budget header and details to target company.
SELECT (lc_SelBuHd)
SCATTER MEMVAR
SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
INSERT INTO &gcDataDir.glBudHd FROM MEMVAR
SELECT glBudHd
=gfObj_Lock(.T.)
=gfAdd_Info("glBudHd")


SELECT (lc_SelBuDt)
SCAN FOR &lc_SelBuDt..cBudCode+&lc_SelBuDt..cBudYear = ;
         &lc_SelBuHd..cBudCode+&lc_SelBuHd..cBudYear  

  SCATTER MEMVAR
  SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
  INSERT INTO &gcDataDir.glBudDt FROM MEMVAR
  =gfAdd_Info("glBudDt")
  SELECT (lc_SelBuDt)

ENDSCAN

    lnReCopied = lnReCopied + 1   && increase number of copyied budgets
    SELECT  (lc_SelBuHd)
    REPLACE &lc_SelBuHd..CopyStat WITH 'Copied'
    REPLACE &lc_SelBuHd..Inc WITH '  ' 
    SELECT glBudHd
    =gfObj_Lock(.F.)

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** printing function


*!**************************************************************************
*!
*!      Function:  lfvClose
*!
*!**************************************************************************
*
FUNCTION  lfvClose

glQuitting  = .T.  
 
*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
*

FUNCTION lfTherm
lnSelected = lnSelected + 1

IF lnSelected % 13 = 0 
  =gfThermo(lnTotalRec,lnSelected,'Collecting budget lines...','')
ENDIF