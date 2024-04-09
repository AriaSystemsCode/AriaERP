*:************************************************************************
*:
*: Procedure file: GLCPSEG.PRG
*:
*:         System: GL System
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified: 1/26/94
*:
*:  Procs & Fncts:
*:
*:    FUNCTION  lfwPopup    ***  When company popup function        ***
*:    FUNCTION  lfvPopup    ***  Valid company popup function       ***
*:    FUNCTION  lfwList     ***  When list function                 ***
*:    FUNCTION  lfvSelOne   ***  Function to select one segment     ***
*:    FUNCTION  lfvSelAll   ***  Function to select all segments    ***
*:    FUNCTION  lfvSelNon   ***  Function to unselect all segments  ***
*:    FUNCTION  lfvInvert   ***  Function to invert select status   ***
*:    FUNCTION  lfvCopy     ***  Function to copy selected segments ***
*:    FUNCTION  lfvPrint    ***  Function to print copy stat.report ***
*:    FUNCTION  lfvSelSeg   ***  Function to select segments to copy***
*:    FUNCTION  lfvClose    ***  Close main screen                  ***
*:    FUNCTION  lfSamAcode  ***  To check if same account structure ***
*:    FUNCTION  lfwSegLst   ***  When segments list function        ***
*:    FUNCTION  lfvSegPop   ***  Valid segment type popup function  ***
*:    FUNCTION  lfwSegPop   ***  When segment type popup function   ***
*:    FUNCTION  lfvSegOne   ***  to select one segment from list    ***
*:    FUNCTION  lfvSegAll   ***  to select all segments from list   ***
*:    FUNCTION  lfvSegInv   ***  to invert segments status          ***
*:    FUNCTION  lfvSegNon   ***  to unselect all segments           ***
*:    FUNCTION  lfvAdd      ***  add selected segments to the list  ***
*:    FUNCTION  lfvCancel   ***  when press cancel                  ***
*:    FUNCTION  lfTherm     ***  activate global thermometer fun.   ***
*:               
*:      Documented 1/26/1994
*:************************************************************************
*:Modifications :
*:E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*:E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*:E300692,1 					SYCFSPRD, SYCFSHLD
*:E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*:           ccomp_id field .
*:E301077,71 Walid (WAM) 03/03/99 Open ACCOD file with gfOpenFile() .
*:B128958,1 NNA 09/04/2005 Fix bug that we get an error when copying segment values from another company
*:************************************************************************

DECLARE laComLst   [1]  && array of companys list
DECLARE laSegLst   [1]  && array of segments list
DECLARE laFileStru [1]

llNoContrl  = .T.
lcOldCom    = " "    && var. to store old company id     
lcComp_ID   = " "    && var. to store new company id  
lcCompany   = " "    && var. to return selected (company id,name) from popup
lcComPath   = " "    && var. to store the path for existing source company

lcSgRngSl   = "All Segments"
lcOld_Seg   = ""

lc_SelSeg   = " "    && Temp file name for selected segments
lc_Select   = " "    && Temp file name to select segments from list
*lcAlAcCod   = " "    && Alias name to GLAcCods  in source company
lcAlSgVal   = " "    && Alias name to GLSEGVAL in source company
lcSegs      = " "    && var. to hold main list string 
lcSelect    = " "    && var. to hold child list string

lcAcsSegNo  = " "

lnTotalRec  = 0      && var. to hold the number of segments (GLSEGVAL)
lnSelected  = 0      && var. to hold no of selected segments
lnlistLine  = 0      && var. to hold position pointer in the list 

lcBut1Stat  = "DISABLE"
lcBut2Stat  = "DISABLE"
lcPrompt    = "\<Select"
lcEmptyTxt  = 'No company Selected'

laCtrStat   = 'DISABLE' && To disable the browse pad in the menu

IF !gfSetup()    
  RETURN
ENDIF
*E301077,71 [BEGIN]
=gfOpenFile(gcDataDir+'ACCOD','','SH')
*E301077,71 [END..]

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
  lc_SelSeg  = gfTempName()  && Temp file name for selected segments
  lc_Select  = gfTempName()  && Temp file name for group lines

*  lcAlAcCod  = gfTempName() && Alias name to GLAcCods in source company
  lcAlSgVal  = gfTempName() && Alias name to GLSEGVAL in source company


  *** create empty temp file (lc_SelSeg) 
  *** with the same structure of GLSEGVAL + Inc, CopyStat

  SELECT GLSEGVAL
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

  CREATE TABLE &gcWorkDir.&lc_SelSeg FROM ARRAY laFileStru    

  INDEX ON  CACSSEGNO+CSEGVALUE TAG cSegVal

  *** create empty temp file (lc_Select) 
  *** with the same structure of GLSEGVAL + Inc

  SELECT GLSEGVAL
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]

  laFileStru[lnFileStru+1,1] = 'Inc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3
  laFileStru[lnFileStru+1,4] = 0

  CREATE TABLE &gcWorkDir.&lc_Select FROM ARRAY laFileStru    

  DO CASE
   CASE _DOS
     lcSegs   = "Inc+'    '+cAcsSegNo+'    '+cSegValue+'   '+SUBSTR(cSegLnDes,1,23)+'   '+CopyStat"
     lcSelect = "Inc+'  '+cAcsSegNo+'   '+cSegValue+' '+cSegLnDes"
   CASE _WINDOWS
     lcSegs   = "Inc+'   '+cAcsSegNo+'    '+cSegValue+' '+SUBSTR(cSegLnDes,1,22)+'  '+CopyStat"
     lcSelect = "Inc+'  '+cAcsSegNo+'   '+cSegValue+' '+cSegLnDes"
  ENDCASE

ENDIF

SELECT (lc_SelSeg)  
*E300683,5 Call *.SPR from screens directory
* DO GlCpSeg.SPR 
DO (gcScrDir + gcWinAppl + '\GlCpSeg.SPR')
*E300683,5 end   
*** case quit this program ...
IF glQuitting 

*  *** close temp file (lcAlAcCod) 
*  IF USED(lcAlAcCod)
*    SELECT(lcAlAcCod)
*    USE
*  ENDIF
 
  *** close temp file (lcAlSgVal) 
  IF USED(lcAlSgVal)
    SELECT(lcAlSgVal)
    USE
  ENDIF

  *** close temp file (lc_SelSeg) then erase this file. 
  IF USED(lc_SelSeg)
    SELECT(lc_SelSeg)
    USE
  ENDIF

  ERASE &gcWorkdir.&lc_SelSeg..DBF
  ERASE &gcWorkdir.&lc_SelSeg..CDX

  *** close temp file (lc_Select) then erase this file.
  IF USED(lc_Select)
    SELECT(lc_Select)
    USE
  ENDIF
  
  ERASE &gcWorkdir.&lc_Select..DBF
  ERASE &gcWorkdir.&lc_Select..CDX

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
  =lfRefresh()    && refresh says filed on the screen


*  *** check if the temp file (lcAlAcCod) is used before
*  IF USED(lcAlAcCod)
*    *** if used then select this area 
*    SELECT (lcAlAcCod)
*  ELSE   
*    *** else select a new empty work area
*    SELECT 0   
*  ENDIF  
*  *** use GLAcCods from the source company in the path lcComPath
*  USE &lcComPath.GLAcCods AGAIN ALIAS  &lcAlAcCod 


  *** check if the temp file (lcAlSgVal) is used before
  IF USED(lcAlSgVal)
    *** if used then select this area 
    SELECT (lcAlSgVal)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  USE &lcComPath.GLSegVal AGAIN ALIAS  &lcAlSgVal ORDER TAG ACSSEGVAL      


  SELECT(lc_SelSeg)
  SET RELATION TO CACSSEGNO+CSEGVALUE INTO &lcAlSgVal 

  lcSgRngSl   = "All Segments"

*  SELECT(lcAlAcCod)
*  SELECT  cAcsshDes + " " + cAcsLgDes;
*    FROM &lcComPath.GLACCODS;
*    WHERE !EMPTY(Nacssegno);
*    INTO ARRAY laSegLst

*E300692,1 Change file name from sycaccod to accod
  *SELECT SYCACCOD
  *SELECT cAcsshDes + " " + cAcsLgDes ;
   FROM  &gcSysHome.SYCACCOD ;
   WHERE !EMPTY(Nacssegno) .AND. ;
         sycaccod.ccomp_id = lcComp_ID ;
   INTO  ARRAY laSegLst
   SELECT ACCOD
   *E300789,7 [BEGIN]
   *SELECT cAcsshDes + " " + cAcsLgDes ;
   *FROM  &gcDataDir.ACCOD ;
   *WHERE !EMPTY(Nacssegno) .AND. ;
   *      accod.ccomp_id = lcComp_ID ;
   *INTO  ARRAY laSegLst
   SELECT cAcsshDes + " " + cAcsLgDes ;
   FROM  &gcDataDir.ACCOD ;
   WHERE !EMPTY(Nacssegno);
   INTO  ARRAY laSegLst
   *E300789,7 [END..]
 *E300692,1 end

  DIMENSION laSegLst[ALEN(laSegLst,1)+1,1]
  =AINS(laSegLst,1)
  laSegLst[1] = 'All Segments '

  SELECT *,'   ' AS 'Inc' ,'S' AS 'cStatus' ;
    FROM   &lcAlSgVal;
    INTO DBF &gcWorkDir.&lc_Select;

  SELECT (lc_Select)
  INDEX ON  CACSSEGNO+CSEGVALUE TAG cSeg_Value


  SELECT (lc_SelSeg)
  
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
    *** select (lc_SelSeg) and removes all records.
    *** and refresh the list window
    SELECT (lc_SelSeg)
    lsSegs   = 1     
    SHOW GET lsSegs  
    ZAP       
    SHOW GET lsSegs  
  ENDIF


  lcBut1Stat     = "ENABLE"
  SHOW GET pbSelSeg ENABLE
  lcBut2Stat     = "DISABLE"
  SHOW GET pbSelOne DISABLE
  SHOW GET pbSelAll DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbInvert DISABLE
  SHOW GET pbCopy   DISABLE
  lsSegs   = 1     
  SHOW GET lsSegs  

ENDIF

*!**************************************************************************
*!
*!      Function:  lfwList
*!
*!**************************************************************************
*
FUNCTION  lfwList
*** check if the number of segments in the list greater than 1 

IF RECCOUNT(lc_SelSeg) > 0 
  IF !EMPTY(CopyStat)
      lcPrompt = "Select"
      SHOW GET pbSelOne,1 PROMPT 'Select' DISABLE
  ELSE 
  *** check the status for (Inc) field 
    IF !EMPTY(Inc)
      *** case the selected .. change the prompt to 'unselect'
      lcPrompt = "\<Unselect"
      SHOW GET pbSelOne,1 PROMPT '\<Unselect'
    ELSE
      *** case the unselected .. change the prompt to 'select'
      lcPrompt = "\<Select"
      SHOW GET pbSelOne,1 PROMPT '\<Select' 
    ENDIF
  ENDIF  
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
lnlistLine = lsSegs
IF EMPTY(CopyStat)
  REPLACE Inc WITH IIF(Inc=' û','  ',' û')
  *** refresh the list and change prompt according to new status.
  SHOW GET lsSegs
  =lfwList()
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*
FUNCTION  lfvSelAll 

*** check the field status (Inc) for all the file (lc_SelSeg)
*** and change status from unselect to select
lnlistLine = lsSegs
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),' û','')
*** refresh the list and change prompt according to new status.
lsSegs = lnlistLine 
SHOW GET lsSegs
=lfwList()


*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 

*** check the field status (Inc) for all the file (lc_SelSeg)
*** and change status from select   to unselect
lnlistLine = lsSegs
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),'  ','')

*** refresh the list and change prompt according to new status.
lsSegs = lnlistLine 
SHOW GET lsSegs
=lfwList()


*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!
*!**************************************************************************
*
FUNCTION  lfvInvert 

*** check the status field (Inc) for all the file (lc_SelSeg)
*** and change status from select   to unselect
***                or from unselect to select
lnlistLine = lsSegs
REPLACE  ALL Inc WITH IIF(EMPTY(CopyStat),IIF(Inc=' û','  ',' û'),'')
*** refresh the list and change prompt according to new status.
lsSegs = lnlistLine 
SHOW GET lsSegs

=lfwList()

*!*************************************************************************
*!
*!     Function:  lfvCopy 
*!
*!**************************************************************************
*
FUNCTION  lfvCopy

*** start copy selected segments
SELECT (lc_SelSeg)
lnlistLine = lsSegs

*** count the number of included segments to the variable lnTotRecs
COUNT FOR !EMPTY(Inc) TO lnTotRecs

*** check if there is any selected segments to copy
*** case there is no selected segments to copy
IF lnTotRecs = 0 
   *** Message "No included segments to copy."
   *** and change the current object to (Select) 
   *** then exit from this function.
   =gfModalGen("TRM02110B00000","DIALOG")   
   _CUROBJ=OBJNUM(pbSelOne)
  RETURN
ENDIF

lnCopySegs = 0               && init. the no of copied segments.
lnCurRec   = 0               && init. the current segment variable.

*** start copying all selected segments only
SCAN FOR !EMPTY(Inc)
lnCurRec = lnCurRec + 1
=gfThermo(lnTotRecs,lnCurRec,'Copying segment ...',cAcsSegNo + '  ' + cSegValue)

IF EOF(lcAlSgVal)
  REPLACE &lc_SelSeg..CopyStat WITH 'Source deleted' 
ELSE
  *** searches the current dbf (GLACCHAR) if the current segment
  *** number in the dbf (lc_SelSeg) is Already exists 

  IF SEEK (cAcsSegNo+cSegValue,"GLSEGVAL")
    *** if found then change the copy status field to "Already exists"
    REPLACE &lc_SelSeg..CopyStat WITH 'Already exists'
  ELSE  
    IF cAcsSegNo = "1"
      IF SEEK (cTypeCode,"GLTYPES")
        IF !BETWEEN(VAL(cSegValue),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno))
          REPLACE &lc_SelSeg..CopyStat WITH 'Out of range'
          LOOP
        ENDIF
      ENDIF   
    ENDIF

    SELECT (lc_SelSeg)    
    SCATTER MEMVAR
    SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
    INSERT INTO &gcDataDir.GLSEGVAL FROM MEMVAR
    =gfAdd_Info('GLSEGVAL')
    REPLACE &lc_SelSeg..CopyStat WITH 'Copied'
    *** increment the number of copyied segments by 1 
    lnCopySegs = lnCopySegs + 1
  ENDIF
ENDIF
  
SELECT (lc_SelSeg)
ENDSCAN

IF lnTotRecs <> lnCurRec
  FOR lnCounter = lnCurRec TO lnTotRecs 
    =gfThermo(lnTotRecs,lnCounter,'Copying segment ...')
  ENDFOR
ENDIF

REPLACE ALL  &lc_SelSeg..Inc WITH '  '
lsSegs = lnlistLine 
SHOW GET lsSegs  
*WAIT ALLTRIM(STR(lnCopySegs))+" Segments copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02238","NOWAIT",ALLTRIM(STR(lnCopySegs)))
IF lnCopySegs > 0
*  REPLACE GLACCODS.lAcsUsed WITH .T.
*E300692,1 Change file name and path from sycaccod to accod
  *IF SEEK(lcComp_ID,"SYCACCOD")
  *REPLACE SYCACCOD.lAcsUsed WITH .T.

  *B128958,1 NNA 09/04/2005 (Start) lcComp_ID Refer to the Company that we Copy From . But the Opened file 
  *B128958,1 NNA            is in the current file so I close it then open it from the Company that we Copy From
  *B128958,1 NNA            then comment the seek line because this file dosen't has a Comp_id Field, this filed was
  *B128958,1 NNA            in (SYCACCOD) 
  *IF SEEK(lcComp_ID,"ACCOD")
  *REPLACE ACCOD.lAcsUsed WITH .T.
  
  IF USED('ACCOD')
    USE IN ACCOD
  ENDIF
  *-- use the Path of the Copy From company.
  =gfOpenFile(lcComPath+'ACCOD','','SH')
  SELECT ACCOD
  *--Relpace the first record with True
  GO TOP
  REPLACE ACCOD.lAcsUsed WITH .T.
  SELECT (lc_SelSeg)
  lnRecNO = RECNO()
  SCAN
    SELECT ACCOD
    LOCATE FOR STR(NACSSEGNO,1) = &lc_SelSeg..CACSSEGNO
    IF FOUND()
      *--Replace lAcsUsed field with .T. for the Segments that we Copied.
      REPLACE ACCOD.lAcsUsed WITH .T.
    ENDIF
  ENDSCAN
  IF BETWEEN(lnRecNo,1,RECCOUNT(lc_SelSeg))
    GOTO lnRecNo IN lc_SelSeg
  ENDIF
  *B128958,1 NNA (End)
*E300692,1 end

  *B128958,1 NNA (Start)
  *ENDIF
  *B128958,1 NNA (End)
ENDIF

=lfwList()

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
*!      Function:  lfvSelSeg
*!
*!**************************************************************************
*

FUNCTION  lfvSelSeg
*** select segments form the source company to be copied.
SELECT (lc_Select)
*E300683,5 Call *.SPR from screens directory
* DO glSlSeg.SPR 
DO (gcScrDir + gcWinAppl + '\glSlSeg.SPR')
*E300683,5 end   
SELECT (lc_SelSeg)
SHOW GET lsSegs  

IF RECCOUNT() > 0 
  lcBut2Stat     = "ENABLE"
  SHOW GET pbSelOne ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbCopy   ENABLE
  lsSegs   = 1     
  SHOW GET lsSegs  
  _CUROBJ= OBJNUM(lsSegs)
  =lfwList()
ELSE 
  lcBut2Stat     = "DISABLE"
  SHOW GET pbSelOne DISABLE
  SHOW GET pbSelAll DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbInvert DISABLE
  SHOW GET pbCopy   DISABLE
  _CUROBJ= OBJNUM(pbSelSeg)
ENDIF


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
*!      Function:  lfwSegLst
*!
*!**************************************************************************
*
FUNCTION lfwSegLst

*** check the status for (Inc) field 
IF !EMPTY(&lc_Select..Inc)
  *** case the selected .. change the prompt to 'unselect'
  SHOW GET pbSele,1 PROMPT '\<Unselect'
ELSE
  *** case the unselected .. change the prompt to 'select'
  SHOW GET pbSele,1 PROMPT '\<Select' 
ENDIF

*!**************************************************************************
*!
*!      Function:  lfwSegPop
*!
*!**************************************************************************
*
FUNCTION lfwSegPop

lcOld_Seg = lcSgRngSl


*!**************************************************************************
*!
*!      Function:  lfvSegPop
*!
*!**************************************************************************
*
FUNCTION lfvSegPop

SELECT (lc_Select)

DO CASE
  CASE _DOS
    lcSgRngSl  = gfActPop(2,2,8,52,'laSegLst',1,1,@lcSgRngSl)
  CASE _WINDOWS
    lcSgRngSl  = laSegLst[lcSgRange]
ENDCASE

IF EMPTY(lcSgRngSl)
  lcSgRngSl = lcOld_Seg
ENDIF


SHOW GETS 
SET FILTER TO
LOCATE
REPLACE ALL  &lc_Select..Inc WITH '  '

DO CASE 
  CASE lcSgRngSl = "All Segments"
    SET FILTER TO
    LOCATE
  OTHERWISE
  *E300692,1 Change file name from sycaccod to accod
    *SELECT SYCACCOD
    SELECT ACCOD
  *E300692,1 end
    
    *E300789,7 [BEGIN]
    *LOCATE FOR ccomp_Id = lcComp_ID .AND. ;
    *           cAcsshDes = SUBSTR(lcSgRngSl,1,15)
    *IF FOUND()
    *  lcAcsSegNo = STR(nAcsSegNo,1)
    *ELSE
    *  lcAcsSegNo = " "
    *ENDIF
    GO TOP
    IF !EOF()
      lcAcsSegNo = STR(nAcsSegNo,1)
    ELSE
      lcAcsSegNo = " "
    ENDIF    
    *E300789,7 [END..]
    SELECT (lc_Select)
    SET FILTER TO cAcsSegNo = lcAcsSegNo
    LOCATE
ENDCASE

lsSelect = 1 
SHOW GET lsSelect
=lfwSegLst()

*!**************************************************************************
*!
*!      Function:  lfvSegOne
*!
*!**************************************************************************
*
FUNCTION lfvSegOne

REPLACE  &lc_Select..Inc WITH IIF(Inc=' û','  ',' û')
*** refresh the list and change prompt according to new status.
SHOW GET lsSelect
=lfwSegLst()


*!**************************************************************************
*!
*!      Function:  lfvSegAll
*!
*!**************************************************************************
*
FUNCTION lfvSegAll

REPLACE ALL  &lc_Select..Inc WITH ' û'
*** refresh the list and change prompt according to new status.
SHOW GET lsSelect
=lfwSegLst()


*!**************************************************************************
*!
*!      Function:  lfvSegInv
*!
*!**************************************************************************
*
FUNCTION lfvSegInv

REPLACE  ALL &lc_Select..Inc WITH IIF(Inc=' û','  ',' û')
*** refresh the list and change prompt according to new status.
SHOW GET lsSelect
=lfwSegLst()


*!**************************************************************************
*!
*!      Function:  lfvSegNon
*!
*!**************************************************************************
*
FUNCTION lfvSegNon

REPLACE ALL  &lc_Select..Inc WITH '  '
*** refresh the list and change prompt according to new status.
SHOW GET lsSelect
=lfwSegLst()


*!**************************************************************************
*!
*!      FUNCTION: lfvAdd
*!
*!**************************************************************************
* Validation for Add push button
FUNCTION lfvAdd

SELECT (lc_Select)
lnTotalRec = 0 
lnMerged   = 0 
COUNT FOR !EMPTY(Inc) TO lnTotalRec

IF lnTotalRec = 0
    *** message "You Have to select type"
    =gfModalGen("TRM02094B02014","DIALOG","segment")
    *** terminate the Function "lfvAdd"
    RETURN
ENDIF
	
lnSelected  = 0
SELECT (lc_SelSeg)

IF RECCOUNT() = 0 
    APPEND FROM &gcWorkDir.&lc_Select FOR !EMPTY(Inc) .AND. lfTherm()
    IF lnTotalRec <> lnSelected
      FOR lnCounter = lnSelected TO lnTotalRec 
        =gfThermo(lnTotalRec,lnCounter,'Adding segment..',' ')
      ENDFOR
    ENDIF
    SELECT (lc_Select)
    REPLACE ALL  &lc_Select..Inc WITH '  '
    SELECT (lc_SelSeg)
    REPLACE ALL  &lc_SelSeg..Inc WITH '  '
    CLEAR READ 
    *WAIT ALLTRIM(STR(lnSelected)) + ' Segments Added ..' WINDOW NOWAIT
    =gfWait("02239","NOWAIT",ALLTRIM(STR(lnSelected)))
ELSE
  lnOption = gfModalGen("TRM02113B02012","DIALOG")
  DO CASE
    *** Replace option is selected
    CASE lnOption = 1
      ZAP
      APPEND FROM &gcWorkDir.&lc_Select FOR !EMPTY(Inc) .AND. lfTherm()
      IF lnTotalRec <> lnSelected
        FOR lnCounter = lnSelected TO lnTotalRec
          =gfThermo(lnTotalRec,lnCounter,'Adding segment..',' ')
        ENDFOR  
      ENDIF
      SELECT (lc_Select)
      REPLACE ALL  &lc_Select..Inc WITH '  '
      SELECT (lc_SelSeg)
      REPLACE ALL  &lc_SelSeg..Inc WITH '  '
      CLEAR READ 
      *WAIT ALLTRIM(STR(lnSelected)) + ' Segments Added ..' WINDOW NOWAIT
      =gfWait("02239","NOWAIT",ALLTRIM(STR(lnSelected)))
    *** Merge option was selected
    CASE lnOption = 2
      SELECT (lc_Select)
      SCAN  FOR !EMPTY(Inc) .AND. lfTherm()
        IF !SEEK (cAcsSegNo+cSegValue,lc_SelSeg)
          SCATTER MEMVAR MEMO
          SELECT(lc_SelSeg)
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE &lc_SelSeg..Inc WITH '  '
          SELECT (lc_Select)          
          lnMerged = lnMerged + 1
        ENDIF     
      ENDSCAN
      SELECT (lc_Select)
      REPLACE ALL  &lc_Select..Inc WITH '  '
      IF lnTotalRec <> lnSelected
        FOR lnCounter = lnSelected TO lnTotalRec 
          =gfThermo(lnTotalRec,lnCounter,'Copying segment ...')
        ENDFOR
      ENDIF
      CLEAR READ 
*      WAIT ALLTRIM(STR(lnMerged)) + ' Segments Added ..' WINDOW NOWAIT
      =gfWait("02239","NOWAIT",ALLTRIM(STR(lnMerged)))
    CASE lnOption = 3
        RETURN         
  ENDCASE

ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfvCancel
*!
*!**************************************************************************
* 
FUNCTION lfvCancel

 SELECT (lc_Select)
 REPLACE ALL  &lc_Select..Inc WITH '  '

*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
*

FUNCTION lfTherm

lnSelected = lnSelected + 1
=gfThermo(lnTotalRec,lnSelected,'Adding segment..',cAcsSegNo + ;
          '  ' + cSegValue)