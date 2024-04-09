*:************************************************************************
*: Procedure file: GLACTCH.PRG 
*:                 Flexible GL Chart of Accounts Transfer
*:         System: ARIA27
*:         Module: GL
*:      Developer: TMI - TAREK MOHAMMED IBRAHIM
*:           Date: 11/08/2005 ( Nov-11-2005 )
*:        Entry  : C # 129762,1 
*:************************************************************************
* Modifications
*B040141,1 TMI 03/15/2006 Exclude the accounts in glsetup file from being updated by adding new non-zero segments
*:************************************************************************


*-- laScrMode  Array that hold the screen modes
DIMENSION laScrMode[4],laFoldWinds[3,2],laComp[1],laData[4],laOldData[4]
lnOpnDm = 5           && number of dimensions of the array laOpenFls
DIMENSION laOpenFls[1,lnOpnDm]
laOpenFls[1,1] = ''   && Store the alias name ( ACCOD is already open )
laOpenFls[1,2] = ''        && Stores the list of fields in this alias holding GL-Accounts
laOpenFls[1,3] = ''        && Saves The order the file opened with it
laOpenFls[1,4] = ''        && Saves a macro to run later , e.g 'WITH CDX' , used in COPY command
laOpenFls[1,5] = .F.       && if .T. indicates that this file needs a special procedure to update 
                           && its GL-Accounts fields

DIMENSION laObjStat[6]  && Array to hold the display status of each seg.
DIMENSION laObjdisp[6]  && Array to control the display of seg. Des. in Child.
DIMENSION laAcDes[6,3]  && Array to hold the description of each seg.
laAcDes = ' '           && Default blank to seg description array

lnMxWd   = 15           && Define the maximum width of a segment included in GL-Accounts
lnXSgPos = 26.200       && X- Position of the six variables lcSegVal -Get this values from SMactch.spr for lcSegVal1 
DIMENSION laYSgPos[6]   && Y- position of the six variables lcSegVal...
laYSgPos[1] = 13.077
laYSgPos[2] = 14.615
laYSgPos[3] = 16.154
laYSgPos[4] = 17.692
laYSgPos[5] = 19.231
laYSgPos[6] = 20.769

lnLineRange = 1.77      && the line height for the dbl click
*- List of GL-Account fields in Codes file to use to update the codes file
lcCodesFlds = 'GLACCOUNT ,CARGLACC  ,CADJACCT  ,CGLINPACCT,CGLOUTACCT,CFRGTACNT ,CGLACCT   ,CCHKACCT  '

lcPercent  = ''  && proceed percent
*- List of setups entries that may store GL-Accounts
lcSetupsAc = 'LNEXRATACC'

*** Default value of all variables holding the seg. width. ***
STORE 0 TO lnSeg1,lnSeg2,lnSeg3,lnSeg4,lnSeg5,lnSeg6
*** Default value of all variables holding the old seg. width. *** Updated in When event of seg. fields
STORE 0 TO lnOldSeg1,lnOldSeg2,lnOldSeg3,lnOldSeg4,lnOldSeg5,lnOldSeg6,lnOldSNo
STORE 0 TO lnLogStr     && LOG variable
DIMENSION laOldSeg[6]   && Store old segmet widthes values
laOldSeg   = 0
lnComp     = 0          && Company #

lnBoxh     = 16         && Max size of box in Child screen is 16
gntherwidth= 57         && maximum widht of the thermometer width screen
laObjStat  = "DISABLE"  && Default status of all seg. is DISABLE
lcOldVal   = ''         && Hold the old value of any object in the screen

laObjdisp  = .T.        && Default Display all lines in child screen 
llCancel   = .F.        && Flag to indicate if there is a canslation in
                        && the child screen
llChange   = .F.        && Check if any change in the data
                        && the platform is DOS
llHistComp = .F.        && Check if selected company is a history company                
lcHistComp = ''         && The company that is a history of the current company
llProceed  = .F.        && if the user selected proceed button
llwComp    = .F.        && Set to .T. if the lnComp is entered for the first time

llCanOpn   = .T.

DIMENSION laStrHst[1]
laStrHst   = ''         && show changed histroy structrue

lcCompDir  = ''         && Current company dir
ibCompany  = 1
puComp     = " "
puComp_ID  = " "
lcCompany  = " "
lcComp_Id  = " "
lcOldComp  = " "
lcDataDir  = " "         && Hold the company setup dir.

*B040141,1  TMI [Start] Initialize excepted values to be updated by only zeros
lcExcepted = ''
*B040141,1  TMI [End  ] 

*- Make a procedure that if the power is lost while commiting , then the screen must be opened and 
*- recommit and complete the procedure by erasing the temp commite folder data ,
*- or ger the user the  option to restore saved backup of the data.
llContComt = .F. .AND. FILE(FULLPATH('')+'GLCHNGR.DAT')  && Disable it
                        && Continue committing 

lcConfirm  = SET('CONFIRM')
SET CONFIRM OFF

lcAccExct  = SET('EXACT')
SET EXACT OFF

DIMENSION laChgHst[1]
laChgHst = ''
STORE 1 TO lnChgHst 

laComp[1] = ' '
STORE ' ' TO laOldData
STORE SPACE(23)  TO lcSegVal1,lcSegVal2,lcSegVal3,lcSegVal4,lcSegVal5,lcSegVal6
STORE ''  TO lcCommtFld,lcBckUpFld,lcBackUp,lcTmpBrw

*- Define segment variables arrays
DIMENSION laSegVal1[lnMxWd],laSegVal2[lnMxWd],laSegVal3[lnMxWd],laSegVal4[lnMxWd],laSegVal5[lnMxWd],laSegVal6[lnMxWd]
STORE ' ' TO laSegVal1,laSegVal2,laSegVal3,laSegVal4,laSegVal5,laSegVal6
STORE ''  TO lcTmpIDX,lcOkStat

*- Browse variable
llBrowse = .F.

*- Initialize State variables for the screen
STORE '' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcfolder,lcfoldprnt,;
            lcWin0,lcWin1,lcWin2,lcWin3,lcScFields
*-* STORE '' TO lcChngSeg,lcViewAcct,lcGlStrHst
STORE '' TO lcChngSeg,lcViewAcct,lcNewAcct

&& Upper and lower limits for the number of segments
lnMn  = 1
lnMx  = 6

lcDummyCom   = REPLICATE("~",2)

*- Screen title
lcWindTitl = 'Flexible GL Chart of Accounts Changer.'

*--Initialize the Folders needed variables.
STORE 1   TO lnActFolder
lnFolderCEnd  = 102.50            && Folder Column end.
lnFolderREnd  =   2.00            && Folder Row end.
lnNoFld       =   3               && Number of folders.
lcwfoldchng   = '=lfActFolder()'  && function to control shows after change the folder.
lcfoldpush    = 'pbFolders'       && push button name for the next folder.

*:* Purpose     : If there are any logged users , do not allow the screen to proceed unless all users logged out, 
*:* While proceeding prevent any other user to log in 
*:* This at the when function of the screen
IF gfUSERLIST(.T.)>1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Other users logged in , can not open the screen')
  RETURN .F.
ENDIF 

IF !gfSetup()
  RETURN
ENDIF  

IF !WEXIST(gcBaseWind)

  *- if no users logged in , then lock the system so no other user can log in
  SELECT SYCINST
  REPLACE LLOCKSYS WITH .T.
  =RLOCK()
  
  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()
  
  lcWin0 = gfTempName()
  lcWin1 = gfTempName()
  lcWin2 = gfTempName()
  lcWin3 = gfTempName()
  
  *- Temp files names
  lcChngSeg  = gfTempName()
  lcViewAcct = gfTempName()
  lcNewAcct  = gfTempName()
*-*   lcGlStrHst = gfTempName()
  
  lcCommtFld = gfTempName()  && Commit Folder
  lcBckUpFld = gfTempName()  && Backup Folder
  
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && Window parent name for the folder
  lnActFolder = 1                   && Active folder

  *--Folder array definition.
  lafoldwinds[1,1] = 'Account code structure'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Processing Log'
  lafoldwinds[2,2] = lcWinCh2
  lafoldwinds[3,1] = 'GL change/Transfer History'
  lafoldwinds[3,2] = lcWinCh3
  
  SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID,PADR(gfGetDataDir(ALLT(cCom_DDir)),LEN(cCom_dDir)),'~'+CCOMP_ID+'~' ;         
         FROM SYCCOMP ;
         ORDER BY CCOMP_ID ;
         WHERE !DELETED('SYCCOMP') ;
         INTO ARRAY laComp 
  
  lnComp = 1
  *- Show the current company 
  IF ASCAN(laComp,'~'+gcAct_comp+'~')>0
    lnComp = ASUBSCRIPT(laComp , ASCAN(laComp,'~'+gcAct_comp+'~') , 1)  
  ENDIF  
  
  *- Initialize screen modes array
  laScrMode    = .F.
  laScrMode[1] = .T.
  
  lcScFields = 'NACSSEGSZ,CACSMASK,CACSEGDES,NACSNOSEG'

  *- Create the temp log file
  CREATE TABLE (gcWorkDir+'LOGFILE') (mLogStr C(200))

ENDIF

DIMENSION laData[4]
STORE 0   TO laData[1],laData[4]
STORE ' ' TO laData[2],laData[3]

*- As per Wael , do not check the History company  [Start] 11/22/2005
llHistComp = .F.
lcHistComp = ''
*- As per Wael , do not check the History company  [End  ] 11/22/2005


PUSH KEY                                     && To save the the current on key label
ON KEY
ON KEY LABEL ESC DO lfvCancel
DO (gcScrDir + gcWinAppl + '\SMACTCH.SPR')   
POP KEY                                      && To Restore the previous assignments for on key label

*-- If we realy quitting the screen 
IF glQuitting
  
  *- Allow other users to log in to the system
  SELECT SYCINST
  REPLACE LLOCKSYS WITH .F.
  UNLOCK
  
  *- Reload old settings
  SET CONFIRM &lcConfirm
  SET EXACT   &lcAccExct  
  
  *- Remove temp files created
  =lfEraseFl(lcChngSeg)
  =lfEraseFl(lcViewAcct)
  =lfEraseFl(lcNewAcct)
  
*-*   =lfEraseFl(lcGlStrHst)

  *- Close aria files opened
  =lfClose()
    
  *- Erase the Commited temp files
  IF llProceed
    lcComt = gcWorkDir + laComp[lnComp,2] + SUBSTR(lcCommtFld,3) + '\'
    FOR lnK = 1 TO ALEN(laOpenFls,1)
      IF !EMPTY(laOpenFls[lnK,1])
        ERASE (lcComt+laOpenFls[lnK,1]+'.DBF')
        ERASE (lcComt+laOpenFls[lnK,1]+'.CDX')
        ERASE (lcComt+laOpenFls[lnK,1]+'.FPT')
      ENDIF
    ENDFOR  
  ENDIF

  ERASE (gcWorkDir+lcTmpIDX+'.IDX')
  
ENDIF  

*                          **********************                          *
*:                         Functions & Procedures                         :*
*                          **********************                          *


*:**************************************************************************
*:* Name        : lfwStart
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : When function of the screen SMACTCH.SPR
*:***************************************************************************
FUNCTION lfwStart
PRIVATE lnHndl,lcComp,lcBkup
IF llContComt
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The commit process failed, Restoring the data from the backup folder.')
  lnHndl     = FOPEN(FULLPATH('')+'GLCHNGR.DAT')
  lcComp     = FGETS(lnHndl)
  lcBckUpFld = FGETS(lnHndl)
  lcBkup = gcWorkDir + lcComp + lcBckUpFld + '\' 
  =FCLOSE(lnHndl)  
  lnComp = ASUBSCRIPT(laComp , ASCAN(laComp,'~'+lcComp+'~') , 1) 

  lcCompDir = ALLTRIM(laComp[lnComp,3])
  IF lfOpenFls() 
    WAIT WINDOW NOWAIT 'Restart committing.. '
    FOR lnK = 1 TO ALEN(laOpenFls,1) 
      =gfThermo(ALEN(laOpenFls,1),lnK,'Restoring files from the backup folder.',laOpenFls[lnK,1])
      SELECT (laOpenFls[lnK,1])
      ZAP
      APPEND FROM (lcBkup+laOpenFls[lnK,1]+'.DBF')
      ERASE (lcBkup+laOpenFls[lnK,1]+'.DBF')
      ERASE (lcBkup+laOpenFls[lnK,1]+'.CDX')
      ERASE (lcBkup+laOpenFls[lnK,1]+'.FPT')
    ENDFOR 
    ERASE (FULLPATH('')+'GLCHNGR.DAT')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'All files containing GL-Accounts restored successfully.')
  ENDIF
  RETURN .F.
ENDIF
*-- end of lfwStart.

*:*************************************************************
*! Name      : lfActFolder
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/08/2005
*! Purpose   : Activate folder when Change.
*!           : This screen consists of three tab-pages: 
*!           : 1)The first tab-sheet is where the user will view the existing GL Account code 
*!           :   structure and may add new segments to his/her existing account code structure.  
*!           : 2)The second tab-sheet will be a processing log that will show each action taken
*!           :   by the system in order to update Aria with the new structure. 
*!           : 3)The third tab-sheet will display all the changes that have taken place on the 
*!           :   account code structures for all companies. 
*:*************************************************************
FUNCTION lfActFolder
PRIVATE lcSegValSt,lnCount,lcI,lcCommSt
IF laScrMode[1]
  RETURN
ENDIF

DO CASE

*--First Folder 
CASE lnactfolder = 1
  SHOW WINDOW (lcWinCh1) TOP
  IF laScrMode[2]
    SHOW GETS WINDOW (lcWinCh1) ENABLE  ONLY
  ELSE
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GET pbCancel ENABLE
  ENDIF

  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    
  *- segment values variables status
  IF laScrMode[2]
    =lfvData_4(.T.)
  ENDIF
    
*--Second Folder
CASE lnactfolder = 2
  SHOW WINDOW (lcWinCh2) TOP
  SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
  lcCommSt = IIF(laScrMode[3],'ENABLE','DISABLE')
  SHOW GET pbCommit &lcCommSt

  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY

*--Third Folder 
CASE lnactfolder = 3
  SHOW WINDOW (lcWinCh3) TOP
  SHOW GETS WINDOW (lcWinCh3) ENABLE  ONLY

  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  
  *- Update the history folder
  =lfUpChgHst()
  =lfwChgHst()

ENDCASE
=lfRefresh()


*:**************************************************************************
*:* Name        : lpShow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Handling the screen mode
*:***************************************************************************
PROCEDURE lpShow

PRIVATE lnSlct
lnSlct = SELECT(0)

DO CASE

  *-- S e l e c t  M o d e
  CASE laScrMode[1]
  
    lnlastfold = lnActFolder
    lnActFolder= 1
    =lfchngfolder(lnActFolder)
    SHOW GET ibFolder(1) ENABLE
    SHOW GET ibFolder(2) DISABLE    
    SHOW GET ibFolder(3) DISABLE    

  *-- E d i t  M o d e
  CASE laScrMode[2]
  
  *-  P r o c e e d  M o d e
  CASE laScrMode[3]
  
    SHOW GET ibFolder(2) ENABLE
    SHOW GET ibFolder(3) ENABLE

    lnlastfold = lnActFolder
    =lfchngfolder(lnactfolder)

  *- C o m m i t   M o d e 
  CASE laScrMode[3]


ENDCASE
=lfRefresh()

SELECT(lnSlct)

*:*************************************************************
*! Name      : lfTrapKy
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/08/2005
*! Purpose   : Trap key.
*:*************************************************************
*
FUNCTION lfTrapKy

*-- End Of lfTrapKy.
*!*************************************************************
*! Name      : lfReadAct
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/08/2005
*! Purpose   : READ Activate function
*!*************************************************************
FUNCTION lfReadAct
  
*--End Of lfReadAct.

*:**************************************************************************
*:* Name        : lfSelCurr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2005
*:* Purpose     : If the field lcSegVal1 , ... 6 is in read , and when a mouse is
*               : clicked in any place in the screen then send a tab key to go out of this control
*:***************************************************************************
FUNCTION lfSelCurr
PRIVATE lcPack_Id
CLEAR TYPEAHEAD
lnX =INT(MROW())  && get the mouse row
lnY =INT(MCOL())  && get the mouse column
*-- if the mouse click is inside the browse window 
IF  (MROW(lcWinch1)<>-1) AND (MCOL(lcWinch1)<>-1) AND (MROW(lcWinch1)>lnLineRange);
      AND BETWEEN(MCOL(lcWinch1),2,WCOL(lcWinch1)-1.77);
     AND BETWEEN(MROW(lcWinch1),2,WROWS(lcWinch1)-1.77)
ELSE
  *-- if the click was outside the browse window then check if the user is selecting
  *-- any object from the control window
  llClick    = .F.
  PUSH KEY
  ON KEY
  lcObjName=lfGetObj(lnX,lnY)
  CLEAR TYPEAHEAD
  *-- if the use is selecting an object from the control window then make
  *-- this object the current object and press it to go to the validation function
  *-- of this object
  IF !EMPTY(lcObjName) AND !("DUMY"$UPPER(lcObjName)) .AND. LEFT(lcObjName,2)#'IB'
    _CUROBJ=OBJNUM(&lcObjName)
    CLEAR TYPEAHEAD
    IF _CUROBJ = OBJNUM(&lcObjName)  
      KEYBOARD "{ENTER}" PLAIN CLEAR
    ENDIF
  ELSE
    CLEAR TYPEAHEAD
  ENDIF
  POP KEY
ENDIF
*-- end of lfSelCurr.

*:***************************************************************************
*! Name      : LFGETOBJ
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : function to get the object name from its corrdinates
*!*************************************************************************
*: Passed parameters  : lnx  row corrdinate
*:                      lny  column corrdinate
*:*************************************************************************
*! Returns   :     object name that has the same corrdinate like the par.
*:*************************************************************************     
FUNCTION lfGetObj
PARAMETER lnX,lnY
PRIVATE lnCount
lcObj=''
* loop through the screen object array and check for the first object that
* it's cordinates is the passed conrrdinate to the function
FOR lnCount=1 TO ALEN(laScObj,1)
  IF BETWEEN(lnX,laScObj[lnCount,1],laScObj[lnCount,3]) AND BETWEEN(lnY,laScObj[lnCount,2],laScObj[lnCount,4])
    lcObj=laScObj[lnCount,5]
    EXIT
  ENDIF
ENDFOR
RETURN lcObj

*!*************************************************************
*! Name      : lpTab
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
*
PROCEDURE lptab

*-- End Of lpTab

*!*************************************************************
*! Name      : lpShiftTab
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpshifttab

*-- end of lpshifttab.

*:**************************************************************************
*:* Name        : lfwComp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2005
*:* Purpose     : When function for selecting company popupp
*:***************************************************************************
FUNCTION lfwComp
IF !llwComp
  llwComp = .T.
  =lfvSlctCmp()
ENDIF  
*-- end of lfwComp.

*:**************************************************************************
*:* Name        : lfvSlctCmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2005
*:* Purpose     : Select a company
*:***************************************************************************
FUNCTION lfvSlctCmp
IF TYPE('lcOldVal')='N' .AND. lnComp = lcOldVal
  RETURN
ENDIF  
IF GCUSER_LVL = 'O'
  lnComp = lcOldVal
ENDIF

SHOW GET ibFolder(1) ENABLE
SHOW GET ibFolder(2) DISABLE    
SHOW GET ibFolder(3) ENABLE

DIMENSION laData[4]
STORE 0   TO laOldData[1],laOldData[4]
STORE ' ' TO laOldData[2],laOldData[3]
=ACOPY(laOldData,laData)

llCanOpn = .T.                          && Allow to open files for the selected company
lcCompDir = ALLTRIM(laComp[lnComp,3])   && Set the correct company data directory
IF !FILE(ALLTRIM(laComp[lnComp,3])+'SETUPS.DBF')
  WAIT WINDOW NOWAIT 'Cannot locate the file SETUPS , company '+ALLTRIM(laComp[lnComp,1])
  RETURN 
ENDIF  

IF USED('ACCOD')
  USE IN ACCOD
ENDIF

laOldData = ''
lcCompDir = ALLTRIM(laComp[lnComp,3])
IF FILE(lcCompDir+'ACCOD.DBF')
  =lfOpn( 'ACCOD' , 'ACCSEGNO' , .F. , 'WITH CDX')
  IF llCanOpn 
    *- lcScFields = 'NACSSEGSZ,CACSMASK,CACSEGDES,NACSNOSEG'
    SELECT ACCOD
    SCATTER FIELDS &lcScFields TO laOldData
  
    *** Collect the no. and size of segments from the old mask
    lnNext = 0
    FOR lnCount  = 1 TO 6
      lcObjName  = "lnSeg"+STR(lnCount,1)
      lnCrurnt   = lnNext
      lnNext     = IIF(AT('-',laOldData[2],lnCount)=0,LEN(ALLTRIM(laOldData[2]))+1,;
                       AT('-',laOldData[2],lnCount))
      lnwidth    = MAX(0,lnNext-lnCrurnt-1) 
      laOldSeg[lnCount] = lnwidth
      &lcObjName = lnwidth
      SHOW GET &lcObjName
    ENDFOR
    
    *-Collects the GL-Accounts structure history 
    =lfUpChgHst()

    =lfRefresh(lcWinCh1)
  ENDIF   
ENDIF  

*-- end of lfvSlctCmp.

*:**************************************************************************
*:* Name        : lfUpChgHst
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Show changed history of GL account code structure in a list box
*-              : Triggered by pressing the "Values" button in the list box in the third tab
*- use the character Chr(179) = '³' as a separatore between columns 
*- the column contains the work 'Value' , on double click show a list of all added values for the selected segment
*:***************************************************************************
FUNCTION lfUpChgHst

PRIVATE lcIDX,lnI

lcCompDir = ALLTRIM(laComp[lnComp,3])

DIMENSION laOpenFls[1,lnOpnDm]
=lfOpn('GLSTRCHG','GLSTRCHG',.F.,'WITH CDX')
=lfOpn('GLSEGCHG','GLSEGCHG')
=lfOpn('GLVALCHG','GLVALCHG')
=lfOpn('GLSEGVAL','ACSSEGVAL',.F.,'WITH CDX')

IF !llCanOpn .OR. !USED('GLSTRCHG')
  RETURN 
ENDIF

SELECT GLSTRCHG
lcIDX = gfTempName()
INDEX ON DTOS(DADD_DATE)+CADD_TIME TO (gcWorkDir+lcIDX)
SET ORDER TO &lcIDX DESC
LOCATE

DIMENSION laChgHst[1,5]
laChgHst = ''
lnI = 0

SCAN
  lnI = lnI +1
  DIMENSION laChgHst[lnI,5]
  laChgHst[lnI,1] = PADC(DTOC(DADD_DATE),12)    + CHR(179) + ;
                    PRVACSMASK                  + CHR(179) + ;
                    NWACSMASK                  
  laChgHst[lnI,5] = GLSTRCHGID

  lnI = lnI +1
  DIMENSION laChgHst[lnI,5]
  laChgHst[lnI,1] = PADC(ALLTRIM(CADD_USER),12) + CHR(179) + ;
                    PRVACSSGDS                  + CHR(179) + ;
                    NWACSSGDS                  
  laChgHst[lnI,2] = PRVACTNOSG
  laChgHst[lnI,3] = NWACTNOSG 
  laChgHst[lnI,4] = NWACSSGDS
  laChgHst[lnI,5] = GLSTRCHGID

  lnI = lnI +1
  DIMENSION laChgHst[lnI,5]
  laChgHst[lnI,1] = SPACE(12)                   + CHR(179) + ;
                    SPACE(LEN(PRVACSSGDS))      + CHR(179) + ;
                    SPACE(LEN(NWACSSGDS))      
  laChgHst[lnI,5] = ''
ENDSCAN

lnI = lnI +1
DIMENSION laChgHst[lnI,5]
laChgHst[lnI,1] = REPLICATE(CHR(196),12)              + CHR(193) + ;
                  REPLICATE(CHR(196),LEN(PRVACSSGDS)) + CHR(193) + ;
                  REPLICATE(CHR(196),LEN(NWACSSGDS)+11) 
laChgHst[lnI,5] = ''
SHOW GET lnChgHst    ENABLE

SELECT GLSTRCHG
CLOSE INDEXES
ERASE (gcWorkDir+lcIDX+'.IDX')
SET ORDER TO TAG GLSTRCHG
*-- end of lfUpChgHst.

*:**************************************************************************
*:* Name        : lfwChgHst
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2005
*:* Purpose     : When function for the list of changes
*:***************************************************************************
FUNCTION lfwChgHst
PRIVATE lcStat,lnK
lcStat = IIF( 'VALUES' $ laChgHst[lnChgHst,1] , 'ENABLE' , 'DISABLE' )
SHOW GET pbValues &lcStat

PRIVATE lcCr
lcCr = CHR(13)
DIMENSION laStrHst[1]
laStrHst[1] = ''

IF !EMPTY( laChgHst[lnChgHst,5] )
  
  SELECT GLSTRCHG
  =SEEK(laChgHst[lnChgHst,5],'GLSTRCHG')
  
  =lfUpdStrAr('Changed applied by the user : ' + GLSTRCHG.CADD_USER)
  =lfUpdStrAr('Date : ' + DTOC(GLSTRCHG.DADD_DATE) + ',Time : ' + GLSTRCHG.CADD_TIME )
  
  IF PRVACTNOSG < NWACTNOSG
    =lfUpdStrAr('Number of segments increased : ' + STR(PRVACTNOSG,1) + ' ==> ' + STR(NWACTNOSG,1) )
  ENDIF
    
  =lfUpdStrAr('Mask changed : ' + PRVACSMASK + ' ==> ' + NWACSMASK )
  =lfUpdStrAr('Descriptions : ' + PRVACSSGDS + ' ==> ' + NWACSSGDS )
           
  SELECT GLSEGCHG
  =SEEK(laChgHst[lnChgHst,5],'GLSEGCHG')
  SCAN REST WHILE GLSTRCHGID+STR(NACSSEGNO,1) = laChgHst[lnChgHst,5]
    IF PRVACSSIZE < NWACSSIZE 
      =lfUpdStrAr('Width of Segment # ' + STR(NACSSEGNO,1) + ;
           IIF(PRVACSSIZE=0,' is set',' is increased from ' + STR(PRVACSSIZE,2)) + ' to ' + STR(NWACSSIZE,2))
    ENDIF
  ENDSCAN
  
  SELECT GLVALCHG
  =SEEK(laChgHst[lnChgHst,5],'GLVALCHG')
  SCAN REST WHILE GLSTRCHGID+STR(NACSSEGNO,1) = laChgHst[lnChgHst,5]
    IF NACSSEGNO=0
      =lfUpdStrAr('GL-Account :'+ ALLTRIM(PRVSEGVAL) + ' balances '+;
                  'transferred to Value' + ALLTRIM(NWSEGVAL) + ' balances')
    ELSE
      IF EMPTY(PRVSEGVAL)
        =lfUpdStrAr('New added value of Segment # '+STR(NACSSEGNO,1) + ' :' + ALLTRIM(NWSEGVAL))
      ELSE
        =lfUpdStrAr('Segment # '+STR(NACSSEGNO,1) + ' :'+ ALLTRIM(PRVSEGVAL) + ' transferred to ' + (NWSEGVAL))
      ENDIF
    ENDIF
  ENDSCAN
  
ENDIF
lnStrHst = 1
SHOW GET lnStrHst ENABLE
=lfRefresh(lcWinCh3)
*-- end of lfwChgHst.


*:***************************************************************************
*
*    FUNCTION lfUpdStrAr
*    TMI
*    11/29/2005   
*    Update the array laStrHst
*
*:***************************************************************************
FUNCTION lfUpdStrAr
PARAMETERS lcStr
PRIVATE lnK
lnK = ALEN(laStrHst)+1
DIMENSION laStrHst[lnK]
laStrHst[lnK] = lcStr

*-- end of lfUpdStrAr.

*-* *:**************************************************************************
*-* *:* Name        : lfvChgHst
*-* *:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*-* *:* Date        : 11/08/2005
*-* *:* Purpose     : The screen that will show the list of values of a seqment added 
*-* *:***************************************************************************
*-* FUNCTION lfvChgHst
*-* PRIVATE lnK,lnFrm,lnTo,lnCount,lnInc,lcDesc
*-* PRIVATE laArray1,laArray2,laArray3,laArray4,laArray5,laArray6,;
*-*         lnXPos1,lnXPos2,lnXPos3,lnXPos4,lnXPos5,lnXPos6,;
*-*         llList1,llList2,llList3,llList4,llList5,llList6
*-* PRIVATE laDes
*-* 
*-* DIMENSION     laArray1[1],laArray2[1],laArray3[1],laArray4[1],laArray5[1],laArray6[1]
*-* STORE  ' ' TO laArray1[1],laArray2[1],laArray3[1],laArray4[1],laArray5[1],laArray6[1]
*-* STORE 1 TO lnXPos1,lnXPos2,lnXPos3,lnXPos4,lnXPos5,lnXPos6
*-* STORE .F. TO llList1,llList2,llList3,llList4,llList5,llList6
*-* 
*-* IF 'VALUES' $ laChgHst[lnChgHst,1] 
*-*   IF laChgHst[lnChgHst,2] < laChgHst[lnChgHst,3]
*-*     DIMENSION laDes[1]
*-*     =gfSubStr(laChgHst[lnChgHst,4],@laDes,'-')
*-*     lnFrm = laChgHst[lnChgHst,2] + 1
*-*     lnTo  = laChgHst[lnChgHst,3]
*-*     lnInc = 0 
*-*     lcDesc = ''
*-*     FOR lnK = lnFrm TO lnTo
*-*       lcK = STR(lnK,1)
*-*       llList&lcK = .T.
*-*       lnXPos&lcK = 2.400 + lnInc*21.400
*-*       lnInc = lnInc + 1
*-*       lcDesc = lcDesc + ALLTRIM(laDes[lnK]) + ','
*-*       SELECT CSEGVALUE FROM GLSEGVAL WHERE CACSSEGNO = lcK INTO ARRAY laArray&lcK
*-*     ENDFOR
*-*     lcDesc = SUBSTR(lcDesc,1,LEN(lcDesc)-1)
*-*     WAIT WINDOW NOWAIT 'Showing all values added for the segment :'+lcDesc 
*-*     PUSH KEY
*-*     ON KEY
*-*     DO (gcScrDir + gcWinAppl + '\SMSGLST.SPR')    
*-*     POP KEY
*-*     WAIT CLEAR
*-*   ELSE
*-*     =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No new segmets added')
*-*   ENDIF
*-* ENDIF

*-- end of lfvChgHst.
 
*:**************************************************************************
*:* Name        : lfvAccept
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Confirm you selection
*:***************************************************************************
FUNCTION lfvAccept
PRIVATE lnResp

IF laComp[lnComp,2] = lcDummyCom
  _CUROBJ = OBJNUM(lnComp)
  RETURN
ENDIF

*- Check disk space
IF !lfChkDskSP()  
  RETURN
ENDIF

IF laComp[lnComp,2] <> lcDummyCom
  lcMsg = 'It is Strongly Recommended to take a backup of the company data befor changing the code structure or transferring GL-Account balances to another one.'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to '+;
                      'change the GL Account code structure/Transfer GL-Accounts or Segments of the selected company?')
  IF lnResp = 1
    =lfvComp()
  ELSE
    _CUROBJ = OBJNUM(lnComp)  
  ENDIF
ENDIF  

*-- end of lfvAccept.

*:**************************************************************************
*:* Name        : lfvComp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Valid function for selected company
*:***************************************************************************
FUNCTION lfvComp

*- Open needed files to operate on 
lcCompDir = ALLTRIM(laComp[lnComp,3])
llCanOpn = .T.
IF lfOpenFls()

  SELECT GLACCHAR
  GO TOP
  IF EOF()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Accounts entered in the chart of account, can not accept this company.')
    RETURN .F.
  ENDIF
  
  SELECT ACCOD
  GO TOP
  IF EOF('ACCOD') OR EMPTY(ACCOD.CACSMASK  )
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You can not select the company '+ALLTRIM(laComp[lnComp,1])+ ' , you must first add the account code structure.')
    RETURN
  ENDIF

  SELECT ACCOD
  GO TOP
  SCATTER FIELDS &lcScFields TO laData

  lnMn = laData[4]
  lnMx = laData[4] + INT( (24-laData[1])/4 )
  
  WAIT WINDOW NOWAIT 'Creating needed temp files..'
  =lfTmpFls()
  
  WAIT WINDOW NOWAIT 'Update new segment number spinner'
  =lfvData_4(.T.)

  laScrMode    = .F.
  laScrMode[2] = .T.

  SHOW GET lnComp    DISABLE
  SHOW GET pbConfirm DISABLE 
  SHOW GET ibDumy    DISABLE   
  SHOW GET ibDumy2   DISABLE   
  SHOW GET ibScr0    DISABLE   

  SHOW GET laData[4]   ENABLE
  SHOW GET pbDiscrip   ENABLE    
  SHOW GET pbView      ENABLE
  SHOW GET pbProceed   ENABLE
  SHOW GET pbCancel    ENABLE  
  SHOW GET ibFolder(2) ENABLE 
  
  WAIT WINDOW NOWAIT 'Refreshing the screen..'
  _CUROBJ = OBJNUM(laData[4])
  =lfRefresh()
  
  WAIT CLEAR
ELSE
  
  _CUROBJ = OBJNUM(lnComp)
    
ENDIF  

*-- end of lfvComp.

*:**************************************************************************
*:* Name        : lfwData_4
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2005
*:* Purpose     : When function for number of segments to set max and min linits
*:***************************************************************************
FUNCTION lfwData_4

*- Prevent the total to be more than 24 width
IF laData[1]>20 
  lnMx = laData[4]
ENDIF  


*-- end of lfwData_4.

*!**************************************************************************
*!
*!      Function: lfvData_4
*!      Date    : 11/08/2005
*!      Developer:TMI - TAREK MOHAMED IBRAHIM
*!      Purpose  : Valid fn. for changing the no. of segments
*!
*!**************************************************************************
* Valid of all segments :Calculat the new mask and description and show 
* the objects enable or disable
*
FUNCTION lfvData_4
PARAMETERS llNoOldVl   && Means : do not check old value
PRIVATE lnCount,lcI,lcSegValSt,lcDumy

IF !llNoOldVl  
  IF lcOldVal = laData[4]
    RETURN
  ENDIF 
ENDIF

*- Let the current active widnwo be lcWinCh1 for the @...GET to work correctly
ACTIVATE WINDOW (lcWinCh1)

lcDumy = ' '
IF !USED('ACCOD')
  =lfOpn( 'ACCOD' , 'ACCSEGNO',.F.,'WITH CDX')
ENDIF

FOR lnCount = 1 TO 6
  lcObjName  = "lnSeg"+STR(lnCount,1)
  &lcObjName = IIF(lnCount<=laData[4],IIF(EMPTY(&lcObjName),3,&lcObjName),0)
  lcObjStat  = IIF(lnCount<=laData[4],"ENABLE","DISABLE")
  laObjdisp[lnCount] =IIF(lnCount<=laData[4],.T.,.F.)
  SHOW GET (lcObjName) &lcObjStat

  lcI = STR(lnCount,1)

  *** Get Short and long description for all segment in array in first time only
  IF EMPTY(laAcDes[lnCount,1])
    IF lnCount<=laData[4]
      IF SEEK(STR(lnCount,1),'ACCOD')
        laAcDes[lnCount,1] = ACCOD.CACSSHDES
        laAcDes[lnCount,2] = ACCOD.CACSLGDES
      ENDIF  
    ELSE
      laAcDes[lnCount,1] = 'Sg'+lcI
      laAcDes[lnCount,2] = 'Default'+lcI
    ENDIF
  ENDIF
  
  *- Initial segments with zeros for new added segments
  IF BETWEEN(lnCount,laOldData[4]+1,laData[4])
    lcSegVal&lcI = PADL(ALLTRIM(lcSegVal&lcI),lnSeg&lcI,'0')
  ELSE
    lcSegVal&lcI = SPACE(15)
  ENDIF

  *- segment values variables status
  lcSegValSt = IIF( BETWEEN(lnCount,laOldData[4]+1,laData[4]) , 'ENABLE' ,'DISABLE' )
  SHOW GET lcSegVal&lcI &lcSegValSt

ENDFOR

*** Redifine the variable used to control the display of the child screen
lnBoxh = (laData[4] * 2) + 5
=lfTotalSiz()

*-- end of lfvData_4.

*!**************************************************************************
*!
*!      Function: lfTotalSiz
*!      Date    : 11/08/2005
*!      Developer:TMI - TAREK MOHAMED IBRAHIM
*!      Purpose  : Recalculate total segments width 
*!
*!**************************************************************************
*
FUNCTION lfTotalSiz
PARAMETERS lcSgno
PRIVATE lcSegDscSt,lcPrompt,lnI,lcI,lnSlct
lnSlct = SELECT()

laData[1]   = MAX(laData[4]-1,0)
laData[2]   = ''
laData[3]   = ''
FOR lnCount = 1 TO laData[4]
  lnVarName  = "lnSeg"+STR(lnCount,1)
  laData[1]  = laData[1] + &lnVarName 
  laData[2]  = laData[2] + IIF(EMPTY(laData[2]),'','-')+;  
               REPLICATE('#',&lnVarName)
  laData[3]  = laData[3] +IIF(lnCount=1,'','-')+;
               IIF(LEN(ALLTRIM(laAcDes[lnCount,1]))>&lnVarName,;
               LEFT(ALLTRIM(laAcDes[lnCount,1]),&lnVarName),;
               ALLTRIM(laAcDes[lnCount,1])+;
               SPACE(&lnVarName-LEN(ALLTRIM(laAcDes[lnCount,1]))))
ENDFOR 

SHOW GET laData[1]
SHOW GET laData[2]
SHOW GET laData[3]
SHOW GET laData[4]

FOR lnI = 1 TO 6
  lcI = STR(lnI,1)
  lcSegDscSt = IIF( lnI<=laData[4] , 'ENABLE' ,'DISABLE' )
  lcPrompt = IIF( lnI<=laData[4] , PADR(laAcDes[lnI,1],lnSeg&lcI) , '.....' )
  SHOW GET pbSgDsc&lcI,1 &lcSegDscSt PROMPT lcPrompt 
ENDFOR

IF !EMPTY(lcSgno)
  *- Update values in lcChngSeg if any.
  SELECT &lcChngSeg
  SET FILTER TO CACSSEGNO = lcSgno 
  GO TOP
  REPLACE CNEWSEGVAL WITH PADL(ALLTRIM(CNEWSEGVAL),lnSeg&lcSgno,'0') ;
          FOR !EMPTY(CNEWSEGVAL)
  SET FILTER TO 
  GO TOP
  
  IF BETWEEN(VAL(lcSgno),laOldData[4]+1,laData[4])
    lcSegVal&lcSgno = PADR(ALLTRIM(lcSegVal&lcSgno),lnSeg&lcSgno,'0')
    SHOW GET lcSegVal&lcSgno
  ENDIF

ENDIF  

=lfRefresh(lcWinCh1)

SELECT (lnSlct)
*-- end of lfTotalSiz.

*!**************************************************************************
*!
*!      Function: lfvNewVal
*!      Date    : 11/08/2005
*!      Developer:TMI - TAREK MOHAMED IBRAHIM
*!      Purpose  : 
*!**************************************************************************
*
FUNCTION lfvNewVal

IF lcOldVal <> EVAL(VARREAD())
  llChange = llChange OR .T.
  SHOW GET pbProceed ENABLE  
ENDIF

*!**************************************************************************
*!      Function: lfvDiscrip
*!      Date    : 11/08/2005
*!      Developer:TMI - TAREK MOHAMED IBRAHIM
*!      Purpose  : Open SMACDES.SPR screen to update the the laAcDes array that holds segments long and short descriptions
*!**************************************************************************
* Branch to segments descriptions screen
*
FUNCTION lfvDiscrip
PRIVATE laTmpArr
llCancel = .F.

=ACOPY(laAcDes,laTmpArr)
DO (gcScrDir + 'SM\SMACDES.SPR')
llChange = llChange OR !llCancel
IF llCancel
  =ACOPY(laTmpArr,laAcDes)
ELSE
  =lfTotalSiz()  
ENDIF

*!**************************************************************************
*!
*!      Function: lfSegDes
*!
*!**************************************************************************
*
* Function used in the picture of all short descriptions to define how manay
* Characters to be accepted in each field
*
FUNCTION lfSegDes
PARAMETERS lnWidth

RETURN REPLICATE("N",lnWidth)

*:**************************************************************************
*:* Name        : lfwSegval
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : When function for the segment value fields
*:***************************************************************************
FUNCTION lfwSegval
PARAMETERS lcK
=lfOldValue()
RETURN .T.

*- There is no concept of changing the format in fox2.6, but I faced here with the problem that 
*- if the segment is of width 4 for example and I need to change it to 6 , then I need to let the
*- segmqnt value field to accept 6 characters
*- To fix this situation , I let the field as it is with "when" funtion returns False , but it draws
*- on the screen get variables , a variable per character as the segment width

PRIVATE lcDumy,lnK,lcPict
lcDumy = ' '
lnK = VAL(lcK)

IF lnK > laOldData[4]  

  lcSegVal&lcK = PADR(lcSegVal&lcK , lnSeg&lcK)
  lcPict = REPLICATE('#',lnSeg&lcK)
  @ laYSgPos[lnK] , lnXSgPos GET lcSegVal&lcK PICTURE lcPict ;
    STYLE 'Q' ;
    FONT 'FOXFONT',9 ;
    COLOR  &gcObjColor

  
  *- To indicate the end of the enabled get
  @ laYSgPos[lnK] , lnXSgPos + 1.5*(lnSeg&lcK+1) GET lcDumy DISABLE    

  READ 

  IF !EMPTY(lcSegVal&lcK)
    lcSegVal&lcK = PADL(STRTRAN(lcSegVal&lcK,' '),lnSeg&lcK,'0')
    SHOW GET lcSegVal&lcK
  ENDIF
  @ laYSgPos[lnK] , lnXSgPos + 1.5*(lnSeg&lcK+1) GET lcDumy DISABLE    && repaint this character
  
ENDIF

RETURN .F.

*-- end of lfwSegval.

*:**************************************************************************
*:* Name        : lfvSegVal
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/26/2005
*:* Purpose     : Valid function for segment value fields
*:***************************************************************************
FUNCTION lfvSegVal
PARAMETERS lcSgno
IF lcSegVal&lcSgno <> lcOldVal
  lcSegVal&lcSgno = PADL( CHRTRAN(lcSegVal&lcSgno,' ','') , lnSeg&lcSgno , '0' )
ENDIF  
*-- end of lfvSegVal.
*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Open needed files and whom to operate on
*:***************************************************************************
FUNCTION lfOpenFls
PRIVATE lcFil,lcFlds
*- Note : no need to open files with tag since I will loop on all records and replace the 
*-        fields containing GL - accounts with the updated values

DIMENSION laOpenFls[1,lnOpnDm]
laOpenFls = ''

=lfOpn('ACCOD' , 'ACCSEGNO',.F.,'WITH CDX')
=lfOpn('GLSTRCHG','GLSTRCHG',.F.,'WITH CDX')
=lfOpn('GLSEGCHG','GLSEGCHG')
=lfOpn('GLVALCHG','GLVALCHG')
=lfOpn('CODES','')
=lfOpn('SETUPS','')
=lfOpn('GLSETUP','')
=lfOpn('GLTYPES','')

=gfOpenFile(gcSysHome+'SYDGLFIL','SYDGLFIL','SH')
SELECT SYDGLFIL
LOCATE
IF EOF()  && This is the first time the file is checked
  APPEND FROM SYDGLFIL.TXT DELIMITED
ENDIF
DO WHILE !EOF('SYDGLFIL')
  SELECT SYDGLFIL
  lcFil = SYDGLFIL.CFILE_NAM
  lcFlds = ''
  SCAN REST WHILE CFILE_NAM = lcFil
    lcFlds = lcFlds + CFLD_NAME + ','
  ENDSCAN
  lcFlds = LEFT(lcFlds,LEN(lcFlds)-1)
  =lfOpn(lcFil,'',lcFlds)  
  IF !llCanOpn
    EXIT
  ENDIF
ENDDO

=lfOpn('GLSEGVAL','ACSSEGVAL',.F.,'WITH CDX')

*B040141,1  TMI [Start] Get the excepted accounts from being changed
lcExcepted = GLSETUP.CSETEXMJ  + '|' + ; 
             GLSETUP.CSETRETMJ + '|' + ; 
             GLSETUP.CSETSUSMJ 
*B040141,1  TMI [End  ] 

RETURN llCanOpn
*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfOpn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Open a file
*:***************************************************************************
FUNCTION lfOpn
PARAMETERS lcFile,lcTag,lcAcFldLst,lcMacro
PRIVATE lnSlct,lcError,llDumy,lcMsg,lnI,lnLn
lnSlct = SELECT()

lcMacro = IIF(EMPTY(lcMacro) , '' , ALLTRIM(lcMacro) )

IF !llCanOpn
  RETURN llCanOpn
ENDIF

WAIT WINDOW NOWAIT 'Opening the file &lcFile ..'  
IF !EMPTY(lcFile) ;
  .AND. TYPE('lcFile')='C' ;
  .AND. FILE(lcCompDir+lcFile+'.DBF')
  
  lcFile = ALLTRIM(lcFile)
  lcTag = IIF(EMPTY(lcTag) , '' , lcTag)
  
  IF USED(lcFile)
    USE IN (lcFile)
    IF ASCAN(laOpenFls,lcFile) > 0
      lnLn = ASUBSCRIPT( laOpenFls , ASCAN(laOpenFls,lcFile) , 1 )
      =ADEL(laOpenFls,lnLn)
      IF ALEN(laOpenFls,1) > 1
        DIMENSION laOpenFls[ALEN(laOpenFls,1)-1,lnOpnDm]
      ENDIF
    ENDIF
  ENDIF
  
  lcError = ON('ERROR')
  llDumy = .F.
  ON ERROR llDumy = .T.  
  =gfOpenFile(lcCompDir+lcFile , lcTag , 'EX' )
  ON ERROR &lcError
  
  IF llDumy
    lcMsg = 'Some files are currently opened by another user or file is corrupted, can not continue.'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
    llCanOpn = .F.    
  ENDIF
  
  IF llCanOpn .AND. ASCAN(laOpenFls,ALIAS())=0
    lcAcFldLst = IIF(EMPTY(lcAcFldLst) , '' , lcAcFldLst)
    lnI = ALEN(laOpenFls,1)
    lnI = IIF( lnI = 1 .AND. EMPTY(laOpenFls[1,1]) , 1 , lnI + 1 )
    DIMENSION laOpenFls[lnI,lnOpnDm]
    laOpenFls[lnI,1] = ALIAS()
    laOpenFls[lnI,2] = lcAcFldLst
    laOpenFls[lnI,3] = lcTag
    laOpenFls[lnI,4] = lcMacro
    laOpenFls[lnI,5] = ( laOpenFls[lnI,1] $ 'GLACCHAR|GLSEGVAL|GLACBALS' )
  ENDIF
  
ENDIF
WAIT CLEAR
SELECT (lnSlct)
RETURN llCanOpn
*-- end of lfOpn.

*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Close Opened files
*:***************************************************************************
FUNCTION lfClose
PRIVATE lnK
FOR lnK = 1 TO ALEN(laOpenFls,1)
  IF !EMPTY(laOpenFls[lnK,1]) .AND. USED(laOpenFls[lnK,1])
    USE IN (laOpenFls[lnK,1])
  ENDIF
ENDFOR
*-- end of lfClose.

*:**************************************************************************
*:* Name        : lfTmpFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Create temp files needed among work
*:***************************************************************************
FUNCTION lfTmpFls
PRIVATE lnI,laStruArr

*- Create a temp file that store changed segments values, or with increased width 

SELECT GLSEGVAL
=AFIELDS(laStruArr)

lnI = ALEN(laStruArr,1)

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CNEWSEGVAL'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 15 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'NWSGSHDES'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 10 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'NWSGLNDES'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 40 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CTMPFLD'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 15 
laStruArr[lnI,4] = 0

CREATE TABLE (gcWorkDir+lcChngSeg) FROM ARRAY laStruArr
INDEX ON CACSSEGNO+CNEWSEGVAL TAG CNEWSEGVAL
INDEX ON CACSSEGNO+CSEGVALUE TAG &lcChngSeg
APPEND FROM (DBF('GLSEGVAL'))

*:***************************************************************************
*- Create temp table used to view Chart of accounts changes 
*:***************************************************************************
lnI = 0
lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CACCNSDES'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 15 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CPRVACCODE' 
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 24 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CNEWACCODE'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 24 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CMNLACCODE'  && Manually modified account code
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 24 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CBFRSEGCHG'  && when changing segmet there may be some lines are duplicated in GLACCHAR,
laStruArr[lnI,2] = 'C'           && save in this field the account code befor applying the segment change
laStruArr[lnI,3] = 24 
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CTMPACTFLD'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 24
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CTYPECODE'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 3
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'LADDED'
laStruArr[lnI,2] = 'L'
laStruArr[lnI,3] = 1
laStruArr[lnI,4] = 0

CREATE TABLE (gcWorkDir+lcViewAcct) FROM ARRAY laStruArr
INDEX ON CNEWACCODE TAG CNEWACCODE
INDEX ON CPRVACCODE TAG &lcViewAcct
SELECT GLACCHAR
SCAN
  SELECT &lcViewAcct
  APPEND BLANK
  REPLACE CACCNSDES  WITH GLACCHAR.CACCNSDES ; 
          CPRVACCODE WITH GLACCHAR.CACCTCODE ;
          CNEWACCODE WITH GLACCHAR.CACCTCODE ;
          CBFRSEGCHG WITH GLACCHAR.CACCTCODE ;
          CTYPECODE  WITH GLACCHAR.CTYPECODE 
ENDSCAN

*-- end of lfTmpFls.

*:**************************************************************************
*:* Name        : lfvProceed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Proceeding to update the chart of accounts with the new code structure
*:**************************************************************************
*- Asumption  : 
*- all updates will take place in a files copied to a temp folder 
*- after processing all files a message appears informing the user that he can commit all changes 
*- a button Commet will be enabled 
*- here we can 
*- 1- create a folder to take a backup copy of all aria files automatically 
*- 2- zap aria files 
*- 3- Append from updated files
*- 4- show a messsage that all files updated successfully
*- if ESC is pressed while processing , then stop and no data is updated
*- If cancel is pressed , remove all temp files created
NOTE : There must be a space on the disk equal the size of the Aria files
*:***************************************************************************

FUNCTION lfvProceed
PRIVATE llContinue,lnK,lcK,laProceed,lnPrc,lcError,llDumy

*- Check that all new added segments are filled
FOR lnK = laOldData[4]+1 TO laData[4]
  lcK = STR(lnK,1)
  IF EMPTY(lcSegVal&lcK)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to enter values to all new added segments.')
    RETURN .F.
  ENDIF
ENDFOR

*- Check that all Short descriptions are added 
FOR lnK = laOldData[4]+1 TO laData[4]
  IF EMPTY(laAcDes[lnK,1])
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to enter values for short descriptions to all new added segments.')
    RETURN .F.
  ENDIF
ENDFOR

*- Check that if there is any changes in case the number of segments not increased
IF laData[4] = laOldData[4]
  SELECT &lcChngSeg
  GO TOP
  LOCATE FOR !EMPTY(CNEWSEGVAL)
  llChngSeg = FOUND()
  
  SELECT &lcViewAcct
  GO TOP
  LOCATE FOR !EMPTY(CMNLACCODE)
  llViewAcct = FOUND()
  IF llChngSeg ;
    .OR. llViewAcct ;
    .OR. laData[1]>laOldData[1]
    
    *- Confirm proceeding
    IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to proceed?') = 2
      RETURN .F.
    ENDIF
  
  ELSE
      
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'To continue you should either Add a new segment,increase the width of a segment,transfer GL-Account to another or transfer a segment value to another.')
    RETURN .F.
    
  ENDIF

ELSE

  *- Confirm proceeding
  IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to proceed?') = 2
    RETURN .F.
  ENDIF

ENDIF

*- Update the temp file lcViewAcct
=lfUpdVewAc() 

PUSH KEY
ON KEY
*---------------------
DIMENSION laProceed[1]
laProceed[1] = laComp[lnComp,2]

llContinue = .T.
FOR lnPrc = 1 TO ALEN(laProceed,1)
  IF !EMPTY(laProceed[lnPrc]) .AND. llContinue
    
    lcComp = laProceed[lnPrc]
    
    PRIVATE lcComt,lcBkup,lnK
    PRIVATE lnFlNo,laFldArr,lcFile,llAborted,lnPos,lcCDX,lcTag
    
    *- First close all files needed to be updated
    =lfClose()
    
    lnPos = ASUBSCRIPT(laComp , ASCAN(laComp,'~'+lcComp+'~') , 1) 
    lcCompDir = ALLTRIM(laComp[lnPos,3])
    IF !lfOpenFls()
      POP KEY
      RETURN .F.
    ENDIF
    
    *- Make commit and backup folders
    lcComt = gcWorkDir + lcComp + SUBSTR(lcCommtFld,3)
    lcBkup = gcWorkDir + lcComp + SUBSTR(lcBckUpFld,3)
    WAIT WINDOW NOWAIT 'Creating temp. folders ..'

    lcError = ON('ERROR')
    llDumy = .F.
    ON ERROR llDumy = .T.
    !MD &lcComt
    !MD &lcBkup
    ON ERROR &lcError
    WAIT CLEAR
    
    lnComt = FCREATE(lcComt+'\TEMP.TXT')
    lnBkup = FCREATE(lcBkup+'\TEMP.TXT')
    IF lnComt<0 .OR. lnBkup<0  
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not create the commit and/or backup folder')
      POP KEY
      RETURN .F.
    ENDIF
    =FCLOSE(lnComt)
    =FCLOSE(lnBkup)
    ERASE (lcComt+'\TEMP.TXT')
    ERASE (lcBkup+'\TEMP.TXT')
        
    lcComt = lcComt + '\'  && Adding the back slach after createing the folder since it is not possible to create it with '\' at most right in fox2.6
    lcBkup = lcBkup + '\'
    
    *- Activate the second folder
    lnActFolder = 2
    =lfActFolder()
    =lfRefresh(lcFolder)
    
    *- Strat recording log data
    =lfUpdLog('Start processing for the company '+lcCompDir)
    
    =lfUpdLog('---------------------------------------------------------') 
    =lfUpdLog('Check if you should run rebalance')
    SELECT GLACCHAR
    lcTmpFil =gfTempName()    
    COPY TO (gcWorkDir+lcTmpFil)
    SELECT 0
    USE (gcWorkDir+lcTmpFil+'.DBF')
    SCAN
      =gfThermo( RECCOUNT(lcTmpFil) , RECNO(lcTmpFil) , 'Check if you should run rebalance','') 
      =lfUpdAcCod( lcTmpFil , 'CACCTCODE' ) 
    ENDSCAN
    DIMENSION laDuplct[1]
    laDuplct[1] = ''
    SELECT CACCTCODE , COUNT(*) AS nCnt;
       FROM (gcWorkDir+lcTmpFil+'.DBF') ;
       GROUP BY CACCTCODE HAVING nCnt > 1 INTO ARRAY laDuplct
    IF !EMPTY(laDuplct[1])
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
                  'Due to replacing some segments or GL-Accounts values ' +;
                  'with other existing values you MUST run the rebalance program after '+;
                  'committing to transfer the balances from old GL accounts to new ones.')
    ENDIF
    USE IN &lcTmpFil
    ERASE (gcWorkDir+lcTmpFil+'.DBF')
    =lfUpdLog('---------------------------------------------------------') 
    
    *- Record stuctre changes in log file
    =lfUpdLog('Increased number of segments by '+ALLTRIM(STR(laData[4]-laOldData[4])) )
    =lfUpdLog('Increased GL-Account code width by '+ALLTRIM(STR(laData[1]-laOldData[1])) )
    
    =lfUpdLog('A temp commit folder &lcComt is created')
    =lfUpdLog('A temp backup folder &lcBkup is created')
    
    *- Copy the opened files to these two folder 
    FOR lnK = 1 TO ALEN(laOpenFls,1)
      IF !EMPTY(laOpenFls[lnK,1]) .AND. USED(laOpenFls[lnK,1])
        SELECT (laOpenFls[lnK,1])
      
        lnRecNo = lnRecNo + 1
        =gfThermo( ALEN(laOpenFls,1) , lnK , '' ,"Copying the file : " + laOpenFls[lnK,1] )
        lcCDX = laOpenFls[lnK,4]
        COPY TO lcComt + laOpenFls[lnK,1] &lcCDX
        COPY TO lcBkup + laOpenFls[lnK,1]
        
        *- Reopen the file in the commit folder
        lcTag = IIF(!EMPTY(laOpenFls[lnK,3]) .AND. !EMPTY(laOpenFls[lnK,4]),'ORDER '+laOpenFls[lnK,3],'')
        USE (lcComt + laOpenFls[lnK,1]) &lcTag EXCLUSIVE
      ENDIF
    ENDFOR
    =lfUpdLog('Files copied to commit folder')
    =lfUpdLog('Files copied to backup folder')
      
    *------------------------------------------------*  
    *- Update files with new GL-Account code structure
    *------------------------------------------------*  
    SHOW GET pbCommit,1 ENABLE  PROMPT 'Press Esc key to abort'

    SELECT &lcViewAcct 
    SET ORDER TO TAG CNEWACCODE 
    
    *- Check if there is a   gl-account A, changed to gl-account B and the second has less records in 
    *- GLACBALS file , then add records for gl-account B to GLACBALS
    
    SELECT GLACBALS
    INDEX ON CACCTCODE+CFISFYEAR+CFSPPRDID TAG GLACBALS
    
    SELECT &lcViewAcct
    LOCATE
    
    WAIT WINDOW NOWAIT 'Checking tranferred GL-accounts.'
    SCAN FOR !EMPTY(CMNLACCODE) .OR. CBFRSEGCHG <> CNEWACCODE
      
      IF !EMPTY(CMNLACCODE)
        =lfUpdLog('Updating GLACBALS file, Account : '+ CMNLACCODE)  
      ENDIF
      
      *- Get the record for the "converted from" account 
      lcAcct = IIF( !EMPTY(CMNLACCODE) , &lcViewAcct..CNEWACCODE , &lcViewAcct..CBFRSEGCHG )

      *- Get the record for the "converted to" account
      lcCvToAc = IIF( !EMPTY(CMNLACCODE) , &lcViewAcct..CMNLACCODE , &lcViewAcct..CNEWACCODE )
      
      DIMENSION laAcct[1]
      laAcct = ''
      SELECT CACCTCODE , CFISFYEAR , CFSPPRDID ;
        FROM GLACBALS ;
        WHERE CACCTCODE = lcAcct ;
        INTO ARRAY laAcct
        
      *- Loop for the "converted to" , if there is a period that has no line in GLACBALS file, add it
      IF !EMPTY(laAcct[1])
        FOR lnK = 1 TO ALEN(laAcct,1)
          IF !SEEK( lcCvToAc + laAcct[lnK,2] + laAcct[lnK,3] , 'GLACBALS' )
            SELECT GLACBALS
            APPEND BLANK
            REPLACE CACCTCODE WITH lcCvToAc      ;
                    CFISFYEAR WITH laAcct[lnK,2] ;
                    CFSPPRDID WITH laAcct[lnK,3]
          ENDIF
        ENDFOR
      ENDIF
            
    ENDSCAN
    WAIT CLEAR
    
    llAborted = .F.
    FOR lnFlNo = 1 TO ALEN(laOpenFls,1)
      IF !EMPTY(laOpenFls[lnFlNo,2])
        
        DIMENSION laFldArr[1]
        laFldArr[1] = ''
        IF ',' $ laOpenFls[lnFlNo,2]
          =gfSubStr(laOpenFls[lnFlNo,2],@laFldArr,',')
        ELSE
          laFldArr[1] = laOpenFls[lnFlNo,2]  
        ENDIF  

        SELECT (laOpenFls[lnFlNo,1])
        SET ORDER TO 
        GO TOP
        SCAN
          
          IF LASTKEY() = 27
            IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you need to exit processing?') = 1 
              llAborted = .T.
              =gfThermo(1,1,'','')  && To prevent the error when pressing the ESC key while thermo is in work
              EXIT
            ENDIF
          ENDIF
          
          *- Adding thermometer counter    
          FOR lnK = 1 TO ALEN(laFldArr)
            lcMsg = ALLT(STR(RECNO(laOpenFls[lnFlNo,1]))) + '/' + ALLTRIM(STR(RECCOUNT(laOpenFls[lnFlNo,1])))
            =gfThermo( RECCOUNT(laOpenFls[lnFlNo,1]) , RECNO(laOpenFls[lnFlNo,1]) , 'Rebuilding GL-Account for '+laOpenFls[lnFlNo,1],lcMsg) 
            IF laOpenFls[lnFlNo,5]
              =lfUpdAcCod( laOpenFls[lnFlNo,1] , laFldArr[lnK] , .T. , .F. , .T. )
            ELSE
              =lfUpdAcCod( laOpenFls[lnFlNo,1] , laFldArr[lnK] )
            ENDIF

            IF llAborted
              =gfThermo(1,1,'','')  && To prevent the error when pressing the ESC key while thermo is in work
              EXIT
            ENDIF
          ENDFOR

          IF llAborted
            =gfThermo(1,1,'','')  && To prevent the error when pressing the ESC key while thermo is in work
            EXIT
          ENDIF
          
        ENDSCAN  
        IF llAborted
          =lfUpdLog('Aborting processing')
          EXIT
        ENDIF
        =lfUpdLog('File '+laOpenFls[lnFlNo,1]+' has been updated with new changes/GL-Accounts transfer.')
      ENDIF
    ENDFOR
    
    SELECT &lcViewAcct 
    SET ORDER TO TAG &lcViewAcct
    
    IF llAborted
      POP KEY
      SHOW GET pbCommit,1 DISABLE  PROMPT ''
      RETURN .F.
    ELSE
      *- Update GL-Account code structure ( not applicape for history company )
      =lfSvStrChg()
      
      SHOW GET pbProceed DISABLE
      lnActFolder = 2
      =lfRefresh()
      SHOW GET pbCommit,1 ENABLE PROMPT 'C  o  m  m  i  t'
    ENDIF    
    *:***************************************************************************X
    
  ENDIF

  =lfUpdLog('')

ENDFOR

IF !llAborted
  SELECT GLSEGVAL 
  SET ORDER TO ACSSEGVAL
  
  SELECT &lcViewAcct
  GO TOP
  SCAN FOR LADDED
    =gfThermo( RECCOUNT(lcViewAcct) , RECNO(lcViewAcct) , 'Adding new accounts to the GL-Chart of accounts.','Adding ' + &lcViewAcct..CNEWACCODE)   
    =lfUpdLog('Adding ' + &lcViewAcct..CNEWACCODE + ' to the GL-chart of accounts')
    lcSegval = SUBSTR(&lcViewAcct..CNEWACCODE,1,AT('-',&lcViewAcct..CNEWACCODE)-1)
    =SEEK('1'+lcSegval,'GLSEGVAL')
    SELECT GLACCHAR
    APPEND BLANK
    REPLACE CACCTCODE WITH &lcViewAcct..CNEWACCODE ;
            CACCNLDES WITH GLSEGVAL.CSEGLNDES ;
            CACCNSDES WITH GLSEGVAL.CSEGSHDES ; 
            CSEGACCAT WITH GLSEGVAL.CSEGACCAT ;
            CSEGACTIV WITH GLSEGVAL.CSEGACTIV ;
            CSEGALPOS WITH GLSEGVAL.CSEGALPOS ;
            CSEGAUTDS WITH GLSEGVAL.CSEGAUTDS ;
            CSEGCAFLO WITH GLSEGVAL.CSEGCAFLO ;
            CSEGCOCAC WITH GLSEGVAL.CSEGCOCAC ;
            CSEGRATIO WITH GLSEGVAL.CSEGRATIO ;
            CSEGTERM  WITH GLSEGVAL.CSEGTERM  ;
            CSTANDARD WITH GLSEGVAL.CSTANDARD ;
            CTYPECODE WITH GLSEGVAL.CTYPECODE ;
            NSEGCONPR WITH GLSEGVAL.NSEGCONPR ;
            NSEGSEQN  WITH GLSEGVAL.NSEGSEQN  
    =lfAudtFlds('','A')  
  ENDSCAN  
  
  =lfUpdLog('Proceed with the CODES file')
  SELECT CODES
  SCAN FOR ALLTRIM(CODES.CRLTD_NAM) $ lcCodesFlds
    =gfThermo( RECCOUNT('CODES') , RECNO('CODES') , 'Rebuilding GL-Account for Codes file','') 
    =lfUpdAcCod( 'CODES' , 'CRLTD_VLU' )
  ENDSCAN
  =gfThermo(1,1,'','')

  =lfUpdLog('Proceed with the SETUPS file')
  SELECT SETUPS
  SCAN FOR ALLTRIM(SETUPS.CFLD_NAME ) $ lcSetupsAc
    =gfThermo( RECCOUNT('SETUPS') , RECNO('SETUPS') , 'Rebuilding GL-Accounts for SETUPS file.','')
    =lfUpdAcCod( 'SETUPS' , 'MDATA_DEF' )
  ENDSCAN
  =gfThermo(1,1,'','')
  
  *- Update file type and ranges  
  =lfUpdLog('Proceed with the type and ranges file')
  SELECT GLTYPES
  GO TOP 
  REPLACE ALL CTYPLACNO WITH PADR(ALLTRIM(CTYPLACNO),lnSeg1,'0') ;
              CTYPUACNO WITH PADR(ALLTRIM(CTYPUACNO),lnSeg1,'0')  

  =lfUpdLog('Proceed with the GLSETUP file')
  SELECT GLSETUP
  GO TOP
  =lfUpdAcCod( 'GLSETUP' , 'CSETEXMJ'  , .F. , .T. )
  =lfUpdAcCod( 'GLSETUP' , 'CSETRETMJ' , .F. , .T. )
  =lfUpdAcCod( 'GLSETUP' , 'CSETSUSMJ' , .F. , .T. )
ENDIF  

POP KEY

IF !llContinue
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Process aborted, no files updated.')
ELSE  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Files updated successfully, to commit changes you have to clikc the commit button.')
ENDIF

llProceed = llContinue

IF llContinue
  laScrMode    = .F.
  laScrMode[3] = .T.
  llChange = .T.
  SHOW GET lnLogStr 
  =lfRefresh(lcWinCh2)
ENDIF

*-- end of lfvProceed.

*:**************************************************************************
*:* Name        : lfUpdVewAc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2005
*:* Purpose     : Update the temp file lcViewAcct
*:***************************************************************************
FUNCTION lfUpdVewAc
PRIVATE lnSlct,lcSvOrd
lnSlct = SELECT()

SELECT &lcViewAcct
lcSvOrd = ORDER(lcViewAcct)
SET ORDER TO 
LOCATE

SCAN
  *- Updating the temp chart of accounts
  =gfThermo( RECCOUNT(lcViewAcct) , RECNO(lcViewAcct) , '' ,'Wait while building the modified temp. Chart of Accounts.') 
  =lfUpdAcCod( lcViewAcct , 'CNEWACCODE' , .T. )
  =lfUpdAcCod( lcViewAcct , 'CMNLACCODE' , .T. )
  =lfUpdAcCod( lcViewAcct , 'CBFRSEGCHG' , .T. , .F. , .T. )
  IF CMNLACCODE = CNEWACCODE
    REPLACE CMNLACCODE WITH ''
  ENDIF
ENDSCAN
=gfThermo( 1 , 1 , '' , '' )

SELECT &lcViewAcct
SET ORDER TO &lcSvOrd

SELECT (lnSlct)
*-- end of lfUpdVewAc.

*:**************************************************************************
*:* Name        : lfUpdAcCod
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Update segments
*:*             <<* This function is the core of this program *>>
*:***************************************************************************
FUNCTION lfUpdAcCod
PARAMETERS lcAlias,lcField,llTmpUpd,llOneSgOnly,llBfrSgChg
PRIVATE lnSlct,lcNwCode,lnFrm,lnI,lcI,llActualUpd,lnUperLmt
lnSlct = SELECT()

SELECT &lcAlias
*- if the field is empty , nothing to do
IF EMPTY(&lcAlias..&lcField)
  SELECT (lnSlct)
  RETURN
ENDIF  

*- llActualUpd :: This is an actual update
llActualUpd = !llTmpUpd

lnUperLmt = IIF( llOneSgOnly , 1 , laData[4] )

SELECT &lcAlias
lcNwCode = ''
lnFrm = 1
FOR lnI = 1 TO lnUperLmt
  lcI = STR(lnI,1)
  IF lnI <= laOldData[4]
    *B040141,1  TMI [Start] change the non-zero segment to a zero one
    IF lnI > 1 .AND. PADR(SUBSTR(lcNwCode,1,laOldSeg[1]),15) $ lcExcepted 
      lcGetSeg = lfChngSeg(lcI , SUBSTR( ALLTRIM(&lcField) , lnFrm , laOldSeg[lnI]) , llBfrSgChg )
      lcNwCode = lcNwCode + lfGtMajAc( lcGetSeg )
    ELSE
      *B040141,1  TMI [End  ] 
      
      lcNwCode = lcNwCode + lfChngSeg(lcI , SUBSTR( ALLTRIM(&lcField) , lnFrm , laOldSeg[lnI]) , llBfrSgChg )
      
      *B040141,1  TMI [Start] Close the new added if statement
    ENDIF
    *B040141,1  TMI [End  ] 
    
    lnFrm = AT('-' , &lcField , lnI ) + 1
  ELSE
    *B040141,1  TMI [Start] if this GL-account value is one of the excepted and is a main segment then add zeros to the new 
    *                       added segments
    lcPrvSgVl = STRTRAN( SUBSTR(lcNwCode , AT('-',lcNwCode)+1 , AT('-',lcNwCode,laOldData[4]) - AT('-',lcNwCode) ) , '-' , '' )
    IF PADR(SUBSTR(lcNwCode,1,laOldSeg[1]),15) $ lcExcepted .AND. VAL(lcPrvSgVl) = 0
      lcNwCode = lcNwCode + REPLICATE('0',LEN(lcSegVal&lcI)) + '-'
    ELSE
      *B040141,1  TMI [End  ] 
      
      lcNwCode = lcNwCode + lcSegVal&lcI + '-'
      
      *B040141,1  TMI [Start] Close the new added if statement
    ENDIF
    *B040141,1  TMI [End  ] 
  ENDIF
ENDFOR
lcNwCode = PADR(SUBSTR(lcNwCode,1,LEN(lcNwCode)-1),24)
IF llActualUpd
  IF SEEK(lcNwCode,lcViewAcct) .AND. !EMPTY(&lcViewAcct..CMNLACCODE)
    lcNwCode = &lcViewAcct..CMNLACCODE
  ENDIF
ENDIF
REPLACE &lcField WITH lcNwCode

SELECT (lnSlct)  
*-- end of lfUpdAcCod.
   
*:**************************************************************************
*:* Name        : lfSvStrChg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Save structure changes to the files GLSTRCHG,GLSEGCHG,GLVALCHG
*:***************************************************************************
FUNCTION lfSvStrChg
PRIVATE lnK,lcK,lcStrSeq,lnFrm

=lfUpdLog(' Saving structure ..')
*- Save old structure
SELECT ACCOD
GO TOP

lcStrSeq = gfsequence('GLSTRCHGID',laComp[lnComp,2])
SELECT GLSTRCHG   
APPEND BLANK
REPLACE GLSTRCHGID WITH lcStrSeq ;
        CCOMP_ID   WITH laComp[lnComp,2] ;
        PRVACTNOSG WITH laOldData[4] ;
        PRVACTSGSZ WITH laOldData[1] ;
        PRVACSMASK WITH laOldData[2] ;
        PRVACSSGDS WITH laOldData[3] ;
        NWACTNOSG  WITH laData[4] ;
        NWACTSGSZ  WITH laData[1] ;
        NWACSMASK  WITH laData[2] ;
        NWACSSGDS  WITH laData[3] ;
        COMMITTED  WITH .T.
=lfAudtFlds('','A')

SELECT GLSEGCHG
FOR lnK =1 TO laData[4]
  lcK = STR(lnK,1)
  =SEEK(lcK,'ACCOD')
  APPEND BLANK
  REPLACE GLSTRCHGID WITH lcStrSeq ;
          NACSSEGNO  WITH lnK ;
          PRVACSSIZE WITH ACCOD.NACSSIZE ;
          PRVACSLGDS WITH ACCOD.CACSLGDES ;
          PRVACSSHDS WITH ACCOD.CACSSHDES ;
          NWACSSIZE  WITH lnSeg&lcK        ;  
          NWACSLGDS  WITH laAcDes[lnK,2] ;
          NWACSSHDS  WITH laAcDes[lnK,1]
  =lfAudtFlds()
ENDFOR  
 
SELECT ACCOD
GO TOP
LOCATE FOR NACSSEGNO = 0
REPLACE CACSEGDES WITH laData[3] ;
        CACSMASK  WITH laData[2] ;
        NACSNOSEG WITH laData[4] ;
        NACSSEGSZ WITH laData[1]
=lfAudtFlds('','E')

lnFrm = 1 
FOR lnK = 1 TO laData[4]
  lcK = STR(lnK,1)
  SELECT ACCOD
  GO TOP
  LOCATE FOR NACSSEGNO = lnK
  IF !FOUND()
    APPEND BLANK
  ENDIF
  REPLACE CACSLGDES  WITH laAcDes[lnK,2] ;
          CACSSHDES  WITH laAcDes[lnK,1] ;
          NACSSEGNO  WITH lnK       ;
          NACSSIZE   WITH LEN(SUBSTR(laData[2],lnFrm,lnSeg&lcK))
  =lfAudtFlds('','E')
  lnFrm = AT('-',laData[2],lnK)+1
ENDFOR

*- Update the GLVALCHG file
SELECT &lcChngSeg 
LOCATE
SCAN FOR !EMPTY(CNEWSEGVAL)
  SELECT GLVALCHG
  APPEND BLANK
  REPLACE GLSTRCHGID WITH lcStrSeq ;
          NACSSEGNO  WITH VAL(&lcChngSeg..CACSSEGNO) ;
          PRVSEGVAL  WITH &lcChngSeg..CSEGVALUE ;
          PRVSGSHDES WITH &lcChngSeg..CSEGSHDES ;
          PRVSGLNDES WITH &lcChngSeg..CSEGLNDES ;
          NWSEGVAL   WITH &lcChngSeg..CNEWSEGVAL;
          NWSGSHDES  WITH &lcChngSeg..NWSGSHDES ;
          NWSGLNDES  WITH &lcChngSeg..NWSGLNDES
  =lfAudtFlds()
ENDSCAN

*- Update the GLVALCHG file with changed account
SELECT &lcViewAcct
LOCATE
SCAN FOR !EMPTY(CMNLACCODE)
  SELECT GLVALCHG
  APPEND BLANK
  REPLACE GLSTRCHGID WITH lcStrSeq ;
          NACSSEGNO  WITH 0 ;
          PRVSEGVAL  WITH &lcViewAcct..CNEWACCODE ;  
          PRVSGSHDES WITH &lcViewAcct..CACCNSDES ;
          PRVSGLNDES WITH '' ;
          NWSEGVAL   WITH &lcViewAcct..CMNLACCODE;
          NWSGSHDES  WITH &lcViewAcct..CACCNSDES ;
          NWSGLNDES  WITH ''
  =lfAudtFlds()
ENDSCAN

*- Add new segments to the file GLSEGVAL , GLVALCHG
FOR lnK = 1 TO 6
  lcK = STR(lnK,1)
  IF !EMPTY(lcSegVal&lcK)

    SELECT GLVALCHG
    APPEND BLANK
    REPLACE GLSTRCHGID WITH lcStrSeq ;
            NACSSEGNO  WITH lnK ;
            NWSEGVAL   WITH lcSegVal&lcK 
    =lfAudtFlds()

    SELECT GLSEGVAL
    APPEND BLANK
    REPLACE CACSSEGNO WITH lcK ;
            CSEGVALUE WITH lcSegVal&lcK   ;
            CSEGSHDES WITH laAcDes[lnK,1] ;
            CSEGLNDES WITH laAcDes[lnK,2] 
    =lfAudtFlds()
    
    *B040141,1  TMI [Start] Add 000-zeros segment to meet the suspense account and return earnings account
    IF lcSegVal&lcK <> REPLICATE('0',lnSeg&lcK) 
      SELECT GLSEGVAL
      APPEND BLANK
      REPLACE CACSSEGNO WITH lcK ;
              CSEGVALUE WITH REPLICATE('0',lnSeg&lcK) ; 
              CSEGSHDES WITH '000-SEGMENT' ;
              CSEGLNDES WITH '000-SEGMENT'
      =lfAudtFlds()
    ENDIF
    *B040141,1  TMI [End  ] 
  
  ENDIF
ENDFOR 

=lfUpdLog(' Updating segments widthes in GLSEGVAL')
SELECT GLSEGVAL
GO TOP
SCAN   
  lcK = GLSEGVAL.CACSSEGNO 
  *- Add the zeros to the right as per Usama
  REPLACE CSEGVALUE WITH PADR(ALLTRIM(CSEGVALUE),lnSeg&lcK,'0')
ENDSCAN

*-- end of lfSvStrChg.

*:**************************************************************************
*:* Name        : lfvCommit
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Commit updated data
*:***************************************************************************
FUNCTION lfvCommit
PRIVATE lnContinue,lnHndl,laCommit,lnCommit,lcBkup

lcBkup = gcWorkDir + laComp[lnComp,2] + SUBSTR(lcBckUpFld,3) + '\' 

*- Confirm Commitment
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'You are going to update your real data, '+;
  'note that a backup copy of all changed data files are located in the folder &lcBkup , '+;
  'are you sure you want to continue?') = 2
  RETURN .F.
ENDIF
 

lnHndl = 0
IF !llContComt
  lnHndl = FCREATE(FULLPATH('')+'GLCHNGR.DAT')
  IF lnHndl < 0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not create the commit log file GLCHNGR.DAT')
    RETURN .F.
  ENDIF  

  *- If the system crashed while commiting , use the file GLCHNGR.DAT to continue the commit process
  =FPUTS(lnHndl,laComp[lnComp,2]) 
  =FPUTS(lnHndl,lcBckUpFld)
  
ENDIF  

DIMENSION laCommit[1]
laCommit[1] = laComp[lnComp,2]
lnContinue = .T.

*:*******************
FOR lnCommit = 1 TO ALEN(laCommit,1)
  
  IF !EMPTY(laCommit[lnCommit]) .AND. lnContinue
    lcComp = laCommit[lnCommit]

    =lfUpdLog("Start committing the files of the company " + laCommit[lnCommit] )
    
    PRIVATE lnK,lnPos,lnCommited,lcComt
    
    lnContinue = .F.
    lcComt = gcWorkDir + lcComp + SUBSTR(lcCommtFld,3) + '\'
    
    *- Close Current files opened in the Commit folder
    =lfClose()
    
    lnPos = ASUBSCRIPT(laComp , ASCAN(laComp,'~'+lcComp+'~') , 1) 
    lcCompDir = ALLTRIM(laComp[lnPos,3])
    IF lfOpenFls()
      FOR lnK = 1 TO ALEN(laOpenFls,1)
        =gfThermo( ALEN(laOpenFls,1) , lnK , '' ,"Committing the file : " + DBF(laOpenFls[lnK,1]) ) 
        
        SELECT (laOpenFls[lnK,1])
        ZAP
        APPEND FROM (lcComt+laOpenFls[lnK,1])
    
        =lfUpdLog("Committing the file : " + DBF(laOpenFls[lnK,1]) )
        
      ENDFOR
      lnContinue = .T.
    ENDIF  
       
  ENDIF
  
ENDFOR  

IF lnContinue 
  =FCLOSE(lnHndl)
  ERASE (FULLPATH('')+'GLCHNGR.DAT')

  laScrMode    = .F.
  laScrMode[4] = .T.
  llChange     = .F. 
  SHOW GET pbCancel,1 PROMPT 'Close'
  SHOW GET pbCommit  DISABLE
  
  *- Go to folder 3
  lnActFolder = 3
  =lfActFolder()
  =lfRefresh(lcFolder)
  
  *- Update the change history
  =lfUpChgHst()
  
  =lfRefresh()
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'All files commited successfully.')
  
ENDIF

*-- end of lfvCommit.

*:**************************************************************************
*:* Name        : lfUpdLog
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Update Log file
*:***************************************************************************
FUNCTION lfUpdLog
PARAMETERS lcMsg
PRIVATE lnSlct
lnSlct = SELECT()

lcMsg = IIF(EMPTY(lcMsg) , '' , lcMsg )
lcMsg = ALLTRIM( CHRTRAN(lcMsg,'.'+CHR(10)+CHR(13),'') )

SELECT LOGFILE
APPEND BLANK
REPLACE mLogStr WITH lcMsg
lnLogStr = RECCOUNT('LOGFILE')
SHOW GET lnLogStr
=lfRefresh(lcWinCh2)

SELECT (lnSlct)
*-- end of lfUpdLog.

*:**************************************************************************
*:* Name        : lfChngSeg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Change the seqment width or contents
*:***************************************************************************
FUNCTION lfChngSeg
PARAMETERS lcI , lcOldSeg , llBfrSgChg
PRIVATE lcRetSeg
lcRetSeg = ''

*-* IF llBfrSgChg 
*-* 
*-*   lcRetSeg = PADR(lcOldSeg,lnSeg&lcI,'0') + '-'
*-*   
*-* 
*-* ELSE
*-* 
*-*   IF SEEK(lcI + lcOldSeg,lcChngSeg) .AND. !EMPTY(&lcChngSeg..CNEWSEGVAL)
*-*     lcOldSeg = ALLTRIM(&lcChngSeg..CNEWSEGVAL) 
*-*   ENDIF
*-*   lcRetSeg = PADR(lcOldSeg,lnSeg&lcI,'0') + '-'
*-* 
*-* ENDIF

IF SEEK(lcI + lcOldSeg,lcChngSeg) .AND. !EMPTY(&lcChngSeg..CNEWSEGVAL)
  IF llBfrSgChg
    
    *- this means that "segment change" will not applied to GLACCHAR , GLACBALS
    
  ELSE
  
    lcOldSeg = ALLTRIM(&lcChngSeg..CNEWSEGVAL) 
    
  ENDIF
ENDIF
lcRetSeg = PADR(lcOldSeg,lnSeg&lcI,'0') + '-'

RETURN lcRetSeg
*-- end of lfChngSeg.

*:**************************************************************************
*:* Name        : lfvChgnSg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Open the "Replace of existing segment values" screen
*:***************************************************************************
FUNCTION lfvChgnSg
PARAMETERS lcSgno
PRIVATE lcDet_Ttl,llOk,lnMarker,lcPict

IF VAL(lcSgno) > laOldData[4]
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'For new added segments use only the new added value.')
  RETURN
ENDIF

lnMarker = 1
lcPict = REPLICATE('#',lnSeg&lcSgno)
lcTmpBrw  = "nRec=IIF(RECNO()=lnMarker,'>',' '):H=' ':W=.F. ,"+;
            "CSEGVALUE :H='Original Value' :W=.F.,"+;
            "CNEWSEGVAL:H='Transferred To' :P=[&lcPict] :V=lfvRplSgm(lcSgno) "
              
lcOkStat = 'DISABLE'              
SELECT &lcChngSeg
SET FILTER TO CACSSEGNO = lcSgno
LOCATE
REPLACE ALL CTMPFLD    WITH CNEWSEGVAL

LOCATE
llOk = .T.
lcDet_Ttl = 'Account Segment [' + ALLT(laAcDes[VAL(lcSgno),1]) + ']'

PUSH KEY
ON KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
ON KEY LABEL ESC llDummy = .T.
DO (gcScrDir+'SM\SMCHGSG.SPX')
POP KEY                

*- redefine Esc key
ON KEY LABEL ESC DO lfvCancel

IF llOk
  *- 
ELSE 
  SELECT &lcChngSeg
  GO TOP
  WAIT WINDOW NOWAIT 'Restoring values .. '
  REPLACE ALL CNEWSEGVAL WITH CTMPFLD
ENDIF
WAIT CLEAR

SELECT &lcChngSeg
SET FILTER TO
LOCATE

*-- end of lfvChgnSg.

*:**************************************************************************
*:* Name        : lfvSgChgOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2005
*:* Purpose     : Ok valid function for chagne segment ok button
*:***************************************************************************
FUNCTION lfvSgChgOk
*- Befor accepting transferred segments values check that the resulting GL-Accounts be existing in 
*- the chart of accounts, if at least one not found show a message that one or more accounts not 
*- found in the chart of accounts do you want to add them automatically , if yes add them to the 
*- lcViewAcct file to add them laster when saving data , if no do not do any thing

PRIVATE lnSlct,llOK,laNewAcct,lnK,lcTitle,lnRecno,lcSvOrd
lnSlct =SELECT()
llOK = .T.

lnRecno = RECNO(lcChngSeg) 

*- Update the temp file lcNewAcct with the changed segments
SELECT CNEWACCODE,CBFRSEGCHG ;
  FROM &lcViewAcct ;
  INTO TABLE (gcWorkDir+lcNewAcct)
SELECT &lcNewAcct
LOCATE 
SCAN
  *- Updating the temp chart of accounts
  =gfThermo( RECCOUNT(lcNewAcct) , RECNO(lcNewAcct) , '' ,'Wait while building the modified temp. Chart of Accounts.') 
  =lfUpdAcCod( lcNewAcct , 'CNEWACCODE' , .T. )
  =lfUpdAcCod( lcNewAcct , 'CBFRSEGCHG' , .T. , .F. , .T. )
ENDSCAN
=gfThermo( 1 , 1 , '' , '' )

DIMENSION laNewAcct[1]
laNewAcct = ''
SELECT CNEWACCODE ,'','',CBFRSEGCHG ; 
   FROM &lcNewAcct ;
   WHERE CNEWACCODE NOT IN ;
         ( SELECT DISTINCT CBFRSEGCHG FROM &lcNewAcct ) ;
   INTO ARRAY laNewAcct 
IF !EMPTY(laNewAcct[1])
  llOK = .F.
  IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'One or more accounts resulting from segments '+;
   'trasfer are not found in the GL-Chart of accounts, do you want to accept segment transfer '+;
   'and add these GL-Account automatically ?') = 1

    SELECT GLSEGVAL
    SET ORDER TO ACSSEGVAL
    
    SELECT &lcViewAcct
    GO TOP
    DELETE FOR LADDED
    PACK
    
    FOR lnK = 1 TO ALEN(laNewAcct,1)
      lcSegval = SUBSTR(laNewAcct[lnK,1],1,AT('-',laNewAcct[lnK,1])-1)  && Get major segment 
      =SEEK('1'+lcSegval,'GLSEGVAL')
      laNewAcct[lnK,3] = laNewAcct[lnK,1]
      laNewAcct[lnK,1] = ALLTRIM(laNewAcct[lnK,1]) + ' : ' + ALLTRIM(GLSEGVAL.CSEGLNDES) + ' (in place of '+ALLT(laNewAcct[lnK,4])+')'
      laNewAcct[lnK,2] = lcSegval
    ENDFOR
    
    lcTitle = 'New Added Accounts confirmation screen'
    llOk = .F.
    PUSH KEY
    ON KEY LABEL ESC llDumy = .T.
    DO (gcScrDir + gcWinAppl + '\SMNWACCT.SPR')
    POP KEY
    
    IF llOk
    
      SELECT &lcViewAcct
      lcSvOrd = ORDER()
      SET ORDER TO CNEWACCODE 
      GO TOP
      
      FOR lnK = 1 TO ALEN(laNewAcct,1)
        =SEEK(lcSgno+laNewAcct[lnK,2],'GLSEGVAL')
        =gfThermo( ALEN(laNewAcct,1) , lnK , 'Adding new GL-accounts to temp file',SUBSTR(laNewAcct[lnK,1],1,gnTherWidth) ) 
        IF !SEEK(laNewAcct[lnK,3],lcViewAcct)
          SELECT &lcViewAcct        
          APPEND BLANK
          REPLACE CACCNSDES  WITH SUBSTR(laNewAcct[lnK,1],AT(':',laNewAcct[lnK,1])+1)  ;
                  CPRVACCODE WITH laNewAcct[lnK,3] ;
                  CNEWACCODE WITH laNewAcct[lnK,3] ;
                  CBFRSEGCHG WITH laNewAcct[lnK,3] ;
                  LADDED     WITH .T.
        ENDIF
      ENDFOR
      
      SELECT &lcViewAcct
      SET ORDER TO &lcSvOrd 
      
    ENDIF

  ENDIF

ELSE  

  IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to accept changes?')=1

    llOk = .T.

    SELECT &lcViewAcct
    lcSvOrd = ORDER()
    SET ORDER TO 
    GO TOP
 
    lnSgPos = IIF(lcSgno='1', 1 , AT('-',laData[3],VAL(lcSgno)-1) + 1 )
   
    DIMENSION laRmvSgm[1]
    laRmvSgm = ''
    SELECT DISTINCT CTMPFLD ;
      FROM &lcChngSeg ;
      WHERE !EMPTY(&lcChngSeg..CTMPFLD) .AND. EMPTY(&lcChngSeg..CNEWSEGVAL) ;
            AND CTMPFLD NOT IN (SELECT DISTINCT CNEWSEGVAL FROM &lcChngSeg) ;
      INTO ARRAY laRmvSgm
    IF !EMPTY(laRmvSgm[1])
      FOR lnK = 1 TO ALEN(laRmvSgm,1)
        SELECT &lcViewAcct
        GO TOP
        SCAN FOR LADDED .AND. SUBSTR(&lcViewAcct..CNEWACCODE,lnSgPos,lnSeg&lcSgno) = ;
                                           SUBSTR(laRmvSgm[lnK,1],1,lnSeg&lcSgno) 
          WAIT WINDOW NOWAIT 'Removing account :' + CNEWACCODE
          DELETE
        ENDSCAN
      ENDFOR
    ENDIF
    
    WAIT CLEAR
    
    SELECT &lcViewAcct
    SET ORDER TO &lcSvOrd 
    PACK

  ELSE
    llOk = .F.
  ENDIF  
ENDIF

IF llOk
  
  CLEAR READ
  
ELSE  

  SELECT &lcChngSeg
  GOTO (lnRecno)
    
ENDIF  

SELECT (lnSlct) 
*-- end of lfvSgChgOk.

*:**************************************************************************
*:* Name        : lfvSgChgCn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2005
*:* Purpose     : Cancel valid function for chagne segment ok button
*:***************************************************************************
FUNCTION lfvSgChgCn

IF lcOkStat = 'ENABLE' .AND. ;
  gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you ignore Transferred values?') <> 1
  RETURN
ENDIF  

llOk = .F.
CLEAR READ  

 
*-- end of lfvSgChgCn.

*:**************************************************************************
*:* Name        : lfBrowLine
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Browsing temp file
*:***************************************************************************
FUNCTION lfBrowLine
PARAMETERS lcBrFlds

BROWSE FIELDS &lcTmpBrw 	  ;
			  NOAPPEND	      ;
			  NOCLEAR     	  ;
			  NODELETE        ;
			  NOMENU      	  ;
			  NOWAIT      	  ;
			  SAVE            ;
			  WHEN lfwBrowse();
              TITLE lcDet_Ttl ;
              WINDOW (lcWin2) IN WINDOW (lcWin0)
*-- end of lfBrowLine.

*:**************************************************************************
*:* Name        : lfWbrowse
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : When function for browse 
*:***************************************************************************
FUNCTION lfWbrowse
lnMarker = RECNO()
SHOW WINDOW (lcDet_Ttl) REFRESH SAME

*:**************************************************************************
*:* Name        : lfvRplSgm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Valid function for the new value to replace an existing one in glsegval file
*:***************************************************************************
FUNCTION lfvRplSgm
PARAMETERS lcSegno
PRIVATE lnRecno,lcSeg,lnSgCount,lnSegno

lnSegno = VAL(lcSegno)

lcOrgSeg= CSEGVALUE
lcSeg   = CNEWSEGVAL
lnRecno = RECNO()

IF !EMPTY(lcSeg)
  
  LOCATE FOR CNEWSEGVAL = lcOrgSeg
  IF FOUND()
    
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not transfer this segment, some segments are transferred to it.')
    GOTO (lnRecno)
    REPLACE CNEWSEGVAL WITH CTMPFLD
    
  ELSE
  
    GOTO (lnRecno)
    IF CNEWSEGVAL = CSEGVALUE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not accept the same value.')
      REPLACE CNEWSEGVAL WITH ''
    ELSE
      GO TOP
      LOCATE FOR CSEGVALUE = lcSeg
      IF !FOUND()
        PRIVATE lcBrFields
        DIMENSION latemp[2]
        latemp = ''
        lcBrFields = "CSEGVALUE :H='Segment value',"+;
                     "CSEGSHDES :H='Short Description' ,"+;
                     "CTYPECODE :H='Segment Type' "
                     
    	  lcPbrowtitl = 'Segment '+ laAcDes[lnSegno,1] +' values'
        SELECT GLSEGVAL	
        IF !FILE(gcWorkDir+lcTmpIDX+'.IDX')
          INDEX ON CSEGVALUE TO gcWorkDir+lcTmpIDX
        ENDIF
        SET ORDER TO &lcTmpIDX 
        SET FILT TO CACSSEGNO = lcSegno
        GO TOP
        =AriaBrow('',lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                            gnBrFsRow2, gnBrFsCol2,'','',;
                          'CSEGVALUE,CTYPECODE','laTemp')                    
        SET FILT TO 
        SET ORDER TO
        GO TOP
  
        SELECT &lcChngSeg            
        GOTO (lnRecno)
        =IIF(!EMPTY(laTemp[1]) .AND. CSEGVALUE = laTemp[1],gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not accept the same value.'),'')
        REPLACE CNEWSEGVAL WITH IIF(EMPTY(laTemp[1]) .OR. CSEGVALUE = laTemp[1] , '' , laTemp[1] )
        
        IF !EMPTY(CNEWSEGVAL) 
          lcSeg = CNEWSEGVAL
          LOCATE FOR CSEGVALUE = lcSeg .AND. !EMPTY(CNEWSEGVAL)
          IF FOUND()
            =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not transfer to segment '+ ALLTRIM(lcSeg) +' , it is already transferred to another segment.')
            GOTO (lnRecno)
            REPLACE CNEWSEGVAL WITH CTMPFLD 
          ENDIF
        ENDIF
        
      ELSE
        
        IF !EMPTY(CNEWSEGVAL)
          =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not transfer to segment '+ ALLTRIM(lcSeg) +' , it is already transferred to another segment.')
          GOTO (lnRecno)
          REPLACE CNEWSEGVAL WITH CTMPFLD 
        ENDIF
        
      ENDIF
    
    ENDIF
      
  ENDIF  
  
ENDIF  

LOCATE FOR CNEWSEGVAL<>CTMPFLD
lcOkStat = IIF(FOUND(),'ENABLE','DISABLE')
SHOW GET pbOk &lcOkStat

IF BETWEEN(lnRecno,1,RECCOUNT())
  GOTO (lnRecno)
ENDIF
*-- end of lfvRplSgm.

*:**************************************************************************
*:* Name        : lfvViewAct
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : This command will allow the user to view the previous and 
*               : new account code structure before actual database update. 
*:***************************************************************************
FUNCTION lfvViewAct
PRIVATE lcDet_Ttl,llOk,lnMarker,lcPict,lnK,lcK

*- Check that all new added segments are filled
FOR lnK = laOldData[4]+1 TO laData[4]
  lcK = STR(lnK,1)
  IF EMPTY(lcSegVal&lcK)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to enter values to all new added segments.')
    RETURN .F.
  ENDIF
ENDFOR
lnMarker = 1
lcPict = laData[2]
lcTmpBrw  = "nRec=IIF(RECNO()=lnMarker,'>',' '):H=' '             :W=.F.,"+;
            "CACCNSDES   :H='GL Account Description'              :W=.F.,"+;
            "CPRVACCODE  :H='Current GL Accounts'              :27:W=.F.,"+;
            "CNEWACCODE  :H='New GL Accounts'                  :27:W=.F.,"+;
            "CMNLACCODE  :H='Transfer Balances to GL Accounts' :39:P=[&lcPict] :V=lfvCgAcnt()"
            
lcOkStat = 'DISABLE'                          
SELECT &lcViewAcct 
GO TOP
SET ORDER TO
REPLACE ALL CNEWACCODE WITH CPRVACCODE

SCAN
  =gfThermo(RECCOUNT(lcViewAcct) , RECNO(lcViewAcct) , ;
       'Building the modified temp. chart of accounts' , CPRVACCODE+' ==> '+CNEWACCODE )
  =lfUpdAcCod( lcViewAcct , 'CNEWACCODE' , .T. )
  =lfUpdAcCod( lcViewAcct , 'CMNLACCODE' , .T. )
  IF CMNLACCODE = CNEWACCODE
    REPLACE CMNLACCODE WITH ''
  ENDIF
  REPLACE CTMPACTFLD WITH CMNLACCODE     
*-*  WAIT WINDOW NOWAIT 'Building the modified Chart of Accounts.. '+
ENDSCAN
WAIT CLEAR

SELECT &lcViewAcct 
SET ORDER TO TAG &lcViewAcct
LOCATE

llOk = .T.
lcDet_Ttl = 'Current & New GL Accounts'

PUSH KEY
ON KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
ON KEY LABEL ESC llDummy = .T.
DO (gcScrDir+'SM\SMVEWAC.SPX')
POP KEY                

*- redefine Esc key
ON KEY LABEL ESC DO lfvCancel

SELECT &lcViewAcct
GO TOP
IF llOk
  *- 
ELSE
  WAIT WINDOW NOWAIT 'Restoring Chart of Accounts.'
  REPLACE ALL CMNLACCODE WITH CTMPACTFLD  
ENDIF
WAIT CLEAR
LOCATE                 
*-- end of lfvViewAct.

*:**************************************************************************
*:* Name        : lfvVwAcnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Valid function for viewing old and new account browse
*:***************************************************************************
FUNCTION lfvVwAcnt
PRIVATE lnSlct,lnRecno,laAcSg,lnK,lcK,lcSeg,lcAcc,lnAcCount,lcMsg
lnSlct = SELECT()

IF EMPTY( CHRTRAN(CNEWACCODE,'-','') )
  lcMsg = 'Can not leave the field empty, restoring the last previous value.'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  REPLACE CNEWACCODE  WITH CTMPACTFLD  
  RETURN
ENDIF

DIMENSION laAcSg[1]
=gfSubStr(CNEWACCODE,@laAcSg,'-')  
FOR lnK = 1 TO ALEN(laAcSg)
  IF EMPTY(laAcSg[lnK])
    lcMsg = 'Can not leave any segment empty, restoring the last previous value.'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
    REPLACE CNEWACCODE  WITH CTMPACTFLD  
    RETURN
  ENDIF
ENDFOR

lnRecno = RECNO()
*- Check the existance of each segment 
lcSeg = ''
FOR lnK = 1 TO ALEN(laAcSg)
  lcK = STR(lnK,1)
  IF lnK <= laOldData[4]
    SELECT &lcChngSeg 
    LOCATE FOR CACSSEGNO = lcK .AND. ( CSEGVALUE == PADR(laAcSg[lnK],15) .OR. CNEWSEGVAL == PADR(laAcSg[lnK],15) )
    IF !FOUND()
      SELECT (lnSlct)    
      lcMsg = 'Transferred segments must be one of the old ones or new added ones, restoring the last previous value.'
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
      REPLACE CNEWACCODE  WITH CTMPACTFLD  
      RETURN
    ENDIF
    SELECT (lnSlct)
  ELSE
    IF PADR(laAcSg[lnK],lnSeg&lcK) <> lcSegVal&lcK
      lcMsg = 'For new segments you can only use one value.'
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
      REPLACE CNEWACCODE  WITH CTMPACTFLD  
      RETURN      
    ENDIF
  ENDIF
ENDFOR

*- Check if this GL-account is repeated 
lcAcc = CNEWACCODE
lnAcCount = 0
GO TOP
COUNT FOR CNEWACCODE == lcAcc TO lnAcCount
IF lnAcCount > 1
  lcMsg = 'Your changes may caused an account duplication.'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  GOTO (lnRecno)
  REPLACE CNEWACCODE WITH CTMPACTFLD
  RETURN      
ENDIF

GO TOP
LOCATE FOR CNEWACCODE<>CTMPACTFLD
lcOkStat = IIF(FOUND(),'ENABLE','DISABLE')
SHOW GET pbOk &lcOkStat

IF BETWEEN(lnRecno,1,RECCOUNT())
  GOTO (lnRecno)
ENDIF
*-- end of lfvVwAcnt.

*:**************************************************************************
*:* Name        : lfvCgAcnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2005
*:* Purpose     : Changed accounts
*:***************************************************************************
FUNCTION lfvCgAcnt
PRIVATE lnSlct,lnRecno,laAcSg,lnK,lcK,lcSeg,lcAcc,lnAcCount,lcMsg
lnSlct = SELECT()
lnRecno = RECNO()

IF !EMPTY(STRTRAN(CMNLACCODE,'-',''))
  
  lcSeg = CMNLACCODE
  
  IF CNEWACCODE = CMNLACCODE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not accept the same value.')
    REPLACE CMNLACCODE WITH ''
  
  ELSE
  
    LOCATE FOR CNEWACCODE = lcSeg
    IF !FOUND()
    
      PRIVATE latemp,lcBrFields,lcTmpAlias
      DIMENSION latemp[2]
      latemp = '' 
      lcBrFields = "CNEWACCODE :H='GL-Account Code'    , "+;
                   "CACCNSDES  :H='GL-Account Code Short Description' , "+;
                   "CTYPECODE  :H='Account Type' "
      lcPbrowtitl = 'New GL chart of accounts'
      SELECT &lcViewAcct 
      lcTmpAlias = gfTempName()
      USE DBF(lcViewAcct) AGAIN IN 0 ALIAS &lcTmpAlias ORDER TAG CNEWACCODE
      SELECT &lcTmpAlias
      GO TOP
      =AriaBrow('',lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                      gnBrFsRow2, gnBrFsCol2,'','',;
                      'CNEWACCODE,CTYPECODE','laTemp')
      USE IN &lcTmpAlias
      
      SELECT &lcViewAcct 
      GOTO (lnRecno)
      =IIF(!EMPTY(latemp[1]) .AND. latemp[1] = CNEWACCODE,gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not accept the same value.'),'')
      REPLACE CMNLACCODE WITH IIF(EMPTY(latemp[1]) .OR. latemp[1] = CNEWACCODE , '' , latemp[1])
    ENDIF
  
  ENDIF
  
ENDIF

GO TOP
LOCATE FOR CHRTRAN(CMNLACCODE,'-',' ') <> CHRTRAN(CTMPACTFLD,'-',' ')
lcOkStat = IIF(FOUND(),'ENABLE','DISABLE')
SHOW GET pbOk &lcOkStat

IF BETWEEN(lnRecno,1,RECCOUNT())
  GOTO (lnRecno)
ENDIF

*-- end of lfvCgAcnt.
*:**************************************************************************
*:* Name        : lfvVewAcOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/26/2005
*:* Purpose     : Valid function for OK button
*:***************************************************************************
FUNCTION lfvVewAcOk
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to accept Transferred values?') = 1
  llOk = .T.
  CLEAR READ
ENDIF  

*-- end of lfvVewAcOk.

*:**************************************************************************
*:* Name        : lfvVewAcCn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/26/2005
*:* Purpose     : Valid function for cancel button for View Accounts screen
*:***************************************************************************
FUNCTION lfvVewAcCn

IF lcOkStat = 'ENABLE' .AND. ;
  gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you ignore Transferred values?') <> 1
  RETURN
ENDIF  

llOk = .F.
CLEAR READ  

*-- end of lfvVewAcCn.


*:**************************************************************************
*:* Name        : lfEraseFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Erase temp files
*:***************************************************************************
FUNCTION lfEraseFl
PARAMETERS lcFile

IF USED(lcFile)
  USE IN &lcFile
ENDIF
ERASE (gcWorkDir+lcFile+'.DBF')
ERASE (gcWorkDir+lcFile+'.CDX')
ERASE (gcWorkDir+lcFile+'.FPT')
  
*-- end of lfEraseFl.

*:**************************************************************************
*:* Name        : lfvCancel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Standard cancellation button which typically ignores all changes / entries
*:              : done by the user and closes the screen. 
*:***************************************************************************
FUNCTION lfvCancel
PRIVATE lcMsg
*- Display user warning message 
  *-Yes will confirm cancellation ignoring all users actions in this screen and close the screen. 
  *-No will take the user back to the same screen. 

lcMsg = 'This will cause cancellation of all changes done to your current account code structure/Transferred GL-Accounts values/Transferred segments values , '+;
        'are you sure you want to cancel?' 
IF !llChange
  glQuitting = .T.
  CLEAR READ
ELSE  
  IF gfModalGen('INM00000B00006',.F.,.F.,.F.,lcMsg) = 1
    glQuitting = .T.
    CLEAR READ
  ENDIF
ENDIF  

*-- end of lfvCancel.

*:**************************************************************************
*:* Name        : lfOldValue
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Function to store old value of the current filed.
*:***************************************************************************
FUNCTION lfOldValue
lcOldVal = EVALUATE(SYS(18))

*:**************************************************************************
*:* Name        : lfAudtFlds
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2005
*:* Purpose     : Update audit fields
*:***************************************************************************
FUNCTION lfAudtFlds
PARAMETERS lcAlias,lcMode
PRIVATE lnSlct
lnSlct = SELECT()

*- If no mode is passed , it is assumed for Add mode
lcMode = IIF(EMPTY(lcMode),'A',lcMode)

IF !EMPTY(lcAlias)
  SELECT &lcAlias
ENDIF
  
DO CASE
CASE lcMode = 'A'
  REPLACE CADD_USER WITH gcUser_ID ;
          CADD_TIME WITH TIME()    ;
          DADD_DATE WITH gdSysDate 
CASE lcMode = 'E'
  REPLACE CEDIT_USER WITH gcUser_ID ;
          CEDIT_TIME WITH TIME()    ;
          DEDIT_DATE WITH gdSysDate
ENDCASE
IF TYPE('CADD_VER')='C'
  REPLACE CADD_VER  WITH 'A27' 
ENDIF
IF TYPE('CEDT_VER')='C'
  REPLACE CEDT_VER  WITH 'A27'
ENDIF  

SELECT (lnSlct)
*-- end of lfAudtFlds.

*:**************************************************************************
*:* Name        : lfCgSgRdAc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11-17-2005
*:* Purpose     : Read active function for change segment screen
*:***************************************************************************
FUNCTION lfCgSgRdAc

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*-- end of lfCgSgRdAc.

*:**************************************************************************
*:* Name        : lfCgSgClrT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11-17-2005
*:* Purpose     : Clear trap function for change segment screen
*:***************************************************************************
FUNCTION lfCgSgClrT

IF WONTOP() = lcDet_Ttl
  ON KEY LABEL TAB     DO lpChSgTab
  ON KEY LABEL BACKTAB DO lpChSgBkT
ENDIF  

*-- end of lfCgSgClrT.

*:**************************************************************************
*:* Name        : lpChSgTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11-17-2005
*:* Purpose     : Tab function for change segment screen
*:***************************************************************************
FUNCTION lpChSgTab
IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWin3)
ENDIF
ON KEY LABEL TAB    
*-- end of lpChSgTab.

*:**************************************************************************
*:* Name        : lpChSgBkT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11-17-2005
*:* Purpose     : Backtab function for the change scgment screen 
*:***************************************************************************
FUNCTION lpChSgBkT

IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWin3)
ENDIF
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB

*-- end of lpChSgBkT.

*:**************************************************************************
*:* Name        : lfChkDskSP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2005
*:* Purpose     : Check disk space
*:***************************************************************************
FUNCTION lfChkDskSP

*- << Put here the appropriate code >>

RETURN .T.
*-- end of lfChkDskSP.

*:**************************************************************************
*:* Name        : lfvFind
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/27/2005
*:* Purpose     : Find screen 	
*:***************************************************************************
*:* Called from : view accounts screen
*:***************************************************************************
FUNCTION lfvFind
DO lfIncSearch
*-- end of lfvFind.

*-* *!**************************************************************************
*-* *!
*-* *!      Function: lfActDeact
*-* *!
*-* *!**************************************************************************
*-* FUNCTION lfActDeact
*-* 
*-* IF WONTOP(lcDet_Ttl)
*-*   *- Add inceremntal search in the account browse.
*-*   FOR lnChrToTrap = 32 TO 126
*-*     ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch
*-*   ENDFOR
*-* ENDIF
*-* 
*-* RETURN .F.

*!*************************************************************
*! Name      : lfIncSearch
*! Developer : TMI
*! Date      : 11/29/2005
*! Purpose   : Incremental search from the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIncSearch()
*!*************************************************************
*B605999,1 ASH 05/20/2002 Trap the incremental search in the browse.
*!*************************************************************
*
FUNCTION lfIncSearch
PRIVATE lnBrRecNO , lcOrgTag , lcExpToSeek

PUSH KEY CLEAR
ON KEY
ON KEY LABEL F1 llDumi = .T.
CLEAR TYPEAHEAD

*-- Charcter to be searched.
lcExpToSeek = ''

*-- Define the window of the incremental search function.
DEFINE WINDOW lwIncSrch ;
    AT 21.000, 10.000 ;
    SIZE 4.167,55.444 ;
    FONT "FoxFont", 9 ;
    STYLE "B" TITLE "Aria Apparel System" ;
    FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)
  MOVE WINDOW lwIncSrch CENTER    
  ACTIVATE WINDOW lwIncSrch NOSHOW

PUSH KEY CLEAR
CLEAR TYPEAHEAD

*-- Call function to activate the search screen.
DO lfActInSr 
POP KEY
*-- Refresh the account browse.
ACTIVATE WINDOW (lcDet_Ttl) 
=lfwBrowse()
KEYBOARD '{TAB}{TAB}'

*!*************************************************************
*! Name      : lfActInSr
*! Developer : TMI
*! Date      : 11/29/2005
*! Purpose   : the second part of the incremental search function
*!           : to get the value of searching.
*!*************************************************************
FUNCTION lfActInSr
PRIVATE lcCnfrm,lcPict
lcCnfrm = SET('CONFIRM')
SET CONFIRM ON
lcPict = ALLTRIM(STRTRAN(laOldData[2],'#','9'))
@ 1.500,14.333 GET lcExpToSeek ;
  SIZE 1.167,41.000 PICTURE lcPict FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"

@ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
  STYLE "B"
	
@ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)

ACTIVATE WINDOW lwIncSrch TOP

READ MODAL
RELEASE WINDOW lwIncSrch

*- Restore confirm setting
SET CONFIRM &lcCnfrm
lcExpToSeek = ALLTRIM(STRTRAN(lcExpToSeek,' -','  '))

*-- Expression to be searched.
lcExpToSeek = ALLTRIM(lcExpToSeek)
*-- If press enter, search for the typed expression.
IF LASTKEY() = 13
    *-- Save the record pointer of the lcViewAcct file.  
    lnOrdRecNO = IIF(RECNO(lcViewAcct) > RECCOUNT(lcViewAcct) , 0 , RECNO(lcViewAcct))
    IF SEEK(lcExpToSeek)
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    ELSE
      *-- restore the record pointer if the typed expression was not found.
      IF lnOrdRecNO > 0 .AND. lnOrdRecNO <= RECCOUNT(lcViewAcct)
        GO lnOrdRecNO IN &lcViewAcct
      ENDIF
    ENDIF
  *--First Folder 
ELSE
  lcExpToSeek = ''
ENDIF

ON KEY
CLEAR TYPEAHEAD
POP KEY
RETURN

*:**************************************************************************
*:* Name        : lfGtMajAc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/2006
*:* Purpose     : Get major account with all non-major segments filled with zeros
*:***************************************************************************
*B040141,1 
FUNCTION lfGtMajAc
PARAMETERS lcString
FOR lnK = 1 TO 9
  lcString = CHRTRAN(lcString,STR(lnK,1),'0')
ENDFOR
RETURN lcString
