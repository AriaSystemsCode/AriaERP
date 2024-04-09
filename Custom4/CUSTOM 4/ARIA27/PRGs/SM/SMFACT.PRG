*:************************************************************************
*:
*: Procedure file: SMFACT.PRG 
*:                 (Factors)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
*: Last modified :
*E300683,4 AMM 06/08/97 Add prgs directory path to the calling of programs
*E300818,1 AMM 02/25/98 Add some objects to the factor screen.
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*B801868,1 AMM 12/21/98 Fix the bug of "table not ordered" when deleting a factor
*B801868,1              by opening 'SYDFILES' tag 'CFILE_NAM' in SYDOBJCT
*E500374,1 NAD 09/12/2000 ADD Gl link code for the factor per company.
*:************************************************************************
EXTERNAL ARRAY laData,laKeyField,laDefProc
DECLARE laKeyField [1,4],laCountry[1]
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CFACCODE'
laKeyField[1,4] = 1

llBrowse = .F.

*E300818,1 AMM Add variable to display the password as astrisks on the screen.
lcPassWord    = SPACE(4)
laDefProc[9]  = .F.       && Use a local save procedure.
*E300818,1 AMM end
IF !gfSetup()    
  RETURN
ENDIF  

laDefProc[7]    = .F.

 
IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  
  lcApVend = gfTempName()
  
  SELECT SYCFACT
ENDIF

*--hesham el-sheltawi
puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[8]),'USA',laData[8]),sycint.ccont_code,'CCONTCODE')        

*E300818,1 AMM fill a variable hold the factor fields status.
lcFactStat = IIF(laData[15] .AND. (laSCrMode[3] .OR. laScrMode[4]),;
			     'ENABLE', 'DISABLE') 
*E300818,1 AMM end


*E500374,1 (Start) Variables needed for the factor  GL link code screen.
lcSysPath=SUBSTR(gcSysHome,1,LEN(gcSysHome)-9)
lcDataPath=lcSysPath+'DBFS\'
llFillData=.T.
lcFclnkTmp=gfTempName()
=lfFactTmp()             && Create the temp file.
SELECT SycComp
LOCATE FOR gfGetMemVar('M_LINK_GL',cComp_Id)='Y'
llGLStat=FOUND()
*E500374,1 (End)


*E300683,4 Call *.SPR from screens directory
* DO Smfact.SPR 
DO (gcScrDir + gcWinAppl + '\Smfact.SPR')
*E300683,4 end

*E500374,1 (Start)   Erase the temp file.
IF USED (lcFclnkTmp)
  USE IN (lcFclnkTmp)
ENDIF
ERASE (gcWorkDir+lcFclnkTmp+'.*')
*E500374,1 (End)

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow


*E500374,1 (Start) Empty the temp file.
=lfvCnclLnk(.T.)
*E500374,1 (End)

*--hesham el-sheltawi
DO CASE
  *E300818,1 AMM start, Comment out to split in two separate cases.
  *CASE laScrMode[1] OR laScrMode[4]
      *puCountry = LOOKUP(SYCINT.CPARt1LAB,'USA',sycint.ccont_code,'CCONTCODE')       
      *IF laScrMode[4]
        *laData[14] = LOOKUP(SYCINT.CCONT_DESC,'USA',sycint.ccont_code,'CCONTCODE')            
      *ENDIF  
  *E300818,1 AMM Select mode.
  CASE laScrMode[1]
    puCountry = LOOKUP(SYCINT.CPARt1LAB,'USA',sycint.ccont_code,'CCONTCODE')           
    SHOW GET laData[15] DISABLE
    SHOW GET laData[16] DISABLE
    SHOW GET laData[17] DISABLE
    SHOW GET laData[18] DISABLE
    SHOW GET ibPassWord DISABLE
    lcPassWord = SPACE(4)
    
    *E500374,1 (Start) Control the status of the factor link code button.
    SHOW GET pbFacLnk DISABLE
    *E500374,1 (END)
    
    =lfRefresh()
    *E300818,1 AMM View Mode
  CASE laScrMode[2]
    puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[8]),'USA',laData[8]),sycint.ccont_code,'CCONTCODE')       
    *E300818,1 AMM start, Disable next objects in view mode.
    SHOW GET laData[15] DISABLE
    SHOW GET laData[16] DISABLE
    SHOW GET laData[17] DISABLE
    SHOW GET laData[18] DISABLE
    SHOW GET ibPassWord DISABLE
    *E300818,1 AMM Display the corresponding astrisks on the screen instead 
    *E300818,1 AMM of the true password.
    lcPassWord = lfGetPass()
    =lfRefresh()
    *E300818,1 AMM end    
  *E300818,1 AMM Edit mode
  
    *E500374,1 (Start) Control the status of the factor link code button.
    SHOW GET pbFacLnk ENABLE
    *E500374,1 (End)
  
  CASE laScrMode[3]
    IF laData[15]
      *E300818,1 AMM  Factor uses electronic batch transmissins is "Yes",
      *E300818,1 AMM  enable all the related data and vice versa.
      SHOW GET laData[16] ENABLE
      SHOW GET laData[17] ENABLE
      SHOW GET laData[18] ENABLE
      SHOW GET ibPassWord ENABLE
    ELSE
      SHOW GET laData[16] DISABLE
      SHOW GET laData[17] DISABLE
      SHOW GET laData[18] DISABLE
      SHOW GET ibPassWord DISABLE
    ENDIF    
  *E300818,1 AMM Add mode
  
 
  CASE laScrMode[4]
    puCountry = LOOKUP(SYCINT.CPARt1LAB,'USA',sycint.ccont_code,'CCONTCODE')       
    laData[14] = LOOKUP(SYCINT.CCONT_DESC,'USA',sycint.ccont_code,'CCONTCODE')            
    SHOW GET laData[15] ENABLE
    SHOW GET laData[16] DISABLE
    SHOW GET laData[17] DISABLE
    SHOW GET laData[18] DISABLE
    SHOW GET ibPassWord DISABLE    
  *E300818,1 AMM end
  
ENDCASE 

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
 
EXTERNAL ARRAY laData 

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13)
  =gfSeekRec() 
  *E300818,1 AMM When getting the factor and its password, display the corresponding 
  *E300818,1 AMM astrisks on the screen instead of the true password.
  lcPassWord = lfGetPass()
  =lfRefresh()
  *E300818,1 AMM end
ENDIF
llBrowse = .F.
 
*!**************************************************************************
*!
*!      Function: lpDelScr
*!
*!**************************************************************************
*
FUNCTION lpDelScr

llDelFact = .T.

SELECT SYDFILES
IF SEEK("APVENDOR")
  lcStr = SYDFILES.mFile_App
  lcStr = STRTRAN(lcStr,"$","")
  DECLARE laStr[1]
  =gfSubStr(lcStr,@laStr,',')
  IF !EMPTY(laStr[1])
    SELECT SYCCOMP
    SCAN
      FOR lnCount = 1 TO ALEN(laStr,1)
        IF AT(laStr[lnCount],SYCCOMP.mComp_mdl) > 0
          *E301098,1 Hesham (Start)
          *lcFilPath = ALLTRIM(SYCCOMP.cCom_ddir)+"APVENDOR.DBF"
          lcFilPath = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_ddir))+"APVENDOR.DBF"
          *E301098,1 Hesham (End)
          IF FILE(lcFilPath)
            SELECT 0
            USE (lcFilPath) AGAIN ALIAS &lcApVend
            LOCATE FOR &lcApVend..cFacCode = laData[1]
            IF FOUND()
              llDelFact = .F.
              *** Company ð has vendor ð factored to factor ð. ***
              *** You cannot delete this factor...
              *** <  Ok  > ***
              lcTmpStr = ALLTRIM(SYCCOMP.cCom_name)+"|"+;
                         ALLTRIM(&lcApVend..cVendcode)+"|"+;
                         ALLTRIM(laData[1])
              =gfModalGen("TRM00187B00000","DIALOG",lcTmpStr)
              EXIT
            ENDIF
            IF USED(lcApVend)
              USE IN ALIAS(lcApVend)
            ENDIF
          ENDIF
        ENDIF
      ENDFOR
      IF !llDelFact
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

IF USED(lcApVend)
  USE IN ALIAS(lcApVend)
ENDIF

IF !llDelFact
  SELECT SYCFACT
  RETURN
ENDIF

*E500374,1 (Start) Delete the factor from the factor file in each company.
SELECT SycComp
SCAN
  IF USED('Factor')
    USE IN FACTOR
  ENDIF  
  =gfOpenFile(ALLTRIM(cCom_DDir)+'Factor',ALLTRIM(cCom_DDir)+'Factor','SH')
  SELECT Factor
  IF SEEK (laData[1])
    DELETE
  ENDIF
ENDSCAN  
*E500374,1 (End)
SELECT SYCFACT
SCATTER MEMVAR MEMO BLANK
GATHER  MEMVAR MEMO 
DELETE

laScrMode    = .F.
laScrMode[1] = .T.

*!**************************************************************************
*! NAME      : lfvFEBT
*! DEVELOPER : AHMED MOHAMMED IBRAHIM
*! DATE      : 02/25/98
*! PURPOSE   : VALID FUNCTION OF THE CHECK BOX "FACTOR USES ELECTRONIC 
*!             BATCH TRANSMISSIONS" IN THE FACTOR SCREEN
*! REFERENCE : *E300818,1
*!**************************************************************************
*! CALLS     :
*!              PROCEDURES : none
*!              FUNCTIONS  : lfRefresh()
*!**************************************************************************
*! PASSED PARAMETERS  : NONE
*!**************************************************************************
*! RETURNS   : .T.
*!**************************************************************************
*! EXAMPLE   : =lfFEBT()
*!**************************************************************************

FUNCTION lfvFEBT
*E300818,1 AMM If 'Factor uses elec...' option is true enable the objects 
*E300818,1 AMM related to it.
IF laData[15]
  *E300818,1 AMM  Factor uses electronic batch transmissions is "Yes",
  *E300818,1 AMM  enable all the related data.
  SHOW GET laData[16] ENABLE
  SHOW GET laData[17] ENABLE
  SHOW GET laData[18] ENABLE
  SHOW GET ibPassWord ENABLE
ELSE
  *E300818,1 AMM  Factor uses electronic batch transmissins is "No",
  *E300818,1 AMM  blank and disable all the related data.
  laData[16] = SPACE(8)
  laData[17] = SPACE(6)
  laData[18] = SPACE(4)
  laData[19] = SPACE(4)
  lcPassWord = SPACE(4)
  SHOW GET laData[16] DISABLE
  SHOW GET laData[17] DISABLE
  SHOW GET laData[18] DISABLE
  SHOW GET ibPassWord DISABLE
  =lfRefresh()
ENDIF

*!**************************************************************************
*! NAME      : lfwRdPass
*! DEVELOPER : AHMED MOHAMMED IBRAHIM
*! DATE      : 02/25/98
*! PURPOSE   : Read the password from the user (the WHEN function of the 
*!             invisible button).
*! REFERENCE : *E300818,1
*!**************************************************************************
*! CALLS     :
*!              PROCEDURES : none
*!              FUNCTIONS  : lfRefresh()
*!**************************************************************************
*! PASSED PARAMETERS  : NONE
*!**************************************************************************
*! RETURNS   : .T.
*!**************************************************************************
*! EXAMPLE   : =lfwRdPass()
*!**************************************************************************

FUNCTION lfwRdPass

*E300818,1 AMM Initialize the coordinate of the password object.
lnColom = 62.25
lnRow   = 27.58
*E300818,1 AMM Array laScObj hold the coordinate of each object on the screen.
*E300818,1 AMM so, get the coordinate of the Invisible button of the password
lnPos = ASCAN(laScObj,'IBPASSWORD')
IF lnPos # 0
  LNPOS   = ASUBS(laScObj,lnpos,1)
  lnRow   = laScObj[lnPos,1]
  lnColom = laScObj[lnPos,2]
ENDIF
lnCol   = lnColom

*E300818,1 AMM Put the password value in an array.
DECLARE laPass[4]
STORE SPACE(1) TO laPass

*E300818,1 AMM fill the array with the four chr. of the password.
FOR lnCount = 1 TO 4
  laPass[lnCount] = SUBSTR(laData[19],lnCount,1)
ENDFOR

lnReadKey= 0

*E300818,1 Set cursor position
@ lnRow,lnCol SAY ''

*E300818,1 AMM Loop in the 4 ch. of the password value.

DO WHILE lnCol < lnColom+4
  lnReadKey = INKEY(0,'MS')

  IF INLIST(lnReadKey,13,27,9,15,18,3,5,24,151)
    *E300818,1 AMM If the key entered was :-
    *E300818,1 AMM {Enter} - {Escape} - {Tab} - {ShiftTab} {PGUP} {PGDN} {UPARROW} 
    *E300818,1 AMM {DNARROW} or MOUSE CLICK exit from the loop.
    EXIT
  ENDIF

  DO CASE
    CASE lnReadKey = 4               && right arrow
      *E300818,1 AMM If not last chr. and the next chr. not empty move the 
      *E300818,1 AMM cursor right.
      IF lnCol+1 < lnColom+4 .AND. !EMPTY(laPass(lnCol-lnColom+2))
        lnCol=lnCol+1
        @ lnRow,lnCol SAY ''
      ENDIF
      
    CASE lnReadKey = 127              && backspace
      IF lnCol > lnColom
        *E300818,1 AMM Decrement the col number and shift the chr. on the right to 
        *E300818,1 AMM the cursor one chr. left.
        lnCol = lnCol - 1
        FOR lnCnt = lnCol - lnColom + 1 TO 3
          laPass[lnCnt] = laPass[lnCnt+1]
        ENDFOR
        laPass[4] = ' '
        lcPassWord = ''
        FOR lnCnt = 1 TO 4
          lcPassWord = lcPassWord + IIF(!EMPTY(laPass[lnCnt]),'*',"")
        ENDFOR
        @ lnRow,lnCol SAY ''
        =lfRefresh()
      ENDIF
    
    CASE lnReadKey = 7                 && DELTETE BUTTON    
      *E300818,1 AMM Shift the chr. on the right to the cursor one chr. left.
      FOR lnCnt = lnCol - lnColom + 1 TO 3
        laPass[lnCnt] = laPass[lnCnt+1]
      ENDFOR
      laPass[4] = ' '
      lcPassWord = ''
      FOR lnCnt = 1 TO 4
        lcPassWord = lcPassWord + IIF(!EMPTY(laPass[lnCnt]),'*',"")
      ENDFOR
      @ lnRow,lnCol SAY ''
      =lfRefresh()
         
    CASE lnReadKey = 19                  && left arrow
      *E300818,1 AMM If not first chr. move the position and cursor one chr. left.
      lnCol = IIF(lnCol=lnColom,lnColom,lnCol-1)
      @ lnRow,lnCol SAY ''
      
    CASE !BETWEEN(lnReadKey, 40, 122)
      *E300818,1 AMM If the ch. pressed not numeric or alphbetic, loop again
      @ lnRow,lnCol SAY ''
      LOOP
    OTHERWISE
      *E300818,1 AMM Save the entered ch. in the array.
      laPass(lnCol-lnColom+1) = UPPER(CHR(lnReadKey))
      *E300818,1 AMM Add one to the column value.
      lnCol=lnCol+1
      lcPassWord = ''
      *E300818,1 AMM Get the string wich will be displayed on the screen.
      FOR lnCnt = 1 TO 4
        lcPassWord = lcPassWord + IIF(!EMPTY(laPass[lnCnt]),'*',"")
      ENDFOR
      =lfRefresh()
  ENDCASE
  @ lnRow,lnCol SAY ''
ENDDO

*E300818,1 AMM Get the true password value  in laData[19] and the corresponding 
*E300818,1 AMM astrisk signs in the lcPassword.
lcPassWord = ''
laData[19] = ''
FOR lnCnt = 1 TO 4
  lcPassWord = lcPassWord + IIF(!EMPTY(laPass[lnCnt]),'*',"")
  laData[19] = laData[19] + IIF(!EMPTY(laPass[lnCnt]),laPass[lnCnt],"")
ENDFOR

*E300818,1 AMM If the last key is {SHIFT TAB} {UPARROW} {PGUP} go to previous object, 
*E300818,1 AMM ELSE go to next object.
IF INLIST(lnReadKey,15,5,18)
  _CUROBJ = OBJNUM(laData[17])
ELSE
  *E300818,1 AMM If not a mouse click
  IF lnReadKey # 151      
    _CUROBJ = OBJNUM(pbSAV)
  ENDIF
ENDIF

*!**************************************************************************
*! NAME      : lfGetPass
*! DEVELOPER : AHMED MOHAMMED IBRAHIM
*! DATE      : 02/25/98
*! PURPOSE   : Fill the variable which will displayed instead of the password.
*! REFERENCE : *E300818,1
*!**************************************************************************
*! CALLS     :
*!              PROCEDURES : none
*!              FUNCTIONS  : none
*!**************************************************************************
*! PASSED PARAMETERS  : NONE
*!**************************************************************************
*! RETURNS   : lcPassWord
*!**************************************************************************
*! EXAMPLE   : =lfGetPass()
*!**************************************************************************
FUNCTION lfGetPass

PRIVATE lcPassWord, lnCnt
lcPassWord = ''
IF laData[15] .AND. !EMPTY(laData[19])
  FOR lnCnt = 1 TO 4
    lcPassWord = lcPassWord + IIF(!EMPTY(SUBSTR(laData[19],lnCnt,1)),'*','')
  ENDFOR
ENDIF
RETURN lcPassWord

*!**************************************************************************
*! NAME      : lpSavScr
*! DEVELOPER : AHMED MOHAMMED IBRAHIM
*! DATE      : 02/25/98
*! PURPOSE   : Local save procedure.
*! REFERENCE : *E300818,1
*!**************************************************************************
*! CALLS     :
*!              PROCEDURES : none
*!              FUNCTIONS  : gfModalGen()
*!**************************************************************************
*! PASSED PARAMETERS  : NONE
*!**************************************************************************
*! RETURNS   : NONE
*!**************************************************************************
*! EXAMPLE   : =lpSavScr()
*!**************************************************************************
PROCEDURE lpSavScr

PRIVATE lcField
*E300818,1 llCSave = .F. if saving will not be possible
DO CASE
  CASE laData[15] .AND. EMPTY(laData[16])
    *E300818,1 Message : "   You have to enter the ð.  "
    *E300818,1                 ® OK  ¯
    lcField = 'ID assigned by the factor'
    =gfModalGen("TRM04066B00000","DIALOG",lcField)
    _CUROBJ = OBJNUM(laData[16])
    llCSave    = .F.

  CASE laData[15] .AND. EMPTY(laData[18])
    *E300818,1 Message : "   You have to enter the ð.  "
    *E300818,1                 ® OK  ¯
    lcField = 'confirmation ID assigned by the factor'
    =gfModalGen("TRM04066B00000","DIALOG",lcField)
    _CUROBJ = OBJNUM(laData[18])
    llCSave    = .F.

  CASE laData[15] .AND. EMPTY(laData[17])
    *E300818,1 Message : "   You have to enter the ð.  "
    *E300818,1                 ® OK  ¯
    lcField = 'number assigned by the factor'
    =gfModalGen("TRM04066B00000","DIALOG",lcField)
    _CUROBJ = OBJNUM(laData[17])
    llCSave    = .F.

  CASE laData[15] .AND. EMPTY(laData[19])
    *E300818,1 Message : "   You have to enter the ð.  "
    *E300818,1                 ® OK  ¯
    lcField = 'factor security password'
    =gfModalGen("TRM04066B00000","DIALOG",lcField)
    _CUROBJ = OBJNUM(ibPassWord)
    llCSave    = .F.

ENDCASE

IF llCSave
  SELECT SYCFACT
  IF laScrMode[4]    
    APPEND BLANK
  ENDIF
  *E300818,1 Store laData values in the current record
  GATHER FROM laData FIELDS &lcScFields
  
  *E500374,1 (Start) Update the factor file with the link code.
  IF SEEK (laData[1],lcFcLnkTmp)
    SELECT (lcFcLnkTmp)  
    SCAN REST WHILE cFacCode=laData[1]
      IF USED('Factor')
        USE IN FACTOR
      ENDIF  
      =gfOpenFile(lcDataPath+ALLTRIM(cComp_Id)+'\Factor',lcDataPath+ALLTRIM(cComp_Id)+'\Factor','SH')
      SELECT Factor
      IF SEEK (laData[1],'Factor')
        =RLOCK()    
        REPLACE  Link_Code  WITH  &lcFcLnkTmp..Link_Code
        UNLOCK
      ELSE
        INSERT INTO Factor (cFacCode,Link_Code) VALUES (laData[1],&lcFcLnkTmp..Link_Code)
        =gfAdd_Info('Factor')
      ENDIF     
      
    ENDSCAN
  ENDIF
 *E500374,1 (End)

ENDIF
SELECT SYCFACT

*!*************************************************************
*! Name      : lfvFacLnk
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 09/11/2000
*! Purpose   : Valid funtion for the factor GL link code 
*!           : in the Factor Screen .
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFacLnk()
*!*************************************************************
FUNCTION lfvFacLnk
PRIVATE lcOAlias
lcOAlias=ALIAS()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrwTitl)
lcFacCode=laData[1]
lcFacName=laData[2]
STORE SPACE(0) TO lcOldValue
lcBsWind=gfTempName()
lcBrwCont=gfTempName()
lcPushB=gfTempName()
lcBmpOK= gcBmpHome + "OK.bmp"
lcBmpCncl= gcBmpHome + "can.bmp"
lcMainTtl='Factor Gl link code'
lcBrwTitl='Gl link codes' 


IF llFillData
  SELECT SycComp
  SCAN
    IF gfGetMemVar('M_LINK_GL',cComp_Id)='Y'
      SCATTER MEMVAR 
  
      IF USED('Factor')
        USE IN FACTOR
      ENDIF  
      =gfOpenFile(lcDataPath+ALLTRIM(cComp_Id)+'\Factor',lcDataPath+ALLTRIM(cComp_Id)+'\Factor','SH')
      lcFacLnk= IIF (SEEK (lcFacCode,'Factor'),Factor.Link_CODE  ,'')
    
      SELECT  (lcFclnkTmp)
      APPEND BLANK
      REPLACE cComp_Id   WITH  m.cComp_Id ,;
              cCom_Name  WITH  m.cCom_Name,;  
              cFacCode   WITH  lcFacCode  ,;    
              Link_Code  WITH  lcFacLnk 
    ENDIF            
  ENDSCAN
ENDIF

DO (gcScrDir + gcWinAppl + '\SMFcLnk.SPR')
SELECT (lcOAlias)
*!*************************************************************
*! Name      : lfBrow
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 09/11/2000
*! Purpose   : Browse function .
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrow()
*!*************************************************************
FUNCTION lfBrow
SELECT  (lcFcLnkTmp)
GO TOP  
lclnkBRW =":H='Link code' :15:V=lfvLnkCode():P='XXXXXX'"+IIF(laScrMode[4] OR laScrMode[3],'',':R')  

BROWSE FIELDS                             ;
       cComp_Id  :H='Company code' :18 :R,;
       cCom_Name :H='Company name' :40 :R,;
       Link_Code  &lclnkBRW               ; 
       WINDOW    (lcbrwcont)              ;
       IN WINDOW (lcBsWind)               ;
       NOMENU                             ;         
       NOAPPEND                           ;
       NODELETE                           ;         
       NOWAIT                             ;
       SAVE                               ;
       NOCLEAR                            ;
       WHEN      lfwBrwLnk()              ; 
       FOR       cFacCode= lcFacCode      ;  
       TITLE     (lcBrwTitl)


*!*************************************************************
*! Name      : lfwBrwLnk
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 09/11/2000
*! Purpose   : Validate  link code
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrwLnk()
*!*************************************************************
FUNCTION  lfwBrwLnk
PRIVATE lcLinkCode
lcOldValue=Link_Code
IF USED ('Gl_Link')
  USE IN GL_Link 
ENDIF
=gfOpenFile(lcDataPath+ALLTRIM(cComp_Id)+'\GL_Link',lcDataPath+ALLTRIM(cComp_Id)+'\GL_Link','SH')  
FUNCTION lfvLnkCode
IF !SEEK('01'+LinK_Code,'Gl_Link')
  lcLinkCode = LinK_Code
  =RLOCK()
  REPLACE link_CODE WITH IIF(gfGLBrowse('01',@lcLinkCode),lcLinkCode,lcOldValue)
  UNLOCK
ENDIF

*!**********************************************************
*! Name      : lfvOkLnk
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 09/11/2000
*! Purpose   : Valid function for the Ok button 
*!             in the factor link code screen .
*! Ref       : E500374,1
*!**********************************************************
*! Calls     : None
*!**********************************************************
*! Passed Parameters  :  
*!**********************************************************
*! Returns            :  None
*!**********************************************************
*! Example            :  =lfvOkLnk()
*!**********************************************************
FUNCTION lfvOkLnk
llFillData=.F.

*!**********************************************************
*! Name      : lfvCnclLnk
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 09/11/2000
*! Purpose   : Valid function for the cancel button 
*!             in the factor link code screen .
*! Ref       : E500374,1
*!**********************************************************
*! Calls     : None
*!**********************************************************
*! Passed Parameters  :  llFrmScr To check if it was called
*!                       from the screen or from lpShow
*!**********************************************************
*! Returns            :  None
*!**********************************************************
*! Example            :  =lfvCnclLnk()
*!**********************************************************
FUNCTION lfvCnclLnk
PARAMETERS llFrmScr
PRIVATE lcOAlis
lcOAlis=Alias()
SELECT (lcFclnkTmp)
DELETE ALL
llFillData=.T.

IF !llFrmScr
  CLEAR READ
ENDIF
SELECT (lcOAlis)
*!*************************************************************
*! Name      : lfFactTmp
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 09/11/2000
*! Purpose   : Create the temp file for the factor link code.
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfFactTmp()
*!*************************************************************
FUNCTION lfFactTmp

PRIVATE  lcOAlias
lcOAlias= ALIAS()
PRIVATE laFileStru
DECLARE laFileStru [4,4]
laFileStru[1,1] = 'cComp_Id'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 2
laFileStru[1,4] = 0
laFileStru[2,1] = 'cCom_Name'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 30
laFileStru[2,4] = 0
laFileStru[3,1] = 'cFacCode'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 6
laFileStru[3,4] = 0
laFileStru[4,1] = 'Link_Code'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 6
laFileStru[4,4] = 0
=gfCrtTmp(lcFclnkTmp,@laFileStru,[cFacCode],lcFclnkTmp) 
SELECT(lcOAlias) 
*!*************************************************************
*! Name      : lfTrap
*! Developer : NAD
*! Date      : 09/11/2000
*! Purpose   : Trap the Tab ,BackTab ,Esc Keys
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTrap()
*!*************************************************************
FUNCTION lfTrap

IF WONTOP() = lcBrwTitl 
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpBackTab	  
ENDIF
*ON KEY LABEL ESCAPE  DO lpEsc
RETURN .F.
*!*************************************************************
*! Name      : lfClrTrap
*! Developer : NAD
*! Date      : 09/11/2000
*! Purpose   : Clear the  Trap
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrTrap()
*!*************************************************************
FUNCTION lfClrTrap

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*!*************************************************************
*! Name      : lpTab
*! Developer : NAD
*! Date      : 09/11/2000
*! Purpose   : the Tab Key
*! Ref       : E500374,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpTab()
*!*************************************************************

PROCEDURE lpTab
ON KEY LABEL TAB
IF WONTOP() = lcBrwTitl  
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbOk)  
ENDIF
*!*************************************************************
*! Name      : lpBackTab
*! Developer : NAD
*! Date      : 09/11/2000
*! Purpose   : The BackTab Key
*! Ref       : E500374,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpBackTab()
*!*************************************************************

PROCEDURE lpBackTab
ON KEY LABEL BACKTAB
IF WONTOP()=lcBrwTitl
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbCncl) 
ENDIF




