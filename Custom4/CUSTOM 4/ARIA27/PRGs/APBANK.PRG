*:************************************************************************
*:
*: Procedure file: APBANK.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow
*:               : lfvData_1
*:               : lpSavScr
*:               : lpDelScr
*:               : lfvNew
*:               : lfvRemove
*:               : lfwChecks
*:               : lfvChkCode
*:               : lfvNxtMan
*:               : lfvNxtPrn
*:               : lfUpdTemp
*:               : lfwAccount
*:               : lfvAccount
*:               : lfwChkAcct
*:               2 lfvChkAcct
*:               : lfDiplObjs
*:               : lfAssgObjs
*:               : lfwCurrncy
*:               : lfvCurrncy
*:
*:      Documented 06/02/1994
*:************************************************************************
*E300266,5 MAN 08/08/95 Default the address code with company default.
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300921,1 TAK 07/12/98 Added changes due to Point of Sale module (cash register).
*E300921,1              Added a bank type (Bank/Safe) and GL Cash account
*E300921,1              which used only in (S)afe Case. 
*E300921,1              If the bank type is safe ,select the safe users
*E300921,1              insted of checking accounts. 
*E300921,1              Changed the path of the screen due to sharing.
*E301077,71 AMM 02/18/99 Reduce Opened files.
*B603052,1 AKA 07/05/99 Fix bug invalid function arg. value , type , count
*C102057,1 ABD 12/04/2000 Make change on the 'Cash Register Setup' screen
*C102057,1 ABD            Upon use in the Ps Module.
*B606184,1 ALB 07/18/2002 No save bank record without at least one gl account
*:************************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
*--hesham
*DECLARE laKeyField [1,4],laFileStru[1],laWndObj  [3,3]
DECLARE laKeyField [1,4],laFileStru[1],laWndObj  [3,3],laCountry[1,2]

*E300921,1 TAK (Start) Declare two arrays for bank types and selected users.
DECLARE laBType[2,2],laSafeUsrs[1]
laBType[1,1] = 'Bank'
laBType[1,2] = 'B'
laBType[2,1] = 'Safe'
laBType[2,2] = 'S'
laSafeUsrs = ''
lnBnkTyp   = 1    &&Variable hold 1 for bank 2 for safe.
*--Check the safe users and user safes.
*E301077,71 AMM Move this part after calling gfsetup()
*=gfOpenFile(gcSysHome+'sycinst','','SH')
*GO TOP IN SYUUser
*--If there is a users defined in the system and login required.
*llUsersExt = !EOF('SYUUser') AND SYCINST.lInsLogRq   
*E301077,71 AMM end
** lcCshAcct      Variable to hold the Gl cash account.

*--Check of needed Installed modules.
llApLink   = (OCCURS('AP',gcCmpModules)<>0)
*--Open this files with AP installed only.
*E301077,71 AMM Open files when needing to them.
*IF llApLink
  *=gfOpenFile(gcDataDir+'APSETUP','','SH')
  *=gfOpenFile(gcDataDir+'APINVAHD','HTYPCOD','SH')
  *=gfOpenFile(gcDataDir+'APINVHDR',' ','SH')
*ENDIF
*E301077,71 AMM end
*E300921,1 (End).

** lcTmpCheck   Variable to hold the temp. file name.
** lcChecks     Variable to hold the string of the list.
** lcChkCode    Variable to hold the check code.
** lcChkCodSt   Variable to hold the check code status (enable or disable)
** lcChkLnDes   Variable to hold the long description.
** lcChkShDes   Variable to hold the short description.
** lcObjStat    Variable to hold the status on the objects.
** lcOldAcc     Variable to hold the old account.
** lcChkAcct    Variable to hold the check account. 
** lcDiscAcct   Variable to hold the account description.
** lcAdjAcct    Variable to hold the adjust account.
** lcTCheck     Variable to hold

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
** lcOldCurr    Variable to hold the old currency.  && Added by Mohamed Hassan
** lcCurrency   Variable to hold the currency.      && Added by Mohamed Hassan
**
** llNewChk     Variable to indicate that there is a new check added.
** llBrowse     Variable to activate the browse.
** llChild      Variable to 
**
** ldLstPrChk   Variable to hold the last printed check date.
** ldLstMaChk   Variable to hold the last manual  check date.
**
** lnLstPrChk   Variable to hold the last printed Check amount.
** lnLstMaChk   Variable to hold the last manual  Check amount.
** lnChkNxtMN   Variable to hold the next check no. manual.
** lnChkNxtPN   Variable to hold the next check no. printed.
** lnNoOfRec    Variable to hold the no of records in the list
** lnOldValue   Variable to 
** lsChecks     Variable to hold the no of the list
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'BANKCODE'
laKeyField[1,4] = 1

laDefProc[7] = .F.       && Use the custom delete procedure.
laDefProc[9] = .F.       && Use the custom save procedure.
lcTCheck     = 'Checking accounts'

STORE .F.        TO llNewChk   , llBrowse
STORE .T.        TO llChild    
STORE {}         TO ldLstPrChk , ldLstMaChk
STORE 0.00       TO lnLstPrChk , lnLstMaChk
STORE 0          TO lnChkNxtMN , lnChkNxtPN , lnNoOfRec , lnOldValue
STORE 1          TO lsChecks  
STORE lcEmptyAcc TO lcChkAcct  , lcDiscAcct , lcAdjAcct , lcCshAcct

STORE ' '        TO lcTmpCheck , lcChecks   , lcChkCode , lcChkCodSt,;
                    lcChkLnDes , lcChkShDes , lcObjStat , lcOldAcc  ,;
                    lcOldCurr  , lcCurrency 

IF !gfSetup()    
  RETURN
ENDIF  
*E301077,71 AMM start
=gfOpenFile(gcDataDir+'APBANKS','BANKCODE','SH')
=gfOpenFile(gcDataDir+'APCHECKS','BANKCHECK','SH')

llOpinst = gfSysOpen(gcSysHome+'sycinst','','SH')
*IHB open syuuser with index [start]
*=gfOpenFile(gcSysHome+'SYUUser','','SH')
=gfOpenFile(gcSysHome+'SYUUser','Cuser_id','SH')
*IHB open syuuser with index [end]
GO TOP
*--If there is a users defined in the system and login required.
llUsersExt = !EOF('SYUUser') AND SYCINST.lInsLogRq   
IF llOpinst
  =gfSysClose('sycinst')
ENDIF
*E301077,71 AMM end


*C102057,1 ABD -Make change on the 'Cash Register adjustment screen
*C102057,1 ABD -Upon use in the Ps Module. [Begin]
*- If call from PS module.
IF gcWinAppl = "PS"
  llFromPs   = .T.
  lcWindTitl = "Cash Register Setup"
  lcButtontl = '\<Users'
  lnBnkTyp   = 2    &&Variable hold 1 for bank 2 for safe.
ELSE && call from AP Module.
  llFromPs   = .F.  
  lcButtontl = '\<Checking accounts...'
ENDIF
*C102057,1 ABD [End]


laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "LADATA[1]"
laWndObj [1,3] = "PBCHKACS"

laWndObj [2,1] = "CWDAPCHECK"
laWndObj [2,2] = "LSCHECKS"
laWndObj [2,3] = "PBCLOSE"

laWndObj [3,1] = "GWCCONTRL1"
laWndObj [3,2] = "PBTOP"
laWndObj [3,3] = "PBCLS"

IF !WEXIST(gcBaseWind)
  lcTmpCheck = gfTempName()  && Get a random name for the temp. file.
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  ** Create the Temp file from array **
  SELECT APCHECKS
  =AFIELDS(laFileStru)

  lnArrPos = ASUBSCRIPT(laFileStru,ASCAN(laFileStru,'DCHKLPDAT'),1)
  =gfAdel(@laFileStru,lnArrPos,1)

  lnArrPos = ASUBSCRIPT(laFileStru,ASCAN(laFileStru,'DCHKLMDAT'),1)
  =gfAdel(@laFileStru,lnArrPos,1)

  lnArrPos = ASUBSCRIPT(laFileStru,ASCAN(laFileStru,'NCHKLPAMT'),1)
  =gfAdel(@laFileStru,lnArrPos,1)

  lnArrPos = ASUBSCRIPT(laFileStru,ASCAN(laFileStru,'NCHKLMAMT'),1)
  =gfAdel(@laFileStru,lnArrPos,1)

  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'NRECNO'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 4
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0

  *** Creat temp Fiscal Header file from the array.
  CREATE TABLE &gcWorkDir.&lcTmpCheck FROM ARRAY laFileStru

  SELECT(lcTmpCheck)
  DO CASE
    CASE _DOS
      ** The display of the list under DOS **
      lcChecks ="&lcTmpCheck..CCHKACCT+'  '+"+;
                "&lcTmpCheck..CCHKGLACC+' '+"+;
                "STR(&lcTmpCheck..NCHKNXTPN)+'        '+"+;
                "STR(&lcTmpCheck..NCHKNXTMN)"
    CASE _WINDOWS
      ** The display of the list under WINDOWS **
      lcChecks ="&lcTmpCheck..CCHKACCT+' '+"+;
                "&lcTmpCheck..CCHKGLACC+' '+"+;
                "STR(&lcTmpCheck..NCHKNXTPN)+'    '+"+;
                "STR(&lcTmpCheck..NCHKNXTMN)"
  ENDCASE
ENDIF

*MAN Open the file before the SQL
=gfOpenFile(gcSysHome+"SYCINT",'Ccontcode','SH')
*--hesham el-sheltawi    start
SELECT sycint.ccont_desc,sycint.ccont_code ;
       FROM (gcSysHome+"SYCINT") ;
       INTO ARRAY laCountry
IF _TALLY = 0
  laCountry = " "
ENDIF
*E300266,5 MAN 08/08/95 Default the address code with company default
*puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[12]),'USA',laData[12]),sycint.ccont_code,'CCONTCODE')     
puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[12]),gcContCode,laData[12]),sycint.ccont_code,'CCONTCODE')
*--hesham el-sheltawi    end

SELECT APCHECKS
SET ORDER TO TAG BANKCHECK 
SELECT (lcTmpCheck)
SET RELATION TO &lcTmpCheck..cbnkcode + &lcTmpCheck..cchkacct INTO APCHECKS ADDITIVE

SELECT APBANKS
*E300683,1 Call *.SPR from screens directory
* DO Apbank.SPR 
*E300921,1 Change path due to sharing of this screen.
*DO (gcScrDir + gcWinAppl + '\Apbank.SPR')
DO (gcScrDir + 'Apbank.SPR')
*E300683,1 end          

SELECT (lcTmpCheck)
SET RELATION TO 
 
IF glQuitting
  *** If quit from this screen close its child screen. ***
  IF WEXIST('CWDAPCHECK')
    RELEASE WINDOW CWDAPCHECK
  ENDIF
  
  *** ERASE the Temp. file which hold the Checks.
  IF USED(lcTmpCheck)
    USE IN &lcTmpCheck
  ENDIF
  ERASE &gcWorkDir.&lcTmpCheck+'.DBF'
  ERASE &gcWorkDir.&lcTmpCheck+'.CDX'
  ERASE &gcWorkDir.&lcTmpCheck+'.FPT'
ENDIF  
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode

DO CASE 
  CASE laScrMode[1]    && Select mode.

    ** If the child window is open so we are going to close it because
    ** there is no need to live it open in the select mode.

    IF WVISIBLE('CWDAPCHECK')
      =gfChClose('CWDAPCHECK')
    ENDIF  
    ** Select the Temp file and zap the old record data
    SELECT(lcTmpCheck)
    ZAP
    ** Assign the list variable to 1
    lsChecks  = 1
    ** Disable the chiled screen objects.
    lcObjStat = 'DISABLE'
    ldLstPrChk = {}    
    ldLstMaChk = {}    
    lnLstPrChk = 0.00  
    lnLstMaChk = 0.00  


    *E300921,1 TAK (Start) In Select mode reset the safe type setings
    *E300921,1 to bank type (default) and clear the selected user array.
    lcCshAcct  = lcEmptyAcc
    *C102057,1 ABD change the default to be 2 if come from Ps Module. [Begin]
    IF llFromPs
      lnBnkTyp   = 2
    Else
      lnBnkTyp   = 1
    ENDIF
    *C102057,1 ABD  [End]
    *C102057,1 ABD change check to be if come from Ps module. [Begin]
    *lcTxtToPrmt = IIF(lnBnkTyp=2,'\<Users...','\<Checking accounts...')
    lcTxtToPrmt = IIF(lnBnkTyp=2 .OR. llFromPs,'\<Users...','\<Checking accounts...')
    *C102057,1 ABD [End]
    SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt DISABLE
    *--Clear users array.
    DIMENSION laSafeUsrs[1]
    laSafeUsrs = ''
    *E300921,1 (End).
    
  CASE laScrMode[2] .OR. laScrMode[3]    && View or Edit mode.

    *E300921,1 TAK (Start) If type is Safe read safe users.
    IF laScrMode[2]
      lcCshAcct  = lcEmptyAcc
    ENDIF
    laData[13] = IIF(EMPTY(laData[13]),'B',laData[13])
    *C102057,1 ABD  check only if you are not call from Ps modules.  [Begin]
    IF llFromPs
      lnBnkTyp = 2
    ELSE
      lnBnkTyp = ASUBSCRIPT(laBType,ASCAN(laBType,laData[13]) , 1)
    ENDIF
    *C102057,1 ABD change check to be if come from Ps module. [Begin]
    *lcTxtToPrmt = IIF(lnBnkTyp=2,'\<Users...','\<Checking accounts...')
    lcTxtToPrmt = IIF(lnBnkTyp=2 .OR. llFromPs,'\<Users...','\<Checking accounts...')
    *C102057,1 ABD [End]
    IF laData[13] = 'S' AND !llUsersExt
      SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt DISABLE
    ELSE
      SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt
    ENDIF
    IF laData[13] = 'S'
      *--Read the Gl cash account.
      IF laScrMode[2] AND SEEK(laData[1],'APCHECKS')
        lcCshAcct = APCHECKS.cChkGLAcc
      ENDIF
       
      *--Define the safe users. 
      IF laScrMode[2] AND !EMPTY(APBANKS.mSafeUsers)
        lcSfUserStr = APBANKS.mSafeUsers
        lnStrLn=1
        FOR lnCnt = 1 TO INT(LEN(lcSfUserStr)/11)
          lcSfUsr = SUBSTR(lcSfUserStr,lnStrLn,10)
          IF !EMPTY(lcSfUsr) 
            =SEEK(lcSfUsr,'SYUUSER')
            DIMENSION laSafeUsrs[lnCnt]
            laSafeUsrs[lnCnt] = SYUUSER.cUsr_Name+'    '+SYUUSER.cUser_id
            lnStrLn =lnStrLn + 11 
          ELSE
            EXIT        
          ENDIF  
        ENDFOR 
      ENDIF 
    *E300921,1 (End).
 
    ELSE
      ** We are going to collect all the checks for the bank.
      SELECT cBnkCode,cChkAcct,cChkShDes,cChkGlAcc,cDiscAcct, ;
        cAdjAcct,nChkNxtPn,nChkNxtMn,cCurrCode,;
        RECNO() AS 'nRecNo',"S" AS 'cStatus';
        FROM &gcDataDir.APCHECKS;
        WHERE CBNKCODE+CCHKACCT = laData[1];
        ORDER BY cChkAcct;
        INTO DBF &gcWorkDir.&lcTmpCheck

      ** Select the Temp file and the store the no of records in a variable.
      SELECT(lcTmpCheck)
      SET RELATION TO &lcTmpCheck..cbnkcode + &lcTmpCheck..cchkacct INTO APCHECKS ADDITIVE

       lnNoOfRec  = RECCOUNT()   && Variable to hold the no of records in the Temp file.
      lsChecks   = 1            && Assign the variable of the list to record no 1.
    ENDIF

*-- hesham el-sheltawi          start
    FOR lnCount1 = 1 TO ALEN(laCountry,1)
      IF ALLTRIM(laData[8]) = ALLTRIM(laCountry[lnCount1,2])
        DO CASE
          CASE _DOS
            lcCountry = laCountry[lnCount1,1]
          CASE _WINDOWS
             *E300266,5 MAN 08/08/95 Default the address code with company default
             *puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[12]),'USA',laData[12]),sycint.ccont_code,'CCONTCODE')     
             puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[12]),gcContCode,laData[12]),sycint.ccont_code,'CCONTCODE')     
             
        ENDCASE
        EXIT
      ENDIF
    ENDFOR
*-- hesham el-sheltawi          end

    ** If Edit mode & no of records of the Temp file is > 0 we are going
    ** to ENABLE the objects of the child screen else we are going to DISABLE it.
    lcObjStat  = IIF(laScrMode[3] .AND. lnNoOfRec > 0,'ENABLE','DISABLE')
    =lfAssgObjs()


  CASE laScrMode[4]    && Add mode.

  *--hesham el-sheltawi      start
    DO CASE
      CASE _DOS
        lcCountry = laCountry[1,1]
      CASE _WINDOWS
         *E300266,5 MAN 08/08/95 Default the address code with company default
         *puCountry = LOOKUP(SYCINT.CPARt1LAB,'USA',sycint.ccont_code,'CCONTCODE')     
         puCountry = LOOKUP(SYCINT.CPARt1LAB,gcContCode,sycint.ccont_code,'CCONTCODE')

    ENDCASE
    *E300266,5 MAN 08/08/95 Default the address code with company default
    *laData[8]  = LOOKUP(SYCINT.cCont_Desc,'USA',sycint.ccont_code,'CCONTCODE')      
    laData[8]  = LOOKUP(SYCINT.cCont_Desc,gcContCode,sycint.ccont_code,'CCONTCODE')      
    SHOW GET laData[8]
  *--hesham el-sheltawi      end

    *E300921,1 TAK (Start) Read the bank type field.
    laData[13]  = IIF(lnBnkTyp=2,'S','B')
    *C102057,1 ABD change check to be if come from Ps module. [Begin]
    *lcTxtToPrmt = IIF(lnBnkTyp=2,'\<Users...','\<Checking accounts...')
    lcTxtToPrmt = IIF(lnBnkTyp=2 .OR. llFromPs,'\<Users...','\<Checking accounts...')
    *C102057,1 ABD [End]

    IF laData[13] = 'S' AND !llUsersExt
      SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt DISABLE
    ELSE
      SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt
    ENDIF
    *E300921,1 (End).

    SELECT(lcTmpCheck)
    lnNoOfRec = RECCOUNT()  && Assign the no of records in the Temp file to a variable.
    lsChecks  = 1           && Assign the variable of the list to 1
    lcObjStat = 'DISABLE'   && Disable the objects of the child screen.
ENDCASE   

** If there is no records in the Temp file so we are going to DISABLE
** the remove button in the child screen.
IF lnNoOfRec = 0
  SHOW GET pbRemove DISABLE
ELSE   
  ** If Add or Edit mode so we are going to enable the Remove Button.
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET pbRemove ENABLE
  ELSE
    ** If View mode so we are going to Disable the Remove Button.
    SHOW GET pbRemove DISABLE
  ENDIF  
ENDIF  

IF laScrMode[3] .OR. laScrMode[4]
  SHOW GET pbNew ENABLE
ELSE
  SHOW GET pbNew DISABLE
ENDIF

    
** Enable or Disable the child screen objects upon the variable
** that we assign befor.

=lfDiplObjs(lcObjStat)

*E300921,1 TAK (Start) Disable or enabel the Gl cash account object.
lcObjDEbl = IIF(laData[13] = 'S' AND (laScrMode[3] OR laScrMode[4]),'ENABLE','DISABLE')
SHOW GET ibGlCshAcc &lcObjDEbl
SHOW GET lcCshAcct  &lcObjDEbl
*E300921,1 (End).

SHOW GET pbClose    ENABLE
*** Disable error handler until the list is refreshed,then
*** Enable it again
lcErrSett      = ON("ERROR")
ON ERROR lnDum = 1
SHOW GET lsChecks   ENABLE
ON ERROR &lcErrSett.

SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

*C102057,1 ABD- set filter to safe only if come from PS module. [Begin]
IF llFromPs
  PRIVATE lnAlias , lcOldFiltr
  lnAlias = SELECT(0)
  lcOldFiltr = FILTER()
  SET FILTER TO cBnkType = 'S'  
ENDIF
*C102057,1 ABD [End]

laData[1] = PADR(ALLTRIM(laData[1]), FSIZE('cBnkCode','APBANKS'))

IF llBrowse .OR. ATC("?",laData[1]) > 0
  llBrowse = .F.
  =gfBrows()  
ELSE
  IF !EMPTY(laData[1]) AND LASTKEY()= 13
    =gfSeekRec()
  ENDIF
ENDIF

SHOW GET laData[1]

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"

PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode    

*** Added By : Malak Hanna
*E300921,1 TAK (Start) Check for existing of the Gl cash account in case
*E300921,1 of type is Safe.
*IF lnNoOfRec = 0  

*B606184,1 ALB Add condition if there is any empty accounts [Begin]
IF (lnNoOfRec = 0  AND laData[13] = 'B') OR ;
   (lcCshAcct  = lcEmptyAcc  AND laData[13] = 'S') 
*B606184,1 ALB Add condition if there is any empty accounts [End]   

*IF (lnNoOfRec = 0  AND laData[13] = 'B') OR ;
   (lcCshAcct  = lcEmptyAcc  AND laData[13] = 'S')
  IF laData[13] = 'S'
    *** You have to enter the GL Cash account. 
    =gfModalGen("TRM04165B00000","DIALOG")  
*E300921,1 (End).
  ELSE
    *** You have to enter at least one checking account. 
    =gfModalGen("TRM04054B00000","DIALOG")  
  ENDIF 
  *B606184,1 ALB Let user be able to enter the Gl account [Begin]
  *lcChkCode  = SPACE(12)
  lcChkAcct  = lcEmptyAcc
  SHOW GET lcChkCode DISABLE
  SHOW GET lcChkAcct ENABLE 
  *SHOW GET lcChkAcct DISABLE
  *B606184,1 ALB Let user be able to enter the Gl account[End]
  
  SHOW GET pbNew ENABLE
  llCSave = .F.
  RETURN 
ENDIF
*B606184,1 ALB Massage if the user enter no Gl account [Begin]
IF (laData[13] = 'B' AND !lfChkGlAcct())
  =gfModalGen(.F.,'DIALOG',.F.,.F.,'You have to enter the GL account')
  lcChkAcct  = lcEmptyAcc
  SHOW GET lcChkCode DISABLE
  SHOW GET lcChkAcct ENABLE 
  llCSave = .F.
  RETURN
ENDIF
*B606184,1 ALB Massage if the user enter no Gl account [End]
SELECT APBANKS

IF laScrMode[4]    && Add mode
  ** If we are in the add mode we are going to add a new blank record to
  ** add the new data.
  APPEND BLANK
ENDIF
  
GATHER FROM laData FIELDS &lcScFields MEMO && Add the new data to the record.

=gfAdd_Info()  && Add the audit information to the record.

*E300921,1 TAK (Start) Update Selected Users if bank type is Safe.
IF laData[13] = 'S'
  *E300921,1 field (mSafeUsers) for array laSafeUsrs.
  IF !EMPTY(laSafeUsrs[1])  &&User was selected.
    lcSfUsrStr = ''
    FOR lnCnt = 1 TO ALEN(laSafeUsrs,1)
      lcSfUsrStr = lcSfUsrStr + PADR(SUBSTR(laSafeUsrs[lnCnt],40,10),10)+'|'
    ENDFOR
    REPLACE mSafeUsers WITH lcSfUsrStr
  ENDIF

  *--Update GL Cash Account.
  SELECT APCHECKS
  IF laScrMode[4]
    APPEND BLANK
    REPLACE cBnkCode  WITH laData[1],;
            cChkAcct  WITH 'CASH REGISTR',;
            cChkGLAcc WITH lcCshAcct,;
            cCurrCode WITH gcBaseCurr
    =gfAdd_Info()  && Add the audit information to the record.
  ELSE
    IF SEEK(laData[1])
      REPLACE cChkAcct  WITH 'CASH REGISTR',;
              cChkGLAcc WITH lcCshAcct
    ENDIF
  ENDIF
*E300921,1 (End).
ELSE

  SELECT(lcTmpCheck)
  =gfTmp2Mast('APCHECKS',lcTmpCheck)  && Function to add or update the temp file.

ENDIF
SELECT APBANKS

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
* This procedure replaces the default Delete procedure for the push button
* "DELETE"
PROCEDURE lpDelScr

EXTERNAL ARRAY laScrMode
*E300921,1 (Start) Make this checks only if Ap installed.
IF llApLink
*E300921,1 (End).
  *E301077,71 AMM Open file
  *SELECT APSETUP
  *GO TOP
  =gfOpenFile(gcDataDir+'APSETUP',' ','SH')  
  *E301077,71 AMM end
  
  IF cBnkCode = laData[1]   
    ** Message : " You cannot delete the default bank. "
    **           "                  ® Ok ¯             "
    *** 
    =gfModalGen("TRM04050B00000","DIALOG")  
    SELECT APBANKS
    RETURN 
  ENDIF

  *E301077,71 AMM Open file
  *SELECT APINVHDR
  =gfOpenFile(gcDataDir+'APINVHDR',' ','SH')
  *E301077,71 AMM end
  LOCATE FOR CBNKCODE = laData[1]
  IF FOUND()
    ** Message : " Invoice no ð for vendor ð is  "
    **           " approved to be paid from ð."
    **           "               ® Ok ¯          "
    lcInvNo  = CINVNO
    lcVendor = CVENDCODE
    =gfModalGen("TRM04093B00000","DIALOG",ALLTRIM(LCINVNO)+"|"+ALLTRIM(LCVENDOR)+"|"+'bank')
    SELECT APBANKS
    RETURN
  ENDIF
  *E301077,71 AMM
  *SELECT APINVAHD
  =gfOpenFile(gcDataDir+'APINVAHD','HTYPCOD','SH')
  *E301077,71 AMM end
  LOCATE FOR CBNKCODE = laData[1]
  IF FOUND()
    ** Message : " ð for vendor ð is using this ð. "
    **           "                ® Ok ¯           "
    lcRecInst = IIF(CAUTMTYPE = 'R','Recurring code','Instalment code')
    lcRecCode = lcRecInst+' '+CAUTMCODE
    lcVendor  = CVENDCODE
    SELECT APBANKS
    =gfModalGen("TRM04151B00000","DIALOG",ALLTRIM(lcRecCode)+"|"+ALLTRIM(lcVendor)+"|bank")
    RETURN
  ENDIF
ENDIF
*E301077,71 AMM Open file 
*SELECT APPAYMNT
=gfOpenFile(gcDataDir+'APPAYMNT','','SH')
*E301077,71 AMM end
LOCATE FOR CBNKCODE = laData[1]
IF FOUND()
  ** Message : " Payment no ð is exist for ð. "
  **           "               ® Ok ¯         "
  lcPayment = CPAYDOCNO
  lcBank    = laData[1]  
  SELECT APBANKS
  =gfModalGen("TRM04092B00000","DIALOG",ALLTRIM(lcPayment)+"|"+'bank '+ALLTRIM(lcBank))
  RETURN
ENDIF

SELECT (lcTmpCheck)
 
*** Delete all Style Number for this Style
REPLACE ALL cStatus WITH 'D'

DELETE ALL

=gfTmp2Mast('APCHECKS',lcTmpCheck)  && Function to delete the records from the Temp file.

*** Delete the user record from the user file
SELECT APBANKS
** Make the deleted record blank and then delete.
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
DELETE
** Change the screen mode to the Select mode.
laScrMode    = .F.
laScrMode[1] = .T.

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
* Function to add new checks in the child screen.
*
FUNCTION lfvNew

** Make the objects of the child screen empty.
lcChkCode  = SPACE(12)
lcChkLnDes = SPACE(40)
lcChkShDes = SPACE(15)
lnChkNxtMN = 1
lnChkNxtPN = 1
lcChkAcct  = lcEmptyAcc
lcDiscAcct = lcEmptyAcc
lcAdjAcct  = lcEmptyAcc
lcCurrency = SPACE(5)
ldLstPrChk = {}
ldLstMaChk = {}
lnLstPrChk = 0.00
lnLstMaChk = 0.00

** Show the check code object & the remove button enable and disable the 
** rest of the objects till the user enter a valid check cosde.

=lfDiplObjs('DISABLE')
llNewChk= .T.
SHOW GET lcChkCode  ENABLE
lcChkCodSt = 'ENABLE'
SHOW GET pbRemove   DISABLE
SHOW GET pbNew      DISABLE

_CUROBJ = OBJNUM(lcChkCode)

SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfvRemove
*!
*!**************************************************************************
* Fuction to remove the records from the checks screen.
*
FUNCTION lfvRemove
*E300921,1 (Start) Make this checks only if Ap installed.
IF llApLink
*E300921,1 (End).
  *E301077,71 AMM Open file
  *SELECT APSETUP
  *GO TOP
  =gfOpenFile(gcDataDir+'APSETUP',' ','SH')  
  *E301077,71 AMM end
  IF cBnkCode = &lcTmpCheck..cBnkCode .AND. ;
     cChkAcct = &lcTmpCheck..cChkAcct
    ** MESSAGE : "You cannot remove the default "
    **           "checking account.             "
    **           "            ® Ok ¯            "
    SELECT (lcTmpCheck)
    =gfModalGen("TRM04049B00000","DIALOG")     
    RETURN
  ENDIF
  *E301077,71 AMM Open file
  *SELECT APINVHDR
  =gfOpenFile(gcDataDir+'APINVHDR',' ','SH')
  *E301077,71 AMM end
  LOCATE FOR CBNKCODE = laData[1] .AND. CCHKACCT = &lcTmpCheck..cChkAcct
  IF FOUND()
    ** Message : " Invoice no ð for vendor ð   "
    **           " is approved to be paid from "
    **           " this ð.                 "
    **           "               ® Ok ¯        "
    lcInvNo  = CINVNO
    lcVendor = CVENDCODE
    =gfModalGen("TRM04093B00000","DIALOG",ALLTRIM(LCINVNO)+"|"+ALLTRIM(LCVENDOR)+"|"+'check')
    SELECT (lcTmpCheck)
    RETURN
  ENDIF

  *E301077,71 AMM Open file
  *SELECT APINVAHD
  =gfOpenFile(gcDataDir+'APINVAHD','HTYPCOD','SH')
  *E301077,71 AMM end
  LOCATE FOR CBNKCODE = laData[1] .AND. CCHKACCT = &lcTmpCheck..cChkAcct
  IF FOUND()
    ** Message : " ð for vendor ð is using this ð. "
    **           "                ® Ok ¯           "
    lcRecInst = IIF(CAUTMTYPE = 'R','Recurring code','Instalment code')
    lcRecCode = lcRecInst+' '+CAUTMCODE
    lcVendor  = CVENDCODE
    =gfModalGen("TRM04151B00000","DIALOG",ALLTRIM(lcRecCode)+"|"+ALLTRIM(lcVendor)+"|check")
    SELECT (lcTmpCheck)
    RETURN
  ENDIF
ENDIF

*E301077,71 AMM Open file
*SELECT APPAYMNT
=gfOpenFile(gcDataDir+'APPAYMNT','','SH')
*E301077,71 AMM end
LOCATE FOR CBNKCODE = laData[1] .AND. CCHKACCT = &lcTmpCheck..cChkAcct
IF FOUND()
  ** Message : " Payment no ð is exist for ð. "
  **           "               ® Ok ¯         "
  lcPayment = CPAYDOCNO
  lcCheck   = &lcTmpCheck..cChkAcct
  =gfModalGen("TRM04092B00000","DIALOG",ALLTRIM(lcPayment)+"|"+'check account '+ALLTRIM(lcCheck))
   SELECT (lcTmpCheck)
  RETURN
ENDIF

** MESSAGE : " Are you sure you want to remove it ? **
**           "     ®  Yes  ¯       <  No  >         **

IF gfModalGen("QRM00007B00007","ALERT") = 1

  SELECT(lcTmpCheck)
  ** If the record status is 'S'ame or 'M'odified so we are going to
  ** assign the status to 'D'eleted, but if the record is 'A'dded we
  ** are going assign the status to 'S'ame.
    
  REPLACE cStatus WITH SUBSTR('DDD',AT(cStatus,'SMA'),1)
  ** Delete this record from the Temp file.
  DELETE
  ** Decrease the no of record by 1.
  lnNoOfRec  = lnNoOfRec - 1
  ** Assign the varaible of the list with the no of the records in the file.
  lsChecks   = lnNoOfRec

  SELECT(lcTmpCheck)

  ** If the Temp file is empty we are going to the top of the file.
  IF EOF() .AND. lnNoofRec = 0
    GO TOP
  ELSE
    SKIP -1
  ENDIF  

  =lfAssgObjs()
  
  ** If the no of records in the file is = 0 so we are going to enable
  ** the check code object,new button,close button 'Enable' and disable
  ** the rest of the objects.
  
  IF lnNoOfRec = 0
    =lfDiplObjs('DISABLE')
    SHOW GET pbRemove     DISABLE
  ELSE
    ** If the no of records in the temp file is > 0 so we are going to
    ** Enable all the objects and disable the Check Code.
  
    =lfDiplObjs('ENABLE')
    SHOW GET lcChkCode    DISABLE    
    lcChkCodSt = 'DISABLE'
    SHOW GET pbRemove     ENABLE
  ENDIF

  SELECT APBANKS
ENDIF
IF lnNoOfRec > 0
  ** If the no of records in the Temp file is > 0 so the current
  ** object will be the List.
  _CUROBJ = OBJNUM(lsChecks)
ELSE
  ** If the no of records in the Temp file is = 0 so the current
  ** object will be the New push button.
  _CUROBJ = OBJNUM(pbNew)
ENDIF  

SHOW GET pbNew     ENABLE
SHOW GET lsChecks

*!**************************************************************************
*!
*!      Function: lfwChecks
*!
*!**************************************************************************
*
FUNCTION lfwChecks

SELECT(lcTmpCheck)

=lfAssgObjs()
=lfDiplObjs()
SHOW GET lcChkCode DISABLE    
lcChkCodSt = 'DISABLE'
SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfvChkCode
*!
*!**************************************************************************
* Function to validate the check code.
*
FUNCTION lfvChkCode

SELECT(lcTmpCheck)
lcChkCode = ALLTRIM(lcChkCode)
SHOW GET lcChkCode

** We are going to start the validation as soon as the Check Code field is
** not empty 

IF EMPTY(lcChkCode) 
  llNewChk   = .F. 
  IF lnNoOfRec = 0
    lcChkCode  = SPACE(12)
    lcChkAcct  = lcEmptyAcc
    =lfDiplObjs('DISABLE')
    SHOW GET pbRemove  DISABLE
    SHOW GET pbNew     ENABLE
  ELSE
    lsChecks = RECNO(lcTmpCheck)
    SHOW GET lsChecks
    =lfAssgObjs()
    =lfDiplObjs('ENABLE')
    SHOW GET lcChkCode DISABLE    
    lcChkCodSt = 'DISABLE'
    SHOW GET pbRemove  ENABLE
    SHOW GET pbNew     ENABLE      
  ENDIF
ELSE
  ** We are going to locate for the Check Code in the Temp file to make
  ** sure that this code is not exist in the file.
  lcSavExact = SET('EXACT')
  SET EXACT ON
  LOCATE FOR cChkAcct = lcChkCode
  SET EXACT &lcSavExact
  IF FOUND()
    ** If we found that this code is exist in the file so we are going to 
    ** Dispaly it on the screen to let the user now that its already exist.
    llNewChk   = .F. 
    =lfAssgObjs()
    lsChecks = RECNO(lcTmpCheck)
    SHOW GET lsChecks
    =lfDiplObjs('ENABLE')
    SHOW GET lcChkCode  DISABLE
    lcChkCodSt = 'DISABLE'
    SHOW GET pbNew      ENABLE
    SHOW GET pbRemove   ENABLE    
  ELSE  
    SHOW GET lcChkCode DISABLE
    lcChkCodSt = 'DISABLE'
    SHOW GET lcChkAcct ENABLE 
    SHOW GET ibGlAcc   ENABLE
  ENDIF     
ENDIF

SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfvNxtMan
*!
*!**************************************************************************
*
FUNCTION lfvNxtMan

IF lnChkNxtMN < 1
  ** MESSAGE   " Negative values are not allowed. "
  **           "              ® Ok ¯              "
  =gfModalGen("TRM04087B00000","DIALOG")
  lnChkNxtMN = lnOldValue
  SHOW GET lnChkNxtMN
  _CUROBJ = _CUROBJ
ELSE  
  =lfUpdTemp()
  _CUROBJ = OBJNUM(pbNew)
ENDIF



*!**************************************************************************
*!
*!      Function: lfvNxtPrn
*!
*!**************************************************************************
*
FUNCTION lfvNxtPrn

IF lnChkNxtPN < 1
  ** MESSAGE   " Negative values are not allowed. "
  **           "              ® Ok ¯              "
  =gfModalGen("TRM04087B00000","DIALOG")
  lnChkNxtPN = lnOldValue
  SHOW GET lnChkNxtPN
  _CUROBJ = _CUROBJ  
ELSE  
  IF EOF('APCHECKS')     && new record
    =lfUpdTemp()
  ELSE
    IF lnChkNxtPN < APCHECKS.nChkNxtPN
      =gfModalGen("TRM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.nChkNxtPN)))
      lnChkNxtPN = lnOldValue
      SHOW GET lnChkNxtPN
      _CUROBJ = _CUROBJ  
    ELSE
      =lfUpdTemp()
    ENDIF
  ENDIF

ENDIF

*!**************************************************************************
*!
*!      Function: lfUpdTemp
*!
*!**************************************************************************
*
FUNCTION lfUpdTemp

SELECT(lcTmpCheck)

** Replace the record with the variables of the screen.

REPLACE  cChkAcct   WITH lcChkCode  ,;
         cChkShDes  WITH lcChkShDes ,;
         cCurrcode  WITH lcCurrency ,;
         nChkNxtMN  WITH lnChkNxtMN ,;
         nChkNxtPN  WITH lnChkNxtPN ,;
         cChkGLAcc  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcChkAcct,'-'),'0')),;
                             SPACE(24),lcChkAcct),;
         cDiscAcct  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcDiscAcct,'-'),'0')),;
                             SPACE(24),lcDiscAcct),;
         cAdjAcct   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAdjAcct,'-'),'0')),;
                             SPACE(24),lcAdjAcct),;
         cStatus    WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

SHOW GET lsChecks
SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfwAccount
*!
*!**************************************************************************
*
FUNCTION lfwAccount
PARAMETERS lcOldAcct

lcOldAcc = lcOldAcct

*!**************************************************************************
*!
*!      Function: lfvAccount
*!
*!**************************************************************************
*
FUNCTION lfvAccount
PARAMETERS lnObjNum

lcAccount = SYS(18)
*B603052,1 (Start)
*IF EMPTY(STRTRAN(STRTRAN(&lcAccount,'-'),'0'))
IF EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0'))
*B603052,1 (End)
  &lcAccount  = lcEmptyAcc
  SHOW GET (lcAccount)
ENDIF

*B603052,1 (Start)   
*IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(&lcAccount,'-'),'0')) 
IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')) 
*B603052,1 (End)   
  =lfApAcs(.F.,llBrowse)
  llBrowse = .F.
  *B603052,1 (Start)   
  *IF EMPTY(STRTRAN(STRTRAN(&lcAccount,'-'),'0'))
  IF EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0'))  
  *B603052,1 (End)   
    DO CASE
      CASE lnObjNum = 2
        lcDiscAcct = lcOldAcc
        SHOW GET lcDiscAcct

      CASE lnObjNum = 3
        lcAdjAcct  = lcOldAcc
        SHOW GET lcAdjAcct

      *E300921,1 TAK Added new object (4),for Gl cash account.
      CASE lnObjNum = 4
        lcCshAcct  = lcOldAcc
        SHOW GET lcCshAcct
      *E300921,1 (End).
      
    ENDCASE  
  ENDIF

  *E300921,1 TAK Don't update temp if it is Gl cash account, becouse
  *E300921,1 the record for this account will be created in lpSavScr.
  IF lnObjNum <> 4
  *E300921,1 (End).
    =lfUpdTemp()
  ENDIF
  
  =gfUpdate()
ENDIF

SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfwChkAcct
*!
*!**************************************************************************
*
FUNCTION lfwChkAcct
lcOldAcc = lcChkAcct
IF llNewChk
  SHOW GET lsChecks DISABLE
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvChkAcct
*!
*!**************************************************************************
*
FUNCTION lfvChkAcct

IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(lcChkAcct,'-'),'0'))
  =lfApAcs(.F.,llBrowse)
  llBrowse = .F.
  IF llNewChk
    llNewChk = .F.
    IF EMPTY(STRTRAN(STRTRAN(lcChkAcct,'-'),'0'))
      IF lnNoOfRec = 0
        lcChkCode  = SPACE(12)
        lcChkAcct  = lcEmptyAcc
        SHOW GET pbRemove  DISABLE
        SHOW GET pbNew     ENABLE
        _CUROBJ  = OBJNUM(pbNew)
        =lfDiplObjs('DISABLE')
      ELSE
        SHOW GET lsChecks
        lsChecks = RECNO(lcTmpCheck)
        =lfAssgObjs()
        SHOW GET lcChkCode DISABLE    
        lcChkCodSt = 'DISABLE'     
        SHOW GET pbRemove  ENABLE
        =lfDiplObjs('ENABLE')
        SHOW GET pbNew     ENABLE      
      ENDIF
    ELSE
      SELECT(lcTmpCheck)
      ** Insert the record in the Temp file.
      INSERT INTO &gcWorkDir.&lcTmpCheck;
             (CBNKCODE,cChkAcct,NRECNO,CSTATUS);
      VALUES(laData[1],lcChkCode,RECNO(),"A")

      lsChecks  = RECNO(lcTmpCheck) && Assign the list to the record no of the new check.
      lnNoOfRec = lnNoOfRec + 1     && Increment the no of records by 1
      SHOW GET lcChkCode DISABLE           
      lcChkCodSt = 'DISABLE'
      SHOW GET pbRemove  ENABLE
      SHOW GET pbNew     ENABLE
      lcCurrency = IIF(EMPTY(lcCurrency),gcBaseCurr,lcCurrency)
      =lfDiplObjs('ENABLE')
      SHOW GET lsChecks ENABLE
      =lfUpdTemp()
    ENDIF
  ELSE
    IF EMPTY(STRTRAN(STRTRAN(lcChkAcct,'-'),'0'))
      lcChkAcct  = IIF(EMPTY(lcOldAcc), lcEmptyAcc, lcOldAcc)
      SHOW GET lcChkAcct ENABLE
      SHOW GET ibGLAcc   ENABLE
    ENDIF
    =lfUpdTemp()		&& Add Alb
    
  ENDIF
ENDIF  

=gfUpdate()

SELECT APBANKS

*!**************************************************************************
*!
*!      Function: lfDiplObjs
*!
*!**************************************************************************
FUNCTION lfDiplObjs
PARAMETERS lcObjStatus

*** Added By : Malak Hanna
lcObjStatus = IIF(EMPTY(lcObjStatus),'',lcObjStatus)

** Display the objects in the screen.
SHOW GET lcChkCode  &lcObjStatus
SHOW GET lcChkLnDes &lcObjStatus
SHOW GET lcChkShDes &lcObjStatus
SHOW GET lnChkNxtMN &lcObjStatus
SHOW GET lnChkNxtPN &lcObjStatus
SHOW GET lcChkAcct  &lcObjStatus
SHOW GET lcDiscAcct &lcObjStatus
SHOW GET lcAdjAcct  &lcObjStatus
SHOW GET ldLstPrChk &lcObjStatus
SHOW GET ldLstMaChk &lcObjStatus    
SHOW GET lnLstPrChk &lcObjStatus
SHOW GET lnLstMaChk &lcObjStatus    
SHOW GET ibGlAcc    &lcObjStatus
SHOW GET ibDisAcc   &lcObjStatus
SHOW GET ibAdjAcc   &lcObjStatus
IF &lcTmpCheck..CSTATUS = 'A'
  SHOW GET ibCurrncy  ENABLE
  SHOW GET lcCurrency ENABLE
ELSE
  SHOW GET ibCurrncy  DISABLE
  SHOW GET lcCurrency DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfAssgObjs
*!
*!**************************************************************************
FUNCTION lfAssgObjs

*** Added By : Malak Hanna
*** Assign the screen variables with the variables of the file.
lcChkCode  = &lcTmpCheck..cChkAcct
lcChkShDes = &lcTmpCheck..cChkShDes
lnChkNxtMN = &lcTmpCheck..nChkNxtMN
lnChkNxtPN = &lcTmpCheck..nChkNxtPN
lcChkAcct  = IIF(EMPTY(&lcTmpCheck..cChkGLAcc),lcEmptyAcc, &lcTmpCheck..cChkGLAcc)
lcDiscAcct = IIF(EMPTY(&lcTmpCheck..cDiscAcct),lcEmptyAcc, &lcTmpCheck..cDiscAcct)
lcAdjAcct  = IIF(EMPTY(&lcTmpCheck..cAdjAcct),lcEmptyAcc, &lcTmpCheck..cAdjAcct)
lcCurrency = &lcTmpCheck..cCurrCode
ldLstPrChk = APCHECKS.dChkLPDat
ldLstMaChk = APCHECKS.dChkLMDat  
lnLstPrChk = APCHECKS.nChkLPAmt
lnLstMaChk = APCHECKS.nChkLMAmt

*!**************************************************************************
*!
*!      Procedure: lfwCurrncy
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
PROCEDURE lfwCurrncy

lcOldCurr = lcCurrency

*!**************************************************************************
*!
*!      Procedure: lfvCurrncy
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
PROCEDURE lfvCurrncy

*IHB open SYCCURR if not already open [start]
=gfOpenFile(gcSysHome+'SYCCURR','Ccurrcode','SH')
*IHB [end]

IF llBrowse .OR. !SEEK(lcCurrency,'SYCCURR') .OR. ATC("?",lcCurrency) > 0 .AND. LASTKEY() = 13
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    lcCurrency = lcOldCurr
  ELSE
    lcCurrency = laTemp[1]
  ENDIF
  llBrowse = .F.
ENDIF
SELECT(lcTmpCheck)
REPLACE cCurrCode WITH lcCurrency
SHOW GET lcCurrency


*:*************************************************************
*: Name      : lfvBnkTyp
*: Developer : Timour A. K.
*: Date      : 07/12/98
*: Purpose   : E300921,1 (Bank Type Valid function).
*:*************************************************************
*: Returns   : ............
*:*************************************************************
*: Example   : =lfvBnkTyp()
*:*************************************************************
FUNCTION lfvBnkTyp

laData[13]  = IIF(lnBnkTyp=2,'S','B')
*C102057,1 ABD change check to be if come from Ps module. [Begin]
*lcTxtToPrmt = IIF(lnBnkTyp=2,'\<Users...','\<Checking accounts...')
lcTxtToPrmt = IIF(lnBnkTyp=2 .OR. llFromPs,'\<Users...','\<Checking accounts...')
*C102057,1 ABD [End]

IF laData[13] = 'S' AND !llUsersExt
  SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt DISABLE
ELSE
  SHOW GET pbChkAcs,1 PROMPT lcTxtToPrmt
ENDIF
lcObjDEbl = IIF(laData[13] = 'S' AND (laScrMode[3] OR laScrMode[4]),'ENABLE','DISABLE')
IF laData[13] = 'B'
  lcCshAcct = lcEmptyAcc
ENDIF
SHOW GET ibGlCshAcc &lcObjDEbl
SHOW GET lcCshAcct  &lcObjDEbl
RETURN


*:*************************************************************
*: Name      : lfvSafeUsrs
*: Developer : Timour A. K.
*: Date      : 07/12/98
*: Purpose   : E300921,1 (Select Safe Users).
*:*************************************************************
*: Returns   : This function will fill the safe users array
*:             with the selected users. 
*:*************************************************************
*: Example   : =lfvSafeUsrs()
*:*************************************************************
FUNCTION lfvSafeUsrs

SELECT cUsr_Name+'    '+cUser_id FROM SYUUser INTO ARRAY laSource ORDER BY 1
DIMENSION laOldTrAr[1]
laOldTrAr = ''
=ACOPY(laSafeUsrs,laOldTrAr)

*--Call the global mover function.
=gfMover(@laSource,@laSafeUsrs,"Select Safe Users")

IF LASTKEY()=27
  = ACOPY(laOldTrAr,laSafeUsrs)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfChkGlAcct
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 07/18/2002
*! Purpose   : Check that bank code has gl account
*!			   B#606184
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfChkGlAcct()
*!*************************************************************
FUNCTION lfChkGlAcct
PRIVATE llCorrGlAcct
llCorrGlAcct = .T.
lcAlias = ALIAS()
SELECT (lcTmpCheck)
lnRecNo = RECNO()
LOCATE
SCAN WHILE (llCorrGlAcct AND !EOF())
  llCorrGlAcct = IIF(EMPTY(ALLTRIM(CCHKGLACC)),.F.,.T.)
ENDSCAN
GOTO lnRecNo
SELECT (lcAlias)
RETURN llCorrGlAcct