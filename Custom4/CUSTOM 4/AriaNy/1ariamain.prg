*!**************************************************************************
*! Name      : ARIAMAIN.PRG
*! Purpose   : ARIA'S Custom Process Program.
*!**************************************************************************
*! Parameters: loFormSetRef -> FormSet object reference.
*!             lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
PARAMETER loFormSetRef,lcEvntFun,lcFunPars

PRIVATE lcScrMode,lnDataSess,lcAccount
lcAccount  = PADR(CUSTOMER.Account,5)
lcScrMode  = loFormSetRef.ActiveMode
lnDataSess = IIF(TYPE("loFormSetRef")="O",loFormSetRef.DataSessionID,SET("Datasession"))
SET DATASESSION TO lnDataSess

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue



*!**************************************************************************
*! Name      : lfCUSTPROF
*! Purpose   : ARIA'S Custom Option bars defenisions.
*!**************************************************************************
FUNCTION lfCUSTPROF

DEFINE BAR 15 OF _INQURYPOP PROMPT "\-" SKIP FOR _Screen.ActiveForm.Parent.Activemode='S'
DEFINE BAR 16 OF _INQURYPOP PROMPT "Customer \<Profile" SKIP FOR _Screen.ActiveForm.Parent.Activemode='S'    &&"Customer Profile"
DEFINE BAR 17 OF _INQURYPOP PROMPT "Acti\<vation Keys" SKIP FOR _Screen.ActiveForm.Parent.Activemode$'SA'   &&"Activation Keys"
DEFINE BAR 18 OF _INQURYPOP PROMPT "\<Web Open Issues" SKIP FOR _Screen.ActiveForm.Parent.Activemode$'SA'   &&"Web Open Issues"
DEFINE BAR 19 OF _INQURYPOP PROMPT "Progra\<mming Entries" SKIP FOR _Screen.ActiveForm.Parent.Activemode$'SA'   &&"Programming Entries"
DEFINE BAR 20 OF _INQURYPOP PROMPT "\<Time Transactions" SKIP FOR _Screen.ActiveForm.Parent.Activemode$'SA'   &&"Time Transactions"
DEFINE BAR 21 OF _INQURYPOP PROMPT "\<Connection Information" SKIP FOR _Screen.ActiveForm.Parent.Activemode$'S'    &&"Connection Information"
RETURN



*!**************************************************************************
*! Name      : lfReadConStr
*! Purpose   : Read SQL connection string. (get it if not defined).
*!**************************************************************************
FUNCTION lfReadConStr

*--Read connection string.
lcConnFile = oAriaApplication.ResourceHome+"ConnInfo.txt"

IF !FILE(lcConnFile)
  *lcConnStr = "driver={SQL Server};server=ariaweb;DATABASE=db01;uid=msp;pwd=aria"  &&Egypt connection string.
  lcConnStr = "driver={SQL Server};server=ariasrv1;DATABASE=DB01;uid=web;pwd=webaria"
  lcConnStr = INPUTBOX("Enter connection string","SQL Connection string",lcConnStr)
  IF EMPTY(lcConnStr)
    MESSAGEBOX("No server connection information are defined, unable to proceed!",16,_SCREEN.Caption)
  ELSE
    =STRTOFILE(ALLTRIM(lcConnStr),lcConnFile,0)
  ENDIF
ELSE
  lcConnStr = ALLTRIM(FILETOSTR(lcConnFile))
ENDIF
RETURN lcConnStr


*!**************************************************************************
*! Name      : lfCUSTPSCR
*! Purpose   : ARIA'S Custom Option bars run.
*!**************************************************************************
FUNCTION lfCUSTPSCR
LPARAMETERS lnBarSel

IF lnBarSel=3 OR lnBarSel=4
  oWeb = CREATEOBJECT('HyperLink')
  IF lnBarSel=3   &&Web Open Issues
    oweb.NavigateTo("http://www.ariany.com/myaria/ariaadmin/")
    *oweb.NavigateTo("http://www.ariany.com/ariaadmn/default.asp")
  ELSE  &&   &&Programming Entries
    lcPageUrl = 'http://www.ariany.com/Tracking/default.asp?strCustID='+lcAccount
    oweb.NavigateTo(lcPageUrl)
  ENDIF

ELSE
  IF  lnBarSel=1 AND !CUSTOMER.Status $ 'AH'
    *--Validate customer status for Customer Profile.
    MESSAGEBOX("You cannot set up a profile for Potential or Cancelled customers."+CHR(13)+"Only Active or Hold are allowed.",_SCREEN.Caption)
    RETURN
  ENDIF
  
  *--Read connection string.    
  lcConnString = lfReadConStr()
  IF EMPTY(lcConnString)
    RETURN
  ENDIF

  DO CASE
    ****************************************************
    CASE lnBarSel=1   &&Customer Profile
      =lfProfInfo()
    ****************************************************
    CASE lnBarSel=2   &&Activation Keys
      =lfAkeyInfo()
    ****************************************************
    CASE lnBarSel=5   &&Time Transactions
      IF !USED("CUSTTTRN") OR CUSTTTRN.CustomerID#lcAccount
        lnResult = oAriaApplication.remotesystemdata.execute("SELECT * FROM CustomerTransaction WHERE CustomerID='"+lcAccount+"'",'',"CUSTTTRN","",lcConnString,3,"",lnDataSess)
        IF lnResult<1
          MESSAGEBOX("Error retriving CustomerTransaction data, Table maybe does not exist.",16,_SCREEN.Caption)
          RETURN
        ENDIF
      ENDIF
      *--Call Global browse to view this file.
      SELECT CUSTTTRN
      GO TOP
      IF !EOF()
        lcBrowExpresion = ""
        lcBrFields = "Transactionno :H='Tran. #',Transdate :H='Date' :30,doctype :H='Doc. Type' :25,docno :H='Doc. No.',value"
        =ARIABROW(lcBrowExpresion,"Customer Time Transactions",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2)
      ELSE
        MESSAGEBOX("No time transactions done to display!",64,_SCREEN.Caption)
      ENDIF
      
    ****************************************************
    CASE lnBarSel=6   &&Connection Information
      IF !USED("CUSTCONN")
        lnResult = oAriaApplication.remotesystemdata.execute("Select * from CUSTCONN where ccust_id='"+lcAccount+"'",'',"CUSTCONN","CUSTCONN",lcConnString,3,"",lnDataSess)
        IF lnResult<1
          MESSAGEBOX("Error retriving customer connection information!",16,_SCREEN.Caption)
          RETURN .F.
        ENDIF
      ENDIF
      *--Call connections screen.
      DO FORM (oAriaApplication.ScreenHome+"sucsconn.SCX") WITH lcScrMode
    ****************************************************
  ENDCASE

ENDIF
RETURN




*!**************************************************************************
*! Name      : lfProfInfo
*! Purpose   : Run customer profile screen.
*!**************************************************************************
FUNCTION lfProfInfo

*--Open needed tables.
STORE 1 TO lnResult01,lnResult02,lnResult03,lnResult04,lnResult05
IF !USED('CUSTPROF')
  lnResult01 = oAriaApplication.remotesystemdata.execute("Select * from CUSTPROF  where ccust_id='"+lcAccount+"'",'',"CUSTPROF","CUSTPROF",lcConnString,3,"",lnDataSess)
  IF lnResult01>0 AND USED('CUSTPROF')
    SELECT CUSTPROF
    =CURSORSETPROP("Buffering",5)
    GO TOP
    IF EOF()
     APPEND BLANK
     REPLACE CCUST_ID   WITH lcAccount,;
             NCUST_USRN WITH  1,;
             CCUST_HOMD WITH "",;
             DCUST_INSD WITH {},;
             DCUST_PADU WITH {},;
             NCUST_QMAM WITH 0.00,;
             NTIME_BAL  WITH 0.00,;
             NTIME_BLK  WITH 0.00,;
             LACTUSER   WITH .T.,;
             CPROCURE   WITH 'P',;
             NRENTCHRGE WITH 0.00,;
             LHOSTING   WITH .F.,;
             NHOSTCHRGE WITH 0.00,;
             NAVAILMAP  WITH 0,;
             CADD_USER  WITH oAriaApplication.User_ID,;
             TADD_DATE  WITH DATETIME()

    ENDIF
  ENDIF
ENDIF

IF !USED('CSTSOFTW') AND lnResult01>0
  lnResult02 = oAriaApplication.remotesystemdata.execute("SELECT * from CSTSOFTW where ccust_id='"+lcAccount+"'",'',"CSTSOFTW","CSTSOFTW",lcConnString,3,"",lnDataSess)
ENDIF
*--Open applicatio and modules librory files (if not opened from before).
IF !USED('SUAPPMOD') AND lnResult01>0 AND lnResult02>0
  lnResult03 = oAriaApplication.remotesystemdata.execute("SELECT * from SUAPPMOD ",'',"SUAPPMOD","",lcConnString,3,"",lnDataSess)
ENDIF
IF !USED('ARIA_APP') AND lnResult01>0 AND lnResult02>0 AND lnResult03>0
  lnResult04 = oAriaApplication.remotesystemdata.execute("SELECT * from ARIA_APP ",'',"ARIA_APP","",lcConnString,3,"",lnDataSess)
ENDIF
IF lcScrMode # 'V' AND !USED('TRKCSTPRG')
  lnResult05 = oAriaApplication.remotesystemdata.execute("SELECT * FROM TrackingEntries WHERE cTrackType='C' AND cCustid='"+lcAccount+"'",'',"TRKCSTPRG","",lcConnString,3,"",lnDataSess)
ENDIF


*--If one of the cursor retrival is fail, don't continue.
IF lnResult01<1 OR lnResult02<1 OR lnResult03<1 OR lnResult04<1 OR lnResult05<1
  MESSAGEBOX("Error retriving customer profile data!",16,_SCREEN.Caption)
  RETURN .F.
ENDIF


*--Select Standard Modules.
IF !USED('StandMod')
  SELECT CSTSOFTW.* ,SUAPPMOD.cMod_Desc AS cDesc,ARIA_APP.cApp_Name;
    FROM CSTSOFTW,SUAPPMOD,ARIA_APP ;
    WHERE CSTSOFTW.cModType $ 'SU' AND CSTSOFTW.cApp_ID=ARIA_APP.cApp_ID AND CSTSOFTW.cApp_ID+CSTSOFTW.cMod_ID=SUAPPMOD.cApp_ID+SUAPPMOD.cMod_ID ;
    ORDER BY CSTSOFTW.cApp_ID,CSTSOFTW.cMod_ID INTO CURSOR 'StandMod' READWRITE 

  SELECT StandMod
  INDEX ON cCust_id+cApp_id+cmod_id+cmodtype+centryno TAG 'StandMod'
ENDIF

*--Select Custom Programs.
IF !USED('CustmProg')
  SELECT CSTSOFTW.* ,ARIA_APP.cApp_Name;
    FROM CSTSOFTW,ARIA_APP ;
    WHERE CSTSOFTW.cModType = 'C' AND CSTSOFTW.cApp_ID=ARIA_APP.cApp_ID ;
    ORDER BY CSTSOFTW.cApp_ID,CSTSOFTW.cEntryNo INTO CURSOR 'CustmProg' READWRITE
ENDIF

*--Select EDI Mappings.
IF !USED('CstMapping')
SELECT CSTSOFTW.* ,SUAPPMOD.cMod_Desc as cDesc,ARIA_APP.cApp_Name;
  FROM CSTSOFTW,SUAPPMOD,ARIA_APP ;
  WHERE CSTSOFTW.cModType = 'E' AND CSTSOFTW.cApp_ID=ARIA_APP.cApp_ID AND CSTSOFTW.cApp_ID+CSTSOFTW.cMod_ID=SUAPPMOD.cApp_ID+SUAPPMOD.cMod_ID ;
  INTO CURSOR 'CstMapping' READWRITE

  SELECT CstMapping
  INDEX ON cCust_id+cApp_id+cmod_id+cmodtype+centryno TAG 'CstMapping'
ENDIF

*--Select EDI Packages.
IF !USED('CstPackage')
SELECT CSTSOFTW.* ,SUAPPMOD.cMod_Desc as cDesc,ARIA_APP.cApp_Name;
  FROM CSTSOFTW,SUAPPMOD,ARIA_APP ;
  WHERE CSTSOFTW.cModType = 'P' AND CSTSOFTW.cApp_ID=ARIA_APP.cApp_ID AND CSTSOFTW.cApp_ID+CSTSOFTW.cMod_ID=SUAPPMOD.cApp_ID+SUAPPMOD.cMod_ID ;
  INTO CURSOR 'CstPackage' READWRITE
ENDIF


*--Call cust prof screen
DO FORM (oAriaApplication.ScreenHome+"SUCsPro.SCX") WITH lcScrMode,lcConnString
RETURN




*!**************************************************************************
*! Name      : lfProfInfo
*! Purpose   : Run Activaton key screen.
*!**************************************************************************
FUNCTION lfAkeyInfo

IF !USED('CUSTPUSR')
  lnResult01 = oAriaApplication.remotesystemdata.execute("Select ccust_id,ncust_usrn from CUSTPROF  where ccust_id='"+lcAccount+"'",'',"CUSTPUSR","",lcConnString,3,"",lnDataSess)
  IF lnResult01<1
    MESSAGEBOX("Error retriving customer profile data!",16,_SCREEN.Caption)
    RETURN .F.
  ENDIF
ENDIF
PRIVATE lnNoOfUsers
lnNoOfUsers = CUSTPUSR.ncust_usrn
USE IN CUSTPUSR

IF !USED("SUACTKEY")
  lnResult = oAriaApplication.remotesystemdata.execute("Select * from SUACTKEY where ccust_id='"+lcAccount+"'",'',"SUACTKEY","SUACTKEY",lcConnString,3,"",lnDataSess)
  SELECT SUACTKEY
  =CURSORSETPROP("Buffering",5)
  IF lnResult<1
    MESSAGEBOX("Error retriving customer activation key information!",16,_SCREEN.Caption)
    RETURN .F.
  ENDIF
ENDIF
    
IF !USED('STDMODCUR')  
  *--Select Standard Modules.
  lcSqlStrg="SELECT CSTSOFTW.cMod_id as cModID,SUAPPMOD.cMod_Desc AS cDesc FROM CSTSOFTW,SUAPPMOD WHERE ccust_id='"+lcAccount+"' AND CSTSOFTW.cModType = 'S' AND CSTSOFTW.cApp_ID=SUAPPMOD.cApp_ID AND CSTSOFTW.cMod_ID=SUAPPMOD.cMod_ID"
  lnResult02 = oAriaApplication.remotesystemdata.execute(lcSqlStrg,'',"STDMODCUR","",lcConnString,3,"",lnDataSess)
  IF lnResult02<1
    MESSAGEBOX("Error retriving data.",16,_SCREEN.Caption)
    RETURN .F.
  ENDIF
ENDIF
GO TOP IN STDMODCUR
IF EOF('STDMODCUR') AND lcScrMode#'V'
  MESSAGEBOX("No standard sofware is defined for this customer!"+CHR(13)+"Unable to generate activation key.",16,_Screen.Caption)
  RETURN
ENDIF

    
*--Call act key screen.
DO FORM (oAriaApplication.ScreenHome+"SUActKey.SCX") WITH lcScrMode

RETURN



*!**************************************************************************
*! Name      : lfRECCHGE
*! Purpose   : On customer screen record changes.
*!             Close all files in order to recalculate new data.
*!**************************************************************************
FUNCTION lfRECCHGE

SET DATASESSION TO lnDataSess

*--Cust prof file.
IF USED('CUSTPROF')
  USE IN CUSTPROF
ENDIF

*--Software files
IF USED('CSTSOFTW')
  USE IN CSTSOFTW
ENDIF
IF USED('StandMod')
  USE IN StandMod
ENDIF
IF USED('CustmProg')
  USE IN CustmProg
ENDIF
IF USED('CstMapping')
  USE IN CstMapping
ENDIF
IF USED('CstPackage')
  USE IN CstPackage
ENDIF
IF USED('TRKCSTPRG')
  USE IN TRKCSTPRG
ENDIF

*--connection file.
IF USED("CUSTCONN")
  USE IN CUSTCONN
ENDIF

*--Act key files.
IF USED('SUACTKEY')
  USE IN SUACTKEY
ENDIF
IF USED('STDMODCUR')
  USE IN STDMODCUR
ENDIF

*--Trading Partners version.
IF USED('CUSTTDPRDT')
  USE IN CUSTTDPRDT
ENDIF

RETURN


*!**************************************************************************
*! Name      : lfARIASAVE
*! Purpose   : On customer screen saving (save Aria's custom files).
*!**************************************************************************
FUNCTION lfARIASAVE

*--Check if there is something to save.
llDoSave = USED("CUSTCONN") OR USED('SUACTKEY') OR USED('CUSTPROF') OR USED('CUSTTDPRDT')

IF !llDoSave
  RETURN
ENDIF

*--Read connection string.    
lcConnString = lfReadConStr()
IF EMPTY(lcConnString)
  RETURN
ENDIF

*--Begin saving transaction.
lcTranCdResult = oAriaApplication.remotesystemdata.BeginTran(lcConnString,3,'',.T.)
IF TYPE("lcTranCdResult")#'C'
  WAIT WINDOW "Error begin saving transaction!, No profile information will be saved!"
  RETURN
ENDIF


*--Update Connection information.
IF USED("CUSTCONN")
  lnResult = oAriaApplication.remotesystemdata.SQLUpdate("CUSTCONN",lcTranCdResult,lnDataSess,"cCust_ID,CconnCode")
  IF lnResult < 1
    WAIT WINDOW "Unable to update connections informations!" TIMEOUT 5
  ENDIF
ENDIF

*--Update activation key information.
IF USED('SUACTKEY')
  lnResult = oAriaApplication.remotesystemdata.SQLUpdate("SUACTKEY",lcTranCdResult,lnDataSess,"cCust_ID,cactkey,Dkeydate,Nnumuser,cact_type")
  IF lnResult < 1
    WAIT WINDOW "Unable to update activation key informations!" TIMEOUT 5
  ENDIF
ENDIF

*--Update Trading partner transaction version.
IF USED('CUSTTDPRDT')
  lnResult = oAriaApplication.remotesystemdata.SQLUpdate("CUSTTDPRDT",lcTranCdResult,lnDataSess,"cCust_ID,cApp_id,cMod_id,cPartcode,cEdiTrnTyp,cVersion")
  IF lnResult < 1
    WAIT WINDOW "Unable to update mappings trading partners transactions versions!" TIMEOUT 5
  ENDIF
ENDIF

*--Update Customer Profile.
IF USED('CUSTPROF')
  *B037262,1 SSH 03-11-2003 Fix problem of not updating the SQL file
  PRIVATE lnCurrOldalias
  lnCurrOldalias = SELECT(0)
  SELECT CUSTPROF
  *B037262,1 SSH 03-11-2003 Fix problem of not updating the SQL file
  lnResult = oAriaApplication.remotesystemdata.SQLUpdate("CUSTPROF",lcTranCdResult,lnDataSess,"cCust_ID")
  IF lnResult < 1
    WAIT WINDOW "Unable to update customer profile information!" TIMEOUT 5
  ENDIF
  *B037262,1 SSH 03-11-2003 Fix problem of not updating the SQL file
  SELECT(lnCurrOldalias)
  *B037262,1 SSH 03-11-2003 Fix problem of not updating the SQL file
ENDIF

*--Update Customer software.
IF USED('CSTSOFTW') AND USED('CSTSOFTW')
  lnCurrOldalias = SELECT(0)
  =lfUpdCstSoft()
  SELECT cStsoftw
  lnResult = oAriaApplication.remotesystemdata.SQLUpdate("CSTSOFTW",lcTranCdResult,lnDataSess,"cCust_ID,cApp_id,cModType,cMod_Id,Centryno")
  IF lnResult < 1
    WAIT WINDOW "Unable to update customer software information!" TIMEOUT 5
  ENDIF
  SELECT(lnCurrOldalias)  
ENDIF


lnResult3 = oAriaApplication.remotesystemdata.CommitTran(lcTranCdResult,.T.)
IF lnResult3 < 1
  WAIT WINDOW "Unable to commint saving transactions!"
ENDIF
  
RETURN


*!************************************************************************************
*! Name      : lfUpdCstSoft
*! Purpose   : Update Customer software buffer table with the changes in temp cursor.
*! ***********************************************************************************
*! Modifications:
*! B037319,1 SSH Fix the bug of not save the modifications
*!************************************************************************************
FUNCTION lfUpdCstSoft
*SSH

SELECT CustmProg
INDEX ON cCust_id+cApp_id+cmod_id+cmodtype+centryno TAG 'CustmProg'
SELECT CstPackage
INDEX ON cCust_id+cApp_id+cmod_id+cmodtype+centryno TAG 'CstPackage'

*--Delete any software.
SET DELETED OFF
SELECT cStsoftw
SCAN
  lcKey=cCust_id+cApp_id+cmod_id+cmodtype+centryno
  lcTmpFile = IIF(cmodtype $ 'SU','Standmod',IIF(cmodtype $ 'C','CustmProg',IIF(cmodtype $ 'E','CstMapping','CstPackage')))
  IF !SEEK(lcKey,lcTmpFile) OR DELETED(lcTmpFile)
    DELETE
  ENDIF
ENDSCAN
SET DELETED ON

*-Add/Update software.
SELECT Standmod
SCAN
  lcKey=cCust_id+cApp_id+cmod_id+cmodtype+centryno
  SCATTER MEMVAR
  SELECT cStsoftw
  LOCATE FOR cCust_id+cApp_id+cmod_id+cmodtype+centryno=lcKey
  IF !FOUND()
    APPEND BLANK
    REPLACE cAdd_user WITH oAriaApplication.User_ID,;
            tadd_date WITH DATETIME()
  *! B037319,1 SSH Fix the bug of not save the modifications
  ELSE
    DELETE
    APPEND BLANK
  *! B037319,1 SSH Fix the bug of not save the modifications
  ENDIF
  GATHER MEMVAR
  REPLACE Ccust_id   WITH CUSTPROF.cCust_id,;
		  csoft_modu WITH cDesc,;
		  Centryno   WITH '',;
		  NQty       WITH 1,;
		  Csoft_plat WITH 'W',;
		  NEDIPUse   WITH 0,;
		  LEDIMpPk   WITH .F.
ENDSCAN


SELECT CustmProg
SCAN
  lcKey=cCust_id+cApp_id+cmod_id+cmodtype+centryno
  SCATTER MEMVAR
  SELECT cStsoftw
  LOCATE FOR cCust_id+cApp_id+cmod_id+cmodtype+centryno=lcKey
  IF !FOUND()
    APPEND BLANK
    REPLACE cAdd_user  WITH oAriaApplication.User_ID,;
            tadd_date  WITH DATETIME()
  *! B037319,1 SSH Fix the bug of not save the modifications
  ELSE
    DELETE 
    APPEND BLANK
    GATHER MEMVAR MEMO
  *! B037319,1 SSH Fix the bug of not save the modifications
  ENDIF
  REPLACE Ccust_id   WITH CUSTPROF.cCust_id,;
		  cApp_ID    WITH m.cApp_ID,;
		  cMod_ID    WITH 'CUSTPRG',;
		  CmodType   WITH 'C',;
		  csoft_modu WITH m.csoft_modu,;
		  Centryno   WITH m.Centryno,;
          dSoft_ins  WITH {},;
		  NQty       WITH 1,;
		  Lmaintain  WITH m.Lmaintain,;
		  Csoft_plat WITH 'W',;
		  Nprice     WITH m.nPrice,;
		  cReference WITH m.cReference,;
		  NEDIPUse   WITH 0,;
		  LEDIMpPk   WITH .F.
ENDSCAN

SELECT CstMapping
SCAN
  lcKey=cCust_id+cApp_id+cmod_id+cmodtype+centryno
  SCATTER MEMVAR
  SELECT cStsoftw
  LOCATE FOR cCust_id+cApp_id+cmod_id+cmodtype+centryno=lcKey
  IF !FOUND()
    APPEND BLANK
    REPLACE cAdd_user  WITH oAriaApplication.User_ID,;
            tadd_date  WITH DATETIME()
  *! B037319,1 SSH Fix the bug of not save the modifications
  ELSE
    DELETE
    APPEND BLANK
  *! B037319,1 SSH Fix the bug of not save the modifications
  ENDIF
  GATHER MEMVAR
  REPLACE Ccust_id   WITH CUSTPROF.cCust_id,;
		  csoft_modu WITH cDesc,;
		  Centryno   WITH '',;
		  NQty       WITH 1,;
		  Csoft_plat WITH 'W',;
		  NEDIPUse   WITH 0
ENDSCAN

lnSqn=1
SELECT CstPackage
SCAN
  lcKey=cCust_id+cApp_id+cmod_id+cmodtype+centryno
  SCATTER MEMVAR
  SELECT cStsoftw
  LOCATE FOR cCust_id+cApp_id+cmod_id+cmodtype+centryno=lcKey
  IF !FOUND()
    APPEND BLANK
    REPLACE cAdd_user  WITH oAriaApplication.User_ID,;
            tadd_date  WITH DATETIME()
  *! B037319,1 SSH Fix the bug of not save the modifications
  ELSE
    DELETE 
    APPEND BLANK
    GATHER MEMVAR MEMO
  *! B037319,1 SSH Fix the bug of not save the modifications
  ENDIF
  REPLACE Ccust_id   WITH CUSTPROF.cCust_id,;
		  cApp_ID    WITH m.cApp_ID,;
		  cMod_ID    WITH m.cMod_ID,;
		  CmodType   WITH 'P',;
		  csoft_modu WITH m.cDesc,;
		  Centryno   WITH ALLTRIM(STR(lnSqn)),;
          dSoft_ins  WITH CUSTPROF.dcust_insd,;
		  NQty       WITH m.nQty,;
		  Lmaintain  WITH .F.,;
		  Csoft_plat WITH 'W',;
		  Nprice     WITH 0,;
		  cReference WITH m.cReference,;
		  NEDIPUse   WITH M.nedipuse,;
		  LEDIMpPk   WITH .F.

  lnSqn=lnSqn+1
ENDSCAN


*!**************************************************************************
*! Name    : lfARIADELE
*! Purpose : On customer screen deleting (Delete Aria's customer profile files).
*!**************************************************************************
FUNCTION lfARIADELE

*--Read connection string.    
lcConnString = lfReadConStr()
IF EMPTY(lcConnString)
  RETURN
ENDIF

*--Begin saving transaction.
lcTranCdResult = oAriaApplication.remotesystemdata.BeginTran(lcConnString,3,'',.T.)
IF TYPE("lcTranCdResult")#'C'
  WAIT WINDOW "Error begin saving transaction!, No profile information will be deleted!"
  RETURN
ENDIF

*--Delete Customer Profile information.
=lfDelSqlFile("CUSTPROF",lcTranCdResult)

=lfDelSqlFile("CSTSOFTW",lcTranCdResult)

=lfDelSqlFile("CUSTCONN",lcTranCdResult)

=lfDelSqlFile("SUACTKEY",lcTranCdResult)

=lfDelSqlFile("CUSTTDPRDT",lcTranCdResult)

=lfDelSqlFile("CSTSOFTW",lcTranCdResult)


lnResult3 = oAriaApplication.remotesystemdata.CommitTran(lcTranCdResult,.T.)
IF lnResult3 < 1
  WAIT WINDOW "Unable to commint customer profile delete transaction!"
ENDIF

RETURN

FUNCTION lfDelSqlFile
LPARAMETERS lcFileName,lcTranCdResult

lcDeleStr="DELETE FROM "+lcFileName+" WHERE cCust_Id=?m.lcAccount"
lnResult = oAriaApplication.remotesystemdata.Execute(lcDeleStr,'',"","",lcTranCdResult,4)
IF lnResult < 1
  WAIT WINDOW "Unable to delte customer informations at "+lcFileName+" Sql file!" TIMEOUT 5
ENDIF

*!**************************************************************************
*! Name      : lfINITIALIZE
*! Purpose   : 
*!**************************************************************************
FUNCTION lfINITIALIZE

WAIT WINDOW 'Trigger'