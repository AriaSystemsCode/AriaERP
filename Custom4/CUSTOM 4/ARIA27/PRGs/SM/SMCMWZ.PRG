*:----------------------------------------------------------------
*: Program file        : SMCMWZ.PRG
*: Program description : Company Wizard
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SM)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : SMWMAIN.PRG
*:----------------------------------------------------------------
*: Modifications       : 
*:B#804025,1 SSH 25/03/2001 Fix some bugs in screen layout.
*:----------------------------------------------------------------
*:
PARAMETER llFromResu
EXTERNAL ARRAY laData
DECLARE laCompInfo[2],laCountry[1,2]
llBrowse    = .F.
STORE '' TO lcDataDir
lcInsMdl    = " "
lcOldHComp  = ""
lcOldMod    = " "
lcPrntName  = " "
lcOldPrnt   = " "
lcOldPrnN   = " "
lnComRec    = 0
lcHPrompt   = ""
lnCurRec    = 0
lcOldActCm  = ""
lcOldDataD  = ""
lcCountry   = ""
lcCurrStat  = ""  && hold the Currancy display Status
lcOldCurr   = ''  && hold the old value of the currency
lcMultCurr  = "DISABLE"
lcMultCCnt  = "DISABLE"
lcSMCmSt    = ""  && hold the Setup window name
lnProgCopy  = gnProgCopy &&	 No of Program copies running at the moment
lcCmStTitl  = "Company Setups "+IIF(lnProgCopy=1,'',' /'+ALLTRIM(STR(lnProgCopy))) && hold the setup window title
lcSycComp   = ''
IF EMPTY(lcNextActn)    && Run the screen from the menu not from back button.
  IF !gfSetup()
    RETURN
  ENDIF  
ENDIF

lcScFields = "CCOMP_ID   , CCOM_NAME  , caddress1  , caddress2  , caddress3,;
              caddress4  , caddress5  , caddress6  , CCOM_PHON  , CCOM_FAX,;
              CCOM_DDIR  , CCURR_YER  , CCURR_PRD  , CCOMPPRNT  , MCOMP_MDL ,;
              CCONT_CODE , CCURRCODE "

IF !WEXIST(lcBaseWind)
  lcCurrStat = 'DISABLE'
  lcSMCmSt    = 'CWR'+RIGHT(gfTempName(),7)  && hold the Setup window name
  SELECT SYWCOMP
  *--- Start update the wizard company file.
  IF lcNextActn = 'B' .OR. !llSkip .OR. llFromResu
    SCATTER FIELDS &lcScFields MEMO TO laData
    IF EMPTY(laData[1])
      llSkip = .T.
    ENDIF
  ELSE
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  ENDIF
  SELECT SYCINT
  SELECT DISTINCT sycint.ccont_desc,sycint.ccont_code ;
         FROM (gcSysHome+"SYCINT") ;
         INTO ARRAY laCountry
  IF _TALLY = 0
    laCountry = " "
  ENDIF
ENDIF
puCountry = LOOKUP(SYCINT.CPARt1LAB,gcIntCont,sycint.ccont_code,'CCONTCODE')
lnOldAla = SELECT(0)
SELECT SYWSEQNC
SET ORDER TO Runfrm
*--- SEEK('1') &&.OR. (SEEK('1') .AND. ALLTRIM(cScrNam) = "SMCMWZ")
IF !llFromResu
  ON KEY LABEL ESCAPE  DO lpRetEscp
  DO (gcScrDir + gcWinAppl + '\SMCMWZ.SPR')
  ON KEY LABEL ESCAPE
  IF glQuitting
    IF USED('ACCOD')
      USE IN ACCOD
    ENDIF  
    IF USED('FISHD')
      USE IN FISHD
    ENDIF  
    RELEASE WINDOW (lcSMCmSt)
  ENDIF
ENDIF
SELECT SYWSEQNC
SET ORDER TO Csequanc
SELECT(lnOldAla)

*!*************************************************************
*! Name      : lfvData_1  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function of the company ID...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvData_1()
*!*************************************************************
*!
FUNCTION lfvData_1

IF (!EMPTY(laData[1]).AND. LASTKEY() = 13)
  =gfOpenFile(gcSysHome+"SycComp",'CCOMP_ID','SH')
  IF SEEK(laData[1],'SycComp')
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    =gfModalGen('QRM00274B00000','Dialog','Company code already exist.')
    laData[1] = '  '
    _CUROBJ=OBJNUM(laData[1])
    SHOW GETS
  ELSE
    llAddNew = (gfModalGen('QRM00001B00031','Dialog',"Company ID :"+laDAta[1])=1)
    IF llAddNew
      =lfAddNew()
    ELSE
      laData[1] = '  '
      _CUROBJ=OBJNUM(laData[1])
    ENDIF
  ENDIF
  SELECT SYCCOMP
ENDIF

*!*************************************************************
*! Name      : lfvDatDir  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function for the push button < Data Dictionary >...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvDatDir()
*!*************************************************************
*!
FUNCTION lfvDatDir

*!**************************************************************************
*!
*!      Function: lfwData_11
*!
*!**************************************************************************
** When function for laData[11] field {company path}
*
FUNCTION lfwData_11


*!**************************************************************************
*!
*!      Function: lfwData_14
*!
*!**************************************************************************
** When function for the laData[14] {parent company}...
*
FUNCTION lfwData_14

lcOldPrnt = laData[14]
lcOldPrnN = lcPrntName

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
** Valid function for the laData[14] {parent company}...
*
FUNCTION lfvData_14

IF laData[14] = lcOldPrnt
  RETURN
ENDIF  
  
IF EMPTY(laData[14]) .AND. LASTKEY() = 13
  lcPrntName = ""
  =lfRefresh()
  RETURN
ENDIF

lnComRec = RECNO()

IF LASTKEY() = 13
  =gfOpenFile(gcSysHome+"SycComp",'CCOMP_ID','SH')
  IF SEEK(laData[14],"SycComp")
    IF !EMPTY(SycComp.cCompPrnt)
      ** Company {laData[14]} is already a child company. **
      ** You cannot choose a child company as a parent company...
      ** <  Ok  > **
      =gfModalGen("TRM00165B00000","DIALOG",;
                  "( "+laData[14]+"-"+ALLTRIM(SycComp.cCom_name)+" )")
      laData[14] = lcOldPrnt
      SHOW GET laData[14]
      IF lnComRec > 0 .AND. lnComRec <= RECCOUNT()
        GO lnComRec
      ENDIF
      SELECT SYCCOMP
      RETURN
    ENDIF
    
    IF laData[14] = laData[1]
      ** You cannot choose the current company **
      ** to be a parent to itself...
      ** <  Ok  > **
      =gfModalGen("TRM00166B00000","DIALOG")
      laData[14] = lcOldPrnt
      IF lnComRec > 0 .AND. lnComRec <= RECCOUNT()
        GO lnComRec
      ENDIF
      SELECT SYCCOMP
      RETURN
    ENDIF
    
    lcDataDir  = gfGetDataDir(ALLTRIM(SycComp.cCom_DDir))
    =lfOpnFiles('ACCOD','ACCSEGNO')
    IF IIF(AT('?',laData[14]) > 0 ,!SEEK('','ACCOD'),.F.)    
      ** This company does not have an account **
      ** code structure.  You cannot choose it **
      ** as a parent company...
      ** <  Ok  > **
      =gfModalGen("TRM00198B00000","DIALOG")
      laData[14] = lcOldPrnt
      SHOW GET laData[14]
      SELECT SYCCOMP
      RETURN
    ENDIF
    lcPrntName = SycComp.cCom_name
  ELSE
    SELECT SycComp
    SET FILTER TO EMPTY(SycComp.cCompPrnt) .AND. ;
        SycComp.cComp_Id <> laData[1]
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcSaveBrow    = lcBrFields
    lcBrFields    = "cComp_id :H='Company ID',cCom_name :H='Company Name'"
    lcOld_ttl     = lcFile_ttl
    lcFile_ttl    = "Company information"
    laCompInfo[1] = laData[14]
    laCompInfo[2] = lcPrntName
    IF gfBrows(.F.,"cComp_id,cCom_name","laCompInfo")    
      IF laData[14] = laCompInfo[1] 
        laData[14]  = lcOldPrnt
      ELSE
        laData[14] = laCompInfo[1]
        lcPrntName = laCompInfo[2]      
      ENDIF
      =SEEK(laData[14], 'SycComp')
      lcDataDir  = gfGetDataDir(ALLTRIM(SycComp.cCom_DDir))
      =lfOpnFiles('ACCOD','ACCSEGNO')
      IF !SEEK("",'ACCOD')
        ** This company does not have an account **
        ** code structure.  You cannot choose it **
        ** as a parent company...
        ** <  Ok  > **
        =gfModalGen("TRM00198B00000","DIALOG")
        laData[14] = lcOldPrnt
        lcPrntName = lcOldPrnN
        SELECT SycComp
      ENDIF
    ELSE
      laData[14] = lcOldPrnt
    ENDIF
    lcBrFields    = lcSaveBrow 
    lcFile_ttl    = lcOld_ttl
  ENDIF
ELSE
  laData[14] = lcOldPrnt
ENDIF

SELECT SycComp
SET FILTER TO
IF lnComRec > 0 .AND. lnComRec <= RECCOUNT()
  GO lnComRec
ENDIF

SHOW GET laData[14]

=lfRefresh()

IF lcOldPrnt <> laData[14]

  IF EMPTY(laData[14])
    ** If this company is a parent compnay. **
    
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (gcSysHome+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" ;
           INTO ARRAY laAllMod

  ELSE
    ** If this company is a child company. **
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (gcSysHome+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" .AND. ;
                 !lparntmdl ;
           INTO ARRAY laAllMod
  ENDIF
ENDIF
SELECT SYCCOMP
*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
FUNCTION lfvData_16

DO CASE
  CASE _DOS
    laData[16] = gfActPop(5,21,11,43,'laCountry',2,1,@lcCountry)
    =lfRefresh()
  CASE _WINDOWS
    laData[16] = laCountry[ibCountry,2]
    SHOW GET ibComp
ENDCASE

IF EMPTY(laData[8])
  laData[8] = lcCountry
  SHOW GET laData[8]
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfwCurrency

lcOldCurr=laData[17]

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfvCurrency

IF (!EMPTY(laData[17]) AND !SEEK(laData[17],'SYCCURR')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Currency"
  =gfBrows(.F.,"cCurrCode","laTemp")
  laData[17]=laTemp[1]
  SELECT (lcSelect)
  SHOW GET laData[17]
ENDIF

IF EMPTY(laData[17])
  laData[17] = lcOldCurr
ENDIF
llBrowse = .F.
=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfGetCur
*!
*!**************************************************************************
*
FUNCTION lfGetCur

laData[17] = LOOKUP(SYCINT.cCurrCode,laData[16],sycint.ccont_code,'CCONTCODE')
SHOW GET laData[17]
=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfvData_18
*!
*!**************************************************************************
*
FUNCTION lfvData_18

IF laData[18]
  lcMultCurr = 'ENABLE'
  laData[19] = .T.
  laData[20] = .T.
  laData[21] = .F.
  laData[22] = 30
ELSE
  lcMultCurr = 'DISABLE'
  laData[19] = .F.
  laData[20] = .F.
  laData[21] = .F.
  laData[22] = 0
ENDIF
=lfShowCurr()

*!**************************************************************************
*!
*!      Function: lfShowCurr
*!
*!**************************************************************************
*
FUNCTION lfShowCurr

*!**************************************************************************
*!
*!      Function: lfOpnFiles
*!
*!**************************************************************************
FUNCTION lfOpnFiles
PARAMETER lcFilToOpn, lcTagToOpn

IF FILE(lcDataDir + lcFilToOpn + '.DBF')
  IF USED(lcFilToOpn)
    USE IN (lcFilToOpn)
  ENDIF  
  SELECT 0
  USE (lcDataDir + lcFilToOpn) AGAIN ORDER TAG (lcTagToOpn) 
ENDIF

*!*************************************************************
*! Name      : lfvNext
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid functino for next button..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvNext()
*!*************************************************************
*!
FUNCTION lfvNext

PRIVATE lnOldAls , llOpen
lnOldAls = SELECT(0)

IF llHComp=1
  IF EMPTY(lcHComp)
    WAIT WINDOW "History company ID can not be empty."
    _CUROBJ=OBJNUM(lcHComp)
    RETURN
  ELSE
    IF EMPTY(laData[2])
      WAIT WINDOW "Company name can not be empty."
      _CUROBJ = OBJNUM(laData[2])
      RETURN
    ELSE
      IF !lfCreHCmp()
        _CUROBJ=OBJNUM(lcHComp)
        RETURN
      ENDIF
    ENDIF
  ENDIF
ELSE
  ** Check if the company name is empty or not...
  IF EMPTY(laData[2])
    ** You cannot save this company without name. **
    =gfModalGen("TRM00171B00000","DIALOG")
    _CUROBJ = OBJNUM(laData[2])
    RETURN
  ENDIF
  *--- [Start] Start update the 
  =lfSavToNxt()
ENDIF
SELECT(lnOldAls)
CLEAR READ

*!*************************************************************
*! Name      : lfSavToNxt
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid functino to save when press next..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfSavToNxt()
*!*************************************************************
*!
FUNCTION lfSavToNxt

** Get the path...
laData[11] = IIF(RIGHT(ALLTRIM(laData[11]),1)<>'\',;
                  UPPER(ALLTRIM(laData[11]))+'\',;
                  UPPER(ALLTRIM(laData[11])))
gcDataDir  = gfGetDataDir(ALLTRIM(laData[11]))
SELECT SYWCOMP
IF !EMPTY(laData[1]) .AND. !SEEK(laData[1])
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()

*--- Start update the wizard company file.
lcNextActn = 'N'

*!*************************************************************
*! Name      : lfvRsumLat
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid functino for resume later button..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvRsumLat()
*!*************************************************************
*!
FUNCTION lfvRsumLat
=lfvNext()
lcNextActn = 'U'
CLEAR READ

*!*************************************************************
*! Name      : lfWhenWz
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : When functino ..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfWhenWz()
*!*************************************************************
*!
FUNCTION lfWhenWz

IF !EMPTY(laData[1])
  =lfAddNew()
ENDIF
IF lcNextActn = 'B' .OR. !llSkip
  SHOW GET laData[1] DISABLE
  SHOW GET pbNext     ENABLE
  SHOW GET pbCancel,1 ENABLE PROMPT "\<Cancel"
  RETURN
ENDIF
SHOW GETS DISABLE
puCountry = LOOKUP(SYCINT.CPARt1LAB,gcIntCont,sycint.ccont_code,'CCONTCODE')     
lcPrntName = " "
DECLARE laAllMod[1]
STORE " " TO laAllMod
STORE 'DISABLE' TO lcMultCCnt,lcMultCurr
=lfShowCurr()

SHOW GET pbInsSet   DISABLE
SHOW GET pbNext    DISABLE
SHOW GET laData[11] DISABLE
SHOW GET pbDatDir   DISABLE
SHOW GET laData[14] DISABLE
lcCurrStat = 'DISABLE'
SHOW GET laData[17] DISABLE
SHOW GET ibAddrType DISABLE
SHOW GET ibCurrKey  DISABLE
SHOW GET pbCancel,1 ENABLE PROMPT "\<Close"
SHOW GET laData[1]  ENABLE
SHOW GET PBCANCEL   ENABLE
SHOW GET lcFScrHlp  ENABLE
SHOW GET laData[15] ENABLE
_CUROBJ=OBJNUM(laData[1])

*!*************************************************************
*! Name      : lfAddNew
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functino to add new Copmany..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfAddNew()
*!*************************************************************
*!
FUNCTION lfAddNew

lcCurrStat = 'ENABLE'
SHOW GETS ENABLE
IF EMPTY(laData[16])
  puCountry  = LOOKUP(SYCINT.CPARt1LAB,gcIntCont,sycint.ccont_code,'CCONTCODE')
  laData[8]  = LOOKUP(SYCINT.cCont_Desc,gcIntCont,sycint.ccont_code,'CCONTCODE')
  laData[16] = gcIntCont
  laData[17] = LOOKUP(SYCINT.cCurrCode,gcIntCont,sycint.ccont_code,'CCONTCODE')
ELSE
  puCountry  = LOOKUP(SYCINT.CPARt1LAB,ALLTRIM(laData[16]),sycint.ccont_code,'CCONTCODE')
  laData[8]  = LOOKUP(SYCINT.cCont_Desc,ALLTRIM(laData[16]),sycint.ccont_code,'CCONTCODE')
  laData[16] = laData[16]
  laData[17] = LOOKUP(SYCINT.cCurrCode,ALLTRIM(laData[16]),sycint.ccont_code,'CCONTCODE')
ENDIF
lcPrntName = " "
** Build the string of the company directory **
laData[11]=gcAllCmp+IIF(RIGHT(ALLTRIM(gcAllCmp),1)='\','','\')+;
                        UPPER(ALLTRIM(laData[1]))+'\'
SHOW GET laData[8]
SHOW GET laData[11] ENABLE
SHOW GET pbDatDir   ENABLE
SHOW GET pbInsSet   DISABLE
SHOW GET pbNext   ENABLE
SHOW GET laData[17] ENABLE
SHOW GET ibAddrType ENABLE
SHOW GET ibCurrKey  ENABLE        
STORE 'ENABLE' TO lcMultCCnt
SHOW GET pbCancel,1 ENABLE PROMPT "\<Cancel"
=lfShowCurr()

*!*************************************************************
*! Name      : lpRetEscp
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : valid Functino for escap..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lpRetEscp()
*!*************************************************************
*!
PROCEDURE lpRetEscp

lcNextActn = 'C'
CLEAR READ

*********************
FUNCTION lfvHComp
PRIVATE lcOldAls ,lcOldFld , lcOld_ttl
*********************

lcOldAls = SELECT(0)
IF !USED("SYCCOMP")
  =gfOpenFile(gcSysHome+"SycComp",'CCOMP_ID','SH')  
ENDIF
IF !EMPTY(lcHComp) .AND. !SEEK(lcHComp,'SYCCOMP')
  DECLARE laHComp[1]
  lcOldFld = lcBrFields
  lcBrFields    = "cComp_id :H='Company ID',cCom_name :H='Company Name'"
  lcOld_ttl     = lcFile_ttl
  lcFile_ttl    = "Company information"
  =gfBrows(.F.,"cComp_id,cCom_name","laHComp")
  lcBrFields = lcOldFld
  lcFile_ttl = lcOld_ttl
  lcHComp = IIF(TYPE("laHComp")="C" .AND. !EMPTY(laHComp),laHComp,"")
  SHOW GET lcHComp
ELSE
  IF EMPTY(lcHComp)
    WAIT WINDOW "History company ID can not be empty."
  ENDIF
  IF EMPTY(laData[2])
    _CUROBJ=OBJNUM(laData[2])
  ENDIF
  RETURN
ENDIF
IF !EMPTY(lcHComp) .AND. SEEK(lcHComp,'SYCCOMP')
  IF gcAct_Comp == lcHComp
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "The active company cannot be the current one.")
    _CUROBJ = _CUROBJ
    USE IN SYCCOMP
    RETURN
  ENDIF
  IF lcHComp == laData[1]
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "This is a History Company,"+;
                "select another company to be the Active Company.")
    _CUROBJ = _CUROBJ
    USE IN SYCCOMP
    RETURN
  ENDIF
  llHiYN = GfGetMemVar('LLHIST',lcHComp)
  IF TYPE("llHiYN")="L" .AND. !llHiYN .AND. EMPTY(GfGetMemVar('M_Comp_Id',lcHComp))
  ELSE
    IF TYPE("llHiYN") = "L"
      IF !llHiYN
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This Company has a History Company already.")
      ELSE
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This is a History Company.")
      ENDIF
    ELSE
      =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"Cannot create History Company for this company.")
    ENDIF
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF
SELECT(lcOldAls)

*********************
FUNCTION lfSavOld
*********************

lcOldHComp = lcHComp

*********************
FUNCTION lfChkHCmp
*********************

lcHComp=IIF(llHComp=0,'',lcOldHComp)
  SHOW GET lcHComp
IF llHComp=1
  lcHPrompt  = "\<Finish"
  SHOW GET pbNext,1 PROMPT lcHPrompt
  SHOW GET pbResume DISABLE
ELSE
  lcHPrompt  = "\<Next >"
  SHOW GET pbNext,1 PROMPT lcHPrompt
  SHOW GET pbResume ENABLE
ENDIF

FUNCTION lfCreHCmp
PRIVATE lcOldAls
lcOldAls = SELECT(0)
IF gcAct_Comp == lcHComp
  =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
              "The active company cannot be the current one.")
  _CUROBJ = _CUROBJ
  RETURN (.F.)
ENDIF
IF lcHComp == laData[1]
  =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
              "This is a History Company,"+;
              "select another company to be the Active Company.")
  _CUROBJ = _CUROBJ
  RETURN (.F.)
ENDIF
lnAlias = SELECT(0)
SELECT SYCCOMP
lnRec = RECNO()
=gfOpenFile(gcSysHome+"SYCCONFG",'Nvarpos','SH')
SELECT SYCCOMP
IF SEEK(lcHComp)
  lcModu = mComp_Mdl
  lcModSet = mModlSet
  IF !GfGetMemVar('LLHIST',lcHComp) .AND. EMPTY(GfGetMemVar('M_Comp_Id',lcHComp))
    IF !SEEK(laData[1])
      =lfCretComp(.T.)
    ENDIF
    SELECT SYCCOMP
    =SEEK(laData[1])
    lcTrgDir = ALLTRIM(CCOM_DDIR)
    REPLACE mComp_Mdl WITH lcModu,;
            mModlSet  WITH lcModSet
    =SEEK(lcHComp)
    lcSorDir = ALLTRIM(CCOM_DDIR)
    IF USED("SETUPS")
      USE IN SETUPS
    ENDIF
    USE lcSorDir+"\SETUPS" IN 0 ORDER Varname
    SELECT Setups
    IF SEEK("LLHIST")
      REPLACE MData_Def WITH ".F."
    ENDIF
    IF SEEK("M_COMP_ID")
      REPLACE MData_Def WITH laData[1]
    ENDIF
    SELECT SYCCOMP
    lcCompCod = laData[1]
    SCATTER FIELDS &lcScFields MEMO TO laData
    laData[1] = lcCompCod
    laData[2] = 'History of ' + laData[2]
    laData[11] = lcTrgDir
    laData[15] = STRTRAN(laData[15],'GL|','')
    laData[15] = STRTRAN(laData[15],'GL,','')
    laData[15] = STRTRAN(laData[15],'GL','')
    lcCurModul = STRTRAN(MMODLSET,'GL|','')
    lcCurModul = STRTRAN(lcCurModul,'GL,','')
    lcCurModul = STRTRAN(lcCurModul,'GL','')
    =SEEK(laData[1])
    REPLACE mModlSet  WITH lcCurModul
    *-- copy modules files.
    IF USED('SETUPS')
      USE IN SETUPS
    ENDIF
    IF USED('ACCOD')
      USE IN ACCOD
    ENDIF
    IF USED('FISHD')
      USE IN FISHD
    ENDIF
    IF USED('FSHLD')
      USE IN FSHLD
    ENDIF
    IF USED('FSPRD')
      USE IN FSPRD
    ENDIF
    IF USED('ICISTRU')
      USE IN ICISTRU
    ENDIF
    IF USED('GLSETUP')
      USE IN GLSETUP
    ENDIF
    IF USED('APSETUP')
      USE IN APSETUP
    ENDIF
    IF USED('GLACCHAR')
      USE IN GLACCHAR
    ENDIF
    DECLARE laInstModl[1]
    =gfSubStr(laData[15],@laInstModl,'|')
    lnI = 1
    FOR lnI = 1 TO ALEN(laInstModl,1)
      =lfInstall(laInstModl[lnI])
    ENDFOR
    *-- copy setups file.
    lcActSetup = gfTempName()
    lcHstSetup = gfTempName()
    =gfOpenFile(lcSorDir+'setups','modvar','SH',@lcActSetup,.T.)
    =gfOpenFile(lcTrgDir+'setups','modvar','SH',@lcHstSetup,.T.)
    SELECT (lcActSetup)
    GO TOP
    SCAN
      SCATTER MEMO MEMVAR
      IF !SEEK(EVALUATE(KEY()),lcHstSetup)
        SELECT (lcHstSetup)
        APPEND BLANK
      ENDIF
      SELECT (lcHstSetup)
      GATHER MEMO MEMVAR
      SELECT (lcActSetup)
    ENDSCAN
    IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID',lcActSetup)
      SELECT (lcActSetup)
      REPLACE MDATA_DEF WITH laData[1]
    ELSE
      IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
        SELECT SYCCONFG
        SCATTER MEMO MEMVAR
        SELECT (lcActSetup)
        APPEND BLANK
        GATHER MEMO MEMVAR
        REPLACE MDATA_DEF WITH lcHComp
      ENDIF
    ENDIF
    =gfCloseFile(lcActSetup)
    IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID',lcHstSetup)
      SELECT (lcHstSetup)
      REPLACE MDATA_DEF WITH lcHComp
    ELSE
      IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
        SELECT SYCCONFG
        SCATTER MEMO MEMVAR
        SELECT (lcHstSetup)
        APPEND BLANK
        GATHER MEMO MEMVAR
        REPLACE MDATA_DEF WITH lcHComp
      ENDIF
    ENDIF
    IF SEEK (CHR(255)+CHR(255)+'LLHIST',lcHstSetup)
      SELECT (lcHstSetup)
      REPLACE MDATA_DEF WITH '.T.'
    ELSE
      IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'1','SYCCONFG')
        SELECT SYCCONFG
        SCATTER MEMO MEMVAR
        SELECT (lcHstSetup)
        APPEND BLANK
        GATHER MEMO MEMVAR
        REPLACE MDATA_DEF WITH '.T.'
      ENDIF
    ENDIF
    =gfCloseFile(lcHstSetup)
    =gfOpenFile(lcTrgDir+'setups','modvar','SH')
    *-- copy ICISTRU file.
    IF FILE(lcSorDir+'icistru.dbf')
      COPY FILE lcSorDir+'icistru.cdx' TO lcTrgDir+'icistru.cdx'
      COPY FILE lcSorDir+'icistru.dbf' TO lcTrgDir+'icistru.dbf'
    ENDIF
    *-- copy APSETUP file.
    IF FILE(lcSorDir+'apsetup.dbf')
      COPY FILE lcSorDir+'apsetup.dbf' TO lcTrgDir+'apsetup.dbf'
    ENDIF
    *-- copy GLSETUP file.
    IF FILE(lcSorDir+'glsetup.dbf')
      COPY FILE lcSorDir+'glsetup.dbf' TO lcTrgDir+'glsetup.dbf'
      COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.fpt'
    ENDIF
    *-- copy GLACCHAR file.
    IF FILE(lcSorDir+'glacchar.dbf')
      COPY FILE lcSorDir+'glacchar.cdx' TO lcTrgDir+'glacchar.cdx'
      COPY FILE lcSorDir+'glacchar.dbf' TO lcTrgDir+'glacchar.dbf'
    ENDIF
    *-- copy account code file.
    COPY FILE lcSorDir+'accod.cdx' TO lcTrgDir+'accod.cdx'
    COPY FILE lcSorDir+'accod.dbf' TO lcTrgDir+'accod.dbf'
      *-- copy sequence file.
    COPY FILE lcSorDir+'sequence.cdx' TO lcTrgDir+'sequence.cdx'
    COPY FILE lcSorDir+'sequence.dbf' TO lcTrgDir+'sequence.dbf'
    *-- copy fiscal year files.
    COPY FILE lcSorDir+'fishd.cdx' TO lcTrgDir+'fishd.cdx'
    COPY FILE lcSorDir+'fishd.dbf' TO lcTrgDir+'fishd.dbf'
    COPY FILE lcSorDir+'fishd.fpt' TO lcTrgDir+'fishd.fpt'
    COPY FILE lcSorDir+'fshld.cdx' TO lcTrgDir+'fshld.cdx'
    COPY FILE lcSorDir+'fshld.dbf' TO lcTrgDir+'fshld.dbf'
    COPY FILE lcSorDir+'fsprd.cdx' TO lcTrgDir+'fsprd.cdx'
    COPY FILE lcSorDir+'fsprd.dbf' TO lcTrgDir+'fsprd.dbf'
    *--- swich to view mode for not hanging when use scroll bal
  ELSE
    IF !GfGetMemVar('LLHIST',lcHComp)
      =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,lcHComp+" company has a History Company already.")
    ELSE
      =gfModalGen("TRM00000B00000","DIALOG","companies",.F., lcHComp+" is a History Company.")
    ENDIF
    _CUROBJ=OBJNUM(lcHComp)
    RETURN(.F.)
  ENDIF
ENDIF
*--- end of lfvHistCmp.

SELECT(lcOldAls)