*:************************************************************************
*:
*: Procedure file: APWSETUP.PRG        { Accounts payable setup }
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Ahmed Salah SHalaby - (SSH)
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfShow
*:               : lfvAcctCode
*:               : lfvBnkChk
*:               : lfvGlLink
*:               : lfvTaxType
*:               : lfvOk
*:               : lfOpnFiles
*:               : lfClsFiles
*:               : lfCanLink
*:               : lfwOldVal
*:               :
*:
*:************************************************************************
PARAMETER llLogic,lcCompID,lcCompPath,lcInstModl 

PRIVATE laBankObjs
DECLARE laSummary[11,2], laBankObjs[2,3]

** laBankObjs    Array to hold bank objects'names for validations      
** lcWinTtl    : Variable to hold.
** lcCmNam     : Variable to hold.
** lcTType     : Variable to hold.
** lcTClass    : Variable to hold.
** lcTBuyer    : Variable to hold.
** 
** lnNoOfPrd   : Variable to hold.
** lnOldPrds   : Variable to hold.
** lnOldBatch  : Variable to hold.
** lnOldTrans  : Variable to hold.
** lnObjNumb   : Variable to hold.
**
** llOldAcc    : Variable to hold.
** llOldCheck  : Variable to hold.
** llOldAdj    : Variable to hold.
** llOldDist   : Variable to hold.
** llOldDisc   : Variable to hold.
** llTermnat   : Variable to hold.
**
** rbTrans     : Variable to hold. 
** laSummary   : Variable to hold.

** llSetUsed   : A/P setup.
** llBnkUsed   : Banks file.
** llChkUsed   : Checks account file.
** llBrowse    : Browse flag.

STORE .F. TO llOldAcc  , llOldCheck , llOldAdj   , llOldDist  ,;
             llOldDisc , llTermnat

STORE 0   TO lnNoOfPrd , lnOldPrds  , lnOldBatch , lnOldTrans
STORE 1   TO lnObjNumb , rbTrans , lnUpDwn &&--- 1- first 3 objects ,, 2- the second group.

lcTitl1 = "Accounts Payable   "  &&--- Inotially  altarnative == "Adjustment "
lcTitl2 = "Expense          "    &&--- Inotially  altarnative == "Cash payment"
lcTitl3 = "Discount "            &&--- Inotially  altarnative == "Exchange rate differences"
lcDesc1 = ""
lcDesc2 = ""
lcDesc3 = ""
lcVar1  = ''
lcVar2  = ''
lcVar3  = ''

*--- Altarnative for each of the upove vairbale to control scrolling
*--- "IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[3],APACCHAR.cAcctCode,'AcctCode'),' ')" 
*--- "IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[4],APACCHAR.cAcctCode,'AcctCode'),' ')"
*--- "IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,lcAPExDifAcc,APACCHAR.cAcctCode,'AcctCode'),' ')"

STORE ''  TO lcWinTtl  , lcCmNam    , lcTType    , lcTClass   ,;
             lcTBuyer  , laSummary  , lcMdlName

STORE '' To  lc1099CmNu    
lcScFields = "CAPACCT,CEXPACCT,CADJACCT,CCASHACCT,CPVARACCT,CDISCACCT,CAPSGLLINK,CAPSGLCOM,LSETDON,CAPSVGR1D,CAPSVGR2D,CAPSVGR3D,CTAXTYPE,CBNKCODE,CCHKACCT,CAPSAASCM,NAPSRBPW,NAPSRELM,LAPSSAP,LAPSSCHK,LAPSSADJ,LAPSSDIST,LAPSSDISC,LAPS1099,CEXDIFACC,c1099spnu"
lcApSetup = gfTempName()
lcBanks   = gfTempName()
lcChecks  = gfTempName()
lcAcCod   = gfTempName()
lcWindTitl = "Accounts Payable Setup"

llDisable = lfEndDis()

lcWorkDir = IIF(EMPTY(lcCompPath),gcDataDir,lcCompPath)
lcUsedCom = IIF(EMPTY(lcCompPath),gcAct_Comp,lcCompID)

*** Initi. the variable that hold the old value of objects
lcOldVal   = ''           && Old value
lcParComp  = ''           && Parent company
lcSummary  = SPACE(40)

STORE .F.  TO llSetUsed,llBnkUsed,llChkUsed,llBrowse

rbTaxType  = 2
rbTrans    = 1

** Prepare an array to hold bank objects to be used for
** global bank and checking accounts validations as follows :
** One row for every object, such that 
** row no. 1 holds bank object names,
** row no. 2 holds checking account object names
** row no. 3 holds the corresponding G/L account object names,
** Columns are ordered as follows :
** Column no. 1 : invisible button name for corresponding object
** Column no. 2 : object name (e.g. bank object name)
** Column no. 3 : object description name(if required)
laBankObjs  = ' '
laBankObjs[1,1] = 'ibBnkCode'      && Bank code invisible button
laBankObjs[1,2] = 'laData[14]'     && Bank code 
laBankObjs[2,1] = 'ibChkCode'      && Checking account invisible button
laBankObjs[2,2] = 'laData[15]'     && Checking account 

*** Get the perant company and the data path
SELECT SYWCOMP
SET ORDER TO TAG CCOMP_ID

lcParComp = LOOKUP(ccompprnt,lcUsedCom,ccomp_id,'ccomp_id')
lcParComp = IIF(EMPTY(lcParComp),lcUsedCom,lcParComp)

*** Check if user can link with G/L or not
lnLinkMsg = 0
llCanLink = .F. 

=lfCanLink()

*** Open needed files Chart,Banks,Checks files
=lfOpnFiles()
=gfOpenFile(gcsyshome+"SYWAPSET","","SH")
lcApsAcMas = &lcAcCod..cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(SYWAPSET.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
lcEmptyAcc = REPLICATE('0',lnApsAcLen)

SELECT(lcApSETUP)
*** This information is entered for first time
GO TOP
IF RECCOUNT() < RECNO()
  SCATTER FIELDS &lcScFields TO laData BLANK 
  STORE lcEmptyAcc TO laData[1], laData[2], laData[3],;
                      laData[4], laData[5], laData[6]
  laData[8]  = lcParComp
  laData[9]  = .F.
  laData[13] = 'T'
  laData[18] = 1
ELSE
  SCATTER FIELDS &lcScFields TO laData
  FOR lnCount = 1 TO 6
    laData[lnCount] = STRTRAN(SUBSTR(laData[lnCount],1,lnApsAcLen),' ','0')
  ENDFOR
ENDIF

cbApAcc    = laData[19]
cbCheck    = laData[20]
cbAdjAcc   = laData[21]
cbDistrib  = laData[22]
cbDiscount = laData[23]
cb1099     = laData[24]
lcVar1  = laData[1]
lcVar2  = laData[2]
lcVar3  = laData[6]

lcDesc1 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[3],APACCHAR.cAcctCode,'AcctCode'),' ')
lcDesc2 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[4],APACCHAR.cAcctCode,'AcctCode'),' ')
lcDesc3 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[6],APACCHAR.cAcctCode,'AcctCode'),' ')
lc1099spnu = laData[26]
lcSavAls = ALIAS()
SELECT SYWCOMP

lcCmNam   = LOOKUP(ccom_name,lcParComp,ccomp_id,'ccomp_id')
lcCompId = lcParComp+" - "+lcCmNam
IF llLogic 
  laData[7] = IIF(IIF('GL' $ lcInstModl,.T.,.F.),laData[7],'N')
  laData[8] = IIF(laData[7] = 'Y',lcParComp,'  ')
  =SEEK(lcCompID) 
  lcApsAcMas = &lcAcCod..cAcsMask
  lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(SYWAPSET.cApsgllink='Y','9','X'))
  lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
  lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
ENDIF
SELECT SYWAPSET
llApS1099=lApS1099
*** This line is very important to refresh the station buffer with new 
*** information from the disk
llApGlLink = .F.
GO TOP
IF !EOF()
  GO 1
  llApGlLink = (CAPSGLLINK = 'Y')
ENDIF  
lcExDifAcc  = IIF(llApGlLink, IIF(EMPTY(AGLSETUP.CSETEXMJ), lcEmptyAcc,;
							  ALLTRIM(AGLSETUP.CSETEXMJ) + ;
                              STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"9","0")),;
                              gfGetMemVar('LNEXRATACC', gcAct_Comp))

lcApsAcMas = STRTRAN(lcApsAcMas,'9','X')

lcAPExDifAcc = IIF(gfGetMemVar('LLMULCURR'), lcExDifAcc, '') 

SELECT (lcSavAls)
*wab
*DO (gcScrDir +  gcAct_Appl + '\apwsetup.SPR')
DO (gcScrDir +  "SM\" + '\apwsetup.SPR')
*wab "SM\"
=lfClsFiles()

SELECT SYWAPSET
RELEASE WINDOW (gcBaseWind)

*!************************************************************************
*!
*!      Function : lfShow
*!
*!************************************************************************
*
FUNCTION lfShow

*** Show if A/P linked with G/L 
cbGlLink  = IIF(laData[7]='Y',.T.,.F.)
IF llDisable
  SHOW GET cbGlLink DISABLE
ELSE
  SHOW GET cbGlLink ENABLE  
ENDIF
*** Show tax type
rbTaxType = AT(laData[13],'LT')

*** Default group title fields if empty
laData[10] = IIF(EMPTY(laData[10]),lcTType,laData[10])
laData[11] = IIF(EMPTY(laData[11]),lcTClass,laData[11])
laData[12] = IIF(EMPTY(laData[12]),lcTBuyer,laData[12])

*** Check if the bank code is empty ot not
IF EMPTY(laData[14])
  *** If empty disable check code
  SHOW GET laData[15] DISABLE
  SHOW GET ibChkCode DISABLE
ELSE
  *** If not enable check code
  SHOW GET ibChkCode  ENABLE
  SHOW GET laData[15] ENABLE
ENDIF

IF cbGlLink
  laData[7] = 'Y'
  SHOW GET ibAcct1  ENABLE
  SHOW GET ibAcct2  ENABLE
  IF lnUpDwn = 1 
    SHOW GET ibAcct3  ENABLE
  ENDIF
  SHOW GET ibAcct4  ENABLE
  SHOW GET ibAcct5  ENABLE
  SHOW GET ibAcct6  ENABLE
  SHOW GET pbFormat ENABLE
ELSE
  laData[7] = 'N'
  SHOW GET ibAcct1  DISABLE
  SHOW GET ibAcct2  DISABLE
  SHOW GET ibAcct3  DISABLE
  SHOW GET ibAcct4  DISABLE
  SHOW GET ibAcct5  DISABLE
  SHOW GET ibAcct6  DISABLE
  SHOW GET pbFormat DISABLE
ENDIF
laData[8] = IIF(laData[7] = 'Y',lcParComp,'  ')
=lfRefresh()

*!************************************************************************
*!
*!      Function : lfwOldVal
*!
*!************************************************************************
* Save the old value before edit any object
*
FUNCTION lfwOldVal 

lcOldVal = EVAL(VARREAD())

*!************************************************************************
*!
*!      Function : lfvAcctCode
*!
*!************************************************************************
* Check the account code and return the account description if the 
* A/P linked with G/L
*
FUNCTION lfvAcctCode

lcRpCurFld = VARREAD()

IF !llBrowse .AND. lcOldVal = EVAL(lcRpCurFld)
  RETURN
ENDIF

DECLARE laTemp(2)
lcOldBrFld = lcBrFields
lcBrFields = 'CACCTCODE :H="Account code",CACCNLDES :H="Description"'
laTemp[1]  = ''
laTemp[2]  = ''

** Check If year field is empty
IF (!EMPTY(ALLTRIM(&lcRpCurFld.)) AND cbGlLink) OR llBrowse
  lcOldAlias = SELECT()    && Save the current alias
  SELECT APACCHAR
  *** Search for the account code
  IF (llBrowse .OR. '?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.))
    =gfBrows([],'CAcctCode,CAccnlDes',"laTemp",'Account Codes ',.F.)
    IF !EMPTY(laTemp[1])
      &lcRpCurFld = laTemp[1]
    ELSE
      &lcRpCurFld = lcOldVal
    ENDIF
  ENDIF
  SELECT (lcOldAlias)
ENDIF
=lfRefresh()
IF llBrowse
  _CUROBJ = _CUROBJ
  llBrowse = .F.
ENDIF
=lfUpdVar()
=lfShowGet()

*!************************************************************************
*!
*!      Function : lfvBnkChk
*!
*!************************************************************************
* Bank code and checking account validation
*
FUNCTION lfvBnkChk

IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse,' ', lcBanks, lcChecks)
  RETURN 1
ENDIF  

*!************************************************************************
*!
*!      Function : lfvGlLink
*!
*!************************************************************************
* This function makes transfare between .T. OR .F. and 'Y' OR 'N'
* when select G/L link or not

FUNCTION lfvGlLink

IF llCanLink
  IF cbGlLink
    laData[7] = 'Y'
    IF gfGetMemVar('LLMULCURR')
	  lcAPExDifAcc = IIF(EMPTY(AGLSETUP.CSETEXMJ), lcEmptyAcc,;
	  					 ALLTRIM(AGLSETUP.CSETEXMJ) + ;
                         STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"X","0"))
    ELSE
      lcAPExDifAcc = lcEmptyAcc
    ENDIF
    SHOW GET ibAcct1  ENABLE
    SHOW GET ibAcct2  ENABLE
    IF lnUpDwn = 1 
      SHOW GET ibAcct3  ENABLE
    ENDIF
    SHOW GET ibAcct4  ENABLE
    SHOW GET ibAcct5  ENABLE
    SHOW GET ibAcct6  ENABLE
    SHOW GET pbFormat ENABLE
  ELSE
    laData[7]  = 'N'
    IF gfGetMemVar('LLMULCURR')
      lcAPExDifAcc = gfGetMemVar('LNEXRATACC', gcAct_Comp)
    ELSE
      lcAPExDifAcc = lcEmptyAcc
    ENDIF
    SHOW GET ibAcct1  DISABLE
    SHOW GET ibAcct2  DISABLE
    SHOW GET ibAcct3  DISABLE
    SHOW GET ibAcct4  DISABLE
    SHOW GET ibAcct5  DISABLE
    SHOW GET ibAcct6  DISABLE
    SHOW GET pbFormat DISABLE
  ENDIF
  laData[8] = IIF(laData[7] = 'Y',lcParComp,'  ')
  lcOldAlias = SELECT()    && Save the current alias
  SELECT APACCHAR
  SELECT (lcOldAlias)
ELSE
  DO CASE
    CASE lnLinkMsg = 1
      =gfModalGen("TRM04068B00000","DIALOG")
    CASE lnLinkMsg = 2
      =gfModalGen("TRM04068B00000","DIALOG")
    CASE lnLinkMsg = 3
      =gfModalGen("TRM04069B00000","DIALOG")
  ENDCASE  
  cbGlLink = .F.
  SHOW GET cbGlLink
ENDIF
=lfRefresh()
=lfUpdVar()

*!************************************************************************
*!
*!      Function : lfvTaxType
*!
*!************************************************************************
* This function makes transfare between 1 OR 2 and 'T' OR 'L'
* when select tax type

FUNCTION lfvTaxType

laData[13] = SUBSTR('LT',rbTaxType,1)

*!************************************************************************
*!
*!      Function : lfvOk
*!
*!************************************************************************
*
FUNCTION lfvOk


** Check if any information not compleate
DO CASE
  CASE EMPTY(STRTRAN(STRTRAN(laData[1],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[1],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
    =gfModalGen("TRM04077B00000","DIALOG",'A/P')
    _CUROBJ = OBJNUM(laData[1])
    SHOW GET laData[1]
    RETURN


  CASE EMPTY(STRTRAN(STRTRAN(laData[2],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[2],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
    =gfModalGen("TRM04077B00000","DIALOG",'Expense')
    _CUROBJ = OBJNUM(laData[2])
    SHOW GET laData[2]
    RETURN

  CASE EMPTY(STRTRAN(STRTRAN(laData[3],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[3],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
    =gfModalGen("TRM04077B00000","DIALOG",'Adjustment')
    _CUROBJ = OBJNUM(laData[3])
    SHOW GET laData[3]
    RETURN

  CASE EMPTY(STRTRAN(STRTRAN(laData[4],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[4],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
    =gfModalGen("TRM04077B00000","DIALOG",'Cash payment')
    _CUROBJ = OBJNUM(laData[4])
    SHOW GET laData[4]
    RETURN
  CASE EMPTY(STRTRAN(STRTRAN(laData[6],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[6],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
    =gfModalGen("TRM04077B00000","DIALOG",'Discount')
    _CUROBJ = OBJNUM(laData[6])
    SHOW GET laData[6]
    RETURN
  OTHERWISE
   
    *** Check if accounts exist in accounts chart or not if the A/P linked 
    *** with G/L
    IF laData[7] = 'Y'
      SELECT(lcAPSETUP)
      *** If this information entered for first time
      GO TOP
      IF EOF()
        APPEND BLANK
      ENDIF  
      laData[9]  = .T.
      GATHER FROM laData FIELDS &lcScFields
      =gfAdd_Info(lcAPSETUP)
      *** We have to enhance the speed of disk refreshing so other 
      *** programes cansence the new data in the A/P setup file
      FLUSH

      *** Let account codes structure used
      SELECT (lcAcCod)
      REPLACE LACSUSED WITH .T.
      =lfUpdatLnk()
      SELECT(lcAPSETUP)
      glQuitting = .T.
      CLEAR READ
    ELSE
      laData[8]  = "  "
      laData[17] = 1
      laData[18] = 1
      laData[19] = .F.
      laData[20] = .F.
      laData[21] = .F.
      laData[22] = .F.
      laData[23] = .F.
      SELECT(lcAPSETUP)
      GO TOP
      IF EOF()
        APPEND BLANK
      ENDIF  
      laData[9] = .T.
      GATHER FROM laData FIELDS &lcScFields
      =gfAdd_Info(lcAPSETUP)
      FLUSH
      =lfUpdatLnk()
      glQuitting = .T.
      CLEAR READ
    ENDIF
ENDCASE

*!************************************************************************
*!
*!      Function : lfOpnFiles
*!
*!************************************************************************
* Open all needed files

FUNCTION lfOpnFiles

IF llCanLink
  SELECT 0
  USE &gcSysHome.SYWGLCHR AGAIN ALIAS APACCHAR
  SELECT APACCHAR
  SET ORDER TO TAG ACCTCODE
ENDIF
USE &gcSysHome.SYWAPSET AGAIN ALIAS &lcApSetup IN 0
USE (gcSysHome + 'SYWACCOD') AGAIN ALIAS (lcAcCod) ORDER TAG ACCSEGNO IN 0

*!************************************************************************
*!
*!      Function : lfClsFiles
*!
*!************************************************************************
* Close all the files were opened by this program
*
Function lfClsFiles

IF llCanLink
  USE IN APACCHAR
ENDIF

IF USED("AGLSETUP")
  USE IN AGLSETUP
ENDIF
USE IN &lcApSetup

IF USED(lcAcCod)
  USE IN (lcAcCod)
ENDIF  

*!************************************************************************
*!
*!      Function : lfCanLink
*!
*!************************************************************************
*
FUNCTION lfCanLink

lcOldAlias = SELECT()

SELECT SYWCOMP

=SEEK(lcParComp)

IF IIF(llLogic,'GL' $ lcInstModl,'GL' $ SYWCOMP.Mcomp_mdl)
  IF FILE(gcSysHome+'SYWGLSET.DBF')
    SELECT 0
    USE &gcSysHome.SYWGLSET AGAIN ALIAS AGLSETUP
    
    IF AGLSETUP.Lsetdon
      llCanLink = .T.
      lnNoOfPrd = AGLSETUP.NSETBATPW
    ELSE
      llCanLink = .F.
      lnLinkMsg = 1
    ENDIF
  ELSE
    llCanLink = .F.
    lnLinkMsg = 2
  ENDIF
ELSE
  lnLinkMsg = 3
  llCanLink = .F.
ENDIF

SELECT (lcOldAlias)

*!************************************************************************
*!
*!      Function : lfvFormat
*!
*!************************************************************************
*
FUNCTION lfvFormat

lnOldBatch = laData[17]
lnOldTrans = laData[18]
llOldAcc   = laData[19]
llOldCheck = laData[20]
llOldAdj   = laData[21]
llOldDist  = laData[22]
llOldDisc  = laData[23]

cbApAcc    = laData[19]
cbCheck    = laData[20]
cbAdjAcc   = laData[21]
cbDistrib  = laData[22]
cbDiscount = laData[23]

rbTrans    = IIF(laData[18] > 1,2,1)
laData[18] = IIF(laData[18] > 1,laData[18]-1,1)
laData[17] = IIF(laData[17] = 0,1,laData[17])
lnObjNumb  = OBJNUM(pbFormat)
DO (gcScrDir +  gcAct_Appl + '\APSETRL.SPR')

*!************************************************************************
*!
*!      Function : lfShowSRF
*!
*!************************************************************************
*
FUNCTION lfShowSRF

IF rbTrans = 2
  IF _DOS
    lcSummary = laSummary[laData[18],1]
    SHOW GET ibSummary ENABLE
    =lfRefresh2()
  ELSE
    puSummary = INT(VAL(laSummary[laData[18],2]))
    SHOW GET puSummary ENABLE
  ENDIF
ELSE
  IF _DOS
    SHOW GET ibSummary DISABLE
    lcSummary = SPACE(40)
    =lfRefresh2()
  ELSE
    SHOW GET puSummary DISABLE
  ENDIF  
ENDIF

*!************************************************************************
*!
*!      Function : lfvData_17
*!
*!************************************************************************
*
FUNCTION lfvData_17

IF laData[17] <= 0 
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'Entered value|zero')
  SHOW GET laData[17]
  _CUROBJ = _CUROBJ
  RETURN
ELSE
  IF laData[17] > lnNoOfPrd 
    ** MESSAGE : " The batch posting window in the GL setup "
    **           " is setup to be maximum ð periods.  You   "
    **           " cannot create one batche for number of   "
    **           " periods that exceeds the batch posting   "
    **           " window.                                  "
    **           "                      ® Ok ¯              "
    =gfModalGen("TRM04129B00000","DIALOG",ALLTRIM(STR(lnNoOfPrd)))
    SHOW GET laData[17]
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
ENDIF  

*!************************************************************************
*!
*!      Function : lfvTrans
*!
*!************************************************************************
*
FUNCTION lfvTrans

IF rbTrans = 2
  puSummary = INT(VAL(laSummary[laData[18],2]))
  SHOW GET puSummary ENABLE
ELSE
  puSummary = 0
  SHOW GET puSummary DISABLE
  laData[18] = 1
ENDIF

*!**************************************************************************
*!
*!      Function: lfwSummar
*!
*!**************************************************************************
*
FUNCTION lfwSummar

lnOldPrds = laData[18]

*!**************************************************************************
*!
*!      Function: lfvSummar
*!
*!**************************************************************************
*
FUNCTION lfvSummar
 
DO CASE
  laData[18] = INT(VAL(laSummary[puSummary,2]))
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvSetDef
*!
*!**************************************************************************
*
FUNCTION lfvSetDef

laData[18] = IIF(rbTrans = 1,1,laData[18]+1)
laData[19] = cbApAcc
laData[20] = cbCheck
laData[21] = cbAdjAcc
laData[22] = cbDistrib
laData[23] = cbDiscount

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvClsDef
*!
*!**************************************************************************
*
FUNCTION lfvClsDef

laData[17] = lnOldBatch
laData[18] = lnOldTrans
laData[19] = llOldAcc
laData[20] = llOldCheck
laData[21] = llOldAdj
laData[22] = llOldDist
laData[23] = llOldDisc
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvClsDef
*!
*!**************************************************************************
* valid function for suppress 1099 processing
FUNCTION lfvSup1099
laData[24] = cb1099


*!**************************************************************************
*!
*!      Function: lfUpdatLnk
*!
*!**************************************************************************
*
***  Added by Yasser on 3-11-95
FUNCTION lfUpdatLnk

IF !llCanLink
  RETURN
ENDIF

SELECT AGLSETUP
DECLARE laLinkApp[1,2]
=gfSubstr(AGLSETUP.mLinkApp,@laLinkApp,"|~")
lnApRow = ASCAN(laLinkApp,"AP")

IF laData[7] = 'Y'
  IF lnApRow = 0
    DECLARE laLinkApp[ALEN(laLinkApp,1)+1,2]
    laLinkApp[ALEN(laLinkApp,1),1] = "AP"
    laLinkApp[ALEN(laLinkApp,1),2] = ALLTRIM(lcMdlName)
   ENDIF
ELSE
   IF lnApRow > 0
     IF ALEN(laLinkApp,1) > 1
       lnApRow = ASUBSCRIPT(laLinkApp,lnApRow,1)
       =ADEL(laLinkApp,lnApRow,1)
       DECLARE laLinkApp[ALEN(laLinkApp,1)-1,2] 
     ELSE
       laLinkApp = ""
     ENDIF 
   ENDIF
ENDIF

lcCol1 ="" 
lcCol2 ="" 
   
FOR lnLnkApp = 1 TO ALEN(laLinkApp,1)
  lcCol1 = lcCol1+IIF(EMPTY(lcCol1),"","|")+ALLTRIM(laLinkApp[lnLnkApp,1])
  lcCol2 = lcCol2+IIF(EMPTY(lcCol2),"","|")+ALLTRIM(laLinkApp[lnLnkApp,2])
ENDFOR

IF !EMPTY(lcCol1) .AND. !EMPTY(lcCol2)
  REPLACE AGLSETUP.mLinkApp WITH ALLTRIM(lcCol1)+'~'+ALLTRIM(lcCol2)
ELSE
  REPLACE AGLSETUP.mLinkApp WITH ""
ENDIF  
USE IN AGLSETUP

*!********************************************************************************
*! Name      : lfvstpay
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 01/30/2000
*! Purpose   : for the State/Payers state number field.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: lcFile->Locked file.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfRecLock()
*!********************************************************************************
FUNCTION lfvstpay
laData[26] = lc1099spnu

*--END OF lfvstpay.

*!*************************************************************
*! Name      : lfvAPNext  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function for next button...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvAPNext()
*!*************************************************************
*!
FUNCTION lfvAPNext
glQuitting = .T.
lcNextActn = 'N'
=lfvOk()

*!*************************************************************
*! Name      : lfvAPPre  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function for previous button...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvAPPre()
*!*************************************************************
*!
FUNCTION lfvAPPre
glQuitting = .T.
lcNextActn = 'B'
=lfvOk()

*!*************************************************************
*! Name      : lfvAPResu  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function for resume later button...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvAPResu()
*!*************************************************************
*!
FUNCTION lfvAPResu
lcNextActn = 'U'
glQuitting = .T.
CLEAR READ

*!*************************************************************
*! Name      : lfvDwn  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function down scroll bar...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvDwn()
*!*************************************************************
*!
FUNCTION lfvDwn

lnUpDwn = 2
SHOW GET pbDwn DISABLE
SHOW GET pbup  ENABLE
lcTitl1 = "Adjustment "
lcTitl2 = "Cash payment"
lcTitl3 = "Exchange rate differences"
lcDesc1 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[3],APACCHAR.cAcctCode,'AcctCode'),' ')
lcDesc2 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[4],APACCHAR.cAcctCode,'AcctCode'),' ')
lcDesc3 = IIF(EMPTY(lcAPExDifAcc),'',IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,lcAPExDifAcc,APACCHAR.cAcctCode,'AcctCode'),' '))

lcVar1  = laData[3]
lcVar2  = laData[4]
lcVar3  = lcApExDifAcc
_CUROBJ = OBJNUM(lcVar1)

=lfShowGet()

*!*************************************************************
*! Name      : lfvUp  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function up scroll bar...
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvUp()
*!*************************************************************
*!
FUNCTION lfvUp

lnUpDwn = 1
SHOW GET pbDwn ENABLE
SHOW GET pbup  DISABLE
lcTitl1 = "Accounts Payable   "
lcTitl2 = "Expense          "
lcTitl3 = "Discount "
lcDesc1 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[1],APACCHAR.cAcctCode,'AcctCode'),' ')
lcDesc2 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[2],APACCHAR.cAcctCode,'AcctCode'),' ')
lcDesc3 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[6],APACCHAR.cAcctCode,'AcctCode'),' ')
lcVar1  = laData[1]
lcVar2  = laData[2]
lcVar3  = laData[6]
=lfShowGet()
_CUROBJ = OBJNUM(lcVar1)

*!*************************************************************
*! Name      : lfShowGet  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to show get.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfShowGet()
*!*************************************************************
*!
FUNCTION lfShowGet

SHOW GET lcDesc1
SHOW GET lcDesc2
SHOW GET lcDesc3
=lfRefresh()
SHOW GET lcVar1
SHOW GET lcVar2
SHOW GET lcVar3
IF lnUpDwn = 1 .AND. laData[7] = 'Y'
  SHOW GET ibAcct3 ENABLE
ELSE
  SHOW GET ibAcct3 DISABLE
ENDIF


*!*************************************************************
*! Name      : lfScrolVar  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : scrol bar valid function.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfScrolVar()
*!*************************************************************
*!
FUNCTION lfScrolVar

IF lnUpDwn = 1
  laData[1] = lcVar1
  laData[2] = lcVar2
  laData[6] = lcVar3
ELSE
  laData[3]    = lcVar1
  laData[4]    = lcVar2
  lcApExDifAcc = lcVar3
ENDIF


*!*************************************************************
*! Name      : lfUpdVar  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : update variable function.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfUpdVar()
*!*************************************************************
*!
FUNCTION lfUpdVar

=lfScrolVar()
IF lnUpdwn = 1
  lcDesc1 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[1],APACCHAR.cAcctCode,'AcctCode'),' ')
  lcDesc2 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[2],APACCHAR.cAcctCode,'AcctCode'),' ')
  lcDesc3 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[6],APACCHAR.cAcctCode,'AcctCode'),' ')
ELSE
  lcVar3  = lcApExDifAcc
  lcDesc1 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[3],APACCHAR.cAcctCode,'AcctCode'),' ')
  lcDesc2 = IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,laData[4],APACCHAR.cAcctCode,'AcctCode'),' ')
  lcDesc3 = IIF(EMPTY(lcAPExDifAcc),'',IIF(laData[7] = 'Y',LOOKUP(APACCHAR.CACCNLDES,lcAPExDifAcc,APACCHAR.cAcctCode,'AcctCode'),' '))
ENDIF
=lfShowGet()

*!*************************************************************
*! Name      : lfEndDis  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Enable disale .
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfEndDis()
*!*************************************************************
*!
FUNCTION lfEndDis
PRIVATE lnOldAls , llReturn
llReturn = .T.
lnOldAls = SELECT(0)
IF !USED("SYWACHRT")
   = gfOpenFile(gcSysHome+"SYWACHRT","Cidentify","SH")
ENDIF
SELECT SYWACHRT
LOCATE FOR lSelect
IF FOUND()
  llReturn = (VAL(cIdentify) = 1)
ENDIF
SELECT(lnOldAls)
RETURN(llReturn)