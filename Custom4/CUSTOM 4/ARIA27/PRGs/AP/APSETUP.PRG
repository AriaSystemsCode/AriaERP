*:************************************************************************
*:
*: Procedure file: APSETUP.PRG        { Accounts payable setup }
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Albert Raif
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
*:      Documented 00/00/1994
*:************************************************************************
* 
*B600314,1 MAN 05/27/95 Close alias AGLSETUP If you close the scr.
*E300258,1 HESHAM EL-SHELTAWI 06/29/95
*E300258,1 added the field lApS1099 to be used in all the screens using the
*E300258,1 1099 FORMS
*B600814,1 M.H 12/03/95 The Exchange difference account is invalid when there is no GL link.
*B601013,1 RENEE 04/08/95. Exchange difference account is changed to a 
*B601013,1                 display the exchange difference account (non editable)
*B601013,1                 - Removed laDAta[25] and ibAcct7 from the screen APSETUP.SCX,;
*B601013,1                   substituted it with variable lcAPExDiffAcc
*B601308,1 RENEE 11/10/96. Remove extra title in APSETUP.SCX
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300692,5 RENEE 07/02/97 Open ACCOD from the calling company
*B601993,1 RENEE 03/29/98. Fix the format of an empty exchange difference account
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301077,80 IHB   03/03/1999 Enhance opening and closing files
*B802949,1  ABD   01/24/2000 Add the State/Payers state number field to
*B802949,1                   the APSETUP screen & and APVENDOR File.
*B606903,1 KHM 02/22/2003 Fix the bug of allowing the user to select inactive account
*B609008,1 MMT 09/14/2009 Fix Error on SAAS While editing AP setup
*******************************************************************************


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

**B802949,1 ABD Add the State/Payers state number field to the APSETUP
**B802949,1     screen And APVENDOR File [ Begin ]
**lc1099CmNu   : Variable to hold.
**B802949,1 ABD [ End ]

STORE .F. TO llOldAcc  , llOldCheck , llOldAdj   , llOldDist  ,;
             llOldDisc , llTermnat

STORE 0   TO lnNoOfPrd , lnOldPrds  , lnOldBatch , lnOldTrans

STORE 1   TO lnObjNumb , rbTrans

STORE ''  TO lcWinTtl  , lcCmNam    , lcTType    , lcTClass   ,;
             lcTBuyer  , laSummary  , lcMdlName

**B802949,1 ABD Add the State/Payers state number field to the APSETUP
**B802949,1     screen And APVENDOR File [ Begin ]
STORE '' To  lc1099CmNu    
**B802949,1 ABD [ End ]
             
             

IF !gfSetup()
  RETURN
ENDIF

lcApSetup = gfTempName()
lcBanks   = gfTempName()
lcChecks  = gfTempName()
*E300692,5 Add temp name for ACCOD alias
lcAcCod   = gfTempName()
*E300692,5 end

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
SELECT SYCCOMP
SET ORDER TO TAG CCOMP_ID

lcParComp = LOOKUP(ccompprnt,lcUsedCom,ccomp_id,'ccomp_id')
lcParComp = IIF(EMPTY(lcParComp),lcUsedCom,lcParComp)

*E301098,1 Hesham (Start)
*lcDataDir = ALLTRIM(LOOKUP(ccom_ddir,lcParComp,ccomp_id,'ccomp_id'))
lcDataDir = gfGetDataDir(ALLTRIM(LOOKUP(ccom_ddir,lcParComp,ccomp_id,'ccomp_id')))
*E301098,1 Hesham (End)

*B609008,1 MMT 09/14/2009 Fix Error on SAAS While editing AP setup[Start]
*IF glMapPath
IF glMapPath AND !GCMULTIINST
*B609008,1 MMT 09/14/2009 Fix Error on SAAS While editing AP setup[End]

  lcDataDir = gfFixPath(lcDataDir)
ENDIF

*** Check if user can link with G/L or not
lnLinkMsg = 0
llCanLink = .F. 

=lfCanLink()

*** Open needed files Chart,Banks,Checks files
=lfOpnFiles()

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
  IF llCanLink
    SELECT APACCHAR
  ENDIF    
ENDIF

cbApAcc    = laData[19]
cbCheck    = laData[20]
cbAdjAcc   = laData[21]
cbDistrib  = laData[22]
cbDiscount = laData[23]
cb1099     = laData[24]

*B802949,1 ABD Add the State/Payers state number field to the APSETUP
*B802949,1     screen And APVENDOR File [ Begin ]
lc1099spnu = laData[26]
*B802949,1 ABD [ End ]



lcSavAls = ALIAS()


SELECT SYCCOMP

lcCmNam   = LOOKUP(ccom_name,lcParComp,ccomp_id,'ccomp_id')

IF llLogic 
  laData[7] = IIF(IIF('GL' $ lcInstModl,.T.,.F.),laData[7],'N')
  laData[8] = IIF(laData[7] = 'Y',lcParComp,'  ')
  =SEEK(lcCompID) 
  *E300692,5 CHANGE FILE NAME FROM SYCACCOD TO 
  *lcApsAcMas = SYCACCOD.cAcsMask
  lcApsAcMas = &lcAcCod..cAcsMask
  *E300692,5 end
  lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
  lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
  lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
ENDIF

lcApsAcMas = STRTRAN(lcApsAcMas,'9','X')

*B601013,1 Remove laData[25] and substitute with say field, lcAPExDifAcc
*B601013,1 lcExDifAcc is initializeds with the appropriate exchange difference
*B601013,1 account in AP.PRG
*IF gfGetMemVar('LLMULCURR')
*  laData[25] = IIF(laData[7] = 'Y',lcExDifAcc,'')
*ENDIF  
lcAPExDifAcc = IIF(gfGetMemVar('LLMULCURR'), lcExDifAcc, '') 
*B601013,1 end.

SELECT (lcSavAls)


*E300683,1 Call *.SPR from screens directory
*DO APSETUP.SPR
DO (gcScrDir +  gcAct_Appl + '\APSETUP.SPR')
*E300683,1 end

=lfClsFiles()

SELECT APSETUP
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
  SHOW GET ibAcct3  ENABLE
  SHOW GET ibAcct4  ENABLE
  SHOW GET ibAcct5  ENABLE
  SHOW GET ibAcct6  ENABLE
  SHOW GET pbFormat ENABLE
  *B601013,1 Remove ibAcct7 button and laData[25] get field from the screen
  *SHOW GET ibAcct7  DISABLE
  *SHOW GET laData[25] DISABLE
  *B601013,1 end.
ELSE
  laData[7] = 'N'
  SHOW GET ibAcct1  DISABLE
  SHOW GET ibAcct2  DISABLE
  SHOW GET ibAcct3  DISABLE
  SHOW GET ibAcct4  DISABLE
  SHOW GET ibAcct5  DISABLE
  SHOW GET ibAcct6  DISABLE
  SHOW GET pbFormat DISABLE
  *B601013,1 Remove ibAcct7 button from the screen
  *SHOW GET ibAcct7  DISABLE
  *B601013,1 end.
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

  *B606903,1 KHM 02/22/2003 (Begin) Do not select the inactive account
  IF !EOF() AND cSegActiv = "I"
    =gfModalGen("TRM04202B00000","DIALOG",ALLTRIM(&lcRpCurFld))
    &lcRpCurFld = lcOldVal
    _CUROBJ = _CUROBJ
  ENDIF
  *B606903,1 KHM 02/22/2003 (End)

  SELECT (lcOldAlias)
ENDIF

=lfRefresh()

IF llBrowse
  _CUROBJ = _CUROBJ
  llBrowse = .F.
ENDIF

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
      
      *B601013,1 Remove ibAcct7 button and laData[25] get fields from the screen
      *laData[25] = lcExDifAcc
      *SHOW GET ibAcct7  DISABLE
      *B601013,1 end.
      *SHOW GET laData[25] DISABLE
      *B601993,1 Initialize with an empty account if the G/L exchange difference account
	  *B601993,1 is empty
	  *lcAPExDifAcc = ALLTRIM(AGLSETUP.CSETEXMJ) + ;
                     STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"X","0")
	  lcAPExDifAcc = IIF(EMPTY(AGLSETUP.CSETEXMJ), lcEmptyAcc,;
	  					 ALLTRIM(AGLSETUP.CSETEXMJ) + ;
                         STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"X","0"))
      *B601993,1 end
      *B601013,1 end.
    ELSE
      *B601013,1 Remove laData[25] get fields from the screen
      *laData[25] = lcEmptyAcc
      lcAPExDifAcc = lcEmptyAcc
      *B601013,1 end.
    ENDIF
    SHOW GET ibAcct1  ENABLE
    SHOW GET ibAcct2  ENABLE
    SHOW GET ibAcct3  ENABLE
    SHOW GET ibAcct4  ENABLE
    SHOW GET ibAcct5  ENABLE
    SHOW GET ibAcct6  ENABLE
    SHOW GET pbFormat ENABLE
  ELSE
    laData[7]  = 'N'
    IF gfGetMemVar('LLMULCURR')
      *B601013,1 Remove ibAcct7 and laData[7] from the screen
      *laData[25] = IIF(EMPTY(APSETUP.CEXDIFACC),lcEmptyAcc,APSETUP.CEXDIFACC)
      *SHOW GET ibAcct7  DISABLE
      *SHOW GET laData[25] ENABLE
      *B601013,1 Get the sxchange difference account from the company setup
      lcAPExDifAcc = gfGetMemVar('LNEXRATACC', gcAct_Comp)
      *B601013,1 end.
    ELSE
      *B601013,1 Remove laData[7] from the screen
      *laData[25] = lcEmptyAcc
      lcAPExDifAcc = lcEmptyAcc
      *B601013,1 end.
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

 *MAN
 * CASE EMPTY(STRTRAN(STRTRAN(laData[5],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[5],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
 *   =gfModalGen("TRM04077B00000","DIALOG",'Purchase')
 *   _CUROBJ = OBJNUM(laData[5])
 *   SHOW GET laData[5]
 *   RETURN

  CASE EMPTY(STRTRAN(STRTRAN(laData[6],'0',' '),'-',' ')) .OR. (laData[7] = 'Y' .AND. !SEEK(laData[6],'APACCHAR'))
    ** Message : " ð account not valid.            "
    **           "              ® Ok ¯             "
    =gfModalGen("TRM04077B00000","DIALOG",'Discount')
    _CUROBJ = OBJNUM(laData[6])
    SHOW GET laData[6]
    RETURN

  *B601013,1 Remove this check since it is no longer required, will be 
  *B601013,1 managed from the company setup screen and the GLsetup screen.
  *CASE gfGetMemVar('LLMULCURR') .AND. (EMPTY(STRTRAN(STRTRAN(laData[25],'0',' '),'-',' ')) .OR.(laData[7] = 'Y' .AND. !SEEK(laData[6],'APACCHAR')))
  *  *B600814,1 M.H The Exchange difference account is invalid when there is no GL link.
  *  *    IF EMPTY(STRTRAN(STRTRAN(laData[25],'0',' '),'-',' ')) .OR. laData[7] = 'N'
  *  *B600814,1 M.H End.
  *  ** Message : " ð account not valid.            "
  *  **           "              ® Ok ¯             "
  *  =gfModalGen("TRM04077B00000","DIALOG",'Exchange difference')
  *  _CUROBJ = OBJNUM(laData[24])
  *  SHOW GET laData[24]
  *  RETURN
  *  *B600814,1 M.H
  *  *    ENDIF
  *  *B600814,1 M.H End.
  *B601013,1 end.
  
  CASE (EMPTY(laData[14]) AND RECCOUNT(lcBANKS)>0)
    ** Message : " You have to enter the ð ."
    **           "           ® Ok ¯         "
    =gfModalGen("TRM04066B00000","DIALOG",'bank code')
    _CUROBJ = OBJNUM(laData[14])
    SHOW GET laData[14]
    RETURN

  CASE (EMPTY(laData[15]) AND !EMPTY(laData[14]) AND RECCOUNT(lcCHECKS)>0)
    =gfModalGen("TRM04020B00000","DIALOG",'checking account')
    _CUROBJ = OBJNUM(laData[15])
    SHOW GET laData[15]
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

      *B601013,1 Remove laData[25] (old exchange difference account) 
      *B601013,1 from the APsetup file 
      *IF gfGetMemVar('LLMULCURR')
      *  laData[25] = IIF(laData[7] = 'Y','',laData[25])
      *ELSE
      *  laData[25] = IIF(laData[7] = 'N',laData[25],lcEmptyAcc)
      *ENDIF
      *B601013,1 end.
      
      GATHER FROM laData FIELDS &lcScFields
      =gfAdd_Info(lcAPSETUP)
      *** We have to enhance the speed of disk refreshing so other 
      *** programes cansence the new data in the A/P setup file
      FLUSH

      *** Let account codes structure used
      *E300692,5 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
      *SELECT SYCACCOD
      *SET ORDER TO TAG COMPID
      *=SEEK(lcParComp)
      SELECT (lcAcCod)
      *E300692,5 end
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
      *B601013,1 Remove laData[25] (old exchange difference account) 
      *B601013,1 from the APsetup file 
      *IF gfGetMemVar('LLMULCURR')
      *  laData[25] = IIF(laData[7] = 'Y','',laData[25])
      *ELSE
      *  laData[25] = IIF(laData[7] = 'N',laData[25],lcEmptyAcc)
      *ENDIF
      *B601013,1 end.
   
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
  USE &lcDataDir.GLACCHAR AGAIN ALIAS APACCHAR
  SELECT APACCHAR
  SET ORDER TO TAG ACCTCODE
ENDIF

*IF !USED(lcApSetup)
*SELECT 0
USE &lcWorkDir.APSETUP AGAIN ALIAS &lcApSetup IN 0
*  llSetUsed = .T.
*ENDIF

*SELECT(lcApSetup)

*IF !USED(lcBanks)
*  SELECT 0
USE &lcWorkDir.APBANKS AGAIN ALIAS &lcBanks IN 0 ORDER TAG BANKCODE
*  llBnkUsed = .T.
*ENDIF

*SELECT (lcBanks)
*SET ORDER TO TAG BANKCODE

*IF !USED(lcCHECKS)
*  SELECT 0
USE &lcWorkDir.APCHECKS AGAIN ALIAS &lcCHECKS IN 0 ORDER TAG BANKCHECK
*  llChkUsed = .T.
*ENDIF

*SELECT (lcCHECKS)
*SET ORDER TO TAG BANKCHECK

*E300692,5 Open ACCOD file from the parent's 
USE (lcDataDir + 'ACCOD') AGAIN ALIAS (lcAcCod) ORDER TAG ACCSEGNO IN 0
*E300692,5 end

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

*B600314,1 Closing the file if opened
IF USED("AGLSETUP")
  USE IN AGLSETUP
ENDIF

*IF llSetUsed
USE IN &lcApSetup
*  llSetUsed = .F.
*ENDIF

*IF llBnkUsed
  USE IN &lcBanks
*  llBnkUsed = .F.
*ENDIF

*IF llChkUsed
  USE IN &lcChecks
*  llChkUsed = .F.
*ENDIF

*E300692,5 close ACCOD
IF USED(lcAcCod)
  USE IN (lcAcCod)
ENDIF  
*E300692,5 end  



*!************************************************************************
*!
*!      Function : lfCanLink
*!
*!************************************************************************
*
FUNCTION lfCanLink

lcOldAlias = SELECT()

SELECT SYCCOMP

=SEEK(lcParComp)

IF IIF(llLogic,'GL' $ lcInstModl,'GL' $ SYCCOMP.Mcomp_mdl)
  IF FILE(lcDataDir+'GLSETUP.DBF')
    SELECT 0
    USE &lcDataDir.GLSETUP AGAIN ALIAS AGLSETUP
    
    IF AGLSETUP.Lsetdon
      llCanLink = .T.
      lnNoOfPrd = AGLSETUP.NSETBATPW
    ELSE
      llCanLink = .F.
      lnLinkMsg = 1
    ENDIF
    
    * USE IN AGLSETUP
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
llTermnat  = .T.
CLEAR READ

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
  IF _DOS
    SHOW GET ibSummary ENABLE
    lcSummary = laSummary[laData[18],1]
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
    puSummary = 0
    SHOW GET puSummary DISABLE
  ENDIF    
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
  CASE _DOS
    laData[18] = INT(VAL(gfActPop(8,2,14,60,'laSummary',2,1,@lcSummary)))
    =lfRefresh2()
  CASE _WINDOWS
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
*E300258,1 added the field lApS1099 to be used in all the screens using the
*E300258,1 1099 FORMS
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
* valid function for the State/Payers state number field.
*B802949,1  ABD Add the State/Payers state number field to
*B802949,1      the APSETUP screen & and APVENDOR File. 
FUNCTION lfvstpay
laData[26] = lc1099spnu

*--END OF lfvstpay.
*!**************************************************************************
