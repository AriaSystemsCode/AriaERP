*:************************************************************************
*:
*: Procedure file: APRCRIN.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Renee Ezzat
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow
*:               : lfvData_2
*:               : lfwOldVals
*:               : lfvVendor
*:               : lfvType
*:               : lfwPayMeth
*:               : lfvPayMeth
*:               : lfvData_5
*:               : lfvDuration
*:               : lfvData_7
*:               : lfvData_8
*:               : lfvData_11
*:               : lfvData_12
*:               : lfvData_13
*:               : lfvData_15
*:               : lfwDivision
*:               : lfvDivision
*:               : lfvAprovPay
*:               : lfwTermCode
*:               : lfvTermCode
*:               : lfvRemit
*:               : lfvDistrib
*:               : lfvAPDGlAct
*:               : lfBrowse
*:               : lfwDisBrow
*:               : lfvBrowse
*:               : lfvNew
*:               : lfvRem
*:               : lfwAccounts
*:               : lfvAccounts
*:               : lfDispObjct
*:               : lpSavScr
*:               : lpDelScr
*:               : lfActivate
*:               : lfvClsChld
*:               : lfGetArrElem
*:               : lfvPopups
*:               : lfvDistAmnt
*:               : lfwTaxCode
*:               : lfvTaxCode
*:               : lfvBnkChk
*:               : lfvAmounts
*:               : lfvData_27
*:               : lfvOkApr
*:               : lfvClrApr
*:               : lfvCanPay
*:               : lfLineNo
*:               : lfDefinePad
*:               : lfBrowTrap
*:               : lfBrowUnTrap
*:               : lfRelease
*:               : lpCtrlEntr
*:               : lpTab
*:               : lpShiftTab
*:               : lfvData_33
*:               :
*:
*:  Documented      /  /
*:************************************************************************
*E300296,1 M.H   10/10/95 Add the currency to the AP module.
*E300296,4 RENEE 10/25/95 Check Bank\checking account currency compatibility
*E500030,1 M.H   11/02/95 Change the Duration word in the screen to be Frequency.
*E300643,1  HS 04/15/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300643,1              3)Make some changes for we have change the function
*E300643,1              [gfRltFld]
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,80 IHB  03/03/1999 Enhance opening and closing files
*E300789,4 IHB  07/03/1999 Remove company ID from ACCOD, FISHD, FSPRD, FSHLD, CODES
*:************************************************************************

EXTERNAL ARRAY laData,laKeyField
PRIVATE laBankObjs, lcSetExact 
DECLARE laKeyField [2,4], laType[1,2]    , laDuration[1,2], ;
        laPayMeth[1,2]  , laTaxCode[1,2] , laRemitTo[3,2] , ;
        laTermCode[3,2], laBankObjs[3,3]
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'HTYPCOD'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'HTYPCOD'
laKeyField[2,4] = 2

DECLARE laWndObj[7,3]       && Have screen name & first & last obj. for each screen.

laDefProc[7]    = .F.       && Use a local delete procedure.
laDefProc[9]    = .F.       && Use a local save procedure.

** lcCompany      :  Company.
** lcPhone        :  Phone.
** lcRmtComp      :  Remit to company field.
** lcRmtAddr1     :  Remit to address1 field.
** lcRmtAddr2     :  Remit to address2 field.
** lcRmtAddr3     :  Remit to address3 field.

** lcType         :  Say field for type     popup (DOS).
** lcDuration     :  Say field for duration popup (DOS).
** lcTermCode     :  Say field for terms popup (DOS).
** lcDivision     :  Say field for divisions popup (DOS).
** lcPayMeth      :  Say field for payment method popup (DOS).
** lcTaxDes       :  Say field for tax code popup (DOS).
** lcRemitTo      :  Say field for remit to popup (DOS).
** lcCodeFilt     : 

** lcRcrChld1     :  Random name for the Dist. child screen no 1.
** lcRcrChld2     :  Random name for the Dist. child screen no 2.
** lcRcrChld3     :  Random name for the Dist. child screen no 3.
** lcRcrChld4     :  Random name for the Dist. child screen no 2.
** lcRcrChld5     :  Random name for the Dist. child screen no 3.

** lcBrowsTtl     :  Browse window title.
** lcTDstTitl     :  Child window title.
** lcTmpDist      :  Random name for the Distribution Temp. file.
** lcTaxCode      :  Tax code variable (of distribution lines).
** lcOldDesc      :  Old descriptions.
** lcOldVal       :  Old value of character fields.

** lcAccDesc      :  AP account description.
** lcApDGLDesc    :  Gl distribution account description.

** lcAcctStat     :  Distribution lines objects state.
** lcAmntStat     : 
** lcTaxStat      : 
** lcNewStat      :  New button display state.
** lcRemStat      :  Remove button display state.
** lcRemitStat    : 
** lcTypesStat    : 
** lcVendStat     :  Vendor display state.
** lcDataStat     : 
** lcFactStat     : 
** lcDButStat     : 
** lcData13Stat   : 
** lcAprvStat     : 
** lcAddStat      : 
** lcEditStat     : 
** lcOldCurr      :  Variable to hold the old currency.
**                
** lnTerDiscR     :  Term discount %.
** lnTerDiscD     :  Term discpunt days.
** lnTerDueD      :  Term due days.
** lnDistLines    :  Number of distribution lines.
** lnApdAmnt      :  Distributed amount per record.
** lnDistAmnt     :  Total distributed amount.
** lnTotAppPay    :  Total approved amount.
** lnOldVal       :  Old value of numeric fields.
** lnTypeLen      :  Types popup width.
** lnDurLen       :  Duration popup width.
** lnPMLen        :  Payment methos popup width.
** lnRemitLen     :  Remit to popup width.

** lnOldType      :  Old type popup value.
** lnOldDur       :  Old duration popup value.
** lnOldPayM      :  Old payment method popup value.
** lnOldRemit     :  Old remit to popup value.
** puType         :  Types popup (WINDOWS).
** puDuration     :  Duration popup (WINDOWS).
** puPayMeth      :  Payment method popup (WINDOWS).
** puRemitTo      :  Remit to popup (WINDOWS).

** ldOldVal       :  Old value of date fields.
** ldFisBgDt      :  Begin date of current fiscal year.
** ldFisEnDt      :  End date of current fiscal year.

** llBrowse       :  Browse invisible buttons flag.
** llFirstAdd     :  First entry in ADD mode.

STORE ' '       TO lcCompany  , lcPhone    , lcRmtComp  , lcRmtAddr1 ,;
                   lcRmtAddr2 , lcRmtAddr3 , lcType     , lcDuration ,;
                   lcTermCode , lcDivision , lcPayMeth  , lcTaxDes   ,;
                   lcRemitTo  , lcCodeFilt , lcRcrChld1 , lcRcrChld2 ,;
                   lcRcrChld3 , lcRcrChld4 , lcRcrChld5 , lcBrowsTtl ,;
                   lcTDstTitl , lcTmpDist  , lcTaxCode  , lcOldDesc  ,;
                   lcOldVal   , lcSetExact , lcOldCurr

STORE SPACE(60) TO lcAccDesc  , lcApDGLDesc

STORE 'DISABLE' TO lcAcctStat , lcAmntStat , lcTaxStat  , lcNewStat  ,;
                   lcRemStat  , lcRemitStat, lcTypesStat, lcVendStat ,;
                   lcDataStat , lcFactStat , lcDButStat , lcData13Stat,;
                   lcAprvStat , lcAddStat  , lcEditStat

STORE 0         TO lnTerDiscR , lnTerDiscD , lnTerDueD  , lnDistLines,;
                   lnApdAmnt  , lnDistAmnt , lnTotAppPay, lnOldVal   ,;
                   lnTypeLen  , lnDurLen   , lnPMLen    , lnRemitLen

STORE 1         TO lnOldType  , lnOldDur   , lnOldPayM  , lnOldRemit ,;
                   puType     , puDuration , puPayMeth  , puRemitTo

STORE {}        TO ldOldVal   , ldFisBgDt  , ldFisEnDt

STORE .F.       TO llBrowse   , llFirstAdd

*** Prompts and paads variables
lcTBrowse    = '\<Browse'
lcTNew       = 'New'
lcTHiNew     = '\<New'
lcTRem       = 'Remove'
lcTHiRem     = 'Remo\<ve'
lcNew        = lcTNew
lcRem        = lcTRem

lcDistAcct   = lcEmptyAcc   && default distribution account
lcApdGLAct   = lcEmptyAcc   && distribution account                    
lcTypesCol   = lcDisCont    && color of type popup (DOS)
lcPopCol     = lcDisCont    && color of other popups

lcEmptyCode  = ''

*E301077,80 IHB 03/03/1999 opend needed files only [start]
*-- llFISHD  are used to identify whether 
*-- the files are used or not
STORE .F. TO  llFISHD
*E301077,80 IHB [end]

IF !gfSetup()
  RETURN
ENDIF

lcOnEsc      = ON("KEY","ESC")
                            && default on Esc procrdure on entry
lcOnCtrlEntr = ON("KEY","CTRL+ENTER")
                            && default on Ctrl+Enter procrdure on entry

llTrapTab    = .F.

*** Array laFields is formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name

DECLARE laFields[3,3], laInvADt[1]
laFields[1,1] = 'laData[4]'         &&'lcVendCode'
laFields[1,2] = 'cVendCode'
laFields[1,3] = 'VENCODE'
laFields[2,1] = 'lcCompany'         && 'lcVenComp'
laFields[2,2] = 'cVenComp'
laFields[2,3] = 'VENCOMP'
laFields[3,1] = 'lcPhone'           &&'lcPhoneNo'
laFields[3,2] = 'cPhoneNo'
laFields[3,3] = 'VENPHONE'
 
*** Text variables :
STORE ' ' TO lcTFirstInv, lcTNextInv, lcTLastInv, lcTFirstNum,;
      lcTNextNum, lcTZero, lcTInvAmnt, lcTChldTitl, lcTA_EDist, lcTV_Dist,;
      lcTInvoice 

IF !WEXIST(gcBaseWind)
  *E301077,80 IHB 03/03/1999 opend FISHD file
  *E300789,4 IHB Remove company ID [start]
  PRIVATE llOpCmp,lcDataDir,llFISHD
  llOpCmp = gfSysOpen(gcSysHome+'SYCCOMP','Ccomp_id','SH')
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  llFISHD   = gfSysOpen(lcDataDir+'FISHD'    ,'COMPFYEAR','SH')
  IF llOpCmp
    =gfSysClose('SYCCOMP')
  ENDIF
  *E300789,4 IHB Remove company ID [end]
  *E301077,80 end
  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK  
  laData[1]   = 'R'            && recurring codes only
  lcCodeFilt  = ''

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcEmptyCode = gfCodDes(' ')
  lcEmptyCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]
  
  lcTermCode  = lcEmptyCode
  lcDivision  = lcEmptyCode
  lcTaxDes    = lcEmptyCode

  laTermCode[1,1] = 'NTERDUED'
  laTermCode[1,2] = 'lnTerDueD'  
  laTermCode[2,1] = 'NTERDISCD'
  laTermCode[2,2] = 'lnTerDiscD'  
  laTermCode[3,1] = 'NTERDISCR'
  laTermCode[3,2] = 'lnTerDiscR'  

  laTaxCode[1,1] = 'CGLINPACCT'
  laTaxCode[1,2] = 'lcApDGlAct'

  lcRcrChld1  = gfTempName()   && Random name for the Dist. child screen no 1.
  lcRcrChld2  = gfTempName()   && Random name for the Dist. child screen no 2.
  lcRcrChld3  = gfTempName()   && Random name for the Dist. child screen no 3.
  lcRcrChld4  = gfTempName()   && Random name for the Dist. child screen no 5.
  lcRcrChld5  = gfTempName()   && Random name for the Dist. child screen no 6.
 
  lcTmpDist   = gfTempName()   && distribution lines temporary file

  SELECT APINVADT
  =AFIELDS(laInvADt)

  lnInvADt    = ALEN(laInvADt,1)
  DIMENSION laInvADt[lnInvADt+2,4]

  laInvADt[lnInvADt+1,1] = 'CSTATUS'
  laInvADt[lnInvADt+1,2] = 'C'
  laInvADt[lnInvADt+1,3] = 1
  laInvADt[lnInvADt+1,4] = 0
  
  laInvADt[lnInvADt+2,1] = 'NRECNO'
  laInvADt[lnInvADt+2,2] = 'N'
  laInvADt[lnInvADt+2,3] = 10
  laInvADt[lnInvADt+2,4] = 0

  *** Creat temp ditribution file from the array.
  CREATE TABLE (gcWorkDir+lcTmpDist) FROM ARRAY laInvADt

  *** Prepare Types array from SYDFIELD and get its maximum width
  lnTypeLen  = gfGetVld('cAutMBase',@laType)
  lcType     = laType[1,1]
 
  *** Prepare Duration array from SYDFIELD and get its maximum width
  lnDurLen   = gfGetVld('cAutFUnit',@laDuration)
  lcDuration = laDuration[1,1]   

  *** Prepare Remit to array from SYDFIELD and get its maximum width
  lnRemitLen   = gfGetVld('cInvRemit',@laRemitTo)
  lcRemitTo    = laRemitTo[1,1]   
  
  *** Prepare Payment method array from SYDFIELD and get its maximum width
  *** Remove 'Credit cards' (C) option from the array
  lnPMLen   = gfGetVld('cVenPMeth',@laPayMeth)
  FOR lnCount = 1 TO ALEN(laPayMeth)
    IF 'C' $ laPayMeth[lnCount,2] 
      =ADEL(laPayMeth, lnCount) = 1
      DIMENSION laPayMeth[ALEN(laPayMeth,1) - 1,2] 
      EXIT
    ENDIF  
  ENDFOR
   
  *** Prepare an array to hold bank objects to be used for
  *** global bank and checking accounts validations as follows :
  *** One row for every object, such that 
  *** row no. 1 holds bank object names,
  *** row no. 2 holds checking account object names
  *** row no. 3 holds the corresponding G/L account object names,
  *** Columns are ordered as follows :
  *** Column no. 1 : invisible button name for corresponding object
  *** Column no. 2 : object name (e.g. bank object name)
  *** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'laData[21]'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'laData[22]'     && Checking account 
  laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button  
  laBankObjs[3,2] = 'laData[23]'     && G/L account
  laBankObjs[3,3] = 'lcAccDesc'      && G/L account description  
  
  *** Get the current fiscal year's start and end date
  *** to be used as defaults for the invoice dates from
  *** the fiscal calendar of the parent company.
*E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *SELECT SYCFISHD
  SELECT FISHD
*E300692,1 end

  *E300789,4 IHB Remove company ID [start]
  *IF SEEK(gcPrnt_Cmp)
  IF !EOF()
  *E300789,4 IHB Remove company ID [end]
    
    *E300789,4 IHB Remove company ID [start]
    *LOCATE REST WHILE cComp_ID = gcPrnt_Cmp FOR  cFisYstat = 'C'
    LOCATE REST FOR  cFisYstat = 'C'
    *E300789,4 IHB Remove company ID [end]
    
    IF FOUND()
      ldFisBgDt = dFisBgDat         && begin date of current fiscal year
      ldFisEnDt = dFisEnDat         && end   date of current fiscal year
    ELSE
      STORE {} TO ldFisBgDt, ldFisEnDt
    ENDIF
  ELSE
    STORE {} TO ldFisBgDt, ldFisEnDt
  ENDIF

  *E301077,80 IHB 03/03/1999 opend needed files only [start]
  IF USED('FISHD') .AND. llFISHD
    =gfSysClose('FISHD')
  ENDIF
  *E301077,80 IHB 03/03/1999 opend needed files only [end]

ELSE
  *** Get rbRemit value
  puType     = lnOldType   
  puDuration = lnOldDur
  puPayMeth  = lnOldPayM
  puRemitTo  = lnOldRemit
   
  IF !EMPTY(laData[4])
    =SEEK(laData[4],'APVENDOR')
  ENDIF 
ENDIF

IF _WINDOWS

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puTermCode prompt field SYCCODES.cdiscrep scroll;
  *FROM 16.7,2.37 TO 21.00,34.92;
  *MESSAGE gfObj_msg()

  DEFINE POPUP puTermCode prompt field CODES.cdiscrep scroll;
  FROM 16.7,2.37 TO 21.00,34.92;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puTermCode DO lfvTermCode

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puDivision prompt field SYCCODES.cdiscrep scroll;
  *FROM 19.95,2.37 TO 24.25,34.92;
  *MESSAGE gfObj_msg()

  DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 19.95,2.37 TO 24.25,34.92;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puTaxDes prompt field SYCCODES.cdiscrep scroll;
  *FROM 2.4,8.20 TO 6.6,40.75;
  *MESSAGE gfObj_msg()

  DEFINE POPUP puTaxDes prompt field CODES.cdiscrep scroll;
  FROM 2.4,8.20 TO 6.6,40.75;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puTaxDes DO lfvTaxCode
ENDIF

*** The browse child window is not included in this array
*** because it does not include any gets.

laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "IBRCRCODE"
laWndObj [1,3] = "PBAPPROVE"

laWndObj [2,1] = "GWCCONTRL1"
laWndObj [2,2] = "PBTOP"
laWndObj [2,3] = "PBCLS"

laWndObj [3,1] = "CWRAPRCRDS"
laWndObj [3,2] = "laData[32]"
laWndObj [3,3] = "PBCLOSE"

laWndObj [4,1] = lcRcrChld1
laWndObj [4,2] = "laData[32]"
laWndObj [4,3] = "PBCLOSE"

laWndObj [5,1] = lcRcrChld3
laWndObj [5,2] = "laData[32]"
laWndObj [5,3] = "PBCLOSE"

laWndObj [6,1] = lcRcrChld4
laWndObj [6,2] = "laData[32]"
laWndObj [6,3] = "PBCLOSE"

laWndObj [7,1] = lcRcrChld5
laWndObj [7,2] = "laData[32]"
laWndObj [7,3] = "PBCLOSE"

SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APINVADT
SET ORDER TO TAG DTYPCOD
lcInvADtEx      = SYS(14,VAL(SYS(21)))   && current index expression

*** Base file for the current screen
SELECT APINVAHD
SET ORDER TO TAG HTYPCOD                 && should already be set
lcInvAHdEx     = SYS(14,VAL(SYS(21))) 

SET FILTER TO
SET FILTER TO cAutMType = "R"          && filter on recurring 
*** Set relation to details file on the current key.
SET RELATION TO cAutMType + cAutMCode INTO APINVADT ADDITIVE
      
PUSH KEY

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300789,4 IHB Remove company ID [start]
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+lcCodeFilt) OR cFld_Name ='N/A'
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR cFld_Name ='N/A'
*E300789,4 IHB Remove company ID [end]

*E300683,1 Call *.SPR from screens directory
*DO APRcrIn.SPR

DO (gcScrDir + gcWinAppl + '\APRcrIn.SPR')
*E300683,1 end


*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 

RELEASE PAD _BROWSE OF _MSYSMENU

POP KEY

SELECT APINVAHD
SET RELATION OFF INTO APINVADT
SET FILTER TO 

RELEASE WINDOW (lcBrowsTtl)

IF glQuitting
  RELEASE WINDOW CWRAPRCRDS
  IF USED(lcTmpDist)
    USE IN (lcTmpDist)
  ENDIF  
  ERASE (gcWorkdir+lcTmpDist+".DBF")  && Erase the Temp file.
ENDIF

IF _WINDOWS
  RELEASE POPUPS puTermCode, puDivision, puTaxDes
ENDIF 
glFromBrow  = .F.

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
laData[1] = 'R'        && recurring invoices only.
*SET EXACT &lcSetExact
DO CASE
  CASE laScrMode[1]
  
    IF WVISIBLE('CWRAPRCRDS')
      =lfvClsChld()
    ENDIF
    
    SELECT APINVAHD
    
    STORE SPACE(40) TO lcRmtcomp, lcRmtAddr1, lcRmtAddr2, lcRmtAddr3
    STORE 0         TO lnTerDiscR, lnTerDiscD, lnTerDueD, lnDistLines,;
                       lnApdAmnt, lnDistAmnt, lnTotAppPay
    lcCompany    = SPACE(30)          && Company
    lcPhone      = SPACE(18)          && phone
    STORE  lcEmptyAcc TO lcDistAcct, lcApdGLAct,laData[23], laData[32]
    lcType       = laType[1,1]
    lcDuration   = laDuration[1,1]   

    lcCodeFilt = ''
    lcTaxCode  = ''
    lcTermCode = lcEmptyCode
    lcDivision = lcEmptyCode
    lcTaxDes   = lcEmptyCode

    lcPayMeth  = laPayMeth[1,1]
    lcRemitTo  = laRemitTo[1,1]

    STORE 1         TO lnOldType,lnOldDur,lnOldPayM,lnOldRemit,puType,;
                       puDuration,puPayMeth,puRemitTo
 
    STORE 'DISABLE' TO lcAcctStat,lcRemStat,lcTypesStat,lcTaxStat,;
                       lcData13Stat,lcVendStat,lcDataStat,lcDButStat,;
                       lcAmntStat,lcAprvStat,lcAddStat,lcEditStat,lcFactStat
 
  CASE laScrMode[2] .OR. laScrMode[3]

    *** Get the current vendor information
    IF SEEK(laData[4],'APVENDOR')
      lcCompany    = APVENDOR.cVenComp
      lcPhone      = APVENDOR.cPhoneNo
    ENDIF

    *** If 'Edit' mode,
    IF laScrMode[3] 
      *** Collect the distribution lines of the current recurring payable
      *** invoice into the temporary file lcTmpDist 
      SELECT *,'S' AS 'cStatus',;
             RECNO() AS 'NRECNO';
        FROM (gcDataDir+'APINVADT');
        WHERE &lcInvADtEx = laData[1] + laData[2];
        INTO DBF (gcWorkDir+lcTmpDist)

      SELECT(lcTmpDist)
      lnDistLines = RECCOUNT()        
      
      *** Get the total of the distribution lines = lnDistAmnt
      SUM nAPDAmnt TO lnDistAmnt
      GO TOP
      IF lnDistLines > 0
        lcRemStat    = 'ENABLE'
        lcTypesStat  = 'DISABLE'
      ELSE
        lcRemStat    = 'DISABLE'
        lcTypesStat  = 'ENABLE'
      ENDIF  
      lcData13Stat   = IIF(laData[3]='P','DISABLE','ENABLE')
      lcDataStat     = 'ENABLE'
      lcEditStat     = 'ENABLE'
    ELSE
           
      SELECT(lcTmpDist)
      ZAP
      
      SELECT APINVADT
      SUM nAPDAmnt TO lnDistAmnt FOR &lcInvADtEx = laData[1]+ laData[2]
           
      *** Refresh the existing relation to position the record
      *** pointer in the details file on the first line of details
      SELECT APINVAHD
      IF !EOF('APINVAHD')   
        GO RECNO()
      ELSE
        GO TOP
      ENDIF
      STORE 'DISABLE' TO lcRemStat, lcTypesStat,;
            lcData13Stat,lcDataStat, lcEditStat
    ENDIF  
    
    lcDButStat = 'ENABLE'

    RELEASE WINDOW (lcBrowsTtl)
    =lfBrowse()  

    *** Type
    lnElemNum   = lfGetArrElem(laData[3], 'laType')
    lcType      = IIF(lnElemNum > 0, laType[lnElemNum, 1]," ")
    puType      = lnElemNum

    *** Duration
    lnElemNum   = lfGetArrElem(laData[6], 'laDuration')
    lcDuration  = IIF(lnElemNum > 0, laDuration[lnElemNum, 1]," ")
    puDuration  = lnElemNum

    *** Payment method
    lnElemNum   = lfGetArrElem(laData[20], 'laPayMeth')
    lcPayMeth   = IIF(lnElemNum > 0, laPayMeth[lnElemNum, 1]," ")
    puPayMeth   = lnElemNum

    *** Remit to objects 
    lnElemNum   = lfGetArrElem(laData[18], 'laRemitTo')
    lcRemitTo   = IIF(lnElemNum > 0, laRemitTo[lnElemNum, 1]," ")
    puRemitTo   = lnElemNum
    =lfvRemit(.T.)
    
    lcCodeFilt = ''

    *E300643,1 Change this lines for the changes we have made 
    *          to (gfCodDes) and (gfRltFld) [Begin]
    *lcTermCode = gfCodDes(laData[14])
    *=gfRltFld(laData[14],@laTermCode)
    *lcDivision = gfCodDes(laData[17])
    lcTermCode = gfCodDes(laData[14] , 'CTERMCODE')
    =gfRltFld(laData[14] , @laTermCode , 'CTERMCODE')
    lcDivision = gfCodDes(laData[17] , 'CDIVISION')
    *E300643,1 Change this lines [End]    
    
    *** Total approved amount = approved to pay + approved discount
    ***                         + approved adjustment. 
    lnTotAppPay = laData[24] + laData[25] + laData[26]
    lcAprvStat  = IIF(laScrMode[3] .OR. lnTotAppPay > 0,;
                      'ENABLE', 'DISABLE')
    *** AP account 
    IF EMPTY(laData[32])
      laData[32] = lcEmptyAcc
    ENDIF  
    lcAccDesc   = IIF(llApGlLink,;
                      ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,laData[32],;
                      lcLinkChar.cAcctCode,"ACCTCODE")),' ')
      
    IF WVISIBLE('CWRAPRCRDS')
      IF !EOF('APINVAHD')   
        GO RECNO()
      ELSE
        GO TOP
      ENDIF
      =lfDispObjct() 
    ENDIF  
  
  CASE laScrMode[4]

    SELECT(lcTmpDist)
    ZAP

    *** Initialize variables with defaults
    laData[3]  = laType[1,2]
    laData[5]  = 1         && duration
    laData[6]  = laDuration[1,2]
    laData[7]  = ldFisBgDt    && start invoice date 
    laData[8]  = ldFisBgDt    && next  invoice date 
    laData[9]  = ldFisEnDt    && last invoice date
    laData[11] = 1            && first invoice number
    laData[12] = 1            && next invoice number

    STORE 'ENABLE'  TO lcTypesStat,lcVendStat,lcAddStat

    STORE 'DISABLE' TO lcData13Stat,lcAprvStat,lcDataStat,;
                       lcDButStat,lcAmntStat

    llFirstAdd   = .T.
ENDCASE

*** Disable types popup if distribution lines > 0
lcTypesCol    = IIF(lnDistLines = 0 .AND. laScrMode[3] .OR. laScrMode[4],;
                    lcEnbCont, lcDisCont)
lcPopCol      = IIF(laScrMode[1] .OR. laScrMode[2], lcDisCont,;
                    lcEnbCont)
lcRemitStat  = IIF(laScrMode[3] .AND. laData[18] = 'O','ENABLE','DISABLE')       
lcNewStat    = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')       
SHOW GET laData[4]   &lcVendStat
SHOW GET laData[7]   &lcAddStat
SHOW GET laData[8]   &lcEditStat
SHOW GET laData[11]  &lcAddStat
SHOW GET laData[12]  &lcEditStat
SHOW GET lcCompany   &lcVendStat
SHOW GET lcPhone     &lcVendStat
SHOW GET ibVendor    &lcVendStat
SHOW GET ibCompany   &lcVendStat
SHOW GET ibPhone     &lcVendStat
SHOW GET ibRemitTo   &lcDataStat
SHOW GET puRemitTo   &lcDataStat
SHOW GET laData[33]  &lcFactStat
SHOW GET ibFactor    &lcFactStat
SHOW GET lcRmtComp   &lcRemitStat
SHOW GET lcRmtAddr1  &lcRemitStat
SHOW GET lcRmtAddr2  &lcRemitStat
SHOW GET lcRmtAddr3  &lcRemitStat
SHOW GET ibType      &lcTypesStat
SHOW GET puType      &lcTypesStat
SHOW GET pbRem       &lcRemStat
SHOW GET laData[13]  &lcData13Stat
SHOW GET laData[15]  &lcData13Stat
SHOW GET laData[19]  &lcDataStat
SHOW GET ibPayMeth   &lcDataStat
SHOW GET puPayMeth   &lcDataStat
SHOW GET ibDivision  &lcDataStat
SHOW GET ibTermCode  &lcDataStat
SHOW GET laData[16]  &lcDataStat
SHOW GET pbDisLine   &lcDButStat
SHOW GET pbApprove   &lcAprvStat
SHOW GET lcApDGlAct  &lcAcctStat
SHOW GET ibApGlAcc   &lcAcctStat
SHOW GET lcApDGLDesc &lcAcctStat
SHOW GET lnApdAmnt   &lcAmntStat
SHOW GET ibTaxCode   &lcTaxStat
SHOW GET lcTaxDes    &lcTaxStat

*E300296,4 Show currency field
SHOW GET laData[34]  &lcDataStat
SHOW GET ibCurrncy   &lcDataStat
*E300296,4 end.

SELECT APINVAHD

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2
laData[2]     = ALLTRIM(laData[2])
laData[2]     = IIF(ISDIGIT(LEFT(laData[2],1)),;
                         PADL(laData[2], FSIZE('cAutMCode','APINVAHD'),'0'),;
                         PADR(laData[2],FSIZE('cAutMCode','APINVAHD')))
SHOW GET laData[2]
IF llBrowse
  llBrowse = .F.
  =gfBrows()
ELSE
  IF !EMPTY(laData[2]) .AND. LASTKEY()= 13 
    IF '?' $ laData[2]
      laData[1]    = '?'
      laData[2]    = SPACE(FSIZE('cAutMCode','APINVAHD'))
    ENDIF  
    lcSetExact = SET('EXACT')
    =gfSeekRec()
    *SET EXACT &lcSetExact
  ENDIF
ENDIF
laData[1]  =  'R'

*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
FUNCTION lfwOldVals
PARAMETERS lcOldObjNm

&lcOldObjNm = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!        Function : lfvVendor
*!
*!**************************************************************************
FUNCTION lfvVendor

IF lfSekVnd('APVENDOR',lcOldVal, llBrowse)

  IF WVISIBLE('CWRAPRCRDS')
    =lfvClsChld()
  ENDIF
  
  *** Upon selection of a vendor, do the following :
  *** 1. a default division
  laData[17] = APVENDOR.cDivision

  *E300643,1 Change this lines for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcDivision = gfCodDes(laData[17])
  lcDivision = gfCodDes(laData[17] , 'CDIVISION')
  *E300643,1 Change this lines [End]          
  
  *** 2. a default term code and its related fields
  laData[14] = APVENDOR.cTermCode

  *E300643,1 Change this lines for the changes we have made 
  *          to (gfCodDes) and (gfRltFld) [Begin]
  *lcTermCode = gfCodDes(laData[14])
  *=gfRltFld(laData[14],@laTermCode)
  lcTermCode = gfCodDes(laData[14] , 'CTERMCODE')
  =gfRltFld(laData[14],@laTermCode , 'CTERMCODE')
  *E300643,1 Change this lines [End]
  
  *** 3. a default payment method
  laData[20]  = APVENDOR.cVenPMeth

  *** If the vendor's payment method is credit cards, 
  *** use 'Printed checks' instead
  IF laData[20] = 'C'
    laData[20]  = 'P'
  ENDIF  
      
  *** Payment method
  lnElemNum   = lfGetArrElem(laData[20], 'laPayMeth')
  lcPayMeth   = IIF(lnElemNum > 0, laPayMeth[lnElemNum, 1]," ")
  puPayMeth   = lnElemNum
  lnOldPayM   = puPayMeth

  *** 4. a default payment priority
  laData[19]  = APVENDOR.cVenPrior             
      
  *** 5. A default AP account
  IF !EMPTY(APVENDOR.CAPACCT)
    laData[32]   = APVENDOR.CAPACCT
  ELSE   
    laData[32] = IIF(!EMPTY(laData[17]) .AND.;
                     SEEK(laData[17], 'APDIV') .AND.;
                     !EMPTY(APDIV.CAPACCT),;
                     APDIV.CAPACCT, APSETUP.CAPACCT)
  ENDIF  
  *** AP account description
  lcAccDesc   = IIF(llApGlLink,;
                    ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,laData[32],;
                    lcLinkChar.cAcctCode,"ACCTCODE")),' ')
      
  *** 6. A default distribution account
  IF !EMPTY(APVENDOR.CEXPACCT)
    lcDistAcct = APVENDOR.CEXPACCT
  ELSE
    lcDistAcct = IIF(SEEK(laData[17], 'APDIV') .AND. ;
                     !EMPTY(APDIV.CEXPACCT), ;
                     APDIV.CEXPACCT, APSETUP.CEXPACCT)
  ENDIF   
  *** Distribution account description
  lcAPDGLDesc = IIF(llApGlLink,;
                    ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,lcDistAcct,;
                    lcLinkChar.cAcctCode,"ACCTCODE")),' ')

  *** 7. Distribtion lines window selection
  *** Tax codes 
  IF APVENDOR.cTaxType = 'L'
    lcTaxDes   = lcEmptyCode
    lcTaxCode  = SPACE(6)
  ELSE
    lcTaxDes   = " "
  ENDIF  
  
  *** 8. Remit to fields
  laData[18]  = laRemitTo[1,2]      && default remit to vendor
  lcRemitTo   = laRemitTo[1,1]
  puRemitTo   = 1
  =lfvRemit(.T.)
  
  *** 9. Currency code
  * E300296,1 M.H 10/10/95 Add the currency to the AP module.
  laData[34]  = APVENDOR.CCURRCODE  && Added by Mohamed Hassan
      
  lcData13Stat = IIF(laData[3]='P','DISABLE','ENABLE')
  lcDataStat = "ENABLE"
  lcDButStat = "ENABLE"
  lcAprvStat = "ENABLE"
  lcFactStat = 'DISABLE'
  SHOW GET laData[13] &lcData13Stat      
  SHOW GET laData[14] ENABLE
  SHOW GET laData[15] &lcData13Stat
  SHOW GET laData[16] ENABLE
  SHOW GET laData[19] ENABLE
  SHOW GET ibDivision ENABLE
  SHOW GET ibTermCode ENABLE
  SHOW GET ibPayMeth  ENABLE
  SHOW GET puPayMeth  ENABLE
  SHOW GET ibRemitTo  ENABLE
  SHOW GET puRemitTo  ENABLE
  SHOW GET pbDisLine  ENABLE
  SHOW GET pbApprove  ENABLE
  SHOW GET ibTaxCode  &lcTaxStat
  SHOW GET lcTaxDes   &lcTaxStat
  SHOW GET laData[32]
  SHOW GET lcAccDesc
  SHOW GET lcTermCode
  SHOW GET lcDivision
  
  *E300296,4 Enable currency fields
  SHOW GET ibCurrncy  ENABLE
  *SHOW GET laData[34]    && *E300296,1 Added by Mohamed Hassan
  SHOW GET laData[34] ENABLE
  *E300296,4 end.

  RELEASE WINDOW (lcBrowsTtl)
  =lfBrowse()  
  _CUROBJ = OBJNUM(laData[5])
ENDIF

llBrowse     = .F.

*!**************************************************************************
*!
*!      Function : lfvType
*!
*!**************************************************************************
*
FUNCTION lfvType

PRIVATE lcOldType

lcOldType = laData[3]
laData[3] = lfvPopups('laType', @lcType, 0,58,lnTypeLen) 
*** If 'P'ercent, invoice amount = 100, and disabled
***               discount       = 0,    and disabled  
IF laData[3] <> lcOldType
  IF laData[3] = 'P'
    laData[13] = 100
    laData[15] = 0
    lcData13Stat = "DISABLE"
  ELSE
    laData[13] = 0
    laData[15] = 0
    lcData13Stat = "ENABLE"
  ENDIF
  SHOW GET laData[13] &lcData13Stat
  SHOW GET laData[15] &lcData13Stat
  =lfRefresh(gcBaseWind+','+lcRcrChld1)
ENDIF  
lnOldType = puType

*!**************************************************************************
*!
*!        Function : lfwPayMeth
*!
*!**************************************************************************
FUNCTION lfwPayMeth

lcOldVal = laData[20]

*!**************************************************************************
*!
*!      Function: lfvPayMeth
*!
*!**************************************************************************
*
FUNCTION lfvPayMeth

*** RENEE
*laData[20] = lfvPopups('laPayMeth', @lcPayMeth, 12,39,lnPMLen)
laData[20] = lfvPopups('laPayMeth', @lcPayMeth, 12,31,24)
IF laData[20] <> lcOldVal .AND. INLIST('H', laData[20], lcOldVal)
  *** If payment method is toggled between 'Cash payments (H)' and
  *** the other available payment methods, clear the approval fields
  *** Store corresponding laData elements
  laData[21]  = SPACE(8)
  laData[22]  = SPACE(12)
  laData[23]  = lcEmptyAcc
  STORE 0 TO laData[24], laData[25], laData[26], laData[27]
ENDIF
IF _DOS
  =lfRefresh(gcBaseWind)
ENDIF
lnOldPayM = puPayMeth

*!**************************************************************************
*!
*!        Function : lfvData_5
*!
*!**************************************************************************
*  Valid function for laData[5] 
*
FUNCTION lfvData_5
*** If the field is empty, or,
*** if the entered value is not greater than zero,
***     Message :  "   ð should be greater than ð.   "
***                            ®   OK   ¯
*** restore the old value of the field
IF laData[5] <> lnOldVal .AND.;
   ((laData[5] < 1 .AND. gfModalGen("TRM04072B00000","DIALOG",lcTDur+'|'+lcTZero) > 0))
  laData[5] = lnOldVal
  _CUROBJ = _CUROBJ
ENDIF

*!**************************************************************************
*!
*!        Function : lfvDuration
*!
*!**************************************************************************
*  Valid function for Duration popup
*
FUNCTION lfvDuration
laData[6] = lfvPopups('laDuration', @lcDuration,;
                       2,63,12)
IF _DOS 
  =lfRefresh(gcBaseWind)
ENDIF                          
lnOldDur = puDuration

*!**************************************************************************
*!
*!        Function : lfvData_7
*!
*!**************************************************************************
*  Valid function for laData[7] (first invoice date)  
*
FUNCTION lfvData_7

*** Return the old value of the field in the following cases :
*** a. if it is emptied, or
*** b. if it is greater than the last invoice date if the latter is
***    not empty, in which case the following message is presented, or
*** Message :   The ð date cannot be less than the ð date.     "
***                              ®   OK   ¯
*** c. If the date is invalid
IF (laData[7] <> ldOldVal .OR. EMPTY(laData[7])) .AND. ;
  ( (!EMPTY(laData[9]) .AND. laData[9] < laData[7] ;
  .AND. gfModalGen("TRM04028B00000","DIALOG",lcTFirstInv+'|'+lcTLastInv) > 0) .OR.;
  !lfvDtMsg(gcPrnt_Cmp))

  laData[7] = ldOldVal
  _CUROBJ = _CUROBJ
ELSE  
  laData[8]  = laData[7]
  SHOW GET laData[8]
  =lfRefresh(gcBaseWind)
ENDIF

*!**************************************************************************
*!
*!        Function : lfvData_8
*!
*!**************************************************************************
*  Valid function for laData[8], laData[9] 
*
FUNCTION lfvData_8
PARAMETERS lcTDate

PRIVATE lcCurObj

*** Return the old value of the field in the following cases :
*** a. if it is emptied, or
*** b. if it is less than the first invoice date if the latter is
***    not empty, in which case the following message is presented, or
*** Message :   The ð date cannot be less than the ð date.     "
***                              ®   OK   ¯
*** c. If the date is invalid
lcCurObj      = SYS(18)

IF (EVALUATE(lcCurObj) <> ldOldVal .OR. EMPTY(EVALUATE(lcCurObj))) .AND. ;
   ( (!EMPTY(laData[7]) .AND. EVALUATE(lcCurObj) < laData[7] .AND.;
   gfModalGen("TRM04028B00000","DIALOG",lcTDate+'|'+lcTFirstInv) > 0) .OR.;
  (laData[8] > laData[9] .AND.;
   gfModalGen("TRM04028B00000","DIALOG",lcTLastInv+'|'+lcTNextInv) > 0) .OR.;
  !lfvDtMsg(gcPrnt_Cmp))

  &lcCurObj = ldOldVal
  _CUROBJ   = _CUROBJ
ENDIF

*!**************************************************************************
*!
*!        Function : lfvData_11
*!
*!**************************************************************************
* laData[11] is enabled only in the ADD mode  
*
FUNCTION lfvData_11

*** Return the old value of the field in the following cases :
*** a. if it is emptied, or
*** b. if the entered value is not greater than zero, or,
IF laData[11] <> lnOldVal .AND.;
  ( (laData[11] < 1 .AND.;
      gfModalGen("TRM04072B00000","DIALOG",;
                  lcTFirstNum+'|'+lcTZero) > 0) ) 
                  
  laData[11] = lnOldVal
  _CUROBJ = _CUROBJ
ELSE  
  laData[12]  = laData[11]
  SHOW GET laData[12]
ENDIF

*!**************************************************************************
*!
*!        Function : lfvData_12
*!
*!**************************************************************************
*  laData[12]  is enabled only in the EDIT mode
*
FUNCTION lfvData_12

*** If the field is empty, or,
*** if the entered value is not greater than zero, or, 
*** if the next invoice number is not greater than the first invoice number
***     Message :  "   ð should be greater than ð.   "
***                            ®   OK   ¯
*** restore the old value of the field
IF laData[12] <> lnOldVal .AND.;
   ((laData[12] < 1 .AND. ;
     gfModalGen("TRM04072B00000","DIALOG",lcTNextNum+'|'+lcTZero) > 0) .OR.;
     laData[11] > laData[12] .AND.;
     gfModalGen("TRM04072B00000","DIALOG",;
     lcTNextNum+'|'+LOWER(lcTFirstNum)) > 0))   
  laData[12] = lnOldVal
  _CUROBJ = _CUROBJ
ENDIF

*!**************************************************************************
*!
*!        Function : lfvData_13
*!
*!**************************************************************************
*  Valid function for laData[13] (invoice amount)
*
FUNCTION lfvData_13

*** if the entered value is not greater than zero,
*** Message :  "   ð should be greater than ð.   "
***                       ®   OK   ¯
*** Or the invoice amount is edited and is now less
*** that the total approved amount,
*** Message : " The invoice amount can not be less "
***           " than the total pay amount + the    "
***           " approved discount amount.          "
***           "           ®   OK   ¯               "
*** restore the old value of the field
IF laData[13] <> lnOldVal .AND.;
   ((!(laData[13] > 0)  .AND. ;
       gfModalGen("TRM04072B00000","DIALOG",lcTInvAmnt+'|'+lcTZero)> 0); 
   .OR.;
   (laData[13] < lnTotAppPay .AND. ;
    gfModalGen("TRM04009B00000","DIALOG") > 0)) 
   
  laData[13] = lnOldVal
  _CUROBJ    = _CUROBJ
ELSE
  *** if a term code is selected, refresh the discount amount
  IF !EMPTY(laData[14])   
    laData[15] = laData[13] * lnTerDiscR/100
    SHOW GET laData[15]
  ENDIF
  =lfRefresh(gcBaseWind+','+lcRcrChld1)
ENDIF

*!**************************************************************************
*!
*!        Function : lfvData_15
*!
*!**************************************************************************
*  Valid function for laData[15] (invoice amount)
*
FUNCTION lfvData_15

*** if the entered value is negative
***     Message :  "   Negative values are not allowed.   "
***                            ®  OK   ¯
*** or if it is greater than the invoice amount,
***     Message : " The offered discount can not be    "
***               " greater than the invoice amount.   "
***                            ®  OK   ¯
*** restore the old value of the field
IF laData[15] <> lnOldVal .AND.;
   ((laData[15] < 0 .AND. ;
     gfModalGen("TRM04087B00000","DIALOG")>0) .OR.;
    (laData[15] > laData[13] .AND. gfModalGen("TRM04011B00000","DIALOG")>0))   
  laData[15] = lnOldVal
  _CUROBJ    = _CUROBJ
ENDIF

*!**************************************************************************
*!
*!      Function: lfwDivision
*!
*!**************************************************************************
*
FUNCTION lfwDivision
PRIVATE  lcCurAlias

lcCurAlias = ALIAS()

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = 'CDIVISION'
LOCATE
SELECT (lcCurAlias)

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
*
FUNCTION lfvDivision

DO CASE
  CASE _DOS
    laData[17] =gfActPop(10,2,16,35,'SYCCODES','cCode_No','cDiscrep',@lcDivision)

  CASE _WINDOWS

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *lcDivision = SYCCODES.cdiscrep
    *laData[17] = SYCCODES.cCode_No
    lcDivision = CODES.cdiscrep
    laData[17] = CODES.cCode_No
    *E300643,1 Change this line [End]
    
    SHOW GET lcDivision
ENDCASE

=lfRefresh(gcBaseWind)   && refresh says filed on the screen

IF _WINDOWS
  =gfUpdate()
  DEACTIVATE POPUP puDivision
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvAprovPay
*!
*!**************************************************************************
*
FUNCTION lfvAprovPay
PRIVATE lc1099Stat

IF !laScrMode[2]

  lcApprvStat  = 'ENABLE'
  ** If bank code in the vendor file is not empty.
  lc1099Stat   = IIF(!EMPTY(APVENDOR.cVen1099T),'ENABLE','DISABLE')

  *** Check if the invoice amount is greater than zero
  IF !( laData[13] > 0 )
    *** If it is not, present the following message and do not
    *** branch to the distribution lines screen.
    *** Message : "   You have to enter the ð.  "
    ***                 ® OK  ¯
    =gfModalGen("TRM04066B00000","DIALOG",lcTInv)
    _CUROBJ = OBJNUM(laData[13])
  ELSE  
    lcApprv    = lcTApprove     && push button prompt (pbApprove)
    *** Store current contents of laData for retreival in case of 'Cancelling'
    lcBankCode = laData[21]   
    lcCheckAcc = laData[22]   
    lcGLAcount = laData[23]   
    lcApprPay  = laData[24]   
    lcApprDis  = laData[25]   
    lcApprAdj  = laData[26]   
    lc1099Amnt = laData[27]   
    
    *** Default the approved to pay amount with the
    *** invoice amount - discount amount if it is empty
    IF laData[24] = 0 
      laData[24] = laData[13] - laData[15]
      laData[25] = laData[15]      && approved discount = offered discount
    ENDIF
     
    DO CASE
      *** If payment method is not cash payments,
      CASE laData[20] <> 'H'
        *** If the bank code is empty, 
        IF EMPTY(laData[21])
          llBnkChkChng = .F.
          IF !EMPTY(APVENDOR.CBNKCODE)
            laData[21] = APVENDOR.CBNKCODE && Assign the bank code from vendor.
            laData[22] = APVENDOR.CCHKACCT && Assign the check code from vendor.
            llBnkChkChng = .T.
          ELSE
            IF !EMPTY(laData[17]) .AND. SEEK(laData[17], 'APDIV') .AND. !EMPTY(APDIV.CBNKCODE)
              laData[21] = APDIV.CBNKCODE  && Assign the bank code from division.
              laData[22] = APDIV.CCHKACCT  && Assign the bank code from division.
              llBnkChkChng = .T.
            ELSE
              laData[21] = APSETUP.CBNKCODE  && Assign the bank code from division.
              laData[22] = APSETUP.CCHKACCT  && Assign the bank code from division.
              llBnkChkChng = .T.
            ENDIF
          ENDIF
          IF llBnkChkChng

            IF SEEK(laData[21]+laData[22],'APCHECKS')
              *E300296,4 Check if the checking account has the same
              *E300296,4 currency as that of the invoice, if not,
              *E300296,4 get the first checking account using
              *E300296,4 the invoice currency, if any is found,
              *E300296,4 or clear al fields if none is found.
              *laData[23] = APCHECKS.cChkGlAcc     
              IF APCHECKS.cCurrCode = ladata[34]
                laData[23] = APCHECKS.cChkGlAcc     
              ELSE
                SELECT APCHECKS
                LOCATE REST WHILE cBnkCode = laData[21];
                       FOR cCurrCode = laData[34]
                IF FOUND()
                  laData[22] = APCHECKS.cChkAcct
                  laData[23] = APCHECKS.cChkGlAcc     
                ELSE
                  ladata[21] = SPACE(8)
                  ladata[22] = SPACE(12)
                  ladata[23] = lcEmptyAcc
                ENDIF
              ENDIF
              *E300296,4 end.           
            ENDIF
            
          ENDIF
        ENDIF
    
        *** Check account
        IF EMPTY(laData[23])
          laData[23] = lcEmptyAcc
        ENDIF  
        lcAccDesc   = IIF(llApGlLink,;
                    ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,laData[23],;
                    lcLinkChar.cAcctCode,"ACCTCODE")),' ')
                    
        *E300296,4 Add a state for Checking account, and G/L checking account
        *E300296,4 that is to be disable if the bank code is empty
        lcChkActStat = IIF(EMPTY(laData[21]), 'DISABLE', 'ENABLE')
        *E300296,4 end.
        
        *E300683,1 Call *.SPR from screens directory
        *DO APRCRA1.SPR
        DO (gcScrDir + gcWinAppl + '\APRCRA1.SPR')
        *E300683,1 end
      *** Case payment method is cash payment : 
      OTHERWISE   
        IF EMPTY(STRTRAN(STRTRAN(laData[23],'-'),'0'))     && If cash account is empty
          laData[23] = IIF(!EMPTY(APVENDOR.CCASHACCT),;
                           APVENDOR.CCASHACCT,;
                           IIF(!EMPTY(laData[17]) .AND. SEEK(laData[17], 'APDIV');   
                               .AND. !EMPTY(APDIV.CCASHACCT),;
                                APDIV.CCASHACCT,;
                                APSETUP.CCASHACCT))
        ENDIF
        lcAccDesc   = IIF(llApGlLink,;
                          ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,laData[23],;
                          lcLinkChar.cAcctCode,"ACCTCODE")),' ')
      
        *E300683,1 Call *.SPR from screens directory
        * DO APRCRA2.SPR
        DO (gcScrDir + gcWinAppl + '\APRCRA2.SPR')
        *E300683,1 end

    ENDCASE
  ENDIF  
ELSE
  lcAccDesc     = IIF(llApGlLink,;
                      ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,laData[23],;
                      lcLinkChar.cAcctCode,"ACCTCODE")),' ')
  lcApprv       = lcTOK     && push button prompt (pbApprove)

  *E300296,4 Add a state for Checking account, and G/L checking account
  *E300296,4 that is to be disable if View mode. (lcChkActStat)
  STORE 'DISABLE' TO lcApprvStat, lc1099Stat, lcChkActStat
  *E300296,4 end.

  IF EMPTY(laData[23])
    laData[23] = lcEmptyAcc
  ENDIF  
  IF laData[20] = 'H'
  
     *E300683,1 Call *.SPR from screens directory
     * DO APRCRA2.SPR
     DO (gcScrDir + gcWinAppl + '\APRCRA2.SPR')
     *E300683,1 end
  ELSE
    *E300683,1 Call *.SPR from screens directory
    * DO APRCRA1.SPR
    DO (gcScrDir + gcWinAppl + '\APRCRA1.SPR')
    *E300683,1 end
    
  ENDIF  
ENDIF

*** Total approved amount = approved to pay + approved discount
***                         + approved adjustment. 
lnTotAppPay = laData[24] + laData[25] + laData[26]
=lfRefresh(gcBaseWind) 

*!**************************************************************************
*!
*!      Function: lfwTermCode
*!
*!**************************************************************************
*
FUNCTION lfwTermCode
PRIVATE  lcCurAlias

*E300789,4 IHB  07/03/1999 Remove company ID [start]
*lcCurAlias = ALIAS()
lnCurAlias = SELECT(0)
*E300789,4 IHB  07/03/1999 Remove company ID [end]

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = 'CTERMCODE'
LOCATE

*E300789,4 IHB  07/03/1999 Remove company ID [start]
*SELECT (lcCurAlias)
SELECT (lnCurAlias)
*E300789,4 IHB  07/03/1999 Remove company ID [end]

*!**************************************************************************
*!
*!      Function: lfvTermCode
*!
*!**************************************************************************
*
FUNCTION lfvTermCode

DO CASE
  CASE _DOS
    laData[14]  =gfActPop(8,2,14,35,'SYCCODES','cCode_No','cDiscrep',@lcTermCode)

  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made
    *          to SYCCODES [Begin]
    *lcTermCode = SYCCODES.cdiscrep
    *laData[14] = SYCCODES.cCode_No
    lcTermCode = CODES.cdiscrep
    laData[14] = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET lcTermCode
ENDCASE

*E300643,1 Change this line for the changes we have made to (gfRltFld) [Begin]
*=gfRltFld(laData[14],@laTermCode)
=gfRltFld(laData[14] , @laTermCode , 'CTERMCODE')
*E300643,1 Change this line for the changes we have made to (gfRltFld) [End]

IF laData[3] = 'A'  && Update discount field if base is 'A'mounts
  laData[15]  =  laData[13] * lnTerDiscR / 100
  SHOW GET laData[15]  
ENDIF  

=lfRefresh(gcBaseWind+','+lcRcrChld1)    && refresh says filed on the screen
IF _WINDOWS
  =gfUpdate()
  DEACTIVATE POPUP puTermCode
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvRemit
*!
*!**************************************************************************
*
FUNCTION lfvRemit
PARAMETERS llJustShow

*** Default factor for the vendor
laData[33] = IIF(EMPTY(laData[33]), APVENDOR.cFacCode, laData[33])
laData[18] = lfRemit(laData[18], !llJustShow, laData[4], laData[33],;
                     @lcRemitStat,;
               'lcRmtComp', 'lcRmtAddr1', 'lcRmtAddr2', 'lcRmtAddr3',;
                6, 39, 'laRemitTo', @lcRemitTo, lnRemitLen)
laData[33] = IIF(laData[18] = 'F', laData[33], SPACE(6))

lnOldRemit = puRemitTo

*** If a vendor code exists, 
DO CASE
  CASE laData[18] = 'V'
    lcFactStat   = 'DISABLE'

  CASE laData[18] = 'F'
    lcFactStat  = IIF(!laScrMode[2], 'ENABLE', 'DISABLE')
    
  CASE laData[18] = 'O'
    IF llJustShow
      lcRmtComp   = laData[28]
      lcRmtAddr1  = laData[29] 
      lcRmtAddr2  = laData[30] 
      lcRmtAddr3  = laData[31] 
      SHOW GET lcRmtComp  &lcRemitStat
      SHOW GET lcRmtAddr1 &lcRemitStat
      SHOW GET lcRmtAddr2 &lcRemitStat
      SHOW GET lcRmtAddr3 &lcRemitStat
    ENDIF  
    lcFactStat   = 'DISABLE'     
ENDCASE
SHOW GET laData[33] &lcFactStat
SHOW GET ibFactor &lcFactStat
=lfRefresh(gcBaseWind)


*!**************************************************************************
*!
*!      Function : lfvDistrib
*!
*!**************************************************************************
*
FUNCTION lfvDistrib


*** Check if the invoice amount is greater than zero
IF !laScrMode[2] .AND. !( laData[13] > 0 ) 
  *** If it is not, present the following message and do not
  *** branch to the distribution lines screen.
  *** Message : "   You have to enter the ð.  "
  ***                 ® OK  ¯
  =gfModalGen("TRM04066B00000","DIALOG",lcTInv)
  _CUROBJ = OBJNUM(laData[13])
  RETURN
ENDIF  

IF !WVISIBLE('CWRAPRCRDS')
  *** Add highlight characters to child window buttons
  lcRemStat  = IIF((laScrMode[3] .OR. laScrMode[4] ) .AND.;
                    lnDistLines > 0 ,'ENABLE','DISABLE')

  IF laScrMode[4] .AND. llFirstAdd
    llFirstAdd = .F.
    SELECT (lcTmpDist)

    *** Create a default distribution record
    *** tax code is initialized to spaces
    INSERT INTO(lcTmpDist);
      (cAutMType, cAutMCode, nAPDAmnt, cAPDGlAct, nAPDLinNo, cStatus);
      VALUES(laData[1], laData[2], laData[13], lcDistAcct, 1, 'A')
 
    =gfAdd_Info(lcTmpDist)
 
    *** Default the distributed amount with the invoice amount
    lnDistLines  = 1             && current lines number
    lnDistAmnt   = laData[13]    && current total distributed amounts (default)
  
    lcVendStat   = "DISABLE"
    lcTypesStat  = "DISABLE"
    lcRemStat    = 'ENABLE'
    SHOW GET laData[4]   &lcVendStat
    SHOW GET lcCompany   &lcVendStat
    SHOW GET lcPhone     &lcVendStat
    SHOW GET ibVendor    &lcVendStat
    SHOW GET ibCompany   &lcVendStat
    SHOW GET ibPhone     &lcVendStat
    SHOW GET ibType      &lcTypesStat
    SHOW GET puType      &lcTypesStat
    SHOW GET pbRem       &lcRemStat
    SHOW GET lcAccDesc    
    SHOW WINDOW (lcBrowsTtl) REFRESH SAME
  ENDIF
  =lfDispObjct()
  IF _DOS
    lcNew      = lcTHiNew
    lcRem      = lcTHiRem
    SHOW GET pbNew,1 PROMPT lcNew &lcNewStat
    SHOW GET pbRem,1 PROMPT lcRem &lcRemStat
  ENDIF  
ENDIF  
=gfActWind('CWRAPRCRDS', lcTDstTitl, gcBaseWind)

*!**************************************************************************
*!
*!      Function : lfvAPDGlAct
*!
*!**************************************************************************
*  Valid function for the get field 'lcAPDGlAct' in the distribution 
*  lines screen
*
FUNCTION lfvAPDGlAct
PRIVATE lcCurAlias

=lfvAccounts('lcApdGLAct','lcApDGLDesc')
lcCurAlias   = ALIAS()
SELECT (lcTmpDist)
REPLACE &lcTmpDist..cApdGLAct WITH ;
          IIF(EMPTY(STRTRAN(STRTRAN(lcApdGLAct,'-'),'0')),;
              SPACE(lnApsAcLen), lcApDGlAct),;
        &lcTmpDist..cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)           
SELECT (lcCurAlias)


*!**************************************************************************
*!
*!      Function : lfBrowse
*!
*!**************************************************************************
*
FUNCTION lfBrowse
PRIVATE lcClrSchm, lcCurAlias

lcCurAlias = ALIAS()
lcClrSchm  = IIF(_DOS," COLOR SCHEME 13","NOMENU")

IF laScrMode[3] .OR. laScrMode[4]
  SELECT(lcTmpDist)
  lcBrowsTtl = lcTA_EDist     && 'Add / Edit Distribution lines'
ELSE
  SELECT APINVADT
  lcBrowsTtl = lcTV_Dist      && 'View Distribution lines'
ENDIF    

IF llApGlLink
  lnActWdth   = lnApsAcLen
  IF APVENDOR.CTAXTYPE = 'L'
    lnTaxWdth   = 10
    lnDesWdth = IIF(_DOS, 38, 34) - lnApsAcLen
  ELSE
    lnDesWdth = IIF(_DOS, 49, 45) - lnApsAcLen
  ENDIF   
ELSE
  IF APVENDOR.CTAXTYPE = 'L'
    lnActWdth   = lnApsAcLen
    lnTaxWdth   = IIF(_DOS, 49, 45) - lnApsAcLen
  ELSE
    lnActWdth   = IIF(_DOS, 50, 46)
  ENDIF   
ENDIF

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcBrowStr = "nAPDLinNo :4 :R :H='No.' :W= .F.,"+;
*            IIF(APVENDOR.cTaxType = 'L',;
*            "CTAXDES=gfCodDes(cTaxCode) :"+STR(lnTaxWdth,2)+":H='Tax Code' :R,","")+;                
*            "CAPDGLACT :"+STR(lnActWdth,2)+" :H='Account' :P=lcApsAcMas :R,"+;
*            IIF(llApGlLink,;
*            "CDESC = ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,cAPDGlAct,"+;
*            "lcLinkChar.cAcctCode,'ACCTCODE')) :"+;
*            STR(lnDesWdth,2)+":R :H='Description',","")+;            
*            "NAPDAMNT :15 :H='Dist. amount' :R"
 
lcBrowStr = "nAPDLinNo :4 :R :H='No.' :W= .F.,"+;
            IIF(APVENDOR.cTaxType = 'L',;
            "CTAXDES=gfCodDes(cTaxCode , 'CTAXCODE') :"+STR(lnTaxWdth,2)+":H='Tax Code' :R,","")+;                
            "CAPDGLACT :"+STR(lnActWdth,2)+" :H='Account' :P=lcApsAcMas :R,"+;
            IIF(llApGlLink,;
            "CDESC = ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,cAPDGlAct,"+;
            "lcLinkChar.cAcctCode,'ACCTCODE')) :"+;
            STR(lnDesWdth,2)+":R :H='Description',","")+;            
            "NAPDAMNT :15 :H='Dist. amount' :R"

*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

BROWSE FIELDS &lcBrowStr;
              WINDOW (lcRcrChld2) ;
              WHEN lfwDisBrow();
              VALID :F lfvBrowse();
              IN WINDOW CWRAPRCRDS;
              LOCK 0;
              NOAPPEND;
              NOEDIT;
              NODELETE;
              NOCLEAR;
              NOWAIT;
              SAVE;
              TITLE lcBrowsTtl &lcClrSchm
               
SELECT (lcCurAlias)

*!**************************************************************************
*!
*!      Function : lfvBrowse
*!
*!**************************************************************************
*
FUNCTION lfvBrowse
IF !WONTOP(lcBrowsTtl)
  =lfBrowUnTrap() .AND. gfStopBrow()                  
  *** If getting into any screen other than those invoilved in the traps,
  *** release traps
  IF !WONTOP('GWCCONTRL1') .OR. WPARENT(WONTOP()) <> 'CWRAPRCRDS'
    IF llTrapTab
      ON KEY LABEL TAB      
      ON KEY LABEL BACKTAB  
      ON KEY LABEL ESC        &lcOnEsc
      ON KEY LABEL CTRL+ENTER &lcOnCtrlEntr
      llTrapTab = .F.
    ENDIF 
  ENDIF  
  SELECT APINVAHD
ENDIF

*!**************************************************************************
*!
*!      Function : lfwDisBrow
*!
*!**************************************************************************
*
FUNCTION lfwDisBrow
=lfDispObjct()
SHOW GET laData[32]

*!**************************************************************************
*!
*!      FUNCTION : lfvNew
*!
*!**************************************************************************
* 
FUNCTION lfvNew
*** Look for an appropriate place to default the distribution account,
*** but certainly not with every new line
IF EMPTY(STRTRAN(STRTRAN(lcDistAcct,'-'),'0'))
  IF !EMPTY(APVENDOR.CEXPACCT)
    lcDistAcct = APVENDOR.CEXPACCT
  ELSE
    IF SEEK(laData[17],'APDIV') .AND. !EMPTY(APDIV.CAPACCT)
      lcDistAcct = APDIV.CEXPACCT
    ELSE
      lcDistAcct = APSETUP.CEXPACCT
    ENDIF
  ENDIF
ENDIF  

*** Increment the number of lines
lnDistLines = lnDistLines + 1   

*** Default the new record with the undistributed amount
INSERT INTO(lcTmpDist);
    (cAutMType, cAutMCode, nAPDAmnt, cAPDGlAct, nAPDLinNo, cStatus);
    VALUES(laData[1], laData[2], laData[13]-lnDistAmnt,;
    lcDistAcct, lnDistLines, 'A')
=gfAdd_Info(lcTmpDist)

IF APVENDOR.CTAXTYPE = 'L'
  _CUROBJ = IIF(_DOS, OBJNUM(ibTaxCode), OBJNUM(lcTaxDes))
ELSE
  _CUROBJ = OBJNUM(lcApdGLAct)
ENDIF  
lnDistAmnt = lnDistAmnt + &lcTmpDist..nAPDAmnt

lcRemStat  = "ENABLE"
STORE "DISABLE" TO lcVendStat, lcTypesStat 
SHOW GET laData[4] DISABLE
SHOW GET lcCompany DISABLE
SHOW GET lcPhone   DISABLE
SHOW GET ibVendor  DISABLE
SHOW GET ibCompany DISABLE
SHOW GET ibPhone   DISABLE
SHOW GET ibType    DISABLE
SHOW GET puType    DISABLE
SHOW GET pbRem     ENABLE

SHOW WINDOW (lcBrowsTtl) REFRESH SAME

=lfDispObjct()

*!**************************************************************************
*!
*!      FUNCTION : lfvRem
*!
*!**************************************************************************
* 
FUNCTION lfvRem

*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
  
  lnDistLines = lnDistLines - 1
  
  SELECT(lcTmpDist)
  lnDistAmnt  = lnDistAmnt - nApdAmnt
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)
  
  *** Delete the current record (to be removed )
  *** and update the line number field
  DELETE

  *** Check if you have to go to next record or the top one
  SKIP 
  IF !EOF(lcTmpDist)
    lnRecNo = RECNO()
    REPLACE REST nAPDLinNo WITH nAPDLinNo - 1,;
                 cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)                            
    GO lnRecNo
  ELSE
    SKIP -1  
  ENDIF  

  SHOW WINDOW (lcBrowsTtl) REFRESH
  IF lnDistLines = 0 
    *** Enable the vendor selection fields only in Add mode
    IF laScrMode[4]
      lcVendStat   = "ENABLE"
      SHOW GET laData[4]  ENABLE
      SHOW GET lcCompany  ENABLE 
      SHOW GET lcPhone    ENABLE
      SHOW GET ibVendor   ENABLE
      SHOW GET ibCompany  ENABLE
      SHOW GET ibPhone    ENABLE
    ENDIF  
    lcTypesStat  = "ENABLE"
    lcRemStat    = "DISABLE"
    SHOW GET ibType     ENABLE
    SHOW GET puType     ENABLE
    SHOW GET pbRem     DISABLE
    =lfDispObjct('DISABLE')
  ELSE
    =lfDispObjct()  
  ENDIF
ENDIF




*!**************************************************************************
*!
*!      Function: lfwAccounts
*!
*!**************************************************************************
*
FUNCTION lfwAccounts
PARAMETERS lcObjName, lcDescName
lcOldVal  = EVALUATE(lcObjName)
lcOldDesc = EVALUATE(lcDescName)

*!**************************************************************************
*!
*!      Function: lfvAccounts
*!
*!**************************************************************************
*
FUNCTION lfvAccounts
PARAMETERS lcObjName, lcDescName
PRIVATE lcAcct, lcAcctDesc

lcAcct     = EVALUATE(lcObjName)
lcAcctDesc = SPACE(60)
IF llApGlLink
 
  *** Get a new account and its description
  IF !lfApAcs(@lcAcctDesc,llBrowse)  
    &lcObjName  = lcOldVal
    &lcDescName = lcOldDesc 
    _CUROBJ    = _CUROBJ
  ELSE
    &lcDescName = lcAcctDesc
  ENDIF
  SHOW GET (lcDescName)
ELSE
  IF '?' $ lcAcct .OR. EMPTY(STRTRAN(STRTRAN(lcAcct,'-'),'0'))
    &lcObjName = lcOldVal
    _CUROBJ    = _CUROBJ
  ENDIF 
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfDispObjct
*!
*!**************************************************************************
* Valid function for the objects display in the child screen.
*
FUNCTION lfDispObjct
PARAMETERS lcStatOfDisp
PRIVATE lnElemNum

lcStatOfDisp = IIF(TYPE('lcStatOfDisp') <> 'C'," ",lcStatOfDisp) 

IF laScrMode[3] .OR. laScrMode[4]
  SELECT(lcTmpDist)
ELSE
  SELECT APINVADT
ENDIF    

lcApdGLAct  = IIF(EMPTY(cApdGLAct),lcEmptyAcc,cApdGLAct)
lcAPDGlDesc = IIF(llApGlLink,;
                  ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,lcApdGLAct,;
                  lcLinkChar.cAcctCode,"ACCTCODE")),' ')
lnApDAmnt   = nApDAmnt
lcTaxCode   = cTaxCode

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcTaxDes    = gfCodDes(lcTaxCode)
lcTaxDes    = gfCodDes(lcTaxCode , 'CTAXCODE')
*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

*** Prepare state of display of child window objects,
*** Tax pop state, distribution amount state
IF lnDistLines > 0
  IF !EMPTY(lcStatOfDisp)
    STORE lcStatOfDisp TO lcTaxStat, lcAmntStat
  ELSE  
    lcTaxStat = IIF(APVENDOR.cTaxType = 'L' .AND. !laScrMode[2],;
                    'ENABLE', 'DISABLE')
    lcAmntStat= IIF(!laScrMode[2], 'ENABLE', 'DISABLE') 
  ENDIF
ELSE
  STORE 'DISABLE' TO lcTaxStat, lcAmntStat
ENDIF
  
*** Account states depend on the tax type, comes from tax code validation
=lfvTaxCode(.T.,lcStatOfDisp)
SHOW GET lnApdAmnt   &lcAmntStat
SHOW GET lcTaxDes    &lcTaxStat    && Windows popup
SHOW GET ibTaxCode   &lcTaxStat    && Dos     popup

IF APVENDOR.cTaxType = 'T' .OR. _WINDOWS
  =lfRefresh('CWRAPRCRDS,'+lcRcrChld1+','+lcRcrChld4)
ELSE
  =lfRefresh('CWRAPRCRDS,'+lcRcrChld1+','+lcRcrChld3+','+lcRcrChld4)
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*  Local save procedure
*
PROCEDURE lpSavScr

SELECT APINVAHD

*** llCSave = .F. if saving will not be possible
DO CASE
  CASE EMPTY(laData[4])
    *** Message : "   You have to enter the ð.  "
    ***                 ® OK  ¯
    =gfModalGen("TRM04066B00000","DIALOG",lcTVendor)
    _CUROBJ = OBJNUM(laData[4])
    llCSave    = .F.
  CASE laScrMode[4] .AND. !SEEK(laData[4],'APVENDOR')
    *** Message :   "    ð not found.  "
    ***             "         ®  Ok  ¯          "
    = gfModalGen("TRM04002B00000","DIALOG",lcTCapVen+ALLTRIM(laData[4]))
    llCSave     = .F.
    
  *** if the entered value is not greater than zero,
  CASE !(laData[13] > 0 )
    ***     Message :  "   ð should be greater than ð.   "
    ***                            ®  OK   ¯
    *** restore the old value of the field
    =gfModalGen("TRM04072B00000","DIALOG",lcTInvAmnt+'|'+lcTZero) > 0)) 
    _CUROBJ = OBJNUM(laData[13])
    llCSave = .F.
  
  *** If the distributed amount is not equal to the invoice
  *** amount, present the following message and do not save
  CASE lnDistAmnt <> laData[13]
    *** Message : " The distribution amount does not "
    ***           " total the invoice amount.        "
    ***           "               ®  Ok  ¯             "
    =gfModalGen("TRM04031B00000","DIALOG")
    llCSave = .F.

  CASE laScrMode[4] .AND. !EMPTY(APVENDOR.CFACCODE) ;
       .AND. laData[18] <> 'F'
    *** Message : " Vendor ð is factored, this invoice has not been  "
    ***           " remited to the factor.                           "
    ***           " <Remit to factor> <Stop saving> <Continue saving>"
    *** ð = 'Vendor name'
    lnOption = gfModalGen("QRM04065B04003","DIALOG",ALLTRIM(laData[4]))
    DO CASE
      CASE lnOption = 1
        laData[18]  = 'F'
        laData[33]  = APVENDOR.cFacCode
        
      CASE lnOption = 2
        llCSave = .F.
    ENDCASE
  
    *** If the invoice is to be remit to factor, and there is
    *** no factor code, do not save and present the following message
    CASE laData[18] = 'F' .AND. EMPTY(laData[33])
      *** Message : "   You have to enter the ð.  "
      ***                 ® OK  ¯
      =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
      _CUROBJ = OBJNUM(laData[33])
      llCSave    = .F.
ENDCASE

IF llCSave
  *** If the invoice is to be remitted to 'Other',
  *** store the Other's information
  IF laData[18] = 'O'
    laData[28] = lcRmtComp
    laData[29] = lcRmtAddr1
    laData[30] = lcRmtAddr2
    laData[31] = lcRmtAddr3
  ELSE
    STORE SPACE(40) TO laData[28], laData[29], laData[30], laData[31] 
  ENDIF
  laData[33]   = IIF(laData[18] = 'F', laData[33], SPACE(6))

  *** If accounts fields are empty, clear them from '0'
  IF EMPTY(STRTRAN(STRTRAN(laData[23],'-'),'0'))
    laData[23]   = SPACE(lnApsAcLen)
  ENDIF  
  IF EMPTY(STRTRAN(STRTRAN(laData[32],'-'),'0'))
    laData[32]   = SPACE(lnApsAcLen)
  ENDIF  
  SELECT APINVAHD

  IF laScrMode[4]
    APPEND BLANK
  ENDIF

  *** Store laData values in the current record
  GATHER FROM laData FIELDS &lcScFields

  =gftmp2Mast('APINVADT',lcTmpDist)       && 'Saving recurring payable '+laData[2]+' ...')
  
ENDIF
  
SELECT APINVAHD


*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*  Local delete procedure  
*
PROCEDURE lpDelScr

*** Delete details
SELECT APINVADT
SCATTER MEMVAR MEMO BLANK
SCAN FOR &lcInvADtEx = laData[1] + laData[2]
  GATHER MEMVAR MEMO
  DELETE
ENDSCAN

*** Then delete the header record
SELECT APINVAHD
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

*** Return to "SELECT" mode
laScrMode        = .F.
laScrMode[1]     = .T.

*!**************************************************************************
*!
*!      Function: lfActivate
*!
*!**************************************************************************
*  Read cycle Activate clause function
*
FUNCTION lfActivate

SELECT APINVAHD

*** WVISIBLE(WPARENT(WOUTPUT())) returns .f. if the
*** current output window WOUTPUT() is CWRAPRCRDS but is not visible,
*** as in pushing 'Distribution lines' button, 
*** and returns .T. if the current output window is the main window
*** hence, this function activates the child window if it is not visible
IF !WVISIBLE(WPARENT(WOUTPUT()))
  SHOW WINDOW (WPARENT(WOUTPUT())) TOP
ENDIF

IF WPARENT(WONTOP()) = 'CWRAPRCRDS' .OR.;
  WONTOP('GWCCONTRL1') .AND. WPARENT(WLAST()) = 'CWRAPRCRDS'
  IF _DOS                      
    =lfDefinePad()
  ENDIF
  IF !llTrapTab
    ON KEY LABEL TAB         DO lpTab 
    ON KEY LABEL BACKTAB     DO lpShiftTab 
    ON KEY LABEL ESC         Do lpCtrlEntr
    ON KEY LABEL CTRL+ENTER  Do lpCtrlEntr
    llTrapTab = .T.
  ENDIF  
ELSE
  IF llTrapTab
    ON KEY LABEL TAB      
    ON KEY LABEL BACKTAB  
    ON KEY LABEL ESC        &lcOnEsc
    ON KEY LABEL CTRL+ENTER &lcOnCtrlEntr
    llTrapTab = .F.
  ENDIF  
  RELEASE PAD _BROWSE OF _MSYSMENU
ENDIF
IF !WONTOP(lcBrowsTtl) .AND. glFromBrow
  =lfBrowUnTrap()
  =gfStopBrow()
ENDIF

IF APVENDOR.CTAXTYPE = 'L'
  SHOW WINDOW (lcRcrChld3) SAME
ELSE
  HIDE WINDOW (lcRcrChld3) SAME
ENDIF  

SELECT APINVAHD


*!**************************************************************************
*!
*!      Function: lfDeact
*!
*!**************************************************************************
*
* Current Read's deactivate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfDeact
*** If getting into the browse, 
IF WONTOP(lcBrowsTtl)
  glFromBrow  = .T. 
  RELEASE PAD _BROWSE OF _MSYSMENU
  =lfBrowTrap()
  IF !llTrapTab
    ON KEY LABEL TAB         DO lpTab 
    ON KEY LABEL BACKTAB     DO lpShiftTab 
    ON KEY LABEL ESC         Do lpCtrlEntr
    ON KEY LABEL CTRL+ENTER  Do lpCtrlEntr
    llTrapTab = .T.
  ENDIF  
ENDIF


*!**************************************************************************
*!
*!      Function: lfvClsChld
*!
*!**************************************************************************
*
FUNCTION lfvClsChld
IF _DOS
  lcNew      = lcTNew
  lcRem      = lcTRem
  SHOW GET pbNew,1 PROMPT lcNew &lcNewStat
  SHOW GET pbRem,1 PROMPT lcRem &lcRemStat
ENDIF
=gfChClose('CWRAPRCRDS')

*!**************************************************************************
*!
*!      Function: lfGetArrElem
*!
*!**************************************************************************
*
FUNCTION lfGetArrElem
PARAMETERS lcSrchCode,laSrchArr

PRIVATE lnArrElem

lnArrElem = 1

DO WHILE lcSrchCode <> &laSrchArr[lnArrElem, 2] .AND. ;
         lnArrElem < ALEN((laSrchArr),1)
  lnArrElem = lnArrElem + 1
ENDDO  

RETURN IIF(BETWEEN(lnArrElem, 1, ALEN((laSrchArr), 1)),lnArrElem, 0)

*!**************************************************************************
*!
*!      Function: lfvPopups
*!
*!**************************************************************************
*
FUNCTION lfvPopups
PARAMETERS lcArrName, lcDispFld, lnStRow, lnStCol, lnPopWidth

PRIVATE lcRetVal, lnPopLen

DO CASE
  CASE _DOS
    lnPopLen     = ALEN((lcArrName),1)
    lcRetVal     = gfActPop(lnStRow, lnStCol, ;
                            IIF(lnPopLen>5,lnStRow + 7,lnStRow + 2 + lnPopLen),;
                            lnStCol + 3 + lnPopWidth,;
                            lcArrName, 2, 1, @lcDispFld)
  CASE _WINDOWS
    lcRetVal    = IIF(EVALUATE(SYS(18)) > 0,;
                      &lcArrName[EVALUATE(SYS(18)),2]," ")
                      
ENDCASE
RETURN lcRetVal


*!**************************************************************************
*!
*!      Function: lfvDistAmnt
*!
*!**************************************************************************
*
FUNCTION lfvDistAmnt
PRIVATE lcCurAlias

*** Adjust totals
lnDistAmnt  = lnDistAmnt - &lcTmpDist..nAPDAmnt + lnAPDAmnt
lcCurAlias  = ALIAS()
SELECT (lcTmpDist)
REPLACE nAPDAmnt WITH lnAPDAmnt,;
        cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)           
SELECT (lcCurAlias)        
=lfRefresh(lcRcrChld1)

*!**************************************************************************
*!
*!      Function: lfwTaxCode
*!
*!**************************************************************************
*
FUNCTION lfwTaxCode
PRIVATE  lcCurAlias

lcCurAlias = ALIAS()

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = 'CTAXCODE'
LOCATE
SELECT (lcCurAlias)


*!**************************************************************************
*!
*!      Function: lfvTaxCode
*!
*!**************************************************************************
*
FUNCTION lfvTaxCode
PARAMETERS llJustShow, lcStatOfDisp
PRIVATE lcCurAlias, lcOldTax, lcOldAcct

lcOldTax    = &lcTmpDist..cTaxCode
lcOldAcct   = &lcTmpDist..cApDGlAct

IF !llJustShow
  DO CASE
    CASE _DOS
      lcTaxCode =gfActPop(0,11,5,44,'SYCCODES','cCode_No','cDiscrep',@lcTaxDes)

    CASE _WINDOWS

      *E300643,1 Change this lines for the changes we have made 
      *          to SYCCODES [Begin]
      *lcTaxDes  = SYCCODES.cdiscrep
      *lcTaxCode = SYCCODES.cCode_No
      lcTaxDes  = CODES.cdiscrep
      lcTaxCode = CODES.cCode_No
      *E300643,1 Change this lines [End]
      
      SHOW GET lcTaxDes
  ENDCASE
ENDIF

IF !EMPTY(lcTaxCode) 

  *E300643,1 Change this line for the changes we have made 
  *          to (gfRltFld) [Begin]
  *=gfRltFld(lcTaxCode,@laTaxCode)
  =gfRltFld(lcTaxCode , @laTaxCode , 'CTAXCODE')
  *E300643,1 Change this line [End]
  
  lcApDGlAct = SUBSTR(lcApDGlAct,1,lnApsAcLen)
  lcApDGlAct = IIF(!EMPTY(STRTRAN(STRTRAN(lcApDGlAct,'-'),'0')),;
                     lcApDGlAct, lcEmptyAcc)        
  
  *** If there is a G/L link, validate the G/L input account,
  *** from the chart of accounts.
  *** If it is not found, present the following message and
  *** reset tax code to N/A
  *** Message :    "    G/L input account ð  for tax code ð     "
  ***              "    is not found in the chart of accounts.  "
  ***              "    You cannot select this tax code.        "
  IF !llJustShow
    IF llApGlLink .AND.;
      !SEEK(lcApDGlAct, 'lcLinkChar')
      =gfModalGen("TRM04091B00000","DIALOG",;
                   ALLTRIM(lcApDGlAct)+;
                   '|'+ALLTRIM(lcTaxDes))
      IF lcTaxCode <> lcOldTax
        lcApDGlAct  = lcOldAcct
        lcTaxCode   = lcOldTax

        *E300643,1 Change this line for the changes we have made 
        *          to (gfCodDes) [Begin]
        *lcTaxDes    = gfCodDes(lcOldTax)
        lcTaxDes    = gfCodDes(lcOldTax , 'CTAXCODE')
        *E300643,1 Change this line [End]
        
      ELSE 
        lcApDGlAct = lcDistAcct
        lcTaxCode  = SPACE(6)
        lcTaxDes   = lcEmptyCode
      ENDIF  
      SHOW GET lcTaxDes
    
    ENDIF
  ENDIF    
ENDIF
IF !llJustShow
  lcCurAlias = ALIAS()
  SELECT (lcTmpDist)
  REPLACE cTaxCode  WITH lcTaxCode,;
          cApdGlAct WITH lcApdGlAct,;
          cStatus   WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)           
  SELECT (lcCurAlias)        
ENDIF

lcAcctStat = IIF(TYPE('lcStatOfDisp') = 'C' .AND. !EMPTY(lcStatOfDisp),;
                lcStatOfDisp, IIF(lnDistLines > 0 .AND. ;
                (laScrMode[3] .OR. laScrMode[4]);
                .AND. EMPTY(lcTaxCode) , 'ENABLE', 'DISABLE'))
SHOW GET lcApdGlAct  &lcAcctStat
SHOW GET ibApGlAcc   &lcAcctStat
SHOW GET lcApDGlDesc &lcAcctStat
IF _DOS
  =lfRefresh(lcRcrChld3)
ElSE
  =gfUpdate()
  DEACTIVATE POPUP puTaxDes
ENDIF

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[21] (bank code), and laData[22] 
* (checking account code)
*
FUNCTION lfvBnkChk
*E300296,4 Add parameters to lfBnkChk to restrict bank\checking account
*E300296,4 selection to those using the same currency as that of the 
*E300296,4 invoice.
*IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse)

IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, '',;
             '', '', laData[34])     
  RETURN 1
*E300296,4 Enable lcAccDesc if a valid bank is selected.
ELSE
  SHOW GET lcAccDesc ENABLE  
*E300296,4 end.  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvAmounts
*!
*!**************************************************************************
* Valid function of the discount.
*
FUNCTION lfvAmounts
PRIVATE lcCurObj

lcCurObj   = SYS(18)

IF EVALUATE(lcCurObj) > 0
  *** If the total approved amount (approved amount + approved discount+
  *** approved adjustment) > the invoice amount , present the following
  *** message and return the old value of the object
  IF laData[24]+laData[25]+laData[26] > laData[13]
    *** Message : " Total approved amount can not be greater than "
    ***           " the open invoice amount.                              "
    ***           "                       ®  Ok  ¯                  "
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
    &lcCurObj = lnOldVal
    _CUROBJ   = _CUROBJ
  ENDIF
ELSE
  *** Check if negative entries are accepted
  IF EVALUATE(lcCurObj) < 0 .AND. gfModalGen("TRM04087B00000","DIALOG") > 0) 
    &lcCurObj = lnOldVal
    _CUROBJ   = _CUROBJ
  ENDIF
ENDIF
  
*!**************************************************************************
*!
*!      Function: lfvData_27
*!
*!**************************************************************************
* Validation function of 1099 amount.
*
FUNCTION lfvData_27
*** If the approved 1099 amount is greater than the approved to pay
*** amount, 
*** or, the approved 1099 amount is negative,
*** present the following message(s) and return the old value 
*** of the field
IF laData[27] <> lnOldVal .AND.;
  (laData[27] > 0 .AND. laData[27] > laData[24] .AND.;
   gfModalGen("TRM04025B00000", "DIALOG",;
   lcT1099Am + '|' + lcTAprPay) > 0 ;
   .OR. laData[27] < 0 .AND. gfModalGen("TRM04087B00000","DIALOG") > 0)) 
 laData[27] = lnOldVal
 _CUROBJ  = _CUROBJ
ENDIF

*!**************************************************************************
*!
*!      Function: lfvOkApr
*!
*!**************************************************************************
* Validation function of the ok push button of the Approved for payment.
*
FUNCTION lfvOkApr
PRIVATE lcEmptyObj, llOK

lcEmptyObj = ' '
llOk       = .T.
IF !laScrMode[2]   && In case of view mode ®  Ok  ¯ is terminating
  DO CASE
    CASE laData[24] = 0 
      *** Message : " You have to enter the approved to pay amount."
      ***                       ®  Ok  ¯         
      =gfModalGen("TRM04022B00000","DIALOG")
      _CUROBJ = OBJNUM(laData[24])
      llOK = .F.   
    CASE laData[20] = 'H' .AND. EMPTY(STRTRAN(STRTRAN(laData[23],'-'),'0'))
      *** Message : " You have to enter the ð account.     "
      ***                             ®  Ok  ¯             
      =gfModalGen("TRM04020B00000","DIALOG",lcTPayAcct)
      _CUROBJ = OBJNUM(laData[23])
      llOK = .F.       
    CASE laData[20] <> 'H'      
      lcEmptyObj = IIF(EMPTY(laData[21]),'laData[21]',;
                       IIF(EMPTY(laData[22]),'laData[22]',;
                          IIF(EMPTY(STRTRAN(STRTRAN(laData[23],'-'),'0')),;
                          'laData[23]',' ')))
  ENDCASE
  IF !EMPTY(lcEmptyObj)
    llOK = .F.   
    *** Message : " You have to enter the bank code,the  "
    ***           " checking account,and the GL account. "
    ***           "                  ®  Ok  ¯              "
    =gfModalGen("TRM04021B00000","DIALOG")
    _CUROBJ = OBJNUM(&lcEmptyObj) 
  ENDIF    
ENDIF
IF llOK
  CLEAR READ    
ENDIF

*!**************************************************************************
*!
*!      Function: lfvClrApr
*!
*!**************************************************************************
* Validation function of the Clear approval  push button of the
* Approved for payment screen
*
FUNCTION lfvClrApr

laData[21] = SPACE(8)
laData[22] = SPACE(12)
laData[23] = lcEmptyAcc
laData[24] = 0      
laData[25] = 0
laData[26] = 0
laData[27] = 0
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvCanPay
*!
*!**************************************************************************
* Validation function of the cancel push button of the Approved for payment.
*
FUNCTION lfvCanPay

laData[21] = lcBankCode
laData[22] = lcCheckAcc
laData[23] = lcGLAcount
laData[24] = lcApprPay
laData[25] = lcApprDis
laData[26] = lc1099Amnt
laData[27] = lcApprAdj
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfLineNo
*!
*!**************************************************************************
* Increments line number
*
FUNCTION lfLineNo

PARAMETERS lnLineNo
lnLineNo = lnLineNo + 1
RETURN lnLineNo

*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
*
FUNCTION lfDefinePad
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
ON SELECTION PAD _BROWSE OF _msysmenu ACTIVATE WINDOW (lcBrowsTtl)

*!**************************************************************************
*!
*!      Function: lfBrowTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowTrap
ON KEY LABEL ALT-N      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL ALT-V      DO lpBrowKeyTrap WITH LASTKEY()


*!**************************************************************************
*!
*!      Function: lfBrowUnTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowUnTrap
ON KEY LABEL ALT-N 
ON KEY LABEL ALT-V 


*!**************************************************************************
*!
*!      Function: lpBrowKeyTrap
*!
*!**************************************************************************
*
FUNCTION lpBrowKeyTrap
PARAMETERS lnLastKey
KEYBOARD "SHIFT+HOME"
IF !laScrMode[2]
  DO CASE
    *** Case Alt + N
    CASE lnLastKey = 49                          && push button <  New   >
      =lfvNew()
    *** Case Alt + V  
    CASE lnLastKey = 47 .AND. lnDistLines > 0     && push button < Remove >
      =lfvRem()
  ENDCASE    
ENDIF


*!**************************************************************************
*!
*!      Procedure: lpCtrlEntr
*!
*!**************************************************************************
*
PROCEDURE lpCtrlEntr
IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU 
ENDIF  
ACTIVATE WINDOW (lcRcrChld5)
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!**************************************************************************
*!
*!      Procedure: lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab
IF _DOS
  HIDE MENU _MSYSMENU SAVE
  SHOW MENU _MSYSMENU SAVE
ENDIF  
ON KEY LABEL TAB lnDummmi = 1
DO CASE
  CASE WONTOP(lcRcrChld1) .AND. _CUROBJ = OBJNUM(laData[32])
    =lfvAccounts('laData[32]','lcAccDesc') .AND. gfUpdate()
    ACTIVATE WINDOW (lcBrowsTtl)      

  CASE WONTOP(lcBrowsTtl)
    IF APVENDOR.CTAXTYPE = 'L' 
      ACTIVATE WINDOW (lcRcrChld3)
      _CUROBJ = OBJNUM(ibTaxCode)
    ELSE
      ACTIVATE WINDOW (lcRcrChld4)
      _CUROBJ = OBJNUM(lcApdGlAct)
    ENDIF  

  CASE WONTOP('GWCCONTRL1') .AND. _CUROBJ = OBJNUM(pbCls)
    IF laScrMode[2]
      ACTIVATE WINDOW (lcBrowsTtl)
    ELSE
      ACTIVATE WINDOW (lcRcrChld1)
      _CUROBJ = OBJNUM(laData[32])
    ENDIF  
  OTHERWISE
    ON KEY LABEL TAB   
    KEYBOARD "{TAB}" PLAIN
ENDCASE
ON KEY LABEL TAB DO lpTab

*!**************************************************************************
*!
*!      Procedure: lpShiftTab
*!
*!**************************************************************************
*
PROCEDURE lpShiftTab
IF _DOS
  HIDE MENU _MSYSMENU SAVE
  SHOW MENU _MSYSMENU SAVE
ENDIF  
ON KEY LABEL BACKTAB lnDummmi = 1
DO CASE
  CASE APVENDOR.cTaxType = 'L' .AND. ;
       WONTOP(lcRcrChld3) .OR. ;
       APVENDOR.cTaxType = 'T' .AND. ;
       WONTOP(lcRcrChld4) .AND.;
       _CUROBJ = OBJNUM(lcApdGlAct) .OR. ;
       WONTOP(lcRcrChld5) .AND.;
         (laScrMode[2] .AND. _CUROBJ = OBJNUM(pbClose) .OR.;
          lnDistLines = 0 .AND. _CUROBJ = OBJNUM(pbNew))
    ACTIVATE WINDOW (lcBrowsTtl)      

  CASE WONTOP(lcBrowsTtl)
    IF !laScrMode[2]
      ACTIVATE WINDOW (lcRcrChld1)
      _CUROBJ = OBJNUM(laData[32])
    ELSE
      ACTIVATE WINDOW 'GWCCONTRL1'
      _CUROBJ = OBJNUM(pbCls)
    ENDIF  

  OTHERWISE
    ON KEY LABEL BACKTAB
    KEYBOARD "{BACKTAB}" PLAIN
ENDCASE
ON KEY LABEL BACKTAB DO lpShiftTab

*!**************************************************************************
*!
*!      Function: lfvData_33
*!
*!**************************************************************************
* Valid function for get field laData[33] representing factor
*
FUNCTION lfvData_33
IF llBrowse .OR. !EMPTY(laData[33]) 

  *E301077,80 IHB 03/03/1999 opend needed files only [start]
  =gfOpenfile(gcSYSHome+'SYCFACT'  ,'CFACCODE','SH')
  *E301077,80 IHB [end]
  IF lfGetFac(lcOldVal, llBrowse)

   
    lcRmtComp       = SYCFACT.cFacComp
    lcRmtAddr1      = SYCFACT.cAddress1
    lcRmtAddr2      = SYCFACT.cAddress2
    lcRmtAddr3      = lfGetAdr('SYCFACT', 'CFACCODE')

  ENDIF  
ELSE
  STORE SPACE(40) TO lcRmtComp, lcRmtAddr1, lcRmtAddr2, lcRmtAddr3
ENDIF   
SHOW GET lcRmtComp   
SHOW GET lcRmtAddr1 
SHOW GET lcRmtAddr2 
SHOW GET lcRmtAddr3 
llBrowse = .F.

*!**************************************************************************
*!
*!      Procedure: lfwData_34
*!
*!**************************************************************************
* E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
PROCEDURE lfwData_34

lcOldCurr = laData[34]

*!**************************************************************************
*!
*!      Procedure: lfvData_34
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*E300296,4 Clear approval fields if the bank\checking account
*E300296,4 does not use the selected currency
SHOW GET laData[34]  &lcDataStat
*E300296,4 end.

PROCEDURE lfvData_34

*E301077,80 IHB 03/03/1999 opend needed files only [start]
=gfOpenFile(gcSYSHome+'SYCCURR'  ,'Ccurrcode','SH')
*E301077,80 IHB [end]

IF llBrowse .OR. !SEEK(laData[34],'SYCCURR') .OR. ATC("?",laData[34]) > 0 .AND. LASTKEY() = 13
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
    laData[34] = lcOldCurr
  ELSE
    laData[34] = laTemp[1]
  ENDIF
ENDIF

SELECT APINVAHD
SHOW GET laData[34]
llBrowse = .F.   && REN

*E300296,4 Compare the new currency with the old one.
IF laData[34] <> lcOldCurr
  
  IF !EMPTY(laData[22]) .AND. (!SEEK(laData[23], 'APCHECKS');
      .OR. APCHECKS.cCurrCode <> laData[34])
    *E300296,4 Clear approval if the current checking account does
    *E300296,4 not use the selected currency.
    laData[21] = SPACE(8)
    laData[22] = SPACE(12)
    laData[23] = lcEmptyAcc
    STORE 0 TO laData[24], laData[25], laData[26], laData[27]       
  ENDIF    

ENDIF
*E300296,4 end.
  