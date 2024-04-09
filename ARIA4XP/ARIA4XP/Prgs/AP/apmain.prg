*:************************************************************************
*:  Program File: APMAIN.PRG
*:  Desc.       : General functions used by the AP screens
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/14/2012
*:  Reference   : E303064,1
*:************************************************************************



************************************************************
*! Name      : lfOpen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/15/2012
*! Purpose   : Open tables 
************************************************************
*E303064,1 TMI 02/15/2012 
FUNCTION lfOpenPRGFILES
LPARAMETERS lcProg

*E303104,1 TMI 04/08/2012 [Start] Define lcPath var
LOCAL lcPath
lcPath = ''
*E303104,1 TMI 04/08/2012 [End  ] 
lcProg = PADR(UPPER(lcProg),10)
SET MULTILOCKS ON
lnRemResult = oAriaApplication.RemoteSystemData.Execute;
   ("Select * from SYDOBJCT WHERE CAPOBJNAM= '&lcProg'",'',"SYDOBJCT","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

=gfOpenTable(oAriaApplication.SysPath+'sydFiles','CFILE_NAM','SH')   && CFILE_NAM
=gfOpenTable(oAriaApplication.SysPath+'sydField','CFLD_NAME','SH')   && CFLD_NAME

*=SEEK('AP'+lcProg,'SYDOBJCT','CAPP_ID')
lcFiles = SYDOBJCT.MPRGFILES
DIME laFiles[2],laTbl[1],laIndx[1]
STORE '' TO laFiles,laTbl,laIndx
=gfSubStr(lcFiles,@laFiles,'|')
=gfSubStr(laFiles[1],@laTbl,',')
=gfSubStr(laFiles[2],@laIndx,',')
FOR i = 1 TO ALEN(laTbl)  
  *E303104,1 TMI 04/08/2012 [Start] 
  *=gfOpenTable(laTbl[i],laIndx[i],'SH')
  lcPath = IIF(LEFT(laTbl[i],2)='SY',oAriaApplication.SysPath,'')
  =gfOpenTable(lcPath+laTbl[i],laIndx[i],'SH')
  *E303104,1 TMI 04/08/2012 [End  ] 
ENDFOR 

*** Load program base file 
lcBaseFile = ALLTRIM(sydObjct.cBaseFile)

*- End of lfOpen.

************************************************************
*! Name       : lfAddProp
*! Developer  : TMI
*! Date       : 02/05/2012
*! Purpose    : A function to add properties to the object that passed as a parameter
************************************************************
FUNCTION lfAddProp
PARAMETERS loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
LOCAL lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = OCCURS(',',lcPropName)
FOR lnI = 1 TO lnLen
  lcPropToCreate = ALLTRIM(SUBSTR(lcPropName,1,AT(',',lcPropName)-1))
  IF TYPE('loObj.&lcPropToCreate')='U'
    loObj.AddProperty(lcPropToCreate,PropValue)
  ENDIF 
  *E303107,1 TMI 04/15/2012 [Start] in case that the property is already exists
  loObj.&lcPropToCreate = PropValue
  *E303107,1 TMI 04/15/2012 [End  ] 
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR 
*- End of lfAddProp.

************************************************************************************************
*Name       : lfUpdStruArr
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : Update the rest columnas  of a newely added fields to a structure array
************************************************
FUNCTION lfUpdStruArr
PARAMETERS laFileStru,lnFileStru
LOCAL lnI,lnJ
FOR lnI = lnFileStru+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[lnI,5],laFileStru[lnI,6]
  FOR lnJ = 7 TO 16
    laFileStru[lnI,lnJ] = ""
  ENDFOR 
  STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]  
ENDFOR 
*- End of lfUpdStruArr.


*!*************************************************************
*! Name      : gfDbfField
*! Developer : Hesham Shereef
*! Date      : 1993-1995 
*! Purpose   : To build the browse field names in string from dectionary
*!*************************************************************
*! Calls     : 
*!      Called by: GFSCRINI()               (function  in ARIA3.PRG)
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*  RETURN THE HEADER FOR THE FIELDS USED IN THE BROWSING
*:->
FUNCTION gfDbfField
PARAMETER lcDbfName

*E303064,1 TMI 02/21/2012 [Start] get the A4xp syspath
gcSysHome = oAriaApplication.SysPath
*E303064,1 TMI 02/21/2012 [End  ] 

lcDbfName = ALLTRIM(UPPER(lcDbfName))

lcSavAlias=SELECT(0)
lcBrFields=''
llFilesByP=.F.
llFieldByP=.F.
llFlFldByP=.F.

IF USED("sydFlFld")
  SELECT sydflfld 
  lnFlFldRec=IIF(RECNO()>RECCOUNT(),0,RECNO())
ELSE
  llFlFldByP=.T.
  SELECT 0
  USE &gcSysHome.sydFlFld.dbf
ENDIF  

set order to tag cfile_nam

IF USED("sydField")
  SELECT sydfield 
  lnFldRec=IIF(RECNO()>RECCOUNT(),0,RECNO())
ELSE
  llFieldByP=.T.
  SELECT 0
  USE &gcSysHome.sydField.dbf
ENDIF  
set order to tag cfld_name

IF USED("sydFiles")
  SELECT sydfiles 
   lnFileRec=IIF(RECNO()>RECCOUNT(),0,RECNO())
ELSE
  llFilesByP=.T.
  SELECT 0
  USE &gcSysHome.sydFiles.dbf
ENDIF  
set order to tag cfile_nam

**********************************************************
IF SEEK(lcDbfName)

  lcbrFields=ALLTRIM(mbrow_fld)

  IF !EMPTY(lcBrFields)
    DIMENSION laHead[1]
    = gfSubStr(lcbrFields,@laHead,"|")
    SELECT sydField
    lcBrFields=""
    FOR lnCount=1 TO ALEN(laHead,1)
      laHead[lnCount] = UPPER(ALLTRIM(laHead[lnCount]))
      IF SEEK (laHead[lnCount],"sydfield")
      *E300631,1 YMA 04/06/97 Changed the lookup to use the "CODES"
      *E300631,1 YMA 04/06/97 instead of "SYCCodes"
*       lcBrFields=lcBrFields+laHead[lnCount]+;
*                  IIF(lvldEntry,'=LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+'+lcDbfName;
*                  +'.'+laHead[lnCount]+',SYCCODES.cCode_No,"CODES")';
*                  ,'')+IIF(EMPTY(sydfield.cFld_Head),' ,'," :H='"+;
*                  ALLTRIM(sydfield.cFld_Head)+"',")

        *E300789,1 Heaham (Start)
        *lcBrFields=lcBrFields+laHead[lnCount]+;
                   IIF(lvldEntry,'=LOOKUP(CODES.cDiscrep,gcAct_Comp+'+lcDbfName;
                   +'.'+laHead[lnCount]+',CODES.cCode_No,"CODES")';
                   ,'')+IIF(EMPTY(sydfield.cFld_Head),' ,'," :H='"+;
                   ALLTRIM(sydfield.cFld_Head)+"',")

        lcBrFields=lcBrFields+laHead[lnCount]+;
                   IIF(lvldEntry,'=LOOKUP(CODES.cDiscrep,'+['N'+]+lcDbfName;
                   +'.'+laHead[lnCount]+',CODES.cCode_No,"CODES")';
                   ,'')+IIF(EMPTY(sydfield.cFld_Head),' ,'," :H='"+;
                   ALLTRIM(sydfield.cFld_Head)+"',")

        *E300789,1 Heaham (End)
        *E300631,1 YMA 04/06/97 End.
      ENDIF            

    ENDFOR
    lcBrFields=SUBSTR(lcBrFields,1,LEN(lcBrFields)-1)           
  *********************************************************************
  ELSE    
    SELECT sydFlFld
    SEEK lcDbfName
    SCAN REST WHILE cFile_nam=lcDbfName
      IF SEEK(UPPER(cFld_Name),"sydField")
        IF !(ALLTRIM(sydField.cFld_Name) $ "CADD_USER,CADD_TIME,DADD_DATE,LLOK_STAT"+;
                                 ",CLOK_USER,DLOK_DATE,CLOK_TIME")

          *E300631,1 YMA 04/06/97 Changed the lookup to use the "CODES"
          *E300631,1 YMA 04/06/97 instead of "SYCCodes"
*         lcBrFields=lcBrFields+ALLTRIM(sydFlFld.cFld_Name)+;
*              IIF(sydField.lvldEntry,'=LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+'+lcDbfName;
*              +'.'+ALLTRIM(sydFlFld.cFld_Name)+',SYCCODES.cCode_No,"CODES")';
*              ,'')+IIF(EMPTY(sydField.cFld_Head),' ,'," :H='"+;
*                ALLTRIM(sydField.cFld_Head)+"',")
          *E300789,1 Heaham (Start)
          *lcBrFields=lcBrFields+ALLTRIM(sydFlFld.cFld_Name)+;
               IIF(sydField.lvldEntry,'=LOOKUP(CODES.cDiscrep,gcAct_Comp+'+lcDbfName;
               +'.'+ALLTRIM(sydFlFld.cFld_Name)+',CODES.cCode_No,"CODES")';
               ,'')+IIF(EMPTY(sydField.cFld_Head),' ,'," :H='"+;
                 ALLTRIM(sydField.cFld_Head)+"',")
        
          lcBrFields=lcBrFields+ALLTRIM(sydFlFld.cFld_Name)+;
               IIF(sydField.lvldEntry,'=LOOKUP(CODES.cDiscrep,'+['N'+]+lcDbfName;
               +'.'+ALLTRIM(sydFlFld.cFld_Name)+',CODES.cCode_No,"CODES")';
               ,'')+IIF(EMPTY(sydField.cFld_Head),' ,'," :H='"+;
                 ALLTRIM(sydField.cFld_Head)+"',")

        *E300789,1 Heaham (Start)
        *E300631,1 YMA 04/06/97 End.

        ENDIF           
      ENDIF          
    ENDSCAN
    lcBrFields=SUBSTR(lcBrFields,1,LEN(lcBrFields)-1)             
  ENDIF
ENDIF


IF llFilesByP
  USE IN sydFiles
ELSE
 SELECT sydFiles
 IF lnFileRec > 0 
   GO lnFileRec
 ENDIF  
ENDIF
IF llFieldByP
  USE IN sydField
ELSE
  SELECT sydField
  IF lnFldRec > 0
    GO lnFldRec
  ENDIF  
ENDIF
IF llFlFldByP
  USE IN sydFlFld
ELSE
 SELECT sydFlFld
 IF lnFlFldRec > 0 
   GO lnFlFldRec
 ENDIF  
ENDIF

SELECT(lcSavAlias)
RETURN lcBrFields



************************************************************
*! Name      : lfVndBrw
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/06/2012
*! Purpose   : vendor/phone/company browse
************************************************************
FUNCTION lfVndBrw
PARAMETERS loFormSet,loFld,lcType
LOCAL lcSvOrd,lnSlct,lcFld,lnFldLn
lnSlct = SELECT(0)
SELECT APVENDOR
lcSvOrd = ORDER()
SELECT APVENDOR
lcBrowOrd = IIF(lcType='V','VENCODE',IIF(lcType='P','VENPHONE','VENCOMP'))
lcFld = IIF(lcType='V','CVENDCODE',IIF(lcType='P','CPHONENO','CVENCOMP'))
lnFldLn = LEN(APVENDOR.&lcFld)
llBrowse = loFld.Selectedfrombrowse
loFld.Selectedfrombrowse = .F.
loFld.KeyTextBox.VALUE = PADR(loFld.KeyTextBox.VALUE,lnFldLn)

lcFile_Ttl = "Vendors"
lcBrFields = "CVENDCODE :H= 'Vendor Code',;
              CVENCOMP :H= 'Company',;
              CPHONENO :H= 'Phone'"

IF llBrowse .OR. !EMPTY(loFld.KeyTextBox.VALUE)
  IF llBrowse .OR. !SEEK(loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  &&lower
    lnClosRec  = RECNO(0)
    IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
      DIMENSION laTemp[3]
      laTemp = ''
      IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF
      =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp','Vendors')
      IF !EMPTY(laTemp[1])
        loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(laTemp[1])  &&lower
        loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = ALLTRIM(laTemp[2])
        loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = ALLTRIM(laTemp[3])
      ELSE
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
      ENDIF
    ELSE
      lcTVenKey = IIF(lcType='V','Vendor Code',IIF(lcType='P','Phone','Vendor/Company name'))
      lnOption  = gfModalGen('QRM00001B00014','Dialog',;
      lcTVenKey + " : " +ALLTRIM(loFld.KeyTextBox.VALUE))  
      
      DO CASE
        CASE lnOption = 1
          DIMENSION laTemp[3]
          laTemp = ''
          IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
            GO lnClosRec
          ELSE
            GO TOP
          ENDIF
	
          =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp','Vendors')
          
          IF !EMPTY(laTemp[1])
            loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(laTemp[1])  &&lower
            loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = ALLTRIM(laTemp[2])
            loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = ALLTRIM(laTemp[3])
          ELSE
            loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
          ENDIF
          loFormSet.REFRESH()
        CASE lnOption = 2
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
          RETURN .F.
      ENDCASE
    ENDIF
  ELSE
    loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = APVENDOR.CVENDCODE
    loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = APVENDOR.CPHONENO
    loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = APVENDOR.CVENCOMP
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
ENDIF
  
SET ORDER TO &lcSvOrd
SELECT (lnSlct)
*- End of lfVndBrw.


************************************************************
*! Name      : lfvRemit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/07/2012
*! Purpose   : Remit to 
************************************************************ 
FUNCTION lfvRemit
PARAMETERS loFormSet,llJustShow
LOCAL lnSlct

lnSlct = SELECT(0) 
 
** Default factor for the vendor
lcFactor = loFormset.Ariaform1.kbFacCode.Keytextbox.Value
lcFactor   = IIF(EMPTY(lcFactor), APVENDOR.cFacCode, lcFactor)

lcRemitStat = loFormSet.lcRemitStat 
WITH loFormset.Ariaform1
  lcRem1 = .txtOutComp.Value 
  lcRem2 = .txtOutAddr1.Value 
  lcRem3 = .txtOutAddr2.Value 
  lcRem4 = .txtOutAddr3.Value 
  lcRem5 = '' 
  lcRem6 = ''

  DIMENSION laRemitTo[3,2]
  lcRemitTo = .cboInvRemit.DisplayValue
  lnRemitLen = gfGetVld('cInvRemit',@laRemitTo)
  .cboInvRemit.Value = gfRemit(.cboInvRemit.Value , !llJustShow, .KBVendCode.Keytextbox.Value, lcFactor, @lcRemitStat,;
               'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4','lcRem5','lcRem6',;
                13, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)

  ACOPY(laRemitTo,loFormSet.laRemitTo)
  .kbFacCode.Keytextbox.Value  = IIF(.cboInvRemit.Value = 'F', lcFactor , SPACE(6))
  loFormSet.lcFactStat  = IIF(.cboInvRemit.Value = 'F', 'ENABLE', 'DISABLE')
  .kbFacCode.Enabled = IIF(.cboInvRemit.Value  = 'F', .T. , .F. )

  .txtOutComp.Value  = IIF(.cboInvRemit.Value='F','',lcRem1)
  .txtOutAddr1.Value = IIF(.cboInvRemit.Value='F','',lcRem2)
  .txtOutAddr2.Value = IIF(.cboInvRemit.Value='F','',lcRem3)
  .txtOutAddr3.Value = IIF(.cboInvRemit.Value='F','',lcRem4)
  loFormSet.lcRem5 = lcRem5

  llAddrsEnabled = .cboInvRemit.Value = 'O'
  .txtOutComp.Enabled = llAddrsEnabled
  .txtOutAddr1.Enabled = llAddrsEnabled
  .txtOutAddr2.Enabled = llAddrsEnabled
  .txtOutAddr3.Enabled = llAddrsEnabled
ENDWITH 

SELECT (lnSlct)
*- End of lfvRemit.


*!*************************************************************
*! Name      : gfRemit
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 7/29/2004
*! Purpose   : Global Function.
*!*************************************************************
*! Parameters: Many
*!*************************************************************
*! Returns   : lcRmtToCode
*!*************************************************************
FUNCTION gfRemit
PARAMETERS lcRmtToCode, llActPopup, lcVendCode, lcFactCode, lcRemitStat,;
           lcRemitCmp, lcRemitAd1, lcRemitAd2, lcRemitAd3,lcRemitAd4,lcRemitAd5, lnRow, lnCol,;
           lcArrName, lcDispStr, lnPopWdth
*** lcRmtToCode : normally ('V', 'F', 'O')
*** llActPopup  : .T. if the popup is to be activated
*** lcVendCode  : vendor code
*** lcFactCode  : factor code
*** lcRemitStat : remit objects display status
*** lcRemitCmp  : remit company object name
*** lcRemitAd1  : remit address1 object name
*** lcRemitAd2  : remit address2 object name
*** lcRemitAd3  : remit address(3+4+5) object name
*** lnRow       : DOS popup top left row
*** lnCol       : DOS popup top left column
*** lcArrName   : remit to array name
*** lcDispStr   : display string of the popup
*** lnPopWdth   : dos popup width
*** example call from APPYINV.PRG
*** laData[5]    = lfRemit(laData[5], !llJustShow, laData[1], laData[36],;
***              @lcRemitStat, 'laData[6]', 'laData[7]', 'laData[8]', 'laData[9]',;
***              6, 39, 'laRemitTo', @lcRemitTo, lnRemitLen)
PRIVATE lcRemitFil, lcRemitTag, lcKeyCode

*Old: IF llActPopup
  *Old: lcRmtToCode = lfvPopups(lcArrName, @lcDispStr,;
                       lnRow, lnCol, lnPopWdth)
*Old: ELSE
  lcRmtToCode = UPPER(ALLTRIM(lcRmtToCode)) 
*Old: ENDIF                           

lcKeyCode = IIF(lcRmtToCode = 'V', lcVendCode, lcFactCode)
STORE .F. TO llOpVen, llOpFact

IF lcRmtToCode = 'V'
  lcRemitFil = 'APVENDOR'
  lcRemitTag = 'VENCODE'
  llOpVen    = gfOpenFile(oAriaApplication.DataDir+'APVENDOR','VENCODE','SH')  &&lower
ELSE
  lcRemitFil = 'SYCFACT'
  lcRemitTag = 'CFACCODE'
  llOpFact   = gfOpenFile(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH')  &&lower
ENDIF  

DO CASE
  CASE lcRmtToCode $ 'VF'
    IF SEEK(lcKeyCode, lcRemitFil) 
      &lcRemitCmp  = PADR(IIF(lcRmtToCode = 'V',;
                       APVENDOR.cVenComp, SYCFACT.cFacComp), 40)
      &lcRemitAd1 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,1)
      &lcRemitAd2 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,2)
      &lcRemitAd3 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,3)
      &lcRemitAd4 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,4)      
      &lcRemitAd5 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,5)      
    ELSE
      STORE SPACE(40) TO &lcRemitCmp, &lcRemitAd1,;
                          (lcRemitAd2), (lcRemitAd3),;
                          (lcRemitAd4), (lcRemitAd5)              
    ENDIF
    lcRemitStat= 'DISABLE'    
  OTHERWISE
    lcRemitStat= IIF(loFormSet.ActiveMode="V",'DISABLE','ENABLE')
ENDCASE

IF USED('APVENDOR') .AND. llOpVen
  *=gfCloseFile('APVENDOR')
  USE IN APVENDOR
ENDIF
IF USED('SYCFACT') .AND. llOpFact
  *=gfCloseFile('SYCFACT')
  USE IN SYCFACT
ENDIF
*Old: SHOW GET (lcRemitCmp) &lcRemitStat &&Revise 
*Old: SHOW GET (lcRemitAd1) &lcRemitStat &&Revise 
*Old: SHOW GET (lcRemitAd2) &lcRemitStat &&Revise 
*Old: SHOW GET (lcRemitAd3) &lcRemitStat &&Revise 
*Old: SHOW GET (lcRemitAd4) &lcRemitStat &&Revise 
*Old: SHOW GET (lcRemitAd5) &lcRemitStat &&Revise 
IF type('_Screen.ActiveForm.txtOutcomp,Value')='C'
  WITH _Screen.ActiveForm
    STORE (lcRemitStat='ENABLE') TO .txtOutcomp.Enabled, .txtOutAddr1.Enabled, ;
       .txtOutAddr2.Enabled,  .txtOutAddr3.Enabled
  ENDWITH
ENDIF
RETURN lcRmtToCode

RETURN
*--end of gfRemit 

*!*************************************************************
*! Name      : lfVDtMsg
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 7/26/2004
*! Purpose   : Validation function for date by giving messages.
*!*************************************************************
*! Parameters: The company that you want to validate from its periods.
*!           : Logical parameter to accept the date in a locked period.
*!*************************************************************
*! Returns   : True or False
*!*************************************************************
FUNCTION lfVDtMsg
PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer


PRIVATE llLockStat, lcCompYer, lcCompPrd
PRIVATE lnYear, lcYerPer, lcMessage, lcMessgSel
PRIVATE lcCurYear, lcCurPrd, llVaildDate

lcCompYer  = ' '   && Variable to hlod the current year of the company.
lcCompPrd  = ' '   && Variable to hlod the current period of the company.

lcCurYear  = ' '
lcCurPrd   = ' '

llVaildDate= .F.   && Variable to indicate if the date is valid or not

lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)

llLockPer  = IIF(llLockPer,llLockPer,.F.)

lnYear     = 0     && Variable to hold the year No.

llLockStat = .F.   && Variable to hold the lock stat of the period.
ldDateTVal = IIF(TYPE('ldDateTVal')='O',ldDateTVal.Text1.VALUE,ldDateTVal)

IF lfVlDate(lcCompany,lcCrPrd,lcCrYer,ldDateTVal)  && Call the date validation function.

  lcCrPrd  = lcCurPrd
  lcCrYer  = lcCurYear
  lcYerPer = lcCurPrd+'-'+lcCurYear

  lnYear   = (INT(VAL(lcCurYear))-INT(VAL(lcCompYer))) + 3

  lnYear   = IIF(lnYear <= 0,1,lnYear)

  IF lnYear = 3
    lnYear  = lnYear + SIGN(VAL(lcCrPrd) - VAL(oAriaApplication.CurrentPeriod))
  ENDIF  && End Period Validation

  DO CASE
    CASE llLockStat

      =gfModalGen("TRM00134B00000","DIALOG",lcYerPer)
      llVaildDate = .F.

    CASE lnYear > 0

      lcMessage  = 'history prior   current  future  '

      lcMessgSel = ALLTRIM(SUBSTR(lcMessage,(lnYear*8)-7,8))

      IF lnYear <> 3
        =gfModalGen("TRM00136B00000","DIALOG",lcMessgSel+"|"+lcYerPer)
      ENDIF

      IF lnYear = 1   && If the year = 1 we are not going to accept.
        llVaildDate = .F.
      ELSE
        llVaildDate = .T.
      ENDIF
  ENDCASE
ELSE
  =gfModalGen("TRM00133B00000","DIALOG")

  llVaildDate = .F.
ENDIF
RETURN llVaildDate
*--end of lfVDtMsg

*!*************************************************************
*! Name      : lfVlDate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/24/2012
*! Purpose   : To Validate dates from the periods file.
*!*************************************************************
*! Parameters: The company that you want to validate from its periods.
*!           : Logical parameter to accept the date in a locked period.
*!*************************************************************
*! Returns   : True or False
*!*************************************************************
FUNCTION lfVlDate
PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer
LOCAL ap1

PRIVATE llOpenPrd, lcSavOrdPr, llValidDate, lcExactStat

IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
  PRIVATE lcCurYear, lcCurPrd, lLockStat

  lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

  lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)
ENDIF

llLockPer  = IIF(llLockPer,llLockPer,.F.)

llValidDate= .F.      && Variable to return with from the function.

lcCurYear  = ' '      && Variable to hold the current year.
lcCurPrd   = ' '      && Varibale to hold the current period.
lcExactStat= ' '      && Variable to hold the SET EXACT status.
lLockStat  = .F.      && Variable to hold the lock stat of the record.

lcSavSelct = ALIAS()  && Variable to save the currently selected file.

llOpenPrd  = .F.      && Variable to indicate that the there is a file;
                        && OPEN BY the FUNCTION.

ap1 = CREATEOBJECT("ap")

*PRIVATE llOpAPS, llOpcomp

*llOpcomp = gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','Ccomp_id','SH')  &&lower
*llOpAPS  = gfOpenFile(oAriaApplication.DataDir+'APSETUP','','SH')  &&lower

lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

lcCompYer  = oAriaApplication.CurrentYear
lcCompPrd  = oAriaApplication.CurrentPeriod



lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)


lcCompYer  = oAriaApplication.CurrentYear
lcCompPrd  = oAriaApplication.CurrentPeriod

IF !USED('FSPRD')   && Check if the period file is open or not.
  llOpenPrd = .T.      && Indicates that the file is open by the function.
  SELECT 0             && Select an empty work area.
  lcdatadir = ap1.lcDataDir
  =gfOpenTable('fsprd','COMFYRPRDI','SH')
    
ELSE
  SELECT FSPRD      && The file is open so we are going to select
    
  lcSavOrdPr = ORDER() && Save the file order
  =gfSetOrder('COMFYRPRDI')
ENDIF

IF TYPE('ldDateTVal') <> 'D'
  ldDate = IIF(TYPE('_screen.ActiveForm.ActiveControl.Value')='D',_SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE,{})
ELSE
  ldDate = ldDateTVal
ENDIF

lcExactStat = SET('EXACT')
SET EXACT OFF
GO TOP

SET EXACT &lcExactStat


LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
IF FOUND() .AND. BETWEEN(ldDate,ap1.ldPyBgDate,ap1.ldNyEnDate)

  llLockStat = lFspLocks  && Assign the variable with the period lock stat.
  lcCurYear  = cFisFYear  && Assign the variable with fiscal year of the period.
  lcCurPrd   = cFspprdid  && Assign the variable with the period no.
  lcCrYer    = cFisFYear  && Assign the variable with fiscal year of the period.
  lcCrPrd    = cFspprdid  && Assign the variable with the period no.
  llLockPer  = lFspLocks  && Assign the variable with the period lock stat.
  llValidDate= .T.        && Assign the variable with .T.

  IF USED('FSPRD') .AND. llOpenPrd
    llOpenPrd = .F.
    =gfCloseTable('FSPRD')
  ELSE
    SELECT FSPRD
    =gfSetOrder(lcSavOrdPr)
  ENDIF

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF
ELSE
  IF USED('FSPRD') .AND. llOpenPrd
    llOpenPrd = .F.
    =gfCloseTable('FSPRD')
  ELSE
    SELECT FSPRD
    =gfSetOrder(lcSavOrdPr)
  ENDIF

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF
ENDIF

RETURN llValidDate
*--end of lfVlDate



************************************************************
*! Name      : lfvFactor
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/07/2012
*! Purpose   : Valid fucntion for Factor
************************************************************
FUNCTION lfvFactor
PARAMETERS loFormSet,loFld

=gfOpenTable(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH')

DIMENSION laCompAdr[4]
llBrowse = loFld.Selectedfrombrowse 
loFactor = loFormset.Ariaform1.kbFacCode.Keytextbox
IF llBrowse .OR. !EMPTY(loFactor.Value)
  IF lfGetFac(loFactor.OldValue, llBrowse)
    lcRem1      = SYCFACT.cFacComp
    laCompAdr = ""
    =gfGetAdr('SYCFACT',"","","",@laCompAdr)
    WITH loFormSet.Ariaform1
      .txtOutComp.Value = SYCFACT.cFacComp
      .txtOutAddr1.Value = laCompAdr[1]
      .txtOutAddr2.Value = laCompAdr[2]
      .txtOutAddr3.Value = laCompAdr[3]
      loFormSet.lcRem5   = laCompAdr[4]
    ENDWITH
  ENDIF 
ELSE
  WITH loFormSet.Ariaform1
    .txtOutComp.Value = ''
    .txtOutAddr1.Value = ''
    .txtOutAddr2.Value = ''
    .txtOutAddr3.Value = ''
  ENDWITH
ENDIF   
*- End of lfvFactor.

************************************************************
*! Name      : lfGetFac
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/07/2012
*! Purpose   : Global Function used by the validation function of the Factor Control.
*************************************************************!*************************************************************
FUNCTION lfGetFac
PARAMETERS lcOldVal, llBrowse
*** Old factor value
*** .T. if browsed from browsing invisible button
PRIVATE lcPrFactor, loFactObj, lcCurAlias, lnSavFacTg, lnClosRec,;
        llFactFound, llOpenFact, lcFile_Ttl, lcBrFields 

loFactObj    = IIF(TYPE('_Screen.ActiveForm.ActiveControl.value')='C', ;
                   _Screen.ActiveForm.ActiveControl, ;
                   _Screen.ActiveForm.ActiveControl.Parent.KeyTextBox)
lcPrFactor     = ALLTRIM(loFactObj.Value)

llFactFound  = .F.

IF llBrowse .OR. !EMPTY(lcPrFactor) 

  lcCurAlias = ALIAS()

  IF !USED('SYCFACT')  && Check if the factors file is open or not.
    llOpenFact  = .T.     && Indicates that the file is open by the function.
    ** Use the file and assign the index.
    SELECT 0
    *USE &oAriaApplication.SysPath.SYCFACT ORDER TAG cFacCode  &&lower
    gfOpenFile(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH')
  ELSE
    llOpenFact = .F.
    SELECT SYCFACT
    lnSavFacTg = VAL(SYS(21)) 
    SET ORDER TO TAG cFacCode 
  ENDIF  

  lcPrFactor   = PADR(ALLTRIM(lcPrFactor), FSIZE('cFacCode','SYCFACT'))
  *Old: &lcFactObj = lcPrFactor
  loFactObj.Value = lcPrFactor
  *Old: SHOW GET (lcFactObj) &&Revise 

  IF llBrowse .OR. !SEEK(lcPrFactor,'SYCFACT')
    *** If a record is to be selected 
    DIMENSION laTemp[1]
    laTemp = ''
    lcFile_Ttl = 'Factors'
    lcBrFields = ' '
    lnClosRec = RECNO(0)
    
    *** Get browse fields from dictionary
     =GFGETBRF(@lcFile_Ttl, @lcBrFields, 'SYCFACT',;  &&lower
               "cFacCode,cFaccomp,cPhoneNo,cFaxNo,cFacTitle,cFacCont")
    IF BETWEEN(lnClosRec,1,RECCOUNT())
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF        

    =gfBrows(.F.,'cFacCode','laTemp')
    *** If a factor has been selected from the browse
    IF !EMPTY(laTemp[1])
      *Old: &lcFactObj  = laTemp[1]
      loFactObj.Value = laTemp[1]
      llFactFound = .T.      
    ELSE
      *Old: &lcFactObj  = lcOldVal
      loFactObj.Value = loFactObj.OldValue
      llFactFound = .F.       
    ENDIF
    *Old: SHOW GET (lcFactObj) &&Revise 
  ELSE
    llFactFound = .T.
  ENDIF  

  IF USED('SYCFACT')
    IF llOpenFact
      USE IN SYCFACT
    ELSE
      SET ORDER TO lnSavFacTg IN SYCFACT
    ENDIF  
  ENDIF

  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF
RETURN llFactFound
*--end of lfGetFac 

********************************************************************************
*Name       : lfAdd_Info
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : user information
********************************************************************************
FUNCTION lfAdd_Info
PARAMETERS llAdd,lcAlias
LOCAL lnSlct
lnSlct = SELECT(0)
lcAlias = IIF(EMPTY(lcAlias),ALIAS(),lcAlias)
SELECT (lcAlias)

IF llAdd
  IF TYPE('&lcAlias..cAdd_User') = 'C'
    REPLACE cAdd_User  WITH oAriaApplication.User_ID ,;
            dAdd_Date  WITH DATE()    ,;
            cAdd_Time  WITH gfGetTime(),;
            cAdd_Ver   WITH oAriaApplication.cShortVersion
  ENDIF 
ELSE && Modified Record
  IF TYPE('&lcAlias..cEdit_User') = 'C'
    REPLACE cEdit_User  WITH oAriaApplication.User_ID ,;
            dEdit_Date  WITH DATE()    ,;
            cEdit_Time  WITH gfGetTime(),;
            cEdt_Ver    WITH oAriaApplication.cShortVersion
  ENDIF             
ENDIF   

IF TYPE('CLOK_USER')='C'
  Replace LLOK_STAT WITH .F. ;
          CLOK_USER WITH '' ;
          DLOK_DATE WITH {} ;
          CLOK_TIME WITH ''
ENDIF          
SELECT (lnSlct)
*- End of lfAdd_Info

*!**************************************************************************
*!
*!      Function: lfBnkChk
*!
*!**************************************************************************
* Valid function for get field laData[43] representing factor
*E300296,4 REN 10/24/95 Add a parameter to include a condition 
*E300296,4              for selecting a bank/checking account with 
*E300296,4              a specific currency.
*B600761,1 REN 10/30/95 Fix the problem of bank/checking account mismatch.
*B800456,1 M.H 02/11/96 Add a new parameter to hold the browse title.

FUNCTION lfBnkChk
*E300824,1 AMM start, Add a new parameter llVldBnk holds the return value.
*PARAMETERS laObjArr, lcOldVal, llBrowse, lcChkState,;
           lcBankAlias, lcCheckAlias, lcCurrCode
PARAMETERS laObjArr, lcOldVal, llBrowse, lcChkState,;
           lcBankAlias, lcCheckAlias, lcCurrCode, llVldBnk
*E300824,1 AMM end
*E300296,4 Add parameter lcCurrCode : currency code to browse for.

*** laObjArr   : pointer to an array holding the bank's object names
*** lcOldVal   : old value of the field
*** llBrowse   : .T. if called from a browsing invisible button
*** lcChkState  : pointer to a variable, returns object display status
***               for bank's related objects. (optional)

*E300824,1 AMM start, Exclude the variable llVldBnk from being private.
*PRIVATE lnCallObj, lnSavTag, laRetVal, llVldBnk, llChgChk, lcCurFltr 
PRIVATE lnCallObj, lnSavTag, laRetVal, llChgChk, lcCurFltr 
*E300824,1 AMM end
*** lnCallObj  : calling object, whether a bank or a check
***             (1 if bank, 2 if checking accounts)
*** laRetVal   : array to hold return values from lfGetExt function
*E300296,4 lcCurFltr : current filter expression

llVldBnk = .T.
*** Get the calling object from the array
lcBankAlias  = IIF(!EMPTY(lcBankAlias) , lcBankAlias ,'APBANKS' )
lcCheckAlias = IIF(!EMPTY(lcCheckAlias), lcCheckAlias,'APCHECKS')

IF TYPE('laObjArr') = 'C'
  FOR lnCallObj = 1 TO ALEN(laObjArr,1)
    laObjArr[lnCallObj, 2] = UPPER(STRTRAN(STRTRAN(laObjArr[lnCallObj, 2],;
                               "[","("),"]",")"))
  ENDFOR 
  lnCallObj = ASCAN(laObjArr, SYS(18))
  lcOldVal  = IIF(TYPE('lcOldVal') <> 'C', " ", lcOldVal)
ELSE
  lnCallObj = 0
ENDIF
IF lnCallObj > 0  .AND. USED(lcBankAlias) .AND. USED(lcCheckAlias) ;
  .AND. (llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal)
  lnCallObj  = ASUBSCRIPT(laObjArr, lnCallObj, 1)
  DECLARE laRetVal[2]
  laRetVal   = " "
  
  lcCurAlias = ALIAS()

  llChgChk   = .T.     && Checking account is assumed to change
                       && It also reflects if a bank is changed or not
                       && if called from the first case (banks)
  
  *E300296,4 Check passed parameter lcCurrCode
  SELECT (lcCheckAlias)
  lcCurFltr    = FILTER()
  IF !EMPTY(lcCurrCode)
    SET FILTER TO cCurrCode = lcCurrCode
  ELSE
    SET FILTER TO 
  ENDIF  
  *E300296,4 end.
  
  DO CASE
    *** Case called from a bank
    CASE lnCallObj = 1
      *** If a valid bank entry is selected, 
      *E300296,4 Check if a passed bank code has corresponding
      *E300296,4 entries in the filtered CHECKS file (lcCheckAlias).
      *E300296,4 If not, return old values.
      *IF lfGetExt(lcBankAlias, 'BANKCODE', lcOldVal, llBrowse,;
         'cBnkCode', @laRetVal, 'cBnkCode,cBnkLnDes') .AND.;
         SEEK(laRetVal[1], lcCheckAlias)
         
*B800456,1 M.H 02/11/96 Begin.
*      IF lfGetExt(lcBankAlias, 'BANKCODE', lcOldVal, llBrowse,;
        'cBnkCode', @laRetVal, 'cBnkCode,cBnkLnDes') 
      IF lfGetExt(lcBankAlias, 'BANKCODE', lcOldVal, llBrowse,;
        'cBnkCode', @laRetVal, 'cBnkCode,cBnkLnDes','', '', '', 'Banks') 
*B800456,1 M.H 02/11/96 End.
*B600761,1 Position the record pointer on the returned checking account.
        IF SEEK(laRetVal[1], lcCheckAlias)
          &laObjArr[1,2] = laRetVal[1]        && cBnkCode
          *B600761,1 Use parameter check alias instead of APCHECKS
          *&laObjArr[2,2] = APCHECKS.cChkAcct          
          &laObjArr[2,2] = &lcCheckAlias..cChkAcct          
          *B600761,1 end.
        ELSE
          =gfModalGen('INM04155B00000','DIALOG',;
                      ALLTRIM(lcCurrCode) + '|'+;
                      ALLTRIM(&lcBankAlias..cBnkCode)+'-'+ALLTRIM(&lcBankAlias..cBnkShDes))

          *E300296,4 If not, if the current object is not empty, return its
          *E300296,4 old value whatever it is (whether empty or another valid 
          *E300296,4 entry)
          IF !EMPTY(EVALUATE(laObjArr[1,2]))
            &laObjArr[1,2] = lcOldVal
            llChgChk       = .F.
          ELSE
            *E300296,4 Clear objects before display
            &laObjArr[1,2]  = SPACE(FSIZE('cBnkCode', lcBankAlias))
            &laObjArr[2,2]  = SPACE(FSIZE('cChkAcct', lcCheckAlias))
          ENDIF  
        ENDIF 
        *E300296,4 end. 
      ELSE    
        *** If not, if the current object is not empty, return its
        *** old value whatever it is (whether empty or another valid 
        *** entry)
        IF !EMPTY(EVALUATE(laObjArr[1,2]))
          &laObjArr[1,2] = lcOldVal
          llChgChk       = .F.
        ELSE
          *** Clear objects before display
          &laObjArr[1,2]  = SPACE(FSIZE('cBnkCode', lcBankAlias))
          &laObjArr[2,2]  = SPACE(FSIZE('cChkAcct', lcCheckAlias))
        ENDIF  
      ENDIF
      
    *** Case called from a checking account
    CASE lnCallObj = 2
      lcBnkCode  = laObjArr[1,2]
*B800456,1 M.H 02/11/96 Begin.
*      IF lfGetExt(lcCheckAlias, 'BANKCHECK', lcOldVal, llBrowse,;
         'cChkAcct', @laRetVal, 'cBnkCode,cChkAcct,cChkShDes,cChkGlAcc',;
          &lcBnkCode + ALLTRIM(EVALUATE(SYS(18))), lcBnkCode)  
      IF lfGetExt(lcCheckAlias, 'BANKCHECK', lcOldVal, llBrowse,;
         'cChkAcct', @laRetVal, 'cBnkCode,cChkAcct,cChkShDes,cChkGlAcc',;
          &lcBnkCode + ALLTRIM(EVALUATE(SYS(18))), lcBnkCode, '', 'Checking Accounts')
*B800456,1 M.H 02/11/96 End.
        llChgChk   = .T.
        *B600761,1 Use parameter check alias instead of APCHECKS
        *&laObjArr[2,2] = APCHECKS.cChkAcct
        &laObjArr[2,2] = &lcCheckAlias..cChkAcct          
        *B600761,1 end.
      ELSE     
        llChgChk   = .F.
        &laObjArr[2,2] = lcOldVal
      ENDIF
  ENDCASE

  llVldBnk   = !EMPTY(EVALUATE(SYS(18)))   
  lcChkState = IIF(llVldBnk, 'ENABLE', 'DISABLE')
  SHOW GET (laObjArr[1,2]) 
  SHOW GET (laObjArr[2,1]) &lcChkState 
  SHOW GET (laObjArr[2,2]) &lcChkState 

  IF llChgChk
    *** Bank description 
    IF !EMPTY(laObjArr[1,3]) .AND. lnCallObj = 1 
      *B600761,1 Use parameter bank alias instead of APBANKS
      *&laObjArr[1,3]  = IIF(llVldBnk, APBANKS.cBnkShDes, "")
      &laObjArr[1,3]  = IIF(llVldBnk, &lcBankAlias..cBnkShDes, "")
      *B600761,1 end.
      SHOW GET (laObjArr[1,3]) 
    ENDIF  
    *** Checking account description
    IF !EMPTY(laObjArr[2,3]) 
      *B600761,1 Use parameter check alias instead of APCHECKS
      *&laObjArr[2,3]  = IIF(llVldBnk, APCHECKS.cChkShDes, "")      
      &laObjArr[2,3]  = IIF(llVldBnk, &lcCheckAlias..cChkShDes, "")      
      *B600761,1 end.
      SHOW GET (laObjArr[2,3]) 
    ENDIF  
  
    *** If a G/L account entry exists 
    IF ALEN(laObjArr,1) > 2 
      *B600761,1 Use parameter check alias instead of APCHECKS
      *&laObjArr[3,2] = IIF(llVldBnk .AND. !EMPTY(APCHECKS.cChkGlAcc),;
                           APCHECKS.cChkGlAcc, lcEmptyAcc)
      &laObjArr[3,2] = IIF(llVldBnk .AND. !EMPTY(&lcCheckAlias..cChkGlAcc),;
                           &lcCheckAlias..cChkGlAcc, lcEmptyAcc)                           
      *B600761,1 end.                      
      *** G/L account description
      IF !EMPTY(laObjArr[3,3])
        &laObjArr[3,3]  = IIF(llVldBnk .AND. llApGlLink,;
                        ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,&laObjArr[3,2] ,;
                        lcLinkChar.cAcctCode,"ACCTCODE"))," ")
        SHOW GET (laObjArr[3,3]) 
      ENDIF  
      SHOW GET (laObjArr[3,1]) &lcChkState 
      SHOW GET (laObjArr[3,2]) &lcChkState   
    ENDIF  
  ENDIF 

  *E300296,4 Restore filter in (lcCheckAlias)
  SELECT (lcCheckAlias)
  SET FILTER TO &lcCurFltr
  *E300296,4 end.

  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  
  *** If bank is empty (invalid), return to the same object
  IF !llVldBnk
    _CUROBJ = _CUROBJ
    KEYBOARD "{HOME}"
  ENDIF  
ENDIF  
llBrowse = .F.
RETURN llVldBnk
  