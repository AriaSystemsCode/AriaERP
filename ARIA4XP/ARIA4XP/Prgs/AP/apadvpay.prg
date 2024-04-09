 ***********************************************************************
*:  Program File: APADVPAY.PRG
*:  Desc.       : Advanced Payment program
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/01/2012
*:  Reference   : *E303015,1 
*:************************************************************************
*:B610124,1 SAB 17/10/2012 Fix error when changing the currency [T20121017.00001]
*:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002]
*:B610318,1 HES 04/28/2013 Fix bug of not updating the GL account when change Bank Code [T20130416.0021]
*:B610334,1 SAB 05/22/2013 Fix error when opening the Advanced Manual Check Payment [T20130514.0034]

** Manual checks, Non check payments, Cash payments. ***
PARAMETER lnPyChMN         && Refere where the program is called from 
&& apply the calling from client folder 

lcRunScx = lfGetScx("AP\APADVPA.SCX")
DO FORM (lcRunScx) WITH lnPyChMN

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF   
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303015,1 TMI 02/02/2012 
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
      
SET MULTILOCKS ON
*- Open tables 
=gfOpenTable('APPAYMNT','TYPMETHNO','SH')
=gfOpenTable('APVENDOR','VENCODE','SH')
=gfOpenTable('APINVHDR','INVVEND','SH')
=gfOpenTable('APCHECKS','BANKCHECK','SH')
=gfOpenTable('SYCINT','CCONTCODE','SH')
=gfOpenTable('APDIV','DIVISION','SH')
=gfOpenTable('CODES','IDRLTFNAME','SH')
=gfOpenTable('APBANKS','BANKCODE','SH')
=gfOpenTable('SYCCURR','CCURRCODE','SH')
=gfOpenTable('APDIST','','SH')
=gfOpenTable('APVENHST','VENDYEAR','SH')
=gfOpenTable('APSETUP','APSETUP','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH')

*- Define variables 
=lfDefineVars(loFormSet)

*- initializations
WITH loFormSet
  .nWorkArea = 'APPAYMNT'
  .otoolbar.nWorkArea = 'APPAYMNT'
  .DataEnvironment.InitialSelectedAlias = 'APPAYMNT'
ENDWITH 
=lfAddProp(loFormSet,'lcSequence',gfSequence('CAPSESSNO'))
loFormSet.Ariaform1.Caption = ALLTRIM(loFormSet.Ariaform1.Caption)+'      Session: '+loFormSet.lcSequence

loFormSet.Ariaform1.lcTAcct.Caption = IIF(loFormSet.lnPyChMN = 3,"Cash acct.","G/L acct.")

*-
lcVars = "llMultiCr, llEditEx, llAddRate"
=lfAddProp(loFormSet,lcVars,.F.)

=lfAddProp(loFormSet,'lcBaseSmbl,lcTExRateMsg','')

WITH loFormSet
  .lcTExRateMsg = 'The exchange rate|zero'
  IF gfGetMemVar('LLMULCURR')
    .llMultiCr  = .T.
    .llEditEx   = gfGetMemVar('LLEDITEXRA')
    .llAddRate  = gfGetMemVar('LLEXCHRATE')
    .lcBaseSmbl = IIF(SEEK(oAriaApplication.BaseCurrency, 'SYCCURR'),;
                    ALLTRIM(SYCCURR.cCurrSmbl), '')
  ENDIF
ENDWITH 

** Get Codes from codes file.
SELECT CODES
SET FILTER TO (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N'+'CDIVISION') OR;
              (cDefCode+CRLTFIELD+CFLD_NAME ='N'+'N'+'N/A')

STORE ' ' TO lcVendCode, lcVPhone, lcVendComp, lcBankCode, lcCheckCode, lcGlAcct, lcAPAcct, lcDebMemN, lcRef

*- some Settings
WITH loformSet.Ariaform1
  .BankChk.Visible = loFormSet.lnPyChMN <> 3
  .lcTAcct.Caption  =  IIF(loFormSet.lnPyChMN = 3,'Cash Acct.',.lcTAcct.Caption)
 * .lcDebMemN.InputMask = REPLICATE('!',FSIZE('CINVNO','APINVHDR'))
ENDWITH   



*- Go to Select mode
loFormSet.ChangeMode('S')

*- End of lfFormInit.

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
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR 
 *- End of lfAddProp.
 
************************************************************
*! Name      :  lfvVendor
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/05/2012
*! Purpose   : Valid function for Vendor Code
************************************************************ 
FUNCTION  lfvVendor
PARAMETERS loFormSet,loFld,lcType
LOCAL lcVendCode ,lcVPhone   ,lcVendComp 

IF !EMPTY(loFld.KeyTextBox.OldValue) AND EMPTY(loFld.KeyTextBox.Value)
  loFld.KeyTextBox.Value = loFld.KeyTextBox.OldValue
  RETURN .F.
ENDIF 

WITH loFormSet.AriaForm1
lcVendCode = .KBVendCode.KeyTextBox.VALUE 
lcVPhone   = .KBVendPhone.KeyTextBox.VALUE 
lcVendComp = .KBVendCompany.KeyTextBox.VALUE
ENDWITH 

IF lfVndBrw(loFormSet,loFld,lcType)
  WITH loFormSet.AriaForm1
  IF APVENDOR.CVENPRIOR = '0'
    ** MESSAGE: " Vendor XXXXXX has payment priority 0."
    **          " This vendor in on hold.              " 
    ** Choices: "                ® OK ¯                " 
    =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(lcVendCode))
    .KBVendCode.KeyTextBox.VALUE    = lcVendCode 
    .KBVendPhone.KeyTextBox.VALUE   = lcVPhone   
    .KBVendCompany.KeyTextBox.VALUE = lcVendComp 
    RETURN .F.
  ENDIF 
  
  .cboDivision.Value = APVENDOR.CDIVISION
  .cboDivision.Refresh()

  =lfDefBank(loFormset)
  =lfObjDis(loFormset,.F.)
  =lfvRemit(loFormset,.T.)

  *- Default by the vendor currency if its a cash payment.
   IF loFormSet.llMultiCr
    IF loFormSet.lnPyChMN = 3
      .kbCurrCode.Keytextbox.Value = ;
            IIF(EMPTY(APVENDOR.CCURRCODE),oAriaApplication.BaseCurrency,APVENDOR.CCURRCODE)
    ENDIF  
    
    
    lnCurrUnit = loFormSet.lnCurrUnit
    lcCurrCode = .kbCurrCode.Keytextbox.Value
    ldPayDat = .ldPayDat.Value
    
    lcExSin2 = ' ' 
    lcExSin1 = gfGetExSin(@lcExSin2,.kbCurrCode.Keytextbox.Value)
    lnCurrRate  = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                  .T.,oAriaApplication.ActiveCompanyID, oAriaApplication.BaseCurrency, .T.)
    IF lnCurrRate = 0 .AND. !loFormSet.llAddRate
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found for ð.                        "
      **           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",;
                  ALLTRIM(lcCurrCode)+'|'+ALLTRIM(oAriaApplication.BaseCurrency)+'|'+DTOC(ldPayDat))
    ENDIF
    IF lcCurrCode = oAriaApplication.BaseCurrency .OR. !loFormSet.llEditEx
      .lnCurrRate.Enabled = .F.
    ELSE 
      .lnCurrRate.Enabled = .T.
    ENDIF

    loFormSet.lnCurrUnit = lnCurrUnit 
    .kbCurrCode.Keytextbox.Value = lcCurrCode 
    .ldPayDat.Value = ldPayDat 
    
    loFormset.lcExSin2 = lcExSin2 
    loFormset.lcExSin1 = lcExSin1 
    .lnCurrRate.Value = lnCurrRate 
    
  ENDIF
  ENDWITH &&loFormSet.AriaForm1
  RETURN .T.
ELSE 

  RETURN .F.    
ENDIF 
 *- End of  lfvVendor.
 
 
************************************************************
*! Name      : lfvRemit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/07/2012
*! Purpose   : Remit to 
************************************************************ 
FUNCTION lfvRemit
PARAMETERS loFormSet,llJustShow
 
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

SELECT APPAYMNT
*- End of lfvRemit.


*!*************************************************************
*! Name      : lfvRemit
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 7/29/2004
*! Purpose   : To Validate The RemitTo ComboBox.
*!*************************************************************
*! Parameters: loFormSet, llJustShow
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION X_lfvRemit
PARAMETERS loFormSet, llJustShow
PRIVATE lcRemitStat, lcRemitTo

IF !INLIST(loFormSet.ActiveMode,'A','E')
  RETURN
ENDIF


*** Default factor for the vendor
loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value = ;
   IIF(EMPTY(loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value), APVENDOR.cFacCode, ;
       loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value)
       
       
DIMENSION laAry[6]
laAry[1]=loFormSet.AriaForm1.txtOutComp.Value
laAry[2]=apinvhdr.coutaddr1
laAry[3]=apinvhdr.coutaddr2
laAry[4]=apinvhdr.coutaddr3
laAry[5]=apinvhdr.coutaddr4
laAry[6]=apinvhdr.coutaddr5
lcRemitStat = loFormSet.lcRemitStat
lcRemitTo = loFormSet.lcRemitTo
DIMENSION laRemitTo[ALEN(loFormSet.laRemitTo,1),ALEN(loFormSet.laRemitTo,2)]
=ACOPY(loFormSet.laRemitTo,laRemitTo)

loFormSet.AriaForm1.cboInvRemit.Value  = ;
  gfRemit(loFormSet.AriaForm1.cboInvRemit.Value, !llJustShow, ;
          loFormSet.AriaForm1.KBVendCode.KeyTextBox.Value, ;
          loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value, @lcRemitStat, ;
          'laAry[1]','laAry[2]','laAry[3]','laAry[4]','laAry[5]','laAry[6]',; 
          3, 40, 'laRemitTo', @lcRemitTo, loFormSet.lnRemitLen)
loFormSet.AriaForm1.txtOutComp.Value = laAry[1]
REPLACE apinvhdr.coutaddr1 WITH laAry[2] IN apinvhdr
REPLACE apinvhdr.coutaddr2 WITH laAry[3] IN apinvhdr
REPLACE apinvhdr.coutaddr3 WITH laAry[4] IN apinvhdr
REPLACE apinvhdr.coutaddr4 WITH laAry[5] IN apinvhdr
REPLACE apinvhdr.coutaddr5 WITH laAry[6] IN apinvhdr
loFormSet.lcRemitStat = lcRemitStat
loFormSet.lcRemitTo = lcRemitTo
=ACOPY(laRemitTo,loFormSet.laRemitTo)

              
loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value = ;
  IIF(loFormSet.AriaForm1.cboInvRemit.Value = 'F', ;
      loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value, SPACE(6))

*loFormSet.lnOldRemit = puRemitTo


*** If a vendor code exists, 
DO CASE
  CASE loFormSet.AriaForm1.cboInvRemit.Value = 'F'
    loFormSet.lcFactStat   = IIF(!loFormSet.ActiveMode="V", 'ENABLE', 'DISABLE')  &&lower
    
  OTHERWISE
    loFormSet.lcFactStat   = 'DISABLE'     
ENDCASE
*Old: =lfShiftlaData(loFormSet.lnStartAdr)
*Old: SHOW GET loFormSet.laAddrs[1] &loFormSet.lcRemitStat &&Revise 
*Old: SHOW GET loFormSet.laAddrs[2] &loFormSet.lcRemitStat &&Revise 
*Old: SHOW GET loFormSet.laAddrs[3] &loFormSet.lcRemitStat &&Revise 
STORE (loFormSet.lcRemitStat='ENABLE') TO loFormSet.AriaForm1.txtOutComp.Enabled, ;
  loFormSet.AriaForm1.txtOutAddr1.Enabled, loFormSet.AriaForm1.txtOutAddr2.Enabled, ;
  loFormSet.AriaForm1.txtOutAddr3.Enabled

*Old: SHOW GET loFormSet.AriaForm1.kbFacCode.KeyTextBox.Value &loFormSet.lcFactStat  &&lower &&Revise 
*Old: SHOW GET ibFactor   &loFormSet.lcFactStat &&Revise 
loFormSet.AriaForm1.kbFacCode.Enabled=(loFormSet.lcFactStat='ENABLE')

*=lfRefresh()

SELECT APINVHDR
*B128399,1 KHM 06/15/2005 Return 0 to stay in the popup [Begin]
*RETURN 1
RETURN 0


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

****************************************************************************************
*Name      : lfDefineVars
*Developer : TMI - TAREK MOHAMED IBRAHIM
*Date      : 02/05/2012
*Purpose   : Define all variables to be used in the screen
***************************************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset

=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))   && Year,period variables

lcVars = 'lcSesNum  , lcVendCode , lcOVndVal , lcOVndcod ,'+;
         'lcVendComp, lcOVndcmp  , lcVPhone  , lcOVPhone ,'+;
         'lcVendDiv , lcDivision , lcObjDisp , lcDivCode ,'+;
         'lcCInvRef , lcOVendCod , lcOldVnCmp, lcOldDivDs,'+;
         'lcOldDvCod, lcOldVal   , lcOldAcc  , lcDebMemN ,'+;
         'lcODebMemN, lcTinv     , lcRem1    , lcRem2    ,'+;
         'lcRem3    , lcRem4     , lcRem5    , lcPeriod  , lcFisPrd  ,'+;
         'lcYear    , lcFisYer   , lcRef     , lcTDebMem ,'+;
         'lcTRef    , lcTsession , lcPhone   , lcCompany ,'+;
         'lcRemitTo , lcInvRemit , lcFactor  , lcTAcct   ,'+;
         'puDiv     , puDivision , lcOldCurr , lcCurrCode,'+;
         'lcExSin1  , lcExSin2'
=lfAddProp(loFormSet,lcVars,' ')   

=lfAddProp(loFormSet,'laCompAdr[4]',' ')   
 
lcVars = 'lcFactStat, lcRemitStat, lcDataStat, laCtrStat ,lcObjDisp , lcRateDisp'
=lfAddProp(loFormSet,lcVars,'DISABLE')

=lfAddProp(loFormSet,'lcGlAcct, lcAPAcct',loFormSet.ap1.lcEmptyAcc)

=lfAddProp(loFormSet,'lcInvRef',SPACE(15))
=lfAddProp(loFormSet,'lcCheckcode',SPACE(12))
=lfAddProp(loFormSet,'lcBankCode',SPACE(8))

lcVars = 'ln1099Amnt, lnO1099Amnt, lnPayAmnt , lnOPayAmnt'
=lfAddProp(loFormSet,lcVars,0.00)

lcVars = 'lnChkNum  , lnOChkNum  , lnGnChkNm , lnRemitLen,'+;
         'lnCurrRate, lnCurrUnit , lnOldRate'
=lfAddProp(loFormSet,lcVars,0)
=lfAddProp(loFormSet,'llPay',.F.)
=lfAddProp(loFormSet,'ldPayDat',oAriaApplication.systemdate)

=lfAddProp(loFormSet,'laRemitTo[1]','')
DIMENSION loFormSet.laRemitTo[3,2]
DIMENSION laRemitTo[3,2]
lnRemitLen = gfGetVld('cInvRemit',@laRemitTo)
ACOPY(laRemitTo,loFormSet.laRemitTo)

*- End of lfDefineVars.


************************************************************
*! Name      : lfvPayDat
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/05/2012
*! Purpose   : Payment date validation
************************************************************
FUNCTION lfvPayDat
PARAMETERS loFormset,loFld,llForce
IF loFld.Value <> loFld.OldValue OR llForce
  *- If the payment date is empty, do not validate and
  *- clear the exchange rate
  IF EMPTY(loFld.Value)
    loFormset.Ariaform1.lnCurrRate.Value = 0
    loFormset.Ariaform1.lnCurrRate.Enabled = .F.
  ELSE
    *- Get the year and period from apsetup. 
    STORE ' ' TO lcFisPrd,lcFisYer
    IF lfVDtMsg(oAriaApplication.PrntCompanyID,@lcFisPrd,@lcFisYer,loFld.Value)
      loFormSet.lcPeriod = lcFisPrd
      loFormSet.lcYear   = lcFisYer
      IF loFormSet.llMultiCr
        lnCurrUnit = loFormSet.lnCurrUnit
        lcCurrCode = loFormset.Ariaform1.kbCurrCode.Keytextbox.Value
        ldPayDat   = loFormset.Ariaform1.ldPayDat.Value
        
        lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                     .T.,oAriaApplication.ActiveCompanyID, oAriaApplication.BaseCurrency, .T.)
        *- Got the equation signs. (Begin)
        *- Passed pointer parameter to get Unit sgin. (Begin)
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
        IF lnCurrRate = 0 .AND. !loFormSet.llAddRate
          ** MESSAGE : " A valid ð to ð exchange rate could not "
          **           " be found for ð.                        "
          **           "                  ® Ok ¯                "
          =gfModalGen("QRM04157B00000","DIALOG",;
                       ALLTRIM(lcCurrCode)+'|'+ALLTRIM(oAriaApplication.BaseCurrency);
                       +'|'+DTOC(ldPayDat))
        ENDIF
        IF lcCurrCode = oAriaApplication.BaseCurrency .OR. !loFormSet.llEditEx
          SHOW GET lnCurrRate DISABLE
        ELSE
          SHOW GET lnCurrRate ENABLE
        ENDIF
        
        loFormSet.lnCurrUnit = lnCurrUnit
        *:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002][Start]
        IF loFormSet.lnCurrRate  = 0 OR !loFormSet.Ariaform1.lnCurrRate.Enabled
        *:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002][End]
          loFormSet.Ariaform1.lnCurrRate.Value = lnCurrRate 
        *:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002][Start]
        ELSE
          lnCurrRate = loFormSet.lnCurrRate
        ENDIF
        *:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002][End]
      ENDIF
    ELSE
      loFld.Value = loFld.OldValue
      *- If the payment date is empty, clear the exchange rate
      ldPayDat   = loFormset.Ariaform1.ldPayDat.Value
      IF EMPTY(ldPayDat)
        loFormset.Ariaform1.lnCurrRate.Value = 0
        loFormset.Ariaform1.lnCurrRate.Enabled = .F.
      ENDIF  
      RETURN .F.
    ENDIF  
  ENDIF
ENDIF

 *- End of lfvPayDat.

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

IF TYPE('&lcAlias..cowner') = 'C'
  replace &lcAlias..cowner WITH ''
ENDIF 

IF TYPE('CLOK_USER')='C'
  Replace LLOK_STAT WITH .F. ;
          CLOK_USER WITH '' ;
          DLOK_DATE WITH {} ;
          CLOK_TIME WITH ''
ENDIF          
SELECT (lnSlct)
*- End of lfAdd_Info

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011 
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
IF TYPE('loFormSet.lcDataStat')='U'
  RETURN 
ENDIF   

DO case
CASE loFormSet.ActiveMode = 'S'
  WITH loFormSet.AriaForm1
  .KBVendCode.Keytextbox.Value = ''
  .KBVendPhone.Keytextbox.Value = ''
  .KBVendCompany.Keytextbox.Value = ''

  .KBVendCode.ENABLED = .T.
  .KBVendPhone.ENABLED = .T.
  .KBVendCompany.ENABLED = .T.

  =lfObjDis(loFormSet,.T.)
  ENDWITH 
ENDCASE 

WITH loFormSet.oToolBar
  *.cmdSelect.Enabled = .F.
  .cmdFind.Enabled = .F.
  .cmdTop.Enabled = .F.
  .cmdPrev.Enabled = .F.
  .cmdNext.Enabled = .F.
  .cmdEnd.Enabled = .F.
  .cmdEdit.Enabled = .F.  
ENDWITH 
*- End of lfChangeMode.

************************************************************
*! Name      : lfObjDis
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/07/2012
*! Purpose   : Enable Disable controls
************************************************************
FUNCTION lfObjDis
PARAMETERS loFormSet,llPayDone

LOCAL lcFactStat, lcRemitStat
STORE ' ' TO lcFactStat, lcRemitStat

WITH loFormSet.Ariaform1
*- Set Default Values
IF llPayDone
  .cboDivision.Value = ''
  .BankChk.kbBanks.Keytextbox.Value = ''
  .BankChk.kbChkAccount.Keytextbox.Value = ''

  .kbCurrCode.Keytextbox.Value = ''
  .lnCurrRate.Value = 000.00

  .GLActCode.Keytextbox.Value = loFormset.ap1.lcEmptyAcc
  .APActCode.Keytextbox.Value = loFormset.ap1.lcEmptyAcc  

  .txtGLActName.Value = ''
  .txtAPActName.Value = ''
  
  .lcDebMemN.Value = ''
  .lcRef.Value = ''
  .lnPayAmnt.Value = 00000000.00
  .ldPayDat.Value = oAriaApplication.systemdate
  .ldPayDat.OldValue = oAriaApplication.systemdate
  .ln1099Amnt.Value = 00000000.00
  
  .cboInvRemit.Value = 'V'  && Vendor
  .txtOutComp.Value = ''
  .txtOutAddr1.Value = ''
  .txtOutAddr2.Value = ''
  .txtOutAddr3.Value = ''
  .kbFacCode.Keytextbox.Value = ''  
  
  loFormSet.lnCurrUnit = 00.00
  
  STORE 'DISABLE' TO lcFactStat, lcRemitStat
ENDIF 

*- Enable / Disable
IF !EMPTY(.KBVendCode.Keytextbox.Value)
  STORE 'ENABLE' TO loFormSet.lcDataStat
  lcFactStat = IIF(.cboInvRemit.Value = 'F', 'ENABLE', 'DISABLE')
  loFormSet.lcObjDisp  = IIF(loFormSet.lnPyChMN = 3, loFormSet.lcDataStat, loFormSet.lcObjDisp)
  
  llEnable = .T.
  .cboDivision.Enabled = llEnable
  .APActCode.Enabled = llEnable
  .GLActCode.Enabled = loFormSet.lcObjDisp = 'E'
  .cboInvRemit.Enabled = loFormSet.lcDataStat = 'E'

  .BankChk.Enabled = llEnable
  .BankChk.kbBanks.Enabled = llEnable
  .BankChk.kbChkAccount.Enabled = loFormSet.lcObjDisp = 'E'
  
  .lcDebMemN.Enabled = llEnable
  .lcRef.Enabled = llEnable
  .lnPayAmnt.Enabled = llEnable
  .ldPayDat.Enabled = llEnable

  IF loFormSet.llMultiCr
    IF loFormSet.lnPyChMN = 3
      .kbCurrCode.Enabled = llEnable
    ENDIF
    IF .kbCurrCode.Keytextbox.Value = oAriaApplication.BaseCurrency .OR. !loFormSet.llEditEx
      .lnCurrRate.Enabled = .F.
    ELSE
      .lnCurrRate.Enabled = .T.
    ENDIF
  ENDIF

  IF !EMPTY(APVENDOR.CVEN1099T)
    .ln1099Amnt.Enabled = .T.
  ELSE
    .ln1099Amnt.Enabled = .F.
  ENDIF  

  .cmdPay.Enabled = .T.
  LOCAL llOther
  llOther = .cboInvRemit.Value = 'O'
  .txtOutComp.Enabled = llOther
  .txtOutAddr1.Enabled = llOther
  .txtOutAddr2.Enabled = llOther
  .txtOutAddr3.Enabled = llOther
  .kbFacCode.Enabled = .cboInvRemit.Value = 'F'
ELSE

  STORE 'DISABLE' TO loFormSet.lcDataStat, loFormSet.lcFactStat, loFormSet.lcObjDisp

  llEnable = .F.
  .cboDivision.Enabled = llEnable
  .APActCode.Enabled = llEnable
  .GLActCode.Enabled = llEnable
  .cboInvRemit.Enabled = llEnable

  .BankChk.Enabled = llEnable
  .BankChk.kbBanks.Enabled = llEnable
  .BankChk.kbChkAccount.Enabled = llEnable
  
  .lcDebMemN.Enabled = llEnable
  .lcRef.Enabled = llEnable
  .lnPayAmnt.Enabled = llEnable
  .ldPayDat.Enabled = llEnable

  .kbCurrCode.Enabled = llEnable
  .lnCurrRate.Enabled = llEnable
  .ln1099Amnt.Enabled = llEnable
  .cmdPay.Enabled = llEnable

  .txtOutComp.Enabled = llEnable
  .txtOutAddr1.Enabled = llEnable
  .txtOutAddr2.Enabled = llEnable
  .txtOutAddr3.Enabled = llEnable
  .kbFacCode.Enabled = llEnable
ENDIF
ENDWITH 
*- End of lfEnable.

************************************************************
*! Name      : lfvPay
*! Developer : TMI
*! Date      : 02/05/2012
*! Purpose   : show up the payment screen
************************************************************
FUNCTION lfvPay 
PARAMETERS loFormSet

STORE 0 TO lnOChkNum, lnGnChkNm, lnChkNum

WITH loFormSet.Ariaform1
  lcBankCode = .BankChk.kbBanks.Keytextbox.Value
  lcCheckCode= .BankChk.kbChkAccount.Keytextbox.Value
  lcGlAcct   = .GLActCode.Keytextbox.Value
  lcAPAcct   = .APActCode.KEYtextbox.Value
ENDWITH 

IF loFormSet.lnPyChMN<>3 .AND. EMPTY(lcBankCode)
  ** If the bank code is empty, present the following message 
  ** and return to the bank code field
  ** Message : "   You have to enter the ð.  "
  **                 ® OK  ¯
  lcTBnkCode  = "the bank code"
  =gfModalGen("TRM04066B00000","DIALOG",lcTBnkCode)
  loFormSet.Ariaform1.BankChk.kbBanks.Keytextbox.Setfocus()
  RETURN .F.
ENDIF

* Initialize multi currency variables before rate checking
* Default the currency by the base currency if
* not multi currency.
IF loFormSet.llMultiCr
  lcCurrCode = loFormset.Ariaform1.kbCurrCode.Keytextbox.value  
  lnCurrUnit = loFormSet.lnCurrUnit
ELSE 
  lcCurrCode = oAriaApplication.BaseCurrency
  lnCurrRate = 1
  lnCurrUnit = 1
ENDIF

lcExSin2 = ' ' 
lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)

loVendCode = loFormset.Ariaform1.KBVendCode.Keytextbox
lcVendCode = PADR(loVendCode.Value,FSIZE('CVENDCODE','APVENDOR'))
*- message information
lcTinv      = "Invoice "
lcTDebMem   = "debit memo"
lcTRef      = "Reference"
lcTsession  = "Session  "
lcTFactor   = "factor code"
lcTBnkCode  = "the bank code"

*- Change date validation as follows 
*- Check if the date is empty,
loPayDat = loFormset.Ariaform1.ldPayDat
ldPayDat = loPayDat.Value
lnCurrRate = loFormSet.Ariaform1.lnCurrRate.Value
IF EMPTY(ldPayDat)
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'payment date')  
  loPayDat.Setfocus()
  RETURN .F.
ELSE
  *- Validate the date if it has not been validated before
  =lfvPayDat(loFormSet,loPayDat,.T.)
  IF EMPTY(ldPayDat) 
    loPayDat.Setfocus()
    RETURN .F.
  ENDIF  

  *- Check if the exchange rate is 0.
  IF lnCurrRate = 0 
    lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                     .T.,oAriaApplication.ActiveCompanyID, oAriaApplication.BaseCurrency, .T.)
    IF lnCurrRate = 0 
      IF !loFormSet.llAddRate
        ** MESSAGE : " A valid ð to ð exchange rate could not "
        **           " be found on ð.                         "
        **           "                  ® Ok ¯                "
        =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(lcCurrCode)+'|'+ALLTRIM(oAriaApplication.BaseCurrency)+'|'+DTOC(ldPayDat))
        loPayDat.Setfocus()
      ENDIF  
      RETURN .F.
    ENDIF  
  ENDIF
ENDIF  

loDebMemN = loFormSet.Ariaform1.lcDebMemN
lcDebMemN = loDebMemN.Value
IF EMPTY(lcDebMemN)
  ** Message: " You have to enter the ð.    "
  ** Choices: "           ® Ok ¯            "  
  =gfModalGen("TRM04066B00000","DIALOG",lcTDebMem)
  loDebMemN.Setfocus()
  RETURN .F.
ELSE
  SELECT APINVHDR
  lcSavOrder = SET('ORDER')
  SET ORDER TO TAG VENDINV
  IF SEEK(lcVendCode+lcDebMemN)
    lcTinvno = lcTinv+lcDebMemN 
    ** Message: " Invoice XXXXXXXXXX is already exist "
    **          " for vendor YYYYYYY.                 "
    ** Choices: "               ® Ok ¯                "  
    =gfModalGen("TRM04024B00000","DIALOG",lcTinvno+"|"+ALLTRIM(lcVendCode))
    loDebMemN.Setfocus()
    RETURN .F.
  ENDIF  
  SET ORDER TO &lcSavOrder
  SELECT APPAYMNT
ENDIF  
loPayAmnt = loFormset.Ariaform1.lnPayAmnt
lnPayAmnt = loPayAmnt.Value
IF lnPayAmnt <= 0
  ** Message: " The payment amount should be greater"
  **          " than zero.                          "
  ** Choices: "               ® Ok ¯                "  
  =gfModalGen("TRM04096B00000","DIALOG")
  loPayAmnt.Setfocus()
  RETURN .F.
ENDIF  

lo1099Amnt = loFormSet.Ariaform1.ln1099Amnt
ln1099Amnt = lo1099Amnt.Value
IF !BETWEEN(ln1099Amnt,0,lnPayAmnt)
  ** Message : " The 1099 amount must be between 0 and XXXXXX"
  ** Choices : "                    ® Ok ¯                   "
  =gfModalGen("TRM04017B00000","DIALOG","0|"+STR(lnPayAmnt))
  lo1099Amnt.Setfocus()
  RETURN .F.
ENDIF

** If the invoice is to be remit to factor, and there is
** no factor code, do not proceed and present the following message
loInvRemit = loFormset.Ariaform1.cboInvRemit
lcInvRemit = loInvRemit.Value
loFactor = loFormset.Ariaform1.kbFacCode.Keytextbox
lcFactor = loFactor.Value
IF lcInvRemit = 'F' .AND. EMPTY(lcFactor)
  ** Message : " You have to enter the ð.      "
  ** Choices : "             ® Ok ¯            "  
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  loFactor.Setfocus()
  RETURN .F.
ENDIF

IF loFormSet.lnPyChMN = 1   && Calling from manual check.
  **Get the next manual check number from apchecks** 
  SELECT APCHECKS
  IF SEEK(lcBankCode+lcCheckCode)
    lnOChkNum = NCHKNXTMN
    lnGnChkNm = NCHKNXTMN + 1
    lnChkNum  = lnOChkNum
  ENDIF 
  SELECT APPAYMNT
ENDIF

loFormSet.Ariaform1.lnCurrRate.Value = lnCurrRate 

llPay = .F.
lcRunScx = lfGetScx("AP\APPYADIN.SCX")
DO FORM (lcRunScx) WITH loFormSet TO llPay

IF llPay  &&Return form pay not from cancel 
  loFormset.ChangeMode('S')
  llPay =.F.
ENDIF

*- End of lfvPay.

************************************************************
*! Name      : lfVProPay 
*! Developer : TMI
*! Date      : 02/05/2012
*! Purpose   : Do the payment process
************************************************************ 
FUNCTION lfVProPay 
PARAMETERS loPayFormSet

lcRet = loPayFormSet.Ariaform1.lcPay_ChkNo.Value
IF EMPTY(lcRet)
  loPayFormSet.Ariaform1.lcPay_ChkNo.Value = IIF(loFormSet.lnPyChMN = 1 , lnChkNum , SPACE(12))
  RETURN .F.
ENDIF   
  
 
loFormSet = loPayFormSet.Callingform
STORE loPayFormSet.Ariaform1.lcPay_ChkNo.Value TO lnChkNum,lcPayNum
IF TYPE('lcPayNum')='C'
  lcPayNum = PADR(ALLTRIM(lcPayNum),8)
endif
*lnChkNum = &lnChkNum

WITH loFormset.Ariaform1
  lcRem1 = .txtOutComp.Value 
  lcRem2 = .txtOutAddr1.Value 
  lcRem3 = .txtOutAddr2.Value 
  lcRem4 = .txtOutAddr3.Value 
  lcRem5 = loFormSet.lcRem5
  
  lcSesNum = loFormSet.lcSequence
  lcRef = .lcRef.Value
  lcDivCode = .cboDivision.Value
ENDWITH 

lnApsAcLen = LEN(ALLTRIM(loFormSet.ap1.lcApsAcMas))

SELECT APINVHDR
lcSavOrder = SET('ORDER')
SET ORDER TO TAG VENDINV

SET ORDER TO &lcSavOrder
IF loFormSet.lnPyChMN = 1  && if manual check.
  IF(EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
      .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
      .OR. EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')))
    ** MESSAGE: " You have to enter the bank code,the   "
    **          " checking account,and the GL account.  "
    ** Choices: "                 ® Ok ¯                "
    =gfModalGen("TRM04021B00000","DIALOG")
    loFormSet.Ariaform1.BankChk.kbBanks.Keytextbox.Setfocus()
    RETURN .F.
  ENDIF
ENDIF

IF loFormSet.lnPyChMN <> 1   && if not manual check.
  IF EMPTY(lcPayNum)
    ** Message: " You have to enter The ð.           "
    ** Choices: "               ® Ok ¯               "  
    =gfModalGen("TRM04066B00000","DIALOG",'payment number')
    loPayFormSet.Ariaform1.lcPay_ChkNo.SetFocus()
    RETURN .F. && <------ quit function.
  ELSE
    SELECT APPAYMNT
    IF SEEK('PH'+SPACE(8)+SPACE(12)+lcPayNum) .OR.;
       SEEK('PN'+lcBankCode+lcCheckCode+lcPayNum)
      ** Message: " The payment number already exists. "
      ** Choices: "               ® Ok ¯               "  
      =gfModalGen("TRM04051B00000","DIALOG")
      loPayFormSet.Ariaform1.lcPay_ChkNo.SetFocus()
      RETURN .F. && <------ quit function.
    ENDIF 
  ENDIF
ELSE && in case of manual check. 
  IF EMPTY(lnChkNum)
    lnChkNum=lnGnChkNm
    loPayFormSet.Ariaform1.lcPay_ChkNo.Value = lnChkNum
    RETURN .F. && <------ quit function.
  ELSE
    IF lnChkNum < 0
      ** Message: " Negative values are not allowed.   "
      ** Choices: "               ® Ok ¯               "  
      =gfModalGen("TRM04087B00000","DIALOG")
      lnChkNum=lnOChkNum
      loPayFormSet.Ariaform1.lcPay_ChkNo.Value = lnChkNum
      loPayFormSet.Ariaform1.lcPay_ChkNo.SetFocus()
      RETURN .F. && <------ quit function.
    ELSE
      SELECT APPAYMNT
      IF SEEK('PM'+lcBankCode+lcCheckCode+PADL(lnChkNum,8,'0'))
        ** Message: " A manual check already exists with this number."
        **          " You can not have duplicate manual check numbers"
        **          " for the same checking account.                 "    
        ** Choices: "                      ® Ok ¯                    "
        =gfModalGen("TRM04053B00000","DIALOG")
        loPayFormSet.Ariaform1.lcPay_ChkNo.SetFocus()
        RETURN .F. && <------ quit function.
      ENDIF
    ENDIF  
  ENDIF
ENDIF  

*- year/period
lcYear   = loFormSet.lcYear
lcPeriod = loFormSet.lcPeriod

*- Start updating
SELECT APVENDOR
IF SEEK(lcVendCode)
  SELECT APVENHST
  lcSavOrder = SET('ORDER')
  IF SYS(22) <> 'VENDYEAR' 
    SET ORDER TO TAG VENDYEAR
  ENDIF
  
  IF !SEEK(lcVendCode+lcYear)
    APPEND BLANK
    REPLACE CVENDCODE WITH lcVendCode ;
            CFISFYEAR WITH lcYear   
    =lfAdd_info(.T.)
    =gfTableUpdate()
  ENDIF
  
  IF SEEK(lcVendCode+lcYear)
    *- first lock the files
    DIMENSION laLocked[2]    
    SELECT APCHECKS
    llContinue = .T.
    IF loFormSet.lnPyChMN = 1
      llContinue = llContinue AND gfObj_lock(.T.,.T.)
      laLocked[1] = IIF(llContinue,ALIAS(),' ')
    ENDIF 
    SELECT APVENHST
    llContinue = llContinue AND gfObj_lock(.T.,.T.)
    laLocked[2] = IIF(llContinue,ALIAS(),' ')
    IF llContinue
      SELECT APCHECKS
      REPLACE NCHKNXTMN WITH IIF(loFormSet.lnPyChMN = 1, lnChkNum+1 , VAL(lcPayNum)+1)
      =lfAdd_Info()  && Add the audit information to the record.        

      SELECT APVENHST
      lnChkExUnt = 0
      lnChkExRat = 0
      IF loFormSet.llMultiCr
        lnChkExRat = gfChkRate('lnChkExUnt',lcCurrCode,ldPayDat,.T.,.F.)
        IF lnChkExRat = 0
          lnChkExUnt = 1
          lnChkExRat = 1
        ENDIF
      ELSE
        lnChkExUnt = 1
        lnChkExRat = 1
      ENDIF  

      ** Add record to appaymnt file. **
      SELECT APPAYMNT
      APPEND BLANK
      REPLACE DPAYDATE   WITH ldPayDat,;
              CFISFYEAR  WITH lcYear,;   
              CFSPPRDID  WITH lcPeriod,;   
              CPAYCLNO   WITH lcVendCode,;
              CPAYCOMP   WITH APVENDOR.CVENCOMP,;
              NPAYAMNT   WITH lnPayAmnt,;
              NINV1099A  WITH ln1099Amnt,;
              CBNKCODE   WITH lcBankCode,;
              CCHKACCT   WITH lcCheckCode,;
              LPAYADVAN  WITH .T.,;
              CPAYTYPE   WITH "P",;
              CPAYRECST  WITH 'O';
              CPAYMETH   WITH IIF(loFormSet.lnPyChMN = 1,"M",IIF(loFormSet.lnPyChMN = 2,'N','H')),;
              CPAYDOCNO  WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
              CCURRCODE  WITH lcCurrCode,;   
              NEXRATE    WITH lnChkExRat,;   
              NCURRUNIT  WITH lnChkExUnt   
      =lfAdd_Info()  && Add the audit information to the record.

      ** Add record to apinvhdr file. **

      SELECT APINVHDR
      APPEND BLANK

      IF TYPE('APINVHDR.DPOSTDATE') = 'D'
        REPLACE COUTCOMP   WITH lcRem1,;    
                COUTADDR1  WITH lcRem2,;
                COUTADDR2  WITH lcRem3,;
                COUTADDR3  WITH lcRem4,;
                COUTADDR4  WITH lcRem5,;
                NINVAMNT   WITH 0 - lnPayAmnt,;
                NINV1099A  WITH ln1099Amnt,;
                CVENDCODE  WITH lcVendCode,;
                CINVNO     WITH lcDebMemN,;
                CDIVISION  WITH lcDivCode,;
                DINVDATE   WITH ldPayDat,;
                CINVREF    WITH lcRef,;
                DINVDUDAT  WITH ldPayDat,;
                CINVSTAT   WITH "A",;
                CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                     SPACE(lnApsAcLen),lcAPAcct),;
                CFISFYEAR  WITH lcYear,;
                CFSPPRDID  WITH lcPeriod,;
                CINVREMIT  WITH lcInvRemit,;
                CFACCODE   WITH lcFactor,;
                CVENPMETH  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
                CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
                CCURRCODE  WITH lcCurrCode,;
                NEXRATE    WITH lnCurrRate,;
                NCURRUNIT  WITH lnCurrUnit,;
                DPOSTDATE  WITH ldPayDat
      ELSE
        REPLACE COUTCOMP   WITH lcRem1,;
                 COUTADDR1  WITH lcRem2,;
                 COUTADDR2  WITH lcRem3,;
                 COUTADDR3  WITH lcRem4,;
                 COUTADDR4  WITH lcRem5,;
                 NINVAMNT   WITH 0 - lnPayAmnt,;
                 NINV1099A  WITH ln1099Amnt,;
                 CVENDCODE  WITH lcVendCode,;
                 CINVNO     WITH lcDebMemN,;
                 CDIVISION  WITH lcDivCode,;
                 DINVDATE   WITH ldPayDat,;
                 CINVREF    WITH lcRef,;
                 DINVDUDAT  WITH ldPayDat,;
                 CINVSTAT   WITH "A",;
                 CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
                 CFISFYEAR  WITH lcYear,;
                 CFSPPRDID  WITH lcPeriod,;
                 CINVREMIT  WITH lcInvRemit,;
                 CFACCODE   WITH lcFactor,;
                 CVENPMETH  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
                 CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
                 CCURRCODE  WITH lcCurrCode,;
                 NEXRATE    WITH lnCurrRate,;
                 NCURRUNIT  WITH lnCurrUnit
      ENDIF
      =lfAdd_Info()  && Add the audit information to the record. 

      ** Modify record in apvendor file. **
      SELECT APVENDOR
            
      IF SEEK(lcVendCode)
        REPLACE DVENLPAYD  WITH ldPayDat,;
                NVENLPAYA  WITH IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVEN1099B  WITH NVEN1099B + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(ln1099Amnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVENOPNDR  WITH NVENOPNDR + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVENBAL    WITH NVENBAL   - IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVENCPAY   WITH NVENCPAY  + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
        =lfAdd_Info()  && Add the audit information to the record.
      ENDIF  

      ** Modify record in vendor history file. **
      SELECT APVENHST
      lcSavOrder = SET('ORDER')
      IF SYS(22) <> 'VENDYEAR' 
        SET ORDER TO TAG VENDYEAR
      ENDIF

      IF SEEK(lcVendCode+lcYear)
        REPLACE NVNHTOTPA WITH NVNHTOTPA + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
        IF loFormSet.lnPyChMN = 1        && Case calling bar is Manual check payment.
          REPLACE NVNHMCHKP WITH NVNHMCHKP + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                 ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
        ELSE
          IF loFormSet.lnPyChMN = 2      && Case calling bar is Non check payment.
            REPLACE NVNHNCHKP WITH NVNHNCHKP + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                              ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
          ELSE                 && Case calling bar is Cash payment.
            REPLACE NVNHCASHP WITH NVNHCASHP + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                              ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
          ENDIF
        ENDIF   
        IF !EMPTY(lcPeriod)            
          lcfield="NVNHPAY"+ALLTRIM(STR(VAL(lcPeriod)))
          REPLACE &lcfield WITH &lcfield + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
        ENDIF  
      ENDIF  
      SET ORDER TO &lcSavOrder
      =lfAdd_Info()  && Add the audit information to the record.  

      ** Add Record to distribution file. **
      SELECT APDIST
      APPEND BLANK

      *- Used the variables hold signs in the currency equation. (Begin)
      REPLACE CVENDCODE   WITH lcVendCode,;
              CINVNO      WITH lcDebMemN,;
              DAPDTRDAT   WITH ldPayDat,;
              LAPDPOST    WITH .F.,;
              CAPDGLACT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
              NAPDAMNT    WITH lnPayAmnt,;
              CAPDACTID   WITH "A",;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CAPSESSNO   WITH lcSesNum,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckcode,;
              CAPDTRTYP   WITH IIF(loFormSet.lnPyChMn = 1,'M',IIF(loFormSet.lnPyChMn = 2,'N','H')),;
              CAPDREF     WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CCURRCODE   WITH lcCurrCode,;
              NEXRATE     WITH lnCurrRate,;
              NCURRUNIT   WITH lnCurrUnit,;
              NEQVAMNT    WITH IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)

      =lfAdd_Info()  && Add the audit information to the record.        
    
      ** Add 2nd record to distrebution file. **
      APPEND BLANK
      
      *- Used the variables hold signs in the currency equation. (Begin)
      REPLACE CVENDCODE   WITH lcVendCode,;
              CINVNO      WITH lcDebMemN,;
              CAPDGLACT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                               SPACE(lnApsAcLen),lcGlAcct),;
              NAPDAMNT    WITH 0-lnPayAmnt,;
              CAPDACTID   WITH "C",;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CAPSESSNO   WITH lcSesNum,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckcode,;
              DAPDTRDAT   WITH ldPayDat,;
              LAPDPOST    WITH .F.,;
              CAPDTRTYP   WITH IIF(loFormSet.lnPyChMn=1,'M',IIF(loFormSet.lnPyChMn=2,'N','H')),;
              CAPDREF     WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CCURRCODE   WITH lcCurrCode,;
              NEXRATE     WITH lnCurrRate,;
              NCURRUNIT   WITH lnCurrUnit,;
              NEQVAMNT    WITH 0-IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
      
      =lfAdd_Info()  && Add the audit information to the record.
    ELSE
      *- loop to unlock locked files
      FOR lnI = 1 TO ALEN(laLocked)
        IF !EMPTY(laLocked[lnI])
          SELECT (laLocked[lnI])
          =gfObj_lock(.F.)        
        ENDIF 
      ENDFOR 
      RETURN .F.
    ENDIF
  ENDIF
ENDIF  

*- final tabel update
SELECT APCHECKS
=gfTableUpdate()

SELECT APINVHDR
=gfTableUpdate()

SELECT APVENHST
=gfTableUpdate()

SELECT APVENDOR
=gfTableUpdate()

SELECT APDIST
=gfTableUpdate()

SELECT APPAYMNT
=gfTableUpdate()

*- back to select mode
loFormSet.ChangeMode('S')

llPay =.T.    &&Return form pay not from cancel 

RETURN .T.
 *- End of lfVProPay .

************************************************************
*! Name      : lfDefBank
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/06/2012
*! Purpose   : * Get default division, bank, and checks.
************************************************************
FUNCTION lfDefBank
PARAMETERS loFormset

SELECT APVENDOR
** set default bank and check **
WITH loFormset.AriaForm1
IF loFormset.lnPyChMN <> 3  && if not calling from cash payment
  GO TOP
  IF !EMPTY(.KBVendCode.Keytextbox.Value)
    DO CASE
        CASE SEEK(.KBVendCode.Keytextbox.Value) .AND. !EMPTY(CBNKCODE) && if found values in vendor file.
          .BankChk.kbBanks.Keytextbox.Value = CBNKCODE
          .BankChk.kbChkAccount.Keytextbox.Value= CCHKACCT

        CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
          .BankChk.kbBanks.Keytextbox.Value = APDIV.CBNKCODE
          .BankChk.kbChkAccount.Keytextbox.Value= APDIV.CCHKACCT

        OTHERWISE                      && get value from setup file.
          .BankChk.kbBanks.Keytextbox.Value = APSETUP.CBNKCODE
          .BankChk.kbChkAccount.Keytextbox.Value= APSETUP.CCHKACCT
      ENDCASE
      
      *- Default the currency by the bank currency.
      IF loFormSet.llMultiCr
        IF !EMPTY(.BankChk.kbChkAccount.Keytextbox.Value)
          IF SEEK(.BankChk.kbBanks.Keytextbox.Value+.BankChk.kbChkAccount.Keytextbox.Value,'APCHECKS')
            .kbCurrCode.Keytextbox.Value = IIF(EMPTY(APCHECKS.CCURRCODE),oAriaApplication.BaseCurrency,APCHECKS.CCURRCODE)
          ELSE
            .kbCurrCode.Keytextbox.Value = oAriaApplication.BaseCurrency
          ENDIF
        ELSE
          .kbCurrCode.Keytextbox.Value = oAriaApplication.BaseCurrency
        ENDIF
      ENDIF
      
      IF !EMPTY(.BankChk.kbBanks.Keytextbox.Value) .AND. !EMPTY(.BankChk.kbChkAccount.Keytextbox.Value)
        IF SEEK(.BankChk.kbBanks.Keytextbox.Value+.BankChk.kbChkAccount.Keytextbox.Value,'APCHECKS')
          .GLActCode.Keytextbox.Value = IIF(EMPTY(APCHECKS.CCHKGLACC),loformset.ap1.lcEmptyAcc, APCHECKS.CCHKGLACC)
        ELSE  
          IF !EMPTY(APVENDOR.CAPACCT)
            .GLActCode.Keytextbox.Value = APVENDOR.CAPACCT
          ELSE 
            IF !EMPTY(APDIV.CAPACCT)
              .GLActCode.Keytextbox.Value = APDIV.CAPACCT
            ELSE
              .GLActCode.Keytextbox.Value = IIF(EMPTY(APSETUP.CAPACCT),loformset.ap1.lcEmptyAcc, APSETUP.CAPACCT)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF !EMPTY(APVENDOR.CAPACCT)
         .APActCode.Keytextbox.Value = APVENDOR.CAPACCT
      ELSE 
        IF !EMPTY(APDIV.CAPACCT)
          .APActCode.Keytextbox.Value = APDIV.CAPACCT
        ELSE
          .APActCode.Keytextbox.Value = IIF(EMPTY(APSETUP.CAPACCT),lcEmptyAcc, APSETUP.CAPACCT)
        ENDIF
      ENDIF

  ENDIF
ELSE
  IF SEEK(.KBVendCode.Keytextbox.Value)
    IF !EMPTY(APVENDOR.CCASHACCT)
      .GLActCode.Keytextbox.Value = APVENDOR.CCASHACCT
    ELSE 
      IF !EMPTY(APDIV.CCASHACCT)
        .GLActCode.Keytextbox.Value = APDIV.CCASHACCT
      ELSE
        .GLActCode.Keytextbox.Value = IIF(EMPTY(APSETUP.CCASHACCT),loformset.ap1.lcEmptyAcc, APSETUP.CCASHACCT)
      ENDIF
    ENDIF
  ENDIF  
  IF SEEK(.KBVendCode.Keytextbox.Value)
    IF !EMPTY(APVENDOR.CAPACCT)
      .APActCode.Keytextbox.Value = APVENDOR.CAPACCT
    ELSE 
      IF !EMPTY(APDIV.CAPACCT)
        .APActCode.Keytextbox.Value = APDIV.CAPACCT
      ELSE
        .APActCode.Keytextbox.Value = IIF(EMPTY(APSETUP.CAPACCT),loformset.ap1.lcEmptyAcc, APSETUP.CAPACCT)
      ENDIF
    ENDIF
  ENDIF  
ENDIF  
.APActCode.Keytextbox.Valid()
.GLActCode.Keytextbox.Valid()
ENDWITH 
loFormSet.lcObjDisp = IIF(!EMPTY(loFormset.Ariaform1.BankChk.kbBanks.Keytextbox.Value), "ENABLE", "DISABLE")
SELECT APPAYMNT
*- End of lfDefBank.

************************************************************
*! Name      : lfvCurr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/06/2012
*! Purpose   : Validating Currency
************************************************************
FUNCTION lfvCurr
PARAMETERS loFormSet,loFld

llBrowse = loFld.Selectedfrombrowse
loFld.Selectedfrombrowse = .F.
lcCurrCode = loFormset.Ariaform1.kbCurrCode.Keytextbox.Value
ldPayDat   = loFormset.Ariaform1.ldPayDat.Value

IF llBrowse .OR. !SEEK(lcCurrCode,'SYCCURR') .OR. ATC("?",lcCurrCode) > 0 .AND. LASTKEY() = 13
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  IF EMPTY(laTemp[1])
    lcCurrCode = lcOldCurr
  ELSE
    lcCurrCode = laTemp[1]
  ENDIF
ENDIF

lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
lnCurrUnit = loFormSet.lnCurrUnit

IF lcCurrCode <> loFormset.Ariaform1.kbCurrCode.Keytextbox.OldValue
    lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                     .T.,oAriaApplication.ActiveCompanyID, oAriaApplication.BaseCurrency, .T.)
    IF lnCurrRate = 0 .AND. !loFormSet.llAddRate
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found for ð.                        "
      **           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",;
        ALLTRIM(lcCurrCode)+'|'+ALLTRIM( oAriaApplication.BaseCurrency)+'|'+DTOC(ldPayDat))
    ENDIF
    IF lcCurrCode =  oAriaApplication.BaseCurrency .OR. !loFormSet.llEditEx
      loFormSet.Ariaform1.lnCurrRate.Enabled = .F.
    ELSE
      loFormSet.Ariaform1.lnCurrRate.Enabled = .T.
    ENDIF  
    loFormSet.Ariaform1.lnCurrRate.Value = lnCurrRate

    loFormSet.lnCurrUnit = lnCurrUnit
    loFormset.Ariaform1.kbCurrCode.Keytextbox.Value = lcCurrCode
    loFormset.Ariaform1.ldPayDat.Value = ldPayDat
    
ENDIF
SELECT APINVHDR

 *- End of lfvCurr.
 
************************************************************
*! Name      : lfvCurrRate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/06/2012
*! Purpose   : Validating currency
************************************************************
FUNCTION lfvCurrRate
PARAMETERS loFld

IF loFld.Value <> loFld.OldValue .AND. loFld.Value < 0
  *- Add an appropriate message.
  *- if the entered value is not greater than zero,
  *- Message :  "   ð should be greater than ð.   "
  *-                  ®   OK   ¯
  =gfModalGen("TRM04072B00000","DIALOG", 'The exchange rate|zero') 
  loFld.Value = loFld.OldValue 
  loFld.Refresh()
ENDIF
*:B610124,1 SAB 17/10/2012 Fix error when changing the currency [Start]
*=lfRefresh()
*:B610124,1 SAB 17/10/2012 Fix error when changing the currency [End]
*:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002][Start]
loFld.Parent.Parent.lnCurrRate = loFld.Value
*:B610308,1 MMT 04/17/2013 Advanced payment screens does not save currency exchange rate[T20130409.0002][End]

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

************************************************************
*! Name      : lfvFactor
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/07/2012
*! Purpose   : Valid fucntion for Factor
************************************************************
FUNCTION lfvFactor
PARAMETERS loFormSet,loFld

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


NOTE :=>
NOTE :=>
*E303016,1 TMI 01/24/2012 [Start] add this function here just until a separate task is created for it
NOTE :=>
NOTE :=>
*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Hesham El_Sheltawi
*! Date      : 08/14/2002
*! Purpose   : Function to logicaly lock a record
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  Lock or unlock
*!                       
*!*************************************************************
*! Returns            :  .T. --> succeded
*!                       .F. --> unsuccess
*!*************************************************************
*! Example            :  gfObj_lock(.T.,.T.)
*!*************************************************************
FUNCTION gfObj_Lock
*E303016,1 TMI 01/24/2012 [Start] do not allow the same user to overwrite the same session
**                                that is, if llDenyOvwrt is .T. then deny overwrite
**                                         if llDenyOvwrt is .F. then allow overwrite    ( as defaul )
*PARAMETERS lLok_Set
PARAMETERS lLok_Set,llDenyOvwrt
*E303016,1 TMI 01/24/2012 [End  ] 
PRIVATE lnRecNo,lRet_Flag
lnWorkArea = ALIAS()
IF EMPTY(lnWorkArea)
  RETURN
ENDIF
PRIVATE lnOldrpSt
SELECT (lnWorkArea)
lnDataSession = SET("DATASESSION")
lnAlias = SELECT()
lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  SELECT (lnWorkArea)
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   UNLOCK RECORD lnRecNo
   IF llLocked
     TABLEREVERT(.F.)
   ENDIF 
   IF DELETED()
     =gfModalGen('INM00095B00000','ALERT')
     SELECT (lnAlias)
     *THIS.CHangemode("S")
     RETURN .F.
   ENDIF
  ENDIF  

  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      lcLok_User = cLok_User
      IF !EMPTY(lcLok_User)
        IF ALLTRIM(lcLok_User) = ALLTRIM(oAriaApplication.User_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          *E303016,1 TMI 01/24/2012 [Start] 
          IF llDenyOvwrt 
            =gfModalGen("INM00028B00000","ALERT",lcLok_User)
            STORE .F. TO lLok_It, lRet_Flag
          ELSE 
            *E303016,1 TMI 01/24/2012 [End  ] 
            IF gfModalGen("INM00240B00006","ALERT")=2  
              lLok_It    = .F.
              lRet_Flag  = .F.
            ELSE      
              lLok_It    = .T.
            ENDIF
            *E303016,1 TMI 01/24/2012 [Start] 
          ENDIF 
          *E303016,1 TMI 01/24/2012 [End  ] 
        ELSE

          *We save old value of reprocess first.[START]
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          
					SET DATASESSION TO 1
					llLoop = .F.
					SELECT syuStatc
          IF SEEK ('INI'+'OLDVARS'+lcLok_User,'syuStatc') 
            LOCAL lnStatcRec
            SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
                lnStatcRec = RECNO()
	            IF RLOCK('syuStatc')
  	            UNLOCK RECORD lnStatcRec IN  syuStatc 
        	      lLok_It    = .T.  	            
*!*	  	            lnStatcRec = RECNO()
*!*	    	          GO (oAriaApplication.UserStaticRecord) IN syuStatc 
*!*	      	        =RLOCK('syuStatc')
*!*	        	      GO lnStatcRec
          	  ELSE
            	  UNLOCK
              	 GO (oAriaApplication.UserStaticRecord) IN syuStatc 
              	=RLOCK('syuStatc') 
              	*** Display the message "Record is in use by user AAAA"
              	lcLok_User = oAriaApplication.getUserName(lcLok_User)
              	*** Record is in use by user ????    
              	SET DATASESSION TO (lnDataSession)
              	IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
                	llLoop = .T.
              	ENDIF  
              	lLok_It    = .F.
              	lRet_Flag  = .F.
              	EXIT 
            	ENDIF
           ENDSCAN 	
          ELSE
            lLok_It    = .T. 
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  lnOldrpSt
					SET DATASESSION TO (lnDataSession)
          IF llLoop
            LOOP 
          ENDIF 

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (lnDataSession)
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
			=TABLEUPDATE(0,.T.)              
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (lnDataSession)
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH oAriaApplication.User_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()
    =TABLEUPDATE(0,.T.)
    lRet_Flag  = .T.    
  ENDIF
ENDIF
SELECT (lnWorkArea)
UNLOCK
SELECT (lnAlias)

RETURN lRet_Flag

*:B610318,1 HES 04/28/2013 Fix bug of not updating the GL account when change Bank Code [T20130416.0021][Start]
*!*************************************************************
*! Name      : lfRefGLAcc
*! Developer : Hesham ElMasry
*! Date      : 04/24/2013
*! Purpose   : Function to Refresh the GL Account code
*!*************************************************************
FUNCTION lfRefGLAcc
LPARAMETERS loFormSet

WITH loFormset.AriaForm1
*:B610334,1 SAB 05/22/2013 Fix error when opening the Advanced Manual Check Payment [T20130514.0034][Start]
*IF loFormset.lnPyChMN <> 3  && if not calling from cash payment
*-SAB -----
*IF TYPE('loFormset.lnPyChMN') AND loFormset.lnPyChMN <> 3  && if not calling from cash payment
IF TYPE('loFormset.lnPyChMN') = 'N' AND loFormset.lnPyChMN <> 3  && if not calling from cash payment
*-SAB -----
*:B610334,1 SAB 05/22/2013 Fix error when opening the Advanced Manual Check Payment [T20130514.0034][End]
  IF !EMPTY(.KBVendCode.Keytextbox.Value)
      IF !EMPTY(.BankChk.kbBanks.Keytextbox.Value) .AND. !EMPTY(.BankChk.kbChkAccount.Keytextbox.Value)
        IF SEEK(.BankChk.kbBanks.Keytextbox.Value+.BankChk.kbChkAccount.Keytextbox.Value,'APCHECKS')
          .GLActCode.Keytextbox.Value = IIF(EMPTY(APCHECKS.CCHKGLACC),loformset.ap1.lcEmptyAcc, APCHECKS.CCHKGLACC)          
          .GLActCode.SharedValidation()
        ENDIF
      ENDIF
  ENDIF
ENDIF
ENDWITH
*:B610318,1 HES 04/28/2013 Fix bug of not updating the GL account when change Bank Code [T20130416.0021][End]