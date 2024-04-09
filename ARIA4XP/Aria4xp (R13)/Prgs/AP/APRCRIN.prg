*:************************************************************************
*:  Program File: APRCRIN.PRG
*:  Desc.       : Recurring Payable
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/14/2012
*:  Reference   : E303064,1
*:************************************************************************
*: B609857,1 SAB 03/11/2012 Fix AP Media Problems reported by Mariam [T20120304.00004]
*: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004]
*: B611084,1 MMT 11/25/2015 User cannot add Distribution lines in AP recurring payables screen[T20151104.0021]
*B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [T20180509.0007]
*:************************************************************************

#INCLUDE R:\aria4xp\prgs\ap\APRCRIN.h
*- Call the screen
lcRunScx = lfGetScx("AP\APRcrIn.scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
*E303064,1 TMI 02/15/2012
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
*! Date      : 02/14/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome+oAriaapplication.ActiveModuleID
SET PROCEDURE TO (lcPath+'\APMAIN.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES('APRCRIN')

*** Load program base file
*: B609857,1 SAB 03/11/2012 Fix AP Media Problems reported by Mariam [Start]
*=lfAddProp(loFormSet,'lcBaseFile',sydObjct.cBaseFile)
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))
*: B609857,1 SAB 03/11/2012 Fix AP Media Problems reported by Mariam [End]

*: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004][Start]
=lfAddProp(loFormSet,'lcEmptycode','      ')
loFormSet.lcEmptyCode = gfCodDes(' ' , ' ')
*: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004][End]

*** Get the base file title
lcFile_ttl = PROPER(ALLTRIM(LOOKUP(sydFiles.cFile_ttl,loFormSet.lcBaseFile,;
                 sydFiles.cFile_Nam)))
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcTransTtl =  loFormSet.geta27filettl(ALLTRIM(loFormSet.lcBaseFile))&&,SET("Datasession"),ALIAS()) 
  lcFile_ttl = IIF(EMPTY(lcTransTtl),lcFile_ttl ,lcTransTtl)
ENDIF
*N000682,1 MMT 12/09/2012 Globalization changes[END]                 
=lfAddProp(loFormSet,'lcFile_ttl',lcFile_ttl)
=lfAddProp(loFormSet,'CAUTMTYPE','R')
lcCAUTMCODE_Ttl = ALLTRIM(LOOKUP(sydField.CFLD_HEAD,'CAUTMCODE ',;
                 sydField.CFLD_NAME))
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcTransAUTMCODE_Ttl = loFormSet.geta27fldheader (ALLTRIM('CAUTMCODE '))&&,SET("Datasession"),ALIAS())
  lcCAUTMCODE_Ttl =  IIF(EMPTY(lcTransAUTMCODE_Ttl),lcCAUTMCODE_Ttl,lcTransAUTMCODE_Ttl)
ENDIF
*N000682,1 MMT 12/09/2012 Globalization changes[END]                 
=lfAddProp(loFormSet,'LCCAUTMCODE_TTL',LCCAUTMCODE_TTL )
loFormset.Ariaform1.kbRecurringCode.Keytextbox.InputMask = REPLICATE('X',FSIZE('CAUTMCODE',loFormSet.lcBaseFile))


*- Define variables
=lfDefineVars(loFormSet)

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "'R'+CAUTMCODE"
  .cBrowseIndexFields     = "CAUTMTYPE+CAUTMCODE"
  .cBrowseIndexName       = 'HTYPCOD'
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)

ENDWITH

=lfAddProp(loFormSet,'lcSequence',gfSequence('CAPSESSNO'))
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*loFormSet.Ariaform1.Caption = ALLTRIM(loFormSet.Ariaform1.Caption)+'      Session: '+loFormSet.lcSequence
loFormSet.Ariaform1.Caption = ALLTRIM(loFormSet.Ariaform1.Caption)+'      '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_SESSION,loformset.GetHeaderText("LANG_APRCRIN_SESSION",loformset.HeaderAlias))+' '+loFormSet.lcSequence
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*- Create temp table
=lfCrtTemp()

*- fill the arrays of popup fields
=lfPopArrs('cAutMBase','laType')

  *** Prepare Duration array from SYDFIELD and get its maximum width
=lfPopArrs('cAutFUnit','laDuration')

  *** Prepare Remit to array from SYDFIELD and get its maximum width
=lfPopArrs('cInvRemit','laRemitTo')

*** Prepare Payment method array from SYDFIELD and get its maximum width
*** Remove 'Credit cards' (C) option from the array
=lfPopArrs('cVenPMeth','laPayMeth')
FOR lnCount = 1 TO ALEN(loFormSet.laPayMeth)
  IF 'C' $ loFormSet.laPayMeth[lnCount,2]
    =ADEL(loFormSet.laPayMeth, lnCount) = 1
    DIMENSION loFormSet.laPayMeth[ALEN(loFormSet.laPayMeth,1) - 1,2]
    EXIT
  ENDIF
ENDFOR  && laPayMeth

WITH loFormSet.Ariaform1
  .cboType.RowSource = 'Thisformset.laType'
  =lfColumnWidthes(.cboType)
  .cboFreqType.RowSource = 'Thisformset.laDuration'
  =lfColumnWidthes(.cboFreqType)
  .cboInvRemit.RowSource = 'Thisformset.laRemitTo'
  =lfColumnWidthes(.cboInvRemit)
  .cboVendPmeth.RowSource = 'Thisformset.laPayMeth'
  =lfColumnWidthes(.cboVendPmeth)
ENDWITH

=lfAddProp(loFormSet,'ldFisBgDt,ldFisEnDt',{})
SELECT FISHD
LOCATE
IF !EOF()
  LOCATE REST FOR  cFisYstat = 'C'
  IF FOUND()
    loFormSet.ldFisBgDt = FISHD.dFisBgDat         && begin date of current fiscal year
    loFormSet.ldFisEnDt = FISHD.dFisEnDat         && end   date of current fiscal year
  ENDIF
ENDIF
IF USED('FISHD')
  =gfSysClose('FISHD')
ENDIF

SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APINVADT
SET ORDER TO TAG DTYPCOD

=lfAddProp(loFormSet,'lcInvADtEx',SYS(14,VAL(SYS(21)))  ) && current index expression

*** Base file for the current screen
SELECT APINVAHD
SET ORDER TO TAG HTYPCOD                 && should already be set
=lfAddProp(loFormSet,'lcInvAHdEx',SYS(14,VAL(SYS(21)))  ) && current index expression

SET FILTER TO
SET FILTER TO cAutMType = "R"          && filter on recurring
*** Set relation to details file on the current key.
SET RELATION TO cAutMType + cAutMCode INTO APINVADT ADDITIVE
SET RELATION TO CVENDCODE INTO APVENDOR ADDITIVE

SELECT CODES
*SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR cFld_Name ='N/A'

*- Go to Select mode
loFormSet.ChangeMode('S')

*- End of lfFormInit.

************************************************************
*! Name      : lfColumnWidthes
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/23/2012
*! Purpose   : Adjust column widthes
************************************************************
FUNCTION lfColumnWidthes
PARAMETERS loPop
LOCAL lcW
lcW = ALLTRIM(STR(loPop.Width - 25 ))
loPop.ColumnWidths = '&lcW,0'
*- End of lfColumnWidthes.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/15/2012
*! Purpose   : Define variables as properties
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet
*substr(lcScfields , at(',',','+lcScfields,21))
lcScFields = 'CAUTMTYPE,CAUTMCODE,CAUTMBASE,CVENDCODE,NAUTFREQ,CAUTFUNIT,DAUTSTGEN,DAUTNXGEN,DAUTENGEN,'+;
             'DAUTLGEN,NAUTFSTNO,NAUTNXTNO,NINVAMNT,CTERMCODE,NINVDISOF,CINVREF,CDIVISION,CINVREMIT,'+;
             'CVENPRIOR,CVENPMETH,CBNKCODE,CCHKACCT,CCHKGLACC,NINVAMTAP,NINVDISAP,NINVADJAP,NINVA1099,'+;
             'COUTCOMP,COUTADDR1,COUTADDR2,COUTADDR3,CAPACCT,CFACCODE,CCURRCODE'
=lfAddProp(loFormSet,'lcScFields',lcScFields)    && lcScFields

=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))   && ap1
=lfAddProp(loFormSet,'lnApsAcLen',LEN(loFormSet.ap1.lcEmptyAcc))

*- Define approve variables
=lfAddProp(loFormSet,'CBNKCODE',SPACE(8))
=lfAddProp(loFormSet,'CCHKACCT',SPACE(12))
=lfAddProp(loFormSet,'CCHKGLACC',loFormSet.ap1.lcEmptyAcc)
=lfAddProp(loFormSet,'NINVAMTAP,NINVDISAP,NINVADJAP,NINVA1099',0)

=lfAddProp(loFormSet,'CAPACCT,lcDistAcct',loFormSet.ap1.lcEmptyAcc)
=lfAddProp(loFormSet,'lcTaxDes,lcTaxCode',' ')


*- Define Terms codes
=lfAddProp(loFormSet,'laTermCodeInitVal',0)
=lfAddProp(loFormSet,'lnTerDueD',loFormSet.laTermCodeInitVal)
=lfAddProp(loFormSet,'lnTerDiscD',loFormSet.laTermCodeInitVal)
=lfAddProp(loFormSet,'lnTerDiscR',loFormSet.laTermCodeInitVal)

=lfAddProp(loFormSet,'lnDistAmnt',0)

=lfAddProp(loFormSet,'lnDistLines',0)

=lfAddProp(loFormSet,'laTermCode[1]','')
DIMENSION loFormSet.laTermCode[3,2]
WITH loFormSet
  .laTermCode[1,1] = 'NTERDUED'
  .laTermCode[1,2] = 'lnTerDueD'
  .laTermCode[2,1] = 'NTERDISCD'
  .laTermCode[2,2] = 'lnTerDiscD'
  .laTermCode[3,1] = 'NTERDISCR'
  .laTermCode[3,2] = 'lnTerDiscR'
ENDWITH

*- Set the control Source of the term releated fields
WITH loFormSet.Ariaform1
  .txtTerDueD.ControlSource = 'Thisformset.lnTerDueD'
  .txtTerDiscD.ControlSource = 'Thisformset.lnTerDiscD'
  .txtTerDiscR.ControlSource = 'Thisformset.lnTerDiscR'
ENDWITH


*- Define Tax codes
=lfAddProp(loFormSet,'laTaxCodeInitVal',loFormSet.ap1.lcEmptyAcc)
=lfAddProp(loFormSet,'lcApDGlAct',loFormSet.laTaxCodeInitVal)
=lfAddProp(loFormSet,'laTaxCode[1]','')
DIMENSION loFormSet.laTaxCode[1,2]
loFormSet.laTaxCode[1,1] = 'CGLINPACCT'
loFormSet.laTaxCode[1,2] = 'lcApDGlAct'


lcVars = 'lcFactStat, lcRemitStat, lcDataStat, laCtrStat ,lcObjDisp , lcRateDisp'
=lfAddProp(loFormSet,lcVars,'DISABLE')

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

*- Save status
=lfAddProp(loFormSet,'llCSave',.F.)

*- Disable Vars
lcVars = 'lcAcctStat , lcAmntStat , lcTaxStat  , lcNewStat  ,'+;
         'lcRemStat  , lcRemitStat, lcTypesStat, lcVendStat ,'+;
         'lcDataStat , lcFactStat , lcDButStat , lcData13Stat,'+;
         'lcAprvStat , lcAddStat  , lcEditStat'
=lfAddProp(loFormSet,lcVars,'DISABLE')

=lfAddProp(loFormSet,'llFirstAdd',.F.)

=lfAddProp(loFormSet,'SlctdAlias','')

*- End of lfDefineVars.

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
    ** Choices: "                ® OK ¯
    *             "
    *: B609857,1 MMT 07/01/2012 Fix AP Media Problems reported by Mariam [T20120304.00004]
    *=gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(lcVendCode))
    =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(apvendor.cvendcode))
    *: B609857,1 MMT 07/01/2012 Fix AP Media Problems reported by Mariam [T20120304.00004]
    *
    .KBVendCode.KeyTextBox.VALUE    = lcVendCode
    .KBVendPhone.KeyTextBox.VALUE   = lcVPhone
    .KBVendCompany.KeyTextBox.VALUE = lcVendComp
    RETURN .F.
  ENDIF

  .txtAmount.Enabled = .cboType.Value=='A'

  .lnDisc.Enabled = .T.
  .lcRef.Enabled = .T.

  .cboDivision.Value = APVENDOR.CDIVISION
  .cboDivision.Enabled = .T.
  .cboDivision.Refresh()

  .cboTerms.Value = APVENDOR.CTERMCODE
  .cboTerms.Enabled = .T.
  .cboTerms.Refresh()

  .txtVendPrior.Value = APVENDOR.CVENPRIOR
  .txtVendPrior.Enabled = .T.
  .txtVendPrior.Refresh()

  .cboVendPmeth.Value = APVENDOR.CVENPMETH
  .cboVendPmeth.Enabled = .T.
  IF .cboVendPmeth.Value = 'C'   && If the vendor's payment method is credit cards,  use 'Printed checks' instead
    .cboVendPmeth.Value = 'P'
  ENDIF
  .cboVendPmeth.Refresh()

  .kbCurrCode.Keytextbox.Value = APVENDOR.CCURRCODE
  .kbCurrCode.Enabled = .T.
  .kbCurrCode.Keytextbox.Refresh()


  =lfvRemit(loFormset,.T.)
  loFormset.Ariaform1.cboInvRemit.Enabled = .T.

  *- Get the related fields values of the terms used
  =lfGetCodeRltFld(loFormset,'laTermCode','CTERMCODE',loFormSet.Ariaform1.cboTerms.Value)

  *** 5. A default AP account
  IF !EMPTY(APVENDOR.CAPACCT)
    *Old : laData[32]   = APVENDOR.CAPACCT
    loFormSet.CAPACCT = APVENDOR.CAPACCT
  ELSE
    loFormSet.CAPACCT = IIF(!EMPTY(APVENDOR.CDIVISION) .AND.;
                     SEEK(APVENDOR.CDIVISION, 'APDIV') .AND.;
                     !EMPTY(APDIV.CAPACCT),;
                     APDIV.CAPACCT, APSETUP.CAPACCT)
  ENDIF

  *** 6. A default distribution account
  IF !EMPTY(APVENDOR.CEXPACCT)
    loFormSet.lcDistAcct = APVENDOR.CEXPACCT
  ELSE
    loFormSet.lcDistAcct = IIF(SEEK(APVENDOR.CDIVISION, 'APDIV') .AND. ;
                     !EMPTY(APDIV.CEXPACCT), ;
                     APDIV.CEXPACCT, APSETUP.CEXPACCT)
  ENDIF

  *** 7. Distribtion lines window selection
  *** Tax codes
  IF APVENDOR.cTaxType = 'L'
    *: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004][Start]
    *loFormSet.lcTaxDes   = loFormSet.ap1.lcEmptyCode
    loFormSet.lcTaxDes   = loFormSet.lcEmptyCode
    *: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004][End]
    loFormSet.lcTaxCode  = SPACE(6)
  ELSE
    loFormSet.lcTaxDes   = " "
  ENDIF

  .cmdDist.Enabled = .T.
  .cmdApprv.Enabled = .T.

  ENDWITH &&loFormSet.AriaForm1
  RETURN .T.
ELSE

  RETURN .F.
ENDIF
 *- End of  lfvVendor.

************************************************************
*! Name      : lfvAmount
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : Amount Validation
************************************************************
FUNCTION lfvAmount
PARAMETERS loFormSet,loFld

LOCAL lnRet
lnRet = 1
IF loFld.Value <> loFld.OldValue .AND.;
   ((!(loFld.Value > 0)  .AND. ;
       gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTInvAmnt,loFormSet.GetHeaderText("LANG_APRCRIN_lcTInvAmnt",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTZero,loFormSet.GetHeaderText("LANG_APRCRIN_lcTZero",loFormSet.HeaderAlias)))> 0);
   .OR.;
   (loFld.Value < loFld.Parent.txtTotApproved.Value .AND. ;
    gfModalGen("TRM04009B00000","DIALOG") > 0))

  loFld.Value = loFld.OldValue
  lnRet = 0
ELSE
  *** if a term code is selected, refresh the discount amount
  IF !EMPTY(loFld.parent.cboTerms.Value)
    loFld.parent.lnDisc.Value = loFld.Value * loFormSet.lnTerDiscR/100
    loFld.parent.lnDisc.Refresh()
  ENDIF
ENDIF
RETURN lnRet
*- End of lfvAmount.

************************************************************
*! Name      : lfvDisc
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : Disc validation
************************************************************
FUNCTION lfvDisc
PARAMETERS loFld

*** if the entered value is negative
***     Message :  "   Negative values are not allowed.   "
***                            ®  OK   ¯
*** or if it is greater than the invoice amount,
***     Message : " The offered discount can not be    "
***               " greater than the invoice amount.   "
***                            ®  OK   ¯
*** restore the old value of the field
LOCAL lnRet
lnRet = 1
IF loFld.Value <> loFld.OldValue .AND.;
   ((loFld.Value < 0 .AND. ;
     gfModalGen("TRM04087B00000","DIALOG")>0) .OR.;
    (loFld.Value > loFld.parent.txtAmount.Value .AND. gfModalGen("TRM04011B00000","DIALOG")>0))
  loFld.Value = loFld.OldValue
  lnRet = 0
ENDIF
RETURN lnRet
*- End of lfvDisc. laData[15]

************************************************************
*! Name      : lfvPayMeth
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : PayMeth
************************************************************
FUNCTION lfvPayMeth
PARAMETERS loFormSet,loFld
LOCAL lnRet
lnRet = 1

IF loFld.Value <> loFld.OldValue .AND. INLIST('H', loFld.Value, loFld.OldValue)
  *** If payment method is toggled between 'Cash payments (H)' and
  *** the other available payment methods, clear the approval fields
  *** Store corresponding laData elements
  WITH loFormSet
  .CBNKCODE = SPACE(8)
  .CCHKACCT = SPACE(12)
  .CCHKGLACC = loFormSet.ap1.lcEmptyAcc
  STORE 0 TO .NINVAMTAP,.NINVDISAP,.NINVADJAP,.NINVA1099

  .Ariaform1.txtTotApproved.Value = 0
  ENDWITH
ENDIF

RETURN lnRet
*- End of lfvPayMeth.

************************************************************
*! Name      : lfvCurCod
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : Currency validation
************************************************************
FUNCTION lfvCurCod
PARAMETERS loFormSet,loFld

=gfOpenTable(oAriaApplication.SysPath+'SYCCURR'  ,'Ccurrcode','SH')

llBrowse = loFld.Selectedfrombrowse
IF llBrowse .OR. !SEEK(loFld.Keytextbox.Value,'SYCCURR') .OR. ATC("?",loFld.Keytextbox.Value) > 0 &&.AND. LASTKEY() = 13
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	  lcFile_Ttl = "Currency"
*!*	  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
*!*	               "CCURRDESC :R :H= 'Description',  " +;
*!*	               "CCURRSMBL :R :H= 'Symbol'"
*!*	  =gfBrows('','CCURRCODE','laTemp')
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_CURRTTL,loFormSet.GetHeaderText("LANG_APRCRIN_CURRTTL",loFormSet.HeaderAlias))
  lcBrFields = "CCURRCODE :R :H= '"+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_CURRCODE,loFormSet.GetHeaderText("LANG_APRCRIN_CURRCODE",loFormSet.HeaderAlias))+"'," +;
               "CCURRDESC :R :H= '"+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_DESC,loFormSet.GetHeaderText("LANG_APRCRIN_DESC",loFormSet.HeaderAlias))+"',  " +;
               "CCURRSMBL :R :H= '"+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_SYMBOL,loFormSet.GetHeaderText("LANG_APRCRIN_SYMBOL",loFormSet.HeaderAlias))+"'"
  =gfBrows('','CCURRCODE','laTemp',lcFile_Ttl)
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  IF EMPTY(laTemp[1])
    loFld.Keytextbox.Value = loFld.Keytextbox.OldValue
  ELSE
    loFld.Keytextbox.Value = laTemp[1]
  ENDIF
ENDIF

loFld.Selectedfrombrowse = .F.

*- Compare the new currency with the old one.
IF loFld.Keytextbox.Value <> loFld.Keytextbox.OldValue
  IF !EMPTY(loFormset.CCHKACCT) .AND. (!SEEK(loFormSet.CCHKGLACC, 'APCHECKS');
      .OR. APCHECKS.cCurrCode <> loFld.Keytextbox.Value)
    WITH loFormSet
    .CBNKCODE = SPACE(8)
    .CCHKACCT = SPACE(12)
    .CCHKGLACC = loFormSet.ap1.lcEmptyAcc
    STORE 0 TO .NINVAMTAP,.NINVDISAP,.NINVADJAP,.NINVA1099
    ENDWITH
  ENDIF
ENDIF
*- End of lfvCurr.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

*- if this function is called from the standard DODEFAUL then these variables are still not defined
IF TYPE('loFormset.laType')='U'
  RETURN
ENDIF

WITH loFormSet.AriaForm1
  .cmdDist.Enabled = .T.

  DO case
  CASE loFormSet.ActiveMode = 'S'
    .cmdDist.Enabled = .F.
    .kbRecurringCode.Enabled = .T.

    .cboType.Value = loFormset.laType[1,2]
    .cboFreqType.Value = loFormset.laDuration[1,2]
    .cboInvRemit.Value = loFormset.laRemitTo[1,2]
    .cboVendPmeth.Value = loFormset.laPayMeth[1,2]

    .KBVendCode.KeyTextBox.VALUE    = ''
    .KBVendPhone.KeyTextBox.VALUE   = ''
    .KBVendCompany.KeyTextBox.VALUE = ''

    .txtFreq.Value = 1

    .txtStartInvDt.Value = {}
    .txtNextInvDt.Value = {}

    .txtLastInvDt.Value = {}
    .txtLastGenDt.Value = {}

    .txtFirstInvNo.Value = 1
    .txtNextInvNo.Value = 1

    .txtAmount.Value = 0.00
    .cboTerms.Value = ''
    .lnDisc.Value = 0.00

    .lcRef.Value = ''
    .cboDivision.Value = ''

    .kbFacCode.Keytextbox.Value = ''
    .txtOutComp.Value = ''
    .txtOutAddr1.Value = ''
    .txtOutAddr2.Value = ''
    .txtOutAddr3.Value = ''

    .txtVendPrior.Value = ''

    .kbCurrCode.Keytextbox.Value = ''
    .txtTotApproved.Value = 0.00

    .dtInvDuDat.Text1.Value = {}

  CASE loFormSet.ActiveMode = 'A'

    loFormSet.llFirstAdd = .T.

    SELECT(loFormSet.lcTmpDist)
    ZAP

    .cmdApprv.Enabled = .F.
    .cmdDist.Enabled = .F.

    .kbRecurringCode.Enabled = .F.

    .KBVendCode.ENABLED = .T.
    .KBVendPhone.ENABLED = .T.
    .KBVendCompany.ENABLED = .T.

    .txtFreq.Enabled = .T.
    .cboFreqType.Enabled = .T.

    .txtStartInvDt.Enabled = .T.
    .txtLastInvDt.Enabled = .T.
    .txtFirstInvNo.Enabled = .T.

    .txtNextInvDt.Enabled = .F.
    .txtNextInvNo.Enabled = .F.

    .txtAmount.Enabled = .T.
    .cboTerms.Enabled = .F.
    .cboDivision.Enabled = .F.
    .lcRef.Enabled = .F.
    .lnDisc.Enabled = .T.

    .cboInvRemit.Enabled = .F.
    .txtOutComp.Enabled = .F.
    .txtOutAddr1.Enabled = .F.
    .txtOutAddr2.Enabled = .F.
    .txtOutAddr3.Enabled = .F.
    .kbFacCode.Enabled = .F.

    .txtVendPrior.Enabled = .F.
    .txtTerdued.Enabled = .F.
    .txtTerDiscd.Enabled = .F.
    .cboVendPMeth.Enabled = .F.
    .dtInvDuDat.Enabled = .F.
    .txtTerDiscr.Enabled = .F.
    .kbCurrCode.Enabled = .F.
    .txttotApproved.Enabled = .F.

    *- initial values in select mode
    .txtFreq.Value = 1
    .txtStartInvDt.Value = {}
    .txtLastInvDt.Value = {}
    .dtInvDuDat.Text1.Value = {}
    .txtNextInvDt.Value = {}
    .txtLastGenDt.Value = {}
    .txtFirstInvNo.Value = 1
    .txtNextInvNo.Value = 1
    .txtAmount.Value = 00.0
    .lnDisc.Value = 0
    .txtTerdued.Value = 0
    .txtTerDiscd.Value = 0
    .txtTerDiscr.Value = 0.000
    .txttotApproved.Value = 0.00

  CASE loFormSet.ActiveMode $ 'E|V'

    DIMENSION laData[1]
    lcScFields = loFormSet.lcScFields
    SELECT APINVAHD
    SCATTER FIELDS &lcScFields TO laData

    *- record values in edit mode
    .kbRecurringCode.Keytextbox.Value = laData[2]
    .cboType.Value = laData[3]

    .KBVendCode.KeyTextBox.VALUE    = laData[4]
    .KBVendPhone.KeyTextBox.VALUE   = APVENDOR.CPHONENO
    .KBVendCompany.KeyTextBox.VALUE = APVENDOR.CVENCOMP

    .txtFreq.Value = laData[5]
    .cboFreqType.Value = laData[6]

    .txtStartInvDt.Value = laData[7]
    .txtNextInvDt.Value = laData[8]

    .txtLastInvDt.Value = laData[9]
    .txtLastGenDt.Value = laData[10]

    .txtFirstInvNo.Value = laData[11]
    .txtNextInvNo.Value = laData[12]

    .txtAmount.Value = laData[13]
    .cboTerms.Value = laData[14]
    .lnDisc.Value = laData[15]

    .lcRef.Value = laData[16]
    .cboDivision.Value = laData[17]

    .cboInvRemit.Value = laData[18]

    .txtVendPrior.Value = laData[19]
    .cboVendPmeth.Value = laData[20]


    loFormSet.CBNKCODE = laData[21]
    loFormSet.CCHKACCT = laData[22]
    loFormSet.CCHKGLACC = laData[23]
    loFormSet.NINVAMTAP = laData[24]
    loFormSet.NINVDISAP = laData[25]
    loFormSet.NINVADJAP = laData[26]
    loFormSet.NINVA1099 = laData[27]

    .txtOutComp.Value = laData[28]
    .txtOutAddr1.Value = laData[29]
    .txtOutAddr2.Value = laData[30]
    .txtOutAddr3.Value = laData[31]

    loFormSet.CAPACCT = laData[32]
    .kbFacCode.Keytextbox.Value = laData[33]
    .kbCurrCode.Keytextbox.Value = laData[34]

    lnTotAppPay = laData[24] + laData[25] + laData[26]
    .txtTotApproved.Value = lnTotAppPay
    .cmdApprv.Enabled = loFormSet.ActiveMode = 'E' .OR. lnTotAppPay > 0

    =lfGetCodeRltFld(loFormset,'laTermCode','CTERMCODE',.cboTerms.Value)
    .dtInvDuDat.Text1.Value = loFormSet.lnTerDueD + .txtNextInvDt.Value

    =lfvRemit(loFormSet,.T.)

    *** If 'Edit' mode,
    lcInvADtEx = loFormSet.lcInvADtEx
    IF loFormSet.ActiveMode = 'E'
      .kbRecurringCode.Enabled = .F.

      .KBVendCode.ENABLED = .F.
      .KBVendPhone.ENABLED = .F.
      .KBVendCompany.ENABLED = .F.

      .txtStartInvDt.Enabled = .F.
      .txtFirstInvNo.Enabled = .F.
      .txtLastGenDt.Enabled = .F.
      .txtFirstInvNo.Enabled = .F.
      =lfvRemit(loFormset)

      .txtTerdued.Enabled = .F.
      .dtInvDuDat.Enabled = .F.
      .txtTerDiscd.Enabled = .F.
      .txtTerDiscr.Enabled = .F.
      .txttotApproved.Enabled = .F.

      *** Collect the distribution lines of the current recurring payable
      *** invoice into the temporary file lcTmpDist
      SELECT *,'S' AS 'cStatus',;
             RECNO() AS 'NRECNO';
        FROM (oAriaApplication.DataDir+'APINVADT');
        WHERE &lcInvADtEx = laData[1] + laData[2];
        INTO DBF (oAriaApplication.WorkDir+loFormSet.lcTmpDist)

      SELECT(loFormSet.lcTmpDist)
      loFormSet.lnDistLines = RECCOUNT()

      *** Get the total of the distribution lines = lnDistAmnt
      SUM nAPDAmnt TO loFormSet.lnDistAmnt
      GO TOP
      IF loFormSet.lnDistLines > 0
        loFormSet.lcRemStat    = 'ENABLE'
        loFormSet.lcTypesStat  = 'DISABLE'
      ELSE
        loFormSet.lcRemStat    = 'DISABLE'
        loFormSet.lcTypesStat  = 'ENABLE'
      ENDIF
      loFormSet.lcData13Stat   = IIF(laData[3]='P','DISABLE','ENABLE')
      loFormSet.lcDataStat     = 'ENABLE'
      loFormSet.lcEditStat     = 'ENABLE'
    ELSE

      SELECT(loFormSet.lcTmpDist)
      ZAP

      SELECT APINVADT
      SUM nAPDAmnt TO loFormSet.lnDistAmnt FOR &lcInvADtEx = 'R'+ loFormSet.Ariaform1.kbRecurringCode.Keytextbox.Value

      *** Refresh the existing relation to position the record
      *** pointer in the details file on the first line of details
      SELECT APINVAHD
      IF !EOF('APINVAHD')
        GO RECNO()
      ELSE
        GO TOP
      ENDIF
      WITH loFormSet
        STORE 'DISABLE' TO .lcRemStat, .lcTypesStat,;
              .lcData13Stat, .lcDataStat, .lcEditStat
      ENDWITH
    ENDIF

  ENDCASE

  .txtLastGenDt.Enabled = .F.
  .Refresh()
ENDWITH
*- End of lfChangeMode.

************************************************************
*! Name      : lfvRecurringCode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/18/2012
*! Purpose   : Reurring code valid function
************************************************************
FUNCTION lfvRecurringCode
PARAMETERS loFormSet,loFld
LOCAL lnSlct
lnSlct = SELECT(0)

WITH loFld.Keytextbox

.Value     = ALLTRIM(.Value)
.Value     = IIF(ISDIGIT(LEFT(.Value,1)),;
                         PADL(.Value, FSIZE('cAutMCode','APINVAHD'),'0'),;
                         PADR(.Value,FSIZE('cAutMCode','APINVAHD')))

ENDWITH
lcFile_Ttl = loFormSet.lcFile_Ttl
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse
SELECT (lcBaseFile)
IF llBrowse .OR. !SEEK(loFormSet.CAUTMTYPE+loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0
  lnClosRec  = RECNO(0)
  IF BETWEEN(lnClosRec,1,RECCOUNT(lcBaseFile))
    GO lnClosRec
  ELSE
    GO TOP
  ENDIF
  IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0

    DIMENSION laTemp[1]
    laTemp = ''
    =gfBrows(' ','CAUTMCODE','laTemp',lcFile_Ttl)

    IF !EMPTY(laTemp[1])
      loFld.KeyTextBox.VALUE = ALLTRIM(laTemp[1])
      llView = .T.
    ELSE
      loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
    ENDIF
  ELSE
    lnOption  = gfModalGen('QRM00001B00001','Dialog',;
    loFormSet.lcCAUTMCODE_Ttl + " : " +ALLTRIM(loFld.KeyTextBox.VALUE))

    DO CASE
      CASE lnOption = 1
        DIMENSION laTemp[1]
        laTemp = ''
        =gfBrows(' ','CAUTMCODE','laTemp',lcFile_Ttl)

        IF !EMPTY(laTemp[1])
          loFld.KeyTextBox.VALUE = ALLTRIM(laTemp[1])
          llView = .T.
        ELSE
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        ENDIF
        loFormSet.REFRESH()
      CASE lnOption = 2
        loFormSet.CHangeMode('A')
        RETURN

      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CAUTMCODE
  llView = .T.
ENDIF
IF llView = .T.
  loFormSet.CHangeMode('V')
ENDIF

SELECT (lnSlct)
*- End of lfvRecurringCode.

************************************************************
*! Name      : lfPopArrs
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/21/2012
*! Purpose   : Populate popup arrays
************************************************************
FUNCTION lfPopArrs
PARAMETERS lcFld, lcArr
=lfAddProp(loFormSet,lcArr+'[1]','')
DIMENSION &lcArr[1,1]
lnTypeLen  = gfGetVld(lcFld,@&lcArr)
DIMENSION loFormSet.&lcArr[ALEN(&lcArr,1),ALEN(&lcArr,2)]
ACOPY(&lcArr,loFormSet.&lcArr)

*- End of lfPopArrs.

************************************************************
*! Name      :  lfvType
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : Type Validation
************************************************************
FUNCTION lfvType
PARAMETERS loFld

*** If 'P'ercent, invoice amount = 100, and disabled
***               discount       = 0,    and disabled
IF loFld.Value <> loFld.OldValue
  IF loFld.Value = 'P'
    loFld.Parent.txtAmount.Value = 100
    loFld.Parent.lnDisc.Value = 0
    loFld.Parent.txtAmount.Enabled = .F.
    loFld.Parent.lnDisc.Enabled = .F.
    loFld.Parent.lblPrcntSign.Caption = '%'
  ELSE
    loFld.Parent.txtAmount.Value = 0
    loFld.Parent.lnDisc.Value = 0
    loFld.Parent.txtAmount.Enabled = .T.
    loFld.Parent.lnDisc.Enabled = .T.
    loFld.Parent.lblPrcntSign.Caption = ' '
  ENDIF
ENDIF
*- End of lfvType.

************************************************************
*! Name      : lfStartInvDt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : StartInvDt Valid
************************************************************
FUNCTION lfStartInvDt
PARAMETERS loFormSet,loFld
LOCAL loLast,lnRet

*** Return the old value of the field in the following cases :
*** a. if it is emptied, or
*** b. if it is greater than the last invoice date if the latter is
***    not empty, in which case the following message is presented, or
*** Message :   The ð date cannot be less than the ð date.     "
***                              ®   OK   ¯
*** c. If the date is invalid

loLast = loFld.Parent.txtLastInvDt
IF (loFld.Value <> loFld.OldValue OR EMPTY(loFld.Value)) AND ;
  ((!EMPTY(loLast.Value) AND loLast.Value < loFld.Value AND ;
  gfModalGen("TRM04028B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTFirstInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTFirstInv",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTLastInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTLastInv",loFormSet.HeaderAlias))) > 0) .OR. ;
  !lfvDtMsg(oAriaApplication.PrntCompanyID))

  loFld.Value = loFld.OldValue
  lnRet = 0
ELSE
  loFld.Parent.txtNextInvDt.Value = loFld.Value
  lnRet = 1
ENDIF
loFld.Parent.dtInvDuDat.Text1.Value = loFormSet.lnTerDueD + loFld.Parent.txtNextInvDt.Value

RETURN lnRet
*- End of lfStartInvDt.

************************************************************
*! Name      : lfLastInvDt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : txtLastInvDt Validation
************************************************************
FUNCTION lfLastInvDt
PARAMETERS loFormset,loFld
*** Return the old value of the field in the following cases :
*** a. if it is emptied, or
*** b. if it is less than the first invoice date if the latter is
***    not empty, in which case the following message is presented, or
*** Message :   The ð date cannot be less than the ð date.     "
***                              ®   OK   ¯
*** c. If the date is invalid
LOCAL lnRet,loStart,loNext,lcTDate
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTDate = IIF('LAST'$UPPER(loFld.Name),LANG_APRCRIN_lcTLastInv,LANG_APRCRIN_lcTNextInv)
lcTDate = IIF('LAST'$UPPER(loFld.Name),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTLastInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTLastInv",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTNextInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTNextInv",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lnRet = 1
loStart = loFld.Parent.txtStartInvDt
loNext = loFld.Parent.txtNextInvDt
loLast = loFld.Parent.txtLastInvDt
IF (loFld.Value <> loFld.OldValue .OR. EMPTY(loFld.Value)) .AND. ;
   ( (!EMPTY(loStart.Value) .AND. loFld.Value < loStart.Value .AND.;
   gfModalGen("TRM04028B00000","DIALOG",lcTDate+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTFirstInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTFirstInv",loFormSet.HeaderAlias))) > 0) .OR.;
  (loNext.Value > loLast.Value .AND.;
   gfModalGen("TRM04028B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTLastInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTLastInv",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTNextInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTNextInv",loFormSet.HeaderAlias))) > 0) .OR.;
  !lfvDtMsg(oAriaApplication.PrntCompanyID))

  loFld.Value = loFld.OldValue
  lnRet = 0
ENDIF
RETURN lnRet
*- End of lfLastInvDt .

************************************************************
*! Name      : lfFirstInvNo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : txtFirstInvNo validation
************************************************************
FUNCTION lfFirstInvNo    && is enabled only in the ADD mode
PARAMETERS loFld

*** Return the old value of the field in the following cases :
*** a. if it is emptied, or
*** b. if the entered value is not greater than zero, or,
LOCAL lnRet
IF loFld.Value <> loFld.OldValue .AND.;
  ( (loFld.Value < 1 .AND.;
      gfModalGen("TRM04072B00000","DIALOG",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTFirstNum,loFormSet.GetHeaderText("LANG_APRCRIN_lcTFirstNum",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTZero,loFormSet.GetHeaderText("LANG_APRCRIN_lcTZero",loFormSet.HeaderAlias))) > 0) )
*N000682,1 11/20/2012 MMT Globlization changes[End]


  loFld.Value = loFld.OldValue
  lnRet = 0
ELSE
  loFld.Parent.txtNextInvNo.Value = loFld.Value
  lnRet = 1
ENDIF
RETURN lnRet

*- End of lfFirstInvNo.

************************************************************
*! Name      : lfNextInvNo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : txtNextInvNo validation
************************************************************
FUNCTION lfNextInvNo  && is enabled only in the EDIT mode
PARAMETERS loFld
*** If the field is empty, or,
*** if the entered value is not greater than zero, or,
*** if the next invoice number is not greater than the first invoice number
***     Message :  "   ð should be greater than ð.   "
***                            ®   OK   ¯
*** restore the old value of the field
LOCAL lnRet
lnRet = 1
IF loFld.Value <> loFld.OldValue .AND.;
   ((loFld.Value < 1 .AND. ;
     gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTNextNum,loFormSet.GetHeaderText("LANG_APRCRIN_lcTNextNum",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTZero,loFormSet.GetHeaderText("LANG_APRCRIN_lcTZero",loFormSet.HeaderAlias))) > 0) .OR.;
     loFld.Parent.txtFirstInvNo.Value > loFld.Value .AND.;
gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTNextNum,loFormSet.GetHeaderText("LANG_APRCRIN_lcTNextNum",loFormSet.HeaderAlias))+'|'+LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTFirstNum,loFormSet.GetHeaderText("LANG_APRCRIN_lcTFirstNum",loFormSet.HeaderAlias)))) > 0)
*N000682,1 11/20/2012 MMT Globlization changes[End]


  loFld.Value = loFld.OldValue
  lnRet = 0
ENDIF
RETURN lnRet

*- End of lfNextInvNo.

************************************************************
*! Name      : lfCrtTemp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/19/2012
*! Purpose   : Create temp tables needed for the screen
************************************************************
FUNCTION lfCrtTemp

SELECT APINVADT
=AFIELDS(laInvADt)

lnInvADt    = ALEN(laInvADt,1)
DIMENSION laInvADt[lnInvADt+2,18]

laInvADt[lnInvADt+1,1] = 'CSTATUS'
laInvADt[lnInvADt+1,2] = 'C'
laInvADt[lnInvADt+1,3] = 1
laInvADt[lnInvADt+1,4] = 0

laInvADt[lnInvADt+2,1] = 'NRECNO'
laInvADt[lnInvADt+2,2] = 'N'
laInvADt[lnInvADt+2,3] = 10
laInvADt[lnInvADt+2,4] = 0

*- update other array fields
=lfUpdStruArr(@laInvADt,lnInvADt)

*** Creat temp  file from the array.
=lfAddProp(loFormSet,'lcTmpDist',gfTempName())

*** Creat temp ditribution file from the array.
CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTmpDist) FROM ARRAY laInvADt

*- End of lfCrtTemp.


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
*B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
*FUNCTION gfObj_Lock
FUNCTION lfObj_Lock
*B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

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


************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : FormBeforeSave VALIDATION
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet


SELECT APINVAHD

*** llCSave = .F. if saving will not be possible

llCSave = .T.
DO CASE
  CASE EMPTY(loFormset.Ariaform1.KBVendCode.Keytextbox.Value)
    *** Message : "   You have to enter the ð.  "
    ***                 ® OK  ¯
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04066B00000","DIALOG",LANG_APRCRIN_lcTVendor)
=gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTVendor,loFormSet.GetHeaderText("LANG_APRCRIN_lcTVendor",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormset.Ariaform1.KBVendCode.Keytextbox.Setfocus()
    llCSave    = .F.

  CASE loFormset.ActiveMode = 'A' .AND. !SEEK(loFormset.Ariaform1.KBVendCode.Keytextbox.Value,'APVENDOR')
    *** Message :   "    ð not found.  "
    ***             "         ®  Ok  ¯          "
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen("TRM04002B00000","DIALOG",LANG_APRCRIN_lcTCapVen+ALLTRIM(loFormset.Ariaform1.KBVendCode.Keytextbox.Value))
= gfModalGen("TRM04002B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTCapVen,loFormSet.GetHeaderText("LANG_APRCRIN_lcTCapVen",loFormSet.HeaderAlias))+ALLTRIM(loFormset.Ariaform1.KBVendCode.Keytextbox.Value))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    llCSave     = .F.

  *** if the entered value is not greater than zero,
  CASE loFormset.Ariaform1.txtAmount.Value <= 0
    ***     Message :  "   ð should be greater than ð.   "
    ***                            ®  OK   ¯
    *** restore the old value of the field
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04072B00000","DIALOG",LANG_APRCRIN_lcTInvAmnt+'|'+LANG_APRCRIN_lcTZero) > 0))
=gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTInvAmnt,loFormSet.GetHeaderText("LANG_APRCRIN_lcTInvAmnt",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTZero,loFormSet.GetHeaderText("LANG_APRCRIN_lcTZero",loFormSet.HeaderAlias))) > 0))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormset.Ariaform1.txtAmount.Setfocus()
    llCSave = .F.

  *** If the distributed amount is not equal to the invoice
  *** amount, present the following message and do not save
  CASE loFormSet.lnDistAmnt <> loFormset.Ariaform1.txtAmount.Value
    *** Message : " The distribution amount does not "
    ***           " total the invoice amount.        "
    ***           "               ®  Ok  ¯             "
    =gfModalGen("TRM04031B00000","DIALOG")
    llCSave = .F.

  CASE loFormset.ActiveMode = 'A' .AND. !EMPTY(APVENDOR.CFACCODE) ;
       .AND. loFormset.Ariaform1.cboInvRemit.Value <> 'F'
    *** Message : " Vendor ð is factored, this invoice has not been  "
    ***           " remited to the factor.                           "
    ***           " <Remit to factor> <Stop saving> <Continue saving>"
    *** ð = 'Vendor name'
    lnOption = gfModalGen("QRM04065B04003","DIALOG",ALLTRIM(loFormset.Ariaform1.KBVendCode.Keytextbox.Value))
    DO CASE
      CASE lnOption = 1
        loFormset.Ariaform1.cboInvRemit.Value = 'F'
        loFormset.Ariaform1.kbFacCode.Keytextbox.Value  = APVENDOR.cFacCode

      CASE lnOption = 2
        llCSave = .F.
    ENDCASE

    *** If the invoice is to be remit to factor, and there is
    *** no factor code, do not save and present the following message
    CASE loFormset.Ariaform1.cboInvRemit.Value = 'F' .AND. EMPTY(loFormset.Ariaform1.kbFacCode.Keytextbox.Value)
      *** Message : "   You have to enter the ð.  "
      ***                 ® OK  ¯
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04066B00000","DIALOG",LANG_APRCRIN_lcTFactor)
=gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTFactor,loFormSet.GetHeaderText("LANG_APRCRIN_lcTFactor",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormset.Ariaform1.kbFacCode.Keytextbox.Setfocus()
      llCSave    = .F.

ENDCASE
loFormSet.llCSave = llCSave
RETURN llCSave


*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      Function : lfvDistrib
*!
*!**************************************************************************
*
FUNCTION lfvDistrib
PARAMETERS loFormSet

*** Check if the invoice amount is greater than zero
WITH loFormSet.Ariaform1
IF loFormSet.ActiveMode<>'V' .AND. .txtAmount.Value <= 0
  *** If it is not, present the following message and do not
  *** branch to the distribution lines screen.
  *** Message : "   You have to enter the ð.  "
  ***                 ® OK  ¯
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04066B00000","DIALOG",LANG_APRCRIN_lcTInv)
=gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTInv",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .txtAmount.Setfocus()
  RETURN
ENDIF

  *** Add highlight characters to child window buttons
lcRemStat  = IIF((loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A' ) .AND.;
                  loFormSet.lnDistLines > 0 ,'ENABLE','DISABLE')

IF loFormSet.ActiveMode = 'A' .AND. loFormSet.llFirstAdd
  loFormSet.llFirstAdd = .F.
  SELECT (loFormSet.lcTmpDist)
  *** Create a default distribution record
  *** tax code is initialized to spaces
  INSERT INTO(loFormSet.lcTmpDist);
    (cAutMType, cAutMCode, nAPDAmnt, cAPDGlAct, nAPDLinNo, cStatus);
    VALUES('R', .kbRecurringCode.Keytextbox.Value, .txtAmount.Value, loFormSet.lcDistAcct, 1, 'A')

  =gfAdd_Info(loFormSet.lcTmpDist)

  *** Default the distributed amount with the invoice amount
  loFormSet.lnDistLines  = 1                   && current lines number
  loFormSet.lnDistAmnt   = .txtAmount.Value    && current total distributed amounts (default)

  lcVendStat   = "DISABLE"
  lcTypesStat  = "DISABLE"
  lcRemStat    = 'ENABLE'

  .KBVendCode.ENABLED = .F.
  .KBVendPhone.ENABLED = .F.
  .KBVendCompany.ENABLED = .F.
  .cboType.Enabled = .F.

  *SHOW GET pbRem       &lcRemStat
  *SHOW GET lcAccDesc
  *SHOW WINDOW (lcBrowsTtl) REFRESH SAME
ENDIF
ENDWITH

=lfDispObjct(loFormSet)
lcRunScx = lfGetScx("AP\APDIST.scx")
DO FORM (lcRunScx) WITH loFormSet


*- End of lfvDistrib.

************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2012
*! Purpose   : Save process
************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet

IF loFormSet.llCSave
  *** If the invoice is to be remitted to 'Other',
  *** store the Other's information
*!*	  IF laData[18] = 'O'
*!*	    laData[28] = lcRmtComp
*!*	    laData[29] = lcRmtAddr1
*!*	    laData[30] = lcRmtAddr2
*!*	    laData[31] = lcRmtAddr3
*!*	  ELSE
*!*	    STORE SPACE(40) TO laData[28], laData[29], laData[30], laData[31]
*!*	  ENDIF
*!*	  laData[33]   = IIF(laData[18] = 'F', laData[33], SPACE(6))

  *** If accounts fields are empty, clear them from '0'
  IF EMPTY(STRTRAN(STRTRAN(loFormSet.CCHKGLACC,'-'),'0'))
    loFormSet.CCHKGLACC   = SPACE(loFormSet.lnApsAcLen)
  ENDIF
  IF EMPTY(STRTRAN(STRTRAN(loFormSet.CAPACCT,'-'),'0'))
    loFormSet.CAPACCT   = SPACE(loFormSet.lnApsAcLen)
  ENDIF

  SELECT APINVAHD
  IF loFormSet.ActiveMode = 'A'
    APPEND BLANK
  ENDIF

  *** Store laData values in the current record
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields TO laData
  WITH loFormSet.Ariaform1
  laData[1]  =   'R'
  laData[2]  =   .kbRecurringCode.Keytextbox.Value
  laData[3]  =   .cboType.Value

  laData[4]  =   .KBVendCode.KeyTextBox.VALUE

  laData[5]  =   .txtFreq.Value
  laData[6]  =   .cboFreqType.Value

  laData[7]  =   .txtStartInvDt.Value
  laData[8]  =   .txtNextInvDt.Value

  laData[9]  =   .txtLastInvDt.Value
  laData[10] =   .txtLastGenDt.Value

  laData[11] =   .txtFirstInvNo.Value
  laData[12] =   .txtNextInvNo.Value

  laData[13] =   .txtAmount.Value
  laData[14] =   .cboTerms.Value
  laData[15] =   .lnDisc.Value

  laData[16] =   .lcRef.Value
  laData[17] =   .cboDivision.Value

  laData[18] =   .cboInvRemit.Value
  laData[19] =   .txtVendPrior.Value
  laData[20] =   .cboVendPmeth.Value

  laData[21] =   loFormSet.CBNKCODE
  laData[22] =   loFormSet.CCHKACCT
  laData[23] =   loFormSet.CCHKGLACC
  laData[24] =   loFormSet.NINVAMTAP
  laData[25] =   loFormSet.NINVDISAP
  laData[26] =   loFormSet.NINVADJAP
  laData[27] =   loFormSet.NINVA1099

  laData[28] =   .txtOutComp.Value

  laData[29] =   .txtOutAddr1.Value
  laData[30] =   .txtOutAddr2.Value
  laData[31] =   .txtOutAddr3.Value
  laData[32] =   loFormset.CAPACCT
  laData[33] =   .kbFacCode.Keytextbox.Value

  laData[33] =   .kbFacCode.Keytextbox.Value
  laData[34] =   .kbCurrCode.Keytextbox.Value
  ENDWITH

  GATHER FROM laData FIELDS &lcScFields
  gfTableUpdate()

  lcTmpDist = loFormSet.lcTmpDist
  =gftmp2Mast('APINVADT',lcTmpDist)       && 'Saving recurring payable '+laData[2]+' ...')

  SELECT APINVADT
  gfTableUpdate()
ENDIF

SELECT APINVAHD
*- End of lfFormSavefiles.

************************************************************
*! Name      : lfvTermCode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/26/2012
*! Purpose   : TermCode
*************************************************************
FUNCTION lfvTermCode
PARAMETERS loFormSet,loFld
LOCAL lnRet
lnRet = 1

=lfGetCodeRltFld(loFormset,'laTermCode','CTERMCODE',loFld.Value)

IF loFld.parent.cboType.Value = 'A'  && Update discount field if base is 'A'mounts
  loFld.parent.lnDisc.Value  =  ROUND(loFld.parent.txtAmount.Value * loFormSet.lnTerDiscR / 100,2)
  loFld.parent.lnDisc.Refresh()
ENDIF

RETURN lnRet
*- End of lfvTermCode.

************************************************************
*! Name      : lfGetCodeRltFld
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/26/2012
*! Purpose   : RltFld
************************************************************
FUNCTION lfGetCodeRltFld
PARAMETERS loFormSet,lcCodeArr,lcCode,lcFldVal
LOCAL lnInitVal,lnI
lcInitVal = loFormSet.&lcCodeArr.InitVal
FOR lnI = 1 TO ALEN(loFormSet.&lcCodeArr,1)
  lcVar = loFormSet.&lcCodeArr[lnI,2]
  STORE lcInitVal TO &lcVar
ENDFOR
DIMENSION &lcCodeArr[ALEN(loFormSet.&lcCodeArr,1),ALEN(loFormSet.&lcCodeArr,2)]
ACOPY(loFormSet.&lcCodeArr,&lcCodeArr)
=gfRltFld(lcFldVal , @&lcCodeArr , lcCode)
WITH loFormSet
  FOR lnI = 1 TO ALEN(loFormSet.&lcCodeArr,1)
    lcVar = loFormSet.&lcCodeArr[lnI,2]
    .&lcVar = &lcVar
  ENDFOR
ENDWITH
*- End of lfGetCodeRltFld.

************************************************************
*! Name      : lfvDivision
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/26/2012
*! Purpose   : Division
************************************************************
FUNCTION lfvDivision
PARAMETERS loFormSet,loFld
RETURN 1
*- End of lfvDivision.

*!**************************************************************************
*!
*!      Function: lfvAprovPay
*!
*!**************************************************************************
*
FUNCTION lfvAprovPay
PARAMETERS loFormSet

PRIVATE lc1099Stat
lcRunScx = lfGetScx("AP\APRcrApr.scx")

loFormSet.SlctdAlias = 'APINVAHD'

WITH loFormSet.Ariaform1
IF !loFormSet.ActiveMode = 'V'

  lcApprvStat  = 'ENABLE'
  ** If bank code in the vendor file is not empty.
  lc1099Stat   = IIF(!EMPTY(APVENDOR.cVen1099T),'ENABLE','DISABLE')

  *** Check if the invoice amount is greater than zero
  IF !( .txtAmount.Value > 0 )
    *** If it is not, present the following message and do not
    *** branch to the distribution lines screen.
    *** Message : "   You have to enter the ð.  "
    ***                 ® OK  ¯
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04066B00000","DIALOG",LANG_APRCRIN_lcTInv)
=gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTInv,loFormSet.GetHeaderText("LANG_APRCRIN_lcTInv",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    .txtAmount.Setfocus()
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcApprv    = LANG_APRCRIN_lcTApprove     && push button prompt (pbApprove)
lcApprv    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTApprove,loFormSet.GetHeaderText("LANG_APRCRIN_lcTApprove",loFormSet.HeaderAlias))     && push button prompt (pbApprove)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *** Store current contents of laData for retreival in case of 'Cancelling'
    lcBankCode = loFormSet.CBNKCODE
    lcCheckAcc = loFormSet.CCHKACCT
    lcGLAcount = loFormSet.CCHKGLACC

    lcApprPay  = loFormSet.NINVAMTAP
    lcApprDis  = loFormSet.NINVDISAP
    lcApprAdj  = loFormSet.NINVADJAP
    lc1099Amnt = loFormSet.NINVA1099

    *** Default the approved to pay amount with the
    *** invoice amount - discount amount if it is empty
    IF loFormSet.NINVAMTAP = 0
      loFormSet.NINVAMTAP = .txtAmount.Value - loFormSet.Ariaform1.lnDisc.Value
      loFormSet.NINVDISAP = loFormSet.Ariaform1.lnDisc.Value      && approved discount = offered discount
    ENDIF

    DO CASE
      *** If payment method is not cash payments,
      CASE loFormset.Ariaform1.cboVendPmeth.Value <> 'H'
        *** If the bank code is empty,
        IF EMPTY(loFormSet.CBNKCODE)
          llBnkChkChng = .F.
          IF !EMPTY(APVENDOR.CBNKCODE)
            loFormSet.CBNKCODE = APVENDOR.CBNKCODE && Assign the bank code from vendor.
            loFormSet.CCHKACCT = APVENDOR.CCHKACCT && Assign the check code from vendor.
            llBnkChkChng = .T.
          ELSE
            IF !EMPTY(loFormset.Ariaform1.cboDivision.Value) .AND. SEEK(loFormset.Ariaform1.cboDivision.Value, 'APDIV') .AND. !EMPTY(APDIV.CBNKCODE)
              loFormSet.CBNKCODE = APDIV.CBNKCODE  && Assign the bank code from division.
              loFormSet.CCHKACCT = APDIV.CCHKACCT  && Assign the bank code from division.
              llBnkChkChng = .T.
            ELSE
              loFormSet.CBNKCODE = APSETUP.CBNKCODE  && Assign the bank code from division.
              loFormSet.CCHKACCT = APSETUP.CCHKACCT  && Assign the bank code from division.
              llBnkChkChng = .T.
            ENDIF
          ENDIF
          IF llBnkChkChng

            IF SEEK(loFormSet.CBNKCODE+loFormSet.CCHKACCT,'APCHECKS')
              *E300296,4 Check if the checking account has the same
              *E300296,4 currency as that of the invoice, if not,
              *E300296,4 get the first checking account using
              *E300296,4 the invoice currency, if any is found,
              *E300296,4 or clear al fields if none is found.
              *laData[23] = APCHECKS.cChkGlAcc
              IF APCHECKS.cCurrCode = loFormset.Ariaform1.kbCurrCode.Keytextbox.Value
                loFormSet.CCHKGLACC = APCHECKS.cChkGlAcc
              ELSE
                SELECT APCHECKS
                LOCATE REST WHILE cBnkCode = loFormSet.CBNKCODE;
                       FOR cCurrCode = loFormset.Ariaform1.kbCurrCode.Keytextbox.Value
                IF FOUND()
                  loFormSet.CCHKACCT = APCHECKS.cChkAcct
                  loFormSet.CCHKGLACC = APCHECKS.cChkGlAcc
                ELSE
                  loFormSet.CBNKCODE = SPACE(8)
                  loFormSet.CCHKACCT = SPACE(12)
                  loFormSet.CCHKGLACC = loFormSet.ap1.lcEmptyAcc
                ENDIF
              ENDIF
              *E300296,4 end.
            ENDIF

          ENDIF
        ENDIF

        *** Check account
        IF EMPTY(loFormSet.CCHKGLACC)
          loFormSet.CCHKGLACC = loFormSet.ap1.lcEmptyAcc
        ENDIF
        *lcAccDesc   = IIF(loFormSet.ap1.llApGlLink,;
                    ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,loFormSet.CCHKGLACC,;
                    lcLinkChar.cAcctCode,"ACCTCODE")),' ')

        *E300296,4 Add a state for Checking account, and G/L checking account
        *E300296,4 that is to be disable if the bank code is empty
        lcChkActStat = IIF(EMPTY(loFormSet.CBNKCODE), 'DISABLE', 'ENABLE')
        *E300296,4 end.

        *E300683,1 Call *.SPR from screens directory
        *DO APRCRA1.SPR
        *DO (gcScrDir + gcWinAppl + '\APRCRA1.SPR')
        DO FORM (lcRunScx) WITH loFormSet,.T.

        *E300683,1 end
      *** Case payment method is cash payment :
      OTHERWISE
        IF EMPTY(STRTRAN(STRTRAN(loFormSet.CCHKGLACC,'-'),'0'))     && If cash account is empty
          loFormSet.CCHKGLACC = IIF(!EMPTY(APVENDOR.CCASHACCT),;
                           APVENDOR.CCASHACCT,;
                           IIF(!EMPTY(loFormset.Ariaform1.cboDivision.Value) .AND. SEEK(loFormset.Ariaform1.cboDivision.Value, 'APDIV');
                               .AND. !EMPTY(APDIV.CCASHACCT),;
                                APDIV.CCASHACCT,;
                                APSETUP.CCASHACCT))
        ENDIF
        *lcAccDesc   = IIF(loFormSet.ap1.llApGlLink,;
                          ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,loFormSet.CCHKGLACC,;
                          lcLinkChar.cAcctCode,"ACCTCODE")),' ')

        *E300683,1 Call *.SPR from screens directory
        * DO APRCRA2.SPR
        *DO (gcScrDir + gcWinAppl + '\APRCRA2.SPR')
        DO FORM (lcRunScx) WITH loFormSet,.F.
        *E300683,1 end

    ENDCASE
  ENDIF
ELSE
  *lcAccDesc     = IIF(loFormSet.ap1.llApGlLink,;
                      ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,loFormSet.CCHKGLACC,;
                      lcLinkChar.cAcctCode,"ACCTCODE")),' ')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcApprv       = LANG_APRCRIN_lcTOK     && push button prompt (pbApprove)
lcApprv       = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTOK,loFormSet.GetHeaderText("LANG_APRCRIN_lcTOK",loFormSet.HeaderAlias))     && push button prompt (pbApprove)
*N000682,1 11/20/2012 MMT Globlization changes[End]


  *E300296,4 Add a state for Checking account, and G/L checking account
  *E300296,4 that is to be disable if View mode. (lcChkActStat)
  STORE 'DISABLE' TO lcApprvStat, lc1099Stat, lcChkActStat
  *E300296,4 end.

  IF EMPTY(loFormSet.CCHKGLACC)
    loFormSet.CCHKGLACC = loFormSet.ap1.lcEmptyAcc
  ENDIF
  IF loFormset.Ariaform1.cboVendPmeth.Value = 'H'

     *E300683,1 Call *.SPR from screens directory
     * DO APRCRA2.SPR
     *DO (gcScrDir + gcWinAppl + '\APRCRA2.SPR')
     DO FORM (lcRunScx) WITH loFormSet,.F.
     *E300683,1 end
  ELSE
    *E300683,1 Call *.SPR from screens directory
    * DO APRCRA1.SPR
    *DO (gcScrDir + gcWinAppl + '\APRCRA1.SPR')
    DO FORM (lcRunScx) WITH loFormSet,.T.
    *E300683,1 end

  ENDIF
ENDIF
ENDWITH
*** Total approved amount = approved to pay + approved discount
***                         + approved adjustment.
*!*	lnTotAppPay = loFormSet.NINVAMTAP + loFormSet.NINVDISAP + loFormSet.NINVADJAP
*!*	loFormset.Ariaform1.txtTotApproved.Value = lnTotAppPay
*!*	loFormset.Ariaform1.txtTotApproved.Refresh()
*=lfRefresh(gcBaseWind)

*- End of lfvAprovPay.

*!**************************************************************************
*!
*!      Function: lfvClrApr
*!
*!**************************************************************************
* Validation function of the Clear approval  push button of the
* Approved for payment screen
FUNCTION lfvClrApr
PARAMETERS loAprFormSet
loFormSet = loAprFormSet.CallingForm

loFormSet.CBNKCODE = SPACE(8)
loFormSet.CCHKACCT = SPACE(12)
loFormSet.CCHKGLACC = loFormSet.ap1.lcEmptyAcc
loFormSet.NINVAMTAP = 0
loFormSet.NINVDISAP = 0
loFormSet.NINVADJAP = 0
loFormSet.NINVA1099 = 0

lnTotAppPay = loFormSet.NINVAMTAP + loFormSet.NINVDISAP + loFormSet.NINVADJAP
loFormset.Ariaform1.txtTotApproved.Value = lnTotAppPay
loFormset.Ariaform1.txtTotApproved.Refresh()

*- End of lfvClrApr.
************************************************************
*! Name      : lfvCanPay
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/29/2012
*! Purpose   : lfvCanPay
************************************************************
FUNCTION lfvCanPay
PARAMETERS loAprFormSet

*- End of lfvCanPay.
*!**************************************************************************
*!
*!      Function: lfvOkApr
*!
*!**************************************************************************
* Validation function of the ok push button of the Approved for payment.
*
FUNCTION lfvOkApr
PARAMETERS loAprFormSet
loFormSet = loAprFormSet.CallingForm
PRIVATE lcEmptyObj, llOK
lnRet = 1
lcEmptyObj = ' '
llOk       = .T.

IF !loFormSet.ActiveMode = 'V'   && In case of view mode ®  Ok  ¯ is terminating

  DO CASE

    CASE loFormSet.NINVAMTAP = 0
      *** Message : " You have to enter the approved to pay amount."
      ***                       ®  Ok  ¯
      =gfModalGen("TRM04022B00000","DIALOG")
      loAprFormSet.AriaForm1.txtinvamtap.SetFocus
      llOK = .F.
    CASE loFormset.Ariaform1.cboVendPmeth.Value = 'H' .AND. EMPTY(STRTRAN(STRTRAN(loFormSet.CCHKGLACC,'-'),'0'))
      *** Message : " You have to enter the ð account.     "
      ***                             ®  Ok  ¯
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04020B00000","DIALOG",LANG_APRCRIN_lcTPayAcct)
=gfModalGen("TRM04020B00000","DIALOG",LANG_APRCRIN_lcTPayAcct)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      loAprFormSet.AriaForm1.GLActCode.Keytextbox.SetFocus
      lnRet = 0
      llOK = .F.
    CASE loFormset.Ariaform1.cboVendPmeth.Value <> 'H'
      WITH loAprFormSet.Ariaform1
      lcEmptyObj = IIF(EMPTY(.BankChk.kbBanks.Keytextbox.Value),.BankChk.kbBanks.Keytextbox,;
                       IIF(EMPTY(.BankChk.kbChkAccount.Keytextbox.Value),.BankChk.kbChkAccount.Keytextbox,;
                          IIF(EMPTY(STRTRAN(STRTRAN(.GLActCode.Keytextbox.Value,'-'),'0')),;
                          .GLActCode.Keytextbox,' ')))
      ENDWITH
  ENDCASE
  IF type('lcEmptyObj')='O'
    llOK = .F.
    *** Message : " You have to enter the bank code,the  "
    ***           " checking account,and the GL account. "
    ***           "                  ®  Ok  ¯              "
    =gfModalGen("TRM04021B00000","DIALOG")
    **_CUROBJ = OBJNUM(&lcEmptyObj)
    lcEmptyObj.Setfocus()
  ELSE

   *E303064,1 TMI 02/29/2012 [Start]
   IF loFormset.Ariaform1.cboVendPmeth.Value<>'H' AND !lfvBnkChk(loAprFormSet)
     llOk       = .F.
   ENDIF
 *E303064,1 TMI 02/29/2012 [End  ]

  ENDIF
ENDIF
IF llOK
  WITH loAprFormSet.Ariaform1
    loFormset.CBNKCODE  =   .BankChk.kbBanks.Keytextbox.Value
    loFormset.CCHKACCT  =   .BankChk.kbChkAccount.Keytextbox.Value
    loFormset.CCHKGLACC =   .GLActCode.Keytextbox.Value

    loFormset.NINVAMTAP =   .txtinvamtap.Value
    loFormset.NINVDISAP =   .txtinvdisap.Value
    loFormset.NINVADJAP =   .txtinvadjap.Value
    loFormset.NINVA1099 =   .txtinva1099.Value
  ENDWITH

  lnTotAppPay = loFormSet.NINVAMTAP + loFormSet.NINVDISAP + loFormSet.NINVADJAP
  loFormset.Ariaform1.txtTotApproved.Value = lnTotAppPay
  loFormset.Ariaform1.txtTotApproved.Refresh()

ENDIF

RETURN llOk

*- End of lfvOkApr.

************************************************************
*! Name      : lfv1099Aprov
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/26/2012
*! Purpose   :
************************************************************
FUNCTION lfv1099Aprov  && FUNCTION lfvData_27
PARAMETERS loAprFormSet,loFld
loFormSet = loAprFormSet.CallingForm
lnRet = 1
* Validation function of 1099 amount.
*
*** If the approved 1099 amount is greater than the approved to pay
*** amount,
*** or, the approved 1099 amount is negative,
*** present the following message(s) and return the old value
*** of the field
IF loFld.Value <> loFld.OldValue .AND.;
  (loFld.Value > 0 .AND. loFld.Value > loFormSet.NINVAMTAP .AND.;
   gfModalGen("TRM04025B00000", "DIALOG",;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcT1099Am,loFormSet.GetHeaderText("LANG_APRCRIN_lcT1099Am",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTAprPay,loFormSet.GetHeaderText("LANG_APRCRIN_lcTAprPay",loFormSet.HeaderAlias))) > 0 ;
   .OR. loFormSet.NINVA1099 < 0 .AND. gfModalGen("TRM04087B00000","DIALOG") > 0)
 loFld.Value = loFld.OldValue
 *_CUROBJ  = *_CUROBJ
  lnRet = 0
ENDIF
RETURN lnRet

*- End of lfv1099Aprov.

*!**************************************************************************
*!
*!      Function: lfvAmounts
*!
*!**************************************************************************
* Valid function of the discount.
* this is called from the APRCRAPR.SCX
*
FUNCTION lfvAmounts
PARAMETERS loAprFormSet,loFld
loFormSet = loAprFormSet.CallingForm

PRIVATE lcCurObj,lnRet
lnRet = 1

lcCurObj   = SYS(18)
WITH loAprFormSet.Ariaform1
IF loFld.Value > 0
  *** If the total approved amount (approved amount + approved discount+
  *** approved adjustment) > the invoice amount , present the following
  *** message and return the old value of the object
*  IF loFormSet.NINVAMTAP+loFormSet.NINVDISAP+loFormSet.NINVADJAP > loFormSet.Ariaform1.txtAmount.Value
  IF .txtinvamtap.Value+.txtinvdisap.Value+.txtinvadjap.Value > loFormSet.Ariaform1.txtAmount.Value
    *** Message : " Total approved amount can not be greater than "
    ***           " the open invoice amount.                              "
    ***           "                       ®  Ok  ¯                  "
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04015B00000","DIALOG",LANG_APRCRIN_lcTAprAmnt+"|"+LANG_APRCRIN_lcTInvoice)
=gfModalGen("TRM04015B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTAprAmnt,loFormSet.GetHeaderText("LANG_APRCRIN_lcTAprAmnt",loFormSet.HeaderAlias))+"|"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTInvoice,loFormSet.GetHeaderText("LANG_APRCRIN_lcTInvoice",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *&lcCurObj = lnOldVal
    *_CUROBJ   = *_CUROBJ
    loFld.Value = loFld.OldValue
    lnRet = 0
  ENDIF
ELSE
  *** Check if negative entries are accepted
  IF loFld.Value < 0 .AND. gfModalGen("TRM04087B00000","DIALOG") > 0
    *&lcCurObj = lnOldVal
    *_CUROBJ   = *_CUROBJ
    loFld.Value = loFld.OldValue
    lnRet = 0
  ENDIF
ENDIF
ENDWITH
RETURN lnRet
*- End of lfvAmounts.

*!**************************************************************************
*!
*!      Function: lfvTaxCode
*!
*!**************************************************************************
FUNCTION lfvTaxCode
PARAMETERS loFormSet,llJustShow, lcStatOfDisp
PRIVATE lcCurAlias, lcOldTax, lcOldAcct

lcTmpDist = loFormSet.lcTmpDist
lcOldTax    = &lcTmpDist..cTaxCode
lcOldAcct   = &lcTmpDist..cApDGlAct

WITH loFormSet
IF !llJustShow
  DO CASE
    CASE _DOS
      loFormSet.lcTaxCode =gfActPop(0,11,5,44,'SYCCODES','cCode_No','cDiscrep',@lcTaxDes)

    CASE _WINDOWS

      *E300643,1 Change this lines for the changes we have made
      *          to SYCCODES [Begin]
      *lcTaxDes  = SYCCODES.cdiscrep
      *lcTaxCode = SYCCODES.cCode_No
      loFormSet.lcTaxDes  = CODES.cdiscrep
      loFormSet.lcTaxCode = CODES.cCode_No
      *E300643,1 Change this lines [End]

      *SHOW GET lcTaxDes
  ENDCASE
ENDIF

IF !EMPTY(loFormSet.lcTaxCode)

  *E300643,1 Change this line for the changes we have made
  =lfGetCodeRltFld(loFormSet,'laTaxCode','CTAXCODE',loFormSet.lcTaxCode)

  *E300643,1 Change this line [End]
  .lcApDGlAct = SUBSTR(.lcApDGlAct,1,loFormSet.lnApsAcLen)
  .lcApDGlAct = IIF(!EMPTY(STRTRAN(STRTRAN(.lcApDGlAct,'-'),'0')),;
                       .lcApDGlAct, .ap1.lcEmptyAcc)
  *** If there is a G/L link, validate the G/L input account,
  *** from the chart of accounts.
  *** If it is not found, present the following message and
  *** reset tax code to N/A
  *** Message :    "    G/L input account ð  for tax code ð     "
  ***              "    is not found in the chart of accounts.  "
  ***              "    You cannot select this tax code.        "
  IF !llJustShow
    IF loFormSet.ap1.llApGlLink .AND.;
      !SEEK(.lcApDGlAct, 'lcLinkChar')
      =gfModalGen("TRM04091B00000","DIALOG",;
                   ALLTRIM(lcApDGlAct)+;
                   '|'+ALLTRIM(lcTaxDes))
      IF loFormSet.lcTaxCode <> lcOldTax
        .lcApDGlAct  = lcOldAcct
        .lcTaxCode   = lcOldTax

        *E300643,1 Change this line for the changes we have made
        *          to (gfCodDes) [Begin]
        *lcTaxDes    = gfCodDes(lcOldTax)
        .lcTaxDes    = gfCodDes(lcOldTax , 'CTAXCODE')
        *E300643,1 Change this line [End]

      ELSE
        .lcApDGlAct = .lcDistAcct
        .lcTaxCode  = SPACE(6)
        *: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004][Start]
        *.lcTaxDes   = .ap1.lcEmptyCode
        .lcTaxDes   = loFormSet.lcEmptyCode
        *: B610245,1 HIA 02/14/2014  Aria4xp - AP - Recurring Payable [T20130207.0004][End]
      ENDIF
      *SHOW GET lcTaxDes

    ENDIF
  ENDIF
ENDIF
IF !llJustShow
  lcCurAlias = ALIAS()
  SELECT (.lcTmpDist)
  REPLACE cTaxCode  WITH .lcTaxCode,;
          cApdGlAct WITH .lcApdGlAct,;
          cStatus   WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
  SELECT (lcCurAlias)
ENDIF
ENDWITH
lcAcctStat = IIF(TYPE('lcStatOfDisp') = 'C' .AND. !EMPTY(lcStatOfDisp),;
                lcStatOfDisp, IIF(loFormSet.lnDistLines > 0 .AND. ;
                (loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A');
                .AND. EMPTY(loFormSet.lcTaxCode) , 'ENABLE', 'DISABLE'))
*!*	*SHOW GET lcApdGlAct  &lcAcctStat
*!*	*SHOW GET ibApGlAcc   &lcAcctStat
*!*	*SHOW GET lcApDGlDesc &lcAcctStat
*!*	IF _DOS
*!*	  =lfRefresh(lcRcrChld3)
*!*	ElSE
*!*	  =gfUpdate()
*!*	  DEACTIVATE POPUP puTaxDes
*!*	ENDIF
*- End of lfvTaxCode.

*!**************************************************************************
*!
*!      Function: lfvDistAmnt
*!
*!**************************************************************************
FUNCTION lfvDistAmnt
PARAMETERS loAPDISTFormSet
loFormSet = loAPDISTFormSet.CallingForm

PRIVATE lcCurAlias
lcTmpDist = loFormSet.lcTmpDist
*** Adjust totals
lnApdAmnt = loAPDISTFormSet.Ariaform1.txtlnAPDAmnt.Value
loFormSet.lnDistAmnt  = loFormSet.lnDistAmnt - &lcTmpDist..nAPDAmnt + lnAPDAmnt
lcCurAlias  = ALIAS()
SELECT (lcTmpDist)
REPLACE nAPDAmnt WITH lnAPDAmnt,;
        cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
SELECT (lcCurAlias)
*=lfRefresh(lcRcrChld1)
*- End of lfvDistAmnt

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*  Local delete procedure
PROCEDURE lpDelScr
PARAMETERS loFormSet

*** Delete details
lcInvADtEx = loFormSet.lcInvADtEx
SELECT APINVADT
SCATTER MEMVAR MEMO BLANK
SCAN FOR &lcInvADtEx = 'R'+loFormSet.Ariaform1.kbRecurringCode.Keytextbox.Value
  GATHER MEMVAR MEMO
  DELETE
ENDSCAN
=gfTableUpdate()

*** Then delete the header record
SELECT APINVAHD
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE
=gfTableUpdate()

*** Return to "SELECT" mode
loFormSet.ChangeMode('S')

*- End of lpDelScr.

*!**************************************************************************
*!
*!      Function: lfDispObjct
*!
*!**************************************************************************
* Valid function for the objects display in the child screen.
*
FUNCTION lfDispObjct
PARAMETERS loFormSet,lcStatOfDisp
PRIVATE lnElemNum

lcStatOfDisp = IIF(TYPE('lcStatOfDisp') <> 'C'," ",lcStatOfDisp)

IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
  SELECT(loFormSet.lcTmpDist)
ELSE
  SELECT APINVADT
ENDIF
loFormSet.SlctdAlias = ALIAS()


loFormSet.lcApdGLAct  = IIF(EMPTY(cApdGLAct),lcEmptyAcc,cApdGLAct)
*lcAPDGlDesc = IIF(loFormSet.ap1.llApGlLink,;
                  ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,lcApdGLAct,;
                  lcLinkChar.cAcctCode,"ACCTCODE")),' ')
lnApDAmnt   = nApDAmnt
loFormSet.lcTaxCode   = cTaxCode

lcTaxDes    = gfCodDes(loFormSet.lcTaxCode , 'CTAXCODE')

IF loFormSet.lnDistLines > 0
  IF !EMPTY(lcStatOfDisp)
    STORE lcStatOfDisp TO loFormSet.lcTaxStat, loFormSet.lcAmntStat
  ELSE
    loFormSet.lcTaxStat = IIF(APVENDOR.cTaxType = 'L' .AND. !loFormSet.ActiveMode = 'V',;
                    'ENABLE', 'DISABLE')
    loFormSet.lcAmntStat= IIF(!loFormSet.ActiveMode = 'V', 'ENABLE', 'DISABLE')
  ENDIF
ELSE
  STORE 'DISABLE' TO loFormSet.lcTaxStat, loFormSet.lcAmntStat
ENDIF

*** Account states depend on the tax type, comes from tax code validation
=lfvTaxCode(loFormSet,.T.,lcStatOfDisp)

*!*	*SHOW GET lnApdAmnt   &lcAmntStat
*!*	*SHOW GET lcTaxDes    &lcTaxStat    && Windows popup
*!*	*SHOW GET ibTaxCode   &lcTaxStat    && Dos     popup

*- End of lfDispObjct.

*!**************************************************************************
*!
*!      Function: lfvAccounts
*!
*!**************************************************************************
*
FUNCTION x_lfvAccounts
PARAMETERS lcObjName, lcDescName
PRIVATE lcAcct, lcAcctDesc

lcAcct     = EVALUATE(lcObjName)
lcAcctDesc = SPACE(60)
IF loFormSet.ap1.llApGlLink

  *** Get a new account and its description
  IF !lfApAcs(@lcAcctDesc,llBrowse)
    &lcObjName  = lcOldVal
    &lcDescName = lcOldDesc
    *_CUROBJ    = *_CUROBJ
  ELSE
    &lcDescName = lcAcctDesc
  ENDIF
  *SHOW GET (lcDescName)
ELSE
  IF '?' $ lcAcct .OR. EMPTY(STRTRAN(STRTRAN(lcAcct,'-'),'0'))
    &lcObjName = lcOldVal
    *_CUROBJ    = *_CUROBJ
  ENDIF
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      FUNCTION : lfvNew
*!
*!**************************************************************************
FUNCTION lfvNew
PARAMETERS loAPDISTFormSet
loFormSet = loAPDISTFormSet.CallingForm

*** Look for an appropriate place to default the distribution account,
*** but certainly not with every new line
WITH loFormSet
IF EMPTY(STRTRAN(STRTRAN(.lcDistAcct,'-'),'0'))
  IF !EMPTY(APVENDOR.CEXPACCT)
    .lcDistAcct = APVENDOR.CEXPACCT
  ELSE
    IF SEEK(.Ariaform1.cboDivision.Value,'APDIV') .AND. !EMPTY(APDIV.CAPACCT)
      .lcDistAcct = APDIV.CEXPACCT
    ELSE
      .lcDistAcct = APSETUP.CEXPACCT
    ENDIF
  ENDIF
ENDIF
ENDWITH

*** Increment the number of lines
loFormSet.lnDistLines = loFormSet.lnDistLines + 1

*** Default the new record with the undistributed amount
lcTmpDist = loFormSet.lcTmpDist
WITH loFormSet.Ariaform1
INSERT INTO(lcTmpDist);
    (cAutMType, cAutMCode, nAPDAmnt, cAPDGlAct, nAPDLinNo, cStatus);
    VALUES('R',.kbRecurringCode.Keytextbox.Value, .txtAmount.Value - loFormSet.lnDistAmnt, ;
    loFormSet.lcDistAcct, loFormSet.lnDistLines, 'A')
ENDWITH
=gfAdd_Info(loFormSet.lcTmpDist)

IF APVENDOR.CTAXTYPE = 'L'
  * Old : *_CUROBJ = IIF(_DOS, OBJNUM(ibTaxCode), OBJNUM(lcTaxDes))
  loAPDISTFormSet.AriaForm1.cbotaxCode.SetFocus()
ELSE
  * Old : *_CUROBJ = OBJNUM(lcApdGLAct)
  loAPDISTFormSet.AriaForm1.glActCode.Keytextbox.SetFocus()
ENDIF
loFormSet.lnDistAmnt = loFormSet.lnDistAmnt + &lcTmpDist..nAPDAmnt

lcRemStat  = "ENABLE"
STORE "DISABLE" TO lcVendStat, lcTypesStat
*SHOW GET laData[4] DISABLE
*SHOW GET lcCompany DISABLE
*SHOW GET lcPhone   DISABLE
*SHOW GET ibVendor  DISABLE
*SHOW GET ibCompany DISABLE
*SHOW GET ibPhone   DISABLE
*SHOW GET ibType    DISABLE
*SHOW GET puType    DISABLE
*SHOW GET pbRem     ENABLE

WITH loFormSet.Ariaform1
  .KBVendCode.ENABLED = lcVendStat = 'ENABLE'
  .KBVendPhone.ENABLED = lcVendStat = 'ENABLE'
  .KBVendCompany.ENABLED = lcVendStat = 'ENABLE'
  .cboType.Enabled = lcVendStat = 'ENABLE'
ENDWITH
WITH loAPDISTFormSet
  .AriaForm1.cmdRemove.Enabled = .T.
  .AriaForm1.grdDist.Refresh()
  .AriaForm1.grdDist.AfterRowColChange()
ENDWITH
*SHOW WINDOW (lcBrowsTtl) REFRESH SAME

=lfDispObjct(loFormSet)
*- End of lfvNew.

*!**************************************************************************
*!
*!      FUNCTION : lfvRem
*!
*!**************************************************************************
FUNCTION lfvRem
PARAMETERS loAPDISTFormSet
loFormSet = loAPDISTFormSet.CallingForm
lcTmpDist = loFormSet.lcTmpDist

*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1

  loFormSet.lnDistLines = loFormSet.lnDistLines - 1

  SELECT(lcTmpDist)
  loFormSet.lnDistAmnt  = loFormSet.lnDistAmnt - nApdAmnt
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

  *SHOW WINDOW (lcBrowsTtl) REFRESH
  IF loFormSet.lnDistLines = 0
    *** Enable the vendor selection fields only in Add mode
    IF loFormSet.ActiveMode = 'A'
      lcVendStat   = "ENABLE"
      *SHOW GET laData[4]  ENABLE
      *SHOW GET lcCompany  ENABLE
      *SHOW GET lcPhone    ENABLE
      *SHOW GET ibVendor   ENABLE
      *SHOW GET ibCompany  ENABLE
      *SHOW GET ibPhone    ENABLE
      .KBVendCode.ENABLED = lcVendStat   = "ENABLE"
      .KBVendPhone.ENABLED = lcVendStat   = "ENABLE"
      .KBVendCompany.ENABLED = lcVendStat   = "ENABLE"
      .cboType.Enabled = lcVendStat   = "ENABLE"
    ENDIF
    lcTypesStat  = "ENABLE"
    lcRemStat    = "DISABLE"
    *SHOW GET ibType     ENABLE
    *SHOW GET puType     ENABLE
    *SHOW GET pbRem     DISABLE
    loAPDISTFormSet.AriaForm1.cmdRemove.Enabled = lcRemStat  = "ENABLE"

    =lfDispObjct(loFormSet,'DISABLE')
  ELSE
    =lfDispObjct(loFormSet)
  ENDIF
ENDIF
loAPDISTFormSet.AriaForm1.grdDist.Refresh()
*- End of lfvRem.

*!**************************************************************************
*!
*!      Function : lfvAPDGlAct
*!
*!**************************************************************************
*  Valid function for the get field 'lcAPDGlAct' in the distribution
*  lines screen
FUNCTION lfvAPDGlAct
PRIVATE lcCurAlias

=lfvAccounts('lcApdGLAct','lcApDGLDesc')
lcCurAlias   = ALIAS()
SELECT (lcTmpDist)
REPLACE &lcTmpDist..cApdGLAct WITH ;
          IIF(EMPTY(STRTRAN(STRTRAN(lcApdGLAct,'-'),'0')),;
              SPACE(loFormSet.lnApsAcLen), lcApDGlAct),;
        &lcTmpDist..cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
SELECT (lcCurAlias)


************************************************************
*! Name      : lfvFreq
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/26/2012
*! Purpose   : Valid function for Frequency
*************************************************************
FUNCTION lfvFreq
PARAMETERS loFld
*** If the field is empty, or,
*** if the entered value is not greater than zero,
***     Message :  "   ð should be greater than ð.   "
***                            ®   OK   ¯
*** restore the old value of the field
LOCAL lnRet
lnRet = 1
IF loFld.Value <> loFld.OldValue .AND.;
((loFld.Value < 1 .AND. gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTDur,loFormSet.GetHeaderText("LANG_APRCRIN_lcTDur",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APRCRIN_lcTZero,loFormSet.GetHeaderText("LANG_APRCRIN_lcTZero",loFormSet.HeaderAlias))) > 0))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFld.Value = loFld.OldValue
  *_CUROBJ = *_CUROBJ
  lnRet = 0
ENDIF
RETURN lnRet

*- End of lfvFreq.

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[21] (bank code), and laData[22]
* (checking account code)
*
FUNCTION lfvBnkChk
PARAMETERS loAprFormSet
loFormSet = loAprFormSet.CallingForm

LOCAL lnSlct
lnSlct = SELECT(0)
*E300296,4 Add parameters to lfBnkChk to restrict bank\checking account
*E300296,4 selection to those using the same currency as that of the
*E300296,4 invoice.
*IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse)

*!*	IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, '','', '', laData[34])
*!*	  RETURN 1
*!*	*E300296,4 Enable lcAccDesc if a valid bank is selected.
*!*	ELSE
*!*	  SHOW GET lcAccDesc ENABLE
*!*	*E300296,4 end.
*!*	ENDIF
WITH loAprFormSet.AriaForm1.BankChk
lcKey = .kbBanks.Keytextbox.Value+.kbChkAccount.Keytextbox.Value
lcCurr = loFormset.Ariaform1.kbCurrCode.Keytextbox.Value
IF SEEK(lcKey,'APCHECKS','BANKCHECK')   && CBNKCODE+CCHKACCT
  SELECT APCHECKS
  LOCATE REST WHILE CBNKCODE+CCHKACCT = lcKey FOR CCURRCODE = lcCurr
  IF !FOUND()
          =gfModalGen('INM04155B00000','DIALOG',;
                      ALLTRIM(lcCurr) + '|'+;
                      ALLTRIM(.kbBanks.Keytextbox.Value)+'-'+ALLTRIM(.kbChkAccount.Keytextbox.Value ))
    SELECT (lnSlct)
    RETURN .F.
  ENDIF
ENDIF
ENDWITH
SELECT (lnSlct)

*MT
FUNCTION lfvremline
PARAMETERS loAPDISTFormSet
loFormSet = loAPDISTFormSet.CallingForm
lcTmpDist = loFormSet.lcTmpDist
*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
  
  loFormSet.lnDistLines = loFormSet.lnDistLines - 1
  SELECT(lcTmpDist)
  loFormSet.lnDistAmnt = loFormSet.lnDistAmnt - &lcTmpDist..nAPDAmnt
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
  IF EOF(lcTmpDist) OR BOF(lcTmpDist)
    loAPDISTFormSet.AriaForm1.cmdRemove.Enabled = .F.
  ELSE
    loAPDISTFormSet.AriaForm1.cmdRemove.Enabled = .T.
  ENDIF
  loAPDISTFormSet.AriaForm1.grdDist.Refresh()
  loAPDISTFormSet.AriaForm1.grdDist.AfterRowColChange()
  
ENDIF
*MT
*: B611084,1 MMT 11/25/2015 User cannot add Distribution lines in AP recurring payables screen[T20151104.0021][Start]
************************************************************
*! Name      : lfvDistAcct
*! Developer : MMT - Mariam Mazhar
*! Date      : 11/25/2015
*! Purpose   : Valid function for GL account to update temp.
*************************************************************
FUNCTION lfvDistAcct
PARAMETERS loAPDISTFormSet
loFormSet = loAPDISTFormSet.CallingForm 

PRIVATE lcCurAlias
lcTmpDist = loFormSet.lcTmpDist
lcApdAct = loAPDISTFormSet.Ariaform1.glActCode.KeytextBox.Value
lcCurAlias  = ALIAS()
SELECT (lcTmpDist)
REPLACE cAPDGlAct WITH lcApdAct,;
        cStatus  WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)           
SELECT (lcCurAlias)        
loAPDISTFormSet.AriaForm1.grdDist.refresh()
*: B611084,1 MMT 11/25/2015 User cannot add Distribution lines in AP recurring payables screen[T20151104.0021][End]