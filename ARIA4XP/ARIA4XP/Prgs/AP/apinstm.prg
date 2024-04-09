*:************************************************************************
*:  Program File: APINSTM.PRG
*:  Desc.       : Installment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/14/2012
*:  Reference   : E303066,1
*:************************************************************************
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004]
*!B609980,1 SAB 06/27/2012 Fix error in message when selecting hold vendor [Media R12 Issues]
*- Call the screen
lcRunScx = lfGetScx("AP\APINSTM.scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2012
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
*! Date      : 02/14/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome+oAriaapplication.ActiveModuleID
SET PROCEDURE TO (lcPath+'\APMAIN.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES('APINSTM')

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- Define variables 
=lfDefineVars(loFormSet)

loFormSet.lcTKeyCode  = ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CVENDCODE',;
                        SYDFIELD.cFld_Name,'CFLD_NAME')) + '\' + ;
                ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CINVNO',;
                        SYDFIELD.cFld_NAme,'CFLD_NAME')) + ' : '
*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "'I'+CAUTMCODE"
  .cBrowseIndexFields     = "CAUTMTYPE+CAUTMCODE"
  .cBrowseIndexName       = 'HTYPCOD'
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl 
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)
ENDWITH 

  *** Prepare Types array from SYDFIELD and get its maximum width
SELECT FISHD
GO TOP
IF !EOF()
  LOCATE REST FOR  cFisYstat = 'C'
  loFormSet.ldFisBgDt = IIF(FOUND(),dFisBgDat,{}) 
ENDIF

*!*	lnTypeLen  = gfGetVld('cAutMBase',@laInstType)  
*!*	lcType     = laInstType[1,1]
*!*	 
*!*	*** Prepare Duration array from SYDFIELD and get its maximum width
*!*	lnDurLen   = gfGetVld('cAutFUnit',@laDuration)
*!*	lcDuration = laDuration[1,1]   

*!*	*** Prepare Remit to array from SYDFIELD and get its maximum width
*!*	lnRemitLen = gfGetVld('cInvRemit',@laRemitTo)
*!*	lcRemitTo  = laRemitTo[1,1]   
*!*	  
*!*	*** Prepare Payment method array from SYDFIELD and get its maximum width
*!*	=gfGetVld('cVenPMeth',@laPayMeth)


*- fill the arrays of popup fields
=lfPopArrs('cAutMBase','laInstType')

  *** Prepare Duration array from SYDFIELD and get its maximum width
=lfPopArrs('cAutFUnit','laDuration')

  *** Prepare Remit to array from SYDFIELD and get its maximum width

=lfPopArrs('cInvRemit','laRemitTo')

*** Prepare Payment method array from SYDFIELD and get its maximum width
*** Remove 'Credit cards' (C) option from the array
=lfPopArrs('cVenPMeth','laPayMeth')

WITH loFormSet.Ariaform1
  .lcType.RowSource = 'Thisformset.laInstType'
  =lfColumnWidthes(.lcType)
  .lcDuration.RowSource = 'Thisformset.laDuration'
  =lfColumnWidthes(.lcDuration)
  .lcRemitTo.RowSource = 'Thisformset.laRemitTo'
  =lfColumnWidthes(.lcRemitTo)
  .lcPayMeth.RowSource = 'Thisformset.laPayMeth'
  =lfColumnWidthes(.lcPayMeth)
ENDWITH 

SELECT APINVAHD
SET FILTER   TO
SET RELATION TO
SET FILTER   TO cAutMType = "I"          
*SET RELATION TO laData[3] + laData[2] INTO APINVHDR ADDITIVE
*SET RELATION TO laData[2]             INTO APVENDOR ADDITIVE
SET RELATION TO cInvNo+cVendCode  INTO APINVHDR ADDITIVE 
SET RELATION TO cVendCode          INTO APVENDOR ADDITIVE

loFormSet.ChangeMode('S')


*- End of lfFormInit.
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
*! Date      : 03/12/2012
*! Purpose   : lfDefineVars
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))

lcScFields = 'CAUTMTYPE,CVENDCODE,CINVNO,CAUTMBASE,NAUTFREQ,CAUTFUNIT,CINVREF,DAUTSTGEN,'+;
             'DAUTNXGEN,NINVAMTAP,NINVA1099,CBNKCODE,CCHKACCT,CCHKGLACC'
=lfAddProp(loFormSet,'lcScFields',lcScFields)
=lfAddProp(loFormSet,'loScFields[1]','')
*- Define an array loFormSet.loScFields to map it to laData array
DIMENSION loFormSet.loScFields[OCCURS(',',lcScFields)+1]
WITH loFormSet.Ariaform1
  loFormSet.loScFields[ 1] = 'I'
  loFormSet.loScFields[ 2] = .KBVendCode.Keytextbox
  loFormSet.loScFields[ 3] = .kbInvoice.Keytextbox
  loFormSet.loScFields[ 4] = .lcType
  loFormSet.loScFields[ 5] = .txtFreq

  loFormSet.loScFields[ 6] = .lcDuration
  loFormSet.loScFields[ 7] = .lcRef
  loFormSet.loScFields[ 8] = .txtFirstInstDt
  loFormSet.loScFields[ 9] = .txtNextInstDt
  loFormSet.loScFields[10] = .txtInstallment

  loFormSet.loScFields[11] = .txt1099Amount
  loFormSet.loScFields[12] = .chkBankCodeAcct.kbBanks.Keytextbox
  loFormSet.loScFields[13] = .chkBankCodeAcct.kbChkAccount.Keytextbox
  loFormSet.loScFields[14] = .GLActCode.Keytextbox
ENDWITH 

lcVars = 'llBrowse    , llValidData , laDefProc[9]'
=lfAddProp(loFormSet,lcVars,.F.)

lcVars = 'lcStatDt8   , lcStatDt9   , lcStatDt11   , lcStatDt12  ,'+;
         'lcStatDt13  , lcStatDt14'
=lfAddProp(loFormSet,lcVars,'DISABLE')

lcVars =           'ldLastGen   , ldInvDate   , ldOldVal     , ldFisBgDt'
=lfAddProp(loFormSet,lcVars,{})

lcVars =           'lnTotAppPay , lnTerDiscD  , lnInvAmnt    , lnInvDisc   ,'+;
                   'lnOldVal    , lnTypeLen   , lnDurLen'
=lfAddProp(loFormSet,lcVars,0)

lcVars =           'lcCompany   , lcPhone     , lcType       , lcDuration  ,'+;
                   'lcRemitTo   , lcFactCode  , lcRmtComp    , lcRmtAddr1  ,'+;
                   'lcRmtAddr2  , lcRmtAddr3  , lcTermCode   , lcInvRef    ,'+;
                   'lcDivision  , lcPayPrio   , lcPayMeth    , lcOldVal    ,'+;
                   'lcSavExact  , lcPopCol    , lcFactCol    , lcPrompt    ,'+;
                   'lcEmptyCode , lcTKeyCode  , lcCurrCode'
=lfAddProp(loFormSet,lcVars,'')

=lfAddProp(loFormSet,'llSYDFIELD,  llFISHD',.F.)

=lfAddProp(loFormSet,'laInstType[1],laDuration[1],laRemitTo[1],laPayMeth[1]','')

lcFile_ttl = PROPER(ALLTRIM(LOOKUP(sydFiles.cFile_ttl,loFormSet.lcBaseFile,;
                 sydFiles.cFile_Nam)))
=lfAddProp(loFormSet,'lcFile_ttl',lcFile_ttl)


*- End of lfDefineVars.

************************************************************
*! Name      : lfUpdateData
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/12/2012
*! Purpose   : Update laData
************************************************************
FUNCTION lfUpdateData
PARAMETERS llData2Scr
LOCAL lnI
FOR lnI = 2 TO ALEN(loFormSet.loScFields)
  o = loFormSet.loScFields[lnI] 
  IF llData2Scr
    o.Value = laData[lnI]
  ELSE
    laData[lnI] = o.Value 
  ENDIF 
ENDFOR 
*- End of lfUpdateData.

************************************************************
*! Name      :  lfvVendor
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/05/2012
*! Purpose   : Valid function for Vendor Code
************************************************************ 
FUNCTION lfvVendor
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
    *!B609980,1 SAB 06/27/2012 Fix error in message when selecting hold vendor [Start]
    *=gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(lcVendCode))
    =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(APVENDOR.CVENDCODE))
    *!B609980,1 SAB 06/27/2012 Fix error in message when selecting hold vendor [End]
    .KBVendCode.KeyTextBox.VALUE    = lcVendCode 
    .KBVendPhone.KeyTextBox.VALUE   = lcVPhone   
    .KBVendCompany.KeyTextBox.VALUE = lcVendComp 
    RETURN .F.
  ENDIF 
  ENDWITH 
  
  RETURN .T.
ELSE 

  RETURN .F.    
ENDIF 
 *- End of  lfvVendor.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011 
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

*- if this function is called from the standard DODEFAUL then these variables are still not defined
IF TYPE('loFormset.laInstType')='U'
  RETURN
ENDIF   

*laData[1] = 'I'        && recurring invoices only.

SELECT APINVAHD
IF RECNO() <= RECCOUNT()    && to refresh relations between files.
  GO RECNO()
ELSE
  GO TOP  
ENDIF
DIMENSION laData[14]
DO CASE
  CASE loFormSet.ActiveMode = 'S'
    WITH loFormSet.Ariaform1
    .KBVendCode.KeyTextbox.Value = ''
    .KBVendCode.KeyTextbox.Valid()
    
    .KBVendCode.Enabled = .T.
    .KBVendPhone.Enabled = .T.
    .KBVendCompany.Enabled = .T.
    .kbInvoice.Enabled = .T.

    STORE  loFormSet.ap1.lcEmptyAcc TO laData[14]
    lcStatDt8   = 'DISABLE'
    lcStatDt9   = 'DISABLE'
    lcStatDt11  = 'DISABLE'    
    lcStatDt12  = 'DISABLE'
    lcStatDt13  = 'DISABLE'
    lcStatDt14  = 'DISABLE'
    
    .txtFirstInstDt.Enabled = .F.
    .txtNextInstDt.Enabled = .F.
    .txtInstallment.Enabled = .F.
    .txt1099Amount.Enabled = .F.
    .chkBankCodeAcct.Enabled = .F.
    .GLActCode.Enabled = .F.

    *.lcPrompt.Value    = 'GL acct.'
    *.lcCompany.Value   = SPACE(30)          && Company
    *.lcPhone.Value     = SPACE(16)          && phone
    .lcType.Value      = loFormSet.laInstType[1,1] 
    .lcDuration.Value  = loFormSet.laDuration[1,1]
    .lcRemitTo.Value   = loFormSet.laRemitTo[1,1]
    .ldLastGen.Value   = {}
    .ldInvDate.Value   = {}
    .kbFacCode.Keytextbox.Value = ''
    *.lcFactCol   = IIF(_DOS,'DISABLE COLOR ,,,,,,,,,N+/N*','DISABLE COLOR ,,,,,,,,,RGB(192,192,192,192,192,192)')
    .lcRmtComp.Value   = '' 
    .lcRmtAddr1.Value  = '' 
    .lcRmtAddr2.Value  = '' 
    .lcRmtAddr3.Value  = '' 
    .lcInvRef.Value    = ''
    .lcPayPrio.Value   = ''
    .lcPayMeth.Value   = ''
    .lnTotAppPay.Value = 0
    .lnTerDiscD.Value  = 0 
    .lnInvAmnt.Value   = 0 
    .lnInvDisc.Value   = 0 
    .lcTermCode.Value  = ''  &&loFormSet.ap1.lcEmptyCode
    .lcDivision.Value  = ''  &&loFormSet.ap1.lcEmptyCode

    .lcTermCode.Valid()
    .lcDivision.Valid()

    .kbCurrCode.Keytextbox.Value  = ''     && * 
    
    .lcType.Valid()
    .KBVendCode.KeyTextbox.Setfocus()
    ENDWITH 
    

  *CASE laScrMode[2] .OR. laScrMode[3]
  CASE loFormSet.ActiveMode $ 'V|E'

    WITH loFormSet
    *** Type
    lnElemNum = ASCAN(.laInstType,laData[4])
    lcType    = IIF(lnElemNum > 0, .laInstType[ASUBSCRIPT(.laInstType,lnElemNum ,1),1]," ")    

    *** Duration
    lnElemNum  = ASCAN(.laDuration,laData[6])
    lcDuration = IIF(lnElemNum > 0, .laDuration[ASUBSCRIPT(.laDuration,lnElemNum ,1),1]," ")    
    ENDWITH 

    *** AP account 
    IF EMPTY(laData[14])
      laData[14] = loFormSet.ap1.lcEmptyAcc
    ENDIF  

    lcObjectSt = IIF(loFormSet.ActiveMode = 'V','DISABLE','ENABLE')
    lcStatDt8  = 'DISABLE' 
    lcStatDt9  = lcObjectSt
    lcStatDt12 = lcObjectSt
    lcStatDt13 = lcObjectSt
    lcStatDt14 = lcObjectSt
    o = loFormSet.loScFields[8]
    o.Enabled = lcStatDt8 = 'ENABLE'
    o = loFormSet.loScFields[9]
    o.Enabled = lcStatDt9 = 'ENABLE'
    o = loFormSet.loScFields[12]
    o.Enabled = lcStatDt12 = 'ENABLE'
    loFormSet.Ariaform1.chkBankCodeAcct.Enabled = lcStatDt13 = 'ENABLE'
    
    *SHOW GET laData[8] DISABLE   && in both cases view and edit modes.
    
    IF loFormSet.ActiveMode = 'E' .AND.  ;
       (APINVHDR.nInvAmnt  - APINVHDR.NINVPAID - ; 
        APINVHDR.NINVDISTK - APINVHDR.NINVADJ   ) = 0
      *** Message :" Invoice ð for vendor ð is fully paid."
      ***                      <   OK   >
      =gfModalGen("TRM04075B00000","DIALOG",ALLTRIM(laData[3])+'|'+ALLTRIM(laData[2]))
    ENDIF
    
  CASE loFormSet.ActiveMode = 'A'
  
    laData[1] = 'I'
    o = loFormSet.loScFields[2]
    laData[2] = o.Value
    o = loFormSet.loScFields[3]
    laData[3] = o.Value
    
    laData[4]  = 'A'
    laData[5]  = 1
    laData[6]  = 'P'
    laData[7] = SPACE(15)
    laData[8]  = loFormSet.ldFisBgDt   && begin date of current fiscal year
    laData[9]  = loFormSet.ldFisBgDt   && begin date of current fiscal year
    laData[10] = 00.00
    laData[11] = 00.00

    ladata[13] = SPACE(12)
    ladata[14] = loFormSet.ap1.lcEmptyAcc

    lcStatDt8  = 'ENABLE' 
    lcStatDt9  = 'DISABLE'
    lcStatDt12 = 'ENABLE' 
    lcStatDt13 = 'ENABLE' 
    lcStatDt14 = 'ENABLE' 
    
    WITH loFormSet.Ariaform1
    .txtFirstInstDt.Enabled = lcStatDt8  = 'ENABLE' 
    .txtNextInstDt.Enabled = lcStatDt9  = 'ENABLE' 
    *.txtInstallment.Enabled = .F.
    *.txt1099Amount.Enabled = .F.
    .chkBankCodeAcct.Enabled = lcStatDt12 = 'ENABLE'
    .GLActCode.Enabled = lcStatDt14 = 'ENABLE' 
    ENDWITH 

    *SHOW GET laData[9] DISABLE   
  
    *E301077,80 IHB 03/03/1999 opend needed files only [start]
    *MAN
    *llAPSETUP  = gfSysOpen(gcDataDir+'APSETUP' ,' ','SH')
    *llAPDIV    = gfSysOpen(gcDataDir+'APDIV' ,'DIVISION','SH')
    *= gfOpenFile(gcDataDir+'APSETUP' ,' ','SH')
    *= gfOpenFile(gcDataDir+'APDIV' ,'DIVISION','SH')
    
    *E301077,80 IHB 03/03/1999 opend needed files only [end]
  
    IF APINVHDR.cVenPMeth = 'H'
      IF EMPTY(APVENDOR.cCashAcct)
        IF  !EMPTY(APVENDOR.cDivision)       .AND. ;
            SEEK(APVENDOR.cDivision,'APDIV') .AND. ;
            !EMPTY(APDIV.cCashAcct)
          laData[14] = APDIV.cCashAcct
        ELSE
          laData[14] = APSETUP.cCashAcct
        ENDIF
      ELSE
        laData[14] = APVENDOR.cCashAcct
      ENDIF
    ELSE
      IF EMPTY(APVENDOR.cBnkCode)
        IF !EMPTY(APINVHDR.cDivision)       .AND. ;
          SEEK(APINVHDR.cDivision,'APDIV')  .AND. ;
          !EMPTY(APDIV.cBnkCode)
          laData[12] = APDIV.cBnkCode
          laData[13] = APDIV.cChkAcct
        ELSE
          laData[12] = APSETUP.cBnkCode
          laData[13] = APSETUP.cChkAcct
        ENDIF
      ELSE
        laData[12] = APVENDOR.cBnkCode
        laData[13] = APVENDOR.cChkAcct
      ENDIF

      IF SEEK(laData[12]+ladata[13],'APCHECKS')
        * Check if the checking account has the same
        * currency as that of the invoice, if not,
        * get the first checking account using
        * the invoice currency, if any is found,
        * or clear al fields if none is found.
        *laData[14] = APCHECKS.cChkGlAcc     
        IF APCHECKS.cCurrCode = APINVHDR.cCurrCode
          laData[14] = APCHECKS.cChkGlAcc     
        ELSE
          SELECT APCHECKS
          LOCATE REST WHILE cBnkCode = laData[12];
                 FOR cCurrCode = APINVHDR.cCurrCode
          IF FOUND()
            laData[13] = APCHECKS.cChkAcct
            laData[14] = APCHECKS.cChkGlAcc     
          ELSE
            ladata[12] = SPACE(8)
            ladata[13] = SPACE(12)
            ladata[14] = loFormSet.ap1.lcEmptyAcc
            lcStatDt13 = 'DISABLE'    
            lcStatDt14 = 'DISABLE'      
          ENDIF
        ENDIF
        * end.           
      ELSE
        ladata[12] = SPACE(8)
        ladata[13] = SPACE(12)
        ladata[14] = loFormSet.ap1.lcEmptyAcc
        lcStatDt13 = 'DISABLE'    
        lcStatDt14 = 'DISABLE'      
        * show objects after ENDIF
        **SHOW GET laData[13] DISABLE
        **SHOW GET laData[14] DISABLE
        **SHOW GET ibChecks   DISABLE
        **SHOW GET ibGlAcc    DISABLE
      ENDIF  
      * show objects 
      *SHOW GET laData[12]
      *SHOW GET laData[13] &lcStatDt14
      *SHOW GET laData[14] &lcStatDt14
      *SHOW GET ibChecks   &lcStatDt14
      *SHOW GET ibGlAcc    &lcStatDt14
      * end.
    ENDIF

    *E301077,80 IHB 03/03/1999 opend needed files only [start]
    *IF USED('APSETUP') .AND. llAPSETUP
    *  =gfCloseFile('APSETUP')
    *ENDIF
    *IF USED('APDIV') .AND. llAPDIV
    *  =gfCloseFile('APDIV')
    *ENDIF
    *E301077,80 IHB [end]
    
    =lfUpdateData(.T.)
    
ENDCASE

IF loFormSet.ActiveMode $ 'A|E' 
WITH loFormSet.Ariaform1
.KBVendCode.Enabled = .F.
.KBVendPhone.Enabled = .F.
.KBVendCompany.Enabled = .F.
.kbInvoice.Enabled = .F.

ENDWITH 
ENDIF 

*IF !laScrMode[1]
IF loFormSet.ActiveMode <> 'S'
  =lfReadInv(loFormSet)  
  loFormset.Ariaform1.GLActCode.sharedvalidation()
ENDIF

*IF laScrMode[3] .OR. laScrMode[4]
IF loFormSet.ActiveMode $ 'E|A'
  IF EMPTY(APVENDOR.cVen1099T)
    lcStatDt11 = 'DISABLE'
    *SHOW GET laData[11] DISABLE   
  ELSE
    lcStatDt11 = 'ENABLE'
    *SHOW GET laData[11] ENABLE
  ENDIF
ELSE  
  lcStatDt11 = 'DISABLE'
  *SHOW GET laData[11] DISABLE   
ENDIF
o = loFormSet.loScFields[11]
o.Enabled = lcStatDt11 = 'ENABLE'

*lcPopCol  = IIF(laScrMode[1] .OR. laScrMode[2], SCHEME(1,10),SCHEME(1,2))
*SHOW GET lcFactCode &lcFactCol
SELECT APINVAHD

*- End of lfChangeMode.

*!**************************************************************************
*!
*!        Function : lfReadInv
*!
*!**************************************************************************
FUNCTION lfReadInv
PARAMETERS loFormSet
WITH loFormSet.Ariaform1

IF loFormSet.ActiveMode $ 'V|A'
.ldLastGen.Value    = APINVAHD.dAutLGen
.ldInvDate.Value    = APINVHDR.dInvDate
.lnInvAmnt.Value    = APINVHDR.nInvAmnt
.lnInvDisc.Value    = APINVHDR.nInvDisOf
.lcInvRef.Value     = APINVHDR.cInvRef
.lcPayPrio.Value    = APINVHDR.cVenPrior
.lnTotAppPay.Value  = APINVHDR.nInvPaid + APINVHDR.nInvAdj
.lnTerDiscD.Value   = APINVHDR.nInvDisTk
.lcRemitTo.Value    = APINVHDR.CINVREMIT
.lcRmtComp.Value    = APINVHDR.COUTCOMP
.lcRmtAddr1.Value   = APINVHDR.COUTADDR1
.lcRmtAddr2.Value   = APINVHDR.COUTADDR2
.lcRmtAddr3.Value   = APINVHDR.COUTADDR3
.kbFacCode.Keytextbox.Value   = APINVHDR.CFACCODE
* Currency code is that of the invoice
.kbCurrCode.Keytextbox.Value  = APINVHDR.cCurrCode


IF APINVHDR.cVenPMeth = 'H'
  lcStatDt12   =     'DISABLE'
  *SHOW GET ibBank     DISABLE
  *SHOW GET laData[12] DISABLE
  lcStatDt13   =     'DISABLE'    
  *SHOW GET ibChecks   DISABLE
  *SHOW GET laData[13] DISABLE
  lcPrompt = 'Cash acc'
  loFormset.Ariaform1.chkBankCodeAcct.Enabled = lcStatDt12   =     'ENABLE'
ELSE
  lcPrompt = 'GL acct.'
ENDIF

*E300643,1 Change this lines for the changes we have made to (gfCodDes) [Begin]
*lcTermCode = gfCodDes(APINVHDR.cTermCode)   && Term description
*lcDivision = gfCodDes(APINVHDR.cDivision)   && Division description
*old : lcTermCode = gfCodDes(APINVHDR.cTermCode , 'CTERMCODE')   && Term description
.lcTermCode.Value = APINVHDR.cTermCode
.lcTermCode.Valid()

*old : lcDivision = gfCodDes(APINVHDR.cDivision , 'CDIVISION')   && Division description
.lcDivision.Value = APINVHDR.cDivision
.lcDivision.Valid()

.lcType.Valid()
*E300643,1 Change this lines for the changes we have made to (gfCodDes) [End]
  
*** Payment method
lnElemNum = ASCAN(loFormset.laPayMeth,APINVHDR.cVenPMeth)
*.lcPayMeth.Value = IIF(lnElemNum > 0, loFormset.laPayMeth[ASUBSCRIPT(loFormset.laPayMeth,lnElemNum ,1),1]," ")    
.lcPayMeth.Value = APINVHDR.CVENPMETH 

*** Remit to objects *
lnElemNum = ASCAN(loFormset.laRemitTo,APINVHDR.cInvRemit)
*.lcRemitTo.Value = IIF(lnElemNum > 0, loFormset.laRemitTo[ASUBSCRIPT(loFormset.laRemitTo,lnElemNum ,1),1]," ")    
ENDIF

.ldLastGen.Enabled  = .F.
.ldInvDate.Enabled  = .F.
.lnInvAmnt.Enabled  = .F.
.lnInvDisc.Enabled  = .F.
.lcInvRef.Enabled  = .F.
.lcPayPrio.Enabled  = .F.
.lnTotAppPay.Enabled  = .F.
.lnTerDiscD.Enabled  = .F.
.lcRmtComp.Enabled  = .F.
.lcRmtAddr1.Enabled  = .F.
.lcRmtAddr2.Enabled  = .F.
.lcRmtAddr3.Enabled  = .F.
.kbFacCode.Enabled  = .F.
.kbCurrCode.Enabled  = .F.
.lcTermCode.Enabled  = .F.
.lcDivision.Enabled  = .F.
.lcPayMeth.Enabled  = .F.
.lcRemitTo.Enabled  = .F.
ENDWITH 


*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
FUNCTION lfwOldVals

PARAMETERS lcOldObjNm
&lcOldObjNm = EVALUATE(SYS(18))

************************************************************
*! Name      : lfChngLine
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/13/2012
*! Purpose   : lfChngLine
************************************************************
FUNCTION lfChngLine
PARAMETERS loFormSet
WITH loFormSet.Ariaform1
  .KBVendCode.KeyTextBox.VALUE    = APINVAHD.CVENDCODE
  .KBVendPhone.KeyTextBox.VALUE   = APVENDOR.CPHONENO
  .KBVendCompany.KeyTextBox.VALUE = APVENDOR.CVENCOMP
  .kbInvoice.Keytextbox.Value = APINVAHD.CINVNO
  =lfvInvoice(loFormSet,.kbInvoice)
ENDWITH 
*- End of lfChngLine.

************************************************************
*! Name      : lfvInvoice
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/12/2012
*! Purpose   : lfvInvoice
************************************************************
*FUNCTION lfvData_3
FUNCTION lfvInvoice
PARAMETERS loFormSet,loFld
LOCAL llValidation

llValidation = .T.

SELECT APINVHDR
SET ORDER TO VENDINV
lcSavExact = SET('EXACT')
SET EXACT ON
DIMENSION  laTemp[2]
laTemp     = ''
*lcVendor   = laData[2]

LOCAL oVnd,oInv
oVnd = loFormSet.Ariaform1.kbVendCode.Keytextbox
oInv = loFormset.Ariaform1.kbInvoice.Keytextbox

oVnd.Value = ALLTRIM(oVnd.Value)
oVnd.Value = PADR(oVnd.Value,FSIZE('CVENDCODE','APINVHDR'))

oInv.Value = ALLTRIM(oInv.Value)
oInv.Value = PADR(oInv.Value,FSIZE('CINVNO','APINVHDR'))

lcVendor  = oVnd.Value
lcInvlice = oInv.Value

*!*	lcSavTitle = lcFile_Ttl
*!*	lcSavBrFld = lcBrFields
lcFile_Ttl = ' '
lcBrFields = ' '
=gfGetBrF(@lcFile_Ttl, @lcBrFields, 'APINVHDR')
llBrowse = loFld.Selectedfrombrowse

IF llBrowse .OR. !EMPTY(oInv.Value) &&.AND. LASTKEY() = 13
  IF llBrowse .OR. ATC("?",oInv.Value) > 0 .OR. EMPTY(oInv.Value) .OR. EMPTY(oVnd.Value)
          
    IF EMPTY(oVnd.Value)
      =gfBrows([FOR cInvStat <> 'V'],'CVENDCODE,CINVNO','laTemp')
    ELSE
      SET EXACT &lcSavExact
      IF SEEK(oVnd.Value)
        =gfBrows([FOR CVENDCODE = lcVendor AND cInvStat <> 'V'],'CVENDCODE,CINVNO','laTemp')
      ELSE
        *** Message : " No ð found for vendor ð."
        ***           "                  ® Ok ¯              "
        =gfModalGen("TRM04042B00000","DIALOG","invoices|"+ALLTRIM(oVnd.Value))
      ENDIF  
    ENDIF  
    
    SET ORDER TO INVVEND IN APINVHDR    
    IF EMPTY(laTemp[2])
      *lcInvlice = lcOldVal
      *SHOW GET lcInvlice
      *_CUROBJ = OBJNUM(lcInvlice)
      oInv.Value = oInv.OldValue
      llValidation = .F.
    ELSE
      oVnd.Value    = laTemp[1]
      oInv.Value    = laTemp[2]
      *SHOW GET lcVendor      
      *SHOW GET lcInvlice
      llValidation =lfCheckInsta()
      IF llValidation
        loFormSet.Ariaform1.KBVendCode.sharedvalidation()
      ENDIF 
    ENDIF
  ELSE
    IF SEEK(oVnd.Value+oInv.Value,'APINVHDR','VENDINV') .AND. APINVHDR.cInvStat <> 'V'
      llValidation = lfCheckInsta()
    ELSE  
      lnClosRec = RECNO(0)
      IF BETWEEN(lnClosRec,1,RECCOUNT('APINVHDR'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF
      *** Message :" ð is not found in the data file."
      ***        < Browse >   < Reenter >
      lnOption = gfModalGen('QRM00001B00014','Dialog',;
                             loFormSet.lcTKeyCode + ALLTRIM(oVnd.Value)+' '+ALLTRIM(oInv.Value))
      DO CASE
        CASE lnOption = 1             && Browse
          SET EXACT &lcSavExact
          IF SEEK(oVnd.Value)
            =gfBrows([FOR CVENDCODE = lcVendor AND cInvStat <> 'V'],'CVENDCODE,CINVNO','laTemp')
    
            IF EMPTY(laTemp[1])
              *lcInvlice = lcOldVal
              *SHOW GET lcInvlice
              *_CUROBJ = OBJNUM(lcInvlice)
              oInv.Value = oInv.OldValue
              llValidation = .F.
            ELSE
              oVnd.Value    = laTemp[1]
              oInv.Value    = laTemp[2]
              *SHOW GET lcVendor      
              *SHOW GET lcInvlice
              =lfCheckInsta()
            ENDIF
          ELSE
            *** Message : " No ð for vendor ð"
            ***           "          ® Ok ¯              "
            =gfModalGen("TRM04042B00000","DIALOG","invoices|"+ALLTRIM(oVnd.Value))
            *lcInvlice = lcOldVal
            *SHOW GET lcInvlice
            *_CUROBJ = OBJNUM(lcInvlice)
            oInv.Value = oInv.OldValue
            llValidation = .F.
          ENDIF  
        
        CASE lnOption = 2       && Reenter
          *lcInvlice = lcOldVal
          *SHOW GET lcInvlice
          *SHOW GET ibInvoice ENABLE
          *_CUROBJ = OBJNUM(lcInvlice)
          oInv.Value = oInv.OldValue
          llValidation = .F.
      ENDCASE
    ENDIF
  ENDIF
ENDIF

*!*	lcFile_Ttl = lcSavTitle 
*!*	lcBrFields = lcSavBrFld 
SET EXACT &lcSavExact
llBrowse = .F.
SET ORDER TO INVVEND IN APINVHDR
SELECT APINVAHD

*oVnd.Value = lcVendor  
*oInv.Value = lcInvlice 
RETURN llValidation
*- End of lfvInvoice.

*!**************************************************************************
*!
*!        Function : lfCheckInsta
*!
*!**************************************************************************
*
FUNCTION lfCheckInsta

oVnd = loFormSet.Ariaform1.kbVendCode.Keytextbox
oInv = loFormset.Ariaform1.kbInvoice.Keytextbox

SET ORDER TO INVVEND IN APINVHDR

lcVendor = oVnd.Value
IF APINVHDR.cVenPMeth = 'C'
  *** Message :" You cannot create an installment record for 
  ***            invoices paid by credit card."
  ***                      <   OK   >
  =gfModalGen('QRM04079B00000','Dialog')
  oInv.Value = SPACE(12)
  *_CUROBJ = OBJNUM(laData[3])
  *oInv.SetFocus()
  RETURN .F.
ENDIF

IF APINVHDR.nInvAmnt < 0
  *** Message :" You cannot create an installment record for debit memos."
  ***                      <   OK   >
  =gfModalGen('QRM04081B00000','Dialog')
  oInv.Value = SPACE(12)
  *_CUROBJ = OBJNUM(laData[3])
  *oInv.Setfocus()
  RETURN .F.
ENDIF

DIMENSION laData[1]

SELECT APINVAHD
lcScFields = loFormSet.lcScFields
IF SEEK('I'+oVnd.Value+oInv.Value)
  SCATTER FIELDS &lcScFields MEMO TO laData
  =lfUpdateData(.T.)
  * Old : laScrMode    = .F.
  * Old : laScrMode[2] = .T.
  loFormSet.ChangeMode('V')
  SET EXACT &lcSavExact
  SET ORDER TO INVVEND IN APINVHDR
  *SHOW GETS
ELSE
  *lcSavTit2 = lcFile_Ttl
  *lcSavBrF2 = lcBrFields
  DIMENSION laTemp2[1]
  laTemp2   = ''

  *** Message :" There is no installment record for this invoice."
  ***          < Browse > < Add > < Reenter >
  lnOption = gfModalGen('QRM04117B00001','Dialog')
    DO CASE
      CASE lnOption = 1
        =gfGetBrF(@lcFile_Ttl, @lcBrFields, 'APINVAHD')
        =gfBrows([FOR cAutMType+CVENDCODE = "I"+lcVendor],'CINVNO','laTemp2')
        IF EMPTY(laTemp2[1])
           oInv.Value = SPACE(12)
           *_CUROBJ = OBJNUM(laData[3])
           *oInv.Setfocus()
           RETURN .F.
        ELSE
          SCATTER FIELDS &lcScFields MEMO TO laData
          =lfUpdateData(.T.)
          * Old : laScrMode    = .F.
          * Old : laScrMode[2] = .T.
          loFormSet.ChangeMode('V')
          SET EXACT &lcSavExact
          SET ORDER TO INVVEND IN APINVHDR
          *SHOW GETS
        ENDIF        

      CASE lnOption  = 2
        * Old : laScrMode    = .F.
        * Old : laScrMode[4] = .T.
        loFormSet.ChangeMode('A')
        SET EXACT &lcSavExact
        SET ORDER TO INVVEND IN APINVHDR
        *SHOW GETS

      CASE lnOption = 3
        oInv.Value = SPACE(12)
        *oInv.Setfocus()
        *_CUROBJ = OBJNUM(laData[3])
        RETURN .F.
    ENDCASE  
 
  *lcFile_Ttl = lcSavTit2
  *lcBrFields = lcSavBrF2 
ENDIF  


SELECT APINVHDR
 
*!**************************************************************************
*!
*!        Function : lfvData_5
*!
*!**************************************************************************
*  Valid function for laData[5] 
FUNCTION lfvData_5
PARAMETERS loFld

*IF laData[5] < 1
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
*IF loFld.Value < 1
IF TYPE('loFld.Value') == 'N' .AND. loFld.Value < 1
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
  *** Message : "    ð should be greater than ð.  "  
  ***                      <   OK   >
  =gfModalGen("TRM04072B00000","DIALOG",'The duration|zero')
  *laData[5] = lnOldVal
  loFld.Value = loFld.OldValue
  *_CUROBJ   = *_CUROBJ
  RETURN
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_8
*!
*!**************************************************************************
*
FUNCTION lfvData_8
PARAMETERS loFormSet,loFld

*IF laData[8] = ldOldVal 
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
*IF loFld.Value = loFld.OldValue
IF TYPE('loFld.Value') == 'D' AND loFld.Value = loFld.OldValue
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
  RETURN
ENDIF

IF EMPTY(loFld.Value)
  *** Message : " You have to enter the ð."
  ***                      <   OK   >
  =gfModalGen("TRM04066B00000","DIALOG",'first installment date')
  *laData[8] = ldOldVal
  loFld.Value = loFld.OldValue
  *_CUROBJ   = *_CUROBJ
ELSE    
  IF ! lfvDtMsg(oAriaApplication.PrntCompanyID)
*    laData[8] = ldOldVal
  loFld.Value = loFld.OldValue
    *_CUROBJ   = *_CUROBJ
  ENDIF
ENDIF

*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
*laData[9]  = laData[8]
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
o = loFormSet.loScFields[9]
o.Value = loFld.Value
*SHOW GET laData[9]

*!**************************************************************************
*!
*!      Function: lfvData_9
*!
*!**************************************************************************
*
FUNCTION lfvData_9
PARAMETERS loformset,loFld

*IF laData[9] = ldOldVal 
IF loFld.Value = loFld.OldValue
  RETURN
ENDIF

o8 = loFormset.loScFields[8]

*IF EMPTY(laData[9])
IF EMPTY(loFld.Value)
  *** Message : " You have to enter the ð."
  ***                      <   OK   >
  =gfModalGen("TRM04066B00000","DIALOG",'next installment date')
  *laData[9] = ldOldVal
  loFld.Value = loFld.OldValue
  *_CUROBJ   = *_CUROBJ
ELSE  
  *IF laData[9] < laData[8]
  *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
  *IF loFld.Value < o8.Value
  IF TYPE('loFld.Value') == 'N' AND loFld.Value < o8.Value
  *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
    *** Message :" The ð date cannot be less than the ð date."
    ***                      <   OK   >
    =gfModalGen("TRM04028B00000","DIALOG",'next installment|start installment')
    *laData[9] = ldOldVal
    loFld.Value = loFld.OldValue
    *_CUROBJ   = *_CUROBJ
  ELSE
    IF ! lfvDtMsg(oAriaApplication.PrntCompanyID)
      *laData[9] = ldOldVal
    loFld.Value = loFld.OldValue
      *_CUROBJ   = *_CUROBJ
    ENDIF
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_10
*!
*!**************************************************************************
*
FUNCTION lfvData_10
PARAMETERS loformset,loFld

llValidData = .T.      

*IF laData[10] < 0
o4 = loFormSet.loScFields[4]
o10 = loFormSet.loScFields[10]

*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
*IF loFld.Value < 0
IF TYPE('loFld.Value') == 'N' .AND. loFld.Value < 0
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
  *** Message : "   ð should be greater than ð.   "  
  ***                      <   OK   >
  =gfModalGen("TRM04072B00000","DIALOG",;
  'Installment '+IIF(o4.Value = 'P','percent','amount')+'|zero')
  *laData[10]  = lnOldVal
  loFld.Value = loFld.OldValue 
  *_CUROBJ     = *_CUROBJ
  llValidData = .F.
  RETURN llValidData 
ENDIF

*IF laData[4] = 'P'
IF o4.Value = 'P'
  *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
  *IF o10.Value > 100 
  IF TYPE('o10.Value') == 'N' .AND. o10.Value > 100 
  *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
    *** Message :  "The installment percent cannot be greater than 100."
    ***                      <   OK   >
    *** restore the old value of the field
    =gfModalGen("TRM04119B00000","DIALOG")
    *laData[10]  = lnOldVal
    o10.Value = o10.OldValue
    *_CUROBJ     = *_CUROBJ
    llValidData = .F.    
  ELSE
    *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
    *IF ( o10.Value * APINVHDR.nInvAmnt / 100 ) >  ;
       ( APINVHDR.nInvAmnt - (APINVHDR.NINVDISTK + ; 
                              APINVHDR.NINVPAID + ;
                              APINVHDR.NINVADJ ))
    IF (TYP('o10.Value') == 'N' AND o10.Value * APINVHDR.nInvAmnt / 100 ) > (APINVHDR.nInvAmnt - (APINVHDR.NINVDISTK + APINVHDR.NINVPAID + APINVHDR.NINVADJ))
    *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
      *** Message :"ð amount cannot be greater than the open ð amount."
      ***                      <   OK   >
      *** restore the old value of the field
      =gfModalGen("TRM04015B00000","DIALOG",'The installment|invoice')
      *laData[10]  = lnOldVal
      o10.Value = o10.OldValue
      *_CUROBJ     = *_CUROBJ
      llValidData = .F.      
    ENDIF
  ENDIF
ELSE
  *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
  *IF o10.Value > ( APINVHDR.nInvAmnt - (APINVHDR.NINVDISTK + ; 
                    APINVHDR.NINVPAID + ;
                    APINVHDR.NINVADJ  ))
  IF TYPE('o10.Value') == 'N' AND o10.Value > (APINVHDR.nInvAmnt - (APINVHDR.NINVDISTK + APINVHDR.NINVPAID + APINVHDR.NINVADJ))
  *!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
    *** Message :"ð amount cannot be greater than the open ð amount."
    ***                      <   OK   >
    *** restore the old value of the field
    =gfModalGen("TRM04015B00000","DIALOG",'The installment|invoice')
    *laData[10]  = lnOldVal
    o10.Value = o10.OldValue
    *_CUROBJ     = *_CUROBJ
    llValidData = .F.          
  ENDIF    
ENDIF
RETURN llValidData

*!**************************************************************************
*!
*!      Function: lfvData_11
*!
*!**************************************************************************
*
FUNCTION lfvData_11
PARAMETERS loformset,loFld

llValidData = .T.      

*IF laData[10] < 0
o10 = loFormSet.loScFields[10]

*IF laData[11] < 0
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][Start]
*IF loFld.Value < 0
IF TYPE('loFld.Value') == 'N' .AND. loFld.Value < 0
*!B609890,1 SAB 04/17/2012 Fix Data Validation Problems [T20120304.0004][End]
  *** Message : "   ð should be greater than ð.   "  
  ***                      <   OK   >
  =gfModalGen("TRM04072B00000","DIALOG",'1099 Amount|zero')
  *laData[11]  = lnOldVal
  loFld.Value = loFld.OldValue 
  *_CUROBJ     = *_CUROBJ
  llValidData = .F.
  RETURN llValidData
ENDIF

*IF laData[11] > laData[10]
IF loFld.Value > o10.Value
  *** Message :"ð amount cannot be greater than the open ð amount."
  ***                      <   OK   >
  *** restore the old value of the field
  =gfModalGen("TRM04015B00000","DIALOG",'The 1099|installment')
  *laData[11]  = lnOldVal
  loFld.Value = loFld.OldValue 
  *_CUROBJ     = *_CUROBJ
  llValidData = .F.  
ENDIF  
RETURN llValidData

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[12] (bank code), and laData[13] 
* (checking account code)
*
FUNCTION lfvBnkChk
PRIVATE lcFltrCond, lcFltrMsg
IF EVALUATE(SYS(18)) <> lcOldVal
  =gfUpdate()
ENDIF
* Add parameters to lfBnkChk to restrict bank\checking account
* selection to those using the same currency as that of the 
* invoice.
*IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcStatDt13)
*lcFltrCond = [cCurrCode = '] + APINVHDR.cCurrCode + [']
*lcFltrMsg  = ['INM04155B00000','DIALOG',']+ALLTRIM(APINVHDR.cCurrCode) + ;
              [|'+ ALLTRIM(APBANKS.cBnkCode) ]   

*IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcStatDt13,;
             '', '', @lcFltrCond, @lcFltrMsg)

IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcStatDt13,;
             '', '', APINVHDR.cCurrCode)             
* end.
  lcObjectSt = 'DISABLE'
  lcStatDt14 = 'DISABLE'
  RETURN 1
ELSE
  lcObjectSt = 'ENABLE'
  lcStatDt14 = 'ENABLE'        
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
*
FUNCTION lfvData_14

lcDummy = ''
IF EMPTY(STRTRAN(laData[14],'-',' ')) .OR. ! lfApAcs(@lcDummy,llBrowse)
  laData[14] = lcOldVal
ENDIF
  
*SHOW GET laData[14]
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvType
*!
*!**************************************************************************
*
FUNCTION lfvType
PARAMETERS loFormset,loFld

loFormSet.Ariaform1.lblPer1.Caption = IIF(loFld.Value="P","%"," ")
loFormSet.Ariaform1.lblPer2.Caption = IIF(loFld.Value="P","%"," ")

*!**************************************************************************
*!
*!        Function : lfvDuration
*!
*!**************************************************************************
*  Valid function for Duration popup
*
FUNCTION lfvDuration
PARAMETERS loFld

  lcSavExact = SET('EXACT')
  SET EXACT ON
  lnElemNum = ASCAN(laDuration,lcDuration)
  laData[6] = IIF(lnElemNum > 0, laDuration[ASUBSCRIPT(laDuration,lnElemNum ,1),2]," ")    
  SET EXACT &lcSavExact

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!      ( Before Save functionality )
*!**************************************************************************
*
PROCEDURE lpSavScr
PARAMETERS loFormSet

GO RECNO('APINVHDR') IN APINVHDR    && 
GO RECNO('APVENDOR') IN APVENDOR    &&

*lnOldVal    = laData[10]
llValidData = .F.

o10 = loFormSet.loScFields[10]
o11 = loFormSet.loScFields[11]
o12 = loFormSet.loScFields[12]
o13 = loFormSet.loScFields[13]
o14 = loFormSet.loScFields[14]

llValidData = lfvData_10(loformset,o10)               && ReValidate laData[10]
IF ! llValidData
  llcSave = .F.  
  *_CUROBJ = OBJNUM(laData[10])
  =lfReadInv(loFormSet)  
  RETURN llcSave 
ENDIF  

*lnOldVal    = laData[11]
llValidData = .F.

llValidData=lfvData_11(loformset,o11)               && ReValidate laData[11]
IF ! llValidData
  llcSave = .F.  
  *_CUROBJ = OBJNUM(laData[11])
  =lfReadInv(loFormSet)  
  RETURN llcSave 
ENDIF  

IF APINVHDR.cVenPMeth = 'H' AND EMPTY(STRTRAN(STRTRAN(o14.Value,'-'),'0'))
  *** Message :  "You have to enter the ð."
  ***                      <   OK   >
  =gfModalGen("TRM04066B00000","DIALOG",'cash account')
  llcSave = .F.  
  *_CUROBJ = OBJNUM(laData[14])
  =lfReadInv(loFormSet)  
  RETURN llcSave 
ENDIF

IF APINVHDR.cVenPMeth <> 'H' 
  *** If the bank code is empty, present the following message 
  *** and return to the bank code field
  *IF EMPTY(laData[12])
  IF EMPTY(o12.Value)
    *** Message : "   You have to enter the ð.  "
    ***                 ® OK  ¯
    =gfModalGen("TRM04066B00000","DIALOG","the bank code")
    llcSave = .F.  
    *_CUROBJ = OBJNUM(laData[12])
    =lfReadInv(loFormSet)  
    RETURN llcSave 
  ENDIF

  *IF !SEEK(laData[12],'APBANKS')  
  IF !SEEK(o12.Value,'APBANKS')  
  
    *** Message :  " ð not found."
    ***                      <   OK   >
    =gfModalGen("TRM0400200000","DIALOG",'Bank code ' + ALLTRIM(o12.Value))
    llcSave = .F.  
    *_CUROBJ = OBJNUM(laData[12])
    =lfReadInv(loFormSet)  
    RETURN llcSave 
  ENDIF 

  *IF !SEEK(laData[12]+ladata[13],'APCHECKS')  
  IF !SEEK(o12.Value+o13.Value,'APCHECKS')  
    *** Message :  "ð account not valid."
    ***                      <   OK   >
    =gfModalGen("TRM04077B00000","DIALOG",'Checking')
    llcSave = .F.  
    *_CUROBJ = OBJNUM(laData[13])
    =lfReadInv(loFormSet)  
    RETURN llcSave 
  ENDIF

  *IF EMPTY(STRTRAN(laData[14],'-',' '))
  IF EMPTY(STRTRAN(o14.Value,'-',' '))
    *** Message :" You have to enter the ð."
    ***                      <   OK   >
    =gfModalGen("INM04066B00000","DIALOG",'G/L account')
    llcSave = .F.  
    *_CUROBJ = OBJNUM(laData[14])
    =lfReadInv(loFormSet)  
    RETURN llcSave 
  ELSE
    *IF llApGlLink .AND. ! SEEK(ALLTRIM(laData[14]),'lcLinkChar')
    IF loFormSet.ap1.llApGlLink .AND. ! SEEK(ALLTRIM(o14.Value),'lcLinkChar')
      *** Message :" ð not found."
      ***                      <   OK   >
      =gfModalGen("INM04002B00000","DIALOG","G/L account|"+ALLTRIM(o14.Value))
      llcSave = .F.  
      *_CUROBJ = OBJNUM(laData[14])
      =lfReadInv(loFormSet)  
      RETURN llcSave 
    ENDIF
  ENDIF  
ENDIF

************************************************************
*! Name      : lfFormSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/12/2012
*! Purpose   : 
************************************************************
FUNCTION lfFormSave
PARAMETERS loFormSet
SELECT APINVAHD
IF loFormSet.ActiveMode = 'A'
  APPEND BLANK
ENDIF
DIMENSION laData[ALEN(loFormSet.loScFields)]
laData[1] = loFormSet.loScFields[1]
lfUpdateData(.F.)
lcScFields = loformset.lcScFields
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info('APINVAHD')
=gfTableUpdate()

*- End of lfFormSave.

************************************************************
*! Name      : lfDelScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/13/2012
*! Purpose   : lfDelScr
************************************************************
FUNCTION lfDelScr
PARAMETERS loFormset
SELECT APINVAHD
DELETE 
gfTableUpdate()
*- End of lfDelScr.


