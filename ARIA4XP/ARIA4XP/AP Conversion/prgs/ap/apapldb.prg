***********************************************************************
*:   Program file: APAPLDB.PRG
*:  Program desc.: Apply Debit Screen
*:         System: Aria 4XP
*:      Developer: Mariam MAzhar (MMT)
*:           Date: 10/26/2009
*:      Reference: *N000636
*:************************************************************************
* Modifications
*N000636,3 TMI 07/26/2011 Enable the screen to work in A4xp FOX 
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX 
*:************************************************************************

#INCLUDE R:\aria4xp\PRGS\ap\APAPLDB.h
=oAriaApplication.remotesystemdata.execute("SELECT * FROM sydField","", "sydField","",oAriaApplication.SystemConnectionString,3 ,"",SET("Datasession"))

DO FORM (oAriaApplication.ScreenHome+"\AP\APAPLDB.SCX")
*!*************************************************************
*! Name      : lfvVendCode
*! Developer : Mariam Mazhat(MMT)
*! Date      : 10/26/2009
*! Purpose   : Function called from the Shared Validation of the Vendor Code
*!*************************************************************
*! Parameters: The FormSet
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvVendCode
  LPARAMETERS loFormSet
  
  loFormSet.ariaform1.grdInvoice.RecordSource = ''
  loFormSet.ariaform1.grdDBM.RecordSource = ''
  loFormSet.ldCurrDate = oariaApplication.SystemDate
  SELECT APVENDOR
  =gfSetOrder('VENCODE')
  =gfSeek('')
  llBrowse = loFormSet.AriaForm1.kbVendCode.Selectedfrombrowse
  loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = PADR(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE,LEN(APVENDOR.cVendCode))
  lcVendor  = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  



  IF llBrowse .OR. !EMPTY(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE)
    IF llBrowse .OR. !SEEK(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE) .OR. ATC("?",loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE) > 0  &&lower
      lnClosRec  = RECNO(0)
      IF llBrowse .OR. ATC("?",loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE) > 0  
        DIMENSION laTemp[3]
        laTemp = ''
        lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value 
        lcFile_Ttl = LANG_APAPLDB_VENDORS
        IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
          GO lnClosRec
        ELSE
          GO TOP
        ENDIF
        =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp','Vendors')
        IF !EMPTY(laTemp[1])
          lcVendor  = laTemp[1]
          loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(laTemp[1])  &&lower
          lcPhone   = laTemp[2]
          *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
          *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = ALLTRIM(laTemp[2])          
          loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(ALLTRIM(laTemp[2]),'@R '+gfPhoneTem())
          *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
          lcCompany = laTemp[3]
          loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = ALLTRIM(laTemp[3])
        ELSE
          loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCode.KeyTextBox.OldValue
        ENDIF
      ELSE
        lcTVenKey = ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CVENDCODE',SYDFIELD.cFld_Name))
		lcTVenInv = lcTVenKey + '\' + ;
		ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CINVNO',SYDFIELD.cFld_NAme)) + ' : '
		
        lnOption = gfModalGen('QRM00001B00014','Dialog',;
        lcTVenKey + " : " +ALLTRIM(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE))  
        
        lnOption = IIF(lnOption=2,3,lnOption)
        DO CASE
          CASE lnOption = 1

            DIMENSION laTemp[3]

            laTemp = ''
            lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value 
            lcFile_Ttl = "Vendor"
            IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
              GO lnClosRec
            ELSE
              GO TOP
            ENDIF
			
            =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp',LANG_APAPLDB_VENDORS)

            IF !EMPTY(laTemp[1])
              lcVendor  = laTemp[1]
              loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(laTemp[1])  &&lower
              lcPhone   = laTemp[2]
              *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
              *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = ALLTRIM(laTemp[2])
              loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(ALLTRIM(laTemp[2]),'@R '+gfPhoneTem())
              *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
              lcCompany = laTemp[3]
              loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = ALLTRIM(laTemp[3])
            ELSE
              loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCode.KeyTextBox.OldValue
            ENDIF
            loFormSet.REFRESH()
          CASE lnOption = 3
            loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCode.KeyTextBox.OldValue
            RETURN .F.
        ENDCASE
      ENDIF
    ELSE
      lcPhone   = APVENDOR.CPHONENO
      *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
      *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = APVENDOR.CPHONENO      
      loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(ALLTRIM(APVENDOR.CPHONENO),'@R '+gfPhoneTem())
      *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
      lcCompany = APVENDOR.CVENCOMP
      loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = APVENDOR.CVENCOMP
    ENDIF
  ELSE
    loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCode.KeyTextBox.OldValue
  ENDIF

  loFormSet.AriaForm1.KBVendCode.ENABLED = .T.
  loFormSet.AriaForm1.KBVendPhone.ENABLED = .T.
  loFormSet.AriaForm1.KBVendCompany.ENABLED = .T.

  llBrowse = .F.
  lcVenInvTypes = ALLTRIM(APVENDOR.cVenInvTyp)
  lcVenInvTypes = lcVenInvTypes + IIF(gfGetmemvar('M_BOMVAR') AND 'M' $ lcVenInvTypes,'A','')
  lcVenInvTypes = lcVenInvTypes + IIF(!EMPTY(gfGetmemvar('M_DYEOPR')) AND 'M' $ lcVenInvTypes,'D','')
  IF SEEK(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE,'APVENDOR')
    lfVldVend(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE,LOFORMSET)
    SELECT (loFormSet.lcTempVenHst)
    IF RECCOUNT()> 0
      ZAP 
    ENDIF   
    APPEND BLANK 
    REPLACE CVENDCODE WITH loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE
    SELECT (loFormSet.lcTempVend)
    IF RECCOUNT()> 0
      ZAP 
    ENDIF   
    APPEND BLANK 
    REPLACE CVENDCODE WITH loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE
  ENDIF 
  *--end of lfvVendCode

  *:********************************************************************
  *: Name      : lfGetBrHd
  *: Developer : Mariam Mazhar(MMT)
  *: Date      : 10/26/2009
  *: Purpose   : Return the Brows Fields and its header
  *!*************************************************************
  *! Parameters: The FormSet
  *!           : lcBrFile
  *:********************************************************************
  *: Returns            : lcBrReturn
  *:********************************************************************
FUNCTION lfGetBrHd
  PARAMETER loFormSet, lcBrFile

  PRIVATE lcBrReturn

  lcBrReturn = ''
  lcAlias = ALIAS()
  IF !USED('SYDFIELD')
    =oAriaApplication.remotesystemdata.execute("SELECT * FROM sydField","", "sydField","",oAriaApplication.SystemConnectionString,3 ,"",SET("Datasession"))
  ENDIF
  IF !USED('SYDFLFLD')
    =oAriaApplication.remotesystemdata.execute("SELECT * FROM SYDFLFLD","", "SYDFLFLD","",oAriaApplication.SystemConnectionString,3 ,"",SET("Datasession"))
  ENDIF

  SELECT sydfield.cfld_name,sydfield.cfld_head FROM sydfield,sydflfld WHERE ;
    sydflfld.cfile_nam = lcBrFile AND sydfield.cfld_name = sydflfld.cfld_name ;
    ORDER BY sydflfld.nfld_pos INTO ARRAY laFldName

  IF _TALLY <> 0
    lnCount = 1
    lnMaxColm = IIF(_TALLY-10 > 29,29,_TALLY-10)
    lcBrReturn = lcBrReturn + laFldName[lnCount,1] + " :H= '" + ALLTRIM(laFldName[lnCount,2]) + "'"
    FOR lnCount = 2 TO lnMaxColm
      lcBrReturn = lcBrReturn + ", "+ laFldName[lnCount,1] + " :H= '" + ALLTRIM(laFldName[lnCount,2]) + "'"
    ENDFOR
  ENDIF
  SELECT (lcAlias)
  RETURN lcBrReturn
  
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhat(MMT)
*! Date      : 10/26/2009
*! Purpose   : Function called from the Screen init Method
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
loFormSet.lc_InvHdr = gfTempName()
loFormSet.lc_debit = gfTempName()
loFormSet.lcTempVenHst= gfTempName()
loFormSet.lcTempVend= gfTempName()
llOpComp = gfOpenTable('SYCCOMP','CCOMP_ID','SH')

loFormSet.lcCurrYear  = IIF(gfSEEK(oAriaApplication.PrntCompanyID,'SYCCOMP'),SYCCOMP.cCurr_Yer," ")               && current year
IF llOpComp
  =gfCloseTable('SYCCOMP')
ENDIF

STORE SPACE(15)   TO loFormSet.lcInvRef
STORE SPACE(30)   TO loFormSet.lcPayMeth
STORE {}          TO loFormSet.ldDueFrom  , loFormSet.ldDueTo    , loFormSet.ldDiscFrom , loFormSet.ldDiscTo
STORE 1           TO loFormSet.rbDates 
STORE " "         TO loFormSet.lcPayPrior
STORE "*"         TO loFormSet.lcDivision 


*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
loFormSet.AriaForm1.KBVendPhone.KeyTextBox.InputMAsk = gfPhoneTem()
loFormSet.AriaForm1.KBVendPhone.KeyTextBox.Format = gfPhoneTem()
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 


loFormSet.lcFisFYear = SPACE(4)    
loFormSet.lcFspPrdId = SPACE(2)    

=gfOpenTable('GLACCHAR','ACCTCODE','SH','lcLinkChar')
SELECT 'lcLinkChar'
=gfSeek('')
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
*!*  =gfOpenTable('APSETUP','APSETUP','SH')
*!*  =gfOpenTable('ACCOD','ACCSEGNO','SH')
*!*  =gfOpenTable('APDIV','DIVISION','SH')
*!*  =gfOpenTable('apvendor','vencode','SH')
*!*  =gfOpenTable('apinvhdr','VENDINV','SH')
*!*  =gfOpenTable('APVENHST','VENDYEAR','SH')
*!*  =gfOpenTable('APDIST','INVVEND','SH')
=gfOpenTable(oAriaApplication.DataDir+'APSETUP',oAriaApplication.DataDir+'APSETUP','SH')
=gfOpenTable(oAriaApplication.DataDir+'ACCOD',oAriaApplication.DataDir+'ACCSEGNO','SH')
=gfOpenTable(oAriaApplication.DataDir+'APDIV',oAriaApplication.DataDir+'DIVISION','SH')
=gfOpenTable(oAriaApplication.DataDir+'apvendor',oAriaApplication.DataDir+'vencode','SH')
=gfOpenTable(oAriaApplication.DataDir+'apinvhdr',oAriaApplication.DataDir+'VENDINV','SH')
=gfOpenTable(oAriaApplication.DataDir+'APVENHST',oAriaApplication.DataDir+'VENDYEAR','SH')
=gfOpenTable(oAriaApplication.DataDir+'APDIST',oAriaApplication.DataDir+'INVVEND','SH')
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 


SELECT APSETUP
=gfSeek('')
GO TOP
loFormSet.llApGlLink = (CAPSGLLINK = 'Y')


SELECT ACCOD
=gfSeek("")
GO TOP 
loFormSet.llGlLink   = "GL"  $ oAriaApplication.CompanyInstalledModules 
IF loFormSet.llGlLink  
  =gfOpenTable(oariaApplication.datadir +'GLSETUP' ,' ','SH')
  SELECT GLSETUP
  GO TOP
ENDIF

loFormSet.lcApsAcMas= ACCOD.cAcsMask
loFormSet.lcApsAcMas= STRTRAN(loFormSet.lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
loFormSet.lcApsAcMas= ALLTRIM("X"+SUBSTR(loFormSet.lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(loFormSet.lcApsAcMas))
loFormSet.lcEmptyAcc = REPLICATE('0',lnApsAcLen)
loFormSet.lcExDifAcc  = IIF(loFormSet.llApGlLink, IIF(EMPTY(GLSETUP.CSETEXMJ), loFormSet.lcEmptyAcc,;
                ALLTRIM(GLSETUP.CSETEXMJ) + ;
                              STRTRAN(SUBSTR(loFormSet.lcApsAcMas,AT("-",loFormSet.lcApsAcMas)),"9","0")),;
                              gfGetMemVar('LNEXRATACC', oAriaApplication.ActiveCompanyID))


SELECT APVENHST
DIMENSION laVenHst[1,18]
AFIELDS('laVenHst')
=gfCrtTmp(loFormSet.lcTempVenHst,@laVenHst,'CVENDCODE+CFISFYEAR',loFormSet.lcTempVenHst)

SELECT APVENDOR
DIMENSION laVendor[1,18]
AFIELDS('laVendor')
=gfCrtTmp(loFormSet.lcTempVend,@laVendor,'CVENDCODE',loFormSet.lcTempVend)



*=gfOpenTable('apinvhdr','VENDINV','SH',loFormSet.lc_InvHdr)
WITH loFormSet
  *N000636,3 TMI 07/26/2011 [Start] check if tables are fox or Sql
  *.cbrowsetabledbengine   = "SQL"
  .cBrowseTableDBEngine    = IIF(lfIsNative('APINVHDR'),'NATIVE','SQL')  
  *N000636,3 TMI 07/26/2011 [End  ] 
  .nWorkArea        = 'APVENDOR'
  .otoolbar.nWorkArea        ='APVENDOR'
  .DataEnvironment.InitialSelectedAlias = 'APVENDOR'
  .cBrowseFileName        = "APVENDOR"
  .cBrowseIndexExpression = "CVENDCODE"
  .cBrowseIndexFields     = "CVENDCODE"
  .cBrowseIndexName       = 'VENCODE'
  .cBrowseAliasName       = "APVENDOR"
  .cBrowseTableName       = "APVENDOR"
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = LANG_APAPLDB_VENDORS
ENDWITH 
loFormSet.ariaBrFields.edtBrowseFields.Value = lfGetBrHd(loFormSet,'APVENDOR')
loFormSet.lcApDbSess  = gfSequence('CAPSESSNO')
loFormSet.AriaForm1.Caption = loFormSet.AriaForm1.Caption + LANG_APAPLDB_SESSION + loFormSet.lcApDbSess 

WITH loFormSet.ariaform1.grdDBM
  .RecordSource = ''
  .Column1.Header1.Caption = LANG_APAPLDB_DMNO
  .Column2.Header1.Caption = LANG_APAPLDB_DMDATE
  .Column3.Header1.Caption = LANG_APAPLDB_REFER
  .Column4.Header1.Caption = LANG_APAPLDB_OPENBAL 
  .Column5.Header1.Caption = LANG_APAPLDB_TOTALAPP 
  .Column6.Header1.Caption = LANG_APAPLDB_DIVISION 
  .Column7.Header1.Caption = LANG_APAPLDB_CURRCODE 
ENDWITH 
WITH loFormSet.ariaform1.grdInvoice
  .RecordSource = ''
  .Column1.Header1.Caption = LANG_APAPLDB_INVOICENO
  .Column2.Header1.Caption = LANG_APAPLDB_INVOICEDATE
  .Column3.Header1.Caption = LANG_APAPLDB_DUEDATE 
  .Column4.Header1.Caption = LANG_APAPLDB_P 
  .Column5.Header1.Caption = LANG_APAPLDB_OPENAMT
  .Column6.Header1.Caption = LANG_APAPLDB_TOTALAPP
  .Column7.Header1.Caption = LANG_APAPLDB_PAYMTHD
  .Column8.Header1.Caption = LANG_APAPLDB_DIVISION 
ENDWITH 

DIMENSION laPayMeth[1,2]
laPayMeth = ''
lnPayMLen  = gfGetVld('CVENPMETH',@laPayMeth,.T.)
lcSetExact = SET('EXACT')
SET EXACT ON
lnElem = ASCAN(laPayMeth, 'C')
IF lnElem > 0 
  =ADEL(laPayMeth, ASUBSCRIPT(laPayMeth, lnElem, 1))
  DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
ENDIF
SET EXACT &lcSetExact
DIMENSION loFormSet.laPayMeth[ALEN(laPayMeth,1),2]
ACOPY(laPayMeth,loFormSet.laPayMeth)

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : Mariam Mazhat(MMT)
*! Date      : 10/26/2009
*! Purpose   : Function called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
DO CASE 
  CASE loFormSet.ActiveMode = 'S'
  
    WITH loFormSet.AriaForm1
      .KBVendCode.ENABLED = .T.
      .KBVendPhone.ENABLED = .T.
      .KBVendCompany.ENABLED = .T.
      .KBVendPhone.KeyTextBox.VALUE = ''
      .KBVendCompany.KeyTextBox.VALUE = ''
      .KBVendCode.KeyTextBox.VALUE = ''
      .lblApplyDm.Visible = .F. 
      .lblToInv.Visible = .F.
      .txtDbtMEm.Visible = .F. 
      .txtInvoice.Visible = .F. 
      .dtAppDate.Value  = {}
      .dtAppDate.Enabled = .F.
      .grdInvoice.RecordSource = ''
      .grdDBM.RecordSource = ''
      .txtDbtMem.Value = ''
      .txtInvoice.Value = ''
      
      TRY 
        .KBVendCode.KeyTextBox.SetFocus()
      CATCH
      ENDTRY 
    ENDWITH 
  CASE loFormSet.ActiveMode = 'E'
  
    WITH loFormSet.AriaForm1
      .KBVendCode.ENABLED = .F.
      .KBVendPhone.ENABLED = .F.
      .KBVendCompany.ENABLED = .F.
      .rGBScope.rbAll.Value = 1
      .dtAppDate.Enabled =.T.
      .lblApplyDm.Visible = .T. 
      .lblToInv.Visible = .T.
      .txtDbtMEm.Visible = .T. 
      .txtInvoice.Visible = .T. 
      .dtAppDate.Value  = oAriaApplication.SystemDate
      lfGetData(loFormSet)
      .grdInvoice.afterRowColChange()
      .grdDBM.afterRowColChange()
      TRY 
        .dtAppDate.SetFocus()
      CATCH
      ENDTRY 

    ENDWITH 
  CASE loFormSet.ActiveMode = 'V'
    loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = APVENDOR.CVENDCODE 
    lfvVendCode(loFormSet)
ENDCASE   

*!*************************************************************
*! Name      : lfVldVend
*! Developer : Mariam Mazhat(MMT)
*! Date      : 10/26/2009
*! Purpose   : Function to Validate Vendor
*!*************************************************************
FUNCTION lfVldVend
PARAMETERS lcVendCode,LOFORMSET
lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 
lnCurAlias = SELECT(0)

IF APVENDOR.cVenPrior = "0"
  =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(lcVendCode))
  LOFORMSET.ChangeMode('S')
ENDIF   


lfGetInvDM(loFormSet)

lcDebtMem = &lc_debit..cInvNo
lcInvoice = &lc_InvHdr..cInvNo
llFound = .T.
DO CASE
  *** If the vendor's priority = 0, present
  *** the following message and exit.
  CASE APVENDOR.cVenPrior = "0"
    *** Message : "  Vendor � has payment priority 0. This vendor is on hold. "
    ***                  <  OK  >
    =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(lcVendCode))
    llFound = .F.

    *** If there are no debit memos for this vendor, present
    *** the following message and exit.
  CASE EOF(lc_debit)
    *** Message : "   No � to apply for vendor �. "
    ***                  <  OK  >
    =gfModalGen("TRM04026B00000","DIALOG",LANG_APAPLDB_DEBITMEMO +"|"+ALLTRIM(lcVendCode))
    llFound = .F.

  *** If there are no invoices for this vendor, present
  *** the following message and exit.
  CASE EOF(lc_InvHdr) .AND. !gfGetMemVar('LLMULCURR')
    *** Message : "  No � to apply for vendor �."
    ***                  <  OK  >
    =gfModalGen("TRM04026B00000","DIALOG",LANG_APAPLDB_OPENINV+"|"+ALLTRIM(lcVendCode))
    llFound = .F.
ENDCASE   

IF !llFound 
  LOFORMSET.ChangeMode('S')
ELSE
*!*    *MT
*!*    SELECT APVENDOR 
*!*    =gfSeek(lcVendCode)
*!*    IF lfObj_lock(.T.,'APVENDOR',lcVendCode,ORDER(),.F.)  
*!*      SELECT APVENDOR 
*!*      =gfReplace('')
*!*      =gfTableUpdate()
*!*    ELSE
*!*      LOFORMSET.ChangeMode('S')
*!*      RETURN 
*!*    ENDIF 
*!*    
*!*    SELECT (lc_debit)
*!*    SCAN 
*!*      SELECT APINVHDR 
*!*      =gfSeek(&lc_debit..CVENDCODE+&lc_debit..CINVNO,'APINVHDR','VENDINV')
*!*      IF lfObj_lock(.T.,'APINVHDR',&lc_debit..CVENDCODE+&lc_debit..CINVNO,'VENDINV',.T.)  
*!*        SELECT APINVHDR 
*!*        =gfReplace('')
*!*        =gfTableUpdate()
*!*      ELSE
*!*        SELECT (lc_debit)
*!*        DELETE 
*!*      ENDIF   
*!*    ENDSCAN
*!*    LOCATE 
*!*    SELECT (lc_InvHdr)
*!*    SCAN 
*!*      SELECT APINVHDR 
*!*      =gfSeek(&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'APINVHDR','VENDINV')
*!*      IF lfObj_lock(.T.,'APINVHDR',&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'VENDINV')  
*!*        SELECT APINVHDR 
*!*        =gfReplace('')
*!*        =gfTableUpdate()
*!*      ELSE
*!*        SELECT (lc_InvHdr)
*!*        DELETE 
*!*      ENDIF   
*!*    ENDSCAN
*!*    LOCATE 
*!*    DO CASE   
*!*      CASE EOF(lc_debit)
*!*        *** Message : "   No � to apply for vendor �. "
*!*        ***                  <  OK  >
*!*        =gfModalGen("TRM04026B00000","DIALOG",LANG_APAPLDB_DEBITMEMO +"|"+ALLTRIM(lcVendCode))
*!*        llFound = .F.

*!*      *** If there are no invoices for this vendor, present
*!*      *** the following message and exit.
*!*      CASE EOF(lc_InvHdr) .AND. !gfGetMemVar('LLMULCURR')
*!*        *** Message : "  No � to apply for vendor �."
*!*        ***                  <  OK  >
*!*        =gfModalGen("TRM04026B00000","DIALOG",LANG_APAPLDB_OPENINV+"|"+ALLTRIM(lcVendCode))
*!*        llFound = .F.
*!*    ENDCASE   
*!*    IF !llFound 
*!*      SELECT APVENDOR 
*!*      =gfSeek(lcVendCode)
*!*      =lfObj_lock(.F.)
*!*      SELECT APVENDOR 
*!*      =gfReplace('')
*!*      =gfTableUpdate()
*!*      LOFORMSET.ChangeMode('S')
*!*      RETURN 
*!*    ENDIF   
*!*    
  LOFORMSET.ChangeMode('E')
  *MT
ENDIF 

*!*************************************************************
*! Name      : lfvCompPhone
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/26/2009
*! Purpose   : Function called from Shared Validation of the Company and Phone
*!*************************************************************
*! Parameters: The FormSet
*!           : A Paremeter to indicate whether called from the Company or the Phone
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvCompPhone
  LPARAMETERS loFormSet, lcObjNum
  loFormSet.ariaform1.grdInvoice.RecordSource = ''
  loFormSet.ariaform1.grdDBM.RecordSource = ''
  SELECT APVENDOR
  lcSavOrder = ORDER()
  =gfSeek('')
  llBrowse = IIF(lcObjNum='1',loFormSet.AriaForm1.kbVendPhone.Selectedfrombrowse, ;
    loFormSet.AriaForm1.kbVendCompany.Selectedfrombrowse)

  IF lcObjNum = '1'
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = PADR(loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE,LEN(APVENDOR.CPHONENO))    
    loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(PADR(loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE,LEN(APVENDOR.CPHONENO)),'@R '+gfPhoneTem())
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[End] 
    lcObjName = ALLTRIM(loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE)
  ELSE
    loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = PADR(loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE,LEN(APVENDOR.CVENCOMP))
    lcObjName = ALLTRIM(loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE)
  ENDIF

  IF lcObjNum = '1' .AND. loFormSet.AriaForm1.KBVendPhone.KeyTextBox.OldValue = ;
      loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE .AND. !llBrowse
    RETURN
  ENDIF

  IF lcObjNum = '2' .AND. loFormSet.AriaForm1.KBVendCompany.KeyTextBox.OldValue = ;
    loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE .AND. !llBrowse
    RETURN
  ENDIF

  gfSETORDER(IIF(lcObjNum = '1','VENPHONE','VENCOMP'))

  lcSavExact = SET('EXACT')
  SET EXACT ON

  IF llBrowse .OR. !EMPTY(lcObjName)
    IF llBrowse .OR. !SEEK(lcObjName) .OR. ATC("?",lcObjName) > 0
      lnClosRec  = RECNO(0)
      DIMENSION laTemp[3]

      laTemp = ''

      lcBrFields = "CVENDCODE :H= '"+LANG_APAPLDB_VENDCODE+"',"+;
                  "CVENCOMP :H= '"+LANG_APAPLDB_COMPANY+"',"+;
                  "CPHONENO :H= '"+LANG_APAPLDB_PHONE+"'"

      lcFile_Ttl = LANG_APAPLDB_VENDORS

      IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF
      =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp',lcFile_Ttl )

      IF !EMPTY(laTemp[1])
        lcVendor  = laTemp[1]
        loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(laTemp[1])  &&lower
        lcPhone   = laTemp[2]
        *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
        *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = ALLTRIM(laTemp[2])        
        loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(ALLTRIM(laTemp[2]),'@R '+gfPhoneTem())
        *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
        lcCompany = laTemp[3]
        loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = ALLTRIM(laTemp[3])
      ELSE
        IF lcObjNum = '1'
          loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendPhone.KeyTextBox.OldValue
        ELSE
          loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCompany.KeyTextBox.OldValue
        ENDIF
      ENDIF
    ELSE
      IF !EOF()
        SKIP 1
        lcNxtPhone = APVENDOR.CPHONENO
        lcNxtComp  = UPPER(ALLTRIM(APVENDOR.CVENCOMP))
        IF lcObjName = IIF(lcObjNum = '1',lcNxtPhone,lcNxtComp)
          lnClosRec  = RECNO(0)
          SKIP - 1
          DIMENSION laTemp[3]

          laTemp = ''

          lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                      CVENCOMP :H= 'Company',;
                      CPHONENO :H= 'Phone'"

          lcFile_Ttl = LANG_APAPLDB_VENDORS
          IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
            GO lnClosRec
          ELSE
            GO TOP
          ENDIF

          IF lcObjNum = '1'
            =gfBrows([FOR CPHONENO = lcObjName],'CVENDCODE,CPHONENO,CVENCOMP','laTemp',lcFile_Ttl)
          ELSE
            =gfBrows([FOR UPPER(ALLTRIM(CVENCOMP)) = lcObjName],'CVENDCODE,CPHONENO,CVENCOMP','laTemp',lcFile_Ttl )
          ENDIF

          IF !EMPTY(laTemp[1])
            lcVendor  = laTemp[1]
            loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(laTemp[1])  
            lcPhone   = laTemp[2]
            *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
            *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = ALLTRIM(laTemp[2])            
            loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(ALLTRIM(laTemp[2]),'@R '+gfPhoneTem())
            *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[End] 
            lcCompany = laTemp[3]
            loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = ALLTRIM(laTemp[3])
          ELSE
            IF lcObjNum = '1'
              loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendPhone.KeyTextBox.OldValue
            ELSE
              loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCompany.KeyTextBox.OldValue
            ENDIF
          ENDIF
        ELSE
          SKIP - 1
          lcVendor  = CVENDCODE
          loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE = ALLTRIM(CVENDCODE)  &&lower
          lcPhone   = CPHONENO
          *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
          *loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = CPHONENO
          loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = TRANSFORM(CPHONENO,'@R '+gfPhoneTem())
          *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
          lcCompany = CVENCOMP
          loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = CVENCOMP
        ENDIF
      ENDIF
    ENDIF
  ELSE
    IF lcObjNum = '1'
       loFormSet.AriaForm1.KBVendPhone.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendPhone.KeyTextBox.OldValue
    ELSE
       loFormSet.AriaForm1.KBVendCompany.KeyTextBox.VALUE = loFormSet.AriaForm1.KBVendCompany.KeyTextBox.OldValue
    ENDIF
  ENDIF

  SET EXACT &lcSavExact

  loFormSet.AriaForm1.KBVendCode.ENABLED = .T.
  loFormSet.AriaForm1.KBVendPhone.ENABLED = .T.
  loFormSet.AriaForm1.KBVendCompany.ENABLED = .T.

  SELECT APVENDOR
  =gfSetOrder(lcSavOrder)
  llBrowse = .F.
  loFormSet.REFRESH()
  IF SEEK(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE,'APVENDOR')
    lfVldVend(loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE,LOFORMSET)
  ENDIF   
  SELECT APINVHDR
  
  *--end of lfvCompPhone
*!*************************************************************
*! Name      : lfvAppdate
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/26/2009
*! Purpose   : Function called to Validate App. Date
*!*************************************************************
FUNCTION lfvAppdate
PARAMETERS loFormSet
ldAppDate =loFormSet.ariaForm1.dtAppDate.value 
ldOldAppDt=loFormSet.ariaForm1.dtAppDate.oldvalue 
PRIVATE lcYear, lcPeriod
  
*** This validation function should be executed right after the user 
*** selects a valid vendor.
IF (!EMPTY(ldAppDate) .AND. ldAppDate <> ldOldAppDt) OR !loFormSet.llPassDate 
  lcYear   = loFormSet.lcFisFYear
  lcPeriod = loFormSet.lcFspPrdId 
  *** If the date is invalid, empty the date field, 
  *** as well as fiscal year, and fiscal periods variables.
  IF !lfvDtMsg(oAriaApplication.PrntCompanyID, @lcPeriod, @lcYear,loFormSet.ariaForm1.dtAppDate)
    lcFisFYear  = SPACE(4)
    lcFspPrdId  = SPACE(2)
    ldAppDate  = {}     
    loFormSet.ariaForm1.dtAppDate.value  = {}
  ELSE 
    loFormSet.lcFspPrdId = lcPeriod 
    loFormSet.lcFisFYear =  lcYear 
  ENDIF  
  ldOldAppDt = ldAppDate
  loFormSet.ariaForm1.dtAppDate.oldvalue = loFormSet.ariaForm1.dtAppDate.value 
  loFormSet.llPassDate = .T.
ENDIF

*!*************************************************************
*! Name      : lfVDtMsg
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
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

        lcMessage  = LANG_APAPLDB_HSTPROR

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
*! Developer : Mariam Mazhar (ASM)
*! Date      : 10/26/2009
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

  llOpenPrd  = .F.      && Variable to indicate that the there is a file OPEN BY the FUNCTION.

  ap1 = CREATEOBJECT("ap")

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

*!*************************************************************
*! Name      : lfGetData
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Control Source of grids assigment
*!*************************************************************
FUNCTION lfGetData
PARAMETERS loFormSet

WITH loFormSet.ariaform1.grdDBM
  .RecordSource = ''
  .RecordSource = loFormSet.lc_debit 
  .Column1.ControlSource = loFormSet.lc_debit+'.cInvNo'
  .Column2.ControlSource=  loFormSet.lc_debit+'.dInvDate'
  .Column3.ControlSource = loFormSet.lc_debit+'.cInvRef'
  .Column4.ControlSource = 'ThisFormSet.lftotApp()'
  .Column5.ControlSource = 'ThisFormSet.lfGetTotalAmt()'
  .Column6.ControlSource =  "ThisFormSet.lfGetDivDsc()"
  .Column7.ControlSource = loFormSet.lc_debit+'.cCurrCode'
  .READONLY = .T.
  .Refresh()
  .DoScroll(2)
ENDWITH 

WITH loFormSet.ariaform1.grdInvoice
  .RecordSource = ''
  .RecordSource = loFormSet.lc_InvHdr 
  .Column1.ControlSource = loFormSet.lc_InvHdr +'.cInvNo'
  .Column2.ControlSource = loFormSet.lc_InvHdr +'.dInvDate'
  .Column3.ControlSource = loFormSet.lc_InvHdr +'.dInvDuDat'
  .Column4.ControlSource = loFormSet.lc_InvHdr +'.cVenPrior'
  .Column5.ControlSource = 'ThisFormSet.lftotApp1()'
  .Column6.ControlSource = 'ThisFormSet.lfGetTotalAmt1()'
  .Column7.ControlSource = 'ThisFormSet.lfGetPayMeth()'
  .Column8.ControlSource = "ThisFormSet.lfGetDivDesc()"
  .READONLY = .T.
  .Refresh()
  .DoScroll(2)
ENDWITH 

*!*************************************************************
*! Name      : lfUndo
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : UNDO 
*!*************************************************************
FUNCTION lfUndo
PARAMETERS loFormSet
lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 
llMessg = .F.
SELECT(lc_InvHdr)
LOCATE FOR Modified = 1
IF FOUND()
  llMessg = .T.
ENDIF 
IF !llMessg 
  SELECT(lc_debit)
  LOCATE FOR Modified = 1
  IF FOUND()
    llMessg = .T.
  ENDIF 
ENDIF 

IF llMessg 
  IF MessageBox('Are you sure you want to lose all your changes?',4+16+256,_screen.Caption)=7
    RETURN
  ENDIF  
ENDIF 

SELECT(lc_InvHdr)
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX [Start]
*SCAN FOR llok_stat 
SCAN FOR llok_stat AND modified = 1
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX [END]
  =gfSeek(&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'APINVHDR','VENDINV')
  SELECT APINVHDR
  lfObj_lock(.F.)  
  =gfReplace('')
ENDSCAN 

SELECT(lc_debit)
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX [Start]
*SCAN FOR llok_stat 
SCAN FOR llok_stat AND modified = 1 
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX [END]
  =gfSeek(&lc_debit..CVENDCODE+&lc_debit..CINVNO,'APINVHDR','VENDINV')  
  SELECT APINVHDR
  lfObj_lock(.F.)  
  =gfReplace('')
ENDSCAN 
SELECT APINVHDR
=gfTableUpdate()
loFormSet.ChangeMode('S')  
loFormSet.AriaForm1.KBVendCode.KeyTextBox.SetFocus()
*!*************************************************************
*! Name      : lfGetPayMeth
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Get Payment Method Desc.
*!*************************************************************
FUNCTION lfGetPayMeth
PARAMETERS loFormSet,lcPMeth
PRIVATE lnCount
FOR lnCount = 1 TO ALEN(loFormSet.laPayMeth,1)
  IF loFormSet.laPayMeth[lnCount,2] = lcPMeth
    RETURN loFormSet.laPayMeth[lnCount,1]
  ENDIF
ENDFOR
RETURN ' '

*!*************************************************************
*! Name      : lfvAppFull
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Apply Full Button Click
*!*************************************************************
FUNCTION lfvAppFull
PARAMETERS loFormSet

lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 
lcDbNo = &lc_debit..cInvNo 
lcInvNo = &lc_InvHdr..cInvNo 
*!*  NEEDS TO REFRESH DATA IN GRIDS TO MAKE SURE THAT THIS DATA IS UP TO DATE
lcVendCode = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
IF &lc_debit..modified <> 1
  lfRefreshInv(lcVendCode ,lcDbNo ,'D')
ENDIF   
IF &lc_InvHdr..modified  <> 1
  lfRefreshInv(lcVendCode ,lcInvNo ,'I')
ENDIF   
SELECT (lc_debit)
= SEEK(PADR(lcVendCode,8) + PADR(lcDbNo,12) )
SELECT (lc_InvHdr)
= SEEK(PADR(lcVendCode,8) + PADR(lcInvNo,12) )
lfGetData(loFormSet)
lcDebtMem = &lc_debit..cInvNo
lcInvoice = &lc_InvHdr..cInvNo

PRIVATE lcCurAlias
lcDebMem    = &lc_debit..cInvNo
lcInvoice   = &lc_InvHdr..cInvNo
ldAppDate   = loFormSet.ariaForm1.dtAppDate.value 
*Message :     " Are you sure you wish to apply Debit Memo No. � [(lcDebMem)]"
*"to � [Invoice No. (lcInvoice)] ?                              "
*                <  Apply  >               <  Cancel  >         
lnAprApp = gfModalGen("QRM04159B04007","DIALOG",ALLTRIM(lcDebMem)+'|'+ LANG_APAPLDB_INVONO   +ALLTRIM(lcInvoice))
IF lnAprApp = 2
  RETURN
ENDIF

*** If application date is valid, proceed with the application process.
IF lfVldAppDt() .AND. lfRecLock() 
  IF lfVldAppl() .AND. lfApply() 
    IF !EOF(lc_InvHdr)
      SKIP IN (lc_InvHdr)
    ENDIF
  ENDIF
  lcCurAlias = ALIAS()
  SELECT APINVHDR
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF
*!*************************************************************
*! Name      : lfVldAppDt
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Application date validation
*!*************************************************************
*
FUNCTION lfVldAppDt

PRIVATE llVldDate
llVldDate = .T.
lcYear   = loFormSet.lcFisFYear
lcPeriod = loFormSet.lcFspPrdId 
ldAppDate = loFormSet.ariaForm1.dtAppDate.value 
DO CASE
  *** If application date is empty, return .F. and present
  *** the following message.
  CASE EMPTY(ldAppDate)
    llVldDate = .F.
    *** Message :  "  You have to enter �.     "
    ***                   <   OK   >
    =gfModalGen("TRM04066B00000","DIALOG",LANG_APAPLDB_APPDATE)
    loFormSet.ariaForm1.dtAppDate.SetFocus()
  *** If application date has not been validated, and is now found
  *** to be invalid, return .F. and present the following message.
  CASE !loFormSet.llPassDate .AND. ;
       !lfvDtMsg(oAriaApplication.PrntCompanyID, @lcPeriod, @lcYear,loFormSet.ariaForm1.dtAppDate)

    llVldDate  = .F.
    ldAppDate  = {}
    loFormSet.lcFisFYear = SPACE(4)
    loFormSet.lcFspPrdId = SPACE(2)
    loFormSet.ariaForm1.dtAppDate.SetFocus()
ENDCASE

IF !llVldDate
  lcCurAlias = ALIAS()
  SELECT (lcCurAlias)
ENDIF
RETURN llVldDate

*!*************************************************************
*! Name      : lfRecLock
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : *  Attempt to object lock current debit memo and current invoice
*  after checking for their validity
*!*************************************************************
FUNCTION lfRecLock
PRIVATE lcCurAlias, lcOldFilter, llRecLock, llInvLock

*** lcCurAlias  : current alias
*** lcOldFilter : current invoice filter value
*** llRecLock   : .T. if object lock is possible.
*** llInvLock   : .T. if invoice is within scope
 
*** Message :     "   � No. � has just been � by another user.                         "
***               "   Cannot apply Debit Memo No. � to Invoice No. �.  "
***                                   <  OK  >

lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 
lcVendor  = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
lcCurAlias = ALIAS()
llInvLock  = .T.
SELECT (lc_debit)
IF !EOF()
  GO RECNO()
ENDIF  
SELECT APINVHDR 
=gfSeek(&lc_debit..CVENDCODE+&lc_debit..CINVNO,'APINVHDR','VENDINV')
IF (!&lc_debit..llok_stat AND (&lc_debit..Modified = 0) AND lfObj_lock(.T.)) OR ;
   (&lc_debit..llok_stat AND &lc_debit..Modified = 1)
  SELECT APINVHDR 
  =gfReplace('')
  =gfTableUpdate()
  REPLACE llok_stat  WITH .T.,;
          Modified WITH 1 IN (lc_debit)
  SELECT (lc_Invhdr)
  IF !EOF()
    GO RECNO()
  ENDIF  
  SELECT APINVHDR 
  =gfSeek(&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'APINVHDR','VENDINV')
  IF (!&lc_InvHdr..llok_stat AND (&lc_InvHdr..Modified = 0) AND lfObj_lock(.T.)) OR ;
     (&lc_InvHdr..llok_stat AND &lc_InvHdr..Modified = 1)
     
    =gfReplace('')
    =gfTableUpdate()
    REPLACE llok_stat  WITH .T. ,;
            Modified   WITH 1  IN (lc_InvHdr)
    llRecLock = .T.
    DO CASE
      *** If the current debit memo has just been paid
   
      
      CASE &lc_debit..nInvAmnt - &lc_debit..nInvPaid - ;
           &lc_debit..nInvDisTk - &lc_debit..nInvAdj = 0
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                       LANG_APAPLDB_DBTMEMO + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                       LANG_APAPLDB_PAIDINV+ '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current debit memo has just been voided 
      CASE &lc_debit..cInvStat = 'V'
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                    LANG_APAPLDB_DBTMEMO + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                    LANG_APAPLDB_VOIDED + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current invoice has just been paid
      CASE &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid - ;
           &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj = 0
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                     LANG_APAPLDB_INVOICE + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                     LANG_APAPLDB_PAIDINV+ '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current invoice has just been voided 
      CASE &lc_InvHdr..cInvStat = 'V'
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                       LANG_APAPLDB_INVOICE + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                       LANG_APAPLDB_VOIDED + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current invoice falls out of the current invoice scope due
      *** to other user's recent editing, unlock the record and present
      *** the following message 
      *** Message :   "   Invoice No. � has just been modified   "
      ***             "   by another user.  This invoice is now  "    
      ***             "   out of scope.                          "
      ***                           <  OK  >
      CASE !gfSEEK(lcVendor+lcInvoice, "APINVHDR",'VENDINV')
        llInvLock   = gfModalGen("TRM04150B00000", "DIALOG",;
                                  ALLTRIM(lcInvoice)) = 0
        llRecLock   = .F.             
        SELECT (lc_InvHdr)
        SELECT APINVHDR
        =gfSEEK(lcVendor+lcInvoice) .AND. gfObj_Lock(.F.)
        =gfReplace('')
        =gfTableUpdate()
        SELECT (lc_InvHdr)
        REPLACE llok_stat  WITH .F. ,;
                Modified   WITH 0
        LOCATE
    ENDCASE    
    IF !llRecLock .AND. llInvLock
      SELECT APINVHDR
      =gfObj_lock(.F.)
      =gfReplace('')
      =gfTableUpdate()
      REPLACE llok_stat  WITH .F.,;
              Modified   WITH 0 IN  (lc_InvHdr)
    ENDIF
    IF !llRecLock      
      SELECT APINVHDR
      =gfSEEK(&lc_debit..CVENDCODE+&lc_debit..CINVNO) 
      =gfObj_lock(.F.)
      =gfReplace('')
      =gfTableUpdate()
      REPLACE llok_stat  WITH .F.,;
              Modified   WITH 0  IN  (lc_debit)
    ENDIF
  ELSE
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    IF !(!&lc_InvHdr..llok_stat AND (&lc_InvHdr..Modified = 0)) 
      IF !lfObj_lock(.T.)
      ENDIF
    ENDIF
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
    SELECT APINVHDR
    =gfSEEK(&lc_debit..CVENDCODE+&lc_debit..CINVNO) 
    =gfObj_lock(.F.)  
    =gfReplace('')
    =gfTableUpdate()
    llRecLock = .F.
     REPLACE llok_stat  WITH .F.,;
             Modified   WITH 0 IN  (lc_debit)
  ENDIF  
ELSE
  llRecLock = .F.
  *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
  IF !(!&lc_debit..llok_stat AND (&lc_debit..Modified = 0)) 
    IF !lfObj_lock(.T.) 
    ENDIF
  ENDIF
  *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
  
ENDIF
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)  
RETURN llRecLock .AND. llInvLock
*!***************************************************************************
*!* Name        : lfObj_Lock
*!* Developer   : Mariam MAzhar
*!* Date        : 10/26/2009
*!* Module      : Account Payable (AP)
*!* Purpose     : To issue lock or unlock to any record in any file
*!***************************************************************************
*!* Called from : ApaplDB.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfObj_Lock()
*!***************************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set,lcTable,lcKeyValue,lcIndex,llDebit
PRIVATE lnRecNo,lRet_Flag
PRIVATE lnOldrpSt
lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   IF DELETED()
     UNLOCK
     =gfModalGen('INM00095B00000','ALERT')
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
      IF !EMPTY(cLok_User)
        *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
        lcLok_User = cLok_User 
        lnOAlias = SELECT()
        *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
      
        IF ALLTRIM(cLok_User) = ALLTRIM(oAriaApplication.User_ID)
          *!Messaging the user that he cannot edit the same record
          *!from more than one session and permit him from editing
          *!the same record
          lnMsg = 0
          IF ALIAS() = 'APINVHDR'
            IF llDebit
              lcRtyCncMs = LANG_APAPLDB_EDITING + LANG_APAPLDB_DEBITMEMO +;
                           ALLTRIM(APINVHDR.CINVNO)+LANG_APAPLDB_FORVEND+;
                           ALLTRIM(CVendCode)+LANG_APAPLDB_ANOTHSESS
            ELSE
              lcRtyCncMs = LANG_APAPLDB_EDITING + LANG_APAPLDB_INVOICE +;
                           ALLTRIM(APINVHDR.CINVNO)+LANG_APAPLDB_FORVEND+;
                           ALLTRIM(CVendCode)+LANG_APAPLDB_ANOTHSESS
            ENDIF 
            lnMsg = gfModalGen("INM00274B00015","ALERT", lcRtyCncMs) 
          ELSE
            IF ALIAS() = 'APVENDOR'
              lcRtyCncMs = LANG_APAPLDB_EDITING+LANG_APAPLDB_VENDOR +;
                           ALLTRIM(APVENDOR.CVendCode)+LANG_APAPLDB_ANOTHSESS
                           
              lnMsg = gfModalGen("INM00274B00015","ALERT", lcRtyCncMs) 
            ELSE
              lnMsg = gfModalGen("INM04205B00015","ALERT")
            ENDIF   
          ENDIF   
          
          IF lnMsg  = 1
            IF TYPE('lcTable') = 'C' AND TYPE('lcKeyValue') = 'C' AND TYPE('lcIndex') = 'C'
              =gfSeek(lcKeyValue,lcTable,lcIndex)
            ENDIF
            LOOP
          ENDIF  
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE  
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          IF !USED('syuStatc')
            *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
            *=gfOpenFile('syuStatc','Cuser_id','SH','syuStatc')
            lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
            IF oAriaApplication.multiinst
              lcSQLDICPATH =  oAriaApplication.ClientA4Path+'SQLDictionary\'      
            ENDIF 
            =gfOpenFile(lcSQLDICPATH + 'syuStatc','Cuser_id','SH','syuStatc')            
            *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
          ENDIF  
          IF SEEK ('INI'+'OLDVARS'+lcLok_User ,'syuStatc') 
          *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
*!*              IF RLOCK('syuStatc')
*!*                UNLOCK IN  syuStatc 
*!*                GO gnMyStRec IN syuStatc 
*!*                =RLOCK('syuStatc')
*!*                lLok_It    = .T.
*!*              ELSE
*!*                UNLOCK
*!*                *** Display the message "Record is in use by user AAAA"
*!*                lnSavRec   = IIF(RECNO('SYUUSER')>RECCOUNT('SYUUSER'),0,;
*!*                             RECNO('SYUUSER'))
*!*                lcLok_User = ALLTRIM(PROPER(LOOKUP(syuUser.cUsr_name,cLok_User,;
*!*                             syuUser.cUser_id,'cUser_id')))
*!*                IF lnSavRec > 0
*!*                  GO lnSavRec IN SYUUSER
*!*                ENDIF  
            *            MT
           
            LOCAL lnStatcRec
            SELECT syuStatc
            SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
              lnStatcRec = RECNO()
              IF RLOCK('syuStatc')
                UNLOCK RECORD lnStatcRec IN  syuStatc 
                lLok_It    = .T.                
              ELSE
                UNLOCK RECORD lnStatcRec
                lcLok_User = oAriaApplication.getUserName(lcLok_User)
                lLok_It    = .F.
                EXIT 
              ENDIF
           ENDSCAN  
           SELECT  (lnOAlias)
           IF !lLok_It    
           *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END]           
              *** Record is in use by user ????    
              lnMsg = 0
              IF ALIAS() = 'APINVHDR'
                IF llDebit
                  lcRtyCncMs = LANG_APAPLDB_DEBITMEMO  +ALLTRIM(APINVHDR.CINVNO)+LANG_APAPLDB_FORVEND+ ALLTRIM(CVendCode)+LANG_APAPLDB_EDITED + ALLTRIM(lcLok_User)+"."                
                ELSE                
                  lcRtyCncMs = LANG_APAPLDB_INVOICE +ALLTRIM(APINVHDR.CINVNO)+LANG_APAPLDB_FORVEND+ ALLTRIM(CVendCode)+LANG_APAPLDB_EDITED + ALLTRIM(lcLok_User)+"."
                ENDIF 
                *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
                *lnMsg = gfModalGen("INM00274B00015","ALERT",lcLok_User, lcRtyCncMs) 
                lnMsg = gfModalGen("INM00274B00015","ALERT", lcRtyCncMs) 
                *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
              ELSE
                IF ALIAS() = 'APVENDOR'
                  lcRtyCncMs = " Vendor "+ ALLTRIM(APVENDOR.CVendCode)+LANG_APAPLDB_EDITED+ ALLTRIM(lcLok_User)+"."
                  *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
                  *lnMsg = gfModalGen("INM00274B00015","ALERT",lcLok_User, lcRtyCncMs)                  
                  lnMsg = gfModalGen("INM00274B00015","ALERT", lcRtyCncMs) 
                  *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
                ELSE
                  lnMsg = gfModalGen("INM00274B00015","ALERT",lcLok_User) 
                ENDIF   
              ENDIF   
              IF lnMsg = 1
                IF TYPE('lcTable') = 'C' AND TYPE('lcKeyValue') = 'C' AND TYPE('lcIndex') = 'C'
                  =gfSeek(lcKeyValue,lcTable,lcIndex)
                ENDIF 
                LOOP
              ENDIF  
              lLok_It    = .F.
              lRet_Flag  = .F.
            ENDIF
          ELSE
            lLok_It    = .T. 
          ENDIF          
          SET REPROCESS TO  lnOldrpSt
        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        IF gfModalGen("INM00029B00015","ALERT") = 1
          IF TYPE('lcTable') = 'C' AND TYPE('lcKeyValue') = 'C' AND TYPE('lcIndex') = 'C'
            =gfSeek(lcKeyValue,lcTable,lcIndex)
          ENDIF 
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
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO
*** Chek if you have to lock the record or not
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH oAriaApplication.User_ID  , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()

    lRet_Flag  = .T.    
  ENDIF
ENDIF
UNLOCK
RETURN lRet_Flag

*--> End Of Function lfObj_Lock.
*!*************************************************************
*! Name      : lfVldAppl
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Application validation
*!*************************************************************
*
FUNCTION lfVldAppl
PRIVATE llVldDate
ldAppDate   = loFormSet.ariaForm1.dtAppDate.value 
llVldDate = .T.
lcInv=LANG_APAPLDB_INVOICE 
DO CASE
  *** Check if the current debit memo has a zero priority (on hold)
  CASE &lc_debit..cVenPrior = "0" 
    lcDebitMem = SUBSTR(LANG_APAPLDB_DEBITMEMOS,1,LEN(LANG_APAPLDB_DEBITMEMOS)-1)
    *** Message : "  � has payment priority 0. � is on hold. "
    ***                  <  OK  >
    =gfModalGen("TRM04059B00000","DIALOG",;
    lcDebitMem+" "+ALLTRIM(&lc_debit..cInvNo)+"|"+;
    lcDebitMem+" "+ALLTRIM(&lc_debit..cInvNo))
    llVldDate = .F.

  *** Check if the current invoice has a zero priority (on hold)
  CASE &lc_InvHdr..cVenPrior = "0" 
    *** Message : "  � has payment priority 0. � is on hold. "
    ***                  <  OK  >
    =gfModalGen("TRM04059B00000","DIALOG",;
    lcInv +" "+ALLTRIM(&lc_InvHdr..cInvNo)+"|"+;
    lcInv +" "+ALLTRIM(&lc_InvHdr..cInvNo))
    llVldDate = .F.

  *** If application date is less than the debit memo date, 
  *** return .F. and present the following message
  CASE ldAppDate < &lc_debit..dInvDate
    
    llVldDate = .F.
    *** Message : "  The application date cannot be less than the    " 
    ***           "  � date. Cannot apply debit memo � to �. "
    ***                              <  OK  >
    =gfModalGen("TRM04036B00000","DIALOG",LANG_APAPLDB_DEBITMEMO+'|'+;
                 ALLTRIM(&lc_debit..cInvNo)+'|'+lcInv+' '+ALLTRIM(&lc_InvHdr..cInvNo))
  
  *** If application date is less than the invoice date, 
  *** return .F. and present the following message
  CASE ldAppDate < &lc_InvHdr..dInvDate
    llVldDate = .F.
    *** Message : "  The application date cannot be less than the    " 
    ***           "  � date. Cannot apply debit memo � to invoice �. "
    ***                              <  OK  >
    =gfModalGen("TRM04036B00000","DIALOG",lcInv+'|'+;
                  ALLTRIM(&lc_debit..cInvNo)+'|'+ALLTRIM(&lc_InvHdr..cInvNo))
ENDCASE
RETURN llVldDate    

*!*************************************************************
*! Name      : lfApply
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Application process
*!*************************************************************
*
FUNCTION lfApply
PARAMETERS llApplyAll
PRIVATE lnDM_OpnAm, lnDM_AprAm, lnIn_OpnAm, lnIn_AprAm,;
        lnOption, llClearApr, llClearDM, llWillApply 
*** lnDM_OpnAm  : debit memo open amount 
*** lnDM_AprAm  : debit memo approved amount 
*** lnIn_OpnAm  : invoice open amount 
*** lnIn_AprAm  : invoice approved amount 
*** lnOption    : return value of the message dialog if presented
*** llWillApply : reset it application process is cancelled by user.

lnDM_OpnAm   = &lc_debit..nInvAmnt - &lc_debit..nInvPaid ;
                   - &lc_debit..nInvDisTk - &lc_debit..nInvAdj
lnDM_AprAm   = &lc_debit..nInvAdjAp + &lc_debit..nInvDisAp ;
                     + &lc_debit..nInvAmtAp
lnIn_OpnAm   = &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid ;
                     - &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj
lnIn_AprAm   = &lc_InvHdr..nInvAdjAp + &lc_InvHdr..nInvDisAp ;
                     + &lc_InvHdr..nInvAmtAp
STORE .F. TO llClearApr, llClearDM
llWillApply  = .T.
*** Amount to be applied is the minimum of the debit memo open
*** absolute value and the invoice open amount
lnApplAmnt   = MIN(ABS(lnDM_OpnAm), lnIn_OpnAm)
*** No discount is to be applied
lnDiscAmnt   = 0     

*** Check if the debit memo has approved amounts thath may me
*** cleared by application.
*** If it does, present the following message 
*** Present the following dialog :
*** Message :  "   Debit memo No. � has a total approved amount   "
***            "   of �.  Applying this debit memo to invoice     "
***            "   No. � will clear its approved amount.          "
***                       < Apply >   < Cancel >
*** Continue with the process if the user selects to apply
IF lnDM_AprAm <> 0 .AND. lnApplAmnt > ABS(lnDM_OpnAm - lnDM_AprAm)
  lnOption    = gfModalGen(IIF(llApplyAll, "QRM04143B04009","QRM04143B04007"),;
               "DIALOG",ALLTRIM(&lc_debit..cInvNo) + '|' ;
                + ALLTRIM(STR(lnDM_AprAm, 16, 2)) + '|' +;
                ALLTRIM(&lc_InvHdr..cInvNo)) 
  llApplyAll  = llApplyAll .AND. !(lnOption = 3)    && Cancel application (all)
  llWillApply = lnOption = 1       && .f. if Skip or Cancel
  llClearDM   = llWillApply
ENDIF 
IF llWillApply
  *** Check if the invoice has approved amounts that may be cleared
  *** by application.
  *** IF the debit memo open amount is greater than the difference
  *** between between the invoice open amount and its approved amount
  IF lnIn_AprAm > 0 .AND. ;
    lnApplAmnt > (lnIn_OpnAm - lnIn_AprAm)
    *** If the open invoice amount is totally approved,
    *** present the following dialog :
    *** Message :  "  Invoice No. � is totally approved.          "
    ***            "  Applying to this invoice will clear its     "
    ***            "  approved amount. Do you wish to apply to    "
    ***            "  the full open amount,� or cancel            "
    ***            "  application ?                               " 
    ***            < Apply >     [ < Skip > ]   < Cancel >
    *** else, present the following dialog :
    *** Message :  "  Applying to invoice no. � will clear its    "
    ***            "  approved amount. Do you wish to apply to    "
    ***            "  the full open amount, to the open           "
    ***            "  unapproved amount,� or cancel appplication? " 
    ***      < Full amount >  < Unapproved amount > [ <Skip> ] < Cancel >
    lnOption = IIF(llApplyAll,;
               gfModalGen(IIF(lnIn_OpnAm = lnIn_AprAm,;
                          "QRM04144B04009", "QRM04027B04008"),;
               "DIALOG", ALLTRIM(&lc_InvHdr..cInvNo)+'|'+lcTSkip),;            
               gfModalGen(IIF(lnIn_OpnAm = lnIn_AprAm,;
                        "QRM04144B04007","QRM04027B04000"),;
                        "DIALOG", ALLTRIM(&lc_InvHdr..cInvNo)+'||'))
    DO CASE
      *** If applying unapproved amount, adjust the amount to be 
      *** applied value 
      CASE lnOption = 1
        llClearApr = .T.      && Clear approval fields
      CASE lnOption = 2 .AND. lnIn_OpnAm <> lnIn_AprAm
        lnApplAmnt = lnIn_OpnAm - lnIn_AprAm 
      OTHERWISE
        llWillApply = .F.
        llApplyAll  = !(lnIn_OpnAm = lnIn_AprAm .AND. lnOption = 3 ;
                       .OR. lnOption = 4)
    ENDCASE  
  ENDIF  
ENDIF

*** If the applied amount is greater than zero, proceed with the 
*** application process
llWillApply = llWillApply .AND. ;
       (lnApplAmnt > 0 .AND. lfApplDbMm(llClearApr, llClearDM))
lnCurALias = SELECT()
SELECT (lnCurAlias)
RETURN llWillApply 
  
*!*************************************************************
*! Name      : lfApplDbMm
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : File updates, called from application process
*!*************************************************************
*
FUNCTION lfApplDbMm
PARAMETERS llClearApr, lfClearDM
PRIVATE lcCurAlias, llApplied


lcFisFYear = loFormSet.lcFisFYear
lcFspPrdId = loFormSet.lcFspPrdId
lcapdbsess = loFormSet.lcapdbsess

lcCurAlias  = ALIAS()
llApplied   = .F.
lcVendor    = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
*** Attempt to lock the follosing records in the following files:
*!*    llCanLoc = .T.
*!*    SELECT APINVHDR 
*!*    =gfSeek(&lc_debit..CVENDCODE + &lc_debit..CINVNO,'APINVHDR','VENDINV')
*!*    IF !&lc_debit..llok_stat 
*!*      IF gfObj_Lock("APINVHDR",.T.)
*!*        SELECT APINVHDR
*!*        =gfReplace('')
*!*        =gfTableUpdate()
*!*        REPLACE llok_stat WITH .T. IN (lc_debit)
*!*      ELSE
*!*        llCanLoc = .F.
*!*      ENDIF   
*!*    ELSE
*!*      IF &lc_debit..Modified <> 1
*!*        IF gfObj_Lock("APINVHDR",.T.)
*!*          SELECT APINVHDR
*!*          =gfReplace('')
*!*          =gfTableUpdate()
*!*          REPLACE llok_stat WITH .T. IN (lc_debit)
*!*        ELSE
*!*          llCanLoc = .F.  
*!*        ENDIF   
*!*      ELSE
*!*        llCanLoc = .T.  
*!*      ENDIF 
*!*    ENDIF  
*!*    
*!*    IF llCanLoc 
*!*      =gfSeek(&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'APINVHDR','VENDINV')
*!*      IF !&lc_InvHdr..llok_stat 
*!*        IF gfObj_Lock("APINVHDR",.T.)
*!*          SELECT APINVHDR
*!*          =gfReplace('')
*!*          =gfTableUpdate()
*!*          REPLACE llok_stat WITH .T. IN (lc_InvHdr)
*!*        ELSE
*!*          =gfSeek(&lc_debit..CVENDCODE + &lc_debit..CINVNO,'APINVHDR','VENDINV')
*!*          gfObj_Lock("APINVHDR",.F.)
*!*          SELECT APINVHDR
*!*          =gfReplace('')
*!*          =gfTableUpdate()   
*!*          RETURN llApplied        
*!*        ENDIF     
*!*      ELSE
*!*        IF &lc_InvHdr..Modified <> 1
*!*          IF gfObj_Lock("APINVHDR",.T.)
*!*             SELECT APINVHDR
*!*             =gfReplace('')
*!*             =gfTableUpdate()
*!*            REPLACE llok_stat WITH .T. IN (lc_InvHdr)
*!*          ELSE
*!*            =gfSeek(&lc_debit..CVENDCODE + &lc_debit..CINVNO,'APINVHDR','VENDINV')
*!*            gfObj_Lock("APINVHDR",.F.)
*!*            SELECT APINVHDR
*!*            =gfReplace('')
*!*            =gfTableUpdate()
*!*            RETURN llApplied   
*!*          ENDIF   
*!*        ENDIF   
*!*      ENDIF   
*!*    ELSE
*!*      RETURN llApplied   
*!*    ENDIF   
  
*!*    SELECT APVENHST_A
*!*    =gfReplace('')
*!*    =gfTableUpdate()
  
*!*    IF !SEEK(lcVendor  +  loFormSet.lcFisFYear,'APVENHST')
*!*      =gfSEEK(lcVendor  +  loFormSet.lcFisFYear,'APVENHST')
*!*    ENDIF   
  
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,&lc_debit..cCurrCode)
  *MT
*!*    SELECT APVENDOR
*!*    REPLACE APVENDOR.nVenOpnDr  WITH  APVENDOR.nVenOpnDr - ROUND(lnApplAmnt &lcExSin1 &lc_debit..NEXRATE &lcExSin2 &lc_debit..NCURRUNIT,2)
*!*    =gfAdd_Info('APVENDOR')
*!*    =gfReplace("")
  SELECT(loFormSet.lcTempVend)
  REPLACE nVenOpnDr WITH nVenOpnDr + ROUND(lnApplAmnt &lcExSin1 &lc_debit..NEXRATE &lcExSin2 &lc_debit..NCURRUNIT,2)
  *MT
  *** Update the vendor's history file for the fiscal year
  *** of the application date with the applied amount and 
  *** the discount amount.
*!*    SELECT APVENHST
*!*    
*!*    REPLACE APVENHST.nVnHDisTkn WITH APVENHST.nVnHDisTkn + ROUND(lnDiscAmnt * &lc_InvHdr..NEXRATE / &lc_InvHdr..NCURRUNIT,2),;
*!*            APVENHST.nVnHDMApp WITH APVENHST.nVnHDMApp + ROUND(lnApplAmnt * &lc_debit..NEXRATE / &lc_debit..NCURRUNIT,2)
*!*    =gfAdd_Info('APVENHST')
*!*    =gfReplace("")
  
  SELECT(loFormSet.lcTempVenHst)
  REPLACE nVnHDisTkn WITH nVnHDisTkn + ROUND(lnDiscAmnt * &lc_InvHdr..NEXRATE / &lc_InvHdr..NCURRUNIT,2),;
          nVnHDMApp WITH nVnHDMApp + ROUND(lnApplAmnt * &lc_debit..NEXRATE / &lc_debit..NCURRUNIT,2)
  
  
  *** Update the current debit memo paid amount field in the debit memo file 
  *** (APINVHDR) (this is always a negative value)
  SELECT (lc_debit)
  IF llClearDM


            
    REPLACE nInvPaid   WITH nInvPaid - lnApplAmnt,;
            nInvAmtAp  WITH 0 ,;
            nInvAdjAp  WITH 0 ,; 
            nInvDisAp  WITH 0 ,;
            cBnkCode   WITH '',;
            cChkAcct   WITH '',;
            cChkGlAcc  WITH '',;
            nInvFAAp   WITH 0 
            

     
  ELSE
    REPLACE nInvPaid WITH nInvPaid - lnApplAmnt
  
  ENDIF
  =gfAdd_Info(lc_debit)            
  REPLACE  Modified WITH 1
  
  SELECT (lc_InvHdr)
  *** If llClearApr, clear the approval fields
  IF llClearApr
    REPLACE nInvDisTk WITH nInvDisTk + lnDiscAmnt ,;
            nInvAdj   WITH nInvAdj   + lnApplAmnt ,; 
            nInvAmtAp WITH 0 ,;
            nInvAdjAp WITH 0 ,; 
            nInvDisAp WITH 0 ,;
            cBnkCode  WITH '',;
            cChkAcct  WITH '',;
            cChkGlAcc WITH '',;
            nInvFAAp  WITH 0
            
   
  ELSE
    REPLACE nInvDisTk WITH nInvDisTk + lnDiscAmnt ,;
            nInvAdj   WITH nInvAdj   + lnApplAmnt 
  ENDIF
  REPLACE  Modified WITH 1
  =gfAdd_Info(lc_InvHdr)              
  
  lcNewrec = lfGetNewRec('APDIST') 
  SELECT APDIST
  m.Rec_no = lcNewrec 
  gfAppend('APDIST',.T.)
  REPLACE  cVendCode WITH  &lc_debit..cVendCode,;
           cInvNo    WITH  &lc_debit..cInvNo,;
           cAPDTrTyp WITH 'A',;
           dAPDTrDat WITH ldAppDate,;
           lAPDPost  WITH .F.,;
           cAPDRef   WITH &lc_InvHdr..cInvNo,;
           cAPDGLAct WITH &lc_InvHdr..cAPAcct,;
           nAPDAmnt  WITH lnApplAmnt + lnDiscAmnt,;
           cCurrCode WITH &lc_InvHdr..cCurrCode,;
           nExrate   WITH &lc_InvHdr..nExrate,;
           nCurrUnit WITH &lc_InvHdr..nCurrUnit,;
           nEqvAmnt  WITH ROUND((lnApplAmnt+lnDiscAmnt) &lcExSin1 &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT,2),;
           cAPDActID WITH 'A',;
           cFisFYear WITH lcFisFYear,;
           cFspPrdId WITH lcFspPrdId,;
           cAPSessNo WITH lcApDbSess
           
   =gfAdd_Info('APDIST')           
   =gfReplace('')        
 
  PRIVATE lnDiffLin1 , lnDiffLin2
  lnDiffLin1 = ROUND((lnApplAmnt+lnDiscAmnt) &lcExSin1 &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT,2)
  
  lcNewrec = lfGetNewRec('APDIST') 
  SELECT APDIST
  m.Rec_no = lcNewrec 
  gfAppend('APDIST',.T.)
  
  REPLACE  cVendCode WITH  &lc_debit..cVendCode,;
           cInvNo    WITH  &lc_debit..cInvNo,;
           cAPDTrTyp WITH 'A',;
           dAPDTrDat WITH ldAppDate,;
           lAPDPost  WITH .F.,;
           cAPDRef   WITH &lc_InvHdr..cInvNo,;
           cAPDGLAct WITH &lc_debit..cAPAcct,;
           nAPDAmnt  WITH 0 - lnApplAmnt,;
           cCurrCode WITH &lc_debit..cCurrCode,;
           nExrate   WITH &lc_debit..nExrate,;
           nCurrUnit WITH &lc_debit..nCurrUnit,;
           nEqvAmnt  WITH 0 - ROUND(lnApplAmnt &lcExSin1 &lc_debit..NEXRATE &lcExSin2 &lc_debit..NCURRUNIT,2),;
           cAPDActID WITH 'A',;
           cFisFYear WITH lcFisFYear,;
           cFspPrdId WITH lcFspPrdId,;
           cAPSessNo WITH lcApDbSess
  
  =gfAdd_Info('APDIST')           
  =gfReplace('') 

  lnDiffLin2 = 0 - ROUND(lnApplAmnt &lcExSin1 &lc_debit..NEXRATE &lcExSin2 &lc_debit..NCURRUNIT,2)
  lnExDifAmnt = -(lnDiffLin1 + lnDiffLin2)
  
  * Add a new record in the APDIST file if there is a diffrence between the exchange rates.
  IF lnExDifAmnt <> 0 
  
    lcNewrec = lfGetNewRec('APDIST') 
    SELECT APDIST
    m.Rec_no = lcNewrec 
    gfAppend('APDIST',.T.)
  
    REPLACE  cVendCode WITH  &lc_debit..cVendCode,;
             cInvNo    WITH  &lc_debit..cInvNo,;
             cAPDTrTyp WITH 'A',;
             dAPDTrDat WITH ldAppDate,;
             lAPDPost  WITH .F.,;
             cAPDRef   WITH &lc_InvHdr..cInvNo,;
             cAPDGLAct WITH loFormSet.lcExDifAcc,;
             nAPDAmnt  WITH lnExDifAmnt,;
             cCurrCode WITH oAriaApplication.BAseCurrency,;
             nExrate   WITH 1,;
             nCurrUnit WITH 1,;
             nEqvAmnt  WITH lnExDifAmnt,;
             cAPDActID WITH 'J',;
             cFisFYear WITH lcFisFYear,;
             cFspPrdId WITH lcFspPrdId,;
             cAPSessNo WITH lcApDbSess
    
    =gfAdd_Info('APDIST')           
    =gfReplace('') 
    SELECT(loFormSet.lcTempVend)
    REPLACE Nvenbal   WITH Nvenbal  + lnExDifAmnt,;
            Nvencpur  WITH Nvencpur + lnExDifAmnt
    SELECT APDIST
  ENDIF

  IF lnDiscAmnt > 0
    
    lcNewrec = lfGetNewRec('APDIST') 
    SELECT APDIST
    m.Rec_no = lcNewrec 
    gfAppend('APDIST',.T.)
  
    REPLACE  cVendCode WITH  &lc_debit..cVendCode,;
             cInvNo    WITH  &lc_debit..cInvNo,;
             cAPDTrTyp WITH 'A',;
             dAPDTrDat WITH ldAppDate,;
             lAPDPost  WITH .F.,;
             cAPDRef   WITH &lc_InvHdr..cInvNo,;
             cAPDGLAct WITH lcDiscAcct,;
             nAPDAmnt  WITH 0-lnDiscAmnt,;
             cCurrCode WITH &lc_InvHdr..cCurrCode,;
             nExrate   WITH &lc_InvHdr..nExrate,;
             nCurrUnit WITH &lc_InvHdr..nCurrUnit,;
             nEqvAmnt  WITH 0-ROUND(lnDiscAmnt &lcExSin1 &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT,2),;
             cAPDActID WITH 'S',;
             cFisFYear WITH lcFisFYear,;
             cFspPrdId WITH lcFspPrdId,;
             cAPSessNo WITH lcApDbSess
    
    =gfAdd_Info('APDIST')           
    =gfReplace('') 
  ENDIF 
  llApplied = .T.
  
*ENDIF
*=gfRLock("APVENDOR,APINVHDR,&lc_InvHdr.,APVENHST",.F.)
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
RETURN llApplied

*!*************************************************************
*! Name      : lfvSelInv
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Scope Validation
*!*************************************************************
*
FUNCTION lfvSelInv
  LPARAMETERS loFormSet
PRIVATE lcOldDiv, lcOldDOpt, lcOldInvRef, lcOldPPr, lcOldPMeth, lcOldPOpt;
        ldOldDuF, ldOldDuT, ldOldDsF, ldOldDsT, lnOldrbDat,;
        lnOldpuPM, lnOldpuDv     
        
lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 

lcDebMem   = &lc_debit..cInvNo
lcInvoice  = &lc_InvHdr..cInvNo

IF loFormSet.ariaform1.rgBScope.rbAll.Value  <> 1
  llInvOk   = .F.
  ** Save old data so as to restore it if the user cancels
  lcOldDiv    = loFormSet.lcDivision
  lcOldInvRef = loFormSet.lcInvRef
  lcOldPPr    = loFormSet.lcPayPrior
  lcOldPMeth  = loFormSet.lcPayMeth
  ldOldDuF    = loFormSet.ldDueFrom
  ldOldDuT    = loFormSet.ldDueTo
  ldOldDsF    = loFormSet.ldDiscFrom
  ldOldDsT    = loFormSet.ldDiscTo
  lnOldrbDat  = loFormSet.rbDates

  lcPayPrior = loFormSet.lcPayPrior
  lcPayMeth  =  loFormSet.lcPayMeth
  ldDueTo    = loFormSet.ldDueTo
  ldDueFrom  = loFormSet.ldDueFrom
  ldDiscTo   = loFormSet.ldDiscTo
  ldDiscFrom = loFormSet.ldDiscFrom
  lcInvRef   = loFormSet.lcInvRef
  lcDivision = loFormSet.lcDivision
  
  DO FORM (oAriaApplication.ScreenHome+"\AP\apapldbi.SCX") WITH loFormSet
  
  lcPayPrior = loFormSet.lcPayPrior
  lcPayMeth  = loFormSet.lcPayMeth
  ldDueTo    = loFormSet.ldDueTo
  ldDueFrom  = loFormSet.ldDueFrom
  ldDiscTo   = loFormSet.ldDiscTo
  ldDiscFrom = loFormSet.ldDiscFrom
  lcInvRef   = loFormSet.lcInvRef
  lcDivision = loFormSet.lcDivision
  
  lcVendCode = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
  IF llInvOk
    loFormSet.ariaform1.grdInvoice.RecordSource = ''
    
    lcWhereCond = "  cVendCode = '"+lcVendCode+;
                  "' AND (nInvAmnt - nInvPaid - nInvDistK - nInvAdj) > 0  AND cInvStat <> 'V'"
  
     
    IF loFormSet.rbDates = 1 
      IF !EMPTY(ldDueFrom)
      lcDateFlt = IIF(!EMPTY(ldDueTo),;
                    [ AND dInvDuDat BETWEEN ']+DTOS(ldDueFrom)+[' AND ']+DTOS(ldDueTo)+['],;
                    [ AND dInvDuDat >= ']+DTOS(ldDueFrom)+['])
      ELSE
        lcDateFlt = IIF(!EMPTY(ldDueTo),;
                       [ AND dInvDuDat <= ']+DTOS(ldDueTo)+['], " ")
      ENDIF
    ELSE
      IF !EMPTY(ldDiscFrom)
        lcDateFlt = IIF(!EMPTY(ldDiscTo),;
                       [ AND dInvDate+nTerDiscD BETWEEN ']+DTOS(ldDiscFrom)+[' AND ']+DTOS(ldDiscTo)+['],;
                       [ AND dInvDate+nTerDiscD >= ']+DTOS(ldDiscFrom)+['])
      ELSE
        lcDateFlt = IIF(!EMPTY(ldDiscTo),;
                      [ AND dInvDate+nTerDiscD <= ']+DTOS(ldDiscTo)+['], " ")
      ENDIF
    ENDIF  
    lcWhereCond = lcWhereCond + lcDateFlt 
  
    lcWhereCond  = lcWhereCond +  IIF(!EMPTY(lcInvRef),;
                   [ AND cInvRef LIKE '%]+lcInvRef+[%'],"") +;
                   IIF(!EMPTY(lcPayPrior),[ AND cVenPrior = ']+lcPayPrior+['],"") +;
                   IIF(!('*'$ lcDivision), [ AND cDivision = ']+lcDivision+['],"") + ;
                   IIF(!EMPTY(lcPayMeth ),[ AND cVenPMeth =']+lcPayMeth+['],"") 

    SELECT APINVHDR         
    =gfSqlRun("Select *,0 As Modified From APINVHDR Where "+lcWhereCond  ,'APINVHDR',.T.,lc_InvHdr)
    SELECT (lc_InvHdr)
    oAriaApplication.remotesystemdata.mclonestandardcursor(lc_InvHdr,SET("Datasession"))
    SELECT (lc_InvHdr)
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX [Start]
    =CURSORSETPROP("Buffering" ,3)
    INDEX on CVENDCODE+CINVNO TAG (lc_InvHdr)
    INDEX on DTOS(DINVDUDAT)  TAG 'DUEDATE' ADDITIVE                                                                                                        
    SET ORDER TO (lc_InvHdr)    
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX [END]
    LOCATE 
    WITH loFormSet.ariaform1.grdInvoice
      .RecordSource = ''
      .RecordSource = loFormSet.lc_InvHdr 
      .Column1.ControlSource = loFormSet.lc_InvHdr +'.cInvNo'
      .Column2.ControlSource = loFormSet.lc_InvHdr +'.dInvDate'
      .Column3.ControlSource = loFormSet.lc_InvHdr +'.dInvDuDat'
      .Column4.ControlSource = loFormSet.lc_InvHdr +'.cVenPrior'
      .Column5.ControlSource = 'ThisFormSet.lftotApp1()'
      .Column6.ControlSource = 'ThisFormSet.lfGetTotalAmt1()'
      .Column7.ControlSource = 'ThisFormSet.lfGetPayMeth()'
      .Column8.ControlSource = "ThisFormSet.lfGetDivDesc()"
      .READONLY = .T.
      .Refresh()
    ENDWITH 
  ENDIF 
ENDIF 
*!*************************************************************
*! Name      : lfvDscDate
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*!*************************************************************
*
FUNCTION lfvDscDate
PARAMETERS loBrForm,lnObjNo
PRIVATE lcCurrObj, lcCentSet
ldDiscTo = loBrForm.ariaForm1.dtpDisEn.value
ldDiscFrom = loBrForm.ariaForm1.dtpDisSt.value  
lcDiscTo = DTOC(ldDiscTo)
lcDiscFrom = DTOC(ldDiscFrom)
IF !EMPTY(ldDiscFrom) .AND. !EMPTY(ldDiscTo);
  .AND. ldDiscFrom > ldDiscTo 
  *** If discount from date (ldDiscFrom) > discount to date(ldDiscTo)
  *** present the following message and return the old dalue.
  *** Message : "  The � date cannot be less than the � date.  "
  ***                           <   OK   >
  lcCentSet = SET('CENTURY')
  SET CENTURY ON
  =gfModalGen("TRM04028B00000","DIALOG", LANG_APAPLDB_DISCOUNTTO+'|'+LANG_APAPLDB_DISCOUNTFROM)
  SET CENTURY &lcCentSet                                
  IF lnObjNo = 1
    loBrForm.ariaForm1.dtpDisSt.value  = loBrForm.ariaForm1.dtpDisSt.Oldvalue  
  ELSE
    loBrForm.ariaForm1.dtpDisEn.value = loBrForm.ariaForm1.dtpDisEn.Oldvalue
  ENDIF
ENDIF  
*!*************************************************************
*! Name      : lfvDueDate
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*!*************************************************************
*
FUNCTION lfvDueDate
PARAMETERS loBrForm,lnObjNo
PRIVATE lcCurrObj, lcCentSet
ldDueTo = loBrForm.ariaForm1.dtpDueEn.value 
ldDueFrom = loBrForm.ariaForm1.dtpDueSt.value  
IF !EMPTY(ldDueFrom) .AND. !EMPTY(ldDueTo) ;
  .AND. (ldDueFrom > ldDueTo)
  *** If due from date (ldDueFrom) > due to date(ldDueTo)
  *** present the following message and return the old dalue.
  *** Message : "  The � date cannot be less than the � date.  "
  ***                           <   OK   >
  lcCentSet = SET('CENTURY')
  SET CENTURY ON
  =gfModalGen("TRM04028B00000","DIALOG",LANG_APAPLDB_DUETO+'|'+LANG_APAPLDB_DUEFROM)
  SET CENTURY &lcCentSet             
  
  IF lnObjNo = 1
    loBrForm.ariaForm1.dtpDueSt.value  = loBrForm.ariaForm1.dtpDueSt.Oldvalue  
  ELSE
    loBrForm.ariaForm1.dtpDueEn.value = loBrForm.ariaForm1.dtpDueEn.Oldvalue
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfvInvOk
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Valid function for push button OK in APAPLDBI screen ( Select invoices)
*!*************************************************************
*
FUNCTION lfvInvOk
PARAMETERS loBrForm
lnCurALias = SELECT()
*** Prepare the date filter according to the final selection :
WITH loBrForm.ariaform1
  lcPayPrior = .txtVendPrior.Value  
  lcPayMeth = .cboVendPmeth.Value  
  ldDueTo = .dtpDueEn.value 
  ldDueFrom = .dtpDueSt.value 
  ldDiscTo = .dtpDisEn.value
  ldDiscFrom = .dtpDisSt.value  
  lcInvRef = .txtInvRef.Value  
  lcDivision  = .cboDivision.Value   
ENDWITH 
IF loBrForm.ariaform1.rgBDate.rbDue.Value = 1
  loBrForm.loParentFormSet.rbDates = 1 
  IF !EMPTY(ldDueFrom)
    lcDateFlt = IIF(!EMPTY(ldDueTo),;
                  [.AND. BETWEEN(dInvDuDat, ldDueFrom, ldDueTo)],;
                  [.AND. dInvDuDat >= ldDueFrom])
   ELSE
     lcDateFlt = IIF(!EMPTY(ldDueTo),;
                     [.AND. dInvDuDat <= ldDueTo], " ")
   ENDIF
ELSE
  IF !EMPTY(ldDiscFrom)
    lcDateFlt = IIF(!EMPTY(ldDiscTo),;
                  [.AND. BETWEEN(dInvDate+nTerDiscD, ldDiscFrom, ldDiscTo)],;
                  [.AND. dInvDate+nTerDiscD >= ldDiscFrom])
  ELSE
    lcDateFlt = IIF(!EMPTY(ldDiscTo),;
                    [.AND. dInvDate+nTerDiscD <= ldDiscTo], " ")
  ENDIF
  loBrForm.loParentFormSet.rbDates = 2 
ENDIF  
*** Prepare the rest of the filter expression

lcInvFlt   =".T. " + IIF(!EMPTY(lcInvRef),;
                 [.AND. LIKE(STRTRAN(UPPER(lcInvRef),' ','?'),UPPER(cInvRef))],"") +;
             IIF(!EMPTY(lcPayPrior),[.AND. cVenPrior = lcPayPrior],"") +;
             IIF(!('*'$ lcDivision), [.AND. cDivision = lcDivision  ],"") + ;
             IIF(!EMPTY(lcPayMeth ),[.AND. cVenPMeth = lcPayMeth ],"") + ;
             lcDateFlt  

SELECT (lc_InvHdr)
SET FILTER TO &lcInvFlt.
LOCATE 
SELECT (lnCurAlias)
lcVendor  = loBrForm.loParentFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
IF EOF(lc_InvHdr) 
  =gfModalGen("TRM04035B00000","DIALOG",ALLTRIM(lcVendor))
  SELECT (lc_InvHdr)
  SET FILTER TO 
  LOCATE 
ELSE  
  llInvOk = .T. 
  WITH loBrForm.loParentFormSet
    .lcPayPrior = lcPayPrior 
    .lcPayMeth=   lcPayMeth    
    .ldDueTo = ldDueTo     
    .ldDueFrom = ldDueFrom 
    .ldDiscTo = ldDiscTo    
    .ldDiscFrom = ldDiscFrom  
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    *.lcInvRef = lcInvRef    
    .lcInvRef = ALLTRIM(lcInvRef)
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
    .lcDivision = lcDivision  
  ENDWITH 
  SELECT (lc_InvHdr)
  SET FILTER TO 
  LOCATE 
  loBrForm.Release()
ENDIF  

*!*************************************************************
*! Name      : lfGetAll
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : ALl Radio Button
*!*************************************************************
*
FUNCTION lfGetAll
LPARAMETERS loFormSet
lc_InvHdr = loFormSet.lc_InvHdr          
lcVendCode = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
loFormSet.ariaform1.grdInvoice.RecordSource = ''
SELECT APINVHDR         
=gfSqlRun("Select *,0 As Modified From APINVHDR Where cVendCode = '"+lcVendCode+;
          "' AND (nInvAmnt - nInvPaid - nInvDistK - nInvAdj) > 0  AND cInvStat <> 'V'",'APINVHDR',.T.,lc_InvHdr)

SELECT (lc_InvHdr)
oAriaApplication.remotesystemdata.mclonestandardcursor(lc_InvHdr,SET("Datasession"))
SELECT (lc_InvHdr)
LOCATE 
WITH loFormSet.ariaform1.grdInvoice
  .RecordSource = ''
  .RecordSource = loFormSet.lc_InvHdr 
  .Column1.ControlSource = loFormSet.lc_InvHdr +'.cInvNo'
  .Column2.ControlSource = loFormSet.lc_InvHdr +'.dInvDate'
  .Column3.ControlSource = loFormSet.lc_InvHdr +'.dInvDuDat'
  .Column4.ControlSource = loFormSet.lc_InvHdr +'.cVenPrior'
  .Column5.ControlSource = 'ThisFormSet.lftotApp1()'
  .Column6.ControlSource = 'ThisFormSet.lfGetTotalAmt1()'
  .Column7.ControlSource = 'ThisFormSet.lfGetPayMeth()'
  .Column8.ControlSource = "ThisFormSet.lfGetDivDesc()"
  .READONLY = .T.
  .Refresh()
ENDWITH   

*!*************************************************************
*! Name      : lfVenInit
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Vendor Summary Screen Init
*!*************************************************************
*
FUNCTION lfVenInit
LPARAMETERS loBarnchFormSet
IF !USED('APVENHST_A')
  =gfOpenTable('APVENHST','VENDYEAR','SH','APVENHST_A')
ENDIF   
=gfSeek(APVENDOR.CVENDCODE+loBarnchFormSet.loParentForm.lcFisFYear,'APVENHST_A')
WITH loBarnchFormSet.ariaForm1
  .kbVendCode.keytextbox.Value  = APVENDOR.CVENDCODE
  .kbVendCompany.keytextbox.Value =  APVENDOR.CVENCOMP
  *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
  *.kbVendPhone.keytextbox.Value = APVENDOR.CPHONENO
  .kbVendPhone.keytextbox.format = gfPhoneTem()
  .kbVendPhone.keytextbox.InputMask = gfPhoneTem()
  .kbVendPhone.keytextbox.Value = TRANSFORM(APVENDOR.CPHONENO,'@R '+gfPhoneTem())
  *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
  .dtpPay.value  = apvendor.dvenlpayd
  .dtpPurch.value  = apvendor.dvenlpurd
  .dtpRec.value  = apvendor.dvenlpord
  .txt1099.Value = apvendor.nven1099b
  .txtlstpay.Value  = apvendor.cvenlpayn
  .txtOpenDeb.Value =  apvendor.nvenopndr
  .txtOpenPos.Value = apvendor.nvenopnpo
  .txtRec.Value = apvendor.nvenlpora
  .txtPur.Value = apvendor.nvenlpura
  .txtPay.Value = apvendor.nvenlpaya
  .txtYtd.Value =APVENHST_A.nVnHTotPa
  .txtCurBal.Value =  apvendor.nvenbal
ENDWITH  
*!*************************************************************
*! Name      : lfVendorSum
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Vendor Summary Screen call
*!*************************************************************
*
FUNCTION lfVendorSum
LPARAMETERS loFormSet
DO FORM (oAriaApplication.ScreenHome+"\AP\APAPLVN.SCX") WITH loFormSet

*!*************************************************************
*! Name      : lfCallDetFrm
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Debit/Invoice Summary Screen call
*!*************************************************************
*
FUNCTION lfCallDetFrm
LPARAMETERS lcType,loFormSet
DO FORM (oAriaApplication.ScreenHome+"\AP\APAPLDI.SCX") WITH lcType,loFormSet

*!*************************************************************
*! Name      : lfInitDet
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Debit/Invoice Summary Screen init
*!*************************************************************
*
FUNCTION lfInitDet
LPARAMETERS loBrnchFormSet
LOCAL loPrentForm ,lcType

loPrentForm = loBrnchFormSet.loPrentForm 
lcType = loBrnchFormSet.lcType 
lc_InvHdr = loPrentForm.lc_InvHdr 
lc_debit = loPrentForm.lc_debit 
DIMENSION loBrnchFormSet.laRemitTo[3,2]
loBrnchFormSet.laRemitTo = ''
DIMENSION laArray[ALEN(loBrnchFormSet.laRemitTo,1),2]
=ACOPY(loBrnchFormSet.laRemitTo,laArray)
lnRemitLen   = gfGetVld('cInvRemit',@laArray)
DIMENSION loBrnchFormSet.laRemitTo[ALEN(laArray,1),2]
=ACOPY(laArray,loBrnchFormSet.laRemitTo)
WITH loBrnchFormSet.ARIAForm1
  .caption = IIF(lcType = 'I' ,LANG_APAPLDB_INVINFO, LANG_APAPLDB_DBINFO)
  .LBl1099.Caption = LANG_APAPLDB_1099AMT
  .LBLAdj.Caption = LANG_APAPLDB_ADJAPP
  .lblAmt.Caption = IIF(lcType = 'I',LANG_APAPLDB_AMOUNT,LANG_APAPLDB_DBAMOUNT)
  .lblDate.Caption = IIF(lcType = 'I',LANG_APAPLDB_DATE,LANG_APAPLDB_DBDATE)
  .lblDisc.Caption = LANG_APAPLDB_DISCOUNT
  .lblDueDate.Caption = IIF(lcType = 'I',LANG_APAPLDB_DUEDATE ,LANG_APAPLDB_DBDUEDATE )
  .lblInvdbNo.Caption = IIF(lcType = 'I',LANG_APAPLDB_INVOICE,LANG_APAPLDB_DBTMEMO )
  .lblPaid.Caption =  LANG_APAPLDB_PAID
  .lblRemit.Caption = LANG_APAPLDB_REMIT
  .kbInvNo.Enabled = .F.
  .kbInvNo.keytextbox.Enabled = .F.
  .kbInvNo.keycmd.Enabled = .F.
  .cboInvRemit.ROWSource =  "ThisFormSet.laRemitTo"
  IF lcType = 'I'
    .cboInvRemit.Value = &lc_InvHdr..cinvremit
    .kbInvNo.keytextbox.VAlue = &lc_InvHdr..cinvNo
    .DtpDate.VAlue = &lc_InvHdr..dinvdate
    .DtpDue.VAlue = &lc_InvHdr..dinvdudat
    .txt1099Amt.VAlue = &lc_InvHdr..ninv1099a
    .txtamt.VAlue = &lc_InvHdr..ninvamnt
    .txtPaid.VAlue = lfPaidAmount('I')
    .txtDiscount.VAlue = &lc_InvHdr..ninvdistk
    .txtAdju.VAlue = &lc_InvHdr..nInvAdj 
  ELSE
    .cboInvRemit.Value = &lc_debit..cinvremit
    .kbInvNo.keytextbox.VAlue = &lc_debit..cinvNo
    .DtpDate.VAlue = &lc_debit..dinvdate
    .DtpDue.VAlue = &lc_debit..dinvdudat
    .txt1099Amt.VAlue = &lc_debit..ninv1099a
    .txtamt.VAlue = &lc_debit..ninvamnt
    .txtPaid.VAlue = lfPaidAmount('D')
    .txtDiscount.VAlue = &lc_debit..ninvdistk
    .txtAdju.VAlue = &lc_debit..nInvAdj 
  ENDIF   
ENDWITH  

*!*************************************************************
*! Name      : lfPaidAmount
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Get Paid Amount
*!*************************************************************
*
FUNCTION lfPaidAmount
  LPARAMETERS lcType

  LOCAL lnPaidAmount
  IF lcType = 'I'
    lnPaidAmount = &lc_InvHdr..ninvpaid + &lc_InvHdr..ninvadj
  ELSE
    lnPaidAmount = &lc_debit..ninvpaid + &lc_debit..ninvadj
  ENDIF     
  RETURN lnPaidAmount

*!*************************************************************
*! Name      : lfGetInvDM
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Refresh data
*!*************************************************************
*
FUNCTION lfGetInvDM
PARAMETERS loFormSet
lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 
lcVendCode = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
 SELECT APINVHDR
=gfSqlRun("Select *,0 As Modified From APINVHDR Where cVendCode = '"+lcVendCode+;
          "' AND (nInvAmnt - nInvPaid - nInvDistK - nInvAdj) < 0 AND cInvStat <> 'V'",'APINVHDR',.T.,lc_debit)
          
SELECT (lc_debit)
oAriaApplication.remotesystemdata.mclonestandardcursor(lc_debit,SET("Datasession"))
SELECT (lc_debit)
=CURSORSETPROP("Buffering" ,3)
INDEX on CVENDCODE+CINVNO TAG (lc_debit)
LOCATE  
SELECT APINVHDR         
=gfSqlRun("Select *,0 As Modified From APINVHDR Where cVendCode = '"+lcVendCode+;
          "' AND (nInvAmnt - nInvPaid - nInvDistK - nInvAdj) > 0  AND cInvStat <> 'V'",'APINVHDR',.T.,lc_InvHdr)
          
SELECT (lc_InvHdr)
oAriaApplication.remotesystemdata.mclonestandardcursor(lc_InvHdr,SET("Datasession"))
SELECT (lc_InvHdr)
=CURSORSETPROP("Buffering" ,3)
INDEX on CVENDCODE+CINVNO TAG (lc_InvHdr)
INDEX on DTOS(DINVDUDAT)  TAG 'DUEDATE' ADDITIVE                                                                                                        
SET ORDER TO (lc_InvHdr)
LOCATE 
*!*************************************************************
*! Name      : lfGetNewRec
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : get New rec_no 
*!*************************************************************
*
FUNCTION lfGetNewRec
LPARAMETERS lcTableName
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
RETURN ''
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
lcOldAlis = SELECT()
*N000636,3 TMI 07/27/2011 [Start] check Natives
*llNID = gfSqlRun('Select NEWID() as NEWIDVAL',lcTableName,.T.,'NEWID_CUR')
llNID = IIF(lfIsNative(lcTableName),.F.,;
            gfSqlRun('Select NEWID() as NEWIDVAL',lcTableName,.T.,'NEWID_CUR'))
*N000636,3 TMI 07/27/2011 [End  ] 
lcNewRec = ''
IF llNID 
  lcNewRec  = NEWID_CUR.NEWIDVAL
ELSE
  lcNewRec = ''
ENDIF  
SELECT(lcOldAlis)
RETURN lcNewRec 

*!*************************************************************
*! Name      : lfSaveTable
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Save Files
*!*************************************************************
*
FUNCTION lfSaveTable
PARAMETERS loFormSet
lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 



SELECT(lc_InvHdr)
SCAN FOR Modified = 1
  SCATTER MEMO MEMVAR 
  =gfSeek(&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'APINVHDR','VENDINV')
  SELECT APINVHDR
  GATHER MEMO MEMVAR 
  lfObj_lock(.F.)  
  =gfReplace('')
ENDSCAN 

SELECT(lc_debit)
SCAN FOR Modified = 1
  SCATTER MEMO MEMVAR 
  =gfSeek(&lc_debit..CVENDCODE+&lc_debit..CINVNO,'APINVHDR','VENDINV')  
  SELECT APINVHDR
  GATHER MEMO MEMVAR 
  lfObj_lock(.F.)  
  =gfReplace('')

ENDSCAN 
SELECT APINVHDR
=gfTableUpdate()

SELECT(loFormSet.lcTempVend)
REPLACE APVENDOR.nVenOpnDr WITH APVENDOR.nVenOpnDr - nVenOpnDr,;
        APVENDOR.Nvenbal   WITH APVENDOR.Nvenbal  + Nvenbal   ,;
        APVENDOR.Nvencpur  with APVENDOR.Nvencpur + Nvencpur 
=gfAdd_Info('APVENDOR')
SELECT APVENDOR
lfObj_lock(.F.)  
=gfReplace('')
=gfTableUpdate()

SELECT APVENHST
SELECT(loFormSet.lcTempVenHst)
REPLACE APVENHST.nVnHDisTkn WITH APVENHST.nVnHDisTkn + nVnHDisTkn ,;
        APVENHST.nVnHDMApp WITH APVENHST.nVnHDMApp + nVnHDMApp

=gfAdd_Info('APVENHST')
SELECT "APVENHST"
lfObj_lock(.F.)  
=gfReplace('')
=gfTableUpdate()

SELECT APDIST
=gfTableUpdate()

*!*************************************************************
*! Name      : lfBeforSave
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Before Save 
*!*************************************************************
*
FUNCTION lfBeforSave
PARAMETERS loFormSet
lcVendor  = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
SELECT APVENDOR
=gfSeek(PADR(lcVendor,8))
SELECT APVENHST
*N000636,3 TMI 07/27/2011 [Start] lock APVENHST file if fiscal year found 
*=gfSEEK(PADR(lcVendor,8)  +  loFormSet.lcFisFYear,'APVENHST')
llVenHstFnd = gfSEEK(PADR(lcVendor,8)+loFormSet.lcFisFYear,'APVENHST')
*N000636,3 TMI 07/27/2011 [End  ] 
SELECT APVENDOR
IF gfObj_Lock(.T.)
  SELECT APVENDOR
  =gfReplace('')
  =gfTableUpdate()
  SELECT "APVENHST"
  *N000636,3 TMI 07/27/2011 [Start] lock APVENHST file if fiscal year found 
  *IF gfObj_Lock(.T.)
  IF llVenHstFnd AND gfObj_Lock(.T.)
    *N000636,3 TMI 07/27/2011 [End  ] 
    SELECT APVENHST
    =gfReplace('')
    =gfTableUpdate()
    RETURN .T.
  ELSE
   SELECT APVENDOR
   gfObj_Lock(.F.) 
   =gfReplace('')
   =gfTableUpdate()
  ENDIF   
ELSE
  RETURN .F.
ENDIF   

*!*************************************************************
*! Name      : lfvAppPart
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Partially Apply button
*!*************************************************************
*
FUNCTION lfvAppPart
PARAMETERS loFormSet

lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 

PRIVATE lcCurAlias, lcOldAcct, lnDM_OpnAm, lnDM_AprAm, lnIn_OpnAm,;
        lnIn_AprAm, lnPAplAmnt, lnOldVal, lnPAplDisc, lnOption,;
        llLastBrWin, llClearApr, llClearDM

*** lcCurAlias : current alias
*** lnDM_OpnAm : debit memo open amount 
*** lnDM_AprAm : debit memo approved amount
*** lnIn_OpnAm : invoice open amount 
*** lnIn_AprAm : invoice approved amount 
*** lnPAplAmnt : Partial amount to be applied
*** lnPAplDisc : Partial discount to be applied
*** lnOption   : return value of the message dialog if presented
lcDbNo = &lc_debit..cInvNo 
lcInvNo = &lc_InvHdr..cInvNo 
*!*  NEEDS TO REFRESH DATA IN GRIDS TO MAKE SURE THAT THIS DATA IS UP TO DATE
lcVendCode = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
*lfGetInvDM(loFormSet)
IF &lc_debit..modified <> 1
  lfRefreshInv(lcVendCode ,lcDbNo ,'D')
ENDIF   
IF &lc_InvHdr..modified <> 1
  lfRefreshInv(lcVendCode ,lcInvNo ,'I')
ENDIF   
SELECT (lc_debit)
= SEEK(PADR(lcVendCode,8) + PADR(lcDbNo,12) )
SELECT (lc_InvHdr)
= SEEK(PADR(lcVendCode,8) + PADR(lcInvNo,12) )
lfGetData(loFormSet)

SELECT APSETUP
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
*=gfSeek('')
=gfSeek('','APSETUP','APSETUP')
*N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
GO TOP
lcDebMem    = &lc_debit..cInvNo
lcInvoice   = &lc_InvHdr..cInvNo
lcAplDbMm = lcDebMem 
lcAplInv  = lcInvoice   
ldAppDate =loFormSet.ariaForm1.dtAppDate.value 
STORE .F. TO llClearApr, llClearDM
IF lfVldAppDt() .AND. lfRecLock()
  IF lfVldAppl()
    lcOldAcct = loFormSet.lcEmptyAcc
    lcOldDesc = SPACE(65)
    lcAccDesc = SPACE(65)

    lnDM_OpnAm  = &lc_debit..nInvAmnt - &lc_debit..nInvPaid ;
                   - &lc_debit..nInvDisTk - &lc_debit..nInvAdj
    lnDM_AprAm  = &lc_debit..nInvAdjAp + &lc_debit..nInvDisAp ;
                   + &lc_debit..nInvAmtAp
    lnIn_OpnAm  = &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid ;
                   - &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj
    lnIn_AprAm  = &lc_InvHdr..nInvAdjAp + &lc_InvHdr..nInvDisAp ;
                   + &lc_InvHdr..nInvAmtAp
    lnOldVal    = 0
    *lnPAplAmnt  = MIN(ABS(lnDM_OpnAm), lnIn_OpnAm)
                              && partial amount to be applied default
                              && by the available application amount
    lnPAplAmnt  = 0          && partial amount to be applied default by 0
    lnPAplDisc  = 0          && partial disacount to be applied is now 0
    llApply     = .F.
    lcDiscAcct  = IIF(gfSEEK(&lc_InvHdr..cDivision,'APDIV') ;
                    .AND. !EMPTY(APDIV.cDiscAcct), ;
                      APDIV.cDiscAcct, APSETUP.cDiscAcct) 
    IF EMPTY(lcDiscAcct)  
      lcDiscAcct = loFormSet.lcEmptyAcc
      lcAccDesc  = SPACE(60)
    ELSE
      lcAccDesc  = IIF(loFormSet.llApGlLink, IIF(gfSeek(lcDiscAcct,'lcLinkChar'),ALLTRIM(lcLinkChar.CACCNLDES),''),'')
*      lcAccDesc  = IIF(llApGlLink, ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,;
                       lcDiscAcct,lcLinkChar.CACCTCODE,"ACCTCODE")),' ')
    ENDIF     
 
    DO FORM (oAriaApplication.ScreenHome+'\AP\APAPLDBP.Scx')
  
    IF llApply 
      lcDebMem   = &lc_debit..cInvNo
      lcInvoice  = &lc_InvHdr..cInvNo
      lnApplAmnt = lnPAplAmnt
      lnDiscAmnt = lnPAplDisc
      IF lfApplDbMm(llClearApr, llClearDM)
      ENDIF
    ENDIF  
  ENDIF    
  *** Release object locks
  lcCurAlias = ALIAS()
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF


*!*************************************************************
*! Name      : lfvApply
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Valid function for push button : < Apply > from partial application 
*  screen
*!*************************************************************
*
FUNCTION lfvApply
PARAMETERS loPartFormSet
PRIVATE lcMessg, lnDivision


lnPAplDisc = loPartFormSet.ariaForm1.txtDicAmt.Value
lnPAplAmnt = loPartFormSet.ariaForm1.txtAppAmt.Value
lcDiscAcct = loPartFormSet.ariaForm1.kbGlAct.keytextbox.Value
*** proceed with the application process
llApply    = .T.
DO CASE
  *** If the amount to be applied is less than or equal to zero,
  *** present the following message and do not apply.
  CASE lnPAplAmnt <= 0
    *** Message :  "     The amount to be applied should be greater    "
    ***            "     than zero.                                    " 
    =gfModalGen("TRM04029B00000","DIALOG")
    llApply = .F.
  
  *** If a GL company exists in the setup file (APSETUP),
  *** Look for the discount account in its chart of accounts
  *** file
  CASE lnPAplDisc > 0 .AND. loFormSet.llApGlLink
    *** If no account is entered, present the following message
    IF VAL(STRTRAN(lcDiscAcct,'-','0')) = 0 .OR.;
       EMPTY(STRTRAN(lcDiscAcct,'-'))
       *** Message :  "  You have to enter �.     "
       ***                   <   OK   >
       =gfModalGen("TRM04066B00000","DIALOG",LANG_APAPLDB_DISCACC)
       loPartFormSet.ariaForm1.kbGlAct.keytextbox.SetFocus()
       llApply = .F.
    ELSE
      *** If the acccount is not found in the chart of accounts
      *** file of the link company, present the following message.
      *** Message :  "    The discount account in �  does not      "
      ***            "    exist in the chart of accounts. You      "
      ***            "    have to enter another discount account,  "
      ***            "    or clear the discount amount.            "
      ***                        <   OK   >
  
      IF loFormSet.llApGlLink .AND. !gfSEEK(lcDiscAcct, 'lcLinkChar')
        IF  gfSEEK(&lc_InvHdr..cDivision,'APDIV') ;
           .AND. !EMPTY(APDIV.cDiscAcct) 

          lcMessg    = LANG_APAPLDB_TDiv + ' ' + gfCodDes(&lc_InvHdr..cDivision , 'CDIVISION')
          
        ELSE
          lcMessg    = LANG_APAPLDB_TApSetup
        ENDIF            
        
        =gfModalGen("TRM04038B00000","DIALOG",lcMessg)
        llApply = .F.
        loPartFormSet.ariaForm1.kbGlAct.keytextbox.SetFocus()
      ENDIF
    ENDIF    
    
  *** Check if the debit memo has approved amounts that may be
  *** cleared by application.
  *** If it does, present the following message 
  *** Present the following dialog :
  *** Message :  "   Debit memo No. � has a total approved amount   "
  ***            "   of �.  Applying this debit memo to invoice     "
  ***            "   No. � will clear its approved amount.          "
  ***                       < Apply >   < Cancel >
  CASE lnDM_AprAm <> 0 .AND. ;
    lnPAplAmnt > ABS(lnDM_OpnAm - lnDM_AprAm) 

    IF gfModalGen("QRM04143B04007","DIALOG",;
              ALLTRIM(&lc_debit..cInvNo) + '|' + ;
              ALLTRIM(STR(lnDM_AprAm, 16, 2)) + '|' +;
              ALLTRIM(&lc_InvHdr..cInvNo)) = 1
      llClearDM = .T.
    ELSE
      llApply = .F.
    ENDIF
ENDCASE

IF llApply 
  loPartFormSet.Release()
ENDIF

*!*************************************************************
*! Name      : lfvAplAmnt
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Validate function for Apply Amount
*!*************************************************************
*
FUNCTION lfvAplAmnt
PARAMETERS loApFormSet

lnOldVal = loApFormSet.AriaForm1.txtAppAmt.OldValue
lnPAplAmnt = loApFormSet.AriaForm1.txtAppAmt.Value
PRIVATE llValid
IF lnPAplAmnt <> lnOldVal
  llValid = .T.
  DO CASE
    *** If the applied amount is negative, present a message
    *** and return
    CASE lnPAplAmnt < 0
      *** Message : "   Negative values are not allowed.   "  
      ***                      <   OK   >
      =gfModalGen("TRM04087B00000","DIALOG")
      llValid = .F.  
      
    *** If the amount to be applied is greater than the open
    *** debit memo amount, present a message and do not accept.
    CASE lnPAplAmnt > ABS(lnDM_OpnAm)
      *** Message : " � amount cannot be greater than the open �amount. "
      ***                      <   OK   >
      =gfModalGen("TRM04015B00000","DIALOG",LANG_APAPLDB_APPDB)
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the invoice open amount, present a message
    *** and do not accept
    CASE (lnPAplAmnt + lnPAplDisc) > lnIn_OpnAm 
      *** Message : " � amount cannot be greater than the open �amount.
      ***                      <   OK   >
      =gfModalGen("TRM04015B00000","DIALOG",LANG_APAPLDB_APPINV)
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the difference between the invoice open amount, 
    *** and the invoioce approved amount, present a message
    *** and proceed
    CASE lnIn_AprAm > 0 ;
         .AND. (lnPAplAmnt + lnPAplDisc) > (lnIn_OpnAm - lnIn_AprAm) 
      *** Message :  "  Total amount applied to this invoice will   " 
      ***            "  clear its approved amounts.                 "
      ***                      <   OK   >
      =gfModalGen("TRM04030B00000","DIALOG")
      llClearApr  = .T.
   ENDCASE
   IF !llValid
      loApFormSet.AriaForm1.txtAppAmt.Value = lnOldVal
      RETURN 0
   ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfvDscAmnt
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Validate function for discount Amount
*!*************************************************************
*
FUNCTION lfvDscAmnt
PARAMETERS loApFormSet

lnOldVal = loApFormSet.AriaForm1.txtDicAmt.OldValue
lnPAplDisc = loApFormSet.AriaForm1.txtDicAmt.Value


IF lnPAplDisc <> lnOldVal
  llValid = .T.
  DO CASE

    *** If the discount amount is negative, present a message
    *** and return
    CASE lnPAplDisc < 0
      *** Message : "   Negative values are not allowed.   "  
      ***                      <   OK   >
      =gfModalGen("TRM04087B00000","DIALOG")
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the invoice open amount, present a message
    *** and do not accept
    CASE (lnPAplAmnt + lnPAplDisc) > lnIn_OpnAm 
      *** Message : " � amount cannot be greater than the open �amount.
      ***                      <   OK   >
      =gfModalGen("TRM04015B00000","DIALOG",LANG_APAPLDB_APPINV)
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the difference between the invoice open amount, 
    *** and the invoioce approved amount, present a message
    *** and proceed
    CASE (lnPAplAmnt + lnPAplDisc) > (lnIn_OpnAm - lnIn_AprAm) 
      *** Message :  "  Total amount applied to this invoice will   " 
      ***            "  clear its approved amounts.                 "
      ***                      <   OK   >
      =gfModalGen("TRM04030B00000","DIALOG")
  ENDCASE
  IF !llValid
    lnPAplDisc = lnOldVal
    loApFormSet.AriaForm1.txtDicAmt.Value = loApFormSet.AriaForm1.txtDicAmt.OldValue
    RETURN 0
  ENDIF
  loApFormSet.AriaForm1.kbGlAct.keytextbox.value = lcDiscAcct
  loApFormSet.AriaForm1.txtAccDesc.Value  = lcAccDesc  
  IF lnPAplDisc > 0 
    loApFormSet.AriaForm1.kbGlAct.Enabled = .T.
  ELSE
    loApFormSet.AriaForm1.kbGlAct.Enabled = .F.
  ENDIF   
  loApFormSet.AriaForm1.txtAccDesc.Enabled = .F.
  RETURN 1
ENDIF  

*!*************************************************************
*! Name      : lfvDisAcct
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Validate function for discount Account
*!*************************************************************
*
FUNCTION lfvDisAcct
PARAMETERS loApFormSet
lcOldAcct = loApFormSet.AriaForm1.kbGlAct.keytextbox.oldvalue 
lcDiscAcct = loApFormSet.AriaForm1.kbGlAct.keytextbox.value 
*!*  llApGlLink = (APSETUP.CAPSGLLINK = 'Y')
loApFormSet.AriaForm1.txtAccDesc.Value =IIF(loFormSet.llApGlLink, IIF(gfSeek(lcDiscAcct,'lcLinkChar'),ALLTRIM(lcLinkChar.CACCNLDES),''),'')


*!*************************************************************
*! Name      : lfRefreshInv
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Refresh Data
*!*************************************************************
*
FUNCTION lfRefreshInv
LPARAMETERS lcVendr,lcInvoice,lcType
IF lcType = 'D'
  SELECT APINVHDR
  =gfSqlRun("Select *,0 As Modified From APINVHDR Where cVendCode = '"+lcVendr+"' AND CINVNO='"+lcInvoice+"'"+;
            " AND (nInvAmnt - nInvPaid - nInvDistK - nInvAdj) < 0 AND cInvStat <> 'V'",'APINVHDR',.T.,'Temp_Dep')
  IF USED('Temp_Dep')            
    SELECT 'Temp_Dep'
    oAriaApplication.remotesystemdata.mclonestandardcursor('Temp_Dep',SET("Datasession"))
    LOCATE 
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    IF !EOF()
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
      SCATTER MEMO MEMVAR 
      SELECT(lc_debit)
      GATHER MEMO MEMVAR 
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    ENDIF
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[ENd]  
  ENDIF 
ENDIF             
            
IF lcType = 'I'          
  SELECT APINVHDR         
  =gfSqlRun("Select *,0 As Modified From APINVHDR Where cVendCode = '"+lcVendCode+"' AND CINVNO='"+lcInvoice+"'"+;
          " AND (nInvAmnt - nInvPaid - nInvDistK - nInvAdj) > 0  AND cInvStat <> 'V'",'APINVHDR',.T.,'Temp_Inv')
          
  IF USED('Temp_Inv')            
    SELECT 'Temp_Inv'
    oAriaApplication.remotesystemdata.mclonestandardcursor( 'Temp_Inv',SET("Datasession"))
    LOCATE 
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    IF !EOF()
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
    
    SCATTER MEMO MEMVAR 
    SELECT(lc_InvHdr)
    GATHER MEMO MEMVAR 
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[Start] 
    ENDIF
    *N000636,3 MMT 10/11/2011 Enable the screen to work in A4xp FOX[END] 
  ENDIF 
ENDIF            
*!*************************************************************
*! Name      : lfvAppAll
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Apply All button
*!*************************************************************
*
FUNCTION lfvAppAll
PARAMETERS loFormSet
lc_InvHdr = loFormSet.lc_InvHdr 
lc_debit = loFormSet.lc_debit 
PRIVATE lcCurAlias, lnCurTag, llApplied, llApplyAll
lcDebMem   = &lc_debit..cInvNo
lcInvoice  = &lc_InvHdr..cInvNo
ldAppDate   = loFormSet.ariaForm1.dtAppDate.value 
* Message :     " Are you sure you wish to apply Debit Memo No. � [(lcDebMem)]"
* "to � [all invoices] ?                                        "
*<  Apply  >               <  Cancel  >         
lnAprApp = gfModalGen("QRM04159B04007","DIALOG",ALLTRIM(lcDebMem)+'|'+ LANG_APAPLDB_ALLINVOICE)
IF lnAprApp = 2
  RETURN
ENDIF
lcVendCode = loFormSet.AriaForm1.KBVendCode.KeyTextBox.VALUE  
SELECT (lc_InvHdr)
SCAN FOR &lc_InvHdr..modified  <> 1
  lfRefreshInv(lcVendCode ,&lc_InvHdr..CINVNO,'I')
ENDSCAN 




llApplied  = .F.
llApplyAll = .T.
*** If application date is empty, return .F. and present
*** the following message.
IF lfVldAppDt()
  lcCurAlias = ALIAS()
  *** Attempt to lock debit memo record
  *** If locking succeeds, proceed
  SELECT APINVHDR 
  =gfSeek(&lc_debit..CVENDCODE+&lc_debit..CINVNO,'APINVHDR','VENDINV')
  IF (!&lc_debit..llok_stat AND (&lc_debit..Modified = 0) AND lfObj_lock(.T.)) OR ;
     (&lc_debit..llok_stat AND &lc_debit..Modified = 1)
    SELECT APINVHDR 
    =gfReplace('')
    =gfTableUpdate()
    REPLACE llok_stat  WITH .T.,;
            Modified   WITH 1 IN (lc_debit)
    DO CASE
      *** Check if the debit memo has just been paid by another user
      CASE &lc_debit..nInvAmnt - &lc_debit..nInvPaid - ;
           &lc_debit..nInvDisTk - &lc_debit..nInvAdj = 0
        *** Message :     "   � No. � has just been � by another user!  "
        ***               "   Cannot apply Debit Memo No. � to Invoice  "
        ***               "   No. �.                                    "
        ***                                   <  OK  >
        =gfModalGen("TRM04071B00000","DIALOG",;
                       LANG_APAPLDB_DBTMEMO + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                       LANG_APAPLDB_DBTMEMO + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** Check if the debit memo has just been voided by another user
      CASE &lc_debit..cInvStat = 'V'
        *** Message :     "   � No. � has just been � by another user!  "
        ***               "   Cannot apply Debit Memo No. � to Invoice  "
        ***               "   No. �.                                    "
        ***                                   <  OK  >
        =gfModalGen("TRM04071B00000","DIALOG",;
                    LANG_APAPLDB_DBTMEMO + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                    LANG_APAPLDB_VOIDED+ '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** Check if the current debit memo has a zero priority (on hold)
      CASE &lc_debit..cVenPrior = "0" 
        lcDebitMem = LANG_APAPLDB_DEBITMEMO
        *** Message : "  � has payment priority 0. � is on hold. "
        ***                  <  OK  >
        =gfModalGen("TRM04059B00000","DIALOG",;
        lcDebitMem+" "+ALLTRIM(&lc_debit..cInvNo)+"|"+;
        lcDebitMem+" "+ALLTRIM(&lc_debit..cInvNo))

      *** If application date is less than the debit memo date, 
      *** return .F. and present the following message
      CASE ldAppDate < &lc_debit..dInvDate
        *** Message : "  The application date cannot be less than the    " 
        ***           "  � date. Cannot apply debit memo � to invoice �. "
        ***                              <  OK  >
        =gfModalGen("TRM04036B00000","DIALOG",LANG_APAPLDB_DEBITMEMO+'|'+;
                      ALLTRIM(&lc_debit..cInvNo)+'|'+LANG_APAPLDB_ALLINV    )

      OTHERWISE
        SELECT (lc_InvHdr)
        lnCurTag   = ORDER()
        *** Tag expression is DTOS(dInvDuDat)
        SET ORDER TO 'DUEDATE'
        GO TOP
        
        lcDbMem    = &lc_debit..cInvNo
        SCAN WHILE llApplyAll .AND. &lc_debit..cInvNo = lcDbMem .AND.;
                    ((&lc_debit..nInvAmnt - &lc_debit..nInvPaid - ;
                    &lc_debit..nInvDisTk - &lc_debit..nInvAdj ) < 0)
                    
        SELECT APINVHDR 
        =gfSeek(&lc_InvHdr..CVENDCODE+&lc_InvHdr..CINVNO,'APINVHDR','VENDINV')
        IF (!&lc_InvHdr..llok_stat AND (&lc_InvHdr..Modified = 0) AND lfObj_lock(.T.)) OR ;
           (&lc_InvHdr..llok_stat AND &lc_InvHdr..Modified = 1)
          SELECT   APINVHDR 
          =gfReplace('')
          =gfTableUpdate()
          REPLACE llok_stat  WITH .T.,;
                  Modified   WITH 1 IN (lc_InvHdr)                    
                    
       *   IF gfObj_Lock(.T.)
*!*              =lfRefresh(lcApDbWin1+IIF(WVISIBLE('CWRAPAPLVN'), ",CWRAPAPLVN", "")+;
*!*                        IIF(WVISIBLE('CWRAPAPLDM'), ",CWRAPAPLDM", "")+;
*!*                        IIF(WVISIBLE('CWRAPAPLIN'), ",CWRAPAPLIN", ""))
            lcInv = LANG_APAPLDB_INVOICE
            DO CASE
              *** Check if the invoice has just been paid by another user
              CASE &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid - ;
                   &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj = 0
                *** Message : "  � No. � has just been � by another user!  "
                ***           "  Cannot apply Debit Memo No. � to Invoice  "
                ***           "  No. �.                                    "
                ***                                   <  OK  >
                =gfModalGen("TRM04071B00000","DIALOG",;
                     LANG_APAPLDB_INVOICE + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                     LANG_APAPLDB_PAIDINV+ '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0
         
              *** Check if the invoice has just been voided by another user
              CASE &lc_InvHdr..cInvStat = 'V'
                *** Message : "   � No. � has just been � by another user!  "
                ***           "   Cannot apply Debit Memo No. � to Invoice  "
                ***           "   No. �.                                    "
                ***                                   <  OK  >
                =gfModalGen("TRM04071B00000","DIALOG",;
                       LANG_APAPLDB_INVOICE + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                       LANG_APAPLDB_VOIDED + '|' + ALLTRIM(&lc_debit..cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

              *** Check if the current invoice has a zero priority (on hold)
              *** Message : "  � has zero payment priority.  � is on hold.  "
              ***           "  Do you wish to continue applying debit memo  "
              ***           "  � to the rest of the invoices?               "
              ***                      < Continue >    < Stop >
              CASE &lc_InvHdr..cVenPrior = "0" 
                IF gfModalGen("QRM04063B04002","DIALOG",;
                   LANG_APAPLDB_INVOICE +" "+ALLTRIM(&lc_InvHdr..cInvNo)+"|"+;
                   LANG_APAPLDB_INVOICE +" "+ALLTRIM(&lc_InvHdr..cInvNo)+"|"+;
                   ALLTRIM(&lc_debit..cInvNo)) = 2
                  EXIT      
                ENDIF

              *** If application date is less than the invoice date, 
              *** return .F. and present the following message
              *** Message : "   The application date cannot be less than     " 
              ***           "   the � date. Cannot apply debit memo � to     "
              ***           "   invoice �. Do you want to continue applying  "
              ***           "   debit memo � to the rest of the invoices ?   "
              ***                      < Continue >    < Stop >
              CASE (ldAppDate < &lc_InvHdr..dInvDate) 
                IF gfModalGen("QRM04037B04002","DIALOG",LANG_APAPLDB_INVOICE +'|'+;
                             ALLTRIM(&lc_debit..cInvNo)+'|'+;
                             ALLTRIM(&lc_InvHdr..cInvNo)+'|'+;
                             ALLTRIM(&lc_debit..cInvNo)) = 2
                  EXIT
                ENDIF
              OTHERWISE
                llApplied = lfApply(@llApplyAll) .OR. llApplied 
            ENDCASE

          ENDIF    
        ENDSCAN
        SET ORDER TO (lnCurTag)
        GO TOP
     ENDCASE
     SELECT (lc_InvHdr)
 
  ENDIF  
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF


*!*************************************************************
*! Name      : lfGrdBMRowCol
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Grd DB After Row Col.
*!*************************************************************
*
FUNCTION lfGrdBMRowCol
PARAMETERS loFormSet
lc_debit = loFormSet.lc_debit 
loFormSet.AriaForm1.txtDbtMem.Value =&lc_debit..CinvNo

*!*************************************************************
*! Name      : lfGrdINVRowCol
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Grd Invoice After Row Col.
*!*************************************************************
*
FUNCTION lfGrdINVRowCol
PARAMETERS loFormSet
lc_InvHdr = loFormSet.lc_InvHdr 
loFormSet.AriaForm1.txtInvoice.Value =&lc_InvHdr..CinvNo


*!*************************************************************
*! Name      : lfIsNative
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 07/21/2011 
*! Purpose   : check if table is fox
*!*************************************************************
*N000636,3 TMI 07/21/2011 
FUNCTION lfIsNative
PARAMETERS lcAlias

lcAlias = UPPER(lcAlias)
LOCAL llNative,lcTempCurs,lnSlct
lnSlct = SELECT(0)
lcTempCurs = gfTempName()
llNative = .T.
*<Write here the code that checks if this table is native>
lnRemResult = oAriaApplication.RemoteSystemData.Execute("Select * from SYDFILES WHERE CFILE_NAM = '&lcAlias'",'',"&lcTempCurs","",oAriaApplication.cAria4sysfiles,3,"",SET("Datasession"))
SELECT (lcTempCurs)
LOCATE 
llNative = !FOUND()
USE IN (lcTempCurs)

SELECT (lnSlct)
RETURN llNative
*!*************************************************************
*! Name      : lfChkScp
*! Developer : Mariam MAzhar(MMT)
*! Date      : 11/01/2009
*! Purpose   : Enable/Disable Scope
*!*************************************************************
FUNCTION lfChkScp
PARAMETERS loFormSet
lc_debit = loFormSet.lc_debit 
lc_InvHdr = loFormSet.lc_InvHdr 
lcAlis = SELECT(0)
lnRecNumD = RECNO(lc_debit)
lnRecNumH = RECNO(lc_InvHdr)
SELECT (lc_debit)
LOCATE FOR Modified = 1 
IF FOUND()
  loFormSet.AriaForm1.rGBScope.Enabled = .F.  
  loFormSet.AriaForm1.rGBScope.rbAll.Enabled = .F.  
  loFormSet.AriaForm1.rGBScope.rbSelect.Enabled = .F.  
ENDIF 

IF BETWEEN(lnRecNumD ,1,RECCOUNT())
  GO RECORD lnRecNumD 
ENDIF

IF loFormSet.AriaForm1.rGBScope.Enabled 
  SELECT(lc_InvHdr )
  LOCATE FOR Modified = 1 
  IF FOUND()
    loFormSet.AriaForm1.rGBScope.Enabled = .F.  
    loFormSet.AriaForm1.rGBScope.rbAll.Enabled = .F.  
    loFormSet.AriaForm1.rGBScope.rbSelect.Enabled = .F.  
  ENDIF 
  IF BETWEEN(lnRecNumH ,1,RECCOUNT())
    GO RECORD lnRecNumH 
  ENDIF 
ENDIF   
SELECT(lcAlis)