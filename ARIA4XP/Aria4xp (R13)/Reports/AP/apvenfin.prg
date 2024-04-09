*:***************************************************************************
*: Program file  : APVENFIN
*: Program desc. : AP VENDOR
*: System        : ARIA4XP
*: Module        : Accounts Payable(AP)
*: Developer     : Mohamed hamdy (MHM)
*: Date          : 09/28/2009
*:***************************************************************************
*:Modification
*:************************************************************************
#INCLUDE R:\aria4xp\reports\ap\apjorn.h

Dimension laPayMethod[1,2]
=gfGetVld('CVENPMETH',@laPayMethod)

If loOgScroll.llOGFltCh
  =lfCrTemp() &&To create Temp File
  =lfColData() &&Data Collection
Endif

Set Step On
*- set the correct order before getting data
Select &lcTempFile
lRun = .T.
Do Case

Case lcRpSort  = 'P'
  lcRpForm = 'APVNFIN'
  Set Order To 'PAYMENT' && CVENDCODE+NPAYDOCNO+CINVNO

Case lcRpSort  = 'V'
  lcRpForm = 'APVNFINI'
  Set Order To 'INVOICE' && CVENDCODE+CINVNO+NPAYDOCNO
Otherwise
  *Don't run
  lRun = .F.
Endcase
If lRun = .T.
  Select &lcTempFile
  Locate
  loOgScroll.lcOGLastForm = lcRpForm
  Do gfDispRe With Eval('lcRpForm')
Endif
*!**************************************************************************
*! Function      : lfCrTemp
*! Purpose       : Creating Temp file
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 09/28/2009
*!**************************************************************************
Function lfCrTemp
Local lnSlct
lnSlct = Select(0)

If Used(lcTempFile)
  Select (lcTempFile)
  Zap
  Select (lnSlct)
  Return
Endif

Dimension laTempStru[14,18]

Local lnI,lnJ
lnI = 0
lnI = lnI + 1
laTempStru[lnI,1]='CVENDCODE'
laTempStru[lnI,2]='C'
laTempStru[lnI,3]= 8
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='CPAYMETH'
laTempStru[lnI,2]='C'
laTempStru[lnI,3]= 1
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='CINVNO'
laTempStru[lnI,2]='C'
laTempStru[lnI,3]= 12
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NINVAMNT'
laTempStru[lnI,2]='N'
laTempStru[lnI,3]= 15
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='DINVDATE'
laTempStru[lnI,2]='D'
laTempStru[lnI,3]= 8
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NINVAMTAP'
laTempStru[lnI,2]='N'
laTempStru[lnI,3]= 15
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NPAYDOCNO'
laTempStru[lnI,2]='C'
laTempStru[lnI,3]= 8
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='DINVDUDAT'
laTempStru[lnI,2]='D'
laTempStru[lnI,3]= 8
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='DAPDTRDAT'
laTempStru[lnI,2]='D'
laTempStru[lnI,3]= 8
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NINVPAID'
laTempStru[lnI,2]='N'
laTempStru[lnI,3]= 15
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='CDESC'
laTempStru[lnI,2]='C'
laTempStru[lnI,3]= 30
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NINVAMTAPP'
laTempStru[lnI,2]='N'
laTempStru[lnI,3]= 15
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NPAYAMNT'
laTempStru[lnI,2]='N'
laTempStru[lnI,3]= 15
laTempStru[lnI,4]= 0

lnI = lnI + 1
laTempStru[lnI,1]='NPAYAMNTO'
laTempStru[lnI,2]='N'
laTempStru[lnI,3]= 15
laTempStru[lnI,4]= 0
*- Update other array elements
For lnI = 1 To Alen(laTempStru,1)
  For lnJ = 7 To 16
    laTempStru[lnI,lnJ] = ''
  Endfor
  Store 0 To laTempStru[lnI,17],laTempStru[lnI,18]
Endfor

Dimension laIndex[2,2]
laIndex[1,1] = 'CVENDCODE+NPAYDOCNO+CINVNO+DTOS(DAPDTRDAT)'
laIndex[1,2] = 'PAYMENT'

laIndex[2,1] = 'CVENDCODE+CINVNO+NPAYDOCNO+DTOS(dinvdate)'
laIndex[2,2] = 'INVOICE'
=gfCrtTmp(lcTempFile,@laTempStru,@laIndex)

Select (lnSlct)

*--End of function
*!**************************************************************************
*! Function      : lfColData
*! Purpose       : Collection of data
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/09/2009
*!**************************************************************************
Function lfColData


* Collect vendor option
lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgfxFlt')
lcVendCursor = loOgScroll.laOgfxFlt[lnVendPos,6]
lnSelectedVendorCounter = 0
If !Empty(lcVendCursor) And Used(lcVendCursor)
  Select &lcVendCursor.
  Locate
  Count To lnSelectedVendorCounter For !Deleted()
Endif

* Collect Invoice option
lnInvPos    = lfGetPos('APINVHDR.CINVNO','laOgfxFlt')
lcInvCursor = loOgScroll.laOgfxFlt[lnInvPos,6]
lnSelectedInvCounter = 0
If !Empty(lcInvCursor) And Used(lcInvCursor)
  Select &lcInvCursor.
  Locate
  Count To lnSelectedInvCounter For !Deleted()
Endif

* Collect Payment option
lnPayPos    = lfGetPos('APDIST.CAPDREF','laOgfxFlt')
lcPayCursor = loOgScroll.laOgfxFlt[lnPayPos,6]
lnSelectedPayCounter = 0
If !Empty(lcPayCursor) And Used(lcPayCursor)
  Select &lcPayCursor.
  Locate
  Count To lnSelectedPayCounter For !Deleted()
Endif
Set Step On


lnTrDatePos = lfGetPos('APINVHDR.DINVDATE','laOgfxFlt')
lcTrDate = loOgScroll.laOgfxFlt[lnTrDatePos,6]

lcPTrD = At('|',lcTrDate)+1
lcTrDateFrom = Substr(lcTrDate,1,lcPTrD-2)
lcTrDateTo = Substr(lcTrDate,lcPTrD)

If Empty(Alltrim(lcTrDateFrom )) And Empty(Alltrim(lcTrDateTo ))
  lcDateExp = ".T."
Endif
If !Empty(Alltrim(lcTrDateFrom )) And !Empty(Alltrim(lcTrDateTo ))
  If lcRpSort  = 'V'
    lcDateExp = "BETWEEN(DTOS(APINVHDR.dinvdate),'"+Dtos(Ctod(lcTrDateFrom))+"','"+Dtos(Ctod(lcTrDateTo))+"')"
  Else
    lcDateExp = "BETWEEN(DTOS(m.DAPDTRDAT),'"+Dtos(Ctod(lcTrDateFrom))+"','"+Dtos(Ctod(lcTrDateTo))+"')"
  Endif
Endif

If Empty(Alltrim(lcTrDateFrom )) And !Empty(Alltrim(lcTrDateTo ))
  If lcRpSort  = 'V'
    lcDateExp = "DTOS(APINVHDR.dinvdate) <= '"+Dtos(Ctod(lcTrDateTo))+"'"
  Else
    lcDateExp = "DTOS(m.DAPDTRDAT) <= '"+Dtos(Ctod(lcTrDateTo))+"'"
  Endif
Endif




If !Used('appaymnt')
  =gfOpenTABLE('appaymnt','TYPCLNO','SH')
  Select appaymnt
  =gfSEEK('')
Endif


=lfInsLine('')

*!*	Endcase

*!**************************************************************************
*! Function      : lfInsLine
*! Purpose       : Inserting lines in Temp File
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/03/2009
*!**************************************************************************
Function lfInsLine
Lparameters lcWhile
Set Step On
Select 0
lcWhile = Iif(Empty(lcWhile),'.t.',lcWhile)
Select cVendCode,CINVNO,DAPDTRDAT,CBNKCODE,CCHKACCT As NPAYDOCNO,Sum(napdamnt) As napdamnt  From APDIST Where capdactid ='A' And CAPDTRTYP ='A' ;
  AND napdamnt > 0 And !Empty(CAPDREF) And ( lnSelectedVendorCounter=0 Or Seek(APDIST.cVendCode, lcVendCursor))  And ;
  ( lnSelectedPayCounter=0 Or Seek(APDIST.CAPDREF, lcPayCursor)) And ( lnSelectedInvCounter=0 Or Seek(CINVNO, lcInvCursor)) And;
  gfSEEK(CAPDREF+cVendCode,'APINVHDR','INVVEND') And ;
  APINVHDR.ninvamnt > 0 And cApdStat <> 'V' And &lcWhile. ;
  GROUP By cVendCode,CINVNO,DAPDTRDAT,CBNKCODE,CCHKACCT ;
  INTO Cursor 'APPDEBITS'

If _Tally> 0

  Select 'APPDEBITS'
  Locate
  Scan
    Scatter Memvar Memo
    If gfSEEK(m.CINVNO + m.cVendCode,'APINVHDR','INVVEND')  && CINVNO+CVENDCODE
      Scatter Memvar Memo
      If &lcDateExp.
        m.DINVDUDAT=APINVHDR.DINVDUDAT
        m.NINVAMTAPP=APINVHDR.NINVAMTAP
        m.ninvamnt=APINVHDR.ninvamnt
        m.NINVAMTAP = m.napdamnt
        m.NINVPAID =  APINVHDR.NINVPAID
        Insert Into &lcTempFile From Memvar
      Endif
    Endif

  Endscan
Endif

Select appaymnt

Set Order To  TYPCLNO   && CPAYTYPE+CPAYCLNO
Locate
=gfSEEK('P')
Scan Rest While CPAYTYPE+CPAYCLNO = 'P'
  If ( lnSelectedPayCounter=0 Or Seek(appaymnt.cPAYDOCNO, lcPayCursor)) And ( lnSelectedVendorCounter=0 Or Seek(appaymnt.CPAYCLNO, lcVendCursor)) And   appaymnt.CPAYMETH $ 'PHMN' And  appaymnt.CPAYSTAT <> 'V' And &lcWhile.
    Select appaymnt
    Scatter Memvar Memo

    Select APDIST
    If gfSEEK(appaymnt.CPAYMETH+appaymnt.CBNKCODE+appaymnt.CCHKACCT+appaymnt.cPAYDOCNO ,'apdist','PAYMNTS') && CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID
      Scan Rest While CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+capdactid = appaymnt.CPAYMETH+appaymnt.CBNKCODE+appaymnt.CCHKACCT+appaymnt.cPAYDOCNO

        If gfSEEK(APDIST.CINVNO+APDIST.cVendCode,'APINVHDR','INVVEND') And  ( lnSelectedInvCounter=0 Or Seek(CINVNO, lcInvCursor))   && CINVNO+CVENDCODE
          Scatter Memvar Memo
          If &lcDateExp.
            m.DINVDUDAT  = APINVHDR.DINVDUDAT
            m.NINVAMTAPP = APINVHDR.NINVAMTAP
            m.ninvamnt   = APINVHDR.ninvamnt
            m.NINVAMTAP  = APDIST.napdamnt
            m.NINVPAID   = APINVHDR.NINVPAID
            m.cVendCode  = m.CPAYCLNO
            m.CINVNO     = APINVHDR.CINVNO
            m.DINVDATE   = m.Dpaydate
            m.NPAYDOCNO  = m.cPAYDOCNO
            *  m.NPAYAMNT   = m.NPAYAMNT
            m.CDESC      = Alltrim(lfGtPyMthdDesc(m.CPAYMETH ))
            Insert Into &lcTempFile From Memvar
          Endif

        Endif
      Endscan
    Endif
  Endif
Endscan

*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Getting the number of element from array
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 09/29/2009
*!**************************************************************************
Function lfGetPos
Parameters lcOpt,lcArray
Local lnPos
lnPos = Ascan(loOgScroll.&lcArray,lcOpt)
lnPos = Asubscript(loOgScroll.&lcArray,lnPos,1)
Return lnPos
*--End of function

*!*************************************************************
*! Name      : lfvApAcCod
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is to validate the accounts from
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!*************************************************************
Function lfvApAcCod
Local loFld,lnPos
loFld = loOgScroll.ActiveControl

lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = Strtran(lcApsAcMas,'#',Iif(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = Alltrim("X"+Substr(lcApsAcMas,2))
lnApsAcLen = Len(Alltrim(lcApsAcMas))

lcSavAlias  = Alias()  && Variable to save the selected alias.

lcFieldCont = _Screen.ActiveForm.ActiveControl.Value    && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = Replicate('0',lnApsAcLen)

lnPos = Ascan(loOgScroll.laOGvrFlt,"APDIST.CAPDGLACT")
If lnPos > 0
  lnPos    = Asubscript(loOgScroll.laOGvrFlt,lnPos,1)
Endif

*- Prevent executing the browse if the account code is empty.
If !Empty(Strtran(lcFieldCont,"-",""))

  If llApGlLink .And. lcFieldCont <> lcEmptyAcs

    If !Used('GLACCHAR')
      =gfOpenTABLE('GLACCHAR','ACCTCODE','SH')
      Select GLACCHAR
      =gfsetorder('ACCTCODE')
      =gfSEEK('')
    Endif

    *- be sure you browse from the correct table
    Select GLACCHAR
    =gfsetorder('ACCTCODE')

    If !Seek(lcFieldCont) .Or. Atc('?',lcFieldCont) > 0
      Dimension laTemp[2]
      laTemp = ''

      * N000682 ,1 Thabet Handle globalization issues [Start]
      *lcBrfields="CACCTCODE :H= 'Account Code',;
      CACCNLDES :H= 'Account Description'"
      *lcFile_Ttl="Chart of accounts"
      lcBrfields="CACCTCODE :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNT_CODE,oAriaApplication.GetHeaderText("LANG_ACCOUNT_CODE",AHEADERFILE))+"',"+;
        "CACCNLDES :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNT_DESCRIPTION,oAriaApplication.GetHeaderText("LANG_ACCOUNT_DESCRIPTION",AHEADERFILE))+"'"
      lcFile_Ttl=Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CHART_OF_ACCOUNTS,oAriaApplication.GetHeaderText("LANG_CHART_OF_ACCOUNTS",AHEADERFILE))
      * N000682 ,1 Thabet Handle globalization issues [END]
      =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

      If !Empty(laTemp[1])
        lcFieldCont = Alltrim(laTemp[1])
      Else
        lcFieldCont = Replicate('0',lnApsAcLen)
      Endif
    Endif


    If !Empty(laTemp[1])
      loFld.Value = laTemp[1]
      loOgScroll.laOGvrFlt[lnPOS,6] = laTemp[1]
    Else
      loFld.Value = loFld.OldValue
      *- restore the old value the the laOgvrflt array itself
      *!*	      lnGlAcctPos = lfGetPos('APDIST.CAPDGLACT','laOgVrFlt')
      *!*	      loOGScroll.laOGvrFlt[lnGlAcctPos,6] = loFld.OldValue
    Endif

    If !Empty(lcSavAlias)
      Select(lcSavAlias)
    Endif

  Endif
Else
  loOgScroll.laOGvrFlt[lnPOS,6] = ""
Endif

If !Empty(lcSavAlias)
  Select(lcSavAlias)
Endif
****************************************************************
*! Name      : lfvSess
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/11/2011
*! Purpose   : Valid function to pad session #
****************************************************************
Function lfvSess
Local loFld
loFld = _Screen.ActiveForm.ActiveControl
If !Empty(loFld.Value)
  loFld.Value = Padl(Alltrim(loFld.Value),8,'0')
Endif
*- end of FUNCTION lfvSess


************************************************************
*! Name      : lfvSort
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is used to order the report based on
*!             Gl account or transaction type.
************************************************************
Function lfvSort
If lcRpSort  = 'P'
  lcRpForm = 'APVNFIN'
Else
  lcRpForm = 'APVNFINI'
Endif
*!*	Select APDIST

*!*	Do Case
*!*	Case lcRpSort = 'P'
*!*	  lcRpForm = 'apvenfin'
*!*	   Set Order To 'PAYMENT' && CVENDCODE+NPAYDOCNO+CINVNO
*!*	  llRpPrven = .T.
*!*	Case lcRpSort = 'V'
*!*	  lcRpForm = 'apvenfini'
*!*	  Set Order To 'INVOICE' && CVENDCODE+CINVNO+NPAYDOCNO
*!*	  llRpPrven = .T.
*!*	Endcase
*!*	ClearRead()

*!*	=lfToglShowTrnNo()

*!*	lcRpFormat = Iif(lcRpSort<>'G','A',lcRpFormat)
*!*	If Ascan(laOGObjType,Upper('lcRpFormat')) # 0
*!*	  lnPos = Asubscript(laOGObjType,Ascan(laOGObjType,Upper('lcRpFormat')),1)
*!*	  laOGObjCnt[lnPos] = (Padr(lcRpSort,1)='G')
*!*	  =lfOGShowGet(Upper('lcRpFormat'))
*!*	Endif
Endfunc
*!*************************************************************
*! Name      : lfwApJour
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function is used as a when function for this report
*!*************************************************************
Function lfwApJour

=lfToglShowTrnNo()

If !Used('APVENDOR')
  =gfOpenTABLE('APVENDOR','VENCODE','SH')
  Select APVENDOR
  =gfsetorder('VENCODE')
  =gfSEEK('')
Endif

If !Used('ACCOD')
  =gfOpenTABLE("ACCOD",'ACCSEGNO','SH')
  Select ACCOD
  =gfsetorder('ACCSEGNO')
  =gfSEEK('')
Endif

If !Used('APSETUP')
  =gfOpenTABLE('APSETUP','APSETUP','SH')
  Select APSETUP
  =gfSEEK('')
Endif

If !Used('appaymnt')
  =gfOpenTABLE('appaymnt','appaymnt','SH')
  Select appaymnt
  =gfSEEK('')
Endif

If !Used('APdist')
  =gfOpenTABLE('APdist','APdist','SH')
  Select APDIST
  =gfSEEK('')
Endif

If !Used('APINVHDR')
  =gfOpenTABLE('APINVHDR','APINVHDR','SH')
  Select APINVHDR
  =gfSEEK('')
Endif

If APSETUP.cApsgllink = 'Y'
  llApGlLink = .T.
Else
  llApGlLink = .F.
Endif

lnTrnTyPos = Ascan(laOGvrFlt,'APDIST.CAPDTRTYP')
If lnTrnTyPos > 0
  lnTrnTyPos = Asubscript(laOGvrFlt,lnTrnTyPos,1)
Endif

*!*************************************************************
*! Name      : lfvTrnNo
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : browse transaction no. from APDIST file
*!*************************************************************
Function lfvTrnNo

Private lnTrnType
Local loFld
loFld = loOgScroll.ActiveControl

Private  lcPriorDbf , lcPriorCdx  , lcPriorFlt
lcPriorDbf  = Select(0)
lcPriorCdx  = Order()
lcPriorFlt  = Filter()

Private lcFiltExp
lcFiltExp = ""

Select APDIST
Set Order To INVVEND
Locate

* Assign no space to lcInvNo
lcInvNo = ''

If !Empty(lcTrnNo)
  If !Seek(lcTrnNo) .Or. Atc("?",lcTrnNo) > 0
    ** MESSAGE : " This record is not found in the  "
    **           " data file.                       "
    **           "      < Browse >   < Reenter>     "
    lnClosRec  = Recno(0)
    Dimension laTemp[4]
    laTemp = ''

    * N000682 ,1 Thabet Handle globalization issues [Start]
    *lcBrFields = "CVENDCODE :H= 'Vendor code'    ,;
    CINVNO    :H= 'Invoice number' ,;
    DAPDTRDAT :H= 'Invoice date'   ,;
    CAPDREF   :H= 'Reference'"
    * lcFile_Ttl = "Invoice"
    lcBrfields = "CVENDCODE :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VENDOR_CODE,oAriaApplication.GetHeaderText("LANG_VENDOR_CODE",AHEADERFILE))+"',"+;
      "CINVNO    :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICE_NUMBER,oAriaApplication.GetHeaderText("LANG_INVOICE_NUMBER",AHEADERFILE))+"',"+;
      "DAPDTRDAT :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICE_DATE,oAriaApplication.GetHeaderText("LANG_INVOICE_DATE",AHEADERFILE))+"',"+;
      "CAPDREF   :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REFERENCE,oAriaApplication.GetHeaderText("LANG_REFERENCE",AHEADERFILE))+"' "
    lcFile_Ttl = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICE,oAriaApplication.GetHeaderText("LANG_INVOICE",AHEADERFILE))
    * N000682 ,1 Thabet Handle globalization issues [END]
    If Between(lnClosRec,1,Reccount('APDIST'))
      Go lnClosRec
    Else
      Go Top
    Endif

    * APDIST.cApdTrTyp
    * 'A' --- DM Application
    * 'B' --- Bank Adj.
    * 'H' --- Cash Payment
    * 'I' --- Invoice
    * 'M' --- Manual Payment
    * 'N' --- Non Manual Payment
    * 'P' --- Printed Checks
    Dimension  laTrnType[8]

    *C101831,1 SSE 03/27/2000 Adjust cases of Transaction Types [Begin]
    Do Case
    Case Empty(laOGvrFlt[lnTrnTyPos,6])
      lnTrnType = 1
    Case laOGvrFlt[lnTrnTyPos,6] = "A"
      lnTrnType = 2
    Case laOGvrFlt[lnTrnTyPos,6] = "B"
      lnTrnType = 3
    Case laOGvrFlt[lnTrnTyPos,6] = "H"
      lnTrnType = 4
    Case laOGvrFlt[lnTrnTyPos,6] = "I"
      lnTrnType = 5
    Case laOGvrFlt[lnTrnTyPos,6] = "M"
      lnTrnType = 6
    Case laOGvrFlt[lnTrnTyPos,6] = "N"
      lnTrnType = 7
    Case laOGvrFlt[lnTrnTyPos,6] = "P"
      lnTrnType = 8
    Endcase
    *C101831,1 SSE 03/27/2000 [End]

    laTrnType[1]  = [ .T. ]
    laTrnType[2]  = [ cApdTrTyp = 'A' AND cApdActId = 'A' AND nApdAmnt >  0  AND cApdStat <> 'V' ]
    laTrnType[3]  = [ cApdTrTyp = 'B' AND cApdActId = 'D' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[4]  = [ cApdTrTyp = 'H' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[5]  = [ cApdTrTyp = 'I' AND cApdActId = 'A' AND nApdAmnt <  0  AND cApdStat <> 'V' ]
    laTrnType[6]  = [ cApdTrTyp = 'M' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[7]  = [ cApdTrTyp = 'N' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[8]  = [ cApdTrTyp = 'P' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]

    lcFiltExp = laTrnType(lnTrnType)

    *- Add vendor code to filter if not bank adjustment.
    If !Empty(laOGvrFlt[1,6]) And lnTrnType <> 3
      lcFiltExp = lcFiltExp  +  " AND  APDIST.CVENDCODE = '"+laOGvrFlt[1,6]+"'  "
    Endif

    =gfbrows( 'FOR' + lcFiltExp  ,'CVENDCODE,CINVNO,DAPDTRDAT,CAPDREF','laTemp')

    If !Empty(laTemp[1])
      * Assign selected vendor no. to lcVenNo variable
      lcVenNo   = laTemp[1]
      * Assign selected Reference no. to lcTrnNo variable
      lcTrnNo   = laTemp[4]
      * Assign invoice no to lcInvNo variable
      lcInvNo   = laTemp[2]

      loFld.Value = laTemp[4]
    Else
      * Assign no space to lcTrnNo and lcInvNo
      lcTrnNo = ''
      lcInvNo = ''
      loFld.Value = loFld.OldValue
    Endif

  Endif
Endif

Select (lcPriorDbf)
Set Order To &lcPriorCdx
Set Filter To  &lcPriorFlt

*!**************************************************************************
*!
*!      FUNCTION: lfRepshow
*!
*!**************************************************************************
Function lfRepshow


*!***************************************************************************
*! Name      : lfvFormat
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : Valid for changing Report Format
*!***************************************************************************
Function lfvFormat
lcRpForm = Iif(lcRpFormat='A','APJORN','APJORNB')
*-- End of lfvFormat.

*!***************************************************************************
*       FUNCTION lfVTranTyp
*!***************************************************************************
Function lfVTranTyp
=lfToglShowTrnNo()

*!**************************************************************************
*!
*!  FUNCTION lfInputMask
*!
*!**************************************************************************
Function lfInputMask

If !Used('APSETUP')
  =gfOpenTABLE('APSETUP','APSETUP','SH')
  Select APSETUP
  =gfsetorder('APSETUP')
  =gfSEEK('')
Endif
If !Used('ACCOD')
  =gfOpenTABLE("ACCOD",'ACCSEGNO','SH')
  Select ACCOD
  =gfsetorder('ACCSEGNO')
  =gfSEEK('')
Endif
Local lcApsAcMas
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = Strtran(lcApsAcMas,'#',Iif(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = Alltrim("X"+Substr(lcApsAcMas,2))
lnApsAcLen = Len(Alltrim(lcApsAcMas))

Return lcApsAcMas
*end of lfInputMask


********************************************************************************************
* Check if a value is selected
********************************************************************************************
Function lfIsSlcted
Parameters lcCursor
Local llIsSelected,lnSlct
lnSlct = Select(0)
llIsSelected = .F.
If !Empty(lcCursor) And Used(lcCursor)
  Select &lcCursor
  Locate
  llIsSelected = !Eof()
Endif
Select (lnSlct)
Return llIsSelected

******************************************************************************************
*! Name      : lfToglShowTrnNo
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/10/2011
*! Purpose   : Toggle "Tran. No" OG variable Enabled/Disabled
******************************************************************************************
Function lfToglShowTrnNo
Local lnTrTypePos,lcTrType
*!*	lnTrTypePos = lfGetPos('APDIST.CAPDTRTYP','laOgVrFlt')
*!*	lcTrType = loOGScroll.laOGvrFlt[lnTrTypePos,6]
If Ascan(laOGObjType,'LCTRNNO') # 0
  lnPos = Asubscript(laOGObjType,Ascan(laOGObjType,'LCTRNNO'),1)
  laOGObjCnt[lnPos] = Padr(lcRpSort,1) <> 'G' And !Empty(lcTrType)
  lcTrnNo = Iif(!Empty(lcTrType),lcTrnNo,'')
  =lfOGShowGet('LCTRNNO')
Endif


******************************************************************************************
*! Name      : lfToglShowTrnNo
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/10/2011
*! Purpose   : Check if tag exists
******************************************************************************************
Function lfTagFound
Parameters lcTag,lcAlias
Local lnSlct,lnTag,llExists
lnSlct = Select(0)
llExists = .F.
lcTag = Alltrim(Upper(lcTag))
Select (lcAlias)
lnI = 1
Do While !Empty(Tag(lnI))
  If lcTag == Tag(lnI)
    llExists = .T.
    Exit
  Endif
  lnI = lnI + 1
Enddo

Select (lnSlct)
Return llExists

************************************************************************************************
* Name        : lfPolishExp
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
Function lfPolishExp
Parameters lcExp,lcRmv
Local lnPos,lcRight
lcRight = ")"
lnPos = At(lcRmv,lcExp)
Do While lnPos>0
  lnAndPos = Rat(' AND ',Substr(lcExp,1,lnPos))
  lcLeftStr = Left(lcExp,lnAndPos-1)
  lnPranth = At(lcRight,Substr(lcExp,lnAndPos))
  lcRightStr = Substr(lcExp,lnAndPos+lnPranth+Len(lcRight)-1)
  lcExp = lcLeftStr+lcRightStr
  lnPos = At(lcRmv,lcExp)
Enddo

************************************************************
*! Name      : lfGetOpenAmt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/17/2014
*! Purpose   : add a line for the open amount balance for the selected vendor
*! B610677,1
************************************************************
*!*	Function lfGetOpenAmt
*!*	Local lnSlct,laVend,lnI,lcRpExp,lcBefore,lnPos
*!*	lnSlct = Select(0)

*!*	lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgVrFlt')
*!*	*!*	lnGlAcctPos = lfGetPos('APDIST.CAPDGLACT','laOgVrFlt')
*!*	*!*	lnTrTypePos = lfGetPos('APDIST.CAPDTRTYP','laOgVrFlt')
*!*	*!*	lnSessNoPos = lfGetPos('APDIST.CAPSESSNO','laOgVrFlt')
*!*	*!*	lnTrDatePos = lfGetPos('APDIST.DAPDTRDAT','laOgFxFlt')
*!*	lcVend = loOgScroll.laOGvrFlt[lnVendPos,6]
*!*	*!*	lcGlAcct = loOGScroll.laOGvrFlt[lnGlAcctPos,6]
*!*	*!*	lcTrType = loOGScroll.laOGvrFlt[lnTrTypePos,6]
*!*	*!*	lcTrDate = loOGScroll.laOGfxFlt[lnTrDatePos,6]
*!*	*!*	lcSessNo = loOGScroll.laOGvrFlt[lnSessNoPos,6]

*!*	*!*	lcPTrD = AT('|',lcTrDate)+1
*!*	*!*	lcTrDateFrom = SUBSTR(lcTrDate,1,lcPTrD-2)
*!*	*!*	lcTrDateTo = SUBSTR(lcTrDate,lcPTrD)

*!*	*!*	lcP = AT('|',lcSessNo)+1
*!*	*!*	lcSessFrom = SUBSTR(lcSessNo,1,lcP-2)
*!*	*!*	lcSessTo = SUBSTR(lcSessNo,lcP)

*!*	lnVen = 0
*!*	If !Empty(lcVend) And Used(lcVend)
*!*	  Select &lcVend
*!*	  Locate
*!*	  Count To lnVen For !Deleted()
*!*	Endif

*!*	Store {} To ldFrmDate , ldEnddate
*!*	If !Empty(laOgfxFlt[lnTrDatePos,6])
*!*	  lnPipPos = Atc('|',laOgfxFlt[1,6]) - 1
*!*	  ldFrmDate = Ctod(Substr(laOgfxFlt[1,6],1,lnPipPos))
*!*	  ldEnddate = Ctod(Substr(laOgfxFlt[1,6],lnPipPos+ 2,Len(laOgfxFlt[lnTrDatePos,6])))
*!*	Endif

*!*	lcRpExp = lcOgFltr
*!*	lcRpExp = lcRpExp + Iif(Empt(lcRpExp) , '' , ' AND ' )  + " APDIST.nApdAmnt <> 0  "

*!*	lcIncI = "MHNP" + Iif(llRpIncVdInv,"I","")

*!*	*!* E303658,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [Start ]
*!*	*!*	lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + ;
*!*	*!*	                    "(APDIST.capdstat <> 'V' .OR. "+;
*!*	*!*	                    "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ '&lcIncI' ))"
*!*	Set Step On
*!*	lcRpExp = lcRpExp + Iif(Empt(lcRpExp) , '' , ' AND ' )  + ;
*!*	  "( "+Iif(llRpShVdInv .And. llRpIncVdInv,"","APDIST.capdstat <> 'V' .OR.") +;
*!*	  "(APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ '&lcIncI' ))"

*!*	*!* E303658,1 AEG 04/07/2016 modify ap jornal to show voided invoices only [END ]
*!*	lcRpExp = lcRpExp + Iif(!llRpPrven And lcRpSort = 'V',Iif(Empt(lcRpExp) , '' , ' AND ' )  + " !EMPTY(APDIST.cvendcode) " ,'')
*!*	lcRpExp = lcRpExp + Iif(lcRpRel = 'B','',;
*!*	  IIF(Empty(lcRpExp),'',' AND ')+Iif(lcRpRel='U','!','')+"APDIST.lApdPost")
*!*	lcRpExp = Strtran(lcRpExp,'.AND.',' AND ')
*!*	lcRpExp = Strtran(lcRpExp,'  ',' ')
*!*	If !Empty(lcTrnNo)
*!*	  lcRpExp = lcRpExp + Iif(Empt(lcRpExp) , '' , ' AND ' ) + ;
*!*	    " APDIST.cApdRef= '"+lcTrnNo+"'"
*!*	Endif

*!*	*- Remove the BETWEEN ( start date, end date )
*!*	lcBefore = Substr(lcRpExp,1,At("BETWEEN(",lcRpExp)-1)
*!*	lcRpExp = Substr(lcRpExp,At("BETWEEN(",lcRpExp))
*!*	lnPos = At(")",lcRpExp,2)
*!*	lcRpExp = Stuff(lcRpExp ,1,lnPos," .T. ")
*!*	lcRpExp = lcBefore + lcRpExp

*!*	*After collecting data loop through the selected vendor and sum from the apdist the neqvamt for date prior to the FROM date
*!*	Select APDIST
*!*	If !File(gcWorkDir +lcJourTmp+ '.CDX') Or !lfTagFound('cJourTagV','APDIST')
*!*	  Index On cVendCode+CAPDTRTYP Tag cJourTagV Of (gcWorkDir +  lcJourTmp + '.CDX')
*!*	Else
*!*	  Set Order To Tag cJourTagV Of (gcWorkDir +  lcJourTmp + '.CDX')
*!*	Endif

*!*	*- Add a new record to the temp cursor with the summed open amt such that it is located on top of the collected lines for the vendor
*!*	*- Make the description of this line ‘Opening amount’

*!*	Dimension laVend[1]
*!*	*B610677,4 TMI 03/13/2014 13:33 [Start] don't get the array of vendors from the lcTempFile, it does not cover the case that there is an opening balance while
*!*	*                                       there is no transactions for the selected criteria, get it instead from APVENDOR or LCVEND
*!*	*SELECT DISTINCT CVENDCODE FROM DBF(lcTempFile) INTO ARRAY laVend
*!*	lcVendSrc = Iif(lnVen = 0 , 'APVENDOR' , lcVend )
*!*	Select Distinct cVendCode From &lcVendSrc Into Array laVend
*!*	*B610677,4 TMI 03/13/2014 13:33 [End  ]
*!*	Select &lcTempFile
*!*	Scatter Memvar Blank
*!*	m.CAPDGLACT = 'Opening Balance'
*!*	For lnI = 1 To Alen(laVend,1)
*!*	  m.NEQVAMNT = 0
*!*	  m.cVendCode = laVend[lnI]
*!*	  Select APDIST
*!*	  =Seek(laVend[lnI])
*!*	  llNoStartDate = Empty(Chrtran(lcTrDateFrom,'\',''))
*!*	  Sum NEQVAMNT To m.NEQVAMNT Rest While cVendCode+CAPDTRTYP = laVend[lnI] For Iif(llNoStartDate,.T.,DAPDTRDAT < Ctod(lcTrDateFrom) ) And Evaluate(lcRpExp)
*!*	  If m.NEQVAMNT <> 0
*!*	    Insert Into &lcTempFile From Memvar
*!*	  Endif
*!*	Endfor

*!*	Select (lnSlct)
*!*	*- End of lfGetOpenAmt.

*!**************************************************************************
*! Function      : lfCrTemp
*! Purpose       : Creating Temp file
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 09/28/2009
*!**************************************************************************
Function lfGtPyMthdDesc

Lparameters lcvenpmeth
lcvenpmethDesc = lcvenpmeth
For I = 1 To Alen(laPayMethod,1)
  If laPayMethod(I,2) == lcvenpmeth
    lcvenpmethDesc = laPayMethod(I,1)
    Exit
  Endif
Endfor
Return lcvenpmethDesc
Endfunc
**********************************************************
