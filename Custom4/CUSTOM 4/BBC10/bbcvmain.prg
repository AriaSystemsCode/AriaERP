*!**************************************************************************
*! Name      : BBCMAIN.PRG
*! Developer : Hesham Elmasry
*! Date      : 06/16/2009
*! Purpose   : Custom triggers for BBC customer
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications:
*!* C201971,1 MHM 03/23/2017 Consider BBC UDF(s)
*!**************************************************************************
PARAMETER lcevntfun, lcfunpars
lcfunpars = IIF(TYPE('lcFunPars') = 'C', lcfunpars, '')
lcfuntorun = 'lf' + ALLTRIM(lcevntfun) + '(' + lcfunpars + ')'
llretvalue = EVALUATE(lcfuntorun)
RETURN llretvalue

*!**************************************************************************
*! Name       : lfFilFlds
*! Developer  : Hesham Elmasry (HES)
*! Date       : 06/15/2009
*! Purpose    : Fill the Two fields dOrgCmDate and dModCmDate with the POSHDR.Complete
*! Type       : Custom
*!**************************************************************************
FUNCTION lfFilFlds

SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
REPLACE dOrgCmDate WITH COMPLETE ;
  dModCmDate WITH COMPLETE

* End of lfFilFlds

*!**************************************************************************
*! Name       : lfSvCurCmD
*! Developer  : Hesham Elmasry (HES)
*! Date       : 06/15/2009
*! Purpose    : Save the current COMPLETE date for the PO # in a formset property
*! Type       : Custom
*!**************************************************************************
FUNCTION lfSvCurCmD

IF TYPE('loFormSet.ldCurCmDat') = 'U'
  loFormSet.ADDPROPERTY('ldCurCmDat',{})
ENDIF

SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
loFormSet.ldCurCmDat = COMPLETE

* End of lfSvCurCmD

*!**************************************************************************
*! Name       : lfVldCmDat
*! Developer  : Hesham Elmasry (HES)
*! Date       : 06/15/2009
*! Purpose    : Check if the Complete date modified or not to update the dModCmDate field
*! Type       : Custom
*!**************************************************************************
FUNCTION lfVldCmDat

LOCAL ldOldCmDate, ldNewCmDate

ldOldCmDate = loFormSet.ldCurCmDat
ldNewCmDate = loFormSet.ariaForm1.PgfPOStyle.page1.cntheaderFolder.DtPickerComplete.Text1.VALUE

IF ldOldCmDate <> ldNewCmDate
  SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
  REPLACE dModCmDate WITH ldOldCmDate
ENDIF
* End of lfVldCmDat
*!* C201971,1 MHM 03/23/2017 Consider BBC UDF(s)[Begin]
*!**************************************************************************
*! Name       : lfModify
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    : Modify structure by add three columns.
*! Type       : Custom
*!**************************************************************************
FUNCTION lfMdfTktL

lcALIASC = ALIAS()
TRY

  ALTER TABLE (lcTmpTktL)  ADD COLUMN NCasePack N(9,0) ADD COLUMN cStylePO C(30) ADD COLUMN cPackby C(30)

CATCH
ENDTRY
SELECT (lcALIASC)
*!**************************************************************************
*! Name       : lfGtpkLnInf
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    :
*! Type       : Custom
*!**************************************************************************
FUNCTION lfGtpkLnInf

lcALIASC = ALIAS()

SELECT  (lcTmpTktL)
REPLACE cStylePO WITH Pack_Lin.cStylePO ,;
  cPackby WITH Pack_Lin.cPackby,;
  NCasePack WITH Pack_Lin.NCasePack IN (lcTmpTktL)

SELECT (lcALIASC)
*!**************************************************************************
*! Name       : lfGtpkLnInO
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    :
*! Type       : Custom
*!**************************************************************************
FUNCTION lfGtpkLnInO

lcalias = ALIAS()

SELECT Pack_Lin
SET ORDER TO PACKSTYLE   && PACK_NO+STR(NO_CART,4)+STYLE+DYELOT
=SEEK(MPACKNO)
SCAN REST WHILE PACK_NO+STR(No_Cart,4)+STYLE+DYELOT = MPACKNO

  IF Pack_Lin.nOrdLineNo = ordline.LINENO


    REPLACE cStylePO WITH Pack_Lin.cStylePO ,;
      cPackby WITH Pack_Lin.cPackby ,;
      NCasePack WITH Pack_Lin.NCasePack IN (lcTmpTktL)

    EXIT


  ENDIF
ENDSCAN


SELECT (lcalias)

*!**************************************************************************
*! Name       : lfUpdTmp
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    :
*! Type       : Custom
*!**************************************************************************
FUNCTION lfUpdTmpPk

SELECT Pack_Lin
SET ORDER TO PACKSTYLE   && PACK_NO+STR(NO_CART,4)+STYLE+DYELOT
=SEEK(MPACKNO)
SCAN REST WHILE PACK_NO+STR(No_Cart,4)+STYLE+DYELOT = MPACKNO
  IF (Pack_Lin.Packtype = 'P' )
    FOR lnCount = 1 TO SCALE.CNT
      lcCount = STR(lnCount,1)
      IF QTY&lcCount > 0
        XTOTQTY = QTY&lcCount
        XWEIGHT = QTY&lcCount*STYLE.nStyWeight
        XSKU    = XSKU&lcCount
        xUpc    = ALLTRIM(xUpc&lcCount)
        MSIZE   = SCALE.SZ&lcCount
        XPRICE  = Price
        *E302242,1 WLD Increase the UPC to be 14 digits - GTIN compliance [Begin]
        XKUPC = IIF(!EMPTY(ALLTRIM(xUpc)),PADL(ALLTRIM(xUpc),14,'0'),' ')
        *E302242,1 WLD Increase the UPC to be 14 digits - GTIN compliance [End  ]
        *E302549,1 WLD Adding Cation loop 08/13/2008 [Begin]
        MUOM ='EA'
        MCTT = MCTT+1
        MTotQty   = MTotQty   + XTOTQTY
        MTotWght  = MTotWght  + XWEIGHT
        lnTOtAmnt = lnTOtAmnt + XTOTQTY*XPRICE
      ENDIF
    ENDFOR
    XSKU = Sku
    =SEEK('P'+MSCALE+&lcTmpTktL..PrePak,'Scale','Scale')
    XPPTOT = SCALE.PPTOT
    =SEEK('S'+MSCALE,'Scale')
    MSIZE = XPPTOT
    =THIS.AddHeaderDataDetail()
  ENDIF
ENDSCAN



*****************************************


*!**************************************************************************
*! Name       : lfUPDTPRP
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    :
*! Type       : Custom
*!**************************************************************************
FUNCTION lfUPDTPRP

lcALIASC = ALIAS()

SELECT  (lcTmpTktL)
MTOTCARTON = &lcTmpTktL..NCasePack
*LOCATE
IF UPPER(ALLTRIM(&lcTmpTktL..cPackby)) == 'P'
  REPLACE PrePak WITH cPackby
ENDIF

* T20170509.0013 - Issue #6 - Generated files incorrect (BBC Apparel Group, LLC)
IF UPPER(ALLTRIM(&lcTmpTktL..cPackby)) == 'S'
  REPLACE PrePak WITH ''
ENDIF
*Locate


SELECT (lcALIASC)
*!**************************************************************************



*!**************************************************************************
*! Name       : lfFILLVARS
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    :
*! Type       : Custom
*!**************************************************************************
FUNCTION lfFILLVARS

MTOTCARTON = &lcTmpTktL..NCasePack
mStylePO   = &lcTmpTktL..cStylePO
***************************************************************************
*!* C201971,1 MHM 03/23/2017 Consider BBC UDF(s)[End]

*!**************************************************************************
*! Name       : lfJORDIVS
*! Developer  : Mohammed Shaker (MSH)
*! Date       : 03/28/2017
*! Purpose    : Get Customer Account Sent In N1*ST Segment
*! Type       : Custom
*!**************************************************************************
FUNCTION lfJORDIVS
SET STEP ON
** Apply Trigger If Partner Is JOOR
IF ALLT(EdiAcPrt.CpartCode) = 'JOOR'

  IF LEFT(EDILIBDT.mTranText,6)='START|'
    lcTran  = EDILIBDT.mTranText
    lnStart = VAL(SUBSTR(lcTran,7,AT('|LENGTH|',lcTran)-7))
    lnlen   = VAL(SUBSTR(lcTran,AT('|LENGTH|',lcTran)+8))
    lnFile  = FOPEN(oAriaApplication.EDIPath +ALLTRIM(EdiLibHd.cFilePath)+ALLTRIM(EdiLibHd.cEdiFilNam))
    *IF lnFile = -1
    *   =oAriaApplication.MessageBox('TRM48006B00000','DIALOG')
    *  RETURN
    *ENDIF
    STORE '' TO lcLine , lcFieldSep , lcCustAcc
    STORE 'ISA' TO lcSegLine && Supposing X12 -  JOOR
    lcSegLine = FGETS(lnFile)
    lcFieldSep = SUBSTR(lcSegLine , 4 , 1)
    lcLineSep  = IIF(LEN(SUBSTR(lcSegLine , AT(lcFieldSep ,;
      lcSegLine , 16) + 1)) = 1 , '',RIGHT(lcSegLine , 1))
    ** Read File Transaction From Known Position Calculated From EdiLibDt.MTranText
    =FSEEK(lnFile,lnStart)
    DO WHILE !FEOF(lnFile)
      lcSegLine = FGETS(lnFile)
      IF SUBSTR(lcSegLine , 1 , AT(lcFieldSep,lcSegLine,1)-1) = 'SE'
        *IF SUBSTR(lcSegline , 1 , AT(lcFieldSep,lcSegline,1)-1) = 'GE'
        EXIT
      ENDIF
      IF SUBSTR(lcSegLine , 1 , AT(lcFieldSep,lcSegLine,1)-1) = 'N1' AND ;
          SUBSTR(lcSegLine , AT(lcFieldSep,lcSegLine,1)+1 , AT(lcFieldSep,lcSegLine,2)-AT(lcFieldSep,lcSegLine,1)-1) = 'ST'
        lnStartPos= AT(lcFieldSep,lcSegLine,4)+1
        IF OCCURS(lcFieldSep,lcSegLine) > 4
          lnNumOfChr= AT(lcFieldSep,lcSegLine,5)- AT(lcFieldSep,lcSegLine,4)- 1
          lcCustAcc = SUBSTR(lcSegLine,lnStartPos,lnNumOfChr)
        ELSE
          IF !EMPTY(lcLineSep)
            lnNumOfChr= AT(lcLineSep,lcSegLine,1)- AT(lcFieldSep,lcSegLine,4)- 1
            lcCustAcc = SUBSTR(lcSegLine,lnStartPos,lnNumOfChr)
          ELSE
            lcCustAcc = SUBSTR(lcSegLine,lnStartPos)
          ENDIF
        ENDIF

        *C202077,1 FODA 10/09/2017 display message if client not found [BEGIN]
        *IF SEEK('M' + lcCustAcc , 'Customer' ,'Customer')
        SET STEP ON 
        IF SEEK('M' + PADR(lcCustAcc,5) , 'Customer' ,'Customer')
		*C202077,1 FODA 10/09/2017 display message if client not found [END]
          MAccount = lcCustAcc
          EXIT
        *C202077,1 FODA 10/09/2017 display message if client not found [BEGIN]
    	ELSE 
          MAccount = ''
		  MESSAGEBOX("customer code " +lcCustAcc + " does not exist !",0+16,"Wrong Customer")
          EXIT
        *C202077,1 FODA 10/09/2017 display message if client not found [END]
        ENDIF
      ENDIF
    ENDDO
    =FCLOSE(lnFile)
  ENDIF

ENDIF
RETURN .T. && !EMPTY(MACCOUNT)
ENDFUNC

* B611322,1 MHM Genrated files Incorrect Issue #6
*!**************************************************************************
*! Name       : lfCHGPID
*! Developer  : Mohamed Hamdy (MHM)
*! Date       : 03/13/2017
*! Purpose    :
*! Type       : Custom
*!**************************************************************************
FUNCTION lfCHGPID

IF UPPER(ALLTRIM(Pack_Lin.cPackby)) == 'P' AND !EMPTY(ALLTRIM(Pack_Lin.pack_id))
  m.pack_id = SPACE(16)
ENDIF
***************************************************************************
* B611322,1 MHM Genrated files Incorrect Issue #6