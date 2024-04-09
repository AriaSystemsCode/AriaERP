*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah
*: Date      : 04/18/2006
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C200797,1 HIA Requested modification on your customer label from Sears/K-Mart in order to display the first GTIN# if there's more than one.
*C200911,1 WLD 01/21/2008 Add seprate line for each Packing Info in receiving 850 map NYL
*C201035,1 WLD 07/30/2008 Grouping PO1 depending on Packing Info in receiving 850 map NYL
*C201197,1 AHS 10/26/2009 Customize BLK label for Kazu [T20091022.0011]
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfGetUPC
*! Developer : Hassan Ali (HIA)
*! Date      : 06/14/2007
*! Purpose   : Requested modification on your customer label from Sears/K-Mart in order to display the first GTIN# if there's more than one.
*:****************************************************************************************
*! Called from : Class "prnlabel"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfGetUPC
  IF SYCASNHD.CVER = 'KS1'
    IF EMPTY(ALLTRIM(M.GTIN)) AND ALLTRIM(UPPER(M.CSIZEDESC)) = 'MIXED'
      m.GTIN = lfSkuDesc(m.Order,PACK_LIN.NORDLINENO,'UK','UK')
    ENDIF
  ENDIF
ENDFUNC
*****************************************************************************************
FUNCTION lfGetSKU
  IF SYCASNHD.CVER = 'KS1'
    IF EMPTY(ALLTRIM(M.csku)) AND ALLTRIM(UPPER(M.CSIZEDESC)) = 'MIXED'
      m.CSKU = lfSkuDesc(m.Order,PACK_LIN.NORDLINENO,'IN','IN')
    ENDIF
  ENDIF
ENDFUNC
*****************************************************************************************
FUNCTION lfGETSTD
  IF SYCASNHD.CVER = 'KS1'
    IF !EMPTY(ALLTRIM(M.STYDESC)) AND ALLTRIM(UPPER(M.CSIZEDESC)) = 'MIXED'
      m.CSIZEDESC = M.STYDESC
    ENDIF
  ENDIF
ENDFUNC

*:****************************************************************************************
*! Name      : lfSetSpt
*! Developer : Waleed Hamed (WLD)
*! Date      : 01/21/2008
*! Purpose   : Add seprate line for each Packing Info
*:****************************************************************************************
*! Called from : Class "EDIPROCESSPO"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfSetSpt
  IF Edipd.cmaPSet='NYL'
    oObject.laslines = .T.
  ENDIF
ENDFUNC
*:****************************************************************************************
*! Name      : lfNewPKG
*! Developer : Waleed Hamed (WLD)
*! Date      : 01/21/2008
*! Purpose   : Add seprate line for each Packing Info in receiving 850 map NYL
*:****************************************************************************************
*! Called from : Class "EDIPROCESSPO"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************

FUNCTION lfNewPKG
  IF lcPkgType <> MPkgType
    lcPkgType = MPkgType
    oObject.NEWPO1 = .T.
    *oObject.PO1Counter = oObject.PO1Counter + 1
  ELSE
    oObject.NEWPO1 = .F.
    *oObject.PO1Counter = oObject.PO1Counter - 1
  ENDIF

ENDFUNC
*:****************************************************************************************
*! Name      : lfAddPKG
*! Developer : Waleed Hamed (WLD)
*! Date      : 07/20/2008
*! Purpose   : Saving Packing Info in case of receiving 850 map NYL
*:****************************************************************************************
*! Called from : Class "EDIPROCESSPO"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*C201035,1 WLD 07/30/2008 Grouping PO1 depending on Packing Info in receiving 850 map NYL

FUNCTION lfAddPKG
  REPLACE cowner WITH MPKGTYPE  IN EDIORDLN
ENDFUNC

*:****************************************************************************************
*! Name      : lfPO1Grp
*! Developer : Waleed Hamed (WLD)
*! Date      : 07/20/2008
*! Purpose   : Grouping PO1 depending on Packing Info in receiving 850 map NYL
*:****************************************************************************************
*! Called from : Class "EDIPROCESSPO"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*C201035,1 WLD 07/30/2008 Grouping PO1 depending on Packing Info in receiving 850 map NYL

FUNCTION lfPO1Grp
  lcALIAS = ALIAS()
  SELECT EDIORDLN
  =SEEK(PADR(lcDumStore , 8) + XSTYLE + lcPack)
  SCAN REST WHILE EVALUATE(KEY()) = PADR(lcDumStore , 8) + XSTYLE + lcPack
    IF ALLT(EDIORDLN.cowner) == MPKGTYPE
      oObject.NEWPO1  =.F.
      oObject.PO1Counter = EDIORDLN.NPO1
      EXIT
    ELSE
      oObject.NEWPO1  =.T.
    ENDIF
  ENDSCAN
  SELECT (lcALIAS)
ENDFUNC
*:****************************************************************************************
*! Name      : lfEdtData
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2009
*! Purpose   : Add Edit Data Check box
*:****************************************************************************************
*! Called from : Visual label screen
*:****************************************************************************************
* WLD Add Edit Data Check box 07/09/2009
FUNCTION lfEdtData

  oFormSet.AriaForm1.ADDOBJECT('chkEditData','ARIACHECKBOX')

  WITH oFormSet.AriaForm1.chkEditData
    .CAPTION  = 'Edit Data'
    .LEFT     = 11
    .TOP      = 137
    .VISIBLE  = .T.
    .VALUE    =.F.
    .TABINDEX = 9
  ENDWITH
ENDFUNC
****lfEdtData

*:****************************************************************************************
*! Name      : lfOpnEdtData
*! Developer : Walid Hamed (WLD)
*! Date      : 07/09/2009
*! Purpose   : Open screen Edit Data
*:****************************************************************************************
*! Called from : PrnLabel of EDI
*:****************************************************************************************
*WLD Open screen Edit Data  07/09/2009
FUNCTION lfOpnEdtData

  IF oVsLblForm.AriaForm1.chkEditData.VALUE
    SELECT lcAliasTmpRpt
    tgmaSelect =SELECT()
    SELECT (oVsLblForm.TmpOutH)
    *DO FORM oAriaApplication.ScreenHome + 'AS\GMAEDTDT.scx' WITH oVsLblForm.TmpOutH,tgmaSelect,;
    IIF(oVsLblForm.AriaForm1.chkPrintDetailedLabel.VALUE,1,0)
    DO FORM oAriaApplication.clientscreenhome +'AS\KAZEDTDT.scx' WITH oVsLblForm.TmpOutH,tgmaSelect,;
      IIF(oVsLblForm.AriaForm1.chkPrintDetailedLabel.VALUE,1,0)

  ENDIF
ENDFUNC
****lfOpnEdtData
*:****************************************************************************************
*! Name      : lfFilArr
*! Developer : AHMED MOUSTAFA (AHS)
*! Date      : 10/20/2009
*! Purpose   : Fill array at UCC128
*:****************************************************************************************
*! Called from : Class "prnlabel"
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*C201197,1 AHS 10/26/2009 Customize BLK label for Kazu [Start]
FUNCTION lfFilArr
  IF SYCASNHD.CVER = 'BL5'
    lcCurAlias = ALIAS()
    lnBL5Cnt = 0
    SELECT PACK_LIN
    =SEEK(lcPackNo+STR(m.Cart_No,4),'PACK_LIN','PACKSTYLE')   &&pack_no+STR(no_cart,4)+style
    SCAN REST WHILE Pack_No+STR(no_cart,4)+STYLE = lcPackNo+STR(m.Cart_No,4)
      lnBL5Cnt = lnBL5Cnt+1
      IF lnBL5Cnt= 1 && 1st line
        lnPos = AT('-',STYLE)
        DETAIL11 = SUBSTR(STYLE,1,12) && Style major of 1st line
        DETAIL21 = SUBSTR(STYLE,lnPos+1,8) && Color of 1st line
        styqty1 = qty1
        styqty2 = qty2
        styqty3 = qty3
        styqty4 = qty4
        styqty5 = qty5
        styqty6 = qty6
        styqty7 = qty7
        styqty8 = qty8
      ENDIF
      IF lnBL5Cnt = 2 && 2nd line
        lnPos = AT('-',STYLE)
        DETAIL12 = SUBSTR(STYLE,1,12) && Style major of 2nd line
        DETAIL22 = SUBSTR(STYLE,lnPos+1,8) && Color of 2nd line
        DETAIL41 = qty1
        DETAIL42 = qty2
        DETAIL43 = qty3
        DETAIL44 = qty4
        DETAIL45 = qty5
        DETAIL46 = qty6
        DETAIL47 = qty7
        DETAIL48 = qty8
        EXIT
      ENDIF
    ENDSCAN
    SELECT (lcCurAlias)
  ENDIF
  *C201197,1 AHS 10/26/2009 Customize BLK label for Kazu [End]
