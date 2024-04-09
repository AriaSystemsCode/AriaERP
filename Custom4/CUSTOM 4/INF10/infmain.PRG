*!***************************************************************************************************************************************
*! Name      : INFMAIN.PRG
*! Developer : Walid Hamed (WLD)
*! Date      : 02/21/2008
*! Purpose   : Infinity Classics Main Program .
*!***************************************************************************************************************************************
*! Parameters: loFormSet -> FormSet object
*!             lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!***************************************************************************************************************************************
*! Returns   : Logical value.
*!***************************************************************************************************************************************
*! Modifcations :
* C200945,1 WLD 02/21/2008 Custom Scan and Pack Program
*!***************************************************************************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : lfUPCFRMT
*! Developer : Walid Hamed (WLD)
*! Date      : 02/21/2008
*! Purpose   : Change UPC Format
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
* C200945,1 WLD 02/21/2008 Custom Scan and Pack Program
FUNCTION lfUPCFRMT

  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.INPUTMASK = ''
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED  = .T.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)
ENDFUNC
*End Function  lfUPCFRMT

*!*************************************************************
*! Name      : lfUPCQTY
*! Developer : Walid Hamed (WLD)
*! Date      : 02/21/2008
*! Purpose   : Fill UPC QTY
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
* C200945,1 WLD 02/21/2008 Custom Scan and Pack Program
FUNCTION lfUPCQTY

  IF llUpcAdd_INF
    lcAlias_Inf = ALIAS()
    SELECT (loFormSet.lcCtnDtl)
    IF  SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+EVALUATE(loFormSet.lcPackLines+'.STYLE')+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze')
      =SEEK(EVALUATE(loFormSet.lcPackLines+'.STYLE')+ORDLINE.Dyelot+STR(ORDLINE.LINENO,6)+ALLTRIM(sTYLEUPC.SIZE),loFormSet.lcPackLines,loFormSet.lcPackLines)
      IF EVALUATE(loFormSet.lcPckLin+'.OQty') > 0 && EVALUATE(loFormSet.lcPackLines+'.OQty') > 0
        lnStyQty = EVALUATE(loFormSet.lcPckLin+'.OQty') && EVALUATE(loFormSet.lcPackLines+'.OQty') - EVAL(loFormSet.lcCtnDtl+'.Qty')
        IF lnStyQty > 0
          loFormSet.lnPackQty = loFormSet.lnPackQty - EVAL(loFormSet.lcCtnDtl+'.Qty')
          loFormSet.lnPackWgh = loFormSet.lnPackWgh - EVAL(loFormSet.lcCtnDtl+'.Weight')
          REPLACE TotPcs WITH (TotPcs - EVAL(loFormSet.lcCtnDtl+'.Qty')) ,;
            TotWgh WITH (TotWgh - EVAL(loFormSet.lcCtnDtl+'.Weight')) IN (loFormSet.lcCtnHdr)
          REPLACE PWgh   WITH PWgh  - EVAL(loFormSet.lcCtnDtl+'.Weight')   IN (loFormSet.lcPckLin)
          lnWgh = EVAL(loFormSet.lcCtnDtl+'.Weight') + (lnStyQty * EVALUATE(loFormSet.lcPackLines+'.StyWgh'))
        ENDIF
      ELSE
        lnStyQty = 0

      ENDIF
    ELSE
      lnStyQty = IIF(lcType_INF<>"S",MIN(STYLE.nInPackQty,EVALUATE(loFormSet.lcPckLin+'.OQty')),EVALUATE(loFormSet.lcPackLines+'.OQty'))
      lnWgh = lnStyQty * EVALUATE(loFormSet.lcPackLines+'.StyWgh')
    ENDIF
    SELECT (lcAlias_Inf)
  ENDIF
ENDFUNC
*End Function  lfUPCQTY

*!*************************************************************
*! Name      : lfAUTOSCAN
*! Developer : Walid Hamed (WLD)
*! Date      : 02/21/2008
*! Purpose   : Automating the Scan and Pack
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
* C200945,1 WLD 02/21/2008 Custom Scan and Pack Program
FUNCTION lfAUTOSCAN

  lcType_INF = LEFT(UPPER(ALLTRIM(lcUPC)),1)

  DO CASE
    CASE  TYPE(lcType_INF)= 'N'
      IF EOF(loFormSet.lcCtnHdr)
        =lfvCtHdNew(loFormset)
        llRtn_INF = .F.
      ENDIF
      IF loFormSet.STYLEUPC.SEEK(lcUpc)
        SELECT OrdLine
        =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
        LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
          'O'+lcOrderNo+lcStore+StyleUpc.STYLE;
          FOR PikTkt = lcPackNo
        IF FOUND()
          IF SEEK(StyleUPC.STYLE+ORDLINE.Dyelot+STR(ORDLINE.LINENO,6)+ALLTRIM(sTYLEUPC.SIZE),loFormSet.lcPckLin,loFormSet.lcPckLin) ;
              AND  EVALUATE(loFormSet.lcPckLin+'.OQty')= 0
            llRtn_INF = .T.
          ENDIF
        ENDIF
      ENDIF

    CASE  lcType_INF ='N'
      loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)
      =lfvCtHdNew(loFormset)
      lcUpc = SPACE(13)
      llRtn_INF = .T.

    CASE lcType_INF ='S'
      loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)
      lcUpc = SPACE(13)
      lcOldAlias =ALIAS()
      SELECT (loFormSet.lcPackLines)
      lnOldRec = RECNO(loFormSet.lcPackLines)
      lcCtnSty = EVALUATE(loFormSet.lcCtnDtl+'.Style')
      IF EMPTY(lcCtnSty) AND EOF(loFormSet.lcCtnHdr)
        SELECT (loFormSet.lcPackLines)
        LOCATE
        lcCtnSty = EVALUATE(loFormSet.lcPackLines+'.Style')
        =lfvCtHdNew(loFormset)
      ENDIF
      IF !EMPTY(lcCtnSty)
        =SEEK(lcCtnSty,loFormSet.lcPackLines)
        SCAN REST WHILE STYLE+Dyelot+STR(nOrdLineNo,6)+cSizeNo = lcCtnSty
          IF EVALUATE(loFormSet.lcPackLines+'.cSizeno') != Styleupc.SIZE
            IF loFormSet.llUPCInst AND loFormSet.STYLEUPC.SEEK(EVALUATE(loFormSet.lcPackLines+'.Style') +EVALUATE(loFormSet.lcPackLines+'.cSizeno'),'StyleUpc')
              lcUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
              =lfvCtnSty(loFormSet,.F.,lcCtnSty,.T.)
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
      llRtn_INF = .T.

  ENDCASE
  lcFrmCnfrm = loFormSet.lcSetConf
  SET CONFIRM &lcFrmCnfrm

ENDFUNC
*End Function  lfUPCQTY
*!*************************************************************
*! Name      : lfNEWSCAN
*! Developer : Walid Hamed (WLD)
*! Date      : 02/21/2008
*! Purpose   : Scan Barcode of New Carton
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
* C200945,1 WLD 02/21/2008 Custom Scan and Pack Program
FUNCTION lfNEWSCAN
  STORE .T. TO loFormSet.AriaForm1.pgfPacking.CartonInfo.cmdScan.ENABLED
ENDFUNC
*End Function  lfNEWSCAN

*!*************************************************************
*! Name      : lfSNDALLQTY
*! Developer : Walid Hamed (WLD)
*! Date      : 02/21/2008
*! Purpose   : In case of Send All Requested Qty we will prevent in Valid UPC method to show message
*!             "Packed Quantity for Style/Size XXXXX exceeds Ordered Quantity. Can not modify the Ordered Quantity?"
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
* C200945,1 WLD 02/21/2008 Custom Scan and Pack Program
FUNCTION lfSNDALLQTY
  IF lcType_INF ='S'
    STORE 0 TO lnPacked
  ENDIF
ENDFUNC
*End Function  lfSNDALLQTY
