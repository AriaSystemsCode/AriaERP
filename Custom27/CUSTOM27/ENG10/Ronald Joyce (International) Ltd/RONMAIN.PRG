*!**************************************************************************
*! Name      : RONMAIN.PRG
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/24/2002
*! Purpose   : Ronald Joyce Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C200361,1
*!**************************************************************************
*
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfIgPick
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/24/2002
*! Purpose   : Ignore the picked ordline's.
*!**************************************************************************
*! Passed Parameters : None.
*!**************************************************************************
*
FUNCTION lfIgPick

IF llRpIgPick
  
  *C200387,1 AMH 07/21/2002 Ignore picked ordline's with zero available to allocate
  *C200387,1                and generate PO with the balance quantity [Start]
  *lcExpr = lcExpr + ' .AND. (EMPTY(ORDLINE.PIKTKT) .OR. ORDLINE.PIKTKT="******")'
  lcExpr = lcExpr + ' .AND. (TOTQTY-TOTCUT-TOTPIK > 0)'
  *C200387,1 AMH [End]
  
ENDIF
*-- End of lfIgPick.

*!**************************************************************************
*! Name      : lfGetBal  200387
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/21/2002
*! Purpose   : Get the balance quantity between order line quantity
*!             and its picked quantity
*!**************************************************************************
*! Passed Parameters : None.
*!**************************************************************************
*
FUNCTION lfGetBal

IF llRpIgPick
  m.Qty1 = m.Qty1 - m.Pik1
  m.Qty2 = m.Qty2 - m.Pik2
  m.Qty3 = m.Qty3 - m.Pik3
  m.Qty4 = m.Qty4 - m.Pik4
  m.Qty5 = m.Qty5 - m.Pik5
  m.Qty6 = m.Qty6 - m.Pik6
  m.Qty7 = m.Qty7 - m.Pik7
  m.Qty8 = m.Qty8 - m.Pik8
  m.TotQty = m.TotQty - m.TotPik
ENDIF
*-- End of lfGetBal.
*!**************************************************************************
*! Name      : lfComplExp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2002
*! Purpose   : To add the region to lcRpExp.
*!**************************************************************************
*! Example   : =lfComplExp()
*!**************************************************************************
*C200419,1
FUNCTION lfComplExp
IF !EMPTY(lcRegion)
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + [Customer.Region $ lcRegion]
ENDIF
*-- End of lfComplExp

*!**************************************************************************
*! Name      : lfCheckReg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2002
*! Purpose   : To check for Region value if changed.
*!**************************************************************************
*! Example   : =lfCheckReg()
*!**************************************************************************
*C200419,1
FUNCTION lfCheckReg
STORE '' TO lcDivision , lcRegion
=ASORT(laRpTRegn)
=ASORT(laRpTDivn)
FOR lnInd = 1 TO ALEN(laRpTRegn)
  lcRegion = lcRegion + PADR(laRpTRegn[lnInd],6) + SPACE(1)
ENDFOR
lcRegion = STRTRAN(ALLTRIM(lcRegion),' ',"|")

FOR lnInd = 1 TO ALEN(laRpTDivn)
  lcDivision = lcDivision + PADR(laRpTDivn[lnInd],6) + '|'
ENDFOR
lcDivision = IIF(LEN(ALLTRIM(lcDivision))=1,'',lcDivision)

*-- Check if either Region or Division is changed. 
*IF !llOGFltCh AND (!(lcRegion == lcOldRegn) OR !(lcDivision == lcOldDivsn))
*  llOGFltCh = .T.
*ENDIF

IF !llOGFltCh AND !(lcRegion == lcOldRegn)
  llOGFltCh = .T.
ENDIF
lcOldRegn = lcRegion
IF !llOGFltCh AND !(lcDivision == lcOldDivsn)
  llOGFltCh = .T.
ENDIF
lcOldDivsn = lcDivision
*-- End of lfCheckReg.

*!**************************************************************************
*! Name      : lfFillMovr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2002
*! Purpose   : To collect data for the region , division mover.
*!**************************************************************************
*! Example   : =lfFillMovr()
*!**************************************************************************
*C200419,1
FUNCTION lfFillMovr
PARAMETER lfDummy
lfDummy = .T.

DIMENSION laRpSRegn[1,1],laRpTRegn[1,1],laRpSDivn[1,1],laRpTDivn[1,1]
STORE '' TO laRpSRegn,laRpTRegn,laRpSDivn,laRpTDivn

*--The Region.
SELECT CODES
LOCATE
SELECT DISTINCT cCode_NO + ' - ' + cDiscRep ;
FROM CODES ;
WHERE cDefCode + cFld_Name + cCode_No + cDiscRep + cRltd_Nam = "NREGION" ;
AND crltField = 'N';
INTO ARRAY laRpSRegn

*--The Division.
SELECT CODES
LOCATE
SELECT DISTINCT cCode_NO + ' - ' + cDiscRep ;
FROM CODES ;
WHERE cDefCode + cFld_Name + cCode_No + cDiscRep + cRltd_Nam = "NCDIVISION" ;
AND crltField = 'N';
INTO ARRAY laRpSDivn

RETURN lfDummy
*-- End of lfFiLLMovr.

*!**************************************************************************
*! Name      : lfvRegion
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2002
*! Purpose   : To open the region mover.
*!**************************************************************************
*! Example   : =lfvRegion()
*!**************************************************************************
*C200419,1
FUNCTION lfvRegion
PARAMETER lfDummy
lfDummy = .T.

= gfMover(@laRpSRegn,@laRpTRegn,'Region',.T.,'')
RETURN lfDummy
*-- End of lfvRegion.

*!**************************************************************************
*! Name      : lfvDivson
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2002
*! Purpose   : To open the Division mover.
*!**************************************************************************
*! Example   : =lfvDivson()
*!**************************************************************************
*C200419,1
FUNCTION lfvDivson
PARAMETER lfDummy
lfDummy = .T.

= gfMover(@laRpSDivn,@laRpTDivn,'Division',.T.,'')
RETURN lfDummy
*-- End of lfvDivson.

*!**************************************************************************
*! Name      : lfCompFilt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2002
*! Purpose   : To add the Division to lcDebFlt , lcCreFlt , lcHstFlt.
*!**************************************************************************
*! Example   : =lfCompFilt()
*!**************************************************************************
*C200419,1
FUNCTION lfCompFilt
IF !EMPTY(lcDivision)
  lcDebFlt = lcDebFlt + IIF(EMPTY(lcDebFlt),'',[ AND ]) + ;
                        [IIF(TranType='1',IIF(SEEK(Tran,'InvHdr'),InvHdr.cDivision $ lcDivision,.T.),.T.)]
  lcCreFlt = lcCreFlt + IIF(EMPTY(lcCreFlt),'',[ AND ]) + ;
                        [IIF(TranType='0',IIF(SEEK(Tran,'RetHdr'),RetHdr.cDivision $ lcDivision,.T.),.T.)]
  lcHstFlt = lcHstFlt + IIF(EMPTY(lcHstFlt),'',[ AND ]) + ;
                        [IIF(TranType $ '01',IIF(SEEK(Tran,'InvHdr') OR (SEEK(Tran,'RetHdr') AND SEEK(RetHdr.Invoice,'InvHdr')),InvHdr.cDivision $ lcDivision,.T.),.T.)]
ENDIF
*-- End of lfCompFilt
