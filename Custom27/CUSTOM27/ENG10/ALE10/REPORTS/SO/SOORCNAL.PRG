*:**************************************************************************
*: Program file  : SOORCNAL
*: Program desc. : Custom Form for Alena (England).
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C200229
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*:B606722,1 ASH 12/18/2002 Print Note1, Note2 [Change in FRX only]
*:**************************************************************************
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcTempOrd)
INDEX ON cOrdType + Order + Style + Store + PrePak TAG (lcTempIndx)

SELECT (lcAlias)
*-- End of program

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/15/2001
*! Purpose   : Calculate last Record in order details.
*!*************************************************************
*! Called from : [SORDCONAL.FRX, ORDER GROUP HEADER BAND] 
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
*
FUNCTION lfLastRec
PARAMETER lcDummy
lcDummy = .T.
PRIVATE lcThAlias,lnThRec
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTempOrd)
lnThRec = RECNO(lcTempOrd)    && Save Current record #.
LOCATE REST FOR ( cOrdType + Order > OrdHdr.cordtype + OrdHdr.order)
IF (Order != OrdHdr.Order)
  SKIP -1
ENDIF   
lnLastRec = RECNO(lcTempOrd)

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF

IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF

SELECT (lcThAlias)            && Restore Alias.
RETURN ''
*-- End of lfLastRec.
