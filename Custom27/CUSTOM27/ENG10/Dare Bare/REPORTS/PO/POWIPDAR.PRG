****************************************************************************
*: Program file      : POWIPDAR.PRG   (C#200440)
*: Program desc.     : WORK IN PROGRESS REPORT
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : AHMED MAHER (AMH)
*: Date              : 01/14/2003
*:**************************************************************************

lcTempFile = gfTempName()
CREATE CURSOR (lcTempFile) (CVENDOR C(8),CPO C(6),CSTATUS C(1),CORDER C(6),CSTYLE C(12),;
                            CSTYDESC C(30),CCLRDESC C(30),CACCOUNT C(5),DENTERED D,DCOMPLETE D,;
                            DSTARTPROD D,NTOTQTY N(7,0),DRECDATE D,NRECQTY N(7,0),;
                            NDUEQTY N(7,0),NLINENO N(6,0))
INDEX ON CVENDOR+DTOS(DCOMPLETE)+CPO+STR(NLINENO,6) TAG (lcTempFile) OF (lcTempFile)

*-- To get the selected vendor if any.
lcVendFile = ''
llUseVendr = .F.
lnPosition = ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcVendFile = laOGFxFlt[lnPosition,6]
  llUseVendr = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Customer if any.
lcCustFile = ''
llUseCustm = .F.
lnPosition = ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcCustFile = laOGFxFlt[lnPosition,6]
  llUseCustm = IIF(!EMPTY(lcCustFile) .AND. USED(lcCustFile) .AND. RECCOUNT(lcCustFile)>0,.T.,.F.)
ENDIF

*-- To get the complete date if any.
lcRpExp0 = ".T."
lnPosition = ASCAN(laOGFxFlt,'POSHDR.COMPLETE')
IF lnPosition > 0
  =ACOPY(laOGFxFlt,laBtwFlt,lnPosition,7)
  DIME laBtwFlt[1,7]
  lcRpExp0 = gfGenFlt('laBtwFlt',.T.)
  lcRpExp0 = IIF(EMPTY(lcRpExp0),".T.",lcRpExp0)
ENDIF
lcRpExp0 = lcRpExp0 + " .AND. POSHDR.STATUS" + IIF(llActOnly,"='A'","$'HOA'")
lcRpExp0 = lcRpExp0 + IIF(llExclFnsh," .AND. !POSHDR.CFINISH","")
DECLARE laQty[8]
STORE 0 TO laQty

IF llUseVendr
  SELECT (lcVendFile)
  SCAN
    IF SEEK(CVENDCODE+'P','POSHDR')
      SELECT POSHDR
      SCAN REST WHILE VENDOR+CSTYTYPE+PO = EVALUATE(lcVendFile+'.CVENDCODE')+'P';
                FOR &lcRpExp0.
        WAIT WINDOW 'PO # : '+PO NOWAIT
        llInWork = SEEK('I'+PO,'MFGOPRDT')
        IF llWorkOnly .AND. !llInWork
          LOOP
        ENDIF
        IF SEEK('P'+POSHDR.PO,'POSLN')
          SELECT POSLN
          SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'P'+POSHDR.PO;
                      FOR TRANCD = '1'
            *khalid
            =SEEK('2'+PO+STR(LINENO,6),'CUTPICK')
            =SEEK('O'+CUTPICK.ORDER,'ORDHDR')
            *IF !SEEK('2'+PO+STR(LINENO,6),'CUTPICK') .OR. !SEEK('O'+CUTPICK.ORDER,'ORDHDR');
               .OR. IIF(llUseCustm,!SEEK(ORDHDR.ACCOUNT,lcCustFile),.F.)
            
            IF IIF(llUseCustm,!SEEK(ORDHDR.ACCOUNT,lcCustFile),.F.)
            *khalid
            
              LOOP
            ENDIF
            =lfUpdTemp()
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  IF llUseCustm
    SET ORDER TO ORDACCT IN ORDHDR
    SET ORDER TO CUTORD IN CUTPICK
    SET ORDER TO POSHDR IN POSHDR
    SELECT (lcCustFile)
    SCAN
      IF SEEK(ACCOUNT+'O','ORDHDR')
        SELECT ORDHDR
        SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = EVALUATE(lcCustFile+'.ACCOUNT')+'O'
          IF SEEK('2'+ORDER,'CUTPICK')
            SELECT CUTPICK
            SCAN REST WHILE TRANCD+ORDER+CORDLINE = '2'+ORDHDR.ORDER
              IF SEEK('P'+CTKTNO+STYLE+CTKTLINENO+'1','POSLN')
                SELECT POSLN
                SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD =;
                                'P'+CUTPICK.CTKTNO+CUTPICK.STYLE+CUTPICK.CTKTLINENO+'1'
                  WAIT WINDOW 'PO # : '+PO NOWAIT
                  =SEEK('P'+PO,'POSHDR')
                  IF &lcRpExp0.
                    LOOP
                  ENDIF
                  llInWork = SEEK('I'+PO,'MFGOPRDT')
                  IF llWorkOnly .AND. !llInWork
                    LOOP
                  ENDIF
                  =lfUpdTemp()
                ENDSCAN
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
    SET ORDER TO POSHDRV IN POSHDR
    SET ORDER TO CUTPKORD IN CUTPICK
    SET ORDER TO ORDHDR IN ORDHDR
  ELSE
    SELECT POSHDR
    SCAN FOR &lcRpExp0.
      WAIT WINDOW 'PO # : '+PO NOWAIT
      llInWork = SEEK('I'+PO,'MFGOPRDT')
      IF llWorkOnly .AND. !llInWork
        LOOP
      ENDIF
      IF SEEK('P'+POSHDR.PO,'POSLN')
        SELECT POSLN
        SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'P'+POSHDR.PO;
                    FOR TRANCD = '1'
          *khalid
          =SEEK('2'+PO+STR(LINENO,6),'CUTPICK')
          =SEEK('O'+CUTPICK.ORDER,'ORDHDR')
          *IF !SEEK('2'+PO+STR(LINENO,6),'CUTPICK') .OR. !SEEK('O'+CUTPICK.ORDER,'ORDHDR')
          *  LOOP
          *ENDIF
          *khalid
          
          =lfUpdTemp()
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
WAIT CLEAR

SELECT (lcTempFile)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

DO gfDispRe WITH EVAL('lcRpName')
USE IN (lcTempFile)
*-- end of Report Code.

*!*************************************************************
*! Name      : lfsrvVen
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/14/2003
*! Purpose   : Rise change Vendor flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvVen()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVVen
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GO TOP IN APVENDOR
ENDIF
*-- End Of lfSRVVen.

*!*************************************************************
*! Name      : lfUpdTemp
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/16/2003
*! Purpose   : Update the temp file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdTemp()
*!*************************************************************
*
FUNCTION lfUpdTemp

=SEEK(STYLE,'STYLE')
STORE 0 TO laQty
M.CVENDOR    = POSHDR.VENDOR
M.CPO        = PO
M.CSTATUS    = IIF(POSHDR.STATUS='O',IIF(llInWork,'W','O'),;
               IIF(POSHDR.STATUS='A',IIF(POSHDR.CFINISH,'F','A'),'H'))
M.CORDER     = CUTPICK.ORDER
M.CSTYLE     = SUBSTR(STYLE,1,12)
M.CSTYDESC   = SUBSTR(STYLE.DESC1,1,30)
M.CCLRDESC   = gfCodDes(SUBSTR(STYLE,14),'COLOR')
M.CACCOUNT   = ORDHDR.ACCOUNT
M.DENTERED   = POSHDR.ENTERED
M.DCOMPLETE  = POSHDR.COMPLETE
M.DSTARTPROD = POSHDR.CSTARTPROD
M.NTOTQTY    = TOTQTY
M.NLINENO    = LINENO
M.NDUEQTY    = 0
INSERT INTO (lcTempFile) FROM MEMVAR
lnRecLines = 0
lcPosLnKey = CSTYTYPE+PO+STYLE+STR(LINENO,6)+'2'
IF SEEK(lcPosLnKey,'POSLN_A')
  SELECT POSLN_A
  SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = lcPosLnKey
    lnRecLines = lnRecLines + 1
    M.DRECDATE = DATE
    M.NRECQTY  = TOTQTY
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      laQty[lnI] = laQty[lnI] + EVALUATE('QTY'+lcI)
    ENDFOR
    IF lnRecLines > 1
      INSERT INTO (lcTempFile) FROM MEMVAR
    ELSE
      SELECT (lcTempFile)
      GATHER FIELDS DRECDATE,NRECQTY MEMVAR
    ENDIF
  ENDSCAN
ENDIF
SELECT POSLN
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  M.NDUEQTY = M.NDUEQTY + MAX(EVALUATE('QTY'+lcI)-laQty[lnI],0)
ENDFOR
SELECT (lcTempFile)
GATHER FIELDS NDUEQTY MEMVAR
*-- end of lfUpdTemp.