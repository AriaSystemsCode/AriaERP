****************************************************************************
*: Program file      : ICFPNDP.PRG
*: Program desc.     : FRESH PRODUCE NAMEDROP REPORT
*: System            : Aria Apparel System (A27).
*: Module            : (IC)
*: Developer         : TAK
*: Date              : 10/26/2000
*:**************************************************************************


WAIT WINDOW 'Start Collecting Orders...' NOWAIT

*--Default the order status to Open and Hold.
lcOrdFltr = "ORDHDR.STATUS $ 'OH'"

lcSelDate = ""
IF OCCURS('ORDHDR.ENTERED',lcRpExp) <> 0
  IF lcDateType <> 'E'
    lcRpExp   = STRTRAN(lcRpExp,'ORDHDR.ENTERED','ORDHDR.'+IIF(lcDateType = 'C','COMPLETE','START'))
  ENDIF
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
  
  IF lnPosition > 0 AND !EMPTY(SUBSTR(laOGFxFlt[lnPosition,6],1,2))
    lcSelDate = IIF(lcDateType='E','Enter Date:',IIF(lcDateType='C','Complete Date:','Start Dates:'))+STRTRAN(laOGFxFlt[lnPosition,6],'|',' - ')
  ENDIF
  
  lcOrdFltr = lcOrdFltr + " AND " + lcRpExp
ENDIF



lcTmpFile = gfTempName()


CREATE TABLE(gcWorkDir+lcTmpFile);
    (CDSGNCODE C(6),CDSGNNAME C(30),CDSGNSIZE C(6), CDSGNCLR C(15), NND_SIZE N(6,2), ;
     CNDRPTT C(40),CNDRPTB C(40),CSPECINS C(40), cNDrpId C(6), ;
     ORDER C(6), ACCOUNT C(5),REP1 C(3),ENTERED D(8),START D(8),COMPLETE D(8) )
INDEX ON cDsgnCode+cNDrpId+Order TAG (lcTmpFile)

*--Collecting Data.
SELECT BOMVAR
SEEK 'SO'
SCAN REST WHILE cIDType='SO' FOR !EMPTY(cNDrpId)
  WAIT WINDOW 'Orders :'+cCost_Id NOWAIT
  
  lcFlKey = BOMVAR.cDsgnCode+BOMVAR.cNDrpId+BOMVAR.cCost_Id
  
  IF SEEK('O'+cCost_Id,'ORDHDR') AND &lcOrdFltr
     IF SEEK(cDsgnCode,'ICDESIGN') AND SEEK(cNDrpId,'ICNAMDRP') AND !SEEK(lcFlKey,lcTmpFile)
       =lfUpdTmp()
     ENDIF
  ENDIF
ENDSCAN
WAIT CLEAR

SELECT (lcTmpFile)
GO TOP
IF !EOF()
  INDEX ON STR(NND_SIZE,6,2)+CDSGNSIZE+ORDER TAG(lcTmpFile) DESCENDING
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
  GO TOP

  = gfDispRe(lcRpForm)

  SET RELATION TO
ELSE
  *--No records was selected, Nothing to print!!
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVI TO SCREEN
ENDIF

IF USED(lcTmpFile)
  SELECT (lcTmpFile)
  USE
  ERASE(gcWorkDir+lcTmpFile+'.DBF')
  ERASE(gcWorkDir+lcTmpFile+'.CDX')
ENDIF

RETURN

**********************************************
FUNCTION lfUpdTmp

SELECT (lcTmpFile)
APPEND BLANK
REPLACE CDSGNCODE WITH BOMVAR.cDsgnCode,; 
        cNDrpId   WITH BOMVAR.cNDrpId,;
        ORDER     WITH BOMVAR.cCost_Id,;
        CDSGNNAME WITH ICDESIGN.CDSGNNAME,; 
        CDSGNSIZE WITH ICDESIGN.CDSGNSIZE,; 
        CDSGNCLR  WITH ICDESIGN.CDSGNCLR,; 
        NND_SIZE  WITH ICDESIGN.NND_SIZE,;
        CNDRPTT   WITH ICNAMDRP.CNDRPTT,;   
        CNDRPTB   WITH ICNAMDRP.CNDRPTB,;   
        CSPECINS  WITH ICNAMDRP.CSPECINS,;  
        ACCOUNT   WITH ORDHDR.Account,;
        REP1      WITH ORDHDR.Rep1 ,;
        ENTERED   WITH ORDHDR.Entered,;
        START     WITH ORDHDR.START,;
        COMPLETE  WITH ORDHDR.COMPLETE
RETURN
