CLOSE DATA
lcDataDir = GETDIR('','SELECT DATA DIRECTORY:')

IF !EMPTY(lcDataDir)
  lcWorkDir = GETENV('TMP')+'\'
  IF EMPTY(lcWorkDir)
    lcWorkDir = GETDIR('','SELECT TEMP. DIRECTORY:')
  ENDIF
  IF EMPTY(lcWorkDir)
    RETURN
  ENDIF
ELSE
  RETURN
ENDIF

lcTempFile = 'X'+RIGHT(SYS(2015),7)

USE (lcDataDir+'STYLE')    ORDER  TAG STYLE    IN 0
USE (lcDataDir+'STYDYE')   ORDER  TAG STYDYE   IN 0
USE (lcDataDir+'STYINVJL') ORDER  TAG STYINVJL IN 0
USE (lcDataDir+'BOMLINE')  ORDER  TAG BOMLINE  IN 0
USE (lcDataDir+'BOMCOST')  ORDER  TAG POBOMCLS  IN 0
USE (lcDataDir+'InvtAdj')  ORDER  TAG InvtAdj  IN 0
USE (lcDataDir+'InvLine')  ORDER  TAG Invlines IN 0
USE (lcDataDir+'GLDIST')   ORDER  TAG Gldistno IN 0
USE (lcDataDir+'CUTTKTH')  ORDER  TAG CUTTKTH  IN 0
USE (lcDataDir+'CUTTKTL')  ORDER  TAG CUTTKTL  IN 0
SELECT CUTTKTL
COPY STRU TO (lcWorkDir+lcTempFile)
USE (lcWorkDir+lcTempFile) IN 0 EXCL
SELECT (lcTempFile)
INDEX ON STYLE+cWareCode+cRSession TAG StyWare OF (lcWorkDir+lcTempFile) 

SELECT CUTTKTL
SCAN FOR TranCd = '2'
  WAIT WINDOW 'Fixing Cuttkt # '+Cuttkt NOWAIT

  STORE 0 TO lnLCost1,lnLCost2,lnLCost3,lnLCost4,lnLCost5
  SELECT BOMLINE
  IF SEEK('M2'+CUTTKTL.Cuttkt)
    *--Calculate landed cost.
    FOR lnCnt=1 TO 5
      lcCnt = STR(lnCnt,1)
      =SEEK('M2'+CUTTKTL.Cuttkt)
      SUM REST (UnitCost*UnitQty)*StyQty ;
         WHILE cImTyp+cType+cTktNo = 'M2'+CUTTKTL.Cuttkt ;
           FOR cBomTyp=lcCnt AND Style=CUTTKTL.Style AND cRSession=CUTTKTL.cRSession ;
            TO lnLCost&lcCnt
      lnLCost&lcCnt = IIF(CUTTKTL.TotQty<>0,(lnLCost&lcCnt/CUTTKTL.TotQty),0)
    ENDFOR

    IF CUTTKTL.nLan_Cst1 = lnLCost1 AND CUTTKTL.nLan_Cst2 = lnLCost2 AND ;
       CUTTKTL.nLan_Cst3 = lnLCost3 AND CUTTKTL.nLan_Cst4 = lnLCost4 AND ;
       CUTTKTL.nLan_Cst5 = lnLCost5
      LOOP
    ENDIF

    SELECT (lcTempFile)
    IF !SEEK(CUTTKTL.Style+CUTTKTL.cWareCode)
      APPEND BLANK
      REPLACE STYLE     WITH CUTTKTL.Style     ,;
              LINENO    WITH CUTTKTL.LineNo    ,;
              cRSession WITH CUTTKTL.cRSession ,;
              cWareCode WITH CUTTKTL.cWareCode 
    ELSE
      IF CUTTKTL.cRSession < cRsession
        REPLACE cRSession WITH CUTTKTL.cRSession 
      ENDIF          
    ENDIF              
            
         
    *-- Cuttktl
    SELECT CUTTKTL
    DIME laOldLanCst[5]
    laOldLanCst = 0
    SCATTER FIELDS nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5 TO laOldLanCst
    =RLOCK()
    REPLACE nLan_Cst1 WITH lnLCost1,;
            nLan_Cst2 WITH lnLCost2,;
            nLan_Cst3 WITH lnLCost3,;
            nLan_Cst4 WITH lnLCost4,;
            nLan_Cst5 WITH lnLCost5
    UNLOCK
    lnNewCost = nLan_Cst1 +nLan_Cst2 +nLan_Cst3 +nLan_Cst4 +nLan_Cst5

    *-- Cuttkth
    SELECT CUTTKTH
    IF SEEK(CUTTKTL.Cuttkt)
      =RLOCK()
      REPLACE nlan_cost1 WITH nlan_cost1 + (CUTTKTL.nLan_Cst1 -laOldLanCst[1]),;
              nlan_cost2 WITH nlan_cost2 + (CUTTKTL.nLan_Cst2 -laOldLanCst[2]),;
              nlan_cost3 WITH nlan_cost3 + (CUTTKTL.nLan_Cst3 -laOldLanCst[3]),;
              nlan_cost4 WITH nlan_cost4 + (CUTTKTL.nLan_Cst4 -laOldLanCst[4]),;
              nlan_cost5 WITH nlan_cost5 + (CUTTKTL.nLan_Cst5 -laOldLanCst[5])
      UNLOCK
    ENDIF
 
    SELECT StyINvJl
    IF SEEK(CUTTKTL.Style+CUTTKTL.cWareCode+CUTTKTL.cRSession+;
            DTOS(CUTTKTL.Date)+CUTTKTL.CutTkt)
      LOCATE REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode = ;
                        CUTTKTL.Style+CUTTKTL.cWareCode+CUTTKTL.cRSession+;
                        DTOS(CUTTKTL.Date)+CUTTKTL.CutTkt ;
                  FOR LineNo = CUTTKTL.LineNo AND cTrType = '5'
      IF FOUND()
        REPLACE nCost   WITH lnNewCost ,;
                nStkVal WITH nTotStk * lnNewCost
        SELECT GLDIST
        IF SEEK(CUTTKTL.CutTkt+ "CT" +CUTTKTL.cRSession+'006') 
          REPLACE nEqvAmnt  WITH StyInvJl.nStkVal ,;
                  nGLAmount WITH StyInvJl.nStkVal
          IF SEEK(CUTTKTL.CutTkt+"CT"+CUTTKTL.cRSession+'013')
            REPLACE nEqvAmnt  WITH StyInvJl.nStkVal ,;
                    nGLAmount WITH StyInvJl.nStkVal
          ENDIF
        ENDIF
      ENDIF
    ENDIF
 
  ENDIF
 
  
ENDSCAN

*B802475 (Start)

lcStyle = &lcTempFile..Style
DIMENSION laAvgCost[2]

SELECT (lcTempFile)
SCAN
  IF SEEK(&lcTempFile..Style+&lcTempFile..cWareCode,'StyInvJl')
    STORE 0 TO laAvgCost,lnAvgCost
    
    SELECT StyInvJl
    SCAN REST WHILE Style+cWareCode = &lcTempFile..Style+&lcTempFile..cWareCode;
                    AND cSession <= &lcTempFile..cRSession
      *-- Calculate AVG Cost
      = lfAccum()
    ENDSCAN
    SCAN REST WHILE Style+cWareCode = &lcTempFile..Style+&lcTempFile..cWareCode
      *-- Fix the cost in the Journal, Gl Entries, Transaction File
      IF cIRtype  = 'I'
        = lfFxTran()
      ENDIF  
      *-- Calculate AVG Cost
      = lfAccum() 
    ENDSCAN
    
    *-- Update the StyDye with AvgCost
    IF SEEK(&lcTempFile..Style+&lcTempFile..cWareCode,'StyDye')
      SELECT StyDye
      REPLACE nStkVal  WITH laAvgCost[1],;
              Ave_Cost WITH lnAvgCost
    ENDIF
    
    IF lcStyle <> &lcTempFile..Style
      *-- Update the Style Avg Cost by calculation from the StyDye
      SELECT SUM(nStkVal),SUM(TotStk) FROM StyDye;
        WHERE Style+cWareCode+Dyelot = lcStyle ;
        INTO ARRAY laStyleInfo

      IF SEEK(lcStyle,'Style')
        SELECT Style
        REPLACE nStkVal  WITH laStyleInfo[1];
                Ave_Cost WITH IIF(ASCAN(laStyleInfo,0)>0,lnAvgCost,laStyleInfo[1]/laStyleInfo[2])
      ENDIF
      lcStyle = &lcTempFile..Style
    ENDIF  
    
  ENDIF
ENDSCAN
      SELECT SUM(nStkVal),SUM(TotStk) FROM StyDye;
        WHERE Style+cWareCode+Dyelot = lcStyle ;
        INTO ARRAY laStyleInfo

      IF SEEK(lcStyle,'Style')
        SELECT Style
        REPLACE nStkVal  WITH laStyleInfo[1];
                Ave_Cost WITH IIF(ASCAN(laStyleInfo,0)>0,lnAvgCost,laStyleInfo[1]/laStyleInfo[2])
      ENDIF
      lcStyle = &lcTempFile..Style

*B802475 (End)

CLOSE ALL
ERASE (lcWorkDir+lcTempFile+'.DBF')
ERASE (lcWorkDir+lcTempFile+'.CDX')

*******************************************

FUNCTION lfFxTran

*-- Update STyInvJr
SELECT StyInvJl
REPLACE nCost   WITH lnAvgCost ,;
        nStkVal WITH nTotStk*lnAvgCost
 
*-- Update GL Entries
DO CASE
  CASE INLIST(StyInvJl.cTrType,'1','I')
    lcTrType = 'IA'
    IF SEEK('M'+StyInvJl.cTrCode,'BOMCost')
      SELECT BOMCost
*      LOCATE REST WHILE cimtyp + ctktno = 'M'+StyInvJl.cTrCode ;
                  FOR   Item+cISession+cDyelot+cWareCode  = ;
                        StyInvJl.Style+StyInvJl.CISession + ;
                        StyInvJl.cDyelot+StyInvJl.cWareCode
      LOCATE REST WHILE cimtyp + ctktno = 'M'+StyInvJl.cTrCode ;
                  FOR   Item+cDyelot+cWareCode  = ;
                        StyInvJl.Style+ ;
                        StyInvJl.cDyelot+StyInvJl.cWareCode                        
      IF FOUND()
        lcCatKey = '013'
      ELSE
        lcCatKey = '007'
      ENDIF
    ELSE
      IF SEEK('I'+StyInvJl.cTrCode,'BOMCost')
        SELECT BOMCost
*        LOCATE REST WHILE cimtyp + ctktno = 'I'+StyInvJl.cTrCode ;
                    FOR   Item+cISession+cDyelot+cWareCode  = ;
                          StyInvJl.Style+StyInvJl.CISession + ;
                          StyInvJl.cDyelot+StyInvJl.cWareCode
        LOCATE REST WHILE cimtyp + ctktno = 'I'+StyInvJl.cTrCode ;
                    FOR   Item+cDyelot+cWareCode  = ;
                          StyInvJl.Style+ ;
                          StyInvJl.cDyelot+StyInvJl.cWareCode                          
        IF FOUND()
          lcCatKey = '013'
        ELSE
          lcCatKey = '007'
        ENDIF
      ELSE
        lcCatKey = '007'
      ENDIF
    ENDIF
  CASE StyInvJl.cTrType = '2'
    lcTrType = 'IP'
    lcCatKey = '007'
  CASE StyInvJl.cTrType = '3'
    lcTrType = 'IN'
    lcCatKey = '008'
  CASE StyInvJl.cTrType = '8'
    lcTrType = 'VR'
    lcCatKey = '008'
ENDCASE

SELECT GLDIST
IF SEEK(StyInvJl.cTrCode+lcTrType)
  LOCATE REST WHILE tran_no+tran_type+glsession+catg_key = ;
                    StyInvJl.cTrCode+lcTrType ;
              FOR Catg_Key = '006'
  IF FOUND()
    REPLACE nEqvAmnt  WITH StyInvJl.nStkVal ,;
            nGLAmount WITH StyInvJl.nStkVal
    
    IF SEEK(StyInvJl.cTrCode+lcTrType)
      LOCATE REST WHILE tran_no+tran_type+glsession+catg_key = ;
                        StyInvJl.cTrCode+lcTrType ;
              FOR Catg_Key = lcCatKey
      IF FOUND()
        REPLACE nEqvAmnt  WITH -StyInvJl.nStkVal ,;
                nGLAmount WITH -StyInvJl.nStkVal
      ENDIF
    ENDIF
  ENDIF
ENDIF

*-- Update Transaction Files
DO CASE
  CASE INLIST(StyInvJl.cTrType,'1','2','I')
    SELECT BOMCost
    IF FOUND()
        LOCATE REST WHILE cimtyp + ctktno = 'M'+StyInvJl.cTrCode ;
                  FOR   Item+cISession+cDyelot+cWareCode  = ;
                        StyInvJl.Style+STR(VAL(StyInvJl.CISession)-1,6) + ;
                        StyInvJl.cDyelot+StyInvJl.cWareCode
                        
       REPLACE nUnitCst WITH lnAvgCost ;
               nTotCst  WITH nTotQty*lnAvgCost
    ELSE
      SELECT InvtAdj
      IF SEEK(StyInvJl.Style)
        LOCATE REST WHILE Style = StyInvJl.Style      ;
                    FOR   cFromWare+Dyelot+cSession = ;
                          StyInvJl.Style+StyInvJl.cDyelot+StyInvJl.cSession ;
                    AND   TotAdj < 0
        IF FOUND()
          REPLACE Unt_Cost WITH lnAvgCost
        ENDIF
      ENDIF
    ENDIF
  CASE StyInvJl.cTrType = '3'
    SELECT InvLine
    IF SEEK(StyInvJl.Style+StyInvJl.cTrCode)
      SCAN REST WHILE Style+Invoice = StyInvJl.Style+StyInvJl.cTrCode
        REPLACE Cost WITH lnAvgCost
      ENDSCAN
    ENDIF
ENDCASE

*******************************************

FUNCTION lfAccum

laAvgCost[1] = laAvgCost[1] + StyInvJl.nStkVal
laAvgCost[2] = laAvgCost[2] + StyInvJl.nTotStk
lnAvgCost = IIF(ASCAN(laAvgCost,0)>0,lnAvgCost,laAvgCost[1]/laAvgCost[2])