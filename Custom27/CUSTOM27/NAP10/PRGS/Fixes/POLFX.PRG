SELECT testcst
ZAP

*---cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
SELECT CUTTKTL
SET ORDER TO 1
SET FILTER TO TRANCD='2'
GO TOP
DO WHILE !EOF()
  lcTmpPo = CUTTKT
  STORE 0 TO  ln1 ,ln2 , ln3,ln4, ln5,lnn1, lnn2, lnn3,lnn4,lnn5
  SCAN REST WHILE CUTTKT=lcTmpPo 
    ln1 = ln1 + nAct_Cst1*TotQty
    ln2 = ln2 + nAct_Cst2*TotQty
    ln3 = ln3 + nAct_Cst3*TotQty
    ln4 = ln4 + nAct_Cst4*TotQty
    ln5 = ln5 + nAct_Cst5*TotQty
  ENDSCAN

  SELECT BOMCOST
  SUM nTotCst TO lnn1 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "1M"+lcTmpPo
  
  SUM nTotCst TO lnn2 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "2M"+lcTmpPo
  SUM nTotCst TO lnn3 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "3M"+lcTmpPo
  SUM nTotCst TO lnn4 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "4M"+lcTmpPo
  SUM nTotCst TO lnn5 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "5M"+lcTmpPo
  IF ABS(lnn1-ln1) > 0.9 
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH lcTmpPo,;
            TYPE     WITH '1',;
            lndiff   WITH lnn1-ln1,;
            posln    WITH ln1,;
            Bomcost  WITH lnn1
    WAIT WINDOW "SELECT COST# "+TYPE+" For PO: "+lcTmpPo NOWAIT
  ENDIF

  IF ABS(lnn2-ln2) > 0.9 
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH lcTmpPo,;
            TYPE     WITH '2',;
            lndiff   WITH lnn2-ln2,;
            posln    WITH ln2,;
            Bomcost  WITH lnn2
    WAIT WINDOW "SELECT COST# "+TYPE+" For PO: "+lcTmpPo NOWAIT
  ENDIF
  
  IF ABS(lnn3-ln3) > 0.9 
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH lcTmpPo,;
            TYPE     WITH '3',;
            lndiff   WITH lnn3-ln3,;
            posln    WITH ln3,;
            Bomcost  WITH lnn3
    WAIT WINDOW "SELECT COST# "+TYPE+" For PO: "+lcTmpPo NOWAIT
  ENDIF
  
  IF ABS(lnn4-ln4) > 0.9 
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH lcTmpPo,;
            TYPE     WITH '4',;
            lndiff   WITH lnn4-ln4,;
            posln    WITH ln4,;
            Bomcost  WITH lnn4
    WAIT WINDOW "SELECT COST# "+TYPE+" For PO: "+lcTmpPo NOWAIT
  ENDIF

  IF ABS(lnn5-ln5) > 0.9 
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH lcTmpPo,;
            TYPE     WITH '5',;
            lndiff   WITH lnn5-ln5,;
            posln    WITH ln5,;
            Bomcost  WITH lnn5
    WAIT WINDOW "SELECT COST# "+TYPE+" For PO: "+lcTmpPo NOWAIT
  ENDIF
  SELECT CUTTKTL
ENDDO
SELECT CUTTKTL
SET ORDER TO Cutrec
SELECT TESTCST
GO TOP
SELECT TESTCST
SELECT TESTCST
DELETE ALL FOR !(type$'124')
PACK
WAIT WINDOW STR(RECCOUNT()) +" PO Need to be fixed... Press any key to continuo."
SET RELATION TO TESTCST.PO INTO CUTTKTH
SET FILTER TO CUTTKTH.STATUS = "S"
GO TOP

return
SCAN FOR type$'35'
  lcTmpPo   = PO
  lnPOLNTot = 0
  lnCstTyp  = TESTCST.Type
  SELECT BOMCOST

  SELECT ctktno ,cbomtype, crsession ,SUM(nTotCst) AS SMTOTCST FROM BOMCOST;
       WHERE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
             lnCstTyp+"M"+lcTmpPo .AND. !EMPTY(crsession) .AND. SEEK(crsession+cTktno,'CTL');
       GROUP BY ctktno,crsession INTO DBF TMPBOM.DBF

  SELECT TMPBOM
  SCAN
    lcTmpResSess = crsession
    lnTotActCst  = SMTOTCST
    IF lnTotActCst <> 0 and !EMPTY(crsession) and SEEK(crsession,'CUTTKTL')
      
      SELECT CUTTKTL
      SUM TOTQTY TO lnPOLNTot FOR crsession+cuttkt+style+trancd=;
                                 lcTmpResSess+lcTmpPo .AND. TRANCD='2'
      lnUnitCost = lnTotActCst/lnPOLNTot

      REPLACE ALL CUTTKTL.NACT_CST&lnCstTyp  WITH lnUnitCost;
        FOR crsession+cuttkt+style+trancd=;
                                 lcTmpResSess+lcTmpPo .AND. TRANCD='2'
    ELSE
      IF !SEEK(crsession,'CUTTKTL')
        WAIT WINDOW "Not found Recieve Session in CutTktL"
      ENDIF
      WAIT WINDOW "Error Fixing PO# "+lcTmpPo
      WAIT WINDOW "ZERO Total Actual Cost. Nothing will be fixed"
    ENDIF
  ENDSCAN
  USE IN TMPBOM
  ERASE TMPBOM.DBF
ENDSCAN


