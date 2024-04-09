*---cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
SELECT BOMCOST
SET ORDER TO Bomcstkt
SELECT CUTTKTL
SET ORDER TO 1
SET FILTER TO TRANCD='2'
GO TOP
SELECT CUTTKTH
SCAN FOR STATUS<>"X"
  STORE 0 TO  ln1 ,ln2 , ln3,ln4, ln5,lnn1, lnn2, lnn3,lnn4,lnn5
  SELECT CUTTKTL
  IF SEEK(CUTTKTH.CUTTKT)
    SCAN REST WHILE CUTTKT=;
                    CUTTKTH.CUTTKT;
              FOR TRANCD='2'
      ln1 = ln1 + nAct_Cst1*TotQty
      ln2 = ln2 + nAct_Cst2*TotQty
      ln3 = ln3 + nAct_Cst3*TotQty
      ln4 = ln4 + nAct_Cst4*TotQty
      ln5 = ln5 + nAct_Cst5*TotQty
    ENDSCAN
  ENDIF
  
*---cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
  SELECT BOMCOST
  SUM nTotCst TO lnn1 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "1M"+CUTTKTH.CUTTKT
  
  SUM nTotCst TO lnn2 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "2M"+CUTTKTH.CUTTKT
  SUM nTotCst TO lnn3 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "3M"+CUTTKTH.CUTTKT
  SUM nTotCst TO lnn4 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "4M"+CUTTKTH.CUTTKT

  SUM nTotCst TO lnn5 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "5M"+CUTTKTH.CUTTKT

  IF ABS(CUTTKTH.NACT_Cost1-ln1) > .9 .OR. ABS(CUTTKTH.NACT_Cost1-lnn1) > 1.9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH CUTTKTH.CUTTKT,;
            TYPE     WITH '1',;
            lndiff   WITH ABS(CUTTKTH.NACT_Cost1-ln1),;
            bocstdif WITH ABS(CUTTKTH.NACT_Cost1-lnn1),;
            posln    WITH ln1,;
            poshdr   WITH CUTTKTH.NACT_Cost1  ,;
            Bomcost  WITH lnn1
  ENDIF


  IF ABS(CUTTKTH.NACT_Cost2-ln2) > .9 .OR. ABS(CUTTKTH.NACT_Cost2-lnn2) > .9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH CUTTKTH.CUTTKT,;
            TYPE     WITH '2',;
            lndiff   WITH ABS(CUTTKTH.NACT_Cost2-ln2),;
            bocstdif WITH ABS(CUTTKTH.NACT_Cost2-lnn2),;
            posln    WITH ln2,;
            Bomcost  WITH lnn2,;
            poshdr   WITH CUTTKTH.NACT_Cost2  
            
  ENDIF


  IF ABS(CUTTKTH.NACT_Cost3-ln3) > .9 .OR. ABS(CUTTKTH.NACT_Cost3-lnn3) > .9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH CUTTKTH.CUTTKT,;
            TYPE     WITH '3',;
            lndiff   WITH ABS(CUTTKTH.NACT_Cost3-ln3),;
            bocstdif WITH ABS(CUTTKTH.NACT_Cost3-lnn3),;
            posln    WITH ln3,;
            Bomcost  WITH lnn3,;
            poshdr   WITH CUTTKTH.NACT_Cost3  
  ENDIF


  IF ABS(CUTTKTH.NACT_Cost4-ln4) > .9 .OR. ABS(CUTTKTH.NACT_Cost4-lnn4) > .9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH CUTTKTH.CUTTKT,;
            TYPE     WITH '4',;
            lndiff   WITH ABS(CUTTKTH.NACT_Cost4-ln4),;
            bocstdif WITH ABS(CUTTKTH.NACT_Cost4-lnn4),;
            posln    WITH ln4,;
            Bomcost  WITH lnn4,;
            poshdr   WITH CUTTKTH.NACT_Cost4  
  ENDIF

  IF .T. AND (ABS(CUTTKTH.NACT_Cost5-ln5) > .9 .OR. ABS(CUTTKTH.NACT_Cost5-lnn5) > .9)
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH CUTTKTH.CUTTKT,;
            TYPE     WITH '5',;
            lndiff   WITH ABS(CUTTKTH.NACT_Cost5-ln5),;
            bocstdif WITH ABS(CUTTKTH.NACT_Cost5-lnn5),;
            posln    WITH ln5,;
            Bomcost  WITH lnn5,;
            poshdr   WITH CUTTKTH.NACT_Cost5
  ENDIF

ENDSCAN



*  IF ABS(POSHDR.NACT_Cost1-lnn1) > .9
*    SELECT TESTCST
*    APPEND BLANK
*    REPLACE PO       WITH POSHDR.PO,;
*            TYPE     WITH '1',;
*            lndiff   WITH 0,;
*            bocstdif WITH ABS(POSHDR.NACT_Cost1-lnn1),;
*            Bomcost    WITH lnn1,;
*            poshdr   WITH POSHDR.NACT_Cost1  
*  ENDIF


*  IF ABS(POSHDR.NACT_Cost2-lnn2) > .9
*    SELECT TESTCST
*    APPEND BLANK
*    REPLACE PO       WITH POSHDR.PO,;
*            TYPE     WITH '2',;
*            lndiff   WITH 0,;
*            bocstdif WITH ABS(POSHDR.NACT_Cost2-lnn2),;
*            Bomcost    WITH lnn2,;
*            poshdr   WITH POSHDR.NACT_Cost2  
*  ENDIF

  *IF ABS(POSHDR.NACT_Cost3-lnn3) > .9
  *  SELECT TESTCST
  *  APPEND BLANK
  *  REPLACE PO       WITH POSHDR.PO,;
  *          TYPE     WITH '3',;
  *          lndiff   WITH 0,;
  *          bocstdif WITH ABS(POSHDR.NACT_Cost3-lnn3),;
  *          Bomcost    WITH lnn3,;
  *          poshdr   WITH POSHDR.NACT_Cost3  
  *ENDIF

*  IF ABS(POSHDR.NACT_Cost4-lnn4) > .9
*    SELECT TESTCST
*    APPEND BLANK
*    REPLACE PO       WITH POSHDR.PO,;
*            TYPE     WITH '4',;
*            lndiff   WITH 0,;
*            bocstdif WITH ABS(POSHDR.NACT_Cost4-lnn4),;
*            Bomcost    WITH lnn4,;
*            poshdr   WITH POSHDR.NACT_Cost4  
*  ENDIF
