*---cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
SELECT BOMCOST
SET ORDER TO Bomcstkt
SELECT POSLN
SET ORDER TO 1
SET FILTER TO TRANCD='2'
GO TOP
SELECT POSHDR
SCAN FOR STATUS<>"X"  and !EMPTY(PO) and entered >= {07/01/2000}

  STORE 0 TO  ln1 ,ln2 , ln3,ln4, lnn1, lnn2, lnn3,lnn4
  SELECT POSLN
  IF SEEK("P"+POSHDR.PO)
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd=;
                    "P"+POSHDR.PO;
              FOR TRANCD='2'
      ln1 = ln1 + nAct_Cst1*TotQty
      ln2 = ln2 + nAct_Cst2*TotQty
      ln3 = ln3 + nAct_Cst3*TotQty
      ln4 = ln4 + nAct_Cst4*TotQty
    ENDSCAN
  ENDIF
*---cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
  SELECT BOMCOST
  SUM ntotcst TO lnn1 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "1I"+POSHDR.PO
  
  SUM ntotcst TO lnn2 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "2I"+POSHDR.PO
  SUM ntotcst TO lnn3 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "3I"+POSHDR.PO
  SUM ntotcst TO lnn4 FOR cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          "4I"+POSHDR.PO

  IF ABS(POSHDR.NACT_Cost1-ln1) > 0.9 .OR. ABS(POSHDR.NACT_Cost1-lnn1) > 0.9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH POSHDR.PO,;
            TYPE     WITH '1',;
            lndiff   WITH ABS(POSHDR.NACT_Cost1-ln1),;
            bocstdif WITH ABS(POSHDR.NACT_Cost1-lnn1),;
            posln    WITH ln1,;
            poshdr   WITH POSHDR.NACT_Cost1  ,;
            Bomcost  WITH lnn1
  ENDIF


  IF ABS(POSHDR.NACT_Cost2-ln2) > 0.9 .OR. ABS(POSHDR.NACT_Cost2-lnn2) > 0.9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH POSHDR.PO,;
            TYPE     WITH '2',;
            lndiff   WITH ABS(POSHDR.NACT_Cost2-ln2),;
            bocstdif WITH ABS(POSHDR.NACT_Cost2-lnn2),;
            posln    WITH ln2,;
            Bomcost  WITH lnn2,;
            poshdr   WITH POSHDR.NACT_Cost2  
            
  ENDIF


  IF ABS(POSHDR.NACT_Cost3-ln3) > 0.9 .OR. ABS(POSHDR.NACT_Cost3-lnn3) > 0.9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH POSHDR.PO,;
            TYPE     WITH '3',;
            lndiff   WITH ABS(POSHDR.NACT_Cost3-ln3),;
            bocstdif WITH ABS(POSHDR.NACT_Cost3-lnn3),;
            posln    WITH ln3,;
            Bomcost  WITH lnn3,;
            poshdr   WITH POSHDR.NACT_Cost3  
  ENDIF


  IF ABS(POSHDR.NACT_Cost4-ln4) > 0.9 .OR. ABS(POSHDR.NACT_Cost4-lnn4) > 0.9
    SELECT TESTCST
    APPEND BLANK
    REPLACE PO       WITH POSHDR.PO,;
            TYPE     WITH '4',;
            lndiff   WITH ABS(POSHDR.NACT_Cost4-ln4),;
            bocstdif WITH ABS(POSHDR.NACT_Cost4-lnn4),;
            posln    WITH ln4,;
            Bomcost  WITH lnn4,;
            poshdr   WITH POSHDR.NACT_Cost4  
  ENDIF

ENDSCAN



*  IF ABS(POSHDR.NACT_Cost1-lnn1) > 0.9
*    SELECT TESTCST
*    APPEND BLANK
*    REPLACE PO       WITH POSHDR.PO,;
*            TYPE     WITH '1',;
*            lndiff   WITH 0,;
*            bocstdif WITH ABS(POSHDR.NACT_Cost1-lnn1),;
*            Bomcost    WITH lnn1,;
*            poshdr   WITH POSHDR.NACT_Cost1  
*  ENDIF


*  IF ABS(POSHDR.NACT_Cost2-lnn2) > 0.9
*    SELECT TESTCST
*    APPEND BLANK
*    REPLACE PO       WITH POSHDR.PO,;
*            TYPE     WITH '2',;
*            lndiff   WITH 0,;
*            bocstdif WITH ABS(POSHDR.NACT_Cost2-lnn2),;
*            Bomcost    WITH lnn2,;
*            poshdr   WITH POSHDR.NACT_Cost2  
*  ENDIF

  *IF ABS(POSHDR.NACT_Cost3-lnn3) > 0.9
  *  SELECT TESTCST
  *  APPEND BLANK
  *  REPLACE PO       WITH POSHDR.PO,;
  *          TYPE     WITH '3',;
  *          lndiff   WITH 0,;
  *          bocstdif WITH ABS(POSHDR.NACT_Cost3-lnn3),;
  *          Bomcost    WITH lnn3,;
  *          poshdr   WITH POSHDR.NACT_Cost3  
  *ENDIF

*  IF ABS(POSHDR.NACT_Cost4-lnn4) > 0.9
*    SELECT TESTCST
*    APPEND BLANK
*    REPLACE PO       WITH POSHDR.PO,;
*            TYPE     WITH '4',;
*            lndiff   WITH 0,;
*            bocstdif WITH ABS(POSHDR.NACT_Cost4-lnn4),;
*            Bomcost    WITH lnn4,;
*            poshdr   WITH POSHDR.NACT_Cost4  
*  ENDIF
