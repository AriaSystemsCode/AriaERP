SELECT TESTCST
SCAN for type$"124"
  IF SEEK(PO,"CUTTKTH") .AND. cuttkth.status='S'
    WAIT WINDOW "FIX CUTTKT" + PO NOWAIT
    lcTmpCst = type
    SELECT CUTTKTH
    lnLneCst = NAct_Cost&lcTmpCst/Pcs_Rec
    SELECT CUTTKTL
    REPLACE ALL nAct_Cst&lcTmpCst. WITH lnLneCst;
            FOR CUTTKT = CUTTKTH.CUTTKT .AND. TRANCD="2"
  ENDIF
ENDSCAN