STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin

IF !gfSetup()
  RETURN
ENDIF
lcPExSign = gfGetExSin(@lcPUntSin,POSHDR.cPriceCur)
=gfOpenFile(gcDataDir+'CutTktH','CutTktH','SH') 
=gfOpenFile(gcDataDir+'CutTktL','CutTktL','SH') 

=gfOpenFile(gcDataDir+'POSLN','POSLN','SH') 
=gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH') 
SET ORDER TO TAG BOMLINE IN BOMLINE

*-- Fixing the Estimated costs in the lines.
SELECT PosHdr
SEEK "P"
SCAN REST WHILE cStyType+PO = "P"
  WAIT WINDOW "Fixing estimated cost for PO # " + PO NOWAIT
  IF SEEK('I'+'1'+PosHdr.PO,'BOMLINE')
  
     *-- Replace the Estimated and Equ. Estimated with 0
     SELECT PosLn
     =SEEK('P'+PosHdr.PO)
      REPLACE REST nCost1  WITH 0,nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
                   nCost5  WITH 0,nECost1 WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
                   nECost4 WITH 0,nECost5 WITH 0;
      WHILE cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = 'P'+PosHdr.PO

    *-- Get the costs from the BomLine
    SELECT BOMLINE
    lcBomLnKey = EVALUATE(Key())
    DO WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
             'I'+'1'+PosHdr.PO FOR !lVoid
      SCATTER MEMVAR
      STORE 0 TO lnECost, lnTransQty
      lnBomLLine = LineNo
      lcBomLType = cBomTyp
      lcBomStyle = Style
      lcBomSClr  = SClr
      lcBomItem  = Item
      lcBomIClr  = IClr
      lcBomMCode = MfgCode
      lcBomCatgT = cCatgTyp
      lcBomLnKey = EVALUATE(Key())

      SELECT PosLn
      IF SEEK('P'+m.CTktNo+m.Style+STR(m.LineNo,6))
        SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = ;
                        'P'+m.CTktNo+m.Style+STR(m.LineNo,6)
          lnEstCost  = 0
          SELECT BOMLINE
          SEEK lcBomLnKey
          SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+;
                          Item+IClr+MfgCode=;
                          'I'+'1'+PosHdr.PO+STR(lnBomLLine,6)+lcBomLType+;
                          lcBomStyle+lcBomSClr
             lcBomSizes = IIF(EMPTY(cSizes),'12345678',cSizes)
       
             lnTransQty  = 0
             FOR lnCntr = 1 TO 8
               lcCntr   = STR(lnCntr,1)
               IF lcCntr $ lcBomSizes
                   lnTransQty = lnTransQty + PosLn.Qty&lcCntr          
               ENDIF
             ENDFOR
       
             lnEstCost  = lnEstCost + ((UnitQty * UnitCost) * lnTransQty)
          ENDSCAN
          lnEstCost = IIF(PosLn.TotQty <> 0,lnEstCost / PosLn.TotQty,0)
    
          DO CASE
            CASE lcBomCatgT = 'P'
              lnECost = (lnEstCost &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
            CASE !INLIST(lcBomCatgT ,'S','F','T')
              lnECost = (lnEstCost &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
            OTHERWISE
              lnECost  = lnEstCost
          ENDCASE   
          SELECT PosLn
          REPLACE ('nCost'+lcBomLType) WITH lnEstCost ,;
                  ('nECost'+m.cBomTyp) WITH lnECost
          IF lcBomCatgT = 'P'
            IF nCost1 > Gros_Price
              REPLACE Gros_Price WITH nCost1 ; 
                      Disc_Pcnt  WITH 0
            ELSE
              REPLACE Disc_Pcnt  WITH IIF(Gros_Price = 0 , 0 , 100 - nCost1 * 100 / Gros_Price)
            ENDIF
          ENDIF
      ENDSCAN
    ELSE
      SELECT BomLine
      IF !EOF()
        SKIP
      ENDIF
    ENDIF  
    SELECT BomLine
  ENDDO
  SEEK lcBomLnKey
ENDIF
ENDSCAN
WAIT CLEAR

*-- Fixing the Estimated costs in the header.
*-- Fixing the Landed costs in the lines and header.
SELECT PosHdr
SEEK "P"
SCAN REST WHILE cStyType+PO = "P"
  WAIT WINDOW "Fixing landed costs for PO # " + PO NOWAIT

  STORE 0 TO lnEstCost1,lnEstCost2,lnEstCost3,lnEstCost4,lnEstCost5,;
             lnLndCost1,lnLndCost2,lnLndCost3,lnLndCost4,lnLndCost5

  IF SEEK('P'+PosHdr.PO,'PosLn')
    SELECT PosLn
    SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = 'P'+PosHdr.PO
      FOR lnCountr = 1 TO 5
        lcCountr = STR(lnCountr,1)
        REPLACE nECost&lcCountr WITH nCost&lcCountr
      ENDFOR
      DO CASE
        CASE TranCd = "1"
          FOR lnECounter = 1 TO 5
            lcECounter = STR(lnECounter,1)
            lnEstCost&lcECounter = lnEstCost&lcECounter + (nCost&lcECounter*TotQty)
          ENDFOR
        CASE TranCd $ "245"
          FOR lnLCounter = 1 TO 5          
            lcLCounter = STR(lnLCounter,1)
            REPLACE nELanCost&lcLCounter WITH nLan_Cst&lcLCounter
            lnLndCost&lcLCounter = lnLndCost&lcLCounter + (nELanCost&lcLCounter*TotQty)
          ENDFOR        
      ENDCASE
    ENDSCAN
    SELECT PosHdr
    FOR lnCntr = 1 TO 5
      lcCntr = STR(lnCntr,1)
      REPLACE nICost&lcCntr    WITH lnEstCost&lcCntr ,;
              nFCost&lcCntr    WITH lnEstCost&lcCntr ,;
              nLan_Cost&lcCntr WITH lnLndCost&lcCntr ,;
              nFLanCost&lcCntr WITH lnLndCost&lcCntr      
    ENDFOR  
  ENDIF
ENDSCAN
WAIT CLEAR

*-------------------- Cutting Ticket ----------------
SELECT CutTktH
SCAN
  WAIT WINDOW "Fixing estimated cost for CT # " + CutTkt NOWAIT
  IF SEEK('M'+'1'+CutTktH.CutTkt,'BOMLINE')
    SELECT CutTktL
    SET ORDER TO TAG CutTktL
    =SEEK(CutTktH.CutTkt)
    REPLACE REST nCost1 WITH 0,nCost2 WITH 0,nCost3 WITH 0,nCost4 WITH 0,nCost5 WITH 0;
    WHILE cuttkt+style+dyelot+trancd+STR(RECNO(),7) = CutTktH.CutTkt
    
    SELECT BOMLINE
    lcBomLnKey = EVALUATE(Key())
    DO WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
             'M'+'1'+CutTktH.CutTkt FOR !lVoid
      SCATTER MEMVAR
      lnECost    = 0
      lnTransQty  = 0
      lnBomLLine = LineNo
      lcBomLType = cBomTyp
      lcBomStyle = Style
      lcBomSClr  = SClr
      lcBomItem  = Item
      lcBomIClr  = IClr
      lcBomMCode = MfgCode
      lcBomCatgT = cCatgTyp
      lcBomLnKey = EVALUATE(Key())

      SELECT CUTTKTL
      IF SEEK(m.CTktNo+m.Style)
        SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = m.CTktNo+m.Style
          lnEstCost  = 0
          SELECT BOMLINE
          SEEK lcBomLnKey
          SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+;
                          Item+IClr+MfgCode=;
                          'M'+'1'+CutTktH.CutTkt+STR(lnBomLLine,6)+lcBomLType+;
                          lcBomStyle+lcBomSClr
             lcBomSizes = IIF(EMPTY(cSizes),'12345678',cSizes)
       
             lnTransQty  = 0
             FOR lnCntr = 1 TO 8
               lcCntr   = STR(lnCntr,1)
               IF lcCntr $ lcBomSizes
                 lnTransQty = lnTransQty + CutTktL.Qty&lcCntr
               ENDIF
             ENDFOR
       
             lnEstCost  = lnEstCost + ((UnitQty * UnitCost) * lnTransQty)

          ENDSCAN
          lnEstCost = IIF(CutTktL.TotQty <> 0 ,lnEstCost / CutTktL.TotQty,0)
          SELECT CutTktL
          REPLACE ('nCost'+m.cBomTyp) WITH lnEstCost
        ENDSCAN
      ELSE  
        SELECT BomLine
        IF !EOF()
          SKIP
        ENDIF
      ENDIF  
        SELECT BomLine
    ENDDO
    SEEK lcBomLnKey
  ENDIF
ENDSCAN  

SELECT CutTktH
SCAN 
  WAIT WINDOW "Fixing Estimated and landed costs for CT # " + CutTkt NOWAIT

  STORE 0 TO lnEstCost1,lnEstCost2,lnEstCost3,lnEstCost4,lnEstCost5,;
             lnLndCost1,lnLndCost2,lnLndCost3,lnLndCost4,lnLndCost5

  IF SEEK(CutTktH.CutTkt,'CutTktL')
    SELECT CutTktl
    SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = CutTktH.CutTkt
      DO CASE
        CASE TranCd = "1"
          FOR lnECounter = 1 TO 5
            lcECounter = STR(lnECounter,1)
            lnEstCost&lcECounter = lnEstCost&lcECounter + (nCost&lcECounter*TotQty)
          ENDFOR
        CASE TranCd $ "245"
          FOR lnLCounter = 1 TO 5          
            lcLCounter = STR(lnLCounter,1)
            lnLndCost&lcLCounter = lnLndCost&lcLCounter + (nLan_Cst&lcLCounter*TotQty)
          ENDFOR        
      ENDCASE
    ENDSCAN
    SELECT CutTktH
    FOR lnCntr = 1 TO 5
      lcCntr = STR(lnCntr,1)
      REPLACE nEst_Cost&lcCntr WITH lnEstCost&lcCntr ,;
              nLan_Cost&lcCntr WITH lnLndCost&lcCntr
    ENDFOR  
  ENDIF
ENDSCAN
WAIT CLEAR
=lfUpdCtPo()

*--- SSH:
FUNCTION lfUpdCtPo

*---- Start Fix Cuttkts
USE gcWorkDir+"lsttes2" IN 0
SELECT CUTTKTH
SET ORDER TO 1
SELECT lsttes2
SCAN
  SELECT CUTTKTH
  IF SEEK(lsttes2.PO) .AND. Status = "S"
    WAIT WINDOW "Fix CutTkt# "+CutTkth.CutTkt NOWAIT
    STORE 0 TO lnLActCst1 , lnLActCst2, lnLActCst3, lnLActCst4, lnLActCst5
    IF CUTTKTH.Pcs_Rec<>0
      lnLActCst1 = CUTTKTH.nAct_Cost1/CUTTKTH.Pcs_Rec
      lnLActCst2 = CUTTKTH.nAct_Cost2/CUTTKTH.Pcs_Rec
      lnLActCst3 = CUTTKTH.nAct_Cost3/CUTTKTH.Pcs_Rec
      lnLActCst4 = CUTTKTH.nAct_Cost4/CUTTKTH.Pcs_Rec
      lnLActCst5 = CUTTKTH.nAct_Cost5/CUTTKTH.Pcs_Rec
    ENDIF
    SELECT CUTTKTL
    REPLACE nAct_Cst1 WITH lnLActCst1,;
            nAct_Cst2 WITH lnLActCst2,;
            nAct_Cst3 WITH lnLActCst3,;
            nAct_Cst4 WITH lnLActCst4,;
            nAct_Cst5 WITH lnLActCst5 ;
         FOR cuttkt+style+dyelot+trancd = CutTkth.CutTkt .AND. trancd='2'
  ENDIF
ENDSCAN
SELECT POSLN
SCAN
  WAIT WINDOW "Fix PO# "+POSLN.PO NOWAIT  
  REPLACE neactcost1 WITH nAct_Cst1,;
          neactcost2 WITH nAct_Cst2,;
          neactcost3 WITH nAct_Cst3,;
          neactcost4 WITH nAct_Cst4,;
          neactcost5 WITH nAct_Cst5
ENDSCAN
SELECT POSHDR
SCAN
  WAIT WINDOW "Fix PO# "+POSHDR.PO NOWAIT  
  REPLACE nfactcost1 WITH nAct_Cost1,;
          nfactcost2 WITH nAct_Cost2,;
          nfactcost3 WITH nAct_Cost3,;
          nfactcost4 WITH nAct_Cost4,;
          nfactcost5 WITH nAct_Cost5
ENDSCAN
= gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Fixing estimated ,landed and actual costs have been done successfully.')
USE IN lsttes2