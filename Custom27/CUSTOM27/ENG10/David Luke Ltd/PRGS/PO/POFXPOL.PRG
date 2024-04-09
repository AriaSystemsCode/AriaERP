*:--------------------------------------------------------------------------
*: Program file        : POFXPOL.PRG
*: Program description : Fix PO lines by adding cancelled records for POs that do not have
*:                       have it. Because of partially receiving to stock then cancel the PO to 
*:                       complete it.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Style Purchase Order - (PO)
*: Developer Name      : Khalid Mohi El-Din Mohamed - (KHM)
*: Tracking Job Number : B607149
*: Example             : DO SMFIXGL.PRG
*:--------------------------------------------------------------------------

IF !gfSetUp()
  RETURN
ENDIF

SELECT POSHDR
SEEK "P"
SCAN REST WHILE cStyType+PO = "P" FOR Status = 'C' AND Cancel > 0
  WAIT WINDOW "PO # " + PO NOWAIT
  
  IF SEEK("P"+PosHdr.PO,'PosLn')
    SELECT PosLn
     
    DO WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = "P"+PosHdr.PO
      SCATTER MEMVAR MEMO      
      STORE 0 TO lnQty1, lnQty2, lnQty3, lnQty4, lnQty5, lnQty6, lnQty7, lnQty8
      lcKey = cStyType+PO+Style+STR(LineNo,6)

      SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = lcKey
        IF TranCd = '1'
          FOR lnCtr = 1 TO 8
            lcCtr = STR(lnCtr,1)
            lnQty&lcCtr = lnQty&lcCtr + Qty&lcCtr
          ENDFOR
        ELSE
          IF TranCd $ '245'
            FOR lnCtr = 1 TO 8
              lcCtr = STR(lnCtr,1)
              lnQty&lcCtr = MAX(lnQty&lcCtr - Qty&lcCtr, 0)
            ENDFOR
            m.Date      = Date
            m.cRSession = cRSession
            FOR lnX = 1 TO 5
              lcX = STR(lnX,1)
              m.nLan_Cst&lcX  = nLan_Cst&lcX
              m.nELanCost&lcX = nELanCost&lcX
            ENDFOR  
          ENDIF
        ENDIF
      ENDSCAN
      
      IF lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8 > 0
        m.TranCd = '5'
        m.Reference = "Fix I"
        SELECT PosLn
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE Qty1   WITH lnQty1 ,;
                Qty2   WITH lnQty2 ,;
                Qty3   WITH lnQty3 ,;
                Qty4   WITH lnQty4 ,;
                Qty5   WITH lnQty5 ,;
                Qty6   WITH lnQty6 ,;
                Qty7   WITH lnQty7 ,;
                Qty8   WITH lnQty8 ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      ENDIF
    ENDDO
  ENDIF
ENDSCAN
WAIT CLEAR
=gfModalGen('QRM00000B00000','ALERT','','','Adding missing transactions has been done successfully.')