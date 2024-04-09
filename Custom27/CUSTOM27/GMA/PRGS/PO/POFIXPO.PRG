*:***************************************************************************
*: Program file  : POFIXPO                        (B604790)
*: Program desc. : Fix the estmated costs in the POSLN file for GMA.
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Purchase Order (PO)
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************

IF !gfSetup()
  RETURN
ENDIF

SELECT POSHDR
SEEK 'P'
SCAN REST WHILE cStyType+Po = 'P'
  WAIT WINDOW 'Fixing Purchase Order # : '+ALLTRIM(PO) NOWAIT
  IF SEEK('P'+PO,'POSLN')
    SELECT POSLN
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+POSHDR.PO FOR TRANCD = '1'
      lcCurKey = EVALUATE(KEY())
      SCATTER FIELDS NCOST1,NCOST2,NCOST3,NCOST4,NCOST5,;
                     NECOST1,NECOST2,NECOST3,NECOST4,NECOST5,STYLE,LINENO MEMVAR
      SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd =;
                      'P'+POSHDR.PO+m.Style+STR(m.LineNo,6) FOR TRANCD <> '1'
        IF TranCd = '3' AND TotQty <> Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ENDIF              

        IF NCOST1 <> m.NCost1 .OR. NCOST2 <> m.NCost2 .OR. NCOST3 <> m.NCost3 .OR. ;
           NCOST4 <> m.NCost4 .OR. NCOST5 <> m.NCost5
          GATHER FIELDS NCOST1,NCOST2,NCOST3,NCOST4,NCOST5,;
                        NECOST1,NECOST2,NECOST3,NECOST4,NECOST5 MEMVAR
        ENDIF
      ENDSCAN
      SEEK lcCurKey
    ENDSCAN
  ENDIF
ENDSCAN
WAIT CLEAR
= gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Fixing estimated costs has been done successfully.')