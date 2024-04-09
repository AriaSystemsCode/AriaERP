    lcMastPOLn = This.cPosLn
    SCAN
      IF TotQty=0  AND SEEK(cBusDocu+cStyType+PO+Style+STR(LineNo,6), lcMastPOLn)
        REPLACE Qty1 WITH &lcMastPOLn..Qty1 ,;
                Qty2 WITH &lcMastPOLn..Qty2 ,;
                Qty3 WITH &lcMastPOLn..Qty3 ,;
                Qty4 WITH &lcMastPOLn..Qty4 ,;
                Qty5 WITH &lcMastPOLn..Qty5 ,;
                Qty6 WITH &lcMastPOLn..Qty6 ,;
                Qty7 WITH &lcMastPOLn..Qty7 ,;
                Qty8 WITH &lcMastPOLn..Qty8 ,;
                TotQty WITH &lcMastPOLn..TotQty 
        DELETE
      ENDIF
    ENDSCAN
