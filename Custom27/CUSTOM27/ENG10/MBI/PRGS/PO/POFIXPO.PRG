*:************************************************************************
*: Program file  : POFIXPO 
*: Program desc. : Fix the cancel quantity in the POSHDR for the corrupted closed POs
*:                 and missing date in the receive to cancel record.
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : Khalid Mohi El-Din
*: Date          : 07/30/2002
*: Reference     : B606318
*:************************************************************************

IF !gfSetup()
  RETURN
ENDIF

SELECT PosLn
SET ORDER TO PosLn

SELECT POSHDR
SET ORDER TO PosHdr

SEEK 'P'
SCAN REST WHILE cStyType+PO = "P" FOR Status = "S"
  WAIT WINDOW "PO# " + PO NOWAIT
  lnCanQty = 0
  
  IF SEEK("P"+PosHdr.PO,"PosLn")
    SELECT PosLn    
        
    *-- Check if this is a corrupted PO
    SEEK "P"+PosHdr.PO
    LOCATE REST WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+PosHdr.PO;
                FOR TranCd = "5" AND EMPTY(Date)
    
    IF FOUND()
      
      DO WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+PosHdr.PO
      
        lcStyle = Style
        
        *-- Check if there is a corrupted style in this PO
        LOCATE REST WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+PosHdr.PO+lcStyle ;
                FOR TranCd = "5" AND EMPTY(Date)
        ldLastDate = {}
        
        FOR lnCnt = 1 TO 8
          lcCnt = STR(lnCnt,1)
          lnQty&lcCnt = 0
        ENDFOR
        
        IF FOUND()
          SEEK "P"+PosHdr.PO+lcStyle
          SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+PosHdr.PO+lcStyle
               
            ldLastDate = MAX(ldLastDate,Date)
            DO CASE
              CASE TranCd = "1"
                FOR lnCntr = 1 TO 8
                  lcCntr = STR(lnCntr,1)
                  lnQty&lcCntr = lnQty&lcCntr + Qty&lcCntr
                ENDFOR
              CASE TranCd $ "245" AND !EMPTY(Date)
                FOR lnCntr = 1 TO 8
                  lcCntr = STR(lnCntr,1)
                  lnQty&lcCntr = MAX(lnQty&lcCntr - Qty&lcCntr,0)
                ENDFOR
            ENDCASE            
            IF TranCd = "5" AND EMPTY(Date)
              
              FOR lnCntrX = 1 TO 8
                lcCntrX = STR(lnCntrX,1)
                lnQty&lcCntrX = MAX(lnQty&lcCntrX,0)
              ENDFOR

              IF lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8 > 0
                  REPLACE Date      WITH ldLastDate,;
                          dPostDate WITH ldLastDate,;
                          Qty1      WITH lnQty1    ,;
                          Qty2      WITH lnQty2    ,;
                          Qty3      WITH lnQty3    ,;
                          Qty4      WITH lnQty4    ,;
                          Qty5      WITH lnQty5    ,;
                          Qty6      WITH lnQty6    ,;
                          Qty7      WITH lnQty7    ,;
                          Qty8      WITH lnQty8    ,;
                          TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
              ENDIF
            ENDIF                      
          ENDSCAN
        ENDIF
      ENDDO

      SELECT PosLn
      SEEK "P"+PosHdr.PO
      
      DELETE REST WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+PosHdr.PO;
                      FOR TranCd = "5" AND TotQty = 0
      
      SEEK "P"+PosHdr.PO                
      SUM REST TotQty WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+PosHdr.PO;
                      FOR TranCd = "5" TO lnCanQty
      
      SELECT PosHdr
      REPLACE Cancel WITH lnCanQty
  
    ENDIF  
  ENDIF
ENDSCAN
WAIT CLEAR
= gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Fixing closed POs has been successfully done.')