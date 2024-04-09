*:************************************************************************
*: PROGRAM   : SOORCNVA.PRG      (C102153)
*: DESC.     : Order confirmation Form (VA)
*: System    : Aria Apparel System    (A27)
*: Module    : SALES ORDER  (SO)
*: Developer : Ahmed Mohamed Elanwar (AME)
*: DATE      : 04/10/2001
*: REFER TO  : (C102153)
*:************************************************************************
*: Calls : 
*:         FUNCTION  : None.
*:         --------------------------------------------------------------
*:         PROCEDURE : None.
*:************************************************************************
*: Example : DO SOOECNVA
*:************************************************************************
*: This Program is due to C102153 ...
*:************************************************************************
*:Modifications:
*:************************************************************************
IF llSPack  && if print styles Pack
     *-- Remove all lines (but one) within the same order which have the same pack id
     *-- and add all their qtys to the remaine line.
  SELECT ORDHDR
  SET SKIP TO
  LOCATE
  SCAN 
    lcOrdKey = CordType+Order
    DIMENSION laPack[1],laQtyPack[1,11] && Array to hold lines info.
    STORE SPACE(1) TO laPack
    STORE 0 TO laQtyPack,lnPackNo
    *-- laPack[x] takes the pack_id itself
    *-- laQtyPack[x,1] --> laQtyPack[x,8]   Takes sizes qty from 1 to 8
    *-- laQtyPack[x,9]                      Takes TotQty
    *-- laQtyPack[x,10]                     Takes TotAmount
    *-- laQtyPack[x,11]                     Takes Rcord no. of the remaining line.
    *-- lnPackNo                            The No of pack_id's in this order
    SELECT &lcTempOrd
    SCAN WHILE CORDTYPE + ORDER + STORE + STYLE = lcOrdKey
      IF EMPTY(PACK_ID)
        LOOP
      ELSE
        IF ASCAN(laPack,Pack_ID) # 0   && if this pack_id exists before
          laQtyPack[ASCAN(laPack,Pack_ID),9] =  laQtyPack[ASCAN(laPack,Pack_ID),9] + TotQty
          laQtyPack[ASCAN(laPack,Pack_ID),10] =  laQtyPack[ASCAN(laPack,Pack_ID),10] + TotQty*Price
          FOR I = 1 TO 8
            Z = STR(I,1)
            laQtyPack[ASCAN(laPack,Pack_ID),I] = laQtyPack[ASCAN(laPack,Pack_ID),I] + Qty&Z
          ENDFOR
          DELETE
        ELSE
          lnPackNo = lnPackNo +1
          DIMENSION laPack[lnPackNo],laQtyPack[lnPackNo,11]
          laPack[lnPackNo] = PACK_ID
          laQtyPack[lnPackNo,9] = TotQty
          laQtyPack[lnPackNo,10] = TotQty*Price
          laQtyPack[lnPackNo,11] = RECNO()
          FOR I = 1 TO 8
            Z = STR(I,1)
            laQtyPack[lnPackNo,I] = Qty&Z
          ENDFOR
        ENDIF
      ENDIF
    ENDSCAN
    IF lnPackNo # 0  && if there's any pack_id 's at all.
      SELECT &lcTempOrd
      FOR I = 1 TO lnPackNo       && loop the no. of pack_id 's
        IF BETWEEN(laQtyPack[I,11],1,RECCOUNT())
          GO laQtyPack[I,11]                  && go to the remaining line of pack_id
          REPLACE TOTQTY WITH laQtyPack[I,9]  && the total qty for all lines of that pack_id
          REPLACE PRICE WITH laQtyPack[I,10]/laQtyPack[I,9]  && the Average price
          REPLACE ALLOCATED WITH laQtyPack[I,10]  && store Amount to allocated field
          FOR J = 1 TO 8
            X  = STR(J,1)
            REPLACE QTY&X WITH laQtyPack[I,J]
          ENDFOR
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
  SET SKIP TO &lcTempOrd
  LOCATE
ENDIF