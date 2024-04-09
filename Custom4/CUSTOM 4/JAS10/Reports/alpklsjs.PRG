****************************************************************************
*: Program file  : ALPKLSJS.PRG --- ALPKLSJS.FRX
*: Program desc. : Custom Print Packing List for JASCO (JAS10)
*: System        : Aria Apparel System (Aria4XP).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : Mariam Mazhar Due to issue #C201378.Exe,C201145.122 [T20110802.0046]
*: Date          : 08/29/2011
*:**************************************************************************
IF llOGFltCh
    SELECT (lcPackTmp)
    SCAN 
      SELECT (lcPakLnTmp)
      SCAN FOR PACK_NO =  &lcPackTmp..PACK_NO
        lfSpckln()
        SELECT (lcPakLnTmp)
        REPLACE PACK_ID WITH SUBSTR(laSpckTemp[1] ,7)
      ENDSCAN    
    ENDSCAN 
    IF LCRPSORT ='K'
      SELECT (lcPackTmp)
      SET RELATION OFF INTO (lcPakLnTmp)
      SELECT (lcPakLnTmp)
      INDEX on PACK_NO+PACK_ID TAG 'PACK_ID' ADDITIVE 
      SET ORDER TO 'PACK_ID'
      SELECT (lcPackTmp)
      SET RELATION TO PACK_NO INTO (lcPakLnTmp) ADDITIVE 
    ENDIF
ENDIF
*!**************************************************************************
*! Name      : lfAdjOrder
*: Developer : Mariam Mazhar
*! Date      : 08/29/2011
*! Purpose   : Function to change temp file index 
*!**************************************************************************
FUNCTION lfAdjOrder
lnOldSel = SELECT(0)
IF LCRPSORT ='K'
  SELECT (lcPakLnTmp)
  SET ORDER TO 'PACK_ID'
ENDIF
SELECT(lnOldSel)
RETURN ""