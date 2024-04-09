*C201267,1 SMA 09/19/2010 customize order form to print separately Style/Color....[BEGIN]
DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
 IF laItemSeg[lnCount,1]='C'
   lnClrLen = LEN(laItemSeg[lnCount,3])
   lnClrPos = laItemSeg[lnCount,4]
   lcClrSpr = ALLT(laItemSeg[lnCount,6])
   EXIT
 ENDIF
ENDFOR
*C201267,1 SMA 09/19/2010 customize order form to print separately Style/Color....[END]