PRIVATE llCustOpn

WAIT WINDOW 'Opening customer file' NOWAIT
llCustOpn = gfOpenFile(lcDataDir+'GLDist')
IF llCustOpn
  WAIT WINDOW 'Fixing SKU code on customer file' NOWAIT
  REPLACE ALL SkuTmpl with cSkuCode
  USE IN CUSTOMER
ENDIF
WAIT CLEAR
