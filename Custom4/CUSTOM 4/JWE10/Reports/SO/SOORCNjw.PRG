FUNCTION lfGetOrdChg
LPARAMETERS lcOrderNo
lnOldSel = SELECT(0)


lnRetValue = 0
IF !USED('OrderChg')
  =gfOpenTable('OrderChg','ORDERCHG')
ENDIF
IF gfSeek('O'+lcOrderNo,'OrderChg','ORDERCHG')
  SELECT ORDERCHG
  LOCATE REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE = 'O'+lcOrderNo FOR cChrgType ='C'
  IF FOUND()
    lnRetValue = ORDERCHG.nchrgamnt
  ENDIF  
ENDIF
SELECT(lnOldSel)
RETURN lnRetValue 