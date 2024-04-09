*:***************************************************************************
*: Program file  : SOORCNVS
*: Program desc. : Custom Order Confirmation Form For Vision(VIS20)
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : SO
*: Developer     : Mariam Mazhar  (MMT)
*: Date          : 06/23/2009
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:MOdifcations :
*: B608990,1 MMT 09/14/2009 Fix bug of page orientation[T20090213.0022]
*! B609550,1 MMT 03/15/2011 Custom Order confirmation form VS prints wrong totals[T20110303.0011]
*: C201595,1 MMT 09/30/2013 Convert Custom Invoice Form and Order Conf. to graphic[T20130809.0003]
*:***************************************************************************
*: This Report Program is due to C20170(Aria4),C201165(Aria27)... [T20090213.0022]
*:***************************************************************************
*B608990,1 MMT 09/14/2009 Fix bug of page orientation[Start]
*:C201595,1 MMT 09/30/2013 Convert Custom Invoice Form and Order Conf. to graphic[Start]
*loogScroll.cCROrientation = 'L'
loOGScroll.cCROrientation = 'P'
*:C201595,1 MMT 09/30/2013 Convert Custom Invoice Form and Order Conf. to graphic[End]
*B608990,1 MMT 09/14/2009 Fix bug of page orientation[End]
IF !USED('NTA')
  =gfOpenTable('NotePad','NotePad','SH','NTA')
ENDIF 

*!*************************************************************
*! Name      : lfGetLastLine
*! Developer : Mariam Mazhar  (MMT)
*! Date      : 06/23/2009
*! Purpose   : Function to Get last line in order
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
LNTOTAMT  =  0 
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTempOrd)
lnThRec = RECNO(lcTempOrd)    && Save Current record #.
*! B609550,1 MMT 03/15/2011 Custom Order confirmation form VS prints wrong totals[Start]
*lcThStore = Store
lcThStore = lcStoreID 
*! B609550,1 MMT 03/15/2011 Custom Order confirmation form VS prints wrong totals[End]
SCAN FOR IIF(lcRpSortBy = 'S',CORDTYPE + ORDER + STORE + STYLE,CORDTYPE + ORDER + STORE + STR(LINENO,6)) = ;
		 OrdHdr.cordtype + OrdHdr.order + lcThStore
  LNLstLn = lineno
  LNTOTAMT = LNTOTAMT + (Eval(lcTempOrd+'.price') * Eval(lcTempOrd+'.totQty'))
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF
IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF

SELECT (lcThAlias)            && Restore Alias.
RETURN ''

*! B609550,1 MMT 03/15/2011 Custom Order confirmation form VS prints wrong totals[Start]
*!*************************************************************
*! Name      : lfSaveStr
*! Developer : Mariam Mazhar  (MMT)
*! Date      : 03/15/2011
*! Purpose   : Function to Save Store of current page
*!*************************************************************
FUNCTION lfSaveStr
lcStoreID = &lcTempOrd..Store
RETURN ''
*! B609550,1 MMT 03/15/2011 Custom Order confirmation form VS prints wrong totals[End]