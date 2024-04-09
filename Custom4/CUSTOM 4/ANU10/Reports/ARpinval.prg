*:***************************************************************************
*: Program file  : ARPINVAL
*: Program desc. : Custom Invoice Form for ANUE LIGNE
*: For Report    : Invoice 
*: System        : Aria 4XP
*: Module        : (AR)
*: Developer     : Mariam Mazhar [MMT]
*: Ticket NO     : T20070314.0009 
*: Track  NO     : C200801
*: Date          : 06/19/2007
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  :..
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARPINVAN
*!**************************************************************************
*! Modifications      :
*!
*! B608460,1 WAM 02/28/2008 Get the COD tracking# from the INVHDR & CONSINVH filed 
*:***************************************************************************


*!*************************************************************
*! Name      : lfGetPckTkt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 20/06/2007
*! Purpose   : get INVOICE Picking tickets 
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Called From : Frx
*!*************************************************************
FUNCTION lfGetPckTkt
PARAMETERS lcInvNum,lcUpsNum


lcCurrAlias = SELECT(0)

IF !USED('consinvh')
  =gfOpenTable(oAriaApplication.DataDir+'consinvh',oAriaApplication.DataDir+'CONSINVH','SH')
ENDIF 

IF !USED('ARUPSSHP')
  =gfOpenTable(oAriaApplication.DataDir+'ARUPSSHP',oAriaApplication.DataDir+'ARUPSSHP','SH')
ENDIF
lcPikNum =""
IF invhdr.consol = 'Y'
  IF gfSeek(lcInvNum,'consinvh')
    SELECT consinvh
    SCAN REST WHILE INVOICE+STORE+ORDER+PIKTKT = lcInvNum
      lcPikNum = lcPikNum +IIF(!EMPTY(lcPikNum),",","")+consinvh.PIKTKT
      *B608460,1 WAM 02/28/2008 Get the COD tracking# from the CONSINVH file
*!*	      IF gfSeek(consinvh.PIKTKT,'ARUPSSHP')
*!*	        lcUpsNum = lcUpsNum + IIF(!EMPTY(lcUpsNum),",","")+ARUPSSHP.ctrack_no
*!*	      ENDIF
      lcUpsNum = lcUpsNum + IIF(!EMPTY(lcUpsNum),",","")+ALLTRIM(CONSINVH.cCodTrckNo )
      *B608460,1 WAM 02/28/2008 (End)
    ENDSCAN
  ENDIF 
ELSE

  lcPikNum = invhdr.PIKTKT
  *B608460,1 WAM 02/28/2008 Get the COD tracking# from the INVHDR file
*!*	  IF gfSeek(invhdr.PIKTKT,'ARUPSSHP')
*!*	    lcUpsNum = lcUpsNum + IIF(!EMPTY(lcUpsNum),",","")+ARUPSSHP.ctrack_no
*!*	  ENDIF
  lcUpsNum = lcUpsNum +IIF(!EMPTY(lcUpsNum),",","")+ INVHDR.cCodTrckNo 
  *B608460,1 WAM 02/28/2008 (End)

ENDIF  

SELECT(lcCurrAlias)

RETURN lcPikNum 


