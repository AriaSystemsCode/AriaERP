*:************************************************************************
*: Program file  : SOSHLBDO.Prg
*: Program desc. : SHIPPING LABELS REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO,AR,AL
*: Developer     : Ahmed Salah Shalaby -SSH
*: Date          : 12/08/98
*: Reference     : C#101553
*:************************************************************************
*: Calls : NONE
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*B802676,1 ASH 10/04/99 Fix the bug of not printing the shipvia in case of printing from invoice.
*C101824,1 SSE 03/21/2000 Add 50 Characters Free comments in Option Grid
*:************************************************************************

*B802676,1 ASH 10/04/99 (Begin) Set realtion to invhdr file to get the right shipvia.
IF lcXTYPE='I'
  lnAlias = ALIAS()
  SELECT (LCMAINF)
  SET RELATION TO INVOICE INTO INVHDR ADDITIVE
  SELECT (lnAlias)
ENDIF
*B802676,1 ASH 10/04/99 (End)
XLBLCNT = 0

FUNCTION lfSubIndex
PARAMETER lcPara
XLBLCNT = LABELS - 1

*!**************************************************************************
*! Name      : lfvComents
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/22/2000
*! Purpose   : Validation function for the Push button Comments
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfOptMsg()
*!**************************************************************************
*
FUNCTION lfvComents
PARAMETERS lcDummy
lcDummy = .T.
PRIVATE laComments
DECLARE laComments[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laComments[1,1] = 'lcRpComent'        && 1st. line Variable
laComments[1,2] = 50                  && Line length

=gfOptMsg('laComments')
RETURN ""
*-- End of lfvComents.