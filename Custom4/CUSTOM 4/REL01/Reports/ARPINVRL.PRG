*:***************************************************************************
*: Program file  : ARPINVRL.PRG
*: Program desc. : Customized Invoice Form RL for RELIQ, INC. ( REL01 )
*: Date          : 12/13/2006
*: System        : Aria Advantage Series.4XP
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mariam Mazhar[MMT]
*: Tracking Job #: C200725
*:***************************************************************************
*: Example : DO ARPINVRL
*:***************************************************************************

PRIVATE lnSlct,lcCrdOrd,lcHstOrd

lnSlct = SELECT()


IF !EMPTY(lcRPSalRep)
 lcRpExp = IIF(EMPTY(lcRpExp) , "INVHDR.REP1 == lcRPSalRep" , lcRpExp + " AND " + "INVHDR.REP1 == lcRPSalRep")
ENDIF

=gfOpenTable(oAriaApplication.DataDir+'CREDIT',oAriaApplication.DataDir+'CREDIT','SH')
=gfOpenTable(oAriaApplication.DataDir+'ARHIST',oAriaApplication.DataDir+'ARHISTT','SH')

lcPaidFl = '_'+SUBSTR(lcTmpDbt,2)

IF USED(lcPaidFl)
  USE IN &lcPaidFl
ENDIF  

DIMENSION laFileStru[2,4]
  laFileStru[1,1] = 'INVOICE'
  laFileStru[1,2] = 'C'
  laFileStru[1,3] = 6
  laFileStru[1,4] = 0
  
  laFileStru[2,1] = 'AMOUNT'
  laFileStru[2,2] = 'N'
  laFileStru[2,3] = 14
  laFileStru[2,4] = 2

  =gfCrtTmp(lcPaidFl,@laFileStru,'INVOICE',lcPaidFl)



*- Open InvHdr in a temp alias name , to keep settings of INVHDR file
lcTmpInvH = gfTempName()


=gfOpenTable(oAriaApplication.DataDir+'INVHDR',oAriaApplication.DataDir+'INVHDR','SH',lcTmpInvH)


lcRLExp = STRTRAN(lcRpExp,'INVHDR',lcTmpInvH)
SELECT &lcTmpInvH
GO TOP
SCAN FOR &lcRLExp
  *- Get amounts paid from Credit file
  lnAmnt = 0
  SELECT CREDIT
  IF gfSEEK(&lcTmpInvH..ACCOUNT,'CREDIT')
    SCAN REST WHILE ACCOUNT+TRAN+DTOS(TRANDATE) = &lcTmpInvH..ACCOUNT ;
              FOR ALLTRIM(CREDIT.REFERENCE) = &lcTmpInvH..INVOICE 
      lnAmnt = lnAmnt + ABS(CREDIT.AMOUNT)
    ENDSCAN
  ENDIF
  
  *- Get amounts paid from Arhist file , key off applied
  SELECT ARHIST
  IF gfSEEK(&lcTmpInvH..ACCOUNT,'ARHIST')
    SCAN REST WHILE ACCOUNT+TRAN+CINSTALNO = &lcTmpInvH..ACCOUNT;
              FOR ALLTRIM(ARHIST.REFERENCE) = &lcTmpInvH..INVOICE 
      lnAmnt = lnAmnt + ABS(ARHIST.AMOUNT)
    ENDSCAN
  ENDIF
  
  *- Add amounts paid to the temp created file for amounts
  SELECT &lcPaidFl
  APPEND BLANK  
  REPLACE INVOICE WITH &lcTmpInvH..INVOICE ;
          AMOUNT  WITH AMOUNT+lnAmnt
  
ENDSCAN

*- Set a relation for the file that contains paid amounts to be shown in the report
SELECT INVHDR
SET RELATION TO INVOICE INTO &lcPaidFl ADDITIVE
GO TOP

SELECT (lnSlct)
*!*************************************************************
*! Name      : lfvSalsRp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/13/2006
*! Purpose   : Validate sales rep.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSalsRp()
*!*************************************************************
FUNCTION lfvSalsRp
lcCurrAlias = SELECT(0)
=gfOpenFILE(oAriaApplication.DataDir+'SalesRep',oAriaApplication.DataDir+'SalesRep','SH')
SELECT('SalesRep')
IF '?' $ lcRPSalRep OR  !EMPTY(lcRPSalRep) AND  !SEEK(lcRPSalRep, "SALESREP")
    XREPCODE = lcRPSalRep
    DO REPCHK WITH lcRPSalRep, .T.
ENDIF
SELECT(lcCurrAlias)


