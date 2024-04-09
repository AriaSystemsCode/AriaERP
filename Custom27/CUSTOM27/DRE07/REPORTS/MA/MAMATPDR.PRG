*:*********************************************************************************************
*: Program file  : MAMATPDR.PRG 
*: Program desc. : Optional program for Material PO form for Dream Apparel
*: System        : Aria Advantage Series.
*: Module        : MA
*: Developer     : AHMED MAHER (AMH)
*: Date          : 01/13/2003
*: Refer to      : C(102779)
*:*********************************************************************************************

*--Add the customer filter.
llUseCust = IIF(!EMPTY(lcRpAcct) .AND. USED(lcRpAcct) .AND. RECCOUNT(lcRpAcct)>0,.T.,.F.)
lcRpExp = lcRpExp + IIF(llUseCust," .AND. SEEK(POFHDR.CMATACCT,lcRpAcct)","")
SET RELATION TO 'M'+PofHdr.cmatacct INTO Customer ADDI

*:****************************************************************
*: Name        : lfvAccount
*: Developer   : AHMED MAHER (AMH)
*: Date        : 01/19/2003
*: Purpose     : Validate the customer in range option.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfvAccount()
*:****************************************************************
*
FUNCTION lfvAccount
PARAMETERS lcDumm

lcDumm = .T.
IF !EMPTY(lcRpAcct)
  PRIVATE lcAlias
  lcAlias = SELECT(0)
  IF !USED(lcRpAcct)
    CREATE TABLE (gcWorkDir+lcRpAcct)  (Account C(5))
    INDEX ON ACCOUNT TAG (lcRpAcct) OF (lcRpAcct)
  ENDIF
  lcBrowFlds = [account   :H = 'Acct#'   :10,] + ;
               [btName    :H = 'Name'    :35,] + ;
               [Phone1    :H = 'Phone'   :16,] + ;
               [cAddress6 :H = 'Country' :30,] + ;
               [netBal    :H = 'Balance']
  IF USED('CUSTOMER')
    SELECT CUSTOMER
  ELSE
    =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  ENDIF
  llContinue = gfrange(lcBrowFlds,lcRpAcct,"account",['M'])
  SELECT (lcAlias)
ENDIF
RETURN ""
*-- End of lfvAccount.