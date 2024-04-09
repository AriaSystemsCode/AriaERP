*:***************************************************************************
*: Program file  : ARPINVNI.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR NINA MCLEMORE, LLC
*: Date          : 08/03/2003
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Abdou Elgendy (ABD)
*: Tracking Job Number: 102845
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVST
*:***************************************************************************
*: Modifications :
*:*************************************************************************
*:
*--Run the code in case the user select "YES".
IF llRpKeyOf
  llUseArh = .F.
  IF !USED('ARHIST')
    =gfOpenFile(gcDataDir+"ARHIST",gcDataDir+"ARHISTT",'SH')
    llUseArh = .T.
  ENDIF  

  SELECT ARHIST
  IF !llUseArh
    SET ORDER TO TAG ARHISTT
  ENDIF

  =lfCreatTmp()  && Create work cursor.
  =lfCollData()  && Collect the data.

  SELECT INVHDR
  SET RELATION TO ACCOUNT + INVOICE INTO (lcCrdtInv) ADDITIVE
ENDIF

*-- End of the Program
*:*************************************************************
*: Name      : lfCreatTmp
*: Developer : Abdou Elgendy (ABD)
*: Date      : 08/03/2003
*: Purpose   : Create Temp. file that hold the Credit data.
*:*************************************************************
*: Calls     :
*:         Procedures : ....
*:         Functions  : ....
*:*************************************************************
*: Called from        : Report code
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example            : =lfCreatTmp()
*:*************************************************************
*:
FUNCTION lfCreatTmp

PRIVATE lcAlias

lcAlias = ALIAS()

CREATE CURSOR (lcCrdtInv) (Invoice C(6) , Account C(5) , TotCrd N(6,2))
INDEX ON ACCOUNT + Invoice TAG Invoice of (gcWorkDir + lcCrdtInv)

SELECT(lcAlias)

*--End of lfCreatTmp.
*:*************************************************************
*: Name      : lfCollData
*: Developer : Abdou Elgendy (ABD)
*: Date      : 08/03/2003
*: Purpose   : Function to collect the data.
*:*************************************************************
*: Calls     :
*:         Procedures : ....
*:         Functions  : ....
*:*************************************************************
*: Called from        : ARPINVAP.PRG
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example            : =lfCollData()
*:*************************************************************
*:
FUNCTION lfCollData
PRIVATE lcAlias , lcRcNoHdr

lcAlias = ALIAS()
SELECT INVHDR
lcRcNoHdr = EVAL(KEY())

SCAN FOR &lcRpExp
  IF EVAL(lcTmpDbt+'.CFILE_NUM') = "1"
    lcInvoice = INVHDR.ACCOUNT + INVHDR.INVOICE
    SELECT ARHIST
    IF SEEK(lcInvoice , 'ARHIST')
      SUM REST AMOUNT WHILE ACCOUNT + TRAN + CINSTALNO = INVHDR.ACCOUNT + INVHDR.INVOICE FOR TranType='1' TO lnCrdAmt
      SELECT (lcCrdtInv)
      IF lnCrdAmt # 0
        APPEND BLANK
        REPLACE Invoice WITH INVHDR.INVOICE ,;
                Account WITH INVHDR.ACCOUNT ,;
                TotCrd  WITH lnCrdAmt
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

=SEEK(lcRcNoHdr)
SELECT(lcAlias)

*--End of lfCollData.
*:*************************************************************