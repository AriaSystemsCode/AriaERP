************************************************************************
*: Program file   : SOPOINTS (C# ARROW)
*: Program desc.  : Calculate and update customer gift points for (ARROW)
*:       Module   :   A 2.7
*:         System : Aria Apparel System
*:      Developer : AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls : FUNCTIONS :
*:                    gfOpenFile(),gfTempName()
*:*********************************************************************
*: Passed Parameters  : None
*:*********************************************************************

*---Open nessesary file's
= gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
= gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
= gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
= gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
= gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINE','SH')
= gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
lcPos = gfTempName()
CREATE TABLE &lcPos ;
	(Account C(05), Pos N(10,3), GPOINT N(10))
INDEX ON Account TAG (lcPos)
lcNeg = gfTempName()
CREATE TABLE (lcNeg) ;
	(Account C(05), Neg N(10,3), RPOINT N(10))
INDEX ON Account TAG (lcNeg)

*---Loop To Get Customer point and customer invoice
SELECT INVLINE
SCAN
  =SEEK(Invoice,'InvHdr')
  IF INVHDR.STATUS <> 'V'
    SCAT MEMVAR MEMO
    IF !SEEK(m.Account,lcPOS)
      WAIT WINDOW "Totaling sales for customer "+m.Account+" ...Please Wait" NOWAIT
      SELECT(lcPOS)
      APPEND BLANK
      REPLACE ACCOUNT WITH INVLINE.ACCOUNT;
              POS     WITH IIF(SUBSTR(INVLINE.Style,1,5)='ARROW',0,;
                           (INVLINE.TOTQTY*INVLINE.PRICE));
              GPOINT  WITH IIF(SUBSTR(INVLINE.Style,1,5)='ARROW',;
                           IIF(SEEK(InvLine.Style,'Style'),;
                                    (Style.nPoint)*InvLine.TotQty,0),0)
    ELSE
      SELECT(lcPOS)
      IF SUBSTR(INVLINE.Style,1,5) = 'ARROW'
        REPLACE GPOINT WITH GPOINT + IIF(SEEK(InvLine.Style,'Style'),;
                                            (Style.nPoint)*InvLine.TotQty,0)
      ELSE
        REPLACE POS WITH POS + (INVLINE.TOTQTY*INVLINE.PRICE)
      ENDIF    
    ENDIF
  ENDIF
ENDSCAN

*---Loop To Get customer return
SELECT RETLINE
SCAN
  =SEEK(crMemo,'RETHDR')m
  IF RETHDR.STATUS <> 'V'
    SCAT MEMVAR MEMO
    IF !SEEK(m.Account,lcNeg)
      WAIT WINDOW "Totaling return for customer "+m.Account+" ...Please Wait" NOWAIT
      SELECT(lcNeg)
      APPEND BLANK
      REPLACE ACCOUNT WITH RETLINE.ACCOUNT;
              RPoint  WITH IIF(SUBSTR(RETLINE.Style,1,5)='ARROW',;
                           IIF(SEEK(RETLINE.Style,'Style'),;
                                    (Style.nPoint)*RETLINE.TotQty,0),0);
              Neg     WITH RETLINE.TOTQTY*RETLINE.PRICE
    ELSE
      SELECT(lcNeg)
      IF SUBSTR(RETLINE.Style,1,5) = 'ARROW'
        REPLACE RPOINT WITH RPOINT + IIF(SEEK(RETLINE.Style,'Style'),;
                                            (Style.nPoint)*RETLINE.TotQty,0)
      ELSE
        REPLACE NEG WITH NEG + (RETLINE.TOTQTY*RETLINE.PRICE)
      ENDIF    
    ENDIF
  ENDIF
ENDSCAN

*--- Start Update Customer File
SELECT (lcPos)
SET RELATION TO Account INTO (lcNeg) ADDIT
lnCustPoint = 0
SCAN
  lnAmt    = (&lcPos..Pos - &lcNeg..Neg)
  lnPoint  = (&lcPos..GPoint - &lcNeg..RPoint)
  DO CASE
*---Customer First time Cases
    CASE (lnAmt >= 250)
      lnAmtPnt = ROUND(((lnAmt - 250)/100),0)
      lnCustPoint = (15 + lnAmtPnt) - lnPoint
    CASE 250 > lnAmt AND lnAmt > 0
      lnCustPoint = 5 - lnPoint
  ENDCASE
  IF SEEK('M'+&lcPos..Account,'Customer')
    SELECT CUSTOMER
    WAIT WINDOW "Updating net sales for customer "+ALLTRIM(Account)+" : " +ALLTRIM(BtNAme)+";  points: "+STR(lnCustPoint,4)+" ...Please Wait" NOWAIT
    REPLACE USR_DFND4 WITH STR(lnCustPoint,4)
  ENDIF
ENDSCAN
WAIT WINDOW "Customer file was updated successfully...Press any key"
WAIT CLEAR
IF USED(lcPos)
  USE IN (lcPos)
  ERASE &gcWorkDir.&lcPos+'.DBF'
  ERASE &gcWorkDir.&lcPos+'.CDX'
ENDIF
IF USED(lcNeg)
  USE IN (lcNeg)
  ERASE &gcWorkDir.&lcNeg+'.DBF'
  ERASE &gcWorkDir.&lcNeg+'.CDX'
ENDIF