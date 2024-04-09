lcDataDir=GETDIR('','Select data directory')
IF EMPTY(lcDataDir)
  WAIT WINDOW 'No data directory was selected. Cannot Proceed.'
  RETURN
ENDIF  

CLOSE DATA

*-- Open the necessary files.
create cursor tmpRet ( crmemo C(6) ,HDRQTY N(7), LINQTY N(7),LINAMT N(11,2) )

SELECT 0
USE (lcDataDir+'RETHDR.dbf') SHARED
SET ORDER TO TAG RETHDR
SELECT 0
USE (lcDataDir+'RETline.dbf')  SHARED
SET ORDER TO TAG RETline
SELECT 0
USE (lcDataDir+'credit.dbf')  SHARED
SET ORDER TO TAG credit
SELECT 0
USE (lcDataDir+'arhist.dbf')  SHARED
SET ORDER TO TAG Arhistt    &&Arhistht


*-Collect wrong returns.
SELECT RETHDR
SCAN
  WAIT WINDOW CRMEMO NOWAIT
  IF SEEK(RETHDR.CRMEMO,'RETLINE') 
    STORE 0 TO lnTotQty,lnAmount
    SELECT RETLINE
    SUM TotQty,Amount TO lnTotQty,lnAmount WHILE CRMEMO = RETHDR.CRMEMO FOR CRET_TRNCD<>'4'
    IF RETHDR.PIECES <> lnTotQty
      SELECT TMPRET
      APPEND BLANK
      REPLACE CRMEMO WITH RETHDR.CRMEMO,;
              HDRQTY WITH RETHDR.PIECES,; 
              LINQTY WITH lnTotQty,;
              LINAMT WITH lnAmount

   ENDIF 
  ENDIF
ENDSCAN

*-- Update wrong returns.
SELECT TMPRET
SCAN
  *--Update Return Header.
  SELECT RETHDR
  IF SEEK(TMPRET.CRMEMO)
    =RLOCK()
    REPLACE Pieces    WITH TMPRET.LINQTY,;
            Gross_Amt WITH TMPRET.LINAMT,;
            Amount    WITH TMPRET.LINAMT,;
            TotCredit WITH TMPRET.LINAMT + RETHDR.TAX_AMT,;
            CommAmt1  WITH ABS(ROUND(TMPRET.LINAMT * (RETHDR.CommPcnt1/100),2)) * -1,;
            CommAmt2  WITH ABS(ROUND(TMPRET.LINAMT * (RETHDR.CommPcnt2/100),2)) * -1
    UNLOCK
    *--Update Credit file.
    SELECT CREDIT
    IF SEEK(Rethdr.account+ Rethdr.crmemo)
      =RLOCK()
      REPLACE Amount WITH -ABS(RETHDR.Amount)
      UNLOCK
    ELSE
      *--Update ArHist file.
      *--We have 3 returns has been key off and need a special update.
      SELECT ARHIST
      IF SEEK(Rethdr.account+ Rethdr.crmemo)
        SCATTER MEMVAR
        lnAmtDiff = ABS(Amount) - ABS(RETHDR.Amount)
        =RLOCK()
        REPLACE Amount WITH -ABS(RETHDR.Amount)
        UNLOCK
        lcHistKey = account+history
        SET ORDER TO TAG Arhistht
        IF SEEK(account+history+'7')
          REPLACE Amount WITH Amount-lnAmtDiff
        ELSE
          APPEND BLANK
          GATHER MEMVAR
          REPLACE TranType WITH '7',TranCode WITH '000114',Tran WITH '001000',;
                  TranDate WITH HistDate,Amount WITH -lnAmtDiff,Reference WITH 'CR memo amount diff.',;
                  CADJACCT WITH '40300'
        ENDIF
        SET ORDER TO TAG Arhistt        
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
WAIT CLEAR
CLOSE DATA
