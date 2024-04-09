PRIVATE lcAlStatus,lcSlStatus,lcRepExpr,lcSelAls
lcSelAls = SELECT(0)

lcStTime = TIME()

lcAlStatus = ''
IF lcRpAloSt # 'A'
  lcAlStatus = IIF(lcRpAloSt='L','TOTPIK/TOTQTY=1',;
               IIF(lcRpAloSt='N','TOTPIK/TOTQTY=0',;
               'BETWEEN(TOTPIK/TOTQTY,0.01,0.99)'))
ENDIF               

lcSlStatus =  IIF(lcRpRecSt='S','LLSEL',IIF(lcRpRecSt='N','!LLSEL',''))
lcSlStatus = IIF(EMPTY(lcAlStatus) OR EMPTY(lcSlStatus),'',' .AND. ') + lcSlStatus
lcRepExpr  = lcAlStatus + lcSlStatus

SELECT (lcTmpOrdLn)
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer ADDITIVE
SET RELATION TO Style INTO STYLE ADDITIVE

lnPrnRecNo = IIF(EOF(),lnPrnRecNo,RECNO())

*--------------------
IF lcRpSortBy = 'A'
  lcOutExpr1 = ['Account :  ' + ACCOUNT + '  ' + CUSTOMER.BTNAME]
  lcOutExpr2 = ['Account :  ' + ACCOUNT + '  ']
  IF lcOldSort != 'A'
    REPLACE ALL cSortField WITH ACCOUNT+ORDER+STYLE+DYELOT
  ENDIF  
ELSE
  lcOutExpr1 = [lcStyleTtl + ' : ' + STYLE + '  ' + STYLE.DESC1]
  lcOutExpr2 = [lcStyleTtl + ' : ' + STYLE + '  ']
  IF lcOldSort != 'S'
    REPLACE ALL cSortField WITH STYLE+DYELOT+ORDER
  ENDIF  
ENDIF
*--------------------
lcOldSort = lcRpSortBy

GO TOP
lcRepExpr = IIF(EMPTY(lcRepExpr),'.T.',lcRepExpr)
DO gfDispRe WITH EVAL('lcRpAlForm') , 'FOR ' + lcRepExpr

SET RELATION OFF INTO Customer
SET RELATION OFF INTO STYLE

SELECT (lcSelAls)
*--------------------------

FUNCTION lfClearRep
SELECT (lcTmpOrdLn)
REPLACE ALL cSortField WITH EVALUATE(lcIndexExp)
IF BETWEEN(lnPrnRecNo,1,RECCOUNT())
  GO lnPrnRecNo
ELSE
  GO TOP
ENDIF
lcOldSort = ''
*-----------------

FUNCTION lfvRepForm
*-- Enable/Disable Sort By. [Begin]
lcRpSortBy = IIF(lcRpRepFrm = 'D', 'A' , 'S')
laOGObjCnt[5] = (lcRpRepFrm = 'D')    && Enable if detail format.
= lfOGShowGet('lcRpSortBy')  && Called to show object get.
*-- Enable/Disable Sort By. [Begin]

lcRpAlForm = IIF(lcRpRepFrm = 'D','ALAUTODT','ALAUTOSU')
= lfRepPltFr(lcRpAlForm)
*-- end of lfvRepForm.
