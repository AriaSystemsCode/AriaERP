**************************************************************************
*: Program file  : ARCSTMOL (Converted from 26 to 27. FOR OLSEN)
*: Program desc. : CUSTOMIZED CUSTOMER STATEMENT.
*: Developer     : Mohamed Shokry Mohamed (MHM)
*: Date          : 002/12/2008
*: Refer to      : (T20071228.0011   C200954)
****************************************************************************

*--Case the Multi currency only.
IF !USED(lcPostDc)
  =gfOpenFile(gcDataDir+"PostDChq","PostDChq", 'SH', @lcPostDc, .T.)
ENDIF  

lcTmpFact = SPACE(5)
llNewCust  = .T.
TODAY  = DATE()
*--Set the variables.
DIMENSION laAddress[1,1]
STORE ' ' TO laAddress,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
STORE ' ' TO lcFName,lcFAddr1,lcFAddr2,lcFAddr3,lcFAddr4,lcFAddr5,lcFAddr6 
STORE {} TO LDATE,HDATE


lnPos     = ASCAN(laOGVrFlt,'DEBIT.TRANDATE')
*-- Get the ldate and hdate
IF lnPos <> 0
  lnDatePos = ASUBSCRIPT(laOGVrFlt,lnPos,1)
  lnPPos    = AT("|",laogvrflt[lnDatePos,6])
  LDATE     = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],1,lnPPos-1))
  HDATE     = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],lnPPos+1))
ENDIF

HDATE = IIF(HDATE=CTOD('  /  /    '), DATE(), HDATE)
THRUDATE = DTOC(LDATE-1)
END      = HDATE

SELECT (lcTmpAcct)

SET RELATION TO
SET RELATION TO 'M' + Account INTO Customer ADDITIVE

SELECT (lcTmpTrans)

DO GfDispRe WITH EVAL('lcFormName')

SET DEVICE TO SCREEN


RETURN

*!*************************************************************
*! Name      : LFGetHdr
*! Developer : MMohamed Shokry(MHM)
*! Date      : 02/22/2008
*! Purpose   : Geth the header informations.
*!*************************************************************
*! Called from : ARCSTMOL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =LFGetHdr()
*!*************************************************************
FUNCTION LFGetHdr
para lcParam

lnCurrAlis = ALIAS()
SELECT (lcTmpAcct)

LLFACTORW = (llRPRemit .AND. !EMPTY(CFACCODE))
IF LLFACTORW .AND. lcTmpFact<>CFACCODE 
  lcTmpFact = CFACCODE
      *-- C101585 Seek the factor in sycFact not in customer file.
  SELECT SycFact
  =SEEK(lcTmpFact,'SycFact')
  STORE '' TO lcFName,lcFAddr1,lcFAddr2,lcFAddr3,lcFAddr4,lcFAddr5,lcFAddr6
  lcFName  = SycFact.cFacComp
  lcFAddr1 = cAddress1
  IF !EMPTY(lcFAddr2)
    lcFAddr2  = cAddress2
    lcFAddr3  = SUBSTR(cAddress3,1,15) +','+SUBSTR(cAddress4,1,3)+','+SUBSTR(cAddress5,1,10)
  ELSE
    lcFAddr2  = SUBSTR(cAddress3,1,15) +','+SUBSTR(cAddress4,1,3)+','+SUBSTR(cAddress5,1,10)
  ENDIF
    SELECT (lcTmpAcct)
ENDIF

STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
=SEEK('M' + &lcTmpTrans..Account,'CUSTOMER')
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
IF EMPTY(lcBtAdd2)
  lcBtAdd2 = lcBtAdd3
  lcBtAdd3 = lcBtAdd4
  lcBtAdd4 = lcBtAdd5
  lcBtAdd5 = lcBtAdd6
  lcBtAdd6 = ""
ENDIF

SELECT (lcTmpAcct)
lcADName = IIF(LLFACTORW,lcFName,lcBtName)
lcAdd1= IIF(LLFACTORW,lcFAddr1,lcBtadd1)
lcAdd2= IIF(LLFACTORW,lcFAddr2,lcBtadd2)
lcAdd3= IIF(LLFACTORW,lcFAddr3,lcBtadd3)

SELECT(lnCurrAlis )
RETURN lcParam

*!*************************************************************
*! Name      : LFCalcGrp
*! Developer : MMohamed Shokry(MHM)
*! Date      : 02/22/2008
*! Purpose   : Geth the Total Info.
*!*************************************************************
*! Called from : ARCSTMOL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =LFCalcGrp()
*!*************************************************************
FUNCTION LFCalcGrp
para lcParam

lnCredit=0
lnAge120=0
lnAge90=0
lnAge60=0
lnAge30=0
lnCurrent=0
lnTotal = 0
lnCurrAlis = ALIAS()

SELECT (lcTmpTrans)
LnPostDChq = 0

IF USED(lcPostDc)
  SELECT (lcPostDc)
  =SEEK(&lcTmpTrans..Account)
  SUM Amount REST WHILE Account = &lcTmpTrans..Account TO LnPostDChq
ENDIF 

SELECT (lcTmpTrans)
lnCurrrec = RECNO()
lcGroupKey = cGroupKey
SUM REST AMOUNT TO lnBEGIN WHILE cGroupKey = lcGroupKey .AND.TRANDATE<LDATE
SELECT (lcTmpTrans)
=SEEK (lcGroupKey)
SCAN WHILE cGroupKey = lcGroupKey
*B603782,1 Change variable name in Scan loop [End]
  lnTotal= lnTotal+AMOUNT
  *-- Calculate DAYS
  IF lcAgeType='T'                    &&Aging by terms
    XDUEDATE = IIF(DUEDATE=CTOD('  /  /  '), TRANDATE+30, DUEDATE)  
    DAYS=HDATE-XDUEDATE
    DO CASE               
      CASE AMOUNT<0             
        lnCredit=lnCredit+AMOUNT  
      CASE DAYS>=91             
        lnAge120=lnAge120+AMOUNT  
      CASE DAYS>=61             
        lnAge90=lnAge90+AMOUNT    
      CASE DAYS>=31             
        lnAge60=lnAge60+AMOUNT    
      CASE DAYS>=1              
        lnAge30=lnAge30+AMOUNT    
      OTHERWISE                 
        lnCurrent=lnCurrent+AMOUNT 
    ENDCASE
    *--Aging by transaction date
  ELSE                                  
    DAYS=HDATE-TRANDATE
    DO CASE               
      CASE AMOUNT<0       
        lnCredit=lnCredit+AMOUNT   
      CASE DAYS>=120             
        lnAge120=lnAge120+AMOUNT   
      CASE DAYS>=90              
        lnAge90=lnAge90+AMOUNT     
      CASE DAYS>=60              
        lnAge60=lnAge60+AMOUNT     
      CASE DAYS>=30              
        lnAge30=lnAge30+AMOUNT     
      OTHERWISE                  
        lnCurrent=lnCurrent+AMOUNT 
    ENDCASE                              
  ENDIF   
ENDSCAN 


SELECT (lcTmpTrans)
GOTO lnCurrrec
SELECT(lnCurrAlis )
 
*TMI set this variable to false in start of printing each group
llEndGrp = .F. 
*TMI set this variable to false in start of printing each group

RETURN lcParam