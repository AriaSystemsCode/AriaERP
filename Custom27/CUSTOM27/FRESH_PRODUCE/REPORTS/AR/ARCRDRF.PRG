****************************************************************************
*: Program file      : ARCRDRF.PRG
*: Program desc.     : CREDIT REFERENCE
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (AR)
*: Developer         : TAK
*: Date              : 10/10/2000
*:**************************************************************************

*-- To check if there is any selected Accounts.
llEOFAcc = .T.
*-- To hold the file of the selected style if any
lcAccFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcAccFile) AND USED(lcAccFile)
    SELECT (lcAccFile)
    GOTO TOP
    llEOFAcc = EOF()
  ENDIF  
ENDIF

IF llEOFAcc
  WAIT WINDOW 'No Accounts was selected, Nothing to print!!'
  SET DEVI TO SCREEN
  RETURN
ENDIF

llOpend1=gfOpenFile(gcDataDir+'Invhdr','','SH')
SELECT INVHDR
SET ORDER TO TAG Invhdra DESC

llOpend2=gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
lcCopAdd1    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
lcCopAdd2    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
lcCopAdd3    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
lcCopAdd4    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
lcCopAdd5    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
lcCopAdd6    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)



SELECT (lcAccFile)
SET RELA TO 'M'+Account INTO CUSTOMER
GO TOP

= gfDispRe(lcRpForm)


SELECT (lcAccFile)
GO TOP

IF llOpend1 AND USED('INVHDR')    
  USE IN INVHDR
ENDIF

IF llOpend2 AND USED('SYCCOMP')    
  USE IN SYCCOMP
ENDIF
RETURN



**********************************
FUNCTION lfwRepWhen


FUNCTION lfSetAcct
PARAMETERS lcParm
SET ORDER TO CUSTOMER IN CUSTOMER
GO TOP IN CUSTOMER


***********************************
FUNCTION lfLastSold

ldLSdate = {}
IF SEEK(Account,'INVHDR')
  ldLSdate = INVHDR.INVDATE
ENDIF
RETURN ldLSdate
