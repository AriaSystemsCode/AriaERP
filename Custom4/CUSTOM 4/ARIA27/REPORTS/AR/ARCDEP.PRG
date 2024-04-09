*:**************************************************************************
*: Program file  : ARCDEP
*: Program desc. : CUSTOMER DEPARTMENT REPORT
*: Date          : 03/18/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE (AR)
*: Developer     : Sameh (SSE)
*:**************************************************************************
*: Calls : 
*:    Procedures : lpPrint
*:    Functions  : lfwGrid() ,lfvAccount() ,lfvCusDep() ,lfwOldVal() ,
*:                 lfDeptEnab()
*:**************************************************************************
*: Example : DO ARCDEP
*:**************************************************************************
*: This Program is due to E301174 ...
*:**************************************************************************
*
lcRpAcc = IIF(!EMPTY(lcRpAcc),PADR(lcRpAcc,5),ALLTRIM(lcRpAcc))
lcRpExp = STRTRAN(lcRpExp,'CUSTDEPT.','')

SELECT CUSTDEPT
SET ORDER TO Custdept

LOCATE FOR ACCOUNT + DEPT = lcRpAcc AND &lcRpExp

*-- If there are records selected 
IF !EOF()
  *-- PRINT THE REPORT FROM THE WORKFILE
  CLEAR TYPEAHEAD	
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ELSE        && no records selected
  *-- Text   : 'No Record Selected for the report..!'
  *-- Buttom : '               <OK>                 '
   =gfModalGen('TRM00052B00000','DIALOG')
ENDIF
*-- End of Report code.

****************************************************************************
************************* *-- Report Functions --* *************************
****************************************************************************

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/21/1999
*! Purpose   : Printing loop for the main report
*!**************************************************************************
*! Called from : Main Program (ARCDEP)
*!**************************************************************************
*! Calls       : gfCodDes()
*!**************************************************************************
*! Example     : Do lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint
*--Printing loop
R_TITLE   = 'Departments Report'
R_WIDTH   = 'N'
ROW       = 99
PAGENO    = 0
lcAccount = ''

SCAN REST WHILE ACCOUNT + DEPT = lcRpAcc AND &lcRpExp
  WAIT WINDOW 'Printing Account ' + Account + ', ' + 'Department ' + Dept NOWAIT
  *-- Print the Header.
  IF ROW >= 53
    ROW = 0
    PAGENO = PAGENO+1
    
    DO RPT_HDR WITH 'ARCDEP','',R_WIDTH

    *-----------'1.....6.....12......................36...41....47...52....58....................'
    @ 05,00 SAY 'Acct. Dept. Description             Rep1 Comm1 Rep2 Comm2 ........Terms.........'
    @ 06,00 SAY REPLICATE('=',80)
    ROW = 7
  ENDIF

  @ ROW ,00 SAY IIF(!(Account==lcAccount),Account,'')
  @ ROW ,06 SAY Dept
  @ ROW ,12 SAY PADR(ALLTRIM(cDeptDesc),23)
  @ ROW ,36 SAY Rep1
  @ ROW ,41 SAY Comm1
  @ ROW ,47 SAY Rep2
  @ ROW ,52 SAY Comm2
  @ ROW ,58 SAY PADR(ALLTRIM(gfCodDes(CTERMCODE,'CTERMCODE')),22)
  ROW = ROW + 1

  IF !(Account==lcAccount)
    lcAccount = Account
  ENDIF
ENDSCAN
*--End Printing loop

****************************************************************************
*************************** *-- OG Functions --* ***************************
****************************************************************************

*!**************************************************************************
*! Name      : lfwGrid
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/21/1999
*! Purpose   : FUNCTION TO ENABLE/DISABLE PREVIEW & RUN PUSH BUTTON 
*!**************************************************************************
*! Called from : [Option Grid When function]
*!**************************************************************************
*! Example     : =lfwGrid()
*!**************************************************************************
*
FUNCTION lfwGrid
lnDeptPos = ASCAN(laOGFxFlt,'CUSTDEPT.DEPT')
IF lnDeptPos > 0
  = lfDeptEnab()
ENDIF  
*-- End of lfwGrid.

*!**************************************************************************
*! Name      : lfDeptEnab
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   : To Disable or Enable the department
*!**************************************************************************
*! Example   : = lfDeptEnab()
*!**************************************************************************
*!
FUNCTION lfDeptEnab
lnDeptPos = ASUBSCRIPT(laOGVrFlt,lnDeptPos,1)
laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnDeptPos] = llDeptStat
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDeptPos)) + ',6]')
*-- End of lfDeptEnab.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh (SSE)
*! Date      : 03/22/1999
*! Purpose   : Save any Get Object old value
*!**************************************************************************
*! Example   : =lfwOldVal()
*!**************************************************************************
*!
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/16/1999
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAccount()
*!**************************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

*-- Avoid recurresion clear read.
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF

*-- If the user changed the stored account (old value)   
  IF !(lcObjVal == lcOldVal)
    IF EMPTY(lcObjVal)
      laOGFxFlt[1,6] = ''
      llDeptStat = .F.   && this flag will Disable the Department      
    ELSE 
      IF SEEK(lcObjVal,'CustDept')
        llDeptStat = .T.   && this flag will Enable the Department
      ELSE
        llDeptStat = .F.   && this flag will Disable the Department
        =gfModalGen('INM40018B00000','DIALOG',lcRpAcc)
        *-- Text   : 'No Departments assigned to customer ô'
        *-- Button : '                <OK>                 '
        lcRpAcc = ''
        laOGFxFlt[1,6] = ''
      ENDIF  
    ENDIF
    = lfDeptEnab()       && call function which Enable/Disable Department 
    CLEAR READ   && Clear current read cycle, to activate new one.
  ENDIF  
  
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)

ENDIF  
*-- End of lfvAcct.

*!**************************************************************************
*! Name      : lfvCusDep
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 03/16/99
*! Purpose   : Validation function for Dept field (CustDept file)
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : =lfvCusDep()
*!**************************************************************************
*
FUNCTION lfvCusDep
PRIVATE lcCDepFld,lcCusDep,lnSelcFile,lcSelcTag,laTemp,lcBrFields,lcCDepExp

lcCDepFld  = VARREAD()
lcCusDep   = PADR(EVAL(lcCDepFld),5)

lnSelcFile = SELECT(0)

SELECT CustDept
GO TOP

lcSelcTag  = ORDER('CustDept')
SET ORDER TO TAG CustDept
lcRpAcc = PADR(lcRpAcc,5)

*IF '?' $ lcCusDep .OR. (!EMPTY(lcCusDep) .AND. !SEEK(lcCusDep , 'CUSTDEPT'))
IF !EMPTY(lcCusDep) .AND. ('?' $ lcCusDep .OR. !SEEK(lcRpAcc + lcCusDep , 'CustDept')) 
  
  DIMENSION laTemp[1]
  laTemp = ''     
  lcBrFields = "Dept:H='Department',CdeptDesc:H='Description',;
                Rep1:H='Rep 1',Comm1:H='Comm 1',Rep2:H='Rep 2',;
                Comm2:H='Comm 2',CtermCode:H='Term' "
   
  
  IF !('?' $ lcCusDep) AND BETWEEN(RECNO(0),1,RECCOUNT())
    GO RECNO(0)
  ENDIF  
  
  = gfBrows("FOR Account = lcRpAcc",'Dept','laTemp','Customer Department')
  IF !EMPTY(laTemp[1])
    lcCusDep = laTemp[1]
  ELSE 
    lcCusDep = ''
  ENDIF
ENDIF

&lcCDepFld = lcCusDep
SET ORDER TO lcSelcTag
SELECT (lnSelcFile)
*-- End of lfvCusDep.
