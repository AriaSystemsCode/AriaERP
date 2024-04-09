*:--------------------------------------------------------------------------
*: Program file        : ARCODOS.PRG (E:# 301122)
*: Program description : C.O.D Outstanding REPORT
*: For screen          : NONE
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Account Receivable - (AR)
*: Developer Name      : Walid Abou El-Magd (WAM)
*: Last modi           : 
*:---------------------------------------------------------------------------
*: Calls               : 
*:---------------------------------------------------------------------------
*: Passed Parameters   : None.
*:---------------------------------------------------------------------------
*: Example             :  
*:---------------------------------------------------------------------------
*: Modification
*
*:---------------------------------------------------------------------------
*B603061,1 BWA 03/06/2000 Fix the bug of variable "lafilename" not found.
*B603739,1 BWA 07/31/2000 Fix the bug of getting 'Table is not ordered' when validate the account.
*****************************************************************************
*:----------------------------------------------------------------
lcRpExp = IIF(lcRpExp='.T.','',lcRpExp+[ AND ]) + [(INVHDR.COD_FLAG = 'Y' OR !EMPTY(INVHDR.CODTAG))]
lcRpExp = STRTRAN(lcRpExp,'DEBIT.','')

****** Set the report variables ******
lcTermcode      =''
ROW             = 99
PAGENO          = 00
MAXROW          = 55
XTOT            = 0                      &&-- to sum the total amount
XREPORT         = 'ARCODOS'
R_TITLE         = 'C.O.D. OUTSTANDING'
XTITLE          = SPACE(0)
R_WIDTH         = 'W'                    && Standard report size is Wide
lcWorkFile      = gfTempName()           && temporary file

****** Files and Records selection ******
SELECT DEBIT                        &&-- copy the structure of DEBIT 
SET RELATION TO TRAN INTO INVHDR
***** Copying Records into the Temporary File ***********
*------------------ CREATE DEBIT.TEMP.FILE ---> [BEGIN]
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
*-- CREATION OF THE STRUCTURE OF THE TEMP FILE [BEGIN]
CREATE TABLE &gcWorkDir.&lcWorkFile FROM ARRAY laFileStru
INDEX ON account+tran+cinstalno+DTOS(trandate) TAG (lcWorkFile)
SELECT DEBIT

LOCATE FOR &lcRpExp
IF FOUND()
  SCAN REST FOR &lcRpExp
    SCATTER MEMVAR MEMO 
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDSCAN
ENDIF
SET RELATION TO  

*------------------ CREATE DEBIT.TEMP.FILE ---> [ END ]
IF RECCOUNT(lcWorkFile) = 0
  = gfModalGen('TRM00052B00000','DIALOG' )  
  DO lpClose
  RETURN
ENDIF

SELECT (lcWorkFile)
SET RELATION TO TRAN INTO INVHDR

*********************************************************
SELECT INVHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER 

****** Start Printing ******
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3
* ACCT  ............NAME.............. INVOICE DATE     ORDER  .....TERMS..... CODTAG PIKTKT  C.O.D. AMT
* XXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXX  MM/DD/YY XXXXXX XXXXXXXXXXXXXXX XXXXXX XXXXXX  9999999.99
*       ------------ 30 --------------                         ------ 15 -----
*                                                              **** TOTAL ****               99999999.99

CLEAR TYPEAHEAD
SET DEVICE TO PRINT
SELECT (lcWorkFile) 
GO TOP
DO lpPrnProc                       &&-- Print loop 

DO ENDREPORT
SET DEVICE TO SCREEN

*-- Reset relation and close files.
SELECT INVHDR
SET RELATION TO
DO lpClose

*-------------------
*    END ARCODOS
*-------------------

PROCEDURE lpPrnProc

 
*DO WHILE INKEY() <> 32  
DO WHILE .T.    
  IF ROW > MAXROW
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH &&-- print the report header again
    @ 05,00 SAY '  ACCT  ............NAME.............. INVOICE DATE     ORDER  .....TERMS..... CODTAG PIKTKT  C.O.D. AMT'
    @ 06,00 SAY REPLICATE('=',130)
    ROW=7
  ENDIF
  SELECT (lcWorkFile)
  lcTermcode = gfCodDes(InvHdr.cTermcode,'cTermcode')
  @ ROW,02 SAY ACCOUNT
  @ ROW,08 SAY CUSTOMER.BTNAME
  @ ROW,39 SAY TRAN
  @ ROW,47 SAY INVHDR.INVDATE
  @ ROW,56 SAY INVHDR.ORDER
  @ ROW,63 SAY SUBSTR(lcTermcode,1,15)
  @ ROW,79 SAY INVHDR.CODTAG
  @ ROW,86 SAY INVHDR.PIKTKT
  @ ROW,94 SAY INVHDR.COD_AMT PICTURE '9999999.99'    && TAK 5/08/93
  ROW = ROW+1
  XTOT = XTOT + INVHDR.COD_AMT                        && TAK 5/08/93
  SKIP
  IF EOF()
    EXIT
  ENDIF
ENDDO

@ ROW+1,63 SAY '**** TOTAL ****'
@ ROW+1,93 SAY XTOT PICTURE '99999999.99'
*-- end of lpPrnProc.
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose           : 
*!*************************************************************
*! Called from       : 
*!*************************************************************
*! Calls             : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

IF llMultCurr
  lnCurrPos  = ASCAN(laOGFxFlt,'DEBIT.CCURRCODE')
  IF lnCurrPos > 0
    lnCurrPos  = ASUBSCRIPT(laOGFxFlt,lnCurrPos,1)
    laOGFxFlt[lnCurrPos,6] = gcBaseCurr
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  ENDIF  
ENDIF
*-- end of lfwOGWhen.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose           : Save the old value
*!*************************************************************
*! Called from       : 
*!*************************************************************
*! Calls             : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose           : Validate function for the Customer Account field
*!*************************************************************
*! Called from       : Account field [Option Grid]
*!*************************************************************
*! Calls             : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcItsName , lcItsVal , llObjRet

lcItsName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , lcOldVal)
  &lcItsName = lcItsVal
ENDIF    
*-- end of lfvAcc.

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvInvNo

PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag , lcBrFields

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  
  lcFile_Ttl = 'Receivable invoices' 
  
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    
    &lcObjName = laOldVal
  ENDIF    
  
  SET RELATION OFF INTO CUSTOMER
ENDIF    

*--IF The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE    
  SET ORDER TO lcInvHdTag IN INVHDR
ENDIF    

*--IF The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE
  SET ORDER TO lcCstmrTag IN CUSTOMER
ENDIF
*!*************************************************************
*! Name      : lfCurrArry
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose   : function to fill the two arrays one for curr code 
*!           : and the other for curr disc
*!*************************************************************
*! Called from : Currency field [Option Grid]
*!*************************************************************
*! Calls       : gfOpenFile()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCurrArry
PRIVATE llOpnCurr , lnI
llOpnCurr = .F.

DIMENSION laCurrVal [1,1]
DIMENSION laCurrDesc[1,1]
IF !USED('SYCCURR')
  llOpnCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
ENDIF

*-----------Fill Val array.AND Disc array [BEGIN]

lnI = 1

SELECT SYCCURR

SCAN
  DIMENSION laCurrVal [lnI,1]
  DIMENSION laCurrDesc[lnI,1]
  laCurrVal [lnI,1] = Ccurrcode
  laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  lnI = lnI + 1
ENDSCAN

*-----------Fill Val array.AND Disc array [ END ]


IF llOpnCurr
  *B603061,1 BWA 03/06/2000 Fix the bug of variable "lafilename" not found.[START]
  *=gfCloseFile('SYCCURR')
  IF USED('SYCCURR')
    USE IN SYCCURR
  ENDIF  
  *B603061,1 BWA 03/06/2000 [END]
ENDIF
RETURN .T.
*-- end of lfCurrArry
*!*************************************************************
*! Name      : lfInvSet
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfInvSet
PARAMETERS lcParm
PRIVATE lcInvHdTag,lcCstmrTag
STORE '' TO lcInvHdTag,lcCstmrTag
*-- Set Code
IF lcParm = 'S' 
  
  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
  SELECT INVHDR
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
ELSE  && Reset code
  SELECT CUSTOMER
  
  *B603739,1 BWA 07/31/2000 Fix the bug of getting 'Table is not ordered' when validate the account.[START]
  *SET ORDER TO lcCstmrTag
  SET ORDER TO CUSTOMER
  *B603739,1 [END]
    
  SELECT INVHDR
  SET RELATION OFF INTO CUSTOMER
  SET ORDER TO lcInvHdTag
ENDIF
*-- end of lfInvSet.
*!*************************************************************
*! Name      : lpClose
*! Developer : Walid abou el-magd (WAM)
*! Date      : 01/28/1998
*! Purpose   : Procedure to close the temp file and its Associated files
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpClose
USE IN  (lcWorkFile)
ERASE   (gcWorkDir+lcWorkFile+".DBF")
ERASE   (gcWorkDir+lcWorkFile+".CDX")
IF FILE (gcWorkDir+lcWorkFile+".FPT")
  ERASE (gcWorkDir+lcWorkFile+".FPT")
ENDIF  

