*:***************************************************************************
*: Program file  : ARHIST.PRG
*: Program desc. : Print customer history report
*: For Report    : ARHISTA.FRX & ARHISTB.FRX
*! Date          : 08/04/1999
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Account Receivable (AR)
*: Developer     : Ashraf Medhat With review of AHMED MOHAMMED MOHAMMED
*: Reeference    : *E301328,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARHIST
*:***************************************************************************
*: Modification :
*:B803004,1 WMA 07/10/2000 concatenate the two columns (Trancode & Desc) in one column (Trancode)
*:B803004,1                printing the description only in case of automatic generated code,
*:B803004,1                but in case of manually entered code we will print both the code 
*:B803004,1                and the description.
*:B605644,1 SSE 03/20/2002 Fix bug of printing full company name, it was printing 21 characters before.
*:B605644,1                All modifications is in FRX (ARHISTA , ARHISTB)
*:B121592,1 NNA 02/19/2004 Fix bug of printing all invoices (void & active) because there is no
*:B121592,1 NNA            option to not include void invoices so I added an option to the option Grid
*:B130043,1 MMR 11/30/2005 1)Fix bug of not printing records that have trancd='I' or 'R' in the FRX 
*:B130043,1 MMR            2)Fix bug of not printing Check amount details in form A in the FRX.
*:B130043,1 MMR 11/30/2005 3)Fix bug of printing voided invoices and credit memos in case voided option=N   
*:B132443,1 TMI 07/10/2006 fix a bug that the invhdr file and rethdr file are not open
*:B607852,1 TMI 12/04/2006 Fix a bug that the arhist report is very slow  (Ticket # T20061019.0005)
*:***************************************************************************

*-- Variable used to print the account one time per history ID
llAccPrtd = .F.

*-- Variable hold the date period which to be printed on the .FRX
lcDatPrd  = SPACE(0)
lnDatapos = ASCAN(laOGFxFlt,'ARHIST.HISTDATE')
*-- Check if The user entered a date and get the date period to be printed.
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  *-- The user whether entered the two date or the end date only.
  IF !EMPTY(STRTRAN(laOGFxFlt[lnDatapos,6],'|'))
    IF EMPTY(CTOD(LEFT(laOGFxFlt[lnDatapos,6],10)))
      lcDatPrd = "Period: To " + LEFT(laOGFxFlt[lnDatapos,6],10)
    ELSE
      lcDatPrd = "Period: " + LEFT(laOGFxFlt[lnDatapos,6],10) + " - "+RIGHT(laOGFxFlt[lnDatapos,6],10)
    ENDIF
  ENDIF
ENDIF
=lfCreatNdx()
*B132443,1  TMI [Start] open INVHDR,RETHDR files
IF !USED('INVHDR')
  =gfOpenFile(gcDataDir+'INVHDR','INVHDRA','SH')
ENDIF
IF !USED('RETHDR')
  =gfOpenFile(gcDataDir+'RETHDR','RETHDRA','SH')
ENDIF
*B132443,1  TMI [End  ] 

*B130043,1 MMR 11/30/2004 3)Fix bug of printing voided invoices and credit memos in case voided option=N 
IF lcRpList = 'N'
  lcAlias=ALIAS()
  SELECT ARHIST
  SET RELATION TO ACCOUNT+TRAN INTO INVHDR  ADDITIVE
  SET RELATION TO ACCOUNT+TRAN  INTO RETHDR  ADDITIVE
  SELECT (lcAlias) 
ENDIF  

*B607852,1 TMI [Start] Add this extra filter after collecting data into the temp cursor lcTmpMaxN
*lcRpExp=lcRpExp + IIF(lcRpList = 'Y',''," AND INVHDR.STATUS<>'V' AND RETHDR.STATUS<>'V'")
*B607852,1 TMI [End  ] 

*B130043,1 MMR[End]
IF llOGFltCh
  IF lcRpFrmt = 'A'
    *-- Get the maximum transaction # in each selected history number less than the chargeback (8)
    *B130043,1 MMR 11/30/2004 3)Fix bug of printing voided invoices and credit memos in case voided option=N   
    *B121592,1 NNA 02/19/2004 (Begin) Filter According to the Requested type of invoices
	*lcRpExp=lcRpExp + IIF(lcRpList = 'Y',''," AND ARHIST.TRANTYPE<>'I' AND ARHIST.TRANTYPE<>'R'")
    *B121592,1 NNA (End)
    *B607852,1 TMI [Start] remove this select statement 
    *IF lcRpList = 'N'
    *  SELECT ARHIST.ACCOUNT,ARHIST.HISTORY,MAX(VAL(ARHIST.trantype)) AS NMAXTR WHERE VAL(ARHIST.trantype)<8 .AND. &lcRpExp ;
    *     FROM ARHIST,INVHDR,RETHDR GROUP BY ARHIST.history INTO CURSOR (lcTmpMaxN)
    *ELSE
    *B607852,1 TMI [End  ] 
    *B130043,1 MMR.[End]
    
    SELECT ACCOUNT,HISTORY,MAX(VAL(trantype)) AS NMAXTR WHERE VAL(trantype)<8 .AND. &lcRpExp ;
         FROM ARHIST GROUP BY history INTO CURSOR (lcTmpMaxN)
    
    *B130043,1 MMR 11/30/2004 3)Fix bug of printing voided invoices and credit memos in case voided option=N   
    *B607852,1 TMI [Start] remove the endif 
    *ENDIF
    *B607852,1 TMI [End  ] 
    *B130043,1 MMR.[END]

    SELECT (lcTmpMaxN)
    INDEX ON ACCOUNT+HISTORY TAG (lcTmpMaxN)
  ENDIF
ENDIF
SET ORDER TO TAG Ccode_no IN CODES
SELECT ARHIST
*-- set relation to get the related field ALLOW_TYPE directly from file
*B130043,1 MMR 11/30/2004 3)Fix bug of printing voided invoices and credit memos in case voided option=N 
*SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER,;
*   'N'+IIF(TRANTYPE='7','CCREDITCOD',IIF(TRANTYPE='2','TRANCODE  ',''))+arhist.trancode+SPACE(30)+'ALLOW_TYPE' INTO CODES
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER,;
   'N'+IIF(TRANTYPE='7','CCREDITCOD',IIF(TRANTYPE='2','TRANCODE  ',''))+arhist.trancode+SPACE(30)+'ALLOW_TYPE' INTO CODES ADDITIVE
*B130043,1 MMR.[END]
   
IF lcRpFrmt='A'
  SET RELATION TO ACCOUNT+HISTORY INTO (lcTmpMaxN)  ADDITIVE
ENDIF

*B607852,1 TMI [Start] add criteria to not to show void transactions 
IF lcRpList = 'N'
  lcRpExp = lcRpExp + " AND INVHDR.STATUS<>'V' AND RETHDR.STATUS<>'V'"
ENDIF  
*B607852,1 TMI [End  ] 

GO TOP


IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
ELSE
  *DO gfDispRe WITH EVAL('lcRpFormNa'),'FOR '+ IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
  =gfDispRe(lcRpFormNa,'FOR '+ IIF(EMPTY(lcRpExp) , .T. , lcRpExp))
ENDIF
SELECT ARHIST
SET RELATION TO
*-- end of main code.

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : When OG Fuction.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
FUNCTION lfwGrid

=lfvFormat()
*-- end of lfwGrid.

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Report format validation
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
FUNCTION lfvFormat
*-- lcRpForm = (D or S) Details/Summary
*-- lcRpFrmt = (A or B)   
PRIVATE lnFormPos
lnFormPos = ASCAN(laOGObjType,'lcRpForm')
IF lnFormPos > 0
  *-- If Enable the Detail/Summary setting if form B only.
  lnFormPos = ASUBSCRIPT(laOGObjType,lnFormPos,1)
  laOGObjCnt[lnFormPos] = (lcRpFrmt = "B")
  =lfOGShowGet('lcRpForm')
ENDIF
lcRpFormNa = "ARHIST" + lcRpFrmt
= lfRepPltFr(lcRpFormNa)
*-- end of lfvFormat.
*
FUNCTION lfSetAcct
PARAMETERS lcParm
SET ORDER TO CUSTOMER IN CUSTOMER
GO TOP IN CUSTOMER

*-- end of lfSetAcct.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Close files when closing the option grid
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************

FUNCTION lfClearRep
IF FILE(gcWorkDir +lcArHist+ '.CDX') 
  SELECT ARHIST
  CLOSE INDEX
  ERASE (gcWorkDir +lcArHist+ '.CDX')
ENDIF
*-- Close the cursor if any
IF USED(lcTmpMaxN)
  USE IN (lcTmpMaxN)
ENDIF

*!*************************************************************
*! Name      : lfCreatNdx
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Create a custom index on the ARHIST file
*!*************************************************************
*! Called from : ARHIST.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatNdx()
*!*************************************************************
FUNCTION lfCreatNdx
SELECT ARHIST
*-- If index file is already created 
IF !FILE(gcWorkDir +lcArHist+ '.CDX') 
  INDEX ON ACCOUNT+HISTORY+TRANTYPE+TRANCODE TAG cAHTTTag OF (gcWorkDir +  lcArHist + '.CDX')
ELSE
  SET ORDER TO TAG cAHTTTag OF (gcWorkDir +  lcArHist + '.CDX')
ENDIF

*!*************************************************************
*! Name      : lfAccPrtd
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 08/04/1999
*! Purpose   : to Print the customer one time per history ID
*!*************************************************************
*! Called from : ARHISTA.FRX, ARHISTB.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAccPrtd()
*!*************************************************************
FUNCTION lfAccPrtd
PARAMETERS lcCallFrm
llAccPrtd = (lcCallFrm = 'D')


*B803004,1 WMA Function to return the Code Desc. [Begin]    
*!*************************************************************
*! Name      : lfGetCodDes
*! Developer : 
*! Date      : 
*! Purpose   : 
*!*************************************************************
*! Called from : ARHISTA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : lcTranCode
*!*************************************************************
*! Return      : lcRetVal
*!*************************************************************
*! Example     : = lfGetCodDes()
*!*************************************************************
FUNCTION lfGetCodDes
PARAMETERS lcTranCode
PRIVATE lcRetVal

lcRetVal = ''
DO CASE
  CASE TranType $ "579"
    lcRetVal = gfCodDes(lcTranCode,'CCREDITCOD',.T.)
  CASE TranType $ "28"
    lcRetVal = gfCodDes(lcTranCode,'TRANCODE',.T.)
  CASE TranType = '4'
    lcRetVal = ALLTRIM(Store)+IIF(!EMPTY(Store),'-','')+Desc 
  OTHERWISE
    lcRetVal = Desc
ENDCASE

   
RETURN lcRetVal
*End of lfGetCodDes()
*B803004,1 WMA [End]    



*:**************************************************************************
*:* Name        : lfCollData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/03/2006
*:* Purpose     : Collect data
*:***************************************************************************
FUNCTION lfCollData
PRIVATE lnSlct
lnSlct = SELECT()

SELECT ARHIST
LOCATE

*- check the report results using the old select statement
SELECT ACCOUNT,HISTORY,MAX(VAL(trantype)) AS NMAXTR WHERE VAL(trantype)<8 .AND. &lcRpExp ;
         FROM ARHIST GROUP BY history INTO CURSOR (lcTmpMaxN)


SELECT (lnSlct)
*-- end of lfCollData.