*:************************************************************************
*: Program file  : SORQAPR.Prg
*: Program desc. : PRINT REQUEST FOR ORDER APPROVAL
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 12/19/98
*: Reference     : *E301102,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfSRVOrd()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SORQAPR
*:************************************************************************
*: Modifications:
*:B802092,1 IHB 04/01/1999 Fix the bug of "Variable HO not found"
*:B802343,1 AMM 06/16/1999 Fix the Bug of 'Syntax error' in the Order# in range function
*:B802381,1 AMM 07/17/1999 Print bill to addresses instead of ship to ones.
*:E500271,4 SSE   07/14/1999 Add popup in OG to filter for the flag field named
*:E500271,4                  LEDIORDER which indicate whether these Orders is coming
*:E500271,4                  from EDI Module or not 
*:B802472,1 RAM 07/29/99 Fix the bug of incermental search in Order# in range 
*:B802472,1              was not working in SYREPUVR file
*B603506,1 ABD 06/12/2000 Increase amount sizes to fit currencies.
*B603506,1 ABD            [BookAmt,ShipAmt,CancelAmt,Openamt]
*B603375,1 MHM 10/04/2000 Fix some bugs 
*B603375,1 MHM            1)Coustomer address feilds need to be shifted to left by 4 digit.
*B603375,1 MHM            2) When you select to Sort by Account, Order# in list comes empty.
*B603375,1 MHM            3)Improve performance of browsing orders
*B603713,11 MHM 11/19/2000 Increase the length of the fields 
*E302203,1 ABD 07/22/2003 Add The Start Ship Date On The Request For Approval Form.
*:************************************************************************
*-- Initialize some variables
A       = REPLICATE('-',80)
ROW     = 99
PAGENO  = 0
R_TITLE = 'REQUEST FOR CREDIT APPROVAL'
*B603506,1 ABD- Increase amount sizes to fit currencies & Change the layout. [Begin]
*L1      = 'ORDER             COMPLETE PURCHASE       ORDER        NET  APPROVED   APPROVAL'
*L2      = 'NUMBER S RECEIVED DATE     ORDER#        AMOUNT     AMOUNT    AMOUNT       CODE'
L1      = 'ORDER    PURCHASE                ORDER           NET      APPROVED   APPROVAL'
L2      = 'NUMBER S ORDER#                 AMOUNT        AMOUNT        AMOUNT       CODE'
*B603506,1 ABD  [End]
R_WIDTH = 'N'
XTOTAMT = 0

lcPhonPict  = gfPhoneTem()          && Variable to hold the Phone Format
*B802092,1 Adjust filter expression [start]
*lcRpExp = lcRpExp + " .AND. OPENAMT > 0 .AND. STATUS $ "+ IIF(lcRpStat='A',"HO","'"+lcRpStat+"'")
lcRpExp = lcRpExp + " .AND. OPENAMT > 0 .AND. STATUS $ "+ IIF(lcRpStat='A',"'HO'","'"+lcRpStat+"'")
*B802092,1 IHB [end]

*E500271,4 add Field LEDIORDER to the filter expression [Begin.]
IF !EMPTY(lcRpEdiFlt)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcRpEdiFlt
ENDIF
*E500271,4 add Field LEDIORDER to the filter expression [End.]

SELECT ORDHDR
LOCATE ALL FOR &lcRpExp
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SELECT ORDHDR
SET ORDER TO TAG IIF(lcRpSort = 'O',"ORDHDR","ORDACCT") IN ORDHDR
SET FILTER TO &lcRpExp

SET DEVICE TO PRINT
SELECT ORDHDR
SET RELATION TO IIF(STORE=SPACE(8) ,;
        'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

SCAN
   XACCOUNT = ACCOUNT
   XORDER   = ORDER
   XTOTAMT  = XTOTAMT + BOOKAMT

   IF ROW > 55
     PAGENO = PAGENO + 1
     DO RPT_HDR WITH 'SORQAPR',XTITLE,'N'
     ROW    = 5
   ENDIF

   IF !EOF('CUSTOMER')
     XBTNAME  = CUSTOMER.BTNAME
     *B802381,1 AMM Get bill to addresses instead of ship to ones
     *XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
     *XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
     *XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
     
     XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '', 1,'2')
     XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '', 2,'2')
     XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '', 3,'2')
     *B802381,1 AMM end

     IF LEN(TRIM(XBTADDR2)) = 0
       XBTADDR2 = XBTADDR3
       XBTADDR3 = ''
     ENDIF
   ELSE
     XBTNAME  = ''
     XBTADDR1 = ''
     XBTADDR2 = ''
     XBTADDR3 = ''
   ENDIF

   XTER_DES = gfCodDes(ORDHDR.CTERMCODE, 'CTERMCODE')
*.........+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*123456789012345678901234567890123456789012345678901234567890123456789012345678901234
*1ACCT  : 12345           FACT.#:1234567890           123456789012345678901234567890
*2PHONE : (123) 123-1234  TERMS :12345678901234567890 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*3DUNS# :                                             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*4RATNG.:                                             XXXXXXXXXXXXXXX XXX XXXXXXXXXX
*5                                                                                  
*6ORDER              COMPLETE PURCHASE      ORDER         NET  APPROVED  APPROVAL
*7NUMBER S RECEIVED  DATE     ORDER#        AMOUNT     AMOUNT    AMOUNT      CODE
*8123456 X MM/DD/YY  MM/DD/YY 1234567890 123456.99  123456.99 123456.99 123456789
*9
*1NOTES:

  *LINE1
  @ ROW,01 SAY 'ACCT  :'
  @ ROW,09 SAY ACCOUNT
  @ ROW,26 SAY 'FACT.#:'
  @ ROW,31 SAY CUSTOMER.FACTACCT

  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[start]
  *@ ROW,54 SAY XBTNAME
  @ ROW,54 SAY LEFT(XBTNAME,26)
  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[end]

  ROW = ROW + 1

  *LINE2
  @ ROW,01 SAY 'PHONE :'
  @ ROW,09 SAY PHONE  PICTURE lcPhonPict
  @ ROW,26 SAY 'TERMS :'
  *B802381,1 AMM Adjust to fit in the report
  *@ ROW,31 SAY LEFT(XTER_des,21)
  @ ROW,31 SAY LEFT(XTER_des,19)
  *B802381,1 AMM end
  
  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[start]
  *@ ROW,54 SAY XBTADDR1
  @ ROW,54 SAY LEFT(XBTADDR1,26)
  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[End]
  
  ROW = ROW + 1

  *LINE3
  @ ROW,01 SAY 'DUNS# :'
  @ ROW,09 SAY CUSTOMER.DUNS
  
  *B603506,1 ABD- Increase amount sizes to fit currencies & Change the layout. [Begin]
  @ ROW,26 SAY 'RATNG.:'
  @ ROW,33 SAY CUSTOMER.DUNSRTG
  *B603506,1 ABD  [End]

  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[Start]
  *@ ROW,54 SAY XBTADDR2
  @ ROW,54 SAY LEFT(XBTADDR2,26)
  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[End]

  ROW = ROW + 1

  *LINE4
  *B603506,1 ABD- Increase amount sizes to fit currencies & Change 
  *B603506,1 ABD- The layout & print them up. [Begin]
  *@ ROW,01 SAY 'RATNG.:'
  *@ ROW,09 SAY CUSTOMER.DUNSRTG

  *E302203,1 ABD - Remark the next few lines and Add The Start Ship Date 
  *E302203,1 ABD - On The Request For Approval Form.
  *@ ROW,01 SAY 'ENTERED :'
  *@ ROW,11 SAY ENTERED
  *@ ROW,23 SAY 'COMPLETE :'
  *@ ROW,33 SAY COMPLETE

   @ ROW,01 SAY 'ENTERED:'
   @ ROW,09 SAY ENTERED
   @ ROW,19 SAY 'START:'
   @ ROW,25 SAY START
   @ ROW,35 SAY 'COMPLETE:'
   @ ROW,44 SAY COMPLETE
   *E302203,1 ABD - [End] 

  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[Start]
  *@ ROW,54 SAY XBTADDR3
  @ ROW,54 SAY LEFT(XBTADDR3,26)
  *B603375,1 MHM 1)Coustomer address feilds shifted to left by 4 digit.[End]

  *B603506,1 ABD [End]
  ROW = ROW+1

  *LINE 5
  @ ROW,01 SAY L1
  ROW = ROW + 1

  *LINE 6
  @ ROW,01 SAY L2
  ROW = ROW + 1

  *LINE 7
  @ ROW,01 SAY ORDER
  @ ROW,08 SAY STATUS
  *B603506,1 ABD  Remark next lines and print them up one line.[Begin]
  *@ ROW,10 SAY ENTERED
  *@ ROW,19 SAY COMPLETE
  *@ ROW,28 SAY IIF(MULTIPO,'*MULTI-PO*',LEFT(CUSTPO,10))
  *B603506,1 ABD [End]
  
  *B603506,1 ABD Remark the next lines and increace the Picture.[Begin]
  *@ ROW,38 SAY OPENAMT PICTURE '9999999.99'                      &&MFM 01/31/94
  *@ ROW,49 SAY OPENAMT * ( 1 - (DISC/100) ) PICTURE '9999999.99' &&MFM 01/31/94
  *@ ROW,60 SAY APPRAMT   PICTURE '999999.99'
  *@ ROW,70 SAY APPROVAL

  *:B603713,11  MHM 10/17/2000 [start]
  *@ ROW,10 SAY IIF(MULTIPO,'*MULTI-PO*',CUSTPO)
  *@ ROW,26 SAY OPENAMT PICTURE '9999999999.99'
  *@ ROW,40 SAY OPENAMT * ( 1 - (DISC/100) ) PICTURE '9999999999.99' 
  *@ ROW,54 SAY APPRAMT   PICTURE '9999999999.99'
  *@ ROW,68 SAY APPROVAL
  IF lcRpDeciml = "Y"
    @ ROW,10 SAY IIF(MULTIPO,'*MULTI-PO*',CUSTPO)
    @ ROW,26 SAY OPENAMT PICTURE '9999999999.99'
    @ ROW,40 SAY OPENAMT * ( 1 - (DISC/100) ) PICTURE '9999999999.99' 
    @ ROW,54 SAY APPRAMT   PICTURE '9999999999.99'
    @ ROW,68 SAY APPROVAL
  ELSE
    @ ROW,10 SAY IIF(MULTIPO,'*MULTI-PO*',CUSTPO)
    @ ROW,26 SAY OPENAMT PICTURE '9999999999999'
    @ ROW,40 SAY OPENAMT * ( 1 - (DISC/100) ) PICTURE '9999999999999' 
    @ ROW,54 SAY APPRAMT   PICTURE '9999999999999'
    @ ROW,68 SAY APPROVAL
  ENDIF  
  *:B603713,11  MHM 10/17/2000 [END]
  *B603506,1 ABD [End]

  ROW = ROW + 1

  * LINE 8
  @ ROW,01 SAY 'NOTES:'
  @ ROW,09 SAY NOTE1+' '+NOTE2
  ROW = ROW + 1

  * LINE 9
  @ ROW,00 SAY A
  ROW = ROW + 1

ENDSCAN
DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN


*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/19/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen


*!*************************************************************
*! Name      : lfSRVOrd
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/19/98
*! Purpose   : control browsing Orders.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVOrd()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVOrd
PARAMETERS lcParam

DO CASE
  CASE lcParam = 'S'
    SET ORDER TO CUSTOMER IN CUSTOMER
    SELECT ORDHDR
    *B603375,1 MHM  When you select to Sort by Account, Order# in list comes empty.[start]
    *SET ORDER TO TAG IIF(lcRpSort = 'O',"ORDHDR","ORDACCT")
    SET ORDER TO TAG "ORDHDR"
    *B603375,1 MHM  When you select to Sort by Account, Order# in list comes empty.[end]
    SET RELATION TO IIF(STORE=SPACE(8) ,;
        'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
    *B603375,1 MHM  Improve performance of browsing orders.[start]
    *we remove the line ('O' FOR IIF(lcRpStat='A', STATUS $"HO",Status=lcRpStat).AND.(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)))
    * from syrepuvr Order# field in mbrwfltexp and expand it here 
    DO CASE
      CASE lcRpStat = 'A'
          SET FILTER TO (CORDTYPE + ORDER = 'O') AND (STATUS $ "HO");
          .AND. (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt))
      CASE lcRpStat = 'H'
          SET FILTER TO (CORDTYPE + ORDER = 'O') AND  (Status = "H");
          .AND. (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt))
      CASE lcRpStat = 'O'
          SET FILTER TO (CORDTYPE + ORDER = 'O') AND (Status = "O");
          .AND. (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt))
    ENDCASE
    *B603375,1 MHM  Improve performance of browsing.[End]
  CASE lcParam = 'R'
    SELECT ORDHDR
    SET RELATION TO
    llClearOrd = .F.
ENDCASE

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.
