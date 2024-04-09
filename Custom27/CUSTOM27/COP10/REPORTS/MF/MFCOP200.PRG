*:***************************************************************************
*: Program file  : MFCop200
*: Program desc. : OPEN WORK IN PROCESS REEPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : Mohamed Shokry
*: Reference     : *C101917
*: Notes         : 1- This reports is built from the A26 report (MFG950).
*:               : 2- The report skips 6 lines in the beginning of each page 
*:               :    as per the customer request
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrtRep() ,SRpt_Hdr() 
*:    Functions  : lfvOStatus(), lfwOGWhen(), lfvVend() ,lfSRVFab() ,lfFabSum()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFCOP200
*:***************************************************************************
*! Note     : 1- We replace est Qty with open qty accourding to nat mail .
*!               and add subtotal with contr1,2  
*!            2- we add tow filed to cuttkth file avccourding to nat. mail 
*!            3- add convert program to convert fileds data from 26 to 27 
*!               fileds [cCusCop,cOrdCop]
*:***************************************************************************
*: Modifications:
*:***************************************************************************
*-- VARIABLE PART     [Start]
R_TITLE   = 'OPEN WORK IN PROCESS REPORT'
XREPORT   = 'MFCOP200'
XTOT1     = 0                     && Variable To Hold Sub total
PAGENO    = 0
ROW       = 99
XTIME     = TIME()
llVend    = .F.                   && var to check for vendor
SORTFIELD = ' '                   && Variable for sorting data 
BREAK     = ' '                   && Variable to get sort title
*--TOTALD                         && Variable to display sort by
*-- lcRpStatus                    && Variable to get Status[defined at syrepuvr]
*--lnRpSrtCd                      && Variable to get sort [defined at syrepuvr]
*--laFileStru                     && Array to create temp file for codes [defined at syrepuvr]
*--lcCount                        && Variable counter for array laFileStru
*-- VARIABLE PART     [End]

*-- Create Temp File To Store Operation Code and contractor code 
lcTmpOPH = GFTEMPNAME()

CREATE CURSOR (lcTmpOPH) (CTKTNO C(6), cOperSeq C(2),cOprCode C(6),cContCode C(8) )
INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH

*-- Temp File to Get All needed data
WORKFILE  = GFTEMPNAME()

SELECT CUTTKTH
SET RELATION TO STYLE INTO STYLE
lcRpExp = IIF(EMPTY(lcRpStatus),lcRpExp+".AND. STATUS $ 'AOHS' " , lcRpExp+".AND. STATUS $ lcRpStatus")
LOCATE ALL FOR &lcRpExp

IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*-- set sorting
DO CASE
  CASE lnRpSrtCd = 1
    SORTFIELD = 'CUTTKT'

  CASE lnRpSrtCd = 2
    SORTFIELD = 'CONTR1+CUTTKT'
    BREAK     = 'CONTR1'
    TOTALD    = 'FIRST CONTRACTOR + CUTTKT #'

  CASE lnRpSrtCd = 3
    SORTFIELD = 'PATTERN+CUTTKT'
    BREAK     = 'PATTERN'
    TOTALD    = 'PATTERN '

  CASE lnRpSrtCd = 4
    SORTFIELD = 'CONTR2+DTOS(COMPLETE)+CUTTKT'
    BREAK     = 'CONTR2'
    TOTALD    = 'SECOND CONTRACTOR + COMPLETION DATE'

  CASE lnRpSrtCd = 5
    SORTFIELD = 'CONTR2+CUTTKT'
    BREAK     = 'CONTR2'
    TOTALD    = 'SECOND CONTRACTOR + CUTTKT #'

  CASE lnRpSrtCd = 6
    SORTFIELD = 'CONTR1+STATUS+CUTTKT'
    BREAK     = 'CONTR1+STATUS'
    TOTALD    = 'FIRST CONTRACTOR + STATUS'

ENDCASE

SELECT CUTTKTH
=AFIELDS(laFileStru)
FOR lnCount=1 TO 3
  lcCount = STR(lnCount,1)
  lnPos   = ASCAN(laFileStru,'MFG_OPR'+lcCount,1)
  
  *-- If MFG_OPR1,MFG_OPR2,MFG_OPR3 fields removed from the file for any 
  *-- reason, create them in the temporary FILE
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'MFG_OPR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 6
    laFileStru[lnFileStru,4] = 0
  ELSE
    laFileStru[lnPos+2] = 6
  ENDIF

  lnPos   = ASCAN(laFileStru,'CONTR'+lcCount,1)
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'CONTR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 8
    laFileStru[lnFileStru,4] = 0
  ENDIF
ENDFOR

Create Table (gcWorkDir+WORKFILE) FROM ARRAY laFileStru
INDEX ON &SORTFIELD TAG &WORKFILE

*-- Colletct Data (MHM)
SELECT CUTTKTH
SCAN FOR &lcRpExp
  llVend = .F.
  *--Get first 3 operations in sequence applied to the CUTTKT, and get their 
  *--contractors
  IF SEEK('M'+CUTTKTH.CUTTKT,'MFGOPRHD')
    SELECT (lcTmpOPH)
    ZAP  
    SELECT MFGOPRHD
    *-- Put all operations in this cuttkt cost sheet in the temprorary file
    SCAN WHILE CtktNo = CUTTKTH.CUTTKT
      SCATTER FIELDS CTKTNO,cOperSeq,cOprCode,cContCode MEMVAR
      INSERT INTO (lcTmpOPH) FROM MEMVAR
    ENDSCAN

    STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3

    SELECT (lcTmpOPH)
    GO TOP
    IF !EOF()
      lnCount = 1
      *-- Operations is sorted by sequence number, so get first 3 of them 
      *-- to put in temporary file of CUTTKTH
      SCAN
        lcCOUNT = STR(lnCount,1)
        *-- If contractor of any operation match the required contractor set the
        *-- flag to add this cuttkt to the report data.
        llVend  = IIF(!llVend .AND. !EMPTY(lcRpVend),lcRpVend = cContCode,llVend)
        IF lnCount <= 3 
          m.CONTR&lcCount   = cContCode
          m.MFG_OPR&lcCount = COPRCODE
        ENDIF
        lnCount = lnCount+1
      ENDSCAN
    ENDIF
  ELSE
    STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                      m.MFG_OPR2, m.MFG_OPR3
  ENDIF
  IF EMPTY(lcRpVend) .OR. llVend
    SELECT CUTTKTH
    SCATTER MEMVAR MEMO
    INSERT INTO (WORKFILE) FROM MEMVAR
  ENDIF
ENDSCAN
SET RELATION TO
SELECT (WORKFILE)
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SET RELATION TO STYLE INTO STYLE
GOTO TOP

HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

SET DEVICE TO PRINT

SELE &WORKFILE
DO lpPrtRep

DO ENDREPORT       && END THE REPORT OR DISPLAY ON SCREEN

SET DEVICE TO SCREEN
*-- End MFCop200
*!*************************************************************
*! Name      : lpPrtRep
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! Purpose   : OPEN WORK IN PROCESS REEPORT 
*!*************************************************************
*! Called from : MFCOP200.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : DO lpPrtRep
*!*************************************************************
*!*************************************************************
PROCEDURE lpPrtRep
STORE 0 TO XTOT1 , XSUT_PCS
XTOT_PCS=0
DO WHILE INKEY() <>32
  IF ROW >=61
    PAGENO = PAGENO+1
    IF BREAK <> ' '
      XSORTBY = 'TOTAL :'+TOTALD
    ELSE
      XSORTBY='  '
    ENDIF

    DO SRPT_HDR WITH XREPORT,XTITLE,R_WIDTH,XSORTBY
    @ 12,00 SAY 'ACT DATE    STYLE              CTKT#   OPN QTY    CUSTOMER        COMPDATE'

    @ 13,00 SAY REPLICATE('=',80)
    ROW =14
  ENDIF
  *-- we add this part accourding to nat. mail to calculate subtotal for cont1,cont2 [start]
  DO WHILE !EMPTY(BREAK) .AND. INLIST(lnRpSrtCd,4,5)
    IF &BREAK = HBREAK
      XSUT_PCS=XSUT_PCS+XTOT_PCS                   && To get total open qty    
      EXIT
    ENDIF
    XSUT_PCS=XSUT_PCS+XTOT_PCS  
    @ ROW,00 SAY REPLICATE('=',80)
    ROW = ROW+ 1
    DO CASE
      CASE lnRpSrtCd = 4
        @ ROW,01 SAY '**** SUB TOTAL ****  '+HBREAK
      CASE lnRpSrtCd = 5
        @ ROW,01 SAY '**** SUB TOTAL ****  '+HBREAK
    ENDCASE
    @ ROW,39 SAY XSUT_PCS PICTURE '9999999'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',80)
    ROW = ROW+1
    ******************** INITIALIZE SUBTOTALS ************
    HBREAK = &BREAK
    XSUT_PCS = 0
    EXIT
  ENDDO
  *- we add this part accourding to nat. mail to calculate subtotal for cont1,cont2 [end]


  **** GET EST UNITS *******
  SELE &WORKFILE
  XTOT_PCS=PCS_OPN                    && To get total open qty

  IF EOF()
    @ ROW+1,00 SAY REPLICATE('-',80)
    @ ROW+2,00 SAY "TOTAL :"
    @ ROW+2,39 SAY XTOT1 PICTURE '9999999'
    @ ROW+3,00 SAY REPLICATE('-',80)
    EXIT
  ENDIF

  IF ROW >= 57
    @ ROW+1,00 SAY REPLICATE('-',80)
    @ ROW+2,00 SAY "TOTAL :"
    @ ROW+2,39 SAY XTOT1 PICTURE '9999999'
    @ ROW+3,00 SAY REPLICATE('-',80)
    * Reinitilizate the total variable for a new page 
    XTOT1  = 0 
    ROW = 99
    LOOP
  ENDIF

  SELE &WORKFILE
  @ ROW,00 SAY ACT_DATE
  @ ROW,12 SAY STYLE
  @ ROW,29 SAY CUTTKT
  @ ROW,39 SAY PCS_OPN PICTURE '9999999' 
  *-- add customer account according to nat. mail[start]
  @ ROW,50 SAY IIF(EMPTY(CCUSCOP),'CUT TO STOCK',CCUSCOP)   
  *-- add customer account according to nat. mail[end]
  @ ROW,66 SAY COMPLETE

  *** ADD THE SUBTOTALS TOTALS ***
  XTOT1=XTOT1+XTOT_PCS
  ROW=ROW+1

  SELECT &WORKFILE
  SKIP
ENDDO
*--End lpPrtRep
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! Purpose   : Valid function of the status button
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : =lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ''
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Closed'  ,'S',;
                            IIF(laRpTarget[lnI] = 'Actual'  ,'A',''))))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwOGWhen
DECLARE laRpSource[4],laRpTarget[1]

lnStatus = lcRpStatus
STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Closed'   TO laRpSource[3]
STORE 'Actual'   TO laRpSource[4]

STORE SPACE(0) TO lcRpStatus, laRpTarget

*--End lfwRepWhen
*!*************************************************************
*! Name      : lfvVend
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVend) .AND. ;
   ('?' $ lcRPVend .OR. !SEEK(lcRPVend , 'APVENDOR'))
  =gfApVnBrow(@lcRPVend)
ENDIF
*--End lfAdjSeg
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!Procedures : ....
*!Functions  : gfModalGen
*!****************************************************************************
*! Called from : Option Grid
*!****************************************************************************
*! Passed Parameters  : [ S => Set code in in range at option grid and call 
*!                             lfSRVFab
*!                    :   R => Reset code in in range at option grid and call 
*!                             lfSRVFab 
*!                    :   V => Valid  code
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example   : =lfSRVFab()
*!****************************************************************************
*! Note      : 
*!****************************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*********************************************************************
*! PROC      : SRpt_Hdr
*! DESC      : PRINT REPORT HEADER
*! Developer : Mohamed Shokry
*! Date      : 07/04/2000
*! NOTES     : This function is a copy from the global function RPT_HDR but 
*!             we added an extra line to print the total.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Modifications :
*!********************************************************************

PROCEDURE SRpt_Hdr
PARAMETER XPROG,XRPTNAME,XTYPE, XSBY

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
R_TITLE  = TRIM(R_TITLE)
X1 = ((80 - (LEN(TRIM(QCOMPANY))))/2)
X2 = ((80 - (LEN( R_TITLE  )))/2)
X3 = ((80 - (LEN( XRPTNAME )))/2)
IF  XTYPE='N'
  @ 07,000 SAY XPROG
  @ 07,X1  SAY QCOMPANY
  @ 07,67 SAY DATE()
  @ 07,76 SAY '~'   &&TAK 06/05/94
  @ 08,000 SAY TIME()
  @ 08,X2  SAY R_TITLE
  @ 08,67 SAY 'PAGE#'
  @ 08,73 SAY STR(PAGENO,4)
  @ 09,X3 SAY XRPTNAME
  @ 10,00 SAY XSBY
  @ 11,00 SAY REPLICATE('*',80)
ENDIF