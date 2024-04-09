*:***************************************************************************
*: Program file  : MFCop200
*: Program desc. : OPEN WORK IN PROCESS REEPORT
*: System        : Aria Advantage Series VER. 4xp
*: Module        : MF
*: Developer     : Mariam Mazhar 
*: Reference     : *C101917
*: Notes         : 1- This reports is built from the A26 report (MFG950).
*:               : 2- The report skips 6 lines in the beginning of each page 
*:               :    as per the customer request
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
DIMENSION loOgScroll.laCRParams[3,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'OPEN WORK IN PROCESS REPORT'

loOgScroll.laCRParams[2,1] = 'lnRpSrtCd'
loOgScroll.laCRParams[2,2] = lnRpSrtCd

loOgScroll.laCRParams[3,1] = 'OpTitle'
loOgScroll.laCRParams[3,2] = XTITLE

IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data......." NOWAIT 
  lfCrtTemp()
  lfCollect()
ENDIF 

SELECT(WORKFILE)
LOCATE
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
COPY TO oAriaApplication.WorkDir +  lcTemp + ".DBF"
DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTemp + ".DBF"
=gfDispRe()
RETURN




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
*!*	lcTmpOPH = GFTEMPNAME()

*!*	CREATE CURSOR (lcTmpOPH) (CTKTNO C(6), cOperSeq C(2),cOprCode C(6),cContCode C(8) )
*!*	INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH

*!*	*-- Temp File to Get All needed data
*!*	WORKFILE  = GFTEMPNAME()

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

= lfOGMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

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
IF !llFrstTime
  loDBFStyle    = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  loDBFPOSHDR  = CreateObject("RemoteTable",'POSHDR','POSHDR','POSHDR',SET("DATASESSION"))
  loDBFMFGOPRHD = CreateObject("RemoteTable","MFGOPRHD","MFGOPRHD",'MFGOPRHD',SET("DATASESSION"))

  lcSqlStat1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOGScroll.ORDA.SQLRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF 



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
*!*************************************************************
*! Name      : lfFabSum
*! Developer : MAriam Mazhar[MMT]
*! Date      : 06/11/2006
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
LPARAMETERS lcFab, lcComp
LOCAL lnTotcomp,  lnAlias
lnTotcomp = 0

  IF SEEK(ALLTRIM(lcFab), lcTmpFab)
    SUM &lcTmpFab..&lcCOMP. TO lnTotcomp WHILE &lcTmpFab..Fabric = lcFab
  ENDIF

RETURN lnTotcomp

*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/11/2006
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.

*!**************************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/10/2006
*! Purpose   : to Create temp. file
*!**************************************************************************
FUNCTION lfCrtTemp
DO CASE
  CASE lnRpSrtCd = 1
    SORTFIELD = 'PO'

  CASE lnRpSrtCd = 2
    SORTFIELD = 'CONTR1+PO'
    BREAK     = 'CONTR1'
    TOTALD    = 'FIRST CONTRACTOR + CUTTKT #'

  CASE lnRpSrtCd = 3
    SORTFIELD = 'PATTERN+PO'
    BREAK     = 'PATTERN'
    TOTALD    = 'PATTERN '

  CASE lnRpSrtCd = 4
    SORTFIELD = 'CONTR2+DTOS(COMPLETE)+PO'
    BREAK     = 'CONTR2'
    TOTALD    = 'SECOND CONTRACTOR + COMPLETION DATE'

  CASE lnRpSrtCd = 5
    SORTFIELD = 'CONTR2+PO'
    BREAK     = 'CONTR2'
    TOTALD    = 'SECOND CONTRACTOR + CUTTKT #'

  CASE lnRpSrtCd = 6
    SORTFIELD = 'CONTR1+STATUS+PO'
    BREAK     = 'CONTR1+STATUS'
    TOTALD    = 'FIRST CONTRACTOR + STATUS'

ENDCASE

DIMENSION laTempStru[4,4]

laTempStru = ''
laTempStru [1,1] = 'CTKTNO'
laTempStru [1,2] = 'C'
laTempStru [1,3] = 6
laTempStru [1,4] = 0

laTempStru [2,1] = 'cOperSeq'
laTempStru [2,2] = 'C'
laTempStru [2,3] = 2
laTempStru [2,4] = 0

laTempStru [3,1] = 'cOprCode'
laTempStru [3,2] = 'C'
laTempStru [3,3] = 6
laTempStru [3,4] = 0

laTempStru [4,1] = 'cContCode'
laTempStru [4,2] = 'C'
laTempStru [4,3] = 8
laTempStru [4,4] = 0

 = gfCrtTmp(lcTmpOPH,@laTempStru,"CTKTNO+cOperSeq+cOprCode" ,lcTmpOPH,.T.)

DIMENSION laFileStru[1,18]

laFileStru = ''
SELECT POSHDR
=AFIELDS(laFileStru)
FOR lnCount=1 TO 3
  lcCount = STR(lnCount,1)
  lnPos   = ASCAN(laFileStru,'MFG_OPR'+lcCount,1)
  *-- If MFG_OPR1,MFG_OPR2,MFG_OPR3 fields removed from the file for any 
  *-- reason, create them in the temporary FILE
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,18]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'MFG_OPR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 6
    laFileStru[lnFileStru,4] = 0
    STORE ' ' TO  laFileStru[lnFileStru,7] ,laFileStru[lnFileStru,8],;
                  laFileStru[lnFileStru,9] ,laFileStru[lnFileStru,10],;
                  laFileStru[lnFileStru,11],laFileStru[lnFileStru,12],;
                  laFileStru[lnFileStru,13],laFileStru[lnFileStru,14],;
                  laFileStru[lnFileStru,15],laFileStru[lnFileStru,16]
    STORE 0   TO  laFileStru[lnFileStru,17],laFileStru[lnFileStru,18]
  ELSE
    laFileStru[lnPos+2] = 6
  ENDIF

  lnPos   = ASCAN(laFileStru,'CONTR'+lcCount,1)
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,18]
    lnFileStru = lnFileStru + 1
    laFileStru[lnFileStru,1] = 'CONTR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 8
    laFileStru[lnFileStru,4] = 0
    STORE ' ' TO  laFileStru[lnFileStru,7] ,laFileStru[lnFileStru,8],;
                  laFileStru[lnFileStru,9] ,laFileStru[lnFileStru,10],;
                  laFileStru[lnFileStru,11],laFileStru[lnFileStru,12],;
                  laFileStru[lnFileStru,13],laFileStru[lnFileStru,14],;
                  laFileStru[lnFileStru,15],laFileStru[lnFileStru,16]
    STORE 0   TO  laFileStru[lnFileStru,17],laFileStru[lnFileStru,18]
  ENDIF
ENDFOR

 = gfCrtTmp(WORKFILE,@laFileStru,SORTFIELD ,WORKFILE,.T.)

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/10/2006
*! Purpose   : to Collect data
*!**************************************************************************
FUNCTION lfCollect

llUseContr = .F.
llUseDiv   = .F.
llUseGrp   = .F.
llUseStyle = .F.
llUsePO	   = .F.
llUseSea   = .F.
llUseFab   = .F.

lcContFile = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'MFGOPRHD.cContCode'),1)
IF lnPosition > 0
  lcContFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseContr = IIF(!EMPTY(lcContFile) .AND. USED(lcContFile) .AND. RECCOUNT(lcContFile)>0,.T.,.F.)
ENDIF
IF llUseContr
  SELECT (lcContFile)
  LOCATE 
  IF EOF()
    llUseContr = .F.
  ENDIF 
ENDIF   
*-- To get the selected Division if any.
lcDivs = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.CDIVISION'),1)
IF lnPosition > 0
  lcDivs 	= LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcDivFile = loogscroll.gfTempName()
  llUseDiv  = IIF(LEN(lcDivs)>0,.T.,.F.) and lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
ENDIF

*---Season Filter
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'POSHDR.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGFXFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

*--Style Fabric
lcFabFile = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUseFab = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF
IF llUseFab 
  SELECT(lcFabFile)
  LOCATE 
  IF EOF()
    llUseFab = .F.
  ENDIF 
ENDIF 

*STYLE.CSTYGROUP
lnGrpPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.CSTYGROUP'),1)
IF lnGrpPos > 0
  lcGrpStr = LOOGSCROLL.laOGFXFlt[lnGrpPos,6]
  lcGrpFile = loOGScroll.gfTempName()
  llUseGrp = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcGrpFile)
ENDIF
*--Style
lcStylFile = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llUseStyle 
  SELECT(lcStylFile)
  LOCATE 
  IF EOF()
    llUseStyle = .F.
  ENDIF 
ENDIF 
*
lcPOFile = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,"POSHDR.PO"),1)
IF lnPosition > 0
  lcPOFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUsePO = IIF(!EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. RECCOUNT(lcPOFile)>0,.T.,.F.)
ENDIF
IF llUsePO
  SELECT(lcPOFile)
  LOCATE 
  IF EOF()
    llUsePO = .F.
  ENDIF 
ENDIF 

ldCompDateS = {}
ldCompDateE = {}

ldStartDateS = {}
ldStartDateE = {}

*-- To get the Completion date.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'POSHDR.COMPLETE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGVRFlt[lnPosition,6])
 ldCompDateS = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosition,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosition,6],1,10))
 ldCompDateE = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosition,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosition,6],12,21))
ENDIF

*-- To get the Entered date.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'POSHDR.ENTERED'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGVRFlt[lnPosition,6])
 ldStartDateS = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosition,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosition,6],1,10))
 ldStartDateE = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosition,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosition,6],12,21))
ENDIF

llVend    = .F.                   && var to check for vendor

IF llUsePO
  SELECT(lcPOFile)
  SCAN
    IF loDBFPOSHDR.Seek('PU'+&lcPOFile..PO) AND ;
      IIF(llUseStyle,SEEK(POSHDR.Style,lcStylFile),.T.) AND ;
      IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) ;
      AND IIF(llUseSea,SEEK(POSHDR.SEASON,lcSeaFile),.T.) AND ;
      IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
      IIF(!EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE),BETWEEN(POSHDR.COMPLETE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.) AND;
      IIF(!EMPTY(ldStartDateS ) AND !EMPTY(ldStartDateE ),BETWEEN(POSHDR.ENTERED,CTOD(ldStartDateS),CTOD(ldStartDateE) ),.T.) AND ;
      IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
      AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
      IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) 
        llVend = .F.  
        IF loDBFMFGOPRHD.Seek('M'+&lcPOFile..PO) 
          SELECT (lcTmpOPH)
    		  ZAP  
          SELECT MFGOPRHD
          SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+&lcPOFile..PO
            SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
            INSERT INTO (lcTmpOPH) FROM MEMVAR
          ENDSCAN
	      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
   	    SELECT (lcTmpOPH)
    	  LOCATE 
		    IF !EOF()
		      lnCount = 1
		      *-- Operations is sorted by sequence number, so get first 3 of them 
		      *-- to put in temporary file of CUTTKTH
	        SCAN
     	      lcCOUNT = STR(lnCount,1)
	          *-- If contractor of any operation match the required contractor set the
       		  *-- flag to add this cuttkt to the report data.
	          llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
     IF !llUseContr .OR. llVend
		 SELECT POSHDR
		 SCATTER MEMVAR MEMO
		 INSERT INTO (WORKFILE) FROM MEMVAR
	   ENDIF    
    ENDIF 
  ENDSCAN 
ELSE
  IF llUseStyle
    SELECT (lcStylFile)
    SCAN 
    IF loDBFPOSHDR.Seek('0001'+&lcStylFile..cstymajor,'poshdrs') 
      SELECT POSHDR
      SCAN REST WHILE cinvtype+ style+ cbusdocu+ cstytype+po = '0001'+&lcStylFile..cstymajor FOR ;
         Cstytype = 'U' AND Cbusdocu = 'P' AND ;
	       IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) ;
	       AND IIF(llUseSea,SEEK(POSHDR.SEASON,lcSeaFile),.T.) AND ;
     	   IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
	       IIF(!EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE),BETWEEN(POSHDR.COMPLETE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.) AND;
     	   IIF(!EMPTY(ldStartDateS ) AND !EMPTY(ldStartDateE ),BETWEEN(POSHDR.ENTERED,CTOD(ldStartDateS),CTOD(ldStartDateE)),.T.) AND ;
  		   IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
	  	   AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
		     IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
         llVend = .F.
         IF loDBFMFGOPRHD.Seek('M'+POSHDR.PO) 
           SELECT (lcTmpOPH)
    		   ZAP  
           SELECT MFGOPRHD
           SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+POSHDR.PO
             SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
             INSERT INTO (lcTmpOPH) FROM MEMVAR
           ENDSCAN
  	       STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
   	       SELECT (lcTmpOPH)
      	   LOCATE 
  	  	   IF !EOF()
  		       lnCount = 1
  		       *-- Operations is sorted by sequence number, so get first 3 of them 
  		       *-- to put in temporary file of CUTTKTH
  	         SCAN
       	      lcCOUNT = STR(lnCount,1)
  	          *-- If contractor of any operation match the required contractor set the
         		  *-- flag to add this cuttkt to the report data.
  	          llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
           IF !llUseContr .OR. llVend
  	  	     SELECT POSHDR
        		 SCATTER MEMVAR MEMO
  	      	 INSERT INTO (WORKFILE) FROM MEMVAR
    	     ENDIF    
         ENDSCAN 
	     ENDIF 
     ENDSCAN 
   ELSE
     IF llUseDiv 
     SELECT(lcDivFile)
     SCAN 
     IF loDBFPOSHDR.Sqlrun("Select * from POSHDR(INDEX = POSHDR) where Cstytype = 'U' AND Cbusdocu = 'P'  AND POSHDR.CDIVISION = '"+&lcDivFile..CDIVISION+"'")
       SELECT POSHDR
       SCAN FOR IIF(llUseSea,SEEK(POSHDR.SEASON,lcSeaFile),.T.) AND ;
         IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
         IIF(!EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE),BETWEEN(POSHDR.COMPLETE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.) AND;
         IIF(!EMPTY(ldStartDateS ) AND !EMPTY(ldStartDateE ),BETWEEN(POSHDR.ENTERED,CTOD(ldStartDateS),CTOD(ldStartDateE) ),.T.) AND ;
         IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
         AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
         IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
         llVend = .F.
         IF loDBFMFGOPRHD.Seek('M'+POSHDR.PO) 
           SELECT (lcTmpOPH)
           ZAP  
           SELECT MFGOPRHD
           SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+POSHDR.PO
             SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
             INSERT INTO (lcTmpOPH) FROM MEMVAR
           ENDSCAN
           STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
            SELECT (lcTmpOPH)
           LOCATE 
           IF !EOF()
             lnCount = 1
             *-- Operations is sorted by sequence number, so get first 3 of them 
             *-- to put in temporary file of CUTTKTH
             SCAN
               lcCOUNT = STR(lnCount,1)
              *-- If contractor of any operation match the required contractor set the
               *-- flag to add this cuttkt to the report data.
              llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
           IF !llUseContr .OR. llVend
             SELECT POSHDR
             SCATTER MEMVAR MEMO
             INSERT INTO (WORKFILE) FROM MEMVAR
           ENDIF    
         ENDSCAN 
     ENDIF  
     ENDSCAN 
     ELSE
       IF llUseSea
         SELECT(lcSeaFile)
         SCAN 
         
           IF loDBFPOSHDR.Sqlrun("Select * from POSHDR(INDEX = POSHDR) where Cstytype = 'U' AND Cbusdocu = 'P' AND POSHDR.SEASON = '"+&lcSeaFile..SEASON+"'")
             SELECT POSHDR
             SCAN FOR IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
                      IIF(!EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE),BETWEEN(POSHDR.COMPLETE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.) AND;
                      IIF(!EMPTY(ldStartDateS ) AND !EMPTY(ldStartDateE ),BETWEEN(POSHDR.ENTERED,CTOD(ldStartDateS),CTOD(ldStartDateE) ),.T.) AND ;
                      IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
                      AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
                      IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
               llVend = .F.
               IF loDBFMFGOPRHD.Seek('M'+POSHDR.PO) 
                 SELECT (lcTmpOPH)
                 ZAP  
                 SELECT MFGOPRHD
                 SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+POSHDR.PO
                   SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
                   INSERT INTO (lcTmpOPH) FROM MEMVAR
                 ENDSCAN
                 STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
                 SELECT (lcTmpOPH)
                 LOCATE 
                 IF !EOF()
                   lnCount = 1
                   *-- Operations is sorted by sequence number, so get first 3 of them 
                   *-- to put in temporary file of CUTTKTH
                 SCAN
                   lcCOUNT = STR(lnCount,1)
                   *-- If contractor of any operation match the required contractor set the
                   *-- flag to add this cuttkt to the report data.
                   llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
             IF !llUseContr .OR. llVend
               SELECT POSHDR
               SCATTER MEMVAR MEMO
               INSERT INTO (WORKFILE) FROM MEMVAR
             ENDIF    
           ENDSCAN 
         ENDIF  
       ENDSCAN 
     ELSE
       IF !EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE)
         IF loDBFPOSHDR.Sqlrun("Select * from POSHDR(INDEX = POSHDR) WHERE Cstytype = 'U' AND Cbusdocu = 'P' "+;
               "AND POSHDR.COMPLETE Between '"+ldCompDateS+"' AND '"+ldCompDateE+"'")
             SELECT POSHDR
             SCAN FOR IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
               IIF(!EMPTY(ldStartDateS ) AND !EMPTY(ldStartDateE ),BETWEEN(POSHDR.ENTERED,CTOD(ldStartDateS),CTOD(ldStartDateE )),.T.) AND ;
               IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
               AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
               IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
               llVend = .F.
               IF loDBFMFGOPRHD.Seek('M'+POSHDR.PO) 
                 SELECT (lcTmpOPH)
                 ZAP  
                 SELECT MFGOPRHD
                 SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+POSHDR.PO
                   SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
                   INSERT INTO (lcTmpOPH) FROM MEMVAR
                 ENDSCAN
                 STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
                 SELECT (lcTmpOPH)
                 LOCATE 
                 IF !EOF()
                   lnCount = 1
                   *-- Operations is sorted by sequence number, so get first 3 of them 
                   *-- to put in temporary file of CUTTKTH
                   SCAN
                     lcCOUNT = STR(lnCount,1)
                      *-- If contractor of any operation match the required contractor set the
                       *-- flag to add this cuttkt to the report data.
                    llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
           IF !llUseContr .OR. llVend
             SELECT POSHDR
             SCATTER MEMVAR MEMO
             INSERT INTO (WORKFILE) FROM MEMVAR
           ENDIF    
           ENDSCAN 
           ENDIF  
         ELSE
           
           IF !EMPTY(ldStartDateS ) AND !EMPTY(ldStartDateE )
             IF loDBFPOSHDR.Sqlrun("Select * from POSHDR(INDEX = POSHDR) WHERE Cstytype = 'U' AND Cbusdocu = 'P' "+;
               "AND POSHDR.ENTERED Between '"+ldStartDateS+"' AND '"+ldStartDateE +"'")
             SELECT POSHDR
             SCAN FOR IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
               IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
               AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
               IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.)
               llVend = .F.
               IF loDBFMFGOPRHD.Seek('M'+POSHDR.PO) 
                 SELECT (lcTmpOPH)
                 ZAP  
                 SELECT MFGOPRHD
                 SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+POSHDR.PO
                   SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
                   INSERT INTO (lcTmpOPH) FROM MEMVAR
                 ENDSCAN
                 STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
                 SELECT (lcTmpOPH)
                 LOCATE 
                 IF !EOF()
                   lnCount = 1
                   *-- Operations is sorted by sequence number, so get first 3 of them 
                   *-- to put in temporary file of CUTTKTH
                   SCAN
                     lcCOUNT = STR(lnCount,1)
                      *-- If contractor of any operation match the required contractor set the
                       *-- flag to add this cuttkt to the report data.
                    llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
           IF !llUseContr .OR. llVend
             SELECT POSHDR
             SCATTER MEMVAR MEMO
             INSERT INTO (WORKFILE) FROM MEMVAR
           ENDIF    
           ENDSCAN 
           ENDIF  
           ELSE
             IF loDBFPOSHDR.Seek('PU') 
             SELECT POSHDR
             SCAN REST WHILE Cstytype = 'U' AND Cbusdocu = 'P' ;
               FOR IIF(EMPTY(lcRpStatus),POSHDR.STATUS $ 'AOHS'  ,POSHDR.STATUS $ lcRpStatus) AND ;
               IIF(llUseFab OR llUseGrp,loDBFStyle.Seek(POSHDR.Style,'CStyle'),.T.);
               AND IIF(llUseFab,SEEK(Style.Fabric,lcFabFile),.T.) AND ;
               IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) 
               
               llVend = .F.
               IF loDBFMFGOPRHD.Seek('M'+POSHDR.PO) 
                 SELECT (lcTmpOPH)
                 ZAP  
                 SELECT MFGOPRHD
                 SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+POSHDR.PO
                   SCATTER FIELDS Ctktno,cOperSeq,cOprCode,cContCode MEMVAR
                   INSERT INTO (lcTmpOPH) FROM MEMVAR
                 ENDSCAN
                 STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
                 SELECT (lcTmpOPH)
                 LOCATE 
                 IF !EOF()
                   lnCount = 1
                   *-- Operations is sorted by sequence number, so get first 3 of them 
                   *-- to put in temporary file of CUTTKTH
                   SCAN
                     lcCOUNT = STR(lnCount,1)
                      *-- If contractor of any operation match the required contractor set the
                       *-- flag to add this cuttkt to the report data.
                    llVend  = IIF(!llVend and llUseContr,SEEK(&lcTmpOPH..CCONTCODE,lcContFile),llVend)
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
           IF !llUseContr .OR. llVend
             SELECT POSHDR
             SCATTER MEMVAR MEMO
             INSERT INTO (WORKFILE) FROM MEMVAR
           ENDIF    

           ENDSCAN 
             ENDIF 
           ENDIF 
         ENDIF 
       ENDIF 
      ENDIF 
   ENDIF 
ENDIF 

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/10/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert)
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.
*!************************************************************
*! Name      : RefreshStatus
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/13/2006
*! Purpose   : function to refresh status 
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION RefreshStatus
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
