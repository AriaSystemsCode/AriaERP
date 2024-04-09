*:***************************************************************************
*: Program file  : ARCOS200
*: Program desc. : SUMMERY SALES BY GROUP
*! Date          : 04/22/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Reference     : *C101502,1
*:***************************************************************************
*: Example : DO ARCOS200
*:***************************************************************************
*: Modifications:
*: B802596,1 BWA 10/06/1999 Fix the bug of the advanced option that get error 
*:                          in the sydreprt.dbf .
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:***************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint   = .F.

*-- Variable Declaration

STORE 0 TO lnFTotGrnd , lnSTotGrnd
lnFGrand   = 0
lnSGrand   = 0
R_WIDTH    ='W'
ROW        = 02
lnPage     = 00
lcAccount  = SPACE(0)
lcFilter   = lcRpExp     
lcSlsRep   = lcRpSales
lcTmpFile  = gfTempName()
lcColTemp  = gfTempName()
lcColTemp1 = gfTempName()

*-- Variable Declaration (END)

*-- variables lcAccount,lcGroup,lcStat get there values from the criteria
lnDatePos  = ASCAN(laoGFxflt,'CUSTOMER.ACCOUNT')
IF  lnDatePos > 0
  lnDatePos  = ASUBSCRIPT(laoGFxflt,lnDatePos,1)
  *IF USED(laOGFxFlt[lnDatePos,6]) .AND. !EOF(laOGFxFlt[lnDatePos,6])
  IF USED(laOGFxFlt[lnDatePos,6]) 
    GOTO TOP IN (laOGFxFlt[lnDatePos,6])
    lcLAcct = EVAL(laOGFxFlt[lnDatePos,6]+'.account')
  
    GOTO BOTTOM IN (laOGFxFlt[lnDatePos,6])
    lcBAcct = EVAL(laOGFxFlt[lnDatePos,6]+'.account')
   
    IF lcLAcct  = lcBAcct
      lcAccount = lcLAcct
    ELSE  
      lcAccount = lcLAcct +' TO ' + lcBAcct
    ENDIF  
  ENDIF  
ENDIF

lnDatePos    = ASCAN(laOGFxFlt,"STYLE.CSTYGROUP")
IF lnDatePos > 0
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  lcGroup    = laOGFxFlt[lnDatePos,6]
ENDIF

lnDatePos    = ASCAN(laOGFxFlt,"CUSTOMER.CADDRESS4")
IF lnDatePos > 0
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  lcStat    = laOGFxFlt[lnDatePos,6]
ENDIF
*-- value of variables lcAccount,lcGroup,lcStat (END)

*-- THE MAIN PROGRAM 
DO lpCollData

*-- PRINT THE REPORT 
IF llPrint
  SET DEVICE TO PRINT
  =lfPriHdr()
  =lfPriBod()
  =lfPrnFot()
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF
*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Function To Select Data Using Select "SQL"
*!           : and collect the data.  
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollData
*!*************************************************************

PROCEDURE lpCollData

PRIVATE lnAlias,lcTgrp,lnTemp1,lnTmep2,lnRecNo,lnQTemp1,lnQTemp2
STORE 0 TO lnTotFPer,lnTotSPer,lnTemp1,lnTemp2,lnSGrand,lnFGrand
STORE 0 TO lnFTotGrnd , lnSTotGrnd
lcTgrp  = ""
IF EMPTY(ldFDate1) .OR. EMPTY(ldFDate2) ;
  .OR. EMPTY(ldSDate1) .OR. EMPTY(ldSDate2)
  WAIT 'You have to enter first & second date.' WINDOW NOWAIT
  RETURN
ENDIF   

SELECT  INVLINE.Style,INVLINE.TotQty,INVLINE.Price,;
        INVHDR.Account,STYLE.CstyGroup,;
        INVHDR.InvDate;
   FROM Customer,INVLINE,INVHDR,STYLE;
   WHERE &lcFilter;
     .AND. IIF(!EMPTY(lcSlsRep),INVHDR.Rep1 = lcSlsRep,.T.);
     .AND. STYLE.Style = INVLINE.Style ;
     .AND. (BETWEEN(InvHdr.InvDate,ldFDate1,ldFDate2) OR;
            BETWEEN(InvHdr.InvDate,ldSDate1,ldSDate2));
     .AND. INVLINE.Invoice = InvHdr.Invoice;
     .AND. Customer.Type + Customer.Account + Customer.Store =;
           IIF(EMPTY(INVLINE.Store),'M','S') + InvLine.Account+InvLine.Store;
    INTO DBF (lcTmpFile)

INDEX ON CstyGroup TO &lcTmpFile
SELECT &lcTmpFile
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ELSE
  llPrint   = .T.
  *-- CREAT CURSOR TO HOLD THE TOTALS
  CREATE CURSOR &lcColTemp (Groups C(06),FLinTot N(10,02),;
                                      FPer    N(6,2),;
                                      FTotQty N(10,02),;
                                      FPerQty N(6,2),;
                                      STotQty N(10,02),;
                                      SPerQty N(6,2),;
                                      SLinTot N(10,02),;
                                      SPer N(6,2))

  INDEX ON SUBSTR(STR(FPer,7,3),ATC('.',STR(FPer,7,3))+1) TAG &lcColTemp  OF (lcColTemp)
  INDEX ON SUBSTR(STR(SPer,7,3),ATC('.',STR(SPer,7,3))+1) TAG &lcColTemp1 OF (lcColTemp) ADDITIVE
  SELECT &lcTmpFile

  SUM ALL (&lcTmpFile..TotQty)*(&lcTmpFile..Price),(&lcTmpFile..TotQty);
      FOR BETWEEN(&lcTmpFile..InvDate,ldFDate1,ldFDate2);
      TO lnFGrand , lnFTotGrnd

  SUM ALL (&lcTmpFile..TotQty)*(&lcTmpFile..Price),(&lcTmpFile..TotQty);
      FOR BETWEEN(&lcTmpFile..InvDate,ldSDate1,ldSDate2);
      TO lnSGrand , lnSTotGrnd
  GOTO TOP

  DO WHILE !EOF()
    lcTgrp   = &lcTmpFile..CSTYGROUP
    lnTemp1  = 0
    lnTemp2  = 0
    lnQTemp1 = 0
    lnQTemp2 = 0

    SCAN REST  WHILE &lcTmpFile..CSTYGROUP = lcTgrp
      IF BETWEEN(&lcTmpFile..InvDate,ldFDate1,ldFDate2)
        lnTemp1 = lnTemp1 + (&lcTmpFile..TotQty)*(&lcTmpFile..Price)
        lnQTemp1 = lnQTemp1 + (&lcTmpFile..TotQty)
      ELSE
        lnTemp2 = lnTemp2 + (&lcTmpFile..TotQty)*(&lcTmpFile..Price)
        lnQTemp2 = lnQTemp2 + (&lcTmpFile..TotQty)
      ENDIF
    ENDSCAN

    INSERT INTO (lcColTemp) (Groups,FLinTot,SLinTot,FTotQty,  ;
                            STotQty,FPerQty,SPerQty,FPer,SPer);
    VALUES  ( lcTgrp , lnTemp1, lnTemp2, lnQTemp1 , lnQTemp2, ;
              IIF(lnFTotGrnd<>0,(lnQTemp1/lnFTotGrnd)*100,0), ;
              IIF(lnSTotGrnd<>0,(lnQTemp2/lnSTotGrnd)*100,0), ;
              IIF(lnFGrand<>0,(lnTemp1/lnFGrand)*100,0)     , ;
              IIF(lnSGrand<>0,(lnTemp2/lnSGrand)*100,0) )

    lnTotFPer = lnTotFPer + INT(&lcColTemp..FPer)
    lnTotSPer = lnTotSPer + INT(&lcColTemp..SPer)
    SELECT &lcTmpFile
  ENDDO
  lnTotFPer = 100 - lnTotFPer
  lnTotSPer = 100 - lnTotSPer
  SELECT (lcColTemp)
ENDIF
*!*************************************************************
*! Name      : lfPriHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Print Page Header
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfPriHdr()
*!*************************************************************

FUNCTION lfPriHdr
PRIVATE  lcAccRang

lnPage = lnPage + 1
ROW    = 2
lcTit = "SUMMARY SALES BY GROUP"
@ ROW ,(132 - LEN(lcTit))/2 SAY lcTit

ROW = ROW + 3
IF EMPTY(lcAccount)
  @ ROW , 00 SAY "ACCOUNTS         :"
ELSE  
  @ ROW , 00 SAY "ACCOUNTS BETWEEN :"
ENDIF  
@ ROW , 20 SAY IIF(EMPTY(lcAccount),"ALL ACCOUNTS.",lcAccount)
ROW = ROW + 1
@ ROW , 00 SAY "STATE            :"
@ ROW , 20 SAY IIF(EMPTY(lcStat),"ALL STATES.      ",lcStat)
IF !EMPTY(lcStat)
  @ ROW , 25 SAY gfCodDes(lcStat,'STATE')
ENDIF
ROW = ROW + 1
@ ROW , 00 SAY "SALES REP.       :"
@ ROW , 20 SAY IIF(EMPTY(lcSlsRep),"ALL SALES REPS.",lcSlsRep)
IF !EMPTY(lcSlsRep)
   @ ROW , 25 SAY IIF(SEEK(ALLTRIM(lcSlsRep),'SALESREP'),SalesRep.Name,'')
ENDIF
ROW = ROW + 1
@ ROW , 00 SAY "STYLE GROUPS     :"
@ ROW , 20 SAY IIF(EMPTY(lcGroup),"ALL STYLE GROUPS.",lcGroup)
IF !EMPTY(lcGroup)
  @ ROW , 25 SAY gfCodDes(lcGroup,'CSTYGROUP ')
ENDIF
ROW = ROW + 2
*--- Header first line
*---First range
@ ROW , 000 SAY REPLICATE('Ä',28)  && (Group \ Grp Disc)
@ ROW , 033 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 045 SAY REPLICATE('Ä',10)  && (Pieses)
@ ROW , 057 SAY REPLICATE('Ä',08)  && (Grp%)
@ ROW , 069 SAY REPLICATE('Ä',08)  && (Pieses%)
*---First range(END)
*---Second range
@ ROW , 081 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 093 SAY REPLICATE('Ä',10)  && (Pieses)
@ ROW , 105 SAY REPLICATE('Ä',08)  && (Grp%)
@ ROW , 117 SAY REPLICATE('Ä',08)  && (Pieses%)
*---Second range(END)
*--- Header first line(END)

ROW = ROW + 1
@ ROW , 34 SAY ldFDate1
@ ROW , 82 SAY ldSDate1
ROW = ROW + 1
@ ROW , 000 SAY "GROUPS"
@ ROW , 007 SAY "GROUP DESCRIPTION"
@ ROW , 034 SAY ldFDate2
@ ROW , 047 SAY "Pieces"
@ ROW , 059 SAY "GRP%"
@ ROW , 069 SAY "Pieces %"
@ ROW , 082 SAY ldSDate2
@ ROW , 095 SAY "Pieces"
@ ROW , 107 SAY "GRP%"
@ ROW , 117 SAY "Pieces %"
ROW = ROW + 1
*--- Header second line
@ ROW , 00 SAY REPLICATE('Ä',06)
@ ROW , 07 SAY REPLICATE('Ä',21)
*---First range
@ ROW , 033 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 045 SAY REPLICATE('Ä',10)  && (Pieses)
@ ROW , 057 SAY REPLICATE('Ä',08)  && (Grp%)
@ ROW , 069 SAY REPLICATE('Ä',08)  && (Pieses%)
*---First range(END)
*---Second range
@ ROW , 081 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 093 SAY REPLICATE('Ä',10)  && (Pieses)
@ ROW , 105 SAY REPLICATE('Ä',08)  && (Grp%)
@ ROW , 117 SAY REPLICATE('Ä',08)  && (Pieses%)
*---Second range(END)
*--- Header second line(END)

ROW = ROW + 1
*--End functio print header.
*!*************************************************************
*! Name      : lfPriBod
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Print body of the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfPriBod()
*!*************************************************************

FUNCTION lfPriBod
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT &lcColTemp
INDEX ON Groups TAG &lcColTemp  OF (lcColTemp)
GOTO TOP
SCAN 
   @ ROW , 00 SAY &lcColTemp..Groups
   lcGrpDis = gfCodDes(&lcColTemp..Groups,'CSTYGROUP ')
   @ ROW , 07 SAY LEFT(lcGrpDis,26)
   @ ROW , 34 SAY &lcColTemp..FLinTot PICTURE '999999.99'
   @ ROW , 46 SAY &lcColTemp..FTotQty PICTURE '999999'
   @ ROW , 57 SAY &lcColTemp..FPer PICTURE '999.99'
   @ ROW , 64 SAY "%"
   @ ROW , 69 SAY &lcColTemp..FPerQty PICTURE '999.99'
   @ ROW , 76 SAY "%"
   @ ROW , 82 SAY &lcColTemp..SLinTot PICTURE '999999.99'
   @ ROW , 94 SAY &lcColTemp..STotQty PICTURE '999999'
   @ ROW , 105 SAY &lcColTemp..SPer PICTURE '999.99'
   @ ROW , 112 SAY "%"
   @ ROW , 116 SAY &lcColTemp..SPerQty PICTURE '999.99'
   @ ROW , 124 SAY "%"
   ROW = ROW + 1
   IF ROW > 60
     =lfPriHdr()
   ENDIF
ENDSCAN
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfPrnFot
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Print Page Footer
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfPrnFot()
*!*************************************************************

FUNCTION lfPrnFot
@ ROW , 00 SAY REPLICATE('Ä',28)
*---First range
@ ROW , 033 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 045 SAY REPLICATE('Ä',10)  && (Pieses)
*---First range(END)
*---Second range
@ ROW , 081 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 093 SAY REPLICATE('Ä',10)  && (Pieses)
*---Second range(END)

ROW  = ROW + 1
@ ROW , 03  SAY "** GRAND TOTAL **"
@ ROW , 33  SAY lnFGrand PICTURE '9999999.99'
@ ROW , 45  SAY lnFTotGrnd PICTURE '9999999'
@ ROW , 81  SAY lnSGrand PICTURE '9999999.99'
@ ROW , 93  SAY lnSTotGrnd PICTURE '9999999'
ROW  = ROW + 1
*---Closing the page
@ ROW , 00 SAY REPLICATE('Ä',28)
*---First range
@ ROW , 033 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 045 SAY REPLICATE('Ä',10)  && (Pieses)
*---First range(END)
*---Second range
@ ROW , 081 SAY REPLICATE('Ä',10)  && (TotQty * Price)
@ ROW , 093 SAY REPLICATE('Ä',10)  && (Pieses)
*---Second range(END)
*--End functio print Footer.

*!*************************************************************
*! Name      : lfvAcc
*! Developer : BASSEM RAFAAT 
*! Date      : 04/22/1999
*! Purpose   : Validate Account
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc

PRIVATE lcCusFld,lcCustom,lnSelFile,lcCusTag

lcCusFld  = VARREAD()
lcCustom  = EVAL(lcCusFld)

lnSelFile =  SELECT(0)
SELECT CUSTOMER
lcCusTag  = ORDER('CUSTOMER')
SET ORDER TO TAG CUSTOMER IN CUSTOMER

IF !EMPTY(lcCustom) .AND. ( '?' $ lcCustom .OR. !SEEK((IIF( EMPTY(STORE) , 'M'+lcCustom , 'S'+lcCustom+STORE )) , 'CUSTOMER'))
  =CUSBROWM (@lcCustom)
ENDIF

&lcCusFld = lcCustom
SET ORDER TO lcCusTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvSalesr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/99
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSalesr()
*!*************************************************************
FUNCTION lfvSalesr
*PRIVATE lcVar , lcObj , laTemp 
PRIVATE lcSaleF , lcSales , lnSelFile , lcCusTag

lcSaleF = VARREAD()
lcSales = EVALUATE(lcSaleF) 

lnSelFile =  SELECT(0)
SELECT SALESREP
lcCusTag  = ORDER('SALESREP')
SET ORDER TO TAG SALESREP IN SALESREP


IF !EMPTY(lcSales) .AND. ('?' $ lcSales OR !SEEK(lcSales , 'SALESREP'))

  DIMENSION laTemp[1]
  laTemp = ''    
  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')

  IF !EMPTY(laTemp[1])
    lcSales   = laTemp[1]
    lcRepName = SalesRep.Name
  ELSE   
    lcSales   = ""
    lcRepName = ""
  ENDIF    
ENDIF    

&lcSaleF = lcSales	 
SET ORDER TO lcCusTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvDateRng1
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo
llFirst = .T.
ldFrom = ldFDate1
ldTo   = ldFDate2 

lcTitle = 'Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900
*B603955,1 ABD - & I change the name to use this valid function. [Begin]
*FUNCTION lfvpbDateOk
FUNCTION lfvpbok
*B603955,1 ABD - [End]
IF llFirst
  IF EMPTY(ldFrom)
    =gfModalGen('TRM00250B00000','DIALOG', 'first date it cannot be empty') 
    _CUROBJ = OBJNUM(ldFrom)
    RETURN
  ENDIF	
    
  IF ldFrom > ldTo
     WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
    _CUROBJ = OBJNUM(ldFrom)
  ELSE
  
  IF !EMPTY(ldSDate1) .AND. !EMPTY(ldSDate2)
    IF !EMPTY(ldTo) .AND. ( ldTo >= ldSDate1)
      WAIT  "Intersection between first and second range ." WINDOW NOWAIT
      _CUROBJ = OBJNUM(ldTo)
      RETURN 
    ENDIF
  ENDIF

    ldFDate1 = ldFrom
    ldFDate2 = ldTo
    CLEAR READ
  ENDIF
ELSE
  IF EMPTY(ldFrom)
    =gfModalGen('TRM00250B00000','DIALOG', 'second date it cannot be empty') 
    _CUROBJ = OBJNUM(ldFrom)
    RETURN
  ENDIF

  IF !EMPTY(ldFrom) .AND. ( ldFrom >= ldFDate1 .AND. ldFrom <= ldFDate2 )
    WAIT  "Intersection between first and second range ." WINDOW NOWAIT
    _CUROBJ = OBJNUM(ldFrom)
    RETURN 
  ENDIF
    
  IF ldFrom > ldTo
     WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
    _CUROBJ = OBJNUM(ldFrom)
  ELSE
    ldSDate1 = ldFrom
    ldSDate2 = ldTo
    CLEAR READ
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvDtRng2
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/1999
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDtRng2()
*!*************************************************************

FUNCTION lfvDtRng2

PRIVATE ldFrom,ldTo
llFirst = .F.
IF  EMPTY(ldFDate1) .OR. EMPTY(ldFDate2)
  Wait window 'You have to enter first date first.'
  RETURN
ENDIF

ldFrom = ldSDate1
ldTo   = ldSDate2

lcTitle = 'Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]
*!*************************************************************