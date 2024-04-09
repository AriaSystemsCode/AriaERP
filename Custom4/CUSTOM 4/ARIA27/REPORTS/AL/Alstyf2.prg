*:***************************************************************************
*: Program file  : ALSTYAL
*: Program desc. : ALLOCATION REPORT FORM #2
*: Date          : 03/25/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpRepPrn
*:    Functions  : lfMakeExpr() , lfvStyle() , lfAdjSeg() , lfwOgWhen() ,;  
*:                 lfCreatCur() , lfClearRep()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes         : When Adding any new variable filters it must be in Piktkt Table
*: B803227 (AME) : Or you must make the proper relation between opened tables see , 
*:***************************************************************************
*: Example : DO ALSTYAL
*:***************************************************************************
*: This Program is due to E301181 ...
*:***************************************************************************
*E301249,1 IHB 06/20/1999 Make priority field C(3) in CUSTOMER 
*E301249,1 				  and ORDHDR files
*B803227,1 AME 05/21/2000 Take Variable Filter Expression in consideration
*                         when we make lcPikExpr.  
*B603660,1 AME 06/04/2000 Variable Filter for piktkt & Account Code have become in range as 
*                         Default.
*B603660,1 AME 06/04/2000 Status is no longer in var. Filt. instead it becomes in list.
*B803320,1 AME 06/04/2000 Location Code is  removed from  Var. Filt. to be fixed Filter 
*                         (In List)
*B803548,1 HBG 08/06/2000 In Pick Ticket Status 'Holed' should be 'Hold'
*B605419,1 BWA 01/28/2002 Fix the bug of the broken fields in case there is no records to display.
*B606197,1 BWA 07/02/2002 Fix the bug of the balance and the begin quntities is the same.
*:***************************************************************************
*

*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.
IF llOGFltCh
  *B803227,1  Define Variable to hold Varible filter Expression,  AME[start] 
  PRIVATE lcVarFlt
  *B803227,1 AME[end]
  
  lnMajorLen = LEN(lcStylePic)

  *-- if the temp. file is used and it's not empty this means that the report
  *-- will erase the data filled inside it and re-create it again for the new
  *-- filter expression 
 
  

  IF USED(lcOrdlTmp) AND RECCOUNT(lcOrdlTmp) > 0
    USE IN (lcOrdlTmp)
    *B803227,1 Comment out and transfered to check if the file was empty or not ,AME [start]
    *=lfCreatCur(lcOrdlTmp,'laOrdStru','STYLE')
    *B803227,1 AME [End]
  ENDIF

  *B803227,1 Creat the Cursor here instead of lfWOGWhen if not opened only , AME [start]
  =lfCreatCur(lcOrdlTmp,'laOrdStru','STYLE')
  *B803227,1 AME [End]
  
  IF USED(lcStyTemp) AND RECCOUNT(lcStyTemp) > 0
    USE IN (lcStyTemp)
    *B803227,1 Comment out and transfered to check if the file was empty or not ,AME [start]
    *=lfCreatCur(lcStyTemp,'laStyStru','STYLE')
    *B803227,1 AME [End]
  ENDIF

  *B803227,1 Creat the Cursor here instead of lfWOGWhen if not opened only , AME [start]
  =lfCreatCur(lcStyTemp,'laStyStru','STYLE')
  *B803227,1 AME [End]
 
  STORE '' TO lcPikExpr,lcStyleExp

  *-- loop fixed filter array to make optimized expressions, For Both Style and OrdLine.
  FOR lnI = 1 TO ALEN(laOGFxFlt,1)
    *-- if User enter filtered data [Begin.]
    IF !EMPTY(laOGFxFlt[lnI,6]) AND IIF(laOGFxFlt[lnI,7]='V', .T. , ;
       USED(laOGFxFlt[lnI,6]) AND RECCOUNT(laOGFxFlt[lnI,6]) > 0)
       
       lcFltExpr = lfMakeExpr(lnI)  && Make Expression

       *-- if you have a filter expression.
       IF !EMPTY(lcFltExpr)
         IF 'STYLE.' $ lcFltExpr
           lcStyleExp = lcStyleExp + IIF(EMPTY(lcStyleExp),'',' AND ') +;
                                     lcFltExpr
         ELSE
           lcPikExpr = lcPikExpr + IIF(EMPTY(lcPikExpr),'',' AND ') +;
                                     lcFltExpr
         ENDIF
       ENDIF  
    
    ENDIF
    *-- if User enter filtered data [End.]
    
  ENDFOR
  *-- end loop fixed filter array to make optimized expressions.

  *B803227,1  Add The Variable Filter to The Piktkt filter Expression ;
  *B803227,1  First: we get the varible filter Expr. From the laOGVrFlt Array;
  *B803227,1  to var. lcVarFlt;
  *B803227,1  Second: We check for lcVarFlt 
  *B803227,1  Third: if lcVarFlt exists add it to lcPikExpr, AME [START]
  
  lcSCent = SET('CENTURY')
  SET CENTURY ON
  
  FOR I = 1  TO ALEN(laOGVrFlt,1)             && Change the type 
    IF laOGVrFlt[I,3] = 'D' .AND. (laOGVrFlt[I,5] = 'Like' OR laOGVrFlt[I,5] = 'Exactly Like')                  && of the date filters  
      laOGVrFlt[I,6]  = DTOC(laOGVrFlt[I,6])  && to character
    ENDIF                                     
  ENDFOR                                      
  
  lcVarFlt = gfGenFlt('laOGVrFlt',.T.,.T.)
  
  FOR I = 1  TO ALEN(laOGVrFlt,1)                                                                               && reback the type 
    IF laOGVrFlt[I,3] = 'D' .AND. (laOGVrFlt[I,5] = 'Like' OR laOGVrFlt[I,5] = 'Exactly Like')                  && of the date filters  
      laOGVrFlt[I,6]  = CTOD(laOGVrFlt[I,6])                                                                    && to Date again
    ENDIF 
  ENDFOR
  
  SET CENTURY &lcSCent
  
  IF !EMPTY(lcVarFlt) AND !(".T."$lcVarFlt)
    lcPikExpr = lcPikExpr + IIF(EMPTY(lcPikExpr),"",[ AND ]) + lcVarFlt
  ENDIF  
  *B803227,1 AME[END]
  
  
  *B603660,1 Add the lcRpStatus to lcPikExpr AME[Start]
  lcPikExpr = lcPikExpr + IIF(EMPTY(lcPikExpr),"",[ AND ]) + "Status $  lcRpStatus" 
  *B603660,1  AME[End]

  IF !EMPTY(lcPikExpr)
    lcPikExpr = 'FOR ' + STRTRAN(lcPikExpr,'PIKTKT.','')
  ENDIF
    
  IF EMPTY(lcStyleExp)
    lcStyleExp = [FOR Piktkt==lcPikTkt] 
  ELSE
    lcStyleExp = 'FOR ' + STRTRAN(lcStyleExp,'STYLE.','')
    lcStyleExp = STRTRAN(lcStyleExp,'CSTYMAJOR','SUBSTR(STYLE,1,lnMajorLen)')
    lcStyleExp = lcStyleExp + [ AND Piktkt==lcPikTkt] 
  ENDIF
  
  *-- Collect data [Begin.]
  SELECT PIKTKT
  SET RELATION TO 'O'+order INTO Ordline ADDITIVE

  *-- Scan Piktkt to get lines of this Piktkt from Ordline file [Begin.]
  SCAN &lcPikExpr
    lcOrder  = ORDER
    lcPikTkt = Piktkt

    SELECT ORDLINE
    SCAN REST WHILE cOrdType+Order+STR(lineNo,6) = 'O'+lcOrder &lcStyleExp
	    SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdlTmp) FROM MEMVAR
    ENDSCAN  
  ENDSCAN
  *-- Scan Piktkt to get lines of this Piktkt from Ordline file [End.]
  
  SELECT PIKTKT
  SET RELATION TO
  *-- collect data [End.]

  *-- if (lcOrdlTmp) has lines , collect Pik1,Pik2,...etc for each style
  SELECT (lcOrdlTmp)
  IF RECCOUNT() > 0

    *-- Scan (lcOrdlTmp) to get sum of Pik1,Pik2,...etc for each style  
    SCAN
      *-- if found update the field with the new summation
      IF SEEK(STYLE,lcStyTemp)
    
        REPLACE &lcStyTemp..ALO1   WITH &lcStyTemp..ALO1   + PIK1 ,;
                &lcStyTemp..ALO2   WITH &lcStyTemp..ALO2   + PIK2 ,;
                &lcStyTemp..ALO3   WITH &lcStyTemp..ALO3   + PIK3 ,;
                &lcStyTemp..ALO4   WITH &lcStyTemp..ALO4   + PIK4 ,;
                &lcStyTemp..ALO5   WITH &lcStyTemp..ALO5   + PIK5 ,;
                &lcStyTemp..ALO6   WITH &lcStyTemp..ALO6   + PIK6 ,;
                &lcStyTemp..ALO7   WITH &lcStyTemp..ALO7   + PIK7 ,;
                &lcStyTemp..ALO8   WITH &lcStyTemp..ALO8   + PIK8 ,;
                &lcStyTemp..TOTALO WITH &lcStyTemp..TOTALO + TOTPIK
      
      ELSE  && add new style with new picked quantity

        *B606197,1 BWA 07/02/2002 Fix the bug of the balance and the begin quntities is the same.[START]
        *INSERT INTO (lcStyTemp)                                             ;
                 (Style,Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo)     ;
          VALUES (Style,Pik1,Pik2,Pik3,Pik4,Pik5,Pik6,Pik7,Pik8,TotPik)     

        INSERT INTO (lcStyTemp)                                             ;
                 (Style,Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo)     ;
          VALUES (&lcOrdlTmp..Style,&lcOrdlTmp..Pik1,&lcOrdlTmp..Pik2,&lcOrdlTmp..Pik3,&lcOrdlTmp..Pik4,;
                                    &lcOrdlTmp..Pik5,&lcOrdlTmp..Pik6,&lcOrdlTmp..Pik7,&lcOrdlTmp..Pik8,;
                                    &lcOrdlTmp..TotPik)
        *B606197,1 BWA 07/02/2002.[END]

      ENDIF  && endif of updating fields 

    ENDSCAN  && end of scan (lcOrdlTmp)
  ENDIF  && endif of RECCOUNT()>0  
ENDIF  && end of llOGFltCh

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcOrdlTmp) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')

  *B605419,1 BWA 01/28/2002 The page broken after this message because the device didn't return to its original status which is screen.[START]
  SET DEVICE TO SCREEN
  *B605419,1 BWA 01/28/2002.[END]

ELSE  && display the report
  
  CLEAR TYPEAHEAD	
  SET DEVICE TO PRINT

  SELECT (lcOrdlTmp)
  *SET RELATION TO STYLE INTO STYLE, STYLE INTO (lcStyTemp),;
  *IIF(EMPTY(STORE) , 'M'+ACCOUNT , 'S'+ACCOUNT+STORE ) INTO CUSTOMER,;
  *ORDER INTO ORDHDR

  *-- SSE commented out this relation in order to make lcOrdlTmp related to 
  *-- ORDHDR by 'O+ORDHDR inspite of ORDHDR
  SET RELATION TO STYLE INTO STYLE, STYLE INTO (lcStyTemp),;
  IIF(EMPTY(STORE) , 'M'+ACCOUNT , 'S'+ACCOUNT+STORE ) INTO CUSTOMER,;
  'O'+ORDER INTO ORDHDR

  DO lpRepPrn

  SELECT (lcOrdlTmp)
  SET RELATION TO

  DO ENDREPORT
  SET DEVICE TO SCREEN

ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]


*!*************************************************************
*! Name      : lpRepPrn
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Printing loop for the main report
*!*************************************************************
*! Called from : Main Program (ARCDEP)
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpRepPrn
*!*************************************************************
*
*-- Begin Print loop
PROCEDURE lpRepPrn

*-- Variables used in Print loop [Begin.]
R_TITLE    = "Allocation Report By Style"
R_WIDTH    = 'N'
ROW    = 99
PAGENO = 0
XNEWSTY= .T.
XSTYLE = SPACE(19)
lcWare = ''
DIMENSION XTOTAVL(9)
*-- Variables used in Print loop [End.]

*-- Start of SCAN (@ Row,Col SAY...) 
SCAN 

  IF ROW >55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ALSTYAL','',R_WIDTH
    ROW = 5
  ENDIF

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8
*STYLE        CLR
*123456789012 123456
*                BAL: 12345 12345 123456 123456 123456 12345 12345 12345 123456
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8

  IF STYLE <> XSTYLE
    IF !EMPTY(XSTYLE)
      @ ROW,17 SAY 'BAL:'
      @ ROW,22 SAY XTOTAVL(1) PICTURE '99999'
      @ ROW,28 SAY XTOTAVL(2) PICTURE '99999'
      @ ROW,34 SAY XTOTAVL(3) PICTURE '99999'
      @ ROW,40 SAY XTOTAVL(4) PICTURE '99999'
      @ ROW,46 SAY XTOTAVL(5) PICTURE '99999'
      @ ROW,52 SAY XTOTAVL(6) PICTURE '99999'
      @ ROW,58 SAY XTOTAVL(7) PICTURE '99999'
      @ ROW,64 SAY XTOTAVL(8) PICTURE '99999'
      @ ROW,72 SAY XTOTAVL(9) PICTURE '999999'
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('.',80)
      ROW = ROW + 1
    ENDIF

	*-- Print both style and color.
	*MAB NOW
	*@ ROW,01 SAY PADR(STYLE.CSTYMAJOR,lnMajorLen)
    *@ ROW,14 SAY SUBSTR(Style,lnClrPos,lnClrPic)     
	@ ROW,01 SAY STYLE

    @ ROW,22 SAY GETSCALE(STYLE.SCALE,SPACE(1) )    &&11/30/94
    ROW = ROW + 1
    @ ROW,17 SAY 'BGN:'
    @ ROW,22 SAY STYLE.STK1 - ( STYLE.ALO1 - &lcStyTemp..ALO1) PICTURE '99999'
    @ ROW,28 SAY STYLE.STK2 - ( STYLE.ALO2 - &lcStyTemp..ALO2) PICTURE '99999'
    @ ROW,34 SAY STYLE.STK3 - ( STYLE.ALO3 - &lcStyTemp..ALO3) PICTURE '99999'
    @ ROW,40 SAY STYLE.STK4 - ( STYLE.ALO4 - &lcStyTemp..ALO4) PICTURE '99999'
    @ ROW,46 SAY STYLE.STK5 - ( STYLE.ALO5 - &lcStyTemp..ALO5) PICTURE '99999'
    @ ROW,52 SAY STYLE.STK6 - ( STYLE.ALO6 - &lcStyTemp..ALO6) PICTURE '99999'
    @ ROW,58 SAY STYLE.STK7 - ( STYLE.ALO7 - &lcStyTemp..ALO7) PICTURE '99999'
    @ ROW,64 SAY STYLE.STK8 - ( STYLE.ALO8 - &lcStyTemp..ALO8) PICTURE '99999'
    @ ROW,72 SAY STYLE.TOTSTK - ( STYLE.TOTALO - &lcStyTemp..TOTALO) PICTURE '999999'
    ROW = ROW + 1

    FOR X = 1 TO 8
      Z = STR(X,1)
      XTOTAVL(X) = STYLE.STK&Z - STYLE.ALO&Z
    ENDFOR
    XTOTAVL(9) = STYLE.TOTSTK - STYLE.TOTALO

    XSTYLE = STYLE
    XNEWSTY= .T.
  ENDIF

  lcWare = IIF(SEEK(PIKTKT,'PIKTKT'),PIKTKT.CWARECODE,lcWare)

*E301249,1 Make priority field C(3) in CUSTOMER [start]
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*
**ORDER  WAREHOUSE   ACCT#    STORE    NAME            P S START    COMPLETE
**123456 123456      123456   12345678 123456789012345 1 X 99/99/99 99/99/99
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*
*ORDER  WAREHOUSE   ACCT#    STORE    NAME            P   S START    COMPLETE
*123456 123456      123456   12345678 123456789012345 999 X 99/99/99 99/99/99
*E301249,1 [end]

  IF XNEWSTY .AND. llMultWH
    
    *E301249,1 Make priority field C(3) in CUSTOMER [start]
	*@ ROW,01 SAY 'Order  Warehouse   Acct#    Store    Name            P S Start    Complete'
	@ ROW,01 SAY 'Order  Warehouse   Acct#    Store    Name            P   S Start    Complete'
    *E301249,1 [end]
    
    ROW = ROW + 1
  ELSE
    
    *E301249,1 Make priority field C(3) in CUSTOMER [start]
    *@  ROW,00 SAY 'Order  Acct#    Store    Name            P S Start    Complete'
    @  ROW,00 SAY 'Order  Acct#    Store    Name            P   S Start    Complete'
    *E301249,1 [end]
    
    ROW = ROW + 1
  ENDIF

  @ ROW,00 SAY ORDER

  IF llMultWH
    @ ROW,08 SAY lcWare
  ENDIF
  
  @ ROW,IIF(llMultWH,20,7) SAY ACCOUNT
  @ ROW,IIF(llMultWH,29,16) SAY STORE
  @ ROW,IIF(llMultWH,38,25) SAY SUBSTR(CUSTOMER.STNAME,1,15)
  
  *E301249,1 Make priority field C(3) in CUSTOMER [start]
  *@ ROW,IIF(llMultWH,54,41) SAY ORDHDR.PRIORITY
  *@ ROW,IIF(llMultWH,56,43) SAY ORDHDR.STATUS
  *@ ROW,IIF(llMultWH,58,45) SAY START
  *@ ROW,IIF(llMultWH,67,54) SAY COMPLETE
  @ ROW,IIF(llMultWH,54,41) SAY ALLTRIM(ORDHDR.PRIORITY)
  @ ROW,IIF(llMultWH,56,43)+2 SAY ORDHDR.STATUS
  @ ROW,IIF(llMultWH,58,45)+2 SAY START
  @ ROW,IIF(llMultWH,67,54)+2 SAY COMPLETE
  *E301249,1 [end]
  
  ROW = ROW + 1
  @ ROW,22 SAY QTY1 PICTURE '99999'
  @ ROW,27 SAY IIF(PIK1<QTY1 , '*' ,'')

  @ ROW,28 SAY QTY2 PICTURE '99999'
  @ ROW,33 SAY IIF(PIK2<QTY2 , '*' ,'')

  @ ROW,34 SAY QTY3 PICTURE '99999'
  @ ROW,39 SAY IIF(PIK3<QTY3 , '*' ,'')

  @ ROW,40 SAY QTY4 PICTURE '99999'
  @ ROW,45 SAY IIF(PIK4<QTY4 , '*' ,'')

  @ ROW,46 SAY QTY5 PICTURE '99999'
  @ ROW,51 SAY IIF(PIK5<QTY5 , '*' ,'')

  @ ROW,52 SAY QTY6 PICTURE '99999'
  @ ROW,57 SAY IIF(PIK6<QTY6 , '*' ,'')

  @ ROW,58 SAY QTY7 PICTURE '99999'
  @ ROW,63 SAY IIF(PIK7<QTY7 , '*' ,'')

  @ ROW,64 SAY QTY8 PICTURE '99999'
  @ ROW,70 SAY IIF(PIK8<QTY8 , '*' ,'')

  @ ROW,72 SAY TOTQTY PICTURE '999999'
  @ ROW,79 SAY GROUP
  ROW = ROW + 1

ENDSCAN
*-- End of SCAN (@ Row,Col SAY...)

@ ROW,17 SAY 'BAL:'
@ ROW,22 SAY XTOTAVL(1) PICTURE '99999'
@ ROW,28 SAY XTOTAVL(2) PICTURE '99999'
@ ROW,34 SAY XTOTAVL(3) PICTURE '99999'
@ ROW,40 SAY XTOTAVL(4) PICTURE '99999'
@ ROW,46 SAY XTOTAVL(5) PICTURE '99999'
@ ROW,52 SAY XTOTAVL(6) PICTURE '99999'
@ ROW,58 SAY XTOTAVL(7) PICTURE '99999'
@ ROW,64 SAY XTOTAVL(8) PICTURE '99999'
@ ROW,72 SAY XTOTAVL(9) PICTURE '999999'
RETURN
*-- End of Print loop

*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Make an expression for one item in fixed filter array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Item row in fixed filter array
*!*************************************************************
*! Returns            : line expression
*!*************************************************************
*! Example   : =lfMakeExpr()
*!*************************************************************
*!
FUNCTION lfMakeExpr
PARAMETERS lnFxFltItm
PRIVATE lcItmExpr

lcItmExpr = ''
*-- if operator is like
IF 'LIKE' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
  lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' = "' +  laOGFxFlt[lnFxFltItm,6] + '"'

ELSE  && operator is either Between or InList

  *-- if operator is between.
  IF 'BETWEEN' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
    *-- if Type is Date
    PRIVATE lcFrom,lcTo
    lcFrom = SUBSTR(laOGFxFlt[lnFxFltItm,6],1,ATC('|',laOGFxFlt[lnFxFltItm,6]) - 1)
    lcTo   = SUBSTR(laOGFxFlt[lnFxFltItm,6],ATC('|',laOGFxFlt[lnFxFltItm,6]) + 1)

    IF laOGFxFlt[lnFxFltItm,3] = 'D'
      
      lcFrom = CTOD(lcFrom)
      lcTo   = CTOD(lcTo)

      IF !EMPTY(lcFrom)
        lcFrom = DTOS(lcFrom)
        lcTo   = DTOS(lcTo)
        lcItmExpr = 'BETWEEN( DTOS(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ') , "' + ;
                                 lcFrom + '" , "' + lcTo + '")'
      ENDIF  

    
    ELSE  && Type is not date 
*-- if the fixed filter is not date (e.g. Numeric or Character) and 
*-- Operator is BETWEEN
        lcItmExpr = 'BETWEEN(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' , "' + ;
                                 lcFrom + '" , "' + lcTo + '")'
    ENDIF

  ELSE  && else operator is in list

    *-- if in range case.
    IF laOGFxFlt[lnFxFltItm,7] = 'R'
      lcItmExpr = 'SEEK(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ', "' +;
                            ALLTRIM(laOGFxFlt[lnFxFltItm,6]) + '")'
    ELSE  && default in list case
      lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' $ "' + laOGFxFlt[lnFxFltItm,6] + '"'
    ENDIF                           
    
  ENDIF

ENDIF
RETURN IIF(EMPTY(lcItmExpr),lcItmExpr,'(' + lcItmExpr + ')')
*-- end of lfMakeExpr.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Sameh (SSE)
*! Date      : 03/23/99
*! Purpose   : Validate Style 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************

FUNCTION lfvStyle
PRIVATE lcStyFld,lcStyle,lnSelcFile,lcCusTag

lcStyFld   = VARREAD()
lcStyle    = EVAL(lcStyFld)

lnSelcFile = SELECT(0)
SELECT Style
lcCusTag  = ORDER('style')
SET ORDER TO TAG Cstyle IN Style
IF !EMPTY(lcStyle)
  IF ('?' $ lcStyle .OR. !SEEK(lcStyle,'Style'))
    lcStyle = gfStyBrw('M',"","",.F.)
  ELSE 
    &lcStyFld = lcStyle    
  ENDIF	
ENDIF
&lcStyFld = lcStyle
SET ORDER TO lcCusTag
SELECT (lnSelcFile)

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : option grid of POPRLB.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
*
FUNCTION lfAdjSeg

STORE 0 TO lnFPos , lnDPos , lnZPos   , lnGPos , lnCPos , lnOPos , lnTPos , ;
           lnQPos , lnSPos , lnMajPos
STORE 0 TO lnMajLen
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
*--Get Major Length

FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it,
  *-- display the style major instead (style major will browse from the
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos   = lnC
      lnClrPos = laMajSeg[lnC,4]
      lnClrPic = LEN(laMajSeg[lnC,3])
      *IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Only These Colors'
      *ENDIF
  ENDCASE
ENDFOR
*-- end of lfAdjSeg.

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Load Settings before Report starts (When Func.)
*!*************************************************************
*! Called from : option grid of ALSTYAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************

FUNCTION lfwOgWhen

DIMENSION laOrdStru[1,4],laStyStru[1,4]


*B603660,1 Creat the Arrays That hold target & Source of Status inlist,AME[End]
DECLARE laRpSource[5],laRpTarget[5]
STORE 'Open'     TO laRpSource[1] , laRpTarget[1] 
STORE 'Released' TO laRpSource[2] , laRpTarget[2]
STORE 'Pulled'   TO laRpSource[3] , laRpTarget[3]
*B803548,1 HBG It is spelled 'Hold' not 'Holed' [Begin]
*STORE 'Holed'   TO laRpSource[4] , laRpTarget[4]
STORE 'Hold'   TO laRpSource[4] , laRpTarget[4]
*B803548,1 [End]

STORE 'Complete' TO laRpSource[5] , laRpTarget[5]
lcRpStatus = 'OXPHC'             && Variable that hold Status Exprission
*B603660,1 AME[End]

SELECT ORDLINE
=AFIELDS(laOrdStru)
*B803227,1 No Need to Create the temp. Cursors Every Time Run When function ,AME[start]
*=lfCreatCur(lcOrdlTmp,'laOrdStru','STYLE')
*B803227,1 AME[End]

*B803227,1 No Need to Create the temp. Cursors Every Time Run When function ,AME[start]
SELECT STYLE
=AFIELDS(laStyStru)
*=lfCreatCur(lcStyTemp,'laStyStru','STYLE')
*B803227,1 AME[End]
*-- end of lfwOgWhen.


*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Create Cursor file from the temp. file
*!*************************************************************
*! Called from : option grid of ALSTYAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatCur()
*!*************************************************************
*
FUNCTION lfCreatCur
PARAMETERS lcCurName,lcCurStrut,lcTagExpr
*B803227,1 Creat the Cursor just if it's not used only , AME[start]
IF !USED(lcCurName)
*B803227,1 AME  [End]
  CREATE CURSOR (lcCurName) FROM ARRAY (lcCurStrut)
  INDEX ON &lcTagExpr TAG (lcCurName) OF (gcWorkDir+lcCurName+'.CDX') 
*B803227,1  Creat the Cursor just if it's not used only , AME[start]
ENDIF   
*B803227,1 AME [End]
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Close OG Function.
*!*************************************************************
*! Called from : OG < Close > or < Reset > Buttons.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep

llOGFltCh = .T.

IF USED(lcOrdlTmp)
  USE IN (lcOrdlTmp)
ENDIF

IF USED(lcStyTemp)
  USE IN (lcStyTemp)
ENDIF
*--end of lfClearRep.

*!*************************************************************
*! Name      : lfvAcc  (B603660)
*! Developer : Ahmed Mohamed_El_anwar Abd_El_Fattah
*! Date      : 25/05/2000
*! Purpose   : Validation function for the Account. field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*B603660
FUNCTION lfvAcc
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
*lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*--IF Statment to check if we are going to Browse
SELECT Customer
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK ('M'+lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "ACCOUNT :R :H= 'Acct #' ,"+  ;
               "BTNAME :R :H= 'Name' ,"+     ;
               "PHONE1 :R :H= 'Phone' ,"+    ;
               "CADDRESS6 :R :H= 'Country',"+;
               "NETBAL :R :H= 'Balance' "
  lcFile_Ttl = "Account.."
  lcBrowCond = [FOR TYPE = "M" ]
  = gfBrows(lcBrowCond,'ACCOUNT','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ''
  ENDIF
ENDIF    && End of IF !EMPTY(lcObj) AND ...etc.
&lcVar = lcObj      && Update the field
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfvPikt  (B603660)
*! Developer : AHMED EL_ANWAR (AME)
*! Date      : 25/05/2000
*! Purpose   : Validate the piktkt
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPikt()
*!*************************************************************
*B603660
FUNCTION lfvPikt
PRIVATE lcPikFld,lcPikTkt,lnSelFile,lcPikTag
	
lcPikFld  = VARREAD()
lcPikTkt  = EVAL(lcPikFld)
lcPikTkt  = IIF(EMPTY(lcPikTkt)  , lcPikTkt , PADL(ALLTRIM(lcPikTkt) , 6 , '0'))
lnSelFile =  SELECT(0)

SELECT PIKTKT
lcPikTag  = ORDER('PIKTKT')
SET ORDER TO TAG PIKTKT IN PIKTKT

IF !EMPTY(lcPikTkt) .AND. ('?' $  lcPikTkt   .OR.  !SEEK(lcPikTkt , 'PIKTKT'))
  DIMENSION laTemp[1]
  laTemp = ''     
  lcBrFields = "Piktkt:H='Piktkt',Account:H='Account',;
                Store:H='Store',Order:H='Order' "
  
   = gfBrows('','Piktkt','laTemp')
  IF !EMPTY(laTemp[1])
    lcPikTkt = laTemp[1]
  ELSE 
    lcPikTkt = ''
  ENDIF
ENDIF

&lcPikFld = lcPikTkt
SET ORDER TO lcPikTag
SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfvOStatus  (B603660)
*! Developer : AHMED MOHAMED EL_ANWAR(AME)
*! Date      : 25/05/2000
*! Purpose   : - Evaluate Status expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*B603660
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select PikTkt Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *B803548,1 HBG It is spelled 'Hold' not 'Holed' [Begin]
    *lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Released','X',;
                              IIF(laRpTarget[lnI] = 'Pulled','P' , ;
                              IIF(laRpTarget[lnI] = 'Holed','H' , ;
                              IIF(laRpTarget[lnI] = 'Complete','C','')))))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Released','X',;
                              IIF(laRpTarget[lnI] = 'Pulled','P' , ;
                              IIF(laRpTarget[lnI] = 'Hold','H' , ;
                              IIF(laRpTarget[lnI] = 'Complete','C','')))))                              
    *B803548,1 [End]                             

  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OXPHC', ALLTRIM(lcRpStatus))


*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.

*!**************************************************************************
*! Name      : lfsPikTkt 
*! Developer : Ahmed Mohamed El_Anwar Abdel_Fattah (AME)
*! Date      : 06/05/2000
*! Purpose   : Directs PikTkt File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsPikTkt()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
*B603660
FUNCTION lfsPikTkt

PARAMETERS lcParm

GO TOP  IN PikTkt

*-- End of lfsPikTkt.

*!**************************************************************************
*! Name      : lfsCustmr
*! Developer : Ahmed Mohamed El_Anwar Abdel_Fattah (AME)
*! Date      : 06/05/2000
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsCustmr()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
*B603660
FUNCTION lfsCustmr

PARAMETERS lcParm

GO TOP  IN Customer

*-- End of lfsCustmr.
