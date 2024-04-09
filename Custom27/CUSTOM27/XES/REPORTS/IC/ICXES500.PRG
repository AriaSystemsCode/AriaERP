*:***************************************************************************
*: Program file  : ICXES500
*: Program desc. : Custom Style Report for XES.
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Ahmed Mohamed El_Anwar (AME)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfModalGen,gfItemMask,
*:               : gfcodDes,lfEvalSegs,
*:               : lfwOldVal,FaBrow,lfvFabric,
*:               : lfSRVSty,lfStySum,lfClearRep.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICXES500
*:***************************************************************************
*: This Report Program is due to C102166 ...
*: In Aria 2.6 it was MEZ500 with 
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*The data printing in this report can be cosidered two separate reports
*Detailed (Cut&sold by size) AND Summary

*---- Add The make by & style status to filter expression.
lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = [style = ''] +;
            IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[Style.Make]) +;
            IIF(lcRpStatus='B','',[ AND (Style.Status="] + lcRpStatus + [")])

lcRpExp =  lcRepExpr + IIF( '.T.' $ lcRpExp , '' , [ AND ] + lcRpExp )

XTitle = lcRpOpTlt   && OPTIONAL TITLE FROM OG.

DIMENSION laGrndTot[7,9]    && Array to hold all grand totals.
STORE 0         TO laGrndTot   && set the values of grand totals.

*C102166,4 AME [START] Flag to show wether we printed any data or not
llWrDl = .F.  && in start flag down.
*C102166,4 AME [End]

SELECT STYLE
SET RELATION TO 'S'+Scale INTO Scale ADDITIVE

*---- check if there's any style that meets filter condition.
SET FILTER TO &lcRpExp
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SET FILTER TO


*--- the grand totals lables for detailed report.
lcTitStr1 = IIF(lcRpWipPln ='W','WIP ...........','PLAN...........')   && line1 title
lcTitStr2 = 'HANGING STOCK..'                                          && line2 title
lcTitStr3 = 'TOTAL AVAIL ...'										   && line3 title	
lcTitStr4 = 'OPEN ORDERS ...'										   && line4 title   
lcTitStr5 = 'OPEN TO SELL ..'										   && line5 title
lcTitStr6 = 'YTD SHIPPED ...'										   && line6 title
lcTitStr7 = 'BOOKED ........'										   && line7 title	

*********************************************************************
***   CUT & SOLD  REPORT
*********************************************************************

R_WIDTH = 'W'
R_TITLE = 'CUT & SOLD'


SET DEVICE TO PRINT
PAGENO = 0  

IF llCutSolSz
  llFirsTime = .T.  && flag to indicate the first time entering.
  lcSavSty =' '     && variable takes style (without color)

  SCAN FOR &lcRpExp   && main loop scan (in detailed report)
    *--- do This lineS of Codes just the first time
    IF llFirsTime 
      llFirsTime = .F.
      lcSavSty =' '
      PAGENO = PAGENO+1
      DO RPT_HDR WITH 'ICXES500',XTITLE,R_WIDTH
      lnRow    = 5        &&variable holds row No.
      lcPerPage  = 1    && The style No./page 
      lcMaxPp = 5       && MaxNumber of Styles in one page
    ENDIF
    
    IF lcPerPage > lcMaxPp  && if exceed the max no of styles/page.
      PAGENO   = PAGENO+1
      DO RPT_HDR WITH 'ICXES500',XTITLE,R_WIDTH
      lnRow      = 5
      lcPerPage  = 1
      lcSavSty = ' '
    ENDIF
      
    WAIT WINDOW 'Collecting Detailed Data Style:..  '  + STYLE  NOWAIT
  
    *** FILTER OUT MINIMUM O-T-S REQUEST

    IF lnRpMinQty <> 0
      STORE 0 TO POSOTS, NEGOTS   && variables hold -ve & +ve OTS Qtys
      
      *-----------  calculate both +ve & -ve  OTS.
      FOR X=1 To 8
        Z=STR(X,1)
        WKTOT = IIF(lcRpWipPln='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z  
        IF WKTOT >0
          POSOTS = POSOTS + WKTOT
        ELSE
          NEGOTS = NEGOTS + WKTOT
        ENDIF
      ENDFOR
      
      *--- in case under min. qty. skip this style.
      DO CASE
        CASE lnRpMinQty >0 .AND. POSOTS<lnRpMinQty
          LOOP
        CASE lnRpMinQty <0 .AND. NEGOTS>lnRpMinQty
          LOOP
      ENDCASE
   
    ENDIF
    *C102166,4 AME [START] iF WE DEALD with file by any
    *C102166,4 AME         printing rise the deal with flag. 
    llWrDl = .T.
    *C102166,4 AME [End]
    lnPriceA = PRICEA          && hold this style price.

    *----- if the style change print headr 
    IF CStyMajor<>lcSavSty
      lcSavSty = CStyMajor
      @ lnRow,00 SAY 'STYLE:'
      @ lnRow,07 SAY CStyMajor
      @ lnRow,15 SAY DESC
      @ lnRow,50 SAY 'SEA/DIV: ' + SEASON + '/' + CDIVISION
      lnRow = lnRow+2
      @ lnRow,00 SAY 'CLR ...CLR.DESCR...'
      @ lnRow,22 SAY 'SCALE: ' + '(' + SCALE + ')'
      *---- claculate scale string.
      lcStr = ""
      FOR I = 1 TO SCALE.CNT
        Z =  STR(I,1)
        IF EMPTY(SCALE.SZ&Z)
          EXIT
        ELSE
          lcStr = lcStr+ALLTRIM(SCALE.SZ&Z)+IIF(I=SCALE.CNT,"",SPACE(6-len(ALLTRIM(SCALE.SZ&Z))))
        ENDIF
      ENDFOR
      lcStr = ALLTRIM(lcStr)
    
      @ lnRow,40  SAY lcStr
      @ lnRow,90  SAY 'TOT'
      @ lnRow,97  SAY '$ VALUE'        && RAA ON 05/17/93
      @ lnRow,106 SAY 'NEG-OTS POS-OTS NET-OTS'
      lnRow=lnRow+1
    ENDIF
    STORE ' ' TO lcFld1,lcFld2,lcFld3    && variables to hold fields to print names.
     
    FOR Y = 1 TO 7   && main printing loop (details)
      *---- the data of one style are printed in 7 lines
      ***********************************************************************************************************************************************
      *       :0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012*
      *line1  :Clr clr description   WIP ........... wip1  wip2  wip3  wip4  wip5  wip6  wip7  wip8  9totwip   99999999                             * 
      * also line1  can be                                                                                                                          *     
      *line1  :Clr clr description   PLAN .......... pln1  pln2  pln3  pln4  pln5  pln6  pln7  pln8  9totpln   99999999                             *
      *line2  :                      HANGING STOCK.. stk1  stk2  stk3  stk4  stk5  stk6  stk7  stk8  9999999   99999999                             *
      *line3  :                      TOTAL AVAIL ... avl1  avl2  avl3  avl4  avl5  avl6  avl7  avl8  9999999   99999999                             *
      *line4  :                      OPEN ORDERS ... ord1  ord2  ord3  ord4  ord5  ord6  ord7  ord8  9999999   99999999                             *
      *line5  :                      OPEN TO SELL .. ots1  ots2  ots3  ots4  ots5  ots6  ots7  ots8                      9negots 9posots 9netots    *
      *line6  :                      YTD SHIPPED ... shp1  shp2  shp3  shp4  shp5  shp6  shp7  shp8  9999999   99999999                             *
      *line7  :                      BOOKED ........ bok1  bok2  bok3  bok4  bok5  bok6  bok7  bok8  9999999   99999999                             *
      ***********************************************************************************************************************************************
      DO CASE    && in this do case we print line title and get the right field to print.
        CASE Y=1   && line one print color - color description - (wip title or plan title ) => (wip or plan) 
          @ lnRow,00 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
          lcColr=SUBSTR(STYLE,lnNonMajSt,lnColorLen)
          lcClrData = gfcodDes(lcColr,"COLOR")
          IF !EMPTY(lcClrdata)
            @lnRow,6 SAY LEFT(lcClrData,16)              && Color Description
          ENDIF
          SELE STYLE
          @ lnRow,22 SAY lcTitStr1                     
          lcFld1 = IIF(lcRpWipPln='W','WIP','PLAN')  
        CASE Y=2
          @ lnRow,22 SAY 'HANGING STOCK..'
          lcFld1='STK'
        CASE Y=3              && line three print             (Total Available  ) => (stk + (wip or plan))
          @ lnRow,22 SAY 'TOTAL AVAIL ...'
          lcFld1=IIF(lcRpWipPln='W','WIP','PLAN')  
          lcFld2='STK'
        CASE Y=4
          @ lnRow,22 SAY 'OPEN ORDERS ...'
          lcFld1='ORD'
        CASE Y=5
          @ lnRow,22 SAY 'OPEN TO SELL ..'     && line  five prints  (open to sell )   => (stk + (wip or plan) - ord)
          lcFld1=IIF(lcRpWipPln='W','WIP','PLAN')  
          lcFld2='STK'
          lcFld3='ORD'
        CASE Y=6
          @ lnRow,22 SAY 'YTD SHIPPED ...'
          lcFld1='SHP'
        CASE Y=7
          @ lnRow,22 SAY 'BOOKED ........'    && line seven prints  (Booked )   => (ord + shp )
          lcFld1='ORD'
          lcFld2='SHP'
      ENDCASE

      FOR X = 1 TO 8    && in this for loop we print the 8 sizes data according to the field(s) we choosed 
                        && then we add that value to the grand totals matrix.
        Z=STR(X,1)
        DO CASE
          CASE lcFld3<>' '
            @ lnRow,(X*6)+32 SAY &lcFld1.&Z + &lcFld2.&Z - &lcFld3.&Z PICTURE '@Z 99999'    && (wip or plan) +  stk - ord      in case of open to sell line
            laGrndTot[Y,X] = laGrndTot[Y,X] + &lcFld1.&Z + &lcFld2.&Z - &lcFld3.&Z
          CASE lcFld2<>' '
            @ lnRow,(X*6)+32 SAY &lcFld1.&Z + &lcFld2.&Z            PICTURE '@Z 99999'       && (add the two fields (in case of total avi.  stock + (wip or plan)
            																				 &&  and in case of booked (order + shiped)		
            laGrndTot[Y,X] = laGrndTot[Y,X] + &lcFld1.&Z + &lcFld2.&Z
          OTHERWISE                                                                          && in cases of one field print it
            @ lnRow,(X*6)+32 SAY &lcFld1.&Z                       PICTURE '@Z 99999'
            laGrndTot[Y,X] = laGrndTot[Y,X] + &lcFld1.&Z
        ENDCASE
      ENDFOR

      DO CASE   && in this  do case we print the total of the field(s) printed in the same line.
        CASE Y=1
          @ lnRow,86 SAY IIF(lcRpWipPln='W',TOTWIP,TOTPLAN) PICTURE '9999999'               
          @ lnRow,96 SAY (IIF(lcRpWipPln='W',TOTWIP,TOTPLAN) * lnPriceA ) PICTURE '99999999' 
          laGrndTot[1,9] = laGrndTot[1,9] + (IIF(lcRpWipPln='W',TOTWIP,TOTPLAN) * lnPriceA)    && add to grdtot mat. (totwip  amnt. or totplan amnt.)
        CASE Y=2
          @ lnRow,86 SAY TOTSTK                   PICTURE '9999999'
          @ lnRow,96 SAY TOTSTK * lnPriceA         PICTURE '99999999'
          laGrndTot[2,9] = laGrndTot[2,9] + (TOTSTK * lnPriceA)             &&  add to grdtot mat. total stk amnt.
        CASE Y=3
          @ lnRow,86 SAY IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)+TOTSTK PICTURE '9999999'             
          @ lnRow,96 SAY (IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)+TOTSTK)*lnPriceA  PICTURE '99999999' 
          laGrndTot[3,9] = laGrndTot[3,9] + ((IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)+TOTSTK)*lnPriceA)    && add to grdtot mat. total available amnt. 
        CASE Y=4
          @ lnRow,86 SAY TOTORD                   PICTURE '9999999'
          @ lnRow,96 SAY TOTORD * lnPriceA         PICTURE '99999999'
          laGrndTot[4,9] = laGrndTot[4,9] + (TOTORD * lnPriceA)   && add to grdtot mat. totord amnt.
        CASE Y=5
          STORE 0 TO NEGOTS,POSOTS,NETOTS
          FOR X = 1 TO 8
            Z=STR(X,1)
            WKTOT = IIF(lcRpWipPln='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z      
            IF WKTOT >0
              POSOTS = POSOTS + WKTOT
            ELSE
              NEGOTS = NEGOTS + WKTOT
            ENDIF
            NETOTS = NETOTS + WKTOT
          ENDFOR
          IF NEGOTS <>0
            @ lnRow,106 SAY NEGOTS PICTURE '9999999'
          ENDIF
          @ lnRow,114 SAY POSOTS   PICTURE '9999999'
          @ lnRow,122 SAY NETOTS   PICTURE '9999999'
        CASE Y=6
          @ lnRow,86 SAY TOTSHP    PICTURE '9999999'
          @ lnRow,96 SAY TOTSHP * lnPriceA   PICTURE '99999999'
          laGrndTot[6,9] = laGrndTot[6,9] + (TOTSHP * lnPriceA)       && add to grdtot mat. totship amnt.
        CASE Y=7
          @ lnRow,86 SAY TOTORD+TOTSHP PICTURE '9999999'
          @ lnRow,96 SAY (TOTORD+TOTSHP)*lnPriceA  PICTURE '99999999'
          laGrndTot[7,9] = laGrndTot[7,9] + ((TOTORD+TOTSHP)*lnPriceA)  && add to grdtot mat. totbooked amnt.
      ENDCASE
      lnRow=lnRow+1   && increment line No. in the same style.
      STORE ' ' TO lcFld1,lcFld2,lcFld3     && reset fields names. 
    ENDFOR   &&  Y =1 to 7 main printing loop (details)
    lcPerPage = lcPerPage +1     && increment No. of styles that were printed in this page.
    *C102166,4 AME [START] space by one line.
    lnRow = lnRow + 1
    *C102166,4 AME [END] 
  ENDSCAN     && style for &lcrpexp  (main detailes loop)
  *C102166,4 AME [Start] If We didn't deal with file by any printing
  *C102166,4             Dont print totals
  IF !llWrDl
    SET DEVICE TO SCREEN
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF   
  *C102166,4 AME [End]
  
  
  *****************PRINTING GRAND TOTALS**************************************
  ******************For Detailed Report***************************************
  lnRow = lnRow + 1       && increment line No. to separate from prev. lines
  @ lnRow,00 SAY REPLICATE('=',131)
  lnRow = lnRow + 1       && increment line No. again to separate from totals.
  @ lnRow,00 SAY '** GRAND TOTAL **'
  
  FOR Y = 1 TO 7  && printing grand totals loop.
    lnTotQty = 0       && variable holds total of this line's totals
    lcTitStr = 'lcTitStr'+ALLTRIM(STR(Y))   && variable gets the title name form lctitstr1 to lctitstr7
    @ lnRow,22 SAY &lcTitStr                && print title.  of this line.
    FOR X = 1 TO 8
      @ lnRow,(X*6)+32 SAY laGrndTot[Y,X]  PICTURE '@Z 999999'  && print grdtot mat. every element i ite propre place.
      lnTotQty = lnTotQty + laGrndTot[Y,X]                      && calculate total of this line's totals
    ENDFOR
    @ lnRow,86       SAY lnTotQty       PICTURE '9999999'    && print total of this line's totals
    @ lnRow,96       SAY laGrndTot[Y,9]  PICTURE '99999999'  && print grdtot amnt.     
    lnRow = lnRow + 1  
  ENDFOR  
ENDIF

************************ END CUT & SOLD BY SIZE *******************
  
IF !llCutSolSz .OR. (llCutSolSz .AND. llRecap)    &&  if not print by size or print by size with recape. to yes.
  
  
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+
  *    CLR ...CLR.DESCR...  DIV    SEA        WIP     STOCK     AVAIL    ORDERS     O-T-S   SHIPPED     BOOKED  
  *    123 123456789012345  XXXXXX XXXXXX99999999  99999999  99999999  99999999  99999999  99999999  999999999
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8

  *---- check if there's any style that meets filter condition.
  SELECT STYLE 
  SET FILTER TO &lcRpExp
  GO TOP
  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
  SET FILTER TO

  R_TITLE   = [ CUT & SOLD SUMMARY]
  lnRow      = 5    &&variable holds row No.
  MAX_LINES = 58    && max No. of lines in one page.
  
  STORE 0 TO TOT2,TOT3,TOT4,TOT5,TOT6,TOT8,TOT9          && totals
  STORE 0 TO SUBTOT2,SUBTOT3,SUBTOT4,SUBTOT5,SUBTOT6,SUBTOT8,SUBTOT9  && subtotals

  *---- claculate scale string.
  lcScale = ""
  FOR I = 1 TO SCALE.CNT
    Z =  STR(I,1)
    IF EMPTY(SCALE.SZ&Z)
      EXIT
    ELSE
      lcScale = lcScale+SCALE.SZ&Z+IIF(I=SCALE.CNT,"","    ")
    ENDIF
  ENDFOR
  lcScale = ALLTRIM(lcScale)
  
  lcSub_Save  = CStyMajor + SPACE(5) + lcScale   &&var . to indicate change in style maj. or scale str. 

  PAGENO = PAGENO +1    
  DO RPT_HDR WITH 'ICXES500',XTITLE,R_WIDTH    && print header AFTER increment pageno.


  LINENO = 11
  @ lnRow,0 SAY ''
  lnRow = lnRow + 1
  @ lnRow,0 SAY '    CLR ...CLR.DESCR...  DIV    SEA       '+IIF(lcRpWipPln='W',' WIP ','PLAN ')+'    STOCK     AVAIL    ORDERS     O-T-S   SHIPPED     BOOKED'  
  lnRow = lnRow + 1
  @ lnRow,0 SAY ''
  lnRow = lnRow + 2    
  @ lnRow,0 SAY '** '
  @ lnRow,3 SAY CStyMajor + SPACE(2) + STYLE.DESC
  
  SCAN FOR &lcRpExp   && main loop (in summary report)
    
    *** FILTER OUT MINIMUM O-T-S REQUEST
    IF lnRpMinQty <> 0
      STORE 0 TO POSOTS, NEGOTS
      FOR X=1 To 8
        Z=STR(X,1)
        WKTOT = IIF(lcRpWipPln='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z  
        IF WKTOT >0
          POSOTS = POSOTS + WKTOT
        ELSE
          NEGOTS = NEGOTS + WKTOT
        ENDIF
      ENDFOR
      DO CASE
        CASE lnRpMinQty >0 .AND. POSOTS<lnRpMinQty
          LOOP
        CASE lnRpMinQty <0 .AND. NEGOTS>lnRpMinQty
          LOOP
      ENDCASE
    ENDIF
    *C102166,4 AME [START] iF WE DEALD with file by any
    *C102166,4 AME         printing rise the deal with flag. 
    llWrDl = .T.
    *C102166,4 AME [End]

    ************************************************************
    *---- claculate scale string.
    lcScale = ""
    FOR I = 1 TO SCALE.CNT
      Z =  STR(I,1)
      IF EMPTY(SCALE.SZ&Z)
        EXIT
      ELSE
        lcScale = lcScale+SCALE.SZ&Z+IIF(I=SCALE.CNT,"","    ")
      ENDIF
    ENDFOR
    lcScale = ALLTRIM(lcScale)

    *--- if sty maj or scale strin have changed.
    *----- print subtotals 
    IF lcSub_Save  <> CStyMajor + SPACE(5) + lcScale

      lnRow = lnRow + 2
      @ lnRow,0 SAY '**Subtotal** '
      @ lnRow,38 SAY SUBTOT2   PICTURE '99999999'
      @ lnRow,48 SAY SUBTOT3   PICTURE '99999999'
      @ lnRow,58 SAY SUBTOT4   PICTURE '99999999'
      @ lnRow,68 SAY SUBTOT5   PICTURE '99999999'
      @ lnRow,78 SAY SUBTOT6   PICTURE '99999999'
      @ lnRow,88 SAY SUBTOT8   PICTURE '99999999'
      @ lnRow,98 SAY SUBTOT9   PICTURE '999999999'  
      LINENO=LINENO+5

      *---- Reclaculate scale string.
      lcScale = ""
      FOR I = 1 TO SCALE.CNT
        Z =  STR(I,1)
        IF EMPTY(SCALE.SZ&Z)
          EXIT
        ELSE
          lcScale = lcScale+SCALE.SZ&Z+IIF(I=SCALE.CNT,"","    ")
        ENDIF
      ENDFOR
      lcScale = ALLTRIM(lcScale)
       
      *-------re_evaluate lcsub_save
      lcSub_Save  = CStyMajor + SPACE(5) + lcScale
      
      *---- add sub totals values to total values.       
      TOT2     = TOT2 + SUBTOT2
      TOT3     = TOT3 + SUBTOT3
      TOT4     = TOT4 + SUBTOT4
      TOT5     = TOT5 + SUBTOT5
      TOT6     = TOT6 + SUBTOT6
      TOT8     = TOT8 + SUBTOT8
      TOT9     = TOT9 + SUBTOT9           

      STORE 0 TO SUBTOT2,SUBTOT3,SUBTOT4,SUBTOT5,SUBTOT6,SUBTOT8,SUBTOT9  && set subtotals to 0 agian.
      
      *--- if still within range of lines.
      *--- print sty maj + scale str +desc.
      IF  (LINENO < MAX_LINES)
        lnRow = lnRow + 2
        @ lnRow,0 SAY '** '
        *---- claculate scale string.      
        lcScale = ""
        FOR I = 1 TO SCALE.CNT
          Z =  STR(I,1)
          IF EMPTY(SCALE.SZ&Z)
            EXIT
          ELSE
            lcScale = lcScale+SCALE.SZ&Z+IIF(I=SCALE.CNT,"","    ")
          ENDIF
        ENDFOR
        lcScale = ALLTRIM(lcScale)

        *c102166,4 ame[Start]
        *@ lnRow,3 SAY CStyMajor + SPACE(5) + lcScale + SPACE(2) + STYLE.DESC
        @ lnRow,3 SAY CStyMajor + SPACE(2) + STYLE.DESC
        *C102166,4 ame [end]
      ENDIF
   
    ENDIF  && if change sty maj or scale str.
    
    *--- if line no exceeds max No.
    *---- print headrd again.
    IF LINENO >= MAX_LINES         

      EJECT

      PAGENO=PAGENO+1      
      DO RPT_HDR WITH 'ICXES500',XTITLE,R_WIDTH

      lnRow = 5
        
      LINENO=11
      lnRow = lnRow +1
      @ lnRow,0 SAY ''
      lnRow = lnRow +1
      @ lnRow,0 SAY '    CLR ...CLR.DESCR...  DIV    SEA       '+IIF(lcRpWipPln='W',' WIP ','PLAN ')+'    STOCK     AVAIL    ORDERS     O-T-S   SHIPPED     BOOKED' 
      lnRow = lnRow +1
      @ lnRow,0 SAY ''
      lnRow = lnRow +1
      @ lnRow,0 SAY '** '
      
      lcScale = ""
      FOR I = 1 TO SCALE.CNT
        Z =  STR(I,1)
        IF EMPTY(SCALE.SZ&Z)
          EXIT
        ELSE
          lcScale = lcScale+SCALE.SZ&Z+IIF(I=SCALE.CNT,"","    ")
        ENDIF
      ENDFOR
      lcScale = ALLTRIM(lcScale)
      *c102166,4 ame[Start]
      *@ lnRow,3 SAY CStyMajor + SPACE(5) + lcScale + SPACE(2) + STYLE.DESC
      @ lnRow,3 SAY CStyMajor + SPACE(2) + STYLE.DESC
      *C102166,4 ame [end]
    ENDIF
    
    
    *------------ printing main data  of the style
    WAIT WINDOW "Collecting Data...Style: "+Style NOWAIT      

    
    lnTot1 = ((IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)+TOTSTK-TOTORD) + ABS(IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)+TOTSTK-TOTORD))/2     && zero in case of -ve values.
    
    lnRow = lnRow + 1
    
    @ lnRow,0      SAY SPACE(4)
    @ lnRow,4      SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    lcColr=SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    lcClrData = gfcodDes(lcColr,"COLOR")
    IF !EMPTY(lcClrdata)
      @ lnRow,10 SAY LEFT(lcClrData,15)            && Color Description
    ENDIF
    @ lnRow,25       SAY CDIVISION
    @ lnRow,32       SAY SEASON
    @ lnRow,38       SAY IIF(lcRpWipPln='W',TOTWIP,TOTPLAN) PICTURE '99999999'         
    @ lnRow,48       SAY TOTSTK           PICTURE '99999999'
    @ lnRow,58       SAY IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)+TOTSTK PICTURE '99999999'  
    @ lnRow,68       SAY TOTORD           PICTURE '99999999'
    @ lnRow,78       SAY lnTot1            PICTURE '99999999'
    @ lnRow,88       SAY TOTSHP           PICTURE '99999999'
    @ lnRow,98       SAY TOTORD+TOTSHP    PICTURE '999999999'  
    LINENO=LINENO+1

    *----- add values to subtotals 
    SUBTOT2 = SUBTOT2 + IIF(lcRpWipPln='W',TOTWIP,TOTPLAN)               
    SUBTOT3 = SUBTOT3 + TOTSTK
    SUBTOT4 = SUBTOT4 + IIF(lcRpWipPln='W',TOTWIP,TOTPLAN) + TOTSTK      
    SUBTOT5 = SUBTOT5 + TOTORD
    SUBTOT6 = SUBTOT6 + lnTot1
    SUBTOT8 = SUBTOT8 + TOTSHP
    SUBTOT9 = SUBTOT9 + TOTORD + TOTSHP   

  ENDSCAN   && style for &lcrpexp main loop (in summary report)
  *C102166,4 AME [Start] If We didn't deal with file by any printing
  *C102166,4             Dont print totals
  IF !llWrDl
    SET DEVICE TO SCREEN
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
  *C102166,4 AME [End]
  *------print subtotals  of the last style/scale.
  lnRow = lnRow +2
  @ lnRow,0 SAY '**Subtotal** '

  @ lnRow,38 SAY SUBTOT2   PICTURE '99999999'
  @ lnRow,48 SAY SUBTOT3   PICTURE '99999999'
  @ lnRow,58 SAY SUBTOT4   PICTURE '99999999'
  @ lnRow,68 SAY SUBTOT5   PICTURE '99999999'
  @ lnRow,78 SAY SUBTOT6   PICTURE '99999999'
  @ lnRow,88 SAY SUBTOT8   PICTURE '99999999'
  @ lnRow,98 SAY SUBTOT9   PICTURE '999999999'      
  
  *----- add values to totals 
  TOT2     = TOT2 + SUBTOT2
  TOT3     = TOT3 + SUBTOT3
  TOT4     = TOT4 + SUBTOT4
  TOT5     = TOT5 + SUBTOT5
  TOT6     = TOT6 + SUBTOT6
  TOT8     = TOT8 + SUBTOT8
  TOT9     = TOT9 + SUBTOT9               
 
  *------ print grand totals.
  lnRow = lnRow +2
  @ lnRow,0 SAY '*** Total ***'
  @ lnRow,38 SAY TOT2  PICTURE '99999999'
  @ lnRow,48 SAY TOT3  PICTURE '99999999'
  @ lnRow,58 SAY TOT4  PICTURE '99999999'
  @ lnRow,68 SAY TOT5  PICTURE '99999999'
  @ lnRow,78 SAY TOT6  PICTURE '99999999'
  @ lnRow,88 SAY TOT8  PICTURE '99999999'
  @ lnRow,98 SAY TOT9  PICTURE '999999999'     
  lnRow = lnRow +1
  @ lnRow,0 SAY [] &&just to get last line from some printers

ENDIF

*----Print the report ,clean up & return.
DO ENDREPORT
SET DEVICE TO SCREEN


*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]

      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
       
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                   laMajSegs[lnI,3],;
                   lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                   PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                   lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF                     

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  lcShowVar = 'Show details by ' + lcNonMajTl
  lcStyStat = lcStyMajor + ' Status'
  lcStyMake = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.



*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.


*!*************************************************************
*! Name      : lfStySum
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvFabric.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfVCutSol
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 02/04/2001
*! Purpose   : Valid function of Option Print Cut& SOld by Size.
*!*************************************************************
*! Called from : Option Grid. 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfVCutSol()
*!*************************************************************
FUNCTION lfVCutSol
CLEAR READ
*-- end of lfVCutSol.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.  && Rise clear done flag.
*-- end of lfClearRep.
