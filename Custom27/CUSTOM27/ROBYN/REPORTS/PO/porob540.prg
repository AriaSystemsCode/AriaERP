**************************************************************************
*: Program file  : POROB540.PRG (C# 101341)
*: Program desc. : STYLE PURCHASE ORDER RECEIPTS REPORT for (ROB100)
*:                 Convert ROB5400 from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls :PROCEDURES :
*:          lpNoCosting.
*:        FUNCTIONS  : 
*:          lfHeader,lfStyTot,lfColTot,lfwRepWhen,lfvVendor,lfvPO,lfvStyle
*:          lfvWareHo,lfvCostDtl,lfvPrnForn,lfvCurr,lfMajTtGet,lfNonMaj
*:          lfMajPic,lfChkMulWH,lfChkMulCu.
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************

*:C101341,1 SSH laCost array that hold costing items label
lnMajLen = LEN(gfItemMask('PM'))
lcStyHdr = gfItemMask("HM")
DIMENSION laCost [5]
STORE SPACE(9) TO laCost [5]

*:C101341,1 SSH  RESTORE Memory variables (Start)
DIMENSION laSetUps[5,2]
laSetUps[1,1]  = 'M_CISLBL1'
laSetUps[2,1]  = 'M_CISLBL2'
laSetUps[3,1]  = 'M_CISLBL3'
laSetUps[4,1]  = 'M_CISLBL4'
laSetUps[5,1]  = 'M_CISLBL5'
= gfGetMemVar(@laSetups)
laCost[1] = LEFT(laSetUps[1,2],9)
laCost[2] = LEFT(laSetUps[2,2],9)
laCost[3] = LEFT(laSetUps[3,2],9)
laCost[4] = LEFT(laSetUps[4,2],9)
laCost[5] = LEFT(laSetUps[5,2],9)
lcWareCode = SPACE(06)
R_WIDTH  = 'W'                                  && Standard Report is Wide
XREPORT  = 'POROB540'
R_TITLE  = 'STYLE PURCHASE ORDER RECEIPTS REPORT'
*:C101341,1 SSH variable to hold maximum line
MAXROW    = 53
*:C101341,1 SSH  Variables to hold total quantity per style,color,PO and total per report.
STORE 0 TO lnTotSRec,lnfTotRec, lnTotRec,lnTotCRec,lnActu
*:C101341,1 SSH  array to hold total cost amounts per style
DIMENSION laSubS[5,3]
*:C101341,1 SSH  array to hold total cost amounts per color
DIMENSION laSubC[5,3]
*:C101341,1 SSH array to hold the actual cost for each cost element.
DIMENSION laEAct[5]
STORE  0 TO laSubS,laSubC,laEAct

*:C101341,1 SSH Restore costing items name.
DIMENSION laFinal[5,3]
DIMENSION laSub[5,3]
STORE 0 TO laFinal ,laSub, lnX,lnY,lnZ
*:C101341,1 SSH Initializing Variables
XTITLE  = lcRpTitle
XTRNCD  = '2'
lcCstDtl= IIF(llRPCostDt,'Y','N')
XFILTER = lcRpExp
XFILTER = XFILTER + '.AND.TRANCD$XTRNCD'
SORTFIELD = ' '
BREAK     = ' '
BREAKD    = ' '
DO CASE
  CASE lcRPSortBy='P'
    SORTFIELD = 'PO+TRANCD+STYLE'
    BREAK     = 'PO'
    BREAKD    = 'PO'
 CASE lcRPSortBy='V'
   SORTFIELD = 'POSHDR.VENDOR+PO+TRANCD+STYLE'
   BREAK     = 'POSHDR.VENDOR'
   BREAKD    = 'VENDOR'
 CASE lcRPSortBy='S'
   SORTFIELD = 'SUBSTR(STYLE,1,lnMajLen)+TRANCD+SUBSTR(Style,lnClrPo,lnColorLen)+PO'
   BREAK     = 'SUBSTR(STYLE,1,lnMajLen)'
   BREAKD    = 'SUBSTR(STYLE,1,lnMajLen)'
 CASE lcRPSortBy='O'
   SORTFIELD = 'STYLE+TRANCD+PO'
   BREAK     = 'STYLE'
   BREAKD    = 'Color'
 CASE lcRPSortBy='H'
   SORTFIELD = 'SHIPNO+CRSESSION+CWARECODE+STYLE'
   BREAK     = 'SHIPNO'
   BREAKD    = 'SHIPNO'
ENDCASE

*:C101341,1 SSH Do not print costing details
IF lcCstDtl = 'N'
  DO lpNoCosting
  RETURN
ENDIF
*:C101341,1 SSH Report Loop Begin
DO WHILE .T.
  SELECT POSLN
  SET FILTER TO
  SET RELATION TO
  SET RELATION TO cstytype+po INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  WAIT WINDOW  'Selecting records for report ...' NOWAIT
  LOCATE ALL FOR &XFILTER
  IF EOF()
    *:C101341,1 SSH 'Ther are no records to display .'
    = gfModalGen('TRM00052B00000','DIALOG' )
    RETURN
  ENDIF
  WORKFILE = gfTempName()
  SET TALK ON
  COPY REST TO &gcWorkDir.&WORKFILE FOR &XFILTER
  SET TALK OFF
  SELECT POSLN
  SET RELATION TO
  =gfOpenFile('&gcWorkDir.&WORKFILE',' ','EX')
  SET RELATION TO cstytype+po INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  *:C101341,1 SSH Sort to Workfile Index
  IF SORTFIELD<>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW  'SORTING &Z RECORDS FOR STYLE PURCHASE ORDER DETAIL REPORT ...' NOWAIT
    INDEX ON &SORTFIELD TAG &WORKFILE
    SET ORDER TO TAG &WORKFILE
  ENDIF
  *:C101341,1 SSH Variables Initialization
  XTIME  = TIME()
  PAGENO = 0
  ROW    = 99
  SELECT &WORKFILE
  GOTO TOP
  IF BREAKD = 'SHIPNO'
    DELETE ALL FOR EMPTY(SHIPNO)
    GOTO TOP
    IF EOF()
      *:C101341,1 SSH 'Ther are no records to display .'
      = gfModalGen('TRM00052B00000','DIALOG' )      
      RETURN
    ENDIF
  ENDIF
  IF LEN(TRIM(BREAK)) <> 0
    HBREAK = &BREAK
  ENDIF
  *:C101341,1 SSH Begin Printing
  CLEAR TYPEAHEAD
  SET DEVICE TO SCREEN
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  SET DEVICE TO PRINT
  SELECT &WORKFILE
  R_Title = R_Title + IIF( llMultiWH .AND. !EMPTY( lcWareCode ),;
                          ' FOR WAREHOUSE: '+ lcWareCode , '')
  lcStyle = SUBSTR(STYLE,1,lnMajLen)
  lcColor = SUBSTR(Style,lnClrPo,lnColorLen)

  *:C101341,1 SSH BEGIN [MAIN REPORT] LOOP
  DO WHILE INKEY() <> 32
    lnActu = 0
    DO CASE
      CASE (BREAKD # 'Color' AND SUBSTR(STYLE,1,lnMajLen) # lcStyle) 
        =lfColTot()
        =lfStyTot()
        lcStyle = SUBSTR(STYLE,1,lnMajLen)
        lcColor = SUBSTR(Style,lnClrPo,lnColorLen)
      CASE BREAKD # 'Color' AND SUBSTR(STYLE,1,lnMajLen)+SUBSTR(Style,lnClrPo,lnColorLen) # lcStyle+lcColor AND &BREAK = HBREAK
        =lfColTot()
        lcColor = SUBSTR(Style,lnClrPo,lnColorLen)
      CASE &BREAK # HBREAK AND !INLIST(BREAKD,'SUBSTR(STYLE,1,lnMajLen)','Color')
        =lfColTot()
        =lfStyTot()
        lcStyle = SUBSTR(STYLE,1,lnMajLen)
        lcColor = SUBSTR(Style,lnClrPo,lnColorLen)
    ENDCASE
    IF ROW >=53
      =lfHeader()
    ENDIF
    *:C101341,1 SSH Begin Subtotals Loop
    IF LEN(TRIM(BREAK)) <>0 AND &BREAK <> HBREAK
      ROW=ROW+1
      DO CASE
        CASE BREAKD ='PO'
          lnSort    = 'PO'
        CASE BREAKD ='VENDOR'
          lnSort    = 'VENDOR'
        CASE BREAKD ='SUBSTR(STYLE,1,lnMajLen)'
          lnSort    = lcStyHdr
        CASE BREAKD ='Color'
          lnSort    = 'COLOR'
        CASE BREAKD ='SHIPNO'
          lnSort    = 'SHIPNO'
      ENDCASE
      @ ROW,00     SAY REPLICATE('*',(132-LEN(' TOTAL FOR '+lnSort+' '+HBREAK))/2) +;
           ' TOTAL FOR '+lnSort+SPACE(1)+HBREAK+SPACE(1) 
      @ ROW,PCOL() SAY REPLICATE('*',132-PCOL())
      ROW=ROW+1
      @ ROW,000 SAY '**SUB TOTAL**'
      @ ROW,116 SAY lnTotRec    PICTURE '99999999'
      ROW=ROW+1
      laFinal[1,1] = laFinal[1,1] + laSub[1,1]
      laFinal[1,2] = laFinal[1,2] + laSub[1,2]
      laFinal[1,3] = laFinal[1,3] + laSub[1,3]
      @ROW ,003 SAY laCost[1]
      @ROW ,15 SAY laSub[1,1]           PICTURE '9999999.999'
      @ROW ,29 SAY laSub[1,2]           PICTURE '9999999.999'
      @ROW ,43 SAY laSub[1,3]           PICTURE '9999999.999'
      @ROW ,57 SAY laSub[1,1]/lnTotRec  PICTURE '9999999.999'
      @ROW ,71 SAY laSub[1,2]/lnTotRec  PICTURE '9999999.999'
      @ROW ,85 SAY laSub[1,3]/lnTotRec  PICTURE '9999999.999'
      ROW=ROW+1
      laFinal[2,1] = laFinal[2,1] + laSub[2,1]
      laFinal[2,2] = laFinal[2,2] + laSub[2,2]
      laFinal[2,3] = laFinal[2,3] + laSub[2,3]
      @ROW ,003 SAY laCost[2]
      @ROW ,15 SAY laSub[2,1]           PICTURE '9999999.999'
      @ROW ,29 SAY laSub[2,2]           PICTURE '9999999.999'
      @ROW ,43 SAY laSub[2,3]           PICTURE '9999999.999'
      @ROW ,57 SAY laSub[2,1]/lnTotRec  PICTURE '9999999.999'
      @ROW ,71 SAY laSub[2,2]/lnTotRec  PICTURE '9999999.999'
      @ROW ,85 SAY laSub[2,3]/lnTotRec  PICTURE '9999999.999'
      ROW=ROW+1
      laFinal[3,1] = laFinal[3,1] + laSub[3,1]
      laFinal[3,2] = laFinal[3,2] + laSub[3,2]
      laFinal[3,3] = laFinal[3,3] + laSub[3,3]
      @ROW ,003 SAY laCost[3]
      @ROW ,15 SAY laSub[3,1]           PICTURE '9999999.999'
      @ROW ,29 SAY laSub[3,2]           PICTURE '9999999.999'         
      @ROW ,43 SAY laSub[3,3]           PICTURE '9999999.999'
      @ROW ,57 SAY laSub[3,1]/lnTotRec  PICTURE '9999999.999'
      @ROW ,71 SAY laSub[3,2]/lnTotRec  PICTURE '9999999.999'
      @ROW ,85 SAY laSub[3,3]/lnTotRec  PICTURE '9999999.999'
      ROW=ROW+1
      laFinal[4,1] = laFinal[4,1] + laSub[4,1]
      laFinal[4,2] = laFinal[4,2] + laSub[4,2]
      laFinal[4,3] = laFinal[4,3] + laSub[4,3]
      @ROW ,003 SAY laCost[4]
      @ROW ,15 SAY laSub[4,1]           PICTURE '9999999.999'
      @ROW ,29 SAY laSub[4,2]           PICTURE '9999999.999'
      @ROW ,43 SAY laSub[4,3]           PICTURE '9999999.999'
      @ROW ,57 SAY laSub[4,1]/lnTotRec  PICTURE '9999999.999'
      @ROW ,71 SAY laSub[4,2]/lnTotRec  PICTURE '9999999.999'
      @ROW ,85 SAY laSub[4,3]/lnTotRec  PICTURE '9999999.999'
      ROW=ROW+1
      laFinal[5,1] = laFinal[5,1] + laSub[5,1]
      laFinal[5,2] = laFinal[5,2] + laSub[5,2]
      laFinal[5,3] = laFinal[5,3] + laSub[5,3]
      @ROW ,003 SAY laCost[5]
      @ROW ,15 SAY laSub[5,1]           PICTURE '9999999.999'
      @ROW ,29 SAY laSub[5,2]           PICTURE '9999999.999'
      @ROW ,43 SAY laSub[5,3]           PICTURE '9999999.999'
      @ROW ,57 SAY laSub[5,1]/lnTotRec  PICTURE '9999999.999'
      @ROW ,71 SAY laSub[5,2]/lnTotRec  PICTURE '9999999.999'
      @ROW ,85 SAY laSub[5,3]/lnTotRec  PICTURE '9999999.999'
      ROW = ROW + 1
      @ ROW ,14 SAY ' -----------   -----------   -----------   -----------   -----------   -----------'
      ROW = ROW + 1
      @ ROW , 03 SAY 'Total Cost'
      lnX = laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]+laSub[5,1]
      lnY = laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]+laSub[5,2]
      lnZ = laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]+laSub[5,3]
      @ROW ,15 SAY lnX           PICTURE '9999999.999'
      @ROW ,29 SAY lnY           PICTURE '9999999.999'
      @ROW ,43 SAY lnZ           PICTURE '9999999.999'
      @ROW ,57 SAY lnX/lnTotRec  PICTURE '9999999.999'
      @ROW ,71 SAY lnY/lnTotRec  PICTURE '9999999.999'
      @ROW ,85 SAY lnZ/lnTotRec  PICTURE '9999999.999'
      ROW = ROW+1
      @ ROW ,14 SAY ' ===========   ===========   ===========   ===========   ===========   ==========='
      STORE 0 TO laSub
      lnTotRec = 0
      lnActu   = 0
      HBREAK = &BREAK
    ENDIF
    IF BREAKD = 'Color' AND SUBSTR(STYLE,1,lnMajLen) <> lcStyle
      =lfStyTot()
      lcStyle = SUBSTR(STYLE,1,lnMajLen)
    ENDIF
    IF EOF()
      EXIT
    ENDIF
    DO CASE
      CASE POSHDR.STATUS = 'O'
         lcSTATUS='OPEN'
      CASE POSHDR.STATUS = 'C'
         lcSTATUS= 'COMPLETE'
      CASE POSHDR.STATUS = 'X'
         lcSTATUS='CANCEL'
      CASE POSHDR.STATUS = 'S'
         lcSTATUS= 'CLOSED'
      CASE POSHDR.STATUS = 'H'
         lcSTATUS= 'HOLD'
    ENDCASE
    IF ROW >=53
       ROW = 99
       LOOP
    ENDIF
    SELECT &WORKFILE
    ROW=ROW+1
    @ ROW,000 SAY PO
    @ ROW,007 SAY lcSTATUS
    @ ROW,016 SAY SUBSTR(STYLE,1,lnMajLen)
    @ ROW,037 SAY SUBSTR(Style,lnClrPo,lnColorLen)
    @ ROW,049 SAY POSHDR->VENDOR
    @ ROW,080 SAY SUBSTR(POSHDR.CDIVISION,1,2)
    @ ROW , 93 SAY PosHdr.LINK_CODE + '-'
    @ ROW , 97 SAY IIF(SEEK(POSHDR.LINK_CODE+'013','GL_LINK'),GL_LINK.GLACNT,'')
    SELECT &WORKFILE
    ROW= ROW+1
    @ ROW,000 SAY SHIPNO
    @ ROW,007 SAY &WORKFILE..CWARECODE
    @ ROW,016 SAY IIF(SEEK(STYLE,'STYLE'),SUBSTR(STYLE.DESC,1,19),SPACE(19))
    lcClrDis = gfCodDes(SUBSTR(STYLE,lnClrPo,lnColorLen),'COLOR     ')
    @ ROW,037 SAY SUBSTR(lcClrDis,1,10)
    @ ROW,049 SAY IIF(SEEK(Vendor,'ApVendor'),SUBSTR(ApVendor.CVENCOMP,1,15),SPACE(15))
    lcDivDesc = gfCodDes(STYLE.cDivision,'cDivision')
    @ ROW,080 SAY SUBSTR(lcDivDesc,1,12)
    @ ROW,93 SAY DATE
    @ ROW,116 SAY TOTQTY     PICTURE '99999999'
    ROW=ROW+1
    @ ROW,015 SAY '  Estimated        Landed        Actual      Est/unit      Lan/unit      Act/unit'
    laEAct[1] = IIF(POSHDR.OPEN+POSHDR.RECEIVE > 0,;
                       ROUND(POSHDR.NACT_COST1/(POSHDR.OPEN+POSHDR.RECEIVE),3),0)
    ROW=ROW+1
    laSub[1,1]=laSub[1,1]+(NCOST1*TOTQTY/POSHDR.RATE)      
    laSub[1,2]=laSub[1,2]+(NLAN_CST1*TOTQTY)
    laSub[1,3]=laSub[1,3]+(laEAct[1]*TOTQTY)
    laSubC[1,1] = laSubC[1,1] + (NCOST1*TOTQTY/POSHDR.RATE)
    laSubS[1,1] = laSubS[1,1] + (NCOST1*TOTQTY/POSHDR.RATE)
    laSubC[1,2] = laSubC[1,2] + (NLAN_CST1*TOTQTY)
    laSubS[1,2] = laSubS[1,2] + (NLAN_CST1*TOTQTY)
    laSubC[1,3] = laSubC[1,3] + (laEAct[1]*TOTQTY)      
    laSubS[1,3] = laSubS[1,3] + (laEAct[1]*TOTQTY)      
    @ ROW , 03 SAY laCost[1]
    @ ROW , 15 SAY NCOST1*TOTQTY/POSHDR.RATE      PICTURE '9999999.999'
    @ ROW , 29 SAY NLAN_CST1*TOTQTY              PICTURE '9999999.999'
    @ ROW , 43 SAY laEAct[1]*TOTQTY               PICTURE '9999999.999'
    @ ROW , 57 SAY NCOST1/POSHDR.RATE             PICTURE '9999999.999'      
    @ ROW , 71 SAY NLAN_CST1                     PICTURE '9999999.999'      
    @ ROW , 85 SAY laEAct[1]                      PICTURE '9999999.999'
    lnTotRec  = lnTotRec  + TotQty
    lnfTotRec = lnfTotRec + TotQty
    lnTotSRec = lnTotSRec + TotQty
    lnTotCRec = lnTotCRec + TotQty
    lnActu    = lnactu + laEAct[1]
    ROW = ROW+1
    laSub[2,1]=laSub[2,1]+(NCOST2*TOTQTY)
    laSub[2,2]=laSub[2,2]+(NLAN_CST2*TOTQTY)
    laEAct[2] = IIF(POSHDR.OPEN+POSHDR.RECEIVE > 0,;
                       ROUND(POSHDR.NACT_COST2/(POSHDR.OPEN+POSHDR.RECEIVE),3),0)
    laSub[2,3]=laSub[2,3]+(laEAct[2]*TOTQTY)
    laSubC[2,1]=laSubC[2,1]+(NCOST2*TOTQTY)
    laSubS[2,1]=laSubS[2,1]+(NCOST2*TOTQTY)
    laSubC[2,2]=laSubC[2,2]+(NLAN_CST2*TOTQTY)
    laSubS[2,2]=laSubS[2,2]+(NLAN_CST2*TOTQTY)
    laSubC[2,3]=laSubC[2,3]+(laEAct[2]*TOTQTY)
    laSubS[2,3]=laSubS[2,3]+(laEAct[2]*TOTQTY)
    @ ROW , 03 SAY laCost[2]
    @ ROW , 15 SAY NCOST2 *TOTQTY          PICTURE '9999999.999'
    @ ROW , 29 SAY NLAN_CST2*TOTQTY       PICTURE '9999999.999'
    @ ROW , 43 SAY laEAct[2]*TOTQTY        PICTURE '9999999.999'
    @ ROW , 57 SAY NCOST2				     PICTURE '9999999.999'      
    @ ROW , 71 SAY NLAN_CST2              PICTURE '9999999.999'      
    @ ROW , 85 SAY laEAct[2]               PICTURE '9999999.999'
    lnActu  = lnactu + laEAct[2]
    ROW = ROW+1
    laSub[3,1]=laSub[3,1]+(NCOST3*TOTQTY)
    laSub[3,2]=laSub[3,2]+(NLAN_CST3*TOTQTY)
    laEAct[3] = IIF(POSHDR.OPEN+POSHDR.RECEIVE > 0,;
                       ROUND(POSHDR.NACT_COST3/(POSHDR.OPEN+POSHDR.RECEIVE),3),0)
    laSub[3,3]=laSub[3,3]+(laEAct[3]*TOTQTY)
    laSubC[3,1] = laSubC[3,1] + (NCOST3*TOTQTY)
    laSubS[3,1] = laSubS[3,1] + (NCOST3*TOTQTY)
    laSubC[3,2] = laSubC[3,2] + (NLAN_CST3*TOTQTY)
    laSubS[3,2] = laSubS[3,2] + (NLAN_CST3*TOTQTY)
    laSubC[3,3] = laSubC[3,3] + (laEAct[3]*TOTQTY)
    laSubS[3,3] = laSubS[3,3] + (laEAct[3]*TOTQTY)
    @ ROW , 03 SAY laCost[3]
    @ ROW , 15 SAY NCOST3*TOTQTY           PICTURE '9999999.999'
    @ ROW , 29 SAY NLAN_CST3*TOTQTY       PICTURE '9999999.999'
    @ ROW , 43 SAY laEAct[3]*TOTQTY        PICTURE '9999999.999'
    @ ROW , 57 SAY NCOST3			         PICTURE '9999999.999'      
    @ ROW , 71 SAY NLAN_CST3              PICTURE '9999999.999'      
    @ ROW , 85 SAY laEAct[3]               PICTURE '9999999.999'
    lnActu  = lnactu + laEAct[3]
    ROW = ROW+1
    laSub[4,1]=laSub[4,1]+(NCOST4*TOTQTY)
    laSub[4,2]=laSub[4,2]+(NLAN_CST4*TOTQTY)
    laEAct[4] = IIF(POSHDR.OPEN+POSHDR.RECEIVE > 0,;
                      ROUND(POSHDR.NACT_COST4/(POSHDR.OPEN+POSHDR.RECEIVE),3),0)
    laSub[4,3]=laSub[4,3]+(laEAct[4]*TOTQTY)
    laSubC[4,1]=laSubC[4,1]+(NCOST4*TOTQTY)
    laSubS[4,1]=laSubS[4,1]+(NCOST4*TOTQTY)
    laSubC[4,2]=laSubC[4,2]+(NLAN_CST4*TOTQTY)
    laSubS[4,2]=laSubS[4,2]+(NLAN_CST4*TOTQTY)
    laSubC[4,3]=laSubC[4,3]+(laEAct[4]*TOTQTY)
    laSubS[4,3]=laSubS[4,3]+(laEAct[4]*TOTQTY)
    @ ROW , 03 SAY laCost[4]
    @ ROW , 15 SAY NCOST4*TOTQTY          PICTURE '9999999.999'
    @ ROW , 29 SAY NLAN_CST4*TOTQTY       PICTURE '9999999.999'
    @ ROW , 43 SAY laEAct[4]*TOTQTY       PICTURE '9999999.999'
    @ ROW , 57 SAY NCOST4			      PICTURE '9999999.999'
    @ ROW , 71 SAY NLAN_CST4              PICTURE '9999999.999'
    @ ROW , 85 SAY laEAct[4]              PICTURE '9999999.999'
    lnActu  = lnactu + laEAct[4]
    ROW = ROW+1
    laSub[5,1] =laSub[5,1]+ (NCOST5*TOTQTY)
    laSub[5,2] =laSub[5,2]+(NLAN_CST5*TOTQTY)
    laEAct[5] = IIF(POSHDR.OPEN+POSHDR.RECEIVE > 0,;
                      ROUND(POSHDR.NACT_COST5/(POSHDR.OPEN+POSHDR.RECEIVE),3),0)
    laSub[5,3] =laSub[5,3]+(laEAct[5]*TOTQTY)
    laSubC[5,1] =laSubC[5,1]+ (NCOST5*TOTQTY)
    laSubS[5,1] =laSubS[5,1]+ (NCOST5*TOTQTY)
    laSubC[5,2] =laSubC[5,2]+(NLAN_CST5*TOTQTY)
    laSubS[5,2] =laSubS[5,2]+(NLAN_CST5*TOTQTY)
    laSubC[5,3] =laSubC[5,3]+(laEAct[5]*TOTQTY)
    laSubS[5,3] =laSubS[5,3]+(laEAct[5]*TOTQTY)
    @ ROW , 03 SAY laCost[5]
    @ ROW , 15 SAY NCOST5*TOTQTY          PICTURE '9999999.999'
    @ ROW , 29 SAY NLAN_CST5*TOTQTY       PICTURE '9999999.999'
    @ ROW , 43 SAY laEAct[5]*TOTQTY       PICTURE '9999999.999'
    @ ROW , 57 SAY NCOST5				  PICTURE '9999999.999'      
    @ ROW , 71 SAY NLAN_CST5              PICTURE '9999999.999'      
    @ ROW , 85 SAY laEAct[5]              PICTURE '9999999.999'
    lnActu  = lnactu + laEAct[5]
    ROW = ROW + 1
    @ ROW ,14 SAY ' -----------   -----------   -----------   -----------   -----------   ----------- ' 
    ROW = ROW + 1
    @ ROW , 03 SAY 'Total Cost'
    @ ROW , 15 SAY (NCOST1/POSHDR.RATE+NCOST2+NCOST3+NCOST4+NCOST5)*TOTQTY            PICTURE '9999999.999'
    @ ROW , 29 SAY (NLAN_CST1+NLAN_CST2+NLAN_CST3+NLAN_CST4+NLAN_CST5)*TOTQTY    PICTURE '9999999.999'
    @ ROW , 43 SAY (laEAct[1]+laEAct[2]+laEAct[3]+laEAct[4]+laEAct[5])*TOTQTY    PICTURE '9999999.999'
    @ ROW , 57 SAY (NCOST1/POSHDR.RATE+NCOST2+NCOST3+NCOST4+NCOST5)                   PICTURE '9999999.999'
    @ ROW , 71 SAY (NLAN_CST1+NLAN_CST2+NLAN_CST3+NLAN_CST4+NLAN_CST5)           PICTURE '9999999.999'
    @ ROW , 85 SAY lnActu                                                             PICTURE '9999999.999'
    ROW = ROW + 1
    @ ROW ,14 SAY ' ===========   ===========   ===========   ===========   ===========   ==========='
    SELECT &WORKFILE
    lnTOTqty = TOTQTY
    SKIP
  ENDDO
  *:C101341,1 SSH END [MAIN REPORT] LOOP
  ROW = ROW + 1
  IF ROW >= MAXROW
    =lfHeader()
  ELSE
    @ ROW,00 SAY REPLICATE('=',132)  
  ENDIF
  ROW = ROW+1
  @ ROW,000 SAY '* GRAND TOTAL *'
  @ ROW,116 SAY lnfTotRec    PICTURE '99999999'
  ROW = ROW+1
  @ ROW , 03 SAY laCost[1]
  @ ROW , 15 SAY laFinal[1,1]                      PICTURE '9999999.999'
  @ ROW , 29 SAY laFinal[1,2]                      PICTURE '9999999.999'
  @ ROW , 43 SAY laFinal[1,3]                      PICTURE '9999999.999'
  @ ROW , 57 SAY laFinal[1,1]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 71 SAY laFinal[1,2]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 85 SAY laFinal[1,3]/lnfTotRec            PICTURE '9999999.999'
  ROW = ROW + 1
  @ ROW , 03 SAY laCost[2]
  @ ROW , 15 SAY laFinal[2,1]                      PICTURE '9999999.999'
  @ ROW , 29 SAY laFinal[2,2]                      PICTURE '9999999.999'
  @ ROW , 43 SAY laFinal[2,3]                      PICTURE '9999999.999'
  @ ROW , 57 SAY laFinal[2,1]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 71 SAY laFinal[2,2]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 85 SAY laFinal[2,3]/lnfTotRec            PICTURE '9999999.999'
  ROW= ROW+1
  @ ROW , 03 SAY laCost[3]
  @ ROW , 15 SAY laFinal[3,1]                      PICTURE '9999999.999'
  @ ROW , 29 SAY laFinal[3,2]                      PICTURE '9999999.999'
  @ ROW , 43 SAY laFinal[3,3]                      PICTURE '9999999.999'
  @ ROW , 57 SAY laFinal[3,1]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 71 SAY laFinal[3,2]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 85 SAY laFinal[3,3]/lnfTotRec            PICTURE '9999999.999'
  ROW= ROW+1
  @ ROW , 03 SAY laCost[4]
  @ ROW , 15 SAY laFinal[4,1]                      PICTURE '9999999.999'
  @ ROW , 29 SAY laFinal[4,2]                      PICTURE '9999999.999'
  @ ROW , 43 SAY laFinal[4,3]                      PICTURE '9999999.999'
  @ ROW , 57 SAY laFinal[4,1]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 71 SAY laFinal[4,2]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 85 SAY laFinal[4,3]/lnfTotRec            PICTURE '9999999.999'
  ROW= ROW+1
  @ ROW , 03 SAY laCost[5]
  @ ROW , 15 SAY laFinal[5,1]                      PICTURE '9999999.999'
  @ ROW , 29 SAY laFinal[5,2]                      PICTURE '9999999.999'
  @ ROW , 43 SAY laFinal[5,3]                      PICTURE '9999999.999'
  @ ROW , 57 SAY laFinal[5,1]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 71 SAY laFinal[5,2]/lnfTotRec            PICTURE '9999999.999'
  @ ROW , 85 SAY laFinal[5,3]/lnfTotRec            PICTURE '9999999.999'
  ROW= ROW+1
  @ ROW ,14 SAY ' -----------   -----------   -----------   -----------   -----------   -----------'
  ROW = ROW + 1
  @ ROW , 03 SAY 'Total Cost'
  lnFin1 = laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]
  lnFin2 = laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]
  lnFin3 = laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]
  @ ROW , 15 SAY laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]    PICTURE '999999.999'
  @ ROW , 29 SAY laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]    PICTURE '999999.999'
  @ ROW , 43 SAY laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]    PICTURE '999999.999'
  @ ROW , 57 SAY lnFin1/lnfTotRec         PICTURE '9999999.999'
  @ ROW , 71 SAY lnFin2/lnfTotRec         PICTURE '9999999.999'
  @ ROW , 85 SAY lnFin3/lnfTotRec         PICTURE '9999999.999'
  ROW = ROW + 1
  @ ROW ,14 SAY ' ===========   ===========   ===========   ===========   ===========   ==========='
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('=',132)
  EXIT
ENDDO
DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
=lfErase()
RETURN
*!*************************************************************
*! Name      : lpNoCosting   CUST#(101341)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : print the report with no style costing details.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpNoCosting

DO WHILE .T.
  SELECT POSLN
  SET FILTER TO
  SET RELATION TO
  SET RELATION TO cstytype+po INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  LOCATE ALL FOR &XFILTER
  IF EOF()
    *:C101341,1 SSH 'Ther are no records to display .'
    = gfModalGen('TRM00052B00000','DIALOG' )
    RETURN
  ENDIF
  lcCurr = IIF(llRPFrnCur,'Y','N')
  WORKFILE = gfTempName()
  SET TALK ON
  COPY REST TO &gcWorkDir.&WORKFILE FOR &XFILTER
  SET TALK OFF
  SELECT POSLN
  SET RELATION TO
  =gfOpenFile('&gcWorkDir.&WORKFILE',' ','EX')
  SET RELATION TO cstytype+po INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  *:C101341,1 SSH Sort to Workfile Index
  IF SORTFIELD<>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW  'SORTING &Z RECORDS FOR STYLE PURCHASE ORDER DETAIL REPORT ...' NOWAIT
    INDEX ON &SORTFIELD TAG &WORKFILE
    SET ORDER TO TAG &WORKFILE
  ENDIF
  *:C101341,1 SSH Variables Initialization
  DIMENSION XTOTAL(1,10),XTOTAL1(1,10)
  XTOTAL = 0.00
  XTOTAL1= 0.00
  XTIME  = TIME()
  PAGENO = 0
  ROW    = 99
  SELECT &WORKFILE
  GOTO TOP
  IF BREAKD = 'SHIPNO'
    DELETE ALL FOR EMPTY(SHIPNO)
    GOTO TOP
    IF EOF()
      *:C101341,1 SSH 'Ther are no records to display .'
      = gfModalGen('TRM00052B00000','DIALOG' )
      RETURN
    ENDIF
  ENDIF
  IF LEN(TRIM(BREAK)) <> 0
    HBREAK = &BREAK
  ENDIF
  *:C101341,1 SSH Begin Printing
  SET DEVICE TO SCREEN
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  SET DEVICE TO PRINT
  SELECT &WORKFILE
  R_Title = R_Title + IIF( llMultiWH .AND. !EMPTY( lcWareCode ),;
                          ' FOR WAREHOUSE: '+ lcWareCode , '')

  *:C101341,1 SSH BEGIN [MAIN REPORT] LOOP
  DO WHILE INKEY() <> 32
    IF ROW >=53
       PAGENO = PAGENO+1
       DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
       IF lcCurr = 'Y'
         @ 06,00 SAY 'PO#    VENDOR   RECEIVED SHPMT# S STYLE        COLOR    QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7  QTY8  TOTQTY  PRICE   AMOUNT'
       ELSE
         @ 06,00 SAY 'PO#    VENDOR   RECEIVED SHPMT# S STYLE        COLOR    QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7  QTY8  TOTQTY $PRICE   AMOUNT'
       ENDIF
       @ 07,00 SAY REPLICATE('=',132)
       ROW = 08
    ENDIF
    IF LEN(TRIM(BREAK)) <>0 AND &BREAK <> HBREAK
      IF (lcCurr = 'N' .OR. (lcCurr = 'Y' .AND. BREAKD= 'SUBSTR(STYLE,1,lnMajLen)') )
        ROW=ROW+1
        @ ROW,00 SAY REPLICATE('-',132)
        ROW=ROW+1
        @ ROW,000 SAY '**SUB TOTAL**'
        @ ROW,015 SAY HBREAK
        @ ROW,054 SAY XTOTAL(1,1)  PICTURE '999999'
        @ ROW,061 SAY XTOTAL(1,2)  PICTURE '999999'
        @ ROW,068 SAY XTOTAL(1,3)  PICTURE '999999'
        @ ROW,075 SAY XTOTAL(1,4)  PICTURE '999999'
        @ ROW,082 SAY XTOTAL(1,5)  PICTURE '999999'
        @ ROW,089 SAY XTOTAL(1,6)  PICTURE '999999'
        @ ROW,096 SAY XTOTAL(1,7)  PICTURE '999999'
        @ ROW,103 SAY XTOTAL(1,8)  PICTURE '99999'
        @ ROW,109 SAY XTOTAL(1,9)  PICTURE '9999999'
        @ ROW,121 SAY XTOTAL(1,10) PICTURE '999999999.99'
        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('-',132)
        ROW = ROW+1
      ENDIF
      X = 1
      DO WHILE (X <= 10)
        XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
        XTOTAL(1,X)  = 0.00
        X = X + 1
      ENDDO
      HBREAK = &BREAK
    ENDIF
    *:C101341,1 SSH End Subtotals Loop
    IF EOF()
      EXIT
    ENDIF
    IF ROW >=53
      ROW = 99
      LOOP
    ENDIF
    SELECT &WORKFILE
    ROW=ROW+1
    @ ROW,000 SAY PO
    @ ROW,007 SAY POSHDR->VENDOR
    @ ROW,016 SAY DATE
    @ ROW,025 SAY SHIPNO
    @ ROW,032 SAY POSHDR->STATUS
    @ ROW,034 SAY SUBSTR(STYLE,1,lnMajLen)
    @ ROW,047 SAY SUBSTR(Style,lnClrPo,lnColorLen)
    @ ROW,054 SAY QTY1 PICTURE '999999'
    @ ROW,061 SAY QTY2 PICTURE '999999'
    @ ROW,068 SAY QTY3 PICTURE '999999'
    @ ROW,075 SAY QTY4 PICTURE '999999'
    @ ROW,082 SAY QTY5 PICTURE '999999'
    @ ROW,089 SAY QTY6 PICTURE '999999'
    @ ROW,096 SAY QTY7 PICTURE '999999'
    @ ROW,103 SAY QTY8 PICTURE '99999'
    @ ROW,109 SAY TOTQTY PICTURE '9999999'
    @ ROW,117 SAY IIF(lcCurr = 'Y',nCost1,nCost1/POSHDR->RATE) PICTURE '999.99'
    IF lcCurr = 'N'
      @ ROW,123 SAY (TOTQTY*(nCost1/POSHDR->RATE)) PICTURE '999999.99'   &&TAK 11/06/94
    ELSE
      @ ROW,123 SAY (TOTQTY*nCost1) PICTURE '999999.99'   && TMI 01/15/95
    ENDIF
    XTOTAL(1,1)=XTOTAL(1,1)+QTY1
    XTOTAL(1,2)=XTOTAL(1,2)+QTY2
    XTOTAL(1,3)=XTOTAL(1,3)+QTY3
    XTOTAL(1,4)=XTOTAL(1,4)+QTY4
    XTOTAL(1,5)=XTOTAL(1,5)+QTY5
    XTOTAL(1,6)=XTOTAL(1,6)+QTY6
    XTOTAL(1,7)=XTOTAL(1,7)+QTY7
    XTOTAL(1,8)=XTOTAL(1,8)+QTY8
    XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY
    IF lcCurr = 'N'
      XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*(nCost1/POSHDR->RATE))
    ELSE
      XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*nCost1)
    ENDIF
    SELECT &WORKFILE
    SKIP
  ENDDO
  *:C101341,1 SSH END [MAIN REPORT] LOOP
  X = 2
  IF LEN(TRIM(BREAK)) = 0
    X = 1
  ENDIF
  ROW=ROW+2
  @ ROW,00 SAY REPLICATE('=',132)
  ROW = ROW+1
  @ ROW,000 SAY '* GRAND TOTAL *'
  @ ROW,054 SAY XTOTAL1(1,1)  PICTURE '999999'
  @ ROW,061 SAY XTOTAL1(1,2)  PICTURE '999999'
  @ ROW,068 SAY XTOTAL1(1,3)  PICTURE '999999'
  @ ROW,075 SAY XTOTAL1(1,4)  PICTURE '999999'
  @ ROW,082 SAY XTOTAL1(1,5)  PICTURE '999999'
  @ ROW,089 SAY XTOTAL1(1,6)  PICTURE '999999'
  @ ROW,096 SAY XTOTAL1(1,7)  PICTURE '999999'
  @ ROW,103 SAY XTOTAL1(1,8)  PICTURE '99999'
  @ ROW,109 SAY XTOTAL1(1,9)  PICTURE '9999999'
  @ ROW,121 SAY XTOTAL1(1,10) PICTURE '999999999.99'
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('=',132)
  EXIT
ENDDO
DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
=lfErase()
RETURN

*!*************************************************************
*! Name      : lfHeader           CUST#(101341)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 30/11/1998
*! Purpose   : Print the report header
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfHeader

PAGENO = PAGENO+1
DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
@ 06,00 SAY 'P.O. # Status   Style                Color       Vendor                         Division     Work In Process Account'
@ 07,00 SAY 'Ship.# WHouse   Description          Description Vendor Name                    Description  Received On Total Received Qty'
@ 08,00 SAY REPLICATE('=',132)
ROW = 09

*!*************************************************************
*! Name      : lfStyTot           CUST#(101341)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 30/11/1998
*! Purpose   : Print total per style
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfStyTot

*:C101341,1 SSH If the sorting is by style the DO WHILE loop in the main 
*:C101341,1 SSH program will print the style total, so return without printing
IF BREAKD = 'SUBSTR(STYLE,1,lnMajLen)'
  STORE 0 TO laSubS,laSubC
  STORE 0 TO lnTotSRec,lnTotCRec
  RETURN
ENDIF

*:C101341,1 SSH Those variables are not the same as the ones printed above.
PRIVATE lnX,lnY,lnZ
ROW = ROW + 1
IF ROW > MAXROW
  =lfHeader()
ENDIF
@ ROW,000 SAY '**SUB TOTAL FOR STYLE '+ALLTRIM(lcStyle)
@ ROW,116 SAY lnTotSRec     PICTURE '99999999'
FOR lnCount = 1 TO 5
  ROW=ROW+1
  @ ROW ,03 SAY laCost[lnCount ]
  @ ROW ,15 SAY laSubS[lnCount ,1]            PICTURE '9999999.999'
  @ ROW ,29 SAY laSubS[lnCount ,2]            PICTURE '9999999.999'
  @ ROW ,43 SAY laSubS[lnCount ,3]            PICTURE '9999999.999'
  @ ROW ,57 SAY laSubS[lnCount ,1]/lnTotSRec  PICTURE '9999999.999'
  @ ROW ,71 SAY laSubS[lnCount ,2]/lnTotSRec  PICTURE '9999999.999'
  @ ROW ,85 SAY laSubS[lnCount ,3]/lnTotSRec  PICTURE '9999999.999'
ENDFOR

ROW = ROW + 1
@ ROW ,14 SAY ' -----------   -----------   -----------   -----------   -----------   -----------'
ROW = ROW + 1
@ ROW , 03 SAY 'Total Cost'
lnX = laSubS[1,1]+laSubS[2,1]+laSubS[3,1]+laSubS[4,1]+laSubS[5,1]
@ ROW , 15 SAY lnX     PICTURE '9999999.999'
lnY = laSubS[1,2]+laSubS[2,2]+laSubS[3,2]+laSubS[4,2]+laSubS[5,2]
@ ROW , 29 SAY lnY     PICTURE '9999999.999'
lnZ = laSubS[1,3]+laSubS[2,3]+laSubS[3,3]+laSubS[4,3]+laSubS[5,3]
@ ROW , 43 SAY lnZ     PICTURE '9999999.999'
@ ROW ,57 SAY lnX/lnTotSRec  PICTURE '9999999.999'
@ ROW ,71 SAY lnY/lnTotSRec  PICTURE '9999999.999'
@ ROW ,85 SAY lnZ/lnTotSRec  PICTURE '9999999.999'
ROW = ROW+1
@ ROW ,14 SAY ' ===========   ===========   ===========   ===========   ===========   ==========='
STORE 0 TO lnTotSRec,lnTotCRec
STORE 0 TO laSubS,laSubC

*!*************************************************************
*! Name      : lfColTot           CUST#(101341)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 30/11/1998
*! Purpose   : Print total per color
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfColTot

PRIVATE lnX,lnY,lnZ
*:C101341,1 SSH If the sorting is by color the DO WHILE loop in the main 
*:C101341,1 SSH program will print total by color, so return without printing
IF BREAKD = 'Color'
  STORE 0 TO laSubC
  lnTotCRec = 0
  RETURN
ENDIF

ROW = ROW + 1
IF ROW >= MAXROW
  =lfHeader()
ENDIF
@ ROW,000 SAY '**SUB TOTAL FOR STYLE/COLOR '+ALLTRIM(lcStyle)+'/'+ALLTRIM(lcColor)
@ ROW,116 SAY lnTotCRec     PICTURE '99999999'
FOR lnCount=1 TO 5
  ROW=ROW+1
  @ ROW ,03 SAY laCost[lnCount]
  @ ROW ,15 SAY laSubC[lnCount,1]            PICTURE '9999999.999'
  @ ROW ,29 SAY laSubC[lnCount,2]            PICTURE '9999999.999'
  @ ROW ,43 SAY laSubC[lnCount,3]            PICTURE '9999999.999'
  @ ROW ,57 SAY laSubC[lnCount,1]/lnTotCRec  PICTURE '9999999.999'
  @ ROW ,71 SAY laSubC[lnCount,2]/lnTotCRec  PICTURE '9999999.999'
  @ ROW ,85 SAY laSubC[lnCount,3]/lnTotCRec  PICTURE '9999999.999'
ENDFOR
ROW = ROW + 1
@ ROW ,14 SAY ' -----------   -----------   -----------   -----------   -----------   -----------'
ROW = ROW + 1
@ ROW , 03 SAY 'Total Cost'
lnX = laSubC[1,1]+laSubC[2,1]+laSubC[3,1]+laSubC[4,1]+laSubC[5,1]
@ ROW , 15 SAY lnX      PICTURE '9999999.999'
lnY = laSubC[1,2]+laSubC[2,2]+laSubC[3,2]+laSubC[4,2]+laSubC[5,2]
@ ROW , 29 SAY lnY      PICTURE '9999999.999'
lnZ = laSubC[1,3]+laSubC[2,3]+laSubC[3,3]+laSubC[4,3]+laSubC[5,3]
@ ROW , 43 SAY lnZ      PICTURE '9999999.999'
@ ROW ,57 SAY lnX/lnTotCRec  PICTURE '9999999.999'
@ ROW ,71 SAY lnY/lnTotCRec  PICTURE '9999999.999'
@ ROW ,85 SAY lnZ/lnTotCRec  PICTURE '9999999.999'
ROW = ROW+1
@ ROW ,14 SAY ' ===========   ===========   ===========   ===========   ===========   ==========='
STORE 0 TO lnTotCRec
STORE 0 TO laSubC

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
lnDatapos = ASCAN(laOGFxFlt,'POSLN.DATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate)) + '|' + ALLTRIM(DTOC(gdSysDate))     
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF
*:C101341,1 SSH This function to adjust Enabling stauts of currency fields.
= lfvPrnForn()
*:C101341,1 SSH This function to adjust enabling status 'print in foreign curr.' option
= lfvCostDtl()

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)
SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
&lcVenFld = lcVendor 

*!*************************************************************
*! Name      : lfvPO
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : validate purchase order
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************
FUNCTION lfvPO

lcPOFld = VARREAD()
lcPONo = &lcPOFld
IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK('P'+lcPONo , 'POSHDR'))
  DO POSBrow WITH lcPONo,"",'P'
ENDIF
&lcPOFld = lcPONo

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : validate Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfvWareHo
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : validate warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWareHo()
*!*************************************************************
FUNCTION lfvWareHo

lcWareHo = VARREAD()
lcTag = ORDER('WAREHOUS')
SET ORDER TO WAREHOUS IN WAREHOUS
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBrowWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS

*!*************************************************************
*! Name      : lfvCostDtl
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : validate print cost detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCostDtl()
*!*************************************************************
FUNCTION lfvCostDtl

lnFnrCurPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPFrnCur'),1)
laOGObjCnt[lnFnrCurPo] = !llRPCostDt
IF llRPCostDt
  llRPFrnCur = .F.
ENDIF  
= lfOGShowGet('llRPFrnCur')

*!*************************************************************
*! Name      : lfvPrnForn
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : validate print in foreign cuurency option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrnForn()
*!*************************************************************
FUNCTION lfvPrnForn

*:C101341,1 SSH Adjust Enabling stauts of currency fields (Start)
lncDuCurPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CDUTYCUR'),1)
lncPrCurPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CPRICECUR'),1)
llEnabCur = llMultiCur AND llRPFrnCur
IF !llEnabCur
  STORE SPACE(0) TO laOGFxFlt[lncPrCurPO,6],laOGFxFlt[lncDuCurPO,6]
ENDIF
lnVarCnt = ALEN(laOGObjType,1) - (ALEN(laOGFxFlt,1) + ALEN(laOGVrFlt,1))
lncDuCurPO = lnVarCnt + lncDuCurPO
lncPrCurPO = lnVarCnt + lncPrCurPO
STORE llEnabCur TO laOGObjCnt[lncDuCurPO],laOGObjCnt[lncPrCurPO]
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lncPrCurPo-lnVarCnt)) + ',6]')
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lncDuCurPo-lnVarCnt)) + ',6]')
*:C101341,1 SSH Adjust Enabling stauts of currency fields (End)

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : Validate Currency code in SYCCURR file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Currency Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurr()
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp
lnAlias = SELECT()

lcCurFld = VARREAD()
lcCurVal = &lcCurFld
lcOrder = ORDER('SYCCURR')
SET ORDER TO Ccurrcode IN SYCCURR
IF !EMPTY(lcCurVal) AND ('?' $ lcCurVal OR !SEEK(lcCurVal , 'SYCCURR'))
  = gfcurrbrow(@lcCurVal)
ENDIF  
&lcCurFld = lcCurVal
SET ORDER TO lcOrder IN SYCCURR
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*:C101341,1 SSH Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
llStopConc = .F.
*:C101341,1 SSH Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *:C101341,1 SSH If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *:C101341,1 SSH this means that another type is found rather than color or free
      *:C101341,1 SSH and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
RETURN ''

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*!*************************************************************
*! Name      : lfChkMulWH
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : check system using for multi warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkMulWH()
*!*************************************************************

FUNCTION lfChkMulWH

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y')

*!*************************************************************
*! Name      : lfChkMulCu
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : check system using for multi currency
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkMulCu()
*!*************************************************************
FUNCTION lfChkMulCu

RETURN gfGetMemVar('llMulCurr')

*!*************************************************************
*! Name      : lfErase
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 30/11/1998
*! Purpose   : Erase temprary files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfErase()
*!*************************************************************
FUNCTION lfErase

IF USED(Workfile)
  USE IN &Workfile
  ERASE &gcWorkDir.&Workfile+'.DBF'
  ERASE &gcWorkDir.&Workfile+'.CDX'
ENDIF