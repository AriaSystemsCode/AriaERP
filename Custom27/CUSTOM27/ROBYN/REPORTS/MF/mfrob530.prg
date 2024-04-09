************************************************************************
*: Program file  : MFROB530 (C# 101340)
*: Program desc. : CUTTING TICKET RECEIPTS LOG  for (ROB100)
*:                 Convert ROB5300 from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls :PROCEDURES :
*:          lpNoCosting
*:        FUNCTIONS  : 
*:          lfHeader,lfColTot,lfStyTot,lfvStyle,lfOldVal,lfwRepWhen
*:          lfvOStatus,lfvAccount,lfvCutNum,lfvFabric,lfNonMaj.
*:*********************************************************************
*: Passed Parameters  : None
*:*********************************************************************

STORE 0 TO lnClrPo,lnColorLen
=lfNonMaj()
lnMajLen = LEN(gfItemMask('PM'))
lcDiv = ''
lcSea = ''
lcColDesc = ''
XVEN = lcRpVen
= gfOpenFile(gcDataDir+'mfgoprhd',gcDataDir+'mfgoprhd','SH')
*:C101340,1 SSH  Status [begin]
XSTATUS  = lcRpStatus
XTRNCD   = '2'
CHOICE   = lcSortBy
lcCstDtl = lcStyCostD
DIMENSION laCost [5]
STORE SPACE(9) TO laCost [5]
DIMENSION  laEst [5]
DIMENSION  laLan [5]
DIMENSION  laAct [5]
DIMENSION laSEst [5]
DIMENSION laSLan [5]
DIMENSION laSAct [5]
STORE 0 TO laSEst , laSAct , laSlan ,laEst , laAct,;
           laLan ,lnLan ,lnSubEst,lnSubLan,lnSubAct
laCost[1] = (ALLTRIM(gfGetMemVar('M_cMSLbl1',gcAct_Comp)))
laCost[2] = (ALLTRIM(gfGetMemVar('M_cMSLbl2',gcAct_Comp)))
laCost[3] = (ALLTRIM(gfGetMemVar('M_cMSLbl3',gcAct_Comp)))
laCost[4] = (ALLTRIM(gfGetMemVar('M_cMSLbl4',gcAct_Comp)))
laCost[5] = (ALLTRIM(gfGetMemVar('M_cMSLbl5',gcAct_Comp)))

STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
STORE 0 TO lnFinEst  , lnFinLan ,lnFinAct
DIMENSION laSubS[5,3]
DIMENSION laSubC[5,3]
DIMENSION laEAct[5]
STORE 0 TO laSubS,laSubC,laEAct
STORE 0.000 TO lnTotSRec,lnfTotRec, lnTotRec,lnTotCRec,lnActu
MAXROW = 53
XTITLE = lcOpTtl
R_WIDTH  = 'W'                               && Standard Report is Wide
XREPORT  = 'ROB530'
R_TITLE  = 'CUTTING TICKET RECEIPTS LOG'
*:C101340,1 SSH CREATE TEMP FILE TO COLECT DATA
WORKFILE = gfTempName()
SELECT CUTTKTL
= AFIELDS(laStrArr)
*:C101340,1 SSH  Add Fields for Contractors
lnStrArr = ALEN(laStrArr,1)
*:C101340,1 SSH  Contractor 1
DIMENSION laStrArr[lnStrArr+3,4]
laStrArr[lnStrArr+1,1]='CONT1'
laStrArr[lnStrArr+1,2]='C'
laStrArr[lnStrArr+1,3]='8'
laStrArr[lnStrArr+1,4]='0'
*:C101340,1 SSH  Contractor 2
laStrArr[lnStrArr+2,1]='CONT2'
laStrArr[lnStrArr+2,2]='C'
laStrArr[lnStrArr+2,3]='8'
laStrArr[lnStrArr+2,4]='0'
*:C101340,1 SSH  Contractor 3
laStrArr[lnStrArr+3,1]='CONT3'
laStrArr[lnStrArr+3,2]='C'
laStrArr[lnStrArr+3,3]='8'
laStrArr[lnStrArr+3,4]='0'
CREATE TABLE (gcWorkDir+WORKFILE) FROM ARRAY laStrArr
*:C101340,1 SSH END CREATE TEMP FILE

XDYELOT_S = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))='Y'

Xfilter = lcRpExp
*:C101340,1 SSH Setting Filter by Status [A|O|C|H|X]
XSTATUS1 = TRIM(XSTATUS)
IF !EMPTY(XSTATUS1)
  Xfilter = Xfilter+'.AND.CUTTKTH.STATUS $ XSTATUS1'
ENDIF
*:C101340,1 SSH Setting Filter by Transaction Code
Xfilter = Xfilter + '.AND.TRANCD$XTRNCD'

*:C101340,1 SSH Selecting again the Cutting Ticket Header File
SELECT CUTTKTH
GOTO TOP
*:C101340,1 SSH  SELECT SORT SEQUENCE
SORTFIELD = ' '
BREAK     = ' '
BREAKD    = ' '
DO CASE
  CASE CHOICE = 'T'
    SORTFIELD = 'CUTTKT+TRANCD+STYLE'
    BREAK     = 'CUTTKT'
    BREAKD    = 'CUTTKT'
  CASE CHOICE = 'D'
    SORTFIELD = 'DTOS(DATE)+CUTTKT+TRANCD+STYLE'
    BREAK     = 'DATE'
    BREAKD    = 'DATE'
  CASE CHOICE = 'S'
    SORTFIELD = 'SUBSTR(STYLE,1,lnMajLen)+TRANCD+SUBSTR(Style,lnClrPo,lnColorLen)+CUTTKT'
    BREAK     = 'SUBSTR(STYLE,1,lnMajLen)'
    BREAKD    = 'STYLE'
  CASE CHOICE = 'O'
    SORTFIELD = 'STYLE+TRANCD+CUTTKT'
    BREAK     = 'STYLE'
    BREAKD    = 'COLOR'
ENDCASE
 
***********************************************************
*:C101340,1 SSH SELECT REPORT FILE & INITIALIZE MEMORY LOOP
***********************************************************
IF lcCstDtl = 'N'
  DO lpNoCosting
  =lfReset()
  RETURN
ENDIF
DO WHILE .T.
   SELECT CUTTKTL
   SET FILTER TO
   SET RELATION TO
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE
   WAIT WINDOW  'Selecting records for report ...' NOWAIT
   LOCATE ALL FOR &Xfilter
   IF EOF()
     *:C101340,1 SSH Text 'THERE ARE NO RECORDS TO DISPLAY..!'
     = gfModalGen('TRM00052B00000','DIALOG' )
     = lfReset()
     RETURN
   ENDIF
   SET TALK ON
   SCAN REST FOR &Xfilter
     SCATTER MEMVAR
     SELECT mfgoprhd
     =SEEK('M'+m.CUTTKT)
     LOCATE REST WHILE cimtyp+ctktno+coprcode ='M'+CUTTKTL.CutTkt ;
                 FOR  cContCode = ALLTRIM(Xven) AND !lInHouse 
     IF !FOUND() AND !EMPTY(Xven)
       LOOP
     ENDIF
     lnIndex = 0
     = SEEK('M'+m.CUTTKT)
     STORE '' TO m.CONT1,m.CONT2,m.CONT3
     SCAN REST WHILE cimtyp+ctktno+coprcode='M'+CUTTKTL.CutTkt ;
               AND lnIndex < 2 FOR   !lInHouse 
       lnIndex = lnIndex + 1
       STORE ALLTRIM(cContCode) TO ('m.Cont'+STR(lnIndex,1))
     ENDSCAN
     INSERT INTO (WORKFILE) FROM MEMVAR
   ENDSCAN
   SET TALK OFF
   IF EOF(WORKFILE)
     *:C101340,1 SSH Text 'THERE ARE NO RECORDS TO DISPLAY..!'
     = gfModalGen('TRM00052B00000','DIALOG' )
     =lfReset()
     RETURN
   ENDIF
   SELECT CUTTKTL
   SET RELATION TO
   SELECT (WORKFILE)
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE

   *:C101340,1 SSH Sort to Workfile Index
   IF SORTFIELD <> ' '
     Z = LTRIM(STR(RECCOUNT(),7))
     WAIT WINDOW 'SORTING &Z RECORDS FOR CUTTING TICKET RECEIPTS LOG ...' NOWAIT
     INDEX ON &SORTFIELD TAG &WORKFILE
     SET ORDER TO TAG &WORKFILE
   ENDIF

   *:C101340,1 SSH Variables Initialization
   DIMENSION XTOTAL(1,10),XTOTAL1(1,10)
   XTOTAL = 0.00
   XTOTAL1= 0.00
   STORE 0 TO lnLandCst1,lnLandCst2,lnLandCst3,lnLandCst4,lnLandCst5
   XTIME  = TIME()
   PAGENO = 0
   ROW    = 99
   SELECT &WORKFILE
   GOTO TOP
   IF LEN(TRIM(BREAK)) <> 0
      HBREAK = &BREAK
   ENDIF
   *:C101340,1 SSH Begin Printing
   WAIT WINDOW  'Report printing - <SPACE BAR> to abort' NOWAIT
   SET DEVICE TO PRINT
   SELECT &WORKFILE
   lcStyle = ALLTRIM(SUBSTR(STYLE,1,lnMajLen))
   lcColor = ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen))

   *:C101340,1 SSH BEGIN [MAIN REPORT] LOOP
   DO WHILE INKEY() <> 32
     IF ROW >= 53
       =lfHeader()
     ENDIF
     DO CASE
       CASE BREAKD # 'COLOR' AND ALLTRIM(SUBSTR(STYLE,1,lnMajLen)) <> lcStyle
         =lfColTot()
         =lfStyTot()
         lcStyle = ALLTRIM(SUBSTR(STYLE,1,lnMajLen))
         lcColor = ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen))
       CASE BREAKD # 'COLOR' AND ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen)) <> ;
                                 lcStyle+lcColor AND &BREAK = HBREAK
         =lfColTot()
         lcColor = ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen))
       CASE &BREAK # HBREAK AND !INLIST(BREAKD,'STYLE','COLOR')
         =lfColTot()
         =lfStyTot()
         lcStyle = ALLTRIM(SUBSTR(STYLE,1,lnMajLen))
         lcColor = ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen))
     ENDCASE

     *:C101340,1 SSH Begin Subtotals Loop
     DO WHILE LEN(TRIM(BREAK)) <> 0
        IF &BREAK = HBREAK
           EXIT
        ENDIF
        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('-',132)
        ROW = ROW+1
        DO CASE
          CASE BREAKD = 'CUTTKT'
            lnSort    = 'CUTTKT'
          CASE BREAKD = 'STYLE'
            lnSort    = 'ALLTRIM(SUBSTR(STYLE,1,lnMajLen))'
          CASE BREAKD = 'COLOR'
            lnSort    = 'STYLE/COLOR'
          CASE BREAKD = 'DATE'
            lnSort    = 'DATE'
        ENDCASE
        IF BREAKD = 'DATE'
          @ ROW,00 SAY REPLICATE('*',(132-LEN(' TOTAL FOR '+lnSort+SPACE(1))-9)/2);
                        + ' TOTAL FOR '+lnSort+SPACE(1)
          @ ROW,PCOL() SAY HBREAK
          @ ROW,PCOL() SAY SPACE(1)+REPLICATE('*',131-PCOL())
        ELSE
          @ ROW,00 SAY REPLICATE('*',(132-LEN(' TOTAL FOR '+lnSort+SPACE(1)+HBREAK+SPACE(1)))/2);
                        + ' TOTAL FOR '+lnSort+SPACE(1)+HBREAK+SPACE(1)
          @ ROW,PCOL() SAY REPLICATE('*',132-PCOL())
        ENDIF
        ROW=ROW+1
        @ ROW,000 SAY '**SUB TOTAL**'
        @ ROW,103 SAY lnTotRec     PICTURE '99999999'
        ROW=ROW+1
        @ ROW,015 SAY '  Estimated        Landed        Actual      Est/unit      Lan/unit      Act/unit '
        ROW=ROW+1
        lnSubEst=lnSubEst+laSEST[1]
        lnSubLan=lnSubLan+laSLAN[1]
        lnSubAct=lnSubAct+laSAct[1]
        @ ROW , 03 SAY laCost[1]
        @ ROW , 15 SAY laSEST[1]	          PICTURE '999999.999'
        @ ROW , 29 SAY laSLAN[1]              PICTURE '999999.999'
        @ ROW , 43 SAY laSAct[1]              PICTURE '999999.999'
        @ ROW , 57 SAY laSEst[1]/lnTotRec     PICTURE '9999999.999'
        @ ROW , 71 SAY laSLan[1]/lnTotRec     PICTURE '9999999.999'
        @ ROW , 85 SAY laSAct[1]/lnTotRec     PICTURE '9999999.999'
        ROW=ROW+1
        @ROW , 03 SAY laCost[2]
        lnSubEst = lnSubEst+laSEST[2]
        lnSubLan = lnSubLan+laSLAN[2]
        lnSubAct = lnSubAct+laSACT[2]
        @ ROW , 15 SAY laSEST[2]               PICTURE '999999.999'
        @ ROW , 29 SAY laSLAN[2]               PICTURE '999999.999'
        @ ROW , 43 SAY laSAct[2]               PICTURE '999999.999'        
        @ ROW , 57 SAY laSEst[2]/lnTotRec      PICTURE '9999999.999'
        @ ROW , 71 SAY laSLan[2]/lnTotRec      PICTURE '9999999.999'
        @ ROW , 85 SAY laSAct[2]/lnTotRec      PICTURE '9999999.999'
        ROW=ROW+1
        @ROW ,003 SAY laCost[3]
        lnSubEst=lnSubEst+laSEST[3]
        lnSubLan=lnSubLan+laSLAN[3]
        lnSubAct=lnSubAct+laSACT[3]
        @ ROW , 15 SAY laSEST[3]               PICTURE '999999.999'
        @ ROW , 29 SAY laSLAN[3]               PICTURE '999999.999'
        @ ROW , 43 SAY laSAct[3]               PICTURE '999999.999'
        @ ROW , 57 SAY laSEst[3]/lnTotRec      PICTURE '9999999.999'
        @ ROW , 71 SAY laSLan[3]/lnTotRec      PICTURE '9999999.999'
        @ ROW , 85 SAY laSAct[3]/lnTotRec      PICTURE '9999999.999'
        ROW=ROW+1
        @ROW ,003 SAY laCost[4]
        lnSubEst = lnSubEst+laSEST[4]
        lnSubLan = lnSubLan+laSLAN[4]
        lnSubAct = lnSubAct+laSACT[4]
        @ ROW , 15 SAY laSEST[4]               PICTURE '999999.999'
        @ ROW , 29 SAY laSLAN[4]               PICTURE '999999.999'
        @ ROW , 43 SAY laSAct[4]               PICTURE '999999.999'
        @ ROW , 57 SAY laSEst[4]/lnTotRec      PICTURE '9999999.999'
        @ ROW , 71 SAY laSLan[4]/lnTotRec      PICTURE '9999999.999'
        @ ROW , 85 SAY laSAct[4]/lnTotRec      PICTURE '9999999.999'
        ROW=ROW+1
        @ROW ,003 SAY laCost[5]
        lnSubEst = lnSubEst+laSEST[5]
        lnSubLan = lnSubLan+laSLAN[5]
        lnSubAct = lnSubAct+laSact[5]
        @ ROW , 15 SAY laSEST[5]              PICTURE '999999.999'
        @ ROW , 29 SAY laSLAN[5]              PICTURE '999999.999'
        @ ROW , 43 SAY laSAct[5]              PICTURE '999999.999'
        @ ROW , 57 SAY laSEst[5]/lnTotRec     PICTURE '9999999.999'
        @ ROW , 71 SAY laSLan[5]/lnTotRec     PICTURE '9999999.999'
        @ ROW , 85 SAY laSAct[5]/lnTotRec     PICTURE '9999999.999'
        ROW = ROW + 1
        @ ROW ,14 SAY '  ----------    ----------    ----------    ----------    ----------    ----------'
        ROW = ROW + 1
        @ ROW , 03 SAY 'Total Cost'
        lnEst = lnEst + laSEST[1]
        lnLan = lnLan + laSlan[1]
        lnAcT = lnAcT + laSact[1]
        @ ROW , 15 SAY lnSubEst                    PICTURE '999999.999'
        @ ROW , 29 SAY lnSubLan                    PICTURE '999999.999'
        @ ROW , 43 SAY lnSubAct                    PICTURE '999999.999'
        @ ROW , 57 SAY lnSubEst/lnTotRec           PICTURE '9999999.999'
        @ ROW , 71 SAY lnSubLan/lnTotRec           PICTURE '9999999.999'
        @ ROW , 85 SAY lnSubAct/lnTotRec           PICTURE '9999999.999'
        ROW = ROW+1
        @ ROW ,14 SAY '  ==========    ==========    ==========    ==========    ==========    ========== '
        STORE 0 TO laSEst , laSAct , laSlan
        STORE 0 TO lnLandCst1,lnLandCst2,lnLandCst3,lnLandCst4,lnLandCst5
        lnTotRec = 0
        lnTotQty = totqty
        HBREAK = &BREAK
        EXIT
      ENDDO
      *:C101340,1 SSH End Subtotals Loop
      IF BREAKD = 'COLOR' AND ALLTRIM(SUBSTR(STYLE,1,lnMajLen)) <> lcStyle
         =lfStyTot()
         lcStyle = ALLTRIM(SUBSTR(STYLE,1,lnMajLen))
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      IF ROW >=53
         ROW = 99
         LOOP
      ENDIF
      DO CASE
        CASE CUTTKTH.STATUS = 'O'
           lcSTATUS='OPEN'
        CASE CUTTKTH.STATUS = 'C'
           lcSTATUS= 'COMPLETE'
        CASE CUTTKTH.STATUS = 'X'
           lcSTATUS='CANCEL'
        CASE CUTTKTH.STATUS = 'S'
           lcSTATUS= 'CLOSED'
        CASE CUTTKTH.STATUS = 'H'
           lcSTATUS= 'HOLD'
        CASE CUTTKTH.STATUS = 'A'
           lcStatus = "ACTUAL"
      ENDCASE
      SELECT &WORKFILE
      ROW=ROW+1
      @ ROW,000 SAY CUTTKT
      @ ROW,010 SAY lcSTATUS
      @ ROW,019 SAY SUBSTR(STYLE,1,lnMajLen)
      @ ROW,038 SAY ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen))
      @ ROW,051 SAY CWARECODE
      @ ROW,062 SAY SUBSTR(CUTTKTH.SEASON,1,2)
      @ ROW,074 SAY SUBSTR(CUTTKTH.cDIVISION,1,2)
      SELECT GL_LINK
      = SEEK(CUTTKTH.LINK_CODE)
      @ ROW , 86 SAY LINK_CODE + '-'
      @ ROW , 93 SAY IIF(SEEK(CUTTKTH.LINK_CODE+'013'),GLACNT,'')
      SELECT &WORKFILE
      ROW= ROW+1
      @ ROW,019 SAY IIF(SEEK(STYLE,'STYLE'),SUBSTR(STYLE.DESC,1,18),SPACE(18))
      *:C101340,1 SSH Color Desc.
      lcColDesc = gfCodDes(ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen)),'COLOR     ')
      @ ROW,038 SAY SUBSTR(lcColDesc,1,10)
      @ ROW,051 SAY IIF(XDYELOT_S,DYELOT,SUBSTR(REFERENCE,1,10))
      *:C101340,1 SSH Season desc.
      lcSea = gfCodDes(CUTTKTH.SEASON,'SEASON    ')
      @ ROW,062 SAY SUBSTR(lcSea,1,10)
      *:C101340,1 SSH Division Disc.
      lcDiv = gfCodDes(CUTTKTH.CDIVISION,'CDIVISION')
      @ ROW,074 SAY SUBSTR(lcDiv,1,10)
      @ ROW,86 SAY DATE
      @ ROW,103 SAY TOTQTY PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
      @ ROW,015 SAY '  Estimated        Landed        Actual      Est/unit      Lan/unit      Act/unit '
      ROW=ROW+1
      lnLandCst1=ROUND(nLan_Cst1,2)
      lnLandCst2=ROUND(nLan_Cst2,2)
      lnLandCst3=ROUND(nLan_Cst3,2)
      lnLandCst4=ROUND(nLan_Cst4,2)
      lnLandCst5=ROUND(nLan_Cst5,2)                        
      @ ROW , 03 SAY laCost[1]
      laEAct[1] = IIF(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec > 0,;
                      ROUND(CUTTKTH.NACT_COST1/(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec),3),0) 
      @ ROW , 15 SAY NCOST1*TotQty             PICTURE '9999999.999'
      @ ROW , 29 SAY lnLandCst1*TotQty         PICTURE '9999999.999'
      @ ROW , 43 SAY laEAct[1]*TotQty          PICTURE '9999999.999'
      @ ROW , 57 SAY NCOST1                    PICTURE '9999999.999'
      @ ROW , 71 SAY lnLandCst1                PICTURE '9999999.999'
      @ ROW , 85 SAY laEAct[1]                 PICTURE '9999999.999'
      ROW = ROW+1
      @ ROW , 03 SAY laCost[2]
      laEAct[2] = IIF(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec > 0,;
                      ROUND(CUTTKTH.NACT_COST2/(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec),3),0) 
      @ ROW , 15 SAY NCOST2 *TotQty              PICTURE '9999999.999'
      @ ROW , 29 SAY lnLandCst2 *TotQty          PICTURE '9999999.999'
      @ ROW , 43 SAY laEAct[2] *TotQty           PICTURE '9999999.999'
      @ ROW , 57 SAY NCOST2                      PICTURE '9999999.999'
      @ ROW , 71 SAY lnLandCst2                  PICTURE '9999999.999'
      @ ROW , 85 SAY laEAct[2]                   PICTURE '9999999.999'
      ROW = ROW+1
      @ ROW , 03 SAY laCost[3]
      laEAct[3] = IIF(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec > 0,;
                      ROUND(CUTTKTH.NACT_COST3/(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec),3),0) 
      @ ROW , 15 SAY NCOST3 *TotQty          PICTURE '9999999.999'
      @ ROW , 29 SAY lnLandCst3*TotQty       PICTURE '9999999.999'
      @ ROW , 43 SAY laEAct[3] *TotQty       PICTURE '9999999.999'
      @ ROW , 57 SAY NCOST3                  PICTURE '9999999.999'
      @ ROW , 71 SAY lnLandCst3              PICTURE '9999999.999'
      @ ROW , 85 SAY laEAct[3]               PICTURE '9999999.999'
      ROW = ROW+1
      @ ROW , 03 SAY laCost[4]
      laEAct[4] = IIF(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec > 0,;
                      ROUND(CUTTKTH.NACT_COST4/(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec),3),0) 
      @ ROW , 15 SAY NCOST4 *TotQty             PICTURE '9999999.999'
      @ ROW , 29 SAY lnLandCst4 *TotQty         PICTURE '9999999.999'
      @ ROW , 43 SAY laEAct[4] *TotQty          PICTURE '9999999.999'
      @ ROW , 57 SAY NCOST4                     PICTURE '9999999.999'
      @ ROW , 71 SAY lnLandCst4                 PICTURE '9999999.999'
      @ ROW , 85 SAY laEAct[4]                  PICTURE '9999999.999'
      ROW = ROW+1
      @ ROW , 03 SAY laCost[5]
      laEAct[5] = IIF(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec > 0,;
                      ROUND(CUTTKTH.NACT_COST5/(CUTTKTH.Pcs_Opn+CUTTKTH.Pcs_Rec),3),0)
      @ ROW , 15 SAY NCOST5 *TotQty            PICTURE '9999999.999'
      @ ROW , 29 SAY lnLandCst5 *TotQty        PICTURE '9999999.999'
      @ ROW , 43 SAY laEAct[5] *TotQty         PICTURE '9999999.999'
      @ ROW , 57 SAY NCOST5                    PICTURE '9999999.999'
      @ ROW , 71 SAY lnLandCst5                PICTURE '9999999.999'
      @ ROW , 85 SAY laEAct[5]                 PICTURE '9999999.999'
      ROW = ROW + 1
      @ ROW ,14 SAY '  ----------    ----------    ----------    ----------    ----------    ---------- '
      ROW = ROW + 1
      @ ROW , 03 SAY 'Total Cost'
      lnEst = (NCOST1+NCOST2+NCOST3+NCOST4+NCOST5)*TotQty
      lnLand = (lnLandCst1+lnLandCst2+lnLandCst3+lnLandCst4+lnLandCst5)*TotQty
      lnAcT  = (NACT_CST1+NACT_CST2+NACT_CST3+NACT_CST4+NACT_CST5)*TotQty

      lnActu = (laEAct[1]+laEAct[2]+laEAct[3]+laEAct[4]+laEAct[5])      
      @ ROW , 15 SAY (NCOST1+NCOST2+NCOST3+NCOST4+NCOST5)*TotQty                       PICTURE '9999999.999'
      @ ROW , 29 SAY (lnLandCst1+lnLandCst2+lnLandCst3+lnLandCst4+lnLandCst5)*TotQty   PICTURE '9999999.999'
      @ ROW , 43 SAY lnActu*TotQty                  PICTURE '9999999.999'
      @ ROW , 57 SAY lnEst/TotQty                   PICTURE '9999999.999'
      @ ROW , 71 SAY lnLand/TotQty                  PICTURE '9999999.999'
      @ ROW , 85 SAY lnActu                         PICTURE '9999999.999'
      ROW = ROW + 1
      @ ROW ,14 SAY '  ==========    ==========    ==========    ==========    ==========    =========='
      lnTotRec  = lnTotRec  + TotQty
      lnfTotRec = lnfTotRec + TotQty
      lnTotSRec = lnTotSRec + TotQty
      lnTotCRec = lnTotCRec + TotQty
      FOR I = 1 TO 5
        H= STR(I,1)
        laSEST[I] = laSEST[I] + NCOST&H*TOTQTY
        laEST[I]  = laEST[I]  + NCOST&H*TOTQTY
        laSubC[I,1] = laSubC[I,1] + NCOST&H*TOTQTY
        laSubS[I,1] = laSubS[I,1] + NCOST&H*TOTQTY
      ENDFOR
      FOR I = 1 TO 5
        H= STR(I,1)
        laSACT[I] = laSACT[I] + laEAct[I]*TOTQTY
        laACT[I]  = laACT[I]  + laEAct[I]*TOTQTY
        laSubC[I,3] = laSubC[I,3] + laEAct[I]*TOTQTY
        laSubS[I,3] = laSubS[I,3] + laEAct[I]*TOTQTY
      ENDFOR

      FOR I = 1 TO 5
        H= STR(I,1)
        laSLAN[I] = laSLAN[I] + lnLandCst&H*TOTQTY
        laLAN[I]  = laLAN[I]  + lnLandCst&H*TOTQTY
        laSubC[I,2] = laSubC[I,2] + lnLandCst&H*TOTQTY
        laSubS[I,2] = laSubS[I,2] + lnLandCst&H*TOTQTY
      ENDFOR
      STORE 0 TO lnSubEst,lnSubLan,lnSubAct,lnActu
      XTOTAL(1,1)=XTOTAL(1,1)+QTY1
      XTOTAL(1,2)=XTOTAL(1,2)+QTY2
      XTOTAL(1,3)=XTOTAL(1,3)+QTY3
      XTOTAL(1,4)=XTOTAL(1,4)+QTY4
      XTOTAL(1,5)=XTOTAL(1,5)+QTY5
      XTOTAL(1,6)=XTOTAL(1,6)+QTY6
      XTOTAL(1,7)=XTOTAL(1,7)+QTY7
      XTOTAL(1,8)=XTOTAL(1,8)+QTY8
      XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY
      SELECT &WORKFILE
      SKIP
    ENDDO
    *:C101340,1 SSH END MAIN REPORT LOOP
    X = 2
    IF LEN(TRIM(BREAK)) = 0
      X =1
    ENDIF
    ROW=ROW+2
    IF ROW >= MAXROW
      =lfHeader()
    ELSE
      @ ROW,00 SAY REPLICATE('=',132)
    ENDIF
    ROW = ROW+1
    @ ROW,000 SAY '* GRAND TOTAL *'
    @ ROW,103 SAY lnfTotRec       PICTURE '99999999'
    ROW = ROW+1
    @ ROW , 03 SAY laCost[1]
    @ ROW , 15 SAY laEST[1]                         PICTURE '999999.999'
    @ ROW , 29 SAY lalan[1]                         PICTURE '999999.999'
    @ ROW , 43 SAY laAct[1]                         PICTURE '999999.999'
    @ ROW , 57 SAY laEST[1]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 71 SAY lalan[1]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 85 SAY laAct[1]/lnfTotRec               PICTURE '999999.999'
    ROW = ROW + 1
    @ ROW , 03 SAY laCost[2]
    @ ROW , 15 SAY laEST[2]                         PICTURE '999999.999'
    @ ROW , 29 SAY lalan[2]                         PICTURE '999999.999'
    @ ROW , 43 SAY laact[2]                         PICTURE '999999.999'
    @ ROW , 57 SAY laEST[2]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 71 SAY lalan[2]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 85 SAY laAct[2]/lnfTotRec               PICTURE '999999.999'
    ROW= ROW+1
    @ ROW , 03 SAY laCost[3]
    @ ROW , 15 SAY laEST[3]                         PICTURE '999999.999'
    @ ROW , 29 SAY lalan[3]                         PICTURE '999999.999'
    @ ROW , 43 SAY laact[3]                         PICTURE '999999.999'
    @ ROW , 57 SAY laEST[3]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 71 SAY lalan[3]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 85 SAY laAct[3]/lnfTotRec               PICTURE '999999.999'
    ROW= ROW+1
    @ ROW , 03 SAY laCost[4]
    @ ROW , 15 SAY laEST[4]                         PICTURE '999999.999'
    @ ROW , 29 SAY lalan[4]                         PICTURE '999999.999'
    @ ROW , 43 SAY laact[4]                         PICTURE '999999.999'
    @ ROW , 57 SAY laEST[4]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 71 SAY lalan[4]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 85 SAY laAct[4]/lnfTotRec               PICTURE '999999.999'
    ROW= ROW+1
    @ ROW , 03 SAY laCost[5]
    @ ROW , 15 SAY laEST[5]                         PICTURE '999999.999'
    @ ROW , 29 SAY lalan[5]                         PICTURE '999999.999'
    @ ROW , 43 SAY laact[5]                         PICTURE '999999.999'
    @ ROW , 57 SAY laEST[5]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 71 SAY lalan[5]/lnfTotRec               PICTURE '999999.999'
    @ ROW , 85 SAY laAct[5]/lnfTotRec               PICTURE '999999.999'
    ROW= ROW+1
    FOR I = 1 TO 5
      lnFinEst=lnFinEst+ laEST[I]
      lnFinLan=lnFinLan+ laLan[I]
      lnFinAct=lnFinAct+ laAct[I]
    ENDFOR
    @ ROW ,14 SAY '  ----------    ----------    ----------    ----------    ----------    ----------'
    ROW = ROW + 1
    @ ROW , 03 SAY 'Total Cost'
    @ ROW , 15 SAY lnFinEst               PICTURE '999999.999'
    @ ROW , 29 SAY lnFinLan               PICTURE '999999.999'
    @ ROW , 43 SAY lnFinAct               PICTURE '999999.999'
    @ ROW , 57 SAY lnFinEst/lnfTotRec     PICTURE '999999.999'
    @ ROW , 71 SAY lnFinLan/lnfTotRec     PICTURE '999999.999'
    @ ROW , 85 SAY lnFinAct/lnfTotRec     PICTURE '999999.999'
    ROW = ROW + 1
    @ ROW ,14 SAY '  ==========    ==========    ==========    ==========    ==========    =========='
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    EXIT
 ENDDO
*:C101340,1 SSH END THE REPORT OR DISPLAY ON SCREEN
DO ENDREPORT
= lfReset()
RETURN
*:C101340,1 SSH END MFROB530.PRG
*!*************************************************************
*! Name      : lpNoCosting   CUST#(101340)
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

*:C101340,1 SSH  In order to increse the contractor code to 8 cahracters we had to remove
*:C101340,1 SSH  contractor3 from the report.
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*MFROB                              CUTTKT RECEIPTS      - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*CUTTKT S CONTR1   CONTR2   SE DV RECEIVED REFERENCE  STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY
*XXXXXX X XXXXXXXX XXXXXXXX XX XX MM/DD/YY XXXXXXXXXX 123456789012 123456 99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
* *SUB TOTALS*    XXXXXXXXXXXX                                            99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..

DO WHILE .T.
   SELECT CUTTKTL
   SET FILTER TO
   SET RELATION TO
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE
   WAIT WINDOW  'Selecting records for report ...' NOWAIT
   LOCATE ALL FOR &Xfilter
   IF EOF()
     *:C101340,1 SSH Text 'THERE ARE NO RECORDS TO DISPLAY..!'
     = gfModalGen('TRM00052B00000','DIALOG' )
     =lfReset()
     RETURN
   ENDIF
   SET TALK ON
   SCAN REST FOR &Xfilter
     SCATTER MEMVAR
     SELECT mfgoprhd
     = SEEK('M'+m.CUTTKT)
     LOCATE REST WHILE cimtyp+ctktno+coprcode ='M'+CUTTKTL.CutTkt ;
                 FOR  cContCode = ALLTRIM(Xven) AND !lInHouse 
     IF !FOUND() AND !EMPTY(Xven)
       LOOP
     ENDIF
     lnIndex = 0
     = SEEK('M'+m.CUTTKT)
     STORE '' TO m.CONT1,m.CONT2,m.CONT3
     SCAN REST WHILE cimtyp+ctktno+coprcode='M'+CUTTKTL.CutTkt ;
               AND lnIndex < 2 FOR   !lInHouse 
       lnIndex = lnIndex + 1
       STORE ALLTRIM(cContCode) TO ('m.Cont'+STR(lnIndex,1))
     ENDSCAN
     INSERT INTO (WORKFILE) FROM MEMVAR
   ENDSCAN
   SET TALK OFF
   IF EOF(WORKFILE)
     *:C101340,1 SSH Text 'THERE ARE NO RECORDS TO DISPLAY..!'
     = gfModalGen('TRM00052B00000','DIALOG' )
     =lfReset()
     RETURN
   ENDIF
   SELECT CUTTKTL
   SET RELATION TO
   SELECT (WORKFILE)
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE

*:C101340,1 SSH Sort to Workfile Index
   IF SORTFIELD <> ' '
      Z = LTRIM(STR(RECCOUNT(),7))
      WAIT WINDOW   'SORTING &Z RECORDS FOR CUTTING TICKET RECEIPTS LOG ...' NOWAIT
      INDEX ON &SORTFIELD TAG &WORKFILE
      SET ORDER TO TAG &WORKFILE
   ENDIF

*:C101340,1 SSH In order to increse the contractor code to 8 cahracters we had to remove
*:C101340,1 SSH contractor3 from the report.
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*MFROB                              CUTTKT RECEIPTS      - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*CUTTKT S CONTR1   CONTR2   SE DV RECEIVED REFERENCE  STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY
*XXXXXX X XXXXXXXX XXXXXXXX XX XX MM/DD/YY XXXXXXXXXX 123456789012 123456 99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
* *SUB TOTALS*    XXXXXXXXXXXX                                            99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..

*:C101340,1 SSH Variables Initialization
   DIMENSION XTOTAL(1,10),XTOTAL1(1,10)
   XTOTAL = 0.00
   XTOTAL1= 0.00
   XTIME  = TIME()
   PAGENO = 0
   ROW    = 99
   SELECT &WORKFILE
   GOTO TOP
   IF LEN(TRIM(BREAK)) <> 0
      HBREAK = &BREAK
   ENDIF
   *:C101340,1 SSH Begin Printing
   SET DEVICE TO SCREEN
   WAIT WINDOW  'Report printing - <SPACE BAR> to abort' NOWAIT
   SET DEVICE TO PRINT
   SELE &WORKFILE
   *:C101340,1 SSH BEGIN [MAIN REPORT] LOOP
   DO WHILE INKEY() <> 32
      IF ROW >= 53
         PAGENO = PAGENO+1
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         IF XDYELOT_S
           @ 05,00 SAY 'CUTTKT S CONTR1   CONTR2   SE DV RECEIVED DYELOT     STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY'
         ELSE
           @ 05,00 SAY 'CUTTKT S CONTR1   CONTR2   SE DV RECEIVED REFERENCE  STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY'
         ENDIF
         @ 06,00 SAY REPLICATE('=',132)
         ROW = 06
      ENDIF
      *:C101340,1 SSH Begin Subtotals Loop
      DO WHILE LEN(TRIM(BREAK)) <> 0
         IF &BREAK = HBREAK
            EXIT
         ENDIF
         ROW = ROW+1
         @ ROW,00 SAY REPLICATE('-',132)
         ROW = ROW+1
         @ ROW,000 SAY '*SUB TOTALS*'
         @ ROW,015 SAY HBREAK
         @ ROW,073 SAY XTOTAL(1,1)  PICTURE '99999'
         @ ROW,079 SAY XTOTAL(1,2)  PICTURE '99999'
         @ ROW,085 SAY XTOTAL(1,3)  PICTURE '99999'
         @ ROW,091 SAY XTOTAL(1,4)  PICTURE '99999'
         @ ROW,096 SAY XTOTAL(1,5)  PICTURE '99999'
         @ ROW,103 SAY XTOTAL(1,6)  PICTURE '99999'
         @ ROW,109 SAY XTOTAL(1,7)  PICTURE '99999'
         @ ROW,115 SAY XTOTAL(1,8)  PICTURE '99999'
         @ ROW,122 SAY XTOTAL(1,9)  PICTURE '999999'
         ROW = ROW+1
         @ ROW,000 SAY REPLICATE('-',132)
         ROW = ROW+1
         X = 1
         DO WHILE X<=10
            XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
            XTOTAL(1,X) = 0.00
            X = X+1
         ENDDO
         HBREAK = &BREAK
         EXIT
      ENDDO
      *:C101340,1 SSH End Subtotals Loop
      IF EOF()
         EXIT
      ENDIF
      IF ROW >=53
         ROW = 99
         LOOP
      ENDIF
      SELECT &WORKFILE
      ROW=ROW+1
      @ ROW,000 SAY CUTTKT
      @ ROW,007 SAY CUTTKTH->STATUS
      @ ROW,009 SAY Cont1
      @ ROW,018 SAY Cont2
      @ ROW,027 SAY SUBSTR(CUTTKTH->SEASON,1,2)
      @ ROW,030 SAY SUBSTR(CUTTKTH->cDIVISION,1,2)
      @ ROW,033 SAY DATE
      IF XDYELOT_S
        @ ROW,042 SAY DYELOT
      ELSE
        @ ROW,042 SAY SUBSTR(REFERENCE,1,10)
      ENDIF
      @ ROW,053 SAY SUBSTR(STYLE,1,lnMajLen)
      @ ROW,066 SAY ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen))
      @ ROW,073 SAY QTY1 PICTURE '99999'
      @ ROW,079 SAY QTY2 PICTURE '99999'
      @ ROW,085 SAY QTY3 PICTURE '99999'
      @ ROW,091 SAY QTY4 PICTURE '99999'
      @ ROW,096 SAY QTY5 PICTURE '99999'
      @ ROW,103 SAY QTY6 PICTURE '99999'
      @ ROW,109 SAY QTY7 PICTURE '99999'
      @ ROW,115 SAY QTY8 PICTURE '99999'
      @ ROW,122 SAY TOTQTY PICTURE '999999'
      XTOTAL(1,1)=XTOTAL(1,1)+QTY1
      XTOTAL(1,2)=XTOTAL(1,2)+QTY2
      XTOTAL(1,3)=XTOTAL(1,3)+QTY3
      XTOTAL(1,4)=XTOTAL(1,4)+QTY4
      XTOTAL(1,5)=XTOTAL(1,5)+QTY5
      XTOTAL(1,6)=XTOTAL(1,6)+QTY6
      XTOTAL(1,7)=XTOTAL(1,7)+QTY7
      XTOTAL(1,8)=XTOTAL(1,8)+QTY8
      XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY
      SELECT &WORKFILE
      SKIP
   ENDDO
   *:C101340,1 SSH END MAIN REPORT LOOP
   X = 2
   IF LEN(TRIM(BREAK)) = 0
      X =1
   ENDIF
   ROW=ROW+2
   @ ROW,00 SAY REPLICATE('=',132)
   ROW = ROW+1
   @ ROW,000 SAY '* GRAND TOTAL *'
   @ ROW,073 SAY XTOTAL1(1,1)  PICTURE '99999'
   @ ROW,079 SAY XTOTAL1(1,2)  PICTURE '99999'
   @ ROW,085 SAY XTOTAL1(1,3)  PICTURE '99999'
   @ ROW,091 SAY XTOTAL1(1,4)  PICTURE '99999'
   @ ROW,096 SAY XTOTAL1(1,5)  PICTURE '99999'
   @ ROW,103 SAY XTOTAL1(1,6)  PICTURE '99999'
   @ ROW,109 SAY XTOTAL1(1,7)  PICTURE '99999'
   @ ROW,115 SAY XTOTAL1(1,8)  PICTURE '99999'
   @ ROW,122 SAY XTOTAL1(1,9)  PICTURE '999999'
   ROW = ROW+1
   @ ROW,00 SAY REPLICATE('=',132)
   EXIT
ENDDO
*:C101340,1 SSH END THE REPORT OR DISPLAY ON SCREEN
DO ENDREPORT
=lfReset()
RETURN
*!*************************************************************
*! Name      : lfHeader           CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
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
@ 05,00 SAY 'C.T.     Status    Style             Color        WHouse     Season      Divison      Work In Process Account'
@ 06,00 SAY '                   Description       Description  Ref/Dyelot Description Description  Received On Received Qty'
@ 07,00 SAY REPLICATE('=',132)
ROW = 08
*!*************************************************************
*! Name      : lfColTot           CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Print totals per color.
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

IF BREAKD = 'COLOR'
  STORE 0 TO laSubC
  lnTotCRec = 0
  RETURN
ENDIF

IF ROW >= MAXROW
  =lfHeader()
ENDIF

ROW=ROW+1
@ ROW,000 SAY '**SUB TOTAL FOR STYLE/COLOR '+ALLTRIM(lcStyle)+'/'+ALLTRIM(lcColor)
@ ROW,103 SAY lnTotCRec  PICTURE '99999999'
FOR lnCount = 1 TO 5
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
@ ROW ,14 SAY '  ----------    ----------    ----------    ----------    ----------    ----------'
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
@ ROW ,14 SAY '  ==========    ==========    ==========    ==========    ==========    =========='
STORE 0 TO lnTotCRec
STORE 0 TO laSubC
*!*************************************************************
*! Name      : lfStyTot           CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Print totals per style
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

IF BREAKD = 'STYLE' OR BREAKD = 'CUTTKT'
  STORE 0 TO laSubS,laSubC
  STORE 0 TO lnTotSRec,lnTotCRec
  RETURN
ENDIF

PRIVATE lnX,lnY,lnZ
IF ROW >= MAXROW
  =lfHeader()
ENDIF

ROW=ROW+1
@ ROW,000 SAY '**SUB TOTAL FOR STYLE '+ALLTRIM(lcStyle)
@ ROW,103 SAY lnTotSRec    PICTURE '99999999'
FOR lnCount = 1 TO 5
  ROW=ROW+1
  @ ROW ,03 SAY laCost[lnCount]
  @ ROW ,15 SAY laSubS[lnCount,1]            PICTURE '9999999.999'
  @ ROW ,29 SAY laSubS[lnCount,2]            PICTURE '9999999.999'
  @ ROW ,43 SAY laSubS[lnCount,3]            PICTURE '9999999.999'
  @ ROW ,57 SAY laSubS[lnCount,1]/lnTotSRec  PICTURE '9999999.999'
  @ ROW ,71 SAY laSubS[lnCount,2]/lnTotSRec  PICTURE '9999999.999'
  @ ROW ,85 SAY laSubS[lnCount,3]/lnTotSRec  PICTURE '9999999.999'
ENDFOR
  
ROW = ROW + 1
@ ROW ,14 SAY '  ----------    ----------    ----------    ----------    ----------    ----------'
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
@ ROW ,14 SAY '  ==========    ==========    ==========    ==========    ==========    =========='
STORE 0 TO lnTotSRec,lnTotCRec
STORE 0 TO laSubS,laSubC
*!*************************************************************
*! Name      : lfvStyle           CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Validation function for Validating Style Code
*!*************************************************************
*! Called from : Style Field [Option Grid]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
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
*! Name      : lfOldVal           CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOldVal

laOldVal = EVALUATE(SYS(18))
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
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
FUNCTION lfwRepWhen
DECLARE laRpSource[6],laRpTarget[1]

lnDatapos = ASCAN(laOGFxFlt,'CUTTKTL.DATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate)) + '|' + ALLTRIM(DTOC(gdSysDate))     
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF
lnStatus = lcRpStatus
STORE ' ' TO  laRpTarget
STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Actual'   TO laRpSource[3]
STORE 'Closed'   TO laRpSource[4]
STORE 'Complete' TO laRpSource[5]
STORE 'Canceled' TO laRpSource[6]
lcRpStatus = ' '

*!*************************************************************
*! Name      : lfvAccount
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Validation function for the Vendor
*!*************************************************************
*! Called from : Vendor field [Option Grid]
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet

*:C101340,1 SSH Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*:C101340,1 SSH Varible to hold  the value of the current GET field
lcObjVal = alltrim(EVALUATE(SYS(18)))

*:C101340,1 SSH IF The user wants to Browse or the Vendor is not in the file
SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'APVENDOR'))
  IF !EMPTY(lcObjVal) .AND. (!('?' $ lcObjVal) .AND. !SEEK(lcObjVal , 'APVENDOR'))
    *:C101340,1 SSH Text 'This vendor dos not exist in data file'
    =gfModalGen('TRM38152B00000','DIALOG' )
  ELSE
    llObjRet = gfApVnBrow(@lcObjVal)
    lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
    &lcObjName = lcObjVal
  ENDIF
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Validation function for the Status
*!*************************************************************
*! Called from : Vendor field [Option Grid]
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*:C101340,1 SSH Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Actual','A',;
                            IIF(laRpTarget[lnI] = 'Closed','S',;
                            IIF(laRpTarget[lnI] = 'Complete','C',;
                            IIF(laRpTarget[lnI] = 'Canceled','X',;
                            ''))))))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*:C101340,1 SSH  Compare current selected status with old value  [begin]
*:C101340,1 SSH  to rise change status flag.

*:C101340,1 SSH  if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
*:C101340,1 SSH loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*:C101340,1 SSH Compare current selected status with old value  [end]
*:C101340,1 SSH end of lfvOStatus.

*!*************************************************************
*! Name      : lfvCutNum
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Validation function for Validating Transaction #
*!*************************************************************
*! Called from : Transaction # [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCutNum
PRIVATE lcVar , lcObj , laTemp,lcBrowFields

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
lcPrevAl = SELECT()
STORE '' TO lcBrowCond

DECLARE laTemp[1]
laTemp[1] = lcObj
SELECT CUTTKTH
SET ORDER TO CUTTKTH
DIMENSION laTemp[1]
IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'CUTTKTH'))
  laTemp[1] = ''
  lcBrFields ="CUTTKT    :R :H='CUTTKT'   ,"+;
              "STYLE     :R :H='Style'    ,"+;
              "STATUS    :R :H='Status'   ,"+;
              "ENTERED   :R :H='Issue'    ,"+;
              "COMPLETE  :R :H='Complete' ,"+;
              "SEASON    :R :H='Season'   ,"+;
              "CDIVISION :R :H='Division' ,"+;
              "PCS_BUD   :R :H='Budget'   ,"+;
              "PCS_REC   :R :H='Received' ,"+;
              "PCS_DAM   :R :H='Damaged'  ,"+;
              "PCS_OPN   :R :H='Open' "
    lNThing    = gfBrows(lcBrowCond,'CUTTKT','laTemp')
ENDIF
*:C101340,1 SSH IF The user selected a record
IF !EMPTY(laTemp[1])
  lcObj = laTemp[1]
ELSE
  lcObj = laOldVal
ENDIF
&lcVar = lcObj      && Update the field
SELECT (lcPrevAl)
*!*************************************************************
*! Name      : lfvFabric CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.
IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF  


*!*************************************************************
*! Name      : lfReset CUST#(101340)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 23/11/1998
*! Purpose   : clear relation
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfReset

SELECT CUTTKTL
SET FILTER TO
SET RELATION TO
IF USED('mfgoprhd')
  USE IN mfgoprhd
ENDIF
IF USED(Workfile)
  USE IN &Workfile
  ERASE &gcWorkDir.&Workfile+'.DBF'
  ERASE &gcWorkDir.&Workfile+'.CDX'
ENDIF

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

*:C101340,1 SSH Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
llStopConc = .F.
*:C101340,1 SSH Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *:C101340,1 SSH If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *:C101340,1 SSH this means that another type is found rather than color or free
      *:C101340,1 SSH and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
RETURN ''