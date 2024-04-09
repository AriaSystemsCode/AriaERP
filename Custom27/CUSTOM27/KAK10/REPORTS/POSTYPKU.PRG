*:***************************************************************************
*: Program file  : POSTYPMU.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR Kaktus.
*: Date          : 12/04/2001
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : Mohamed Shokry (MHM)
*: Tracking Job Number: C102252
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYPKU
*:***************************************************************************
*: Modifications :
*:***************************************************************************

llRpLPrnt = llRpLPrnt AND gfUserPriv('IC','ICSTYLE','COSTING')

*-- Variables to hold the non major segments.
STORE 0 TO lnClrPo,lnColorLen,PAGENO,Row
STORE '' TO lcColor,XCLR_DESC,lcCLrDesc
lcTmpPoH = gfTempName()
*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))
*-- To get the non major segments.
=lfNonMaj()

*-- To get the color long name
DIMENSION laClrLName[1,2]
laClrLName[1,1] = 'CLRLNAME'      && Array to get the Color long name
laClrLName[1,2] = 'lcCLrDesc'

*-- To get the lables of the PO items.
DIMENSION laLables[10,2]
laLables[1,1]  = 'M_CISLBL1'
laLables[2,1]  = 'M_CISLBL2'
laLables[3,1]  = 'M_CISLBL3'
laLables[4,1]  = 'M_CISLBL4'
laLables[5,1]  = 'M_CISLBL5'
laLables[6,1]  = 'M_CITYPE1'
laLables[7,1]  = 'M_CITYPE2'
laLables[8,1]  = 'M_CITYPE3'
laLables[9,1]  = 'M_CITYPE4'
laLables[10,1] = 'M_CITYPE5'

=gfGetMemVar(@laLables,gcAct_Comp)
FOR lnArryCnt = 1 TO ALEN(laLables,1)
  lcArryCnt = ALLTRIM(STR(lnArryCnt))
  IF lnArryCnt <= 5
    lcISLbl&lcArryCnt = laLables[lnArryCnt,2]
  ENDIF  
  IF lnArryCnt >= 6
    lcIType&lcArryCnt = laLables[lnArryCnt,2]
  ENDIF  
ENDFOR

*-- To get the company address.
IF EMPTY(laCompAdd[5])
 HLINE1 = gcCom_Name
 HLINE2 = laCompAdd[1]
 HLINE3 = laCompAdd[2]
 HLINE4 = laCompAdd[3]
 HLINE5 = laCompAdd[4]
ELSE
 HLINE1 = gcCom_Name
 HLINE2 = laCompAdd[1]
 HLINE3 = laCompAdd[2]
 HLINE4 = laCompAdd[3]
 HLINE5 = laCompAdd[5]
ENDIF

A = REPLICATE('*',80)
*  .....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   DESCRIPTION                   TOT QTY  CURR      PRICE         AMOUNT'
*    XXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXX     9999999   XXX  999999.99   999999999.99
C = 'STYLE             DESCRIPTION               '
D = 'COLOR  COLOR DESCRIPTION                TOT QTY  CURR      PRICE         AMOUNT'


lcLine1 = HLine1
COL2 = (ROUND((80-LEN(ALLTRIM(HLINE2)))/2,0))
COL3 = (ROUND((80-LEN(ALLTRIM(HLINE3)))/2,0))
COL4 = (ROUND((80-LEN(ALLTRIM(HLINE4)))/2,0))
COL5 = (ROUND((80-LEN(ALLTRIM(HLINE5)))/2,0))

SELECT PosHdr
COPY STRUCTURE TO (gcWorkDir+lcTmpPoH)
=gfOpenFile (gcWorkDir+lcTmpPoH,' ','EX')
INDEX ON Po TAG (lcTmpPoH)

* Get the postion of the POSHDR file in the Option Grid. [Begin]
lnPoPos   = lfItmPos('POSHDR.PO')

*- change the filter and collect the data from the cursour if you browse 
*- in range and select POs that you select only to spped up the performance.
IF "POSHDR.PO" $ lcRpExp
  PRIVATE lcPoExp , lcBrowPos
  lcPoExp = ""

  *-- Copy all laOGVrFlt to another array to save the old value.
  DIMENSION laTempExpr[1] , laBrTmpFlt[1]
  =ACOPY(laOGVrFlt,laTempExpr)
  =ACOPY(laBrFldFlt,laBrTmpFlt)

  DIMENSION laOGVrFlt[1,7] , laBrFldFlt[1,5]
  STORE "" To laOGVrFlt , laBrFldFlt
  =ACOPY(laTempExpr,laOGVrFlt,AELEMENT(laTempExpr,lnPoPos,1),7)
  
  *-- Convert numeric position to string
  lcBrowPos = ALLTRIM(STR(lnPoPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGVrFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
      EXIT
    ENDIF
  ENDFOR
  
  *-- Generate InList expression for transaction Number.
  lcPoExp = gfGenFlt('laOGVrFlt',.T.,.T.)
  
  *- Replace the Inlist in Poshdr.po withe .T.
  lcRpExp = STRTRAN(lcRpExp,lcPoExp,".T.")
  
  *-- Restore original laOGVrFlt , laBrFldFlt
  DIMENSION laOGVrFlt[1] , laBrFldFlt[1]
  =ACOPY(laTempExpr,laOGVrFlt)    && restore Original array
  =ACOPY(laBrTmpFlt,laBrFldFlt)
ENDIF

*- If you select PO in the in browse you will get this file from the Option Grid.
lcTPoFile  = laOGVrFlt[lnPoPos,6]
llWorkDeal = !EMPTY(lcTPoFile) AND USED(lcTPoFile) AND RECCOUNT(lcTPoFile) > 0


*-- Remove the set skip to To speed up the performance that he don't need it
*-- It need it in the FRX only not at at Say reports.
SELECT  POSHDR
SET SKIP TO

*-- Selecting the required Po according to the selection criteria
IF llWorkDeal
  SELECT (lcTPoFile)
  SET RELATION TO 'P'+PO INTO PoSHdr ADDITIVE
  GOTO TOP
  SCAN REST WHILE !EOF()
  lcNewPo = PO
    IF !SEEK(Po,lcTmpPoH)
      SELECT PoSHdr
        SCAN REST WHILE !EOF() FOR  &lcRpEXp .AND. PoSHdr.PO = lcNewPo
          SCATTER MEMVAR MEMO
          INSERT INTO (lcTmpPoH) FROM MEMVAR
      ENDSCAN  
     ENDIF
  ENDSCAN
  SELECT (lcTPoFile)
  SET RELATION TO 
ELSE

  SELECT PoSHdr
  SCAN FOR &lcRpExp 
    IF !SEEK(Po,lcTmpPoH)
      SCATTER MEMVAR
      INSERT INTO (lcTmpPoH) FROM MEMVAR
    ENDIF
  ENDSCAN
  *-- End For If Statement .[Begin]
ENDIF

SELECT (lcTmpPoH)
GOTO TOP
IF EOF()  
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  IF USED(lcTmpPoH)
    USE IN &lcTmpPoH
    ERASE (gcWorkDir+lcTmpPoH+'.DBF')
    ERASE (gcWorkDir+lcTmpPoH+'.CDX')    
  ENDIF
  RETURN
ENDIF

lnMaxRow = 51
PAGENO   = 0
SELECT POSHDR

llNewDoc = .T.
WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT
DO WHILE INKEY() <> 32
   SELECT &lcTmpPoH
   IF EOF()
      EXIT
   ENDIF
   IF FLAG = 'N'
      SKIP
      LOOP
   ENDIF
   IF llNewDoc
      lcPo = PO
      SELECT POSHDR
      =SEEK(lcRpForm + lcPo)
      IF EOF()
         SELECT &lcTmpPoH
         SKIP
         LOOP
      ENDIF
      =lfGetCodes()
      lcVendor = POSHDR->VENDOR
      SELECT POSLN
      
      =SEEK(lcRpForm + lcPo)
      IF EOF()
         SELECT &lcTmpPoH
         SKIP
         LOOP
      ENDIF
      lcVName = lcVenName
      lcVAddr1 = laVenAdr[1]
      lcVAddr2 = laVenAdr[2]
      lcVAddr3 = laVenAdr[3]
      lcVAddr4 = laVenAdr[4]
      IF LEN(TRIM(lcVAddr2)) =0
        lcVAddr2 = lcVAddr3
        lcVAddr3 = ''
      ENDIF
      SELECT POSHDR
      lcStName  = lcShpname
      lcStAddr1 = lashpadr[1]
      lcStAddr2 = lashpadr[2]
      lcStAddr3 = SUBSTR(ALLTRIM(lashpadr[3]),1,15)+' '+;
                 SUBSTR(ALLTRIM(lashpadr[4]),1,3)+' '+;
                 SUBSTR(ALLTRIM(lashpadr[5]),1,10)
      IF LEN(TRIM(lcStAddr2)) =0
         lcStAddr2 = lcStAddr3
         lcStAddr3 = ''
      ENDIF
      =SEEK(PosHdr.cwarecode,'WAREHOUS')
      lcStAddr4 = ALLTRIM(WareHous.cAddress6)

      Hline1 = IIF (EMPTY(lcDivDesc), lcLine1, lcDivDesc)

      SELECT POSHDR
      IF llRpFc
       lcCurr = IIF(llRpFc,cPriceCur,gcBaseCurr)
      ELSE
       lcCurr='USD'
      ENDIF
      lnRate = RATE
      lnICost2 = NICOST2
      lnICost3 = NICOST3
      lnICost4 = NICOST4
      lnICost5 = NICOST5
      IF llRpFc
        lnICost2 = IIF(lcIType7  = 'D',lnICost2*nDutyRat,lnICost2)
        lnICost3 = IIF(lcIType8  = 'D',lnICost3*nDutyRat,lnICost3)
        lnICost4 = IIF(lcIType9  = 'D',lnICost4*nDutyRat,lnICost4)
        lnICost5 = IIF(lcIType10 = 'D',lnICost5*nDutyRat,lnICost5)
      ENDIF
      
      lcPrv_Style =SPACE(15)
      
      llNOTELOOP = .F.
      llSNOTLOOP = .F.
      lnTOTQTY   = 0.00
      lnTOTORD   = 0.00
      lnGTOTORD  = 0.00
      lnGTOTQTY  = 0.00
   ENDIF

   SELECT POSHDR
   COL1=ROUND((80-LEN(ALLTRIM(HLINE1)))/2,0)
   @ 01,04 SAY 'PO # : '+PO
   @ 01,COL1 SAY ALLTRIM(HLINE1)
   @ 01,60 SAY 'DATE  :'+DTOC(ENTERED)
   PAGENO = PAGENO +1

   @ 02,COL2 SAY HLINE2
   @ 02,60 SAY 'PAGE #: '
   @ 02,68 SAY PAGENO PICTURE '999'
   @ 03,COL3 SAY HLINE3
   @ 04,COL4 SAY HLINE4
   @ 05,COL5 SAY HLINE5
   @ 07,26 SAY 'P U R C H A S E     O R D E R'
   SELECT POSHDR
   @ 09,04 SAY '........ VENDOR .........'
   @ 09,47 SAY '........ SHIP TO ........'
   @ 10,04 SAY lcVName
   @ 10,47 SAY lcStName
   @ 11,04 SAY lcVAddr1
   @ 11,47 SAY lcStAddr1
   @ 12,04 SAY lcVAddr2
   @ 12,47 SAY lcStAddr2
   @ 13,04 SAY lcVAddr3
   @ 13,47 SAY lcStAddr3
   @ 14,04 SAY lcVAddr4
   @ 14,47 SAY lcStAddr4
   @ 15,04 SAY 'ATTN: '+CONTACT
   @ 17,04 SAY 'DIVISION     :'+LEFT(lcDivDesc,15)
   @ 17,47 SAY 'SHIP VIA   :'+LEFT(lcShipVia,15)
   @ 18,04 SAY 'COMPLETE     :'
   @ 18,18 SAY COMPLETE
   @ 18,47 SAY 'QUOTA CATG.:'+ QUOTACAT
   @ 19,04 SAY 'TERMS        :'+LEFT(lcTerms,15)
   @ 19,47 SAY 'ORIGIN     :'+ORIGIN
   *--MHM2000
   *@ 20,04 SAY 'F.O.B.       :'+ FOB
   @ 20,04 SAY 'F.O.B.       :'+ CFOB
   *--MHM2000
   @ 20,47 SAY "INDIA'S PO#:"+INSURANCE
   @ 21,04 SAY 'LETTER CREDIT:'+LC
   @ 22,00 SAY A
   @ 23,00 SAY C
   @ 24,00 SAY D
   @ 25,00 SAY A
   ROW       = 26
   llNewDoc  = .T.
   lcSCALE   = ' '
   lcPREVSCALE = ' '
   lnCOUNT    = 0
   llREPEAT   = .F.
   SELECT POSLN
   DO WHILE .T.
     IF EOF() .OR. PO <> lcPo
          llNewDoc   = .T.
          EXIT
     ENDIF
     IF (TRANCD='1') .AND. (TOTQTY>0)
       IF ROW>=lnMaxRow
         llNewDoc = .F.
         EXIT
       ENDIF
       lcColor     = SUBSTR(Style,lnClrPo,lnColorLen)  
       lcClr_Desc  = gfCodDes(lcColor,'COLOR     ')
       =gfRltFld(lcColor, @laClrLName ,'COLOR     ')
       =SEEK(PosLn.Style,'Style')
       lcStyDesc = Style.Desc

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*LN# GRP STYLE   CLR                                 PRICE   PIECES       AMOUNT
*123  X  1234567 123 12345678901234567890          1234.99   123456   123,456.99
*            11111 22222 33333 44444 55555 66666 77777 88888
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

       IF SCALE<>lcPREVSCALE                  
         llREPEAT = .F.
         lcPREVSCALE=SCALE                   
         lcSCALE   = GETSCALE(STYLE->SCALE,SPACE(0) )
         lnCOUNT   = SCALE->CNT
	   ELSE
	   	 llREPEAT = .T.
	   ENDIF
       SELECT POSLN
       lcStyMaj = SUBSTR(POSLN->STYLE,1,lnMajLen)
       
       IF (lcStyMaj <> lcPrv_Style) .OR. (llSNOTLOOP)
         IF !llSNOTLOOP
           @ ROW,00 SAY lcStyMaj
           @ ROW,19 SAY SUBSTR(lcStyDesc,1,20)
           ROW=ROW+1
           lcPrv_Style = SUBSTR(STYLE,1,lnMajLen)
           IF llrpPrtSn
             SELECT NOTEPAD
             =SEEK ('F'+lcStyMaj)
             llSNOTLOOP = IIF(FOUND(), .T., .F.)
             lnMline = 1
           ENDIF
         ENDIF
         lnMemWidth = SET("MEMOWIDTH")
         SET MEMOWIDTH TO 75
         IF llrpPrtPn  .OR. llrpPrtSn
           lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
         ENDIF  
         DO WHILE llSNOTLOOP .AND. llrpPrtSn 
           SELECT NOTEPAD
           IF lnMline > lnMTotLin
             llSNOTLOOP = .F.
             EXIT
           ENDIF
           IF ROW >= 55
             llNewDoc=.F.
             EXIT
           ENDIF 
           IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)
             @ ROW,04 SAY MLINE(MNOTES,lnMline)
             ROW = ROW+1
           ENDIF
           lnMline = lnMline + 1
         ENDDO
         SET MEMOWIDTH TO lnMemWidth
       ENDIF
       IF ROW >= lnMaxRow
         llNewDoc=.F.
         EXIT
       ENDIF
       SELECT POSLN
       IF lnCOUNT >1 
         *khm add the variable of print size to the If command
         IF !llREPEAT AND llRpSize
           lnCont=1
           lnCnt=1
           DO WHILE lnCont<=8
             lcScl=SUBSTR(lcSCALE,lnCnt,5)
             lnCnt=lnCnt+5               
             lcScl=SPACE(1)+lcScl        
             @ ROW,(lnCont*7)+16 SAY PADL(ALLTRIM(lcScl),6)
             lnCont=lnCont+1
           ENDDO
           ROW = ROW + 1
           *khm add the variable of print size to the If command
         ENDIF
       ENDIF
	   @ ROW,00 SAY lcColor
       @ ROW,07 SAY LEFT(lcClr_Desc,15)

       *--- DISPLAY THE SIZE SCALE AND BUCKET QUANTITIES

       *khm add the variable of print size to the If command       
       IF llRpSize
         lnCont = 1
         DO WHILE lnCOUNT >1 .AND. lnCont<=8
            lcCuntr =STR(lnCont,1)
            @ ROW,(lnCont*7)+16 SAY QTY&lcCuntr   PICTURE '@Z 999999'
            lnCont = lnCont+1
         ENDDO
         ROW = ROW+1
       ENDIF  
      *khm add the variable of print size to the If command

       IF !EMPTY (lcCLrDesc ).                          
         @ ROW,00 SAY LEFT(lcCLrDesc,39)
       ENDIF
       @ ROW,40 SAY TOTQTY PICTURE '9999999'
       @ ROW,50 SAY lcCurr
       IF llRpLPrnt
         @ ROW,55 SAY  IIF(lcRpPrice='C' , ;
          IIF(llrpfc,Posln.ncost1,Posln.NeCost1) ,;
          IIF(lcRpPrice='P',ROUND(IIF(lcRpForm = 'N' .AND. llrpPdic,;
             (1-lnDisc/100),1) *Style.nSugRetPri  ,2),'')) PICTURE '999999.99'
         *IF llRpFc
         * @ ROW,55 SAY nCost1 PICTURE '999999.99'
         *ELSE
         * @ ROW,55 SAY (nCost1/lnRate) PICTURE '999999.99'
         *ENDIF
       ENDIF
       
       lnAmount= IIF(lcRpPrice='C',IIF(llrpfc,Posln.ncost1, Posln.NeCost1) ,;
       IIF(lcRpPrice='P', ROUND(IIF(lcRpForm = 'N' .AND. llrpPdic,;
       (1-lnDisc/100),1) *Style.nSugRetPri  ,2),0 ))* Posln.totqty
       *IF llRpFc
       *  lnAmount= TOTQTY * nCost1
       *ELSE
       *  lnAmount= TOTQTY * (nCost1/lnRate)
       *ENDIF
       IF llRpLPrnt
         *--MHM2000
         IF lcRpPrice <> 'N'
           @ ROW,66 SAY lnAmount  PICTURE '9999999999.99'   
         ELSE
           @ ROW,66 SAY " "
         ENDIF  
       ENDIF
       ROW = ROW +1
       
       *--- SUM ORDER TOTALS
       lnTOTQTY  = lnTOTQTY+ TOTQTY
       lnTOTORD  = lnTOTORD+ lnAmount
       ROW = ROW+1
     ENDIF

     lcStyle=SUBSTR(STYLE,1,lnMajLen)
     
     SELECT POSLN
     SKIP
     *--- PRINT STYLE SUBTOTAL
     IF (EOF()) .OR. (SUBSTR(STYLE,1,lnMajLen)<>lcStyle) .OR. (PO <> lcPo)
        @ ROW,10 SAY '*** SUB TOTAL ***'
        @ ROW,40 SAY lnTOTQTY PICTURE '9999999'
        IF llRpLPrnt
         *--MHM2000
          IF lcRpPrice <> 'N'
            @ ROW,66 SAY lnTOTORD PICTURE '9999999999.99'
          ELSE
            @ ROW,66 SAY " "
          ENDIF  
        ENDIF
        ROW = ROW + 2
        lnGTOTQTY = lnGTOTQTY+lnTOTQTY
        lnGTOTORD = lnGTOTORD+lnTOTORD
        lnTOTQTY  = 0
        lnTOTORD  = 0
     ENDIF
   ENDDO
   IF llNewDoc .AND. !llNOTELOOP .AND. llrpPrtPn
      SELECT NOTEPAD
      =SEEK ('P'+lcPo)
      llNOTELOOP = IIF(FOUND(), .T., .F.)
      lnMline = 1
   ENDIF
   IF llrpPrtPn  
     lnMemWidth = SET("MEMOWIDTH")
     SET MEMOWIDTH TO 75
     lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   DO WHILE llNOTELOOP .AND. llrpPrtPn
     SELECT NOTEPAD
     IF lnMline > lnMTotLin
       llNewDoc   = .T.
       llNOTELOOP = .F.
       EXIT
    ENDIF
    IF ROW >= lnMaxRow
      llNewDoc = .F.
      EXIT
    ENDIF
    IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
      @ ROW,04 SAY MLINE(MNOTES,lnMline)
      ROW = ROW+1
    ENDIF
    lnMline = lnMline + 1
   ENDDO
   SET MEMOWIDTH TO lnMemWidth
   IF !llNewDoc
     @ 56,00 SAY REPLICATE('-',80)         
     @ 57,13 SAY 'C O N T I N U E D ...'   
     @ 57,69 SAY '**********'
     @ 58,00 SAY REPLICATE('-',80)
     LOOP
   ENDIF
   @ 55,00 SAY A
   @ 56,10 SAY 'TOTAL  =>'
   @ 56,40 SAY lnGTOTQTY  PICTURE '9999999'
   @ 56,49 SAY 'PIECES'
   IF llRpLPrnt
     IF lcRpPrice <> 'N'
       @ 56,66 SAY lnGTOTORD  PICTURE '9999999999.999'   
     ELSE
       @ 56,66 SAY ""
     ENDIF 
   ENDIF 
   ROW = 57
   IF lnICost2  <> 0 .AND. llRpLPrnt
     @ ROW,10 SAY 'ESTIMATED '+ lcISLbl2
     *--MHM2000
     IF lcRpPrice <> 'N'
       @ ROW,69 SAY lnICost2 PICTURE '9999999.999'
     ELSE
       @ ROW,69 SAY ""
     ENDIF  
     ROW=ROW+1
   ENDIF
   IF lnICost3  <> 0 .AND. llRpLPrnt
     @ ROW,10 SAY 'ESTIMATED ' +  lcISLbl3
     IF lcRpPrice <> 'N'
       @ ROW,69 SAY lnICost3 PICTURE '9999999.999'
     ELSE
       @ ROW,69 SAY " "
     ENDIF  
     ROW=ROW+1
   ENDIF
   IF lnICost4  <> 0 .AND. llRpLPrnt
     @ ROW,10 SAY 'ESTIMATED '+ lcISLbl4
     IF lcRpPrice <> 'N'
       @ ROW,69 SAY lnICost4 PICTURE '9999999.999'
     ELSE
       @ ROW,69 SAY " "
     ENDIF  
     ROW=ROW+1
   ENDIF
   IF lnICost5  <> 0 .AND. llRpLPrnt
     @ ROW,10 SAY 'ESTIMATED '+ lcISLbl5
     IF lcRpPrice <> 'N'
       @ ROW,69 SAY lnICost5 PICTURE '9999999.999'
     ELSE
       @ ROW,69 SAY " "
     ENDIF  
     ROW=ROW+1
   ENDIF
   lnGAmt=lnGTOTORD+lnICost2+lnICost3+lnICost4+lnICost5
   IF llRpLPrnt
     @ ROW,10 SAY 'TOTAL PURCHASE ORDER AMOUNT'
     @ ROW,50 SAY lcCurr
     *--MHM200
     IF lcRpPrice <> 'N'
       @ ROW,66 SAY lnGAmt PICTURE '9999999999.999'
     ELSE
       @ ROW,66 SAY " "
     ENDIF  
     ROW = ROW + 1
   ENDIF
   SELECT &lcTmpPoH
   SKIP
   IF EOF()
      EXIT
   ENDIF
ENDDO
SELECT POSLN
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/22/2000
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/22/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGVrFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGVrFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!*************************************************************
