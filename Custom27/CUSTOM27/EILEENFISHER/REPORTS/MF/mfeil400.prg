*!*************************************************************************
*! Name : MFEIL400.prg (Converted from 26 to 27 for EILEEN FILSHER)
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*! ************************************************************************
*! Synopsis : Pieces left to sell report  (Production Report) .
*!            Custom program  for EILEEN FISHER INC.  (EIL100).
*!            Note : The grid is Similar to ORD920.prg (open orders report).
*! ************************************************************************
*! Calls : 
*!         Procedures : lpBuldTmp
*!                      lpPrint,lpHeder,lpDetail,lpFooter,lpLastPrt.
*!         Functions  : lfCalAve().
*!*************************************************************************

*--Adjust filter
*- Coordinate groups
lcRpExp = IIF(llRpCoGrps,lcRpExp+' AND !EMPTY(GROUP) ',lcRpExp)
IF lcRpSelect = 'S'
  lcRpExp = lcRpExp +" AND !EOF('STYLE') AND STYLE.MAKE "
ENDIF
lcRpExp = lcRpExp + 'AND TOTQTY>0'
SELECT ORDLINE
GO TOP
WAIT WINDOW 'Collecting data.....' NOWAIT
LOCATE FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
COPY STRUCTURE TO (gcWorkDir+lcOrdLTemp)
=gfOpenFile(gcWorkDir+lcOrdLTemp,'','EX')
SELECT ORDLINE
SCAN REST FOR &lcRpExp
  SCATTER MEMVAR MEMO
  INSERT INTO (lcOrdLTemp) FROM MEMVAR
ENDSCAN
SET RELATION TO
SELECT (lcOrdLTemp)
SET RELATION TO 'O'+ORDER INTO ORDHDR
SET RELATION TO STYLE INTO STYLE ADDITIVE
GO TOP
*--Create report temp.
CREATE TABLE (gcWorkDir+lcRepTemp) (Fabric C(7),Style C(19),Size C(1),SizDesc C(6),;
                                    Cnt N(1),StyDesc C(20),LastOrd C(6),Ave_Cones N(6,2),;
                                    Price N(6,2),Qty N(6),Amount N(10,2),Yds N(8),Cut N(6))
*-- Create new main index (fabric+style+size+color).
INDEX ON Fabric+SUBSTR(Style,1,lnMajorLen)+size+SUBSTR(Style,lnNonMajSt,lnColorLen) TAG (lcRepTemp)
*--get The cost element.
lcMType1 = gfGetMemVar('M_CMType1')
lcMType2 = gfGetMemVar('M_CMType2')
lcMType3 = gfGetMemVar('M_CMType3')
lcMType4 = gfGetMemVar('M_CMType4')
lcMType5 = gfGetMemVar('M_CMType5')
*--Get the number of the cost element for fabric,trim,mfg operation.
DIMENSION laFabNo[1]
STORE '0' TO laFabNo
=lfCstAry('lcMType','F','laFabNo')
SELECT (lcOrdLTemp)
GO TOP
lnPrice = 0
STORE SPACE(07) TO lcFab
STORE SPACE(lnMajorLen) TO lcStyle
SCAN
  FOR lnI=1 TO ALEN(laFabNo)
    *-- BOM is indexed on (citmmajor+typ+citmmask+mfgcode+item+iclr).
    SELECT BOM
    *--If the cost element does not exist.
    IF !SEEK(PADR(SUBSTR(&lcOrdLTemp..Style,1,lnMajorLen),19)+laFabNo[lnI])
      LOOP
    ENDIF
    *-- Go through only the fabric records.
    SCAN WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
               PADR(SUBSTR(&lcOrdLTemp..Style,1,lnMajorLen),19)+laFabNo[lnI] ;
         FOR  SUBSTR(cItmMask,lnNonMajSt,lnColorLen)=SUBSTR(&lcOrdLTemp..Style,lnNonMajSt,lnColorLen);
         .OR. SUBSTR(cItmMask,lnNonMajSt,lnColorLen)='******' .AND. !EMPTY(Item)
      SELECT (lcOrdLTemp)
      IF SUBSTR(BOM.Item,1,7)+SUBSTR(Style,1,lnMajorLen) <> lcFab+lcStyle
        lcStyle = SUBSTR(Style,1,lnMajorLen)
        lcFab   = SUBSTR(BOM.Item,1,7)
        *-- calculate ave.cons for style.
        lnAve   = lfCalAve(lcStyle,lcFab,laFabNo[lnI])
      ENDIF
      lcPrompt  = ALLTRIM(SUBSTR(&lcRepTemp..Style,1,lnMajorLen))+"\"+ALLTRIM(SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen))+"\" +;
                  ALLTRIM(&lcRepTemp..SizDesc)
      WAIT WINDOW lcPrompt NOWAIT
      SELECT (lcRepTemp)
      FOR I=1 TO SCALE.CNT
        Z=STR(I,1)
        IF ! SEEK(lcFab+lcStyle+Z+SUBSTR(&lcOrdLTemp..Style,lnNonMajSt,lnColorLen))
          APPE BLANK   
          REPLACE FABRIC    WITH SUBSTR(BOM.Item,1,7),;
                  STYLE     WITH &lcOrdLTemp..Style,;   
                  STYDESC   WITH STYLE.DESC,;
                  LASTORD   WITH &lcOrdLTemp..ORDER,;    
                  SIZE      WITH Z,;
                  SIZDESC   WITH SCALE.SZ&Z,;
                  CNT       WITH SCALE.CNT,;       
                  QTY       WITH &lcOrdLTemp..QTY&Z,;         
                  AMOUNT    WITH (&lcOrdLTemp..QTY&Z*&lcOrdLTemp..PRICE),;
                  AVE_CONES WITH lnAve,;
                  PRICE     WITH lnPrice,;
                  YDS       WITH (lnAve*&lcOrdLTemp..QTY&Z),;
                  CUT       WITH (STYLE.WIP&Z+STYLE.STK&Z) 
        ELSE
          *--Include the orders that have multiple style\color lines.
            REPLACE QTY     WITH (QTY+&lcOrdLTemp..QTY&Z),;         
                    AMOUNT  WITH (AMOUNT+(&lcOrdLTemp..QTY&Z*&lcOrdLTemp..PRICE)),;
                    LASTORD WITH &lcOrdLTemp..ORDER,;
                    YDS     WITH (YDS+(lnAve*&lcOrdLTemp..QTY&Z))
        ENDIF 
      ENDFOR
    ENDSCAN
  ENDFOR
ENDSCAN
SELECT (lcRepTemp)
*--Create temproary index to know no. of colors.
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW  'Sorting &Z selected records for report ...' NOWAIT
SET UNIQUE ON
INDEX ON SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen) TAG (lcRepTemp1)
SET UNIQUE OFF
COUNT ALL TO lnClrNo
IF lnClrNo = 0
  *--Non of the selected styles have any cost sheet. Cannot proceed.
  =gfModalGen('TRM42158B4200','ALERT')
  *--Remove the temp files
  USE IN (lcOrdLTemp)
  ERASE (gcWorkDir+lcOrdLTemp)+'.DBF'
  USE IN (lcRepTemp)
  ERASE (gcWorkDir+lcRepTemp)+'.DBF'
  RETURN
ENDIF
DIMENSION laClr(lnClrNo),;
          laClrTot(lnClrNo),;
          laYrdNed(lnClrNo),;
          laYDSAvl(lnClrNo),;
          laCutTot(lnClrNo)
laClrTot = 0
laCutTot = 0
laYrdNed = 0
GO TOP
*--Build the colors array.
lnCnt=1
SCAN
  laClr(lnCnt)=SUBSTR(Style,lnNonMajSt,lnColorLen)
  lnCnt=lnCnt+1
ENDSCAN
SET ORDER TO TAG (lcRepTemp)
*-- Print Report.
PAGENO = 0
ldDate  = DTOC(DATE())
R_WIDTH = 'W'
*-- Added a new function calling to print format "A" of the report.
llDispRep  = .T.
*--Adjust the scape sequence.
lcPdSETup  = ''
=lfSetDev()
DO CASE
  CASE lcRpForm = "A"
    llDispRep = lfPrtFrmA()
  CASE lcRpForm = "B"
    DO lpPrint
  CASE lcRpForm = "C"
    llNothing = lfPrtFrmA()
    lcRpForm = "B"
    DO lpPrint
    lcRpForm = 'C'
ENDCASE  
S_CLOSEALL = .F.       && Not to close the opened files because
                       && it's possible to reprint the report again
SELECT (lcOrdLTemp)    && Select file not include the CNT feild.
QP10CPI = .F.          && Do not allow page ejecting.
*--Set to maupulate the printing.
SET CONSOLE ON
_pdsetup = lcPdSETup
*-- Do not print the report if the user selected only format "A" and we did not print the format. 
IF llDispRep
  DO ENDREPORT
ENDIF
*--Remove the temp files
USE IN (lcOrdLTemp)
ERASE (gcWorkDir+lcOrdLTemp)+'.DBF'
USE IN (lcRepTemp)
ERASE (gcWorkDir+lcRepTemp)+'.DBF'
RETURN
*---------------------------
*   End prog.  MFEIL400.PRG
*---------------------------

*!**************************************************************
*! FUNC : lfCalAve()
*! DESC : Function to calculate to ave.cons for style.
*! PARA : Style , Fabric 
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*!**************************************************************
FUNCTION lfCalAve
PARA lcStyle,lcFab,lcFabType

IF EMPTY(lcStyle)
  RETURN(0)
ENDIF
lnPrice  = STYLE.PriceA
lnNoSClr = 0
SELECT BOM
lnAve = 0
lnRec = RECNO()
SCAN WHILE cItmMajor+Typ = PADR(lcStyle,19)+lcFabType FOR SUBSTR(ITEM,1,7) = lcFab
  lnNoSClr=lnNoSClr+1
ENDSCAN
GOTO IIF(BETWEEN(lnRec,1,RECCOUNT()),lnRec,RECNO())
SCAN WHILE cItmMajor+Typ = PADR(lcStyle,19)+lcFabType FOR SUBSTR(ITEM,1,7) = lcFab
  lnAve=lnAve+IIF(SUBSTR(cItmMask,lnNonMajSt,lnColorLen)='******',nBomTotQty,(nBomTotQty/lnNoSClr))
ENDSCAN
GOTO IIF(BETWEEN(lnRec,1,RECCOUNT()),lnRec,RECNO())
RETURN(lnAve)

*!**************************************************************
*! PROG : lpPrint
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 09/05/94.
*! DESC : Print forms. 
*!**************************************************************
PROCEDURE lpPrint

*-- Define escape sequence..
lcLetter  = "CHR(27)+'&l2A'"                 &&Letter parer 
llLegal   = "CHR(27)+'&l3A'"                 &&Legal paper   
lcLandScp = "CHR(27)+'&l1O'"                 &&Landscape Mode 
lcComprsd = "CHR(27)+'&k2S'"                 &&Compresed.
lnCunt = IIF(lcRpForm ='C',2,1)
lcRpForm = IIF(lcRpForm ='C','A',lcRpForm)
WAIT WINDOW "Printing format " + ALLTRIM(lcRpForm) + ".. press <SPACE BAR> to abort" NOWAIT
IF gcDevice = "PRINTER"
  SET PRINT ON
  SET DEVICE TO PRINT
  @ 00,00
  IF lcRpPaTyp='T'
    @ PROW(),PCOL() SAY &lcLetter+&lcLandScp+&lcComprsd
  ELSE
    @ PROW(),PCOL() SAY &llLegal+&lcLandScp+&lcComprsd
  ENDIF
ENDIF  
FOR lnNO=1 TO lnCunt 
  *-- Define printed lines..
  IF lcRpForm = 'A'
    lnMClrNo=IIF(lnClrNo>10,10,lnClrNo) 
    lnPrtCol=45+(7*lnMClrNo)+8
  ELSE
    IF lcRpPaTyp='L'
      lnMClrNo=IIF(lnClrNo>5,5,lnClrNo) 
    ELSE
      lnMClrNo=IIF(lnClrNo>3,3,lnClrNo) 
    ENDIF
    lnPrtCol=45+(21*lnMClrNo)+8
  ENDIF
  *-- Printed report lines depend on wich forma and no. of colors used.
  lcMidLn1=REPLICATE(IIF(lcRpForm = 'A','      ณ','      ณ      ณ      บ'),lnMClrNo)
  lcMidLn2=REPLICATE(IIF(lcRpForm = 'A','ฤฤฤฤฤฤล','ฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤื'),lnMClrNo)
  lcMidLn3=REPLICATE(IIF(lcRpForm = 'A','ออออออุ','ออออออัออออออัออออออฮ'),lnMClrNo)
  lcMidLn4=REPLICATE(IIF(lcRpForm = 'A','ฤฤฤฤฤฤย','ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤา'),lnMClrNo)
  lcMidLn5=REPLICATE(IIF(lcRpForm = 'A','ฤฤฤฤฤฤม','ฤฤฤฤฤฤมฤฤฤฤฤฤมฤฤฤฤฤฤะ'),lnMClrNo)
  lcMidLn6=REPLICATE(IIF(lcRpForm = 'A','       ','                     '),lnMClrNo)
  lcMidLn7=REPLICATE('ออออออุออออออุออออออฮ',lnMClrNo)
  lcLN1= 'ณ                   ณ                    ณ      ณ     ณ'+lcMidLn1+'        ณ         ณ        ณ             ณ          ณ'
  lcLN2= 'รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤล'+lcMidLn2+'ฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤด'
  lcLN6= 'ฦอออออออออออออออออออุออออออออออออออออออออฯออออออุอออออุ'+lcMidLn7+'ออออออออุอออออออออุออออออออุอออออออออออออุออออออออออต'
  lcLN3= 'รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤล'+lcMidLn2+'ฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤด'
  lcLN4= 'ณ                   ณ                           ณ     ณ'+lcMidLn1+'        ณ         ณ        ณ             ณ          ณ'
  lcLN7= 'ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤย'+lcMidLn4+'ฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฟ'
  lcLN8= 'ฦอออออออออออออออออออุออออออออออออออออออออุออออออุอออออุ'+lcMidLn3+'ออออออออุอออออออออุออออออออุอออออออออออออุออออออออออต'
  lcLN5= 'ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤม'+lcMidLn5+'ฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤู'
  lcLN9= '                                                       '+lcMidLn6
  lcLN10='                                                       '+lcMidLn6+'             อออออออออออออออออออออออออออออออ'
  lcLN11='ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤมฤฤฤฤฤม'+lcMidLn5+'ฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤู'
  *-- Print loop.
  GO TOP
  STORE 0 TO lnSizTot,lnStyTot,lnPrice,lnAve
  lcFab = Fabric
  DO WHILE ( ! EOF() .AND. INKEY()<>32 )
    IF SEEK(lcFab)
      lnRec=RECNO()
      SUM REST WHILE Fabric=lcFab Qty,Amount TO lnSaleQty,lnSales
      GOTO lnRec
    ENDIF 
    DO lpHeder
    lcFab   = Fabric
    lcStyle = SPACE(lnMajorLen)
    lcSize  = SPACE(1)
    STORE 0 TO laClrTot,lnYdsNed,laYrdNed,laYDSAvl,laCutTot
    SCAN WHILE FABRIC=lcFab
      IF SUBSTR(Style,1,lnMajorLen)<>lcStyle
        IF PROW()+CNT >39
          *-- Print Subtotal for last style.
          DO lpLastPrt WITH .T.
          DO lpHeder
          lcStyle = SPACE(lnMajorLen)
          lcSize  = SPACE(1)
        ENDIF
      ENDIF
      DO lpDetail
    ENDSCAN
    SELECT FABRIC
    FOR J=1 TO lnClrNo
      IF SEEK(lcFab+laClr(J))
        laYDSAvl(J)=OnHand+OnOrder
      ENDIF
    ENDFOR
    SELE (lcRepTemp)
    DO lpLastPrt WITH .F.
    IF PROW() > 30
      DO lpHeder
    ENDIF
    lcFab = Fabric
    DO lpFooter
  ENDDO
  lcRpForm ='B'
ENDFOR
IF gcDevice = "PRINTER"
  EJECT PAGE
  SET PRINT OFF
  SET DEVICE TO SCREEN
ENDIF
RETURN

*!**************************************************************
*! PROG : lpHeder
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 09/05/94.
*! DESC : Print report Heder.
*!**************************************************************
PROCEDURE lpHeder

@ 00,00 
@ 01    ,02 SAY lcRepMsg1 + '                      PRODUCTION REPORT                                    Date: '+ ldDate
@ $+1   ,02 SAY lcRepMsg2 + '                 ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ'  
@ $+1   ,02 SAY lcRepMsg3 + '                 ณSALES    :'
@ $     ,61 SAY lnSales PICTURE '$$,$$$,$$$.99'+'  ณ'
@ $+1   ,02 SAY lcRepMsg4 + '                 รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
IF lcRpForm='A'
  @ $+1 ,02 SAY lcRepMsg5 + '                 ณPLANE    :               ณ'
ELSE
  @ $+1 ,02 SAY lcRepMsg5 + '                 ณPLANNED  :'
ENDIF
@ $     ,61 SAY lnRpPlan PICTURE '$$,$$$,$$$.99'+'  ณ'
@ $+1   ,02 SAY lcRepMsg6 + '                 ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู'
SELE FABRIC
SEEK lcFab
@ $+1   ,02 SAY 'FABRIC : '+lcFab+'  '+DESC+'             '+lcRpTitle
@ $+1   ,00 SAY lcLN7
@ $+1   ,00 SAY 'ณ                   ณ                    ณ AVE  ณ     ณ'
lnCol=55
SELECT CODES
FOR I=1 TO lnMClrNo
  lcClrDesc = gfCodDes(laClr(I) , 'COLOR')
  lcClrDesc = IIF(EMPTY(lcClrDesc ),SPACE(20),lcClrDesc)
  @ PROW(),lnCol SAY IIF(lcRpForm='A',SUBSTR(lcClrDesc,1,6)+'ณ',PADL(ALLTRIM(laClr(I)),6,' ')+' '+SUBSTR(lcClrDesc,1,13)+'บ')
  lnCol=lnCol+IIF(lcRpForm='A',7,21)
ENDFOR
SELECT (lcRepTemp)
@ PROW(),lnCol SAY 'TOTAL BYณTOTAL PCSณ     %  ณ    SALES    ณ    YDS   ณ'
@ $+1   ,00 SAY 'ณ      '+lcMajTitle+'        ณ     DESCRIPTION    ณ CONS ณ$ PERณ'
lnCol=55                                  &&C100421,1 MFM 06/15/95.
FOR I=1 TO lnMClrNo
  @ PROW(),lnCol SAY IIF(lcRpForm='A',PADL(ALLTRIM(laClr(I)),6,' ')+'ณ','CUT   :SALES :UNITS บ')
  lnCol=lnCol+IIF(lcRpForm='A',7,21)
ENDFOR
@ PROW(),lnCol SAY '  SIZE  ณPER '+lcMajTitle+'ณBY '+lcMajTitle+'ณ   BY '+lcMajTitle+'  ณ   NEEDED ณ'
@ $+1   ,00 SAY lcLN8
RETURN

*!**************************************************************
*! PROG : lpFooter
*! Auth : AHMED MOHAMMED MOHAMMED
*! Date : 01/06/98.
*! DESC : Print report Footer.
*! NOTES: This procedure is built from the old procedure and took 
*!        the same name, the old one name is lpFooter1.
*!**************************************************************
PROCEDURE lpFooter

@ PROW()+1,00 SAY lcLN6  
@ PROW()+1,00 SAY LEFT(lcLN4,21)+'TOTAL PIECES BY'+lcNonMajTl+'      ณ     ณ'
lnCol=IIF(lcRpForm = 'A',47,54)+8
FOR I=1 TO lnMClrNo
  IF lcRpForm = 'B'
    @ PROW(),lnCol-7 SAY laCutTot(I) PICTURE '999999'
  ENDIF
  @ PROW(),lnCol-1 SAY 'ณ'
  @ PROW(),lnCol SAY laClrTot(I) PICTURE '999999'
  @ PROW(),lnCol+6 SAY 'ณ      บ'
  lnCol=lnCol+IIF(lcRpForm = 'A',7,21)
ENDFOR
@ PROW(),lnPrtCol+02 SAY 'TTL PCS:'+'ณ'
@ PROW(),lnPrtCol+14 SAY lnSaleQty PICTURE '999999'+'ณ'
@ PROW(),lnPrtCol+25 SAY '='+'   ณ'
@ PROW(),lnPrtCol+30 SAY lnSales  PICTURE '$$,$$$,$$$.99'+'ณ'
@ PROW(),lnPrtCol+44 SAY lnYdsNed PICTURE '9999999.99'+'ณ'
@ $+1,00 SAY lcLN3
@ PROW()+1,00 SAY LEFT(lcLN4,21)+'PERCENTAGE BY '+lcNonMajTl+'       ณ     ณ'
lnCol=IIF(lcRpForm = 'A',47,54)+8
FOR I=1 TO lnMClrNo
  @ PROW(),lnCol-1 SAY 'ณ'
  @ PROW(),lnCol SAY (laClrTot(I)/lnSaleQty)*100 PICTURE '999.9%'
  @ PROW(),lnCol+6 SAY 'ณ      บ'
  lnCol=lnCol+IIF(lcRpForm = 'A',7,21)
ENDFOR
@ PROW(),lnCol-6 SAY SUBSTR(lcLN4,lnCol-5)
@ $+1,00 SAY lcLN3
@ PROW()+1,00 SAY 'ณ '+'ACTUAL'+'            ณ'
@ PROW(),13+8 SAY 'YARDAGE NEEDED'+'             ณ     ณ'
lnCol=IIF(lcRpForm = 'A',47,54)+8
FOR I=1 TO lnMClrNo
  @ PROW(),lnCol-1 SAY 'ณ'
  @ PROW(),lnCol SAY ROUND(laYrdNed(I),0) PICTURE '999999'
  @ PROW(),lnCol+6 SAY 'ณ      บ'
  lnCol=lnCol+IIF(lcRpForm = 'A',7,21)
ENDFOR
@ PROW(),lnCol-6 SAY SUBSTR(lcLN4,lnCol-5)
@ $+1,00 SAY lcLN3
@ PROW()+1,00 SAY 'ณ                   ณ'+'YARDAGE AVAILABLE'+'          ณ     ณ'
lnCol=IIF(lcRpForm = 'A',47,54)+8
FOR I=1 TO lnMClrNo
  @ PROW(),lnCol-1 SAY 'ณ'
  @ PROW(),lnCol SAY laYDSAvl(I) PICTURE '999999'
  @ PROW(),lnCol+6 SAY 'ณ      บ'
  lnCol=lnCol+IIF(lcRpForm = 'A',7,21)
ENDFOR
@ PROW(),lnCol-6 SAY SUBSTR(lcLN4,lnCol-5)
@ $+1,00 SAY lcLN3
@ PROW()+1,00 SAY 'ณ                   ณ'+'APPROX PCS LEFT TO SELL'+'    ณ     ณ'
lnCol=IIF(lcRpForm = 'A',47,61)+8
STORE 0 TO lnTotAVL,lnGTotAVL
FOR I=1 TO lnMClrNo
  IF lcRpForm = 'A'
    lnTotAVL=((laYDSAvl(I)-laYrdNed(I))/(laYrdNed(I)/laClrTot(I)))
  ELSE
    lnTotAVL=(laCutTot(I)-laClrTot(I))+(laYDSAvl(I)/(laYrdNed(I)/laClrTot(I)))
  ENDIF
  @ PROW(),lnCol-14 SAY  '      ณ      ณ'
  @ PROW(),lnCol SAY ROUND(lnTotAVL,0) PICTURE '999999'
  @ PROW(),lnCol+6 SAY 'บ'
  lnCol=lnCol+IIF(lcRpForm = 'A',7,21)
  lnGTotAVL=lnGTotAVL+lnTotAVL
ENDFOR
@ PROW(),lnPrtCol+10 SAY 'ณ'
@ PROW(),lnPrtCol+14 SAY  ROUND(lnGTotAVL,0) PICTURE '999999'
@ PROW(),lnPrtCol+20 SAY 'ณ'
@ PROW(),lnPrtCol+25 SAY '='+'   ณ'
lnProd=(lnGTotAVL*(lnSales/lnSaleQty))
@ PROW(),lnPrtCol+30 SAY lnProd PICTURE '$$,$$$,$$$.99'
@ PROW(),lnPrtCol+43 SAY 'ณ          ณ'
@ $+1,00 SAY lcLN5
@ $+1,00 SAY lcLN9
@ PROW(),lnPrtCol+18 SAY 'PRODUCING :'
@ PROW(),lnPrtCol+30 SAY (lnSales+lnProd) PICTURE '$$,$$$,$$$.99'
@ $+1,00 SAY lcLN10
@ $+1,00 SAY ''
RETURN

*!**************************************************************
*! PROG : lpLastPrt
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 09/05/94.
*! DESC : Print Subtotal for last style.
*! PARA : Called form (.T. detail loop),(.F. End of forma)
*!**************************************************************
PROCEDURE lpLastPrt
PARAMETERS llInDetail

lnStyTot=lnStyTot+lnSizTot
@ PROW(),PCOL() SAY SUBSTR(lcLn1,PCOL()+1,lnPrtCol+2-PCOL())
@ PROW(),lnPrtCol+2 SAY STR(lnSizTot,8)+'ณ'
@ PROW(),lnPrtCol+11 SAY STR(lnStyTot,9)+'ณ'
@ PROW(),lnPrtCol+25 SAY (lnStyTot/lnSaleQty)*100 PICTURE '999%'+'ณ'
@ PROW(),lnPrtCol+30 SAY (lnStyTot*lnPrice) PICTURE '$$,$$$,$$$.99'+'ณ'
@ PROW(),lnPrtCol+44 SAY (lnStyTot*lnAve) PICTURE '9999999.99'+'ณ'
IF llInDetail
  @ $+1   ,00 SAY lcLN11
  @ $+1,lnPrtCol+18 SAY 'CONTINUE....'
ENDIF
RETURN

*:************************************************************************
*: Prg  : lfCstAry
*: Desc.: Fill an array with the cost fileds no for the passed type.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfCstAry
PARAMETER lcFild, lcType, lcAray
PRIVATE lnCheck, lcVar, lcFild, lcType, lcAray

FOR lnCheck = 1 TO 5
  lcVar  = lcFild + ALLTRIM(STR(lnCheck))
  IF EVAL(lcVar) = lcType
    DIMENSION &lcAray[ALEN(&lcAray)+1]
    &lcAray[ALEN(&lcAray)] = ALLTRIM(STR(lnCheck))
  ENDIF
ENDFOR
=ADEL(&lcAray,1)
DIMENSION &lcAray[ALEN(&lcAray)-1]

*:*************************************************************
*: Program file  : lfPrtFrmA
*: Program desc. : Function to print format "A" of the report.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Calls : 
*:         Procedures : None
*:         Functions  : lfFABldTmp()
*:                      lfFAStrPrt() 
*:                      lfFAPrint () 
*:                      lfFAEndPrt() 
*:*************************************************************
* Format "A" layout..
***0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7...
*01                                                                              PRODUCTION REPORT                                                                               
*02  ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ                                                  DATE : ฐฐ/ฐฐ/ฐฐ   
*03  ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                    ณ SALES             : $$,$$$,$$$.99 ณ                                                  TIME : ฐฐ/ฐฐ/ฐฐ   
*04  ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                    รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด                                                  PAGE : ฐ          
*05  ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                    ณ PRODUCING APPROX. : $$,$$$,$$$.99 ณ                                                  USER : ฐฐฐฐฐฐฐฐฐฐ 
*06  ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู                                                                    
*07  ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                                                                                                                             
*08  FABRIC : ฐฐฐฐฐฐฐ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ                                                                                                                                       
*09ฺฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤฤฤยฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*10ณ STYLE      ณDESCRIPTION         ณCNSMPTณWHLSLณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณ      ณTOT PCSณ%  BYณ     SALES BYณ
*11ณ            ณ                    ณ  UNITณ $PERณ 1ฐฐฐฐณ 2ฐฐฐฐณ 3ฐฐฐฐณ 4ฐฐฐฐณ 5ฐฐฐฐณ 6ฐฐฐฐณ 7ฐฐฐฐณ 8ฐฐฐฐณ 9ฐฐฐฐณ 10ฐฐฐณ 11ฐฐฐณ 12ฐฐฐณ 13ฐฐฐณ 14ฐฐฐณSTY&CLRณSTYLEณ        STYLEณ
*12ฦออออออออออออุออออออออออออออออออออุออออออุอออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุอออออออุอออออุอออออออออออออต
*13ณฐฐฐฐฐฐฐฐฐฐฐฐณฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐณ999.99ณ999.9ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ9999999ณ 999%ณ$$,$$$,$$$.99ณ
*14ณ            ณฐฐฐฐฐฐ              ณ      ณ     ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ9999999ณ     ณ             ณ
*15ณ            ณฐฐฐฐฐฐ              ณ      ณ     ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ9999999ณ     ณ             ณ
*16ณ            ณฐฐฐฐฐฐ              ณ      ณ     ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ9999999ณ     ณ             ณ
*17รฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤฤฤลฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤด
*18ณฐฐฐฐฐฐฐฐฐฐฐฐณฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐณ999.99ณ999.9ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ9999999ณ 999%ณ$$,$$$,$$$.99ณ
*19ณ            ณฐฐฐฐฐฐ              ณ      ณ     ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ9999999ณ     ณ             ณ
*20ณ            ณฐฐฐฐฐฐ              ณ      ณ     ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ9999999ณ     ณ             ณ
*21ณ            ณฐฐฐฐฐฐ              ณ      ณ     ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ 99999ณ9999999ณ     ณ             ณ
*22ฦออออออออออออฯออออออออออออออออออออฯออออออฯอออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุออออออุอออออออุอออออุอออออออออออออต
*23ณTOTAL PIECES                                  ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ999999ณ9999999ณ     ณ$$,$$$,$$$.99ณ
*24ิออออออออออออออออออออออออออออออออออออออออออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯออออออฯอออออออฯอออออฯอออออออออออออพ
*25ฺฤฤฤฤฤฤยฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*26ณCOLOR ณTOT PCSณYDS NEEDEDณYDS ORDEREDณAPPROX PCS LTS                                                                                                           $$,$$$,$$$.99ณ
*27ณ      ณ       ณ          ณ           ณ        999999                                                                                                                        ณ
*28ฦออออออุอออออออุออออออออออุอออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออต
*29ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*30ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*..ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*..ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*..ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*..ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*..ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*..ณฐฐฐฐฐฐณ 999999ณ    999999ณ     999999ณ        999999                                                                                                                        ณ
*40ภฤฤฤฤฤฤมฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
*:*************************************************************

FUNCTION lfPrtFrmA

DIMENSION laSizes[1]        && Array to hold the printed sizes description.
laSizes    = SPACE(5)       && Initialize the array with empty space.
lnMaxSize  = 014            && The maximum number of sizes to be printed.
lcCrDevice = SET("DEVICE")  && Save the currecnt active device.
lcTmpFormA = gfTempName()   && The format temp file name.
lcFAClrTot = gfTempName()   && Temp file for the colors totals.
lnCurAlias = SELECT()       && Save the current alias number
llFAPrnted = .F.            && Flag to tell if the form was printed or not.
IF lfFABldTmp()             && If we could build the used temp file then...
  llNothing  = lfFAStrPrt() && Prepare the printer to start printing.
  llNothing  = lfFAPrint () && Print the report.
  llNothing  = lfFAEndPrt() && Reset the printer.
  llFAPrnted = .T.          && The form was printed.
ENDIF                       && That's it.
*--Adjust the scape sequence.
*=lfSetDev()
SELECT (lnCurAlias)         && Return to the presaved alias.
USE IN (lcTmpFormA)         && Close the temp cursor used by format A.
USE IN (lcFAClrTot)         && Close the colors cursor used by format A.
SET DEVICE TO &lcCrDevice   && Resore the previously active device.
RETURN (llFAPrnted)         && Return if the form was printed or not.

*:*************************************************************
*: Program file  : lfFAStrPrt
*: Program desc. : Function to setup the user printer before printing
*:                 the format. (Used for format "A" only)
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFAStrPrt

IF gcDevice = "PRINTER"
  *-- Define escape sequence..
  *-- These codes are written specially for the laser printer
  *-- that is used at Eileen Fisher.
  lcLetter   = "CHR(27)+'&l2A'"                 && Letter Parer 
  lcLegal    = "CHR(27)+'&l3A'"                 && Legal Paper   
  lcLandScap = "CHR(27)+'&l1O'"                 && Landscape Mode 
  lcComprsd  = "CHR(27)+'&k2S'"                 && Compressed Mode.
  
  SET PRINTER ON
  SET DEVICE TO PRINTER
  @ 00,00
  IF lcRpPaTyp = "T"
    @ PROW(),PCOL() SAY &lcLetter+&lcLandScap+&lcComprsd
  ELSE
    @ PROW(),PCOL() SAY &lcLegal+&lcLandScap+&lcComprsd
  ENDIF
ENDIF  

*:*************************************************************
*!FUNC  : lfFAEndPrt
*!DESC. : Function to reset the printer after finishing
*:                 printing the format. (Used for format "A" only)
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFAEndPrt

IF gcDevice = "PRINTER"
  EJECT PAGE
  SET PRINTER OFF
  SET DEVICE TO SCREEN
ENDIF

*:*************************************************************
*: FUNC : lfFABldTmp
*: DESC : Function to build the two temp files used by the formate.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Calls : 
*:         Procedures : None
*:         Functions  : lfFAGtSize()
*:                      lfFANewRec()
*:                      lfFAAddClr()
*:                      lfTherm()
*:                      lfFABrSizs()
*:                      lfFAClrThrm()
*:*************************************************************
FUNCTION lfFABldTmp

CREATE CURSOR (lcTmpFormA) ;
              (cType C(1)  , cFabric C(7), cStyle C(19),nSiz01 N(7,0)    ,;
               nSiz02 N(7,0), nSiz03 N(7,0), nSiz04 N(7,0),nSiz05 N(7,0) ,;
               nSiz06 N(7,0), nSiz07 N(7,0), nSiz08 N(7,0),nSiz09 N(7,0) ,;
               nSiz10 N(7,0), nSiz11 N(7,0), nSiz12 N(7,0),nSiz13 N(7,0) ,;
               nSiz14 N(7,0), cDesc C(20),nAveCones N(6,2),nPrice N(6,2) ,;
               nYDS N(8,0), nTotPcs N(8), nTotSals N(10,2) ,nPerSent N(6,2))
INDEX ON cFabric+SUBSTR(cStyle,1,lnMajorLen)+cType+SUBSTR(cStyle,lnNonMajSt,lnColorLen)+STR(RECNO(),6) TAG (lcTmpFormA)
CREATE CURSOR (lcFAClrTot) ;
              (cFabric C(7), cColor C(6), cType C(1), nTotPcs N(6,0),;
               nTotYdsNed N(6,0), nTotYdsOrd N(6,0), nTotAprox N(6,0))
INDEX ON cFabric+cType+cColor TAG (lcFAClrTot)
lnTotStPcs = 0
lnTotRecs  = RECCOUNT(lcRepTemp)
lnCurRec   = 0
SELECT (lcRepTemp)
SCAN
  lcCurFab  = &lcRepTemp..Fabric
  lcSizeFld = SPACE(1)
  lnCurRec  = lnCurRec + 1
  IF lfFAGtSize()
    llNothing  = lfFANewRec("A") AND lfFANewRec("B") 
    llNothing  = lfFAAddClr()
    lcPrompt   = "Fabric\"+lcMajTitle+"\"+lcNonMajTl+"\Size : "+ALLTRIM(&lcRepTemp..Fabric)+"\"+;
                  ALLTRIM(SUBSTR(&lcRepTemp..Style,1,lnMajorLen))+"\"+ALLTRIM(SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen))+"\" +;
                  ALLTRIM(&lcRepTemp..SizDesc)
    llNothing  = lfTherm (lnTotRecs,lnCurRec,"Compiling Data",lcPrompt)
  ELSE
    SET DEVICE TO SCREEN
    llNothing = lfFAClrThrm (lnTotRecs,lnCurRec,"Compiling Data","")
    IF gfDialog ("!","The number of sizes to be printed for "           +;
                     "the selected range of styles exceeds the "        +;
                     "maximum number of sizes that could be printed "   +;
                     "in this format. Cannot proceed with this format!" ,;
                     "\!\<Browse Scales;\<Cancel") = 1
      llNothing = lfFABrSizs()
    ENDIF
    RETURN .F.
  ENDIF    
ENDSCAN
llNothing = lfFAClrThrm (lnTotRecs,lnCurRec,"Compiling Data","")
llNothing = lfFATotRep()
llNothing = lfFATotClr()

*:*************************************************************
*: FUNC : lfFATotClr
*: DESC : Get the colors totals.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFATotClr

lcFabric  = SPACE(7)
SELECT (lcFAClrTot)
GOTO TOP
DO WHILE .T.
  DO WHILE &lcFAClrTot..cFabric = lcFabric AND !EOF()
    SKIP
  ENDDO
  IF EOF()
    EXIT
  ELSE
    lcFabric = cFabric
    lcExp    = cFabric+cType+cColor
    REPLACE ALL nTotAprox WITH (nTotYdsOrd-nTotYdsNed)/(nTotYdsNed/nTotPcs);
            FOR cFabric = lcFabric
    SUM ALL nTotAprox          ;
        FOR cFabric = lcFabric ;
        TO  lnTotAprox
    APPEND BLANK
    REPLACE cFabric   WITH lcFabric ,;
            cColor    WITH SPACE(6) ,;
            cType     WITH "A"      ,;
            nTotAprox WITH lnTotAprox
    = SEEK(lcExp)        
  ENDIF
ENDDO

*:*************************************************************
*: FUNC : lfFATotRep
*: DESC : Get the reports totals.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFATotRep

lcFabric  = SPACE(7)
SELECT (lcTmpFormA)
GOTO TOP
DO WHILE .T.
  DO WHILE &lcTmpFormA..cFabric = lcFabric AND !EOF()
    SKIP
  ENDDO
  IF EOF()
    EXIT
  ELSE
    lcFabric = &lcTmpFormA..cFabric
    lcExp    = cFabric+SUBSTR(cStyle,1,lnMajorLen)+cType+SUBSTR(cStyle,lnNonMajSt,lnColorLen)+STR(RECNO(),6)
    SUM ALL nTotPcs,nTotSals,nSiz01,nSiz02,nSiz03,nSiz04,nSiz05,;
            nSiz06,nSiz07,nSiz08,nSiz09,nSiz10,nSiz11,nSiz12,nSiz13,;
            nSiz14;
        FOR cType+cFabric = "A"+lcFabric ;
        TO  lnTotStPcs,lnTotSals,lnTotS01,lnTotS02,lnTotS03,lnTotS04,;
            lnTotS05,lnTotS06,lnTotS07,lnTotS08,lnTotS09,lnTotS10,;
            lnTotS11,lnTotS12,lnTotS13,lnTotS14
  
    REPLACE ALL FOR cType+cFabric = "A"+lcFabric;
            nPerSent WITH (nTotPcs/lnTotStPcs)*100
    APPEND BLANK
    REPLACE cType    WITH "C"            ,;
            cFabric  WITH lcFabric       ,;
            cStyle   WITH "~~~~~~~~~~~~-~~~~~~" ,;
            nSiz01   WITH lnTotS01       ,;
            nSiz02   WITH lnTotS02       ,;
            nSiz03   WITH lnTotS03       ,;
            nSiz04   WITH lnTotS04       ,;
            nSiz05   WITH lnTotS05       ,;
            nSiz06   WITH lnTotS06       ,;
            nSiz07   WITH lnTotS07       ,;
            nSiz08   WITH lnTotS08       ,;
            nSiz09   WITH lnTotS09       ,;
            nSiz10   WITH lnTotS10       ,;
            nSiz11   WITH lnTotS11       ,;
            nSiz12   WITH lnTotS12       ,;
            nSiz13   WITH lnTotS13       ,;
            nSiz14   WITH lnTotS14       ,;
            nTotPcs  WITH lnTotStPcs     ,;
            nTotSals WITH lnTotSals
    = SEEK(lcExp)
  ENDIF
ENDDO

*:***********************************************************************
*: FUNC : lfFAGtSize
*: DESC : This function will update the global array "laSizes" that holds
*:        all the used sizes description with any new used size, and it will
*:        build a string that holds the field name that should be used to save this 
*:        new size in the temp file according to the size order in the array. It will return false
*:        if the number of used sizes exceeds the maximum number that could be
*:        number that could be printed in the form (14).
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFAGtSize
PRIVATE lnSizeNo, lnNoOfSizs

lnSizeNo    = ASCAN(laSizes, &lcRepTemp..SizDesc)
lnNoOfSizs  = ALEN(laSizes)
IF lnSizeNo = 0
  llFrstTime = (lnNoOfSizs = 1 AND EMPTY(laSizes[1]))
  DIMENSION laSizes[IIF(llFrstTime,1,lnNoOfSizs+1)]  
  STORE ALEN(laSizes) TO lnNoOfSizs, lnSizeNo
  laSizes[lnNoOfSizs] = &lcRepTemp..SizDesc
ENDIF
lcSizeFld = "nSiz" + IIF(lnSizeNo<10,"0","") + ALLTRIM(STR(lnSizeNo))

RETURN (lnNoOfSizs <= lnMaxSize)

*:*************************************************************
*: Program file  : lfFANewRec
*: Program desc. : Function to update (Create or accumulate records 
*:                 in) the temp file (lcTmpFormA) with the converted 
*:                 records from the temp file (lcRepTemp). 
*:                 There are two types of records that 
*:                 could be saved through this function, these
*:                 types are : Type "A" which means that this 
*:                 record will hold the total numbers for a specific 
*:                 Fabric\Style and Type "B" which means that this 
*:                 record holds one of the details lines for that 
*:                 Fabric\Style. These types will be passed as 
*:                 a parameter to this function.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFANewRec
PARAMETERS lcRecType

lcColor = IIF(lcRecType = "A",SPACE(6), SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen))
SELECT (lcTmpFormA)
IF !SEEK(&lcRepTemp..Fabric+SUBSTR(&lcRepTemp..Style,1,lnMajorLen)+lcRecType+lcColor)
  APPEND BLANK
  *-- We will update the color(after the style) if the form <> 'A'.
  lcSty = IIF(lcRecType <> "A",&lcRepTemp..Style,SUBSTR(&lcRepTemp..Style,1,lnMajorLen))
  REPLACE cType   WITH lcRecType       ,;
          cFabric WITH &lcRepTemp..Fabric ,;
          cStyle  WITH lcSty
  IF lcRecType = "A"
    REPLACE cDesc     WITH &lcRepTemp..StyDesc   ,;
            nAveCones WITH &lcRepTemp..Ave_Cones ,;
            nPrice    WITH &lcRepTemp..Price     ,;
            nYDS      WITH &lcRepTemp..YDS
  ENDIF  
ENDIF
REPLACE &lcSizeFld  WITH &lcSizeFld + &lcRepTemp..Qty ,;
        nTotPcs     WITH nTotPcs    + &lcRepTemp..Qty ,;
        nTotSals    WITH nTotSals   + &lcRepTemp..Amount

*:*************************************************************
*: Program file  : lfFAAddClr
*: Program desc. : Function to update (Create or accumulate records 
*:                 in) the temp file (lcFAClrTot) with the converted 
*:                 records from the temp file (lcRepTemp). 
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFAAddClr
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcFAClrTot)
IF !SEEK(&lcRepTemp..Fabric+"B"+SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen))
  APPEND BLANK
  REPLACE cFabric    WITH &lcRepTemp..Fabric ,;
          cColor     WITH SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen)  ,;
          cType      WITH "B"             ,;
          nTotYdsOrd WITH IIF(SEEK(&lcRepTemp..Fabric+SUBSTR(&lcRepTemp..Style,lnNonMajSt,lnColorLen),"Fabric"),;
                              Fabric.OnHand+Fabric.OnOrder,0)
ENDIF
REPLACE nTotPcs    WITH nTotPcs    + &lcRepTemp..Qty ,;
        nTotYdsNed WITH nTotYdsNed + &lcRepTemp..Qty * &lcRepTemp..Ave_Cones
SELECT (lnAlias)

*:*************************************************************
*: Program file  : lfTherm
*: Program desc. : To display a thermometer for any running action.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Example : llNothing  = lfTherm(100,50,"Update","")
*:*************************************************************
FUNCTION lfTherm
PARAMETERS lnToBeDone,lnDone,lcMessage,lcVariable

lcMessage   = IIF(TYPE('lcMessage')  = 'C', lcMessage , " ")
lcVariable  = IIF(TYPE('lcVariable') = 'C', lcVariable, " ")
lcCurDev    = SET("DEVICE")
SET DEVICE TO SCREEN
gcFontFace	=	"MS Sans Serif"
gcFontSize	=	8.000
gcFontStyle	=	"B"
gntherwidth =  57
IF NOT WEXIST("gwdThermo")
  DEFINE WINDOW gwdThermo ;
   AT  INT((SROW() - (( 5.615 * ;
   FONTMETRIC(1, gcFontFace, gcFontSize, gcFontStyle )) / ;
   FONTMETRIC(1, WFONT(1,""), WFONT( 2,""), WFONT(3,"")))) / 2), ;
   INT((SCOL() - (( 63.833 * ;
   FONTMETRIC(6, gcFontFace, gcFontSize, gcFontStyle )) / ;
   FONTMETRIC(6, WFONT(1,""), WFONT( 2,""), WFONT(3,"")))) / 2) ;
   SIZE 5.615,63.833 ;
   FONT gcFontFace, gcFontSize ;
   STYLE gcFontStyle ;
   NOFLOAT ;
   NOCLOSE ;
   NONE ;
   COLOR RGB(0, 0, 0, 192, 192, 192)
   MOVE WINDOW gwdThermo CENTER
   ACTIVATE WINDOW gwdThermo NOSHOW

  @ 0.5,3 SAY lcMessage FONT gcFontFace, gcFontSize STYLE gcFontStyle
  @ 0.000,0.000 TO 0.000,63.833 ;
    COLOR RGB(255, 255, 255, 255, 255, 255)
  @ 0.000,0.000 TO 5.615,0.000 ;
    COLOR RGB(255, 255, 255, 255, 255, 255)
  @ 0.385,0.667 TO 5.231,0.667 ;
    COLOR RGB(128, 128, 128, 128, 128, 128)
  @ 0.308,0.667 TO 0.308,63.167 ;
    COLOR RGB(128, 128, 128, 128, 128, 128)
  @ 0.385,63.000 TO 5.308,63.000 ;
    COLOR RGB(255, 255, 255, 255, 255, 255)
  @ 5.231,0.667 TO 5.231,63.167 ;
    COLOR RGB(255, 255, 255, 255, 255, 255)
  @ 5.538,0.000 TO 5.538,63.833 ;
    COLOR RGB(128, 128, 128, 128, 128, 128)
  @ 0.000,63.667 TO 5.615,63.667 ;
    COLOR RGB(128, 128, 128, 128, 128, 128)
  @ 3.000,3.333 TO 4.300,3.333 ;
    COLOR RGB(128, 128, 128, 128, 128, 128)
  @ 3.000,60.333 TO 4.308,60.333 ;
    COLOR RGB(255, 255, 255, 255, 255, 255)
  @ 3.000,3.333 TO 3.000,60.333 ;
   COLOR RGB(128, 128, 128, 128, 128, 128)
  @ 4.300,3.333 TO 4.300,60.500 ;
    COLOR RGB(255, 255, 255, 255, 255, 255)
  SHOW WINDOW gwdThermo TOP
ENDIF
lnPersent = INT((lnDone /lnToBeDone )*100)
lnblocks  = lnPersent*gntherwidth/100
@ 1.7,3 SAY lcVariable+SPACE(gntherwidth-LEN(lcVariable)) FONT gcFontFace, gcFontSize STYLE gcFontStyle
@ 3.039,gntherwidth/2 SAY ALLTRIM(STR(lnPersent))+"%" FONT "SYSTEM",8 ;
COLOR RGB(0,0,0,192, 192, 192)
@ 3.000,3.333 FILL TO 4.300,lnblocks + 3.333 ;
  COLOR RGB(255,255,255,128,128,128)
IF lnToBeDone <= lnDone
  RELEASE WINDOW gwdThermo
ENDIF
SET DEVICE TO &lcCurDev

*:*************************************************************
*: Program file  : lfFABrSizs
*: Program desc. : To display a list with all the styles selected
*:                 to be displaied in the report with their size
*:                 scales.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFABrSizs

lcGrVerLn = "ณ"
lcTmpBrow = gfTempName()
CREATE CURSOR (lcTmpBrow) ;
              (cStyle C(lnMajorLen), cSize01 C(5), cSize02 C(5), cSize03 C(5),;
               cSize04 C(5), cSize05 C(5), cSize06 C(5), cSize07 C(5),;
               cSize08 C(5))
              
INDEX ON cStyle TAG (lcTmpFormA)
SELECT (lcRepTemp)
SCAN
  lcSizFld = "cSize0" + ALLTRIM(&lcRepTemp..Size)
  SELECT (lcTmpBrow)
  IF !SEEK(SUBSTR(&lcRepTemp..Style,1,lnMajorLen))
    APPEND BLANK
    REPLACE cStyle WITH SUBSTR(&lcRepTemp..Style,1,lnMajorLen)
  ENDIF
  REPLACE &lcSizFld WITH &lcRepTemp..SizDesc
ENDSCAN
lcFields  = "PADR(ALLTRIM(&lcTmpBrow..cStyle ),12)+lcGrVerLn+"+;
            "PADR(ALLTRIM(&lcTmpBrow..cSize01),05)+lcGrVerLn+"+;
            "PADR(ALLTRIM(&lcTmpBrow..cSize02),05)+lcGrVerLn+"+; 
            "PADR(ALLTRIM(&lcTmpBrow..cSize03),05)+lcGrVerLn+"+;
            "PADR(ALLTRIM(&lcTmpBrow..cSize04),05)+lcGrVerLn+"+; 
            "PADR(ALLTRIM(&lcTmpBrow..cSize05),05)+lcGrVerLn+"+; 
            "PADR(ALLTRIM(&lcTmpBrow..cSize06),05)+lcGrVerLn+"+; 
            "PADR(ALLTRIM(&lcTmpBrow..cSize07),05)+lcGrVerLn+"+;
            "PADR(ALLTRIM(&lcTmpBrow..cSize08),05)"
GOTO TOP IN (lcTmpBrow)
llNothing = lfSizeBrow()
USE IN (lcTmpBrow)

*:*************************************************************
*: Program file  : lfFAPrint
*: Program desc. : This function contains the main printing 
*:                 loop and the declaration needed for the 
*:                 printing process.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFAPrint

lnMaxCPL  = IIF (lcRpPaTyp = "T", 174, 224)
lnPageNo  = 0
lcRepDate = "DATE : " + DTOC(DATE())
lcRepTime = "TIME : " + TIME()
lcCUserID = IIF(!EMPTY(gcUser_Id),"USER : "+gcUser_Id,"")
lcRpTitle = ALLTRIM(lcRpTitle)
lcGrVerLn = "ณ"
lcGrHorLn = "ฤ"
lcGrLin02 = "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
lcGrLin03 = "ณ SALES             : "
lcGrLin04 = "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
lcGrLin05 = "ณ PRODUCING APPROX. : "
lcGrLin06 = "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
lcGrFLn09 = "ฺฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤยฤฤฤฤฤย"
lcGrSLn09 = "ยฤฤฤฤฤฤฤยฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
lcGrFLn10 = "ณ"+lcMajTitle+"       ณDESCRIPTION         ณCNSMPTณWHLSLณ"
lcGrSLn10 = "ณTOT PCSณ%  BYณ     SALES BYณ"
lcGrFLn11 = "ณ            ณ                    ณ  UNITณ $PERณ"
lcGrSLn11 = "ณSTY&CLRณ"+lcMajTitle+"ณ        "+lcMajTitle+"ณ"
lcGrFLn12 = "ฦออออออออออออุออออออออออออออออออออุออออออุอออออุ"
lcGrSLn12 = "ุอออออออุอออออุอออออออออออออต"
lcGrStSep = "รฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤลฤฤฤฤฤล"+lfFAVarHdr(5)+"ลฤฤฤฤฤฤฤลฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤด"
lcGrStEnd = "ภฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤมฤฤฤฤฤม"+lfFAVarHdr(6)+"มฤฤฤฤฤฤฤมฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤู"
lcGrFbSep = "ฦออออออออออออฯออออออออออออออออออออฯออออออฯอออออุ"+lfFAVarHdr(4)+"ุอออออออุอออออุอออออออออออออต"
lcGrFbEnd = "ิออออออออออออออออออออออออออออออออออออออออออออออฯ"+lfFAVarHdr(7)+"ฯอออออออฯอออออฯอออออออออออออพ"
lnHdrWid  = LEN(SPACE(02)+lcRepMsg1+SPACE(36)+lcGrLin02+SPACE(50)+lcRepDate+SPACE(02))
lnHSCol   = INT((lnMaxCPL-lnHdrWid)/2)
lnReptWid = LEN(lcGrFLn09) + (ALEN(laSizes)*7)-1 + LEN(lcGrSLn09)
lnSCol    = INT((lnMaxCPL-lnReptWid)/2)
lcGrRpEnd = "ภ"+ REPLICATE(lcGrHorLn,lnReptWid-2)+ "ู"
lnRow     = 0
lnTotSals = 0
lnMaxPrt  = 40
lnStrtPrt = 0
lnTotRecs = RECCOUNT(lcTmpFormA)
lnCurRec  = 0
lnAproxSl = 0

CLEAR TYPEAHEAD
WAIT "Printing format A..press <SPACE BAR> to abort" WINDOW NOWAIT
SELECT (lcTmpFormA)
GOTO TOP
DO WHILE !EOF() AND INKEY() <> 32
  lcFabric  = &lcTmpFormA..cFabric
  lcFabDesc = IIF(SEEK(lcFabric,"Fabric"), Fabric.Desc, SPACE(30)) 
  lcExp     = cFabric+SUBSTR(cStyle,1,lnMajorLen)+cType+SUBSTR(cStyle,lnNonMajSt,lnColorLen)+STR(RECNO(),6)
  llNothing = SEEK(lcFabric+REPLICATE("~",12)+"C")
  lnTotSals = &lcTmpFormA..nTotSals
  lnTotPLS  = IIF(SEEK (lcFabric+"A",lcFAClrTot), &lcFAClrTot..nTotAprox, 0)
  lnAproxSl = (nTotSals/nTotPcs) * lnTotPLS
  lnProduc  = lnTotSals+lnAproxSl
  llNothing = SEEK(lcExp)
  llNothing = lfFAprtHdr(.T.)
  llNothing = lfFAPrtDet()
  llNothing = lfFAClrTot()
ENDDO
WAIT CLEAR
llNothing = lfFAClrThrm (lnTotRecs,lnCurRec,"Printing","")

*:*************************************************************
*: Program file  : lfFAprtHdr
*: Program desc. : To print the format's header, either with
*:                 the tabel's header or without it.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Passed Parameters  : llPrtStHdr : Print the tabel's header
*:                                   or not.
*:*************************************************************
FUNCTION lfFAprtHdr
PARAMETERS llPrtStHdr
PRIVATE lnHRow

lnHRow   = 0
lnPageNo = lnPageNo + 1
lcPageNo = "PAGE : " + ALLTRIM(STR(lnPageNo))

@ lnHRow+01,lnHSCol+000 SAY SPACE(77)+"PRODUCTION REPORT"
IF !EMPTY(lcRpTitle)
  @ lnHRow+02,(lnMaxCPL-LEN(lcRpTitle))/2 SAY lcRpTitle
  lnHRow = 1
ENDIF  
@ lnHRow+02,lnHSCol+000 SAY SPACE(02)+lcRepMsg1+SPACE(36)+lcGrLin02+SPACE(50)+lcRepDate
@ lnHRow+03,lnHSCol+000 SAY SPACE(02)+lcRepMsg2+SPACE(36)+lcGrLin03
@ lnHRow+03,lnHSCol+091 SAY lnTotSals PICTURE "$$,$$$,$$$.99"
@ lnHRow+03,lnHSCol+105 SAY lcGrVerLn+SPACE(50)+lcRepTime
@ lnHRow+04,lnHSCol+000 SAY SPACE(02)+lcRepMsg3+SPACE(36)+lcGrLin04+SPACE(50)+lcPageNo
@ lnHRow+05,lnHSCol+000 SAY SPACE(02)+lcRepMsg4+SPACE(36)+lcGrLin05
@ lnHRow+05,lnHSCol+091 SAY lnProduc PICTURE "$$,$$$,$$$.99"
@ lnHRow+05,lnHSCol+105 SAY lcGrVerLn+SPACE(50)+lcCUserID
@ lnHRow+06,lnHSCol+000 SAY SPACE(02)+lcRepMsg5+SPACE(36)+lcGrLin06
@ lnHRow+07,lnHSCol+000 SAY SPACE(02)+lcRepMsg6
@ lnHRow+08,lnHSCol+000 SAY SPACE(02)+"FABRIC : "+ALLTRIM(lcFabric)+SPACE(1)+lcFabDesc
IF llPrtStHdr
  @ lnHRow+09,lnSCol+000 SAY lcGrFLn09+lfFAVarHdr(1)+lcGrSLn09
  @ lnHRow+10,lnSCol+000 SAY lcGrFLn10+lfFAVarHdr(2)+lcGrSLn10
  @ lnHRow+11,lnSCol+000 SAY lcGrFLn11+lfFAVarHdr(3)+lcGrSLn11
  @ lnHRow+12,lnSCol+000 SAY lcGrFLn12+lfFAVarHdr(4)+lcGrSLn12
  lnStrtPrt = lnHRow + 13
ELSE
  lnStrtPrt = lnHRow + 9
ENDIF  
lnRow = lnStrtPrt

*:*************************************************************
*: Program file  : lfFAVarHdr
*: Program desc. : To build a string of a contenuas line with 
*:                 a number of seperators according to the number
*:                 of sizes printed in the format.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Passed Parameters  : lnType : The type of line needed.
*:*************************************************************
*: Example : lcStrToPrt = lfFAVarHdr(5)
*:*************************************************************
FUNCTION lfFAVarHdr
PARAMETERS lnType
PRIVATE lnNoOfSizs, lnI, lcRetStr

lcRetStr   = SPACE(0)
lnNoOfSizs = ALEN(laSizes)
FOR lnI = 1 TO lnNoOfSizs
  DO CASE
    CASE lnType = 1
      lcConnect = REPLICATE(lcGrHorLn,6)
      lcTermnat = "ย"
    CASE lnType = 2
      lcConnect = SPACE(6)
      lcTermnat = lcGrVerLn
    CASE lnType = 3
      lcConnect = PADL(ALLTRIM(laSizes[lnI]),6)
      lcTermnat = lcGrVerLn
    CASE lnType = 4
      lcConnect = REPLICATE("อ",6)
      lcTermnat = "ุ"
    CASE lnType = 5
      lcConnect = REPLICATE(lcGrHorLn,6)
      lcTermnat = "ล"
    CASE lnType = 6
      lcConnect = REPLICATE(lcGrHorLn,6)
      lcTermnat = "ม"
    CASE lnType = 7
      lcConnect = REPLICATE("อ",6)
      lcTermnat = "ฯ"
  ENDCASE
  lcRetStr = lcRetStr + lcConnect
  IF lnI <> lnNoOfSizs
    lcRetStr = lcRetStr + lcTermnat
  ENDIF
ENDFOR

RETURN (lcRetStr)

*:*************************************************************
*: Program file  : lfFAPrtDet
*: Program desc. : To print the format's details part.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Calls : 
*:         Procedures : None
*:         Functions  : lfTherm()
*:                      lfFAprtHdr()
*:*************************************************************
FUNCTION lfFAPrtDet

SELECT (lcTmpFormA)
lcStyle   = SPACE(lnMajorLen)
lnOldSCol = lnSCol
SCAN WHILE lcFabric = &lcTmpFormA..cFabric
  lnCurRec = lnCurRec + 1
  IF lcStyle <> SUBSTR(cStyle,1,lnMajorLen) AND cType <> "C"
    lcStyle = SUBSTR(cStyle,1,lnMajorLen)
    lcExp   = cFabric+SUBSTR(cStyle,1,lnMajorLen)+cType+SUBSTR(cStyle,lnNonMajSt,lnColorLen)+STR(RECNO(),6)
    COUNT REST WHILE cFabric+SUBSTR(cStyle,1,lnMajorLen) = lcFabric+lcStyle TO lnStyRows
    llLastSty = !(cType = "A")
    lnStyRows = lnStyRows + IIF(llLastSty, 3, 1)
    llNothing = SEEK(lcExp)
    IF lnMaxPrt < lnRow+lnStyRows  
      @ lnRow,lnSCol+000 SAY lcGrStEnd
      llNothing = lfFAprtHdr(.T.)
    ELSE
      IF lnRow <> lnStrtPrt
        @ lnRow,lnSCol+000 SAY lcGrStSep
        lnRow = lnRow + 1
      ENDIF
    ENDIF
  ENDIF
  lcPrompt = IIF(cType = "C",;
                "Fabric : " + ALLTRIM(cFabric),;
                "Fabric\"+lcMajTitle+"\"+lcNonMajTl+":" +;
                  ALLTRIM(cFabric)+"\"+ALLTRIM(SUBSTR(cStyle,1,lnMajorLen))+"\"+ALLTRIM(SUBSTR(cStyle,lnNonMajSt,lnColorLen)))
  llNothing = lfTherm (lnTotRecs,lnCurRec,"Printing",lcPrompt)
  DO CASE
    CASE cType = "A"
      @ lnRow,lnSCol+000 SAY lcGrVerLn
      @ lnRow,lnSCol+001 SAY SUBSTR(cStyle,1,lnMajorLen)+lcGrVerLn+cDesc+lcGrVerLn
      @ lnRow,lnSCol+035 SAY nAveCones PICTURE "999.99"
      @ lnRow,lnSCol+041 SAY lcGrVerLn
      @ lnRow,lnSCol+042 SAY nPrice    PICTURE "999.9"
      @ lnRow,lnSCol+047 SAY lcGrVerLn
      @ lnRow,lnSCol+048 SAY lfFAPrtQty(6)
      @ lnRow,lnSCol+145 SAY lcGrVerLn
      @ lnRow,lnSCol+146 SAY nTotPcs   PICTURE "9999999"
      @ lnRow,lnSCol+153 SAY lcGrVerLn
      @ lnRow,lnSCol+155 SAY nPerSent  PICTURE "999%"
      @ lnRow,lnSCol+159 SAY lcGrVerLn
      @ lnRow,lnSCol+160 SAY nTotSals  PICTURE "$$,$$$,$$$.99"
      @ lnRow,lnSCol+173 SAY lcGrVerLn
    CASE cType = "B"
      @ lnRow,lnSCol+000 SAY lcGrVerLn+SPACE(12)+lcGrVerLn+PADR(SUBSTR(cStyle,lnNonMajSt,lnColorLen),6)+SPACE(14)+lcGrVerLn
      @ lnRow,lnSCol+035 SAY SPACE(6)+lcGrVerLn+SPACE(5)+lcGrVerLn
      @ lnRow,lnSCol+048 SAY lfFAPrtQty(5)
      @ lnRow,lnSCol+145 SAY lcGrVerLn
      @ lnRow,lnSCol+146 SAY nTotPcs   PICTURE "9999999"
      @ lnRow,lnSCol+153 SAY lcGrVerLn+SPACE(5)+lcGrVerLn+SPACE(13)+lcGrVerLn
    CASE cType = "C"
      @ lnRow,lnSCol+000 SAY lcGrFbSep
      lnRow  = lnRow + 1
      @ lnRow,lnSCol+000 SAY lcGrVerLn
      @ lnRow,lnSCol+001 SAY "TOTAL PIECES"+SPACE(34)+lcGrVerLn
      @ lnRow,lnSCol+048 SAY lfFAPrtQty(6)
      @ lnRow,lnSCol+145 SAY lcGrVerLn
      @ lnRow,lnSCol+146 SAY nTotPcs   PICTURE "9999999"
      @ lnRow,lnSCol+153 SAY lcGrVerLn+SPACE(5)+lcGrVerLn
      @ lnRow,lnSCol+160 SAY nTotSals  PICTURE "$$,$$$,$$$.99"
      @ lnRow,lnSCol+173 SAY lcGrVerLn
      lnRow  = lnRow + 1
      lnSCol = lnOldSCol 
      @ lnRow,lnSCol+000 SAY lcGrFbEnd
  ENDCASE
  lnRow  = lnRow + 1
  lnSCol = lnOldSCol 
ENDSCAN  

*:*************************************************************
*: Program file  : lfFAPrtQty
*: Program desc. : To print the quantities printed in the detail 
*:                 part in the format.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Passed Parameters  : lnPicture : How are we going to display 
*:                                  these quantities ?
*:*************************************************************
*: Example : llNothing  = lfFAPrtQty(5)
*:*************************************************************
FUNCTION lfFAPrtQty
PARAMETERS lnPicture
PRIVATE lcRetStr, lnNoOfSizs

lcRetStr   = SPACE(0)
lnNoOfSizs = ALEN(laSizes)

FOR lnI = 1 TO lnNoOfSizs
  lcField  = "nSiz"+IIF(lnI<10,"0","")+ALLTRIM(STR(lnI))
  lcRetStr = lcRetStr+IIF(&lcField<>0,PADL(STR(&lcField,lnPicture),6),SPACE(6)) 
  IF lnI <> lnNoOfSizs
    lcRetStr = lcRetStr + lcGrVerLn
  ENDIF
ENDFOR
lnSCol = lnSCol-((lnMaxSize-lnNoOfSizs)*7)

RETURN (lcRetStr)

*:*************************************************************
*: Program file  : lfFAClrTot
*: Program desc. : To print the colors totals at the  end of the
*:                 format.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfFAClrTot
PRIVATE lnAlias

lnAlias    = SELECT()
lnNoOfSizs = ALEN(laSizes)
lnSpaces   = 15 + ((lnNoOfSizs-1)*7) + 28
lcGrLn251  = "ฺฤฤฤฤฤฤยฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤย"
lcGrLn261  = "ณ"+lcNonMajTl+"ณTOT PCSณYDS NEEDEDณYDS ORDEREDณAPPROX PCS LTS"
lcGrLn271  = "ณ      ณ       ณ          ณ           ณ"
lcGrLn281  = "ฦออออออุอออออออุออออออออออุอออออออออออุ"
lcGrEnd01  = "ภฤฤฤฤฤฤมฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤม"
lcGrLn252  = REPLICATE("ฤ",lnSpaces)+"ฟ"
lcGrLn282  = REPLICATE("อ",lnSpaces)+"ต"
lcGrEnd02  = REPLICATE("ฤ",lnSpaces)+"ู"
llPrtHdr   = .T.
llNewPage  = (lnMaxPrt<lnRow+6)
lcTotPLS   = PADL(ALLTRIM(STR(lnTotPLS)),14)
lnVCol     = lnSCol-((lnMaxSize-lnNoOfSizs)*7)
llPrtEnd   = .F.

SELECT (lcFAClrTot)
IF SEEK (lcFabric+"B")
  SCAN REST WHILE cFabric = lcFabric
    IF llPrtHdr
      IF llNewPage
        IF llPrtEnd 
          @ lnRow,lnSCol+000 SAY lcGrEnd01 + lcGrEnd02
        ENDIF  
        llNothing = lfFAprtHdr(.F.)
        llNewPage = .F.
      ENDIF
      @ lnRow+00,lnSCol+000 SAY lcGrLn251+lcGrLn252
      @ lnRow+01,lnSCol+000 SAY lcGrLn261
      @ lnRow+01,lnVCol+160 SAY lnAproxSl PICTURE "$$,$$$,$$$.99"
      @ lnRow+01,lnVCol+173 SAY lcGrVerLn
      @ lnRow+02,lnSCol+000 SAY lcGrLn271+lcTotPLS
      @ lnRow+02,lnVCol+173 SAY lcGrVerLn
      @ lnRow+03,lnSCol+000 SAY lcGrLn281+lcGrLn282
      lnRow    = lnRow + 4
      llPrtHdr = .F.
    ENDIF
    lcDetLine = lcGrVerLn + PADR(cColor,6)        + lcGrVerLn +;
                PADL(ALLTRIM(STR(nTotPcs)),7)     + lcGrVerLn +;
                PADL(ALLTRIM(STR(nTotYdsNed)),10) + lcGrVerLn +;
                PADL(ALLTRIM(STR(nTotYdsOrd)),11) + lcGrVerLn +;
                PADL(ALLTRIM(STR(nTotAprox)),14)
    @ lnRow,lnSCol+000 SAY lcDetLine
    @ lnRow,lnVCol+173 SAY lcGrVerLn
    lnRow = lnRow + 1
    IF lnMaxPrt = lnRow
      STORE .T. TO llNewPage, llPrtHdr
      llPrtEnd = .F.
      @ lnRow,lnSCol+000 SAY lcGrEnd01 + lcGrEnd02
    ELSE
      llPrtEnd = .T.
    ENDIF  
  ENDSCAN
  IF llPrtEnd 
    @ lnRow,lnSCol+000 SAY lcGrEnd01 + lcGrEnd02
  ENDIF  
ENDIF
SELECT(lnAlias)

*:*************************************************************
*: Program file  : lfFAClrThrm
*: Program desc. : To clear the therm window if needed to.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
*: Passed Parameters  : lnTot   : Total number for the process.
*:                      lnCur   : Current updated number.
*:                      lcTitle : Main title of the process.
*:                      lcProm  : Current updated title.
*:*************************************************************
*: Example : llNothing  = lfFAClrThrm(10,2,"","")
*:*************************************************************
FUNCTION lfFAClrThrm
PARAMETERS lnTot, lnCur, lcTitle, lcProm
PRIVATE lnI

IF lnCur < lnTot
  FOR lnI = lnCur TO lnTot
    llNothiing = lfTherm (lnTot,lnI,lcTitle,lcProm)
  ENDFOR
ENDIF  

*:*************************************************************
*: Program file  : lfSizeBrow
*: Program desc. : To display the used styles and sizes list 
*:                 in a window on the screen.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 03/01/99
*:*************************************************************
FUNCTION lfSizeBrow

lcGrVerLin = "ณ"
lcMid      = REPLICATE ("ยฤฤฤฤฤ",8)
lcTopLine  = "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤ" + lcMid + "ฤฟ"
lcBotLine  = ""
lcTitle    = lcGrVerLin+" "+lcMajTitle+SPACE(7)+;
             lcGrVerLin+"SIZ 1"+lcGrVerLin+"SIZ 2"+;
             lcGrVerLin+"SIZ 3"+lcGrVerLin+"SIZ 4"+;
             lcGrVerLin+"SIZ 5"+lcGrVerLin+"SIZ 6"+;
             lcGrVerLin+"SIZ 7"+lcGrVerLin+"SIZ 8"+;
             SPACE(1)+lcGrVerLin
DEFINE WINDOW lwsizbrow                                     ;
         FROM  INT((SROW()-11)/2),   INT((SCOL()-68)/2)     ;
         TO    INT((SROW()-11)/2)+9, INT((SCOL()-68)/2)+67  ;
         FLOAT NOCLOSE SHADOW NOMINIMIZE DOUBLE COLOR SCHEME 5
DEFINE POPUP lcSizPopUp PROMPT FIELD &lcFields SCROLL MARGIN MARK ""
ACTIVATE WINDOW lwsizbrow NOSHOW
@ 00,02 SAY "The sizes used in the selected range of styles :"
@ 01,01 SAY lcTopLine
@ 02,01 SAY lcTitle
@ 03,01 GET lsSizeBrow PICTURE "@&N" POPUP lcSizPopUp SIZE 4,64 ;
            DEFAULT " " COLOR SCHEME 5
@ 07,28 GET pbDone PICTURE "@*HT \!\?\<Done" SIZE 1,10,1 DEFAULT 1
ACTIVATE WINDOW lwsizbrow     
READ CYCLE MODAL
RELEASE WINDOW lwsizbrow
RELEASE POPUPS lcSizPopUp

*!**************************************************************
*! PROG : lpDetail
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! DESC : Print report Details.
*!**************************************************************
PROCEDURE lpDetail

IF ( Size<>lcSize .OR. SUBSTR(Style,1,lnMajorLen)<>lcStyle )
  lnStyTot=lnStyTot+lnSizTot
  IF ! EMPTY(lcStyle)
    @ PROW(),PCOL() SAY SUBSTR(lcLn1,PCOL()+1,lnPrtCol+2-PCOL())
    @ PROW(),lnPrtCol+2 SAY STR(lnSizTot,8)+'ณ'
    IF SUBSTR(Style,1,lnMajorLen)=lcStyle
      @ PROW(),lnPrtCol+11 SAY SUBSTR(lcLN1,lnPrtCol+12)
    ENDIF
  ENDIF
  IF SUBSTR(Style,1,lnMajorLen)=lcStyle
    @ $+1   ,00 SAY LEFT(lcLN1  ,13)
    @ PROW(),14 SAY SizDesc
    @ PROW(),20 SAY SUBSTR(lcLn1,21,35)  
  ELSE
    IF !EMPTY(lcStyle)
      @ PRow(),lnPrtCol+11 SAY STR(lnStyTot,9)+'ณ'
      @ PRow(),lnPrtCol+25 SAY (lnStyTot/lnSaleQty)*100 PICTURE '999%'
      @ PRow(),lnPrtCol+29 SAY 'ณ'
      @ PRow(),lnPrtCol+30 SAY (lnStyTot*lnPrice) PICTURE '$$,$$$,$$$.99'
      @ PRow(),lnPrtCol+43 SAY 'ณ'
      @ PROW(),lnPrtCol+44 SAY (lnStyTot*lnAve) PICTURE '9999999.99'
      @ PRow(),lnPrtCol+54 SAY 'ณ'
      @ $+1,00 SAY lcLN2  
    ENDIF
  ENDIF
  lnSizTot=0
  lcSize = Size
ENDIF

IF SUBSTR(Style,1,lnMajorLen)<>lcStyle
  @ PRow()+1,00   SAY 'ณ '+SUBSTR(Style,1,lnMajorLen)
  @ PRow(),14   SAY SizDesc+'ณ' 
  @ PRow(),13+8 SAY StyDesc+'ณ'
  @ PRow(),34+8 SAY Ave_Cones PICTURE '999.99'+'ณ'
  @ PRow(),41+8 SAY Price    PICTURE '999.9'+'ณ'
  lnPrice=Price
  lnAve  =Ave_Cones 
  lcStyle=SUBSTR(Style,1,lnMajorLen)
  lnStyTot=0
ENDIF
lcColor=SUBSTR(Style,lnNonMajSt,lnColorLen)
lnCol=IIF(lcRpForm = 'A',40,33)+8
FOR J=1 TO IIF(lcRpForm = 'A',10,IIF(lcRpPaTyp='L',5,3))
  lnCol=lnCol+IIF(lcRpForm = 'A',7,21)
  IF lcColor=laClr(J) .OR. J=IIF(lcRpForm = 'A',10,IIF(lcRpPaTyp='L',5,3))
    EXIT
  ENDIF
ENDFOR
IF J=IIF(lcRpForm = 'A',10,IIF(lcRpPaTyp='L',5,3))
  lnQty=0
  lnCut=0
  DO WHILE Fabric+SUBSTR(Style,1,lnMajorLen)+Size=lcFab+lcStyle+lcSize
    lnQty=lnQty+Qty
    lnCut=lnCut+Cut
    SKIP
  ENDDO
  SKIP -1
ELSE
  lnCut=Cut
  lnQty=Qty
ENDIF
@ PROW(),PCOL() SAY SUBSTR(lcLn1,PCOL()+1,lnCol-(7+PCOL()))
@ PROW(),lnCol-7 SAY lnCut PICTURE '999999'+'ณ'
@ PROW(),lnCol SAY lnQty PICTURE '999999'+'ณ      บ'
lnSizTot=lnSizTot+lnQty
lnYdsNed=lnYdsNed+(lnQty*lnAve)
FOR J=1 TO IIF(lcRpForm = 'A',10,IIF(lcRpPaTyp='L',5,3))
  IF lcColor=laClr(J) .OR. J=IIF(lcRpForm = 'A',10,IIF(lcRpPaTyp='L',5,3))
    laClrTot(J)=laClrTot(J)+lnQty
    laCutTot(J)=laCutTot(J)+lnCut  
    laYrdNed(J)=laYrdNed(J)+(lnQty*lnAve)
    EXIT
  ENDIF
ENDFOR
RETURN

FUNCTION lfwRepWhen
IF FILE('MFEIL400.MEM')
  RESTORE FROM MFEIL400.MEM ADDITIVE
ELSE
  STORE SPACE(31) TO lcRepMsg1,lcRepMsg2,lcRepMsg3,lcRepMsg4,lcRepMsg5,lcRepMsg6
ENDIF  


*!**************************************************************
*! PROG : lfwOldVal
*! AUTH : Adel Mohammed El Gazzar (ADEL)
*! DESC : Save the old value
*!**************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*!**************************************************************
*! PROG : lfFillArr
*! AUTH : Adel Mohammed El Gazzar (ADEL)
*! DESC : Fill the select by arrays and get the color. 
*!**************************************************************
FUNCTION lfFillArr
*--Fill the select by arrays here
DIMENSION laRpSlArr[4,1],laRpFilArr[4,1]
laRpSlArr[1,1] = lcMajTitle+'(s)'
laRpSlArr[2,1] = 'Account'
laRpSlArr[3,1] = 'Fabric'
laRpSlArr[4,1] = 'Everything'
laRpFilArr[1,1] = 'S'
laRpFilArr[2,1] = 'A'
laRpFilArr[3,1] = 'F'
laRpFilArr[4,1] = 'E'

*--Get the color here
*-Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure.
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
*--

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 03/02/99
* Refer To : (C101400)
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 
*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 
SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEK)
*  DATE      : 03/02/99
*  Refer To  : (C101400)
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)

****************************************************************************
* FUNC: lfvAccount
* DESC: To valid the account.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 03/02/99
* Refer To  : (C101400)
****************************************************************************
FUNCTION lfvAccount

lcAlias = ALIAS()
*--Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*--- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18))
SELECT CUSTOMER
SET ORDER TO TAG CUSTOMER
*--IF The user want to Browse or if the Account he entered is not in the file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 
SELECT (lcAlias)
*--
****************************************************************************
* FUNC: lfClrRead
* DESC: To clear read.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 03/02/99
* Refer To  : (C101400)
****************************************************************************
FUNCTION lfClrRead

CLEAR READ
*--

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Badran (MAB)
*! DATE      : 03/02/99
*! Purpose   : Validation function for the Order field
*! Refer To  : (C101400)
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*--IF Statment to check if we are going to Browse
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK ('O'+lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "ORDER     :R :H= 'ORDER#' , "    +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "Terms=gfCodDes(CTERMCODE , 'CTERMCODE') :R :H= 'Terms' ,"  +;
               "ShipV=gfCodDes(ShipVia , 'SHIPVIA')   :R :H= 'ShipVia' ,"  +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "
  lcFile_Ttl = "Orders..."
  lcBrowCond = [FOR STATUS != "X" AND OPEN >= 1 ]
  = gfBrows(lcBrowCond,'ORDER','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = lcOldVal
  ENDIF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lcAlias)
*--
*!*************************************************************
*!FUNC  : lfSetDev
*!DESC. : Function to Adjust the scape sequence.
*!Auth  : Adel Mohammed El Gazzar (ADEL)
*!Date  : 03/01/99
*:*************************************************************
FUNCTION lfSetDev

*--Start the report printing.
SET DEVICE TO PRINT
*--Set to maupolate the printing.
lcPdSETup = _pdsetup
DO CASE
  CASE gcDevice = 'SCREEN'
    _PDSETUP=''
    gcOutFile = gcWorkDir+gfTempName()+'.TXT'
    SET PRINTER TO &gcOutFile
  CASE gcDevice = 'FILE'  
    _PDSETUP=''  
    SET PRINTER TO &gcOutFile  
  CASE gcDevice = 'PRINTER'  
    _PDSETUP=''  
    gcOutFile = gcWorkDir+gfTempName()+'.TXT'
    SET PRINTER TO &gcOutFile
ENDCASE

*!*************************************************************
*!FUNC  : lfvOKBut
*!DESC  : Function to validate the ok button in the memo screen.
*!Auth  : Adel Mohammed El Gazzar (ADEL)
*!Date  : 03/01/99
*:*************************************************************
FUNCTION lfvOKBut

SAVE ALL LIKE lcRepMsg* TO MFEIL400.MEM

*!*************************************************************
*!FUNC  : lfvMemo
*!DESC  : Function to brow the memo screen.
*!Auth  : Adel Mohammed El Gazzar (ADEL)
*!Date  : 03/01/99
*:*************************************************************
FUNCTION lfvMemo

IF FILE('MFEIL400.MEM')
  RESTORE FROM MFEIL400.MEM ADDITIVE
ELSE
  STORE SPACE(31) TO lcRepMsg1,lcRepMsg2,lcRepMsg3,lcRepMsg4,lcRepMsg5,lcRepMsg6
ENDIF  
IF llRpMemo
   DO GCREPHOME+GCACT_APPL+'\MFEIL400.SPX'
*  DO W:\ARIA27\REPORTS\MF\MFEIL400.SPX
ENDIF

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfSetIndx 
PARAMETER lcPar

IF lcPar = 'S'
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF  


