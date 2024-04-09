*!***************************************************************************
*! Program file  : ICBNRPL.PRG
*! Program desc. : Bin Location Replenishment for GPS27 (UK - Tony)
*! Date          : 03/02/2006
*! System        : Aria Advantage Series.
*! Module        : INVENTORY CONTROL (IC)
*! Developer     : NADER NABIL (NNA)
*! Tracking Job Number: C130447
*!***************************************************************************
*! Functions     : lfStySum,lfSRSty,lfOTSbStat,lfvWareHo,lfCrttemp,lfChkStrct,
*!               : lfOpnFiles,lfSetRela,lfSave,lfRecLock,lfInvUpdt,lfSavBnDat,
*!               : lfDLSVBNI,lfSavBnInv,lfDLSTYCRL,lfSTYCRL,lfUpdGLDist,
*!               : lfGetTrnQt,lfPrntRpt.
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Notes   : ....
*!***************************************************************************
*! Example : DO ICBNRPL
*!***************************************************************************
*! Modifications :
*B607763,1 TMI 07/18/2006 Fix a bug that when the stk = alo in a Pulk bin then no replanshiment is occured
*C132754,1 TMI 07/19/2006 Replenish only bins with CREPLENISH is set to 'Y' 
*!***************************************************************************
PRIVATE lcExpr,lnClrLen,lnClrStPos,lnStyLen,lnStyStPos,lnScaLen,lnScaStPos,lnRemain
STORE 0 TO lnClrLen,lnClrStPos,lnStyLen,lnStyStPos,lnScaLen,lnScaStPos,lnRecCount,lnRecNo,lnOldStk,lnCycle,lnRemain
STORE '' TO lcExpr,lcCostMth,lcStyleBns,lcTmpBnTo,lcTmpBnFrm,lcStyle,lcWareCode,lcGlFYear,lcGlPeriod,;
            lcBinAdj,lcTmpAdj,lcTmpFile,lcType,lcFromWare,lcRcvSessNo
STORE .F. TO llWareLoc,llMultiWH,llGlLink
STORE gdSysDate TO ldPstDate,ldDate
=CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod','IA',.T.)

IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"You don't use Bin Location , Please set [Use Bin Location] Setup to 'Yes'")  
  RETURN
ENDIF

lcCostMth  = gfGetMemVar('M_Cost_Meth')
llWareLoc  = gfGetMemVar('M_WareLoc')  ='Y'
llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
llGlLink   = gfGetMemVar('M_Link_GL')  ='Y'

lcEscapeKy = ON("KEY","ESC")
lcExpr     = gfOpGrid('ICBNRPL', .T.)
IF EMPTY(lcExpr) 
  RETURN
ENDIF
IF !lfOpnFiles("STYLE,STYDYE,BININVJL,WHSLOC,WHBINLOC,WAREHOUS,CODES,SCALE,INVTADJ",;
               "STYLE,STYDYE,STYINVJL,WHSLOC,WHBINLST,WAREHOUS,CODES,SCALE,INVTADJ")
  RETURN
ENDIF
*--Create the needed temp files.
lcBinAdj  = gfTempName()
lcTmpAdj  = gfTempName()
lcTmpFile = gfTempName()
=lfCrttemp()
*--Get length of style major,color and scale
= lfChkStrct()
*--Set Relationship between files
= lfSetRela()
SELECT STYDYE
COUNT FOR &lcExpr TO lnRecCount
LOCATE
SCAN FOR &lcExpr
  lcStyle = STYDYE.STYLE
  lcWareCode = STYDYE.CWARECODE
  lnRecNo = lnRecNo + 1
  =gfThermo( lnRecCount , lnRecNo ,'Replenishment Style : ',Style.Style)
  *B607763,1  TMI [Start] Add new 8 fields for the qty in the binlocation to check if the Pulk binlocation is able to transfer qty to  
  *SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
  *     (Whbinloc.Qty1-Whbinloc.ALO1) AS QTY1,(Whbinloc.Qty2-Whbinloc.ALO2) AS QTY2,;
  *     (Whbinloc.Qty3-Whbinloc.ALO3) AS QTY3,(Whbinloc.Qty4-Whbinloc.ALO4) AS QTY4,;
  *     (Whbinloc.Qty5-Whbinloc.ALO5) AS QTY5,(Whbinloc.Qty6-Whbinloc.ALO6) AS QTY6,;
  *     (Whbinloc.Qty7-Whbinloc.ALO7) AS QTY7,(Whbinloc.Qty8-Whbinloc.ALO8) AS QTY8,;
  *     (Whbinloc.TotQty-Whbinloc.TOTALO) AS TOTQTY,Whbinloc.cbinclass, Whbinloc.csection,WhsLoc.cBlkPck,Whsloc.cFlatHang;
  *   FROM  Whbinloc, Whsloc;
  *   WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style = SPACE(19);
  *         AND Whbinloc.style+Whbinloc.cWarecode+Whbinloc.cLocation = lcStyle+lcWareCode ;
  *   INTO CURSOR lcStyleBns 
  *C132754,1  TMI [Start] add the criteria to select only lines with WHSLOC.CREPLENISH is 'Y'
  WAIT WINDOW NOWAIT 'Please wait while selecting data ... '
  SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
       (Whbinloc.Qty1-Whbinloc.ALO1) AS QTY1,(Whbinloc.Qty2-Whbinloc.ALO2) AS QTY2,;
       (Whbinloc.Qty3-Whbinloc.ALO3) AS QTY3,(Whbinloc.Qty4-Whbinloc.ALO4) AS QTY4,;
       (Whbinloc.Qty5-Whbinloc.ALO5) AS QTY5,(Whbinloc.Qty6-Whbinloc.ALO6) AS QTY6,;
       (Whbinloc.Qty7-Whbinloc.ALO7) AS QTY7,(Whbinloc.Qty8-Whbinloc.ALO8) AS QTY8,;
       (Whbinloc.TotQty-Whbinloc.TOTALO) AS TOTQTY,Whbinloc.cbinclass, Whbinloc.csection,WhsLoc.cBlkPck,Whsloc.cFlatHang,;
       Whbinloc.Qty1 AS Q1,Whbinloc.Qty2 AS Q2,Whbinloc.Qty3 AS Q3,Whbinloc.Qty4 AS Q4,;
       Whbinloc.Qty5 AS Q5,Whbinloc.Qty6 AS Q6,Whbinloc.Qty7 AS Q7,Whbinloc.Qty8 AS Q8 ;
     FROM  Whbinloc, Whsloc;
     WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style = SPACE(19);
           AND Whbinloc.style+Whbinloc.cWarecode+Whbinloc.cLocation = lcStyle+lcWareCode ;
           AND WHSLOC.CREPLENISH = 'Y' ;
     INTO CURSOR lcStyleBns 
  WAIT CLEAR
  *C132754,1  TMI [End  ] 
  *B607763,1  TMI [End  ] 

  SELECT * FROM lcStyleBns WHERE CBLKPCK='P' INTO CURSOR lcTmpBnTo     
  SELECT * FROM lcStyleBns WHERE CBLKPCK='B' INTO CURSOR lcTmpBnFrm ORDER BY CLOCATION
   
  SELECT lcTmpBnTo
  SCAN
    FOR Z = 1 TO 8
      LCZ = ALLTRIM(STR(Z))
      lnCycle   = 1
      lnRemain  = 0
      m.Adj&LCZ = 0
      *B607763,1  TMI [Start] check the qty in the pulk location is > 0 not the qty-alo
      *IF QTY&LCZ > 0 AND QTY&LCZ<=STYLE.NREPLTRG
      IF Q&LCZ > 0 AND QTY&LCZ<=STYLE.NREPLTRG
        *B607763,1  TMI [End  ] 
        lnOldStk = lcTmpBnTo.TOTQTY
        SELECT lcTmpBnFrm
        llContinue = .T.
        SCAN FOR QTY&LCZ > 0 AND llContinue
          IF !lfGetTrnQt()
            LOOP
          ENDIF
          *--Update lcTmpAdj and lcBinAdj files .
          =lfUpdTemp()
        ENDSCAN
      ENDIF
      SELECT lcTmpBnTo
    ENDFOR
  ENDSCAN
ENDSCAN
IF RECCOUNT(lcBinAdj) = 0 OR RECCOUNT(lcTmpAdj)=0
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"There are no Pick bin locations need to replenishment , No records updated")  
  RETURN
ENDIF

=lfSave()

SELECT STYDYE
SET RELATION TO
SELECT WHBINLOC
SET RELATION TO
SELECT(lcTmpAdj)
SET RELATION TO
SELECT STYLE
SET RELATION TO
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
SELECT(lcBinAdj)
SET RELATION TO
SET RELATION TO STYLE INTO STYLE ADDITIVE

lnRecNo = 0
SCAN
  lnRecNo = lnRecNo + 1
  =gfThermo(RECCOUNT(lcBinAdj),lnRecNo,'Preparing data to print for style : ',&lcBinAdj..Style)
  SCATTER MEMVAR MEMO
  m.Desc1    = STYLE.DESC1
  m.Clr_Code = SUBSTR(&lcBinaDj..Style,lnClrStPos,lnClrLen)
  m.Clr_Desc = gfCodDes(m.Clr_Code,'COLOR')
  m.cPurCode = Style.cPurCode
  m.From_Bin = m.LocFrom
  m.To_Bin   = m.LocTo
  
  SELECT(lcTmpFile)
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    IF &lcBinAdj..TrnQty&LCI>0
      m.Qty  = &lcBinAdj..TrnQty&LCI
      m.Size = SCALE.SZ&LCI
      INSERT INTO (lcTmpFile) FROM MEMVAR
     ENDIF
   ENDFOR
ENDSCAN
SELECT(lcBinAdj)
SET RELATION TO
SELECT(lcTmpFile)
SET RELATION TO
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE
=lfPrntRpt()
*!***************************************************************************
*!* Name        : lfUpdTemp
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Update Temp File with the Collected data
*!***************************************************************************
*!* Called from : ICBNRPL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdTemp()
*!***************************************************************************
FUNCTION lfUpdTemp
SELECT(lcBinAdj)
SET ORDER TO TAG (lcBinAdj)
IF !SEEK(STYLE.STYLE+STYDYE.CWARECODE+lcTmpBnFrm.CLOCATION+lcTmpBnTo.CLOCATION+STYLE.CDIVISION)
  APPEND BLANK
  REPLACE &lcBinAdj..cWareCode  WITH STYDYE.CWARECODE     ,;
          &lcBinAdj..CDIVISION  WITH STYLE.CDIVISION      ,;
          &lcBinAdj..STYLE      WITH STYLE.STYLE          ,;
          &lcBinAdj..LocFrom    WITH lcTmpBnFrm.CLOCATION ,;
          &lcBinAdj..ClassFrom  WITH lcTmpBnFrm.CBINCLASS ,;
          &lcBinAdj..FlatFrom   WITH lcTmpBnFrm.cFlatHang ,;
          &lcBinAdj..LocTo      WITH lcTmpBnTo.CLOCATION  ,;
          &lcBinAdj..ClassTO    WITH lcTmpBnTo.CBINCLASS  ,;
          &lcBinAdj..FlatTo     WITH lcTmpBnTo.cFlatHang  ,;
          &lcBinAdj..TrnQty&LCZ WITH m.Adj&LCZ  
ELSE
  REPLACE &lcBinAdj..TrnQty&LCZ WITH &lcBinAdj..TrnQty&LCZ+m.Adj&LCZ 
ENDIF
SELECT (lcTmpAdj)  
IF !SEEK(Style.Style)
  APPEND BLANK
  REPLACE Style      WITH Style.Style         ,;
  		  cFromWare  WITH STYDYE.CWARECODE    ,;
		  cToWare    WITH STYDYE.CWARECODE    ,;
		  cReason    WITH "Bin Location Replenishment" ,;
		  Date       WITH ldDate              ,;
	      Type       WITH "T"                 ,; 
          Unt_Cost   WITH STYLE.Ave_Cost      ,;
          Old_Cost   WITH STYLE.Ave_Cost      ,;
          Adj&LCZ    WITH m.Adj&LCZ           ,;
          TotAdj     WITH m.ADJ&LCZ           ,;
          TotOld     WITH Style.TotStk        ,;
          OldQty&LCZ WITH STYLE.STK&LCZ       ,;
          NTOTOLDTO  WITH Style.TotStk        ,; 
          NOLDTO&LCZ WITH STYLE.STK&LCZ       ,;
          dAdd_Date  WITH gdSysDate           ,;
          cAdd_Time  WITH TIME()              ,;
          cAdd_User  WITH gcUser_id           ,;
          GlFYear    WITH lcGlFyear           ,;
          GlPeriod   WITH lcGlPeriod          ,;
          nTotOldTo  WITH lnOldStk
ELSE
  REPLACE Adj&LCZ    WITH Adj&LCZ + m.Adj&LCZ ,;
          TotAdj     WITH TotAdj + m.Adj&LCZ
ENDIF
*-- End of Function lfUpdTemp.
*!***************************************************************************
*!* Name        : lfStySum
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : sum a specific field for the current style in style file
*!***************************************************************************
*!* Called from : Option Grid
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : Calculated field value.
*!***************************************************************************
*!* Example     : = lfStySum()
*!***************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of Function lfStySum.
*!***************************************************************************
*!* Name        : lfSRSty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Set and Rest functions for style filter.
*!***************************************************************************
*!* Called from : Option Grid.
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSRSty()
*!***************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!***************************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- End of Function lfSRSty.
*!***************************************************************************
*!* Name        : lfOTSbStat
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : to adjust the status of "OTS based on" option
*!***************************************************************************
*!* Called from : Option Grid.
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOTSbStat()
*!***************************************************************************
FUNCTION lfOTSbStat
lnOTSSig = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPOTSSig'),1)
lnOTSMin = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lnRPOTSMin'),1)
lnOTSB   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPOTSB'),1)

llOTSFoun = ASCAN(laRPRepTar,"OTS") > 0 OR ASCAN(laRPRepTar,"Imm. OTS") > 0

IF llOTSFoun
  IF (!(lcRPSortBy == "W") AND  !llRPWhDeta)
    llStatOB = .T.
    llStatOS = .T.
    llStatOM = .T.
  ELSE
    *-- if OTS is selected but the sort by location or print location detail
    *-- then make the OTS based on WIP "W" and disable this option "OTS based on"
    *-- because this will be from StyDye file which has not plan fields
    lcRPOTSB = 'W'      
    llStatOB = .F.
    llStatOS = .T.
    llStatOM = .T.
  ENDIF
ELSE
    llStatOB = .F.
    llStatOS = .F.
    llStatOM = .F.
ENDIF

laOGObjCnt[lnOTSB]   = llStatOB
laOGObjCnt[lnOTSSig] = llStatOS
laOGObjCnt[lnOTSMin] = llStatOM

= lfOGShowGet('lcRPOTSB')
= lfOGShowGet('lcRPOTSSig')
= lfOGShowGet('lnRPOTSMin')
*-- End of Function lfOTSbStat.
*!***************************************************************************
*!* Name        : lfvWareHo
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Validate warehouse
*!***************************************************************************
*!* Called from : Option Grid.
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvWareHo()
*!***************************************************************************
FUNCTION lfvWareHo
lcWareHo = VARREAD()
lcTag = ORDER('WAREHOUS')
SET ORDER TO WAREHOUS IN WAREHOUS
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
  *--Only Finished goods inventory locations can be selected.
  &lcWareHo = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS
*-- End of Function lfvWareHo.

*!***************************************************************************
*!* Name        : lfCrttemp
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Create temp files
*!***************************************************************************
*!* Called from : ICBNRPL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCrttemp()
*!***************************************************************************
FUNCTION lfCrttemp
*--Create the needed Temp file for the Report
DIMENSION laTempStru[11,4]
laTempStru = ''

laTempStru[1, 1] = 'cWareCode'
laTempStru[1, 2] = 'C'
laTempStru[1, 3] = 6
laTempStru[1, 4] = 0

laTempStru[2, 1] = 'CDivision'
laTempStru[2, 2] = 'C'
laTempStru[2, 3] = 6
laTempStru[2, 4] = 0

laTempStru[3, 1] = 'STYLE'
laTempStru[3, 2] = 'C'
laTempStru[3, 3] = 19
laTempStru[3, 4] = 0

laTempStru[4, 1] = 'Desc1'
laTempStru[4, 2] = 'C'
laTempStru[4, 3] = 60
laTempStru[4, 4] = 0

laTempStru[5, 1] = 'CLR_Code'
laTempStru[5, 2] = 'C'
laTempStru[5, 3] = 6
laTempStru[5, 4] = 0

laTempStru[6, 1] = 'CLR_DESC'
laTempStru[6, 2] = 'C'
laTempStru[6, 3] = 30
laTempStru[6, 4] = 0

laTempStru[7, 1] = 'CPURCODE'
laTempStru[7, 2] = 'C'
laTempStru[7, 3] = 6
laTempStru[7, 4] = 0

laTempStru[8, 1] = 'SIZE'
laTempStru[8, 2] = 'C'
laTempStru[8, 3] = 5
laTempStru[8, 4] = 0

laTempStru[9, 1] = 'FROM_BIN'
laTempStru[9, 2] = 'C'
laTempStru[9, 3] = 10
laTempStru[9, 4] = 0

laTempStru[10, 1] = 'TO_BIN'
laTempStru[10, 2] = 'C'
laTempStru[10, 3] = 10
laTempStru[10, 4] = 0

laTempStru[11, 1] = 'QTY'
laTempStru[11, 2] = 'N'
laTempStru[11, 3] = 10
laTempStru[11, 4] = 0
CREATE TABLE (gcWorkDir+lcTmpFile) FROM ARRAY laTempStru
INDEX ON cWareCode+CDivision+Style+CLR_Code+SIZE+FROM_BIN TAG (lcTmpFile)  OF (lcTmpFile)

*--Create the needed Temp file for inventory updating
SELECT INVTADJ
lcTmpAdj = gfTempName()
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru,'UNT_COST')
  lnNo2=ASCAN(laFStru,'OLD_COST')
*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE 7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+2,4]
laFStru[lnFStru+1,1] = 'cAdjReason'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0
laFStru[lnFStru+2,1] = 'cRefer'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0
CREATE TABLE (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru

INDEX ON Style+STR(RECNO(),6) TAG &lcTmpAdj
*SET RELATION TO Style INTO STYLE

*--Create a Cursor for Bin Location.
CREATE TABLE (gcWorkDir+lcBinAdj) (STYLE C(19),CWARECODE C(6),CDIVISION C(6),LocFrom C(10),LocTo C(10),ClassFrom C(2),;
       ClassTo C(2),FlatFrom C(1),FlatTo C(1),TrnQty1 N(6),TrnQty2 N(6),TrnQty3 N(6),TrnQty4 N(6),TrnQty5 N(6),;
       TrnQty6 N(6),TrnQty7 N(6),TrnQty8 N(6))
INDEX ON CWARECODE+cDivision+STYLE+LocFrom TAG WHDVSTLOCF OF (lcBinAdj)
INDEX ON STYLE+CWARECODE+LocFrom+LocTo+cDivision TAG (lcBinAdj) OF (lcBinAdj)

*-- End of Function lfCrttemp.
*!***************************************************************************
*!* Name        : lfChkStrct
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Get the Style and Color Length.
*!***************************************************************************
*!* Called from : ICBNRPL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkStrct()
*!***************************************************************************
FUNCTION lfChkStrct
*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLen  = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*-- End of Function lfChkStrct.
*!***************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Open Needed files to the program
*!***************************************************************************
*!* Called from : SMDLRBL.PRG
*!***************************************************************************
*!* Parameters  : lcFilesExp --> Files that need to open.
*!*             : lcTages    --> Tags for the opened files.
*!*             : lcCurPik   --> Current Piktkt.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOpnFiles()
*!***************************************************************************
FUNCTION lfOpnFiles
PARAMETERS lcFilesExp,lcTages
DIMENSION laOpnFiles[1],laOpnTages[1]
=GFSUBSTR(lcFilesExp,@laOpnFiles,',')
=GFSUBSTR(lcTages,@laOpnTages,',')
FOR I=1 TO ALEN(laOpnFiles,1)
  IF !USED(laOpnFiles[I])
    =gfOpenFile(gcDataDir+laOpnFiles[I],laOpnTages[I],'SH')
  ELSE
    SET ORDER TO TAG laOpnTages[I] IN laOpnFiles[I]
  ENDIF
ENDFOR
RETURN .T.
*-- End of Function lfOpnFiles.
*!***************************************************************************
*!* Name        : lfSetRela
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Set relatioship between some files
*!***************************************************************************
*!* Called from : ICBNRPL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSetRela()
*!***************************************************************************
FUNCTION lfSetRela

SELECT STYDYE
SET RELATION TO
SET RELATION TO StyDye.style+ StyDye.cWareCode INTO WhBinLoc ADDITIVE
SET RELATION TO StyDye.style INTO Style ADDITIVE

SELECT STYLE
SET RELATION OFF INTO SCALE
SET RELATION TO 'S'+STYLE.SCALE INTO SCALE ADDITIVE

SELECT WHBINLOC
SET RELATION OFF INTO WHSLOC
SET RELATION TO Whbinloc.cwarecode+ Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE

*-- End of Function lfSetRela.
*!***************************************************************************
*!* Name        : lfSave
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Inventory updating
*!***************************************************************************
*!* Called from : ICBNRPL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSave()
*!***************************************************************************
FUNCTION lfSave
PARAMETERS llFromEDI
PRIVATE lcFromWare,lcToWare
STORE '' TO lcFromWare,lcToWare
lcType = 'T'
*-- Open a temp file to be used in calling gl distributer proc.
IF llGlLink
  =lfOpnFiles("GLDist,StyInvJl","GLDistAc,StyInvJl")
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO &gcWorkDir.&lcTmpGlDt
  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF  
*--Save to master files.
DIMENSION laToSave[8],laTemp[8]
=lfOpnFiles("StyInvJl","StyInvJl")

SELECT (lcTmpAdj)
SCAN
  lcFromWare = &lcTmpAdj..cFromWare
  lcToWare = &lcTmpAdj..cToWare
  
  *--Check if nothing to adjust or transfer (all Zero).
  IF lcType = 'T'
    SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8 TO laToSave
    llZeroAdj = .T.
    FOR lnI = 1 To 8 
      llZeroAdj = IIF(laToSave[lnI]=0, .T., .F.)
      IF !llZeroAdj
        EXIT
      ENDIF
    ENDFOR
    IF llZeroAdj
      DELETE
      LOOP
    ENDIF
  ENDIF
 
  = SEEK( Style,'STYLE')
  = SEEK( Style + cFromWare + SPACE(10),'STYDYE')

  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
  IF llGoOn
    =SEEK(Style+cFromWare,'STYDYE')
    llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE'))
  ENDIF  

  SELECT (lcTmpAdj)
  IF !llGoOn
    *-Style XDX: XXX/XXX is in use by another user, Unable to update.
    =gfModalGen('TRM42067B42001','DIALOG',+'|'+'|'+ALLTRIM(STYLE))
    DELETE
    UNLOCK ALL
    LOOP
  ENDIF

  *-- Store Style old cost, Old stock, And link code.
  SELECT Style
  lnOldStk   = TotStk
  lnOldCost  = IIF(lcCostMth<>'S',Ave_Cost,TotCost)  
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  SELECT (lcTmpAdj)


  *--Saving the old quantity in the master file and the checking of the FROM stock.
  *-- Store the stock in the TO warehouse before updating the master file.
  = SEEK( Style + cToWare,'STYDYE')
  SELECT StyDye
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk     
  *--If transfer save the link code for to warehouse.
  lcToLink = IIF(llGlLink ,IIF(!EMPTY(Gl_Link),Gl_Link,lcLinkCode),"")
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS nOldTo1,nOldTo2,nOldTo3,nOldTo4,nOldTo5,nOldTo6,nOldTo7,nOldTo8
  REPLACE nTotOldTo WITH lnTotStk

  = SEEK( Style + cFromWare ,'STYDYE')
  *-- We should be sure that the stock in the FROM warehouse if enougth
  *-- to be transfered to the TO warehouse.
  SELECT StyDye
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk     
  
  llZeroStk = .F.
  FOR lnI = 1 TO 8 
    llZeroStk = IIF(laToSave[lnI] = 0, .T., .F.)
    IF !llZeroStk
      EXIT
    ENDIF
  ENDFOR
  SELECT (lcTmpAdj)
  IF llZeroStk
    *-Style/xDx : xxx/xxx is out of stock in warehouse: xxx, Cannot transfer.
    =gfModalGen('TRM42068B42001','DIALOG',''+'|'+ALLTRIM(Style)+'|'+' '+'|'+ALLTRIM(lcFromWare))
    DELETE REST WHILE Style+cFromWare+Dyelot = STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot
    UNLOCK ALL
    LOOP                  
  ENDIF
    
  *-- Check the stock per size.
  llVldTran = .T.
  FOR lnI = 1 TO 8 
    lcAdjNum = 'Adj' + STR(lnI, 1)
    llVldTran = IIF(laToSave[lnI] >= &lcAdjNum OR &lcAdjNum = 0, .T., .F.)
    IF !llVldTran 
      EXIT
    ENDIF
  ENDFOR
  *-- If there is any size that does not have enougth stock to
  *-- be transfered then please till this to the user and ask him
  *-- if he wants to transfer all the stock that exists in the FROM
  *-- warehouse to the TO warehouse.
  IF !llVldTran
    *--The stock level has changed for style/xdx:xxx/xxx in warehouse: xxx Transfer what is available ? \<Yes;\<No.
    IF gfModalGen('TRM42069B42002','DIALOG',''+'|'+ALLTRIM(Style)+'|'+' '+'|'+ALLTRIM(lcFromWare)) = 1

      *-- If the user wants to transfer the stock, all we have to
      *-- do is to save the FROM warehouse stock in the temp. file
      *-- as the adjustments quantity.
      GATHER FROM laToSave FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8
      REPLACE TotAdj WITH lnTotStk
    ELSE
      DELETE
      UNLOCK ALL
      LOOP
    ENDIF
  ENDIF

  SELECT IIF(!llMultiWH , 'Style', 'StyDye')
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                              OldQty5,OldQty6,OldQty7,OldQty8
  REPLACE TotOld    WITH lnTotStk
  REPLACE dPostDate WITH ldPstDate
  
  WAIT WINDOW 'Start updating => '+ALLTRIM(Style) NOWAIT


  *-- Store WAREHOUSE link code. If warehouse link code is empty default
  *-- warehouse link code to style link code. If the last one is empty
  *-- default to 'DEF' link code.
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")

  *--Start Updating.
  IF !lfInvUpdt()
    SELECT (lcTmpAdj)
    LOOP
  ENDIF
  
  SELECT (lcTmpAdj)
  REPLACE cSession WITH STYINVJL.cSession

  SELECT (lcTmpAdj)
  UNLOCK ALL
ENDSCAN

*--Update master adjustment file.
WAIT WINDOW ' Updating the master adjustment file.' NOWAIT
=lfSavBnDat()

USE IN (lcTmpAdj)
SELECT InvtAdj
APPEND FROM &gcWorkDir.&lcTmpAdj FOR !DELETED()

USE (gcWorkDir+lcTmpAdj) IN 0 EXCLUSIVE
SELECT (lcTmpAdj)
SET ORDER TO TAG &lcTmpAdj
SELECT (lcTmpAdj)
ZAP
SET RELATION TO STYLE INTO STYLE

*-- Update distripution master file
IF llGlLink
  IF !(&lcBinAdj..LocFrom = &lcBinAdj..LocTo AND  lcFromWare = lcToWare)
    WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
    SELECT (lcTmpGlDt)
    *-- Generate a unique session number.
    lcGlSess = gfsequence('GLSESSION')
    REPLACE ALL GLSESSION WITH lcGlSess
    USE
    SELECT GLDIST  
    APPEND FROM &gcWorkDir.&lcTmpGlDt     
    ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
  ENDIF  
ENDIF  
WAIT CLEAR
*-- End of Function lfSave
*!***************************************************************************
*!* Name        : lfRecLock
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Record Lock.
*!***************************************************************************
*!* Called from : ICBNRPL.PRG --> lfSave
*!***************************************************************************
*!* Parameters  : lcFile->Locked file.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRecLock()
*!***************************************************************************
FUNCTION lfRecLock
PARAMETERS lcFile

SET REPROCESS TO 5 SECONDS 
DO WHILE .T.
  *-This record is in use by another user !','\!\<Retry;\<Cancel'
  lnChoice=gfModalGen('INM00029B00015','DIALOG')
  IF lnChoice = 1 
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF 
  ELSE
    lnRet = .F.
    EXIT   
  ENDIF
ENDDO
SET REPROCESS TO 0
RETURN (lnRet)
*-- End of Function lfRecLock.

*!***************************************************************************
*!* Name        : lfInvUpdt
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Update inventory.
*!***************************************************************************
*!* Called from : ICBNRPL.PRG --> lfSave
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfInvUpdt()
*!***************************************************************************
FUNCTION lfInvUpdt
SELECT (lcTmpAdj)
*--Gl adjustment account.
lcAdjAcct = ' '
IF llGlLink AND !EMPTY(cAdjReason)
  lcAdjReason = cAdjReason
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IA'      TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPstDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF
SELECT (lcTmpAdj)
*--Adjustment quantity array TRANSFER Case.
IF lcType = 'T'
  DECLARE laAdjust[9]
  FOR I = 1 TO 8
    Z=STR(I,1)
    laAdjust[I] = -Adj&Z
  ENDFOR
  laAdjust[9] = -TotAdj

  *--Call the global function for update style inventory control.
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start) AAMER 04/19/98
  PRIVATE lcRefer 
  lcRefer = "Bin Location Replenishment"
  lcAdjRef = &lcTmpAdj..cRefer
  lnRet=gfStyCrl('1',Style,cFromWare,Dyelot,Date,'',@laAdjust,0,;
            lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)
  *--Read an issue seesion no to use it in receive if LIFO or FIFO.
  lcIsuSessNo = STYINVJL.cSession
  
  *--Return with no save if function return fulse.
  IF lnRet = 0
    RETURN .F.
  ENDIF  
  *nader
  =lfdlsvbni()
  *nader
  *--Next Adjustment will use the link code for TO Warehouse.
  IF llGlLink
    laGLDistAr[1,1] = lcToLink
    laGLDistAr[2,1] = lcToLink
  ENDIF 
ENDIF

*--Warehouse Code.
lcAdjWareH = IIF(lcType = 'T',lcToWare,lcFromWare )
*--If transfer and costing method LIFO or FIFO receive by cost of issue.
IF lcType = 'T' AND lcCostMth $ 'FL' 
  SELECT STYINVJL 
  SEEK Style+lcFromWare+lcIsuSessNo
  SCAN WHILE Style+cWareCode+cSession = Style+lcFromWare+lcIsuSessNo
    lnSavRcNo = RECNO()
    *--Adjustment quantity array.
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = ABS(nStk&Z)
    ENDFOR
    laAdjust[9] = ABS(nTotStk)

    *--Call the global function for update style inventory control.
    *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
    PRIVATE lcRefer 
    lcRefer = "Bin Location Replenishment"
    
    lcAdjRef = &lcTmpAdj..cRefer
    lnRet=gfStyCrl('1',Style,lcAdjWareH,cDyelot,dTrDate,'',@laAdjust,;
                   nCost,lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
    lcRcvSessNo = STYINVJL.cSession
    =lfSavBnInv()
    *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (End)
    SELECT STYINVJL
    GOTO lnSavRcNo   
  ENDSCAN 
ELSE
  *--Adjustment quantity array.
  DECLARE laAdjust[9]
  SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj TO laAdjust

  *--Adjustment cost.
  lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
  *--Type of the adjustment.
  lcAdjTyp = '1'

  *--Call the global function for update style inventory control.
  *-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
  PRIVATE lcRefer 
  lcRefer = "Bin Location Replenishment"
  lcAdjRef = &lcTmpAdj..cRefer

  lnRet = 0
  =lfDLSTYCRL()
  lcIsuSessNo = STYINVJL.cSession
  =lfDLSVBNI()
ENDIF

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF  
RETURN
*-- End of Function lfInvUpdt.
*!***************************************************************************
*!* Name        : lfSavBnDat
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Save Data
*!***************************************************************************
*!* Called from : ICBNRPL.PRG --> lfSave
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSavBnDat()
*!***************************************************************************
FUNCTION lfSavBnDat
IF !lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC","STYINVJL,STYINVJL,WHBINLOC,WHSLOC")
  RETURN
ENDIF
SET ORDER TO TAG (lcBinAdj) IN (lcBinAdj)
SELECT (lcTmpAdj)
LOCATE
SCAN
  SELECT(lcBinAdj)
  IF SEEK(&lcTmpAdj..Style)
    SCAN REST WHILE STYLE+CWARECODE+LocFrom+LocTo+cDivision = &lcTmpAdj..Style
      lnTotTrnQt = TrnQty1+TrnQty2+TrnQty3+TrnQty4+TrnQty5+TrnQty6+TrnQty7+TrnQty8
      *--from location
      IF SEEK(&lcTmpAdj..cfromware +&lcBinAdj..LocFrom +&lcTmpAdj..Style,'WHBINLOC') 
        SELECT WHBINLOC
        REPLACE Qty1       WITH MAX(Qty1- &lcBinAdj..TrnQty1,0),;
                Qty2       WITH MAX(Qty2- &lcBinAdj..TrnQty2,0),;
                Qty3       WITH MAX(Qty3- &lcBinAdj..TrnQty3,0),;
                Qty4       WITH MAX(Qty4- &lcBinAdj..TrnQty4,0),;
                Qty5       WITH MAX(Qty5- &lcBinAdj..TrnQty5,0),;
                Qty6       WITH MAX(Qty6- &lcBinAdj..TrnQty6,0),;
                Qty7       WITH MAX(Qty7- &lcBinAdj..TrnQty7,0),;
                Qty8       WITH MAX(Qty8- &lcBinAdj..TrnQty8,0),;
                TotQty     WITH MAX(TotQty- lnTotTrnQt,0)
        IF TotQty = 0
          DELETE
        ENDIF
      ENDIF
      *--To location
      IF !SEEK(&lcTmpAdj..cToware +&lcBinAdj..LocTo +&lcTmpAdj..Style,'WHBINLOC') 
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE STYLE      WITH &lcTmpAdj..Style     ,;
                CWARECODE  WITH &lcTmpAdj..cToware   ,;
                clocation  WITH &lcBinAdj..LocTo   ,;
                Qty1       WITH Qty1+ &lcBinAdj..TrnQty1,;
                Qty2       WITH Qty2+ &lcBinAdj..TrnQty2,;
                Qty3       WITH Qty3+ &lcBinAdj..TrnQty3,;
                Qty4       WITH Qty4+ &lcBinAdj..TrnQty4,;
                Qty5       WITH Qty5+ &lcBinAdj..TrnQty5,;
                Qty6       WITH Qty6+ &lcBinAdj..TrnQty6,;
                Qty7       WITH Qty7+ &lcBinAdj..TrnQty7,;
                Qty8       WITH Qty8+ &lcBinAdj..TrnQty8,;
                TotQty     WITH TotQty+ lnTotTrnQt
 
          *--Update cBlkPck and cSection
          lcOldOrd = ORDER('WHSLOC')
          SET ORDER TO TAG WhsLoc IN  WHSLOC
          IF SEEK(&lcTmpAdj..cfromware +&lcBinAdj..LocFrom +SPACE(19),'WHSLOC') 
            SELECT WHBINLOC
            REPLACE cBlkPck   WITH WHSLOC.cBlkPck  ,;
                    cSection  WITH WHSLOC.cSection ,;
                    cBinClass WITH WHSLOC.cBinClass
          ENDIF
          SET ORDER TO TAG &lcOldOrd IN  WHSLOC
        =gfAdd_Info('WHBINLOC')
      ELSE
        SELECT WHBINLOC
        REPLACE Qty1   WITH Qty1+ &lcBinAdj..TrnQty1  ,;
                Qty2   WITH Qty2+ &lcBinAdj..TrnQty2  ,;
                Qty3   WITH Qty3+ &lcBinAdj..TrnQty3  ,;
                Qty4   WITH Qty4+ &lcBinAdj..TrnQty4  ,;
                Qty5   WITH Qty5+ &lcBinAdj..TrnQty5  ,;
                Qty6   WITH Qty6+ &lcBinAdj..TrnQty6  ,;
                Qty7   WITH Qty7+ &lcBinAdj..TrnQty7  ,;
                Qty8   WITH Qty8+ &lcBinAdj..TrnQty8  ,;
                TotQty WITH TotQty+ lnTotTrnQt
      ENDIF
      SELECT WHBINLOC
      IF TotQty = 0
        DELETE
      ENDIF         
    ENDSCAN
  ENDIF
ENDSCAN 
*-- End of Function lfSavBnDat.
*!***************************************************************************
*!* Name        : lfDLSVBNI
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update BinInvJL & from issue Styinvjl
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg
*!***************************************************************************
*!* Calls       : lfOpnFiles()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSVBNI()
*!***************************************************************************
FUNCTION lfDLSVBNI
PRIVATE lcStyOrd,lnAlias
lnAlias = SELECT(0)
IF !lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL")
  RETURN
ENDIF
SELECT STYINVJL 
lnCurrec = RECNO()
lcsty1 = Style
lcWarhos = &lcTmpAdj..cFromWare
IF !SEEK(Style+&lcTmpAdj..cFromWare+lcIsuSessNo)
  lcWarhos = &lcTmpAdj..cToWare
  =SEEK(lcsty1 + lcWarhos + lcIsuSessNo)
ENDIF
SCAN WHILE Style+cWareCode+cSession = lcsty1 + lcWarhos + lcIsuSessNo
  SCATT MEMVAR MEMO
  SELECT(lcBinAdj)
  SET ORDER TO TAG (lcBinAdj)
  =SEEK(STYINVJL.STYLE)
  SCAN REST WHILE STYLE+CWARECODE+LocFrom+LocTo+cDivision = STYINVJL.Style
    IF STYINVJL.cirtype = 'I'
      m.clocation = &lcBinAdj..LocFrom
      m.nStk1   = -(&lcBinAdj..TrnQty1)
      m.nStk2   = -(&lcBinAdj..TrnQty2)
      m.nStk3   = -(&lcBinAdj..TrnQty3)
      m.nStk4   = -(&lcBinAdj..TrnQty4)
      m.nStk5   = -(&lcBinAdj..TrnQty5)
      m.nStk6   = -(&lcBinAdj..TrnQty6)
      m.nStk7   = -(&lcBinAdj..TrnQty7)                        
      m.nStk8   = -(&lcBinAdj..TrnQty8)      
      m.nTotStk = -(&lcBinAdj..TrnQty1+&lcBinAdj..TrnQty2+&lcBinAdj..TrnQty3+&lcBinAdj..TrnQty4+&lcBinAdj..TrnQty5+;
                  &lcBinAdj..TrnQty6+&lcBinAdj..TrnQty7+&lcBinAdj..TrnQty8)
    ELSE
      m.clocation = &lcBinAdj..LocTO
      m.nStk1     = &lcBinAdj..TrnQty1
      m.nStk2     = &lcBinAdj..TrnQty2
      m.nStk3     = &lcBinAdj..TrnQty3
      m.nStk4     = &lcBinAdj..TrnQty4
      m.nStk5     = &lcBinAdj..TrnQty5
      m.nStk6     = &lcBinAdj..TrnQty6
      m.nStk7     = &lcBinAdj..TrnQty7                        
      m.nStk8     = &lcBinAdj..TrnQty8      
      m.nTotStk   = &lcBinAdj..TrnQty1+&lcBinAdj..TrnQty2+&lcBinAdj..TrnQty3+&lcBinAdj..TrnQty4+&lcBinAdj..TrnQty5+;
                    &lcBinAdj..TrnQty6+&lcBinAdj..TrnQty7+&lcBinAdj..TrnQty8
      IF EMPTY(&lcBinAdj..LocTO)
        m.clocation = &lcBinAdj..LocFrom
      ENDIF  
    ENDIF  
    SELECT BININVJL
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDSCAN
SELECT STYINVJL 
IF BETWEEN(lnCurrec,1,RECCOUNT())
  GOTO lnCurrec IN STYINVJL
ENDIF
SELECT(lnAlias)
*-- End of Function lfDLSVBNI.
*!***************************************************************************
*!* Name        : lfSavBnInv
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update BinInvJL & from RECEIVE Styinvjl
*!***************************************************************************
*!* Called from : ICINVSAV.PRG --> FOR PRGS. ICSTYTR.PRG,ICSTYPH.PRG,ICSYTAD.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSavBnInv()
*!***************************************************************************
FUNCTION lfSavBnInv
PRIVATE lcStyOrd,lnAlias,lnCrRec 
lnAlias = SELECT(0)
*--Open the needed files
IF !lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL")
  RETURN
ENDIF
SELECT STYINVJL
lnCrRec = RECNO()
IF SEEK(Style+&lcTmpAdj..cToWare+lcRcvSessNo)
  SCATTER MEMVAR MEMO
  SELECT(lcBinAdj)
  =SEEK(STYINVJL.STYLE)
  SCAN REST WHILE STYLE+CWARECODE+LocFrom+LocTo+cDivision = STYINVJL.Style
    m.clocation = &lcBinAdj..LocTo
    m.nStk1   = &lcBinAdj..TrnQty1
    m.nStk2   = &lcBinAdj..TrnQty2
    m.nStk3   = &lcBinAdj..TrnQty3
    m.nStk4   = &lcBinAdj..TrnQty4
    m.nStk5   = &lcBinAdj..TrnQty5
    m.nStk6   = &lcBinAdj..TrnQty6
    m.nStk7   = &lcBinAdj..TrnQty7                        
    m.nStk8   = &lcBinAdj..TrnQty8      
    m.nTotStk = &lcBinAdj..TrnQty1+&lcBinAdj..TrnQty2+&lcBinAdj..TrnQty3+&lcBinAdj..TrnQty4+&lcBinAdj..TrnQty5+;
                &lcBinAdj..TrnQty6+&lcBinAdj..TrnQty7+&lcBinAdj..TrnQty8
    SELECT BININVJL
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF
SELECT STYINVJL
IF BETWEEN(lnCrRec,1,RECCOUNT())
  GOTO lnCrRec IN STYINVJL
ENDIF
*-- End of Function lfSavBnInv.
*!***************************************************************************
*!* Name        : lfDLSTYCRL
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Trigger to Validate Post Style Per Bin Location in inv. Locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSTYCRL()
*!***************************************************************************
FUNCTION lfDLSTYCRL
IF TYPE('lcType') = 'U'
  lcBinLoc = &lcBinAdj..cLocation 
  lnRet = lfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[5],'',@laAdjust,laAdjust[10],;
                   '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
  SELECT WHBINLOC
  lcOldOrder = ORDER()
  SET ORDER TO Whbinloc
  SELECT (lcBatLin)
  SCAN FOR &lcBatLin..clocation = lcBinLoc
    IF SEEK(&lcBatLin..cwarecode + &lcBatLin..clocation+&lcBatLin..Style,'WHBINLOC') 
      SELECT WHBINLOC
      REPLACE Qty1   WITH MAX(&lcBatLin..stk1,0)  ,;
              Qty2   WITH MAX(&lcBatLin..stk2,0)  ,;
              Qty3   WITH MAX(&lcBatLin..stk3,0)  ,;
              Qty4   WITH MAX(&lcBatLin..stk4,0)  ,;
              Qty5   WITH MAX(&lcBatLin..stk5,0)  ,;
              Qty6   WITH MAX(&lcBatLin..stk6,0)  ,;
              Qty7   WITH MAX(&lcBatLin..stk7,0)  ,;
              Qty8   WITH MAX(&lcBatLin..stk8,0)  ,;
              TotQty WITH MAX(&lcBatLin..Totstk,0)
    ENDIF
  ENDSCAN
  SELECT WHBINLOC
  SET ORDER TO &lcOldOrder 
  SELECT MDINVNTL
ELSE
  lnRet=lfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
ENDIF
*-- End of Function lfDLSTYCRL.
*!***************************************************************************
*!* Name        : lfSTYCRL
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : A Copy of GfStyCrl to be Applicable with Bin Location
*!***************************************************************************
*!* Called from : ICINVLK.PRG 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSTYCRL()
*!***************************************************************************
FUNCTION lfSTYCRL
PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,laAdjStk,;
  lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,lnStarStep,lcTmpLFile,lcStepFld,;
  laGLInvAry,lnLineNo,lcLastRSess,lcAdjRef,laLockInfo, lnTranCost
*--Initialize function variables.
PRIVATE lcOldWAr,laOldstk,llChekUncmp,lnSAveCost,lnWAveCost,lnSOldStk,lnSOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lcTmpJour,lcInvJour,lnRetStep,llUInvtry,lnSStkVal,lnWStkVal,lnTranCost,lnStkVal,lnLineNo,;
        lcLastRSess,lcAdjRef,lnDyeCost,lnPrvQty,lnPrvVal

STORE 0 TO lnStkVal,lnTranCost,lnSaveCost,lnSOldStk,lnSOldCst,lnWaveCost,lnWOldStk,lnWOldCst,laOldstk

IF TYPE('lnLineNo') = 'L'
  lnLineNo = 0
ENDIF  
IF TYPE('lcLastRSess') = 'L'
  lcLastRSess = SPACE(6)
ENDIF  
IF TYPE('lcAdjRef') = 'L'
  lcAdjRef = SPACE(6)
ENDIF
IF !(TYPE('lcType') = 'U')
  lcBinLoc = &lcBinAdj..LocFrom
ENDIF  

lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P'
  PRIVATE llOpnWarhs 
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF SEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> gcCurSite
    IF llOpnWarhs
      USE IN STYINVJL
    ENDIF
    RETURN (1)
  ENDIF
ENDIF
IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

*--Style and Warehouse Average Cost,Old Stock and Old Cost variables.

DIME laOldstk[9]
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcSDyelot  = IIF(EMPTY(lcSDyelot),SPACE(10),lcSDyelot)

*--Check Uncomplete session flag if steps are passed as value not as zero.
llChekUncmp = ( lnStarStep <> 0 )    
*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,IIF(EMPTY(laGLInvAry[1,1]),.F.,.T.))
*--Return step to continue for after exit the function.
lnRetStep   = 0
*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_Cost_Meth')


*--Check the existing of the style and
*--Point the record in style and style dyelot files.
IF ! SEEK(lcStyle,'STYLE') OR !SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  *--The style ???? record are missing,
  *--Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  =gfModalGen('TRM42114B42000','DIALOG',lcStyle)
  RETURN (0)
ENDIF


*--Check if StyInvJL file is Open.
llOpnJurnl=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")

*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '123689I' , 'I' , 'R' )



*--Check the style Inventory Yes or No.
llUInvtry = STYLE.lInvSty
*--Get the Old Stock and Cost before updateing the new tansaction.

lnSOldStk = STYLE.TotStk
lnSOldCst = ABS(IIF(Style.TotStk=0,STYLE.Ave_Cost,STYLE.nStkVal / Style.TotStk))
lnWOldStk = STYDYE.TotStk

IF lcTrType = '9'
  lnWOldCst = IIF(laLockInfo[9]=0,0,laLockInfo[10]/laLockInfo[9])
ELSE
  lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,STYDYE.nStkVal / StyDye.TotStk))
ENDIF  

*--Stock Value variable for style and StyDye.
lnSStkVal = IIF(lcTrType $ '29',0,STYLE.nStkVal )
lnOldSVal = IIF(lcTrType $ '29',STYDYE.nStkVal,0)

IF lcTrType = '9'
  lnWStkVal = laLockInfo[10]
ELSE
  lnWStkVal = IIF(lcTrType $ '29',0,STYDYE.nStkVal)
ENDIF

PRIVATE lnDyeCost
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = StyDye.TotStk
lnPrvVal = StyDye.nStkVal
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  lnPrvQty = StyDye.TotStk
  lnPrvVal = StyDye.TotStk * lnDyeCost
ENDIF

IF lcIRType = 'I' AND (lcTrType <> 'I' OR lcCostMeth $ 'FL')

  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      lnNewCost = IIF((gfGetMemVar('M_WareHouse')='Y'),lnWOldCst ,lnSOldCst )

    CASE lcCostMeth = 'S'   && Standard.
      lnNewCost = STYLE.TotCost

    CASE lcCostMeth $ 'FL'  && FIFO or LIFO.
      *--In this case may be has more than cost so we hold this costs
      *--in lcTmpJour file that the following function will return.
      lcTmpJour = gfTempName()
      IF ! lfIsueCost(.F.)
        SELECT (lcOldWAr)
        RETURN (0)
      ENDIF
  ENDCASE
ENDIF

IF lcTrType = '9'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  SELECT STYDYE
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
ENDIF

*--Temp Old Stock Array used in Physical or Markdown 
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.

IF lcTrType $ '2'
  SELECT WHBINLOC
  IF !EMPTY(lcBinLoc)
    = SEEK(lcWareCode+lcBinLoc+lcStyle,'WHBINLOC')
  ENDIF
  FOR lnCnt = 1 TO 8 
    lcCnt = STR(lnCnt, 1)
    laOldstk[lnCnt] = WHBINLOC.Qty&lcCnt
  ENDFOR
  laOldstk[9] = WHBINLOC.TotQty
  lnOldSVal = laOldstk[9] * STYDYE.AVE_COST
  lnPrvQty  = laOldstk[9]
  lnPrvVal  = laOldstk[9] * STYDYE.AVE_COST
ENDIF

*--Calculate Avarage Cost for Style and StyDye records. 

*-- 1)  Update Style journal file. -------------------------------------
*--Read session no.
*--If receiving transaction and costing methos Lifo or Fifo make sure
*--that the session not duplecated fir same key.
lcRISessn = IIF( TYPE('lcRISessn')='C', lcRISessn ,'' )
IF lcIRType = 'R' AND lcCostMeth $ 'FL' AND !EMPTY(lcRISessn)
  lnJrlRec  = IIF(EOF('STYINVJL'),0,RECNO('STYINVJL'))
  lcRISessn = IIF(SEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  IF lnJrlRec<>0
    GOTO lnJrlRec IN STYINVJL
  ENDIF
ENDIF
*--Read session no.
IF EMPTY(lcRISessn)
  lcRISessn = gfSequence('GLSESSION')
ENDIF

*--Read the adjustment code reason to get the GL Account.
IF !EMPTY(lcAdjCdRsn)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjCdRsn , @laTrmRltFd , "CADJREASON")
ENDIF

*--Initialize next step to continue.
lnTmpStp = lnStarStep

*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I' AND lcCostMeth $ 'FL'

   lnIssTCst = 0
   lnIssTStk = 0

  SELECT (lcTmpJour)  
  SCAN
    REPLACE cSession  WITH lcRISessn,;
            cISession WITH cSession,;
            cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,cTrCode)
    SCATTER MEMVAR
    IF lfCheckUnCmp(lnTmpStp)      
      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR  
      lnIssTCst = lnIssTCst + m.nTotStk * m.nCost
      lnIssTStk = lnIssTStk + m.nTotStk
      REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
              cAdjReason WITH lcAdjCdRsn,;
              cAdjAcct   WITH lcAdjAcct,;
              nStkVal    WITH nTotStk * nCost,;
              LineNo     WITH lnLineNo,;
              nPrvSQty   WITH lnPrvQty,;
              nPrvSval   WITH lnPrvVal

      REPLACE cAdjRef    WITH lcAdjRef
      
      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')
      *--in case of Inventory lock.
      IF lcTrType='9'
        SCATT MEMVAR MEMO
        m.clocation   = lcBinLoc 
        INSERT INTO ('BININVJL') FROM MEMVAR
      ENDIF
      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  

      *--Call TraceKey global function.
      =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    ENDIF  
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    = lfStyWarDy()
  ENDSCAN
  IF lnIssTStk <> 0
    lnIssAvg = ABS(lnIssTCst / lnIssTStk )
  ELSE
    lnIssAvg = 0
  ENDIF  

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')
  ERASE (gcWorkDir+lcTmpJour+'.CDX')

ELSE  && Not LIFO or FIFO or Receiving.

  *--Create an issue record for Physical inventory or 
  *--Markdown inventory transaction in Style inventory Journal.

  IF lcTrType $ '29'
    IF lfDoPhys('I')
      =lfIsuJlTr()
      STORE 0 TO lnPrvQty,lnPrvVal
    ENDIF
  ENDIF

  IF !(lcTrType $ '29') AND lcIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    *-- This is to create 2 records in journal file
    *-- one for rec. the qty with it's old cost
    *-- the other for issue the qty with it's new cost
    = lfAdjRec()
  ENDIF

    *--Create a main record in journal file.
    IF !(lcTrType $ '29') OR (lcTrType $ '29' AND lfDoPhys('R'))          
      IF lfCheckUnCmp(lnTmpStp)      
        SELECT STYINVJL
        lnStkVal = laAdjStk[9] * lnNewCost
        APPEND BLANK
        REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH lnStkVal   ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal

        REPLACE cAdjRef    WITH lcAdjRef

        REPLACE nTranCost WITH lnTranCost
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *--in case of inventory lock
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc 
          INSERT INTO ('BININVJL') FROM MEMVAR
        ENDIF
        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      ENDIF
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()

    IF lcTrType = '9'
      = lfLkAdjRec()
    ENDIF
  ENDIF
ENDIF

*--Initialize next step to continue when return.
lnRetStep = lnTmpStp

*--Close style journal if this function open it.
IF llOpnJurnl AND USED("StyInvJl")
  USE IN STYINVJL
ENDIF

SELECT (lcOldWAr)
RETURN IIF(llChekUncmp , lnRetStep , 1 )
*-- End of Function lfSTYCRL.
*!*************************************************************
*!* Name      : lfUpdGLDist()
*!* Developer : Timour A. K.
*!* Date      : 01/22/98
*!* Module    : Inventory Control (IC)
*!* Purpose   : Update Temp G/L Distribution file.
*!*************************************************************
*:      [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:      [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:      [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:      [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*!*************************************************************
*! Call      : GLDIST
*!*************************************************************
*! Example   : =lfUpdGLDist()
*!*************************************************************
FUNCTION lfUpdGLDist

*-- llNegStkAd Showes if it is main record (Start)
*-- or it is adj. record because the stock is less than Zero
*-- AAMER 11/22/98
PARAMETERS llNegStkAd,llLockAdj

*-- llNegStkAd Showes if it is main record (End)
PRIVATE lnCurAlias

*--Donot update if no GL used.
IF ! llGLUsed
  RETURN
ENDIF

*-- This means it is Main Record (Start)

IF !llNegStkAd
*-- This means it is Main Record (End)

  *--Update Gl for Main inventory record for Isue or Receive.
  *- Receiving Trans.(+1,2,4,5,+6,7):    None
  *-  => +/-  lnAmount = Total Recv. Qty * New Recv. Cost     
  *- Issue Trans.(-1,-3,-6,-8,-2)     :  None
  *-  => +/-  lnAmount = Total Issue Qty * Issue Cost     
  FOR lnAln=1 TO ALEN(laGLInvAry,1)

    laGLInvAry[lnAln,5] = STYINVJL.cTrCode

    IF lfCheckUnCmp(lnTmpStp)
      
      lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]

      DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2],lnGLEnAmount,laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
                     IIF(llLockAdj,laLockInfo[11],laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7],laGLInvAry[lnAln,8]     ,;
                     laGLInvAry[lnAln,9],laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      DO CASE
        CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)

        *--update cadjact field in all cases 

        *--Updae cAdjAcct if it is empty in all catg_keys (Start) AAMER 04/13/99
        *--not if it is empty and catg_key = '007' 
        CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR EMPTY(StyInvJl.cAdjAcct)
        *--Updae cAdjAcct if it is empty in all catg_keys (End)
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)
      ENDCASE
    
      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  
    ENDIF
    lnTmpStp = lnTmpStp + 1
  ENDFOR

*-- This means it is Adj. Record (Start)
ELSE

  lcStyLink = IIF(EMPTY(StyDye.GL_Link),Style.Link_Code,StyDye.GL_Link)
   DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''                 
  lnCurAlias = SELECT(0)
  SELECT StyInvJl
  REPLACE cICAcnt WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1

  DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],;
                 StyInvJl.DtrDate),laGLInvAry[1,7],laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''
                 
  IF EMPTY(StyInvJl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT StyInvJl
    REPLACE cAdjAcct WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1
ENDIF
RETURN
*!***************************************************************************
*!* Name        : lfStyWarDy
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update the StyDye file
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfStyWarDy()
*!***************************************************************************
FUNCTION lfStyWarDy
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
PRIVATE llNew_Cost
llNew_Cost = .F. 

*-- what we do in case of avarage cost lcCostMeth = 'A' ??
IF lcIRType = 'R' .AND. !(lcTrType = '1' .AND. lcCostMeth $ 'FLA') .AND. !(lcTrType = '4' .AND. lcCostMeth $ 'FL' ) ;
  .AND. ( !(lcTrType $ '29') .OR. (lcTrType $ '29' AND lfDoPhys('R')))  
  llNew_Cost = .T.
ENDIF
lnValDiff=0
*--1 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYDYE
  =SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  =RLOCK()
  lnPrvStk  = TotStk   && Old Stock
  lnStkVal  = nStkVal  && Old Stock Value
  lnAveCost = Ave_Cost && Old Average Cost
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
  IF StyInvJl.nTotStk > 0                && Receive transaction
    IF lnPrvStk < 0                      && The stock was negative
      
      IF StyInvJl.nTotStk + lnPrvStk < 0   && The stock still negative after receiving
        IF llNew_Cost
          lnStkVal = lnStkVal + StyInvJl.nTotStk * lnNewCost
        ELSE
          lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost 
        ENDIF
      ELSE                               && Use the transaction cost if the stock will be > 0
        lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
      ENDIF
    ELSE
      lnStkVal = lnStkVal + (StyInvJl.nTotStk * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost))
    ENDIF
  ELSE                                   && Issue transaction
    IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
      lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
    ENDIF
    lnStkVal = TotStk * IIF(llNew_Cost,lnNewCost,IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk))
  ENDIF
  IF TotStk = 0 AND StyInvJl.nTotStk > 0
     lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
  ENDIF
  IF TotStk > 0
    lnAveCost = IIF(llNew_Cost,lnNewCost,lnStkVal/TotStk)
  ENDIF  
  IF lcIRType='I' AND !EMPTY(lcLastRSess)
    lnAveCost = IIF(TotStk=0,StyInvJl.nCost,lnStkVal/TotStk)
  ENDIF
  lnValDiff = lnStkVal - nStkVal
  REPLACE StyDye.nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
          StyDye.Ave_Cost WITH lnAveCost 
  UNLOCK 
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep)  
  *--Call TraceKey global function.
  =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
  lnPrvQty  = TotStk
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
ENDIF 
lnTmpStp = lnTmpStp + 1
*--2 ) Update Stock and Avarege cost in Style file. ------------
IF lfCheckUnCmp(lnStarStep+1)
  SELECT STYLE
  =RLOCK()
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH nStkVal + lnValDiff ,;
          Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
  UNLOCK 
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep+1)  
  *--Call TraceKey global function.
  =gfTraceKey('STYLE',STYLE.Style,'M')
ENDIF 
lnTmpStp = lnTmpStp + 1
*--3 )  Update Stock in Style Dyelot file Dyelot record. --------

*- in Case Of Reciving PO and the cost method is Avarage Cost
IF lcIRType = 'R' .AND. lcTrType = '6' .AND. lcCostMeth = 'A'
  =SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  REPLACE STYDYE.Ave_Cost WITH STYLE.Ave_Cost
ENDIF
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  IF lfCheckUnCmp(lnStarStep+2)
    SELECT STYDYE
    =RLOCK()
    REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
            Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
            Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
            Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
            Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
            Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
            Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
            Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
            TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
    UNLOCK 
    *--Update Uncomplete session Step.
    =lfUpdStep(lnStarStep+2)  
    *--Call TraceKey global function.
    =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
    lnPrvQty = TotStk
    lnPrvVal = TotStk * lnDyeCost
  ENDIF 
ENDIF
lnTmpStp = lnTmpStp + 1
SELECT(lnCurAlias)
*-- End of Function lfStyWarDy.
*!***************************************************************************
*!* Name        : lfDoPhys
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check if we can issue or receive in case of inventory Locking.
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : .T. OR .F.
*!***************************************************************************
*!* Example     : = lfDoPhys()
*!***************************************************************************
FUNCTION lfDoPhys
PARAMETERS lcRI
PRIVATE lcRI
lnRet = .F.
DO CASE
  CASE lcRI = 'I'
    *-- Issue if the old stock value or the old stock qty doen't equal zero 
    *-- or the cost value has changed or the balance has changed.
    lnRet = lnOldSVal   # 0 OR laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
            laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR lnWOldCst # lnNewCost OR ;
            laOldstk[9] # laAdjStk[9]
  CASE lcRI = 'R'
    *-- Receive only if the Issue record hasn't been issued or the new balance is not zero or cost value has changed
    lnRet = !(lnOldSVal   # 0 OR laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
              laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR lnWOldCst   # lnNewCost OR ;
              laOldstk[9] # laAdjStk[9]      )   OR laAdjStk[9] # 0 OR lnWOldCst   # lnNewCost                      
ENDCASE
RETURN lnRet
*-- End of Function lfDoPhys.
*!***************************************************************************
*!* Name        : lfLkAdjRec
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLkAdjRec()
*!***************************************************************************
FUNCTION lfLkAdjRec
PRIVATE lcJourTag,lnCurAlias,lnTotQty,lnTotVal,lnTranVal,lnDiffere
lnCurAlias = SELECT(0)
SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')
SET ORDER TO StyInvJl
lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
IF SEEK(lcStyle+lcWareCode,'StyInvJl')
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode=lcStyle+lcWareCode FOR cDyelot=lcSDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0 AND lfCheckUnCmp(lnTmpStp)
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn       ,;
            Style      WITH lcStyle         ,;
            cWareCode  WITH lcWareCode      ,;
            cDyelot    WITH lcSDyelot       ,;
            dTrDate    WITH laLockInfo[11]  ,;
            cTrType    WITH lcTrType        ,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
            cIRType    WITH IIF(lnDiffere<0,'I','R'),;
            nStkVal    WITH lnDiffere       ,;
            Reference  WITH 'Mark Down Adjustement Value',;
            cAdjReason WITH lcAdjCdRsn      ,;
            cAdjAcct   WITH lcAdjAcct       ,;
            cISession  WITH IIF(lnDiffere<0,cSession,''),;
            cRSession  WITH IIF(lnDiffere>0,cSession,''),;
            nPrvSQty   WITH lnPrvQty        ,;
            nPrvSVal   WITH lnPrvVal        ,;
            cAdjRef    WITH lcAdjRef        ,;
            nTranCost  WITH lnTranCost
    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    lnTmpStp = lnTmpStp + 1
    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF
SET ORDER TO (lcJourTag) IN StyInvJl
SELECT (lnCurAlias)
*-- End of Function lfLkAdjRec.
*!***************************************************************************
*!* Name        : lfIsuJlTr
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : This function will update the journal file with Issue transaction
*!*             : record(s) for Physical or markdown transactions only.  
*!*             : If the method is Standard or Average we create only one Issue record
*!*             : for old stock before physical transaction was done Else if the method
*!*             : is LIFO or FIFO we create issue records depends on all open receivings
*!*             : exist in journal.
*!***************************************************************************
*! Notes        : This Function Copied from Gfstycrl.prg with some modification
*!              : from Mohamed Shokry to be Suitable for the bin location system
*!***************************************************************************
*!* Called from : Binmain.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfIsuJlTr()
*!***************************************************************************
FUNCTION lfIsuJlTr
IF lcCostMeth $ 'FL'  && ISSUE FIFO or LIFO.
  *--Get the open receivings.
  lcTmpJour = gfTempName()
  =lfIsueCost(.T.)
  SELECT (lcTmpJour)
  SCAN
    IF nTotStk <> 0 OR nStkVal <> 0
      REPLACE cSession  WITH lcRISessn,cISession WITH cSession,cTrCode WITH IIF(cTrType $ "12",cSession,cTrCode)
      SCATTER MEMVAR       
      IF lfCheckUnCmp(lnTmpStp)
        SELECT STYINVJL
        APPEND BLANK
        GATHER MEMVAR
        REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
                cAdjReason WITH lcAdjCdRsn      ,;
                cAdjAcct   WITH lcAdjAcct       ,;
                nStkVal    WITH IIF(nTotStk = 0,nStkVal,nTotStk * nCost),;
                nPrvSQty   WITH lnPrvQty        ,;
                nPrvSval   WITH lnPrvVal        ,;
                cAdjRef    WITH lcAdjRef        ,;
                lLockFlg   WITH (lcTrType='9')  ,;
                nTranCost  WITH lnTranCost
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *-- in case of inventory lock
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc 
          INSERT INTO ('BININVJL') FROM MEMVAR
        ENDIF
        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      ENDIF  
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
  ENDSCAN
  USE
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')
ELSE
  IF lfCheckUnCmp(lnTmpStp)       
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn      ,;
            Style      WITH lcStyle        ,;
            cWareCode  WITH lcWareCode     ,;
            cDyelot    WITH lcSDyelot      ,;
            dTrDate    WITH ldTrDate       ,;
            cTrType    WITH lcTrType       ,;
            cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost      WITH lnWOldCst      ,;
            cIRType    WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession  WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession  WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1      WITH -(laOldstk[1]) ,;
            nStk2      WITH -(laOldstk[2]) ,;
            nStk3      WITH -(laOldstk[3]) ,;
            nStk4      WITH -(laOldstk[4]) ,;
            nStk5      WITH -(laOldstk[5]) ,;
            nStk6      WITH -(laOldstk[6]) ,;
            nStk7      WITH -(laOldstk[7]) ,;
            nStk8      WITH -(laOldstk[8]) ,;
            nTotStk    WITH -(laOldstk[9]) ,;
            nStkVal    WITH -(lnOldSVal)   ,;
            lLockFlg   WITH (lcTrType='9') ,;
            Reference  WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn     ,;
            cAdjAcct   WITH lcAdjAcct      ,;
            nPrvSQty   WITH lnPrvQty       ,;
            nPrvSVal   WITH lnPrvVal       ,;
            cAdjRef    WITH lcAdjRef       ,;
            nTranCost  WITH lnTranCost
    *-- Call global function to add audit fields info.
    =gfAdd_Info('STYINVJL')
    *-- in case of inventory lock.
    IF lcTrType='9'
      SCATT MEMVAR MEMO
      m.clocation   = lcBinLoc 
      INSERT INTO ('BININVJL') FROM MEMVAR
    ENDIF
    *--Update Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  
    *--Call TraceKey global function.
    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    lnTmpStp = lnTmpStp + 1
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()
  ENDIF
ENDIF  
RETURN
*-- End of Function lfIsuJlTr.
*!***************************************************************************
*!* Name        : lfIsueCost
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : This function will get the journal records for the receiving
*!*             : transactions that will be applied on the current Issue 
*!*             : transaction and take the cost of the receiving to be a new 
*!*             : cost of issueing,depends on cost method FIFO or LIFO.    
*!***************************************************************************
*! Notes        : This Function Copied from Gfstycrl.prg with some modification
*!              : from Mohamed Shokry to be Suitable for the bin location system
*!***************************************************************************
*!* Called from : BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfIsueCost()
*!***************************************************************************
FUNCTION lfIsueCost
PARAMETERS llForPhys
PRIVATE lcAlias,llContnu,laTotRcvd
lcAlias = ALIAS()
SELECT BININVJL
*--Create the Temp journal file with open receiving transactions.
SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,cIRType,cRSession,cISession,;
       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',SUM(nStk4) AS 'nStk4',;
       SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) as cAdjRef ;
   FROM  BININVJL ;
   WHERE Style + cWareCode + cDyelot + cRSession + cISession = lcStyle + lcWareCode + lcSDyelot ;
       AND clocation =  lcBinLoc;
   GROUP BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.cRSession ;
   ORDER BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.cRSession ;
   INTO DBF (gcWorkDir+lcTmpJour)

SELECT (lcTmpJour)
DELETE ALL FOR nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0

GO TOP
IF EOF()
  IF !llForPhys
    *--No open receiving exist for style XXXX , This transaction line will be ignored.
    =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
    USE
    RETURN .F.
  ELSE  && Get issue for Physical transactions.
    APPEND BLANK
    REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            nCost     WITH lnWOldCst,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH lcTrCode,;
            cIRType   WITH "I",;
            nStkVal   WITH lnOldSVal

    REPLACE cAdjRef    WITH lcAdjRef

    RETURN
  ENDIF 
ENDIF

*--For Not Phyical.
IF !llForPhys
  *--Indexing the file on Ascending or Descending expresion depends on LIFO or FIFO method.
  IF lcCostMeth = 'F'
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  ELSE
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
  ENDIF
  GO TOP


  *--Start checking the only needed open receinving transaction for this
  *--issue transaction and put zero for all not needed receivings.

  *--Array to Hold the accomulation of the receiving untill it cover the issue quantity needed.
  DIME laTotRcvd[9]
  laTotRcvd = 0      
  SCAN
    llContnu  = .F.
    FOR I=1 TO 8 
      Z=STR(I,1)
      IF ABS(laAdjStk[I]) > laTotRcvd[I]
        llContnu = .T.
        laTotRcvd[I] = laTotRcvd[I] + nStk&Z
        laTotRcvd[9] = nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        IF ABS(laAdjStk[I]) <= laTotRcvd[I]
          REPLACE nStk&Z  WITH nStk&Z - (laTotRcvd[I] - ABS(laAdjStk[I]))
          REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
          REPLACE lNeeded WITH .T.
        ELSE
          REPLACE lNeeded WITH .T.
        ENDIF   
      ELSE
        REPLACE nStk&Z  WITH 0
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE lNeeded WITH .T.
      ENDIF   
    ENDFOR 
    IF !llContnu 
      EXIT
    ENDIF
  ENDSCAN

  *--Check if all Issue quantity are covered by the receivings.
  IF llContnu 
    FOR I=1 TO 8 
      Z=STR(I,1)
      IF laAdjStk[I] < 0 and ABS(laAdjStk[I]) > laTotRcvd[I]
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        =gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        USE
        RETURN .F. 
      ENDIF
    ENDFOR
  ENDIF
  *--Delete all not needed receiving transactions.
  DELETE ALL FOR nTotStk = 0 OR !lNeeded
ENDIF

*--Change it to Issue transactions,to use it in updating master Journal file.
REPLACE ALL cIRType WITH "I"      ,;
            dTrDate WITH ldTrDate ,;
            cTrType WITH lcTrType ,;
            cTrCode WITH lcTrCode ,;
            nStk1   WITH -nStk1   ,;
            nStk2   WITH -nStk2   ,;  
            nStk3   WITH -nStk3   ,;  
            nStk4   WITH -nStk4   ,;  
            nStk5   WITH -nStk5   ,;  
            nStk6   WITH -nStk6   ,;  
            nStk7   WITH -nStk7   ,;  
            nStk8   WITH -nStk8   ,;  
            nTotStk WITH -nTotStk ,;
            nStkVal WITH -nStkVal

SELECT (lcAlias)
RETURN .T.
*-- End of Function lfIsueCost.
*!***************************************************************************
*!* Name        : lfAdjRec
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add Receiving record and Issuing record in StyInvJl.
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdjRec()
*!***************************************************************************
FUNCTION lfAdjRec
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn     ,;
          Style      WITH lcStyle       ,;
          cWareCode  WITH lcWareCode    ,;
          cDyelot    WITH lcSDyelot     ,;
          dTrDate    WITH ldTrDate      ,;
          cTrType    WITH '1'           ,;
          cTrCode    WITH lcRISessn     ,;
          nCost      WITH lnWOldCst     ,;
          cIRType    WITH "R"           ,;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-lnWStkVal,nTotStk * lnWOldCst),;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn    ,;
          cAdjAcct   WITH lcAdjAcct     ,;
          cRSession  WITH cSession      ,;
          nPrvSQty   WITH lnPrvQty      ,;
          nPrvSVal   WITH lnPrvVal      ,;
          cAdjRef    WITH lcAdjRef      ,;
          nTranCost  WITH lnTranCost

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*-- Iss. with the new cost
IF lfCheckUnCmp(lnTmpStp)   
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn      ,;
          Style      WITH lcStyle        ,;
          cWareCode  WITH lcWareCode     ,;
          cDyelot    WITH lcSDyelot      ,;
          dTrDate    WITH ldTrDate       ,;
          cTrType    WITH '1'            ,;
          cTrCode    WITH lcRISessn      ,;
          nCost      WITH lnNewCost      ,;
          cIRType    WITH "I",;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH nTotStk * lnNewCost,;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn     ,;
          cAdjAcct   WITH lcAdjAcct      ,;
          cISession  WITH cSession       ,;
          nPrvSQty   WITH lnPrvQty       ,;
          nPrvSVal   WITH lnPrvVal       ,;
          cAdjRef    WITH lcAdjRef       ,; 
          nTranCost  WITH lnTranCost
  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1
*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()
*-- End of Function lfAdjRec.
*!***************************************************************************
*!* Name        : lfUpdStep
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update uncomplete session step.
*!***************************************************************************
*!* Called from : Binmain.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdStep()
*!***************************************************************************
FUNCTION lfUpdStep
PARA lnCurntStep
PRIVATE lnAlas
IF llChekUncmp
  lnAlas = SELECT()
  SELECT (lcTmpLFile)
  =RLOCK()
  REPLACE &lcStepFld WITH lnCurntStep
  UNLOCK
  SELECT(lnAlas)
ENDIF
RETURN
*-- End of Function lfUpdStep.
*!***************************************************************************
*!* Name        : lfCheckUnCmp
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Function To Check uncomplete session Steps.
*!***************************************************************************
*!* Called from : BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : .T. for Check , .F. for Already checked no need to check.
*!***************************************************************************
*!* Example     : = lfCheckUnCmp()
*!***************************************************************************
FUNCTION lfCheckUnCmp
PARA lnStepNo
IF !llChekUncmp
   RETURN .T.
ELSE
  RETURN ( &lcTmpLFile..&lcStepFld < lnStepNo )
ENDIF
*-- End of Function lfCheckUnCmp.
*!***************************************************************************
*!* Name        : lfGetTrnQt
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Get Qty that will be transfered.
*!***************************************************************************
*!* Called from : ICBNRPL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetTrnQt()
*!***************************************************************************
FUNCTION lfGetTrnQt
PRIVATE lnOldAlias,lnStkQty,lcOldTag,lnTransQty
STORE '' TO lcOldTag
STORE 0 TO lnOldAlias,lnStkQty,lnTransQty
lnOldAlias = SELECT(0)
lcOldTag = ORDER(lcBinAdj)
SET ORDER TO TAG WHDVSTLOCF IN (lcBinAdj)
IF SEEK(STYDYE.CWARECODE+STYLE.CDIVISION+STYLE.STYLE+lcTmpBnFrm.CLOCATION,lcBinAdj)
  SELECT(lcBinAdj)
  SCAN REST WHILE CWARECODE+cDivision+STYLE+LocFrom = STYDYE.CWARECODE+STYLE.CDIVISION+STYLE.STYLE+lcTmpBnFrm.CLOCATION
    lnTransQty = lnTransQty + &lcBinAdj..TrnQty&LCZ
  ENDSCAN
  *--If I Transfered the all Stk Qty from this bin then I'll skip to another Bulk Bin.
  IF (lcTmpBnFrm.QTY&LCZ - lnTransQty) <=0
    SELECT(lnOldAlias)
    RETURN .F.
  ENDIF
  *--If still there is a qty not transfered , then I'll transfere it.
  IF (lcTmpBnFrm.QTY&LCZ - lnTransQty) > 0
    lnStkQty = lcTmpBnFrm.QTY&LCZ - lnTransQty
  ENDIF
ELSE
  lnStkQty = lcTmpBnFrm.QTY&LCZ
ENDIF

IF lnCycle = 1
  *-- if Bin Stock is greater than the Replenishment Qty but the Left Qty is less than or equal Replenishment Tolerance
  IF (lnStkQty-STYLE.NREPLQTY)<=STYLE.NREPLTOLER
    m.Adj&LCZ = lnStkQty
    IF m.Adj&LCZ>=STYLE.NREPLQTY
      llContinue =.F.
    ELSE	&& if Bin Stock is Less than the Replenishment Qty
      lnRemain = STYLE.NREPLQTY - m.Adj&LCZ
      lnCycle = lnCycle +1
    ENDIF    
  ELSE 	&& if Bin Stock is greater than the Replenishment Qty
    m.Adj&LCZ=STYLE.NREPLQTY
    llContinue =.F.
  ENDIF
ELSE	    && If lnCycle > 1 
  lnCycle = lnCycle +1
  IF lnRemain > lnStkQty
    m.Adj&LCZ = lnStkQty
    lnRemain  = lnRemain - lnStkQty
  ELSE
    m.Adj&LCZ = lnRemain
    llContinue =.F.
  ENDIF
ENDIF
SET ORDER TO TAG &lcOldTag IN (lcBinAdj)
SELECT(lnOldAlias)
RETURN .T.
*-- End of Function lfGetTrnQt.
*!***************************************************************************
*!* Name        : lfPrntRpt
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/05/2006
*!* Purpose     : Print report for the result of the Transfering.
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfPrntRpt()
*!***************************************************************************
FUNCTION lfPrntRpt
*-- Message : M42208 => 'the Bin Location Replenishment'
*-- Button  : B36010 => '<Preview>  <Print>  <Cancel>'
STORE 0 TO lnPrintRp
IF RECCOUNT(lcTmpFile)>0 
  DO WHILE lnPrintRp <>3
    lnPrintRp  = gfModalGen('QRM42208B42017','DIALOG','the Bin Location Replenishment')
    R_WIDTH    = 'N'
    lcRpName   = 'IC\ICBNRPL'
    lcSetConsl = SET('CONSOLE')    && save console setting
    DO CASE
      CASE  lnPrintRp =  1
        OGPlatForm   = 'WINDOWS'
        lcOGPlatForm = OGPlatForm
        gcDevice     = "SCREEN"
      CASE  lnPrintRp =  2
        lcRpName     = 'IC\ICBNRPL'
        OGPlatForm   = 'WINDOWS'
        lcOGPlatForm = OGPlatForm
        gcDevice     = "PRINTER"
        SET CONSOLE OFF
        IF !pSetup(.F.,.F.)
          SET DEVICE TO SCREEN
          RETURN
        ENDIF
      CASE  lnPrintRp =  3
        SET DEVICE TO SCREEN
        RETURN
    ENDCASE
    SELECT (lcTmpFile)
    GOTO TOP
    *--Print the Report
    DO CASE
      CASE gcDevice = "SCREEN"
        IF glHeader           
          REPORT FORM &gcRepHome.&lcRpName PREVIEW 
        ELSE
          REPORT  FORM &gcRepHome.&lcRpName  PREVIEW PLAIN        
        ENDIF  
      CASE gcDevice = "PRINTER"
        IF glHeader           
          REPORT FORM &gcRepHome.&lcRpName TO PRINTER NOCONSOLE NOEJECT  
        ELSE
          REPORT  FORM &gcSysHome.&lcRpName  TO PRINTER  NOEJECT NOCONSOLE PLAIN      
        ENDIF  
    ENDCASE  
    SET DEVICE TO SCREEN
    IF lnPrintRp != 1  
      EXIT
    ENDIF
    SET CONSOLE &lcSetConsl
  ENDDO
ENDIF
SET DEVICE TO SCREEN
WAIT CLEAR
RETURN
*-- End of Function lfPrntRpt.
