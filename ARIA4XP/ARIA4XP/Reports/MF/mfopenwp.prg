*:***************************************************************************
*: Program file  : MFOPENWP
*: Program desc. : OPEN WORK IN PROCESS REEPORT
*: System        : Aria4XP
*: Module        : MF
*: Developer     : Tarek Noaman (TNA)
*: Reference     : N132010
*: Date          : 5/7/2006
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFOPENWP
*:***************************************************************************
*:                   Option grid Filter contains 
*:01-Report Format                 <A>/<B>                            lcRpNO
*:02-Optional Title                IS                                 XTITLE
*:03-Status                        In List                            POSHDR.STATUS
*:04-Sort By                       IS                                 lnRpSrtCd
*:05-MFG for Labor Cost Calc.      IS                                 XMFG
*:06-Page Break by Cont.           IS                                 llrpPBrk
*:07-Contractor                    In List                            MFGOPRHD.CCONTCODE
*:08-Style                         In List                            STYLE.CSTYMAJOR
*:09-Cutting Ticket#               In List                            POSHDR.PO
*:10-Style Group                   In List                            STYLE.CSTYGROUP
*:11-Fabric                        In List                            STYLE.FABRIC
*:12-Season                        In List                            POSHDR.SEASON
*:13-Division                      In List                            POSHDR.CDIVISION
*:14-Entered Date                  Between                            POSHDR.ENTERED
*:15-Completion Date               Between                            POSHDR.COMPLETE
*:***************************************************************************
*:                         Tables Used
*:                        _____________
*:01- POSHDR
*:02- STYLE
*:03- MFGOPRHD
*:04- CUTPICK
*:05- CTKTBOM
*:***************************************************************************
*Modifications:
*B608009,1 MMT 03/21/07 Fix bug of error if completed cutting ticket chosen and make layout legal paper size T20061215.0013 
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[T20091106.0020]
*B609306,2 MMT 06/17/2010 Add new option for report Form A to Display total cost or Materials cost[T20091106.0020]
*E302753,1 SMA 09/14/2010 add new option to print either Order# or Cust PO# for the related C/T.[T20100818.0042]
*B609430,1 TMI 10/13/2010 Fix a problem that the estimated cost sometimes show 0 [T20100920.0020 ]
*:***************************************************************************
IF lcRpNO = 'B' .AND. EMPTY(XMFG)
  *-- Message 'You have to enter the MFG code'
  =gfModalGen('TRM00250B00000','DIALOG','MFG code')
  RETURN
ENDIF


lcStTime   = TIME()

IF  loOgScroll.llOGFltCh 

	STORE '' TO lcSelCond,lcTable,lcSort
	=lfCollData()
	*E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[BEGIN]
	*DIMENSION loOGScroll.laCRParams[6,2]
	DIMENSION loOGScroll.laCRParams[7,2]
	*E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[END]
loOGScroll.laCRParams[1,1]  = 'Layout'
loOGScroll.laCRParams[1,2]  = 'Format '+lcRpNO
loOGScroll.laCRParams[2,1]  = 'OpTitle'
loOGScroll.laCRParams[2,2]  = XTITLE
loOGScroll.laCRParams[3,1]  = 'sortby'
loOGScroll.laCRParams[3,2]  = lcSort
loOGScroll.laCRParams[4,1]  = 'ReportName'
loOGScroll.laCRParams[4,2]  = 'Open Work In Process'
loOGScroll.laCRParams[5,1]  = 'GroupBy'
loOGScroll.laCRParams[5,2]  = lnRpSrtCd
loOGScroll.laCRParams[6,1]  = 'PageBreak'
loOGScroll.laCRParams[6,2]  = llrpPBrk
*E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[BEGIN]
loOGScroll.laCRParams[7,1]  = 'Order/Custpo'
loOGScroll.laCRParams[7,2]  = lcRpPco
*E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[END]

ENDIF 
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FPOSHDR
GO TOP
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

IF RECCOUNT()=0
  *--There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF 

lcTmpFPO = loOgScroll.gfTempName()

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1] = oAriaApplication.WorkDir+lcTmpFPO+'.DBF'

SELECT FPOSHDR
COPY TO oAriaApplication.WorkDir + lcTmpFPO + ".DBF"
*COPY TO oAriaApplication.WorkDir + "FCUTTKTH.DBF"
=gfOpenFile(oAriaApplication.WorkDir + (lcTmpFPO),'','EX')
USE IN (lcTmpFPO)  &&close the file will display from to be opened exclusive

loOgScroll.lcOGLastForm = IIF(lcRpNO='A',"MFOPENWA","MFOPENWB")
loogScroll.cCROrientation = 'L'


=gfDispRe()


*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman	(TNA)
*! Date      : 5/7/2006
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[Start]
*DIMENSION laCost[10,2]
DIMENSION laCost[14,2]
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[End]
laCost[1,1]  = 'M_CMTYPE1 '
laCost[2,1]  = 'M_CMTYPE2 '
laCost[3,1]  = 'M_CMTYPE3 '
laCost[4,1]  = 'M_CMTYPE4 '
laCost[5,1]  = 'M_CMTYPE5 '
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[Start]
*!*  laCost[6,1]  = 'M_CMSLBL1 '
*!*  laCost[7,1]  = 'M_CMSLBL2 '
*!*  laCost[8,1]  = 'M_CMSLBL3 '
*!*  laCost[9,1]  = 'M_CMSLBL4 '
*!*  laCost[10,1] = 'M_CMSLBL5 '
laCost[6,1]  = 'M_CMTYPE6 '
laCost[7,1]  = 'M_CMTYPE7 '
laCost[8,1]  = 'M_CMSLBL1 '
laCost[9,1]  = 'M_CMSLBL2 '
laCost[10,1] = 'M_CMSLBL3 '
laCost[11,1] = 'M_CMSLBL4 '
laCost[12,1] = 'M_CMSLBL5 '
laCost[13,1] = 'M_CMSLBL6 '
laCost[14,1] = 'M_CMSLBL7 '
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[End]
=gfGetMemvar(@laCost,oAriaApplication.ActiveCompanyID)

*E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[BEGIN]
*CREATE CURSOR FPOSHDR (po C(6),account C(5),style C(19),status C(1),del_date D,pattern C(10),complete D, ;
                       contr1 C(8),contr2 C(8),contr3 C(8),mfg_opr1 C(6),mfg_opr2 C(6),mfg_opr3 C(6), ;
                       custpo C(10),cdivision C(6),ctkttype C(6),act_date D,vendor C(8),width C(6),entered D, ;
                       nstyorder N(7),cancel N(7),receive N(7),damage N(7),pcs_canold N(7),open N(7), ;
                       unt_cst N(7,3),desc C(20),LaborCst N(8,2),TotLabCst N(9,2))
*B609430,1 TMI 10/13/2010 [Start] increase the field unt_cst width from 7 to 13
*CREATE CURSOR FPOSHDR (po C(6),account C(5),style C(19),status C(1),del_date D,pattern C(10),complete D, ;
                       contr1 C(8),contr2 C(8),contr3 C(8),mfg_opr1 C(6),mfg_opr2 C(6),mfg_opr3 C(6), ;
                       custpo C(10),cdivision C(6),ctkttype C(6),act_date D,vendor C(8),width C(6),entered D, ;
                       nstyorder N(7),cancel N(7),receive N(7),damage N(7),pcs_canold N(7),open N(7), ;
                       unt_cst N(7,3),desc C(20),LaborCst N(8,2),TotLabCst N(9,2),Order C(6))                      
CREATE CURSOR FPOSHDR (po C(6),account C(5),style C(19),status C(1),del_date D,pattern C(10),complete D, ;
                       contr1 C(8),contr2 C(8),contr3 C(8),mfg_opr1 C(6),mfg_opr2 C(6),mfg_opr3 C(6), ;
                       custpo C(10),cdivision C(6),ctkttype C(6),act_date D,vendor C(8),width C(6),entered D, ;
                       nstyorder N(7),cancel N(7),receive N(7),damage N(7),pcs_canold N(7),open N(7), ;
                       unt_cst N(13,3),desc C(20),LaborCst N(8,2),TotLabCst N(9,2),Order C(6))                      
*B609430,1 TMI 10/13/2010 [End  ] 
*E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[END]
                                              
SELECT FPOSHDR
DO CASE
  CASE lnRpSrtCd = 1
    INDEX ON ACCOUNT+DTOS(DEL_DATE)+PO TAG POSHDRTEMP
    lcSort="Account+Customer Delivery Date"
  CASE lnRpSrtCd = 2
    INDEX ON PO TAG POSHDRTEMP
    lcSort="Cutting Ticket #"
  CASE lnRpSrtCd = 3
    INDEX ON CONTR1+PO TAG POSHDRTEMP
    lcSort="First Cont. + CUTTKT #"
  CASE lnRpSrtCd = 4
    INDEX ON PATTERN+PO TAG POSHDRTEMP
    lcSort="Pattern"
  CASE lnRpSrtCd = 5
    INDEX ON CONTR2+DTOS(COMPLETE)+PO TAG POSHDRTEMP
    lcSort="Second Cont. + Completion Date"
  CASE lnRpSrtCd = 6
    INDEX ON CONTR2+PO TAG POSHDRTEMP
    lcSort="Second Contractor + CUTTKT #"
  CASE lnRpSrtCd = 7
    INDEX ON CONTR1+STATUS+PO TAG POSHDRTEMP
    lcSort="First Contractor + Status"
ENDCASE

lcSelFld  = "poshdr.po,poshdr.account,poshdr.style,poshdr.status,poshdr.del_date,poshdr.pattern,poshdr.complete,"
lcSelFld  = lcSelFld + "poshdr.custpo,poshdr.cdivision,poshdr.ctkttype,poshdr.act_date,poshdr.entered,poshdr.season,"
lcSelFld  = lcSelFld + "poshdr.nact_cost1,poshdr.nact_cost2,poshdr.nact_cost3,poshdr.nact_cost4,poshdr.nact_cost5,"
lcSelFld  = lcSelFld + "poshdr.nstyorder,poshdr.cancel,poshdr.receive,poshdr.damage,poshdr.pcs_canold,pcs_act,poshdr.[open]"
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[Start]
lcSelFld  = lcSelFld +",poshdr.nact_cost6,poshdr.nact_cost7,poshdr.NICOST1,"+;
            "poshdr.NICOST2,poshdr.NICOST3,poshdr.NICOST4,poshdr.NICOST5,poshdr.NICOST6,poshdr.NICOST7"
*B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[End]
lcTable          = "POSHDR"
lcWhereCondition = lfCreateWhereCondition()
lcSelCond        = " POSHDR.CBUSDOCU ='P' AND POSHDR.CSTYTYPE ='U' " + lcWhereCondition
=lfGetJoins()

lfopensql(lcSelFld,lcTable,'TPOSHDR',lcSelCond)

=lfCreateTables()

*!*	=gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')

*B608009,1 MMT 03/21/07 Fix bug of error if completed cutting ticket chosen and make layout legal paper size[Start]
SELECT TPOSHDR
LOCATE
IF !EOF() 
*B608009,1 MMT 03/21/07 Fix bug of error if completed cutting ticket chosen and make layout legal paper size[End]

  SELECT TPOSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET RELATION TO 'M'+PO INTO CTKTBOM ADDITIVE

  SELECT CTKTBOM
  SET RELATION TO SUBSTR(ITEM,1,7) INTO TITEM ADDITIVE

*B608009,1  MMT 03/21/07 Fix bug of error if completed cutting ticket chosen and make layout legal paper size[Start]
ENDIF 
*B608009,1 MMT 03/21/07 Fix bug of error if completed cutting ticket chosen and make layout legal paper size[End]

lcTmpOPH = loOgScroll.gfTempName()
CREATE CURSOR (lcTmpOPH) (CTKTNO C(6),cOperSeq C(2),cOprCode C(6),cContCode C(8))
INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH

lcRpExp=STRTRAN(lcRpExp,'POSHDR','TPOSHDR')
SELECT TPOSHDR
SCAN FOR EVALUATE(lcRpExp)
  XTOT_PCS=0
  DO CASE
    CASE STATUS='A'
      XTOT_PCS=PCS_ACT
    OTHERWISE
      XTOT_PCS=NSTYORDER - CANCEL
  ENDCASE 
  lnActCst = 0
  *B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[Start]
  *FOR lnI=1 TO 5  
  FOR lnI=1 TO 7
  *B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[End]
    *B609306,2 MMT 06/17/2010 Add new option for report Form A to Display total cost or Materials cost[Start]
    *IF INLIST(laCost[lnI,2],'F','T')
    IF IIF(lcRpNO = 'A' AND lcRpUntCst = 'T',.T.,INLIST(laCost[lnI,2],'F','T'))
    *B609306,2 MMT 06/17/2010 Add new option for report Form A to Display total cost or Materials cost[End]
      lcZ = STR(lnI,1)
      *B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[Start]
      IF lcRpPrnCst = 'A'
      *B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[End]
      lnActCst = lnActCst + nAct_Cost&lcZ
      *B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[Start]
      ELSE
        lnActCst = lnActCst + NICOST&lcZ      
      ENDIF
      *B609306,1 MMT 06/17/2010 Open WIP report - doesn't show the Total cost[End]
    ENDIF
  ENDFOR
  m.unt_cst = IIF(XTOT_PCS<>0,ROUND(lnActCst/XTOT_PCS,3),0)

  IF lcRpNO='B'
    STORE 0 TO XLAB_CST,XTOT_LAB_CST
    SELECT CTKTBOM
    SEEK 'M'+TPOSHDR.PO
    LOCATE REST WHILE cIMTyp+Cuttkt='M'+TPOSHDR.PO FOR MFGCode=xMFG
    IF FOUND()
      XLAB_CST    = UNTCOST  * UNTQTY
      XTOT_LAB_CST= XLAB_CST * PIECES
    ELSE
      SELECT BOM
      SEEK TPOSHDR.STYLE
      LOCATE REST WHILE ITEM=TPOSHDR.STYLE FOR MfgCode=XMFG
      IF FOUND()
        XLAB_CST    = UNTCOST  * nBomTOTQTY
        XTOT_LAB_CST= XLAB_CST * XTOT_PCS
      ENDIF
    ENDIF
    m.LaborCst  = XLAB_CST
    m.TotLabCst = XTOT_LAB_CST
  ENDIF

  IF SEEK('M'+TPOSHDR.PO,'MFGOPRHD')
    SELECT (lcTmpOPH)
    ZAP
    SELECT MFGOPRHD
    SCAN WHILE cTktNo = TPOSHDR.PO
      SCATTER FIELDS CTKTNO,cOperSeq,cOprCode,cContCode MEMVAR
      INSERT INTO (lcTmpOPH) FROM MEMVAR
    ENDSCAN
    SELECT (lcTmpOPH)
    GO TOP
    IF !EOF()
      lnCount = 1
      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3                        
      SCAN
        lcCOUNT = STR(lnCount,1)
        IF lnCount <= 3 
          m.CONTR&lcCount   = cContCode
          m.MFG_OPR&lcCount = COPRCODE
        ENDIF
        lnCount = lnCount+1
      ENDSCAN
    ELSE
      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
    ENDIF
  ELSE
    STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, m.MFG_OPR2, m.MFG_OPR3
  ENDIF

  SELECT TPOSHDR
  lcolddate=SET("Date" )
  SET DATE AMERICAN 
  SCATTER MEMVAR MEMO FIELDS EXCEPT CONTR1,CONTR2,CONTR3,MFG_OPR1,MFG_OPR2,MFG_OPR3
  m.Account = SPACE(0)
  m.CustPo = SPACE(0)
  m.Del_Date = {  /  /  }
  *E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[BEGIN]
  m.order= SPACE(0)
  *E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[END]
  =lfAdCustDt()
  INSERT INTO FPOSHDR FROM MEMVAR
  IF lcRpNO='A'
    REPLACE FPOSHDR.VENDOR WITH TITEM.VENDOR
    REPLACE FPOSHDR.WIDTH WITH TITEM.CITEMFLD1
  ELSE
    REPLACE FPOSHDR.DESC WITH SUBSTR(STYLE.DESC,1,12)
  ENDIF
  SET DATE &lcolddate

ENDSCAN


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.


*!*************************************************************
*! Name      : lfvFormAB
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : to enable or disable the MFG operation due to the form 
*!             (A or B)
*!*************************************************************
*! Called from : MFOPENWP.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : =lfvFormAB()
*!*************************************************************
FUNCTION lfvFormAB
*-- Get the position of the MFG operation in the array to enable or disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'XMFG'),1)
laOGObjCnt[lnPos] = lcRpNO = 'B'
XMFG = IIF(lcRpNO = 'A','',XMFG)
= lfOGShowGet('XMFG')



*!*	lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPPBRK'),1)
*!*	laOGObjCnt[lnPos] = lnRpSrtCd = 5
*!*	= lfOGShowGet('llrpPBrk')

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 02/04/99
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

SET ORDER TO TAG CSTYLE IN STYLE
*-- Get the position of the MFG operation in the array to enable or disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'XMFG'),1)
laOGObjCnt[lnPos] = lcRpNO = 'B'
= lfOGShowGet('XMFG')

lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPPBRK'),1)
laOGObjCnt[lnPos] = lnRpSrtCd = 5
= lfOGShowGet('llrpPBrk')


*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 02/04/99
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg
STORE 0 TO lnFPos, lnDPos, lnZPos, lnGPos, lnCPos, lnOPos, lnTPos, ;
           lnQPos, lnSPos

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO lnMajSeg
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    EXIT
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfvSrtBy
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : If sorting by 'Second cont.+Completion date' Enable 
*!             "Page break by cont." setting
*!*************************************************************
*! Called from : MFOPENWP.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : =lfvSrtBy()
*!*************************************************************
FUNCTION lfvSrtBy
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPPBRK'),1)
laOGObjCnt[lnPos] = lnRpSrtCd = 5
= lfOGShowGet('llrpPBrk')


*!*************************************************************
*! Name      : lfAdCustDt
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/07/2001
*! Purpose   : Add Customer , Cust. PO# and Del date to WORKFILE
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfAdCustDt()
*!*************************************************************
FUNCTION lfAdCustDt
PRIVATE lnAlias, llMultiAct
lnAlias = SELECT(0)
StORE .F. TO llMultiAct

IF gfSEEK('1'+PO,'CUTPICK')
  SELECT CUTPICK
  SCAN REST WHILE ctktno = TPOSHDR.PO
    IF SEEK('O'+ORDER,'ORDHDR')
      IF !(m.Account==ORDHDR.ACCOUNT)
        IF EMPTY(m.Account)
          m.Account = ORDHDR.ACCOUNT
          m.CustPo = ORDHDR.CUSTPO
          m.Del_Date = ORDHDR.COMPLETE
          *E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[BEGIN]
          m.Order= ORDHDR.Order
          *E302753,1 SMA 09/14/2010 add new option to print Order# for the related C/T.[END]
        ELSE
          llMultiAct = .T.
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
IF llMultiAct
  m.Account = 'MULTI'
  m.CustPo = 'MULTI'
  
  m.Del_Date = {  /  /  }
ENDIF
SELECT (lnAlias)
*-- end of lfAdCustDt.



*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 06/30/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql
LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(!EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")
lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))
IF lnConnectionHandlar = 1
  lnResult = loOGScroll.oRDA.mCloneStandardCursor (lcCursor,SET("DATASESSION"))
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCreateWhereCondition
*! Developer : Tarek Noaman	(TNA)
*! Date      : 5/7/2006
*! Purpose   : Creates Where Conditions
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCreateWhereCondition
PRIVATE lcWhere
lcWhere=""

lcStatus = lfCheckFilter(1,'POSHDR.STATUS')
IF !EMPTY(lcStatus)
  lcStatus = STRTRAN(lcStatus,"|","','")
  lcWhere = " AND Poshdr.Status IN ('" + lcStatus +"')"
ELSE
  lcWhere = " AND Poshdr.Status IN ('A','O','H','S')"
ENDIF

lcDivision = lfCheckFilter(1,'POSHDR.CDIVISION')
IF !EMPTY(lcDivision)
  lcDivision = STRTRAN(lcDivision,"|","','")
  lcWhere=lcWhere + " AND Poshdr.Cdivision IN ('"+ lcDivision +"')"
ENDIF

lcseason = lfCheckFilter(1,'POSHDR.SEASON')
IF !EMPTY(lcseason)
  lcseason = STRTRAN(lcseason,"|","','")
  lcWhere=lcWhere + " AND Poshdr.Season IN ('"+ lcseason +"')"
ENDIF 

lcDate = lfCheckFilter(1,'POSHDR.COMPLETE')
IF !EMPTY(lcDate)
  lcDate  = STRTRAN(lcDate,"|",",")
  lcStart = SUBSTR(lcDate,1,AT(",",lcDate)-1)
  lcEnd   = SUBSTR(lcDate,AT(",",lcDate)+1)
  lcWhere=" AND Poshdr.Complete between '"+lcStart+"' AND '"+lcEnd+"'"
ENDIF 

lcDate = lfCheckFilter(1,'POSHDR.ENTERED')
IF !EMPTY(lcDate)
  lcDate  = STRTRAN(lcDate,"|",",")
  lcStart = SUBSTR(lcDate,1,AT(",",lcDate)-1)
  lcEnd   = SUBSTR(lcDate,AT(",",lcDate)+1)
  lcWhere=" AND Poshdr.Entered between '"+lcStart+"' AND '"+lcEnd+"'"
ENDIF 

RETURN (lcWhere)

*!*************************************************************
*! Name      : lfCheckFilter
*! Developer : Tarek Noaman	(TNA)
*! Date      : 5/7/2006
*! Purpose   :
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
lcReturn = ""
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ENDIF
ENDCASE
RETURN lcReturn

*!*************************************************************
*! Name      : lfGetJoins
*! Developer : Tarek Noaman	(TNA)
*! Date      : 5/7/2006
*! Purpose   : Get Join Conditions
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfGetJoins

lcTempCuttktNo = lfCheckFilter(1, 'POSHDR.PO')
llCuttktNo     = !EMPTY(lcTempCuttktNo) AND USED(lcTempCuttktNo) AND RECCOUNT(lcTempCuttktNo) > 0
IF llCuttktNo
  lcCurName = lcTempCuttktNo
  SELECT &lcCurName
  lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
  lcSelCond  = lcSelCond + " AND POSHDR.PO = "+lcSQLOrder+'.PO'
  lcTable    = lcTable + ' INNER JOIN '+lcSQLOrder +' ON POSHDR.PO='+lcSQLOrder+'.PO'
ENDIF

lcTempStyle = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llStyle     = !EMPTY(lcTempStyle) AND USED(lcTempStyle) AND RECCOUNT(lcTempStyle) > 0
IF llStyle
  lcCurName = lcTempStyle
  SELECT &lcCurName
  lcSQLOrder = loOgScroll.gfSQLTempName('','Style C(19)',lcCurName,'Style')
  lcSelCond  = lcSelCond + " AND POSHDR.STYLE = "+lcSQLOrder+'.Style'
  lcTable    = lcTable + ' INNER JOIN '+lcSQLOrder +' ON POSHDR.STYLE='+lcSQLOrder+'.STYLE'
ENDIF

lcTempCont  = lfCheckFilter(1, 'MFGOPRHD.CCONTCODE')
llCont      = !EMPTY(lcTempCont) AND USED(lcTempCont) AND RECCOUNT(lcTempCont) > 0
IF llCont
  lcCurName = lcTempCont
  SELECT &lcCurName
  lcSQLOrder = loOgScroll.gfSQLTempName('','CContCode C(8)',lcCurName,'CContCode')
  lcSelCond  = lcSelCond + " AND POSHDR.PO = MFGOPRHD.CTKTNO AND MFGOPRHD.CCONTCODE= "+lcSQLOrder+'.CContCode'
  lcTable    = lcTable + ' INNER JOIN MFGOPRHD ON POSHDR.PO=MFGOPRHD.CTKTNO INNER JOIN '+lcSQLOrder +' ON MFGOPRHD.CCONTCODE='+lcSQLOrder+'.CCONTCODE'
ENDIF
RETURN


*!*************************************************************
*! Name      : lfCreateTables
*! Developer : Tarek Noaman	(TNA)
*! Date      : 5/7/2006
*! Purpose   : Create wanted tables from SQL
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCreateTables
SELECT TPOSHDR
SELECT DISTINCT PO FROM TPOSHDR INTO CURSOR TMPPO
lcCurName = "TMPPO"
SELECT &lcCurName
LOCATE 
IF !EOF()
  lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')

  *--Create MFGOPRHD
  lcSelFld     = "mfgoprhd.cimtyp,mfgoprhd.ctktno,mfgoprhd.coperseq,mfgoprhd.coprcode,mfgoprhd.ccontcode"
  lcTable      = 'MFGOPRHD INNER JOIN '+lcSQLOrder +' ON MFGOPRHD.CTKTNO='+lcSQLOrder+'.PO'
  lcSelCond    = " MFGOPRHD.CIMTYP='M' "
  lfopensql(lcSelFld,lcTable,'MFGOPRHD',lcSelCond)
  SELECT MFGOPRHD
  INDEX ON CIMTYP+CTKTNO TAG MFGIDX

  *--Create CTKTBOM
  lcSelFld     = "ctktbom.cimtyp,ctktbom.cuttkt,ctktbom.item,ctktbom.untcost,ctktbom.untqty,ctktbom.pieces,ctktbom.mfgcode"
  lcTable      = 'CTKTBOM INNER JOIN '+lcSQLOrder +' ON CTKTBOM.CUTTKT='+lcSQLOrder+'.PO'
  lcSelCond    = " CTKTBOM.CIMTYP='M' "
  lfopensql(lcSelFld,lcTable,'CTKTBOM',lcSelCond)
  SELECT CTKTBOM
  INDEX ON CIMTYP+CUTTKT TAG CTKTBOMIDX

  *--Create CUTPICK
  lcSelFld     = "CUTPICK.TRANCD,cutpick.ctktno,CUTPICK.[order]"
  lcTable      = 'CUTPICK INNER JOIN '+lcSQLOrder +' ON CUTPICK.CTKTNO='+lcSQLOrder+'.PO'
  lcSelCond    = " CUTPICK.TRANCD='1' "
  lfopensql(lcSelFld,lcTable,'CUTPICK',lcSelCond)
  SELECT CUTPICK
  INDEX ON TRANCD+CTKTNO TAG CUTPIKIDX
  
  *--Create Item
  lcSelFld     = "item.style,item.vendor,item.citemfld1"
  lcTable      = 'ITEM'
  lcSelCond    = " ITEM.cinvtype='0002' "
  lfopensql(lcSelFld,lcTable,'TITEM',lcSelCond)
  SELECT TITEM
  INDEX ON style  TAG ITEMIDX

  IF lcRpNO='B'
    *--Create BOM
    lcSelFld     = "bom.item,bom.mfgcode,bom.UNTCOST,bom.nBomTOTQTY"
    lcTable      = 'BOM'
    lcSelCond    = ""
    lfopensql(lcSelFld,lcTable,'BOM',lcSelCond)
    SELECT BOM
    INDEX ON ITEM TAG BOMIDX
  ENDIF

ENDIF
RETURN