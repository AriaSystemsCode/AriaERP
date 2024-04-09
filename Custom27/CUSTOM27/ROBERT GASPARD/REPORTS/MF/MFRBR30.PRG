*:************************************************************************
*: Program file  : MFRBR30.PRG
*: Program desc. : TRAKIGN TICKT REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : AHMED M. REDA (ARD)
*: Date          : 02/01/2000
*:************************************************************************
*: Example : DO MFRBR30
*:************************************************************************
*:C200093,1
*Modifications
*B803342,5 ARH 06/14/2000 (Begin) Fixed the bug Data type mismatch
*B603772,1 SSH add Estimate houres to the insert command
*: B#603835,1 SSH 28/08/2000 Add new field for rework.
*:************************************************************************

lcStTime   = TIME()
m.est_act=0       && initiate variables
*--Creating Temp File AND SETTING THE INDEX [Begin]
*-- if user change filter criteria then you must collect data again
IF llOGFltCh
  *-- if you have previous data clear work files then recreate it. [begin]
  IF !USED(lcTmpTrakh) OR (RECCOUNT(lcTmpTrakh) > 0)
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear work files then recreate it. [End  ]

  *-- Collect Report data [Start]
  =lfCollect()  && Collect New data
ENDIF
*: B#603835,1 SSH 28/08/2000 Add new field for rework.
=gfOpenFile(gcDataDir+'Trstrend','Trstrend','SH')
*: B#603835,1 SSH 

SELECT (lcTmpTrakh)
*-- Change index if user change sort by. [Begin]
GO TOP
IF !SEEK(&lcIndKey)
  REPLACE ALL cTempKey WITH EVALUATE(lcIndKey)
ENDIF
*-- Change index if user change sort by. [End  ]
*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s), ' +  'in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Second(s)...' NOWAIT
DO gfDispRe WITH EVAL('lcRpForm')
*: B#603835,1 SSH 28/08/2000 use in the file
USE IN Trstrend
*: B#603835,1 SSH 
*-- end of report code.

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Generate Work File.
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfWorkFile
PRIVATE laTempStru
IF USED(lcTmpTrakh)
  USE IN (lcTmpTrakh)
ENDIF
*-- cTempKey : A new field to sort and group on it.

*--Creating Temp File AND SETTING THE INDEX [Begin]

*B803342,5 ARH 06/14/2000 (Begin) Adjusted the size of the field ISSUE_QTY to fix
*B803342,5 ARH            the bug Data type mismatch
*CREAT CURSOR (lcTmpTrakh) ;
  (cTempKey C(19) , CUTTKT C(6),COPRCODE C(6),STATUS C(1),ID C(12),STYLE C(19),FLAG C(1),;
   ISSUE_QTY N(12),NQTY_PEN N(12),NQTY_COMP N(12),EST_HRS N(5,2), EST_ACT N(5,2),NHOURS  N(5,2),;
   DISS_DATE D(10),DENDATE D(10), TYPE C(1))

CREAT CURSOR (lcTmpTrakh) ;
  (cTempKey C(19) , CUTTKT C(6),COPRCODE C(6),STATUS C(1),ID C(12),STYLE C(19),FLAG C(1),;
   ISSUE_QTY N(12,3),NQTY_PEN N(12),NQTY_COMP N(12),EST_HRS N(5,2), EST_ACT N(5,2),NHOURS N(5,2),;
   DISS_DATE D(10),DENDATE D(10), TYPE C(1))
*B803342,5 ARH 06/14/2000 (End)

SELECT (lcTmpTrakh)
ZAP
INDEX ON cTempKey TAG (lcTmpTrakh) OF (gcWorkDir+lcTmpTrakh+'.CDX')
*--Creating Temp File AND SETTING THE INDEX [End]
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Collecting Report data
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfCollect
SELECT TRAKHDR
SCAN FOR &lcRpExp
  WAIT WINDOW "Collecting data For Cutting Ticket # " +  CUTTKT + ". Please Wait." NOWAIT
  SCATTER MEMVAR MEMO

  *--
  =lfvSortBy()
  *--

  m.cTempKey = &lcIndKey
  *-- Calculated fields [Begin]
  m.EST_HRS = lfGetEsh()  && Estimated Hours
  *-- Calculated fields [End  ]
  
  *B803342,5 ARH 06/14/2000 (Begin) Fixed the bug Data type mismatch  
  *INSERT INTO (lcTmpTrakh) FROM MEMVAR
  *B603772,1 SSH add Estimate houres to the insert command
  INSERT INTO (lcTmpTrakh) (Cuttkt, cOprCode, Status, ID, Style, Flag, Issue_Qty, nQty_Pen,;
                            nQty_Comp, nHours, dIss_Date, dEnDate, Type , EST_HRS);
                            VALUES (TrakHdr.Cuttkt, TrakHdr.cOprCode, TrakHdr.Status, TrakHdr.ID,;
                                   TrakHdr.Style, TrakHdr.Flag, TrakHdr.Issue_Qty, TrakHdr.nQty_Pen,;
                                   TrakHdr.nQty_Comp, TrakHdr.nHours, TrakHdr.dIss_Date, TrakHdr.dEnDate,;
                                   TrakHdr.Type , m.EST_HRS)
  *B803342,5 ARH 06/14/2000 (End)

ENDSCAN
*-- end of lfCollect.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : .
*!*************************************************************
*! Called from : MFRBR30.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime

*!*************************************************************
*! Name      : lfvCutNum
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Validation function for cut teckit#.
*!*************************************************************
*! Called from : Cut Teckit # [Option Grid]
*!*************************************************************
*! Calls       : lfvCutNum()
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCutNum
PRIVATE lcVar , lcObj , laTemp,lcBrowFields, llWoNotFnd
lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
lcPrevAl = SELECT(0)
STORE '' TO lcBrowCond

SELECT TRAKHDR
SET ORDER TO TRAKHDR

llWoNotFnd = ('?' $ lcObj) OR !(SEEK('C'+lcObj) OR SEEK('P'+lcObj) OR SEEK('M'+lcObj))
DIMENSION laTemp[1]
*-- Setting the fields for browsing [Begin]
IF !EMPTY(lcObj) AND llWoNotFnd
  lcBrFields ="cTYPE=IIF(Type='C','Cut Ticket',IIF(Type='P','Purchase Order','Misc.')) :R :H='Type' :20,"+;
              "CUTTKT    :R :H='Work Order'     ,"+;
              "STYLE     :R :H='Style'          ,"+;
              "ID        :R :H='User ID'        ,"+;
              "COPRCODE  :R :H='Operation code' ,"+;
              "DISS_DATE :R :H='Start Date'     ,"+;
              "DENDATE   :R :H='End Date'       ,"+;
              "STATUS    :R :H='Status'"

  =gfBrows(lcBrowCond,'CUTTKT','laTemp')
  lcObj = IIF(EMPTY(laTemp[1]),"",laTemp[1])
ENDIF
*-- Setting the fields for browsing [End  ]

*-- IF The user selected a record
&lcVar = lcObj
SELECT (lcPrevAl)
*-- End Of lfvCutNum

*!*************************************************************
*! Name      : lfvItmNum
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Validation function for Validating Of Item #
*!*************************************************************
*! Called from : Item # [Option Grid]
*!*************************************************************
*! Calls       : lfvItmNum() 
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvItmNum
PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*-- End Of lfvItmNum

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : When function to restore the old value.
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfWOldVal
laOldVal = EVALUATE(SYS(18))
*-- END of lfWOldVal.

*!*************************************************************
*! Name      : lfWOgrid
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 01/02/2000
*! Purpose   : Opton Grid When Function
*!*************************************************************
*! Called from : [Option Grid]
*!*************************************************************
*! Calls       : lfWOgrid()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfWOgrid
=lfvSortBy()
*-- End Of lfWOgrid


*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : To prepare the type of sorting for the report.
*!*************************************************************
*! Called from : lfwogrid()
*!*************************************************************
*! Calls       : lfvSortBy()
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvSortBy
DO CASE
  *-- Case sort by Work Order
  CASE lcRpSortBy ='W'
    lcSrtBy  ='Work Order'
    lcIndKey = [CUTTKT]

  *-- Case sort by Operation
  CASE lcRpSortBy ='O'
    lcSrtBy ='Operation'
    lcIndKey = [COPRCODE]

  *-- Case sort by User Id
  CASE lcRpSortBy ='I'
    lcSrtBy ='User ID'
    lcIndKey = [ID]

  *-- Case sort by Style
  CASE lcRpSortBy ='N'
    lcSrtBy  = lcMajItm
    lcIndKey = [STYLE]
ENDCASE
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Function that we call when Close the option grid, to close all opened aliases.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .T.  

*-- Close Temporary tables. [Start]
IF USED(lcTmpTrakh)
  USE IN (lcTmpTrakh)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfFillSort
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : To setup the variable.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfFillSort
lcTmpTrakh = gfTempName()
lcMajItm   = gfItemMask("HM")
lcMajPic   = gfItemMask("PM")
lcItmTitl  = gfItemMask("HI")

DIMENSION laSortDesc[4,1] , laSortVal[4,1]
laSortDesc[1,1] = "Work Order"
laSortDesc[2,1] = "Operation"
laSortDesc[3,1] = "User Id"
laSortDesc[4,1] = lcMajItm

laSortVal[1,1] = "W"
laSortVal[2,1] = "O"
laSortVal[3,1] = "I"
laSortVal[4,1] = "N"
*-- end of lfFillSort.

*!*************************************************************
*! Name      : lfGetEsh
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 01/02/2000
*! Purpose   : To calculate Estemated hourse field from BOM.DBF
*!*************************************************************
*! Called from : lfcollect()
*!*************************************************************
*! Return      : Estemated Hours.
*!*************************************************************
*
FUNCTION lfGetEsh
PRIVATE lnMajLen, lcMajorTmp, lnAlias, lnIntNstQt,lnFltNstQt, lnNSTBMQTY
lnIntNstQt = 0
lnFltNstQt = 0
lnNSTBMQTY = 0
*-- Get the major part properities [Begin]
lnMajLen   = LEN(gfItemMask("PM"))
lcMajorTmp = SUBSTR(m.STYLE,1,lnMajLen)
*-- Get the major part properities [End  ]

lnAlias = SELECT(0)
SELECT BOM
=SEEK(lcMajorTmp)
LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcMajorTmp ;
             FOR MFGCODE=m.COPRCODE

*-- CALCULATE THE TIME TO DECIMAL FRACTION
lnNSTBMQTY = NESTBOMQTY                &&*--- From BOM
*lnIntNstQt = INT(lnNSTBMQTY)           &&*--- Integar part
*lnFltNstQt = NESTBOMQTY - lnIntNstQt   &&*--- Fraction

*IF lnFltNstQt != 0
*  lnFltNstQt = (lnFltNstQt*100)/60
*  lnNSTBMQTY = lnIntNstQt + lnFltNstQt
*ENDIF

SELECT (lnAlias)
*RETURN BOM.NESTBOMQTY * m.ISSUE_QTY
lnNSTBMQTY = lnNSTBMQTY * m.ISSUE_QTY

m.est_act = lfEsT_Act(lnNstBmQty)     && calculate EST/ACT.

lnIntNstQt = INT(lnNSTBMQTY)
lnFltNstQt = lnNSTBMQTY - lnIntNstQt
IF lnFltNstQt != 0
  lnFltNstQt = (lnFltNstQt*60)/100
ENDIF

lnNSTBMQTY = lnIntNstQt + lnFltNstQt
RETURN lnNSTBMQTY


*-- End Of lfGetEsh

*!*************************************************************
*! Name      : lfEsT_Act
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 01/02/2000
*! Purpose   : To calculate Estemated hourse / Act. Hours
*!*************************************************************
*! Called from : lfEsT_Act()
*!*************************************************************
*! Return      : Estemated / Act. hours
*!*************************************************************
*
FUNCTION lfEsT_Act
PARAMETERS lnEstHrs
PRIVATE lnIntNhrs, lnFltNhrs, lnNhrs
*-- Variable decleration.
lnIntNhrs = 0
lnFltNhrs = 0
lnNhrs = 0

lnIntNhrs = INT(m.NHOURS)
lnFltNhrs = m.NHOURS - lnIntNhrs

IF lnFltNhrs != 0
  lnFltNhrs = (lnFltNhrs*100)/60
  lnNhrs = lnIntNhrs + lnFltNhrs
ELSE
  lnNhrs = lnIntNhrs
ENDIF

IF lnNhrs = 0
  m.est_act = ''
  RETURN
ELSE
  lnNhrs = lnEstHrs/lnNhrs
ENDIF

*lnIntNhrs = INT(lnNhrs)
*lnFltNhrs = lnNhrs - lnIntNhrs

*IF lnFltNhrs != 0
 * lnFltNhrs = (lnFltNhrs*60)/100
*ENDIF

*m.EST_ACT = lnIntNhrs + lnFltNhrs
m.est_act = lnnhrs
return m.est_act

*-- End Of lfEsT_Act


*!*************************************************************
*! Name      : lfTotAct
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 01/02/2000
*! Purpose   : To calculate total Actual hourse
*!*************************************************************
*! Called from : lfTotAct()
*!*************************************************************
*! Return      : total Actual Hours.
*!*************************************************************
FUNCTION lfTotAct
PARAMETERS lnTotAct
PRIVATE lnInthrs, lnFlthrs, lnhrs
lnInthrs = 0
lnFlthrs = 0
lnhrs    = 0

lnTotAct = lnTotAct/60
lnInthrs = INT(lnTotAct)
lnFlthrs = lnTotAct - lnInthrs

IF lnFlthrs != 0
  lnFlthrs = (lnFlthrs*60)/100
  lnhrs = lnInthrs + lnFlthrs
ELSE
  lnhrs = lnInthrs
ENDIF

RETURN lnhrs
*-- End Of 

*!*************************************************************
*! Name      : lfvCutNum
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
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
  SELECT CUTTKTH
  SET ORDER TO CUTTKTH
  DIMENSION laTemp[1]
  IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'CUTTKTH'))
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
*-- IF The user selected a record
IF !EMPTY(laTemp[1])
  lcObj = laTemp[1]
*--HDM B602300,1[START]
*ELSE
*  lcObj = laOldVal
*--HDM B602300,1[END]
ENDIF

&lcVar = lcObj      && Update the field

SELECT (lcPrevAl)

*!*************************************************************
*! Name      : lfGetRework  *: B#603835,1
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 04/08/1998
*! Purpose   : Get rework piecies
*!*************************************************************
*! Called from : Transaction # [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*: B#603835,1 SSH 28/08/2000 Funtion to get rework.
FUNCTION lfGetRework
PRIVATE lnOldAls , lnToRet
lnOldAls = SELECT(0)
lnToRet = 0
lcCurrType = TYPE
lcCurrCut  = CUTTKT
lcCurrOpr  = COPRCODE
lcCurrSty  = STYLE
SELECT Trstrend
*--- TYPE+CUTTKT+MFGCODE+STYLE+STATUS+DTOS(DISS_DATE)
IF SEEK(lcCurrType+lcCurrCut+lcCurrOpr+lcCurrSty+"W")
  SCAN REST WHILE TYPE+CUTTKT+MFGCODE+STYLE+STATUS+DTOS(DISS_DATE)=;
                  lcCurrType+lcCurrCut+lcCurrOpr+lcCurrSty+'W'
    lnToRet = lnToRet + Issue_Qty
  ENDSCAN
ENDIF
SELECT(lnOldAls)
RETURN lnToRet
