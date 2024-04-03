*:************************************************************************
*: Program file  : SORQAPR.Prg
*: Program desc. : PRINT REQUEST FOR ORDER APPROVAL
*: System        : ARIA4XP
*: Module        : SO
*: Developer     : Tarek Noaman (TNA)
*: Date          : 05/23/2006
*: Reference     : N037670
*:************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfSRVOrd()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SORQAPR
*:************************************************************************
*:                   Option grid Filter contains
*:01-Optional Title                IS                                 XTITLE
*:02-Status                        In List                            ORDHDR.STATUS
*:03-Sort By                       IS                                 lcRpSort
*:04-Print Orders/EDI              IS                                 lcRpEdiPrn
*:05-Order #                       In List                            ORDHDR.ORDER
*:06-Entered Date                  Between                            ORDHDR.ENTERED
*:07-Completion Date               Between                            ORDHDR.COMPLETE
*:08-Account #                     In List                            lnRpReserP
*:09-Factor #                      In List                            lnRpOther
*:***************************************************************************
*!*	ACTIVATE WINDOW trace
*!*	_screen.Visible =.T.
*!*	SUSPEND
#Include r:\aria4xp\reports\so\sorqapr.h
lcPhonPict = gfPhoneTem()

lcStTime   = TIME()
llDummy    = loOgScroll.llOGFltCh AND lfCollData()
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FOrder
GO TOP

* N000682 ,1 Thabet Handle globalization issues [Start]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Selected + ALLTRIM(STR(RECCOUNT())) + LANG_Records_in + ALLTRIM(STR(lnInterval,6,2)) + LANG_Seconds NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT())) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records_in,oAriaApplication.GetHeaderText("LANG_Records_in",AHEADERFILE)) + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Seconds,oAriaApplication.GetHeaderText("LANG_Seconds",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]

IF RECCOUNT()=0
  *--There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF

DIMENSION loOGScroll.laCRParams[5,2]
loOGScroll.laCRParams[1,1]  = 'Layout'
loOGScroll.laCRParams[1,2]  = ''
loOGScroll.laCRParams[2,1]  = 'sortby'
loOGScroll.laCRParams[2,2]  = IIF(lcRpSort='A','Account','Order')
loOGScroll.laCRParams[3,1]  = 'ReportName'
* N000682 ,1 Thabet Handle globalization issues [Start]
*loOGScroll.laCRParams[3,2]  = 'Request For Credit Approval'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOGScroll.laCRParams[3,2]  = LANG_REPTTL
loOGScroll.laCRParams[3,2]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPTTL,oAriaApplication.GetHeaderText("LANG_REPTTL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [End]
loOGScroll.laCRParams[4,1]  = 'OpTitle'
loOGScroll.laCRParams[4,2]  = XTitle
loOGScroll.laCRParams[5,1]  = 'PrnDec'
loOGScroll.laCRParams[5,2]  = lcRpDeciml


lcTmpOrd = loOgScroll.gfTempName()

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1] = oAriaApplication.WorkDir+lcTmpOrd+'.DBF'

SELECT FOrder
COPY TO oAriaApplication.WorkDir + lcTmpOrd + ".DBF"
*COPY TO oAriaApplication.WorkDir + "FOrder.DBF"

loOgScroll.lcOGLastForm = "SORQAPR"
loogScroll.cCROrientation = 'P'

= gfDispRe()

ERASE oAriaApplication.WorkDir+lcTmpOrd+'.DBF'


*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman	(TNA)
*! Date      : 4/18/2006
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData

CREATE CURSOR FOrder  (order C(6),account C(5),Xphone C(20),entered D,start D,complete D,status C(1),Xcustpo c(15), ;
                       openamt N(14,2),disc N(5,2),appramt N(14,2),approval c(10),XTer_des c(20),note1 c(30),note2 c(30), ;
                       factacct c(10),btname c(30),duns c(11),dunsrtg c(4),addr1 c(30),addr2 c(30),addr3 c(30))

SELECT FOrder
IF lcRpSort='A'
  INDEX ON Account TAG ORDTEMP
ELSE
  INDEX ON order TAG ORDTEMP
ENDIF

SELECT ORDHDR
SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER

lcRpExp = lcRpExp + " .AND. OPENAMT > 0 .AND. STATUS $ 'HO' "
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpExp = lcRpExp + " .AND. " + IIF(lcRpEdiPrn="O","!OrdHdr.lEdiOrder","OrdHdr.lEdiOrder")
ENDIF

SCAN FOR EVALUATE(lcRpExp)
  SCATTER MEMVAR memo
  INSERT INTO FOrder FROM MEMVAR
  REPLACE FOrder.factacct WITH Customer.factacct,;
          FOrder.btname   WITH Customer.btname,;
          FOrder.duns     WITH Customer.duns,;
          FOrder.dunsrtg  WITH Customer.dunsrtg,;
          FOrder.Xphone   WITH TRANSFORM(ORDHDR.PHONE , '@R '+lcPhonPict),;
          FOrder.XTer_des WITH gfCodDes(ORDHDR.CTERMCODE, 'CTERMCODE'),;
          FOrder.Xcustpo  WITH IIF(MULTIPO,'*MULTI-PO*',CUSTPO),;
          FOrder.addr1    WITH gfGetAdr('CUSTOMER' , '' , '' , '', 1,'2'),;
          FOrder.addr2    WITH gfGetAdr('CUSTOMER' , '' , '' , '', 2,'2'),;
          FOrder.addr3    WITH gfGetAdr('CUSTOMER' , '' , '' , '', 3,'2')
  IF LEN(TRIM(FOrder.addr2)) = 0
    REPLACE FOrder.addr2 WITH FOrder.addr3,;
            FOrder.addr3 WITH ''
  ENDIF
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
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/19/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.

*!*************************************************************
*! Name      : lfSRVOrd
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/19/98
*! Purpose   : control browsing Orders.
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
*! Example   : =lfSRVOrd()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVOrd
PARAMETERS lcParam

DO CASE
  CASE lcParam = 'S'
    SET ORDER TO CUSTOMER IN CUSTOMER
    SELECT ORDHDR
    SET ORDER TO TAG "ORDHDR"
    SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
    SET FILTER TO (CORDTYPE + ORDER = 'O') AND (STATUS $ "HO");
    .AND. (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt))

  CASE lcParam = 'R'
    SELECT ORDHDR
    SET RELATION TO
    llClearOrd = .F.
ENDCASE
