*:***************************************************************************
*: Program file  : ICSRANK.PRG
*: Program desc. : Style Ranking Report
*: For Report    : (ICSRANK.FRX)
*: System        : Aria4XP.
*: Module        : Inventory Control (IC)
*: Developer     :Ayman Mahoud Ahmed (AYM)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfDispRe,gfGetMemVar,gfModalGen,gfItemMask,gfBrowWare.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,
*:               : lfEvalSegs,lfCollTime,lfvRankBy,
*:               : lfSRVSty,lfStySum,lfvFabric,FaBrow,
*:               : lfvInvWare,lfRankFile,lfGetMem,lfvColor,lfPreGrd.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSRANK
*:***************************************************************************
*: This Report Program is due to E301042 ...
*:***************************************************************************
*: Modifications:
*! B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*:***************************************************************************
*!*  _screen.Visible = .T.
*!*  ACTIVATE WINDOW trace
*!*  SUSPEND
* N000682 ,1 Thabet Handle globalization issues [Start]
#INCLUDE R:\ARIA4XP\REPORTS\icsrank.H
* N000682 ,1 Thabet Handle globalization issues [END]
lcStTime = TIME()
llCalRetrn = .F.  && Compute returns.

STORE ' .T. ' TO styfltr,invflter ,RETFLTR
llMultCurr = gfGetMemvar('LLMULCURR')

*-- if user clear setting or change criteria.

IF llClearFn OR llOGFltCh OR llChCurr
   STORE .F. TO llSmajFltr , llDiv   , llSea  , llGrp ,  llFAB  ,  llCol

  lcLastExpr = lcRpExp   && To later know that user change critria.
  STORE .F. TO llClearFn,llChStyle,llChClr , llChCurr

  llCalRetrn = ('RM' $ oAriaApplication.CompanyInstalledModules );
      AND gfOpenTable(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDR','SH');
      AND gfOpenTable(oAriaApplication.DataDir+'RETLINE',oAriaApplication.DataDir+'RETLINES','SH');

  IF RECCOUNT(RANK_FILE) > 0
    USE IN (RANK_FILE)
    = lfRankFile()
  ENDIF

lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVLINE.INVDATE'),1)
LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)
lcPERIOD   = IIF(EMPTY(LDATE)and EMPTY(HDATE),'','Entered Period: &LDATE- &HDATE')


**** Build Style Filter ... BEGIN
*-- Styles Filter
lcSmajFltr= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llSmajFltr   = !EMPTY(lcSmajFltr) AND USED(lcSmajFltr) AND RECCOUNT(lcSmajFltr) > 0
IF llSmajFltr
  SELECT (lcSmajFltr)
  INDEX ON CSTYMAJOR TAG (lcSmajFltr)
  STYFLTR=STYFLTR+" AND SEEK(STYLE.CSTYMAJOR,'"+lcSmajFltr+"')"
ELSE
  IF TYPE("lcSmajFltr") = "C" AND USED(lcSmajFltr)
    USE IN (lcSmajFltr)
  ENDIF
  lcSmajFltr= ''
ENDIF
* Check if there is a filter on Style CDIVISION
lcCurName = lfCheckFilter(3, 'STYLE.CDIVISION')
lcDiv   = loOgScroll.gfTempName()
llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
IF llDiv
  SELECT (lcDiv)
  INDEX on CDivision TAG (lcDiv)
  STYFLTR=STYFLTR+" AND SEEK(STYLE.CDIVISION,'"+lcDiv+"')"
ENDIF

* Check if there is a filter on Style SEASON
lcCurName = lfCheckFilter(3, 'STYLE.SEASON')
lcSea  = loOgScroll.gfTempName()
llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
IF llSea
  SELECT (lcSea  )
  INDEX on SEASON TAG (lcSea  )
  STYFLTR=STYFLTR+" AND SEEK(STYLE.SEASON,'"+lcSea+"')"
ENDIF

* Check if there is a filter on Style Group
lcCurName = lfCheckFilter(3, 'STYLE.CSTYGROUP')
lcGrp  = loOgScroll.gfTempName()
llGrp   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcGrp  ,"Group")
IF llGrp
  SELECT (lcGrp  )
  INDEX on Group TAG (lcGrp  )
  STYFLTR=STYFLTR+" AND SEEK(STYLE.CSTYGROUP,'"+lcGrp+"')"
ENDIF

* Check if there is a filter on Style FABRIC
lcFAB  = lfCheckFilter(3, 'STYLE.FABRIC')
llFAB   = !EMPTY(lcFAB  )AND USED(lcFAB)  AND RECCOUNT(lcFAB  )<>0
IF llFAB
  SELECT (lcFAB  )
  INDEX on CSTYMAJOR TAG (lcFAB  )
  STYFLTR=STYFLTR+" AND SEEK(STYLE.FABRIC,'"+lcFAB+"')"
ENDIF

* Check if there is a filter on Style COLOR
lcCurName = lfCheckFilter(1, 'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
lcCol  = loOgScroll.gfTempName()
llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color")
IF llCol
  SELECT (lcCol  )
  INDEX on Color TAG (lcCol  )
  STYFLTR=STYFLTR+" AND SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'"+lcCol  +"')"
ENDIF

lcFree= lfCheckFilter(1, 'SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen)')
llFree=IIF(EMPTY(lcFree),.f.,.t.)
IF llFree
  STYFLTR=STYFLTR+" AND SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen)='"+lcFree+"'"
ENDIF


* PATTERN Filter
lcPattern= lfCheckFilter(3, 'STYLE.PATTERN')
llPattern=IIF(EMPTY(lcPattern),.f.,.t.)
IF llPattern
  STYFLTR=STYFLTR+" AND STYLE.PATTERN='"+lcPattern+"'"
ENDIF
STYFLTR=STYFLTR+ IIF(lcRpStatus='L','',[ AND (Style.Status="] + lcRpStatus + [")])
**** Build Style Filter ... END
***************************************************************************************************
**** Build INVOICE Filter ... BEGIN


RETHDFL=' .T. '
* Check if there is a filter on Warecodes
lcWare  = lfCheckFilter(3, 'INVHDR.CWARECODE')
llWare   = !EMPTY(lcWare  )AND USED(lcWare  )  AND RECCOUNT(lcWare  )<>0
IF llWare
  SELECT (lcWare  )
  INDEX on CWARECODE TAG (lcWare  )
  invflter =invflter +" AND SEEK(INVHDR.CWARECODE,'"+lcWare  +"')"
  RETHDFL=RETHDFL+" AND SEEK(RETHDR.CWARECODE,'"+lcWare  +"')"

ENDIF

* Check if there is a filter on Warecodes
lcCur = lfCheckFilter(1, 'INVHDR.CCURRCODE')
llCur  = !EMPTY(lcCur )AND USED(lcCur )  AND RECCOUNT(lcCur )<>0
IF llCur
  SELECT (lcCur )
  INDEX on CCURRCODE TAG (lcCur )
  invflter =invflter +" AND SEEK(INVHDR.CCURRCODE,'"+lcCur +"')"
  RETHDFL=RETHDFL+" AND SEEK(RETHDR.CCURRCODE,'"+lcCur +"')"

ENDIF

*Date Filter
IF !EMPTY(LDATE)
  invflter =invflter +" AND  BETWEEN(INVDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
  RETHDFL=RETHDFL+" AND  BETWEEN(CRDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
ELSE
  IF  !EMPTY(HDATE)
    invflter =invflter +" AND  INVDATE<=CTOD('"+HDATE+"') "
    RETHDFL=RETHDFL+" AND  CRDATE<=CTOD('"+HDATE+"') "
  ENDIF
ENDIF

**** Build INVOICE Filter ... END

*-- Start collecting rank data.
  llRpColor = llRpColor AND (lcFree_Clr = 'C')  && Style/Color
  SET ORDER TO (RANK_FILE) IN (RANK_FILE)
  lcOldRank = ''  && To ReIndex File again.

  *-- initial value for memory variables.
  STORE '' TO m.Style,m.Desc
  STORE 0 TO m.AVG_PRICE,m.COST,m.TOT_PCS,m.TOT_AMT,;
             m.RET_PCS,m.RET_AMT,m.NET_PCSP,m.NET_AMTP,;
             m.NET_PCS,m.NET_AMT,m.CGS,m.GROS_PRO

  SELECT INVLINE
  SET RELATION TO Invoice INTO InvHdr ADDITIVE

*!*	IF .f. && llSmajFltr or llDiv   or llSea  or llGrp or  llFAB   or  llCol
*!*	  SELECT STYLE
*!*	  SCAN WHILE STYLE ='' FOR &styfltr
*!*	    lcStyle=STYLE
*!*	    WAIT WINDOW "Collecting data for style "+lcStyle NOWAIT
*!*	    ** INvoice Section
*!*	    = lfDoInv('')
*!*	    ** Return Section
*!*	    =lfDoRet ('')
*!*	  ENDSCAN  && Style
*!*	  *-- Scan all invoices in selected critria.
*!*	  WAIT CLEAR
*!*	ELSE
  SELECT INVLINE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  lcStyle=''
  = lfDoInv(styfltr)
  SELECT INVLINE
  SET RELATION TO

  SELECT RETLINE
*!*	  SET RELATION TO STYLE INTO STYLE ADDITIVE
  =lfDoRet (styfltr)
*!*	ENDIF
ENDIF


SELECT (RANK_FILE)
IF RECCOUNT(RANK_FILE) > 0
*-- ReIndex File if old index was changed.
  DO CASE

    CASE lcRpRankBy = 'S'
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
      *INDEX ON - NET_AMT  TAG (RANK_FILE1) OF (RANK_FILE)
      INDEX ON - NET_AMT  TAG (RANK_FILE1)
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
    CASE lcRpRankBy = 'P'
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
      *INDEX ON - NET_PCS  TAG (RANK_FILE1) OF (RANK_FILE)
      INDEX ON - NET_PCS  TAG (RANK_FILE1)
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
    CASE lcRpRankBy = 'G'
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
      *INDEX ON - GROS_PRO  TAG (RANK_FILE1) OF (RANK_FILE)
      INDEX ON - GROS_PRO  TAG (RANK_FILE1)
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
  ENDCASE
ENDIF


*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.

* N000682 ,1 Thabet Handle globalization issues [Start]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(RANK_FILE))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE))+ ALLTRIM(STR(RECCOUNT(RANK_FILE))) +;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records_in,oAriaApplication.GetHeaderText("LANG_Records_in",AHEADERFILE))+ ALLTRIM(STR(lnInterval,6,2)) +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Seconds,oAriaApplication.GetHeaderText("LANG_Seconds",AHEADERFILE)) NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)

  SET ORDER TO STYLE IN STYLE
  SET ORDER TO WAREHOUS IN WAREHOUS


  = lfRankFile()  && Create temporary used in collecting data.

  XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')

ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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
*  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
  lcColorTlt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ONLYCOLORS,oAriaApplication.GetHeaderText("LANG_ONLYCOLORS",AHEADERFILE))+' ' +;
			   ALLTRIM(lcNonMajTl) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S,oAriaApplication.GetHeaderText("LANG_S",AHEADERFILE))
  * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	  lcShowVar = 'Show details by ' + lcNonMajTl
*!*	  lcStyStat = lcStyMajor + ' Status'
*!*	  lcStyMake = lcStyMajor + ' (Make/Buy)'
lcShowVar = IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('AHEADERFILE')<>'C',LANG_SHOWDET,oAriaApplication.GetHeaderText("LANG_SHOWDET",AHEADERFILE))+' ' + lcNonMajTl
lcStyStat = lcStyMajor + ' '+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('AHEADERFILE')<>'C',LANG_STATUS,oAriaApplication.GetHeaderText("LANG_STATUS",AHEADERFILE))
lcStyMake = lcStyMajor + ' '+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('AHEADERFILE')<>'C',LANG_MAKEBUY,oAriaApplication.GetHeaderText("LANG_MAKEBUY",AHEADERFILE))
 * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfvRankBy
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Rank By validation, Save old Rank to use in when reindex.
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRankBy()
*!*************************************************************
*
FUNCTION lfvRankBy
lcOldRank = IIF(laOldVal=1,'S',IIF(laOldVal=2,'P','G'))
*-- end of lfvRankBy.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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
*! Name      : lfRankFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRankFile()
*!*************************************************************
*
FUNCTION lfRankFile


CREATE CURSOR (RANK_FILE) (STYLE C(19)     , DESC C(20)      , AVG_PRICE N(12,2)   ,;
                           COST N(7,2)     , TOT_PCS N(7,0)  , TOT_AMT N(13,2)     ,;
                           RET_PCS N(7,0)  , RET_AMT N(14,2) , NET_PCSP N(7,0)     ,;
                           NET_AMTP N(13,2), NET_PCS N(7,0)  , NET_AMT N(13,2)     ,;
                           CGS N(13,2)     , GROS_PRO N(13,2), lnexrate N(9,4))
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON STYLE TAG (RANK_FILE) OF (RANK_FILE)
INDEX ON STYLE TAG (RANK_FILE)
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

*--End of lfRankFile.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*-- Close temp. opended files, if it used.
IF USED(RANK_FILE)
 USE IN (RANK_FILE)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGetMem
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/21/1998
*! Purpose   : Fill RANK_FILE memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = ?()
*!*************************************************************
FUNCTION lfGetMem
m.Style     = IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR))
m.Desc      = STYLE.DESC
m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)

STORE TOTQTY TO m.TOT_PCS,m.NET_PCS,m.NET_PCSP
STORE ((TOTQTY*PRICE)*(1-INVHDR.DISCPCNT/100))*(1-INVHDR.TRDE_DISC/100) TO ;
      m.NET_AMT,m.NET_AMTP,m.TOT_AMT

m.CGS       = m.COST*m.TOT_PCS


*--MUST SELECT INVHDR
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT INVHDR

m.TOT_AMT  = IIF(lcRpCurr <> 'F' , gfAmntDisp(m.TOT_AMT, lcRpCurr , ldRpExDate , lcRpTmpNam),m.TOT_AMT)
m.NET_AMT  = m.TOT_AMT
m.NET_AMTP = m.TOT_AMT
*m.CGS      = IIF(lcRpCurr == 'F' , gfAmntDisp(m.COST, lcRpCurr , ldRpExDate , lcRpTmpNam),m.COST) * m.TOT_PCS

SELECT (lcAlias)
*E125824,1 BWA 02/21/2004[END]

m.GROS_PRO  = m.NET_AMT - m.CGS
m.AVG_PRICE = m.TOT_AMT / m.TOT_PCS

*--End of lfGetMem.
*!*************************************************************
*! Name      : lfvColor
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/21/1998
*! Purpose   : Validation function for print by color, Rise change color flag.
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvColor()
*!*************************************************************
*
FUNCTION lfvColor
llChClr = .T.
*-- end of lfvColor.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
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
*! Name      : lfPreGrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/21/1998
*! Purpose   : Function called to determine if we continue showing OG or not.
*!*************************************************************
*! Called from : ICREPORT program
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPreGrd()
*!*************************************************************
*
FUNCTION lfPreGrd
PRIVATE lcUserMake
lcUserMake = ''

*-- if no setups for AR module
IF !('AR' $ gcComp_Mdl)
  lcUserMake = 'setup'
ENDIF

*-- if AR module not installed.
IF EMPTY(lcUserMake) AND !('AR' $ oAriaApplication.CompanyInstalledModules)
  lcUserMake = 'install'
ENDIF

IF !EMPTY(lcUserMake)
  *-- Message
  *-- Rank Styles Report require XXX of XXX module for Company XXX,
  *-- First, you have to XXX it through the company information program.
  *--                               < Ok >
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *=gfModalGen('TRM00336B00000','DIALOG',lcUserMake + '|' + 'Accounts Receivable' + '|' + gcCom_Name + '|' + lcUserMake)
  =gfModalGen('TRM00336B00000','DIALOG',lcUserMake + '|' + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('AHEADERFILE')<>'C',LANG_AR,oAriaApplication.GetHeaderText("LANG_AR",AHEADERFILE)) + '|' + gcCom_Name + '|' + lcUserMake)
  * N000682 ,1 Thabet Handle globalization issues [ENd]
  llDoOpGrid = .F.
ENDIF
*-- end of lfPrOpGd.

*E125824,1 BWA 02/21/2004 Add the Currency option.[START]
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : Get the description of Curr.
*!*************************************************************
*! Called from : Syrepuvr "Currency Display" Record.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

llChCurr = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)

*--End of lfvCurDisp.
*!*************************************************************
*! Name      : lfDefCurr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************

FUNCTION lfDefCurr

llMultCur = gfGetMemVar('llMulCurr')
RETURN  IIF(llMultCur,'F','O')



*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : Get the defaukt value. [This () Called from SYDREPRT]
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfRepShow()
*!*************************************************************
FUNCTION lfRepShow
*!*  PRIVATE lnCurrPos

*!*  lnCurrPos = ASCAN(laOgFxFlt,'INVHDR.CCURRCODE')
*!*  IF lnCurrPos > 0
*!*    lnCurrPos  = ASUBSCRIPT(laOgFxFlt,lnCurrPos,1)
*!*  ENDIF

*!*  laOGFxFlt[lnCurrPos,6]= gcBaseCurr
*!*  =lfOGShowGet("lnRepCurr")



*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

*!*************************************************************
*! Name      : lfDoInv
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfDoInv
LPARAMETERS lcStyfltr
lcFlt=IIF(EMPTY(lcStyfltr),''," AND "+lcStyfltr)
SELECT invline
SEEK(lcStyle)
SCAN WHILE style+invoice+STR(lineno,6)=lcstyle FOR INVHDR.STATUS#'V' AND  &invflter  &lcFlt
  *B608158,1 SSH Add Wait window

  * N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW "Collecting data for Style: "+invline.Style  nowait
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collecting_Data_for_Style,oAriaApplication.GetHeaderText("LANG_Collecting_Data_for_Style",AHEADERFILE)) +;
             invline.Style  nowait
  * N000682 ,1 Thabet Handle globalization issues [END]
  *B608158,1 SSH
  =lfGetMem()  && Fill Memory variables with its corresponding values.
   *-- if you find this style in temp. file
  IF SEEK(IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR)),RANK_FILE)
    SELECT (RANK_FILE)
    REPLACE TOT_PCS    WITH TOT_PCS    + m.TOT_PCS   ,;
            TOT_AMT    WITH TOT_AMT    + m.TOT_AMT   ,;
            NET_PCSP   WITH NET_PCSP   + m.TOT_PCS   ,;
            NET_AMT    WITH NET_AMT    + m.TOT_AMT   ,;
            NET_PCS    WITH NET_PCS    + m.TOT_PCS   ,;
            NET_AMTP   WITH NET_AMTP   + m.TOT_AMT   ,;
            CGS        WITH CGS        + m.CGS       ,;
            GROS_PRO   WITH NET_AMT    - CGS         ,;
            AVG_PRICE  WITH IIF(TOT_PCS<>0,(TOT_AMT/TOT_PCS),0)

  SELECT INVLINE
  ELSE
   INSERT INTO (RANK_FILE) FROM MEMVAR
  ENDIF
  *B608158,1 SSH clear wait window
  WAIT clear
  *B608158
ENDSCAN

*!*************************************************************
*! Name      : lfDoRet
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   :
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfDoRet
LPARAMETERS lcStyfltr
lcFlt=IIF(EMPTY(lcStyfltr),''," AND "+lcStyfltr)
IF llCalRetrn
      *-- intially free all memory variables.
      STORE '' TO m.Style,m.Desc
      STORE 0 TO m.AVG_PRICE,m.COST,m.TOT_PCS,m.TOT_AMT,;
                 m.RET_PCS,m.RET_AMT,m.NET_PCSP,m.NET_AMTP,;
                 m.NET_PCS,m.NET_AMT,m.CGS,m.GROS_PRO
      *-- Scan all returns in the specific criteria.
      SELECT RETLINE
      IF GFSEEK(lcStyle)
        SET RELATION TO STYLE INTO STYLE ADDITIVE
        SCAN WHILE style+crmemo=lcStyle FOR  &RETFLTR &lcFlt
          *B608158,1 SSH Add Wait window

          * N000682 ,1 Thabet Handle globalization issues [Start]
*          WAIT WINDOW "Collecting data for Style: "+RETLINE.Style  nowait
          WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collecting_Data_for_Style,oAriaApplication.GetHeaderText("LANG_Collecting_Data_for_Style",AHEADERFILE)) +;
                      RETLINE.Style  nowait
          * N000682 ,1 Thabet Handle globalization issues [END]
          *B608158,1 SSH
          IF !(GFSEEK(crmemo,'RETHDR') AND RETHDR.STATUS#'V' AND &RETHDFL)
            LOOP
          ENDIF
          m.STYLE     = STYLE
          m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)
          PRIVATE lcAliasR
          lcAliasR = ALIAS()
          SELECT RETHDR
          m.DESC      = STYLE.DESC
          m.AMOUNT    = IIF(lcRpCurr <> 'F' , gfAmntDisp(RETLINE.AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),RETLINE.AMOUNT)
          SELECT (lcAliasR)
          m.TOTQTY    = TOTQTY
          m.AVG_PRICE = m.AMOUNT / m.TOTQTY
          *-- if style was not exist in temp. file
          IF !SEEK(IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR)),RANK_FILE)
            INSERT INTO (RANK_FILE) FROM MEMVAR
          ENDIF
          SELECT (RANK_FILE)
          REPLACE RET_PCS   WITH RET_PCS + m.TOTQTY ,;
                  RET_AMT   WITH RET_AMT + m.AMOUNT ,;
                  NET_PCS   WITH ( TOT_PCS - RET_PCS ),;
                  NET_AMT   WITH ( TOT_AMT - RET_AMT )
          *-- if return from invoice.
          IF !EMPTY(RETLINE.INVOICE)
            REPLACE NET_PCSP WITH ( NET_PCSP - m.TOTQTY ),;
                    NET_AMTP WITH ( NET_AMTP - m.AMOUNT )

            IF llRpColor
              REPLACE  CGS WITH NET_PCSP * m.COST
            ELSE
              REPLACE  CGS WITH CGS - (m.TOTQTY * m.COST)
            ENDIF
          ENDIF
          REPLACE GROS_PRO WITH NET_AMT - CGS
          SELECT RETLINE
        ENDSCAN  &&retline
        *B608158,1 SSH Clear wati
        WAIT CLEAR
        *B608158,1 SSH
      ENDIF
    ENDIF
