*:***************************************************************************
*: Program file  : MAMATLST
*: Program desc. : Material master list
*: For Report    : MAMATLSTA.FRX ,MAMATLSTB.FRX
*: System        : Aria Advantage Series.
*: Module        : Materials (MA)
*: Developer     : Heba Mohamed Amin	(HMA)
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrint
*:    Functions  : lfBldSqlCur(),lfClearRed(),lfFillBin(),lfGetWline(),
*:                 lfSumFab(),lfvBins(),lfvVendor(),lfvWarehs(),lfwOldVal(),
*:                 lfwRepWhen()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAMATLST
*:***************************************************************************
*: Modifications:
*B125765 ,1 HMA  12/21/2004 remove the user privilege from the report(don't call gfUserPriv Function)
*B125737 ,1 HMA  01/09/2005 Return the whole value of numeric field value instead of return only its integer value
*E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve tha performance,Don't recollect Data if nothing change in Option Grid
*N000548,1  MMT 08/20/2007 convert locking screen to Aria4xp[T20060908.0003]
*:***************************************************************************
#INCLUDE R:\Aria4xp\reports\ma\mamatlst.h


PUBLIC lcMtCstMth

IF loOGScroll.llOGFltCh   &&If Filter Changed
  lcGrpField = ""
  lcSelFab = ""
  lcSelVend = ""
  lcSelLoc = ""
  llFabric = .F.
  llVendor = .F.
  llWareHous= .F.

  *-- llDontPrn variable that is used to prevent printing if there is not
  *--           any record matches the report criteria
  llDontPrn = .F.
  *B125765 ,1 HMA  12/21/2004 remove the user privilege from the report [BEGIN]
  *-- llCostAccs variable that showes if the user has costing access or not
  *llCostAccs = gfUserPriv('IC','ICSTYLE')
  *qCostPrv   = llCostAccs
  *B125765 ,1 HMA  12/21/2004 remove the user privilege from the report [END]
  *-- lcMtCstMth variable that hold the system costing method
  lcMtCstMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET')))
  *-- llDyelot   variable that showes if the system uses dyelot or not.
  *-- llRPPrnDye variable in option grid that showes if the user choes to print dyelot.

  *** LOOP FOR REPORT CRITERIA ***
  DO lpPrint

ELSE
*E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve tha performance,Don't recollect Data if nothing change in Option Grid [Begin]
  IF !USED(WORKFILE)
    USE oAriaApplication.WorkDir +  WORKFILE  + ".DBF" IN 0
  ENDIF
  IF RECCOUNT(lcFabric) = 0  OR  RECCOUNT(lcFabDye1) = 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    SELECT(WORKFILE)
    DO gfDispRe WITH EVAL('lcRPForm')
  ENDIF
ENDIF
*E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve tha performance,Don't recollect Data if nothing change in Option Grid [End]

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/22/2004
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
LOCAL lnResult1

*--Change Title of Dyelot Line
DO CASE
   CASE oAriaApplication.ActiveModuleID = 'MA'
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcDyeTlt= LANG_Mamatlst_PrintDyelot
lcDyeTlt= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Mamatlst_PrintDyelot,oAriaApplication.GetHeaderText("LANG_Mamatlst_PrintDyelot",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

   CASE oAriaApplication.ActiveModuleID = 'IC'
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcDyeTlt= LANG_Mamatlst_PrintConfig
lcDyeTlt= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Mamatlst_PrintConfig,oAriaApplication.GetHeaderText("LANG_Mamatlst_PrintConfig",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE
*--Check dyelot option in MA Module Setup
llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
LNPRNDYEP = ASUBSCRIPT(LOOGSCROLL.LAOGOBJTYPE,ASCAN(LOOGSCROLL.LAOGOBJTYPE,'LLRPPRNDYE'),1)
LOOGSCROLL.LAOGOBJCNT[LNPRNDYEP] = LLDYELOT
= LFOGSHOWGET('LLRPPRNDYE')

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))


IF oAriaApplication.ActiveModuleID = 'MA' .AND. !llFrstTime
  lcFabDye=loogscroll.gftempname()
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC "

  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"

  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

  llFrstTime = .T.

ENDIF


IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF



*!*************************************************************
*! Name      : lpPrint
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/22/2004
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint
LOCAL lnResult2 ,lcSelFab,lcSelVend

*-- Append the selected Fabrics  to the where condition
lcSelFab=lfBldSqlCur('ITEM.CSTYMAJOR','llFabric','lcSelFab','Fabric C(19)','CSTYMAJOR') &&IT'S A CURSOR HOLD THE SELECTED FABRBRICS
*-- Append the selected Vendors to the where condition
lcSelVend=lfBldSqlCur('APVENDOR.CVENDCODE','llVendor','lcSelVend','Vendor C(8)','CVENDCODE') &&IT'S A CURSOR HOLD THE SELECTED VENDORS
*--Retrieve the Value Of the Color Variable Filter to the Where Condition after change it into Sql Expression.
lcColorVal =""
IF !EMPTY(loOgScroll.laOgVrflt[1,6])
  lcColorVal="'"+STRTRAN(loOgScroll.laOgVrflt[1,6],"|","','")+"'"
ENDIF

*--Create Sql Cursor Filtered by  all the Selection Criteria.
lcFabric=loogscroll.gftempname()

lcSelFld2= "SELECT ITEM.STYLE,ITEM.CSTYMAJOR AS FABRIC,ITEM.[DESC],ITEM.CITEMFLD3 AS CONTENT,;
            ITEM.LOCATION AS LOC,ITEM.INTRODUCED,ITEM.ITEM_TYPE,ITEM.VENDOR,ITEM.PATTERN,ITEM.CITEMFLD1 AS WIDTH ,ITEM.MAKE ,"

lcSelFld2= lcSelFld2+"ITEM.CITEMFLD2 AS CFABWEIGHT,ITEM.LEADTIME ,ITEM.NICOST1 AS NFABCOST ,ITEM.NICOST2 AS NITEM_TAX ,;
                      ITEM.NICOST3 AS NITEMQUOTA,ITEM.NICOST4 AS NITM_FRT,ITEM.NTOTREORD AS REORDER,ITEM.SEASON,ITEM.CDIVISION,"

lcSelFld2= lcSelFld2 + "ITEM.TOTCOST AS COSTBUY ,ITEM.NAVECSTBUY ,ITEM.CDYE_FLG , UOM.CUOM_B AS UOMBUY,;
                        UOM.CUOM_V AS UOMUSE ,UOM.NCONF AS CONV FROM ITEM INNER JOIN UOM ON ITEM.CCONVBUY = UOM.CUOMCODE "

*--Add the filter of selected Items.
IF llFabric  && Fabrics selected in the in list browse
  lcSelFld2 = lcSelFld2 + " INNER JOIN " + lcSelFab + " TmpFabric ON TmpFabric.Fabric = ITEM.CSTYMAJOR "
ENDIF

*--Add filter of selected vendors.
IF llVendor
  lcSelFld2 = lcSelFld2 + " INNER JOIN " + lcSelVend + " TmpVendor ON TmpVendor.Vendor = ITEM.VENDOR "
ENDIF

*--Add the Filter of Inventory Type (Fabric OR Style).
lcSelFld2= lcSelFld2 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"

*--Add the color Filter.
IF !EMPTY(lcColorVal)
  lcSelFld2= lcSelFld2 + " AND RIGHT(ITEM.STYLE," +ALLTRIM(STR(lnColorLen)) +" ) IN (" + lcColorVal +")"
ENDIF


*-- Add filter (Material Type) = 'B' -> Both, 'D'-> Domestic, 'I'->Imported
IF lcRpDomImp <>'B'
  IF lcRpDomImp = 'D'
    lcFlt1 = " ITEM.MAKE = 1 "
  ELSE
    lcFlt1 = " ITEM.MAKE = 0 "
  ENDIF
  *-- add Material Type Filter to where condition
  lcSelFld2= lcSelFld2 +" AND" +lcFlt1
ENDIF



*--Add Filter of lcRpSqlExp
lcRpSqlExp=loOgScroll.lcRpSqlExp

IF !EMPTY(lcRpSqlExp)
  lcSelFld2= lcSelFld2 + " AND " +lcRpSqlExp
ENDIF

lnResult2 = loOGScroll.oRDA.SqlRun(lcSelFld2,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

*--Add Filter of Onhand Qty
lcWherCond=""
lcOnHand_Q = IIF(llRPOnhand,'Y','N')
IF lcOnHand_Q= 'Y'
  IF EMPTY(lcWherCond)
    lcWherCond= "&lcFabDye..ONHAND <> 0 "
  ELSE
    lcWherCond= lcWherCond + " AND " + "&lcFabDye..ONHAND <> 0 "
  ENDIF
ENDIF

*--Add Filter of Selected Locations.
lcWareHous=loOgScroll.laOgFxflt[3,6]
PRIVATE llWareHSelectd,lnNotDeleted
llWareHSelectd = .F.
IF !EMPTY(lcWareHous) AND USED(lcWareHous) AND RECCOUNT(lcWareHous) > 0
  SELECT (lcWareHous)
  COUNT FOR !DELETED() TO lnNotDeleted
  *PACK
  *llWareHSelectd = RECCOUNT(lcWareHous) > 0
  llWareHSelectd = lnNotDeleted > 0
ENDIF
*--Create another File(FABDYE1) to hold FABDYE Fields with filters needed to be made on it
lcFabDye1=loogscroll.gftempname()
IF !EMPTY(lcWherCond )
  IF llWareHSelectd
    SELECT &lcFabDye..*  FROM &lcFabDye  INNER JOIN &lcWareHous ON &lcWareHous..CWARECODE = &lcFabDye..CWARECODE WHERE  &lcWherCond INTO CURSOR &lcFabDye1
  ELSE
    SELECT *  FROM &lcFabDye WHERE &lcWherCond INTO CURSOR &lcFabDye1
  ENDIF
ELSE
  *IF !EMPTY(lcWareHous)
  IF llWareHSelectd
   SELECT &lcFabDye..* FROM &lcFabDye INNER JOIN &lcWareHous ON &lcWareHous..CWARECODE = &lcFabDye..CWARECODE INTO CURSOR &lcFabDye1
  ELSE
    SELECT * FROM &lcFabDye INTO CURSOR &lcFabDye1
  ENDIF
ENDIF

SELECT(lcFabDye1)
INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye1


IF lnResult2 >=1
  SELECT (lcFabric)
  lnBuffering = CURSORGETPROP("Buffering",lcFabric)
  =CURSORSETPROP("Buffering",3,lcFabric)
  INDEX ON STYLE TAG lcFabric
  SET RELATION TO STYLE INTO &lcFabDye1 ADDITIVE
ENDIF

SELECT (lcFabric)

=AFIELDS(laFileStru)

lnArrLen = ALEN(laFileStru,1)

DIMENSION laFileStru[lnArrLen+16,18]

laFileStru[lnArrLen+1,1] = 'Dyelot'
laFileStru[lnArrLen+1,2] = 'C'
laFileStru[lnArrLen+1,3] = 10
laFileStru[lnArrLen+1,4] = 0

laFileStru[lnArrLen+2,1] = 'UseDye'
laFileStru[lnArrLen+2,2] = 'L'
laFileStru[lnArrLen+2,3] = 1
laFileStru[lnArrLen+2,4] = 0

laFileStru[lnArrLen+3,1] = 'NextDye'
laFileStru[lnArrLen+3,2] = 'L'
laFileStru[lnArrLen+3,3] = 1
laFileStru[lnArrLen+3,4] = 0

laFileStru[lnArrLen+4,1] = 'ClrDesc'
laFileStru[lnArrLen+4,2] = 'C'
laFileStru[lnArrLen+4,3] = 12
laFileStru[lnArrLen+4,4] = 0

laFileStru[lnArrLen+5,1] = 'ItemDesc'
laFileStru[lnArrLen+5,2] = 'C'
laFileStru[lnArrLen+5,3] = 20
laFileStru[lnArrLen+5,4] = 0

laFileStru[lnArrLen+6,1] = 'ITEMTYPE'
laFileStru[lnArrLen+6,2] = 'C'
laFileStru[lnArrLen+6,3] = 1
laFileStru[lnArrLen+6,4] = 0

laFileStru[lnArrLen+7,1] = 'NEXTWARE'
laFileStru[lnArrLen+7,2] = 'L'
laFileStru[lnArrLen+7,3] = 1
laFileStru[lnArrLen+7,4] = 0

laFileStru[lnArrLen+8,1] = 'lLast'
laFileStru[lnArrLen+8,2] = 'L'
laFileStru[lnArrLen+8,3] = 1
laFileStru[lnArrLen+8,4] = 0

laFileStru[lnArrLen+9,1] = 'Usage'
laFileStru[lnArrLen+9,2] = 'N'
laFileStru[lnArrLen+9,3] = 12
laFileStru[lnArrLen+9,4] = 3

laFileStru[lnArrLen+10,1] = 'OnHand'
laFileStru[lnArrLen+10,2] = 'N'
laFileStru[lnArrLen+10,3] = 12
laFileStru[lnArrLen+10,4] = 3

laFileStru[lnArrLen+11,1] = 'OnOrder'
laFileStru[lnArrLen+11,2] = 'N'
laFileStru[lnArrLen+11,3] = 12
laFileStru[lnArrLen+11,4] = 3

laFileStru[lnArrLen+12,1] = 'nStkVal'
laFileStru[lnArrLen+12,2] = 'N'
laFileStru[lnArrLen+12,3] = 12
laFileStru[lnArrLen+12,4] = 2

laFileStru[lnArrLen+13,1] = 'SeasonDesc'
laFileStru[lnArrLen+13,2] = 'C'
laFileStru[lnArrLen+13,3] = 20
laFileStru[lnArrLen+13,4] = 0

laFileStru[lnArrLen+14,1] = 'DivDesc'
laFileStru[lnArrLen+14,2] = 'C'
laFileStru[lnArrLen+14,3] = 20
laFileStru[lnArrLen+14,4] = 0

laFileStru[lnArrLen+15,1] = 'Color'
laFileStru[lnArrLen+15,2] = 'C'
laFileStru[lnArrLen+15,3] = 7
laFileStru[lnArrLen+15,4] = 0

laFileStru[lnArrLen+16,1] = 'cWARECODE'
laFileStru[lnArrLen+16,2] = 'C'
laFileStru[lnArrLen+16,3] = 6
laFileStru[lnArrLen+16,4] = 0


FOR lncnt=7 TO 16
  FOR lnInc=1 TO 16
    STORE SPACE(0) TO laFileStru[lnArrLen+lnInc,lnCnt]
  ENDFOR
ENDFOR
FOR lnInc=1 TO 16
  STORE 0 TO laFileStru[lnArrLen+lnInc,17],laFileStru[lnArrLen+lnInc,18]
ENDFOR

WORKFILE = loOgScroll.gfTempName()

*--put indexing here and create another index for using in search for warehouse

IF lcRPSortBy='I'
  lcSrtByIdx="STYLE+ITEMTYPE"
  lcGrpField = 'FABRIC'
ELSE
  lcSrtByIdx="VENDOR+STYLE+ITEMTYPE"
  lcGrpField = 'VENDOR'
ENDIF

DIMENSION laIndex[2,2]
laIndex[1,1] =lcSrtByIdx
laIndex[1,2] ='SortIdx'
laIndex[2,1] ="STYLE+CWARECODE+ITEMTYPE"
laIndex[2,2] = 'WareIdx'

=gfCrtTmp(WORKFILE,@laFileStru,@laIndex,WORKFILE,.T.)

SELECT(WORKFILE)
SET ORDER TO TAG SortIdx


*-- Start Collecting Data
SELECT (lcFabDye1)
LOCATE
IF EOF()
*  WAIT WINDOW LANG_Mamatlst_NoRecord NOWAIT
  =gfModalGen('TRM00052B40011','ALERT')
  llDontPrn = .T.
  RETURN
ENDIF

SELECT (lcFabric)
IF EOF()
*  WAIT WINDOW LANG_Mamatlst_NoRecord NOWAIT
  =gfModalGen('TRM00052B40011','ALERT')
  llDontPrn = .T.
  RETURN
ENDIF


*--check if user select print warehous details.
SCAN
  IF &lcFabric..STYLE <> &lcFabDye1..STYLE
    LOOP
  ENDIF

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Mamatlst_Collecting + ALLTRIM(&lcFabric..STYLE) NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Mamatlst_Collecting,oAriaApplication.GetHeaderText("LANG_Mamatlst_Collecting",AHEADERFILE)) + ALLTRIM(&lcFabric..STYLE) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lcClrDesc = SUBSTR(gfCodDes(SUBSTR(&lcFabric..STYLE,lnMajorlen+2,lnColorLen),'COLOR'),1,12)
  lcTyp_Desc = SUBSTR(gfCodDes(&lcFabric..ITEM_TYPE,'ITEM_TYPE'),1,20)
  lcSea_Desc = SUBSTR(gfCodDes(&lcFabric..SEASON,'SEASON'),1,20)
  lcDiv_Desc = SUBSTR(gfCodDes(&lcFabric..CDIVISION,'CDIVISION'),1,20)
  SCATTE MEMVAR MEMO
  SELECT (WORKFILE)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE UseDye     WITH .F.,;
          NEXTDYE    WITH (llDyelot AND llRPPrnDye AND &lcFabric..CDYE_FLG = 'Y' AND !EOF('&lcFabDye1')),;
          ClrDesc    WITH lcClrDesc,;
          ItemDesc   WITH lcTYP_DESC,;
          SeasonDesc WITH lcSea_Desc,;
          DivDesc    WITH lcDiv_Desc ,;
          ITEMTYPE   WITH 'A',;
          ONHAND     WITH lfSumFab2(STYLE,'ONHAND'),;
          ONORDER    WITH lfSumFab2(STYLE,'ONORDER'),;
          USAGE      WITH lfSumFab2(STYLE,'USAGE'),;
          NSTKVAL    WITH lfSumFab2(STYLE,'NSTKVAL') ,;
          COLOR      WITH SUBSTR(&lcFabric..STYLE,lnMajorlen+1,lnColorLen)
  *--Check Deylots.

  IF llDyelot AND llRPPrnDye AND &lcFabric..CDYE_FLG = 'Y'
    *-- So We Have Dyelots to print
    SELECT (lcFabDye1)
    IF SEEK(&lcFabric..Style)
      SCAN REST WHILE &lcFabDye1..STYLE+&lcFabDye1..CWARECODE+&lcFabDye1..DYELOT = &lcFabric..Style FOR !EMPTY(&lcFabDye1..Dyelot)
*        WAIT WINDOW LANG_Mamatlst_Dyelots + &lcFabric..Fabric + LANG_Mamatlst_Color + SUBSTR(&lcFabric..STYLE,lnMajorlen+1,lnColorLen) NOWAIT
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Mamatlst_Dyelots + &lcFabric..STYLE  NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Mamatlst_Dyelots,oAriaApplication.GetHeaderText("LANG_Mamatlst_Dyelots",AHEADERFILE)) + &lcFabric..STYLE  NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

        SCATTE MEMVAR MEMO
        SELECT (WORKFILE)
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE CDYE_FLG WITH &lcFabric..CDYE_FLG,;
                UseDye   WITH .T.,;
                ITEMTYPE WITH 'B'
      ENDSCAN

    ENDIF
  ENDIF
  *--Check Print House Details.
  IF llRPPntW
    SELECT(WORKFILE)
    GO BOTTOM
    lnRecFld=RECNO()
    IF lfGetWLine(&lcFabric..Style)         && calling function to get warehous detail
      SELECT(WORKFILE)
      IF BETWEEN(lnRecFld,1,RECCOUNT())
        GO lnRecFld
        REPLACE  NEXTDYE WITH  .T. ,;
                 NEXTWARE WITH .T. ,;
                 ITEMTYPE WITH 'B'
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

SELECT (WORKFILE)
Z = LTRIM(STR(RECCOUNT(),7))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Mamatlst_Sorting + Z + LANG_Mamatlst_Records NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Mamatlst_Sorting,oAriaApplication.GetHeaderText("LANG_Mamatlst_Sorting",AHEADERFILE)) + Z + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Mamatlst_Records,oAriaApplication.GetHeaderText("LANG_Mamatlst_Records",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

SET ORDER TO TAG SortIdx
*SET RELATION TO VENDOR INTO &lcVendFile
SELECT (lcFabric)
SET RELATION TO
SELECT (WORKFILE)
GO BOTTOM
REPLACE lLast WITH .T.
GO TOP
IF lcRpForm='MAMATLSB'
  loOGScroll.cCRPaperSize = 'A4'
ENDIF
loogScroll.cCROrientation = 'L'
DO gfDispRe WITH EVAL('lcRPForm')

*!*************************************************************
*! Name      : lfClearRed
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/22/2004
*! Purpose   : clear read
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfClearRed()
*!*************************************************************
FUNCTION lfClearRed
ClearRead()

*!*************************************************************
*! Name      : lfvBins *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Functio for bin
*!           :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvBins()
*!*************************************************************
FUNCTION lfvBins

*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
*= gfMover(@laRpSource,@laRpTarget,'Style Bin',.T.,'')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,Lang_Style_Bin,.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,Lang_Style_Bin,.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]


*!*************************************************************
*! Name      : lfFillBin  *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Fill Bins
*!           :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
FUNCTION lfFillBin

** gfOpenFile must be changed to use SqlRun with cAriaNativeDataFilesConStr
llUSdBy =gfOpenFile(oAriaApplication.DataDir+'WHSLOC' ,'WHSLOC','SH')
DIME laRpSource[1,1]
DIME laRpTarget[1,1]
SELECT WHSLOC
SELECT DISTINCT CLOCATION FROM WHSLOC WHERE !EMPTY(CLOCATION)INTO ARRAY laRpSource
USE IN IIF(llUSdBy,'WHSLOC',0)



*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/22/2004
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Get fields and some of the Option grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*C102161,1 KAM
FUNCTION lfwOldVal
lcOldVal = EVALUATE(OGSYS18(.T.))      && Varible to hold the old value
RETURN

*--end function lfwOldVal


!*************************************************************
*! Name      : lfGetWLine
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/22/2004
*! Purpose   : get the onhand amount for warehouses
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfGetWLine()
*!*************************************************************

FUNCTION lfGetWLine
PARAMETERS lcFabClr
PRIVATE lcOldArea,lnRecNo,llRetVal,lcExp

lcOldArea=SELECT()
STORE .F. TO llRetVal
lnRecNo  =RECNO()
SELECT (WORKFILE)
lcOrder  =ORDER()
SET ORDER TO TAG WareIdx
SELECT (lcFabdye1)
IF SEEK(lcFabClr)
  SCAN REST WHILE &lcFabDye1..STYLE+&lcFabDye1..CWARECODE+&lcFabDye1..DYELOT = lcFabClr FOR EMPTY(&lcFabDye1..dyelot)
    SCATTER MEMVAR MEMO
    lcSrchKey=&lcFabdye1..Style+&lcFabdye1..cWARECODE + 'C'
    SELECT (WORKFILE)
    IF !SEEK(lcSrchKey)  .AND. lcFabClr = &lcFabdye1..Style
      STORE .T. TO llRetVal
      INSERT INTO (WORKFILE) FROM MEMVAR
    ELSE
      REPLACE ONHAND   WITH &lcFabDye1..ONHAND +M.ONHAND
      REPLACE ONORDER  WITH &lcFabDye1..ONORDER +M.ONORDER
      REPLACE NSTKVAL  WITH &lcFabDye1..NSTKVAL +M.NSTKVAL
      REPLACE USAGE    WITH &lcFabDye1..USAGE +M.USAGE
    ENDIF

    REPLACE CONV  WITH &lcFabric..CONV
    REPLACE COLOR WITH SUBSTR(&lcFabric..STYLE,lnMajorlen+1,lnColorLen)
    REPLACE UseDye   WITH .T.
    REPLACE ITEMTYPE WITH 'C'
  ENDSCAN

ENDIF &&SEEK(lcFabDye1)
SET ORDER TO TAG SortIdx IN (WORKFILE)
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
RETURN llRetVal

*!*************************************************************
*! Name      : lfSumFab1
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/21/2004
*! Purpose   : sum a specific field for the current fabric
*!                  in fabric file
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************

FUNCTION lfSumFab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF

SELECT(lnAlias)

RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfSumFab2
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/26/2004
*! Purpose   : sum a specific field for the current fabric
*!                  in item file
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfSumFab2
PARAMETERS lcFab,lcComp
lnTotComp = 0
lnSelected=SELECT()
SELECT(lcFabDye1)
IF RECCOUNT() != 0
  lnStyRec = IIF(BETWEEN(RECNO(lcFabDye1),1,RECCOUNT(lcFabDye1)),RECNO(lcFabDye1),1)  && Return the Current Record
  IF SEEK(ALLTRIM(lcFab))
*HMA
*    SUM &lcCOMP TO lnTotComp WHILE  ALLTRIM(STYLE) = ALLTRIM(lcFab)  .AND. EMPTY(&lcFabDye1..DYELOT)
    SUM &lcCOMP TO lnTotComp WHILE  STYLE = PADR(lcFab,19)  .AND. EMPTY(&lcFabDye1..DYELOT)
*HMA
  ENDIF
  GO lnStyRec
ENDIF
SELECT(lnSelected)
*B125737 ,1 HMA  01/09/2005 Return the whole vaue of numeric field value instead of return only its integer value [BEGIN]
*RETURN INT(lnTotcomp)
RETURN lnTotcomp
*B125737 ,1 HMA  01/09/2005 Return the whole vaue of numeric field value instead of return only its integer value [END]

*!*************************************************************
*! Name      : lfBldSqlCur
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/22/2004
*! Purpose   : Build Sql Cursors Needed For Getting Data
*!*************************************************************
*! Example   : = lfBldSqlCur()
*!*************************************************************

FUNCTION lfBldSqlCur
PARAMETERS lcFilter,lcFound,lcCursor,lcFldName,lcSntFld
LOCAL   lnPosition,lnFltPos,lnRow,lcExpression
STORE 0 TO lnPosition,lnFltPos,lnRow
STORE '' TO lcExpression
lnFltPos = ASCAN(loOGScroll.laOGFxFlt,lcFilter)
IF lnFltPos > 0
  lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnFltPos,1)
  lcTmpCur = loOGScroll.laOGFxFlt[lnRow,6]
  IF !EMPTY(lcTmpCur)  && user selected some styles.
    SELECT &lcTmpCur
    &lcFound = (RECCOUNT() > 0)
    IF &lcFound
      &lcCursor = loOgScroll.gfSQLTempName('',lcFldName,lcTmpCur,lcSntFld) && SQL Temp File
      IF EMPTY(&lcCursor)
        *-- SQL connection Error. Can't open The Report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF
RETURN &lcCursor




*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 1/11/2004
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : MARIAM MAZHAR (MMT)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
FUNCTION lfCreatExp
  IF TYPE('llCallFromScr') = 'L'
    =ACOPY(loOGScroll.laOGFxFlt , laFxExpr)
    =ACOPY(loOGScroll.laOGVrFlt , laVrExpr)
  ENDIF
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF
  RETURN lcStatusStr
ENDFUNC
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]

