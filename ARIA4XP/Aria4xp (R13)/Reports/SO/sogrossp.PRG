*:***************************************************************************
*: Program file  				: SOGROSSP
*: Program desc. 				: Gross profit Based on sales order
*: Module        				: Account receivable (AR)
*: Developer     				: HEBA FATHI   (HFK)
*: Tracking Job Number 	: 037333
*! Date          				: 12/21/2003
*:***************************************************************************
*: Calls :
*:    Programs 					: ....
*:    Screens	      		: ....
*:    Global Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOGROSSP
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*! B608821,1 MMT 03/17/2009 Don't Display Zero Qty Lines [T20080430.0019]
*! E302638,1 MMT 09/24/2009 add option deduct trade disc in gross profit reports(So,AR)[T20090724.0021]
*! E302748,1 SMA 09/02/2010 Modify gross profit reports(SO,AR)to deduct the trade discount per line [T20100120.0060]
*! E303715,1 MMT 09/27/2016 Convert So gross profit report to crystal and add sales rep sort by [P20160601.0001 - Issue#31]
*! B611302,1 MHM 05/2/2017  order grossprofit [T20170421.0013]
*! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [T20170421.0013]

*!***************************************************************************
*-- Check if the user selects foreign currency and did not select
*--  a currency then use the base currency.
#INCLUDE R:\Aria4xp\reports\so\SOGROSSP.H
llMltiWr = UPPER(gfGetMemVar("M_WAREHOUS",oAriaApplication.ActiveCompanyID))='Y'

IF lcRpCurr = 'F' AND EMPTY(lcCurrCod)
  lcCurrCod = oAriaApplication.BaseCurrency
ENDIF

*-- Creat array hold the trade discount value from the codes file.
lnTradDisc = 0
DECLARE laTrdDis[1,2]
laTrdDis[1,1] = 'NTERDISCR'
laTrdDis[1,2] = 'lnTradDisc'

*-- Make sure the filter runs if the second line in the order contains
*-- the selected one sryle group.
lcStyGroup = " .T. "
*-- HFK, 02/10/2005, modify expression to avoid problems when selecting more than 24 style group
*!*  lnStyPos = ATC("INLIST(STYLE.CSTYG",lcRpExp)
*!*  IF lnStyPos <> 0
*!*    llLast = (ATC('AND',SUBSTR(lcRpExp,lnStyPos)) =0)
*!*    lcStyGroup = SUBSTR(lcRpExp,lnStyPos,IIF(llLast,LEN(lcRpExp),ATC('AND',SUBSTR(lcRpExp,lnStyPos))-1))
*!*    lcRpExp = STRTRAN(lcRpExp,lcStyGroup,".T.")
*!*  ENDIF
lcNewExp = ""
IF !EMPTY(loOGScroll.lcRpExp)
  IF ATC('INLIST(STYLE.CSTYGROUP',loOGScroll.lcRpExp)> 0 .OR. ATC('ORDHDR.CDIVISION',loOGScroll.lcRpExp)> 0 ;
      .OR. ATC('ORDHDR.SEASON',loOGScroll.lcRpExp)> 0
    lnOccur= OCCURS(' AND',loOGScroll.lcRpExp)
    IF lnOccur > 0
      FOR lnCount= 1 TO lnOccur+1
        lnStart = IIF(lnCount=1,1,ATC(' AND',loOGScroll.lcRpExp,lnCount-1)+5)
        lnEnd = IIF(lnCount = lnOccur+1,LEN(loOGScroll.lcRpExp)+1,ATC(' AND',loOGScroll.lcRpExp,lnCount))
        lnLength = lnEnd - lnStart
        lcScatteredExp = SUBSTR(loOGScroll.lcRpExp,lnStart,lnLength)
        DO CASE
        CASE ATC('INLIST(STYLE.CSTYGROUP',lcScatteredExp)> 0
          lcGroupExp = SUBSTR(lcScatteredExp,ATC(',',lcScatteredExp,1)+1,LEN(lcScatteredExp)-1)
          lcGroupExp = STRTRAN(lcGroupExp,',',' Or Style.cStyGroup = ')
          lcGroupExp = STRTRAN(lcGroupExp,')','')
          lcGroupExp = '( STYLE.CSTYGROUP = '+lcGroupExp+')'
          lcStyGroup = lcGroupExp
          lcNewExp = IIF(EMPTY(lcNewExp),lcGroupExp,lcNewExp+' AND ' +lcGroupExp)
        CASE ATC('ORDHDR.CDIVISION',lcScatteredExp)> 0
          lcDivisionExp = SUBSTR(lcScatteredExp,ATC(',',lcScatteredExp,1)+1,LEN(lcScatteredExp)-1)
          lcDivisionExp = STRTRAN(lcDivisionExp,',',' Or ORDHDR.CDIVISION = ')
          lcDivisionExp = STRTRAN(lcDivisionExp,')','')
          lcDivisionExp = '( ORDHDR.CDIVISION = '+lcDivisionExp+')'
          lcNewExp = IIF(EMPTY(lcNewExp),lcDivisionExp,lcNewExp+' AND ' +lcDivisionExp)
        CASE ATC('ORDHDR.SEASON',lcScatteredExp)> 0
          lcSeasonExp = SUBSTR(lcScatteredExp,ATC(',',lcScatteredExp,1)+1,LEN(lcScatteredExp)-1)
          lcSeasonExp = STRTRAN(lcSeasonExp,',',' Or ORDHDR.SEASON= ')
          lcSeasonExp = STRTRAN(lcSeasonExp,')','')
          lcSeasonExp = '( ORDHDR.SEASON = '+lcSeasonExp+')'
          lcNewExp = IIF(EMPTY(lcNewExp),lcSeasonExp,lcNewExp+' AND ' +lcSeasonExp)
        OTHERWISE
          lcNewExp = IIF(EMPTY(lcNewExp),lcScatteredExp,lcNewExp+' AND ' +lcScatteredExp)
        ENDCASE
      ENDFOR
    ELSE   && EXPRESSION HAVE ONLY ONE VARIABLE
      lcScatteredExp = loOGScroll.lcRpExp
      DO CASE
      CASE ATC('INLIST(STYLE.CSTYGROUP',lcScatteredExp)> 0
        lcGroupExp = SUBSTR(lcScatteredExp,ATC(',',lcScatteredExp,1)+1,LEN(lcScatteredExp)-1)
        lcGroupExp = STRTRAN(lcGroupExp,',',' Or Style.cStyGroup = ')
        lcGroupExp = STRTRAN(lcGroupExp,')','')
        lcGroupExp = '( STYLE.CSTYGROUP = '+lcGroupExp+')'
        lcStyGroup = lcGroupExp
        lcNewExp = IIF(EMPTY(lcNewExp),lcGroupExp,lcNewExp+' AND ' +lcGroupExp)
      CASE ATC('ORDHDR.CDIVISION',lcScatteredExp)> 0
        lcDivisionExp = SUBSTR(lcScatteredExp,ATC(',',lcScatteredExp,1)+1,LEN(lcScatteredExp)-1)
        lcDivisionExp = STRTRAN(lcDivisionExp,',',' Or ORDHDR.CDIVISION = ')
        lcDivisionExp = STRTRAN(lcDivisionExp,')','')
        lcDivisionExp = '( ORDHDR.CDIVISION = '+lcDivisionExp+')'
        lcNewExp = IIF(EMPTY(lcNewExp),lcDivisionExp,lcNewExp+' AND ' +lcDivisionExp)
      CASE ATC('ORDHDR.SEASON',lcScatteredExp)> 0
        lcSeasonExp = SUBSTR(lcScatteredExp,ATC(',',lcScatteredExp,1)+1,LEN(lcScatteredExp)-1)
        lcSeasonExp = STRTRAN(lcSeasonExp,',',' Or ORDHDR.SEASON= ')
        lcSeasonExp = STRTRAN(lcSeasonExp,')','')
        lcSeasonExp = '( ORDHDR.SEASON = '+lcSeasonExp+')'
        lcNewExp = IIF(EMPTY(lcNewExp),lcSeasonExp,lcNewExp+' AND ' +lcSeasonExp)
      OTHERWISE
        lcNewExp = IIF(EMPTY(lcNewExp),lcScatteredExp,lcNewExp+' AND ' +lcScatteredExp)
      ENDCASE
    ENDIF
  ELSE
    lcNewExp = loOGScroll.lcRpExp
  ENDIF
  loOGScroll.lcRpExp = lcNewExp
ENDIF
*-- HFK, 02/10/2005, modify expression to avoid problems when selecting more than 24 style group


lcSPTime = TIME()         && Variable to hold the print Time
lcNewExp = SPACE(0)            && VARIABLE TO CARRY lcRpExp + sales rep
IF "REP1" $ lcRpExp
  =lfAddFxOr("ORDHDR.REP1","ORDHDR.REP2")
ELSE
  lcNewExp = lcRpExp
ENDIF

*-- To get orders only
*-- Handle the case of choose empty currency
IF EMPTY(lcCurrCod)
  lcNewExp= lcNewExp +'.AND. ORDHDR.CORDTYPE$"O"'+'.AND. ORDHDR.STATUS$lcRpStatus'
ELSE
  lcNewExp= lcNewExp +'.AND. ORDHDR.CORDTYPE$"O"'+'.AND. ORDHDR.STATUS$lcRpStatus'+'.AND. ORDHDR.cCurrcode$lcCurrCod'
ENDIF

SELECT ORDHDR
*-hfk, 08/09/2004, added this variable to set relation when it is not set
LOCAL llRelation
IF llRelation = .F.
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + STORE INTO CUSTOMER
  SET RELATION TO CordType + ORDER INTO ORDLINE ADDITIVE
  *-HFK, 05/15/2005
  SELECT ORDLINE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET RELATION TO STYLE INTO STYDYE ADDITIVE
  SELECT ORDHDR
  *-HFK, 05/15/2005
  llRelation = .T.
ENDIF
LOCATE
LOCATE FOR &lcNewExp
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display'
  RETURN
ENDIF
*--VARIABLES SECTION
*-- lcRpTmpN                         Temp file to collect data(difined at syrepuver)
*-- lcRpTmpNam                        Temp file for currancy (difined at syrepuver)
*-- lcRpTitle                         variable to print title at FRX. (difined at syrepuver)
*-- llMltiWr : variable for Multi ware house (difined at syrepuver)
*-- lcMarkUp : variable for Mark up (difined at syrepuver)
*-- lcCostMth                        variable to hold cost method (define at syrepuvr)
*-- lnCostValu                        && variable to get cost
*-- lnpieces                          && variable to get pieces
*-- lns_price                         && variable to get price
*-- lnGrsAmnt                         && variable to get gross amount
*-- lnNetAmnt                         && variable to get net amount
*-- lnProfit                          && variable to get profit
*-- lnPrftPrct                        && variable to get profit percentage
*-- llRpshow                           && Variable to show Grand Total
*-- lcSvAlis                         to get alias
STORE 0 TO lnCostValu,lnpieces,lns_price,lnGrsAmnt,lnNetAmnt, lnProfit, lnPrftPrct ,LNTAMNT

*-- lcGrpVar = IIF(lcRpSortBy = 'O','ORDER',IIF(lcRpSortBy = 'A','ACCOUNT',IIF(lcRpSortBy = 'S','STYLE','CLASS')))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpVar = IIF(lcRpSortBy = 'O',LANG_SoGrossp_Order,IIF(lcRpSortBy = 'A',LANG_SoGrossp_Account,IIF(lcRpSortBy = 'S',LANG_SoGrossp_Style,LANG_SoGrossp_Class)))
*E303715,1 MMT 09/27/2016 Convert So gross profit report to crystal and add sales rep sort by [Start]
*lcGrpVar = IIF(lcRpSortBy = 'O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Order,oAriaApplication.GetHeaderText("LANG_SoGrossp_Order",AHEADERFILE)),IIF(lcRpSortBy = 'A',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Account,oAriaApplication.GetHeaderText("LANG_SoGrossp_Account",AHEADERFILE)),IIF(lcRpSortBy = 'S',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Style,oAriaApplication.GetHeaderText("LANG_SoGrossp_Style",AHEADERFILE)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Class,oAriaApplication.GetHeaderText("LANG_SoGrossp_Class",AHEADERFILE)))))
lcGrpVar = IIF(lcRpSortBy = 'O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
  LANG_SoGrossp_Order,oAriaApplication.GetHeaderText("LANG_SoGrossp_Order",AHEADERFILE)),;
  IIF(lcRpSortBy = 'A',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Account,;
  oAriaApplication.GetHeaderText("LANG_SoGrossp_Account",AHEADERFILE)),IIF(lcRpSortBy = 'S',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Style,;
  oAriaApplication.GetHeaderText("LANG_SoGrossp_Style",AHEADERFILE)),;
  IIF(lcRpSortBy = 'R',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
  LANG_SoGrossp_Sales,oAriaApplication.GetHeaderText("LANG_SoGrossp_Sales",AHEADERFILE)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Class,oAriaApplication.GetHeaderText("LANG_SoGrossp_Class",AHEADERFILE))))))
*E303715,1 MMT 09/27/2016 Convert So gross profit report to crystal and add sales rep sort by [End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcGrpCurr = cCurrCode
llSameGrp = .T.
*-- END VARIABLE SECTION

*--COLLECT DATA
DO lpCreateTmp
=lfIndex()              && SET INDEX TO THE FILE ACCORDING TO TYPE

DO lpColect
SELECT (lcRpTmpN)
*!-- Printing Section
GO TOP

*--Select Summary / Detailes reports
IF   lcRPSumDet ="S"
  lcRpName = "SOGROSPS"
ELSE
  lcRpName = "SOGROSPD"
ENDIF
*! E303715,1 MMT 09/27/2016 Convert So gross profit report to crystal and add sales rep sort by [P20160601.0001 - Issue#31][Start]
*loogScroll.cCROrientation = 'P'
DIMENSION loOGScroll.laCRTables[1]
COPY TO oAriaApplication.WorkDir + lcRpTmpN + ".DBF"
loOGScroll.laCRTables[1] = oAriaApplication.WorkDir+lcRpTmpN+ ".DBF"

USE IN (lcRpTmpN)

loOGScroll.lcOGLastForm = lcRpName

loOGScroll.cCROrientation = 'L'

DIMENSION loOGScroll.laCRParams[14,2]
loOGScroll.laCRParams[1,1]='lcStyTitle'
loOGScroll.laCRParams[1,2]=lcStyTitle
loOGScroll.laCRParams[2,1]='gcCom_Name'
loOGScroll.laCRParams[2,2]=gcCom_Name
loOGScroll.laCRParams[3,1]='lcRpTitle'
loOGScroll.laCRParams[3,2]=lcRpTitle
loOGScroll.laCRParams[4,1]='gcUser_Id'
loOGScroll.laCRParams[4,2]=gcUser_Id
loOGScroll.laCRParams[5,1]='lcSPTime'
loOGScroll.laCRParams[5,2]=lcSPTime
loOGScroll.laCRParams[6,1]='cCurrCode'
loOGScroll.laCRParams[6,2]=lcCurrCod
loOGScroll.laCRParams[7,1]='GProfit'
loOGScroll.laCRParams[7,2]=IIF(lcRpSortBy = 'S',"STYLE",IIF(lcRpSortBy = 'A',"ACCOUNT",IIF(lcRpSortBy = 'O',"ORDER",IIF(lcRpSortBy = 'C',"CLASS","SALES REP"))))
loOGScroll.laCRParams[8,1]='gdSysDate'
loOGScroll.laCRParams[8,2]=gdSysDate
loOGScroll.laCRParams[9,1]='lcRpDeciml'
loOGScroll.laCRParams[9,2]=lcRpDeciml
loOGScroll.laCRParams[10,1]='lcGrpVar'
loOGScroll.laCRParams[10,2]=lcGrpVar
loOGScroll.laCRParams[11,1]='lcRpSortBy'
loOGScroll.laCRParams[11,2]=lcRpSortBy
loOGScroll.laCRParams[12,1]='LCRPMARK'
loOGScroll.laCRParams[12,2]=LCRPMARK
loOGScroll.laCRParams[13,1]='LCRPCURR'
loOGScroll.laCRParams[13,2]=lcRpCurr
loOGScroll.laCRParams[14,1]='GCBASECURR'
loOGScroll.laCRParams[14,2]=GCBASECURR
*! E303715,1 MMT 09/27/2016 Convert So gross profit report to crystal and add sales rep sort by [P20160601.0001 - Issue#31][End]
DO gfDispRe WITH EVALUATE('lcRpName')
*!-- End of Printing Section

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************
*!
PROCEDURE lpColect
*-- Relation to collect data

*!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
M_TrdDiscL = gfGetMemVar('M_TRDDISCL')
*!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]
SELECT ORDHDR

IF TYPE('llRelation')='U'
  LOCAL llRelation
ENDIF
IF llRelation = .F.
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + STORE INTO CUSTOMER
  SET RELATION TO CordType + ORDER INTO ORDLINE ADDITIVE
  llRelation = .T.  && added this variable to detect relation set.
ENDIF
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE
*-- Don't overwrite the relation: add ADDITIVE
SET RELATION TO STYLE INTO STYDYE ADDITIVE
SELECT ORDHDR
=SEEK('O'+ORDER,'ORDHDR')
SCAN FOR &lcNewExp
  *-- Get the trade discount for the order.
  =gfRltFld(ORDHDR.CTERMCODE , @laTrdDis , 'CTERMCODE')
  SELECT ORDLINE
  *-- Make sure the filter runs if the second line in the order contains
  *-- the selected one sryle group.
  *! B608821,1 MMT 03/17/2009 Don't Display Zero Qty Lines [Start]
  *  SCAN REST WHILE order = ORDHDR.order
  SCAN REST WHILE ORDER = ORDHDR.ORDER FOR TOTQty <> 0
    *! B608821,1 MMT 03/17/2009 Don't Display Zero Qty Lines [End]
    IF !&lcStyGroup
      LOOP
    ENDIF
    *--Make sure the filter runs if the second line in the order contains

    SCATTER MEMVAR MEMO
    *-- check if avarge cost or standerd , if avarge check if multi warehouse
    IF lcCostMth = 'A'
      IF llMltiWr = .T.
        =SEEK(STYLE+cwarecode,'STYDYE')
        *--In Case of muli curr. calculate the var by function lfGetFcurr
        IF llMultCurr
          SELECT ORDHDR
          lnCostValu = lfGetFCurr(STYDYE.Ave_cost, lcRpCurr , ldRpExDate , lcRpTmpNam)
          SELECT ORDLINE
        ELSE
          lnCostValu = STYDYE.Ave_cost
        ENDIF
      ELSE
        =SEEK(STYLE,'STYLE')
        *-- In Case of muli curr. calculate the var by function lfGetFcurr
        IF llMultCurr
          SELECT ORDHDR
          lnCostValu = lfGetFCurr(STYLE.Ave_cost, lcRpCurr , ldRpExDate , lcRpTmpNam)
          SELECT ORDLINE
        ELSE
          lnCostValu = STYLE.Ave_cost
        ENDIF
      ENDIF
    ELSE
      =SEEK(STYLE,'STYLE')
      lnCostValu = STYLE.TotCost
    ENDIF
    *-- get CCurrCode to use at  gfAmntDisp and insert into temp table
    m.cCurrCode = ORDHDR.cCurrCode
    lnpieces =TOTQty
    lns_price =price
    *--Calculate the gros amount.[START]
    lnGrsAmnt =TOTQty*price
    lnNetAmnt =TOTQty*price
    lnNetAmnt1=lnNetAmnt
    lnProfit =lnNetAmnt-lnCostValu*TOTQty
    *-- get profit percentage
    *---mhm lcMarkUp
    IF LCRPMARK = 'T'
      IF lcRpCurr = "F"
        lcRpCurr = "O"
        lcSvAlis = ALIAS()
        lnNetAmnt1=lnNetAmnt
        SELECT ORDHDR
        price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
        lnNetAmnt =TOTQty*price
        LNTAMNT = lnNetAmnt
        lnProfit =lnNetAmnt-lnCostValu*TOTQty
        lnPrftPrct = IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TOTQty))*100,0)
        lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TOTQty))<0 .AND. lnCostValu<0),-lnPrftPrct,lnPrftPrct)
        *-- To avoid deviding by zero
        lnPrftPrct = IIF(lnCostValu =0 .OR. TOTQty = 0,0,(lnProfit/(lnCostValu*TOTQty))*100)
        SELECT (lcSvAlis)
        lcRpCurr = "F"
        lnNetAmnt=lnNetAmnt1
      ELSE
        lcSvAlis = ALIAS()
        lnNetAmnt1=lnNetAmnt
        SELECT ORDHDR
        price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
        lnNetAmnt =TOTQty*price
        LNTAMNT = lnNetAmnt
        lnProfit =lnNetAmnt-lnCostValu*TOTQty
        lnPrftPrct = IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TOTQty))*100,0)
        lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TOTQty))<0 .AND. lnCostValu<0),-lnPrftPrct,lnPrftPrct)
        *-- To avoid deviding by zero
        lnPrftPrct = IIF(lnCostValu = 0 .OR. TOTQty = 0, 0,(lnProfit/(lnCostValu*TOTQty))*100)
        SELECT (lcSvAlis)
        lnNetAmnt=lnNetAmnt1
      ENDIF
    ELSE
      IF lcRpCurr = "F"
        lcRpCurr = "O"
        lcSvAlis = ALIAS()
        lnNetAmnt1=lnNetAmnt
        SELECT ORDHDR
        price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
        lnNetAmnt =TOTQty*price
        LNTAMNT = lnNetAmnt
        lnProfit =lnNetAmnt-lnCostValu*TOTQty
        lnPrftPrct = IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0)
        lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TOTQty))<0 .AND. lnNetAmnt<0),-lnPrftPrct,lnPrftPrct)
        lnPrftPrct = IIF(lnNetAmnt = 0 ,0,(lnProfit/lnNetAmnt)*100)
        lcRpCurr = "F"
        SELECT (lcSvAlis)
        lcRpCurr = "F"
        lnNetAmnt=lnNetAmnt1
      ELSE
        lcSvAlis = ALIAS()
        lnNetAmnt1=lnNetAmnt
        SELECT ORDHDR
        price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
        lnNetAmnt =TOTQty*price
        LNTAMNT = lnNetAmnt
        lnProfit =lnNetAmnt-lnCostValu*TOTQty
        lnPrftPrct = IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0)
        lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TOTQty))<0 .AND. lnNetAmnt<0),-lnPrftPrct,lnPrftPrct)
        lnPrftPrct = IIF(lnNetAmnt = 0 ,0,(lnProfit/lnNetAmnt)*100)
        SELECT (lcSvAlis)
        lnNetAmnt=lnNetAmnt1
      ENDIF
    ENDIF
    *--IF multi currancy calculate it.
    IF lcRpCurr = "F" OR lnNetAmnt = 0
      m.NetAmnt = lnNetAmnt
    ELSE
      lcSvAlis = ALIAS()
      SELECT ORDHDR
      m.NetAmnt =gfAmntDisp(lnNetAmnt,lcRpCurr,ldRpExDate,lcRpTmpNam)
      lnProfit =NetAmnt-lnCostValu*TOTQty
      SELECT (lcSvAlis)
    ENDIF

    IF lcRpCurr="F" OR lnGrsAmnt=0
      m.GROS_PRICE = lnGrsAmnt
    ELSE
      lcSvAlis = ALIAS()
      SELECT ORDHDR
      m.GROS_PRICE =gfAmntDisp(lnGrsAmnt,lcRpCurr,ldRpExDate,lcRpTmpNam)
      SELECT (lcSvAlis)
    ENDIF

    IF lcRpCurr="F" OR lns_price=0
      m.price = lns_price
    ELSE
      lcSvAlis = ALIAS()
      SELECT ORDHDR
      m.price =gfAmntDisp(lns_price,lcRpCurr,ldRpExDate,lcRpTmpNam)
      SELECT (lcSvAlis)
    ENDIF
    *-cost cannot be change(cost in base currancy) Only in foreign case
    *--get totl cost
    m.Cost = lnCostValu*TOTQty
    *--IF multi currancy calculate it [ end]
    m.CodDes=gfCodDes(&lcRpTmpN..CLASS,'CLASS')  && variable to get code description
    m.TOTQty  = lnpieces
    m.ProfitPer = lnPrftPrct

    *! B611302,1 MHM 05/2/2017  order grossprofit [start]
    *m.Rep1 = ORDHDR.Rep1
    IF SEEK (ORDHDR.Rep1, 'salesrep','SALESREP')
      m.Rep1 = salesrep.NAME
    ENDIF
    *! B611302,1 MHM 05/2/2017  order grossprofit [END]

    m.CLASS = CUSTOMER.CLASS
    m.S_GROUP = STYLE.CSTYGROUP

    *! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [start]
    *m.Name = CUSTOMER.Btname
    m.Name = CUSTOMER.Account+ ' - ' +ALLTRIM(CUSTOMER.Btname)
    *! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [End]
    m.Profit = lnProfit
    *-- Calculate the net amount.
    *-- Compute the net amount based on the merchandise discount not only the trade disc.
    *! E302638,1 MMT 09/24/2009 add option deduct trade disc in gross profit reports(So,AR)[Start]
    *m.NetAmnt = m.NetAmnt - (m.NetAmnt * lnTradDisc/100) - (m.NetAmnt * OrdHdr.Disc / 100)

    *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
    IF M_TrdDiscL
      m.NetAmnt = m.NetAmnt - (m.NetAmnt * IIF(llRPTrdDsc,ORDLINE.Trde_Disc,0)/100) - (m.NetAmnt * ORDHDR.Disc / 100)
    ELSE
      *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]
      m.NetAmnt = m.NetAmnt - (m.NetAmnt * IIF(llRPTrdDsc,lnTradDisc,0)/100) - (m.NetAmnt * ORDHDR.Disc / 100)
      *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
    ENDIF
    *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]

    *! E302638,1 MMT 09/24/2009 add option deduct trade disc in gross profit reports(So,AR)[End]
    m.Profit  = m.NetAmnt - m.Cost
    *-- Calculate the Profit % .
    IF LCRPMARK = 'T'
      lnPrftPrct = IIF(lnCostValu <> 0 , (m.Profit/m.Cost)*100 , 0)
      lnPrftPrct = IIF(((m.NetAmnt - m.Cost )<0 .AND. lnCostValu<0) , -lnPrftPrct , lnPrftPrct)
      lnPrftPrct = IIF(lnCostValu = 0 .OR. TOTQty = 0 , 0 , (m.Profit/m.Cost)*100)
    ELSE
      lnPrftPrct = IIF(m.NetAmnt <> 0,(m.Profit/m.NetAmnt)*100,0)
      lnPrftPrct = IIF(((m.NetAmnt - m.Cost)<0 .AND. m.NetAmnt<0),-lnPrftPrct,lnPrftPrct)
      lnPrftPrct = IIF(m.NetAmnt = 0 ,0,(m.Profit/m.NetAmnt)*100)
    ENDIF
    m.ProfitPer = ROUND(lnPrftPrct,2)
    INSERT INTO (lcRpTmpN) FROM MEMVAR
  ENDSCAN
ENDSCAN
SELECT ORDHDR
SET RELATION TO
llRelation = .F.
*- end lpColect
*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 14/06/2000
*! Purpose   : Craete temp. file
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCreateTmp
*!*************************************************************

PROCEDURE lpCreateTmp
IF USED(lcRpTmpN)
  SELECT (lcRpTmpN)
  ZAP
ELSE
  *--HFK, Create Cursor
  DIMENSION laRpTmpN[20,4]
  laRpTmpN[1,1]='Order'
  laRpTmpN[1,2]='C'
  laRpTmpN[1,3]=6
  laRpTmpN[1,4]=0

  laRpTmpN[2,1]='Style'
  laRpTmpN[2,2]='C'
  laRpTmpN[2,3]=19
  laRpTmpN[2,4]=0

  laRpTmpN[3,1]='CLASS'
  laRpTmpN[3,2]='C'
  laRpTmpN[3,3]=6
  laRpTmpN[3,4]=0

  laRpTmpN[4,1]='S_GROUP'
  laRpTmpN[4,2]='C'
  laRpTmpN[4,3]=6
  laRpTmpN[4,4]=0

  laRpTmpN[5,1]='Profit'
  laRpTmpN[5,2]='N'
  laRpTmpN[5,3]=13
  laRpTmpN[5,4]=2

  laRpTmpN[6,1]='ProfitPer'
  laRpTmpN[6,2]='N'
  laRpTmpN[6,3]=7
  laRpTmpN[6,4]=3

  laRpTmpN[7,1]='LINENO'
  laRpTmpN[7,2]='N'
  laRpTmpN[7,3]=6
  laRpTmpN[7,4]=0

  laRpTmpN[8,1]='PRICE'
  laRpTmpN[8,2]='N'
  laRpTmpN[8,3]=12
  laRpTmpN[8,4]=2

  laRpTmpN[9,1]='GROS_PRICE'
  laRpTmpN[9,2]='N'
  laRpTmpN[9,3]=12
  laRpTmpN[9,4]=2

  laRpTmpN[10,1]='TOTQTY'
  laRpTmpN[10,2]='N'
  laRpTmpN[10,3]=6
  laRpTmpN[10,4]=0

  laRpTmpN[11,1]='COST'
  laRpTmpN[11,2]='N'
  laRpTmpN[11,3]=10
  laRpTmpN[11,4]=2

  laRpTmpN[12,1]='cCurrCode'
  laRpTmpN[12,2]='C'
  laRpTmpN[12,3]=3
  laRpTmpN[12,4]=0

  laRpTmpN[13,1]='Nexrate'
  laRpTmpN[13,2]='N'
  laRpTmpN[13,3]=9
  laRpTmpN[13,4]=4

  laRpTmpN[14,1]='Ncurrunit'
  laRpTmpN[14,2]='N'
  laRpTmpN[14,3]=4
  laRpTmpN[14,4]=0


  laRpTmpN[15,1]='REP1'
  laRpTmpN[15,2]='C'
  *! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [start]
  *laRpTmpN[15,3]= 3
  laRpTmpN[15,3]= 32
  *! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [END]
  laRpTmpN[15,4]=0



  laRpTmpN[16,1]='NetAmnt'
  laRpTmpN[16,2]='N'
  laRpTmpN[16,3]=13
  laRpTmpN[16,4]=2 

  laRpTmpN[17,1]='Account'
  laRpTmpN[17,2]='C'
  laRpTmpN[17,3]=5
  laRpTmpN[17,4]=0

  laRpTmpN[18,1]='Status'
  laRpTmpN[18,2]='C'
  laRpTmpN[18,3]=1
  laRpTmpN[18,4]=0

  laRpTmpN[19,1]='Name'
  laRpTmpN[19,2]='C'

  *! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [start]
  *laRpTmpN[19,3]=30
  laRpTmpN[19,3]=40

  *! B611302,1 MHM 05/2/2017  modify gross profit report in summary when sortby salesrep and account [End]
  laRpTmpN[19,4]=0

  laRpTmpN[20,1]='CodDes'
  laRpTmpN[20,2]='C'
  laRpTmpN[20,3]=30
  laRpTmpN[20,4]=0
  lcRpTmpN = loOGScroll.gfTempName()
  gfCrtTmp(lcRpTmpN,@laRpTmpN,,lcRpTmpN,.T.)
ENDIF
*--HFK ,[End]
*--end lpCreateTmp
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : HEBA FATHI    (HFK)
*! Date      : 21/12/2003
*! Purpose   : Option Grid When function
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
*--DECLARE array to store order status and make mover
DECLARE laRpSource[3],laRpTarget[1]
*!*	  STORE 'Open'     TO laRpSource[1]
*!*	  STORE 'Hold'     TO laRpSource[2]
*!*	  STORE 'Bid'      TO laRpSource[3]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_SoGrossp_Open TO laRpSource[1]
*STORE LANG_SoGrossp_Hold TO laRpSource[2]
*STORE LANG_SoGrossp_Bid  TO laRpSource[3]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Open,oAriaApplication.GetHeaderText("LANG_SoGrossp_Open",AHEADERFILE)) TO laRpSource[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Hold,oAriaApplication.GetHeaderText("LANG_SoGrossp_Hold",AHEADERFILE)) TO laRpSource[2]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Bid,oAriaApplication.GetHeaderText("LANG_SoGrossp_Bid",AHEADERFILE))  TO laRpSource[3]
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcRpStatus = 'OHB'
*--Value to be adjusted from syrepuvr file
SHOW GET lcCurrCod
*-- END lfwRepWhen
*!*************************************************************
*! Name      : lfIndex
*! Developer : HEBA FATHI    (HFK)
*! Date      : 21/12/2003
*! Purpose   : Set index to filter accourding to our selection
*!            :(Style,Account,Order,Class,Sales Rep)
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfIndex()
*!*************************************************************
FUNCTION lfIndex
PRIVATE lcSvAlias
lcSvAlias = SELECT(0)
SELECT (lcRpTmpN)
DO CASE
CASE lcRpSortBy='A'
  IF llMultCurr                          && IF MULTI CURR
    INDEX ON Account+cCurrCode+ORDER+STYLE+STR(RECNO(),7) TAG &lcRpTmpN
  ELSE
    INDEX ON Account+ORDER+STYLE+STR(RECNO(),7) TAG &lcRpTmpN
  ENDIF
  *!*lcHBreak='ACCOUNT'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcHBreak=LANG_SoGrossp_Account
  lcHBreak=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Account,oAriaApplication.GetHeaderText("LANG_SoGrossp_Account",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

CASE lcRpSortBy='O'
  IF llMultCurr                          && IF MULTI CURR
    INDEX ON cCurrCode+ORDER+STYLE+Account+STR(RECNO(),7) TAG &lcRpTmpN
  ELSE
    INDEX ON ORDER+STYLE+Account+STR(RECNO(),7) TAG &lcRpTmpN
  ENDIF
  *!*lcHBreak='ORDER'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcHBreak=LANG_SoGrossp_Order
  lcHBreak=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Order,oAriaApplication.GetHeaderText("LANG_SoGrossp_Order",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

CASE lcRpSortBy='S'
  IF llMultCurr                          && IF MULTI CURR
    INDEX ON STYLE+cCurrCode+ORDER+STR(RECNO(),7) TAG &lcRpTmpN
  ELSE
    INDEX ON STYLE+ORDER+STR(RECNO(),7) TAG &lcRpTmpN
  ENDIF
  *!*lcHBreak='STYLE'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcHBreak=LANG_SoGrossp_Style
  lcHBreak=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Style,oAriaApplication.GetHeaderText("LANG_SoGrossp_Style",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

CASE lcRpSortBy='C'
  IF llMultCurr                          && IF MULTI CURR
    INDEX ON CLASS+cCurrCode+ORDER+STYLE+STR(RECNO(),7) TAG &lcRpTmpN
  ELSE
    INDEX ON CLASS+ORDER+STYLE+STR(RECNO(),7) TAG &lcRpTmpN
  ENDIF
  *!*lcHBreak='CLASS'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcHBreak=LANG_SoGrossp_Class
  lcHBreak=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Class,oAriaApplication.GetHeaderText("LANG_SoGrossp_Class",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


ENDCASE
SELECT (lcSvAlias)
*--end lfIndex
*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  *!*	  USE (oAriaApplication.SysPath  + "SYCCOMP") ORDER TAG cComp_ID IN 0
  =gfOpenFile(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.SysPath+'cComp_ID','SH')
  llOpenComp = .T.
ENDIF
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    *!*	    USE (oAriaApplication.SysPath+"SYCINT.DBF") IN 0
    =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (oAriaApplication.SysPath+"SYCEXCH.DBF") IN 0 ORDER TAG CURRENCY
    llOpenExch = .T.
  ENDIF

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO cCurrCode  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT cCurrCode FROM SYCCURR ORDER BY cCurrCode INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = cCurrCode + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfClearRep
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 18/06/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep
IF llMultCurr
ENDIF
*-- End Of lfClearRep.
*!**************************************************************************
*! Name      : lfAddFxOr
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Create expression Filter
*!**************************************************************************
*! Passed Parameters  : Exist Field / New Field
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAddFxOr()
*!**************************************************************************
FUNCTION lfAddFxOr
PARAMETER lcExstFld,lcNewFld
*-- We will extract the filter part concerning with REP1 and add REP2 to the filter expression
IF RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) )) # 0
  *-- Get the expression part before the rep1 expression
  lcTempV = SUBSTR(lcRpExp,RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) ))+4   )
ELSE
  lcTempV = lcRpExp
ENDIF
IF ATC(')AND',lcTempV) # 0 .OR. ATC(') AND',lcTempV) # 0
  *-- Get the exact REP1 expression
  lcTempV = LEFT(lcTempV,ATC('AND',lcTempV)-1 )
ENDIF
*-- Add an expression pretty much like REP1 but of REP2
lcExpr  = '(' + lcTempV + '.OR.' + STRTRAN(lcTempV,'ORDHDR.REP1','ORDHDR.REP2') + ')'
*-- Form the new expression
lcNewExp = STRTRAN(lcRpExp,lcTempV,lcExpr)

RETURN
*-- End Of lfAddFxOr.
*!*************************************************************
*! Name      : lfvOStatus
*: Developer : HEBA FATHI    (HFK)
*! Date      : 21/12/2003
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag.
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*!***************************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

*!*	= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

* B123663,1 SMM Change gfMover to lfOGMover
*-- = gfMover(@laRpSource,@laRpTarget,LANG_SoGrossp_SelOrdStats,.T.,'')  && call mover function.

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,LANG_SoGrossp_SelOrdStats,.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_SelOrdStats,oAriaApplication.GetHeaderText("LANG_SoGrossp_SelOrdStats",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

* B123663,1 SMM End
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *!*lcRpStatus = lcRpStatus +IIF(laRpTarget[lnI] = 'Open','O',;
    *!*                         IIF(laRpTarget[lnI] = 'Hold','H',;
    *!*                         IIF(laRpTarget[lnI] = 'Bid','B','')))
    lcRpStatus = lcRpStatus +IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Open,oAriaApplication.GetHeaderText("LANG_SoGrossp_Open",AHEADERFILE)),'O',;
      IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Hold,oAriaApplication.GetHeaderText("LANG_SoGrossp_Hold",AHEADERFILE)),'H',;
      IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Bid,oAriaApplication.GetHeaderText("LANG_SoGrossp_Bid",AHEADERFILE)) ,'B','')))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OHB',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length

IF LEN(lcOldStat) != LEN(lcRpStatus)
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.
*!**************************************************************************
*! Name      : lfSeTSRep
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Go top in the SALESREP IN RANGE
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep
PARAMETERS OpGrdParm
DO CASE
CASE OpGrdParm = 'S'
  SELECT salesrep
  SET ORDER TO TAG  salesrep
  GO TOP
CASE OpGrdParm = 'R'
  SELECT salesrep
  SET ORDER TO
ENDCASE
*!**************************************************************************
*! Name      : lfSROrder
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Go top in the ORDHDR IN RANGE
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
CASE lcParm = 'S'
  SELECT ORDHDR
  GO TOP
CASE lcParm = 'R'
  SELECT ORDHDR
ENDCASE
*-- end of lfSROrder.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : get the description of Curr.
*!*************************************************************
*! Example     : = lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)
llOGFltCh = .T.

*--Refersh currency field.
IF lcRpCurr = 'F' AND EMPTY(lcCurrCod)
  lcCurrCod = oAriaApplication.BaseCurrency
ENDIF
SHOW GET lcCurrCod
*-- end of lfvCurDisp.
*!*************************************************************
*! Name      : lfDefCurr
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************
FUNCTION lfDefCurr
RETURN IIF(llMultCurr,'F','O')
*-- end of lfDefCurr.
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : HEBA FATHI    (HFK)
*! Date      : 21/12/2003
*! Purpose   : Function to validate browse of Currancy
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfvCurCode
lnAlias=SELECT(0)

*--In case of empty currency and forign currency
*--we can't show grand total (eg. we can't add lirra to Dollars)
*--so assign show varible to False.
*--Comment this lines to don't let the field of the currency empty.[START]
*--Uncomment these lines to give the user the ability to leave
*--the currency field empty in case of printing report in equivalent amount.
llRpshow  = .F.
IF ( lcRpCurr = 'F'  .AND. !EMPTY(lcCurrCod) )  .OR.  lcRpCurr <> 'F'
  llRpshow = .T.
ENDIF

*--Add the check of currency
IF lcRpCurr <> 'F' AND EMPTY(lcCurrCod)  && if empty currency don't browse
  RETURN
ENDIF
SELECT SYCCURR
*--Check if the currency code is empty or not.
IF EMPTY(lcCurrCod) .OR. !SEEK(lcCurrCod)
  GO TOP
  DECLARE laTemp[1]
  *!*	laTemp     = ''
  *!* lcBrFields = [CCURRCODE :R :H= 'Currency Code'	,CCURRDESC :R :H= 'Description',CCURRSMBL :R :H= 'Symbol']
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcBrFields = [CCURRCODE :R :H= ']+LANG_SoGrossp_CurCod+[' ,CCURRDESC :R :H=']+ LANG_SoGrossp_Desc+[',CCURRSMBL :R :H=']+ LANG_SoGrossp_Symbol+[']
  lcBrFields = [CCURRCODE :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_CurCod,oAriaApplication.GetHeaderText("LANG_SoGrossp_CurCod",AHEADERFILE))+[' ,CCURRDESC :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Desc,oAriaApplication.GetHeaderText("LANG_SoGrossp_Desc",AHEADERFILE))+[',CCURRSMBL :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Symbol,oAriaApplication.GetHeaderText("LANG_SoGrossp_Symbol",AHEADERFILE))+[']
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lcSavBrFld = lcBrFields
  *!*lcFile_Ttl = "Currency"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcFile_Ttl = LANG_SoGrossp_Curr
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoGrossp_Curr,oAriaApplication.GetHeaderText("LANG_SoGrossp_Curr",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *!*	  =gfBrows('','CCURRCODE','laTemp')
  =gfBrows('','CCURRCODE','laTemp',lcFile_Ttl)
  lcBrFields = lcSavBrFld
  lcCurrCod  = IIF(TYPE('laTemp[1]')='L',GCBASECURR,laTemp[1])
  SHOW GET lcCurrCod
ENDIF

*--Check if the currency code is empty or not.[START]
*!*	IF EMPTY(lcCurrCod)
*!*	  _CUROBJ = OBJNUM(lcCurrCod)
*!*	ENDIF


SELECT(lnAlias)
*-- end
*!*************************************************************
*! Name      : lfvChngCur               (B604649,1)
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : This function called from the Report
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvChngCur()
*!*************************************************************
FUNCTION lfvChngCur
llRpshow  = .F.
IF ( lcRpCurr = 'F'  AND !EMPTY(lcCurrCod) )  OR  lcRpCurr <> 'F'
  llRpshow = .T.
ENDIF

*!********************************************************************
*! Name      : lfGetFCurr  (B604649,1)
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : Return the Foreign amount From Base currency.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp.
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : lfGetFCurr(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************

FUNCTION lfGetFCurr
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile
PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
IF lcRpDispCur = 'F'
  lnExRate   = 0
  lnUnit     = 0

  IF EMPTY(lcGetFile)
    lcRpCurCde = IIF(llAprvCurr,CAPRCURCOD,cCurrCode)
  ELSE
    lcRpCurCde = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..cCurrCode)
  ENDIF

  IF lcRpCurCde = oAriaApplication.BaseCurrency
    lnExRate    = 1
    lnUnit      = 1
  ELSE
    ldExRateDt = Entered
    lnExRate   = ORDHDR.Nexrate
    lnUnit     = ORDHDR.Ncurrunit
  ENDIF

  lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
  lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,lcRpCurCde)
  lcExSin1 = IIF(lcExSin1 = '/','*','/')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  lnAmount = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDIF

SELECT (lnSavAlias)
RETURN lnAmount
*-- end of lfGetFCurr.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : This function called from the currency field to
*!           : validate the currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfwCurCode
lcOldCurr = lcCurrCod

*!*************************************************************
*! Name      : lfvChange
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : This function called from the Report type to
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvChange()
*!*************************************************************
FUNCTION lfvChange

IF   lcRPSumDet ="S"
  lcRpName = "SOGROSPS"
ELSE
  lcRpName = "SOGROSPD"
ENDIF
*!
*!*****************************************************************************************
*! Name      : RefreshStatus
*! Developer : HFK
*! Date      : 08/10/2004
*! Purpose   : To dislay the staus in the text box
*! Entry no. : 037333
*!*****************************************************************************************
*!
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
*-- end of RefreshStatus.

