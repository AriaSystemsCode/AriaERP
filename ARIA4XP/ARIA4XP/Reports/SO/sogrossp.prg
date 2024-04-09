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
*! E303652,1 AEG 03/17/2016 Convert sogross profit report to crystal and sales rep to sort by [T20150714.0050]
*! B611157,1 MMT 06/20/2016 Change SO Gross profit report orientation to be landscape[P20151218.0003-Issue#1]
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
      for lnCount= 1 to lnOccur+1
       lnStart = IIF(lnCount=1,1,ATC(' AND',loOGScroll.lcRpExp,lnCount-1)+5)
       lnEnd = IIF(lnCount = lnOccur+1,LEN(loOGScroll.lcRpExp)+1,ATC(' AND',loOGScroll.lcRpExp,lnCount))
       lnLength = lnend - lnstart
       lcScatteredExp = substr(loOGScroll.lcRpExp,lnStart,lnLength)
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
  SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
  SET RELATION TO CordType + order INTO ORDLINE ADDITIVE
  *-HFK, 05/15/2005
  SELECT ORDLINE
  SET RELATION TO STYLE INTO STYLE ADDITIVE 
  SET RELATION TO Style INTO STYDYE ADDITIVE  
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
*!*E303652,1 03/17/2016 AEG Convert So gross rofit report to crystal and add sales rep sort by [BEGIN]
*!*lcGrpVar = IIF(lcRpSortBy = 'O',LANG_SoGrossp_Order,IIF(lcRpSortBy = 'A',LANG_SoGrossp_Account,IIF(lcRpSortBy = 'S',LANG_SoGrossp_Style,LANG_SoGrossp_Class)))
lcGrpVar = IIF(lcRpSortBy = 'O',LANG_SoGrossp_Order,IIF(lcRpSortBy = 'A',LANG_SoGrossp_Account,IIF(lcRpSortBy = 'S',LANG_SoGrossp_Style,IIF(lcRpSortBy = 'R',LANG_SoGrossp_Sales,LANG_SoGrossp_Class))))
*!*E303652,1 03/17/2016 AEG Convert So gross rofit report to crystal and add sales rep sort by [END]
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

*!* E303652,1 03/17/2016 AEG Convert So gross rofit report to crystal and add sales rep sort by [BEGIN]
DIMENSION loOgScroll.laCRTables[1]
COPY TO oAriaApplication.WorkDir + lcRpTmpN + ".DBF"
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir+lcRpTmpN+ ".DBF"

USE IN (lcRpTmpN)

loOgScroll.lcOGLastForm = lcRpName

*!* E303652,1 03/17/2016 AEG Convert So gross rofit report to crystal and add sales rep sort by [END]
*! B611157,1 MMT 06/20/2016 Change SO Gross profit report orientation to be landscape[P20151218.0003-Issue#1][Start]
*loogScroll.cCROrientation = 'P'
loogScroll.cCROrientation = 'L'
*! B611157,1 MMT 06/20/2016 Change SO Gross profit report orientation to be landscape[P20151218.0003-Issue#1][End]
*!* E303652,1 03/17/2016 AEG Convert So gross rofit report to crystal and add sales rep sort by [BEGIN]
*!* DO gfDispRe WITH EVALUATE('lcRpName')

DIMENSION loOgScroll.laCRParams[14,2]
 loOgScroll.laCRParams[1,1]='lcStyTitle'
 loOgScroll.laCRParams[1,2]=lcStyTitle
 loOgScroll.laCRParams[2,1]='gcCom_Name'
 loOgScroll.laCRParams[2,2]=gcCom_Name
 loOgScroll.laCRParams[3,1]='lcRpTitle'
 loOgScroll.laCRParams[3,2]=lcRpTitle
 loOgScroll.laCRParams[4,1]='gcUser_Id'
 loOgScroll.laCRParams[4,2]=gcUser_Id
 loOgScroll.laCRParams[5,1]='lcSPTime'
 loOgScroll.laCRParams[5,2]=lcSPTime
 loOgScroll.laCRParams[6,1]='cCurrCode'
 loOgScroll.laCRParams[6,2]=lcCurrCod
 loOgScroll.laCRParams[7,1]='GProfit'
 loOgScroll.laCRParams[7,2]=IIF(lcRpSortBy = 'S',"STYLE",IIF(lcRpSortBy = 'A',"ACCOUNT",IIF(lcRpSortBy = 'O',"ORDER",IIF(lcRpSortBy = 'C',"CLASS","SALES REP"))))
 loOgScroll.laCRParams[8,1]='gdSysDate'
 loOgScroll.laCRParams[8,2]=gdSysDate
 loOgScroll.laCRParams[9,1]='lcRpDeciml'
 loOgScroll.laCRParams[9,2]=lcRpDeciml
 loOgScroll.laCRParams[10,1]='lcGrpVar'
 loOgScroll.laCRParams[10,2]=lcGrpVar
 loOgScroll.laCRParams[11,1]='lcRpSortBy'
 loOgScroll.laCRParams[11,2]=lcRpSortBy 
 loOgScroll.laCRParams[12,1]='LCRPMARK'
 loOgScroll.laCRParams[12,2]=LCRPMARK
 loOgScroll.laCRParams[13,1]='LCRPCURR'
 loOgScroll.laCRParams[13,2]=LCRPCURR
 loOgScroll.laCRParams[14,1]='GCBASECURR'
 loOgScroll.laCRParams[14,2]=GCBASECURR

gfDispRe()
*!* E303652,1 03/17/2016 AEG Convert So gross rofit report to crystal and add sales rep sort by [END]

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
  SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
  SET RELATION TO CordType + order INTO ORDLINE ADDITIVE
  llRelation = .T.  && added this variable to detect relation set.
ENDIF 
SELECT ORDLINE
SET RELATION TO Style INTO STYLE
*-- Don't overwrite the relation: add ADDITIVE
SET RELATION TO Style INTO STYDYE ADDITIVE
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
  SCAN REST WHILE order = ORDHDR.order FOR TOTQty <> 0
  *! B608821,1 MMT 03/17/2009 Don't Display Zero Qty Lines [End] 
    IF !&lcStyGroup
      LOOP
    ENDIF
    *--Make sure the filter runs if the second line in the order contains

    SCATTER MEMVAR MEMO
    *-- check if avarge cost or standerd , if avarge check if multi warehouse
    IF lcCostMth = 'A'
      IF llMltiWr = .T.
        =SEEK(style+cwarecode,'STYDYE')
        *--In Case of muli curr. calculate the var by function lfGetFcurr
        IF llMultCurr
          SELECT ORDHDR
          lnCostValu = lfGetFCurr(StyDye.Ave_cost, lcRpCurr , ldRpExDate , lcRpTmpNam)
          SELECT ORDLINE
        ELSE
          lnCostValu = StyDye.Ave_cost
        ENDIF
      ELSE
        =SEEK(style,'STYLE')
        *-- In Case of muli curr. calculate the var by function lfGetFcurr
        IF llMultCurr
          SELECT ORDHDR
          lnCostValu = lfGetFCurr(Style.Ave_cost, lcRpCurr , ldRpExDate , lcRpTmpNam)
          SELECT ORDLINE
        ELSE
          lnCostValu = Style.Ave_cost
        ENDIF
      ENDIF
    ELSE
      =SEEK(style,'STYLE')
      lnCostValu = Style.TotCost
    ENDIF
    *-- get CCurrCode to use at  gfAmntDisp and insert into temp table
    m.CCurrCode = ORDHDR.CCURRCODE
    lnpieces =Totqty
    lns_price =price
    *--Calculate the gros amount.[START]
    lnGrsAmnt =TotQty*Price
    lnNetAmnt =TotQty*Price
    lnNetAmnt1=lnNetAmnt
    lnProfit =lnNetAmnt-lnCostValu*TotQty
    *-- get profit percentage
    *---mhm lcMarkUp
    IF lcRpMark = 'T'
      IF lcRpCurr = "F"
        lcRpCurr = "O"
        lcSvAlis = ALIAS()
        lnNetAmnt1=lnNetAmnt
        SELECT ORDHDR
        price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
        lnNetAmnt =TotQty*Price
        LNTAMNT = lnNetAmnt
        lnProfit =lnNetAmnt-lnCostValu*TotQty
        lnPrftPrct = IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TotQty))*100,0)
        lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnCostValu<0),-lnPrftPrct,lnPrftPrct)
		    *-- To avoid deviding by zero
		    lnPrftPrct = IIF(lnCostValu =0 .OR. TotQty = 0,0,(lnProfit/(lnCostValu*TotQty))*100)
		    SELECT (lcSvAlis)
		    lcRpCurr = "F"
		    lnNetAmnt=lnNetAmnt1
      ELSE
        lcSvAlis = ALIAS()
		    lnNetAmnt1=lnNetAmnt
		    SELECT ORDHDR
		    price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
		    lnNetAmnt =TotQty*Price
		    LNTAMNT = lnNetAmnt
		    lnProfit =lnNetAmnt-lnCostValu*TotQty
		    lnPrftPrct = IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TotQty))*100,0)
		    lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnCostValu<0),-lnPrftPrct,lnPrftPrct)
		    *-- To avoid deviding by zero
		    lnPrftPrct = IIF(lnCostValu = 0 .OR. TotQty = 0, 0,(lnProfit/(lnCostValu*TotQty))*100)
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
		    lnNetAmnt =TotQty*Price
		    LNTAMNT = lnNetAmnt
		    lnProfit =lnNetAmnt-lnCostValu*TotQty
		    lnPrftPrct = IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0)
		    lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnNetAmnt<0),-lnPrftPrct,lnPrftPrct)
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
		    lnNetAmnt =TotQty*Price
		    LNTAMNT = lnNetAmnt
		    lnProfit =lnNetAmnt-lnCostValu*TotQty
		    lnPrftPrct = IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0)
		    lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnNetAmnt<0),-lnPrftPrct,lnPrftPrct)
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
      lnProfit =NetAmnt-lnCostValu*TotQty
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
	    m.PRICE = lns_price
	  ELSE
	    lcSvAlis = ALIAS()
		  SELECT ORDHDR
		  m.PRICE =gfAmntDisp(lns_price,lcRpCurr,ldRpExDate,lcRpTmpNam)
	    SELECT (lcSvAlis)
	  ENDIF
    *-cost cannot be change(cost in base currancy) Only in foreign case
    *--get totl cost
    m.Cost = lnCostValu*TotQty
    *--IF multi currancy calculate it [ end]
    m.CodDes=gfCodDes(&lcRpTmpN..CLASS,'CLASS')  && variable to get code description
    m.TOTQTY  = lnpieces
    m.ProfitPer = lnPrftPrct
    m.Rep1 = ORDHDR.Rep1
		m.CLASS = CUSTOMER.CLASS
		m.S_GROUP = STYLE.CSTYGROUP
		m.Name = CUSTOMER.Btname
		m.Profit = lnProfit
		*-- Calculate the net amount.
		*-- Compute the net amount based on the merchandise discount not only the trade disc.
		*! E302638,1 MMT 09/24/2009 add option deduct trade disc in gross profit reports(So,AR)[Start]
		*m.NetAmnt = m.NetAmnt - (m.NetAmnt * lnTradDisc/100) - (m.NetAmnt * OrdHdr.Disc / 100)
		
		*!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
        IF M_TrdDiscL
         m.NetAmnt = m.NetAmnt - (m.NetAmnt * IIF(llRPTrdDsc,ordline.Trde_Disc,0)/100) - (m.NetAmnt * OrdHdr.Disc / 100)
        ELSE 
         *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]
	 	m.NetAmnt = m.NetAmnt - (m.NetAmnt * IIF(llRPTrdDsc,lnTradDisc,0)/100) - (m.NetAmnt * OrdHdr.Disc / 100)
		*!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
        ENDIF 
        *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]
        
		*! E302638,1 MMT 09/24/2009 add option deduct trade disc in gross profit reports(So,AR)[End]
		m.Profit  = m.NetAmnt - m.Cost
		*-- Calculate the Profit % .
		IF lcRpMark = 'T'
		  lnPrftPrct = IIF(lnCostValu <> 0 , (m.Profit/m.Cost)*100 , 0)
		  lnPrftPrct = IIF(((m.NetAmnt - m.Cost )<0 .AND. lnCostValu<0) , -lnPrftPrct , lnPrftPrct)
		  lnPrftPrct = IIF(lnCostValu = 0 .OR. TotQty = 0 , 0 , (m.Profit/m.Cost)*100)
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
	laRpTmpN[15,3]=3
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
	laRpTmpN[19,3]=30
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
  STORE LANG_SoGrossp_Open TO laRpSource[1]
  STORE LANG_SoGrossp_Hold TO laRpSource[2]
  STORE LANG_SoGrossp_Bid  TO laRpSource[3]
  lcRpStatus = 'OHB'
  *--Value to be adjusted from syrepuvr file
  SHOW GET LCCURRCOD
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
  CASE lcRPSortBy='A'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON Account+cCurrCode+Order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Account+Order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ENDIF
    *!*lcHBreak='ACCOUNT'
    lcHBreak=LANG_SoGrossp_Account
  CASE lcRPSortBy='O'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON cCurrCode+Order+Style+Account+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Order+Style+Account+STR(RECNO(),7) TAG &lcRpTmpN
    ENDIF
    *!*lcHBreak='ORDER'	
    lcHBreak=LANG_SoGrossp_Order
  CASE lcRPSortBy='S'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON Style+cCurrCode+Order+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Style+Order+STR(RECNO(),7) TAG &lcRpTmpN
    ENDIF
    *!*lcHBreak='STYLE'
    lcHBreak=LANG_SoGrossp_Style
  CASE lcRPSortBy='C'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON Class+cCurrCode+order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Class+order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ENDIF
    *!*lcHBreak='CLASS'
    lcHBreak=LANG_SoGrossp_Class

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
    USE (oAriaApplication.SysPath+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
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
 
 = lfOGMover(@laRpSource,@laRpTarget,LANG_SoGrossp_SelOrdStats,.T.,'')  && call mover function.
* B123663,1 SMM End
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *!*lcRpStatus = lcRpStatus +IIF(laRpTarget[lnI] = 'Open','O',;
    *!*                         IIF(laRpTarget[lnI] = 'Hold','H',;
    *!*                         IIF(laRpTarget[lnI] = 'Bid','B','')))
    lcRpStatus = lcRpStatus +IIF(laRpTarget[lnI] = LANG_SoGrossp_Open,'O',;
                             IIF(laRpTarget[lnI] = LANG_SoGrossp_Hold,'H',;
                             IIF(laRpTarget[lnI] = LANG_SoGrossp_Bid ,'B','')))
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
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP
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
SHOW GET LCCURRCOD
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
  lcBrFields = [CCURRCODE :R :H= LANG_SoGrossp_CurCod ,CCURRDESC :R :H= LANG_SoGrossp_Desc,CCURRSMBL :R :H= LANG_SoGrossp_Symbol]
	lcSavBrFld = lcBrFields
  *!*lcFile_Ttl = "Currency"
  lcFile_Ttl = LANG_SoGrossp_Curr
  *!*	  =gfBrows('','CCURRCODE','laTemp')
  =gfBrows('','CCURRCODE','laTemp',lcFile_Ttl)
	lcBrFields = lcSavBrFld
	lcCurrCod  = IIF(TYPE('laTemp[1]')='L',gcbasecurr,laTemp[1])
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
    lcRpCurCde = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
  ELSE
    lcRpCurCde = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
  ENDIF

  IF lcRpCurCde = oAriaApplication.BaseCurrency
    lnExRate    = 1
    lnUnit      = 1
  ELSE
    ldExRateDt = Entered
    lnExRate   = OrdHdr.Nexrate
    lnUnit     = OrdHdr.Ncurrunit
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
lcOldCurr = lcCurrcod

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

