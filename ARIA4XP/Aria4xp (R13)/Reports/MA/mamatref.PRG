*:**********************************************************************************************
*: Program file  : MAVMREF.PRG
*: Program desc. : Vendor Item\Color Report
*: System        : Aria Advantage Series.4xp
*: Module        : MA
*: Developer     : MAariam Mazhar(MMT)
*: Date          : 06/20/2006
*: Refer to      : N(037634)
*:**********************************************************************************************
#INCLUDE R:\ARIA4XP\REPORTS\MA\MAMATREF.H
lcStTime = TIME()
PRIVATE lnSetDecim
lnSetDecim = SET('DECIMALS')
SET DECIMALS TO 3
STORE "" TO lcSepRate
lfGetStyinfo()
loOgScroll.cCRorientation = 'P'
IF loOGScroll.llOgFltCh
   IF USED(lcTmpDet)
     SELECT (lcTmpDet)
     ZAP
     =lfCollect()
	SELECT('FABRIC')
	ZAP
	SELECT (lcTmpDet)
	SCAN
    IF loDBFITEM.Seek("0002"+PADR(&lcTmpDet..FABRIC,lnMajorLen)+lcSepRate +PADR(&lcTmpDet..COLOR,lnFabNMLen),'Style') ;
     AND !SEEK(&lcTmpDet..FABRIC+&lcTmpDet..COLOR,'Fabric') AND  loDBFUOM.Seek(ITEM.CCONVBUY)
    SELECT Item
    SCATTER MEMO MEMVAR
    m.Fabric = &lcTmpDet..FABRIC
    m.color = &lcTmpDet..COLOR
    m.Content =  Item.CITEMFLD3
    m.Desc = Item.Desc
    m.Item_type = Item.Item_type
    m.nfabCost = Item.NICOST1
    m.uombuy = UOM.CUOM_B
    m.uomuse = UOM.CUOM_V
    m.onhand = Item.TOTSTK
    m.onorder = item.TOTWIP
    INSERT INTO 'Fabric' FROM MEMVAR
  ENDIF
ENDSCAN

   ENDIF
ENDIF
*--
SELECT (lcTmpDet)
GO TOP
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*--Sort
IF lcRpSortBy = "V"
  SET ORDER TO MatCol
ELSE
  SET ORDER TO ITEM
ENDIF



SET RELATION TO
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDI
SET RELATION TO VENDOR INTO VENDMATH     ADDI
lcCurrItem = " "
*-- Start printing
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Selected + ALLTRIM(STR(RECCOUNT())) + Lang_Records_in + ALLTRIM(STR(lnInterval,6,2)) + Lang_Seconds NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selected,oAriaApplication.GetHeaderText("Lang_Selected",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT())) +;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Records_in,oAriaApplication.GetHeaderText("Lang_Records_in",AHEADERFILE)) + ALLTRIM(STR(lnInterval,6,2)) +;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Seconds,oAriaApplication.GetHeaderText("Lang_Seconds",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


DO gfDispRe WITH EVAL('lcRPForm')
SET DEVICE TO SCREEN

*B122673,1 NNA 05/15/2004 (Begin) Return to the Original [Set Decimals]
SET DECIMALS TO lnSetDecim
*B122673,1 NNA (End)

RETURN
*--End of program ALBULK

*!*************************************************************
*! Name      : lfvVendor
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor

SELECT APVENDOR
SET ORDER TO TAG VenCode
IF !EMPTY(lcRPVendor) .AND. ;
   ('?' $ lcRPVendor .OR. !SEEK(lcRPVendor , 'APVENDOR') OR ATC("M",ALLTRIM(APVENDOR.cVenSupTyp))=0)
  *=gfApVnBrow(@lcRPVendor)
  =gfApVnBrow(@lcRPVendor,.F.,'M')
  IF EMPTY(lcRPVendor)
    lcRPVendor = lcOldVal
  ENDIF
ENDIF
DO lpSuppHanl

*!*************************************************************
*! Name      : lfwOldVal
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
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
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
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

IF !llFrstTime
  loDBFAPVENDOR = CreateObject("RemoteTable","APVENDOR","VENCODE",'APVENDOR',SET("DATASESSION"))&&,"",.T.)
  loDBFPOSLN    = CreateObject("RemoteTable","POSLN","POSLN",'POSLN',SET("DATASESSION"))&&,"",.T.)
  loDBFVENDMATL = CreateObject("RemoteTable","VENDMATL","VENMAT1",'VENDMATL',SET("DATASESSION"))&&,"",.T.)
  loDBFVENDMATH = CreateObject("RemoteTable","VENDMATH","VENDMATH",'VENDMATH',SET("DATASESSION"))&&,"",.T.)
  loDBFITEM = CreateObject("RemoteTable","ITEM","cstyle",'ITEM',SET("DATASESSION"))&&,"",.T.)
  loDBFUOM =  CreateObject("RemoteTable","UOM","uomcode",'UOM',SET("DATASESSION"))&&,"",.T.)

  lcSqlStat1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOGScroll.ORDA.SQLRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF

DO lpSuppHanl

*--Create teh temp file
IF !EMPTY(lcTmpDet) AND !USED(lcTmpDet)
  SELECT VENDMATL
  =AFIELDS(laFileStru)
  lnArrLen = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnArrLen+1,18]
  *--Add the Print field.
  laFileStru[lnArrLen+1,1] = 'Print'
  laFileStru[lnArrLen+1,2] = 'L'
  laFileStru[lnArrLen+1,3] = ''
  laFileStru[lnArrLen+1,4] = ''

  STORE ' ' TO  laFileStru[lnArrLen+1,7],laFileStru[lnArrLen+1,8],;
            laFileStru[lnArrLen+1,9],laFileStru[lnArrLen+1,10],;
            laFileStru[lnArrLen+1,11],laFileStru[lnArrLen+1,12],;
            laFileStru[lnArrLen+1,13],laFileStru[lnArrLen+1,14],;
            laFileStru[lnArrLen+1,15],laFileStru[lnArrLen+1,16]
  STORE 0 TO    laFileStru[lnArrLen+1,17] ,laFileStru[lnArrLen+1,18]

  DECLARE laIndex[3,2]
  laIndex[1,1] = 'Vendor+fabric+color+cvenfab+cvencolr'
  laIndex[1,2] = 'MatCol'
  laIndex[2,1] = 'Vendor+cvenfab+cvencolr+fabric+color'
  laIndex[2,2] = 'VenMat'
  laIndex[3,1] = 'Fabric+Color+Vendor'
  laIndex[3,2] = 'Item'

  =gfCrtTmp(lcTmpDet,@laFileStru,@laIndex)
  SELECT(lcTmpDet)
  SET ORDER to  MatCol
ENDIF

SELECT VENDMATL
=AFIELDS(laFileStru)
=gfCrtTmp(lcTmpBrow,@laFileStru,'VENDOR+CVENFAB',lcTmpBrow)

DIMENSION laFileStruct[10,4]

laFileStruct[1,1] = 'Fabric'
laFileStruct[1,2] = 'C'
laFileStruct[1,3] = 7
laFileStruct[1,4] = 0

laFileStruct[2,1] = 'Color'
laFileStruct[2,2] = 'C'
laFileStruct[2,3] = 6
laFileStruct[2,4] = 0

laFileStruct[3,1] = 'desc'
laFileStruct[3,2] = 'C'
laFileStruct[3,3] = 30
laFileStruct[3,4] = 0

laFileStruct[4,1] = 'item_type'
laFileStruct[4,2] = 'C'
laFileStruct[4,3] = 6
laFileStruct[4,4] = 0

laFileStruct[5,1] = 'nfabcost'
laFileStruct[5,2] = 'N'
laFileStruct[5,3] = 9
laFileStruct[5,4] = 3

laFileStruct[6,1] = 'uombuy'
laFileStruct[6,2] = 'C'
laFileStruct[6,3] = 3
laFileStruct[6,4] = 0

laFileStruct[7,1] = 'uomuse'
laFileStruct[7,2] = 'C'
laFileStruct[7,3] = 3
laFileStruct[7,4] = 0

laFileStruct[8,1] = 'onhand'
laFileStruct[8,2] = 'N'
laFileStruct[8,3] = 12
laFileStruct[8,4] = 3

laFileStruct[9,1] = 'onorder'
laFileStruct[9,2] = 'N'
laFileStruct[9,3] = 12
laFileStruct[9,4] = 3

laFileStruct[10,1] = 'content'
laFileStruct[10,2] = 'C'
laFileStruct[10,3] = 60
laFileStruct[10,4] = 0

=gfCrtTmp('Fabric',@laFileStruct,'Fabric+Color','Fabric')
*!*************************************************************
*! Name      : lfvFabBet
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************
FUNCTION lfvFabBet


lcFabObj = VARREAD()
lcFab    = &lcFabObj
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC')
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC

*!**************************************************************************
*! Name      : lpSuppHanl
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : To check for Order status in Order to Enable/Disable the 2
*!             Option added to the Selection grid.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : DO lpChkStat
*!**************************************************************************

PROCEDURE lpSuppHanl
PRIVATE llEmpVen
llEmpVen = .F.



IF ASCAN(laOGFxFlt,'VENDMATL.CVENFAB') # 0
*!*	  *-- Get the position of the vendor in the varaible filter
  lnVendorPo = ASUBSCRIPT( laOGFxFlt, ASCAN(laOGFxFlt,'VENDMATL.CVENFAB'),1)
  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnVendorPo + 1] = .F.
  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
ENDIF


*!*************************************************************
*! Name      : lfSRVFab
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : control browsing primary fabric and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
*!*	    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
*!*	    SELECT FABRIC
*!*	    SET ORDER TO TAG cFabric
*!*	    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
*!*	    GO TOP IN FABRIC
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
*!*	    USE IN FABRIC_X
*!*	    SELECT FABRIC
*!*	    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
LPARAMETERS lcFab, lcComp
LOCAL lnTotcomp,  lnAlias
lnTotcomp = 0

  IF SEEK(ALLTRIM(lcFab), lcTmpFab)
    SUM &lcTmpFab..&lcCOMP. TO lnTotcomp WHILE &lcTmpFab..Fabric = lcFab
  ENDIF

RETURN lnTotcomp


*!*************************************************************
*! Name      : lfCollect
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : Collect Data
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfCollect

*----Key Indecies :
*-1 MatCol : vendor+fabric+color+cvenfab+cvencolr
*-2 VenMat : vendor+cvenfab+cvencolr+fabric+color

llvendSele = .F.
lnVendorPo = ASUBSCRIPT(loogscroll.laOGFxFlt, ASCAN(loogscroll.laOGFxFlt,'APVENDOR.CVENDCODE'),1)
lcFileSele = loogscroll.laOGFxFlt[lnVendorPo,6]
llvendSele = IIF(!EMPTY(lcFileSele) and USED(lcFileSele) and RECCOUNT(lcFileSele)>0,.T.,.F.)

llItemSele = .F.
lnItemPo   = ASUBSCRIPT(loogscroll.laOGFxFlt, ASCAN(loogscroll.laOGFxFlt,'VENDMATL.FABRIC'),1)
lcItemSele = loogscroll.laOGFxFlt[lnItemPo,6]
llItemSele = IIF(!EMPTY(lcItemSele) AND USED(lcItemSele) and RECCOUNT(lcItemSele)>0,.T.,.F.)

llSuppItm = .F.
lnSuppPo   = ASUBSCRIPT(loogscroll.laOGFxFlt, ASCAN(loogscroll.laOGFxFlt,'VENDMATL.CVENFAB'),1)
lcSuppSele = loogscroll.laOGFxFlt[lnSuppPo,6]
llSuppItm  = IIF(!EMPTY(lcSuppSele) AND USED(lcSuppSele) and RECCOUNT(lcSuppSele)>0,.T.,.F.)

IF llvendSele
  SELECT(lcFileSele)
  LOCATE
  IF EOF()
    llvendSele  = .F.
  ENDIF
ENDIF
*---
IF llItemSele
 SELECT(lcItemSele)
 LOCATE
 IF EOF()
   llItemSele = .F.
 ENDIF
ENDIF

IF llSuppItm
  SEle(lcSuppSele)
  LOCATE
  IF EOF()
    llSuppItm = .F.
  ENDIF
ENDIF


IF llvendSele
  SELECT(lcFileSele)
  SCAN
    IF loDBFVENDMATL.Seek(&lcFileSele..CVENDCODE)
      SELECT VENDMATL
      SCAN REST WHILE vendor+cvenfab+cvencolr+fabric+color = &lcFileSele..CVENDCODE ;
           FOR IIF(llItemSele,SEEK(VENDMATL.FABRIC,lcItemSele),.T.) AND ;
           	   IIF(llSuppItm,SEEK(VENDMATL.CVENFAB,lcSuppSele),.T.)
        SCATTER MEMVAR MEMO
        IF !SEEK(vendor+cvenfab+cvencolr+fabric+color,lcTmpDet)
          INSERT INTO (lcTmpDet) FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  loDBFVENDMATL.Seek("")
  SELECT VENDMATL
  SCAN FOR IIF(llItemSele,SEEK(VENDMATL.FABRIC,lcItemSele),.T.)
    SCATTER MEMVAR MEMO
    IF !SEEK(vendor+cvenfab+cvencolr+fabric+color,lcTmpDet)
      INSERT INTO (lcTmpDet) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF
RETURN

*!*	SELECT VENDMATL
*!*	IF EMPTY(lcRPVendor)
*!*	  lcWhilExp = ''
*!*	  SET ORDER TO MatCol
*!*	ELSE
*!*	  lcWhilExp = ' WHILE vendor+cvenfab+cvencolr+fabric+color = lcRPVendor '
*!*	  SET ORDER TO VenMat
*!*	  =SEEK(lcRPVendor)
*!*	ENDIF
*!*	SCAN &lcWhilExp FOR &lcRpExp
*!*	  SCATTER MEMVAR MEMO
*!*	  IF !SEEK(IIF(EMPTY(lcRPVendor),vendor+fabric+color+cvenfab+cvencolr,vendor+cvenfab+cvencolr+fabric+color),lcTmpDet)
*!*	    INSERT INTO (lcTmpDet) FROM MEMVAR
*!*	  ENDIF
*!*	ENDSCAN
*!*	SET ORDER TO VENMAT1 IN VENDMATL

*!*	*--Handle the data not to be reprinted
*!*	IF .F. AND EMPTY(lcRPVendor)
*!*	 SELECT (lcTmpDet)
*!*	 SET ORDER TO ITEM
*!*	 SCAN
*!*	   lcKey = EVAL(KEY())
*!*	   REPLACE Print with '.T.'
*!*	
*!*	 ENDSCAN
*!*	ENDIF
*!*	RETURN

*!*************************************************************
*! Name      : lfGetItem
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : Get the current fabric color
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfGetItem

lcCurrItem = Fabric + Color
RETURN ''

*!*************************************************************
*! Name      : lfCalcOnOrd
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : Calculate the OnOrder qty for a specific
*!           : Item\Color\Vendor
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfCalcOnOrd
PARAMETERS lcKey,lcVen

PRIVATE lnAlias
lnAlias = SELECT()
=loDBFPOSLN.Seek('0002'+PADR(SUBSTR(lcKey,1,7),lnMajorLen)+lcSepRate+PADR(SUBSTR(lcKey,8),lnFabNMLen),'poslns')
SELECT POSLN
SUM REST WHILE CINVTYPE+Style+cbusdocu+cstytype+po+STR(lineno)+trancd = '0002'+ PADR(SUBSTR(lcKey,1,7),lnMajorLen)+lcSepRate+PADR(SUBSTR(lcKey,8),lnFabNMLen)  IIF(TRANCD='1',IIF(POSLN.CBUSDOCU = 'P',QTY1,-QTY1),0.000) TO lnOnOrd FOR (POSLN.CBUSDOCU  = 'P' OR POSLN.CBUSDOCU  = 'R') ;
     AND VENDOR = lcVen
SELECT (lnAlias)
RETURN lnOnOrd

*!*************************************************************
*! Name      : lfCreateIndecies
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

DIMENSION laIndex[2,2]

laIndex[1,1] = 'Fabric'
laIndex[1,2] = 'lcFabDye'

laIndex[2,1] = 'Style'
laIndex[2,2] = 'Style'


SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)

=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.

*!*************************************************************
*! Name      : lfSRVend
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : SRV function of vendor
*!*************************************************************
FUNCTION lfSRVend
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'R'  && Set code

   SELECT (lcTmpBrow)
   ZAP
   lnVendorPo = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
   lcFileSele = IIF(!EMPTY(laOGFxFlt[lnVendorPo,6]) and USED(laOGFxFlt[lnVendorPo,6]) and RECCOUNT(laOGFxFlt[lnVendorPo,6]) > 0 ,laOGFxFlt[lnVendorPo,6],"")

   IF !EMPTY(lcFileSele)
     SELECT(lcFileSele)
     LOCATE
     IF !EOF()
	     SCAN
		  IF loDBFVENDMATL.seek(&lcFileSele..CVENDCODE)
		    SELECT VENDMATL
		    SCAN REST WHILE VENDOR+CVENFAB = &lcFileSele..CVENDCODE
		      SCATTER MEMO MEMVAR
		      INSERT INTO (lcTmpBrow) FROM  MEMVAR
		    ENDSCAN
	       ENDIF
	     ENDSCAN
	     IF ASCAN(laOGFxFlt,'VENDMATL.CVENFAB') # 0
	  	 *!*	  *-- Get the position of the vendor in the varaible filter
		 lnVendorPo = ASUBSCRIPT( laOGFxFlt, ASCAN(laOGFxFlt,'VENDMATL.CVENFAB'),1)
		  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnVendorPo + 1] = .T.
		  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
		 ENDIF
	  ELSE
		IF ASCAN(laOGFxFlt,'VENDMATL.CVENFAB') # 0
			*!*	  *-- Get the position of the vendor in the varaible filter
  			  laOGFxFlt[lnVendorPo,6] = ''
			  lnVendorPo = ASUBSCRIPT( laOGFxFlt, ASCAN(laOGFxFlt,'VENDMATL.CVENFAB'),1)
			  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnVendorPo + 1] = .F.
			  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
	    ENDIF
	    lnVendorPo = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt, ASCAN(LOOGSCROLL.laOGFxFlt,'VENDMATL.CVENFAB'),1)
		  lcSeleItem = LOOGSCROLL.laOGFxFlt[lnVendorPo,6]
		  IF !EMPTY(lcSeleItem) AND USED(lcSeleItem) AND RECCOUNT(lcSeleItem)>0
		    SELECT (lcSeleItem)
		    ZAP
		  ENDIF
	  ENDIF
   ELSE
     IF EMPTY(lcFileSele)OR (!EMPTY(lcFileSele) AND (!USED(lcFileSele) OR RECCOUNT(lcFileSele)= 0))
       SELEct(lcTmpBrow)
       ZAP
       IF ASCAN(LOOGSCROLL.laOGFxFlt,'VENDMATL.CVENFAB') # 0
  		  *-- Get the position of the vendor in the varaible filter
		  lnVendorPo = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt, ASCAN(LOOGSCROLL.laOGFxFlt,'VENDMATL.CVENFAB'),1)
		  lcSeleItem = LOOGSCROLL.laOGFxFlt[lnVendorPo,6]
		  IF !EMPTY(lcSeleItem) AND USED(lcSeleItem) AND RECCOUNT(lcSeleItem)>0
		    SELECT (lcSeleItem)
		    ZAP
		  ENDIF
*          LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNVENDORPO] = .F.
			IF ASCAN(laOGFxFlt,'VENDMATL.CVENFAB') # 0
			*!*	  *-- Get the position of the vendor in the varaible filter
  			  laOGFxFlt[lnVendorPo,6] = ''
			  lnVendorPo = ASUBSCRIPT( laOGFxFlt, ASCAN(laOGFxFlt,'VENDMATL.CVENFAB'),1)
			  LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(laOGFxFlt,1) +  lnVendorPo + 1] = .F.
			  = LFOGSHOWGET('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
			ENDIF
		ENDIF
     ENDIF
  ENDIF
ENDCASE


*!*************************************************************
*! Name      : lfsrvItm
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : SRV function of item
*!*************************************************************
FUNCTION lfsrvItm
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
   lnVendorPo = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
   lcFileSele = IIF(!EMPTY(laOGFxFlt[lnVendorPo,6]) and USED(laOGFxFlt[lnVendorPo,6]) and RECCOUNT(laOGFxFlt[lnVendorPo,6]) > 0 ,laOGFxFlt[lnVendorPo,6],"")
   IF EMPTY(lcFileSele)
      SELEct(lcTmpBrow)
      ZAP
   ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfGetStyinfo
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : Get the style structure information
*!*************************************************************
FUNCTION lfGetStyinfo
lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lcMajPict  = gfItemMask("PM")

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

lcSepRate = laMajSegs[lnMajSeg,6]

*!*************************************************************
*! Name      : lfSumFab2
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
*! Purpose   : sum a specific field for the current fabric
*!                  in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab2()
*!*************************************************************
FUNCTION lfSumFab2
PARAMETERS lcFab,lcComp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcTmpFab)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcTmpFab)
  LOCATE
  IF SEEK(lcFab,lcTmpFab,'Style')
    SUM &lcCOMP TO lnTotcomp WHILE STYLE = lcFab
    *AND EMPTY(DYELOT)
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF
SELECT(lnAlias)

RETURN INT(lnTotcomp)
*!*************************************************************
*! Name      : lfCollTime
*: Developer : Mariam Mazhar [MMT]
*: Date      : 06/22/2006
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


