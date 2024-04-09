*:***************************************************************************
*: Program file  : ICOTSNOR.PRG
*: Program desc. : CUSTOMIZATION OPEN TO SELL REPORET FOR NORTH BAY.
*: For Report    : ICOTSNOD.FRX,ICOTSNOS.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAAFAT ERNEST (BWA)
*: Date          : 12/30/2004
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICOTSNOR
*:***************************************************************************
*: This Report Program is due to CP124951 ...
*:***************************************************************************
*: Modifications : 
*:***************************************************************************

STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
=lfChkStrct() && Get the length of the style , color and scale.

PRIVATE lnI , lnHdr
STORE 0 TO lnI , lnHdr

PRIVATE lcAlias , laSortSzes , laGetHdrs
*--mhm
*DECLARE laSortSzes[1] , laGetHdrs[1,2]
DECLARE laSortSzes[1,2] , laGetHdrs[1,2]
*--mhm
STORE SPACE(0) TO laSortSzes , laGetHdrs

*--Section to creat Temp. file and collect the data.
=lfCreatTmp()  && Create work cursor.
=lfCollData()  && Collect the data.

SELECT (lclinesNor)
SET ORDER TO TAG DivSea
IF lcRPSortBy == "D"
  SET FILTER TO Totqty # 0
ELSE
  SET FILTER TO TotWplsH # 0
ENDIF
LOCATE

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

DO gfDispRe WITH EVAL('lcRPFormNa')

*--Function to clear the Temp. file.
=lfBasToClr(lclinesNor , 'F')

                      *-- Function section --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

lcAlias = ALIAS()

IF lcRPSortBy == "D"
*--mhmhmhmhm[Start]
  *CREATE CURSOR (lclinesNor) (Style C(lnStyLnGl) , Season C(6) , cdivision C(6) , Color C(lnClrLnGl) ,;
                              Inseam C(6)        , ScalLtr C(1), WipFld N(6)    , TotWplsH N(6)      ,;
                              Hdr1 C(6) , Hdr2 C(6) , Hdr3 C(6) , Hdr4 C(6) , Hdr5 C(6) , Hdr6 C(6)  ,;
                              Hdr7 C(6) , Hdr8 C(6) , Hdr9 C(6) , Hdr10 C(6), Hdr11 C(6), Hdr12 C(6) ,;
                              Qty1 N(6) , Qty2 N(6) , Qty3 N(6) , Qty4 N(6) , Qty5 N(6) , Qty6 N(6)  ,;
                              Qty7 N(6) , Qty8 N(6) , Qty9 N(6) , Qty10 N(6), Qty11 N(6), Qty12 N(6) ,;
                              Totqty N(7)  , Price N(9,2))
  *INDEX ON cdivision + Season + Style + Color + ScalLtr + Inseam TAG DivSea of (gcWorkDir + lclinesNor)
  *INDEX ON Style + Color + ScalLtr + Inseam TAG ScalLtr of (gcWorkDir + lclinesNor)

  CREATE CURSOR (lclinesNor) (Style C(lnStyLnGl) , Season C(6) , cdivision C(6) , Color C(lnClrLnGl) ,;
                              Inseam C(6) ,SizOrd C(2), ScalLtr C(1), WipFld N(6)    , TotWplsH N(6)      ,;
                              Hdr1 C(6) , Hdr2 C(6) , Hdr3 C(6) , Hdr4 C(6) , Hdr5 C(6) , Hdr6 C(6)  ,;
                              Hdr7 C(6) , Hdr8 C(6) , Hdr9 C(6) , Hdr10 C(6), Hdr11 C(6), Hdr12 C(6) ,;
                              Qty1 N(6) , Qty2 N(6) , Qty3 N(6) , Qty4 N(6) , Qty5 N(6) , Qty6 N(6)  ,;
                              Qty7 N(6) , Qty8 N(6) , Qty9 N(6) , Qty10 N(6), Qty11 N(6), Qty12 N(6) ,;
                              Totqty N(7)  , Price N(9,2))
  INDEX ON cdivision + Season + Style + Color + ScalLtr + SizOrd+ Inseam TAG DivSea of (gcWorkDir + lclinesNor)
  INDEX ON Style + Color + ScalLtr + Inseam TAG ScalLtr of (gcWorkDir + lclinesNor)
*--mhmhmhmhm

ELSE
  CREATE CURSOR (lclinesNor) (Style C(lnStyLnGl) , Season C(6) , cdivision C(6) , TotWplsH N(6) ,;
                              Price N(9,2))

  INDEX ON cdivision + Season + Style TAG DivSea of (gcWorkDir + lclinesNor)
  INDEX ON Style TAG ScalLtr of (gcWorkDir + lclinesNor)

ENDIF

SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : .PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias , lcScale , lcKey , lcColor , lcStyle

lcAlias = ALIAS()

SELECT STYLE
STORE SPACE(0) TO lcScale , lcHldScal , lcKey , lcColor , lcStyle

SCAN FOR &lcRpExp
  WAIT WINDOW 'Selecting Records For The Report ...' + STYLE.STYLE NOWAIT
  
  IF lcRPSortBy == "D"
    *--This code due to if the style change and the the color of the 2nd style like the 1st style.
    IF lcStyle # SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)
       STORE SPACE(0) TO lcColor , lcStyle
    ENDIF
    
    *--Case new style with same color and scale, Fill this variable "lcStyle"
    IF EMPTY(lcStyle) AND (lcKey $ lcHldScal)
      lcStyle = SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)
    ENDIF

    lcKey = SUBSTR(STYLE.STYLE , lnScaPosGl , 1)
    IF !(lcKey $ lcHldScal)
      =lfGetSizes()
      =lfaddrcrds()
      lcHldScal = lcHldScal + lcKey
    ENDIF

    *--Case the color change , Add new records for the new color.
    IF lcColor # SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl) AND !EMPTY(lcStyle)
      =lfaddrcrds()
    ENDIF
    lcColor = SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl)

    *--Insert the records from the style file to the Temp. file.
    =lfInsrtVal()

    *--Get the value of the style
    lcStyle = SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)
  ELSE
    *--Insert the records from the style file to the Temp. file.
    =lfSmaryVal()
  ENDIF
ENDSCAN

SELECT(lcAlias)

*--End of lfCollData.
*!*************************************************************
*! Name      : lfSmaryVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Function insert the summary values in Temp. File.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : .PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfSmaryVal()
*!*************************************************************
FUNCTION lfSmaryVal
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT (lclinesNor)
lcStyInsr = SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)

IF SEEK(lcStyInsr)
  REPLACE &lclinesNor..TotWplsH WITH &lclinesNor..TotWplsH + STYLE.TOTWIP + (STYLE.TOTSTK - STYLE.TOTORD)
ELSE
  APPEND BLANK
  REPLACE &lclinesNor..Style     WITH lcStyInsr       ,;
          &lclinesNor..Price     WITH STYLE.PRICEA    ,;
          &lclinesNor..cdivision WITH STYLE.CDIVISION ,;
          &lclinesNor..Season    WITH STYLE.SEASON    ,;
          &lclinesNor..TotWplsH WITH &lclinesNor..TotWplsH + STYLE.TOTWIP + (STYLE.TOTSTK - STYLE.TOTORD)
ENDIF

SELECT(lcAlias)

*--End of lfSmaryVal.
*!*************************************************************
*! Name      : lfInsrtVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Function to insert the values in the Temp. File.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : .PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfInsrtVal()
*!*************************************************************
FUNCTION lfInsrtVal
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT (lclinesNor)
lcStyInsr = SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)
lcClrInsr = SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl)

IF SEEK("S" + STYLE.SCALE, "SCALE")
  FOR lnSclCnt = 1 TO SCALE.CNT
    lcSclCnt = "Sz" + ALLTRIM(STR(lnSclCnt))
    IF SEEK( lcStyInsr + lcClrInsr + LEFT(SCALE.SCALE,1) + ALLTRIM(SCALE.&lcSclCnt) , lclinesNor)
      FOR lnStylFld = 1 TO 12
        lcTempFld = "Hdr" + ALLTRIM(STR(lnStylFld))
        lcQtyFld  = "Qty" + ALLTRIM(STR(lnStylFld))
        lcStyl    = ALLTRIM(STR(lnSclCnt))

		IF STYLE.WIP&lcStyl + ( STYLE.STK&lcStyl - STYLE.ORD&lcStyl ) # 0
          IF ALLTRIM(SUBSTR(SCALE.SCALE,2)) = ALLTRIM(&lclinesNor..&lcTempFld)
	        REPLACE &lclinesNor..&lcQtyFld WITH STYLE.WIP&lcStyl + ( STYLE.STK&lcStyl - STYLE.ORD&lcStyl )

	        REPLACE &lclinesNor..WipFld   WITH &lclinesNor..WipFld + STYLE.WIP&lcStyl                        ,;
	                &lclinesNor..TotWplsH WITH &lclinesNor..TotWplsH + (STYLE.STK&lcStyl - STYLE.ORD&lcStyl) ,;
	                &lclinesNor..Totqty   WITH &lclinesNor..Totqty + STYLE.WIP&lcStyl + (STYLE.STK&lcStyl - STYLE.ORD&lcStyl)
	        EXIT
          ENDIF
		ENDIF
      ENDFOR
    ENDIF
  ENDFOR
ENDIF

SELECT(lcAlias)

*--End of lfInsrtVal.
*!*************************************************************
*! Name      : lfaddrcrds
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Function to add records in Temp. File.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : .PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfaddrcrds()
*!*************************************************************
FUNCTION lfaddrcrds
PRIVATE lcAlias , lcScalChck

lcAlias = ALIAS()
SELECT (lclinesNor)

FOR lnSz = 1 TO ALEN(laSortSzes,1)
  IF LEFT(laSortSzes[lnSz , 1] , 1 ) = lckey
    IF lnSz # 1
      SCATTER MEMVAR MEMO
    ENDIF
    lcScalChck = ScalLtr
    APPEND BLANK

    IF lnSz # 1
      GATHER MEMVAR MEMO FIELDS EXCEPT Qty* , WipFld , TotWplsH , TotQty
    ENDIF
   
    *--mhm
    *REPLACE Style     WITH SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)     ,;
            cdivision WITH STYLE.CDIVISION                              ,;
            Season    WITH STYLE.SEASON                                 ,;
			Color     WITH SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl)     ,;
			Inseam    WITH ALLTRIM(SUBSTR(laSortSzes[lnSz,1],2))        ,;
			ScalLtr   WITH lckey
    REPLACE Style     WITH SUBSTR(STYLE.STYLE,lnStyPosGl,lnStyLnGl)     ,;
            cdivision WITH STYLE.CDIVISION                              ,;
            Season    WITH STYLE.SEASON                                 ,;
			Color     WITH SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl)     ,;
			Inseam    WITH ALLTRIM(SUBSTR(laSortSzes[lnSz,1],2))        ,;
			SizOrd    WITH ALLTRIM(laSortSzes[lnSz,2])                  ,;
			ScalLtr   WITH lckey
    *--mhm2004

    lnCount = 0
    IF lnSz = 1	 OR lcScalChck # ScalLtr    && Add the scales in the first time only then get them from the first recored,
      REPLACE Hdr1  WITH SPACE(0) ,;
              Hdr2  WITH SPACE(0) ,;
              Hdr3  WITH SPACE(0) ,;
              Hdr4  WITH SPACE(0) ,;
              Hdr5  WITH SPACE(0) ,;
              Hdr6  WITH SPACE(0) ,;
              Hdr7  WITH SPACE(0) ,;
              Hdr8  WITH SPACE(0) ,;
              Hdr9  WITH SPACE(0) ,;
              Hdr10 WITH SPACE(0) ,; 
              Hdr11 WITH SPACE(0) ,;
              Hdr12 WITH SPACE(0)

      FOR lnHedr = 1 TO ALEN(laGetHdrs,1)
	    IF laGetHdrs[lnHedr , 2] = lckey
	      lnCount = lnCount + 1 
	      lcHdrFld = "Hdr" + ALLTRIM(STR(lnCount))
	      REPLACE &lclinesNor..&lcHdrFld WITH laGetHdrs[lnHedr,1]
        ENDIF
	  ENDFOR
    ENDIF

  ENDIF
ENDFOR

SELECT(lcAlias)

*--End of lfaddrcrds.
*!*************************************************************
*! Name      : lfGetSizes
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Function to collect the scale data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : .PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfGetSizes()
*!*************************************************************
FUNCTION lfGetSizes
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT SCALE
LOCATE

IF SEEK("S" + SUBSTR(STYLE.STYLE , lnScaPosGl , lnScaLnGl))
  SCAN FOR TYPE + SCALE + PREPAK = "S" + lcKey
    lnHdr = IIF(lnHdr = 0 , 1 , ALEN(laGetHdrs,1) + 1)
    DIMENSION laGetHdrs[lnHdr,2]
    =AINS(laGetHdrs, lnHdr)
    laGetHdrs[lnHdr,1] = ALLTRIM(SUBSTR(SCALE.SCALE,2))
    laGetHdrs[lnHdr,2] = lcKey
  
    FOR lnSz = 1 TO SCALE.CNT
      lcSze = "Sz" + ALLTRIM(STR(lnSz))
      lcValue = ALLTRIM(LEFT(SCALE.SCALE,1) + ALLTRIM(SCALE.&lcSze))
      IF ASCAN(laSortSzes, lcValue) = 0
        *--Scale Size not found add it.
        lnI = IIF(lnI = 0 , 1 , ALEN(laSortSzes,1) + 1)
        *--mhm
      	*DIMENSION laSortSzes[lnI,1]
        *=AINS(laSortSzes, lnI)
    	*laSortSzes[lnI] = lcValue
      	DIMENSION laSortSzes[lnI,2]
        =AINS(laSortSzes, lnI,1)
    	laSortSzes[lnI,1] = lcValue
    	laSortSzes[lnI,2] = ALLTRIM(STR(lnSz))
        *--mhm
      ENDIF
    ENDFOR
  ENDSCAN
  *--Sort the array.
  =ASORT(laSortSzes)
ENDIF

SELECT(lcAlias)

*--End of lfGetSizes.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : To get major segment Picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfsrSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
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

*--End of lfsrvSty.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!*************************************************************
*! Name      : lfChngForm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Change printed form.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChngForm()
*!*************************************************************
FUNCTION lfChngForm

IF lcRPSortBy == 'D'
  lcRPFormNa = "ICOTSNOD"
ELSE
  lcRPFormNa = "ICOTSNOS"
ENDIF

=lfRepPltFr(lcRPFormNa)

*--End of lfChngForm.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/30/2004
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.