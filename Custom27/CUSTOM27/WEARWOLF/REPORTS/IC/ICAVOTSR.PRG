*:***************************************************************************
*: Program file  : ICAVOTSR.PRG
*: Program desc. : CUSTOMIZED AVAILABLE TO SELL REPORT FOR WEARWOLF.
*: Date          : 10/29/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102642 [B606540,1]
*:***************************************************************************
*: Calls : lfCreatTmp , lfcolctDat , lfUpdatVal , lfMajTtGet , lfMajPic , lfsrSty
*:       : lfAddField , lfBasToClr
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICAVOTSR
*:***************************************************************************
*: Modifications :
*: B606679,1 BWA 11/24/2002 Fix the bug of printing Zero total and correct the spelling.
*: B606824,1 BWA 12/31/2002 Modify some fields in the FRX due to customer.[FIX IN FRX]
*:***************************************************************************

*--THE COLOR LENGTH.
STORE 0 TO lnClrLnAv , lnClrPosAv
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1] = 'C'
    lnClrLnAv  = LEN(laItemSeg[lnCount,3])
    lnClrPosAv = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--The Style length.
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

*B606679,4 BWA 12/04/2002 Creat array hold the scale 'A'.[STRAT]
*--Creat array hold the scale A to ba able to control in it like the customer want.
DIMENSION laScaleA[11]
laScaleA[1]  = "A05"
laScaleA[2]  = "A10"
laScaleA[3]  = "A15"
laScaleA[4]  = "A20"
laScaleA[5]  = "A25"
laScaleA[6]  = "A30"
laScaleA[7]  = "A35"
laScaleA[8]  = "A40"
laScaleA[9]  = "A45"
laScaleA[10] = "A50"
laScaleA[11] = "A55"
*B606679,4 BWA 12/04/2002.[END]

&& lnFldNum Variable hold the biggest size breakdown of the scale to creat the temp. file.
&& lnFldRow Variable hold the number of the scale descreption.
STORE 0 TO lnFldNum , lnFldRow

*--The main functions of the report.
=lfCreatTmp()
=lfColctDat()

*--Section determines the Pipe that will print after the Qty filed.
DIMENSION laQtyPip[62,1]
=lfFillAray()

*B606679,1 BWA 11/24/2002 Print the first 2 chr. in case the Scale is "D".[START]
*--Case Wanted from the customer. Print first 2 chr. from Scale "D" in the header.
SELECT (lcPrnHdr)
LOCATE
IF SEEK("D")
  FOR ln2 = 1 TO lnFldNum
    lc2 = ALLTRIM(STR(ln2))
    IF !(ALLTRIM(&lcPrnHdr..SZ&lc2) == "QTY")
      REPLACE &lcPrnHdr..SZ&lc2 WITH LEFT(&lcPrnHdr..SZ&lc2,2)
    ENDIF
  ENDFOR
ENDIF
*B606679,1 BWA 11/24/2002.[END]

*-- Browsing the report.
SELECT (lcPrntTmp)
LOCATE
IF RECCOUNT() = 0 .OR. EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  SET RELATION TO &lcPrntTmp..SCALE INTO (lcPrnHdr) ADDITIVE

  *B606679,1 BWA 11/24/2002 Fix the bug of printing Zero total.[START]
  *DO gfDispRe WITH EVALUATE('lcRPFormNa	')
  DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + "&lcPrntTmp..TotLine <> 0 "
  *B606679,1 BWA 11/24/2002.[END]

ENDIF
WAIT CLEAR

*--Break the relation of the Temp. file.
SELECT (lcPrntTmp)
SET RELATION TO

*--Call the function of deleting the temprory files.
=lfBasToClr(lcPrntTmp , 'F')
=lfBasToClr(lcPrnHdr , 'F')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*--Section to print the Temp. File for the scale size.
&& lcScalChr Variable hold the select scale to use it in the comparison.
&& lnScale   Variable hold count of the size breakdown of the scale.

STORE SPACE(0) TO lcScalChr
STORE 0 TO lnScale

SELECT SCALE
SCAN
  IF lcScalChr # LEFT(SCALE.SCALE,1)
    lcScalChr = LEFT(SCALE.SCALE,1)
    STORE 0 TO lnScale
    lnScale   = lnScale + SCALE.CNT
  ELSE
    lnScale   = lnScale + SCALE.CNT
  ENDIF
    lnFldNum  = IIF( lnFldNum > lnScale , lnFldNum , lnScale)
ENDSCAN

*--Creat table hold the header and the size breakdown to print from it in the header of the FRX.
DIMENSION laPrnHdr[1,4]

*--Main fields.
=lfAddField("laPrnHdr", "Scale" , "C",1  ,0)
=lfAddField("laPrnHdr", "lcPos" , "C",30 ,0)

*--Creat the Size fields for the Temp. File
&& 10 it is an imagen number for the Qty fields which will be add to the size breakdown.
lnFldNum = lnFldNum + 10
FOR lnCrtTmp = 1 TO lnFldNum
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laPrnHdr", "Sz" + lcNumSiz ,"C",5,0)
ENDFOR

CREATE Table (lcPrnHdr) FROM ARRAY laPrnHdr
ZAP
INDEX ON Scale TAG Scale OF (gcWorkDir + lcPrnHdr + ".CDX")

*B606679,4 BWA 12/04/2002 Add the scale sizes in the header file as the customer demand.[STRAT]
*--Special case due to the customer demand. For the scale "A"
DIMENSION laSizeA1[60,2] , laSizeA2[60,2] , laSizeA3[60,2] , laSizeA4[60,2]
STORE SPACE(0) TO laSizeA1 , laSizeA2 , laSizeA3 , laSizeA4

SELECT SCALE
=SEEK("SA")
SCAN REST WHILE TYPE + SCALE + PREPAK = "SA"
  DO CASE
    CASE ALLTRIM(SCALE) $ "A05 , A10 , A30 , A35"
      FOR lnLp1 = 1 TO 8
        lcLp1 = "SZ" + ALLTRIM(STR(lnLp1))
        IF !EMPTY(SCALE.&lcLp1)
          FOR lnAr = 1 TO ALEN(laSizeA1,1)
            IF EMPTY(laSizeA1[lnAr,1])
              laSizeA1[lnAr,1] = "A"
              laSizeA1[lnAr,2] = SCALE.&lcLp1
             EXIT
            ENDIF
          ENDFOR
        ENDIF
      ENDFOR

    CASE ALLTRIM(SCALE) $ "A15 , A20"
      FOR lnLp1 = 1 TO 8
        lcLp1 = "SZ" + ALLTRIM(STR(lnLp1))
        IF !EMPTY(SCALE.&lcLp1)
          FOR lnAr = 1 TO ALEN(laSizeA2,1)
            IF EMPTY(laSizeA2[lnAr,1])
              laSizeA2[lnAr,1] = "B"
              laSizeA2[lnAr,2] = SCALE.&lcLp1
              EXIT
            ENDIF
          ENDFOR
        ENDIF
      ENDFOR
    
    CASE ALLTRIM(SCALE) $ "A25 , A40 , A45"
      FOR lnLp1 = 1 TO 8
        lcLp1 = "SZ" + ALLTRIM(STR(lnLp1))
        IF !EMPTY(SCALE.&lcLp1)
          FOR lnAr = 1 TO ALEN(laSizeA3,1)
            IF EMPTY(laSizeA3[lnAr,1])
              laSizeA3[lnAr,1] = "C"
              laSizeA3[lnAr,2] = SCALE.&lcLp1
              EXIT
            ENDIF
          ENDFOR
        ENDIF
      ENDFOR

    CASE ALLTRIM(SCALE) $ "A50 , A55"
      FOR lnLp1 = 1 TO 8
        lcLp1 = "SZ" + ALLTRIM(STR(lnLp1))
        IF !EMPTY(SCALE.&lcLp1)
          FOR lnAr = 1 TO ALEN(laSizeA4,1)
            IF EMPTY(laSizeA4[lnAr,1])
              laSizeA4[lnAr,1] = "D"
              laSizeA4[lnAr,2] = SCALE.&lcLp1
              EXIT
            ENDIF
          ENDFOR
        ENDIF
      ENDFOR
  ENDCASE
ENDSCAN

*--Section sort the array and creat the record in the header file.
SELECT (lcPrnHdr)
APPEND BLANK
REPLACE Scale WITH "A"

FOR lnQty = 1 TO 4
  lcQty = ALLTRIM(STR(lnQty))
  FOR lnLop = 1 TO ALEN(laSizeA&lcQty,1)
    IF !EMPTY(laSizeA&lcQty[lnLop,1])

      *--Section to add the fields.
      FOR lnAdd = 1 TO lnFldNum
        lcAdd = ALLTRIM(STR(lnAdd))
        IF EMPTY(&lcPrnHdr..SZ&lcAdd)
          REPLACE &lcPrnHdr..SZ&lcAdd WITH laSizeA&lcQty[lnLop,2]
          EXIT
        ENDIF
     ENDFOR
      
    ELSE
      *--Section to add the qty field.
      FOR lnAdd = 1 TO lnFldNum
        lcAdd = ALLTRIM(STR(lnAdd))
        IF EMPTY(&lcPrnHdr..SZ&lcAdd)
          REPLACE &lcPrnHdr..SZ&lcAdd WITH "QTY"
          EXIT
        ENDIF
      ENDFOR
      EXIT
    ENDIF
  ENDFOR
ENDFOR
*B606679,4 BWA 12/04/2002.[END]

*--The Temporary Index for the scale file to get all the size breakdown in 1 line.
&& lcChrDim1  Variable hold the scale cdim1.
&& lcScalComp Variable hold the 1st chr. of the scale for the comparison.
STORE SPACE(0) TO lcChrDim1 , lcScalComp

SELECT SCALE
LOCATE
*--mhm 24/11/2005
=SEEK('S')
*--mhm 24/11/2005
*B606679,4 BWA 12/04/2002 Scan the scale file for all the scales except the scale "A".[STRAT]
*SCAN REST WHILE TYPE + SCALE + PREPAK = "S"
SCAN REST WHILE TYPE + SCALE + PREPAK = "S" FOR LEFT(SCALE.SCALE,1) # "A"
*B606679,4 BWA 12/04/2002.[END]

  IF SEEK(LEFT(SCALE.Scale,1) , lcPrnHdr)
    IF !(lcChrDim1 == ALLTRIM(CDIM1))
      lcChrDim1 = ALLTRIM(CDIM1)
      SELECT (lcPrnHdr)

      *--Section to add the qty field.
      FOR lnAdd = 1 TO lnFldNum
        lcAdd = ALLTRIM(STR(lnAdd))
        IF EMPTY(&lcPrnHdr..SZ&lcAdd)
          REPLACE &lcPrnHdr..SZ&lcAdd WITH "QTY"
          EXIT
        ENDIF
     ENDFOR

     *--Section to add the new line.
     lnSizToAdd = 0
     FOR lnAdd = 1 TO lnFldNum
       lcAdd = ALLTRIM(STR(lnAdd))
       IF EMPTY(&lcPrnHdr..SZ&lcAdd)
         lnSizToAdd = lnSizToAdd + 1
         lcSizToAdd = ALLTRIM(STR(lnSizToAdd))
         IF lnSizToAdd <= SCALE.CNT
           IF !EMPTY(SCALE.SZ&lcSizToAdd)
             REPLACE &lcPrnHdr..SZ&lcAdd WITH SCALE.SZ&lcSizToAdd
           ELSE
             EXIT
           ENDIF
         ELSE
           EXIT
         ENDIF
       ENDIF
     ENDFOR

    ELSE

      SELECT (lcPrnHdr)
      lnSizToAdd = 0
      FOR lnAdd = 1 TO lnFldNum
        lcAdd = ALLTRIM(STR(lnAdd))
        IF EMPTY(&lcPrnHdr..SZ&lcAdd)
          lnSizToAdd = lnSizToAdd + 1
          lcSizToAdd = ALLTRIM(STR(lnSizToAdd))
          IF lnSizToAdd <= SCALE.CNT
            IF !EMPTY(SCALE.SZ&lcSizToAdd)
              REPLACE &lcPrnHdr..SZ&lcAdd WITH SCALE.SZ&lcSizToAdd
            ELSE
              EXIT
            ENDIF
          ELSE
            EXIT
          ENDIF
        ENDIF
      ENDFOR
    ENDIF

  ELSE

    *--Section to print "QTY" in the last field of the scale.
    IF lcScalComp # LEFT(SCALE.SCALE,1) AND !EMPTY(lcScalComp)
      SELECT (lcPrnHdr)
      =SEEK(lcScalComp , lcPrnHdr)
      FOR lnAdd = 1 TO lnFldNum
        lcAdd = ALLTRIM(STR(lnAdd))
        IF EMPTY(&lcPrnHdr..SZ&lcAdd)
          REPLACE &lcPrnHdr..SZ&lcAdd WITH "QTY"
          EXIT
        ENDIF
      ENDFOR
    ENDIF
    lcScalComp = LEFT(SCALE.SCALE,1)

    *--Section to print the new line for the new scale.
    SELECT SCALE
    lcChrDim1 = ALLTRIM(CDIM1)
    SCATTER MEMVAR MEMO
    SELECT (lcPrnHdr)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE Scale WITH LEFT(SCALE.Scale,1)

  ENDIF
ENDSCAN

*--For the last line in the scale file.
*--Section to add the qty field for the last scale.
SELECT (lcPrnHdr)
FOR lnAdd = 1 TO lnFldNum
  lcAdd = ALLTRIM(STR(lnAdd))
  IF EMPTY(&lcPrnHdr..SZ&lcAdd)
    REPLACE &lcPrnHdr..SZ&lcAdd WITH "QTY"
    EXIT
  ENDIF
ENDFOR

*--The temp. file which I will print from it.
*--Creat the temp file from the scale array.
DIMENSION laPrntTemp[1,4]

*--Main fields.
=lfAddField("laPrntTemp","StyGroup","C",6,0)
=lfAddField("laPrntTemp","Style"   ,"C",6,0)
=lfAddField("laPrntTemp","Color"   ,"C",4,0)
=lfAddField("laPrntTemp","Scale"   ,"C",1,0)
=lfAddField("laPrntTemp","TotLine" ,"N",5,0)

FOR lnHd = 1 TO lnFldNum
  lcHdNum = ALLTRIM(STR(lnHd))
  =lfAddField("laPrntTemp", "Sz" + lcHdNum ,"N",5,0)
ENDFOR

*--Creat this temp. file to collect the data.
CREATE Table (lcPrntTmp) FROM ARRAY laPrntTemp
ZAP
*ashraf on 03/09/2003
*INDEX ON StyGroup + Style TAG StyGrop OF (gcWorkDir + lcPrntTmp + ".CDX")
INDEX ON StyGroup + Scale + Style TAG StyGrop OF (gcWorkDir + lcPrntTmp + ".CDX")

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfcolctDat()
*!*************************************************************
FUNCTION lfcolctDat

&& lnTotVal   Variable hold the all value for the style
&& lnTotOts   Variable hold all the value for the open to sell style
&& lcStylText Variable hold the first 6 chr of the style and the 1st 4 chr of the color.

STORE SPACE(0) TO lcStylText
STORE 0 TO lnTotVal  && Variable hold the all value for the style
STORE 0 TO lnTotOts  && Variable hold all the value for the open to sell style

SELECT STYLE
IF !('"S" + SCALE INTO SCALE' $ SET('RELATION'))
  SET RELATION TO "S" + SCALE INTO SCALE ADDITIVE
  SET RELATION TO LEFT(STYLE.SCALE,1) INTO (lcPrnHdr) ADDITIVE
ENDIF

LOCATE
SCAN FOR &lcRpExp
  WAIT WINDOW 'Collecting Data For Style#...' + LEFT(STYLE.STYLE,lnLenth) NOWAIT

  IF lcStylText # LEFT(STYLE.STYLE , 6) + SUBSTR(STYLE.STYLE , lnClrPosAv , 4)
     lcStylText = LEFT(STYLE.STYLE , 6) + SUBSTR(STYLE.STYLE , lnClrPosAv , 4)

     SELECT (lcPrntTmp)
     APPEND BLANK
     REPLACE StyGroup WITH STYLE.CstyGroup                      ,;
             Style    WITH LEFT(STYLE.STYLE , 6)                ,;
             Scale    WITH LEFT(STYLE.SCALE , 1)                ,;
             Color    WITH SUBSTR(STYLE.STYLE , lnClrPosAv , 4)
  ENDIF
  =lfUpdatVal()
ENDSCAN

*--Break the relation.
SELECT STYLE
SET RELATION TO

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfUpdatVal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
*! Purpose   : Function to get the values from the style to the tmp.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUpdatVal()
*!*************************************************************
FUNCTION lfUpdatVal
PRIVATE lcAlasSty

lcAlasSty = SELECT(0)
lnSerch = 1
SELECT STYLE
FOR lnVal = 1 TO SCALE.CNT
  lcVal = ALLTRIM(STR(lnVal))
  IF !EMPTY(SCALE.SZ&lcVal)
    FOR lnSerch = 1 TO lnFldNum
      lcSerch = ALLTRIM(STR(lnSerch))
      IF SCALE.SZ&lcVal == &lcPrnHdr..Sz&lcSerch
        lcOrdVal = 'ORD' + lcVal
        lcWIPVal = 'WIP' + lcVal
        lcSTKVal = 'STK' + lcVal
        lnOtsVal = STYLE.&lcSTKVal + STYLE.&lcWIPVal - STYLE.&lcOrdVal

        *--Section check the open to sell quantities due to the user criteria from option "Print OTS Quantities".
        IF lcRPOts = "P"
          lnOtsVal = IIF(lnOtsVal < 0 , 0 , lnOtsVal)
        ELSE
          IF lcRPOts = "N"
            lnOtsVal = IIF(lnOtsVal > 0 , 0 , lnOtsVal)
          ENDIF
        ENDIF
        REPLACE &lcPrntTmp..Sz&lcSerch WITH lnOtsVal
        lnTotOts = lnTotOts + lnOtsVal
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDFOR

*--Section add the total quantity for all the sizes.
FOR lnQty = lnSerch TO lnFldNum
  lcQty   = ALLTRIM(STR(lnQty))
  IF ALLTRIM(&lcPrnHdr..Sz&lcQty) == "QTY"
     REPLACE &lcPrntTmp..Sz&lcQty WITH &lcPrntTmp..Sz&lcQty + lnTotOts
     EXIT
  ENDIF
ENDFOR

*--Update the TotLine.
REPLACE &lcPrntTmp..TotLine WITH &lcPrntTmp..TotLine + lnTotOts

*--Get it's initial value.
STORE 0 TO lnTotOts

SELECT(lcAlasSty)
*--End of lfUpdatVal
*!*************************************************************
*! Name      : lfMajTtGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
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
*! Name      : lfsrSty
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
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

*--End of lfsrSty.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/29/2002
*! Purpose   : Add fields to the array of file structer.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
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
*!*************************************************************
*! Name      : lfFillAray
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/29/2002
*! Purpose   : Fill the array hold the numbers that will show the pipe.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfFillAray()
*!*************************************************************
FUNCTION lfFillAray

*--Section determines the Pipe that will print after the Qty filed.
* 1 >> A1 , 10 >> B1 , 19 >> C1 , 28 >> D1 , 37 >> E1 , 46 >> F1 , 55 >> G1
* 2 >> A2 , 11 >> B2 , 20 >> C2 , 29 >> D2 , 38 >> E2 , 47 >> F2 , 56 >> G2
* 3 >> A3 , 12 >> B3 , 21 >> C3 , 30 >> D3 , 39 >> E3 , 48 >> F3 , 57 >> G3
* 4 >> A4 , 13 >> B4 , 22 >> C4 , 31 >> D4 , 40 >> E4 , 49 >> F4 , 58 >> G4
* 5 >> A5 , 14 >> B5 , 23 >> C5 , 32 >> D5 , 41 >> E5 , 50 >> F5 , 59 >> G5
* 6 >> A6 , 15 >> B6 , 24 >> C6 , 33 >> D6 , 42 >> E6 , 51 >> F6 , 60 >> G6
* 7 >> A7 , 16 >> B7 , 25 >> C7 , 34 >> D7 , 43 >> E7 , 52 >> F7 , 61 >> G7
* 8 >> A8 , 17 >> B8 , 26 >> C8 , 35 >> D8 , 44 >> E8 , 53 >> F8 , 62 >> G8
* 9 >> A9 , 18 >> B9 , 27 >> C9 , 36 >> D9 , 45 >> E9 , 54 >> F9 ,

FOR lnChr = 1 TO 62

  DO CASE
    CASE CEILING(lnChr/9) = 1
      lcStr = "A"
    CASE CEILING(lnChr/9) = 2
      lcStr = "B"
    CASE CEILING(lnChr/9) = 3
      lcStr = "C"
    CASE CEILING(lnChr/9) = 4
      lcStr = "D"
    CASE CEILING(lnChr/9) = 5
      lcStr = "E"
    CASE CEILING(lnChr/9) = 6
      lcStr = "F"
    CASE CEILING(lnChr/9) = 7
      lcStr = "G"
  ENDCASE

  lcChr = ALLTRIM(STR(lnChr - ((CEILING(lnChr/9)-1) * 9)))
  laQtyPip[lnChr,1] = lcStr + lcChr
ENDFOR

*--Check the postion of the Pipen.
SELECT (lcPrnHdr)
SCAN

  STORE SPACE(0) TO lcExper
  FOR lnFld = 1 TO 62
    lcFld = ALLTRIM(STR(lnFld))
    IF ALLTRIM(&lcPrnHdr..Sz&lcFld) == "QTY"
      lcExper = lcExper + laQtyPip[lnFld,1] + ','
    ENDIF
  ENDFOR
  REPLACE lcPos WITH lcExper
  
ENDSCAN
*--End of lfFillAray