****************************************************************************
*: Program file  : ICSLCAT1.PRG 
*: Program desc. : Sales discriptive list with price by style
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Control (IC)
*: Developer     : TMI - Tarek Mohamed Ibrahim 
*: Date          : 04/10/2001
*:**************************************************************************
*: Calls : FUNCTIONS  : lfSVSty() 
*:                    : lfItmPos()
*:                    : lfvUpdSes()
*:                    : lfGetDefVl()
*:                    : lfSesLngNm()
*:                    : lfGetClrD()
*:                    : lfColClrs()
*:                    : lfSesDsc()
*:                    : lfGetSizes()
*:                    : lfFrmtDate()
*:                    : lfwRepWhen()
*:                    : lfvDvsion()
*:                    : lfvGroup()
*:                    : lfvCrATVar()
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:Modifications  :
*:B604437,1 TMI 05/07/2001 Put mover for division and groups and other bugs
*:B604468,1 TMI 05/09/2001 Some modifications for discriptive reports
*:B604883,1 ASH 09/06/2001 Change the print sort to be with fabric short description instead of fabric code.
*:B605149,1 BWA 11/26/2001 1-Add new filter for sorting by style.
*:B605149,1 BWA 12/03/2001 Sort the color field.
*:E301837,1 BWA 02/14/2002 Add new filter for the style status to the grid.
*:B605696,1 ASH 03/17/2002 Apply the status filter to Style/Color not style major only.
*:B606408,1 BWA 08/22/2002 Fix the bug of printing 6 Colors to be 8 Colors [Fix in FRX]
*:**************************************************************************
PRIVATE lnClrPos,lnClrLen,lnOldSlct,lcStyOrder , lcLnFed ,;
        M.CSTYMAJOR , M.DESC , M.SIZE , M.COLOR , M.MSALESDESC , M.PRICEA , M.NSUGRETPRI 
*-- lnClrPos : holds color position
*-- lnClrLen : hold  color length 
*-- lnOldSlct: Old selected work area
*-- lcStyOrder:Hold STYLE ORDER
*-- lcLnFed: line feed and carriage return
lnOldSlct = SELECT(0)
STOR 0 TO lnClrPos,lnClrLen        
lcLF = CHR(13) + CHR(10)
STORE '' TO M.CSTYMAJOR , M.DESC , M.SIZE , M.COLOR , M.MSALESDESC , M.PRICEA , M.NSUGRETPRI

*-- TMI B604437 [Start] If no BMP file is selected then exit
IF EMPTY(lcRpBmpFil)
  =gfDialog('I','A BMP file must be selected.')
  RETURN
ENDIF
*-- Link the bmp file selected to the field Logo
lcBmpFile = gfTempName()
CREATE CURSOR (lcBmpFile) (Logo G)
APPEND BLANK
*-- TMI B604468 [Start] Add default path to the BMP
*APPEND GENERAL Logo FROM (lcRpBmpFil) LINK
APPEND GENERAL Logo FROM (gcDef_Path+'DLBMPS\'+lcRpBmpFil) LINK
*-- TMI B604468 [END  ]
*-- TMI B604437 [End  ]

*-- Get color position and color length
=lfGetClrD()

*-- Temp Cursor
*B605149,1 BWA 11/26/2001 Add the creation of the Temp name in the SYREPUVR.DBF[START]
*lcTempCur = gfTempName()
*B605149,1 BWA 11/26/2001.[END]

*-- TMI B604437 [Start] Add expression obtained from movers to lcRpExp
STORE SPACE(0) TO lcDivision,lcGroups
=lfvCrATVar()
*-- TMI B604437 [End  ]

*-- Create cursor fields
*B604883,1 ASH 09/06/2001 (Begin) Add fabric short description field, and add a new index to sort on it.
CREATE CURSOR (lcTempCur) (CSTYMAJOR  C(19)  ,;
                           FABRIC     C(7)   ,;
                           SIZE       M      ,;
                           COLOR      M      ,;
                           MSALESDESC M      ,;
                           PRICEA     N(12,2),;
                           cFabShDesc C(43)  ,;
                           NSUGRETPRI N(8,2) )

INDEX ON cFabShDesc+CSTYMAJOR TAG ITEMSTYLE OF (lcTempCur)
*B604883,1 ASH 09/06/2001 (End)

*B605149,1 BWA 11/26/2001 Add new filter for sorting by style.[START]
INDEX ON CSTYMAJOR + cFabShDesc TAG STYLE OF (lcTempCur)
*B605149,1 BWA 11/26/2001.[END]

INDEX ON FABRIC+CSTYMAJOR TAG ITEM_STYLE OF (lcTempCur)
SET ORDER TO ITEM_STYLE

*-- Order Settings
SET ORDER TO TAG SCALE IN SCALE
SET ORDER TO TAG CFABRIC IN FABRIC

*-- TMI B604437 [Start] Open STYLE in a new work aria to collect colors from
lcStyCursr = gfTempName()
USE (gcDataDir+'STYLE') AGAIN IN 0 ALIAS (lcStyCursr) ORDER TAG STYLE
*-- TMI B604437 [End  ]

*-- Collecting data
SELECT STYLE
lcStyOrder = ORDER('STYLE')
SET ORDER TO Cstyle
*-- Cstyle : Unique index on STYLE.CSTYMAJOR
SET RELATION TO 'S'+SCALE INTO SCALE
=SEEK(ALLTRIM(lcRpSeaVal))
SCAN WHILE CSTYMAJOR = ALLTRIM(lcRpSeaVal) FOR &lcRpExp
  WAIT WINDOW 'Selecting record for style : ' + STYLE.CSTYMAJOR NOWAIT
  M.CSTYMAJOR  =  STYLE.CSTYMAJOR  
  M.FABRIC     =  STYLE.FABRIC
  *B604883,1 ASH 09/06/2001 (Begin) Add the short fabric description to the temp file.
  IF SEEK(M.Fabric,'Fabric')
    M.cFabShDesc = Fabric.cFabShDesc 
  ELSE
    M.cFabShDesc = ''
  ENDIF
  *B604883,1 ASH 09/06/2001 (End)
  *TMI B604437 [Start] Create a string holds sizes without blank lines
  *M.SIZE       =  ALLT(SCALE.SZ1)+lcLF+ALLT(SCALE.SZ2)+lcLF+ALLT(SCALE.SZ3)+lcLF+ALLT(SCALE.SZ4)+lcLF+ALLT(SCALE.SZ5)+lcLF+ALLT(SCALE.SZ6)+lcLF+ALLT(SCALE.SZ7)+lcLF+ALLT(SCALE.SZ8)
  M.SIZE       = lfGetSizes()
  *TMI B604437 [End  ]
  M.COLOR      =  lfColClrs(LEFT(STYLE.CSTYMAJOR,lnClrPos-2))
  M.MSALESDESC =  STYLE.MSALESDESC
  M.PRICEA     =  STYLE.PRICEA
  M.NSUGRETPRI =  STYLE.NSUGRETPRI
  *B605696,1 ASH 03/17/2002 (Begin) Don't print the style if all it's colors not matching the status filter.
  IF !EMPTY(M.COLOR)
  *B605696,1 ASH 03/17/2002 (End)
    INSERT INTO (lcTempCur) FROM MEMVAR
  ENDIF
ENDSCAN


*B605149,1 BWA 12/03/2001 Sort the color field.[START]
DIMENSION laSortMemo[1]
SELECT (lcTempCur)
SCAN
  lnMemLins = 0
  lnMemLins = MEMLINES(EVAL(lcTempCur+'.COLOR'))

  IF lnMemLins > 1
    DIMENSION laSortMemo[lnMemLins]
    STORE SPACE(0) TO laSortMemo
    FOR lnNum = 1 TO lnMemLins
      laSortMemo[lnNum] = MLINE(EVAL(lcTempCur+'.COLOR'),lnNum)
    ENDFOR

    =ASORT(laSortMemo)
    FOR lnFill = 1 TO lnMemLins
      IF lnFill = 1
        REPLACE &lcTempCur..COLOR WITH laSortMemo[lnFill] + IIF(lnFill = lnMemLins , "" , CHR(13) + CHR(10) )
      ELSE
        REPLACE &lcTempCur..COLOR WITH laSortMemo[lnFill] + IIF(lnFill = lnMemLins , "" , CHR(13) + CHR(10) ) ADDITIVE
      ENDIF
    ENDFOR
  ENDIF
ENDSCAN
*B605149,1 BWA 12/03/2001.[END]


*-- TMI B604437 [Start] Close (lcStyCursr)
IF USED (lcStyCursr)
  USE IN (lcStyCursr)  
ENDIF
*-- TMI B604437 [End  ]

SELECT STYLE
SET ORDER TO &lcStyOrder

*-- Set Order 
lcObLnkOrd = ORDER('OBJLINK')
lcObjOrd   = ORDER('OBJECTS')
SET ORDER TO TAG Objlnkty IN OBJLINK
SET ORDER TO TAG OBJECTID IN OBJECTS

*-- Set relations
SELECT (lcTempCur)
SET RELATION TO "S"+CSTYMAJOR INTO OBJLINK
*SET RELATION TO FABRIC INTO FABRIC ADDITIVE

SELECT OBJLINK
SET RELATION TO COBJECT_ID INTO OBJECTS

*-- Run the report
SELECT (lcTempCur)
*B604883,1 ASH 09/06/2001 (Begin) Set orderto the new index with the fabric description.

*B605149,1 BWA 11/26/2001 Add new filter for sorting by style.[START]
*SET ORDER TO ITEMSTYLE
IF lcRpSortd = "F"
  SET ORDER TO ITEMSTYLE
ELSE
  SET ORDER TO STYLE
ENDIF
*B605149,1 BWA 11/26/2001.[END]

*B604883,1 ASH 09/06/2001 (End)

LOCATE
IF RECCOUNT()=0
  =gfDialog("I","There are no records to display.")
ELSE
  DO gfDispRe WITH EVAL('lcRpForm')  
ENDIF

SET ORDER TO TAG &lcObjOrd IN OBJECTS
SET ORDER TO TAG &lcObLnkOrd IN OBJLINK

SELECT(lnOldSlct)
*-- End of program KCSLCAT1.PRG

*!*************************************************************
*! Name      : lfSVSty
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : SRV ( or Set-Reset-Valid function for InRange)
*!*************************************************************
*! Passed Parameters  : lcParm ('S','R' or 'V')
*!*************************************************************
*! Returns            : ...
*!*************************************************************
*! Example   : = lfSVSty()
*!*************************************************************
FUNCTION lfSVSty
PARAMETERS lcParm
PRIVATE lcFltExpr
lcFltExpr = "SET FILTER TO"
SELECT STYLE
IF lcParm = "S"
  lcStyOrder = ORDER("STYLE")  
  SET ORDER TO Cstyle  
  IF !EMPTY(lcRpSeaVal)
  *-- TMI 604468,4 [Start] The style inlist shows no styles
    *lcFltExpr = [SET FILTER TO cStyMajor = '] + lcRpSeaVal + "'"
    lcFltExpr = [SET FILTER TO cStyMajor = '] + ALLT(lcRpSeaVal) + "'"
  *-- TMI 604468,4 [End  ]
  ENDIF

ELSE  && ELSE Rest case
  SET ORDER TO &lcStyOrder.
  
ENDIF
&lcFltExpr.
LOCATE
*-- end of lfSVSty.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : lcItmInFlt : Field name
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfvUpdSes
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : (Valid funciton of Season Popup)
*!             Updates 'm.lcRpSeaVal'
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfvUpdSes
*-- TMI B604468 [Start] no need to this line
*lcRpSeaVal = ALLTRIM(laOgFxFlt[lnSeaPos,6])
*-- TMI B604468 [End  ]
lcRpSeaDes = gfCodDes(lcRpSeaVal,'SEASON')
= lfOGShowGet('lcRpSeaDes')
*-- end of lfvUpdSes.

*!*************************************************************
*! Name      : lfGetDefVl
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Get default value of a code from Codes.dbf
*!*************************************************************
*! Passed Parameters  : lcCode : Code value
*!*************************************************************
*! Returns            : Code default value
*!*************************************************************
*! Example   : = lfGetDefVl("SEASON")
*!*************************************************************
FUNCTION lfGetDefVl
PARAMETERS lcCode
PRIVATE lcRetVal , lcCodeOrd
lcCodeOrd = ORDER("CODES")
SET ORDER TO Ccode_no IN CODES
lcRetVal = IIF(SEEK("D"+lcCode,"CODES"),Codes.cCode_No,"")
SET ORDER TO &lcCodeOrd. IN CODES
RETURN lcRetVal
*-- end of lfGetDefVl

*!*************************************************************
*! Name      : lfFillCode
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Dummy function Called from OG to fill popup arrays
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
PROCEDURE lfFillCode
PRIVATE lcCodeOrd , lnCount

lcCodeOrd = ORDER("CODES")

SET ORDER TO 0 IN CODES
= lfFillOne("laSeason"  ,"laSeaVal","SEASON"   )
*-- TMI B604437 [Start] No need to these lines
*=lfFillOne("laDivision","laDivVal","CDIVISION")
*=lfFillOne("laGroup"   ,"laGrpVal","CSTYGROUP")
*-- TMI B604437 [End  ]

SET ORDER TO &lcCodeOrd. IN CODES

*-- TMI B604437 [End  ] Fill mover arrays
DIMENSION laRpSDivi[1,1],laRpTDivi[1,1],laRpTGrp[1,1],laRpSGrp[1,1]
STORE '' TO laRpSGrp,laRpTGrp,laRpSDivi,laRpTDivi

*--The Division.
SELECT CODES
LOCATE
SELECT CCODE_NO+' - '+CDISCREP FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCDIVISION ' ;
INTO ARRAY laRpSDivi
                                        
LOCATE
SELECT CCODE_NO+' - '+CDISCREP FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCSTYGROUP ' ;
INTO ARRAY laRpSGrp

*-- TMI B604437 [End  ]

*-- TMI B604468 [Start] Fill laBmpFils Array
DIME laDummy[1,1]
DIME laBmpFils[1,1]
STORE '' TO laDummy,laBmpFils
=ADIR(laDummy,gcDef_Path+'DLBMPS\*.BMP')
DIME laBmpFils[ALEN(laDummy,1),1]
FOR I = 1 TO ALEN(laDummy,1) 
  laBmpFils[I] = laDummy[I,1]
NEXT
=ASORT(laBmpFils)
*-- TMI B604468 [End  ]

*-- end of lfFillCode.

*!*************************************************************
*! Name      : lfFillOne
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Fill one array from codes file
*!*************************************************************
*! Passed Parameters  : lcShowArry  : Array that appears in popup
*!                      lcValArray  : Return values array
*!                      lcCodeVal   : Filter Codes table on this Code 
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfFillOne("laSeason","laSeaVal","SEASON")
*!*************************************************************
PROCEDURE lfFillOne
PARAMETERS lcShowArry , lcValArray , lcCodeVal
DECLARE &lcShowArry.[1,1]

SELECT CCODE_NO+"-"+CDISCREP FROM CODES ;
  WHERE cDefCode + cRltField + cFld_Name = "NN" + lcCodeVal ;
  ORDER BY CCODE_NO ;
  INTO ARRAY &lcShowArry.

IF _TALLY > 0
  DECLARE &lcValArray.[_TALLY,1]
  lnCount = 0
  FOR lnCount = 1 TO _TALLY
    &lcValArray.[lnCount] = PADL(&lcShowArry.[lnCount],6) 
  ENDFOR
ENDIF
*-- end of lfFillOne.

*!*************************************************************
*! Name      : lfSesLngNm
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Get season long name from 'CATHCODE.DBF'
*!*************************************************************
*! Passed Parameters  : lcLngName is 'CSEALNAME' or 'CSEA2NAME'
*!*************************************************************
*! Returns            : Season long name 1 or 2
*!*************************************************************
*! Example   : = lfSesLngNm('CSEALNAME')
*!*************************************************************
FUNCTION lfSesLngNm
PARAMETERS lcLngName

lcToRet=''
lnAlias = SELECT(0)
SELECT CATHCODE
IF SEEK("Y"+PADR("SEASON",10)+PADR(lcRpSeaVal,6)+PADR(lcLngName,10))
  lcToRet = ALLT(CRLTD_VLU)
ENDIF
*-- TMI B604437 [Start] Return season long name as it is
*lcToRet = PPER(LEFT(lcToRet,1)) + LOWER(SUBSTR(lcToRet,2))
*-- TMI B604437 [End  ]
SELECT(lnAlias)
RETURN lcToRet
*-- end of lfSesLngNm.

*!*************************************************************
*! Name      : lfGetClrD
*! Developer : TMI
*! Date      : 04/03/2001
*! Purpose   : To get color position also color length
*!*************************************************************
*! Called from : ICSLCAT1.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD

*!*************************************************************
*! Name      : lfColClrs
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Create a string holds ALL colors of passed style
*!*************************************************************
*! Passed Parameters  : lcStyMajor
*!*************************************************************
*! Returns            : Style colors
*!*************************************************************
*! Example   : = lfColClrs("100-C22      ")
*!*************************************************************
FUNCTION lfColClrs
*-- Collect colors for this style
PARAMETERS lcStyMajor
PRIVATE lcStyOrder , lnOldRec , lnOldSlct , lcToRet
DIME laClrs[1]
lnOldSlct = SELECT(0)
STORE '' TO laClrs , lcToRet

lcStyOrder = ORDER('STYLE')
lnOldRec   = RECNO('STYLE')

*-- TMI B604437 [Start] Don't Change the order But select (lcStyCursr) [this is with order STYLE]
*SET ORDER TO STYLE
SELECT (lcStyCursr)
SEEK lcStyMajor
*-- TMI B604437 [End  ]

*-- Collect Colors
*B605696,1 ASH 03/17/2002 (Begin) Add the FOR expression to apply the status filter on style/color.
*SCAN REST WHILE STYLE = lcStyMajor FOR Status $ lcRpSt1
SCAN REST WHILE STYLE = lcStyMajor FOR IIF(!EMPTY(lcRpSt1),Status $ lcRpSt1,.T.)
*B605696,1 ASH 03/17/2002 (End)
  lcToRet = lcToRet + ALLT(gfCodDes(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR')) + lcLF
ENDSCAN
*-- Remove last lcLF
lcToRet = LEFT(lcToRet,LEN(lcToRet)-2)
  
*-- TMI B604437 [Start] No need for the following lines
*SELECT STYLE
*SET ORDER TO &lcStyOrder
*IF lnOldRec<=RECCOUNT()
*  GOTO (lnOldRec)
*ENDIF
*SELECT(lnOldSlct)
*-- TMI B604437 [End  ]

RETURN lcToRet
*-- end of lfColClrs.


FUNCTION lfSesDsc
lcToRet = ALLT(lcRpSeaDes)
lcToRet = UPPER(LEFT(lcToRet,1)) + LOWER(SUBSTR(lcToRet,2))
RETURN lcToRet
*-- end of lfSesDsc.

*:**************************************************************************
*:* Name        : lfGetSizes                          Entry B604437
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2001
*:* Purpose     : Get sizes 
*:***************************************************************************
*:* Called from : ICSSLCAT.PRG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : String contains sizes for this style
*:***************************************************************************
*:* Example     :  = lfGetSizes()
*:***************************************************************************
FUNCTION lfGetSizes
PRIVATE lcRetSizes
lcRetSizes = ''
FOR lnCount = 1 TO SCALE.CNT
  lcSz = ALLT(EVAL('SCALE.SZ'+STR(lnCount,1)))
  lcRetSizes = lcRetSizes+lcSz+lcLF
NEXT
*-- Remove last carriage return & line feed
lcRetSizes = LEFT(lcRetSizes,LEN(lcRetSizes)-2)
RETURN lcRetSizes
*-- end of lfGetSizes.

*:**************************************************************************
*:* Name        : lfFrmtDate                          Entry B604437
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/30/2001
*:* Purpose     : To format date as "April 05,2001"
*:***************************************************************************
*:* Called from : frx
*:***************************************************************************
*:* Passed Parameters : date to format
*:***************************************************************************
*:* Return      : formated date
*:***************************************************************************
*:* Example     :  = lfFrmtDate(date())
*:***************************************************************************
FUNCTION lfFrmtDate
PARAMETERS ldThisDate
*-- TMI B604468 [Start] IF NO ldThisDate PASSED RETUTN SPACE(0)
*RETURN CMONTH(ldThisDate)+' '+PADL(LTRIM(STR(DAY(ldThisDate),2)),2,'0')+','+STR(YEAR(ldThisDate),4)
RETURN IIF(EMPTY(ldThisDate),'',CMONTH(ldThisDate)+' '+PADL(LTRIM(STR(DAY(ldThisDate),2)),2,'0')+','+STR(YEAR(ldThisDate),4))
*-- TMI B604468 [End  ]
*-- end of lfFrmtDate.


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : Called when OG is opened
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ...
*!*************************************************************
*! Example   : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- TMI B604468 [Start] Season is put in a variable
*lnSeaPos = lfItmPos("STYLE.SEASON")
*-- TMI B604468 [End  ]
*-- TMI B604437 [Start] CDIVISION and CSTYGROUP are removed from fixed filter list,so comment these lines:
*lnDivPos = lfItmPos("STYLE.CDIVISION")
*lnGrpPos = lfItmPos("STYLE.CSTYGROUP")
*-- TMI B604437 [End  ]

*-- if it is the default case
IF lnOgSeting = 1

  *-- Assign default values    
*-- TMI B604468 [Start] Season is put in a variable
*  laOGFxFlt[lnSeaPos,6] = lcRpSeaVal
*-- TMI B604468 [End  ]

*-- TMI B604437 [Start] Comment the following
*  laOGFxFlt[lnDivPos,6] = lcRpDivVal
*  laOGFxFlt[lnGrpPos,6] = lcRpGrpVal
*-- TMI B604437 [End  ]

  *-- Show get the default values.
*-- TMI B604468 [Start]Season is put in a variable
*  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSeaPos)) + ',6]')  
*-- TMI B604468 [End  ]
*-- TMI B604437 [Start] Comment the following  
* = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDivPos)) + ',6]')  
* = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnGrpPos)) + ',6]')  
*-- TMI B604437 [End  ]

*-- TMI B604468 [Start] Trim lcRpSeaVal so the filter can work in default case
lcRpSeaVal = ALLTRIM(lfGetDefVl("SEASON"))
lcRpSeaDes = gfCodDes(lcRpSeaVal,'SEASON')
=lfOGShowGet('lcRpSeaVal')  
*-- TMI B604468 [End  ]

*-- TMI B604468 [Start] Fill laBmpFils Array
 lcRpBmpFil = laBmpFils[1,1]
 = lfOGShowGet('lcRpBmpFil')
*-- TMI B604468 [End  ]    
ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvDvsion        Entry B604437
*! Developer : TMI
*! Date      : 05/03/2001
*! Purpose   : Function to call mover Division.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
*--The Division.
FUNCTION lfvDvsion
= gfMover(@laRpSDivi,@laRpTDivi,'Select Divisions',.T.,'')
*--End of lfvDvsion.

*:**************************************************************************
*:* Name        : lfvGroup 					Entry B604437
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/03/2001
*:* Purpose     : Valid function for groups
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGroup()
*:***************************************************************************
FUNCTION lfvGroup
=gfMover(@laRpSGrp,@laRpTGrp,'Select Groups',.T.,'')
*-- end of lfvGroup.

*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : TMI
*! Date      : 05/03/2001
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

*--The filter
LCRPEXP = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

*--The Division.
FOR lnInd = 1 TO ALEN(laRpTDivi)
  lcDivision = lcDivision + PADR(laRpTDivi[lnInd],6) + ' | '
ENDFOR
lcDivision = IIF(ALLTRIM(lcDivision) = '|','',lcDivision)

IF !EMPTY(lcDivision)
  LCRPEXP = LCRPEXP + ' AND STYLE.cDivision $ lcDivision'
ENDIF

*--The Groups.
FOR lnInd = 1 TO ALEN(laRpTGrp)
  lcGroups = lcGroups + PADR(laRpTGrp[lnInd],6) + ' | '
ENDFOR
lcGroups = IIF(ALLTRIM(lcGroups) = '|','',lcGroups)

IF !EMPTY(lcGroups)
  LCRPEXP = LCRPEXP + ' AND STYLE.CSTYGROUP $ lcGroups'
ENDIF

*-- TMI B604468 [Start] Update lcRpExp with Season 
*--The season
IF !EMPTY(lcRpSeaVal)
  LCRPEXP = LCRPEXP + ' AND STYLE.SEASON = lcRpSeaVal'
ENDIF
*-- TMI B604468 [End  ]

*E301837,1 BWA 02/14/2002 Add new filter for the style status to the grid.[START]
*B605696,1 ASH 03/17/2002 (Begin) Remove the status filter from the expression to put it on color level.
*IF !EMPTY(lcRpSt1)
*  LCRPEXP = LCRPEXP + ' AND STYLE.STATUS $ lcRpSt1'
*ENDIF
*B605696,1 ASH 03/17/2002 (End)
*E301837,1 BWA 02/14/2002.[END]

*-- end of lfvCrATVar.

*:**************************************************************************
*:* Name        : lfvGetBmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/06/2001
*:* Purpose     : Selects a bmp file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGetBmp()
*:***************************************************************************
FUNCTION lfvGetBmp
*-- TMI B604468 [Start] Get Bmp from a Popup
*lcRpBmpFil = GETFILE('BMP')
*lcRpBmpFil = laBmpFils[1]
*-- TMI B604468 [End  ]
*-- end of lfvGetBmp.
