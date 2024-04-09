*:******************************************************************************
*: Program file  : ICSSLCAT.PRG    Entry  C102270
*: Program desc. : Store Special Listings
*: System        : Aria Apparel System (A27)
*: Module        : Inventory Control   (IC )
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Date          : 04/15/2001
*: NOTE          : TASK ID (Custom)102270
*: Purpose       : Printing data for a specific descriptive list ID.
*:******************************************************************************
*: Calls : 
*:    FUNCTIONS  : lfwOGWhen()
*:               : lfvDscID()
*:               : lfwDscID()
*:               : lfSRV()
*:               : lfGetClrD()
*:               : lfProper()
*:               : lfGetSizes()
*:               : lfFrmtDate()
*: --------------------------------------------------------------------------
*:    PROCEDURES : 
*:******************************************************************************
*: Passed Parameters : None
*:******************************************************************************
*:Modifications  :
*:B604437,1 TMI 05/07/2001 Put mover for division and groups and other bugs
*:B604468,1 TMI 05/09/2001 Some modifications for discriptive reports
*:B604883,1 ASH 09/06/2001 Change the print sort to be with fabric short description instead of fabric code.
*:B605149,1 BWA 11/26/2001 Add new filter for sorting by style.
*:B605149,1 BWA 12/03/2001 Sort the color field.
*:B605406,1 BWA 01/22/2002 Fix the bug of printing the report in different size of paper due to the printer setup.
*:E301837,1 BWA 02/14/2002 Add new filter for the style status to the grid.
*:B606408,1 BWA 08/22/2002 Fix the bug of printing 6 Colors to be 8 Colors [Fix in FRX]
*:******************************************************************************

*-- If no Discriptive List ID selected then abort
IF EMPTY(lcRpDscID)
  = gfDialog("I",[Descriptive List ID Can't be empty])
  RETURN
ENDIF
*-- TMI B604437 [Start] If no BMP file is selected then exit
IF EMPTY(lcRpBmpFil)
  =gfDialog('I','A BMP file must be selected.')
  RETURN
ENDIF
*-- Link the bmp file selected to the field Logo
lcBmpFile = gfTempName()
CREATE CURSOR (lcBmpFile) (Logo G)
APPEND BLANK
*-- TMI B604468,1 [Start] Add default path to the BMP
*APPEND GENERAL Logo FROM (lcRpBmpFil) LINK
APPEND GENERAL Logo FROM (gcDef_Path+'DLBMPS\'+lcRpBmpFil) LINK
*-- TMI B604468,1 [END  ]

*-- TMI B604437 [End  ]

*-- lcLF : Holds Line feed
lcLF = CHR(13) + CHR(10)
*TMI B604437 [Start] Define PLAN variable , rename COLOR_PLAN to COLOR
*PRIVATE M.CSTYMAJOR , M.FABRIC , M.SIZE , M.COLOR_PLAN , M.MDLDDESC 
*STORE '' TO M.CSTYMAJOR , M.FABRIC , M.SIZE , M.COLOR_PLAN , M.MDLDDESC 
*B604883,1 ASH 09/06/2001 (Begin) Add a new variable to hold the fabric short description.
PRIVATE M.CSTYMAJOR , M.FABRIC , M.SIZE , M.COLOR , M.PLAN , M.MDLDDESC , M.cFabShDesc
STORE '' TO M.CSTYMAJOR , M.FABRIC , M.SIZE , M.COLOR , M.PLAN , M.MDLDDESC , M.cFabShDesc
*B604883,1 ASH 09/06/2001 (End)
*TMI B604437 [End  ]

*-- Temp Cursor
*B605149,1 BWA 11/26/2001 Add the creation of the Temp name in the SYREPUVR.DBF[START]
*lcTempCur = gfTempName()
*B605149,1 BWA 11/26/2001.[END]

*-- Create cursor 
*TMI B604437 [Start] Add a new field for PLAN , rename COLOR_PLAN to COLOR
*CREATE CURSOR (lcTempCur) (CSTYMAJOR  C(19)  ,;
                           FABRIC     C(7)   ,;
                           SIZE       M      ,;
                           COLOR_PLAN M      ,;
                           MDLDDESC   M)
*B604883,1 ASH 09/06/2001 (Begin) Add the fabric short desc. to the temp file.                           

*B605406,1 BWA 01/22/2002 Add a new field "Sort" to use it in the sorting when add the record of the empty Memo.[SATRT]
*CREATE CURSOR (lcTempCur) (CSTYMAJOR  C(19)  ,;
                           FABRIC     C(7)   ,;
                           SIZE       M      ,;
                           COLOR      M      ,;
                           PLAN       M      ,;
                           cFabShDesc C(43)  ,;
                           MDLDDESC   M)

CREATE CURSOR (lcTempCur) (CSTYMAJOR  C(19)  ,;
                           FABRIC     C(7)   ,;
                           SIZE       M      ,;
                           COLOR      M      ,;
                           PLAN       M      ,;
                           cFabShDesc C(43)  ,;
                           Sort       C(1)   ,;
                           MDLDDESC   M)
*B605406,1 BWA 01/22/2002.[END]

*B604883,1 ASH 09/06/2001 (End)
*TMI B604437 [End  ]

*-- Create index                           
*B604883,1 ASH 09/06/2001 (Begin) Add a new index on the fabric short description.

*B605406,1 BWA 01/22/2002 Modify the index with the new field "SORT".[SATRT]
*INDEX ON cFabShDesc+CSTYMAJOR TAG ITEMDESC OF (lcTempCur)
INDEX ON cFabShDesc + CSTYMAJOR + SORT TAG ITEMDESC OF (lcTempCur)
*B605406,1 BWA 01/22/2002.[END]

*B605149,1 BWA 11/26/2001 Add new filter for sorting by style.[START]

*B605406,1 BWA 01/22/2002 Modify the index with the new field "SORT".[SATRT]
*INDEX ON CSTYMAJOR + cFabShDesc TAG STYLE OF (lcTempCur)
INDEX ON CSTYMAJOR + cFabShDesc + SORT TAG STYLE OF (lcTempCur)
*B605406,1 BWA 01/22/2002.[END]

*B605149,1 BWA 11/26/2001.[END]

*B604883,1 ASH 09/06/2001 (End)
INDEX ON FABRIC+CSTYMAJOR TAG ITEM_DESC OF (lcTempCur)
SET ORDER TO ITEM_DESC

*-- Save Order
lcOrdHD    = ORDER('HLDSLSHD')
lcOrdDT    = ORDER('HLDSLSDT')
lcOrdStyle = ORDER('STYLE')
lcOrdScale = ORDER('SCALE')
lcOrdObjLk = ORDER('OBJLINK')
lcOrdObjs  = ORDER('OBJECTS')
lcOrdFabr  = ORDER('FABRIC')

*-- Set Order
SET ORDER TO TAG HLDSLSHD IN HLDSLSHD
SET ORDER TO TAG HLDSLSDT IN HLDSLSDT
SET ORDER TO TAG STYLE    IN STYLE
SET ORDER TO TAG SCALE    IN SCALE
SET ORDER TO TAG OBJLNKTY IN OBJLINK
SET ORDER TO TAG OBJECTID IN OBJECTS
SET ORDER TO TAG CFABRIC  IN FABRIC

*-- Set relations
SELECT HLDSLSHD
SET RELATION TO CDLHLSTID INTO HLDSLSDT
SELECT HLDSLSDT
SET RELATION TO STYLE INTO STYLE
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE

*B605406,1 BWA 01/22/2002 Modify the width of the memo field.[SATRT]
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 60
*B605406,1 BWA 01/22/2002.[END]

*E301837,1 BWA 02/14/2002 Add new filter for the style status to the grid.[START]
IF !EMPTY(lcRpStCt)
  LCRPEXP = LCRPEXP + ' AND STYLE.STATUS $ lcRpStCt'
ENDIF
*E301837,1 BWA 02/14/2002.[END]

*-- Collecting data
SELECT HLDSLSHD
IF SEEK(lcRpDscID)
  *TMI B604437 [Start] Define array to hold plan values
  *lnTotUnt = 0
  DIME laPlan[1,2]
  lnPlanCnt = 0
  *TMI B604437 [End  ]
  *-- Loop in the child 'HLDSLSDT.DBF'
  SELECT HLDSLSDT
  SCAN REST WHILE HLDSLSDT.CDLDLSTID = M.lcRpDscID FOR &lcRpExp
    M.CSTYMAJOR  = PADR(LEFT(HLDSLSDT.STYLE,lnClrPos-2),19)
    M.FABRIC     = STYLE.FABRIC
    *B604883,1 ASH 09/06/2001 (Begin) Add the fabric short description to the temp file.
    IF SEEK(M.Fabric,'Fabric')
      M.cFabShDesc = Fabric.cFabShDesc 
    ELSE
      M.cFabShDesc = ''
    ENDIF
    *B604883,1 ASH 09/06/2001 (End)
    *TMI B604437 [Start] Assign M.SIZE,M.COLOR variables
    *M.SIZE       = ALLT(SCALE.SZ1)+lcLF+ALLT(SCALE.SZ2)+lcLF+ALLT(SCALE.SZ3)+lcLF+ALLT(SCALE.SZ4)+lcLF+ALLT(SCALE.SZ5)+lcLF+ALLT(SCALE.SZ6)+lcLF+ALLT(SCALE.SZ7)+lcLF+ALLT(SCALE.SZ8)
    *M.COLOR_PLAN = gfCodDes(SUBSTR(HLDSLSDT.STYLE,lnClrPos,lnClrLen),'COLOR') + ' ' + STR(HLDSLSDT.NDLDTLUNT,7)    
    *M.MDLDDESC   = HLDSLSDT.MDLDDESC
    *lnTotUnt     = lnTotUnt + HLDSLSDT.NDLDTLUNT
    IF ASCAN(laPlan,M.CSTYMAJOR) = 0
      *-- This condition to call lfGetSizes() only onece for this CSTYMAJOR
      M.SIZE       = lfGetSizes()    
    ENDIF    
    M.COLOR      = ALLT(gfCodDes(SUBSTR(HLDSLSDT.STYLE,lnClrPos,lnClrLen),'COLOR'))
    M.PLAN       = STR(HLDSLSDT.NDLDTLUNT,7)    
    M.MDLDDESC   = ALLT(HLDSLSDT.MDLDDESC)
    *TMI B604437 [End  ]

    SELECT (lcTempCur)
    IF SEEK(M.FABRIC+M.CSTYMAJOR)    
      *TMI B604437 [Start] Update COLOR,PLAN fileds
      *-- Update Color_Plan
      *REPLACE COLOR_PLAN WITH lcLF+M.COLOR_PLAN ADDITIVE
      REPLACE COLOR WITH lcLF+M.COLOR ADDITIVE
      REPLACE PLAN  WITH lcLF+M.PLAN  ADDITIVE
      *--sum plan values in the array laPlan
      lnPlanPos = ASCAN(laPlan,M.CSTYMAJOR)+1
      laPlan[lnPlanPos] = laPlan[lnPlanPos] + HLDSLSDT.NDLDTLUNT          
      *TMI B604437 [End  ]       
    ELSE
      *-- Add a new StyleMajor line
      INSERT INTO (lcTempCur) FROM MEMVAR
      *TMI B604437 [Start] Increase laPlan by 1 for the new added stylemajor
      lnPlanCnt = lnPlanCnt + 1
      DIME laPlan[lnPlanCnt,2]            
      laPlan[lnPlanCnt,1] = M.CSTYMAJOR
      laPlan[lnPlanCnt,2] = HLDSLSDT.NDLDTLUNT   && This Plan is for the first style in CSTYMAJOR list
      *TMI B604437 [End  ]       
    ENDIF
    
    SELECT HLDSLSDT
  ENDSCAN  
  *TMI B604437 [Start] and sum the grand total of PLAN's into lnTotUnt
  SELECT (lcTempCur)

   *B605149,1 BWA 12/03/2001 Sort the color field.[START]
   DIMENSION laSortMemo[1,2]
   SCAN
     lnMemLins = 0
     lnMemLins = MEMLINES(EVAL(lcTempCur+'.COLOR'))

     IF lnMemLins > 1
       DIMENSION laSortMemo[lnMemLins,2]
       STORE SPACE(0) TO laSortMemo
       FOR lnNum = 1 TO lnMemLins
         laSortMemo[lnNum,1] = MLINE(EVAL(lcTempCur+'.COLOR'),lnNum)
         laSortMemo[lnNum,2] = MLINE(EVAL(lcTempCur+'.PLAN'),lnNum)
       ENDFOR

       =ASORT(laSortMemo)
       FOR lnFill = 1 TO lnMemLins
         IF lnFill = 1
           REPLACE &lcTempCur..COLOR WITH laSortMemo[lnFill,1] + IIF(lnFill = lnMemLins , "" , CHR(13) + CHR(10) ) ,;
                   &lcTempCur..PLAN  WITH laSortMemo[lnFill,2] + IIF(lnFill = lnMemLins , "" , CHR(13) + CHR(10) )
         ELSE
           REPLACE &lcTempCur..COLOR WITH laSortMemo[lnFill,1] + IIF(lnFill = lnMemLins , "" , CHR(13) + CHR(10) ) ADDITIVE ,;
                   &lcTempCur..PLAN  WITH laSortMemo[lnFill,2] + IIF(lnFill = lnMemLins , "" , CHR(13) + CHR(10) ) ADDITIVE
         ENDIF
       ENDFOR
     ENDIF
   ENDSCAN
   *B605149,1 BWA 12/03/2001.[END]

  *-- lnTotUnt : Holds the grand total of plans
  lnTotUnt = 0  
  SCAN
    lnPlan = laPlan[ASCAN(laPlan,CSTYMAJOR)+1]
    lnTotUnt = lnTotUnt + lnPlan
    REPLACE PLAN WITH lcLF + '-------' + lcLF + STR(lnPlan,7) ADDITIVE
  ENDSCAN
  *TMI B604437 [End  ]  
ENDIF

*-- Set relations for (lcTempCur)
SELECT (lcTempCur)
*B604883,1 ASH 09/06/2001 (Begin) Remove this relation no need for it anymore.
*SET RELATION TO FABRIC INTO FABRIC
*B604883,1 ASH 09/06/2001 (End)
SET RELATION TO 'S'+CSTYMAJOR  INTO OBJLINK ADDITIVE
SELECT OBJLINK
SET RELATION TO COBJECT_ID INTO OBJECTS

SELECT (lcTempCur)
*B604883,1 ASH 09/06/2001 (Begin) Set orderto the new index with the fabric description.

*B605149,1 BWA 11/26/2001 Add new filter for sorting by style.[START]
*SET ORDER TO ITEMDESC
IF lcRpSortd = "F"
  SET ORDER TO ITEMDESC
ELSE
  SET ORDER TO STYLE
ENDIF
*B605149,1 BWA 11/26/2001.[END]

*B604883,1 ASH 09/06/2001 (End)

*B605406,1 BWA 01/22/2002 Code to handle printing the report in the different paper size.[SATRT]
STORE 0  TO lnRowCont , lnContLin
STORE SPACE(0) TO lcFilEmpty

*--Check the size of the printer paper.
IF PRTINFO(2) = 5
  lnLegalLen = 68
ELSE
  lnLegalLen = 50
ENDIF

SELECT (lcTempCur)
SCAN

  *-- 68 the printed rows in one page.
  STORE 0 TO lnFullLin
  FOR lnEmptyLin = 1 TO MEMLINES(EVAL(lcTempCur + '.MDLDDESC'))
    lnFullLin = lnFullLin + IIF(EMPTY(ALLTRIM(MLINE(EVAL(lcTempCur +'.MDLDDESC'),lnEmptyLin))) , 0 , 1)
  ENDFOR

  lnContLin = lnFullLin + 1

  IF lnContLin < 8
    lnContLin = 8
  ENDIF

  lnRowCont = lnRowCont +  lnContLin

  IF lnRowCont >= lnLegalLen AND lnContLin <= 8
    IF lnLegalLen - (lnRowCont - lnContLin) <= 8
      lnRowCont = lnContLin
    ENDIF 
  ENDIF
  
  IF lnRowCont <= lnLegalLen
    lnContLin = lnFullLin + 1
    IF lnContLin < 8
      lnContLin = 8
    ENDIF
  ELSE
    STORE SPACE(0) TO lcFilEmpty
    lnToPrt = lnLegalLen - (lnRowCont - lnContLin)
    IF lnToPrt > 8
      FOR lnEmpty = 1 To lnToPrt
        IF lnEmpty = lnToPrt
          lcFilEmpty = lcFilEmpty + REPLICATE(' ',10)
        ELSE
          lcFilEmpty = lcFilEmpty + REPLICATE(' ',10) + CHR(13) + CHR(10)
        ENDIF
      ENDFOR
      PRIVATE lcKeyTmp
      lnRowCont = lnContLin
      SELECT(lcTempCur)
      REPLACE &lcTempCur..Sort WITH "B"
      lcKeyTmp = EVAL(KEY())

      lcStyle = &lcTempCur..CSTYMAJOR
      APPEND BLANK
      REPLACE &lcTempCur..CSTYMAJOR WITH lcStyle    ,;
              &lcTempCur..FABRIC    WITH "BASSEM"   ,;
              &lcTempCur..MDLDDESC  WITH lcFilEmpty ,;
              &lcTempCur..Sort      WITH "A"

    =SEEK(lcKeyTmp , lcTempCur)
    ELSE
      lnRowCont = lnContLin
    ENDIF
  ENDIF
ENDSCAN

SET MEMOWIDTH TO lnOldMemW
*B605406,1 BWA 01/22/2002.[END]

LOCATE
IF RECCOUNT()=0
  =gfDialog("I","No records to display")
ELSE
  *-- Run the report
  DO gfDispRe WITH EVAL('lcRpForm')  
ENDIF

*-- Restore Old orders
SET ORDER TO &lcOrdHD    IN HLDSLSHD
SET ORDER TO &lcOrdDT    IN HLDSLSDT
SET ORDER TO &lcOrdStyle IN STYLE
SET ORDER TO &lcOrdScale IN SCALE
SET ORDER TO &lcOrdObjLk IN OBJLINK
SET ORDER TO &lcOrdObjs  IN OBJECTS
SET ORDER TO &lcOrdFabr  IN FABRIC

*:**************************************************************************
*:* Name        : lfwOGWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2001
*:* Purpose     : Init settings of OG
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lfwOGWhen()
*:***************************************************************************
FUNCTION lfwOGWhen

STOR 0 TO lnClrPos,lnClrLen
*-- Get color position and color length
=lfGetClrD()

*-- TMI B604468,1 [Start] Update the bmp var if it is the default case
IF lnOgSeting = 1
 lcRpBmpFil = laBmpFils[1,1]
 = lfOGShowGet('lcRpBmpFil')
ENDIF
*-- TMI B604468,1 [End  ]    
*-- end of lfwOGWhen.

*:**************************************************************************
*:* Name        : lfvDscID
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2001
*:* Purpose     : Valid function for the Descriptive List ID variable (lcRpDscID)
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lfvDscID()
*:***************************************************************************
FUNCTION lfvDscID
DECL laTempData[1,1]
laTempData[1,1] = ''
SELECT HLDSLSHD
SET ORDER TO HLDSLSHD
IF !EMPTY(lcRpDscID)
  IF SEEK(lcRpDscID)
    *-- Nothing to do
  ELSE
    *-- Call Browser
    lcFile_Ttl  = "Descriptive List ID's"
    lcBrFields  = [CDLHLSTID :R :H = 'Discriptive List ID'       ,]+; 
                  [SEASON    :R :H = 'Season'                    ,]+;
                  [CDLHSSDSC :R :H = 'Season Description'        ,]+;
                  [CDLHSLDSC :R :H = 'Season long description 1' ,]+;
                  [CDLHS2DSC :R :H = 'Season long description 2' ,]+;
                  [DDLHDLVDT :R :H = 'Delivery Date'             ,]+;
                  [DDLHRVSDT :R :H = 'Date of last revision'      ]
    =AriaBrow('',lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CDLHLSTID','laTempData')
    lcRpDscID  = laTempData[1,1]
  ENDIF
ENDIF

*-- Check if OldValue and NewValue are not empty
IF ( EMPTY(lcOldDscId) AND !EMPTY(lcRpDscID)) OR ;
   (!EMPTY(lcOldDscId) AND  EMPTY(lcRpDscID))
  CLEAR READ
ENDIF
*-- end of lfvDscID.

*:**************************************************************************
*:* Name        : lfwDscID
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2001
*:* Purpose     : Captures the old value of lcRpDscID
*:***************************************************************************
*:* Called from : WHEN function of lcRpDscID
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lfwDscID()
*:***************************************************************************
FUNCTION lfwDscID
lcOldDscId = lcRpDscID
*-- end of lfvwDscID

*:**************************************************************************
*:* Name        : lfSRV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2001
*:* Purpose     : Set Reset Valid Function for Style-InRange
*:***************************************************************************
*:* Called from : Style-InRange
*:***************************************************************************
*:* Passed Parameters : lcParm
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lfSRV('S')
*:***************************************************************************
FUNCTION lfSRV   
PARAMETERS lcParm

SELECT STYLE
IF lcParm = "S"   
  lcStyOrder = ORDER("STYLE")
  lcHLDOrder = ORDER("HLDSLSDT")
  SET ORDER TO Cstyle IN STYLE
  SET ORDER TO STYLE IN HLDSLSDT  
  SET RELATION TO LEFT(STYLE,lnClrPos-2) INTO HLDSLSDT    
  IF !EMPTY(lcRpDscID)
    SELECT HLDSLSDT
    SET FILTER TO  HLDSLSDT.CDLDLSTID = lcRpDscID
    LOCATE
  *-- TMI B604437 [Start] Comment these lines , instead this code is added to SYREPUVR.DBF    
  * SELECT STYLE
  * SET FILTER TO !EOF('HLDSLSDT')
  * LOCATE
  *-- TMI B604437 [End  ]   
  ENDIF

ELSE  && ELSE Rest case
  SELECT STYLE
  SET FILTER TO
  SET RELATION OFF INTO HLDSLSDT
  LOCATE
  IF !EMPTY(lcStyOrder)
    SET ORDER TO &lcStyOrder IN STYLE
  ENDIF  

  SELECT HLDSLSDT
  SET FILTER TO
  LOCATE
  IF !EMPTY(lcHLDOrder)
    SET ORDER TO &lcHLDOrder IN HLDSLSDT
  ENDIF      
  
  SELECT STYLE
ENDIF
*-- end of lfSRV.

*:**************************************************************************
*:* Name        : lfGetClrD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2001
*:* Purpose     : Set the color position & color length in style segment
*:***************************************************************************
*:* Called from : lfwOGWhen
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lfGetClrD()
*:***************************************************************************
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

*:**************************************************************************
*:* Name        : lfProper
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2001
*:* Purpose     : Proper the string as whole
*:***************************************************************************
*:* Called from : ICSSLCAT.FRX
*:***************************************************************************
*:* Passed Parameters : lcString
*:***************************************************************************
*:* Return      : Proper string
*:***************************************************************************
*:* Example     : lcString('this is a season') ==> 'This is a season'
*:***************************************************************************
FUNCTION lfProper
PARAMETER lcString
PRIVATE lcToRet 
lcToRet = ''
lcToRet  = ALLT(lcString)
lcToRet  = UPPER(LEFT(lcToRet,1)) + LOWER(SUBSTR(lcToRet,2))
RETURN lcToRet 
*-- end of lfProper.

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
*:* Purpose     : To format date as "April 30,2001"
*:***************************************************************************
*:* Called from : frx
*:***************************************************************************
*:* Passed Parameters : date to format
*:***************************************************************************
*:* Return      : formated date
*:***************************************************************************
*:* Example     :  = lfFrmtDate({4/30/2001})
*:***************************************************************************
FUNCTION lfFrmtDate
PARAMETERS ldThisDate
*-- TMI B604468,1 [Start] IF NO ldThisDate PASSED RETUTN SPACE(0)
*RETURN CMONTH(ldThisDate)+' '+PADL(LTRIM(STR(DAY(ldThisDate),2)),2,'0')+','+STR(YEAR(ldThisDate),4)
RETURN IIF(EMPTY(ldThisDate),'',CMONTH(ldThisDate)+' '+PADL(LTRIM(STR(DAY(ldThisDate),2)),2,'0')+','+STR(YEAR(ldThisDate),4))
*-- TMI B604468,1 [End  ]
*-- end of lfFrmtDate.
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
lcRpBmpFil = GETFILE('BMP')
*-- end of lfvGetBmp.


*:**************************************************************************
*:* Name        : lfFillCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/08/2001
*:* Purpose     : Fill the bmps files array
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFillCode()
*:***************************************************************************
FUNCTION lfFillCode

*-- TMI B604468,1 [Start] Fill laBmpFils Array
DIME laDummy[1,1]
DIME laBmpFils[1,1]
STORE '' TO laDummy,laBmpFils
=ADIR(laDummy,gcDef_Path+'DLBMPS\*.BMP')
DIME laBmpFils[ALEN(laDummy,1),1]
FOR I = 1 TO ALEN(laDummy,1) 
  laBmpFils[I] = laDummy[I,1]
NEXT
=ASORT(laBmpFils)
*-- TMI B604468,1 [End  ]

*-- end of lfFillCode.