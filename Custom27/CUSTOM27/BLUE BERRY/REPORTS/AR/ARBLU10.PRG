*:****************************************************************************
*: Program file        : ARBLU10   && for customer Blue Berry
*: Program desc.       : Year to date sales report
*: For Report          : ARBLU10A.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Account Receivable - (AR)
*: Developer           : RAMY MABROUK (RAM)
*: Tracking Job Number : C101722,1
*: Date                : Jan/2000
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfModalGen()
*:                 gfDispRe()
*:                 gfItemMask()
*:****************************************************************************
*: Called From         : System Menu (AR --> Output --> Year to Date Sales)
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO ARBLU10
*:****************************************************************************
*: Modification  :
*: B803333,1 NAD 06/11/2000 Fix bug the quantites of each style and color is not correct, 
*: B803333,1                and the size scale represented is inconsistent.
*: B803583,1 BWA 08/14/2000 Fix the bug of the total pieces for a customer different from  this report
*: B803583,1                and the sales journal for the same customer.[WAM]
*: B804255,1 BWA 07/04/2001 1) Fix the bug of 'Variable 'ICTMP' not found.
*: B804255,1                2) A total line quantity to be printed at the end of the size scale before the Ext. Prc field.[FRX]
*: B804255,1                3) The report lists all the styles without grouping in case of printing by account.
*:****************************************************************************

WAIT WINDOW 'Collecting Data' NOWAIT

*-- Creat the Temp cursor and define some variables [start]

*B803583,1 Add new field for the total value in the temp. file.[START]
*CREATE CURSOR (lclines)  (Invoice C(6), Account C(5)  , STYLE C(19)    , Scale C(3)    ,;
  SZ1 C(5)     , SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)      ,;
  SZ5 C(5)     , SZ6 C(5)      , SZ7 C(5)      ,;
  SZ8 C(5)     , SZ9 C(5)      , SZ10 C(5)     , SZ11 C(5)     ,;
  SZ12 C(5)    , SZ13 C(5)     , SZ14 C(5)     , Qty1 N(6)     ,;
  Qty2 N(6)    , Qty3 N(6)     , Qty4 N(6)     ,;
  Qty5 N(6)    , Qty6 N(6)     , Qty7 N(6)     , Qty8 N(6)     ,;
  Qty9 N(6)    , Qty10 N(6)    , Qty11 N(6)    , Qty12 N(6)    ,;
  Qty13 N(6)   , Qty14 N(6)    , Price1 N(9,2) , Price2 N(9,2) ,;
  Price3 N(9,2), Price4 N(9,2) , Price5 N(9,2) , Price6 N(9,2) ,;
  Price7 N(9,2), Price8 N(9,2) , Price9 N(9,2) , Price10 N(9,2),;
  Price11 N(9,2),Price12 N(9,2), Price13 N(9,2), Price14 N(9,2),;
  STORE C(8)    , DESC C(20))

*B804255,1 BWA 07/04/2001 Add a new field to check in case the customer+store is change.[START]
*CREATE CURSOR (lclines)  (Invoice C(6), Account C(5)  , STYLE C(19)    , Scale C(3)    ,;
  SZ1 C(5)     , SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)      ,;
  SZ5 C(5)     , SZ6 C(5)      , SZ7 C(5)      ,;
  SZ8 C(5)     , SZ9 C(5)      , SZ10 C(5)     , SZ11 C(5)     ,;
  SZ12 C(5)    , SZ13 C(5)     , SZ14 C(5)     , Qty1 N(6)     ,;
  Qty2 N(6)    , Qty3 N(6)     , Qty4 N(6)     ,;
  Qty5 N(6)    , Qty6 N(6)     , Qty7 N(6)     , Qty8 N(6)     ,;
  Qty9 N(6)    , Qty10 N(6)    , Qty11 N(6)    , Qty12 N(6)    ,;
  Qty13 N(6)   , Qty14 N(6)    , Price1 N(9,2) , Price2 N(9,2) ,;
  Price3 N(9,2), Price4 N(9,2) , Price5 N(9,2) , Price6 N(9,2) ,;
  Price7 N(9,2), Price8 N(9,2) , Price9 N(9,2) , Price10 N(9,2),;
  Price11 N(9,2),Price12 N(9,2), Price13 N(9,2), Price14 N(9,2),;
  STORE C(8)    , DESC C(20) , TotVal N(13,2))

CREATE CURSOR (lclines)  (Invoice C(6), Account C(5)  , STYLE C(19)    , Scale C(3)    ,;
  SZ1 C(5)     , SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)      ,;
  SZ5 C(5)     , SZ6 C(5)      , SZ7 C(5)      ,;
  SZ8 C(5)     , SZ9 C(5)      , SZ10 C(5)     , SZ11 C(5)     ,;
  SZ12 C(5)    , SZ13 C(5)     , SZ14 C(5)     , Qty1 N(6)     ,;
  Qty2 N(6)    , Qty3 N(6)     , Qty4 N(6)     ,;
  Qty5 N(6)    , Qty6 N(6)     , Qty7 N(6)     , Qty8 N(6)     ,;
  Qty9 N(6)    , Qty10 N(6)    , Qty11 N(6)    , Qty12 N(6)    ,;
  Qty13 N(6)   , Qty14 N(6)    , Price1 N(9,2) , Price2 N(9,2) ,;
  Price3 N(9,2), Price4 N(9,2) , Price5 N(9,2) , Price6 N(9,2) ,;
  Price7 N(9,2), Price8 N(9,2) , Price9 N(9,2) , Price10 N(9,2),;
  Price11 N(9,2),Price12 N(9,2), Price13 N(9,2), Price14 N(9,2),;
  STORE C(8)    , DESC C(20) , TotVal N(13,2) , llPrntBr L(1) )
*B804255,1 BWA 07/04/2001 [END]

*B803583,1 [END]

 INDEX ON ACCOUNT + LEFT(STYLE , lnFLen + lnCLen + 1) + STORE TAG ACSTTYLE OF (gcWorkDir+lclines)


lcInv    =  ''
lcNewMaj  = ''
lcNewScal = ''
lcAccount = ''
lcStore   = ''
lnTot     = 0

*-- end

*-- Set Some Relation

SELECT INVLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO INVOICE INTO INVHDR ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE

*-- end


*-- Start the data collection
*B803333,1  (Start) Seek in the style without scale
lcExp=STRTRAN(lcRpExp,'INVLINE.STYLE','LEFT(INVLINE.STYLE , lnFLen + lnCLen + 1)',2)
*lcScanExp = IIF(UPPER(lcRpExp)==".T.", "", "FOR " + lcRpExp)

*B803583,1 Add to the filter the condition not to take the void invoice.[START]
lcExp =  lcExp + IIF(EMPTY(lcExp) , "" , " AND " ) + " INVHDR.Status <> 'V' " 
*B803583,1 [END]

lcScanExp = IIF(UPPER(lcExp)==".T.", "", "FOR " + lcExp)
*B803333,1  (End)
SELECT INVLINE
SCAN &lcScanExp                      && Scan for the selected criteria
  *-- check the current major if changed or not
  IF (lcNewMaj + lcAccount + lcStore) # LEFT(INVLINE.STYLE , lnFLen + lnCLen + 1) + ACCOUNT + STORE
    DIMENSION laSizes[14,3]
    STORE SPACE(0) TO laSizes
    lnSzCnt   = 1
    lcNewMaj  = LEFT(INVLINE.STYLE , lnFLen + lnCLen + 1)
    lcNewScal = SCALE.Scale
    lcAccount = INVLINE.ACCOUNT
    lcStore   = INVLINE.STORE
    lcStyDesc = STYLE.Desc
    lcInv  = INVLINE.INVOICE
    SELECT SCALE
    *B803333,1 (Start) to be sure that the pointer is at the first scale.
    SEEK 'S'+LEFT(SCALE.Scale,lnSLen)
    *B803333,1 (End)
    lnCnt = 1
    *-- scan the scale file to get all sizes for the selected style->scale
    SCAN WHILE LEFT(SCALE.Scale,lnSLen) = LEFT(STYLE.Scale,lnSLen)
      FOR lnC = 1 TO 8
        lnC = STR(lnC,1)
        IF !EMPTY(SCALE.SZ&lnC)
          laSizes[lnCnt,1] = SCALE.SZ&lnC
        ELSE
          EXIT
        ENDIF
        lnC = VAL(lnC)
        lnCnt = lnCnt + 1
      ENDFOR
    ENDSCAN
    SELECT INVLINE
    GO RECNO()
    SELECT STYLE
    GO RECNO()
    FOR lnC = 1 TO SCALE.CNT && Fill array with the all sizes for the current scale if any
      lnC = STR(lnC,1)
      lnPos = ASCAN(laSizes,SCALE.sz&lnC)
      IF lnPos > 0
        IF EMPTY(laSizes[lnPos+1])
          laSizes[lnPos+1] = 0
        ENDIF
        lnQty = INVLINE.QTY&lnC
        laSizes[lnPos+1] = laSizes[lnPos+1] + lnQty
        IF !EMPTY(laSizes[lnPos+1])
          laSizes[lnPos+2] = INVLINE.Price
        ENDIF
      ENDIF
      lnC = VAL(lnC)
    ENDFOR

  ELSE && if not changed the major
    *B803583,1 Empty the array from the old quantity.[START]
    FOR lnPos2 = 1 To 14
      laSizes[lnPos2,2] = 0
    ENDFOR
    *B803583,1 [END]

    FOR lnC = 1 TO SCALE.CNT && Fill array with the all sizes for the current scale if any
      lnC = STR(lnC,1)
      lnPos = ASCAN(laSizes,SCALE.SZ&lnC)
      
      IF lnPos = 0
        laSizes[lnSzCnt,1] = SCALE.SZ&lnC
        laSizes[lnSzCnt,2] = INVLINE.QTY&lnC
        laSizes[lnSzCnt,3] = INVLINE.Price
      ELSE
        lnQty = INVLINE.QTY&lnC
        IF EMPTY(laSizes[lnPos+1])
          laSizes[lnPos+1] = 0
        ENDIF
        
        laSizes[lnPos+1] = laSizes[lnPos+1] + lnQty
        *--new
        laSizes[lnPos+2] = INVLINE.Price
      ENDIF
      lnSzCnt = lnSzCnt + 1
      lnC = VAL(lnC)
    ENDFOR
  ENDIF
*****

  lcAlias = SELECT(0)
  FOR lnCnt = 1 TO 14 && check the array for the empty elements to fill with 0
    IF EMPTY(laSizes[lnCnt,2])
      laSizes[lnCnt,2] = 0
    ENDIF
    IF EMPTY(laSizes[lnCnt,3])
      laSizes[lnCnt,3] = 0
    ENDIF
  ENDFOR
  SELECT (lcLines)
  *-- if we have that major before
  IF SEEK(lcAccount+lcNewmaj+lcStore , lcLines)
  *IF SEEK(lcAccount+lcNewmaj , lcLines)

      *Qty2     WITH laSizes[2,2] + Qty2 ,;
      *Qty3     WITH laSizes[3,2] + Qty3 ,;
      *Qty4     WITH laSizes[4,2] + Qty4 ,;
      *Qty5     WITH laSizes[5,2] + Qty5 ,;
      *Qty6     WITH laSizes[6,2] + Qty6 ,;
      *Qty7     WITH laSizes[7,2] + Qty7 ,;
      *Qty8     WITH laSizes[8,2] + Qty8 ,;
      *Qty9     WITH laSizes[9,2] + Qty9 ,;
      *Qty10    WITH laSizes[10,2] + Qty10 ,;
      *Qty11    WITH laSizes[11,2] + Qty11 ,;
      *Qty12    WITH laSizes[12,2] + Qty12 ,;
      *Qty13    WITH laSizes[13,2] + Qty13 ,;
      *Qty14    WITH laSizes[14,2] + Qty14 ,;
      
      *B803583,1 Add the quantity to the fields not replace them.[START]
      *REPLACE Qty1     WITH laSizes[1,2] ,;
      	 	   Qty2     WITH laSizes[2,2] ,;
      		   Qty3     WITH laSizes[3,2] ,;
      		   Qty4     WITH laSizes[4,2] ,;
      		   Qty5     WITH laSizes[5,2] ,;
      		   Qty6     WITH laSizes[6,2] ,;
      		   Qty7     WITH laSizes[7,2] ,;
      		   Qty8     WITH laSizes[8,2] ,;
      		   Qty9     WITH laSizes[9,2] ,;
      		   Qty10    WITH laSizes[10,2] ,;
      		   Qty11    WITH laSizes[11,2] ,;
      		   Qty12    WITH laSizes[12,2] ,;
      		   Qty13    WITH laSizes[13,2] ,;
      		   Qty14    WITH laSizes[14,2] ,;
      		   Sz1      WITH laSizes[1,1] ,;
      		   Sz2      WITH laSizes[2,1] ,;
      		   Sz3      WITH laSizes[3,1] ,;
      		   Sz4      WITH laSizes[4,1] ,;
      		   Sz5      WITH laSizes[5,1] ,;
      		   Sz6      WITH laSizes[6,1] ,;
      		   Sz7      WITH laSizes[7,1] ,;
      		   Sz8      WITH laSizes[8,1] ,;
      		   Sz9      WITH laSizes[9,1] ,;
      		   Sz10     WITH laSizes[10,1] ,;
      		   Sz11     WITH laSizes[11,1] ,;
      		   Sz12     WITH laSizes[12,1] ,;
      		   Sz13     WITH laSizes[13,1] ,;
      		   Sz14     WITH laSizes[14,1]

    REPLACE Qty1     WITH laSizes[1,2]  + Qty1  ,;
      		Qty2     WITH laSizes[2,2]  + Qty2  ,;
            Qty3     WITH laSizes[3,2]  + Qty3  ,;
      		Qty4     WITH laSizes[4,2]  + Qty4  ,;
      		Qty5     WITH laSizes[5,2]  + Qty5  ,;
            Qty6     WITH laSizes[6,2]  + Qty6  ,;
      		Qty7     WITH laSizes[7,2]  + Qty7  ,;
      		Qty8     WITH laSizes[8,2]  + Qty8  ,;
      		Qty9     WITH laSizes[9,2]  + Qty9  ,;
      		Qty10    WITH laSizes[10,2] + Qty10 ,;
      		Qty11    WITH laSizes[11,2] + Qty11 ,;
      		Qty12    WITH laSizes[12,2] + Qty12 ,;
      		Qty13    WITH laSizes[13,2] + Qty13 ,;
      		Qty14    WITH laSizes[14,2] + Qty14 ,;
      		Sz1      WITH laSizes[1,1]          ,;
      		Sz2      WITH laSizes[2,1]  		,;
      		Sz3      WITH laSizes[3,1] 			,;
      		Sz4      WITH laSizes[4,1] 			,;
      		Sz5      WITH laSizes[5,1] 			,;
      		Sz6      WITH laSizes[6,1] 			,;
      		Sz7      WITH laSizes[7,1] 			,;
      		Sz8      WITH laSizes[8,1] 			,;
      		Sz9      WITH laSizes[9,1] 			,;
      		Sz10     WITH laSizes[10,1] 		,;
      		Sz11     WITH laSizes[11,1] 		,;
      		Sz12     WITH laSizes[12,1] 		,;
      		Sz13     WITH laSizes[13,1] 		,;
      		Sz14     WITH laSizes[14,1]
    *B803583,1 [END]

    REPLACE Price1     WITH laSizes[1,3] ,;
      Price2     WITH laSizes[2,3] ,;
      Price3     WITH laSizes[3,3] ,;
      Price4     WITH laSizes[4,3] ,;
      Price5     WITH laSizes[5,3] ,;
      Price6     WITH laSizes[6,3] ,;
      Price7     WITH laSizes[7,3] ,;
      Price8     WITH laSizes[8,3] ,;
      Price9     WITH laSizes[9,3] ,;
      Price10    WITH laSizes[10,3] ,;
      Price11    WITH laSizes[11,3] ,;
      Price12    WITH laSizes[12,3] ,;
      Price13    WITH laSizes[13,3] ,;
      Price14    WITH laSizes[14,3]

    *B803583,1 Accumulate the value to the total value in the temp. file.[START]
    REPLACE Totval WITH Totval + lfTotval()
    *B803583,1 [END]
      
  ELSE && else it is the first time

    *B803583,1 Add the value to the total value in the temp. file.[START]
    *INSERT INTO (lcLines) (INVOICE , STYLE , Account , STORE , DESC , Scale) VALUES (lcInv , lcNewMaj , lcAccount , lcStore , lcStyDesc , lcNewScal)
    *GATHER FROM laSizes FIELDS SZ1 , Qty1 , Price1 , SZ2 , Qty2 , Price2 , SZ3 , Qty3 , Price3 , SZ4 , Qty4 , Price4 ,;
      SZ5 , Qty5 , Price5 , SZ6 , Qty6 , Price6 , SZ7 , Qty7 , Price7 , SZ8 , Qty8 , Price8 ,;
      SZ9 , Qty9 , Price9 , SZ10, Qty10, Price10, SZ11, Qty11, Price11, SZ12, Qty12, Price12,;
      SZ13, Qty13, Price13, SZ14, Qty14, Price14

    INSERT INTO (lcLines) (INVOICE , STYLE , Account , STORE , DESC , Scale,Totval) VALUES (lcInv , lcNewMaj , lcAccount , lcStore , lcStyDesc , lcNewScal,lfTotval())
    GATHER FROM laSizes FIELDS SZ1 , Qty1 , Price1 , SZ2 , Qty2 , Price2 , SZ3 , Qty3 , Price3 , SZ4 , Qty4 , Price4 ,;
      SZ5 , Qty5 , Price5 , SZ6 , Qty6 , Price6 , SZ7 , Qty7 , Price7 , SZ8 , Qty8 , Price8 ,;
      SZ9 , Qty9 , Price9 , SZ10, Qty10, Price10, SZ11, Qty11, Price11, SZ12, Qty12, Price12,;
      SZ13, Qty13, Price13, SZ14, Qty14, Price14
    *B803583,1 [END]

  ENDIF
  SELECT (lcAlias)
ENDSCAN

*-- if we didn find any records math our criteria
IF EOF(lcLines)
  SELECT (lcLines)
  USE
  = gfModalGen("INM00052B00000" , "DIALOG")
  RETURN
ENDIF


SELECT (lclines)
*-- select the index and fill the groups variables depends on the sort by option
IF lcSortBy = 'S'
  INDEX ON STYLE + ACCOUNT + STORE TAG STYLE OF (gcWorkDir+lclines)
  lcGroup2 = 'SUBSTR(STYLE , lnFLen + 2 , lnCLen)'
  lcGroup1 = 'LEFT(STYLE , lnFLen)'
ELSE
  INDEX ON ACCOUNT + STORE +STYLE TAG ACSTTYLE OF (gcWorkDir+lclines)
  lcGroup1 = 'Account'

  *B804255,1 BWA 07/04/2001 Printed by account it lists all the styles without grouping.[START]
  *lcGroup2 = "Account + '/' + Store"
  lcGroup2 = "Account + '/' + Store" + " + LEFT(STYLE,lnFLen) " 

  STORE SPACE(6) TO lcAccBr
  STORE SPACE(8) TO lcStorBr
  SCAN
    IF (lcAccBr + lcStorBr) # EVAL(lclines+'.ACCOUNT') + EVAL(lclines+'.STORE')
      REPLACE llPrntBr WITH .T.
      lcAccBr  = EVAL(lclines+'.ACCOUNT')
      lcStorBr = EVAL(lclines+'.STORE')
    ENDIF
  ENDSCAN
  *B804255,1 BWA 07/04/2001 [END]

ENDIF
*-- Call the .FRX

DO gfDispRe WITH EVALUATE('lcRepNam')

**-- Clear the temp cursor
*SELECT (lclines)
*USE

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : RAMY MABROUK (RAM)
*! Date      : 2/06/2000
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : ARBLU10.PRG
*!*************************************************************
*! Calls       : gfItemMask()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg

STORE 0 TO lnCPos , lnClen , lnFPos , lnFlen

*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it,
  *-- display the style major instead (style major will browse from the
  *-- style file directly)

  DO CASE
  CASE laMajSeg[lnC,1] = 'F'
    *-- If there are more than one "FREE" segment , get first one only
    lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
    lnFlen = LEN(laMajSeg[lnC,3])
    IF EMPTY(laMajSeg[lnC,5])
      laMajSeg[lnC,5] = 'Style'
    ENDIF

  CASE laMajSeg[lnC,1] = 'C'
    lnCPos = lnC
    lnClen = LEN(laMajSeg[lnC,3])
    IF EMPTY(laMajSeg[lnC,5])
      laMajSeg[lnC,5] = 'Color'
    ENDIF

  ENDCASE
ENDFOR



**********************************************
FUNCTION lfGetTot

lnTot = 0
FOR I = 1 TO 14
   I = ALLTRIM(STR(i))
  lnTot = lnTot + (price&I * qty&I)
  I = VAL(I)
ENDFOR

RETURN lnTot

*!*************************************************************
*! Name      : lfStyClr
*! Developer : Nader Anis (Nad)
*! Date      : 2/06/2000
*! Purpose   : Set and reset function for the style
*! Ref       : B803333,1
*!*************************************************************
*! Called from : ARBLU10.PRG
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStyClr('S')
*!*************************************************************

FUNCTION lfStyClr
PARAMETERS lcDum

DO CASE 
  CASE lcDum='S'
    IF EMPTY(lcTmp)   
      lcTmp =gfTempName()
      SELECT DISTINCT LEFT(Style.STYLE , lnFLen + lnCLen + 1) AS Style ;
             FROM     Style INTO DBF (gcWorkDir + lcTmp) 
    ENDIF
   
    IF USED('Style')
      USE IN Style
    ENDIF 
    IF USED(lcTmp) 
      USE IN (lcTmp)
    ENDIF  
    USE (gcWorkDir + lcTmp) IN 0 ALIAS Style EXCL
    SELECT Style
    INDEX ON Style TAG Style    
    GO TOP
  CASE lcDum='R'
    USE IN STYLE
    =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
ENDCASE   

*!*************************************************************
*! Name      : lfClearREp
*! Developer : Nader Anis (Nad)
*! Date      : 06/11/2000
*! Purpose   : To delete the temp file
*! Ref       : B803333,1
*!*************************************************************
*! Called from : ARBLU10.PRG
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearREp('S')
*!*************************************************************
FUNCTION lfClearREp

*B804255,1 BWA 07/04/2001 1) Fix the bug of 'Variable 'ICTMP' not found.[START]
*ERASE  (gcWorkDir + lcTmp)
IF USED(lcTmp)
  USE IN (lcTmp)
  ERASE (gcWorkDir + lcTmp +'.DBF')
  ERASE (gcWorkDir + lcTmp +'.CDX')
ENDIF
*B804255,1 BWA 07/04/2001 [END]

*--End of lfClearREp.
*B803583,1 Add new field for the total value in the temp. file.[START]
*!*************************************************************
*! Name      : lfTotval
*! Developer : BASSEM RAFAAT(BWA)
*! Date      : 08/14/2000
*! Purpose   : To COMPUTE THE TOTAL VALUE
*! Ref       : B803583,1
*!*************************************************************
*! Called from : ARBLU10.PRG
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lnTotval
*!*************************************************************
*! Example     : =llfTotval()
*!*************************************************************
FUNCTION lfTotval
lnTotval = 0

FOR I = 1 TO 14
  lnTotval = lnTotval + (laSizes[I,2] * laSizes[I,3] )
ENDFOR
RETURN lnTotval
*B803583,1 [END]