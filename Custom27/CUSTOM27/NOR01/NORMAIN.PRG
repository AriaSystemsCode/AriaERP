*!**************************************************************************
*: Name      : NORMAIN.PRG
*: Developer : TMI - TAREK MOHAMMED IBRAHIM
*: Date      : 11/17/2004
*: Purpose   : NOR01 -NORTH BAY APPAREL, LTD - Custom Process Program .
*:**************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:**************************************************************************
*C124051,1 TMI 11/24/2004 Release styles not completly allocated
*:**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfWHENRET
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2004
*:* Purpose     : Return false for the when function "lfwOldVal" for NOR01 
*                 except when called from option grid
*:***************************************************************************
*C124051,1
FUNCTION lfWHENRET
PRIVATE lnI,llOpGrid,lcProg
llOpGrid = .F.
lnI = 0
DO WHILE .T.
  lnI = lnI + 1
  IF PROG(lnI) = 'GRIDHOST'
    llOpGrid = .T.
    EXIT
  ENDIF
  IF EMPTY(PROG(lnI))
    EXIT
  ENDIF
ENDDO
RETURN llOpGrid
*-- end of lfWHENRET.

*:**************************************************************************
*:* Name        : lfRELALO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/23/2004
*:* Purpose     : Release styles not completly allocated
*:***************************************************************************
*C124051,1
FUNCTION lfRELALO
PRIVATE lnRec,lcAcptSty,lnK,lcK,llShrt,lnShortage,lcSvOrd,lcCustOrd;
        lcOrd,lcSty,lcAccount,lcWH,laOrd

lnRec = RECNO(lcTmpOrdLn)
lcCustOrd = ORDER('CUSTOMER')

lcAcptSty = '_'+SUBSTR(lcAutAlCh1,2)
lcLogFile = '_'+SUBSTR(lcAutAlCh3,2)  

SET ORDER TO TAG STYDYE IN STYDYE
SET ORDER TO TAG STYLE IN STYLE
SET ORDER TO TAG ORDLINE IN ORDLINE
SET ORDER TO TAG ORDHDR IN ORDHDR
SET ORDER TO CUSTOMER IN CUSTOMER

SELECT (lcTmpOrdLn)
SET ORDER TO TAG &lcTmpOrdLn
SET RELATION TO

*- Create Log file 
CREATE TABLE (gcWorkDir+lcLogFile) (ORDER C(6),ACCOUNT C(6),BTNAME C(30),STYLE C(19),;
                                    COLOR C(6),SHRTQTY N(8))
INDEX ON ORDER+STYLE TAG &lcLogFile

*- Get styles with at least one line is picked after picking process
SELECT DIST &lcTmpOrdLn..ORDER, ;
            SUBSTR(&lcTmpOrdLn..STYLE,1,lnMajorLen) AS STYLE, ;
            &lcTmpOrdLn..CWARECODE,.F. AS LRELEASED;
  FROM &lcTmpOrdLn ;
  WHERE !EMPTY(PIKTKT) ;
  INTO TABLE &gcWorkDir.&lcAcptSty
INDEX ON ORDER+STYLE TAG &lcAcptSty

*-Release partially allocated styles ( some colors only are allocated )
SELECT &lcAcptSty
SCAN
  SELECT &lcTmpOrdLn
  GO TOP
  LOCATE FOR ORDER = &lcAcptSty..ORDER .AND. ;
             STYLE = &lcAcptSty..STYLE .AND. ;
             EMPTY(PIKTKT)
  IF FOUND()
    GO TOP
    LOCATE FOR ORDER     = &lcAcptSty..ORDER .AND. ;
               STYLE     = &lcAcptSty..STYLE .AND. ;
               CWARECODE = &lcAcptSty..CWARECODE 
    SCAN REST FOR ORDER  = &lcAcptSty..ORDER .AND. ;
               STYLE     = &lcAcptSty..STYLE .AND. ;
               CWARECODE = &lcAcptSty..CWARECODE 
      REPLACE nProcNo WITH 0
      IF llSel .AND. TotPik > 0
        lcRelStyle = Style          && Variable to hold the Style
        =lfRelQty()
        lnSelAlo = lnSelAlo - 1
        lnAloRec = lnAloRec - 1
      ENDIF
      SELECT &lcAcptSty
      REPLACE LRELEASED WITH .T.
    ENDSCAN    && End of SCAN Loop
  ENDIF
ENDSCAN

*- Collect shortage quantities
lcWH = 'PADR( IIF( EMPTY(lcRpPkFWrh) , &lcTmpOrdln..CWARECODE , lcRpPkFWrh ) , 6)'
DIMENSION laOrd[8]

SELECT &lcTmpOrdLn
lcSvOrd = ORDER()
INDEX ON ORDER+STYLE TO &gcWorkDir.&lcLogFile

SELECT &lcTmpOrdLn
DO WHILE !EOF(lcTmpOrdLn)
  STORE 0 TO lnShortage,laOrd
  
  SELECT &lcTmpOrdLn
  =SEEK(&lcTmpOrdLn..STYLE+&lcWH,'STYDYE')
  lcAccount = ACCOUNT
  lcOrd     = ORDER
  lcSty     = STYLE  
  SCAN REST WHILE ORDER+STYLE = lcOrd+lcSty
    =SEEK('S' + Scale , 'SCALE')
    FOR lnK = 1 TO SCALE.CNT
      lcK = STR(lnK,1)
      laOrd[lnK] = laOrd[lnK] + &lcTmpOrdLn..QTY&lcK
    ENDFOR
  ENDSCAN
  
  FOR lnK = 1 TO SCALE.CNT
    lcK = STR(lnK,1)
    llShrt = laOrd[lnK] > STYDYE.STK&lcK-STYDYE.ALO&lcK
    lnShortage = lnShortage + IIF(llShrt, laOrd[lnK] - (STYDYE.STK&lcK-STYDYE.ALO&lcK) , 0 )
  ENDFOR
  
  *- if the style is not picked at all , then it is not in the file "lcAcptSty"
  *- this check to ensure that for styles that are selected completly that there is no shortages
  IF SEEK(lcOrd+SUBSTR(lcSty,1,lnMajorLen),lcAcptSty) .AND. !&lcAcptSty..LRELEASED
    lnShortage = 0
  ENDIF
  IF lnShortage > 0 
    SELECT &lcLogFile
    IF !SEEK(lcOrd+SUBSTR(lcSty,1,lnMajorLen),lcLogFile)
      =SEEK('M'+lcAccount,'CUSTOMER')
      APPEND BLANK
      REPLACE ORDER   WITH lcOrd ;
              ACCOUNT WITH CUSTOMER.ACCOUNT ;
              BTNAME  WITH CUSTOMER.BTNAME ;
              STYLE   WITH SUBSTR(lcSty , 1 , lnMajorLen )
    ENDIF
    REPLACE SHRTQTY WITH SHRTQTY + lnShortage
  ENDIF
ENDDO

USE IN &lcAcptSty
ERASE (gcWorkDir+lcAcptSty+'.DBF')
ERASE (gcWorkDir+lcAcptSty+'.CDX')

SET ORDER TO &lcSvOrd   IN &lcTmpOrdLn
SET ORDER TO &lcCustOrd IN CUSTOMER

SELECT &lcTmpOrdLn
IF BETWEEN(lnRec,1,RECCOUNT())
  GOTO (lnRec)
ELSE
  GO TOP  
ENDIF

*IF The system use Dyelots
IF llUseDyes
  SET RELATION TO Style + cWareCode + DyeLot INTO STYDYE
ELSE    && Else
  SET RELATION TO Style + cWareCode + SPACE(10) INTO STYDYE
ENDIF    && End of IF

lcRelSt = IIF(lnSelAlo = 0 , 'DISABLE' , 'ENABLE')

SHOW GET pbRel &lcRelSt
SHOW GET pbGen &lcRelSt

=SEEK(Style , 'STYLE')
=SEEK('S' + STYLE.Scale , 'SCALE')

lcStyle = Style
lcStore = Store
lcGroup = Group
lcWareCode = cWareCode
lnWareCode = ASCAN(laWareCode , lcWareCode)
lcDyelot = IIF(llUseDyes , Dyelot , SPACE(10))
lnTotPik = TotPik
lnTotQty = TotQty
SCATTER FIELDS LIKE PIK* TO laPik
SCATTER FIELDS LIKE QTY* TO laQty
  
*IF the current record is selected
IF llSel
  =lfShowGets(.T.)
ELSE    && Else
  lcCh3Stat = 'DISABLE'
  laPikSt   = 'DISABLE'
  SHOW GETS WINDOW (lcAutAlCh3) DISABLE ONLY
ENDIF    && End of IF
=lfRefresh()

*-- end of lfRELALO.

*:**************************************************************************
*:* Name        : lfSHOWLOG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/18/2004
*:* Purpose     : Show log file for NOR01 , ALLOCATING STYLES
*:***************************************************************************
*:* Called from : ALAUTAL.PRG
*:***************************************************************************
*C124051,1 
FUNCTION lfSHOWLOG
PRIVATE lnSlct,lcLogFile,lnResp,lnCnt
lnSlct = SELECT()

lcLogFile = '_'+SUBSTR(lcAutAlCh3,2)
IF USED(lcLogFile) .AND. RECCOUNT(lcLogFile) > 0

  SELECT &lcLogFile
  GO TOP
  
  DO WHILE .T.
    GO TOP  
    *- Button 42017 : Pre\<view;\<Print;\?\<Cancel
    lnResp = gfModalGen('INM00000B42017',.F.,.F.,.F.,'Show Log File for not allocated styles ?')
    DO CASE
    CASE lnResp = 1
      REPORT FORM (gcRepHome+'AL\ALALOLOG') PREVIEW
    CASE lnResp = 2
      REPORT FORM (gcRepHome+'AL\ALALOLOG') TO PRINTER
    CASE lnResp = 3
      EXIT
    ENDCASE
  ENDDO
  
  USE IN &lcLogFile
  ERASE (gcWorkDir+lcLogFile+'.DBF')  
  ERASE (gcWorkDir+lcLogFile+'.CDX')

ENDIF

SELECT (lnSlct)

*-- end of lfSHOWLOG.
