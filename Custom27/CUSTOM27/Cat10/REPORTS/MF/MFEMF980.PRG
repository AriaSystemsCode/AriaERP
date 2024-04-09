*:************************************************************************
*:
*: Procedure file: MFEMF980.PRG
*:
*:         System: ARIA 2.7
*:         Module: Manufacturing
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 21/01/2001
*:
*:************************************************************************
*: For Custom # C102116 
*:***************************************************************************
*:Modifications:
*: B604824,1 AMH 08/19/2001 Print the division desc. under Label header instead of Car L.
*: B605478,1 HBG 02/04/2002 Fix bugs :
*:                             - Generate log # in each preview.
*:                             - Not filter on vendore when select for a log #
*: B605522,1 HBG 02/11/2002 Fix bugs :
*:                             - Can't activate the browse by mosue.
*:                             - Display lot # in the browse.
*:                             - Change the message when removing line for this log#.
*: B605966,1 HBG 05/21/2002 Allow to add the cutting ticket to more than one
*: B605966,1 HBG            log.
*:***************************************************************************************

IF lcRpType = 'R'
  *-- Return till the Rotation report done
  RETURN
ENDIF  

lcCareDsc1 = ''
DECLARE laCare1[1,2]
laCare1[1,1] = 'CDESC1'
laCare1[1,2] = 'lcCareDsc1'

SELECT Style
SET ORDER TO TAG Cstyle  


IF llRpRprLog
  IF EMPTY(lcRpLogNum)
    *--Message 'You have to select a Log #...!'
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You have to select a Log #. Cannot proceed.')
    RETURN
  ENDIF
  lcRpType = 'L'
ELSE
  IF EMPTY(lcRpVendor)
    *--Message 'You have to select a Vendor...!'
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You have to select a Vendor. Cannot proceed.')
    RETURN
  ENDIF
  *-- Collect the needed date to print
  =lfCollect()
  *-- Preaper variabels needed in printing
  =lfPrepVar()
ENDIF

SELECT (lcCtkTemp)

SET ORDER TO TAG &lcCtkTemp
GOTO TOP

IF EOF()
  *--Message 'There are no records to display...!'  
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

IF lcRpType = 'B'
  lcRpName = 'MFEMFLOG'
  DO gfDispRe WITH EVALUATE ('lcRpName')
  *-- These lines are commented tile the rotation report done
  *SELECT (lcCtkTemp)
  *GOTO TOP
  *lcRpName = 'MFEMFROT' 
  *DO gfDispRe WITH EVALUATE ('lcRpName')
ELSE
  IF lcRpType = 'L'
    lcRpName = 'MFEMFLOG'
  ELSE
    lcRpName = 'MFEMFROT'  
  ENDIF
  DO gfDispRe WITH EVALUATE ('lcRpName')
ENDIF

*B605478,1 HBG 02/04/2002 increment log# only when Run "Print the report"[Begin]
IF VARREAD() = "PBRUN"
  IF lcRpType = 'L' .OR. lcRpType = 'B' AND !llRpRprLog
    SELECT APVENDOR
    IF SEEK(lcRpVendor)
      lnLog_No = cLog_No                     
      lnLog_No = ALLTRIM(STR((VAL(lnLog_No)+1),4))  
      REPLACE APVENDOR.cLog_No WITH lnLog_No
    ENDIF  
  ELSE
    lnLog_No = SPACE(4)
  ENDIF
ENDIF  
*B605478,1[End]


IF !llRpRprLog
  *-- Close Temp file
  IF USED(lcCtkTemp)
    SELECT (lcCtkTemp)
    SET RELATION TO
    USE
    ERASE (gcDataDir+lcCtkTemp+ '.DBF')
    ERASE (gcDataDir+lcCtkTemp+ '.CDX')
    IF FILE (gcDataDir+lcCtkTemp+ '.FPT')
      ERASE (gcDataDir+lcCtkTemp+ '.FPT')
    ENDIF  
  ENDIF
ENDIF
  
*!*************************************************************
*! Name      : lfCollect
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : Collect Data
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfCollect

IF !llRpRprLog
  SELECT CUTTKTH
  =AFIELDS(laStrufile)
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'CutAt'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'SewAt'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'Marker'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 10
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'CutWith'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 24
  laStrufile[lnFileStru+1,4] = 0
  
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'cCarlbl'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 24
  laStrufile[lnFileStru+1,4] = 0
  
  *B605966,1 HBG 05/21/2002 (Begin) Adding a vendor field to the temp file.
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'cCutVen'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  *B605966,1 HBG 05/21/2002 (End)

  CREATE TABLE (gcDataDir+lcCtkTemp) FROM ARRAY laStrufile
  SELECT (lcCtkTemp)
  INDEX ON CutAt+cPack+Season+cdivision+Style+Cuttkt TAG &lcCtkTemp  
  INDEX ON Style+STR(nLot_No,4) TAG &lcCtkTemp1
  SET ORDER TO TAG &lcCtkTemp
  SET RELATION TO style INTO Style ADDITIVE
  
  IF lcRpSelect = 'D' 
    lcRpExp = IIF(lcRpExp = ".T.",IIF(!EMPTY(lcRpStyle) AND LEN(ALLTRIM(lcRpStyle)) <> 1,;
                                      "Style = lcRpStyle",".T."),;
                  lcRpExp + IIF(!EMPTY(lcRpStyle) AND LEN(ALLTRIM(lcRpStyle)) <> 1,;
                                "AND Style = lcRpStyle",""))
  ENDIF 
  lcRpExp = lcRpExp + IIF(lcRpStatus <> 'A',' .AND. STATUS = lcRpStatus','')

  SELECT CUTTKTH
  
  SET RELATION TO style INTO Style ADDITIVE
  
  GO TOP
  SCAN FOR &lcRpExp
    SCAT MEMVAR
    llAddRec = IIF(!EMPTY(lcRpCutVnd) OR !EMPTY(lcRpSewVnd),;
                    IIF(lcRpSelect = 'D',lfChkVend(),.T.),.T.)
    IF !llAddRec 
      LOOP
    ENDIF
    
    INSERT INTO (lcCtkTemp) FROM MEMVAR
    FOR lnI = 1 TO 5
       lcI = ALLTRIM(STR(lnI))
       lcCutSty = 'cct'+ lcI + 'csty'
       lcCutLot = 'nct'+ lcI + 'nlot'
       IF !EMPTY(&lcCutSty)
         REPLACE &lcCtkTemp..CutWith WITH ALLTRIM(&lcCutSty) + '-' + ALLTRIM(STR(&lcCutLot))
         EXIT
       ENDIF
    ENDFOR
    *khalid
    *IF SEEK(STYLE,'BOM')
    IF SEEK(m.STYLE,'BOM')
    *khalid
      SELECT BOM
      *khalid
      *LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = STYLE FOR ccatgtyp = 'F'
      LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = m.STYLE FOR ccatgtyp = 'F'
      *khalid
      IF FOUND()
        REPLACE &lcCtkTemp..Marker WITH cmarker
      ENDIF
    ENDIF

    SELECT MFGOPRHD
    =SEEK('M'+CUTTKTH.CUTTKT)
    lnCountr = 1
    SCAN REST WHILE cimtyp+ctktno+coprcode = 'M' + CUTTKTH.CUTTKT ;
              FOR !lInHouse               
      IF lnCountr = 1 
        IF  !EMPTY(cContCode) AND SEEK(cContCode,'APVENDOR')
          REPLACE &lcCtkTemp..CutAt WITH APVENDOR.cVenComp
        ENDIF
        lnCountr = lnCountr + 1
      ELSE
        IF !EMPTY(cContCode) AND SEEK(cContCode,'APVENDOR')
          REPLACE &lcCtkTemp..SewAt WITH APVENDOR.cVenComp
        ENDIF
      ENDIF
    ENDSCAN
    
    *B604824,1 AMH Print the division desc. under Label header instead of Car label [Start]
    *= gfRltFld(STYLE.cCarLbl , @laCare1 , 'CCARLBL')
    *REPLACE &lcCtkTemp..cCarlbl WITH lcCareDsc1
    SELECT (lcCtkTemp)
    REPLACE cCarlbl WITH gfCodDes(CUTTKTH.CDIVISION, 'CDIVISION')
    *B604824,1 AMH [End]
    
    SELECT CUTTKTH
  ENDSCAN
ENDIF

*!*************************************************************
*! Name      : lfPrepVar
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : preaper the neede variabel
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrepVar()
*!*************************************************************
FUNCTION lfPrepVar


*B605478,1 HBG 02/04/2002 Move this block of code , it will 
*B605478,1                increment log# only when print the report[Begin]
*IF lcRpType = 'L' .OR. lcRpType = 'B'
*  SELECT APVENDOR
*  IF SEEK(lcRpVendor)
*    lnLog_No = cLog_No                     
*    lnLog_No = ALLTRIM(STR((VAL(lnLog_No)+1),4))  
*    REPLACE APVENDOR.cLog_No WITH lnLog_No
*  ENDIF  
*ELSE
*  lnLog_No = SPACE(4)
*ENDIF
*B605478,1 [End]

IF (lcRpType = 'L' .OR. lcRpType = 'B') AND !llRpRprLog
  SELECT (lcCtkTemp)
  *B605478,1 HBG 02/04/2002 Get the log# of the vendore[Begin]
  IF SEEK(lcRpVendor,'APVENDOR')
    lnLog_No = APVENDOR.cLog_No
  ENDIF
  *B605478,1 [End]
  SCAN
    IF !SEEK(APVENDOR.cLog_No+CUTTKT,'CUTLOG')
      SELECT CUTLOG
      APPEND BLANK
      *B605966,1 HBG 05/21/2002 (Begin) Adding the replacement of the vendor.
      *REPLACE cLog_No  WITH lnLog_No ,;
              CUTTKT   WITH &lcCtkTemp..CUTTKT  ,;
              STYLE    WITH &lcCtkTemp..STYLE   ,;
              nLot_No  WITH &lcCtkTemp..nLot_No ,;
              PCS_OPN  WITH &lcCtkTemp..PCS_OPN ,;
              DlogDate WITH gdSysDate
      REPLACE cLog_No  WITH lnLog_No ,;
              CUTTKT   WITH &lcCtkTemp..CUTTKT  ,;
              STYLE    WITH &lcCtkTemp..STYLE   ,;
              nLot_No  WITH &lcCtkTemp..nLot_No ,;
              PCS_OPN  WITH &lcCtkTemp..PCS_OPN ,;
              DlogDate WITH gdSysDate,;
              cCutVen  WITH lcRpVendor
      *B605966,1 HBG 05/21/2002 (End)
      SELECT (lcCtkTemp)
    ENDIF  
  ENDSCAN
ENDIF  

*!*************************************************************
*! Name      : lfClrRead
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : Refresh the option grid
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClrRead ()
*!*************************************************************
FUNCTION lfClrRead

*-- Refresh the filters in the option grid.
CLEAR READ

*-- End of lfClrRead.

*!*************************************************************
*! Name      : lfvVendor
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

SELECT APVENDOR
IF !EMPTY(lcRpVendor) .AND. ('?' $ lcRpVendor .OR. !SEEK(lcRpVendor, 'APVENDOR'))
  =gfApVnBrow(@lcRpVendor)
ENDIF

*!*************************************************************
*! Name      : lfvCutVnd
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : validate cut vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCutVnd()
*!*************************************************************

FUNCTION lfvCutVnd

SELECT APVENDOR
IF !EMPTY(lcRpCutVnd) .AND. ('?' $ lcRpCutVnd .OR. !SEEK(lcRpCutVnd, 'APVENDOR'))
  =gfApVnBrow(@lcRpCutVnd)
ENDIF

*!*************************************************************
*! Name      : lfvSewVnd
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Sew Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSewVnd()
*!*************************************************************

FUNCTION lfvSewVnd

SELECT APVENDOR
IF !EMPTY(lcRpSewVnd) .AND. ('?' $ lcRpSewVnd .OR. !SEEK(lcRpSewVnd, 'APVENDOR'))
  =gfApVnBrow(@lcRpSewVnd)
ENDIF

*!*************************************************************
*! Name      : lfvLogNum
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : validate Log #
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvLogNum()
*!*************************************************************

FUNCTION lfvLogNum
PRIVATE lnTotal , lcLog

SELECT CUTLOG
IF !EMPTY(lcRpLogNum)  AND !SEEK(PADR(lcRpLogNum,4))
  DIMENSION laTempData[1]
  STORE '' TO laTempData
  lcKey = lcRpLogNum

  LogTemp = gfTempName()
  CREATE CURSOR (LogTemp) ;
	  (log_no C(4) , Log_Date D(8) , Pcs_Opn N(7))
  INDEX ON LOG_NO TAG &LogTemp
  
  lnTotal=0
  SELECT CUTLOG
  GOTO TOP
  DO WHILE !EOF()
    lcLog = clog_no
    SELECT (LogTemp)
    IF !SEEK(lcLog)
      INSERT INTO (LogTemp) (log_no,Log_Date) VALUE (CUTLOG.clog_no,CUTLOG.DLogDate)
    ENDIF
    SELECT CUTLOG
    SUM REST PCS_OPN TO lnTotal WHILE clog_no+cuttkt = lcLog
    SELECT (LogTemp)
    REPLACE Pcs_Opn   WITH  lnTotal
    SELECT CUTLOG
  ENDDO
  
  SELECT (LogTemp)
  GOTO TOP   
  lcBrFields = "log_no  :R :H = 'Log #'     :6,"+;
               "Log_Date   :H = 'log Date'  :9 :R,"+;
               "Pcs_Opn    :H = 'Total Qty.':11 "
           
  IF !AriaBrow('',"Log #",;
               20, 01,20,05,;
               .F.,.F.,"&LogTemp..log_no","laTempData")
    lcRpLogNum = SPACE(4)
    RETURN    
  ELSE
    lcRpLogNum = laTempData[1]
  ENDIF  
ENDIF  

IF lcRpLogNum <> SPACE(4)
  SELECT CUTTKTH
  =AFIELDS(laStrufile)
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'cLog_no'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 4
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'Dlogdate'
  laStrufile[lnFileStru+1,2] = 'D'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'CutAt'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'SewAt'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'Marker'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 10
  laStrufile[lnFileStru+1,4] = 0
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'CutWith'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 24
  laStrufile[lnFileStru+1,4] = 0
  
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'cCarlbl'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 24
  laStrufile[lnFileStru+1,4] = 0

  *B605966,1 HBG 05/21/2002 (Begin) Adding the vendor field.
  lnFileStru = ALEN(laStrufile,1)
  DIMENSION laStrufile[lnFileStru+1,4]
  laStrufile[lnFileStru+1,1] = 'cCutVen'
  laStrufile[lnFileStru+1,2] = 'C'
  laStrufile[lnFileStru+1,3] = 8
  laStrufile[lnFileStru+1,4] = 0
  *B605966,1 HBG 05/21/2002 (End)
    
  CREATE TABLE (gcDataDir+lcCtkTemp) FROM ARRAY laStrufile
  SELECT (lcCtkTemp)    
  SET RELATION TO style INTO Style ADDITIVE
  INDEX ON CutAt+cPack+Season+cdivision+Style+Cuttkt TAG &lcCtkTemp  
  INDEX ON Style+STR(nLot_No,4) TAG &lcCtkTemp1
  SET ORDER TO TAG &lcCtkTemp
  
  IF SEEK(PADR(lcRpLogNum,4),'CUTLOG')
    SELECT CUTLOG
    SCAN REST WHILE clog_no+cuttkt = PADR(lcRpLogNum,4)
      SELECT CUTTKTH
      IF SEEK(cutlog.cuttkt) AND STYLE = cutlog.style
        SCATTER MEMVAR MEMO
        INSERT INTO &gcDataDir.&lcCtkTemp FROM MEMVAR
        REPLACE &lcCtkTemp..clog_no  WITH CUTLOG.clog_no
        REPLACE &lcCtkTemp..Dlogdate WITH CUTLOG.Dlogdate
        *B605966,1 HBG 05/21/2002 (Begin) Adding the replacement of the vendor
        REPLACE &lcCtkTemp..cCutVen  WITH CUTLOG.cCutVen 
        *B605966,1 HBG 05/21/2002 (End)
      ENDIF
    ENDSCAN      
  ENDIF
  SELECT (lcCtkTemp) 
  
  *khalid
  SET ORDER TO
  LOCATE
  *khalid
  
  SCAN
    SELECT MFGOPRHD
    IF SEEK('M'+&lcCtkTemp..CUTTKT)
      lnCountr = 1
      SCAN REST WHILE cimtyp+ctktno+coprcode = 'M'+&lcCtkTemp..CUTTKT ;
                FOR !lInHouse               
        IF lnCountr = 1 
          IF  !EMPTY(cContCode) AND SEEK(cContCode,'APVENDOR')
            REPLACE &lcCtkTemp..CutAt WITH APVENDOR.cVenComp
          ENDIF
          lnCountr = lnCountr + 1
        ELSE
          IF !EMPTY(cContCode) AND SEEK(cContCode,'APVENDOR')
            REPLACE &lcCtkTemp..SewAt WITH APVENDOR.cVenComp
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF  
    *khalid
    *IF SEEK(CUTTKTH.STYLE,'BOM')
    IF SEEK(&lcCtkTemp..STYLE,'BOM')
    *khalid
      SELECT BOM
      LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = &lcCtkTemp..STYLE FOR ccatgtyp = 'F'
      IF FOUND()
        REPLACE &lcCtkTemp..Marker WITH cMarker
      ENDIF
    ENDIF
    SELECT (lcCtkTemp) 
    FOR lnI = 1 TO 5
      lcI = ALLTRIM(STR(lnI))
      lcCutSty  = 'cct'+ lcI + 'csty'
      lcCutLot  = 'nct'+ lcI + 'nlot'
      IF EMPTY(&lcCutSty)
        lnI = lnI + 1
      ELSE     
        REPLACE &lcCtkTemp..CutWith WITH ALLTRIM(&lcCutSty) + '-' + ALLTRIM(STR(&lcCutLot))
        lnI = 5
      ENDIF
    ENDFOR 
    
    *B604824,1 AMH Print the division desc. under Label header instead of Car label [Start]
    *= gfRltFld(STYLE.cCarLbl , @laCare1 , 'CCARLBL')
    *REPLACE &lcCtkTemp..cCarlbl WITH lcCareDsc1
    SELECT (lcCtkTemp)
    REPLACE cCarlbl WITH gfCodDes(CDIVISION, 'CDIVISION')
    *B604824,1 AMH [End]
        
  ENDSCAN
  *khalid
  SELECT (lcCtkTemp)
  SET ORDER TO TAG &lcCtkTemp
  *khalid
  
  lcOldValue = SPACE(0)
  lcMjrPct = gfItemMask('PM')
  llbrowse = .F.
  llNew = .F.
  lnCurObj = 1
  *-- Browse screen titel
  lcStyBrow = 'Cut Tickets for Log# :' + lcRpLogNum  
  SELECT CUTLOG
  DO (gcRepHome+gcAct_Appl+"\MFEMFCT.SPR")
  
ENDIF


*!*************************************************************
*! Name      : lfvBrow
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : Browse function for the Cuttkt for this LOG #.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvBrow

lcSelect = SELECT()

*B605522,1 HBG 02/11/2002 Display lot # in the browse [Begin]
*lcBrowFild = "CUTTKT :R :H = 'CUTTKT'      :8:R,"+;
             "Style     :H = 'Style'      :30:R,"+;
             "Dlogdate:R:H = 'Log Date '  :10:R,"+;
             "PCS_OPN :R:H = 'Open':P = '999999':10:R"
lcBrowFild = "CUTTKT :R :H = 'CUTTKT'      :8:R,"+;
             "Style     :H = 'Style'      :30:R,"+;
             "nlot_no   :H = 'Lot #'      :5:R,"+;
             "Dlogdate:R:H = 'Log Date '  :10:R,"+;
             "PCS_OPN :R:H = 'Open':P = '999999':10:R"
*B605522,1 [End]                          


SELECT (lcCtkTemp)
GOTO TOP
*B605522,1 HBG 02/11/2002 refresh fields in the screen when moving in the browse [Begin]
*BROWSE FIELDS &lcBrowFild              ;
       FOR &lcCtkTemp..cLog_no = PADR(lcRpLogNum,4) ;
       WINDOW MFEMF01                  ;
       IN WINDOW MFEMFCT               ;
       LOCK 0                          ;
       NOAPPEND                        ;
       NOCLEAR                         ;
       NODELETE                        ;
       NOWAIT                          ;
       NOEDIT                          ;
       NOMENU                          ;
       SAVE                            ;
       TITLE (lcStyBrow)

BROWSE FIELDS &lcBrowFild              ;
       FOR &lcCtkTemp..cLog_no = PADR(lcRpLogNum,4) ;
       WINDOW MFEMF01                  ;
       IN WINDOW MFEMFCT               ;
       WHEN lfwBrwWhen()               ;
       LOCK 0                          ;
       NOAPPEND                        ;
       NOCLEAR                         ;
       NODELETE                        ;
       NOWAIT                          ;
       NOEDIT                          ;
       NOMENU                          ;
       SAVE                            ;
       TITLE (lcStyBrow)
*B605522,1 [End]      
SHOW WINDOW (lcStyBrow) REFRESH SAME
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : HEND GHANEM
*! Date      : 02/11/2002 
*! Purpose   : WHEN function of the browse command
*!*************************************************************
*! Calls              : lfRefresh()
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
*!B605522,1 
FUNCTION lfwBrwWhen

lcStyle = &lcCtkTemp..style
lnLot   = STR(&lcCtkTemp..nlot_no,4)
lcCtkt  = &lcCtkTemp..cuttkt

SHOW GET lcStyle DISABLE
SHOW GET lnLot   DISABLE
SHOW GET lcCtkt  DISABLE
=lfRefresh()

*!*************************************************************
*! Name      : lfvDeact
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvDeact
IF WONTOP() = lcStyBrow
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTrapKey WITH 'MFEMF02' , 'pbIns'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'MFEMF03' , 'pbCls'
  ON KEY LABEL ESC     RETURN
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   :
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfReadAct

IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcStyBrow)
*-- END


*!*************************************************************
*! Name      : lfvMod
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : lnoper --> to determain which operation
*!                    : will be done (Insert\Delete)
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
PROCEDURE lfvMod
PARAMETERS lnOper


DO CASE
  CASE lnOper = 1      && Insert Case
    llNew = .T.
    *B605522,1 HBG 02/11/2002 Reset fields in the screen to add new line [Begin]
    STORE '' TO lcStyle,LcCtkt
    lnLot  = SPACE(4)
    *B605522,1 [End]
    SHOW GET ibStyle ENABLE
    SHOW GET lcStyle ENABLE
    SHOW GET ibLot   ENABLE
    SHOW GET lnLot   ENABLE    
    SHOW GET ibCtkt  ENABLE
    SHOW GET LcCtkt  ENABLE

    

  CASE lnOper = 2      && Delete Case
    
    *B605522,1 HBG 02/11/2002 Change the message when trying to delete line [Begin]
    *lnChoice =gfModalGen('INM00000B38006','DIALOG',.F.,.F.,'Are you sure you want to delete this line?')    
    lnChoice =gfModalGen('INM00000B38006','DIALOG',.F.,.F.,;
              'Are you sure you want to delete Style-Lot : '+ ALLTRIM(&lcCtkTemp..Style) + '-' + ALLTRIM(STR(&lcCtkTemp..nlot_no)) +' ?')    
    *B605522,1 [End]
    IF lnChoice = 2
      RETURN
    ELSE
      ***   DELETE FROM TEMP. FILE ***
      SELECT (lcCtkTemp)
      lcCuttkt = Cuttkt
      DELETE
      ***   DELETE FROM CUTLOG FILE ***
      SELECT CUTLOG
      SET ORDER TO TAG CUTLOG     
      IF SEEK(lcRpLogNum + lcCuttkt)
        DELETE
      ENDIF  
    ENDIF  
ENDCASE

SELECT (lcCtkTemp)
GO TOP

=lfvBrow()

*!*************************************************************
*! Name      : lfvAccept
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
PROCEDURE lfvAccept

IF llNew
  
  ** SAVING IN TEMP FILE ************
  SELECT(lcCtkTemp)
  INDEX ON CUTTKT  TAG &lcCtkTemp
  SET DELETE OFF
  IF SEEK(lcCtkt)
    RECALL
  ELSE  
    =SEEK(lcCtkt,'CUTTKTH')
    APPEND BLANK
    REPLACE  CUTTKT     WITH  lcCtkt              ;
             NLOT_NO    WITH  CUTTKTH.nlot_no     ;
             STYLE      WITH  CUTTKTH.Style       ;
             DESC       WITH  CUTTKTH.DESC        ;  
             Status     WITH  CUTTKTH.Status      ;      
             cpack      WITH  CUTTKTH.cpack       ;
             SEASON     WITH  CUTTKTH.SEASON      ;
             cDIVISION  WITH  CUTTKTH.cdivision 

    *B605966,1 HBG 05/21/2002 (Begin) Replace the vendor code
    =SEEK(lcRpLogNum+lcCtkt,'CUTLOG')
    REPLACE cCutVen WITH CUTLOG.cCutVen
    *B605966,1 HBG 05/21/2002 (End)
    
    IF SEEK('M'+lcCtkt,'MFGOPRHD')
      SELECT MFGOPRHD
      lnCountr = 1
      SCAN REST WHILE cimtyp+ctktno+coprcode = 'M' + CUTTKTH.CUTTKT ;
                FOR !lInHouse               
      
        IF lnCountr = 1 
          IF  !EMPTY(cContCode) AND SEEK(cContCode,'APVENDOR')
            REPLACE &lcCtkTemp..CutAt WITH APVENDOR.cVenComp
          ENDIF
          lnCountr = lnCountr + 1
        ELSE
          IF !EMPTY(cContCode) AND SEEK(cContCode,'APVENDOR')
            REPLACE &lcCtkTemp..SewAt WITH APVENDOR.cVenComp
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
    IF SEEK(CUTTKTH.STYLE,'BOM')
      SELECT BOM
      LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = CUTTKTH.STYLE FOR ccatgtyp = 'F'
      IF FOUND()
        REPLACE &lcCtkTemp..Marker WITH cmarker
      ENDIF
    ENDIF
    SELECT CUTTKTH
    FOR lnI = 1 TO 5
      lcI = ALLTRIM(STR(lnI))
      lcCutSty = 'cct'+ lcI + 'csty'
      lcCutLot = 'nct'+ lcI + 'nlot'
      IF EMPTY(&lcCutSty)
        lnI = lnI + 1
      ELSE     
        REPLACE &lcCtkTemp..CutWith WITH ALLTRIM(&lcCutSty) + '-' + ALLTRIM(STR(&lcCutLot))
        lnI = 5
      ENDIF
    ENDFOR 
    
    *B604824,1 AMH Print the division desc. under Label header instead of Car label [Start]
    *= gfRltFld(STYLE.cCarLbl , @laCare1 , 'CCARLBL')
    *REPLACE &lcCtkTemp..cCarlbl WITH lcCareDsc1
    SELECT (lcCtkTemp)
    REPLACE cCarlbl WITH gfCodDes(CDIVISION, 'CDIVISION')
    *B604824,1 AMH [End]
        
  ENDIF    
  SELECT(lcCtkTemp) 
  GO TOP
  SET DELETE ON
  SET ORDER TO TAG (lcCtkTemp)

  ** SAVING IN MASTER FILE ***********
  SELECT CUTLOG
  *B605966,1 HBG 05/21/2002 (Begin) Getting the vendor code
  IF SEEK(lcRpLogNum)
    lcCutVend = CUTLOG.cCutVen
  ENDIF
  *B605966,1 HBG 05/21/2002 (End)
  
  SET DELETE OFF 
  IF SEEK(lcRpLogNum + lcCtkt)
    RECALL
  ELSE  
    =SEEK(lcCtkt,'CUTTKTH')
    APPEND BLANK
  ENDIF         
  =RLOCK()

  *B605966,1 HBG 05/21/2002 (Begin) Adding the replacement of the vendor
  *REPLACE  clog_no   WITH  lcRpLogNum     ;    
           CUTTKT    WITH  lcCtkt          ;
           STYLE     WITH  CUTTKTH.Style   ;
           dlogdate  WITH  DATE()          ;
           nlot_no   WITH  CUTTKTH.nlot_no ;
           PCS_OPN   WITH  CUTTKTH.PCS_OPN 
  REPLACE  clog_no   WITH  lcRpLogNum      ,;    
           CUTTKT    WITH  lcCtkt          ,;
           STYLE     WITH  CUTTKTH.Style   ,;
           dlogdate  WITH  DATE()          ,;
           nlot_no   WITH  CUTTKTH.nlot_no ,;
           PCS_OPN   WITH  CUTTKTH.PCS_OPN ,;
           cCutVen   WITH  lcCutVend 
  *B605966,1 HBG 05/21/2002 (End)
  UNLOCK
  SET DELETE ON
  CLEAR READ   
ELSE
  CLEAR READ
ENDIF  

=lfvBrow()

*!*************************************************************
*! Name      : lfvClose
*! Developer : Hend Ghanem (HBG)
*! Date      : 18/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
PROCEDURE lfvClose


*** CANCEL THE DELETION FROM TEMP. FILE ***
SELECT (lcCtkTemp)
INDEX ON CUTTKT  TAG &lcCtkTemp
SET DELETE OFF
IF SEEK(lcCuttkt)
  RECALL
ENDIF
SET DELETE ON
*** CANCEL THE DELETION FROM CUTLOG FILE ***
SELECT CUTLOG
SET ORDER TO TAG CUTLOG   
SET DELETE OFF  
IF SEEK(lcRpLogNum + lcCuttkt)
  RECALL
ENDIF  
SET DELETE ON
lcRpLogNum = SPACE(4)
CLEAR READ


*!*************************************************************
*! Name      : lfvMajor
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/21/2000
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Due to C102094,102095
*:*************************************************************
*
FUNCTION lfvMajor

PRIVATE llCmFrmBrw, llOpenSty, lnAlias, lcOrder
STORE .F. TO llCmFrmBrw, llOpenSty

IF !llbrowse AND EMPTY(SUBSTR(lcStyle,1,3)) AND EMPTY(SUBSTR(lcStyle,5,LEN(ALLTRIM(lcStyle))))
  RETURN
ENDIF
IF !USED('STYLE')
  = gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
  llOpenSty = .T.
ENDIF
SELECT Style
*SET FILTER TO Make AND Status <> 'X'

IF llbrowse OR !EMPTY(lcStyle) .OR. !SEEK(lcStyle,'STYLE')
  IF llbrowse OR ATC('?',lcStyle) <> 0
    llbrowse = .F.
    lcStyle = gfStyBrw('M',"","",.F.)
    llCmFrmBrw = .T.
  ENDIF
  IF !SEEK(lcStyle,'STYLE') .AND. !EMPTY(lcStyle)
    lcStyle = gfStyBrw('M',"","",.F.)
    llCmFrmBrw = .T.
  ENDIF
ENDIF
IF llOpenSty
  = gfCloseFile('STYLE')
ENDIF

IF !(lcOldValue == lcStyle)
  lcCtkt = SPACE(0)
  lnLot  = SPACE(4)
  SHOW GET lcCtkt
  SHOW GET lnLot
  SHOW GET pbOk DISABLE
ENDIF
IF EMPTY(SUBSTR(lcStyle,1,3)) AND EMPTY(SUBSTR(lcStyle,5,LEN(ALLTRIM(lcStyle))))
 _CUROBJ = _CUROBJ
ENDIF
SELECT CutTktH
RETURN
*-- end of lfvMajor.

*!*************************************************************
*! Name      : lfwMajor
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/21/2000
*! Purpose   : When function to get the current style field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Due to C102094,102095
*:*************************************************************
*
FUNCTION lfwMajor

lcOldValue = lcStyle
RETURN
*-- end of lfwMajor.
*!*************************************************************
*! Name      : lfvLot()
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/24/2000
*! Purpose   : Valid function to validate Lot field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Due to C102094,C102095
*!*************************************************************
*
FUNCTION lfvLot

PRIVATE llCmFrmBrw, lnAlias, lcOrder, llFound, lcFile_Ttl, lcKeyVal
STORE .F. TO llCmFrmBrw, llFound
DIMENSION laTempData[3]
STORE '' TO laTempData

IF EMPTY(lnLot) AND !llBrowse
  lcCtkt = SPACE(6)
  SHOW GET lcCtkt
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT CUTTKTH
lcOrder = SET('ORDER')
SET ORDER TO CUTTKTHS
=SEEK(lcStyle)
lcKeyVal = IIF(EOF(),"[]","[lcStyle]")

lcFile_Ttl = 'Cutting Tickets'
lcBrFields = "CutTkt  :H='Cutkt'   ,"+;
             "Style   :H='Style'   ,"+;
             "nLot_No :H='Lot #'   ,"+;
             "Status  :H='S'       ,"+;
             "Entered :H='Issue'   ,"+;
             "Complete:H='Complete',"+;
             "cDivision:H='Di'     ,"+;
             "Pcs_Bud :H='Budget':P='999999',"+;
             "Pcs_Rec :H='Recvd' :P='999999',"+;
             "Pcs_Dam :H='Damged':P='999999',"+;
             "Pcs_Opn :H='Open'  :P='999999' "

IF llbrowse
  llbrowse = .F.
  =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'nLot_No,CutTkt,Style','laTempData')
  lnLot = IIF(EMPTY(laTempData[1]),SPACE(4),STR(laTempData[1],4))
  lcCtkt = laTempData[2]
  lcStyle = IIF(!EMPTY(ALLTRIM(laTempData[3])),ALLTRIM(laTempData[3]),lcStyle)
  llCmFrmBrw = .T.
ELSE
  lnLot = INT(VAL(lnLot))
  IF lnLot = 0
    =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'nLot_No,CutTkt,Style','laTempData')
    lnLot = IIF(EMPTY(laTempData[1]),SPACE(4),STR(laTempData[1],4))
    lcCtkt = laTempData[2]
    lcStyle = IIF(!EMPTY(ALLTRIM(laTempData[3])),ALLTRIM(laTempData[3]),lcStyle)
    llCmFrmBrw = .T.
  ELSE
    LOCATE REST WHILE STYLE+CUTTKT=lcStyle FOR nLot_No = lnLot
    IF FOUND()
      llFound = .T.
      lcCtkt  = CutTkt
      lnLot = STR(lnLot,4)
    ELSE
      =SEEK(lcStyle)
      lcKeyVal = IIF(EOF(),"[]","[lcStyle]")
      =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'nLot_No,CutTkt,Style','laTempData')
      lnLot = IIF(EMPTY(laTempData[1]),SPACE(4),STR(laTempData[1],4))
      lcCtkt = laTempData[2]
      lcStyle = IIF(!EMPTY(ALLTRIM(laTempData[3])),ALLTRIM(laTempData[3]),lcStyle)
      llCmFrmBrw = .T.
    ENDIF  
  ENDIF
ENDIF
IF !EMPTY(lcCtkt)
  SHOW GET pbOk ENABLE
  SHOW GET lcStyle ENABLE
ELSE
  SHOW GET pbOk DISABLE
  _CUROBJ = _CUROBJ
ENDIF
SET ORDER TO &lcOrder.
SELECT (lnAlias)
RETURN
*-- end of lfvLot.
*!*************************************************************
*! Name      : lfvCutTkt
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/21/2000
*! Purpose   : Valid function to validate cuttkt field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Due to C102094
*!*************************************************************
*
FUNCTION lfvCutTkt
PRIVATE lnAlias, lcBrFields, lcBrowTitle

lnAlias = SELECT(0)
IF !llBrowse  AND EMPTY(lcCtkt)
  RETURN
ENDIF

SELECT CutTktH
SET ORDER TO CutTktH
IF llBrowse OR (!EMPTY(lcCtkt) AND !SEEK(lcCtkt,"CutTktH"))
  llBrowse  = .F.
  xCutTkt   = lcCtkt
  xStyle    = lcStyle
  llNoThing = CutBrow(xCutTkt,xStyle)
  IF EMPTY(xCutTkt)
    lcCtkt = SPACE(1)
    _CUROBJ = _CUROBJ
  ELSE
    lcCtkt  = xCutTkt
  ENDIF
ENDIF
IF EMPTY(lcCtkt)
  SHOW GET pbOk DISABLE
  lnLot   = SPACE(4)
  _CUROBJ = _CUROBJ
ELSE
  lcStyle = ALLTRIM(Style)
  lnLot   = STR(nLot_No,4)
  SHOW GET pbOk ENABLE
ENDIF
SHOW GET lcStyle ENABLE
SHOW GET lnLot   ENABLE
SELECT(lnAlias)

*B605966,1 HBG 05/21/2002 (Begin) The following code is commented out.
*SELECT CUTLOG
*SCAN
*  IF Cuttkt = lcCtkt
*    =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'This Cut Ticket is Assaign to Log #'+CUTLOG.clog_no)    
*    lcStyle  = SPACE(LEN(lcMjrPct))
*    lcCtkt   = SPACE(0)
*    lnLot    = SPACE(4)
*    SHOW GET lcStyle  
*    SHOW GET lcCtkt   
*    SHOW GET lnLot    
*    RETURN
*  ENDIF
*ENDSCAN
*B605966,1 HBG 05/21/2002 (End)

*-- end of lfvCutTkt.

*!*************************************************************
*! Name      : lfvOMsg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To Open the Optional message screen
*!*************************************************************
*! Called from : MFPRTCT.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOMsg()
*!*************************************************************
FUNCTION lfvOMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 2nd. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 3rd. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.

*!*************************************************************
*! Name      : lfChkVend
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : Validate the Cut at & Sew at vendores
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************

FUNCTION lfChkVend
PRIVATE llReturn , lnCountr

llReturn = .F.
SELECT MFGOPRHD
IF SEEK('M'+CUTTKTH.CUTTKT)
  IF !EMPTY(lcRpCutVnd) OR !EMPTY(lcRpSewVnd)
    lnCountr = 1
    SCAN REST WHILE cimtyp+ctktno+coprcode = 'M' + CUTTKTH.CUTTKT ;
              FOR !lInHouse               
      IF lnCountr = 1 
        IF lcRpCutVnd = cContCode
          llReturn = .T.
        ENDIF
        lnCountr = lnCountr + 1
      ELSE
        IF lcRpSewVnd = cContCode
          llReturn = .T.
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF  
RETURN llReturn


*!*************************************************************
*! Name      : lfsrvSty  
*! Developer : HEND GHANEM (HBG)
*! Date      : 21/01/2001
*! Purpose   : Validate the Cut at & Sew at vendores
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************

FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SELECT CUTTKTH
  LOCATE
  SET ORDER TO TAG Cuttkths
ELSE
  SET ORDER TO TAG Cuttkth  IN CUTTKTH
ENDIF
