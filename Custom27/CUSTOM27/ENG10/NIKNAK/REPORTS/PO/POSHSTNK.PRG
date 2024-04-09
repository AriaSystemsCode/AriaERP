*:***************************************************************************
*: Program file  : POSHSTNK
*: Program desc. : Custom P/O Shipment Status For NIKNAK
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Orders
*: Developer     : NADER NABIL (NNA)
*: Task NO #     : C#123650,1
*:***************************************************************************
*: Calls :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO POSHSTNK
*:***************************************************************************
*: Modification :
*:***************************************************************************

lcRpOpTitl = lcRpOpTtl
*--Get the value of the lcRpStatus variable 

IF !EMPTY(laRpTarget[1])
  lcRpExp = IIF(EMPTY(lcRpExp) ,'.T. .AND. SHPMTHDR.STATUS $ (lcRpStatus)' , lcRpExp +' .AND. SHPMTHDR.STATUS $ (lcRpStatus)' )
ENDIF

IF FILE(gcDataDir + 'SHPCRTMF.DBF') .AND. !USED('SHPCRTMF')
  =gfOpenFile(gcDataDir +'SHPCRTMF',gcDataDir+'SHPCRTMF','SH')
ENDIF
= lfCrtTemp()
= lfSetRela()
= lfColData()

*-- Get From and To dates.

lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.DADD_DATE'),1)
LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,;
            ATC('|',laOGFxFlt[lnDatePos,6])-1))

HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],;
            ATC('|',laOGFxFlt[lnDatePos,6])+1))

lcOldCen = SET('CENTURY')
SET CENTURY ON

SELECT (lcShpTmp)
LOCATE

DO gfDispRe WITH gcRepHome +gcAct_Appl+ '\POSHSTNK.FRX'
SET CENTURY &lcOldCen

*!*************************************************************
*! Name        : lfCrtTemp
*! Developer   : NADER NABIL (NNA)
*! Date        : 08/29/2004
*! Purpose     : Create a Temp file to store the Collection Data
*!*************************************************************
*! Called from : PoShStNk.Prg
*!*************************************************************
*! Example     : = lfCrtTemp()
*!*************************************************************
FUNCTION lfCrtTemp
DIMENSION laFileStru[17,4]
laFileStru[ 1 , 1 ] = 'SHIPNO' 
laFileStru[ 1 , 2 ] = 'C' 
laFileStru[ 1 , 3 ] = 6          
laFileStru[ 1 , 4 ] = 0

laFileStru[ 2 , 1 ] = 'PO'
laFileStru[ 2 , 2 ] = 'C'
laFileStru[ 2 , 3 ] = 6
laFileStru[ 2 , 4 ] = 0

laFileStru[ 3 , 1 ] = 'VIA'
laFileStru[ 3 , 2 ] = 'C'
laFileStru[ 3 , 3 ] = 5
laFileStru[ 3 , 4 ] = 0

laFileStru[ 4 , 1 ] = 'VESSEL'
laFileStru[ 4 , 2 ] = 'C'
laFileStru[ 4 , 3 ] = 12
laFileStru[ 4 , 4 ] = 0

laFileStru[ 5 , 1 ] = 'AIRWAYB'
laFileStru[ 5 , 2 ] = 'C'
laFileStru[ 5 , 3 ] = 15
laFileStru[ 5 , 4 ] = 0

laFileStru[ 6 , 1 ] = 'INVOICE'
laFileStru[ 6 , 2 ] = 'C'
laFileStru[ 6 , 3 ] = 10
laFileStru[ 6 , 4 ] = 0

laFileStru[ 7 , 1 ] = 'NO_OF_CRTN'
laFileStru[ 7 , 2 ] = 'N'
laFileStru[ 7 , 3 ] = 6
laFileStru[ 7 , 4 ] = 0

laFileStru[ 8 , 1 ] = 'QTY'
laFileStru[ 8 , 2 ] = 'N'
laFileStru[ 8 , 3 ] = 7
laFileStru[ 8 , 4 ] = 0

laFileStru[ 9 , 1 ] = 'VALUE'
laFileStru[ 9 , 2 ] = 'N'
laFileStru[ 9 , 3 ] = 10
laFileStru[ 9 , 4 ] = 2

laFileStru[ 10, 1 ] = 'LC_NO'
laFileStru[ 10, 2 ] = 'C'
laFileStru[ 10, 3 ] = 6
laFileStru[ 10, 4 ] = 0

laFileStru[ 11, 1 ] = 'LC_PDATE'
laFileStru[ 11, 2 ] = 'D'
laFileStru[ 11, 3 ] = 10
laFileStru[ 11, 4 ] = 0

laFileStru[ 12 , 1 ] = 'LC_PAMNT'
laFileStru[ 12 , 2 ] = 'N'
laFileStru[ 12 , 3 ] = 13
laFileStru[ 12 , 4 ] = 2

laFileStru[ 13 , 1 ] = 'SHIP_DATE'
laFileStru[ 13 , 2 ] = 'D'
laFileStru[ 13 , 3 ] = 10
laFileStru[ 13 , 4 ] = 0

laFileStru[ 14 , 1 ] = 'ETA_DATE'
laFileStru[ 14 , 2 ] = 'D'
laFileStru[ 14 , 3 ] = 10
laFileStru[ 14 , 4 ] = 0

laFileStru[ 15 , 1 ] = 'WH_DATE'
laFileStru[ 15 , 2 ] = 'D'
laFileStru[ 15 , 3 ] = 10
laFileStru[ 15 , 4 ] = 0

laFileStru[ 16 , 1 ] = 'CUST_CODE'
laFileStru[ 16 , 2 ] = 'C'
laFileStru[ 16 , 3 ] = 6
laFileStru[ 16 , 4 ] = 0

laFileStru[ 17 , 1 ] = 'CUST_DDATE'
laFileStru[ 17 , 2 ] = 'D'
laFileStru[ 17 , 3 ] = 10
laFileStru[ 17 , 4 ] = 0

CREATE TABLE (gcWorkDir+lcShpTmp) FROM ARRAY laFileStru
INDEX ON SHIPNO+PO TAG (lcShpTmp) OF (gcWorkDir+lcShpTmp)

CREAT CURSOR lcShPo (SHIPNO C(6), PO C(6))
INDEX ON (SHIPNO+PO) TAG lcShPo
*--END OF FUNCTION lfCrtTemp.

*!*************************************************************
*! Name        : lfSetRela
*! Developer   : NADER NABIL (NNA)
*! Date        : 08/29/2004
*! Purpose     : Set relations between the Report Files
*!*************************************************************
*! Called from : PoShStNk.Prg
*!*************************************************************
*! Example     : = lfSetRela()
*!*************************************************************
FUNCTION lfSetRela

SELECT SHPMTHDR
  SET RELATION TO ShipNo INTO POSLN ADDITIVE
  SET RELATION TO ShipNo INTO SHPCRTMF ADDITIVE
SELECT POSLN
  SET RELATION TO cStyType + Po INTO POSHDR ADDITIVE
SELECT POSHDR
  SET RELATION TO 'S'+ POSHDR.clcno INTO Lcjournl ADDITIVE
  SET RELATION TO '2'+ POSHDR.po INTO Cutpick ADDITIVE
SELECT CUTPICK
  SET RELATION TO 'O'+CUTPICK.Order INTO ORDHDR ADDITIVE

*--END OF FUNCTION lfSetRela.
*!*************************************************************
*! Name        : lfColData
*! Developer   : NADER NABIL (NNA)
*! Date        : 08/29/2004
*! Purpose     : Collect data for the Report
*!*************************************************************
*! Called from : PoShStNk.Prg
*!*************************************************************
*! Example     : = lfColData()
*!*************************************************************
FUNCTION lfColData
PRIVATE lnTotQty , lnTotVal ,llLcPAmnt , lcShipNo , lcShipVia , lnCartons
STORE 0   TO lnTotQty , lnTotVal , lnCartons 
STORE .F. TO llLcPAmnt
STORE ''  TO lcShipNo , lcShipVia , lcPoNo , lcCrtNo

SELECT SHPMTHDR
SCAN FOR &lcRpExp
  IF SEEK(SHPMTHDR.SHIPNO,'POSLN')
    SELECT POSLN
    SCAN REST WHILE shipno+cstytype+po+style+STR(lineno,6)+trancd = SHPMTHDR.SHIPNO
      IF !(lcPoNo==POSHDR.PO)
        lCPoNo = POSHDR.PO
        SELECT lcShPo
        APPEND BLANK
        REPLACE lcShPo.SHIPNO WITH SHPMTHDR.SHIPNO , lcShPo.PO WITH POSLN.PO
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

SELECT lcShPo
IF RECCOUNT() > 0
  SCAN
    SELECT SHPMTHDR
    = SEEK(lcShPo.SHIPNO)
    IF !(lcShipNo == SHPMTHDR.SHIPNO)
      lcShipNo  = SHPMTHDR.SHIPNO
      lcShipVia = gfCodDes(SHPMTHDR.ShipVia , 'SHIPVIA')
      STORE .F. TO llLcPAmnt
    ENDIF
    SELECT (lcShpTmp)
    *-- Get Needed information from the SHPMTHDR File
    IF !SEEK(LCShPO.SHIPNO + LCSHPO.PO)
      APPEND BLANK
      REPLACE &lcShpTmp..SHIPNO    WITH SHPMTHDR.SHIPNO              ,;
              &lcShpTmp..PO        WITH LCSHPO.PO                    ,;
              &lcShpTmp..VIA       WITH SUBSTR(lcShipVia,1,5)        ,;
              &lcShpTmp..VESSEL    WITH SUBST(SHPMTHDR.CVESSEL,1,12) ,;              
              &lcShpTmp..AIRWAYB   WITH SHPMTHDR.AIRWAYB             ,;              
              &lcShpTmp..INVOICE   WITH SHPMTHDR.CSHIP_ADV           ,;              
              &lcShpTmp..SHIP_DATE WITH SHPMTHDR.ENTERED             ,;              
              &lcShpTmp..ETA_DATE  WITH SHPMTHDR.ETA                 ,;              
              &lcShpTmp..WH_DATE   WITH SHPMTHDR.DWAREHOUS              
    ENDIF

    *-- Get the Total POSLN Qty

    IF SEEK(LCShPO.SHIPNO + 'P' + LCSHPO.PO,'POSLN')
      SELECT POSLN
      SCAN REST WHILE shipno+cstytype+po+style+STR(lineno,6)+trancd = ;
                      LCShPO.SHIPNO + 'P' + LCSHPO.PO
        lnTotQty = lnTotQty + POSLN.TotQty
        lnTotVal = lnTotVal + (POSLN.TotQty * POSLN.NCOST1)
      ENDSCAN
      SELECT (lcShpTmp)
      IF SEEK(LCShPO.SHIPNO + LCSHPO.PO)
        REPLACE &lcShpTmp..QTY        WITH lnTotQty , VALUE WITH lnTotVal           
        IF SEEK(LCShPO.SHIPNO + 'P' + LCSHPO.PO,'POSLN')
          REPLACE &lcShpTmp..CUST_CODE  WITH POSHDR.CDIVISION                          ,;        
                  &lcShpTmp..LC_NO      WITH IIF(!EMPTY(POSHDR.CLCNO),POSHDR.CLCNO,'')
        ENDIF
      ENDIF      
      lnTotQty = 0
      lnTotVal = 0
    ENDIF
*********************************************    
   *-- Get Needed information from the lcJournl File (LC Paid Date , LC Paid Amount)
   IF SEEK('S' + POSHDR.CLCNO , 'LCJOURNL')
     SELECT LCJOURNL
     SCAN 
       IF CLCTRNTYP = '5' .AND. CSHIPNO = SHPMTHDR.SHIPNO
         SELECT (lcShpTmp)
         IF SEEK(LCShPO.SHIPNO + LCSHPO.PO)
           REPLACE &lcShpTmp..LC_PDate   WITH LCJOURNL.DTRANDATE
           IF !llLcPAmnt
             REPLACE &lcShpTmp..LC_PAMNT WITH LCJOURNL.NTRNAMUNT
             STORE .T. TO llLcPAmnt
           ENDIF
         ENDIF
       ENDIF
     ENDSCAN
   ENDIF         
*********************************************    

   *-- Get No Of Cartons from SHPCRTMF File
   IF SEEK(LCShPO.SHIPNO ,'SHPCRTMF')
     SELECT SHPCRTMF
     SCAN REST WHILE SHPCRTMF.CSHIPNO = LCSHPO.SHIPNO ;
               FOR SHPCRTMF.PO = LCSHPO.PO
       IF !(cCartonNo == lcCrtNo)
         lcCrtNo = cCartonNo
         lnCartons = lnCartons + 1
       ENDIF
     ENDSCAN
     SELECT (lcShpTmp)
     IF SEEK(LCShPO.SHIPNO + LCSHPO.PO)
       REPLACE NO_OF_CRTN WITH lnCartons
     ENDIF       
     lnCartons = 0
   ENDIF
*********************************************    
   *-- Get [Cust Del Date ] from the ORDHDR File

   IF SEEK('2'+POSHDR.PO,'CUTPICK')
     SELECT ORDHDR
     IF SEEK('O'+CUTPICK.ORDER)
       ldComplete = ORDHDR.COMPLETE
       SELECT (lcShpTmp)
       IF SEEK(LCShPO.SHIPNO + LCSHPO.PO)
         REPLACE &lcShpTmp..CUST_DDATE WITH ldComplete 
       ENDIF       
     ENDIF
   ENDIF
  ENDSCAN
ENDIF
*--END OF FUNCTION lfColData.

*!*************************************************************
*! Name        : lfwRepWhen
*! Developer   : NADER NABIL (NNA)
*! Date        : 08/29/2004
*! Purpose     : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
DECLARE laRpSource[3],laRpTarget[1]

lnStatus = lcRpStatus

STORE 'Open'      TO laRpSource[1]
STORE 'Completed' TO laRpSource[2]
STORE 'Canceled'  TO laRpSource[3]

lcRpStatus = ''

lcOldVal = SPACE(01)
IF !USED('SYCCOMP')
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
ENDIF
=SEEK(gcAct_Comp)

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : NADER NABIL (NNA)
*! Date      : 08/29/2004
*! Purpose   : Valid function for STSTUS Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************

FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])

  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
      IIF(laRpTarget[lnI] = 'Canceled','X',;
      IIF(laRpTarget[lnI] = 'Complete','C','')))

  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length
IF LEN(lcOldStat) != LEN(lcRpStatus)
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF

*!*************************************************************
*! Name      : lfvShpNo
*! Developer : NADER NABIL (NNA)
*! Date      : 08/29/2004
*! Purpose   : Valid function for Shipment #
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfvShpNo
PRIVATE lcVar , lcObj , laTemp,lcBrowFields

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))

lcPrevAl = SELECT()
STORE '' TO lcBrowCond

DECLARE laTemp[1]
SELECT SHPMTHDR
SET ORDER TO Shpmthdr
DIMENSION laTemp[1]

IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'SHPMTHDR'))
  lcBrFields = "SHIPNO    :R :H='Shipment #',"+;
  "lcSt = IIF(STATUS='O','Open',IIF(STATUS='X','Cancelled','Completed')) :R :H='Status',"+;
  "ENTERED   :R :H='Entered'      ,"+;
  "ETA       :R :H='E.T.A.'       ,"+;
  "TOTQTY    :R :H='TOTQTY'       ,"+;
  "RECV_STK  :R :H='Received'     ,"+;
  "RECV_DAM  :R :H='Damaged'      ,"+;
  "CVESSEL   :R :H='VESSEL'      ,"+;
  "RECV_CAN  :R :H='Cancelled' "

  lNThing    = gfBrows(lcBrowCond,'SHIPNO','laTemp')
ELSE
  laTemp=lcObj
ENDIF


IF !EMPTY(laTemp[1])
  lcObj = laTemp[1]
ELSE
  lcObj = ''
ENDIF

&lcVar = lcObj      && Update the field
SELECT (lcPrevAl)


