*:***************************************************************************
*: Program file  : POWIPCOP   200676
*: Program desc. : Custom Import WIP for Copy Cats
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : Mariam Mazhar (MMT)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POWIPCOP
*:**************************************************************************
loOgScroll.cCRorientation = 'L'
IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data........" NOWAIT 
  IF !USED(lcTmpFile) OR (RECCOUNT(lcTmpFile) > 0)
   IF USED(lcTmpFile)
     USE IN (lcTmpFile)
   ENDIF  
   =lfWIPFile()    
 ENDIF 
 lfCollect()
ENDIF 

*!*	SELECT POSHDR
*!*	IF OCCURS(".STATUS",lcRpExp) = 0
*!*	  lcRpExp = lcRpExp + " AND POSHDR.STATUS $ 'OH'"
*!*	ENDIF  
*!*	SCAN FOR &lcRpExp
*!*	  =lfGetMem()  && Fill Memory variables with its corresponding values.
*!*	ENDSCAN

SELECT (lcTmpFile)
LOCATE

DO gfDispRe WITH EVAL('lcRpForm')
*!*************************************************************
*! Name      : lfWIPFile
*! Developer : MARIAM MAZHAR(MMT)
*! Date      : 11/09/2006
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfWIPFile()
*!*************************************************************
*
FUNCTION lfWIPFile

DIMENSION laFileStru[13,4]
laFileStru[1,1] = 'PO'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 6
laFileStru[1,4] = 0

laFileStru[2,1] = 'Account'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 6
laFileStru[2,4] = 0

laFileStru[3,1] = 'cVendCode'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 8
laFileStru[3,4] = 0

laFileStru[4,1] = 'cVenComp'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 30
laFileStru[4,4] = 0

laFileStru[5,1] = 'Confo'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 6
laFileStru[5,4] = 0

laFileStru[6,1] = 'cancelled'
laFileStru[6,2] = 'D'
laFileStru[6,3] = 8
laFileStru[6,4] = 0

laFileStru[7,1] = 'complete'
laFileStru[7,2] = 'D'
laFileStru[7,3] = 8
laFileStru[7,4] = 0

laFileStru[8,1] = 'Style'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 19
laFileStru[8,4] = 0


laFileStru[9,1] = 'Units'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 11
laFileStru[9,4] = 0

laFileStru[10,1] = 'InTransit'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 11
laFileStru[10,4] = 0


laFileStru[11,1] = 'ETA'
laFileStru[11,2] = 'D'
laFileStru[11,3] = 8
laFileStru[11,4] = 0

laFileStru[12,1] = 'Period'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 3
laFileStru[12,4] = 0

laFileStru[13,1] = 'Notes'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 30
laFileStru[13,4] = 0

=gfCrtTmp(lcTmpFile,@laFileStru,'cVendCode+ALLTRIM(STR(YEAR(Complete)))+Period',lcTmpFile,.T.)

*-- end of lfWIPFile.
*!*************************************************************
*! Name      : lfCollect
*! Developer : MARIAM MAZHAR(MMT)
*! Date      : 11/09/2006
*! Purpose   : Collect data
*!*************************************************************
FUNCTION lfCollect
IF TYPE('loDBFPOSHDR') <> 'O'
  loDBFPOSHDR = CreateObject("RemoteTable",'POSHDR','POSHDR','POSHDR',SET("DATASESSION"))
ENDIF

IF TYPE('loDBFPOSLN') <> 'O'
  loDBFPOSLN = CreateObject("RemoteTable",'POSLN','POSLN','POSLN',SET("DATASESSION"))
ENDIF

IF TYPE('loDBFCUTPICK') <> 'O'
  loDBFCUTPICK = CreateObject("RemoteTable",'CUTPICK','CUTPICK','CUTPICK',SET("DATASESSION"))
ENDIF

IF TYPE('loDBFORDHDR') <> 'O'
  loDBFORDHDR = CreateObject("RemoteTable",'ORDHDR','ORDHDR','ORDHDR',SET("DATASESSION"))
ENDIF

IF TYPE('loDBFAPVENDOR') <> 'O'
  loDBFAPVENDOR = CreateObject("RemoteTable",'APVENDOR','VENCODE','APVENDOR',SET("DATASESSION"))
ENDIF

IF TYPE('loDBFSHPMTHDR') <> 'O'
  loDBFSHPMTHDR = CreateObject("RemoteTable",'SHPMTHDR','SHPMTHDR','SHPMTHDR',SET("DATASESSION"))
ENDIF



lcVendFile = ''
llUseVend  = .F.
LowDate  = {}
HighDate = {}

*Date filter
lnDatePos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'POSHDR.COMPLETE'),1)
IF lnDatePos > 0
  LowDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],1,10))
  HighDate = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos ,6],12,21))
ENDIF 

lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'POSHDR.VENDOR'),1)
IF lnPosition > 0
  lcVendFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseVend  = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
ENDIF
IF llUseVend  
  SELECT(lcVendFile)
  LOCATE 
  IF EOF()
    llUseVend = .F.
  ENDIF 
ENDIF 

lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'POSHDR.STATUS'),1)
IF lnPosition > 0
  lcStatus = IIF(!EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6]),STRTRAN(LOOGSCROLL.laOGFxFlt[lnPosition,6],'|',"','"),"")
  lcStatus = IIF(!EMPTY(lcStatus),"INLIST(POSHDR.STATUS,'"+lcStatus+"')","")
ENDIF


IF llUseVend  
  SELECT(lcVendFile)
  SCAN 
    IF loDBFPOSHDR.SEEK(&lcVendFile..CVENDCODE+'PP','poshdrv')
      SELECT POSHDR
      SCAN REST WHILE  vendor+cbusdocu+ cstytype+po = &lcVendFile..CVENDCODE+'PP';
           FOR IIF(!EMPTY(LowDate) AND !EMPTY(HighDate),BETWEEN(POSHDR.COMPLETE,LowDate,HighDate),.T.) AND ;
           IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),INLIST(Poshdr.Status,'O','H'))
        =lfGetMem()  && Fill Memory variables with its corresponding values.
      ENDSCAN 
    ENDIF 
  ENDSCAN 
ELSE
  IF !EMPTY(LowDate) AND !EMPTY(HighDate)
    IF loDBFPOSHDR.Sqlrun("Select * from poshdr where cbusdocu = 'P' and Cstytype = 'P' and POSHDR.COMPLETE between '"+DTOC(LowDate)+"' And '"+DTOC(HighDate)+"'")
      SELECT POSHDR
      SCAN  FOR IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),INLIST(Poshdr.Status,'O','H'))
        =lfGetMem()  && Fill Memory variables with its corresponding values.
      ENDSCAN  
    ENDIF 
  ELSE
    IF loDBFPOSHDR.SEEK('PP','poshdr')
      SELECT POSHDR
      SCAN  FOR IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),INLIST(Poshdr.Status,'O','H'))
        =lfGetMem()  && Fill Memory variables with its corresponding values.
      ENDSCAN  
    ENDIF 
  ENDIF 
ENDIF 
*!*************************************************************
*! Name      : lfGetMem
*! Developer : MARIAM MAZHAR(MMT)
*! Date      : 11/09/2006
*! Purpose   : Fill Tempfile memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMem()
*!*************************************************************
*
FUNCTION lfGetMem
PRIVATE lnUnits 
STORE 0 TO lnUnits

*-- get Needed data from CutPik File and OrdHdr 
IF loDBFCUTPICK.SEEK('2'+PosHdr.Po,'CUTPICK') And loDBFORDHDR.SEEK('O'+CUTPICK.Order,'ORDHDR')
  m.Account   = OrdHdr.Account
  m.Confo     = ORDHDR.REP2
  m.cancelled = OrdHdr.Complete
ELSE
  m.Account   = " "
  m.Confo     = " "
  m.cancelled = {}
ENDIF

*-- get Needed data from PosHdr File and Apvendor
m.cVendCode = PosHdr.Vendor
m.PO        = PosHdr.Po

M.Notes = IIF(lcRpPrNot = "I" , ALLTRIM(POSHDR.INSURANCE) , IIF(lcRpPrNot = "B" , ALLTRIM(POSHDR.BUYER) , SPACE(0)))


IF loDBFAPVENDOR.SEEK(PosHdr.Vendor)
  m.cVenComp = Apvendor.cVenComp
ENDIF

m.complete = PosHdr.complete
m.Period   = IIF(MONTH(PosHdr.complete) > 9,"B"+ALLTRIM(STR(MONTH(PosHdr.complete))),"A"+ALLTRIM(STR(MONTH(PosHdr.complete))))

IF loDBFPOSLN.SEEK(PosHdr.cBUSDOCU+PosHdr.cStyType+PosHdr.Po,'POSLN')
  m.Style    =PosLn.Style
  m.InTransit = 0
  *--Posln
  SELECT POSLN
  SCAN REST WHILE cbusdocu+ cstytype+ po = PosHdr.cBUSDOCU+PosHdr.cStyType+PosHdr.Po
    DO CASE
       CASE trancd = '1' 
         lnUnits = lnUnits + TotQty
       CASE trancd = '2' 
         m.InTransit = m.InTransit + TotQty
       CASE trancd = '3' 
         m.InTransit = m.InTransit + TotQty
         IF !EMPTY(ShipNo) AND loDBFSHPMTHDR.SEEK(PosHdr.cBUSDOCU+PosHdr.cStyType+ShipNo,'SHPMTHDR')
           m.ETA = SHPMTHDR.ETA
         ENDIF
       CASE trancd = '4' 
         lnUnits = lnUnits - TotQty
       CASE trancd = '5' 
         lnUnits = lnUnits - TotQty
    ENDCASE
  ENDSCAN
  m.Units = lnUnits
ENDIF
INSERT INTO (lcTmpFile) FROM MEMVAR
 
*-- end of lfGetMem.
*!*************************************************************
*! Name      : lfGetMonth
*! Developer : MARIAM MAZHAR(MMT)
*! Date      : 11/09/2006
*! Purpose   : Get Month Data
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMonth()
*!*************************************************************
*
FUNCTION lfGetMonth

DO CASE
 CASE Period = 'A1 '
      lcRPMonth = 'JANUARY'
 CASE Period = 'A2 '
      lcRPMonth = 'FEBRUARY'
 CASE Period = 'A3 ' 
      lcRPMonth = 'MARCH'
 CASE Period = 'A4 '
      lcRPMonth = 'APRIL'
 CASE Period = 'A5 '
      lcRPMonth = 'MAY'
 CASE Period = 'A6 '
      lcRPMonth = 'JUNE'
 CASE Period = 'A7 '
      lcRPMonth = 'JULY'
 CASE Period = 'A8 '
      lcRPMonth = 'AUGUST'
 CASE Period = 'A9 '
      lcRPMonth = 'SEPTEMBER'
 CASE Period = 'B10'
      lcRPMonth = 'OCTOBER'
 CASE Period = 'B11'
      lcRPMonth = 'NOVEMBER'
 CASE Period = 'B12'
      lcRPMonth = 'DECEMBER'
ENDCASE