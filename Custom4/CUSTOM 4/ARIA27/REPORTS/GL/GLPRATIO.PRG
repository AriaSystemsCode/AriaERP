*:************************************************************************
*:
*: Procedure file: GLPRATIO.RPR
*:               : Performance ratios
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:25/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B605850,1 ASH 04/11/2002 Fix the bug of wrong browsing the periods.
*:************************************************************************

*** Report Setup
   
*IF llOGFltCh OR glEscPrs

  *** Save escape setting
  lcSaveDel = SET('DELETE')
  lcSaveEsc = SET('ESCAP')
  lcSaveOnEs = ON('ESCAPE')
 * SET DELETE OFF
  SET ESCAP ON
  ON ESCAP DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
      
  *** Activate the system select therom.
  SET TALK ON
  
  WAIT 'Collecting data...' WINDOW NOWAIT
  
  ***   Create select  statment
DIMENSION laRatio[21],laRatio45[3],laPTDNI[1]
STORE 000000.000 TO laRatio,laRatio45,laPTDNI

SELECT SUM(IIF(CSEGRATIO="10",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="11",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="12",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="13",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="14",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="15",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="16",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="20",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="21",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="22",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="23",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="24",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="30",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="40",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="50",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="60",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="61",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="70",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="71",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="80",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;  
  SUM(IIF(CSEGRATIO="31",NACBOPBAL+NACBPTDDR-NACBPTDCR,0));  
 FROM GLACBALS, GLACCHAR;
 WHERE GLACBALS.CACCTCODE = GLACCHAR.CACCTCODE;
   AND CFISFYEAR+CFSPPRDID = lcRPYear+lcRPperiod;
   AND CSEGRATIO IN ("10","11","12","13","14","15","16","20","21","22","23","24",;
   "30","40","50","60","61","70","71","80");
   INTO ARRAY laRatio

SELECT SUM(NACBPTDCR-NACBPTDDR);
 FROM GLACBALS, GLACCHAR;
 WHERE GLACBALS.CACCTCODE = GLACCHAR.CACCTCODE;
   AND CFISFYEAR+CFSPPRDID = lcRPYear+lcRPperiod;
   AND LEFT(GLACCHAR.CTYPECODE,1) IN ('S','I','T','E','C');
   INTO ARRAY laPTDN

SELECT GLACBALS
SET ORDER TO TAG FISFYEAR
IF SEEK(lcRPYear+lcRPperiod)
  SKIP -1
  IF !BOF()
    lcPYear=cfisfyear
    lcPperiod=cfspprdid  
  ELSE
    lcPYear=''
    lcPperiod=''
  ENDIF
  *B605850,1 ASH 04/11/2002 (Begin) Fix the bug of variable not found.
ELSE  
  lcPYear=''
  lcPperiod=''
  *B605850,1 ASH 04/11/2002 (End)
ENDIF
SET ORDER TO
IF !EMPTY(lcPYear) AND !EMPTY(lcPYear)
SELECT SUM(IIF(CSEGRATIO="13",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="40",NACBOPBAL+NACBPTDDR-NACBPTDCR,0)),;
  SUM(IIF(CSEGRATIO="50",NACBOPBAL+NACBPTDDR-NACBPTDCR,0));
 FROM GLACBALS, GLACCHAR;
 WHERE GLACBALS.CACCTCODE = GLACCHAR.CACCTCODE;
   AND CFISFYEAR+CFSPPRDID = lcPYear+lcPPeriod;
   AND CSEGRATIO IN ("40","50");
   INTO ARRAY laRatio45
ENDIF

*HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
*SELECT SYCFSPRD
SELECT FSPRD
*HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]

SET ORDER TO TAG COMFYRPRDI
*B605850,1 ASH 04/11/2002 (Begin) Remove the company code from the seek expr.
*lnCurDays=IIF( SEEK(gcAct_comp+lcRpYear+lcRpPeriod),dfsppendt-dfsppbgdt,0)
*lnPriDays=IIF( SEEK(gcAct_comp+lcPYear+lcPPeriod),dfsppendt-dfsppbgdt,0)
lnCurDays=IIF( SEEK(lcRpYear+lcRpPeriod),dfsppendt-dfsppbgdt,0)
lnPriDays=IIF( SEEK(lcPYear+lcPPeriod),dfsppendt-dfsppbgdt,0)
*B605850,1 ASH 04/11/2002 (End)


lnAvgAR    = laRatio[2]/laRatio45[2]
lnDaylInv  = laRatio45[1]/laRatio45[3]
lnInvTurn  = laRatio[15]/(laRatio[4]+laRatio[5])
lnArTurn   = laRatio[14]/laRatio[2]
lnFixTurn  = laRatio[14]/laRatio[6]
lnAssTurn  = laRatio[14]/(laRatio[1]+laRatio[2]+laRatio[3]+laRatio[4]+;
                          laRatio[5]+laRatio[6]+laRatio[7])
lnDebt     = (laRatio[8]+laRatio[9]+laRatio[10]+laRatio[11]+laRatio[12])/;
             (laRatio[1]+laRatio[2]+laRatio[3]+laRatio[4]+laRatio[5];
             +laRatio[6]+laRatio[7])             
lnCurrent = (laRatio[1]+laRatio[2]+laRatio[3]+laRatio[4]+laRatio[5]+;
             laRatio[7])/(laRatio[8]+laRatio[9])
lnAcid    = (laRatio[1]+laRatio[2]+laRatio[3])/(laRatio[8]+laRatio[9])
lnGrosPft = (laRatio[14]-laRatio[15])/laRatio[14]
lnDayScycl= lnPriDays * (lnAvgAR + lnDaylInv) 
lnDaySturn= (laRatio[4]/laRatio[15])  * lnCurDays
lnDaySar  = lnAvgAR *  lnCurDays
lnDaySyr  = lnDaySar + lnDaySturn
lnCollect = laRatio[2] / (laRatio[14]/lnDaySyr)
lnTimeInt = ((laRatio[21]+laPTDNI[1])-(laRatio[18]+laRatio[19]+laRatio[20]))/;
             (laRatio[18]+laRatio[19])  
lnFixChgs = (laRatio[9]+laRatio[21]+laPTDNI[1]-(laRatio[10]+laRatio[18]+laRatio[19]))/;
            (laRatio[18]+laRatio[10])      
lnDbtEqut = (laRatio[8]+laRatio[9]+laRatio[10]+laRatio[11]) / ;
            (laRatio[12]+laRatio[13]+laRatio[21]+laPTDNI[1])
lnRetEqut = (laRatio[21]+laPTDNI[1])/(laRatio[13]+laRatio[21]+laPTDNI[1])
lnNetPrft = (laRatio[21]+laPTDNI[1]) /laRatio[14]
lnRetAsst = (laRatio[21]+laPTDNI[1]) /(laRatio[1]+laRatio[2]+laRatio[3];
             +laRatio[4]+laRatio[5]+laRatio[6]+laRatio[7])

  *** Restore all enviroment 
  WAIT CLEAR
  SET TALK OFF
  SET DELETE &lcSaveDel
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  *** before that check if press Escape or not
*ENDIF 
DO gfDispRe WITH EVAL('lcRpForm')
glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

*!************************************************************************
*!
*!      FUNCTION lfGetCurYr
*!
*!************************************************************************
*
FUNCTION lfGetCurYr

llCompUsd=.f.
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF
SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(gcAct_Comp,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
ENDIF
IF llCompUsd
  USE IN SYCCOMP
ENDIF
RETURN lcRetVal

*!*************************************************************************
*!
*!              Function lfvOneYear
*!
*!*************************************************************************
* This function valid the only one year
FUNCTION lfvOneYear
PARAMETER lcOtherFld

DECLARE laRpRetFld(2)
lcRpCurFld      = VARREAD()
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF NOT USED("SYCFSPRD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
  *  llUsedBefo = .T.
  *ELSE
  *  SELECT SYCFSPRD
  *  SET ORDER TO TAG comfyrprdi
  *ENDIF
  IF NOT USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ELSE
    SELECT FSPRD
    SET ORDER TO TAG comfyrprdi
  ENDIF
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  *** Search for the current company+year+Prd
  *B605850,1 ASH 04/11/2002 (Begin) Remove the company code from the seek expr.
  *IF '?' $ &lcRpCurFld. OR !SEEK(gcAct_comp+&lcRpCurFld)
    *=gfBrows([gcAct_comp],'CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    =gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)    
  *B605850,1 ASH 04/11/2002 (End)
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFisFyear
  ENDIF
  SHOW GET (lcRpCurFld)  
  SHOW GET (lcOtherFld)  

IF llUsedBefo       && .F.- this file used by the system
  
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
  *USE IN SYCFSPRD
  USE IN FSPRD
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
  
ENDIF
SELECT (lcOldAlias)  
RETURN 

*!*************************************************************************
*!
*!              Function lfvOnePrd
*!
*!*************************************************************************
* This function valid the only one period
FUNCTION lfvOnePrd
PARAMETER lcOtherFld

DECLARE laRpRetFld(2)
lcRpCurFld      = VARREAD()
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF NOT USED("SYCFSPRD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
  *  llUsedBefo = .T.
  *ELSE
  *  SELECT SYCFSPRD
  *  SET ORDER TO TAG comfyrprdi
  *ENDIF
  IF NOT USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ELSE
    SELECT FSPRD
    SET ORDER TO TAG comfyrprdi
  ENDIF
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  lcYearVal = 'lcRpYear'
  *** Search for the current company+year+Prd
  *B605850,1 ASH 04/11/2002 (Begin) Remove the company code from the seek expr.
  *IF '?' $ &lcRpCurFld. OR !SEEK(gcAct_comp+&lcYearVal+&lcRpCurFld)
    *=gfBrows(IIF(EMPTY(&lcOtherFld),[gcAct_comp],[gcAct_comp+&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    =gfBrows(IIF(EMPTY(&lcOtherFld),'',[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
  *B605850,1 ASH 04/11/2002 (End)
    
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFSPPRDID
  ENDIF
  SHOW GET (lcRpCurFld)  
  SHOW GET (lcOtherFld)  

IF llUsedBefo       && .F.- this file used by the system
  
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
  *USE IN SYCFSPRD
  USE IN FSPRD
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
  
ENDIF
SELECT (lcOldAlias)  
RETURN 

*!************************************************************************
*!
*!      FUNCTION lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())

