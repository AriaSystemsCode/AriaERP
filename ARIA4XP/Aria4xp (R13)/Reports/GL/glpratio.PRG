*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLPRATIO.PRG
*:  Module      : General Ledger
*:  Desc.       : Performance ratios
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303238,1
*:************************************************************************
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#include r:\aria4xp\reports\gl\glpratio.H
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

  *** Save escape setting
  lcSaveDel = SET('DELETE')
  lcSaveEsc = SET('ESCAP')
  lcSaveOnEs = ON('ESCAPE')
 
  SET ESCAP ON
  ON ESCAP DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
      
  *** Activate the system select therom.
  SET TALK ON

* N000682 ,1 Thabet Handle globalization issues [Start]  
  
    *WAIT 'Collecting data...' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collecting_data,oAriaApplication.GetHeaderText("LANG_Collecting_data",AHEADERFILE)) WINDOW NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]
  
  ***   Create select  statment
DIMENSION laRatio[21],laRatio45[3],laPTDNI[1]
STORE 000000.000 TO laRatio,laRatio45,laPTDNI
*E303238,1 TMI 10/02/2012 set comatability to fox2.6 to get the select statement correctly
SET ENGINEBEHAVIOR 70

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
ELSE  
  lcPYear=''
  lcPperiod=''
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

SELECT FSPRD

SET ORDER TO TAG COMFYRPRDI
lnCurDays=IIF( SEEK(lcRpYear+lcRpPeriod),dfsppendt-dfsppbgdt,0)
lnPriDays=IIF( SEEK(lcPYear+lcPPeriod),dfsppendt-dfsppbgdt,0)
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
*E303238,1 TMI 10/02/2012 [Start] restore compatability
SET ENGINEBEHAVIOR 90

DO gfDispRe WITH EVAL('lcRpForm')

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
IF SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP')
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

loFld = loOgScroll.ActiveControl

DECLARE laRpRetFld(2)
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+;
                '",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty
  
  IF NOT USED("FSPRD") 
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ELSE
    SELECT FSPRD
    SET ORDER TO TAG comfyrprdi
  ENDIF
  
  *** Search for the current company+year+Prd
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)        
    =gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISYEAR,oAriaApplication.GetHeaderText("LANG_FISYEAR",AHEADERFILE)),.F.)    
    * N000682 ,1 Thabet Handle globalization issues [End]
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = loFld.OldValue
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFisFyear
  ENDIF

IF llUsedBefo       && .F.- this file used by the system
  
  USE IN FSPRD
  
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

loFld = loOgScroll.ActiveControl

DECLARE laRpRetFld(2)
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year ,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
               '",CFspprdid:H="'+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+;
               '",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty
  
  IF NOT USED("FSPRD") 
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ELSE
    SELECT FSPRD
    SET ORDER TO TAG comfyrprdi
  ENDIF
  
  
  lcYearVal = 'lcRpYear'
  *** Search for the current company+year+Prd
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows(IIF(EMPTY(&lcOtherFld),'',[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)    
    =gfBrows(IIF(EMPTY(&lcOtherFld),'',[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISYEAR,oAriaApplication.GetHeaderText("LANG_FISYEAR",AHEADERFILE)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [End]  
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = loFld.OldValue
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFSPPRDID
  ENDIF

IF llUsedBefo       && .F.- this file used by the system
  
  USE IN FSPRD
  
ENDIF
SELECT (lcOldAlias)  
RETURN 

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : When function
************************************************************
FUNCTION lfRepWhen

*- End of lfRepWhen.