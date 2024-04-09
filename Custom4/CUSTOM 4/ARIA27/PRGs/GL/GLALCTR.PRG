*:************************************************************************
*:
*: Procedure file: GLALCTR.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Renee Ezzat Ashamalla
*:      Copyright (c) 
*:  Last modified: 04/11/1994
*:
*:  Procs & Fncts
*:    PROCEDURE lpShow       *** local READ SHOW clause                 ***
*:    PROCEDURE lpSetWinds   *** handles switching between child        ***
*:                           *** windows                                ***
*:    PROCEDURE lpSavScr     *** local <  Save  > procedure             ***
*:    PROCEDURE lpDelScr     *** local < Delete > procedure             ***
*:    FUNCTION  lfTotals     *** called from Select_SQL command         ***
*:    FUNCTION  lfvData_2    *** Valid function of laData[2]  get field ***
*:    FUNCTION  lfvType      *** Valid function of radio button rbType  ***
*:    FUNCTION  lfvSrcJrnl   *** Valid function of popup puSrcJrnl      ***
*:                           *** (WINDOWS), or ibSrcJrnl (DOS)          ***
*:    FUNCTION  lswAlcDet    *** When  function of list lsAlcDet        ***
*:    FUNCTION  lfvAccCode   *** Valid function of cAcctCode  get field ***
*:    FUNCTION  lfvDebit     *** Valid function of lnDebit    get field ***
*:    FUNCTION  lfvredit     *** Valid function of lnCredit   get field ***
*:    FUNCTION  lfvAllocPr   *** Valid function of lnAllocPer get field ***
*:    FUNCTION  lfvNew       *** Valid function of push button pbNew    ***
*:    FUNCTION  lfvRem       *** Valid function of push button pbRem    ***
*:    FUNCTION  lfSrcJrnl    *** Source journal array preparation for   ***
*:                           *** popup pusrcjrnl(WINDOWS), or ibSrcJrnl ***
*:                           *** (DOS)                                  ***
*:                
*:      Documented  /  /   at   :  
*:************************************************************************
*B600381,1 Reham On 06/06/95
*B600381,1 1- Control the enabling & disabling of the <NEW> button in the
*B600381,1    different modes.
*B600381,1 2- Change the command from "LOCATE" to the next command to prevent
*B600381,1    changing the record no. if leaving the screen to another one &
*B600381,1    back again in the GLAUTHD
*B600381,1 3- Add {llNewDet} flag to know if there is new detail line with
*B600381,1    empty acc. to control disable & enable of the <New> button.
*B600381,1 4- Move some code from the SCX setup to prevent changing the data
*B600381,1    if leave the screen & back again & set the default before
*B600381,1    entering the screen.
*B600381,1 5- Fix the list title under windows to be alined with its obj.
*B600381,1 6- Prevent the disabling of the list in the view mode.

*B600385,1 Reham On 06/07/95
*B600385,1 In the key field, If coming from the browse to the select mode
*B600385,1 point to the current object which is the key field.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E301077,71 Walid (WAM) 03/07/99 Open GLTYPES with gfOpenFile() .
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField,laDefProc, laScrMode, laType

DECLARE laKeyField[2,4] , laTSrcJrnl[1,2] , laType[2,1]  , laAutDtFld[1,4]
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'TYPECODE'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'TYPECODE'
laKeyField[2,4] = 2

laDefProc[7]    = .F.         && use local Delete procedure 
laDefProc[9]    = .F.         && use local Save   procedure 

*** Variables in screen object file :
** laType     :  'Amounts/Percent' array [2,2] for 'Amounts/Percent' popup
** lcTDfRef   :  'On'
** lcTDfDesc  :  'Created by'
** lcTAllcPrc :  'allocated percentages' 
** lcTAllcTrs :  'allocation transaction|allocation transaction'
** lcTDrOrCr  :  'either debit or credit'  
** lcTAllPrcs :  'the allocation percentage'  
** lcTTotal1  :  'Total :'
** lcTTotal2  :  'Totals :'
** lcTDebit   :  'Debit'        --- Screen display
** lcTCredit  :  'Credit'       --- Screen display
** lcTAlloc   :  'Allocation %' --- Screen display
*** Variables declaration 
** lsAlcDet   :  List variable ( pointer )
** lcType     :  Initial type popup selection.
** lcSrcJrnl  :  Holds the selection of the S.J popup
** lcDSrcJrn  :  Default source journal string 
** lcOldWnTyp :  Initial window type ( Amounts,"A" )
** lcWinName  :  Initial window 
** lcTTotals  :                                 
** lcFieldStr :  List field string
** lc_TmpAlc  :  Temporary file name variable
** lcAStamp   :  Audit information of current record
** lcOldAcct  :  Old Account code
** lcAccDesc1 :  Corresponds to field GLAUTDT.cAccnlDes
** lcAccDesc2 :  Corresponds to field GLAUTDT.cAccnlDes
** lcCodeType :  Variable to hold the current account type 
** lcAutDtTg  :  Tag name of GLAUTDT upon entry.      
** lcAutDtEx  :  Tag expression of GLAUTDT current tag 
** lnTmpRcCnt :  Number of records in th list
** lnOldAlRec :  The previously selected record (position) 
** lnOldDr    :  Old debit value
** lnOldCr    :  Old credit value
** lnOldPer   :  Old percent value
** lnDebit    :  Variable to accept debit  value
** lnCredit   :  Variable to accept credit value
** lnAllocPer :  Variable to accept allocation percentage       
** lnTotdr    :  Variable to hold total debits
** lnTotcr    :  Variable to hold total credits (,percents)
** lnSrcJrnl  :       
** lnDSrcJrn  :  Default source journal array index
** lnAutDtFld :  Number of fields of 'GLAUTDT' file
** lnThermRec :  Thermometer record counter 

STORE "" TO  laType     , lcTDfRef   , lcTDfDesc  , lcTAllcPrc ,;
             lcTAllcTrs , lcTDrOrCr  , lcTAllPrcs , lcTTotal1  ,;
             lcTTotal2  , lcTDebit   , lcTCredit  , lcTAlloc   ,;
             lcType     , lcSrcJrnl  , lcDSrcJrn  , lcTTotals  ,;
             lcFieldStr , lc_TmpAlc  , lcAStamp   , lcOldAcct  ,;
             lcAccDesc1 , lcAccDesc2 , lcCodeType , lcTyp_Stat ,;
             lcAct_Stat , lcDeb_Stat , lcCrd_Stat , lcPer_Stat ,;
             lcNew_Stat , lcRem_Stat

*B600381,1 Reham On 06/06/95 Move this code from the SCX to prevent changing
*B600381,1 the data if leave the screen & back again & set the default before
*B600381,1 entering the screen.
laType[1,1] = "Amounts"
laType[2,1] = "Percent"

*** Variables declaration 
lsAlcDet   = 1   
lnPopLen   = 0
lcOldWnTyp = "A" 
lcWinName  = "GLALCCH1"
lcAcctCode = REPLICATE ("0",lnAcsSegSz)  

DO CASE
  CASE _DOS
    *** list totals string ( amount mode ) ***
    lcLsTotal1 = "IIF(lnTmpRcCnt>0,"+;
                     "SPACE(MAX(LEN(lcTTotal1),LEN(lcTTotal2))"+;
                     "-LEN(lcTTotal1))+"+;
                     "lcTTotals+' '+STR(lnTotDr,15,2)+' '+"+;
                     "STR(lnTotCr,15,2),SPACE(32+LEN(lcTTotals)))"
    *** list totals string ( percent mode ) ***
    lcLsTotal2 = "IIF(lnTmpRcCnt>0,"+;
                     "SPACE(25+MAX(LEN(lcTTotal1),LEN(lcTTotal2))"+;
                     "-LEN(lcTTotal2))+"+;
                     "lcTTotals+' '+STR(lnTotCr,6,2),"+;
                     "SPACE(34+LEN(lcTTotals)))" 
  CASE _WINDOWS
    *** list totals string ( amount mode ) ***        
    lcLsTotal1 = "IIF(lnTmpRcCnt > 0,"+;
                     "'Totals : '+STR(lnTotDr,15,2)+' '+"+;
                     "STR(lnTotCr,15,2),SPACE(40))"
    *** list totals string ( percent mode ) ***
    lcLsTotal2 = "IIF(lnTmpRcCnt > 0,"+;
                     "SPACE(1)+'Total : '+SPACE(25)+STR(lnTotCr,6,2),"+;
                     "SPACE(42))"
ENDCASE

lcLsTotal  = lcLsTotal1  
lcTTotals  = lcTTotal1

STORE '' TO lcAutDtTg  , lcAutDtEx

STORE 0  TO lnTmpRcCnt , lnOldAlRec , lnOldDr    , lnOldCr    ,;
            lnOldPer   , lnDebit    , lnCredit   , lnAllocPer ,;
            lnTotdr    , lnTotcr    , lnSrcJrnl  , lnDSrcJrn  ,;
            lnAutDtFld , lnThermRec

STORE .F.  TO llFromBton , llBrowse

*B600381,1 Reham On 06/06/95 This flag to know if there is new 
*B600381,1 detail line without an account.
llNewDet = .F.

puSrcJrnl  = 1

IF !gfSetup()    
  RETURN
ENDIF  
*E301077,71 [BEGIN]
=gfOpenFile(gcDataDir+'GLTYPES',gcDataDir+'Typecode','SH')
*E301077,71 [END..]
lnSpaces   = IIF(_DOS,7,1)
lcTypeDisp = IIF(_DOS,'ibType','puType')
lcCurrSign = '$'         && currency sign
lcUserName = gcUserName
*lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')
lcCurDate  = IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                 LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))

SELECT GLTYPES

GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting = .T.
  RETURN 
ENDIF
 
IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields TO laData BLANK
  laData[1]  = 'A'
  laData[6]  = REPLICATE ("0",lnAcsSegSz)  
  *** Create a name for the temporary file 
  lc_TmpAlc  = gfTempName()
  
  *** Prepare lcFieldStr for the list 
  DO CASE 
    CASE _DOS
      lcFieldStr = "SUBSTR(&lc_TmpAlc..cAcctcode,1,lnAcsSegSz)+' '+"+;
                   "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpAlc..cAcctcode,"+;
                   "GLACCHAR.cAcctcode,'ACCTCODE'),1,"+;
                   "IIF(laData[5]='A',40-lnAcsSegSz,60-lnAcsSegSz))+' '+"+;
                   "IIF(laData[5]='A',"+; 
                   "IIF(&lc_TmpAlc..cDrOrCr='D',"+;
                   "STR(&lc_TmpAlc..nAmount,15,2)+SPACE(12)+'0.00',"+;
                   "SPACE(11)+'0.00 '+STR(&lc_TmpAlc..nAmount,15,2)),"+;
                   "SPACE(5)+STR(&lc_TmpAlc..nAmount,6,2))"
    CASE _WINDOWS
      lcFieldStr = "SUBSTR(&lc_TmpAlc..cAcctcode,1,lnAcsSegSz)+' '+"+;
                   "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpAlc..cAcctcode,"+;
                   "GLACCHAR.cAcctcode,'ACCTCODE'),1,"+;
                   "IIF(laData[5]='A',30-lnAcsSegSz,46-lnAcsSegSz))+' '+"+;
                   "IIF(laData[5]='A',"+; 
                   "IIF(&lc_TmpAlc..cDrOrCr='D',"+;
                   "STR(&lc_TmpAlc..nAmount,15,2)+SPACE(12)+'0.00',"+;
                   "SPACE(11)+'0.00 '+STR(&lc_TmpAlc..nAmount,15,2)),"+;
                   "SPACE(9)+STR(&lc_TmpAlc..nAmount,6,2))"
  ENDCASE  

  SELECT GLAUTDT
  
  lcAutDtTg = SYS(22)
  
  SET ORDER TO TAG TYPCODACC
  
  *** Current tag expression is cAutType + cAutCode + cAcctCode
  lcAutDtEx = SYS(14,VAL(SYS(21)))
  
  *** Create the temporary file with the appropriate fields , 
  *** (in this case all fields of file "GLAUTDT" + 2 more fields
  *** for the record number and status('M'odify,'D'elete,'A'dd,'S'ame)

  =AFIELDS(laAutDtFld)
  lnAutDtFld = ALEN(laAutDtFld,1)
  DIMENSION laAutDtFld[lnAutDtFld+2,4]

  laAutDtFld[lnAutDtFld+1,1] = 'nRecNo'
  laAutDtFld[lnAutDtFld+1,2] = 'N'
  laAutDtFld[lnAutDtFld+1,3] = 10
  laAutDtFld[lnAutDtFld+1,4] = 0

  laAutDtFld[lnAutDtFld+2,1] = 'cStatus'
  laAutDtFld[lnAutDtFld+2,2] = 'C'  
  laAutDtFld[lnAutDtFld+2,3] = 1
  laAutDtFld[lnAutDtFld+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_TmpAlc;
      FROM ARRAY laAutDtFld

  *** Create source journal array for the source journals popup
  =lfSrcJrnl()

  *B600381,1 Reham On 06/06/95 Move this code from the SCX to set the 
  *B600381,1 default before entering the screen.
  lcType    = laType[1,1]
  laData[5] = 'A'
  laData[7] = lcSJ_Def
  lcSrcJrnl = lcDSrcJrn
ENDIF

*lcSrcJrnl  = lcDSrcJrn

SELECT GLAUTHD

SET FILTER TO
*** Filter on allocation transactions records only
SET FILTER TO cAutType="A"
*** Activate filter

*B600381,1 Reham On 06/06/95
*B600381,1 Change the command from "LOCATE" to the next command to prevent
*B600381,1 changing the record no. if leaving the screen to another one &
*B600381,1 back again in the GLAUTHD
LOCATE FOR CAUTCODE = laData[2]

IF laScrMode[1] .OR. laScrMode[2]
  STORE "DISABLE" TO lcTyp_Stat,lcAct_Stat,lcDeb_Stat,lcCrd_Stat,;
                     lcPer_Stat,lcNew_Stat,lcRem_Stat
ELSE
  lcTyp_Stat = IIF(lnTmpRcCnt = 0 , "ENABLE" , "DISABLE")
  lcAct_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  
  IF laData[5]='A'
    lcDeb_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
    lcCrd_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
    lcPer_Stat = "DISABLE"
  ELSE
    lcDeb_Stat = "DISABLE"
    lcCrd_Stat = "DISABLE"
    lcPer_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  ENDIF
  
*  lcNew_Stat = IIF(VAL(STRTRAN(laData[6],'-','')) > 0 .AND.; 
               ((laData[5]='A' .AND.( lnDebit>0 .OR. lnCredit>0));
               .OR. (laData[5]='P' .AND. lnAllocPer>0.AND.lnTotCr<100)),;
               "ENABLE","DISABLE")
  
  *B600381,1 Reham On 06/06/95
  *B600381,1 Control the enabling & disabling of the <NEW> button in the
  *B600381,1 different modes.
  IF lnTmpRcCnt = 0
    lcNew_Stat = IIF(VAL(STRTRAN(laData[6],'-','')) = 0 ,"DISABLE","ENABLE")
  ELSE
    lcNew_Stat = IIF(llNewDet OR (laData[5] = "P" AND lnTotCr=100) OR ;
                 (VAL(STRTRAN(laData[6],'-','')) = 0) ,"DISABLE","ENABLE")
  ENDIF
  
  lcRem_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")

  *B600381,1 Reham On 06/06/95
  *B600381,1 Restore the right data for the S.J. & Type popups if leaving
  *B600381,1 the screen & back again.
  puType    = IIF(laData[5] $ 'AP', AT(laData[5],'AP'), 0)
  lnSrcJrnl = IIF(lnPopLen>0,ASCAN('laTSrcJrnl',laData[7]),0)
  puSrcJrnl = IIF(lnSrcJrnl > 0,ASUBSCRIPT('laTSrcJrnl',lnSrcJrnl,1),lnDSrcJrn)
ENDIF

*E300683,5 Call *.SPR from screens directory
* DO GlAlcTr.SPR 
DO (gcScrDir + gcWinAppl + '\GlAlcTr.SPR')
*E300683,5 end   
*** Clean Up
SELECT GLAUTHD
SET FILTER TO  

IF glQuitting
  *** reset tag of GLAUTDT
  SELECT GLAUTDT
  IF !EMPTY(lcAutDtTg)
    SET ORDER TO TAG (lcAutDtTg)
  ELSE
    SET ORDER TO
  ENDIF

  *** Close open temporary files.  
  IF USED(lc_TmpAlc)
    USE IN ALIAS(lc_TmpAlc)
  ENDIF
  ERASE &gcWorkDir.&lc_TmpAlc..dbf  
ENDIF 

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

lcSavWin  = ' '
laData[1] = 'A'

IF _WINDOWS
  lcSavWin = WOUTPUT()
  ACTIVATE WINDOW glalcch SAME
*  IF laData[5] = 'P'
*	  @ 6.846,lnAcsSegSz+28.2 TO 8.615,lnAcsSegSz+28.2 ;
*		PEN 1, 8 ;
*      COLOR RGB(192,192,192,192,192,192)
	
*	  @ 6.846,lnAcsSegSz+28.4 TO 8.615,lnAcsSegSz+28.4 ;
*		PEN 1, 8 ;
*      COLOR RGB(192,192,192,192,192,192)
*  ELSE
*	  @ 6.846,lnAcsSegSz+28.2 TO 8.615,lnAcsSegSz+28.2 ;
*		PEN 1, 8
*	
*	  @ 6.846,lnAcsSegSz+28.4 TO 8.615,lnAcsSegSz+28.4 ;
*		PEN 1, 8 ;
*		  COLOR RGB(255,255,255,255,255,255)
 * ENDIF
  ACTIVATE WINDOW &lcSavWin SAME
ENDIF


DO CASE  
  *** "Select" mode (laScrMode[1]=.T.) 
  CASE laScrMode[1] 
    ACTIVATE WINDOW glalcch TOP
    SELECT (lc_TmpAlc)
    
    lsAlcDet       = 1
    SHOW GET lsAlcDet   ENABLE

    *** Delete old data ( if any )
    ZAP
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsAlcDet    
    ON ERROR &lcErrSett.
    
    *** Initialize variables for display
    lnTmpRcCnt = 0       && No nonempty records in the temporary file
    lnOldAlRec = 1    

    *** Reset defaults :
    laData[5]  = 'A'      && Initialise to 'A'mounts
    laData[6]  = REPLICATE ("0",lnAcsSegSz)  
                            && corresponding to allocation accout code
    laData[7]  = lcSJ_Def
    lcType     = laType[1]       && Initialise to 'A'mounts
    lcSrcJrnl  = lcDSrcJrn
    lcAcctCode = REPLICATE ("0",lnAcsSegSz)  
    lcAccDesc1 = SPACE(60)
    lcAccDesc2 = SPACE(60)
    lcOldAcct  = ''
    lcOldWnTyp = " "      
    
    STORE 0 TO lnCredit   , lnDebit    , lnAllocPer , lnTotDr    ,;
               lnTotCr    , lnOldDr    , lnOldCr    , lnOldPer   
    
    puType     = 1
    puSrcJrnl  = lnDSrcJrn

    *** lnDebit,lnCredit and lnAllocPer fields are appropriately handled
    *** through the call to procedure "lpSetWinds"
    SHOW GET lcAcctCode  DISABLE
    SHOW GET ibActBrow   DISABLE
      
    *** Controls
    SHOW GET pbNew       DISABLE 
    SHOW GET pbRem       DISABLE 
    SHOW GET &lcTypeDisp DISABLE
   
    *** Choose window 1 as a default
    DO lpSetWinds WITH 1
    
  *** "View" mode (laScrMode[2]=.T.), or "Edit" mode (laScrMode[3]=.T.)
  CASE laScrMode[2] .OR. laScrMode[3] 
    ACTIVATE WINDOW glalcch TOP
    lnDebit    = 0
    lnCredit   = 0
    llFirstRec = .T.
    lnSrcJrnl  = IIF(lnPopLen>0,ASCAN('laTSrcJrnl',laData[7]),0)
    *** Header file objects ( GLAUTHD )
    DO CASE
      CASE _DOS
        lcType    = IIF(laData[5] $ 'AP',;
                       laType[AT(laData[5],'AP')],'')
        lcSrcJrnl = IIF(lnSrcJrnl > 0,;
                       laTSrcJrnl[ASUBSCRIPT('laTSrcJrnl',lnSrcJrnl,1),1],;
                       lcDSrcJrn)    
      CASE _WINDOWS
        puType    = IIF(laData[5] $ 'AP', AT(laData[5],'AP'), 0)
        puSrcJrnl = IIF(lnSrcJrnl > 0,;
                       ASUBSCRIPT('laTSrcJrnl',lnSrcJrnl,1),lnDSrcJrn)
    ENDCASE
    lcAccDesc1 = LOOKUP(GLACCHAR.cAccnlDes,laData[6],;
                        GLACCHAR.cAcctCode,'ACCTCODE')
    
    *** Get the type of the allocation account
    IF laScrMode[3]
      lcCodeType  = IIF(LEFT(LTRIM(LOOKUP(GLACCHAR.cTypeCode,laData[6],;
                       GLACCHAR.cAcctCode,'ACCTCODE')),1)="Y","S","T")
    ENDIF                             

    lcAStamp = IIF(laScrMode[2],GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                   +GLAUTHD.cAdd_Time,lcAStamp)
    IF laScrMode[2] .OR. lcAStamp <> GLAUTHD.cAdd_User+;
                    DTOC(GLAUTHD.dAdd_Date)+GLAUTHD.cAdd_Time
      lnTotDr = 0
      lnTotCr = 0

      SELECT GLAUTDT      
      SELECT *,RECNO() AS 'nRecNo',	"S" AS 'cStatus';
           FROM &gcDataDir.GLAUTDT;
           INTO DBF &gcWorkDir.&lc_TmpAlc;
           WHERE &lcAutDtEx. = "A"+laData[2];
           .AND. lfTotals(AT(laData[5],'AP'))
    ENDIF
    
    SELECT (lc_TmpAlc)

    *** Get the number of records currently in the temporary file
    lnTmpRcCnt = RECCOUNT(lc_TmpAlc)

    *** Select the previously selected record,( lnOldAlRec)
    lsAlcDet   = IIF(laScrMode[3],lnOldAlRec,1)
    lnOldAlRec = lsAlcDet
  
    IF RECCOUNT(lc_TmpAlc) > 0
      GO lnOldAlRec
    ENDIF   
            
    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** an empty transaction,there seems to be no reason for checking 
    *** for the number of records in the file,or to initialize variables
    *** to spaces,hence,
    
    lcAcctCode = cAcctCode
    lcAccDesc2 = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpAlc..cAcctCode,;
                       GLACCHAR.cAcctCode,'ACCTCODE')
    
    IF laData[5] = "A"
      IF cDrorCr = "D"
        lnDebit  = nAmount
      ELSE
        lnCredit = nAmount
      ENDIF
    ELSE
      lnAllocPer = nAmount
    ENDIF        
    
    IF !EMPTY(laData[5])
      DO lpSetWinds WITH AT(laData[5],'AP')
    ENDIF
    
    *** Prepare push buttons
    lcObjState = IIF(laScrMode[3],"ENABLE","DISABLE")

    SHOW GET pbRem        &lcObjState.
    SHOW GET lcAcctCode   &lcObjState.
    SHOW GET ibActBrow    &lcObjState.
    
    IF laData[5] = 'A'
      SHOW GET lnDebit    &lcObjState.
      SHOW GET lnCredit   &lcObjState.
    ELSE
      SHOW GET lnAllocPer &lcObjState.
    ENDIF 	
    lcObjState = IIF(laScrMode[2].OR.(laScrMode[3].AND.laData[5]='P';
                    .AND.lnTotCr=100),"DISABLE","ENABLE")
    SHOW GET pbNew        &lcObjState.
    IF laScrMode[2]
      SHOW GET &lcTypeDisp    DISABLE
    ENDIF   
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsAlcDet     ENABLE
    ON ERROR &lcErrSett.
      
  *** "Add" mode (laScrMode[4]=.T.) 
  CASE laScrMode[4]
    *** Prepare defaults
    *** Remember that lcCurDate's length depends upon the century setting.
    lcDfRef    = lcTDfRef+' '+lcCurDate 
    laData[3]  = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))  
    laData[4]  = SUBSTR(lcTDfDesc+' '+lcUsername,1,;     
                       FSIZE('cAutDes','GLAUTHD'))

    SHOW GET &lcTypeDisp ENABLE
    SHOW GET  lsAlcDet   
ENDCASE 
SHOW GET puSrcJrnl 

SELECT GLAUTHD   

*!**************************************************************************
*!
*!      Function : lfTotals
*!
*!*************************************************************************
*    This function gets the total debits and credits,called
*    while SELECT_SQL command is executed.
*
FUNCTION lfTotals
PARAMETERS lnType

*** Skip first record (SELECT SQL counts it twice),first time
IF llFirstRec
  llFirstRec = .F.
ELSE  
  *** If Dollars screen and account is Debit,add it to debits totals
  *** If Dollars screen and account is Credit,add it to credits totals
  *** If it is percentage screen,add amounts field.The same field
  *** of credits totals (lnTotCr) is used for optimization. 
  IF lnType = 1 .AND. cDrOrCr = "D"
    lnTotDr = lnTotDr + nAmount
  ELSE
    lnTotCr = lnTotCr + nAmount
  ENDIF
ENDIF 

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*  VALID function for the get field "laData[2]" corresponding
*  to field GLAUTHD.cAutCode that constitutes a part of a 
*  two field primary key : GLAUTHD.cAutType + GLAUTHD.cAutCode   
*
FUNCTION lfvData_2

IF llBrowse .OR. (!EMPTY(laData[2]) .AND. LASTKEY() = 13)
  IF llBrowse
    =gfSeekRec()
    
    *B600385,1 Reham On 06/07/95 Added to point to the current object.
    IF laScrMode[1]
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
  ELSE
    laData[2] = ALLTRIM(laData[2])
    laData[2] = IIF(ISDIGIT(LEFT(laData[2],1)),;
                   PADL(laData[2], FSIZE('cAutCode','GLAUTHD'),'0'),;
                   PADR(laData[2], FSIZE('cAutCode','GLAUTHD'),SPACE(1)))

    SHOW GET laData[2]
  
    IF LASTKEY() = 13
      IF LEFT(laData[2],1) = '?'
        laData[1] = '?'
        laData[2] = SPACE(8)
      ENDIF
      =gfSeekRec()
      
      *B600385,1 Reham On 06/07/95 Added to point to the current object.
      IF laScrMode[1]
        _CUROBJ = OBJNUM(laData[2])
      ENDIF
    ENDIF
  ENDIF
ENDIF

llBrowse  = .F.
laData[1] = 'A'

*!**************************************************************************
*!
*!      Function: lfvType
*!
*!**************************************************************************
*
FUNCTION lfvType

DO CASE
  CASE _DOS
    laData[5] = SUBSTR('AP',ASCAN(laType,;
                      gfActPop(0,65,4,75,'laType',1,1,@lcType)),1)
    =lfRefresh()  
  CASE _WINDOWS
    laData[5] = SUBSTR('AP',puType,1)
ENDCASE    

IF !EMPTY(laData[5])
  DO lpSetwinds WITH AT(laData[5],'AP') 
  =lfRefresh() 
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvSrcJrnl
*!
*!**************************************************************************
*
FUNCTION lfvSrcJrnl

=lfSrcJrnl()
DO CASE
  CASE _DOS
    laData[7] = gfActPop(1,29,lnPopLen+3,75,'laTSrcJrnl',2,1,@lcSrcJrnl)
    =lfRefresh()  
  CASE _WINDOWS
    laData[7] = laTSrcJrnl[puSrcJrnl,2]
ENDCASE    

*!**************************************************************************
*!
*!      Procedure : lpSetWinds
*!
*!**************************************************************************
*  This procedure handles switching between the two screen layouts, 
*  according to the base type of the allocation account,whether it
*  is in Amount (Dollars) or Percent   
*
PROCEDURE lpSetwinds
PARAMETERS lnOption
PRIVATE lcStr

IF laData[5]<>lcOldWnTyp
  lcOldWnTyp = laData[5]
  SHOW GETS WINDOW &lcWinName. DISABLE     ONLY
  lcWinName  = "GLALCCH"+ALLTRIM(STR(lnOption))  
  lcStr      = "lcTTotal"+ALLTRIM(STR(lnOption))  
  lcTTotals  = &lcStr.
  lcStr      = "lcLsTotal"+ALLTRIM(STR(lnOption))  
  lcLsTotal  = &lcStr.
  ACTIVATE WINDOW &lcWinName.
ENDIF

*!**************************************************************************
*!
*!      Function: lswAlcDet
*!
*!**************************************************************************
*    WHEN function for the list "lsAlcDet" 
*
FUNCTION lswAlcDet

*** IF There are no records in the list,
*** prohibit selection from list.
IF  lnTmpRcCnt = 0
  _CUROBJ = OBJNUM(pbNew) 
  RETURN .F.
ELSE   
  *** If current record has no account code entry nor an amount entry,
  *** prohibit selection from list.
  IF VAL(STRTRAN(lcAcctCode,'-','')) = 0 ;
        .OR. (laData[5] = 'A'.AND.lnDebit = 0 .AND. lnCredit = 0);
        .OR. (laData[5] = 'P'.AND.lnAllocPer = 0)
    RETURN .F.
  ENDIF
ENDIF 

SELECT (lc_TmpAlc)

*** Refresh get fields with current contents
lcAcctCode = cAcctCode
lcAccDesc2 = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpAlc..cAcctCode,;
                   GLACCHAR.cAcctCode,'ACCTCODE')
IF laData[5] = "A" 
  IF cDrOrCr = "D"
    lnDebit  = nAmount
    lnCredit = 0
  ELSE
    lnCredit = nAmount
    lnDebit  = 0
  ENDIF
ELSE
  lnAllocPer = nAmount
ENDIF  

SHOW GET lcAcctCode 
SHOW GET lcAccDesc2 

IF laData[5] = 'A'
  SHOW GET lnDebit
  SHOW GET lnCredit
ELSE  
  SHOW GET lnAllocPer
ENDIF  

*** Save the currently selected record position
lnOldAlRec = lsAlcDet

SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfwActBrw1
*!
*!**************************************************************************
* When func. for browse icon...
*
FUNCTION lfwActBrw1

DO CASE
  CASE LASTKEY() = 9 .OR. LASTKEY() = 24
    _CUROBJ = OBJNUM(lcacctcode)
  CASE LASTKEY() = 15 .OR. LASTKEY() = 5
    _CUROBJ = OBJNUM(lsAlcDet)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfwActBrw2
*!
*!**************************************************************************
* When func. for browse icon...
*
FUNCTION lfwActBrw2

DO CASE
  CASE LASTKEY() = 9 .OR. LASTKEY() = 24
    _CUROBJ = OBJNUM(laData[6])
  CASE LASTKEY() = 15 .OR. LASTKEY() = 5
    DO CASE
      CASE _DOS
        _CUROBJ = OBJNUM(ibSrcJrnl)
      CASE _WINDOWS
        _CUROBJ = OBJNUM(puSrcJrnl)
    ENDCASE
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*    VALID function for get fields "lcAcctCode" , and laData[6]
*
FUNCTION lfvAccCode
PARAMETERS lcAcct,lnAcctPos
PRIVATE lcCodeT,lcAccnlDes,lcCurrObj

lcCurrObj = SYS(18)

IF LEFT(LTRIM(lcAcct),1)<>'?'.AND. !ISDIGIT(LTRIM(lcAcct))ISALPHA(LTRIM(lcAcct))
  =gfModalGen("TRM02061B00000","Dialog")
  &lcCurrObj. = lcOldAcct  
  SHOW GET &lcCurrObj.
  _CUROBJ     = OBJNUM(&lcCurrObj)
  RETURN
ENDIF

IF lcAcct = lcOldAcct 
  *** No need for validation
  RETURN
ELSE
  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry.
  IF LEFT(LTRIM(lcAcct),1)<>'?'.AND.VAL(STRTRAN(lcAcct,'-','')) = 0 
    &lcCurrObj. = lcOldAcct  
    SHOW GET &lcCurrObj.
  ELSE
    lcCodeT    = ''
    lcAccnlDes = ''
    *** If there are no record in the temporary file (list),
    *** the account type of the upper account code may be changed.
    *** This condition applies for the following cases :
    *** a. A new transaction ( Add mode )
    *** b. Removing all records in the list.
    IF lnTmpRcCnt = 0
      lcCodeType  = "A"
    ENDIF

    IF lfVldAccnt(lcCodeType,"C","L",.T.,@lcAccnlDes,@lcCodeT,"")
      IF lcCodeType = "A"
        lcCodeType  = IIF(LEFT(LTRIM(lcCodeT),1)="Y","S","T")
      ENDIF    
      IF lnAcctPos  = 2

        SELECT (lc_TmpAlc)

        *** If previously modified,"M---->M"
        *** If a new entry,        "A---->A"
        *** else                   "S---->M"       
        lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)
   
        REPLACE cAcctcode WITH lcAcctCode ,;
                cStatus   WITH lcStatus
  
        *** Refresh objects
        lcAccDesc2 = lcAccnlDes
        SHOW GET lcAccDesc2
        SHOW GET lsAlcDet
    
        SELECT GLAUTHD
        
        lcObjState = IIF(VAL(STRTRAN(laData[6],'-','')) > 0 .AND.; 
                     ((laData[5]='A' .AND.( lnDebit>0 .OR. lnCredit>0));
                     .OR. (laData[5]='P' .AND. lnAllocPer>0.AND.lnTotCr<100)),;
                     "ENABLE","DISABLE")
        *B600381,1 Reham On 06/06/95
        *B600381,1 This flag added to know the status of the <New> button.
        llNewDet   = IIF(VAL(STRTRAN(laData[6],'-','')) > 0 .AND.; 
                     ((laData[5]='A' .AND.( lnDebit>0 .OR. lnCredit>0));
                     .OR. (laData[5]='P' .AND. lnAllocPer>0.AND.lnTotCr<100)),;
                     .F.,.T.)
                     
        SHOW GET pbNew &lcObjState.
        IF laData[5] = 'A'
          SHOW GET lnDebit   ENABLE
          SHOW GET lnCredit  ENABLE
          lcNxtObj = IIF(lnDebit > 0 .OR. lnCredit > 0 ,"pbNew","lnDebit")
          _CUROBJ  = OBJNUM(&lcNxtObj.)
        ELSE
          SHOW GET lnAllocPer ENABLE
          lcNxtObj = IIF(lnAllocPer > 0,IIF(lnTotCr = 100 ,;
                     "pbSav" , "pbNew") , "lnAllocPer")
          _CUROBJ  = OBJNUM(&lcNxtObj.)
        ENDIF
        =lfRefresh()
      ELSE
        *** Modifying upper account code :
        IF lnTmpRcCnt = 0
           *B600381,1 Reham On 06/06/95
           *B600381,1 This flag added to know that <New> button is enable.
           llNewDet = .F.
           SHOW GET pbNew    ENABLE
        ENDIF   
        lcAccDesc1 = lcAccnlDes  
        SHOW GET lcAccDesc1
      ENDIF
    ELSE
      &lcCurrObj. = lcOldAcct
      SHOW GET &lcCurrObj.
      _CUROBJ     = OBJNUM(&lcCurrObj)
    ENDIF
    CLEAR TYPEAHEAD 	
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvDebit
*!
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    lnTotDr field is used for totals
*   
FUNCTION lfvDebit

IF lnDebit <> lnOldDr
  *** Reject negative or zero entries
  IF lnDebit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    lnDebit = lnOldDr  
    SHOW GET lnDebit
    _CUROBJ = OBJNUM(lnDebit)
  ELSE 

    SELECT (lc_TmpAlc)

    IF lnDebit > 0 .AND. cDrOrCr = "C"

      *** If there is a Credit entry for the same record,ignore it,
      *** blank it and adjust Credit totals. 
      lnCredit = 0
      lnTotCr  = lnTotCr - nAmount
      lnTotDr  = lnTotDr + lnDebit
           
      SHOW GET lnCredit    
    ELSE 
      lnTotDr  = lnTotDr - nAmount + lnDebit
    ENDIF
    lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH lnDebit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "D" 

    SELECT GLAUTHD

    *** Adjust controls
    IF lnDebit > 0
      _CUROBJ = OBJNUM(pbNew)
    ENDIF  
    SHOW GET lsAlcDet
    =lfRefresh()
  ENDIF
ENDIF

lcObjState = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 ;
                .AND.(lnDebit>0 .OR. lnCredit>0),"ENABLE","DISABLE")
*B600381,1 Reham On 06/06/95
*B600381,1 This flag to know the <New> button status.
llNewDet   = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 ;
                .AND.(lnDebit>0 .OR. lnCredit>0),.F.,.T.)
SHOW GET pbNew &lcObjState.
 
*!**************************************************************************
*!
*!      Function:  lfvCredit
*!
*!**************************************************************************
*    VALID function for get field "lnCredit".
*    lnTotCr field is used for totals
*   
FUNCTION lfvCredit

*** If there is an entry in 
IF lnCredit <> lnOldCr 
  
  *** Reject negative or zero entries
  IF lnCredit < 0
    =gfModalGen("TRM02036B00000","DIALOG") 
    lnCredit = lnOldCr
    SHOW GET lnCredit
    _CUROBJ  = OBJNUM(lnCredit)
  ELSE 

    SELECT (lc_TmpAlc)
    
    DO CASE
      CASE lnCredit > 0 .AND. cDrOrCr = "D"
        *** If there is a Debit entry for the same record,ignore it,
        *** blank it and adjust Debit totals. 
        lnDebit = 0
        lnTotDr = lnTotDr - nAmount  
        lnTotCr = lnTotCr + lnCredit

        SHOW GET lnDebit
      OTHERWISE
        lnTotCr = lnTotCr - nAmount + lnCredit
    ENDCASE
    lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH lnCredit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "C" 
    *** Adjust controls
    SHOW GET lsAlcDet
    =lfRefresh()
 
    SELECT GLAUTHD
  ENDIF
ENDIF

lcObjState = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 .AND.;
                (lnDebit > 0 .OR. lnCredit > 0),"ENABLE","DISABLE")
*B600381,1 Reham On 06/06/95
*B600381,1 This flag to know the staus of the <NEW> button.
llNewDet = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 .AND.;
              (lnDebit > 0 .OR. lnCredit > 0),.F.,.T.)
SHOW GET pbNew &lcObjState.

*!**************************************************************************
*!
*!      Function:  lfvAllocPr
*!
*!**************************************************************************
*    VALID function for get field "lnAllocPer"
*    Note that lnTotCr field is used for Allocation Percentage totals
*   
FUNCTION lfvAllocPr
PRIVATE lcModalWin

IF lnAllocPer <> lnOldPer 
  *** Reject negative or zero entries
  IF lnAllocPer < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    lnAllocPer = lnOldPer  
    SHOW GET lnAllocPer
    _CUROBJ    = OBJNUM(lnAllocPer)
  ELSE 
    SELECT (lc_TmpAlc)

    *** If the user inputs an entry in this field,
    IF (lnTotCr - nAmount + lnAllocPer) > 100
      *** If this entry caused the total of the allocation percentage
      *** to exceed 100 %, do not accept the entry,and present a message
      =gfModalGen("TRM02017B00000","DIALOG",lcTAllcPrc)
    
      *** Return to field
      lnAllocPer = lnOldPer
      SHOW GET lnAllocPer
      _CUROBJ    = OBJNUM(lnAllocPer)    
    ELSE  
      lnTotCr    = lnTotCr-nAmount+lnAllocPer
      lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
      REPLACE nAmount  WITH lnAllocPer ;
              cStatus  WITH lcStatus
    
      *** Adjust controls
      SHOW GET lsAlcDet
      =lfRefresh()
 
      SELECT GLAUTHD
    ENDIF
  ENDIF
ENDIF

lcObjState = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 .AND.;
                 lnAllocPer > 0 .AND. lnTotCr < 100 , "ENABLE" , "DISABLE")      

*B600381,1 Reham On 06/06/95
*B600381,1 This flag to know the status of the <New> button.
llNewDet   = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 .AND.;
                 lnAllocPer > 0 .AND. lnTotCr < 100 , .F. , .T.)
SHOW GET pbNew &lcObjState.

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*    VALID function for push button "New" (pbNew).
*
FUNCTION lfvNew

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

SELECT (lc_TmpAlc)

LOCATE FOR EMPTY(cAutType)
IF FOUND()
  REPLACE cAutType WITH laData[1],;
          cAutCode WITH laData[2],;   
          cStatus  WITH 'A'
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpAlc.;
              (cAutType,cAutCode,cStatus);
         VALUES (laData[1],laData[2],"A")      
ENDIF

*** Add Audit Information to the newly created record
=gfAdd_Info()

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 
lcAcctCode = REPLICATE ("0",lnAcsSegSz)  
lcAccDesc2 = SPACE(60)
IF laData[5] = "A"
  lnDebit  = 0
  lnCredit = 0
ELSE
  lnAllocPer = 0
ENDIF

*** Increase number of records in temporary file
lnTmpRcCnt = lnTmpRcCnt + 1

*** Select the new record from the list
lsAlcDet   = lnTmpRcCnt

SHOW GET lcAcctCode   ENABLE
SHOW GET ibActBrow    ENABLE
SHOW GET lcAccDesc2

*** Disable numeric fields until an account is entered.
IF laData[5] = "A"
  SHOW GET lnDebit    DISABLE
  SHOW GET lnCredit   DISABLE
ELSE
  SHOW GET lnAllocPer DISABLE
ENDIF

IF lnTmpRcCnt > 0
  SHOW GET pbRem       ENABLE
  SHOW GET &lcTypeDisp DISABLE
ENDIF

*** Disable New button until a valid account is ebtered
SHOW GET pbNew DISABLE

*B600381,1 Reham On 06/06/95
*B600381,1 Set the flag to .T. to know that there is a new detail line 
*B600381,1 with an empty account.
llNewDet = .T.

*** Do not forget to refresh the list
SHOW GET lsAlcDet   

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object) 
_CUROBJ = OBJNUM(lcAcctCode)

*** Always return to the original work aria
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*    VALID function for push button "Remove" (pbRem).
*  
FUNCTION lfvRem

*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
  SELECT (lc_TmpAlc)

  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Adjust totals
  IF cDrOrCr = "D"
    lnTotDr  = lnTotDr - nAmount
  ELSE 
    lnTotCr  = lnTotCr - nAmount
  ENDIF
  
  *** Decrement number of records in list
  lnTmpRcCnt = lnTmpRcCnt - 1

  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  SKIP
  IF EOF(lc_TmpAlc)
    GO TOP
  ENDIF  
  lsAlcDet   = IIF(lnTmpRcCnt=0,1,IIF(lsAlcDet=lnTmpRcCnt+1,1,lsAlcDet))
  lnOldAlRec = lsAlcDet

  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  lcAcctCode = IIF(lnTmpRcCnt=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)
  lcAccDesc2 = IIF(lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                  &lc_TmpAlc..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
  
  IF laData[5] = "A"
    lnDebit  = IIF(lnTmpRcCnt = 0 .OR. cDrOrCr = "C", 0 , nAmount)
    lnCredit = IIF(lnTmpRcCnt = 0 .OR. cDrOrCr = "D", 0 , nAmount)
  ELSE
    lnAllocPer = IIF(lnTmpRcCnt = 0 , 0 , nAmount)
  ENDIF

  *** Adjust controls
  lcObjState = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")

  SHOW GET lcAcctCode &lcObjState.       
  SHOW GET ibActBrow  &lcObjState.
  SHOW GET lcAccDesc2 
  SHOW GET pbRem      &lcObjState.
  SHOW GET pbNew      ENABLE
  
  *B600381,1 Reham On 06/06/95
  *B600381,1 Set the flag to .F. to know that the <NEW> button status is enable.
  llNewDet = .F.
  
  IF laData[5] = "A"
    SHOW GET lnDebit  &lcObjState.
    SHOW GET lnCredit &lcObjState.
  ELSE
    SHOW GET lnAllocPer &lcObjState.
  ENDIF
  
  IF lnTmpRcCnt = 0
    SHOW GET &lcTypeDisp ENABLE
  ENDIF  

  *** Update list contents
  SHOW GET lsAlcDet

  =lfRefresh()

  _CUROBJ = OBJNUM(lsAlcDet)

  SELECT GLAUTHD
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!*******************************************************************************
*    This procedure handles saving,instead of the global procedure.
*    "Save" option corresponds to the ninth position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving procedure.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
PROCEDURE lpSavScr 

DO CASE
  *** If the file is empty,i.e. there are no allocation transactions,
  *** "Cannot save an empty transaction" 
  CASE (lnTmpRcCnt = 0)
    =gfModalGen("TRM02035B00000","Dialog",lcTAllcTrs)
    llCSave = .F.
    _CUROBJ = OBJNUM(pbNew)
    RETURN

  CASE VAL(STRTRAN(laData[6],'-','')) = 0
    =gfModalGen("TRM02040B00000","DIALOG")  
    llCSave = .F.
    _CUROBJ = OBJNUM(laData[6])
    RETURN

  CASE VAL(STRTRAN(lcAcctCode,'-','')) = 0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave = .F.
    _CUROBJ = OBJNUM(lcAcctCode)
    RETURN

  CASE laData[5] = 'A' .AND. lnDebit = 0 .AND. lnCredit = 0
    =gfModalGen("TRM02033B00000","DIALOG",lcTDrORCR)
    llCSave = .F.
    _CUROBJ = OBJNUM(lnDebit)
    RETURN

  *** Present a message with percent=0 not allowed
  CASE laData[5] = 'P' .AND. lnAllocPer = 0 
    =gfModalGen("TRM02033B00000","DIALOG",lcTAllPrcs)
    llCSave = .F.
    _CUROBJ = OBJNUM(lnDebit)
    RETURN

  CASE lnTotDr = lnTotCr 
    *** Total debits and credits must not balance
    =gfModalGen("TRM02032B00000","DIALOG")
    llCSave = .F.   
    _CUROBJ = OBJNUM(lsAlcDet)
    RETURN

  OTHERWISE
    SELECT GLAUTHD

    *** If adding a new record,append a blank one
    IF laScrMode[4]
      APPEND BLANK
      =gfObj_Lock(.T.) 
    ENDIF

    *** Store laData values in the current record
    GATHER FROM laData fields &lcScFields.

    *** Now save the data in the temporary file using the following global
    *** function which performs correct translation from the temporary
    *** file lc_TmpAlc,and the main file GLAUTDT
    =gfTmp2Mast("GLAUTDT",lc_TmpAlc,;
                'Saving allocation entry '+laData[2]+' ...')  

    SELECT GLAUTHD
    IF laScrMode[4]
      =gfObj_Lock(.F.)
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!*******************************************************************************
*    This procedure handles deletion,instead of the global procedure.
*    "Delete" option corresponds to the seventh position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global delete procedure.
*
PROCEDURE lpDelScr 
PRIVATE lcAutType, lcAutCode

*** Check if this record is already deleted by another user from
*** a different station. If it is, the record pointer is no longer
*** on the viewed record which is now actually out of scope if 
*** SET('DELETED')='ON'

IF GLAUTHD.cAutType + GLAUTHD.cAutCode <> laData[1]+laData[2] 
  *** If the record is already deleted, present the following message,
  *** and go to 'Select' mode
  *** Message : "
  
  =gfModalGen("TRM00095B00000","ALERT")
  
  *** Go back to 'Select' mode
  laScrMode    = ""
  laScrMode[1] = .T.
  RETURN
ENDIF  

lnTotRec   = RECCOUNT(lc_TmpAlc)+1
lnThermRec = 0

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpAlc is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK

REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;        
    FOR &lcAutDtEx. = laData[1]+laData[2] .AND. ;
        lfThermo('Deleting allocation entry '+laData[2]+' ...')
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode 

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

IF lnThermRec < lnTotRec
  =gfThermo(lnTotRec,lnTotRec,;
        'Deleting allocation entry '+laData[2]+' ...','')
ENDIF

*** Return to "SELECT" mode
laScrMode    = .F.
laScrMode[1] = .T.
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function : lfThermo
*!
*!*******************************************************************************
*  This function calls global function gfThermo (thermometer)
*  It takes as a parameter a counter to be incremented and tested 
*  at every call. The thermometer is called at increments of 13
*  instead of 1 for faster processing.
*
FUNCTION lfThermo
PARAMETERS lcThermStr

lnThermRec = lnThermRec + 1

*!**************************************************************************
*!
*!      Function: lfSrcJrnl
*!
*!**************************************************************************
*  Creates an array of Source journals for the Source Journals popup
*
FUNCTION lfSrcJrnl

*** Create an array holding avialable Source Journals from 
*** GLSORJOR.DBF to be displayed in popup (puSrcJrnl/ibSrcJrnl)
SELECT ALLTRIM(CSRCJRNL)+'-'+CJORLNDES,CSRCJRNL;
       FROM &gcDataDir.GLSUBJOR;
       INTO ARRAY laTSrcJrnl

*** Define a length for the DOS popup.
lnPopLen  = IIF(_TALLY > 5,5,_TALLY )
  
*** Get the row number of the array element corresponding to
*** the default source journal if the array is not empty.
lnDSrcJrn = IIF(lnPopLen>0, ASCAN('laTSrcJrnl',lcSJ_Def), 0)  
  
*** If the array is empty (_TALLY=0), add the default Source Journal
*** value (global variable lcSJ_Def) to the array
*** Else, look for the default Source journal value in the array, if
*** found, use it as a default in 'Select' mode and 'Add' mode, else
*** add the default source journal values (global variable lcSJ_Def) 
*** to the array.
IF lnDSrcJrn = 0
  lnDSrcJrn = IIF(lnPopLen = 0, 1, ALEN('laTSrcJrnl',1) + 1)
   
  DIMENSION laTSrcJrnl[lnDSrcJrn,2]
  laTSrcJrnl[lnDSrcJrn,1] = lcSJ_Def+'-'+''
*  laTSrcJrnl[lnDSrcJrn,1] = lcSJ_Def+'-'+lcSJ_Desc    
  laTSrcJrnl[lnDSrcJrn,2] = lcSJ_Def 
  lnPopLen = lnPopLen + 1
ELSE
  lnDSrcJrn = ASUBSCRIPT('laTSrcJrnl',lnDSrcJrn,1)     
ENDIF 
  
*** Default source journal description is still missing in the 
*** last parameter of the following expression 
*** (lcSJ_Def+'-'+default source journal description) 
lcDSrcJrn = laTSrcJrnl[lnDSrcJrn,1]
