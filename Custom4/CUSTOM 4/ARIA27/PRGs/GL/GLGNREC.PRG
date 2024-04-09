*:************************************************************************
*:
*: Procedure file: GLGNREC.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfwGnRcDt  *** Date when function.
*:                 lfvGnRcDt  *** Date valid function.
*:                 lfvGenerat *** Valid func. of generate push button.
*:                 lfScanGen  *** Have the scan loop of the generate func.
*:                 lfCheck    *** Call from lfvGenerat to validate each record.
*:                 lfwRecCod  *** When func. of select recurring radio button.
*:                 lfvRecCod  *** Valid func. of select recurring code radio but.
*:                 lfwRecHdr  *** When func. of recurring header list.
*:                 lfInsDetl  *** Get the recurring details into the view list.
*:                 lfvRecHdr  *** Valid func. of recurring header list.
*:                 lfvSelect  *** Select push button valid func.
*:                 lfvSelAll  *** Select all push button valid func.
*:                 lfvInvert  *** Invert push button valid func.
*:                 lfvSelNon  *** Select none push button valid func.
*:                 lfvView    *** View push button valid func.
*:                 lfvUpdate  *** Update push button valid func.
*:                 
*:      Documented   /  /
*:************************************************************************
*B600389,1 Reham On 06/07/95
*B600389,1 Point to the right record in the detail temp file to refresh 
*B600389,1 the relation between the header & its lines.
*B600546,1 ORABY 07/10/95 After viewing a recurring trans. the record pointer 
*B600546,1                in the header file changes position.
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*B602245,1 KAM 09/07/2000 Fix the bug of filtering the transaction type in file GLAUTDT
*:************************************************************************
*
DECLARE laWndObj  [2,3]    && Have screen name & first & last obj. for each screen.

lcGenStat  = "ENABLE"

lcObjStat  = "DISABLE"

lcPrompt   = ""

ldGenRecDt = {}  && Default for the generating date.

ldOldDate  = {}  && To hold the old generating date.

ldTransDat = {}  && To hold the next date.

ldPerBgDat = {}  && Hold begin date of the period of sending date.

ldPerEnDat = {}  && Hold end date of the period of sending date.

lcTrHd_Tmp = ""  && Hold the trans. header that has been creating after updating.

lcTrDt_Tmp = ""  && Hold the trans. details that has been creating after updating.

lcBach_Tmp = ""  && Hold the batch records that has been creating after updating.

lcHead_Tmp = ""  && Hold the header temp. file name.

lcDetl_Tmp = ""  && Hold the details temp. file name.

lcRecr_Tmp = ""  && Hold the recurring dates info. temp. file name.

lcTmpSelCd = ""  && Hold the Selected recurring info. temp. file name.

lcScanCond = ""  && Hold scan condition in the generate function.

lcListStr  = ""  && The header list var.

lcOldCode  = ""  && Hold the old automatic code.

lcPrv1_Tag = ""  && Save previous tag of automatic header file.

lcPrv2_Tag = ""  && Save previous tag of automatic details file.

lcPrv3_Tag = ""  && Save previous tag of chart of account file.

llRetCheck = .T. && To know if return false or true from lfCheck function.

llChild    = .F. && Var use in the child screen.

llNoContrl = .T. && Flag to say that this screen has no control pannel.

laCtrStat  = "DISABLE"  && To disable all bars of record popoup 

llLockSt   = .F. && Var to know if the period is locked or no.

llSelCode  = .F. && Variable to hold if the select temp. is created.

llFirstSel = .T.  && Variable to hold if there is any selection befor

llUpdate   = .F.  && Variable to hold if there is a Update done in the selection screen

lnDay_Per  = 0   && Hold the increment of the next date by periods.

lcPer      = 0   && Hold the period of sending date.

lnTotDeb   = 0   && Hold total detail debit amount in the view screen.

lnTotCrd   = 0   && Hold total detail credit amount in the view screen.

ll1st_Rec  = 0   && Flag to skip counting 1st rec. in SQL command.

lnCurrRec  = 0   && Var. hold the current record no. during running thermometer.

lnTotRec   = 0   && Var. hold the no. of records in the recurring master or its temp.

lnOldRecCd = 1   && Var. to keep the old radio button value.

rbRecCod   = 1   && Default value of radio button is "All recurring codes".

lsRecDtl   = 1   && List value for details in the child screen.

lsRecHdr   = 1   && List value for header in the main screen.

lnNoOfPrd  = 0   && Variable to hold the no of period for the selected copmany.
 
*** These variables are defined in the screen object file. ***
lcTGenRec  = ""  && Child window title.
lcTRecurr  = ""  && Hold the word "recurring"
lcTherMsg1 = ""  && Hold the message : _
                 &&   "Generating recurring entries for recurring code..."
lcTherMsg2 = ""  && Hold the message : _
                 &&   "Update recurring entries for recurring code..."
lcTSelect  = ""  && Hold the word "S\<elect"
lcTUnSelct = ""  && Hold the word "\<Unselect"
lcTDefTxt1 = ""  && Hold the word "Recurring trn."
lcTDefTxt2 = ""  && Hold the word "Recurring transation code"
lcTDefTxt3 = ""  && Hold the word "Created by "

*** Function in the main program ***
*** Intialise all the variables & open all the files needed   ***
*** in this session and controling disabling and enabling of  ***
*** the menu bars and writting the screen names in the window ***
*** bars ....

IF !gfSetup()    
  RETURN
ENDIF  

laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "LDGENRECDT"
laWndObj [1,3] = "PBCLOSE"

laWndObj [2,1] = "CWDGLGNRC2"
laWndObj [2,2] = "LSRECDTL"
laWndObj [2,3] = "PBRCCLOSE"

SELECT GLAUTHD
SET ORDER TO TAG TYPECODE

*** Seek just for one recurring code in the automaic header file. ***

IF !SEEK("R")
  *** There is no recurring transaction ***
  *** for the process generate...
  *** < Ok > ***
  =gfModalGen("TRM02109B00000","DIALOG","recurring")
  glQuitting  = .T.  
  RETURN 
ENDIF

IF !WEXIST(gcBaseWind)
  lcTrHd_Tmp = gfTempName()   && Create a name for trans. header temp. file.

  lcTrDt_Tmp = gfTempName()   && Create a name for trans. details temp. file.

  lcBach_Tmp = gfTempName()   && Create a name for batch temp. file.

  lcHead_Tmp = gfTempName()   && Create a name for the temp. file.
  
  lcDetl_Tmp = gfTempName()   && Create a name for the temp. file.

  lcRecr_Tmp = gfTempName()   && Create a name for the temp. file.
  
  lcTmpSelCd = gfTempName()   && Create a name for the temp. file.
  
  *** Create an empty temp. file from the transaction ***
  *** header file...
  SELECT GLTRNSHD
  COPY STRUCTURE TO &gcWorkDir.&lcTrHd_Tmp CDX
  SELECT 0
  USE &gcWorkDir.&lcTrHd_Tmp EXCLUSIVE

  *** Create an empty temp. file from the transaction ***
  *** details file...
  SELECT GLTRNSDT
  COPY STRUCTURE TO &gcWorkDir.&lcTrDt_Tmp CDX
  SELECT 0
  USE &gcWorkDir.&lcTrDt_Tmp EXCLUSIVE

  *** Create an empty temp. file from the batch file ***
  SELECT GLBATCH
  COPY STRUCTURE TO &gcWorkDir.&lcBach_Tmp CDX
  SELECT 0
  USE &gcWorkDir.&lcBach_Tmp EXCLUSIVE

  *** Create an empty temp. file from the automatic ***
  *** details for the list string.. ***
  COPY STRUCTURE TO &gcWorkDir.&lcDetl_Tmp ;
       FIELDS GLAUTDT.CACCTCODE,GLACCHAR.CACCNLDES,;
              GLAUTDT.CDRORCR,GLAUTDT.NAMOUNT

  SELECT 0
  USE &gcWorkDir.&lcDetl_Tmp EXCLUSIVE

  *** Create temp. file hold the recurring code + the ***
  *** next generation date + the last generated  date ***
  *** to update the automatic header dates info after ***
  *** finishing update...
  SELECT GLAUTHD
  COPY STRUCTURE TO &gcWorkDir.&lcRecr_Tmp ;
       FIELDS CAUTCODE,DAUTLGDAT,DAUTNGDAT
  SELECT 0
  USE &gcWorkDir.&lcRecr_Tmp EXCLUSIVE

  *** Index this temp. file on the automatic ***
  *** recurring code...
  INDEX ON CAUTCODE TAG CAUTCODE
  SET ORDER TO TAG CAUTCODE

  *** Create an empty temp. file from the automatic ***
  *** header + 1 field "Incl" for the list string.. ***
  SELECT GLAUTHD
  COPY STRUCTURE EXTENDED TO &gcWorkDir.&lcHead_Tmp ;
       FIELDS GLAUTHD.CAUTCODE,GLAUTHD.CAUTDES,GLAUTHD.CAUTREF,;
              GLAUTHD.CSRCJRNL,GLAUTHD.CAUTREV
  
  SELECT 0
  USE &gcWorkDir.&lcHead_Tmp EXCLUSIVE
  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("dNext_Dat","D",8)
  
  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("dTransDat","D",8)
  
  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("Incl","C",1)

  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("cYear","C",4)

  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("cPeriod","C",2)

  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("cStandard","C",1)

  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("dPerBgDat","D",8)

  INSERT INTO &lcHead_Tmp ;
         (FIELD_NAME,FIELD_TYPE,FIELD_LEN);
  VALUES ("dPerEnDat","D",8)

  CREATE &gcWorkDir.&lcHead_Tmp FROM &gcWorkDir.&lcHead_Tmp
  
  *** Make index on automatic code & another one on the period. ***
  INDEX ON cyear+cperiod+cstandard TAG CPERIOD
  INDEX ON cautcode TAG CAUTCODE
  
  ***Set index to automatic code. ***
  SET ORDER TO TAG CAUTCODE
  
  *** List string from the temp. file. ***
  lcListStr  = "&lcHead_Tmp..CAUTCODE+' '+DTOC(&lcHead_Tmp..dTransDat)+SPACE(10-LEN(DTOC(&lcHead_Tmp..dTransDat)))+'  '+&lcHead_Tmp..Incl+'   '+SUBSTR(&lcHead_Tmp..CAUTDES,1,30)+' '+&lcHead_Tmp..CAUTREF"
  
  *** Get the end date of the current period ***
  *** as a default date...
*  IF SEEK(gcAct_Comp+lcCurr_yer+lcCurr_prd,"SYCFSPRD")  
*E300692,1 Change file name from SYCFSPRD to FSPRD
  *IF SEEK(gcPrnt_Cmp+lcCurr_yer+lcCurr_prd,"SYCFSPRD")
    *ldGenRecDt = SYCFSPRD.dfsppendt

  *E300789,6 Index does have company Id [Begin
  *IF SEEK(gcPrnt_Cmp+lcCurr_yer+lcCurr_prd,"FSPRD")
  IF SEEK(lcCurr_yer+lcCurr_prd,"FSPRD")
  *E300789,6 Index does have company Id [End

    ldGenRecDt = FSPRD.dfsppendt
*E300692,1 end 
  ENDIF

*E300692,1 Change file name from SYCFISHD to FISHD
  *SELECT SYCFISHD
  SELECT FISHD
*E300692,1 end
*  LOCATE FOR cComp_Id+cFisFYear+cFisYStat = gcAct_Comp+lcCurr_yer+'C'

  *E300789,6 Index does have company Id [Begin
  *LOCATE FOR cComp_Id+cFisFYear+cFisYStat = gcPrnt_Cmp+lcCurr_yer+'C'
  LOCATE FOR cFisFYear+cFisYStat = lcCurr_yer+'C'
  *E300789,6 Index does have company Id [End

  IF FOUND()
    lnNoOfPrd = INT(VAL(cFisNoPrd))
  ENDIF

ENDIF

*** Set filter in automatic header file ***
*** to the recurring entries...
SELECT GLAUTHD
SET FILTER TO CAUTTYPE = "R"

*** Save previous & set tag to "TYPECODE" ***
SELECT GLAUTHD
lcPrv1_Tag = SYS(22)
SET ORDER TO TAG TYPECODE

*** Save previous & set tag to "TYPCODACC" ***
SELECT GLAUTDT
lcPrv2_Tag = SYS(22)
SET ORDER TO TAG TYPCODACC

*** Save previous & set tag to "ACCTCODE" ***
SELECT GLACCHAR
lcPrv3_Tag = SYS(22)
SET ORDER TO TAG ACCTCODE

*** Set relation between the automatic details  ***
*** file & the chart of account file to account ***
*** code...
SELECT GLAUTDT
SET RELATION TO cAcctcode INTO GLACCHAR

*** Set relation between the automatic header ***
*** file & the automatic details file to the  ***
*** the automatic code...
SELECT GLAUTHD
SET RELATION TO cAuttype+cAutcode INTO GLAUTDT

*** Set relation between the automatic header ***
*** file & the  temp.  file to  the automatic ***
*** type & code...
SELECT (lcHead_Tmp)
SET RELATION TO "R"+cAutcode INTO GLAUTHD

*E300683,5 Call *.SPR from screens directory
* DO GLGNREC.SPR 
DO (gcScrDir + gcWinAppl + '\GLGNREC.SPR')
*E300683,5 end   
*** Clear the relation in the automatic details file. ***
*** & set tag to the previous tag before runnig...
SELECT GLAUTDT
SET RELATION TO
SET ORDER TO TAG &lcPrv2_Tag
  
*** Clear the relation & the filter in the ***
*** automatic header file & set tag to the ***
*** previous tag before runnig...
SELECT GLAUTHD
SET FILTER TO 
SET RELATION TO
SET ORDER TO TAG &lcPrv1_Tag

IF llSelCode
  IF USED(lcTmpSelCd)
    SELECT (lcTmpSelCd)
    SET RELATION TO
  ENDIF
ENDIF

SELECT (lcHead_Tmp)
SET RELATION TO

*** Set tag to the previous tag before runnig. ***
SELECT GLACCHAR
SET ORDER TO TAG &lcPrv3_Tag

IF glQuitting
  IF WEXIST("CWDGLGNRC2")
    RELEASE WINDOW CWDGLGNRC2
  ENDIF
  *** If exit from this program, erasing all the temp & text files ***
  IF USED(lcHead_Tmp)
    USE IN ALIAS(lcHead_Tmp)
  ENDIF
  ERASE &gcWorkDir.&lcHead_Tmp..DBF
  ERASE &gcWorkDir.&lcHead_Tmp..CDX

  IF USED(lcRecr_Tmp)
    USE IN ALIAS(lcRecr_Tmp)
  ENDIF
  ERASE &gcWorkDir.&lcRecr_Tmp..DBF
  ERASE &gcWorkDir.&lcRecr_Tmp..CDX

  IF llSelCode
    IF USED(lcTmpSelCd)
      USE IN ALIAS(lcTmpSelCd)
    ENDIF
    ERASE &gcWorkDir.&lcTmpSelCd..DBF
    ERASE &gcWorkDir.&lcTmpSelCd..CDX
  ENDIF

  IF USED(lcDetl_Tmp)
    USE IN ALIAS(lcDetl_Tmp)
  ENDIF
  ERASE &gcWorkDir.&lcDetl_Tmp..DBF

  IF USED(lcTrHd_Tmp)
    USE IN ALIAS(lcTrHd_Tmp)
  ENDIF
  ERASE &gcWorkDir.&lcTrHd_Tmp..DBF
  ERASE &gcWorkDir.&lcTrHd_Tmp..CDX
  ERASE &gcWorkDir.&lcTrHd_Tmp..FPT

  IF USED(lcTrDt_Tmp)
    USE IN ALIAS(lcTrDt_Tmp)
  ENDIF
  ERASE &gcWorkDir.&lcTrDt_Tmp..DBF
  ERASE &gcWorkDir.&lcTrDt_Tmp..CDX
  ERASE &gcWorkDir.&lcTrDt_Tmp..FPT

  IF USED(lcBach_Tmp)
    USE IN ALIAS(lcBach_Tmp)
  ENDIF
  ERASE &gcWorkDir.&lcBach_Tmp..DBF
  ERASE &gcWorkDir.&lcBach_Tmp..CDX
  ERASE &gcWorkDir.&lcBach_Tmp..FPT

ENDIF 

*!**************************************************************************
*!
*!      Function: lfwGnRcDt
*!
*!**************************************************************************
*
*** The generated date when function. ***
FUNCTION lfwGnRcDt

*** Save the old date value. ***
ldOldDate = ldGenRecDt

*!**************************************************************************
*!
*!      Function: lfvGnRcDt
*!
*!**************************************************************************
*
*** The generated date valid function. ***

FUNCTION lfvGnRcDt

IF !EMPTY(ldGenRecDt)
  IF ldOldDate <> ldGenRecDt
    IF !BETWEEN(ldGenRecDt,ldPyBgDate,ldNyEnDate)
      *** The generation date should be within the posting window. ***
      *** <  Ok  > ***
      =gfModalGen("INM02074B00000","DIALOG")
      ldGenRecDt = ldOldDate
      SHOW GET ldGenRecDt ENABLE
      _CUROBJ = OBJNUM(ldGenRecDt)
    ELSE
      *** If change the generating date with valid date, ***
      *** zap the previous data from the temp. file...   
      =lfCleanFil()
    ENDIF
  ENDIF
ENDIF

*** Enable all the inv. buttons in the corners ***
*** of each screen ( Home - Child ) ...
SHOW GET ibBgDumi1   ENABLE
SHOW GET ibEndDumi1  ENABLE
SHOW GET ibBgDumi2   ENABLE
SHOW GET ibEndDumi2  ENABLE

*** Enable the child close button all the time. ***
SHOW GET pbRCClose   ENABLE

*!**************************************************************************
*!
*!      Function: lfvGenerat
*!
*!**************************************************************************
*
*** Generate push button valid function. ***

FUNCTION lfvGenerat

IF rbRecCod = 1
  SELECT GLAUTHD
  lnTotRec  = 0
  lnCurrRec = 0
  COUNT FOR CAUTTYPE+CAUTCODE = "R" TO lnTotRec
  IF lnTotRec > 0
    =lfScanGen("GLAUTHD","GLAUTHD.dAutngdat <= ldGenRecDt .AND. !EMPTY(GLAUTHD.cAutcode)")
  ELSE
    =gfModalGen("TRM02053B00000","DIALOG",lcTRecurr)
  ENDIF
ELSE
  SELECT (lcTmpSelCd)
  lnTotRec  = 0
  lnCurrRec = 0
  COUNT FOR cInclude = 'û' TO lnTotRec
  IF lnTotRec > 0
    =lfScanGen(lcTmpSelCd,"&lcTmpSelCd..dAutngdat <= ldGenRecDt .AND. !EMPTY(&lcTmpSelCd..cInclude) .AND. !EMPTY(&lcTmpSelCd..cAutcode)")
  ELSE
    =gfModalGen("TRM02053B00000","DIALOG",lcTRecurr)
  ENDIF
ENDIF  

SELECT (lcHead_Tmp)
COUNT TO lnTotRec
IF lnTotRec <> 0
  GO TOP
  =lfInsDetl()   && Get the recurring details into the view list
  SELECT (lcHead_Tmp)
  SHOW GET lsRecHdr

  lcGenStat = "DISABLE"
  lcObjStat = "ENABLE"

  *** Disable the generating date & the generate button ***
  *** & the selecting recurring radio button...
  SHOW GET pbGenerat  DISABLE

  *** Refresh all the buttons under the list that ***
  *** change the update mode of the records...
  SHOW GET pbSelect ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbView   ENABLE
  SHOW GET pbUpdate ENABLE
ELSE
  *** All recurring transaction generated ***
  *** through this date...
  *** <   Ok   > ***
  =gfModalGen("TRM02108B00000","DIALOG")
  _CUROBJ = OBJNUM(ldGenRecDt)
ENDIF

*!**************************************************************************
*!
*!      Function: lfScanGen
*!
*!**************************************************************************
*
*** The generate scan function that called with parameters ***
*** from the generate valid function.

FUNCTION lfScanGen
PARAMETERS lcAlias,lcExprScan

*** If there is records selected or found in the automatic header. ***

SCAN FOR &lcExprScan

  lnCurrRec = lnCurrRec + 1
  =gfThermo(lnTotRec,lnCurrRec,lcTherMsg1,&lcAlias..CAUTCODE)

  IF lfCheck()
    *** Replace in the temp. file. ***
  
    ldTransDat = &lcAlias..dAutngdat           && Save next date.

    DO WHILE .T.
      lcYear     = ""
      ldPerBgDat = {}
      ldPerEnDat = {}
      llLockSt   = .F.

      *** Get the period of the sending date & its ***
      *** begining date & ending date...
      *lcPer  = gfPeriod(ldTransDat,gcAct_Comp,@lcYear,;
                        @ldPerBgDat,@ldPerEnDat,@llLockSt)
      lcPer  = gfPeriod(ldTransDat,gcPrnt_Cmp,@lcYear,;
                        @ldPerBgDat,@ldPerEnDat,@llLockSt)

      lcPer  = PADL(lcPer,2,'0')

      IF llLockSt
        *** Period xx-zzzz is locked. ***
        *** <  Ok  > ***
        lcTmpStr = lcPer+"-"+lcYear
        =gfModalGen("TRM02107B00000","DIALOG",lcTmpStr)
        EXIT
      ENDIF

      *** Add record in the temp. file. ***
      INSERT INTO &lcHead_Tmp;
             (CAUTCODE,dTransDat,Incl,CAUTDES,CAUTREF,CSRCJRNL,;
             cYear,cPeriod,cStandard,dPerBgDat,dPerEnDat,CAUTREV);
             VALUES (&lcAlias..CAUTCODE,ldTransDat," ",;
                     &lcAlias..CAUTDES,&lcAlias..CAUTREF,&lcAlias..CSRCJRNL,;
                     lcYear,lcPer,GLACCHAR.cStandard,ldPerBgDat,ldPerEnDat,&lcAlias..CAUTREV)

      DO CASE
        CASE &lcAlias..cAutfrqun = "D"
          *** If the freq. type is "days" --> "D". ***
          ldTransDat = ldTransDat + &lcAlias..nAutfrqno

          REPLACE &lcHead_Tmp..dNext_Dat WITH ldTransDat

        CASE &lcAlias..cAutfrqun = "P"
          *** If the freq. type is "Periods" --> "P". ***
          lnDay_Per     = 0
          lnIncrment    = lfChk_Dat(ldTransDat)
          IF lnIncrment = 0
            *** If the total has to be add will make the date ***
            *** out of posting window, make this date blanck. ***
            ***_______________________________________________***
            REPLACE &lcHead_Tmp..dNext_Dat WITH {}
            EXIT
          ELSE
            *** If there is value has to be added to the date ***
            *** add it to the date...
            ldTransDat = ldTransDat +lnIncrment

            REPLACE &lcHead_Tmp..dNext_Dat WITH ldTransDat

          ENDIF
      ENDCASE

      IF (ldTransDat > ldGenRecDt) .OR. (ldTransDat > &lcAlias..dAutendat) .OR.;
        (!BETWEEN(ldTransDat,ldPyBgDate,ldNyEnDate))
        *** If the new date is out of posting window. ***
        EXIT
      ENDIF

    ENDDO
    
  ENDIF
  SELECT (lcAlias)
ENDSCAN

IF lnTotRec > lnCurrRec
  FOR lnThCount = lnCurrRec TO lnTotRec
    =gfThermo(lnTotRec,lnThCount,lcTherMsg1,&lcAlias..CAUTCODE)
  ENDFOR
ENDIF

*!**************************************************************************
*!
*!      Function: lfCheck
*!
*!**************************************************************************
*
*** This function called from the generate function to know  ***
*** if the next generate date is within the posting window   ***
*** and the lines of this recurring has any inactive account ***
*** or not allow for posting.

FUNCTION lfCheck

IF !BETWEEN(&lcAlias..dAutngdat,ldPyBgDate,ldNyEnDate)
  IF EMPTY(&lcAlias..dAutngdat)
    =gfModalGen("TRM02212B00000","DIALOG",&lcAlias..cAutcode)
  ELSE
    *** Reccuring entry {code-desc} not valid. ***
    *** Next transaction date {generating date}***
    *** is prior to the posting window. ***
    *** You must correct the next transaction date. ***
    *** <  Ok  > ***
    lcTmpStr = &lcAlias..cAutcode+"|"+ALLTRIM(&lcAlias..cAutdes)+"|"+DTOC(&lcAlias..dAutngdat)
    =gfModalGen("TRM02075B00000","DIALOG",lcTmpStr)
  ENDIF  
  RETURN .F.
ENDIF

llRetCheck = .T.


SELECT GLAUTDT
*B602245 KAM (start )
*SCAN REST WHILE cAutcode = &lcAlias..cAutcode
SCAN REST WHILE cAutcode = &lcAlias..cAutcode  AND GLAUTDT.CAUTTYPE = "R"
*B602245 KAM (end)
  IF GLACCHAR.cSegactiv = "I"
    *** Reccuring entry ð has one or more accounts inactive. ***
    *** You cannot generate this reccuring transaction. ***
    *** <  Ok  > ***
    lcTmpStr = &lcAlias..cAutcode+"|"+ALLTRIM(&lcAlias..cAutdes)
    =gfModalGen("TRM02077B00000","DIALOG",lcTmpStr)
    llRetCheck = .F.
    EXIT
  ENDIF

  IF GLACCHAR.cSegalpos = "N"
    *** Reccuring entry ð has one or more accounts ***
    *** not allowed to recieve posting from G/L.   ***
    *** You cannot generate this reccuring transaction. ***
    *** <  Ok  > ***
    lcTmpStr = &lcAlias..cAutcode+"|"+ALLTRIM(&lcAlias..cAutdes)
    =gfModalGen("TRM02078B00000","DIALOG",lcTmpStr)
    llRetCheck = .F.
    EXIT
  ENDIF

  SELECT GLAUTDT
ENDSCAN

IF !llRetCheck
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfChk_Dat
*!
*!**************************************************************************
*
*** This function called from the scan generate function to know  ***
*** the increment that has to be added to the next generated date ***

FUNCTION lfChk_Dat

PARAMETERS ldNxtDat

lcPrvAlias = ALIAS()

lcYear     = ""
ldPerBgDat = {}
ldPerEnDat = {}
llLockSt   = .F.

*** Get the period of the sending date & its ***
*** begining date & ending date...
*lcPer  = gfPeriod(ldNxtDat,gcAct_Comp,@lcYear,@ldPerBgDat,@ldPerEnDat,@llLockSt)
lcPer  = gfPeriod(ldNxtDat,gcPrnt_Cmp,@lcYear,@ldPerBgDat,@ldPerEnDat,@llLockSt)

IF lcPer = 0
  lnDay_Per = 0
ELSE
*E300692,1 Change file name from SYCFSPRD to SYCFSPRD
  *SELECT SYCFSPRD
  SELECT FSPRD
*E300692,1 end  
  lcPer  = PADL(lcPer,2,'0')    && Modi by MH
*  lcPer = RIGHT("0"+ALLTRIM(STR(lcPer+1)),2)
*  IF SEEK(gcAct_Comp+lcYear+lcPer)

  *E300789,6 Index does have company Id [Begin
  *IF SEEK(gcPrnt_Cmp+lcYear+lcPer)
  IF SEEK(lcYear+lcPer)
  *E300789,6 Index does have company Id [end

    FOR lnCount = 1 TO &lcAlias..nAutfrqno

      *E300789,6 field comp Id not in file[begin
      *IF EOF() .OR. gcAct_Comp <> CCOMP_ID
      IF EOF()
      *E300789,6 field comp Id not in file[end 

        lnDay_Per = 0
        EXIT
      ELSE
        lnDay_Per = lnDay_Per + ((dFsppendt - dFsppbgdt)+1)
        SKIP
      ENDIF
    ENDFOR
  ELSE
    lnDay_Per = 0
  ENDIF
ENDIF

SELECT (lcPrvAlias)
RETURN lnDay_Per

*!**************************************************************************
*!
*!      Function: lfwRecCod
*!
*!**************************************************************************
*
*** The recurring code radio button when function. ***

FUNCTION lfwRecCod

lnOldRecCd = rbRecCod

*!**************************************************************************
*!
*!      Function: lfvRecCod
*!
*!**************************************************************************
*
*** The recurring code radio button valid function. ***

FUNCTION lfvRecCod

*IF lnOldRecCd <> rbRecCod

  IF rbRecCod = 1
    IF USED('&lcTmpSelCd')
      SELECT (lcTmpSelCd)  
      GO TOP
      REPLACE ALL &lcTmpSelCd..cSelect WITH ' '
    ENDIF
    =lfCleanFil()

    IF USED('&lcTmpSelCd')
      SELECT (lcTmpSelCd)
      SET RELATION TO
    ENDIF
    
    SELECT (lcHead_Tmp)
    SET RELATION TO

    *** Set relation between the automatic header ***
    *** file & the automatic details file to the  ***
    *** the automatic code...
    SELECT GLAUTHD
    SET RELATION TO cAuttype+cAutcode INTO GLAUTDT

    *** Set relation between the automatic header ***
    *** file & the  temp.  file to  the automatic ***
    *** type & code...
    SELECT (lcHead_Tmp)
    SET RELATION TO "R"+cAutcode INTO GLAUTHD
  ELSE
    llSelCode = .T.
    IF llFirstSel 
      *E300683,5 Call programs from PRGS directory
      *DO glSelCd.PRG WITH lcTmpSelCd,'R',.T.
      DO (gcAppHome + gcWinAppl + '\glSelCd.PRG ') WITH lcTmpSelCd,'R',.T.
      *E300683,5 end
      llFirstSel = .F.
    ELSE
      *E300683,5 Call programs from PRGS directory
      *DO glSelCd.PRG WITH lcTmpSelCd,'R'
      DO (gcAppHome + gcWinAppl + '\glSelCd.PRG ') WITH lcTmpSelCd,'R'
      *E300683,5 end
    ENDIF  
    SELECT (lcTmpSelCd)
    IF llUpdate
      SELECT GLAUTHD
      SET RELATION TO

      SELECT (lcHead_Tmp)
      SET RELATION TO

      *** Set relation between the automatic header ***
      *** file & the automatic details file to the  ***
      *** the automatic code...
      SELECT (lcTmpSelCd)
      SET RELATION TO cAuttype+cAutcode INTO GLAUTDT

      *** Set relation between the automatic header ***
      *** file & the  temp.  file to  the automatic ***
      *** type & code...
      SELECT (lcHead_Tmp)
      SET RELATION TO "R"+cAutcode INTO &lcTmpSelCd
      =lfCleanFil()
    ENDIF
  ENDIF
  _CUROBJ = OBJNUM(pbGenerat)
*ENDIF

llUpdate = .F.

*!**************************************************************************
*!
*!      Function: lfCleanFil
*!
*!**************************************************************************
*
*** This function called from different places to show some ***
*** objects and zap the temp. files & make the situation as ***
*** if you enter the screen for the first time to start new ***
*** generate recurring proccess.

FUNCTION lfCleanFil

SELECT (lcHead_Tmp)
lsRecHdr = 1
SHOW GET lsRecHdr
ZAP
SHOW GETS WINDOW (gcBaseWind)

lcGenStat = "ENABLE"
lcObjStat = "DISABLE"

*** Show disable all the selected buttons & enable ***
*** only the radio button & the generate button...
SHOW GET pbGenerat ENABLE
*SHOW GET pbSelect DISABLE
SHOW GET pbSelAll DISABLE
SHOW GET pbInvert DISABLE
SHOW GET pbSelNon DISABLE
SHOW GET pbView   DISABLE
SHOW GET pbUpdate DISABLE

lcPrompt = lcTSelect
SHOW GET pbSelect,1 PROMPT lcTSelect DISABLE

*** Zap the temp. file that hold the details information. ***
SELECT (lcDetl_Tmp)
lsRecDtl = 1
SHOW GET lsRecDtl ENABLE
ZAP

*** Set total debit & total credit variables to zero. ***
lnTotDeb = 0
lnTotCrd = 0

*** Show the view screen that has the list to clean ***
*** it from the previous data...
SHOW GETS WINDOW CWDGLGNRC2

*!**************************************************************************
*!
*!      Function: lfwRecHdr
*!
*!**************************************************************************
*
*** The Recurring header list when function. ***

FUNCTION lfwRecHdr

IF RECCOUNT(lcHead_Tmp) = 0
  RETURN
ENDIF

= lfStatus()

IF WVISIBLE('CWDGLGNRC2') .AND. &lcHead_Tmp..CAUTCODE <> lcOldCode
  *** If the child window visible and the automatic ***
  *** code has  been changed :  excute  function to ***
  *** insert details of the new code in the list of ***
  *** child window ...
  =lfInsDetl()  && Get the recurring details into the view list
ENDIF

SELECT (lcHead_Tmp)

*** Save the current automatic code in the list. ***
lcOldCode = &lcHead_Tmp..CAUTCODE

*!**************************************************************************
*!
*!      Function: lfStatus
*!
*!**************************************************************************
*
*** The status function that show the prompt of the select push button. ***

FUNCTION lfStatus

*** Display the button select if field "Incl = ' '" & ***
*** the button unselect if field "Incl = 'û'"...
IF &lcHead_Tmp..Incl = 'û'
  lcPrompt = lcTUnSelct
  SHOW GET pbSelect,1 PROMPT lcTUnSelct
ELSE
  lcPrompt = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF

*!**************************************************************************
*!
*!      Function : lfInsDetl
*!
*!**************************************************************************
*
*** This function called from 2 places, from the generate function ***
*** and from the recurring header list when function. 

FUNCTION lfInsDetl

*** This function to insert the details of the current ***
*** automatic code in the list in the child screen...

*** Zap the previous temp. file that hold the details ***
*** for the previous automatic code...
SELECT (lcDetl_Tmp)
lsRecDtl = 1
SHOW GET lsRecDtl ENABLE
ZAP
SHOW GETS WINDOW CWDGLGNRC2
lcCode = &lcHead_Tmp..CAUTCODE   && Save current automatic code.
*B600546,1 Getting the current record in the header file
lnRecNo = RECNO(lcHead_Tmp)
lnTotDeb  = 0
lnTotCrd  = 0

*** This is for changing the  pointer in the temp. header ***
*** file to refresh the relation between the temp. header ***
*** file and the automatic details master file...
SELECT (lcHead_Tmp)
*B600546,1 This seek was locating the first template record not the current on because
*B600546,1 the current index is on the template code only that is why the record pointer changes position
*SEEK(lcCode)
*B600546,1 Go to the current record in the header file
IF !EOF()
  GOTO lnRecNo
ELSE
  GO BOTTOM
ENDIF

SELECT GLAUTDT
*B600389,1 Reham On 06/07/95
*B600389,1 Added this line to point to the right record in the detail temp
*B600389,1 file & refresh the relation.

*B602245 KAM (start)
*LOCATE FOR GLAUTDT.CAUTCODE  = lcCode
*SCAN REST WHILE GLAUTDT.CAUTCODE  = lcCode

LOCATE FOR GLAUTDT.CAUTCODE  = lcCode  AND GLAUTDT.CAUTTYPE = "R"
SCAN REST WHILE GLAUTDT.CAUTCODE  = lcCode AND GLAUTDT.CAUTTYPE = "R"

*B602245 KAM (end)

  *** Insert all the details lines in the detail temp. file. ***
  INSERT INTO &lcDetl_Tmp ;
         (CACCTCODE,CACCNLDES,CDRORCR,NAMOUNT) ;
         VALUES (GLAUTDT.CACCTCODE,LOOKUP(GLACCHAR.cAccnldes,GLAUTDT.cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE'),;
         GLAUTDT.CDRORCR,GLAUTDT.NAMOUNT)

  *** Calculate the total debits & total credits. ***
  IF GLAUTDT.CDRORCR ="D"
    lnTotDeb = lnTotDeb + GLAUTDT.NAMOUNT
  ELSE
    lnTotCrd = lnTotCrd + GLAUTDT.NAMOUNT
  ENDIF
  
ENDSCAN

*** Refresh the list that hold all the details. ***
SELECT (lcDetl_Tmp)
GO TOP
SHOW GETS WINDOW CWDGLGNRC2

*!**************************************************************************
*!
*!      Function: lfvRecHdr
*!
*!**************************************************************************
*
*** The recurring header list valid function. ***

FUNCTION lfvRecHdr

*** Select the temp. file that has all the generated recurring. ***
SELECT (lcHead_Tmp)

*** Check the current record  if its selected in the ***
*** records has to be update, unselect this record & ***
*** if its unselected, select it...
REPLACE &lcHead_Tmp..Incl WITH IIF(&lcHead_Tmp..Incl=" ","û"," ")

*** Refresh the list. ***
SHOW GET lsRecHdr ENABLE

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
*** The select push button valid function. ***

FUNCTION lfvSelect

*** Call the list valid function. ***

=lfvRecHdr()

*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
*
*** The select all push button valid function. ***

FUNCTION lfvSelAll

*** Select the temp. file that has all the generated recurring. ***
SELECT (lcHead_Tmp)

*** Select all the records in the list to update. ***
REPLACE ALL &lcHead_Tmp..Incl WITH "û"

*** Refresh the list. ***
SHOW GET lsRecHdr ENABLE

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
*** The invert push button valid function. ***

FUNCTION lfvInvert

*** Select the temp. file that has all the generated recurring. ***
SELECT (lcHead_Tmp)

*** Reverse the selected records & the unselected ***
*** records in the list to update...
REPLACE ALL &lcHead_Tmp..Incl WITH IIF(&lcHead_Tmp..Incl=" ","û"," ")

*** Refresh the list. ***
SHOW GET lsRecHdr ENABLE

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvSelNon
*!
*!**************************************************************************
*
*** The select none push button valid function. ***

FUNCTION lfvSelNon

*** Select the temp. file that has all the generated recurring. ***
SELECT (lcHead_Tmp)

*** Select nothing from the list to update. ***
REPLACE ALL &lcHead_Tmp..Incl WITH " "

*** Refresh the list. ***
SHOW GET lsRecHdr ENABLE

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvView
*!
*!**************************************************************************
*
*** The valid function of the view push button. ***

FUNCTION lfvView

*** Call the collecting details function. ***
=lfInsDetl()

*** Call the child screen thru the global function activate window. ***
=gfActWind('CWDGLGNRC2',lctGenRec)

*!**************************************************************************
*!
*!      Function: lfvUpdate
*!
*!**************************************************************************
*
*** The update push button valid function. ***

FUNCTION lfvUpdate
 

*** Get the current user name. ***
lcUserNam = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')

lcOldPerd  = ' '  && To save the the period.

llFrsLoop  = .T.  && To know if adding batch rec. before or not.

lnTotBatDr = 0    && Hold total debits for saving into batch file.

lnTotBatCr = 0    && Hold total credits for saving into batch file.

lnTotTrnDr = 0    && Hold total debits for saving into trans. header file.

lnTotTrnCr = 0    && Hold total credits for saving into trans. header file.

lnBatch    = 0    && Variable to hold the the temp batch no

lnTrans    = 0    && Variable to hold the the temp transaction no

*** Loop inside the temp. file for the selected records. ***
SELECT (lcHead_Tmp)
SET ORDER TO TAG CPERIOD     && Set order to period tag.

lnCurrRec = 0
lnTotRec  = 0
COUNT FOR !EMPTY(Incl) TO lnTotRec

*** Check if there is no recurring selected to be update. ***
IF lnTotRec = 0
  *** No {recurring} selected for this criteria. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02095B00000","DIALOG",lcTRecurr)
  _CUROBJ = OBJNUM(lsRecHdr)
  RETURN
ENDIF

SCAN FOR !EMPTY(Incl)          && Only the included recrds.

  lnCurrRec = lnCurrRec + 1
  =gfThermo(lnTotRec,lnCurrRec,lcTherMsg1,&lcHead_Tmp..CAUTCODE)

  *** Fill the recurring temp. file to update the recurring ***
  *** header  file  with  the  transaction date & the  next ***
  *** genrating date...

  SELECT (lcRecr_Tmp)
  IF SEEK(&lcHead_Tmp..cAutcode)
    REPLACE DAUTLGDAT WITH &lcHead_Tmp..dTransdat;
            DAUTNGDAT WITH &lcHead_Tmp..dNext_Dat
  ELSE
    INSERT INTO &lcRecr_Tmp;
           (CAUTCODE,DAUTLGDAT,DAUTNGDAT);
           VALUES;
           (&lcHead_Tmp..cAutcode,&lcHead_Tmp..dTransdat,;
           &lcHead_Tmp..dNext_Dat)
  ENDIF
  
  *** If the period changed add new rec. in ***
  *** the batch file...
  IF &lcHead_Tmp..cPeriod + &lcHead_Tmp..cStandard <> lcOldPerd
  
    IF !llFrsLoop         && If record added from before save its totals.

      SELECT (lcBach_Tmp)
      REPLACE NBATCNTOT WITH lnTotBatDr;
              NBATOTDR  WITH lnTotBatDr; 
              NBATOTCR  WITH lnTotBatCr
      =gfAdd_Info()
      
      lnTotBatDr = 0
      lnTotBatCr = 0
          
    ENDIF
  
    lnBatch = lnBatch + 1
    
    *** Add new record in the temp. batch file. ***
    INSERT INTO &lcBach_Tmp ; 
           (CBATCHNO,CBATSTAT,CBATTYPE,DBATPBEG,;
           DBATPEND,CBATREFER,CBATDESC,CSRCMODUL,CCOMP_ID,CBATPYR) ;
     VALUES(PADL(lnBatch,FSIZE('CBATCHNO'),'0'),'U',;
            IIF(&lcHead_Tmp..cStandard = 'Y','N','S'),;
            &lcHead_Tmp..dPerBgDat,&lcHead_Tmp..dPerEnDat,;
            lcTDefTxt1,lcTDefTxt3+" "+lcUserNam,gcAct_Appl,;
            gcAct_Comp,&lcHead_Tmp..cYear)

    llFrsLoop = .F.
    
  ENDIF

  *** Save the current period and Type .
  lcOldPerd = &lcHead_Tmp..cPeriod + &lcHead_Tmp..cStandard
    
  lnTrans = lnTrans + 1
  
  IF &lcHead_Tmp..cAutrev = 'Y'
    *** Prepare variables to calculate rev. date ***
    *** and its period & year...
    lcYer     = ""
    ldPerBgDt = {}
    ldPerEnDt = {}
    llLockSt  = .F.

    *** Rev. date = transaction date + 1 day. ***
    ldTrnRevDt = &lcHead_Tmp..dPerEnDat + 1
  
    *** Get the period of the sending date & its ***
    *** begining date & ending date...

*    lcPerd  = gfPeriod(ldTrnRevDt,gcAct_Comp,@lcYer,;
                      @ldPerBgDt,@ldPerEnDt,@llLockSt)
    lcPerd  = gfPeriod(ldTrnRevDt,gcPrnt_Cmp,@lcYer,;
                      @ldPerBgDt,@ldPerEnDt,@llLockSt)
                      
    IF lcPerd = lnNoOfPrd
      lcPerd = '01'
      lcYer  = STR(INT(VAL(lcYer)+1),4)
    ELSE    
      lcPerd = PADL(lcPerd+1,2,'0')
    ENDIF
    
  ELSE
    ldTrnRevDt = {}
    lcYer      = ""
    lcPerd     = ""
  ENDIF

  *** Add new record in the transaction header file. ***
  INSERT INTO &lcTrHd_Tmp ;
         (CBATCHNO,CTRANNO,CTRNDESC,CTRNREFER,;
          DTRNPDATE,CTRNPYR,CTRNPPRD,CTRNSTAT,;
          CTRNTYPE,CSRCMODUL,CTRNREVER,DTRNREVDT,;
          CTRNREVYR,CTRNREVPR,CCOMP_ID,CAUTTYPE,;
          CAUTCODE,CSTANDARD,CSRCJRNL);
   VALUES(PADL(lnBatch,FSIZE('CBATCHNO'),'0'),;
          PADL(lnTrans,FSIZE('CTRANNO'),'0'),;
          lcTDefTxt2+" "+&lcHead_Tmp..cAutcode,;
          lcTDefTxt1,&lcHead_Tmp..dTransDat,&lcHead_Tmp..cYear,;
          &lcHead_Tmp..cPeriod,'U','A',gcAct_Appl,&lcHead_Tmp..cAutrev,;
          ldTrnRevDt,lcYer,lcPerd,gcAct_Comp,'R',&lcHead_Tmp..cAutcode,;
          GLACCHAR.CSTANDARD,&lcHead_Tmp..CSRCJRNL)

  *** Add all the details in the temp. file into ***
  *** the transation detail file...
  SELECT GLAUTDT
  *B602245 KAM (start) 
  *SCAN REST WHILE GLAUTDT.CAUTCODE = &lcHead_Tmp..cAutcode 
  SCAN REST WHILE GLAUTDT.CAUTCODE = &lcHead_Tmp..cAutcode AND GLAUTDT.CAUTTYPE = "R"
  *B602245 KAM (end )
  
    INSERT INTO &lcTrDt_Tmp;
           (CBATCHNO,CTRANNO,CACCTCODE,CDRORCR,NAMOUNT,;
            DTRNPDATE,CTRNPYR,CTRNPPRD);
    VALUES (PADL(lnBatch,FSIZE('CBATCHNO'),'0'),;
            PADL(lnTrans,FSIZE('CTRANNO'),'0'),;
            GLAUTDT.CACCTCODE,GLAUTDT.CDRORCR,;
            GLAUTDT.NAMOUNT,&lcHead_Tmp..dTransdat,;
            &lcHead_Tmp..cYear,&lcHead_Tmp..cPeriod)
            
    SELECT (lcTrDt_Tmp)
    =gfAdd_Info()

    *** Calculate all the debits & credits amounts ***
    *** for this group of details... 
    DO CASE
      CASE GLAUTDT.cDrorcr = 'D'
        lnTotTrnDr = lnTotTrnDr + GLAUTDT.nAmount
      CASE GLAUTDT.cDrorcr = 'C'
        lnTotTrnCr = lnTotTrnCr + GLAUTDT.nAmount
    ENDCASE
       
    SELECT GLAUTDT
  ENDSCAN

  *** Save the debit & credit amount for the last ***
  *** group of details in the transaction file...
  SELECT (lcTrHd_Tmp)
  REPLACE NTRNTOTDR WITH lnTotTrnDr;
          NTRNTOTCR WITH lnTotTrnCr
  =gfAdd_Info()
  
  *** Calculate all the debit & credit amounts for the ***
  *** group of transaction that have the same period & ***
  *** including in the selecting  records that  has to ***
  *** be update...
  lnTotBatDr = lnTotBatDr + lnTotTrnDr
  lnTotBatCr = lnTotBatCr + lnTotTrnCr
 
  *** Set the acumilators of trans debits & credits to ***
  *** zeros to calculate the next group of details...
  lnTotTrnDr = 0
  lnTotTrnCr = 0
  
  SELECT (lcHead_Tmp)
ENDSCAN

*** Modify the totals for the last record ***
*** created in the batch temp. file...
SELECT (lcBach_Tmp)
REPLACE NBATCNTOT WITH lnTotBatDr;
        NBATOTDR  WITH lnTotBatDr; 
        NBATOTCR  WITH lnTotBatCr
=gfAdd_Info()

IF lnTotRec > lnCurrRec 
  FOR lnThCount = lnCurrRec TO lnTotRec
    =gfThermo(lnTotRec,lnThCount,lcTherMsg1,&lcHead_Tmp..CAUTCODE)
  ENDFOR
ENDIF

IF gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT,GLAUTHD",.T.)

  *** Update master batch/transactions header/transaction details
  SET ORDER TO TAG BATCHNO  IN &lcBach_Tmp
  SET ORDER TO TAG BATCHTRN IN &lcTrHd_Tmp
  SET ORDER TO TAG BATCHTRN IN &lcTrDt_Tmp

  *** GLBATCH  ---> &lcBach_Tmp ***
  *** GLTRNSHD ---> &lcTrHd_Tmp ***
  *** GLTRNSDT ---> &lcTrDt_Tmp ***
  *** GLAUTHD  ---> &lcRecr_Tmp ***

  SELECT(lcBach_Tmp)
  SET RELATION TO

  SELECT(lcTrHd_Tmp)
  SET RELATION TO
  SET RELATION TO &lcTrHd_Tmp..cbatchno INTO &lcBach_Tmp ADDITIVE

  SELECT(lcTrDt_Tmp)
  SET RELATION TO &lcTrDt_Tmp..cbatchno+&lcTrDt_Tmp..ctranno INTO &lcTrHd_Tmp ADDITIVE

  *** Set variables for the Thermometer
  lnCurtrns  = 0
  lnTotTrns  = RECCOUNT()

  *** Variable to compair if there is a change in the batch No.
  lcBatchKey = ' '

  *** Variable to compair if there is a change in the Transaction No.
  lcTrnNoKey = ' '

  *** Variables to sum the total debit & credit per Batch
  lnTotBatDr = 0
  lnTotBatCr = 0

  *** Variables to sum the total debit & credit per Transaction
  lnTotTrnDr = 0
  lnTotTrnCr = 0

  SCAN
    *** Assign the thermometer variable with the Record no.
    lnCurtrns  = lnCurtrns + 1
    =gfThermo(lnTotTrns,lnCurtrns,lcTherMsg2,&lcTrHd_Tmp..CAUTCODE)

    *** Compair if the batch no = the old batch or not. If not
    *** we are going to creat a new batch no.
    IF lcBatchKey <> cBatchNo

      lcBatchKey = cBatchNo

      ** Creating New batch No.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcBatchNo = PADL(gfSequence("BATCH",1,gcAct_Comp),6,'0')
      lcBatchNo = gfSequence('CBATCHNO')
      *E300663,1 Change this line [End]
      
      SELECT(lcBach_Tmp)
      SCATTER MEMVAR MEMO
      m.cBatchNo = lcBatchNo
      m.nBatCnTot= 0
      m.nBatotDr = 0
      m.nBatotCr = 0

      SELECT GLBATCH
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info('GLBATCH')

      SELECT(lcTrDt_Tmp)
      lnTotBatDr = 0
      lnTotBatCr = 0

    ENDIF

    *** Compair if the batch no = the old transaction or not.  If not
    *** we are going to creat a new transaction no.
    IF lcTrnNoKey <> cTranNo

      lcTrnNoKey = cTranNo

      *** Create New Transaction No.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcTranNo = PADL(gfSequence("TRANSACT",1,gcAct_Comp),8,'0')
      lcTranNo = gfSequence('CTRANNO')
      *E300663,1 Change this line [End]
      
      SELECT(lcTrHd_Tmp)
      SCATTER MEMVAR MEMO
      m.cBatchNo = lcBatchNo
      m.cTranNo  = lcTranNo
      m.nTrnTotDr= 0
      m.nTrnTotCr= 0

      SELECT GLTRNSHD
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info('GLTRNSHD')
 
      SELECT(lcTrDt_Tmp)
      lnTotTrnDr = 0
      lnTotTrnCr = 0
    ENDIF

    SELECT(lcTrDt_Tmp)
    SCATTER MEMVAR MEMO
    m.cBatchNo = lcBatchNo
    m.cTranNo  = lcTranNo

    IF &lcTrDt_Tmp..cDrorcr = 'D'
      lnTotTrnDr = lnTotTrnDr+nAmount
      lnTotBatDr = lnTotBatDr+nAmount
    ELSE
      lnTotTrnCr = lnTotTrnCr+nAmount
      lnTotBatCr = lnTotBatCr+nAmount
    ENDIF

    SELECT GLTRNSDT
    APPEND BLANK
    GATHER MEMVAR MEMO
    =gfAdd_Info('GLTRNSDT')

    SELECT GLTRNSHD
    REPLACE nTrnTotDr WITH lnTotTrnDr,;
            nTrnTotCr WITH lnTotTrnCr

    SELECT GLBATCH 
    REPLACE nBatCnTot WITH lnTotBatDr,;
            nBatotDr  WITH lnTotBatDr,;
            nBatotCr  WITH lnTotBatCr 

    SELECT(lcTrDt_Tmp)

  ENDSCAN

  SELECT GLTRNSHD
  REPLACE nTrnTotDr WITH lnTotTrnDr,;
          nTrnTotCr WITH lnTotTrnCr

  SELECT GLBATCH
  REPLACE nBatCnTot WITH lnTotBatDr,;
          nBatotDr  WITH lnTotBatDr,;
          nBatotCr  WITH lnTotBatCr 

  IF lnCurtrns < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      =gfThermo(lnTotTrns,lnCurtrns,lcTherMsg2,"")
    ENDFOR
  ENDIF

  *** Set the relation off ***
  SELECT(lcBach_Tmp)
  SET RELATION TO

  SELECT(lcTrHd_Tmp)
  SET RELATION TO

  SELECT(lcTrDt_Tmp)
  SET RELATION TO

  *** Update the recurring codes { next - last dates } in  the ***
  *** automatic header  file from the  temp. file {lcRecr_Tmp} ***
  *** that has beeen creating inside the loop...

  SELECT (lcRecr_Tmp)
  SCAN
    IF SEEK('R'+&lcRecr_Tmp..CAUTCODE,'GLAUTHD')
      REPLACE GLAUTHD.DAUTLGDAT WITH &lcRecr_Tmp..DAUTLGDAT;
              GLAUTHD.DAUTNGDAT WITH &lcRecr_Tmp..DAUTNGDAT
    ENDIF
    SELECT (lcRecr_Tmp)
  ENDSCAN

  *** Unlock the locked file
  =gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT,GLAUTHD",.F.)
ELSE 
  =gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT,GLAUTHD",.F.)
ENDIF  

*** Clear all the temp. files preparing to ***
*** generate another time...
SELECT (lcTrHd_Tmp)
ZAP

SELECT (lcTrDt_Tmp)
ZAP

SELECT (lcBach_Tmp)
ZAP

SELECT (lcRecr_Tmp)
ZAP

IF llSelCode
  IF USED(lcTmpSelCd)
    SELECT (lcTmpSelCd)
    ZAP
  ENDIF
ENDIF

=lfCleanFil()  && To reback the stuiation again as before at the begining.

IF WVISIBLE('CWDGLGNRC2') 
  =gfChClose('CWDGLGNRC2')
ENDIF
