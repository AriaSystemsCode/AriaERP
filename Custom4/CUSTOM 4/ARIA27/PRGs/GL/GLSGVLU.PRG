*:************************************************************************
*:
*: Procedure file: GLSGVLU.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfvData_1  *** The laData[1] valid function ***
*:                 lpShow     *** Show proc. for home screen ***
*:                 lfValShow  *** Show proc. for the child screen ***
*:                 lfwData_2  *** Segment value when func. ***
*:                 lfvData_2  *** Segment value valid func. ***
*:                 lfSelOptn  *** Called from Seg. valid to select action ***
*:                 lfvData_3  *** laData[3] valid func.
*:                 lfvData_4  *** laData[4] valid func.
*:                 lfvOpen    *** Push button valid func. to activate child screen ***
*:                 lfSegDes   *** To define the picture of the seg. desc. ***
*:                 lfvData_5  *** Valid func. of the status popup ***
*:                 lfvData_6  *** Valid func. of the term popup ***
*:                 lfvData_7  *** Valid func. of the categry popup ***
*:                 lfvData_10 *** Valid func. of the ratio group popup ***
*:                 lfvData_11 *** Valid func. of the cash flow popup ***
*:                 lfvData_12 *** Valid func. of the allow posting check box ***
*:                 lfwData_13 *** When func. of the automatic destribution ***
*:                 lfvData_13 *** Valid func. of the automatic destribution ***
*:                 lfvData_15 *** Valid func. of the consolidation percentage ***
*:                 lfDigit    *** Func. to valid seg. value has no char. ***
*:                 lpSavScr   *** Save record if calling from menu. ***
*:                 lfvSegOK   *** Save record if calling from account validation ***
*:                 lpSavScr   *** Save the new record ***
*:                 lpDelScr   *** Delete record ***
*:               
*:      Documented 12/22/1993
*:************************************************************************
*B600374,1 Reham On 06/01/95 Restore the right value of the Allow GL
*B600374,1 posting if back to the from another screen.
*B600472,1 Reham On 06/18/95
*B600472,1 I'll go to view mode only if return from the browse
*B600472,1 with .T. because this will mean that ladata[2] is not
*B600472,1 empty.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*B602255,1 AAN 09/08/2000 Fix bug of displaying the account description 
*B602255,1                We updated the GLSGVLU screen only 
*B804399,1 SSE 09/23/2001 Fix bug of Updating Long description with short description, and not
*B804399,1                Updating Ratio group and cash flow.
*:************************************************************************
*
PARAMETERS pcSegNum,pcSegValue  && If calling from the account validation.

*** To know if the prog. calling from the menu or account validation.
llSegMd = IIF(TYPE("pcSegNum") $ "UL",.F.,.T.)

EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode

DECLARE laKeyField [2,4]       && Have the tag information.
laKeyField[1,1] = 'laData[1]'  && The 1st var that has the index expr.
laKeyField[1,2] =.F.           && If this field terminatig for index or not.
laKeyField[1,3] = 'ACSSEGVAL'  && Tag name.
laKeyField[1,4] = 1            && Position of field inside index.
laKeyField[2,1] = 'laData[2]'  && The 2nd var that has the index expr.
laKeyField[2,2] =.T.           && If this field terminatig for index or not.
laKeyField[2,3] = 'ACSSEGVAL'  && Tag name.
laKeyField[2,4] = 2            && Position of field inside index.

DECLARE laSegSize  [1,2]  && Array to have seg. size & desc.
DECLARE laObjdisp  [6]    && Array to control the display of seg. Des. in Child

DECLARE laWndObj   [3,3]  && Have screen name & first & last obj. for each screen

*** this defination of these arrays is temporary till it be ***
*** data driven from screen object file ...

*** The status popup array ***
DECLARE laTStatus [2,2]
laTStatus [1,1] = "Active"
laTStatus [1,2] = "A"
laTStatus [2,1] = "Inactive"
laTStatus [2,2] = "I"

*** The categry popup array ***
DECLARE laTCatgry [4,2]
laTCatgry [1,1] = "Cash"
laTCatgry [1,2] = "C"
laTCatgry [2,1] = "Non cash"
laTCatgry [2,2] = "N"
laTCatgry [3,1] = "Depreciation"
laTCatgry [3,2] = "D"
laTCatgry [4,1] = "Amortization"
laTCatgry [4,2] = "A"

*** The term popup array ***
DECLARE laTTerm   [2,2]
laTTerm   [1,1] = "Long term"
laTTerm   [1,2] = "L"
laTTerm   [2,1] = "Current"
laTTerm   [2,2] = "C"

lc_TempAc  = ""     && Name for the temp. file hold accounts upon deletion.
llDispFlg  = .F.    && to display all obj. if 1st seg.
llNumeric  = .F.    && To know if 1st seg. has any char. or not.
llBrowse   = .F.
lnSegNo    = 1      && Have the segment no.
lnTotlen   = 0
lnRitPos   = 2
lcOldAuto  = ""     && The old automatic distribution.

lcRatioDes = ""        && Ratio group say var.
puRatio    = ""
puRatioGrp = ""
lcCashFDes = ""        && Cash flow say var.
puCash     = ""
puCashFlow = ""
lcTerm     = ""     && Term say var.
lcCatgry   = ""     && Categry say var.
lcStatus   = ""     && Status say var.

lcActMajr = " "
lcPopTerm = " "
lcPopCatg = " "
lcPopRato = " "
lcPopCash = " "
lcPopStat = " "
lcSegStat = " "
 
STORE '' TO lcSeg1,lcSeg2,lcSeg3,lcSeg4,lcSeg5,lcSeg6  && Seg. values

laObjdisp  = .F.    && Default Display all lines in child screen 

lnBoxSize  = 19
cbPost     = 1      && Default value forcheck box.
lcTypCode  = '   '  && The type code.
lcTypDesc  = ''     && The type description.
lctActMajr = ''     && Veriabel to hold the child window titel 

llChild    = .F.

laDefProc    = .T.
laDefProc[9] = .F.  && Use local Save   procedure
laDefProc[7] = .F.  && Use local Delete procedure

*** These variables defined before in the screen object file. ***
lcTNonApp  = ""
lcTFldTxt  = ""
lcTAtBrTit = ""
lcTPrcntg  = ""
lcTitleAct = ""

*** Var to hold random name of the child window. ***
lcChldWind = " "

*** Function in the main program ***
*** Intialise all the variables & open all the files needed   ***
*** in this session and controling disabling and enabling of  ***
*** the menu bars and writting the screen names in the window ***
*** bars ....

IF !gfSetup()    
  RETURN
ENDIF  

llSegMd    = IIF(TYPE("pcSegNum") $ "UL",.F.,.T.)

LCTACTMAJR = "Account Major options"
LCTFLDTXT  = "Field"
LCTATBRTIT = "Automatic Distribution"
LCTPRCNTG  = "Percentage"
LCTITLEACT = "Accounts"
lcTNonApp  = "N/A"

DECLARE laWndObj   [3,3]  && Have screen name & first & last obj. for each screen

SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF
SELECT GLSEGVAL

IF !WEXIST(gcBaseWind)
  *** Get random name for the balances window. ***
  lcChldWind = "CWR"+SUBSTR(ALLTRIM(gfTempName()),1,7)

  lc_TempAc  = gfTempName()
  SCATTER FIELDS &lcScFields TO laData BLANK

  *** Select the segment size & desc. in laSegSize array. ***  
*E300692,1 Change file name and path from SYCACCOD to ACCOD  
  *SELECT nacssize,cAcsshdes FROM &gcSysHome.sycaccod ;
         WHERE !EMPTY(nacssize) .AND. ;
               ccomp_Id = gcAct_comp ;
         INTO ARRAY laSegSize

*E300789,7 [BEGIN] Remove ccomp_Id = gcAct_comp from SELECT-SQL command         
 * SELECT nacssize,cAcsshdes FROM &gcDataDir.accod ;
         WHERE !EMPTY(nacssize) .AND. ;
               ccomp_Id = gcAct_comp ;
         INTO ARRAY laSegSize
  SELECT nacssize,cAcsshdes FROM &gcDataDir.accod ;
         WHERE !EMPTY(nacssize) ;
         INTO ARRAY laSegSize
*E300789,7 [END..]         

 *E300692,1 end        
         

  *** Calculate seg. postions & sizes. ***
  FOR lnCount = 1 TO ALEN(laSegSize,1)
    lnTotlen  = lnTotlen  + laSegSize [lnCount,1] + 4
    laObjdisp[lnCount]  = .T.
  ENDFOR
  lnRitPos  = INT((78/2) - (lnTotlen/2))

  IF _WINDOWS
    IF llSegMd
      puRatio = lcTNonApp
      DEFINE POPUP puRatioGrp prompt field GLRACOD.cRatDesc scroll;
      FROM 16.917+1.538,16.5 TO 22,57;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puRatioGrp DO lfvData_10
      
      puCash = lcTNonApp
      DEFINE POPUP puCashFlow prompt field GLCFITEM.cCfidesc scroll;
      FROM 19.333+1.538,16.5 TO 25,57;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puCashFlow DO lfvData_11
    ELSE
      puRatio = lcTNonApp
      DEFINE POPUP puRatioGrp prompt field GLRACOD.cRatDesc scroll;
      FROM 6.667+1.538,16.5 TO 12,57;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puRatioGrp DO lfvData_10
      
      puCash = lcTNonApp
      DEFINE POPUP puCashFlow prompt field GLCFITEM.cCfidesc scroll;
      FROM 9.083+1.538,16.5 TO 15,57;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puCashFlow DO lfvData_11
    ENDIF
  ELSE
    lcRatioDes = lcTNonApp
    lcCashFDes = lcTNonApp
  ENDIF
ENDIF

laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "lcSeg1"
laWndObj [1,3] = "pbOpen"

laWndObj [2,1] = "GWCCONTRL1"
laWndObj [2,2] = "PBTOP"
laWndObj [2,3] = "PBCLS"

laWndObj [3,1] = lcChldWind
laWndObj [3,2] = "ibPopTerm"
laWndObj [3,3] = "pbSgClose"

IF llSegMd
  *** If calling from account validation. ***
  llDispFlg = (pcSegNum = "1")

  lnSegNo   = VAL(pcSegNum)
  laData[1] = pcSegNum
  
  lcSegVlu  = "lcSeg" + pcSegNum
  &lcSegVlu = pcSegValue
  laData[2] = pcSegValue
  
  lcSegStat = "DISABLE"
  
  IF laData[1] = "1"

    *** Get the type code & desc. for this segment. ***
    SELECT GLTYPES
    GO TOP
    LOCATE FOR BETWEEN(VAL(pcSegValue),VAL(cTyplacno),VAL(cTypuacno))
    IF FOUND()
      lcTypCode    = GLTYPES.cTypeCode
      lcTypDesc    = GLTYPES.cTypeDesc
      laData[8]    = lcTypCode
      SHOW GET lcTypCode DISABLE
      SHOW GET lcTypDesc DISABLE
    ENDIF

    lcPopTerm = IIF(LEFT(laData[8],1) $ 'AL',"ENABLE","DISABLE")
    lcTerm    = IIF(LEFT(laData[8],1) $ 'AL',laTTerm[1,1],lcTNonApp)
    laData[6] = IIF(LEFT(laData[8],1) $ 'AL',laTTerm[1,2]," ")
    ibPopTerm = 1

    lcPopCatg = IIF(LEFT(laData[8],1) = 'A',"ENABLE","DISABLE")
    lcCatgry  = IIF(LEFT(laData[8],1) = 'A',laTCatgry[1,1],lcTNonApp)
    laData[7] = IIF(LEFT(laData[8],1) = 'A',laTCatgry[1,2]," ")
    ibPopCatg = 1

    lcPopStat = "ENABLE"
    lcStatus  = laTStatus[1,1]
    laData[5] = laTStatus[1,2]
    ibPopStat = 1

    SELECT GLRACOD
    SET FILTER TO
    SET FILTER TO CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " "
    
    lcPopRato  = IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE")
    lcRatioDes = lcTNonApp
    puRatio    = lcTNonApp
    
    lcPopCash  = IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE")
    lcCashFDes = lcTNonApp
    puCash     = lcTNonApp

    *** Set defaults if 1st segment. ***
    cbPost     = 1
    laData[12] = "Y"
  ELSE
    STORE "DISABLE" TO lcPopTerm,lcPopCatg,lcPopRato,lcPopCash,lcPopStat
  ENDIF
  
  SELECT GLSEGVAL  
  
  laScrMode    = .F.
  laScrMode[4] = .T.

  *E300683,5 Call *.SPR from screens directory
  * DO GLSEGMD.SPR 
  DO (gcScrDir + gcWinAppl + '\GLSEGMD.SPR')
  *E300683,5 end   

ELSE
  IF laData[1] = '1'
    lcActMajr = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
                IIF(VAL(laData[1]) = 1,"ENABLE","DISABLE"))
    lcPopTerm = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
                IIF(LEFT(laData[8],1) $ 'AL',"ENABLE","DISABLE"))
    lcPopCatg = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
                IIF(LEFT(laData[8],1) = 'A',"ENABLE","DISABLE"))
    lcPopRato = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
                IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE"))
    lcPopCash = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
                IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE"))
    lcPopStat = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE")

    *B600374,1 Reham On 06/01/95 Restore the right value if back to 
    *B600374,1 the from another screen.
    cbPost = IIF(laData[12] = "Y",1,0)

    *B600372,1 Reham On 06/03/95 Set filter in the GLRACOD according to 
    *B600372,1 current type code.
    SELECT GLRACOD
    SET FILTER TO 
    SET FILTER TO CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " "
  ELSE
    STORE "DISABLE" TO lcPopTerm,lcPopCatg,lcPopRato,;
                       lcPopCash,lcPopStat,lcActMajr
  ENDIF

  *** If calling from the menu. ***
  *E300683,5 Call *.SPR from screens directory
  * DO GLSGVLU.SPR 
  DO (gcScrDir + gcWinAppl + '\GLSGVLU.SPR')
  *E300683,5 end   
ENDIF

*B600372,1 Reham On 06/03/95 Clear the filter in the GLRACOD if left the screen.
SELECT GLRACOD
SET FILTER TO 

IF glQuitting
  *** If quit from this screen close its child screen. ***
  RELEASE POPUP puRatioGrp
  RELEASE POPUP puCashFlow
  
  IF WEXIST(lcChldWind)
    RELEASE WINDOW &lcChldWind
  ENDIF

  *** If exit from this program, erasing all the temp & text files ***
  IF USED(lc_TempAc)
    USE IN ALIAS(lc_TempAc)
  ENDIF
  ERASE &gcWorkDir.&lc_TempAc..DBF
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    *** Select mode ***
    lcTypCode = "   "
    lcTypDesc = ""

    *** Set the pic. for all segment values. ***
    FOR lnCount = 1 TO ALEN(laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      &lcObjNam = SPACE(laSegSize[lnCount,1])
      SHOW GET &lcObjNam ENABLE
    ENDFOR
    SHOW GET pbOpen     DISABLE

  CASE laScrMode[2]
    *** View mode ***
    SELECT GLSEGVAL
    *** Get type code & desc. ***
    IF laData[1] ="1"
      lcTypCode = laData[8]
      lcTypDesc = LOOKUP(GLTYPES.cTypedesc,laData[8],GLTYPES.cTypecode,'Typecode')
    ELSE
      lcTypCode = "   "
      lcTypDesc = ""
    ENDIF

    *** Set the pic. for all segment values. & disable all the segments. ***
    FOR lnCount = 1 TO ALEN(laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      IF lnCount <> VAL(laData[1])
        &lcObjNam = SPACE(laSegSize[lnCount,1])
      ENDIF
      SHOW GET &lcObjNam DISABLE
    ENDFOR

    lcObjNam   = 'lcSeg' + ALLTRIM(laData[1])
    &lcObjNam  = laData[2] 

    IF laData[1] = '1' .AND. !EMPTY(laData[2])
      *** If 1st segment, enable account major button. ***
      SHOW GET pbOpen ENABLE
    ELSE
      *** If ! 1st segment disable the button & close the child screen. ***
      SHOW GET pbOpen DISABLE
      IF WVISIBLE(lcChldWind) 
        =gfChClose(lcChldWind)
      ENDIF
    ENDIF
  CASE laScrMode[3] .OR. laScrMode[4]
    *** Edit mode - Add mode ***
    *** Disable all the segment ***
    FOR lnCount = 1 TO ALEN(laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      SHOW GET &lcObjNam DISABLE
    ENDFOR
    IF laDAta[1] = '1' .AND. !EMPTY(laData[2])
      SHOW GET pbOpen    ENABLE
    ELSE
      lcTypCode = "   "
      lcTypDesc = ""
      SHOW GET pbOpen    DISABLE
    ENDIF
ENDCASE

*IF laData[1] = "1"
  =lfValShow()  && Show function of the child screen. ***
*ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfValShow
*!
*!**************************************************************************
*
FUNCTION lfValShow

*** Show function of the child screen. ***

DO CASE

  CASE laScrMode[1]
    *** Set the default values for all the popups. ***
    STORE lcTNonApp TO lcRatioDes,lcCashFDes,lcTNonApp,lcCatgry,;
                       lcStatus,puRatio,puCash
    ibPopTerm  = 1
    ibPopCatg  = 1
    ibPopStat  = 1
    cbPost     = 1

  CASE laScrMode[2]
  
    *** Set the popups values according to its fields. ***
    lcTerm    = IIF(AT(laData[6] , "LC") > 0 , laTTerm[AT(laData[6] , "LC"),1] , lcTNonApp )
    ibPopTerm = AT(laData[6] , "LC")
    lcStatus  = IIF(AT(laData[5] , "AI") > 0 , laTStatus[AT(laData[5] , "AI"),1] , lcTNonApp )
    ibPopStat = AT(laData[5] , "AI")
    lcCatgry  = IIF(AT(laData[7] , "CNDA") > 0 , laTCatgry[AT(laData[7] , "CNDA"),1] , lcTNonApp )
    ibPopCatg = AT(laData[7] , "CNDA")

    IF laData[12] = "Y"
      cbPost  = 1
    ELSE
      cbPost  = 0
    ENDIF
  
    *** Get the ratio group value from GLRACOD. ***
    IF SEEK(LEFT(laData[8],1)+laData[10],"GLRACOD")
      lcRatioDes = GLRACOD.cRatDesc
      puRatio    = GLRACOD.cRatDesc
    ELSE
      lcRatioDes = lcTNonApp
      puRatio    = lcTNonApp
    ENDIF

    *** Get the cash flow value from GLCFITEM. ***
    IF _DOS
      lcCashFDes = LOOKUP(GLCFITEM.cCfidesc,laData[11],GLCFITEM.cSegcaflo,'CFICODE')
      lcCashFDes = IIF(EMPTY(lcCashFDes) , lcTNonApp , lcCashFDes)
    ELSE
      puCash = LOOKUP(GLCFITEM.cCfidesc,laData[11],GLCFITEM.cSegcaflo,'CFICODE')
      puCash = IIF(EMPTY(puCash) , lcTNonApp , puCash)
    ENDIF
  CASE laScrMode[3]

    *** Set filter in the GLRACOD according to current type code. ***
    SELECT GLRACOD
    SET FILTER TO 
    SET FILTER TO CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " "
    SELECT GLSEGVAL 

    IF LEFT(laData[8],1) = 'A' .OR. LEFT(laData[8],1) = 'L'
      SHOW GET ibPopTerm ENABLE
    ELSE
      lcTerm = lcTNonApp
      SHOW GET ibPopTerm DISABLE
    ENDIF

    IF LEFT(laData[8],1) = 'A'
      SHOW GET ibPopCatg ENABLE
    ELSE
      lcCatgry = lcTNonApp
      SHOW GET ibPopCatg DISABLE
    ENDIF

    IF LEFT(laData[8],1) <> 'Y'   && If type code is standard.
      IF _DOS
        SHOW GET ibPopRatio ENABLE
        SHOW GET ibPopCashF ENABLE
      ELSE
        SHOW GET ibRatio ENABLE
        SHOW GET ibCash  ENABLE
        SHOW GET puCash  ENABLE
        SHOW GET puRatio ENABLE
      ENDIF
    ELSE
      STORE lcTNonApp TO lcRatioDes,puRatio,lcCashFDes,puCash
      IF _DOS
        SHOW GET ibPopRatio DISABLE
        SHOW GET ibPopCashF DISABLE
      ELSE
        SHOW GET ibRatio DISABLE
        SHOW GET ibCash  DISABLE
        SHOW GET puRatio DISABLE
        SHOW GET puCash  DISABLE
      ENDIF
    ENDIF
  CASE laScrMode[4]
    
    IF laData[1] = "1"
      *** Set filter in the GLRACOD according to current type code. ***
      SELECT GLRACOD
      SET FILTER TO 
      SET FILTER TO CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " "
      SELECT GLSEGVAL 

      *** If 1st seg. set default for status & allow posting. ***
      lcStatus   = laTStatus [1,1]
      ibPopStat  = 1
      laData[5]  = laTStatus [1,2]
      laData[12] = "Y"
      laData[16] = IIF(SUBSTR(laData[8],1,1) = 'Y' , 'N' , 'Y')
 
      IF LEFT(laData[8],1) = 'A' .OR. LEFT(laData[8],1) = 'L'
        lcTerm    = laTTerm [1,1]
        laData[6] = laTTerm [1,2]
        ibPopTerm = 1
        SHOW GET ibPopTerm ENABLE
      ELSE
        lcTerm    = lcTNonApp
        laData[6] = " "
        ibPopTerm = IIF(_DOS,0,lcTNonApp)
        SHOW GET ibPopTerm DISABLE
      ENDIF

      IF LEFT(laData[8],1) = 'A'
        lcCatgry  = laTCatgry [1,1]
        laData[7] = laTCatgry [1,2]
        ibPopCatg = 1
        SHOW GET ibPopCatg ENABLE
      ELSE
        lcCatgry  = lcTNonApp
        laData[7] = " "
        ibPopCatg = IIF(_DOS,0,lcTNonApp)
        SHOW GET ibPopCatg DISABLE
      ENDIF

      STORE lcTNonApp TO lcRatioDes,puRatio,lcCashFDes,puCash
      IF LEFT(laData[8],1) <> 'Y'
        IF _DOS
          SHOW GET ibPopRatio  ENABLE
          SHOW GET ibPopCashF  ENABLE
        ELSE
          SHOW GET ibRatio ENABLE
          SHOW GET ibCash  ENABLE
          SHOW GET puRatio ENABLE
          SHOW GET puCash  ENABLE
        ENDIF
      ELSE
        IF _DOS
          SHOW GET ibPopRatio DISABLE
          SHOW GET ibPopCashF DISABLE
        ELSE
          SHOW GET ibRatio DISABLE
          SHOW GET ibCash  DISABLE
          SHOW GET puRatio DISABLE
          SHOW GET puCash  DISABLE
        ENDIF
      ENDIF
    ENDIF
ENDCASE

IF laScrMode[1] .OR. laScrMode[2]
  SHOW GET ibPopTerm  DISABLE
  SHOW GET ibPopCatg  DISABLE
  SHOW GET ibPopStat  DISABLE
  SHOW GET cbPost     DISABLE
  IF _DOS
    SHOW GET ibPopRatio DISABLE
    SHOW GET ibPopCashF DISABLE
  ELSE
    SHOW GET ibRatio DISABLE
    SHOW GET ibCash  DISABLE
    SHOW GET puRatio DISABLE
    SHOW GET puCash  DISABLE
  ENDIF
  SHOW GET laData[9]  DISABLE
  SHOW GET laData[13] DISABLE
  SHOW GET laData[14] DISABLE
  SHOW GET laData[15] DISABLE
ELSE
  SHOW GET ibPopStat  ENABLE
  SHOW GET cbPost     ENABLE
  SHOW GET laData[9]  ENABLE
  SHOW GET laData[13] ENABLE
  SHOW GET laData[14] ENABLE
  SHOW GET laData[15] ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_2
*!
*!**************************************************************************
*
FUNCTION lfwData_2

PARAMETERS lnSegmenNo

*** When function of the segment value. ***

lnSegNo = lnSegmenNo
IF laScrMode[1]                && If select mode close child screen.
  IF WVISIBLE(lcChldWind) 
    =gfChClose(lcChldWind)
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

PARAMETERS lcSegVlu

*** Valid function for segment value. ***

lcExacStat = SET("EXACT")

IF !EMPTY(&lcSegVlu) .AND. LASTKEY() = 13
  laData[1] = ALLTRIM(STR(lnSegNo))
  IF &lcSegVlu <> "?"
    &lcSegVlu = RIGHT('000000000000000'+ALLTRIM(&lcSegVlu),laSegSize[lnSegNo,1])
  ENDIF

  laData[2] = ALLTRIM(&lcSegVlu)
  IF ALLTRIM(&lcSegVlu) = "?"
    *** Browse the segments according to the seg. no. that have "?". ***
    FOR lnCount = 1 TO ALEN(laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      &lcObjNam = SPACE(laSegSize[lnCount,1])
      SHOW GET &lcObjNam ENABLE
    ENDFOR
    
    *B600472,1 I'll go to view mode only if return from the browse
    *B600472,1 with .T. because this will mean that ladata[2] is not
    *B600472,1 empty.
    *lcOldSegVl = laData[2]
    *=gfBrows('laData[1]',lcScFields,'laData')
    *IF (lcOldSegVl <> laData[2])
 *ymo (start)
 *   IF gfBrows('laData[1]',lcScFields,'laData') 
 
    lcOldSegVl = laData[2]
    =gfBrows('laData[1]',lcScFields,'laData')
    IF (lcOldSegVl <> laData[2]) .AND. !EMPTY(laData[2])
 *ymo(end)
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ENDIF
    
    lcObjNam = 'lcSeg' + ALLTRIM(STR(lnSegNo))
    _CUROBJ  = OBJNUM(&lcObjNam)

    *RETURN
  ELSE
    *** Check if segment value has no char. ***
    llNumeric=lfDigit(laData[2],.T.)
    IF !llNumeric
      FOR lnCount = 1 TO ALEN(laSegSize,1)
        lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
        &lcObjNam = SPACE(laSegSize[lnCount,1])
        SHOW GET &lcObjNam ENABLE
      ENDFOR
      *** Only numeric value is accepted here !!! ***
      *** < Ok > ***
      = gfModalGen("TRM02005B00000","DIALOG")
      lcObjNam = SYS(18)
      _CUROBJ  = OBJNUM(&lcObjNam)
      *RETURN
    ELSE
      *** If it is the first segment, check that it is not a zero
      IF lnSegNo = 1 .AND. VAL(laData[2]) = 0
        FOR lnCount = 1 TO ALEN(laSegSize,1)
          lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
          &lcObjNam = SPACE(laSegSize[lnCount,1])
          SHOW GET &lcObjNam ENABLE
        ENDFOR
        *** Segment no. 1 cannot be zeros. ***
        *** <  Ok  > ***
        =gfModalGen("TRM02217B00000","DIALOG")
        lcObjNam = SYS(18)
        _CUROBJ  = OBJNUM(&lcObjNam)
      
      ELSE 
        SET EXACT ON
        IF SEEK(laData[1]+laData[2])
          SET EXACT &lcExacStat
          *** If The segment found in the file. ***
          SCATTER FIELDS &lcScFields MEMO TO laData
          laScrMode    = .F.
          laScrMode[2] = .T.       && Go to view mode
          SHOW GETS
        ELSE
           SET EXACT &lcExacStat
          *** If not found in the segment file. ***
          IF laData[1] = "1"
            SET EXACT &lcExacStat
            *** If its 1st segment check in GLTYPES file ***
            *** if this segment between any ranges...... ***
            SELECT GLTYPES
            SET ORDER TO TAG TYPLACNO
            GO TOP
            LOCATE FOR BETWEEN(VAL(laData[2]),VAL(GLTYPES.cTyplacno),;
                               VAL(GLTYPES.cTypuacno))
            IF !FOUND()
              *** This value does not fall within any range. ***
              *** Please enter another value. ***
              *** < Ok > ***
              = gfModalGen("TRM02004B00000","DIALOG")
              laScrMode    = .F.
              laScrMode[1] = .T.
              SHOW GETS
            ELSE
              =lfSelOptn()     && Func. have buttons to select action.
            ENDIF
            SELECT GLTYPES
            SET ORDER TO TAG TYPECODE
            SELECT GLSEGVAL
          ELSE
            =lfSelOptn()       && Func. have buttons to select action.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ELSE
  *** Set segments picture to spaces. ***
  FOR lnCount = 1 TO ALEN(laSegSize,1)
    lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
    &lcObjNam = SPACE(laSegSize[lnCount,1])
    SHOW GET &lcObjNam ENABLE
  ENDFOR
  SHOW GET laData[3] DISABLE
  SHOW GET laData[4] DISABLE
  SHOW GET pbOpen    DISABLE
ENDIF

SET EXACT &lcExacStat

*!**************************************************************************
*!
*!      Function: lfSelOptn
*!
*!**************************************************************************
*
FUNCTION lfSelOptn

IF glAutoAdd         && If automatic add, option = < add >
  lnOption = 2
ELSE
  *** {Field} is not found in the data file. ***
  *** <Browse> <Add> <Reenter> ***
  lnOption = gfModalGen('QRM00001B00001','Dialog',lcTFldTxt+" "+laData[1]+laData[2]) 
ENDIF  

DO CASE
  CASE lnOption = 1         && Browse.
    FOR lnCount = 1 TO ALEN(laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      &lcObjNam = SPACE(laSegSize[lnCount,1])
      SHOW GET &lcObjNam ENABLE
    ENDFOR
    
    *** Browse according to segment no. ***
*    SET EXACT &lcExacStat
    SELECT GLSEGVAL
    
    *B600472,1 I'll go to view mode only if return from the browse
    *B600472,1 with .T. because this will mean that ladata[2] is not
    *B600472,1 empty.
    *lcOldSegVl = laData[2]
    *=gfBrows('laData[1]',lcScFields,'laData')
    *IF lcOldSegVl <> laData[2]
    
    IF gfBrows('laData[1]',lcScFields,'laData')
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ENDIF

    lcObjNam = 'lcSeg' + ALLTRIM(STR(lnSegNo))
    _CUROBJ  = OBJNUM(&lcObjNam)
    RETURN
  CASE lnOption = 2         && Add
    *** If 1st segment get type code & desc. ***
*    SET EXACT &lcExacStat
    IF laData[1] = "1"
      lcTypCode  = GLTYPES.cTypeCode
      lcTypDesc  = GLTYPES.cTypeDesc
      laData[8]  = lcTypCode
    ENDIF
    *** Go to add mode. ***
    laScrMode    = .F.
    laScrMode[4] = .T.
    SHOW GETS
  CASE lnOption = 3         && Reenter
    *** Go back to select mode again. ***
*    SET EXACT &lcExacStat
    laScrMode = .F.
    laScrMode[1]=.T.
    SHOW GETS
ENDCASE
 
*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3
 
*!**************************************************************************
*!
*!      Function: lfvData_4
*!
*!**************************************************************************
*
FUNCTION lfvData_4

_CUROBJ = OBJNUM(laData[3])

*!**************************************************************************
*!
*!      Function: lfvOpen
*!
*!**************************************************************************
*

FUNCTION lfvOpen

*** Function to activate the child window that has ***
*** the account major information if 1st segment.. ***

=gfActWind(lcChldWind,lctActMajr)

*!**************************************************************************
*!
*!      Function: lfSegDes
*!
*!**************************************************************************
*
* Function used in the picture of all short descriptions to define how manay
* Characters to be accepted in each field
*
FUNCTION lfSegDes
PARAMETERS lnWidth

RETURN REPLICATE("X",lnWidth)


*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*
FUNCTION lfvData_5

*** Valid func. for status popup. ***

DO CASE
  CASE _DOS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    IF llSegMd
      laData[5] = gfActPop(11,60,15,75,'laTStatus',2,1,@lcStatus)
    ELSE
      laData[5] = gfActPop(1,60,5,73,'laTStatus',2,1,@lcStatus)
    ENDIF

    =lfRefresh()   && Say refresh func. "lfDosRef" or "lfWinref"

  CASE _WINDOWS
    laData[5] = SUBSTR("AI" , ibPopStat , 1)

ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6

*** Valid func. for Term popup. ***
DO CASE
  CASE _DOS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    IF llSegMd
      laData[6] = gfActPop(11,3,15,17,'laTTerm',2,1,@lcTerm)
    ELSE
      laData[6] = gfActPop(1,2,5,15,'laTTerm',2,1,@lcTerm)
    ENDIF

    =lfRefresh()    && Say refresh func. "lfDosRef" or "lfWinref"
  CASE _WINDOWS
    laData[6] = SUBSTR("LC" , ibPopTerm , 1)

ENDCASE
 
*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7

*** Valid func. for categry popup. ***

DO CASE
  CASE _DOS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    IF llSegMd
      laData[7] = gfActPop(11,31,17,45,'laTCatgry',2,1,@lcCatgry)
    ELSE
      laData[7] = gfActPop(1,30,7,45,'laTCatgry',2,1,@lcCatgry)
    ENDIF

    =lfRefresh()   && Say refresh func. "lfDosRef" or "lfWinref"

  CASE _WINDOWS
   laData[7] = SUBSTR("CNDA" , ibPopCatg , 1)

ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_10
*!
*!**************************************************************************
*
FUNCTION lfvData_10

*** Valid func. for ratio group popup. ***

DO CASE
  CASE _DOS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    IF llSegMd
      laData[10] = gfActPop(14,23,21,64,'GLRACOD','CSEGRATIO','CRATDESC',@lcRatioDes)
    ELSE
      laData[10] = gfActPop(4,21,10,61,'GLRACOD','CSEGRATIO','CRATDESC',@lcRatioDes)
    ENDIF
    =lfRefresh()    && Say refresh func.
  CASE _WINDOWS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    puRatio    = GLRACOD.CRATDESC
    laData[10] = GLRACOD.CSEGRATIO
    SHOW GET puRatio
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puRatioGrp
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_11
*!
*!**************************************************************************
*
FUNCTION lfvData_11

*** Valid func. for Cash flow popup. ***

DO CASE
  CASE _DOS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    IF llSegMd
      laData[11]  = gfActPop(15,23,22,71,'GLCFITEM','CSEGCAFLO','CCFIDESC',@lcCashFDes)
    ELSE
      laData[11]  = gfActPop(5,21,11,68,'GLCFITEM','CSEGCAFLO','CCFIDESC',@lcCashFDes)
    ENDIF
    =lfRefresh()   && Say refresh func.
  CASE _WINDOWS
    *** Call activate popup func. from gl.prg with different  ***
    *** position depending on if this program called from the ***
    *** menu or from account validation..
    puCash     = GLCFITEM.CCFIDESC
    
    *B804399,1 Correcting the field to be laData[11]. [Begin]
    *laData[10] = GLCFITEM.CSEGCAFLO
    laData[11] = GLCFITEM.CSEGCAFLO
    *B804399,1 Correcting the field to be laData[11]. [End]
    
    SHOW GET puCash
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puCashFlow
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_12
*!
*!**************************************************************************
*
FUNCTION lfvData_12

*** Valid func. for allow posting check box. ***

laData[12] = IIF( cbPost = 0 , "N" , "Y" ) 

*!**************************************************************************
*!
*!      Function: lfwData_13
*!
*!**************************************************************************
*
FUNCTION lfwData_13

*** When func. for automatic distribution. ***

lcOldAuto = laData[13]

*!**************************************************************************
*!
*!      Function: lfvData_13
*!
*!**************************************************************************
*
FUNCTION lfvData_13

*** Valid func. for automatic distribution. ***

DECLARE laAutoInfo[1,1]

IF !EMPTY(laData[13]) .OR. llBrowse
 
  IF lcOldAuto <> laData[13] .OR. llBrowse

    IF SEEK("D","GLAUTHD")

      lcCurFile = ALIAS()
      SELECT GLAUTHD
      *** If found update the user name from automatic dist. file

      IF !SEEK("D"+laData[13],"GLAUTHD") .OR. llBrowse  
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
            GO RECNO(0)
        ELSE
          GO TOP
        ENDIF

        lcSaveBrow     =  lcBrFields  && Save old valu of the variable
        lcBrFields     =  "cautcode :H='Distribution code',cautref :H='Distribution reference',"+;
                          "cautdes  :H='Distribution description'"
        lcSav_ttl      = lcFile_Ttl
        lcFile_Ttl     = "Automatic Distribution"
  
        laAutoInfo [1] = laData[13]

        =gfBrows("'D'","cautcode" ,"laAutoInfo")
    
        lcBrFields = lcSaveBrow 
        lcFile_Ttl = lcSav_ttl


        *** If cancel from the brow you will come back  with the same id
        IF laData[13] = laAutoInfo[1] 
          laData[13]  = lcOldAuto
        ELSE
          laData[13]  = laAutoInfo[1]

          SELECT GLAUTDT
          SET ORDER TO TAG TYPCODACC
          SELECT GLACCHAR
          SET ORDER TO TAG ACCTCODE
          SELECT GLAUTDT
          SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE

          SELECT GLAUTDT
          IF SEEK("D"+laData[13],"GLAUTDT")
            llAutcd_Ok = .T.
            SCAN REST WHILE GLAUTDT.cAutcode = laData[13]
              IF GLACCHAR.cSegactiv = "I"
                *** This  automatic  distribution has an ***
                *** inactive account.  You cannot select ***
                *** this automatic distribution code. ***
                *** <  Ok  > ***
                =gfModalGen("TRM02194B00000","DIALOG")
                llAutcd_Ok = .F.
                EXIT
              ENDIF
              
              IF GLACCHAR.cStandard <> laData[16]
                *** This automatic distribution code ***
                *** has a { standard : statistical } ***
                *** account.  You cannot select this ***
                *** automatic distribution code.
                *** <  Ok  > ***
                lcStandStr = IIF(GLACCHAR.cStandard = "Y","standard","statistical")
                =gfModalGen("TRM02195B00000","DIALOG",lcStandStr)
                llAutcd_Ok = .F.
                EXIT
              ENDIF
              
              SELECT GLAUTDT
            ENDSCAN
            IF !llAutcd_Ok
              laData[13]  = lcOldAuto
            ENDIF 
          ENDIF
          
          SELECT GLAUTDT
          SET RELATION TO
        ENDIF
      ENDIF
      SELECT (lcCurFile)
    ELSE

      *** There is no records to display. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00052B00000","DIALOG")
      laData[13] = SPACE(8)
    ENDIF
  ENDIF
ENDIF

SHOW GET laData[13]
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvData_9
*!
*!**************************************************************************
*
*** The report seq. no. ***

FUNCTION lfvData_9

IF laData[9] < 0
  *** Negative values are not allowed. ***
  =gfModalGen("TRM02036B00000","DIALOG")
  laData[9] = 0
  SHOW GET laData[9]
  _CUROBJ    = OBJNUM(laData[9])
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_15
*!
*!**************************************************************************
*
FUNCTION lfvData_15

*** Valid func. for consolidation per. ***

DO CASE
  CASE laData[15] > 100
    *** Total percentage cannot exceed 100 %. ***
    =gfModalGen("TRM02017B00000","DIALOG",lcTPrcntg)
    laData[15] = 0
    SHOW GET laData[15]
    _CUROBJ    = OBJNUM(laData[15])
  CASE laData[15] < 0
    *** Negative values are not allowed. ***
    =gfModalGen("TRM02036B00000","DIALOG")
    laData[15] = 0
    SHOW GET laData[15]
    _CUROBJ    = OBJNUM(laData[15])
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"

PROCEDURE lpSavScr
EXTERNAL ARRAY laScrMode    

SELECT GLSEGVAL

IF laData[1] = "1"
  *** If 1st segment put the type standard or statistical. ***
  laData[16] = IIF(SUBSTR(laData[8],1,1) = 'Y' , 'N' , 'Y')
ENDIF

IF laScrMode[4]
  APPEND BLANK
ENDIF

GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
* This procedure replaces the default delete procedure for the push button
* "DELETE"

PROCEDURE lpDelScr

*E300692,1 Change file name from SYCACCOD to ACCOD  
*SELECT SYCACCOD

SELECT ACCOD
*E300692,1 end

*E300789,7 [BEGIN] The following code will be commented till *E300789,7 [END..]
*The modified code will start with E300789,7 [BEGIN] 
*SET ORDER TO COMPID

*lnConter   = 1
*lnStartPos = 1

*E300692,1 Change file name from SYCACCOD to ACCOD  
*IF SEEK(gcAct_comp,"SYCACCOD")
  *lnAryLenth = SYCACCOD.NACSNOSEG
*IF SEEK(gcAct_comp,"ACCOD")
*  lnAryLenth = ACCOD.NACSNOSEG
*E300692,1 end  
  
*  DECLARE laSegments[lnAryLenth,2]

*  SKIP
*  SCAN WHILE cComp_Id = gcAct_Comp
*    laSegments [lnConter,1] = lnStartPos
  *E300692,1 Change file name from SYCACCOD to ACCOD    
    *laSegments [lnConter,2] = SYCACCOD.NACSSIZE
    *lnStartPos              = lnStartPos + SYCACCOD.NACSSIZE + 1
*    laSegments [lnConter,2] = ACCOD.NACSSIZE
*    lnStartPos              = lnStartPos + ACCOD.NACSSIZE + 1
  *E300692,1 end  
    
*    lnConter                = lnConter + 1 
*  ENDSCAN
*ENDIF
*E300789,7 [END..]

*E300789,7 [BEGIN] Start the new code 
*SET ORDER TO COMPID

lnConter   = 1
lnStartPos = 1

*E300692,1 Change file name from SYCACCOD to ACCOD  
*IF SEEK(gcAct_comp,"SYCACCOD")
  *lnAryLenth = SYCACCOD.NACSNOSEG
GO TOP
IF !EOF()
  lnAryLenth = NACSNOSEG
*E300692,1 end  
  
  DECLARE laSegments[lnAryLenth,2]

  SKIP
  SCAN REST
    laSegments [lnConter,1] = lnStartPos
  *E300692,1 Change file name from SYCACCOD to ACCOD    
    *laSegments [lnConter,2] = SYCACCOD.NACSSIZE
    *lnStartPos              = lnStartPos + SYCACCOD.NACSSIZE + 1
    laSegments [lnConter,2] = NACSSIZE
    lnStartPos              = lnStartPos + NACSSIZE + 1
  *E300692,1 end  
    
    lnConter                = lnConter + 1 
  ENDSCAN
ENDIF

*E300789,7 [END..] End the new code

lcTmpAcCd   = REPLICATE('_',lnAcsSegSz)

lcTmpAcCd   = stuff(lcTmpAcCd,laSegments[VAL(laData[1]),1],;
                    laSegments[VAL(laData[1]),2],;
                    ALLTRIM(laData[2]))

*** Check that there is no Accounts will be without ***
*** segments if this deletion happen.... 
SELECT GLACCHAR

SELECT CACCTCODE,CACCNSDES ;
     FROM &gcDataDir.GLACCHAR ;
     WHERE CACCTCODE LIKE lcTmpAcCd;
     INTO DBF &gcWorkDir.&lc_TempAc

SELECT (lc_TempAc)
IF _TALLY > 0

  *** There is one or more accounts using this segment value. ***
  *** You cannot delete this segment value.  Do you want to ***
  *** display these accounts ? ***
  *** <  Yes  > <  No  > ***
  IF gfModalGen("QRM02169B00006","DIALOG") = 1
    PUSH KEY
      
    *** Trap few keys to control the browse ***
    ON KEY LABEL ESC        DO lfTrpAc
    ON KEY LABEL TAB        DO lfTrpAc
    ON KEY LABEL BACKTAB    DO lfTrpAc
    ON KEY LABEL CTRL+W     lnDumi = 1
    ON KEY LABEL CTRL+Q     lnDumi = 1
    ON KEY LABEL CTRL+END   lnDumi = 1
    *E300683,5 Call *.SPR from screens directory
    * DO GLSGACT.SPR 
    DO (gcScrDir + gcWinAppl + '\GLSGACT.SPR')
    *E300683,5 end        
    *** Clean the keys trapped before calling the browse screen ***
    =lfKeyClean()
  ENDIF
  SELECT GLSEGVAL
ELSE
  SELECT GLSEGVAL

  DELETE
   
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF    
  
***********************************************************************
*!
*!      FUNCTION: lfvSegOk
*!
*!**************************************************************************
* 
FUNCTION lfvSegOK

*** This is the saving func. that branched from the ok ***
*** button if the program called from account valid.   ***

glQuitting = .T.    && To enable the segment bar in the menu after closing. 
llSave     = .T.    && To know that this value added in the seg. file.

SELECT GLSEGVAL

IF laData[1] = "1"
  *** If 1st segment put the type standard or statistical. ***
  laData[16] = IIF(SUBSTR(laData[8],1,1) = 'Y' , 'N' , 'Y')
ENDIF

APPEND BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()

lcTSegVal = lcTSegVal+" "+ALLTRIM(laData[2])
WAIT lcTSegVal WINDOW TIMEOUT 2

*!**************************************************************************
*!
*!      Function: lfWhBrow
*!
*!**************************************************************************
*
FUNCTION lfWhBrow

BROWSE FIELDS cAcctcode:H="Account code strucure",cAccnsdes:H='Description';
       NOAPPEND;
       NODELETE;
       NOEDIT;
       NOCLEAR;
       LOCK 0;
       SAVE;
       NOWAIT;
       TITLE lcTitleAct;
       WINDOW GLSGACT2 IN WINDOW AWDGLSGACT

*!**************************************************************************
*!
*!      Function: lfTrpAc
*!
*!**************************************************************************
*
FUNCTION lfTrpAc

*** Trabing the tab & esc key for the account screen ***

DO CASE
  CASE LASTKEY() = 27           && If press ESC, close the screen.
    ACTIVATE WINDOW GLSGACT1
    _CUROBJ = OBJNUM(pbOk)
    KEYBOARD "{ENTER}"
  CASE LASTKEY() = 9 .OR. LASTKEY() = 15  && If press TAB or Shift+tab
    IF WONTOP() = lcTitleAct
      ACTIVATE WINDOW GLSGACT1
      _CUROBJ = OBJNUM(pbOk)
    ELSE
      IF _CUROBJ = OBJNUM(pbOk)
        ACTIVATE WINDOW (lcTitleAct)
      ELSE
        _CUROBJ = _CUROBJ + 1
      ENDIF
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfKeyClean
*!
*!**************************************************************************
*
FUNCTION lfKeyClean

ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+END
