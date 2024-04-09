*:************************************************************************
*:
*: Procedure file: GLTYPE.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow     *** Show proc. for the home screen ***
*:                 lfvAccTyp  *** The popup valid function ***
*:                 lfvData_1  *** The laData[1] valid function ***
*:                 lfwData_5  *** The lower limit when function ***
*:                 lfvData_5  *** The lower limit valid function ***
*:                 lfwData_6  *** The upper limit when function ***
*:                 lfvData_6  *** The upper limit valid function ***
*:                 lfvLwUp1   *** ranges <> 0 & upper > lower limit ***
*:                 lfvChkSeg  *** Check if the temp file empty or not ***
*:                 lfTrpAc    *** To trap the keys inside the browse screen ***
*:                 lfKeyClean *** To clean the traping keys after close browse ***
*:                 lfLowUpr   *** To know the width of the ranges ***
*:                 lpSavScr   *** Save the new record ***
*:                 lpDelScr   *** Delete record ***
*:               
*:      Documented 12/22/1993
*:************************************************************************
*B600376,1 Reham On 06/03/95 
*B600376,1 1- If press esc from the browse, back to select mode.
*B600376,1 2- If left the screen & back again, restore the right 
*B600376,1    value of the type category.
*E700014,1 Hesham El-Sheltawi 04/01/96
*E700014,1 defaulting the report short desc. and long desc. with
*E700014,1 Type Description
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*:************************************************************************
*

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc

DECLARE laKeyField [1,4]       && Have the tag information.
laKeyField[1,1] = 'laData[1]'  && The var that has the index expr.
laKeyField[1,2] =.T.           && If this field terminatig for index or not.
laKeyField[1,3] = 'TYPECODE'   && Tag name.
laKeyField[1,4] = 1            && Position of field inside index.

DECLARE laConTyp   [9,1]   && Combine between laTAccTyp & Desc. of main types.
DECLARE laDescrip  [1,2]   && Hold the desc. of the main types

*** These variables declared in the screen object file. ***
lcTitleAct = ""        && Var hold the the browse title. "Segments values"
 
lc_TempAc  = ""        && The name of the temp file that have the segments.
lcLowUpr   = ""        && The parameter in the lfvChkSeg func. (lower-upper).
lcLwUp     = ""        && Parametetr used in the validation of (lower-upper).
lcAccTyp1  = "A"       && The 1st ch. from the code type.

lcOldVal   = ""        && Old Lower or Upper acc. no. limit.
lcOldLow   = ""        && The old lower limit.
lcOldUpr   = ""        && The old upper limit.

lnUprRng   = 0         && The max. value of Upper acc. no.
lnRec      = 0         && Keep the record no.
lnWidth    = 0         && The width of the lower or upper limit.
puAccTyp   = 1         && Set the popup value to 1st type.

llFindFlag    = .F.       && To know if lower or upper is valid or not.
laDefProc     = .T.
laDefProc [7] = .F.    && Disable the delete button.
laDefProc [9] = .F.    && Disable the save button.

*** Declared in screen obj. file have types desc. ***
DECLARE laTAccTyp  [9,1]
laTAccTyp[1,1] = "Asset"
laTAccTyp[2,1] = "Cost of sales"
laTAccTyp[3,1] = "Equity"
laTAccTyp[4,1] = "Other Income"
laTAccTyp[5,1] = "Liability"
laTAccTyp[6,1] = "Sales"
laTAccTyp[7,1] = "Taxes"
laTAccTyp[8,1] = "Expenses"
laTAccTyp[9,1] = "Statistical"

*** Function in the main program ***
*** Intialise all the variables & open all the files needed   ***
*** in this session and controling disabling and enabling of  ***
*** the menu bars and writting the screen names in the window ***
*** bars ....

IF !gfSetup()    
  RETURN
ENDIF  

*** The string that have the browse fields in the gltype file ***
lcBrFields  = "cTypecode :H='Code',lcBelong=laTAccTyp[AT(SUBSTR(GLTYPES."+;
              "cTypeCode,1,1),'ACQILSTEY')] :H='Belongs to',cTypedesc"+ ;
              ":H='Description',cTyplacno :H='Lower',cTypuacno :H='Upper'"

IF !WEXIST(gcBaseWind)
  lc_TempAc  = gfTempName()

  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  *** Save the width of lower or upper from the width of 1st seg. ***
*Change file name from SYCACCOD to ACCOD  
  *SELECT SYCACCOD
  SELECT ACCOD
*E300692,1 end 

*E300789,7 [BEGIN] The following code will be commented till *E300789,7 [END..]
*The modified code will start with E300789,7 [BEGIN]
*The main purpose to from this modification is to reflect the removing of
* ccomp_id field from ACCOD file .
*  SET ORDER TO COMPID
*Change file name from SYCACCOD to ACCOD    
  *IF SEEK(gcAct_Comp,"SYCACCOD")
*  IF SEEK(gcAct_Comp,"ACCOD")
*E300692,1 end  
*    SKIP
  *Change file name from SYCACCOD to ACCOD      
    *lnWidth = SYCACCOD.NACSSIZE
*    lnWidth = ACCOD.NACSSIZE
  *E300692,1 end  
*  ENDIF
*E300789,7 [END..]

*E300789,7 [BEGIN] -------------------- Start the new code
*  SET ORDER TO COMPID
*Change file name from SYCACCOD to ACCOD    
  *IF SEEK(gcAct_Comp,"SYCACCOD")
  GO TOP 
  IF !EOF()
*E300692,1 end  
    SKIP
  *Change file name from SYCACCOD to ACCOD      
    *lnWidth = SYCACCOD.NACSSIZE
    lnWidth = ACCOD.NACSSIZE
  *E300692,1 end  
  ENDIF
*E300789,7 [END..] -------------------- End the new code


  SELECT GLTYPES
  
  *** Select all desc. of the main types ***
  SELECT cTypecode,cTypedesc FROM &gcDataDir.GLTYPES;
         WHERE SUBSTR(cTypeCode,2,2) = '00';
         INTO ARRAY laDescrip

  DIMENSION laDescrip[9,2],laDescTyp[9,2]
  =ACOPY(laDescrip,laDescTyp)
  =gfADel(@laDescTyp,2,2)

  laConTyp = ' '

  *** Combine the 2 arrays(laTAccTyp & laDescTyp) ***
  *** in one array for the popup....              ***
  FOR lnCount = 1 TO 9
    laConTyp [lnCount] = laTAccTyp[lnCount] + ;
    SPACE(19-LEN(ALLTRIM(laTAccTyp[lnCount]))) + '   ' + ;
    IIF(ASCAN(laDescTyp,SUBSTR('ACQILSTEY',lnCount,1))>0 , ;
    laDescrip[ASCAN(laDescTyp,SUBSTR('ACQILSTEY',lnCount,1)),2] , ' ')
  ENDFOR

ENDIF

*B600376,1 Reham On 06/03/95 If left the screen & back again, restore the
*B600376,1 right value of the type category.
puAccTyp  = AT(lcAccTyp1,'ACQILSTEY')
 
*E300683,5 Call *.SPR from screens directory
* DO Gltype.SPR 
DO (gcScrDir + gcWinAppl + '\Gltype.SPR')
*E300683,5 end   
*** Before quitting from the screen & the program erase ***
*** all the temp. files created in this prgogram.       ***
IF glQuitting
  IF USED(lc_TempAc)
    USE IN ALIAS(lc_TempAc)
  ENDIF
  ERASE &gcWorkDir.&lc_TempAc..DBF
  ERASE &gcWorkDir.&lc_TempAc..FPT
  ERASE &gcWorkDir.&lc_TempAc..CDX
ENDIF
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE 

  *** Select mode ***
  CASE laScrMode[1]
    puAccTyp  = AT(lcAccTyp1,'ACQILSTEY')

  *** View mode ***
  CASE laScrMode[2]
    lnRec     = RECNO()
    lcAccTyp1 = SUBSTR(GLTYPES.cTypecode,1,1)
    puAccTyp  = AT(lcAccTyp1,'ACQILSTEY')
    laData[1] = SUBSTR(GLTYPES.cTypecode,2,2)
    
    lcOldLow  = laData[5]    && Save the old lower limit.
    lcOldUpr  = laData[6]    && Save the old upper limit.


  *** Edit mode ***
  CASE laScrMode[3]
    lcAccTyp1 = SUBSTR(GLTYPES.cTypecode,1,1)
    puAccTyp  = AT(lcAccTyp1,'ACQILSTEY')
    laData[1] = SUBSTR(GLTYPES.cTypecode,2,2)
    
    IF laData[1] = "00"
      SHOW GET laData[5] DISABLE
      SHOW GET laData[6] DISABLE
    ENDIF

  *** Add mode ***
  CASE laScrMode[4]
    IF LEN(laData[1]) > 2
      lcAccTyp1 = SUBSTR(laData[1],1,1)
      puAccTyp  = AT(lcAccTyp1,'ACQILSTEY')
      laData[1] = SUBSTR(laData[1],2,2)
      
    ENDIF
    IF laData[1] = "00"
      *** If its main type, disable the lower & upper limit. ***
      SHOW GET laData[5] DISABLE
      SHOW GET laData[6] DISABLE
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvAccTyp
*!
*!**************************************************************************
*
FUNCTION lfvAccTyp

*** The popup valid function ***

lcAccTyp1  =  SUBSTR('ACQILSTEY',puAccTyp,1)

SHOW GET lcAccTyp1 DISABLE

_CUROBJ = OBJNUM(laData[1])

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

*** The laData[1] valid function ***

IF !EMPTY(laData[1]) .AND. LASTKEY() = 13
  IF LEFT(laData[1],1) = "?" 
    *B600376,1 Reham On 06/03/95 If press esc from the browse, back to select mode.
    *lcOldTyp = laData[1]
    lcOldTyp  = "   "
    =gfBrows('lcAccTyp1')
    IF lcOldTyp <> laData[1]
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ELSE
      laData[1] = ""
      _CUROBJ   = OBJNUM(laData[1])
    ENDIF
    RETURN
  ELSE
    llNumeric=lfDigit(ALLTRIM(laData[1]),.T.)
  
    IF !llNumeric

      *** Only numeric value is accepted here !!! ***
      *** < Ok > ***
      =gfModalGen("TRM02005B00000","DIALOG")
      laData[1] = ""
      _CUROBJ   = OBJNUM(laData[1])
      RETURN
    ELSE
      laData[1] = RIGHT('00'+ALLTRIM(laData[1]),2)
      laData[1] = lcAccTyp1 + laData[1]
      =gfSeekRec()
      laData[1] = RIGHT(laData[1],2)
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_5
*!
*!**************************************************************************
*
FUNCTION lfwData_5

*** The lower limit when function ***

lcOldVal = laData[5]    && Save the old entered value.

*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*
FUNCTION lfvData_5

*** The lower limit valid function ***

IF !EMPTY(laData[5]) .AND. laData[5] <> lcOldVal
  laData[5] = RIGHT('000000000000000'+ALLTRIM(laData[5]),lnWidth)
  SHOW GET laData[5] ENABLE
  lnRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
  
  *** Check if its not = 0 & less than upper limit ***
  IF lfvLwUp1('laData[5]')
    GO TOP
    *** Check if there is any existed types overlap this range ***
    IF EMPTY(laData[6])
      LOCATE FOR ALLTRIM(lcAccTyp1+laData[1]) <> cTypeCode .AND.;
             BETWEEN(VAL(laData[5]),VAL(cTyplacno),VAL(cTypuacno))
    ELSE
      LOCATE FOR ALLTRIM(lcAccTyp1+laData[1]) <> cTypeCode .AND.;
             BETWEEN(VAL(cTypuacno),VAL(laData[5]),VAL(laData[6]))
    ENDIF

    IF FOUND()
      llFindFlag = .T.
      IF laScrMode[3]
        *** Ranges are overlapped for type ð. ***
        *** < Cancel > < Edit > ***
        IF gfModalGen("QRM02001B02000","DIALOG",ALLTRIM(GLTYPES.cTypshdes)) = 1   && Cancel
          laData[5] = lcOldVal
          SHOW GET laData[5] ENABLE
          _CUROBJ  = OBJNUM(laData[5])
        ELSE                                                         && If edit
          laData[5] = SPACE(lnWidth)
          SHOW GET laData[5] ENABLE
          _CUROBJ  = OBJNUM(laData[5])
        ENDIF
      ELSE
        *** Ranges are overlapped for type ð. ***
        *** < OK > ***
        =gfModalGen("TRM02001B00000","DIALOG",ALLTRIM(GLTYPES.cTypshdes))
        laData[5] = SPACE(lnWidth)
        SHOW GET laData[5] ENABLE
        _CUROBJ  = OBJNUM(laData[5])
      ENDIF
    ENDIF
  
    IF lnRec > 0
      GO lnRec
    ENDIF  

    IF !llFindFlag
      *** If editing an existing type with a higher lower ***
      *** limit check if there is any segments will be    ***
      *** without types ...                               ***
      IF laScrMode[3] .AND. VAL(laData[5]) > VAL(lcOldVal)

        *** Select from the segment values file all the segment ***
        *** that between the old lower limit & the new value... ***
        SELECT GLSEGVAL
        SELECT cSegvalue,cSeglndes FROM &gcDataDir.GLSEGVAL;
               WHERE BETWEEN(VAL(cSegvalue),VAL(lcOldVal),VAL(laData[5])-1);
                     .AND. cAcssegno = "1" .AND.  ;
                     cTypecode = ALLTRIM(lcAccTyp1+laData[1]);
               INTO DBF &gcWorkDir.&lc_TempAc
        =lfvChkSeg('laData[5]')
      ENDIF
    ELSE
      llFindFlag = .F.
    ENDIF

    SELECT GLTYPES
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_6
*!
*!**************************************************************************
*
FUNCTION lfwData_6

*** The upper limit when function ***

lcOldVal = laData[6]

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6

*** The upper limit valid function ***

IF !EMPTY(laData[6]) .AND. laData[6] <> lcOldVal
  laData[6] = RIGHT('000000000000000'+ALLTRIM(laData[6]),lnWidth)
  SHOW GET laData[6] ENABLE
  lnRec = IIF(RECNO()>RECCOUNT(),0,RECNO())

  *** Check if its not = 0 & less than upper limit ***
  IF lfvLwUp1('laData[6]')
    GO TOP

    *** Check if there is any existed types overlap this range ***
    IF EMPTY(laData[5])
      LOCATE FOR ALLTRIM(lcAccTyp1+laData[1]) <> cTypeCode .AND.;
             BETWEEN(VAL(laData[6]),VAL(cTyplacno),VAL(cTypuacno))
    ELSE
      LOCATE FOR ALLTRIM(lcAccTyp1+laData[1]) <> cTypeCode .AND.;
             BETWEEN(VAL(cTyplacno),VAL(laData[5]),VAL(laData[6]))
    ENDIF
    IF FOUND()
      llFindFlag = .T.
      IF laScrMode[3]

        *** Ranges are overlapped for type ð. ***
        *** < Cancel > < Edit > ***
        IF gfModalGen("QRM02001B02000","DIALOG",ALLTRIM(GLTYPES.cTypshdes)) = 1   && Cancel
          laData[6] = lcOldVal
          SHOW GET laData[6] ENABLE
          _CUROBJ  = OBJNUM(laData[6])
        ELSE                                                         && If edit
          laData[6] = SPACE(lnWidth)
          SHOW GET laData[6] ENABLE
          _CUROBJ  = OBJNUM(laData[6])
        ENDIF
      ELSE
        *** Ranges are overlapped for type ð. ***
        *** < OK > ***
        =gfModalGen("TRM02001B00000","DIALOG",ALLTRIM(GLTYPES.cTypshdes))
        laData[6] = SPACE(lnWidth)
        SHOW GET laData[6] ENABLE
        _CUROBJ  = OBJNUM(laData[6])
      ENDIF
    ENDIF
  
    IF lnRec > 0
      GO lnRec
    ENDIF  

    IF !llFindFlag
      *** If editing an existing type with a higher lower ***
      *** limit check if there is any segments will be    ***
      *** without types ...                               ***
      IF laScrMode[3] .AND. VAL(laData[6]) < VAL(lcOldVal)

        *** Select from the segment values file all the segment ***
        *** that between the old lower limit & the new value... ***
        SELECT GLSEGVAL
        SELECT cSegvalue,cSeglndes FROM &gcDataDir.GLSEGVAL;
               WHERE BETWEEN(VAL(cSegvalue),VAL(laData[6]),VAL(lcOldVal)+1);
                     .AND. cAcssegno = "1" .AND.  ;
                     cTypecode = ALLTRIM(lcAccTyp1+laData[1]);
               INTO DBF &gcWorkDir.&lc_TempAc
        =lfvChkSeg('laData[6]')
      ENDIF
    ELSE
      llFindFlag = .F.
    ENDIF

    SELECT GLTYPES
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function : lfvLwUp1
*!
*!**************************************************************************
*

FUNCTION lfvLwUp1

PARAMETERS lcLwUp

*** Validate that no range has zero value ***
IF VAL(&lcLwUp) = 0
  *** Ranges cannot be zero !!! ***
  *** <   Ok   > ***
  = gfModalGen("TRM02041B00000","DIALOG")
  &lcLwUp = lcOldVal
  SHOW GET &lcLwUp ENABLE
  _CUROBJ = OBJNUM(&lcLwUp)
  RETURN .F.
ENDIF

*** Validate that the upper limit is higher than the lower limit ***
IF (!EMPTY(laData[5]) .AND. !EMPTY(laData[6])) .AND. laData[5] > laData[6]
  *** Upper limit range should be greater ***
  *** than the lower limit range.         ***
  *** <   Ok   > ***
  = gfModalGen("INM02003B00000","DIALOG"," ")
  &lcLwUp = lcOldVal
  SHOW GET &lcLwUp ENABLE
  _CUROBJ = OBJNUM(&lcLwUp)
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function : lfvChkSeg
*!
*!**************************************************************************
*

FUNCTION lfvChkSeg

PARAMETERS lcLowUpr

*** Check if the temp. file used in the sql ***
*** from the segment file is not empty ...  ***

SELECT (lc_TempAc)
GO TOP

IF !EMPTY(cSegvalue)
  *** This modification will cause one or more  ***
  *** accounts to be without a type. ***
  *** < Cancel > < Edit > ***
  lnOption = gfModalGen("QRM02002B02001","DIALOG")
  DO CASE
    CASE lnOption = 1                     && Show
      
      *** Browse all the segments that overlap with ***
      *** the new value in 'Segment List Screen'... ***
      &lcLowUpr = lcOldVal
      SHOW GET &lcLowUpr ENABLE
      PUSH KEY
      
      *** Trap few keys to control the browse ***
      ON KEY LABEL ESC        DO lfTrpAc
      ON KEY LABEL TAB        DO lfTrpAc
      ON KEY LABEL CTRL+W     lnDumi = 1
      ON KEY LABEL CTRL+Q     lnDumi = 1
      ON KEY LABEL CTRL+END   lnDumi = 1
      *E300683,5 Call *.SPR from screens directory
      * DO GLLSACT.SPR 
      DO (gcScrDir + gcWinAppl + '\GLLSACT.SPR')
      *E300683,5 end  
      *** Clean the keys trapped before calling the browse screen ***
      =lfKeyClean()

    CASE lnOption = 2                     && Cancel
      &lcLowUpr = lcOldVal
      SHOW GET &lcLowUpr ENABLE
      _CUROBJ   = OBJNUM(&lcLowUpr)
    CASE lnOption = 3                     && Edit
      &lcLowUpr  = SPACE(lnWidth)
      SHOW GET &lcLowUpr ENABLE
      _CUROBJ   = OBJNUM(&lcLowUpr)
  ENDCASE
ENDIF
SELECT GLTYPES

*!**************************************************************************
*!
*!      Function: lfBrowSeg
*!
*!**************************************************************************
*
FUNCTION lfBrowSeg

BROWSE FIELDS cSegvalue:H="Segment value",cSeglndes:H='Description';
       NOAPPEND;
       NODELETE;
       NOEDIT;
       NOCLEAR;
       LOCK 0;
       SAVE;
       NOWAIT;
       TITLE lcTitleAct;
       WINDOW GLLSACT2 IN WINDOW AWDGLLSACT

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
     ACTIVATE WINDOW GLLSACT1
    _CUROBJ = OBJNUM(pbOk)
    KEYBOARD "{ENTER}"
  CASE LASTKEY() = 9 .OR. LASTKEY() = 15  && If press TAB or Shift+tab
    IF WONTOP() = lcTitleAct
      ACTIVATE WINDOW GLLSACT1
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

*!**************************************************************************
*!
*!      Function: lfLowUpr
*!
*!**************************************************************************
*
FUNCTION lfLowUpr

PARAMETERS lnWidth

*** Function to define the lower or upper limit picture ***

RETURN REPLICATE("9",lnWidth)

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"

PROCEDURE lpSavScr
EXTERNAL ARRAY laScrMode    

*** If it is not a main type & empty of lower or     ***
*** upper limit accounts return again to enter them. ***
IF laData[1] <> '00' .AND.;
   (EMPTY(laData[5]) .OR. EMPTY(laData[6]))
  *** You cannot save type with empty ranges !!! ***
  *** <   Ok   > ***
  = gfModalGen("INM02021B00000","DIALOG")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(laData[5])
  RETURN
ENDIF

SELECT GLTYPES
IF laData[1] = '00'
  *** If saving main type, put its desc. in the popup. ***
  IF laScrMode[3]
    laConTyp[puAccTyp] = ALLTRIM(LEFT(laConTyp[puAccTyp],22))
  ENDIF
  laConTyp[puAccTyp] = ALLTRIM(laConTyp[puAccTyp]) + SPACE(22-LEN(ALLTRIM(laConTyp[puAccTyp]))) + ALLTRIM(laData[2])
ENDIF

IF laData[1] <> '00' .AND. laScrMode[3]
  
  *** Check if change lower limit, if there is ***
  *** any types overlap with the new value.... ***
  IF lcOldLow < laData[5]

    *** Select from the segment values file all the segment ***
    *** that between the old lower limit & the new value... ***
    SELECT GLSEGVAL
    SELECT cSegvalue,cSeglndes FROM &gcDataDir.GLSEGVAL;
           WHERE cAcssegno = "1" .AND.  ;
                 cTypecode = ALLTRIM(lcAccTyp1+laData[1]) .AND.;
                 BETWEEN(VAL(cSegvalue),VAL(lcOldLow),VAL(laData[5])-1);
           INTO DBF &gcWorkDir.&lc_TempAc
  
    SELECT (lc_TempAc)
  
    IF !EMPTY(cSegvalue)
      *** This modification will cause one or more ***
      *** accounts to be without a type. ***
      *** <   Ok   > ***
      lnOption = gfModalGen("QRM02002B00000","DIALOG")
      SELECT GLTYPES
      *** Set the field value to  the old  value has ***
      *** been save in the view mode before editing. ***
      laData[5] = lcOldLow
    ENDIF
    SELECT GLTYPES
  ENDIF

  IF lcOldUpr > laData[6]

    *** Select from the segment values file all the segment ***
    *** that between the old upper limit & the new value... ***
    SELECT GLSEGVAL
    SELECT cSegvalue,cSeglndes FROM &gcDataDir.GLSEGVAL;
           WHERE cAcssegno = "1" .AND.  ;
                 cTypecode = ALLTRIM(lcAccTyp1+laData[1]) .AND.;
                 BETWEEN(VAL(cSegvalue),VAL(laData[6])+1,VAL(lcOldUpr));
           INTO DBF &gcWorkDir.&lc_TempAc
  
    SELECT (lc_TempAc)
  
    IF !EMPTY(cSegvalue)

      *** This modification will cause one or more ***
      *** accounts to be without a type. ***
      *** <   Ok   > ***
      lnOption = gfModalGen("QRM02002B00000","DIALOG")
      SELECT GLTYPES

      *** Set the field value to  the old  value has ***
      *** been save in the view mode before editing. ***
      laData[6] = lcOldUpr
    ENDIF
    SELECT GLTYPES
  ENDIF
ENDIF

IF laData[1] <> '00'
  lnRec = IIF(RECNO()>RECCOUNT(),0,RECNO())

  IF RECCOUNT() <> 0
    GO 1
  ENDIF

  *** Check if there is no ranges overlap the values in the new ***
  *** record before saving to avoid any overlap can happen if   ***
  *** there is 2 user saving in the same time ...
  LOCATE FOR ALLTRIM(lcAccTyp1+laData[1]) <> cTypeCode .AND.;
             (BETWEEN(VAL(cTypuacno),VAL(laData[5]),VAL(laData[6])) .OR. ;
             BETWEEN(VAL(cTyplacno),VAL(laData[5]),VAL(laData[6])))

  IF FOUND()
    IF lnRec > 0
      GO lnRec
    ENDIF

    *** Ranges are overlapped for type ð. ***
    *** <   Ok   > ***
    =gfModalGen("QRM02001B00000","DIALOG",ALLTRIM(GLTYPES.cTypshdes))
    _CUROBJ = OBJNUM(laData[5])
    llCSave = .F.
    RETURN
  ENDIF

  IF lnRec > 0
    GO lnRec
  ENDIF  
ENDIF

*** This flag used by the system to allow  the user ***
*** to modify the existing segments in the  account ***
*** code structure since the types' ranges have not ***
*** been defined yet ...

*** So if add new type, set this flag to .T. ***
*Change file name from SYCACCOD to ACCOD      
*SELECT SYCACCOD
SELECT ACCOD
*E300692,1 end

*E300789,7 [BEGIN] The following code will be commented till *E300789,7 [END..]
*The modified code will start with E300789,7 [BEGIN]
*The main purpose to from this modification is to reflect the removing of
* ccomp_id field from ACCOD file .

*SET ORDER TO COMPID
*E *Change file name from SYCACCOD to ACCOD      
*E *IF SEEK(gcAct_Comp,"SYCACCOD")
*E IF SEEK(gcAct_Comp,"ACCOD")
*E *E300692,1 end
*E *Change file name from SYCACCOD to ACCOD        
*E   *IF !SYCACCOD.lAcsused
*E  IF !ACCOD.lAcsused
*E   *E300692,1 end
*E     REPLACE lAcsused WITH .T.
*E   ENDIF
*E ENDIF
*E300789,7 [END..]

*E300789,7 [BEGIN] Start the new code 
IF !EOF()
  IF !ACCOD.lAcsused
    REPLACE lAcsused WITH .T.
  ENDIF
ENDIF

*E300789,7 [END..] End the new code 

SELECT GLTYPES
IF laScRMode[4]
  APPEND BLANK
ENDIF

*** Put the right value for the type code ***
laData[1] = lcAccTyp1 + laData[1]

*** Put the standard field value according to ***
*** the type (standard "Y" - statistical "N") ***
laData[7] = IIF(SUBSTR(laData[1],1,1) = 'Y' , 'N' , 'Y')

GATHER FROM laData FIELDS &lcScFields
FLUSH
=gfAdd_Info()

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
* 

PROCEDURE lpDelScr

*** Check that there is no segments will be without ***
*** types if this deletion happen.... 
SELECT GLSEGVAL
SELECT cSegvalue,cSeglndes FROM &gcDataDir.GLSEGVAL;
       WHERE BETWEEN(VAL(cSegvalue),VAL(laData[5]),VAL(laData[6]));
             .AND. cAcssegno = "1" .AND.  ;
             cTypecode = ALLTRIM(lcAccTyp1+laData[1]);
       INTO DBF &gcWorkDir.&lc_TempAc
  
SELECT (lc_TempAc)
  
IF !EMPTY(cSegvalue)

  *** You cannot delete this type.         ***
  *** This Deletion will cause one or more ***
  *** accounts to be without a type. ***
  *** <  Ok  > ***
  lnOption = gfModalGen("QRM02197B00000","DIALOG")
  SELECT GLTYPES
ELSE
  SELECT GLTYPES
  *** If it is a main type remove its description from the popup ***
  IF laData[1] = '00'
    laConTyp[puAccTyp] = SUBSTR(laConTyp[puAccTyp],1,13)
  ENDIF

  DELETE
   
  lnRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
  *** So if delete a type & the types file  ***
  *** is empty, set the used flag to .F. in ***
  *** the account code structure file ..... ***
  GO TOP IN GLTYPES
  IF EOF()
  *Change file name from SYCACCOD to ACCOD          
    *SELECT SYCACCOD
    SELECT ACCOD
  *E300692,1 end  
  
  *E300789,7 [BEGIN] The following code will be commented till *E300789,7 [END..]
*   * to refelect the changes on ACCOD file .   
*    SET ORDER TO COMPID
*  *Change file name from SYCACCOD to ACCOD            
*    *IF SEEK(gcAct_Comp,"SYCACCOD")
*    IF SEEK(gcAct_Comp,"ACCOD")
*  *E300692,1 end  
*      REPLACE lAcsused WITH .F.
*    ENDIF
  *E300789,7 [END..]

  *E300789,7 [BEGIN]

    IF !EOF()
      REPLACE lAcsused WITH .F.
    ENDIF
  *E300789,7 [END..]
  ENDIF
    
  SELECT GLTYPES
  IF lnRec <> 0
    GO lnRec
  ENDIF

  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF    

*E700014,1 Hesham (Start)
*E700014,1 adding this function to initialize the report desc. with
*E700014,1 the type desc. this function is the valid function of
*E700014,1 the type desc. field in screen gltype
FUNCTION lfvData_2
IF EMPTY(laData[3]) AND laScrMode[4]
  laData[3] = laData[2]
  SHOW GET laData[3]  
ENDIF
IF EMPTY(laData[4]) AND laScrMode[4]
  laData[4] = laData[2]
  SHOW GET laData[4]
ENDIF
*E700014,1 Hesham (End)