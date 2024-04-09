*:****************************************************************************************
*: Program file  : NIKMAIN.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200155,1
*: Date          : 01/29/2001
*: Purpose       : Nike Custom Process Program .
*:****************************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*C#200155,1
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:**************************************************************************
*: Name          : lfUpDat_Pr
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200155,1
*: Date          : 01/29/2001
*: Purpose       : Update every scale / size with specific Price.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfCretTemp()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200155,1.
*:**************************************************************************
*
FUNCTION lfUpDat_Pr
PRIVATE lnAlias , lcSelStyle , lnStyleLen
STORE '' To lcSelStyle
STORE 0 To lnAlias , lnStyleLen ,lnPrRecNo 

lnAlias = SELECT(0)
*-- Function to create the temp file.
= lfCretTemp ()
*-- lccolorfil
SELECT (lccolorfil)
GOTO TOP
SCAN
  IF !SEEK (STYLE + SCALE,lcTmpStypc)
    = SEEK ('S'+RIGHT(ALLTRIM(Style),3),'SCALE')
    SELECT (lcTmpStypc)
    FOR I = 1 To SCALE.CNT
      Z = STR(I,1)
      APPEND BLANK
      REPLACE STYLE WITH &lccolorfil..STYLE ,;
              SIZES WITH SCALE.SZ&z         ,;
              Price WITH &lccolorfil..PRICEA
    ENDFOR
    SELECT (lccolorfil)
  ENDIF
ENDSCAN

SELECT (lcTmpStypc)
GOTO TOP
*-- Do the screen

lcUpPrTtl = 'Prices'
lcBrowTitl = 'Edit Prices'


lcMarker = ''
lcUnMark = SPACE(1)

PUSH KEY
ON KEY
ON KEY LABEL ESC     DO lpUpEsc
ON KEY LABEL TAB     DO lpTrab
ON KEY LABEL backtab DO lpshifttrb

DO (gcScrDir+gcWinAppl+"\ICNMJRP.SPX")

ON KEY
POP KEY


SELECT(lnAlias)

*-- End Of lfUpDat_Pr.
*:*************************************************************
*: Name      : lfCretTemp
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function create the temp file.
*:*************************************************************
*: Called from : Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCretTemp()
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
FUNCTION lfCretTemp

*-- Create File hold the browse fileds
DIMENSION lafilestru[5,4]

lafilestru[1 , 1] = 'STYLE'
lafilestru[1 , 2] = 'C'
lafilestru[1 , 3] = 19
lafilestru[1 , 4] = 0

lafilestru[2 , 1] = 'Sizes'
lafilestru[2 , 2] = 'C'
lafilestru[2 , 3] = 5
lafilestru[2 , 4] = 0

lafilestru[3 , 1] = 'Price'
lafilestru[3 , 2] = 'N'
lafilestru[3 , 3] = 10
lafilestru[3 , 4] = 2

lafilestru[4 , 1] = 'cStatus'
lafilestru[4 , 2] = 'C'
lafilestru[4 , 3] = 1
lafilestru[4 , 4] = 0

lafilestru[5 , 1] = 'cMarker'
lafilestru[5 , 2] = 'C'
lafilestru[5 , 3] = 1
lafilestru[5 , 4] = 0



CREATE DBF (gcWorkDir+lcTmpStypc) FROM ARRAY lafilestru
SELECT (lcTmpStypc)

INDEX ON Style TAG (lcTmpStypc) Unique ADDI

*-- End OF lfCretTemp.
*:*************************************************************
*: Name      : lfUpdBrow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : create style forign price browse.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : Non.
*:*************************************************************
*: Example   : = lfUpdBrow()
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
FUNCTION lfUpdBrow
SELECT (lcTmpStyPc)
lnPrRecNo  = RECNO(lcTmpStyPc)

lcBrFields = "lcMarker=IIF(RECNO() = lnPrRecNo,'>',' '):1:H=' ':W=.F.,"+;
             "STYLE     :R:W=.F.:H='STYLE':22,"+;
             "Sizes     :R:W=.F.:H='Size ':10,"+;
  	         "Price     :P='@! 999999999.99':V=(Price>=0) .AND. lfUpstats():H='Price':15"

BROWSE FIELDS &lcBrFields;
       WINDOW    ICNMJ1  ;
       IN WINDOW ICNMJRP ;
       NOAPPEND;
       NOCLEAR ;
       NODELETE;
       NOMENU  ;
       SAVE    ;
       WHEN lfwBrowUp()  ;              
       TITLE (lcBrowTitl)

*-- End OF lfUpdBrow
*:*************************************************************
*: Name      : lfwBrowUp
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Refresh record pointer.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfwBrowUp()
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
FUNCTION lfwBrowUp

lnPrRecNo  = RECNO(lcTmpStyPc)
REPLACE ALL cMarker WITH lcUnMark
GO lnPrRecNo
REPLACE cMarker WITH lcMarker

SHOW WINDOW ICNMJRP REFRESH

*-- End OF lfwBrowUp.
*:*************************************************************
*: Name      : lfUpdPrics
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Refresh record pointer.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfUpdPrics()
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
FUNCTION lfUpdPrics

SELECT (lcTmpStypc)
GOTO TOP
SCAN FOR cStatus = 'U'
  glupdated = .T. 
  =SEEK(&lcTmpStypc..STYLE,lccolorfil)
  SELECT (lccolorfil)  
  REPLACE PriceA     WITH &lcTmpStypc..Price,;
          nSugRetPri WITH &lcTmpStypc..Price
  SELECT (lcTmpStypc)          
ENDSCAN

*-- End OF lfUpdPrics.
*:*************************************************************
*: Name      : lfUpstats
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Update filed status
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfUpstats()
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
FUNCTION lfUpstats

REPLACE cStatus WITH 'U'

*-- End OF lfUpstats
*:*************************************************************
*: Name      : lpTrab
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Tab key trapping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpTrab
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
PROCEDURE lpTrab

IF WONTOP(lcBrowTitl)
  ACTIVATE WINDOW ('ICNMJ2')
ELSE
  ACTIVATE WINDOW (lcBrowTitl)
ENDIF

RETURN
*-- End OF lpTrab.
*:*************************************************************
*: Name      : lpshifttrb
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Shift Tab key trapping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpshifttrb
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
PROCEDURE lpshifttrb

DO lpTrab

RETURN

*-- End Of lpshifttrb.
*:*************************************************************
*: Name      : lpUpEsc
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
PROCEDURE lpUpEsc

ACTIVATE WINDOW ('ICNMJ2')
_CUROBJ = OBJNUM(pbEPok)
KEYBOARD CHR(17)+CHR(13)

RETURN
*-- End Of lpUpEsc
*:**************************************************************************
*: Name          : lfUpDat_Cs
*: Developer     : Ahmed Mohamed El_Anwar (AME)
*: Date          : 02/11/2001
*: Purpose       : Update every scale / size with specific Cost..
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfUpDat_CS()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to B604192.
*:**************************************************************************
*
FUNCTION lfUpDat_Cs
PRIVATE lnAlias , lcSelStyle , lnStyleLen
STORE '' To lcSelStyle
STORE 0 To lnAlias , lnStyleLen ,lnPrRecNo 

lnAlias = SELECT(0)
*-- Function to create the temp file.
=lfCretCTemp ()
*-- lccolorfil
SELECT (lccolorfil)
GOTO TOP
SCAN
  IF !SEEK (STYLE + SCALE,lcTmpStypc)
    = SEEK ('S'+RIGHT(ALLTRIM(Style),3),'SCALE')
    SELECT (lcTmpStypc)
    FOR I = 1 To SCALE.CNT
      Z = STR(I,1)
      APPEND BLANK
      REPLACE STYLE WITH &lccolorfil..STYLE ,;
              SIZES WITH SCALE.SZ&z         ,;
              Cost WITH &lccolorfil..NICost1
    ENDFOR
    SELECT (lccolorfil)
  ENDIF
ENDSCAN

SELECT (lcTmpStypc)
GOTO TOP
*-- Do the screen

lcUpPrTtl = 'Costing'
lcBrowTitl = 'Edit Cost'


lcMarker = ''
lcUnMark = SPACE(1)

PUSH KEY
ON KEY
ON KEY LABEL ESC     DO lpUpEsc
ON KEY LABEL TAB     DO lpTrab
ON KEY LABEL backtab DO lpshifttrb

DO (gcScrDir+gcWinAppl+"\ICNMJRC.SPX")

ON KEY
POP KEY


SELECT(lnAlias)

*-- End Of lfUpDat_CS.
*:*************************************************************
*: Name      :lfCretCTemp
*: Developer : AHMED MOHAMED ELANWAR - (AME)
*: Date      : 02/11/2001
*: Purpose   : Function create  temp file.
*:*************************************************************
*: Called from : Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfCretCTemp()
*:*************************************************************
*: Due to B#604192,1.
*:*************************************************************
*
FUNCTION  lfCretCTemp

*-- Create File hold the browse fileds
DIMENSION lafilestru[13,4]

lafilestru[1 , 1] = 'STYLE'
lafilestru[1 , 2] = 'C'
lafilestru[1 , 3] = 19
lafilestru[1 , 4] = 0

lafilestru[2 , 1] = 'Sizes'
lafilestru[2 , 2] = 'C'
lafilestru[2 , 3] = 5
lafilestru[2 , 4] = 0

lafilestru[3 , 1] = 'Cost'
lafilestru[3 , 2] = 'N'
lafilestru[3 , 3] = 10
lafilestru[3 , 4] = 2

lafilestru[4 , 1] = 'cStatus'
lafilestru[4 , 2] = 'C'
lafilestru[4 , 3] = 1
lafilestru[4 , 4] = 0

lafilestru[5 , 1] = 'cMarker'
lafilestru[5 , 2] = 'C'
lafilestru[5 , 3] = 1
lafilestru[5 , 4] = 0


lafilestru[6 , 1] = 'NICost2'
lafilestru[6 , 2] = 'N'
lafilestru[6 , 3] = 10
lafilestru[6 , 4] = 2


lafilestru[7 , 1] = 'NICost3'
lafilestru[7 , 2] = 'N'
lafilestru[7 , 3] = 10
lafilestru[7 , 4] = 2


lafilestru[8 , 1] = 'NICost4'
lafilestru[8 , 2] = 'N'
lafilestru[8 , 3] = 10
lafilestru[8 , 4] = 2


lafilestru[9 , 1] = 'NICost5'
lafilestru[9 , 2] = 'N'
lafilestru[9 , 3] = 10
lafilestru[9 , 4] = 2


lafilestru[10 , 1] = 'NPrCost2'
lafilestru[10 , 2] = 'N'
lafilestru[10 , 3] = 10
lafilestru[10 , 4] = 2


lafilestru[11 , 1] = 'NPrCost3'
lafilestru[11 , 2] = 'N'
lafilestru[11 , 3] = 10
lafilestru[11 , 4] = 2


lafilestru[12 , 1] = 'NPrCost4'
lafilestru[12 , 2] = 'N'
lafilestru[12 , 3] = 10
lafilestru[12 , 4] = 2


lafilestru[13 , 1] = 'NPrCost5'
lafilestru[13 , 2] = 'N'
lafilestru[13 , 3] = 10
lafilestru[13 , 4] = 2





CREATE DBF (gcWorkDir+lcTmpStypc) FROM ARRAY lafilestru
SELECT (lcTmpStypc)

INDEX ON Style TAG (lcTmpStypc) Unique ADDI

*-- End OF lfCretCTemp.
*:*************************************************************
*: Name      : lfUpdCBrow
*: Developer : AHMED MOHAMED ELANWAR - (AME)
*: Date      : 02/11/2001
*: Purpose   : create style Cost browse.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : Non.
*:*************************************************************
*: Example   : = lfUpdCBrow()
*:*************************************************************
*: Due to  B#604192,1.
*:*************************************************************
*
FUNCTION lfUpdCBrow
SELECT (lcTmpStyPc)
lnPrRecNo  = RECNO(lcTmpStyPc)

lcBrFields = "lcMarker=IIF(RECNO() = lnPrRecNo,'>',' '):1:H=' ':W=.F.,"+;
             "STYLE     :R:W=.F.:H='STYLE':22,"+;
             "Sizes     :R:W=.F.:H='Size ':10,"+;
  	         "Cost      :P='@! 999999999.99':V=(Cost>=0) .AND. lfUpstats():H=lcCostLbl1:15"

BROWSE FIELDS &lcBrFields;
       WINDOW    ICNMJ1  ;
       IN WINDOW ICNMJRP ;
       NOAPPEND;
       NOCLEAR ;
       NODELETE;
       NOMENU  ;
       SAVE    ;
       WHEN lfwBrowUp()  ;              
       TITLE (lcBrowTitl)

*-- End OF lfUpdCBrow
*:*************************************************************
*: Name      : lfUpdCost
*: Developer : AHMED MOHAMED ELANWAR - (AME)
*: Date      : 02/11/2001
*: Purpose   : Refresh record pointer.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfUpdCost()
*:*************************************************************
*: Due to  B#604192,1.
*:*************************************************************
*
FUNCTION lfUpdCost

SELECT (lcTmpStypc)
GOTO TOP
SCAN FOR cStatus = 'U'
  glupdated = .T. 
  IF llMulCurr   && if multi currency.
    IF lcIType1 $ 'PMD' AND ( lnCurRate<>1 OR lnCurUnt<>1 )
      lcSign1   = IIF(lcIType1='P',lcPMethod,lcDMethod)
      lcSign2   = IIF(lcIType1='P',lcUnPMeth,lcUnDMeth)
      lnEquCost = Cost &lcSign1 lnCurRate &lcSign2 lnCurUnt
    ELSE
      lnEquCost = Cost
    ENDIF
  ENDIF 
  =SEEK(&lcTmpStypc..STYLE,lccolorfil)
  SELECT (lccolorfil)  
  REPLACE nICost1    WITH &lcTmpStypc..Cost,;
          Gros_Price WITH &lcTmpStypc..Cost,;
          TOTCost    WITH IIF(!llMulCurr,&lcTmpStypc..Cost,lnEquCost)

  IF (STYLE.Totstk <> 0 OR STYLE.Totret <> 0 AND laScrMode[3]) OR laScrMode[4]
    REPLACE Ave_cost WITH IIF(!llMulCurr,&lcTmpStypc..Cost,lnEquCost)
  ENDIF

  SELECT (lcTmpStypc)          
ENDSCAN

*-- End OF lfUpdCost.

*:*************************************************************
*: Name      : lfCOSTUPDT
*: Developer : AHMED MOHAMED ELANWAR - (AME)
*: Date      : 02/11/2001
*: Purpose   : UPDATE TOTAL COST FIELD.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfCOSTUPDT()
*:*************************************************************
*: Due to  B#604192,1.
*:*************************************************************
*
FUNCTION lfCOSTUPDT
REPLACE TOtCost     WITH &lccolorfil..TotCost
RETURN

*:*************************************************************
*: Name      : lfPRNTRUN
*: Developer : Khalid Mohi El-Din
*: Date      : 05/07/2001
*: Purpose   : To do not display the printer screen
*:*************************************************************
*: Example   : = lfPRNTRUN()
*:*************************************************************
FUNCTION lfPRNTRUN
IF TYPE("lcRpRName") = "C" AND lcRpRName = "POBRCDRS"
  RETURN .T.
ELSE
  RETURN .F.
ENDIF  