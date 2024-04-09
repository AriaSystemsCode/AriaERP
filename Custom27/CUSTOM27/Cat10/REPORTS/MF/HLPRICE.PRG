*:****************************************************************
*: Program file  : HLPRICE.PRG
*: Program desc. : Package Price.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/10/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvSeason , lfCreatemp
*:               :            : lfCollect	  
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcImgDir  :- Variable Hold the Path for image Directory.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- lcRpDate  :- Variable Hold the Date.
*-- End Declaration variables.

lcImgDir  = ALLTRIM(hlsetup.cSetImgDr)+;
            IIF(RIGHT(ALLTRIM(hlsetup.cSetImgDr),1)<>'\','\','')


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()
ENDIF

SELECT (lcTemp)
SET RELATION TO cStynum INTO HLCSTYLE
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')

SET RELATION TO 
RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

IF EMPTY(lcRpDate)
  lcSavCent= SET('CENTURY')
  SET CENTURY ON
  lcRpDate = MDY(DATE())
  SET CENTURY &lcSavCent
ENDIF

*-- End of lfwRepWhen.
*:*************************************************************
*: Name      : lfvSeason
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Browse all styles to select Season!
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvSeason ()
*:*************************************************************
*
FUNCTION lfvSeason
PRIVATE lnAlias
lnAlias = SELECT (0)

IF MDOWN()
  RETURN
ENDIF

IF !EMPTY(lcRpSeason) .AND. LASTKEY() = 13
  SELECT HLCSTYLE
  LOCATE FOR cstyseacd = lcRpSeason
  IF !FOUND()
   DECLARE  laBrowArr[1]
    laBrowArr = ''

    lcBrFields = [cstynum:H="Style number ", cstyseacd:H="Style season code", cstyrang:H="Style range",cstrangds:H="Range description"]
    lcRpSeason = IIF(ARIABROW('',"Styles",gnBrFSRow1, gnBrFSCol1,;
                      gnBrFSRow2, gnBrFSCol2,'','','cStyNum','laBrowArr'),;
                      hlcStyle.cstyseacd,SPACE(3))

  ENDIF
  lcRpStyRan = hlcStyle.cStyRang
  lcRpFabRan = hlcStyle.cStyFabCn
  
  CLEAR READ
ENDIF

SELECT(lnAlias)

*-- End OF lfvSeason
*:**************************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Create the temp files.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : None.
*:**************************************************************************
*: Example   : = lfCreatemp()
*:**************************************************************************
*
FUNCTION lfCreatemp

DIMENSION laTempStru[1,4]
laTempStru = ''
SELECT HLCSTYLE
lnFildLen = AFIELDS(laTempStru)

CREATE Table (lcTemp) FROM ARRAY laTempStru
SELECT (lcTemp)
ZAP
INDEX ON cstysewat+ cstynum TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')


*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect

SELECT HLCSTYLE
LOCATE
SCAN FOR ALLTRIM(cstyseacd) = ALLTRIM(lcRpSeason)
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTemp) FROM MEMVAR
ENDSCAN

*-- End OF lfCollect
*:*************************************************************
