*:****************************************************************
*: Program file  : HLCOST.PRG
*: Program desc. : Labor Cost Sheet.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/10/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvSeason , lfCreatemp
*:               :            : lfCollect  , lfwLine1  , lfwLine2
*:               :            : lfwLine3   , lfFieldName, lfwLin1
*:               :            : lfwLin2    , lfwLin3   , lfFieldLine
*:               :            : lfwTotal1  , lfwTotal2 , lfwTotal3
*:               :            : lfFieldTot
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- laCompAdd  :- Array Hold the Address for the Company.
*-- lcPhone    :- Hold the Phone Number for the Company.
*- llSetOrder  :- if filed cstymslrg is empty set order to.
*-- End Declaration variables.


DIMENSION laCompAdd[2]
STORE '' TO laCompAdd
llSetOrder = .F.
SELECT SYCCOMP
SEEK gcAct_Comp

laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
lcPhone      = cCom_Phon

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  lcTemp1 = gfTempName()
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()
  IF !llSetOrder
    SELECT (lcTemp)
    SET ORDER TO TAG (lcTemp1)
  ENDIF
ENDIF

SELECT (lcTemp)
DO gfDispRe WITH EVAL('lcRpForm')

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

    lcBrFields = [cStynum:H="Style Number"]
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
INDEX ON cstyseacd+cstynum TAG (lcTemp1) OF (gcWorkDir+lcTemp+'.CDX')
INDEX ON cstymslrg TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')

SET ORDER TO (lcTemp) DESCENDING

*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
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
SCAN FOR ALLTRIM(lcRpSeason) = ALLTRIM(cstyseacd)
  SCATTER MEMVAR MEMO
  llSetOrder = IIF(EMPTY(ALLTRIM(m.cstymslrg)),llSetOrder,.T.)
  INSERT INTO (lcTemp) FROM MEMVAR
ENDSCAN

*-- End OF lfCollect
*:*************************************************************
*: Name      : lfwLine1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwLine1()
*:*************************************************************
*
FUNCTION lfwLine1

IF (!EMPTY(nStyFabCo) AND !EMPTY(nStyFabCb);
       AND !EMPTY(nStyFabCc) AND !EMPTY(nStyFabCd))
  lnObjDisp1 = 1
ELSE
  lnObjDisp1 = 0
  RETURN .F.
ENDIF

*-- End OF  lfwLine1.
*:*************************************************************
*: Name      : lfwLine2
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwLine2()
*:*************************************************************
*
FUNCTION lfwLine2

lnLines = 0
IF !EMPTY(nStyFabCo)
  lnLines = lnLines + 1
ENDIF  

IF !EMPTY(nStyFabCb)
  lnLines = lnLines + 1
ENDIF  

IF !EMPTY(nStyFabCc)
  lnLines = lnLines + 1
ENDIF  

IF !EMPTY(nStyFabCd)
  lnLines = lnLines + 1
ENDIF  

IF (lnLines >= 3)
  lnObjDisp2 = 1
ELSE
  lnObjDisp2 = 0
  RETURN .F.
ENDIF

*-- End Of lfwLine2
*:*************************************************************
*: Name      : lfwLine3
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwLine3()
*:*************************************************************
*
FUNCTION lfwLine3

lnLines = 0
IF !EMPTY(nStyFabCo)
  lnLines = lnLines + 1
ENDIF  

IF !EMPTY(nStyFabCb)
  lnLines = lnLines + 1
ENDIF  

IF !EMPTY(nStyFabCc)
  lnLines = lnLines + 1
ENDIF  

IF !EMPTY(nStyFabCd)
  lnLines = lnLines + 1
ENDIF  

IF (lnLines >= 2)
  lnObjDisp3 = 1
ELSE
  lnObjDisp3 = 0
  RETURN .F.
ENDIF
    
*-- End OF lfwLine3
*:*************************************************************
*: Name      : lfFieldName
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFieldName()
*:*************************************************************
*
FUNCTION lfFieldName

IF lnObjDisp1 = 0
  lnObjDisp1 = 1
  RETURN 0
ENDIF  

IF lnObjDisp2 = 0
  lnObjDisp2 = 1
  RETURN 0
ENDIF  

IF lnObjDisp3 = 0
  lnObjDisp3 = 1
  RETURN 0
ENDIF  

DO CASE

  CASE !EMPTY(nStyFabCo) .AND. lnField1=0
    lnField1   = 1
    RETURN (ROUND(nStyFabCo,2))

  CASE !EMPTY(nStyFabCb) .AND. lnField2=0
    lnField2   = 1
    RETURN (ROUND(nStyFabCb,2))

  CASE !EMPTY(nStyFabCc) .AND. lnField3=0
    lnField3   = 1
    RETURN (ROUND(nStyFabCc,2))

  CASE !EMPTY(nStyFabCd) .AND. lnField4=0
    lnField4   = 1
    RETURN (ROUND(nStyFabCd,2))

  OTHERWISE
    RETURN 0  

ENDCASE

*-- End OF lfFieldName.
*:*************************************************************
*: Name      : lfwLin1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwLin1()
*:*************************************************************
*
FUNCTION lfwLin1

IF (!EMPTY(nStyFabCo) AND !EMPTY(nStyFabCb);
       AND !EMPTY(nStyFabCc) AND !EMPTY(nStyFabCd))
  lnObjDis1 = 1
ELSE
  lnObjDis1 = 0
  RETURN .F.
ENDIF

*-- End Of lfwLin1.
*:*************************************************************
*: Name      : lfwLin2
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwLin2()
*:*************************************************************
*
FUNCTION lfwLin2

lnLine = 0
IF !EMPTY(nStyFabCo)
  lnLine = lnLine + 1
ENDIF  

IF !EMPTY(nStyFabCb)
  lnLine = lnLine + 1
ENDIF  

IF !EMPTY(nStyFabCc)
  lnLine = lnLine + 1
ENDIF  

IF !EMPTY(nStyFabCd)
  lnLine = lnLine + 1
ENDIF  

IF (lnLine >= 3)
  lnObjDis2 = 1
ELSE
  lnObjDis2 = 0
  RETURN .F.
ENDIF

*-- End OF lfwLin2
*:*************************************************************
*: Name      : lfwLin3
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwLin3()
*:*************************************************************
*
FUNCTION lfwLin3

lnLine = 0
IF !EMPTY(nStyFabCo)
  lnLine = lnLine + 1
ENDIF  

IF !EMPTY(nStyFabCb)
  lnLine = lnLine + 1
ENDIF  

IF !EMPTY(nStyFabCc)
  lnLine = lnLine + 1
ENDIF  

IF !EMPTY(nStyFabCd)
  lnLine = lnLine + 1
ENDIF  

IF (lnLine >= 2)
  lnObjDis3 = 1
ELSE
  lnObjDis3 = 0
  RETURN .F.
ENDIF

*-- End OF lfwLin3.    
*:*************************************************************
*: Name      : lfFieldLine
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFieldLine()
*:*************************************************************
*
FUNCTION lfFieldLine

IF lnObjDis1 = 0
  lnObjDis1 = 1
  RETURN 0
ENDIF  

IF lnObjDis2 = 0
  lnObjDis2 = 1
  RETURN 0
ENDIF  

IF lnObjDis3 = 0
  lnObjDis3 = 1
  RETURN 0
ENDIF  

DO CASE

  CASE !EMPTY(nStyFabCo) .AND. lnField11=0
    lnField11  = 1
    RETURN (ROUND(nStyFabYa,1))

  CASE !EMPTY(nStyFabCb) .AND. lnField22=0
    lnField22  = 1
    RETURN (ROUND(nStyFabYb,1))

  CASE !EMPTY(nStyFabCc) .AND. lnField33=0
    lnField33  = 1
    RETURN (ROUND(nStyFabYc,1))

  CASE !EMPTY(nStyFabCd) .AND. lnField44=0
    lnField44  = 1
    RETURN (ROUND(nStyFabYd,1))

  OTHERWISE
    RETURN 0  

ENDCASE

*-- End Of lfFieldLine.
*:*************************************************************
*: Name      : lfwTotal1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwTotal1()
*:*************************************************************
*
FUNCTION lfwTotal1

IF (!EMPTY(nStyFabCo) AND !EMPTY(nStyFabCb);
    AND !EMPTY(nStyFabCc) AND !EMPTY(nStyFabCd))
  lnObjTot1 = 1
ELSE
  lnObjTot1 = 0
  RETURN .F.
ENDIF

*-- End OF lfwTotal1.
*:*************************************************************
*: Name      : lfwTotal2
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwTotal2()
*:*************************************************************
*
FUNCTION lfwTotal2

lnTotal = 0
IF !EMPTY(nStyFabCo) 
  lnTotal = lnTotal + 1
ENDIF  

IF !EMPTY(nStyFabCb)
  lnTotal = lnTotal + 1
ENDIF  

IF !EMPTY(nStyFabCc)
  lnTotal = lnTotal + 1
ENDIF  

IF !EMPTY(nStyFabCd)
  lnTotal = lnTotal + 1
ENDIF  

IF (lnTotal >= 3)
  lnObjTot2 = 1
ELSE
  lnObjTot2 = 0
  RETURN .F.
ENDIF

*-- End Of lfwTotal2.
*:*************************************************************
*: Name      : lfwTotal3
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwTotal3()
*:*************************************************************
*
FUNCTION lfwTotal3

lnTotal = 0
IF !EMPTY(nStyFabCo)
  lnTotal = lnTotal + 1
ENDIF  

IF !EMPTY(nStyFabCb)
  lnTotal = lnTotal + 1
ENDIF  

IF !EMPTY(nStyFabCc)
  lnTotal = lnTotal + 1
ENDIF  

IF !EMPTY(nStyFabCd)
  lnTotal = lnTotal + 1
ENDIF  

IF (lnTotal >= 2)
  lnObjTot3 = 1
ELSE
  lnObjTot3 = 0
  RETURN .F.
ENDIF

*-- End OF lfwTotal3
*:*************************************************************
*: Name      : lfFieldTot
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFieldTot()
*:*************************************************************
*
FUNCTION lfFieldTot

IF lnObjTot1 = 0
  lnObjTot1 = 1
  RETURN 0
ENDIF  

IF lnObjTot2 = 0
  lnObjTot2 = 1
  RETURN 0
ENDIF  

IF lnObjTot3 = 0
  lnObjTot3 = 1
  RETURN 0
ENDIF  

DO CASE

  CASE !EMPTY(nStyFabCo) .AND. lnTotal1=0
    lnTotal1  = 1
    RETURN (ROUND(ROUND(nStyFabYa,1) * ROUND(nStyFabCo,2),2))

  CASE !EMPTY(nStyFabCb) .AND. lnTotal2=0
    lnTotal2  = 1
    RETURN (ROUND(ROUND(nStyFabYb,1) * ROUND(nStyFabCb,2),2))

  CASE !EMPTY(nStyFabCc) .AND. lnTotal3=0
    lnTotal3  = 1
    RETURN (ROUND(ROUND(nStyFabYc,1) * ROUND(nStyFabCc,2),2))

  CASE !EMPTY(nStyFabCd) .AND. lnToTal4=0
    lnTotal4  = 1
    RETURN (ROUND(ROUND(nStyFabYd,1) * ROUND(nStyFabCd,2),2))

  OTHERWISE
    RETURN 0  

ENDCASE

*-- End OF lfFieldTot.
*:*************************************************************