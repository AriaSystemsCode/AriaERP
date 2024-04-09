*:***************************************************************************
*: Program file  : SOSTYCUS.PRG      (SOSTYCUS.FRX)
*: Program desc. : CUSTOMIZED SALES ORDER WITH CUSTOMER FOR ERIC.
*: Date          : 26/04/2005
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C126843
*:
*:***************************************************************************
*: Calls : 
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO SOSTYCUS
*:***************************************************************************
*: Modification:
*****************************************************************************

*-- Get the Variables its Initial value
STORE SPACE(0) TO lcFilter , lcRpColor , lcRpStatus

=lfCreatTmp()
=lfRelation()
=lfvCrATVar()
=lfColctDat()

SELECT (lclines)
SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE

SET FILTER TO EVAL(lclines+'.TOTOPEN') # 0
LOCATE

DO gfDispRe WITH "SOSTYCUS" , 'FOR ' + "&lclines..TotOpen <> 0 "
SELECT (lclines)
SET RELATION OFF INTO Customer
WAIT CLEAR
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Function to create the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

CREATE CURSOR (lclines) (Account C(5)  , TotOpen N(7) , StyDesc C(60) , llPrnTot L ,;
                         Color C(lnClrLnGl) , Style C(lnStyLnGl))
ZAP
INDEX ON Style + Color + Account TAG Account OF (gcWorkDir + lcLines + '.CDX')

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Function to Collect the date from the ordline file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias , lcAccount , lcTagStyl

lcAlias = ALIAS()
SELECT STYLE
lcTagStyl = ORDER()
lcFilter = IIF(EMPTY(lcFilter) , '.T.' , lcFilter)
lcSekStyle = ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'STYLE.CSTYMAJOR'),1),6])
SET ORDER TO TAG STYLE
LOCATE

SCAN
  WAIT WINDOW 'Selecting Records For The Report ...' + ALLTRIM(STYLE.CSTYMAJOR) NOWAIT
  IF !EMPTY(lcRpColor) AND !SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl) $ lcRpColor
    LOOP
  ENDIF

  SELECT ORDLINE
  LOCATE
  =SEEK(ALLTRIM(STYLE.CSTYMAJOR))

  SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcSekStyle 
    IF !EVAL(lcFilter)
      LOOP
    ENDIF

    IF SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl) == LEFT(STYLE.CSTYMAJOR,lnStyLnGl)
      IF SUBSTR(ORDLINE.STYLE,lnClrPosGL,lnClrLnGl) == SUBSTR(STYLE.STYLE,lnClrPosGL,lnClrLnGl)
        lcValuSeek = SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl) + SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) + ORDLINE.ACCOUNT

        IF ORDHDR.STATUS ='C'
          IF SEEK(lcValuSeek,lclines)
            REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TotBook
          ELSE
            SELECT (lclines)
            APPEND BLANK
            REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
	                &lclines..TotOpen WITH ORDLINE.TotBook                            ,;
		            &lclines..StyDesc WITH STYLE.DESC1                                ,;
		            &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                    &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
          ENDIF
        ELSE

          IF SEEK(lcValuSeek,lclines)
            REPLACE &lclines..TotOpen WITH &lclines..TotOpen  + ORDLINE.TOTQTY
          ELSE
            SELECT (lclines)
            APPEND BLANK
            REPLACE &lclines..Account WITH ORDLINE.ACCOUNT                            ,;
	                &lclines..TotOpen WITH ORDLINE.TOTQTY                             ,;
		            &lclines..StyDesc WITH STYLE.DESC1                                ,;
		            &lclines..Color   WITH SUBSTR(ORDLINE.STYLE,lnClrPosGl,lnClrLnGl) ,;
                    &lclines..Style   WITH SUBSTR(ORDLINE.STYLE,lnStyPosGl,lnStyLnGl)
          ENDIF
        ENDIF  
      ENDIF
    ELSE
      EXIT
    ENDIF
  ENDSCAN
ENDSCAN

SELECT STYLE
SET ORDER TO TAG &lcTagStyl

SELECT (lcAlias)

*-- End of lfcolctDat.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Function to prepare the relations of the program.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT ORDLINE
SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE
SELECT STYLE
SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE

*-- End of lfRelation.
*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

lcFilter = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

*--The Colors.
FOR lnClr = 1 TO ALEN(laRpTColor)
  lcRpColor = lcRpColor + PADR(laRpTColor[lnClr],lnClrLnGl) + ' | '
ENDFOR
lcRpColor = IIF(ALLTRIM(lcRpColor) = '|','',lcRpColor)

*--The Status.
FOR lnI = 1 TO ALEN(laRpTStats,1)
  DO CASE
    CASE laRpTStats[lnI] == "Open"
      laRpTStats[lnI] = "O"
    CASE laRpTStats[lnI] == "Hold"
      laRpTStats[lnI] = "H"
    CASE laRpTStats[lnI] == "Canceled"
      laRpTStats[lnI] = "X"
    CASE laRpTStats[lnI] == "Completed"
      laRpTStats[lnI] = "C"
  ENDCASE
ENDFOR

FOR lnStats = 1 TO ALEN(laRpTStats,1)
  lcRpStatus = lcRpStatus + laRpTStats[lnStats] + ','
ENDFOR
lcRpStatus = IIF(ALLTRIM(lcRpStatus) = "," , '' , lcRpStatus)

*--Return the original values.
FOR lnI = 1 TO ALEN(laRpTStats,1)
  DO CASE
    CASE laRpTStats[lnI] == "O"
      laRpTStats[lnI] = "Open"
    CASE laRpTStats[lnI] == "H"
      laRpTStats[lnI] = "Hold"
    CASE laRpTStats[lnI] == "X"
      laRpTStats[lnI] = "Canceled"
    CASE laRpTStats[lnI] == "C"
      laRpTStats[lnI] = "Completed"
  ENDCASE
ENDFOR

IF !EMPTY(lcRpStatus)
  lcFilter = lcFilter + ' AND ORDHDR.STATUS $ lcRpStatus'
ENDIF

*-- End of lfvCrATVar.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Validate Style.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF '?' $ &lcStyle OR (!EMPTY(&lcStyle) .AND. !SEEK(&lcStyle.,'Style') )
  &lcStyle = gfStyBrw('M',"","",.F.)
ELSE
  IF !EMPTY(&lcStyle) .AND. SEEK(&lcStyle.,'Style')
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = ''
  ENDIF
ENDIF

SET ORDER TO lcTag IN STYLE

*--End of lfvStyle.
*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
  SET ORDER TO TAG CSTYLE IN STYLE
  GO TOP IN STYLE
ENDIF

*--End of lfSetSty.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF EMPTY(laRpSStats)
  DECLARE laRpSStats[4],laRpTStats[1,1] , laRpSColor[1,1],laRpTColor[1,1]

  STORE '' TO laRpTStats , laRpTColor
  STORE 'Open'      TO laRpSStats[1]
  STORE 'Hold'      TO laRpSStats[2]
  STORE 'Canceled'  TO laRpSStats[3]
  STORE 'Completed' TO laRpSStats[4]

ENDIF

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

IF lcParm = 'S'
  GO TOP IN CUSTOMER
ENDIF

*--End of lfsrAcc.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Show the mover with all the status to be select from it.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus

=gfMover(@laRpSStats,@laRpTStats,'Select Order Status',.T.,'')  && call mover function.

*--End of lfvOStatus.
*!*************************************************************
*! Name      : lfvColors
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/04/2005
*! Purpose   : Show the mover with all the colors to be select from it. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvColors
PRIVATE lcOldStat , lcCurrChr , lcStyle

DIMENSION laRpSColor[1,1],laRpTColor[1,1]
STORE '' TO laRpSColor , laRpTColor , lcRpColor

lcStyle = ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'STYLE.CSTYMAJOR'),1),6])

*--The Colors.
SELECT STYLE
SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPosGl,lnClrLnGl) FROM STYLE WHERE CSTYMAJOR = lcStyle INTO ARRAY laRpSColor

=gfMover(@laRpSColor,@laRpTColor,'Select Style Color(s)',.T.,'')  && call mover function.

*--End of lfvColors.
