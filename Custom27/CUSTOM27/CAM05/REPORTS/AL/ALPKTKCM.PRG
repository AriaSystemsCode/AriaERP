*:***************************************************************************
*: Program file  : ALPKTKCM.PRG
*: Program desc. : CUSTOMIZED SALES ORDER ALLOCATION PICKING TICKET FOR CAMBIO.
*: Date          : 07/17/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102665
*: 
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKCM
*:***************************************************************************
*: Modifications :
*: B606537,1 BWA 10/14/2002 Increase the space in the FRX to print more lines.[FIX IN FRX]
***************************************************************************

*--Creat table hold the color and its descreption.
CREATE Table (lcTmpClor) (Color C(lnClrLnM1) , Desc C(30))
INDEX ON Color TAG Color OF (gcWorkDir + lcTmpClor + ".CDX")

*--Section of the color description.
SELECT (lcTmpOrdL)
SCAN
  IF !SEEK(SUBSTR(STYLE,lnClrPosM1,lnClrLnM1) , lcTmpClor )
    SELECT (lcTmpClor)
    APPEND BLANK
    REPLACE COLOR WITH SUBSTR(&lcTmpOrdL..STYLE,lnClrPosM1,lnClrLnM1) ;
            Desc  WITH ALLTRIM(gfCodDes(SUBSTR(&lcTmpOrdL..Style,lnClrPosM1,lnClrLnM1) , 'COLOR'))
  ENDIF
ENDSCAN

SELECT (lcTmpOrdL)
LOCATE
SET RELATION TO SUBSTR(&lcTmpOrdL..STYLE,lnClrPosM1,lnClrLnM1) INTO (lcTmpClor) ADDITIVE

*--Restore the optional message lines.
IF FILE(gcDataDir + "MesgLin" +'.MEM')
  RESTORE FROM gcDataDir + "MesgLin" +'.MEM' ADDITIVE
ELSE
  lcRpMesg1 = "The final shipping date is the last day we will ship from our warehouse. Add 10 days for the In-store cancellation"
  lcRpMesg2 = "date to allow for transportation time. All claims for shortages or defects must be submitted to CAMBIO AMERICA"
  lcRpMesg3 = "LTD in writing within 10 days."
  lcRpMesg4 = "We will accept no returns without authorization label which we will issue promptly upon receipt of a valid return"
  lcRpMesg5 = "request. Any unauthorized returns will not be credited. Undamaged goods must be returned as originally packed"
  lcRpMesg6 = "otherwise a penalty charge will result. Late payments are subject to 1 ½% interest per month."
ENDIF

*--Section of printing the Form 2 times with differnet feaders in the bottom.
IF gcDevice <> 'SCREEN'
  STORE 0 TO lnContPrn
  llAlpktk = .F.
  FOR LnLoop = 1 TO 2
    SELECT (lcTmpOrdL)
    LOCATE

    IF EOF(lcTmpOrdL)
      *-- Message <No records to display.>
      =gfModalGen('TRM00052B00000','DIALOG' )
      SET DEVICE TO SCREEN
      EXIT
    ENDIF

    lnContPrn = lnContPrn + 1
    DO gfDispRe WITH EVAL('lcFormName')
  ENDFOR
ENDIF

SELECT (lcTmpOrdL)
LOCATE

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfvOptMesg
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/17/2002
*! Purpose   : Function to browse the custom optional message.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfUsrVldFn('lfvOptMesg','ALPKTKCM')
*!*************************************************************
FUNCTION lfvOptMesg
PARAMETER lcReturn

PRIVATE LaMessag
DECLARE LaMessag[6,2]       && Array to hold the name and length of the variables to be used in the Legal Liability screen

LaMessag[1,1] = 'lcRpMesg1'        && line Variable #1
LaMessag[2,1] = 'lcRpMesg2'        && line Variable #2
LaMessag[3,1] = 'lcRpMesg3'        && line Variable #3
LaMessag[4,1] = 'lcRpMesg4'        && line Variable #4
LaMessag[5,1] = 'lcRpMesg5'        && line Variable #5
LaMessag[6,1] = 'lcRpMesg6'        && line Variable #6
LaMessag[1,2] = 120                && Line length

IF FILE(gcDataDir + "MesgLin" +'.MEM')
  RESTORE FROM gcDataDir + "MesgLin" +'.MEM' ADDITIVE
ENDIF

IF EMPTY(lcRpMesg1) AND EMPTY(lcRpMesg2) AND EMPTY(lcRpMesg3) AND EMPTY(lcRpMesg4) AND EMPTY(lcRpMesg5) AND EMPTY(lcRpMesg6)
  lcRpMesg1 = "The final shipping date is the last day we will ship from our warehouse. Add 10 days for the In-store cancellation"
  lcRpMesg2 = "date to allow for transportation time. All claims for shortages or defects must be submitted to CAMBIO AMERICA"
  lcRpMesg3 = "LTD in writing within 10 days."
  lcRpMesg4 = "We will accept no returns without authorization label which we will issue promptly upon receipt of a valid return"
  lcRpMesg5 = "request. Any unauthorized returns will not be credited. Undamaged goods must be returned as originally packed"
  lcRpMesg6 = "otherwise a penalty charge will result. Late payments are subject to 1 ½% interest per month."
ENDIF

lcReturn = gfOptMsgCm('LaMessag')    && Call Function to write Legal Liability

SET MEMOWIDTH TO 120
SAVE TO gcDataDir + "MesgLin" +'.MEM' ALL LIKE lcRpMesg*

RETURN lcReturn

*--End of lfvOptMesg.
*!*************************************************************
*! Name      : gfOptMsgCm
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/17/2002
*! Purpose   : Function to creat custom optional message.
*!*************************************************************
*! Called from : ALPKTKCM.PRG
*!*************************************************************
*! Calls       : lfvOk
*!*************************************************************
*! Passed Parameters : 1) Array name
*!                   : 2) Title [To be used as the Screen title]
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =gfOptMsgCm()
*!*************************************************************
FUNCTION gfOptMsgCm
PARAMETERS lcArrayNam , lcTitle

PRIVATE laLine   , lnRowNumb , lnLineWid , lnLineNo ,;
        lnMaxWid , lnLinePos , lnBoxWid
IF TYPE('lcArrayNam') <> 'C' .OR. TYPE(lcArrayNam) <> 'C'
  RETURN
ENDIF

lcTitle = IIF(TYPE('lcTitle') <> 'C' .OR. EMPTY(lcTitle) ,;
              'Optional message' , lcTitle)

lnLineWid = IIF(TYPE(lcArrayNam + '[1,2]') <> 'N' .OR.;
                EMPTY(&lcArrayNam.[1 , 2]) , 50 , &lcArrayNam.[1 , 2])    && Variable to hold the line length

lnMaxWid = lnLineWid          && Variable to hold the Max line length

IF TYPE(lcArrayNam + '[1,1]') = 'U'
  DECLARE laLine(1,2)
  laLine[1,1] = &lcArrayNam
  laLine[1,2] = REPLICATE('X' , lnLineWid)
ELSE
  FOR lnRowNumb = 1 TO MIN(6 , ALEN(&lcArrayNam , 1))
    IF TYPE(lcArrayNam + '[' + STR(lnRowNumb , 1) + ',1]') <> 'C' .OR.;
       EMPTY(&lcArrayNam.[lnRowNumb , 1]) .OR.;
       TYPE(&lcArrayNam.[lnRowNumb , 1]) <> 'C'
      EXIT
    ELSE
      DIMENSION laLine(lnRowNumb , 2)
      laLine[lnRowNumb,1] = EVALUATE(&lcArrayNam.[lnRowNumb , 1])
      laLine[lnRowNumb,2] = IIF(TYPE(lcArrayNam + '[' + STR(lnRowNumb ,;
                                1) + ',3]') <> 'C' .OR.;
                                EMPTY(&lcArrayNam.[lnRowNumb , 3]) .OR.;
                                ALEN(&lcArrayNam , 2) < 3 ,;
                                REPLICATE('X' , lnLineWid) ,;
                                &lcArrayNam.[lnRowNumb , 3])

      lnMaxWid = MAX(lnMaxWid , IIF(TYPE(lcArrayNam + '[' + STR(lnRowNumb ,;
                     1) + ',2]') <> 'N' .OR. ALEN(&lcArrayNam , 2) < 2 ,;
                     0 , &lcArrayNam.[lnRowNumb , 2]))
    ENDIF
  ENDFOR
ENDIF

lnMaxWid = MIN(lnMaxWid , 58)
lnLineNo = ALEN(laLine , 1)          && Variable to hold the number of lines
lnLineWid = lnMaxWid * 2
lnLinePos = ((615 - (lnLineWid * 5) - 3) / 8) / 2
lnBoxWid = (lnLineWid * 5 / 8) + 2
lnBoxHi = ((lnLineNo*14)+13-lnLineNo)/12

lcSOkBmp  = gcBmpHome + 'OK.BMP'          && Variable to hold the name and location of the OK BMP
lcSCanBmp = gcBmpHome + 'CAN.BMP'          && Variable to hold the name and location of the CANCEL BMP

DO (gcRepHome + gcAct_Appl + '\OptMsgCm.SPR')

*--End of gfOptMsgCm.
*!*************************************************************
*! Name      : lfvOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/17/2002
*! Purpose   : Valid function of push button Ok.
*!*************************************************************
*! Called from : SCREEN OptMsgCm
*!*************************************************************
*! Calls       : lfvOk
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvOk()
*!*************************************************************
FUNCTION lfvOk

PRIVATE lcVarName

IF TYPE(lcArrayNam + '[1,1]') = 'U'
  &lcArrayNam = laLine[1,1]
ELSE
  FOR lnRowNumb = 1 TO ALEN(laLine , 1)
    lcVarName   = &lcArrayNam.[lnRowNumb , 1]
    &lcVarName  = laLine[lnRowNumb , 1]
  ENDFOR
ENDIF

*--End of lfvOk.
*!*************************************************************
*! Name      : lfFrstDat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/17/2002
*! Purpose   : Function to get the first complete date in ordline.
*!*************************************************************
*! Called from : ALPKTKCM.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfFrstDat()
*!*************************************************************
FUNCTION lfFrstDat
PARAMETERS lcDatLin

IF llRpOrdLNt
  lcRetDat = ORDLINE.COMPLETE
  lcDatLin = ORDLINE.COMPLETE
ELSE
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
  lcRetDat = ORDLINE.COMPLETE
  lcDatLin = ORDLINE.COMPLETE
  SET RELATION OFF INTO ORDLINE
ENDIF


RETURN lcDatLin
*--End of lfFrstDat.