*:************************************************************************
*:
*: Procedure file: ALPKTKT.PRG
*:
*:         System: ARIA 2.7
*:         Module: Main System [Global Function]
*:         Author: Haytham el_Sheltawi
*:      Copyright (c) 
*:  Last modified: 10/15/1997
*:
*:  Procs & Fncts     : lfvOk
*:  Passed Parameters : 1) Array name
*:                      2) Title [To be used as the Screen title]
*:
*:  Documented    10/15/1997
*:************************************************************************
*E300744,1 This program was coded by HS for the Enhancement
*:************************************************************************
*
PARAMETERS lcArrayNam , lcTitle

PRIVATE laLine , lnRowNumb , lnLineWid , lnLineNo , lnMaxWid , lnLinePos ,;
        lnBoxWid

*IF The first parameter is not of type Character [The Array name] or
*The first element of the Array is not of type Character
IF TYPE('lcArrayNam') <> 'C' .OR. TYPE(lcArrayNam) <> 'C'
  RETURN
ENDIF

lcTitle = IIF(TYPE('lcTitle') <> 'C' .OR. EMPTY(lcTitle) ,;
              'Optional message' , lcTitle)

lnLineWid = IIF(TYPE(lcArrayNam + '[1,2]') <> 'N' .OR.;
                EMPTY(&lcArrayNam.[1 , 2]) , 50 , &lcArrayNam.[1 , 2])    && Variable to hold the line length

lnMaxWid = lnLineWid          && Variable to hold the Max line length

*IF (lcArrayNam) is not an Array
IF TYPE(lcArrayNam + '[1,1]') = 'U'
  DECLARE laLine(1,2)
  laLine[1,1] = &lcArrayNam
  laLine[1,2] = REPLICATE('X' , lnLineWid)
ELSE    && Else
  
  *FOR Loop to scan the Array (lcArrayNam) [IF The Array is more than 5 rows
  * scan the First 5 only]
  FOR lnRowNumb = 1 TO MIN(5 , ALEN(&lcArrayNam , 1))
    
    *IF The First column of the current row is not of type Character
    * OR if it is empty OR its not holding the name of a Variable of type
    *Character
    IF TYPE(lcArrayNam + '[' + STR(lnRowNumb , 1) + ',1]') <> 'C' .OR.;
       EMPTY(&lcArrayNam.[lnRowNumb , 1]) .OR.;
       TYPE(&lcArrayNam.[lnRowNumb , 1]) <> 'C'
      
      EXIT
    ELSE    && Else
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
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF

lnMaxWid = MIN(lnMaxWid , 58)
lnLineNo = ALEN(laLine , 1)          && Variable to hold the number of lines
lnLineWid = lnMaxWid * 2
lnLinePos = ((615 - (lnLineWid * 5) - 3) / 8) / 2
lnBoxWid = (lnLineWid * 5 / 8) + 2
lnBoxHi = ((lnLineNo*14)+13-lnLineNo)/12

lcSOkBmp  = gcBmpHome + 'OK.BMP'          && Variable to hold the name and location of the OK BMP
lcSCanBmp = gcBmpHome + 'CAN.BMP'          && Variable to hold the name and location of the CANCEL BMP

DO SYOPTMSG.SPR

*!*************************************************************
*! Name      : lfvOk
*! Developer : Haytham El_Sheltawi
*! Date      : 10/15/1997
*! Purpose   : Valid function of push button Ok
*!*************************************************************
*! Called from : SCREEN SYOPTMSG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOk

PRIVATE lcVarName

*IF lcArrayNam is not an Array [IF lcArrayNam is a Variable]
IF TYPE(lcArrayNam + '[1,1]') = 'U'
  &lcArrayNam = laLine[1,1]
ELSE    && Else
  
  *FOR Loop to scan the Array
  FOR lnRowNumb = 1 TO ALEN(laLine , 1)
    lcVarName = &lcArrayNam.[lnRowNumb , 1]
    &lcVarName = laLine[lnRowNumb , 1]
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF
