*:***************************************************************************
*: Program file  : ICOTSEX.PRG
*: Program desc. : Export OTS to Excel
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/30/2017
*! Entry No.     : C201993 - {P20170224.0002}
*!***************************************************************************
*: Modifications:
*!***************************************************************************
lcogexpr = gfopgrid('ICOTSEX',.T.)
*!*************************************************************
*! Name      : lfmajttget
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : major length
*!*************************************************************
FUNCTION lfmajttget
 RETURN gfitemmask('HM')
*
*!*************************************************************
*! Name      : lfmajpic
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : major picture
*!*************************************************************
FUNCTION lfmajpic
 lcmajpic = '@! '+gfitemmask('PM')
 RETURN lcmajpic
*
*!*************************************************************
*! Name      : lfsrsty
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Set/Reset Style browser
*!*************************************************************
PROCEDURE lfsrsty
 PARAMETER lcparm
 IF lcparm='S'
    =gfOpenTable('Style','STYLE','SH','STYLE_X')
    SELECT STYLE
    SET ORDER TO CSTYLE
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GOTO TOP IN STYLE
 ELSE
    gfCloseTable('STYLE_X')
    SELECT STYLE
    SET ORDER TO STYLE
 ENDIF
*
*!*************************************************************
*! Name      : lfstysum
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Style Qtys summation 
*!*************************************************************
FUNCTION lfstysum
 PARAMETER lcsty, lccomp, lnaddtovar
 PRIVATE lnstyrec
 lntotcomp = 0
 IF RECCOUNT('STYLE')<>0
    lnstyrec = RECNO('STYLE')
    SELECT STYLE_X
    SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
    SELECT style
    IF BETWEEN(lnstyrec, 1, RECCOUNT())
       GOTO lnstyrec
    ENDIF
    DO CASE
       CASE lnaddtovar=1
          lno_t_s = lntotcomp
       CASE lnaddtovar=2
          lno_t_s = lno_t_s+lntotcomp
       CASE lnaddtovar=3
          lno_t_s = lno_t_s-lntotcomp
    ENDCASE
 ENDIF
 RETURN INT(lntotcomp)
*!*************************************************************
*! Name      : lfwmask
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : When of style mask
*!*************************************************************
PROCEDURE lfwmask

*!*************************************************************
*! Name      : lfgetclr
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : get Color length
*!*************************************************************
PROCEDURE lfgetclr
 PRIVATE lamajsegs, i
 DIMENSION lamajsegs[ 1, 1]
 = gfitemmask(@lamajsegs)
 FOR i = 1 TO ALEN(lamajsegs, 1)
    IF lamajsegs(i,1)='C'
       lnclrpo = lamajsegs(i,4)
       lncolorlen = LEN(lamajsegs(i,3))
    ENDIF
 ENDFOR
*!*************************************************************
*! Name      : lfExportOTS
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Export OTS
*!*************************************************************
FUNCTION lfExportOTS
 STORE 0 TO lncolorlen, lnclrpo, lnstypos
 = lfGetClr()
 PRIVATE lctmpsty
 lcTmpSty = loogscroll.gftempname()
 lnMajLen = LEN(gfitemmask('PM'))
 =gfOpenTable('STYLE','STYLE','SH')
 SELECT style
 = AFIELDS(lastru)
 =gfCrtTmp(lctmpsty,@lastru,"STYLE",'STYLE')
 
llSelectStyle = .F.
lcCursorStyle = ''
lnPosStyle = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
IF lnPosStyle > 0
  lnPosStyle = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStyle,1)
  lcCursorStyle= loOgScroll.laOgFxFlt[lnPosStyle,6]
  IF !EMPTY(lcCursorStyle)
    SELECT(lcCursorStyle)
    LOCATE
    IF !EOF()
      llSelectStyle = .T.
	ENDIF
  ENDIF	
ENDIF
lcSeaFile = ''
llUseSeason  = .F.
lnSeaPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
IF lnSeaPos > 0
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnSeaPos,6]),loOgScroll.laOgFXFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel)
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
  ENDIF
ENDIF

*DIVISION
llUseDiv = ''
llUseDiv  = .F.
lnDivPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CDIVISION")
IF lnDivPos > 0
  lnDivPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnDivPos,6]),loOgScroll.laOgFXFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel)
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
  ENDIF
ENDIF


*Style Group
lcGrpFile =''
llUseGrp  = .F.
lnGrpPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYGROUP")
IF lnGrpPos  > 0
  lnGrpPos  = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnGrpPos ,1)
  lcGrpSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnGrpPos ,6]),loOgScroll.laOgFXFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel)
    lcGrpFile = loOGScroll.gfTempName()
    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
  ENDIF
ENDIF

*Color
lcClr1File =''
llUseClr1  = .F.
lnClr1Pos = ASCAN(loOgScroll.laOgFXFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
IF lnClr1Pos > 0
  lnClr1Pos  = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnClr1Pos ,6]),loOgScroll.laOgFXFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel )
    lcClr1File = loOGScroll.gfTempName()
    llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File )
  ENDIF
ENDIF
lcmsk = STRTRAN(lcrpmask, '*', '?')
IF llSelectStyle 
  SELECT (lcCursorStyle) 
  LOCATE
  SCAN 
     lcSeekSty = SUBSTR(&lcCursorStyle..cStyMajor, 1, lnmajlen)
     SELECT STYLE
     = gfSEEK(lcseeksty, 'STYLE')
     SCAN REST WHILE STYLE = lcSeekSty FOR IIF(llUseClr1 ,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcClr1File ),.T.) AND ;
                                       IIF(llUseSeason ,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
                                       IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
                                       IIF(llUseGrp ,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
                                       IIF(LEN(STRTRAN(lcrpmask, '*', ''))>0, LIKE(lcmsk,SUBSTR(STYLE,1,lnMajLen)),.T.) AND ;
                                       LEN(ALLTRIM(cStyMajor))<=11 AND (RIGHT(ALLTRIM(cStyMajor),1) $ [A|B|T] .OR. RIGHT(ALLTRIM(cStyMajor),2) $ [BX|BY|BZ|TX] )
                                       
        WAIT WINDOW NOWAIT style
        SCATTER MEMVAR
        INSERT INTO &lcTmpSty FROM MEMVAR                              
     ENDSCAN
  ENDSCAN 
ELSE
  SELECT STYLE
  =gfSeek('')
  LOCATE
  SCAN REST FOR IIF(llUseClr1 ,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcClr1File ),.T.) AND ;
                                       IIF(llUseSeason ,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
                                       IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
                                       IIF(llUseGrp ,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
                                       IIF(LEN(STRTRAN(lcRpMask, '*', ''))>0, LIKE(lcmsk,SUBSTR(STYLE,1,lnMajLen)),.T.) AND ;
                                       LEN(ALLTRIM(cStyMajor))<=11 AND (RIGHT(ALLTRIM(cStyMajor),1) $ [A|B|T] .OR. RIGHT(ALLTRIM(cStyMajor),2) $ [BX|BY|BZ|TX] )
                                     
    WAIT WINDOW NOWAIT style
    SCATTER MEMVAR
    INSERT INTO &lcTmpSty FROM MEMVAR                              
  ENDSCAN
ENDIF
SELECT &lcTmpSty 
LOCATE
IF EOF(lctmpsty)
  = gfmodalgen('INM00000B00000',.F.,.F.,.F.,'No records to display.')
  RETURN .f.
ENDIF
SELECT &lcTmpSty 
WAIT WINDOW NOWAIT 'Opening the Excel application ... '
lctempmemo = loogscroll.gftempname()
SAVE TO (oAriaApplication.WorkDir+lctempmemo+'.MEM')
lccommline = (oAriaApplication.WorkDir+lctempmemo+'.MEM')
lfCreateExcel(lccommline)
WAIT CLEAR


 
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE
  CASE   ALLTRIM(lcFieldName) = 'SEASON'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

  CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

  CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

  CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
ENDCASE
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfCreateExcel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Export to Excel
*!*************************************************************
FUNCTION lfCreateExcel
 LPARAMETERS lcmemofile
 
 IF TYPE('lcMemoFile')<>'C'
    RETURN
 ENDIF
 RESTORE FROM (lcmemofile) ADDITIVE
 lcdate = DATE()
 lctime = SUBSTR(TIME(), 1, 5)
 lnxlspagelns = 48
 SELECT (lctmpsty)
 rpsheet = CREATEOBJECT("EXCEL.APPLICATION")
 row = 1
 WITH rpsheet
    .workbooks.add
    .activeworkbook.sheets(1).activate
    .caption = 'Falcon Bay OTS Custom Report'
    DIMENSION lasizes[24]
    lasizes[1] = '     '
    lasizes[2] = '     '
    lasizes[3] = 'S    '
    lasizes[4] = 'M    '
    lasizes[5] = 'L    '
    lasizes[6] = 'XL   '
    lasizes[7] = 'XXL  '
    lasizes[8] = '1X   '
    lasizes[9] = '2X   '
    lasizes[10] = '3X   '
    lasizes[11] = '4X   '
    lasizes[12] = '5X   '
    lasizes[13] = '6X   '
    lasizes[14] = '7X   '
    lasizes[15] = '8X   '
    lasizes[16] = '9X   '
    lasizes[17] = '10X  '
    lasizes[18] = 'LT   '
    lasizes[19] = 'XLT  '
    lasizes[20] = '2XLT '
    lasizes[21] = '3XLT '
    lasizes[22] = '4XLT '
    lasizes[23] = '5XLT '
    lasizes[24] = '6XLT '
    SELECT (lctmpsty)
    LOCATE
    lclastprefix = SUBSTR(style, 1, 2)
    DO WHILE  .NOT. EOF(lctmpsty)
       IF MOD(row, lnxlspagelns)=1
          .cells(row, 1) = lcdate
          .cells(row, 2) = lctime
          FOR lncol = 3 TO 24
             .cells(row, lncol) = ALLTRIM(lasizes(lncol))
          ENDFOR
          .cells(row, 25) = 'TOT'
          lcrow = ALLTRIM(STR(row))
          lcrange = "A"+lcrow+":"+"Y"+lcrow
          .range(lcrange).select
          WITH rpsheet.selection
             .font.name = 'ARIAL'
             .font.size = 10
             .font.bold = .T.
             .cells.horizontalalignment = -4152
          ENDWITH
          .cells(row, 1).numberformat = "mm/dd/yy;@"
          .cells(row, 2).numberformat = "[$-409]h:mm AM/PM;@"
          row = row+1
       ENDIF
       SELECT &lctmpsty
       DO CASE
          CASE RIGHT(ALLTRIM(cstymajor), 1)$'A|B|T'
             lnstylen = 1
          CASE RIGHT(ALLTRIM(cstymajor), 2)$'BX|BY|BZ|TX'
             lnstylen = 2
       ENDCASE
       lcstyname = SUBSTR(cstymajor, 1, LEN(ALLTRIM(cstymajor))-lnstylen)
       .cells(row, 1) = lcstyname
       .cells(row, 1).font.bold = .T.
       .cells(row, 1).horizontalalignment = -4131
       .cells(row, 2) = 'WIP/STK'
       .cells(row+1, 2) = 'SOLD'
       .cells(row+2, 2) = 'OTS'
       STORE 0 TO lntotwip, lntotord, lntotots
       WAIT WINDOW NOWAIT lcstyname
       SCAN REST WHILE style=lcstyname
          lcstymajor = ALLTRIM(cstymajor)
          DO CASE
             CASE RIGHT(lcstymajor, 1)='A'
                = lfupdcells(3, 7, 1)
             CASE RIGHT(lcstymajor, 1)='B'
                = lfupdcells(8, 11, 1)
             CASE RIGHT(lcstymajor, 2)='BX'
                = lfupdcells(12, 13, 5)
             CASE RIGHT(lcstymajor, 2)='BY'
                = lfupdcells(14, 15, 7)
             CASE RIGHT(lcstymajor, 2)='BZ'
                = lfupdcells(16, 17, 1)
             CASE RIGHT(lcstymajor, 1)='T'
                = lfupdcells(18, 22, 1)
             CASE RIGHT(lcstymajor, 2)='TX'
                = lfupdcells(23, 24, 6)
          ENDCASE
       ENDSCAN
       .cells(row, 25) = lntotwip
       .cells(row+1, 25) = lntotord
       .cells(row+2, 25) = lntotots
       IF lclastprefix <> SUBSTR(&lctmpsty..STYLE,1,2)
          lclastprefix = SUBSTR(style, 1, 2)
          row = row+lnxlspagelns-MOD(row, lnxlspagelns)+1
          LOOP
       ENDIF
       row = row+4
       IF MOD(row, lnxlspagelns)=2
          row = row-1
       ENDIF
       lclastprefix = SUBSTR(style, 1, 2)
    ENDDO
    lcrow = ALLTRIM(STR(row))
    lcrange = "C1:Y"+lcrow
    .range(lcrange).select
    .selection.cells.numberformat = "0_);[Red](0)"
    .columns(1).columnwidth = 8.86 
    .columns(2).columnwidth = 8.30 
    .columns(3).columnwidth = 4.43 
    .columns(4).columnwidth = 4.43 
    .columns(5).columnwidth = 4.43 
    .columns(6).columnwidth = 4.43 
    .columns(7).columnwidth = 4.43 
    .columns(8).columnwidth = 4.43 
    .columns(9).columnwidth = 4.43 
    .columns(10).columnwidth = 4.43 
    .columns(11).columnwidth = 4.43 
    .columns(12).columnwidth = 4.43 
    .columns(13).columnwidth = 4.43 
    .columns(14).columnwidth = 4.43 
    .columns(15).columnwidth = 4.43 
    .columns(16).columnwidth = 4.43 
    .columns(17).columnwidth = 4.43 
    .columns(18).columnwidth = 4.43 
    .columns(19).columnwidth = 4.43 
    .columns(20).columnwidth = 4.55 
    .columns(21).columnwidth = 4.55 
    .columns(22).columnwidth = 4.55 
    .columns(23).columnwidth = 4.55 
    .columns(24).columnwidth = 4.55 
    .columns(25).columnwidth = 6.00 
    .cells(1, 1).select
 ENDWITH
 WAIT CLEAR
 row = 1
 rpsheet.visible = .T.
ENDPROC
**
*!*************************************************************
*! Name      : lfUpdCells
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Update Qty Cells
*!*************************************************************
PROCEDURE lfUpdCells
 LPARAMETERS lnfr, lnto, lnszpos
 LOCAL lncell, lcr
 FOR lncell = lnfr TO lnto
    lcr = STR(lnszpos, 1)
    .cells(ROW  ,lncell).VALUE = wip&lcr + stk&lcr            
    .cells(ROW+1,lncell).VALUE = ord&lcr                      
    .cells(ROW+2,lncell).VALUE = stk&lcr + wip&lcr - ord&lcr  
    lntotwip = lntotwip + wip&lcr + stk&lcr 
    lntotord = lntotord + ord&lcr
    lntotots = lntotots + stk&lcr + wip&lcr - ord&lcr
    lnszpos = lnszpos+1
 ENDFOR
*!*************************************************************
*! Name      : lfvmask
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : Style Mask Validation
*!*************************************************************
FUNCTION lfvmask
 lcrpmask = STRTRAN(lcrpmask, ' ', '*')
*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/30/2017
*! Purpose   : OG When Function
*!*************************************************************
FUNCTION lfwRepWhen
lcrpmask = '*************'