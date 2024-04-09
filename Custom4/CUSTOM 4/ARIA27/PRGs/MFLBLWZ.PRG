*:***************************************************************************
*: Program file  : MFLBLWZ
*: Program desc. : CARE LABEL & HANG TAG DESIGNER in ARIA 27
*: For screen    : MFLBLWZ.SCX
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 05/27/2003
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO MFLBLWZ
*:***************************************************************************
*: Due to E500452
*:***************************************************************************
*

PRIVATE laStep,laLblDim,laLblFld,laLblLine,laLblPic,lcText,lcTmpLblOb,lcLblSave
PRIVATE lnStep,lnLblDim,lnLblFld,lnLblLine,lnLblPic,lnEngMet,lnLblSave
PRIVATE lcLineAlrm,lcHghtAlrm,lcLineFont

STORE ' ' TO laStep,laLblDim,laLblFld,laLblLine,laLblPic,lcText,lcTmpLblOb,lcLblSave
STORE 1   TO lnStep,lnLblDim,lnLblFld,lnLblLine,lnLblPic,lnEngMet,lnLblSave
STORE SPACE(0) TO lcLineAlrm,lcHghtAlrm,lcLineFont

lcStep1Pic = gcBmpHome + "STEP1.BMP"
lcStep2Pic = gcBmpHome + "STEP2.BMP"
lcStep3Pic = gcBmpHome + "STEP3.BMP"
lcMoveR    = gcBmpHome + "ADD.BMP"
lcMoveL    = gcBmpHome + "REMOVE.BMP"

*-- To get the style header
lcStyHdr   = ALLTRIM(gfItemMask("HM"))
lnStyleWid = LEN(gfItemMask("PM"))

IF !gfSetup()
  RETURN
ENDIF  

lcTmpLblOb = gfTempName()
CREATE CURSOR (lcTmpLblOb) (NLINENO N(2,0),NORDER N(2,0),CTYPE C(1),MEXPR M,VPOS N(9,3),;
                            HPOS N(9,3),HEIGHT N(9,3),WIDTH N(9,3))
INDEX ON STR(NLINENO,2,0)+STR(NORDER,2,0) TAG (lcTmpLblOb) OF (lcTmpLblOb)

lcTmpFile  = gfTempName()
DECLARE laFldStu[74,4]

laFldStu[01,1] = 'PLATFORM'
laFldStu[01,2] = 'C'
laFldStu[01,3] = 8
laFldStu[01,4] = 0

laFldStu[02,1] = 'UNIQUEID'
laFldStu[02,2] = 'C'
laFldStu[02,3] = 10
laFldStu[02,4] = 0

laFldStu[03,1] = 'TIMESTAMP'
laFldStu[03,2] = 'N'
laFldStu[03,3] = 10
laFldStu[03,4] = 0

laFldStu[04,1] = 'OBJTYPE'
laFldStu[04,2] = 'N'
laFldStu[04,3] = 2
laFldStu[04,4] = 0

laFldStu[05,1] = 'OBJCODE'
laFldStu[05,2] = 'N'
laFldStu[05,3] = 3
laFldStu[05,4] = 0

laFldStu[06,1] = 'NAME'
laFldStu[06,2] = 'M'
laFldStu[06,3] = 10
laFldStu[06,4] = 0

laFldStu[07,1] = 'EXPR'
laFldStu[07,2] = 'M'
laFldStu[07,3] = 10
laFldStu[07,4] = 0

laFldStu[08,1] = 'VPOS'
laFldStu[08,2] = 'N'
laFldStu[08,3] = 9
laFldStu[08,4] = 3

laFldStu[09,1] = 'HPOS'
laFldStu[09,2] = 'N'
laFldStu[09,3] = 9
laFldStu[09,4] = 3

laFldStu[10,1] = 'HEIGHT'
laFldStu[10,2] = 'N'
laFldStu[10,3] = 9
laFldStu[10,4] = 3

laFldStu[11,1] = 'WIDTH'
laFldStu[11,2] = 'N'
laFldStu[11,3] = 9
laFldStu[11,4] = 3

laFldStu[12,1] = 'STYLE'
laFldStu[12,2] = 'M'
laFldStu[12,3] = 10
laFldStu[12,4] = 0

laFldStu[13,1] = 'PICTURE'
laFldStu[13,2] = 'M'
laFldStu[13,3] = 10
laFldStu[13,4] = 0

laFldStu[14,1] = 'ORDER'
laFldStu[14,2] = 'M'
laFldStu[14,3] = 10
laFldStu[14,4] = 0

laFldStu[15,1] = 'UNIQUE'
laFldStu[15,2] = 'L'
laFldStu[15,3] = 1
laFldStu[15,4] = 0

laFldStu[16,1] = 'COMMENT'
laFldStu[16,2] = 'M'
laFldStu[16,3] = 10
laFldStu[16,4] = 0

laFldStu[17,1] = 'ENVIRON'
laFldStu[17,2] = 'L'
laFldStu[17,3] = 1
laFldStu[17,4] = 0

laFldStu[18,1] = 'BOXCHAR'
laFldStu[18,2] = 'C'
laFldStu[18,3] = 1
laFldStu[18,4] = 0

laFldStu[19,1] = 'FILLCHAR'
laFldStu[19,2] = 'C'
laFldStu[19,3] = 1
laFldStu[19,4] = 0

laFldStu[20,1] = 'TAG'
laFldStu[20,2] = 'M'
laFldStu[20,3] = 10
laFldStu[20,4] = 0

laFldStu[21,1] = 'TAG2'
laFldStu[21,2] = 'M'
laFldStu[21,3] = 10
laFldStu[21,4] = 0

laFldStu[22,1] = 'PENRED'
laFldStu[22,2] = 'N'
laFldStu[22,3] = 5
laFldStu[22,4] = 0

laFldStu[23,1] = 'PENGREEN'
laFldStu[23,2] = 'N'
laFldStu[23,3] = 5
laFldStu[23,4] = 0

laFldStu[24,1] = 'PENBLUE'
laFldStu[24,2] = 'N'
laFldStu[24,3] = 5
laFldStu[24,4] = 0

laFldStu[25,1] = 'FILLRED'
laFldStu[25,2] = 'N'
laFldStu[25,3] = 5
laFldStu[25,4] = 0

laFldStu[26,1] = 'FILLGREEN'
laFldStu[26,2] = 'N'
laFldStu[26,3] = 5
laFldStu[26,4] = 0

laFldStu[27,1] = 'FILLBLUE'
laFldStu[27,2] = 'N'
laFldStu[27,3] = 5
laFldStu[27,4] = 0

laFldStu[28,1] = 'PENSIZE'
laFldStu[28,2] = 'N'
laFldStu[28,3] = 5
laFldStu[28,4] = 0

laFldStu[29,1] = 'PENPAT'
laFldStu[29,2] = 'N'
laFldStu[29,3] = 5
laFldStu[29,4] = 0

laFldStu[30,1] = 'FILLPAT'
laFldStu[30,2] = 'N'
laFldStu[30,3] = 5
laFldStu[30,4] = 0

laFldStu[31,1] = 'FONTFACE'
laFldStu[31,2] = 'M'
laFldStu[31,3] = 10
laFldStu[31,4] = 0

laFldStu[32,1] = 'FONTSTYLE'
laFldStu[32,2] = 'N'
laFldStu[32,3] = 3
laFldStu[32,4] = 0

laFldStu[33,1] = 'FONTSIZE'
laFldStu[33,2] = 'N'
laFldStu[33,3] = 3
laFldStu[33,4] = 0

laFldStu[34,1] = 'MODE'
laFldStu[34,2] = 'N'
laFldStu[34,3] = 3
laFldStu[34,4] = 0

laFldStu[35,1] = 'RULER'
laFldStu[35,2] = 'N'
laFldStu[35,3] = 1
laFldStu[35,4] = 0

laFldStu[36,1] = 'RULERLINES'
laFldStu[36,2] = 'N'
laFldStu[36,3] = 1
laFldStu[36,4] = 0

laFldStu[37,1] = 'GRID'
laFldStu[37,2] = 'L'
laFldStu[37,3] = 1
laFldStu[37,4] = 0

laFldStu[38,1] = 'GRIDV'
laFldStu[38,2] = 'N'
laFldStu[38,3] = 2
laFldStu[38,4] = 0

laFldStu[39,1] = 'GRIDH'
laFldStu[39,2] = 'N'
laFldStu[39,3] = 2
laFldStu[39,4] = 0

laFldStu[40,1] = 'FLOAT'
laFldStu[40,2] = 'L'
laFldStu[40,3] = 1
laFldStu[40,4] = 0

laFldStu[41,1] = 'STRETCH'
laFldStu[41,2] = 'L'
laFldStu[41,3] = 1
laFldStu[41,4] = 0

laFldStu[42,1] = 'STRETCHTOP'
laFldStu[42,2] = 'L'
laFldStu[42,3] = 1
laFldStu[42,4] = 0

laFldStu[43,1] = 'TOP'
laFldStu[43,2] = 'L'
laFldStu[43,3] = 1
laFldStu[43,4] = 0

laFldStu[44,1] = 'BOTTOM'
laFldStu[44,2] = 'L'
laFldStu[44,3] = 1
laFldStu[44,4] = 0

laFldStu[45,1] = 'SUPTYPE'
laFldStu[45,2] = 'N'
laFldStu[45,3] = 1
laFldStu[45,4] = 0

laFldStu[46,1] = 'SUPREST'
laFldStu[46,2] = 'N'
laFldStu[46,3] = 1
laFldStu[46,4] = 0

laFldStu[47,1] = 'NOREPEAT'
laFldStu[47,2] = 'L'
laFldStu[47,3] = 1
laFldStu[47,4] = 0

laFldStu[48,1] = 'RESETRPT'
laFldStu[48,2] = 'N'
laFldStu[48,3] = 2
laFldStu[48,4] = 0

laFldStu[49,1] = 'PAGEBREAK'
laFldStu[49,2] = 'L'
laFldStu[49,3] = 1
laFldStu[49,4] = 0

laFldStu[50,1] = 'COLBREAK'
laFldStu[50,2] = 'L'
laFldStu[50,3] = 1
laFldStu[50,4] = 0

laFldStu[51,1] = 'RESETPAGE'
laFldStu[51,2] = 'L'
laFldStu[51,3] = 1
laFldStu[51,4] = 0

laFldStu[52,1] = 'GENERAL'
laFldStu[52,2] = 'N'
laFldStu[52,3] = 3
laFldStu[52,4] = 0

laFldStu[53,1] = 'SPACING'
laFldStu[53,2] = 'N'
laFldStu[53,3] = 3
laFldStu[53,4] = 0

laFldStu[54,1] = 'DOUBLE'
laFldStu[54,2] = 'L'
laFldStu[54,3] = 1
laFldStu[54,4] = 0

laFldStu[55,1] = 'SWAPHEADER'
laFldStu[55,2] = 'L'
laFldStu[55,3] = 1
laFldStu[55,4] = 0

laFldStu[56,1] = 'SWAPFOOTER'
laFldStu[56,2] = 'L'
laFldStu[56,3] = 1
laFldStu[56,4] = 0

laFldStu[57,1] = 'EJECTBEFOR'
laFldStu[57,2] = 'L'
laFldStu[57,3] = 1
laFldStu[57,4] = 0

laFldStu[58,1] = 'EJECTAFTER'
laFldStu[58,2] = 'L'
laFldStu[58,3] = 1
laFldStu[58,4] = 0

laFldStu[59,1] = 'PLAIN'
laFldStu[59,2] = 'L'
laFldStu[59,3] = 1
laFldStu[59,4] = 0

laFldStu[60,1] = 'SUMMARY'
laFldStu[60,2] = 'L'
laFldStu[60,3] = 1
laFldStu[60,4] = 0

laFldStu[61,1] = 'ADDALIAS'
laFldStu[61,2] = 'L'
laFldStu[61,3] = 1
laFldStu[61,4] = 0

laFldStu[62,1] = 'OFFSET'
laFldStu[62,2] = 'N'
laFldStu[62,3] = 3
laFldStu[62,4] = 0

laFldStu[63,1] = 'TOPMARGIN'
laFldStu[63,2] = 'N'
laFldStu[63,3] = 3
laFldStu[63,4] = 0

laFldStu[64,1] = 'BOTMARGIN'
laFldStu[64,2] = 'N'
laFldStu[64,3] = 3
laFldStu[64,4] = 0

laFldStu[65,1] = 'TOTALTYPE'
laFldStu[65,2] = 'N'
laFldStu[65,3] = 2
laFldStu[65,4] = 0

laFldStu[66,1] = 'RESETTOTAL'
laFldStu[66,2] = 'N'
laFldStu[66,3] = 2
laFldStu[66,4] = 0

laFldStu[67,1] = 'RESOID'
laFldStu[67,2] = 'N'
laFldStu[67,3] = 3
laFldStu[67,4] = 0

laFldStu[68,1] = 'CURPOS'
laFldStu[68,2] = 'L'
laFldStu[68,3] = 1
laFldStu[68,4] = 0

laFldStu[69,1] = 'SUPALWAYS'
laFldStu[69,2] = 'L'
laFldStu[69,3] = 1
laFldStu[69,4] = 0

laFldStu[70,1] = 'SUPOVFLOW'
laFldStu[70,2] = 'L'
laFldStu[70,3] = 1
laFldStu[70,4] = 0

laFldStu[71,1] = 'SUPRPCOL'
laFldStu[71,2] = 'N'
laFldStu[71,3] = 1
laFldStu[71,4] = 0

laFldStu[72,1] = 'SUPGROUP'
laFldStu[72,2] = 'N'
laFldStu[72,3] = 2
laFldStu[72,4] = 0

laFldStu[73,1] = 'SUPVALCHNG'
laFldStu[73,2] = 'L'
laFldStu[73,3] = 1
laFldStu[73,4] = 0

laFldStu[74,1] = 'SUPEXPR'
laFldStu[74,2] = 'M'
laFldStu[74,3] = 10
laFldStu[74,4] = 0

CREATE TABLE (gcWorkDir+lcTmpFile+'.LBX') FROM ARRAY laFldStu

DECLARE laStep[3],laLblLine[1,8],laLblPic[1,2]
laStep[1] = 'Step 1 - Choose Label Dimensions'
laStep[2] = 'Step 2 - Define Layout'
laStep[3] = 'Step 3 - Finish'

laLblLine[1,1] = REPLICATE('.',60)
laLblLine[1,2] = 'Arial'
laLblLine[1,3] = 8
laLblLine[1,4] = 'N'
laLblLine[1,5] = 'Regular'
laLblLine[1,6] = 1666.667
laLblLine[1,7] = 0
laLblLine[1,8] = 0
lcLineFont = laLblLine[lnLblLine,2]+STR(laLblLine[lnLblLine,3],3,0)+' '+laLblLine[lnLblLine,5]

=lfGetPic()

DO CASE
  CASE gcAct_Appl = "MF"
    DECLARE laLblFld[10,3]
    laLblFld[01,1] = PADR('Cutting Ticket'    ,20) + ' 6'
    laLblFld[02,1] = PADR(lcStyHdr            ,20) + STR(lnStyleWid,2,0)
    laLblFld[03,1] = PADR('Size'              ,20) + ' 5'
    laLblFld[04,1] = PADR('Color description' ,20) + '30'
    laLblFld[05,1] = PADR('Dyelot'            ,20) + '10'
    laLblFld[06,1] = PADR('Style description' ,20) + '20'
    laLblFld[07,1] = PADR('Style price A'     ,20) + '12'
    laLblFld[08,1] = PADR('Fabric description',20) + '20'
    laLblFld[09,1] = PADR('Fabric contents'   ,20) + '30'
    laLblFld[10,1] = PADR('Color code'        ,20) + ' 6'

    laLblFld[01,2] = 'CUTTKT'
    laLblFld[02,2] = 'STYLE'
    laLblFld[03,2] = 'SIZE'
    laLblFld[04,2] = 'CLRDESC'
    laLblFld[05,2] = 'DYELOT'
    laLblFld[06,2] = 'STYDESC'
    laLblFld[07,2] = 'PRICEA'
    laLblFld[08,2] = 'FABDESC'
    laLblFld[09,2] = 'FABCONT'
    laLblFld[10,2] = 'COLOR'
    
    laLblFld[01,3] = 10
    laLblFld[02,3] = lnStyleWid + 4
    laLblFld[03,3] = 9
    laLblFld[04,3] = 34
    laLblFld[05,3] = 14
    laLblFld[06,3] = 24
    laLblFld[07,3] = 16
    laLblFld[08,3] = 24
    laLblFld[09,3] = 34
    laLblFld[10,3] = 10
  CASE gcAct_Appl = "PO"
    DECLARE laLblFld[9,3]
    laLblFld[01,1] = PADR('Purchase order'    ,20) + ' 6'
    laLblFld[02,1] = PADR(lcStyHdr            ,20) + STR(lnStyleWid,2,0)
    laLblFld[03,1] = PADR('Size'              ,20) + ' 5'
    laLblFld[04,1] = PADR('Color description' ,20) + '30'
    laLblFld[05,1] = PADR('Style description' ,20) + '20'
    laLblFld[06,1] = PADR('Style price A'     ,20) + '12'
    laLblFld[07,1] = PADR('Fabric description',20) + '20'
    laLblFld[08,1] = PADR('Fabric contents'   ,20) + '30'
    laLblFld[09,1] = PADR('Color code'        ,20) + ' 6'

    laLblFld[01,2] = 'PO'
    laLblFld[02,2] = 'STYLE'
    laLblFld[03,2] = 'SIZE'
    laLblFld[04,2] = 'CLRDESC'
    laLblFld[05,2] = 'STYDESC'
    laLblFld[06,2] = 'PRICEA'
    laLblFld[07,2] = 'FABDESC'
    laLblFld[08,2] = 'FABCONT'
    laLblFld[09,2] = 'COLOR'
    
    laLblFld[01,3] = 10
    laLblFld[02,3] = lnStyleWid + 4
    laLblFld[03,3] = 9
    laLblFld[04,3] = 34
    laLblFld[05,3] = 24
    laLblFld[06,3] = 16
    laLblFld[07,3] = 24
    laLblFld[08,3] = 34
    laLblFld[09,3] = 10
  CASE gcAct_Appl = "SO"
    DECLARE laLblFld[7,3]
    laLblFld[01,1] = PADR('Sales Order'      ,20) + ' 6'
    laLblFld[02,1] = PADR(lcStyHdr           ,20) + STR(lnStyleWid,2,0)
    laLblFld[03,1] = PADR('Size'             ,20) + ' 5'
    laLblFld[04,1] = PADR('Color description',20) + '30'
    laLblFld[05,1] = PADR('Style description',20) + '20'
    laLblFld[06,1] = PADR('Style price A'    ,20) + '12'
    laLblFld[07,1] = PADR('Color code'       ,20) + ' 6'

    laLblFld[01,2] = 'ORDER'
    laLblFld[02,2] = 'STYLE'
    laLblFld[03,2] = 'SIZE'
    laLblFld[04,2] = 'CLRDESC'
    laLblFld[05,2] = 'STYDESC'
    laLblFld[06,2] = 'PRICEA'
    laLblFld[07,2] = 'COLOR'
    
    laLblFld[01,3] = 10
    laLblFld[02,3] = lnStyleWid + 4
    laLblFld[03,3] = 9
    laLblFld[04,3] = 34
    laLblFld[05,3] = 24
    laLblFld[06,3] = 16
    laLblFld[07,3] = 10
ENDCASE

DECLARE laLblDim[1,8]
=lfGetDim()

lcEscKey = ON('KEY','ESC')
ON KEY LABEL ESC DO lfvCancel
DO (gcScrDir+'MFLBLWZ.SPX')
ON KEY LABEL ESC &lcEscKey.

*-- Screen Cleanup Code.
IF WEXIST('MFLBLFONT')
  RELEASE WINDOWS MFLBLFONT
ENDIF
glQuitting = .T.  && Rise quit flag because it's modal screen.

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
  ERASE (gcWorkDir+lcTmpFile+'.LBX')
  ERASE (gcWorkDir+lcTmpFile+'.LBT')
ENDIF
*-- end of program code.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvCancel
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/27/2003
*! Purpose   : To validate the Cancel button.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*!
FUNCTION lfvCancel

CLEAR READ
*-- end of lfvCancel.

*!*************************************************************
*! Name      : lfvBack
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/27/2003
*! Purpose   : To validate the Back button.
*!*************************************************************
*!
FUNCTION lfvBack

lnStep = lnStep - 1
SHOW GET lnStep
=lfvStep()
*-- end of lfvBack.

*!*************************************************************
*! Name      : lfvNext
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/27/2003
*! Purpose   : To validate the Next button.
*!*************************************************************
*!
FUNCTION lfvNext

lnStep = lnStep + 1
SHOW GET lnStep
=lfvStep()
*-- end of lfvNext.

*!*************************************************************
*! Name      : lfvFinish
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/27/2003
*! Purpose   : To validate the Finish button.
*!*************************************************************
*!
FUNCTION lfvFinish

IF lnStep < 3
  lnStep = 3
  SHOW GET lnStep
  =lfvStep()
ELSE
  PRIVATE llExit
  llExit = .F.
  
  DO (gcScrDir+'MFLBLSAV.SPX')
  
  IF llExit
    IF lnLblSave = 2
      MODIFY LABEL (gcRepHome+gcAct_Appl+'\'+gcAct_Appl+ALLTRIM(lcLblSave)+'.LBX')
      USE (gcRepHome+gcAct_Appl+'\'+gcAct_Appl+ALLTRIM(lcLblSave)+'.LBX') IN 0 ALIAS TMPLABEL
      SELECT TMPLABEL
      SCAN FOR OBJTYPE = 17
        lnOccurs = OCCURS('\',PICTURE)
        lnStart = AT('\',PICTURE,lnOccurs)
        lcPicture = SUBSTR(PICTURE,lnStart+1)
        REPLACE PICTURE WITH '"' + gcBmpHome + lcPicture
      ENDSCAN
      USE IN TMPLABEL
    ENDIF
    
    IF lnLblSave = 3
      lcRpLabel = gcAct_Appl+ALLTRIM(lcLblSave)
      SAVE TO (gcWorkDir+'LABEL.MEM') ALL LIKE lcRpLabel
      =gfOpenFile(gcSysHome+'SYCMENU',gcSysHome+'PROSS_ID','SH')
      IF SEEK('MFHNGCR   ')
        SELECT SYCMENU
        LOCATE REST WHILE CPROSS_ID = 'MFHNGCR   ' FOR CAPP_ID = gcAct_Appl
        IF FOUND()
          DO gpMenuBar WITH CMSTR_NAM , INT(VAL(CBAR_POS))
        ENDIF
      ENDIF
      USE IN SYCMENU
    ENDIF
    
    =lfvCancel()
  ENDIF
ENDIF
*-- end of lfvFinish.

*!*************************************************************
*! Name      : lfvStep
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/27/2003
*! Purpose   : Valid function of Step PopUp
*!*************************************************************
*!
FUNCTION lfvStep

DO CASE
  CASE lnStep = 1
    IF WEXIST('MFLBLFONT')
      RELEASE WINDOWS MFLBLFONT
    ENDIF
    SHOW GETS WINDOW MFLBLWZ2 ENABLE ONLY
    SHOW WINDOW MFLBLWZ2 TOP
    SHOW GETS WINDOW MFLBLWZ4 DISABLE ONLY
    SHOW GETS WINDOW MFLBLWZ5 DISABLE ONLY
    SHOW GET pbBack DISABLE
    SHOW GET pbNext ENABLE
  CASE lnStep = 2
    DEFINE WINDOW MFLBLFONT FROM 0,0 TO 0,0 FONT laLblLine[lnLblLine,2],laLblLine[lnLblLine,3];
                  STYLE laLblLine[lnLblLine,4]
    SHOW GETS WINDOW MFLBLWZ4 ENABLE ONLY
    SHOW WINDOW MFLBLWZ4 TOP
    SHOW GETS WINDOW MFLBLWZ2 DISABLE ONLY
    SHOW GETS WINDOW MFLBLWZ5 DISABLE ONLY
    SHOW GET pbBack ENABLE
    SHOW GET pbNext ENABLE
  CASE lnStep = 3
    
    IF lfChkAlrm() .AND. gfModalGen('QRM38264B00006','ALERT') = 2
      lnStep = lnStep - 1
      SHOW GET lnStep
      =lfvStep()
      RETURN
    ENDIF
    IF WEXIST('MFLBLFONT')
      RELEASE WINDOWS MFLBLFONT
    ENDIF
    SHOW GETS WINDOW MFLBLWZ5 ENABLE ONLY
    SHOW WINDOW MFLBLWZ5 TOP
    SHOW GETS WINDOW MFLBLWZ2 DISABLE ONLY
    SHOW GETS WINDOW MFLBLWZ4 DISABLE ONLY
    SHOW GET pbBack ENABLE
    SHOW GET pbNext DISABLE
    =lfCreatLbl()
ENDCASE
*-- end of lfvStep.

*!*************************************************************
*! Name      : lfGetDim
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Get the label dimensions
*!*************************************************************
*!
FUNCTION lfGetDim

SELECT PADR(CLBLNAME,25) + ' ' + PADR(CLBLDIM,20) + ' ' + ALLTRIM(STR(NLBLNMBACR,2,0)) AS CLBLDESC,;
       CLBLUOM,NLBLHEIGHT,NLBLLFTMRG,NLBLNMBACR,NLBLSPCBTW,NLBLTOPMRG,NLBLWIDTH;
       FROM SYLBLDIM;
       WHERE CLBLUOM = IIF(lnEngMet=1,'E','M');
       INTO ARRAY laLblDim
IF _TALLY = 0
  laLblDim[1,1] = ''
ENDIF
lnLblDim = 1
SHOW GET lnLblDim
*-- end of lfGetDim.

*!*************************************************************
*! Name      : lfvUserLbl
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of New Label button
*!*************************************************************
*!
FUNCTION lfvUserLbl

PRIVATE laLblDimU,lnLblDimU,lcEditStat
lcEditStat = 'ENABLE'
STORE 1 TO lnLblDimU
DECLARE laLblDimU[1,10]
=lfGetUsrDm()

DO (gcScrDir+'MFUSRLBL.SPX')

=lfGetDim()
*-- end of lfvUserLbl.

*!*************************************************************
*! Name      : lfvMoveR
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of move right button
*!*************************************************************
*!
FUNCTION lfvMoveR

PRIVATE lnOrder,lnObjHeght,lnObjWidth
lnObjHeght = ROUND((FONTMETRIC(1,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                              laLblLine[lnLblLine,4])+2)/96*10000,3)
IF EMPTY(lcText)
  lnOrder    = IIF(laLblLine[lnLblLine,1]=REPLICATE('.',60),1,OCCUR('|',laLblLine[lnLblLine,1])+2)
  lnObjWidth = ROUND(((FONTMETRIC(6,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                                laLblLine[lnLblLine,4])*laLblFld[lnLblFld,3])+2)/96*10000,3)
  INSERT INTO (lcTmpLblOb) (NLINENO,NORDER,CTYPE,MEXPR,HEIGHT,WIDTH);
                    VALUES (lnLblLine,lnOrder,'F',laLblFld[lnLblFld,2],lnObjHeght,lnObjWidth)
  IF laLblLine[lnLblLine,1] = REPLICATE('.',60)
    laLblLine[lnLblLine,1] = laLblFld[lnLblFld,2]
  ELSE
    laLblLine[lnLblLine,1] = laLblLine[lnLblLine,1] + '|' + laLblFld[lnLblFld,2]
  ENDIF
  laLblLine[lnLblLine,7] = laLblLine[lnLblLine,7] + lnObjWidth
  IF lnLblFld < ALEN(laLblFld,1)
    lnLblFld = lnLblFld + 1
  ENDIF
ELSE
  lnOrder    = IIF(laLblLine[lnLblLine,1]=REPLICATE('.',60),1,OCCUR('|',laLblLine[lnLblLine,1])+2)
  lnObjWidth = ROUND(((FONTMETRIC(6,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                                laLblLine[lnLblLine,4])*LEN(ALLTRIM(lcText)))+2)/96*10000,3)
  INSERT INTO (lcTmpLblOb) (NLINENO,NORDER,CTYPE,MEXPR,HEIGHT,WIDTH);
                    VALUES (lnLblLIne,lnOrder,'T','"'+ALLTRIM(lcText)+'"',lnObjHeght,lnObjWidth)
  IF laLblLine[lnLblLine,1] = REPLICATE('.',60)
    laLblLine[lnLblLine,1] = ALLTRIM(lcText)
  ELSE
    laLblLine[lnLblLine,1] = laLblLine[lnLblLine,1] + '|' + ALLTRIM(lcText)
  ENDIF
  laLblLine[lnLblLine,7] = laLblLine[lnLblLine,7] + lnObjWidth
  lcText = ''
ENDIF

laLblLine[lnLblLine,8] = laLblLine[lnLblLine,8] + 1
=lfGetAlrm()
SHOW GETS
*-- end of lfvMoveR.

*!*************************************************************
*! Name      : lfvMoveL
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of move left button
*!*************************************************************
*!
FUNCTION lfvMoveL

PRIVATE lnLblCnt,lnOrder,lnAlias,lnLen,lnObjHeght,lnObjWidth
IF laLblLine[lnLblLine,1] = REPLICATE('.',60)
  lnLblCnt = ALEN(laLblLine,1)
  IF lnLblCnt > 1
    =ADEL(laLblLine,lnLblLine,1)
    DECLARE laLblLine[lnLblCnt-1,8]
    lnAlias = SELECT(0)
    SELECT (lcTmpLblOb)
    LOCATE FOR NLINENO > lnLblLine
    IF FOUND()
      REPLACE REST NLINENO WITH NLINENO - 1
    ENDIF
    SELECT (lnAlias)
    IF lnLblLine = lnLblCnt
      lnLblLine = lnLblLine - 1
    ENDIF
  ENDIF
ELSE
  lnOrder = OCCUR('|',laLblLine[lnLblLine,1]) + 1
  lnAlias = SELECT(0)
  STORE 0 TO lnObjHeght,lnObjWidth
  IF SEEK(STR(lnLblLine,2,0)+STR(lnOrder,2,0),lcTmpLblOb)
    SELECT (lcTmpLblOb)
    lnObjHeght = HEIGHT
    lnObjWidth = WIDTH
    DELETE
  ENDIF
  IF lnOrder = 1
    laLblLine[lnLblLine,1] = REPLICATE('.',60)
    laLblLine[lnLblLine,6] = ROUND((FONTMETRIC(1,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                                              laLblLine[lnLblLine,4])+2)/96*10000,3)
    laLblLine[lnLblLine,7] = 0
  ELSE
    lnLen = AT('|',laLblLine[lnLblLine,1],lnOrder-1) - 1
    laLblLine[lnLblLine,1] = SUBSTR(laLblLine[lnLblLine,1],1,lnLen)
    laLblLine[lnLblLine,7] = laLblLine[lnLblLine,7] - lnObjWidth
    IF lnObjHeght >= laLblLine[lnLblLine,6]
      IF SEEK(STR(lnLblLine,2,0),lcTmpLblOb)
        SELECT (lcTmpLblOb)
        CALCULATE REST WHILE STR(NLINENO,2,0)+STR(NORDER,2,0) = STR(lnLblLine,2,0);
                  MAX(HEIGHT) TO laLblLine[lnLblLine,6]
      ENDIF
    ENDIF
  ENDIF
  laLblLine[lnLblLine,8] = laLblLine[lnLblLine,8] - 1
  SELECT (lnAlias)
ENDIF
=lfGetAlrm()
SHOW GETS
*-- end of lfvMoveL.

*!*************************************************************
*! Name      : lfvSpace
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Space button
*!*************************************************************
*!
FUNCTION lfvSpace

PRIVATE lnOrder,lnObjHeght,lnObjWidth
lnObjHeght = ROUND((FONTMETRIC(1,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                              laLblLine[lnLblLine,4])+2)/96*10000,3)
lnObjWidth = ROUND((FONTMETRIC(6,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                            laLblLine[lnLblLine,4])+2)/96*10000,3)
lnOrder    = IIF(laLblLine[lnLblLine,1]=REPLICATE('.',60),1,OCCUR('|',laLblLine[lnLblLine,1])+2)
INSERT INTO (lcTmpLblOb) (NLINENO,NORDER,CTYPE,MEXPR,HEIGHT,WIDTH);
                  VALUES (lnLblLine,lnOrder,'S','.',lnObjHeght,lnObjWidth)
IF laLblLine[lnLblLine,1] = REPLICATE('.',60)
  laLblLine[lnLblLine,1] = '.'
ELSE
  laLblLine[lnLblLine,1] = laLblLine[lnLblLine,1] + '|' + '.'
ENDIF
laLblLine[lnLblLine,7] = laLblLine[lnLblLine,7] + lnObjWidth
laLblLine[lnLblLine,8] = laLblLine[lnLblLine,8] + 1
=lfGetAlrm()
SHOW GETS
*-- end of lfvSpace.

*!*************************************************************
*! Name      : lfvNewLine
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of New Line button
*!*************************************************************
*!
FUNCTION lfvNewLine

PRIVATE lnLblCnt
lnLblCnt = ALEN(laLblLine,1)
DECLARE laLblLine[lnLblCnt+1,8]
laLblLine[lnLblCnt+1,1] = REPLICATE('.',60)
laLblLine[lnLblCnt+1,2] = 'Arial'
laLblLine[lnLblCnt+1,3] = 8
laLblLine[lnLblCnt+1,4] = 'N'
laLblLine[lnLblCnt+1,5] = 'Regular'
laLblLine[lnLblCnt+1,6] = 1666.667
laLblLine[lnLblCnt+1,7] = 0
laLblLine[lnLblCnt+1,8] = 0
lnLblLine = lnLblCnt + 1
=lfGetAlrm()
SHOW GETS
*-- end of lfvNewLine.

*!*************************************************************
*! Name      : lfvFont
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Font button
*!*************************************************************
*!
FUNCTION lfvFont

PRIVATE lcFont,lnPos1,lnPos2,lcFontSty,lnObjHeght,lnObjWidth,lnLineHgt,lnLineWdth,lnPos

MODIFY WINDOW MFLBLFONT FONT laLblLine[lnLblLine,2],laLblLine[lnLblLine,3];
              STYLE laLblLine[lnLblLine,4]
ACTIVATE WINDOW MFLBLFONT IN WINDOW MFLBLWZ4
lcFont = GETFONT()
DEACTIVATE WINDOW MFLBLFONT

IF !EMPTY(lcFont)
  lnPos1 = AT(',',lcFont)
  lnPos2 = AT(',',lcFont,2)
  laLblLine[lnLblLine,2] = SUBSTR(lcFont,1,lnPos1-1)
  laLblLine[lnLblLine,3] = VAL(SUBSTR(lcFont,lnPos1+1,lnPos2-lnPos1-1))
  lcFontSty = SUBSTR(lcFont,lnPos2+1)
  laLblLine[lnLblLine,4] = lcFontSty
  DO CASE
    CASE lcFontSty == 'N'
      laLblLine[lnLblLine,5] = 'Regular'
    CASE lcFontSty == 'I'
      laLblLine[lnLblLine,5] = 'Italic'
    CASE lcFontSty == 'B'
      laLblLine[lnLblLine,5] = 'Bold'
    CASE lcFontSty == 'BI'
      laLblLine[lnLblLine,5] = 'Bold Italic'
  ENDCASE
  lnObjHeght = ROUND((FONTMETRIC(1,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                                laLblLine[lnLblLine,4])+2)/96*10000,3)
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  STORE 0 TO lnLineHgt,lnLineWdth
  SELECT (lcTmpLblOb)
  IF SEEK(STR(lnLblLine,2,0))
    SCAN REST WHILE STR(NLINENO,2,0)+STR(NORDER,2,0) = STR(lnLblLine,2,0)
      IF CTYPE # 'P'
        REPLACE HEIGHT WITH lnObjHeght
      ENDIF
      lnLineHgt = MAX(lnLineHgt,HEIGHT)
      DO CASE
        CASE CTYPE = 'F'
          lnPos = ASCAN(laLblFld,ALLTRIM(MEXPR))
          lnObjWidth = 0
          IF lnPos > 0
            lnPos = ASUBSCRIPT(laLblFld,lnPos,1)
            lnObjWidth = ROUND(((FONTMETRIC(6,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                                laLblLine[lnLblLine,4])*laLblFld[lnPos,3])+2)/96*10000,3)
          ENDIF
        CASE CTYPE = 'T'
          lnObjWidth = ROUND(((FONTMETRIC(6,laLblLine[lnLblLine,2],;
                              laLblLine[lnLblLine,3],laLblLine[lnLblLine,4])*(LEN(MEXPR)-2))+2)/96*10000,3)
        CASE CTYPE = 'S'
          lnObjWidth = ROUND((FONTMETRIC(6,laLblLine[lnLblLine,2],laLblLine[lnLblLine,3],;
                              laLblLine[lnLblLine,4])+2)/96*10000,3)
        CASE CTYPE = 'P'
          lnObjWidth = 1575
      ENDCASE
      REPLACE WIDTH WITH lnObjWidth
      lnLineWdth = lnLineWdth + lnObjWidth
    ENDSCAN
  ENDIF
  SELECT (lnAlias)
  laLblLine[lnLblLine,6] = lnLineHgt
  laLblLine[lnLblLine,7] = lnLineWdth
ENDIF
=lfGetAlrm()
*-- end of lfvFont.

*!*************************************************************
*! Name      : lfvPicture
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Picture button
*!*************************************************************
*!
FUNCTION lfvPicture

DO (gcScrDir+'MFLBLPIC.SPX')
SHOW GETS
*-- end of lfvPicture.

*!*************************************************************
*! Name      : lfvPreview
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Preview button
*!*************************************************************
*!
FUNCTION lfvPreview

SELECT (lcTmpFile)
COPY TO (gcRepHome+gcAct_Appl+'\'+lcTmpFile+'.LBX')

gcDevice   = 'SCREEN'
lcFormName = gcRepHome+gcAct_Appl+'\'+lcTmpFile
lcOGTmpForm = ''
lcRpSelect = 'E'
lcRpStatus = 'L'
lnRpPercnt = 100
lcRpSize   = 'A'
lcRpSort   = 'S'
lcRpExp    = '.T.'

DO (gcRepHome+'MFHNGCR') WITH .T.

ERASE (gcRepHome+gcAct_Appl+'\'+lcTmpFile+'.LBX')
ERASE (gcRepHome+gcAct_Appl+'\'+lcTmpFile+'.LBT')
*-- end of lfvPreview.

*!*************************************************************
*! Name      : lfvEdit
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Edit button in the custom label screen
*!*************************************************************
*!
FUNCTION lfvEdit

PRIVATE lcLblDimTl,lcLblName,lcDim,lnDimMsr,lcLblMsr,lcPbAddUpd
PRIVATE lnLeft,lnTop,lnHeight,lnWidth,lnBetween,lnNmbAcr

lnDimMsr  = IIF(laLblDimU[lnLblDimU,2]='E',1,2)
lcLblName = laLblDimU[lnLblDimU,03]
lcDim     = laLblDimU[lnLblDimU,04]
lnHeight  = laLblDimU[lnLblDimU,05]
lnLeft    = laLblDimU[lnLblDimU,06]
lnNmbAcr  = laLblDimU[lnLblDimU,07]
lnBetween = laLblDimU[lnLblDimU,08]
lnTop     = laLblDimU[lnLblDimU,09]
lnWidth   = laLblDimU[lnLblDimU,10]

lcLblDimTl = 'Edit Label Dimensions'
lcLblMsr   = 'Enter label measurements in ' + IIF(lnDimMsr=1,'inches','centimeters') + ':'
lcPbAddUpd = 'Update'

DO (gcScrDir+'MFLBLDIM.SPX')

=lfGetUsrDm()
IF lnLblDimU > ALEN(laLblDimU,1)
  lnLblDimU = ALEN(laLblDimU,1)
ENDIF
SHOW GET pbEdit   &lcEditStat.
SHOW GET pbDelete &lcEditStat.
SHOW GETS
*-- end of lfvEdit.

*!*************************************************************
*! Name      : lfvDelete
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Delete button in the custom label screen
*!*************************************************************
*!
FUNCTION lfvDelete

IF gfModalGen('QRM38260B00006','ALERT') = 1
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT SYLBLDIM
  LOCATE FOR CLBLNAME = laLblDimU[lnLblDimU,03]
  DELETE
  =lfGetUsrDm()
  IF lnLblDimU > ALEN(laLblDimU,1)
    lnLblDimU = ALEN(laLblDimU,1)
  ENDIF
  SHOW GET pbEdit   &lcEditStat.
  SHOW GET pbDelete &lcEditStat.
  SHOW GETS
  SELECT (lnAlias)
ENDIF
*-- end of lfvDelete.

*!*************************************************************
*! Name      : lfvNew
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of New button in the custom label screen
*!*************************************************************
*!
FUNCTION lfvNew

PRIVATE lcLblDimTl,lcLblName,lcDim,lnDimMsr,lcLblMsr,lcPbAddUpd
PRIVATE lnLeft,lnTop,lnHeight,lnWidth,lnBetween,lnNmbAcr
STORE 1 TO lnDimMsr,lnNmbAcr
STORE SPACE(0) TO lcLblName
STORE 0 TO lnLeft,lnTop,lnHeight,lnWidth,lnBetween
lcDim = '0.0000" x 0.0000"'
lcLblDimTl = 'New Label Dimensions'
lcLblMsr   = 'Enter label measurements in inches:'
lcPbAddUpd = 'Add'

DO (gcScrDir+'MFLBLDIM.SPX')

=lfGetUsrDm()
SHOW GET pbEdit   &lcEditStat.
SHOW GET pbDelete &lcEditStat.
SHOW GETS
*-- end of lfvNew.

*!*************************************************************
*! Name      : lfvAdd
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : Valid function of Add button in the New label definition screen
*!*************************************************************
*!
FUNCTION lfvAdd

IF EMPTY(lcLblName)
  =gfModalGen('TRM38256B00000','ALERT')
  _CUROBJ = OBJNUM(lcLblName)
  RETURN
ENDIF

IF EMPTY(lnHeight)
  =gfModalGen('TRM38257B00000','ALERT')
  _CUROBJ = OBJNUM(lnHeight)
  RETURN
ENDIF

IF EMPTY(lnWidth)
  =gfModalGen('TRM38258B00000','ALERT')
  _CUROBJ = OBJNUM(lnWidth)
  RETURN
ENDIF

SELECT SYLBLDIM
LOCATE FOR CLBLNAME = IIF(lcPbAddUpd#'Update',PADR(lcLblName,30),laLblDimU[lnLblDimU,03])
IF lcPbAddUpd # 'Update' .OR. !(PADR(lcLblName,30) == laLblDimU[lnLblDimU,03])
  IF FOUND()
    PRIVATE lnChoose
    lnChoose = 0
    IF lcPbAddUpd = 'Update'
      LOCATE FOR CLBLNAME = PADR(lcLblName,30)
      IF FOUND()
        lnChoose = gfModalGen('QRM38259B00006','ALERT')
        IF lnChoose = 2
          _CUROBJ = OBJNUM(lcLblName)
          RETURN
        ENDIF
      ELSE
        lnChoose = 1
      ENDIF
    ENDIF
    IF EMPTY(lnChoose)
      lnChoose = gfModalGen('QRM38259B00006','ALERT')
      IF lnChoose = 2
        _CUROBJ = OBJNUM(lcLblName)
        RETURN
      ENDIF
    ELSE
      DELETE
    ENDIF
    IF lcPbAddUpd = 'Update'
      LOCATE FOR CLBLNAME = laLblDimU[lnLblDimU,03]
    ENDIF
  ELSE
    IF lcPbAddUpd = 'Add'
      APPEND BLANK
    ENDIF
  ENDIF
ENDIF
REPLACE CLBLUOM    WITH IIF(lnDimMsr=1,'E','M'),;
        CLBLTYPE   WITH 'U',;
        CLBLNAME   WITH lcLblName,;
        CLBLDIM    WITH lcDim,;
        NLBLHEIGHT WITH lnHeight,;
        NLBLLFTMRG WITH lnLeft,;
        NLBLNMBACR WITH lnNmbAcr,;
        NLBLSPCBTW WITH lnBetween,;
        NLBLTOPMRG WITH lnTop,;
        NLBLWIDTH  WITH lnWidth
=gfAdd_Info('SYLBLDIM')
=lfvCancel()
*-- end of lfvAdd.

*!*************************************************************
*! Name      : lfConvert
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/28/2003
*! Purpose   : function to convert from english to metric systems
*!*************************************************************
*!
FUNCTION lfConvert

PRIVATE lcOper
lcOper    = IIF(lnDimMsr=1,'*','/')
lnLeft    = EVALUATE('ROUND(((lnLeft   '+lcOper+'39.3700787402E-2)),4)')
lnTop     = EVALUATE('ROUND(((lnTop    '+lcOper+'39.3700787402E-2)),4)')
lnHeight  = EVALUATE('ROUND(((lnHeight '+lcOper+'39.3700787402E-2)),4)')
lnWidth   = EVALUATE('ROUND(((lnWidth  '+lcOper+'39.3700787402E-2)),4)')
lnBetween = EVALUATE('ROUND(((lnBetween'+lcOper+'39.3700787402E-2)),4)')
lcLblMsr  = 'Enter label measurements in ' + IIF(lnDimMsr=1,'inches','centimeters') + ':'
=lfReDim()
*-- end of lfConvert.

*!*************************************************************
*! Name      : lfReDim
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/29/2003
*! Purpose   : function to Refresh the dimensions field
*!*************************************************************
*!
FUNCTION lfReDim

PRIVATE lcDimMsr
lcDimMsr = IIF(lnDimMsr=1,'"',' cm')
lcDim = ALLTRIM(STR(lnHeight,8,4)) + lcDimMsr + ' x ' + ALLTRIM(STR(lnWidth,8,4)) + lcDimMsr
SHOW GETS
*-- end of lfReDim.

*!*************************************************************
*! Name      : lfGetUsrDm
*! Developer : AMH (AHMED MAHER)
*! Date      : 05/29/2003
*! Purpose   : function to Refresh the user dimensions list
*!*************************************************************
*!
FUNCTION lfGetUsrDm

SELECT PADR(CLBLNAME,25) + ' ' + PADR(CLBLDIM,20) + ' ' + ALLTRIM(STR(NLBLNMBACR,2,0)) AS CLBLDESC,;
       CLBLUOM,CLBLNAME,CLBLDIM,NLBLHEIGHT,NLBLLFTMRG,NLBLNMBACR,NLBLSPCBTW,NLBLTOPMRG,NLBLWIDTH;
       FROM SYLBLDIM;
       WHERE CLBLTYPE = 'U';
       INTO ARRAY laLblDimU
IF _TALLY = 0
  laLblDimU[1,1] = ''
  lcEditStat = 'DISABLE'
ELSE
  lcEditStat = 'ENABLE'
ENDIF
*-- end of lfGetUsrDm.

*!*************************************************************
*! Name      : lfvAddPic
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/01/2003
*! Purpose   : Valid function of Add button in the picture screen
*!*************************************************************
*!
FUNCTION lfvAddPic

PRIVATE lnOrder
lnOrder = IIF(laLblLine[lnLblLine,1]=REPLICATE('.',60),1,OCCUR('|',laLblLine[lnLblLine,1])+2)
INSERT INTO (lcTmpLblOb) (NLINENO,NORDER,CTYPE,MEXPR,HEIGHT,WIDTH);
                  VALUES (lnLblLine,lnOrder,'P','"'+gcBmpHome+ALLTRIM(laLblPic[lnLblPic,2])+'.BMP"',1575,1575)
IF laLblLine[lnLblLine,1] = REPLICATE('.',60)
  laLblLine[lnLblLine,1] = laLblPic[lnLblPic,2]
ELSE
  laLblLine[lnLblLine,1] = laLblLine[lnLblLine,1] + '|' + laLblPic[lnLblPic,2]
ENDIF
laLblLine[lnLblLine,6] = MAX(laLblLine[lnLblLine,6],1575)
laLblLine[lnLblLine,7] = laLblLine[lnLblLine,7] + 1575
laLblLine[lnLblLine,8] = laLblLine[lnLblLine,8] + 1
=lfGetAlrm()
=lfvCancel()
*-- end of lfvAddPic.

*!*************************************************************
*! Name      : lfCreatLbl
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/01/2003
*! Purpose   : function to creat label
*!*************************************************************
*!
FUNCTION lfCreatLbl

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpFile)
ZAP
APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+gfTempName(),;
        TIMESTAMP  WITH  784434970,;
        OBJTYPE    WITH 1,;
        OBJCODE    WITH 53,;
        VPOS       WITH laLblDim[lnLblDim,5],;
        HPOS       WITH ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4) *;
                                  laLblDim[lnLblDim,4] * 10000,;
        HEIGHT     WITH ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4) *;
                                  laLblDim[lnLblDim,6] * 10000,;
        WIDTH      WITH IIF(laLblDim[lnLblDim,5]=1,-1,;
                        ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4) *;
                                  laLblDim[lnLblDim,8] * 10000),;
        ENVIRON    WITH .F.,;
        FONTFACE   WITH 'Courier New',;
        FONTSTYLE  WITH 0,;
        FONTSIZE   WITH 10,;
        RULER      WITH IIF(laLblDim[lnLblDim,2]='E',1,2),;
        RULERLINES WITH 1,;
        GRID       WITH .T.,;
        GRIDV      WITH 12,;
        GRIDH      WITH 12,;
        TOP        WITH .T.,;
        BOTTOM     WITH .T.,;
        ADDALIAS   WITH .T.,;
        CURPOS     WITH .T.
        
APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+gfTempName(),;
        OBJTYPE    WITH 9,;
        OBJCODE    WITH 1,;
        HEIGHT     WITH ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4) *;
                                  laLblDim[lnLblDim,7] * 10000,;
        WIDTH      WITH 0,;
        PLAIN      WITH .T.

APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+gfTempName(),;
        OBJTYPE    WITH 9,;
        OBJCODE    WITH 2,;
        HEIGHT     WITH 0,;
        WIDTH      WITH 0

APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+gfTempName(),;
        OBJTYPE    WITH 9,;
        OBJCODE    WITH 4,;
        HEIGHT     WITH ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4) *;
                                  laLblDim[lnLblDim,3] * 10000,;
        WIDTH      WITH 0

APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+gfTempName(),;
        OBJTYPE    WITH 9,;
        OBJCODE    WITH 6,;
        HEIGHT     WITH 0,;
        WIDTH      WITH 0

APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        UNIQUEID   WITH '_0'+gfTempName(),;
        OBJTYPE    WITH 9,;
        OBJCODE    WITH 7,;
        HEIGHT     WITH 0,;
        WIDTH      WITH 0

APPEND BLANK
REPLACE PLATFORM   WITH 'WINDOWS',;
        OBJTYPE    WITH 23,;
        VPOS       WITH 16,;
        HPOS       WITH 8,;
        HEIGHT     WITH 12,;
        WIDTH      WITH 9,;
        PENRED     WITH 4,;
        PENGREEN   WITH 0,;
        FONTFACE   WITH 'Courier New',;
        FONTSTYLE  WITH 0,;
        FONTSIZE   WITH 10

=lfGetPos()

SELECT (lcTmpLblOb)
PRIVATE lnObjType,lcExpr,lnVPos,lnHPos,lnHeight,lnWidth,lnLineNo,lnFontSty,lcFillChar
SCAN FOR CTYPE # 'S'
  lcExpr   = MEXPR
  lnVPos   = VPOS
  lnHPos   = HPOS
  lnHeight = HEIGHT
  lnWidth  = WIDTH
  lnLineNo = NLINENO
  DO CASE
    CASE CTYPE = 'F'
      lnObjType  = 8
      lcFillChar = 'C'
    CASE CTYPE = 'T'
      lnObjType  = 5
      lcFillChar = ''
    CASE CTYPE = 'P'
      lnObjType  = 17
      lcFillChar = ''
  ENDCASE
  DO CASE
    CASE laLblLine[lnLineNo,4] == 'N'
      lnFontSty = 0
    CASE laLblLine[lnLineNo,4] == 'B'
      lnFontSty = 1
    CASE laLblLine[lnLineNo,4] == 'I'
      lnFontSty = 2
    CASE laLblLine[lnLineNo,4] == 'BI'
      lnFontSty = 3
  ENDCASE
  SELECT (lcTmpFile)
  APPEND BLANK
  REPLACE PLATFORM   WITH 'WINDOWS',;
          UNIQUEID   WITH '_0'+gfTempName(),;
          OBJTYPE    WITH lnObjType,;
          OBJCODE    WITH 0,;
          VPOS       WITH lnVPos,;
          HPOS       WITH lnHPos,;
          HEIGHT     WITH lnHeight,;
          WIDTH      WITH lnWidth,;
          FILLCHAR   WITH lcFillChar,;
          MODE       WITH 1,;
          TOP        WITH .T.,;
          SUPALWAYS  WITH .T.,;
          SUPRPCOL   WITH 3,;
          SUPGROUP   WITH 0
  
  IF EVALUATE(lcTmpLblOb+'.CTYPE') = 'P'
    REPLACE PICTURE    WITH lcExpr,;
            GENERAL    WITH 2
  ELSE
    REPLACE EXPR       WITH lcExpr,;
            PENRED     WITH 0,;
            PENGREEN   WITH 0,;
            PENBLUE    WITH 0,;
            FILLRED    WITH 255,;
            FILLGREEN  WITH 255,;
            FILLBLUE   WITH 255,;
            FONTFACE   WITH laLblLine[lnLineNo,2],;
            FONTSTYLE  WITH lnFontSty,;
            FONTSIZE   WITH laLblLine[lnLineNo,3],;
            SPACING    WITH IIF(lnFontSty>1,2,0)
  ENDIF
  
  IF EVALUATE(lcTmpLblOb+'.CTYPE') = 'F'
    REPLACE OFFSET     WITH IIF(laLblLine[lnLineNo,8]=1,2,0),;
            TOTALTYPE  WITH 0,;
            RESETTOTAL WITH 1
  ENDIF
  IF EVALUATE(lcTmpLblOb+'.CTYPE') = 'T'
    REPLACE PICTURE    WITH '"@I"'
  ENDIF
ENDSCAN
SELECT (lnAlias)
*-- end of lfCreatLbl.

*!*************************************************************
*! Name      : lfGetPos
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/02/2003
*! Purpose   : function to get the object positions
*!*************************************************************
*!
FUNCTION lfGetPos

PRIVATE lnAlias,lnI,lnTotHeght,lnVPos,lnHPos,lnObjWidth,lnObjTop
lnAlias = SELECT(0)
SELECT (lcTmpLblOb)

lnTotHeght = 0
FOR lnI = 1 TO ALEN(laLblLine,1)
  lnTotHeght = lnTotHeght + laLblLine[lnI,6]
ENDFOR

lnObjTop = ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4) *;
                     laLblDim[lnLblDim,7] * 10000
lnVPos = 0
FOR lnI = 1 TO ALEN(laLblLine,1)
  IF lnI = 1
    lnVPos = MAX(((ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4)*;
                   laLblDim[lnLblDim,3]*10000)-lnTotHeght)/2,0)
  ELSE
    lnVPos = lnVPos + laLblLine[lnI-1,6]
  ENDIF
  
  IF SEEK(STR(lnI,2,0))
    STORE 0 TO lnHPos,lnObjWidth
    SCAN REST WHILE STR(NLINENO,2,0)+STR(NORDER,2,0) = STR(lnI,2,0)
      IF NORDER = 1
        lnHPos = MAX(((ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4)*;
                       laLblDim[lnLblDim,8]*10000)-laLblLine[lnI,7])/2,0)
      ELSE
        lnHPos = lnHPos + lnObjWidth
      ENDIF
      lnObjWidth = WIDTH
      REPLACE VPOS WITH 4166.667 + lnObjTop + lnVPos,;
              HPOS WITH lnHPos
    ENDSCAN
  ENDIF
ENDFOR
SELECT (lnAlias)
*-- end of lfGetPos.

*!*************************************************************
*! Name      : lfvSave
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/03/2003
*! Purpose   : Valid function of the save botton in the save screen
*!*************************************************************
*!
FUNCTION lfvSave

IF EMPTY(lcLblSave)
  =gfModalGen('TRM38261B00000','ALERT')
  _CUROBJ = OBJNUM(lcLblSave)
  RETURN
ENDIF

IF FILE(gcRepHome+gcAct_Appl+'\'+gcAct_Appl+ALLTRIM(lcLblSave)+'.LBX')
  IF gfModalGen('QRM38262B00006','ALERT') = 2
    _CUROBJ = OBJNUM(lcLblSave)
    RETURN
  ENDIF
ENDIF

SELECT (lcTmpFile)
APPEND BLANK
REPLACE PLATFORM   WITH 'WIZARD  ',;
        TIMESTAMP  WITH 0,;
        OBJTYPE    WITH 0,;
        OBJCODE    WITH 0,;
        VPOS       WITH 0,;
        HPOS       WITH 0,;
        HEIGHT     WITH 0,;
        WIDTH      WITH 0,;
        COMMENT    WITH lcLblDesc,;
        ENVIRON    WITH .F.

COPY TO (gcRepHome+gcAct_Appl+'\'+gcAct_Appl+ALLTRIM(lcLblSave)+'.LBX')
llExit = .T.
=lfvCancel()
*-- end of lfvSave.

*!*************************************************************
*! Name      : lfGetDim
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/12/2003
*! Purpose   : Get the label pictures
*!*************************************************************
*!
FUNCTION lfGetPic

SELECT CPICDESC,CFILENAME;
       FROM SYLBLPIC;
       INTO ARRAY laLblPic
lnLblPic = 1
*-- end of lfGetPic.

*!*************************************************************
*! Name      : lfGetAlrm
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/16/2003
*! Purpose   : Get the Alrm text
*!*************************************************************
*!
FUNCTION lfGetAlrm

lnTotHeght = 0
FOR lnI = 1 TO ALEN(laLblLine,1)
  lnTotHeght = lnTotHeght + laLblLine[lnI,6]
ENDFOR

lnVPos = (ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4)*laLblDim[lnLblDim,3]*10000);
         -lnTotHeght
lnHPos = (ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4)*laLblDim[lnLblDim,8]*10000);
         -laLblLine[lnLblLine,7]
lcLineAlrm = IIF(lnHPos<0,'Over Width','')
lcHghtAlrm = IIF(lnVPos<0,'Over Height','')
lcLineFont = laLblLine[lnLblLine,2]+STR(laLblLine[lnLblLine,3],3,0)+' '+laLblLine[lnLblLine,5]
=lfRefresh()
*-- end of lfGetAlrm.

*!*************************************************************
*! Name      : lfChkAlrm
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/16/2003
*! Purpose   : Check the Alrm text
*!*************************************************************
*!
FUNCTION lfChkAlrm

STORE 0 TO lnTotHeght,lnHPos

FOR lnI = 1 TO ALEN(laLblLine,1)
  lnTotHeght = lnTotHeght + laLblLine[lnI,6]
  lnHPos = MIN((ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4)*laLblDim[lnLblDim,8];
                *10000)-laLblLine[lnI,7],lnHPos)
ENDFOR

lnVPos = (ROUND(IIF(laLblDim[lnLblDim,2]='E',1,39.3700787402E-2),4)*laLblDim[lnLblDim,3]*10000);
         -lnTotHeght
RETURN lnHPos<0 .OR. lnVPos<0
*-- end of lfGetAlrm.