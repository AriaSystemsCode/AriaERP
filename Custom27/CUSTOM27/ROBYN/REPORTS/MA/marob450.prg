**************************************************************************
*: Program file  : MAROB450.PRG (C# 101334)
*: Program desc. : Compare color codes form Code table with Style / color
*:                 and Fabric / color codes and display either Unallocated 
*:                 color codes or all Style / color and Fabric / color codes 
*:                 according to the choice of the user.  
*:                 Convert ROB4500.PRG from 2.6 to 2.7
*:         Module: Aria Apparel Series (MA).
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls :PROCEDURES :
*:          lpHeader,lpPrRep,lpAllCol3,lpHeader1.
*:        FUNCTIONS  :
*:          lfCopyToTemp.
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*B605416,1 WAB 01/24/2002 remove the field Ccomp_id while clocting date from codes file
*:************************************************************************
lnPage  = 1
lnPage1 = 1
lnRow   = 5
lnRow1  = 9
lcColor = SPACE(7)
lcList  = lcRpUnAll
lcLname = ''
DIMENSION laLname[1,2]
laLname[1,1] = "CLRLNAME"
laLname[1,2] = "lcLname"
DO CASE
  CASE lcList = "U"
    lcUnAsgnClr  = gfTempName()
    CREATE CURSOR (lcUnAsgnClr) (Color C(07) , Desc C(15),ClrLname C(30))

    *B605416,1 WAB (Start) - there is no field name cComp_id
    *IF SEEK (gcAct_Comp+'COLOR','CODES')
    *SELECT CODES
    *SCAN REST WHILE ccomp_id+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
    *           gcAct_Comp + 'COLOR'  FOR CrltField = 'N'
    IF SEEK ('N'+'COLOR','CODES')
      SELECT CODES
      SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
                 'N' + 'COLOR'  FOR CrltField = 'N'                 
    *B605416,1 WAB (End)
        lcColor = ccode_no
        lcDesc  = SUBSTR(CDiscRep,1,15)
        IF !SEEK(lcColor,'Style') .AND. !SEEK(lcColor,'Fabric')
          = lfCopyToTemp()
        ENDIF
      ENDSCAN
      SET DEVICE TO PRINTER
      DO lpHeader
      DO lpPrRep
      DO ENDREPORT
      SET DEVICE TO SCREEN
    ELSE
      WAIT WINDOW 'No color codes'
    ENDIF      
  CASE lcList = "A"
    lcStyleClr  = gfTempName()
    lcFabClr = gfTempName()
    WAIT WINDOW 'COLLECTING DATA...'  NOWAIT

    SELECT CODES.ccode_no, SUBSTR(Style.Style,1,12) AS cStyle,Style.Desc ;
    FROM CODES , Style ;
     WHERE CODES.cFld_Name = "COLOR     "  AND;
           CODES.CrltField = 'N'           AND;
           SUBSTR(Style.STYLE,14,6) = CODES.ccode_no ;
     ORDER BY ccode_no , STYLE INTO CURSOR &lcStyleClr

    SELECT CODES.ccode_no , Fabric.Fabric,Fabric.Desc;
     FROM CodeS, Fabric ;
      WHERE CODES.cFld_Name = "COLOR     "  AND;
            CrltField = 'N'                 AND;
            Fabric.Color = CODES.ccode_no;
      ORDER BY ccode_no , FABRIC INTO CURSOR &lcFabClr
    WAIT CLEAR
    DO lpAllCol3
ENDCASE  

***************************************************************************
*....+....1....+....2....+....3....+....4.....+....5....+....6....+....7....+....
*1-- Prog.Name                 Company Name                        Date                   CUR_TIME        
*2-- Time                      Report Name                         Page#
*3--          
*4--                   
*5--  Color Code      Color Description       Color Long Name
*6--  ----------      -----------------       -----------------
*7--  лллллллллл      лллллллллллллллл        ллллллллллллллл
*8--------------------------------------------------------------------------------
*********************************************************************************

*!*************************************************************
*! Name      : lpHeader
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 01/02/1999
*! Purpose   : Setting the header of the unallocated color report 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpHeader
*!*************************************************************
PROCEDURE lpHeader  

@ 01,01 SAY "MAROB450"
@ 01,30 SAY qCompany
@ 01,72 SAY gdSysDate
@ 02,01 SAY TIME()
@ 02,30 SAY "DEFINING UNALLOCATED COLORS"
@ 02,72 SAY "Page#"
@ 02,78 SAY lnPage PICTURE '99'
@ 03,01 SAY "--------------------------------------------------------------------------------"
@ 04,02 SAY "Color Code"
@ 04,16 SAY "Color Description"
@ 04,40 SAY "Color Long Name"
@ 05,01 SAY "--------------------------------------------------------------------------------"

*!*************************************************************
*! Name      : lpPrRep
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 01/02/1999
*! Purpose   : Printing unallocated color codes , descriptions and long names
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrRep
*!*************************************************************
PROCEDURE lpPrRep

lnRow = 6 
SELECT &lcUnAsgnClr
GO TOP
SCAN WHILE !EOF() 
  @ lnRow,02 SAY Color 
  @ lnRow,16 SAY Desc
  @ lnRow,40 SAY ClrLname
  lnRow = lnRow +1
  IF lnRow < 64
    LOOP
  ELSE
    lnPage = lnPage +1
    DO lpHeader
    lnRow = 6
    LOOP
  ENDIF          
ENDSCAN  

*!*************************************************************
*! Name      : lfCopyToTemp
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 01/02/1999
*! Purpose   : Saving unallocated colors into the temproary file &lcUnAsgnClr
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCopyToTemp
*!*************************************************************
FUNCTION lfCopyToTemp

= gfRltFld(lcColor, @laLname, "COLOR")
INSERT INTO (lcUnAsgnClr) (Color,Desc,ClrLname) VALUES (lcColor,lcDesc,lcLname)

*!*************************************************************
*! Name      : lpAllCol3
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 01/02/1999
*! Purpose   : Printing each color code and its all related Style and Fabric color
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpAllCol3
*!*************************************************************
PROCEDURE lpAllCol3

SET DEVICE TO PRINTER
DO lpHeader1
GOTO TOP IN &lcStyleClr
GOTO TOP IN &lcFabClr
SELECT Codes

*B605416,1 WAB (Start) - there is no field name cComp_id
*=SEEK(gcAct_Comp + 'COLOR')
*lnRow = 7
*SCAN REST WHILE ccomp_id+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
*           gcAct_Comp + 'COLOR'  FOR CrltField = 'N'
=SEEK('N' + 'COLOR')
lnRow = 7
SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
            'N' + 'COLOR'  FOR CrltField = 'N'                 
*B605416,1 WAB (END) 
  lcColor = ccode_no
  lcDesc  = SUBSTR(CDiscRep,1,15)
  = gfRltFld(lcColor, @laLname, "COLOR")
  lcClrLname = lcLname
  lcToPrintCol = lcColor + SPACE(11) +  lcDesc + SPACE(15) + lcClrLname
  IF lnRow =< 62
    @ lnRow,02 SAY lcToPrintCol
  ELSE
    lnRow = 7
    lnPage1 = lnPage1 +1
    DO lpHeader1
    @ lnRow,02 SAY lcToPrintCol
  ENDIF          
  lnRow=lnRow+1
  DO WHILE .T.
    WAIT WINDOW lcColor+'/' + &lcStyleClr..cStyle NOWAIT
    lcToPrintCol = lcColor
    SELECT &lcStyleClr
    IF &lcStyleClr..ccode_no = lcColor
      lcToPrint = &lcStyleClr..cStyle + SPACE(4) + &lcStyleClr..Desc
      llStyDone = .F.
      SKIP IN &lcStyleClr
    ELSE
      lcToPrint =  SPACE(36)
      llStyDone = .T.  
    ENDIF
    SELECT &lcFabClr
    WAIT WINDOW ALLTRIM(lcColor)+'/' + &lcFabClr..Fabric NOWAIT
    IF &lcFabClr..ccode_no = lcColor
      lcToPrint = lcToPrint + SPACE(10) + &lcFabClr..Fabric + SPACE(4) + &lcFabClr..Desc
      llFabDone = .F.
      SKIP IN &lcFabClr
    ELSE
      llFabDone = .T.
    ENDIF
    IF llStyDone AND llFabDone
      EXIT
    ELSE 
      lnRow = lnRow + 1
      IF lnRow =< 62
        @ lnRow,02 SAY lcToPrint
      ELSE
        lnRow = 7
        lnPage1 = lnPage1 +1
        DO lpHeader1
        @ lnRow,02 SAY lcToPrint
      ENDIF          
    ENDIF
  ENDDO
  lnRow=lnRow+1
  @ lnRow,00 SAY "--------------------------------------------------------------------------------"  
  lnRow=lnRow+1
ENDSCAN      
WAIT CLEAR
DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpHeader1           CUST#(101334)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 01/02/1999
*! Purpose   : Setting the header of the all Style and Fabric color codes report
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :  DO lpHeader1
*!*************************************************************
PROCEDURE lpHeader1
@ 01,01 SAY "ROB4500"
@ 01,27 SAY qCompany
@ 01,72 SAY gdSysDate
@ 02,01 SAY TIME()
@ 02,27 SAY "ALL STYLE - MATERIAL COLORS"
@ 02,72 SAY "Page#" 
@ 02,78 SAY lnPage1 PICTURE '99'
@ 03,01 SAY "--------------------------------------------------------------------------------"
@ 04,02 SAY "Color Code"
@ 04,19 SAY "Color Description"
@ 04,49 SAY "Color Long Name"

@ 05,02 SAY "Style"
@ 05,18 SAY "Style Description"
@ 05,48 SAY "Material"
@ 05,59 SAY "Material Description"
@ 06,01 SAY "--------------------------------------------------------------------------------"

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 01/02/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH    = 'W'