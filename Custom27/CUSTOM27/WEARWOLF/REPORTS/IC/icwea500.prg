*:***************************************************************************
*: Program file  : ICWEA500
*: Program desc. : CUSTOM STYLE MASTER LIST PROGRAM FOR (WEA)
*: Date          : 07/04/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls :  
*:         Procedures : lpRptHdr
*:                    : lpPrint
*:
*:          Functions : lfNonMaj()
*:                    : gfModalGen()
*:                    : gfItemMask()
*:                    : gfStyBrw()
*:                    : FaBrow()
*:                    : lfvStyle()
*:                    : lfvFabric()
*:                    : lfSRVSty()
*:                    : lfStySum()
*:                    : lfwRepWhen()
*:                    : lfItmPos() 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO ICWEA500
*:***************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR WEA (101547) 
*:***************************************************************************
*:C101547 
*:***************************************************************************
*:Modifications :
*:B803938,1 ABD 01/22/2001 Fix bug that the reports print wrong data and wromg
*:                         Scale and wrong description.
*:B604252,1 WAB 02/21/2001 Change the way of divising to groups from based on the scale description
*:B604252,1 WAB            to be based on cDim1 ( dimension one )
*:B605273,1 BWA 12/24/2001 Fix the bug of variable "lcStrGrp4" not found 
*:B605273,1 BWA            and use variable "lnCountGrp" to redimension the arrays hold the data.
*:***************************************************************************

*-- llExtSize is not used in this PRG , because this custom   
*-- program is already designed to use Extended size scale.  
lcRpExp = STRTRAN(lcRpExp,"STYLE.","")

*-- Ask about Print Zero available this select all OPEN TO SELL which is greater than 0
IF !llRpPrint0
  lcRpOTSExp = '(TOTSTK+TOTWIP-TOTORD) > 0'    
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcRpOTSExp
ENDIF

*-- select the records that match criteria from STYLE File to be printed out
SELECT STYLE
SET FILTER TO &lcRpExp
GO TOP
IF EOF()             && if end of file (no records match criteria)
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF

R_WIDTH = 'XW'       && reports is Landscape EXTRA WIDE (XW)
R_TITLE = 'Available to Sell Report'
SET DEVICE TO PRINT

*B605273,1 BWA 12/24/2001 Fix the bug of variable "lcStrGrp4" not found 
*B605273,1 BWA            and use variable "lnCountGrp" to redimension the arrays hold the data.[START]
PRIVATE lcAliasScl , lcScalLtr , lcKeyScl
lcAliasScl = SELECT(0)
STORE SPACE(0) TO lcScalLtr
STORE 0 TO lnCountScl , lnCountGrp

SELECT SCALE
lcKeyScl = EVAL(KEY())
LOCATE
SCAN WHILE TYPE + SCALE + PREPAK = "S"
  IF lcScalLtr # LEFT(SCALE.SCALE,1)
    IF lnCountScl > lnCountGrp
      lnCountGrp = lnCountScl
    ENDIF
    lnCountScl = 0
    lnCountScl = lnCountScl + 1
  ELSE
     lnCountScl = lnCountScl + 1
  ENDIF
  lcScalLtr = LEFT(SCALE.SCALE,1)
ENDSCAN

IF lnCountScl > lnCountGrp
  lnCountGrp = lnCountScl
ENDIF

=SEEK(lcKeyScl)
SELECT(lcAliasScl)
*B605273,1 BWA 12/24/2001.[END]

*-- initialize variables used in report [Begin.]

*B605273,1 BWA 12/24/2001 Fix the bug of variable "lcStrGrp4" not found 
*B605273,1 BWA            and use variable "lnCountGrp" to redimension the arrays hold the data.[START]
*STORE SPACE(1) TO lcStrGrp1,lcStrGrp2,lcStrGrp3,SaveStyle,SaveColor,lcScaleDes,;
                  lcOldStyle,lcOldScal1,lcOldScal2,lcOldScal3,lcOldColor
*STORE  0  TO lnGroups,lnGrpDscLn
*STORE  1  TO lnGroup1,lnGroup2,lnGroup3,lcOldNo

STORE SPACE(1) TO SaveStyle,SaveColor,lcScaleDes,;
                  lcOldStyle,lcOldScal1,lcOldScal2,lcOldScal3,lcOldColor
STORE  0  TO lnGroups,lnGrpDscLn
STORE  1  TO lcOldNo

FOR lnGrp = 1 TO lnCountGrp
  lcGrp = ALLTRIM(STR(lnGrp))
  STORE SPACE(1) TO lcStrGrp&lcGrp
  STORE  1  TO lnGroup&lcGrp
ENDFOR
*B605273,1 BWA 12/24/2001.[END]

PAGENO = 1
*-- initialize variables used in report [End.]

DO Rpt_Hdr WITH 'ICWEA500',"",R_WIDTH
PAGENO = PAGENO+1                              && count the page no to be printed in page header
ROW    = 5

*B605273,1 BWA 12/24/2001 Fix the bug of variable "lcStrGrp4" not found 
*B605273,1 BWA            and use variable "lnCountGrp" to redimension the arrays hold the data.[START]
*DIMENSION laScaleVal[3,11]                     && array to hold the 3 Size Scales Values with 11 sizes maximum     
*DIMENSION laScalDesc[3]                        && array to hold the desc of each scale size
DIMENSION laScaleVal[lnCountGrp,11]                     && array to hold the 3 Size Scales Values with 11 sizes maximum     
DIMENSION laScalDesc[lnCountGrp]                        && array to hold the desc of each scale size
*B605273,1 BWA 12/24/2001.[END]

*B604252,1 WAB (START) - initial variables one for holding cdim1 foreach group and old dimention value
STORE SPACE(1) TO lcScalDim,lcOldDim1,lcOldDim2,lcOldDim3

*B605273,1 BWA 12/24/2001 Fix the bug of variable "lcStrGrp4" not found 
*B605273,1 BWA            and use variable "lnCountGrp" to redimension the arrays hold the data.[START]
*DIMENSION laScalDim[3]
DIMENSION laScalDim[lnCountGrp]
*B605273,1 BWA 12/24/2001.[END]

*B604252,1 WAB (End) 


*-- Main Do..While Loop (collecting Data) [Begin.]
DO WHILE !EOF() AND INKEY() <> 32
  WAIT WINDOW 'Printing ' + ALLTRIM(lcMajTtl) + ' ' + ALLTRIM(CSTYMAJOR) + ' <Space Bar> to abort' NOWAIT
  lnScNo    = 0                                 && to count the number of sizes in one scale size
  SaveStyle = SUBSTR(STYLE,1,lnMajLen)          && var to hold current style
  SaveColor = SUBSTR(STYLE,lnClrPo,lnColorLen)  && var to hold current color 
  
  IF SEEK('S'+SCALE,'SCALE')    && seek for SCALE in Scale file
    *-- this prog is using the scales in 3 groups e.g. 11 sizes or less but not more
    *-- in SMALL size group and so on , so lnGroups is used to represent the group no
    IF lnGroups <> 0           && If Scale number greater than zero 
      *B803938,1 ABD Save all description in the array. [Begin]
      *lcSc_Desc = SUBSTR(SCALE.CSCL_DESC,1,LEN(ALLTRIM(SCALE.CSCL_DESC))-1)
      *lnGrpDscLn  = ASCAN(laScalDesc,SUBSTR(lcSc_Desc,1,3))
      lcSc_Desc  = SCALE.CSCL_DESC
      *B604252,1 WAB (START) - store dimention value to array
      *lnGrpDscLn = ASCAN(laScalDesc,lcSc_Desc)
      lcScalDim = SCALE.cDim1
      lnGrpDscLn = ASCAN(laScalDim,lcScalDim)
      *B604252,1 WAB (End)
      *B803938,1 ABD [End]
      
    ENDIF
    IF lnGrpDscLn<>0              && If Desc number greater than zero (this means that the current dimension is the same as the one stored in the array)
      D = ALLTRIM(STR(lnGrpDscLn))
      IF lnGroup&D<11         && if Scale number (Group number e.g. = 2) contains number of sizes less than 11
        lnScNo = lnGroup&D + SCALE.CNT       && Add the remaining number of sizes in this Group 
        *B803938,1 ABD Add new variables. [Begin]
        *FOR I  = lnGroup&D + 1 TO IIF(lnScNo>11,11,lnScNo)
        *  K = ALLTRIM(STR(I-lnGroup&D))
        *  lcStrGrp&D = lcStrGrp&D+PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') +' '
        llFirst    = .F.
        lnNegscale = -1 
        FOR I  = lnGroup&D + 1 TO lnScNo  && scan for the secound dimention if > 11. 
          K = IIF (lnScNo > 11,ALLTRIM(STR(I - Scale.Cnt)),ALLTRIM(STR(I-lnGroup&D)))
          IF I > 11
            lnNegscale =  lnNegscale + 1
          ENDIF
          IF I = 12  && if scale more than 11 scale add the new scale to the new array.
            D = STR(EVAL(D)+1,1)          
            lnGrpDscLn = lnGrpDscLn + 1
            llFirst = .T.
          ENDIF
          lcStrGrp&D = lcStrGrp&D+PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') +' '
          IF I <= 11
            laScaleVal[lnGrpDscLn,I]=IIF((STK&K+WIP&K-ORD&K)<0,0,(STK&K+WIP&K-ORD&K))
          ELSE
            lnNewI = I - 11
            laScaleVal[lnGrpDscLn,lnNewI]=IIF((STK&K+WIP&K-ORD&K)<0,0,(STK&K+WIP&K-ORD&K))
          ENDIF  
         *B803938,1 ABD [End]        
        ENDFOR
        
        *B803938,1 ABD Add the descr. of the scale to the array if scale more than 11. [Begin]
        *lnGroup&D = lnGroup&D + SCALE.CNT
        *lnGroup&D = IIF(lnGroup&D>11,11,lnGroup&D) 
        IF I > 12
          laScalDesc[lnGroups+1] = SCALE.CSCL_DESC        
          *B604252,1 WAB (START) - store cdim1 to the array
          laScalDim[lnGroups+1] = SCALE.cDim1
          *B604252,1 WAB (END)
          lnGroups  = lnGroups + 1
          lclastGrp = STR(EVAL(D)-1,1) 
          lnGroup&lclastGrp = 11
        ENDIF  
       lnGroup&D = lnGroup&D + IIF(llFirst , lnNegscale , SCALE.CNT)
       *B803938,1 ABD [End]
        
      ENDIF
    ELSE                        && Else Desc number is not greater than zero
      lnGroups = lnGroups + 1 && var to hold number of Size Scales whether the 1st 11th scales or 2nd  11th scales ...etc
      IF lnGroups > 3
        DO lpPrint
        LOOP
      ENDIF      
      laScalDesc[lnGroups] = SCALE.CSCL_DESC    && store the Scale Desc. in an array of 3 
      *B604252,1 WAB (START) - store the cdim1 to the array
      laScalDim[lnGroups] = SCALE.cDim1
      *B604252,1 WAB (END)
      lcNo = ALLTRIM(STR(lnGroups))
      lnGroup&lcNo = SCALE.CNT
      FOR I = 1 TO SCALE.CNT      && For..Next to add the Scales of any group into one string [Begin.]

        K = ALLTRIM(STR(I))
        lcStrGrp&lcNo = lcStrGrp&lcNo + PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ')+' '
        *B803938,1 ABD [Begin]        
        laScaleVal[lnGroups,I] = IIF((STK&K+WIP&K-ORD&K)<0,0,(STK&K+WIP&K-ORD&K))
        *B803938,1 ABD [End]
        
      ENDFOR                      && For..Next to add the Scales of any group into one string [End.]
    ENDIF                     && EndIf of Desc number 
  ELSE                        && else SCALE not found in Scale file
    IF !EOF()                 && If also not end of file
      SKIP                    && then Skip one record 
    ENDIF                    
    LOOP                      
  ENDIF                    && endif SEEK ('S'+Scale,'Scale') 
  
  *-- this If..Endif is responsible for Storing values of each Scale Size in an array [Begin.]
  *B803938,1 ABD Remark the next lines . [Begin]
  *IF (SUBSTR(STYLE,1,lnMajLen) = SaveStyle AND SUBSTR(STYLE,lnClrPo,lnColorLen) = SaveColor AND !EOF()) OR (EMPTY(SaveStyle) AND EMPTY(SaveColor) AND !EOF())
  *  IF lnGrpDscLn<>0
  *    D = ALLTRIM(STR(lnGrpDscLn))
  *    FOR I = lnGroup&D - SCALE.CNT + 1 TO lnGroup&D
  *      L=ALLTRIM(STR(I - (lnGroup&D - SCALE.CNT)))
  *      laScaleVal[lnGrpDscLn,I]=IIF((STK&L+WIP&L-ORD&L)<0,0,(STK&L+WIP&L-ORD&L))
  *    ENDFOR
  *  ELSE
  *    lcScaleDes = laScalDesc[lnGroups]
  *    FOR I = 1 TO SCALE.CNT
  *      X = ALLTRIM(STR(I))
  *      laScaleVal[lnGroups,I] = IIF((STK&X+WIP&X-ORD&X)<0,0,(STK&X+WIP&X-ORD&X))
  *    ENDFOR
  *  ENDIF
  *ENDIF
  *B803938,1 ABD [End]  
  *-- this If..Endif is responsible for Storing values of each Scale Size in an array [End.]
  
  IF !EOF()                  && if not EOF move one record
    SKIP
  ENDIF

  *-- this If..Endif checks for Style or Color if any is changed [Begin.]		
  IF EOF() OR ((SUBSTR(STYLE,1,lnMajLen) <> SaveStyle .OR. SUBSTR(STYLE,lnClrPo,lnColorLen) <> SaveColor) AND !EMPTY(SaveStyle) AND !EMPTY(SaveColor))
    DO lpPrint               && do the print session for this current style
  ENDIF
  *-- this If..Endif checks for Style or Color if any is changed [End.]	
ENDDO
*-- Main Do..While Loop (collecting Data) [End.]

DO ENDREPORT                 && print the report by ENDREPORT PRG
SET DEVICE TO SCREEN         && return the device again to screen
RETURN                        
*-- End of Report.

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh (SSE)
*! Date      : 07/04/1999
*! Purpose   : Print the data after being collected.
*!**************************************************************************
*! Called from : ICWEA500.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : DO lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint
IF ROW > 38              && if page reached max rows (at row = 38)
  ROW = 1                && start new page at row 1     
  DO Rpt_Hdr WITH 'ICWEA500',"",R_WIDTH
  
  PAGENO = PAGENO + 1    && increase page number with 1
  ROW    = 5             && start print details at row = 5
ENDIF
*-- (lnGrpTot1,lnGrpTot2,lnGrpTot3) for Total of each Scale Group
*-- (lnColorTot) for total of all Groups 
STORE 0 TO lnGrpTot1 , lnGrpTot2 , lnGrpTot3 , lnColorTot

*-- for..Endfor to sum the total of the 1st Scale Group (11 Sizes or less)
FOR I = 1 TO lnGroup1
  lnGrpTot1 = lnGrpTot1 + laScaleVal[1,I]
ENDFOR
lnColorTot = lnColorTot + lnGrpTot1      

IF lnGroups > 1      && If Scales Group greater than 1 .. get sum of 2nd Scale Group
  *-- for..Endfor to sum the total of the 2nd Scale Group (11 Sizes or less)
  FOR I = 1 TO lnGroup2
    lnGrpTot2 = lnGrpTot2 + laScaleVal[2,I]
  ENDFOR
  lnColorTot = lnColorTot + lnGrpTot2
ENDIF                && Endif Scales Group greater than 1

IF lnGroups > 2      && If Scales Group greater than 2 .. get sum of 3rd Scale Group
  *-- for..Endfor to sum the total of the 3rd Scale Group (11 Sizes or less)
  FOR I = 1 TO lnGroup3
    lnGrpTot3 = lnGrpTot3 + laScaleVal[3,I]
  ENDFOR
  lnColorTot = lnColorTot + lnGrpTot3
ENDIF                && Endif Scales Group greater than 2

llChange  = .F.        && Flag to detect that STYLE has changed
llScaleCh = .F.        && Flag to detect that Scale Group has changed

*-- to print the SCALE Description & the SCALE SIZES of each Group [Begin.]
IF SaveStyle = lcOldStyle AND SaveColor = lcOldColor 
  FOR I = 1 TO MIN(lnGroups,lcOldNo)
    J = ALLTRIM(STR(I))
    *B604252,1 WAB (START) - using the lascalDim instead of laScalDesc
    *IF SUBSTR(laScalDesc[I],1,3) <> lcOldScal&J
    IF laScalDim[I] <> lcOldDim&J
    *B604252,1 WAB (End) 
      llScaleCh = .T.
    ENDIF
  ENDFOR
  IF llScaleCh        && If Scale Group is changed   
    Row = IIF(Row = 5,Row,Row - 1)
    @ ROW,026 SAY laScalDesc[1]
    IF lnGroups > 1   && if SCALE Groups is greater than 1
      @ ROW,078 SAY laScalDesc[2] 
    ENDIF             && Endif SCALE Groups is greater than 1
    IF lnGroups > 2   && if SCALE Groups is greater than 2
      @ ROW,131 SAY laScalDesc[3]
    ENDIF             && Endif SCALE Groups is greater than 2
    ROW = ROW + 1
    FOR I = 1 TO MIN(lnGroups,3)
      J = ALLTRIM(STR(I))
      lcOldNo = MIN(lnGroups,3)
      *B604252,1 WAB (START) - using the lascalDim instead of laScalDesc
      *lcOldScal&J = SUBSTR(laScalDesc[I],1,3)
      lcOldDim&J = laScalDim[I]
      *B604252,1 WAB (End) 
    ENDFOR
    @ ROW,025 SAY lcStrGrp1
    @ ROW,077 SAY lcStrGrp2
    @ ROW,130 SAY lcStrGrp3      
    ROW = ROW + 2
  ENDIF               && Endif Scale Group is changed
ENDIF  
*-- to print the SCALE Description & the SCALE SIZES of each Group [End.]

*-- to detect that COLOR is changed or STYLE is changed [Begin.] 
IF SaveStyle <> lcOldStyle OR SaveColor <> lcOldColor
  lcOldColor = SaveColor
  FOR I = 1 TO MIN(lnGroups,lcOldNo)
    J = ALLTRIM(STR(I))
    *B604252,1 WAB (START) - using the lascalDim instead of laScalDesc
    *IF SUBSTR(laScalDesc[I],1,3) <> lcOldScal&J
    IF laScalDim[I] <> lcOldDim&J
    *B604252,1 WAB (End) 
      llChange = .T.
    ENDIF
  ENDFOR
ENDIF  
*-- to detect that COLOR is changed or STYLE is changed [End.] 

*-- this part to detect that STYLE has changed [Begin.]
IF SaveStyle <> lcOldStyle OR llChange
  lcOldStyle = SaveStyle
  FOR I = 1 TO MIN(lnGroups,3)
    J = ALLTRIM(STR(I))
    lcOldNo = MIN(lnGroups,3)
    *B604252,1 WAB (START) - using the lascalDim instead of laScalDesc
    *lcOldScal&J = SUBSTR(laScalDesc[I],1,3)
    lcOldDim&J = laScalDim[I]
    *B604252,1 WAB (End) 
  ENDFOR
  @ ROW,000 SAY SUBSTR(SaveStyle,1,8)
  @ ROW,009 SAY 'Color'      && 'Color' is hardcoded 'cause it's Custom PRG for comp. using Color
  @ ROW,018 SAY 'Style'
  @ ROW,026 SAY laScalDesc[1]
  @ ROW,070 SAY 'Total'

  IF lnGroups > 1            && If SCALE Groups is greater than 1  
    @ ROW,078 SAY laScalDesc[2]
    @ ROW,123 SAY 'Total'
  ENDIF                      && Endif SCALE Groups is greater than 1
    
  IF lnGroups > 2            && If SCALE Groups is greater than 2  
    @ ROW,131 SAY laScalDesc[3]
    @ ROW,176 SAY 'Total'
  ENDIF                      && Endif SCALE Groups is greater than 2
      
  ROW = ROW + 1
  @ ROW,18  SAY 'Total'
  @ ROW,025 SAY lcStrGrp1
  @ ROW,077 SAY lcStrGrp2
  @ ROW,130 SAY lcStrGrp3      
  ROW = ROW + 1
ELSE
  ROW = IIF(Row = 5,Row,Row - 1)
ENDIF
*-- this part to detect that STYLE has changed [End.]
  
@ ROW,09 SAY SUBSTR(SaveColor,1,6)
@ ROW,18 SAY lnColorTot PICTURE '99999'

*-- this part prints the SCALE value of each Group [Begin.] 
COL = 26
FOR J = 1 TO lnGroup1
  @ ROW,COL SAY laScaleVal[1,J] PICTURE '999'
  COL = COL + 4
ENDFOR
@ ROW,71 SAY lnGrpTot1 PICTURE '9999'

IF lnGroups > 1           && If SCALE Groups is greater than 1    
  COL = 78
  FOR J = 1 TO lnGroup2
    @ ROW,COL SAY laScaleVal[2,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  @ ROW,124 SAY lnGrpTot2 PICTURE '9999'
ENDIF                     && Endif SCALE Groups is greater than 1 
    
IF lnGroups > 2           && If SCALE Groups is greater than 2
  COL = 131
  FOR J = 1 TO lnGroup3
    @ ROW,COL SAY laScaleVal[3,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  @ ROW,177 SAY lnGrpTot3 PICTURE '9999'  
ENDIF                     && Endif SCALE Groups is greater than 2
*-- this part prints the SCALE value of each Group [Begin.] 

*-- this part restores variables to its default [Begin.]    
ROW = ROW + 2
STORE 0 TO lnGroups  , lnGrpDscLn 
STORE 1 TO lnGroup1 , lnGroup2 ,lnGroup3           
STORE ' ' TO lcStrGrp1 , lcStrGrp2 , lcStrGrp3
laScalDesc = ''
*B604252,1 WAB (START) - empty the lascalDim value
laScalDim = ''
*B604252,1 WAB (End) 
SaveStyle  = SUBSTR(STYLE,1,lnMajLen)
SaveColor  = SUBSTR(STYLE,lnClrPo,lnColorLen)
*-- this part restores variables to its default [End.]    
*-- End of lpPrint.

*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 07/04/99
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Sameh (SSE)
*! Date      : 07/04/99
*! Purpose   : validate style
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfStyBrw()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvStyle()
*!**************************************************************************
* 
FUNCTION lfvStyle
lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*--End of lfvStyle.

*!**************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh (SSE)
*! Date      : 07/04/99
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* 
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh (SSE)
*! Date      : 07/04/99
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : lcParm
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X    
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    SELECT STYLE
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.


*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh (SSE)
*! Date      : 07/04/99
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns            : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh (SSE)
*! Date      : 08/08/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*
FUNCTION lfwRepWhen
*-- make the default Style Status in the target array to ACTIVE
lnStatPos = lfItmPos('STYLE.STATUS')
laOGFxFlt[lnStatPos,6] = 'A'
*-- End of lfwRepWhen.


*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh (SSE)
*! Date      : 08/05/1999
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : Position
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
