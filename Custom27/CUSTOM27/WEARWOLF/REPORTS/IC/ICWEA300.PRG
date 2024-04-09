*:***************************************************************************
*: Program file  : ICWEA300
*: Program desc. : CUSTOM CUT & SOLD REPORT PROGRAM FOR (WEA)
*: Date          : 07/27/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls :  
*:         Procedures : lpRptHdr
*:                    : lpPrint
*:
*:         Functions  : lfNonMaj()
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
*: Example : DO ICWEA300
*:***************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR WEA (101546) 
*:***************************************************************************
*:C101546 
*:***************************************************************************
*:Modifications :
*:B803937,1 ABD 01/22/2001 Fix bug that the reports print wrong data and wromg
*:                         Scale and wrong description.
*:B604252,1 WAB 02/21/2001 Change the way of divising to groups from based on the scale descripton
*:B604252,1 WAB            to be based on cDim1 ( dimension one )
*:B804086,1 MHM 04/01/2001 1 - Move the date and page # printed in the header of the report
*:B804086,1 MHM            2 - Move The TOTAL COLUMN to left
*:B804376,1 MHM 08/22/2001 Fix the bug of Error message "Subscript out of bound"     
*:B605697,1 SWK 03/17/2002 Wrong computing total order & total WIP.
*:***************************************************************************


*-- llExtSize is not used in this PRG , because this custom   
*-- program is already designed to use Extended size scale.  
lcRpExp = STRTRAN(lcRpExp,"STYLE.","")

lcRpOTSExp = '(TOTSTK+TOTORD+TOTWIP) <> 0'    
IF !EMPTY(lcRpExp)
  lcRpExp = lcRpExp + [ AND ]
ENDIF
lcRpExp = lcRpExp + lcRpOTSExp

SELECT STYLE
SET FILTER TO &lcRpExp
GO TOP

IF EOF()             && if end of file (no records match criteria)
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF

R_WIDTH = 'XW'
R_TITLE = "Cut & Sold"
SET DEVICE TO PRINT

*-- initialize variables used in report [Begin.]
STORE SPACE(1) TO lcStrGrp1,lcStrGrp2,lcStrGrp3,SaveStyle,SaveColor,lcScaleDes
STORE 0 TO lnTotStk1,lnTotStk2,lnTotStk3,lnTotOrd1,lnTotOrd2,lnTotOrd3,;
           lnTotWip1,lnTotWip2,lnTotWip3,lnGroups,lnGrpDscLn
STORE 1 TO lnGroup1,lnGroup2,lnGroup3
PAGENO = 1
*-- initialize variables used in report [End.]

DO Rpt_Hdr WITH 'ICWEA300',"",R_WIDTH
PAGENO = PAGENO + 1
ROW    = 5

*-- laScaleVal Array to hold the 3 Size Scales with 11 sizes maximum
DIMENSION laStock[3,11] , laWip[3,11] , laOrd[3,11] , laScaleVal[3,11]
DIMENSION laScalDesc[3]
*B604252,1 WAB (START) - initial 2 variables one for holding cdim1 foreach group
STORE SPACE(1) TO lcScalDim
DIMENSION laScalDim[3]
*B604252,1 WAB (END) 

DO WHILE !EOF() AND INKEY() <> 32
  WAIT WINDOW 'Printing ' + lcMajTtl + ' ' + ALLTRIM(CSTYMAJOR) + ' <Space Bar> to abort' NOWAIT
  lnScNo    = 0                              && to count the number of sizes in one scale size
  SaveStyle = SUBSTR(STYLE,1,lnMajLen)       && var to hold current style
  SaveColor = SUBSTR(STYLE,lnClrPo,6)        && var to hold current color 

  IF SEEK('S'+SCALE,'SCALE')    && seek for SCALE in Scale file
    *-- this prog is using the scales in 3 groups e.g. 11 sizes or less but not more
    *-- in SMALL size group and so on , so lnGroups is used to represent the group no
    IF lnGroups <> 0           && If Scale number greater than zero 
    
      *B803937,1 ABD Save all description in the array. [Begin]
      *lcSc_Desc = SUBSTR(SCALE.CSCL_DESC,1,LEN(ALLTRIM(SCALE.CSCL_DESC))-1)      
      *lnGrpDscLn  = ASCAN(laScalDesc,SUBSTR(lcSc_Desc,1,3))
      lcSc_Desc  = SCALE.CSCL_DESC
      *B604252,1 WAB (START) - store the dimention filed to the array
      *lnGrpDscLn = ASCAN(laScalDesc,lcSc_Desc)
      lcScalDim = SCALE.cDim1
      lnGrpDscLn = ASCAN(laScalDim,lcScalDim)
      *B604252,1 WAB (END)  
      *B803937,1 ABD [End]

    ENDIF
    IF lnGrpDscLn<>0              && If Desc number greater than zero (this means that the current dimension is the same as the one stored in the array)
      D = ALLTRIM(STR(lnGrpDscLn))
      IF lnGroup&D<11
        lnScNo = lnGroup&D + SCALE.CNT
        *B803937,1 ABD Add new variables. [Begin]
        *FOR I  = lnGroup&D + 1 TO IIF(lnScNo>11,11,lnScNo)
        *K = ALLTRIM(STR(I-lnGroup&D))
        *lcStrGrp&D = lcStrGrp&D+PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') +' '        
        llFirst    = .F.
        lnNegscale = -1 
        FOR I  = lnGroup&D + 1 TO lnScNo  && scan for the secound dimention. 
          *-- ABD change the way of collecting the data to get the Count of scale first. [Begin]
          K = IIF (lnScNo > 11,ALLTRIM(STR(I - Scale.Cnt)),ALLTRIM(STR(I-lnGroup&D)))
          IF I = 12  && if scale more than 11 scale add the new scale to the new array.
            D = STR(EVAL(D)+1,1)
            lnGrpDscLn = lnGrpDscLn + 1
            llFirst = .T.
          ENDIF
          IF I <=11
            laStock[lnGrpDscLn,I] = STK&K
            laWip[lnGrpDscLn,I]   = WIP&K
            laOrd[lnGrpDscLn,I]   = ORD&K
            laScaleVal[lnGrpDscLn,I] = STK&K+WIP&K-ORD&K
          ELSE
           lnNewI = I - 11
            laStock[lnGrpDscLn,lnNewI] = STK&K
            laWip[lnGrpDscLn,lnNewI]   = WIP&K
            laOrd[lnGrpDscLn,lnNewI]   = ORD&K
            laScaleVal[lnGrpDscLn,lnNewI] = STK&K+WIP&K-ORD&K
            lnNegscale =  lnNegscale + 1            
          ENDIF
          *-- Accum. the total ber recored to void accum. wrong if scale more tha 11 .
          lcStrGrp&D = lcStrGrp&D+PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') +' '          
          lnTotStk&D = lnTotStk&D + STK&K
          *B605697,1 SWK 03/17/2002 (Begin) Wrong computing total order & total WIP.
          *lnTotOrd&D = lnTotOrd&D + WIP&K
          *lnTotWip&D = lnTotWip&D + ORD&K
          lnTotOrd&D = lnTotOrd&D + ORD&K
          lnTotWip&D = lnTotWip&D + WIP&K
          *B605697,1 SWK 03/17/2002 (End)
          *B803937,1 ABD [End]
          
        ENDFOR
        *B803937,1 ABD Add the descr. of the scale to the array if scale more than 11. [Begin]
        *lnGroup&D = lnGroup&D + SCALE.CNT
        *lnGroup&D = IIF(lnGroup&D>11,11,lnGroup&D) 
        IF I > 12
          laScalDesc[lnGroups+1] = SCALE.CSCL_DESC        
          *B604252,1 WAB (START) - store the cdim1 to the array
          laScalDim[lnGroups+1] = SCALE.cDim1        
          *B604252,1 WAB (END)  
          lnGroups  = lnGroups + 1
          lclastGrp = STR(EVAL(D)-1,1) 
          lnGroup&lclastGrp = 11
        ENDIF  
       lnGroup&D = lnGroup&D + IIF(llFirst , lnNegscale , SCALE.CNT)
       *B803937,1 ABD [End]
      ENDIF
    ELSE                        && Else Desc number is not greater than zero
      lnGroups = lnGroups + 1   && var to hold number of Size Scales whether the 1st 11th scales or 2nd  11th scales ...etc
      IF lnGroups > 3
        DO lpPrint
        LOOP
      ENDIF
      laScalDesc[lnGroups] = SCALE.CSCL_DESC
      *B604252,1 WAB (START) - store the cdim1 to the array
      laScalDim[lnGroups] = SCALE.cDim1
      *B604252,1 WAB (End)  
      lcNo = ALLTRIM(STR(lnGroups))
      lnGroup&lcNo = SCALE.CNT
      FOR I = 1 TO SCALE.CNT
        K = ALLTRIM(STR(I))
        lcStrGrp&lcNo = lcStrGrp&lcNo + PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ')+' '
        *B803937,1 ABD Accum. the stk once only here.  [Begin]
        X = ALLTRIM(STR(I))
        laStock[lnGroups,I] = STK&X
        laWip[lnGroups,I]   = WIP&X
        laOrd[lnGroups,I]   = ORD&X
        laScaleVal[lnGroups,I] = STK&X+WIP&X-ORD&X
        *B803937,1 ABD [End]
      ENDFOR
      *B803937,1 ABD  add the total to variables. [Begin]
      lnTotStk&lcNo = TotStk
      lnTotOrd&lcNo = TotOrd
      lnTotWip&lcNo = TotWip
      *B803937,1 ABD [End]      
    ENDIF                     && EndIf of Desc number 
  ELSE                        && else SCALE not found in Scale file
    IF !EOF()                 && If also not end of file
      SKIP                    && then Skip one record 
    ENDIF                    
    LOOP                      
  ENDIF                    && endif SEEK ('S'+Scale,'Scale') 

  *B803937,1 ABD Remark the next lines and Accum.  them up once only... [Begin]
  *IF (SUBSTR(STYLE,1,lnMajLen) = SaveStyle AND SaveColor = SUBSTR(STYLE,lnClrPo,6) AND !EOF()) OR (EMPTY(SaveStyle) AND EMPTY(SaveColor) AND !EOF())
  *  IF lnGrpDscLn<>0
  *    D = ALLTRIM(STR(lnGrpDscLn))   
  *    FOR I = lnGroup&D - SCALE.CNT + 1 TO lnGroup&D
  *      L = ALLTRIM(STR(I - (lnGroup&D - SCALE.CNT)))
  *      laStock[lnGrpDscLn,I] = STK&L
  *      laWip[lnGrpDscLn,I]   = WIP&L
  *      laOrd[lnGrpDscLn,I]   = ORD&L
  *      laScaleVal[lnGrpDscLn,I] = STK&L+WIP&L-ORD&L
  *    ENDFOR      
  *    lnTotStk&D = lnTotStk&D + TotStk
  *    lnTotOrd&D = lnTotOrd&D + TotOrd
  *    lnTotWip&D = lnTotWip&D + TotWip
  *  ELSE
  *    lcScaleDes=laScalDesc[lnGroups]
  *    FOR I = 1 TO SCALE.CNT
  *      X = ALLTRIM(STR(I))
  *      laStock[lnGroups,I] = STK&X
  *      laWip[lnGroups,I]   = WIP&X
  *      laOrd[lnGroups,I]   = ORD&X
  *      laScaleVal[lnGroups,I] = STK&X+WIP&X-ORD&X
  *    ENDFOR
  *    lnTotStk&lcNo = TotStk
  *    lnTotOrd&lcNo = TotOrd
  *    lnTotWip&lcNo = TotWip
  *  ENDIF
  *ENDIF      
  *B803937,1 ABD [End]
  
  IF !EOF()            && if not EOF move one record
    SKIP
  ENDIF

  IF EOF() OR ((SUBSTR(STYLE,1,lnMajLen) <> SaveStyle .OR. SUBSTR(STYLE,lnClrPo,6) <> SaveColor) AND !EMPTY(SaveStyle) AND !EMPTY(SaveColor))
    DO lpPrint               && do the print session for this current style
  ENDIF
  *:B804376,1 MHM 08/22/2001 print style if groups = 3 and style still have data [Start]
  IF lnGroups = 3
    DO lpPrint
  ENDIF
  *:B804376,1 MHM [End]
ENDDO

DO ENDREPORT                   && print the report by ENDREPORT PRG
SET DEVICE TO SCREEN         && return the device again to screen
RETURN                        
*-- End of Report.


*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh (SSE)
*! Date      : 07/26/1999
*! Purpose   : Print the data after being collected.
*!**************************************************************************
*! Called from : ICWEA300.PRG
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

IF ROW > 38               && if page reached max rows (at row = 38)
  ROW = 1                 && start new page at row 1     
  DO Rpt_Hdr WITH 'ICWEA300',"",R_WIDTH
  PAGENO = PAGENO + 1     && increase page number with 1
  ROW    = 5              && start print details at row = 5
ENDIF

*-- prints the STYLE and SCALE Description [Begin.] 
@ ROW,000 SAY SUBSTR(SaveStyle,1,8)
@ ROW,009 SAY 'Color'   && 'Color' is hardcoded 'cause it's Custom PRG for comp. using Color
@ ROW,018 SAY 'Style'
*:B804086,1 MHM 04/01/2001 move total to left[start]
*@ ROW,026 SAY laScalDesc[1]
*@ ROW,070 SAY 'Total'
@ ROW,024 SAY laScalDesc[1]
@ ROW,068 SAY 'Total'
*:B804086,1 MHM 04/01/2001 move total to left[end]
IF lnGroups > 1       && if SCALE Group greater than 1
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,078 SAY laScalDesc[2]
  *@ ROW,123 SAY 'Total'
  @ ROW,074 SAY laScalDesc[2]
  @ ROW,119 SAY 'Total'
  *:B804086,1 MHM 04/01/2001 move total to left[End]
ENDIF                 && Endif SCALE Group greater than 1
    
IF lnGroups > 2       && if SCALE Group greater than 1
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,131 SAY laScalDesc[3]
  *@ ROW,176 SAY 'Total'
  @ ROW,125 SAY laScalDesc[3]
  @ ROW,170 SAY 'Total'
  *:B804086,1 MHM 04/01/2001 move total to left[End]

ENDIF                 && Endif SCALE Group greater than 1
*-- prints the STYLE and SCALE Description [End.] 

*-- prints COLORS and SCALE Sizes of each Group [Begin.]     
ROW = ROW + 1
@ ROW,09 SAY SUBSTR(SaveColor,1,6)
@ ROW,18 SAY 'Total'
*:B804086,1 MHM 04/01/2001 move total to left[start]
*@ ROW,025 SAY lcStrGrp1
*@ ROW,077 SAY lcStrGrp2
*@ ROW,130 SAY lcStrGrp3
@ ROW,023 SAY lcStrGrp1
@ ROW,073 SAY lcStrGrp2
@ ROW,124 SAY lcStrGrp3
*:B804086,1 MHM 04/01/2001 move total to left[end]
*-- prints COLORS and SCALE Sizes of each Group [End.]     

*-- Prints the Available ON HAND Stock [Begin.]
ROW = ROW + 1
@ ROW,00 SAY 'On hand'
@ ROW,18 SAY lnTotStk1+lnTotStk2+lnTotStk3 PICTURE '99999'
*:B804086,1 MHM 04/01/2001 move total to left[start]
*COL = 26
COL = 24
*:B804086,1 MHM 04/01/2001 move total to left[End]
FOR J = 1 TO lnGroup1
  @ ROW,COL SAY laStock[1,J] PICTURE '999'
  COL = COL + 4
ENDFOR
*:B804086,1 MHM 04/01/2001 move total to left[start]
*@ ROW,71 SAY lnTotStk1 PICTURE '9999'
@ ROW,69 SAY lnTotStk1 PICTURE '9999'
*:B804086,1 MHM 04/01/2001 move total to left[End]

IF lnGroups > 1           && If SCALE Group greater than 1
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 78
  COL = 74
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup2
    @ ROW,COL SAY laStock[2,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,124 SAY lnTotStk2 PICTURE '9999'
  @ ROW,120 SAY lnTotStk2 PICTURE '9999'
  *:B804086,1 MHM 04/01/2001 move total to left[End]
ENDIF                     && Endif SCALE Group greater than 1
    
IF lnGroups > 2           && If SCALE Group greater than 2
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 131  
  COL = 125  
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup3
    @ ROW,COL SAY laStock[3,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,177 SAY lnTotStk3 PICTURE '9999'  
  @ ROW,171 SAY lnTotStk3 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[End]
ENDIF                     && Endif SCALE Group greater than 2
*-- Prints the Available ON HAND Stock [End.]

*-- Prints WORK IN PROCESS (WIP) [Begin.]
ROW = ROW + 1
@ ROW,00 SAY 'WIP'
@ ROW,18 SAY lnTotWip1+lnTotWip2+lnTotWip3 PICTURE '99999'
*:B804086,1 MHM 04/01/2001 move total to left[start]
*COL = 26
COL = 24
*:B804086,1 MHM 04/01/2001 move total to left[End]
FOR J = 1 TO lnGroup1
  @ ROW,COL SAY laWip[1,J] PICTURE '999'
  COL = COL + 4
ENDFOR
*:B804086,1 MHM 04/01/2001 move total to left[start]
*@ ROW,71 SAY lnTotWip1 PICTURE '9999'
@ ROW,69 SAY lnTotWip1 PICTURE '9999'
*:B804086,1 MHM 04/01/2001 move total to left[End]

IF lnGroups > 1          && If SCALE Group greater than 1
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 78
  COL = 74
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup2
    @ ROW,COL SAY laWip[2,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,124 SAY lnTotWip2 PICTURE '9999'  
  @ ROW,120 SAY lnTotWip2 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[End]
ENDIF                   && Endif SCALE Group greater than 1
    
IF lnGroups > 2         && If SCALE Group greater than 2
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 131
  COL = 125
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup3
    @ ROW,COL SAY laWip[3,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,177 SAY lnTotWip3 PICTURE '9999'  
  @ ROW,171 SAY lnTotWip3 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[End]
ENDIF                   && Endif SCALE Group greater than 2
*-- Prints WORK IN PROCESS (WIP) [End.]

*-- Prints ON HAND [Begin.]
ROW = ROW + 1
@ ROW,00 SAY 'On order'
@ ROW,18 SAY lnTotOrd1+lnTotOrd2+lnTotOrd3 PICTURE '99999'
*:B804086,1 MHM 04/01/2001 move total to left[start]
*COL = 26
COL = 24
*:B804086,1 MHM 04/01/2001 move total to left[End]
FOR J = 1 TO lnGroup1
  @ ROW,COL SAY laOrd[1,J] PICTURE '999'
  COL = COL + 4
ENDFOR
*:B804086,1 MHM 04/01/2001 move total to left[start]
*@ ROW,71 SAY lnTotOrd1 PICTURE '9999'
@ ROW,69 SAY lnTotOrd1 PICTURE '9999'
*:B804086,1 MHM 04/01/2001 move total to left[End]
    
IF lnGroups > 1        && If SCALE Group greater than 1
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 78
  COL = 74
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup2
    @ ROW,COL SAY laOrd[2,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,124 SAY lnTotOrd2 PICTURE '9999'  
  @ ROW,120 SAY lnTotOrd2 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[End]
ENDIF                  && Endif SCALE Group greater than 1 
    
IF lnGroups > 2        && If SCALE Group greater than 2
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 131  
  COL = 125 
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup3
    @ ROW,COL SAY laOrd[3,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,177 SAY lnTotOrd3 PICTURE '9999'  
  @ ROW,171 SAY lnTotOrd3 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[ENd]
ENDIF                  && Endif SCALE Group greater than 2 
*-- Prints ON HAND [End.]

*-- Prints AVAILABLE TO SELL [Begin.]    
ROW = ROW + 1
@ ROW,00 SAY 'Avail to sell'
@ ROW,18 SAY lnTotStk1+lnTotStk2+lnTotStk3+lnTotWip1+lnTotWip2+lnTotWip3-lnTotOrd1-lnTotOrd2-lnTotOrd3 PICTURE '99999'
*:B804086,1 MHM 04/01/2001 move total to left[start]
*COL = 26
COL = 24
*:B804086,1 MHM 04/01/2001 move total to left[start]
FOR J = 1 TO lnGroup1
  @ ROW,COL SAY laScaleVal[1,J] PICTURE '999'
  COL = COL + 4
ENDFOR

*:B804086,1 MHM 04/01/2001 move total to left[start]
*@ ROW,71 SAY lnTotStk1+lnTotWip1-lnTotOrd1 PICTURE '9999'
@ ROW,69 SAY lnTotStk1+lnTotWip1-lnTotOrd1 PICTURE '9999'
*:B804086,1 MHM 04/01/2001 move total to left[End]

IF lnGroups > 1          && If SCALE Group greater than 1
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 78  
  COL = 74 
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup2
    @ ROW,COL SAY laScaleVal[2,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,124 SAY lnTotStk2+lnTotWip2-lnTotOrd2 PICTURE '9999'  
  @ ROW,120 SAY lnTotStk2+lnTotWip2-lnTotOrd2 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  
ENDIF                    && Endif SCALE Group greater than 1 
    
IF lnGroups > 2          && If SCALE Group greater than 2
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *COL = 131  
  COL = 125
  *:B804086,1 MHM 04/01/2001 move total to left[End]
  FOR J = 1 TO lnGroup3
    @ ROW,COL SAY laScaleVal[3,J] PICTURE '999'
    COL = COL + 4
  ENDFOR
  
  *:B804086,1 MHM 04/01/2001 move total to left[start]
  *@ ROW,177 SAY lnTotStk3+lnTotWip3-lnTotOrd3 PICTURE '9999'  
  @ ROW,171 SAY lnTotStk3+lnTotWip3-lnTotOrd3 PICTURE '9999'  
  *:B804086,1 MHM 04/01/2001 move total to left[ENd]
  
ENDIF                   && Endif SCALE Group greater than 2 
*-- Prints AVAILABLE TO SELL [End.]    
    
ROW = ROW + 2               
STORE SPACE(0) TO laStock , laWip , laOrd , laScalDesc
*B604252,1 WAB (START) -empty the array value
STORE SPACE(0) TO laScalDim
*B604252,1 WAB (END) 
STORE SPACE(1) TO lcStrGrp1,lcStrGrp2,lcStrGrp3
STORE 0 TO lnGroups,lnTotStk1,lnTotStk2,lnTotStk3,lnTotOrd1,lnTotOrd2,lnTotOrd3 ,;
           lnTotWip1,lnTotWip2,lnTotWip3,lnGrpDscLn
STORE 1 TO lnGroup1,lnGroup2,lnGroup3
SaveStyle  = SUBSTR(STYLE,1,lnMajLen)
SaveColor  = SUBSTR(STYLE,lnClrPo,6)
*-- End of lpPrint.

*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 07/26/99
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
*! Date      : 07/26/99
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
*! Date      : 07/26/99
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
*! Date      : 07/26/99
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

PRIVATE lcRpStatus
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
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh (SSE)
*! Date      : 07/26/99
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
*! Date      : 07/27/1999
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
*! Date      : 07/27/1999
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

