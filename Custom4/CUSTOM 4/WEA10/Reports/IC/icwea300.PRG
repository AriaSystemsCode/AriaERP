*:***************************************************************************
*: Program file  : ICWEA300
*: Program desc. : Convert CUSTOM CUT & SOLD REPORT PROGRAM FOR (WEA) to Aria4xp
*: Date          : 02/18/2007
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Hassan Ibrahim (HIA)
*: C200742 - T20070117.0015 
*:***************************************************************************
*: Calls :
*:         Procedures : lpRptHdr
*:                    : LPINSERT
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
*:B608039,1 MMT 04/12/2007 fix bug of left margin is too Small 
*:B608506,1 MMT 04/07/2008 Fix bug of Missing Scales in layout {T20071115.0010}
*:***************************************************************************


*-- llExtSize is not used in this PRG , because this custom
*-- program is already designed to use Extended size scale.
LCRPEXP = STRTRAN(LCRPEXP,"STYLE.","")
LCRPOTSEXP = '(TOTSTK+TOTORD+TOTWIP) <> 0'
IF !EMPTY(LCRPEXP)
  LCRPEXP = LCRPEXP + [ AND ]
ENDIF
LCRPEXP = LCRPEXP + LCRPOTSEXP

SELECT STYLE
SET FILTER TO &LCRPEXP
GO TOP

IF EOF() && if end of file (no records match criteria)
  *--No records to display.
  = GFMODALGEN('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

*-- initialize variables used in report [Begin.]
STORE SPACE(1) TO LCSTRGRP1,LCSTRGRP2,LCSTRGRP3,SAVESTYLE,SAVECOLOR,LCSCALEDES
STORE 0 TO LNTOTSTK1,LNTOTSTK2,LNTOTSTK3,LNTOTORD1,LNTOTORD2,LNTOTORD3,;
  LNTOTWIP1,LNTOTWIP2,LNTOTWIP3,LNGROUPS,LNGRPDSCLN
STORE 1 TO LNGROUP1,LNGROUP2,LNGROUP3

*-- initialize variables used in report [End.]
loogScroll.lcOGLastForm = 'ICWEA300'

*:B608039,1 MMT 04/12/2007 fix bug of left margin is too Small [Start]
*loogScroll.cCROrientation = 'P'
loogScroll.cCROrientation = 'L'
*:B608039,1 MMT 04/12/2007 fix bug of left margin is too Small [End]

DIMENSION LOOGSCROLL.LACRPARAMS[3,2]
LOOGSCROLL.LACRPARAMS[1,1] = 'ReportName'
LOOGSCROLL.LACRPARAMS[1,2] = 'Custom cut & sold'
LOOGSCROLL.LACRPARAMS[2,1] = 'ReportTitle'
LOOGSCROLL.LACRPARAMS[2,2] = 'Cut & Sold'
LOOGSCROLL.LACRPARAMS[3,1] = 'CompanyName'
LOOGSCROLL.LACRPARAMS[3,2] = ALLTRIM(OARIAAPPLICATION.ACTIVECOMPANYNAME)

LCTMPFILE=LOOGSCROLL.GFTEMPNAME()
=LFCREATEWRKCUR(LCTMPFILE)
SELECT STYLE

*-- laScaleVal Array to hold the 3 Size Scales with 11 sizes maximum
DIMENSION LASTOCK[3,11] , LAWIP[3,11] , LAORD[3,11] , LASCALEVAL[3,11], LaSTRGRP[3,11]
STORE 0 TO LASTOCK, LAWIP, LAORD, LASCALEVAL
STORE '' TO LaSTRGRP

DIMENSION LASCALDESC[3]
STORE '' TO LASCALDESC
*B604252,1 WAB (START) - initial 2 variables one for holding cdim1 foreach group
STORE SPACE(1) TO LCSCALDIM
DIMENSION LASCALDIM[3]

*B604252,1 WAB (END)
*loOGScroll.llOGFltCh 
DO WHILE !EOF() AND INKEY() <> 32
  *WAIT WINDOW 'Printing ' + LCMAJTTL + ' ' + ALLTRIM(CSTYMAJOR) + ' <Space Bar> to abort' NOWAIT
  WAIT WINDOW 'Printing ' + LCMAJTTL + ' ' + ALLTRIM(CSTYMAJOR) NOWAIT
  LNSCNO    = 0                              && to count the number of sizes in one scale size
  SAVESTYLE = SUBSTR(STYLE,1,LNMAJLEN)       && var to hold current style
  SAVECOLOR = SUBSTR(STYLE,LNCLRPO,6)        && var to hold current color

  IF SEEK('S'+SCALE,'SCALE')    && seek for SCALE in Scale file
    *-- this prog is using the scales in 3 groups e.g. 11 sizes or less but not more
    *-- in SMALL size group and so on , so lnGroups is used to represent the group no
    IF LNGROUPS <> 0           && If Scale number greater than zero

      *B803937,1 ABD Save all description in the array. [Begin]
      *lcSc_Desc = SUBSTR(SCALE.CSCL_DESC,1,LEN(ALLTRIM(SCALE.CSCL_DESC))-1)
      *lnGrpDscLn  = ASCAN(laScalDesc,SUBSTR(lcSc_Desc,1,3))
      LCSC_DESC  = SCALE.CSCL_DESC
      *B604252,1 WAB (START) - store the dimention filed to the array
      *lnGrpDscLn = ASCAN(laScalDesc,lcSc_Desc)
      LCSCALDIM = SCALE.CDIM1
      LNGRPDSCLN = ASCAN(LASCALDIM,LCSCALDIM)
      *B604252,1 WAB (END)
      *B803937,1 ABD [End]

    ENDIF
    IF LNGRPDSCLN<>0              && If Desc number greater than zero (this means that the current dimension is the same as the one stored in the array)
      D = ALLTRIM(STR(LNGRPDSCLN))
      IF LNGROUP&D<11
        LNSCNO = LNGROUP&D + SCALE.CNT
        *B803937,1 ABD Add new variables. [Begin]
        *FOR I  = lnGroup&D + 1 TO IIF(lnScNo>11,11,lnScNo)
        *K = ALLTRIM(STR(I-lnGroup&D))
        *lcStrGrp&D = lcStrGrp&D+PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') +' '
        LLFIRST    = .F.
        LNNEGSCALE = -1
        
        *:B608506,1 MMT 04/07/2008 Fix bug of Missing Scales in layout {Start}
        lnCountIndex = 0
        *:B608506,1 MMT 04/07/2008 Fix bug of Missing Scales in layout {End}
        
        FOR I  = LNGROUP&D + 1 TO LNSCNO  && scan for the secound dimention.
          *-- ABD change the way of collecting the data to get the Count of scale first. [Begin]
          
          *:B608506,1 MMT 04/07/2008 Fix bug of Missing Scales in layout {Start}
          *K = IIF (LNSCNO > 11,ALLTRIM(STR(I - SCALE.CNT)),ALLTRIM(STR(I-LNGROUP&D)))
	      lnCountIndex = lnCountIndex + 1
	      K = ALLTRIM(STR(lnCountIndex))
    	  *:B608506,1 MMT 04/07/2008 Fix bug of Missing Scales in layout {End}
          
          IF I = 12  && if scale more than 11 scale add the new scale to the new array.
            D = STR(EVAL(D)+1,1)
            LNGRPDSCLN = LNGRPDSCLN + 1
            LLFIRST = .T.
          ENDIF
          IF I <=11
            LASTOCK[lnGrpDscLn,I] = STK&K
            LAWIP[lnGrpDscLn,I]   = WIP&K
            LAORD[lnGrpDscLn,I]   = ORD&K
            LASCALEVAL[lnGrpDscLn,I] = STK&K+WIP&K-ORD&K
            LaSTRGRP[lnGrpDscLn,I] = PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ')             
          ELSE
            LNNEWI = I - 11
            LASTOCK[lnGrpDscLn,lnNewI] = STK&K
            LAWIP[lnGrpDscLn,lnNewI]   = WIP&K
            LAORD[lnGrpDscLn,lnNewI]   = ORD&K
            LASCALEVAL[lnGrpDscLn,lnNewI] = STK&K+WIP&K-ORD&K
            LNNEGSCALE =  LNNEGSCALE + 1
            LaSTRGRP[lnGrpDscLn,lnNewI] = PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') 
          ENDIF
          *-- Accum. the total ber recored to void accum. wrong if scale more tha 11 .
          LCSTRGRP&D = LCSTRGRP&D+PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') +' '

          
          LNTOTSTK&D = LNTOTSTK&D + STK&K
          *B605697,1 SWK 03/17/2002 (Begin) Wrong computing total order & total WIP.
          *lnTotOrd&D = lnTotOrd&D + WIP&K
          *lnTotWip&D = lnTotWip&D + ORD&K
          LNTOTORD&D = LNTOTORD&D + ORD&K
          LNTOTWIP&D = LNTOTWIP&D + WIP&K
          *B605697,1 SWK 03/17/2002 (End)
          *B803937,1 ABD [End]

        ENDFOR
        *B803937,1 ABD Add the descr. of the scale to the array if scale more than 11. [Begin]
        *lnGroup&D = lnGroup&D + SCALE.CNT
        *lnGroup&D = IIF(lnGroup&D>11,11,lnGroup&D)
        IF I > 12
          LASCALDESC[lnGroups+1] = SCALE.CSCL_DESC
          *B604252,1 WAB (START) - store the cdim1 to the array
          LASCALDIM[lnGroups+1] = SCALE.CDIM1
          *B604252,1 WAB (END)
          LNGROUPS  = LNGROUPS + 1
          LCLASTGRP = STR(EVAL(D)-1,1)
          LNGROUP&LCLASTGRP = 11
        ENDIF
        LNGROUP&D = LNGROUP&D + IIF(LLFIRST , LNNEGSCALE , SCALE.CNT)
        *B803937,1 ABD [End]
      ENDIF
    ELSE                        && Else Desc number is not greater than zero
      LNGROUPS = LNGROUPS + 1   && var to hold number of Size Scales whether the 1st 11th scales or 2nd  11th scales ...etc
      IF LNGROUPS > 3
        DO LPINSERT
        LOOP
      ENDIF
      LASCALDESC[lnGroups] = SCALE.CSCL_DESC
      *B604252,1 WAB (START) - store the cdim1 to the array
      LASCALDIM[lnGroups] = SCALE.CDIM1
      *B604252,1 WAB (End)
      LCNO = ALLTRIM(STR(LNGROUPS))
      LNGROUP&LCNO = SCALE.CNT
      FOR I = 1 TO SCALE.CNT
        K = ALLTRIM(STR(I))
        LCSTRGRP&LCNO = LCSTRGRP&LCNO + PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ')+' '
        LaSTRGRP[lnGroups,I] = PADL(SUBSTR(ALLTRIM(SCALE.SZ&K),1,3),3,' ') 
        *B803937,1 ABD Accum. the stk once only here.  [Begin]
        X = ALLTRIM(STR(I))
        LASTOCK[lnGroups,I] = STK&X
        LAWIP[lnGroups,I]   = WIP&X
        LAORD[lnGroups,I]   = ORD&X
        LASCALEVAL[lnGroups,I] = STK&X+WIP&X-ORD&X
        *B803937,1 ABD [End]
      ENDFOR
      *B803937,1 ABD  add the total to variables. [Begin]
      LNTOTSTK&LCNO = TOTSTK
      LNTOTORD&LCNO = TOTORD
      LNTOTWIP&LCNO = TOTWIP
      *B803937,1 ABD [End]
    ENDIF                     && EndIf of Desc number
  ELSE                        && else SCALE not found in Scale file
    IF !EOF()                 && If also not end of file
      SKIP                    && then Skip one record
    ENDIF
    LOOP
  ENDIF                    && endif SEEK ('S'+Scale,'Scale')

  IF !EOF()            && if not EOF move one record
    SKIP
  ENDIF

  IF EOF() OR ((SUBSTR(STYLE,1,LNMAJLEN) <> SAVESTYLE .OR. SUBSTR(STYLE,LNCLRPO,6) <> SAVECOLOR) AND !EMPTY(SAVESTYLE) AND !EMPTY(SAVECOLOR))
    DO LPINSERT               && do the print session for this current style
  ENDIF
  *:B804376,1 MHM 08/22/2001 print style if groups = 3 and style still have data [Start]
  IF LNGROUPS = 3
    DO LPINSERT
  ENDIF
  *:B804376,1 MHM [End]
ENDDO


*DO ENDREPORT                   && print the report by ENDREPORT PRG
*SET DEVICE TO SCREEN         && return the device again to screen

SELECT(LOOGSCROLL.LACRTABLES[1])
LCTMPFILE=LOOGSCROLL.GFTEMPNAME()
COPY TO (ADDBS(OARIAAPPLICATION.WORKDIR)+LCTMPFILE+".DBF") TYPE FOX2X 
*COPY TO ("D:\ARIA4XP\WORK\CUTSOLD.DBF") TYPE FOX2X 
USE IN (LOOGSCROLL.LACRTABLES[1])

LOOGSCROLL.LACRTABLES[1] = ADDBS(OARIAAPPLICATION.WORKDIR)+LCTMPFILE+".DBF"    
=gfDispRe ()

RETURN

*-- End of Report.
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
FUNCTION LFNONMAJ
  *-- Compute Free/Color Items in Style Structure. [Begin]
  LNMAJSEG  = GFITEMMASK('SM')  && No. of major segments.
  DIMENSION LAMAJSEG[1,1]
  = GFITEMMASK(@LAMAJSEG)

  LLSTOPCONC = .F.

  *-- Loop Around Non Major elements.
  FOR LNI = LNMAJSEG + 1 TO ALEN(LAMAJSEG,1)
    LNNONMAJPO = IIF(LNNONMAJPO = 0,LAMAJSEG[lnI,4],LNNONMAJPO)
    IF LAMAJSEG[lnI,1] = 'F' AND !LLSTOPCONC
      LCFREECLR  = IIF(EMPTY(LCFREECLR),LAMAJSEG[lnI,1],LCFREECLR)
      LCNONMAJPI = IIF(EMPTY(LCNONMAJPI),LAMAJSEG[lnI,3],;
        LCNONMAJPI + LAMAJSEG[lnI-1,6] + LAMAJSEG[lnI,3])
      LCNONMAJT  = IIF(EMPTY(LCNONMAJT),PADR(LAMAJSEG[lnI,2],LEN(LAMAJSEG[lnI,3])),;
        LCNONMAJT + LAMAJSEG[lnI-1,6] + PADR(LAMAJSEG[lnI,2],LEN(LAMAJSEG[lnI,3])))
    ENDIF

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF LAMAJSEG[lnI,1] = 'C' OR (!EMPTY(LCFREECLR) AND LAMAJSEG[lnI,1] != 'F')
      IF LAMAJSEG[lnI,1] = 'C'
        LNCLRPO    = LAMAJSEG[lnI,4]
        LCFREECLR  = LAMAJSEG[lnI,1]    && which will be 'C'
        LCNONMAJPI = LAMAJSEG[lnI,3]
        LCNONMAJT  = PADR(LAMAJSEG[lnI,2],LEN(LAMAJSEG[lnI,3]))
        EXIT
      ELSE
        *-- this means that another type is found rather than color or free
        *-- and so we neednot to concat. to free variables
        LLSTOPCONC = .T.
      ENDIF
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(LCNONMAJPI) TO LNFREELEN , LNCOLORLEN
  LCCOLORTT = 'Only This ' + ALLTRIM(LCNONMAJT)
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
FUNCTION LFVSTYLE
  LCSTYLE = VARREAD()
  LCTAG = ORDER('STYLE')
  SET ORDER TO CSTYLE IN STYLE

  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(&LCSTYLE.,'Style')
      &LCSTYLE = STYLE.CSTYMAJOR
    ELSE
      &LCSTYLE = GFSTYBRW('M',"","",.F.)
    ENDIF
  ELSE
    &LCSTYLE = ''
  ENDIF
  SET ORDER TO LCTAG IN STYLE
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
FUNCTION LFVFABRIC
  LCFABOBJ = VARREAD()
  LCFAB    = &LCFABOBJ
  LLUSEBYME = .F.

  IF !USED('FABRIC')
    LLUSEBYME = .T.
    USE (GCDATADIR+'FABRIC') IN 0 SHARE
  ENDIF

  LCTAG = ORDER('FABRIC')
  SET ORDER TO FABRIC IN FABRIC

  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(LCFAB,'FABRIC')
      &LCFABOBJ = FABRIC.FABRIC
    ELSE
      = FABROW(@LCFAB,'*')
      &LCFABOBJ = LCFAB
    ENDIF
  ELSE
    &LCFABOBJ = ''
  ENDIF
  SET ORDER TO FABRIC IN FABRIC
  IF LLUSEBYME
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
FUNCTION LFSRVSTY
  PARAMETERS LCPARM

  PRIVATE LCRPSTATUS
  DO CASE
    CASE LCPARM = 'S'  && Set code
      *-- open this file in another alias to set order to Style Major
      *-- unique index.
      *USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
      SELECT STYLE
      SET ORDER TO TAG CSTYLE
      SET RELATION TO STYLE.STYLE INTO STYLE_X
      GO TOP IN STYLE
    CASE LCPARM = 'R'  && Reset code
      SELECT STYLE
      SET ORDER TO TAG STYLE
      LLCLEARSTY = .F.
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
FUNCTION LFSTYSUM
  PARAMETERS LCSTY,LCCOMP,LNADDTOVAR
  PRIVATE LNSTYREC
  LNTOTCOMP = 0

  IF RECCOUNT('STYLE') != 0
    LNSTYREC = RECNO('STYLE')
    SELECT STYLE_X
    *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
    SUM &LCCOMP TO LNTOTCOMP WHILE ALLTRIM(CSTYMAJOR) == ALLTRIM(LCSTY)
    SELECT STYLE
    IF BETWEEN(LNSTYREC,1,RECCOUNT())
      GO LNSTYREC
    ENDIF
    DO CASE
      CASE LNADDTOVAR = 1
        LNO_T_S = LNTOTCOMP
      CASE LNADDTOVAR = 2
        LNO_T_S = LNO_T_S + LNTOTCOMP
      CASE LNADDTOVAR = 3
        LNO_T_S = LNO_T_S - LNTOTCOMP
    ENDCASE
  ENDIF
  RETURN INT(LNTOTCOMP)
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
FUNCTION LFWREPWHEN
  *-- make the default Style Status in the target array to ACTIVE
  LNSTATPOS = LFITMPOS('STYLE.STATUS')
  LAOGFXFLT[lnStatPos,6] = 'A'
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
FUNCTION LFITMPOS
  PARAMETERS LCITMINFLT
  PRIVATE LNITMPOS

  LNITMPOS = ASCAN(LAOGFXFLT,LCITMINFLT)
  IF LNITMPOS > 0
    LNITMPOS = ASUBSCRIPT(LAOGFXFLT,LNITMPOS,1)
  ENDIF
  RETURN LNITMPOS
  *-- End of lfItmPos.



  *!**************************************************************************
  *! Name      : LPINSERT
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
  *! Example     : DO LPINSERT
  *!**************************************************************************
  *
PROCEDURE LPINSERT

  INSERT INTO (LOOGSCROLL.LACRTABLES[1])(CSTYLE , CCOLOR ,;
    SCL11 ,SCL12 ,SCL13 ,SCL14 ,SCL15 ,SCL16 ,SCL17 ,SCL18 ,SCL19 ,SCL110 ,SCL111 ,;
    SCL21 ,SCL22 ,SCL23 ,SCL24 ,SCL25 ,SCL26 ,SCL27 ,SCL28 ,SCL29 ,SCL210 ,SCL211 ,;
    SCL31 ,SCL32 ,SCL33 ,SCL34 ,SCL35 ,SCL36 ,SCL37 ,SCL38 ,SCL39 ,SCL310 ,SCL311 ,;  
    STK11 ,STK12 ,STK13 ,STK14 ,STK15 ,STK16 ,STK17 ,STK18 ,STK19 ,STK110 ,STK111 ,;
    STK21 ,STK22 ,STK23 ,STK24 ,STK25 ,STK26 ,STK27 ,STK28 ,STK29 ,STK210 ,STK211 ,;
    STK31 ,STK32 ,STK33 ,STK34 ,STK35 ,STK36 ,STK37 ,STK38 ,STK39 ,STK310 ,STK311 ,;
    TOTSTK1 ,TOTSTK2 ,TOTSTK3 ,;
    WIP11 ,WIP12 ,WIP13 ,WIP14 ,WIP15 ,WIP16 ,WIP17 ,WIP18 ,WIP19 ,WIP110 ,WIP111 ,;
    WIP21 ,WIP22 ,WIP23 ,WIP24 ,WIP25 ,WIP26 ,WIP27 ,WIP28 ,WIP29 ,WIP210 ,WIP211 ,;
    WIP31 ,WIP32 ,WIP33 ,WIP34 ,WIP35 ,WIP36 ,WIP37 ,WIP38 ,WIP39 ,WIP310 ,WIP311 ,;
    TOTWIP1 ,TOTWIP2 ,TOTWIP3 ,;
    ORD11 ,ORD12 ,ORD13 ,ORD14 ,ORD15 ,ORD16 ,ORD17 ,ORD18 ,ORD19 ,ORD110 ,ORD111 ,;
    ORD21 ,ORD22 ,ORD23 ,ORD24 ,ORD25 ,ORD26 ,ORD27 ,ORD28 ,ORD29 ,ORD210 ,ORD211 ,;
    ORD31 ,ORD32 ,ORD33 ,ORD34 ,ORD35 ,ORD36 ,ORD37 ,ORD38 ,ORD39 ,ORD310 ,ORD311 ,;
    TOTORD1 ,TOTORD2 ,TOTORD3,;
    SCLVL11 ,SCLVL12 ,SCLVL13 , SCLVL14 ,SCLVL15 ,SCLVL16 ,SCLVL17 ,SCLVL18 ,SCLVL19 ,SCLVL110 ,SCLVL111 ,;
    SCLVL21 ,SCLVL22 ,SCLVL23 ,SCLVL24 ,SCLVL25 ,SCLVL26 ,SCLVL27 ,SCLVL28 ,SCLVL29 ,SCLVL210 ,SCLVL211 ,;
    SCLVL31 ,SCLVL32 ,SCLVL33 ,SCLVL34 ,SCLVL35 ,SCLVL36 ,SCLVL37 ,SCLVL38 ,SCLVL39 ,SCLVL310 ,SCLVL311 ,;
    TOTSCLV1 ,TOTSCLV2 ,TOTSCLV3,;
    SCLDSC1 ,SCLDSC2 ,SCLDSC3 , NGROUP1, NGROUP2, NGROUP3, nGroups);
    VALUES (SUBSTR(SAVESTYLE,1,8), SUBSTR(SAVECOLOR,1,6),;
    LaSTRGRP[1,1] ,LaSTRGRP[1,2] ,LaSTRGRP[1,3] ,LaSTRGRP[1,4] ,LaSTRGRP[1,5] ,LaSTRGRP[1,6],LaSTRGRP[1,7] ,LaSTRGRP[1,8] ,LaSTRGRP[1,9] ,LaSTRGRP[1,10] ,LaSTRGRP[1,11] ,;
    LaSTRGRP[2,1] ,LaSTRGRP[2,2] ,LaSTRGRP[2,3] ,LaSTRGRP[2,4] ,LaSTRGRP[2,5] ,LaSTRGRP[2,6],LaSTRGRP[2,7] ,LaSTRGRP[2,8] ,LaSTRGRP[2,9] ,LaSTRGRP[2,10] ,LaSTRGRP[2,11] ,;
    LaSTRGRP[3,1] ,LaSTRGRP[3,2] ,LaSTRGRP[3,3] ,LaSTRGRP[3,4] ,LaSTRGRP[3,5] ,LaSTRGRP[3,6],LaSTRGRP[3,7] ,LaSTRGRP[3,8] ,LaSTRGRP[3,9] ,LaSTRGRP[3,10] ,LaSTRGRP[3,11] ,;
    LASTOCK[1,1],LASTOCK[1,2] ,LASTOCK[1,3] ,LASTOCK[1,4] ,LASTOCK[1,5] ,LASTOCK[1,6] ,LASTOCK[1,7] ,LASTOCK[1,8] ,LASTOCK[1,9] ,LASTOCK[1,10] ,LASTOCK[1,11],;
    LASTOCK[2,1] ,LASTOCK[2,2] ,LASTOCK[2,3] ,LASTOCK[2,4 ],LASTOCK[2,5] ,LASTOCK[2,6] ,LASTOCK[2,7] ,LASTOCK[2,8] ,LASTOCK[2,9],LASTOCK[2,10] ,LASTOCK[2,11],;
    LASTOCK[3,1] ,LASTOCK[3,2] ,LASTOCK[3,3] ,LASTOCK[3,4] ,LASTOCK[3,5] ,LASTOCK[3,6] ,LASTOCK[3,7] ,LASTOCK[3,8] ,LASTOCK[3,9],LASTOCK[3,10] ,LASTOCK[3,11],;
    LNTOTSTK1 ,LNTOTSTK2 ,LNTOTSTK3 ,;
    LAWIP[1,1],LAWIP[1,2] ,LAWIP[1,3] ,LAWIP[1,4] ,LAWIP[1,5] ,LAWIP[1,6] ,LAWIP[1,7] ,LAWIP[1,8] ,LAWIP[1,9] ,LAWIP[1,10] ,LAWIP[1,11] ,;
    LAWIP[2,1] ,LAWIP[2,2] ,LAWIP[2,3] ,LAWIP[2,4 ],LAWIP[2,5] ,LAWIP[2,6] ,LAWIP[2,7] ,LAWIP[2,8] ,LAWIP[2,9],LAWIP[2,10] ,LAWIP[2,11] ,;
    LAWIP[3,1] ,LAWIP[3,2] ,LAWIP[3,3] ,LAWIP[3,4] ,LAWIP[3,5] ,LAWIP[3,6] ,LAWIP[3,7] ,LAWIP[3,8] ,LAWIP[3,9],LAWIP[3,10] ,LAWIP[3,11] ,;
    LNTOTWIP1 ,LNTOTWIP2,LNTOTWIP3,;
    LAORD[1,1],LAORD[1,2] ,LAORD[1,3] ,LAORD[1,4] ,LAORD[1,5] ,LAORD[1,6] ,LAORD[1,7] ,LAORD[1,8] ,LAORD[1,9] ,LAORD[1,10] ,LAORD[1,11] ,;
    LAORD[2,1] ,LAORD[2,2] ,LAORD[2,3] ,LAORD[2,4 ],LAORD[2,5] ,LAORD[2,6] ,LAORD[2,7] ,LAORD[2,8] ,LAORD[2,9],LAORD[2,10] ,LAORD[2,11] ,;
    LAORD[3,1] ,LAORD[3,2] ,LAORD[3,3] ,LAORD[3,4] ,LAORD[3,5] ,LAORD[3,6] ,LAORD[3,7] ,LAORD[3,8] ,LAORD[3,9],LAORD[3,10] ,LAORD[3,11] ,;
    LNTOTORD1 ,LNTOTORD2 ,LNTOTORD3 ,;
    LASCALEVAL[1,1],LASCALEVAL[1,2] ,LASCALEVAL[1,3] ,LASCALEVAL[1,4] ,LASCALEVAL[1,5] ,LASCALEVAL[1,6] ,LASCALEVAL[1,7] ,LASCALEVAL[1,8] ,LASCALEVAL[1,9] ,LASCALEVAL[1,10] ,LASCALEVAL[1,11] ,;
    LASCALEVAL[2,1] ,LASCALEVAL[2,2] ,LASCALEVAL[2,3] ,LASCALEVAL[2,4 ],LASCALEVAL[2,5] ,LASCALEVAL[2,6] ,LASCALEVAL[2,7] ,LASCALEVAL[2,8] ,LASCALEVAL[2,9],LASCALEVAL[2,10] ,LASCALEVAL[2,11] ,;
    LASCALEVAL[3,1] ,LASCALEVAL[3,2] ,LASCALEVAL[3,3] ,LASCALEVAL[3,4] ,LASCALEVAL[3,5] ,LASCALEVAL[3,6] ,LASCALEVAL[3,7] ,LASCALEVAL[3,8] ,LASCALEVAL[3,9],LASCALEVAL[3,10] ,LASCALEVAL[3,11] ,;
    (LNTOTSTK1+LNTOTWIP1-LNTOTORD1) ,(LNTOTSTK2+LNTOTWIP2-LNTOTORD2) ,(LNTOTSTK3+LNTOTWIP3-LNTOTORD3),;
    LASCALDESC[1],LASCALDESC[2],LASCALDESC[3],lNGROUP1,lNGROUP2,lNGROUP3,lnGroups ) 




  *-- Init all variables
  STORE 0 TO LASTOCK , LAWIP , LAORD  
  STORE SPACE(0) TO LASCALDIM,LASCALDESC
  STORE SPACE(1) TO LCSTRGRP1,LCSTRGRP2,LCSTRGRP3
  STORE 0 TO LNGROUPS,LNTOTSTK1,LNTOTSTK2,LNTOTSTK3,LNTOTORD1,LNTOTORD2,LNTOTORD3 ,;
    LNTOTWIP1,LNTOTWIP2,LNTOTWIP3,LNGRPDSCLN
  STORE '' TO LaSTRGRP  
  STORE 1 TO LNGROUP1,LNGROUP2,LNGROUP3
  SAVESTYLE  = SUBSTR(STYLE,1,LNMAJLEN)
  SAVECOLOR  = SUBSTR(STYLE,LNCLRPO,6)
  ENDPROC
  *-- End of lpINSERT.
  *!**************************************************************************
  *! Name      : lfCreateWrkCur
  *! Developer : Hassan (HIA)
  *! Date      : 01-FEB-2007
  *! Purpose   : Create cursor for collecting data
  *!**************************************************************************
  *! Called from : ICWEA300.PRG
  *!**************************************************************************
  *! Calls       :
  *!**************************************************************************
  *! Passed Parameters : Temp_name
  *!**************************************************************************
  *! Return      : None
  *!**************************************************************************
  *! Example     : =lfCreateWrkCur(lcTmpFile)
  *!**************************************************************************
PROCEDURE LFCREATEWRKCUR (LCTMPFILENAME AS STRING)

  IF !EMPTY(LCTMPFILENAME)

    LCTMPFILE=ADDBS(OARIAAPPLICATION.WORKDIR)+LCTMPFILENAME+".DBF"

    CREATE TABLE (LCTMPFILE) (CSTYLE C(19), CCOLOR C(19),;
      SCL11 C(7),SCL12 C(7),SCL13 C(7),SCL14 C(7),SCL15 C(7),SCL16 C(7),SCL17 C(7),SCL18 C(7),SCL19 C(7),SCL110 C(7),SCL111 C(7),;
      SCL21 C(7),SCL22 C(7),SCL23 C(7),SCL24 C(7),SCL25 C(7),SCL26 C(7),SCL27 C(7),SCL28 C(7),SCL29 C(7),SCL210 C(7),SCL211 C(7),;
      SCL31 C(7),SCL32 C(7),SCL33 C(7),SCL34 C(7),SCL35 C(7),SCL36 C(7),SCL37 C(7),SCL38 C(7),SCL39 C(7),SCL310 C(7),SCL311 C(7),;
      STK11 N(7),STK12 N(7),STK13 N(7),STK14 N(7),STK15 N(7),STK16 N(7),STK17 N(7),STK18 N(7),STK19 N(7),STK110 N(7),STK111 N(7),;
      STK21 N(7),STK22 N(7),STK23 N(7),STK24 N(7),STK25 N(7),STK26 N(7),STK27 N(7),STK28 N(7),STK29 N(7),STK210 N(7),STK211 N(7),;
      STK31 N(7),STK32 N(7),STK33 N(7),STK34 N(7),STK35 N(7),STK36 N(7),STK37 N(7),STK38 N(7),STK39 N(7),STK310 N(7),STK311 N(7),;
      TOTSTK1 N(15),TOTSTK2 N(15),TOTSTK3 N(15),;
      WIP11 N(7),WIP12 N(7),WIP13 N(7),WIP14 N(7),WIP15 N(7),WIP16 N(7),WIP17 N(7),WIP18 N(7),WIP19 N(7),WIP110 N(7),WIP111 N(7),;
      WIP21 N(7),WIP22 N(7),WIP23 N(7),WIP24 N(7),WIP25 N(7),WIP26 N(7),WIP27 N(7),WIP28 N(7),WIP29 N(7),WIP210 N(7),WIP211 N(7),;
      WIP31 N(7),WIP32 N(7),WIP33 N(7),WIP34 N(7),WIP35 N(7),WIP36 N(7),WIP37 N(7),WIP38 N(7),WIP39 N(7),WIP310 N(7),WIP311 N(7),;
      TOTWIP1 N(15),TOTWIP2 N(15),TOTWIP3 N(15),;
      ORD11 N(7),ORD12 N(7),ORD13 N(7),ORD14 N(7),ORD15 N(7),ORD16 N(7),ORD17 N(7),ORD18 N(7),ORD19 N(7),ORD110 N(7),ORD111 N(7),;
      ORD21 N(7),ORD22 N(7),ORD23 N(7),ORD24 N(7),ORD25 N(7),ORD26 N(7),ORD27 N(7),ORD28 N(7),ORD29 N(7),ORD210 N(7),ORD211 N(7),;
      ORD31 N(7),ORD32 N(7),ORD33 N(7),ORD34 N(7),ORD35 N(7),ORD36 N(7),ORD37 N(7),ORD38 N(7),ORD39 N(7),ORD310 N(7),ORD311 N(7),;
      TOTORD1 N(15),TOTORD2 N(15),TOTORD3 N(15),;
      SCLVL11 N(7),SCLVL12 N(7),SCLVL13 N(7),SCLVL14 N(7),SCLVL15 N(7),SCLVL16 N(7),SCLVL17 N(7),SCLVL18 N(7),SCLVL19 N(7),SCLVL110 N(7),SCLVL111 N(7),;
      SCLVL21 N(7),SCLVL22 N(7),SCLVL23 N(7),SCLVL24 N(7),SCLVL25 N(7),SCLVL26 N(7),SCLVL27 N(7),SCLVL28 N(7),SCLVL29 N(7),SCLVL210 N(7),SCLVL211 N(7),;
      SCLVL31 N(7),SCLVL32 N(7),SCLVL33 N(7),SCLVL34 N(7),SCLVL35 N(7),SCLVL36 N(7),SCLVL37 N(7),SCLVL38 N(7),SCLVL39 N(7),SCLVL310 N(7),SCLVL311 N(7),;
      TOTSCLV1 N(15),TOTSCLV2 N(15),TOTSCLV3 N(15),;
      SCLDSC1 C(19),SCLDSC2 C(19),SCLDSC3 C(19), NGROUP1 N(3), NGROUP2 N(3), NGROUP3 N(3),nGroups N(1))


    DIMENSION LOOGSCROLL.LACRTABLES[1]
    LOOGSCROLL.LACRTABLES[1] =  LCTMPFILENAME
    

  ENDIF
ENDPROC

*!****************************************************************************




















