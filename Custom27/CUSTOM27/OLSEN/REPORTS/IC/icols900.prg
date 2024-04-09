*:**************************************************************************
*: Program file  : ICOLS900
*: Program desc. : PRINT STYLE STOCK PROGRAM FOR (OLSEN)
*: Date          : 07/25/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:                    : lpAddStDye
*:                    : lpCompFotr
*:                    : gfDispRep 
*:             
*:         Functions  : lfwRepWhen()
*:                    : lfClearRep()
*:                    : lfUseScale()
*:                    : lfGetScale()
*:                    : lfAssignSc()
*:                    : lfScalePgH()
*:                    : lfEndOfRep()
*:                    : lfCurrComp()
*:                    : lfNonMaj()
*:                    : lfSRVSty()
*:                    : lfStySum()
*:                    : lfvCompany()
*:                    : lfCreatFil()
*:                    : lfCmpExpr()
*:                    : lfItmPos() 
*:                    : gfMover() 
*:                    : gfModalGen()
*:                    : gfItemMask()
*:                    : gfStyBrw()
*:**************************************************************************
*: Example : DO ICOLS900
*:**************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR OLSEN (101583) 
*:**************************************************************************
*:C101583 
lcStTime = TIME()
*-- lcCmpDir   : Company directory.
*-- lcCompName : Company Full Name
*-- lcOldScale : to store Old Scale
*-- llEndOfRep : logical to detect the end of report
llEndOfRep = .F.
STORE '' TO lcCmpDir , lcCompName , lcOldScale

*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.

IF llOGFltCh         && Flag to check if user changed criteria or not
  *-- if the temp. file is used and it's not empty this means that the report
  *-- will erase the data filled inside it and re-create it again for the new
  *-- filter expression 

  *-- If temp file is used and has records inside it [Begin]
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    = lfCreatFil()          && to create the temp file     
  ENDIF
  *-- If temp file is used and has records inside it [End]
  lcLstCmpEx = lcRpCmpExp       && store the Company ID Exp in another var.

  lcColorExp = ''         
  *-- lcStyFile  : carry the name of temp file created in STYLE (INLIST)
  lcStyFile  = laOGFxFlt[lnStyPos,6] 
  *-- If Color Fixed Filter position empty 
  IF EMPTY(laOGFxFlt[lnClrSgPos,6])
    *-- If Free Fixed Filter position empty not empty
    IF !EMPTY(laOGFxFlt[lnFreSgPos,6])
      lcColorExp  = "&laOGFxFlt[lnFreSgPos,1]." + ' $ laOGFxFlt[lnFreSgPos,6]'
    ENDIF
  ELSE   && Color Fixed Filter position is not empty
    lcColorExp  = "&laOGFxFlt[lnClrSgPos,1]." + ' $ laOGFxFlt[lnClrSgPos,6]'
  ENDIF 
  lcColorExp = STRTRAN(lcColorExp,"STYLE.","")

  DO lpCollData             && to get the company info  
ENDIF        
*-- Endif of user changed criteria or not

*--  if Temp File is empty display message 
IF RECCOUNT(lcWorkFile) = 0      
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE          && Temp File contains data  
  SELECT (lcWorkFile)
  DO gfDispRe WITH EVALUATE('lcRpName')
ENDIF

*--  Endif of Temp File is empty  
****************************************************************************
*************************** *-- End of Report--* ***************************
****************************************************************************


*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfOGShowGet()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
*-- Collect data about all companies (Comp ID , CompName , CompDir) and fill it in ARRAY [Begin.]
IF EMPTY(laRpCmpCod)
  SET ORDER TO Ccomp_id IN SYCCOMP
  DECLARE laRpCmpCod[1,2]

  STORE '' TO lcRpCmpExp
  *-- Collect all companies
  SELECT cComp_ID + " - " + cCom_Name,cCom_dDir ;
    FROM SYCCOMP                            ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
  
  *DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[1,1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    *STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1]
  ENDFOR

  *-- make the target in the mover set to default company (active company)
  laRpTarCmp[1,1] = laRpcmpCod[ASCAN(laRpSorCmp,gcAct_Comp,1),1]

ENDIF
*-- Collect data about all companies (Comp ID , CompName , CompDir) and fill it in ARRAY [End.]
= lfCmpExpr()                  && check if Selected companies in the target array is changed         
= lfCreatFil()                 && to create the temp file 

*-- these variables to detect the position of STYLE and COLOR in Fixed Filter [Begin]
lnStyPos   = lfItmPos('STYLE.STYLE')
lnClrSgPos = lfItmPos('SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')
lnFreSgPos = lnClrSgPos + 1
*-- these variables to detect the position of STYLE and COLOR in Fixed Filter [End]

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

*!**************************************************************************
*! Name      : lfCmpExpr
*! Developer : Sameh (SSE)
*! Date      : 08/05/1999
*! Purpose   : Evaluate Company expression.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : lfvCompany,lfwRepWhen
*!**************************************************************************
*! Passed Parameters  : ....
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfCmpExpr()
*!**************************************************************************
FUNCTION lfCmpExpr
PRIVATE laTarget
IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF
  
= ASORT(laTarget)
lcRpCmpExp = ''

FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR
llOGFltCh = llOGFltCh OR !(lcLstCmpEx == lcRpCmpExp)
*-- End of lfCmpExpr.


*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
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
*! Name      : lfSRVSty
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
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
*! Example   : =lfSRVSty()
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
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- End of lfSRVSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
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


*!**************************************************************************
*! Name      : lfvCompany
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
*! Purpose   : Call mover function then Evaluate Company expression 
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover , lfCmpExpr 
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Passed Parameters  : ....
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfvCompany()
*!**************************************************************************
FUNCTION lfvCompany
= gfMover(@laRpSorCmp,@laRpTarCmp,'Select Company',.T.,'')  && call mover function.
= lfCmpExpr()
*-- End of lfvCompany.

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
*! Purpose   : Create temporary File structure.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!**************************************************************************
*! Called from : OG When function.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
DIMENSION laTempFile[13,4] , laTempStru[1,4]
STORE '' TO laTempFile , laTempStru
PRIVATE lnFileCnt , lnFldRow

*-- creating Fields of Temp File [Begin.]
*-- first adding the Comp_ID structure from SYCCOMP File
laTempFile[1,1]  = 'CCOMP_ID'
laTempFile[1,2]  = 'C'
laTempFile[1,3]  = 2
laTempFile[1,4]  = 0

*-- second adding some fields from STYDYE File 
SELECT STYDYE 
=AFIELDS(laTempStru)  && copy all File fields and types to this array
laTempFile[2,1]  = 'STYLE'
laTempFile[3,1]  = 'CWARECODE'
laTempFile[4,1]  = 'STK1'
laTempFile[5,1]  = 'STK2'
laTempFile[6,1]  = 'STK3'
laTempFile[7,1]  = 'STK4'
laTempFile[8,1]  = 'STK5'
laTempFile[9,1]  = 'STK6'
laTempFile[10,1] = 'STK7'
laTempFile[11,1] = 'STK8'
laTempFile[12,1] = 'TOTSTK'

*-- Loop to get other dimensions of STYDYE fields 
FOR lnFileCnt = 2 TO 12
  lnFldRow = ASCAN(laTempStru,laTempFile[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempStru,lnFldRow,1)
    laTempFile[lnFileCnt , 2 ] = laTempStru[lnFldRow , 2 ]
    laTempFile[lnFileCnt , 3 ] = laTempStru[lnFldRow , 3 ]
    laTempFile[lnFileCnt , 4 ] = laTempStru[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of STYDYE fields

*-- third adding the SCALE structure from STYLE File
laTempFile[13,1] = 'SCALE'
laTempFile[13,2] = 'C'
laTempFile[13,3] = 3
laTempFile[13,4] = 0

RELEASE laTempStru     && not used anymore so there's no need to keep it
CREATE TABLE (gcWorkDir+lcWorkFile) FROM ARRAY laTempFile   && create Temp File
*-- Create the Index expression on Temp File [Begin.]
INDEX ON cComp_ID+STYLE+CWARECODE TAG &lcWorkFile 
*-- Create the Index expression on Temp File [End.]  
*-- creating Fields of Temp File [End.]
*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
*! Purpose   : Close any opened files if user press OG <Close> Button
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : ....
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*!
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run 
llOGFltCh = .T.  

IF USED('SCALE')               && close Scale File
  USE IN SCALE
ENDIF

IF USED(lcWorkFile)            && close the Temp File
  USE IN (lcWorkFile)
ENDIF
ERASE &gcWorkDir.&lcWorkFile+'.DBF'    && erase the DBF File
IF FILE (gcWorkDir+lcWorkFile+".CDX")
  ERASE (gcWorkDir+lcWorkFile+".CDX")
ENDIF  
RETURN
*-- End of lfClearRep.

*!**************************************************************************
*! Name      : lpAddStDye
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
*! Purpose   : Fills temporary file with data.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!**************************************************************************
*! Called from : 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : lpAddStDye
*!**************************************************************************
*
PROCEDURE lpAddStDye
PARAMETERS lcScanVal
SELECT (lcStyDyAls)
*-- Scan with Full index expression (RushMore)
SCAN FOR Style + cWareCode + Dyelot = lcScanVal
  IF EMPTY(Dyelot) AND (EMPTY(lcColorExp) OR EVALUATE(lcColorExp)) AND ;
     TotStk > 0
    WAIT WINDOW "Insert " + lcMajTtl + " : " + Style +;
                " From Company " + lcCompID NOWAIT
    SCATTER MEMVAR
    m.cComp_ID = lcCompID
    m.Scale    = &lcStyleAls..SCALE
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF
ENDSCAN
*-- End of lpAddStDye.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh (SSE)
*! Date      : 07/25/99
*! Purpose   : Get Company info to be stored in Variables
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!**************************************************************************
*! Called from : 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
*-- lnInSource : Position of item in source array.
*-- lnComp     : Counter to companies.
*-- lcCompID   : Company ID

PRIVATE lnComp , lcCompID , lnInSource , llUsrRange
STORE 0 TO lnComp , lnInSource
llUsrRange = !EMPTY(lcStyFile) AND USED(lcStyFile) AND RECCOUNT(lcStyFile) > 0

*-- get Company Position from the array in order to get company info. [Begin.] 
FOR lnComp = 1 TO IIF(EMPTY(laRpTarCmp),ALEN(laRpSorCmp,1),ALEN(laRpTarCmp,1))
  lnInSource  = IIF(EMPTY(laRpTarCmp),lnComp,ASCAN(laRpSorCmp,"\"+laRpTarCmp[lnComp],1))
  IF lnInSource = 0
    lnInSource  = ASCAN(laRpSorCmp,laRpTarCmp[lnComp],1)
  ENDIF    
  *-- variables to store Company information (CompID , CompName , CompDir)
  lcCompID = PADR(laRpCmpCod[lnInSource,1],2)     && 
  lcCmpDir = LOWER(ALLTRIM(laRpCmpCod[lnInSource,2]))
  
  *-- If current company have these 3 files [Begin]
  IF FILE(lcCmpDir+'STYDYE.DBF') AND FILE(lcCmpDir+'STYLE.DBF') AND ;
     FILE(lcCmpDir+'SCALE.DBF')      
    USE lcCmpDir+'STYDYE' AGAIN IN 0 ALIAS (lcStyDyAls)
    USE lcCmpDir+'STYLE'  AGAIN IN 0 ALIAS (lcStyleAls) ORDER TAG STYLE

    SELECT (lcStyDyAls)
    *-- Make relation between STYDYE & STYLE to collect data from both files.
    SET RELATION TO STYLE INTO (lcStyleAls) 
  ELSE
    WAIT WINDOW "IC Module not installed for company " +;
                laRpCmpCod[lnInSource,1] TIMEOUT 2
    LOOP             
  ENDIF  

  *-- if user select in range data
  IF llUsrRange
	SELECT (lcStyFile)
	SCAN  	
      DO lpAddStDye WITH PADR(cStyMajor,lnMajLen) && Add Record(s) for current style.
    ENDSCAN  
  ELSE  && else in Range was empty.
    DO lpAddStDye WITH ''          && Add Record(s) for all styles.
  ENDIF  
  WAIT CLEAR

  IF USED(lcStyDyAls)
    USE IN (lcStyDyAls)      && Select STYDYE to close it first    
  ENDIF
  
  IF USED(lcStyleAls)
    USE IN (lcStyleAls)       && Select STYDYE to close it first    
  ENDIF

ENDFOR
*-- get Company Position from the array in order to get company info. [End.] 
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lfCurrComp
*! Developer : Sameh (SSE)
*! Date      : 08/11/99
*! Purpose   : Get Company info for current comp. to be stored in Variables
*!**************************************************************************
*! Example   : =lfCurrComp()
*!**************************************************************************
*
FUNCTION lfCurrComp
IF !EMPTY(cComp_Id)
  lnCompNo = ASCAN(laRpCmpCod,cComp_Id ,1)           
  *-- if lnCompNo greater than one (found in the array)
  IF lnCompNo > 0  
    lnCompNo   = ASUBSCRIPT(laRpCmpCod,lnCompNo,1)
    lcCompName = ALLTRIM(laRpCmpCod[lnCompNo,1])       && var to hold comp. name
    lcCmpDir   = ALLTRIM(laRpCmpCod[lnCompNo,2])
  ENDIF
  =lfUseScale()
ENDIF  
RETURN ''
*-- End of lfCurrComp.

*!**************************************************************************
*! Name      : lfUseScale
*! Developer : Sameh (SSE)
*! Date      : 08/11/99
*! Purpose   : to open the Scale File for the current Company
*!**************************************************************************
*! Example   : =lfUseScale()
*!**************************************************************************
*
FUNCTION lfUseScale
*-- to close the previous scale file opened from another company
IF USED('SCALE')
  USE IN SCALE
ENDIF
*-- to open the Scale File for the current Company
USE (lcCmpDir+'SCALE') IN 0 ORDER TAG SCALE    
*-- End of lfUseScale.

*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : Sameh (SSE)
*! Date      : 08/11/99
*! Purpose   : to empty lcOldScale var. in each Page Header Band in  
*!             order to be printed once at the start of the Page if 
*!             the Scale is not changed 
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : = lfScalePgH()
*!**************************************************************************
*
FUNCTION lfScalePgH
lcOldScale = ''
RETURN ''
*-- End of lfScalePgH.

*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Sameh (SSE)
*! Date      : 08/11/99
*! Purpose   : to seek for 
*!             
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : = lfGetScale()
*!**************************************************************************
*
FUNCTION lfGetScale
=SEEK(SCALE,'SCALE')
RETURN ''
*-- End of lfGetScale.

*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : Sameh (SSE)
*! Date      : 08/11/99
*! Purpose   : to save the current Scale after printing it in order not to 
*!             print it except when Scale changes 
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : = lfAssignSc()
*!**************************************************************************
*
FUNCTION lfAssignSc
lcOldScale = Scale
RETURN ''
*-- End of lfAssignSc.

*!**************************************************************************
*! Name      : lfEndOfRep
*! Developer : Sameh (SSE)
*! Date      : 08/11/99
*! Purpose   : to detect the end of file
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : = lfEndOfRep()
*!**************************************************************************
*
FUNCTION lfEndOfRep
llEndOfRep = .T.
RETURN ''
*-- End of lfEndOfRep.
