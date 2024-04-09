

*!*	   parm1 - "AUTOGRAPH" (required)
*!*	   parm2 - chart type (numeric)
*!*	   parm3 - chart subtype (numeric)
*!*	   parm4 - title (character)
*!*	   parm5 - series by row / column (logical)
*!*	   parm6 - has legend (logical)
*!*	   parm7 - use autoformat (logical)
*!*	   parm8 - output DBF (character)
*!*	   parm9 - don't display graph with MODIFY GENERAL (logical) 
PARAMETERS lcMemoFile  &&,lnGrTyp,lnGrSub,lcTitl,llByRow,lHasLeg
IF TYPE('lcMemoFile')# 'C' &&OR TYPE('lcTmpFile')#'C'
  RETURN
ENDIF
RESTORE FROM (lcMemoFile+'.MEM') ADDI
lcVTitle = IIF(TYPE('lcVTitle')#'C',"",lcVTitle)
_SCREEN.CAPTION = lcVTitle+" Graph"
HIDE MENU _MSYSMENU
_SCREEN.WINDOWSTATE = 2
*_SCREEN.VISIBLE = .F.
*lnGrTyp = IIF(TYPE('lnGrTyp')='C',VAL(lnGrTyp),lnGrTyp)
*lnGrTyp = IIF(TYPE('lnGrTyp')#'N',1,lnGrTyp)
*lnGrSub = IIF(TYPE('lnGrSub')='C',VAL(lnGrSub ),lnGrSub )
*lnGrSub = IIF(TYPE('lnGrSub')#'N',1,lnGrSub)
*lcTitl  = IIF(TYPE('lcTitl')#'C',"",lcTitl)

*llByRow = IIF(TYPE('llByRow')='C',EVAL(llByRow),llByRow)
*llByRow = IIF(TYPE('llByRow')#'L',.T.,llByRow)
*lHasLeg = IIF(TYPE('lHasLeg')='C',EVAL(lHasLeg),lHasLeg)
*lHasLeg = IIF(TYPE('lHasLeg')#'L',.T.,lHasLeg)

lcTmpFile = 'X'+ SUBSTR(SYS(2015),4)
SELECT * FROM (lcVDataF) INTO TABLE (lcVWorkDir+lcTmpFile)
CLOSE DATA
ERASE (lcMemoFile+'.MEM')
DO FORM GRAF WITH lcVWorkDir+lcTmpFile,lcvTitle,lcVWorkDir,lcvAppHome
READ EVENTS
*SELECT datafile
*DO w:\aria27\prgs\wzgraph.app WITH "AUTOGRAPH" ,lnGrTyp,lnGrSub,lcTitl,.t.,.t.,.F.,lcTmpFile,.t.
*USE IN DATAFILE
*CLOSE DATA
*= ConvertTo26(lcTmpFile)
*CLOSE DATA
RETURN
    

FUNCTION ConvertTo26
LPARAMETERS lcFileName
PRIVATE lcFileDir,lcTempName,llFileUsed,lcAlias,lcFileTag,lcError,llError

lcTempName = 'X'+SUBSTR(SYS(2015),4)
IF RAT('.',lcFileName)>0
  lcFileName = SUBSTR(lcFileName,1,RAT('.',lcFileName)-1)
ENDIF  
lcAlias = ALIAS()
lcAlias = IIF(EMPTY(lcAlias),SELECT(),lcAlias)
lcFileDir = IIF(ATC('\',lcFileName)=0,'',SUBSTR(lcFileName,1,RAT('\',lcFileName)))
lcFileName = STRTRAN(lcFileName,lcFileDir)
llFileUsed = USED(lcFileName)
lcFileTag = ''
IF !llFileUsed
  USE (lcFileDir+lcFileName) IN 0
ELSE
  lcFileTag =  TAG(lcFileName)
ENDIF
SELECT (lcFileName)
COPY TO (lcFileDir+lcTempName) TYPE FOX2X WITH CDX
USE IN (lcFileName)
ERASE (lcFileDir+lcFileName+'.DBF')
ERASE (lcFileDir+lcFileName+'.FPT')
ERASE (lcFileDir+lcFileName+'.CDX')
RENAME (lcFileDir+lcTempName+'.DBF') TO (lcFileDir+lcFileName+'.DBF')

*lcError = ON('ERROR')
*llError = .F.
*ON ERROR llError = .T.
IF FILE(lcFileDir+lcTempName+'.FPT')
  RENAME (lcFileDir+lcTempName+'.FPT') TO (lcFileDir+lcFileName+'.FPT')
ENDIF
IF FILE(lcFileDir+lcTempName+'.CDX')
  RENAME (lcFileDir+lcTempName+'.CDX') TO (lcFileDir+lcFileName+'.CDX')
ENDIF
*ON ERROR &lcError

IF llFileUsed
  USE (lcFileDir+lcFileName) IN 0
  IF !EMPTY(lcFileTag)
    SET ORDER TO TAG (lcFileTag) IN (lcFileName)
  ENDIF
ENDIF
SELECT (lcAlias)



FUNCTION lfGenGraf
   #DEFINE cr CHR(13)
   #DEFINE tab CHR(9)

   *USE datafile IN 1
   *USE grphfile IN 2
   SELECT datafile
   cGraphString = ""
   go top
   * Build tab-delimited string of field names:
   FOR iCounter = 1 TO FCOUNT()
      cGraphString = cGraphString  + FIELDS(iCounter) ;
         + IIF(iCounter < FCOUNT(),tab,cr)
   ENDFOR

   * Concatenate the data, converting numeric fields to character:
   SCAN
         FOR iCounter = 1 TO FCOUNT()
         cGraphString = cGraphString  + IIF(TYPE(Fields(iCounter))='C',;
            EVALUATE(FIELDS(iCounter)) ;
            ,str(EVALUATE(FIELDS(iCounter)),16,2);
            );
            + IIF(iCounter < FCOUNT(),tab,cr)
      ENDFOR
   ENDSCAN
*   SET TEXT ON
*   SET TEXT TO C:\HESH.GRA
*   \ <<cGraphString>>
*   SET TEXT TO
*   SET TEXT OFF
   SELECT grphfile
   APPEND GENERAL mgraph DATA cGraphString
* !C:\HESH.GRA

