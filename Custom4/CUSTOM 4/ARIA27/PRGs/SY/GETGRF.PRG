*:************************************************************************
*: Program file  : GETGRF.PRG
*: Program desc. : PROGRAM TO GENERATE A CHART FOR SPECIFIC DATA
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Hesham El-Sheltawi
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : gfSubStr()
*:************************************************************************
*: Passed Parameters  : lcTempCurs   Temprory cursor to add the graph in
*:                      lnGraphType  graph type (Pie,bar,line) value 1 to 14
*:                      lcFields     fields to create the chart for
*:                      llByRow      by row or by column
*:************************************************************************
*:E300740,1 Hesham El-Sheltawi
PARAMETERS lcTempCurs,lnGraphType,lcFields,llByRow
PRIVATE lnSelect,lnGraphType,IBTNROW1,IBTNROW2,G_DATAALIA,G_GENGRAPH,;
        FSERIESINC,FALLDONE,CLABELFIEL,CLEGENDFIE,ASLEGENDFI,ASLABELFIE,G_TITLE,;
        G_TEMPGRPH,llByArray,lcCursor,lcTitleType,laFields,lnCount,lcSafety,;
        lcLegFields,lcProcedure,lcTempCurs


IF TYPE('lcFields')='U'
  WAIT 'No data defined to make a chart for' window nowait
  RETURN .F.
ENDIF
lcTempCurs = UPPER(lcTempCurs)
lnSelect = SELECT()


lnGraphType = IIF(TYPE('lnGraphType')#'N',1,lnGraphType)
G_TEMPGRPH  = SYS(3)
llByRow     = IIF(TYPE('llByRow')#'L',.F.,llByRow)

IF !BETWEEN(lnGraphType,1,14)
  lnGraphType = 1
ENDIF

IF lnGraphType > 12
  llByRow = .F.
ENDIF
IBTNROW1       =   lnGraphType
IBTNROW2       =   0

llByArray = IIF(TYPE('lcFields[1,1]')='U',.F.,.T.)
lcCursor = 'X'+RIGHT(SYS(2015),7)
IF llByArray
  PRIVATE lnField,lcFieldStr


  lcTitleType = TYPE('lcFields[1,1]')
  DIMEN laFields[ALEN(lcFields,2),4]
  laFields[1,1] = 'Title'
  laFields[1,2] = lcTitleType
  laFields[1,3] = IIF(lcTitleType='D',8,LEN(lcFields[1,1]))
  laFields[1,4] = 0
  lcFieldStr = laFields[1,1]
  
  FOR lnCount = 2 TO  ALEN(laFields,1)
    laFields[lnCount,1] = 'Value'+ALLT(STR(lnCount))
    laFields[lnCount,2] = 'N'
    laFields[lnCount,3] = 10
    laFields[lnCount,4] = 0
    lcFieldStr = lcFieldStr+IIF(EMPTY(lcFieldStr),'',',')+laFields[lnCount,1]    
  ENDFOR
  CREATE DBF (gcWorkDir+lcCursor) FROM ARRAY laFields
  FOR lnCount = 1 TO ALEN(lcFields,1)
    FOR lnField = 1 TO ALEN(lcFields,2)
      IF lnField = 1
        APPEND BLANK
      ENDIF  
      REPLACE &laFields[lnField,1] WITH lcFields[lnCount,lnField]
    ENDFOR
  ENDFOR
  lcLegFields = lcFieldStr
ELSE
  Private lcMastAlis
  lcMastAlis = ALIAS()
  SELECT &lcFields FROM (lcMastAlis) INTO DBF (gcWorkDir+lcCursor)
  lcFields = FIELD(1)
  FOR lnCount = 2 TO FCOUNT()
  lcFields = lcFields +','+ FIELD(lnCount)
  ENDFOR
  lcLegFields = lcFields  
ENDIF

IF USED(lcCursor)
  USE IN (lcCursor)
ENDIF
* lnGrTyp,lnGrSub,lcTitl,llByRow,lHasLeg
CREATE TABLE (lcTempCurs) (OLEGRAPH G)
USE 
lcMemFile = gfTempName()
lcvDataF = gcWorkDir+lcCursor
lcvWorkDir = gcWorkDir
lcvAppHome = gcAppHome
lcvTitle = ""
SAVE TO (gcWorkDir+lcMemFile+'.MEM') ALL LIKE lcv*
lcCommLine = "-C"+gcDef_path+"ARIA.FPW "+gcWorkDir+lcMemFile
*gcWorkDir+lcCursor+".DBF "+lcTempCurs + ".DBF " + ALLT(STR(lnGraphType))+" 1 "+IIF(llByRow,'.T.','.F.')+" .T."
lcVTitle = "Graph"
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 1
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")

  *BADRAN 01/11/2001 Change the graph path to be at the root of aria27 [Begin]
  *=EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",GCAPPHOME+[GRAF.EXE ]+lcCommLine,"+STR(SW_Hide)+")")

  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",GCDEF_PATH+[GRAF.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  *BADRAN 01/11/2001 Change the graph path to be at the root of aria27 [End  ]

   RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")


ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF   

*-Ram
*WAIT TIMEOUT 4

*G_DATAALIA = ALIAS() 
*G_GENGRAPH = "GENGRAPH.DBF"
*FALLDONE   = .F.
*FSERIESINC = !llByRow


*DIMEN laFields[1]
*=gfSubStr(lcLegFields,@laFields)
*G_TITLE = laFields[1]

*FOR lnCount = 1 TO ALEN(laFields,1)
*  IF TYPE(laFields[lnCount]) $ 'CD'
*    G_TITLE = laFields[lnCount]
*    =Adel(laFields,lnCount)
*    DIMEN laFields[ALEN(laFields,1)-1]
*    EXIT
*  ENDIF
*ENDFOR

*IF llByRow
*  DIMEN ASLABELFIE[ALEN(laFields,1)]   && Hold the label field
*  =ACOPY(laFields,ASLABELFIE)
*  DIMEN ASLEGENDFI[1]
*  ASLEGENDFI(1) = G_TITLE
*ELSE
*  DIMEN ASLABELFIE[1]   && Hold the label field
*  ASLABELFIE(1) = G_TITLE
*  DIMEN ASLEGENDFI[ALEN(laFields,1)]
*  =ACOPY(laFields,ASLEGENDFI)
*ENDIF

*CLABELFIEL         =   ALEN(ASLABELFIE)
*CLEGENDFIE         =   ALEN(ASLEGENDFI)  && this var change the leg & num of char. bars




*lcProcedure = SET('PROCEDURE')
*SET PROC TO (_genGraph)
*LL=.F.
*ON ERROR &&LL=.T.
*lcSafety = SET('SAFETY')
*SET SAFETY OFF
*USE (G_GENGRAPH) IN 0
*IF EMPTY(ASLABELFIE)
*  G_GRAPHTYP = lnGraphType
*  DO DOSAMPGRPH
*ELSE
*  DO GDISPS2 IN GW 
*ENDIF
*USE IN GENGRAPH
*SET PROCEDURE TO &lcProcedure
**IF llByArray
*   USE IN (lcCursor)
**ENDIF

PRIVATE lcAliasName
lcAliasName = lcTempCurs
IF ATC('\',lcTempCurs)>0
  lcAliasName = STRTRAN(SUBSTR(lcTempCurs,RAT('\',lcTempCurs)+1),'.DBF')
ENDIF
IF USED(lcAliasName)
  USE IN (lcAliasName)
ENDIF

*COPY TO (lcTempCurs) 
*SET SAFETY &lcSafety
*USE IN TEMPGRPH
*G_TEMPGRPH = STRTRAN(UPPER(G_TEMPGRPH),'.DBF')
*ERASE (G_TEMPGRPH+'.DBF')
*ERASE (G_TEMPGRPH+'.FPT')
USE (lcTempCurs) IN 0
SELECT (lnSelect)
