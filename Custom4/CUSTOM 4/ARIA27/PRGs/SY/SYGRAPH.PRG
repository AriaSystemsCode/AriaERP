*:************************************************************************
*: Program file  : sygraph.PRG
*: Program desc. : PROGRAM TO GENERATE A CHART FOR SPECIFIC DATA
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Hesham El-Sheltawi
*:************************************************************************
*: Calls : 
*:         Procedures : GetGrf.prg
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : lnTypeGrf    graph type (Pie,bar,line) value 1 to 14
*:                      lcFields     fields to create the chart for
*:                      llByRow      by row or by column
*:************************************************************************
*:E300740,1 Hesham El-Sheltawi
PARAMETER lnTypeGrf,lcFields,llRow,lcGraphType,lcGrfDefa,lcGrfGenrl
PRIVATE lnGraph,llByRow,lcGraphFile,llReturn,lcPrompt,lnSlctAlias
lnSlctAlias = SELECT()
llReturn = .F.
lcGraphType = IIF(TYPE('lcGraphType')#'C','',lcGraphType)
lcGrfGenrl = IIF(TYPE('lcGrfGenrl')#'C','',lcGrfGenrl)
lcGrfDefa = IIF(TYPE('lcGrfDefa')#'C','',lcGrfDefa)



*HESHAM (Start)
*DO sygraph.spr

DIMEN laGrfTemp[1,2]
laGrfTemp = ''
lcGraphVr = "SCREEN"
lcGrPhScr = SYS(2015)
lcGrfDisp = SYS(2015)
lcDispGrf = SYS(2015)
lcGrfBtns = SYS(2015)
lcGrfcntr = SYS(2015)
lcGraphFile = 'X'+RIGHT(SYS(2015),7)
llByRow = IIF(TYPE('llRow')#'L',.T.,llRow)
lnGraph = IIF(TYPE('lnTypeGrf')#'N',8,lnTypeGrf)
lcGrfRead = 'WITH '+lcGrPhScr &&+','+(lcGraphFile+'.OLEGRAPH')
lcGraphType = IIF(lcGraphVr='SCREEN',lcGraphType,'')
lcGrfFields = IIF(lcGraphVr='SCREEN',lcFields,'')
lcGraphAlis = IIF(lcGraphVr='SCREEN',ALIAS(),'')
lcGrfGenrl  = IIF(lcGraphVr='SCREEN',lcGrfGenrl,'')
lcGraphType = IIF(TYPE('lcGraphType')#'C','',lcGraphType)
lcGrfGenrl = IIF(TYPE('lcGrfGenrl')#'C','',lcGrfGenrl)  
lcGrfDefa = IIF(TYPE('lcGrfDefa')#'C','',lcGrfDefa)
lcGrfFields = IIF(EMPTY(lcGrfFields),lcGrfDefa,lcGrfFields)

*IF !EMPTY(lcGrfFields)
*  =GETGRF(gcWorkDir+lcGraphFile,lnGraph,lcGrfFields,llByRow)
*ENDIF

IF !EMPTY(lcGrfGenrl)
  =GETGRF(gcWorkDir+lcGraphFile,lnGraph,lcGrfGenrl,llByRow)
ENDIF

*HESHAM (End)
SELECT (lnSlctAlias)

RETURN llReturn


