*:************************************************************************
*: Program file  : GFENDPRN.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
IF _DOS OR lcOGPlatForm='DOS'
  DO CASE
    CASE gcDevice="PRINTER"
      SET PRINTER TO
    CASE gcDevice="FILE"
      SET PRINTER TO    
    CASE gcDevice="SCREEN"
      lcMemWidth=SET('MEMOWIDTH')
      SET MEMOWIDTH TO 230
      SET PRINTER TO 
   IF LASTKEY()<>27
      lcOGWinTitl=IIF(TYPE('lcOGWinTitl')='C',lcOGWinTitl,'Report')
      DO GETFILE WITH gcOutFile ,lcOGWinTitl
   ENDIF      
     SET MEMOWIDTH TO lcMemWidth
     ERASE &gcOutFile.
  ENDCASE  
ENDIF  