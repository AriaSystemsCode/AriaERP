*:***************************************************************************
*: Program file  : ARSTMSG.PRG        &E301188.
*: Program desc. : Adding & modifing statement messages.
*: System        : Aria Advantage Series.
*: Module        : Account Rec. (AR)
*: Developer     : Ashraf Sherif Mohammad.
*: Date          : 03/31/99
*:***************************************************************************
*: Calls : 
*:    Procedures : lpShow,
*:    Functions  : gfOpenFile,gfTempName,gfSetup,lfVPDays,lpSavScr,lfRefresh
*:               : lfMsg1,lfMsg2,lfMsg3
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************

llFormC=.F.
=gfOpenFile(gcSysHome+"SYFRMCDH", "FORMCDHD ", "SH")

IF SEEK('ARCSTM')
  llFormC=ALLTRIM(CCURFORM)='C'
ENDIF
IF llFormC
  STORE SPACE(78) TO LCMADL1C, LCMADL2C, LCMADL3C, LCMADL4C, LCMADL5C ,;
                     lcML1C  , lcML2C  , lcML3C  , MADL1   , MADL2    ,;
                     MADL3   , MADL4   , MADL5   , M120L1  , M120L2   ,;
                     M120L3  , M90L1   , M90L2   , M90L3   , M60L1    ,;
                     M60L2   , M60L3   , M30L1   , M30L2   , M30L3
  STORE SPACE(78) TO lcm12l1 , lcm12L2 , lcm12L3 , lcM90L1 , lcM90L2  ,;
                     lcM90L3 , lcM60L1 , lcM60L2 , lcM60L3 , lcM30L1  ,;
                     lcM30L2 , lcM30L3
ELSE
  STORE SPACE(60) TO LCMADL1, LCMADL2, LCMADL3, LCMADL4, LCMADL5,; 
    	             M120L1 , M120L2 , M120L3 , MADL1  , MADL2  ,;
   			 	     MADL3  , MADL4  , MADL5  , M120L1 , M120L2 ,;
  			   	     M120L3 , M90L1  , M90L2  , M90L3  , M60L1  ,;
  				     M60L2  , M60L3  , M30L1  , M30L2  , M30L3  ,;
				     lcML1  , lcML2  , lcML3
  STORE SPACE(60) TO lcm12l1, lcm12L2, lcm12L3, lcM90L1, lcM90L2 ,;
                     lcM90L3, lcM60L1, lcM60L2, lcM60L3, lcM30L1 ,;
                     lcM30L2, lcM30L3

ENDIF

llFound    = .F.
lcDayTitle = ''

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(GCBASEWIND)
  llFound = FILE('&gcDataDir.ARSTMSG.MEM')
  IF llFound 
    RESTORE ADDITIVE FROM (gcDataDir+'ARSTMSG')
    IF llFormC
      LCMADL1C = MADL1
      LCMADL2C = MADL2
      LCMADL3C = MADL3
      LCMADL4C = MADL4
      LCMADL5C = MADL5
      lcML1C   = M120L1
      lcML2C   = M120L2
      lcML3C   = M120L3
    ELSE 
      LCMADL1 = MADL1
      LCMADL2 = MADL2
      LCMADL3 = MADL3
      LCMADL4 = MADL4
      LCMADL5 = MADL5
      lcML1   = M120L1
      lcML2   = M120L2
      lcML3   = M120L3
    ENDIF
    lcM12L1 = M120L1
    lcM12L2 = M120L2
    lcM12L3 = M120L3
    lcM90L1 = M90L1
    lcM90L2 = M90L2
    lcM90L3 = M90L3
    lcM60L1 = M60L1
    lcM60L2 = M60L2
    lcM60L3 = M60L3
    lcM30L1 = M30L1
    lcM30L2 = M30L2
    lcM30L3 = M30L3
  ENDIF
  lcDayTitle = ' Over 120 Days '

  IF !llFound
    DO lpInit
  ENDIF

  laScrMode    = .F.
  laScrMode[2] = .T.		&& View mode
  llNoShow     = .F.		&& execute the show procedure upon entry
  *-- laDefProc[9] is .F. to use a local save procedure
  STORE .F. TO laDefProc[9]
  *-- laCtrStat[8]  && Disable Delete button
  *-- laCtrStat[9]  && Disable Select button
  STORE "DISABLE" TO laCtrStat[8], laCtrStat[9]
ENDIF

PbDays = TRIM(lcDayTitle)

DO (gcScrDir + gcWinAppl + '\ARSTMSG.SPR')

PROCEDURE lpShow

IF laSCrMode[2]
  IF llFound
    DO CASE
      CASE UPPER(ALLTRIM(PbDays)) = 'OVER 120 DAYS'
        lcDays = ALLTRIM(STR(12))
      CASE UPPER(ALLTRIM(PbDays)) = 'OVER 90 DAYS'
        lcDays = ALLTRIM(STR(90))
      CASE UPPER(ALLTRIM(PbDays)) = 'OVER 60 DAYS'
        lcDays = ALLTRIM(STR(60))
      CASE UPPER(ALLTRIM(PbDays)) = 'OVER 30 DAYS'
        lcDays = ALLTRIM(STR(30))
    ENDCASE
    lcMess1='lcM'+lcDays+'L1'
    lcMess2='lcM'+lcDays+'L2'
    lcMess3='lcM'+lcDays+'L3'
    IF llFormC
      LCMADL1C = lcMADL1
      LCMADL2C = lcMADL2
      LCMADL3C = lcMADL3
      LCMADL4C = lcMADL4
      LCMADL5C = lcMADL5
      lcML1C   = &lcMess1
      lcML2C   = &lcMess2
      lcML3C   = &lcMess3
      
    ELSE 
      LCMADL1 = lcMADL1
      LCMADL2 = lcMADL2
      LCMADL3 = lcMADL3
      LCMADL4 = lcMADL4
      LCMADL5 = lcMADL5
      lcML1   = &lcMess1
      lcML2   = &lcMess2
      lcML3   = &lcMess3
    ENDIF
    llFound = .T. 
  ELSE
    DO lpInit
    llFound = .F.
  ENDIF
ENDIF
*-- Disable Delete buttons always
SHOW GET pbDlt  DISABLE
SHOW GET pbSlct DISABLE
SHOW GET PbDays ENABLE

FUNCTION lfVPDays
lcDayTitle = ' '+ALLTRIM(PbDays)+' ' 
IF llFound
  DO CASE
    CASE UPPER(PbDays) = 'OVER 120 DAYS'
      IF llFormC
        lcML1C = lcM12L1
        lcML2C = lcM12L2
        lcML3C = lcM12L3
      ELSE
        lcML1 = lcM12L1
        lcML2 = lcM12L2
        lcML3 = lcM12L3
      ENDIF

    CASE UPPER(PbDays) = 'OVER 90 DAYS'
      IF llFormC
        lcML1C = lcM90L1
        lcML2C = lcM90L2
        lcML3C = lcM90L3
      ELSE
        lcML1 = lcM90L1
        lcML2 = lcM90L2
        lcML3 = lcM90L3
      ENDIF
    
    CASE UPPER(PbDays) = 'OVER 60 DAYS'
      IF llFormC
        lcML1C = lcM60L1
        lcML2C = lcM60L2
        lcML3C = lcM60L3
      ELSE
        lcML1 = lcM60L1
        lcML2 = lcM60L2
        lcML3 = lcM60L3
      ENDIF 

    CASE UPPER(PbDays) = 'OVER 30 DAYS'
      IF llFormC
        lcML1C = lcM30L1
        lcML2C = lcM30L2
        lcML3C = lcM30L3
      ELSE
        lcML1 = lcM30L1
        lcML2 = lcM30L2
        lcML3 = lcM30L3
      ENDIF
  ENDCASE
ELSE
  IF llFormC
    STORE SPACE(78) TO lcML1C,lcML2C,lcML3C  
  ELSE
    STORE SPACE(60) TO lcML1,lcML2,lcML3  
  ENDIF
ENDIF  
=lfRefresh()
IF llFormC
  SHOW GET lcML1C
  SHOW GET lcML2C
  SHOW GET lcML3C
ELSE
  SHOW GET lcML1
  SHOW GET lcML2
  SHOW GET lcML3
ENDIF  

PROCEDURE lpSavScr
MADL1 = lcMADL1
MADL2 = lcMADL2
MADL3 = lcMADL3
MADL4 = lcMADL4
MADL5 = lcMADL5
M120L1= lcm12l1
M120L2= lcm12L2
M120L3= lcm12L3
M90L1 = lcM90L1
M90L2 = lcM90L2
M90L3 = lcM90L3
M60L1 = lcM60L1
M60L2 = lcM60L2
M60L3 = lcM60L3
M30L1 = lcM30L1
M30L2 = lcM30L2
M30L3 = lcM30L3
SAVE ALL LIKE M* TO (gcDataDir+'ARSTMSG')


FUNCTION lfMsg1
DO CASE
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 120 DAYS'
    lcm12l1 = IIF(llFormC,lcML1C,lcML1)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 90 DAYS'
    lcM90L1 = IIF(llFormC,lcML1C,lcML1)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 60 DAYS'
    lcM60L1 = IIF(llFormC,lcML1C,lcML1)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 30 DAYS'
    lcM30L1 = IIF(llFormC,lcML1C,lcML1)
ENDCASE

FUNCTION lfMsg2
DO CASE
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 120 DAYS'
    lcm12L2 = IIF(llFormC,lcML2C,lcML2)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 90 DAYS'
    lcM90L2 = IIF(llFormC,lcML2C,lcML2)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 60 DAYS'
    lcM60L2 = IIF(llFormC,lcML2C,lcML2)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 30 DAYS'
    lcM30L2 = IIF(llFormC,lcML2C,lcML2)
ENDCASE

FUNCTION lfMsg3
DO CASE
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 120 DAYS'
    lcm12L3 = IIF(llFormC,lcML3C,lcML3)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 90 DAYS'
    lcM90L3 = IIF(llFormC,lcML3C,lcML3)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 60 DAYS'
    lcM60L3 = IIF(llFormC,lcML3C,lcML3)
 CASE UPPER(ALLTRIM(PbDays)) = 'OVER 30 DAYS'
    lcM30L3 = IIF(llFormC,lcML3C,lcML3)
ENDCASE

PROCEDURE lpInit
IF llFormC
  STORE SPACE(78) TO LCMADL1C, LCMADL2C, LCMADL3C, LCMADL4C, LCMADL5C ,;
                     lcML1C  , lcML2C  , lcML3C  , MADL1   , MADL2    ,;
                     MADL3   , MADL4   , MADL5   , M120L1  , M120L2   ,;
                     M120L3  , M90L1   , M90L2   , M90L3   , M60L1    ,;
                     M60L2   , M60L3   , M30L1   , M30L2   , M30L3
  STORE SPACE(78) TO lcm12l1, lcm12L2, lcm12L3, lcM90L1 , lcM90L2  ,;
                     lcM90L3 , lcM60L1 , lcM60L2 , lcM60L3 , lcM30L1  ,;
                     lcM30L2 , lcM30L3

ELSE
  STORE SPACE(60) TO LCMADL1, LCMADL2, LCMADL3, LCMADL4, LCMADL5,; 
    	             M120L1 , M120L2 , M120L3 , MADL1  , MADL2  ,;
   			 	     MADL3  , MADL4  , MADL5  , M120L1 , M120L2 ,;
  			   	     M120L3 , M90L1  , M90L2  , M90L3  , M60L1  ,;
  				     M60L2  , M60L3  , M30L1  , M30L2  , M30L3  ,;
				     lcML1  , lcML2  , lcML3
  STORE SPACE(60) TO lcm12L1, lcm12L2, lcm12L3, lcM90L1 , lcM90L2  ,;
                     lcM90L3 , lcM60L1 , lcM60L2 , lcM60L3 , lcM30L1  ,;
                     lcM30L2 , lcM30L3

ENDIF
