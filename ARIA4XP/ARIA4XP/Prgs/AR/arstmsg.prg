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
*: Converted to Aria4XP by AYM on 01/16/2007
*: T20060908.0009   - N000577
*:***************************************************************************


DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleId+"\ARSTMSG") 

FUNCTION  lpShow
PARAMETERS loFormSet,lcModeToChange

IF lcModeToChange = 'V'
  IF loFormSet.llFound
    DO CASE
      CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 120 DAYS'
        lcDays = ALLTRIM(STR(12))
      CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 90 DAYS'
        lcDays = ALLTRIM(STR(90))
      CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 60 DAYS'
        lcDays = ALLTRIM(STR(60))
      CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 30 DAYS'
        lcDays = ALLTRIM(STR(30))
    ENDCASE
    lcMess1='loFormSet.lcm'+lcDays+'L1'
    lcMess2='loFormSet.lcm'+lcDays+'L2'
    lcMess3='loFormSet.lcm'+lcDays+'L3'
    IF loFormSet.llFormC
      loFormSet.ariaform1.txtMADL1C.value= loFormSet.lcMADL1
      loFormSet.ariaform1.txtMADL2C.value= loFormSet.lcMADL2
      loFormSet.ariaform1.txtMADL3C.value= loFormSet.lcMADL3
      loFormSet.ariaform1.txtMADL4C.value= loFormSet.lcMADL4
      loFormSet.ariaform1.txtMADL5C.value= loFormSet.lcMADL5
      loFormSet.ariaform1.txtML1C.value= &lcMess1
      loFormSet.ariaform1.txtML2C.value= &lcMess2
      loFormSet.ariaform1.txtML3C.value= &lcMess3
    ELSE 
      loFormSet.ariaform1.txtMADL1.value= loFormSet.lcMADL1
      loFormSet.ariaform1.txtMADL2.value= loFormSet.lcMADL2
      loFormSet.ariaform1.txtMADL3.value= loFormSet.lcMADL3
      loFormSet.ariaform1.txtMADL4.value= loFormSet.lcMADL4
      loFormSet.ariaform1.txtMADL5.value= loFormSet.lcMADL5
      loFormSet.ariaform1.txtML1.value   = &lcMess1
      loFormSet.ariaform1.txtML2.value   = &lcMess2
      loFormSet.ariaform1.txtML3.value   = &lcMess3
    ENDIF
    loFormSet.llFound = .T. 
   
  ELSE
    loFormSet.Init()
    loFormSet.llFound = .F.
  ENDIF
ENDIF



FUNCTION lfVPDays
PARAMETERS loFormSet

IF !loFormSet.llFormC
  DO case
    CASE loFormSet.ariaform1.cmbdays1.Value=1
       loFormSet.PbDays='Over 120 Days' 
    CASE loFormSet.ariaform1.cmbdays1.Value=2
       loFormSet.PbDays='Over 90 Days' 
    CASE loFormSet.ariaform1.cmbdays1.Value=3
       loFormSet.PbDays='Over 60 Days' 
    CASE loFormSet.ariaform1.cmbdays1.Value=4
       loFormSet.PbDays='Over 30 Days'    
  ENDCASE      
ELSE
  DO case
    CASE loFormSet.ariaform1.cmbdays2.Value=1
       loFormSet.PbDays='Over 120 Days' 
    CASE loFormSet.ariaform1.cmbdays2.Value=2
       loFormSet.PbDays='Over 90 Days' 
    CASE loFormSet.ariaform1.cmbdays2.Value=3
       loFormSet.PbDays='Over 60 Days' 
    CASE loFormSet.ariaform1.cmbdays2.Value=4
       loFormSet.PbDays='Over 30 Days'    
  ENDCASE  
ENDIF   
 
loFormSet.lcDayTitle = ' '+ALLTRIM(loFormSet.PbDays)+' ' 

IF loFormSet.llFound
  DO CASE
    CASE UPPER(loFormSet.PbDays) = 'OVER 120 DAYS'
      IF loFormSet.llFormC
        loFormSet.ariaform1.txtML1C.value = loFormSet.lcm12L1
        loFormSet.ariaform1.txtML2C.value = loFormSet.lcm12L2
        loFormSet.ariaform1.txtML3C.value = loFormSet.lcm12L3
      ELSE
        loFormSet.ariaform1.txtML1.value = loFormSet.lcm12L1
        loFormSet.ariaform1.txtML2.value = loFormSet.lcm12L2
        loFormSet.ariaform1.txtML3.value = loFormSet.lcm12L3
      ENDIF

    CASE UPPER(loFormSet.PbDays) = 'OVER 90 DAYS'
      IF loFormSet.llFormC
        loFormSet.ariaform1.txtML1C.value = loFormSet.lcm90L1
        loFormSet.ariaform1.txtML2C.value = loFormSet.lcm90L2
        loFormSet.ariaform1.txtML3C.value = loFormSet.lcm90L3
      ELSE
        loFormSet.ariaform1.txtML1.value = loFormSet.lcm90L1
        loFormSet.ariaform1.txtML2.value = loFormSet.lcm90L2
        loFormSet.ariaform1.txtML3.value = loFormSet.lcm90L3
      ENDIF

    
    CASE UPPER(loFormSet.PbDays) = 'OVER 60 DAYS'
      IF loFormSet.llFormC
        loFormSet.ariaform1.txtML1C.value = loFormSet.lcm60L1
        loFormSet.ariaform1.txtML2C.value = loFormSet.lcm60L2
        loFormSet.ariaform1.txtML3C.value = loFormSet.lcm60L3
      ELSE
        loFormSet.ariaform1.txtML1.value = loFormSet.lcm60L1
        loFormSet.ariaform1.txtML2.value = loFormSet.lcm60L2
        loFormSet.ariaform1.txtML3.value = loFormSet.lcm60L3
      ENDIF


    CASE UPPER(loFormSet.PbDays) = 'OVER 30 DAYS'
      IF loFormSet.llFormC
        loFormSet.ariaform1.txtML1C.value = loFormSet.lcm30L1
        loFormSet.ariaform1.txtML2C.value = loFormSet.lcm30L2
        loFormSet.ariaform1.txtML3C.value = loFormSet.lcm30L3
      ELSE
        loFormSet.ariaform1.txtML1.value = loFormSet.lcm30L1
        loFormSet.ariaform1.txtML2.value = loFormSet.lcm30L2
        loFormSet.ariaform1.txtML3.value = loFormSet.lcm30L3
      ENDIF

  ENDCASE
  
 
ENDIF  
loFormSet.ariaform1.LBLCL2.caption     = ALLTRIM(loFormSet.pbdays)
loFormSet.ariaform1.LBLNOCL1.caption   = ALLTRIM(loFormSet.pbdays)




PROCEDURE lpSavScr
PARAMETERS loFormSet
IF loFormSet.llFormC
	loFormSet.lcMADL1=ALLTRIM(loFormSet.ariaform1.txtMADL1c.value)
	loFormSet.lcMADL2=ALLTRIM(loFormSet.ariaform1.txtMADL2c.value)
	loFormSet.lcMADL3=ALLTRIM(loFormSet.ariaform1.txtMADL3c.value)
	loFormSet.lcMADL4=ALLTRIM(loFormSet.ariaform1.txtMADL4c.value)
	loFormSet.lcMADL5=ALLTRIM(loFormSet.ariaform1.txtMADL5c.value)

ELSE
	loFormSet.lcMADL1=ALLTRIM(loFormSet.ariaform1.txtMADL1.value)
	loFormSet.lcMADL2=ALLTRIM(loFormSet.ariaform1.txtMADL2.value)
	loFormSet.lcMADL3=ALLTRIM(loFormSet.ariaform1.txtMADL3.value)
	loFormSet.lcMADL4=ALLTRIM(loFormSet.ariaform1.txtMADL4.value)
	loFormSet.lcMADL5=ALLTRIM(loFormSet.ariaform1.txtMADL5.value)
ENDIF 
MADL1 = loFormSet.lcMADL1
MADL2 = loFormSet.lcMADL2
MADL3 = loFormSet.lcMADL3
MADL4 = loFormSet.lcMADL4
MADL5 = loFormSet.lcMADL5
M120L1= loFormSet.lcm12l1
M120L2= loFormSet.lcm12L2
M120L3= loFormSet.lcm12L3
M90L1 = loFormSet.lcM90L1
M90L2 = loFormSet.lcM90L2
M90L3 = loFormSet.lcM90L3
M60L1 = loFormSet.lcM60L1
M60L2 = loFormSet.lcM60L2
M60L3 = loFormSet.lcM60L3
M30L1 = loFormSet.lcM30L1
M30L2 = loFormSet.lcM30L2
M30L3 = loFormSet.lcM30L3
SAVE ALL LIKE M* TO (oAriaApplication.DataDir+'ARSTMSG')


FUNCTION lfMsg1
PARAMETERS loFormSet
DO CASE
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 120 DAYS'
    loFormSet.lcm12L1 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML1C.value,loFormSet.ariaform1.txtML1.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 90 DAYS'
    loFormSet.lcM90L1 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML1C.value,loFormSet.ariaform1.txtML1.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 60 DAYS'
    loFormSet.lcM60L1 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML1C.value,loFormSet.ariaform1.txtML1.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 30 DAYS'
    loFormSet.lcM30L1 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML1C.value,loFormSet.ariaform1.txtML1.value)
ENDCASE


FUNCTION lfMsg2
PARAMETERS loFormSet
DO CASE
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 120 DAYS'
    loFormSet.lcm12L2 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML2C.value,loFormSet.ariaform1.txtML2.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 90 DAYS'
    loFormSet.lcM90L2 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML2C.value,loFormSet.ariaform1.txtML2.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 60 DAYS'
    loFormSet.lcM60L2 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML2C.value,loFormSet.ariaform1.txtML2.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 30 DAYS'
    loFormSet.lcM30L2 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML2C.value,loFormSet.ariaform1.txtML2.value)
ENDCASE


FUNCTION lfMsg3
PARAMETERS loFormSet

DO CASE
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 120 DAYS'
    loFormSet.lcm12L3 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML3C.value,loFormSet.ariaform1.txtML3.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 90 DAYS'
    loFormSet.lcM90L3 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML3C.value,loFormSet.ariaform1.txtML3.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 60 DAYS'
    loFormSet.lcM60L3 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML3C.value,loFormSet.ariaform1.txtML3.value)
 CASE UPPER(ALLTRIM(loFormSet.PbDays)) = 'OVER 30 DAYS'
    loFormSet.lcM30L3 = IIF(loFormSet.llFormC,loFormSet.ariaform1.txtML3C.value,loFormSet.ariaform1.txtML3.value)
ENDCASE










