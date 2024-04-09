*:***************************************************************************
*: Program file  : POSTYPFL
*: Program desc. : Print PO 
*: For Report    : POSTYPFL.FRX
*: System        : Aria Advantage Series ARIA4XP  
*: Module        : Purchase Order (PO)
*: Developer     : Hassan Ibrahim Ali (HIA)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSTYPOFL
*:***************************************************************************
*: 07/03/2007 Due to  # T20070319.0003 /C200806,C200807
*: Display Sablon code descirption, Sablon color descirption and Accessory Color descirption
*: C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[T20080214.0006]
*:***************************************************************************

IF !USED('SOCODES')
  =gfOpenTable(oAriaApplication.DataDir+'SOCODES',oAriaApplication.DataDir+'SOCODES','SH')
ENDIF

SELECT SOCODES
SET ORDER TO SOCODES   && CORDTYPE+ORDER+STR(LINENO,6) 

IF !USED('CUTPICK')
  =gfOpenTable(oAriaApplication.DataDir+'CUTPICK',oAriaApplication.DataDir+'CUTPICK','SH')
ENDIF

*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[Start]
IF  .NOT. USED('PROFVALU')
  gfOpenTable('PROFVALU','PROFILE','SH')
ENDIF
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[End]

SELECT CUTPICK
=gfSetOrder('CUTPKORD')


SELECT POSHDR 
*:***************************************************************************
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[Start]
FUNCTION lfGetSabInfo
PARAMETERS lcSablonCode
lcAlias  = SELECT()
lcRetVal = ''
IF gfSeek('2'+POSLN.PO+STR(POSLN.LINENO,6) ,'CUTPICK')  AND Seek('SOO'+CUTPICK.ORDER+CUTPICK.cordline,'PROFVALU')
  SELECT PROFVALU
  SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'SOO'+CUTPICK.ORDER+CUTPICK.cordline
    IF UPPER(ALLTRIM(gfCodDes(PROFVALU.CPRO_CODE  , 'CPRO_CODE' ))) = UPPER(lcSablonCode)
	  lcRetVal = ALLTRIM(profvalu.cpro_value)       
	  EXIT 
    ENDIF 
  ENDSCAN 
ELSE
  lcRetVal =  ''
ENDIF 
SELECT (lcAlias)
RETURN lcRetVal
*C200968,1 MMT 03/23/2008 Convert Sablon Information to be Profiles in Aria4xp[End]