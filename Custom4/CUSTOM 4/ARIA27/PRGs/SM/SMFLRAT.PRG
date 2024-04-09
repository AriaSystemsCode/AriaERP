*:************************************************************************
*: Program file  : SMFLRAT.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: SYSTEM MANAGER (SM)
*:      Developer: AHMED MAHER (AMH)
*:      Referance: E302135,1
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*:Modifications  :
*:037304,1 ABD 12/07/2003 Fix Problem with the Ups Fuel Surcharge Rate Screen, 
*:037304,1 ABD            Field Fuel Charge calculate the freight with wrong way.
*:037304,1 ABD            We update the 2 field [cfrtRate,NiniRate] to be 3 Decimal not 2.
*:129973,1 NNA 12/14/2005 Fix bug that when you open the screen you get the open dialog screen (fixed in SCX and SPR)
*:040052,1 EIH 01/02/2006 Make UPS fuel surcharge support Ground fuel surcharge.  
*:************************************************************************

IF !gfSetup()
  RETURN
ENDIF  

SELECT SYSRATES
lnNewRate = IIF(NINIRATE=0,0,(CFRTRATE - NINIRATE) / NINIRATE * 100)
DO (gcScrDir +gcWinAppl + '\SMFLRAT.SPX')

*!***************************************************************************
*!
*!         FUNCTION :lfvOk
*!
*!***************************************************************************
*
FUNCTION lfvOk

SELECT SYSRATES
*ash on 01/04/2004 Apply the new rule of UPS to not include the fule charges on the UPS ground services.
*REPLACE ALL CFRTRATE WITH NINIRATE * (1 + (lnNewRate / 100)) 
REPLACE ALL CFRTRATE WITH NINIRATE * (1 + (lnNewRate / 100)) FOR ShipType <> 'USUPSG'

*040052,1 EIH 01/02/2006 Replace for ground fuel.[Begin]  
REPLACE ALL CFRTRATE WITH NINIRATE * (1 + (lnNewGRate / 100)) FOR ShipType = 'USUPSG'
*040052,1 EIH 01/02/2006 Replace for ground fuel.[Begin]  