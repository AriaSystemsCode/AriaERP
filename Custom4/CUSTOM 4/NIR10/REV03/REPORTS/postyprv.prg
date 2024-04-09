*:***************************************************************************
*: Program file  : POSTYPRV
*: Program desc  : Print PO for Revue
*: For Report    : POSTYPRV.FRX
*: System        : Aria 4xp
*: Module        : PO
*: Developer     : Mostafa Eid(mos)
*: Reference     : C201004
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Modifications :
*: B610018,1 MMT 07/24/2012 Custom PO form RV print incorrect pages per PO[Revue Conversion project]
*!*************************************************************
*! Name      : lfGetRlFld
*! Developer : MOSTAFA EID (MOS)
*! Date      : 05/25/2008
*! Purpose   : to get the Related fields of HTS code.
*!*************************************************************
*! Called from : POSTYPRV.FRX
*!*************************************************************
*! Calls       : gfRltFld()
*!*************************************************************
FUNCTION lfGetRlFld
PARAMETERS lcReturn
*: B610018,1 MMT 07/24/2012 Custom PO form RV print incorrect pages per PO[Revue Conversion project][Start]
*!*	DIMENSION laQuotaCat[1,2]
*!*	DIMENSION laDutyRate[1,2]
*!*	laQuotaCat[1,1] = 'CCOTA1'
*!*	laQuotaCat[1,2] = 'lcQuotaCat'
*!*	laDutyRate[1,1] = 'NDUTY1'
*!*	laDutyRate[1,2] = 'lnDutyRate'
*=gfRltFld(POSHDR.CHTS, @laQuotaCat , 'CHTS')
*=gfRltFld(POSHDR.CHTS, @laDutyRate , 'CHTS')
lcQuotaCat = ''
lnDutyRate = 0
lcAlias = ALIAS()
SELECT Codes
SET ORDER TO TAG Codes
IF SEEK('N'+POSHDR.CHTS+'Y'+PADR('CHTS',10))
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+POSHDR.CHTS+'Y'+PADR('CHTS',10)
    DO CASE 
      CASE crltd_nam = PADR('CCOTA1',10)
        lcQuotaCat = crltd_vlu
      CASE crltd_nam = PADR('NDUTY1',10)  
        lnDutyRate = VAL(crltd_vlu)
    ENDCASE
  ENDSCAN
ENDIF
SELECT (lcAlias)
*: B610018,1 MMT 07/24/2012 Custom PO form RV print incorrect pages per PO[Revue Conversion project][End]
RETURN ''
*--end of lfGetRlFld.