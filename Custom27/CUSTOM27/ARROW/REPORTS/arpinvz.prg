*:************************************************************************
*: Program file  : ARPINVOZ.Prg
*: Program desc. : Optional program of custom invoice form Z (OMAR RAMADAN)
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 05/24/1999
*: REF           : 
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetProf()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARPINVOZ
*:************************************************************************
lnAlias=SELECT(0)
*-- Open the profile table
=gfOpenFile(gcDataDir+'PROFILE','PROFILE','SH')
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetProf
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 05/24/1999
*! Purpose   : Get Customer profile
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetProf()
*!*************************************************************

FUNCTION lfGetProf
PARAMETERS lcRetArr,lcAccnt,lcStore
lnAlias=SELECT(0)

DIMENSION laProf[1,3]
STORE SPACE(0) TO laProf
IF SEEK('C'+lcAccnt+lcStore , 'PROFILE' )
  lnC = 1
  SELECT PROFILE
  SCAN WHILE cContType+cCont_ID+Store='C'+lcAccnt+lcStore .AND. lnC <=5
    lcDesc = gfCodDes(PROFILE.cPro_Code,'CPRO_CODE')
    IF LEFT(lcDesc,1) = '#'
      DIMENSION laProf[lnC,3]
      laProf[lnC,1] = PROFILE.cPro_Code
      laProf[lnC,2] = lcDesc
      laProf[lnC,3] = PROFILE.cPro_Value
      lnC = lnC + 1
    ENDIF
  ENDSCAN
  SELECT (lnAlias)
ENDIF
lcRetArr = SPACE(0)



