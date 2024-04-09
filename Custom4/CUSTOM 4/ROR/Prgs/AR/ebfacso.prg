*B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[T20101130.0036]

*B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
IF oAriaApplication.MULTIINST 
 lcParmLst = "'O'"
 =gfCallForm('EBFACIN',oAriaApplication.ActiveModuleID,lcParmLst)
ELSE
*B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]
DO (oAriaApplication.ProgramHome+oAriaApplication.ActiveModuleID+'\EBFACIN') WITH 'O'
*B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
ENDIF
*B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]