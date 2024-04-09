*-- Call the materail screen with the inventory type '0002'(Materail)
*=oAriaApplication.DoProgram('AWRICITEM','"0002"',.F.,'MA')
*B125778,1 AMH Receive prometers in this program [Start]
*PRIVATE lcICInvType
*lcICInvType = '0002'
*
*DO FORM (oAriaApplication.ScreenHome + "ICITEM") WITH lcICInvType
LPARAMETERS lcpInvType, lcpStyle, lcpColor
PRIVATE lcICInvType
lcICInvType = IIF(EMPTY(lcpInvType),'0002',lcpInvType)

DO FORM (oAriaApplication.ScreenHome + "ICITEM") WITH lcICInvType, lcpStyle, lcpColor
*B125778,1 AMH [End]
