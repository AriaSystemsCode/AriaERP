*--Scale Screen****************************************************************

*--Company using Extended Size Scale Setup.
llExtendedSizeScale = gfGetMemVar('M_USEEXSSC')

IF llExtendedSizeScale
  *--Extended size scale screen.
  *=oAriaApplication.DoProgram('AWRICEScale','','','IC')
  *DO FORM (oAriaApplication.ScreenHome+"IC\ICEScale.scx")
  =oAriaApplication.ExecCMD([DO FORM (oAriaApplication.ScreenHome+"IC\ICEScale.scx")])
ELSE
  *--Normal non extended scale screen.
  *=oAriaApplication.DoProgram('AWRICSScale','','','IC')
  DO FORM (oAriaApplication.ScreenHome+"IC\ICSScale.scx")
ENDIF
