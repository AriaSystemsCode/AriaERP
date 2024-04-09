*!**********************************************************************************************
*! Name      : ICPIKGM
*! Developer : Mariam Mazahr
*! Date      : 05/12/2008
*! Purpose   : call picking Screen
*!**********************************************************************************************
PARAMETERS lcPicktype
DO FORM (oAriaApplication.ClientScreenHome+oAriaApplication.ActiveModuleId+"\icRCVGM") WITH 'P'
