*:**************************************************************************
*: Program file  : SOSHLBOG.Prg
*: Program desc. : SHIPPING LABELS REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO,AR,AL
*: Developer     : Sameh Saiid Ezzat 
*: Date          : 08/15/2000
*: Reference     : C101857
*:**************************************************************************
*C101857,1 SSE 08/15/2000 Convert Shipping label for OGIO INTERNATIONAL Inc.
*:**************************************************************************

*!**************************************************************************
*! Name      : lfAddress
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/15/2000
*! Purpose   : Get warehouse Address to be printed in the Label
*!**************************************************************************
*! Called from : Label LBX
*!**************************************************************************
*
FUNCTION lfAddress
PARAMETERS lcDummy

lcAddress2 = IIF(EMPTY(WareHous.cAddress2),SPACE((50-LEN(ALLTRIM(SUBSTR(WareHous.cAddress3,1,15));
             + ' ' + ALLTRIM(SUBSTR(WareHous.cAddress4,1,3)) + ' ' + ;
             ALLTRIM(SUBSTR(WareHous.cAddress5,1,10))))/2) + ALLTRIM(WareHous.cAddress3) + ' ' + ;
             WareHous.cAddress4 + ' ' + WareHous.cAddress5,;
             SPACE((50-LEN(ALLTRIM(WareHous.cAddress2)))/2)+ALLTRIM(WareHous.cAddress2))
lcAddress3 = IIF(EMPTY(WareHous.cAddress2),SPACE(17) + 'Tel:' + TRANSFORM(WareHous.cPhone,;
             gfPhoneTem()),SPACE((50-LEN(ALLTRIM(SUBSTR(WareHous.cAddress3,1,15)) + ' ' + ;
             ALLTRIM(SUBSTR(WareHous.cAddress4,1,3)) + ' ' + ALLTRIM(SUBSTR(WareHous.cAddress5,;
             1,10))))/2) + ALLTRIM(SUBSTR(WareHous.cAddress3,1,15)) + ' ' + ;
             SUBSTR(WareHous.cAddress4,1,3) + ' ' + SUBSTR(WareHous.cAddress5,1,10))
RETURN ''
*-- End of lfAddress.