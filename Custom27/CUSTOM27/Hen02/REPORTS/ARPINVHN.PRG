************************************************************************************************
* Program     : ARPINVHN.PRG
* Developer   : TMI - TAREK MOHAMED IBRAHIM
* DATE        : 07/21/2003
* Entry       : B607429,1
* PORPOUS     : Add field to Henry Segal form to show Vender #
* APPLICATION : ARIA27
* Module      : AR2
***********************************************************************************************

PRIVATE lnSlct
lnSlct = SELECT()
*-- Open customer file with other alias to get the vendor #
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH',@lcCustmr,.T.)

SELECT INVHDR
SET RELATION TO 'M'+ACCOUNT INTO (lcCustmr)

SELECT (lnSlct)