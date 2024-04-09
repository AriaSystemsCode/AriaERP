*C101697,1 SAM   01/04/2000
*:************************************************************************
*: Program file  : ICBARCD.PRG
*: Program desc. : PRINT BAR CODED ID CARDS
*: System        : ARIA27
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh Aldesouki
*: Date          : 01/04/2000
*:************************************************************************
*: Calls         : 
*:         Procedures : ....
*:         Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: B803310,1 SSH Set Divice to screen.
*:*************************************************************
*--  set used alias
lnAlias = SELECT(0)

SELECT SYUUSER
SET FILTER TO &lcRpExp
=gfDispRe(lcFName ,.F.,.F.,'L')
*: B803310,1 SSH Set Divice to screen.
SET DEVICE TO SCREEN
*: B803310,1 SSH [END]

SELECT (lnAlias)

*:********   End of Program
*:*******************************************************************


