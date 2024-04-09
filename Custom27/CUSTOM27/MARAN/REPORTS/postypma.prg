*:***************************************************************************
*: Program file  : POSTYPMA.PRG
*: Program desc. : Get Style UPC and Prepack Information
*: For Report    : POSTYPMA
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO)
*: Developer     : Ken Li (KEN)
*: Customer      : Maran
*:
*C101598 KEN, 08/05/99
*:***************************************************************************
*: Modifications      :
*:B603162,1 WAB 09/20/1999 Fix The Bug that occured when you go out of the form 
*:B603162,1                and go to the style screen you get file is in use error 
*:B803067,1 SSH 29/02/00   Fix the bug of print the first prepack alwayes
*:B803067,1 SSH 29/02/00   even if the scale have more than one.
*!***************************************************************************
lnAlias = SELECT()
=gfOpenFile(gcDataDir+'styleupc',gcDataDir+'Styleupc','SH')
IF !USED("SCALE_B")
  *B603162,1 WAB - Use global function gfopenfile to open file with an tmpname
  *B603162,1 WAB - START
  *SELECT 0 
  *USE (gcDataDir+'SCALE') AGAIN  ALIAS SCALE_B
  *SET ORDER TO TAG SCALE IN SCALE_B
  =gfOpenFile(gcDataDir+'SCALE','SCALE','SH',@lcTmpScl,.T.)

  *B603162,1 WAB - END
ENDIF

SELECT POSLN

*B603162,1 WAB - USE THE tmpname (lcTmpScl) insted of scale_b
*B603162,1 WAB - START
*SET RELATION TO 'P' + Scale INTO SCALE_B ADDITIVE
*:B803067,1 SSH 29/02/00   Add the prepak to the relation.
*SET RELATION TO 'P' + Scale INTO (lcTmpScl) ADDITIVE
SET RELATION TO 'P' + Scale+PrePak INTO (lcTmpScl) ADDITIVE
*:B803067,1 SSH 29/02/00   (End)
*B603162,1 WAB - END
SELECT(lnAlias)
