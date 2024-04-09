IF  .NOT. gfsetup()
   RETURN
ENDIF
STORE .F. TO lldebit, llcredit, llokbut
lnadjust = 1
DO (gcscrdir+gcact_appl+'\ARKRADC.SPX')
glquitting = .T.
lldebit = (lnadjust=1)
llcredit = (lnadjust=2)
llkranote = .T.
lctoedit = gftempname()
CREATE CURSOR (lctoedit) (medtnotes M)
APPEND BLANK
= gfopenfile(gcdatadir+'NOTEPAD',gcdatadir+'NOTEPAD','SH')
llkrachrge = .T.
IF llokbut
   lcscrcode = IIF(llcredit, 'AWRARCRADJ', IIF(lldebit, 'AWRARDBADJ', 'AWRARBKADJ'))
   llfuncall = .T.
   DO gpdoprog WITH lcscrcode, .F., 'AR'
   IF USED(lctoedit)
      USE IN (lctoedit)
   ENDIF
ENDIF

PROCEDURE lfvokbut
 llokbut = .T.

PROCEDURE arbkadj
 DO (gcapphome+'ARDBADJ')
