*:***************************************************************************
*: Program file  : APDBTMEM.PRG
*: Program desc. : AP DEBIT MEMO FOR GMA ACCOSORIES
*! Date          : 12/12/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT PAYABLE (AP)
*: Developer     : MOHAMED SHOKRY (MHM)			
*: Tracking Job Number: C#200465
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO APDBTMEM
*:**************************************************************************
*:Modifications :
*:***************************************************************************
*--create temp file
DIME laCompAdd[1,5]

=lfCrtTmp()

SELECT APINVHDR
SET RELATION TO cVendCode INTO ApVendor ADDITIVE

*--get addresses
=lfGetAdd()

m.ADDRSS1 = SYCCOMP.cCom_Name 
m.ADDRSS2 = laCompAdd[1]
m.ADDRSS3 = ALLTRIM(laCompAdd[2])+ALLTRIM(laCompAdd[3])+ALLTRIM(laCompAdd[4])+ALLTRIM(laCompAdd[5])
m.ADDRSS4 = SYCCOMP.cCom_Phon
m.ADDRSS5 = SYCCOMP.cCom_fax             


SELECT APINVHDR
SCAN FOR  &lcRpExp
  IF nInvAmnt<0
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTempLine)  FROM MEMVAR
  ENDIF  
ENDSCAN
SELECT (lcTempLine)
LOCATE
DO gfDispRe WITH EVALUATE('lcRpForm')


*!*************************************************************
*! Name      : lfGetAdd
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/12/2002
*! Purpose   : Function to Get Company Address 
*!*************************************************************
*! Called from : APDBTMEM.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetAdd()
*!*************************************************************
FUNCTION lfGetAdd
PRIVATE lcAlias
lcAlias = ALIAS()   && Save Current alias.
SELECT SYCCOMP
SEEK gcAct_Comp

lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcCompFax  = cCom_fax              && Variable to hold the Company Fax
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format

*-- Load Company address.
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
SELECT (lcAlias)    && Restore before function alias.


*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/12/2002
*! Purpose   : Function to create Temp file 
*!*************************************************************
*! Called from : lfCrtTmp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTmp()
*!*************************************************************
FUNCTION lfCrtTmp

lcTempLine = gfTempName()

DIMENSION laTempStru[1,4]
laTempStru = ''
SELECT APINVHDR
= AFIELDS(laTempStru)

DIMENSION laTempStru[ALEN(laTempStru,1)+1 , 4]
laTempStru[ALEN(laTempStru,1),1] = 'ADDRSS1'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'ADDRSS2'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'ADDRSS3'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'ADDRSS4'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'ADDRSS5'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

CREATE DBF (gcWorkDir+lcTempLine)FROM ARRAY laTempStru

INDEX ON cInvNo+cVendCode TAG (lcTempLine) 

*!*************************************************************
*! Name      : lfSRDbtMem
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/12/2002
*! Purpose   : Rise change Debit Memo flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRDbtMem()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSRDbtMem
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SELECT APINVHDR
    LOCATE
ENDCASE
*-- end of lfSRDbtMem.

*!*************************************************************
*! Name      : lfSRDVEN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/12/2002
*! Purpose   : Rise change Vendor flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRDVEN()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSRDVEN
PARAMETERS lcParm 

DO CASE
  CASE lcParm = 'S'
    SELECT ApVendor
    LOCATE
ENDCASE
*-- end of lfSRDVEN.
