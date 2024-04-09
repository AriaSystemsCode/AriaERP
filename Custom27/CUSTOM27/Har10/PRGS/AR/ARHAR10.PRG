***************************************************************************
*: Program file       : ARHAR10.PRG
*: Program desc.      : Screen to Add or modify division addresses.REF(C101252)
*: System             : Aria Apparel System (A27)
*: Module             : ACCOUNT RECEIVABLE  (AR)
*: Developer          : BASSEM RAFAAT       (BWA)
*: Date               : 01/02/2001
*: Tracking Job Number: C102106
*:**************************************************************************
*: Calls             :
*:        FUNCTIONS  : ...........
*:        PROCEDURES : ...........
*:        PROGRAMES  : ARHAR100.spr
*:**************************************************************************
*: Passed Parameters : ...........
*:**************************************************************************

           *--Section to get wanted settings  --*
lcConfirm  = SET('CONFIRM')
SET CONFIRM OFF

           *--Section of Initializing the variables --*
STORE SPACE(35) TO lcAdr1,lcAdr2,lcAdr3,lcAdr4
llSel = .T.
llBrowse = .F.

IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
ENDIF

           *-- Begin of Section of Initializing the arrays to retrive in it the codes --*
DECLARE laCodes[1,10] , laDivision[1,2]
STORE '' TO laCodes , laDivision , lcDefault
STORE 1 TO lnDivision

laCodes[1,1] = 'CDIVISION'
laCodes[1,2] = 'laDivision'
laCodes[1,3] = 'lnDivision'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .T.
laCodes[1,7] = 'CODES'
laCodes[1,8] = 'CODES'
laCodes[1,9] = ''
laCodes[1,10] = 'cDivision'

* "L" : List of the available codes.
=gfwCodePop(@laCodes,'CDIVISION','L')

       *-- End of Section of Initializing the arrays to retrive in it the codes --*

* If the file doesn't exist, create it
IF !FILE(gcDataDir+'DIVDATA.DBF')
  =gfDialog('!',"File DIVDATA doesn't exist, cannot proceed.")
  =lfClosFls()
  RETURN
ELSE
  =gfOpenFile(gcDataDir+'DIVDATA','DIVDATA','SH')
ENDIF

lcSetEsc = ON('KEY', 'ESC')
ON KEY LABEL ESC DO lfvCancel
DO (gcScrDir + gcWinAppl +'\ARHAR10.SPX')
ON KEY LABEL ESC &lcSetEsc
SET CONFIRM &lcConfirm
=lfClosFls()

               *-- End of the program code --*
*!*************************************************************
*! Name      : lfvDiv
*! Developer : BASSEM RAFAAT 
*! Date      : 01/02/2001
*! Purpose   : Valid function for the division field.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvDiv()
*!*************************************************************
FUNCTION lfvDiv

lcDiv = LEFT(laDivision[lndivision,1],6)

DO CASE
  CASE lcDiv = 'N/A   '
    llSel = .T.
    * Refresh the screen
    =lfShow(llSel)

  CASE SEEK(lcDiv,'DIVDATA')

    lcAdr1 = DIVDATA.cAdr1
    lcAdr2 = DIVDATA.cAdr2
    lcAdr3 = DIVDATA.cAdr3
    lcAdr4 = DIVDATA.cAdr4
    llSel = .F.
    * Refresh the screen
    =lfShow(llSel)

  * If the division is not in the new file but in the codes file.
  CASE SEEK('N'+lcDiv+'N'+'CDIVISION ','Codes')

    INSERT INTO DIVDATA (cDivision,cDesc) VALUES (lcDiv,Codes.cDiscrep)
    STORE SPACE (35) TO lcAdr1,lcAdr2,lcAdr3,lcAdr4
    llSel = .F.
    =lfShow(llSel)

ENDCASE
=gfwCodePop(@laCodes,'CDIVISION','L')

*-- End of lfvDiv.
*!*************************************************************
*! Name      : lfShow
*! Developer : BASSEM RAFAAT 
*! Date      : 01/02/2001
*! Purpose   : Function to refresh the screen, Enable, disable required objects
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfShow()
*!*************************************************************
FUNCTION lfShow
PARAMETERS llSelM

IF llSelM
  STORE SPACE(35) TO lcAdr1,lcAdr2,lcAdr3,lcAdr4
  
  SHOW GET pbOk      DISABLE
  SHOW GET lcAdr1    DISABLE
  SHOW GET lcAdr2    DISABLE
  SHOW GET lcAdr3    DISABLE
  SHOW GET lcAdr4    DISABLE
  =lfRefresh()

ELSE

  SHOW GET lcAdr1    ENABLE
  SHOW GET lcAdr2    ENABLE
  SHOW GET lcAdr3    ENABLE
  SHOW GET lcAdr4    ENABLE
  =lfRefresh()
  SHOW GET pbOk ENABLE

ENDIF
*-- End of lfShow.
*!*************************************************************
*! Name      : lfvCancel
*! Developer : BASSEM RAFAAT 
*! Date      : 01/02/2001
*! Purpose   : Valid function of the cancel button
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvCancel()
*!*************************************************************
FUNCTION lfvCancel

CLEAR READ

*-- End of lfvCancel.
*!*************************************************************
*! Name      : lfvOk
*! Developer : BASSEM RAFAAT 
*! Date      : 01/02/2001
*! Purpose   : Valid function of OK button
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvOk()
*!*************************************************************
FUNCTION lfvOk

*-- Save the record.
SELECT DIVDATA
REPLACE cAdr1 WITH lcAdr1,;
        cAdr2 WITH lcAdr2,;
        cAdr3 WITH lcAdr3,;
        cAdr4 WITH lcAdr4

llSel = .T.
=lfShow(llSel)
lnDivision = 1
_CUROBJ = OBJNUM(lnDivision)

=gfwCodePop(@laCodes,'CDIVISION','N')
SHOW GET lnDivision

*-- End of lfvOk.
*!*************************************************************
*! Name      : lfClosFls
*! Developer : BASSEM RAFAAT 
*! Date      : 01/02/2001
*! Purpose   : To Close Necessary Files That Open Before.
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : =lfClosFls()
*!**************************************************************
FUNCTION lfClosFls

*-- Close All Open Files.
= gfCloseFile('CODES')

*-- End of lfClosFls.