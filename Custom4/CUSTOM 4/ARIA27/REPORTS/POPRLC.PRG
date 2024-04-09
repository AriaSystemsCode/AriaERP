*:**************************************************************************
*: Program file  : POPRLC.PRG 
*: Program desc. : Print Letter of Credit
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : ABDOU ELGENDI - (ABD)
*: Date          : 06/23/1999
*:**************************************************************************
*: Calls : FUNCTIONS  : lfWhen , lfSETLcno , lfAdrShift , lfVendAdd , lfPrntNp
*:                    :     
*:         PROCEDURE  : gfDispRe
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:E301292 ,1     : New Program code.
*:**************************************************************************
*:Modifications :
*:B603075,1 HDM 08/10/1999 Changed The name of when function to lfWhenGrid
*:                         as lfWhen reserved as a global function in the 
*:                         OG program
*:**************************************************************************
*
 STORE '' TO ICLCODE
*-- llCalFad variable that is used to hold the shift function
*-- laCompAdd Dimension used to get the Account,Name,Buyer,Addressess
*--           and shift any empty record 

DIMENSION laVenAdd[5]
STORE SPACE(0) TO laVenAdd

DIMENSION laCompAdd[5]
laCompAdd = ''

SELECT SYCCOMP
=SEEK(gcAct_comp)
laCompAdd[1] = gfGetAdr('Syccomp' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('Syccomp' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('Syccomp' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('Syccomp' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('Syccomp' , '' , '' , '' , 5)
=lfAdrShift('laCompAdd')
SELECT LC
GO TOP
= lfVendAdd()

*:**************************************************************************
*--- Call the FRX.

DO gfDispRe WITH EVAL('lcRpForm'), 'FOR ' + lcRpExp

*-- End Of Report Code.
*:**************************************************************************
*-- Functions and Procedures :
*:**************************************************************************
*! Name      : lfWhen()    
*! Developer : ABDOU ELGENDI
*! Date      : 06/13/1999
*! Purpose   : To Set When running the Option Grid.
*!*************************************************************
*! Called from : Option Grid
*! Passed Parameters : None
*:**************************************************************************
*! Return      : None
*:**************************************************************************
*! Example     : =lfWhen()    
*:**************************************************************************
*
*-- HDM B603075,1[Start] Changed The name of when function to lfWhenGrid
*--               as lfWhen reserved as a global function in the 
*--               OG program
*FUNCTION lfWhen

FUNCTION lfWhenGrid
*-- HDM B603075,1[End]
=lfSETLcno()
*-- End Of lfWhenGrid.
*:**************************************************************************
*! Name      : lfSETLcno
*! Developer : ABDOU ELGENDI
*! Date      : 06/13/1999
*! Purpose   : TO SET ORDER TO TAGs When Running The Option Grid
*:**************************************************************************
*! Called from : Option Grid
*:**************************************************************************
*! Calls       : ......
*:**************************************************************************
*! Passed Parameters : None
*:**************************************************************************
*! Return      : None
*:**************************************************************************
*! Example     : =lfSETLcno()
*:**************************************************************************
*
FUNCTION lfSETLcno 
PARAMETERS OpGrdParm
   SET ORDER TO TAG lc       IN lc
   SET ORDER TO TAG Ccomp_id IN Syccomp
   SET ORDER TO TAG Vencode  IN APVENDOR
   SET ORDER TO TAG Notepad  IN Notepad 
*-- End Of lfSETLcno 
*:**************************************************************************
*! Name      : lfAdrShift
*! Developer : ABDOU ELGENDI
*! Date      : 06/14/99
*! Purpose   : used to shift the lacompadd array
*:**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:**************************************************************************
*! Called from : variable from report POPRLC.frx 
*:**************************************************************************
*! Passed Parameters  : lcArrayNam
*:**************************************************************************
*! Returns            : None
*:**************************************************************************
*! Example   : = lfAdrShift()
*:**************************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam
FOR lnCount = 1 TO 6
  
*--IF The current Array element is of type character and empty
 IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
   EMPTY(&lcArrayNam.[lnCount])
   =ADEL(&lcArrayNam , lnCount)
   lnCount = lnCount - 1
 ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
*--HDM =>FOR lnCount = 1 TO 5
FOR lnCount = 1 TO ALEN(&lcArrayNam)
*--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift
*:**************************************************************************
*! Name      : lfVendAdd
*! Developer : ABDOU ELGENDI
*! Date      : 06/14/99
*! Purpose   : To Get The Address For every Vendor
*! Called from : variable from report POPRLC.frx 
*:**************************************************************************
*! Passed Parameters  : ........
*:**************************************************************************
*! Returns            : None
*:**************************************************************************
*! Example   : = lfVendAdd()
*:**************************************************************************
*
FUNCTION lfVendAdd

=SEEK(lc.Vendor,'APVendor')
laVenAdd[1] = gfGetAdr('APVendor' , '' , '' , '' , 1)
laVenAdd[2] = gfGetAdr('APVendor' , '' , '' , '' , 2)
laVenAdd[3] = gfGetAdr('APVendor' , '' , '' , '' , 3)
laVenAdd[4] = gfGetAdr('APVendor' , '' , '' , '' , 4)
laVenAdd[5] = gfGetAdr('APVendor' , '' , '' , '' , 5)
=lfAdrShift('laVenAdd')

RETURN ''
*-- End Of lfVendAdd
*:**************************************************************************
*! Name      : lfPrntNp
*! Developer : ABDOU ELGENDI
*! Date      : 06/14/99
*! Purpose   : To Print The Note Pad In The End of Every Report
*! Called from : variable from report POPRLC.frx 
*:**************************************************************************
*! Passed Parameters  : .........
*:**************************************************************************
*! Returns            : None
*:**************************************************************************
*! Example   : = lfPrntNp()
*:**************************************************************************
*
FUNCTION lfPrntNp

STORE '' TO   lcNotes , lcEXP
  IF !EMPTY (lC.Expire)
    lcEXP = "EXPIRE :" + DTOC(LC.Expire)
  ENDIF
  lcNotes = "LC # " + LC.Clcno + CHR(13) + "Date :" + DTOC(LC.Created) +;
            CHR(13) + lcEXP + CHR(13) + "*--- N o t e  P a d ---*" +;
            CHR(13) + NOTEPAD.mNotes
RETURN lcNotes
*-- End Of lfPrntNp
*:**************************************************************************
