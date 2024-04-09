*:***************************************************************************
*: Program file  : MAEAS10
*: Program desc. : Custom Bar Code Printing For East Coast
*: System        : Aria Advantage Series.
*: Module        : Materials (MA)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 01/19/2000
*: Reference     : C101693
*:***************************************************************************
*: Calls : 
*:    Procedures : lpCollData, lpCreatFil
*:                 
*:                   
*:    Functions  : gfModalGen, lfwRepWhen
*:               
*:***************************************************************************
*: Example : DO MAEAS10
*:***************************************************************************
*

*-- If user changed filter from OG [Begin.]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF

  SELECT PoFLn
  SCAN FOR &lcRpExp
    lcKey = crsession+fabric+color+cwarecode+dyelot

    IF SEEK(lcKey,'Rolls')
      SELECT Rolls
      SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey
        IF SEEK(PoFLn.Fabric+PoFLn.Color,'Fabric')
          m.cFabDesc  = Fabric.Desc  
          m.cFabWidth = Fabric.Width
          m.cUOMBuy   = Fabric.UOMBuy
        ENDIF
        
        IF SEEK(PoFLn.Fabric+SPACE(12)+PoFLn.Color+PoFLn.cWareCode,'WhsLoc')
		  m.cLocation = WhsLoc.cLocation
        ENDIF
        
        m.cFabric   = PoFLn.Fabric
        m.cColor    = PoFLn.Color
		m.cDyelot   = PoFLn.Dyelot	
        m.cPattern  = PoFLn.Pattern
        m.dTranDate = PoFLn.Date
        m.cVendor   = PoFLn.Vendor

        m.nYarDage   = nQty
        m.cBarCode   = cRollId

        INSERT INTO (lcWorkFile) FROM MEMVAR 
      ENDSCAN
    ENDIF
  ENDSCAN

ENDIF
*-- EndIf of user changed filter from OG [End.]
*-- If no records in temp file (empty)
SELECT (lcWorkFile)
*-- If Seek is successful (There's Records)

IF RECCOUNT(lcWorkFile) <> 0
  DO gfDispRe WITH EVALUATE('lcRpName')
ELSE     && there is no records in Temp file
  *-- <Message> No records to display.
  *-- <Buttons>          OK
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF
*-- EndIf of no records in temp file (empty)

*!***************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/19/2000
*! Purpose   : OG when function
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : = lfwRepWhen()
*!***************************************************************************
*
FUNCTION lfwRepWhen

DO lpCreatFil   && Create the Work Temp file

*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/19/2000
*! Purpose   : Create work File.
*!***************************************************************************
*! Called from : Report code.
*!***************************************************************************
*! Example   : DO lpCreatFil
*!***************************************************************************
*
PROCEDURE lpCreatFil

*nYarDage  --> Rolls.nQty
*cUOMBuy   --> Fabric.UOMBuy
*dTranDate --> Pofln.Date
*cBarCode  --> Rolls.cRollId

CREATE TABLE (lcWorkFile) ;
  (cFabric C(7), cFabDesc C(20), cColor C(6), cDyelot C(10),;
  nYarDage N(8,0), cPattern C(10), cUOMBuy C(3), cVendor C(8), ;
  dTranDate D(8), cFabWidth C(6), cLocation C(10), cBarCode C(20))

SELECT (lcWorkFile)
ZAP
INDEX ON cBarCode+cFabric TAG (lcWorkFile)
*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/19/2000
*! Purpose   : Function that we call when Close the option grid.
*!***************************************************************************
*! Called from : [Option Grid] < Close > button.
*!***************************************************************************
*! Example     : = lfClearRep()
*!***************************************************************************
*
FUNCTION lfClearRep
*-- Close temp. opended files, if it is used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
*-- End of lfClearRep.
