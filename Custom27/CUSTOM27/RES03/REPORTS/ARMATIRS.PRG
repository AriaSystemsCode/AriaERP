*:**************************************************************************
*: Program file  : ARMATIRS
*: Program desc. : Custom Material Invoice Form For RES03
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : NADER NABIL (NNA)
*: Date          : 03/25/2005
*: Reference     : CP#126214
*:**************************************************************************
*: Functions     : lfCreatTmp,lfColData,lfGetRoll,lfCloseFil
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications:
*:**************************************************************************
IF ALLTRIM(LACOMPADD[5]) = ALLTRIM(TRANSFORM(lcCompphon , lcPhonPict))
  LACOMPADD[5] = ''
ENDIF
IF ALLTRIM(LACOMPADD[6]) = ALLTRIM(TRANSFORM(lcCompphon , lcPhonPict))
  LACOMPADD[6] = ''
ENDIF
IF !USED('WAREHOUS')
  = gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH') 
ENDIF
IF !USED('ROLLS')
  = gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'SESSION','SH') 
ENDIF
IF !USED('MATINVJL')
  = gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MFGOPR','SH') 
ENDIF
SELECT MINVHDR
SET SKIP TO 
= lfCreatTmp()
= lfColData()
*--Get the Fedral ID for the company
lcFedral = ''
lcFedral = gfGetMemVar('M_FEDERAL')
SELECT MINVHDR
LOCATE
SET RELATION TO Minvhdr.cwarecode INTO Warehous ADDITIVE
SET RELATION TO Minvhdr.Cmatinv INTO (lcCntFile) ADDITIVE
SET RELATION TO Minvhdr.Cmatinv INTO (lcTmpFile) ADDITIVE
IF llPRNRoll
  SET SKIP TO IIF(RECCOUNT(lcTmpFile)<>0,(lcTmpFile),MInvLine)
ELSE
  SET SKIP TO MInvLine
ENDIF
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
llarpinv = .F.
=lfCloseFil()
*!***************************************************************************
*! Name      : lfCreatTmp
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Create the Temp files that will use to print the contract no
*!           : and Roll_ID
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCreatTmp()
*!***************************************************************************
FUNCTION lfCreatTmp
CREATE TABLE (gcWorkDir+lcTmpFile) (Invoice C(6),Account C(5),Fabric C(7),UOM C(3),;
             Color C(6),DESC1 C(60),LineNo N(6),Roll_ID C(20),QTY N(10),Price N(12,5))
INDEX ON (INVOICE+STR(LINENO,6)+Roll_ID) TAG (lcTmpFile)  OF (lcTmpFile)

CREATE TABLE (gcWorkDir+lcCntFile) (Invoice C(6),Account C(5),Contract C(6),Rolls_No N(10))
INDEX ON INVOICE TAG (lcCntFile)  OF (lcCntFile)

*--End of Function lfCreatTmp.
*!***************************************************************************
*! Name      : lfColData
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Collecting Data
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfColData()
*!***************************************************************************
FUNCTION lfColData
DECLARE laContract[1]
STORE '' TO lcAccount,lcStore
STORE 0 TO lnRollsNo
SELECT MINVHDR
SCAN FOR &lcRpExp
  lcAccount = MINVHDR.ACCOUNT
  lcStore   = MINVHDR.STORE
  laContract = ''
  SELECT MAX(Cmorder) FROM MASOHDR INTO ARRAY laContract WHERE CORDTYPE='C' AND ;
         STATUS='O' AND ACCOUNT=lcAccount AND STORE = lcStore
  INSERT INTO (lcCntFile)(Invoice,Account,Contract) ;
         VALUES (MINVHDR.Cmatinv,MINVHDR.Account,laContract[1])
  IF SEEK(MINVHDR.CMATINV,'MINVLINE')
    SELECT MINVLINE
    SCAN REST WHILE cmatinv+STR(lineno,6) = MINVHDR.CMATINV
      =lfGetRoll()
    ENDSCAN
  ENDIF
  SELECT(lcTmpFile)
  COUNT TO lnRollsNo FOR INVOICE = MINVHDR.CMATINV
  SELECT(lcCntFile)
  REPLACE Rolls_No WITH lnRollsNo
ENDSCAN
*--End of Function lfColData.
*!***************************************************************************
*! Name      : lfGetRoll
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Get the Roll ID for the Minvline Fabric
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Report Code.
*!***************************************************************************
*! Example   : =lfGetRoll()
*!***************************************************************************
FUNCTION lfGetRoll
lnOldAlias=SELECT(0)
IF SEEK(MINVHDR.Cmatinv,'MATINVJL')
  SELECT MATINVJL
  SCAN REST WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode = MINVHDR.Cmatinv ;
       FOR cFabric =MinvLine.Fabric  AND cColor = MinvLine.Color AND LINENO = MinvLine.LINENO
    IF SEEK(cTrn_Seq+cFabric+cColor,'ROLLS')
      SELECT ROLLS
      SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot+crollid = MATINVJL.cTrn_Seq +;
                      MATINVJL.cFabric + MATINVJL.cColor FOR cRsession = MATINVJL.cRsession
        INSERT INTO (lcTmpFile)(Invoice,Account,UOM,Fabric,Color,DESC1,LineNo,Roll_ID,QTY,Price);
        VALUES (MINVHDR.Cmatinv,MINVHDR.Account,MINVLINE.CSellUom,;
                IIF(!EMPTY(MINVLINE.AltFabric),MINVLINE.AltFabric,MINVLINE.Fabric),;
                IIF(!EMPTY(MINVLINE.AltFabric),MINVLINE.AltColor,MINVLINE.Color),MinvLine.DESC1,;
                MinvLine.LineNo,ROLLS.cRollID,ROLLS.NQTY,MinvLine.nSellNetPr)
      ENDSCAN
    ELSE
      INSERT INTO (lcTmpFile)(Invoice,Account,UOM,Fabric,Color,DESC1,LineNo,QTY,Price);
      VALUES (MINVHDR.Cmatinv,MINVHDR.Account,MINVLINE.CSellUom,;
              IIF(!EMPTY(MINVLINE.AltFabric),MINVLINE.AltFabric,MINVLINE.Fabric),;
              IIF(!EMPTY(MINVLINE.AltFabric),MINVLINE.AltColor,MINVLINE.Color),MinvLine.DESC1,;
              MinvLine.LineNo,MinvLine.FABQTY,MinvLine.nSellNetPr)
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnOldAlias)
*--End of Function lfGetRoll.
*!***************************************************************************
*! Name      : lfCloseFil
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Close the opened files
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCloseFil()
*!***************************************************************************
FUNCTION lfCloseFil
PRIVATE lcFiles 
DIMENSION laFiles[3]
STORE '' TO laFiles
lcFiles ='ROLLS,WAREHOUS,MATINVJL'
=gfSubStr(lcFiles,@laFiles,',')
FOR I = 1 TO ALEN(laFiles,1)
  IF USED(laFiles[I])
    USE IN &laFiles[I]
  ENDIF
ENDFOR
RETURN
*--End of Function lfCloseFil.
