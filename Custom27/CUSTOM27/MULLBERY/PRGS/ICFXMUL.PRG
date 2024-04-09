*:************************************************************
*: Program file  : ALFXMUL.PRG
*: Program desc. : Fix data in STYLE file for Mul10,
*:                 Fill the field cDefWare from the stydye file
*: For screen    : Menu
*:        System : Aria Advantage Series.
*:        Module : IC
*:     Developer : tarek Mohammed Ibrahim TMI
*:     Entry     : B606440,1
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO icPack
*:************************************************************
*: Modifications :
*:************************************************************


lnRespns = gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,;
                      'This program will update the STYLE Default WareHouse from the STYDYE file; Are you sure you want to continue?')

IF lnRespns = 1
  *--Open needed files
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
  =gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
  
  SELECT STYLE
  SCAN FOR EMPTY(ALLTRIM(CDEFWARE))
    WAIT WINDOW NOWAIT 'STYLE : ' + STYLE.STYLE  
    IF SEEK(STYLE.STYLE,'STYDYE')
      REPLACE CDEFWARE WITH STYDYE.CWARECODE
    ENDIF
  ENDSCAN  
  WAIT CLEAR
  
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,;
                      "Update process completed." )
ENDIF                      