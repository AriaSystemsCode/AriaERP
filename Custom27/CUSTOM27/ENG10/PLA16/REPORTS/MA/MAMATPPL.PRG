*:***************************************************************************
*: Program file  : MAMATPPL.PRG
*: Program desc. : CUSTOMIZED Material Purchase Order FOR PLANET.
*: Date          : 01/04/2005
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C125590
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAMATPPL
*:***************************************************************************
*: Modifications :
*****************************************************************************

PRIVATE lcAlias
lcAlias = ALIAS()

IF !USED(lcVendmatl)
  =gfOpenFile(gcDataDir+"Vendmatl","Matcol",'SH', @lcVendmatl, .T.)
ENDIF

SELECT POFLN
INDEX ON CMATTYPE+POMAT+STR(LINENO,6)+FABRIC+COLOR+TRANCD TAG lcPlCstTag OF (gcWorkDir+lcPlCstTag+'.CDX') ADDITIVE
SET RELATION TO vendor + fabric + color + cvenfab + cvencolr INTO (lcVendmatl) ADDITIVE

SELECT (lcAlias)
                       *-- Report Code End  --*
*!*************************************************************
*! Name      : lfMPOEnd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/04/2005
*! Purpose   : Know the record number of line number at which 
*!           : we print Material PO Notepad, and this is because 
*!           : we print memo fields from withen detail band. 
*!*************************************************************
*! Called from : POMAT Group header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMPOEnd()
*!*************************************************************
FUNCTION lfMPOEnd
PARAMETERS llReturn
PRIVATE lcOrder , lcRecNo

IF !llExternal
  PRIVATE lcSelected
  lcSelected = ALIAS()           && Save Current Alias.
  SELECT POFLN_A
  lcOrder = ORDER()
  lcRecNo = EVAL(KEY())
  INDEX ON CMATTYPE+POMAT+STR(LINENO,6)+FABRIC+COLOR+TRANCD TAG lcPOFLN_A OF (gcWorkDir+lcPOFLN_A+'.CDX') ADDITIVE
  SET FILTER TO CMATTYPE+POMAT+STR(LINENO,6)+FABRIC+COLOR+TRANCD+STR(RECNO(),7) = ;
                PoFLn.cmattype + PoFLn.pomat  AND TRANCD = '1'
  GO BOTTOM
  lnMPOEnd = RECNO('POFLN_A')
  SET FILTER TO
  SET ORDER TO TAG &lcOrder
  =SEEK(lcRecNo)
  SELECT (lcSelected)            && Restore Alias.
ENDIF  
llEndGroup  = .F.  && Initially Asure that we does not reach end of outer group.RETURN ''

RETURN ""

*--End of lfMPOEnd.