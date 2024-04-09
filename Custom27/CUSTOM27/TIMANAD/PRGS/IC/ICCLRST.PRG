*:**************************************************************************************
*: Program file  : ICCLRST
*: Program desc. : Custom Program to clear range of styles C#200224
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Hend Ghanem (HBG)
*: Date          : 13/09/2001
*:**************************************************************************************


*--- Open all the needed files

=gfOpenFile(gcDataDir+'ICSTYHST',gcDataDir+'STYHST','SH')
=gfOpenFile(gcDataDir+'ICSTYPOS',gcDataDir+'ICSTYPOS','SH')
=gfOpenFile(gcDataDir+'INVTADJ',gcDataDir+'INVTADJ','SH')
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'STYPRICE',gcDataDir+'STYPRICE','SH')
=gfOpenFile(gcDataDir+'WHSLOC',gcDataDir+'WHSLOCST','SH')
=gfOpenFile(gcDataDir+'WIPADJ',gcDataDir+'WIPADJ','SH')
=gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
=gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'STYLEUPC','SH')
=gfOpenFile(gcDataDir+'RALINE',gcDataDir+'RALINES','SH')
=gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINES','SH')
=gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
=gfOpenFile(gcDataDir+'MDINVNTL',gcDataDir+'MDINVNTLS','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCKLNST','SH')
=gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SKU_STYLE','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLNS','SH')
=gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
=gfOpenFile(gcDataDir+'CTKTRCVL',gcDataDir+'CTKTRCVLC','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINES','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
=gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CONSINVL','SH')
=gfOpenFile(gcDataDir+'POSTRAN',gcDataDir+'POSTRLN','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINES','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
=gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
=gfOpenFile(gcDataDir+'BOMHDR','','SH')
=gfOpenFile(gcDataDir+'BOMVAR','','SH')
=gfOpenFile(gcDataDir+'MFGOPRHD',gcDataDir+'Mfgoprhd','SH')
=gfOpenFile(gcDataDir+'BOMLINE',gcDataDir+'Mfgopr','SH')


lnMajLen  = LEN(gfItemMask("PM"))

WAIT  WINDOW 'Delete ICSTYHST file' NOWAIT 
SELECT ICSTYHST
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete ICSTYPOS file' NOWAIT 
SELECT ICSTYPOS
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete INVTADJ file' NOWAIT 
SELECT INVTADJ
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete STYDYE file' NOWAIT 
SELECT STYDYE
DELETE FOR PADL(STYLE,3) <> '11W' 

WAIT WINDOW 'Delete STYLE file' NOWAIT 
SELECT STYLE
DELETE FOR PADL(STYLE,3) <> '11W'
           
WAIT WINDOW 'Delete STYPRICE file' NOWAIT            
SELECT STYPRICE
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete WHSLOC file' NOWAIT            
SELECT WHSLOC
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete WIPADJ file' NOWAIT            
SELECT WIPADJ           
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete STYINVJL file' NOWAIT            
SELECT STYINVJL
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete STYLEUPC file' NOWAIT            
SELECT STYLEUPC
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete RALINE file' NOWAIT            
SELECT RALINE
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete MDINVNTL file' NOWAIT            
SELECT MDINVNTL 
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete RETLINE And RETHDR file' NOWAIT 
SELECT RETLINE
SCAN
  IF PADL(STYLE,3) <> '11W'
    IF SEEK(RETLINE.CRMEMO,'RETHDR') 
      SELECT RETHDR
      DELETE 
    ENDIF  
    SELECT RETLINE   
  ENDIF         
ENDSCAN
LOCATE
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete SPCK_LIN and SPCK_HDR file' NOWAIT 
*-- Delete from "SPCK_LIN,SPCK_HDR"           
SELECT SPCK_LIN
SCAN
  IF PADL(STYLE,3) <> '11W'
  
     IF SEEK(TYPE+ACCOUNT+STYLE,'SPCK_HDR') 
       SELECT SPCK_HDR
       DELETE 
     ENDIF  
     SELECT SPCK_LIN    
  ENDIF         
ENDSCAN
LOCATE
DELETE FOR PADL(STYLE,3) <> '11W'

WAIT WINDOW 'Delete POSLN and POSHDR and CUTPICK and CTKTRCVL and MFGOPRHD files' NOWAIT 
*-- Delete from "POSLN,POSHDR,CUTPICK,CTKTRCVL"           
SELECT POSLN       
SCAN
  IF PADL(STYLE,3) <> '11W'
  
     IF SEEK(POSLN.cStyType+POSLN.Po,'POSHDR') 
       SELECT POSHDR
       DELETE 
     ENDIF  
     IF SEEK(POSLN.TRANCD+POSLN.PO+POSLN.STYLE,'CUTPICK') 
       SELECT CUTPICK
       DELETE 
     ENDIF  
     IF SEEK('P'+POSLN.PO+POSLN.STYLE,'CTKTRCVL') 
       SELECT CTKTRCVL
       DELETE 
     ENDIF  
     IF SEEK(PADL(POSLN.Style,lnMajLen),'BOM')
       IF SEEK(BOM.typ+POSLN.PO+BOM.coprcode,'MFGOPRHD')
         SELECT MFGOPRHD
         DELETE
       ENDIF  
     ENDIF   
     SELECT POSLN          
  ENDIF         
ENDSCAN
LOCATE
DELETE FOR PADL(STYLE,3) <> '11W'  
           
WAIT WINDOW 'Delete BOM file' NOWAIT 
*-- Delete from "BOM"             
SELECT BOM
DELETE FOR PADL(citmmajor,3) <> '11W'

WAIT WINDOW 'Delete BOMLINE file' NOWAIT 
*-- Delete from "BOMLINE "  
SELECT BOMLINE 
DELETE FOR PADL(Style,3) <> '11W'

           
WAIT WINDOW 'Delete BOMHDR file' NOWAIT            
*-- Delete from "BOMHDR"             
SELECT BOMHDR
DELETE FOR PADL(citmmajor,3) <> '11W' 

WAIT WINDOW 'Delete BOMVAR file' NOWAIT            
*-- Delete from "BOMVAR"  
SELECT BOMVAR
DELETE FOR PADL(citmmajor,3) <> '11W'

WAIT WINDOW 'Delete INVLINE and INVHDR and CONSINVL and POSTRAN files' NOWAIT            
*-- Delete from "INVLINE,INVHDR,CONSINVL,POSTRAN"  
SELECT INVLINE       
SCAN
  IF PADL(STYLE,3) <> '11W'
     IF SEEK(INVLINE.INVOICE,'INVHDR') 
       SELECT INVHDR
       DELETE 
     ENDIF  
     IF SEEK(INVLINE.invoice+INVLINE.store+INVLINE.order+INVLINE.style,'CONSINVL') 
       SELECT CONSINVL
       DELETE 
     ENDIF  
     IF SEEK(INVLINE.invoice+INVLINE.style,'POSTRAN') 
       SELECT POSTRAN
       DELETE 
     ENDIF  
     SELECT INVLINE       
  ENDIF         
ENDSCAN
LOCATE
DELETE FOR PADL(STYLE,3) <> '11W'            
           
WAIT WINDOW 'Delete ORDLINE and ORDHDR and ORDCANLN files' NOWAIT            
*-- Delete from "ORDLINE,ORDHDR,ORDCANLN"  
SELECT ORDLINE       
SCAN
  IF PADL(STYLE,3) <> '11W'
     IF SEEK('O'+ORDLINE.Order,'ORDHDR') 
       SELECT ORDHDR
       DELETE 
     ENDIF  
     IF SEEK('O'+ORDLINE.Order,'ORDCANLN') 
       SELECT ORDCANLN
       DELETE 
     ENDIF  
     SELECT ORDLINE       
  ENDIF         
ENDSCAN
LOCATE
DELETE FOR PADL(STYLE,3) <> '11W'                     


=gfModalGen("TRM00000B00000" , "DIALOG" ,.F.,.F., "Deleting styles completed.")

