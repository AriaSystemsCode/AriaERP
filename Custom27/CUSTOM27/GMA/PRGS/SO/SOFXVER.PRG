*!**************************************************************************
*! Name      : SOFXVER.PRG
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/05/2001
*! Purpose   : Fix OrdLine by filling cPkVersion field with '1' if empty
*! Reference : C101946,1
*!**************************************************************************
*

IF gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,; 
   'This program will update version in Ordline file, Are you sure you want to continue?') = 1
  *--Open needed files
  =gfOpenFile(gcDataDir+'OrdLine','','SH')
  REPLACE CPKVERSION WITH STR(1,4)  FOR EMPTY(CPKVERSION) AND !EMPTY(Pack_Id)
  
  *-- Close OrdLine file.
  USE IN OrdLine
  
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,"All packs are updated with version '1'" )
ENDIF
*-- End of program.