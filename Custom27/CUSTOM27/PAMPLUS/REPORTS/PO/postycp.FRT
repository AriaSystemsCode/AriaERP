     !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    E  "  winspool HP LaserJet 2100 LPT1:,\\ariant\hp2                                            �HP LaserJet 2100                 � 0C�  �
od   ,  ,  Letter                                                        ����                DINU"   0  L5a                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     �DRIVER=winspool
DEVICE=HP LaserJet 2100
OUTPUT=LPT1:,\\ariant\hp2
ORIENTATION=0
PAPERSIZE=1
ASCII=1
COPIES=1
DEFAULTSOURCE=1
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=2
COLLATE=1
                             poshdr.cstytype+ poshdr.po                                       SUBSTR(POSLN.STYLE,1,lnRpMajSiz)                                Times New Roman                c"1407 Broadway, Suite 2310, New York, NY 10018. Tel. 212-221-0212. Fax 212-221-0144.WWW.Agapo.com"                               Times New Roman                "Purchase Order"               	poshdr.po                      Times New Roman                Times New Roman                "PO#"                          Times New Roman                "Date Issued:"                 ,(ALLT(lcRpImDir+LEFT(STYLE.STYLE,8))+'.JPG')                    -ALLT(lcImagePath+LEFT(STYLE.STYLE,8))+'.JPG'                    =llRpPic AND FILE(ALLT(lcRpImDir+LEFT(STYLE.STYLE,8))+'.JPG' )                                    Times New Roman                "Description:"                 style.desc1                                                   Times New Roman                Times New Roman                "Style:"                        SUBSTR(POSLN.STYLE,1,lnRpMajSiz)                                                               Times New Roman                Times New Roman                "Quota Cat:"                   Times New Roman                "FOB Price:"                   POSHDR.CFOB                                                   Times New Roman                POSHDR.QUOTACAT                                               Times New Roman                Times New Roman                "Total Quantity:"              poshdr.open                                                   Times New Roman                Times New Roman                "Color"                         SUBSTR(POSLN.style,lnRpMajSiz+2)                                                               Times New Roman                                                                                                             Univers                        "Ship Date:"                   Univers                        "Ship Via:"                    Univers                        "Buyer:"                       PLOOKUP(codes.cdiscrep,'N'+"SHIPVIA   "+poshdr.shipvia,CODES.CCODE_NO,"CCODE_NO")                                                Times New Roman                poshdr.complete                                               Times New Roman                poshdr.entered                                                Times New Roman                poshdr.buyer                                                  Times New Roman                Times New Roman                	"Vendor:"                      apvendor.cvencomp                                             Times New Roman                apvendor.caddress1                                            Courier New                    apvendor.caddress2                                            Courier New                    �ALLT(apvendor.caddress3)+IIF(!EMPTY(apvendor.caddress3),', ','')+ALLT(apvendor.caddress4)+IIF(!EMPTY(apvendor.caddress4),', ','')+ALLT(apvendor.caddress5)+IIF(!EMPTY(apvendor.caddress5),', ','')+ALLT(apvendor.caddress6)                                         Courier New                    Times New Roman                "Total"                                                                                                                                                                                                                                                                                
posln.qty1                                                    Times New Roman                posln.totqty                                                  Courier New                    
posln.qty2                                                    Times New Roman                
posln.qty3                                                    Times New Roman                
posln.qty4                                                    Times New Roman                
posln.qty5                                                    Times New Roman                
posln.qty6                                                    Times New Roman                
posln.qty7                                                    Times New Roman                
posln.qty8                                                    Times New Roman                                               Times New Roman                "Packing Instructions:"        Times New Roman                "Fabric/Trim Information"      Times New Roman                "Main Label:"                  Times New Roman                "Care Label:"                  Times New Roman                "Wash Care:"                   Times New Roman                "Price Ticket:"                Times New Roman                "Placement:"                   Times New Roman                
"Hangtag:"                     Times New Roman                "Special Tag (1):"             Times New Roman                "Special Tag (2):"             Times New Roman                	"Fabric:"                      Times New Roman                	"Lining:"                      Times New Roman                
"Buttons:"                     Times New Roman                	"Zipper:"                      Times New Roman                
"Emb Ref:"                     Times New Roman                "Additional Trim:"             Times New Roman                	"Thread:"                      Times New Roman                "Placement:"                   :LOOKUP(NOTEPAD.MNOTES,'P'+poshdr.po,NOTEPAD.KEY,"NOTEPAD")                                     Times New Roman                	scale.sz1                                                     Times New Roman                	scale.sz2                                                     Times New Roman                	scale.sz3                                                     Times New Roman                	scale.sz4                                                     Times New Roman                	scale.sz5                                                     Times New Roman                	scale.sz6                                                     Times New Roman                	scale.sz7                                                     Times New Roman                	scale.sz8                                                     Times New Roman                lfCodeDesc('CMAINLBL')                                        Times New Roman                lfCodeDesc('CCARELBL')                                        Times New Roman                lfCodeDesc('CWSHCARE')                                        Times New Roman                lfCodeDesc('LPRCTKCT')                                        Times New Roman                lfCodeDesc('CPLACMNT')                                        Times New Roman                lfCodeDesc('CHANGTAG')                                        Times New Roman                lfCodeDesc('CSPECIAL1')                                       Times New Roman                lfCodeDesc('CSPECIAL2')                                       Times New Roman                Times New Roman                "Button Bag:"                  lfCodeDesc('LBUTBAG')                                         Times New Roman                lfCodeDesc('CUSRFAB')                                         Times New Roman                lfCodeDesc('CUSRLINNG')                                       Times New Roman                lfCodeDesc('CUSRBUTTN')                                       Times New Roman                lfCodeDesc('CUSRZIPPR')                                       Times New Roman                lfCodeDesc('CUSRTHRED')                                       Times New Roman                lfCodeDesc('CUSREMBRF')                                       Times New Roman                lfCodeDesc('CUSRADDTRM')                                      Times New Roman                '(ALLT(lcRpImDir+ALLT(lcRpComp))+'.JPG')                         ( (ALLT(lcRpImDir+ALLT(lcRpComp))+'.JPG')                        -FILE((ALLT(lcRpImDir+ALLT(lcRpComp))+'.JPG'))                   Times New Roman                "Placement:"                   lfCodeDesc('CPLCMNT2')                                        Times New Roman                lfCodeDesc('CPLACMNT')                                        Times New Roman                Courier New                    Times New Roman                Times New Roman                Times New Roman                Times New Roman                Univers                        Times New Roman                Times New Roman                Times New Roman                dataenvironment                kLeft = 73
Top = 42
Width = 520
Height = 349
InitialSelectedAlias = "poshdr"
Name = "Dataenvironment"
                      /PROCEDURE OpenTables
FOR lnCount = 1 TO ALEN(laRpFxCur,1)
  IF !EMPTY(laRpFxCur[lnCount,1]) AND FILE(lcRpWrDir+laRpFxCur[lnCount,2]+'.DBF')
    USE (lcRpWrDir+laRpFxCur[lnCount,2]) IN 0 ORDER 1
  ENDIF
ENDFOR
FOR lnCount = 1 TO ALEN(laRpVrCur,1)
  IF !EMPTY(laRpVrCur[lnCount,1]) AND FILE(lcRpWrDir+laRpVrCur[lnCount,2]+'.DBF')
    USE (lcRpWrDir+laRpVrCur[lnCount,2]) IN 0 ORDER 1
  ENDIF
ENDFOR

*USE (lcRpDtDir+'POSHDR') IN 0 ORDER TAG POSHDR
*USE (lcRpDtDir+'POSLN') IN 0 ORDER TAG POSLNW
*USE (lcRpDtDir+'STYLE') IN 0 ORDER TAG STYLE
*USE (lcRpDtDir+'SCALE') IN 0 ORDER TAG SCALE
*USE (lcRpDtDir+'APVENDOR') IN 0 ORDER TAG VENCODE
*SELECT POSLN
*SET RELATION TO STYLE INTO STYLE ADDITIVE
*SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
*SET ORDER TO TAG POSLNW
*SELECT poshdr
*SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
*SET RELATION TO IIF(lcRpForm='A',POSHDR.cSTYType,lcRpForm)+PO INTO POSLN ADDITIVE
*SET SKIP TO POSLN

ENDPROC
PROCEDURE BeforeOpenTables
LOCAL lnCount
lnTotMem = AMEMBERS(aMems,THIS,2)
WITH THIS
  FOR lnCount = 1 TO lnTotMem
    IF UPPER(EVAL("."+aMems[lnCount]+".BaseClass")) = "CURSOR"
      WITH EVAL("."+aMems[lnCount])          
        IF EMPTY(.DATABASE)
          lcCurSource = .CURSORSOURCE
          lcCurSource = IIF(ATC('\',lcCurSource)=0,lcCurSource,SUBSTR(lcCurSource,RAT('\',lcCurSource)+1))
          .CURSORSOURCE = IIF(UPPER(LEFT(lcCurSource,2))='SY',lcRpSysDir,lcRpDtDir;
                          )+lcCurSource
        ENDIF  
      ENDWITH 
    ENDIF
  ENDFOR
ENDWITH


ENDPROC
                               ����    �  �                        ��   %   �      >               �  U  �  ��  ���(�C�� ����v �5 %�CC �  �� �
� C� C �  �� � .DBF0	��r �  Q�  �� C �  �� ��� 1� � �� ��  ���(�C�� ����� �5 %�CC �  �� �
� C� C �  �� � .DBF0	��� �  Q�  �� C �  �� ��� 1� � �� U  LNCOUNT	 LARPFXCUR	 LCRPWRDIR	 LARPVRCUR% ��  � T� �C�� � ��b�� ��� ��� ��  ���(�� ���3 %�CC� .C �  � �
 .BaseClass�f� CURSOR��� ��C� .C �  � ���� %�C�� ���� T� ��� ��4 T� �CC� \� �� � � � C� C� \� ��\6��- T�� �CCC� �=f� SY� � � � 6� �� � �� � �� �� U	  LNCOUNT LNTOTMEM AMEMS THIS DATABASE LCCURSOURCE CURSORSOURCE
 LCRPSYSDIR	 LCRPDTDIR
 OpenTables,     �� BeforeOpenTablesQ    ��1 �QA A �QA A  1 q �� q1�� A�A A A A A 3                       �        �  $      )   �                             cursor                        �Left = 4
Top = 57
Width = 90
Height = 90
Alias = "poshdr"
Order = "poshdr"
CursorSource = d:\aria27\dbfs\99\poshdr.dbf
Name = "Cursor1"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   cursor                         �Left = 150
Top = 10
Width = 90
Height = 90
Alias = "posln"
Order = "poslnw"
CursorSource = d:\aria27\dbfs\99\posln.dbf
Name = "Cursor2"
                   cursor                         �Left = 269
Top = 34
Width = 90
Height = 90
Alias = "style"
Order = "cstyle"
CursorSource = d:\aria27\dbfs\99\style.dbf
Name = "Cursor3"
                   cursor                         �Left = 246
Top = 181
Width = 90
Height = 90
Alias = "scale"
Order = "scale"
CursorSource = d:\aria27\dbfs\99\scale.dbf
Name = "Cursor4"
                   cursor                         �Left = 190
Top = 121
Width = 90
Height = 90
Alias = "apvendor"
Order = "vencode"
CursorSource = d:\aria27\dbfs\99\apvendor.dbf
Name = "Cursor5"
           relation                       qParentAlias = "posln"
RelationalExpr = "style"
ChildAlias = "style"
ChildOrder = "style"
Name = "Relation1"
                 relation                       uParentAlias = "posln"
RelationalExpr = "'S'+SCALE"
ChildAlias = "scale"
ChildOrder = "scale"
Name = "Relation2"
             relation                       xParentAlias = "poshdr"
RelationalExpr = "vendor"
ChildAlias = "apvendor"
ChildOrder = "vencode"
Name = "Relation3"
          relation                       �ParentAlias = "poshdr"
RelationalExpr = "IIF(lcRpForm='A',POSHDR.cSTYType,lcRpForm)+PO"
ChildAlias = "posln"
ChildOrder = "posln"
OneToMany = .T.
Name = "Relation4"
                         cursor                         �Left = 381
Top = 26
Width = 90
Height = 90
Alias = "codes"
Order = "ccode_no"
CursorSource = d:\aria27\dbfs\99\codes.dbf
Filter = (codes.crltfield='N')
Name = "Cursor6"
                  cursor                         �Left = 375
Top = 184
Width = 90
Height = 90
Alias = "notepad"
Order = "notepad"
CursorSource = d:\aria27\dbfs\99\notepad.dbf
Name = "Cursor7"
              