����  =j  Zi  �                     �    J  �  �   �  �{E     4�  � � � T� �F�	 MEMOWIDTHz�� G$(����� 5� � %�F� \�  �� ��� � T� �F� FULLPATHz�� GQ �O T�  �FF� �  � .FRX2� F� �  � .LBX2�
 � �  � � � � \�  8�� SET FULL &lcFullSet
 �4 T� �FF� \�  �� � F�  F� \�  ��`� �  8�� %�F� �	 � .FRX2��t�& ERASE &gcWorkDir.&lcOGTmpForm..FRX
( ERASE &gcWorkDir.&lcOGTmpForm..FRT  
 � %�F� �	 � .LBX2����& ERASE &gcWorkDir.&lcOGTmpForm..LBX
( ERASE &gcWorkDir.&lcOGTmpForm..LBT  
 �, %�FF�  �� .FRX2� FF�  �� .LBX2��� %�FF�  �� .FRX2����t SELECT * FROM &lcFormName..FRX WHERE PLATFORM=lcOGPlatForm AND objtype<>10 INTO DBF &gcWorkDir.&lcOGTmpForm..FRX
 ��t SELECT * FROM &lcFormName..LBX WHERE PLATFORM=lcOGPlatForm AND objtype<>10 INTO DBF &gcWorkDir.&lcOGTmpForm..LBX
 � �;� J��  �(�
 �	 � B� � %�� ����- �F� 
� F� ����	� F� � ��	��O >� ��F� �� ��F� �� ��F� �� ��F� ��F� ����� F� � ��	�� � %�F�	 ����� Q��	 �� � %�F� ���� Q�� �� � G$(�� �� U LCFORMNAMELCARRAYNAMLLCHKCHANGLNMEMOWID LCFILENAMELCFULLSET GCREPHOME GCACT_APPLGCWORKDIR LCOGTMPFORLCOGFORMV LFCANDISP OBJTYPE ��OBJCODE ��VPOS �    LFOGVPOS  HPOS      LFOGHPOS  HEIGHT    LFOGHSIZE WIDTH � z LFOGWSIZE FOR      �  4�  � T� �F�� �  ���) T� �F� � � F�� � ��� � 8��? RETURN IIF(lnArrPos=0,'',ALLTRIM(&lcArrayName[lnArrPos,6]))
 U LNRECNO MELNARRPOS MLAOGOBJPOS�  4�  � T�� �� ��! %�F� #VPOSITION � �� ��� �" T� �F� F� #VPOSITION � ΐ��( T� �FF� F� #VPOSITION � ��
`��� T�� �F� ��� � B��� �� U LNOBJPOS EYPOS POS MVPOS BJPOSCOMMENT  SLCCLAUS MELCSIZE  T �  4�  � T�� �� ��! %�F� #HPOSITION � �� ��� �" T� �F� F� #HPOSITION � ΐ��( T� �FF� F� #HPOSITION � ��
`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EXPOS POS MHPOS BJPOSCOMMENT  SLCCLAUS MELCSIZE  T LCOGPLATFOFONTFACE OFONTSIZE  �  4�  � T�� �� �� T� �F� #HSIZE � ��� %�� � ��� � T� �F� F� #HSIZE � ΐ��$ T� �FF� F� #HSIZE � ��`��� T�� �F� ��� � B��� �� U LNOBJPOS EHEIGHT S MLNCLAUS OSCOMMENT  SLCCLAUS MELCSIZE  T �  4�  � T�� �� �� T� �F� #WSIZE � ��� %�� � ��� � T� �F� F� #WSIZE � ΐ��$ T� �FF� F� #WSIZE � ��`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EWIDTH  S MLNCLAUS OSCOMMENT  SLCCLAUS MELCSIZE  T LCOGPLATFOFONTFACE OFONTSIZE  | T�  ��  �� T� �e�� %�F� �
��� � %�F� #OBJDISP� �� ��� �G T�  �FF� F� #OBJDISP� ΐF� #OBJDISPF� F� #OBJDISP� ΐ��	`�� � � %�F�  �
��l� T� �F� #OBJDISPF� j��� �� ���(�� ��h�$ T�  �FF� F� #OBJDISP� � �`���G T�  �FF�  F� #OBJDISP�  ΐF� #OBJDISPF�  F� #OBJDISP�  ΐ��	`�� T� �F�  ��� %�� 
��d� !� � �� �	 B�� �� U LCCONDITIOLLCANDISP COMMENT OSLNNOCOND OLNCOUNT ME( 4�  � 5� %�F� (�  �� ��2 � B�F� ,�  ��� �� T� �� �� T� �� �� �� ���(�F�  @��� ��z �� �F�  � �`� (��� � T� �� ��� �F�  � �`� )��� � T� �� ��� �F�  � �`� ,��� %�� � ��� T� �� �� !� � � �� �	 B�� �� U LCSTRING OLNCOUTS   LNCOMPOS SLNCOUNT  OLFMODISTYL   LFOGVPOS  A�  LFOGHPOS   �  LFOGHSIZE �  LFOGWSIZE �  LFCANDISP  D
  LFGETCOM   �  � �� q qqa �qA A�a�A �a�A ��A� AA � A A � ��C � � A � � A � 2 q A��2 q � !�A � 2 q � !��A � 2 q � ��AA � 2 q � ��A�A � 2 � � �qA A �qAq� � A A A A � 2 q Q q� � � � �� ���� A A A A A � 1             )   �  ,   .   a  2   7   �  <   A   )
  G   K   X  R   V     ^   j     s       �-  {1  �  �-  m{EC   � 4�  � � � � �- T� �FF�	 lcRprtLblf� UL� � R� � 8�� G*(� G(&�, T� �FF� llPrntToFilef� L� � � /8��% %�� � �2	� � � PRINTER	��� � T� �/�� �6 T� �FF�
 lcCriteriaf� C� F� �
	� � � �  8�� 5� � � �	 �
 � T�	 �/��: T�  �FF� lcOGTmpFormf� C� F� �� �  � � � 8�� T�
 �F� DEFAULTz�� %�F� \�  �� ���� T� �F� FULLPATHz�� GQ �O T�  �FF� �  � .FRX2� F� �  � .LBX2�
 � �  � � � � \�  8�� SET FULL &lcFullSet
 � J��  �(� � � T� �FFRFQ� � � FR8�� T� �F�
 lcCriteria� ��& T� �F� WHILE � �F� FOR � ��� %�F� WHILE F� j�� ����@ T� �F� � F� �F� FOR � ��`� F� F� WHILE � �`8�� � %�F� FOR F� j�� ��7�T T� �F� � F� F� FOR � ��`�* F� F� FOR � ��F� � �F� FOR � �`8�� � T� �FQ� �� %�F� �
� F� �
���� %�F� ���y� #)� � T� �� �  � �� LOCATE &lcLoctCond
 T� �F6�� � %�� � ����	 #�� �� � %�� 
���$ ��F� INM00052B00000� DIALOG� �� B� � T� �F� ERROR��� {�  �@ T� �F�2� � � PRINTER	� � lfChckPrtStat() AND � �  8�� ��{��� �� � PRINTER��j� %�� ���u T� �� \\SPOOLER\P=FF� ^�� \S=F� �� \Q=F� �F��� � \C=FF�^�� �  8F�  � � \B=�! � � \N8�� �*� T� ��" �� � %��2��X� SET PRINTER TO &lcPrintSet
 �f� G*(� �& �� � SCREEN� � � FILE���� SET PRINTER TO &gcOutFile
 � T�# �F� CURSORz�� G]� %��2� � 
	��#�* %�� � PRINTER� F�a� READY	��Y� %��� BOTH,BEFORE��U� T�$ ���� T��� FORMFEED�� � T���$ �� � � %�� � SCREEN���� T� ��(�� T�	��  �� T�(��  �� � 5� � �% � T� �FQ�� T� ����RlcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1)])+ ' '+lcOGFOR
 T����� T�& �F��a�� T�' �� ��, s,��& �������(������������ t,���& �� z,��& ���0 �� ������� Printing Page 1���������5 ��������� Press [ESC] to abort��������� t,)��& �� %��( ���	� %�� � L��T	�A LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria    
 ��	�G REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
 � �U
� %�� � L���	�7 LABEL FORM (lcOGRprtNam) TO PRINTER &lcCriteria    
 �Q
�Q REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE PLAIN NOEJECT  &lcCriteria    
 � � <,��& �� %�� � SCREEN���
� T�(�� �� �* %�� � PRINTER� F�a� READY	��� %���
 BOTH,AFTER���
� T�$ ���� T��� FORMFEED�� � T���$ �� � � %�� 
���
 ��F�) �� � ���F T� �FF� ��	 � ALL� � 8�  FF� �� �  � � �  8�  � �� ��~�|� �� � SCREEN����% %��* � WINDOW� �* � MAC���� %��( ��D� %�� � L���/ LABEL FORM &lcOGRprtNam PREVIEW &lcCriteria
 �@�0 REPORT FORM &lcOGRprtNam PREVIEW &lcCriteria
 � ��� %�� � L����> LABEL  FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
 ���> REPORT FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
 � � ��� T� ��(�� T�	��  �� T�(��  ��LlcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+ ' '+lcOGFor      
 T����� T�& �F��a�� T�' �� ��] s,��& �������(������@�� MS SANS SERIF����A�� B�B����������������� t,���& �� z,��& ���0 ��������� Printing Page 1���������5 ��������� Press [ESC] to abort��������� t,)��& �� %��( ��� %�� � L����< LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria
 ��F REPORT FORM &lcOGRprtNam TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
 � ��� %�� � L��j�2 LABEL FORM &lcOGRprtNam TO PRINTER &lcCriteria
 ���B REPORT FORM &lcOGRprtNam TO PRINTER PLAIN NOEJECT  &lcCriteria
 � � <,��& �� %�� 
����
 ��F�) �� � T�(�� �� � �� � PRINTER����J T�+ �F�5� F� ARIAPRN7�* FF� ARIAPRN7� LIB� �  � F� ARIAPRN788��# %��* � DOS� �* � UNIX���� T�, �� F�- � .TXT�� G*(��, �� %��� BOTH,BEFORE���� T�$ ���� T��� FORMFEED�� � T���$ �� � v�`� %��( ���� %�� � L��Q�= LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
 ���G REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
 � �\� %�� � L����A LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
 �X�Q REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
 � � w� %��� BOTH,BEFORE���� T�$ ���� T��� FORMFEED�� � T���$ �� � G*(� G*(��" �� T�. �/�� T�/ �F� ERROR��� 1� llError=.T.� T�0 �F� SAFEz�� G.� T�1 ��(�� T�(��  �� +�F�a� OFFLINE����+ %�F� QRM00054B00015� ALERT� ���~� T�. �e�� !� � � %�F�, 2� �. /	��O� T�2 ��" �� ����K�- ��+ � ???�
 �+ � ??�	 �+ � ?��Z� T�3 �F�- �� T�4 �FZ��' CREATE CURS &lcTmpCur (mprnfile M) 
 � �5 ��, �� G*(� %�� ����u T� �� \\SPOOLER\P=FF� ^�� \S=F� �� \Q=F� �F��� � \C=FF�^�� �  8F�  � � \B=�! � � \N8��& SET PRINTER TO &lcPrintSet        
 ��� G*(��2 �� � G* � T�6 �F� MEMOWz�� T�7 �F� CONSz�� T�8 ���� T�9 ���� T�: ���� T�; ���� G
� G$(��A��
 T��e�� T������ T��� �� T����� ��< ���(������( &lcPrntTool MPRNFILE FUNCTION 'V253'
 �� G*� G*(� SET CONS &lcConsSet
 G$(��6 �� T���8 �� T���9 �� T���: �� T���; �� USE IN &lcTmpCur

 F��4 �� ��+ � COPY���� G*(��2 �� ��< ���(������ !COPY &gcOutFile &gcLoclPRT
 �� ��+ � LIB����. %�F� TXTSPOOL.FLL2� F�
 PRTWIN.EXE2	���� Gb(� TXTSPOOL.FLL�% T�? �� IBM Proprinter on F�2 ��� T�@ �F �? �A �� ��< ���(����v� ��F �, � ARIA SYSTEMS�= �� �� ��F �@ �A �� <�� TXTSPOOL.FLL� � ��+ � TYPE��� T�B �F� HEADINGz�� T�6 �F� MEMOWz�� T�7 �F� CONSz�� T�8 ���� T�: ���� T�; ���� G$(�����
 T��/�� T������ T��� �� G� G
� T� ��(�� T�	��  �� T�(��  �� G*(��" �� ��< ���(������ O(!��, �� �� G*(� SET HEADING &lcHeadSet
 SET CONS &lcConsSet
 G$(��6 �� T�(�� �� T���8 �� T���: �� T���; �� 2�K� ��< ���(����G� ��, �(��" �� �� � � ON ERROR &lcOnErr
 SET SAFE &lcSetSafe    
 T�(��1 ��
  ��, �� ��� %��� NONE���� � v��� %��( ���� %�� � L���= LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
 ��� %��� NONE����G REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
 ���? REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE &lcCriteria 
 � � ��� %�� � L��*�A LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
 ��� %��� NONE����Q REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
 ���I REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE PLAIN &lcCriteria     
 � � � w� � �� � FILE��|� %��( ���� %�� � L��m�= LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
 ���G REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
 � �x� %�� � L���A LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
 �t�Q REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
 � � � � G*(� SET CURSOR &lcSetCur
 ON ERROR &lcOldErrHnd
 SET DEFAULT TO &lcSavDFDir
 <�
 � G(&� UC LCOGRPRTNALCCRITERIALLENDJOB  LCRPRTLBL LLPRNTTOFIGCDEVICE ILCTEMPPR ALNOGCOUNT LNOGTOTAL LLQUITING LCSAVDFDIRLCOGTMPFORGCWORKDIR LCFULLSET GCREPHOME GCACT_APPLLCOGWHILE LCOGFOR   LNDSRECNO LCOGSCOPE LFGETSCOPELLWHILEFRSLLCANDISP LCLOCTCONDGFMODALGENLCOLDERRHNLCPRNTERROGLPRNT_LANLCPRINTSETGNPRNT_NO GCSERV_NAMGCQUEU_NAMGLBANER   GCBANER_H GCPRNT_PORLCSETCUR  LCADDV E ILCOGSETESCLCTEMPWIN LNPPAGE   GLHEADER  GFENDPRN  LCOGPLATFOLCPRNTTOOLGCOUTFILE GFTEMPNAMELLERROR ORLCONERR   LCSETSAFE LCPDSETUP GCLOCLPRT LCTMPCUR LLCWORKA MEMPRNFILE  LNMEMOWID LCCONSSET LLWRAPSTATLNINDENT  LNRMARGIN LNLMARGIN LNNOCOPY  TXTSPOOL LFLL       LCWINPRT  LCORGPRNT PRTWIN    LCHEADSET �  4�  � T� �e�� T� ��  �� �� ���(�F�  @��� � %�F�  � �`� '"��a � T� �� 
�� �1 T� �� F� � FF�  � �`j� F�  � �`8�� ��	 B�� �� U LCOGCRITERLLTOUP RIALCOGUPCRITLNOGCOUNT � 4�  � 5� � T� �F� EXACTz�� G � T� ��  ��, &lcOGCriteria=lfCondUpper(&lcOGCriteria)
n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
 lcCriteria = &lcOGCriteria
X lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' ALL '),'ALL ','')
8 &lcOGCriteria = STRTRAN(&lcOGCriteria,' ALL ' , ' ')
n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
Z lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' REST '),'REST ','')
; &lcOGCriteria = STRTRAN(&lcOGCriteria , ' REST ' , ' ')
n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
* IF ATC(' RECORD ',&lcOGCriteria)>0�{�L lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria)+8))
 T� �� �  �� T� �F� �F�  � ��`��! T� �� � RECORD � �  ��� lcOGRec = SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria),(ATC(' RECORD ',&lcOGCriteria)+LEN(lcOGrcCont)+8)-ATC(' RECORD ',&lcOGCriteria))
4 &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
 �n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
( IF ATC(' NEXT ',&lcOGCriteria)>0�z�J lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria)+6))
 T� �� �  �� T� �F� �F�  � ��`��+ T� �� FF� �
� � NEXT � � �  8��� lcOGRec = SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria),(ATC(' NEXT ',&lcOGCriteria)+LEN(lcOGrcCont)+6)-ATC(' NEXT ',&lcOGCriteria))
4 &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
 � SET EXACT &lcOGExact
	 B�� �� U LCOGCRITERLCOGEXACT LCOGSCOPE LCOGREC T LCOGRCCONT� 4�  � 5� � T� �F� EXACTz�� G � T� ��  �� T� �F �  � ��/ T� �� F�  F�  � ALL��
 � ALL � �  8��1 T� �� F�  F�  � REST�� � REST � �  8�� %�F� RECORD �  �� ��(�$ T� �FF�  F� RECORD �  ��`��� T� �F� �F�  � ��`��! T� �� � RECORD � �  �� � %�F� NEXT �  �� ����" T� �FF�  F� NEXT �  ��`��� T� �F� �F�  � ��`��+ T� �� FF� �
� � NEXT � � �  8�� � SET EXACT &lcOGExact
	 B�� �� U LCOGCRITERLCOGEXACT LCOGSCOPE LCCRITERIALFCONDUPPELCOGREC ERLCOGRCCONT�  4�  � � �� WAIT SUBSTR(SPACE(LEN(lcMessage))+lcMessage+SPACE(LEN(lcMessage)),MOD(&lnRpCount,LEN(lcMessage)*2),LEN(lcMessage)) WINDOW NOWAIT 
! &lnRpCount=&lnRpCount+1      
 B�e�� U LNRPTOTAL LNRPCOUNT LCMESSAGE ?  4�  � �1 B�FF��� /� F�  ���� F� ���	
8�� U LNMROW AL LNMCOL NT �  5�  � T�  �F��� t,��� ��P �F�2� � � �8��F�2� �� �8���� Printing Page FF�^���������� T� ���� %�F�  �
��� � t,��  �� � U LCOUTWIN  LCTEMPWIN LNPPAGE E   B�F� PRINTERz� ON�� U  LFCONDUPPE �!  LFGETSCOPE |"  LFGETCOPE1 T)  LFRPTHERMO y+  LFQUITREPO Z,  LFDISPPAGE �,  LFCHCKPRTS p-  q�a q �Q� A aq� �aqqa �qA ��a�A �AA �� Q A aa� A � A � AA A Q� � �� Q� � A � �� a A a�A Qa A��� AA � A A q� � � A � � � !� � �� � Q� � !� qA � !q� A A � q� A ��� AA � A A � � A � a� qQ� !�� A � !�� �A A � � � � �� � �� � Q� � !�� aA � !!� !A A � � � A � A ��1�� �� AA � A � � !�� qA � !� A A A �� AA � A a � � Q11a � � ��� A A A �� � �� � qQ � a � Qa� � A a A1� � � � a � � � � � a�A a a q� � � � � A� Q� a�A A�aQ!a�A � AA QaA1� � � � � � � a a � � � � a� A a �q� � � � � � a!A A A Q�� � � AA � � !�� Aq� �A A � !� A� �A A A A A Q� !�� qA � !� A A A A a ���q q 2 q � � ��� A A � 2 q q Aa � �����������!�	AA ���!���AA �� 2 q q Aa � !��A�A �!��A �� 2 � Qq 2 � 2 q � � � � A 2 �1             l  :  m  w  o;  y  �  �B  �  �  &F  �  �  G  �  �  �G  �  �  �H  �      [  �  v   �  m{Ec   � 4�  � � DIMENSION &lcArrayName[1]
 STORE .F. TO &lcArrayName
 T� �F�	 MEMOWIDTHz�� G$(����� 5� � %�F� \�  �� ��� � T� �F� FULLPATHz�� GQ �: T�  �FF� �  � .FRX2�
 � �  � � � � \�  8�� SET FULL &lcFullSet
 �4 T� �FF� \�  �� � F�  F� \�  ��`� �  8�� %�F� �
��]� F� � USE &lcFormName..FRX  
 � %�F� ���o�� SELECT VPOS,HPOS,lfCanDisp() FROM &lcFormName..FRX WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7 AND PLATFORM = lcOGPlatForm ORDER BY VPOS,HPOS INTO ARRAY &lcArrayName
  =gfADel(@&lcArrayName,1,2)  
. =gfADel(@&lcArrayName,1,2)                
 � %�F� ����� Q�� �� � %�F� ����� Q�� �� � G$(�� �� U LCFORMNAMELCARRAYNAMLNMEMOWID LCFILENAMELCFULLSET GCREPHOME GCACT_APPLLCOGTMPFOR�  4�  � T� �F�� �  ���) T� �F� � � F�� � ��� � 8��? RETURN IIF(lnArrPos=0,'',ALLTRIM(&lcArrayName[lnArrPos,6]))
 U LNRECNO MELNARRPOS MLAOGOBJPOS�  4�  � T�� �� ��! %�F� #VPOSITION � �� ��� �" T� �F� F� #VPOSITION � ΐ��( T� �FF� F� #VPOSITION � ��
`��� T�� �F� ��� � B��� �� U LNOBJPOS EYPOS POS MVPOS BJPOSCOMMENT  SLCCLAUS T LCSIZE  E �  4�  � T�� �� ��! %�F� #HPOSITION � �� ��� �" T� �F� F� #HPOSITION � ΐ��( T� �FF� F� #HPOSITION � ��
`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EXPOS POS MHPOS BJPOSCOMMENT  SLCCLAUS T LCSIZE  E LCOGPLATFOFONTFACE OFONTSIZE  �  4�  � T�� �� �� T� �F� #HSIZE � ��� %�� � ��� � T� �F� F� #HSIZE � ΐ��$ T� �FF� F� #HSIZE � ��`��� T�� �F� ��� � B��� �� U LNOBJPOS EHEIGHT S MLNCLAUS OSCOMMENT  SLCCLAUS T LCSIZE  E �  4�  � T�� �� �� T� �F� #WSIZE � ��� %�� � ��� � T� �F� F� #WSIZE � ΐ��$ T� �FF� F� #WSIZE � ��`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EWIDTH  S MLNCLAUS OSCOMMENT  SLCCLAUS T LCSIZE  E LCOGPLATFOFONTFACE OFONTSIZE  | T�  ��  �� T� �e�� %�F� �
��� � %�F� #OBJDISP� �� ��� �G T�  �FF� F� #OBJDISP� ΐF� #OBJDISPF� F� #OBJDISP� ΐ��	`�� � � %�F�  �
��l� T� �F� #OBJDISPF� j��� �� ���(�� ��h�$ T�  �FF� F� #OBJDISP� � �`���G T�  �FF�  F� #OBJDISP�  ΐF� #OBJDISPF�  F� #OBJDISP�  ΐ��	`�� T� �F�  ��� %�� 
��d� !� � �� �	 B�� �� U LCCONDITIOLLCANDISP COMMENT OSLNNOCOND OLNCOUNT T ( 4�  � 5� %�F� (�  �� ��2 � B�F� ,�  ��� �� T� �� �� T� �� �� �� ���(�F�  @��� ��z �� �F�  � �`� (��� � T� �� ��� �F�  � �`� )��� � T� �� ��� �F�  � �`� ,��� %�� � ��� T� �� �� !� � � �� �	 B�� �� U LCSTRING OLNCOUTS   LNCOMPOS SLNCOUNT  OLFMODISTYL /  LFOGVPOS ��  LFOGHPOS  ͻ  LFOGHSIZE   LFOGWSIZE �  LFCANDISP FU  LFGETCOM  F
  � ���� q qqa �qA Aq �A � �A � � A � � A � 2 q A��2 q � !�A � 2 q � !��A � 2 q � ��AA � 2 q � ��A�A � 2 � � �qA A �qAq� � A A A A � 2 q Q q� � � � �� ���� A A A A A � 1                     #   �  %   ,   �  /   6   �  :   @   �  E   K   �
  Q   _   �  f       W	  1
  O   �	  m{E�   5 4�  � � 5� � � T� �F�	 MEMOWIDTHz�� G$(����� T� �FZ��2 DIMENSION &lcArrayName[1,ALEN(&lcArrayName,2)]
 STORE '' TO &lcArrayName
 5� � %�F� \�  �� ��� T� �F� FULLPATHz�� GQ �: T�  �FF� �  � .FRX2�
 � �  � � �	 � \�  8�� SET FULL &lcFullSet
 �4 T� �FF� \�  �� � F�  F� \�  ��`� �  8�� %�F� �
���� F� � USE &lcFormName..FRX  
 � -��
 � �� %�F6
���� T� �� DOS�� � %�F� �����# %�� � DOS� � � UNIX����� SELECT EXPR,lfOGVPos() ,lfOGHPos(), lfOGHSize(),lfOGWSize(), STYLE,objtype,objcode FROM &lcFormName..FRX WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7 AND PLATFORM = lcOGPlatForm ORDER BY VPOS,HPOS INTO ARRAY &lcArrayName
 ���� SELECT EXPR,lfOGVPos() ,lfOGHPos(), lfOGHSize(),lfOGWSize(), FONTFACE,FONTSTYLE,FONTSIZE,objtype,objcode FROM &lcFormName..FRX WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7 AND PLATFORM = lcOGPlatForm ORDER BY VPOS,HPOS INTO ARRAY &lcArrayName
 � � %�F� ����� Q�� �� � %�F� ���� Q�� �� �
 F�� �� G$(�� �� U LCFORMNAMELCARRAYNAMLCERRHAND LLERRHAPENLNMEMOWID LCOGSELECTLCFILENAMELCFULLSET GCREPHOME GCACT_APPLPLATFORM ELCOGPLATFOLCOGTMPFOR�  4�  � T�� �� ��! %�F� #VPOSITION � �� ��� �" T� �F� F� #VPOSITION � ΐ��( T� �FF� F� #VPOSITION � ��
`��� T�� �F� ��� � B��� �� U LNOBJPOS EYPOS AYNAMVPOS HAND COMMENT ENLCCLAUS D LCSIZE  CT�  4�  � T�� �� ��! %�F� #HPOSITION � �� ��� �" T� �F� F� #HPOSITION � ΐ��( T� �FF� F� #HPOSITION � ��
`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EXPOS AYNAMHPOS HAND COMMENT ENLCCLAUS D LCSIZE  CTLCOGPLATFOFONTFACE OFONTSIZE  �  4�  � T�� �� �� T� �F� #HSIZE � ��� %�� � ��� � T� �F� F� #HSIZE � ΐ��$ T� �FF� F� #HSIZE � ��`��� T�� �F� ��� � B��� �� U LNOBJPOS EHEIGHT NAMLNCLAUS D COMMENT ENLCCLAUS D LCSIZE  CT�  4�  � T�� �� �� T� �F� #WSIZE � ��� %�� � ��� � T� �F� F� #WSIZE � ΐ��$ T� �FF� F� #WSIZE � ��`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EWIDTH  NAMLNCLAUS D COMMENT ENLCCLAUS D LCSIZE  CTLCOGPLATFOFONTFACE OFONTSIZE  LFOGVPOS T_�  LFOGHPOS R��  LFOGHSIZE   LFOGWSIZE  �  � � �� � !�q qqa �qA Aq �A � � A � 1q� �A A � � A � � A � � 2 q � !�A � 2 q � !��A � 2 q � ��AA � 2 q � ��A�A � 1             '   �  (   0     2   :   �  =   D   �	  H       �    �   �  m{E�   � 4�  � � � 5� � 5� � T� �/�� T� �FF��� /� � 8�� %�� ��Y� T� �FZ�� %�F� SYDREPRT�
��� � F� �# USE &gcSysHome.SYDREPRT ORDER 1
 T� �e�� �� � F� �
 G((� 1� � T� ��  �� ��FF�  �ǚ��  -�� � A40�+��	 F�  ����. T� �FFF�
 �
� FF�
 ����
 FF�  ���8��� %�� 
� � 	��Q�
 ��F� �� �
 F�� �� T� �F� FULLPATHz�� GQ �q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� SET FULL &lcFullSet
 T� �� �� T� �F� CENTURYz�� G� DO &lcSetRpPrg
 SET CENTURY &lcSetCent
 ��� 5� � � � � � � � ����# � ������� �������& J��  �(� � � � � � �  � J��  �(� � � � � �c �! �������" �������# �������$ �������% �������& ������� �' ������� �( ������� T�) �� N�� %�F� SYDREPRT�
���� F� �# USE &gcSysHome.SYDREPRT ORDER 1
 T� �e�� ��� F� �
 G((� 1� � T� ��  �� ��FF�  �ǚ��  -�� � A40�+��	 F�  ����. T� �FFF�
 �
� FF�
 ����
 FF�  ���8��� T� �F� FULLPATHz�� GQ �q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� SET FULL &lcFullSet
 T� �F�	 PROCEDUREz�� T�* �F� ERROR��� 1� llDumErr = .T.� G+(�F� ��� ON ERROR &lcOldErr
 %�F�+ ���� ��F �  / � �, �� %�� ���� Q� � � � G+(�� �� %�F�
 laPrgTempsf� U���� ��- ���(�F�� ������ %�FF �- �� �
���� %�FF �- �� ���/� Q�F �- �� �� �  ��. F �- �� � .DBF��  ��. F �- �� � .FPT��  ��. F �- �� � .CDX��  ��. F �- �� � .TXT��  ��. F �- �� � .TMP�� � �� � � U/ LCRPPRG  ELLDOREPORTLCREPDIR  LCSETCENT LLREPFILE LCREPALIASSYDREPRT  LCSETRPPRGCVER RG   CREP_ID PLCMAINREPIDLLFRXFORM LAPPARELRPGFSETDEVICLCFULLSET GCREPHOME GCACT_APPLQCOMPANY GGCCOM_NAMELCBRFIELDSLCBASEFILELAFIELD_H LAFIELD_N LCFILE_TTLLCSETPROCDLAPRGTEMPSLCRPFORM  LCRPFILES LCRPEXPR  LCRPTARGT LCRPORDER LCRPFIELDSLCRPEXP   LARPMATYPELARPSTYPE LARPCTYPE LARPETYPE LARPITYPE LARPTTYPE LARPCOL   LATEMPCOL R_WIDTH   LCOLDERR DLFPREGRD GGFOPGRID  LNCOUNT CDGCWORKDIR �  4�  � � � 5� �: T� �FF�	 lcMessagef� C� � � � Collecting Data8�� &lnRpCount = &lnRpCount + 1
 T� �F� ESCAPEz�� G�  IF &lnRpCount<=lnRpTotal�� �1 =gfThermo(lnRpTotal,&lnRpCount,lcMessage,' ')
 � SET ESCAPE &lcOldEsc
 U LNRPTOTAL LNRPCOUNT LCMESSAGE LCOLDESC  �  4�  � � � �( T� �FF� lnStepf� N� � � �8�� T� �F� �� �� � 8��: T� �FF�	 lcMessagef� C� � � � Collecting Data8�� �� �� �(��  ���� ��� � ��F �   �  � �  � �� �� %�F�	 gwdThermo���� � <,� � � U LNRPTOTAL LNRPCOUNT LCMESSAGE LNSTEP C  LNCOUNTER GFTHERMO  GWDTHERMO !  R,:�� Collecting data... �� U    U   	 B��  �� U LCFORMNAME`  4�  � %�� 
��O �: T�  �FF� �  � .FXP2�
 � �  � � � � \�  8�� �
 B�� 
�� U LCFORM AMELLFRXFORM GCREPHOME GCACT_APPL�  5�  � %�F� �
��� � T�  �F� ���q T�  �FF� �  � .FXP2�
 � �  �G FF� � � \�  � .FXP2� � � � \�  � � F�  �?� \�  88��
 ��  �� � U LCPROGTODOLCOPTPROG GCREPHOME GCACT_APPLY 4�  � � � 5� � %�F�  �
��R� T� �F� \�� %�F� �� F� �
	��[ � T� �� �� � %�F� �
��N�q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� %�F� ���� ��  ��� ��� � �A�5 DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn,&lcParam
 �	 B�� �� � � U LCFNCNAM OLCFNCLOC  LCPARAM E LCRETRN PLLCOPTPROG GCREPHOME GCACT_APPLLFRPTHERMO �  LFCLOSTHRM �	  LFWAITMSG E:  LFCLEARREP _  REMLFGETFO f  REMLFISAPP �  REMLFOPTPR   REMLFUSRVL �  � q q � �� � �q 1� � q � A � �Q� A � qa q� aa !�� �� 1a�111� �q 1� � q � A � �qa q�Qa� a� 1� � A A � ���q1A �����A A A A 2 � q ��Qa A �2 1�����A �� A 2 �2 2 � 2 q � �A � 2 q � � A 2 � q � �� A � Q� QA � A A 4             V   :  Z   a   �  f   l   %  r   n   i  u   o   �  w   q   �  z   w   u  �   ~   �  �            0      ,   m{E�   
  B�e�� U  q 1             PROGS\GFCRTFRM.PRG c:\users\tarek\appdata\local\temp\ GFCRTFRM.FXP PROGS\GFDISPRE.PRG GFDISPRE.FXP PROGS\GFFRMCNT.PRG GFFRMCNT.FXP PROGS\GFINSTAR.PRG GFINSTAR.FXP PROGS\SOREPORT.PRG SOREPORT.FXP PROGS\LFPREGRD.PRG LFPREGRD.FXP  *   ?     6    ?  A     V    A  *N     v    *N  �X     �    �X  i     �    i  Zi     �   