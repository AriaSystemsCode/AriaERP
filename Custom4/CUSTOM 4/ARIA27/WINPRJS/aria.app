��� 4  z � �                    [Y0	   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _S051416HG 628594896      /  F      ]                                �                       WINDOWS _S051416HI 628594956�  �  �  �      �                                                           WINDOWS _S0514YJ9Y 630147717    ?  L  ^  {                                                           WINDOWS _S0514YJA3 630147717�  �  �      3                                                           WINDOWS _S0514YJA8 630147717�  �  �  �  �        g  �                                               WINDOWS _S0514YJAI 630147717�  �  �  	  .	  K	      �	  �	                                               WINDOWS _S0514YJAP 630147717�
  �
  �
  �  �  �
      L  B                                               COMMENT RESERVED                                ]                                                            F                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      Name = "Dataenvironment"
      1      2      ariaformset      ..\..\..\classes\main.vcx      formset      ariaformset     AutoRelease = .T.
formhastoolbar = 
Name = "ariaformset"
Ariaform1.Height = 88
Ariaform1.Width = 218
Ariaform1.DoCreate = .T.
Ariaform1.AutoCenter = .T.
Ariaform1.BorderStyle = 2
Ariaform1.Caption = "Change System Date"
Ariaform1.MaxButton = .F.
Ariaform1.Name = "Ariaform1"
      	ariashape      ..\..\..\classes\main.vcx      shape      
Ariashape1      ariaformset.Ariaform1      BTop = 7
Left = 5
Height = 41
Width = 207
Name = "Ariashape1"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel1      ariaformset.Ariaform1      RCaption = "System Date:"
Left = 18
Top = 20
TabIndex = 1
Name = "Arialabel1"
      ariatextbox      ..\..\..\classes\main.vcx      textbox      Ariatextbox1      ariaformset.Ariaform1      ^StrictDateEntry = 0
Format = "D"
Left = 100
TabIndex = 2
Top = 17
Name = "Ariatextbox1"
      �PROCEDURE Valid
IF EMPTY(This.Value) AND !EMPTY(This.Text)
  = MessageBox ("Invalid Date.", 16, _SCREEN.Caption)
  RETURN 0
ENDIF
ENDPROC
PROCEDURE Init
This.Value = oAriaApplication.SystemDate
ENDPROC
     ���    �   �                         �   %   �       �      �           �  U     T�  � �� � � �� <� � U  OARIAAPPLICATION
 SYSTEMDATE THISFORM ARIATEXTBOX1 VALUE THISFORMSET Click,     ��1 aq 1                       _       )   �                         )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariaformset.Ariaform1      Ariacommandbutton2      ariacommandbutton      ..\..\..\classes\main.vcx      commandbutton      Ariacommandbutton1      ariaformset.Ariaform1      UTop = 54
Left = 128
Caption = "Cancel"
TabIndex = 4
Name = "Ariacommandbutton1"
      /PROCEDURE Click
Release ThisFormSet
ENDPROC
      ����    �   �                         �k   %   G       \      V           �  U  
  <�  � U  THISFORMSET Click,     ��1 q 1                       $       )   �                         ariacommandbutton      ..\..\..\classes\main.vcx      commandbutton      PTop = 54
Left = 39
Caption = "Ok"
TabIndex = 3
Name = "Ariacommandbutton2"
      jPROCEDURE Click
oAriaApplication.SystemDate = ThisForm.Ariatextbox1.Value
Release ThisFormSet
ENDPROC
     ����    g  g                        p�   %   �            �           �  U  P  %�C�  � ��
 C�  � �
	��I �! ��C� Invalid Date.��9� �x��	 B�� �� � U  THIS VALUE TEXT CAPTION  T�  � �� � �� U  THIS VALUE OARIAAPPLICATION
 SYSTEMDATE Valid,     �� Init�     ��1 �� A 2 11                       �         �   �       )   g                  07   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Class                                                                                               WINDOWS _RZ91005A4 626625349�(  �  �  �(      w
  �o  �  w�         w%  �                             COMMENT RESERVED                        +#                                                                   WINDOWS _S0B0LN560 628969697�  (    �      �  ޓ  ?�  ݡ         �  �                             COMMENT RESERVED                        �                                                                   WINDOWS _S0B0S8IM4 629041754Q  �  �  h      �  ��  %�  �         ;  H  z          -               COMMENT RESERVED                                                                                           WINDOWS _RXG0QEGOM 629631030�$      [%  �$      %  �#  d�  !�          �$  �$  �$          g  u           COMMENT RESERVED                        P                                                                   WINDOWS _RY20YT9HR 630216343�&  �    �&        ��  � �         �&  9  �&          �"               COMMENT RESERVED                        #                                                                   WINDOWS _RXG0YGISE 630216416h  �  �        �  z9  |  y           1  >              #               COMMENT RESERVED                                                                                           WINDOWS _RXG0QWN68 630216438�  �  �  �      �  Y  2r  �         �  �              \               COMMENT RESERVED                        B                                                                   WINDOWS _RXG0RDW4F 630216474�  I  ;  �      �    �]  �	         }  �              o               COMMENT RESERVED                        ^                                                                   WINDOWS _RXG0REYIG 630216498�  K  =          �  �J  �         �  �              �               COMMENT RESERVED                        �                                                                   WINDOWS _S1G059HNG 640095653'7  �7  �7  7      Y7  �E  �A 8�          >7  >#              K7               COMMENT RESERVED                        t7                                                                   WINDOWS _S1G083O26 640097784�7  �7  �7  �7      �7  �H  SK r�         �  �"              #               COMMENT RESERVED                        �                                                                   WINDOWS _S1G0ERGHW 640106059�  �  �  �      �  �;  � �/         �  �&              t               COMMENT RESERVED                        f                                                                   WINDOWS _S1G0GW68B 640106732G#  �6  �6  �      o  �%  � l          ^#  ?              X6               COMMENT RESERVED                        H6                                                                   WINDOWS _RXG0RCJ4Z 641225587�  �"  �"  ,      H  �D  �i �          �5  �6                             COMMENT RESERVED                        �!                                                                   WINDOWS _S1Y0LTXG6 641290781�  u!  �  �      �  K�  �C  G          �"  �              �"               COMMENT RESERVED                                                                                           WINDOWS _S1G06OIAG 644838486X  f6  y6  �!      �  �I  (� ��         
7  ~'              �6               COMMENT RESERVED                        �6                                                                   WINDOWS _RXG0R9C8Z 644838785,'  �(  �(  C'      ['  t�  �� �          '  #'              z#               COMMENT RESERVED                        �&                                                                   WINDOWS _RZ10RAEL1 646745147�  �5  Y  �      �4      e�  h?          |  �  �!                           WINDOWS _RZ10SYILB 642276715�5  �5  �5  6  6  c"              �] A!                                       WINDOWS _RZ10SYIMT 642280115n(  U  ](  G(  +(  9      �'  >                                               WINDOWS _S2C0P43DK 642276704D  4  �  "  �  �                                                           WINDOWS _RZ10SYIO4 642280115�  x  h  �  .  �      �  �<                                               WINDOWS _RZ91005A4 642280115�  �  (    �  �
      �
  8                                               WINDOWS _S2C0P43JS 642280115�  �    �  �  �      �  �                                               COMMENT RESERVED                        j                                                                 WINDOWS _S8S0KOOMU 658206486L    �  c      |  "�  �  e          ?  �              1               COMMENT RESERVED                                                                                           WINDOWS _08G0SQOA9 694053932�!  P"  B"  "      !"  	q   < j          �!  G              �               COMMENT RESERVED                        �!                                                                   WINDOWS _S1G0FUX2S 694313324f  �  �  }      �  �:  St c�          P  ]              B               COMMENT RESERVED                        �                                                                   WINDOWS _RXG0RAD5Q 787378934�  u  i
  �      :  4�  �" .�          |  �  %          n               COMMENT RESERVED                        [                                                                   WINDOWS _RZ10O2QRG 787897489�      �  �        J  �� �  �(  �  }  �  �	          o               COMMENT RESERVED                        8                                                                   WINDOWS _S1L0VO8GG 82466095316  �6  �6  �(      �6  bB  �� �~          �  �"              i%               COMMENT RESERVED                        k#                                                                   �F                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      JMS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
Arial, 0, 9, 5, 15, 12, 21, 3, 0
      
reportform      Class      7      ariaformset      
reportform      ?PROCEDURE Click
THIS.Value = 0
RELEASE ThisFormSet
ENDPROC
      Close      checkbox      
styleprice      Pixels      Class      1      baseglogalclass      
styleprice      price
      custom      1      #reportform.Ariaform1.Ariacontainer1      main.vcx      toolbarbutton      #reportform.Ariaform1.Ariacontainer1      PRINT      checkbox      
ariareport      laformulas^
assingformula^
      Pixels      Class      1      
olecontrol      
ariareport      ,OLEObject = C:\WINNT\System32\CRYSTL32.OCX
      
olecontrol      Name = "checkprd"
      decimaltofraction      Pixels      Class      baseglogalclass      decimaltofraction      Name = "decimaltofraction"
      custom      isedtble      updatetrace      Pixels      Class      1      baseglogalclass      updatetrace      Name = "updatetrace"
      custom      globals.vcx      	substring      Pixels      Class      1      baseglogalclass      	substring      Name = "substring"
      globals.vcx      addcurrencysymbol      Pixels      BHeight = 31
Width = 33
optionprogram = ""
Name = "ariareport"
      getsequence      Pixels      Class      1      baseglogalclass      getsequence      Class      Class      main.vcx      toolbarbutton      1      Pixels      globals.vcx      *edigetseq 
      Name = "getsequence"
      baseglogalclass      Name = "isedtble"
      baseglogalclass      notepad      custom      Name = "styleprice"
      baseglogalclass      �Top = 5
Left = 56
Height = 22
Width = 22
Picture = ..\bmps\close.bmp
TabIndex = 3
TabStop = .F.
ToolTipText = "Close"
ZOrderSet = 2
Name = "Close"
      �optionprogram
*changetosubreport 
^laformulas[1,2] 
*changeformula 
*reportprint 
*reportpreview 
*assingformula 
*changedatapath 
*beforeprinting 
*openfile open tables
*chkform 
*optprog 
      custom      Name = "adduserinfo"
      NPROCEDURE Click
THIS.Value = 0
ThisForm.Ariareport1.ReportPrint()
ENDPROC
      �Top = 5
Left = 25
Height = 22
Width = 22
Picture = ..\bmps\print.bmp
TabIndex = 2
TabStop = .F.
ToolTipText = "Print"
ZOrderSet = 2
Name = "PRINT"
      ����    �   �                         ��   %   d       {      s           �  U    T�  � �� �� <� � U  THIS VALUE THISFORMSET Click,     ��1 q 1                       4       )   �                         globals.vcx      ferrinfo      PREVIEW      1      Class      1      baseglogalclass      addcurrencysymbol      Name = "addcurrencysymbol"
      custom      globals.vcx      custom      globals.vcx      Pixels     a���    H  H                        �   %   �      �  1   �          �  U  � 4�  � � � � 5� � � � � �	 � T� �CW�� T�
 �C� GetTempName�N��, T� �CC� lcFileb� C� �  � C�
 � 6�� <�
 � T� �� A��M T� �CC� lcFileStruc[1]b� U� � A� CCC� ��=� (� � S� � F66�� H�� �L� �� � F��(�
 F�� �� ��C�� ��� T� �� A�� �� � A��L� ��C�� �� ��� � T� �� A��/ T� �CC� lcTagExp[1]b� U� � A� � S6�� %�� � A���� h1�� � � �� � ��C CREATE TABLE (oAriaApplication.WorkDir+lcFileName) &lcFileStruc
 �
 F�� �� %�� � A��|� ��	 ���(�C�� ����x�: INDEX ON &lcTagExp[lnCount,1] Tag &lcTagExp[lnCount,2]
 �� ��/ %�C� lcTagExpb� C� C� lcTagb� C	���� T� �C�/�� �/ %�C� lcTagExpb� C� C� lcTagb� C	���! INDEX ON &lcTagExp Tag &lcTag
 � � Q� Q�� � � �� %�� � A��h�# SET ORDER TO TAG &lcTagExp[1,2]
 ���/ %�C� lcTagExpb� C� C� lcTagb� C	���� G((�� �� � �
 F�� ��	 B�� �� U  LCFILE LCFILESTRUC LCTAGEXP LCTAG
 LCFILETYPE LAFILESTRUC
 LCFILENAME
 LNWORKAREA	 LCTAGTYPE LNCOUNT OGETTEMPNAME DO OARIAAPPLICATION WORKDIR do,     ��1 2�� ��q � �� !� � � !!A � �"q� 1A � !��A � �� A �A A A !1� �� A A � � 2                       �      )   H                        codepop      globals.vcx      Name = "notepad"
      Pixels      notepad      getitemmask      1      Name = "getitemmask"
      getadr      Pixels      Class      baseglogalclass      getadr      Name = "getadr"
      custom      globals.vcx      custom      globals.vcx      custom      Name = "getmemvar"
      1      Pixels      Class      1      baseglogalclass      codepop      Name = "codepop"
      custom      globals.vcx      Pixels      shape     (Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     (Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
      getvalidentries     (Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
      Class      1      baseglogalclass      getvalidentries      Name = "getvalidentries"
      custom      globals.vcx      gettempname      Pixels      Class      1      1      baseglogalclass      Pixels      YThis is the class used as a parent class for all the global functions written as classes.      PPROCEDURE Click
THIS.Value = 0
ThisForm.Ariareport1.ReportPreview()
ENDPROC
      #reportform.Ariaform1.Ariacontainer1      formset      baseglogalclass      gettempname      Name = "gettempname"
      custom      globals.vcx      �Top = 5
Left = 3
Picture = ..\bmps\prtprev.bmp
TabIndex = 1
TabStop = .F.
ToolTipText = "Print Preview"
ZOrderSet = 1
Name = "PREVIEW"
      checkbox      main.vcx      toolbarbutton      RTop = 0
Left = -2
Height = 31
Width = 417
ZOrderSet = 0
Name = "Ariashape1"
      #reportform.Ariaform1.Ariacontainer1      
Ariashape1      main.vcx      	ariashape      main.vcx     ���    �  �                           %   l      �     x          �  U  � 4�  � � � �) T� �CC� lnMesUnb� N� �� � 6��) T� �CC� lnFFrUnb� N� �� � 6��) T� �CC� lnLFrUnb� N� �� � 6�� J�� �(� � � � T� ��  C�  8��i T� �CC� C� ��8� � ���? CC� C� �d8� � �d�! CC� C� �
8� � �
� � 666�� T� �� � �� %�� � � � � ��b� B�CC�  8�ZC�X�� �! T� �C�  � � � � C� 6��  T� �C�  � � ���� �6��  T�	 �C�  � � ���� �6�� �� �� �(�� ����	 ��-�( %�C� � � 8� � � � ��)� T� �CC� � � 8�� !� � ��m T�
 �CCC�  8� � CC�  8� Z� �  6� ��  CCCCC�  8� � C� � � 6� Z�� �� /CCC� � Z�� ���0 T�
 �C� *�
 � CC�  8� Z�      � �
 6��	 B��
 �� U  LNMEAS LNMESUN LNFFRUN LNLFRUN LNFRAC	 LNDECIMAL
 LNNUMDECPL LNEND LNSTART LNSTEP LCTEMP do,     ��1 1���R2��aA ���A A A �� 2                       �      )   �                        ����    �   �                         �L   %   J       ^      V           �  U    B�� XCC��]�\�� U   do,     ��1  q2                       �      )   �                         ,OLEObject = C:\WINNT\System32\crystl32.ocx
      globals.vcx      getitemmask      reportfilter
reportfilename
      checkprd      createtempfile      Class      baseglogalclass      createtempfile      Name = "createtempfile"
      custom      globals.vcx      ETop = 37
Left = 378
Height = 31
Width = 33
Name = "Ariareport1"
      Pixels      1      1      Pixels      custom      Class      globals.vcx      Pixels      	getmemvar      adduserinfo      1      baseglogalclass      Class      chkrate      Pixels     !ClassLibrary^
Height^
Width^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
Picture^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
      Class      1      custom      baseglogalclass      ndatasession
oform
*do 
      DHeight = 17
Width = 20
ndatasession = 
Name = "baseglogalclass"
      custom      Pixels      Class     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
      *trnsstr 
      	getmemvar      baseglogalclass      Class      1      getrelatedfields      Class      1      baseglogalclass      getrelatedfields      Name = "getrelatedfields"
      1      �PROCEDURE Init
This.Resize()
ENDPROC
PROCEDURE Resize
This.Left  = 0
This.Width = ThisForm.Width + 1
This.Ariashape1.Width = This.Width + 5
ENDPROC
      reportform.Ariaform1      Ariacontainer1      	container      ariacontainer      chkrate      custom      globals.vcx      adduserinfo      baseglogalclass      ��ࡱ�                >  ��	                               ����        ������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������   ����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������R o o t   E n t r y                                               ��������                               ��E[,P�   @       O l e O b j e c t D a t a                                            ����                                        �        A c c e s s O b j S i t e D a t a                             &  ������������                                       8        C h a n g e d P r o p s                                         ������������                                       �         ����         ����      	   
         ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������V     �      F P     ��ݺ��ݺ�  �                ,d d �8                              8                                 �   WindowControlBox    L      WindowMaxButton    L                                �� ����<   p                                                                                                                                           WindowMinButton    L      WindowState 	   I
         WindowShowGroupTree    L      WindowShowNavigationCtls    L      WindowShowCancelBtn    L      WindowShowPrintBtn    L      WindowShowExportBtn    L      WindowShowZoomCtl    L      WindowShowProgressCtls    L      WindowShowSearchBtn    L      WindowShowPrintSetupBtn    L      WindowShowRefreshBtn    L                                                                                                                                                                                                                                                            �reportfilename = ('')
formhastoolbar = 
Name = "reportform"
Ariaform1.Height = 221
Ariaform1.Width = 413
Ariaform1.DoCreate = .T.
Ariaform1.WindowState = 0
Ariaform1.Name = "Ariaform1"
      Class      main.vcx      
ariareport      globals.vcx      
olecontrol      reportform.Ariaform1      Ariareport1      baseglogalclass      isedtble      Pixels      globals.vcx      custom      1      Name = "chkrate"
      custom      globals.vcx      globals.vcx      custom      checkprd      Pixels      Class      getexsin      baseglogalclass      Class      Pixels      Name = "getexsin"
      getexsin      custom      globals.vcx      globals.vcx      custom      Name = "ferrinfo"
      ferrinfo      baseglogalclass      ����    �   �                         �   %   �       �      �           �  U  #  T�  � �� �� ��C� � � �� U  THIS VALUE THISFORM ARIAREPORT1 REPORTPRINT Click,     ��1 1                       C       )   �                         eTop = 1
Left = 0
Width = 414
Height = 33
BorderWidth = 0
TabIndex = 1
Name = "Ariacontainer1"
     (Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
      ����    �   �                         #�   %   �       �      �           �  U  #  T�  � �� �� ��C� � � �� U  THIS VALUE THISFORM ARIAREPORT1 REPORTPREVIEW Click,     ��1 1                       E       )   �                        ]���    D  D                        k�   %   �       �      �           �  U    ��C�  � �� U  THIS RESIZED  T�  � �� �� T�  � �� � ��� T�  � � ��  � ��� U  THIS LEFT WIDTH THISFORM
 ARIASHAPE1 Init,     �� ResizeN     ��1 � 2 q�1                                :   �       )   D                       ����    �  �                        W�   %   /      �     X          �  U  � 	 ��C��� %�C�  � �
��� � T� �� � �� %�C� �  � 0
��_ � T� �� � � � � \�� � T� � � � �� �  � �� ��C� � � �	 �� � ��C� �
 �� U  THIS REPORTFILENAME LCREPORTHOME OARIAAPPLICATION
 REPORTHOME ACTIVEMODULEID THISFORMSET	 ARIAFORM1 ARIAREPORT1 CHANGEDATAPATH REFRESHt  %�C�  � � ���= � T�  � � � �-�� T�  � � � �-�� �m � T�  � � � �a�� T�  � � � �a�� � U  THISFORM ARIAREPORT1 REPORTFILENAME ARIACONTAINER1 PREVIEW ENABLED PRINT Init,     �� Ariaform1.Refreshg    ��1 � 1q�A �1A � 2 QAA� AAA 1                       �        �  �      )   �                       !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     PROCEDURE do
LPARAMETERS lcNoteType, lcKey

IF TYPE("lcNoteType") = "C" AND !EMPTY(lcNoteType) AND ;
   TYPE("lcKey")      = "C" AND !EMPTY(lcKey)
  lcToDo = oAriaApplication.ScreenHome + "SY\NOTEPAD.SCX"
  DO FORM (lcToDo) WITH lcNoteType, lcKey
ENDIF
ENDPROC
     :Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
Destroy^
Error^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     q���    X  X                        �^   %   �                      �  U  �  ��  � �E %�C�
 lcNoteTypeb� C� C�  �
	� C� lcKeyb� C	� C� �
	��� �" T� �� � � SY\NOTEPAD.SCX�� �� ���  � � � U 
 LCNOTETYPE LCKEY LCTODO OARIAAPPLICATION
 SCREENHOME do,     ��1 � S!1A 1                             )   X                       !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     PROCEDURE do
*!*************************************************************
*! Name      : gfTraceKey
*! Developer : Hesham El-Sheltawi (Hesham)
*! Date      : 03/30/98
*! Purpose   : Adding Record in the transaction tracing file
*!             For modification happed in specific data file
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : lcFileName   file that was modified
*!                      lcKeyExpr    key of the record modified
*!                      lcEventOccr  Event occurs on record "E,A,D"
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = gfTraceKey('INVHDR','INV#1','E')
*!*************************************************************
*!E300842,4 
*!*************************************************************
*FUNCTION gfTraceKey
PARAMETERS lcFileName,lcKeyExpr,lcEventOccr
** If the communication module is not installed for the active company
** return
*TAK E300973,1
*IF ! ('CM' $ gcComp_Mdl)
IF ! ('CM' $ oAriaApplication.CompanyInstalledModules)
  RETURN
ENDIF
** end checking module existance
** define private variables used by the function
PRIVATE lnAlias,llCMTRACE,llViewUsed,llViewSite

lnAlias = SELECT()            && save the active work area
llCMTRACE = USED('CMTRACE')   && check opening of CMTRACE FILE
llViewUsed = USED('CMVIEWD')  && check opening of CMVIEWD FILE
llViewSite = USED('CMSITVEW') && check opening of CMSITEVEW FILE 
** IF trans. tracing file was not opend then open it
IF !llCMTRACE
  USE (oAriaApplication.DataDir+'CMTRACE') IN 0 ORDER TAG CTRANS
ENDIF
** end IF trans.
** IF site views file was not opend then open it
IF !llViewSite
  USE (oAriaApplication.DataDir+'CMSITVEW') IN 0 ORDER TAG CVIEWID
ENDIF
** End IF site views
** IF view details file was not opend then open it
IF !llViewUsed
  USE (oAriaApplication.DataDir+'CMVIEWD') IN 0 ORDER TAG FILENAME
ENDIF
** End IF view details 
lcFileName = PADR(UPPER(lcFileName),8)  && make the file name 8 char width
lcKeyExpr = lcKeyExpr+CHR(250)
lcKeyExpr = PADR(lcKeyExpr,LEN(CMTRACE.CKEYEXPR))  && make the key expr 80 char width
SET ORDER TO TAG CVIEWID IN CMSITVEW    
SELECT CMVIEWD
SET ORDER TO TAG FILENAME
SET RELATION TO CVIEWID INTO CMSITVEW ADDI
SELECT CMTRACE
SET ORDER TO TAG CTRANS
** if the file we want to add record for exist in any user defined view
** and this view is selected for any site to send data for
IF SEEK(lcFileName,'CMVIEWD') AND !EOF(CMSITVEW)
  ** check the event occured on the record
  DO CASE
    ** if the event was add new record then add the record directly in
    ** the trans. tracing file
    CASE lcEventOccr = 'A'
     INSERT INTO CMTRACE (CFILE_NAM,CKEYEXPR,CSTATUS);
                 VALUES  (lcFileName,lcKeyExpr,lcEventOccr)
    ** otherwise if the event was delete or modify then             
    OTHERWISE
      ** check if there was a record added for the same file+key in the
      ** trans. tracing file
      IF SEEK(lcFileName+lcKeyExpr)
        ** if there was record for the file+key then
        DO CASE
          ** if the event was deleting a record and the record in the
          ** trans. tracing file was just added the delete the record
          ** from the trans. tracing file
          CASE lcEventOccr = 'D' AND CSTATUS = 'A'
            BLANK
            DELETE
          ** if the event was deleting a record and the record in the
          ** trans. tracing file was modied then change the status
          ** of the record in the trans. tracing file to deleted
          CASE lcEventOccr = 'D' AND CSTATUS = 'M'        
            REPLACE CSTATUS WITH 'D'
        ENDCASE
      ** else if there was no record for file+key in the trans. tracing
      ** file then add new record with status like the event passed for
      ** the function
      ELSE
        INSERT INTO CMTRACE (CFILE_NAM,CKEYEXPR,CSTATUS);
                    VALUES  (lcFileName,lcKeyExpr,lcEventOccr)
      ENDIF
  ENDCASE  
ENDIF
** clear relation on the site views file
SELECT CMVIEWD
SET RELATION OFF INTO CMSITVEW

** if the trace file was opend by the function the close it
IF !llCMTRACE
  USE IN CMTRACE
ENDIF
** end closing trace file

** if the sites view file was opend by the function the close it
IF !llViewSite
  USE IN CMSITVEW
ENDIF
** end closing sites view file

** if the views details file was opend by the function the close it
IF !llViewUsed
  USE IN CMVIEWD
ENDIF
** end closing views details file

** restore the current work area
SELECT (lnAlias)

ENDPROC
     /PROCEDURE do
*!*************************************************************
*! Name      : gfSubStr
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To extract element from string or to convert string to array
*!*************************************************************
*! Passed Parameters  : String to be used
*!                      poiter to array or element position
*!                      sparators used in the string
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* This function will return eather a string part # OR an array of all
* the string parts according to the type of the second parameter. The
* firest parameter will be the string or string variable. If the
* second parameter have a numeric type, the function will return the
* but if it is an array the function will return the array with each
*  element having a part from the string.
* 
*FUNCTION gfSubStr
PARAMETERS lcString,lnAryOrPos,lcSepta

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR

ENDPROC
     ����    �  �                        �&   %         C               �  U  � 4�  � � � 5� � � T� �� � �� T� �� �	 �� T�
 �� � ��, T� �CC�
 lcActvCompb� C� � � � 6�� T� �� �� %�� � 
��� �+ Q�  ��
 � SYCCOMP��� �� cComp_ID� %�C� � CompFile���� � T� �CC� � �� �� � Q� � �- Q�  ��
 � SYDFIELD��� ��	 CFLD_NAME�+ Q�  �� � CODESET��� �� Fildname�; T� �CC�  �	 FieldFile�� � EDITABLECC� � �f� -6��. T� �CC�  � CodeSetF��	 � � � � � 6�� Q� � Q� �	 B�� �� U  LCPFIELD LNFIELDW
 LCACTVCOMP LLRETVAL	 LCDATADIR
 GCACT_COMP OARIAAPPLICATION ACTIVECOMPANYID	 GCDATADIR DATADIR	 GCSYSHOME SYSPATH CCOMP_ID COMPFILE GFGETDATADIR	 CCOM_DDIR	 CFLD_NAME	 FIELDFILE FILDNAME CODESETF	 MCODEINFO	 NFLD_WDTH do,     ��1 0� � �� 1��cB � A ����� � � 1                       �      )   �                       :Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
Destroy^
Error^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     	�PROCEDURE do
*!*************************************************************
*! Name      : gfGetVld
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995 
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*FUNCTION gfGetVld
PARAMETERS lcFieldNam, laArrName,llAddAll
PRIVATE lcCurrFile,lcOldTag,lcSetExct,llSydField,lnMaxLen

*** Save current environment
lcCurrFile    = SELECT()
lcSetExact    = SET('EXACT')
llSydField    = .F.
lnMaxLen      = 0

*** Check if fields system file is opened, if it is 
*** set the appropriate tag, if not open use it with
*** the appropriate tag.
*** Tag to be used is 'cFld_Name'
*** Its expression is
IF !USED('SYDFIELD')
  SELECT 0
*TAK E300973,1 Changed to work under visual.
* USE &gcSysHome.SYDFIELD ORDER TAG cFld_Name 
  USE (oAriaApplication.SysPath+"SYDFIELD") ORDER TAG cFld_Name

  llSydField       = .T.
ELSE
  SELECT SYDFIELD
  lcOldTag         = SYS(22)
  SET ORDER TO TAG cFld_Name
ENDIF

*** Search for field name
IF SEEK(UPPER(lcFieldNam), 'SYDFIELD')
  *TAK E300973,1 Changed to work under visual.
  *=gfSubStr(SYDFIELD.mVEntries, @laArrName, "|~")   
  PRIVATE oSubString
  oSubString = CREATEObject("SubString", This.oForm)
  oSubString.Do(SYDFIELD.mVEntries, @laArrName, "|~")
  RELEASE oSubString
  *TAK E300973,1 End.

  lnMaxLen = LEN(laArrName[1,1])
  FOR lnCount = 2 TO ALEN(laArrName,1) 
    lnMaxLen = MAX(lnMaxLen,LEN(laArrName[lnCount,1]))
  ENDFOR  
ELSE
  laArrName = " "
ENDIF  

IF llAddAll
  *** Add one element on top to be used for 'All' option.     
  *** Assign a value to this element later.
  IF !EMPTY(laArrName[1,1])
    DIMENSION laArrName[ALEN(laArrName,1)+1,2]
    =AINS(laArrName, 1, 1)
  ENDIF  
  laArrName[1,1] = "All"
  laArrName[1,2] = " "
ENDIF  

*** Restore environment
IF llSydField .AND. USED('SYDFIELD')
  USE IN 'SYDFIELD'   
ELSE
  IF !EMPTY(lcOldTag)
    SET ORDER TO TAG (lcOldTag)
  ENDIF  
ENDIF

SELECT (lcCurrFile)
SET EXACT &lcSetExact
RETURN lnMaxLen


ENDPROC
     �PROCEDURE do
*!*************************************************************
*! Name      : gfTempName
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Creat temp file name
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
RETURN ("X"+SUBSTR(SYS(2015),4))

ENDPROC
     I���    0  0                        �   %   I      �  I   U          �  U  � 4�  � � � � � � � �� �# 5� �	 �
 � � � � � � T� �� ��- T� �CC� lcCompIdb� C�	 � � � � 6��4 T� �CCC�
 lcBaseCurrb� C�	 � � � � 6���� T�	 �CW�� T� �-�� %�� � ��!� %�C� lcExUnitb� C��� &lcExUnit = 1
 B��      �?�� � � %�C� SYCCURR�
��g� T� �a�� F�  � Q�� � � SYCCURR�� �v� F� � � T� �-�� %�C� SYCEXCH�
���� T� �a�� F�  � Q�� � � SYCEXCH�� ��� F� � � T� �C��� T�
 �C��� G((� CURRENCY<� T� �C� NEARv�� GF � G(� %�C� C� ��C� ����N� T� �� �� ��� J�-�(� � � J�� �(� � T� �C�	 GetMemVar�N��7 T� �C� LLMULCURR,LLEXCHRATE,LNEXRATDAY � � � ��- %�� � � C� ��� �  � � 	���� T� �� �� ��� %�� � � 	��8�  � SYCHRATE(� �� � � � ��� %�� � � 
	��|�' ��C� TRM00249B00000� DIALOG� �" �� � � � � %�C� lcExUnitb� C����R &lcExUnit = LOOKUP(SYCCURR.NCURRUNIT,lcCurrency,SYCCURR.CCURRCODE,"CCURRCODE")
 � %�C�
 �
��� G((��
 �� � SET FILTER TO &lcOldFlt
 %�� ��N� Q� � � %�� ��g� Q� � � SET NEAR &lcSetNear

 F��	 ��	 B�� �� U#  LCEXUNIT
 LCCURRENCY LDDATE	 LLDISPMSG LCCOMPID
 LCBASECURR
 LLNOERRMSG
 OGETMEMVAR
 LLEXUSEDBY
 LCOLDALIAS LCOLDTAG	 LNRETRATE	 LDCURRDAY LCOLDFLT LLCURUSEDBY LNEXRATE OARIAAPPLICATION ACTIVECOMPANYID BASECURRENCY SYSPATH SYCCURR SYCEXCH CURRENCY	 LCSETNEAR NEXRATE	 LLMULCURR
 LLEXCHRATE
 LNEXRATDAY LNNOVAR DO	 CBASECURR	 CCURRCODE	 DRATEDATE SYCHRATE
 MESSAGEBOX do,     ��1 �s 2� �A� � �A A q� � �� q A � q� � �� q A � � #2a b �� � � � �q�� � A� TxA A A A �!A � A �� � A � � A s� � 2                       �      )   0                       ����    �  �                        y�   %   P      �     \          �  U  � 4�  � � � �� � � %�C� lcUntSinb� C��D � T�  �� /�� � %�C�
 lcBaseCurrb� UL��w � T� �� � �� � %�� � ��� �
 B�� *�� � %�C� SYCCURR�
��� � T� �a�� Q�  �� � � SYCCURR�� �� � T� �-�� � G(�	 (�	 CCURRCODE� ��C� � SYCCURR���X T� �C�	 �
 � M� C� ��	 � 
	� �	 �
 � D� C� ��	 � 	� � *� � /6�� %�� ���� Q�	 � �	 B�� �� U  LCUNTSIN
 LCCURRENCY
 LCBASECURR LCRETURN LLCLOSE OARIAAPPLICATION BASECURRENCY SYSPATH	 CCURRCODE SYCCURR CCURMETH
 MCUREXCEPT do,     ��1 P� � �� A �A � A t� �� � B aQ�� � A � 5                       v	      )   �                       (Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     �PROCEDURE Init
LPARAMETERS oForm

IF TYPE("oForm") = "O" AND "FORM" $ UPPER(oForm.Class)
  This.oForm = oForm
  This.nDataSession = SET("DATASESSION")
  SET DATASESSION TO This.oForm.DataSessionId
ENDIF

ENDPROC
PROCEDURE Destroy

IF TYPE("This.nDataSession") = "N" AND !ISNULL(This.nDataSession) 
  SET DATASESSION TO This.nDataSession
ENDIF
This.oForm = .NULL.
ENDPROC
     )Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
Error^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     ����    �  �                        <�   %   N      �     m          �  U  y  ��  �- %�C� oFormb� O� � FORMC�  � f	��r � T� �  ��  �� T� � �C� DATASESSIONv�� G�(�� �  � �� � U  OFORM CLASS THIS NDATASESSION DATASESSIONIDU 2 %�C� This.nDataSessionb� N�
 C�  � �
	��@ � G�(��  � �� � T�  � ���� U  THIS NDATASESSION OFORM Init,     �� Destroy�     ��1 q ��A 3 "� A � 1                       �         �   y      )   �                       !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     !Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     (Height^
Width^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     2Height^
Width^
trnsstr^
Name^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     )Height^
Width^
ClassLibrary^
Picture^
AddObject^
Class^
Comment^
ControlCount^
Controls^
Error^
HelpContextID^
Objects^
Parent^
ParentClass^
ReadExpression^
ReadMethod^
RemoveObject^
ResetToDefault^
SaveAsClass^
ShowWhatsThis^
WhatsThisHelpID^
WriteExpression^
WriteMethod^
     ����    �  �                        �   %   �      j  C   �          �  U  � 4�  � � � 5� � � %�C�  ���T� �� ���(�C�� ����L� H�S ��# �CC � �� ��\� Cc��� � T� ��  ��# �CC � �� ��\� Nn��� � T� �� ��# �CC � �� ��\� Ll��� � T� �-��# �CC � �� ��\� Dd��� T� ��        �� � T� �C � �� �� &lcFieldNam = lcObjValue
 �� B� � T� �C�� T� �-�� %�C� CODES�
���� Q�  ��	 �
 � Codes�� T� �a�� � F� � T� �C�]��
 G((� 0� � ���� T� ��  ��J o� CODES�� ��� ��� ���� � � � N� Y� � � �  	��� � �� ���(�C�� ����N� T� �C�� C � �� ��� %�� � ��}� �J� T� �C�� � ���� H���� �C � �� � C���� T� �C � �� �� �C � �� � N���� T� �C� .C � �� �� %�� � ���� T� �C� DECIMALSv�� G(�� �� T� �CC � �� g��) SET DECIMALS TO &lcSavDecim          
 ��� T� �CC � �� g�� � �C � �� � L����' T� �CCC � �� �� YT� a� -6�� �C � �� � D��� T� �CC � �� #�� � T� �C � �� �� &lcFieldNam = lcObjValue
 � �� SET ORDER TO &lcSavOrder
 %�� ���� Q� � � F�CC� �� � � � 6�� U 	 LCCODEVAL
 LAARRAYNAM	 LCFLDNAME LATEMPCODES
 LCOBJVALUE LNCOUNT
 LCFIELDNAM
 LCSAVSELCT
 LLUSECODES OARIAAPPLICATION DATADIR CODES
 LCSAVORDER	 CRLTD_NAM	 CRLTD_TYP	 CRLTD_VLU CDEFCODE	 CRLTFIELD	 CFLD_NAME CCODE_NO
 LNPOSITION
 LNDECIMPOS
 LCSAVDECIM do,     ��1  � � � �� 1� 2� 2� 21B Q�B A A � � Q�� A q � � � �  ���� q� �Q��q� q�� qA �q�qA R�A A �� � A �3                       [      )   �                       ���    �  �                        �   %   �      �  W             �  U  � 4�  �3 5� � � � � � � � �	 �
 � � � J��  �(� �# T� �C� lcMaskOrHead[1]b� U�� T� �� I�� %�� 
��� �  %�C� lcMaskOrHeadb� C��� � B�-�� � T�  �C�  f��' T� �CC�  >�� C�  �R� � I6�� T�  �C�  �=�� � T� �� �� T� �CW�� T� �-�� %�C� ICISTRU�
��`� Q�  �� � � ICISTRU�� T� �a�� ��� F� � T� �C��� T� �CO�� � F� � G((� SEGNO� ��C� U1��� T�	 �� �� T� �� �� T� ���� ~$+�� � � U��a� %�� � N���� T� �� ��� � �� ����� T� �� ������ �� T� �� �����C� ��� T� �� �����C� X� Q�� T� �� ������ �� T� �� �����C� ��� T� �� ������ �� T� �� ������ �� T� �� C� X� QC� ��� � T� �� � CC� �>�� %�� � N� � 	��<� T� �� I�� � %�� � M� � 	��]� !� � � %�� ���� �  �C�� ����C�� ���� T� �C�� ��  ��� �#� H���� ��  � S���� T� �� �� ��  � P�	 � � M	��2� %�C� SN� � � ��.� T� �C� �C� >�\�� � ��  � H�	 � � M	���� %�C� SN� � � ����/ T� �C�	 �C � �� CC � �� >�\�� ��� T� ��	 �� �) ��  � H�	 � � N	� � � 	���� T� �C�	 C��� \�� ��  � H�	 � � I	��� T� ��	 �� � � %�� ��<� Q� � �� F� � G((�� �� %�� � � � CN	��{�	 #�� �� � �
 F�� ��	 B�� �� U  LCMASKORHEAD LCRETURN LLSTRUCTUSE LNRECNO LCSTRUCTORD
 LNCURALIAS LLARRAY LASEG	 LCITEMDIM LCHEADER LNSTARTNONM LNNOSEG LNPOSISTION	 LCLOOPEXT OARIAAPPLICATION DATADIR ICISTRU SEGNO	 CISEGHEAD
 CITEMRECTY CISEGNO	 CISEGTYPE	 CISEGSDES	 NISEGSIZE	 CISEGLDES	 CISEGSEPR
 LSEGENDMAJ THIS DO do,     ��1 t 2� 1� � q A � q!A � � � q�� � q � � A q � � � � � �!1��������A ��� A �A A A � �a� � !� ���A ���� � A ���� A A � � � q � �� A A � � 3                       M
      )   �                       PROCEDURE do
LPARAMETERS lcFileName, oForm
PRIVATE lcFileName, lcSavAlias, lnOldDataSession

lnOldDataSession = SET ("DATASESSION")
IF TYPE("oForm") = "O"
  SET DATASESSION TO oForm.DataSessionID
ENDIF

lcSavAlias = SELECT(0)
IF TYPE ("lcFileName") = "C" AND !EMPTY(lcFileName)
  SELECT (lcFileName)
ENDIF

REPLACE cAdd_User WITH oAriaApplication.User_ID ,;
        dAdd_Date WITH DATE()                   ,;
        cAdd_Time WITH gfGetTime()

SELECT (lcSavAlias)

SET DATASESSION TO lnOldDataSession
ENDPROC
     �PROCEDURE do
PARAMETERS lnMeas,lnMesUn,lnFFrUn,lnLFrUn

lnMesUn = IIF(TYPE('lnMesUn')<>'N',3,lnMesUn)
lnFFrUn= IIF(TYPE('lnFFrUn')<>'N',2,lnFFrUn)
lnLFrUn= IIF(TYPE('lnLFrUn')<>'N',2,lnLFrUn)

STORE 0 TO lnFrac,lnDecimal,lnNumDecPl

lnDecimal=lnMeas-INT(lnMeas)
lnNumDecPl=IIF(ABS(lnDecimal-INT(lnDecimal/1000))>0,1000,;
           IIF(ABS(lnDecimal-INT(lnDecimal/100))>0,100,;
           IIF(ABS(lnDecimal-INT(lnDecimal/10))>0,10,0)))           
lnDecimal=lnDecimal*lnNumDecPl           
IF lnDecimal=0 OR lnNumDecPl=0
  RETURN STR(INT(lnMeas),3)+SPACE(6)
ENDIF
lnEnd=IIF(lnMeas<0,lnDecimal,ABS(lnDecimal))
lnStart=IIF(lnMeas<0,-1,1)
lnStep=IIF(lnMeas<0,-1,1)
FOR lnFrac= lnStart TO lnEnd   STEP lnStep
  IF INT(lnNumDecPl*lnFrac/lnDecimal)-(lnNumDecPl*lnFrac/lnDecimal) = 0
    lnDecimal =ABS(INT(lnNumDecPl*lnFrac/lnDecimal))
    EXIT
  ENDIF
ENDFOR
*B800528,1 (Start)
*B800528,1 Alltriming the first part of the fraction
*lcTemp    = PADL(IIF(INT(lnMeas)<>0,STR(INT(lnMeas),lnMesUn),''),lnMesUn)+' '+ PADL(STR(IIF(INT(lnMeas)<0,ABS(lnFrac),lnFrac),lnFFrUn),lnFFrUn)+'/'+PADR(ALLTR(STR(lnDecimal,lnLFrUn)),lnLFrUn)
lcTemp    = PADL(IIF(INT(lnMeas)<>0,STR(INT(lnMeas),lnMesUn),''),lnMesUn)+' '+ PADL(ALLT(STR(IIF(INT(lnMeas)<0,ABS(lnFrac),lnFrac),lnFFrUn)),lnFFrUn)+'/'+PADR(ALLTR(STR(lnDecimal,lnLFrUn)),lnLFrUn)
*B800528,1 (End)
*lcTemp    = PADL(IIF(INT(lnMeas)<>0,STR(INT(lnMeas),3),''),3)+' '+ PADL(SUBSTR(ALLT(STR(IIF(INT(lnMeas)<0,ABS(lnFrac),lnFrac))),1,2),2)+'/'+PADR(SUBSTR(ALLT(STR(lnDecimal)),1,2),2)
*lcTemp    = PADL(IIF(INT(lnMeas)<>0,STR(INT(lnMeas),3),''),3)+' '+ PADL(STR(lnFrac,2),2)+'/'+PADR(ALLTR(STR(lnDecimal,2)),2)
lcTemp = IIF("*" $ lcTemp,STR(INT(lnMeas),lnMesUn)+'     ',lcTemp)
RETURN lcTemp

ENDPROC
     ���    �  �                        j*	   %   V      6  �   
          �  U   4�  � � T�  �C�  f�� T� �C� � �  ��� T� �� �� %�� � ��g � T� �C� � � ���� � %�� � ��� � %�CC��� � �
��� �* � � �C� � �����C� � ���� � T� �C� � ���� � T� � �� ������  �� T� � �� ������ �� U  LCFORMULANAME LCVALUE LNFORMULAPOS THIS
 LAFORMULAS
 LNARRAYPOSO  %�C�  � ��H � ��C�  � �� ��C�  � �� T�  � ���� B�C�  � �� � U  THIS BEFOREPRINTING PRINTERSELECT ASSINGFORMULA DESTINATION PRINTREPORTB  %�C�  � ��; � ��C�  � �� T�  � �� �� B�C�  � �� � U  THIS BEFOREPRINTING ASSINGFORMULA DESTINATION PRINTREPORT�  ��  � T� � ��  ��, %�C� THISFORMSET.ReportFilterb� C��V � T� � �� � �� � %�CC��� � �
��� � ��  ���(�C� � ����� �? T� � ��  ���C �  �� � � ='C �  �� � � '�� �� � U  LNCOUNT THIS SELECTIONFORMULA THISFORMSET REPORTFILTER
 LAFORMULAS FORMULAS�  T�  �� �� T� �C� � �� +�CC �  � � �
��� � T� �C �  � � �� T� �C� C� \� ��\��; T� � ��  ��CCC� f�=� SY�	 � � � � � 6� �� T�  ��  ��� � U	  LNCOUNT LNEND THIS RETRIEVEDATAFILES	 DATAFILES
 LCFILENAME OARIAAPPLICATION SYSPATH DATADIRc  ��  � � � 5� � %�C� �
��A � Q�  ���  � ���� �� �\ � G(�� �(�� �� � U  LCPATH LCFILE LCTAG	 LNINSTPOSg 4�  � � � � � 5� � � � � T�	 �C� W��) ��C� � � FORMCDDT� FORMCDDT�
 � ��) ��C� � � FORMCDHD� FORMCDHD�
 � �� F� �+ %�C� lcRepIDb� C� CCC�  �=f�	���� T� �CC� �C� �f�� � ������� %�C� �
���
 @� � �� T� ��  �� �# � ������� ������� J��  �(� � � T� �� �� ~$+�� CC�  �=f���� � ������� %�C� �
����
 @� � ��� T� ��  �� � %�CC��� �
����? T� �CC�� ���� CC��� �	� �� C�� ���6�� 5� � T� �� ��% �� �� �(�� C�� ������� T� �� ��� � �� �����  T� �� �����CC� ��f��! T� �� �����C � �� ��! T� �� �����C � �� �� �� �* T� �CC� ��fCC� ��fCC� ��f�� F� � %�C� ����� T� �� ��� � �� �����# T� �� ������ � � � �� T� �� ������ �� T� �� �����C� ��� T� �� �����C� ��� T� �� ������ �� T�
 � �CC� � �f�� � F� � � �� J�C� X�(� � � %�C� FORMCDHD���'� Q� � � %�C� FORMCDDT���J� Q� � �
 F��	 �� B�C� �
�� U  LCREPID
 LCFORMNAME	 LAFRMSETS	 LASPECFRM	 LAALLSETS	 LNCURALIA	 LNCOUNTER LCFORMID
 LASETTINGS
 LNCURALIAS THIS OPENFILE OARIAAPPLICATION DATADIR FORMCDHD CFORMMAJ CCURFORM	 MFORMSETS LNLOOPSTART	 LNSETSELM	 LNLOOPCNT CFORMID FORMCDDT COPTPROG
 MFRMSPSETS LFRXFORM OPTIONPROGRAM� 4�  � � 5� � %�C� � �
���� T� �C� � ��� H�D ���F �C� cModuleb� C� C� �
	� C� � � � \� � .FXP0	��� �% T� �� � � � \� � .FXP��* �C� � � � � \� � .FXP0���( T� �� � � � � \� � .FXP�� �C� � � � .FXP0��;� T� �� � � � .FXP��% �C� � � EB\� � .FXP0����# T� �� � � EB\� � .FXP��% �C� � � UP\� � .FXP0����# T� �� � � UP\� � .FXP��% �C� � � NC\� � .FXP0���# T� �� � � NC\� � .FXP��% �C� � � AS\� � .FXP0��[�# T� �� � � AS\� � .FXP�� 2���" T� �� � C� �=� \� �� � �� ���  � � U  OOBJ CMODULE
 LCPROGTODO THIS OPTIONPROGRAM OARIAAPPLICATION
 REPORTHOME ACTIVEMODULEID#  �  ������� J��  �(�  � U 
 LAFORMULAS changeformula,     �� reportprint�    �� reportpreview-    �� assingformula�    �� changedatapath�    �� openfile    �� chkform�    �� optprog#    �� Init#    ��1 � � a� �A ��A QA ��3 � � � A 2 � � A 2 q �1A ���A A 2 � �Q��A 3 � q �� 1A 3 � q1� ��s �r3� � � A 2� �3� � � A s�q � Q1B A �q � 11����rB q A � � A s� A q� A � � 6 � � q 2!� aQ����R1R1Q1Q1� !B � A 3 1� 1                               6  �        �  ?  #      c  �  *   )   �  _  6   2   ~  6  B   :   T  =  N   y   [  =  �   �   X  �  �    )   �                       -���                              �   %   d      �    �          �  U  R 4�  � � 5� � � � � T� �C�� F� � T� ��  �� %�� C�  
��� T� �C� �� %�C� �G� ��� � T� �� Z�� T� �� ��� �� � T� �C� �8��� T� �CC� �G�@ �� � �� ���(�� ���� � T� �� � Z�� �� T� �� � �� �)� T� ��  �� � %��  CC� 9�	 C� >Qg���� %�C� ���x� T� �� A�� T�
 �C� �� ��� %�� � Z���� T� �� � A�� ���$ T� �C� C� >�=CC� � �� � T�
 �CC� � �� � T�  �� �� �� T�  ��  ��� � T� ��  �� T� �� �� %�C� �
��K�
 F�� �� � U  LNSEQ CCHR	 LCSPREFIX
 LNCHARASCI LNI LCALIAS SEQUENCE LCCHAR	 LNCHARPOS LNRETLEN
 LCCHRTOUPD CSEQ_CHR LNRETVAL
 LCEXTRASTR� 4�  � � � � �# 5� � � � �	 �
 � � � 5� � T� �C�  ��/ T� �CC� lcFieldb� C�
 CC� f�� C� X6�� T� �C� W�� T�  �C�  f��: T� �CC� lcCompanyIdb� C� C� �
	� � � � � 6�� T� �� � �� T� �� �� T� �� � �� T� �� � �� T� �� � �� T� �� � �� T� �� � �� T� �C� � �=�� T� �C�	 GetMemVar� � �N��7 T� �CC� lcCompanyIdb� C� C� �
	� � � � 6��" T�  �C�
 M_UNQSTPRX � � �! ��4 %�C� lcCompanyIdb� C� C� �
	� � � 	���� %�C� sycComp���_� F�" � T�# �-�� T�$ �CC�]g�� T�% �CO�� G(�" (� cComp_Id� ��� T�# �a��& Q�  �� � syccomp��� cComp_Id� � %�C� � syccomp���g� T� �C�" �' ���h %�CC� �C� \� ��\fCC� �C� \� ��\f�. CC� �C� \� ��\fCC� �C� \� ��\f	��c�4 T� �C� �C� \� ��\C� C� \� ���\�� � � %��# ���� Q�" � ��� G(�" (�	 ltSycComp� %�C�% �C� syccompN����� #�� syccomp���% �� � � �2 T� �CC�	 lcGroupIdb� C� C� ��� C�X6��0 T� �CC�
 lcDivisionb� C� C� �� C�
X6�� T� �� ��& T�( �C�	 M_DIV_SEQ � � �! � Y�� <� �  %��( � C� �	� C� �
	��^� �) �������! T�) �������� DIVGROUP��" T�) ��������	 lcGroupId�� 5�* �& T�* �C� GetRelatedFields� � �N��% ��CC� ���) �	 CDIVISION�* �! �� <�* � T� �C� ��\�� �% T� �C�( � C� ��\� C�X6�� %�C� SEQUENCE�
���� T�+ �a��6 USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
 �-� F�, � T�+ �-�� T�- �CC�]g�� T�. �CO�� G(�, (�	 Cseq_type� �$ %�CC�  �
�� � SEQUENCE�
���� %�C� sydflfld�
���� T�0 �a��= USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
 �� F�1 � T�0 �-�� T�2 �CC�]g�� T�3 �CO��  G(�� sydflfld�(�	 Cfld_name� � %�C� sydfield�
��p� T�5 �a��= USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
 ��� F�6 � T�5 �-�� T�7 �CC�]g�� T�8 �CO��  G(�� sydfield�(�	 Cfld_name� � T�9 �CC� �� �  � � 6�� ��CC�9 �
�� sydfield��� F�1 � ��CC�9 �
���� -$��: �+��4 C�9 �
��� T�; ��1 �< ��) %�C� �
� CC�  �
�� SEQUENCE�	���� F�, � T�; �� �� ~$+��/ �= C�  �
����� T�; �C�; �> D�� �" T�; �C�; �P�  8��P�  �� �W r�� SEQUENCE�/ �> �= �? �@ �A ���  ���; ��� ���6 �? ���6 �@ ��C�  �� %��1 �: ���
� %�C� sydfiles�
���	� T�B �a��= USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
 ��	� F�C � T�B �-�� T�D �CC�]g�� T�E �CO��  G(�� sydfiles�(�	 Cfile_nam� � ��C�1 �F � sydfiles��� F�, � >�F ���C �F ��G ���C �G �� %��B ��M
� Q�C � ��
� G(�C (�
 ltSydfiles� %�C�E �C� SydfilesN����
� #�� Sydfiles���E �� � � � %��0 ���
� Q�1 � �� G(�1 (�
 ltSydflfld� %�C�3 �C� SydflfldN���� #�� Sydflfld���3 �� � � %��5 ��5� Q�6 � ��� G(�6 (�
 ltSydfield� %�C�8 �C� SydfieldN����� #�� Sydfield���8 �� � � � +�C� SEQUENCES
���� � T� ��, �> �� T� ��, �A �� T�H ��, �@ C�  >�� T� ��, �> �� T�	 ��  �� %�C�, �A �
��� 5�I �J �K � %��, �A C�  
��� %�CC�, �A �G� ��� T�I �� Z�� T�J �C�, �A ��� ��� T�I �CCC�, �A �G�@ �� T�J �CC�, �A �8��� � ��K ���(��J ����� T�	 ��	 � Z�� �� T�	 ��	 �I �� �� T�I ��  �� � �  %�C�, �F �
�
 C�, �G �
	�� � T�L �C�, �F ��� T�M �C�, �G ��� %�C�L �
���� T�N �a��D USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED
 ��
 F��L �� T�N �-�� T�O �CC�]g�� T�P �CO�� G(��L �(��M �� �
 F��L ��! T�Q �CCm�C� +�  Cm�\�� �R ���� 5�S � T�S �C� GetValidEntries�N��' %�C�Q �
� C �Q �R �S �! � 	��9� ��T ���(�C��R ����5�  T� ��	 C� �H C�	 >� 0���# +�CC �T ��R �  � �L ���1� ��C �  � � �U ��  T� ��	 C� �H C�	 >� 0��� � �� ���  T� ��	 C� �H C�	 >� 0��� +�C� �L ����� ��C �  � � �U ��  T� ��	 C� �H C�	 >� 0��� � � <�S � %��N ���� Q��L �� �� G(��L �(�	 ltSeqFile� %�C�P �C�L N���� #��L ���P �� � � � F�, �  T� ��	 C� �H C�	 >� 0��� ��C �  � � �U �� >�> ��� ��A ��� �� %��> � � � � 	����& >�A ��CC�A �� � A� CC�A � 6�� � %�C�	 �
����  T� �C�	 �C� �H �� 0��� � Z� T� ��  C� �H � 0��� %��+ ��$� Q�, � �}� G(�, (�
 ltSequence� %�C�. �C� SequenceN���y� #�� Sequence���. �� � �
 F�� ��
 B�� �� UV 	 LCSEQTYPE LCCOMPANYID	 LCGROUPID
 LCDIVISION LCFIELD LNRETVAL
 LCSAVALIAS	 LCDATADIR
 LNOLDGENNM
 LCEXTRASTR LCTOFIND LCKEYEXP	 LCCMPCODE
 LCCHRTOUPD OARIAAPPLICATION ACTIVECOMPANYID	 GCDATADIR DATADIR
 GCCOMP_MDL COMPANYINSTALLEDMODULES	 GCSYSHOME SYSPATH	 GCCURSITE CURRENTSITE
 GCACT_COMP	 GCORGPATH DEFAULTPATH
 GCACT_APPL ACTIVEMODULEID
 OGETMEMVAR THIS OFORM
 LCUNQPREFX DO SYCCOMP	 LUSYCCOMP	 LTSYCCOMP	 LESYCCOMP CCOMP_ID	 CCOM_DDIR
 LLDIVONSEQ LADIVDLT ORLATDFIELDS
 LUSEQUENCE SEQUENCE
 LTSEQUENCE
 LESEQUENCE	 CSEQ_TYPE
 LUSYDFLFLD SYDFLFLD
 LTSYDFLFLD
 LESYDFLFLD	 CFLD_NAME
 LUSYDFIELD SYDFIELD
 LTSYDFIELD
 LESYDFIELD	 LCPROPFLD
 LENUMERATE LNDEFSEQ NDEF_SEQ
 CSEQ_GROUP NSEQ_NO	 CDATA_TYP	 NFLD_WDTH CSEQ_CHR
 LUSYDFILES SYDFILES
 LTSYDFILES
 LESYDFILES	 CFILE_NAM	 CFILE_TAG LNRETLEN LCCHAR	 LNCHARPOS LNI	 LCSEQFILE LCSEQTAG	 LUSEQFILE	 LTSEQFILE	 LESEQFILE
 LCKEYFIELD LAVLDENT	 OGETVALID LNCOUNT	 EDIGETSEQ	 edigetseq,     �� do    ��1 � 1� q � Q� a� � q�A �!A � � B �� � � � !!� AA RA � � A � � � A 4 �q:s � �� � �� X�q!Baq � � Q� � aA �!�BA A � � � a�aA A A "� aq 1!s aQq QB R�� a� q � � aA B�� �� q � � A �� �� q � � A ��q ��q � �!A !A u�� �� q � � A �q �� � � q�qA A A � � � q�qA A � � � q�qA A A �A e� 1� ��� a� ��A �!A � � A B !!� B� � � � 1A � � q �q�3RA A � 3RA A q � � � �qA A A q 	Qq �aA A C �� � � q�qA A � � 3                       �     ,     'w  F    )                          ����    �  �                        ��   %   d      �  �   p          �  U  x 4�  � � � � 5� � � � � �	 � 5�
 � � � � � � 5� � � � � � � � 5� � � � � � � 5� � � � �3 %�C�
 laInfArrayb� C� C� lcFieldb� C��� � B�-�� � T�  �C� X��1 T� �CC�
 lcFillWithb� C� � L� CC� f�6�� %�C� ,� �� ��w� T�  �C� C� >C� ,� �R�� T�  �C�  ��� T� �CC�  �� � L� � V6�� ���' T� �C� � LDANT
� � L� � 6�� �. T� �CC�	 lcActCompb� C�	 �! �" � � 6�� T�# �C��  � ��� %��# � ��� B�-�� � T� �CC� �
�f�� T� �C�# ��  �� T� �C�# ��  �� T� �C�# ��  �� T�	 �C�# ��  �� T�
 �C�# ��  �� T� �C�# ��  �� T� �C�# ��  �� T� �C�# ��  �� T� �C�# �	�  �� T�$ �� �� T� �C � �$  � �% ��# T� �C� ALLC� � �'� �6���# T� �C� N/AC� � �'� �6��� T� �C�* �� T� �C� X�� T� �C�X� -C�X�� T� �CW�� H���O� �� � L� � � 	��)�) lcCurCode = &lcToUpdat[&lcCurPopUp,2]
 F�& � T� �C��� G((�E T� �C� �( �" CCODE_NO+lcSeprat+CDISCREP AS Code� � CDISCREP6�� %�� ����� SELECT &lcFields,CCODE_NO FROM Codes WHERE cDefCode+CRLTFIELD+CFLD_NAME = "N"+"N"+lcField ORDER BY CCODE_NO  INTO ARRAY &lcToUpdat
 �\�� SELECT &lcFields,CCODE_NO FROM Codes WHERE cDefCode+CRLTFIELD+CFLD_NAME = "N"+"N"+lcField ORDER BY CDISCREP  INTO ARRAY &lcToUpdat
 � F�& � SET ORDER TO TAG &lcTag
 T�  ��# ���� L�� %��	 ��%�0 DIMENSION &lcToUpdat[ALEN(&lcToUpdat,1)+1,2]
 = AINS(&lcToUpdat,1)
 &lcToUpdat[1] = lcAll
 &lcToUpdat[2] = lcAllCode
 � %��
 ����0 DIMENSION &lcToUpdat[ALEN(&lcToUpdat,1)+1,2]
 = AINS(&lcToUpdat,1)
 &lcToUpdat[1] = lcNA
 &lcToUpdat[2] = lcNACode
 �. lnNewVal    = ASCAN(&lcToUpdat, lcCurCode)
E &lcCurPopUp = IIF(lnNewVal <> 0, IIF(lnNewVal=1,1,lnNewVal/2), 1)
 �� � V��X� T� �C�  ���� F�& � T� �C��� G((� CODES�- T� �CC� N� � N� �� �' � C� X6�� G((� lcOrdTag� DIMENSION &lcToUpdat[1,2]
E &lcToUpdat[1,1] = IIF(llEditable, lcCode+lcSeprat+lcDesc, lcDesc)
 &lcToUpdat[1,2] = lcCode
 &lcCurPopUp     = 1
 T�  ��# ���� �� �� � T���� T� �C�	 lcAltFileb� C� C� �
	� C�	 lcAltIndxb� C	� C� �
	� C� lcAltExpb� C	� C� �
	� C�
 lcAltFieldb� C	� C� �
	�� T� �C� � � � �  6�� T� �C� � � � �  6�� T� �C� � � � �  6�� T� �C� � � � �  6�� J�C� X�(� � � %�� ��!�
 F�� �� T� �C��� T�( �C�CC�]g]�� lcCurRecP = &lcIdxExp
 SET ORDER TO TAG &lcSerIndx
% IF SEEK(&lcSerExp, lcSerFile)��
�! lcCode = ALLTRIM(&lcSerField)
 %�C� ��	 � � *��b
� T� �CC� �� � � � 6�� ��
�  lcCode = PADR(&lcSerField,6)
 F�& � T� �C��� G((� CODES�- T� �CC� N� � N� �� �' � C� X6�� G((� lcOrdTag� � �
 F�� �� SET ORDER TO TAG &lcTag
 ��C�) ��� � T� �C� ���� T� �C� ���� DIMENSION &lcToUpdat[1,2]
I &lcToUpdat[1,1]     = IIF(llEditable, lcCode+lcSeprat+lcDesc, lcDesc)
  &lcToUpdat[1,2]     = lcCode
 &lcCurPopUp         = 1
 T�  ��# ���� T�� �� � D��u� F�& � T� �C��� G((� cCode_No� J�C� X�(� � � %�C� D� ���y� T� ��* �� T� ��' �� � SET ORDER TO TAG &lcTag
 T� �C� ���� T� �C� ���� DIMENSION &lcToUpdat[1,2]
I &lcToUpdat[1,1]     = IIF(llEditable, lcCode+lcSeprat+lcDesc, lcDesc)
  &lcToUpdat[1,2]     = lcCode
 &lcCurPopUp         = 1
 T�  ��# ���� D�� �� � AN��O� DIMENSION &lcToUpdat[1,2]
< &lcToUpdat[1,1] = IIF(lcFillWith = "A", lcAll,     lcNA)
@ &lcToUpdat[1,2] = IIF(lcFillWith = "A", lcAllCode, lcNACode)
 &lcCurPopUp     = 1
 T�  ��# ���� �� � SHOW GET &lcCurPopUp

 F�� �� U+ 
 LAINFARRAY LCFIELD
 LCFILLWITH	 LCACTCOMP LNALIAS LCTAG	 LCTOUPDAT
 LCCURPOPUP LCSTATUS LLADDALL LLADDNOA	 LCALTFILE	 LCALTINDX LCALTEXP
 LCALTFIELD
 LLEDITABLE LCALL LCNA	 LCALLCODE LCNACODE LCSEPRAT	 LCCURCODE LCFIELDS LNNEWVAL	 LLALTFILE	 LCSERFILE	 LCSERINDX LCSEREXP
 LCSERFIELD LCCODE LCDESC LCORDTAG	 LCSETWITH OARIAAPPLICATION ACTIVECOMPANYID LNPOS LNFW
 GFISEDTBLE CODES CDISCREP LCIDXEXP	 LCCURRECP CCODE_NO do,     ��1 �1����13q A � q�� �� qA �Cq A CSQQQQQQQQ� �31� � �� � ��q � a Q� d� dA s ��� ���A � ���A �Q#!q � � ��Q�qq#�����2� � � a��Q��� q � � �A A � �� A !!����#q � 1A� � A �!!����3��qqB �� 3                       .      )   �                       �PROCEDURE Init
DODEFAULT()
IF !EMPTY(THIS.ReportFileName)
  lcReportHome = oAriaApplication.ReportHome
  IF !FILE(lcReportHome+THIS.ReportFileName)
    lcReportHome = oAriaApplication.ReportHome+oAriaApplication.ActiveModuleID+'\'
  ENDIF  
  ThisFormset.Ariaform1.Ariareport1.ReportFileName=lcReportHome+THIS.ReportFileName
  ThisFormset.Ariaform1.Ariareport1.ChangeDataPath()
ENDIF  
THISFORMSET.REFRESH()
ENDPROC
PROCEDURE Ariaform1.Refresh
IF EMPTY(ThisForm.Ariareport1.ReportFileName)
  ThisForm.Ariacontainer1.PREVIEW.Enabled = .F.
  ThisForm.Ariacontainer1.PRINT.Enabled = .F.  
ELSE
  ThisForm.Ariacontainer1.PREVIEW.Enabled = .T.
  ThisForm.Ariacontainer1.PRINT.Enabled = .T.  
ENDIF
ENDPROC
     �PROCEDURE do
*!*************************************************************
*! Name      : AddCurrencySymbol
*! Developer : Haytham El-Sheltawi
*! Date      : 11/29/1998
*! Purpose   : Function to add the currency symbol to a numeric value
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) Currency code.
*!                     2) Amount.
*!                     3) Number of decimal places.
*!*************************************************************
*! Example	      	 : .AddCurrencySymbol('USDLR' , 100.97 , 2)
*!*************************************************************
*
PARAMETERS lcCurrCode , lnAmount , lnDecimals

IF TYPE('lnAmount') <> 'N' .OR. TYPE('lcCurrCode') <> 'C'
  RETURN ''
ENDIF
lnDecimals = IIF(TYPE('lnDecimals') <> 'N' , 0 , lnDecimals)

PRIVATE lnSelect , lcOldOrd , llCloseFil , lcReturn

lnSelect   = SELECT()

IF !USED('SYCCURR')
  llCloseFil = .T.
  SELECT 0
  USE &gcSysHome.SYCCURR ORDER TAG CCURRCODE
ELSE
  llCloseFil = .F.
  SELECT SYCCURR
  lcOldOrd = ORDER()
  SET ORDER TO TAG CCURRCODE
ENDIF

lcCurrCode = PADR(ALLTRIM(lcCurrCode) , LEN(cCurrCode))
IF lnDecimals <> 0
  lcReturn = STR(lnAmount , LEN(ALLTRIM(STR(lnAmount))) + lnDecimals + 1 ,;
                 lnDecimals)
  
ELSE
  lcReturn = ALLTRIM(STR(lnAmount))
ENDIF

IF SEEK(lcCurrCode)
  lcReturn = IIF(SET('CURRENCY') = 'LEFT' , ALLTRIM(cCurrSmbl) , '') +;
             lcReturn +;
             IIF(SET('CURRENCY') = 'LEFT' , '' , ALLTRIM(cCurrSmbl))
  
ENDIF

IF llCloseFil
  USE
ELSE
  SET ORDER TO (lcOldOrd)
ENDIF

SELECT (lnSelect)

RETURN lcReturn

ENDPROC
     !�PROCEDURE do
*!*************************************************************
*! Name      : gfStyPrice
*! Developer : Renee Ezzat
*! Date      : 12/20/95
*! Purpose   : Return the style/color price in a given currency.
*!*************************************************************
*! Parameters: lcStyle    : Style
*!             lcColor    : color
*!             lcStyCur   : Style currency code
*!             lcPriceLvl : price level ('A', 'B', or 'C')
*!             llNoAdd    : .T. if adding prices on the fly
*!                          is not to be allowed despite the company
*!                          setup.
*!*************************************************************
*! Calls     : gfGetMemVar
*!             STYPRICE.SPR     
*!*************************************************************
*! Returns                : Price level in passed currncy for
*!                          the passed style/color, or
*!                          -1 if it is not found and/or not
*!                          added.
*!*************************************************************
*! Example   : lnPrice = gfStyPrice(lcStyle, lcColor, lcCurrCode, 'A')
*!*************************************************************
*E300328,1 RENEE 12/21/95. 
*E300620,4 Reham On 08/05/97
*E300620,4 Remove color from the function.
*B602290,1 HSS 11/29/98 Get the price from STYLE file in the case of base
*B602290,1              currency.
*:*************************************************************************
*E300620,4 Reham On 08/05/97  ** Begin **
*E300620,4 Remove th color parameter.
*PARAMETERS lcStyle, lcColor, lcPriceLvl, lcStyCur, llNoAdd 
PARAMETERS lcStyle, lcPriceLvl, lcStyCur, llNoAdd 
*E300620,4 Reham On 08/05/97  ** End   **
PRIVATE lcFilter, lnOldVal, lnCurAlias, lnCurTag, lnCurRec, llOpenFile   

*-- Add this line to create an object from the class GetMemVar to be used
*-- instead of gfGetMemVar [Begin]
LOCAL oGetMemVar
oGetMemVar = CREATEOBJECT('GetMemVar')
*-- Add this line to create an object from the class GetMemVar [End]

*E300328,1 Called only if multi currency
*E300637,1 Use the new global setups function
*IF !gfGetMemVar('LLMULCURR', qSysCode)

*-- Change this line to use oGetMemVar [Begin]
*IF !gfGetMemVar('LLMULCURR',gcAct_Comp)
IF !oGetMemVar.Do('LLMULCURR')
*-- Change this line to use oGetMemVar [End]

*E300637,1 (End)
  RETURN -1
ENDIF

*E300620,4 Reham On 08/05/97  ** Begin **
*E300620,4 Remove the color from the if condition.
*E300328,1 Check parameters
*E300328,1 Return if no parameters are passed
*IF EMPTY(lcStyle) .OR. EMPTY(lcColor) .OR.;
*   EMPTY(lcPriceLvl) .OR. ATC(lcPriceLvl, 'ABC') = 0)
IF EMPTY(lcStyle) .OR. EMPTY(lcPriceLvl) .OR. ATC(lcPriceLvl, 'ABC') = 0
*E300620,4 Reham On 08/05/97  ** End   **
  RETURN -1
ENDIF  

*E300328,1 If there is no currency code parameter, default with
*E300328,1 the base currency.
IF EMPTY(lcStyCur)
  
  *-- Change this line to use oAriaApplication.BaseCurrency
  *lcStyCur = gcBaseCurr
  lcStyCur = oAriaApplication.BaseCurrency
ENDIF  

*E300328,1 Check if STYPRICE is open,
*E300328,1 If the file is used, store current environment
lnCurAlias = SELECT()
lnOldVal   = 0
IF USED('STYPRICE')
  llOpenFile = .F.
  SELECT STYPRICE
  *E300328,1 Get current tag
  lnCurTag   = VAL(SYS(21))
  *E300328,1 Get current record
  lnCurRec   = IIF(!EOF(), RECNO(), 0)
  *E300328,1 Get current filter
  lcFilter   = FILTER() 
  SET FILTER TO
  SET ORDER TO TAG STYPRICE  
ELSE
  *E300637,1 Use the new global variables names
  *llOpenFile = gfOpenFile(qDD+'STYPRICE', 'STYPRICE' , 'SH')
  
  *-- Change this line to use oAriaApplication.DataDir [Begin]
  *llOpenFile =gfOpenFile(gcDataDir+'STYPRICE',gcDataDir+'STYPRICE','SH')
  
  llOpenFile = .T.
  SELECT 0
  USE (oAriaApplication.DataDir + 'STYPRICE') ORDER TAG STYPRICE
  
  *-- Change this line to use oAriaApplication.DataDir [End]
  *E300637,1 (End)
  
  *-- Move this line [Begin]
  *lnCurAlias = 0
  *-- Move this line [End]
ENDIF

*E300328,1 Adjust Customer level parameter.
lcPriceLvl = LEFT(ALLTRIM(lcPriceLvl), 1) 

*B602290,1 Add these lines to get the style price from the STYLE file in the
*B602290,1 case of base currency [Begin]

*B602290,1 If base currency

*-- Change this line to use oAriaApplication.BaseCurrency
*IF ALLTRIM(lcStyCur) == ALLTRIM(gcBaseCurr)
IF ALLTRIM(lcStyCur) == ALLTRIM(oAriaApplication.BaseCurrency)
  PRIVATE llCloseSty , lcStyOrder , lnStyRecNo
  
  IF !USED('STYLE')
    llCloseSty = .T.
    SELECT 0
    
    *-- Change this line to use oAriaApplication.DataDir [Begin]
    *=gfOpenFile(gcDataDir + 'STYLE' , gcDataDir + 'STYLE' , 'SH')
    USE (oAriaApplication.DataDir + 'STYLE') ORDER TAG STYLE
    
    *-- Change this line to use oAriaApplication.DataDir [End]
    
    SELECT STYLE
  ELSE
    llCloseSty = .F.
    SELECT STYLE
    lnStyRecNo = IIF(EOF() , 0 , RECNO())
    lcStyOrder = ORDER()
    SET ORDER TO TAG STYLE
  ENDIF
  
  lcStyle = PADR(lcStyle , LEN(Style))
  IF SEEK(lcStyle)
    This.Price = Price&lcPriceLvl
  ELSE
    This.Price = -1
  ENDIF
  
  IF llCloseSty
    USE IN STYLE
  ELSE
    SELECT STYLE
    SET ORDER TO (lcStyOrder) IN STYLE
    IF lnStyRecNo <> 0
      GO lnStyRecNo IN STYLE
    ELSE
       GO BOTTOM IN STYLE
       SKIP IN STYLE
    ENDIF
  ENDIF
ELSE    && If not base currency
*B602290,1 Add these lines to get the style price from the STYLE file [End]
  
  *E300620,4 Reham On 08/05/97  ** Begin **
  *E300620,4 Remove the color from the seek condition.
  *E300328,1 If a record exists, get the price that corresponds to the
  *E300328,1 passed level.
  *IF SEEK(lcStyle + lcColor + lcStyCur, 'STYPRICE')
  IF SEEK(lcStyle + lcStyCur, 'STYPRICE')
  *E300620,4 Reham On 08/05/97  ** End   **
    This.Price = STYPRICE.Price&lcPriceLvl
  *E300328,1 Otherwise,   
  ELSE
    This.Price = -1
    *E300637,1 Use the new global setups function
    *IF !llNoAdd .AND. gfGetMemVar('LLSTYPRICE', qSysCode)
    
    *-- Change this line to use oGetMemVar [Begin]
    *IF !llNoAdd .AND. gfGetMemVar('LLSTYPRICE', gcAct_Comp)
    IF !llNoAdd .AND. oGetMemVar.Do('LLSTYPRICE')
    *-- Change this line to use oGetMemVar [End]
    
    *E300637,1 (End)
      STORE 0 TO lnPriceA, lnPriceB, lnPriceC
      *E300637,1 Use the new global variables names
      *DO (qSD + 'STYPRICE.SPR')
      
      *-- Change this line to use DO FORM [Begin]
      *DO STYPRICE.SPR
      DO FORM (oAriaApplication.ScreenHome + 'SY\STYPRICE');
         WITH lcStyle , lcStyCur , lcPriceLvl , This
      *-- Change this line to use DO FORM [End]
      
      *E300637,1 (End)
    ELSE
      *E300620,4 Reham On 08/05/97  ** Begin **
      *E300620,4 Remove the color from the message.
      *=gfDialog('I', 'No prices are defined for style/color '  +;
      *               ALLTRIM(lcStyle) + '/' + ALLTRIM(lcColor) +;
      *               ' in ' + ALLTRIM(lcStyCur)) 
      
      *B602290,1 Change this line to use gfModalGen() [Begin]
      *=gfDialog('I', 'No prices are defined for style ' + ;
      *               ALLTRIM(lcStyle) + ' in ' + ALLTRIM(lcStyCur)) 
      
      *-- Change this line to use oAriaApplication.MessageBox [Begin]
      *=gfModalGen("TRM00344B00000" , "DIALOG" , ALLTRIM(gfItemMask('HI')) +;
      *            " " + ALLTRIM(lcStyle) + "|" + ALLTRIM(lcStyCur))
      
      LOCAL oItemMask , lcStyleTit
      oItemMask  = CREATEOBJECT('GetItemMask')
      lcStyleTit = ALLTRIM(oItemMask.Do("HI"))
      oItemMask  = .NULL.
      =oAriaApplication.MessageBox("TRM00344B00000" , "DIALOG" ,;
                                   lcStyleTit + " " + ALLTRIM(lcStyle) +;
                                   "|" + ALLTRIM(lcStyCur))
      *-- Change this line to use oAriaApplication.MessageBox [End]
      
      *B602290,1 Change this line to use gfModalGen() [End]
      
      *E300620,4 Reham On 08/05/97  ** End   **
    ENDIF  
  ENDIF  
  
*B602290,1 Add these lines to get the style price from the STYLE file in the
*B602290,1 case of base currency [Begin]
ENDIF    && End of IF ALLTRIM(lcStyCur) == ALLTRIM(gcBaseCurr)
*B602290,1 Add these lines to get the style price from the STYLE file [End]

*E300328,1 Restore environment
IF !llOpenfile
  SELECT STYPRICE
  SET ORDER TO (lnCurTag)
  IF !EMPTY(lcFilter)
    SET FILTER TO (lcFilter)
  ENDIF
  IF lnCurRec > 0
    GO lnCurRec
  ENDIF  
ENDIF
SELECT (lnCurAlias)

*E300328,1 Return price
RETURN This.Price


ENDPROC
     �PROCEDURE trnsstr
*FUNCTION lfTrnsStr
PARAMETERS lcValueStr,lcDataType,lcDirection
DO CASE
  CASE lcDataType $ 'CM'
     RETURN ALLT(lcValueStr)
  CASE lcDataType = 'N'
      RETURN VAL(lcValueStr)
  CASE lcDataType='D'
     RETURN CTOD(lcValueStr)
  CASE lcDataType = 'L'
     RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
ENDCASE

ENDPROC
PROCEDURE do
*!*************************************************************
*! Name      : gfGetMemVar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/05/95
*! Purpose   : Return Variable(s) for company settings from SYCSETUP
*!*************************************************************
*! Parameters: lcArray   && variable to restore
*!                       && OR one dimension array to restore the variable(s)
*!                       name with the same variable name
*!                       && OR two dimension array to restore the variable(2)
*!                       in column 1 into variable names in column 2
*!             lcCompID  &&company id to get its settings
*!*************************************************************
*! Called by : 
*!*************************************************************
*! Returns            : VALUE OF VARIABLE OR no of variables restored
*!*************************************************************
*! Example   : lcVarName=gfGetMemVar('LLMULCURR ','01')
*!             WILL return from the sycsetup file the setting
*!             value for company 01 the variable called "LLMULCURR "
*!*************************************************************
*
*FUNCTION gfGetMemVar
PARAMETERS lcArray,lcCompID
PRIVATE lnAliasNo,llCUsedBy,llArrayORvar,llTwoDimen,lcSetupTag,;
        lcConfgTag,lnRetCount,lcOnErr,llError,laVarArr,llUseSycC
*B601818,1  Get company path
PRIVATE lcCompDir, lcSetPath, llReUsedBy, lnCurTag, lnCurRec
*B601818,1  end

lnAliasNo=SELECT()
llUsedBy  = .F.
llCUsedBy = .F.
llSUsedBy = .F.
llUseSycC = .F.
lcCompID  = IIF(TYPE('lcCompId')<>'C',oAriaApplication.ActiveCompanyID,lcCompID)
*B601818,1  Get company path
llUseComp = !USED('SYCCOMP')
IF !USED('SYCCOMP')
  USE (oAriaApplication.SysPath+'SYCCOMP') IN 0 ORDER TAG CCOMP_ID
ENDIF
lcCompDir  = IIF(SEEK(lcCompID, 'SYCCOMP'), ALLTRIM(SYCCOMP.cCom_DDir), oAriaApplication.DataDir)
llReUsedBy = .F.
*B601818,1  end
llArrayORvar = TYPE('lcArray[1]')='C'
llTwoDimen = IIF(llArrayORvar AND ALEN(lcArray,2)=2,IIF(TYPE('lcArray[1,2]')='L','A','N'),'V' )
IF !llArrayORvar AND ',' $ lcArray
   DIMENSION laVarArr[1]
   =gfSubStr(lcArray,@laVarArr)
   DIMENSION lcArray[ALEN(laVarArr)]
   =ACOPY(laVarArr,lcArray)
   llArrayORvar = .T.
   llTwoDimen = 'V'
*ELSE
*  lcArray = UPPER(PADR(LCARRAY,10))   
ENDIF
IF !USED('SETUPS')
  SELECT 0
  *B601818,1 use SETUPS from the company directory
  *USE (oAriaApplication.DataDir+'SETUPS')
  USE (lcCompDir+'SETUPS') AGAIN
  *B601818,1 end
  llSUsedBy = .T.
ELSE
  SELECT SETUPS
  *B601818,1 Check if the file is opened from the company path
  lcSetPath = SET('FULLPATH')
  SET FULLPATH ON
  lcSetupsDir = DBF()
  SET FULLPATH &lcSetPath
  IF !(lcCompDir) $ lcSetupsDir
    lnCurTag  = VAL(SYS(21))
    lnCurRec  = RECNO()
    USE (lcCompDir+'SETUPS') AGAIN
    llReUsedBy = .T.
  ENDIF  
  *B601818,1 end  
ENDIF
lcSetupTag =TAG()
SET ORDER TO TAG VARNAME
lcRetVal=''
lnRetCount=0
lcOnErr=ON('ERROR')
ON ERROR llError = .T.
IF !llArrayORvar
  IF SEEK(PADR(UPPER(lcArray),10))
    DO CASE
      CASE cDefa_Typ='V'
        lcRetVal = THIS.TrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
      CASE cDefa_Typ='E'
       lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
    ENDCASE
  ELSE
    llUseSycC = .T.
    IF !USED('SYCCONFG')
      SELECT 0
      USE (oAriaApplication.Syspath+'SYCCONFG')
      llCUsedBy = .T.
    ELSE
      SELECT SYCCONFG
    ENDIF
    lcConfgTag=TAG()
    SET ORDER TO TAG VARNAME

    IF SEEK(PADR(UPPER(lcArray),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=THIS.TrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE    
    ENDIF
  ENDIF
ELSE
  llUseSycC = .T.
  IF !USED('SYCCONFG')
    SELECT 0
    USE (oAriaApplication.Syspath+'SYCCONFG')
    llCUsedBy = .T.
  ELSE
    SELECT SYCCONFG
  ENDIF
  lcConfgTag=TAG()
  SET ORDER TO TAG VARNAME

  FOR lnCount = 1 TO ALEN(lcArray,1)
    llError = .F.
    lcRetVal=''
    SELECT SETUPS
    IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=THIS.TrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE
    ELSE
      SELECT SYCCONFG
      IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
        DO CASE
          CASE cDefa_Typ='V'
            lcRetVal=THIS.TrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
          CASE cDefa_Typ='E'
           lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
        ENDCASE    
      ENDIF
    ENDIF  
    DO CASE
      CASE llTwoDimen = 'N'
        &lcArray[lnCount,2] = lcRetVal
      CASE llTwoDimen = 'V'
        &lcArray[lnCount] = lcRetVal
      CASE llTwoDimen = 'A'
        lcArray[lnCount,2] = lcRetVal
    ENDCASE
    lnRetCount=lnRetCount+IIF(!llError,1,0)    
  ENDFOR
ENDIF
ON ERROR &lcOnErr
IF llUseSycC
  SELECT SYCCONFG
  IF !EMPTY(lcConfgTag)
    SET ORDER TO TAG (lcConfgTag)
  ELSE
    SET ORDER TO
  ENDIF
  IF llcUsedBy
    USE IN SYCCONFG
  ENDIF
ENDIF  

SELECT SETUPS
IF !EMPTY(lcSetupTag)
  SET ORDER TO TAG (lcSetupTag)
ELSE
  SET ORDER TO
ENDIF
IF llSUsedBy
  USE IN SETUPS
ENDIF

*B601818,1 ReUse SETUPS file
IF llReUsedBy .AND. !EMPTY(lcSetupsDir)
  SELECT SETUPS
  USE (lcSetupsDir) ORDER lnCurTag
  IF BETWEEN(lnCurRec, 1, RECCOUNT())
    GO lnCurRec
  ELSE
    GO TOP
  ENDIF  
ENDIF  
IF llUseComp
  USE IN SYCCOMP
ENDIF
*B601818,1 end  

SELECT (lnAliasNo)
RETURN IIF(!llArrayORvar,lcRetVal,lnRetCount)





ENDPROC
     v���    ]  ]                        �   %           z              �  U  -	 4�  � � � � � � �' 5� � �	 �
 � � � � � �" %�C�  �� C� lcTagb� C��� � %�CC���} � B�-�� �� � T�  �C�� � �% %�C� �� C� lcAddGrpb� C��� � T� ��  �� T� �� E�� �� � T� �C� ��� T� �� �� � T�	 �C��
 F��  �� T�
 �CC�]g�� %�C� �����' %�C� �
� C�	 lcKeyCodeb� C	���� H�k���. �C� �� C� lcTagb� C� CC�]�	���� F�CC�	 �
� �	 � � 6�� B�-�� �C� �
��� G((�� �� %�C� �
��� G((� lnOldTag� F�CC�	 �
� �	 � � 6�� B�-�� � 2��� %�CC�]���Y� F�CC�	 �
� �	 � � 6�� B�-�� � %�C� �
���� F�CC�	 �
� �	 � � 6�� B�-�� � � �# lcAdrCode = cCont_Cod&lcGrpCode
 � � ������� T� ��  ��; T� �CC� lnLineNob� N� C� ���	� C� 8� �6�� J�-�(� � � %�C� SYCINT�
��w� T� �a��) Q�  �� � � SYCINT���	 cContCode� ��� F� � T� �CC�]g�� G((�	 cContCode� � %�C� SYCCOMP�
���� T� �a��) Q�  �� � � SYCCOMP��� cComp_ID� �5� F� � T� �CC�]g�� T� �CO�� G((� cComp_ID� �D %�C� � SYCINT��) C� � � SYCCOMP�� C� � � SYCINT�	���� T� �������� � ��- T� �������C�  �
 .cAddress1� ��� T� �������� � �� T� �������� � ��- T� �������C�  �
 .cAddress2� ��� T� �������� � �� T� �������� � ��- T� �������C�  �
 .cAddress3� ��� T� �������� � �� T� �������� � ��- T� �������C�  �
 .cAddress4� ��� T� �������� �  �� T� �������� �! ��- T� �������C�  �
 .cAddress5� ��� T� �������� �" �� T� �������� �# ��- T� �������C�  �
 .cAddress6� ��� T� �������� �$ �� %�C�
 lcCurrCodeb� C��)�% &lcCurrCode    = SYCINT.cCurrCode
 ���� �� � ��C�� ���� T�% ��  �� ��& ���(�C�� ������ %�C �& �� � ����' T�' �CCC �& �� �C �& �� \���8 T�% ��% CC�% �� C�% �R� ,� �  � � , 6�' �� � �� ��) T�% �C�  �	 .cAddressC� �Z� ��� � %�C� SYCCOMP����� %�� ��G� Q� � ��� G(� (�
 lnSavCmpTg� %�C� �C� SYCCOMPN����� #� �� �� � � � %�C� SYCINT����� %�� ���� Q� � ��� G(� (�
 lnSavIntTg� � � G(��  �(� lnOldTag� F�CC�	 �
� �	 � � 6��	 B��% �� U(  LCALIAS LCTAG	 LCKEYCODE	 LCADRCODE LNLINENO LCADDGRP
 LCCURRCODE
 LNSAVINTTG
 LNSAVCMPTG
 LCCURALIAS LNOLDTAG	 LLOPENINT	 LLOPENCMP
 LLCONTINUE	 LNCOMPREC	 LCGRPCODE	 LAADDRESS OARIAAPPLICATION SYSPATH	 CCONTCODE SYCINT CCOMP_ID SYCCOMP ACTIVECOMPANYID
 CCONT_CODE	 NPART1ORD	 NPART1LEN	 NPART2ORD	 NPART2LEN	 NPART3ORD	 NPART3LEN	 NPART4ORD	 NPART4LEN	 NPART5ORD	 NPART5LEN	 NPART6ORD	 NPART6LEN LCRETVAL LNCOUNT	 LCADDPART do,     ��1 �y)� q � � A A Q� � � � � A � � � q� ��q � �q A � �q A �q A A A 6A 2� �� c� �� q !A r� �� q � A C�������������������Q� A � ��r�A A � �A b� � � q�� A A A R� � � qA A ��� 2                       /      )   ]                       �PROCEDURE do
*FUNCTION CrtTmp
PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag
PRIVATE lcFileType,laFileStruc,lcFileName,lnWorkArea,lcTagType,lnCount
lnWorkArea = SELECT()

oGetTempName = CREATEOBJECT('GetTempName')
lcFileName = IIF(TYPE('lcFile')='C',lcFile,oGetTempName.DO())
RELEASE oGetTempName

lcFileType = 'A'
lcFileType = IIF(TYPE("lcFileStruc[1]")#"U",'A',;
             IIF(LEFT(ALLT(lcFileStruc),1)='(','S','F'))
DO CASE
  CASE lcFileType = 'F'
    SELECT (lcFileStruc)
    =AFIELDS(laFileStruc)
    lcFileType = 'A'
  CASE lcFileType= 'A'
    =ACOPY(lcFileStruc,laFileStruc)
ENDCASE
lcTagType = 'A'
lcTagType = IIF(TYPE("lcTagExp[1]")#"U",'A','S')

IF lcFileType = 'A'
  CREATE TABLE (oAriaApplication.WorkDir+lcFileName) FROM ARRAY laFileStruc
ELSE
  CREATE TABLE (oAriaApplication.WorkDir+lcFileName) &lcFileStruc
ENDIF
SELECT (lcFileName)
IF lcTagType = 'A'
  FOR lnCount = 1 TO ALEN(lcTagExp,1)
    INDEX ON &lcTagExp[lnCount,1] Tag &lcTagExp[lnCount,2]
  ENDFOR
ELSE
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') <> 'C'
    lcTag = Field(1)
  ENDIF
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
    INDEX ON &lcTagExp Tag &lcTag
  ENDIF
ENDIF  
USE
USE (oAriaApplication.WorkDir+lcFileName) &&EXCL
IF lcTagType = 'A'
  SET ORDER TO TAG &lcTagExp[1,2]
ELSE
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
    SET ORDER TO TAG (lcTag)
  ENDIF
ENDIF  
SELECT (lnWorkArea)
RETURN lcFileName

ENDPROC
     	�PROCEDURE do
*!********************************************************************
*! Name      : gfGetExSin
*! Developer : Mohamed Hassan
*! Date      : 11/27/95
*! Purpose   : Return Exchange Rate sign
*!********************************************************************
*! Parameters: lcCurrency  && Currency to define or return exh. rate for
*!             lcBaseCurr  && Variable to define base currency. 
*!             lcUntSin    && Pointer to unit sign character.
*!********************************************************************
*! Call      : 
*!********************************************************************
*! Returns   : * OR /
*!********************************************************************
*! Example   : lcExSign = gfGetExSin(@lcUntSin,'ENG')
*!             The user can pass the currency as a parametter 
*!             or the function is going to use the base currency.
*!             The function is going to return the exchnage rate
*!             sign.
*!********************************************************************
*
PARAMETERS lcUntSin, lcCurrency, lcBaseCurr

LOCAL lcReturn , llClose

IF TYPE('lcUntSin') = 'C' 
  lcUntSin = '/'
ENDIF

IF TYPE('lcBaseCurr') $ 'UL'
  lcBaseCurr = oAriaApplication.BaseCurrency
ENDIF

IF lcCurrency = lcBaseCurr
  RETURN '*'
ENDIF
** This file should always be in use if the system is multi currency,
** Sometimes it is closed by Apparel programs, so, if it is
** not used,open the file.
IF !USED('SYCCURR')
  *HAYTHAR ADD [Begin]
  llClose = .T.
  *HAYTHAR ADD [End]
  USE (oAriaApplication.SysPath+"SYCCURR") IN 0

*HAYTHAR ADD [Begin]
ELSE
  llClose = .F.
*HAYTHAR ADD [End]
ENDIF  
SET ORDER TO TAG CCURRCODE IN SYCCURR
=SEEK(lcBaseCurr,'SYCCURR')

*HAYTHAR ADD [Begin]
lcReturn = IIF((SYCCURR.cCurMeth = 'M' .AND.;
               !(ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept)) .OR.;
               SYCCURR.cCurMeth = 'D' .AND.;
               (ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept) , '*' , '/')

IF llClose
  USE IN SYCCURR
ENDIF
*HAYTHAR ADD [End]

*B601623,1 Change this line [Begin]
*RETURN IIF((SYCCURR.cCurMeth = 'M' .AND. !(lcCurrency $ SYCCURR.mCurExcept)) .OR. SYCCURR.cCurMeth = 'D' .AND.  (lcCurrency $ SYCCURR.mCurExcept),'*', '/')
RETURN lcReturn
*B601623,1 Change this line [End]

*B600875,1 function to create the upmodule txt file if needed 

ENDPROC
     �PROCEDURE do
*************************************************************************
*FUNCTION FErrInfo
*DESC: Function get error information, which depends on transaction type. 
*NOTE: Called from Function Checkprd
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed 
*PARA: lcType   : Transaction type.
*    : lcDKind  : Date kind
*    : lcErrMes1: Message line #1
*    : lcErrMes2: Message line #2
*MODI:
*E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
*E100219,9              from operation
*N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
*E300324,6 RENEE 12/28/95 function fErrInfo() : Change text of type 'IN'
*E300324,6                AND 'V1' from 'System date' to 'Invoice date',
*E300324,6                and remove the checking message.
*************************************************************************
PARAMETERS lcType,lcDKind,lcErrMes1,lcErrMes2

DO CASE
  CASE lcType = 'IN'                 && Invoice
    *E300324,6 Change message text.
    *&lcDKind   = 'System date '
    &lcDKind   = 'Invoice date '
    *E300324,6 end.
    
    &lcErrMes1 = 'Not allowed to create invoices for this date. '
    
    *E300324,6 Remove checking message text.
    *&lcErrMes2 = 'Please check the system date.'
    &lcErrMes2 = ''
    *E300324,6 end.  
    
  CASE lcType = 'VI1'                 && Void invoice 1st.
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to void invoices for this date. '
    &lcErrMes2 = 'Please check the system date.'
    
  CASE lcType = 'VI2'                 && Void Invoice 2nd.
    &lcDKind   = 'Invoice date '
    &lcErrMes1 = 'Not allowed to void invoices from prior periods.'
    &lcErrMes2 = ''
      
  CASE lcType = 'IA'                 && Inventory Adjustment
    &lcDKind   = 'Transaction date '
    &lcErrMes1 = 'Not allowed to enter inventory adjustment for this date.'
    &lcErrMes2 = ''
      
  CASE lcType = 'IP'                 && Inventory Physical
    &lcDKind   = 'Transaction date '
    &lcErrMes1 = 'Not allowed to enter physical inventory for this date.'
    &lcErrMes2 = ''
    
  CASE lcType = 'ZE'                 && Zero out Stock
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to zero out stock for this date. '
    &lcErrMes2 = 'Please check the system date.'
      
  CASE lcType = 'PO'                 && Receive P/Os
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to enter P/O receivings for this date. '
    &lcErrMes2 = 'Please check the system date.'
      
  CASE lcType = 'CT'                 && Receive C/Ts
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to enter C/T receivings for this date. '
    &lcErrMes2 = 'Please check the system date.'

  CASE lcType = 'RM'                 && Return merchandise
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to receive returns for this date. '
    &lcErrMes2 = 'Please check the system date.'
    
  CASE lcType = 'VR1'                 &&  Void Return 1st
    &lcDKind   = 'System date '
    &lcErrMes1 = 'Not allowed to void credit memo for this date. '
    &lcErrMes2 = 'Please check the system date.'
    
  CASE lcType = 'VR2'                 &&  Void Return 2nd
    &lcDKind   = 'Return date '
    &lcErrMes1 = 'Not allowed to void credit memo from prior periods.'
    &lcErrMes2 = ''
    
  CASE lcType = 'CR'                 &&  Customer Payment
    &lcDKind   = 'Batch date '
    &lcErrMes1 = 'Not allowed to enter payments for this batch date.'
    &lcErrMes2 = ''

  CASE lcType = 'AJ'                 &&  Adjustment
    &lcDKind   = 'Batch date '
    &lcErrMes1 = 'Not allowed to enter adjustments for this batch date.'
    &lcErrMes2 = ''
    
  CASE lcType = 'KO'                 &&  Key Off
    &lcDKind   = 'Key off date '
    &lcErrMes1 = 'Not allowed to make key off for this date.'
    &lcErrMes2 = ''

  CASE lcType = 'RO'                 &&  Receive from material operation
    &lcDKind   = 'Material operation receiving date '
    &lcErrMes1 = 'Not allowed to receive from material operation for this date.'
    &lcErrMes2 = ''

  CASE lcType = 'RS'                 &&  Receive from style operation
    &lcDKind   = 'Style operation receiving date '
    &lcErrMes1 = 'Not allowed to receive from style operation for this date.'
    &lcErrMes2 = ''

  CASE lcType = 'MM'                 &&  Receive M.F.G. order
    &lcDKind   = 'M.F.G. order receiving date '
    &lcErrMes1 = 'Not allowed to receive M.F.G. order for this date.'
    &lcErrMes2 = ''
  OTHERWISE
    &lcDKind   = 'Transaction date '
    &lcErrMes1 = ''
    &lcErrMes2 = ''
ENDCASE   

RETURN(.T.)

ENDPROC
      ��ࡱ�                >  ��	                               ����        ������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������   ����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������R o o t   E n t r y                                               ��������                                �^�#��   @       O l e O b j e c t D a t a                                            ����                                        �        A c c e s s O b j S i t e D a t a                             &  ������������                                       8        C h a n g e d P r o p s                                         ������������                                       �          ����         ����      ������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������V     �      F P     ��ݺ��ݺ�  �                ,d d �8                              8                                 �   WindowState 	   I
         WindowShowGroupTree    L                                  <   p                                                                                                                                             WindowShowExportBtn    L      WindowShowSearchBtn    L      WindowShowPrintSetupBtn    L      WindowShowRefreshBtn    L                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
XPROCEDURE do
*!*************************************************************
*Function to return the item code mask or the item code header.
*!*************************************************************
PARAMETERS lcMaskOrHead
PRIVATE lcReturn,llStructUse,lnRecNo,lcStructOrd,lnCurAlias,llArray,;
        laSeg,lcItemDim,lcHeader,lnStartNonM,lnNoSeg,lnPosistion
STORE '' TO lcReturn        
llArray = TYPE('lcMaskOrHead[1]') # 'U'
lcItemDim = 'I'
IF !llArray
  IF TYPE('lcMaskOrHead')<>'C'
    RETURN .F.
  ENDIF
  lcMaskOrHead = UPPER(lcMaskOrHead)
  lcItemDim = IIF(LEN(lcMaskOrHead)>1,RIGHT(lcMaskOrHead,1),'I')  
  lcMaskOrHead = LEFT(lcMaskOrHead,1)  
ENDIF
lcLoopExt = lcItemDim 
lnCurAlias = SELECT()
llStructUse = .F.
IF !USED('ICISTRU')
  USE (oAriaApplication.DataDir+'ICISTRU') IN 0
  llStructUse = .T.
ELSE
  SELECT ICISTRU
  lcStructOrd = ORDER()
  lnRecNo = RECNO()
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO
=SEEK('U1')
lcHeader = cIsegHead
lnNoSeg = 0
lnPosistion = 1
SCAN REST WHILE citemrecty+cisegno = 'U'
  IF lcLoopExt <> 'N'
    lnNoSeg = lnNoSeg + 1
    DIMEN laSeg[lnNoSeg,7]
    laSeg[lnNoSeg,1] = cisegType
    laSeg[lnNoSeg,2] = ALLT(cisegsdes)
    laSeg[lnNoSeg,3] = REPL('X',nisegsize)
    laSeg[lnNoSeg,4] = lnPosistion
    laSeg[lnNoSeg,5] = ALLT(CISEGLDES)
    laSeg[lnNoSeg,6] = CISEGSEPR
    laSeg[lnNoSeg,7] = LSEGENDMAJ  
    lcReturn = lcReturn+REPL('X',nisegsize)+ALLT(CISEGSEPR)
  ENDIF
  lnPosistion = lnPosistion + nisegsize + LEN(ALLT(CISEGSEPR))
  IF lcLoopExt = 'N' AND lSegEndMaj
    lcLoopExt = 'I'
  ENDIF
  IF lcItemDim = 'M' AND lSegEndMaj
     EXIT
  ENDIF    
ENDSCAN
IF llArray
  DIMEN lcMaskOrHead[ALEN(laSeg,1),ALEN(laSeg,2)]
  lcReturn=ACOPY(laSeg,lcMaskOrHead)
ELSE  
  DO CASE
    CASE  lcMaskOrHead = 'S'
      lcReturn = lnNoSeg
    CASE  lcMaskOrHead = 'P' AND  lcItemDim='M'
      IF THIS.DO('SN')>0
        lcReturn = SUBSTR(lcReturn,1,LEN(lcReturn)-1)
      ENDIF  
    CASE lcMaskOrHead = 'H' AND lcItemDim='M'
      IF THIS.DO('SN')>0
        lcReturn = SUBSTR(lcHeader,1,laSeg[lnNoSeg,4]+LEN(laSeg[lnNoSeg,2])-1)
      ELSE
        lcReturn = lcHeader        
      ENDIF  
    CASE lcMaskOrHead = 'H' AND lcItemDim='N'  AND lnNoSeg>0
      lcReturn = SUBSTR(lcHeader,laSeg[1,4])  
    CASE lcMaskOrHead = 'H' AND lcItemDim='I'
      lcReturn = lcHeader  
  ENDCASE
ENDIF  

IF llStructUse 
    USE IN ICISTRU
ELSE    
  SELECT ICISTRU
  SET ORDER TO TAG (lcStructOrd)
  IF lnRecNo>0 AND lnRecNo<=RECCOUNT()
    GO lnRecNo
  ENDIF  
ENDIF
SELECT (lnCurAlias)
RETURN lcReturn


ENDPROC
     9PROCEDURE do
*:---------------------------------------------------------------------
*! Name      : gfwCodePop
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to fill any code array with on of the 
*:             following values :
*:               1) A list of the available codes from the codes file.
*:               2) The default code value.
*:               3) "ALL"
*:               4) "N/A"
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : IsEdtble()
*:---------------------------------------------------------------------
*: Passed Parameters  : laInfArray : Pointer to the code information array.
*:                      lcField    : The code to be displayed.
*:                      lcFillWith : Fill the array with ...
*:                      "L" : List of the available codes.
*:                      "D" : Default value.
*:                      "A" : "All"
*:                      "N" : "N/A"
*:---------------------------------------------------------------------
*: Example            : = gfwCodePop(@laCodInfo, "CTERMCODE", "A")
*:---------------------------------------------------------------------
PARAMETERS laInfArray, lcField, lcFillWith, lcActComp

PRIVATE lnAlias, lcTag, lcToUpdat, lcCurPopUp, lcStatus, llAddAll
PRIVATE llAddNoA, lcAltFile, lcAltIndx, lcAltExp, lcAltField, llEditable
PRIVATE lcAll, lcNA, lcAllCode, lcNACode, lcSeprat, lcCurCode, lcTag
PRIVATE lcFields, lnNewVal, llAltFile, lcSerFile, lcSerIndx, lcSerExp
PRIVATE lcSerField, lcCode, lcDesc, lcOrdTag

*-- If the passed parameters are not valid than go out of the function.
IF TYPE("laInfArray")#"C" OR TYPE("lcField")#"C"
  RETURN .F.
ENDIF

*-- If the last parameter is not passed than default it to build the list.
lcSetWith  = SPACE(0)
lcFillWith = IIF(TYPE("lcFillWith")#"C", "L", ALLTRIM(UPPER(lcFillWith)))
IF ATC(",",lcFillWith) > 0
  lcSetWith  = RIGHT(lcFillWith,LEN(lcFillWith)-ATC(",",lcFillWith))
  lcSetWith  = ALLTRIM(lcSetWith)
  lcFillWith = IIF(EMPTY(lcSetWith), "L", "V")
ELSE
  lcFillWith = IIF(!(lcFillWith $ "LDANT"), "L", lcFillWith)
ENDIF

*-- Set the company used variable.
lcActComp = IIF(TYPE("lcActComp")#"C", oAriaApplication.ActiveCompanyID, lcActComp)

*-- Get the position of the nedded field in the code information array.
lnPos      = ASCAN (laInfArray, lcField)

*-- If it does not exist than go out of the function.
IF lnPos = 0 
  RETURN .F.
ENDIF

*-- Be sure that the field name has a 10 character length.
lcField    = UPPER(PADR(lcField,10))

*-- Initialize the needed variables from the code information array.
lcToUpdat  = laInfArray[lnPos+1]
lcCurPopUp = laInfArray[lnPos+2]
lcStatus   = laInfArray[lnPos+3]
llAddAll   = laInfArray[lnPos+4]
llAddNoA   = laInfArray[lnPos+5]
lcAltFile  = laInfArray[lnPos+6]
lcAltIndx  = laInfArray[lnPos+7]
lcAltExp   = laInfArray[lnPos+8]
lcAltField = laInfArray[lnPos+9]

*-- Check if this code is editable or not.
lnFW       = 0
*llEditable = oAriaApplication.IsEdtble(lcField, @lnFW, lcActComp)
llEditable = gfIsEdtble(lcField, @lnFW, lcActComp)

*-- The "ALL" string and code, and the "N/A" string and code.
lcAll      = PADR("ALL",IIF(llEditable, 39, 30))
lcNA       = PADR("N/A",IIF(llEditable, 39, 30))
lcAllCode  = CHR(42)
lcNACode   = SPACE(0)
lcSeprat   = SPACE(1) + "-" + SPACE(1)
lnAlias    = SELECT()
DO CASE
  
  *-- If we want to build the array and it is not built from before.
  CASE lcFillWith = "L" AND lcStatus # lcFillWith
    lcCurCode = &lcToUpdat[&lcCurPopUp,2]
    SELECT Codes
    lcTag = TAG()
    SET ORDER TO
    lcFields = IIF(llEditable, "CCODE_NO+lcSeprat+CDISCREP AS Code", "CDISCREP")

    *--HDM B602174,1 Sort the codes by the code number if the code is editable, 
    *--HDM B602174,1 otherwise sort them by the description.
    *--HDM B602174,1 (Added the if condition and the SQL in the else part).
    IF llEditable
      SELECT &lcFields,CCODE_NO FROM Codes;
      WHERE cDefCode+CRLTFIELD+CFLD_NAME = "N"+"N"+lcField;
      ORDER BY CCODE_NO ;
      INTO ARRAY &lcToUpdat
    ELSE
      SELECT &lcFields,CCODE_NO FROM Codes;
      WHERE cDefCode+CRLTFIELD+CFLD_NAME = "N"+"N"+lcField;
      ORDER BY CDISCREP ;
      INTO ARRAY &lcToUpdat
    ENDIF
    *--HDM B602174,1 End.

    SELECT Codes
    SET ORDER TO TAG &lcTag
    laInfArray[lnPos+3] = "L"

    IF llAddAll
      DIMENSION &lcToUpdat[ALEN(&lcToUpdat,1)+1,2]
      = AINS(&lcToUpdat,1)
      &lcToUpdat[1] = lcAll
      &lcToUpdat[2] = lcAllCode
    ENDIF
    
    IF llAddNoA
      DIMENSION &lcToUpdat[ALEN(&lcToUpdat,1)+1,2]
      = AINS(&lcToUpdat,1)
      &lcToUpdat[1] = lcNA
      &lcToUpdat[2] = lcNACode
    ENDIF

    lnNewVal    = ASCAN(&lcToUpdat, lcCurCode)
    &lcCurPopUp = IIF(lnNewVal <> 0, IIF(lnNewVal=1,1,lnNewVal/2), 1)

  *-- Put a passed value in the array.
  CASE lcFillWith = "V"
    lcCode = PADR(lcSetWith,6)
    SELECT Codes
    lcOrdTag = TAG()
    SET ORDER TO TAG CODES
    lcDesc = IIF(SEEK("N"+lcCode+"N"+lcField), cDiscrep, SPACE(0))
    SET ORDER TO TAG lcOrdTag
    DIMENSION &lcToUpdat[1,2]
    &lcToUpdat[1,1] = IIF(llEditable, lcCode+lcSeprat+lcDesc, lcDesc)
    &lcToUpdat[1,2] = lcCode
    &lcCurPopUp     = 1
    laInfArray[lnPos+3] = lcFillWith
    
  *-- Put only the default value from an alternative file in the Array.
  CASE lcFillWith = "T" 
    llAltFile  = TYPE("lcAltFile")  = "C" AND !EMPTY(lcAltFile ) AND ;
                 TYPE("lcAltIndx")  = "C" AND !EMPTY(lcAltIndx ) AND ;
                 TYPE("lcAltExp")   = "C" AND !EMPTY(lcAltExp  ) AND ;
                 TYPE("lcAltField") = "C" AND !EMPTY(lcAltField)
    lcSerFile  = IIF(llAltFile, lcAltFile , "") 
    lcSerIndx  = IIF(llAltFile, lcAltIndx , "")
    lcSerExp   = IIF(llAltFile, lcAltExp  , "")
    lcSerField = IIF(llAltFile, lcAltField, "")

    STORE SPACE(0) TO lcCode, lcDesc
    IF llAltFile
      SELECT (lcSerFile)
      lcTag     = TAG()
      lcIdxExp  = SYS(14,VAL(SYS(21)))
      lcCurRecP = &lcIdxExp
      SET ORDER TO TAG &lcSerIndx
      IF SEEK(&lcSerExp, lcSerFile)
        lcCode = ALLTRIM(&lcSerField)
        IF EMPTY(lcCode) OR lcCode = "*"
          lcDesc = IIF(EMPTY(lcCode), lcNA, lcAll)
        ELSE
          lcCode = PADR(&lcSerField,6)
          SELECT Codes
          lcOrdTag = TAG()
          SET ORDER TO TAG CODES
          lcDesc = IIF(SEEK("N"+lcCode+"N"+lcField), cDiscrep, SPACE(0))
          SET ORDER TO TAG lcOrdTag
        ENDIF  
      ENDIF  
      SELECT (lcSerFile)
      SET ORDER TO TAG &lcTag
      = SEEK(lcCurRecP)
    ENDIF  
    lcCode = PADR(lcCode,06)
    lcDesc = PADR(lcDesc,30)
    DIMENSION &lcToUpdat[1,2]
    &lcToUpdat[1,1]     = IIF(llEditable, lcCode+lcSeprat+lcDesc, lcDesc)
    &lcToUpdat[1,2]     = lcCode
    &lcCurPopUp         = 1
    laInfArray[lnPos+3] = "T"

  *-- Put only the default value from the codes file in the Array.
  CASE lcFillWith = "D" 
    SELECT Codes
    lcTag = TAG()
    SET ORDER TO TAG cCode_No
    STORE SPACE(0) TO lcCode, lcDesc
    IF SEEK('D'+lcField)
      lcCode = cCode_No
      lcDesc = cDiscrep
    ENDIF
    SET ORDER TO TAG &lcTag
    lcCode = PADR(lcCode,06)
    lcDesc = PADR(lcDesc,30)
    DIMENSION &lcToUpdat[1,2]
    &lcToUpdat[1,1]     = IIF(llEditable, lcCode+lcSeprat+lcDesc, lcDesc)
    &lcToUpdat[1,2]     = lcCode
    &lcCurPopUp         = 1
    laInfArray[lnPos+3] = "D"

  *-- Put only "N/A" or "All" in the list.
  CASE lcFillWith $ "AN" 
    DIMENSION &lcToUpdat[1,2]
    &lcToUpdat[1,1] = IIF(lcFillWith = "A", lcAll,     lcNA)
    &lcToUpdat[1,2] = IIF(lcFillWith = "A", lcAllCode, lcNACode)
    &lcCurPopUp     = 1
    laInfArray[lnPos+3] = lcFillWith

ENDCASE

*-- Refresh the popup.
SHOW GET &lcCurPopUp

SELECT(lnAlias) 


ENDPROC
     fPROCEDURE do
*!*************************************************************
*! Name      : gfRltFld
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995 
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*FUNCTION gfRltFld
*E300631,1 YMA 04/06/97 Added the 3rd parameter "lcFldName"
PARAMETERS lcCodeVal,laArrayNam, lcFldName
PRIVATE laTempCodes,lcObjValue

IF EMPTY(lcCodeVal)   && Case N/A 
  FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
    DO CASE
      CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Cc'
        lcObjValue = ''

      CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Nn'
        lcObjValue = 0

      CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Ll'
        lcObjValue = .F.

      CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Dd'
        lcObjValue = {}
                
    ENDCASE
    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
*    SHOW GET (lcFieldNam)
  ENDFOR  
  RETURN
ENDIF

lcSavSelct  = ALIAS()   && Variable to save the currently selected file.
*E300631,1 YMA 04/06/97 Select the codes file instead of SYCCodes.
*SELECT SYCCODES         && Select CODES file
llUseCodes = .F.
IF !USED("CODES")
*TAK E300973,1 Changed to work under visual.
* USE (gcDataDir+"Codes") IN 0
  USE (oAriaApplication.DataDir+"Codes") IN 0

  llUseCodes = .T.
ENDIF
SELECT CODES         && Select CODES file
*E300631,1 YMA 04/06/97 End.
lcSavOrder = SYS(22)    && Save the file order
SET ORDER TO 0          && To activate rushmore

DECLARE laTempCodes[1]
laTempCodes = ' '

*E300631,1 YMA 04/06/97 Changed the file name to be "Codes" instead of "SYCCodes".
*E300631,1 YMA 04/06/97 And include the lcFldName in the "where" clause.
*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*  FROM SYCCODES;
*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' ;
*  AND   CCODE_NO = lcCodeVal ;
*  INTO ARRAY laTempCodes
*TAK E300973,1 Changed to work under visual.
*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*  FROM CODES;
*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' + lcFldName;
*  AND   CCODE_NO = lcCodeVal ;
*  INTO ARRAY laTempCodes
SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
  FROM CODES;
  WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + lcFldName;
  AND   CCODE_NO = lcCodeVal ;
  INTO ARRAY laTempCodes
*TAK E300973,1 End.
*E300631,1 YMA 04/06/97 End.

FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
  lnPosition = ASCAN(laTempCodes,laArrayNam[lnCount,1])

  IF lnPosition = 0     && not found

  ELSE
    lnPosition = ASUBSCRIPT(laTempCodes,lnPosition,1)
    DO CASE
      CASE laTempCodes[lnPosition,2] = 'C'
        lcObjValue = laTempCodes[lnPosition,3]

      CASE laTempCodes[lnPosition,2] = 'N'
        lnDecimPos = AT('.',laTempCodes[lnPosition,3])
        IF lnDecimPos > 0
          lcSavDecim = SET('DECIMALS')  && Save old decimals setting
          SET DECIMALS TO lnDecimPos
          lcObjValue = VAL(laTempCodes[lnPosition,3])
          SET DECIMALS TO &lcSavDecim          
        ELSE
          lcObjValue = VAL(laTempCodes[lnPosition,3])
        ENDIF  

      CASE laTempCodes[lnPosition,2] = 'L'
        lcObjValue = IIF(ALLTRIM(laTempCodes[lnPosition,3]) $ 'YT',.T.,.F.)

      CASE laTempCodes[lnPosition,2] = 'D'      
        lcObjValue = CTOD(laTempCodes[lnPosition,3])
    ENDCASE

    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ENDIF  
ENDFOR  

SET ORDER TO &lcSavOrder
IF llUseCodes
  USE IN Codes
ENDIF
SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)


ENDPROC
     �PROCEDURE do
*:---------------------------------------------------------------------
*! Name      : gfIsEdtble
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to tell if a specific code is editable by the
*!             user or not.
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : None
*:---------------------------------------------------------------------
*: Passed Parameters  : lcField  -> The code to be checked.
*:                      lnFieldW -> Pointer to a numeric variable to 
*:                                  hold the field width.
*:---------------------------------------------------------------------
*: Example            : = gfIsEdtble("TERMS", @lnWidth)
*:---------------------------------------------------------------------
*Modifications :
*E301046,4 MAB 11/24/1998 SYDFIELD.mCodeInfo now can have more than one code.
*
PARAMETERS lcPField, lnFieldW, lcActvComp
PRIVATE llRetVal, lcDataDir

gcAct_Comp = oAriaApplication.ActiveCompanyId
gcDataDir  = oAriaApplication.DataDir
gcSysHome  = oAriaApplication.SysPath

*-- Set the company used variable with it's path.
lcActvComp = IIF(TYPE("lcActvComp")#"C", gcAct_Comp, lcActvComp)
lcDataDir  = gcDataDir
IF !(lcActvComp == gcAct_Comp)
  USE (gcSysHome+"SYCCOMP") IN 0 ORDER cComp_ID AGAIN ALIAS CompFile
  IF SEEK(lcActvComp,'CompFile')
    *E301098,1 Hesham (Start)
    *lcDataDir = ALLTRIM(CompFile.cCom_DDir)
    lcDataDir = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
    *E301098,1 Hesham (End)
  ENDIF
  USE IN CompFile
ENDIF  

USE (gcSysHome+"SYDFIELD") IN 0 ORDER CFLD_NAME AGAIN ALIAS FieldFile
USE (lcDataDir+"CODESET" ) IN 0 ORDER Fildname  AGAIN ALIAS CodeSetF
*E301046,4 Change [= 'EDITABLE'] to ['EDITABLE' $] because the fields may include other codes.
*llRetVal = IIF(SEEK(lcPField, "FieldFile"), (ALLTRIM(FieldFile.mCodeInfo) = "EDITABLE"), .F.)
llRetVal = IIF(SEEK(lcPField, "FieldFile"),;
              ("EDITABLE" $ UPPER(ALLTRIM(FieldFile.mCodeInfo))), .F.)
*E301046,4 end
lnFieldW = IIF(SEEK(lcPField, "CodeSetF" ), CodeSetF.nfld_wdth, FieldFile.nFld_Wdth)
USE IN FieldFile
USE IN CodeSetF

RETURN llRetVal
     :PROCEDURE do
*!**************************************************************************
*!
*!      Function:  lfGetAdr
*!
*!**************************************************************************
*  Gets address according to the address code, returns address
*
PARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode

*** lcAlias   : source file name 
*** lcTag     : source file tag that is to be used in seeking
*** lckeycode : search key code (of the source file) (optional)
*** lcAdrCode : address code (optional)
*** lnLineNo  : The Address line number to return

PRIVATE lnSavIntTg, lnSavCmpTg, lcCurAlias, lnOldTag,;
        llOpenInt, llOpenCmp, llContinue, lnCompRec,lcCurrCode
 
 * You have to send the source file and 1 or more from the following parameters
 * 1 - The alias name for the source file or you have it the currently selected
 * 2 - Address code to be used in getting the address line  OR
 * 3 - Tag name and Seek Expression to get the  Address code
 * 4 - You can have the source file opened with the proper tag and just send
 *     the seek expr. (In case of not sending Tag ID there must be an active one)      
 
 IF EMPTY(lcAlias) .OR. TYPE('lcTag') <> 'C'
   IF EMPTY(ALIAS())
     RETURN .F.
   ELSE   
     lcAlias = ALIAS()
   ENDIF  
 ENDIF
 IF EMPTY(lcAddGrp) OR TYPE('lcAddGrp') <> 'C'
   lcAddGrp  = ''
   lcGrpCode = 'E'
 ELSE  
   lcAddGrp  = ALLTRIM(lcAddGrp)
   lcGrpCode = lcAddGrp
 ENDIF   
 lcCurAlias = ALIAS()
 SELECT (lcAlias)
 lnOldTag = VAL(SYS(21))   

 *-- No Address code has been sent
 IF EMPTY(lcAdrCode) 
   IF !EMPTY(lcKeyCode) .AND. TYPE('lcKeyCode') <> 'C'
     DO CASE

       *-- A Search Expr has been sent and no Tag Has been Sent and no active tag
       CASE (EMPTY(lcTag) .OR. TYPE('lcTag') <> 'C') AND EMPTY(SYS(22))
         SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
         RETURN .F.

       *-- A Search Expr and a Tag ID have been sent 
       CASE !EMPTY(lcTag)
         *lnOldTag = VAL(SYS(21))   
         SET ORDER TO TAG (lcTag)
         *-- The Search expr is not found
         IF !SEEK(lcKeyCode)
           SET ORDER TO lnOldTag
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
 
       *-- A search expr has been sent without a Tag 
       OTHERWISE 
         *-- There is no active tag
         IF EMPTY(SYS(22)) 
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
         *-- The Search Expr. is not found
         IF !SEEK(lcKeyCode)
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
     ENDCASE 
   ENDIF  

   *-- Just to be able to set the old tag even if it has not been 
   *-- changed in the above DO CASE
   *lnOldTag = VAL(SYS(21))   

   lcAdrCode = cCont_Cod&lcGrpCode
 ENDIF

DECLARE laAddress[6,3]
laAddress = " "
lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
STORE .F. TO llOpenInt, llOpenCmp
*** Check being on a correct alias
   
IF !USED('SYCINT')  && Check if the internationals file is open or not.
  llOpenInt  = .T.     && Indicates that the file is open by the function.
  ** Use the file and assign the index.
  USE (oAriaApplication.SysPath + 'SYCINT') ORDER TAG cContCode IN 0 
ELSE
  SELECT SYCINT       
  lnSavIntTg = VAL(SYS(21))
  SET ORDER TO TAG cContCode   && Change the order
ENDIF  

IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
  llOpenCmp  = .T.     && Indicates that the file is open by the function.
  ** Use the file and assign the index.
  USE (oAriaApplication.SysPath + 'SYCCOMP') ORDER TAG cComp_ID IN 0 
ELSE
  SELECT SYCCOMP       
  lnSavCmpTg = VAL(SYS(21))
  lnCompRec  = RECNO()
  SET ORDER TO TAG cComp_ID   && Change the order
ENDIF  

IF SEEK(lcAdrCode,'SYCINT') .OR. (SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP') ;
   .AND. SEEK(SYCCOMP.cCont_Code,'SYCINT'))
  laAddress[1,1] = SYCINT.nPart1Ord
  laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
  laAddress[1,3] = SYCINT.nPart1LEN
  laAddress[2,1] = SYCINT.nPart2Ord
  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp) 
  laAddress[2,3] = SYCINT.nPart2LEN
  laAddress[3,1] = SYCINT.nPart3Ord
  laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
  laAddress[3,3] = SYCINT.nPart3LEN      
  laAddress[4,1] = SYCINT.nPart4Ord
  laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
  laAddress[4,3] = SYCINT.nPart4LEN      
  laAddress[5,1] = SYCINT.nPart5Ord
  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
  laAddress[5,3] = SYCINT.nPart5LEN      
  laAddress[6,1] = SYCINT.nPart6Ord
  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
  laAddress[6,3] = SYCINT.nPart6LEN
  IF TYPE("lcCurrCode") = 'C'
    &lcCurrCode    = SYCINT.cCurrCode
    SHOW GET (lcCurrCode)
  ENDIF  
  =ASORT(laAddress,1)
  lcRetVal=''
  FOR lnCount = 1 TO ALEN(laAddress,1)
    IF laAddress[lnCount,1] = lnLineNo
      *lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]),laAddress[lnCount,3])
      lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
      lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',', ') + lcAddPart
     ENDIF
  ENDFOR
ELSE
  lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
ENDIF  

IF USED('SYCCOMP')
  IF llOpenCmp 
    USE IN SYCCOMP
  ELSE
    SET ORDER TO lnSavCmpTg IN SYCCOMP  
    IF BETWEEN(lnCompRec,1,RECCOUNT("SYCCOMP"))
      GOTO lnCompRec IN SYCCOMP
    ENDIF  
  ENDIF
ENDIF
  
IF USED('SYCINT')
  IF llOpenInt
    USE IN SYCINT
  ELSE
    SET ORDER TO lnSavIntTg IN SYCINT
  ENDIF  
ENDIF
SET ORDER TO lnOldTag IN (lcAlias)
SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  

RETURN lcRetVal

ENDPROC
     $TPROCEDURE do
*************************************************************************
*FUNCTION CheckPrd
*DESC: Function to validate transaction date
*NOTE: This function is called from evry transaction program.
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed 
*PARA: ldDate   : Transaction date to be check
*    : lcPeriod : Transaction Period
*    : lcFYear  : Transaction Fiscal Year 
*    : lcTranTyp: Type of transaction calls this function
*! MODI:  WAM 09/19/94
*!        1) Modified to call the function 'gfDialog' instead of the 
*!           function 'MsgCenter' to display messages when validate 
*!           transactions dates. Function'MsgCenter' has been deleted also.
*!B602317,1 WAM 12/06/98 Open SBT system company file with another name
*************************************************************************
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg

LOCAL oGetMemVar , oFErrInfo
oGetMemVar = CREATEOBJECT('GetMemVar')
oFErrInfo  = CREATEOBJECT('FErrInfo')

PRIVATE lcDType,lcAddMes1,lcAddMes2,lcSysDir,lcGlVers,lcGlComp, ;
        lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias
        
lnAlias = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

=oGetMemVar.Do('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO')
lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO lcDType,lcAddMes1,lcAddMes2

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  *B602317,1 Open SBT system company file with another name
  *=gfOpenFile(lcSysDir+'SYCCOMP',lcSysDir+'COMPID','SH')
  *=SEEK(lcGlComp,'SYCCOMP')

  USE lcSysDir+'SYCCOMP' ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP')
  *B602317,1 (End)
  
  SELECT 0
  USE (lcSysDir+'SYCHFIS') ORDER TAG COMPID1 AGAIN ALIAS TMPSYCHFIS
  SELECT 0
  USE (lcSysDir+'SYCDFIS') ORDER TAG COMPID1 AGAIN ALIAS TMPSYCDFIS

  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
  ENDIF  
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')  && Transaction is neither 
                                  && 'Void invoice' nor 'void return'.
        llDummy =  oFErrInfo.Do(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      *B602317,1 Open SBT system company file with another name
      *IF Yearprd <>  SYCCOMP.CURYR+SYCCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      *B602317,1 (End)

        llDummy   =  oFErrInfo.Do(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF    
  ENDIF  
  
  *HAYTHAR ADD [BEGIN]
  USE IN TMPSYCHFIS
  USE IN TMPSYCDFIS
  *HAYTHAR ADD [END]
  
  *B602317,1 Open SBT system company file with another name
  USE IN SBTCOMP
  *B602317,1 (End)
ELSE
  SELECT 0
  USE (oAriaApplication.SysPath+'SYCCOMP') ORDER TAG CCOMP_ID AGAIN ALIAS TMPSYCCOMP
  =SEEK(oAriaApplication.PrntCompanyID,'TMPSYCCOMP')
  IF 'GL' $ TMPSYCCOMP.mModlset
    *=gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP','','SH')
    USE (ALLTRIM(TMPSYCCOMP.CCOM_DDIR)+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    *-- Variable that hold the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    llAllPBB = TGLSETUP.LSETALBBE
    *-- Variable that showes the Allow posting before beginning balance (End)
    USE IN TGLSETUP 
  ELSE  
    lDSETBBDAT={}
    *-- Variable that showes the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    *-- .T. is assigend as default because we need not to check
    *-- if the GL module not installed or not linked
    llAllPBB = .T.
    *-- Variable that hold the Allow posting before beginning balance (End)
  ENDIF  
  *E300692,5 Use FISHD, FSPRD instead of SYCFISHD, SYCFSPRD
  *=gfOpenFile(gcSysHome+'SYCFISHD',gcSysHome+'COMPFYEAR','SH')
  *=gfOpenFile(gcSysHome+'SYCFSPRD',gcSysHome+'COMFYRPRDI','SH')
  SELECT 0
  USE (oAriaApplication.DataDir+'FISHD') ORDER TAG COMPFYEAR AGAIN ALIAS TMPFISHD
  SELECT 0
  USE (oAriaApplication.DataDir+'FSPRD') ORDER TAG COMFYRPRDI AGAIN ALIAS TMPFSPRD
  *E300692,5 end
  llContinue = .T.
  IF SEEK('')
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt)
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period     
  ENDIF  
  IF llHideMsg
    *HAYTHAR ADD [BEGIN]
    USE IN TMPSYCCOMP
    USE IN TMPFISHD
    USE IN TMPFSPRD
    *HAYTHAR ADD [END]
    SELECT (lnAlias)
    RETURN(llContinue)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(TMPSYCCOMP.CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF 
  IF llContinue         
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    *-- Check if the system is linked to GL And Allow posting before beginning Balance (Start)
    *-- AAMER 11/12/98
    *IF !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    *-- Check if the system is linked to GL And Allow posting before beginning Balance (End) 
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF  
  ENDIF  
  IF llContinue .AND. Lfsplocks         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue 
    IF Lfspclsds               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')
        llDummy =  oFErrInfo.Do(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1 
        *E300420,1 Button : 00000 
        *E300420,1 Ok
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> TMPSYCCOMP.CCURR_YER+TMPSYCCOMP.CCURR_PRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy =  oFErrInfo.Do(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1 
        *E300420,1 Button : 00000 
        *E300420,1 Ok
        =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF    
  ENDIF  
  
  *HAYTHAR ADD [BEGIN]
  USE IN TMPSYCCOMP
  USE IN TMPFISHD
  USE IN TMPFSPRD
  *HAYTHAR ADD [END]
ENDIF
IF !llContinue             && There is an error.
  IF lcTranTyp $ 'VI2VR2'       && Transaction is either 'Void invoice'
                                && or 'Void return'
    lcErrorM1  = ' not in the current period. '
    lcErrorM2 = ''
  ENDIF
  llDummy =  oFErrInfo.Do(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
  lcErrorM1= lcDType + lcDate + lcErrorM1
  *E300420,1 Message : 00274
  *E300420,1 
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =oAriaApplication.MessageBox('INM00274B00000','ALERT',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  *=gfDialog( 'I',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)

ENDPROC
     ����    �  �                        �v   %   @      �  �   L          �  U  � 4�  � � � � � �� � � T� �C�	 GetMemVar�N�� T� �C� FErrInfo�N��/ 5� � �	 �
 � � � � � � � � T� �CW�� J��  �(� � � � �7 ��C�' M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO� � �� T�
 �C� ��� T� �C� ��� T� �C� ��� J�C�X�(� � �	 � T� �C�  *�� %�� � S����1 Q�  ��
 � SYCCOMP��� SBTCOMP�� 'COMPID'� ��C� � SBTCOMP��� F�  �% Q��
 � SYCHFIS��� �� COMPID1� F�  �% Q��
 � SYCDFIS��� �� COMPID1� T� �a�� %�C� ���'�* -$�C�  � � ��+��  � �	 � � 	�� � %�C4
��� T� �-��/ T� ��"  does not fall within any period. �� T� ��  �� ���) &lcFYear  = SUBSTR(Yearprd,1,4)      
) &lcPeriod = SUBSTR(Yearprd,5,2)      
 � %�� � � 	��<� T� �-��3 T� ��&  falls in a permanently locked period.�� T� ��  �� � %�� � � 	���� T� �-��' T� ��  falls in a locked period.�� T� ��  �� � %�� ��x� %�� ���� %�� � VI2VR2
��u�7 T�  �C � � lcDType�	 lcAddMes1�	 lcAddMes2� � ��: lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
 T� ��  ��- ��C� INM00274B00000� ALERT� � �! �" �� ��� T� �-�� � �t�. %��# �$ �% �$ �& � � � VI2VR2
	��p�7 T�  �C � � lcDType�	 lcAddMes1�	 lcAddMes2� � ��= lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
 T� ��  ��- ��C� INM00274B00000� ALERT� � �! �" �� � � � Q� � Q� � Q�$ � �z� F�  �) Q��! �' � SYCCOMP���) �� CCOMP_ID� ��C�! �* �
 TMPSYCCOMP��� %�� GL�) �+ ��K�% Q�  ��C�) �, �� GLSETUP���- � T�. ��- �/ �� T�0 ��- �1 �� Q�- � �q� T�. ��        �� T�0 �a�� � F�  �( Q��! �2 � FISHD���4 ��	 COMPFYEAR� F�  �) Q��! �2 � FSPRD���6 ��
 COMFYRPRDI� T� �a�� %�C�  ���� -$�C�  �7 �8 ��+��  �7 �� � %�C4
��f� T� �-��/ T� ��"  does not fall within any period. �� T� ��  �� ��� &lcFYear  = Cfisfyear      
 &lcPeriod = Cfspprdid      
 � %�� ���� Q�) � Q�4 � Q�6 �
 F�� ��
 B�� �� �# %�� � �9 CC�) �: g�Z	��W� T� �-��/ T� ��"  belongs to a history fiscal year.�� T� ��  �� � %�� ��	�/ %�� � A� �0 
	� C�. �
	� �  �. 	��	� T� �-��5 T� ��(  falls before the begining balance date.��C T� ��6  No posting allowed before the begining balance date. �� � � %�� � �; 	��u	� T� �-��' T� ��  falls in a locked period.�� T� ��  �� � %�� ��^� %��< ��n
� %�� � VI2VR2
��W
�7 T�  �C � � lcDType�	 lcAddMes1�	 lcAddMes2� � ��: lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
 T� ��  ��- ��C� INM00274B00000� ALERT� � �! �" �� �j
� T� �-�� � �Z�2 %��9 �= �) �: �) �> � � � VI2VR2
	��V�7 T�  �C � � lcDType�	 lcAddMes1�	 lcAddMes2� � ��= lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
 T� ��  ��- ��C� INM00274B00000� ALERT� � �! �" �� � � � Q�) � Q�4 � Q�6 � � %�� 
��p� %�� � VI2VR2����) T� ��  not in the current period. �� T� ��  �� �7 T�  �C � � lcDType�	 lcAddMes1�	 lcAddMes2� � �� T� �� � � ��5 ��C� INM00274B00000� ALERT� � � �	 �! �" ��
 F�� �� B�-�� �
 F�� �� B�a�� U?  LDDATE LCFYEAR LCPERIOD	 LCTRANTYP	 LLHIDEMSG
 OGETMEMVAR	 OFERRINFO LCDTYPE	 LCADDMES1	 LCADDMES2 LCSYSDIR LCGLVERS LCGLCOMP LCDATE
 LLCONTINUE	 LCERRORM1	 LCERRORM2 LNALIAS M_POST_PPRD	 M_SYS_DIR	 M_GL_VERS M_GL_CO DO COMPID1
 TMPSYCHFIS
 TMPSYCDFIS BDATE EDATE COMPID PERMLCK PLOCKED PCLOSED LLDUMMY OARIAAPPLICATION
 MESSAGEBOX YEARPRD SBTCOMP CURYR CURPRD SYSPATH CCOMP_ID
 TMPSYCCOMP PRNTCOMPANYID MMODLSET	 CCOM_DDIR TGLSETUP
 LDSETBBDAT	 DSETBBDAT LLALLPBB	 LSETALBBE DATADIR	 COMPFYEAR TMPFISHD
 COMFYRPRDI TMPFSPRD	 DFSPPBGDT	 DFSPPENDT	 CFISFYEAR	 CCURR_YER	 LFSPLOCKS	 LFSPCLSDS	 CFSPPRDID	 CCURR_PRD do,     ��1  q� ���� �r� � � q� !Q� Q� Q� � �A � � �� � ��A A� 1� A A� q� A � � �r�� �� � A � �s�� �A A A � � � � � ��aR� � 1� B � �� �� � �A � � �� � ��A � � � � � � A 2� �� A � �� Q1A A A� q� A � � �q�� �� � A � !q�� �B A A � � � B � q�� A qQU� � A � � 2                       I$      )   �                       ���    �  �                        'E   %   �
      �  O             �  U  �
 4�  � � � � H�  ��
� ��  � IN��� �  &lcDKind   = 'Invoice date '
A &lcErrMes1 = 'Not allowed to create invoices for this date. '
 &lcErrMes2 = ''
 ��  � VI1��I� &lcDKind   = 'System date '
? &lcErrMes1 = 'Not allowed to void invoices for this date. '
0 &lcErrMes2 = 'Please check the system date.'
 ��  � VI2����  &lcDKind   = 'Invoice date '
C &lcErrMes1 = 'Not allowed to void invoices from prior periods.'
 &lcErrMes2 = ''
 ��  � IA��h�$ &lcDKind   = 'Transaction date '
K &lcErrMes1 = 'Not allowed to enter inventory adjustment for this date.'
 &lcErrMes2 = ''
 ��  � IP����$ &lcDKind   = 'Transaction date '
I &lcErrMes1 = 'Not allowed to enter physical inventory for this date.'
 &lcErrMes2 = ''
 ��  � ZE���� &lcDKind   = 'System date '
@ &lcErrMes1 = 'Not allowed to zero out stock for this date. '
0 &lcErrMes2 = 'Please check the system date.'
 ��  � PO��E� &lcDKind   = 'System date '
F &lcErrMes1 = 'Not allowed to enter P/O receivings for this date. '
0 &lcErrMes2 = 'Please check the system date.'
 ��  � CT���� &lcDKind   = 'System date '
F &lcErrMes1 = 'Not allowed to enter C/T receivings for this date. '
0 &lcErrMes2 = 'Please check the system date.'
 ��  � RM���� &lcDKind   = 'System date '
A &lcErrMes1 = 'Not allowed to receive returns for this date. '
0 &lcErrMes2 = 'Please check the system date.'
 ��  � VR1��5� &lcDKind   = 'System date '
B &lcErrMes1 = 'Not allowed to void credit memo for this date. '
0 &lcErrMes2 = 'Please check the system date.'
 ��  � VR2���� &lcDKind   = 'Return date '
F &lcErrMes1 = 'Not allowed to void credit memo from prior periods.'
 &lcErrMes2 = ''
 ��  � CR��J� &lcDKind   = 'Batch date '
E &lcErrMes1 = 'Not allowed to enter payments for this batch date.'
 &lcErrMes2 = ''
 ��  � AJ���� &lcDKind   = 'Batch date '
H &lcErrMes1 = 'Not allowed to enter adjustments for this batch date.'
 &lcErrMes2 = ''
 ��  � KO��Y�  &lcDKind   = 'Key off date '
= &lcErrMes1 = 'Not allowed to make key off for this date.'
 &lcErrMes2 = ''
 ��  � RO��	�5 &lcDKind   = 'Material operation receiving date '
P &lcErrMes1 = 'Not allowed to receive from material operation for this date.'
 &lcErrMes2 = ''
 ��  � RS���	�2 &lcDKind   = 'Style operation receiving date '
M &lcErrMes1 = 'Not allowed to receive from style operation for this date.'
 &lcErrMes2 = ''
 ��  � MM��C
�/ &lcDKind   = 'M.F.G. order receiving date '
E &lcErrMes1 = 'Not allowed to receive M.F.G. order for this date.'
 &lcErrMes2 = ''
 2��
�$ &lcDKind   = 'Transaction date '
 &lcErrMes1 = ''
 &lcErrMes2 = ''
 � B�a�� U  LCTYPE LCDKIND	 LCERRMES1	 LCERRMES2 do,     ��1  1� 14C��B112A�12A�12�2�a2�a2�B�!B�a12�Q12��12�12Q12!�12�Q1� A11A � 2                       �      )   �                       _���    F  F                        �S   %   y      �  <   �          �  U  p 4�  � � � %�� CM� � 
��+ � B� � 5� � � � � T� �CW�� T� �C� CMTRACE��� T� �C� CMVIEWD��� T� �C� CMSITVEW��� %�� 
��� �' Q�  �� �	 � CMTRACE��� CTRANS� � %�� 
���) Q�  �� �	 � CMSITVEW��� CVIEWID� � %�� 
��<�) Q�  �� �	 � CMVIEWD��� FILENAME� � T�  �CC�  f���� T� �� C�� �� T� �C� C� � >��� G(� (� CVIEWID� F� � G((� FILENAME� G-(�� ��� � F� � G((� CTRANS�# %�C�  � CMVIEWD�� C� +
	�� � H� ��� �� � A��B�0 r�� CMTRACE� � � ���  ��� ��� �� 2��� %�C�  � ����� H�j��� �� � D�	 � � A	���� �� � �� � D�	 � � M	���� >� ��� D�� � ���0 r�� CMTRACE� � � ���  ��� ��� �� � � � F� �
 G-�� � %�� 
��+� Q� � � %�� 
��E� Q� � � %�� 
��_� Q� � �
 F�� �� U 
 LCFILENAME	 LCKEYEXPR LCEVENTOCCR OARIAAPPLICATION COMPANYINSTALLEDMODULES LNALIAS	 LLCMTRACE
 LLVIEWUSED
 LLVIEWSITE DATADIR CTRANS CVIEWID FILENAME CMTRACE CKEYEXPR CMSITVEW CMVIEWD	 CFILE_NAM CSTATUS do,     ��1 `� �A A 3� aaq� qA � �A � �A B1qAq q � 3� #� 3� �A A �� A � A A A r � � � A � � A � � A � 2                       �      )   F                       ���    �  �                        ��   %   "      �  9   .          �  U  � 4�  � � � T� ��  �� T� ���� T� ���� T� ����* T� �CC� lcSeptab� C� � � � ,6�� %�CC� �>���x� T� �C� ��\�� T� �C� �=��D T� �CC� �  �� �& C� �  �CC�  �R� � �� � 6� � 6��D T� �CC� �  �� �& C� �  �CC�  �R� � �� � 6� � 6�� T� �� � �� �� T� �� � ��� T�  �C�  � � ��� ���D T� �CC� �  �� �& C� �  �CC�  �R� � �� � 6� � 6�� � H����� �C�
 lnAryOrPosb� U�� � T� ����  �C�
 lnAryOrPosb� C,L���� %�� ���D� � �� ��� �� ��� %�C�� ��� ��|� � �� ��C�� ���� ��� � �� �� � � T� ��  �� � �� ���(�� ���� %�C�
 lnAryOrPosb� N���� T� �� �� � H����� �� ���A�2 T� �C�  �C� �  � C� �  �� C�  >6\�� �� � ����" T� �C�  C� �  � ��\��2 T� �CC� �R� � C� �C� >�\� � 6�� �� �����A T� �C�  C� �  � ��C� �  � C� �  � ��\�� � %�C�
 lnAryOrPosb� N��#�	 B�� �� � %�� ���o�7 T� �C� �� G���C� �� 8���� �� ��� T� �� ��� �� � �� U	  LCSTRING
 LNARYORPOS LCSEPTA LCSUBSTR LNARYDIM	 LNARYROWS	 LNARYCOLS LCCOLSEP LNARELEM do,     ��1 `� � � � � �RQ!DD�aQ� DA � �� 1� q�� � A B � B r�� A � "!"A �� A q� 1A A 2                       $      )   �                       ����    |  |                        ��   %   �      3  0   �          �  U  � 4�  � � � 5� � � � � � T� �CW�� T� �C� EXACTv�� T� �-�� T� �� �� %�C� SYDFIELD�
��� � F�  �& Q��	 �
 � SYDFIELD���	 cFld_Name� T� �a�� �� � F� � T� �C�]�� G((�	 cFld_Name� � %�CC�  f� SYDFIELD����� 5� � T� �C�	 SubString� � �N�� ��C� � � � |~� � �� <� � T� �CC��� >�� �� ���(�C�� ������ T� �C� CC � �� >D�� �� ��� T� ��  �� � %�� ��C� %�CC��� �
��	� � �C�� �������� ��C�� ����� � T� �������� All�� T� ��������  �� � %�� � C� SYDFIELD�	��w� Q�� SYDFIELD�� ��� %�C� �
���� G((�� �� � �
 F�� �� SET EXACT &lcSetExact
	 B�� �� U 
 LCFIELDNAM	 LAARRNAME LLADDALL
 LCCURRFILE LCOLDTAG	 LCSETEXCT
 LLSYDFIELD LNMAXLEN
 LCSETEXACT OARIAAPPLICATION SYSPATH	 CFLD_NAME SYDFIELD
 OSUBSTRING THIS OFORM DO	 MVENTRIES LNCOUNT do,     ��1  � q� A� � �� c� � q � !A �s ��q c��A � � A � s�1A ��A �!� � A A � �� 3                       �	      )   |                       :���    !  !                        o�   %   W      �  �   t          �  U  �  4�  � � � H� �� � �� � CM��: � B�C�  ��� �� � N��W � B�C�  g�� �� � D��t � B�C�  #�� �� � L��� � B�CCC�  �f� .F.� -� a6�� � U 
 LCVALUESTR
 LCDATATYPE LCDIRECTION�	 4�  � �/ 5� � � � � � � �	 �
 � � � 5� � � � � � T� �CW�� T� �-�� T� �-�� T� �-�� T� �-��- T� �CC� lcCompIdb� C�	 � � � � 6�� T� �C� SYCCOMP�
�� %�C� SYCCOMP�
���) Q�  �� � � SYCCOMP��� CCOMP_ID� �/ T� �CC� � SYCCOMP�� C� � �� � � 6�� T� �-�� T� �C�
 lcArray[1]b� C��P T� �C� � C��  ���	�) CC� lcArray[1,2]b� L� � A� � N6� � V6�� %�� 
�	 � ,�  	��-� � ���� ��C �  � � �� �  �C�� ��� ��C�� ��  ��� T� �a�� T� �� V�� � %�C� SETUPS�
��o� F�  � Q�� � SETUPS��� T� �a�� �� F� � T� �C� FULLPATHv�� GQ � T� �C&�� SET FULLPATH &lcSetPath
 %�� � 
��� T� �CC�]g�� T� �CO�� Q�� � SETUPS��� T� �a�� � � T� �C��� G((� VARNAME� T�  ��  �� T� �� �� T�	 �C� ERROR��� 1� llError = .T.� %�� 
��:� %�CCC�  f�
����� H���� ��! � V����( T�  �CC�$ C� C�
 �  � �% �" �# �� ��! � E��� T�  �CC�$ C� C�
 �  ���� � �6� T� �a�� %�C� SYCCONFG�
��q� F�  � Q�� � � SYCCONFG�� T� �a�� ��� F�& � � T� �C��� G((� VARNAME� %�CCC�  f�
����2� H���.� ��! � V����( T�  �CC�$ C� C�
 �  � �% �" �# �� ��! � E��.� T�  �CC�$ C� C�
 �  ���� � � � �=� T� �a�� %�C� SYCCONFG�
���� F�  � Q�� � � SYCCONFG�� T� �a�� ��� F�& � � T� �C��� G((� VARNAME� ��' ���(�C��  ����9� T�
 �-�� T�  ��  �� F� �6 %�CCCC� � V� C �' �  � C �' ��  6f�
������ H�?��� ��! � V��y�( T�  �CC�$ C� C�
 �  � �% �" �# �� ��! � E���� T�  �CC�$ C� C�
 �  ���� � �s� F�& �6 %�CCCC� � V� C �' �  � C �' ��  6f�
����o� H� �k� ��! � V��:�( T�  �CC�$ C� C�
 �  � �% �" �# �� ��! � E��k� T�  �CC�$ C� C�
 �  ���� � � � H���� �� � N����" &lcArray[lnCount,2] = lcRetVal
 �� � V����  &lcArray[lnCount] = lcRetVal
 �� � A��� T�  ��' ������  �� �  T� �� C�
 
� �� � 6�� �� � ON ERROR &lcOnErr
 %�� ���� F�& � %�C� �
���� G((�� �� ��� G((� � %�� ���� Q�& � � � F� � %�C� �
���� G((�� �� ��� G((� � %�� ���� Q� � � %�� � C� �
	��e	� F� � Q�� ��� lnCurTag� %�C� �CN���T	�	 #�� �� �a	� #)� � � %�� ��~	� Q� � �
 F�� �� B�C� 
� �  � � 6�� U(  LCARRAY LCCOMPID	 LNALIASNO	 LLCUSEDBY LLARRAYORVAR
 LLTWODIMEN
 LCSETUPTAG
 LCCONFGTAG
 LNRETCOUNT LCONERR LLERROR LAVARARR	 LLUSESYCC	 LCCOMPDIR	 LCSETPATH
 LLREUSEDBY LNCURTAG LNCURREC LLUSEDBY	 LLSUSEDBY OARIAAPPLICATION ACTIVECOMPANYID	 LLUSECOMP SYSPATH CCOMP_ID SYCCOMP	 CCOM_DDIR DATADIR GFSUBSTR SETUPS LCSETUPSDIR VARNAME LCRETVAL	 CDEFA_TYP THIS TRNSSTR	 MDATA_DEF	 CDATA_TYP SYCCONFG LNCOUNT trnsstr,     �� do     ��1 � � 1� !� !� !�A 3 p� �r� � � � � �rq�A �� ��� !!� � C a� S� � q ra � �1� Q� A B � � � QQ� a� !�!�A � � �� �� � q A � b� !�!�A A A � � �� �� � q A � �� � q a� !�!�A � q a� !�!�A A A � !!!!�A A A Q� q � � a A � � A A r � � a A � � A sq aA� � Q A A � � A � �6                       `        y  ~      )   !                       w2PROCEDURE edigetseq
*!***************************************************************************
*! Name      : EdiGetSeq
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2003
*! Purpose   : Get the next sequence number.
*!***************************************************************************
*! Example   : =gfGetSeq()
*!***************************************************************************
*! Notes     : This function was written by MAN.
*!***************************************************************************
*B606902,1
*FUNCTION gfGetSeq
PARAMETER lnSeq,cChr
PRIVATE lcSPrefix , lnCharASCI , lnI , lcAlias
lcAlias = ALIAS()
SELECT Sequence
lcSPrefix = ""
IF !(cChr = CHR(0))
  lnCharASCI = ASC(cChr)
  IF MOD(lnCharASCI,26) = 0
    lcChar = "Z"
    lnCharPos = lnCharASCI/26
  ELSE
    lnCharPos = INT(lnCharASCI/26)+1
    lcChar =  chr(mod(lnCharASCI,26)+64)
  ENDIF  
  FOR lnI = 1 TO lnCharPos - 1
    lcSPrefix = lcSPrefix + "Z"
  ENDFOR
  lcSPrefix = lcSPrefix + lcChar

*++NAD
ELSE
 lcChar =""
*++NAD
ENDIF
IF lnSeq = VAL(REPLICATE("9",lnRetLen-LEN(lcSPrefix)))
  IF EMPTY(cChr)
    lcSPrefix = "A"
    *REPLACE cSeq_Chr WITH CHR(1)
    lcChrToUpd = CHR(1)
  ELSE
    IF lcChar = "Z"
      lcSPrefix = lcSPrefix + "A"
    ELSE
      lcSPrefix = LEFT(lcSPrefix,LEN(lcSPrefix)-1) + CHR(ASC(lcChar)+1)
    ENDIF       
    *REPLACE cSeq_Chr WITH CHR(ASC(cSeq_Chr)+1)
    lcChrToUpd = CHR(ASC(cSeq_Chr)+1)
  ENDIF  
  lnSeq = 0
ELSE
  lnSeq = lnSeq + 1
ENDIF

*lnSeq = lnSeq + 1
*REPLACE nSeq_No with lnSeq
*? lcSPrefix+PADL(lnSeq,lnRetLen-LEN(lcSPrefix),"0")

lnRetVal = lnSeq
lcExtraStr = lcSPrefix

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of gfGetSeq.

ENDPROC
PROCEDURE do
*B607093,1 Hassan Matching GfSequance with Aria27 GfSequance [Begin]
*!*	*!*************************************************************
*!*	*! Name      : gfSequence                    E:300632
*!*	*! Developer : Wael Aly Mohamed
*!*	*! Date      : 03/04/1997
*!*	*! Purpose   : To get new sequance number for any item
*!*	*!*************************************************************
*!*	*! Calls     :  GFADD_INFO()
*!*	*!              gfRltFld()
*!*	*!*************************************************************
*!*	*! Passed Parameters  : Sequance type
*!*	*!                      Company ID
*!*	*!                      Group ID
*!*	*!                      Division Code
*!*	*!*************************************************************
*!*	*! Returns            : ............
*!*	*!*************************************************************
*!*	*! Example   :  lcData[1] = gfSequence('CINVOICE')
*!*	*!*************************************************************
*!*	*! Modifications
*!*	*! E300888 06/04/98 YMA Added to generate the required code
*!*	*!                      prefixed with a unique 2 characters
*!*	*!                      code that representthe current site
*!*	*!                      in case if the "CM" Communication
*!*	*!                      module is installed.
*!*	*!*************************************************************
*!*	*FUNCTION gfSequence

*!*	*E300894,1 06/18/98 YMA Add an optional parameter to be used to
*!*	*E300894,1              get the sequence propirities if the original
*!*	*E300894,1              requested sequence has no associated field.
*!*	*PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision
*!*	 PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField
*!*	*E300894,1 06/18/98 YMA End.

*!*	*TAK E300973,1 Define global variabels to use with visual.
*!*	*PRIVATE lnRetVal,lcSavAlias,lcDataDir
*!*	*B603586,1 SSH 29/02/00 Add new Private variables .
*!*	*PRIVATE lnRetVal,lcSavAlias,lcDataDir,;
*!*	        gcDataDir,gcComp_Mdl,gcSysHome,gcCurSite,gcAct_Comp,gcOrgPath
*!*	PRIVATE lnRetVal,lcSavAlias,lcDataDir, lnOldGenNm,lcExtraStr,lcToFind,lcKeyExp,;
*!*	        gcDataDir,gcComp_Mdl,gcSysHome,gcCurSite,gcAct_Comp,gcOrgPath
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*B605873,1 FILL 'lcCompanyId' WITH ACTIVE COMPANY Hassan 09/24/2002 [BEGIN]
*!*	lcCompanyId = IIF(TYPE('lcCompanyId')="C" AND !EMPTY(lcCompanyId),lcCompanyId,oAriaApplication.ActiveCompanyID)
*!*	*B605873,1 Hassan 09/24/2002 [END  ]

*!*	gcDataDir  = oAriaApplication.DataDir
*!*	gcComp_Mdl = oAriaApplication.CompanyInstalledModules
*!*	gcSysHome  = oAriaApplication.SysPath
*!*	gcCurSite  = oAriaApplication.CurrentSite
*!*	gcAct_Comp = oAriaApplication.ActiveCompanyId
*!*	gcOrgPath  = oAriaApplication.DefaultPath
*!*	*TAK E300973,1 end.

*!*	*E300894,1 06/18/98 YMA Validate the optional passed parameter.
*!*	lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
*!*	*E300894,1 06/18/98 YMA End.

*!*	lcSavAlias = SELECT(0)
*!*	lcSeqType  = UPPER(lcSeqType)
*!*	lcDataDir = gcDataDir

*!*	*E300888 06/04/98 YMA If the communication module is installed, then
*!*	*E300888              get the unique site prefix for the active site
*!*	*E300888              from the sites file.

*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*!*	*lcUnqPreFx = SPACE(0)
*!*	*IF "CM" $ gcComp_Mdl
*!*	*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*!*	*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*!*	*  USE IN Sites
*!*	*ENDIF
*!*	oGetMemVar = CREATEObject("GetMemVar", This.oForm)
*!*	PRIVATE lcCmpCode
*!*	lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
*!*	lcUnqPreFx = oGetMemVar.DO("M_UNQSTPRX",lcCmpCode)
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*!*	*E300888 06/04/98 YMA End.

*!*	IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
*!*	  IF USED("sycComp")
*!*	    SELECT sycComp
*!*	    luSycComp = .F.
*!*	    ltSycComp = VAL(SYS(21))
*!*	    leSycComp = RECNO()
*!*	    SET ORDER TO TAG cComp_Id IN syccomp
*!*	  ELSE
*!*	    luSycComp = .T.
*!*	    USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
*!*	  ENDIF

*!*	  IF SEEK(lcCompanyId,'syccomp')
*!*	    lcDataDir = ALLTRIM(syccomp.cCom_dDir)
*!*	    IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = ;
*!*	       UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
*!*	       UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  ;
*!*	       UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
*!*	      lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+;
*!*	                 SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
*!*	    ENDIF
*!*	  ENDIF

*!*	  IF luSycComp
*!*	    USE IN syccomp
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSycComp IN syccomp
*!*	    IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
*!*	      GOTO leSycComp IN 'syccomp'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*E301046,4 Assure that lcGroupId is 3 Char. only
*!*	*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
*!*	lcGroupId  = IIF(TYPE('lcGroupId') ='C' , PADR(lcGroupId,3)  , SPACE(2))
*!*	*E301046,4 end
*!*	lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
*!*	lnRetVal   = 0

*!*	*300632,1 Get division sequence group
*!*	*B802982,1 [start] Don't GET the GroupID if the system is not set to
*!*	*                  generate seq.# based on division
*!*	llDivOnSeq = oGetMemVar.Do('M_DIV_SEQ' , lcCompanyId) = 'Y'
*!*	RELEASE oGetMemVar

*!*	*Change this line to check the llDivOnSeq
*!*	*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	*B802982,1 [End]
*!*	  DECLARE laDivDlt[1,2]
*!*	  laDivDlt[1,1] = 'CSEQ_GROUP'
*!*	  laDivDlt[1,2] = 'lcGroupId'
*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  *=gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  PRIVATE oRlatdFields
*!*	  oRlatdFields = CREATEObject("GetRelatedFields", This.oForm)
*!*	  oRlatdFields.Do(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  RELEASE oRlatdFields
*!*	  *TAK E300973,1 End.

*!*	  *E301046,4 Change lcGroupId to be 3 Char. only
*!*	  *lcGroupId = SUBSTR(lcGroupId,1,10)
*!*	  lcGroupId = SUBSTR(lcGroupId,1,3)
*!*	  *E301046,4 end
*!*	ENDIF
*!*	*B802982,1 [start] make sure the group id is empty if the system
*!*	*                  is not set to generate seq.# based on division
*!*	*                  This case will BE FEASABLE ONLY
*!*	*                  IF llDivOnSeq = .F.
*!*	*                  AND !EMPTY(lcGroupId)
*!*	lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
*!*	*B802982,1 [End]
*!*	IF !USED('SEQUENCE')
*!*	  luSequence = .T.
*!*	  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
*!*	ELSE
*!*	  SELECT SEQUENCE
*!*	  luSequence = .F.
*!*	  ltSequence = VAL(SYS(21))
*!*	  leSequence = RECNO()
*!*	  SET ORDER TO TAG Cseq_type IN SEQUENCE
*!*	ENDIF

*!*	IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
*!*	  IF !USED('sydflfld')
*!*	    luSydflfld = .T.
*!*	    USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydflfld
*!*	    luSydflfld = .F.
*!*	    ltSydflfld = VAL(SYS(21))
*!*	    leSydflfld = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydflfld'
*!*	  ENDIF
*!*	  IF !USED('sydfield')
*!*	    luSydfield = .T.
*!*	    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydfield
*!*	    luSydfield = .F.
*!*	    ltSydfield = VAL(SYS(21))
*!*	    leSydfield  = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydfield'
*!*	  ENDIF
*!*
*!*	  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
*!*	  *E300894,1              proprities instead of the sequence field
*!*	  *E300894,1              if any.
*!*	  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
*!*	  = SEEK(PADR(lcPropFld,10),'sydfield')
*!*	  SELECT sydflfld
*!*	  = SEEK(PADR(lcPropFld,10))
*!*	  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
*!*
*!*	  *=SEEK(PADR(lcSeqType,10),'sydfield')
*!*	  *SELECT sydflfld
*!*	  *=SEEK(PADR(lcSeqType,10))
*!*	  *LOCATE REST WHILE cFld_Name=PADR(lcSeqType,10) FOR lEnumerate
*!*	  *E300894,1 06/18/98 YMA End.

*!*	  lnDefSeq = sydflfld.nDef_Seq
*!*	  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
*!*	    SELECT SEQUENCE
*!*	    lnDefSeq = 0
*!*	    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
*!*	      lnDefSeq = MAX(lnDefSeq,nSeq_No)
*!*	    ENDSCAN
*!*	    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
*!*	  ENDIF
*!*	  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
*!*	       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,sydfield.nFld_Wdth)
*!*	  IF sydflfld.lEnumerate
*!*	    IF !USED('sydfiles')
*!*	      luSydfiles = .T.
*!*	      USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
*!*	    ELSE
*!*	      SELECT Sydfiles
*!*	      luSydfiles = .F.
*!*	      ltSydfiles = VAL(SYS(21))
*!*	      leSydfiles = RECNO()
*!*	      SET ORDER TO TAG Cfile_nam IN 'sydfiles'
*!*	    ENDIF
*!*	    =SEEK(sydflfld.cFile_Nam,'sydfiles')
*!*	    SELECT SEQUENCE
*!*	    REPLACE cFile_Nam WITH sydfiles.cFile_Nam ,;
*!*	            cFile_Tag WITH sydfiles.cFile_Tag
*!*	    IF luSydfiles
*!*	      USE IN Sydfiles
*!*	    ELSE
*!*	      SET ORDER TO TAG ltSydfiles IN Sydfiles
*!*	      IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
*!*	        GOTO leSydfiles IN 'Sydfiles'
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydflfld
*!*	    USE IN Sydflfld
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydflfld IN Sydflfld
*!*	    IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
*!*	      GOTO leSydflfld IN 'Sydflfld'
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydfield
*!*	    USE IN Sydfield
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydfield IN Sydfield
*!*	    IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
*!*	      GOTO leSydfield IN 'Sydfield'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF

*!*	*--MAN Added RLOCK Condition[Start]
*!*	   DO WHILE !RLOCK("SEQUENCE")
*!*	   ENDDO
*!*	  lnRetVal   = SEQUENCE.nSeq_No
*!*	*--MAN Added RLOCK Condition[End]

*!*	*E300888 06/04/98 YMA Compute the required code width assuming that
*!*	*E300888              the minemum code field width = 6.
*!*	lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*!*	*E300888 06/04/98 YMA End.
*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	lnOldGenNm = SEQUENCE.nSeq_No
*!*	lcExtraStr = ''
*!*	IF !EMPTY(SEQUENCE.cSeq_Chr)
*!*	  lcExtraStr = SEQUENCE.cSeq_Chr
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	IF !EMPTY(SEQUENCE.cFile_Nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
*!*	  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
*!*	  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
*!*	  IF !USED(lcSeqFile)
*!*	    luSeqFile = .T.
*!*	    *B601946,1 Use the file again to prevent 'File is in use' message
*!*	    *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
*!*	    USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED
*!*	    *B601946,1 end
*!*	  ELSE
*!*	    SELECT (lcSeqFile)
*!*	    luSeqFile = .F.
*!*	    ltSeqFile = VAL(SYS(21))
*!*	    leSeqFile = RECNO()
*!*	    SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
*!*	  ENDIF
*!*	  SELECT (lcSeqFile)
*!*	  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
*!*	  DECLARE laVldEnt[1]

*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  PRIVATE oGetValid
*!*	  oGetValid = CREATEObject("GetValidEntries")
*!*	* IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
*!*	  IF !EMPTY(lcKeyField) .AND. oGetValid.Do(lcKeyField,@laVldEnt) > 0
*!*	    FOR lnCount = 1 TO ALEN(laVldEnt,1)
*!*	      *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	      *E300888              the unique site prefix.
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
*!*	      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (End)
*!*	      *E300888 06/04/98 YMA End.
*!*	        lnRetVal = lnRetVal + 1
*!*	        *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	        IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	          lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	        ENDIF
*!*	        lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                         ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	        *B603586,1 SSH 29/02/00  (End)
*!*	      ENDDO
*!*	    ENDFOR
*!*	  ELSE
*!*	    *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	    *E300888              the unique site prefix.
*!*	    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	    lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                     ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
*!*	    DO WHILE SEEK(lcKeyExp,lcSeqFile)
*!*	    *B603586,1 SSH 29/02/00  (End)
*!*	    *DO WHILE SEEK(PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
*!*	    *E300888 06/04/98 YMA End.
*!*	      lnRetVal = lnRetVal + 1
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	        lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	      ENDIF
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	      *B603586,1 SSH 29/02/00  (End)
*!*	    ENDDO
*!*	  ENDIF
*!*	  RELEASE oGetValid
*!*
*!*	  IF luSeqFile
*!*	    USE IN (lcSeqFile)
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
*!*	    IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
*!*	      GOTO leSeqFile IN (lcSeqFile)
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT SEQUENCE
*!*	*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*!*	*REPLACE nSeq_No WITH lnRetVal+1
*!*	REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*TAK E300973,1 Changed to work under visual.
*!*	*=gfAdd_info('SEQUENCE')
*!*	*oAriaApplication.AddUserInformation('SEQUENCE')


*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	IF  nSeq_No = 0 .AND. lnOldGenNm <> 0
*!*	  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*!*	ENDIF
*!*	IF !EMPTY(lcExtraStr)
*!*	  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	UNLOCK
*!*	*--MAN Added RLOCK Condition[End]
*!*
*!*	*E300888 06/04/98 YMA Never return a numeric code, and return the code
*!*	*E300888              prefixed with the active site unique prefix code
*!*	*E300888              if any.
*!*	*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*!*	*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))
*!*	 lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
*!*	 *E300888 06/04/98 YMA End.

*!*	IF luSequence
*!*	  USE IN Sequence
*!*	ELSE
*!*	  SET ORDER TO TAG ltSequence IN Sequence
*!*	  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
*!*	    GOTO leSequence IN 'Sequence'
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT (lcSavAlias)
*!*	RETURN(lnRetVal)



*!*************************************************************
*! Name      : gfSequence                    E:300632
*! Developer : Wael Aly Mohamed
*! Date      : 03/04/1997
*! Purpose   : To get new sequance number for any item
*!*************************************************************
*! Calls     :  GFADD_INFO()
*!              gfRltFld()
*!*************************************************************
*! Passed Parameters  : Sequance type
*!                      Company ID
*!                      Group ID
*!                      Division Code
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :  lcData[1] = gfSequence('CINVOICE')
*!*************************************************************
*! Modifications
*! E300888 06/04/98 YMA Added to generate the required code
*!                      prefixed with a unique 2 characters
*!                      code that representthe current site
*!                      in case if the "CM" Communication
*!                      module is installed.
*!B802982,1 02/01/2000 HDM Don't GET the GroupID if the system is not set to
*!                     generate seq.# based on division
*!*************************************************************
PARAMETERS lcSeqType  ,;
lcCompanyId,;
lcGroupId  ,;
lcDivision ,;
lcField


PRIVATE lnRetVal  ,;
lcSavAlias,;
lcDataDir ,;
lnOldGenNm,;
lcExtraStr,;
lcToFind  ,;
lcKeyExp  ,;
lcCmpCode

*! Define a variable for cSeq_Chr updating in sequence file. [Begin]
PRIVATE lcChrToUpd
lcChrToUpd = CHR(0)
*! Define a variable for cSeq_Chr updating in sequence file. [End]

lcField     = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
lcSavAlias  = SELECT(0)
lcSeqType   = UPPER(lcSeqType) && What id the Sequence Type &&Look
lcCompanyId = IIF(TYPE('lcCompanyId')="C" AND !EMPTY(lcCompanyId),lcCompanyId,oAriaApplication.ActiveCompanyID)
gcDataDir   = oAriaApplication.DataDir
lcDataDir   = gcDataDir
gcComp_Mdl  = oAriaApplication.CompanyInstalledModules
gcSysHome   = oAriaApplication.SysPath
gcCurSite   = oAriaApplication.CurrentSite
gcAct_Comp  = oAriaApplication.ActiveCompanyID
gcOrgPath   = oAriaApplication.DefaultPath

*B607093,1 Adding active application varible HASSAN 07/14/2003 [BEGIN]
*! Added by Badran, In order to avoid the error 'gcAct_Appl' not exist as we can not find it [Begin].
*PUBLIC gcAct_Appl
*gcAct_Appl = LEFT(oAriaApplication.ActiveModuleID,2)
*! Added by Badran, In order to avoid the error 'gcAct_Appl' not exist as we can not find it [End  ].

gcAct_Appl = LEFT(oAriaApplication.ActiveModuleID,2)

*B607093,1 Adding active application varible HASSAN 07/14/2003 [END  ]

oGetMemVar = CREATEOBJECT("GetMemVar", THIS.oForm)
lcCmpCode  = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
lcUnqPreFx = oGetMemVar.DO("M_UNQSTPRX",lcCmpCode)

IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
  IF USED("sycComp")
    SELECT sycComp
    luSycComp = .F.
    ltSycComp = VAL(SYS(21))
    leSycComp = RECNO()
    SET ORDER TO TAG cComp_Id IN sycComp
  ELSE
    luSycComp = .T.
    USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
  ENDIF

  IF SEEK(lcCompanyId,'syccomp')
    lcDataDir = ALLTRIM(sycComp.cCom_dDir)
    IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = ;
      UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
      UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  ;
      UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
      lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+;
      SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
    ENDIF
  ENDIF

  IF luSycComp
    USE IN sycComp
  ELSE
    SET ORDER TO TAG ltSycComp IN sycComp
    IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
      GOTO leSycComp IN 'syccomp'
    ENDIF
  ENDIF
ENDIF

lcGroupId  = IIF(TYPE('lcGroupId') ='C' , PADR(lcGroupId,3)  , SPACE(2))
lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
lnRetVal   = 0
llDivOnSeq = oGetMemVar.DO('M_DIV_SEQ' , lcCompanyId) = 'Y'
RELEASE oGetMemVar

*Change this line to check the llDivOnSeq
*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
  DECLARE laDivDlt[1,2]
  laDivDlt[1,1] = 'DIVGROUP'
  laDivDlt[1,2] = 'lcGroupId'
  
  *=gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
  PRIVATE oRlatdFields
  oRlatdFields = CREATEOBJECT("GetRelatedFields", THIS.oForm)
  oRlatdFields.DO(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
  RELEASE oRlatdFields
  lcGroupId = SUBSTR(lcGroupId,1,3)
 
ENDIF

lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
IF !USED('SEQUENCE')
  luSequence = .T.
  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
ELSE
  SELECT SEQUENCE
  luSequence = .F.
  ltSequence = VAL(SYS(21))
  leSequence = RECNO()
  SET ORDER TO TAG Cseq_type IN SEQUENCE
ENDIF

IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
  IF !USED('sydflfld')
    luSydflfld = .T.
    USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
  ELSE
    SELECT sydflfld
    luSydflfld = .F.
    ltSydflfld = VAL(SYS(21))
    leSydflfld = RECNO()
    SET ORDER TO TAG Cfld_name IN 'sydflfld'
  ENDIF
  IF !USED('sydfield')
    luSydfield = .T.
    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
  ELSE
    SELECT sydfield
    luSydfield = .F.
    ltSydfield = VAL(SYS(21))
    leSydfield  = RECNO()
    SET ORDER TO TAG Cfld_name IN 'sydfield'
  ENDIF

  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
  = SEEK(PADR(lcPropFld,10),'sydfield')
  SELECT sydflfld
  = SEEK(PADR(lcPropFld,10))
  LOCATE REST WHILE Cfld_name=PADR(lcPropFld,10) FOR lEnumerate

  lnDefSeq = sydflfld.nDef_Seq
  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
    SELECT SEQUENCE
    lnDefSeq = 0
    SCAN REST WHILE Cseq_type+cseq_group = PADR(lcSeqType,10)
      lnDefSeq = MAX(lnDefSeq,nSeq_No)
    ENDSCAN
    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
  ENDIF


  INSERT INTO SEQUENCE (Cseq_type,nSeq_No,cseq_group,cData_Typ,nFld_Wdth,cSeq_Chr) ;
  VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,;
  sydfield.nFld_Wdth,CHR(0))


  IF sydflfld.lEnumerate
    IF !USED('sydfiles')
      luSydfiles = .T.
      USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
    ELSE
      SELECT sydfiles
      luSydfiles = .F.
      ltSydfiles = VAL(SYS(21))
      leSydfiles = RECNO()
      SET ORDER TO TAG Cfile_nam IN 'sydfiles'
    ENDIF
    =SEEK(sydflfld.Cfile_nam,'sydfiles')
    SELECT SEQUENCE
    REPLACE Cfile_nam WITH sydfiles.Cfile_nam ,;
    cFile_Tag WITH sydfiles.cFile_Tag
    IF luSydfiles
      USE IN sydfiles
    ELSE
      SET ORDER TO TAG ltSydfiles IN sydfiles
      IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
        GOTO leSydfiles IN 'Sydfiles'
      ENDIF
    ENDIF
  ENDIF
  IF luSydflfld
    USE IN sydflfld
  ELSE
    SET ORDER TO TAG ltSydflfld IN sydflfld
    IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
      GOTO leSydflfld IN 'Sydflfld'
    ENDIF
  ENDIF
  IF luSydfield
    USE IN sydfield
  ELSE
    SET ORDER TO TAG ltSydfield IN sydfield
    IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
      GOTO leSydfield IN 'Sydfield'
    ENDIF
  ENDIF
ENDIF

*--MAN Added RLOCK Condition[Start]
DO WHILE !RLOCK("SEQUENCE")
ENDDO
lnRetVal   = SEQUENCE.nSeq_No

*! Get the character expression. [Begin]
lcChrToUpd = SEQUENCE.cSeq_Chr
*! Get the character expression. [End]

*--MAN Added RLOCK Condition[End]

lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*! Use the new field (cSeq_Chr) from sequence file.
lnOldGenNm = SEQUENCE.nSeq_No
lcExtraStr = ''
IF !EMPTY(SEQUENCE.cSeq_Chr)

  *Get the Extra string added to the file. [Begin]
  PRIVATE lcChar , lnCharPos , lnI
  IF !(SEQUENCE.cSeq_Chr = CHR(0))
    IF MOD(ASC(SEQUENCE.cSeq_Chr),26) = 0
      lcChar = "Z"
      lnCharPos = ASC(SEQUENCE.cSeq_Chr)/26
    ELSE
      lcChar =  CHR(MOD(ASC(SEQUENCE.cSeq_Chr),26)+64)
      lnCharPos = INT(ASC(SEQUENCE.cSeq_Chr)/26)+1
    ENDIF
    FOR lnI = 1 TO lnCharPos - 1
      lcExtraStr = lcExtraStr + "Z"
    ENDFOR
    lcExtraStr = lcExtraStr + lcChar
  ELSE
    lcChar = ""
  ENDIF
*! Get the Extra string added to the file. [End]
ENDIF

IF !EMPTY(SEQUENCE.Cfile_nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
  lcSeqFile = ALLTRIM(SEQUENCE.Cfile_nam)
  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
  IF !USED(lcSeqFile)
    luSeqFile = .T.
    *! Use the file again to prevent 'File is in use' message
    USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED
  ELSE
    SELECT (lcSeqFile)
    luSeqFile = .F.
    ltSeqFile = VAL(SYS(21))
    leSeqFile = RECNO()
    SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
  ENDIF
  SELECT (lcSeqFile)
  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
  DECLARE laVldEnt[1]
  PRIVATE oGetValid
  oGetValid = CREATEOBJECT("GetValidEntries")
  IF !EMPTY(lcKeyField) .AND. oGetValid.DO(lcKeyField,@laVldEnt) > 0
    FOR lnCount = 1 TO ALEN(laVldEnt,1)
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      *! Check if next sequence number is valid.
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
        *! Check if next sequence number is valid. [Begin]
        =THIS.EDIgetseq(lnRetVal,lcChrToUpd)
        lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      ENDDO
    ENDFOR
  ELSE
    lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
    
    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
    DO WHILE SEEK(lcKeyExp,lcSeqFile)
       *! Check if next sequence number is valid
       =THIS.EDIgetseq(lnRetVal,lcChrToUpd)
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
    ENDDO
  ENDIF
  RELEASE oGetValid

  IF luSeqFile
    USE IN (lcSeqFile)
  ELSE
    SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
    IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
      GOTO leSeqFile IN (lcSeqFile)
    ENDIF
  ENDIF
ENDIF
SELECT SEQUENCE
*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*REPLACE nSeq_No WITH lnRetVal+1
*REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)

*B606902,1 Always check if we used Characters before. [Begin]
*REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*lnRetVal = lnRetVal + 1
*lnRetVal = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
lnOldGenNm = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
=THIS.EDIgetseq(lnRetVal,lcChrToUpd)
REPLACE nSeq_No WITH lnRetVal , cSeq_Chr WITH lcChrToUpd
*B606902,1 Always check if we used Characters before. [End]

*B603586,1 SSH 29/02/00 (End)
*=gfAdd_info('SEQUENCE')
*--MAN Added RLOCK Condition[Start]

*B606902,1 SSE Commented out. [Begin]
*IF  nSeq_No = 0 .AND. lnOldGenNm <> 0
*  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*ENDIF
*IF !EMPTY(lcExtraStr)
*  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
*ENDIF
*B606902,1 SSE Commented out. [End]

*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
IF  nSeq_No = 0 .AND. lnOldGenNm <> 0
  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
ENDIF
IF !EMPTY(lcExtraStr)
  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
ENDIF
*B603586,1 SSH 29/02/00 (End)

UNLOCK
*--MAN Added RLOCK Condition[End]
*E300888 06/04/98 YMA Never return a numeric code, and return the code
*E300888              prefixed with the active site unique prefix code
*E300888              if any.
*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))

*B606902,1 Get the value that will be displayed in message box. [Begin]
*lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
lnRetVal = lcUnqPreFx + PADL(lnOldGenNm, lnRetLen, "0")
*B606902,1 Get the value that will be displayed in message box. [End]

*E300888 06/04/98 YMA End.

IF luSequence
  USE IN SEQUENCE
ELSE
  SET ORDER TO TAG ltSequence IN SEQUENCE
  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
    GOTO leSequence IN 'Sequence'
  ENDIF
ENDIF
SELECT (lcSavAlias)
RETURN(lnRetVal)
*B607093,1 Hassan Matching GfSequance with Aria27 GfSequance [End  ]

ENDPROC
     ����    �  �                        �M   %   �      h  ]   �          �  U  B 4�  � � � � 5� � � � � �	 � ��
 � T�
 �C�	 GetMemVar�N�� %�C�	 LLMULCURR�
 � 
��v �
 B������ �+ %�C�  �� C� �� C� � ABC�� ��� �
 B������ � %�C� ���� � T� �� � �� � T� �CW�� T� �� �� %�C� STYPRICE���f� T�	 �-�� F� � T� �CC�]g�� T� �CC+
� CO� � 6�� T� �C��� G(� G((� STYPRICE� ��� T�	 �a�� F�  �% Q�� � � STYPRICE��� STYPRICE� � T� �CC� ��=�� %�C� �C� � ���X� 5� � � � %�C� STYLE�
��3� T� �a�� F�  � Q�� � � STYLE��� STYLE� F� � ��� T� �-�� F� � T� �CC+� � � CO6�� T� �C��� G((� STYLE� � T�  �C�  C� >��� %�C�  �����! This.Price = Price&lcPriceLvl
 ��� T� � ������ � %�� ���� Q� � �T� F� � G(� (�� �� %�� � ��7� #� �� �� �P�	 #� 6� H� � � � ��� %�C�  � � STYPRICE�����* This.Price = STYPRICE.Price&lcPriceLvl
 ��� T� � ������& %�� 
� C�
 LLSTYPRICE�
 � 	��)� J�� �(� � � �- �� � � SY\STYPRICE���  � � � � ��� �� � � T� �C� GetItemMask�N�� T� �CC� HI� � ��� T� ����@ ��C� TRM00344B00000� DIALOG� �  C�  �� |C� �� � �� � � � %��	 
��%� F� � G((�� �� %�C� �
��� G(�� �� � %�� � ��!�	 #�� �� � �
 F�� �� B�� � �� U  LCSTYLE
 LCPRICELVL LCSTYCUR LLNOADD LCFILTER LNOLDVAL
 LNCURALIAS LNCURTAG LNCURREC
 LLOPENFILE
 OGETMEMVAR DO OARIAAPPLICATION BASECURRENCY STYPRICE DATADIR
 LLCLOSESTY
 LCSTYORDER
 LNSTYRECNO STYLE THIS PRICE LNPRICEA LNPRICEB LNPRICEC
 SCREENHOME	 OITEMMASK
 LCSTYLETIT
 MESSAGEBOX do,     ��1 1�t ��� A �� A � A � � q� q �� a � � � QH C�� R� � �t � � q �� � A B� � A � � � q � � � � A A � ��� fT�� � ��� F A D � q � � A � A A � � 3                       �!      )   �                       ����    y  y                        1�   %   �      0  $   �          �  U  A 4�  � � �4 %�C� lnAmountb� N� C�
 lcCurrCodeb� C��L �	 B��  �� �, T� �CC�
 lnDecimalsb� N� � � � 6�� 5� � � � � T� �CW�� %�C� SYCCURR�
��� � T� �a�� F�  �. USE &gcSysHome.SYCCURR ORDER TAG CCURRCODE
 �+� T� �-�� F� � T� �C��� G((�	 CCURRCODE� � T�  �CC�  �C� >��� %�� � ��y�# T� �C� CCC� Z�>� �� Z�� ��� T� �CC� Z��� � %�C�  �����Y T� �CC� CURRENCYv� LEFT� C�	 �� �  6� CC� CURRENCYv� LEFT� �  � C�	 �6�� � %�� ��� Q� �'� G((�� �� �
 F�� ��	 B�� �� U
 
 LCCURRCODE LNAMOUNT
 LNDECIMALS LNSELECT LCOLDORD
 LLCLOSEFIL LCRETURN SYCCURR	 CCURRCODE	 CCURRSMBL do,     ��1 � � B� A �2� r� � �� � q � !A b2� A � �B � A � � A � � 2                       �      )   y                       #���    
  
                        Re   %   �      �     �          �  U  �  ��  � � 5�  � � � T� �C� DATASESSIONv�� %�C� oFormb� O��[ � G�(�� � �� � T� �C� W��( %�C�
 lcFileNameb� C� C�  �
	��� �
 F��  �� �$ >� ��� � �� ��C$��	 ��C�
 ��
 F�� �� G�(�� �� U 
 LCFILENAME OFORM
 LCSAVALIAS LNOLDDATASESSION DATASESSIONID	 CADD_USER OARIAAPPLICATION USER_ID	 DADD_DATE	 CADD_TIME	 GFGETTIME do,     ��1 � � ��� A � �� A D� � 1                       	      )   
                        PROCEDURE do
*!*************************************************************
*! Name      : gfChkRate
*! Developer : Hesham El-Sheltawi
*! Date      : 10/09/95
*! Purpose   : Return Exchange Rate for Currency in spec. date
*!*************************************************************
*! Parameters: lcExUnit    && hold variable name to return Currency Units
*!             lcCurrency  && Currency to define or return exh. rate for
*!             ldDate      && Date to define or return exch. rate for
*!             llDispMsg   && Display message or not
*!             lcCompID    && company id to use its settings
*!             lcBaseCurr  && The currency that you want to use as default.
*E300309,1     llNoErrMsg  && .T. if the default error message is
*E300309,1                     not to be displayed, .F. otherwise.
*!*************************************************************
*! Call      : gfGetMemVar
*!*************************************************************
*! Returns            : VALUE OF exchage rate
*!*************************************************************
*! Example   : lcVarName=gfChkRate("lcEngUnit",'ENG',DATE(),.T.)
*!             WILL return from the sycexch file the exchange rate
*!             value for the currency "ENG" at the system date 
*!             and its units in variable called lcEngUnit
*!*************************************************************
*E300309,1 RENEE 11/15/95. Add a parameter to control the display
*E300309,1                 of the message that is displayed if a 
*E300309,1                 valid exchange rate is not found.
*E300309,1                 parameter : llNoErrMsg
*
*E300336,1 RENEE 01/08/96. Enhance performance as concerning to speed
*E300336,1                 of execution.
*E300309,1 Add parameter llNoErrMsg  
*PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr
PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr, llNoErrMsg
*E300309,1 end.

LOCAL oGetMemVar

PRIVATE llExUsedBy,lcOldAlias,lcOldTag,lnRetRate,ldCurrDay,lcOldFlt, llCurUsedBy, lnExRate
lnRetRate   = 0
lcCompID    = IIF(TYPE('lcCompId') <> 'C', oAriaApplication.ActiveCompanyID, lcCompID)
lcBaseCurr  = PADR(IIF(TYPE('lcBaseCurr') <> 'C', oAriaApplication.BaseCurrency, lcBaseCurr), 3)
lcOldAlias  = SELECT()
llCurUsedBy = .F.
IF lcCurrency = lcBaseCurr
  IF TYPE('lcExUnit') = 'C'
    &lcExUnit = 1
    RETURN 1.0000    
  ENDIF
ENDIF
IF !USED('SYCCURR')
  llExUsedBy=.T.
  SELECT 0
  USE (oAriaApplication.SysPath + 'SYCCURR') 
ELSE
  SELECT SYCCURR
ENDIF

llExUsedBy=.F.
IF !USED('SYCEXCH')
  llExUsedBy=.T.
  SELECT 0
  USE (oAriaApplication.SysPath + 'SYCEXCH') 
ELSE
  SELECT SYCEXCH
ENDIF

lcOldFlt=FILTER()
lcOldTag=TAG()
*E300336,1 Set index descendingly
*SET ORDER TO TAG CURRENCY
SET ORDER TO TAG CURRENCY DESCENDING
*E300336,1 Get current NEAR setting
lcSetNear = SET('NEAR')
SET NEAR ON 
*E300336,1 end.
SET FILTER TO
IF SEEK(lcBaseCurr+PADR(lcCurrency,3)+DTOS(ldDate))
  lnRetRate= nExRate
ELSE
  STORE .F. TO LLMULCURR,LLEXCHRATE
  STORE 0 TO LNEXRATDAY
  
  oGetMemVar = CREATEOBJECT('GetMemVar')
  lnNoVar    = oGetMemVar.Do('LLMULCURR,LLEXCHRATE,LNEXRATDAY' , lcCompID)
  *E300336,1 Using set near with a descending index places the record
  *E300336,1 pointer on the next best match. Remarked the following,
  *ldCurrDay={}
  *llFound = .F.
  *lnCount = 1
  *DO WHILE !llFound AND lnCount<=lnExratDay
  *  LOCATE FOR CBASECURR+CCURRCODE+DTOS(DRATEDATE) = lcBaseCurr+PADR(lcCurrency,5)+DTOS(ldDate-lnCount)
  *  llFound = FOUND()
  *  lnCount = lnCount + 1
  *ENDDO  
  *IF llFound
  *    lnRetRate= nExRate    
  *E300336,1 Check the validity of the closest matching record
  IF cBaseCurr + cCurrCode = lcBaseCurr + PADR(lcCurrency,3) .AND. dRateDate >= ldDate - lnExRatDay
    lnRetRate = nExRate    
  *E300336,1 end. 
  ELSE
    IF llExchRate AND llDispMsg
      DO FORM SYCHRATE WITH lcBaseCurr,lcCurrency,ldDate TO lnRetRate 
    ELSE
      *E300309,1 Display the default error message only if 
      *E300309,1 llDispMsg is .T. and llNoErrMsg is .F.
      *IF llDispMsg
      IF llDispMsg .AND. !llNoErrMsg
      *E300309,1 end.
        ** Message : "The last defined excahnge rate exceeds     "
        **           "the valid number of days."+CHR(13)+CHR(10)+"
        **           "The currency will be defaulted to the base "
        **           "currency.                                  "
        **           "                       � Ok �              "
 
        =oAriaApplication.MessageBox("TRM00249B00000","DIALOG")
      ENDIF
    ENDIF
  ENDIF  
ENDIF

IF TYPE('lcExUnit') = 'C'
  &lcExUnit = LOOKUP(SYCCURR.NCURRUNIT,lcCurrency,SYCCURR.CCURRCODE,"CCURRCODE")
ENDIF

IF !EMPTY(lcOldTag)
  SET ORDER TO TAG (lcOldTag)
ENDIF
SET FILTER TO &lcOldFlt
IF llExUsedBy
  USE IN SYCEXCH
ENDIF
IF llCurUsedBy
  USE IN SYCCURR
ENDIF

*E300336,1 Restore near settings
SET NEAR &lcSetNear
*E300336,1 end.

SELECT (lcOldAlias)
RETURN lnRetRate 

ENDPROC
     �PROCEDURE changeformula
PARAMETERS lcFormulaName,lcValue
lcFormulaName = UPPER(lcFormulaName)
lnFormulaPos =ASCAN(THIS.laFormulas,lcFormulaName)
lnArrayPos = 0
IF lnFormulaPos>0
  lnArrayPos = ASUBSCRIPT(THIS.laFormulas,lnFormulaPos,1)
ENDIF
IF lnArrayPos = 0
  IF !EMPTY(THIS.laFormulas[1,1])
    DIMENSION THIS.laFormulas[ALEN(THIS.laFormulas,1)+1,ALEN(THIS.laFormulas,2)]
  ENDIF  
  lnArrayPos = ALEN(THIS.laFormulas,1)
ENDIF
THIS.laFormulas[lnArrayPos,1] = lcFormulaName
THIS.laFormulas[lnArrayPos,2] = lcValue

ENDPROC
PROCEDURE reportprint
IF THIS.BeforePrinting()
  *B603122,3 Hesham (Start)
  *B603122,3 Make the user can change the Active Printer
*!*	  lcPrinter = GETPRINTER()
*!*	  IF EMPTY(lcPrinter)
*!*	    RETURN
*!*	  ENDIF
*!*	  SET PRINTER TO NAME (lcPrinter)
  THIS.PrinterSelect()
  *B603122,3 Hesham (End)
  THIS.AssingFormula()
  THIS.Destination = 1
  RETURN THIS.PRINTREPORT()
ENDIF  
ENDPROC
PROCEDURE reportpreview
IF THIS.BeforePrinting()
  THIS.AssingFormula()
  THIS.Destination = 0
  RETURN THIS.PRINTREPORT()
ENDIF  
ENDPROC
PROCEDURE assingformula
LOCAL lnCount
THIS.SelectionFormula=""
IF TYPE('THISFORMSET.ReportFilter')='C'
  THIS.SelectionFormula= THISFORMSET.ReportFilter
ENDIF
IF !EMPTY(THIS.laFormulas[1,1])
  FOR lnCount = 1 TO ALEN(THIS.laFormulas,1)
    THIS.FORMULAS(lnCount-1) = THIS.laFormulas[lnCount,1]+"='"+THIS.laFormulas[lnCount,2]+"'"
  ENDFOR
ENDIF
ENDPROC
PROCEDURE changedatapath
lnCount = 0
lnEnd = This.RetrieveDatafiles()
DO WHILE !EMPTY(This.Datafiles(lnCount)) &&AND lnCount<=lnEnd
  lcFileName = This.Datafiles(lnCount)
  lcFileName = SUBSTR(lcFileName,RAT('\',lcFileName)+1)
  This.Datafiles(lnCount) = IIF(LEFT(UPPER(lcFileName),2)='SY',oAriaApplication.SysPath,;
                            oAriaApplication.DataDir)+lcFileName
  lnCount = lnCount + 1
ENDDO  

ENDPROC
PROCEDURE openfile
LPARAMETERS lcPath ,lcFile ,lcTag
PRIVATE lnInstPos


IF !USED(lcFile)
  USE (lcPath + lcFile) IN 0 ORDER TAG (lcTag) SHARED
ELSE
  SET ORDER TO TAG (lcTag) IN (lcFile)
ENDIF

ENDPROC
PROCEDURE chkform
*!*************************************************************
*! Name      : ChkForm							
*! Developer : Waleed Hamed
*! Date      : 08/11/2002
*! Purpose   : Function to check if a report ID is a form or a 
*! 			   report, and return form name and settings if it is 
*! 			   a form
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters : Form Major, Form name, settings array
*!   				   (passed by reference)	
*!*************************************************************
*! Example	      	 : IF ChkForm(@lcRep_ID, @lcRepForm, @lcFrmSpSets)
*!*************************************************************
PARAMETERS lcRepID, lcFormName, laFrmSets, laSpecFrm , laAllSets
PRIVATE  lnCurAlia,lnCounter, lcFormID, laSettings 

lnCurAlias = SELECT(0)

=THIS.OPENFILE(oAriaApplication.DataDir,"FORMCDDT","FORMCDDT")
=THIS.OPENFILE(oAriaApplication.DataDir,"FORMCDHD","FORMCDHD")
			
*-- If a parameter is sent as a form major name, (passed from the menu)
SELECT FORMCDHD
IF TYPE('lcRepID') = 'C' .AND. SEEK(UPPER(LEFT(lcRepID,6)))
  *-- Get the form name
  lcFormName = UPPER(ALLTRIM(cFormMaj) + ALLTRIM(cCurForm))

  **-- Restore Form Setting Variables
  DECLARE laFrmSets[1,2]
  IF !EMPTY(mFormSets)
    RESTORE FROM MEMO mFormSets ADDITIVE
  ELSE
    laFrmSets = ''
  ENDIF

  DECLARE laSpecFrm[1,5], laAllSets[1,3]
  STORE '' TO laSpecFrm, laAllSets
  lnCounter = 0
    
  SCAN REST WHILE CFORMMAJ = UPPER(LEFT(lcRepID,6))
     
    *-- Restore Form Setting Variables
    DECLARE laSettings[1,2]
    IF !EMPTY(mFormSets)
      RESTORE FROM MEMO mFormSets ADDITIVE
    ELSE
      laSettings = ''
    ENDIF
      
    *-- Re-Dimension laAllSets To include the new form's settings + old Settings
    IF !EMPTY(laSettings[1,1])
      lnLoopStart = IIF(ALEN(laAllSets,1) = 1 AND EMPTY(laAllSets[1,2]) ,;
                                            1 , ALEN(laAllSets,1) + 1)
      PRIVATE lnSetsElm
      lnSetsElm = 0
      FOR lnLoopCnt = lnLoopStart TO lnLoopStart + ALEN(laSettings,1) -1
        lnSetsElm = lnSetsElm + 1
        DIMENSION laAllSets[lnLoopCnt,3]
        laAllSets[lnLoopCnt,1] = UPPER(PADR(cFormID,2))
        laAllSets[lnLoopCnt,2] = laSettings[lnSetsElm,1]
        laAllSets[lnLoopCnt,3] = laSettings[lnSetsElm,2]

      ENDFOR
    ENDIF
    
    *-- Get the form name
    lcFormID = UPPER(PADR(cFormMaj,6)) + UPPER(PADR(cCurForm,2)) + UPPER(PADR(cFormID,2))
    SELECT FORMCDDT
      
    IF SEEK(lcFormID)
      *-- Get the optional program Name and restore fixed settings 
      *-- per form
      lnCounter = lnCounter + 1
      DIMENSION laSpecFrm[lnCounter,5]
      laSpecFrm[lnCounter,1] = FORMCDHD.cFormMaj + FORMCDHD.cCurForm
      laSpecFrm[lnCounter,2] = cFormID
      laSpecFrm[lnCounter,3] = ALLTRIM(cOptProg)
      laSpecFrm[lnCounter,4] = ALLTRIM(mFrmSpSets)
      laSpecFrm[lnCounter,5] = lFRXForm
      *C102834,4 Get the optional program Name Hassan 07/21/2003 [Begin]
      This.OptionProgram =  UPPER(ALLTRIM(FORMCDDT.cOptProg))
      *C102834,4 Get the optional program Name Hassan 07/21/2003 [End  ]      
    ENDIF
    SELECT FORMCDHD
  ENDSCAN
ELSE
  STORE SPACE(0) TO lcFormName
ENDIF  

*-- Close files if opened in this session
IF USED('FORMCDHD')
  USE IN FORMCDHD
ENDIF
IF USED('FORMCDDT')
  USE IN FORMCDDT
ENDIF

*-- Restore current alias
SELECT (lnCurAlias)
RETURN !EMPTY(lcFormName)




ENDPROC
PROCEDURE optprog
*!*************************************************************
*! Name      : OptProg						*C102834,4
*! Developer : HIA
*! Date      : 07/21/2003
*! Purpose   : Function to get if the current form.
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example	      	 : =lfOptProg
*!*************************************************************
*
PARAMETER oObj,cModule
PRIVATE lcProgToDo
*C101389,1 Add variable lcOptProg
IF !EMPTY(THIS.Optionprogram)
  lcProgToDo = ALLTRIM(THIS.Optionprogram)

  DO CASE
  CASE TYPE('cModule')="C" AND !EMPTY(cModule) AND FILE(oAriaApplication.ReportHome+cModule+'\'+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+cModule+'\'+lcProgToDo+'.FXP'

  CASE FILE(oAriaApplication.ReportHome+oAriaApplication.ActiveModuleID+'\'+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+oAriaApplication.ActiveModuleID+'\'+lcProgToDo+'.FXP'

  CASE FILE(oAriaApplication.ReportHome+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+lcProgToDo+'.FXP'

  CASE FILE(oAriaApplication.ReportHome+'EB\'+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+'EB\'+lcProgToDo+'.FXP'

  CASE FILE(oAriaApplication.ReportHome+'UP\'+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+'UP\'+lcProgToDo+'.FXP'
  CASE FILE(oAriaApplication.ReportHome+'NC\'+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+'NC\'+lcProgToDo+'.FXP'
  CASE FILE(oAriaApplication.ReportHome+'AS\'+lcProgToDo+'.FXP')
    lcProgToDo = oAriaApplication.ReportHome+'AS\'+lcProgToDo+'.FXP'

  OTHERWISE
    lcProgToDo = oAriaApplication.ReportHome+LEFT(lcProgToDo,2)+'\'+lcProgToDo

  ENDCASE

  DO (lcProgToDo) WITH oObj
ENDIF

ENDPROC
PROCEDURE Init
DIMENSION laFormulas[1,2]
STORE "" TO laFormulas
ENDPROC
0~   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Class                                                                                               WINDOWS _RFT0VGZZ3 618751780�D      �S  �k      �/      �� O         �k    �6          �a               WINDOWS _RFT0W43KU 561544727�S  �S  �S  �S  �S  �h      b  L                                              WINDOWS _RFT0W43QQ 618751780S  �i  �i  �i  j  j      �j  V                                              COMMENT RESERVED                        �I      	J                                                           WINDOWS _RF30H8DM0 618752043�`      �x  ]^      L;      � �E         n  �  �}          Yh               WINDOWS _RF30HE8QM 560617504�}  v~  �  �~  ct  ��      ��  #�                                              WINDOWS _REZ0M5SMU 559836272�g  �~  .;  �g  |  m�      Ȯ  �                                              WINDOWS _RF30VYNLE 618752043�  �  ju  f/  �{  �}      `�  
�                                              COMMENT RESERVED                        I]      Sm                                                           WINDOWS _RFT0W4SJ7 618752076�D      �4  b�      ˒      ��  k�         F  P  6�          �K               WINDOWS _RFT0W9CQJ 618752077�v  8O  x  ��  �a  ��      ��  +f                                              WINDOWS _RFT0W9CS8 561543336>u  f~  yt  x  �  ̂      �  %0                                               WINDOWS _RFT0W9CTC 561544475~z  �g  r�  �  �  {�      ҁ  3�                                               COMMENT RESERVED                        +�      �x                                                           WINDOWS _RVJ0XUIFG 621566620�  �(  &  �      ��      �{ k         �  �  �%                           WINDOWS _RVJ0XUINZ 617250287�%  �%  �%  �%  {%  �Z      �*  >�                                               WINDOWS _RVJ0XUISR 618220043j%  Z%  M%  �#  4%  �$                                                           WINDOWS _RVJ0XUIV1 617316732�$  i"  �#  B  �#  Y$      ӈ �b                                              WINDOWS _RVJ0XUIY3 617250287p#    T"    U  �#      �  �                                               WINDOWS _RVJ0XUJ18 617250287B  2  #  4  
  {      ��  ^`                                              WINDOWS _RVJ0XUJ5C 621566624b  $    �  �  u      ��  �]                                              WINDOWS _RVJ0XUJ9S 621566624\  L  7  *  �  +      �  :[                                              WINDOWS _RVJ0XUJEE 621566364    �  �  �  1!      �  �X                                              WINDOWS _RVJ0XUJKW 617255559�  �  �  �  w  b
      �	  ɡ                                               COMMENT RESERVED                        �      [                                                           WINDOWS _RG40TUKZM 622356044s  �1  �1  �'      �4      1  ��          ]  j  �/          O               WINDOWS _RG40U49AP 618751587-  �,  �,  �,  �,  c,                                                           WINDOWS _RG40U49C4 622356044J,  [*  F*  �(  {(  �+      �'  y                                               COMMENT RESERVED                        ;      
                                                           WINDOWS _RFM0WPCC4 640002222�      R-  �v      �u      w �U         �]  �+  S          �X               WINDOWS _RFM0UV7IY 640002222o]  (O  O  �I  �I  )                                                           WINDOWS _REZ0UV7I1 623744616�I  �D  �I  �<  pI  &I      �$ �Q                                              WINDOWS _RFM0UV7IZ 561530123�<  y}  g  �  7V  =s      W  _                                               WINDOWS _RFT0IL7ZX 623744616�;  3M  Oq  �~  l        wi 'O                                              COMMENT RESERVED                        �n      �-                                                           WINDOWS _RFJ0U1801 640056724m      &V  #|      �z      �� "�         >|    p�          �z               WINDOWS _REZ0UV7I1 561277204]  \}  �h  �n  �g  �2      �t 6L                                              WINDOWS _RFN0UD7Q4 561202289�}  =}  -}  ~`  �R  fW      �  ��                                               WINDOWS _RFM0UV7IM 561141062�R  ]  �;  Q  �;  sR      CD  y"                                               WINDOWS _RFM0UV7IN 561281930~D  �;  �H  �H  �H  s^      �r ޴                                               WINDOWS _REZ0UV7I1 640004305I  �  �+  Xx  �  #�      � �I                                              WINDOWS _RFM0UV7IP 573853518�C  &-  M}  �C  �a  Q      � =4                                              WINDOWS _RFM0UV7IX 640056725h  �C  �  �  lx  �t      Ќ �/                                              WINDOWS _REZ0UV7I1 618751842k`  �  �+  .  �C  �o      �  �,                                              WINDOWS _RFO0H3KIW 618751842NL  �V  �\  l}  �\  �_      ��  ��                                               WINDOWS _RFO0UOS7Y 561214048�\  ]  m  op  Tp  L                                                           COMMENT RESERVED                        �      �U                                                           WINDOWS _REZ0UOI9V 655915270Ih      %l  �h      �G      6G  �v          <h  �  �            �O           COMMENT RESERVED                        �`      �k                                                           WINDOWS _RFG1ADLDO 655915276uh      �z  �m      ��      � ��          �I  �   ɏ          gh               COMMENT RESERVED                        �`      
                                                           WINDOWS _REZ0UV7I1 655915280�      �  �      MV        �n          �  �  �          �               COMMENT RESERVED                        �      �                                                           WINDOWS _REZ0V06EE 655915286�x      �x  �~      ,     (X  �[          �D  (!  �v          �v               COMMENT RESERVED                        a      "m                                                           WINDOWS _REZ0XMLEX 655915290/      v+  @      �      U  �X          "  �                             COMMENT RESERVED                        �                                                                   WINDOWS _REZ0V3IW3 655915323�n      �C  Wu      �{      4�  �          rg  �  �`          �t               COMMENT RESERVED                        ��      "B                                                           WINDOWS _REZ0V8AFA 655915331O      �	  ;�      �.      ;  �O          �s  �+  �z          �~               COMMENT RESERVED                        �~      �g                                                           WINDOWS _RXW0MRYY4 655915335�x      �x  �}      �      �  CM          Kx  �*              =x               COMMENT RESERVED                        ,x                                                                   WINDOWS _REZ0V9HIK 655915355�u      �v  �u      �:      �  :J          �u  �              Aq               COMMENT RESERVED                        0q                                                                   WINDOWS _RFC0Y8JWP 655915362�      �3   q      yu      �|  *F          �`  V              �l               COMMENT RESERVED                        �l                                                                   WINDOWS _REZ0VBI9K 6559153669^      -h  �a      ��      ��  ۲          -]  1  �m          RZ               COMMENT RESERVED                        �a      �]                                                           WINDOWS _REZ0VEIC3 655915372>Z      �]  �]      ��      K|  �=          1Z  !  �]          �Y               COMMENT RESERVED                        Z      �Y                                                           WINDOWS _REZ0VGHNY 655915378MR      `R  �Y      - 5W  �g �(         �O  %:  �p          %M               COMMENT RESERVED                        �O      �N                                                           WINDOWS _REZ0VII1T 655915385�H      �N  �N      ��      �Q  �8          �H  �+              �E               COMMENT RESERVED                        �E                                                                   WINDOWS _RFA0TSUUZ 6559153966D      �E  �E      �L      ON  5          �=  �2              �.               COMMENT RESERVED                        �3                                                                   WINDOWS _RVW0YPB6Q 655915401�2      �2  6      aL      0H  �1          6-    �E          �+               COMMENT RESERVED                        *      D                                                           WINDOWS _RUZ13OBU6 655915409�      �  �      -'      .  !&          �  {              �               COMMENT RESERVED                        �                                                                   WINDOWS _RW30YQ2V5 655915429�      \  M      �-      O# -�          �  Y              �               COMMENT RESERVED                        �      {                                                           WINDOWS _RFW0GVV52 655915435/  �    �            k *         =  �              �               COMMENT RESERVED                        �      �                                                           WINDOWS _S140ULZPO 655915440�  �    �      �      �� $�          �  �	              �	               COMMENT RESERVED                        �      c                                                           WINDOWS _REZ0M5SMU 655915443Q      S  a      �      d �         ;  H  v          -               COMMENT RESERVED                              �                                                           WINDOWS _REZ0VN9D2 719025817�       !  �#      E      $�  s�          �   �  �>          �                COMMENT RESERVED                        Z       ^-                                                           WINDOWS _RFW0INQHF 775186747�      �  �      �<      _ }�          �  �  �;          �               WINDOWS _RFW0J9X3H 775186747�9  )      �  �9      ��  {�                                               COMMENT RESERVED                        j                                                                 WINDOWS _RFU0WPKRQ 783441673>;      �a  V      4z      U| P�          �  �2  ��  �a  �  y               COMMENT RESERVED                        g                                                                   WINDOWS _REV0HOIBA 858101292�t      �l  K�      ��      g$          L  �  :�         �t               COMMENT RESERVED                        �~                                                                   WINDOWS _RFB0W2ZP6 858101408l<      �B  �<      �s      �� �         �0  9  y                          WINDOWS _RFB0ZY1OV 606512368�B  �B  �=  �<  �<  cB                                          �=               COMMENT RESERVED                        :      ;<                                                           WINDOWS _RES0JX9WT 861893363      �D  �_      3�     } ��          �_  �  ��          �_               COMMENT RESERVED                        �_      ~_                                                           WINDOWS _REX0MDBDY 875728078�      xa  �      jx     �`          �  �  B         �               WINDOWS _REZ0MJMG8 875728078ca  Sa  Ca  5a  a  �c     �� L�                                               WINDOWS _REZ0NNRZO 875728078H^  �]  _]  :]  ^  �     +� dq                                               WINDOWS _RF50I9QST 875728078�Z      �Z  �U  uZ  �h                                                          WINDOWS _REZ0NNS1U 875728078`Z  �Y  oS  Q  US  �"     �� �e                                               WINDOWS _REZ0NNS45 875728078@S  �P  �P  �K  �P  )"     Vq T                                               WINDOWS _REX0QYDH2 875728078�K      I  H  F  HO                                                           WINDOWS _REZ0UV7I1 875728078�E  �E  �D  �C  �D  �     � (�                                               WINDOWS _RTF0U05RB 875728078�C  pC  SB  _C  B  c     9�  �(                                               WINDOWS _REZ0NNS9L 875728078�A  �>  �>  {<  �:  �      �q O                                               WINDOWS _RTF10TBIB 875728078t6      v4  �3  w:  .:                                                           WINDOWS _REZ0NNS7E 875728078
1  z/  �.  C-  �/  �      �_ ��                                               WINDOWS _REX0QYDKG 875728078-      �+  �+  �*  +6                                                           WINDOWS _REZ0NNSG4 875728078�*  {*  k*  �)  ,*  ��      �Z )?                                               WINDOWS _REZ0UV7I1 875728078*  �'  �'  �'  r'  5l      r�  7                                               WINDOWS _REX0QYDP1 875728078a'      �#  �   V#  �                                                            WINDOWS _REZ0UV7I1 875728078m   k  
  �  �  FK      m  �                                               WINDOWS _RTF0U065G 875728078�  �  @  �  �  �B      ^  �                                               WINDOWS _RTF0U067E 875728078�  F  6  m    dA      J  �                                               WINDOWS _RTF0U0699 875728078�	      �	  =  #  �                                                           WINDOWS _REZ0NNSR7 875728078   �  �  �  �  �3      �y #                                               COMMENT RESERVED                        k      B                                                           �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      JArial, 0, 9, 5, 15, 12, 32, 3, 0
MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariaincrementalsearch      Pixels      Class      2      form      ariaincrementalsearch      form      1      ariaincrementalsearch      TxtSeekValue      textbox      main.vcx      2      !Arial, 0, 9, 5, 13, 11, 12, 2, 0
      ariacontroltoolbar      Pixels      Class      21      toolbar      cmdexit      ariasearchcontainer      line      !Arial, 0, 9, 5, 15, 12, 27, 3, 0
      toolbarbutton      Pixels      Class      1      checkbox      toolbarbutton      mouseinfocus
controlenable
      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariacommandbutton      1      ]Height = 22
Width = 22
Caption = ""
Style = 1
controlenable = 1
Name = "toolbarbutton"
      checkbox      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      multiselectioncolumn      Pixels      Class      commandbutton      ariacommandbutton      ariacontrolsource
btntype
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      commandbutton      ariasearchcontainer      ����    �   �                         ��   %   d       {      s           �  U    T�  � �� �� t,� � U  THIS VALUE
 CALCULATOR Click,     ��1 � 1                       9       )   �                         1      ariacontroltoolbar      ariacontroltoolbar      checkbox      main.vcx      toolbarbutton      Pixels      ariacontroltoolbar      
Separator5      grid      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      addbook      Class      10      ariaformset      addbook      ����    �   �                         ��   %   P       e      _           �  U    T�  � �� �� U  THIS VALUE Click,     ��1 1                              )   �                         	separator      grid      1      	separator      Pixels      rPROCEDURE Valid
IF THISFORM.CMDTO.DEFAULT
  THISFORM.CMDTO.CLICK
ELSE
  THISFORM.CMDCC.CLICK
ENDIF
ENDPROC
     BoundColumn = 2
ColumnCount = 2
ColumnWidths = "150,0"
RowSourceType = 6
RowSource = "mtcontct.cbokcntnam,cbookcont"
FirstElement = 1
Height = 218
ColumnLines = .F.
Left = 11
MultiSelect = .T.
NumberOfElements = 0
Top = 53
Width = 180
Name = "lsContacts"
      addbook.Ariaform1      
lsContacts      listbox      main.vcx      arialistbox      addbook.Ariaform1      LSTO      listbox      main.vcx      arialistbox      RTop = 176
Left = 208
Height = 27
Width = 59
Caption = "Cc>>"
Name = "cmdCc"
      addbook.Ariaform1      ����    �   �                         ��   %   P       e      _           �  U    T�  � �� �� U  THIS VALUE Click,     ��1 1                              )   �                         cmdCalc      ariasearchgrid      ariatextbox      main.vcx      Class      ariacheckbox      multiselectioncolumn      Name = "multiselectioncolumn"
      checkbox     fTop = 3
Left = 3
Name = "AriaSearchContainer1"
cbxFrom.Name = "cbxFrom"
cbxTo.Name = "cbxTo"
chkIs.Name = "chkIs"
cbxExprOperator.Name = "cbxExprOperator"
cbxExprRight.Name = "cbxExprRight"
LstExpression.Name = "LstExpression"
cbxExprLeft.Name = "cbxExprLeft"
txtFrom.Name = "txtFrom"
txtTo.Name = "txtTo"
FilterShortCut.Name = "FilterShortCut"
      main.vcx      
gridcolumn      Pixels      form      ATop = 3
Left = 417
Height = 0
Width = 0
Name = "Separator5"
      ariacontroltoolbar      checkbox      main.vcx      1      ����    �   �                         ��   %   P       e      _           �  U   
 ��  � � U  THISFORM RELEASE Click,     ��1 � 1                       !       )   �                         txtFrom      Class      DPROCEDURE Click
THIS.VALUE=0
ACTIVATE WINDOW CALCULATOR
ENDPROC
      toolbarbutton      ariacontroltoolbar      cmdCalender      1      1      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      textbox      cmdCc      commandbutton      main.vcx      ariacommandbutton      `Top = 82
Left = 208
Height = 27
Width = 59
Caption = "To>>"
Default = .F.
Name = "cmdTo"
      addbook.Ariaform1      cmdTo      commandbutton      form      main.vcx      LSCC      checkbox      4      1      ariacommandbutton      �RowSourceType = 5
RowSource = "THISFORMSET.laCc"
Height = 105
Left = 278
MultiSelect = .T.
Top = 163
Width = 202
Name = "LSCC"
      addbook.Ariaform1      listbox      main.vcx      arialistbox      addbook.Ariaform1      Class      1     9PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
IF nKeyCode = 7
  lnDeleted = 0
  FOR lnCnt = 1 TO THIS.ListCount
	    IF THIS.Selected(lnCnt)  && Is item selected?
          lnDeleted = lnDeleted + 1
          =ADEL(THISFORMSET.laTo,lnCnt)
		ENDIF
   ENDFOR
   IF ALEN(THISFORMSET.laTo,1)-lnDeleted < 1
     DIMEN THISFORMSET.laTo[1,ALEN(THISFORMSET.laTo,2)]
     STORE '' TO THISFORMSET.laTo
   ELSE  
     DIMEN THISFORMSET.laTo[ALEN(THISFORMSET.laTo,1)-lnDeleted,ALEN(THISFORMSET.laTo,2)]   
   ENDIF
   THIS.REQUERY()      
ENDIF
ENDPROC
      4      1      /PROCEDURE Click
thisformset.release
ENDPROC
      	cmdCancel      main.vcx      ariatextbox      cmdOk      ����    �   �                         ƀ   %          �      �           �  U  &  T�  � �� �� ��C�  � � � �� U  THIS VALUE PARENT OWINDPARENT PRINT Click,     ��1 1�1                       w      )   �                         chkIs      (PROCEDURE Click
THIS.VALUE=0
ENDPROC
      main.vcx      toolbarbutton      
gridcolumn      main.vcx      coltext      Pixels      Class      ariacontainer      Pixels      Class      	container      ariacontainer      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.Parent.Error(nError, cMethod, nLine)
ENDPROC
      3Width = 200
Height = 200
Name = "ariacontainer"
      11      JTop = 204
Left = 228
Caption = "Clear Filter"
Name = "cmdClearFilter"
      main.vcx      (PROCEDURE Click
THIS.VALUE=0
ENDPROC
      ariacontroltoolbar      cmdTaskList      1      1      textbox      Class      1      1      checkbox      3     <���    #  #                        ߸   %   �      �     �          �  U  �  ��  � T� � �� �� %�� � � � AE��� � ��C �  � � � � �� T� � � � ���� �a�� ��C� � �	 �� ��C� � �
 �� T� � � � ���� �-�� �� � ��C� � � � �� T� � � �� � �� � U  TLSAVEWITHOUTASK THIS VALUE PARENT
 ACTIVEMODE OWINDPARENT UNDO FORMS
 LOCKSCREEN BUTTONREFRESH
 NAVREFRESH CLOSE VISIBLE OARIAAPPLICATION USERUSETOOLBAR Click,     ��1 q �q��� 1aA 2                       �      )   #                        
ariabrowse      Pixels      ..\icons\brow2.bmp      Class      main.vcx      ariacontrolsource
      cmdClose      cbxExprLeft      combobox      9BorderStyle = 0
SpecialEffect = 1
Name = "gridcolumn"
      coltext      textbox      main.vcx      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariatoolbar      Pixels      toolbar      ariatoolbar      toolbar      grid      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ariakeyfield      Pixels      Class      3      ariacontainer      ����    �   �                         �   %   S       h      b           �  U   
 ��  � � U  THISFORMSET RELEASE Click,     ��1 � 1                       $       )   �                         ariatextbox      toolbarbutton      1      Pixels      Class      textbox      ATop = 3
Left = 346
Height = 0
Width = 0
Name = "Separator3"
      
Separator3      1      textbox      1     BoundColumn = 1
ColumnCount = (ALEN(THISFORMSET.laTo,2))
ColumnWidths = "150,0,0"
RowSourceType = 5
RowSource = "THISFORMSET.laTo"
Height = 101
ColumnLines = .F.
Left = 279
MultiSelect = .T.
NumberOfElements = (ALEN(THISFORMSET.laTo))
Top = 55
Width = 202
Name = "LSTO"
      commandbutton      main.vcx      ����    �   �                         ��   %   ^       s      m           �  U    ��C��  � � �� U  THIS PARENT BUILDBAR Click,     ��1 11                       (       )   �                         ariacontroltoolbar      ariacommandbutton      addbook.Ariaform1      commandbutton      
Arialabel1      	separator      ariatextbox      dTop = 288
Left = 276
Cancel = .T.
Caption = "\<Cancel"
TerminateRead = .F.
Name = "cmdCancel"
      NTop = 288
Left = 108
Caption = "\<Ok"
TerminateRead = .F.
Name = "cmdOk"
      ariacommandbutton      dCaption = "Show Contacts From"
Height = 15
Left = 10
Top = 15
Width = 100
Name = "Arialabel1"
      addbook.Ariaform1      label      main.vcx      	arialabel      addbook.Ariaform1      Ariacombobox1      combobox      main.vcx      ariacombobox      -uretval
oto
occ
^lato[1,2] 
^lacc[1,2] 
      formset     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         ,Caption = "Toolbar1"
Name = "ariatoolbar"
      	separator      ariacontroltoolbar      	cmddelete      checkbox      main.vcx      ariakeyfield      �PROCEDURE When
DoDefault()
IF This.Parent.ActivateBrowseButton
  This.Parent.ActivateBrowseButton = .F.
  RETURN .T.
ELSE
  RETURN MDOWN()
ENDIF
ENDPROC
      ariakeyfield      KeyCmd      main.vcx     @���    '  '                        4f   %   �       �      �           �  U  J  T�  � �� �� ��C� S�  � � � �� ��C�  � � �� ��C�  � � �� U  THIS VALUE PARENT OWINDPARENT
 CHANGEMODE
 NAVREFRESH BUTTONREFRESH Click,     ��1 q2                              )   '                        cmdedit      ariaspinner      toolbarbutton      ariacontroltoolbar      commandbutton      main.vcx      checkbox      main.vcx      toolbarbutton      ariacontroltoolbar      1      �PROCEDURE Valid
SELECT MTCONTCT
lcFilter = '"'+PADR(THIS.VALUE,LEN(CBOOKCOMP))+'"'
SET FILTER TO CBOOKCOMP = &lcFilter
GO TOP
THISFORM.lsContacts.Requery()
ENDPROC
      	container      combobox      textbox      
Separator2      	separator      1      1      5      Pixels      PTop = 1
Left = 104
Height = 21
Width = 16
Caption = "..."
Name = "KeyCmd"
      ariacommandbutton      RFontName = "MS Sans Serif"
FontSize = 8
Left = 1
Top = 1
Name = "Keytextbox"
      ariakeyfield      
Keytextbox      textbox      main.vcx      ariatextbox      	separator      main.vcx      Class      cmdfind      form      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      VFontName = "MS Sans Serif"
FontSize = 8
Height = 23
Width = 100
Name = "coltext"
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.Parent.Error(nError, cMethod, nLine)
ENDPROC
      checkbox      Pixels      �FontName = "MS Sans Serif"
FontSize = 8
Height = 72
RowHeight = 16
Width = 217
lastactiverow = (this.ActiveRow)
lastactivecolumn = (this.ActiveColumn)
Name = "ariagrid"
      Ariatextbox1      main.vcx      3Width = 472
Height = 64
Name = "ariaquiksearch"
      ariacontroltoolbar      >selectedfrombrowse
activatebrowsebutton
*sharedvalidation 
      ����    �   �                         ��   %   P       e      _           �  U   
 ��  � � U  THISFORM RELEASE Click,     ��1 � 1                       !       )   �                         Class      1      toolbarbutton      kPROCEDURE Init
DoDefault()
This.KeyCmd.Left = This.Keytextbox.Left + This.Keytextbox.Width + 5
ENDPROC
      	container      main.vcx     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         spinner      1      1      spinner      �BoundColumn = 2
ColumnCount = 2
ColumnWidths = "200,0"
RowSourceType = 1
Height = 24
ColumnLines = .F.
Left = 357
Style = 2
Top = 144
Visible = .F.
Width = 88
Name = "cbxFrom"
      line      
Separator4      	ariashape      �Top = 3
Left = 417
Height = 22
Width = 22
Picture = ..\bmps\close.bmp
Alignment = 0
ToolTipText = "Close the form"
Name = "cmdexit"
      	separator      form      qWidth = 120
Height = 23
BorderWidth = 0
SpecialEffect = 2
activatebrowsebutton = .F.
Name = "ariakeyfield"
     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         ariaspinner      ATop = 3
Left = 296
Height = 0
Width = 0
Name = "Separator2"
      	separator      �alias Specifies the alias used for each table or view associated with a Cursor object.
*translatetoenglish 
*dbgetprop 
^filetags[1,1] 
     g���    N  N                        �   %   �            �           �  U  e  T�  � �� �� %�C�  � � � ��^ � ��C� S�  � � � �� ��C�  � � �� ��C�  � � �� � U  THIS VALUE PARENT OWINDPARENT DELETE
 CHANGEMODE BUTTONREFRESH
 NAVREFRESH Click,     ��1 qqA 1                       �       )   N                       ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         cFontName = "MS Sans Serif"
FontSize = 8
Format = "!"
Left = 0
Top = 12
Name = "TxtSeekValue"
      ariatextbox      ariaformset      1      ATop = 3
Left = 267
Height = 0
Width = 0
Name = "Separator4"
      ariacontroltoolbar      ariacontroltoolbar      {AutoSize = .T.
FontName = "MS Sans Serif"
FontSize = 8
Caption = "Label1"
Height = 15
Width = 34
Name = "arialabel"
      combobox      custom      OWidth = 505
Height = 174
BorderColor = 192,192,192
Name = "ariasearchgrid"
      ariacommandbutton      ariasearchcontainer      checkbox      main.vcx      <iskey
field
gridtorefresh
formstyle
filename
lcfilter
      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      formset      cmdprint      ariaformset      ariaformset      	Ariaform1      ariacommandbutton      	cmdFilter      �Height = 49
Width = 394
DoCreate = .T.
AutoCenter = .T.
BorderStyle = 2
Caption = "Incremental Search"
ControlBox = .F.
MinButton = .F.
Name = "ariaincrementalsearch"
      Pixels      form      Class     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         checkbox      main.vcx      *textpicture
oldvalue
ariacontrolsource
     3���                              L   %   �      �     �          �  U  �  ��  � T� � �� �� ��C� � � � �� T� � � � �� � � ��  �� ���(�� � � � ��� � T� � � �	 �� ��
 �a�� �� ��C� � � �� ��C� � � ��  �� ���(�� � � � ��� � T� � � �	 �� ��
 �-�� �� U  TLSAVEWITHOUTASK THIS VALUE PARENT OWINDPARENT EDIT
 ACTIVEMODE LNFRMCNT	 FORMCOUNT FORMS
 LOCKSCREEN BUTTONREFRESH
 NAVREFRESH Click,     ��1 q 5��A �A 2                       �      )                           �Top = 3
Left = 388
Height = 22
Width = 22
Picture = ..\bmps\calc.bmp
Alignment = 0
ToolTipText = "Calculator"
Name = "cmdCalc"
      toolbarbutton      ariacontroltoolbar      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      checkbox      2DoCreate = .T.
TabIndex = 1
Name = "Ariaform1"
      main.vcx      ariaform      formset      �Top = 3
Left = 367
Height = 22
Width = 22
Picture = ..\bmps\calend.bmp
Alignment = 0
ToolTipText = "Calender"
Name = "cmdCalender"
      	cmdselect      main.vcx      toolbarbutton      cmdadd      LstExpression      editbox      arialistbox      main.vcx      ariasearchcontainer      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      shape      3PROCEDURE Click
this.parent.BuildBar(2)
ENDPROC
      ariacombobox      main.vcx      ariacontroltoolbar      form      Class      form      	container      checkbox      xFontName = "MS Sans Serif"
FontSize = 8
Width = 100
textpicture = C
ariacontrolsource = ('')
Name = "ariatextbox"
      ariacontrolsource
      	ariashape      shape      ariapageframe      Pixels      main.vcx      toolbarbutton      ariacontroltoolbar      Class     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      oFontName = "MS Sans Serif"
FontSize = 8
Caption = "Check1"
ariacontrolsource = ('')
Name = "ariacheckbox"
      
Separator1      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      Class      	pageframe      combobox      cbxExprOperator      ariasearchcontainer      ariacombobox      	separator      BTop = 204
Left = 28
Caption = "Set Filter"
Name = "cmdFilter"
      ariasearchform      commandbutton      ariacommandbutton      ariasearchform      AriaSearchContainer1      Class      ariaquiksearch      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         �Top = 3
Left = 346
Height = 22
Width = 22
Picture = ..\bmps\tasklst.bmp
Alignment = 0
ToolTipText = "User Task List"
Name = "cmdTaskList"
      	separator      cmdend      Pixels      Class      ,Top = 0
Left = 0
Name = "FilterShortCut"
      ariatextbox      tFontName = "MS Sans Serif"
FontSize = 8
Height = 24
Width = 120
ariacontrolsource = ('')
Name = "ariaspinner"
      @Height = 68
Width = 68
SpecialEffect = 0
Name = "ariashape"
      Pixels      main.vcx     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      ariapageframe      	pageframe      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      	container      main.vcx      main.vcx      ATop = 3
Left = 196
Height = 0
Width = 0
Name = "Separator1"
      ariaoptiongroup      Class      	Check Box     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         ariacontroltoolbar      checkbox      main.vcx      cmdnext      �ColumnCount = 4
ColumnWidths = "180,50,100"
RowSourceType = 1
Height = 134
ColumnLines = .F.
Left = 1
MoverBars = .T.
TabIndex = 1
Top = 1
Width = 539
Name = "LstExpression"
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      optiongroup      optiongroup      bTop = 146
Left = 196
Height = 21
Width = 50
Caption = "Is not"
TabIndex = 3
Name = "chkIs"
      ariacheckbox      ariasearchcontainer      ,filterarray
filefilter
key
filetofilter
      toolbarbutton      ariacontroltoolbar      checkbox      ariatextbox      ariaquiksearch      Ariacombobox1      combobox      main.vcx      ariacombobox      	container     ����    �  �                        �[   %   E      j     T          �  U  �  T�  � �� ��  �� ���(��  � � � ��M � T�  � � � �� �� �a�� �� ��C�  � � � �� ��C�  � �	 ��  �� ���(��  � � � ��� � T�  � � � �� �� �-�� �� U
  THIS VALUE LNFRMCNT PARENT OWINDPARENT	 FORMCOUNT FORMS
 LOCKSCREEN GOEND
 NAVREFRESH Click,     ��1 �A 2�A 2                       l      )   �                        lcKey      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      
ariabrowse      	container      ariasearchform      �Height = 23
Width = 84
FontName = "MS Sans Serif"
FontSize = 8
Caption = "Command1"
ariacontrolsource = ('')
btntype = O
Name = "ariacommandbutton"
      main.vcx      ,PROCEDURE Click
THISFORM.RELEASE
ENDPROC
      )ariacontrolsource^
transportvalueslen^
      �BoundColumn = 2
ColumnCount = 2
ColumnWidths = "200,0"
RowSourceType = 1
Height = 24
ColumnLines = .F.
Left = 446
Style = 2
Top = 144
Visible = .F.
Width = 91
Name = "cbxTo"
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      Pixels     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         Pixels      main.vcx      ariaoptiongroup      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariaoptionbutton      Class      optionbutton      Pixels      toolbarbutton      ariacontroltoolbar      textbox      textbox     BoundColumn = 2
ColumnCount = 2
ColumnWidths = "150,0"
RowSourceType = 6
RowSource = "mtadbook.ccom_name,cbookcomp"
FirstElement = 1
Height = 24
ColumnLines = .F.
Left = 144
NumberOfElements = 0
Style = 2
Top = 12
Width = 168
Name = "Ariacombobox1"
     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         textbox      ariasearchcontainer      ariashortcut      utility.vcx      ariacombobox      Class      cmdprev      ariasearchgrid      checkbox      ariasearchcontainer      Class      main.vcx      ariaoptionbutton      ariacontrolsource
      optionbutton      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariacontroltoolbar      listbox      toolbarbutton      ariasearchgrid     ColumnCount = 2
ColumnWidths = "100,0"
RowSourceType = 9
RowSource = "this.parent.OperatorPopup"
FirstElement = 1
Height = 24
ColumnLines = .F.
Left = 255
NumberOfElements = 0
Style = 2
TabIndex = 4
Top = 144
Width = 95
Name = "cbxExprOperator"
      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ariaform      Pixels      Class      ariaform      yFontName = "MS Sans Serif"
FontSize = 8
Height = 24
Left = 449
Top = 144
Visible = .F.
Width = 87
Name = "txtTo"
      ariatextbox      cbxTo      Class      	container      ariacombobox      *textpicture
oldvalue
ariacontrolsource
      ariacheckbox      ariacommandgroup      ariacontroltoolbar      cmdTop      checkbox      main.vcx      toolbarbutton      toolbar      ariasearchcontainer      ..\icons\brow1.bmp      Pixels      custom      arialistbox      arialistbox      ariaseek     �PROCEDURE Valid
LOCAL lcFontStyle,lcAlias
IF !EMPTY(THIS.VALUE)
  lcAlias = SELECT()
  SELECT (THIS.PARENT.ALIAS)
  lcFontStyle=IIF(THIS.Parent.ARIATEXTBOX1.FONTBOLD,'B','')+;
              IIF(THIS.Parent.ARIATEXTBOX1.FONTITALIC,'I','') 
  THIS.Parent.ARIATEXTBOX1.VALUE = ''
  THIS.Parent.ARIATEXTBOX1.WIDTH = LEN(eval(this.parent.filetags[this.listindex,3]))*FONTMETRIC(7,;
                            THIS.Parent.ARIATEXTBOX1.FONTNAME,THIS.Parent.ARIATEXTBOX1.FONTSIZE,;
                            IIF(EMPTY(lcFontStyle),'N',lcFontStyle))
  THIS.Parent.ARIATEXTBOX1.WIDTH = MIN(THIS.parent.WIDTH-10,THIS.parent.ARIATEXTBOX1.WIDTH)
  THIS.Parent.ARIATEXTBOX1.MAXLENGTH = LEN(eval(this.parent.filetags[this.listindex,3]))
  THIS.Parent.ARIATEXTBOX1.ENABLED=.T.
  SELECT (lcAlias)
ELSE
  THIS.Parent.ARIATEXTBOX1.ENABLED=.F.
  THIS.Parent.ARIATEXTBOX1.VALUE = ''  
ENDIF  
ENDPROC
     ����    �  �                        u!   %   F      k     U          �  U  �  T�  � �� ��  �� ���(��  � � � ��M � T�  � � � �� �� �a�� �� ��C�  � � � �� ��C�  � �	 ��  �� ���(��  � � � ��� � T�  � � � �� �� �-�� �� U
  THIS VALUE LNFRMCNT PARENT OWINDPARENT	 FORMCOUNT FORMS
 LOCKSCREEN GONEXT
 NAVREFRESH Click,     ��1 �A 2�A 2                       m      )   �                        Class      commandbutton      ariasearchcontainer      Ariacombobox1      ariacombobox      main.vcx      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariacombobox      listbox      Class      checkbox      Pixels      Pixels      combobox      combobox      ariacheckbox     BoundColumn = 2
ColumnCount = 1
RowSourceType = 5
RowSource = "THIS.PARENT.FileTags"
FirstElement = (AELEMENT(THIS.PARENT.FileTags,1,1))
Height = 22
Left = 7
NumberOfElements = (ALEN(THIS.PARENT.FileTags,1))
Style = 2
TabIndex = 1
Top = 5
Width = 235
Name = "Ariacombobox1"
      main.vcx      textbox      Ariatextbox1      ariaquiksearch      }FontName = "MS Sans Serif"
FontSize = 8
Height = 22
Left = 8
TabIndex = 2
Top = 32
Width = 148
Name = "Ariatextbox1"
     PROCEDURE InteractiveChange
LOCAL lcAlias
lcAlias = SELECT()
SELECT (THIS.PARENT.ALIAS)
IF !SEEK(ALLT(THIS.VALUE),THIS.PARENT.ALIAS,THIS.PARENT.ARIACOMBOBOX1.VALUE)
  SELECT (THIS.PARENT.ALIAS)
  IF RECNO(0)>0
    GO RECNO(0)
  ENDIF
ENDIF
SELECT (lcAlias)
ENDPROC
      ariaquiksearch      Class      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      ariasearchform      checkbox      �Top = 3
Left = 317
Height = 22
Width = 22
Picture = ..\bmps\delete.bmp
Alignment = 0
ToolTipText = "Delete the current record"
Name = "cmddelete"
      custom      arialine      Pixels      	container      custom      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ariacombobox      �maxlength Specifies the maximum length (in characters) that can be entered into the control. Zero means no limit. For TextBox controls, it only apply to character data if no InputMask is specified.
textpicture
oldvalue
ariacontrolsource
      cbxFrom      editbox      ariasearchform     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         {FontName = "MS Sans Serif"
FontSize = 8
Height = 24
Left = 357
Top = 144
Visible = .F.
Width = 87
Name = "txtFrom"
      ariasearchcontainer      FilterShortCut      �transportvalues Values corisponding to the option buttons in the collection if the field is charactre type
ariacontrolsource
transportvalueslen
      arialine      	arialabel      Pixels      commandbutton     ����    �  �                        ��   %   J      o     Y          �  U  �  T�  � �� ��  �� ���(��  � � � ��M � T�  � � � �� �� �a�� �� ��C�  � � � �� ��C�  � �	 ��  �� ���(��  � � � ��� � T�  � � � �� �� �-�� �� U
  THIS VALUE LNFRMCNT PARENT OWINDPARENT	 FORMCOUNT FORMS
 LOCKSCREEN
 GOPREVIOUS
 NAVREFRESH Click,     ��1 �A 2�A 3                       s      )   �                        LTop = 204
Left = 428
Cancel = .T.
Caption = "Cancel"
Name = "cmdClose"
      Class      �DataSession = 2
BufferMode = 2
formhastoolbar = ('111111')
oldrec = 0
nworkarea = (THIS.GetMasterFile())
activemode = ('S')
key = 
pop_name = ('')
bar_no = 0
Name = "ariaformset"
      ariasearchgrid      commandbutton      Pixels      Pixels      custom      ~BoundColumn = 1
Height = 24
Left = 7
Style = 2
TabIndex = 2
Top = 144
Width = 180
BoundTo = .F.
Name = "cbxExprLeft"
      ariacommandbutton      ariaeditbox      textbox      ,Height = 68
Width = 68
Name = "arialine"
      Class      label      	arialabel      �Height = 260
Width = 549
DoCreate = .T.
AutoCenter = .T.
BorderStyle = 1
Caption = "Find"
FontName = "MS Sans Serif"
FontSize = 8
WindowType = 1
Name = "ariasearchform"
      ariaquiksearch      ariacontrolsource
      ariasearchform      label      Pixels     ���    �   �                         P;   %   �       �      �           �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Error,     ��1 � �1                       ^       )   �                         Ariacommandbutton1      	container      	ariaimage      Pixels      Class      cbxExprRight      ariasearchcontainer      	container      commandgroup      image      commandgroup      image      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
     !���                              ��   %   �       �   	   �           �  U  K 	 ��C��� %��  � � ��4 � T�  � � �-�� B�a�� �D � B�C��� � U  THIS PARENT ACTIVATEBROWSEBUTTON When,     ��1 � 1q � � A 1                       �       )                           BHeight = 18
Width = 29
selectbutton = .T.
Name = "ariabrowse"
      ariacommandbutton      4lastactiverow
lastactivecolumn
ariacontrolsource
      Pixels      combobox      �Width = 542
Height = 177
BorderWidth = 0
operatorpopup = (gfTempName())
valuepopup = (gfTempName())
Name = "ariasearchcontainer"
      ariasearchgrid      �FontName = "MS Sans Serif"
FontSize = 8
Height = 53
Width = 100
textpicture = C
ariacontrolsource = ('')
Name = "ariaeditbox"
      ariasearchcontainer      Class      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      combobox      main.vcx      listbox      main.vcx      txtTo      main.vcx      ariacombobox      fromseek
^filetags[1,1] 
      ariagrid      	ariaimage      }FontName = "MS Sans Serif"
FontSize = 8
Height = 22
Left = 6
TabIndex = 2
Top = 32
Width = 148
Name = "Ariatextbox1"
      main.vcx      main.vcx      cmdClearFilter      	Ariagrid1      main.vcx      ariaapplication      ariagrid      Pixels      ariacommandgroup      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      Ariacommandbutton2      ariaseek      ariaseek      main.vcx      -Height = 68
Width = 68
Name = "ariaimage"
      ariaseek      ariagrid      ariaapplication      ariaseek      commandbutton     DataSession = 1
AutoRelease = .T.
WindowType = 1
formhastoolbar = 
Name = "addbook"
Ariaform1.Height = 343
Ariaform1.Width = 492
Ariaform1.DoCreate = .T.
Ariaform1.BorderStyle = 1
Ariaform1.Caption = "Address Book"
Ariaform1.WindowType = 1
Ariaform1.Name = "Ariaform1"
      Ariaquiksearch1      ariaeditbox      �PROCEDURE Click
IF THISFORM.FIND
  GO this.PARENT.nrecordno IN (THIS.PARENT.ALIAS)
ELSE
  SET ORDER TO (THISFORM.ORDER)
ENDIF  
THISFORM.RELEASE
ENDPROC
      ITop = 96
Left = 316
Caption = "\<Cancel"
Name = "Ariacommandbutton2"
      DTop = 96
Left = 88
Caption = "\<Ok"
Name = "Ariacommandbutton1"
      ,PROCEDURE Click
THISFORM.RELEASE
ENDPROC
     ����    �  �                        )o   %   B      g     Q          �  U  �  T�  � �� ��  �� ���(��  � � � ��M � T�  � � � �� �� �a�� �� ��  � � � � ��C�  � �	 ��  �� ���(��  � � � ��� � T�  � � � �� �� �-�� �� U
  THIS VALUE LNFRMCNT PARENT OWINDPARENT	 FORMCOUNT FORMS
 LOCKSCREEN GOTOP
 NAVREFRESH Click,     ��1 �A �A 3                       o      )   �                        iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      �FontName = "MS Sans Serif"
FontSize = 8
Caption = "Option1"
Height = 19
Width = 71
ariacontrolsource = ('')
Name = "ariaoptionbutton"
      aHeight = 24
Left = 357
Style = 2
TabIndex = 5
Top = 144
Width = 180
Name = "cbxExprRight"
      {FontName = "MS Sans Serif"
FontSize = 8
maxlength = 0
textpicture = C
ariacontrolsource = ('')
Name = "ariacombobox"
      iPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
     ���    �   �                         ~   %   �       �      �           �  U  3 	 ��C���' T�  � � ��  � � �  � � ��� U  THIS KEYCMD LEFT
 KEYTEXTBOX WIDTH Init,     ��1 � q1                       `       )   �                        PROCEDURE Ariacombobox1.Valid
IF THISFORM.Find
  DODEFAULT()
  IF !EMPTY(THISFORM.INITORDER) AND !EMPTY(THISFORM.KEY) AND THIS.VALUE=THISFORM.INITORDER
    THIS.PARENT.ARIATEXTBOX1.VALUE = THISFORM.KEY
  ENDIF
ELSE
  DODEFAULT()
  THIS.PARENT.ARIATEXTBOX1.VISIBLE = .F.
  SET ORDER TO (THIS.VALUE)
  IF TYPE('THISFORM.browse')='O'
    THISFORM.BROWSE.REFRESH()
  ENDIF  
ENDIF
ENDPROC
PROCEDURE Ariatextbox1.InteractiveChange
DODEFAULT()
IF TYPE('THISFORM.browse')='O'
  THISFORM.BROWSE.REFRESH()
ENDIF
ENDPROC
      �Top = 3
Left = 5
BorderWidth = 0
Name = "Ariaquiksearch1"
Ariacombobox1.Name = "Ariacombobox1"
Ariatextbox1.Name = "Ariatextbox1"
      �alias Specifies the alias used for each table or view associated with a Cursor object.
nrecordno
browse
find
order Specifies the controlling index tag for a Cursor object.
key
initorder
      �FontName = "MS Sans Serif"
FontSize = 8
Height = 151
Width = 185
maxlength = 0
textpicture = C
ariacontrolsource = ('')
Name = "arialistbox"
     %PROCEDURE Init
lParameters tcAlias,toBrowse,tlFind,tcOrder,tcKey
IF PARAMETERS()>0
  THIS.ALIAS = tcAlias
ELSE
  THIS.ALIAS = ALIAS()  
ENDIF
THIS.KEY=tcKey
THIS.INITORDER = tcOrder
IF TYPE('THIS.KEY')<>'C'
  THIS.KEY=''
ELSE
  THIS.KEY = EVAL(THIS.KEY)  
ENDIF
IF TYPE('THIS.InitOrder')<>'C'
  THIS.InitOrder=''
ENDIF


this.browse = toBrowse
this.find = tlFind
this.nrecordno = RECNO(ALIAS())
THIS.ARIAQUIKSEARCH1.INIT(THIS.ALIAS,tlFind)
IF !THIS.find
  THIS.ARIAQUIKSEARCH1.ARIATEXTBOX1.VISIBLE = .F.
  THIS.CAPTION = 'Order'
  this.order = ORDER(THIS.ALIAS)
ELSE  
  THIS.CAPTION = 'Seek (Quik find)'
ENDIF

IF THIS.FIND AND VAL(SYS(21)) > 0
  THIS.ARIAQUIKSEARCH1.ARIACOMBOBOX1.LISTINDEX = VAL(SYS(21))
  THIS.ARIAQUIKSEARCH1.ARIACOMBOBOX1.VALID()
ENDIF  
ENDPROC
     (maxlength Specifies the maximum length (in characters) that can be entered into the control. Zero means no limit. For TextBox controls, it only apply to character data if no InputMask is specified.
textpicture Specifies wether the text will be digits or characters
oldvalue
ariacontrolsource
      �Top = 3
Left = 296
Height = 22
Width = 22
Picture = ..\bmps\edit1.bmp
Alignment = 0
ToolTipText = "Edit the current record"
Name = "cmdedit"
      �AllowAddNew = .F.
DeleteMark = .F.
Height = 91
Left = 2
ReadOnly = .T.
RecordSourceType = 1
TabIndex = 3
Top = 60
Width = 504
Name = "Ariagrid1"
      �PROCEDURE Click
This.Value=0
This.Parent.oWindParent.ChangeMode('S')
THIS.Parent.NavRefresh()
THIS.Parent.ButtonRefresh()

ENDPROC
      �Height = 153
Width = 495
DoCreate = .T.
AutoCenter = .T.
Caption = "Form2"
FontName = "MS Sans Serif"
FontSize = 8
WindowType = 1
Name = "ariaseek"
      �addednewbar
operatorpopup
valuepopup
^afilterexpr[1,7] 
^aavailablefields[1,4] 
^aoperators[8,2] 
^afieldvalues[1,4] 
*initializefields 
*buildlist 
*buildbar 
*getvalue 
*removeline 
*arrange 
*buildlistbars 
*skipoperator 
*getmask 
     ����    �  �                        m   %   6      u  
   a          �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR  T�  � ��  � �� U  THIS OLDVALUE VALUEL 4 %�C� ThisFormb� O� C�  � f� ARIAFORM	��E � ��C � �  � �� � U  THISFORM CLASS ADJUSTOBJECTWIDTH THIS Error,     �� When�     �� Init�     ��1 � �2 12 AA 1                       ^         y   �         �         )   �                        �PROCEDURE Click
THIS.VALUE=0
IF This.Parent.oWindParent.Delete()
  This.Parent.oWindParent.ChangeMode('S')
  This.Parent.ButtonRefresh()
  This.Parent.NavRefresh()
ENDIF
ENDPROC
     $PROCEDURE Init
IF TYPE("ThisForm") = "O" AND UPPER(ThisForm.Class) = "ARIAFORM"
  ThisForm.AdjustObjectWidth(This)
ENDIF
ENDPROC
PROCEDURE When
This.OldValue = This.Value
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
     �PROCEDURE InteractiveChange
IF UPPER(THISFORM.CLASS)='ARIAFORM'
  IF (THISFORMSET.EditMode OR ThisFormSET.AddMode) AND ThisFormSET.FormHasControlPanel
     IF !oAriaApplication.oToolBar.CanContinue()
       RETURN .F.
     ENDIF  
  ENDIF  
ENDIF
LOCAL lcAlias
lcAlias = SELECT()
SELECT (THIS.PARENT.ARIAGRID1.RECORDSOURCE)
IF !SEEK(ALLT(THIS.VALUE),THIS.PARENT.ARIAGRID1.RECORDSOURCE,THIS.PARENT.ARIACOMBOBOX1.VALUE)
  SELECT (THIS.PARENT.ARIAGRID1.RECORDSOURCE)
  IF RECNO(0)>0
    GO RECNO(0)
  ENDIF
ENDIF
SELECT (lcAlias)
THIS.PARENT.FROMSEEK=.T.
THIS.PARENT.ARIAGRID1.HIGHLIGHT=.F.
THIS.PARENT.ARIAGRID1.SETFOCUS()
*THIS.PARENT.ARIAGRID1.REFRESH
THISFORMSET.REFRESHALL()
ENDPROC
     8���                              �   %   �      �     �          �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR� ( T�  �� \<Small Icons,\<Large Icons�� T� �� T~T,T~T��# T� �C �  �   � � � � � �� H�l �� � �� ���} � �� ���� � �� ���� � � U  LCBARS LCSTATUS LNBAR THISFORMSET	 ARIAFORM1 ARIASHORTCUT1 SHOWSHORTCUT THIS Error,     ��
 RightClick�     ��1 � �2 �A1� A 3                       ^            j      )                          BoundColumn = 2
ColumnCount = 1
RowSourceType = 5
RowSource = "THIS.PARENT.FileTags"
FirstElement = (AELEMENT(THIS.PARENT.FileTags,1,1))
Height = 22
Left = 5
NumberOfElements = (ALEN(THIS.PARENT.FileTags,1))
Style = 2
TabIndex = 1
Top = 5
Width = 235
Name = "Ariacombobox1"
     -���                              ��   %   �      �     �          �  U  � ��  � T� � �� �� %�C� � � � �
���� 5� � � %�C� � � ��a � F�� � � �� � T� ����< � BROWSE(� �� � � � � � � �
 �-��-��-��a�� %�� ���� T� � � � �CO�� T� � � �� � � � �� T� � � �-�� T� � � � �� � � �� T� � � �-�� T� � � � �� � � �� ��C� V� � � � �� ��C� � � �� ��C� � � �� B�a�� � B�-�� ��� B�-�� � U  OSEARCHDLOG THIS VALUE PARENT OWINDPARENT BROWSEFIELDS OBROWSE
 LLSELECTED	 NWORKAREA BROWSE BROWSETITLE OLDREC TOPFILE ENDFILE
 CHANGEMODE
 NAVREFRESH BUTTONREFRESH Click,     ��1 q �� aA � �� Q���rr A r � q A 2                       �      )                          ���    �   �                         �   %   �       �      �           �  U  <  %��  � � ��  � ��  � � � �5 � ��  � � � � U  THISFORM CMDTO DEFAULT CLICK CMDCC Valid,     ��1 1� � � A 1                       g       )   �                        <PROCEDURE Valid
this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,6]=this.parent.cbxFrom.value+','+this.value
THIS.PARENT.BUILDBAR(4)


ENDPROC
PROCEDURE LostFocus
this.visible = .f.
this.parent.cbxFrom.visible=.f.
this.parent.cbxExprRight.visible=.T.
this.parent.lstExpression.WHEN()
ENDPROC
     K���    2  2                        t�   %   �      �     �          �  U  � T�  � �� ��  �� ���(��  � � � ��M � T�  � � � �� �� �a�� �� %��  � � � � EA��� ��C�  � � �	 �� %�C�  � � �
 
��� �  �� ���(��  � � � ��� � T�  � � � �� �� �-�� �� B� ��0 ��CC� SVC�  � � � � AE��\�  � � � �� � �1� ��  � � � � � T�  � � ��  � � � �� T�  � � ��  � � � �� ��C�  � � �� ��C�  � � ��  �� ���(��  � � � ���� T�  � � � �� �� �-�� �� U  THIS VALUE LNFRMCNT PARENT OWINDPARENT	 FORMCOUNT FORMS
 LOCKSCREEN
 ACTIVEMODE
 BEFORESAVE	 SAVEFILES
 CHANGEMODE ADDNEW TOPFILE BUTTONREFRESH
 NAVREFRESH Click,     ��1 �A �3��A B � A � A ���A 3                             )   2                       ����    �  �                        �h   %   �           �          �  U  0 # ��C� EBMAINC� INCRSRHC�
��  ��
 �� � � U 
 GFDOTRIGER THISFORM RELEASEw& %�C� EBMAINC� CHKTRGG�
��  ��* � B� � %�C� � � ���� � %�� � � ��o � ��CC� � �� � � ��� �� �' T� �� � � � ="C� � �� "�� F�� � � �� LOCATE FOR &lcField
 � �2�: T� �� � � �  AND � � � � ="C� � �� "�� F�� � � �� LOCATE FOR &lcField
 �* %�C� THISFORM.GRIDTORefreshb� O��p� ��C�	 �
 � �� � U 
 GFDOTRIGER THIS PARENT LCFILTER ISKEY VALUE FILENAME LCFIELD FIELD THISFORM GRIDTOREFRESH REFRESH  ��C�  � �� \�� {END}�� U  THIS INTERACTIVECHANGE Valid,     �� InteractiveChange     �� ProgrammaticChangeh    ��1 2� 2 bA A T2�� qsA � �qA �A 2 � � 1                       �         �   "        K  u  +    )   �                       ����    �  �                        V/   %         i     E          �  U    ��C�  � �� U  GLOBALBROWSEWINDOW	 ADDSELECT�  ��  � � � � H�  �� � ��  �� � �	��I � ��C� � ��& ��  �� � �	� � � 	��~ � ��CCO� � �� ��  �� � � 	��� � T� � �C� �	 ��� ��  ���� � ��C� �
 �� � U  NBUTTON NSHIFT NXCOORD NYCOORD GLOBALBROWSEWINDOW	 ADDSELECT MULTISELECT ADDREGON LASTSELECTION SELECTFIELD
 RIGHTCLICK.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Click,     �� MouseUp_     �� Error�    ��1 � 2 1� �� a� �Q� A 2 � �1                       /         M   �          b      )   �                        �Top = 3
Left = 267
Height = 22
Width = 22
Picture = ..\bmps\brow1.bmp
Alignment = 0
ToolTipText = "Search for a specific record"
Visible = .T.
Name = "cmdfind"
     �PROCEDURE Valid
LOCAL lcFontStyle,lcAlias
lcAlias = SELECT()
SELECT (THIS.PARENT.ARIAGRID1.RECORDSOURCE)
lcFontStyle=IIF(THIS.Parent.ARIATEXTBOX1.FONTBOLD,'B','')+;
            IIF(THIS.Parent.ARIATEXTBOX1.FONTITALIC,'I','') 

THIS.Parent.ARIATEXTBOX1.WIDTH = LEN(eval(this.parent.filetags[this.listindex,3]))*FONTMETRIC(7,;
                          THIS.Parent.ARIATEXTBOX1.FONTNAME,THIS.Parent.ARIATEXTBOX1.FONTSIZE,;
                          IIF(EMPTY(lcFontStyle),'N',lcFontStyle))
THIS.Parent.ARIATEXTBOX1.WIDTH = MIN(THIS.parent.WIDTH-10,THIS.parent.ARIATEXTBOX1.WIDTH)
THIS.Parent.ARIATEXTBOX1.MAXLENGTH = LEN(eval(this.parent.filetags[this.listindex,3]))
THIS.Parent.ARIATEXTBOX1.ENABLED=.T.
SELECT (lcAlias)
ENDPROC
     ErasePage = .T.
PageCount = 2
Width = 282
Height = 146
Name = "ariapageframe"
Page1.FontName = "MS Sans Serif"
Page1.FontSize = 8
Page1.Caption = "Page1"
Page1.Name = "Page1"
Page2.FontName = "MS Sans Serif"
Page2.FontSize = 8
Page2.Caption = "Page2"
Page2.Name = "Page2"
     ����    �  �                        �   %   :      y  
   e          �  U  L 4 %�C� ThisFormb� O� C�  � f� ARIAFORM	��E � ��C � �  � �� � U  THISFORM CLASS ADJUSTOBJECTWIDTH THIS  T�  � ��  � �� U  THIS OLDVALUE	 LISTINDEX.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Init,     �� When�     �� Error�     ��1 AA 3 12 � �1                       }         �   �         �     
    )   �                       ����    �  �                        Z�   %   S      t  	   b          �  U  �  %��  � � Like��� �- %�C� ,C�  � � � ��  � � �� ��� �f T�  � � ��  � � � �����CC�  � � � ��  � � �C� ,C�  � � � ��  � � ��\�� ��C��  � � �� � � ��C��  � � �� U  THIS VALUE PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID BUILDBAR Valid,     ��1 ��c1A A 11                       �      )   �                       *PROCEDURE Init
IF TYPE("ThisForm") = "O" AND UPPER(ThisForm.Class) = "ARIAFORM"
  ThisForm.AdjustObjectWidth(This)
ENDIF

ENDPROC
PROCEDURE When
This.OldValue = This.ListIndex
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
      �Top = 3
Left = 238
Height = 22
Width = 22
Picture = ..\bmps\tbprint.bmp
Alignment = 0
ToolTipText = "Print a Report for the form"
Name = "cmdprint"
     iPROCEDURE AfterRowColChange
LPARAMETERS nColIndex
IF THIS.PARENT.FROMSEEK
  THIS.PARENT.FROMSEEK=.F.
  THIS.HIGHLIGHT=.T.  
  THIS.PARENT.ARIATEXTBOX1.SETFOCUS
*  RETURN .F.
*  NODEFAULT
ENDIF
IF UPPER(THISFORMSET.CLASS)='ARIAFORMSET'
  THISFORMSET.REFRESHALL()
  IF !EMPTY(THISFORMSET.FormHasToolBar)
    IF TYPE('oAriaApplication.oToolBar')='O'
      oAriaApplication.oToolBar.EndFile = EOF()
      oAriaApplication.oToolBar.oWindParent.EndFile = oAriaApplication.oToolBar.EndFile 
      oAriaApplication.oToolBar.TopFile = BOF()
      oAriaApplication.oToolBar.oWindParent.TopFile = oAriaApplication.oToolBar.TopFile 
      oAriaApplication.oToolBar.NavRefresh()
      oAriaApplication.oToolBar.REFRESH
    ENDIF
  ENDIF
ELSE
  THISFORM.REFRESH
ENDIF  
ENDPROC
PROCEDURE Init
IF THIS.PARENT.BASECLASS='Container'
  THIS.LEFT =0
  THIS.WIDTH = THIS.PARENT.WIDTH
  THIS.TOP  =0
  THIS.HEIGHT = THIS.PARENT.HEIGHT
  IF EMPTY(This.RecordSource) AND UPPER(THISFORMSET.CLASS)='ARIAFORMSET'
    THIS.RecordSource = thisformset.GetMasterFile()
  ENDIF  
ENDIF
ENDPROC
PROCEDURE BeforeRowColChange
LPARAMETERS nColIndex
IF UPPER(THISFORMSET.CLASS)='ARIAFORMSET'
  IF (THISFORMSET.EditMode OR ThisFormSET.AddMode) AND !EMPTY(ThisFormSET.FormHasToolBar)
     IF !thisformset.CanContinue()
       NODEFAULT  
     ENDIF  
  ENDIF  
ENDIF  
ENDPROC
     ����    �  �                        m   %   6      u  
   a          �  U  L 4 %�C� ThisFormb� O� C�  � f� ARIAFORM	��E � ��C � �  � �� � U  THISFORM CLASS ADJUSTOBJECTWIDTH THIS  T�  � ��  � �� U  THIS OLDVALUE VALUE.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Init,     �� When�     �� Error�     ��1 AA 2 12 � �1                       {         �   �         �     	    )   �                       �PROCEDURE Click
FOR lnCnt = 1 TO ThisForm.lsContacts.ListCount
		IF ThisForm.lsContacts.Selected(lnCnt)  && Is item selected?
          THISFORM.lsContacts.ListIndex = lnCnt
          lnIndex = THISFORM.lsContacts.ListIndex
          IF !EMPTY(THISFORMSET.laCc[1,1])
            DIME THISFORMSET.laCc[ALEN(THISFORMSET.laCc,1)+1,ALEN(THISFORMSET.laCc,2)]
          ENDIF  
          THISFORMSET.laCc[ALEN(THISFORMSET.laCc,1),1] = THISFORM.lsContacts.DISPLAYVALUE
          THISFORMSET.laCc[ALEN(THISFORMSET.laCc,1),2] = THISFORM.lsContacts.VALUE
          THISFORMSET.laCc[ALEN(THISFORMSET.laCc,1),3] = MTADBOOK.cBookComp
          THISFORM.lsCc.REQUERY()
		ENDIF
ENDFOR




ENDPROC
     ���    �  �                        ��   %   f      �  	   �          �  U  L  T�  � �-�� T�  � � � �-�� T�  � � � �a�� ��C�  � � � �� U  THIS VISIBLE PARENT TXTFROM CBXEXPRRIGHT LSTEXPRESSION WHENS = T�  � � ��  � � � ������  � � � � ,�  � �� ��C��  � � �� U  THIS PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID TXTFROM VALUE BUILDBAR	 LostFocus,     �� Valid�     ��1 � AA12 �12                       �         �   /      )   �                       �PROCEDURE Click
FOR lnCnt = 1 TO ThisForm.lsContacts.ListCount
		IF ThisForm.lsContacts.Selected(lnCnt)  && Is item selected?
          THISFORM.lsContacts.ListIndex = lnCnt
          lnIndex = THISFORM.lsContacts.ListIndex
          IF !EMPTY(THISFORMSET.laTo[1,1])
            DIME THISFORMSET.laTo[ALEN(THISFORMSET.laTo,1)+1,ALEN(THISFORMSET.laTo,2)]
          ENDIF  
          THISFORMSET.laTo[ALEN(THISFORMSET.laTo,1),1] = THISFORM.lsContacts.DISPLAYVALUE
          THISFORMSET.laTo[ALEN(THISFORMSET.laTo,1),2] = THISFORM.lsContacts.VALUE
          THISFORMSET.laTo[ALEN(THISFORMSET.laTo,1),3] = MTADBOOK.cBookComp
          THISFORM.lsTo.REQUERY()
		ENDIF
ENDFOR




ENDPROC
     9PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
IF nKeyCode = 7
  lnDeleted = 0
  FOR lnCnt = 1 TO THIS.ListCount
	    IF THIS.Selected(lnCnt)  && Is item selected?
          lnDeleted = lnDeleted + 1
          =ADEL(THISFORMSET.laCc,lnCnt)
		ENDIF
   ENDFOR
   IF ALEN(THISFORMSET.laCc,1)-lnDeleted < 1
     DIMEN THISFORMSET.laCc[1,ALEN(THISFORMSET.laCc,2)]
     STORE '' TO THISFORMSET.laCc
   ELSE  
     DIMEN THISFORMSET.laCc[ALEN(THISFORMSET.laCc,1)-lnDeleted,ALEN(THISFORMSET.laCc,2)]   
   ENDIF
   THIS.REQUERY()      
ENDIF
ENDPROC
     ;bar_no running Bar No. from the menu
pop_name Menu Popup name from which the form is running
multirun Program Is Multi Instance or not
datapath Data Path From which the Data files are Opened for this form
moduleid Form Module ID
programcopyno Hold the Number of running copy of a program
*adjustobjectwidth 
     $PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
PROCEDURE When
This.OldValue = This.Value
ENDPROC
PROCEDURE Init
IF TYPE("ThisForm") = "O" AND UPPER(ThisForm.Class) = "ARIAFORM"
  ThisForm.AdjustObjectWidth(This)
ENDIF
ENDPROC
     4���                              E   %   �      �  <             �  U  � ��  � � � � �' %�C�t� � C� tcCationb� C��B � B� �+ T� �CC� tcStatusb� C� � � � T6�� �� �% T� �CC� tnIndexb� N� -� a6��O T� �CC� tnIndexb� N� � �) C� � ��CCC��� � �� � � �66��! %�� 
� � C� � ��	��� B� ��� %�� ��D� � � �� ��C� � ���� � T� � �� ������  �� T� � �� ������ �� T� � �� ������ �� T� � �� ������ �� � U  TCCATION TOOBJECT TCMETHOD TCSTATUS TNINDEX LLADD THIS LIST�  ��  �9 %�C� tnIndexb� N� C� � ���  � �  � ��D � B� � ��C� � �  ��� %�C� � ������ �* � � �C� � �����C� � ���� � U  TNINDEX THIS LIST'  �  � ������� J�-�(�  � � U  THIS LIST�  T�  � �-�� T�  � �-�� T�  � �-�� T�  � �-�� T�  � �-�� T�  � �� TTTT�� T�  � �a�� T�  � �-�� T�  �	 �-�� ��C�  �
 �� U  THIS ALIAS BROWSEFIELDS BROWSETITLE	 BROWSEKEY	 BROWSEFOR DEFAULTSHORTCUT SELECTBUTTON SELECTBUTTONREFERENCE SELECTBUTTONMETHOD CLEARSHORTCUTD ��  � � T� �CW�� %�C� � �
��7 � F�� � �� � %�C� � ���} �0 T� � �C� GetC� Table� BrowseFields� �� � 5� � � T� ���� ��C� � �� ��� T�	 ��
 � �
 \SY\BROWSE��e ��	 �(�  �� � � � � � � � � � � � � � � � ��
 laShortCut�� � � � �
 F�� ��	 B��  �� U  LLRETURNVALUE LCALIAS THIS ALIAS BROWSEFIELDS GFDATABASEPROP OBROWSE
 LASHORTCUT LIST LCTORUN OARIAAPPLICATION
 SCREENHOME BROWSETITLE	 BROWSEKEY	 BROWSEFOR DEFAULTSHORTCUT SELECTBUTTON SELECTBUTTONREFERENCE SELECTBUTTONMETHOD MULTISELECTALIAS MULTISELECTFIELD addshortcut,     �� removeshortcut3    �� clearshortcut�    �� resetall'    �� browseV    ��1 qqA A �q Q�A � � �A ����A 2 q �A A !��A 2 a� 2 � � � � � A� � � � 2 � � 1� A !A � � A�U� � 2                       e        �  e        �  �      !   �  �  $   ,   �  	  0    )                          w���    ^  ^                        �   %   �      �     �          �  U  #  ��  � � ��C �   � � � �� U  NKEYCODE NSHIFTALTCTRL THISFORM KEYPRESSL 4 %�C� ThisFormb� O� C�  � f� ARIAFORM	��E � ��C � �  � �� � U  THISFORM CLASS ADJUSTOBJECTWIDTH THIS  T�  � ��  � �� U  THIS OLDVALUE	 LISTINDEX.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR KeyPress,     �� Init�     �� When�     �� Error/    ��1 � Q2 AA 2 12 � �1                       c         ~   �           "  
   
   >  �      )   ^                       jPROCEDURE LostFocus
IF this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,5] ='Like'
  THIS.Visible = .F.
  THIS.PARENT.cbxExprRight.Visible = .t.
  this.parent.lstExpression.WHEN()
ELSE
  this.parent.txtto.setfocus()
ENDIF
ENDPROC
PROCEDURE Valid
IF this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,5] <> 'Betweem'
  this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,6]=this.value
  THIS.PARENT.BUILDBAR(4)
ELSE
  this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,6]=this.value+','+;
  this.parent.txtto.value
  THIS.PARENT.BUILDBAR(4)  
ENDIF  
ENDPROC
     c���    J  J                        Y�   %   �      �  (   �          �  U  ~ ��  � � � � T� � �� ��* T� � �CC� lcFileb� C� C� � 6��" %�C� toBrowseColumnb� O��} �
 �� � � B� � F�� � �� %�C�  � �
���� �� � %�C� .�  � �� ��� �" T� �C�  � C� .�  � ��\�� �� � T� ��  � �� � T� �	 �CCCmC� >=fC� f�� T� �
 ��  � �� T� � ��  � ��- %�C� THIS.GRIDTORefresh.PARENTb� O���� T� � �� � � �� � � %�C� �
 �����
 �� � � B� � �� {�  � T� � ��  � �
�� T� � � ��  � �� T� � � ��  � �� T� � � ����* T� � � �CC�  � �� � !� �  � 6�� T� � � ��  � �� T� � � �CC�  f�� T� � �a�� U  TOBROWSECOLUMN TNKEY LCFILE LCFILTER THIS FILENAME RELEASE CONTROLSOURCE LCKEY ISKEY FIELD GRIDTOREFRESH PARENT WIDTH TXTSEEKVALUE	 MAXLENGTH LEFT FORMAT	 INPUTMASK VALUE
 AUTOCENTER  �� U   Init,     �� Unloadw    ��1 3�!� A A � 1q �!� A �11�aA A !� A A R � raa1�aq� 2 Q 1                       2     &   O  V  ,    )   J                       ���    �  �                        ��   %   f      �  	   �          �  U  S = T�  � � ��  � � � ������  � � � � ,�  � �� ��C��  � � �� U  THIS PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID CBXFROM VALUE BUILDBARL  T�  � �-�� T�  � � � �-�� T�  � � � �a�� ��C�  � � � �� U  THIS VISIBLE PARENT CBXFROM CBXEXPRRIGHT LSTEXPRESSION WHEN Valid,     ��	 LostFocus�     ��1 �14 � AA11                       �         �   1      )   �                       :PROCEDURE LostFocus
this.visible = .f.
this.parent.txtFrom.visible=.f.
this.parent.cbxExprRight.visible=.T.
this.parent.lstExpression.WHEN()
ENDPROC
PROCEDURE Valid
this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,6]=this.parent.txtFrom.value+','+this.value
THIS.PARENT.BUILDBAR(4)

ENDPROC
     a���    H  H                        �w   %   �       �      �           �  U  g  F�  �! T� �� "C� � C� >�� "��' SET FILTER TO CBOOKCOMP = &lcFilter
 #)� ��C� � � �� U  MTCONTCT LCFILTER THIS VALUE	 CBOOKCOMP THISFORM
 LSCONTACTS REQUERY Valid,     ��1 q qQ 1                       �       )   H                       �browsefields Browse Fields to be displayed
browsetitle Browse Window Title
browsekey Key expression for the browse command to scope the displayed records
browsefor For expression for the browse command to scope throught the records displayed
defaultshortcut Browse window short cut access
selectbutton Used to visible the select button in the browse window or not
selectbuttonreference Reference for an object to be used in the selection of the select button
selectbuttonmethod Method to be call if select button is selected
alias Alias for file to be browsed
multiselectalias
multiselectfield
^list[1,4] A character string array used to access the items in a ComboBox or ListBox control.
*addshortcut To add a user defined short cut to the browse short cut menu
*removeshortcut to remove a user defined short cut bar from the browse short cut menu
*clearshortcut Clear all the user defined short cut bars
*resetall Reset all the Browse properties to its default values
*browse to run the browse window
     �PROCEDURE Valid
*C200464,1 HBG 2/1/2003 Locate for the record in valid method in Trigger for GMA [Begin]
=gfDoTriger('EBMAIN',PADR('INCRSRHC',10))
*C200464,1 [End]

THISFORM.RELEASE
ENDPROC
PROCEDURE InteractiveChange
*C200464,1 HBG 2/1/2003 Move the code to the vlalid method in Trigger for GMA [Begin]
IF gfDoTriger('EBMAIN',PADR('CHKTRGG',10))
  RETURN
ENDIF
*C200464,1 [End]

*B803883,1 TMI [START] 10/28/2001 Apply the old route if no filter passed
IF EMPTY(THIS.PARENT.lcFilter)
*B803883,1 TMI [END  ] 
  IF THIS.PARENT.ISKEY
    =SEEK(ALLT(THIS.VALUE),THIS.Parent.FILENAME)
  ELSE
    lcField = THIS.PARENT.FIELD+'="'+ALLT(THIS.VALUE )+'"'

    *B606228,1 Select The table before locate Hassan [Begin]
    *Hassan Add the 'lcFilter' property as it was not added.
    SELECT (THIS.PARENT.FILENAME)
    *B606228,1 Select The table before locate Hassan [End]    

    LOCATE FOR &lcField
  ENDIF
*B803883,1 TMI [START] 10/28/2001 If a filter is passed then locate with the filter
ELSE
  lcField = THIS.PARENT.lcFilter+' AND '+THIS.PARENT.FIELD+'="'+ALLT(THIS.VALUE )+'"'
  SELECT (THIS.PARENT.FILENAME)
  LOCATE FOR &lcField
ENDIF
*B803883,1 TMI [END  ] 

IF TYPE('THISFORM.GRIDTORefresh')='O'
*    WAIT ALLT(THIS.VALUE) WINDOW
  THISFORM.GRIDTORefresh.refresh()  
ENDIF
ENDPROC
PROCEDURE ProgrammaticChange
THIS.INTERACTIVECHANGE()
KEYBOARD "{END}"
ENDPROC
     E���    ,  ,                        5�   %   �       �      �           �  U  T  %��  � ��, � #�� � � ��� � � �� �C � G((��  � �� �
 ��  � � U  THISFORM FIND THIS PARENT	 NRECORDNO ALIAS ORDER RELEASE Click,     ��1 �� � A � 1                       �       )   ,                       pHeight = 15
Width = 19
syspath = (FULL(SET('DEFAULT'))+"\SYSFILES\")
datadir = ("")
user_id = ("")
user_name = ("ARIA_"+THIS.STATION)
user_level = ("")
user_group = ("")
activemoduleid = ("SY")
activecompanyid = ("")
activemodulename = ("")
activecompanyname = ("")
activemodulebuildnumber = ("")
systembuildnumber = ("")
winsepta = 0
usercompanyid = 
companybar = 0
workdir = 
reporthome = ''
applicationhome = ("")
bitmaphome = ("")
resourcehome = 
helptopic = ('')
systemname = Aria Advantage Systems
defaultpath = ('')
installpath = 
station = (gfStation())
prntcompanyid = 
basecurrency = ("USDLR")
phonemask = 
companyinstalledmodules = ("SM")
bar_no = 0
pop_name = 
helpfile = (SET('HELP',1))
firsttime = .T.
allowadd = .T.
allowedit = .T.
allowdelete = .T.
defaultcountry = ("")
currentyear = ("")
currentperiod = ("")
systemdate = (DATE())
userstaticrecord = 1
screenhome = ("")
currentsite = ("")
ariaversion = ("Version 2.7")
classdir = ("")
edipath = ("")
errorhandlerenabled = .T.
cfoxdbid = FOX
csqldbid = SQL
cnativedbid = NATIVE
rebldcmp = 
Name = "ariaapplication"
     V���    =  =                        �v   %   �      �  �   |          �  U  � ��  �C %�C� oControlb� O� �  � � 	� CC�  � �=f� ARIA	���� T� �C�  � f��" T� �C� � N� � 7� � W6�� T� �C� �  � Q�� T� �� �	 �� T�
 �� � �� T� �� � �� T� �� � �� T� �� � �� T� �� � �� T� �	 ��  �	 �� T� � ��  � �� T� � ��  � �� T� � ��  � �� T� � ��  � �� T� � ��  � ��" T� �C� �  � �  � C��&�#�� T� �� ���& T� �C��  � �  � C��&�$� �� H���l�- �CC�  � fC� ComboBoxfC� ListBoxf����  T� �C� � N� �
� �6�� �C�  � fC� TextBoxf��W�  T� �C� � N� �� �
6�� 2�l� T� �� �� � T�  � �� �� T� �	 �� �� T� � ��
 �� T� � �� �� T� � �� �� T� � �� �� T� � �� �� � T�  ���� U  OCONTROL	 MAXLENGTH CLASS LCTXTPIC TEXTPICTURE	 LCCHTOUSE LCTEXT
 LLFONTBOLD THISFORM FONTBOLD LLFONTITALIC
 FONTITALIC LLFONTSHADOW
 FONTSHADOW LLFONTSTRIKE FONTSTRIKETHRU LLFONTOUTLINE FONTOUTLINE LLFONTCONDENCE FONTCONDENSE LNWIDTH FONTNAME FONTSIZE LNFONTLENGTH	 BASECLASS
 LNBORWIDTH WIDTHH$ %�C� oAriaApplicationb� O��A�1 %��  � �� C� THISFORM.PARENTb� O	��c �
 ��  � � �� �=�$ %�� � � EA�
 C� � �
	��� �! %�� � � S�	 C� � 
	��� � �� B�-�� �� � %�� � � A��� � ��Ca� � �� � � � T�  � �-��# %�C� THISFORM.PARENTb� O��*� <� � �9� <�  � � � � U	  THISFORM TABINDEX HIDE THISFORMSET
 ACTIVEMODE FORMHASTOOLBAR CANCONTINUE UNDO VISIBLE8 $ %�C� oAriaApplicationb� O��1 � ��C�  � �� � U  OARIAAPPLICATION WINARNGv  %��  � ���" � T�  � �-�� �$ %�C� oAriaApplicationb� O��o � T� � �a�� ��C� � � A� � �� � U  THISFORM TABINDEX VISIBLE OARIAAPPLICATION REBLDCMP SETMENU ACTIVEMODULEID� T�  � ��  � ��$ %�C� oAriaApplicationb� O����. Q�  �� � � sycInt��� ��	 cContCode� %�C� � � Intern���� � T� �CC� � f��� G(�CC� �	 f��� G8(�CC� �
 f��� SET CURRENCY &lcCurPos
 � Q� � G � G2� G.� T� � �� � �� T� � �� � �� T� � �� � �� %�C�
 gnProgCopyb� N���� %�� ���z�" T� � �� � �  /C� �Z�� � T�  � �� �� �- %�C� oAriaApplication.oToolBarb� O���� ��C� � � �� � � U  THIS
 AUTOCENTER OARIAAPPLICATION SYSPATH INTERN	 CCONTCODE DEFAULTCOUNTRY LCCURPOS	 CCURRENCY
 CDATE_TYPE
 CCURRENCYI THISFORM MODULEID PROGRAMMODULEID MULTIRUN DATAPATH DATADIR
 GNPROGCOPY CAPTION PROGRAMCOPYNO OTOOLBAR INIT$ %�C� oAriaApplicationb� O��� ��C�  � �� %�C� � �
��� � T�  � �C� � �\��- %�C� oAriaApplication.oToolBarb� O��� � ��C � �  � � �� � �� T�  � �C� � �\��- %�C� oAriaApplication.oToolBarb� O��� � ��C�  � � �� � � � U  OARIAAPPLICATION WINARNG THISFORMSET FORMHASTOOLBAR	 PROCESSID NAME OTOOLBAR INIT�  ��  � �A %�C� ThisForm.Parentb� O� C� � � f� ARIAFORMSET	��� � H�Y �� � �� �� �  �	�� �
 �� � � �� �� �  �	��� �
 �� � � ��  ���� � ��C-� � �� � � U	  NKEYCODE NSHIFTALTCTRL THISFORM PARENT CLASS THISFORMSET ADDNEW DELETE UNDO.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR adjustobjectwidth,     �� QueryUnload]    �� Hide    �� Unload_    �� Init+    �� Activate�	    �� KeyPressf    �� Error�    ��1 q 4$!Q411111%d� ��� � A A � 3 A� A � AA q � Q� A A A � 1q � q A A A 2 A� A 2 A� A B� qA 5 1B��A!!�A � a a a 411�!A A �A A 2 A� 1��AA � ��A A A 3 � � �� �� � C A 2 � �1                        
     '   B
  �  H   ?   �    a   C   8    f   K   +  �  s   f   �  �  �   u   �  n  �   �   �  �  �    )   =                       ����    p  p                        Đ   %   �       '               �  U  �  ��  � T�  �CW�� F�� � � ��* %�CC� � �� � � � � � � �
�� � F�� � � �� %�C� O� ��{ � #�C� O�� � �
 F��  �� U  LCALIAS THIS PARENT ALIAS VALUE ARIACOMBOBOX1 InteractiveChange,     ��1 q � �1� A A � 1                       
      )   p                       pPROCEDURE Init
LOCAL lnCount,lcFontStyle,lcAlias
DIME THIS.FileTags[1,3]
STORE '' TO THIS.FileTags
IF !EMPTY(THIS.ARIAGRID1.RECORDSOURCE)
  lnCount = 1
  DO WHILE !EMPTY(SYS(14,lnCount,THIS.ARIAGRID1.RECORDSOURCE))
    IF !EMPTY(THIS.FileTags[1,1])
      DIME THIS.FileTags(ALEN(THIS.FileTags,1)+1,3)
    ENDIF
    IF UPPER(THISFORM.CLASS) = 'ARIAFORM'
      THIS.FileTags[ALEN(THIS.FileTags,1),1] = ;
      thisformset.TranslateToEnglish(THIS.ARIAGRID1.RECORDSOURCE,SYS(14,lnCount,THIS.ARIAGRID1.RECORDSOURCE))
    ELSE
      THIS.FileTags[ALEN(THIS.FileTags,1),1] = SYS(14,lnCount,THIS.ARIAGRID1.RECORDSOURCE)    
    ENDIF  
    THIS.FileTags[ALEN(THIS.FileTags,1),2] = TAG(lnCount,THIS.ARIAGRID1.RECORDSOURCE)
    THIS.FileTags[ALEN(THIS.FileTags,1),3] = SYS(14,lnCount,THIS.ARIAGRID1.RECORDSOURCE)
    lnCount = lnCount+1
  ENDDO
  lcAlias = SELECT()
  SELECT (THIS.ARIAGRID1.RECORDSOURCE)
  IF !EMPTY(THIS.FileTags[1,1])
    THIS.ARIAGRID1.TOP = THIS.ARIATEXTBOX1.TOP+THIS.ARIATEXTBOX1.HEIGHT+5
    THIS.ARIAGRID1.HEIGHT = this.Height - (THIS.ARIATEXTBOX1.TOP+THIS.ARIATEXTBOX1.HEIGHT+5)
    THIS.ARIACOMBOBOX1.VALUE = ORDER(THIS.ARIAGRID1.RECORDSOURCE)
    THIS.ARIACOMBOBOX1.NumberOfElements=ALEN(THIS.FileTags,1)
    THIS.ARIACOMBOBOX1.Visible=.t.
    IF !EMPTY(THIS.ARIACOMBOBOX1.VALUE)
      lcFontStyle=IIF(THIS.ARIATEXTBOX1.FONTBOLD,'B','')+;
                  IIF(THIS.ARIATEXTBOX1.FONTITALIC,'I','') 
      THIS.ARIATEXTBOX1.WIDTH = LEN(EVAL(SYS(14,VAL(SYS(21)),THIS.ARIAGRID1.RECORDSOURCE)))*FONTMETRIC(7,;
                                THIS.ARIATEXTBOX1.FONTNAME,THIS.ARIATEXTBOX1.FONTSIZE,;
                                IIF(EMPTY(lcFontStyle),'N',lcFontStyle))
      THIS.ARIATEXTBOX1.WIDTH = MIN(THIS.WIDTH-10,THIS.ARIATEXTBOX1.WIDTH)                          
      THIS.ARIATEXTBOX1.MAXLENGTH = LEN(EVAL(SYS(14,VAL(SYS(21)),THIS.ARIAGRID1.RECORDSOURCE)))
      THIS.ARIATEXTBOX1.Visible=.t.    
    ELSE  
      THIS.ARIATEXTBOX1.ENABLED=.F.        
    ENDIF  
  ELSE
    THIS.ARIACOMBOBOX1.Visible = .F. 
    THIS.ARIATEXTBOX1.Visible  = .F.
  ENDIF
  SELECT (lcAlias)
ENDIF

ENDPROC
     aPROCEDURE Init
*B803883,1 TMI [Start] Add a filter parameter
*lParameters toBrowseColumn,tnKey,lcFile
lParameters toBrowseColumn,tnKey,lcFile,lcFilter
THIS.lcFilter = lcFilter
*B803883,1 TMI [End  ] 
THIS.FILENAME = IIF(TYPE('lcFile')#'C',ALIAS(),lcFile)
IF TYPE('toBrowseColumn')<>'O'
  THIS.RELEASE
  RETURN
ENDIF
SELECT (THIS.FILENAME)
IF !EMPTY(toBrowseColumn.ControlSource)
  LOCAL lcKey
  IF ATC('.',toBrowseColumn.ControlSource)>0
    lcKey = SUBSTR(toBrowseColumn.ControlSource,ATC('.',toBrowseColumn.ControlSource)+1)
  ELSE
     lcKey = toBrowseColumn.ControlSource
  ENDIF  
  THIS.ISKEY = Upper(LEFT(KEY(),LEN(lcKey))) = UPPER(lcKey)
  THIS.FIELD = toBrowseColumn.ControlSource
  THIS.GRIDTORefresh = toBrowseColumn.PARENT
  IF TYPE('THIS.GRIDTORefresh.PARENT')='O'
    THIS.GRIDTORefresh = THIS.GRIDTORefresh.PARENT
  ENDIF
ENDIF  
IF EMPTY(THIS.FIELD)
  THIS.RELEASE
  RETURN
ENDIF

PUSH KEY
ON KEY

THIS.WIDTH = toBrowseColumn.WIDTH + 10
THIS.txtSeekValue.WIDTH = toBrowseColumn.WIDTH
THIS.txtSeekValue.MAXLENGTH = toBrowseColumn.MAXLENGTH
THIS.txtSeekValue.LEFT = 5
THIS.txtSeekValue.FORMAT = IIF(EMPTY(toBrowseColumn.FORMAT),'!',toBrowseColumn.FORMAT)
THIS.txtSeekValue.INPUTMASK = toBrowseColumn.INPUTMASK
THIS.txtSeekValue.VALUE = UPPER(CHR(tnKey))
THIS.AUTOCENTER = .T.
ENDPROC
PROCEDURE Unload
POP KEY
ENDPROC
     ����    z  z                        ��   %   y        !   �          �  U  .  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR�  ��  � � � �L %�� � � � � � � � � � � � � � � � � � � ��� � T� �	 �� ��
 �� �
 � T� � �a�� �� � T� �	 ����
 �� �
 � T� � �a�� � U  NBUTTON NSHIFT NXCOORD NYCOORD THIS LEFT TOP WIDTH HEIGHT VALUE REFRESH MOUSEINFOCUS�  ��  � � � �L %�� � � � � � � � � � � � � � � � � � � ��y � T� �	 �� ��
 �� �
 � �� � %��  �� � � 	��� � T� �	 ���� T� � �-�� �� � T� �	 �� �� � � U  NBUTTON NSHIFT NXCOORD NYCOORD THIS LEFT TOP WIDTH HEIGHT VALUE REFRESH MOUSEINFOCUS>  ��  � � � � T� � �� ��
 �� � � T� � �-�� U  NBUTTON NSHIFT NXCOORD NYCOORD THIS VALUE REFRESH MOUSEINFOCUS Error,     ��	 MouseDown�     ��	 MouseMove�    �� MouseUp�    ��1 � �2 1�� � � � � A 2 1�� � �� � A A 3 1� � 1                       ^         ~   �        �          "  �  "    )   z                        �Top = 3
Left = 217
Height = 22
Width = 22
Picture = ..\bmps\open.bmp
Alignment = 0
ToolTipText = "Select New Record"
Name = "cmdselect"
      �Top = 3
Left = 196
Height = 22
Width = 22
Picture = ..\bmps\tbnew.bmp
Alignment = 0
ToolTipText = "Add new record"
Name = "cmdadd"
     �PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
ThisForm.KeyPress(nKeyCode, nShiftAltCtrl)
ENDPROC
PROCEDURE Init
IF TYPE("ThisForm") = "O" AND UPPER(ThisForm.Class) = "ARIAFORM"
  ThisForm.AdjustObjectWidth(This)
ENDIF
ENDPROC
PROCEDURE When
This.OldValue = This.ListIndex
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
     �ButtonCount = 2
Value = 1
Height = 60
Width = 94
ariacontrolsource = ('')
Name = "ariacommandgroup"
Command1.Top = 5
Command1.Left = 5
Command1.Height = 23
Command1.Width = 84
Command1.FontName = "MS Sans Serif"
Command1.FontSize = 8
Command1.Caption = "Command1"
Command1.Name = "Command1"
Command2.Top = 31
Command2.Left = 5
Command2.Height = 23
Command2.Width = 84
Command2.FontName = "MS Sans Serif"
Command2.FontSize = 8
Command2.Caption = "Command2"
Command2.Name = "Command2"
     �ButtonCount = 2
Value = 1
Height = 46
Width = 77
transportvalueslen = 1
Name = "ariaoptiongroup"
Option1.FontName = "MS Sans Serif"
Option1.FontSize = 8
Option1.Caption = "Option1"
Option1.Value = 1
Option1.Height = 17
Option1.Left = 5
Option1.Top = 5
Option1.Width = 61
Option1.Name = "Option1"
Option2.FontName = "MS Sans Serif"
Option2.FontSize = 8
Option2.Caption = "Option2"
Option2.Height = 17
Option2.Left = 5
Option2.Top = 24
Option2.Width = 61
Option2.Name = "Option2"
     ����    �  �                        ��   %   �      e  -             �  U  W  %��  � �� � ��C�  � �� �P � u,��  � �� T�  � ��	 CMDSELECT�� B� � U  GLOBALBROWSEWINDOW MULTISELECT	 ADDSELECT BROWSETITLE KEYWASPRESS� ��  � � 5� � H� ���? �� ��+ C�  �0�9�� C�  �A�Z�� C�  �a�z�	��� �- T� �C� ARIAINCREMENTALSEARCH �  �  �N�� ��C�� � ��( �� �� � �� �  ��	��� � u,�� � �� T� � ��	 CMDSELECT��4 �� � � �  �	� � �� �  �
	��r� %�� � ��;� ��C� �	 �� �n� u,�� � �� T� �
 ��	 CMDSELECT�� B� �( ��  �y� � �	� �  �]���� ��C�� � �� � U  NKEYCODE NSHIFTALTCTRL
 OINCSEARCH THIS SHOW GLOBALBROWSEWINDOW BROWSETITLE KEYWASSELECT MULTISELECT	 ADDSELECT KEYWASPRESS MOUSEUP�  ��  � � � � H�  �� � ��  �� � �	��I � ��C� � ��& ��  �� � �	� � � 	��~ � ��CCO� � �� ��  �� � � 	��� � T� � �C� �	 ��� ��  ���� � ��C� �
 �� � U  NBUTTON NSHIFT NXCOORD NYCOORD GLOBALBROWSEWINDOW	 ADDSELECT MULTISELECT ADDREGON LASTSELECTION SELECTFIELD
 RIGHTCLICK.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR DblClick,     �� KeyPress�     �� MouseUp    �� Errorh    ��1 � � � �A A 5 � q � ���� �A� � � �A A �A 2 1� �� a� �Q� A 2 � �1                       �      	   �   |        �  H  $   *   d  �  1    )   �                        �Top = 3
Left = 167
Height = 22
Width = 22
Picture = ..\bmps\tbend.bmp
Alignment = 0
ToolTipText = "End of the file"
Name = "cmdend"
      �Top = 3
Left = 146
Height = 22
Width = 22
Picture = ..\bmps\tbnext.bmp
Alignment = 0
ToolTipText = "Next Record"
Name = "cmdnext"
     uPROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
PROCEDURE RightClick
lcBars   = "\<Small Icons,\<Large Icons"
lcStatus = "T~T,T~T"
lnBar  = ThisFormSet.AriaForm1.AriaShortCut1.ShowShortCut(This , lcBars , lcStatus)
 DO CASE
   CASE lnBar = 1

    CASE lnBar = 2

    CASE lnBar = 3
ENDCASE  


ENDPROC
     �PROCEDURE Click
SELECT (THIS.Parent.FileToFilter)
IF !EMPTY(THIS.PARENT.FILTERARRAY)
  Local lcArrayName
  lcArrayName = THIS.PARENT.FILTERARRAY
  DIME &lcArrayName[ALEN(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,1),7]
  =ACOPY(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,&lcArrayName)
  lcFilter = GFGENFLT('_SCREEN.ACTIVEFORM.ARIASEARCHCONTAINER1.AFILTEREXPR',.T.)
  IF EMPTY(lcFilter)
      lcFilter = this.PARENT.FileFilter
      SET FILTER TO &
  ELSE
    IF !EMPTY(this.PARENT.FileFilter)
     lcFilter = this.PARENT.FileFilter +' .AND. '+lcFilter
    ENDIF
  ENDIF
  SET FILTER TO &lcfilter  
ELSE
  lcFilter = GFGENFLT('_SCREEN.ACTIVEFORM.ARIASEARCHCONTAINER1.AFILTEREXPR',.T.)
  IF !EMPTY(lcFilter)
    GO TOP
    IF !EMPTY(this.parent.key) AND TYPE('this.parent.key')='C'
      =SEEK(EVAL(this.parent.key))
    ENDIF
    LOCATE REST FOR &lcFilter
    IF !FOUND()
      =MESSAGEBOX('Can not find the search expression')
    ENDIF
  ENDIF
ENDIF
THISFORM.RELEASE
ENDPROC
     ���    �  �                           %         z     V          �  U  � U %�C�  � �
� C� THIS.ControlSourceb� C	� C� THIS.TransportValuesb� C	��� � T�  � ��  � �� T�  � ��  �� T�  � �CC�  � �>�� � U  THIS CONTROLSOURCE ARIACONTROLSOURCE TRANSPORTVALUESLEN� X %�C� THIS.TransportValuesb� L�	 C�  � �	�! C� THIS.AriaControlSourceb� C	��~ �& T�  � �CCC�  � ��  � ��  � w�� � U  THIS CONTROLSOURCE VALUE ARIACONTROLSOURCE TRANSPORTVALUES TRANSPORTVALUESLEN� X %�C� THIS.TransportValuesb� L�	 C�  � �	�! C� THIS.AriaControlSourceb� C	��| �$ >��  � ���C�  � �  � �  � \�� � U  THIS CONTROLSOURCE ARIACONTROLSOURCE TRANSPORTVALUES VALUE TRANSPORTVALUESLEN.  ��  � � � ��C �   �  � � � � �� U  NERROR CMETHOD NLINE THIS PARENT ERROR Init,     �� Refresh    �� Valid�    �� Error�    ��1 R1qA 2 �aA 2 �AA 2 � �1                               #     	        �          c      )   �                       ����    �  �                        x   %   3      w     U          �  U  � + %�C�  � � � ��  � � � Like��` � T�  � �-�� T�  � � � �a�� ��C�  � � � �� �{ � ��C�  � � �	 �� � U
  THIS PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID VISIBLE CBXEXPRRIGHT WHEN TXTTO SETFOCUS� . %�C�  � � � ��  � � � Betweem��l �+ T�  � � ��  � � � ������  � �� ��C��  � � �� �� �= T�  � � ��  � � � ������  � � ,�  � � � �� ��C��  � � �� � U  THIS PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID VALUE BUILDBAR TXTTO	 LostFocus,     �� Valid    ��1 �� A1� 1A 2 ��1� �1A 1                       �      	     _  
    )   �                       g���    N  N                        �   %   �      �     �          �  U  c %�C�  � �
��\� T�  � � � �� Like�� T�  � � � ��  �� T�  � � � �-��8 T�  � � ��  � � � �����C�  �
 ��  � �	 ��8 T�  � � ��  � � � �����C�  �
 ��  � �	 ��( T�  � � ��  � � � ������  �� ��C�  � � �� T�  � � �-�� ��C��  � � �� ��C��  � � �� ��C��  � � �� ��C��  � � �� � U  THIS VALUE PARENT CBXEXPROPERATOR CBXEXPRRIGHT CHKIS AFILTEREXPR LSTEXPRESSION
 LISTITEMID AAVAILABLEFIELDS	 LISTINDEX SKIPOPERATOR ADDEDNEWBAR BUILDBAR�  %��  � � ��� � %��  � � � ���o � T�  � � � ��  �� T�  � � � ��  �� T�  � � � ��  �� � ��C�  � � �
 �  � � �	 �� ��C�  � � �� T�  � � �-�� ��C�  � � � �� � U  THIS PARENT ADDEDNEWBAR LSTEXPRESSION	 LISTCOUNT CBXEXPRLEFT CONTROLSOURCE CBXEXPROPERATOR CHKIS
 REMOVEITEM	 LISTINDEX ARRANGE REFRESH Valid,     ��	 LostFocus8    ��1 1�aA���1111A 2 1�aaaA �1A 1                       z        �  '      )   N                       =���    $  $                        /�   %   C      �  �   �          �  U  U  %��  � �  � ��N � �� � T� ��  � �� ��C�  � � �� T�  � �� �� � U  THIS	 LISTINDEX
 LISTITEMID LNLISTINDEX PARENT ARRANGEO  ��  � � � � %��  �� � � � 	��H �	 ��C��� ��C� � �� � U  NBUTTON NSHIFT NXCOORD NYCOORD THIS	 LISTCOUNT
 RIGHTCLICK� ��C�  � � � �� %��  � � ����% %�C�  � ��  � � � .OR.��� � T�  � � � �-�� T�  � � � �-�� T�  � �	 � �-�� T�  � �
 � �-�� B� �� � T�  � � � �a�� T�  � � � �a�� T�  � �	 � �a�� T�  � �
 � �a�� � ��C�  � � ��" %�C�  � ��  � � � V���� �� � � T� ��  � � �� H�Y���" �C�  � ��  � � � L���� T� �� Yes,.T.,No,.F.��" �C�  � ��  � � � F����> T� �C� GetC�  � ��  � � � field� Validentries� �� � %�C� �����y DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT  'Value '+THIS.PARENT.aFilterExpr[THIS.listItemID,6]
 ��� T�  � � � �� �� T�  � � � �� ��' %�C� ,C�  � ��  � � �� ����H T�  � � � �CC�  � ��  � � �C� ,C�  � ��  � � ��\��E T�  � � � �CC�  � ��  � � C� ,C�  � ��  � � ��\��� DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT  'Value '+THIS.Parent.cbxFrom.DisplayValue+','+THIS.Parent.cbxTo.DisplayValue
 ���& T�  � � � �C�  � ��  � � ��o DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT  'Value '+THIS.Parent.cbxFrom.DisplayValue
 � �! T�  � � � �C�  � � ���� ��� �� � � T� ��  � � ��M DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT  'Value'
) T� �C�  � � C�  � ��  � � ��� T� �C�  � � � ���� T�  � � � �� �� ��C�  � � � �� � � U  THIS PARENT REFRESH	 LISTCOUNT AFILTEREXPR
 LISTITEMID CBXEXPRRIGHT ENABLED CBXEXPRLEFT CBXEXPROPERATOR CHKIS SKIPOPERATOR	 LCPOPNAME LCVALIDENTRIES
 VALUEPOPUP GFDATABASEPROP CBXFROM	 ROWSOURCE CBXTO VALUE	 LISTINDEX AFIELDVALUES LNBARPOS7
 ��  � �n T� �� TC� � � � � T� � F6� TC� � � � � T� � F6� TC� � � � � T� � F6� T��O T�  �C � �- \<Add,\<Or,\<Insert,\<Remove,\-,\<Save,\<Load � � � � � �� H�� �0
� ��  ����� �� �	 � T� �� � ��� ��C�   � � �
 �� ��C� Is � �� � �� ��C� Like � �� � �� ��C�   � �� � �� � � � �� ����� T� � � �� ������  �� T� � � �� ������  �� T� � � �� ������  �� T� � � �� �����-��# T� � � �� ������ Like�� T� � � �� ������  ��  T� � � �� ������ V�� ��C� ENABLEDa� � � �� T� � �� � �� T� � � �a��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]�� ��C� � � � �� \��
 {SPACEBAR}�� ��  ����� �� �	 � T� �C� � �D�� ��C�   � � �
 �� %�CC��� � � �
��H�( � � � �C� � � �������� �. T� � � �C� � � �������� .OR.��* T� � � �C� � � ��������  ��* T� � � �C� � � ��������  ��( T� � � �C� � � �������-��* T� � � �C� � � ��������  ��* T� � � �C� � � ��������  ��* T� � � �C� � � ��������  �� ��C� ENABLEDa� � � �� T� � � �-��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]�� ��C� � �� ��  ���
� �� �	 � T� �C� � �D�� ��C�   � � �
 �� ��C� Is � �� � �� ��C� Like � �� � �� ��C�   � �� � �� T� � �� �� %�CC��� � � �
����( � � � �C� � � �������� �* T� � � �C� � � ��������  ��* T� � � �C� � � ��������  ��* T� � � �C� � � ��������  ��( T� � � �C� � � �������-��. T� � � �C� � � �������� Like��* T� � � �C� � � ��������  ��+ T� � � �C� � � �������� V�� ��C� ENABLEDa� � � �� T� � � �a��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]��U T� � � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]�� ��C� � � � �� \��
 {SPACEBAR}�� ��  ���0
� ��C� � � �� � U  LNCHOICE
 LCENABLING THIS	 LISTCOUNT	 LISTINDEX PARENT FILTERSHORTCUT SHOWSHORTCUT LNBARPOS	 LCCAPTION ADDITEM ADDLISTITEM AFILTEREXPR SETALL ADDEDNEWBAR CBXEXPRLEFT CONTROLSOURCE CBXEXPROPERATOR CHKIS SETFOCUS INTERACTIVECHANGE
 REMOVELINE InteractiveChange,     ��	 MouseDown�     �� WhenS    ��
 RightClick    ��1 qq A 2 1�� � A 2 1AQAAAAA � AAAAA !� 1� !�!�A � �� aaq�Q"	� a�A A � � 1���a1A A 2 � ��� � AA��q�����1��1RQQ21� QQ��A ��������RQQ� � QA��q��A ��������RQQ21A 2                       �         �   Q  	      l  ?     @   `  �  F    )   $                       ����    �  �                        ��   %         J     *          �  U  l" %�C�  � ��  � � � V��� �( %�C�  � � � ��  � � � V��Z � ��C�  � � �� �� �) T�  � � ��  � � � ������ V��( T�  � � ��  � � � ������  �� ��C�  � � �� � �R�8 T�  � � ��  � � � �����C�  � ��  � � ��8 T�  � � ��  � � � �����C�  � ��  � � �� ��C�  � � �	 �� � ��C��  � �
 �� U  THIS PARENT AFIELDVALUES	 LISTINDEX AFILTEREXPR LSTEXPRESSION
 LISTITEMID GETVALUE AAVAILABLEFIELDS SETFOCUS BUILDBAR Valid,     ��1 !�� ��A � ��1A 11                       �      )   �                       ����    �  �                        x   %   3      w     U          �  U  � . %�C�  � � � ��  � � � Between��l �+ T�  � � ��  � � � ������  � �� ��C��  � � �� �� �= T�  � � ��  � � � ������  � � ,�  � � � �� ��C��  � � �� � U  THIS PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID VALUE BUILDBAR CBXTO� + %�C�  � � � ��  � � � Like��` � T�  � �-�� T�  � � � �a�� ��C�  � � � �� �{ � ��C�  � � �	 �� � U
  THIS PARENT AFILTEREXPR LSTEXPRESSION
 LISTITEMID VISIBLE CBXEXPRRIGHT WHEN CBXTO SETFOCUS Valid,     ��	 LostFocusI    ��1 ��1� �1A 2 �� A1� 1A 1                       d     	   �  _      )   �                       ����    i  i                        �   %   �                      �  U  C F��  � � �� %�C�  � � �
��2� �  � � � ������� J��  �(�  � � � � �� � T� ��  � � ��M DIME &lcArrayName[ALEN(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,1),7]
E =ACOPY(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,&lcArrayName)
 T� ��  � � �� SET FILTER TO &lcFilter
 �
 ��	 �
 � U  THIS PARENT FILETOFILTER FILTERARRAY ARIASEARCHCONTAINER1 AFILTEREXPR LCARRAYNAME LCFILTER
 FILEFILTER THISFORM RELEASE Click,     ��1 a�aq 1�Q1�A � 1                       �      )   i                       ���    �  �                        RP   %   _      �      n          �  U  � F��  � � �� %�C�  � � �
���� �� � T� ��  � � ��M DIME &lcArrayName[ALEN(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,1),7]
E =ACOPY(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,&lcArrayName)
E T� �C�3 _SCREEN.ACTIVEFORM.ARIASEARCHCONTAINER1.AFILTEREXPRa� �� %�C� ���?� T� ��  � � �� G(� ��� %�C�  � � �
���" T� ��  � � �  .AND. � �� � � SET FILTER TO &lcfilter  
 ���E T� �C�3 _SCREEN.ACTIVEFORM.ARIASEARCHCONTAINER1.AFILTEREXPRa� �� %�C� �
���� #)�3 %�C�  � � �
� C� this.parent.keyb� C	��L� ��CC�  � � Ί�� � LOCATE REST FOR &lcFilter
 %�C4
����. ��C�" Can not find the search expression�x�� � � �
 ��	 �
 � U  THIS PARENT FILETOFILTER FILTERARRAY LCARRAYNAME LCFILTER GFGENFLT
 FILEFILTER KEY THISFORM RELEASE Click,     ��1 aq 1�QQ� 1a � a!A A �� QQ 11A �� �A A A � 1                       �      )   �                       ���                              ��   %   ^      �     }          �  U  �  ��  � � �' T� �CC� tcFileb� C� � � C6�� T� � �� �� T� � ��  �� %�C�� ��� � T� � � ��
 Set Filter�� T� � �	 �a�� T� �
 �� �� �� � T� � � �� Find�� T� � �	 �-�� � T� � �� � � �� U  TAARRAYTOFILTER TCFILTER TCFILE THIS FILETOFILTER FILTERARRAY	 CMDFILTER CAPTION CMDCLEARFILTER VISIBLE
 FILEFILTERo  %�CC���  � � ���> � T�  � � �-�� T�  � � �-�� �h � T�  � � �a�� T�  � � �a�� � U  THIS ARIASEARCHCONTAINER1 AFILTEREXPR	 CMDFILTER ENABLED CMDCLEARFILTER Init,     �� Refresh�    ��1 � q�� qA a2 �� A 2                       �        �  �      )                          D���    +  +                        g$   %   �      �     �          �  U   ��  � � %��  ���� T� �� �� �� ���(�� � �� � %�C � � � ��{ � T� �� ��� ��C� � � ��� � �� %�C� � ��� ���� � � � ����C� � ���� J��  �(� � � � �* � � �C� � ��� ��C� � ���� � ��C� �	 �� � U
  NKEYCODE NSHIFTALTCTRL	 LNDELETED LNCNT THIS	 LISTCOUNT SELECTED THISFORMSET LATO REQUERY KeyPress,     ��1 � � �Q!A A ��� �A � A 1                       .      )   +                       ����    q  q                        �   %   �      (     
          �  U  9 ��  ���(�� � � ��2� %�C �  � � � ��.� T� � � ��  �� T� �� � � �� %�CC��� � �
��� �* � � �C� � �����C� � ���� �* T� � �C� � �������� � �	 ��* T� � �C� � �������� � �
 ��' T� � �C� � �������� � �� ��C� � � �� � �� U  LNCNT THISFORM
 LSCONTACTS	 LISTCOUNT SELECTED	 LISTINDEX LNINDEX THISFORMSET LACC DISPLAYVALUE VALUE MTADBOOK	 CBOOKCOMP LSCC REQUERY Click,     ��1 ��11��A ��qA A 5                       �      )   q                       ����    q  q                        �   %   �      (     
          �  U  9 ��  ���(�� � � ��2� %�C �  � � � ��.� T� � � ��  �� T� �� � � �� %�CC��� � �
��� �* � � �C� � �����C� � ���� �* T� � �C� � �������� � �	 ��* T� � �C� � �������� � �
 ��' T� � �C� � �������� � �� ��C� � � �� � �� U  LNCNT THISFORM
 LSCONTACTS	 LISTCOUNT SELECTED	 LISTINDEX LNINDEX THISFORMSET LATO DISPLAYVALUE VALUE MTADBOOK	 CBOOKCOMP LSTO REQUERY Click,     ��1 ��11��A ��qA A 5                       �      )   q                       D���    +  +                        g$   %   �      �     �          �  U   ��  � � %��  ���� T� �� �� �� ���(�� � �� � %�C � � � ��{ � T� �� ��� ��C� � � ��� � �� %�C� � ��� ���� � � � ����C� � ���� J��  �(� � � � �* � � �C� � ��� ��C� � ���� � ��C� �	 �� � U
  NKEYCODE NSHIFTALTCTRL	 LNDELETED LNCNT THIS	 LISTCOUNT SELECTED THISFORMSET LACC REQUERY KeyPress,     ��1 � � �Q!A A ��� �A � A 1                       .      )   +                       y���    `  `                        OY   %   �           �          �  U  @ T�  ��  �� T� ��  �� �� ���(�C� � ����� � %�CC � �� � �
��� �Y T�  ��  CC�  �� �  � � |6C � �� � � ~C � �� � � ~C � �� � ��3 T� �� CC� �� �  � � ;6CC � �� � ��� � �� T� � � �� �� T� � � ��  �� T�  ��  �� T� ��  �� �� ���(�C� � ������ %�CC � �� � �
����Y T�  ��  CC�  �� �  � � |6C � �� � � ~C � �� � � ~C � �� � ��3 T� �� CC� �� �  � � ;6CC � �� � ��� � �� T� �	 � ��  �� T� �	 � �� �� T� �	 �
 �
 � �a��
 �� � � U  LCSENDTO LCNAMES LNCOUNT THISFORMSET LATO OTO VALUE TAG LACC OCC PARENT
 WASCHANGED RELEASE Click,     ��1 � � ���1A A 11� � ���1A A 11q� 1                       �      )   `                       ����    �  �                        �q   %   �      5               �  U  �  %��  � ��g �	 ��C���1 %�C�  � �
�
 C�  � �
	� � � �  � 	��c � T� � � � ��  � �� � �� �	 ��C��� T� � � � �-�� G((�� � ��# %�C� THISFORM.browseb� O��� � ��C�  �	 �
 �� � � U  THISFORM FIND	 INITORDER KEY THIS VALUE PARENT ARIATEXTBOX1 VISIBLE BROWSE REFRESHC 	 ��C���# %�C� THISFORM.browseb� O��< � ��C�  � � �� � U  THISFORM BROWSE REFRESH Ariacombobox1.Valid,     �� Ariatextbox1.InteractiveChangef    ��1 � �A � � A� 1A A 2 � 1A 1                       �        �  	      )   �                        �Height = 22
Left = 47
Margin = 0
ReadOnly = .T.
Top = 3
Visible = .F.
Width = 100
Style = 1
DisabledBackColor = 255,255,255
Name = "lcKey"
      PROCEDURE Click
SELECT (THIS.Parent.FileToFilter)
IF !EMPTY(THIS.PARENT.FILTERARRAY)
  DIME THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr[1,7]
  STORE '' TO THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr
  Local lcArrayName
  lcArrayName = THIS.PARENT.FILTERARRAY
  DIME &lcArrayName[ALEN(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,1),7]
  =ACOPY(THIS.PARENT.ARIASEARCHCONTAINER1.aFilterExpr,&lcArrayName)
  lcFilter = this.Parent.FileFilter
  SET FILTER TO &lcFilter
ENDIF
THISFORM.RELEASE
ENDPROC
     ����    �  �                        ��   %   �      M  .   �          �  U    B��  � �� U  THIS URETVAL' ��  � � � T� � ��  �� T� � �� ��) � � ������� � ������� J��  �(� � � � �# � ������� ������� J��  �(� � � %�C� � �	 �
���� ��C� � �	 � � |�
 �� � � �C�� ������� �� ���(�C�� ������ � ������� ��CC � �� � � ~�
 ��# T� � �� �����C��� ��# T� � �� �����C��� ��# T� � �� �����C��� �� �� � %�C� � �	 �
���� ��C� � �	 � � |�
 �� � � �C�� ������� �� ���(�C�� ������ � ������� ��CC � �� � � ~�
 ��# T� � �� �����C��� ��# T� � �� �����C��� ��# T� � �� �����C��� �� �� � ��C� � � � �� ��C� � � � �� %�� ���� T� � � � �a�� � � T� � � � �a�� � U  LOTO LOCC LNACTIVE THIS OTO OCC THISFORMSET LATO LACC TAG GFSUBSTR LNCOUNT LAVAL	 ARIAFORM1 LSTO REQUERY LSCC CMDTO DEFAULT CMDCCN  F�  � G(�� C�
X�� ��C� � � �� T� � � ���� ��C� � � �� U	  MTCONTCT	 CBOOKCOMP THISFORM
 LSCONTACTS REQUERY THIS ARIACOMBOBOX1	 LISTINDEX VALID Unload,     �� InitN     �� Ariaform1.Init    ��1 � 2 � �q1a���1�111A A a���1�111A A 21A� AA 2 q 12                       =         X   }     (   �  5  ,    )   �                       wPROCEDURE Click
THIS.VALUE=0
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .T. 
ENDFOR  
*WAM
This.Parent.oWindParent.GoEnd()
THIS.Parent.NavRefresh()
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F. 
ENDFOR  
*WAM
ENDPROC
     �PROCEDURE Valid
IF THIS.Value = 'Like'
   IF ATC(',',this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,6])>0
     this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,6] = ;
     SUBSTR(this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,6],1,;
     ATC(',',this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,6])-1)
     this.parent.BuildBar(4)
   ENDIF
ENDIF
this.parent.BuildBar(3)
ENDPROC
     jPROCEDURE Valid
IF this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,5] <> 'Between'
  this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,6]=this.value
  THIS.PARENT.BUILDBAR(4)
ELSE
  this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,6]=this.value+','+;
  this.parent.cbxto.value
  THIS.PARENT.BUILDBAR(4)  
ENDIF  
ENDPROC
PROCEDURE LostFocus
IF this.parent.aFilterExpr[this.parent.lstExpression.Listitemid,5] ='Like'
  THIS.Visible = .F.
  THIS.PARENT.cbxExprRight.Visible = .t.
  this.parent.lstExpression.WHEN()
ELSE
  this.parent.cbxto.setfocus()
ENDIF
ENDPROC
     �PROCEDURE Init
lParameters taArrayToFilter,tcFilter,tcFile
tcFile = IIF(TYPE('tcFile')='C',tcFile,ALIAS())
THIS.FileToFilter = tcFile
THIS.FilterArray=taArrayToFilter
IF PARAMETERS()>0
  THIS.cmdFilter .Caption='Set Filter'
  this.cmdClearFilter.Visible = .T.
  this.FILEFILTER = tcFilter
ELSE
  THIS.cmdFilter .Caption='Find'
  this.cmdClearFilter.Visible = .F.  
ENDIF
THIS.Caption = THIS.cmdFilter .Caption
ENDPROC
PROCEDURE Refresh
IF EMPTY(this.AriaSearchContainer1.aFilterExpr[1,1])
  THIS.cmdFilter.Enabled = .F.    
  THIS.cmdClearFilter.Enabled = .F.
ELSE  
  THIS.cmdFilter.Enabled = .T.    
  THIS.cmdClearFilter.Enabled = .T.
ENDIF  

ENDPROC
     �PROCEDURE Click
LPARAMETERS tlSaveWithoutAsk
THIS.VALUE=0
IF THIS.Parent.ActiveMode $ 'AE'
  This.Parent.oWindParent.Undo(tlSaveWithoutAsk)
  This.Parent.oWindParent.FORMS(1).LockScreen = .T.
  *THISFORM.LockScreen = .T.
  THIS.Parent.ButtonRefresh()
  THIS.Parent.NavRefresh()
  *THISFORM.LockScreen = .F.  
  This.Parent.oWindParent.FORMS(1).LockScreen = .F.
ELSE
  This.Parent.oWindParent.Close()
  THIS.PARENT.VISIBLE = OAriaApplication.UserUseToolbar  
ENDIF

ENDPROC
     @PROCEDURE Unload
*ARIAFORMSET::UNLOAD()
RETURN THIS.uRetVal
ENDPROC
PROCEDURE Init
lParameters loTo,loCc,lnActive
THIS.oTo = loTo
THIS.oCc = loCc
DIME THISFORMSET.laTo[1,3],THISFORMSET.laCc[1,3]
STORE '' TO THISFORMSET.laTo,THISFORMSET.laCc
DIME laTo[1,1],laCc[1,1]
STORE '' TO laTo,laCc
IF !EMPTY(THIS.OTO.Tag)
  =gfSubStr(THIS.OTO.Tag,@laTo,'|')
  DIMEN THIS.laTo[ALEN(laTo,1),3]
  FOR lnCount = 1 TO ALEN(laTo,1)
    DIMEN laVal[1,1]
    =gfSubStr(laTo[lnCount,1],@laVal,'~')
    THIS.laTo[lnCount,1]= laVal[1,1]
    THIS.laTo[lnCount,2]= laVal[2,1]    
    THIS.laTo[lnCount,3]= laVal[3,1]    
  ENDFOR  
ENDIF  
IF !EMPTY(THIS.OCc.Tag)  
  =gfSubStr(THIS.OCc.Tag,@laCc,'|')
  DIMEN THIS.laCc[ALEN(laCc,1),3]
  FOR lnCount = 1 TO ALEN(laCc,1)
    DIMEN laVal[1,1]
    =gfSubStr(laCc[lnCount,1],@laVal,'~')
    THIS.laCc[lnCount,1]= laVal[1,1]
    THIS.laCc[lnCount,2]= laVal[2,1]    
    THIS.laCc[lnCount,3]= laVal[3,1]    
  ENDFOR  
ENDIF 

THIS.ARIAFORM1.lsTo.Requery()
THIS.ARIAFORM1.lsCc.Requery()
IF lnActive = 1
  THIS.ARIAFORM1.CMDTO.DEFAULT = .T.
ELSE
  THIS.ARIAFORM1.CMDCc.DEFAULT = .T.
ENDIF
ENDPROC
PROCEDURE Ariaform1.Init
SELECT MTCONTCT
SET FILTER TO CBOOKCOMP = SPACE(10)
THISFORM.lsContacts.REQUERY()
THIS.AriaComboBox1.ListIndex = 1
THIS.AriaComboBox1.Valid()

ENDPROC
     ����    �  �                        1�   %         ;     %          �  U  A ��  � � T� �CW�� F�� � � � ��A T�  �C� � � � � � B� �  6C� � � � � � I� �  6��] T� � � �	 �CCC� � �� � �
 �>C�� � � � � � � � CC�  �� � N� �  6�$��. T� � � �	 �C� � �	 �
� � � �	 F��* T� � � � �CCC� � �� � �
 �>�� T� � � � �a��
 F�� �� U  LCFONTSTYLE LCALIAS THIS PARENT	 ARIAGRID1 RECORDSOURCE ARIATEXTBOX1 FONTBOLD
 FONTITALIC WIDTH FILETAGS	 LISTINDEX FONTNAME FONTSIZE	 MAXLENGTH ENABLED Valid,     ��1 � � 1���A� 1                       �      )   �                       mPROCEDURE Click
GLOBALBROWSEWINDOW.AddSelect()
ENDPROC
PROCEDURE MouseUp
LPARAMETERS nButton, nShift, nXCoord, nYCoord
DO CASE 
 CASE nButton = 1 AND nShift = 2
  GLOBALBROWSEWINDOW.AddSelect()
 CASE nButton = 1 AND nShift = 1 AND GLOBALBROWSEWINDOW.MultiSelect
    GLOBALBROWSEWINDOW.AddRegon(RECNO())
 CASE nButton = 1 AND GLOBALBROWSEWINDOW.MultiSelect
    GLOBALBROWSEWINDOW.LastSelection = EVAL(GLOBALBROWSEWINDOW.SelectField)
 CASE nButton = 2
  GLOBALBROWSEWINDOW.RightClick()  
ENDCASE
ENDPROC
PROCEDURE Error
LPARAMETERS nerror,cmethod,nline
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
     �PROCEDURE Valid
IF THIS.Parent.aFieldValues[this.listindex,2]='V'
  IF this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,7] = 'V'
    THIS.parent.GetValue()    
  ELSE
    this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,7] = 'V'
    this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,6] = ''  
    THIS.Parent.GetValue()    
  ENDIF  
ELSE
  this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,7] = this.parent.aAvailableFields[this.listIndex,4]
  this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,6] = this.parent.aAvailableFields[this.listIndex,2]  
  This.Parent.LstExpression.SetFocus()  
ENDIF  
this.parent.BuildBar(4)
ENDPROC
     �PROCEDURE Click
lcSendTo = ''
lcNames = ''
FOR lnCount = 1 TO ALEN(THISFORMSET.laTo,1)
  IF !EMPTY(THISFORMSET.laTo[lnCount,2])
    lcSendTo = lcSendTo+IIF(EMPTY(lcSendTo),'','|')+THISFORMSET.laTo[lnCount,1]+'~'+;
               THISFORMSET.laTo[lnCount,2]+'~'+THISFORMSET.laTo[lnCount,3]
    lcNames = lcNames + IIF(EMPTY(lcNames),'',';')+ALLT(THISFORMSET.laTo[lnCount,1])
  ENDIF             
ENDFOR
THISFORMSET.oTo.Value = lcNames
THISFORMSET.oTo.Tag = lcSendTo
lcSendTo = ''
lcNames = ''
FOR lnCount = 1 TO ALEN(THISFORMSET.laCc,1)
  IF !EMPTY(THISFORMSET.laCc[lnCount,2])
    lcSendTo = lcSendTo+IIF(EMPTY(lcSendTo),'','|')+THISFORMSET.laCc[lnCount,1]+'~'+;
               THISFORMSET.laCc[lnCount,2]+'~'+THISFORMSET.laCc[lnCount,3]
    lcNames = lcNames + IIF(EMPTY(lcNames),'',';')+ALLT(THISFORMSET.laCc[lnCount,1])
  ENDIF           
ENDFOR
THISFORMSET.oCc.Tag = lcSendTo
THISFORMSET.oCc.Value = lcNames
THISFORMSET.oCc.Parent.Parent.WasChanged = .t.
thisformset.release
ENDPROC
     2PROCEDURE Valid
IF !EMPTY(THIS.Value)
  this.parent.cbxExprOperator.Value='Like'
  this.parent.cbxExprRight.Value=''
  this.parent.chkIs.Value = .F.
  this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,3] = this.parent.aAvailableFields[this.listIndex,3]
  this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,2] = this.parent.aAvailableFields[this.listIndex,4]
  this.Parent.aFilterExpr[THIS.parent.lstExpression.ListItemID,6] = ''
  THIS.PARENT.SKIPOperator()
  THIS.Parent.AddedNewBar = .F.
  this.parent.BuildBar(1)
  this.parent.BuildBar(2)
  this.parent.BuildBar(3)
  this.parent.BuildBar(4)
ENDIF
ENDPROC
PROCEDURE LostFocus
IF THIS.Parent.AddedNewBar
  IF THIS.PARENT.lstExpression.LISTCOUNT = 1
    THIS.Parent.cbxExprLeft.ControlSource = ''
    THIS.Parent.cbxExprOperator.ControlSource = ''
    THIS.Parent.chkIs.ControlSource = ''
  ENDIF
  THIS.PARENT.lstExpression.RemoveItem(THIS.PARENT.lstExpression.ListIndex)
  THIS.PARENT.Arrange()
  THIS.Parent.AddedNewBar = .F.
  THIS.PARENT.PARENT.REFRESH()
ENDIF
ENDPROC
     ���    �  �                        ��   %   Y      �     t          �  U  I %�C�  � f� ARIAFORM��g �% %�� � � � � � � � 	��c � %�C� � � 
��_ � B�-�� � � � ��	 � T�	 �CW�� F��
 � � � ��- %�CC�
 � ��
 � � � �
 � � � �
��� � F��
 � � � �� %�C� O� ��� � #�C� O�� � �
 F��	 �� T�
 � � �a�� T�
 � � � �-�� ��C�
 � � � �� ��C� � �� U  THISFORM CLASS THISFORMSET EDITMODE ADDMODE FORMHASCONTROLPANEL OARIAAPPLICATION OTOOLBAR CANCONTINUE LCALIAS THIS PARENT	 ARIAGRID1 RECORDSOURCE VALUE ARIACOMBOBOX1 FROMSEEK	 HIGHLIGHT SETFOCUS
 REFRESHALL InteractiveChange,     ��1 �QQq A A A q � 1�11� A A � A1� 1                       �      )   �                        �Top = 3
Left = 26
Height = 22
Width = 22
Picture = ..\bmps\tbback.bmp
Alignment = 0
ToolTipText = "Previous Record"
Name = "cmdprev"
     �PROCEDURE translatetoenglish
lParameters tcAlias,tcExpression,tcSeperator
LOCAL lnCount,laExpArray
tcSeperator = IIF(PCOUNT()<3,'+',tcSeperator)
DIME laExpArray[1,1]
STORE '' to laExpArray
=gfSubStr(tcExpression,@laExpArray,tcSeperator)
tcExpression = ''
FOR lnCount = 1 TO ALEN(laExpArray,1)
  lcEngName = THIS.DBGetProp(tcAlias+'.'+ALLTRIM(laExpArray[lnCount,1]),'FIELD','CAPTION')
  IF !EMPTY(lcEngName)
    laExpArray[lnCount,1] = ALLTRIM(lcEngName)
  ENDIF
  tcExpression = tcExpression+IIF(!EMPTY(tcExpression),tcSeperator,'')+laExpArray[lnCount,1]
ENDFOR
RETURN tcExpression
ENDPROC
PROCEDURE dbgetprop
lParameters tcName,tcType,tcProperty
LOCAL lnCount,lcAlias,lcFieldName,lcTableName,lcDataBase,lcPath,lcRetrunValue
tcName = UPPER(tcName)
tcType = UPPER(tcType)
tcProperty = UPPER(tcProperty)

IF tcType='FIELD'
  IF ATC('.',tcName)>0
    lcAlias = SUBSTR(tcName,1,ATC('.',tcName)-1)
    lcFieldName = SUBSTR(tcName,ATC('.',tcName)+1)
    lcTableName = FULL(DBF(lcAlias))
    IF ATC('\',lcTableName)>0
      lcTableName = SUBSTR(lcTableName,RAT('\',lcTableName)+1)
    ENDIF  
    lcTableName = STRTRAN(lcTableName,'.DBF','')
    tcName = lcTableName+'.'+lcFieldName
  ENDIF
ENDIF

DO CASE
  CASE tcType = 'DATABASE'
    IF DBUSED(tcName)
      SET DATABASE TO (tcName)
    ELSE
      RETURN tcName  
    ENDIF
  CASE tcType $ "FIELD,TABLE,VIEW"
    IF tcType = 'FIELD'
      lcDataBase = CURSORGETPROP("DATABASE",lcAlias)
     ELSE 
       lcDataBase = CURSORGETPROP("DATABASE",tcName)
     ENDIF 
     lcPath =''
     IF ATC('\',lcDataBase)>0
       lcPath = SUBSTR(lcDataBase,1,RAT('\',lcDataBase))
       lcDataBase = STRTRAN(lcDataBase,lcPath,'')
       lcDataBase = STRTRAN(lcDataBase,'.DBC','')
     ENDIF
     IF !EMPTY(lcDataBase) AND !DBUSED(lcDataBase)
       OPEN DATABASE (lcPath+lcDataBase)  
     ENDIF
     IF !EMPTY(lcDataBase)
       SET DATABASE TO (lcDataBase)     
     ENDIF  
ENDCASE
IF !EMPTY(lcDataBase)
  lcRetrunValue = DBGETPROP(tcName,tcType,tcProperty)
ELSE
  lcRetrunValue = tcName
ENDIF  
RETURN lcRetrunValue
ENDPROC
PROCEDURE Init
LParameters tcAlias,lorder
DIME THIS.FileTags(1,3)
STORE '' TO THIS.FileTags
IF !lorder
  THIS.FileTags[ALEN(THIS.FileTags,1),1] = 'No Order'
  THIS.FileTags[ALEN(THIS.FileTags,1),2] = ''
  THIS.FileTags[ALEN(THIS.FileTags,1),3] = ''
ENDIF

IF PARAMETERS()>0
  this.Alias = tcAlias
ELSE
  this.Alias = ALIAS()
ENDIF
IF !EMPTY(THIS.ALIAS)  
  lnCount = 1
  DO WHILE !EMPTY(SYS(14,lnCount,THIS.ALIAS))
    IF !EMPTY(THIS.FileTags[1,1])
      DIME THIS.FileTags(ALEN(THIS.FileTags,1)+1,3)
    ENDIF
    THIS.FileTags[ALEN(THIS.FileTags,1),1] = ;
    this.TranslateToEnglish(THIS.ALIAS,SYS(14,lnCount,THIS.ALIAS))
    THIS.FileTags[ALEN(THIS.FileTags,1),2] = TAG(lnCount,THIS.ALIAS)
    THIS.FileTags[ALEN(THIS.FileTags,1),3] = SYS(14,lnCount,THIS.ALIAS)
    lnCount = lnCount+1
  ENDDO

  lcAlias = SELECT()
  SELECT (THIS.ALIAS)
  IF !EMPTY(THIS.FileTags[1,1])
    THIS.ARIACOMBOBOX1.VALUE = ORDER(THIS.ALIAS)
    THIS.ARIACOMBOBOX1.NumberOfElements=ALEN(THIS.FileTags,1)
    THIS.ARIACOMBOBOX1.Visible=.t.
    IF !EMPTY(THIS.ARIACOMBOBOX1.VALUE)
      lcFontStyle=IIF(THIS.ARIATEXTBOX1.FONTBOLD,'B','')+;
                  IIF(THIS.ARIATEXTBOX1.FONTITALIC,'I','') 
      THIS.ARIATEXTBOX1.WIDTH = LEN(EVAL(SYS(14,VAL(SYS(21)),THIS.ALIAS)))*FONTMETRIC(7,;
                                THIS.ARIATEXTBOX1.FONTNAME,THIS.ARIATEXTBOX1.FONTSIZE,;
                                IIF(EMPTY(lcFontStyle),'N',lcFontStyle))
      THIS.ARIATEXTBOX1.WIDTH = MIN(THIS.WIDTH-10,THIS.ARIATEXTBOX1.WIDTH)                          
      THIS.ARIATEXTBOX1.MAXLENGTH = LEN(EVAL(SYS(14,VAL(SYS(21)),THIS.ALIAS)))
      THIS.ARIATEXTBOX1.Visible=.t.    
    ELSE  
      THIS.ARIATEXTBOX1.ENABLED=.F.        
    ENDIF  
  ELSE
    THIS.ARIACOMBOBOX1.Visible = .F. 
    THIS.ARIATEXTBOX1.Visible  = .F.
  ENDIF
  SELECT (lcAlias)
ENDIF

ENDPROC
     ����    �  �                        ��   %   �      N  !             �  U   ��  � � � � � %�C�� ��7 � T� � ��  �� �N � T� � �C�� � T� � �� �� T� � �� �� %�C� THIS.KEYb� C��� � T� � ��  �� �� � T� � �C� � ��� �" %�C� THIS.InitOrderb� C��� � T� � ��  �� � T� �	 �� �� T� �
 �� �� T� � �CCO�� ��C� �  � � � � �� %�� �
 
���� T� � � � �-�� T� � �� Order�� T� � �C� � ��� ���  T� � �� Seek (Quik find)�� � %�� �
 � CC�]g� 	��� T� � � � �CC�]g�� ��C� � � � �� � U  TCALIAS TOBROWSE TLFIND TCORDER TCKEY THIS ALIAS KEY	 INITORDER BROWSE FIND	 NRECORDNO ARIAQUIKSEARCH1 INIT ARIATEXTBOX1 VISIBLE CAPTION ORDER ARIACOMBOBOX1	 LISTINDEX VALID Init,     ��1 q� � A �� QA !A �AQQ� A ��1A 1                             )   �                       X���    ?  ?                        H   %   =      �  *   �          �  U  U ��  � %�� � � ��I � T� � � �-�� T� � �a�� �� � � � � �! %�C� � f� ARIAFORMSET��<� ��C� �	 �� %�C� �
 �
��8�- %�C� oAriaApplication.oToolBarb� O��4� T� � � �C+�� T� � � � �� � � �� T� � � �C�� T� � � � �� � � �� ��C� � � �� �� � � � � � �N�
 �� � � � U 	 NCOLINDEX THIS PARENT FROMSEEK	 HIGHLIGHT ARIATEXTBOX1 SETFOCUS THISFORMSET CLASS
 REFRESHALL FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR ENDFILE OWINDPARENT TOPFILE
 NAVREFRESH REFRESH THISFORM�   %��  � � �	 Container��� � T�  � �� �� T�  � ��  � � �� T�  � �� �� T�  � ��  � � ��- %�C�  � �� C� �	 f� ARIAFORMSET	��� � T�  � �C� �
 �� � � U  THIS PARENT	 BASECLASS LEFT WIDTH TOP HEIGHT RECORDSOURCE THISFORMSET CLASS GETMASTERFILEu  ��  �! %�C� � f� ARIAFORMSET��n �( %�� � � � � �
 C� � �
	��j � %�C� � 
��f � �� � � � U 	 NCOLINDEX THISFORMSET CLASS EDITMODE ADDMODE FORMHASTOOLBAR CANCONTINUE AfterRowColChange,     �� InitU    �� BeforeRowColChangeu    ��1 q 1� C � 1�!�!�� A A � � A 2 aa�AA A 2 q �!A A A A 1                       
        %  @     !   i  ^  %    )   ?                       9�PROCEDURE initializefields
lParameters taFileFields,taFileFilter
IF PARAMETERS()=2
  DIMEN THIS.aFilterExpr[ALEN(taFileFilter,1),7]
  =ACOPY(taFileFilter,THIS.aFilterExpr)
ENDIF  
DIMEN THIS.aAvailableFields[ALEN(taFileFields,1),ALEN(taFileFields,2)]
DIMEN THIS.aFieldValues[ALEN(taFileFields,1),ALEN(taFileFields,2)]

=ACOPY(taFileFields,THIS.aAvailableFields)
=ACOPY(taFileFields,THIS.aFieldValues)
DIME THIS.aFieldValues[ALEN(THIS.aFieldValues,1)+1,ALEN(THIS.aFieldValues,2)]
THIS.aFieldValues[ALEN(THIS.aFieldValues,1),1] = 'Value'
THIS.aFieldValues[ALEN(THIS.aFieldValues,1),2] = 'V'
THIS.aFieldValues[ALEN(THIS.aFieldValues,1),3] = 'V'
THIS.aFieldValues[ALEN(THIS.aFieldValues,1),3] = 'V'
THIS.cbxExprLeft.NumberOfElements = ALEN(this.aAvailableFields,1)
THIS.cbxExprLeft.RowSource = 'this.Parent.aAvailableFields'
THIS.cbxExprLeft.RowSourceType = 5
THIS.cbxExprLeft.BoundColumn = 2

THIS.cbxExprRight.NumberOfElements = ALEN(this.aFieldValues,1)
*THIS.cbxExprRight.RowSource = 'this.Parent.aFieldValues'
*THIS.cbxExprRight.RowSourceType = 5

*********
Local lcPopName,lnCount
lcPopName = this.ValuePopup
DEFINE POPUP &lcPopName
FOR lnCount = 1 TO ALEN(this.aFieldValues,1)
  DEFINE BAR lnCount of &lcPopName PROMPT THIS.aFieldValues[lnCount,1] 
ENDFOR

this.cbxExprRight.RowSource = this.ValuePopup
this.cbxExprRight.RowSourceType = 9
*********



THIS.cbxExprLeft.Requery
THIS.cbxExprRight.Requery
THIS.BuildList()
ENDPROC
PROCEDURE buildlist
Local lnCount
THIS.lstExpression.CLEAR()
IF !EMPTY(THIS.aFilterExpr[1,1])
  FOR lnCount = 1 TO ALEN(this.aFilterExpr,1)
    IF this.aFilterExpr[lnCount,1] = '.OR.'
      THIS.lstExpression.ADDITEM(REPL('_',40),lnCount)
      THIS.lstExpression.ADDLISTITEM(PADC(' OR ',10,'_'),lnCount,2)
      THIS.lstExpression.ADDLISTITEM(REPL('_',40),lnCount,3)
      THIS.lstExpression.ADDLISTITEM(REPL('_',40),lnCount,4)
      LOOP
    ENDIF

    LOCAL lcValidEntries
    DO CASE
      CASE this.aFilterExpr[lnCount,3] = 'L'
        lcValidEntries = 'Yes,.T.,No,.F.'
      CASE this.aFilterExpr[lnCount,2] = 'F'
        lcValidEntries= gfDataBaseProp('Get',this.aFilterExpr[lnCount,1],'field','Validentries')
    ENDCASE
    
    THIS.lstExpression.ADDITEM(THIS.aFilterExpr[lnCount,1],lnCount)  
    THIS.lstExpression.listIndex = lnCount
    THIS.cbxExprLeft.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]'
    THIS.cbxExprLeft.Value = THIS.aFilterExpr[lnCount,1]
    this.lstExpression.List(this.lstExpression.ListIndex,1)= THIS.cbxExprLeft.DisplayValue
    THIS.lstExpression.ADDLISTITEM(IIF(THIS.aFilterExpr[lnCount,4],'Is not','Is'),lnCount,2)
    THIS.lstExpression.ADDLISTITEM(THIS.aFilterExpr[lnCount,5],lnCount,3)

    IF THIS.aFilterExpr[lnCount,7] = 'V'
      IF EMPTY(lcValidEntries)
        THIS.lstExpression.ADDLISTITEM(THIS.aFilterExpr[lnCount,6],lnCount,4)    
      ELSE
        this.cbxFrom.RowSource = lcValidEntries      
        this.cbxTo.RowSource = lcValidEntries              
        IF ATC(',',this.aFilterExpr[lnCount,6])>0
          this.cbxFrom.Value = SUBSTR(this.aFilterExpr[lnCount,6],1,ATC(',',this.aFilterExpr[lnCount,6])-1)
          this.cbxTo.Value = SUBSTR(this.aFilterExpr[lnCount,6],ATC(',',this.aFilterExpr[lnCount,6])+1)        
          THIS.lstExpression.ADDLISTITEM(THIS.cbxFrom.DisplayValue+','+THIS.cbxTo.DisplayValue,lnCount,4)             
        ELSE
          this.cbxFrom.Value = this.aFilterExpr[lnCount,6]
          THIS.lstExpression.ADDLISTITEM(THIS.cbxFrom.DisplayValue,lnCount,4)             
        ENDIF

      ENDIF  
    ELSE
      Local lnBarPos
      lnBarPos = ASCAN(THIS.aFieldValues,THIS.aFilterExpr[lnCount,6])
      lnBarPos = ASUBSCRIPT(THIS.aFieldValues,lnBarPos,1)
      THIS.lstExpression.ADDLISTITEM(THIS.aFieldValues[lnBarPos,1],lnCount,4)
    ENDIF
  ENDFOR
  THIS.cbxExprOperator.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]'
  THIS.chkIs.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]'
  THIS.cbxExprRight.ControlSource = ''
ENDIF  

ENDPROC
PROCEDURE buildbar
lParameters tnColumn
DO CASE
  CASE tnColumn = 1
    this.lstExpression.List(this.lstExpression.ListIndex,1) = THIS.cbxExprLeft.DisplayValue
  CASE tnColumn = 2
    this.lstExpression.List(this.lstExpression.ListIndex,2) = IIF(this.chkIs.Value,'Is not','Is')
  CASE tnColumn = 3    
    this.lstExpression.List(this.lstExpression.ListIndex,3) = THIS.cbxExprOperator.DisplayValue  
  CASE tnColumn = 4
    IF THIS.aFilterExpr[this.lstExpression.ListItemID,7]='V'
      LOCAL lcValidEntries
      DO CASE
        CASE this.aFilterExpr[this.lstExpression.ListItemID,3] = 'L'
          lcValidEntries = 'Yes,.T.,No,.F.'
        CASE this.aFilterExpr[this.lstExpression.ListItemID,2] = 'F'
          lcValidEntries= gfDataBaseProp('Get',this.aFilterExpr[this.lstExpression.ListItemID,1],'field','Validentries')
      ENDCASE
      IF EMPTY(lcValidEntries)
        this.lstExpression.List(this.lstExpression.ListIndex,4) = THIS.aFilterExpr[this.lstExpression.ListItemID,6]
      ELSE
        this.cbxFrom.RowSource = lcValidEntries      
        this.cbxTo.RowSource = lcValidEntries              
        IF ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])>0
          this.cbxFrom.Value = SUBSTR(this.aFilterExpr[this.lstExpression.ListItemID,6],1,ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])-1)
          this.cbxTo.Value = SUBSTR(this.aFilterExpr[this.lstExpression.ListItemID,6],ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])+1)        
          this.lstExpression.List(this.lstExpression.ListIndex,4) = THIS.cbxFrom.DisplayValue+','+THIS.cbxTo.DisplayValue
        ELSE
          this.cbxFrom.Value = this.aFilterExpr[this.lstExpression.ListItemID,6]
          this.lstExpression.List(this.lstExpression.ListIndex,4) = THIS.cbxFrom.DisplayValue
        ENDIF
      ENDIF  
       
    ELSE   
      Local lnBarPos
      lnBarPos = ASCAN(THIS.aFieldValues,THIS.aFilterExpr[this.lstExpression.ListItemID,6])
      IF lnBarPos > 0    
        lnBarPos = ASUBSCRIPT(THIS.aFieldValues,lnBarPos,1)
        this.lstExpression.List(this.lstExpression.ListIndex,4) = THIS.aFieldValues[lnBarPos,1]
      ENDIF     
    ENDIF  
ENDCASE
ENDPROC
PROCEDURE getvalue
LOCAL lcValidEntries
DO CASE
  CASE this.aFilterExpr[this.lstExpression.ListItemID,3] = 'L'
    lcValidEntries = 'Yes,.T.,No,.F.'
  CASE this.aFilterExpr[this.lstExpression.ListItemID,2] = 'F'
    lcValidEntries= gfDataBaseProp('Get',this.aFilterExpr[this.lstExpression.ListItemID,1],'field','Validentries')
ENDCASE
DO CASE
  CASE this.aFilterExpr[this.lstExpression.ListItemID,5] = 'Between'
    IF !EMPTY(lcValidEntries)
      this.cbxExprRight.Visible = .f.
      this.cbxFrom.top = this.cbxExprRight.top
      IF ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])>0
        this.cbxFrom.Value = SUBSTR(this.aFilterExpr[this.lstExpression.ListItemID,6],1,ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])-1)
        this.cbxTo.Value = SUBSTR(this.aFilterExpr[this.lstExpression.ListItemID,6],ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])+1)        
      ELSE
        this.cbxFrom.Value = this.aFilterExpr[this.lstExpression.ListItemID,6]
      ENDIF
      this.cbxFrom.Width = this.cbxTo.Width      
      this.cbxTo.top = this.cbxExprRight.top          
      this.cbxFrom.RowSource = lcValidEntries
      this.cbxFrom.visible = .t.
      this.cbxTo.RowSource = lcValidEntries      
      this.cbxTo.visible = .t.      
      this.cbxFrom.SetFocus()
      KEYBOARD "{SPACEBAR}"
    ELSE
      this.cbxExprRight.Visible = .f.
      this.txtFrom.top = this.cbxExprRight.top
      IF ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])>0
        this.txtFrom.Value = SUBSTR(this.aFilterExpr[this.lstExpression.ListItemID,6],1,ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])-1)
        this.txtTo.Value = SUBSTR(this.aFilterExpr[this.lstExpression.ListItemID,6],ATC(',',this.aFilterExpr[this.lstExpression.ListItemID,6])+1)        
      ELSE
        this.txtFrom.Value = this.aFilterExpr[this.lstExpression.ListItemID,6]
      ENDIF
      IF this.aFilterExpr[this.lstExpression.ListItemID,2] = 'F'
        Local lcInputMask,lcFormat
        lcInputMask = gfDataBaseProp('DBGetProp',this.aFilterExpr[this.lstExpression.ListItemID,1],'FIELD','INPUTMASK')
        lcFormat = gfDataBaseProp('DBGetProp',this.aFilterExpr[this.lstExpression.ListItemID,1],'FIELD','FORMAT')
        IF EMPTY(lcInputMask)
          lcInputMask = This.GetMask(this.aFilterExpr[this.lstExpression.ListItemID,1])
        ENDIF        
        this.txtFrom.Format = IIF(!EMPTY(lcFormat),lcFormat,'')
        this.txtTo.InputMask = IIF(!EMPTY(lcInputMask),lcInputMask,this.txtTo.InputMask )
        this.txtTo.Format = IIF(!EMPTY(lcFormat),lcFormat,this.txtTo.Format )
      ENDIF
      this.txtFrom.Width = this.txtTo.Width
      this.txtTo.top = this.cbxExprRight.top
      this.txtFrom.visible = .t.
      this.txtTo.visible = .t.
      this.txtFrom.SetFocus()
    ENDIF
  CASE this.aFilterExpr[this.lstExpression.ListItemID,5] = 'In List'
    IF !EMPTY(lcValidEntries)  
    ELSE
    ENDIF
  Otherwise
    IF !EMPTY(lcValidEntries)  
      this.cbxExprRight.Visible = .f.    
      this.cbxFrom.RowSource = lcValidEntries
      this.cbxFrom.Value = this.aFilterExpr[this.lstExpression.ListItemID,6]
*      this.cbxFrom.ControlSource = 'this.parent.aFilterExpr[this.parent.lstExpression.ListItemID,6]'
      this.cbxFrom.visible = .t.
      this.cbxFrom.top = this.cbxExprRight.top
      this.cbxFrom.Width = this.cbxExprRight.Width
      this.cbxFrom.SetFocus()
      KEYBOARD "{SPACEBAR}"    
    ELSE
      IF this.aFilterExpr[this.lstExpression.ListItemID,2] = 'F'
        Local lcInputMask,lcFormat
        lcInputMask = gfDataBaseProp('DBGetProp',this.aFilterExpr[this.lstExpression.ListItemID,1],'FIELD','INPUTMASK')
        lcFormat = gfDataBaseProp('DBGetProp',this.aFilterExpr[this.lstExpression.ListItemID,1],'FIELD','FORMAT')
        IF EMPTY(lcInputMask)
          lcInputMask = This.GetMask(this.aFilterExpr[this.lstExpression.ListItemID,1])
        ENDIF        
        this.txtFrom.InputMask = IIF(!EMPTY(lcInputMask),lcInputMask,this.txtFrom.InputMask )
        this.txtFrom.Format = IIF(!EMPTY(lcFormat),lcFormat,this.txtFrom.Format )
      ENDIF
      this.cbxExprRight.Visible = .f.    
      this.txtFrom.visible = .t.
      this.txtFrom.Value = this.aFilterExpr[this.lstExpression.ListItemID,6]      
      this.txtFrom.top = this.cbxExprRight.top
      this.txtFrom.Width = this.cbxExprRight.Width
      this.txtFrom.SetFocus()      
    ENDIF
ENDCASE
ENDPROC
PROCEDURE removeline
LOCAL lnIndex,lnItemID,lnCount
lnIndex = THIS.lstExpression.ListIndex
lnItemID = THIS.lstExpression.ListItemID
IF THIS.lstExpression.LISTCOUNT = 1
  THIS.cbxExprLeft.ControlSource = ''
  THIS.cbxExprOperator.ControlSource = ''
  THIS.chkIs.ControlSource = ''
ENDIF
THIS.lstExpression.RemoveItem(THIS.lstExpression.ListIndex)
THIS.ARRANGE()
IF lnIndex > THIS.lstExpression.ListCount
  THIS.lstExpression.ListIndex = lnIndex - 1
ELSE
  THIS.lstExpression.ListIndex = lnIndex
ENDIF
THIS.lstExpression.WHEN()
THIS.AddedNewBar = .F.
THIS.PARENT.REFRESH()

ENDPROC
PROCEDURE arrange
LOCAL lnCount,laLstRows,lnSourceElm,lnDesElm
IF this.lstExpression.ListCount>0
  DIMENSION laLstRows(this.lstExpression.ListCount,7)
ELSE
  DIMENSION laLstRows(1,7)
ENDIF  
FOR lnCount = 1 TO this.lstExpression.ListCount
  this.lstExpression.ListIndex = lnCount
  lnSourceElm = (this.lstExpression.ListItemID * 7) - 6
  lnDesElm = (lnCount * 7) - 6
  =ACOPY(THIS.aFilterExpr,laLstRows,lnSourceElm,7,lnDesElm)
ENDFOR
DIMEN THIS.aFilterExpr[ALEN(laLstRows,1),7]
=ACOPY(laLstRows,THIS.aFilterExpr)
THIS.BuildList()

ENDPROC
PROCEDURE skipoperator
Local lcPopName,lnCount
lcPopName = THIS.OperatorPopup
FOR lnCount = 1 TO ALEN(THIS.aOperators,1)
  SET SKIP OF BAR lnCount OF &lcPopName !(this.aFilterExpr[this.lstExpression.ListItemID,3] $ this.aOperators[lnCount,2])
ENDFOR 

lcPopName = THIS.ValuePopup
FOR lnCount = 1 TO ALEN(THIS.aFieldValues,1)
  SET SKIP OF BAR lnCount OF &lcPopName !(this.aFilterExpr[this.lstExpression.ListItemID,3] = this.aFieldValues[lnCount,3] ) AND this.aFieldValues[lnCount,3]<>'V'
ENDFOR 



*THIS.cbxExprOperator.Requery

ENDPROC
PROCEDURE getmask
lParameters tcFieldName
IF ALEN(THIS.aAvailableFields,2)>4
  lnPos = ASCAN(THIS.aAvailableFields,tcFieldName)
  IF lnPos>0
    lnPos = ASUBSCRIPT(THIS.aAvailableFields,lnPos,1)
    IF !EMPTY(THIS.aAvailableFields[lnPos,5]) AND TYPE('THIS.aAvailableFields[lnPos,5]')='C'
      RETURN THIS.aAvailableFields[lnPos,5]
    ENDIF
  ENDIF
ENDIF  
DO CASE
  CASE TYPE(tcFieldName) $ 'CM'
      IF TYPE(tcFieldName)='M'
        RETURN REPL('X',60)
      ELSE
        RETURN REPL('X',LEN(EVAL(tcFieldName)))
      ENDIF
  CASE TYPE(tcFieldName) = 'N'
  
  CASE TYPE(tcFieldName) = 'Y'
    RETURN REPL('9',15)+'.'+REPL(9,4)
  CASE TYPE(tcFieldName) = 'D'  
    RETURN '99/99/9999'
  CASE TYPE(tcFieldName) = 'T'
  
ENDCASE
ENDPROC
PROCEDURE Destroy
Local lcPopName
lcPopName = this.OperatorPopup
RELEASE POPUPS &lcPopName
lcPopName = this.ValuePopup
RELEASE POPUPS &lcPopName

ENDPROC
PROCEDURE Init
this.OperatorPopup = gfTempName()
this.ValuePopup = gfTempName()
DIME this.aOperators[7,2]
this.aOperators[1,1] = 'Like'
this.aOperators[1,2] = 'CMLNDTY'

this.aOperators[2,1] = 'Greater Than'
this.aOperators[2,2] = 'CMNDTY'

this.aOperators[3,1] = 'Less Than'
this.aOperators[3,2] = 'CMNDTY'

this.aOperators[4,1] = 'Greater Or Equal'
this.aOperators[4,2] = 'CMNDTY'

this.aOperators[5,1] = 'Less Or Equal'
this.aOperators[5,2] = 'CMNDTY'

this.aOperators[6,1] = 'Between'
this.aOperators[6,2] = 'CNDTY'

this.aOperators[7,1] = 'Contains'
this.aOperators[7,2] = 'CM'

*this.aOperators[8,1] = 'In List'
*this.aOperators[8,2] = 'CNDTY'


Local lcPopName,lnCount
lcPopName = this.OperatorPopup
DEFINE POPUP &lcPopName
FOR lnCount = 1 TO ALEN(THIS.aOperators,1)
  DEFINE BAR lnCount of &lcPopName PROMPT THIS.aOperators[lnCount,1] 
ENDFOR

this.cbxExprOperator.RowSource = this.OperatorPopup
this.cbxExprOperator.RowSourceType = 9

ENDPROC
PROCEDURE Refresh
IF THIS.lstExpression.ListCount = 0
  THIS.SETALL('ENABLED',.F.)
  THIS.lstExpression.ENABLED=.T.
ELSE
  THIS.SETALL('ENABLED',.T.)
ENDIF
ENDPROC
     PROCEDURE InteractiveChange
IF THIS.LISTINDEX <> THIS.LISTITEMID
  LOCAL lnListIndex
  lnListIndex = THIS.LISTINDEX
  THIS.PARENT.ARRANGE()
  THIS.LISTINDEX = lnListIndex
ENDIF
ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
IF nButton=2 AND THIS.ListCount=0
  DODEFAULT()
  THIS.RIGHTCLICK()
ENDIF
ENDPROC
PROCEDURE When
THIS.PARENT.Parent.REFRESH()
IF THIS.ListCount > 0
  IF THIS.PARENT.aFilterExpr[THIS.listItemID,1] = '.OR.' 
    THIS.PARENT.cbxExprRight.Enabled = .F.
    THIS.PARENT.cbxExprLeft.Enabled = .F.    
    THIS.PARENT.cbxExprOperator.Enabled = .F.        
    THIS.PARENT.chkIs.Enabled = .F. 
    RETURN
  ELSE  
    THIS.PARENT.cbxExprRight.Enabled = .T.
    THIS.PARENT.cbxExprLeft.Enabled = .T.    
    THIS.PARENT.cbxExprOperator.Enabled = .T.
    THIS.PARENT.chkIs.Enabled = .T.
  ENDIF
  THIS.PARENT.SKIPOperator()  
  IF THIS.PARENT.aFilterExpr[THIS.listItemID,7] = 'V'
    LOCAL lcPopName,lcValidEntries
    lcPopName = THIS.PARENT.VALUEPOPUP
    DO CASE
      CASE this.Parent.aFilterExpr[this.ListItemID,3] = 'L'
        lcValidEntries = 'Yes,.T.,No,.F.'
      CASE this.Parent.aFilterExpr[this.ListItemID,2] = 'F'
        lcValidEntries= gfDataBaseProp('Get',this.Parent.aFilterExpr[this.ListItemID,1],'field','Validentries')
    ENDCASE
    IF EMPTY(lcValidEntries)
      DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT ;
      'Value '+THIS.PARENT.aFilterExpr[THIS.listItemID,6]
    ELSE
      this.Parent.cbxFrom.RowSource = lcValidEntries      
      this.Parent.cbxTo.RowSource = lcValidEntries              
      IF ATC(',',this.Parent.aFilterExpr[this.ListItemID,6])>0
        this.Parent.cbxFrom.Value = SUBSTR(this.Parent.aFilterExpr[this.ListItemID,6],1,ATC(',',this.Parent.aFilterExpr[this.ListItemID,6])-1)
        this.Parent.cbxTo.Value = SUBSTR(this.Parent.aFilterExpr[this.ListItemID,6],ATC(',',this.Parent.aFilterExpr[this.ListItemID,6])+1)
        DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT ;
        'Value '+THIS.Parent.cbxFrom.DisplayValue+','+THIS.Parent.cbxTo.DisplayValue
      ELSE
        this.Parent.cbxFrom.Value = this.Parent.aFilterExpr[this.ListItemID,6]
        DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT ;
        'Value '+THIS.Parent.cbxFrom.DisplayValue
      ENDIF
    ENDIF  
    THIS.Parent.cbxExprRight.ListIndex = ALEN(THIS.Parent.aFieldValues,1)
  ELSE
    LOCAL lcPopName,lnBarPos
    lcPopName = THIS.PARENT.VALUEPOPUP
    DEFINE BAR ALEN(THIS.Parent.aFieldValues,1) OF &lcPopName PROMPT  'Value'
    lnBarPos = ASCAN(THIS.PARENT.aFieldValues,THIS.Parent.aFilterExpr[this.ListItemID,6])
    lnBarPos = ASUBSCRIPT(THIS.PARENT.aFieldValues,lnBarPos,1)
    THIS.Parent.cbxExprRight.ListIndex = lnBarPos
    THIS.Parent.cbxExprRight.refresh()
  ENDIF
ENDIF
ENDPROC
PROCEDURE RightClick
Local lnChoice,lcEnabling
lcEnabling = "T"+IIF(this.ListCount>0,'T','F')+'T'+IIF(this.ListIndex>0,'T','F')+'T'+;
             IIF(this.ListCount>0,'T','F')+'T'
lnChoice = This.Parent.FilterShortCut.ShowShortCut(This,"\<Add,\<Or,\<Insert,\<Remove,\-,\<Save,\<Load",lcEnabling)
DO CASE
  CASE lnChoice = 1
    Local lnBarPos,lcCaption
    lnBarPos = THIS.LISTCOUNT+1
    THIS.ADDITEM('',lnBarPos)
    THIS.ADDLISTITEM('Is',lnBarPos,2)
    THIS.ADDLISTITEM('Like',lnBarPos,3)
    THIS.ADDLISTITEM('',lnBarPos,4)    
    DIMEN THIS.Parent.aFilterExpr[lnBarPos,7]
    THIS.Parent.aFilterExpr[lnBarPos,1]=''
    THIS.Parent.aFilterExpr[lnBarPos,2]=''    
    THIS.Parent.aFilterExpr[lnBarPos,3]=''    
    THIS.Parent.aFilterExpr[lnBarPos,4]=.F.
    THIS.Parent.aFilterExpr[lnBarPos,5]='Like'    
    THIS.Parent.aFilterExpr[lnBarPos,6]=''    
    THIS.Parent.aFilterExpr[lnBarPos,7]='V'    
    THIS.PARENT.SETALL('ENABLED',.T.)
    THIS.LISTINDEX = THIS.LISTCOUNT    
    THIS.Parent.AddedNewBar = .T.
    
    THIS.Parent.cbxExprLeft.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]'
    THIS.Parent.cbxExprOperator.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]'
    THIS.Parent.chkIs.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]'
    
    THIS.PARENT.cbxExprLeft.SETFocus()
    KEYBOARD "{SPACEBAR}"
  CASE lnChoice = 2
    Local lnBarPos,lcCaption
    lnBarPos = MAX(THIS.LISTINDEX,1)
    THIS.ADDITEM(' ',lnBarPos)
*    THIS.ADDLISTITEM(PADC(' OR ',10,'_'),lnBarPos,2)
*    THIS.ADDLISTITEM(REPL('_',40),lnBarPos,3)
*    THIS.ADDLISTITEM(REPL('_',40),lnBarPos,4)
*    THIS.LISTINDEX = lnBarPos
    IF !EMPTY(THIS.Parent.aFilterExpr[1,1])
      DIMEN THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1)+1,7]
    ENDIF  
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),1]='.OR.'
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),2]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),3]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),4]=.F.
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),5]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),6]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),7]=''    
    THIS.PARENT.SETALL('ENABLED',.T.)
    THIS.Parent.AddedNewBar = .F.
    
    THIS.Parent.cbxExprLeft.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]'
    THIS.Parent.cbxExprOperator.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]'
    THIS.Parent.chkIs.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]'
    THIS.InterActiveChange() 
  CASE lnChoice = 3
    Local lnBarPos,lcCaption
    lnBarPos = MAX(THIS.LISTINDEX,1)
    THIS.ADDITEM('',lnBarPos)
    THIS.ADDLISTITEM('Is',lnBarPos,2)
    THIS.ADDLISTITEM('Like',lnBarPos,3)
    THIS.ADDLISTITEM('',lnBarPos,4)
    THIS.LISTINDEX = lnBarPos
    IF !EMPTY(THIS.Parent.aFilterExpr[1,1])
      DIMEN THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1)+1,7]
    ENDIF  
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),1]=''
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),2]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),3]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),4]=.F.
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),5]='Like'    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),6]=''    
    THIS.Parent.aFilterExpr[ALEN(THIS.Parent.aFilterExpr,1),7]='V'    
    THIS.PARENT.SETALL('ENABLED',.T.)
    THIS.Parent.AddedNewBar = .T.
    
    THIS.Parent.cbxExprLeft.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]'
    THIS.Parent.cbxExprOperator.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]'
    THIS.Parent.chkIs.ControlSource = 'this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]'
    
    THIS.PARENT.cbxExprLeft.SETFocus()
    KEYBOARD "{SPACEBAR}"
  
  CASE lnChoice = 4
    this.Parent.RemoveLine()
ENDCASE

ENDPROC
     B����    �B  �B                        ��   %   <      �A  F  !=          �  U   %�C�  � ��  � F��  � �� � T�  � � ��  � � VS��, T�  � � ��  � � V� �  � � �	�� T�  � � ��  � � AVSE��D T�  � � ��  � � V� �  �	 �
 
	� �  � 	� �  � � �	�� T�  � � ��  � � V�� T�  � � �a�� ��C�  � �� t�:� U  THIS	 NWORKAREA CMDFIND ENABLED
 ACTIVEMODE CMDPRINT CONTROLENABLE CMDEXIT	 CMDDELETE OWINDPARENT SESSIONISREADONLY ALLOWDELETE	 CMDSELECT CMDCALC
 SETCAPTION ��  � � � � � � � �  ���� � ���� T� � ��  �� T� �	 ��  �� T� �
 ���� T� � ��  ��$ %�C� THIS.oWindParentb� O���� T� � �� � � �� T� � �� � � �� T� � �� � � �� T� � �� � � �� G�(�� � � �� T� � �� � � �� T� � � �� � � �� T� � �� � � �� T� � �� � � �� T� � �� � � �� T� � �� � � ��# T� � � �CC� � � ��\g��# T� � � �CC� � � ��\g��# T� � � �CC� � � ��\g��# T� � � �CC� � � ��\g��# T� �  � �CC� � � ��\g��# T� �! � �CC� � � ��\g��# T� �" � �CC� � � ��\g��# T� �# � �CC� � � ��\g��( %�� 1� � � � C� � � �
	���� T� �$ �a�� ��� T� �$ �-�� � �� T� � �� N�� T� � �-�� T� � �-�� T� � �-�� T� � �-�� T� � �-�� T� � �CCW�� T� � � ��  �� T� � � ���� T� � � ���� T� � � ���� T� � � ���� T� �  � ���� T� �! � ���� T� �" � ���� T� �# � ���� �E T� �! �% �� � � SV� � � �& 
	� � � 	� � �! � �	��S T� �" �% �� � � V� � � �& 
	� � � 	� � �" � �	� � � � V	��D T� �# �% �� � � V� � � �& 
	� � � 	� � �# � �	�� %�� �' ��� B� � U(  ATABLESUSED NTABLESUSED I AMEMS NTOTMEM CWIZFILE LSHOWEDMESS THIS VIEWKEY	 PARENTKEY VIEWTYPE	 GRIDALIAS TOPFILE OWINDPARENT ENDFILE OLDREC
 ACTIVEMODE DATASESSIONID	 NWORKAREA LCKEY CONTROLSOURCE KEY	 KEYOBJECT ALLOWADD	 ALLOWEDIT ALLOWDELETE CMDTOP CONTROLENABLE FORMHASTOOLBAR CMDPREV CMDNEXT CMDEND CMDPRINT CMDADD CMDEDIT	 CMDDELETE VISIBLE ENABLED SESSIONISREADONLY PREVIEWMODEK ��  � � � � � �� � � � �	 �
 � � � ���� �  ���� T�� ��  �� T�� �a�� T�� �CW�� T�� �C�� �y�� �� ���(��� ��-� F�C �� �� �� T��	 �CC� Databaseꉡ
�� T�� ��  �� T��
 �-�� T�� �-�� H���� �C�	 Buffering����/� .� �C� ����k� T�� �C-a��� %��� ��g� .� �; ���	 
�( C� 2C���ꐸ� � C� 3C���ꐸ� 	���� T�� �C� ��� +��� � ��V� #��� �� T�� �CS�� %��� 
��*�, T�� �� Record in use by another user�� Z� !� � %��� 
��>� ��� ���(�C.��:� %�CC�� /b� G��r� .� � %�CC�� /�_CC�� /���6� T�� �a��e %�C�I Data has been changed by another user. Overwrite changes with your edits?�4�9� �x���� T��
 �a�� �2� T�� �-�� Z� !� � � �� � T�� �C�� ��� � %��� ���� T�� �Ca��
 ��� %��� ���� .� � Z� � ���	 ���� ��� T�� �Ca-��� %��� ���� ��� .� � �� � T� �C��  �z�� H���� �� � ��� �C���  ���N� T�� �� Trigger failed.�� �C���  �-����( T�� �� Field doesn't accept NULL�� �C���  �.����" T�� �� Field rule violated�� �C���  �����, T�� �� Record in use by another user�� �C���  �/��H�  T�� �� Row rule violated�� �C���  �\����% T�� �� Unique index violation�� �C���  �1���� %���	 ����e %�C�I Data has been changed by another user. Overwrite changes with your edits?�4�9� �x����� ��� T�� �Caa��� %��� ��A� ��� .� �� ��2 ��C� Could not force table updates.� �9� �x�� � � � 2��� %�C�� �
����! T�� �� Error: C���  �� � �
 ��Ca��� T�� �-�� %�C�� �
��)�2 ��C� Failed to update table: �� � �9� �x�� � �� F��� �� B��� �� U  AERRORS CERRORMESSAGE ATABLESUSED NTABLESUSED NTOTERR NFLD I NOLDAREA LSUCCESS LINDBC
 LOVERWRITE LHADMESSAGE
 NMODRECORD CAPTION� %��  � � SV��v�# T�  � � ��  � �	 tbnew.bmp��# T�  � � ��  � �	 close.bmp��# T�  � � ��  � �	 tbnew.bmp��# T�  � � ��  � �	 close.bmp��6 T�  � � ��  �	 � �  � � V	� �  � �
 �	��7 T�  � � ��  �	 � �  � � SV	� �  � �
 �	�� %�C� P01PU01s��r�M s��4��� "�� \<Close���CC�	 LACTRSTATb� U� a� C�� � DISABLE6�� � ���$ T�  � � ��  � �
 tbsave.bmp��$ T�  � � ��  � �
 tbundo.bmp��$ T�  � � ��  � �
 tbsave.bmp��$ T�  � � ��  � �
 tbundo.bmp�� T�  � � �-�� T�  � � �a�� T�  � � �a�� %�C� P01PU01s����N s��4��� "�� \<Cancel���CC�	 LACTRSTATb� U� a� C�� � DISABLE6�� � � U  THIS
 ACTIVEMODE CMDADD PICTURE BMPPATH CMDEXIT DOWNPICTURE CMDEDIT ENABLED	 ALLOWEDIT CONTROLENABLE P01PU01	 LACTRSTATn ��  � � � T��  �� � �� T� � �a�� %�C� � ��O � F�� � �� �J T� � � �C� �	 � SV� � Add new record� � Save modifications6��H T� �
 � �C� �	 � SV� � Close Current Form� � Undo Changes6�� %�� �	 � SV���� T� � �C+� � � �� %�� � 
��B� H� T� � �C+��
 H������ �O� #6� � T� � �C� C+� � � �� %�� � 
����
 H������ T� � �C�� %�� � 
���� H� � � %�� � ���� #)� � �7 T� � � �� � 
� � �	 � V	� � � � �	��7 T� � � �� � 
� � �	 � V	� � � � �	��7 T� � � �� � 
� � �	 � V	� � � � �	��7 T� � � �� � 
� � �	 � V	� � � � �	�� H����� �� � � C�s���� �� �	 � E���� T� � � �-�� �CN� � C� C+��:� T� � � �-�� T� � � �-�� �� � � 
����6 T� � � �� �	 � V� � � 	� � � � �	��6 T� � � �� �	 � V� � � 	� � � � �	�� �# %�� �	 � SV�
 C� � �
	���� T� �C� � ��� H� �� �C� � b� C��<�% T� �� � � =� [�� � ]�� �C� � b� L��c� T� �� � �� �C� � b� D����' T� �� � � =� {C�� *� }�� �C� � b� T����( T� �� � � =� {C�� �� }�� 2��% T� �� � � =CC�� ��Z��� � F�� � �� H�+�z� �-��6� �� � ���f� SET FILTER TO &cFiltExpr
 �� � ���z� � F�� � �� � ��C� � � �� ��C� � � �� ��C� � �� T� � ���  ��5 T� ����C� � � � � ENABLED�
 � DISABLE6��5 T� ����C� � � � � ENABLED�
 � DISABLE6��5 T� ����C� � � � � ENABLED�
 � DISABLE6��5 T� ����C� � � � � ENABLED�
 � DISABLE6��E T� ����C� � � � � �	 � SV	� � ENABLED�
 � DISABLE6��5 T� ����C� �  � � � ENABLED�
 � DISABLE6��D T� ����C� � � � � �	 � V	� � ENABLED�
 � DISABLE6��5 T� ����C� � � � � ENABLED�
 � DISABLE6�� T� ��	��C�	� ��5 T� ��
��C� �! � � � ENABLED�
 � DISABLE6��E T� ����C� � � � � �	 � EA	� � ENABLED�
 � DISABLE6��E T� ����C� �
 � � � �	 � EA� � ENABLED�
 � DISABLE6�� U"  OLDLOCKSCREEN KEYVALUE	 CFILTEXPR THISFORM
 LOCKSCREEN THIS	 NWORKAREA CMDADD TOOLTIPTEXT
 ACTIVEMODE CMDEXIT ENDFILE TOPFILE CMDTOP ENABLED CONTROLENABLE CMDPREV CMDNEXT CMDEND PREVIEWMODE CMDEDIT	 CMDDELETE	 ALLOWEDIT ALLOWDELETE VIEWKEY	 PARENTKEY	 GRIDALIAS VIEWTYPE OWINDPARENT
 REFRESHALL REFRESH	 LACTRSTAT CMDPRINT CMDFIND| ��  � � � T� � ��  ��$ %�C� THIS.oWindParentb� O��u� �  ���� T� �C��  � � ��b�� ��� � ��q� �� ���(��� ��m�3 %�CC� .C �� �  �
 .BaseClass�f� GRID��i� T� � �C �� �  �� ��C� .� � ���a�! T� � �C�
 sourcetype�� ��� T� � ��� �� %�� � ���]� T� �	 ���
 �� T� � ��� �� � �� !� � �� �� � U  AMEMS NTOTMEM I THIS GRIDREF OWINDPARENT VIEWTYPE RECORDSOURCE	 GRIDALIAS VIEWKEY TAG	 PARENTKEY COMMENTt ��  � �� � � T� ��  �� %�C�� ��a � T�� �� � �� T� � ��  �� T� � ��  �� �z � T�� ���  �� � H�� �L�" �C� FormSet�� � �� ��� � T�	 �� �
 ��$ �C�	 Pageframe�� � �� ��� � T�	 �� � ��3 �C�� � � Optiongroup,Commandgroup�� ��4� T�	 �� � �� 2�L� T�	 �� � �� � �� ���(���	 ��a� H�v�]�" �C� FormSet�� � �� ���� ��CC �� �� � � � ��$ �C�	 Pageframe�� � �� ���� ��CC �� �� � � � ��3 �C�� � � Optiongroup,Commandgroup�� ��!� T� �C �� �� � � ��6 T� �CCC� .� �� � C� C� .� ��\� � 6f��& %�� Cm� C �� �� � � 	���= T� � �� � CC� � �
� � +� �  6C �� � � � ��A T� � �� � CC� � �
� � +� �  6CC �� � � � � �� �= �CC �� �� � � � Optiongroup,Commandgroup�� ��z� ��CC �� �� � � � ��Q �C�	 ContainerC �� �� � � �� �  C� PageC �� �� � � �� ���� ��CC �� �� � � � ��= �CC �� �� � � � ListBox,ComboBox,Spinner�� ��� T� �C �� �� � � ��6 T� �CCC� .� �� � C� C� .� ��\� � 6f��& %�� Cm� C �� �� � � 	���= T� � �� � CC� � �
� � +� �  6C �� � � � ��A T� � �� � CC� � �
� � +� �  6CC �� � � � � �� �E �CC �� �� � � �  CheckBox,TextBox,OleBoundControl�� ��]� T� �C �� �� � � ��6 T� �CCC� .� �� � C� C� .� ��\� � 6f��& %�� Cm� C �� �� � � 	��Y�= T� � �� � CC� � �
� � +� �  6C �� � � � ��A T� � �� � CC� � �
� � +� �  6CC �� � � � � �� � � �� B�� � �� U 
 OCONTAINER I OCONTROLPARENT LCKEY THIS OWINDPARENT FORMKEY	 KEYOBJECT	 BASECLASS
 NCTRLCOUNT	 FORMCOUNT	 PAGECOUNT BUTTONCOUNT CONTROLCOUNT
 GETKEYEXPR FORMS PAGES CONTROLS CONTROLSOURCE VISIBLE GETNAME� %��  � ����+ %�C� THIS.oWindParent.PARENTb� O��\ �! T� �C�� �  � � � ��b�� �� � T� �C�� �  � � ��b�� �+ %�C� THIS.oWindParent.PARENTb� O���� ���  � � � ���� �� ���(��� ����5 %�CC� .C �� � �
 .BaseClass�f� CURSOR���� ��C� .C �� � �����L %�C����� �� 1C� 1C�� .Q
�  C����� �� 3C� 3C�� .Q
	���� B�a�� � �� � �� �� ��� ���  � � ��~� �� ���(��� ��z�5 %�CC� .C �� � �
 .BaseClass�f� CURSOR��v� ��C� .C �� � ���r�L %�C����� �� 1C� 1C�� .Q
�  C����� �� 3C� 3C�� .Q
	��n� B�a�� � �� � �� �� � � B�-�� U	  THIS
 USEDATAENV NTOTMEM AMEMS OWINDPARENT PARENT DATAENVIRONMENT I ALIAS8 ��  � �� � � %�C�� ��� �+ %�C� THIS.oWindParent.PARENTb� O��b � T�� �� � � �� �| � T�� �� � �� � �� � T�� ���  �� � H�� �k�" �C� FormSet�� � �� ��� � T� �� � ��$ �C�	 Pageframe�� � �� ��� T� �� �	 ��3 �C�� � � Optiongroup,Commandgroup�� ��S� T� �� �
 �� 2�k� T� �� � �� � �� ���(��� ��1� H���-�" �C� FormSet�� � �� ���� ��CC �� �� � � � ��$ �C�	 Pageframe�� � �� ��� ��CC �� �� � � � ��3 �C�� � � Optiongroup,Commandgroup�� ��l�& T�� � ��� �� �� � � EA��= �CC �� �� � � � Optiongroup,Commandgroup�� ���� ��CC �� �� � � � ��Q �C�	 ContainerC �� �� � � �� �  C� PageC �� �� � � �� ��2� ��CC �� �� � � � ��= �CC �� �� � � � ListBox,ComboBox,Spinner�� ����# %�C �� �� � � � � ���� T�� � ��� �� �-�� ���& T�� � ��� �� �� � � EA�� �E �CC �� �� � � �  CheckBox,TextBox,OleBoundControl�� ����# %�C �� �� � � � � ��b� T�� � ��� �� �-�� ���& T�� � ��� �� �� � � EA�� �, �CC �� �� � � � EditBox�� ����& T�� � ��� �� �� � � SV�� %�� � 
��f� ��C �� �� � ��b� T� � ��� �� T� � ��� �� T� � ��� �� T� � ��� �� T� � �a�� �� �: T�� � ��� �� �C� � � EA�	 � � � � � 6��: T�� � ��� �� �C� � � EA�	 � � � � � 6��) �CC �� �� � � � Grid�� ��-�& T�� � ��� �� �� � � SV�� � �� U 
 OCONTAINER I OCONTROLPARENT THIS OWINDPARENT PARENT	 BASECLASS
 NCTRLCOUNT	 FORMCOUNT	 PAGECOUNT BUTTONCOUNT CONTROLCOUNT CHANGETOADDMODE FORMS PAGES BUTTONS ENABLED
 ACTIVEMODE CONTROLS CONTROLSOURCE FORMKEY READONLY HASMEMO EDITFORECOLOR	 FORECOLOR EDITDISFORECOLOR DISABLEDFORECOLOR EDITBACKCOLOR	 BACKCOLOR EDITDISBACKCOLOR DISABLEDBACKCOLOR�  ��  � �� � T� ��  ��' +�C� oContObject.Parentb� O��k � T� ��  � � .� �� T�  ��  � �� �3 T� �CC� �R� .� C� �C� >�\� � 6��	 B�� �� U  OCONTOBJECT LCNAME NAME PARENTS ��  � � � � � � ���� %�C� THIS.Parentb� O��G � B� �0 %�C� THIS.oldTalkb� C� � � � ON	��� � G2 � � %�� � ��	� � ����+ %�C� THIS.oWindParent.PARENTb� O��� �! T� �C�� � � �	 �
 ��b�� �� T� �C�� � � �
 ��b�� � �� ���(��� ���+ %�C� THIS.oWindParent.PARENTb� O��.�B ��C�( THIS.oWindParent.Parent.DataEnvironment.C �� � ���*�^ %�C�� �� C� CURSOR�� �� 	� C�
 sourcetype�� ��	� C�	 buffering�� ��	��&� ��Ca�� ��� ��C�	 buffering��� ��� � �� ��; ��C�! THIS.oWindParent.DataEnvironment.C �� � �����^ %�C�� �� C� CURSOR�� �� 	� C�
 sourcetype�� ��	� C�	 buffering�� ��	���� ��Ca�� ��� ��C�	 buffering��� ��� � �� � �� � %�� � ��!� B� � %�� � � OFF��B� G� � GM(�� � �� G&(� F�� � �� %�� � ��{� B� � �� ���(���  ����9 %�CC �� �� �� C� .TMPCC �� �� &�� 	����( ��C�	 buffering� � C �� �� ��� � �� %�� � � OFF��� G_� � %�� � � ON��>� G � � GH(�� � �� U  NTABLESUSED ATABLESUSED I
 NDECURSORS
 ADECURSORS THIS OLDTALK
 USEDATAENV OWINDPARENT PARENT DATAENVIRONMENT ALIAS	 BASECLASS PREVIEWMODE OLDSETDELETE OLDREPROCESS	 NWORKAREA OLDBUFFERING OLDMULTILOCKS OLDSETFIELDS
 OLDREFRESHW ��  � � ���� J�� DISABLE�(� � %�C�	 oWindNameb� O��U � T� � ��  �� �/� T� � �-�� ��C� ENABLED-� � �� T� � � �a�� T� � � ��  �� T� � �	 ��  �� T� �
 �-�� %�C� P01PU01s��+�M s��4��� "�� \<Close���CC�	 LACTRSTATb� U� a� C�� � DISABLE6�� � � �� � � � � %�� � ��t� T� � �-�� ��C� � � �� �$ %�C� THIS.oWindParentb� L���� B� � %�C� TALKv� ON���� G2� T� � �� ON�� ��� T� � �� OFF�� � T� � �� � �� ��C� � �� ��C� � �� ��C� � �� T� �� � �� %�C�� �
��P� � U 	 OWINDNAME	 LACTRSTAT THIS OWINDPARENT SETALL CMDCALC ENABLED LCKEY CONTROLSOURCE VALUE VISIBLE P01PU01 CGRIDREF CWIZHOMEPATH	 SEPARATOR CWIZSTYFILE	 FIRSTTIME DOCK OLDTALK BMPPATH OARIAAPPLICATION
 BITMAPHOME INITVARS BUTTONREFRESH
 NAVREFRESH GRIDREF  U  �  %��9�  ���� �? %�C� THIS.PARENT.oWindParentb� O� C� � � � �
	��� � T� � �-�� ��C� ENABLED-� � �� T� � � �a�� � �� � T� � �-�� ��C� ENABLED-� � �� T� � � �a�� � U 	 FORMCOUNT THIS PARENT OWINDPARENT NAME SETALL CMDCALC ENABLED buttonrefresh,     �� initvars�    ��
 updaterows�    ��
 setcaptionx    ��
 navrefresh�    ��
 getgridref�    ��
 getkeyexprw    �� filewaschanged�&    �� changetoaddmode�)    �� getnamea1    �� Destroy62    �� Init�7    �� Refresh�:    �� Activate�:    ��1 3� A ���B�� � 4 �� � Cfaaac�aaaa11111111�� � � A � � � � � � 111111111A [4BA A 5 }�� � � � � A�B�� � � � �B A� A A �$A� � �Q A A ��A A �� Q� � � Q A A A A A AA � Q� A A Q A � Q � Q A A A A #� �����!����Q�� RR � Q A � A !A A A � !A A � � !!A B � � 3 a1212cqb�B � ABABb�B A 2 � !� 2� A ��a�A � � � Q A �� � A A A Q A B vqqq� qR�AbaA 3!� qQqqqq�� RA � � � C�AB � A � "QQQQQQBQqQQQ3 � C� ��1qqAA A A A A A A 3 q � � !� A � !A1� A �� !�B�2�aa�A �����aa�A Q�aa�A A A � 2 �� �A �a�Q��q A A A A A � 1�Q��q A A A A A A A q 4 q � �Q� !A � A � !B1� A �� !�B�2a����1�� aA Q1�� aA �a�� A A ���aB A 3 q q � q�A 1� 2 r� �A A a A � �� �A ��!�� �A A � ��� �A A A A A A A ra A � a � A A ���A A ra A ba A � 3 q � A�� � �11� a�A A 2� A AA A �a !� 1A 1� � � !C 3 1 1�� �A � � �A 1                       �        �  R     F   s  K&  �   �   l&  �-  !  �   �-  �>  K    �>  B  �  6  <B  ZM  �  m  M  `R  )  �  �R  �]  R  �  �]  �^  �  �  �^  Th  �    oh  �m    8  �m  �o  @  :  �o  �p  R   )   �B                       ���    �  �                        �   %   ;      �  +   I          �  U    ��  � � � � � ������� J��  �(� � � %�C� � � �
���� T�  ���� +�CC��  � � � ]�
���� %�CC��� � �
��� �" � � �C� � �������� � %�C� � f� ARIAFORM���B T� � �C� � �������C� � � C��  � � � ]�	 �
 �� �Q�2 T� � �C� � �������C��  � � � ]�� �/ T� � �C� � �������C�  � � � ���2 T� � �C� � �������C��  � � � ]�� T�  ��  ��� � T� �CW�� F�� � � �� %�CC��� � �
����' T� � � �� � � � � � ���/ T� � � �� � � � � � � � ��� T� � � �C� � � ��� T� � � �C� � ���� T� � � �a�� %�C� � � �
����; T� �C� � � � � B� �  6C� � � � � I� �  6��V T� � � �CCC�CC�]g� � � ]�>C�� � � � � � CC� �� � N� � 6�$��% T� � � �C� � �
� � � F��) T� � � �CCC�CC�]g� � � ]�>�� T� � � �a�� ��� T� � � �-�� � ��� T� � � �-�� T� � � �-�� �
 F�� �� � U  LNCOUNT LCFONTSTYLE LCALIAS THIS FILETAGS	 ARIAGRID1 RECORDSOURCE THISFORM CLASS THISFORMSET TRANSLATETOENGLISH TOP ARIATEXTBOX1 HEIGHT ARIACOMBOBOX1 VALUE NUMBEROFELEMENTS VISIBLE FONTBOLD
 FONTITALIC WIDTH FONTNAME FONTSIZE	 MAXLENGTH ENABLED Init,     ��1 � aa� ��!A �"� !A �!A � �q���a�cQ�� A � A � A 2                       e      )   �                       ����    �  �                        `   %   g      �     v          �  U  � ��  � � %�C� � �
��d� T� �CW�� F�� � � ��A T�  �C� � � � � � B� �  6C� � � � � � I� �  6�� T� � � � ��  ��] T� � � �	 �CCC� � �� � �
 �>C�� � � � � � � � CC�  �� � N� �  6�$��. T� � � �	 �C� � �	 �
� � � �	 F��* T� � � � �CCC� � �� � �
 �>�� T� � � � �a��
 F�� �� ��� T� � � � �-�� T� � � � ��  �� � U  LCFONTSTYLE LCALIAS THIS VALUE PARENT ALIAS ARIATEXTBOX1 FONTBOLD
 FONTITALIC WIDTH FILETAGS	 LISTINDEX FONTNAME FONTSIZE	 MAXLENGTH ENABLED Valid,     ��1 � 1� a���A� � AaA 1                       |      )   �                       ����    �  �                        �U   %   .
      M  q   k
          �  U  @ ��  � � � �� � �  T� �CC�t�� � +� � 6�� � ������� J��  �(� � ��C � �  � � �� T� ��  �� �� ���(�C�� ����0�9 T� �C�  � .CC � �� �� FIELD� CAPTION� � �� %�C� �
��� � T� �� �����C� ��� �. T� �� CC� �
� � � �  6C � �� �� ��	 B�� �� U	  TCALIAS TCEXPRESSION TCSEPERATOR LNCOUNT
 LAEXPARRAY GFSUBSTR	 LCENGNAME THIS	 DBGETPROP  ��  � � � �� � � � � � �	 � T�  �C�  f�� T� �C� f�� T� �C� f�� %�� � FIELD��>� %�C� .�  �� ��:� T� �C�  �C� .�  ��\�� T� �C�  C� .�  ��\�� T� �CC� &��� %�C� \� �� ��� T� �C� C� \� ��\�� � T� �C� � .DBF�  ��� T�  �� � .� �� � � H�O��� �� � DATABASE���� %�C�  ����� G(��  �� ���	 B��  �� �! �� � FIELD,TABLE,VIEW���� %�� � FIELD���� T� �C� DATABASE� ��� �� T� �C� DATABASE�  ��� � T� ��  �� %�C� \� �� ��� T� �C� �C� \� �\�� T� �C� � �  ��� T� �C� � .DBC�  ��� � %�C� �
� C� �
	���� ���� � �� � %�C� �
���� G(�� �� � � %�C� �
���� T�	 �C�  � � ��� �� T�	 ��  �� �	 B��	 �� U
  TCNAME TCTYPE
 TCPROPERTY LNCOUNT LCALIAS LCFIELDNAME LCTABLENAME
 LCDATABASE LCPATH LCRETRUNVALUE� ��  � � � � ������� J��  �(� � � %�� 
��� �, T� � �C� � �������� No Order��$ T� � �C� � ��������  ��$ T� � �C� � ��������  �� � %�C�� ��� � T� � ��  �� �� � T� � �C�� � %�C� � �
���� T� ���� +�CC�� � � ]�
��� %�CC��� � �
��j�" � � �C� � �������� �< T� � �C� � �������C� � C�� � � ]� � ��, T� � �C� � �������C� � � ���/ T� � �C� � �������C�� � � ]�� T� �� ��� � T� �CW�� F�� � �� %�CC��� � �
���� T� � �	 �C� � ��� T� � �
 �C� � ���� T� � � �a�� %�C� � �	 �
����; T� �C� � � � � B� �  6C� � � � � I� �  6��S T� � � �CCC�CC�]g� � ]�>C�� � � � � � CC� �� � N� � 6�$��% T� � � �C� � �
� � � F��& T� � � �CCC�CC�]g� � ]�>�� T� � � �a�� ��� T� � � �-�� � ��� T� � � �-�� T� � � �-�� �
 F�� �� � U  TCALIAS LORDER THIS FILETAGS ALIAS LNCOUNT TRANSLATETOENGLISH LCALIAS ARIACOMBOBOX1 VALUE NUMBEROFELEMENTS VISIBLE LCFONTSTYLE ARIATEXTBOX1 FONTBOLD
 FONTITALIC WIDTH FONTNAME FONTSIZE	 MAXLENGTH ENABLED translatetoenglish,     ��	 dbgetprop�    �� Init^    ��1 � � 1� a� ���A �A � 2 � �� � � bq��q�A �aA A � �� � � A a�� �A � q�Q�A �� A � A A a� � A � 2 � a� �AAA � � A 1� ��!A ���A � � ���a�3Qa� A � A � A 2                       T        t  F     C   a  �  G    )   �                       	PROCEDURE Click
LPARAMETERS tlSaveWithoutAsk
THIS.VALUE=0
** Reverting record
*IF THIS.Parent.ActiveMode $ 'AE'
*  This.Parent.oWindParent.Undo(tlSaveWithoutAsk)
*ELSE
  This.Parent.oWindParent.Edit()
*ENDIF

** Editing record
*E000000,1 Hesham (Start)
*THIS.Parent.EditMode = THIS.Parent.oWindParent.EditMode
*THIS.Parent.SelectMode = THIS.Parent.oWindParent.SelectMode
*THIS.Parent.AddMode = THIS.Parent.oWindParent.AddMode
*DO CASE
*  CASE THIS.Parent.ADDMODE AND THIS.Parent.SELECTMODE
*    THIS.Parent.ActiveMode = 'S'
*  CASE THIS.Parent.ADDMODE AND THIS.Parent.EDITMODE
*    THIS.Parent.ActiveMode = 'A'
*  CASE THIS.Parent.EDITMODE AND !THIS.Parent.ADDMODE
*    THIS.Parent.ActiveMode = 'E'
*  OTHERWISE
*    THIS.Parent.ActiveMode = 'V'    
*ENDCASE    
THIS.Parent.oWindParent.ActiveMode = THIS.Parent.ActiveMode
*E000000,1 Hesham (END)
*THISFORM.LockScreen = .T.     && WAM
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .T. 
ENDFOR  
*WAM
THIS.Parent.ButtonRefresh()
THIS.Parent.NavRefresh()
*THISFORM.LockScreen = .F.  && WAM
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F. 
ENDFOR  
*WAM
ENDPROC
     �PROCEDURE Click
LOCAL oSearchDlog
THIS.VALUE = 0

IF !EMPTY(THIS.PARENT.oWindParent.BrowseFields)
  PRIVATE oBrowse,llSelected
  IF ALIAS()# THIS.PARENT.nWorkArea
    SELECT (THIS.PARENT.nWorkArea)
  ENDIF
  
  oBrowse = .Null.
  DO FORM BROWSE WITH THIS.PARENT.oWindParent.BrowseFields,THIS.PARENT.oWindParent.BrowseTitle,;
                      .F.,.F.,.F.,.T.    TO llSelected

  IF llSelected
    THIS.Parent.oWindParent.OldRec = RECNO()  
    THIS.Parent.OldRec = THIS.Parent.oWindParent.OldRec 
    
    THIS.Parent.TopFile = .F.
    THIS.PARENT.oWindParent.TopFile=THIS.Parent.TopFile 
    THIS.Parent.EndFile = .F.
    THIS.Parent.oWindParent.EndFile = THIS.Parent.EndFile     
    
    This.Parent.oWindParent.ChangeMode('V')
    THIS.Parent.NavRefresh()
    THIS.Parent.ButtonRefresh()
    
    RETURN .T.
  ENDIF

  RETURN .F.
ELSE
  RETURN .F.
ENDIF

ENDPROC
      �Top = 3
Left = 5
Height = 22
Width = 22
Picture = ..\bmps\tbtop.bmp
Alignment = 0
ToolTipText = "Top of the file"
Name = "cmdTop"
     �PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.Parent.Error(nError, cMethod, nLine)
ENDPROC
PROCEDURE MouseDown
LPARAMETERS nButton, nShift, nXCoord, nYCoord
IF nXCoord<THIS.LEFT OR nYCoord<THIS.TOP OR nXCoord>THIS.LEFT+THIS.WIDTH OR;
  nYCoord>THIS.TOP+THIS.HEIGHT
  THIS.VALUE=0
  THIS.REFRESH
  this.MouseInFocus=.T.  
ELSE
  THIS.VALUE=1
  THIS.REFRESH
  this.MouseInFocus=.T.
ENDIF  
ENDPROC
PROCEDURE MouseMove
LPARAMETERS nButton, nShift, nXCoord, nYCoord
IF nXCoord<THIS.LEFT OR nYCoord<THIS.TOP OR nXCoord>THIS.LEFT+THIS.WIDTH OR;
  nYCoord>THIS.TOP+THIS.HEIGHT
  THIS.VALUE=0
  THIS.REFRESH
ELSE
  IF nButton=1 AND this.MouseInFocus
    THIS.VALUE=1
    this.MouseInFocus=.F.
  ELSE    
    THIS.VALUE=0  
  ENDIF  
ENDIF

ENDPROC
PROCEDURE MouseUp
LPARAMETERS nButton, nShift, nXCoord, nYCoord
THIS.VALUE=0
THIS.REFRESH
this.MouseInFocus=.F.
ENDPROC
     nPROCEDURE Init
IF !EMPTY(THIS.ControlSource) AND;
  TYPE('THIS.ControlSource')='C' AND TYPE('THIS.TransportValues')='C'
  THIS.AriaControlSource=THIS.ControlSource
  THIS.ControlSource=''
  THIS.TransportValuesLen=LEN(EVAL(THIS.AriaControlSource))
ENDIF  
ENDPROC
PROCEDURE Refresh
IF TYPE('THIS.TransportValues')<>'L' AND EMPTY(THIS.ControlSource) AND;
  TYPE('THIS.AriaControlSource')='C' 
    THIS.VALUE=CEILING(ATC(EVAL(this.AriaControlSource),THIS.TransportValues)/THIS.TransportValuesLen)
ENDIF
ENDPROC
PROCEDURE Valid
IF TYPE('THIS.TransportValues')<>'L' AND EMPTY(THIS.ControlSource) AND;
  TYPE('THIS.AriaControlSource')='C' 
  REPLACE (THIS.AriaCONTROLSOURCE) WITH SUBSTR(THIS.TransportValues,THIS.Value,THIS.TransportValuesLen)
ENDIF
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
     �PROCEDURE DblClick
IF GLOBALBROWSEWINDOW.MultiSelect
      GLOBALBROWSEWINDOW.ADDSelect()
ELSE
  DEACTI WINDOW (GLOBALBROWSEWINDOW.BrowseTitle)
  GLOBALBROWSEWINDOW.KeywasPress = "CMDSELECT"
  RETURN
ENDIF



ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl
PRIVATE oIncSearch
DO CASE
  CASE nShiftAltCtrl < 2 AND (BETWEEN(nKeyCode,48,57) OR BETWEEN(nKeyCode,65,90) OR BETWEEN(nKeyCode,97,122))
    oIncSearch = CREATEOBJECT('ARIAINCREMENTALSEARCH',THIS,nKeyCode)
    oIncSearch.SHOW(1)
  CASE (nShiftAltCtrl = 3 OR nShiftAltCtrl = 2) AND nKeyCode = 148
      DEACTI WINDOW (GLOBALBROWSEWINDOW.BrowseTitle)  
      GLOBALBROWSEWINDOW.KeyWasSelect = "CMDSELECT"      
  CASE (nShiftAltCtrl = 0 AND nKeyCode = 13) OR (nShiftAltCtrl = 2 AND nKeyCode = 10)
    IF GLOBALBROWSEWINDOW.MultiSelect
      GLOBALBROWSEWINDOW.ADDSelect()
    ELSE
      DEACTI WINDOW (GLOBALBROWSEWINDOW.BrowseTitle)    
      GLOBALBROWSEWINDOW.KeywasPress = "CMDSELECT"
      RETURN
    ENDIF
  CASE (nKeyCode =121 AND nShiftAltCtrl= 1) OR nKeyCode = 93
*     MOUSE AT THIS.TOP,THIS.LEFT PIXELS
     THIS.MOUSEUP(2)
ENDCASE   
ENDPROC
PROCEDURE MouseUp
LPARAMETERS nButton, nShift, nXCoord, nYCoord
DO CASE 
 CASE nButton = 1 AND nShift = 2
  GLOBALBROWSEWINDOW.AddSelect()
 CASE nButton = 1 AND nShift = 1 AND GLOBALBROWSEWINDOW.MultiSelect
    GLOBALBROWSEWINDOW.AddRegon(RECNO())
 CASE nButton = 1 AND GLOBALBROWSEWINDOW.MultiSelect
    GLOBALBROWSEWINDOW.LastSelection = EVAL(GLOBALBROWSEWINDOW.SelectField)
 CASE nButton = 2
  GLOBALBROWSEWINDOW.RightClick()  
ENDCASE
ENDPROC
PROCEDURE Error
LPARAMETERS nerror,cmethod,nline
THIS.PARENT.ERROR(nError, cMethod, nLine)
ENDPROC
     �PROCEDURE Click
THIS.Value = 0
THIS.Parent.oWindParent.Print()

*#DEFINE C_MAKEREPO_LOC			"Could not locate a report to print. Create new one?"
*#DEFINE C_NOOPEN_LOC			"Error opening table. Unable to print report."
*#DEFINE C_GETFILEPROMPT_LOC		"Select a report to print:"
*LOCAL cRepName,nSaveSess,cSaveAlias,cSaveSource,cSaveData 
*THIS.VALUE=0
*cSaveAlias = ALIAS()
*cSaveSource = CURSORGETPROP("SourceName")
*cSaveData = CURSORGETPROP("Database")
*cDiffSource = ""
*cRepName = LEFT(ALIAS(),8)+".FRX"
*nSaveSess = SET("DATASESSION")

*#IF 0
*	* Handling for Private data sessions
*	IF m.nSaveSess # 1
*		SET DATASESSION TO 1
*		SELECT 0
*		IF !EMPTY(m.cSaveData)
*			OPEN DATABASE (m.cSaveData)
*		ENDIF
*		IF USED(m.cSaveAlias)
*			SELECT (m.cSaveAlias)
*			IF CURSORGETPROP("SourceName")#m.cSaveSource
*				cDiffSource = CURSORGETPROP("SourceName")
*				USE IN (m.cSaveAlias)
*				SELECT 0
*			ENDIF
*		ENDIF	
*		IF EMPTY(ALIAS())
*			USE (m.cSaveSource) AGAIN ALIAS (m.cSaveAlias) SHARED
*			IF EMPTY(ALIAS())
*				=MESSAGEBOX(C_NOOPEN_LOC,0,_screen.Caption)
*				RETURN
*			ENDIF
*		ENDIF
*	ENDIF
*#ENDIF

*IF FILE(m.cRepName)
*	REPORT FORM (m.cRepName) PREVIEW NOWAIT
*ELSE
*	m.cRepName = GETFILE("frx",C_GETFILEPROMPT_LOC,"",1)
*	IF !EMPTY(m.cRepName)
*		IF FILE(m.cRepName)
*			* User pressed Open button
*			REPORT FORM (m.cRepName) PREVIEW NOWAIT
*		ELSE
*			* User pressed New button
*			DO (_WIZARD) WITH "AUTOREPORT"
*		ENDIF
*	ENDIF
*ENDIF

*#IF 0
*	IF !EMPTY(cDiffSource)
*		USE (m.cDiffSource) IN 0
*	ENDIF
*
*	SET DATASESSION TO m.nSaveSess
*	SELECT (m.cSaveAlias)
*#ENDIF
ENDPROC
      �Caption = "Control Panel"
Height = 28
Left = 1
Top = -2
Width = 444
ControlBox = .F.
firsttime = .T.
activemode = ("V")
Name = "ariacontroltoolbar"
     >formhastoolbar
previewmode
addmode
editmode
topfile
endfile
oldrec
nworkarea
allowadd
allowedit
allowdelete Specifies whether delete operations are allowed.
oldalias
selectmode
usedataenv
sessionisreadonly
browsefields
browsetitle
activemode
key
keyobject
hasmemo
editforecolor
editdisforecolor
editbackcolor
editdisbackcolor
pop_name
bar_no
multirun
*getmasterfile 
*refreshall 
*dbgetprop 
*translatetoenglish 
*setpath 
*seekrecord 
*gotop 
*goprevious 
*gonext 
*goend 
*close 
*delete Delete Event.
*addnew 
*savefiles 
*edit 
*undo 
*filewaschanged 
*cancontinue 
*opentables Programmatically opens the tables and views associated with the data environment.
*changemode 
*print Prints a character string on a Form object.
*setallprop 
*getkeyexpr 
*getname 
*beforesave 
     	"PROCEDURE addshortcut
lParameters tcCation,toObject,tcMethod,tcStatus,tnIndex
IF PCOUNT()=0 OR TYPE('tcCation')<>'C'
  RETURN
ENDIF
tcStatus = IIF(TYPE('tcStatus')='C',tcStatus,'T')
LOCAL llAdd
llAdd = IIF(TYPE('tnIndex')='N',.F.,.T.)
tnIndex = IIF(TYPE('tnIndex')='N',tnIndex,ALEN(THIS.LIST,1)+IIF(EMPTY(THIS.LIST[1,1]),0,1))
IF !llAdd AND tnIndex > ALEN(THIS.LIST,1)
  RETURN
ELSE 
  IF llAdd
    DIMEN  THIS.LIST[tnIndex,ALEN(THIS.LIST,2)]
  ENDIF
  THIS.LIST[tnIndex,1] = tcCation
  THIS.LIST[tnIndex,2] = toObject
  THIS.LIST[tnIndex,3] = tcMethod
  THIS.LIST[tnIndex,4] = tcStatus
ENDIF  
ENDPROC
PROCEDURE removeshortcut
lParameters tnIndex
IF TYPE('tnIndex')<>'N' OR ALEN(THIS.LIST,1) < tnIndex OR tnIndex=0
  RETURN
ENDIF
=ADEL(THIS.LIST,tnIndex)
IF ALEN(THIS.LIST,1)>1
  DIME THIS.LIST[ALEN(THIS.LIST,1)-1,ALEN(THIS.LIST,2)]
ENDIF
ENDPROC
PROCEDURE clearshortcut
DIMEN THIS.LIST[1,4]
STORE .F. TO THIS.LIST
ENDPROC
PROCEDURE resetall
THIS.ALIAS = .F.
THIS.BrowseFields = .F.
THIS.BrowseTitle  = .F.
THIS.BrowseKey  = .F.
THIS.BrowseFor  = .F.
THIS.DefaultShortCut = 'TTTT'
THIS.SelectButton = .T.
THIS.SelectButtonReference = .F.
THIS.SelectButtonMethod = .F.
THIS.CLEARSHORTCUT()
ENDPROC
PROCEDURE browse
LOCAL llReturnValue,lcAlias
lcAlias = SELECT()
IF !EMPTY(THIS.Alias)
  SELECT (THIS.Alias)
ENDIF
IF EMPTY(THIS.BrowseFields)
  THIS.BrowseFields=gfDataBaseProp('Get',ALIAS(),'Table','BrowseFields')
ENDIF
PRIVATE oBrowse,laShortCut
oBrowse = .Null.
=ACOPY(THIS.LIST,laShortCut)
*--YMA
lcToRun = oAriaApplication.ScreenHome + "\SY\BROWSE"
DO FORM (lcToRun) WITH THIS.BrowseFields,this.BrowseTitle,THIS.BrowseKey,;
                  THIS.BrowseFor,THIS.DefaultShortCut,THIS.SelectButton,;
                  THIS.SelectButtonReference,THIS.SelectButtonMethod,'laShortCut',;
                  THIS.MultiSelectAlias,THIS.MultiSelectField ;
                  TO llReturnValue
*DO FORM BROWSE WITH THIS.BrowseFields,this.BrowseTitle,THIS.BrowseKey,;
                    THIS.BrowseFor,THIS.DefaultShortCut,THIS.SelectButton,;
                    THIS.SelectButtonReference,THIS.SelectButtonMethod,'laShortCut',;
                    THIS.MultiSelectAlias,THIS.MultiSelectField TO llReturnValue
*--YMA
SELECT (lcAlias)    
RETURN llReturnValue    

ENDPROC
     $PROCEDURE Click
This.Value=0
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .T. 
ENDFOR  
*WAM
IF This.Parent.oWindParent.ActiveMode $ 'EA'
	** Code for saving record

  THIS.Parent.oWindParent.BeforeSave()
	IF !THIS.Parent.oWindParent.SaveFiles() 
	  *WAM
    FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
      This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F. 
    ENDFOR  
    *WAM
	  RETURN
	ELSE
	  THIS.Parent.oWindParent.ChangeMode(SUBSTR('SV',ATC(THIS.Parent.oWindParent.ACTIVEMODE,'AE'),1))  
	ENDIF
ELSE
  This.Parent.oWindParent.AddNew
ENDIF
This.Parent.ActiveMode = This.Parent.oWindParent.ActiveMode
THIS.Parent.TopFile = THIS.Parent.oWindParent.TopFile
*THISFORM.LockScreen = .T.      && WAM
THIS.Parent.ButtonRefresh()
THIS.Parent.NavRefresh()
*THISFORM.LockScreen = .F.      && WAM
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F. 
ENDFOR  
*WAM

ENDPROC
     xPROCEDURE Click
THIS.VALUE=0
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .T. 
ENDFOR  
*WAM
This.Parent.oWindParent.GoNext()
THIS.Parent.NavRefresh()
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F. 
ENDFOR  
*WAM
ENDPROC
     ~PROCEDURE Click
THIS.VALUE=0
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .T. 
ENDFOR  
*WAM
This.Parent.oWindParent.GoPrevious()
THIS.Parent.NavRefresh()
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F. 
ENDFOR  
*WAM

ENDPROC
     zPROCEDURE Click
THIS.VALUE=0

*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .T. 
ENDFOR  
*WAM

This.Parent.oWindParent.GoTop
THIS.Parent.NavRefresh()
*WAM
FOR lnFrmCnt = 1 TO This.Parent.oWindParent.FormCount
  This.Parent.oWindParent.FORMS(lnFrmCnt).LockScreen = .F.
ENDFOR  
*WAM

ENDPROC
      �DataSession = 2
Height = 133
Width = 209
DoCreate = .T.
AutoCenter = .T.
Caption = "Form1"
FontName = "MS Sans Serif"
FontSize = 8
MaxButton = .F.
KeyPreview = .T.
bar_no = 0
pop_name = ("")
multirun = .T.
moduleid = ("")
Name = "ariaform"
     =syspath System files Directory
datadir Data Files Directory
user_id User ID loged in the system
user_name User Name Logged in the system
user_level User Level for the user logged in the system
user_group User Group for the user Logged in the system
activemoduleid Active Module ID
activecompanyid Active Company ID
activemodulename Active Module Name
activecompanyname Active Company Name
activemodulebuildnumber Active Module Build Number
systembuildnumber System Build Number
winsepta Hold the position of the last bar in the windows popup in the menu
loginrequired System Login Requierd or Not
usercompanyid Users Default Company ID
companybar Active Company Bar Number
mapdrive The system default running path is in a map drive
workdir Work Directory path
reporthome Reports Applications Directory
applicationhome Module's Application Directory
bitmaphome BitMap's Directory
resourcehome Resource Files Directory
helptopic Active Help Topic
systemname System Full Name
defaultpath Default Path
installpath System Path saved in the installation file
parentmodule Determine if Active Module is a Parent Module
station The workStation ID
prntcompanyid Pranet Company ID
basecurrency Base Currency ID
phonemask Phone Input Mask
companyinstalledmodules Company Installed Modules
oldwindcaption
usermoduleid User default Module ID
multirun
bar_no
pop_name
helpfile
otoolbar
firsttime
endprog
allowadd
allowedit
allowdelete Specifies whether delete operations are allowed.
processid Property Hold the last running program name
defaultcountry
currentyear Company Current Year
currentperiod Company Current Period
systemdate Current SystemDate
companysetupmodules
userstaticrecord
screenhome
currentsite
shutdown
userusetoolbar
ariaversion
classdir
edipath
programmoduleid
dataenv
errorhandlerenabled
lcaria4class Property to hold the Aria4XP classes folder
isremotecomp Property to Check if the current company Use remote data access or not
remotecompanydata Property to hold object from remote data access
activecompanyconstr Property to hold the active company connection string
cfoxdbid
csqldbid
cnativedbid
programhome
rebldcmp
*changecompany Method To change the selected Company
*changemodule Method To change the Active Module
*login Method For the User Login
*setmenu Function To Set the Menu for the system
*setenvironment Method to set the environment
*restoreenvironment Method to restore the environment
*cleanup Method For Clean Up the system before ending
*messagebox Method To display message Box
*restoretoolbars Method To Restore the fox toolbars before leaving the system
^atoolbars[1,1] Property to hold the active toolbars of fox to restore it before releasing the system
^aprocess[1,1] Menu Processes array
*systemsetup System Setup Method
*companysets Company Setup Method
*getset 
*sethlpfl Set the Module Help file
*programsetup Program Setup function to initialize all the program Settings
*doprogram Method Called From Menu to run the program
*runprog Method To Call the Module with the form to run
*menubar 
*sethelptopic 
*programcleanup All Forms Clean up function
*winarng Function to Arrange windows in the menu
*singleuserprogram Function To return if the program we want to run is a single user or not
*adduserinformation Function To Add the user information to a file
*reportprint Method to Run a Report
*doformretval 
*do 
*maincont 
*sysexit 
^asets[1,1] Environment Settings before the system run
*releasetoolbars Method To Release The fox toolbars before running the system
*getuserinfo Method to Get The Users Information
*getcompanyinformation 
*getmoduleinformation 
*help 
*tooladdnew 
*toolgobttm 
*toolclose 
*tooldelete 
*tooledit 
*toolgonext 
*toolgoprev 
*toolsave 
*toolgotop 
*validatact_key 
*validactivationkey 
*loguser 
*setmultiappmenu 
*getdatadir 
^laevnttrig[1,1] array hold the events trigger
*validkeyvalu 
*activecompanyid_assign 
*mreadconstr method to read the connection information from from the Syccomp
*mgenconstr Method to generate the connection string
*rebuildcomp 
     m����    m  m                        �0   %   ca      vk    rc          �  U   ��  �( %�C� THIS.DATAENVIRONMENTb� O�� � %�C� � � �
��T � B�� � � �� �� � T� �C�� � � ��b�� ��� � ��� � ��  ���(�� ��� �3 %�CC� .C �  � �
 .BaseClass�f� CURSOR��� � B�C� .C �  � � .ALIAS��� � �� �� � ��	 B��  �� � U  LNCOUNT THIS DATAENVIRONMENT INITIALSELECTEDALIAS LNTOTMEM AMEMS ��  � �� � � %�C�� ��1 � T�� �� �� �J � T�� ���  �� �$ %�C� m.oControlParentb� O��v � B� � H�� �H�" �C� Formset�� � �� ��� � T� �� � ��$ �C�	 Pageframe�� � �� ��� � T� �� � ��3 �C�� � � Optiongroup,Commandgroup�� ��0� T� �� � �� 2�H� T� �� �	 �� � %��� � ��c� B� � �� ���(��� ��� H����" �C� Formset�� � �� ���� ��CC �� �� � �
 � ��$ �C�	 Pageframe�� � �� ��� ��CC �� �� � �
 � ��3 �C�� � � Optiongroup,Commandgroup�� ��W� ��CC �� �� � � ��= �CC �� �� � � � Optiongroup,Commandgroup�� ���� ��CC �� �� � �
 � ��Q �C�	 ContainerC �� �� � � �� �  C� PageC �� �� � � �� ��� ��CC �� �� � �
 � ��4 �CC �� �� � � � ListBox,Spinner�� ��j� ��CC �� �� � � ��E �CC �� �� � � �  CheckBox,TextBox,OleBoundControl�� ���� ��CC �� �� � � ��, �CC �� �� � � � EditBox�� ��� ��CC �� �� � � �� � �� U 
 OCONTAINER I OCONTROLPARENT THISFORMSET	 BASECLASS
 NCTRLCOUNT	 FORMCOUNT	 PAGECOUNT BUTTONCOUNT CONTROLCOUNT THIS
 REFRESHALL FORMS PAGES BUTTONS REFRESH CONTROLS  ��  � � � �� � � � � � �	 � T�  �C�  f�� T� �C� f�� T� �C� f��: %�C� �$ CONNECTION,DATABASE,FIELD,TABLE,VIEW�� ��� �	 B��  �� � %�� � FIELD���� %�C� .�  �� ���� T� �C�  �C� .�  ��\�� T� �C�  C� .�  ��\�� T� �CC� &��� %�C� \� �� ��N� T� �C� C� \� ��\�� � T� �C� � .DBF�  ��� T�  �� � .� �� � � H����� �� � DATABASE���� %�C�  ����� G(��  �� ���	 B��  �� �! �� � FIELD,TABLE,VIEW���� %�� � FIELD��2� T� �C� DATABASE� ��� �U� T� �C� DATABASE�  ��� � T� ��  �� %�C� \� �� ���� T� �C� �C� \� �\�� T� �C� � �  ��� T� �C� � .DBC�  ��� � %�C� �
���� ���� � �� � G(�� �� � T�	 �C�  � � ���	 B��	 �� U
  TCNAME TCTYPE
 TCPROPERTY LNCOUNT LCALIAS LCFIELDNAME LCTABLENAME
 LCDATABASE LCPATH LCRETRUNVALUE@ ��  � � � �� � �  T� �CC�t�� � +� � 6�� � ������� J��  �(� � ��C � �  � � �� T� ��  �� �� ���(�C�� ����0�9 T� �C�  � .CC � �� �� FIELD� CAPTION� � �� %�C� �
��� � T� �� �����C� ��� �. T� �� CC� �
� � � �  6C � �� �� ��	 B�� �� U	  TCALIAS TCEXPRESSION TCSEPERATOR LNCOUNT
 LAEXPARRAY GFSUBSTR	 LCENGNAME THIS	 DBGETPROP� ��  �F %�C� oAriaApplicationb� O� C� THIS.DATAENVIRONMENTb� O	���� T� �C�� � � ��b�� ��� � ���� ��  ���(�� ����3 %�CC� .C �  � �
 .BaseClass�f� CURSOR���� ��C� .C �  � ����� %�C�� �
��U� %�CC�� �=f� SY��(�! T�� �� � � SysFiles.dbc�� �Q�! T�� �� �	 � AriaData.dbc�� � ��� T�
 ��� ��4 T�
 �CC� \�
 �� � �
 � C�
 C� \�
 ��\6��3 T�� �CCC�
 �=f� SY�	 � � � � �	 6�
 �� � �� � �� �� � U  LNCOUNT LNTOTMEM AMEMS THIS DATAENVIRONMENT DATABASE CURSORSOURCE OARIAAPPLICATION SYSPATH DATADIR LCCURSOURCE ��  � �+ �� � � ������� � � � � %�C�  ���N �	 B�� �� � J�C� X�(� � T� �CW�� T� �C�	 �
 �� %�C� ���� �
 F�� ��	 B�� �� ���
 F�� �� T� �C�CC�]g]��
 ��Ca��� %�C�  ����� T�	 � �-�� T�	 � �-�� %�C�	 � ���:� ��C� V�	 � �� ��C�	 � �� ��C�	 � �� ��� T� � � ��	 � �� T� � � ��	 � �� ��C� V�	 � �� ��C� � � �� ��C� � � �� �
 F�� ��	 B���� ��� H���k�' �C�	 � �
� � � � � � 	���� %�C�	 lcMessageb� L��I�: T� �C� QRM00001B00001� Dialog� 'C�  �� '� � �� ���0 T� �C� QRM00001B00001� DialogC� �� � �� �4 �C�	 � �
� � � � � �	�	 �	 � -��k� %�C�	 lcMessageb� L���: T� �C� QRM00001B00014� Dialog� 'C�  �� '� � �� �H�0 T� �C� QRM00001B00014� DialogC� �� � �� � T� �C� �� �� � 6�� � H�|��� �� ����� T� �C� � � � ��
 F�� �� B�C� � �� � 6�� �� ����� ��C � � � +� �� ��C�	 � �� %��	 � � A���� T� ���� �� ���(�C�� ������! T� �C�  � CCC � �� �>\��3 REPLACE &laKeyFields[lnCount,1] WITH lcKeyValue
 T� �� CCC � �� �>�� �� � ��C�	 � ��
 F�� ��	 B���� �� �����
 F�� ��	 B�� �� � � �
 F�� �� U   TCSEEKEXPRESSION	 LCMESSAGE LCMASTERFILE
 LCOLDALIAS LAKEYFIELDS LNSUBSTRSTART
 LCKEYVALUE LNCOUNT LCKEYEXPRESSION THIS GETMASTERFILE TOPFILE ENDFILE FORMHASTOOLBAR
 CHANGEMODE
 REFRESHALL REFRESH OARIAAPPLICATION OTOOLBAR BUTTONREFRESH
 NAVREFRESH CMDADD CONTROLENABLE LNOPTION
 MESSAGEBOX ALLOWADD
 LLSELECTED CMDFIND CLICK GFSUBSTR ADDNEW
 ACTIVEMODEt  F��  � �� -� T�  � �a�� T�  � �C+�� %�C�  � �
��m � T� � � ��  � �� T� � � ��  � �� � U  THIS	 NWORKAREA TOPFILE ENDFILE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR�  F��  � �� %�C
��$ �
 H������ � T�  � �C�� T�  � �C+�� %�C�  � �
��� � T� � � ��  � �� T� � � ��  � �� � U  THIS	 NWORKAREA TOPFILE ENDFILE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR�  F��  � �� %�C+
��# �	 H���� � T�  � �C+�� T�  � �C�� %�C�  � �
��� � T� � � ��  � �� T� � � ��  � �� � U  THIS	 NWORKAREA ENDFILE TOPFILE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBARt  F��  � �� #6� T�  � �-�� T�  � �a�� %�C�  � �
��m � T� � � ��  � �� T� � � ��  � �� � U  THIS	 NWORKAREA TOPFILE ENDFILE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR 
 ��  � � U  THIS RELEASE� > %�C�" Do you want to delete this record?�$�9�  �x���� � F�� � �� � %�C� � ��� � %�C+
��v �	 H���� � %�C+� C
	��� �
 H������ � � � U  CAPTION THIS	 NWORKAREA	 SAVEFILES�  F��  � �� T�  � �C�� T�  � �CO��4 %�C�
 SourceType��� C� SendUpdates�
	��� �c ��C�O You cannot add a new record because the view(s) selected does not send updates.� �9� �x�� B� � � T�  � �-�� ��C� A�  � �� U  THIS	 NWORKAREA OLDALIAS OLDREC CAPTION TOPFILE
 CHANGEMODE} ��  � � � � � �� � � � �	 �
 � � � ���� �  ���� T�� ��  �� T�� �a�� T�� �CW�� T�� �C�� �y�� �� ���(��� ��-� F�C �� �� �� T��	 �CC� Databaseꉡ
�� T�� ��  �� T��
 �-�� T�� �-�� H���� �C�	 Buffering����/� .� �C� ����k� T�� �C-a��� %��� ��g� .� �; ���	 
�( C� 2C���ꐸ� � C� 3C���ꐸ� 	���� T�� �C� ��� +��� � ��V� #��� �� T�� �CS�� %��� 
��*�, T�� �� Record in use by another user�� Z� !� � %��� 
��>� ��� ���(�C.��:� %�CC�� /b� G��r� .� � %�CC�� /�_CC�� /���6� T�� �a��e %�C�I Data has been changed by another user. Overwrite changes with your edits?�4�9� �x���� T��
 �a�� �2� T�� �-�� Z� !� � � �� � T�� �C�� ��� � %��� ���� T�� �Ca��
 ��� %��� ���� .� � Z� � ���	 ���� ��� T�� �Ca-��� %��� ���� ��� .� � �� � T� �C��  �z�� H���� �� � ��� �C���  ���N� T�� �� Trigger failed.�� �C���  �-����( T�� �� Field doesn't accept NULL�� �C���  �.����" T�� �� Field rule violated�� �C���  �����, T�� �� Record in use by another user�� �C���  �/��H�  T�� �� Row rule violated�� �C���  �\����% T�� �� Unique index violation�� �C���  �1���� %���	 ����e %�C�I Data has been changed by another user. Overwrite changes with your edits?�4�9� �x����� ��� T�� �Caa��� %��� ��A� ��� .� �� ��2 ��C� Could not force table updates.� �9� �x�� � � � 2��� %�C�� �
����! T�� �� Error: C���  �� � �
 ��Ca��� T�� �-�� %�C�� �
��)�2 ��C� Failed to update table: �� � �9� �x�� � ��$ ��CC� VSC� � � EA��\� � �� T� � �-�� F��� �� B��� �� U  AERRORS CERRORMESSAGE ATABLESUSED NTABLESUSED NTOTERR NFLD I NOLDAREA LSUCCESS LINDBC
 LOVERWRITE LHADMESSAGE
 NMODRECORD CAPTION THIS
 CHANGEMODE
 ACTIVEMODE TOPFILE_  T�  � �C�� T�  � �CO�� ��C� E�  � �� %�C�  � �
��X � T� � � ��  � �� � U  THIS OLDALIAS OLDREC
 CHANGEMODE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR
 ACTIVEMODE� ��  � %��  
��~ � %�C� � ��z �L %�C�/ Are you sure you want to lose all your changes?��9� �x���v � B� � � � �� � � � � � %�� �	 ���� � ������� T� �C�� � �
 ��b�� ��� �
 ���� �� ���(�� ����3 %�CC� .C � � �
 .BaseClass�f� CURSOR���� ��C� .C � � ���}�! %�C�	 Buffering�� ����y� ��Ca�� ��� � �� � �� �� �� � ���� T�� �C�� �y�� �� ���(��� �� �  %�C�	 Buffering� ������ ��Ca� ��� � �� �$ ��CC� VSC� � � EA��\� � �� %�C� � �
���� T� � � �� � ��- %�C� oAriaApplication.oToolBarb� O���� T� � � �a�� ��C� � � �� ��C� � � �� T� � � �-�� � � U  TLSAVEWITHOUTASK THIS FILEWASCHANGED CAPTION LNTOTMEM LNCOUNT LAMEMS ATABLESUSED NTABLESUSED
 USEDATAENV DATAENVIRONMENT ALIAS
 CHANGEMODE
 ACTIVEMODE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR THISFORMSET	 ARIAFORM1
 LOCKSCREEN BUTTONREFRESH
 NAVREFRESH� ��  � � � � � %�� � ���� T� �C�� � � ��b�� ��� � ���� �� ���(��� ����5 %�CC� .C �� � �
 .BaseClass�f� CURSOR���� ��C� .C �� � ����� T�	 �C�	 Buffering��
 ��� H�� ��� �C�	 �����Q�L %�C�����
 �� 1C� 1C��
 .Q
�  C�����
 �� 3C� 3C��
 .Q
	��M� B�a�� � �C�	 ������� T�� �C� ��
 ��� %��� � ���� B�a�� � � �� � �� �� ��� �  ���� T� �C��  �y�� �� ���(�� ���� T�	 �C�	 Buffering� ��� H���� �C�	 �����}�H %�C���� �� 1C� 1C� .Q
� C���� �� 3C� 3C� .Q
	��y� B�a�� � �C�	 ������� T�� �C� � ��� %��� � ���� B�a�� � � �� � B�-�� U  ATABLESUSED LNTABLESUSED LNCOUNT NTOTMEM I THIS
 USEDATAENV AMEMS DATAENVIRONMENT LNTABLEBUFF ALIAS
 NMODRECORD�  %�C�  � ��� �r T� �C� You will lose all your changes.C� C�
 �+ Do you want to save changes before closing?�#�9� �x�� H�� �� � �� ���� � ��C�  � �� �� ���� � ��Ca�  � �� 2�� � B�-�� � � B�a�� U  THIS FILEWASCHANGED LNOPTION CAPTION	 SAVEFILES UNDOH ��  � G_ � G�(�� � ��( %�C� THIS.DATAENVIRONMENTb� O���� T� �C�� � � ��b�� ��� � ���� ��  ���(�� ����3 %�CC� .C �  � �
 .BaseClass�f� CURSOR���� T� ��  �� T� ��  �� T� ��  �� T�	 ��  �� T�
 ���� ��C� .C �  � ����� T� �C�� C� \�� ��\�� T� ��� �� T�	 ��� �� T�
 ��� �� %�C�� �
���� T� ��
 ORDER TAG �� �� � �� ��CC �  � � � � �� %�CC� f�=� SY��$� %�C� �
�� �O USE (oAriaApplication.SysPath+lcFileName) AGAIN ALIAS (lcAlias) IN 0 &lcTag
 � ��� %�C� �
����W USE (oAriaApplication.DataDir+lcFileName) AGAIN ALIAS (lcAlias) IN 0 &lcTag        
 � � %�C�	 �
���� L�
 F�� �� SET FILTER TO &lcFilter
 � ��C�	 BUFFERING�
 � ��� � �� �� �( %�C� THIS.DATAENVIRONMENTb� O��A� T� �C�� � � ��b�� ��� � ��=� ��  ���(�� ��9�5 %�CC� .C �  � �
 .BaseClass�f� RELATION��5� ��C� .C �  � ���� G-��� �(��� ����� �� %��� ��� T� �CW�� F��� �� GN(��� ��
 F�� �� � �� ��CC �  � � � � �� � �� �� � U  LNCOUNT THIS DATASESSIONID LNTOTMEM AMEMS DATAENVIRONMENT
 LCFILENAME LCTAG LCALIAS LCFILTER LNBUFFERMODE CURSORSOURCE ALIAS FILTER BUFFERMODEOVERRIDE ORDER REMOVEOBJECT RELATIONALEXPR
 CHILDALIAS PARENTALIAS	 ONETOMANY�  ��  �0 T�  �CC� lcModeToChangeb� C� �  � �  6�� %�C�  �
��� � T� � ��  �� ��C� � �� %�C� � �
��� � T� � � �� � �� � � U  LCMODETOCHANGE THIS
 ACTIVEMODE
 SETALLPROP FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR\ ��  � �� � � � � � T� �C�� T� �C�
 SourceName��� T� �C� Database��� T� ��  �� T� �CC�=� .FRX�� T� �C� DATASESSIONv�� %�C�� 0��� � ?��� ��:� �U�6 T�� �C� frx� Select a report to print:�  ���� %�C�� �
��Q� %�C�� 0��,� ?��� ��:� �M� ��<����
 AUTOREPORT�� � � � U  CTEXT CREPNAME	 NSAVESESS
 CSAVEALIAS CSAVESOURCE	 CSAVEDATA CDIFFSOURCE� ��  � �� � � %�C�� ��1 � T�� �� �� �J � T�� ���  �� �$ %�C� m.oControlParentb� O��v � B� � H�� �H�" �C� FormSet�� � �� ��� � T� �� � ��$ �C�	 Pageframe�� � �� ��� � T� �� � ��3 �C�� � � Optiongroup,Commandgroup�� ��0� T� �� � �� 2�H� T� �� �	 �� � �� ���(��� ���� H�r�W�~ �C�, m.oControlParent.Controls[m.i].ControlSourceb� U�; C�0 m.oControlParent.Controls[m.i].AriaControlSourceb� U	��W�7 %�CC �� �� �
 � �
� CC �� �� �
 � �	��S�, T�� �
 ��� �� �C �� �� �
 � �� � � H�h�a
�" �C� FormSet�� � �� ���� ��CC �� �� � � � ��$ �C�	 Pageframe�� � �� ���� ��CC �� �� � � � ��3 �C�� � � Optiongroup,Commandgroup�� ����2 %�CC �� �� � � � Commandbutton�� ����9 %�C�% m.oControlParent.Buttons[m.i].BtnTypeb� U��Q� H���M�! �C �� �� � � � K����% T�� � ��� �� �� � � S��! �C �� �� � � � B���' T�� � ��� �� �� � � EAV�� 2�M�& T�� � ��� �� �� � � EA�� � ��& T�� � ��� �� �� � � AE�� � ���& T�� � ��� �� �� � � AE�� �= �CC �� �� �
 � � Optiongroup,Commandgroup�� ��� ��CC �� �� �
 � � ��Q �C�	 ContainerC �� �� �
 � �� �  C� PageC �� �� �
 � �� ��{� ��CC �� �� �
 � � ��= �CC �� �� �
 � � ListBox,ComboBox,Spinner�� ��h�' %�CC �� �� �
 � fC� � f��6� %�� � � S��� T�� �
 ��� �� �a�� �2� T�� �
 ��� �� �-�� � �d�& T�� �
 ��� �� �� � � AE�� �E �CC �� �� �
 � �  CheckBox,TextBox,OleBoundControl�� ��]�' %�CC �� �� �
 � fC� � f��+� %�� � � S��� T�� �
 ��� �� �a�� �'� T�� �
 ��� �� �-�� � �Y�& T�� �
 ��� �� �� � � AE�� �2 �CC �� �� �
 � � Commandbutton�� ����: %�C�& m.oControlParent.Controls[m.i].BtnTypeb� U���� H�����! �C �� �� �
 � � K���% T�� �
 ��� �� �� � � S��! �C �� �� �
 � � B��d�' T�� �
 ��� �� �� � � EAV�� 2���& T�� �
 ��� �� �� � � EA�� � ���& T�� �
 ��� �� �� � � AE�� �, �CC �� �� �
 � � EditBox�� ��
�& T�� �
 ��� �� �� � � SV�� %�� � 
���	� ��C �� �� �
 ���	� T� � ��� �� T� � ��� �� T� � ��� �� T� � ��� �� T� � �a�� �� �: T�� �
 ��� �� �C� � � EA�	 � � � � � 6��: T�� �
 ��� �� �C� � � EA�	 � � � � � 6��) �CC �� �� �
 � � Grid�� ��a
�& T�� �
 ��� �� �� � � SV�� � H�r
���~ �C�, m.oControlParent.Controls[m.i].ControlSourceb� U�; C�0 m.oControlParent.Controls[m.i].AriaControlSourceb� U	��d� %�� � � S���� %�CC �� �� �
 � ���O�, T�� �
 ��� �� �C �� �� �
 � �� � %�CC �� �� �
 � �
���� T�� �
 ��� �� ��  �� �8 %�C�$ m.oControlParent.Controls[m.i].Valueb� U����" ��� �
 ��� �� �� Value�� � �`�7 %�CC �� �� �
 � �� CC �� �� �
 � �
	��\�, T�� �
 ��� �� �C �� �� �
 � �� � �U �C� m.oControlParent.Controls[m.i]b� U�  CC �� �� �
 � � Grid�� 	���� ��  �! �" �# �$ � %�� � � S��� %�CC �� �� �
 �% �����& ��  ���(�C �� �� �
 �& ��u�L T�� �
 ��� ��% �C �� �� �
 �% � ,C �  C �� �� �
 �' �( �� ��1 T�� �
 ��� ��% �C �� �� �
 �% � ,�� �& ��  ���(�C �� �� �
 �& ����& T�� �
 ��� ��' ��  ��( ��  �� �� T�� � �-�� ��� T�! �C �� �� �
 �% ��& ��  ���(�C �� �� �
 �& ����W T�� �
 ��� ��' ��  ��( �CC�! C� ,�! �  �C� ,�! �  ��C� ,�! �  �\� ,��� �� T�� � �a�� � � �� U) 
 OCONTAINER I OCONTROLPARENT THIS	 BASECLASS
 NCTRLCOUNT	 FORMCOUNT	 PAGECOUNT BUTTONCOUNT CONTROLCOUNT CONTROLS ARIACONTROLSOURCE CONTROLSOURCE
 SETALLPROP FORMS PAGES BUTTONS BTNTYPE ENABLED
 ACTIVEMODE KEY READONLY HASMEMO EDITFORECOLOR	 FORECOLOR EDITDISFORECOLOR DISABLEDFORECOLOR EDITBACKCOLOR	 BACKCOLOR EDITDISBACKCOLOR DISABLEDBACKCOLOR RESETTODEFAULT LNCOUNT	 LCCOLCONT LCBOUND LCCONTSOURCE LCSPARSE TAG COLUMNCOUNT COLUMNS CURRENTCONTROL� ��  � �� � � T� ��  �� %�C�� ��^ � T�� �� �� T� � ��  �� T� � ��  �� �w � T�� ���  �� �$ %�C� m.oControlParentb� O��� �	 B�� �� � H�� �z�" �C� FormSet�� � �� ��� � T� �� �	 ��$ �C�	 Pageframe�� � �� ��� T� �� �
 ��3 �C�� � � Optiongroup,Commandgroup�� ��b� T� �� � �� 2�z� T� �� � �� � �� ���(��� ���� H�����" �C� FormSet�� � �� ���� ��CC �� �� � � � ��$ �C�	 Pageframe�� � �� ��"� ��CC �� �� � � � ��3 �C�� � � Optiongroup,Commandgroup�� ��O� T� �C �� �� � � ��6 T� �CCC� .� �� � C� C� .� ��\� � 6f��& %�� Cm� C �� �� � � 	��K�= T� � �� � CC� � �
� � +� �  6C �� � � � ��A T� � �� � CC� � �
� � +� �  6CC �� � � � � �� �= �CC �� �� � � � Optiongroup,Commandgroup�� ���� ��CC �� �� � � � ��Q �C�	 ContainerC �� �� � � �� �  C� PageC �� �� � � �� ��� ��CC �� �� � � � ��= �CC �� �� � � � ListBox,ComboBox,Spinner�� ��L� T� �C �� �� � � ��6 T� �CCC� .� �� � C� C� .� ��\� � 6f��& %�� Cm� C �� �� � � 	��H�= T� � �� � CC� � �
� � +� �  6C �� � � � ��A T� � �� � CC� � �
� � +� �  6CC �� � � � � �� �E �CC �� �� � � �  CheckBox,TextBox,OleBoundControl�� ���� T� �C �� �� � � ��6 T� �CCC� .� �� � C� C� .� ��\� � 6f��& %�� Cm� C �� �� � � 	����= T� � �� � CC� � �
� � +� �  6C �� � � � ��A T� � �� � CC� � �
� � +� �  6CC �� � � � � �� � � �� B�� � �� U 
 OCONTAINER I OCONTROLPARENT LCKEY THIS KEY	 KEYOBJECT	 BASECLASS
 NCTRLCOUNT	 FORMCOUNT	 PAGECOUNT BUTTONCOUNT CONTROLCOUNT
 GETKEYEXPR FORMS PAGES BUTTONS CONTROLSOURCE VISIBLE GETNAME CONTROLS�  ��  � �� � T� ��  ��' +�C� oContObject.Parentb� O��k � T� ��  � � .� �� T�  ��  � �� �3 T� �CC� �R� .� C� �C� >�\� � 6��	 B�� �� U  OCONTOBJECT LCNAME NAME PARENT� ��  � �� � � %�C�� ��1 � T�� �� �� �J � T�� ���  �� � H�[ ��" �C� FormSet�� � �� ��� � T� �� � ��$ �C�	 Pageframe�� � �� ��� � T� �� � ��3 �C�� � � Optiongroup,Commandgroup�� ��� T� �� �� 2�� T� �� � �� � �� ���(��� ���� H�C���" �C� FormSet�� � �� ���� ��CC �� �� �
 � �	 ��$ �C�	 Pageframe�� � �� ���� ��CC �� �� � � �	 ��Q �C�	 ContainerC �� �� � � �� �  C� PageC �� �� � � �� ��.� ��CC �� �� � � �	 ��4 �CC �� �� � � � EditBox,TextBox�� ����, T�� � ��� �� �C �� �� � � �� � �� U 
 OCONTAINER I OCONTROLPARENT THIS	 BASECLASS
 NCTRLCOUNT	 FORMCOUNT	 PAGECOUNT CONTROLCOUNT
 BEFORESAVE FORMS PAGES CONTROLS VALUEa $ %�C� oAriaApplicationb� O��Z � ��C � �  � �� T�  � �a�� ��C�  � � A�  � �� � U  OARIAAPPLICATION PROGRAMCLEANUP THISFORMSET REBLDCMP SETMENU ACTIVEMODULEID� T�  ��  �� T� � �C� � ��6 T� � �CC� THIS.nWorkAreab� C� �  � � � 6�� %�C� � �
��� � F�� � �� T�  �C� � ��( %�C� (�  �� � C� +�  �� ��� � T�  �� (�  � )�� � � T� � ��  ��$ %�C� oARiaApplicationb� O��[� G � T� � �� � �� T� � �� � �� T� �	 �� �	 �� T� �
 �� �
 �� T� � �� � �� T� � �� � �� %�C�
 gcBaseWindb� C���� T� � �� �� � %�C� � �
���� ��C � � � � �� � %�C� SYCTRIGG�
��� F�  � Q�� � � SYCTRIGG��
 G((� 1� �*� F� � � T� �C� EXACTv�� G� T� �C� � �C� � >\�� SET EXACT &lcSavExact
 � � ������� T� � ��������  �� %�C� �
� CC� �
��	��W� T� � ��  �� T� ����% ~$+�� � C� �ZC� �
���S� � � �� ����� T� � �� ������ �� T� �� ��� � � �+ T� � �C� THIS.DATAENVIRONMENTb� O�� T� � �C�s�� %�C� � �
���� ��C� � � � �� � U   LCKEYEXP THIS	 NWORKAREA GETMASTERFILE
 GETKEYEXPR KEY ALLOWADD OARIAAPPLICATION	 ALLOWEDIT ALLOWDELETE BAR_NO POP_NAME MULTIRUN THISFORMSET NAME
 GCBASEWIND FORMHASTOOLBAR OTOOLBAR INIT SYSPATH SYCTRIGG
 LCSAVEXACT
 LCCURPRGNM
 LAEVNTTRIG LNCOUNT	 CAPOBJNAM	 CEVENT_ID
 NTRIGORDER
 USEDATAENV SESSIONISREADONLY
 CHANGEMODE
 ACTIVEMODE
 ��  � � �� T� ��[ ,1196,1309,1294,1429,38,227,1651,1421,1795,1770,176,177,1951,2030,1944,1798,1693,1791,1566,�V 1720,1948,174,175,1979,1978,1728,1642,1695,1698,1966,1967,4,1957,1958,1296,3,1986,1910� ,356,355,181,182,��� T� ��M ,1595,1585,111,1157,1539,1422,1581,1582,39,1599,108,2003,1426,1440,1427,1428,�N 1420,1436,1912,34,1681,1716,1715,109,1583,1405,1411,95,228,1577,1545,1954,1945� ,1131,2022,1982,1201,1116,1717,��/ T� ��" ,1308,1000,2000,1792,56,43,6,1955,�� H���	
�8 �� ,CC�  Z�� ,� � � ,CC�  Z�� ,� ���� H�6��� ��  �&����� ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� #)� H� X� ��  ����n�� ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� u� X� ��  ������ ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� u�� X� ��  ������� ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� u� X� ��  ����<�� ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� u�� X� ��  ������ ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� #6� X� ��  ����Y�& %�CCE�%� Program Error�x���� X� �U� %�CC� DEVELOP5�
��7� L� �Q�
 �� � � B(� � � � ��  �}����& %�CCE�%� Program Error�x����� X� ��� %�CC� DEVELOP5�
���� L� ���
 �� � � B(� � � � ��  �l��]�& %�CCE�%� Program Error�x���� X� �Y� %�CC� DEVELOP5�
��;� L� �U�
 �� � � B(� � � � ��  �m����& %�CCE�%� Program Error�x����� X� ��� %�CC� DEVELOP5�
���� L� ���
 �� � � B(� � � � 2���� ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will continue.�� Program Error�x�� %�CC� DEVELOP5�
���� L� ��� B� � � �� ,CC�  Z�� ,� ��l	�� ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z�C� C�
 � Program will terminate.�� Program Error�x�� %�CC� DEVELOP5�
��U	� L� �h	� <� B(�� � 2�	
�Z ��CCEC� C�
 � In Program:� C� C�
 � Line:CC� Z��� Program Error�x�� %�CC� DEVELOP5�
���	� L� �
�
 �� � � B(� � � � U	  NERROR CMETHOD NLINE LCIGERR LCIGERR1	 LCTERMERR THIS RELEASE DOM	 ��C��� T�  �CW�� %�C� SYCTRIGG���<� F� � T� �C� � �C� � >\�� � � ������� T� � ��  �� %�C� �
� CC� �
��	��8� T� � ��  �� T� ���� ��CC� �
�� SYCTRIGG���% ~$+�� �	 C�
 �ZC� �
���4� � � �� ����� T� � �� ������	 �� T� �� ��� � � �
 F��  �� U  LCALIAS SYCTRIGG
 LCCURPRGNM THISFORMSET NAME OARIAAPPLICATION
 LAEVNTTRIG LNCOUNT	 CAPOBJNAM	 CEVENT_ID
 NTRIGORDER getmasterfile,     ��
 refreshall�    ��	 dbgetprop[    �� translatetoenglish�	    �� setpath�    ��
 seekrecord�    �� gotop�    ��
 gopreviousJ    �� gonext)    �� goend    �� close�    �� delete�    �� addnew�    ��	 savefiles�    �� edit)"    �� undo�"    �� filewaschanged�&    �� cancontinue!*    ��
 opentablesM+    ��
 changemode�0    �� print}1    ��
 setallprop*3    ��
 getkeyexpr�C    �� getnameXK    ��
 beforesave-L    �� UnloadUO    �� InitP    �� Error:U    �� Activate�_    ��1 q �a� � �q1�A A A A � � A 3 q � � � A AA A � !A1� A 1A A �� !�B�2����C�Q���D A 3 � �� � � �� A bq��q�A �aA A � �� � � A a�� �A � q�Q�A � A � A a� 2 � � 1� a� ���A �A � 2 q a�q1��� A � � A2A A A A A A 3 � �� � A � � � � � � � a� � � � !� � � aaA � � � � q��� A B��� A �A � q� qa� Q� �1�A A � � � � � B A A � 2 � A � � 1aaA 3 � � � A � � 1aaA 3 � � � A � � 1aaA 3 � Q � � 1aaA 3 � 4 �� A � � A 1� A A A 3 � � � A1A A Q � 3 }�� � � � � A�B�� � � � �B A� A A �$A� � �Q A A ��A A �� Q� � � Q A A A A A AA � Q� A A Q A � Q � Q A A A A #� �����!����Q�� RR � Q A � A !A A A � !A A � � !!A B B� � � � 3 � � ?aA 3 q � �A A A A q1�r1�� A A A A A � � A�� B A A D�1a�A C 5 q��Q��� Q�q A Qa1q A A A A A A � � !q�� Q�q A QQ1q A A A A q 3 "� � � � q A A q 2 q a � ��q1� � � � � ��� � � �A A ���A � qA A A � �A �A A A A ��qQ��� � � � � A A �A A A A 2 q � 1aA A 4 q t� ��� ���� � a!� � �A A A ; q � � � A AA A � !B1� A �� �q�A A � !�B�2!�� Qq� aA � aA � aA ����qQ�� �A � aA QqQ�� �A � aA !�� Qq� aA � aA �a�� A A ���aA � �Q��A ��A �!A � q�A A QqQ�a�A A aaA � �arA A A A 3 q � � � � A A� A � !B1� A �� !�B�2�aa�A �����aa�A Q�aa�A A A � 2 q q � q�A 1� 2 q � � � A � !B1� � A �� !�A��A�A A 3 A� qB 2 � Aa1� �qA A Ba 111111�A 1AA �� �� � q A Ba ��b��� Qa�A A C �11A 3 � t��� �� Q A A Q A Q A Q A Q A Q A !aA � �A � � � A A aA � �A � � � A A aA � �A � � � A A aA � �A � � � A A � �A � A A A ��A � Q a A � ��A � � � A A 2 � � rr �a�� �Qa�A A A � 3                               "  �	     ?   �	  �  Q   n   �  �  �   ~        �   �   A  	%  �   �   %%  �%    �    &  '  $  �   4'  &(  1  �   B(  )  >    9)  �)  I  
  �)  �+  R    
,  �-  h  "  �-  =A  w  �  XA  �C    �  �C  �L  6  �  �L  S  �  �  1S  �T  �  �  �T  �\  �  <  �\  ^    F  1^  �d    \  �d  �  U  �  �  Ō  �  $  �  �  8  .  	�  h�  C  L  ��  �  j  R  ,�  ��  s  �  ��  �  �  �  *�  7�  8   )   m                       �����    ��  ��                        �5   %   ,�      Y�  o  {�          �  U  �	 4�  � � �( T� �CC� llFroMnub� U� a� � 6�� %�C�t���S � T� �a�� � %��  � � ��� � %�� ��� �* ��C� INM00022B00000� ALERT �  � � �� � B� � %�� ��� � %��9� ���� �& ��C� INM00021B00000� ALERT� � �� B� � � %�C� syccomp���.� F� � G((� cComp_Id� �[� F�  � Q�� �	 � syccomp��� 1� � %�C�  ����	�< %�C�
 �
� � � 	� � � � SM|SY
	�
 C� � �
	����< ��C� INM00158B00000� ALERTC� � �� |C� � �� � �� B� � %�C� ,� � �� ����= %�C� � C� �� � � � � SM|SY
	�
 C� � �
	����< ��C� INM00151B00000� ALERTC� � �� |C� � �� � �� B� � T� �C� ���z %�CC� � �C� \� � ��\fCC� �C� \� ��\f�: CC� � �C� \� � ��\fCC� �	 �C� \� �	 ��\f	��L�: T� �C� �	 �C� \� �	 ��\C� C� \� ���\�� � T� �a��( %�C� � �
� � � � SM|SY
	���� %�� � ���� T� �C� � � � �� �� %�� 
����& ��C� INM00145B00000� ALERT� � �� B� � � � �6�% T� �C� � �C� ,� � ��\�� ��C� � SYDAPPL���& T� �CC� � �
� C� � �� � 6��7 T� �CC� |� �� � � � C� �C� |� ��\6�� � ������� T� ��  �� ��C � � � �� T� ��  �� %�CC��� �
��2� 5� � �� ���(�C�� ������c %�CC � �� � SYDAPPL�� CC � �� � � � 	�$ � � 
� CC � �� � � � 	����. T� �� CC� �� �  � � ,6C � �� �� � �� %�C� �����< ��C� INM00151B00000� ALERTC� � �� |C� � �� � �� B� �* T� �� CC� ,� �� � � ,� �  6�� T� � �� �� � � F� � T� � ��  �� T� � �C� � ��� T� � �CC� �� � ��  T� � �CC�
 �� � � �
 6��# T� �  �CC�! ��	 � USA� �! 6��% T� �" �CC�# �� � USDLR� �# 6�� T� �$ ��% �� T� �& ��' �� T� �( �C� ��� T� �) �C� ��� ��CC� �! �� �* ��B %�� �+ �
 C� � �
	� � � � SM	� C� ,� � �� ���� ��C� � � A� �, �� � T�- �C�	 GetMemVar�N��, T� �. �CC�
 M_CURRSITE� � �- �/ ���� <�- � G:���0 �-�� %�� ��$� G:��Cl���0 �a�� T� �1 �Cl�� ���$ ��2 ���(�C�
 _COMPANIES�����8 %�CC� � � -� � f�CCC�
 _COMPANIES�2 �f����� G:���2 ���0 �a�� T� �1 ��2 �� !� � �� �L T�9�3 �C� �4 ��  - C� � �CC� � �
� �  (C� � �� )� �  6�� %�C� syustatc���6	� F�5 � G((� CUSER_ID� �k	� F�  �% Q�� �	 � syustatc��� CUSER_ID� �6 %�C� INI� OLDVARS� �7 � �8 � syustatc����	� >�5 � ��� � �� ��C� a��� � � U9 	 LCCOMP_ID LLFROMNU
 LLCHEKFRST THIS ACTIVECOMPANYID
 MESSAGEBOX	 FORMCOUNT SYCCOMP CCOMP_ID SYSPATH	 CCOMPPRNT PARENTMODULE ACTIVEMODULEID	 CCOM_NAME ACTIVEMODULENAME	 MCOMP_MDL	 LCDATADIR	 CCOM_DDIR INSTALLPATH LLSETDON SYDAPPL LSETREQ MMODLSET LCAPPID
 CBARMODULE LAMODSELECTED GFSUBSTR LNCOUNT ACTIVECOMPANYNAME DATADIR
 GETDATADIR PRNTCOMPANYID DEFAULTCOUNTRY
 CCONT_CODE BASECURRENCY	 CCURRCODE CURRENTYEAR	 CCURR_YER CURRENTPERIOD	 CCURR_PRD COMPANYINSTALLEDMODULES COMPANYSETUPMODULES COMPANYSETS LOGINREQUIRED SETMENU
 OGETMEMVAR CURRENTSITE DO
 _COMPANIES
 COMPANYBAR LNCHKBAR CAPTION
 SYSTEMNAME SYUSTATC CUSER_ID USER_ID STATION{ 4�  � T� �-�� %�C� � ��
 �  � SM	��� �' ��C� INM00058B00000� DIALOG� � �� ��C� SY� I� NOSHOW� � �� ��� B� � %�C�  fC� � f��� �- ��C� INM00046B00000� DIALOG� � � � �� ��C �  � A� � �� ��� B� � T� �C�� %�C� SYDAPPL�
��2� F�  � Q�� �	 � SYDAPPL�� �A� F�
 � � G((� CAPP_ID�  %�C� ,�  �� � C�  �	����& T�  �CC�
 � �
� C�
 � �� �  6��7 T�  �CC� |�  �� � �  � C�  �C� |�  ��\6�� � T� ��  �� T� �-�� %�C� ,�  �� ��� � ������� T� ��  �� ��C �  � � �� %�CC��� �
��� 5� � T�  ��  �� �� ���(�C�� �����V %�CC � �� �� CC � �� � � � 	�! � 
� CC � �� � � � 	���. T�  ��  CC�  �� �  � � ,6C � �� �� � �� %�C�  fC� � f��[�- ��C� INM00046B00000� DIALOG� � � � �� ��� B� � %�C�  �����6 ��C� INM00151B00000� ALERTCC��� |� � � � �� ��� B� �* T�  ��  CC� ,�  �� � � ,� �  6��$ T� �CC�
 � C� |�
 � ��\��� � ��	� %�C�  ����	� %�C� � �
� � � 	����* ��C� INM00157B00000� ALERTCC��� � �� %�C� �
� C� �	����
 F�� �� � ��� B� �& %�C�  � � � �
 �  � SM	��'�6 ��C� INM00151B00000� ALERTCC��� |� � � � �� %�C� �
� C� �	���
 F�� �� � ��� B� � %��
 � ��i	�& T� �C�  � � �� �
 �  � SM�� %�� 
����& ��C� INM00145B00000� ALERT� � �� %�C� �
� C� �	����
 F�� �� � ��� B� �E T� �C�2�	 � Dos�+ C�5� � Windows� C�3�	 � MAC� � UNIX666��j T� ��	 SYDAPPL.CC�2�	 � Dos�' C�5�	 � Win� C�3�	 � MAC� � UNIX666� BUILD==SYDAPPL.CMDLBUILD�� IF ! &lcErrCond���VT� �� module �  �
  have not �8 been upgraded to the latest build number on the current � platform ( � �  )!C� C�
 �  If you proceed and use this �9 module, errors may occur and data may be lost. therefore �* you have to install files for thid module �> from the latest builds to be able to procced with this module!�� ��C� ��9� �x�� B� �' T� �CC�
 � �C� -�
 � ��\���+ T� �CC�
 � � SY�
 � � CAPP_ID~��� ��C�  � SYDAPPL��� %�� � ��e	�� T� ��0 The current build number of the System Manager (� � ) is too old to run (�  �  ), you need at least SM build # � �  to run this build of (�  �" ). Please update SM and try again!�� ��C� ��9� �x�� ��� B� � � T� �C�
 �  ��� T� ��
 �! �� ��	�* ��C� INM00146B00000� ALERTCC��� � �� %�C� �
� C� �	���	�
 F�� �� � ��� B� � � %�� � � SY��'
� ��C� SY� S� � �� � %��" ��d
� T�" �-�� T� �a�� ��C� SY� S� � �� � %�C �  � A� � 
���
� %�C� �
� C� �	���
�
 F�� �� � B� � T� � ��  �� T� � �� �� T� � �� �� T� �# ��
 � ��L T�9� �C� �$ ��  - C� � �CC� � �
� �  (C� � �� )� �  6�� ��C �  � �% �� %�C� �
� C� �	��t�
 F�� �� � U&  LCAPPID
 LLINITMENU THIS ACTIVECOMPANYID
 MESSAGEBOX SETMENU ACTIVEMODULEID ACTIVEMODULENAME	 LCCERFILE SYSPATH SYDAPPL CAPP_ID
 CBARMODULE LCAPPLICATIONNAME LLPARENTMODULE LAMODSELECTED GFSUBSTR LNCOUNT COMPANYINSTALLEDMODULES LSETREQ COMPANYSETUPMODULES ACTIVECOMPANYNAME PRNTCOMPANYID PARENTMODULE LLSETDON LCACTIVEPLAT	 LCERRCOND	 LCMESSAGE CAPTION LCMDLBLDNUM	 CMDLBUILD LCSYSBLDNUM	 CAPP_NAME	 LPARNTMDL
 GLINITMENU ACTIVEMODULEBUILDNUMBER
 SYSTEMNAME SETHLPFL�  T�  �Ca� �� T� �C� sylogin� � �� %�C� lcReturnb� C��j � T� � �� �� ��C� � �� B�a�� �y � B�-�� � U 	 GNUSERLOG
 GFUSERLIST LCRETURN THIS DOFORMRETVAL USER_ID GETUSERINFO; ��  � � �+ T�  �CC� lcModulb� C� �  � � SY6��, T� �CC�	 lcSub_Ctgb� C� � � � I6��. T� �CC� lcNoShowb� C� � � � SHOW6�� T� �C�5�
 � SHOW� � 6�� T� ��3�� T� �a�� 5� � T� �-�� %�C� SYDOBJCT�
��� F�  � Q�� � � SYDOBJCT�� �$� F� � � G((� CAPP_ID� %�� �
 ���� %�C� SYUUSRPR�
���� F�  � Q�� � � SYUUSRPR�� ��� F� � � G((� CUSER_ID� � %��  � SY��!� %�C� SYDAPPL�
���� F�  � Q�� � � SYDAPPL�� T� �a�� �� F� � � G((� CAPP_ID� ��� %�C� SYDAPPL�
��k� F�  � Q�� � � SYDAPPL�� T� �a�� �z� F� � � G((� CAPP_ID� T� �-��( %�C�  � SYDAPPL�� � � � F	���� T� �a�� � %�� ���� Q� � � %�� ���� B� � � %�C� SYCMENU�
��9� F�  � Q�� � � SYCMENU�� �H� F� � � G((�	 APPPOPBAR� T� �C� EXACTv�� G� %�C� ,�  �� ��5� � ������� T� ��  �� ��C �  � � �� %�CC��� �
��1� 5� � %�� � ���� ��C� � �� � T�  ��  ��$ T� �� (SycMenu.cApp_ID = "SY"�� �� ���(�C�� ������  ��CC � ��  �  � � � ��9 T�  ��  CC�  �
� � ,� �  6� "C � �� � "��6 T� �� �  .OR. "C � �� � " $ cBarModule�� �� T� �� � )��>SELECT RTRIM(cMstr_Nam) + "-" + cBar_pos , RTRIM(cPross_ID) , cProcType , RTRIM(cProcPath) , RTRIM(SYDOBJCT.cbasewind) , cSub_Msg , cHlpTopic , CMENUPARAM , CBARMODULE FROM SYCMENU , SYDOBJCT WHERE SYDOBJCT.CAPOBJNAM = SYCMENU.cPross_ID .AND. &lcModuleCondition .AND. SYCMENU.cProcType IN ('C' , 'P') UNION SELECT RTRIM(cMstr_Nam) + "-" + cBar_pos , RTRIM(cPross_ID) , cProcType , RTRIM(cProcPath) , SPACE(10) , cSub_Msg,cHlpTopic , CMENUPARAM , CBARMODULE FROM SYCMENU WHERE &lcModuleCondition .AND. SYCMENU.cProcType IN ('R' , 'G' , 'E' , 'M') INTO ARRAY THIS.aProcess
 � �l� %�C�  ���� SET EXACT &lcExact
. %��  � SY�	 � � I	� � � SHOW	���� GY(� GY�� � %�� �
 �
 �  � SY	��7� F� � G((�
 MDLCMPPRSS�: G(�� � � �	 � � U	� � � � �	 � � G	�� F� � G-(��  � � � �  ��� � � G-(��	 � ��� � T�! �� �� ~$��" � �+��	 �  ����' T�# �CC�$ �� �  � � KEY C�$ �6�� H����� �C�% �� P���	�: %�C�& PRMPAD('_MSYSMENU',ALLTRIM(cMstr_Nam))b� U���	�( T�& �� PCCCC�' �\g�8�� 0���n DEFINE PAD    (ALLTRIM(cMstr_Nam))  OF     _MSYSMENU  PROMPT ALLTRIM(cSub_prpt) AFTER &lcPrivpad &lcHotKey
 s��C� ������ 1��C�' �������C� ��� � 2��� T�( �C�' �� -�) �� %��  � SY��z� %��  � S���
�+ T�* �CC�) g�� � C�) g�� �6��z DEFINE BAR EVAL(cPross_ID) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt) AFTER lnAftrBar COLOR SCHEME 3  &lcHotKey
 �v� %�CC� �f� GFDOHELP���
� T�+ �C�' ��� T�, �C�- ��� T�. �C�) g�� �2 T�/ �CC� �0 �� �  � �	 SKIP FOR C� �0 �6��, %�C� �� �' � P07PU07	� �! � 	��a� .� �6 %��  � M� CC� �� SYDAPPL�	� � � � F	���� .� � %��  � M���� T�! ��! ��� �r DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt) &lcSkipExp &lcHotKey COLOR SCHEME 3
 %�C� �1 �
��r�# G:��C�) g���C�' ���CC� �1 ���� � � �D� %�� �
 ����# %��% � S� C� SYUUSRPR+	��:�� DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar COLOR SCHEME 3 SKIP FOR .T. &lcHotKey
 ���y DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar COLOR SCHEME 3  &lcHotKey
 � �@�y DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar COLOR SCHEME 3  &lcHotKey
 � � %�C�% �� S���� s��C� ������ 1�C�) g���C�' �����C� ��� � � �) 1��� DO gfMenuBar WITH POPUP(),BAR()� %��  � SY�	 � � S	�� � ��� T� �2 �C� P08PU08��� �� J�-�(�3 �4 � %��3 
����' %��  � SY�	 � � I	� �4 ��Y� %�C� SYCCOMP�
���� F�  � Q�� � � SYCCOMP�� ��� F�5 � � G((� CCOMP_ID� %��4 
����* T� � �CC� �7 �
�	 � �7 � � � 6�� ��� G:���8 (�-��
 <��8 � � T�9 �� �� ~�*� T�9 ��9 ��� %�� �
 ���� F� �& -�C� � � � � �� �6 �5 �6 	�� F�5 � %�C� SYUUSRPR4���� s��9 ���8 "��6 � -C�: ��� ���$ s��9 ���8 "��6 � -C�: ����a�� � ��� s��9 ���8 "��6 � -C�: ��� � %�� � �6 ���� G:���9 ���8 (�a�� T� �; ��9 �� T� �< �C�5 �: ��� T� �= �CC�? �� �> ��  T� �@ �CC�A �� �6 � �A 6��+ T� �B �CC� �B �� � USDLR� � �B 6�� ��CC�5 �D �� �C �� � T�E �� "�6 � "��K ON SELECTION BAR lnCompBar OF _COMPANIES  DO gfChngComp WITH &lcComp_Id
 � %�� � SHOW��U� \�� {ALT+M}�;� � ��� G:���8 (�-�� %�� �; � ���� G:��� �; ���8 (�a�� � � �ao��  � SYCMENU�C�' V� -�) ��C� V���  ��C�F V��C�
X���H ���I ���J ���K ���C� �	 � �M � �N � �O � �) �  � SY�� C� �  � R� G� E� M�	�� SYCMENU� SYDOBJCT�C�' V� -�) ��C� V���  ��C�F V��C� �G V���H ���I ���J ���K ���� �L � � �- C� �	 � �M � �N � �O � �) �  � SY�	� C� �  � C� P�	��� �P � � F� � G-(� �h� SET EXACT &lcExact
' ��C� INM00150B00000� DIALOG� �Q �� T� �-�� � �* +�CC� P07PU07C� P07PU07�������� <�C� P07PU07�����R � �4 +�CC� P07PU07C� P07PU07C� P07PU07�������% <�C� P07PU07C� P07PU07�����R � � %�� ��+� Q� � �	 B�� �� US  LCMODUL	 LCSUB_CTG LCNOSHOW LNEXSBAR	 LLRETFLAG
 LLAPPLUSED THIS SYSPATH SYDOBJCT CAPP_ID LOGINREQUIRED SYUUSRPR CUSER_ID SYDAPPL LLEXIT
 CMODULEVER SYCMENU	 APPPOPBAR LCEXACT LAMODSELECTED GFSUBSTR LNCOUNT REBLDCMP REBUILDCOMP LCMODULECONDITION SETMULTIAPPMENU
 MDLCMPPRSS USER_ID
 CGRPORUSER
 USER_GROUP ACTIVECOMPANYID	 CPROSS_ID	 CPROCTYPE LNBARSDEFIEND CSUB_CTG LCHOTKEY	 CSUB_HKEY CSUB_TYP	 LCPRIVPAD	 CMSTR_NAM LCPOPBAR CBAR_POS	 LNAFTRBAR GCHLPPOP GCHLPPRM	 CSUB_PRPT GNHLPBAR	 LCSKIPEXP	 MSKIPEXPR	 MMARKEXPR WINSEPTA	 GLRESTORE
 GLCMPCREAT SYCCOMP CCOMP_ID USERCOMPANYID
 _COMPANIES	 LNCOMPBAR	 CCOM_NAME
 COMPANYBAR ACTIVECOMPANYNAME DATADIR
 GETDATADIR	 CCOM_DDIR PRNTCOMPANYID	 CCOMPPRNT BASECURRENCY COMPANYSETS
 CCONT_CODE	 LCCOMP_ID	 CPROCPATH	 CBASEWIND CSUB_MSG	 CHLPTOPIC
 CMENUPARAM
 CBARMODULE	 CAPOBJNAM CPAD_POS CPOP_POS	 CPOP_LEVL APROCESS
 MESSAGEBOX P07PU07t �  � ��5����� ��C�� ANSI�  � �� ��C�� AUTOSAVE�  � �� ��C�� BELL�  � ��# ��C�� BLOCKSIZE TO� V�  � �� ��C�� BRSTATUS�  � �� ��C�� CARRY �  � �� ��C�� CONFIRM�  � �� ��C�� CLOCK�  � �� ��C�	�
 STATUS BAR�  � �� ��C�
� CONSOLE�  � �� ��C�� CURSOR�  � ��" ��C�� DECIMALS TO� V�  � �� ��C�� DELETED�  � �� ��C�� DEBUG�  � �� ��C��	 DEVICE TO�  � �� ��C�� ESCAPE�  � �� ��C�� EXACT�  � �� ��C��	 EXCLUSIVE�  � �� ��C�� FIELDS�  � �� ��C�� FIXED�  � �� ��C�� FULLPATH�  � �� ��C�� HEADINGS�  � �� ��C�� HELP�  � �� ��C��
 LIBRARY TO�  � �� ��C�� LOCK�  � ��  ��C��	 MARGIN TO� V�  � �� ��C�� MARK TO� V�  � ��# ��C�� MEMOWIDTH TO� V�  � �� ��C�� MENU�  � ��! ��C��
 MESSAGE TO� V�  � ��$ ��C��
 MESSAGE TO�� V�  � �� ��C� �
 MULTILOCKS�  � �� ��C�!� NEAR�  � ��" ��C�"� ODOMETER TO� V�  � �� ��C�#� OPTIMIZE�  � �� ��C�$� PATH TO�  � �� ��C�%� POINT TO� V�  � �� ��C�&� PRINTER�  � ��  ��C�'�
 PRINTER TO��  � ��# ��C�(� PROCEDURE TO� V�  � ��# ��C�)� REPROCESS TO� V�  � �� ��C�*� SAFETY�  � �� ��C�+�
 SCOREBOARD�  � �� ��C�,� SPACE�  � �� ��C�-� STEP�  � �� ��C�.� SYSMENU�  � �� ��C�/� TALK�  � �� ��C�0�	 TEXTMERGE�  � �� ��C�1� UNIQUE�  � ��! ��C�2� RESO TO�� V�  � �� ��C�3� RESO�  � �� ��C�4� CLASSLIB TO�  � �� ��C�5� FULLPATH�  � �� T�  � �C� SHUT��� 1��	 DO GPEXIT� Gd� GB� G� GC(��!�� G[� G� G	 � G>�� G0 � G
 � G] � G(���� G � G � G(&� G� G� G� G� G� GQ� G � G � Gb(� GI� G#(�� �� G:(�� /�� G$(��A�� G% � G&(�C���X�  G&(�� ARIA Advantage Series�� G_ � GF� G'(��d�� Ga �	 G)(�  � G;(�� .�� G*� G*(� Lpt1� GM(��
�� G.� G/� G@� G1� GY�� G2� G`� G6� GQ � U  THIS ASETS GETSET SHUTDOWN LPT1= T�  �-�� 1� llError = .T.� �� ���(�C� � �����@ %�C� THIS.aSets[lnCount,3]b� C� C � �� � � V	��� �: T� �� SET C � �� � �  THIS.aSets[lnCount,2]�� �� �4 T� �� SET C � �� � �  C � �� � �� � &lcCommand
 �� {�  � T� �� � �� ON SHUTDOWN &lcShut
 U  LLERROR LNCOUNT THIS ASETS	 LCCOMMAND LCSHUT SHUTDOWNm  �� ��C�  � �� GY(� T�9� ��  � �� %�C�  � �
�	 C�  � 0	��Y � G(��  � �� � ��C�  � �� U  THIS RESTOREENVIRONMENT CAPTION OLDWINDCAPTION HELPFILE RESTORETOOLBARSr
 4�  � � � � � 5�  � � � � � � � � ���� � ���� �� {�  � T� �C��* T�  �CC� lcDlgIDb� UL� �  � �  6��, T� �CC� lcDlgTypb� UL� � D� � 6��, T� �CC�	 lcVarsStrb� UL� �  � � 6��- T� �CC�
 lcDlgValidb� UL� �  � � 6��- T� �CC�
 lcDlgMessgb� UL� �  � � 6�� T�	 �C�  ��\��% T�
 �CC� 57CC� �=� DA�\g�� T� ���� T� ���� T� �� �� T� �CC� �� �� �6�� T� �CC� �� �
� �6�� %�C� ���� %�C� SydDlObj�
��(� F�  � Q�� � � SydDlObj�� �! G(�C� SydDlObj�(� CDLOBJID� T� �C� SydDlObjO�� %�CC�  �	\� SydDlObj����� T� �C� � ��� ��� T� �� \!\?\<Ok�� �" %�CC�  ��\� SydDlObj���� T� �C� � ���# T�	 �CC� � �� �	 � � � 6�� �"� T� ��  �� � ��C � � � |� �� �� ���(�C�� ����z�! T� �C� � �C � � ����� ��1 T� �CCCC� � \�  �� !�  �� <�  �� ?�  ��� ��C � � � ;� �� T� �C�� ���� �� ���(�� ��� T� �C� CC � � >D�� �� �R� T� �C� ��� T� �� \!\?\<Ok�� T� ���� T� ���� � T� �C�
� �D�� T� �C�2� � �D�� T� �C� � � � 8��. T� �� �� � � � ���� T� �CCC� �>� 8���\ s,�  �    �    ��� ��� �@�� System���
�A�� B�B�������������'�� �! ����	 t,��  �	 z,�  ��S �    �    B������������������������(�    �� �A�� 1�C�������L �    �    B������������������������(�� ��    C�������_ � 0  ��m  B������������������������(� 0  �� �333333�?�A�� 1�C�������X � 0  ��m  B������������������������(�� �333333�?���m  C�������L �    �� �B������������������������(�� ��� �C�������p � 0  �� �333333�?�B������������������������(�� �333333�?��� �333333�?�C�������S �� ��$  B������������������������(�� ��� �A�� 1�C�������w �� �333333�?��ff  B������������������������(�� �333333�?��� ��������?�A�� 1�C������� %�C� �����L T�" �CC� TR  QRY INFOC�	 � TRQRIN������\�� .BMP�� � � � � ���" ��A�� T�� �) � � �� ���� ���� ���� � �� %�C� ���+	�@ �� ���� ��� ���      �?��� ��� ���� @*HT � �� ��	�y @ lnWinHight -2 ,lnButXPos  GET lnDlgOpTion  PICTURE "@*H "+lcButton SIZE 1.5,lnButtSize,lnBetwButt VALID &lcDlgValid
 � t,�  � %�C� �
���	� G(�������� ��C� �� G(� � 9���& %�� � � � C� SydDlObjN	��7
� #�C� SydDlObj��� �� � %�C� �
��U
�
 F�� �� � <,�  � ��	 B�� �� U#  LCDLGID LCDLGTYP	 LCVARSSTR
 LCDLGVALID
 LCDLGMESSG	 LCCURRDBF	 LNBUTTONS LABUTTNO	 LAVARSSTR LCMSGCATGRY
 LNCOLRSCHM LNDLGOPTION
 LNBUTTSIZE LNOLDREC
 LNTEXTSTRT
 LNRIGTSHFT THIS SYSPATH CDLOBJID LCBUTTON SYDDLOBJ MDLOBJ LCMESSAG
 CMSGCATGRY GFSUBSTR	 LNVARSSTR	 LCBUTTCHK LNBUTTNO
 LNWINWIDTH
 LNBETWBUTT	 LNBUTXPOS
 LNWINHIGHT	 GWDDIALOG
 SYSTEMNAME LCICONX  ��  ���(�C� � ����Q � %�C �  �� � ��M � �,�C �  �� � �� � �� U  I THIS	 ATOOLBARS G� %�C� syuuser�
��= � F�  � Q��  � � syuuser�� �L � F� � � -� T�  � �C4�� %�C� SYUUSER���� � Q� � � %�C� SYCINST�
��� � F�  � Q��  � � sycinst�� �� � F� � � %�C� � �
��� T�  � �� � �� ��C�  � �  � �� � T�  � �C� �	 ��� T�  �
 �C� � ��� T�  � �C� � ���" T�  � �CC� � �fC�  � f
��, T�  � �CCC� � ��=fCC�  � �=f
��, T�  � �CC� � �� � USA   � � � 6��! %�C�  � � SYHHELP.HLP0��� G(��  � � SYHHELP.HLP�� GU(� THIS.HelpTopic� � T�  � �� � �� T�  � �C� � ��� T�  � �C� � ��� T�  � �C� � ��� T�  � �C� � ��� T�  � �C� � ��� T�  � �C� � ��� %��  � ����' T�  � �C�  � �C� \�  � ��\��/ T�  � ��  � C�  � C� \�  � ���\��/ T�  � ��  � C�  � C� \�  � ���\��/ T�  � ��  � C�  � C� \�  � ���\��/ T�  � ��  � C�  � C� \�  � ���\�� � %�C�  � C� �  �0���% %�C� _SCREEN.GROUNDBMPb� O���  ��C�	 GroundBmp� image�9�! �� �  T�9�" �# �C�  � C� �  �@��" T�9�" �$ ��9�% �9�" �% ���" T�9�" �& ��9�' �9�" �' ��� T�9�" �( �a�� � GT� T�) �C�* ��o h1��  � �) � .DBF���+ � C����, � C����- � M�. � L�/ � N����� ��0 � M�1 � D� %�C�) ���!� Q�C�) �� � GT(��  � �) ��5 T�  � �CCC� APPHOME5��	 �  � � C� APPHOME56��5 T�  � �CCC� REPHOME5��	 �  � � C� REPHOME56�� F�  � Q��  � � syclogo�� T�  �2 ��3 �� %�C�  � �
���� ��C�  � �  �4 �� � Q�5 � U6  THIS SYSPATH SYUUSER LOGINREQUIRED SYCINST	 CINSDFCOM ACTIVECOMPANYID GETCOMPANYINFORMATION RESOURCEHOME	 CINSRSRDR INSTALLPATH	 CINSYSFDR
 SCREENHOME CSCRDIR MAPDRIVE DEFAULTCOUNTRY
 CCONT_CODE CURRENTSITE
 CCURSITEID WORKDIR	 CINSWINWD APPLICATIONHOME	 CINSWINPD
 REPORTHOME	 CINSWINRD
 BITMAPHOME	 CINSWINBM CLASSDIR	 CCLASSDIR EDIPATH CEDIPATH DEFAULTPATH CDEF_BMP	 ADDOBJECT	 GROUNDBMP PICTURE TOP HEIGHT LEFT WIDTH VISIBLE
 LCRESOURCE
 GFTEMPNAME TYPE ID NAME READONLY CKVAL DATA UPDATED
 SYSTEMNAME CSYSNAME CHANGECOMPANY SYCLOGO� 4�  � T� �C�� T� �a�� %�C� SYCINT�
��^ � T� �-�� F�  � Q�� � � SYCINT�� �m � F� � � G((�	 CCONTCODE� %�C�  ���i� T� �C� ��� T�	 �C�
 ��� T� �C� ��� T� �C� ��� T� �C� ���& T� � �CC� �
� C� �� � � 6�� SET CENTURY  &lccentury
 G8(��	 �� SET CURRENCY &lccurrency
 SET DATE &lcdate_type
 G<(�� �� � %�C� �
����
 F�� �� � %�� 
���� Q� � � U 	 LCCONT_ID
 LCSAVALIAS	 LLUSD_INT THIS SYSPATH SYCINT	 CCONTCODE
 LCCURRENCY	 CCURRENCY
 LCCURRENCI
 CCURRENCYI LCDATE_TYPE
 CDATE_TYPE	 LCCENTURY CCENTURY LCSEPARATOR
 CSEPARATOR	 PHONEMASK
 CPHONETEMP ��  � � � � %�C� lnSetArgb� C��< � T� �� �� �* T� �CC� lnSetArgb� N� � � � 6�� T� � ��  ������ �� T� � ��  ������ �� %�� � ��� � T� � ��  �����C� v�� �� �! T� � ��  �����C� � v�� � U  LNARRNUMBER	 LCSETPROC LNSETARG LCSECPAR THIS ASETS�  4�  � T� �� SYHC�  �� .HLP�� %�C� � � 0��N � G(�� � � �� �� �! %�C� � � SYHHELP.HLP0��� � G(�� � � SYHHELP�� � � U  LCAPP_ID
 LCHELPFILE THIS SYSPATH� ��  � T� �C� �\�� T� �� �� G(� (� CAPP_ID�% %�C�  CC� �\�� SYDOBJCT����� � ���� T� �� � �� T�	 �
 �� � �� T� ��  �� ��C� � � � |� �� %�� ��� %�CCC� �\��	 � 
��� T� �a�� T� �-�� T� �a�� B�-�� � T�	 �
 �-�� � %�CC�� �
���� �� ���(�C�� ������" T� �C� � C � � � � ~�� T� �C� ��� A[��2 %�C� WIN� � SYUSTATC�� � � �	 � 	���� T� �� � �� T� �� � ��" T� �C� � C � � � � ~�� %�� �	 � � � �  	��n�+ ��C� TRM00093B00000� ALERTC� ��	 �! �� T� �a�� T� �-�� T� �a�� B�-�� ���0 %�C� INI� OLDVARS� � � SYUSTATC����� T�" �C�	 REPROCESSv�� GM(���� %�C� SYUSTATCS��� Z�C� SYUSTATC�� #� ��# �� ��C� SYUSTATCS�� GM(��" �� ���O ��C� TRM00094B00000� ALERT� � |CC�$ �% � �$ � � CUSER_ID~��	 �! �� GM(��" �� T� �a�� T� �-�� T� �a�� B�-�� � � � � �� � %��	 �
 
���! %��	 �& � �
 C�	 �' �
	��� GN���	 �& ����	 �' ��a�� � � T�( �CC� �\f�� %��	 �) � �	 �* � O	��t� T�+ �C� SYDAPPL�
�� %�C� SYDAPPL�
���� Q�  ��	 �, � SYDAPPL�� � ��C�  � SYDAPPL���% T�- �CC�. �/ �� �  � C�. �/ �6�� %��+ ���� Q�. � � %�C� SYUUSRPR�
��#� Q�  ��	 �, � SYUUSRPR�� � F�0 � G((� cUser_ID�# %�CC�	 � ��- �	 � �( ����� T�	 �1 ��0 �2 �� T�	 �3 ��0 �4 �� T�	 �5 ��0 �6 �� %�C�0 �7 �
���� ��C�0 �7 �8 � |� �� � �p�# %�CC�	 �9 ��- �	 � �( ���]� T�	 �1 ��0 �2 �� T�	 �3 ��0 �4 �� T�	 �5 ��0 �6 �� %�C�0 �7 �
��Y� ��C�0 �7 �8 � |� �� � �l� B�-�� � � ��� J�a�(�	 �1 �	 �3 �	 �5 � � � ��C�	 �' �	 �& �	 �: �� U;  LCPROGRAMMODULE	 GCININAME
 GCBASEWIND
 LCBASEWIND CAPP_ID SYDOBJCT
 LAPRGNAMES	 LLSUSRPRG LSINGUSR THIS MULTIRUN	 LMULTINST GFSUBSTR	 MPRGNAMES SINGLEUSERPROGRAM GLNOLOG
 GLFIRSTIME
 GLQUITTING LNCOUNTP	 LCPRGWIND	 CBASEWIND	 CAPOBJNAM SYUSTATC CCOMP_ID ACTIVECOMPANYID LCUSER CUSER_ID LCSTION CSTATION
 LCPRGLNAME	 CPRGLNAME USER_ID	 GCSTATION
 MESSAGEBOX LCOLDREP	 GNMYSTREC SYUUSER	 CUSR_NAME BAR_NO POP_NAME
 LCPROSS_ID LOGINREQUIRED
 USER_LEVEL
 LLAPPLUSED SYSPATH
 LCMASTMENU SYDAPPL
 CMODPARENT SYUUSRPR ALLOWADD LADDREC	 ALLOWEDIT LEDITREC ALLOWDELETE LDELEREC MSUBPROC	 LASUBPROC
 USER_GROUP SETHELPTOPIC� 4�  � � � � �� � � �3 T� �CCCC�
 gcSrceModlb� C� � � � �	 6f��� %�C� ,� �� ��#� �
 ������� T�
 ��  �� ��C � �
 � �� T� ��  �� %�CC���
 �
��� �� ���(�C��
 �����  %�CC���
 � � � ��� T� �C���
 �� !� � �� � � T� � �� �� � ������� �� ���(��9� ���/ T� �� _Screen.Forms(CC� Z�� ).Parent�� %�C� b� O��
� %�CC��� �
���� � �C�� �������� �, T� �C�� �������C � �9� � � �� � �� T� �C�  ��� T� �� �� �� ��A�(��Z���� T� �� ��� T� �C�  � �� %�C�� � �� ���� .� ��� !� � �� T� �a�� ��C �   �  � � � �� U 
 GCPROG_NAM GCPARAMETERS LCPROGORWIN
 GCSRCEMODL LAWINDARRAY LNCOUNT
 LCWINPARNT	 GCWINAPPL THIS ACTIVEMODULEID LAPROGMODULES GFSUBSTR COMPANYINSTALLEDMODULES PROGRAMMODULEID	 FORMCOUNT FORMS PARENT NAME
 GCWINDBASE
 GNPROGCOPY LNASCII
 GCBASEWIND
 GLFIRSTIME RUNPROG� 4�  � � � 5� � � %�C � � � ����# %�C� � C�  �\� .SCX0��k � T� �� � C�  �\�� �� �C T� �� � CC�	 gcWinApplb� C� � � � �	 6� \C�  �\�� � %�C� � .SCX0��H�* %�C� gcParametersb� C� C� �
	��%�* DO FORM &lcFormName WITH &gcParameters
 �D� DO FORM &lcFormName
 � ���< ��C�( This program is still under development.��9�
 �x�� � � U 
 GCPRGTORUN GCPARAMETERS	 GCWINAPPL	 GCWINTYPE GCMODPRG THIS PROGRAMSETUP
 SCREENHOME
 LCFORMNAME ACTIVEMODULEID CAPTION� 4�  � � T� �-�� T� ��  �� %�� P05�  ��{ � �� � T� �-�� 1� llError = .T.� ��C �   � � � �� {�  � � T� � �� �� T� � ��  �� G&(�C� �	 ��� T�
 �C� EXACTv�� G�* T� �C� � C�  �� -C� �� 0���� SET EXACT &lcSavExact
 %�� � ��b� T� �C� � � ���� �� � H�J�^� �C � �� � � CP��5� %�CCC � �	� � ������ T� � �CC � �� � ���- ��CCC � �� � �CC � �� � �� � �� �1� T� � �CC � �� � ���> ��CCC � �� � �CC � �� � �-CC � �	� � �� � �� � �C � �� � � G��� 1� llError = .T.� T� �-�� T� � �CC � �� � ���  T� �CC � �� � �� ()�� =&lcCommand
 {�  � %�� ���< ��C�( This program is still under development.��9� �x�� � �C � �� � � R���� T� � �CC � �� � ��� %�CCC � �	� � ������- ��CCC � �� � �CC � �� � �� � �� ���= ��CCC � �� � �CC � �� � �CC � �	� � �� � �� � �C � �� � � M��^� %�CCC � �	� � ����5� ��CCC � �� � �� � �� �Z� ��CCC � �	� � �� � �� � � ���' ��C� INM00147B00000� DIALOG� � �� � U 	 LCPOPNAME LNBARNO GLPOPACT	 GCACT_PAD LLERROR THIS SETHELPTOPIC BAR_NO POP_NAME
 SYSTEMNAME
 LCSAVEXACT	 LNPROGNUM APROCESS LCPROSS	 PROCESSID	 DOPROGRAM	 LCCOMMAND CAPTION REPORTPRINT CHANGEMODULE
 MESSAGEBOX�  4�  � � %�� � � C�  �
	��� �* T� �C� � C�  �� -C� �� 0���� %�� � ��� � T� ��� ��� T� � �CC � �� � ��� �� � T� � ��  �� � �� � T� � ��  �� � U 
 LCPOP_NAME LNBAR_NO LNPRGNUM THIS APROCESS	 HELPTOPIC 4�  �# T�  �CC�� � �9� � � �  6��2 %�C� lcBaseWind.MultiRunb� L� �  � 
	��� �! %��  � � �
 C�  � �
	��� � GN���  � ����  � ��-�� � �4 T�  �CC�
 lcBaseWindb� O� �9� � � �  � 6�� %�C� SYDSUPRG�
��� F�  � Q�� � � SYDSUPRG�� �� F� � � G((� PRGCOMP�& %�CC�  �\� �
 � SYDSUPRG���`� Z� � � ��C� � �� T� �-�� U 
 LCBASEWIND
 ACTIVEFORM NAME MULTIRUN BAR_NO POP_NAME THIS SYSPATH SYDSUPRG PRGCOMP ACTIVECOMPANYID WINARNG GLNOLOGY 4�  � 5�  � � � � %�C� P08PU08s
��5 � B� � T� �� �� T� �� � ��� T� �a�� %��9� � ���� �� ���(��9� ����3 %�C � �9� �	 � C � �9� �
 � Toolbar	���� %�� a��� s�� ��� "�� \-�� T� �� ��� T� �-�� �< s�� ��� "�� \<CC� � � �Z��  C � �9� � ��- T� �� _SCREEN.FORMS(CC� Z�� ).SHOW��3 ON SELECTION BAR gnWinBar OF P08PU08 &lcCommand
 T� �� ��� � �� �! �� �� �(�C� P08PU08����� <�� ��� � �� %��9� �� � a��R�- %�C� oAriaApplication.oToolBarb� O��N� ��C� � � �� � � U 	 GCNEWWIND
 GNWINORDER GNWINBAR
 GLFRSTWIND THIS WINSEPTA	 FORMCOUNT LNCOUNT FORMS VISIBLE	 BASECLASS P08PU08 CAPTION	 LCCOMMAND OARIAAPPLICATION OTOOLBAR INIT� 4�  � T�  �C�  f�� G � %�C� SYDSUPRG�
��R � Q�  �� � � SYDSUPRG�� � G(� (� PRGCOMP�$ %�C�  C� � f� SYDSUPRG�
��� �* r�� SYDSUPRG� � ���  ��� � �� � G� T� �C�	 REPROCESSv�� GM(���� %�C� SYDSUPRGS��� ��C� SYDSUPRG� �	 �� GM(�� �� ��� %�C� syuUser�
��[� Q�  �� � � syuUser�� �- T�
 �CC� � � � � � � CUSER_ID~���* ��C� INM00089B00000� ALERT �
 � � �� GM(�� �� B�-�� � U 	 LCPRGNAME THIS SYSPATH PRGCOMP SYDSUPRG ACTIVECOMPANYID CPROGNAM CCOMP_ID LCOLDREP ADDUSERINFORMATION
 LCUSERNAME SYUUSER	 CUSR_NAME	 CADD_USER CUSER_ID
 MESSAGEBOX�  ��  � � 5�  � � � � T� �C� DATASESSIONv�� %�C� oFormb� O��_ � G�(�� � �� � T� �C� W��! %�C�
 lcFileNameb� UL
��� �
 F��  �� �$ >� ��� � ��	 ��C$��
 ��C� ��
 F�� �� G�(�� �� U 
 LCFILENAME OFORM LAFIELDS
 LCSAVALIAS LNOLDDATASESSION DATASESSION	 CADD_USER THIS USER_ID	 DADD_DATE	 CADD_TIME	 GFGETTIME+ 4�  � � �D T� �CCCC�
 gcSrceModlb� C� � � CC��� � � � � 66f��� %�C� ,� �� ��!� � ������� T� ��  �� ��C � � � �� T� ��  �� %�CC��� �
��� �� ���(�C�� �����  %�CC��� � �	 � ��� T� �C��� �� !� � �� � � T� �
 �� �� %�C� � �  � .SCX0��g� T� �� � �  �� ���> T� �� � CC�	 gcWinApplb� C� � � � � 6� \�  �� �* %�C� gcParametersb� C� C� �
	���* DO FORM &lcFormName WITH &gcParameters
 �$� DO FORM &lcFormName
 � U  LCREPORT GCPARAMETERS	 GCWINAPPL
 GCSRCEMODL THIS ACTIVEMODULEID LAPROGMODULES GFSUBSTR LNCOUNT COMPANYINSTALLEDMODULES PROGRAMMODULEID
 SCREENHOME
 LCFORMNAME-  ��  � �� � � ��  �(� �	 B�� �� U  TCFORM LOFORM LURETVALo ��C�  � �� T�  � ��9� �� T�9� ��  � ��% T�  � �C� AriaControlToolBar�N�� %�C�  � 
��� � �� ��C�  � �� B�-�� � %�C� SYUSTATC�
��� �* Q�  ��  � � SYUSTATC��� CUSER_ID� � %�C� SYUUSER�
���) Q�  ��  � � SYUUSER��� CUSER_ID� � F�
 � %��  � �	 C�  � 
	��M� �� ��C�  � �� B�-�� �~� %�C�  � ���z� T�  � ��  � �� � � ��C�  � �� %��  � ���� ��  � � � � ��C� SY� I� NOSHOW�  � �� %�C�  � �
��c� ��C� SY� S�  � �� %�C�  � � A�  � ��_�L T�9� �C�  � ��  - C�  � �CC�  � �
� �  (C�  � �� )� �  6�� � � 9�� U  THIS RELEASETOOLBARS OLDWINDCAPTION CAPTION
 SYSTEMNAME OTOOLBAR VALIDACTIVATIONKEY RESTOREENVIRONMENT SYSPATH CUSER_ID SYUSTATC LOGINREQUIRED LOGIN	 USER_NAME USER_ID LOGUSER USERUSETOOLBAR SHOW SETMENU USERMODULEID ACTIVEMODULENAME ACTIVECOMPANYID %�C�  � �
��� %�C� syuuser���: � F� �
 G((� 1� �g � F�  � Q��  � � syuuser��� 1� � E��  � �� >� ��-�� ��C� �� F� � #��  � ��O >�	 ���    ��
 ���     �� ���  �� ���  �� ���  �� ���  �� ��-�� � Z� ��C� a��� � T�  � �a�� �� U  THIS USER_ID SYUUSER SYSPATH	 LUSR_LOGD	 CUSR_LLOT	 GFGETTIME SYUSTATC USERSTATICRECORD COBJ_TYP	 COBJ_NAME CUSER_ID CCOMP_ID	 CPLATFORM CSTATION LKEEP_IT ENDPROGW �  � �������) T�  � �������� Form Designer��$ T�  � �������� Standard��" T�  � �������� Layout��* T�  � �������� Query Designer��) T�  � �������� View Designer��) T�  � �������� Color Palette��) T�  � �������� Form Controls��- T�  � �������� Database Designer��+ T�  � ��	������ Report Designer��+ T�  � ��
������ Report Controls��) T�  � �������� Print Preview�� �� ���(�C�  � ����P�) T�  � �� �����CC � ��  � ��� %�C � ��  � ��L� �,�C � ��  � �� � �� U  THIS	 ATOOLBARS In %�C� syuuser�
��C � F�  �$ Q��  � � syuuser��� CUSER_ID� �c � F� � G((� CUSER_ID� � ��C�  � ��� T�  � �� � �� T�  � �� � �� T�  �	 �� �
 �� T�  � �� � �� T�  � �� � � O�� T�  � �� � �� T�  � �� � �� T� �C� � ��� T� �� VC� �\�� 1� llError = .T.� T� �-�� T� ��  � �� T�  � �-�� %�C�  � � � .DBF0���� GT(��  � � �� �R�o h1��  � � � .DBF��� � C���� � C���� � M� � L� � N����� �� � M� � D� Q�C� ��� %�C� SYCRESRC���;� Q� � � GT(��  � � �� � T�  � �� �� {�  �; %�C� �  �� (None)� C� _SCREEN.GROUNDBMPb� O	���� ��C�	 GROUNDBMP�9�! �� �( %�C� �  �
� C�  �" C� �  �0	����% %�C� _SCREEN.GROUNDBMPb� O��2�  ��C�	 GroundBmp� image�9�# �� �  T�9�$ �% �C�  �" C� �  �@��" T�9�$ �& ��9�' �9�$ �' ���" T�9�$ �( ��9�) �9�$ �) ��� T�9�$ �* �a�� � %�C�  � �
���� T�  �+ ��  � �� ��C�  � �  �, �� � %�C�  � �
��E� %�C�  � �  �- 
��&� T�  � ��  �� �A� T�  � ��  �. �� � � %�C� SYUUSER���g� Q� � � U/  THIS SYSPATH CUSER_ID SYUUSER USER_ID	 USER_NAME	 CUSR_NAME USERCOMPANYID	 CUSR_DCOM
 USER_GROUP	 CUSR_GRUP USERMODULEID	 CUSR_DMDL LOGINREQUIRED	 CUSR_LEVL
 USER_LEVEL USERUSETOOLBAR	 LUSR_USTB
 LCRESOURCE	 CUSR_RESR LLERROR LLCURRERRORHANDLERENABLED ERRORHANDLERENABLED RESOURCEHOME TYPE ID NAME READONLY CKVAL DATA UPDATED SYCRESRC CDEF_BMP REMOVEOBJECT
 BITMAPHOME	 ADDOBJECT	 GROUNDBMP PICTURE TOP HEIGHT LEFT WIDTH VISIBLE ACTIVECOMPANYID GETCOMPANYINFORMATION GETMODULEINFORMATION ACTIVEMODULEID= 4�  � %�C� syccomp���5 � F� � G((� cComp_Id� �b � F�  � Q�� � � syccomp��� 1� � %�C�  ���6�  T� � �CC� �� � � � 6��# T� � �CC� ��	 � USA� � 6��% T� �	 �CC�
 �� � USDLR� �
 6�� T� � �� �� T� � �� �� T� � �C� ��� T� � �C� ��� ��CC� � �� � �� � U 	 LCCOMP_ID SYCCOMP CCOMP_ID THIS SYSPATH PRNTCOMPANYID	 CCOMPPRNT DEFAULTCOUNTRY
 CCONT_CODE BASECURRENCY	 CCURRCODE CURRENTYEAR	 CCURR_YER CURRENTPERIOD	 CCURR_PRD COMPANYINSTALLEDMODULES	 MCOMP_MDL COMPANYSETUPMODULES MMODLSET COMPANYSETS� 4�  � %�C� SYDAPPL�
��> � F�  � Q�� � � SYDAPPL�� �M � F� � � G((� CAPP_ID�< %�C�  ��
 � � BV	� C�  � � � 	�
 C� � �
	���� %�C� ���� � T� �	 ��  �� T� �
 �C� � ��� T� � �� � �� T� � �� � �� ���' T� �
 �CC� � C� |� � ��\���& T�  �CC� � �
� C� � �� �  6��7 T�  �CC� |�  �� � �  � C�  �C� |�  ��\6�� T� � �-�� T� � �� � �� � ������� T� ��  �� ��C �  � � �� T�  ��  �� %�CC��� �
���� 5� � �� ���(�C�� ������2 %�CC � �� �� CC � �� � � � 	��~�. T�  ��  CC�  �� �  � � ,6C � �� �� � �� � T� �	 ��  �� � ��C �  � � �� B�a�� � B�-�� U  LCAPPID THIS SYSPATH SYDAPPL CAPP_ID
 CMODULEVER COMPANYSETUPMODULES ACTIVECOMPANYID
 CBARMODULE ACTIVEMODULEID ACTIVEMODULENAME	 CAPP_NAME PARENTMODULE	 LPARNTMDL ACTIVEMODULEBUILDNUMBER	 CMDLBUILD LAMODSELECTED GFSUBSTR LNCOUNT COMPANYINSTALLEDMODULES SETHLPFL
  $�  � U    ��  � � � � U  THIS OTOOLBAR CMDADD CLICK  ��  � � � � U  THIS OTOOLBAR CMDEND CLICK  ��  � � � � U  THIS OTOOLBAR CMDEXIT CLICK  ��  � � � � U  THIS OTOOLBAR	 CMDDELETE CLICK  ��  � � � � U  THIS OTOOLBAR CMDEDIT CLICK  ��  � � � � U  THIS OTOOLBAR CMDNEXT CLICK  ��  � � � � U  THIS OTOOLBAR CMDPREV CLICK  ��  � � � � U  THIS OTOOLBAR CMDADD CLICK  ��  � � � � U  THIS OTOOLBAR CMDTOP CLICKx 4�  � � � � � ������� J��  �(� � � � � � J�-�(� � � %�C� sydappl�
��� � F�  � Q��	 � sydappl�� T� �a�� � T�  �C�  ��� T�
 �C�  �\�� T�
 �C�
 C�
 >��  [�� T�
 �C�
 � -��� T�
 �C�
 �C�
 >�\�� T� �-��> o��	 � sydappl��� ��C��� ]���� � SY������� � �� ���(�C�� ������ T� �� C � �� ��0 T� �� C� �� �  � � ,6C � �� �� �� T� �C�  �R��) T� �CC� �A� C� �7� C� g6�� T� ��  �� T� �� �� T� ��  �� +�� � ���� T� �CC� �G�Z� ��6 T� �� CC� �G�� C� DMUWC� >�\� �  6�� T� �C� �8�� � T� �� ��' T� �C�  ��\C�  C�  >��\�� T� �CCC�
 � -�>�w�� T� ��  �� T� ��  �� �� ���(�� ��\�' T� �� C�
 � ����\��' T� �� C�
 � ����\�� ��( �� �C� g�
�(�C� g�
�	���� �� ���(����w� T� �� ���5 T� �C��C� � C� � � T� �  6C� Z�P�]�� T� ��  �� �� ���(�C� >��0�% T� �� C� � �\C� � �\�� �� T� �C� �=� C� �R�� %�� � ��s� T� �a�� !� � �� %�� a���� !� � �� %�� 
���� T� ��  �� T� ��  �� T� �� �� T� �-�� �� T� �� �� T� �a��  T� �� C� � � T� �  6�� � %�C� SYCINST�� � 	��?� Q� � � %�C� SYDAPPL�� � 	��h� Q� � �	 B�� �� U 	 LCACT_KEY LCRETMODULES	 LNNOUSERS
 LCPLATFORM
 LAMODULINS LCINSMODULES	 LCINSPLAT	 LLINSTUSD	 LLAPPLUSD	 GCSYSHOME LCKEY	 LLVALDKEY CAPP_ID LNCOUNT
 LNPLATFORM LCBINARY	 LNPRIMARY	 LCNOUSERS LNLENACT	 LCHIDCHAR LCOLDACT_KEY	 LNTRILVER	 LLTRILVER LCKEYCONTED LCKEYACT LNHIDLEN	 LLALLDONE SYCINST SYDAPPLU	 T�  �� � �� T� ��  �� 5� � � � � J�-�(� � � � � T� �C� W�� %�C� � � SYDAPPL�
��� �( Q�  ��� � � SYDAPPL��� Capp_id� T� �a�� � F�
 �( %�C� SY�� CCC�
 � ��Rg�(	��� � T� �-�� ��� %�C+��� T� �a�� ��� %�C� � � SYDTRANS�
��_�) Q�  ��� � � SYDTRANS��� batchno� T� �a�� � F� � #6� T� �� � �� T� �CC� �� a� � �x6�� %�� ���� Q� � � � � %�� ���� Q�
 � �
 F�� �� %�� ��v�! %�C� � � ACT_KEY.BIN0����! T� �C� � � ACT_KEY.BIN��� T� �C� ��� ��C� ��� T� �C � �� |� �� T� �C � �� |� �� T� �C � �� |� ��" %�C � � � � � � 
���� T� �� �� T� �� �� T� �� �� T� �-�� T�  �� � �� � SYACTKEY.SPR� %�� ���� T� �C� ��� T� �C� ��� T� �C� ���! T� �C� � � ACT_KEY.BIN���  ��C� � � |� � |� ��� ��C� ��� � � �r� T� �� �� T� �� �� T� �� �� T� �-�� � SYACTKEY.SPR� %�� ��n� T� �C� ��� T� �C� ��� T� �C� ���! T� �C� � � ACT_KEY.BIN���  ��C� � � |� � |� ��� ��C� ��� � � ��� J��  �(� �  � �! �! %�C� � � ACT_KEY.BIN0��2�. %�C� �" � C � � � � � � 
	��.� T�  ��  �� T�# �-�� T�$ �C�	 SYGETACTV� �% �� %��# 
� C�  �����G ��C�3 Invalid Activation Key ! System will be terminated.�@�9�& �x�� B�-�� � T�' �C� \�  ��� T� �C�  �' =�� T�! �C�  �' �C�  >\�� %�C� �" � � 	��*�G ��C�3 Invalid Activation Key ! System will be terminated.�@�9�& �x�� B�-�� � � ��� T�  ��  �� T�# �-�� T�$ �C�	 SYGETACTV� �% �� %��# 
� C�  �����G ��C�3 Invalid Activation Key ! System will be terminated.�@�9�& �x�� B�-�� � T�' �C� \�  ��� T� �C�  �' =�� T�! �C�  �' �C�  >\�� %�C� �" � � 	��~�G ��C�3 Invalid Activation Key ! System will be terminated.�@�9�& �x�� B�-�� � � %�� ����G ��C�3 Invalid Activation Key ! System will be terminated.�@�9�& �x�� B�-�� � � %�� T� ����! %�C� � � I900INV.FXP0���� @�� � � I900INV.FXP�� %�C$�( �����6 ��C�" The Demo version has been expired.�@�9�& �x�� B�-�� � ��� T�( �C$��+ D(�� � � I900INV.FXP��	 ldExpDate� � �# %�C � � � � � � 
��G	�G ��C�3 Invalid Activation Key ! System will be terminated.�@�9�& �x�� B�-�� � B�a�� U) 	 GCSYSHOME THIS SYSPATH	 GCACT_KEY LLERROR_LOCAL
 LLOLDVERSN	 LLTRANUSD LLSYAPPL
 LNOLDALIAS CAPP_ID SYDAPPL	 CMDLBUILD BATCHNO SYDTRANS	 LNBATCHNO NBATCHNO LCHANDEL	 LCCONTENT
 GCCOMPNAME GFSUBSTR	 GCLICENCE VALIDATACT_KEY
 GCPRMTMDLS
 GNMAXUSERS
 GCPLATFORM
 LCCOMPNAME	 LCACT_KEY	 LCLICENCE	 LLALLDONE SYACTKEY SPR
 LCFILEPATH	 LCNEWFILE
 LCFILENAME VALIDKEYVALU	 LLPROCESS LCRETURN DOFORMRETVAL CAPTION	 LNPATHPOS	 LDEXPDATE| 4�  �) T�  �CC�	 llReLoginb� L� -� �  6�� 5� � T� �C�	 REPROCESSv��# ��C�	 Buffering�� SYUSTATC��� GM(���� F� � %��  
����+ %�C� INI� OLDVARS� � � � ���V� %�CS��J� T� �-�� T� �CO�� %�C� WIN���=� ~$+�� � WIN��9�) %��	 � � � C� � �C� � �	��5� � � � �	 #�� �� �R� � ��� G� +�a���� %�C�  �� C'	���� %�CS���� :� ��� .� � ��� � %�CS
���� .� � � !� � G � � �[ >� ��� INI��
 ��� OLDVARS��	 ��� � �� ��� � �� ��� W�� ��� � �� ��a�� ��C� SYUSTATC� � ��
 ��Ca��� T� � �CO�� GM(�� �� U 	 LLRELOGIN LCOLDREP SYUSTATC THIS USER_ID STATION	 GLRESTORE LNINIREC COBJ_TYP CUSER_ID	 COBJ_NAME CCOMP_ID ACTIVECOMPANYID	 CPLATFORM CSTATION LKEEP_IT ADDUSERINFORMATION USERSTATICRECORD� ��  � � � ��C�  � SYDAPPL��� T� �C� � ��� F� � %�� � �
 �  � SY	��� � F�	 � G((�
 MDLCMPPRSS�: G(�� � � �	 � � U	� � � � �	 � � G	�� F� � G-(�� � � � � ���	 � � G-(�� � ��� � T� �� ��
 E�� ��( ~$��  � � � � 	�+�� � ����' T� �CC� �� �  � � KEY C� �6�� H�_��� �C� �� P��q�: %�C�& PRMPAD('_MSYSMENU',ALLTRIM(cMstr_Nam))b� U��m�( T� �� PCCCC� �\g�8�� 0���n DEFINE PAD    (ALLTRIM(cMstr_Nam))  OF     _mSYSMENU  PROMPT ALLTRIM(cSub_prpt) AFTER &lcPrivpad &lcHotKey
 s��C� ������ 1��C� �������C� ��� � 2���R T� �C�* PRMBAR(ALLTRIM(cMstr_Nam) , VAL(cBar_pos))b� U� CCC� �C� g��
	�� T� �C� �� -� �� %�� � �� �# %�� � S� C� SYUUSRPR+	����� DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar COLOR SCHEME 3 SKIP FOR .T. &lcHotKey
 ��y DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar COLOR SCHEME 3  &lcHotKey
 � ���y DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar COLOR SCHEME 3  &lcHotKey
 � %�C� �� S� � 
	���� s��C� ������ 1�C� g���C� �����C� ��� � � �+ 1���! DO gfMenuBar WITH POPUP(),BAR()  � F� � G-(�* +�CC� P05PU05C� P05PU05������y� <�C� P05PU05����� � �4 +�CC� P05PU05C� P05PU05C� P05PU05��������% <�C� P05PU05C� P05PU05����� � � U  	 LCMODULID	 LCSUB_CTG LCNOSHOW
 LCMASTMENU SYDAPPL
 CMODPARENT SYCMENU THIS LOGINREQUIRED SYUUSRPR
 MDLCMPPRSS CUSER_ID USER_ID
 CGRPORUSER
 USER_GROUP CAPP_ID ACTIVECOMPANYID	 CPROSS_ID	 CPROCTYPE SYDOBJCT LNBARSDEFIEND
 CBARMODULE CSUB_CTG LCHOTKEY	 CSUB_HKEY CSUB_TYP	 LCPRIVPAD	 CMSTR_NAM
 LLBAREXIST CBAR_POS LCPOPBAR P05PU05K 4�  � T�  �C�  ���* T�  ��  CC�  �R� \� �  � � \6�� %�C� � �
��y �& T�  �� � C�  C� \�  ���\�� �9�z %�CC� � �C� \� � ��\fCC�  �C� \�  ��\f�: CC� � �C� \� � ��\fCC� � �C� \� � ��\f	��5�: T�  �C� � �C� \� � ��\C�  C� \�  ���\�� � � B�C�  ��� U 	 LCDATADIR THIS DATAENV INSTALLPATH SYSPATH� J��  �(�  � � � � � J��        �(� � T� �� � �� T�	 �C�  ��� %�C�
 ���� �
 ��� �� J�C� ACT_KEY.BIN��(� � �� �
 ���
 �� J�C� ��(� � � %�� � ��� �
 ���	 �� <�	 � T� �a�� B�a�� � J�C� �L2��(� � J�C� �L��(� � %�C� ���A�
 ���	 �� <�	 � ��C� ��� T� �a�� B�a�� �9� T� �-�� T�  �C� ��\�� T� �C� ��\�� T� �C� �&�\��3 T� �CCC� �:�
\�� �        � C� �:�
\6�� T� �C� �D�\�� T� �C� �H�\��- T� �C���
 NADABDXMLX� � � � ]��
 ���	 �� <�	 � ��C� ��� � T� �� �� T� �� �� T� ��  �� %�� � ���� T� �a�� B�a�� � 5� � T� �C� DATEv�� G(� AMERICAN�  %�C� �� E�	 C$C� #	��� T� �a�� Set Date To &lcOldSyDat
 B�a�� � Set Date To &lcOldSyDat
 %�C�
 �
���� T� �C� SAFETYv�� G.�! �� �(�� � Act_Key.bin�� SET SAFETY &lcOldSafty
 � U 	 LCCUST_ID
 LCCUST_NAM LCACTKEY	 LCACTPLAT
 LCSYSACTKY LDEXPR_DATE	 GCSYSHOME THIS SYSPATH	 LCSAVEDIR
 LCFILEPATH FILE_HANDLE
 LCFILENAME LLERROR_LOCAL IFP_SIZE LCSTRING
 LCOMPARSYS
 GCCOMPNAME	 GCACT_KEY	 GCLICENCE
 LCOLDSYDAT AMERICAN
 LCOLDSAFTY	 LCNEWFILE�  ��  � T� � ���  ��L T�9� �C� � ��  - C� � �CC� � �
� �  (C� � �� )� �  6�� T� � �C� � � � �� U  VNEWVAL THIS ACTIVECOMPANYID CAPTION
 SYSTEMNAME ACTIVEMODULENAME ACTIVECOMPANYCONSTR MREADCONSTR�  ��  � �� � � T� ��  �� %�C�  �
��� � T� �C� W�� %�C� syccomp���l � F� � G((� cComp_Id� �� � F�  � Q�� � � syccomp��� 1� � %�C�  ���� � T� �C� � �� �
 F�� �� �	 B�� �� U  LCCOMPANYID LNALIAS LCCONSTR SYCCOMP CCOMP_ID THIS SYSPATH
 MGENCONSTR� ��  � T�  ��  �� H�! �Q� �� � SQL��� �` T�  �� Driver={SQL Server};server=C� ��
 ;DATABASE=C� �� ;uid=C� �� ;pwd=C� ��� �� � FOX��Q�� T�  ��; Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB=C� ��V ;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;�� �  T� � �C�  �
�
 C� � �
	��	 B��  �� U	 	 LCCONNSTR
 CCONDRIVER
 CCONSERVER
 CCONDBNAME
 CCONUSERID
 CCONPASWRD THIS ISREMOTECOMP LCARIA4CLASS&  ��  � � � %�� � �� � � U  NERROR CMETHOD NLINE THIS ERRORHANDLERENABLED�  ��  � T� �  ��  �� �� ��C� � �� %�C� � ��� � T� � �CC� DATA275��� %�C� � �
��� �6 T� � �CC� � �R� \�	 � � � � � � \6�� � �� � ��C� � �� B�-�� � U  LCARIA4CLASS THIS SETENVIRONMENT SYSTEMSETUP DATAENV RESTOREENVIRONMENT  ��C�  � �� �� U  THIS CLEANUP� %�C� SYCCOMP�
��7 � F�  � Q��  � � SYCCOMP�� �F � F� � � G((� CCOMP_ID�
 <�� � T� �� �� ~��� T� �� ��� %��  � ��5� F� �& -�C� �  �	 �  �
 �� � � � 	�� F� � %�C� SYUUSRPR4��� s�� ��� "�� � -C� ��� �1�$ s�� ��� "�� � -C� ����a�� � �\� s�� ��� "�� � -C� ��� � %��  � � ��&� G:��� ��� (�a�� T�  � �� �� T�  � �C� � ��� T�  � �CC� ��  � ��  T�  � �CC� �� � � � 6��+ T�  � �CC�  � �� � USDLR� �  � 6�� ��CC� � ��  � �� � T� �� "� � "��K ON SELECTION BAR lnCompBar OF _COMPANIES  DO gfChngComp WITH &lcComp_Id
 � G:��� (�-�� %��  � � ���� G:���  � ��� (�a�� � T�  � �-�� U  THIS SYSPATH SYCCOMP CCOMP_ID
 _COMPANIES	 LNCOMPBAR LOGINREQUIRED SYUUSRPR CUSER_ID USER_ID
 USER_GROUP	 CCOM_NAME ACTIVECOMPANYID
 COMPANYBAR ACTIVECOMPANYNAME DATADIR
 GETDATADIR	 CCOM_DDIR PRNTCOMPANYID	 CCOMPPRNT BASECURRENCY COMPANYSETS
 CCONT_CODE	 LCCOMP_ID REBLDCMP changecompany,     �� changemodule�    �� login    �� setmenu�    �� setenvironment�3    �� restoreenvironmenta;    �� cleanup�<    ��
 messagebox�=    �� restoretoolbars�I    �� systemsetup�I    �� companysetsCR    �� getset�T    �� sethlpfl�U    �� programsetup�V    ��	 doprogram`    �� runprog�c    �� menubarf    �� sethelptopick    �� programcleanup�l    �� winarng�n    �� singleuserprogram�q    �� adduserinformationt    �� reportprint�u    �� doformretvalYx    �� do�x    �� sysexit%|    �� releasetoolbars�}    �� getuserinfoe�    �� getcompanyinformation�    �� getmoduleinformation.�    �� help�    ��
 tooladdnew�    ��
 toolgobttmQ�    ��	 toolclose��    ��
 tooldelete��    �� tooledit��    ��
 toolgonext.�    ��
 toolgopreve�    �� toolsave��    ��	 toolgotopҎ    �� validatact_key�    �� validactivationkey��    �� loguser٠    �� setmultiappmenu$�    ��
 getdatadirb�    �� validkeyvalu�    �� activecompanyid_assign��    �� mreadconstrp�    ��
 mgenconstr��    �� Error��    �� Init�    �� Destroy��    �� rebuildcomp(�    ��1 � �� A D� �A A A � 2aA A A bq � � �A � ��A A ���A A � ��A � ��� dA A A A � UQaq1� !� qq �5�A A � �A A �A B q Q�1Q!!Q"qA ��q � � !� � A�1A A A A �rq � � QA a1� A C 4 q � �q�r A A ��Rr A A � s� �� q A aqA � � r2� !qq � �e�A A ��q A A � aq A A �AA � � ���� A q A A da�� A q A A a� d�� A q A A R�qfBA A q�Q�Aq A A A !� ��� A q A A B baA � � � aA ��� A A A 1��� A 3 � ��� q � q A 3 � ����� � q � �� �� q A �� �� q A A 2q� �� � q A � q� �� � q A � �� A � � A � A A A s� �� q A "Da s1� !qq 
� A � B��cC P!�#C � � b�a a A �q 1�r �A B� �t� B����A � �5#��� �� � � A #�A A aA A !A $12A A � 4T� �A � �A A E�A A A ��q �� � � qt� �� q A � �� � � A � � q br r�� BA � �A CAQ��QA q�B S!A � � ArA A A 0A r a � as� A A ��A AQA � � A � 3 aq�q1�������!����������q�q�1qA�q!����11���q�q��q��qb a a � a a a a q a a � a a q a a a a a a a a a a � � � a � a a � a � � a � � a a a a a a a a c 2 � Q��� AA � A � q2 Q � q #�� A � 3 q�� � S � � �����QR� � � ��� �� �A r�!� RA #!1� � A d�A c3s�A � � Q� � A d������ � 5����4t� ��A �� � �A � !� a A b b�A � A � R � 5 ��aA A 3 b r� �� q A A � a� A r� �� q A 711A RQQ#���qA 1QQQQQQq����A �QA !!A a � �� � A 1TQ� �11A � 2 q � � a� � q� q A !� � � � � � b�� ��� B � A � � A 3 1�� A ����� A 3 q �b1� �A A 3 s %� KT� 1� �� �� � � q A � A B�"�$$��� � � q � �� qA� 1� � �� � � � q A A A A A A �A A C�sq�A QQ� � A ��A q 52111�A � 52112�A � q A A � �A A �2 1� 2t1� !� q�AA A A A A 7��Aq�A �A A � � q2�B � A A A � �4 � � S2�� 1A r��� qA � �A A 3 � � � Cq � QR� A Db ���q � ����� ��A �R� �� � � �A ����� �A ���� �A G � sA 4 � ��a�� A � A 2 q 2#�A A A�� �� q A c� A � � 3 q 1qA A � A� 1�1� c� A ��1A A A � A ��A A 3 q � a ��A AA�A b �� r�� � q�A ��� r B 3 � 1��� A � � A D� � 2 � Dq1� !� q�AA A A A A �A� �A ��� qA 4 q � � � 3 � -!R$a � q A ��A q�A q �a � q � !1A A � � A �1a��B C Q 3 1aq � � � �A � br � �B A � A � Q 3 a�A!�����������aA A 3 q� A� q A � 1111�11!qR� � �1� �� q� A 2A � ��A �QA !!A 111A 5�� 1A A c� A 3 q bq � � �A � 1Q!!QA 2 q s� �� q A �� Q11� qaq� 11� !� qq �!�A A A B q A q 2 q 2 2 2 2 2 2 2 2 2 2 @11�� y� Q� A � !�1�� ���A %�� � � !�a1A � s�� � qqqA �tW� �QA �� A A H � A A C � � � � � � � � B �� A �� A � �1 � 8q� ��� A r �� � � � � ��� A q Q �� � A A A � � A � � � � ���!� � � � 1� � � � � A A � � � � � 1� � � � � A A � ��� � �rqq A 1!��qq A A � � � �rqq A 2!��qq A A � qq A B !�Aaq A � � �A A 1qq A q 4 q �r �1� r � �� � � !a�A A A A � � A � a � a� A � A A � Q � A A A A A a A A ��� � � 2 � T!q �q 1�r �A B� � �t� B����A � $�4T� �A � �A ��A B A �r a ��A AQA 3 s � �1a� ��A A � 3 `�1� � � �� � � A � q � q A 2!� � q � � r � � RSS3SS�� q � A � � � � q A r 1� �q A �Qa �A 5 r "��2 r � � � aq � � �A � A � A � 2 s � � AA�
A � 2 � C 2 r c � �1aA �� � q A 3 � a 3 y� �� q A � � � q br r�� BA � �A CAQ��QA q�B � ArA � 2                       �     v   	  �6  �   �   �6  �7  v    �7  �y  �  �  �y  ��  X  X  '�  ��  �  f  ʅ  �  �  o  �  �  �  �  �  ��  �  �  ��  ��  �    �  ˮ    <  �  X�  A  I  w�  ��  P  S  ��  �  ]  �  8�  ��  �  �  ��  ��  E  �  �  ��  ^  .  ��  A�  �  ;  f�  �  �  P  4�  ��  �  p  ��  #�    �  L�  '�  6  �  I�  W�  L  �  z�  ��  n  �  �  k�  }  �  ��  R�  �  �  x�  � �     H �  B  t * C  U  U  \  |  + / �  ~  P j �  �  � � �  �  � � �  �    �  �  = X �  �  y � �  �  � � �  �  � 	 �  �  ) C �  �  h j6 �  �  �6 �I �	  _  
J $P 2
  �  JP c o
  �  ;c f �
  �  +f �s �
    �s hu i    �u /w s  %  Pw �y �  0  �y �z �  4  �z � �  C  � i� �  F  �� 3� �   )   ��                       )���    �(  �(                        8   %   �$      (  @  �%          �  U  � ��  � � %�C����J � � � �C�� ������� ��C�� � � ��� �" � � �C��  ����C��  ����" � � �C��  ����C��  ���� ��C��  � � ��� ��C��  � � ���* � � �C� � �����C� � ����) T� � �C� � �������� Value��% T� � �C� � �������� V��% T� � �C� � �������� V��% T� � �C� � �������� V�� T� � � �C� � ����/ T� � � �� this.Parent.aAvailableFields�� T� � �	 ���� T� � �
 ���� T� � � �C� � ���� �� � � T� �� � �� DEFINE POPUP &lcPopName
 �� ���(�C� � ������I DEFINE BAR lnCount of &lcPopName PROMPT THIS.aFieldValues[lnCount,1] 
 �� T� � � �� � �� T� � �	 ��	�� �� � � � �� � � � ��C� � �� U  TAFILEFIELDS TAFILEFILTER THIS AFILTEREXPR AAVAILABLEFIELDS AFIELDVALUES CBXEXPRLEFT NUMBEROFELEMENTS	 ROWSOURCE ROWSOURCETYPE BOUNDCOLUMN CBXEXPRRIGHT	 LCPOPNAME LNCOUNT
 VALUEPOPUP REQUERY	 BUILDLIST� ��  � ��C� � � �� %�CC��� � �
���� ��  ���(�C� � ������  %�C �  �� � � .OR.��� � ��CC� _�(Q �  � � � ��' ��CC�  OR �
� _� �  �� � � ��  ��CC� _�(Q �  �� � � ��  ��CC� _�(Q �  �� � � �� .� � �� � H���� �C �  �� � � L��H� T� �� Yes,.T.,No,.F.�� �C �  �� � � F����9 T� �C� GetC �  �� � � field� Validentries� �� �" ��CC �  �� �  �  � � � �� T� � �	 ��  ��R T� �
 � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,1]�� T� �
 � �C �  �� � ��+ T� � � �� � �	 ������ �
 � ��; ��CCC �  �� � � � Is not� � Is6 �  �� � � ��% ��CC �  �� �  �  �� � � �� %�C �  �� � � V��c� %�C� ���#�% ��CC �  �� �  �  �� � � �� �_� T� � � �� �� T� � � �� ��" %�C� ,C �  �� � �� ���; T� � � �CC �  �� � �C� ,C �  �� � ��\��8 T� � � �CC �  �� � C� ,C �  �� � ��\��/ ��C� � � � ,� � �  �  �� � � �� �[� T� � � �C �  �� � ��  ��C� � �  �  �� � � �� � � ��� �� �! T� �C� � C �  �� � ��� T� �C� � � ����% ��CC � �� �  �  �� � � �� � ��R T� � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,5]��R T� � � ��? this.Parent.aFilterExpr[this.Parent.lstExpression.ListItemID,4]�� T� � � ��  �� � U  LNCOUNT THIS LSTEXPRESSION CLEAR AFILTEREXPR ADDITEM ADDLISTITEM LCVALIDENTRIES GFDATABASEPROP	 LISTINDEX CBXEXPRLEFT CONTROLSOURCE VALUE LIST DISPLAYVALUE CBXFROM	 ROWSOURCE CBXTO LNBARPOS AFIELDVALUES CBXEXPROPERATOR CHKIS CBXEXPRRIGHT ��  � H� �� ��  ���P �+ T� � � �� � � ������ � � �� ��  ���� �A T� � � �� � � �����C� � � � � Is not� � Is6�� ��  ���� �+ T� � � �� � � ������ �	 � �� ��  ����" %�C� � � �� �
 � V��~� �� � H�%���" �C� � � �� �
 � L��b� T� �� Yes,.T.,No,.F.��" �C� � � �� �
 � F����> T� �C� GetC� � � �� �
 � field� Validentries� �� � %�C� ���
�5 T� � � �� � � �����C� � � �� �
 �� �z� T� � � �� �� T� � � �� ��' %�C� ,C� � � �� �
 �� �� �E T� � � �CC� � � �� �
 �C� ,C� � � �� �
 ��\��B T� � � �CC� � � �� �
 C� ,C� � � �� �
 ��\��: T� � � �� � � ������ � � � ,� � � �� �v�# T� � � �C� � � �� �
 ��+ T� � � �� � � ������ � � �� � � �� �� �& T� �C� � C� � � �� �
 ��� %�� � ��� T� �C� � � ����0 T� � � �� � � �����C � �� � �� � � � U  TNCOLUMN THIS LSTEXPRESSION LIST	 LISTINDEX CBXEXPRLEFT DISPLAYVALUE CHKIS VALUE CBXEXPROPERATOR AFILTEREXPR
 LISTITEMID LCVALIDENTRIES GFDATABASEPROP CBXFROM	 ROWSOURCE CBXTO LNBARPOS AFIELDVALUESR ��  � H� �� �" �C� � � �� � � L��Q � T�  �� Yes,.T.,No,.F.��" �C� � � �� � � F��� �> T�  �C� GetC� � � �� � � field� Validentries� �� � H�� �K�( �C� � � �� � � Between��t� %�C�  �
���� T� � � �-�� T� � �	 �� � �	 ��' %�C� ,C� � � �� � �� ����E T� � �
 �CC� � � �� � �C� ,C� � � �� � ��\��B T� � �
 �CC� � � �� � C� ,C� � � �� � ��\�� ���# T� � �
 �C� � � �� � �� � T� � � �� � � �� T� � �	 �� � �	 �� T� � � ��  �� T� � � �a�� T� � � ��  �� T� � � �a�� ��C� � � �� \��
 {SPACEBAR}�� �p� T� � � �-�� T� � �	 �� � �	 ��' %�C� ,C� � � �� � �� ��~�E T� � �
 �CC� � � �� � �C� ,C� � � �� � ��\��B T� � �
 �CC� � � �� � C� ,C� � � �� � ��\�� ���# T� � �
 �C� � � �� � �� �" %�C� � � �� � � F��� �� � �A T� �C�	 DBGetPropC� � � �� � � FIELD�	 INPUTMASK� ��> T� �C�	 DBGetPropC� � � �� � � FIELD� FORMAT� �� %�C� �����$ T� �CC� � � �� � � � �� �$ T� � � �CC� �
� � � �  6��* T� � � �CC� �
� � �	 � � � 6��* T� � � �CC� �
� � �	 � � � 6�� � T� � � �� � � �� T� � �	 �� � �	 �� T� � � �a�� T� � � �a�� ��C� � � �� �( �C� � � �� � � In List���� %�C�  �
���� ��� � 2�K� %�C�  �
��}� T� � � �-�� T� � � ��  ��# T� � �
 �C� � � �� � �� T� � � �a�� T� � �	 �� � �	 �� T� � � �� � � �� ��C� � � �� \��
 {SPACEBAR}�� �G�" %�C� � � �� � � F���� �� � �A T� �C�	 DBGetPropC� � � �� � � FIELD�	 INPUTMASK� ��> T� �C�	 DBGetPropC� � � �� � � FIELD� FORMAT� �� %�C� ���d�$ T� �CC� � � �� � � � �� �* T� � � �CC� �
� � �	 � � � 6��* T� � � �CC� �
� � �	 � � � 6�� � T� � � �-�� T� � � �a��# T� � �
 �C� � � �� � �� T� � �	 �� � �	 �� T� � � �� � � �� ��C� � � �� � � U  LCVALIDENTRIES THIS AFILTEREXPR LSTEXPRESSION
 LISTITEMID GFDATABASEPROP CBXEXPRRIGHT VISIBLE CBXFROM TOP VALUE CBXTO WIDTH	 ROWSOURCE SETFOCUS TXTFROM TXTTO LCINPUTMASK LCFORMAT GETMASK FORMAT	 INPUTMASK- ��  � � � T�  �� � � �� T� �� � � �� %�� � � ���� � T� � �	 ��  �� T� �
 �	 ��  �� T� � �	 ��  �� � ��C� � � � � � �� ��C� � �� %��  � � � ��� � T� � � ��  ��� �� � T� � � ��  �� � ��C� � � �� T� � �-�� ��C� � � �� U  LNINDEX LNITEMID LNCOUNT THIS LSTEXPRESSION	 LISTINDEX
 LISTITEMID	 LISTCOUNT CBXEXPRLEFT CONTROLSOURCE CBXEXPROPERATOR CHKIS
 REMOVEITEM ARRANGE WHEN ADDEDNEWBAR PARENT REFRESH$ ��  � � � � %�� � � � ��C � � �� � � ����� �^ � � ������� � ��  ���(�� � � ��� � T� � � ��  �� T� �� � � ���� T� ��  ���� ��C� �	 �� � �� ��� �� � �	 �C�� ������� ��C�� � �	 ��� ��C� �
 �� U  LNCOUNT	 LALSTROWS LNSOURCEELM LNDESELM THIS LSTEXPRESSION	 LISTCOUNT	 LISTINDEX
 LISTITEMID AFILTEREXPR	 BUILDLIST� ��  � � T�  �� � �� �� ���(�C� � ����� �{ SET SKIP OF BAR lnCount OF &lcPopName !(this.aFilterExpr[this.lstExpression.ListItemID,3] $ this.aOperators[lnCount,2])
 �� T�  �� � �� �� ���(�C� � ������� SET SKIP OF BAR lnCount OF &lcPopName !(this.aFilterExpr[this.lstExpression.ListItemID,3] = this.aFieldValues[lnCount,3] ) AND this.aFieldValues[lnCount,3]<>'V'
 �� U 	 LCPOPNAME LNCOUNT THIS OPERATORPOPUP
 AOPERATORS
 VALUEPOPUP AFIELDVALUES� ��  � %�C� � ������ � T� �C� � �  ��� %�� � ��� � T� �C� � � ����G %�CC � �� � �
�) C� THIS.aAvailableFields[lnPos,5]b� C	��� � B�C � �� � �� � � � H�� ��� �C�  b� CM��+� %�C�  b� M��� B�C� X�<Q�� �'� B�C� XCC�  �>Q�� � �C�  b� N��?� �C�  b� Y��p� B�C� 9�Q� .C�	�Q�� �C�  b� D���� B��
 99/99/9999�� �C�  b� T���� � U  TCFIELDNAME THIS AAVAILABLEFIELDS LNPOSd  ��  � T�  �� � �� RELEASE POPUPS &lcPopName
 T�  �� � �� RELEASE POPUPS &lcPopName
 U 	 LCPOPNAME THIS OPERATORPOPUP
 VALUEPOPUP� T�  � �C� �� T�  � �C� �� �  � �������  T�  � �������� Like��# T�  � �������� CMLNDTY��( T�  � �������� Greater Than��" T�  � �������� CMNDTY��% T�  � ��������	 Less Than��" T�  � �������� CMNDTY��, T�  � �������� Greater Or Equal��" T�  � �������� CMNDTY��) T�  � �������� Less Or Equal��" T�  � �������� CMNDTY��# T�  � �������� Between��! T�  � �������� CNDTY��$ T�  � �������� Contains�� T�  � �������� CM�� �� � � T� ��  � �� DEFINE POPUP &lcPopName
 �� ���(�C�  � ������G DEFINE BAR lnCount of &lcPopName PROMPT THIS.aOperators[lnCount,1] 
 �� T�  � � ��  � �� T�  � �	 ��	�� U
  THIS OPERATORPOPUP
 GFTEMPNAME
 VALUEPOPUP
 AOPERATORS	 LCPOPNAME LNCOUNT CBXEXPROPERATOR	 ROWSOURCE ROWSOURCETYPEg  %��  � � � ��@ � ��C� ENABLED-�  � �� T�  � � �a�� �` � ��C� ENABLEDa�  � �� � U  THIS LSTEXPRESSION	 LISTCOUNT SETALL ENABLED initializefields,     ��	 buildlist    �� buildbar�
    �� getvalue�    ��
 removeline�    �� arrange�    �� skipoperator^    �� getmaskD    �� Destroy&     �� Init�     �� Refresh-$    ��1 � �1A !!21��QQQ��11�� ���A b1� � � 2 q ���qA A r � ����A "1!���Q�� Q� 11!���� �A B � q �QA A !!1A 3 q � ��!q � !�!�A � Q� 11qQ!�� 1�A A � q a�A A A 2 q � !�!�A � ��qQ!� 1A ��111� �qQ!� 1A !� �� AA A��A ��A �� A � 11��1� !� �� AA ��A 1��A A 2 � 11q111A �� qq� 1A � 3 1q�� 1A �1�a�A �1� 3 � ��A �A
A 7 q �a�qAA A A � QA� � 1A AB�A1AB 2 q ��3 a1�!R!�!�!2B�� ��qA b13 q�� �A 1                       �         �  >  ,   T   ]  �  f   }     �*  �   �   �*  �,  �   �   -  /  �   �   A/  I1       g1  G4  !    e4  �4  =    5  �8  E  9  �8  u9  m   )   �(                       3nworkarea
editmode
previewmode
topfile
endfile
oldsetfields
oldsetdelete
oldmultilocks
oldbuffering
addmode
oldrefresh
oldrec
oldreprocess
oldalias
previewinit
usedataenv
gridref
odatarelation
viewkey
gridalias
viewtype
parentkey
hasgeneral
oldtalk
hasmemo
editforecolor
editbackcolor
editdisforecolor
editdisbackcolor
owindparent
bmppath
firsttime
allowadd User Has Privilidge to Add New Record Or Not
allowedit User Has Privilidge to Edit Records Or Not
allowdelete User Has Privilidge to Delete from Records Or Not
formkey Key fields Of the Active Form
activemode
selectmode
keyobject
*buttonrefresh 
*initvars 
*updaterows 
*setcaption 
*navrefresh 
*getgridref 
*getkeyexpr Method to return the key fields of the active form
*filewaschanged Method to Return if there was any fields was edited in the buffer and was not saved
*cancontinue Method to Return if the user can change the record of the master file or as him to save the changes before changing
*changetoaddmode Change The Form Objects to the AddMode
*getname 
     �PROCEDURE adjustobjectwidth
LPARAMETERS oControl

IF TYPE("oControl")  = "O" AND ;
   oControl.MaxLength # 0  AND ;
   UPPER(LEFT(oControl.Class,4)) = "ARIA"
  
  *-- Fill lcText With x OF "W" Where x is maxlength prpoperty 
  *-- value and "W" is the Largest Char or number in width  
  lcTxtPic  = UPPER(oControl.TextPicture)
  lcChToUse = IIF(lcTxtPic = "N", "7", "W")
  lcText    = REPLICATE(lcChToUse, oControl.MaxLength)

  *-- Save the form font properties.
  llFontBold              = ThisForm.FontBold 
  llFontItalic            = ThisForm.FontItalic 
  llFontShadow            = ThisForm.FontShadow 
  llFontStrike            = ThisForm.FontStrikeThru 
  llFontOutline           = ThisForm.FontOutline 
  llFontCondence          = ThisForm.FontCondense 
  
  *-- Change the form font properties to take the control's font
  *-- properties to be able to pass them to the "WFONT()" function.
  ThisForm.FontBold       = oControl.FontBold
  ThisForm.FontItalic     = oControl.FontItalic     
  ThisForm.FontShadow     = oControl.FontShadow     
  ThisForm.FontStrikeThru = oControl.FontStrikeThru 
  ThisForm.FontOutline    = oControl.FontOutline    
  ThisForm.FontCondense   = oControl.FontCondense   
  
  *-- Get The Number Of Cahracters in lcText String With Respect 
  *-- to the Average character Width Under The specified 
  *-- Font Name, Size And Style 
  lnWidth = TXTWIDTH(lcText, oControl.FontName, oControl.FontSize, WFONT(3))

  *-- Adding 1.5 Characters Because of Borders Width   
  lnWidth = lnWidth + 2                                     
  
  *-- Get The Average Cahracter Width Under The Specified 
  *-- Font Name, Size And Style
  lnFontLength = FONTMETRIC(6, oControl.FontName, oControl.FontSize, WFONT(3))* lnWidth 
  
  *-- This Pixels becauser of the ScrollBar  
  DO CASE
    CASE INLIST(UPPER(oControl.BaseClass) ,;
                UPPER("ComboBox")         ,;
                UPPER("ListBox"))
      lnBorWidth = IIF(lcTxtPic="N", 10, 20)
    
    CASE UPPER(oControl.BaseClass) = UPPER("TextBox")
      lnBorWidth = IIF(lcTxtPic="N", 5, 10)
    
    OTHERWISE
      lnBorWidth = 0
  ENDCASE
  
  *-- Set the control's width.
  oControl.Width = lnFontLength &&+ lnBorWidth

  *-- Re-set the form font properties.
  ThisForm.FontBold       = llFontBold
  ThisForm.FontItalic     = llFontItalic
  ThisForm.FontShadow     = llFontShadow
  ThisForm.FontStrikeThru = llFontStrike
  ThisForm.FontOutline    = llFontOutline
  ThisForm.FontCondense   = llFontCondence
ENDIF

oControl = .NULL.

ENDPROC
PROCEDURE QueryUnload
IF TYPE('oAriaApplication')='O'
  IF THISFORM.TABINDEX <>1 AND TYPE('THISFORM.PARENT')='O'
    THISFORM.HIDE
    NODEFAULT  
  ELSE
    IF (THISFORMSET.ActiveMode$'EA') AND !EMPTY(ThisFormSET.FormHasToolBar)
       IF THISFORMSET.ActiveMode # 'S' AND !THISFORMSET.CanContinue()
         NODEFAULT  
         RETURN .F.
       ELSE
         IF THISFORMSET.ActiveMode = 'A'
           THISFORMSET.UNDO(.T.)
         ENDIF  
       ENDIF  
    ENDIF  
    THISFORM.VISIBLE=.F.    
    IF TYPE('THISFORM.PARENT')='O'
      RELEASE THISFORMSET
    ELSE
      RELEASE THISFORM           
    ENDIF  
  ENDIF
ENDIF
ENDPROC
PROCEDURE Hide
IF TYPE('oAriaApplication')='O'
    =oAriaApplication.WinArng()
ENDIF  
ENDPROC
PROCEDURE Unload
IF THISFORM.TABINDEX=1
   THISFORM.VISIBLE = .F.
ENDIF  
*B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu [Begin]
IF TYPE('oAriaApplication') = 'O'
  oAriaApplication.ReBldCmp = .T.
  oAriaApplication.SetMenu(oAriaApplication.ActiveModuleID,'A')
EndIf
*B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu [End]  


ENDPROC
PROCEDURE Init
This.AutoCenter = This.AutoCenter

IF TYPE('oAriaApplication')='O' 
  *-- Set the currency and date settings inside the form 
  *-- because these setings have to be set for each 
  *-- data session.
  USE (oAriaApplication.SysPath+"sycInt") IN 0 ;
  AGAIN ALIAS Intern ORDER cContCode
  IF SEEK(oAriaApplication.DefaultCountry, "Intern")
    lcCurPos = ALLTRIM(UPPER(Intern.cCurrency))
    SET DATE TO ALLTRIM(UPPER(Intern.cDate_Type))
    SET CURRENCY TO ALLTRIM(UPPER(Intern.cCurrencyI))
    SET CURRENCY &lcCurPos
  ENDIF
  USE IN Intern
  SET CENTURY ON
  SET TALK OFF
  SET SAFETY OFF
  
  *-- Set the active module, the multi run flag, and the 
  *-- data dir properties.
  THISFORM.ModuleID = oAriaApplication.ProgramModuleID
  THISFORM.MultiRun = oAriaApplication.MultiRun
  THISFORM.DataPath = oAriaApplication.DataDir
  
  IF TYPE('gnProgCopy')='N' 
    IF gnProgCopy > 1
      THISFORM.Caption = THISFORM.Caption + " /" +STR(gnProgCopy,2)
    ENDIF  
    THIS.PROGRAMCOPYNO = gnProgCopy
  ENDIF  
  
  IF TYPE('oAriaApplication.oToolBar')='O'
    oAriaApplication.oToolBar.Init()
  ENDIF
ENDIF
ENDPROC
PROCEDURE Activate
IF TYPE('oAriaApplication')='O'
   =oAriaApplication.WinArng()    
  IF !EMPTY(THISFORMSET.FormHasToolbar)
    oAriaApplication.ProcessID = SUBSTR(THISFORMSET.Name,2)
    IF TYPE('oAriaApplication.oToolBar')='O'    
      oAriaApplication.oToolBar.INIT(THISFORMSET)
    ENDIF  
  ELSE
    oAriaApplication.ProcessID = SUBSTR(THISFORMSET.NAME,2)  
    IF TYPE('oAriaApplication.oToolBar')='O'
      oAriaApplication.oToolBar.INIT()
    ENDIF  
  ENDIF  
ENDIF  

ENDPROC
PROCEDURE KeyPress
LPARAMETERS nKeyCode, nShiftAltCtrl

IF TYPE("ThisForm.Parent") = "O" AND UPPER(ThisForm.Parent.Class) = "ARIAFORMSET"
  DO CASE
    CASE nShiftAltCtrl = 2 AND nKeyCode = 22     && INSERT KEY
      ThisFormSet.AddNew

    CASE nShiftAltCtrl = 2 AND nKeyCode = 7      && DELETE
      ThisFormSet.Delete

    CASE nKeyCode = 27     && ESc KEY
      ThisFormSet.UnDo(.F.)

    *CASE nKeyCode=1       && Home Key
  ENDCASE
ENDIF
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
THIS.Parent.Error(nError, cMethod, nLine)
ENDPROC
    �>PROCEDURE changecompany
PARAMETERS lcComp_Id,llFroMnu,llChekFrst

llFroMnu   = IIF(TYPE('llFroMnu')  ='U',.T.,llFroMnu  )
IF PCOUNT() < 3
  llChekFrst = .T.
ENDIF


*** Check if selecting the current company
IF lcComp_Id==THIS.ActiveCompanyID
  IF llFroMnu
      =THIS.MessageBox("INM00022B00000","ALERT",lcComp_Id)
   ENDIF   
   RETURN
ENDIF

IF llChekFrst
  *** Check if there is any active program
  IF _SCREEN.FORMCOUNT>1
    =THIS.MessageBox("INM00021B00000","ALERT")
    RETURN
  ENDIF  
ENDIF  

IF USED("syccomp")
  SELECT syccomp
  SET ORDER TO cComp_Id
ELSE
  SELECT 0
  USE (THIS.SysPath+"syccomp") ORDER 1
ENDIF


IF SEEK(lcComp_Id)

  IF !EMPTY(ccompprnt) .AND. THIS.ParentModule .AND. !(THIS.ActiveModuleID $'SM|SY') .AND. !EMPTY(THIS.ActiveModuleID)
    =THIS.MessageBox("INM00158B00000","ALERT",ALLTRIM(sycComp.cCom_Name)+"|"+ALLTRIM(THIS.ActiveModuleName))
    RETURN 
  ENDIF 

  IF ATC(',',THIS.ActiveModuleID) = 0
    *** Check if the selected company have linke to the current module
    IF AT(THIS.ActiveModuleID,ALLTRIM(mComp_Mdl)) = 0 .AND. !(THIS.ActiveModuleID $'SM|SY') .AND. !EMPTY(THIS.ActiveModuleID)
      =THIS.MessageBox("INM00151B00000","ALERT",ALLTRIM(THIS.ActiveModuleName)+"|"+ALLTRIM(sycComp.cCom_Name))
      RETURN 
    ENDIF 
    lcDataDir  = ALLTRIM(cCom_dDir)
     IF UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) = UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
         UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) <>  UPPER(SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2)))
       lcDataDir= SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2))+SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
     ENDIF
    llSetDon = .T.
    IF  !EMPTY(THIS.ActiveModuleID) AND  !(THIS.ActiveModuleID $ 'SM|SY')
      IF SYDAPPL.lsetreq
        llSetDon = ATC(THIS.ActiveModuleID,SYCCOMP.MMODLSET)>0
        *Check if the setup was done for the selected module 
        IF !llSetDon 
          * Setup was not done to this module yet,"+CHR(13)
          *** you have to setup the module through the company information 
          *** program...!
          =THIS.MessageBox("INM00145B00000","ALERT")
          RETURN
        ENDIF     
      ENDIF
    ENDIF
  ELSE
  *E301141,1 Hesham (Start)
  *E301141,1 if the active menu is a multi module menu call
  *E301141,1 then check for the installation for all the combined modules
  *E301141,1 in the selected company before changing it
    lcAppID = SUBSTR(THIS.ActiveModuleID,1,ATC(',',THIS.ActiveModuleID)-1)
   = SEEK(lcAppID,'SYDAPPL')  
   lcAppID = IIF(!EMPTY(SYDAPPL.CBARMODULE),ALLT(SYDAPPL.CBARMODULE),lcAppID)
   lcAppID = IIF(ATC('|',lcAppID)=0,lcAppID,SUBSTR(lcAppID,1,ATC('|',lcAppID)-1))
    DIME laModSelected[1,1]
    laModSelected = ''
    =gfSubStr(lcAppID,@laModSelected)
    lcAppID = ''    
    IF !EMPTY(laModSelected[1,1])
      PRIVATE lnCount
      FOR lnCount = 1 TO ALEN(laModSelected,1)
        *Haythar [Begin]
        *IF SEEK(laModSelected[lnCount,1],'SYDAPPL') AND AT(laModSelected[lnCount,1],SYCCOMP.MMODLSET) > 0
        IF SEEK(laModSelected[lnCount,1] , 'SYDAPPL') .AND.;
           AT(laModSelected[lnCount,1] , SYCCOMP.mComp_Mdl) > 0 .AND.;
           (!SYDAPPL.lSetReq .OR. AT(laModSelected[lnCount,1] , SYCCOMP.mModlSet) > 0)
        *Haythar [End]

          lcAppID = lcAppID+IIF(EMPTY(lcAppID),'',',')+laModSelected[lnCount,1]
        ENDIF
      ENDFOR
      IF EMPTY(lcAppID)
        =THIS.MessageBox("INM00151B00000","ALERT",ALLTRIM(THIS.ActiveModuleName)+"|"+ALLTRIM(sycComp.cCom_Name))
        RETURN
      ENDIF
      lcAppID = lcAppID + IIF(ATC(',',lcAppID)=0,',','')
      THIS.ActiveModuleID = lcAppID
    ENDIF
    *E301141,1 Hesham (End)
  ENDIF  
  SELECT sycComp
  THIS.ActiveCompanyID = lcComp_Id
  THIS.ActiveCompanyName = ALLTRIM(sycComp.cCom_Name)
*B603122,3  Hesham (Start)
*B603122,3  make the system use a remote data dir
*!*	  THIS.DataDir  = ALLTRIM(cCom_dDir)
*!*	   IF UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) = UPPER(SUBSTR(THIS.DataDir,1,ATC('\',THIS.DataDir,2))) AND ;
*!*	       UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) <>  UPPER(SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2)))
*!*	     THIS.DataDir= SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2))+SUBSTR(THIS.DataDir,ATC('\',THIS.DataDir,2)+1)
*!*	   ENDIF
   THIS.DataDir  = THIS.GetDataDir(ALLTRIM(cCom_dDir))
*B603122,3  Hesham (End)
*  THIS.CompanyDefaultModule = ALLTRIM(mComp_Mdl)
*  THIS.CompanyLevel = IIF(EMPTY(ccompprnt),'P','C')
  THIS.PrntCompanyID = IIF(EMPTY(ccompprnt),cComp_Id,ccompprnt)
  THIS.DefaultCountry = IIF(EMPTY(cCont_code),'USA',cCont_code)
  THIS.BaseCurrency = IIF(EMPTY(CCURRCODE),'USDLR',CCURRCODE)
  THIS.CurrentYear = CCURR_YER
  THIS.CurrentPeriod = CCURR_Prd
  THIS.CompanyInstalledModules = ALLTRIM(mcomp_mdl)
  THIS.CompanySetupModules = ALLT(mmodlset)
  =THIS.CompanySets(ALLTRIM(syccomp.ccont_code))
  IF (THIS.LoginRequired AND !EMPTY(THIS.ActiveModuleID) AND THIS.ActiveModuleID <>'SM') OR;
    ATC(',',THIS.ActiveModuleID)>0
    =THIS.SetMenu(THIS.ActiveModuleID,'A')
  ENDIF  
  *B122770,1 WAM Get Current site ID
  oGetMemVar     = CREATEOBJECT("GetMemVar")
  THIS.CurrentSite = PADR(oGetMemVar.DO("M_CURRSITE",THIS.ActiveCompanyId),6)
  RELEASE oGetMemVar
  *B122770,1 WAM (End)

  SET MARK OF POPUP _COMPANIES .F.
  IF llFroMnu 
    SET MARK OF BAR BAR() OF _COMPANIES .T.
    THIS.CompanyBar = BAR()  
  ELSE
    FOR lnChkBar  = 1 TO CNTBAR('_COMPANIES')
      IF ALLTRIM(UPPER(THIS.ActiveCompanyID+"-"+THIS.ActiveCompanyName)) = ALLTRIM(UPPER(PRMBAR('_COMPANIES',lnChkBar)))
        SET MARK OF BAR lnChkBar OF _COMPANIES .T.
        THIS.CompanyBar = lnChkBar
        EXIT
      ENDIF
    ENDFOR
  ENDIF  

  _SCREEN.CAPTION = ALLTRIM(THIS.SystemName)+" - "+ALLTRIM(THIS.ActiveModuleName)+;
            IIF(!EMPTY(THIS.ActiveCompanyID),' ('+ALLTRIM(THIS.ActiveCompanyID)+')','')

  IF USED("syustatc")
    SELECT syustatc
    SET ORDER TO CUSER_ID
  ELSE
    SELECT 0
    USE (THIS.SysPath+"syustatc") ORDER CUSER_ID
  ENDIF
  IF SEEK ('INI'+'OLDVARS'+THIS.User_ID+THIS.Station,'syustatc')
    REPLACE syustatc.cComp_ID WITH THIS.ActiveCompanyID
    =TABLEUPDATE(0,.T.)
  ENDIF  
  

ENDIF  


ENDPROC
PROCEDURE changemodule
PARAMETER lcAppID                      && Accept parameters.

llInitMenu = .F.

*** Return if no company created or selected
IF EMPTY(THIS.ActiveCompanyID) .AND. lcAppID <> 'SM' 
  =THIS.MessageBox("INM00058B00000","DIALOG")
  *B124685,1 WLD If the companies more the 40 company,display all companies [Begin]
  THIS.SetMenu('SY','I','NOSHOW')
  *B124685,1 WLD If the companies more the 40 company,display all companies [End  ]  
  SHOW MENU _MSYSMENU
  RETURN
ENDIF

*** Return if calling the active application
IF UPPER(lcAppID) = UPPER(THIS.ActiveModuleID)
  =THIS.MessageBox("INM00046B00000","DIALOG",THIS.ActiveModuleName)
  *B124685,1 WLD If the companies more the 40 company,display all companies [Begin]
  THIS.SetMenu(lcAppID,'A')
  *B124685,1 WLD If the companies more the 40 company,display all companies [End  ]  
  SHOW MENU _MSYSMENU  
  RETURN                              
ENDIF

*IF !(lcAppID $ THIS.CompanySetupModules) .AND. lcAppID <> "SM"
*  =THIS.MessageBox("INM00239B00000","ALERT")
*  SHOW MENU _MSYSMENU  
*  RETURN                              
*ENDIF

lcCerFile  = ALIAS()


IF !USED("SYDAPPL")
  SELECT 0
  USE (THIS.SysPath+"SYDAPPL")
ELSE
  SELECT SYDAPPL
ENDIF
SET ORDER TO TAG CAPP_ID
*E301141,1 Hesham (Start)
*E301141,1 check if the module we are setting is a combined module
IF ATC(',',lcAppID)=0 AND SEEK(lcAppID)  
  lcAppID = IIF(!EMPTY(SYDAPPL.CBARMODULE),ALLT(SYDAPPL.CBARMODULE),lcAppID)
  lcAppID = IIF(ATC('|',lcAppID)=0,lcAppID,SUBSTR(lcAppID,1,ATC('|',lcAppID)-1))
ENDIF
lcApplicationName = ''
llParentModule = .F.
*E301141,1 check if the module bar the user select is a multi module bar
IF ATC(',',lcAppID)>0
  *E301141,1 get the menu bar modules
  DIME laModSelected[1,1]
  laModSelected = ''
  =gfSubStr(lcAppID,@laModSelected)
  IF !EMPTY(laModSelected[1,1])
    PRIVATE lnCount
    lcAppID = ''
    *E301141,1 check for the installed modules in the active comapey
    FOR lnCount = 1 TO ALEN(laModSelected,1)
      *Haythar [Begin]
      *IF SEEK(laModSelected[lnCount,1]) AND AT(laModSelected[lnCount,1],THIS.CompanyInstalledModules) > 0
      IF SEEK(laModSelected[lnCount,1]) .AND.;
         AT(laModSelected[lnCount,1] , This.CompanyInstalledModules) > 0 .AND.;
         (!lSetReq .OR. AT(laModSelected[lnCount,1] , This.CompanySetupModules) > 0)
      *Haythar [End]
        lcAppID = lcAppID+IIF(EMPTY(lcAppID),'',',')+laModSelected[lnCount,1]
      ENDIF
    ENDFOR
    *** Return if calling the active application
    IF UPPER(lcAppID) = UPPER(THIS.ActiveModuleID)
      =THIS.MessageBox("INM00046B00000","DIALOG",THIS.ActiveModuleName)
      SHOW MENU _MSYSMENU  
      RETURN                              
    ENDIF
    IF EMPTY(lcAppID)
      =THIS.MessageBox("INM00151B00000","ALERT",ALLTRIM(PROMPT())+"|"+THIS.ActiveCompanyName)
      SHOW MENU _MSYSMENU
      RETURN   
    ENDIF
    lcAppID = lcAppID + IIF(ATC(',',lcAppID)=0,',','')
    lcApplicationName = ALLTRIM(SUBSTR(SYDAPPL.CBARMODULE,ATC('|',SYDAPPL.CBARMODULE)+1))
  ENDIF
ELSE
*E301141,1 Hesham (End)
  *** Check if the selected module linked to the system
  IF SEEK(lcAppID)

  *  IF THIS.CompanyLevel='C' .AND. THIS.ParentModule
    IF !EMPTY(THIS.PrntCompanyID) .AND. THIS.ParentModule
      =THIS.MessageBox("INM00157B00000","ALERT",ALLTRIM(PROMPT()))
      IF !EMPTY(lcCerFile) AND USED(lcCerFile)
        SELECT(lcCerFile)
      ENDIF
      SHOW MENU _MSYSMENU
      RETURN 
    ENDIF

    *** Check if the selected module is installed to the current company 
  
    IF AT(lcAppID,THIS.CompanyInstalledModules) = 0 .AND. lcAppID <>'SM' 
      =THIS.MessageBox("INM00151B00000","ALERT",ALLTRIM(PROMPT())+"|"+THIS.ActiveCompanyName)
      IF !EMPTY(lcCerFile) AND USED(lcCerFile)
        SELECT(lcCerFile)
      ENDIF
      SHOW MENU _MSYSMENU
      RETURN 
    ENDIF 


    *** Check if setup requred for the selected module
    IF SYDAPPL.lsetreq
      llSetDon = ATC(lcAppID,THIS.CompanySetupModules)>0 OR lcAppID='SM'  
      *** Check if the setup was done for the selected module 
      IF !llSetDon 
        *** Setup was not done to this module yet,"+CHR(13)
        *** you have to setup the module through the company information 
        *** program...!
        =THIS.MessageBox("INM00145B00000","ALERT")
        IF !EMPTY(lcCerFile) AND USED(lcCerFile)
          SELECT(lcCerFile)
        ENDIF
        SHOW MENU _MSYSMENU
        RETURN
      ENDIF     
   
      lcActivePlat = IIF(_DOS,'Dos',IIF(_WINDOWS,'Windows',IIF(_MAC,'MAC','UNIX')))
      lcErrCond    = 'SYDAPPL.C'+IIF(_DOS,'Dos',IIF(_WINDOWS,'Win',IIF(_MAC,'MAC','UNIX')))+'BUILD==SYDAPPL.CMDLBUILD'    
      IF ! &lcErrCond
        lcMessage=[module ]+lcAppID+[ have not ]+;
                  [been upgraded to the latest build number on the current ]+;
                  [platform ( ]+lcActivePlat+[ )!]+CHR(13)+CHR(10)+[ If you proceed and use this ]+;
                  [module, errors may occur and data may be lost. therefore ]+;
                  [you have to install files for thid module ]+;
                  [from the latest builds to be able to procced with this module!]
        *= gfMsgBox('TR',lcMessage,'\?\!\<Ok','ALERT')
        =MessageBox(lcMessage,16,_screen.caption)
        RETURN
      ENDIF
      lcMdlBldNum = ALLT(SUBSTR(sydappl.CMDLBUILD,1,ATC('-',sydappl.CMDLBUILD)-1))
      lcSysBldNum = ALLT(LOOKUP(SYDAPPL.CMDLBUILD,'SY',SYDAPPL.CAPP_ID,'CAPP_ID'))
      =SEEK(lcAppID,'SYDAPPL')    
      IF lcSysBldNum < lcMdlBldNum
        *=gfMsgBox("IN","The current build number of the System Manager ("+lcSysBldNum ;
        +") is too old to run ("+lcAppID+"), you need at least SM build # "+lcMdlBldNum+;
        " to run this build of ("+lcAppID+"). Please update SM and try again!",.f.,'ALERT')
        lcMessage = "The current build number of the System Manager ("+lcSysBldNum ;
        +") is too old to run ("+lcAppID+"), you need at least SM build # "+lcMdlBldNum+;
        " to run this build of ("+lcAppID+"). Please update SM and try again!"      
         =MessageBox(lcMessage,16,_screen.caption)
        SHOW MENU _MSYSMENU
        RETURN
      ENDIF
    ENDIF
    lcApplicationName = ALLTRIM(SYDAPPL.cApp_name)
    llParentModule  = SYDAPPL.lParntMdl
  *** Case the selected module was not installed to the system
  ELSE
    *** Module � is not installed to your system...!
    =THIS.MessageBox("INM00146B00000","ALERT",ALLTRIM(PROMPT()))
    IF !EMPTY(lcCerFile) AND USED(lcCerFile)
      SELECT(lcCerFile)
    ENDIF
    SHOW MENU _MSYSMENU
    RETURN
  ENDIF
*E000000,1 Hesham (Start)  
ENDIF
*E000000,1 Hesham (End)
IF THIS.ActiveModuleID = 'SY'
  THIS.SetMenu('SY','S')  
ENDIF

*** If this is first module to run intializ the rest of the system
*** menu but dont do it again
IF glInitMenu  
  glInitMenu = .F.
  llInitMenu = .T.
  =THIS.SetMenu('SY','S')
ENDIF

*** Creat the module menu
IF !THIS.SetMenu(lcAppID,'A')
  IF !EMPTY(lcCerFile) AND USED(lcCerFile)
    SELECT(lcCerFile)
  ENDIF
  RETURN 
ENDIF

*** Update the global module variable
THIS.ActiveModuleID = lcAppID
THIS.ActiveModuleName = lcApplicationName
THIS.ParentModule =  llParentModule
THIS.ActiveModuleBuildNumber = sydappl.CMDLBUILD

_SCREEN.CAPTION = ALLTRIM(THIS.SystemName)+" - "+ALLTRIM(THIS.ActiveModuleName)+;
           IIF(!EMPTY(THIS.ActiveCompanyID),' ('+ALLTRIM(THIS.ActiveCompanyID)+')','')

=THIS.SetHlpFl(lcAppID)


IF !EMPTY(lcCerFile) AND USED(lcCerFile)
  SELECT(lcCerFile)
ENDIF

ENDPROC
PROCEDURE login
gnUserLog = gfUserList(.T.)
lcReturn = this.DoFormRetVal("sylogin")
IF TYPE('lcReturn')='C'
  THIS.User_ID = lcReturn
  THIS.GetUserInfo()
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF

ENDPROC
PROCEDURE setmenu
LPARAMETERS lcModul,lcSub_Ctg,lcNoShow
*** Default module id to system
lcModul    = IIF(TYPE('lcModul'  )='C',lcModul   ,'SY')
*** Defualt menu level ti initial
lcSub_Ctg  = IIF(TYPE('lcSub_Ctg')='C',lcSub_Ctg ,'I')
*** In the file pad system barsare going to be defined staring from bar # 50
lcNoShow   = IIF(TYPE('lcNoShow') ='C' ,lcNoShow,'SHOW')
lcNoShow   = IIF(_WINDOWS,'SHOW',lcNoShow)

lnExsBar   = 51
llRetFlag  = .T.
PRIVATE llApplUsed
llApplUsed = .F.
***
IF !USED("SYDOBJCT")
  SELECT 0
  USE (THIS.SysPath+"SYDOBJCT")
ELSE
  SELECT SYDOBJCT
ENDIF

SET ORDER TO TAG CAPP_ID

IF THIS.LoginRequired
  IF !USED("SYUUSRPR")
    SELECT 0
    USE (THIS.SysPath+"SYUUSRPR")
  ELSE
    SELECT SYUUSRPR
  ENDIF

  SET ORDER TO TAG CUSER_ID
ENDIF

IF lcModul='SY'
  IF !USED('SYDAPPL')
    SELECT 0
    USE (THIS.SysPath+"SYDAPPL")
    llApplUsed = .T.
  ELSE
    SELECT SYDAPPL
  ENDIF
  SET ORDER TO TAG CAPP_ID
ELSE
  IF !USED('SYDAPPL')
    SELECT 0
    USE (THIS.SysPath+"SYDAPPL")
    llApplUsed = .T.
  ELSE
    SELECT SYDAPPL
  ENDIF
  SET ORDER TO TAG CAPP_ID
  llExit = .F.
  IF SEEK(lcModul,'SYDAPPL') AND SYDAPPL.CMODULEVER='F'
    llExit = .T.
  ENDIF
  IF llApplUsed
    USE IN SYDAPPL
  ENDIF
  IF llExit
    RETURN
  ENDIF
ENDIF

***
IF !USED('SYCMENU')
  SELECT 0
  USE (THIS.SysPath+"SYCMENU")
ELSE
  SELECT SYCMENU
ENDIF

SET ORDER TO TAG APPPOPBAR



lcExact = SET ("EXACT")
SET EXACT OFF
*E301141,1 Hesham (Start)
*E301141,1 if we are building a menu for a multi modules
IF ATC(',',lcModul)>0
  DIME laModSelected[1,1]
  laModSelected = ''
  =gfSubStr(lcModul,@laModSelected)
  IF !EMPTY(laModSelected[1,1])
    PRIVATE lnCount
    *** restore the fixed menu
    *** configuration befor building the module menu options
    *B124685,1 WLD If the companies more the 40 company, display all companies [Begin]    
    *IF lcModul <> 'SY'
      *POP  MENU _MSYSMENU
      *PUSH MENU _MSYSMENU
    *ENDIF
    *B124685,1 WLD If the companies more the 40 company,display all companies [End  ]
    *B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu [Begin]
    IF This.ReBldCmp
      THIS.ReBuildComp()
    ENDIF
    *B128962,1[End]
    
    
    lcModul = ''
    *Haythar [Begin]
    lcModuleCondition = '(SycMenu.cApp_ID = "SY"'
    *Haythar [End]
    FOR lnCount = 1 TO ALEN(laModSelected,1)
      THIS.SETMULTIAPPMENU(laModSelected[lnCount,1],lcSub_Ctg,lcNoShow)
      lcModul = lcModul+IIF(!EMPTY(lcModul),',','')+'"'+laModSelected[lnCount,1]+'"'
      *Haythar [Begin]
      lcModuleCondition = lcModuleCondition +;
        ' .OR. "' + laModSelected[lnCount,1] + '" $ cBarModule'

      *Haythar [End]
    ENDFOR

    *** Collect the process name and base window name for all menu options
    *Haythar [Begin]
    *SELECT RTRIM(cMstr_Nam)+"-"+cBar_pos,RTRIM(cPross_ID),cProcType,;
    *       RTRIM(cProcPath),RTRIM(SYDOBJCT.cbasewind),cSub_Msg,cHlpTopic,;
    *       CMENUPARAM,CBARMODULE;
    *  FROM SYCMENU, SYDOBJCT;
    *  WHERE SYDOBJCT.CAPOBJNAM = SYCMENU.CPROSS_ID;
    *   AND SYCMENU.CAPP_ID+SYCMENU.CPAD_POS+SYCMENU.CPOP_POS+SYCMENU.CPOP_LEVL+;
    *       SYCMENU.CBAR_POS IN (&lcModul,'SY');
    *   AND SYCMENU.CPROCTYPE IN ('C','P');
    *  UNION;
    *  SELECT RTRIM(cMstr_Nam)+"-"+cBar_pos,RTRIM(cPross_ID),cProcType,;
    *         RTRIM(cProcPath),SPACE(10),cSub_Msg,cHlpTopic,;
    *         CMENUPARAM,CBARMODULE;
    *   FROM  SYCMENU;
    *   WHERE SYCMENU.CAPP_ID+SYCMENU.CPAD_POS+SYCMENU.CPOP_POS+SYCMENU.CPOP_LEVL+;
    *         SYCMENU.CBAR_POS IN (&lcModul,'SY');
    *     AND SYCMENU.CPROCTYPE IN ('R','G','E','M');
    *         INTO ARRAY  THIS.aProcess

    lcModuleCondition = lcModuleCondition + ')'
    SELECT RTRIM(cMstr_Nam) + "-" + cBar_pos , RTRIM(cPross_ID) , cProcType ,;
      RTRIM(cProcPath) , RTRIM(SYDOBJCT.cbasewind) , cSub_Msg , cHlpTopic ,;
      CMENUPARAM , CBARMODULE;
      FROM SYCMENU , SYDOBJCT;
      WHERE SYDOBJCT.CAPOBJNAM = SYCMENU.cPross_ID;
      .AND. &lcModuleCondition;
      .AND. SYCMENU.cProcType IN ('C' , 'P');
      UNION;
      SELECT RTRIM(cMstr_Nam) + "-" + cBar_pos , RTRIM(cPross_ID) , cProcType ,;
      RTRIM(cProcPath) , SPACE(10) , cSub_Msg,cHlpTopic ,;
      CMENUPARAM , CBARMODULE;
      FROM SYCMENU;
      WHERE &lcModuleCondition;
      .AND. SYCMENU.cProcType IN ('R' , 'G' , 'E' , 'M');
      INTO ARRAY THIS.aProcess

    *Haythar [End]
  ENDIF

ELSE
  *E301141,1 Hesham (End)
  *** Chek if this module has any records in the menu file
  IF SEEK(lcModul)

    SET EXACT &lcExact

    *** If this is the first time to run this function,
    *** remove all the system menu and define it automatic
    IF lcModul = 'SY' .AND. lcSub_Ctg = 'I' .AND. lcNoShow = 'SHOW'
      SET SYSMENU TO
      SET SYSMENU  AUTOMATIC
    ENDIF

    *** In case bildin the menu for any module,  restore the fixed menu
    *** configuration befor building the module menu options
    *B124685,1 WLD If the companies more the 40 company, display all companies [Begin]    
    *IF lcModul <> 'SY'
    *  POP  MENU _MSYSMENU
    *  PUSH MENU _MSYSMENU
    *ENDIF
    *B124685,1 WLD If the companies more the 40 company, display all companies [End ]    

    *** Check if building a module menu and the current user has
    *** right limits, add a relation with the user priv. file
    *** and disable the unpermited programs.
    IF THIS.LoginRequired .AND. lcModul <> "SY"
      SELECT SYUUSRPR
      SET ORDER TO TAG MDLCMPPRSS
      SET FILTER TO (CUSER_ID=THIS.User_ID  .AND. cgrporuser='U') .OR.;
        (CUSER_ID=THIS.User_Group .AND. cgrporuser='G')

      SELECT SYCMENU
      SET RELATION TO lcModul+THIS.ActiveCompanyID+cPross_ID+cProcType ;
        INTO SYUUSRPR ADDITIVE
    ENDIF

    SET RELATION TO CAPP_ID+cPross_ID INTO SYDOBJCT ADDITIVE
    lnBarsdefiend = 0
    *** Loop for all records in the menu file for this moudle
    SCAN REST ;
        WHILE CAPP_ID  = lcModul ;
        FOR   cSub_Ctg = lcSub_Ctg

      *** If the bar or pad to be defined have no hot key defined in
      *** the menu file it will give an error if we type KEY null
      lcHotKey  = IIF(EMPTY(cSub_hKey),[],[KEY ]+ALLTRIM(cSub_hKey))

      DO CASE
        *** Check if the type of this line is  pad
      CASE  ALLTRIM(cSub_Typ) = 'P'
        *** Check if this new pad has not been defined before
        IF TYPE([PRMPAD('_MSYSMENU',ALLTRIM(cMstr_Nam))]) = 'U'
          *** Get the name of the privious pad to define the
          *** new pad in the right position
          lcPrivpad = "P"+PADL(INT(VAL(SUBSTR(cMstr_Nam,2))-1),2,'0')

          *** Define new meny pad
          DEFINE PAD    (ALLTRIM(cMstr_Nam)) ;
            OF     _MSYSMENU ;
            PROMPT ALLTRIM(cSub_prpt);
            AFTER &lcPrivpad &lcHotKey

          *** Define new menu popup that going to be opend from this pad
          DEFINE POPUP (ALLTRIM(cPross_ID)) MARGIN SHADOW RELATIVE

          *** link the pad to the popup
          ON PAD (ALLTRIM(cMstr_Nam)) ;
            OF _MSYSMENU ;
            ACTIVATE POPUP (ALLTRIM(cPross_ID))
        ENDIF

        *** Define all menu bars
      OTHERWISE
        *** Get the popup and bar name to be send to the gfBarSkip function
        *** to get the right help message fro array
        lcPopBar = ALLTRIM(cMstr_Nam)+"-"+cBar_pos
        *** To add the bars of the file pad befor the system bars
        *** we have to define the popups relativ and defien all the
        *** menu bars befor bar # 51 of the file pad
        *** But in case of system bars no need for the BEROFE cluase
        IF lcModul = "SY"

          *** Define fox system bars
          IF cProcType = 'S'
            *** Fox system bars are defined without bar no
            *** so we have to calculat the relative positon
            *** to define the bar after
            lnAftrBar = IIF(VAL(cBar_pos)-1 > 0,VAL(cBar_pos)-1,1)

            DEFINE BAR EVAL(cPross_ID) OF (ALLTRIM(cMstr_Nam));
              PROMPT ALLTRIM(cSub_prpt) AFTER lnAftrBar;
              COLOR SCHEME 3 ;
              &lcHotKey
            *** Define Aria system bars
          ELSE
            *** Only system bars can have skip expretion comming from the
            *** Modules objects file
            IF UPPER(ALLTRIM(cPross_ID)) = "GFDOHELP"
              gcHlpPop = ALLTRIM(cMstr_Nam)
              gcHlpPrm = ALLTRIM(cSub_prpt)
              gnHlpBar = VAL(cBar_pos)
            ENDIF

            lcSkipExp = IIF(EMPTY(SYDOBJCT.mskipexpr),[],[SKIP FOR ]+;
              ALLTRIM(SYDOBJCT.mskipexpr))
            IF EMPTY(cPross_ID) AND cMstr_Nam = 'P07PU07' AND lnBarsdefiend = 0
              LOOP
            ENDIF
            IF cProcType = 'M' AND SEEK(ALLT(cPross_ID),'SYDAPPL') AND SYDAPPL.CMODULEVER='F'
              LOOP
            ENDIF
            IF cProcType = 'M'
              lnBarsdefiend = lnBarsdefiend + 1
            ENDIF
            DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
              PROMPT ALLTRIM(cSub_prpt);
              &lcSkipExp &lcHotKey;
              COLOR SCHEME 3
            IF !EMPTY(SYDOBJCT.mMarkExpr)
              SET MARK OF BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
                EVAL(ALLTRIM(SYDOBJCT.mMarkExpr))
            ENDIF
          ENDIF

          *** Define other modules bars
        ELSE
          *** If the current user have lemeted rights, disable
          *** the unpermited programs
          IF THIS.LoginRequired
            *** Check if the current user have access to this bar in
            *** the user prevelage file or not
            *** CHECK FOR MODULES HERE
            IF cSub_Typ <> 'S' .AND. EOF('SYUUSRPR')

              DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
                PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar;
                COLOR SCHEME 3 SKIP FOR .T. &lcHotKey
            ELSE

              DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
                PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar;
                COLOR SCHEME 3 ;
                &lcHotKey
            ENDIF
          ELSE
            *** CHECK FOR MODULES HERE
            DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
              PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar;
              COLOR SCHEME 3 ;
              &lcHotKey
          ENDIF
        ENDIF

        *** Check if this bar is a branching bar and define new
        *** popup if it is
        *** CHECK FOR MODULES HERE
        IF ALLTRIM(cSub_Typ) = 'S'
          DEFINE POPUP (ALLTRIM(cPross_ID)) ;
            MARGIN SHADOW RELATIVE

          ON BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) ;
            ACTIVATE POPUP (ALLTRIM(cPross_ID))
        ENDIF
      ENDCASE
    ENDSCAN

    *** Now we are going to link all the menu bars to the global procedure
    *** gpMneuBar to excute the selected option

    ON SELECTION POPUP ALL DO gfMenuBar WITH POPUP(),BAR()


    *** In case of creating the second level of the menu we are
    *** going to save the configuration to be restored when changing
    *** the module and ther is no need to collect the prosses names
    IF lcModul = 'SY' .AND. lcSub_Ctg = 'S'
      PUSH MENU _MSYSMENU
      THIS.WinSepta = CNTBAR('P08PU08')
    ELSE
      STORE .F. TO glRestore,glCmpCreat
      IF !glRestore
        IF (lcModul = 'SY' .AND. lcSub_Ctg = 'I') .OR. glCmpCreat

          *** Now we are going to collect the companies names and link it's
          *** bars to the change company procedure
          IF !USED("SYCCOMP")
            SELECT 0
            USE(THIS.SysPath+"SYCCOMP")
          ELSE
            SELECT SYCCOMP
          ENDIF

          SET ORDER TO TAG CCOMP_ID

          IF !glCmpCreat
            *            THIS.ActiveCompanyID = IIF(!EMPTY(THIS.UserCompanyID),THIS.UserCompanyID,THIS.DefaultCompanyID)
            THIS.ActiveCompanyID = IIF(!EMPTY(THIS.UserCompanyID),THIS.UserCompanyID,THIS.ActiveCompanyID)
          ELSE
            SET MARK OF POPUP _COMPANIES TO .F.
            RELEASE BAR  ALL OF _COMPANIES
          ENDIF

          lnCompBar  = 0
          *** Collect all avalilabe companies to the compamies popup
          SCAN
            lnCompBar = lnCompBar + 1

            IF THIS.LoginRequired
              SELECT SYUUSRPR
              LOCATE FOR INLIST(CUSER_ID,THIS.User_ID,THIS.User_Group) .AND.;
                CCOMP_ID = SYCCOMP.CCOMP_ID

              SELECT SYCCOMP

              IF FOUND('SYUUSRPR')
                DEFINE BAR lnCompBar OF _COMPANIES PROMPT CCOMP_ID+"-"+ALLTRIM(cCom_Name)
              ELSE
                DEFINE BAR lnCompBar OF _COMPANIES PROMPT CCOMP_ID+"-"+ALLTRIM(cCom_Name) ;
                  SKIP FOR .T.
              ENDIF
            ELSE
              DEFINE BAR lnCompBar OF _COMPANIES PROMPT CCOMP_ID+"-"+ALLTRIM(cCom_Name)
            ENDIF


            IF THIS.ActiveCompanyID = CCOMP_ID
              SET MARK OF BAR lnCompBar OF _COMPANIES TO .T.
              THIS.CompanyBar = lnCompBar
              THIS.ActiveCompanyName = ALLTRIM(SYCCOMP.cCom_Name)
              *B603122,3  Hesham (Start)
              *B603122,3  make the system use a remote data dir
              *!*	              THIS.DataDir  = ALLTRIM(cCom_dDir)
              * Change the data path to the currently mapped drive.
              *!*	              IF UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) = UPPER(SUBSTR(THIS.DataDir,1,ATC('\',THIS.DataDir,2))) AND ;
              *!*	                 UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) <>  UPPER(SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2)))
              *!*	                THIS.DataDir= SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2))+SUBSTR(THIS.DataDir,ATC('\',THIS.DataDir,2)+1)
              *!*	              ENDIF
              THIS.DataDir  = THIS.GetDataDir(ALLTRIM(cCom_dDir))
              *B603122,3  Hesham (End)
              *            THIS.CompanyDefaultModule = ALLTRIM(mComp_Mdl)
              *            THIS.CompanyLevel = IIF(EMPTY(ccompprnt),'P','C')
              THIS.PrntCompanyID = IIF(EMPTY(ccompprnt),CCOMP_ID,ccompprnt)
              THIS.BaseCurrency = IIF(EMPTY(THIS.BaseCurrency),'USDLR',THIS.BaseCurrency)
              THIS.CompanySets(ALLTRIM(SYCCOMP.ccont_code))
            ENDIF
            lcComp_Id = '"'+CCOMP_ID+'"'
            ON SELECTION BAR lnCompBar OF _COMPANIES ;
              DO gfChngComp WITH &lcComp_Id

          ENDSCAN

          *** Open the module popup with the initial menu
          IF lcNoShow = 'SHOW'
            KEYBOARD "{ALT+M}" PLAIN CLEAR
          ENDIF
        ELSE

          *** Reselect the currend company
          SET MARK OF POPUP _COMPANIES TO .F.
          IF THIS.CompanyBar > 0
            *** there is posible error in case of deleting the current company
            SET MARK OF BAR THIS.CompanyBar OF _COMPANIES TO .T.
          ENDIF
        ENDIF
      ENDIF


      *** Collect the process name and base window name for all menu options
      SELECT RTRIM(cMstr_Nam)+"-"+cBar_pos,RTRIM(cPross_ID),cProcType,;
        RTRIM(cProcPath),RTRIM(SYDOBJCT.cbasewind),cSub_Msg,cHlpTopic,;
        CMENUPARAM,CBARMODULE;
        FROM SYCMENU, SYDOBJCT;
        WHERE SYDOBJCT.CAPOBJNAM = SYCMENU.cPross_ID;
        AND SYCMENU.CAPP_ID+SYCMENU.CPAD_POS+SYCMENU.CPOP_POS+SYCMENU.CPOP_LEVL+;
        SYCMENU.cBar_pos IN (lcModul,'SY');
        AND SYCMENU.cProcType IN ('C','P');
        UNION;
        SELECT RTRIM(cMstr_Nam)+"-"+cBar_pos,RTRIM(cPross_ID),cProcType,;
        RTRIM(cProcPath),SPACE(10),cSub_Msg,cHlpTopic,;
        CMENUPARAM,CBARMODULE;
        FROM  SYCMENU;
        WHERE SYCMENU.CAPP_ID+SYCMENU.CPAD_POS+SYCMENU.CPOP_POS+SYCMENU.CPOP_LEVL+;
        SYCMENU.cBar_pos IN (lcModul,'SY');
        AND SYCMENU.cProcType IN ('R','G','E','M');
        INTO ARRAY  THIS.aProcess
    ENDIF

    SELECT SYCMENU
    SET RELATION TO

  ELSE
    SET EXACT &lcExact
    *** There is no menu bares define for this module, go to the menu
    *** program and define new bars for this module.
    =THIS.MESSAGEBOX("INM00150B00000","DIALOG")
    llRetFlag = .F.
  ENDIF
ENDIF
*IF .F.
DO WHILE EMPTY(PRMBAR('P07PU07',GETBAR('P07PU07',1)))
  RELEASE BAR GETBAR('P07PU07',1) OF P07PU07
ENDDO
DO WHILE EMPTY(PRMBAR('P07PU07',GETBAR('P07PU07',CNTBAR('P07PU07'))))
  RELEASE BAR GETBAR('P07PU07',CNTBAR('P07PU07')) OF P07PU07
ENDDO
*ENDIF

IF llApplUsed
  USE IN SYDAPPL
ENDIF
RETURN llRetFlag

ENDPROC
PROCEDURE setenvironment
DIMENSION THIS.aSets[53,3]
=THIS.GETSET(1,'ANSI')
=THIS.GETSET(2,'AUTOSAVE')
=THIS.GETSET(3,'BELL')
=THIS.GETSET(4,'BLOCKSIZE TO','V')
=THIS.GETSET(5,'BRSTATUS')
=THIS.GETSET(6,'CARRY ')
=THIS.GETSET(7,'CONFIRM')
=THIS.GETSET(8,'CLOCK')
=THIS.GETSET(9,'STATUS BAR')
=THIS.GETSET(10,'CONSOLE')
=THIS.GETSET(11,'CURSOR')
=THIS.GETSET(12,'DECIMALS TO','V')
=THIS.GETSET(13,'DELETED')
=THIS.GETSET(14,'DEBUG')
=THIS.GETSET(15,'DEVICE TO')
=THIS.GETSET(16,'ESCAPE')
=THIS.GETSET(17,'EXACT')
=THIS.GETSET(18,'EXCLUSIVE')
=THIS.GETSET(19,'FIELDS')
=THIS.GETSET(20,'FIXED')
=THIS.GETSET(21,'FULLPATH')
=THIS.GETSET(22,'HEADINGS')
=THIS.GETSET(23,'HELP')
=THIS.GETSET(24,'LIBRARY TO')
=THIS.GETSET(25,'LOCK')
=THIS.GETSET(26,'MARGIN TO','V')
=THIS.GETSET(27,'MARK TO','V')
=THIS.GETSET(28,'MEMOWIDTH TO','V')
=THIS.GETSET(29,'MENU')
=THIS.GETSET(30,'MESSAGE TO','V')
=THIS.GETSET(31,'MESSAGE TO',1,'V')
=THIS.GETSET(32,'MULTILOCKS')
=THIS.GETSET(33,'NEAR')
=THIS.GETSET(34,'ODOMETER TO','V')
=THIS.GETSET(35,'OPTIMIZE')
=THIS.GETSET(36,'PATH TO')
=THIS.GETSET(37,'POINT TO','V')
=THIS.GETSET(38,'PRINTER')
=THIS.GETSET(39,'PRINTER TO',1)
=THIS.GETSET(40,'PROCEDURE TO','V')
=THIS.GETSET(41,'REPROCESS TO','V')
=THIS.GETSET(42,'SAFETY')
=THIS.GETSET(43,'SCOREBOARD')
=THIS.GETSET(44,'SPACE')
=THIS.GETSET(45,'STEP')
=THIS.GETSET(46,'SYSMENU')
=THIS.GETSET(47,'TALK')
=THIS.GETSET(48,'TEXTMERGE')
=THIS.GETSET(49,'UNIQUE')
=THIS.GETSET(50,'RESO TO',1,'V')
=THIS.GETSET(51,'RESO')
=THIS.GETSET(52,'CLASSLIB TO')
=THIS.GETSET(53,'FULLPATH')
THIS.ShutDown = ON('SHUT')
ON SHUTDOWN DO GPEXIT

SET ANSI OFF
SET AUTOSAVE OFF
SET BELL OFF
SET BLOCKSIZE TO 33
SET BRSTATUS OFF
SET CARRY OFF
SET CONFIRM ON
SET CLOCK STATUS
SET STATUS BAR ON
SET CONSOLE ON
SET CURSOR ON
SET DECIMALS TO 2
SET DELETED ON
SET DEBUG ON
SET DEVICE TO SCREEN
SET ESCAPE OFF
SET EXACT OFF
SET EXCLUSIVE OFF
SET FIELDS OFF
SET FIXED OFF
SET FULLPATH OFF
SET HEADING ON
SET HELP ON
SET LIBRARY TO
SET LOCK OFF
SET MARGIN TO 0
SET MARK TO "/"
SET MEMOWIDTH TO 65
SET MENU ON
SET MESSAGE TO SROWS()-1 LEFT
SET MESSAG TO "ARIA Advantage Series"
SET MULTILOCKS ON
SET NEAR OFF
SET ODOMETER TO 100
SET OPTIMIZE ON
SET PATH TO
SET POINT TO "."
SET PRINTER OFF
SET PRINTER TO Lpt1
*SET PROCEDURE TO ARIAPROC
SET REPROCESS TO 10
SET SAFETY OFF
SET SCOREBOARD OFF
SET SPACE OFF
SET STEP OFF
SET SYSMENU AUTOMATIC
SET TALK OFF
SET TEXTMERGE OFF
SET UNIQUE OFF
*lcClasslib = '\CLASSES\MAIN'
*SET CLASSLIB TO &lcClassLib &&,CODES,UTILITY
SET FULLPATH ON
ENDPROC
PROCEDURE restoreenvironment
llError = .F.
ON ERROR llError = .T.
FOR lnCount = 1 TO ALEN(THIS.aSets,1)
  IF TYPE('THIS.aSets[lnCount,3]')= 'C' AND THIS.aSets[lnCount,3]='V'
    lcCommand = 'SET '+THIS.aSets[lnCount,1]+' THIS.aSets[lnCount,2]'
  ELSE
    lcCommand = 'SET '+THIS.aSets[lnCount,1]+ ' '+THIS.aSets[lnCount,2]    
  ENDIF
  &lcCommand
ENDFOR
ON ERROR
lcShut = THIS.ShutDown
ON SHUTDOWN &lcShut
ENDPROC
PROCEDURE cleanup
CLOSE DATA
THIS.RestoreEnvironment()
SET SYSMENU TO DEFA
*-- Save current main window title and set
*-- the new one
_screen.Caption = this.OldWindCaption
IF !EMPTY(THIS.HelpFile) AND FILE(THIS.HelpFile)
  SET HELP TO (THIS.HelpFile)
ENDIF
*-- Release all toolbars
this.RestoreToolBars()

ENDPROC
PROCEDURE messagebox
PARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg

PRIVATE lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcCurrDbf,lnButtons,lcDlgMessg

DECLARE laButtNo [1]         && Array to hold the buttons separetly
DECLARE laVarsStr[1]         && Array to hold variabels to be replaced
                             && in the messag

PUSH KEY
ON KEY
lcCurrDbf   = ALIAS()
lcDlgID     = IIF(TYPE('lcDlgID'   ) $ "UL",'',lcDlgID   )
lcDlgTyp    = IIF(TYPE('lcDlgTyp'  ) $ "UL",'D',lcDlgTyp )
lcVarsStr   = IIF(TYPE('lcVarsStr' ) $ "UL",'',lcVarsStr )
lcDlgValid  = IIF(TYPE('lcDlgValid') $ "UL",'',lcDlgValid)
lcDlgMessg  = IIF(TYPE('lcDlgMessg') $ "UL",'',lcDlgMessg)
lcMsgCatgry = SUBSTR(lcDlgID,1,2)

lnColrSchm  = VAL(SUBSTR('57',AT(LEFT(lcDlgTyp,1),'DA'),1))  
lnDlgOpTion = 1              && Variable to get the result in
lnButtSize  = 1
lnOldRec    = 0
lnTextStrt  = IIF(EMPTY(lcDlgMessg),7,2)
lnRigtShft  = IIF(EMPTY(lcDlgMessg),10,5)

IF EMPTY(lcDlgMessg)
  *** Open dialog file if it is not opend
  IF !USED('SydDlObj')
    SELECT 0
    USE (THIS.SysPath+"SydDlObj") 
  ENDIF

  SET ORDER TO TAG CDLOBJID IN ALIAS('SydDlObj')

  lnOldRec = RECNO('SydDlObj')

  *** Get dialog button
  IF SEEK(SUBSTR(lcDlgID,9),'SydDlObj')
    lcButton = ALLTRIM(SydDlObj.mDlObj)
  ELSE
    *** If not found set default button to <ok>
    lcButton = "\!\?\<Ok"
  ENDIF

  *** Get Dialog messag
  IF SEEK(SUBSTR(lcDlgID,3,6),'SydDlObj')
    lcMessag    = ALLTRIM(SydDlObj.mDlObj)
    lcMsgCatgry = IIF(EMPTY(SydDlObj.cMsgCatgry),lcMsgCatgry,SydDlObj.cMsgCatgry)
  ELSE
    *** If not found set default message to null
    lcMessag = ""
  ENDIF


  *** Collect variables to be replaced from string to array
  =gfSubStr(lcVarsStr,@laVarsStr,'|')

  *** Replace each � mark with variabe sent
  FOR lnVarsStr = 1  TO ALEN(laVarsStr,1)
    lcMessag = STRTRAN(lcMessag,'�',laVarsStr[lnVarsStr],1,1)
  ENDFOR  

  *** Remove all controle characters " \ ! < ?" from the button string
  lcButtChk  = STRTRAN(STRTRAN(STRTRAN(STRTRAN(lcButton,'\',''),'!',''),;
             '<',''),'?','')

  *** Collect all Buttons in array 
  =gfSubStr(lcButtChk,@laButtNo,';')

  *** Get No of buttons from array len
  lnButtNo   = ALEN(laButtNo,1)

  *** Get the max width of button
  FOR lnButtons = 1 TO lnButtNo
    lnButtSize = MAX(lnButtSize,LEN(laButtNo[lnButtons]))
  ENDFOR

ELSE
  lcMessag   = ALLTRIM (lcDlgMessg)
  lcButton   = "\!\?\<Ok"  
  lnButtNo   = 1
  lnButtSize = 4
ENDIF


*** Buttons width should not be less than 10 after adding 4 (< >) 
lnButtSize = MAX(10,lnButtSize+ 4 )


*** Calculat window width accordin to total buttons size
lnWinWidth = MAX(50,(lnButtSize * lnButtNo)+6)

*** Calculat space between buttons 
lnBetwButt = INT((lnWinWidth -(lnButtSize * lnButtNo))/lnButtNo)

*** Calculat X position of the button to be centerd
lnButXPos  = (lnWinWidth/2)-((lnButtSize * lnButtNo)+;
             (lnBetwButt*(lnButtNo-1)))/2

*** Calculat window higth according to the length of the messag
lnWinHight = INT(LEN(ALLTRIM(lcMessag))/lnWinWidth) + 6 


*** Define dialog window with right width, hight and color
   
    DEFINE WINDOW gwdDialog ;
	       AT  0.000, 0.000  ;
		   SIZE lnWinHight,lnWinWidth;
		   FONT "System", 10 ;
		   STYLE "B" ;
		   FLOAT ;
           SYSTEM ;
           TITLE (THIS.SystemName);
		   NOMINIMIZE ;
		   COLOR RGB(,,,192,192,192)

			  
*** Activate dialog window no show so the window will pop with messag 
*** when activated
ACTIVATE WINDOW gwdDialog NOSHOW

*** Move the dialog window to center of the screen
MOVE WINDOW gwdDialog CENTER

      @ 0.000,0.000 TO 0.000,lnWinWidth ;
		PEN 1, 8 ;
		STYLE "1" ;
		  COLOR RGB(255,255,255,255,255,255)
  	  @ 0.000,0.000 TO lnWinHight,0.000 ;
		PEN 1, 8 ;
		  COLOR RGB(255,255,255,255,255,255)
  	  @ 0.188,0.429 TO 0.188,lnWinWidth-.6 ;
		PEN 1, 8 ;
		STYLE "1" ;
		  COLOR RGB(128,128,128,128,128,128)
	  @ 0.188,0.429 TO lnWinHight-.3,0.429 ;
		PEN 1, 8 ;
		  COLOR RGB(128,128,128,128,128,128)
	  @ 0.000,lnWinWidth TO lnWinHight,lnWinWidth ;
		PEN 1, 8 ;
		  COLOR RGB(128,128,128,128,128,128)
	  @ 0.188,lnWinWidth-.6 TO lnWinHight-.3,lnWinWidth-.6 ;
		PEN 1, 8 ;
		  COLOR RGB(255,255,255,255,255,255)
	  @ lnWinHight,0.143 TO lnWinHight,lnWinWidth ;
		PEN 1, 8 ;
		STYLE "1" ;
		  COLOR RGB(128,128,128,128,128,128)
	  @ lnWinHight-.3,0.4 TO lnWinHight-.3,lnWinWidth-.4 ;
		PEN 1, 8 ;
		STYLE "1" ;
	      COLOR RGB(255,255,255,255,255,255)
    
      IF EMPTY(lcDlgMessg)
        lcIcon = ALLT(SUBSTR ("TR  QRY INFO",;
             ((AT(lcMsgCatgry,"TRQRIN")+1)/2-1)*4+1,4))+".BMP" 
        @ 1.5,1.5 SAY lcIcon BITMAP STYLE "T"
      ENDIF  

      @ 1.5,lnTextStrt SAY lcMessag SIZE lnWinHight-3,lnWinWidth-lnRigtShft


  
IF EMPTY(lcDlgValid )
  @ lnWinHight -2 ,lnButXPos ;
    GET lnDlgOpTion ;
    PICTURE "@*HT "+lcButton;
    SIZE 1.5,lnButtSize,lnBetwButt
ELSE
  @ lnWinHight -2 ,lnButXPos ;
    GET lnDlgOpTion ;
    PICTURE "@*H "+lcButton;
    SIZE 1.5,lnButtSize,lnBetwButt;
    VALID &lcDlgValid
ENDIF    

  
*** When the read is activated fox will activate the window 
ACTIVATE WINDOW gwdDialog

IF !EMPTY(lcDlgMessg)
  SET BELL TO 2000,2
  ?? CHR(7)
  SET BELL TO 
ENDIF  

READ CYCLE MODAL

IF lnOldRec > 0  .AND. lnOldRec <= RECCOUNT('SydDlObj')
  GO lnOldRec IN ALIAS('SydDlObj')
ENDIF

IF !EMPTY(lcCurrDbf)
  SELECT (lcCurrDbf)
ENDIF

RELEASE WINDOW gwdDialog

POP KEY
RETURN lnDlgOpTion



ENDPROC
PROCEDURE restoretoolbars
FOR i = 1 TO ALEN(THIS.aToolBars, 1)
  IF THIS.aToolBars[i,2]
    SHOW WINDOW (THIS.aToolBars[i,1])
  ENDIF
ENDFOR

ENDPROC
PROCEDURE systemsetup
*** Open installation 
SET EXCL OFF

IF !USED('syuuser')
  SELECT 0
  *** Open the
  USE (THIS.SysPath+"syuuser")
ELSE
  SELECT syuuser
ENDIF  
LOCATE
THIS.LoginRequired  = FOUND()                && Login required flag
IF USED('SYUUSER')
  USE IN SYUUSER
ENDIF  

IF !USED('SYCINST')
  SELECT 0
  *** Open the
  USE (THIS.SysPath+"sycinst")
ELSE
  SELECT SYCINST
ENDIF  

*** Load intial data from install data file
*THIS.LoginRequired  = sycinst.lInsLogRq                && Login required flag
*THIS.DefaultCompanyID  = sycinst.cInsDfCom                && Default company   
*THIS.ActiveCompanyID  = THIS.DefaultCompanyID

IF !EMPTY(sycinst.cInsDfCom)
  THIS.ActiveCompanyID  = sycinst.cInsDfCom               && Default company   
  THIS.GETCOMPANYINFORMATION(THIS.ActiveCompanyID)
ENDIF
*THIS.CHANGECOMPANY(sycinst.cInsDfCom)
THIS.ResourceHome  = ALLTRIM(sycinst.cInsRsrDr)
THIS.InstallPath   = ALLTRIM(sycinst.cInsysFDr)
THIS.ScreenHome = ALLT(sycinst.cScrDir)
*THIS.WORKDIR = ALLT(sycinst.cINSWINWD)

THIS.MapDrive = !(UPPER(ALLTRIM(sycinst.cInsysFDr)) = UPPER(THIS.SysPath))
THIS.MapDrive = !(UPPER(LEFT(ALLTRIM(sycinst.cInsysFDr),1)) = UPPER(LEFT(THIS.SysPath,1)))
THIS.DefaultCountry = IIF(EMPTY(SYCINST.CCONT_CODE),'USA   ',SYCINST.CCONT_CODE)

IF FILE (THIS.SysPath+"SYHHELP.HLP")
  SET HELP TO (THIS.SysPath+"SYHHELP.HLP")
  SET TOPIC TO THIS.HelpTopic
ENDIF
THIS.CurrentSite = sycinst.CCURSITEID
THIS.WorkDir = ALLTRIM(sycinst.cInsWinWD)
THIS.ApplicationHome = ALLTRIM(sycinst.cInsWinPD)
THIS.ReportHome = ALLTRIM(sycinst.cInswinRD)
THIS.BitMapHome = ALLTRIM(sycinst.cInswinBM)
This.ClassDir   = ALLTRIM(SYCINST.cClassDir)
This.EDIPath    = ALLTRIM(SYCINST.cEDIPath)

IF THIS.MapDrive
  THIS.DefaultPath = SUBSTR(THIS.SysPath,1,RAT("\",THIS.SysPath,2))
  THIS.WorkDir =  THIS.DefaultPath + SUBSTR(THIS.WorkDir,RAT("\",THIS.WorkDir,2)+1)
  THIS.ApplicationHome =  THIS.DefaultPath + SUBSTR(THIS.ApplicationHome,RAT("\",THIS.ApplicationHome,2)+1)
  THIS.ReportHome =  THIS.DefaultPath + SUBSTR(THIS.ReportHome,RAT("\",THIS.ReportHome,2)+1)
  THIS.BitMapHome =  THIS.DefaultPath + SUBSTR(THIS.BitMapHome,RAT("\",THIS.BitMapHome,2)+1)
ENDIF 
IF FILE(THIS.BitMapHome+ALLT(sycinst.cdef_bmp))
  IF TYPE('_SCREEN.GROUNDBMP')#'O'
    _SCREEN.ADDOBJECT('GroundBmp','image')
  ENDIF
  _screen.GroundBmp.picture = LOWER(THIS.BitMapHome+ALLT(sycinst.cdef_bmp))
  _screen.GroundBmp.top = (_screen.Height - _screen.GroundBmp.height)/2
  _screen.GroundBmp.left = (_screen.width - _screen.GroundBmp.width)/2
  _screen.GroundBmp.visible = .t.
ENDIF
SET RESOURCE OFF
lcResource = gfTempName()
CREATE TABLE (THIS.WorkDir+lcResource+'.DBF') FREE (type c(12),id c(12),name M,readonly L,ckval N(6,0),data M,updated D) 
*SELECT * FROM  (THIS.SysPath+"SYCRESRC") INTO DBF (THIS.WorkDir+lcResource)
IF USED (lcResource)
  USE IN ALIAS(lcResource)
ENDIF
SET RESOURCE TO (THIS.WorkDir+lcResource)
*USE IN SYCRESRC

*** If in the developing mode redirect apphome and rephome to local settings
THIS.ApplicationHome   = IIF(EMPTY(GETENV('APPHOME')),THIS.ApplicationHome,GETENV('APPHOME'))
THIS.ReportHome   = IIF(EMPTY(GETENV('REPHOME')),THIS.ReportHome,GETENV('REPHOME'))

SELECT 0
USE (THIS.SysPath+"syclogo")
THIS.SystemName = cSysName  
IF !EMPTY(THIS.ActiveCompanyID)
  THIS.ChangeCompany(THIS.ActiveCompanyID)
ENDIF
USE IN SYCLOGO
ENDPROC
PROCEDURE companysets
PARAMETERS lcCont_ID
lcSavAlias = ALIAS()
llUsd_INT = .T.  && Variable to hold the status of the SYCINT file (Open or not)
IF !USED("SYCINT")
  llUsd_INT = .F.
  SELECT 0
  USE (THIS.SysPath+"SYCINT")
ELSE
  SELECT SYCINT
ENDIF
SET ORDER TO TAG CCONTCODE 

IF SEEK(lcCont_ID)
  lccurrency  = ALLTRIM(ccurrency)
  lccurrenci = ALLTRIM(ccurrencyi)
  lcdate_type = ALLTRIM(cdate_type)
  lccentury   = ALLTRIM(ccentury)
  lcseparator = ALLTRIM(cseparator)

  THIS.PhoneMask  = IIF(!EMPTY(cPhoneTemp),ALLTRIM(cPhoneTemp),THIS.PhoneMask)
*  THIS.PhoneSize = LEN(ALLTRIM(THIS.PhoneMask))

  SET CENTURY  &lccentury
  SET CURRENCY TO lccurrenci
  SET CURRENCY &lccurrency

  SET DATE &lcdate_type
  SET SEPARATOR TO lcseparator
*  THIS.DateWidth = IIF('ON'$SET('CENT'),10,8)
ENDIF


IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF
*B600422,1 Colse the file if itwas opened
IF !llUsd_INT
  USE IN SYCINT
ENDIF

ENDPROC
PROCEDURE getset
LPARAMETERS lnArrNumber,lcSetProc,lnSetArg,lcSecPar
IF TYPE('lnSetArg')='C'
  lcSecPar = lnSetArg
ENDIF
lnSetArg = IIF(TYPE('lnSetArg')<>'N',0,lnSetArg)
THIS.aSets[lnArrNumber,1] = lcSetProc
THIS.aSets[lnArrNumber,3] = lcSecPar
IF lnSetArg = 0
  THIS.aSets[lnArrNumber,2] = SET(lcSetProc)
ELSE
  THIS.aSets[lnArrNumber,2] = SET(lcSetProc,lnSetArg)
ENDIF  

ENDPROC
PROCEDURE sethlpfl
PARAMETE lcApp_ID
lcHelpFile = "SYH"+ALLTRIM(lcApp_ID)+".HLP"
*** Activate the dos on line help 
IF FILE (THIS.SysPath+lcHelpFile)
  SET HELP TO (THIS.SysPath+lcHelpFile)
ELSE  
  IF FILE (THIS.SysPath+"SYHHELP.HLP")
    SET HELP TO (THIS.SysPath+"SYHHELP")
  ENDIF 
ENDIF

ENDPROC
PROCEDURE programsetup
*E301141,1 Hesham (Start)                      
*E301141,1 add new paramter
lparameters lcProgramModule
*E301141,1 Hesham (End)                      
*** If Opening this program for first time ether from the menu
*** or called from another program
*gcIniName = STUFF(gcBaseWind,2,1,"W")
gcIniName = SUBSTR(gcBaseWind,2)
lcBaseWind = gcBaseWind

*** Add the program copy number starting from the secound one
*IF gnProgCopy > 1
*  lcWindTitl = lcWindTitl+" /"+STR(gnProgCopy,2)
*ENDIF  

  
*** Check if the program is multiuser or single user
*** and load programs name not workable with in array

SET ORDER TO TAG CAPP_ID IN SYDOBJCT
*IF SEEK(lcProgramModule+ALLTRIM(SUBSTR(gcBaseWind,2)),"SYDOBJCT")
*E301141,1 Hesham (Start)                      
*IF SEEK(This.ActiveModuleID+ALLTRIM(SUBSTR(gcBaseWind,5)),"SYDOBJCT")
IF SEEK(lcProgramModule+ALLTRIM(SUBSTR(gcBaseWind,5)),"SYDOBJCT")
*E301141,1 Hesham (End)                      
  DECLARE laPrgNames [1]
  llSusrPrg  = SYDOBJCT.lSingUsr
  THIS.MultiRun = SYDOBJCT.lMultinst 
  laPrgNames = ' '
  = gfSubStr(SYDOBJCT.mPrgNames,@laPrgNames,'|')
   *** Check if the program is Single user
  IF llSusrPrg
    IF !THIS.SINGLEUSERPROGRAM(ALLTRIM(SUBSTR(gcBaseWind,5)))
      glNoLog    = .T.
      glFirsTime = .F.
      glQuitting = .T.
      RETURN .F.
    ENDIF  
    THIS.MultiRun = .F.
  ENDIF
   *** Check if any of non workable with programs are activ
  IF !EMPTY(laPrgNames[1])
    FOR lnCountp = 1 TO ALEN(laPrgNames,1)
      lcPrgWind  = LOOKUP(sydobjct.cbasewind,laPrgNames[lnCountp],;
                   sydobjct.capobjnam)

      lcPrgWind  = STUFF(lcPrgWind,2,1,"A")

 
      IF SEEK('WIN'+lcPrgWind,'SYUSTATC')  .AND. ;
              SYUSTATC.ccomp_id = THIS.ActiveCompanyID
        lcUser    = syuStatc.cUser_ID        
        lcStion   = syuStatc.cStation

        
        lcPrglName = LOOKUP(sydObjct.cPrgLName,laPrgNames[lnCountp];
                      ,sydObjct.cApObjNam)
      
        IF lcUser = THIS.User_ID .AND. lcStion = gcStation
          =THIS.MessageBox('TRM00093B00000','ALERT',PROPER(lcPrglName))
          glNoLog    = .T.
          glFirsTime = .F.
          glQuitting = .T.
          RETURN .F.
        ELSE
          IF SEEK ('INI'+'OLDVARS'+lcUser+lcStion,'SYUSTATC')
            lcOldRep = SET('REPROCESS')
              SET REPROCESS TO 1
              IF RLOCK('SYUSTATC')
                UNLOCK IN ALIAS('SYUSTATC')
                GO gnMyStRec IN SYUSTATC
                =RLOCK('SYUSTATC')
                SET REPROCESS TO lcOldRep
              ELSE
               =THIS.MessageBox('TRM00094B00000','ALERT',lcPrglName+'|'+;
               ALLTRIM(LOOKUP(syuUser.cUsr_Name,lcUser,syuUser.cUser_id,"CUSER_ID")))
                SET REPROCESS TO lcOldRep
                glNoLog    = .T.
                glFirsTime = .F.
                glQuitting = .T.
                RETURN .F.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDFOR
    ENDIF  
  *** Disable bar line of this window
  IF !THIS.MultiRun
    IF this.Bar_No > 0  .AND. !EMPTY(THIS.Pop_Name)
      SET SKIP OF BAR (THIS.Bar_No) OF (THIS.Pop_Name) .T.
    ENDIF  
  ENDIF  

  *** Check if user has ADD EDIT DELETE privelage in this program
  lcPross_ID = UPPER(SUBSTR(gcBaseWind,5)) 

  *** Check if the user Admin or operator to get the ADD/EDIT/DELETE
  *** Rights for the current user in current compant and Cerrunt module
  IF THIS.LoginRequired AND THIS.User_Level = "O"
    *E301141,1 Hesham (Start)
    *E301141,1 get the menu module bar the program was runing from
    llApplUsed = !USED('SYDAPPL')
    IF !USED('SYDAPPL')
      USE  (THIS.SYSPATH+'SYDAPPL') IN 0
    ENDIF
    =SEEK(lcProgramModule,'SYDAPPL')
    lcMastMenu = IIF(EMPTY(SYDAPPL.CMODPARENT),lcProgramModule,ALLT(SYDAPPL.CMODPARENT))
    *E301141,1 Hesham (End)
    IF llApplUsed
      USE IN SYDAPPL
    ENDIF
    IF !USED('SYUUSRPR')
      USE  (THIS.SYSPATH+'SYUUSRPR') IN 0
    ENDIF
    SELECT SYUUSRPR
    SET ORDER TO TAG cUser_ID
    *E301141,1 Hesham (Start)
    *E301141,1 check for the user priv. using the menu module id
    *E301141,1 and not the active module id
    *IF SEEK(ALLTRIM(THIS.User_ID)+This.ActiveModuleID+THIS.ActiveCompanyID+lcPross_ID)
    IF SEEK(ALLTRIM(THIS.User_ID)+lcMastMenu+THIS.ActiveCompanyID+lcPross_ID)    
    *E301141,1 Hesham (End)
      THIS.ALLOWADD= SYUUSRPR.lAddRec   
      THIS.ALLOWEDIT= SYUUSRPR.lEditRec
      THIS.ALLOWDELETE= SYUUSRPR.lDeleRec
      IF !EMPTY(SYUUSRPR.mSubProc)
        =gfSubStr(SYUUSRPR.mSubProc,@laSubProc,"|")
      ENDIF

    *** Check if the user is a member of any groups that have access
    *** for Add/Edit/Delete in this program 
    ELSE
    *E301141,1 Hesham (Start)
    *E301141,1 check for the group priv. using the menu module id
    *E301141,1 and not the active module id
      *IF SEEK(ALLTRIM(THIS.User_Group)+This.ActiveModuleID+THIS.ActiveCompanyID+lcPross_ID)    
      IF SEEK(ALLTRIM(THIS.User_Group)+lcMastMenu+THIS.ActiveCompanyID+lcPross_ID)
    *E301141,1 Hesham (Start)      
        THIS.ALLOWADD= SYUUSRPR.lAddRec   
        THIS.ALLOWEDIT= SYUUSRPR.lEditRec
        THIS.ALLOWDELETE= SYUUSRPR.lDeleRec
      
        IF !EMPTY(SYUUSRPR.mSubProc)
          =gfSubStr(SYUUSRPR.mSubProc,@laSubProc,"|")
        ENDIF
      ELSE  
        RETURN .F.      
      ENDIF  
    ENDIF
  *** This user is admen give hime every thing
  ELSE
    STORE .T. TO THIS.ALLOWADD,THIS.ALLOWEDIT,THIS.ALLOWDELETE
  ENDIF    
ENDIF

*SET Help Topic to the related one in the menu file.
=THIS.SetHelpTopic(THIS.Pop_Name,THIS.Bar_no)
ENDPROC
PROCEDURE doprogram
PARAMETER gcProg_Nam,gcParameters,lcProgOrWin,gcSrceModl

*** If comming from the menu gcSrceModl will not be defined so we will
*** use the THIS.ActiveModuleID variable to point to the window source module
*** If comming from another program from the same module so we will
*** use the same variable , but if the to be run program is from 
*** another module use  gcSrceModl to point to the window source module
LOCAL laWindArray,lnCount,lcWinParnt
gcWinAppl   = ALLTRIM(UPPER(IIF(TYPE('gcSrceModl')='C',gcSrceModl,;
                      THIS.ActiveModuleID)))
*E301141,1 Hesham (Start)                      
*E301141,1 check if the program belog to multi modules
*E301141,1 then get the first module it belog to
IF ATC(',',gcWinAppl)<>0                      
  DIME laProgModules[1,1]
  laProgModules = ''
  =gfSubStr(gcWinAppl,@laProgModules)
  gcWinAppl=''
  IF !EMPTY(laProgModules[1,1])
    FOR lnCount = 1 TO ALEN(laProgModules,1)
      IF AT(laProgModules[1,1],THIS.CompanyInstalledModules) > 0
        gcWinAppl = laProgModules[1,1]
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF 
THIS.ProgramModuleID = gcWinAppl
*E301141,1 Hesham (End)                      
*** Get the name of the base window in the program that going to be done
*gcBaseWind  = ALLTRIM(UPPER(IIF(' ' $ gcProg_Nam,;
                  SUBSTR(gcProg_Nam,1,ATC(' ',gcProg_Nam)-1),gcProg_Nam)))

*gcBaseWind  = ALLTRIM(gcProg_Nam)
DIMEN laWindArray[1,1]

FOR lnCount = 1 TO _SCREEN.FORMCOUNT
  lcWinParnt = '_Screen.Forms('+ALLT(STR(lnCount))+').Parent'
  IF TYPE(lcWinParnt) = 'O'
    IF !EMPTY(laWindArray[1,1])
      DIMEN laWindArray[ALEN(laWindArray,1)+1,1]  
    ENDIF
    laWindArray[ALEN(laWindArray,1),1] = _Screen.Forms(lnCount).Parent.Name 
  ENDIF
ENDFOR
gcWindBase  = ALLTRIM(gcProg_Nam)

gnProgCopy  = 0
FOR lnAscii = 65 TO 90
  gnProgCopy = gnProgCopy + 1
*  gcBaseWind =STUFF(gcBaseWind,2,1,CHR(lnAscii))
  gcBaseWind = CHR(lnAscii)+gcWindBase
  IF ASCAN(laWindArray,gcBaseWind)>0
*  IF WVISIBLE(gcBaseWind)
    LOOP
  ELSE
    EXIT 
  ENDIF
ENDFOR
**************


*** Define glFirsTime with .T. to be checked in the setup function to
*** intializ the program for first time 
glFirsTime = .T.
*** From the menu you can only call programs from the current module   
*** so you can use the THIS.ActiveModuleID to point to the name of the module
=this.RunProg(gcProg_Nam,gcParameters,gcWinAppl)


ENDPROC
PROCEDURE runprog
PARAMETERS gcPrgToRun,gcParameters,gcWinAppl
PRIVATE gcWinType,gcModPrg
*E301141,1 Hesham (Start)                      
*IF THIS.ProgramSetup()
IF THIS.ProgramSetup(gcWinAppl)
*E301141,1 Hesham (End)                      
  IF FILE(THIS.ScreenHome+SUBSTR(gcPrgToRun,4)+'.SCX')
    lcFormName = THIS.ScreenHome+SUBSTR(gcPrgToRun,4)
  ELSE
    lcFormName = THIS.ScreenHome+IIF(TYPE('gcWinAppl')='C',gcWinAppl,This.ActiveModuleID)+'\'+SUBSTR(gcPrgToRun,4)
  ENDIF  

  IF FILE(lcFormName+".SCX")
    IF TYPE('gcParameters')='C' AND !EMPTY(gcParameters)
      DO FORM &lcFormName WITH &gcParameters
    ELSE
      DO FORM &lcFormName
    ENDIF  
  ELSE
    = MessageBox("This program is still under development.", 16, _screen.caption)
  ENDIF
ENDIF

ENDPROC
PROCEDURE menubar
PARAMETERS lcPopName,lnBarNo

glPopAct  = .F.
gcAct_Pad = ''

*E300279,1  SET Help Topic to the related one in the menu file.
IF "P05" $ lcPopName
  LOCAL llError
  llError = .F.
  ON ERROR llError = .T.
  *=THIS.SetHlpTopic(lcPopName,lnBarNo)
  =THIS.SetHelpTopic(lcPopName,lnBarNo)  
  ON ERROR
ENDIF
THIS.Bar_No = lnBarNo
THIS.Pop_Name = lcPopName
SET MESSAGE TO ALLTRIM(This.SystemName)


*** Get the array row no for this popup-bar
lcSavExact = SET ('EXACT')

SET EXACT OFF


lnProgNum = ASCAN(This.aProcess,ALLTRIM(lcPopName)+"-"+PADL(lnBarNo,2,'0'))

SET EXACT &lcSavExact

*** Check if this option have any process to run 
IF lnProgNum > 0 
  lcPross  = ASUBSCRIPT(This.aProcess,lnProgNum,1)
  Local llError
  DO CASE
    *** Running module system program
    CASE This.aProcess[lcPross,3] $ 'CP'
      IF EMPTY(ALLTRIM(This.aProcess[lcPross,9]))    
         this.ProcessID=ALLTRIM(This.aProcess[lcPross,5])
        =this.DoProgram(ALLTRIM(This.aProcess[lcPross,5]),ALLTRIM(This.aProcess[lcPross,8]))
      ELSE
         this.ProcessID=ALLTRIM(This.aProcess[lcPross,5])
        =this.DoProgram(ALLTRIM(This.aProcess[lcPross,5]),ALLTRIM(This.aProcess[lcPross,8]),.F.,ALLTRIM(This.aProcess[lcPross,9]))
      ENDIF  
   
    *** Running System global procedure
    CASE This.aProcess[lcPross,3] = 'G'
*     DO ("THIS."+ALLTRIM(This.aProcess[lcPross,2]))
      ON ERROR llError = .T.
      llError = .F.
      this.ProcessID=ALLTRIM(This.aProcess[lcPross,2])
      lcCommand = ALLTRIM(This.aProcess[lcPross,2])+'()'
*     THIS.&lcCommand
      =&lcCommand
      ON ERROR  
      IF llError
        MessageBox("This program is still under development.", 16, _SCREEN.Caption)
      ENDIF

    *** Running Module system report
    CASE This.aProcess[lcPross,3] = 'R'
       this.ProcessID=ALLTRIM(This.aProcess[lcPross,2])    
       IF EMPTY(ALLTRIM(This.aProcess[lcPross,9]))    
         =this.ReportPrint(ALLTRIM(This.aProcess[lcPross,2]),ALLTRIM(This.aProcess[lcPross,8]))
       ELSE
         =this.ReportPrint(ALLTRIM(This.aProcess[lcPross,2]),ALLTRIM(This.aProcess[lcPross,8]),;
         ALLTRIM(This.aProcess[lcPross,9]))
       ENDIF  

    *** Selecting mew module 
    CASE This.aProcess[lcPross,3] = 'M'
      IF EMPTY(ALLTRIM(This.aProcess[lcPross,9]))
         =THIS.ChangeModule(ALLTRIM(This.aProcess[lcPross,2]))
      ELSE
         =THIS.ChangeModule(ALLTRIM(This.aProcess[lcPross,9]))
      ENDIF   

    *** Running External program
*    CASE This.aProcess[lcPross,3] = 'E'
*       this.ProcessID=ALLTRIM(This.aProcess[lcPross,2])    
*       DO gpAppLink WITH (ALLTRIM(This.aProcess[lcPross,2])),;
                         (ALLTRIM(This.aProcess[lcPross,4]))
  ENDCASE

*** If no link was don through the menu program
ELSE
  *** Bar Process was not defined, 
  *** you have to link this bar to a program through the menu program
  =THIS.MessageBox("INM00147B00000","DIALOG")
ENDIF


ENDPROC
PROCEDURE sethelptopic
PARAMETER lcPop_Name,lnBar_no
IF lnBar_No > 0  .AND. !EMPTY(lcPop_Name)
  lnPrgNum   = ASCAN(This.aProcess,ALLTRIM(lcPop_Name)+"-"+PADL(lnBar_no,2,'0'))
  IF lnPrgNum > 0
    lnPrgNum    = 1 + (lnPrgNum/7)
    THIS.HelpTopic  = ALLTRIM(This.aProcess[lnPrgNum,7])
  ELSE
    THIS.HelpTopic  = " "
  ENDIF
ELSE
  THIS.HelpTopic  = " " 
ENDIF
ENDPROC
PROCEDURE programcleanup
PARAMETERS lcBaseWind

lcBaseWind=IIF(PARAMETERS()=0,_SCREEN.ACTIVEFORM.NAME,lcBaseWind)

*** Enable the bar of this window in the menu again 
IF TYPE('lcBaseWind.MultiRun')='L' AND !lcBaseWind.MultiRun
  IF lcBaseWind.Bar_No >0 .AND. !EMPTY(lcBaseWind.Pop_Name) 
    SET SKIP OF BAR (lcBaseWind.Bar_No) OF (lcBaseWind.Pop_Name) .F.
  ENDIF  
ENDIF  
lcBaseWind = IIF(TYPE('lcBaseWind')#'O',_SCREEN.ACTIVEFORM.NAME,lcBaseWind.Name)
*** Clear the temp record in static file
IF !USED('SYDSUPRG')
  SELECT 0 
  USE (THIS.SysPath+'SYDSUPRG')
ELSE
  SELECT SYDSUPRG
ENDIF

SET ORDER TO TAG PRGCOMP

*** Check if the program was single user clear the locking
IF SEEK(SUBSTR(lcBaseWind,5)+THIS.ActiveCompanyID,'SYDSUPRG')
  UNLOCK IN SYDSUPRG
ENDIF

*** Rearrang the window popup
THIS.WinArng()

*** Check if ther is any open program using the controle pannel
*lcMainWind = IIF(_WINDOWS,"FNDATION","")
glNoLog    = .F.

ENDPROC
PROCEDURE winarng
PARAMETERS gcNewWind
PRIVATE gcNewWind,gnWinOrder,gnWinBar,glFrstWind
IF !POPUP('P08PU08')
  RETURN
ENDIF

gnWinOrder = 0
gnWinBar   = This.WinSepta + 1
glFrstWind = .T.
IF _SCREEN.FORMCOUNT > 0
  FOR lnCount = 1 TO _SCREEN.FORMCOUNT
    IF _SCREEN.FORMS(lnCount).VISIBLE AND _SCREEN.FORMS(lnCount).BASECLASS<>'Toolbar'
      IF glFrstWind = .T.
        DEFINE BAR gnWinBar;
                OF P08PU08 ;
                    PROMPT "\-"
        gnWinBar = gnWinBar+1
        glFrstWind = .F.
      ENDIF
      DEFINE BAR gnWinBar OF P08PU08  PROMPT '\<'+ALLT(STR(gnWinBar-This.WinSepta-1))+' '+_SCREEN.FORMS(lnCount).Caption
      lcCommand ='_SCREEN.FORMS('+ALLT(STR(lnCount))+').SHOW'
      ON SELECTION BAR gnWinBar OF P08PU08 &lcCommand
      gnWinBar = gnWinBar+1
    ENDIF             
  ENDFOR
ENDIF
FOR lnCount = gnWinBar TO CNTBAR('P08PU08')
    RELEASE BAR lnCount OF P08PU08
ENDFOR
IF _SCREEN.FormCount=1 OR glFrstWind = .T.
  IF TYPE('oAriaApplication.oToolBar')='O'
    oAriaApplication.oToolBar.Init()
  ENDIF
ENDIF
*lcProgName = IIF(_SCREEN.FORMCOUNT >1,'ACTIVE','')
ENDPROC
PROCEDURE singleuserprogram
PARAMETERS lcPrgName
lcPrgName = UPPER(lcPrgName)
SET EXACT ON
*** Check if the program restricted for one user run
IF !USED('SYDSUPRG')
  USE (THIS.SysPath+'SYDSUPRG') IN 0 
ENDIF
SET ORDER TO TAG PRGCOMP IN SYDSUPRG
IF !SEEK(lcPrgName+UPPER(THIS.ActiveCompanyID),'SYDSUPRG')
  INSERT INTO SYDSUPRG ;
    (cProgNam,cComp_ID)  VALUES (lcPrgName,THIS.ActiveCompanyID)
ENDIF

SET EXACT OFF 

lcOldRep = SET('REPROCESS')
SET REPROCESS TO 1

IF RLOCK('SYDSUPRG')
  =THIS.AddUserInformation('SYDSUPRG')
  SET REPROCESS TO lcOldRep
ELSE
  IF !USED('syuUser')
    USE (THIS.SysPath+'syuUser') IN 0   
  ENDIF
  lcUserName = ALLTRIM(LOOKUP(syuUser.cUsr_Name,;
               sydSuPrg.cAdd_User,syuUser.cUser_ID,'CUSER_ID'))
               
  =THIS.MessageBox('INM00089B00000','ALERT',lcUserName)
  SET REPROCESS TO lcOldRep

  RETURN .F.      

ENDIF

ENDPROC
PROCEDURE adduserinformation
LPARAMETERS lcFileName, oForm
PRIVATE    lcFileName,laFields,lcSavAlias, lnOldDataSession

lnOldDataSession = SET ("DATASESSION")
IF TYPE("oForm") = "O"
  SET DATASESSION TO oForm.DataSession
ENDIF

lcSavAlias = SELECT(0)
IF !(TYPE ('lcFileName') $ "UL")
  SELECT (lcFileName)
ENDIF  

REPLACE cAdd_User WITH THIS.User_ID ,;
        dAdd_Date WITH DATE()       ,;
        cAdd_Time WITH gfGetTime()

SELECT (lcSavAlias)

SET DATASESSION TO lnOldDataSession
ENDPROC
PROCEDURE reportprint
PARAMETERS lcReport,gcParameters,gcWinAppl
*lcMdRports = THIS.ReportHome+THIS.ActiveModuleID+"REPORT"
*DO &lcMdRports WITH lcReport
gcWinAppl   = ALLTRIM(UPPER(IIF(TYPE('gcSrceModl')='C',gcSrceModl,;
                      IIF(PARAM()>2,gcWinAppl,THIS.ActiveModuleID))))
IF ATC(',',gcWinAppl)<>0                      
  DIME laProgModules[1,1]
  laProgModules = ''
  =gfSubStr(gcWinAppl,@laProgModules)
  gcWinAppl=''
  IF !EMPTY(laProgModules[1,1])
    FOR lnCount = 1 TO ALEN(laProgModules,1)
      IF AT(laProgModules[1,1],THIS.CompanyInstalledModules) > 0
        gcWinAppl = laProgModules[1,1]
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF 
THIS.ProgramModuleID = gcWinAppl
IF FILE(THIS.ScreenHome+lcReport+'.SCX')
  lcFormName = THIS.ScreenHome+lcReport
ELSE
  lcFormName = THIS.ScreenHome+IIF(TYPE('gcWinAppl')='C',gcWinAppl,This.ActiveModuleID)+'\'+lcReport
ENDIF  
IF TYPE('gcParameters')='C' AND !EMPTY(gcParameters)
  DO FORM &lcFormName WITH &gcParameters
ELSE
  DO FORM &lcFormName
ENDIF  


ENDPROC
PROCEDURE doformretval
LPARAMETERS tcForm
*-- This function is meant to be used with a form class that
*-- is derived from tsformretval which is defined in TSBASE.VCX
*-- Notice how objects with LOCAL scope are automatically
*-- released when the methods ends.

LOCAL loForm, ;
      luRetVal
*loForm = CREATEOBJECT(tcForm)
*loForm.Show() 
DO FORM ( tcForm ) TO luRetVal
RETURN luRetVal

ENDPROC
PROCEDURE do
*-- Start the event loop
this.ReleaseToolBars()  
*_SCREEN.ADDOBJECT('ARIATITLE','ARIALabel')
*_SCREEN.ARIATITLE.CAPTION='Aria Systems'
*_SCREEN.ARIATITLE.FONTSIZE=44
*_SCREEN.ARIATITLE.FONTBOLD=.T.
*_SCREEN.ARIATITLE.BACKSTYLE=0
*_SCREEN.ARIATITLE.AUTOSIZE=.T.
*_SCREEN.ARIATITLE.TOP = 124
*_SCREEN.ARIATITLE.LEFT=134
*_SCREEN.ARIATITLE.VISIBLE=.T.

*-- Save current main window title and set
*-- the new one
this.OldWindCaption = _screen.Caption  
_screen.caption = this.SystemName

THIS.oToolBar=CREATEOBJECT('AriaControlToolBar')
*THIS.oToolBar.SHOW


IF !THIS.ValidActivationKey()
  CLOSE DATA ALL 
  THIS.RestoreEnvironment()
  RETURN .F.
ENDIF
IF !USED('SYUSTATC')
  USE (THIS.SysPath+'SYUSTATC') IN 0 ORDER TAG CUSER_ID
ENDIF
IF !USED('SYUUSER')
  USE (THIS.SysPath+'SYUUSER') IN 0 ORDER TAG CUSER_ID
ENDIF
SELECT SYUSTATC
IF THIS.LoginRequired AND !THIS.LOGIN()
   CLOSE DATA ALL
   THIS.RestoreEnvironment()
   RETURN .F.
ELSE
  IF EMPTY(THIS.USER_NAME)
    THIS.USER_NAME = THIS.USER_ID
  ENDIF  
ENDIF
THIS.LogUser()

IF THIS.UserUseToolBar
  THIS.oToolBar.SHOW
ENDIF  

 *-- Build the system Menu and the Default Module Menu if 
 *-- there is one
*  SET SYSMENU TO
  THIS.SetMenu('SY','I','NOSHOW')  
  IF !EMPTY(THIS.UserModuleID)
    THIS.SetMenu('SY','S')  
    IF THIS.SetMenu(THIS.UserModuleID,'A')
      _SCREEN.CAPTION = ALLTRIM(THIS.SystemName)+" - "+ALLTRIM(THIS.ActiveModuleName)+;
             IIF(!EMPTY(THIS.ActiveCompanyID),' ('+ALLTRIM(THIS.ActiveCompanyID)+')','')
      
    ENDIF
*  ELSE
*    THIS.SetMenu('SY','S')  
  ENDIF
READ EVENTS

ENDPROC
PROCEDURE sysexit
IF !EMPTY(THIS.USER_ID)
  IF USED('syuuser')
    SELECT syuuser
    SET ORDER TO 1
  ELSE
    SELECT 0
    USE (THIS.SysPath+"syuuser") ORDER 1
  ENDIF

  SEEK THIS.User_ID
  REPLACE  lusr_logd WITH .F.        ,;
           cUsr_llot WITH gfGetTime()
         
  SELECT SYUSTATC
  GO THIS.USERSTATICRECORD
  REPLACE cObj_typ    WITH '   ' ,;
          cObj_Name   WITH '    ',;
          cUser_ID    WITH ''    ,;
          cComp_ID    WITH ''    ,;
          cPlatform   WITH " "   ,;
          cStation    WITH ''    ,;
          lKeep_It    WITH .F.             && permenant record
  
  DELETE
  UNLOCK           
  = TABLEUPDATE(0,.T.)  
ENDIF  

THIS.ENDPROG = .T.
CLEAR EVENTS

ENDPROC
PROCEDURE releasetoolbars
DIMENSION THIS.aToolBars[11,2]
THIS.aToolBars[1,1] = "Form Designer"
THIS.aToolBars[2,1] = "Standard"
THIS.aToolBars[3,1] = "Layout"
THIS.aToolBars[4,1] = "Query Designer"
THIS.aToolBars[5,1] = "View Designer"
THIS.aToolBars[6,1] = "Color Palette"
THIS.aToolBars[7,1] = "Form Controls"
THIS.aToolBars[8,1] = "Database Designer"
THIS.aToolBars[9,1] = "Report Designer"
THIS.aToolBars[10,1] = "Report Controls"
THIS.aToolBars[11,1] = "Print Preview"

FOR i = 1 TO ALEN(THIS.aToolBars, 1)
  THIS.aToolBars[i,2] = WVISIBLE(THIS.aToolBars[i,1])
  IF THIS.aToolBars[i,2]
    HIDE WINDOW (THIS.aToolBars[i,1])
  ENDIF
ENDFOR

ENDPROC
PROCEDURE getuserinfo
IF !USED('syuuser')
  SELECT 0
  USE (THIS.SYSPATH+"syuuser") ORDER TAG CUSER_ID
ELSE
  SELECT SYUUSER
  SET ORDER TO TAG CUSER_ID   
ENDIF  
=SEEK(THIS.USER_ID)
THIS.User_Name = syuuser.cUsr_Name
THIS.UserCompanyID = syuuser.cUsr_Dcom
THIS.User_Group = syuuser.cUsr_grup
THIS.UserModuleID = syuuser.cUsr_dMdl
THIS.LoginRequired = (syuuser.cusr_levl='O')
THIS.User_Level = syuuser.cusr_levl
THIS.UserUseToolBar = syuuser.lusr_ustb
lcResource = ALLT(syuuser.cusr_resr)
lcResource = 'V'+SUBSTR(lcResource,2)

ON ERROR llError = .T.
llError = .F.
*B803605,1 Disable Error handler
llCurrErrorHandlerEnabled = THIS.ErrorHandlerEnabled
THIS.ErrorHandlerEnabled = .F.
*B803605,1 (End)

IF FILE(THIS.ResourceHome+lcResource+'.DBF')
  SET RESOURCE TO (THIS.ResourceHome+lcResource)
ELSE
  CREATE TABLE (THIS.ResourceHome+lcResource+'.DBF') FREE (type c(12),id c(12),name M,readonly L,ckval N(6,0),data M,updated D) 
  USE IN (ALLT(lcResource))	
  IF USED("SYCRESRC")
    USE IN SYCRESRC
  ENDIF
  
  SET RESOURCE TO (THIS.ResourceHome+lcResource)  
ENDIF  
*B803605,1 Enable Error handler
THIS.ErrorHandlerEnabled = llCurrErrorHandlerEnabled 
*B803605,1 (End)

ON ERROR
IF ALLT(syuuser.cdef_bmp) = '(None)' AND TYPE('_SCREEN.GROUNDBMP')= 'O'
  _SCREEN.REMOVEOBJECT('GROUNDBMP')
ENDIF
IF !empty(syuuser.cdef_bmp) AND FILE(this.BitMapHome+ALLT(syuuser.cdef_bmp))
  IF TYPE('_SCREEN.GROUNDBMP')#'O'
    _SCREEN.ADDOBJECT('GroundBmp','image')
  ENDIF  
  _screen.GroundBmp.picture = LOWER(this.BitMapHome+ALLT(syuuser.cdef_bmp))
  _screen.GroundBmp.top = (_screen.Height - _screen.GroundBmp.height)/2
  _screen.GroundBmp.left = (_screen.width - _screen.GroundBmp.width)/2
  _screen.GroundBmp.visible = .t.
ENDIF
IF !EMPTY(THIS.UserCompanyID) &&EMPTY(THIS.ActiveCompanyID)
  THIS.ActiveCompanyID = THIS.UserCompanyID
  THIS.GetCompanyInformation(THIS.UserCompanyID)
ENDIF
*THIS.UserModuleID = ''
*THIS.GetCompanyInformation(THIS.UserCompanyID)
*THIS.ActiveModuleID = THIS.UserModuleID

IF !EMPTY(THIS.UserModuleID)
  IF !THIS.GetModuleInformation(THIS.UserModuleID)
    THIS.UserModuleID = ''
*    =THIS.GetModuleInformation(THIS.UserModuleID)
  ELSE
    THIS.UserModuleID = THIS.ActiveModuleID
  ENDIF
ENDIF
*THIS.ActiveModuleID = THIS.UserModuleID

IF USED('SYUUSER')
  USE IN SYUUSER
ENDIF

ENDPROC
PROCEDURE getcompanyinformation
PARAMETERS lcComp_Id

IF USED("syccomp")
  SELECT syccomp
  SET ORDER TO cComp_Id
ELSE
  SELECT 0
  USE (THIS.SysPath+"syccomp") ORDER 1
ENDIF


IF SEEK(lcComp_Id)
*  THIS.CompanyDefaultModule = ALLTRIM(mComp_Mdl)
*  THIS.CompanyLevel = IIF(EMPTY(ccompprnt),'P','C')
  THIS.PrntCompanyID = IIF(EMPTY(ccompprnt),cComp_Id,ccompprnt)
  THIS.DefaultCountry = IIF(EMPTY(cCont_code),'USA',cCont_code)
  THIS.BaseCurrency = IIF(EMPTY(CCURRCODE),'USDLR',CCURRCODE)
  THIS.CurrentYear = CCURR_YER
  THIS.CurrentPeriod = CCURR_Prd
  THIS.CompanyInstalledModules = ALLTRIM(mcomp_mdl)
  THIS.CompanySetupModules = ALLT(mmodlset)  
  THIS.CompanySets(ALLTRIM(syccomp.ccont_code))
ENDIF
ENDPROC
PROCEDURE getmoduleinformation
PARAMETERS lcAppID
*** Update the global module variable

IF !USED("SYDAPPL")
  SELECT 0
  USE (THIS.SysPath+"SYDAPPL")
ELSE
  SELECT SYDAPPL
ENDIF

SET ORDER TO TAG CAPP_ID

*** Check if the selected module linked to the system

IF SEEK(lcAppID) AND CMODULEVER $ 'BV' AND AT(lcAppID,THIS.CompanySetupModules) > 0 AND !EMPTY(THIS.ActiveCompanyID)
*E301141,1 Hesham (Start)
*E301141,1 check if the module is a single module in the menu
  IF EMPTY(cbarModule)
    THIS.ActiveModuleID = lcAppID
    THIS.ActiveModuleName = ALLTRIM(SYDAPPL.cApp_name)
    THIS.ParentModule = SYDAPPL.lParntMdl
    THIS.ActiveModuleBuildNumber = sydappl.CMDLBUILD
  *E301141,1 in case the module belog to another module in the menu file
  *E301141,1 then get all the related modules
  ELSE
    THIS.ActiveModuleName = ALLTRIM(SUBSTR(SYDAPPL.CBARMODULE,ATC('|',SYDAPPL.CBARMODULE)+1))
    lcAppID = IIF(!EMPTY(SYDAPPL.CBARMODULE),ALLT(SYDAPPL.CBARMODULE),lcAppID)
    lcAppID = IIF(ATC('|',lcAppID)=0,lcAppID,SUBSTR(lcAppID,1,ATC('|',lcAppID)-1))
    THIS.ParentModule = .F.
    THIS.ActiveModuleBuildNumber = sydappl.CMDLBUILD
    DIME laModSelected[1,1]
    laModSelected = ''
    =gfSubStr(lcAppID,@laModSelected)
    lcAppID = ''    
    IF !EMPTY(laModSelected[1,1])
      PRIVATE lnCount
      FOR lnCount = 1 TO ALEN(laModSelected,1)
        IF SEEK(laModSelected[lnCount,1]) AND AT(laModSelected[lnCount,1],THIS.CompanyInstalledModules) > 0
          lcAppID = lcAppID+IIF(EMPTY(lcAppID),'',',')+laModSelected[lnCount,1]
        ENDIF
      ENDFOR
    ENDIF
    THIS.ActiveModuleID = lcAppID    
   *E301141,1 Hesham (End)    
  ENDIF
  =THIS.SetHlpFl(lcAppID)
  RETURN .T.
ENDIF
RETURN .F.
ENDPROC
PROCEDURE help
HELP
ENDPROC
PROCEDURE tooladdnew
This.oToolBar.cmdAdd.Click
ENDPROC
PROCEDURE toolgobttm
This.oToolBar.cmdEnd.Click
ENDPROC
PROCEDURE toolclose
This.oToolBar.cmdExit.Click
ENDPROC
PROCEDURE tooldelete
This.oToolBar.cmdDelete.Click
ENDPROC
PROCEDURE tooledit
This.oToolBar.cmdEdit.Click
ENDPROC
PROCEDURE toolgonext
This.oToolBar.cmdNext.Click
ENDPROC
PROCEDURE toolgoprev
This.oToolBar.cmdPrev.Click
ENDPROC
PROCEDURE toolsave
This.oToolBar.cmdAdd.Click
ENDPROC
PROCEDURE toolgotop
This.oToolBar.cmdTop.Click
ENDPROC
PROCEDURE validatact_key
*!*************************************************************
*! Name      : lfvAct_Key
*! Developer : Hesham El-Sheltawi
*! Date      : 05/22/95
*! Purpose   : to validate the activation key and return
*!             the # of users and the installed modules
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcAct_Key
*!                      lcRetModules
*!                      lnNoUsers
*!*************************************************************
*! Returns            : Logical
*!*************************************************************
*! Example   : IF lfvActKey(lcAct_Key,@gcModules,@gnNoUsers)
*!*************************************************************
* Restore last enviroment if requird in case of ubnormal termination
*:->
*Function lfvAct_Key
PARAMETERS lcAct_Key,lcRetModules,lnNoUsers,lcPlatForm
DIMENSION laModulIns[1,2]
STORE '' TO laModulIns,lcInsModules,lcInsPlat,lcRetModules,lcPlatForm
STORE .F. TO llInstUsd,llApplUsd
*IF !USED('SYCINST')
*  SELECT 0
*  USE (gcSysHome+'SYCINST')
*  llInstUsd = .T.
*ENDIF
*GO TOP IN SYCINST
*B600506,1 take care if the SYDAPPL file was opend by the function
*B600506,1 IF yes then at the end of the function close the file
IF !USED('sydappl')
  SELECT 0
  USE (gcSysHome+'sydappl')
  llApplUsd= .T.
ENDIF
lcAct_Key = ALLTRIM(lcAct_Key)
lcKey=SUBSTR(lcAct_Key,2)
lcKey=STUFF(lcKey,LEN(lcKey),1,'')
lcKey=STRTRAN(lcKey,'-')
lcKey=SUBSTR(lcKey,1,LEN(lcKey)-1)
llValdKey = .F.
SELECT cApp_ID,SYS(2007,cApp_ID);
  FROM (gcSysHome+'sydappl');
  INTO ARRAY laModulIns;
  ORDER BY 2;
  WHERE CAPP_ID<>'SY'
 
FOR lnCount = 1 TO ALEN(laModulIns,1)  
  lcInsModules = lcInsModules+laModulIns[lnCount,1]
  lcRetModules = lcRetModules+IIF(lnCount=1,'',',')+laModulIns[lnCount,1]
ENDFOR
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsDos,'D','') 
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsMac,'M','') 
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsUnx,'U','') 
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsWin,'W','') 
lcPlatForm=RIGHT(lcAct_Key,1)
lnPlatForm =IIF(ASC(lcPlatForm)>=65,ASC(lcPlatForm)-55,VAL(lcPlatForm))
lcBinary=''
lnPrimary=lnPlatForm
lcPlatForm=''
DO WHILE lnPrimary>0
  lcBinary=STR(MOD(lnPrimary,2),1)+lcBinary
  lcPlatForm=lcPlatForm+IIF(MOD(lnPrimary,2)=1,SUBSTR('DMUW',LEN(lcBinary),1),'')
  lnPrimary=INT(lnPrimary/2)
ENDDO
lcInsPlat=lcPlatForm
*lcAct_Key=SUBSTR(lcAct_Key,1,LEN(lcAct_Key)-1)
*lcNoUsers = SUBSTR(lcAct_Key,1,1)+RIGHT(lcAct_Key,1)
lcNoUsers = SUBSTR(lcAct_Key,1,1)+SUBSTR(lcAct_Key,LEN(lcAct_Key)-1,1)
*B601500,1 KHM 12/26/96 (Begin) Adding the ceiling function to get
*B601500,1 KHM           the nearest greater number 
*lnLenAct=(LEN(STRTRAN(lcKey,'-')))/2
lnLenAct=CEILING((LEN(STRTRAN(lcKey,'-')))/2)
*B601500,1 KHM (End)
lcHidChar =''
lcOldAct_Key = ''
FOR lnCount = 1 TO lnLenAct
  lcHidChar    = lcHidChar+SUBSTR(lcKey,((lnCount-1)*2)+1,1)
  lcOldAct_Key = lcOldAct_Key+SUBSTR(lcKey,((lnCount-1)*2)+2,1)
ENDFOR
FOR lnCount = VAL(lcNoUsers)*10 TO (VAL(lcNoUsers)*10)+9
  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
  FOR lnTrilVer = 1 TO 2
    llTrilVer = lnTrilVer = 2
  *E300579,1 Hesham El-Sheltawi (End)      

  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
*    lcKeyConted = SYS(2007,PADR(lcInsModules+lcInsPlat+STR(lnCount),80))
    lcKeyConted = SYS(2007,PADR(lcInsModules+lcInsPlat+IIF(llTrilVer,'T','')+STR(lnCount),80))
  *E300579,1 Hesham El-Sheltawi (End)        

  lcKeyAct=''
  FOR lnHidLen = 1 TO LEN(lcHidChar)
    lcKeyAct = lcKeyAct+SUBSTR(lcHidChar,lnHidLen,1)+ SUBSTR(lcKeyConted,lnHidLen,1)
  ENDFOR
  lcKeyAct=LEFT(lcNoUsers,1)+lcKeyAct+RIGHT(lcNoUsers,1)
  *B600506,1 check for the validation key with ignoring the inserted
  *B600506,1 random numbers
  *B601500,1 KHM 12/26/96 (Begin) Check if its the valid key or not
  *IF lcKeyConted=lcOldAct_Key
  IF lcKeyConted==lcOldAct_Key
  *B601500,1 KHM (End)
    llValdKey = .T.
    EXIT
  ENDIF
*  IF lcKeyAct=STRTRAN(lcAct_Key,'-')
*    llValdKey = .T.
*    EXIT
*  ENDIF         
  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
  ENDFOR
  IF llValdKey = .T.
    EXIT
  ENDIF
  *E300579,1 Hesham El-Sheltawi (End)  

ENDFOR 

*E300581,1 Hesham El-Sheltawi (Start)
*E300581,1 To be removed Latter So that the activation key Work
*  lnNoUsers = lnCount
*  llAllDone  = .T.     
* return .t.
*E300581,1 Hesham El-Sheltawi (End)
IF !llValdKey 
  lcRetModules = ''
  lcPlatForm=''
  lnNoUsers = 0
  llAllDone  = .F. 
ELSE
  lnNoUsers = lnCount
  llAllDone  = .T.     
  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
  lcPlatForm = lcPlatForm + IIF(llTrilVer,'T','')
  *E300579,1 Hesham El-Sheltawi (End)  
ENDIF
*B600506,1 take care if the SYDAPPL file was opend by the function
*B600506,1 IF yes then at the end of the function close the file
IF USED('SYCINST') AND llInstUsd 
  USE IN SYCINST
ENDIF
IF USED('SYDAPPL') AND llApplUsd
  USE IN SYDAPPL
ENDIF
RETURN llAllDone


*!*	PARAMETERS lcAct_Key,lcRetModules,lnNoUsers,lcPlatForm
*!*	DIMENSION laModulIns[1,2]
*!*	STORE '' TO laModulIns,lcInsModules,lcInsPlat,lcRetModules,lcPlatForm
*!*	STORE .F. TO llInstUsd,llApplUsd
*!*	IF !USED('sydappl')
*!*	  SELECT 0
*!*	  USE (THIS.SysPath+'sydappl')
*!*	  llApplUsd= .T.
*!*	ENDIF
*!*	lcAct_Key = ALLTRIM(lcAct_Key)
*!*	lcKey=SUBSTR(lcAct_Key,2)
*!*	lcKey=STUFF(lcKey,LEN(lcKey),1,'')
*!*	lcKey=STRTRAN(lcKey,'-')
*!*	lcKey=SUBSTR(lcKey,1,LEN(lcKey)-1)
*!*	llValdKey = .F.
*!*	SELECT cApp_ID,SYS(2007,cApp_ID);
*!*	  FROM (THIS.SysPath+'sydappl');
*!*	  INTO ARRAY laModulIns;
*!*	  ORDER BY 2;
*!*	  WHERE CAPP_ID<>'SY'
*!*	 
*!*	FOR lnCount = 1 TO ALEN(laModulIns,1)  
*!*	  lcInsModules = lcInsModules+laModulIns[lnCount,1]
*!*	  lcRetModules = lcRetModules+IIF(lnCount=1,'',',')+laModulIns[lnCount,1]
*!*	ENDFOR
*!*	lcPlatForm=RIGHT(lcAct_Key,1)
*!*	lnPlatForm =IIF(ASC(lcPlatForm)>=65,ASC(lcPlatForm)-55,VAL(lcPlatForm))
*!*	lcBinary=''
*!*	lnPrimary=lnPlatForm
*!*	lcPlatForm=''
*!*	DO WHILE lnPrimary>0
*!*	  lcBinary=STR(MOD(lnPrimary,2),1)+lcBinary
*!*	  lcPlatForm=lcPlatForm+IIF(MOD(lnPrimary,2)=1,SUBSTR('DMUW',LEN(lcBinary),1),'')
*!*	  lnPrimary=INT(lnPrimary/2)
*!*	ENDDO
*!*	lcInsPlat=lcPlatForm
*!*	lcNoUsers = SUBSTR(lcAct_Key,1,1)+SUBSTR(lcAct_Key,LEN(lcAct_Key)-1,1)
*!*	lnLenAct=CEILING((LEN(STRTRAN(lcKey,'-')))/2)
*!*	lcHidChar =''
*!*	lcOldAct_Key = ''
*!*	FOR lnCount = 1 TO lnLenAct
*!*	  lcHidChar    = lcHidChar+SUBSTR(lcKey,((lnCount-1)*2)+1,1)
*!*	  lcOldAct_Key = lcOldAct_Key+SUBSTR(lcKey,((lnCount-1)*2)+2,1)
*!*	ENDFOR
*!*	FOR lnCount = VAL(lcNoUsers)*10 TO (VAL(lcNoUsers)*10)+9
*!*	  lcKeyConted = SYS(2007,PADR(lcInsModules+lcInsPlat+STR(lnCount),80))
*!*	  lcKeyAct=''
*!*	  FOR lnHidLen = 1 TO LEN(lcHidChar)
*!*	    lcKeyAct = lcKeyAct+SUBSTR(lcHidChar,lnHidLen,1)+ SUBSTR(lcKeyConted,lnHidLen,1)
*!*	  ENDFOR
*!*	  lcKeyAct=LEFT(lcNoUsers,1)+lcKeyAct+RIGHT(lcNoUsers,1)
*!*	  IF lcKeyConted==lcOldAct_Key
*!*	    llValdKey = .T.
*!*	    EXIT
*!*	  ENDIF
*!*	ENDFOR 
*!*	IF !llValdKey 
*!*	  lcRetModules = ''
*!*	  lcPlatForm=''
*!*	  lnNoUsers = 0
*!*	  llAllDone  = .F. 
*!*	ELSE
*!*	  lnNoUsers = lnCount
*!*	  llAllDone  = .T.     
*!*	ENDIF
*!*	IF USED('SYCINST') AND llInstUsd 
*!*	  USE IN SYCINST
*!*	ENDIF
*!*	IF USED('SYDAPPL') AND llApplUsd
*!*	  USE IN SYDAPPL
*!*	ENDIF
*!*	RETURN llAllDone

ENDPROC
PROCEDURE validactivationkey
gcSysHome = THIS.syspath
gcAct_Key = ''



*E301874,1 Abd - Modify the activation key file to be as EXE not a Bin File
*E301874,1 Abd - And get our needed dat from this exe After Build A0039. [Begin]
*E301874,1 ABD - Define Globel variable to know if we run
*E301874,1 ABD - the Old Version or the New One. [Begin]
PRIVATE   llError_Local, lloldversn ,lltranusd , llsyappl
STORE .F. TO lloldversn ,lltranusd , llsyappl , llError_Local
lnoldalias = SELECT(0)
IF !USED(THIS.syspath+"SYDAPPL")
  USE THIS.syspath+"SYDAPPL" SHARED IN 0 ORDER Capp_id
  llsyappl = .T.
ENDIF

SELECT SYDAPPL
IF SEEK('SY') .AND. VAL(RIGHT(ALLTRIM(SYDAPPL.cmdlbuild),2)) >= 40
  lloldversn = .F.
ELSE
  IF EOF()
    lloldversn = .T.
  ELSE
    IF !USED(THIS.syspath+"SYDTRANS")
      USE THIS.syspath+"SYDTRANS" SHARED IN 0 ORDER batchno
      lltranusd = .T.
    ENDIF
    SELECT sydtrans
    GO BOTTOM
    lnbatchno  = sydtrans.nbatchno
    lloldversn = IIF(EMPTY(lnbatchno),.T.,lnbatchno<120 )
    *-- Close The File if Open.
    IF lltranusd
      USE IN sydtrans
    ENDIF
  ENDIF
ENDIF
*-- Close The File
IF llsyappl
  USE IN SYDAPPL
ENDIF

SELECT (lnoldalias)
IF lloldversn
  *E301874,1 Abd -[End]


  IF FILE(THIS.syspath+"ACT_KEY.BIN")
    lcHandel    = FOPEN(THIS.syspath+"ACT_KEY.BIN")
    lcContent   = FGET(lcHandel)
    = FCLOSE(lcHandel)
    gcCompName  = gfSubstr(lcContent,1,"|")
    gcAct_Key   = gfSubstr(lcContent,2,"|")
    gcLicence   = gfSubstr(lcContent,3,"|")
    IF !THIS.ValidatAct_Key(gcAct_Key,@gcPrmtMdls,@gnMaxUsers,@gcPlatForm)
      lcCompName = gcCompName
      lcAct_Key  = gcAct_Key
      lclicence  = gcLicence
      llAllDone  = .F.
      gcSysHome = THIS.syspath
      DO SYACTKEY.SPR
      IF llAllDone
        gcCompName = ALLTRIM(lcCompName)
        gcAct_Key  = ALLTRIM(lcAct_Key)
        gcLicence  = ALLTRIM(lclicence)

        lcHandel   = FCREAT(THIS.syspath+"ACT_KEY.BIN")
        = FPUT(lcHandel,gcCompName+"|"+gcAct_Key+"|"+gcLicence)
        = FCLOSE(lcHandel)
      ENDIF
    ENDIF
  ELSE
    lcCompName = gcCompName
    lcAct_Key  = gcAct_Key
    lclicence  = gcLicence
    llAllDone  = .F.
    DO SYACTKEY.SPR
    IF llAllDone
      gcCompName = ALLTRIM(lcCompName)
      gcAct_Key  = ALLTRIM(lcAct_Key)
      gcLicence  = ALLTRIM(lclicence)

      lcHandel   = FCREAT(THIS.syspath+"ACT_KEY.BIN")
      = FPUT(lcHandel,gcCompName+"|"+gcAct_Key+"|"+gcLicence)
      = FCLOSE(lcHandel)
    ENDIF
  ENDIF
  *E301874,1 Abd - Else For If Statment. [Begin]  
  *RETURN THIS.ValidatAct_Key(gcAct_Key,@gcPrmtMdls,@gnMaxUsers,@gcPlatForm)
ELSE
  STORE '' TO lcfilepath,lcnewfile,lcfilepath,lcfilename
  IF FILE(THIS.syspath+"ACT_KEY.BIN")
    IF THIS.validkeyvalu() .AND. !(THIS.ValidatAct_Key(gcAct_Key,@gcPrmtMdls,@gnMaxUsers,@gcPlatForm))
      lcnewfile = ''
      llProcess = .F.
      *-- Call the Screen.
      lcReturn = THIS.DoFormRetVal("SYGETACTV")
      *-- If this flage is fase don't complete the Process.
      IF !llProcess .OR. EMPTY(lcnewfile)
        = MESSAGEBOX('Invalid Activation Key ! System will be terminated.',64, _SCREEN.CAPTION)
        RETURN .F.
      ENDIF
      lnpathpos  = RAT('\',lcnewfile)
      lcfilepath = LEFT(lcnewfile,lnpathpos)
      lcfilename = SUBSTR(lcnewfile,lnpathpos+1,LEN(lcnewfile))
      IF THIS.validkeyvalu() .AND. llError_Local
        = MESSAGEBOX('Invalid Activation Key ! System will be terminated.',64, _SCREEN.CAPTION)
        RETURN .F.
      ENDIF
    ENDIF
  ELSE
    lcnewfile = ''
    llProcess = .F.
    *-- Call the Screen.
    lcReturn = THIS.DoFormRetVal("SYGETACTV")
    *-- If this flage is fase don't complete the Process.
    IF !llProcess .OR. EMPTY(lcnewfile)
      = MESSAGEBOX('Invalid Activation Key ! System will be terminated.',64, _SCREEN.CAPTION)
      RETURN .F.
    ENDIF

    lnpathpos  = RAT('\',lcnewfile)
    lcfilepath = LEFT(lcnewfile,lnpathpos)
    lcfilename = SUBSTR(lcnewfile,lnpathpos+1,LEN(lcnewfile))
    IF THIS.validkeyvalu() .AND. llError_Local
      = MESSAGEBOX('Invalid Activation Key ! System will be terminated.',64, _SCREEN.CAPTION)
      RETURN .F.
    ENDIF
  ENDIF
  IF llError_Local
    = MESSAGEBOX('Invalid Activation Key ! System will be terminated.',64, _SCREEN.CAPTION)
    RETURN .F.
  ENDIF

ENDIF
IF 'T' $ gcPlatForm
  IF FILE(THIS.syspath+"I900INV.FXP")
    RESTORE FROM (THIS.syspath+"I900INV.FXP") ADDI
    IF DATE() > ldExpDate+30
      = MESSAGEBOX('The Demo version has been expired.',64, _SCREEN.CAPTION)
      RETURN .F.
    ENDIF
  ELSE
    ldExpDate = DATE()
    SAVE TO (THIS.syspath+"I900INV.FXP") ALL LIKE ldExpDate
  ENDIF
ENDIF
IF !(THIS.ValidatAct_Key(gcAct_Key,@gcPrmtMdls,@gnMaxUsers,@gcPlatForm))
  = MESSAGEBOX('Invalid Activation Key ! System will be terminated.',64, _SCREEN.CAPTION)
  RETURN .F.
ENDIF
RETURN .T.
*E301874,1 Abd - [End]

ENDPROC
PROCEDURE loguser
PARAMETERS llReLogin
llReLogin = IIF(TYPE('llReLogin')#'L',.F.,llReLogin)

PRIVATE lcOldRep
lcOldRep = SET('REPROCESS')
= CURSORSETPROP('Buffering', 5, 'SYUSTATC' )  && Enable 
SET REPROCESS TO 1
  *** Save old enviroment variables
  SELECT syustatc
  *** Check if the user had an ubnormal termination in last session
IF !llReLogin  
  IF SEEK ('INI'+'OLDVARS'+THIS.User_ID+THIS.Station)
    IF RLOCK()
      glRestore = .F.
      lnIniRec  = RECNO()
      IF SEEK('WIN')
        SCAN REST WHILE cObj_Typ  = 'WIN'  
          IF cUser_ID  = THIS.User_ID .AND.  ALLTRIM(THIS.STATION) = ALLTRIM(THIS.Station)
            DELETE
          ENDIF
        ENDSCAN       
      ENDIF     
      GO lnIniRec 
    ELSE
    ENDIF  
  *** Fresh start
  ELSE
    SET DELETE OFF
    DO WHILE .T.    
      IF SEEK(' ') .AND. DELETED() 
        IF RLOCK()
          RECALL
        ELSE
          LOOP  
        ENDIF  
      ELSE  
        APPEND BLANK
        IF !RLOCK()
          LOOP
        ENDIF
      ENDIF  
      EXIT
    ENDDO
    SET DELETE ON
  ENDIF  
ENDIF  
  REPLACE cObj_typ    WITH 'INI';
          cObj_Name   WITH 'OLDVARS';
          cUser_ID    WITH THIS.User_ID;
          cComp_ID    WITH THIS.ActiveCompanyID;
          cPlatform   WITH "W";
          cStation    WITH THIS.Station;
          lKeep_It    WITH .T.             && permenant record
  
  THIS.AddUserInformation('SYUSTATC')
*  =TABLEUPDATE(0,.T.)  
  =TABLEUPDATE(.T.)  
  THIS.UserStaticRecord = RECNO()
SET REPROCESS TO lcOldRep
ENDPROC
PROCEDURE setmultiappmenu
LPARAMETERS lcModulID,lcSub_Ctg,lcNoShow
    *** Check if building a module menu and the current user has
    *** right limits, add a relation with the user priv. file
    *** and disable the unpermited programs.
    =SEEK(lcModulID,'SYDAPPL')
    lcMastMenu = ALLT(SYDAPPL.CMODPARENT)
    SELECT SYCMENU
    IF THIS.LoginRequired .AND. lcModulID <> "SY" 
      SELECT SYUUSRPR
      SET ORDER TO TAG MDLCMPPRSS
      SET FILTER TO (CUSER_ID=THIS.User_ID  .AND. cgrporuser='U') .OR.;
                    (CUSER_ID=THIS.User_Group .AND. cgrporuser='G')
                  
      SELECT SYCMENU
      SET RELATION TO capp_id+THIS.ActiveCompanyID+cPross_ID+cproctype ;
                   INTO SYUUSRPR ADDITIVE 
    ENDIF

    SET RELATION TO cApp_ID+cPross_ID INTO SYDOBJCT ADDITIVE 
    lnBarsdefiend = 0
    *** Loop for all records in the menu file for this moudle
    SEEK(lcMastMenu)
    SCAN REST WHILE CAPP_ID = lcMastMenu FOR  lcModulID $ cbarModule AND cSub_Ctg = lcSub_Ctg 

      *** If the bar or pad to be defined have no hot key defined in
      *** the menu file it will give an error if we type KEY null
      lcHotKey  = IIF(EMPTY(cSub_hKey),[],[KEY ]+ALLTRIM(cSub_hKey))   
      DO CASE
        *** Check if the type of this line is  pad 
        CASE  ALLTRIM(cSub_Typ) = 'P'
          *** Check if this new pad has not been defined before
          IF TYPE([PRMPAD('_MSYSMENU',ALLTRIM(cMstr_Nam))]) = 'U'
            *** Get the name of the privious pad to define the 
            *** new pad in the right position 
            lcPrivpad = "P"+PADL(INT(VAL(SUBSTR(cMstr_Nam,2))-1),2,'0')

            *** Define new meny pad
            DEFINE PAD    (ALLTRIM(cMstr_Nam)) ;
                   OF     _mSYSMENU ;
                   PROMPT ALLTRIM(cSub_prpt);
                   AFTER &lcPrivpad &lcHotKey

            *** Define new menu popup that going to be opend from this pad
            DEFINE POPUP (ALLTRIM(cPross_ID)) MARGIN SHADOW RELATIVE

            *** link the pad to the popup
            ON PAD (ALLTRIM(cMstr_Nam)) ;
               OF _mSYSMENU ;
               ACTIVATE POPUP (ALLTRIM(cPross_ID))
          ENDIF          
 
        *** Define all menu bars
        OTHERWISE
          *Haytahr [Begin]
          *IF ALLTRIM(cSub_Typ) = 'S' AND lcMastMenu = lcModulID 
          llBarExist = TYPE("PRMBAR(ALLTRIM(cMstr_Nam) , VAL(cBar_pos))") <> "U" .AND.;
                       !EMPTY(PRMBAR(ALLTRIM(cMstr_Nam) , VAL(cBar_pos)))
          *Haytahr [End]

          *** Get the popup and bar name to be send to the gfBarSkip function
          *** to get the right help message fro array
          lcPopBar = ALLTRIM(cMstr_Nam)+"-"+cBar_pos

            IF THIS.LoginRequired
              *** Check if the current user have access to this bar in
              *** the user prevelage file or not
              *** CHECK FOR MODULES HERE            
              IF cSub_Typ <> 'S' .AND. EOF('SYUUSRPR') 
  
                DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
                      PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar;
                      COLOR SCHEME 3 SKIP FOR .T. &lcHotKey
              ELSE

                DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
                      PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar;
                      COLOR SCHEME 3 ;
                      &lcHotKey
              ENDIF
            ELSE
            *** CHECK FOR MODULES HERE          
              DEFINE BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam));
                     PROMPT ALLTRIM(cSub_prpt)  BEFORE lnExsBar;
                     COLOR SCHEME 3 ;
                     &lcHotKey
            ENDIF 

          *** Check if this bar is a branching bar and define new
          *** popup if it is
          *** CHECK FOR MODULES HERE        
          *Haytahr [Begin]
          *IF ALLTRIM(cSub_Typ) = 'S' AND lcMastMenu = lcModulID 
          IF ALLTRIM(cSub_Typ) = 'S' AND !llBarExist
          *Haytahr [End]
            DEFINE POPUP (ALLTRIM(cPross_ID)) ;
                   MARGIN SHADOW RELATIVE

            ON BAR VAL(cBar_pos) OF (ALLTRIM(cMstr_Nam)) ;
               ACTIVATE POPUP (ALLTRIM(cPross_ID))
          ENDIF

        ENDCASE
    ENDSCAN

    *** Now we are going to link all the menu bars to the global procedure
    *** gpMneuBar to excute the selected option
  
    ON SELECTION POPUP ALL DO gfMenuBar WITH POPUP(),BAR()  
  
    SELECT SYCMENU
    SET RELATION TO
  
DO WHILE EMPTY(PRMBAR('P05PU05',GETBAR('P05PU05',1)))
  RELEASE BAR GETBAR('P05PU05',1) OF P05PU05
ENDDO 
DO WHILE EMPTY(PRMBAR('P05PU05',GETBAR('P05PU05',CNTBAR('P05PU05'))))
  RELEASE BAR GETBAR('P05PU05',CNTBAR('P05PU05')) OF P05PU05
ENDDO 

ENDPROC
PROCEDURE getdatadir
*B603122,3  Hesham (Start)
*B603122,3  Make the system use a remote data dir
PARAMETERS lcDataDir
lcDataDir = ALLT(lcDataDir)
lcDataDir = lcDataDir+IIF(RIGHT(lcDataDir,1)='\','','\')
IF !EMPTY(This.DataEnv)
  lcDataDir = This.DataEnv+SUBSTR(lcDataDir,RAT('\',lcDataDir,2)+1)
ELSE
   IF UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) = UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
       UPPER(SUBSTR(THIS.InstallPath,1,ATC('\',THIS.InstallPath,2))) <>  UPPER(SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2)))
     lcDataDir = SUBSTR(THIS.SysPath,1,ATC('\',THIS.SysPath,2))+SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
   ENDIF
ENDIF  
RETURN ALLTRIM(lcDataDir)
*B603122,3  Hesham (End)
ENDPROC
PROCEDURE validkeyvalu
*:***************************************************************************
*: Name       : validkeyvalu
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 09/25/2002
*: Purpose    : Function To check on the Activation key.
*: Due to     : E301874,1
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : This.validkeyvalu
*:***************************************************************************
*:
*-- lccust_id   ==> Variable Hold the Customer Id.
*-- lccust_nam  ==> Variable Hold the Customer Name
*-- lcActKey    ==> Variable Hold the Activation Key.
*-- ldexpr_date ==> Variable Hold the Expiration Date.
*-- lcactplat   ==> Variable Hold the platform.
*-- Define needed Variables
STORE '' TO lccust_id , lccust_nam , lcActKey , lcactplat , lcSysActKy
STORE {} TO ldexpr_date

gcSysHome = THIS.syspath
lcSaveDir = FullPath('')
    
*-- Check on the Path.
IF EMPTY(lcFilePath)
  CD (gcSysHome)
  STORE FOPEN('ACT_KEY.BIN') TO file_handle       && Open the file      
ELSE
  CD (lcFilePath)
  STORE FOPEN(lcFileName) TO file_handle       && Open the new file      
ENDIF
IF file_handle < 0
  *-- Message Text :- Can not open Activation Key file ! System will be terminated
  *-- Button Text  :- OK.
  CD (lcSaveDir)
  RELEASE lcSaveDir
  llError_Local = .T.
  RETURN .T.
ENDIF  
  
STORE FSEEK(file_handle,12876) TO ifp_size	&& Move pointer to EOF
STORE FREAD(file_handle, 76)   TO lcstring  && Store to memory
IF EMPTY(lcstring)
  *-- Message Text :- Invalid Activation Key ! System will be terminated
  *-- Button Text  :- OK.
  CD (lcSaveDir)
  RELEASE lcSaveDir
  =FCLOSE(file_handle)
  llError_Local = .T.
  
  RETURN .T.
ELSE
  llError_Local = .F.
  *-- Get the Customer Id.
  lccust_id  = SUBSTR(lcstring,03,05)
      
  *-- Get the Customer Name.
  lccust_nam = SUBSTR(lcstring,08,030)
    
  *-- Get the Customer activation Key.
  lcActKey   = SUBSTR(lcstring,38,20)
    
  *-- Get the Customer expiration Date.
  ldexpr_date= IIF(EMPTY(SUBSTR(lcstring,58,10)),{},SUBSTR(lcstring,58,10))
    
  *-- Get the Customer Version Type.
  lcactplat  = SUBSTR(lcstring,68,04)
    
  *-- Get the Customer sys(2007) for the current record.    
  lcSysActKy = SUBSTR(lcstring,72,05)
    
  lComparSys = SYS(2007,'NADABDXMLX'+lccust_nam+lcActKey+ldexpr_date+lcactplat)
  
  CD (lcSaveDir)
  RELEASE lcSaveDir
  =FCLOSE(file_handle)
ENDIF
gcCompName  = lccust_nam
gcAct_Key   = lcActKey
gcLicence   = ''
IF  lcSysActKy <> lComparSys
  llError_Local = .T.
  RETURN .T.
ENDIF
*-- this Code For Expiration Date
PRIVATE lcOldSyDat
lcOldSyDat = SET ('DATE')
SET DATE TO AMERICAN
IF ALLTRIM(lcactplat) = 'E' .AND. DATE() > CTOD(ldexpr_date)
  llError_Local = .T.
  Set Date To &lcOldSyDat
  RETURN .T.
ENDIF
Set Date To &lcOldSyDat

*-- Save the New File.
IF !EMPTY(lcFilePath)
  lcOldSafty = SET('SAFETY')
  SET SAFETY OFF 
  Copy File (lcNewFile) To (gcSysHome+'Act_Key.bin')
  SET SAFETY &lcOldSafty
ENDIF  

*-- End OF lfvKeyValu 
*:***************************************************************************
ENDPROC
PROCEDURE activecompanyid_assign
*E038729,1 AMH Read the company connection string
LPARAMETERS vNewVal
*To do: Modify this routine for the Assign method
THIS.activecompanyid = m.vNewVal

_SCREEN.CAPTION = ALLTRIM(THIS.SystemName)+" - "+ALLTRIM(THIS.ActiveModuleName)+;
          IIF(!EMPTY(THIS.ActiveCompanyID),' ('+ALLTRIM(THIS.ActiveCompanyID)+')','')
this.activecompanyconstr = this.mreadconstr(this.activecompanyid)
ENDPROC
PROCEDURE mreadconstr
*E038729,1 AMH Read the company connection string
LPARAMETERS lcCompanyID

LOCAL lnAlias,lcConStr
lcConStr = ""

IF !EMPTY(lcCompanyID)
  lnAlias = SELECT(0)
  IF USED("syccomp")
    SELECT syccomp
    SET ORDER TO cComp_Id
  ELSE
    SELECT 0
    USE (THIS.SysPath+"syccomp") ORDER 1
  ENDIF
  
  IF SEEK(lcCompanyID)
    lcConStr = this.mgenconstr()
  ENDIF
  SELECT (lnAlias)
ENDIF
RETURN lcConStr
ENDPROC
PROCEDURE mgenconstr
*E038729,1 AMH Generate the connection string

LOCAL lcConnStr
lcConnStr = ""

DO CASE
  CASE CCONDRIVER = 'SQL'
    lcConnStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
                ";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)
  CASE CCONDRIVER = 'FOX'
    lcConnStr = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB="+ALLTRIM(CCONDBNAME)+;
                ";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;"
ENDCASE

this.isRemoteComp = !EMPTY(lcConnStr) .AND. !EMPTY(this.lcAria4Class)
RETURN lcConnStr
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
*B803605,1 DO not enable error handler when getting user resource file
IF THIS.ErrorHandlerEnabled
*B803605,1 (End)
*   = gfErrorTrap(nError, cMethod, nLine)
ENDIF   
ENDPROC
PROCEDURE Init
*E038729,1 AMH Add parameter to hold the Aria 4 XP classes folder [Start]
LPARAMETERS lcAria4Class
this.lcAria4Class = lcAria4Class
*E038729,1 AMH [End]

CLOSE DATA ALL

*-- SAVE the foxpro Environment and set it to the system use
THIS.SetEnvironment()
IF this.SystemSetup() 

This.DataEnv = ALLT(GETENV('DATA27'))
IF !EMPTY(This.DataEnv)
  This.DataEnv= IIF(RIGHT(This.DataEnv,1)='\',This.DataEnv,This.DataEnv+'\')
ENDIF

*  IF THIS.LoginRequired AND !THIS.LOGIN()
*     CLOSE DATA ALL
*     THIS.RestoreEnvironment()
*     RETURN .F.
*  ENDIF
  *-- Save current main window title and set
  *-- the new one
*  this.OldWindCaption = _screen.Caption
*  _screen.caption = this.SystemName

 *-- Release all toolbars  
*  this.ReleaseToolBars()  
 *-- Build the system Menu and the Default Module Menu if 
 *-- there is one
*  SET SYSMENU TO
*  THIS.SetMenu('SY','I','NOSHOW')  
*  IF !EMPTY(THIS.UserModuleID)
*    THIS.SetMenu('SY','S')  
*    IF THIS.SetMenu(THIS.UserModuleID,'A')
*      _SCREEN.CAPTION = ALLTRIM(THIS.SystemName)+" - "+ALLTRIM(THIS.ActiveModuleName)+;
*             IIF(!EMPTY(THIS.ActiveCompanyID),' ('+ALLTRIM(THIS.ActiveCompanyID)+')','')
      
*    ENDIF
*  ELSE
*    THIS.SetMenu('SY','S')  
*  ENDIF
ELSE
  THIS.RestoreEnvironment()
  RETURN .F.
ENDIF

ENDPROC
PROCEDURE Destroy
*-- In case of application error, we call the CleanUp method 
*-- to clean up the environment for us. If we are quitting 
*-- normally, then thelIsClean flag will be .T. indicating 
*-- that the CleanUp method has already been executed and 
*-- the environment is clean.
*_SCREEN.REMOVEOBJECT('ARIATITLE')
this.CleanUp()
CLOSE DATA ALL

ENDPROC
PROCEDURE rebuildcomp
*ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu

*** In case of creating the second level of the menu we are
*** going to save the configuration to be restored when changing
*** the module and ther is no need to collect the prosses names

*** Now we are going to collect the companies names and link it's
*** bars to the change company procedure
IF !USED("SYCCOMP")
  SELECT 0
  USE(THIS.SysPath+"SYCCOMP")
ELSE
  SELECT SYCCOMP
ENDIF

SET ORDER TO TAG CCOMP_ID
RELEASE BAR  ALL OF _COMPANIES

lnCompBar  = 0
*** Collect all avalilabe companies to the compamies popup
SCAN
  lnCompBar = lnCompBar + 1

  IF THIS.LoginRequired
    SELECT SYUUSRPR
    LOCATE FOR INLIST(CUSER_ID,THIS.User_ID,THIS.User_Group) .AND.;
      CCOMP_ID = SYCCOMP.CCOMP_ID

    SELECT SYCCOMP

    IF FOUND('SYUUSRPR')
DEFINE BAR lnCompBar OF _COMPANIES PROMPT CCOMP_ID+"-"+ALLTRIM(cCom_Name)
ELSE
DEFINE BAR lnCompBar OF _COMPANIES PROMPT CCOMP_ID+"-"+ALLTRIM(cCom_Name) ;
  SKIP FOR .T.
ENDIF
ELSE
DEFINE BAR lnCompBar OF _COMPANIES PROMPT CCOMP_ID+"-"+ALLTRIM(cCom_Name)
ENDIF


IF THIS.ActiveCompanyID = CCOMP_ID
  SET MARK OF BAR lnCompBar OF _COMPANIES TO .T.
  THIS.CompanyBar = lnCompBar
  THIS.ActiveCompanyName = ALLTRIM(SYCCOMP.cCom_Name)
  THIS.DataDir  = THIS.GetDataDir(ALLTRIM(cCom_dDir))
  THIS.PrntCompanyID = IIF(EMPTY(ccompprnt),CCOMP_ID,ccompprnt)
  THIS.BaseCurrency = IIF(EMPTY(THIS.BaseCurrency),'USDLR',THIS.BaseCurrency)
  THIS.CompanySets(ALLTRIM(SYCCOMP.ccont_code))
ENDIF
lcComp_Id = '"'+CCOMP_ID+'"'
ON SELECTION BAR lnCompBar OF _COMPANIES ;
  DO gfChngComp WITH &lcComp_Id

ENDSCAN


*** Reselect the currend company
SET MARK OF POPUP _COMPANIES TO .F.
IF THIS.CompanyBar > 0
  *** there is posible error in case of deleting the current company
  SET MARK OF BAR THIS.CompanyBar OF _COMPANIES TO .T.
ENDIF
This.ReBldCmp = .F. 

ENDPROC
     �BPROCEDURE getmasterfile
LOCAL lnCount
IF TYPE('THIS.DATAENVIRONMENT')='O'
  IF !EMPTY(THIS.DATAENVIRONMENT.INITIALSELECTEDALIAS)
    RETURN THIS.DATAENVIRONMENT.INITIALSELECTEDALIAS
  ELSE
    lnTotMem = AMEMBERS(aMems,THIS.DataEnvironment,2)
    WITH THIS.DATAENVIRONMENT
      FOR lnCount = 1 TO lnTotMem
        IF UPPER(EVAL("."+aMems[lnCount]+".BaseClass")) = "CURSOR"
          RETURN EVAL("."+aMems[lnCount]+".ALIAS")
        ENDIF
      ENDFOR
    ENDWITH
  ENDIF
ELSE
  RETURN ''
ENDIF

ENDPROC
PROCEDURE refreshall
LPARAMETER oContainer

* Checks for General fields
LOCAL i,oControlParent

IF PARAMETERS() = 0
   m.oControlParent = THISFORMSET
ELSE
	m.oControlParent = m.oContainer
ENDIF
IF TYPE('m.oControlParent')#'O'
  RETURN
ENDIF

DO CASE 
CASE ATC("Formset",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.FormCount
CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.PageCount
CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0
	nCtrlCount = oControlParent.ButtonCount
OTHERWISE
	nCtrlCount = oControlParent.ControlCount
ENDCASE
IF m.nCtrlCount=0
  RETURN
ENDIF
FOR i = 1 TO m.nCtrlCount 
	DO CASE
	CASE ATC("Formset",m.oControlParent.BaseClass)#0
		THIS.RefreshAll(m.oControlParent.forms[m.i])
	
	CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
		THIS.RefreshAll(m.oControlParent.Pages[m.i])
		
	CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0 
		m.oControlParent.Buttons[m.i].REFRESH()

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Optiongroup,Commandgroup")#0 
		THIS.RefreshAll(m.oControlParent.Controls[m.i])

	CASE ATC("Container",m.oControlParent.Controls[m.i].BaseClass) # 0 OR; 
		ATC("Page",m.oControlParent.Controls[m.i].BaseClass) # 0
		THIS.RefreshAll(m.oControlParent.Controls[m.i])

*	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"ListBox,ComboBox,Spinner") # 0 
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"ListBox,Spinner") # 0 
	 		m.oControlParent.Controls[m.i].REFRESH()
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"CheckBox,TextBox,OleBoundControl") # 0
			m.oControlParent.Controls[m.i].REFRESH()
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"EditBox") # 0
		m.oControlParent.Controls[m.i].REFRESH()
*	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Grid") # 0
*		m.oControlParent.Controls[m.i].ReadOnly = !THIS.EditMode
*		m.oControlParent.Controls[m.i].DeleteMark = THIS.EditMode
	ENDCASE
ENDFOR

ENDPROC
PROCEDURE dbgetprop
lParameters tcName,tcType,tcProperty
LOCAL lnCount,lcAlias,lcFieldName,lcTableName,lcDataBase,lcPath,lcRetrunValue
tcName = UPPER(tcName)
tcType = UPPER(tcType)
tcProperty = UPPER(tcProperty)

IF ATC(tcType,"CONNECTION,DATABASE,FIELD,TABLE,VIEW")=0
  RETURN tcName
ENDIF

IF tcType='FIELD'
  IF ATC('.',tcName)>0
    lcAlias = SUBSTR(tcName,1,ATC('.',tcName)-1)
    lcFieldName = SUBSTR(tcName,ATC('.',tcName)+1)
    lcTableName = FULL(DBF(lcAlias))
    IF ATC('\',lcTableName)>0
      lcTableName = SUBSTR(lcTableName,RAT('\',lcTableName)+1)
    ENDIF  
    lcTableName = STRTRAN(lcTableName,'.DBF','')
    tcName = lcTableName+'.'+lcFieldName
  ENDIF
ENDIF

DO CASE
  CASE tcType = 'DATABASE'
    IF DBUSED(tcName)
      SET DATABASE TO (tcName)
    ELSE
      RETURN tcName  
    ENDIF
  CASE tcType $ "FIELD,TABLE,VIEW"
    IF tcType = 'FIELD'
      lcDataBase = CURSORGETPROP("DATABASE",lcAlias)
     ELSE 
       lcDataBase = CURSORGETPROP("DATABASE",tcName)
     ENDIF 
     lcPath =''
     IF ATC('\',lcDataBase)>0
       lcPath = SUBSTR(lcDataBase,1,RAT('\',lcDataBase))
       lcDataBase = STRTRAN(lcDataBase,lcPath,'')
       lcDataBase = STRTRAN(lcDataBase,'.DBC','')
     ENDIF
     IF !DBUSED(lcDataBase)
       OPEN DATABASE (lcPath+lcDataBase)  
     ENDIF
     SET DATABASE TO (lcDataBase)     
ENDCASE
lcRetrunValue = DBGETPROP(tcName,tcType,tcProperty)
RETURN lcRetrunValue
ENDPROC
PROCEDURE translatetoenglish
lParameters tcAlias,tcExpression,tcSeperator
LOCAL lnCount,laExpArray
tcSeperator = IIF(PCOUNT()<3,'+',tcSeperator)
DIME laExpArray[1,1]
STORE '' to laExpArray
=gfSubStr(tcExpression,@laExpArray,tcSeperator)
tcExpression = ''
FOR lnCount = 1 TO ALEN(laExpArray,1)
  lcEngName = THIS.DBGetProp(tcAlias+'.'+ALLTRIM(laExpArray[lnCount,1]),'FIELD','CAPTION')
  IF !EMPTY(lcEngName)
    laExpArray[lnCount,1] = ALLTRIM(lcEngName)
  ENDIF
  tcExpression = tcExpression+IIF(!EMPTY(tcExpression),tcSeperator,'')+laExpArray[lnCount,1]
ENDFOR
RETURN tcExpression
ENDPROC
PROCEDURE setpath
LOCAL lnCount
IF TYPE("oAriaApplication") = "O" AND TYPE('THIS.DATAENVIRONMENT')='O'
    lnTotMem = AMEMBERS(aMems,THIS.DataEnvironment,2)
    WITH THIS.DATAENVIRONMENT
      FOR lnCount = 1 TO lnTotMem
        IF UPPER(EVAL("."+aMems[lnCount]+".BaseClass")) = "CURSOR"
          WITH EVAL("."+aMems[lnCount])          
            IF !EMPTY(.DATABASE)
              IF UPPER(LEFT(.CURSORSOURCE,2))='SY'
                .DATABASE=oAriaApplication.SysPath+'SysFiles.dbc'
              ELSE
                .DATABASE=oAriaApplication.DataDir+'AriaData.dbc'             
              ENDIF
            ELSE
              lcCurSource = .CURSORSOURCE
              lcCurSource = IIF(ATC('\',lcCurSource)=0,lcCurSource,SUBSTR(lcCurSource,RAT('\',lcCurSource)+1))
              .CURSORSOURCE = IIF(UPPER(LEFT(lcCurSource,2))='SY',oAriaApplication.SysPath,oAriaApplication.DataDir;
                              )+lcCurSource
            ENDIF  
          ENDWITH 
        ENDIF
      ENDFOR
    ENDWITH
ENDIF   

ENDPROC
PROCEDURE seekrecord
*B803625,1 Hassan Add new parameter
*lParameters tcSeekExpression
lParameters tcSeekExpression , lcMessage
*B803625,1 Hassan END
Local lcMasterFile,lcOldAlias,laKeyFields[1,1],lnSubStrStart,;
      lcKeyValue,lnCount,lcKeyExpression

IF EMPTY(tcSeekExpression)
  RETURN 0
ENDIF

STORE SPACE(0) TO laKeyFields
lcOldAlias   = SELECT()
lcMasterFile = THIS.GETMASTERFILE()

IF EMPTY(lcMasterFile)
  SELECT (lcOldAlias)
  RETURN 0
ELSE
  SELECT (lcMasterFile)
  lcKeyExpression = SYS(14,VAL(SYS(21)))
  = TableRevert(.T.)
  IF SEEK(tcSeekExpression)
    THIS.TopFile = .F.
    THIS.EndFile = .F.
    IF EMPTY(THIS.FormHasToolBar)
      This.ChangeMode('V')        
      This.REFRESHALL()      
      This.REFRESH()  
    ELSE
      oAriaApplication.oToolBar.TopFile = This.TopFile
      oAriaApplication.oToolBar.EndFile = This.EndFile
      This.ChangeMode('V')
      oAriaApplication.oToolBar.ButtonRefresh()
      oAriaApplication.oToolBar.NavRefresh()
    ENDIF
    SELECT (lcOldAlias)
    RETURN 1
  ELSE
    *B803625 HASSAN To change the message [Begin]
*!*	    DO CASE
*!*	      CASE !EMPTY(THIS.FormHasToolBar) AND oAriaApplication.oToolbar.cmdAdd.ControlEnable = 0
*!*	        *-- < Browse > < Add > < Reenter >
*!*	        lnOption = oAriaApplication.MessageBox('QRM00001B00001','Dialog',"'"+ALLTRIM(tcSeekExpression)+"'")
*!*	      CASE (!EMPTY(THIS.FormHasToolBar) AND oAriaApplication.oToolbar.cmdAdd.ControlEnable=1) OR THIS.AllowAdd = .F.
*!*	        *-- < Browse > < Reenter >
*!*	        lnOption = oAriaApplication.MessageBox('QRM00001B00014','Dialog',"'"+ALLTRIM(tcSeekExpression)+"'")
*!*	        lnOption = IIF(lnOption=2,3,lnOption)
*!*	    ENDCASE
    DO CASE
      CASE !EMPTY(THIS.FormHasToolBar) AND oAriaApplication.oToolbar.cmdAdd.ControlEnable = 0
        *-- < Browse > < Add > < Reenter >
         *lnOption = oAriaApplication.MessageBox('QRM00001B00001','Dialog',"'"+ALLTRIM(tcSeekExpression)+"'")
        IF TYPE('lcMessage') = "L"
          lnOption = oAriaApplication.MessageBox('QRM00001B00001' , 'Dialog' , "'" + ALLTRIM(tcSeekExpression) + "'" )
        ELSE
          lnOption = oAriaApplication.MessageBox('QRM00001B00001' , 'Dialog' , ALLTRIM(lcMessage) )
        ENDIF  
        
      CASE (!EMPTY(THIS.FormHasToolBar) AND oAriaApplication.oToolbar.cmdAdd.ControlEnable=1) OR THIS.AllowAdd = .F.
        *-- < Browse > < Reenter >
        IF TYPE('lcMessage') = "L"
          lnOption = oAriaApplication.MessageBox('QRM00001B00014','Dialog' , "'" + ALLTRIM(tcSeekExpression) + "'" )
        ELSE
          lnOption = oAriaApplication.MessageBox('QRM00001B00014','Dialog' , ALLTRIM(lcMessage))
        ENDIF  
        lnOption = IIF(lnOption=2,3,lnOption)
    ENDCASE
    *B803625 HASSAN To change the message [End]     
    DO CASE 
      *-- Browse
      CASE lnOption = 1
        llSelected = oAriaApplication.oToolBar.cmdFind.Click()
        SELECT (lcOldAlias)
        RETURN IIF(llSelected, 1, 0)

      *-- Add
      CASE lnOption = 2
        = gfSubStr(lcKeyExpression,@laKeyFields,'+')
        THIS.ADDNEW()
        IF THIS.ActiveMode = 'A'
          lnSubStrStart = 1
          FOR lnCount = 1 TO ALEN(laKeyFields,1)
            lcKeyValue = SUBSTR(tcSeekExpression,lnSubStrStart,LEN(EVAL(laKeyFields[lnCount,1])))
            REPLACE &laKeyFields[lnCount,1] WITH lcKeyValue
            lnSubStrStart = lnSubStrStart + LEN(EVAL(laKeyFields[lnCount,1]))
          ENDFOR  
        ENDIF
        THIS.REFRESHALL()
        SELECT (lcOldAlias)
        RETURN 1

      *-- Reenter
      CASE lnOption = 3
        *-- ABF
        SELECT (lcOldAlias)
        RETURN 0
        *-- ABF End.
    ENDCASE    
  ENDIF
ENDIF  

SELECT (lcOldAlias)
ENDPROC
PROCEDURE gotop
SELECT (THIS.nWorkArea)
LOCATE
THIS.TopFile = .T.
THIS.EndFile = EOF()
IF !EMPTY(THIS.FormHasToolBar)
  oAriaApplication.oToolBar.TopFile = THIS.TopFile
  oAriaApplication.oToolBar.EndFile = THIS.EndFile
ENDIF

ENDPROC
PROCEDURE goprevious
SELECT (THIS.nWorkArea)
IF !BOF()
  SKIP -1
ENDIF
THIS.TopFile = BOF()
THIS.EndFile = EOF()
IF !EMPTY(THIS.FormHasToolBar)
  oAriaApplication.oToolBar.TopFile = THIS.TopFile   
  oAriaApplication.oToolBar.EndFile = THIS.EndFile 
ENDIF  

ENDPROC
PROCEDURE gonext
SELECT (THIS.nWorkArea)
IF !EOF()
  SKIP 1
ENDIF
THIS.EndFile = EOF()
THIS.TopFile = BOF()
IF !EMPTY(THIS.FormHasToolBar)
  oAriaApplication.oToolBar.EndFile = THIS.EndFile
  oAriaApplication.oToolBar.TopFile = THIS.TopFile
ENDIF  

ENDPROC
PROCEDURE goend
SELECT (THIS.nWorkArea)
GO BOTTOM
THIS.TopFile = .F.
THIS.EndFile = .T.
IF !EMPTY(THIS.FormHasToolBar)
  oAriaApplication.oToolBar.TopFile = THIS.TopFile
  oAriaApplication.oToolBar.EndFile = THIS.EndFile
ENDIF

ENDPROC
PROCEDURE close
*IF THIS.QueryUnload()
*  IF !EMPTY(THIS.FormHasToolBar)
*    oAriaApplication.oToolBar.Init()
*  ENDIF
  THIS.RELEASE
*ENDIF

ENDPROC
PROCEDURE delete
#DEFINE MSGBOX_YES		6
#DEFINE C_MSGBOX1		36
#DEFINE C_DELETE_LOC	"Do you want to delete this record?"
#DEFINE C_NOLOCK_LOC	"Record could not be deleted because it is being used by someone else."

* Note: Cascading deletes should be handled via RI triggers in DBC!
IF MESSAGEBOX(C_DELETE_LOC,C_MSGBOX1,_screen.Caption) = MSGBOX_YES
  SELECT (This.nWorkArea)
  DELETE
  IF THIS.SaveFiles()  &&success
    * Success
    IF !EOF()
      SKIP 1
    ENDIF
    IF EOF() AND !BOF()
      SKIP -1
    ENDIF
  ENDIF
ENDIF

ENDPROC
PROCEDURE addnew
#DEFINE C_NOUPDATE_LOC	"You cannot add a new record because the view(s) selected does not send updates."
** Code for adding record
SELECT (THIS.nWorkArea)
THIS.OldAlias = ALIAS()	&&save alias in case reverting
THIS.OldRec = RECNO()	&&save record in case reverting
IF CURSORGETPROP("SourceType")#3 AND !CURSORGETPROP("SendUpdates")
  =MESSAGEBOX(C_NOUPDATE_LOC,0,_screen.Caption)
  RETURN
ENDIF
APPEND BLANK
THIS.TopFile = .F.
This.ChangeMode("A")

ENDPROC
PROCEDURE savefiles
#DEFINE	E_FAIL_LOC			"Failed to update table: "
#DEFINE	E_TRIGGERFAIL_LOC	"Trigger failed."
#DEFINE	E_FIELDNULL_LOC		"Field doesn't accept NULL"
#DEFINE	E_FIELDRULE_LOC		"Field rule violated"
#DEFINE	E_RECORDLOCK_LOC	"Record in use by another user"
#DEFINE	E_ROWRULE_LOC		"Row rule violated"
#DEFINE	E_UNIQUEINDEX_LOC	"Unique index violation"
#DEFINE	E_DIRTYREC_LOC		"Data has been changed by another user. Overwrite changes with your edits?"
#DEFINE	E_NOFORCE_LOC		"Could not force table updates."
#DEFINE E_PROMPT_LOC	 	"Error: "
#DEFINE MSGBOX_YES			6

LOCAL aErrors,cErrorMessage,aTablesUsed,nTablesUsed,nTotErr 
LOCAL nFld,i,nOldArea,lSuccess,lInDBC,lOverwrite,lHadMessage

DIMENSION aTablesUsed[1]
DIMENSION aErrors[1]
m.cErrorMessage=""
m.lSuccess = .T.
m.nOldArea = SELECT()
m.nTablesUsed = AUSED(aTablesUsed)

* Can wrap everything in transaction if using strictly DBCs

FOR i = 1 TO m.nTablesUsed

	SELECT (aTablesUsed[m.i,1])

	m.lInDBC = !EMPTY(CURSORGETPROP("Database"))
	m.cErrorMessage = ""
	m.lOverwrite = .F.
	m.lHadMessage = .F.

	DO CASE
  	CASE CURSORGETPROP("Buffering") = 1
	  	* Skip if buffering not on
  		LOOP
  	CASE GetFldState(0) = 2			&&deleted record
		  * Only delete current record and force it
  		m.lSuccess = TableUpdate(.F.,.T.)
  		IF m.lSuccess				&&successful update
  			LOOP
  		ENDIF
  	CASE !m.lInDBC AND (ATC("2",GetFldState(-1))#0 OR;
	  	 ATC("3",GetFldState(-1))#0)	
	  	* Field was edited - in Free Table
  		* Since free tables are not supported by transactions,
		  * we must process record by record
  		m.nModRecord = GetNextMod(0)
  		DO WHILE m.nModRecord # 0	&&loop locks all records
		  	GO m.nModRecord
  			m.lSuccess = RLOCK()	&&try to lock record
	  		IF !m.lSuccess			&&failed to lock record
		  		m.cErrorMessage = E_RECORDLOCK_LOC
  				UNLOCK ALL
		  		EXIT
  			ENDIF
  			IF !m.lHadMessage	&&so we don't repeat alert
				  * See if record(s) modified by another user
  				FOR m.nFld = 1 TO FCOUNT()
		  			IF TYPE(FIELD(m.nFld)) = "G"	&&skip for General fields
  						LOOP					
  					ENDIF
			  		IF OLDVAL(FIELD(m.nFld)) # CURVAL(FIELD(m.nFld))
		  				m.lHadMessage = .T.
				  		IF MESSAGEBOX(E_DIRTYREC_LOC,4+48,_screen.Caption) = MSGBOX_YES
		  					m.lOverwrite = .T.
					  	ELSE
						  	m.lSuccess = .F.
  							UNLOCK ALL
				  			EXIT
  						ENDIF
  					ENDIF
  				ENDFOR
  			ENDIF
  			m.nModRecord = GetNextMod(m.nModRecord)
  		ENDDO
  		IF m.lSuccess 	&&was able to lock all records
	    	m.lSuccess = TableUpdate(.T.,m.lOverwrite)
		  	IF m.lSuccess &&was able to update all records
  				LOOP
			  ENDIF	
  			UNLOCK ALL
  		ENDIF
  	CASE m.lInDBC
		  BEGIN TRANSACTION
  		* Try to update all records in selected table
		  m.lSuccess = TableUpdate(.T.,.F.)	&&successful update
  		IF m.lSuccess
	  		END TRANSACTION
  			LOOP
  		ENDIF
		  ROLLBACK
	ENDCASE

	* Handle errors
	nTotErr =AERROR(aErrors)
	DO CASE
	CASE nTotErr = 0
		
	CASE aErrors[1,1] = 1539				&& Trigger failed
		m.cErrorMessage = E_TRIGGERFAIL_LOC
	CASE aErrors[1,1] = 1581				&& Field doesn't accept NULL
		m.cErrorMessage = E_FIELDNULL_LOC
	CASE aErrors[1,1] = 1582				&& Field rule violated
		m.cErrorMessage = E_FIELDRULE_LOC
	CASE aErrors[1,1] = 1700				&& Record in use by another user
		m.cErrorMessage = E_RECORDLOCK_LOC
	CASE aErrors[1,1] = 1583				&& Row rule violated
		m.cErrorMessage = E_ROWRULE_LOC
	CASE aErrors[1,1] = 1884				&& Unique index violation
		m.cErrorMessage = E_UNIQUEINDEX_LOC
	CASE aErrors[1,1] = 1585				&& Record changed by another user

		IF m.lInDBC		&&handle free tables above
			* Dislpay conflict alert
			IF MESSAGEBOX(E_DIRTYREC_LOC,4+48,_screen.Caption) = MSGBOX_YES
				*Try to force update
				BEGIN TRANSACTION				
				m.lSuccess = TABLEUPDATE(.T.,.T.)
				IF m.lSuccess
					END TRANSACTION
					LOOP
				ELSE
					ROLLBACK
					=MESSAGEBOX(E_NOFORCE_LOC,0,_screen.Caption)
				ENDIF			
			ENDIF
		ENDIF

	OTHERWISE
		IF !EMPTY(m.cErrorMessage)	&&for free table handling above
			m.cErrorMessage = E_PROMPT_LOC+aErrors[1,2]
		ENDIF
	ENDCASE

	* Had an error we couldn't handle
	=TABLEREVERT(.T.)  &&revert all records
	m.lSuccess = .F.
	IF !EMPTY(m.cErrorMessage)
		=MESSAGEBOX(E_FAIL_LOC+m.cErrorMessage,0,_screen.Caption)
	ENDIF

ENDFOR
*E000000,1 Hesham (Start)
THIS.ChangeMode(SUBSTR('VS',ATC(THIS.ActiveMode,'EA'),1))
THIS.TopFile = .F.
*THIS.EditMode = !THIS.EditMode
*THIS.AddMode = THIS.EditMode
*THIS.SelectMode = THIS.AddMode
*DO CASE
*  CASE THIS.ADDMODE AND THIS.SELECTMODE
*    THIS.ActiveMode = 'S'
*  CASE THIS.ADDMODE AND THIS.EDITMODE
*    THIS.ActiveMode = 'A'
*  CASE THIS.EDITMODE AND !THIS.ADDMODE
*    THIS.ActiveMode = 'E'
*  OTHERWISE
*    THIS.ActiveMode = 'V'    
*ENDCASE    
*E000000,1 Hesham (End)

SELECT (m.nOldArea)
RETURN m.lSuccess

ENDPROC
PROCEDURE edit
THIS.OldAlias = ALIAS()	&&save alias in case reverting
THIS.OldRec = RECNO()	&&save record in case reverting
*E000000,1 Hesham (Start)
THIS.ChangeMode('E')
*THIS.EditMode = !THIS.EditMode
*THIS.AddMode = .F.
*THIS.SelectMode = .F.
*DO CASE
*  CASE THIS.ADDMODE AND THIS.SELECTMODE
*    THIS.ActiveMode = 'S'
*  CASE THIS.ADDMODE AND THIS.EDITMODE
*    THIS.ActiveMode = 'A'
*  CASE THIS.EDITMODE AND !THIS.ADDMODE
*    THIS.ActiveMode = 'E'
*  OTHERWISE
*    THIS.ActiveMode = 'V'    
*ENDCASE    
*E000000,1 Hesham (END)
IF !EMPTY(THIS.FormHasToolBar)
  oAriaApplication.oToolBar.ActiveMode = THIS.ACTIVEMODE
ENDIF    

ENDPROC
PROCEDURE undo
LPARAMETERS tlSaveWithoutAsk
IF !tlSaveWithoutAsk
  IF THIS.FileWasChanged()
    IF MESSAGEBOX('Are you sure you want to lose all your changes?',4+16+256,_SCREEN.CAPTION)=7
      RETURN
    ENDIF
  ENDIF
ENDIF
LOCAL lnTotMem,lnCount,laMems,aTablesUsed,nTablesUsed
IF THIS.UseDataEnv
  DIME laMems[1,1]
  lnTotMem = AMEMBERS(laMems,THIS.DATAENVIRONMENT,2)
  WITH THIS.DATAENVIRONMENT
    * Check for Views
    FOR lnCount = 1 TO lnTotMem
      IF UPPER(EVAL("."+laMems[lnCount]+".BaseClass")) = "CURSOR"
        WITH EVAL("."+laMems[lnCount])
          IF CURSORGETPROP('Buffering',.ALIAS)>1
            =TABLEREVERT(.T.,.ALIAS)
          ENDIF
        ENDWITH
      ENDIF
    ENDFOR
  ENDWITH
ELSE
  DIMENSION aTablesUsed[1]
  m.nTablesUsed = AUSED(aTablesUsed)
  FOR lnCount = 1 TO m.nTablesUsed
    IF CURSORGETPROP('Buffering',lnCount)>1
      *B605168,1 AMM (Start) don't update the file that you have just checked
      *=TableRevert(.T.,aTablesUsed[lnCount,1])
      =TABLEREVERT(.T.,lnCount)
      *B605168,1 AMM end
    ENDIF
  ENDFOR
ENDIF

* Go back to original place
*E000000,1 HESHAM (start)
THIS.ChangeMode(SUBSTR('VS',ATC(THIS.ActiveMode,'EA'),1))
*IF !THIS.ADDMODE
*  SELECT (THIS.OldAlias)
*  IF RECCOUNT() < THIS.OldRec	&&added record at EOF()
*    GO TOP
*  ELSE
*    GO THIS.OldRec
*  ENDIF
*ENDIF

** Editing record
*THIS.EditMode = !THIS.EditMode
*THIS.AddMode = .F.
*THIS.SelectMode = .F.
*DO CASE
*  CASE THIS.ADDMODE AND THIS.SELECTMODE
*    THIS.ActiveMode = 'S'
*  CASE THIS.ADDMODE AND THIS.EDITMODE
*    THIS.ActiveMode = 'A'
*  CASE THIS.EDITMODE AND !THIS.ADDMODE
*    THIS.ActiveMode = 'E'
*  OTHERWISE
*    THIS.ActiveMode = 'V'
*ENDCASE
*E000000,1 HESHAM (start)
IF !EMPTY(THIS.FormHasToolBar)
  oAriaApplication.oToolBar.ActiveMode = THIS.ActiveMode
  *126224,1 RRE fix the wrong in the toolbar when pressing escape [Begin]
  IF TYPE('oAriaApplication.oToolBar')='O'
    THISFORMSET.ariaform1.LOCKSCREEN = .T.
    oAriaApplication.oToolBar.ButtonRefresh()
    oAriaApplication.oToolBar.NavRefresh()
    THISFORMSET.ariaform1.LOCKSCREEN = .F.
  ENDIF
  *126224,1 RRE fix the wrong in the toolbar when pressing escape [END  ]

ENDIF



ENDPROC
PROCEDURE filewaschanged
Local aTablesUsed,lnTablesUsed,lnCount,nTotMem,i
IF THIS.UseDataEnv
  nTotMem = AMEMBERS(aMems,THIS.DataEnvironment,2)	
  WITH THIS.DataEnvironment		
    * Check for Views
    FOR i = 1 TO m.nTotMem
      IF UPPER(EVAL("."+aMems[m.i]+".BaseClass")) = "CURSOR"
        WITH EVAL("."+aMems[m.i])
        lnTableBuff = CURSORGETPROP('Buffering',.Alias)
          DO CASE
            CASE BETWEEN(lnTableBuff,2,3)
              IF !(GETFLDSTATE(-1,.Alias)== '1'+REPLICATE('1',FCOUNT(.Alias))) AND;
               !(GETFLDSTATE(-1,.Alias)== '3'+REPLICATE('3',FCOUNT(.Alias)))            
                RETURN .T.
              ENDIF
            CASE BETWEEN(lnTableBuff,4,5)
              m.nModRecord = GetNextMod(0,.Alias)
     	      IF m.nModRecord # 0	          
   	  	        RETURN .T.
   		      ENDIF
          ENDCASE
        ENDWITH
      ENDIF
    ENDFOR
  ENDWITH
ELSE
  DIMEN aTablesUsed[1]
  lnTablesUsed = AUSED(aTablesUsed)  
  FOR lnCount = 1 TO lnTablesUsed
        lnTableBuff = CURSORGETPROP('Buffering',lnCount)
          DO CASE
            CASE BETWEEN(lnTableBuff,2,3)
              IF !(GETFLDSTATE(-1,lnCount)== '1'+REPLICATE('1',FCOUNT(lnCount))) AND;
               !(GETFLDSTATE(-1,lnCount)== '3'+REPLICATE('3',FCOUNT(lnCount)))            
                RETURN .T.
              ENDIF
            CASE BETWEEN(lnTableBuff,4,5)
              m.nModRecord = GetNextMod(0,lnCount)
     	      IF m.nModRecord # 0	          
   	  	        RETURN .T.
   		      ENDIF
          ENDCASE
  ENDFOR
ENDIF	
RETURN .F.

ENDPROC
PROCEDURE cancontinue
IF THIS.FileWasChanged()
  lnOption=MessageBox('You will lose all your changes.'+CHR(13)+CHR(10)+;
        'Do you want to save changes before closing?',35,_screen.Caption)
  DO CASE
    CASE lnOption=6
      THIS.SaveFiles()
    CASE lnOption=7
      this.Undo(.T.)
    OTHERWISE
      RETURN .F.
  ENDCASE
ENDIF  
RETURN .T.
ENDPROC
PROCEDURE opentables
LOCAL lnCount
SET MULTI ON
SET DATASESSION TO THIS.DataSessionID
IF TYPE('THIS.DATAENVIRONMENT')='O'
    lnTotMem = AMEMBERS(aMems,THIS.DataEnvironment,2)
    WITH THIS.DATAENVIRONMENT
      FOR lnCount = 1 TO lnTotMem
        IF UPPER(EVAL("."+aMems[lnCount]+".BaseClass")) = "CURSOR"
        lcFileName = ''
        lcTag = ''
        lcAlias = ''
        lcFilter = ''
        lnBufferMode = 1
          WITH EVAL("."+aMems[lnCount])          
          lcFileName = SUBSTR(.CURSORSOURCE,RAT('\',.CURSORSOURCE)+1)
          lcAlias = .ALIAS
          lcFilter = .Filter
          lnBuffermode = .buffermodeoverride
          IF !EMPTY(.ORDER)
           lcTag = 'ORDER TAG '+.ORDER
          ENDIF 
          ENDWITH 
          THIS.DATAENVIRONMENT.REMOVEOBJECT(aMems[lnCount])
          IF LEFT(UPPER(lcFileName),2) = 'SY'
            IF !USED(lcAlias)
              USE (oAriaApplication.SysPath+lcFileName) AGAIN ALIAS (lcAlias) IN 0 &lcTag
            ENDIF  
          ELSE  
            IF !USED(lcAlias)        
              USE (oAriaApplication.DataDir+lcFileName) AGAIN ALIAS (lcAlias) IN 0 &lcTag        
            ENDIF  
          ENDIF
          IF !EMPTY(lcFilter)
            SUSP
            SELECT (lcAlias)
            SET FILTER TO &lcFilter
          ENDIF
          =CURSORSETPROP('BUFFERING',lnBufferMode,lcAlias)
          ENDIF
      ENDFOR
    ENDWITH
ENDIF   
IF TYPE('THIS.DATAENVIRONMENT')='O'
  lnTotMem = AMEMBERS(aMems,THIS.DataEnvironment,2)
  WITH THIS.DATAENVIRONMENT
    FOR lnCount = 1 TO lnTotMem
      IF UPPER(EVAL("."+aMems[lnCount]+".BaseClass")) = "RELATION"
        WITH EVAL("."+aMems[lnCount])
          SET RELATION TO (.RelationalExpr) INTO (.ChildAlias) IN (.ParentAlias) ADDI
          IF .OneToMany
            lcAlias = SELECT()
            SELECT (.ParentAlias)
            SET SKIP TO (.ChildAlias)
            SELECT (lcAlias)
          ENDIF
        ENDWITH
        THIS.DATAENVIRONMENT.REMOVEOBJECT(aMems[lnCount])
      ENDIF
    ENDFOR
  ENDWITH    
ENDIF        
ENDPROC
PROCEDURE changemode
lParameters lcModeToChange
lcModeToChange = IIF(TYPE('lcModeToChange')='C',lcModeToChange,'')
IF !EMPTY(lcModeToChange)
  THIS.ActiveMode = lcModeToChange
  THIS.SetAllProp()
  IF !EMPTY(THIS.FormHasToolBar)
    oAriaApplication.oToolBar.ActiveMode = THIS.ACTIVEMODE
  ENDIF    
ENDIF


ENDPROC
PROCEDURE print
LPARAMETERS cText
#DEFINE C_MAKEREPO_LOC			"Could not locate a report to print. Create new one?"
#DEFINE C_NOOPEN_LOC			"Error opening table. Unable to print report."
#DEFINE C_GETFILEPROMPT_LOC		"Select a report to print:"
LOCAL cRepName,nSaveSess,cSaveAlias,cSaveSource,cSaveData 
* B804303 ,1 Hassan 09/16/2001 comment out, no need to it (Begin)
*THIS.VALUE=0
* B804303 ,1 Hassan  End

cSaveAlias = ALIAS()
cSaveSource = CURSORGETPROP("SourceName")
cSaveData = CURSORGETPROP("Database")
cDiffSource = ""
cRepName = LEFT(ALIAS(),8)+".FRX"
nSaveSess = SET("DATASESSION")

#IF 0
	* Handling for Private data sessions
	IF m.nSaveSess # 1
		SET DATASESSION TO 1
		SELECT 0
		IF !EMPTY(m.cSaveData)
			OPEN DATABASE (m.cSaveData)
		ENDIF
		IF USED(m.cSaveAlias)
			SELECT (m.cSaveAlias)
			IF CURSORGETPROP("SourceName")#m.cSaveSource
				cDiffSource = CURSORGETPROP("SourceName")
				USE IN (m.cSaveAlias)
				SELECT 0
			ENDIF
		ENDIF	
		IF EMPTY(ALIAS())
			USE (m.cSaveSource) AGAIN ALIAS (m.cSaveAlias) SHARED
			IF EMPTY(ALIAS())
				=MESSAGEBOX(C_NOOPEN_LOC,0,_screen.Caption)
				RETURN
			ENDIF
		ENDIF
	ENDIF
#ENDIF

IF FILE(m.cRepName)
	REPORT FORM (m.cRepName) PREVIEW NOWAIT
ELSE
	m.cRepName = GETFILE("frx",C_GETFILEPROMPT_LOC,"",1)
	IF !EMPTY(m.cRepName)
		IF FILE(m.cRepName)
			* User pressed Open button
			REPORT FORM (m.cRepName) PREVIEW NOWAIT
		ELSE
			* User pressed New button
			DO (_WIZARD) WITH "AUTOREPORT"
		ENDIF
	ENDIF
ENDIF

#IF 0
	IF !EMPTY(cDiffSource)
		USE (m.cDiffSource) IN 0
	ENDIF

	SET DATASESSION TO m.nSaveSess
	SELECT (m.cSaveAlias)
#ENDIF
ENDPROC
PROCEDURE setallprop
LPARAMETER oContainer
* Checks for General fields
LOCAL i,oControlParent

IF PARAMETERS() = 0
   m.oControlParent = THIS
ELSE
	m.oControlParent = m.oContainer
ENDIF
IF TYPE('m.oControlParent')#'O'
  RETURN
ENDIF
DO CASE 
CASE ATC("FormSet",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.FormCount

CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.PageCount
CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0
	nCtrlCount = oControlParent.ButtonCount
OTHERWISE
	nCtrlCount = oControlParent.ControlCount
ENDCASE
FOR i = 1 TO m.nCtrlCount 
  DO CASE
    CASE TYPE('m.oControlParent.Controls[m.i].ControlSource')#'U'  AND TYPE('m.oControlParent.Controls[m.i].AriaControlSource')#'U'
      IF !EMPTY(m.oControlParent.Controls[m.i].AriaControlSource) AND EMPTY( m.oControlParent.Controls[m.i].ControlSource )
        m.oControlParent.Controls[m.i].ControlSource = m.oControlParent.Controls[m.i].AriaControlSource
      ENDIF  
  ENDCASE
	DO CASE
  	CASE ATC("FormSet",m.oControlParent.BaseClass)#0
	  	THIS.SetAllProp(m.oControlParent.Forms[m.i])
	
  	CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
	  	THIS.SetAllProp(m.oControlParent.Pages[m.i])
		
  	CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0 
	    IF ATC(m.oControlParent.Buttons[m.i].BaseClass,"Commandbutton")#0 
	      IF TYPE('m.oControlParent.Buttons[m.i].BtnType')#'U'
  	      DO CASE 
            CASE m.oControlParent.Buttons[m.i].BtnType = 'K'
  	  	      m.oControlParent.Buttons[m.i].Enabled = THIS.ACTIVEMODE = 'S'
   	       CASE m.oControlParent.Buttons[m.i].BtnType = 'B'
      	      m.oControlParent.Buttons[m.i].Enabled = THIS.ACTIVEMODE $ 'EAV'
     	     OTHERWISE
   	         m.oControlParent.Buttons[m.i].Enabled = THIS.ACTIVEMODE $ 'EA'   		      
     		 ENDCASE    
  	    ELSE
    		  m.oControlParent.Buttons[m.i].Enabled = THIS.ActiveMode$'AE' &&THIS.EditMode AND !THIS.ADDMODE
    		ENDIF  
      ELSE 
  		  m.oControlParent.Buttons[m.i].Enabled = THIS.ActiveMode$'AE' &&THIS.EditMode AND !THIS.ADDMODE
      ENDIF
	  CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Optiongroup,Commandgroup")#0 
		  THIS.SetAllProp(m.oControlParent.Controls[m.i])

  	CASE ATC("Container",m.oControlParent.Controls[m.i].BaseClass) # 0 OR; 
	  	ATC("Page",m.oControlParent.Controls[m.i].BaseClass) # 0
  		THIS.SetAllProp(m.oControlParent.Controls[m.i])

  	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"ListBox,ComboBox,Spinner") # 0 
      IF UPPER(m.oControlParent.Controls[m.i].ControlSource) $ UPPER(THIS.KEY)
        IF This.ActiveMode = 'S' 
    			m.oControlParent.Controls[m.i].Enabled = .T.
		    ELSE
  		  	m.oControlParent.Controls[m.i].Enabled = .f.
		    ENDIF	
      ELSE
  			m.oControlParent.Controls[m.i].Enabled = THIS.ACTIVEMODE $ 'AE' &&THIS.EditMode AND !THIS.ADDMODE
  		ENDIF
  	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"CheckBox,TextBox,OleBoundControl") # 0
	      IF UPPER(m.oControlParent.Controls[m.i].ControlSource) $ UPPER(THIS.KEY )
	        IF THIS.ACTIVEMODE = 'S' &&This.AddMode
      			m.oControlParent.Controls[m.i].Enabled = .T.
    		  ELSE
			      m.oControlParent.Controls[m.i].Enabled = .f.
    		  ENDIF	
  	    ELSE
	    		m.oControlParent.Controls[m.i].Enabled = THIS.ACTIVEMODE $ 'AE' &&THIS.EditMode AND !THIS.ADDMODE
   		 ENDIF
    CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Commandbutton")#0 
	    IF TYPE('m.oControlParent.Controls[m.i].BtnType')#'U'
  	    DO CASE 
  	      CASE m.oControlParent.Controls[m.i].BtnType = 'K'
            m.oControlParent.Controls[m.i].Enabled = THIS.ACTIVEMODE = 'S'
   	     CASE m.oControlParent.Controls[m.i].BtnType = 'B'
   	       m.oControlParent.Controls[m.i].Enabled = THIS.ACTIVEMODE $ 'EAV'
   	     OTHERWISE
   	       m.oControlParent.Controls[m.i].Enabled = THIS.ACTIVEMODE $ 'EA'   		      
   		 ENDCASE    
	    ELSE
  		  m.oControlParent.Controls[m.i].Enabled = THIS.ActiveMode$'AE' &&THIS.EditMode AND !THIS.ADDMODE
  		ENDIF  
	  CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"EditBox") # 0
    	m.oControlParent.Controls[m.i].ReadOnly = THIS.ActiveMode$'SV'
*    	m.oControlParent.Controls[m.i].ENABLED = THIS.ActiveMode$'EA'    	
		  IF !THIS.HasMemo
  			WITH m.oControlParent.Controls[m.i]
		  		THIS.EditForeColor = .ForeColor
				  THIS.EditDisForeColor =  .DisabledForeColor
  				THIS.EditBackColor = .BackColor
		  		THIS.EditDisBackColor =  .DisabledBackColor
				  THIS.HasMemo = .T.
  			ENDWITH
  		ENDIF
  		m.oControlParent.Controls[m.i].ForeColor = IIF(THIS.ActiveMode$'EA' ,THIS.EditForeColor,THIS.EditDisForeColor)
		  m.oControlParent.Controls[m.i].BackColor = IIF(THIS.ActiveMode$'EA' ,THIS.EditBackColor,THIS.EditDisBackColor)

  	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Grid") # 0
  		m.oControlParent.Controls[m.i].ReadOnly = THIS.ActiveMode$'SV' &&!THIS.EditMode
	ENDCASE
	DO CASE
    CASE TYPE('m.oControlParent.Controls[m.i].ControlSource')#'U' AND TYPE('m.oControlParent.Controls[m.i].AriaControlSource')#'U'
      IF This.ActiveMode = 'S'
        IF EMPTY(m.oControlParent.Controls[m.i].AriaControlSource)
          m.oControlParent.Controls[m.i].AriaControlSource = m.oControlParent.Controls[m.i].ControlSource 
        ENDIF  
        IF !EMPTY(m.oControlParent.Controls[m.i].ControlSource)
          m.oControlParent.Controls[m.i].ControlSource = ''
        ENDIF
        IF TYPE('m.oControlParent.Controls[m.i].Value')#'U'
          m.oControlParent.Controls[m.i].ResetToDefault('Value')
        ENDIF
      ELSE  
        IF EMPTY(m.oControlParent.Controls[m.i].ControlSource) AND !EMPTY(m.oControlParent.Controls[m.i].AriaControlSource)
          m.oControlParent.Controls[m.i].ControlSource  = m.oControlParent.Controls[m.i].AriaControlSource  
        ENDIF  
      ENDIF
    CASE TYPE('m.oControlParent.Controls[m.i]')#'U' AND ATC(m.oControlParent.Controls[m.i].baseclass ,'Grid')#0
      LOCAL lnCount,lcColCont,lcBound,lcContSource,lcSparse
      IF This.ActiveMode = 'S' 
        IF EMPTY(m.oControlParent.Controls[m.i].TAG)
          FOR lnCount = 1 TO m.oControlParent.Controls[m.i].ColumnCount
            m.oControlParent.Controls[m.i].TAG = m.oControlParent.Controls[m.i].TAG+','+;
            m.oControlParent.Controls[m.i].COLUMNS(lnCount).CurrentControl
          ENDFOR
          m.oControlParent.Controls[m.i].TAG = m.oControlParent.Controls[m.i].TAG+','
        ENDIF  
        FOR lnCount = 1 TO m.oControlParent.Controls[m.i].ColumnCount
          m.oControlParent.Controls[m.i].COLUMNS(lnCount).CurrentControl = ''
        ENDFOR
        m.oControlParent.Enabled = .F.
      ELSE
        lcColCont = m.oControlParent.Controls[m.i].TAG
        FOR lnCount = 1 TO m.oControlParent.Controls[m.i].ColumnCount
          m.oControlParent.Controls[m.i].COLUMNS(lnCount).CurrentControl = ;
          STRTRAN(SUBSTR(lcColCont,ATC(',',lcColCont,lnCount),ATC(',',lcColCont,lnCount+1)-ATC(',',lcColCont,lnCount)),',')
        ENDFOR
        m.oControlParent.Enabled = .T.            
      ENDIF  
  ENDCASE
ENDFOR

ENDPROC
PROCEDURE getkeyexpr
LPARAMETER oContainer

* Checks for General fields
LOCAL i,oControlParent
lcKey =''
IF PARAMETERS() = 0
   m.oControlParent = THIS
   THIS.key = ''
   this.keyobject = ''
ELSE
	m.oControlParent = m.oContainer
ENDIF
IF TYPE('m.oControlParent')#'O'
  RETURN lcKey
ENDIF

DO CASE 
CASE ATC("FormSet",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.FormCount

CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.PageCount
CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0
	nCtrlCount = oControlParent.ButtonCount
OTHERWISE
	nCtrlCount = oControlParent.ControlCount
ENDCASE

FOR i = 1 TO m.nCtrlCount 
	DO CASE
	CASE ATC("FormSet",m.oControlParent.BaseClass)#0
		THIS.GetKeyExpr(m.oControlParent.Forms[m.i])
	
	CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
		THIS.GetKeyExpr(m.oControlParent.Pages[m.i])
		
	CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0 
             lcKey=m.oControlParent.Buttons[m.i].ControlSource
             lcKey = UPPER(IIF(ATC('.',lcKey)>0,SUBSTR(lcKey,ATC('.',lcKey)+1),lcKey))
             IF lcKey $ KEY() AND m.oControlParent.Buttons[m.i].Visible
               this.key = this.key + IIF(!EMPTY(this.key),'+','') + oControlParent.Buttons[m.i].ControlSource
               this.keyobject = this.keyobject+IIF(!EMPTY(this.keyobject),'+','')+THIS.GetName(oControlParent.Buttons[m.i])
             ENDIF  
               

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Optiongroup,Commandgroup")#0 
		THIS.GetKeyExpr(m.oControlParent.Controls[m.i])

	CASE ATC("Container",m.oControlParent.Controls[m.i].BaseClass) # 0 OR; 
		ATC("Page",m.oControlParent.Controls[m.i].BaseClass) # 0
		THIS.GetKeyExpr(m.oControlParent.Controls[m.i])

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"ListBox,ComboBox,Spinner") # 0 
             lcKey=m.oControlParent.Controls[m.i].ControlSource
             lcKey = UPPER(IIF(ATC('.',lcKey)>0,SUBSTR(lcKey,ATC('.',lcKey)+1),lcKey))
             IF lcKey $ KEY() AND m.oControlParent.Controls[m.i].Visible
               this.key = this.key + IIF(!EMPTY(this.key),'+','') + oControlParent.Controls[m.i].ControlSource
               this.keyobject = this.keyobject+IIF(!EMPTY(this.keyobject),'+','')+THIS.GetName(oControlParent.Controls[m.i])               
             ENDIF  
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"CheckBox,TextBox,OleBoundControl") # 0
             lcKey=m.oControlParent.Controls[m.i].ControlSource
             lcKey = UPPER(IIF(ATC('.',lcKey)>0,SUBSTR(lcKey,ATC('.',lcKey)+1),lcKey))
             IF lcKey $ KEY() AND m.oControlParent.Controls[m.i].Visible
               this.key = this.key + IIF(!EMPTY(this.key),'+','') + oControlParent.Controls[m.i].ControlSource
               this.keyobject = this.keyobject+IIF(!EMPTY(this.keyobject),'+','')+THIS.GetName(oControlParent.Controls[m.i])               
             ENDIF  
	ENDCASE
ENDFOR
RETURN This.key
ENDPROC
PROCEDURE getname
lParameter oContObject
LOCAL lcName
lcName = ''
DO WHILE TYPE('oContObject.Parent')='O'
  lcName = oContObject.Name+'.'+lcName
  oContObject = oContObject.Parent
ENDDO
lcName = IIF(RIGHT(lcName,1)='.',SUBSTR(lcName,1,LEN(lcName)-1),lcName)
RETURN lcName
ENDPROC
PROCEDURE beforesave
LPARAMETER oContainer

* Checks for General fields
LOCAL i,oControlParent

IF PARAMETERS() = 0
  m.oControlParent = THIS
ELSE
	m.oControlParent = m.oContainer
ENDIF

DO CASE 
CASE ATC("FormSet",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.FormCount

CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.PageCount
CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0
	nCtrlCount = 0
OTHERWISE
	nCtrlCount = oControlParent.ControlCount
ENDCASE

FOR i = 1 TO m.nCtrlCount 
	DO CASE
	CASE ATC("FormSet",m.oControlParent.BaseClass)#0
		THIS.BeforeSave(m.oControlParent.Forms[m.i])
	CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
		THIS.BeforeSave(m.oControlParent.Pages[m.i])
	CASE ATC("Container",m.oControlParent.Controls[m.i].BaseClass) # 0 OR; 
		ATC("Page",m.oControlParent.Controls[m.i].BaseClass) # 0
		THIS.BeforeSave(m.oControlParent.Controls[m.i])
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"EditBox,TextBox") # 0
     m.oControlParent.Controls[m.i].Value = m.oControlParent.Controls[m.i].Value 
	ENDCASE
ENDFOR

ENDPROC
PROCEDURE Unload
IF TYPE('oAriaApplication') = 'O'
  =oAriaApplication.ProgramCleanUp(THISFORMSET)
  *B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu [Begin]
  oAriaApplication.ReBldCmp = .T.
  oAriaApplication.SetMenu(oAriaApplication.ActiveModuleID,'A')
  *B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox 
ENDIF  
ENDPROC
PROCEDURE Init
lcKeyExp = ''
THIS.nWorkArea = THIS.GetMasterFile()
THIS.nWorkArea = IIF(TYPE('THIS.nWorkArea')#'C','',THIS.nWorkArea)
IF !EMPTY(THIS.nWorkArea)
  SELECT (THIS.nWorkArea)
  lcKeyExp = THIS.getKeyExpr()
  IF ATC('(',lcKeyExp)>0 OR ATC('+',lcKeyExp)>0
    lcKeyExp = '('+lcKeyExp+')'
  ENDIF
ENDIF
THIS.Key = lcKeyExp

IF TYPE("oARiaApplication") = "O"
  SET DELETE ON
  THIS.AllowAdd    = oARiaApplication.AllowAdd
  THIS.AllowEdit   = oARiaApplication.AllowEdit
  THIS.AllowDelete = oARiaApplication.AllowDelete
  THIS.Bar_No = oARiaApplication.Bar_No
  THIS.Pop_Name = oARiaApplication.Pop_Name
  THIS.MultiRun = oARiaApplication.MultiRun
  IF TYPE('gcBaseWind')='C'
    THISFORMSET.NAME = gcBaseWind
  ENDIF  
  IF !EMPTY(THIS.FormHasToolBar)
    oAriaApplication.oToolBar.Init(THIS)
  ENDIF

  *E301903,1 WAB (Start) -- check if there is trigger for the current screen
  IF !USED('SYCTRIGG')
    SELECT 0
    *** Open the
    USE (oARiaApplication.SysPath+"SYCTRIGG")
    SET ORDER TO 1
  ELSE
    SELECT SYCTRIGG
  ENDIF  
  **** Get the array row no for this popup-bar
  lcSavExact = SET ('EXACT')
  SET EXACT OFF
  *Hassan  [Begin]
  *lnCurPrgNo  = ASCAN(oARiaApplication.aProcess,ALLTRIM(oARiaApplication.POP_NAME)+"-"+PADL(oARiaApplication.BAR_NO,2,'0'))
  *lnCurPrgNo  = IIF(lnCurPrgNo > 0 , ASUBSCRIPT(oARiaApplication.aProcess,lnCurPrgNo,1),lnCurPrgNo)
  *lcCurPrgNm  = oARiaApplication.aProcess[lnCurPrgNo,2]
  lcCurPrgNm = Substr(ThisFormSet.Name,5,Len(ThisFormSet.Name))
  *Hassan  [End]
  
  SET EXACT &lcSavExact
  *DIMENSION oARiaApplication.laEvntTrig[1,1]
  DIMENSION oARiaApplication.laEvntTrig[1,1]
  *Hassan
  oARiaApplication.laEvntTrig[1,1] = ""
  *Hassan
  IF !EMPTY(lcCurPrgNm) AND SEEK(PADR(lcCurPrgNm , 10))
    oARiaApplication.laEvntTrig = ""
    lnCount = 1
    SCAN REST WHILE cApObjNam + cEvent_ID + STR(nTrigOrder,3) = PADR(lcCurPrgNm , 10)
      DIMENSION oARiaApplication.laEvntTrig[lnCount,1]
      *oARiaApplication.laEvntTrig[1,1]= cEvent_ID
      *ADEL
      oARiaApplication.laEvntTrig[lnCount,1]= cEvent_ID
      lnCount = lnCount +1
    ENDSCAN
  ENDIF
  *E301903,1 WAB (End)

ENDIF
THIS.UseDataEnv        = TYPE('THIS.DATAENVIRONMENT')='O'
THIS.SessionIsReadOnly = ISREADONLY()
IF !EMPTY(THIS.FormHasToolBar)
  THIS.ChangeMode(THIS.ActiveMode)
ENDIF  
*THIS.NWORKAREA = SELECT()
ENDPROC
PROCEDURE Error
LPARAMETERS nError, cMethod, nLine
lcIgErr = ;
",1196,1309,1294,1429,38,227,1651,1421,1795,1770,176,177,1951,2030,1944,1798,1693,1791,1566,"+;
"1720,1948,174,175,1979,1978,1728,1642,1695,1698,1966,1967,4,1957,1958,1296,3,1986,1910"+;
",356,355,181,182,"

lcIgErr1 = ;
    ",1595,1585,111,1157,1539,1422,1581,1582,39,1599,108,2003,1426,1440,1427,1428,"+;
    "1420,1436,1912,34,1681,1716,1715,109,1583,1405,1411,95,228,1577,1545,1954,1945"+;
    ",1131,2022,1982,1201,1116,1717,"

lcTermErr = ",1308,1000,2000,1792,56,43,6,1955,"

DO CASE
  CASE ','+ALLT(STR(NERROR))+',' $ lcIgErr OR ','+ALLT(STR(NERROR))+',' $ lcIgErr1
    DO CASE
      CASE NERROR = 38 && Beginning of file encountered. 
         =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                     ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.", 16, "Program Error")  
        GO TOP
        SKIP
        RETRY
      CASE NERROR = 176 && Cannot clear menu that is in use. 
         =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                     ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.", 16, "Program Error")
        DEACTIVATE MENU
        RETRY
      CASE NERROR = 177 && Cannot clear popup that is in use. 
         =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                     ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.", 16, "Program Error")  
        DEACTIVATE POPUP
        RETRY
      CASE NERROR = 174 && Cannot redefine a menu that is in use. 
         =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                     ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.", 16, "Program Error")  
        DEACTIVATE MENU
        RETRY
      CASE NERROR = 175 && Cannot redefine popup that is in use. 
         =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                     ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.", 16, "Program Error")
        DEACTIVATE POPUP
        RETRY
      CASE NERROR = 4   && End of file encountered. 
         =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                     ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.", 16, "Program Error")
        GO BOTT
        RETRY
      CASE NERROR = 1986 && GDI memory is low, close one or more windows and try again. 
        IF MESSAGEBOX(MESSAGE(),5+32,"Program Error") = 4
          RETRY
        ELSE
          IF !EMPTY(GETENV('DEVELOP'))        
            SUSP
          ELSE
            THIS.Release
            RETURN TO DO
          ENDIF  
        ENDIF
      
      CASE NERROR = 125 && Printer is not ready. 
        IF MESSAGEBOX(MESSAGE(),5+32,"Program Error") = 4
          RETRY
        ELSE
          IF !EMPTY(GETENV('DEVELOP'))        
            SUSP
          ELSE
            THIS.Release
            RETURN TO DO
          ENDIF  
        ENDIF
      CASE NERROR = 108 && File is in use by another user. 
        IF MESSAGEBOX(MESSAGE(),5+32,"Program Error") = 4
          RETRY
        ELSE
          IF !EMPTY(GETENV('DEVELOP'))        
            SUSP
          ELSE
            THIS.Release
            RETURN TO DO
          ENDIF  
        ENDIF
      
      CASE NERROR = 109 && Record is in use by another user. 
        IF MESSAGEBOX(MESSAGE(),5+32,"Program Error") = 4
          RETRY
        ELSE
          IF !EMPTY(GETENV('DEVELOP'))        
            SUSP
          ELSE
            THIS.Release
            RETURN TO DO
          ENDIF  
        ENDIF
      
    OTHERWISE
       =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                   ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will continue.",16,"Program Error")  
      IF !EMPTY(GETENV('DEVELOP'))
        SUSP
      ELSE             
       RETURN
      ENDIF 
    ENDCASE   
  CASE  ','+ALLT(STR(NERROR))+',' $ lcTermErr   
     =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+;
                 ALLT(STR(nLine))+CHR(13)+CHR(10)+"Program will terminate.",16,"Program Error")  
    IF !EMPTY(GETENV('DEVELOP'))
      SUSP
    ELSE
      RELEASE ALL
      RETURN TO MASTER
    ENDIF
  OTHERWISE
     =MESSAGEBOX(MESSAGE()+CHR(13)+CHR(10)+"In Program:"+Cmethod+CHR(13)+CHR(10)+"Line:"+ALLT(STR(nLine)),16,"Program Error")
    IF !EMPTY(GETENV('DEVELOP'))
      SUSP
    ELSE
      THIS.Release
      RETURN TO DO
    ENDIF  
ENDCASE
ENDPROC
PROCEDURE Activate
DODEFAULT()
* HIA -- Fill Trigger Array Agian [Begin]
lcAlias = Select()
*WAB
IF USED('SYCTRIGG')
*WAB
  SELECT SYCTRIGG
  lcCurPrgNm = SUBSTR(THISFORMSET.NAME,5,LEN(THISFORMSET.NAME))
  DIMENSION oARiaApplication.laEvntTrig[1,1]
  oARiaApplication.laEvntTrig = ""
  IF !EMPTY(lcCurPrgNm) AND SEEK(PADR(lcCurPrgNm , 10))
    oARiaApplication.laEvntTrig = ""
    lnCount = 1
    =Seek(PADR(lcCurPrgNm , 10),'SYCTRIGG')
    SCAN REST WHILE cApObjNam + cEvent_ID + STR(nTrigOrder,3) = PADR(lcCurPrgNm , 10)
      DIMENSION oARiaApplication.laEvntTrig[lnCount,1]
      oARiaApplication.laEvntTrig[lnCount,1]= cEvent_ID
      lnCount = lnCount +1
    ENDSCAN
  ENDIF
ENDIF 		&& IF USED('SYCTRIGG')  -- WAB
Select (lcAlias)
* HIA -- Fill Trigger Array Agian [End]

ENDPROC
     qPROCEDURE buttonrefresh
* This is a generic routine which refreshes the buttons
* for appropriate table environments.
IF ALIAS() # THIS.nWorkArea
	SELECT (THIS.nWorkArea)
ENDIF

*THIS.SetAllProp()
*THIS.cmdFind.Enabled = !THIS.EditMode
*THIS.cmdPrint.Enabled =  !THIS.EditMode AND THIS.cmdPrint.ControlEnable=1
*THIS.cmdExit.Enabled =  !THIS.EditMode
*THIS.cmdDelete.Enabled =  !THIS.EditMode AND !This.oWindParent.SessionIsReadOnly AND THIS.AllowDelete;
*                          AND THIS.cmdDelete.ControlEnable=1
*THIS.cmdselect.Enabled = !THIS.EditMode

THIS.cmdFind.Enabled = THIS.ActiveMode $ 'VS'
THIS.cmdPrint.Enabled =  THIS.ActiveMode = 'V' AND THIS.cmdPrint.ControlEnable=1
THIS.cmdExit.Enabled =  THIS.ActiveMode $ 'AVSE'
THIS.cmdDelete.Enabled =  THIS.ActiveMode = 'V' AND !This.oWindParent.SessionIsReadOnly AND THIS.AllowDelete;
                         AND THIS.cmdDelete.ControlEnable=1
THIS.cmdselect.Enabled = THIS.ActiveMode = 'V'
*B039850,AAH,EDI-9 BUGS FIX ,12/14/2005 [BEGIN]
THIS.cmdCalc.Enabled   = .T. 
*B039850,AAH,EDI-9 BUGS FIX ,12/14/2005 [END]
THIS.SetCaption()
ACTIVATE MENU _MSYSMENU NOWAIT


ENDPROC
PROCEDURE initvars
						
#DEFINE C_NOUPDATEVIEW_LOC	"Edits to one or more of the Views may not be permanent. "+;
							"To remedy this, ensure the View's Send SQL Updates checkbox is checked in the View Designer."
#DEFINE C_READONLY_LOC		"The table is Read-Only. You will not be able to edit it."

LOCAL aTablesUsed,nTablesUsed,i,aMems,nTotMem,cWizFile,lShowedMess
DIMENSION aTablesUsed[1]
DIMENSION aMems[1]

* This routine sets the member variables
THIS.ViewKey = ""
THIS.ParentKey = ""
THIS.ViewType = 3
THIS.GridAlias = ""


IF TYPE('THIS.oWindParent')='O'
*E000000,1 Hesham (Start)
*  THIS.AddMode = THIS.oWindParent.AddMode 
*  THIS.EditMode = THIS.oWindParent.EditMode   
*  THIS.SelectMode = THIS.oWindParent.SelectMode     
*E000000,1 Hesham (End)
  THIS.TopFile = THIS.oWindParent.TopFile
  THIS.EndFile = THIS.oWindParent.EndFile
  THIS.OLDREC  = THIS.oWindParent.OLDREC
  THIS.ActiveMode  = THIS.oWindParent.ActiveMode
  SET DATASESSION TO THIS.oWindParent.DataSessionID
*E000000,1 Hesham (Start)  
*  THIS.nWorkArea = THIS.oWindParent.nWorkArea  
  THIS.nWorkArea = THIS.oWindParent.nWorkArea &&THIS.oWindParent.GetMasterFile()
*E000000,1 Hesham (End)  
*  SELECT (THIS.nWorkArea)
*  IF EMPTY(THIS.oWindParent.Key)
*    lcKeyExp = THIS.getKeyExpr()
*    IF ATC('(',lcKeyExp)>0 OR ATC('+',lcKeyExp)>0
*      lcKeyExp = '('+lcKeyExp+')'
*    ENDIF
*    THIS.oWindParent.Key = lcKeyExp
*    THIS.oWindParent.KeyObject = THIS.Keyobject
*  ENDIF  
  THIS.lcKey.ControlSource = THIS.oWindParent.Key
  THIS.keyobject = THIS.oWindParent.KeyObject
  THIS.AllowAdd = THIS.oWindParent.AllowAdd
  THIS.AllowEdit = THIS.oWindParent.AllowEdit
  THIS.AllowDelete = THIS.oWindParent.AllowDelete
  THIS.CmdTop.ControlEnable    = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,1,1))
  THIS.CmdPrev.ControlEnable   = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,1,1))
  THIS.CmdNext.ControlEnable   = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,1,1))
  THIS.CmdEnd.ControlEnable    = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,1,1))
  THIS.CmdPrint.ControlEnable  = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,2,1))  
  THIS.CmdAdd.ControlEnable    = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,3,1))  
  THIS.CmdEdit.ControlEnable   = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,4,1))  
  THIS.CmdDelete.ControlEnable = VAL(SUBSTR(THIS.oWindParent.FormHasToolBar,5,1))  
  IF '1' $ THIS.oWindParent.FormHasToolBar AND !EMPTY(THIS.oWindParent.FormHasToolBar)
    THIS.VISIBLE = .T.
  ELSE  
    THIS.VISIBLE = .F.
  ENDIF
ELSE
*E000000,1 Hesham (Start)  
*  THIS.AddMode = .F.
   THIS.ActiveMode = 'N'
*E000000,1 Hesham (End)    

  THIS.TopFile = .F.
  THIS.EndFile = .F.
  THIS.AllowAdd=.f.
  THIS.AllowEdit=.f.
  THIS.AllowDelete=.f.
  THIS.nWorkArea = ALIAS(SELECT())
  THIS.lcKey.ControlSource = ''
  THIS.CmdTop.ControlEnable = 1
  THIS.CmdPrev.ControlEnable = 1
  THIS.CmdNext.ControlEnable = 1
  THIS.CmdEnd.ControlEnable = 1
  THIS.CmdPrint.ControlEnable = 1
  THIS.CmdAdd.ControlEnable = 1
  THIS.CmdEdit.ControlEnable = 1
  THIS.CmdDelete.ControlEnable = 1
ENDIF

* These properties should not be used. They are reserved for use by
* the Preview button of the Form Wizards.
*E000000,1 Hesham (Start)  
*THIS.PreviewMode = IIF(TYPE("THIS.PreviewMode")#"L",.F.,THIS.PreviewMode)
*THIS.PreviewInit = IIF(TYPE("THIS.PreviewInit")#"L",.T.,THIS.PreviewInit)
*THIS.EditMode = IIF(TYPE("THIS.EditMode")#"L",.F.,THIS.EditMode)
*E000000,1 Hesham (End)  
* Disable appropriate buttons

THIS.cmdAdd.Enabled = THIS.ActiveMode $ 'SV' AND !This.oWindParent.SessionIsReadOnly AND this.AllowAdd AND THIS.cmdAdd.ControlEnable=1
*THIS.cmdEdit.Enabled = THIS.ActiveMode $ 'VAE' AND !This.oWindParent.SessionIsReadOnly AND This.AllowEdit ;
                       AND THIS.cmdEdit.ControlEnable=1 AND THIS.ActiveMode='V'
THIS.cmdEdit.Enabled = THIS.ActiveMode $ 'V' AND !This.oWindParent.SessionIsReadOnly AND This.AllowEdit ;
                       AND THIS.cmdEdit.ControlEnable=1 AND THIS.ActiveMode='V'
                       
THIS.cmdDelete.Enabled = THIS.ActiveMode='V' AND !This.oWindParent.SessionIsReadOnly AND THIS.AllowDelete AND THIS.cmdDelete.ControlEnable=1




IF THIS.PreviewMode
	RETURN
ENDIF



ENDPROC
PROCEDURE updaterows
#DEFINE	E_FAIL_LOC			"Failed to update table: "
#DEFINE	E_TRIGGERFAIL_LOC	"Trigger failed."
#DEFINE	E_FIELDNULL_LOC		"Field doesn't accept NULL"
#DEFINE	E_FIELDRULE_LOC		"Field rule violated"
#DEFINE	E_RECORDLOCK_LOC	"Record in use by another user"
#DEFINE	E_ROWRULE_LOC		"Row rule violated"
#DEFINE	E_UNIQUEINDEX_LOC	"Unique index violation"
#DEFINE	E_DIRTYREC_LOC		"Data has been changed by another user. Overwrite changes with your edits?"
#DEFINE	E_NOFORCE_LOC		"Could not force table updates."
#DEFINE E_PROMPT_LOC	 	"Error: "
#DEFINE MSGBOX_YES			6

LOCAL aErrors,cErrorMessage,aTablesUsed,nTablesUsed,nTotErr 
LOCAL nFld,i,nOldArea,lSuccess,lInDBC,lOverwrite,lHadMessage

DIMENSION aTablesUsed[1]
DIMENSION aErrors[1]
m.cErrorMessage=""
m.lSuccess = .T.
m.nOldArea = SELECT()
m.nTablesUsed = AUSED(aTablesUsed)

* Can wrap everything in transaction if using strictly DBCs

FOR i = 1 TO m.nTablesUsed

	SELECT (aTablesUsed[m.i,1])

	m.lInDBC = !EMPTY(CURSORGETPROP("Database"))
	m.cErrorMessage = ""
	m.lOverwrite = .F.
	m.lHadMessage = .F.

	DO CASE
	CASE CURSORGETPROP("Buffering") = 1
		* Skip if buffering not on
		LOOP
	CASE GetFldState(0) = 2			&&deleted record
		* Only delete current record and force it
		m.lSuccess = TableUpdate(.F.,.T.)
		IF m.lSuccess				&&successful update
			LOOP
		ENDIF
	CASE !m.lInDBC AND (ATC("2",GetFldState(-1))#0 OR;
		 ATC("3",GetFldState(-1))#0)	
		* Field was edited - in Free Table
		* Since free tables are not supported by transactions,
		* we must process record by record
		m.nModRecord = GetNextMod(0)
		DO WHILE m.nModRecord # 0	&&loop locks all records
			GO m.nModRecord
			m.lSuccess = RLOCK()	&&try to lock record
			IF !m.lSuccess			&&failed to lock record
				m.cErrorMessage = E_RECORDLOCK_LOC
				UNLOCK ALL
				EXIT
			ENDIF
			IF !m.lHadMessage	&&so we don't repeat alert
				* See if record(s) modified by another user
				FOR m.nFld = 1 TO FCOUNT()
					IF TYPE(FIELD(m.nFld)) = "G"	&&skip for General fields
						LOOP					
					ENDIF
					IF OLDVAL(FIELD(m.nFld)) # CURVAL(FIELD(m.nFld))
						m.lHadMessage = .T.
						IF MESSAGEBOX(E_DIRTYREC_LOC,4+48,_screen.Caption) = MSGBOX_YES
							m.lOverwrite = .T.
						ELSE
							m.lSuccess = .F.
							UNLOCK ALL
							EXIT
						ENDIF
					ENDIF
				ENDFOR
			ENDIF
			m.nModRecord = GetNextMod(m.nModRecord)
		ENDDO
		IF m.lSuccess 	&&was able to lock all records
			m.lSuccess = TableUpdate(.T.,m.lOverwrite)
			IF m.lSuccess &&was able to update all records
				LOOP
			ENDIF	
			UNLOCK ALL
		ENDIF
	CASE m.lInDBC
		BEGIN TRANSACTION
		* Try to update all records in selected table
		m.lSuccess = TableUpdate(.T.,.F.)	&&successful update
		IF m.lSuccess
			END TRANSACTION
			LOOP
		ENDIF
		ROLLBACK
	ENDCASE

	* Handle errors
	nTotErr =AERROR(aErrors)
	DO CASE
	CASE nTotErr = 0
		
	CASE aErrors[1,1] = 1539				&& Trigger failed
		m.cErrorMessage = E_TRIGGERFAIL_LOC
	CASE aErrors[1,1] = 1581				&& Field doesn't accept NULL
		m.cErrorMessage = E_FIELDNULL_LOC
	CASE aErrors[1,1] = 1582				&& Field rule violated
		m.cErrorMessage = E_FIELDRULE_LOC
	CASE aErrors[1,1] = 1700				&& Record in use by another user
		m.cErrorMessage = E_RECORDLOCK_LOC
	CASE aErrors[1,1] = 1583				&& Row rule violated
		m.cErrorMessage = E_ROWRULE_LOC
	CASE aErrors[1,1] = 1884				&& Unique index violation
		m.cErrorMessage = E_UNIQUEINDEX_LOC
	CASE aErrors[1,1] = 1585				&& Record changed by another user

		IF m.lInDBC		&&handle free tables above
			* Dislpay conflict alert
			IF MESSAGEBOX(E_DIRTYREC_LOC,4+48,_screen.Caption) = MSGBOX_YES
				*Try to force update
				BEGIN TRANSACTION				
				m.lSuccess = TABLEUPDATE(.T.,.T.)
				IF m.lSuccess
					END TRANSACTION
					LOOP
				ELSE
					ROLLBACK
					=MESSAGEBOX(E_NOFORCE_LOC,0,_screen.Caption)
				ENDIF			
			ENDIF
		ENDIF

	OTHERWISE
		IF !EMPTY(m.cErrorMessage)	&&for free table handling above
			m.cErrorMessage = E_PROMPT_LOC+aErrors[1,2]
		ENDIF
	ENDCASE

	* Had an error we couldn't handle
	=TABLEREVERT(.T.)  &&revert all records
	m.lSuccess = .F.
	IF !EMPTY(m.cErrorMessage)
		=MESSAGEBOX(E_FAIL_LOC+m.cErrorMessage,0,_screen.Caption)
	ENDIF

ENDFOR

SELECT (m.nOldArea)
RETURN m.lSuccess

ENDPROC
PROCEDURE setcaption
IF THIS.ActiveMode $ 'SV' &&!THIS.EditMode
	THIS.cmdAdd.Picture = THIS.BMPPATH +"tbnew.bmp"
*	THIS.cmdEdit.Picture = THIS.BMPPATH +"edit1.bmp"
	THIS.cmdExit.Picture = THIS.BMPPATH +"close.bmp"
	THIS.cmdAdd.DownPicture = THIS.BMPPATH +"tbnew.bmp"
*	THIS.cmdEdit.DownPicture = THIS.BMPPATH +"edit1.bmp"
	THIS.cmdExit.DownPicture = THIS.BMPPATH +"close.bmp"
*	THIS.cmdEdit.Enabled = This.AllowEdit AND THIS.cmdEdit.ControlEnable=1 
*	THIS.cmdAdd.Enabled = This.AllowEdit AND THIS.cmdAdd.ControlEnable=1 	
	THIS.cmdEdit.Enabled = This.AllowEdit AND THIS.ActiveMode ='V' AND THIS.cmdEdit.ControlEnable=1 
	THIS.cmdAdd.Enabled = This.AllowEdit AND THIS.ActiveMode $ 'SV' AND THIS.cmdAdd.ControlEnable=1 	

	IF POPUP('P01PU01')
*	  DEFINE BAR 9 OF P03PU03 PROMPT "\<Edit" Key CTRL+E,'^E' SKIP FOR ;
    IIF(TYPE('oAriaApplication.oToolBar.cmdEdit') $ 'UL',.T.,oAriaApplication.oToolBar.cmdEdit.Enabled=.F.)
      DEFINE BAR 52 OF P01PU01 PROMPT "\<Close" ;
             SKIP FOR IIF(TYPE('LACTRSTAT')='U',.T.,LACTRSTAT[12]="DISABLE")      
    
	ENDIF
ELSE
	THIS.cmdAdd.Picture = THIS.BMPPATH +"tbsave.bmp"
*	THIS.cmdEdit.Picture = THIS.BMPPATH +"tbundo.bmp"
	THIS.cmdExit.Picture = THIS.BMPPATH +"tbundo.bmp"
	THIS.cmdAdd.DownPicture = THIS.BMPPATH +"tbsave.bmp"
*	THIS.cmdEdit.DownPicture = THIS.BMPPATH +"tbundo.bmp"
	THIS.cmdExit.DownPicture = THIS.BMPPATH +"tbundo.bmp"
*	THIS.cmdEdit.Enabled = .T.
	THIS.cmdEdit.Enabled = .F.
	THIS.cmdExit.Enabled = .T.	
	THIS.cmdAdd.Enabled = .T.

	IF POPUP('P01PU01')
*	  DEFINE BAR 9 OF P03PU03 PROMPT "\<Undo" Key CTRL+Z,'^Z' SKIP FOR ;
    IIF(TYPE('oAriaApplication.oToolBar.cmdEdit') $ 'UL',.T.,oAriaApplication.oToolBar.cmdEdit.Enabled=.F.)

      DEFINE BAR 52 OF P01PU01 PROMPT "\<Cancel" ;
             SKIP FOR IIF(TYPE('LACTRSTAT')='U',.T.,LACTRSTAT[12]="DISABLE")      
  
	ENDIF
ENDIF
ENDPROC
PROCEDURE navrefresh
**** Navigational Button Handling ****
LOCAL OldLockScreen,KeyValue,cFiltExpr
m.OldLockScreen = THISFORM.LockScreen
THISFORM.LockScreen = .T.

IF ALIAS()#THIS.nWorkArea
	SELECT (THIS.nWorkArea)
ENDIF
THIS.CMDADD.ToolTipText = IIF(THIS.ActiveMode $ 'SV','Add new record','Save modifications')
*THIS.CMDEDIT.ToolTipText = IIF(THIS.ActiveMode $ 'SV','Edit the current record','Undo Changes')
THIS.CMDEXIT.ToolTipText = IIF(THIS.ActiveMode $ 'SV','Close Current Form','Undo Changes')
IF THIS.ActiveMode $ 'SV'

	* Check for bottom of file
	THIS.EndFile = EOF() OR THIS.EndFile
   
	* Test to see we are on last record
	IF !THIS.EndFile
		SKIP
		THIS.EndFile = EOF()
		SKIP -1
	ELSE
		GO BOTTOM
	ENDIF

	* Check for top of file
	THIS.TopFile = BOF() OR EOF() OR THIS.TopFile

	* Test to see if we are on first record
	IF !THIS.TopFile
		SKIP -1
		THIS.TopFile = BOF()
		IF !THIS.TopFile
			SKIP
		ENDIF	
	ENDIF

	IF THIS.TopFile
		GO TOP
	ENDIF

ENDIF

*THIS.cmdTop.Enabled = !THIS.TopFile AND !THIS.EditMode AND THIS.cmdTop.ControlEnable=1
*THIS.cmdPrev.Enabled = !THIS.TopFile AND !THIS.EditMode AND THIS.cmdPrev.ControlEnable=1
*THIS.cmdNext.Enabled = !THIS.EndFile AND !THIS.EditMode AND THIS.cmdNext.ControlEnable=1
*THIS.cmdEnd.Enabled = !THIS.EndFile AND !THIS.EditMode AND THIS.cmdEnd.ControlEnable=1
THIS.cmdTop.Enabled = !THIS.TopFile AND THIS.ActiveMode='V' AND THIS.cmdTop.ControlEnable=1
THIS.cmdPrev.Enabled = !THIS.TopFile AND THIS.ActiveMode='V' AND THIS.cmdPrev.ControlEnable=1
THIS.cmdNext.Enabled = !THIS.EndFile AND THIS.ActiveMode='V' AND THIS.cmdNext.ControlEnable=1
THIS.cmdEnd.Enabled = !THIS.EndFile AND THIS.ActiveMode='V' AND THIS.cmdEnd.ControlEnable=1


* Check if no records in query set
DO CASE
CASE THIS.PreviewMode OR ISREADONLY()
	* Nothing
CASE THIS.ActiveMode = 'E' &&THIS.EditMode
	THIS.cmdEdit.Enabled = .F. &&This.AllowEdit AND THIS.cmdEdit.ControlEnable=1 &&.T.
CASE RECCOUNT()=0 OR BOF() OR EOF()
	THIS.cmdEdit.Enabled = .F.
	THIS.cmdDelete.Enabled = .F.
CASE !THIS.cmdEdit.Enabled
*	THIS.cmdEdit.Enabled = THIS.ActiveMode $ 'AEV' AND THIS.AllowEdit AND THIS.cmdEdit.ControlEnable=1 &&.T.
	THIS.cmdEdit.Enabled = THIS.ActiveMode $ 'V' AND THIS.AllowEdit AND THIS.cmdEdit.ControlEnable=1 &&.T.
	THIS.cmdDelete.Enabled = THIS.ActiveMode= 'V' AND THIS.AllowDelete AND THIS.cmdDelete.ControlEnable=1 &&.T.
ENDCASE

* Update Grid for Views
IF THIS.ActiveMode$'SV' .AND. !EMPTY(THIS.ViewKey) &&!THIS.EditMode AND !EMPTY(THIS.ViewKey)
	KeyValue = EVAL(THIS.ParentKey)
	DO CASE
	CASE TYPE(THIS.ParentKey) = "C"
		cFiltExpr = THIS.ViewKey + "=" + "["+m.KeyValue+"]"
	CASE TYPE(THIS.ParentKey) = "L"
		cFiltExpr = THIS.ViewKey
	CASE TYPE(THIS.ParentKey) = "D"
		cFiltExpr = THIS.ViewKey + "=" + "{"+DTOC(m.KeyValue)+"}"	
	CASE TYPE(THIS.ParentKey) = "T"
		cFiltExpr = THIS.ViewKey + "=" + "{"+TTOC(m.KeyValue)+"}"	
	OTHERWISE	
		* Numeric
		cFiltExpr = THIS.ViewKey + "=" + ALLTRIM(STR(m.KeyValue,20,18))
	ENDCASE
		
	SELECT (THIS.GridAlias)
	DO CASE
	CASE .F. &&parameterized query
		* set parameter here
		* =requery()
	CASE THIS.ViewType = 1	&&local views
		SET FILTER TO &cFiltExpr
	CASE THIS.ViewType = 2	&&remote views
	
	ENDCASE
	SELECT (THIS.nWorkArea)
ENDIF

THIS.oWindParent.refreshALL()
THIS.oWindParent.refresh()  
THISFORM.Refresh()

THISFORM.LockScreen = m.OldLockScreen
laCtrStat[1] = IIF(THIS.CMDTOP.ENABLED,'ENABLED','DISABLE')
laCtrStat[2] = IIF(THIS.CMDEND.ENABLED,'ENABLED','DISABLE')
laCtrStat[3] = IIF(THIS.CMDNEXT.ENABLED,'ENABLED','DISABLE')
laCtrStat[4] = IIF(THIS.CMDPREV.ENABLED,'ENABLED','DISABLE')
laCtrStat[5] = IIF(THIS.CMDADD.ENABLED AND THIS.ActiveMode$'SV','ENABLED','DISABLE')
laCtrStat[6] = IIF(THIS.CMDPRINT.ENABLED,'ENABLED','DISABLE')
*laCtrStat[7] = IIF(THIS.CMDEDIT.ENABLED AND THIS.ActiveMode$'SV','ENABLED','DISABLE')
laCtrStat[7] = IIF(THIS.CMDEDIT.ENABLED AND THIS.ActiveMode$'V','ENABLED','DISABLE')
laCtrStat[8] = IIF(THIS.CMDDELETE.ENABLED,'ENABLED','DISABLE')
laCtrStat[9] = laCtrStat[9]
laCtrStat[10] = IIF(THIS.CMDFIND.ENABLED,'ENABLED','DISABLE')
laCtrStat[11] = IIF(THIS.CMDADD.ENABLED AND THIS.ActiveMode$'EA','ENABLED','DISABLE')
laCtrStat[12] = IIF(THIS.CMDEXIT.ENABLED OR THIS.ActiveMode$'EA','ENABLED','DISABLE')

ENDPROC
PROCEDURE getgridref
* Check if we have a grid
LOCAL aMems,nTotMem,i
THIS.GridRef = ""
*hesham start
*IF TYPE("THISFORM") = "O"
IF TYPE("THIS.oWindParent") = "O"
*hesham end
	DIMENSION aMems[1]
*hesham start	
*	nTotMem = AMEMBERS(aMems,THISFORM,2)
	nTotMem = AMEMBERS(aMems,THIS.oWindParent,2)	
*hesham end
	
*hesham start	
*	WITH THISFORM
	WITH THIS.oWindParent
*hesham end	
		FOR i = 1 TO m.nTotMem 
			IF UPPER(EVAL("."+aMems[m.i]+".BaseClass")) = "GRID"
				THIS.GridRef = aMems[m.i]
				WITH EVAL("."+THIS.GridRef)
					* Check if we have a view and get Tag property
					THIS.ViewType = CURSORGETPROP('sourcetype',.RecordSource)
					THIS.GridAlias = .RecordSource
					IF THIS.ViewType # 3
						THIS.ViewKey = .Tag
						THIS.ParentKey = .Comment
					ENDIF
				ENDWITH				
				EXIT
			ENDIF
		ENDFOR
	ENDWITH
ENDIF

ENDPROC
PROCEDURE getkeyexpr
LPARAMETER oContainer

* Checks for General fields
LOCAL i,oControlParent
lcKey =''
IF PARAMETERS() = 0
  m.oControlParent = THIS.oWindParent	  
  THIS.FormKey = ''
  this.keyobject = ''
ELSE
	m.oControlParent = m.oContainer
ENDIF

DO CASE 
  CASE ATC("FormSet",m.oControlParent.BaseClass)#0
  	nCtrlCount = oControlParent.FormCount
  CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
  	nCtrlCount = oControlParent.PageCount
  CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0
  	nCtrlCount = oControlParent.ButtonCount
  OTHERWISE
  	nCtrlCount = oControlParent.ControlCount
ENDCASE

FOR i = 1 TO m.nCtrlCount 
	DO CASE
	CASE ATC("FormSet",m.oControlParent.BaseClass)#0
		THIS.GetKeyExpr(m.oControlParent.Forms[m.i])
	
	CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
		THIS.GetKeyExpr(m.oControlParent.Pages[m.i])
		
	CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0 
    lcKey=m.oControlParent.Controls[m.i].ControlSource
    lcKey = UPPER(IIF(ATC('.',lcKey)>0,SUBSTR(lcKey,ATC('.',lcKey)+1),lcKey))
    IF lcKey $ KEY() AND m.oControlParent.Controls[m.i].Visible
      this.formKey = this.formKey + IIF(!EMPTY(this.formKey),'+','') + oControlParent.Controls[m.i].ControlSource
      this.keyobject = this.keyobject+IIF(!EMPTY(this.keyobject),'+','')+THIS.GetName(oControlParent.Controls[m.i])
    ENDIF  
 
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Optiongroup,Commandgroup")#0 
		THIS.GetKeyExpr(m.oControlParent.Controls[m.i])

	CASE ATC("Container",m.oControlParent.Controls[m.i].BaseClass) # 0 OR; 
		ATC("Page",m.oControlParent.Controls[m.i].BaseClass) # 0
		THIS.GetKeyExpr(m.oControlParent.Controls[m.i])

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"ListBox,ComboBox,Spinner") # 0 
    lcKey=m.oControlParent.Controls[m.i].ControlSource
    lcKey = UPPER(IIF(ATC('.',lcKey)>0,SUBSTR(lcKey,ATC('.',lcKey)+1),lcKey))
    IF lcKey $ KEY() AND m.oControlParent.Controls[m.i].Visible
      this.formKey = this.formKey + IIF(!EMPTY(this.formKey),'+','') + oControlParent.Controls[m.i].ControlSource
      this.keyobject = this.keyobject+IIF(!EMPTY(this.keyobject),'+','')+THIS.GetName(oControlParent.Controls[m.i])               
    ENDIF  
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"CheckBox,TextBox,OleBoundControl") # 0
    lcKey=m.oControlParent.Controls[m.i].ControlSource
    lcKey = UPPER(IIF(ATC('.',lcKey)>0,SUBSTR(lcKey,ATC('.',lcKey)+1),lcKey))
    IF lcKey $ KEY() AND m.oControlParent.Controls[m.i].Visible
      this.formKey = this.formKey + IIF(!EMPTY(this.formKey),'+','') + oControlParent.Controls[m.i].ControlSource
      this.keyobject = this.keyobject+IIF(!EMPTY(this.keyobject),'+','')+THIS.GetName(oControlParent.Controls[m.i])               
    ENDIF  
	ENDCASE
ENDFOR
RETURN This.FormKey
ENDPROC
PROCEDURE filewaschanged
IF THIS.UseDataEnv
  IF TYPE("THIS.oWindParent.PARENT")='O'
    nTotMem = AMEMBERS(aMems,THIS.oWindParent.Parent.DataEnvironment,2)	
  ELSE
    nTotMem = AMEMBERS(aMems,THIS.oWindParent.DataEnvironment,2)	
  ENDIF	
  IF TYPE("THIS.oWindParent.PARENT")='O'
    WITH THIS.oWindParent.PARENT.DataEnvironment
      * Check for Views
      FOR i = 1 TO m.nTotMem
        IF UPPER(EVAL("."+aMems[m.i]+".BaseClass")) = "CURSOR"
          WITH EVAL("."+aMems[m.i])
            IF !(GETFLDSTATE(-1,.Alias)== '1'+REPLICATE('1',FCOUNT(.Alias))) AND ;
             !(GETFLDSTATE(-1,.Alias)== '3'+REPLICATE('3',FCOUNT(.Alias)))
              RETURN .T.
            ENDIF
          ENDWITH
        ENDIF
      ENDFOR
	ENDWITH
  ELSE
    WITH THIS.oWindParent.DataEnvironment		
      * Check for Views
      FOR i = 1 TO m.nTotMem
        IF UPPER(EVAL("."+aMems[m.i]+".BaseClass")) = "CURSOR"
          WITH EVAL("."+aMems[m.i])
            IF !(GETFLDSTATE(-1,.Alias)== '1'+REPLICATE('1',FCOUNT(.Alias))) AND;
             !(GETFLDSTATE(-1,.Alias)== '3'+REPLICATE('3',FCOUNT(.Alias)))            
              RETURN .T.
            ENDIF
          ENDWITH
        ENDIF
      ENDFOR
    ENDWITH
  ENDIF	
ENDIF
RETURN .F.


ENDPROC
PROCEDURE changetoaddmode
LPARAMETER oContainer

* Checks for General fields
LOCAL i,oControlParent

IF PARAMETERS() = 0
  IF TYPE("THIS.oWindParent.PARENT")='O'
     m.oControlParent = THIS.oWindParent.Parent
  ELSE
     m.oControlParent = THIS.oWindParent	  
  ENDIF   
ELSE
	m.oControlParent = m.oContainer
ENDIF

DO CASE 
CASE ATC("FormSet",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.FormCount

CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
	nCtrlCount = oControlParent.PageCount
CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0
	nCtrlCount = oControlParent.ButtonCount
OTHERWISE
	nCtrlCount = oControlParent.ControlCount
ENDCASE

FOR i = 1 TO m.nCtrlCount 
	DO CASE
	CASE ATC("FormSet",m.oControlParent.BaseClass)#0
		THIS.ChangeToAddMode(m.oControlParent.Forms[m.i])
	
	CASE ATC("Pageframe",m.oControlParent.BaseClass)#0
		THIS.ChangeToAddMode(m.oControlParent.Pages[m.i])
		
	CASE ATC(m.oControlParent.BaseClass,"Optiongroup,Commandgroup")#0 
		m.oControlParent.Buttons[m.i].Enabled = THIS.ActiveMode$'EA'

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Optiongroup,Commandgroup")#0 
		THIS.ChangeToAddMode(m.oControlParent.Controls[m.i])

	CASE ATC("Container",m.oControlParent.Controls[m.i].BaseClass) # 0 OR; 
		ATC("Page",m.oControlParent.Controls[m.i].BaseClass) # 0
		THIS.ChangeToAddMode(m.oControlParent.Controls[m.i])

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"ListBox,ComboBox,Spinner") # 0 
    IF m.oControlParent.Controls[m.i].ControlSource $ THIS.FormKey 
	 		m.oControlParent.Controls[m.i].Enabled = .F.
		ELSE	
	 		m.oControlParent.Controls[m.i].Enabled = THIS.ActiveMode$'EA'
    ENDIF
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"CheckBox,TextBox,OleBoundControl") # 0
	  IF m.oControlParent.Controls[m.i].ControlSource $ THIS.FormKey 
			m.oControlParent.Controls[m.i].Enabled = .F.
  	ELSE	
			m.oControlParent.Controls[m.i].Enabled = THIS.ActiveMode$'EA'
		ENDIF
	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"EditBox") # 0
		m.oControlParent.Controls[m.i].ReadOnly = THIS.ActiveMode$'SV'
		IF !THIS.HasMemo
			WITH m.oControlParent.Controls[m.i]
				THIS.EditForeColor = .ForeColor
				THIS.EditDisForeColor =  .DisabledForeColor
				THIS.EditBackColor = .BackColor
				THIS.EditDisBackColor =  .DisabledBackColor
				THIS.HasMemo = .T.
			ENDWITH
		ENDIF
		m.oControlParent.Controls[m.i].ForeColor = IIF(THIS.ActiveMode$'EA',THIS.EditForeColor,THIS.EditDisForeColor)
		m.oControlParent.Controls[m.i].BackColor = IIF(THIS.ActiveMode$'EA',THIS.EditBackColor,THIS.EditDisBackColor)

	CASE ATC(m.oControlParent.Controls[m.i].BaseClass,"Grid") # 0
		m.oControlParent.Controls[m.i].ReadOnly = THIS.ActiveMode$'SV'
*		m.oControlParent.Controls[m.i].DeleteMark = THIS.EditMode
	ENDCASE
ENDFOR

ENDPROC
PROCEDURE getname
lParameter oContObject
LOCAL lcName
lcName = ''
DO WHILE TYPE('oContObject.Parent')='O'
  lcName = oContObject.Name+'.'+lcName
  oContObject = oContObject.Parent
ENDDO
lcName = IIF(RIGHT(lcName,1)='.',SUBSTR(lcName,1,LEN(lcName)-1),lcName)
RETURN lcName
ENDPROC
PROCEDURE Destroy
* Restore various settings
LOCAL nTablesUsed,aTablesUsed,i,nDECursors,aDECursors
DIMENSION aTablesUsed[1]

IF TYPE('THIS.Parent') # "O"
	RETURN
ENDIF

IF TYPE("THIS.oldTalk") = "C" AND THIS.oldTalk="ON"
	SET TALK ON
ENDIF

* OLE Servers can still send data back to General fields 
* even though they are not in Edit Mode. We need to reset 
* buffering to 1 so the buffer is not updated by the OLE Server.
* Also, folks might exit out while editing.
IF THIS.UseDataEnv
	DIMENSION aDECursors[1]
*hesham start	
*	nDECursors = AMEMBERS(aDECursors,THISFORM.DataEnvironment,2)
  IF TYPE("THIS.oWindParent.PARENT")='O'
	nDECursors = AMEMBERS(aDECursors,THIS.oWindParent.Parent.DataEnvironment,2)	
   ELSE
	nDECursors = AMEMBERS(aDECursors,THIS.oWindParent.DataEnvironment,2)	   
   ENDIF	
*hesham end	
	FOR i = 1 TO m.nDECursors
*hesham start	
*		WITH EVAL("THISFORM.DataEnvironment." + aDECursors[m.i])
  IF TYPE("THIS.oWindParent.PARENT")='O'
		WITH EVAL("THIS.oWindParent.Parent.DataEnvironment." + aDECursors[m.i])		
*hesham end		
			IF USED(.ALIAS) AND ATC("CURSOR",.BaseClass)#0 AND ;
			  CursorGetProp("sourcetype",.ALIAS)=3 AND ;
			  CursorGetProp("buffering",.ALIAS)>1
				=TableRevert(.T.,.ALIAS)
				=CursorSetProp("buffering",1,.ALIAS)	&&optimistic table buffering
			ENDIF
		ENDWITH
  
  ELSE
		WITH EVAL("THIS.oWindParent.DataEnvironment." + aDECursors[m.i])		
*hesham end		
			IF USED(.ALIAS) AND ATC("CURSOR",.BaseClass)#0 AND ;
			  CursorGetProp("sourcetype",.ALIAS)=3 AND ;
			  CursorGetProp("buffering",.ALIAS)>1
				=TableRevert(.T.,.ALIAS)
				=CursorSetProp("buffering",1,.ALIAS)	&&optimistic table buffering
			ENDIF
		ENDWITH
   ENDIF		
	ENDFOR
ENDIF

* Skip if using preview mode
IF THIS.PreviewMode
	RETURN
ENDIF

IF THIS.oldSetDelete = "OFF"
	SET DELETED OFF
ENDIF
SET REPROCESS TO THIS.oldReprocess
SET MESSAGE TO
SELECT (THIS.nWorkArea)

IF THIS.UseDataEnv
	RETURN
ENDIF

* The following code is here to support 
* forms not using a DataEnvironment.


FOR i = 1 TO m.nTablesUsed
	IF USED(aTablesUsed[m.i,1]) AND ATC(".TMP",DBF(aTablesUsed[m.i,1]))=0	&&skip for views
		=CursorSetProp("buffering",THIS.oldBuffering,aTablesUsed[m.i,1])	&&optimistic table buffering
	ENDIF
ENDFOR

IF THIS.oldMultiLocks = "OFF"
	SET MULTILOCKS OFF
ENDIF

IF THIS.oldSetFields = "ON"
	SET FIELDS ON
ENDIF

SET REFRESH TO THIS.oldRefresh

ENDPROC
PROCEDURE Init
LPARAMETERS oWindName

*HESHAM START
DIMENSION laCtrStat[12]
STORE 'DISABLE' TO laCtrStat

IF TYPE('oWindName')='O'
  THIS.oWindParent = oWindName
* THIS.SETALL('ENABLED',.T.)    
ELSE
  THIS.oWindParent = .F.
  THIS.SETALL('ENABLED',.F.)
  THIS.CMDCALC.ENABLED = .T.
  THIS.lcKey.ControlSource = ''  
  THIS.lcKey.value=''
  THIS.VISIBLE = .F. 
  IF POPUP('P01PU01')
*    DEFINE BAR 9 OF P03PU03 PROMPT "\<Edit" Key CTRL+E,'^E' SKIP FOR ;
    IIF(TYPE('oAriaApplication.oToolBar.cmdEdit') $ 'UL',.T.,oAriaApplication.oToolBar.cmdEdit.Enabled=.F.)
    DEFINE BAR 52 OF P01PU01 PROMPT "\<Close" ;
           SKIP FOR IIF(TYPE('LACTRSTAT')='U',.T.,LACTRSTAT[12]="DISABLE")      
  ENDIF
ENDIF  
*HESHAM END
LOCAL cGridRef,cWizHomePath,separator,cWizStyFile
IF THIS.FirstTime
  THIS.FirstTime = .f.
  THIS.DOCK(0)
ENDIF  
IF TYPE('THIS.oWindParent')='L'
  RETURN
ENDIF  

*IF TYPE('THIS.Parent') # "O"
*	RETURN
*ENDIF

IF SET("TALK") = "ON"
	SET TALK OFF
	THIS.oldTalk = "ON"
ELSE
	THIS.oldTalk = "OFF"
ENDIF
THIS.BMPPATH = oAriaApplication.BitMapHome &&SUBSTR(THIS.CMDEDIT.PICTURE,1,RAT('\',THIS.CMDEDIT.PICTURE))
THIS.InitVars()
THIS.ButtonRefresh()
THIS.NavRefresh()

cGridRef=THIS.GridRef
IF !EMPTY(m.cGridRef)
	* Change this if you desire to have the grid initially selected.
	* THISFORM.&cGridRef..SetFocus()
ENDIF

ENDPROC
PROCEDURE Refresh
**** Special Preview Mode Handling ****
*E000000,1 Hesham (Start)
*IF THIS.PreviewMode AND THIS.PreviewInit
*	THIS.PreviewInit = .F.
*	THIS.cmdAdd.Enabled = .F.
*	THIS.cmdEdit.Enabled = .F.
*	THIS.cmdDelete.Enabled = .F.
*	THIS.cmdFind.Enabled = .F.
*	THIS.cmdPrint.Enabled = .F.
*	THIS.cmdExit.Enabled = .F.
*	THIS.nWorkArea = SELECT()
*	THIS.GetGridRef()
*	THIS.SetAllProp()
*	THIS.NavRefresh()
*ENDIF
*E000000,1 Hesham (End)
ENDPROC
PROCEDURE Activate
IF _SCREEN.FORMCOUNT > 1
  IF (TYPE('THIS.PARENT.oWindParent')='O' AND !WEXIST(THIS.PARENT.oWindParent.NAME))
    THIS.oWindParent=.F.
    THIS.SETALL('ENABLED',.F.)
    THIS.CMDCALC.ENABLED = .T.
  ENDIF
ELSE    
  THIS.oWindParent=.F.
  THIS.SETALL('ENABLED',.F.)
  THIS.CMDCALC.ENABLED = .T.
ENDIF
ENDPROC
0)   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Class                                                                                               WINDOWS _RG30PGYYV 618752402
  �  �        2      /E  i�          �    �          �               COMMENT RESERVED                        �      �                                                           WINDOWS _RFJ0RY6V6 622006679�      �  �      \  �  �b  ތ          �  �  �          �               COMMENT RESERVED                        �                                                                   COMMENT RESERVED                        �,      �,                                                           WINDOWS _RXL0L65FS 622293226�C      �C  �C      w,      #*  9          ?,  '  L,          3?               COMMENT RESERVED                        E      �D                                                           WINDOWS _RYH0RSJZ6 629631317<  #?  �  $<      �A      Z�  ��          <  �  �B          �;               WINDOWS _R5S0NGX66 629631317�C  �B  �B  ->  >  �=                                                           WINDOWS _RYH0SWRE1 629631317�=  �=  �=  u=  ^=  =                                                           WINDOWS _RYH0TD1ZW 629631317�<      �<  �<  �<  o<              ��  ;<                                       COMMENT RESERVED                        �;      �;                                                           WINDOWS _RWV0QDPAL 642477236	;  A?  �@  �C      aZ      ��  M[          �:  �  �C          �:               COMMENT RESERVED                        �      f                                                           WINDOWS _RXZ0Z7SSA 644778322�@  �A  �A  �@      A      �  �.          �@  �)  �@          �:               COMMENT RESERVED                        �:      �:                                                           WINDOWS _S8P0O2Z20 658069956x9      [:  m:                                                                   WINDOWS _S8T0YG492 658732628�)  �:  }:  �-      B>      ��  Oy          5)  0  t�          ')               WINDOWS _S8T101Y3H 658343464i;  y;  �;  �;  �9  ;              �  �?                                       WINDOWS _S8T101ZXF 658343465=7  �9  �6  �6  �6  ɣ              ! Q?                                       WINDOWS _S8U17CK96 658732628%.  .  B)  �-  �-  C.      �?  �                                               COMMENT RESERVED                        )                                                                   WINDOWS _RXG1AF158 659443868�  h9  W9  :      �9      ��   o          }  �  �9          �               WINDOWS _RXG1AF158 622009004C9  39  #9  9  9  ^8                                                           WINDOWS _RXG1AM32G 622009004M8  =8  08  !8  .7  �7      �6                                                 WINDOWS _RXG1AM33C 622009937�6  �6  �6  w6  h6  M7      '6  �                                               WINDOWS _RXG1AM347 6220090046  6  �5  �5  �5  r5      15  �                                               WINDOWS _RXG1AM352 622009005 5  5  5  �4  �4  |4      ;4  �
                                               WINDOWS _RXG1AM35Y 622009005*4  4  6.  4  �3  �3      R3  �	                                               WINDOWS _RXG1AM36U 622009005?3  /3   3  3  *  �-      -  �                                               WINDOWS _RXG1AM37R 622009005*  �)  �)  �)  �)  O)      �(  E                                               WINDOWS _RXG1AM38N 622009005�(  �(  z(  k(  \(  (      �'  2                                               WINDOWS _RXG1AM39J 622009005�'  z'  k'  \'  M'  �&      �&                                                 WINDOWS _RXG1AM3AG 622009005{&  k&  \&  M&  >&  �%      %                                                 WINDOWS _RXG1AM3BC 622009005�  �  �  �  �  '%      ?  �                                               WINDOWS _RXH00H4NQ 622009005.          �      �                                                 WINDOWS _RXH021W1J 622009005�  �  �  �  ~  I                                                           COMMENT RESERVED                        �      v                                                           WINDOWS _R5S0NGX66 659443939�      h  W        �  H�  �          �  �  �          X               COMMENT RESERVED                        v                                                                   )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      ����    �   �                         ��   %   ]       w      q           �  U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK
 RightClick,     ��1 1                       .       )   �                        ���    �   �                         .�   %   m       �      �           �  U   	 ��C��� U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK Init,     ��
 RightClick<     ��1 � 2 1                                <   T       )   �                        ���    �   �                         .�   %   m       �      �           �  U   	 ��C��� U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK Init,     ��
 RightClick<     ��1 � 2 1                                <   T       )   �                        ���    �   �                         .�   %   m       �      �           �  U   	 ��C��� U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK Init,     ��
 RightClick<     ��1 � 2 1                                <   T       )   �                        ���    �   �                         .�   %   m       �      �           �  U   	 ��C��� U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK Init,     ��
 RightClick<     ��1 � 2 1                                <   T       )   �                        ���    �   �                         .�   %   m       �      �           �  U   	 ��C��� U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK Init,     ��
 RightClick<     ��1 � 2 1                                <   T       )   �                         Pixels      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ariavalidentriescombobox      1     ���    �   �                         .�   %   m       �      �           �  U   	 ��C��� U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK Init,     ��
 RightClick<     ��1 � 2 1                                <   T       )   �                         ����    �   �                         ��   %   ]       w      q           �  U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK
 RightClick,     ��1 1                       .       )   �                         ����    �   �                         ��   %   ]       w      q           �  U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK
 RightClick,     ��1 1                       .       )   �                         ����    �   �                         ��   %   ]       w      q           �  U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK
 RightClick,     ��1 1                       .       )   �                         	resizable      Class      1      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ariaadvancedgrid      Pixels      Class      1      ariagrid      ariaadvancedgrid      sfilefilter = ('')
order = ('')
key = ('')
defaultshortcut = ('TTTF')
filter = ('')
Name = "ariaadvancedgrid"
      grid      main.vcx      ����    �   �                         ��   %   ]       w      q           �  U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK
 RightClick,     ��1 1                       .       )   �                         custom     Afilefilter
order Specifies the controlling index tag for a Cursor object.
key
defaultshortcut
filter Excludes records that do not meet the criteria in the specified expression.
^afilter[1,7] 
^usershortcut[1,4] 
*buildfilefields 
*runmethod 
*addshortcut 
*clearshortcut 
*removeshortcut 
^afilefields[1,4] 
      ����    �   �                         ��   %   ]       w      q           �  U    ��C�  � � �� U  THIS PARENT
 RIGHTCLICK
 RightClick,     ��1 1                       .       )   �                         minitialresize
initialformheight
initialformwidth
addtoarray
setsize
loopthroughcontrols
acontrolstats
      	resizable      custom      )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      address      Pixels      15      ariashortcut      Pixels      Class      1      oHeight = 19
Width = 27
initialresize = .T.
initialformheight = 0
initialformwidth = 0
Name = "resizable"
      Class      ariacontainer      form      #addbar^
aobjects^
isbarchecked^
      custom      ariashortcut      \*showshortcut 
*addbar 
*getobjnumber 
^aobjects[1,1] 
*isbarchecked 
*setbarchecked 
      0Height = 17
Width = 17
Name = "ariashortcut"
      custom     )���                              "g   %   �       �      �           �  U  B 	 ��C���6 T�  � � ��  � � � �  � � �  � � ��� U  THIS PARENT	 TREERATIO TRVTREEVIEW WIDTH HORIZONTALSPACE Click,     ��1 � d2                       �       )                          _initialresize Is this the first time the controls are being adjusted?
initialformheight
initialformwidth
*adjustcontrols call from resize event of a form to adjust the placement and size of contained objects.
*addtoarray 
*setsize 
*loopthroughcontrols 
*reset Resets the Timer control so that it starts counting from 0.
^acontrolstats[1,5] 
      address      -Top = 33
Left = 54
Name = "Ariashortcut1"
      address      Ariashortcut1      custom      utility.vcx      ariashortcut      9PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      address      1      4     m���    T  T                        �   %   �           �          �  U  3 �  � ���� �� � � � �% T� �C� � C� .C� � ���\��( T� �C� � �C� .C� � ���\�� T� �� � �� T� �� �	 ��m SELECT &lcDescrip,&lcCodeField FROM &lcTableDtl WHERE &lcCodeField=&lcCODE INTO ARRAY ThisFormset.paCodes
 T� �
 ����# T� � �� ThisFormset.paCodes�� U  THISFORMSET PACODES
 LCTABLEDTL LCCODEFIELD LCCODE	 LCDESCRIP THIS CONTROLSOURCE HDRCODE CDESCRIP ROWSOURCETYPE	 ROWSOURCE Init,     ��1 1Q��21                       	      )   T                        LHeight = 15
Left = 2
Top = 4
Width = 34
TabIndex = 2
Name = "lblAdd1"
      lblAdd1      label      main.vcx      	arialabel      _PROCEDURE Init
DODEFAULT()
ENDPROC
PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      address      txtAdd6      textbox      main.vcx      ariatextbox      4     
 ���    
  
                        *   %   i      n	  G   �          �  U  h  %��  � ��C �% ��C� INITIALIZE_AND_ADJUST�  � �� T�  � �-�� �a � ��C� ADJUST�  � �� � U  THIS INITIALRESIZE LOOPTHROUGHCONTROLS ��  � T� �C� � ����& T� � �� ������  � � � ��& T� � �� ������  � � � ��& T� � �� ������  � � � ��& T� � �� ������  �	 � � ��E T� � �� �����CC� oControl.FontSizeb� U� � � �  �
 6�� � � �� ������ U  OCONTROL NLEN THIS ACONTROLSTATS TOP INITIALFORMHEIGHT LEFT INITIALFORMWIDTH HEIGHT WIDTH FONTSIZE�  ��  � �" T�  � �� � C � �� � ��" T�  � �� � C � �� � ��" T�  � �� � C � �� � ��$ %��  �	 � Textbox Spinner
��� �" T�  � �� � C � �� � �� � U
  OCONTROL NPOS TOP THISFORM HEIGHT THIS ACONTROLSTATS LEFT WIDTH	 BASECLASS� ��  � T�  �C�  f�� T� �C� DECIMALv�� G(���� T� �� �� T� � �a�� ��� ���(�� � ���� T� �C �� � � ��� %�� �	 �z Commandbutton Combobox Checkbox Listbox Form Grid Textbox Label Shape Editbox Olecontrol Pageframe Image Spinner Container���� T� �� ��� H�1���& ��  � INITIALIZE_AND_ADJUST��}� ��C � �
 � �� ��C �  � �
 � �� ��  � ADJUST���� ��C �  � �
 � �� � �' %�C �� � � �	 �	 Pageframe����& ��� ���(�C �� � � � ����! ��C �� C �� � � � ���� ��� ���(��� ����� %�C �� �� �	 �z Commandbutton Combobox Checkbox Listbox Form Grid Textbox Label Shape Editbox Olecontrol Pageframe Image Spinner Container��~� T� �� ��� H���z�& ��  � INITIALIZE_AND_ADJUST��G� ��CC �� �� �
 � �� ��CC �� ��  � �
 � �� ��  � ADJUST��z� ��CC �� ��  � �
 � �� � � �� �� �� � �� T� � �-�� G(�� �� U  CTASK NOLDDECIMAL NPOS THISFORM
 LOCKSCREEN I CONTROLCOUNT OCONTROL CONTROLS	 BASECLASS THIS
 ADDTOARRAY SETSIZE J	 PAGECOUNT PAGES K'  T�  � �a�� �  � ������� U  THIS INITIALRESIZE ACONTROLSTATS)  T�  � �� � �� T�  � �� � �� U  THIS INITIALFORMHEIGHT THISFORM HEIGHT INITIALFORMWIDTH WIDTH adjustcontrols,     ��
 addtoarray�     �� setsizeJ    �� loopthroughcontrols`    �� reset�    �� Init�    ��1 Q� � aA 2 q QaaaaQ�3 � !!!A!A ; q � b� � � �q�� aQqQA A rb�a	� a��q�A A A A A A A � � 2 � a2 111                       �         �   �  	      �  �        �  W
  &   A   s
  �
  X   D   �
    \    )   
                        PHeight = 21
Left = 97
TabIndex = 13
Top = 96
Width = 224
Name = "txtAdd6"
      _PROCEDURE Init
DODEFAULT()
ENDPROC
PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      PHeight = 21
Left = 259
TabIndex = 12
Top = 72
Width = 62
Name = "txtAdd5"
      address      txtAdd5      textbox      main.vcx      ariatextbox      _PROCEDURE Init
DODEFAULT()
ENDPROC
PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      PHeight = 21
Left = 97
TabIndex = 11
Top = 72
Width = 110
Name = "txtAdd4"
      address      txtAdd4      textbox      main.vcx      ariatextbox      _PROCEDURE Init
DODEFAULT()
ENDPROC
PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      PHeight = 21
Left = 97
TabIndex = 10
Top = 48
Width = 224
Name = "txtAdd3"
      address      txtAdd3      textbox      main.vcx      ariatextbox      _PROCEDURE Init
DODEFAULT()
ENDPROC
PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      ariaexplorer      Pixels      Class      shape      OHeight = 21
Left = 97
TabIndex = 9
Top = 24
Width = 224
Name = "txtAdd2"
      address      1      txtAdd2      ariacontainer      textbox      main.vcx      ariatextbox      address     PROCEDURE Init
DIMENSION ThisFormset.PaCodes[1]
LOCAL lcTableDtl,lcCodeField,lcCODE,lcDescrip
lcCodeField = SUBSTR(This.ControlSource,(ATC(".",ALLTRIM(This.ControlSource))+1))
lcTableDtl  = SUBSTR(This.ControlSource,1,(ATC(".",ALLTRIM(This.ControlSource))-1))
lcCODE      = This.HdrCode
lcDescrip   = This.cDescrip
SELECT &lcDescrip,&lcCodeField FROM &lcTableDtl WHERE &lcCodeField=&lcCODE INTO ARRAY ThisFormset.paCodes
This.RowSourceType = 5
*This.ControlSource = ""
This.RowSource     = "ThisFormset.paCodes"
ENDPROC
      Class      #hdrcode
cdescrip
^pacodes[1,0] 
      ZRowSourceType = 5
Height = 24
Style = 2
Width = 100
hdrcode = 
Name = "hdrdtlcombo"
      address      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      _PROCEDURE Init
DODEFAULT()
ENDPROC
PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      NHeight = 21
Left = 97
TabIndex = 8
Top = 1
Width = 224
Name = "txtAdd1"
      ariaexplorer      ariaexplorer      szrSizerline      utility.vcx      	sizerline      label      jTop = 3
Left = 152
Height = 304
Width = 7
csizertype = Vertical
latend = .T.
Name = "szrSizerline"
     T���    ;  ;                        y<   %   �      �  "   �          �  U  � �  � ������� T�  � ��  ��7 T�  � �CC�  � ��
 C�  � �
	� C� f� C�  � f6��1 %�C� OARIAAPPLICATIONb� O�
 C� � �
	��N� �� � � � T� �CW�� T� �C�	 ��[ USE (oAriaApplication.Datadir+'codes') AGAIN ALIAS &lcTempAlias ORDER TAG CCODE_NO IN 0

 F�� �� T� �� �� %�C� N�  � ����� ~$+��
 � � N�  � ���� %�� � N���� T� �� ��� �  � �� ����� T�  � �� ������ �� T�  � �� ������ �� � � � %�C� D�  � ���� T�  � �� �� T�  � �� �� �5� T�  � �C���  � �� � Q�� ��
 F�� �� � T�  � �� THIS.LACODES�� T�  � ���� ��C�  � �� U  THIS LACODES
 CODESFIELD CONTROLSOURCE OARIAAPPLICATION ACTIVECOMPANYID LCTEMPALIAS LNCOUNT LNALIAS
 GFTEMPNAME CDEFCODE	 CFLD_NAME	 CRLTFIELD CDISCREP CCODE_NO VALUE DISPLAYVALUE	 ROWSOURCE BOUNDCOLUMN REFRESH Init,     ��1 aq� � � �� � q�!a��A A A q� �A � � A �� 1                       0      )   ;                        txtAdd1      textbox      main.vcx      ariatextbox      9PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      aCaption = "Label6"
Height = 15
Left = 2
Top = 99
Width = 34
TabIndex = 7
Name = "lblAdd6"
      address      lblAdd6      main.vcx      	arialabel      9PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      aCaption = "Label3"
Height = 15
Left = 2
Top = 51
Width = 34
TabIndex = 4
Name = "lblAdd3"
      address      lblAdd3      label      main.vcx      	arialabel      9PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      aCaption = "Label4"
Height = 15
Left = 2
Top = 75
Width = 34
TabIndex = 5
Name = "lblAdd4"
      address      lblAdd4      label      main.vcx      	arialabel      9PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      address      lblAdd5      label      main.vcx      ariaexplorer      lstListview      
olecontrol      	arialabel      9PROCEDURE RightClick
This.Parent.RightClick()
ENDPROC
      address      listview      cCaption = "Label5"
Height = 15
Left = 212
Top = 75
Width = 34
TabIndex = 6
Name = "lblAdd5"
      aCaption = "Label2"
Height = 15
Left = 2
Top = 27
Width = 34
TabIndex = 3
Name = "lblAdd2"
      lblAdd2      label      main.vcx      	arialabel      �RowSourceType = 2
RowSource = ""
Height = 24
Left = 51
Style = 2
TabIndex = 1
Top = 2
Visible = .F.
Width = 20
BoundTo = .T.
Name = "cboCountry"
      address      
cboCountry      combobox      main.vcx      ariacombobox      	container      main.vcx      
olecontrol      utility.vcx      ariaexplorer      ?privateintfile
bars
replacedefaultcountry
*adjustcontrols 
      [Width = 322
Height = 119
BorderWidth = 0
replacedefaultcountry = .T.
Name = "address"
      
olecontrol      wtoolbar      	container      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      	ariacodes      Pixels      main.vcx      Pixels      Class      ariacombobox      DTop = 5
Left = 5
Height = 300
Width = 139
Name = "trvTreeview"
      treeview      utility.vcx      
olecontrol      trvTreeview      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ariaprogressbar      Pixels      Class      ariaform      ariaprogressbar      ,OLEObject = C:\WINDOWS\SYSTEM\THREED32.OCX
      GTop = 52
Left = 5
Height = 29
Width = 453
Name = "objProgressBar"
      ariaprogressbar      objProgressBar      
olecontrol      
olecontrol      GCaption = "Second Label"
Left = 7
Top = 29
Name = "lblSecondLabel"
      ariaprogressbar      lblSecondLabel      label      main.vcx      	arialabel      UFontBold = .T.
Caption = "First Label"
Left = 7
Top = 11
Name = "lblFirstLabel"
      ariaprogressbar      lblFirstLabel      �Width = 527
Height = 310
horizontalspace = 5
verticalspace = 5
portions = 1/3
treeviewminimumwidth = 0
listviewminimumwidth = 0
resizewhilemoving = .F.
treeratio = 0
minimumheight = 0
Name = "ariaexplorer"
      main.vcx      Pixels      main.vcx      ?OLEObject = \\ARIANT\WORKING\ARIA27V\COM\OCX\ARIACONTROLS.OCX
      ?OLEObject = \\ARIANT\WORKING\ARIA27V\COM\OCX\ARIACONTROLS.OCX
      �PROCEDURE Click
DoDefault()

This.Parent.TreeRatio = This.Parent.trvTreeview.Width /;
                        (This.Parent.Width -;
                         (This.Parent.HorizontalSpace * 3))

ENDPROC
      combobox      Class      ariacombobox      	ariacodes      codesfield
^lacodes[1,2] 
      hRowSourceType = 5
RowSource = "THIS.LACODES"
Height = 24
Style = 2
Width = 100
Name = "ariacodes"
      combobox      main.vcx     Height = 85
Width = 465
DoCreate = .T.
BorderStyle = 2
Caption = ""
ControlBox = .F.
Closable = .F.
MaxButton = .F.
MinButton = .F.
Movable = .F.
MinHeight = 55
MinWidth = 150
autorelease = .T.
seplength = 5
totalprogress = 0
Name = "ariaprogressbar"
      main.vcx      label      �autorelease Determines whether a FormSet is released when the last Form in the FormSet is released.
seplength
totalprogress
*currentprogress 
*finishprogress 
      	arialabel      hdrdtlcombo      combobox      combobox      ariavalidentriescombobox      �sourcepath Source file path
sourcefield Row Source field
rowdelimiter Row delimiter
columndelimiter Column delimiter
addna
displayvalidentrycodes
^asourcearray[1,2] Row source array
*getvalidentries Gets the valid entries from the file SYDFIELD
      !Arial, 0, 9, 5, 15, 12, 13, 3, 0
      hdrdtlcombo     *PROCEDURE buildfilefields
Local lnCount
DIMENSION THIS.aFileFields[THIS.ColumnCount,4]
FOR lnCount = 1 TO ALEN(THIS.aFileFields,1)
  THIS.aFileFields[lnCount,1] = THIS.Columns[lnCount].Header1.Caption
  THIS.aFileFields[lnCount,2] = THIS.Columns[lnCount].ControlSource
  THIS.aFileFields[lnCount,3] = TYPE(EVAL('THIS.aFileFields[lnCount,2]'))
  IF  ATC('(','THIS.aFileFields[lnCount,3]')>0 OR ATC('+','THIS.aFileFields[lnCount,3]')>0
    THIS.aFileFields[lnCount,4] = 'E'
  ELSE
    THIS.aFileFields[lnCount,4] = 'F'
  ENDIF  
ENDFOR
ENDPROC
PROCEDURE runmethod
lParameters toReference,tcMethod
Local lcCommand
lcCommand = IIF(TYPE('toReference')='O','toReference','')
lcCommand = lcCommand+IIF(EMPTY(lcCommand),'='+tcMethod,'.'+tcMethod)
IF !EMPTY(lcCommand)
  &lcCommand
ENDIF

ENDPROC
PROCEDURE addshortcut
lParameters tcCation,toObject,tcMethod,tcStatus,tnIndex
IF PCOUNT()=0 OR TYPE('tcCation')<>'C'
  RETURN
ENDIF
tcStatus = IIF(TYPE('tcStatus')='C',tcStatus,'T')
LOCAL llAdd
llAdd = IIF(TYPE('tnIndex')='N',.F.,.T.)
tnIndex = IIF(TYPE('tnIndex')='N',tnIndex,ALEN(THIS.UserShortCut,1)+IIF(EMPTY(THIS.UserShortCut[1,1]),0,1))
IF !llAdd AND tnIndex > ALEN(THIS.UserShortCut,1)
  RETURN
ELSE 
  IF llAdd
    DIMEN  THIS.UserShortCut[tnIndex,ALEN(THIS.UserShortCut,2)]
  ENDIF
  THIS.UserShortCut[tnIndex,1] = tcCation
  THIS.UserShortCut[tnIndex,2] = toObject
  THIS.UserShortCut[tnIndex,3] = tcMethod
  THIS.UserShortCut[tnIndex,4] = tcStatus
ENDIF  
ENDPROC
PROCEDURE clearshortcut
DIMEN THIS.UserShortCut[1,4]
STORE .F. TO THIS.UserShortCut
ENDPROC
PROCEDURE removeshortcut
lParameters tnIndex
IF TYPE('tnIndex')<>'N' OR ALEN(THIS.UserShortCut,1) < tnIndex OR tnIndex=0
  RETURN
ENDIF
=ADEL(THIS.UserShortCut,tnIndex)
IF ALEN(THIS.UserShortCut,1)>1
  DIME THIS.UserShortCut[ALEN(THIS.UserShortCut,1)-1,ALEN(THIS.UserShortCut,2)]
ENDIF
ENDPROC
PROCEDURE AfterRowColChange
LPARAMETERS nColIndex
IF TYPE('THISFORMSET')='O'
   THISFORMSET.TopFile = BOF()
   THISFORMSET.EndFile = EOF()
   IF !EMPTY(THISFORMSET.FormHasToolBar) AND TYPE('oAriaApplication.oToolBar')='O'
      oAriaApplication.oToolBar.TopFile = THISFORMSET.TopFile   
      oAriaApplication.oToolBar.EndFile = THISFORMSET.EndFile 
      oAriaApplication.oToolBar.EditMode = .F.
      oAriaApplication.oToolBar.AddMode = .F.
      oAriaApplication.oToolBar.ButtonRefresh()
      oAriaApplication.oToolBar.NavRefresh()
    ELSE
      THISFROMSET.REFRESHALL()      
      THISFROMSET.REFRESH()  
    ENDIF
ENDIF    
ENDPROC
PROCEDURE Init
IF THIS.RECORDSOURCETYPE < 2
  THIS.FILTER = FILTER(THIS.RECORDSOURCE)
  THIS.ORDER = ORDER(THIS.RECORDSOURCE)
ELSE
  THIS.DefaultShortCut = 'FFTF'  
ENDIF  

ENDPROC
PROCEDURE RightClick
Local lnChoice,lcOrder,lcQuikFind,lcFind,lcFilter,lcUserDefBars,lcUserDefAcc,;
      lnCount,lcAlias
lcAlias = ALIAS()
lcFind = SUBSTR(THIS.DefaultShortCut,1,1)
lcFilter = SUBSTR(THIS.DefaultShortCut,2,1)
lcOrder = IIF(!EMPTY(TAG(1,lcAlias)) AND EMPTY(THIS.KEY),'T','F')
lcOrder = IIF(lcOrder = 'T' AND RIGHT(THIS.DefaultShortCut,1)='T','T','F')
lcQuikFind = IIF(!EMPTY(TAG(1,lcAlias)),'T','F')
lcQuikFind = IIF(lcQuikFind = 'T' AND SUBSTR(THIS.DefaultShortCut,3,1)='T','T','F')
STORE '' TO lcUserDefBars,lcUserDefAcc
IF !EMPTY(THIS.USERSHORTCUT[1,1])
  lcUserDefBars = ',\-'
  lcUserDefAcc =  'T'
  FOR lnCount = 1 TO ALEN(THIS.USERSHORTCUT,1)
    lcUserDefBars = lcUserDefBars +','+ THIS.USERSHORTCUT[lnCount,1]
    lcUserDefAcc = lcUserDefAcc + THIS.USERSHORTCUT[lnCount,4]
  ENDFOR
ENDIF
LOCAL oShortcut
oShortCut = CREATEOBJECT('ARIASHORTCUT')
lnChoice = oShortcut.ShowShortCut(This,"\<Find,Fi\<lter,\<Quik Find,\<Order"+lcUserDefBars,lcFind+lcFilter+lcQuikFind+lcOrder+lcUserDefAcc)
IF lnChoice = 1 OR lnChoice = 2
  IF EMPTY(THIS.aFileFields[1,1])
    THIS.BuildFileFields()
  ENDIF
ENDIF
DO CASE
  CASE lnChoice = 1
    SELECT (lcAlias)
    Local oFilter,lcKey
    PRIVATE laFileFields
    IF !EMPTY(THIS.KEY)
      lcKey = IIF(ATC(LEFT(THIS.Key,1)+',',THIS.Key)>0,SUBSTR(THIS.Key,1,ATC(LEFT(THIS.Key,1)+',',THIS.Key)),THIS.Key)
    ELSE
      lcKey = ['']
    ENDIF  
    =ACOPY(THIS.aFileFields,laFileFields)
    oFilter = CREATEOBJECT('ARIASEARCHFORM')
    oFilter.Key = lcKey
    oFilter.ariaSearchContainer1.InitializeFields(@laFileFields)
    oFilter.SHOW()
  CASE lnChoice = 2
    SELECT (lcAlias)  
    Local oFilter
    PRIVATE laFilter,laFileFields
    =ACOPY(THIS.aFileFields,laFileFields)
    =ACOPY(THIS.aFilter,laFilter)    
    oFilter = CREATEOBJECT('ARIASEARCHFORM','laFilter',THIS.FILEFILTER)
    oFilter.ariaSearchContainer1.InitializeFields(@laFileFields,@laFilter)
    oFilter.SHOW()
    DIMEN this.aFilter[ALEN(laFilter,1),7]
    =ACOPY(laFilter,THIS.aFilter)
  CASE lnChoice = 3
    SELECT (lcAlias)  
    LOCAL lcKey
    IF !EMPTY(THIS.KEY)
      lcKey = IIF(ATC(LEFT(THIS.Key,1)+',',THIS.Key)>0,SUBSTR(THIS.Key,1,ATC(LEFT(THIS.Key,1)+',',THIS.Key)),THIS.Key)
    ELSE
      lcKey = ['']
    ENDIF  
    oFilter = CREATEOBJECT('ARIASEEK',ALIAS(),this,.t.,THIS.ORDER,lcKey)
    oFilter.SHOW()    
  CASE lnChoice = 4
    SELECT (lcAlias)  
    oFilter = CREATEOBJECT('ARIASEEK',ALIAS(),this,.f.)
    oFilter.SHOW() 
  CASE lnChoice > 5
    THIS.RUNMETHOD(THIS.USERSHORTCUT[lnChoice-5,2],THIS.USERSHORTCUT[lnChoice-5,3])
ENDCASE

ENDPROC
      �BoundColumn = 2
RowSourceType = 5
RowSource = "This.aSourceArray"
Style = 2
sourcepath = 
sourcefield = 
rowdelimiter = |
columndelimiter = ~
addna = .F.
displayvalidentrycodes = .F.
Name = "ariavalidentriescombobox"
     ����    |  |                        os   %   �      #  .   �          �  U   4�  � �� � � � � �+ T� �CC� lcVldEntb� C� C� � �  6��, %�C� �
� C� � � SYDFIELD.DBF0	��� � T� �� � � SYDFIELD��< USE &lcFile IN 0 AGAIN ALIAS (lcTmpName) ORDER CFLD_NAME
 �� lcVldEnt = IIF(TYPE('lcVldEnt') = 'C' , lcVldEnt , IIF(!EMPTY(lcTmpName) .AND. USED(lcTmpName) .AND. SEEK(PADR(UPPER(ALLTRIM(This.SourceField)) , 10) , lcTmpName) , &lcTmpName..mVEntries , ''))
� T� �CC� This.RowDelimiterb� C�	 C� �	 �� �  �V CC� �	 ���\CC� This.ColumnDelimiterb� C�	 C� �
 �� �  � CC� �
 ���\66�� %�C�  �
��� � ������� ��C �  �  � � ��" � � �C�� ����C�� ���� ��C�� � � ��� %�� � ��� �� ���(�C� � ���� �; T� � �� �����C � �� � � -C � �� � �� �� � �6� � � ������� T� � ��  �� � %�� � ����& %�C� � ����
 C� � �
����" � � �C� � �������� ��C� � ���� � T� � �������� N/A�� T� � �������C�X�� � %�C� �
� C� �	��� Q�� �� � U  LCVLDENT	 LCTMPNAME LCDELEMT LCFILE
 LATMPARRAY LNCOUNT
 GFTEMPNAME THIS
 SOURCEPATH ROWDELIMITER COLUMNDELIMITER GFSUBSTR ASOURCEARRAY DISPLAYVALIDENTRYCODES ADDNA@	 ��C���l T�  � �CC� This.SourceFieldb� C�	 C�  � ��. CC�  � �� �  � C�  � C� .�  � �\6� �  � 6��k T�  � �CC� This.SourcePathb� C�	 C�  � ��. CC� oAriaApplicationb� O�	 � � � �  6� �  � 6�� %�C�  � ��	 C�  � ���$� �  � ������� T�  � ��  �� �9� ��C�  � �� � U  THIS SOURCEFIELD CONTROLSOURCE
 SOURCEPATH OARIAAPPLICATION SYSPATH ASOURCEARRAY GETVALIDENTRIES getvalidentries,     �� Init�    ��1 q r����A V(	2a"1��B A � bA a!!A ��A �� A 3 � ���a� � A 2                       
     $   9
  �  E    )   |                       PROCEDURE showshortcut
LPARAMETERS toObject, tcBarsStr, tcBarsStat
LOCAL lnObjNo, lnBarsNo, lnCurBar, lnFrom, lnToCh, lcBar, lcSetTo

IF PARAMETERS()=0 OR TYPE("toObject")#"O" OR ;
   TYPE("tcBarsStr")#"C" OR EMPTY(tcBarsStr)
  RETURN 0
ENDIF
tcBarsStat = IIF(TYPE("tcBarsStat")#"C","",tcBarsStat)
lnObjNo    = This.GetObjNumber(toObject, tcBarsStr)

DEFINE POPUP AriaPopUp FROM MROW(),MCOL() MARGIN RELATIVE SHORTCUT
lnBarsNo   = OCCURS(",", tcBarsStr) + 1
tcBarsStat = PADR(tcBarsStat,lnBarsNo,"T")
IF lnBarsNo = 1
  This.AddBar(tcBarsStr, 1, tcBarsStat, toObject.Name)
ELSE
  lnCurBar = 1
  lnFrom   = 1
  lnToCh   = ATC(",", tcBarsStr, 1)
  DO WHILE lnCurBar <= lnBarsNo
    lcBar    = SUBSTR(tcBarsStr, lnFrom, lnToCh-lnFrom)
    This.AddBar(lcBar, lnCurBar, tcBarsStat, toObject.Name)
    lnCurBar = lnCurBar + 1
    lnFrom   = lnToCh   + 1
    lnToCh   = ATC(",", tcBarsStr, lnCurBar)
    lnToCh   = IIF(lnToCh=0, LEN(tcBarsStr)+1, lnToCh)
  ENDDO
ENDIF
ON SELECTION POPUP AriaPopUp DEACTIVATE POPUP AriaPopUp
Activate POPUP AriaPopUp
Release POPUP AriaPopUp

lcSetTo = IIF(This.IsBarChecked(toObject.Name, BAR()), "F", "T")
This.SetBarChecked(toObject.Name, BAR(), lcSetTo)

RETURN BAR()

ENDPROC
PROCEDURE addbar
LPARAMETERS tcBar, tnCurBar, tcBarsStat, tcObjName
LOCAL llSetMark

tcBar     = ALLTRIM(tcBar)
llSetMark = .F.
IF LEFT(tcBar,1) = "~"
  tcBar     = SUBSTR(tcBar, 2)
  llSetMark = .T.
ENDIF

DEFINE BAR tnCurBar OF AriaPopUp PROMPT (tcBar)
tcBarStat = SUBSTR(tcBarsStat,tnCurBar,1)
SET SKIP OF BAR tnCurBar OF AriaPopUp (tcBarStat = "F")

IF llSetMark AND This.IsBarChecked(tcObjName, tnCurBar)
  SET MARK OF BAR tnCurBar OF AriaPopUp TO .T.
ENDIF
ENDPROC
PROCEDURE getobjnumber
LPARAMETERS toObject, tcBarsStr
LOCAL lnObjNo, lnListLen, lnObjNo, lnBarsNo

lnObjNo  = ASCAN(This.aObjects, toObject.Name)
lnBarsNo = OCCURS(",", tcBarsStr) + 1
IF lnObjNo = 0
  lnListLen = ALEN(This.aObjects,1)
  lnObjNo   = lnListLen 
  This.aObjects[lnListLen,1] = toObject.Name
  This.aObjects[lnListLen,2] = REPLICATE("F",lnBarsNo)
  DIMENSION This.aObjects[lnListLen+1,2]
  This.aObjects[lnListLen+1,1] = SPACE(0)
  This.aObjects[lnListLen+1,2] = SPACE(0)
ELSE
  lnObjNo = ASUBSCRIPT(This.aObjects,ASCAN(This.aObjects,toObject.Name),1)
  This.aObjects[lnObjNo,2] = PADR(This.aObjects[lnObjNo,2], lnBarsNo, "F")
ENDIF

RETURN (lnObjNo)
ENDPROC
PROCEDURE isbarchecked
LPARAMETERS tcObjName, tnBarNumber
LOCAL llRetVal, lnObjNo

llRetVal = .F.
lnObjNo  = ASCAN(This.aObjects, tcObjName)
IF lnObjNo > 0
  llRetVal = ("T" = SUBSTR(This.aObjects[lnObjNo+1],tnBarNumber,1))
ENDIF

RETURN(llRetVal)
ENDPROC
PROCEDURE setbarchecked
LPARAMETERS tcObjName, tnBarNumber, tcSetTo
LOCAL lnObjNo

lnObjNo = ASCAN(This.aObjects, tcObjName)
IF lnObjNo > 0
  This.aObjects[lnObjNo+1] = ;
  STUFF(This.aObjects[lnObjNo+1], tnBarNumber, 1, tcSetTo)
ENDIF
ENDPROC
PROCEDURE Init
DIMENSION This.aObjects[1,2]
This.aObjects = SPACE(0)
ENDPROC
PROCEDURE Destroy
Deactivate MENU AriaPopUp
ENDPROC
     
G���    .
  .
                        ��   %   �      �	  C   	          �  U  �  5�  � �� ���(����� � T� �C� �Z�� T�  �C� This.lblAdd� ���* T�  � �C� � � .cPart� � Lab���- T�  � �C�  � �CC�  � �� �  � � :6�� T�  �C� This.txtAdd� ���* T�  � �C� � � .nPart� � Len��� �� T�  ���� U  OTMP LNI LCOBJNUM CAPTION THIS PRIVATEINTFILE	 MAXLENGTH� 	 ��C��� %��  � � � ��� � ��C� � �  � ���0 T� �CC� � f�� ARIAFORMSET�
 C� �	 �
	��' %��  �
 � � 	� � � � EA	��� � T� �CC�  � � �f��< REPLACE &lcReplThis WITH oAriaApplication.DefaultCountry
 � � ��C�  � �� U  THIS
 CBOCOUNTRY	 LISTINDEX OARIAAPPLICATION DEFAULTCOUNTRY PRIVATEINTFILE LLCHECKMODE THISFORMSET CLASS FORMHASTOOLBAR REPLACEDEFAULTCOUNTRY
 ACTIVEMODE
 LCREPLTHIS CONTROLSOURCE ADJUSTCONTROLS� 	 ��C��� T�  � �C� �� T� �� � ��1 Q�  �� � sycInt����  � ���	 cContCode� T�  � � ��  � �� T�  �	 �C� X�� F��  � �� ~�� �5 T�  �	 ��  �	 CC�  �	 �� � ~� � ,~6CC�
 ���� � ��C �  �  �	 �  � � �� U  THIS PRIVATEINTFILE
 GFTEMPNAME	 LCSYSPATH OARIAAPPLICATION SYSPATH	 CCONTCODE
 CBOCOUNTRY	 ROWSOURCE BARS
 CCONT_DESC ARIASHORTCUT1 GETOBJNUMBER� 5�  � � � � � �	 ��C���0 T�  �CC� � f�� ARIAFORMSET�
 C� � �
	�� T� �C� X��$ �� ���(�C� ,�	 �
 ����� � ��C�	 �  � � F�	 � � �� %��  ��� �* T� �� C� � � EA� � T� � F6�� �� � T� �� � T�� � ��% T� �CCC�	 � � .cCont_DescΛ��� T� �C� ~� �	 �
 ���� T� �C� ,C�	 �
 � =����" T� �C�	 �  � � T�	 � � ��" T� �C �	 �	 �
  � �	 � � �� %�� � ����3 T� �C� �� �� C� ,�	 �
 � ���6�� T� �C�	 �
 � \�� T� �C� ,� ����& T� �C� C� � � C� >� � 6=�� T� �CCCC� � ~�� ,��f�� F��	 � �� -�CC� �f� ��' T�	 � � �C�	 � � .cCont_Code��� T�	 � � �C� ��� ��C�	 � �� � U  LLCHECKMODE LNBAR LCSTAT	 LCCURCONT LNPOS LNBARNO THISFORMSET CLASS FORMHASTOOLBAR THIS BARS ARIASHORTCUT1 SETBARCHECKED NAME
 ACTIVEMODE PRIVATEINTFILE	 LLNOTHING SHOWSHORTCUT	 LCSUBBARS	 LCSELCONT
 CCONT_DESC
 CBOCOUNTRY VALUE TXTADD6 ADJUSTCONTROLS  Q��  � �� U  THIS PRIVATEINTFILE adjustcontrols,     �� Refreshn    �� Init6    ��
 RightClick�    �� Destroy�    ��1 q r!�����A � 2 � rAqq�A A � 3 � a"� � RA �2 �� � A�� �� !A A R��!!1Qaa�� !qQ� A 3 � 1                       �        �  �        �  �     #   �  �
  .   A       T    )   .
                       +���                              %�   %   �      i  \   �          �  U  3  ��  � T� � ���  �� T� � � �� � 
�� U  VNEWVAL THIS RESIZEWHILEMOVING SZRSIZERLINE LATEND"  B��  � �  � �  � ��� U  THIS TREEVIEWMINIMUMWIDTH LISTVIEWMINIMUMWIDTH HORIZONTALSPACE  B��  � �� U  THIS LSTLISTVIEW  B��  � �� U  THIS TRVTREEVIEW4  ��  � T� � � ���  �� T� � � ���  �� U  VNEWVAL THIS TRVTREEVIEW CONTROLREFRENCE LSTLISTVIEWA	 ��C��� T�  � �� �� ��C�  � � �� ��C�  � � ��; �� � � � �	 �
 � � � � � � � � �) T� �CCC�  � �C� /�  � �\�g��* T� �� CCC�  � C� /�  � �\�g�� T�  � �� � �� T�  � �� �� T� ��  � �� T� ��  � ��& T�	 ��  � �  � �� � �� T�
 ��  � �  � ��� T� �� �	 ��� T� �� �� T� ��  � �� T� ��  � �� T� �� � ��� T� ��  � ��  T� ��  � �  � ��	 �� T� ��  � �  � ���  ��C �  �  �	  �
 �  � � ��  ��C �  �  �  � �  � � ��  ��C �  �  �  � �  � � ��6 T�  � � �������� This.Parent.trvTreeview��" T�  � � ��������  � �� T�  � � �������� ��6 T�  � � �������� This.Parent.lstListview��" T�  � � ��������  � �� T�  � � �������� �� U  THIS	 TREERATIO LSTLISTVIEW SETFOCUS TRVTREEVIEW
 LNPORTIONS LNTREEPORTION LNTREEVIEWLEFT LNTREEVIEWTOP LNTREEVIEWWIDTH LNTREEVIEWHEIGHT LNSIZERLEFT
 LNSIZERTOP LNSIZERWIDTH LNSIZERHEIGHT LNLISTVIEWLEFT LNLISTVIEWTOP LNLISTVIEWWIDTH LNLISTVIEWHEIGHT PORTIONS BORDERWIDTH HORIZONTALSPACE VERTICALSPACE WIDTH HEIGHT MOVE SZRSIZERLINE ALEFTTOP TREEVIEWMINIMUMWIDTH
 ARIGHTDOWN LISTVIEWMINIMUMWIDTH}	 ��C���; ��  � � � � � � � � �	 �
 � � � � J�-�(�  � �$ T� �� � � � �� � �� %�� � � ��� � T� �� � �� T�  �a�� �  T� �� � � � �� �� %�� � � ��^� T� �� � �� T� �a�� %��  
��Z�  T� �� � � � �� �� %�� � � ��V� T� �� � �� T�  �a�� � � � %��  � � 	����  T� � �� � � � ��� � %�� � � � ���� T� � �� � �� � T� �� � � �� T� �� � � �� T� �� � � � ��� T� �� � ��� T� �� �� T� �� � � �� T�	 �� � �� T�
 �� � ��� T� �� � � �� T� �� � � � ���  ��C �  �  �  � � � � ��  ��C �  �  �  �	 � � � ��  ��C �
  �  �  � � � � ��F T� �CC� ThisForm.ActiveControlb� O�
 C� � �
	�	 � � � �6�� ��C� � �  �� ��C� � �  �� %�C� �
��v� ��C� �  �� � U!  LLTREEVIEWMINIMUM LLLISTVIEWMINIMUM LNTREEVIEWLEFT LNTREEVIEWTOP LNTREEVIEWWIDTH LNTREEVIEWHEIGHT LNSIZERLEFT
 LNSIZERTOP LNSIZERWIDTH LNSIZERHEIGHT LNLISTVIEWLEFT LNLISTVIEWTOP LNLISTVIEWWIDTH LNLISTVIEWHEIGHT THIS WIDTH HORIZONTALSPACE	 TREERATIO TREEVIEWMINIMUMWIDTH LISTVIEWMINIMUMWIDTH HEIGHT MINIMUMHEIGHT TRVTREEVIEW LEFT TOP VERTICALSPACE SZRSIZERLINE LSTLISTVIEW MOVE	 OFOCUSOBJ THISFORM ACTIVECONTROL SETFOCUS resizewhilemoving_assign,     �� minimumwidth_access�     �� listviewreference_access    �� treeviewreference_access*    �� controlrefrence_assignP    �� Init�    �� Resize�    ��1 q "r3 �3 � 3 � 3 q RQ3 � ���Bb�R� R�b!�b!�3 � �� CB� A A� � B� A A A BA r1A 21�R� 1R1�d� A 2                    $   �         �   �  	      �       	   1  }        �  N        i  �      .   �  J  \    )                          T���    ;  ;                        L   %         �  '   d          �  U  �  ��  � %�C�	 lnCurrentb� N��( � B� � %��  � � ��i �) T�  �C� � �	 � � � C�  � � G6�� �! T� � � �C�  � � �d8��! %�� � � � � � �d	��� � T� � �-�� � U 	 LNCURRENT THIS TOTALPROGRESS AUTORELEASE THISFORM OBJPROGRESSBAR FLOODPERCENT VISIBLEr  ��  � � T�  �� � � �� %��  �d��] � �� ��  �(��d��Y � T� � � �� �� �� � T� � �-�� U 	 LNCURRENT LNCOUNT THISFORM OBJPROGRESSBAR FLOODPERCENT THIS VISIBLE  �� U  �  ��  � T� � � �� � �� T� � � �� � �� T� � � �� � �� T� � � �� � ��* T� � � �� � � � � �	 � � ��' T� � � �� �	 � � �	 � � ��" T� � �
 �� �
 �� � �� T� � � �� � ��	 ��C��� U  NSTYLE THISFORM LBLFIRSTLABEL LEFT THIS	 SEPLENGTH LBLSECONDLABEL OBJPROGRESSBAR TOP HEIGHT WIDTH FLOODSHOWPCT AUTORELEASE2 	 ��C��� T�  � � ��  �� T�  � � ��  �� U  THISFORM LBLFIRSTLABEL CAPTION LBLSECONDLABEL currentprogress,     �� finishprogressR    �� Activate    �� Show    �� Init�    ��1 q �A A B�A � A 3 � 2q1A A � 3 A 3 q baab�t"b� 3 � 212                       �        �  �        �  �  !        �  %   #   �  6  <    )   ;                       
b���    I
  I
                        �G   %   j      �	  W   �          �  U  � ��  � � � �� � � � � � �	 �F %�C�� � C� toObjectb� O� C�	 tcBarsStrb� C� C� ���} �	 B�� �� �, T� �CC�
 tcBarsStatb� C� �  � � 6�� T� �C �   � �
 � �� s�� �C���C����W� T� �C� ,� ���� T� �C� � � T��� %�� ���7� ��C � � � �  � �
 � �� �� T� ���� T� ���� T� �C� ,� ���� +�� � ��� T� �C� � � � \�� ��C �  �  � �  � �
 � �� T� �� ��� T� �� ��� T� �C� ,� � ���% T� �C� � � C� >�� � 6�� � �& 1��� � DEACTIVATE POPUP AriaPopUp� t�� � <�� �) T�	 �CC�  � Cl�
 � � � F� � T6�� ��C�  � Cl �	 �
 � �� B�Cl�� U  TOOBJECT	 TCBARSSTR
 TCBARSSTAT LNOBJNO LNBARSNO LNCURBAR LNFROM LNTOCH LCBAR LCSETTO THIS GETOBJNUMBER	 ARIAPOPUP ADDBAR NAME ISBARCHECKED SETBARCHECKED�  ��  � � � � �� � T�  �C�  ��� T� �-�� %�C�  �=� ~��h � T�  �C�  �\�� T� �a�� � s�� ��� "��  �� T� �C� � �\�� GN��� ��� �� � F��  %�� � C �  � � � 	��� � G:��� ��� (�a�� � U	  TCBAR TNCURBAR
 TCBARSSTAT	 TCOBJNAME	 LLSETMARK	 ARIAPOPUP	 TCBARSTAT THIS ISBARCHECKED� ��  � � �� � � � � T� �C� � �  � ��� T� �C� ,� ���� %�� � �� � T� �C� � ���� T� �� �� T� � �� ������  � ��" T� � �� �����C� F� Q�� � � �� ������" T� � �� ������C� X��" T� � �� ������C� X�� �}�% T� �C� � C� � �  � �����0 T� � �� �����CC � �� � � � F��� �
 B�� �� U  TOOBJECT	 TCBARSSTR LNOBJNO	 LNLISTLEN LNBARSNO THIS AOBJECTS NAME  ��  � � �� � � T� �-�� T� �C� � �  ��� %�� � ��n �& T� �� TCC� �� � � �\�� �
 B�� �� U 	 TCOBJNAME TNBARNUMBER LLRETVAL LNOBJNO THIS AOBJECTSt  ��  � � � �� � T� �C� � �  ��� %�� � ��m �0 T� � �� ���CC� �� � � �� [�� � U 	 TCOBJNAME TNBARNUMBER TCSETTO LNOBJNO THIS AOBJECTS+  �  � ������� T�  � �C� X�� U  THIS AOBJECTS  u�  � U 	 ARIAPOPUP showshortcut,     �� addbary    �� getobjnumber�    �� isbarchecked�    �� setbarchecked_    �� Init    �� DestroyP    ��1 � �c� A ��Rqa�� � � a!��aQA A a� � ��� 3 1q � � q!� A RQ�AA 2 � 1�qQ� �!�!!� QA � 2 � � � aaA � 2 � q bA 2 a!2 � 1                       �     !   �  �  (   0   �  q	  ;   B   �	  }
  P   K   �
  {  \   R   �  �  f   U   �    j    )   I
                       $PROCEDURE adjustcontrols
IF THIS.InitialResize
	THIS.LoopThroughControls("INITIALIZE_AND_ADJUST")
	THIS.InitialResize = .F.
ELSE
	THIS.LoopThroughControls("ADJUST")
ENDIF
ENDPROC
PROCEDURE addtoarray
LPARAMETERS oControl
nLen = ALEN(THIS.aControlStats,1)
THIS.aControlStats[nLen,1] = oControl.Top / THIS.InitialFormHeight
THIS.aControlStats[nLen,2] = oControl.Left / THIS.InitialFormWidth
THIS.aControlStats[nLen,3] = oControl.Height / THIS.InitialFormHeight
THIS.aControlStats[nLen,4] = oControl.Width / THIS.InitialFormWidth
THIS.aControlStats[nLen,5] = IIF(TYPE("oControl.FontSize") = 'U', 0, oControl.FontSize)
DIMENSION THIS.aControlStats[nLen+1, 5]

ENDPROC
PROCEDURE setsize
LPARAMETERS oControl, nPos
oControl.Top = THISFORM.Height * THIS.aControlStats[nPos,1]
oControl.Left = THISFORM.Width * THIS.aControlStats[nPos,2]
oControl.Width = THISFORM.Width * THIS.aControlStats[nPos,4]
IF !oControl.Baseclass $ "Textbox Spinner"
	oControl.Height = THISFORM.Height * THIS.aControlStats[nPos,3]
ENDIF

*IF oControl.Baseclass = "Commandbutton"
*	IF TXTWIDTH(oControl.caption) > oControl.width 
*		oControl.FontSize = 8
*	ELSE
*		oControl.FontSize = 10
*	ENDIF
*ENDIF

ENDPROC
PROCEDURE loopthroughcontrols
LPARAMETERS cTask
* Valid parameters for cTask are 'Initialize_And_Adjust' and 'Adjust'
cTask = UPPER(cTask)

nOldDecimal = SET("DECIMAL")
SET DECIMAL TO 4

#define BASE_CLASS "Commandbutton Combobox Checkbox Listbox Form Grid Textbox Label Shape Editbox Olecontrol Pageframe Image Spinner Container"

nPos = 0
THISFORM.LockScreen = .T.
FOR m.i = 1 TO THISFORM.ControlCount
	oControl = THISFORM.Controls[m.i]
	IF oControl.Baseclass$BASE_CLASS
		nPos = nPos + 1
		DO CASE
			CASE cTask = 'INITIALIZE_AND_ADJUST'
				THIS.AddToArray(oControl)
				THIS.SetSize(oControl, nPos)
			CASE cTask = 'ADJUST'
				THIS.SetSize(oControl, nPos)
		ENDCASE
	ENDIF
	*A pageframe can contain only pages
	IF THISFORM.Controls[m.i].Baseclass$"Pageframe"
		*Loop through each page of the pageframe
		FOR m.j = 1 TO THISFORM.Controls[m.i].PageCount
			WITH THISFORM.Controls[m.i].pages[m.j]
				*loop through all the controls on the page
				FOR m.k = 1 TO .ControlCount
					IF .Controls[m.k].Baseclass$BASE_CLASS
						nPos = nPos + 1
						DO CASE
							CASE cTask = 'INITIALIZE_AND_ADJUST'
								THIS.AddToArray(.Controls[m.k])
								THIS.SetSize(.Controls[m.k], nPos)
							CASE cTask = 'ADJUST'
								THIS.SetSize(.Controls[m.k], nPos)
						ENDCASE
					ENDIF
				ENDFOR
			ENDWITH
		ENDFOR
	ENDIF			
ENDFOR

THISFORM.LockScreen = .F.
SET DECIMAL TO nOldDecimal
ENDPROC
PROCEDURE reset
THIS.InitialResize = .T.
DIMENSION THIS.aControlStats[1,5]
ENDPROC
PROCEDURE Init
THIS.InitialFormHeight = THISFORM.Height
THIS.InitialFormWidth = THISFORM.Width
ENDPROC
     Mhorizontalspace
verticalspace
portions
treeviewminimumwidth
listviewminimumwidth
resizewhilemoving
treeratio
minimumheight
minimumwidth
treeviewreference
listviewreference
controlrefrence
*resizewhilemoving_assign 
*minimumwidth_access 
*listviewreference_access 
*treeviewreference_access 
*controlrefrence_assign 
      FTop = 5
Left = 167
Height = 300
Width = 354
Name = "lstListview"
     ;PROCEDURE Init
DIMEN THIS.LACODES[1,2]
THIS.LACODES = ""
this.CodesField = IIF(EMPTY(this.CodesField) AND !EMPTY(THIS.CONTROLSOURCE),UPPER(CONTROLSOURCE),UPPER(this.CodesField))
IF TYPE('OARIAAPPLICATION')='O' AND !EMPTY(oAriaApplication.ActiveCompanyID)
  LOCAL lcTempAlias,lnCount,lnAlias
  lnAlias = SELECT()
  lcTempAlias = gfTempName()
  USE (oAriaApplication.Datadir+'codes') AGAIN ALIAS &lcTempAlias ORDER TAG CCODE_NO IN 0
  SELECT (lcTempAlias)
  lnCount = 0
  IF SEEK('N' + This.CodesField)
    SCAN REST WHILE cDefCode + cFld_Name = 'N' + This.CodesField
      IF CRLTFIELD = 'N'
        lnCount = lnCount + 1
        DIMENSION THIS.laCodes[lnCount,2]
        this.laCodes[lnCount,1] = cdiscrep
        this.laCodes[lnCount,2] = cCode_no
      ENDIF
    ENDSCAN
  ENDIF
  IF SEEK('D' + This.CodesField)
    THIS.VALUE = CCODE_NO
    THIS.DisplayValue = cdiscrep
  ELSE  
    THIS.VALUE = THIS.laCodes[1,2]
  ENDIF
  USE IN (lcTempAlias)
  SELECT (lnAlias)
ENDIF
THIS.ROWSOURCE = "THIS.LACODES"
THIS.BoundColumn = 2
THIS.REFRESH()
ENDPROC
     APROCEDURE currentprogress
LPARAMETERS lnCurrent

IF TYPE('lnCurrent') <> 'N'
  RETURN
ENDIF

IF lnCurrent > This.TotalProgress
  lnCurrent = IIF(This.AutoRelease , This.TotalProgress ,;
                  MOD(lnCurrent , This.TotalProgress))
ENDIF

ThisForm.objProgressBar.FloodPercent = INT((lnCurrent/This.TotalProgress)*100)

IF This.AutoRelease .AND. ThisForm.objProgressBar.FloodPercent = 100
  This.Visible = .F.
ENDIF

ENDPROC
PROCEDURE finishprogress
LOCAL lnCurrent , lnCount

lnCurrent = ThisForm.objProgressBar.FloodPercent
IF lnCurrent < 100
  FOR lnCount = lnCurrent TO 100
    ThisForm.objProgressBar.FloodPercent = lnCount
  ENDFOR
ENDIF

This.Visible = .F.

ENDPROC
PROCEDURE Activate
NODEFAULT

ENDPROC
PROCEDURE Show
LPARAMETERS nStyle

ThisForm.lblFirstLabel.Left   = This.SepLength
ThisForm.lblSecondLabel.Left  = This.SepLength
ThisForm.objProgressBar.Left  = This.SepLength

ThisForm.lblFirstLabel.Top    = This.SepLength
ThisForm.lblSecondLabel.Top   = ThisForm.lblFirstLabel.Top +;
                                ThisForm.lblFirstLabel.Height +;
                                This.SepLength

ThisForm.objProgressBar.Top   = This.Height -;
                                ThisForm.objProgressBar.Height -;
                                This.SepLength

ThisForm.objProgressBar.Width = This.Width - (2 * This.SepLength)

ThisForm.objProgressBar.FloodShowPct = This.AutoRelease

DoDefault()

ENDPROC
PROCEDURE Init
DoDefault()

ThisForm.lblFirstLabel.Caption  = ''
ThisForm.lblSecondLabel.Caption = ''

ENDPROC
     
 ��ࡱ�                >  ��	                               ����        ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������R o o t   E n t r y                                               ��������                               �6;�!�           O l e O b j e c t D a t a                                            ����                                        q        A c c e s s O b j S i t e D a t a                             &  ������������                                       V        C h a n g e d P r o p s                                         ������������                                       �             ����         ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������>  ��o^   �.  �      	  �  � R������ � K�Q   �V                             8                       04746E60CE4F11CDB23C0000C076FE                                            �   Caption 	   H           BevelOuter 	   I
         BevelInner 	   I
      
   FloodType 	   I
         FloodColor 	   I
     �	   AutoSize 	   I
       
   BackColor 	   I
     DB MS Sans Serif        �           �               �
   ForeColor 	   I
   	  �                                         �PROCEDURE getvalidentries
PARAMETERS lcVldEnt

LOCAL lcTmpName , lcDelemt , lcFile , laTmpArray , lnCount

*-- If the valid entries will be created from the file SYDFIELD
*-- we are going to create a temp. name to open the file
*-- SYDFIELD using this temp. name.
lcTmpName = IIF(TYPE('lcVldEnt') <> 'C' , gfTempName() , '')

*-- If the valid entries will be created from the file SYDFIELD
*-- and the file exist.
IF !EMPTY(lcTmpName) .AND. FILE(This.SourcePath + 'SYDFIELD.DBF')
  lcFile = This.SourcePath + 'SYDFIELD'
  USE &lcFile IN 0 AGAIN ALIAS (lcTmpName) ORDER CFLD_NAME
ENDIF    && End of IF !EMPTY(lcTmpName) .AND. FILE(This.SourcePath + 'SYDFIELD.DBF')

*-- Get the valid entries string.
lcVldEnt = IIF(TYPE('lcVldEnt') = 'C' , lcVldEnt ,;
               IIF(!EMPTY(lcTmpName) .AND. USED(lcTmpName) .AND.;
                   SEEK(PADR(UPPER(ALLTRIM(This.SourceField)) , 10);
                        , lcTmpName) , &lcTmpName..mVEntries , ''))

*-- Get the delimiters string.
lcDelemt = IIF(TYPE('This.RowDelimiter') <> 'C' .OR.;
               EMPTY(This.RowDelimiter) , '' ,;
               SUBSTR(ALLTRIM(This.RowDelimiter) , 1 , 1);
               + IIF(TYPE('This.ColumnDelimiter') <> 'C' .OR.;
                     EMPTY(This.ColumnDelimiter) , '' ,;
                     SUBSTR(ALLTRIM(This.ColumnDelimiter) , 1 , 1)))

*-- If the valid entries string is not empty
IF !EMPTY(lcVldEnt)
  *-- Create the RowSource array from the valid entries string
  DIMENSION laTmpArray[1,2]
  = gfSubStr(lcVldEnt , @laTmpArray , lcDelemt)

  DIMENSION This.aSourceArray[ALEN(laTmpArray,1),ALEN(laTmpArray,2)]
  = ACOPY(laTmpArray , This.aSourceArray)
  IF This.DisplayValidEntryCodes
    FOR lnCount = 1 TO ALEN(This.aSourceArray , 1)
      This.aSourceArray[lnCount , 1] = This.aSourceArray[lnCount , 2] +;
                                       '-' + This.aSourceArray[lnCount , 1]
      
    ENDFOR
  ENDIF
ELSE    && Else [If the valid entries string is empty]
  *-- Clear the RowSource array
  DIMENSION This.aSourceArray[1,2]
  This.aSourceArray = ''
ENDIF    && End of IF !EMPTY(lcVldEnt)

IF This.AddNA
  IF ALEN(This.aSourceArray,1) > 1 .OR. !EMPTY(This.aSourceArray)
    DIMENSION This.aSourceArray[ALEN(This.aSourceArray,1)+1,2]
    AINS(This.aSourceArray, 1)
  ENDIF
  This.aSourceArray[1,1] = "N/A"
  This.aSourceArray[1,2] = SPACE(2)
ENDIF


*-- If we have opened the file SYDFIELD
IF !EMPTY(lcTmpName) .AND. USED(lcTmpName)
  USE IN (lcTmpName)
ENDIF    && End of IF !EMPTY(lcTmpName) .AND. USED(lcTmpName)

ENDPROC
PROCEDURE Init
DoDefault()
	
*-- If there is no Row Source field use the ControlSource
This.SourceField  = IIF(TYPE('This.SourceField') <> 'C' .OR.;
                        EMPTY(This.SourceField) ,;
                        IIF(EMPTY(This.ControlSource) , '' ,;
                            SUBSTR(This.ControlSource ,;
                                   AT('.' , This.ControlSource) + 1));
                        , This.SourceField)

*-- If there is no Source file path use the System files Directory
This.SourcePath   = IIF(TYPE('This.SourcePath') <> 'C' .OR.;
                        EMPTY(This.SourcePath) ,;
                        IIF(TYPE('oAriaApplication') = 'O' ,;
                            oAriaApplication.SysPath , '') ,;
                        This.SourcePath)

*-- If there is no Row Source field or there is no Source file path
IF EMPTY(This.SourceField) .OR. EMPTY(This.SourcePath)
  DIMENSION This.aSourceArray[1,2]
  This.aSourceArray = ''
ELSE    && Else [If there is Row Source field and Source file path]
  This.GetValidEntries()
ENDIF    && End of IF EMPTY(This.SourceField) .OR. EMPTY(This.SourcePath)

ENDPROC
     ���                              d�   %   u      H  �   "          �  U  v ��  � � � �� � ����� ��  ���(�C� � ����o�* T� � ��  �����C �  � � � � ��' T� � ��  �����C �  � � � ��; T� � ��  �����CC� THIS.aFileFields[lnCount,2]�b��^ %�C� (� THIS.aFileFields[lnCount,3]�� �) C� +� THIS.aFileFields[lnCount,3]�� ��F� T� � ��  ������ E�� �k� T� � ��  ������ F�� � �� U  LNCOUNT THIS AFILEFIELDS COLUMNCOUNT COLUMNS HEADER1 CAPTION CONTROLSOURCE�  ��  � � �� �8 T� �CC� toReferenceb� O� � toReference� �  6��+ T� �� CC� �� � =� � � .� 6�� %�C� �
��� � &lcCommand
 � U  TOREFERENCE TCMETHOD	 LCCOMMAND� ��  � � � � �' %�C�t� � C� tcCationb� C��B � B� �+ T� �CC� tcStatusb� C� � � � T6�� �� �% T� �CC� tnIndexb� N� -� a6��O T� �CC� tnIndexb� N� � �) C� � ��CCC��� � �� � � �66��! %�� 
� � C� � ��	��� B� ��� %�� ��D� � � �� ��C� � ���� � T� � �� ������  �� T� � �� ������ �� T� � �� ������ �� T� � �� ������ �� � U  TCCATION TOOBJECT TCMETHOD TCSTATUS TNINDEX LLADD THIS USERSHORTCUT'  �  � ������� J�-�(�  � � U  THIS USERSHORTCUT�  ��  �9 %�C� tnIndexb� N� C� � ���  � �  � ��D � B� � ��C� � �  ��� %�C� � ������ �* � � �C� � �����C� � ���� � U  TNINDEX THIS USERSHORTCUT ��  � %�C� THISFORMSETb� O��� T� � �C�� T� � �C+��: %�C� � �
�$ C� oAriaApplication.oToolBarb� O	��� � T� � � �� � �� T� � � �� � �� T� � � �-�� T� � � �-�� ��C� � �	 �� ��C� � �
 �� �� ��C� � �� ��C� � �� � � U 	 NCOLINDEX THISFORMSET TOPFILE ENDFILE FORMHASTOOLBAR OARIAAPPLICATION OTOOLBAR EDITMODE ADDMODE BUTTONREFRESH
 NAVREFRESH THISFROMSET
 REFRESHALL REFRESHa  %��  � ���> � T�  � �C�  � ��� T�  � �C�  � ��� �Z � T�  � �� FFTF�� � U  THIS RECORDSOURCETYPE FILTER RECORDSOURCE ORDER DEFAULTSHORTCUT�' ��  � � � � � � � � � T� �C�� T� �C�	 �
 ��\�� T� �C�	 �
 ��\��1 T� �CCC�� ��
�	 C�	 � �	� � T� � F6��6 T� �C� � T� C�	 �
 �R� T	� � T� � F6��% T� �CCC�� ��
� � T� � F6��9 T� �C� � T� C�	 �
 ��\� T	� � T� � F6�� J��  �(� � � %�CC���	 � �
���� T� �� ,\-�� T� �� T�� �� ���(�C�	 � ������! T� �� � ,C � ��	 � �� T� �� C � ��	 � �� �� � �� � T� �C� ARIASHORTCUT�N��R T�  �C �	 �# \<Find,Fi\<lter,\<Quik Find,\<Order� � � � � � � � �� %��  �� �  ����� %�CC���	 � ����� ��C�	 � �� � � H����� ��  �����
 F�� �� �� � � 5� � %�C�	 � �
��8�W T� �CCC�	 � �=� ,�	 � �� �& C�	 � �CC�	 � �=� ,�	 � �\� �	 � 6�� �O� T� �� ''�� � ��C�	 � �� ��� T� �C� ARIASEARCHFORM�N�� T� � �� �� ��C� � � � �� ��C� � �� ��  �����
 F�� �� �� � 5� � � ��C�	 � �� ��� ��C�	 � �� ���/ T� �C� ARIASEARCHFORM� laFilter�	 � �N�� ��C� � � � � �� ��C� � �� �	 � �C�� ������� ��C�� �	 � ��� ��  ���k�
 F�� �� �� � %�C�	 � �
���W T� �CCC�	 � �=� ,�	 � �� �& C�	 � �CC�	 � �=� ,�	 � �\� �	 � 6�� �1� T� �� ''�� �) T� �C� ARIASEEKC �	 a�	 �  � �N�� ��C� � �� ��  �����
 F�� �� T� �C� ARIASEEKC �	 -�N�� ��C� � �� ��  �����/ ��CC�  ���	 � C�  ���	 � �	 � �� � U  LNCHOICE LCORDER
 LCQUIKFIND LCFIND LCFILTER LCUSERDEFBARS LCUSERDEFACC LNCOUNT LCALIAS THIS DEFAULTSHORTCUT KEY USERSHORTCUT	 OSHORTCUT SHOWSHORTCUT AFILEFIELDS BUILDFILEFIELDS OFILTER LCKEY LAFILEFIELDS ARIASEARCHCONTAINER1 INITIALIZEFIELDS SHOW LAFILTER AFILTER
 FILEFILTER ORDER	 RUNMETHOD buildfilefields,     ��	 runmethod�    �� addshortcut�    �� clearshortcut�    �� removeshortcut    �� AfterRowColChange�    �� Init�    ��
 RightClick:    ��1 q ���q���� �A A 2 � q ��� A 3 qqA A �q Q�A � � �A ����A 2 a� 2 q �A A !��A 2 q �� � �aa� � � A A 2 AQQ� AA 3 r� ��aQ��� ��A A q �!��� A A � � � q 1q� � A A�A� � q � AA��� �1� q 1q� � A �� � �� �A 2                       !        A  !        C  �     *   �  9  .   -   ^  i  2   6   �  �	  <   G   
  �
  N   N   �
    W    )                          )PROCEDURE adjustcontrols
PRIVATE oTmp

FOR lnI = 1 TO 6
  lcObjNum       = STR(lnI,1)
  oTmp           = EVAL("This.lblAdd" + lcObjNum)
  oTmp.Caption   = EVAL(This.PrivateIntFile+".cPart" + lcObjNum + "Lab") 
  oTmp.Caption   = ALLTRIM(oTmp.Caption) + IIF(EMPTY(oTmp.Caption), "", ":")
  oTmp           = EVAL("This.txtAdd" + lcObjNum)
  oTmp.MaxLength = EVAL(This.PrivateIntFile+".nPart" + lcObjNum + "Len")
ENDFOR

oTmp = .NULL.
ENDPROC
PROCEDURE Refresh
DoDefault()

IF This.cboCountry.ListIndex = 0
  = SEEK(oAriaApplication.DefaultCountry, This.PrivateIntFile)
  llCheckMode = ALLTRIM(UPPER(ThisFormSet.Class)) = "ARIAFORMSET" AND ;
                !EMPTY(ThisFormSet.FormHasToolBar)
  IF This.ReplaceDefaultCountry AND llCheckMode AND ThisFormSet.ActiveMode $ "EA" 
    lcReplThis = UPPER(ALLTRIM(This.cboCountry.ControlSource))
    REPLACE &lcReplThis WITH oAriaApplication.DefaultCountry
  ENDIF
ENDIF

This.AdjustControls()

ENDPROC
PROCEDURE Init
DoDefault()
This.PrivateIntFile = gfTempName()
lcSysPath           = oAriaApplication.SysPath
USE (lcSysPath + "sycInt") IN 0 AGAIN ALIAS (This.PrivateIntFile) ORDER cContCode
This.cboCountry.RowSource = This.PrivateIntFile

This.Bars = SPACE(0)
SELECT (This.PrivateIntFile)
SCAN
  This.Bars  = This.Bars + IIF(EMPTY(This.Bars), "~", ",~") + ;
               ALLTRIM(PROPER(cCont_Desc))
ENDSCAN
This.Ariashortcut1.GetObjNumber(This, This.Bars)
ENDPROC
PROCEDURE RightClick
PRIVATE llCheckMode, lnBar, lcStat, lcCurCont, lnPos, lnBarNo

DoDefault()

llCheckMode = ALLTRIM(UPPER(ThisFormSet.Class)) = "ARIAFORMSET" AND ;
              !EMPTY(ThisFormSet.FormHasToolBar)

lcStat = SPACE(0)
FOR lnBarNo = 1 TO OCCURS(",", This.Bars) + 1
  This.Ariashortcut1.SetBarChecked(This.Name, lnBarNo, "F")
  IF llCheckMode
    lcStat = lcStat + IIF(ThisFormSet.ActiveMode $ "EA", "T", "F")
  ELSE
    lcStat = lcStat + "T"
  ENDIF
ENDFOR

lcCurCont = PROPER(ALLTRIM(EVAL(This.PrivateIntFile + ".cCont_Desc")))
lnPos     = ATC("~"+lcCurCont, This.Bars) - 1
lnBarNo   = OCCURS(",", LEFT(This.Bars, lnPos)) + 1
llNoThing = This.Ariashortcut1.SetBarChecked(This.Name, lnBarNo, "T")
lnBar     = This.Ariashortcut1.ShowShortCut(This, This.Bars, lcStat)
IF lnBar # 0
  lnPos     = IIF(lnBar = 1, 1, ATC(",", This.Bars, lnBar-1)+1 )
  lcSubBars = SUBSTR(This.Bars, lnPos)
  lnPos     = ATC(",", lcSubBars, 1)
  lcSelCont = LEFT(lcSubBars, IIF(lnPos = 0, LEN(lcSubBars), lnPos))
  lcSelCont = UPPER(ALLTRIM(STRTRAN(STRTRAN(lcSelCont, "~"),",")))

  SELECT (This.PrivateIntFile)
  LOCATE FOR UPPER(ALLTRIM(cCont_Desc)) = lcSelCont
  This.cboCountry.Value = EVAL(This.PrivateIntFile + ".cCont_Code")
  This.txtAdd6.Value = PROPER(lcSelCont)
  This.AdjustControls()
ENDIF  

ENDPROC
PROCEDURE Destroy
USE IN (This.PrivateIntFile)
ENDPROC
     UPROCEDURE resizewhilemoving_assign
LPARAMETERS vNewVal
*To do: Modify this routine for the Assign method
This.ResizeWhileMoving = m.vNewVal

This.szrSizerline.lAtEnd = !This.ResizeWhileMoving

ENDPROC
PROCEDURE minimumwidth_access
*To do: Modify this routine for the Access method
RETURN This.TreeviewMinimumWidth + This.ListviewMinimumWidth +;
       (This.HorizontalSpace * 3)

ENDPROC
PROCEDURE listviewreference_access
*To do: Modify this routine for the Access method
RETURN This.lstListview

ENDPROC
PROCEDURE treeviewreference_access
*To do: Modify this routine for the Access method
RETURN This.trvTreeview

ENDPROC
PROCEDURE controlrefrence_assign
LPARAMETERS vNewVal
*To do: Modify this routine for the Assign method
This.trvTreeview.ControlRefrence = m.vNewVal
This.lstListview.ControlRefrence = m.vNewVal

ENDPROC
PROCEDURE Init
DoDefault()

This.TreeRatio   = 0
This.lstListview.SetFocus()
This.trvTreeview.SetFocus()

LOCAL lnPortions        , lnTreePortion     ,;
      lnTreeviewLeft    , lnTreeviewTop     ,;
      lnTreeviewWidth   , lnTreeviewHeight  ,;
      lnSizerLeft       , lnSizerTop        ,;
      lnSizerWidth      , lnSizerHeight     ,;
      lnListviewLeft    , lnListviewTop     ,;
      lnListviewWidth   , lnListviewHeight

lnTreePortion = VAL(ALLTRIM(SUBSTR(This.Portions ,;
                                   1 , AT('/' , This.Portions) - 1)))

lnPortions    = lnTreePortion +;
                VAL(ALLTRIM(SUBSTR(This.Portions ,;
                                   AT('/' , This.Portions) + 1)))

This.TreeRatio   = lnTreePortion/lnPortions
This.BorderWidth = 0

lnTreeviewLeft   = This.HorizontalSpace
lnTreeviewTop    = This.VerticalSpace
lnTreeviewWidth  = (This.Width - (This.HorizontalSpace * 3)) *;
                   (lnTreePortion/lnPortions)
lnTreeviewHeight = This.Height - (This.VerticalSpace * 2)

lnSizerLeft      = lnTreeviewLeft + lnTreeviewWidth + 1
lnSizerTop       = 0
lnSizerWidth     = This.HorizontalSpace
lnSizerHeight    = This.Height

lnListviewLeft   = lnSizerLeft + lnSizerWidth + 1
lnListviewTop    = This.VerticalSpace
lnListviewWidth  = This.Width - (This.HorizontalSpace * 3) -;
                   lnTreeviewWidth
lnListviewHeight = This.Height - (This.VerticalSpace * 2)

This.trvTreeview.Move(lnTreeviewLeft , lnTreeviewTop , lnTreeviewWidth ,;
                      lnTreeviewHeight)

This.szrSizerline.Move(lnSizerLeft , lnSizerTop , lnSizerWidth ,;
                       lnSizerHeight)

This.lstListview.Move(lnListviewLeft , lnListviewTop , lnListviewWidth ,;
                      lnListviewHeight)

This.szrSizerline.aLeftTop[1,1] = 'This.Parent.trvTreeview'
This.szrSizerline.aLeftTop[1,2] = This.TreeviewMinimumWidth
This.szrSizerline.aLeftTop[1,3] = 0

This.szrSizerline.aRightDown[1,1] = 'This.Parent.lstListview'
This.szrSizerline.aRightDown[1,2] = This.ListviewMinimumWidth
This.szrSizerline.aRightDown[1,3] = 0

ENDPROC
PROCEDURE Resize
DoDefault()

LOCAL llTreeviewMinimum , llListviewMinimum ,;
      lnTreeviewLeft    , lnTreeviewTop     ,;
      lnTreeviewWidth   , lnTreeviewHeight  ,;
      lnSizerLeft       , lnSizerTop        ,;
      lnSizerWidth      , lnSizerHeight     ,;
      lnListviewLeft    , lnListviewTop     ,;
      lnListviewWidth   , lnListviewHeight
      

STORE .F. TO llTreeviewMinimum , llListviewMinimum

lnTreeviewWidth = (This.Width - (This.HorizontalSpace * 3)) *;
                  This.TreeRatio

IF lnTreeviewWidth < This.TreeviewMinimumWidth
  lnTreeviewWidth   = This.TreeviewMinimumWidth
  llTreeviewMinimum = .T.
ENDIF

lnListviewWidth = This.Width - (This.HorizontalSpace * 3) - lnTreeviewWidth
IF lnListviewWidth < This.ListviewMinimumWidth
  lnListviewWidth   = This.ListviewMinimumWidth
  llListviewMinimum = .T.
  IF !llTreeviewMinimum
    lnTreeviewWidth = This.Width - (This.HorizontalSpace * 3) -;
                      lnListviewWidth
    
    IF lnTreeviewWidth < This.TreeviewMinimumWidth
      lnTreeviewWidth   = This.TreeviewMinimumWidth
      llTreeviewMinimum = .T.
    ENDIF
  ENDIF
ENDIF

IF llTreeviewMinimum .AND. llListviewMinimum
  This.Width = lnTreeviewWidth + lnListviewWidth + (This.HorizontalSpace * 3)
ENDIF

IF This.Height < This.MinimumHeight
  This.Height = This.MinimumHeight
ENDIF

lnTreeviewLeft   = This.trvTreeview.Left
lnTreeviewTop    = This.trvTreeview.Top
lnTreeviewHeight = This.Height - (This.VerticalSpace * 2)

lnSizerLeft      = lnTreeviewLeft + lnTreeviewWidth + 1
lnSizerTop       = 0
lnSizerWidth     = This.szrSizerline.Width
lnSizerHeight    = This.Height

lnListviewLeft   = lnSizerLeft + lnSizerWidth + 1
lnListviewTop    = This.lstListview.Top
lnListviewHeight = This.Height - (This.VerticalSpace * 2)

This.trvTreeview.Move(lnTreeviewLeft , lnTreeviewTop , lnTreeviewWidth ,;
                      lnTreeviewHeight)

This.szrSizerline.Move(lnSizerLeft , lnSizerTop , lnSizerWidth ,;
                       lnSizerHeight)

This.lstListview.Move(lnListviewLeft , lnListviewTop , lnListviewWidth ,;
                      lnListviewHeight)

oFocusObj = IIF(TYPE('ThisForm.ActiveControl') = 'O' .AND.;
                !ISNULL(ThisForm.ActiveControl) ,;
                ThisForm.ActiveControl , .NULL.)

This.trvTreeview.SetFocus()
This.lstListview.SetFocus()

IF !ISNULL(oFocusObj)
  oFocusObj.SetFocus()
ENDIF

ENDPROC
     
 ��ࡱ�                >  ��	                               ����        ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������R o o t   E n t r y                                               ��������                                `_��           O l e O b j e c t D a t a                                            ����                                        h       A c c e s s O b j S i t e D a t a                             &  ������������                                       8        C h a n g e d P r o p s                                         ������������                                       :          ��������            �����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������u�)@��� `��4䓲  X    �WG    _ e x t e n t x ^    
�WG8                              8                                 �
   LineStyle 	   I
      
   LabelEdit 	   I
                _ e x t e n t y      ?Ƈ(   i n d e n t a t i o n w�D   	 �V&�$   l i n e s t y l e        �R��(   o l e d r a g m o d e        �sA@   t r e e f o n t R������ � K�Q�  �DB Tahoma     u��8   f o n t R������ � K�Q�  �DB Tahoma    	 <�՞����l a b e l e d i t                                    ��ࡱ�                >  ��	                               ����        ������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������   ����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������R o o t   E n t r y                                               ��������                               @g��   @       O l e O b j e c t D a t a                                            ����                                        �       A c c e s s O b j S i t e D a t a                             &  ������������                                       8        C h a n g e d P r o p s                                         ������������                                                 ��������               �������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������t��La��� `��4䓲  �    �WG    _ e x t e n t x �$    
�WG8                              8                                 �
   LabelEdit 	   I
                                           _ e x t e n t y     u��8   f o n t R������ � K�Q�  �DB Tahoma     ,�f�(   f l a t s c r o l l b a r ��  � �$   h o t t r a c k i n g �� 	 <�՞$   l a b e l e d i t        &e��(   f u l l r o w s e l e c t ��  �$�(   s c a l e h e i g h t  ��E   	 ?�D�    s c a l e m o d e    
  ������s c a l e w i d t h  �E                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ,   %   �C      YL  �  AG  �\}2~   �} ��  � � 1� do gfErrorTrap� u,� Project Manager� T�C� �a�� 7� � � � � � � �# 7�	 �
 � � � � � � � 7� � � � � � �# J�-�(�	 �
 � � � � � � J��   �(� � � � � T� �� �� T� �CC��]f�� T� �C� � \PRGS�  ��� T� �C� :� ��� T� �C� \� �� �� T� �C� � � \�� G(�� �� G~(� T� �� �	 \CLASSES\��  T� �C�� � � *.*� D��� �� ���(�� ��5�5 %�� DC � �� � CC � �� � .� ..�
	��� ��C� C � �� � �� �1� ��C � C � �� �  �� � �� <�! � � � � � � �C T� �C� AriaApplicationCC� lcAria4Classb� C� C�  �� -6�N��$ %�C� oAriaApplicationb� O��T� %�C� �  �
��*�9 %�C� �  � MAIN.VCX0� C� �  � UTILITY.VCX0	��&� �� �" �
 REMOBJ.FXP�� � � ��C� �# �� T� �$ ���� T� ���� � G~(� R� MAIN� V� <�� � U&  LCARIA4CLASS VISIBLE
 GCBASEWIND
 GNPROGCOPY
 GLINITMENU
 GNMAXUSERS
 GCPRMTMDLS	 GCACT_KEY	 LACTRSTAT
 GLTOOLACTV GLMSGREM
 GLUSER_TSK	 GLAUTOADD
 GLLOG_REQU	 GLSYS_LOG
 GCACT_COMP
 GCACT_APPL OARIAAPPLICATION
 GCPLATFORM	 GCLICENCE
 GCCOMPNAME
 LCPROGNAME
 GLERRORHAN LCCURRENTPROCEDURE LNPATHSTART LNLENOFPATH	 LCCURPATH
 LCCLASPATH	 LNCLASSES	 LACLASSES LNCLASS	 LPSETLIBS	 LPSETFILE LCCLASS APPLICATIONHOME DO OTOOLBAR MAIN�  ��  � 5� � � �* T�  �CC�  �R� \� �  � �  � \6�� T� �C�� �  � *.VCX��� �� ���(�� ��� � ��C �  C � �� � �� �� U  LCPATHTOSEARCH	 LNCLASSES LNCLASS	 LACLASSES	 LPSETFILE ��  � � 5� � � %�CC� �Rf� VCX��� � T� ��  � �� T� �C� X��8 %�CC� f� MAIN.VCX� GLOBALS.VCX� UTILITY.VCX���� � T� �� IN ARIA.EXE�� �" %�� \� C� CLASSLIBv
��� �1 SET CLASSLIB TO (lcClass) &lcCommand ADDITIVE
 � � U  LCPATH LCCLASSFILENAME LCCLASS	 LCCOMMAND  B�� XCC��]�\�� U  � 4�  � � � T� ��  �� T� ���� T� ���� T� ����* T� �CC� lcSeptab� C� � � � ,6�� %�CC� �>���x� T� �C� ��\�� T� �C� �=��D T� �CC� �  �� �& C� �  �CC�  �R� � �� � 6� � 6��D T� �CC� �  �� �& C� �  �CC�  �R� � �� � 6� � 6�� T� �� � �� �� T� �� � ��� T�  �C�  � � ��� ���D T� �CC� �  �� �& C� �  �CC�  �R� � �� � 6� � 6�� � H����� �C�
 lnAryOrPosb� U�� � T� ����  �C�
 lnAryOrPosb� C,L���� %�� ���D� � �� ��� �� ��� %�C�� ��� ��|� � �� ��C�� ���� ��� � �� �� � � T� ��  �� � �� ���(�� ���� %�C�
 lnAryOrPosb� N���� T� �� �� � H����� �� ���A�2 T� �C�  �C� �  � C� �  �� C�  >6\�� �� � ����" T� �C�  C� �  � ��\��2 T� �CC� �R� � C� �C� >�\� � 6�� �� �����A T� �C�  C� �  � ��C� �  � C� �  � ��\�� � %�C�
 lnAryOrPosb� N��#�	 B�� �� � %�� ���o�7 T� �C� �� G���C� �� 8���� �� ��� T� �� ��� �� � �� U	  LCSTRING
 LNARYORPOS LCSEPTA LCSUBSTR LNARYDIM	 LNARYROWS	 LNARYCOLS LCCOLSEP LNARELEM� P T�  �CCCC^��\g�� CCC^��\g� � � 12� CCCCCC^��\g�GZ�6��S T� �CC�  g�
� � 0� �  6�  CC^�\CCCC^��\g��	 �  pm� �  am6��
 B�� �� U 
 LCCURRHOUR
 LCCURRTIME#  4�  � � ��C �   � � � �� U 
 LCPOP_NAME	 LNBAR_POS OARIAAPPLICATION MENUBAR  4�  � ��C �  � � �� U 	 LCCOMP_ID OARIAAPPLICATION CHANGECOMPANY
  $�  � U  ,  ,� %��9�  ���% � ��C� � �� � U 	 FORMCOUNT OARIAAPPLICATION SYSEXIT
 4�  � 5� L*� T� �C�	 REPROCESSv�� T� �C� datasv�� G�(���� GM(����* T�  �CC�
 llGetCountb� U� -� �  6�� � ���� T� �C� SYUUSER��� T� �C�� T� ��  �� T� �� �� F� � %�� 
���) Q�  �� �	 � SYUUSER��� cuser_id� � %�C� SYUUSER���6� T� �C� SYUUSERO�� �� o�� �	 � SYUSTATC��C� �
 � � � � � � � 	� � � � �   6CC� �
 � �#����� C� �� �
 � � INI� OLDVARS� C� �
  � � 	��� � %�� � C� syuStatcN��"� #� �� � �� GM(���� ��C� syuStatcS�� � GM(�� �� %��  
��W� ��C� syusrlst� � �� � %�C� �
��u�
 F�� �� �! %�� � � C� SYUUSER�	���� %�� C� SYUUSERN���� #� �� �� � � %�� 
���� Q� � � G�(�� �� %��  ��� B�� �� � U 
 LLGETCOUNT LCOLDREP LNDATASESSION
 LAUSERLIST
 LLUSERUSED
 LCCURRFILE LNUSRREC SYUSTATC OARIAAPPLICATION SYSPATH CUSER_ID USER_ID CSTATION STATION
 LFGETUSRNM COBJ_TYP	 COBJ_NAME GFCHECKUSER USERSTATICRECORD DOFORMRETVAL SYUUSER�  4�  � �+ T� �CC�	 lcStationb� C� �  � � 6�� T� �a��0 %�C� INI� OLDVARS�  � � SYUSTATC���� � %�C� SYUSTATCS��� � Z� � T� �-�� � �� � T� �-�� �# B�� � �  � � � � � �� U  LCUSERID	 LCSTATION	 LLRETFLAG SYUSTATC OARIAAPPLICATION USER_ID STATIONl  T�  �CC�	 P_STATION5��� %�C�  �
��? � T�  �CC���  ]��� �[ � T�  �CC�]�\�� �
 B��  �� U 	 LCSTATION>  4�  � F� � %�C�  ���& �	 B�� �� �7 �	 B��  �� � U 	 LCUSER_ID SYUUSER	 CUSR_NAME�  %��9�  ���] � ��C� � �� ��Ca� � ��/ ��C� � C� � � SY� � S� � A6� � �� �� �M ��C�> You have to close all programs before login in with new use id��x�� � U 	 FORMCOUNT OARIAAPPLICATION LOGIN LOGUSER SETMENU ACTIVEMODULEID  ��C�]�� U  � 3 ��  � � � � � � � � �	 �
 � � �� � � T� �CW�� %�C� �
��d �
 F�� �� � %�C�  ���� �- T�  �C� GetC� Table� BrowseFields� �� � 5� � T� ����* � BROWSE(� ��  � � � � � �
 F�� ��	 B�� �� U  TCBROWSEFIELDS TCBROWSETITLE TCALIAS TCKEY TCFOR	 TCOPTIONS TLSELECT TOSELECTOBJ TCSELECTMETHOD TCUSERSHORTCUT
 TCTEMPFILE
 TCSELFIELD LLRETURNVALUE LCALIAS GFDATABASEPROP OBROWSE BROWSE� ��  � � � � � �� � � � �	 �
 � � T� �C� f�� T� �C� f�� T� �C� f�� T�  �CC�  ���� %�� � FIELD��,� %�C� .� �� ��(� T� �C� �C� .� ��\�� T� �C� C� .� ��\�� T� �C�
 Sourcename� ��� T� �C� � .DBF�  ��� T� �� � .� �� � � H�=��� �� � DATABASE���� %�C� ���r� G(�� �� ���	 B�� �� �! �� � FIELD,TABLE,VIEW���� %�� � FIELD���� T�	 �C� DATABASE� ��� ��� T�	 �C� DATABASE� ��� � %�C�	 �
���� T�
 ��  �� %�C� \�	 �� ��}� T�
 �C�	 �C� \�	 �\�� T�	 �C�	 �
 �  ��� T�	 �C�	 � .DBC�  ��� � %�C�	 �
���� ����
 �	 �� � G(��	 �� ��� B�-�� � � H���w� ��  �	 Dbgetprop��� T� �C� � � ��� ��  �	 Dbsetprop��8� T� �C� � � � ��� ��  � Get��L� ��  � Set��`� ��  � Remove��w� �	 B�� �� U  TCDATABASEFUNCTION TCNAME TCTYPE
 TCPROPERTY TCPROPERTYVALUE LNCOUNT LCALIAS LCFIELDNAME LCTABLENAME
 LCDATABASE LCPATH LCRETRUNVALUE� 4�  � � T� ��  �� T� �� ,�� T� ��  �� T� ����r +�C�a (&lcArray[lnFltStart,1]='.OR.' OR EMPTY(&lcArray[lnFltStart,1])) AND !lnFltStart=ALEN(&lcArray,1)���� � T� �� ��� � lnFltEnd=ALEN(&lcArray,1)
\ +�C�K (&lcArray[lnFltEnd,1]='.OR.' OR EMPTY(&lcArray[lnFltEnd,1])) AND lnFltEnd>1���R� T� �� ��� � T� �� ��& IF lnFltStart>ALEN(&lcArray,1)���	 B��  �� �$ IF lnFltEnd=ALEN(&lcArray,1)�U�? lcWhichElm=lcArray+'['+ALLTRIM(STR(ALEN(&lcArray,1)))+',1]'
 %�C� b� C���	 B��  �� �. IF &lcArray[ALEN(&lcArray,1),1]='.OR.'�Q�	 B��  �� � � T�	 �� �� +��	 � ����d IF  &lcArray[lnCount,3]='N' AND EMPTY(VAL(&lcArray[lnCount,6]))  AND &lcArray[lnCount,7]='V'��� T�	 ��	 ��� .� �� IF !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount,6],lcElmSep,'') ,IIF(&lcArray[lnCount,3]='D','/',''),''))) OR &lcArray[lnCount,1]='.OR.'���& IF !EMPTY(&lcArray[lnCount,1])���& IF &lcArray[lnCount,1]<>'.OR.'��� T� �� C �	  �   � �
 �� ���A T� �CC� �	R� �  .AND. � � C� �C� >�	\� � 6�� %�� � ��m� T� �� �  ) �� T� �� �� �� +�C�� lnCount<lnFltEnd-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,'') ,IIF(&lcArray[lnCount+1,3]='D','/  /',''),''))) OR &lcArray[lnCount+1,1]='.OR.')���<� T�	 ��	 ��� �c IF !EMPTY(ALLTRIM(STRTRAN(&lcArray[lnCount+1,6],lcElmSep,''))) AND !EMPTY(ALLTRIM(lcQuery))���* T� �C� ��  � �  OR � � ( �� T� ���� � � � � T�	 ��	 ��� �A T� �CC� �	R� �  .AND. � � C� �C� >�	\� � 6�� %�� � ��e� T� �� �  ) �� �1 T� �C� � �  .AND. � � �  AND � ���- T� �C� � �  OR � � �  OR � ���	 B�� �� U  LCARRAY LLFILTER LCQUERY LCELMSEP
 LCLINEFEED
 LNFLTSTART LNFLTEND LNOR
 LCWHICHELM LNCOUNT
 LFGETQCOND�
 4�  � � � T� ��  �� H�) ��
�- CASE &lcArray[lnCount,5] = 'Contains'�k�lcFiltExp=IIF(!&lcArray[lnCount,4],'','!(')+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+ ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+ IIF(!&lcArray[lnCount,4],'',' ) ')+lcElmSep+' .AND. '+lcElmSep                       
Q CASE &lcArray[lnCount,5] = 'Like' OR &lcArray[lnCount,5] = 'Exactly Like'��]lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]) +IIF(&lcArray[lnCount,3]='D',")",'')+' '+IIF(!&lcArray[lnCount,4], IIF(&lcArray[lnCount,5] = 'Like','=','=='),'<>')+' '+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep                
l CASE INLIST(&lcArray[lnCount,5],'Greater Than','Less Than','Greater Or Equal', 'Less Or Equal')     �O�Y lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),!&lcArray[lnCount,4])              
qlcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+ IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]) +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+IIF(!&lcArray[lnCount,4],'',' ) ')+ lcElmSep+' .AND. '+lcElmSep                              
, CASE &lcArray[lnCount,5] = 'Between'�� %�� ����5lcFiltExp=IIF(!&lcArray[lnCount,4],'BETWEEN(','!BETWEEN(')+ IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]) +IIF(&lcArray[lnCount,3]='D',")",'')+','+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+ ')'+lcElmSep+' .AND. '+lcElmSep
 ��PlcFiltExp= IIF(!&lcArray[lnCount,4],'','!(')+ IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]) +IIF(&lcArray[lnCount,3]='D',")",'')+' BETWEEN '+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+ IIF(!&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
 �, CASE &lcArray[lnCount,5] = 'In List'��
� %�� ���	�3lcFiltExp=IIF(!&lcArray[lnCount,4],'INLIST(','!INLIST(')+ IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]) +IIF(&lcArray[lnCount,3]='D',")",'')+','+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+ ')'+lcElmSep+' .AND. '+lcElmSep
 ��
�OlcFiltExp= IIF(!&lcArray[lnCount,4],'','!(')+ IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]) +IIF(&lcArray[lnCount,3]='D',")",'')+' IN('+ lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3], &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+')'+ IIF(!&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep    
 � �	 B�� �� U  LNCOUNT LCARRAY LLFILTER	 LCFILTEXP�  4�  � � H� �� � ��  � Greater Than��? �
 B�� >�� ��  �	 Less Than��c �
 B�� <��! ��  � Greater Or Equal��� � B�� >=�� ��  � Less Or Equal��� � B�� <=�� � U 
 LCOPERATOR LLISNOT] 4�  � � � � � T� ��  �� H�1 ��� �� � V���� H�P ��� �� � CM���# %�C� � Between� In List���� �4 T� �C� 
� � � Between	� �  AND � � ,6��+ T� �� "CC�  �� � "� � "�� "�� � � B�� "�  � "�� � �� � N����4 T� �C� � Between� � 
	� �  AND � � ,6�� T� �C�  � � ��� %�C� ���|� T� �� 0�� � �� � D���# %�C� � Between� In List���G�N T� �C� 
� � � Between	� �  AND ALLTRIM(DTOS(� � ,ALLTRIM(DTOS(6��D T� �� ALLTRIM(DTOS({  CC�  �� �   }))� � {  ��   }))�� �{�, T� �� ALLTRIM(DTOS({  C�  ��   }))�� � �� � L���� B��  � �  �� � �� � F���� T� �CC�  �� � ,��� �. %�C� � Between� In List�� CC�  ��	��M�4 T� �C� 
� � � Between	� �  AND � � ,6�� T� �� � � �� �	 B�� �� U 
 MRIGHTHEAD	 CLEFTTYPE	 COPERATOR LCELMSEP
 CRIGHTTYPE LCRETVAL LCSEPER LLFILTER  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDTOP CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDEND CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDNEXT CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDPREV CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDADD CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDPRINT CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDEDIT CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR	 CMDDELETE CLICK  ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDFIND CLICK  U    U    U    ��  � � � � U  OARIAAPPLICATION OTOOLBAR CMDADD CLICKB  %��  � � ��# � ��  � � � � �; � ��  � � � � � U  OARIAAPPLICATION OTOOLBAR EDITMODE CMDEDIT CLICK CMDEXIT  T�  � � ��  � � 
�� U  OARIAAPPLICATION OTOOLBAR VISIBLE  � syabout� U  SYABOUT( ��  � �� � � � � � ������� J��  �(� � � %�C�  �
��� ��C �  � � |� �� �� ���(�C�� ����� � ������� %�CC � �� �
��� ��CC � �� � � ~� ��G T� �� CC� �� �  � � ;6CC��� �� [CC��� �� ] �� � �� �	 B�� �� U 	 LCTAGUSER LCNAMES LATO LAVAL LNCOUNT GFSUBSTR�  ��  � 5� � � T� �CW�� F� � T� ��  �� %�C�  � MTMSATCH���� � ~$+�� �  ��| � T� �� � C� C�
 �� � �
 F�� ��	 B�� �� U  LCMSGID LNALIAS LCATTACH MTMSATCH CMSGID
 CATTCHFILE  � syUpDate� U  SYUPDATEE  ��  � � �� � T� �C� AddUserInfo�N�� B�C �   � � � �� U 
 LCFILENAME OFORM OADDINFO DOc ��  � � �/ T� �CC� nErrorb� N�
 CC�  Z�� CCC,Z�6��) T� �CC� cMethodb� C� � � �  6��+ T� �CC� nLineb� N�
 CC� Z�� �  6�� T� �C� C�
 ��� ��C� An error has occuerd...� � � Error Number : � � � Error Message: CE� � Method 		: � � � Line Number	: � � � Line Code	: C�E� �x�� � � 8� U  NERROR CMETHOD NLINE LCERROR LCMETHOD LCLINE	 LCNEWLINE� 4�  � �# 5� � � � � � � �	 � T� �a��E %�C�
 lcProgNameb� C� C�  �� C� lcEventb� C� C� ���� � B� � T� �C� W�� T�	 �-�� %�C� SYCTRIGG�
��� � F�  � Q��
 � � SYCTRIGG��
 G((� 1� � F� � %�CC�  �
�C� �
������% ~$+�� � C�  �
�C� �
����� T� �� �� T� ��  ��
 F�� �� %�C� � �
��3� � �C� ~� � ���� ��C� � � � ~� �� � �C�� ���� T� ��  �� �� ���(�C�� ����/� T� �� ��CC � � ���? T� �� C� �� �  � �  , 6� laParam[CC� Z�� ]�� �� � T� �C�  ���$ T� �C�
 � �C� \�
 � ��\��
 ��� �� %�� � � C����& llReturn = &lcProgToDo(&lcParmStr)
 �
 ��� �� F� � � ��� T� �-�� �
 F�� ��
 B�� �� U 
 LCPROGNAME LCEVENT
 LNOLDALIAS
 LCPROGTODO
 LAPARAMEXP LAPARAM	 LCPARMSTR LNCOUNT LLRETURN LLISOPEN OARIAAPPLICATION SYSPATH SYCTRIGG	 CAPOBJNAM	 CEVENT_ID CTRIG_ID	 MPARMEXPR GFSUBSTR	 LCOLDPATH	 LCNEWPATH APPLICATIONHOME CACTVTYPb 4�  � � � 5� � � � � T� �C�� 5� � T� �-�� %�C� STYLE�
��� � T� �a��% Q�  ��� �	 � STYLE��� STYLE� � F�
 � %�C�  ���� � T� �� �� T� �� �� T� �� �� T� �� �� �� T� �C�X�� T� �� �� T� �� �� T� ��  �� � J�-�(� � � %�C� STYDYE�
��l� T� �a��' Q�  ��� �	 � STYDYE��� STYDYE� � %�C� STYINVJL�
���� T� �a��+ Q�  ��� �	 � STYINVJL��� STYINVJL� � %�C� WareHous�
���+ Q�  ��� �	 � WareHous��� WareHous� � F� � G-(� G-(��
 � ��� � F�
 � G-(� G-(��
 ��� � F� �# T� �CC� �� � DEFDEF� � 6�� F� � �Y >�
 ���  �� ��� �� ��� �� ��� �� ��� �� ��CC� �� � � � 6�� ��� �� ��C� STYDYE� ��& %�C� � C� GNUPCWH�
��� ��t� %�C� NC� � �� ��p� T� �� �� T� ��  �� T� �a��! ��C� ICSTYLE�
 GNUPCWH   � �� � �' %�C� STYDYE+
� C� STYINVJL+	���� F� �. >� ��CC� ��	 �
 � � � 6�$+��
 �
 �
 �� � F� � G-(� F�
 � G-(� %�� ��� Q� � � %�� �� � Q� � � %�� ��9� Q�
 � � %�C� �
��W�
 F�� �� � B� U!  LCPSTYLE	 LCPDYELOT LCPWARE LCDESC LCALIAS LNCOST
 LCDISCCODS LLOPNSTYFIL OARIAAPPLICATION DATADIR STYLE DESC AVE_COST	 CDISCCODE LCGLCODE	 LINK_CODE LLSTYDYE
 LLSTYINVJL STYDYE STYINVJL WAREHOUS	 CWARECODE DYELOT GL_LINK
 GFADD_INFO
 LAEVNTTRIG COMPANYINSTALLEDMODULES LCWHCODE LCSTY
 LLFRMADWRE
 GFDOTRIGER REST WHILE	 lpSetLibs`    ��	 lpSetFile3    ��
 gfTempNameg    �� gfSubStr�    ��	 gfGetTime{    ��	 gfMenuBarG    ��
 gfChngComp�    �� gfDoHelp�    �� GPEXIT�    ��
 gfUserListO    �� gfCheckUserR    ��	 gfStation�    ��
 lfGetUsrNm�    ��	 GPRELOGIN\    ��
 gfPrintSet_    �� gfBrowser    �� gfDataBaseProp9    �� GFGENFLTT    ��
 lfGetQCond�"    ��	 lfGetOper�-    ��
 lfRightGet�.    �� GFCPTOPC2    �� GFCPBTTM�2    �� GFCPNEXT�2    ��	 GFCPPRVIS
3    �� GFVCPNEWM3    ��
 GFVCPPRINT�3    �� GFCPEDIT�3    ��
 GFCPDELETE4    ��	 GFCPBROWS[4    ��
 GFCHNGORDR�4    ��
 GFSETFILTR�4    ��	 GPRECHIST�4    �� gfCpSave�4    ��	 gfCpClose�4    ��
 GFTBARONOFz5    ��
 gfAbotAria�5    ��	 gfGetName�5    ��
 gfGetAttch?7    �� UPDDATE8    ��
 gfAdd_Info88    �� gfErrorTrap�8    ��
 gfDoTrigerG:    ��
 gpAdStyWar6>    ��r C ar� �3�1�� B�q�Q� a �qQ�� �A A �5E3��A A � � � A b � Q a Q 4 q � ��q�A 4 � � �� ��A #A B 1 q`1 � � � � � �RQ!DD�aQ� DA � �� 1� q�� � A B � B r�� A � "!"A �� A q� 1A A 01 3� 1 � Q 1 q 1 q 5 Q 1� A 4 q � �A� � �� a� � � q � �A aaA �	�� 1A � � �A � A �� A A � � A � � � A 4 � �� q� � A � � A 14 �Q� AA � 3 q q � � � � A 2 1� � �� �A 4 � `1 4� � � A � �A q � �� � �1 q�� � � bq����aA A � �� � � A a�� �A � q�Q�A � A � � q A A � �a��ABrB �  1 � � � � � "A ��A � a� A A�A� A �� A A � "BA A 2	aa�� A� A �A 1�� A A A A A AA �� 3 � � � �V����� V� A �� 6� �A A � 3 � � �� �� � �� A 3 q� � !� 11A�� 1A !AQ� � A !1�A� �A !1A !�A �AQA �  1  1  1  1  1 1  1  1  1  1  1 1  1  1 1� A 4 �4 � 5 q 11a�1��qA A A � 3 q � � q � �1�A A � � 3 �1 � r �R4 � ���R�
S Q A P1 � 3� SA A � � �� �� A r �U� � � 4��3� ���B A � A� RbA � r A � � B � � p1 � 1� s � Q� QA s � � � � � � � � � � B � a� qA �� �A ��A r a Ar a r 3s Q �1d�� � � A A rq �A q a q a � � A � � A � � A � A A 1                 -     �  S   5   �  =  _   C   T  �  �   F   �  &  �   ~   2&  �)    �   *  �,  +  �   -  �/  >  �   �/  �/  R  �   0  X0  X  �   o0  j6  `  �   �6  �7  �  �   
8  �8  �  �   �8  H9  �  �   ^9  �:  �  �   �:  3?  �  �   H?  QG  �  �   lG  JQ    .  _Q  )Z  f  k  @Z  bh  �  �  �h  �i  �  �  �i  �q  �  �  �q  Xt  6  �  mt  �v  H  �  �v  jy  Z  �  �y  �{  l  �  |  |~  ~  �  �~  �  �  �  �  ��  �  �  ��  �  �  �  5�  ��  �  �  ��  �  �  �  %�  {�  �  �  ��  ڏ  �  �  �  l�    �  ��  Q�    �  h�  �  (  �  
�  ��  -  �  ��  O�  3  �  f�  {�  E    ��  &�  T    =�  Ȝ  n  	  ��  M�  z    d�  V�  �  A  n�  ,�    BMv�      6   (   u   v         @�  �  �          ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ������������������  �  �  �  �  �  �������������������������������������������������������������  �  �  �  �  �  �������  �  �  �  �  �  ����������������������������������������������  �  �  �  �  �  �  ����������  �  �  �  �  �  �������  �  �  �  �  �  �������������������������������������������������������������  �  �  �  �  �  ������������������� ������������������  �  �  �  �  �  �������������������������������������������������������������  �  �  �  �  �  �������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  �  �������������  �  �  �  �  �  �������  �  �  �  �  �  �������������������������������������������������������������  �  �  �  �  �  ������������������� ���������������������  �  �  �  �  �  �������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������������������������������  �  �  �  �  �  ���������������������� ���������������������  �  �  �  �  �  �������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �  �������������������  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������������������������������  �  �  �  �  �  ���������������������� ������������������������  �  �  �  �  �  �������������������������������������������������  �  �  �  �  �  �������������  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �  ����������������������  �  �  �  �  �  �������������  �  �  �  �  �  �������������������������������������������������  �  �  �  �  �  ������������������������� ������������������������  �  �  �  �  �  �������������������������������������������������  �  �  �  �  �  �������������  �  �  �  �  �  �������������������������������  �  �  �  �  �  �  �������������������������  �  �  �  �  �  �������������  �  �  �  �  �  �������������������������������������������������  �  �  �  �  �  ������������������������� ���������������������������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  ����������������  �  �  �  �  �  ����������������������������  �  �  �  �  �  �  ����������������������������  �  �  �  �  �  ����������������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  ���������������������������� ���������������������������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  ����������������  �  �  �  �  �  �������������������������  �  �  �  �  �  �  �������������������������������  �  �  �  �  �  ����������������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  ���������������������������� ������������������������������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  ����������������������  �  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  ������������������������������� ������������������������������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  ������������������������������� ���������������������������������  �  �  �  �  �  �������������������������������  �  �  �  �  �  ����������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������������  �  �  �  �  �  ����������������������  �  �  �  �  �  �������������������������������  �  �  �  �  �  ���������������������������������� ���������������������������������  �  �  �  �  �  �������������������������������  �  �  �  �  �  ����������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �������������  �  �  �  �  �  ����������������������  �  �  �  �  �  �������������������������������  �  �  �  �  �  ���������������������������������� ������������������������������������  �  �  �  �  �  �������������������������  �  �  �  �  �  �������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������  �  �  �  �  �  �������������������������  �  �  �  �  �  ������������������������������������� ������������������������������������  �  �  �  �  �  �������������������������  �  �  �  �  �  �������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������  �  �  �  �  �  �������������������������  �  �  �  �  �  ������������������������������������� ���������������������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  ����������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  ���������������������������������������� ���������������������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  ����������������������������������������������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������  �  �  �  �  �  �������������������  �  �  �  �  �  ���������������������������������������� ������������������������������������������  �  �  �  �  �  �������������  �  �  �  �  �  �������������������������������������������������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������  �  �  �  �  �  �������������  �  �  �  �  �  ������������������������������������������� ������������������������������������������  �  �  �  �  �  �������������  �  �  �  �  �  �������������������������������������������������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������  �  �  �  �  �  �������������  �  �  �  �  �  ������������������������������������������� ���������������������������������������������  �  �  �  �  �  �������  �  �  �  �  �  ����������������������������������������������������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �������  �  �  �  �  �  ���������������������������������������������� ���������������������������������������������  �  �  �  �  �  �������  �  �  �  �  �  ����������������������������������������������������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �������  �  �  �  �  �  ���������������������������������������������� ������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �������������������������������������������������������������������������������������������������������  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������� ������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������  �  �  �  �  �  �������������������������������������  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������� ���������������������������������������������������  �  �  �  �  �  �  �  �  �  ����������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������������������  �  �  �  �  �  �  �  �  �  ���������������������������������������������������� ���������������������������������������������������  �  �  �  �  �  �  �  �  �  ����������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������  �  �  �  �  �  ����������������������������������������  �  �  �  �  �  �  �  �  �  ���������������������������������������������������� ������������������������������������������������������  �  �  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �������������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  �  �  ������������������������������������������������������� ������������������������������������������������������  �  �  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ����������������  �  �  �  �  �  �������������������������������������������  �  �  �  �  �  �  �  ������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������� �������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������ �������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������ ����������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������� ����������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������� �������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������ �������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������ ����������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������� ����������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������� �������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������ �������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������ ����������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������� ����������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������� �������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������ ���������������������������������������  �  �  �  �  �  �����������������������������������  �  �  �  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������ ���������������������������������������  �  �  �  �  �  �  �  �  �����������������������������  �    �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������� ������������������������������������������������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������� ���������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������ ������������������������������������������������������������������������  �  �  �  �  �  �  �����  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������� ���������������������������������������  �  �  �  �  �  ��������������������������������������������  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������� ���������������������������������������  �  �  �  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������ ������������������������������������������������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������ ���������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �����  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������� ������������������������������������������������������������������������  �  �  �  �  �  �  ��������������  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������ ���������������������������������������  �  �  �  �  �  ����������������������������������������������������  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������ ���������������������������������������  �  �  �  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������� ������������������������������������������������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������� ���������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  ��������������  �  �  �  �  �    �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������ ������������������������������������������������������������������������  �  �  �  �  �  �  �����������������������  �  �  �  �  �  �  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������� ���������������������������������������  �  �  �  �  �  ����������������������������������������������������  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������� ���������������������������������������  �  �  �  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������ ������������������������������������������������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  �  �����������    �  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������ ���������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �����������������������  �  �    �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������� ������������������������������������������������������������������������  �  �  �  �  �  �  ��������������������������������  �  �  �  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������ ���������������������������������������  �  �  �  �  �  ����������������������������������������������������  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������ ���������������������������������������  �  �  �  �  �  �  �  �  ����������������������������������  �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������� ������������������������������������������������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  �  ����������������  �  �  �  �  �  ��  �  �  �    �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������� ���������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �������������������������������  �  �  �  �  �  �  �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������ ������������������������������������������������������������������������  �  �  �  �  �  �  �����������������������������������������  �  �    �  �  �  �  �  ��  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  �  �  ������������������������������������������������������������������������������������������������������������������������������������������������������������������������ ����������������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ����������������������������������������������������������������������������������������������������������������������������������������������������������������������������  �  �  ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������� �������������������������������������������������������������������������������������������������������������������������������������������������������������������������������  ������������������������������������������������������������������������������������������������������������������������������������������������������������������������������ �������������������������������������������������������������������������������������������������������������������������������������������������������������������������������  ������������������������������������������������������������������������������������������������������������������������������������������������������������������������������ ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������� BM�       v   (               x   �  �                �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ���       w �����p ���  p����w�����w����pw����pw�����w����w���  ww�����ww���  ww���ww��� www    wwwBM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� �     ���������      ����� �����      ���������     ����������  � ���������  �����������    ���������BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwpwwwwwww wwwwwwp0wwwwww0wwwww 30   wp30�wwwp3 �wtwp30�wDwp30�tDDp30�DDDp30�tDDp3�wDwp0��wtwp��wwwp   wwwBMz       >   (                                       ���   ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ��������������������������D�������D������D������D�����D�����D�����D������D������D�����������������������������BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ��������������������������������������������������������������������������������������������������������������BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ���������������������������������������������������������������������������������������������������������BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������������������D������D�����D�����D����D�����D�����D������D������D��������������������������BMf      v   (               �                     �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������� ����������� ����������� ����������� ���     ��� ����������� ����������� ����������� ����������� ����������� ����������� ����������� ����������� ������  ��� ��������� ������ ���� ���   ���� ����������� ����������� ����������� BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwwwwwww     ww �����ww����w������w�������     ����ww����ww�   wwp wwww wwwwwwpwwwwwwwwwp wwwwwwwwwwBMz       >   (                                       ���   ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��BM�       v   (                                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� �     ���������      ����� ���w�      ���������     ����������  � ���������  �����������    ���������BM�       v   (               x   �  �                �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ��������������������� ������ ������ ����   ��� �� ������ ���������������������������� ���� �������  ������������BM�       >   (                                         ���                                                                                                                                 BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwwwwwwwwppppwwwwwwwpwwwwwwp wwwwwwp�wwwwwp�wwwwww�w    �w���p� �p����w �w ���wwwDDDDGwwwDDDDGwwwBMz       >   (                                       ���   ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��BM�       v   (               x                     �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ������������������������������������������������������������������������������������������������������������BMz       >   (                                       ���   ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��BM�       v   (               x                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwwwwwwww�     �w������     �w{w{w�xxx{g{w{wkw���h{w{wfwxx{f�w{w     �fkw{ww�     �wwwwwwwwwBMz       >   (                                       ���   ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��BM�       v   (               x   �  �                �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ���         pwpwppGp�p        pwpwppGpGp        pwp�ppGpKp        pwpwpp�pGp                wwwwwwwwBM�       v   (               `                         �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� p      fDDDDDD@offffff@n`����p@offffff@n`�����@offffff@nwwwff@o���ff@n    ff@o������`vffffffgBMn       >   (                                       ���   ��  ��  �� ��  �� ��  ��  ��  ��  ��  ��  ��   %   �!      �$  E  m"  ��t3�  �� H� ��� ��5���� 5��  �� �� � %�C� TALKv� ON��^ � G2� T�� �� ON�� �x � T�� �� OFF�� � T�� �C�
 COMPATIBLEv�� GA�9 T� �CC� ARIA.ICO0� � ICON FILE "ARIA.ICO"� �  6�� %�C�
 _1nz0yg89n�
����� DEFINE WINDOW _1nz0yg89n  AT  0.000, 0.000   SIZE 16.615,73.833  FONT "MS Sans Serif", 8  STYLE "B"  FLOAT  NOCLOSE  SHADOW  NOMINIMIZE  DOUBLE  COLOR RGB(,,,192,192,192)  &lcIcon
	 z,� �� � J�-�(� � � %�C� SYDAPPL�
��� F�  �  Q�� � SYDAPPL��� CAPP_ID� T� �a�� �,� F�	 � G((� CAPP_ID� � T�
 �� �� T� �� �� T� �� �� T� �� �� %�C� SYCINST�
���� F�  � Q�� � SYCINST�� T� �a�� ��� F� � � %�C�
 _1nz0yg89n�����	 t,�� � T� �-�� ���	 t,�� � T� �a�� �J 纉  �?� ��� Aria Advantage Series�@�� MS Sans Serif����A�� B��H ��� �?� ��� All rights reserved�@�� MS Sans Serif����A�� B��` �&� �   ���+ (c) Copyright 1990 - 1999 Aria Systems Inc.�@�� MS Sans Serif����A�� B��F �  	 �   ��� Licence number  :�@�� MS Sans Serif����A�� B��H �l' �   ��� Activation key    :�@�� MS Sans Serif����A�� B��E �� �   ��� Company name   :�@�� MS Sans Serif����A�� B��t �� �   ��
 ��  �@�� MS Sans Serif�������X9��v�?���      I@���a� C�
 ���� ENTER Company name��S �&� ��� B������������������������(�&� ��*> A�� 1�C�������S �   �?� B������������������������(�   �~*> A�� 1�C�������L ��� ��� B������������������������(� ��� C�������L ��� �?�= B������������������������(� �?�= C�������v �  	 �   �� ��  �@�� MS Sans Serif�������X9��v�?���      I@���a� C� ���� ENTER Licence number��S �� ��� B������������������������(�� ��*> A�� 1�C�������S �l'
 �?� B������������������������(�l'
 �~*> A�� 1�C�������L �I� ��� B������������������������(�";
 ��� C�������L �I� �?�= B������������������������(�";
 �?�= C�������w �l' �   �� ��  �@�� MS Sans Serif�������X9��v�?���      I@�*�C� ���C� ��� ENTER Activation key��S �   ��� B������������������������(�   ��*> A�� 1�C�������S ��N �?� B������������������������(��N �~*> A�� 1�C�������L � ��� B������������������������(�b ��� C�������L � �?�= B������������������������(�b �?�= C�������t �� �?� �� ���@�� MS Sans Serif����A�� B���������M�?���      $@���      �?���� @*HT \!\?Ok��S �&� � � B������������������������(�&� �?U) A�� 1�C�������S �p� ��� B������������������������(�p� �U) A�� 1�C�������L ��� � � B������������������������(�&� � � C�������L ��� �  ) B������������������������(�&� �  ) C�������> �";  � �  B������������(粝 �?�I C�������> �    �    B������������(��N ��*I C������� T� ��  �� %�C�
 _1nz0yg89n�
��@� t,� � � 9���C� ��� <,� � %��� � ON��t� G2 � � %��� � ON���� GA � � %�C� SYDAPPL�� � 	���� Q�	 � � %�C� SYCINST�� � 	���� Q� � � ��2� �4���� 5��  �� �� � %�C� TALKv� ON��@� G2� T�� �� ON�� �Z� T�� �� OFF�� � T�� �C�
 COMPATIBLEv�� GA�9 T� �CC� ARIA.ICO0� � ICON FILE "ARIA.ICO"� �  6�� %�C�
 _1nz0yg8f5�
��.�\ s,� �CC���8��CC��I�8�(�CC���8���CC��I�8�H�N�������� � J�-�(� � � %�C� SYDAPPL�
���� F�  �  Q�� � SYDAPPL��� CAPP_ID� T� �a�� ��� F�	 � G((� CAPP_ID� � T�
 �� �� T� �� �� T� �� �� T� �� �� %�C� SYCINST�
��!� F�  � Q�� � SYCINST�� T� �a�� �0� F� � � %�C�
 _1nz0yg8f5���a�	 t,�� � T� �-�� �}�	 t,�� � T� �a�� �< �� ������� Aria Advantage Series���������� ��: ��������� All rights reserved���������� ��7 ��������� Company name   :���������� ��K ��������
 ��  �������2���a� C�
 ���� ENTER Company name��M �������� ��  �������2���a� C� ���� ENTER Licence number��N ��
������ ��  �������2�*�C� ���C� ��� ENTER Activation key��< �������� ���������
������� @*HT \!\?Ok��7 ��
������� Activation key :���������� ��7 ��������� Licence number :���������� ��R ���������+ (c) Copyright 1990 - 1996 Aria Systems Inc.�������+��� �� T� ��  �� %�C�
 _1nz0yg8f5�
��?� t,� � � 9��� <,� � %��� � ON��l� G2 � � %��� � ON���� GA � � %�C� SYDAPPL�� � 	���� Q�	 � � %�C� SYCINST�� � 	���� Q� � � � U  CURRAREA TALKSTAT COMPSTAT LCICON
 _1NZ0YG89N LLAPPLUS LLINSUS	 GCSYSHOME CAPP_ID SYDAPPL
 LCCOMPNAME
 GCCOMPNAME	 LCLICENCE	 GCLICENCE	 LCACT_KEY	 GCACT_KEY	 LCOLD_KEY SYCINST LLSAYCNT
 _1NZ0YG8BZ
 _1NZ0YG8CJ PBOK
 LCWINDNOGR	 LFARIAACT
 _1NZ0YG8F5
 _1NZ0YG8FN
 _1NZ0YG8G9  U  � �  ������� J��  �(�  � � � %�� � ���� T� �C� ��� T� �C� �\�� T� �C� C� >��  [�� T� �C� � -��� T� �C� �C� >�\�� T� �-��> o�� � sydappl��� ��C��� ]���� � SY�������  � ��	 ���(�C��  ����!� T� �� C �	 ��  �� �� T�
 �C� �R��) T� �CC�
 �A� C�
 �7� C�
 g6�� T� ��  �� T� �� �� T�
 ��  �� +�� � ���� T� �CC� �G�Z� ��6 T�
 ��
 CC� �G�� C� DMUWC� >�\� �  6�� T� �C� �8�� � T� ��
 ��' T� �C� ��\C� C� >��\��  T� �CCC� � -�>��w�� T� ��  �� T� ��  �� ��	 ���(�� ����' T� �� C� �	 ����\��' T� �� C� �	 ����\�� �� T� �-��( ��	 �C� g�
�(�C� g�
�	���� �� ���(����n� T� �� ���5 T� �C��C� � C� � � T� �  6C�	 Z�P�]�� %�C� >C� >���� T� �C� C� >R�� � +�C� >C� >����! T� �C� C� >�CCC�Z�R��� � T� ��  �� �� ���(�C� >���% T� �� C� � �\C� � �\�� �� T� �C� �=� C� �R�� %�C� � -�C� � -���j� T� �a�� !� � �� %�� a���� !� � �� %�� 
����  R,�� Invalid Activation Key�� T� �-�� ��� T� �a�� � � T� �a�� U 
 LAMODULINS LCINSMODULES	 LCINSPLAT	 LCOLD_KEY	 LCACT_KEY LCKEY	 LLVALDKEY CAPP_ID	 GCSYSHOME LNCOUNT
 LCPLATFORM
 LNPLATFORM LCBINARY	 LNPRIMARY	 LCNOUSERS LNLENACT	 LCHIDCHAR LCOLDACT_KEY	 LLTRILVER	 LNTRILVER LCKEYCONTED LCKEYACT LNHIDLEN	 LLALLDONE� ( %�C�
 lcWindNoGrb� C� C�  �
	��� � H�5 �� � �C��  ��a � /,�C���� T�  �C�  C���� �CC��
�  ��� � /,�CC��
��� T�  �C�  CC��
��� � � U 
 LCWINDNOGRK 5�  � � � � � � T� �C� W�� F�  � %�CC�]b� U��R �
 F�� �� � %�CCC��i����:� T�  �C� CENTv�� G � T� �C�]�� T� �CCC��i��% T� ���� �dC� �d8�d�� T� �CCC��*��  T� �C� CCCCC��iZ�CC� Z���� T� �C� #�� &lcY2KObj    = ldY2KDate
 SET CENT &lcY2KCent
 �
 F�� �� U 	 LCY2KCENT	 LDY2KDATE LCY2KOBJ	 LNY2KINCR LNY2KNEW
 LCY2KALIAS  U  � �  ������� J��  �(�  � � � %�� � ��|� T� �C� ��� T� �C� �\�� T� �C� C� >��  [�� T� �C� � -��� T� �C� �C� >�\�� T� �-��> o�� � sydappl��� ��C��� ]���� � SY�������  � ��	 ���(�C��  ����!� T� �� C �	 ��  �� �� T�
 �C� �R��) T� �CC�
 �A� C�
 �7� C�
 g6�� T� ��  �� T� �� �� T�
 ��  �� +�� � ���� T� �CC� �G�Z� ��6 T�
 ��
 CC� �G�� C� DMUWC� >�\� �  6�� T� �C� �8�� � T� ��
 ��' T� �C� ��\C� C� >��\��  T� �CCC� � -�>��w�� T� ��  �� T� ��  �� ��	 ���(�� ����' T� �� C� �	 ����\��' T� �� C� �	 ����\�� ��( ��	 �C� g�
�(�C� g�
�	��(�" T� �C��C� � C�	 Z�P�]�� %�C� >C� >��K� T� �C� C� >R�� � +�C� >C� >����! T� �C� C� >�CCC�Z�R��� � T� ��  �� �� ���(�C� >����% T� �� C� � �\C� � �\�� �� T� �C� �=� C� �R�� %�C� � -�C� � -���$� T� �a�� !� � �� %�� 
��e�  R,�� Invalid Activation Key�� T� �-�� �x� T� �a�� � � T� �a�� U 
 LAMODULINS LCINSMODULES	 LCINSPLAT	 LCOLD_KEY	 LCACT_KEY LCKEY	 LLVALDKEY CAPP_ID	 GCSYSHOME LNCOUNT
 LCPLATFORM
 LNPLATFORM LCBINARY	 LNPRIMARY	 LCNOUSERS LNLENACT	 LCHIDCHAR LCOLDACT_KEY LCKEYCONTED LCKEYACT LNHIDLEN	 LLALLDONE
 _1nz0yg8bz.    ��
 _1nz0yg8cj5    ��	 lfAriaAct/    �� lfvY2K�    ��
 _1nz0yg8fnp    ��
 _1nz0yg8g9w    ��� � T�a � !A �a ��|� A � q� � � q A � � � � r� Q� � q A �� � � � � A ��d�TG55��g55��x55��G55����� �� A � � Sa A Qa A �� A �� A $T�a � !A �a ���A � q� � � q A � � � � r� Q� � q A �� � � � � A ��s����ss#� �� A b � Sa A Qa A �� A �� A D  1  1 2Q� !�1�� ���A %�� � � !�a1A � r� � qqqA � �tWSAA aA � �QA ��� A A H � A A B � � � � A B � � 1 �� � 1� AA A � 1 �� � a� A Q1a � Q� �qA � 1  1 2Q� !�1�� ���A %�� � � !�a1A � r� � qqqA �!QAA aA � �QA ��� A A E � � � � A B � 1                 �   0  �3  �  �   4  I  �  �   >I  N  a  �   HN  �T  {    �T  �X  �  
  �X  �f  �  BM      v   (   "   "         �                        �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwwwwwwwwwwwwwwww   wwwwwwp   wwwwww   wwwww ���� wwwww   wwww ������� wwww   wwwp���������www   www���    ���www   wwp�����  ����ww   ww�����  �����ww   wp������  �����w   wp������  �����w   w������  ������w   w������  ������w   w������  ������w   p�������  ������   p�������  ������   p�������  ������   p�������  ������   p�������  ������   p�������  ������   p�������  ������   p������   ������   w��������������w   w��������������w   w��������������w   wp������ �����w   wp������  �����w   ww�����  �����ww   wwp�����  ����ww   www���� ����www   wwwp���������www   wwww ������� wwww   wwwww ���� wwwww   wwwwwwp   wwwwww   wwwwwwwwwwwwwwwww   BM      v   (   "   "         �                        �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwwwwwwwwwwwwwwww   wwwwwwp   wwwwww   wwwww """  wwwww   wwww """"""" wwww   wwwp"""""""""www   www""""��""" www   wwp""""/��""""ww   ww""""/��"""" ww   wp"""""/��"""""w   wp""""""��"""""w   w""""""""""""" w   w""""""""""""" w   w""""""""""""" w   p"""""""/"""""""   p"""""""/"""""""   p"""""""/�""""""   p"""""""/�""""""   p""""""""��"""""   p""""""""��"""""   p""""""""/��""""   p""""""""/��""""   w""""��""���"" w   w"""/��""���"" w   w"""/��""���"" w   wp"""/��""���""w   wp"""/��""���""ww   ww"""��"/��"" ww   wwp"""/�"���""ww   www"""/���"" www   wwwp"""""""""www   wwww """"""" wwww   wwwww """  wwwww   wwwwwwp   wwwwww   wwwwwwwwwwwwwwwww   BM      v   (   "   "         �                        �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� wwwwwwwwwwwwwwwww   wwwwwwp   wwwwww   wwwww ���� wwwww   wwww ������� wwww   wwwp���������www   www����������www   wwp�����������ww   ww������������ww   wp�������������w   wp�������������w   w��������������w   w��������������w   w��������������w   p���������������   p���������������   p���������������   p���������������   p���������������   p���������������   p���������������   p���������������   w��������������w   w��������������w   w��������������w   wp�������������w   wp�������������w   ww������������ww   wwp�����������ww   www����������www   wwwp���������www   wwww ������� wwww   wwwww ���� wwwww   wwwwwwp   wwwwww   wwwwwwwwwwwwwwwww   0$   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _RXL010OA0 774405083      /  F      ]      �  �+              �      �                       WINDOWS _RXL02HITG 774405483�      �  �  �                                                             WINDOWS _RXL010OA1 622267727�  �  �                d                                               WINDOWS _RXL02HITN 774405083:  J  k  w  �  �      6  �-                                               WINDOWS _RXL02YMBA 774405483J
  ]
  ~
  �
  �
  �
                                                           WINDOWS _RXL0435CF 622268916�      �  �  �  �                                                           WINDOWS _RXL010OA0 622268149b  s  �  �  �  �                                                           WINDOWS _RXL01NXN7 622268033      '  4  B  _                                                           WINDOWS _RXL01NXNB 622268149�    "  /  A  ^                                                           WINDOWS _RXL01NXNF 774405083�  �      /  L        �"                                               WINDOWS _RXL01NXNK 622268149�  �  �  �    -                                                           WINDOWS _RXL01NXNP 774405083�  �  �  �  �        t  �                                               WINDOWS _RXL01NXNY 622268149*  ;  \  i  {  �                                                           WINDOWS _RXL01NXOW 774405083  "  C  P  g  �      !                                                 WINDOWS _RXL01NXPZ 622268149�  �      ,  I                                                           WINDOWS _RXL01NXPV 630147641�  �  �  �  	  &                                                           WINDOWS _RXL010OA1 622268149�  �  �  �  �  �                                                           WINDOWS _RXL010OA0 630147641g  x  �  �  �  �                                                           WINDOWS _RXL01NXPI 774405083G  X  y  �  �  �                                                           WINDOWS _RXL010OA0 774405083)  :  [  h  {  �                                                           WINDOWS _RXL010OA0 630147641    ?  L  _  |                                                           WINDOWS _RXL010OA0 622268149�  �    ,  ?  \                                                           WINDOWS _RXL03X0YK 622268149�  �  �        1                                                            WINDOWS _RXL01NXQ3 622268149�   �   �   �   �   �                                                            WINDOWS _RXL01NXO2 774405083S!  d!  �!  �!  �!  �!      M"  �                                               WINDOWS _RXL010OA0 774405083
$  $  <$  I$  [$  x$                                                           WINDOWS _RXL01NXOB 630147641�$  %  1%  >%  P%  m%                                                           WINDOWS _RXL01NXOG 630147641�%  �%  &   &  2&  O&                                                           WINDOWS _RXL01NXOK 630147641�&  �&  '  ,'  ?'  \'                                                           WINDOWS _RXL01NXNT 622268149�'  (  '(  4(  F(  c(                                                           WINDOWS _RXL010OA0 622268149�(  �(  )  )  !)  >)                                                           WINDOWS _RXL01NXOS 622268149�)  �)  �)  �)  �)  *                                                           WINDOWS _RXL01NXN2 774405083a*  r*  �*  �*  �*  �*      +  �                                               WINDOWS _RXL01NXP3 774405083�,  	-  �,  �,  -
  *-      �-  �                                               COMMENT RESERVED                                �,                                                            /b                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      JLeft = 3
Top = 141
Width = 520
Height = 200
Name = "Dataenvironment"
      <PROCEDURE BeforeOpenTables
ThisFormSet.SetPath()
ENDPROC
      ����    �   �                         �   %   a       v      p           �  U    ��C�  � � �� U  THISFORMSET	 ARIAFORM2 SHOW Click,     ��1 2                       S       )   �                         cursor      cursor      Cursor1      Dataenvironment      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "sydappl"
Order = "cappname"
CursorSource = ..\..\..\sysfiles\sydappl.dbf
Name = "Cursor1"
      2      2      ariaformset      ..\..\..\classes\main.vcx      formset      ariaformset      �AutoRelease = .T.
formhastoolbar = 
Name = "ariaformset"
Ariaform1.Height = 325
Ariaform1.Width = 347
Ariaform1.DoCreate = .T.
Ariaform1.BorderStyle = 2
Ariaform1.Caption = "About ARIA"
Ariaform1.MaxButton = .F.
Ariaform1.Name = "Ariaform1"
      ?PROCEDURE Ariaform1.QueryUnload
Release ThisFormSet
ENDPROC
      ����    �   �                         �B   %   G       l      f           �  U  
  <�  � U  THISFORMSET Ariaform1.QueryUnload,     ��1 q 1                    !   4       )   �                         ariaform      ..\..\..\classes\main.vcx      form      	Ariaform2      ariaformset      �Height = 270
Width = 455
DoCreate = .T.
BorderStyle = 2
Caption = "Modules Build Numbers"
MaxButton = .F.
Visible = .F.
Name = "Ariaform2"
      �PROCEDURE QueryUnload
NoDefault
ThisFormSet.AriaForm2.Visible = .F.
ThisFormset.Ariaform1.cmdModulesBuild.SetFocus()

ENDPROC
PROCEDURE Show
LPARAMETERS nStyle

ENDPROC
     4���                              #�   %   �       �      �           �  U  X $ %�C� oAriaApplicationb� O��7 � T�  � �� � �� �Q � T�  � �C� X�� � U  THIS CAPTION OARIAAPPLICATION ARIAVERSION Init,     ��1 A1� !A 1                       �       )                           ariaformset.Ariaform1      arialistbox      ..\..\..\classes\main.vcx      listbox      Arialistbox1      ariaformset.Ariaform2      �ColumnCount = 3
ColumnWidths = "25,200,150"
RowSourceType = 6
RowSource = "SYDAPPL.capp_id,capp_Name,cmdlbuild"
ControlSource = ""
Height = 262
ColumnLines = .F.
Left = 125
Top = 4
Width = 327
Name = "Arialistbox1"
      image      image      Image1      ariaformset.Ariaform2      mPicture = ..\..\bmps\logo.bmp
BackStyle = 0
Height = 118
Left = 3
Top = 4
Width = 117
Name = "Image1"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel1      ariaformset.Ariaform1      BCaption = "Version:"
Left = 137
Top = 239
Name = "Arialabel1"
      image      image      Image1      ariaformset.Ariaform1      �Picture = ..\..\bmps\logo.bmp
Stretch = 0
BackStyle = 0
Height = 118
Left = 6
Top = 5
Width = 117
ZOrderSet = 1
Name = "Image1"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel3      ariaformset.Ariaform1      tBackStyle = 0
Caption = "This product is licensed to:"
Left = 137
Top = 185
ZOrderSet = 2
Name = "Arialabel3"
      	arialabel      ..\..\..\classes\main.vcx      label      lblCompanyName      ariaformset.Ariaform1      �AutoSize = .F.
FontBold = .T.
Alignment = 2
BackStyle = 0
Caption = "Company Name"
Height = 15
Left = 138
Top = 200
Width = 200
ZOrderSet = 3
Name = "lblCompanyName"
      �PROCEDURE Init
IF TYPE("gcCompName") = "C"
  This.Caption = ALLTRIM(PROPER(gcCompName))
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
     ���                              �   %   �       �      �           �  U  U  %�C�
 gnMaxUsersb� N��4 � T�  � �CCC� Z���� �N � T�  � �C� X�� � U  THIS CAPTION
 GNMAXUSERS Init,     ��1 �a� !A 1                       �       )                           	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel4      ariaformset.Ariaform1      cBackStyle = 0
Caption = "Product ID:"
Left = 137
Top = 224
ZOrderSet = 4
Name = "Arialabel4"
      	arialabel      ..\..\..\classes\main.vcx      label      lblProductId      ariaformset.Ariaform1      dBackStyle = 0
Caption = "Product ID"
Left = 257
Top = 224
ZOrderSet = 5
Name = "lblProductId"
      �PROCEDURE Init
IF TYPE("gcLicence") = "C"
  This.Caption = ALLTRIM(PROPER(gcLicence))
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
     ���                              �z   %   �       �      �           �  U  T  %�C�	 gnUserLogb� N��3 � T�  � �CCC� Z���� �M � T�  � �C� X�� � U  THIS CAPTION	 GNUSERLOG Init,     ��1 �a� !A 1                       �       )                           	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel6      ariaformset.Ariaform1      qBackStyle = 0
Caption = "Users currently loged in:"
Left = 137
Top = 269
ZOrderSet = 6
Name = "Arialabel6"
      	arialabel      ..\..\..\classes\main.vcx      label      lblUsersLogedIn      ariaformset.Ariaform1      �AutoSize = .T.
BackStyle = 0
Caption = "Userss loged in"
Height = 15
Left = 257
Top = 268
Width = 74
ZOrderSet = 7
Name = "lblUsersLogedIn"
      �PROCEDURE Init
IF Type("gnUserLog") = "N"
  This.Caption = ALLTRIM(PROPER(STR(gnUserLog)))
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
     ���                              �]   %   �       �      �           �  U  R  %�C�	 gcLicenceb� C��1 � T�  � �CC� ���� �K � T�  � �C� X�� � U  THIS CAPTION	 GCLICENCE Init,     ��1 �A� !A 1                              )                           	arialabel      ..\..\..\classes\main.vcx      label      Arialabel18      ariaformset.Ariaform1      fBackStyle = 0
Caption = "(212) 714-1378"
Left = 263
Top = 80
ZOrderSet = 8
Name = "Arialabel18"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel17      ariaformset.Ariaform1      \BackStyle = 0
Caption = "Fax:"
Left = 137
Top = 80
ZOrderSet = 8
Name = "Arialabel17"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel16      ariaformset.Ariaform1      fBackStyle = 0
Caption = "(212) 714-1334"
Left = 263
Top = 65
ZOrderSet = 8
Name = "Arialabel16"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel15      ariaformset.Ariaform1      iBackStyle = 0
Caption = "Techinal Support:"
Left = 137
Top = 64
ZOrderSet = 8
Name = "Arialabel15"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel14      ariaformset.Ariaform1      kBackStyle = 0
Caption = "New York, NY, 10016"
Left = 232
Top = 35
ZOrderSet = 8
Name = "Arialabel14"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel13      ariaformset.Ariaform1      mBackStyle = 0
Caption = "16 East, 34th street."
Left = 232
Top = 20
ZOrderSet = 8
Name = "Arialabel13"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel12      ariaformset.Ariaform1      iBackStyle = 0
Caption = "ARIA Systems, Inc."
Left = 232
Top = 5
ZOrderSet = 8
Name = "Arialabel12"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel11      ariaformset.Ariaform1      bBackStyle = 0
Caption = "Product of:"
Left = 137
Top = 5
ZOrderSet = 8
Name = "Arialabel11"
      arialine      ..\..\..\classes\main.vcx      line      	Arialine4      ariaformset.Ariaform1      THeight = 0
Left = 134
Top = 101
Width = 208
ZOrderSet = 16
Name = "Arialine4"
      arialine      ..\..\..\classes\main.vcx      line      	Arialine3      ariaformset.Ariaform1      SHeight = 0
Left = 134
Top = 58
Width = 208
ZOrderSet = 16
Name = "Arialine3"
      	arialabel      ..\..\..\classes\main.vcx      label      lblNumberOfUsers      ariaformset.Ariaform1      ~AutoSize = .T.
BackStyle = 0
Caption = "Number of userss"
Left = 257
Top = 254
ZOrderSet = 7
Name = "lblNumberOfUsers"
      �PROCEDURE Init
IF Type("gnMaxUsers") = "N"
  This.Caption = ALLTRIM(PROPER(STR(gnMaxUsers)))
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
     ���                              �z   %   �       �      �           �  U  S  %�C�
 gcCompNameb� C��2 � T�  � �CC� ���� �L � T�  � �C� X�� � U  THIS CAPTION
 GCCOMPNAME Init,     ��1 �A� !A 1                       �       )                           	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel7      ariaformset.Ariaform1      BackStyle = 0
Caption = "Copyright� 1990-2003 ARIA Systems, Inc."
Left = 137
Top = 109
ZOrderSet = 8
Name = "Arialabel7"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel8      ariaformset.Ariaform1      lBackStyle = 0
Caption = "All rights reserved."
Left = 194
Top = 124
ZOrderSet = 9
Name = "Arialabel8"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel9      ariaformset.Ariaform1      �BackStyle = 0
Caption = "This program is protected by US and"
Height = 15
Left = 137
Top = 139
Width = 174
ZOrderSet = 10
Name = "Arialabel9"
      	arialabel      ..\..\..\classes\main.vcx      label      Arialabel10      ariaformset.Ariaform1      �BackStyle = 0
Caption = "international copyright laws."
Height = 15
Left = 137
Top = 154
Width = 132
ZOrderSet = 11
Name = "Arialabel10"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel5      ariaformset.Ariaform1      hBackStyle = 0
Caption = "Number of users:"
Left = 137
Top = 254
ZOrderSet = 6
Name = "Arialabel5"
      arialine      ..\..\..\classes\main.vcx      line      	Arialine1      ariaformset.Ariaform1      THeight = 0
Left = 134
Top = 175
Width = 208
ZOrderSet = 16
Name = "Arialine1"
      arialine      ..\..\..\classes\main.vcx      line      	Arialine2      ariaformset.Ariaform1      THeight = 0
Left = 134
Top = 290
Width = 208
ZOrderSet = 17
Name = "Arialine2"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel2      ariaformset.Ariaform1      ACaption = "Version"
Left = 257
Top = 239
Name = "Arialabel2"
      �PROCEDURE Init
IF TYPE("oAriaApplication") = "O"
  This.Caption = oAriaApplication.AriaVersion
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
      ����    �   �                         �   %   V       v      p           �  U    ��C�  � �� U  THISFORMSET SETPATH BeforeOpenTables,     ��1 � 1                       1       )   �                         )MS Sans Serif, 0, 8, 5, 13, 11, 12, 2, 0
      cmdModulesBuild      commandbutton      ariacommandbutton      ..\..\..\classes\main.vcx      gAutoSize = .T.
Top = 297
Left = 204
Caption = "Modules Build Numbers..."
Name = "cmdModulesBuild"
      ^PROCEDURE Click
ThisFormSet.AriaForm2.Show()
*ThisFormSet.AriaForm2.Visible = .T.
ENDPROC
     [���    B  B                        �   %   �       �      �           �  U  +  �� T�  � � �-�� ��C�  � � � �� U  THISFORMSET	 ARIAFORM2 VISIBLE	 ARIAFORM1 CMDMODULESBUILD SETFOCUS
  ��  � U  NSTYLE QueryUnload,     �� Show�     ��1 A 13 q 2                       y         �   �       )   B                  0   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _RXK12U6WL 843935168      /  F      ]      �  �9              �      �                       WINDOWS _RXK19XY48 843937600�      �  �  �                                                             WINDOWS _RXK12U6WS 843937600�  �  �              =  	I                                             WINDOWS _RXK17IBJE 8439351684      A  N  \  y      
  �7                                               WINDOWS _RXK14O4OR 843935168"  3  T  a  s  �        �5                                               WINDOWS _RXK14O4OU 843935168#  4  U  b  x  �      b   �G                                               WINDOWS _RXK12U6WL 843935168�"  �"  �"  �"  �"  #      w#  �3                                               WINDOWS _RXK1DZQ2J 843935168�$  �$  �$  �$  �$  �$      �%  QC                                               WINDOWS _RZA132TIZ 843935168#'  4'  U'  b'  t'  �'      �'  n1                                               WINDOWS _RXK12U6WL 843935168
)  )  <)  I)  [)  x)      �)  u/                                               WINDOWS _RXK12U6WL 843935168�*  +  #+  0+  H+  e+      ,  F                                               WINDOWS _RXK12U6WL 843935168D.  U.  v.  �.  �.  �.      7/  �,                                               WINDOWS _RXK12U6WL 843935168O0  `0  �0  �0  �0  �0      01  *                                               WINDOWS _RXK12U6WL 843935168H2  Y2  z2  �2  �2  �2      c3  0(                                               WINDOWS _RXK15A4XW 843935168{4  �4  �4  �4  �4  �4      �5  �%                                               WINDOWS _RXK12U6WL 843935168�6  �6  �6  �6  �6  	7      s7  �#                                               WINDOWS _RXK15G9VI 843935168�8  �8  �8  �8  �8  �8      e9  $!                                               WINDOWS _RXK15G9VM 843935168�:  �:  �:  �:  �:  �:      w;  I                                               WINDOWS _RXK15G9VQ 843935168�<  �<  �<  �<  �<  =      x=  H                                               WINDOWS _RXK12U6WL 843935168�>  �>  �>  �>  �>  �>      U?  e                                               WINDOWS _RXK1AQ5DL 843935168h@  x@  �@  �@  �@  �@      /A  �                                               WINDOWS _RXK12U6WL 843935168LB  \B  }B  �B  �B  �B      C  �                                               WINDOWS _RXK1DZQ2M 843935168;B  B  B  �A  �A  }D      E  �                                               WINDOWS _RXK1AQ5DN 843935168�A  �A  �A  �<  mA  �?      �?  �                                               WINDOWS _RXK1AQ5DR 843935168v>  U>  @>  3.  g<  �-      �=  n                                               WINDOWS _RXK1E868T 626496041'  �-  '  �&  �&  M"                                                           WINDOWS _RZA12WAY8 8439351684"  "  �!  �  �  p      �;  �                                               COMMENT RESERVED                                ?                                                            Q*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
      <PROCEDURE BeforeOpenTables
ThisFormSet.SetPath()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         cursor      cursor      Cursor1      Dataenvironment      �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "syuuser"
Order = "cuser_id"
CursorSource = ..\..\..\sysfiles\syuuser.dbf
Name = "Cursor1"
      2      2      ariaformset      ..\..\..\classes\main.vcx      formset      ariaformset     !AutoRelease = .T.
WindowType = 1
formhastoolbar = 
Name = "ariaformset"
Ariaform1.Height = 253
Ariaform1.Width = 339
Ariaform1.DoCreate = .T.
Ariaform1.BorderStyle = 2
Ariaform1.Caption = "Login"
Ariaform1.MaxButton = .F.
Ariaform1.MinButton = .F.
Ariaform1.Name = "Ariaform1"
     �PROCEDURE done
LOCAL lcUserId, lcEnteredPassword, lcCoddedPassword
LOCAL llAdministrator, llValidUserLogin, lcTitle, lcMsg

lcUserId          = PADR(ALLTRIM(THISFORMSET.Ariaform1.txtUserID.VALUE), 10)
lcEnteredPassword = UPPER(ALLTRIM(THISFORMSET.Ariaform1.txtPassword.VALUE))
lcCoddedPassword  = ALLTRIM(SYS(2007, lcEnteredPassword))
llAdministrator   = (lcUserId = "ADMN") AND (lcEnteredPassword = "ARIA")
llValidUserID     = SEEK(lcUserId,'SYUUSER')
llValidPassword   = llValidUserID AND (lcCoddedPassword == ALLTRIM(syuUser.cUsr_Pass))

*E039010,RRE 07/02/05 Limiting the access to the maximum users number [Begin]
*llValidUserLogin  = llValidUserID AND llValidPassword
lnCurUsers = 0
lnCurUsers = gfUserList(.T.)
IF !USED('SYUSTATC')
  SELECT 0
  USE (oAriaApplication.SysPath+'SYUSTATC')

ENDIF
LOCATE FOR cuser_id = lcUserId
IF FOUND('SYUSTATC')
  llUserExist =.T.
ELSE
  llUserExist =.F.
ENDIF
IF  ! llUserExist
  IF gnMaxUsers > lnCurUsers
    llExceedsMax = .F.
  ELSE
    llExceedsMax = .T.
    IF  !llAdministrator
      MESSAGEBOX("You have exceeded number of users ")
    ENDIF
  ENDIF
ELSE
  llExceedsMax = .F.
ENDIF
llValidUserLogin  = llValidUserID AND llValidPassword AND !llExceedsMax
*E039010,RRE 07/02/05 Limiting the access to the maximum users number [End  ]

*E039010,RRE 07/02/05 Limiting the access to the maximum users number [BEGIN  ]
*!*	IF llAdministrator OR llValidUserLogin
*!*	  THISFORMSET.uRetVal = lcUserId
*!*	  RELEASE THISFORMSET
*!*	ELSE
*!*	  THISFORMSET.uRetVal = .F.
*!*	  lcTitle = PROPER(THISFORMSET.Ariaform1.CAPTION + SPACE(1) + "Error")
*!*	  lcMsg   = IIF(llValidUserID,;
*!*	    "Invalid password. Do you want to try again...?",;
*!*	    "Invalid user ID or password. Do you want to try again...?")
*!*	    IF MESSAGEBOX(lcMsg, 20, lcTitle) = 6
*!*	    THISFORMSET.Ariaform1.txtPassword.VALUE = SPACE(8)
*!*	    THISFORMSET.Ariaform1.txtUserID.SETFOCUS()
*!*	  ELSE
*!*	    RELEASE THISFORMSET
*!*	  ENDIF
*!*	ENDIF
IF llAdministrator OR llValidUserLogin
  THISFORMSET.uRetVal = lcUserId
  RELEASE THISFORMSET
ELSE
  THISFORMSET.uRetVal = .F.
  lcTitle = PROPER(THISFORMSET.Ariaform1.CAPTION + SPACE(1) + "Error")
  IF !llExceedsMax
    lcMsg   = IIF(llValidUserID,;
      "Invalid password. Do you want to try again...?",;
      "Invalid user ID or password. Do you want to try again...?")
    IF MESSAGEBOX(lcMsg, 20, lcTitle) = 6
      THISFORMSET.Ariaform1.txtPassword.VALUE = SPACE(8)
      THISFORMSET.Ariaform1.txtUserID.SETFOCUS()
    ELSE
      RELEASE THISFORMSET
    ENDIF
  ELSE
    RELEASE THISFORMSET
  ENDIF
ENDIF

*E039010,RRE 07/02/05 Limiting the access to the maximum users number [END   ]

ENDPROC
PROCEDURE showuserlist
IF TYPE("gnUserLog") = "N" 
 IF gnUserLog > 0
   = gfUserList()
 ELSE
   lcTitle = PROPER(This.AriaForm1.Caption + SPACE(1) + "Information")
   = MessageBox("No users are currently logged in the system.", 64, lcTitle)
 ENDIF
ENDIF
ENDPROC
PROCEDURE Unload
DoDefault()
RETURN This.uRetVal
ENDPROC
PROCEDURE Ariaform1.RightClick
IF ThisForm.Ariashortcut.ShowShortCut(ThisForm, "Show User List", "T") = 1
  ThisFormSet.ShowUserList()
ENDIF
ENDPROC
     |���    c  c                        ,�   %   �       
     �           �  U     ��C�  � �� ��C�  � � �� U  THISFORM
 RIGHTCLICK	 TXTUSERID SETFOCUS   ��C�  � �� ��C� � � �� U  THISFORMSET SHOWUSERLIST THISFORM	 TXTUSERID SETFOCUS
 RightClick,     �� Click{     ��1 � 2 � 1                       J         f   �       )   c                       )���                              "g   %   �       �      �           �  U    T�  � �-�� <�  � U  THISFORMSET URETVAL  ��C�  � �� U  THISFORM
 RIGHTCLICK Click,     ��
 RightClick^     ��1 � q 3 � 1                       A         b   w       )                          ���                              �z   %   }       �      �           �  U    ��C�  � �� U  THISFORMSET DONE  ��C�  � �� U  THISFORM
 RIGHTCLICK Click,     ��
 RightClickS     ��1 � 2 � 1                       #         D   Y       )                          ����    �  �                        �<   %   �       >     (          �  U  T  %�C�	 gnUserLogb� N��3 � T�  � �CCC� Z���� �M � T�  � �C� X�� � U  THIS CAPTION	 GNUSERLOG  ��C�  � �� U  THISFORMSET SHOWUSERLIST  ��C�  � �� U  THISFORM
 RIGHTCLICK Init,     �� Click�     ��
 RightClick�     ��1 �a� !A 2 � 2 � 1                       �         �   �      	   �   �       )   �                        ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      \Top = 135
Left = 244
Height = 15
Width = 13
Caption = "?"
Name = "Ariacommandbutton1"
      ariaformset.Ariaform1      Ariacommandbutton1      !uretval
*done 
*showuserlist 
      image      image      Image1      ariaformset.Ariaform1      �Picture = ..\..\bmps\logo.bmp
Stretch = 0
BackStyle = 0
Height = 118
Left = 4
Top = 4
Width = 117
ZOrderSet = 1
Name = "Image1"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel1      ariaformset.Ariaform1      sBackStyle = 0
Caption = "This product is licensed to:"
Left = 130
Top = 81
ZOrderSet = 2
Name = "Arialabel1"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      lblCompanyName      ariaformset.Ariaform1      �AutoSize = .F.
FontBold = .T.
FontUnderline = .T.
Alignment = 2
BackStyle = 0
Caption = "Company Name"
Height = 15
Left = 130
Top = 96
Width = 200
ZOrderSet = 3
Name = "lblCompanyName"
      �PROCEDURE Init
IF TYPE("gcCompName") = "C"
  This.Caption = ALLTRIM(gcCompName)
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         commandbutton      ..\..\..\classes\main.vcx      ariacommandbutton      ETop = 124
Left = 9
Height = 14
Width = 15
Name = "Ariashortcut"
      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel5      ariaformset.Ariaform1      gBackStyle = 0
Caption = "Number of users"
Left = 130
Top = 120
ZOrderSet = 6
Name = "Arialabel5"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel2      ariaformset.Ariaform1      }BackStyle = 0
Caption = "Users currently loged in"
Height = 15
Left = 130
Top = 135
ZOrderSet = 6
Name = "Arialabel2"
      lPROCEDURE Click
ThisFormSet.ShowUserList()
ENDPROC
PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         ariaformset.Ariaform1      Ariashortcut      custom      ariashortcut      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel4      ariaformset.Ariaform1      YBackStyle = 0
Caption = ":"
Left = 260
Top = 135
ZOrderSet = 6
Name = "Arialabel4"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel3      ariaformset.Ariaform1      YBackStyle = 0
Caption = ":"
Left = 260
Top = 120
ZOrderSet = 6
Name = "Arialabel3"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      lblNumberOfUsers      ariaformset.Ariaform1      �AutoSize = .F.
BackStyle = 0
Caption = "Number of userss"
Height = 15
Left = 266
Top = 120
Width = 64
ZOrderSet = 7
Name = "lblNumberOfUsers"
      �PROCEDURE Init
IF Type("gnMaxUsers") = "N"
  This.Caption = ALLTRIM(PROPER(STR(gnMaxUsers)))
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         ..\..\..\classes\utility.vcx      ZTop = 223
Left = 250
Height = 23
Width = 84
Caption = "\<Cancel"
Name = "cmdCancel"
      	cmdCancel      	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel7      ariaformset.Ariaform1      }BackStyle = 0
Caption = "Copyright� 1990-2002 ARIA Systems, Inc."
Left = 130
Top = 5
ZOrderSet = 8
Name = "Arialabel7"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel8      ariaformset.Ariaform1      kBackStyle = 0
Caption = "All rights reserved."
Left = 187
Top = 20
ZOrderSet = 9
Name = "Arialabel8"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      
Arialabel9      ariaformset.Ariaform1      �AutoSize = .F.
BackStyle = 0
Caption = "This program is protected by US and"
Height = 32
Left = 130
Top = 35
Width = 307
ZOrderSet = 10
Name = "Arialabel9"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      Arialabel10      ariaformset.Ariaform1      �BackStyle = 0
Caption = "international copyright laws."
Height = 15
Left = 130
Top = 50
Width = 132
ZOrderSet = 11
Name = "Arialabel10"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      Arialabel11      ariaformset.Ariaform1      bBackStyle = 0
Caption = "User ID:"
Left = 130
Top = 166
ZOrderSet = 12
Name = "Arialabel11"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\..\classes\main.vcx      label      Arialabel12      ariaformset.Ariaform1      cBackStyle = 0
Caption = "Password:"
Left = 130
Top = 190
ZOrderSet = 13
Name = "Arialabel12"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         �   %   V       v      p           �  U    ��C�  � �� U  THISFORMSET SETPATH BeforeOpenTables,     ��1 � 1                       1       )   �                         ariatextbox      ..\..\..\classes\main.vcx      textbox      	txtUserID      ariaformset.Ariaform1      {Format = "!"
Left = 184
MaxLength = 10
SelectOnEntry = .T.
Top = 163
Width = 120
ZOrderSet = 14
Name = "txtUserID"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      �PROCEDURE RightClick
ThisForm.RightClick()
ThisForm.txtUserID.SetFocus()
ENDPROC
PROCEDURE Click
ThisFormSet.ShowUserList()
ThisForm.txtUserID.SetFocus()
ENDPROC
      ariaformset.Ariaform1      cmdOk      ariatextbox      ..\..\..\classes\main.vcx      textbox      txtPassword      ariaformset.Ariaform1      lLeft = 184
MaxLength = 8
Top = 187
Width = 98
PasswordChar = "*"
ZOrderSet = 15
Name = "txtPassword"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      �PROCEDURE Click
ThisFormSet.uRetVal = .F.
RELEASE ThisFormSet

ENDPROC
PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      commandbutton      ..\..\..\classes\main.vcx      ariacommandbutton      arialine      ..\..\..\classes\main.vcx      line      	Arialine1      ariaformset.Ariaform1      SHeight = 0
Left = 126
Top = 71
Width = 208
ZOrderSet = 16
Name = "Arialine1"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      dPROCEDURE Click
ThisFormSet.Done()
ENDPROC
PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      aTop = 223
Left = 159
Height = 23
Width = 84
Caption = "\<Ok"
Default = .T.
Name = "cmdOk"
      arialine      ..\..\..\classes\main.vcx      line      	Arialine3      ariaformset.Ariaform1      THeight = 0
Left = 126
Top = 215
Width = 208
ZOrderSet = 17
Name = "Arialine3"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ariaformset.Ariaform1      commandbutton      ..\..\..\classes\main.vcx      ariacommandbutton      ariaformset.Ariaform1      lblUsersLogedIn      label      ..\..\..\classes\main.vcx      	arialabel      arialine      ..\..\..\classes\main.vcx      line      	Arialine2      ariaformset.Ariaform1      THeight = 0
Left = 126
Top = 156
Width = 208
ZOrderSet = 17
Name = "Arialine2"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
     $���                              r�   %   �       �      �           �  U    ��C�  � �� U  THISFORMSET SHOWUSERLIST  ��C�  � �� U  THISFORM
 RIGHTCLICK Click,     ��
 RightClick[     ��1 � 2 � 1                       +         L   a       )                           �AutoSize = .F.
BackStyle = 0
Caption = "Userss loged in"
Height = 15
Left = 266
Top = 135
Width = 54
ZOrderSet = 7
Name = "lblUsersLogedIn"
      �PROCEDURE Init
IF Type("gnUserLog") = "N"
  This.Caption = ALLTRIM(PROPER(STR(gnUserLog)))
ELSE
  This.Caption = SPACE(0)
ENDIF
ENDPROC
PROCEDURE Click
ThisFormSet.ShowUserList()
ENDPROC
PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
     p���    W  W                        ��   %   �       �   	   �           �  U  U  %�C�
 gnMaxUsersb� N��4 � T�  � �CCC� Z���� �N � T�  � �C� X�� � U  THIS CAPTION
 GNMAXUSERS  ��C�  � �� U  THISFORM
 RIGHTCLICK Init,     ��
 RightClick�     ��1 �a� !A 2 � 1                       �         �   �       )   W                       l���    S  S                        ��   %   �       �   	   �           �  U  Q  %�C�
 gcCompNameb� C��0 � T�  � �C� ��� �J � T�  � �C� X�� � U  THIS CAPTION
 GCCOMPNAME  ��C�  � �� U  THISFORM
 RIGHTCLICK Init,     ��
 RightClick�     ��1 �!� !A 2 � 1                       y         �   �       )   S                       ���                                �   %   �      �  F   �          �  U  � ��  � � � �� � � � � T�  �CC� � �	 �
 ��
��� T� �CC� � � �
 �f�� T� �CC��� ]���& T� ��  � ADMN� � � ARIA	�� T� �C�  � SYUUSER��� T� �� � � C� � �	�� T� �� �� T� �Ca� �� %�C� SYUSTATC�
�� � F�  � Q�� � � SYUSTATC�� � -�� �  �� %�C� SYUSTATC4��T� T� �a�� �g� T� �-�� � %�� 
���� %�� � ���� T� �-�� ��� T� �a�� %�� 
����. ��C�" You have exceeded number of users �x�� � � ��� T� �-�� � T� �� � � 	� � 
	�� %�� � � ��J� T� � ��  �� <� � �� T� � �-��$ T� �C� � � C�X� Error��� %�� 
��l�� T� �C� �4 �. Invalid password. Do you want to try again...?�< �9 Invalid user ID or password. Do you want to try again...?6�� %�C� �� �x���Y� T� � � �
 �C�X�� ��C� � �	 � �� �h� <� � � �{� <� � � � U  LCUSERID LCENTEREDPASSWORD LCCODDEDPASSWORD LLADMINISTRATOR LLVALIDUSERLOGIN LCTITLE LCMSG THISFORMSET	 ARIAFORM1	 TXTUSERID VALUE TXTPASSWORD LLVALIDUSERID LLVALIDPASSWORD SYUUSER	 CUSR_PASS
 LNCURUSERS
 GFUSERLIST OARIAAPPLICATION SYSPATH CUSER_ID LLUSEREXIST
 GNMAXUSERS LLEXCEEDSMAX URETVAL CAPTION SETFOCUS�  %�C�	 gnUserLogb� N��� � %��  � ��8 �
 ��C� �� �� �* T� �C� � � C�X� Information���> ��C�, No users are currently logged in the system.�@� �x�� � � U 	 GNUSERLOG
 GFUSERLIST LCTITLE THIS	 ARIAFORM1 CAPTION 	 ��C��� B��  � �� U  THIS URETVALE 1 %�C �  � Show User List� T�  � � ���> � ��C� � �� � U  THISFORM ARIASHORTCUT SHOWSHORTCUT THISFORMSET SHOWUSERLIST done,     �� showuserlist    �� Unload�    �� Ariaform1.RightClick    ��1 � 1��Qa��� � �� �B � q� � � A � � � � � �A A � � A �0Aq � � A� #��1� q A � q A A 5 �� � ��A A 2 � � 2 � A 1                       �
     6   �
  �  S   ?   �     ]   B   +  �  a    )                      0   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _RXK1FBALJ 622247694      /  F      ]                                �                       WINDOWS _RXK1FBALK 622264406�  �  �  �      �                                                           WINDOWS _RXK1FBALR 622247694  $  B  Q  e  �                                                           WINDOWS _RXK1FBALX 622247694�    ,  9  K  h                                                           WINDOWS _RXK1FBAM0 628588085�  �    1  K  h      �  �                                               COMMENT RESERVED                                �                                                            �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      Name = "Dataenvironment"
      1      2      ariaformset      ..\..\classes\main.vcx      formset      ariaformset     %AutoRelease = .T.
WindowType = 1
formhastoolbar = 
Name = "ariaformset"
Ariaform1.Height = 247
Ariaform1.Width = 272
Ariaform1.DoCreate = .T.
Ariaform1.BorderStyle = 2
Ariaform1.Caption = "User List"
Ariaform1.MaxButton = .F.
Ariaform1.MinButton = .F.
Ariaform1.Name = "Ariaform1"
      arialistbox      ..\..\classes\main.vcx      listbox      Arialistbox1      ariaformset.Ariaform1      sRowSourceType = 5
RowSource = "laUserList"
Height = 183
Left = 5
Top = 28
Width = 261
Name = "Arialistbox1"
      	arialabel      ..\..\classes\main.vcx      label      
Arialabel1      ariaformset.Ariaform1      uBackStyle = 0
Caption = "List of the users currently logged to the system"
Left = 8
Top = 9
Name = "Arialabel1"
      ariacommandbutton      ..\..\classes\main.vcx      commandbutton      Ariacommandbutton1      ariaformset.Ariaform1      FTop = 218
Left = 182
Caption = "\<Ok"
Name = "Ariacommandbutton1"
      1PROCEDURE Click
ThisFormSet.Release()
ENDPROC
      ����    �   �                         Q   %   V       k      e           �  U    ��C�  � �� U  THISFORMSET RELEASE Click,     ��1 � 1                       &       )   �                         )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
BMz       >   (                                       ���                                                             0   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _RFI0I63K8 629244495      /  F      ]                          �      �                       WINDOWS _RFI0I63KA 858563671�      �  �      �        AT                  �Q                           WINDOWS _RFJ0M08NC 858563671�Q      �Q  �Q  �Q  �P      �M  G                                               WINDOWS _RFJ0M08NH 858563671�M      �M  �M  �M  +M      L  �E                                               WINDOWS _RFI0I63K8 858563671�K      �K  �K  �K  FK      �J  �D                                               WINDOWS _RFM0Q5S2O 561078665�J  �J  �J  |J  oJ  9J                                                           COMMENT RESERVED                                J                                                            �t                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
      1      1      form      form      Form1      DataSession = 1
ScaleMode = 3
Height = 283
Width = 506
ShowWindow = 0
DoCreate = .T.
ShowTips = .T.
AutoCenter = .T.
Caption = "Form1"
MinHeight = 94
MinWidth = 172
MDIForm = .F.
WindowType = 1
firsttime = .T.
defaultshortcut = 'TTTT'
lastselection = ("")
Name = "Form1"
     @�PROCEDURE trapkeys
 ON KEY LABEL CTRL+Q LL=.T.
ON KEY LABEL CTRL+W LL=.T.
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END GO BOTT
ON KEY LABEL ESC GLOBALBROWSEWINDOW.cmdCancel.Click()
ENDPROC
PROCEDURE clearkeys
ON KEY
ON KEY LABEL LEFTMOUSE ;
GLOBALBROWSEWINDOW.SETBROWSE(MROW(GLOBALBROWSEWINDOW.BROWSETITLE),;
                                             MCOL(GLOBALBROWSEWINDOW.BROWSETITLE))
ON KEY LABEL RIGHTMOUSE GLOBALBROWSEWINDOW.;
       SHORTCUT(MROW(GLOBALBROWSEWINDOW.BROWSETITLE),;
                MCOL(GLOBALBROWSEWINDOW.BROWSETITLE))
                                             
ENDPROC
PROCEDURE setbrowse
lParameters tnMouseRow,tnMouseCol
IF tnMouseRow<>-1 AND tnMouseCol<>-1
  SHOW WINDOW (THIS.BROWSETITLE) TOP
ENDIF
ENDPROC
PROCEDURE shortcut
lParameters tnMouseRow,tnMouseCol
IF tnMouseRow<>-1 AND tnMouseCol<>-1
  SHOW WINDOW (THIS.BROWSETITLE) TOP
  GLOBALBROWSEWINDOW.RIGHTCLICK()
ENDIF
ENDPROC
PROCEDURE buildfilefields
Local lnCount
DIMENSION THIS.aFileFields[oBrowse.ColumnCount,4]
FOR lnCount = 1 TO ALEN(THIS.aFileFields,1)
  THIS.aFileFields[lnCount,1] = oBrowse.Columns[lnCount].Header1.Caption
  THIS.aFileFields[lnCount,2] = oBrowse.Columns[lnCount].ControlSource
  THIS.aFileFields[lnCount,3] = TYPE(EVAL('THIS.aFileFields[lnCount,2]'))
  IF  ATC('(','THIS.aFileFields[lnCount,3]')>0 OR ATC('+','THIS.aFileFields[lnCount,3]')>0
    THIS.aFileFields[lnCount,4] = 'E'
  ELSE
    THIS.aFileFields[lnCount,4] = 'F'  
  ENDIF  
ENDFOR
ENDPROC
PROCEDURE runmethod
lParameters toReference,tcMethod
Local lcCommand
lcCommand = IIF(TYPE('toReference')='O','toReference','')
lcCommand = lcCommand+IIF(EMPTY(lcCommand),'='+tcMethod,'.'+tcMethod)
IF !EMPTY(lcCommand)
  &lcCommand
ENDIF

ENDPROC
PROCEDURE addselect
IF THIS.MultiSelect
   IF SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
     DELETE IN (THIS.MultiSelectFile)
   ELSE
     APPEND BLANK IN (THIS.MultiSelectFile)
     lcFile = (THIS.MultiSelectFile)
     REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
   ENDIF
   THIS.lastSelection = EVAL(THIS.SelectField)
   oBrowse.Refresh()
ELSE
  ThisForm.cmdSelect.CLICK  &&SetFocus
*  KEYBOARD "{SPACEBAR}"
ENDIF   

ENDPROC
PROCEDURE addregon
lParameters lnRecNo
GO lnRecNo
lcValue = EVAL(THIS.SelectField)
IF lcValue < THIS.LastSelection
  lcLastValue= THIS.LastSelection
  THIS.LastSelection = lcValue
  lcValue = lcLastValue
ENDIF
lcLastValue = EVAL(THIS.SelectField)
SELECT (THIS.BrowseFile)
lcFilter = FILTER()
IF !EMPTY(THIS.ForExp)
  lcFilter = THIS.ForExp + IIF(!EMPTY(lcFilter),' .AND. '+ lcFilter,'')
ELSE  
  lcFilter = IIF(!EMPTY(lcFilter),' FOR '+ lcFilter,'')        
ENDIF
lcKey = ""
IF !EMPTY(THIS.KEY)
  =SEEK(THIS.KEY)
  lcKey = "REST WHILE "+KEY()+"="+THIS.KEY
ENDIF
SCAN &lcKey &lcFilter
  IF !SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
    IF BETWEEN(EVAL(THIS.SelectField),THIS.LastSelection,lcValue)
      APPEND BLANK IN (THIS.MultiSelectFile)
      lcFile = (THIS.MultiSelectFile)
      REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
    ENDIF  
  ENDIF
ENDSCAN
THIS.LastSelection = lcLastValue
GO lnRecNo
oBrowse.Refresh()
ENDPROC
PROCEDURE Deactivate
IF WONTOP(THISFORM.BrowseTitle)
  THISFORM.trapKeys()
ELSE
  ON KEY  
ENDIF  
ENDPROC
PROCEDURE Init
lParameters tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect,;
            toSelectObj,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField

THIS.NAME = SYS(2015)     
THIS.BrowseFile = SELECT()       
PUBLIC GLOBALBROWSEWINDOW
GLOBALBROWSEWINDOW = THIS
THIS.nRecordNo=RECNO()
THIS.FILEFILTER = FILTER()
THIS.ORDER = ORDER()
IF TYPE('tcOptions')='C'
  THIS.DefaultShortCut = PADR(UPPER(tcOptions),4,'T')
ENDIF  
IF TYPE('tcTempFile')='C' 
  THIS.multiselect = .T.
  THIS.MultiSelectFile = tcTempFile
ENDIF
IF THIS.multiselect AND TYPE('tcSelField')#'C'
  tcSelField = KEY()
ENDIF
this.SelectField = tcSelField
IF This.MultiSelect AND !USED(tcTempFile)
  CREATE TABLE (oAriaApplication.WorkDir+tcTempFile) (KeyExp C(LEN(EVAL(this.SelectField))))
  INDEX ON KEYEXP TAG KEYEXP
ENDIF
IF This.MultiSelect
  =CURSORSETPROP('Buffering' , 5 ,tcTempFile)
ENDIF  
this.SelectButton = tlSelect
THIS.SelectButtonReference = toSelectObj
THIS.SelectButtonMethod = tcSelectMethod
IF TYPE('tcUserShortCut')='C'
  DIMEN THIS.USERSHORTCUT[ALEN(&tcUserShortCut,1),ALEN(&tcUserShortCut,2)]
  =ACOPY(&tcUserShortCut,THIS.USERSHORTCUT)
ENDIF
THIS.cmdSelect.Visible = THIS.SelectButton
LOCAL lcWinName
lcWinName = SYS(2015)
THIS.WindName = lcWinName
DEFINE WINDOW (lcWinName) FROM -1.5,-1.5 to 21,40 FONT "MS Sans Serif",8,"N" NOCLOSE NOFLOAT NOMIN in window (this.name)
THIS.BrowseTitle = IIF(TYPE('tcBrowseTitle')='C' AND !EMPTY(tcBrowseTitle),tcBrowseTitle,ALIAS())
lcKey = IIF(TYPE('tckey')<>'C','','KEY '+tcKey)
THIS.KEY = tcKey
lcFor = ''
This.Caption = This.BrowseTitle
IF TYPE('tcFor')='C' AND !EMPTY(tcFor)
  tcFor = ALLTRIM(tcFor)
  lcFor = IIF(LEFT(UPPER(tcFor),4)='FOR ',tcFor,'FOR '+tcFor)
ENDIF  
THIS.forExp = lcFor
SELECT (THIS.BrowseFile)



*-- YMA
IF !EMPTY(lcKey)
  IF !SEEK(&tcKey)
    = MessageBox ("No records to display.", 16, _SCREEN.Caption)
    RETURN .F.
  ENDIF
ENDIF

IF !EMPTY(lcFor)
  LOCATE &lcFor
  IF !FOUND()
    = MessageBox ("No records to display.", 16, _SCREEN.Caption)
    RETURN .F.
  ENDIF
ENDIF

*-- YMA


SELECT (THIS.BrowseFile)
IF !EMPTY(tcBrowseFields)
  BROWSE FIELDS &tcBrowseFields ;
   WINDOW (lcWinName) IN WINDOW (this.name) ;
   NOWAIT SAVE NODELE NOAPP NOMODI ;
   NAME oBrowse title THIS.BrowseTitle ;
   &lcKey &lcFor
ELSE
  BROWSE WINDOW (lcWinName) IN WINDOW (this.name) ;
   NOWAIT SAVE NODELE NOAPP NOMODI ;
   NAME oBrowse title THIS.BrowseTitle ;
   &lcKey &lcFor
ENDIF   

FOR lnCount = 1 TO oBrowse.COLUMNCOUNT
   oBrowse.COLUMNS(lnCount).Visible = .T.
   oBrowse.COLUMNS(lnCount).AddObject('TEXT2','GRIDCOLUMN')
   oBrowse.COLUMNS(lnCount).TEXT2.visible = .T.
   oBrowse.COLUMNS(lnCount).CURRENTCONTROL ='TEXT2'
ENDFOR
IF This.MultiSelect AND USED(tcTempFile)
     oBrowse.COLUMNCOUNT = oBrowse.COLUMNCOUNT + 1
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).AddObject('CHECK1','MultiSelectionColumn')
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).CHECK1.visible = .T.
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).CURRENTCONTROL ='CHECK1'      
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).Visible = .T.
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).CHECK1.Caption = ""
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).CHECK1.VISIBLE = .T.
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).ControlSource = [SEEK(]+ALIAS(THIS.BrowseFile)+'.'+THIS.SelectField+',"'+This.MultiSelectFile+'")' 
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).Width = 15     
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).sparse = .f.
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).Bound = .T.
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).Header1.Caption = ""
     oBrowse.COLUMNS(oBrowse.COLUMNCOUNT).ColumnOrder = 1
     this.cmdselect.Caption = "\<Ok"
ENDIF
oAriaApplication.oToolBar.SetALL('ENABLED',.F.)
PUSH KEY
ON KEY
THIS.RESIZE
ENDPROC
PROCEDURE Activate
IF !EMPTY(THIS.KeywasPress)
  lcPush =this.KeywasPress
  this.KeywasPress = ''
  THIS.&lcPush..Click()
ENDIF
IF !EMPTY(THIS.KeyWasSelect)
  lcPush = THIS.KeyWasSelect
  THIS.KeyWasSelect = ''
  this.KeywasPress = ''
*  IF UPPER(THIS.ActiveControl.Name) <> UPPER(lcPush)
*    KEYBOARD "{TAB}"
*  ENDIF
    SHOW WINDOW (THISFORM.browseTitle) 
    ACTIVATE WINDOW (THIS.NAME)
ENDIF
IF THISFORM.FirstTime
  THISFORM.FirstTime = .F.
  SHOW WINDOW (THISFORM.browseTitle) TOP
ELSE
  THISFORM.CLEARKEYS()  
ENDIF

ENDPROC
PROCEDURE Resize
this.COMMAND1.top = 0
this.COMMAND1.LEFT = 0
this.cmdSelect.top = THIS.HEIGHT- (this.cmdSelect.Height+10)
this.cmdCancel.top = THIS.HEIGHT- (this.cmdCancel.Height+10)
LOCAL lnSepButtons
IF THIS.cmdSelect.Visible 
  lnSepButtons = (THIS.WIDTH-(THIS.cmdSelect.Width*2))/3
  this.cmdSelect.LEFT = lnSepButtons
  this.cmdCancel.LEFT =  THIS.cmdSelect.Width + (lnSepButtons * 2)
ELSE
  lnSepButtons = (THIS.WIDTH-(THIS.cmdSelect.Width))/2
  this.cmdCancel.LEFT = lnSepButtons  
ENDIF  
oBrowse.LEFT=-5
oBrowse.TOP=-20
*oBrowse.HEIGHT = THIS.HEIGHT-(abs(oBrowse.top)+this.cmdSelect.Height)
oBrowse.HEIGHT = this.cmdSelect.top + 10
oBrowse.WIDTH = THIS.WIDTH+5
*IF TYPE('_SCREEN.ACTIVEFORM')<>'O'
  SHOW WINDOW (THISFORM.BrowseTitle) TOP
*ENDIF
ENDPROC
PROCEDURE Unload
*B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu [Begin]
oAriaApplication.ReBldCmp = .T.
oAriaApplication.SetMenu(oAriaApplication.ActiveModuleID,'A')
*B128962,1 WLD ReBuildComp  All companies ,there is a bug in fox not display more than 40 companies after disabling the menu [End]
POP KEY
SELECT (THIS.BrowseFile)
LOCAL lcFilter, lcOrder
RELEASE GLOBALBROWSEWINDOW
lcFilter = this.FileFilter
lcOrder  = THIS.ORDER
SET FILTER TO &lcFilter
SET ORDER TO &lcOrder
RETURN THIS.uReturn

ENDPROC
PROCEDURE RightClick
PUSH KEY
ON KEY
Local lnChoice,lcOrder,lcQuikFind,lcFind,lcFilter,lcUserDefBars,lcUserDefAcc,;
      lnCount
lcFind   = SUBSTR(THIS.DefaultShortCut,1,1)
lcFilter = SUBSTR(THIS.DefaultShortCut,2,1)
lcOrder  = IIF(!EMPTY(TAG(1)) AND EMPTY(THIS.KEY),'T','F')
lcOrder  = IIF(lcOrder = 'T' AND RIGHT(THIS.DefaultShortCut,1)='T','T','F')
lcQuikFind = IIF(!EMPTY(TAG(1)),'T','F')
lcQuikFind = IIF(lcQuikFind = 'T' AND SUBSTR(THIS.DefaultShortCut,3,1)='T','T','F')
STORE '' TO lcUserDefBars,lcUserDefAcc,lcMutiSel,lcMultiDefAcc
IF THIS.MultiSelect
  lcMutiSel = ",\-,"+IIF(SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile),"\<Unselect","\<Select")+",Select \<None,Select \<All,\<Invert,\<Range"
  lcMultiDefAcc = "TTTTTT"
ENDIF
IF !EMPTY(THIS.USERSHORTCUT[1,1])
  lcUserDefBars = ',\-'
  lcUserDefAcc =  'T'
  FOR lnCount = 1 TO ALEN(THIS.USERSHORTCUT,1)
    lcUserDefBars = lcUserDefBars +','+ THIS.USERSHORTCUT[lnCount,1]
    lcUserDefAcc = lcUserDefAcc + THIS.USERSHORTCUT[lnCount,4]
  ENDFOR
ENDIF
lnChoice = ThisForm.BrowseShortCut.ShowShortCut(This,"\<Find,Fi\<lter,\<Quik Find,\<Order"+lcMutiSel+lcUserDefBars,lcFind+lcFilter+lcQuikFind+lcOrder+lcMultiDefAcc+lcUserDefAcc)
IF lnChoice = 1 OR lnChoice = 2
  IF EMPTY(THIS.aFileFields[1,1])
    THIS.BuildFileFields()
  ENDIF
ENDIF
DO CASE
  CASE lnChoice = 1                 && Find
    Local oFilter,lcKey
    PRIVATE laFileFields
    IF !EMPTY(THIS.KEY)
      lcKey = IIF(ATC(LEFT(THIS.Key,1)+',',THIS.Key)>0,SUBSTR(THIS.Key,1,ATC(LEFT(THIS.Key,1)+',',THIS.Key)),THIS.Key)
    ELSE
      lcKey = ['']
    ENDIF  
    =ACOPY(THIS.aFileFields,laFileFields)  
    oFilter = CREATEOBJECT('ARIASEARCHFORM')
    oFilter.Key = lcKey
    oFilter.ariaSearchContainer1.InitializeFields(@laFileFields)
    oFilter.SHOW()
  CASE lnChoice = 2              && FIlter
    Local oFilter
    PRIVATE laFilter,laFileFields
    =ACOPY(THIS.aFileFields,laFileFields)
    =ACOPY(THIS.aFilter,laFilter)    
    oFilter = CREATEOBJECT('ARIASEARCHFORM','laFilter',THIS.FILEFILTER)
    oFilter.ariaSearchContainer1.InitializeFields(@laFileFields,@laFilter)
    oFilter.SHOW()
    DIMEN this.aFilter[ALEN(laFilter,1),7]
    =ACOPY(laFilter,THIS.aFilter)
     oBrowse.Refresh()   
  CASE lnChoice = 3             && Quik Find
    LOCAL lcKey
    IF !EMPTY(THIS.KEY)
      lcKey = IIF(ATC(LEFT(THIS.Key,1)+',',THIS.Key)>0,SUBSTR(THIS.Key,1,ATC(LEFT(THIS.Key,1)+',',THIS.Key)),THIS.Key)
    ELSE
      lcKey = ['']
    ENDIF  
    oFilter = CREATEOBJECT('ARIASEEK',ALIAS(),oBrowse,.t.,THIS.ORDER,lcKey)
    oFilter.SHOW()    
  CASE lnChoice = 4             && Order
    oFilter = CREATEOBJECT('ARIASEEK',ALIAS(),oBrowse,.f.)
    oFilter.SHOW() 
  CASE (lnChoice > 5 AND !THIS.MultiSelect) OR (THIS.MultiSelect AND lnChoice>10)
    THIS.RUNMETHOD(THIS.USERSHORTCUT[lnChoice-IIF(THIS.MultiSelect,10,5),2],THIS.USERSHORTCUT[lnChoice-IIF(THIS.MultiSelect,10,5),3])
  CASE THIS.MultiSelect  && Case MultiSelect
    DO CASE
      CASE lnChoice = 6  && Select
        THIS.AddSelect
      CASE lnChoice = 7   && Select None
        DELETE ALL IN (THIS.MultiSelectFile)   
      CASE lnChoice = 8   && Select All   
        SELECT (THIS.BrowseFile)
        lcFilter = FILTER()
        IF !EMPTY(THIS.ForExp)
          lcFilter = THIS.ForExp + IIF(!EMPTY(lcFilter),' .AND. '+ lcFilter,'')
        ELSE  
          lcFilter = IIF(!EMPTY(lcFilter),' FOR '+ lcFilter,'')        
        ENDIF
        lcKey = ""
        IF !EMPTY(THIS.KEY)
          =SEEK(EVALUATE(THIS.KEY))
          lcKey = "REST WHILE "+KEY()+"="+THIS.KEY
        ENDIF
        SCAN &lcKey &lcFilter
          IF !SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
            APPEND BLANK IN (THIS.MultiSelectFile)
            lcFile = (THIS.MultiSelectFile)
            REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
          ENDIF
        ENDSCAN
      CASE lnChoice = 9   && Invert
        SELECT (THIS.BrowseFile)
        lcFilter = FILTER()
        IF !EMPTY(THIS.ForExp)
          lcFilter = THIS.ForExp + IIF(!EMPTY(lcFilter),' .AND. '+ lcFilter,'')
        ELSE  
          lcFilter = IIF(!EMPTY(lcFilter),' FOR '+ lcFilter,'')        
        ENDIF
        lcKey = ""
        IF !EMPTY(THIS.KEY)
          =SEEK(EVALUATE(THIS.KEY))
          lcKey = "REST WHILE "+KEY()+"="+THIS.KEY
        ENDIF
        SCAN &lcKey &lcFilter
          IF !SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
            APPEND BLANK IN (THIS.MultiSelectFile)
            lcFile = (THIS.MultiSelectFile)
            REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
          ELSE
             DELETE IN (THIS.MultiSelectFile)  
          ENDIF
        ENDSCAN
      CASE lnChoice = 10   && Range
        lcReturn = ''
        DO FORM (oAriaApplication.ScreenHome + "\SY\BROWRNG") WITH oBrowse TO lcReturn
        lcLowRange  = LEFT(lcReturn,AT(',',lcReturn,1)-1)
        lcHighRange = SUBSTR(lcReturn,AT(',',lcReturn,1)+1,AT(',',lcReturn,2)-AT(',',lcReturn,1)-1)
        lcSelection = SUBSTR(lcReturn,AT(',',lcReturn,2)+1,AT(',',lcReturn,3)-AT(',',lcReturn,2)-1)
        lcField  = SUBSTR(lcReturn,AT(',',lcReturn,3)+1)
        IF lcSelection <> 'C'
          lcRange = ''
          DO CASE
            CASE EMPTY(lcLowRange)  AND !EMPTY(lcHighRange)
              lcRange = lcField+ ' <= lcHighRange'
            CASE !EMPTY(lcLowRange) AND EMPTY(lcHighRange)
              lcRange = lcField+ ' >= lcLowRange'
            CASE !EMPTY(lcLowRange) AND !EMPTY(lcHighRange)
              lcRange = 'BETWEEN('+lcField+',lcLowRange,lcHighRange)'
          ENDCASE
          SELECT (THIS.BrowseFile)
          lcFilter = FILTER()
          IF !EMPTY(THIS.ForExp)
            lcFilter = THIS.ForExp + IIF(!EMPTY(lcFilter),' .AND. '+ lcFilter,'')
          ELSE  
            lcFilter = IIF(!EMPTY(lcFilter),' FOR '+ lcFilter,'')        
          ENDIF
          IF EMPTY(lcFilter)
            lcFilter = 'FOR ' + lcRange
          ELSE
            lcFilter = lcFilter + ' AND '+ lcRange
          ENDIF
          lcKey = ""
          IF !EMPTY(THIS.KEY)
            =SEEK(EVALUATE(THIS.KEY))
            lcKey = "REST WHILE "+KEY()+"="+THIS.KEY
          ENDIF
          lnFileRecNo = RECNO()
          SCAN &lcKey &lcFilter
            IF lcSelection='S' AND !SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
              APPEND BLANK IN (THIS.MultiSelectFile)
              lcFile = (THIS.MultiSelectFile)
              REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
            ENDIF  
            IF lcSelection='N' AND SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
              DELETE IN (THIS.MultiSelectFile)  
            ENDIF
            IF lcSelection='V'
              IF !SEEK(EVAL(THIS.SelectField),THIS.MultiSelectFile)
                APPEND BLANK IN (THIS.MultiSelectFile)
                lcFile = (THIS.MultiSelectFile)
                REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
              ELSE
                 DELETE IN (THIS.MultiSelectFile)  
              ENDIF
            ENDIF
          ENDSCAN
          IF BETWEEN(lnFileRecNo,1,RECCOUNT())
            GO lnFileRecNo
          ENDIF
        ENDIF  
      ENDCASE
      oBrowse.Refresh()
ENDCASE
POP KEY
ENDPROC
      ����    �   �                         Ё   %   o       �      �           �  U  (  %�a��! � �,)��  � �� B�-�� � U  THISFORM BROWSETITLE GotFocus,     ��1 � � q A 2                       [       )   �                        ����    g  g                        p�   %   �                      �  U    %��  � 
��B �! %��  � � �
 �  � CN	��> � #��  � �� � �a � F��  � ��
 ��Ca��� � F��  � ��
 ��  � � U  THISFORM MULTISELECT	 NRECORDNO MULTISELECTFILE
 BROWSEFILE RELEASE Click,     ��1 � A � � � A � � 1                             )   g                       ����    �  �                        �U   %   I      �     X          �  U  { �� {�  � %��  � 
��� �� � %�C� � � �
���d T� �CC�! THIS.Parent.SelectButtonReferenceb� O�' �! THIS.Parent.SelectButtonReference� �  6��7 T� �� CC� �� � =� � � � � .� � � 6�� %�C� �
��	� IF !&lcCommand�� �� B� � � � �0� F��  � ��
 ��Ca��� � <,��  � �� T�	 ���� <,��  �
 �� �� T�  � �a��
 ��  � � U  THISFORM MULTISELECT	 LCCOMMAND THIS PARENT SELECTBUTTONREFERENCE SELECTBUTTONMETHOD MULTISELECTFILE WINDNAME OBROWSE BROWSETITLE URETURN RELEASE Click,     ��1 Q � q aAqaQ A A A A � � � A � � � R � � 2                       �      )   �                        !Arial, 0, 9, 5, 15, 12, 32, 3, 0
      .Top = 12
Left = 12
Name = "BrowseShortCut"
      Form1      BrowseShortCut      custom      ..\..\..\classes\utility.vcx      ariashortcut      fPROCEDURE GotFocus
IF .T.
  SHOW WINDOW (THISFORM.BrowseTitle) TOP
  RETURN .F.
ENDIF

ENDPROC
      Top = 252
Left = 396
Height = 9
Width = 6
Caption = "Command1"
Style = 1
TabIndex = 3
TabStop = .T.
Name = "Command1"
      Form1      Command1      commandbutton      commandbutton     PROCEDURE Click
IF !THISFORM.MultiSelect
  IF THISFORM.nRecordNo>0 AND THISFORM.nRecordNo<=RECCOUNT()
    GO THISFORM.nRecordNo
  ENDIF
ELSE
  SELECT (THISFORM.MultiSelectFile)  
  =TABLEREVERT(.T.)
ENDIF
SELECT (THISFORM.BrowseFile)
THISFORM.RELEASE
ENDPROC
      tTop = 252
Left = 288
Height = 27
Width = 84
Cancel = .T.
Caption = "Cancel"
TabIndex = 2
Name = "cmdCancel"
      Form1      	cmdCancel      commandbutton      commandbutton     PROCEDURE Click
PUSH KEY
ON KEY
IF !THISFORM.MultiSelect
  Local lcCommand
  IF !EMPTY(THIS.Parent.SelectButtonReference)
    lcCommand = IIF(TYPE('THIS.Parent.SelectButtonReference')='O','THIS.Parent.SelectButtonReference','')
    lcCommand = lcCommand+IIF(EMPTY(lcCommand),'='+THIS.Parent.SelectButtonMethod,'.'+THIS.Parent.SelectButtonMethod)
    IF !EMPTY(lcCommand)
      IF !&lcCommand
        POP KEY
        RETURN
      ENDIF
    ENDIF  
  ENDIF  
ELSE
  SELECT (THISFORM.MultiSelectFile)  
  =TABLEUPDATE(.T.)
ENDIF  
RELEASE WINDOW (THISFORM.WindName)
OBROWSE=.NULL.
RELEASE WINDOW (THISFORM.BrowseTitle)
*SELECT (THISFORM.BrowseFile)
POP KEY
THISFORM.uReturn = .T.
*Release WINDOW (ThisForm.WindName)
THISFORM.RELEASE 

ENDPROC
      �Top = 252
Left = 156
Height = 27
Width = 84
FontName = "Arial"
Caption = "Select"
Default = .T.
TabIndex = 1
ToolTipText = ""
Name = "cmdSelect"
      Form1      	cmdSelect      commandbutton      commandbutton     Vbrowsetitle
firsttime
ureturn
nrecordno
filefilter
order Specifies the controlling index tag for a Cursor object.
key
defaultshortcut
selectbutton
selectbuttonreference
selectbuttonmethod
multiselect Specifies whether a user can make multiple selections in a ListBox control and how the multiple selections can be made.
multiselectfile
selectfield
browsefile
forexp
lastselection
windname
keywaspress
keywasselect
*trapkeys 
*clearkeys 
*setbrowse 
*shortcut 
*buildfilefields 
^afilefields[1,4] 
^afilter[1,7] 
^usershortcut[1,4] 
*runmethod 
*addselect 
*addregon 
     1+���    1  1                        U�   %   �+      �/  �  �,          �  U  �  12� CTRL+Q� LL=.T.� 12� CTRL+W� LL=.T.� 12�	 CTRL+HOME� GO TOP� 12� CTRL+END� GO BOTT�3 12� ESC�$ GLOBALBROWSEWINDOW.cmdCancel.Click()� U  CTRL Q W HOME END ESC {�  �} 12�	 LEFTMOUSE�h GLOBALBROWSEWINDOW.SETBROWSE(MROW(GLOBALBROWSEWINDOW.BROWSETITLE), MCOL(GLOBALBROWSEWINDOW.BROWSETITLE))�~ 12�
 RIGHTMOUSE�h GLOBALBROWSEWINDOW. SHORTCUT(MROW(GLOBALBROWSEWINDOW.BROWSETITLE), MCOL(GLOBALBROWSEWINDOW.BROWSETITLE))� U 	 LEFTMOUSE
 RIGHTMOUSE?  ��  � � %��  ����	 � ���	��8 � �,)�� � �� � U 
 TNMOUSEROW
 TNMOUSECOL THIS BROWSETITLEL  ��  � � %��  ����	 � ���	��E � �,)�� � �� ��C� � �� � U 
 TNMOUSEROW
 TNMOUSECOL THIS BROWSETITLE GLOBALBROWSEWINDOW
 RIGHTCLICKv ��  � � � �� � ����� ��  ���(�C� � ����o�* T� � ��  �����C �  � � � � ��' T� � ��  �����C �  � � � ��; T� � ��  �����CC� THIS.aFileFields[lnCount,2]�b��^ %�C� (� THIS.aFileFields[lnCount,3]�� �) C� +� THIS.aFileFields[lnCount,3]�� ��F� T� � ��  ������ E�� �k� T� � ��  ������ F�� � �� U	  LNCOUNT THIS AFILEFIELDS OBROWSE COLUMNCOUNT COLUMNS HEADER1 CAPTION CONTROLSOURCE�  ��  � � �� �8 T� �CC� toReferenceb� O� � toReference� �  6��+ T� �� CC� �� � =� � � .� 6�� %�C� �
��� � &lcCommand
 � U  TOREFERENCE TCMETHOD	 LCCOMMAND�  %��  � ��� � %�CC�  � ��  � ���8 � ��  � �� �� � ��  � �� T� ��  � ��7 REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
 � T�  � �C�  � ��� ��C� � �� �� � �� �	 �
 � � U  THIS MULTISELECT SELECTFIELD MULTISELECTFILE LCFILE LASTSELECTION OBROWSE REFRESH THISFORM	 CMDSELECT CLICKB ��  �	 #��  �� T� �C� � ��� %�� � � ��c � T� �� � �� T� � �� �� T� �� �� � T� �C� � ��� F�� � �� T� �C��� %�C� � �
��� �0 T� �� � CC� �
� �  .AND. � � �  6�� ��' T� �CC� �
� �  FOR � � �  6�� � T�	 ��  �� %�C� �
 �
��]� ��C� �
 ���' T�	 �� REST WHILE Cm� =� �
 �� � SCAN &lcKey &lcFilter�� %�CC� � �� � �
��� %�CC� � �� � � ���� �� � �� T� �� � ��7 REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
 � � � T� � �� ��	 #��  �� ��C� � �� U  LNRECNO LCVALUE THIS SELECTFIELD LASTSELECTION LCLASTVALUE
 BROWSEFILE LCFILTER FOREXP LCKEY KEY MULTISELECTFILE LCFILE OBROWSE REFRESH6  %�C�  � ��� � ��C�  � �� �/ � {�  � � U  THISFORM BROWSETITLE TRAPKEYS.	/ ��  � � � � � � � � �	 �
 � T� � �C��]�� T� � �CW�� 7� � T� �� �� T� � �CO�� T� � �C��� T� � �C��� %�C�	 tcOptionsb� C��� � T� � �CC� f�� T��� � %�C�
 tcTempFileb� C��
� T� � �a�� T� � ��	 �� �( %�� � � C�
 tcSelFieldb� C	��B� T�
 �Cm�� � T� � ��
 �� %�� � � C�	 �
	����) h1�� � �	 �� � C�CC� � �>�� & �� ��� � � %�� � ���� ��C�	 Buffering��	 ��� � T� � �� �� T� � �� �� T� � �� ��" %�C� tcUserShortCutb� C����L DIMEN THIS.USERSHORTCUT[ALEN(&tcUserShortCut,1),ALEN(&tcUserShortCut,2)]
- =ACOPY(&tcUserShortCut,THIS.USERSHORTCUT)
 � T� � � �� � �� �� � T� �C��]�� T� � �� ��P s,�� �� ���� ���(�����(�@�� MS Sans Serif�������� � ���� N��; T� �  �CC� tcBrowseTitleb� C� C� �
	� � � C6��/ T�! �CC� tckeyb� C� �  � � KEY � 6�� T� �" �� �� T�# ��  �� T� �$ �� �  ��# %�C� tcForb� C� C� �
	��4� T� �C� ���2 T�# �CCC� f�=� FOR � � � � FOR � 6�� � T� �% ��# �� F�� � �� %�C�! �
���� IF !SEEK(&tcKey)���* ��C� No records to display.��9�$ �x�� B�-�� � � %�C�# �
��� LOCATE &lcFor
 %�C4
���* ��C� No records to display.��9�$ �x�� B�-�� � � F�� � �� %�C�  �
����� BROWSE FIELDS &tcBrowseFields  WINDOW (lcWinName) IN WINDOW (this.name)  NOWAIT SAVE NODELE NOAPP NOMODI  NAME oBrowse title THIS.BrowseTitle  &lcKey &lcFor
 �j�� BROWSE WINDOW (lcWinName) IN WINDOW (this.name)  NOWAIT SAVE NODELE NOAPP NOMODI  NAME oBrowse title THIS.BrowseTitle  &lcKey &lcFor
 � ��& ���(��' �( ��� T�' �) ��& �� �a��. ��' �) ��& ��* �� TEXT2���
 GRIDCOLUMN�� T�' �) ��& ��+ � �a�� T�' �) ��& ��, �� TEXT2�� �� %�� � � C�	 �	���� T�' �( ��' �( ���< ��' �) ��' �( ��* �� CHECK1��� MultiSelectionColumn�� T�' �) ��' �( ��- � �a��" T�' �) ��' �( ��, �� CHECK1�� T�' �) ��' �( �� �a�� T�' �) ��' �( ��- �$ ��  �� T�' �) ��' �( ��- � �a��I T�' �) ��' �( ��. �� SEEK(C� � � .� � � ,"� � � ")�� T�' �) ��' �( ��/ ���� T�' �) ��' �( ��0 �-�� T�' �) ��' �( ��1 �a�� T�' �) ��' �( ��2 �$ ��  �� T�' �) ��' �( ��3 ���� T� � �$ �� \<Ok�� � ��C� ENABLED-� �4 �5 �� �� {�  �
 �� �6 � U7  TCBROWSEFIELDS TCBROWSETITLE TCKEY TCFOR	 TCOPTIONS TLSELECT TOSELECTOBJ TCSELECTMETHOD TCUSERSHORTCUT
 TCTEMPFILE
 TCSELFIELD THIS NAME
 BROWSEFILE GLOBALBROWSEWINDOW	 NRECORDNO
 FILEFILTER ORDER DEFAULTSHORTCUT MULTISELECT MULTISELECTFILE SELECTFIELD OARIAAPPLICATION WORKDIR KEYEXP SELECTBUTTON SELECTBUTTONREFERENCE SELECTBUTTONMETHOD	 CMDSELECT VISIBLE	 LCWINNAME WINDNAME BROWSETITLE LCKEY KEY LCFOR CAPTION FOREXP LNCOUNT OBROWSE COLUMNCOUNT COLUMNS	 ADDOBJECT TEXT2 CURRENTCONTROL CHECK1 CONTROLSOURCE WIDTH SPARSE BOUND HEADER1 COLUMNORDER OTOOLBAR SETALL RESIZE�  %�C�  � �
��L � T� ��  � �� T�  � ��  �� THIS.&lcPush..Click()
 � %�C�  � �
��� � T� ��  � �� T�  � ��  �� T�  � ��  �� �,�� � �� t,��  � �� � %�� � ��� � T� � �-�� �,)�� � �� �� � ��C� � �� � U	  THIS KEYWASPRESS LCPUSH KEYWASSELECT THISFORM BROWSETITLE NAME	 FIRSTTIME	 CLEARKEYS� T�  � � �� �� T�  � � �� ��% T�  � � ��  � �  � � �
��% T�  � � ��  � �  � � �
�� �� � %��  � � ��� �$ T� ��  �	 �  � �	 ���� T�  � � �� ��" T�  � � ��  � �	 � ��� ��  T� ��  �	 �  � �	 ��� T�  � � �� �� � T�
 � ������ T�
 � ������ T�
 � ��  � � �
�� T�
 �	 ��  �	 ��� �,)�� � �� U  THIS COMMAND1 TOP LEFT	 CMDSELECT HEIGHT	 CMDCANCEL LNSEPBUTTONS VISIBLE WIDTH OBROWSE THISFORM BROWSETITLE�  T�  � �a�� ��C�  � � A�  � �� �� F�� � �� �� � � <� � T� �� �	 �� T� �� �
 �� SET FILTER TO &lcFilter
 SET ORDER TO &lcOrder
 B�� � �� U  OARIAAPPLICATION REBLDCMP SETMENU ACTIVEMODULEID THIS
 BROWSEFILE LCFILTER LCORDER GLOBALBROWSEWINDOW
 FILEFILTER ORDER URETURN �� {�  �# ��  � � � � � � � � T� �C� �	 ��\�� T� �C� �	 ��\��. T� �CCC���
�	 C� �
 �	� � T� � F6��6 T� �C� � T� C� �	 �R� T	� � T� � F6��" T� �CCC���
� � T� � F6��9 T� �C� � T� C� �	 ��\� T	� � T� � F6�� J��  �(� � � � � %�� � ����r T� �� ,\-,CCC� � �� � �� �
 \<Unselect� � \<Select6�, ,Select \<None,Select \<All,\<Invert,\<Range�� T� �� TTTTTT�� � %�CC��� � �
��i� T� �� ,\-�� T� �� T�� �� ���(�C� � ����e�! T� �� � ,C � �� � �� T� �� C � �� � �� �� �] T�  �C � �# \<Find,Fi\<lter,\<Quik Find,\<Order� � � � � � � � � � � �� %��  �� �  ���� %�CC��� � ���� ��C� � �� � � H�!�	� ��  ���,� �� � � 5� � %�C� �
 �
����W T� �CCC� �
 �=� ,� �
 �� �& C� �
 �CC� �
 �=� ,� �
 �\� � �
 6�� ��� T� �� ''�� � ��C� � �� ��� T� �C� ARIASEARCHFORM�N�� T� �
 �� �� ��C� � � � �� ��C� � �� ��  ���� �� � 5� � � ��C� � �� ��� ��C� � �� ���/ T� �C� ARIASEARCHFORM� laFilter� � �N�� ��C� � � � � �� ��C� � �� � � �C�� ������� ��C�� � � ��� ��C� �  �� ��  ����� �� � %�C� �
 �
����W T� �CCC� �
 �=� ,� �
 �� �& C� �
 �CC� �
 �=� ,� �
 �\� � �
 6�� ��� T� �� ''�� �) T� �C� ARIASEEKC � a� �!  � �N�� ��C� � �� ��  ���� T� �C� ARIASEEKC � -�N�� ��C� � ��3 ��  �� � � 
	� � � � �  �
	����Q ��CC�  C� � � �
� �6�� � C�  C� � � �
� �6�� � � �" �� �� � ��	� H����� ��  �����
 �� �# � ��  ����� �� � �� ��  ����� F�� �$ �� T� �C��� %�C� �% �
��`�0 T� �� �% CC� �
� �  .AND. � � �  6�� ���' T� �CC� �
� �  FOR � � �  6�� � T� ��  �� %�C� �
 �
���� ��CC� �
 Ί��' T� �� REST WHILE Cm� =� �
 �� � SCAN &lcKey &lcFilter��� %�CC� � �� � �
��}� �� � �� T�& �� � ��7 REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
 � � ��  �	��-
� F�� �$ �� T� �C��� %�C� �% �
����0 T� �� �% CC� �
� �  .AND. � � �  6�� �!	�' T� �CC� �
� �  FOR � � �  6�� � T� ��  �� %�C� �
 �
��|	� ��CC� �
 Ί��' T� �� REST WHILE Cm� =� �
 �� � SCAN &lcKey &lcFilter�)
� %�CC� � �� � �
��
� �� � �� T�& �� � ��7 REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
 �%
� �� � �� � � ��  �
���� T�' ��  ��% ��( �) � \SY\BROWRNG�(�' �� � T�* �C�' C� ,�' ��=��< T�+ �C�' C� ,�' ��C� ,�' �C� ,�' ��\��< T�, �C�' C� ,�' ��C� ,�' �C� ,�' ��\�� T�- �C�' C� ,�' ��\�� %��, � C���� T�. ��  �� H�R�� �C�* �� C�+ �
	����  T�. ��- �  <= lcHighRange�� �C�* �
� C�+ �	���� T�. ��- �  >= lcLowRange�� �C�* �
� C�+ �
	���5 T�. �� BETWEEN(�- � ,lcLowRange,lcHighRange)�� � F�� �$ �� T� �C��� %�C� �% �
��r�0 T� �� �% CC� �
� �  .AND. � � �  6�� ���' T� �CC� �
� �  FOR � � �  6�� � %�C� ����� T� �� FOR �. �� ��� T� �� �  AND �. �� � T� ��  �� %�C� �
 �
��F� ��CC� �
 Ί��' T� �� REST WHILE Cm� =� �
 �� � T�/ �CO�� SCAN &lcKey &lcFilter���' %��, � S� CC� � �� � �
	���� �� � �� T�& �� � ��7 REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
 �& %��, � N� CC� � �� � �	��)� �� � �� � %��, � V���� %�CC� � �� � �
���� �� � �� T�& �� � ��7 REPLACE &lcFile..KEYEXP WITH EVAL(THIS.SelectField)
 ��� �� � �� � � � %�C�/ �CN�����	 #��/ �� � � � ��C� �  �� � �� U0  LNCHOICE LCORDER
 LCQUIKFIND LCFIND LCFILTER LCUSERDEFBARS LCUSERDEFACC LNCOUNT THIS DEFAULTSHORTCUT KEY	 LCMUTISEL LCMULTIDEFACC MULTISELECT SELECTFIELD MULTISELECTFILE USERSHORTCUT THISFORM BROWSESHORTCUT SHOWSHORTCUT AFILEFIELDS BUILDFILEFIELDS OFILTER LCKEY LAFILEFIELDS ARIASEARCHCONTAINER1 INITIALIZEFIELDS SHOW LAFILTER AFILTER
 FILEFILTER OBROWSE REFRESH ORDER	 RUNMETHOD	 ADDSELECT
 BROWSEFILE FOREXP LCFILE LCRETURN OARIAAPPLICATION
 SCREENHOME
 LCLOWRANGE LCHIGHRANGE LCSELECTION LCFIELD LCRANGE LNFILERECNO trapkeys,     ��	 clearkeys�     ��	 setbrowse	    �� shortcutw    �� buildfilefields    ��	 runmethod�    ��	 addselect�    �� addregon�    ��
 Deactivate�	    �� Init2
    �� Activate�    �� Resize*    �� Unload,    ��
 RightClickf    ��1 ����12 � ��3 � �� A 2 � �� � A 2 q ���q���� �A A 2 � q ��� A 3 �� � � qA Q� � � B 3 q � !A� A !� � 1� qA � 1� qA ���� qA A A � � 2 !� � � A 2 �2� q � � � � ��A �� A �� A ��� A �A !��A aq ��� 11� !A � ��q A A � �q A A � 
� �A �q���A �q��!���������qA �Q � � 2 1�A 1� � A � � � � A 3 11QQq 1A1!� 1A �q� 3 � qR � � q ��� 3 Q � 2���a!��!1A �� ��A A ���� A A � � q 1q� � A A�A� q � AA��� �1� q 1q� � A �� �� 1� � � � � 1� qA � 1qA ��� qA A � � 1� qA � 1qA ��� q� � A A � Q����!� � ����QA � � 1� qA � Q� �A � 1qA � �q� qA a� A !�� q� � A A A A� A A A � A Q 1                       �         �   e        �  �          �        �  �     #     �  -   +     �  7   9   �  �  H   [   �  �  k   a       r   �   &  4  �   �   Q  F!  �   �   c!  �#    �   �#  z@  $   )   1                  0   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _0RZ0SM2O4 759065366      /  F      ]                                �                       WINDOWS _0RZ0SM2O5 760174967�      �  �      �                                                           WINDOWS _0RZ0SM2OH 759329445�      �  �  �  �      ?  $                                               WINDOWS _0RZ0SM2OI 759329445Y      n  �  �  �      :  =                                               WINDOWS _0RZ0SM2OJ 759329445i      ~  �  �  �      F	  V                                               WINDOWS _0RZ0SQVNP 759319961s
      �
  �
  �
  �
                                                           WINDOWS _0S011Q24Q 759329445  :  G  T  f  w      #  O                                               WINDOWS _0RZ0SM2O4 759329445;  �  {  �  �  �      C  �                                               WINDOWS _0S011Q24R 759329445[  :  �  �  �  �        a                                               WINDOWS _0S011Q24O 760174967)  k  i  v  �  �      7  �	                                               WINDOWS _0S011Q24S 759329445X  L  f
  j  )  X        �                                               WINDOWS _0S011Q24P 759329445  )  \  �  H  u      
                                                 COMMENT RESERVED                                �                                                            x                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      Name = "Dataenvironment"
      1      1      form      form      	frmLicens      �Height = 175
Width = 385
DoCreate = .T.
AutoCenter = .T.
Caption = "Aria Advantage Series"
ControlBox = .F.
MaxButton = .T.
WindowType = 1
WindowState = 0
ZoomBox = .F.
Name = "frmLicens"
      commandbutton      commandbutton      	cmdGetDir      	frmLicens      bTop = 108
Left = 26
Height = 20
Width = 21
Caption = "..."
TabIndex = 5
Name = "cmdGetDir"
      �PROCEDURE Click
PRIVATE lcOldPath 
lcOldPath = FULLPATH('')
lcNewFile  = GETFILE('BIN', 'Select license file .BIN:', 'Select')
CD (lcOldPath)
ThisForm.txtPath.Value = lcNewFile
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         !Arial, 0, 9, 5, 15, 12, 32, 3, 0
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      	frmLicens      commandbutton      commandbutton      cmdAbort      	frmLicens      �Top = 144
Left = 72
Height = 24
Width = 85
FontBold = .F.
FontSize = 9
Caption = "\<Abort"
Style = 0
TabIndex = 7
Name = "cmdAbort"
      @PROCEDURE Click
llProcess = .F.
ThisForm.Release 

ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         label      commandbutton      commandbutton      
cmdProceed      	frmLicens      �Top = 144
Left = 216
Height = 24
Width = 85
FontBold = .F.
FontSize = 9
Caption = "\<Proceed"
TabIndex = 8
Name = "cmdProceed"
      >PROCEDURE Click
llProcess = .T.
ThisForm.Release 
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         label      textbox      textbox      txtPath      	frmLicens      _Enabled = .F.
Height = 20
Left = 48
TabIndex = 6
Top = 108
Width = 298
Name = "txtPath"
      	arialabel      ..\..\classes\main.vcx      label      
Arialabel9      	frmLicens      �AutoSize = .F.
BackStyle = 0
Caption = "This program is protected by US and"
Height = 25
Left = 91
Top = 39
Width = 203
ZOrderSet = 10
Name = "Arialabel9"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\classes\main.vcx      	lblarialc      label      Arialabel10      	frmLicens      �BackStyle = 0
Caption = "international copyright laws."
Height = 15
Left = 91
Top = 54
Width = 132
ZOrderSet = 11
Name = "Arialabel10"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         arialine      ..\..\classes\main.vcx      line      	Arialine1      	frmLicens      RHeight = 0
Left = 71
Top = 72
Width = 208
ZOrderSet = 16
Name = "Arialine1"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         k�   %   V       p      j           �  U    ��C�  � �� U  THISFORM
 RIGHTCLICK
 RightClick,     ��1 � 1                       +       )   �                         	arialabel      ..\..\classes\main.vcx      	arialabel      label      
Arialabel7      	frmLicens      �BackStyle = 0
Caption = "Copyright� 1990-2002 ARIA Systems, Inc."
Height = 15
Left = 91
Top = 9
Width = 203
ZOrderSet = 8
Name = "Arialabel7"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      kBackStyle = 0
Caption = "All rights reserved."
Left = 148
Top = 24
ZOrderSet = 9
Name = "Arialabel8"
      
Arialabel8      ..\..\classes\main.vcx      	arialabel      	frmLicens      ..\..\classes\main.vcx      �FontBold = .T.
BackStyle = 0
Caption = "Invalid Activation Key. Please select the License file :"
Height = 15
Left = 37
Top = 84
Width = 310
ZOrderSet = 11
Name = "lblarialc"
      6PROCEDURE RightClick
ThisForm.RightClick()
ENDPROC
      ����    �   �                         ��   %   f       }      u           �  U    T�  �a��
 �� � � U 	 LLPROCESS THISFORM RELEASE Click,     ��1 � � 1                       3       )   �                         ����    �   �                         ��   %   f       }      u           �  U    T�  �-��
 �� � � U 	 LLPROCESS THISFORM RELEASE Click,     ��1 � � 2                       5       )   �                        L���    3  3                        9   %   �       �      �           �  U  m  5�  � T�  �C�  ���7 T� �C� BIN� Select license file .BIN:� Select���
 ���  �� T� � � �� �� U 	 LCOLDPATH	 LCNEWFILE THISFORM TXTPATH VALUE Click,     ��1 q � q� 11                       �       )   3                  0   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Screen                                                                                              WINDOWS _1BQ0ZFNQU 824609250      /  F      ]                                �                       WINDOWS _1BQ0ZFNQV 824662033�  �  �  �      �      :                    �                           WINDOWS _1BQ0ZFNR4 824659413"  3  U  b  w  �                                                           WINDOWS _1BQ0ZFNR6 824659413	  -	  O	  \	  n	  �	                                                           WINDOWS _1BQ0ZFNR7 824659413�	  
  *
  7
  I
  f
                                                           WINDOWS _1BQ0ZFNR8 824662033�
  �
    "  3  P      �  �                                               WINDOWS _1BQ0ZFNR9 824660925�  �  �  �          �  �                                               WINDOWS _1BQ0ZFNQU 824660746�  v  g  T  7  �                                                           WINDOWS _1BR0KFR6O 824660746$    �  �  �  �                                                           WINDOWS _1BR0KFR6P 824659413�  j  [  L  /  �                                                           COMMENT RESERVED                                �                                                            *                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      dataenvironment      dataenvironment      Dataenvironment      Name = "Dataenvironment"
      1      2      ariaformset      d:\aria27\classes\main.vcx      formset      ariaformset     JAutoRelease = .T.
WindowType = 1
formhastoolbar = 
Name = "ariaformset"
Ariaform1.Height = 145
Ariaform1.Width = 350
Ariaform1.DoCreate = .T.
Ariaform1.Caption = "Currency Exchage Rate"
Ariaform1.Closable = .F.
Ariaform1.MaxButton = .F.
Ariaform1.MinButton = .F.
Ariaform1.WindowType = 1
Ariaform1.Name = "Ariaform1"
     �PROCEDURE Unload
DODEFAULT()
RETURN Thisformset.RetRate 
ENDPROC
PROCEDURE Init
LPARAMETERS lcBaseCurr,lcCurrency, ldDate

DODEFAULT()
thisformset.ccurrency    = lcCurrency
thisformset.basecurrency = lcBaseCurr
Thisformset.CurrentDate  = ldDate
Thisformset.AriaForm1.txtDate.Value = ldDate
Thisformset.Ariaform1.txtCurrency.Value = ALLTRIM(lcCurrency) + ' to ' + ALLTRIM(lcBaseCurr)
ENDPROC
      ����    �   �                         �   %   S       h      b           �  U   
 ��  � � U  THISFORMSET RELEASE Click,     ��1 � 1                       $       )   �                         )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      MInputMask = "99999.99"
Left = 70
TabIndex = 3
Top = 75
Name = "txtRate"
      ariaformset.Ariaform1      txtRate      textbox      d:\aria27\classes\main.vcx      ariatextbox      DEnabled = .F.
Left = 70
TabIndex = 2
Top = 45
Name = "txtDate"
      /basecurrency
ccurrency
retrate
currentdate
      	arialabel      d:\aria27\classes\main.vcx      label      ArialblEneter      ariaformset.Ariaform1      �Caption = "Enter the exchage rate from :"
Height = 15
Left = 20
Top = 15
Width = 139
TabIndex = 6
Name = "ArialblEneter"
      	arialabel      d:\aria27\classes\main.vcx      label      
Arialabel1      ariaformset.Ariaform1      dCaption = "Date:"
Height = 15
Left = 20
Top = 45
Width = 28
TabIndex = 7
Name = "Arialabel1"
      	arialabel      d:\aria27\classes\main.vcx      label      
Arialabel2      ariaformset.Ariaform1      dCaption = "Rate:"
Height = 15
Left = 20
Top = 75
Width = 28
TabIndex = 8
Name = "Arialabel2"
      ariacommandbutton      d:\aria27\classes\main.vcx      commandbutton      	AriacmdOk      ariaformset.Ariaform1      aTop = 110
Left = 81
Height = 26
Width = 73
Caption = "Ok"
TabIndex = 4
Name = "AriacmdOk"
     �PROCEDURE Click
Local ldCurrDate,lnExRate

USE (oAriaApplication.SysPath + 'SYCEXCH') IN 0
ldCurrDate = Thisformset.CurrentDate
lnExRate   = VAL(thisformset.Ariaform1.txtRate.Value)

IF lnExRate<=0
  =oAriaApplication.MessageBox('QRM00247B00000','DIALOG')
  thisformset.ariaform1.txtRate.SetFocus()
ENDIF
IF !SEEK(thisformset.basecurrency+thisformset.ccurrency+DTOS(ldCurrDate),'SYCEXCH','CURRENCY')
  INSERT INTO ('SYCEXCH') (cBaseCurr,cCurrCode,DRATEDATE) VALUES ;
                          (thisformset.basecurrency,thisformset.ccurrency,ldCurrDate)
ENDIF
REPLACE nExRate WITH lnExRate IN 'SYCEXCH'
=gfAdd_Info('SYCEXCH',Thisformset)
Thisformset.RetRate = lnExRate
Thisformset.Release 

ENDPROC
     ���    �  �                        ��   %   q      �     �          �  U  ` ��  � � Q�  �� � � SYCEXCH�� T�  �� � �� T� �C� � � � g�� %�� � ��� �' ��C� QRM00247B00000� DIALOG� �	 �� ��C� � � �
 �� �5 %�C� � � � C�  �� SYCEXCH� CURRENCY�
���9 r��� SYCEXCH�� � � ��� � ��� � ���  �� � >�� SYCEXCH�� ��� �� ��C� SYCEXCH � � �� T� � �� ��
 �� � � U 
 LDCURRDATE LNEXRATE OARIAAPPLICATION SYSPATH THISFORMSET CURRENTDATE	 ARIAFORM1 TXTRATE VALUE
 MESSAGEBOX SETFOCUS BASECURRENCY	 CCURRENCY	 CBASECURR	 CCURRCODE	 DRATEDATE NEXRATE IN
 GFADD_INFO RETRATE RELEASE Click,     ��1 � ��q1A Q�A ��� 2                       �      )   �                        ariacommandbutton      d:\aria27\classes\main.vcx      commandbutton      AriacmdCancel      ariaformset.Ariaform1      jTop = 110
Left = 196
Height = 26
Width = 73
Caption = "Cancel"
TabIndex = 5
Name = "AriacmdCancel"
      /PROCEDURE Click
Thisformset.Release
ENDPROC
      ariaformset.Ariaform1      txtDate      textbox      d:\aria27\classes\main.vcx      ariatextbox      ariaformset.Ariaform1      txtCurrency      textbox      d:\aria27\classes\main.vcx      ariatextbox      cEnabled = .F.
Height = 21
Left = 160
TabIndex = 1
Top = 15
Width = 175
Name = "txtCurrency"
     ���    �  �                        ��   %   d      �     �          �  U   	 ��C��� B��  � �� U  THISFORMSET RETRATE�  ��  � � �	 ��C��� T� � �� �� T� � ��  �� T� � �� �� T� � � �	 �� ��& T� � �
 �	 �C� ��  to C�  ��� U 
 LCBASECURR
 LCCURRENCY LDDATE THISFORMSET	 CCURRENCY BASECURRENCY CURRENTDATE	 ARIAFORM1 TXTDATE VALUE TXTCURRENCY Unload,     �� Init^     ��1 � � 2 � � aa1                       :         U   �      )   �                  0   m                   PLATFORM   C                  UNIQUEID   C	   
               TIMESTAMP  N   
               CLASS      M                  CLASSLOC   M!                  BASECLASS  M%                  OBJNAME    M)                  PARENT     M-                  PROPERTIES M1                  PROTECTED  M5                  METHODS    M9                  OBJCODE    M=                 OLE        MA                  OLE2       ME                  RESERVED1  MI                  RESERVED2  MM                  RESERVED3  MQ                  RESERVED4  MU                  RESERVED5  MY                  RESERVED6  M]                  RESERVED7  Ma                  RESERVED8  Me                  USER       Mi                                                                                                                                                                                                                                                                                          COMMENT Class                                                                                               WINDOWS _0HN0QPHME 827028972�  X  H  �      �  `  �  z          �  �            �               COMMENT RESERVED                        I                                                                  h                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VERSION =   3.00      )MS Sans Serif, 0, 8, 5, 13, 11, 11, 2, 0
      ucclabelversion      \sourcefile^
afterinit^
filterexpression_assign^
getlabelversions^
addemptyitem_assign^
      Pixels      Class      1      ariacombobox      ucclabelversion     _sourcefile Unique (Temporary) name for the file that will hold the different versions of UCC128 labels, which will be used as a source for the combo box.
addemptyitem If this property is set to true the combo box will add an empty Item to the different label versions available in the dropdown list.
filterexpression Filter expression, for where clause, for filtering the available label versions.
afterinit Flag to know if the Init method has been called yet or not.
*filterexpression_assign 
*getlabelversions Method to fill the source file with the available label versions.
*addemptyitem_assign 
     
?���    &
  &
                        V   %   p      �	  [   �          �  U  �  ��  �, %�C� lcNewValb� C� �  � � 
	��� � T� � ��  �� %�� � ��� � �� � T� �� � �� ��C� � �� T� � �� �� %�� � � ��� � T� � ��  �� � � � U  LCNEWVAL THIS FILTEREXPRESSION	 AFTERINIT LCVALUE VALUE GETLABELVERSIONS	 LISTINDEXe ��  � � T�  �C� W�� %�C� SYCASNHD���< � T� �-�� �m � T� �a�� Q�  �� � � SYCASNHD�� � T� � ��  �� %�C� � ���� � Q�� � �� � G �6 %�C� This.FilterExpressionb� C�
 C� � �
	��Q� �� � T� �� � ��/ %�C� This.AddEmptyItemb� L� � �	 	����� SELECT DISTINCT cVer FROM SYCASNHD WHERE &lcFilterExpression UNION (SELECT SPACE(3) AS cVer FROM SYCASNHD) INTO TABLE (oAriaApplication.WorkDir + This.SourceFile)
 �M�x SELECT DISTINCT cVer FROM SYCASNHD WHERE &lcFilterExpression INTO TABLE (oAriaApplication.WorkDir + This.SourceFile)
 � � �/ %�C� This.AddEmptyItemb� L� � �	 	����D o��  � SYCASNHD�C�X�Q� �� SYCASNHD�� ��1�� � � � �� ���( o�� SYCASNHD�� ��1�� � � � �� � � %�� ��� Q� � � F�� � �� & �� ��� � T� � �� � �� ��C� � ��
 F��  �� U  LNOLDWORKAREA LLCLOSEASNLB OARIAAPPLICATION SYSPATH THIS	 ROWSOURCE
 SOURCEFILE FILTEREXPRESSION LCFILTEREXPRESSION ADDEMPTYITEM DISTINCT CVER SYCASNHD WORKDIR LABELVER REFRESH_ ��  �* %�C� llNewValb� L� �  � � 	��X� T� � ��  �� %�� � ��T� �� � T� �� � �� T� � ��  �� %��  ��� �# %�CC�X� � � LabelVer�
��� � r��� � �� ��C�X�� � ��" %�CC�X� � � LabelVer���� �� � �� � � T� � �� � �� T� � �� �� %�� �	 � ��P� T� � ��  �� � � � U
  LLNEWVAL THIS ADDEMPTYITEM	 AFTERINIT LCVALUE VALUE	 ROWSOURCE
 SOURCEFILE CVER	 LISTINDEXh # %�C� This.SourceFileb� C��a � %�C�  � ���C � Q��  � �� �  �� � �  � � .*�� � U  THIS
 SOURCEFILE OARIAAPPLICATION WORKDIRd 	 ��C��� ��  � T�  �C� GetTempName�N�� T� � �C�  � �� <�  � ��C� � �� T� � �a�� U 
 LOTEMPNAME THIS
 SOURCEFILE DO GETLABELVERSIONS	 AFTERINIT filterexpression_assign,     �� getlabelversionsD    �� addemptyitem_assignl    �� Destroy0    �� Init�    ��1 q �q � BA A A 3 � � q� � � �A "� A a aq �e
� �A � �D� �A A � � A � � 2� � 3 q �q � 1�A � !� A A 1BA A A 3 1!� A �A 3 � r �Aq � � 2                    #   �        �  �     4   �  N
  M   L   l
    j   S   4  �  t    )   &
                       �PROCEDURE filterexpression_assign
LPARAMETERS lcNewVal

IF TYPE("lcNewVal") = "C" .AND. !(lcNewVal == This.FilterExpression)
  This.FilterExpression = lcNewVal
  IF This.AfterInit
    LOCAL lcValue
    
    lcValue        = This.Value
    This.GetLabelVersions()
    This.Value     = lcValue
    
    IF This.ListIndex = 0
      This.Value = ""
    ENDIF
  ENDIF
ENDIF

ENDPROC
PROCEDURE getlabelversions
LOCAL lnOldWorkArea , llCloseASNLb

lnOldWorkArea = SELECT(0)
IF USED("SYCASNHD")
  llCloseASNLb = .F.
ELSE
  llCloseASNLb = .T.
  USE (oAriaApplication.SysPath + "SYCASNHD") IN 0
ENDIF

This.RowSource = ""

IF USED(This.SourceFile)
  USE IN (This.SourceFile)
ENDIF
SET DELETED ON
IF TYPE("This.FilterExpression") = "C" .AND. !EMPTY(This.FilterExpression)
  LOCAL lcFilterExpression
  lcFilterExpression = This.FilterExpression
  
  IF TYPE("This.AddEmptyItem") = "L" .AND. This.AddEmptyItem
    SELECT DISTINCT cVer;
              FROM SYCASNHD;
              WHERE &lcFilterExpression;
              UNION (SELECT SPACE(3) AS cVer FROM SYCASNHD);
              INTO TABLE (oAriaApplication.WorkDir + This.SourceFile)
  ELSE
    SELECT DISTINCT cVer;
              FROM SYCASNHD;
              WHERE &lcFilterExpression;
              INTO TABLE (oAriaApplication.WorkDir + This.SourceFile)
  ENDIF
ELSE
  IF TYPE("This.AddEmptyItem") = "L" .AND. This.AddEmptyItem
    SELECT DISTINCT cVer;
              FROM SYCASNHD;
              UNION (SELECT SPACE(3) AS cVer FROM SYCASNHD);
              INTO TABLE (oAriaApplication.WorkDir + This.SourceFile)
  ELSE
    SELECT DISTINCT cVer;
               FROM SYCASNHD;
               INTO TABLE (oAriaApplication.WorkDir + This.SourceFile)
  ENDIF
ENDIF
IF llCloseASNLb
  USE IN SYCASNHD
ENDIF
SELECT (This.SourceFile)
INDEX ON cVer TAG LabelVer

This.RowSource = This.SourceFile
This.Refresh()

SELECT (lnOldWorkArea)

ENDPROC
PROCEDURE addemptyitem_assign
LPARAMETERS llNewVal

IF TYPE("llNewVal") = "L" .AND. llNewVal <> This.AddEmptyItem
  This.AddEmptyItem = llNewVal
  IF This.AfterInit
    LOCAL lcValue
    
    lcValue        = This.Value
    This.RowSource = ""
    IF llNewVal
      IF !SEEK(SPACE(3) , This.SourceFile , "LabelVer")
        INSERT INTO (This.SourceFile) (cVer) VALUES (SPACE(3))
      ENDIF
    ELSE
      IF SEEK(SPACE(3) , This.SourceFile , "LabelVer")
        DELETE IN (This.SourceFile)
      ENDIF
    ENDIF
    This.RowSource = This.SourceFile
    This.Value     = lcValue
    
    IF This.ListIndex = 0
      This.Value = ""
    ENDIF
  ENDIF
ENDIF

ENDPROC
PROCEDURE Destroy
IF TYPE("This.SourceFile") = "C"
  IF USED(This.SourceFile)
    USE IN (This.SourceFile)
  ENDIF
  
  ERASE (oAriaApplication.WorkDir + This.SourceFile + ".*")
ENDIF

ENDPROC
PROCEDURE Init
DoDefault()

LOCAL loTempName

loTempName      = CREATEOBJECT("GetTempName")
This.SourceFile = loTempName.Do()
RELEASE loTempName

This.GetLabelVersions()
This.AfterInit = .T.

ENDPROC
      ~RowSourceType = 2
RowSource = ""
Style = 2
Width = 60
filterexpression = ("")
afterinit = .F.
Name = "ucclabelversion"
      combobox      main.vcx..\screens\sy\visual\ syupdate.scx syupdate.sct ..\classes\ globals.vcx globals.vct main.vcx main.vct utility.vcx utility.vct ..\prgs\sy\aria.prg c:\docume~1\hia\locals~1\temp\ aria.fxp ..\bmps\ logo.bmp prtprev.bmp print.bmp close.bmp close.msk tbtop.bmp tbback.bmp tbnext.bmp tbend.bmp tbnew.bmp open.bmp open.msk tbprint.bmp brow1.bmp brow1.msk edit1.bmp edit1.msk delete.bmp delete.msk tasklst.bmp tasklst.msk calend.bmp calc.bmp calc.msk ..\screens\sy\syactkey.spr syactkey.spx info.bmp qry.bmp tr.bmp syabout.scx syabout.sct sylogin.scx sylogin.sct syusrlst.scx syusrlst.sct print.msk browse.scx browse.sct ..\screens\sy\ sygetactv.scx ..\screens\sy\ sygetactv.sct sychrate.scx sychrate.sct ediutil.vcx ediutil.vct 	)                      M      #           M  �0  0   <           �0   0   H            �D 0   T           �D �
 0   ]           �
 2,
 0   f           2,
 [< 0   r            [< �� �   �           �� �- �   �           �- �. �   �           �. �/ �   �           �/ �0 �   �           �0 >1 �   �           >1 ,2 �   �           ,2 3 �              3 4 �             4 �4 �             �4 \6 �              \6 J7 �   *          J7 �7 �   3          �7 �8 �   <          �8 �9 �   H          �9 ^: �   R          ^: L; �   \          L; �; �   f          �; �< �   p          �< .= �   {          .= > �   �          > �> �   �          �> �? �   �          �? Z@ �   �          Z@ �@ �   �           �@ /f �   �          /f Mi �   �          Mi kl �   �          kl �o �   �          	�o �     �          � H�               	H� ��               �� �               	� �     +          � �"     8          �" G# �   E          	G# �*     O          �* ,�     Z          	,� +� e  t          +� �� �  �          	�� ��     �          �� ��     �          �� A� 0   �          A� � 0   �          