FasdUAS 1.101.10   ��   ��    k             l    Z ����  O     Z  	  k    Y 
 
     l   ��  ��    - ' Get the full path to the script or app     �   N   G e t   t h e   f u l l   p a t h   t o   t h e   s c r i p t   o r   a p p      r        l    ����  I   ��  
�� .earsffdralis        afdr   f      �� ��
�� 
rtyp  m    ��
�� 
ctxt��  ��  ��    o      ���� 0 
scriptpath 
scriptPath      l   ��������  ��  ��        l   ��  ��    7 1 Find the position of "lunch_tracker" in the path     �   b   F i n d   t h e   p o s i t i o n   o f   " l u n c h _ t r a c k e r "   i n   t h e   p a t h       r     ! " ! I   ���� #
�� .sysooffslong    ��� null��   # �� $ %
�� 
psof $ m     & & � ' '  l u n c h _ t r a c k e r % �� (��
�� 
psin ( o    ���� 0 
scriptpath 
scriptPath��   " o      ���� 0 appfolderend appFolderEnd    ) * ) l   ��������  ��  ��   *  + , + l   �� - .��   - / ) Extract everything up to "lunch_tracker"    . � / / R   E x t r a c t   e v e r y t h i n g   u p   t o   " l u n c h _ t r a c k e r " ,  0 1 0 r    - 2 3 2 n    + 4 5 4 7   +�� 6 7
�� 
ctxt 6 m    !����  7 l  " * 8���� 8 \   " * 9 : 9 [   # ( ; < ; o   # $���� 0 appfolderend appFolderEnd < l  $ ' =���� = n   $ ' > ? > 1   % '��
�� 
leng ? m   $ % @ @ � A A  l u n c h _ t r a c k e r��  ��   : m   ( )���� ��  ��   5 o    ���� 0 
scriptpath 
scriptPath 3 o      ���� 0 	appfolder 	appFolder 1  B C B l  . .��������  ��  ��   C  D E D r   . 3 F G F b   . 1 H I H o   . /���� 0 	appfolder 	appFolder I m   / 0 J J � K K 2 : z z z _ c o d e : l u n c h _ t r a c k e r . R G o      ���� 0 fullpath fullPath E  L M L l  4 4��������  ��  ��   M  N O N l  4 4�� P Q��   P 7 1 Convert the AppleScript path to a file reference    Q � R R b   C o n v e r t   t h e   A p p l e S c r i p t   p a t h   t o   a   f i l e   r e f e r e n c e O  S T S r   4 = U V U l  4 9 W���� W c   4 9 X Y X o   4 5���� 0 fullpath fullPath Y m   5 8��
�� 
alis��  ��   V o      ���� 0 fileref fileRef T  Z [ Z l  > >��������  ��  ��   [  \ ] \ l  > >�� ^ _��   ^ 1 + Convert the file reference to a POSIX path    _ � ` ` V   C o n v e r t   t h e   f i l e   r e f e r e n c e   t o   a   P O S I X   p a t h ]  a b a r   > I c d c n   > E e f e 1   A E��
�� 
posx f o   > A���� 0 fileref fileRef d o      ���� 0 posixfullpath posixFullPath b  g h g l  J J�� i j��   i B <display dialog "POSIX Path: " & quoted form of posixFullPath    j � k k x d i s p l a y   d i a l o g   " P O S I X   P a t h :   "   &   q u o t e d   f o r m   o f   p o s i x F u l l P a t h h  l m l l  J J��������  ��  ��   m  n�� n I  J Y�� o��
�� .sysoexecTEXT���     TEXT o b   J U p q p m   J M r r � s s . / u s r / l o c a l / b i n / R s c r i p t   q n   M T t u t 1   P T��
�� 
strq u o   M P���� 0 posixfullpath posixFullPath��  ��   	 m      v v�                                                                                  sevs  alis    \  Macintosh HD               �ym{BD ����System Events.app                                              �����ym{        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p    M a c i n t o s h   H D  -System/Library/CoreServices/System Events.app   / ��  ��  ��     w�� w l     ��������  ��  ��  ��       �� x y��   x ��
�� .aevtoappnull  �   � **** y �� z���� { |��
�� .aevtoappnull  �   � **** z k     Z } }  ����  ��  ��   {   |  v���������� &�������� @���� J���������� r����
�� 
rtyp
�� 
ctxt
�� .earsffdralis        afdr�� 0 
scriptpath 
scriptPath
�� 
psof
�� 
psin�� 
�� .sysooffslong    ��� null�� 0 appfolderend appFolderEnd
�� 
leng�� 0 	appfolder 	appFolder�� 0 fullpath fullPath
�� 
alis�� 0 fileref fileRef
�� 
posx�� 0 posixfullpath posixFullPath
�� 
strq
�� .sysoexecTEXT���     TEXT�� [� W)��l E�O*����� 	E�O�[�\[Zk\Z���,k2E�O��%E�O�a &E` O_ a ,E` Oa _ a ,%j U ascr  ��ޭ