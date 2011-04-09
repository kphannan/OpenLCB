
serjtag �́AUSB910(A) �� jtag �@�\ �� i2c �}�X�^�@�\�����o���āA
ATtiny2313 �ɈڐA�������̂ł��B

USB910 �� jtag �@�\�́Ajtag �ɂ��Ă͒x�����邽�߂ɁA
�H���� �e�s�Q�S�T�q�k�@�t�r�a�p�������ϊ����W���[��(K-1799)
���g���A�����łƂ����ʒu�Â��� ���܂����B

���܂̂Ƃ���A���� jtag �@�\�� �T�|�[�g���Ă���\�t�g�E�F�A�͂���܂���
���Axilprg �Ƃ��� �\�t�g����������΁AXilinx �̃f�o�C�X�ւ̏������݂�
�ł���悤�ɂȂ�܂��B

xilprg http://sourceforge.net/projects/xilprg

    xilprg is a Xilinx FPGA/PROM/CPLD JTAG programmer tool for Win32/Linux.
    Supports Parallel III cable, Digilent USB. cblsrv is a Open-Source 
    CableServer for Xilinx Impact.


�v���g�R��


��ʃR�}���h

'S' : get Software
   * SERJTAG ��Ԃ��܂��B
'V' : get software Version
   * �g�����[�h�̏ꍇ�A�g�����[�h�� ���Z�b�g���܂��B
'v' : get hardware Version
   * �g�����[�h�̏ꍇ�A�g�����[�h�� ���Z�b�g���܂��B
'e' : �g�����[�h(1)�ɂ��܂��B
'j' : �g�����[�h(2)�ɂ��܂��B

�g�����[�h(1) �R�}���h

's' chan size 

   chan (1�o�C�g) �`���l�� 
   size (1�o�C�g) 1-8 �͈̔� 
   �߂�
      �o�C�g�� (�}�C�i�X�̏ꍇ�̓G���[)


'r' chan size

   chan (1�o�C�g) �`���l�� 
   size (1�o�C�g) 1-8 �͈̔� 
   �߂�
       �o�C�g�� (�}�C�i�X�̏ꍇ�̓G���[)
       ��M�����o�C�g�� 

�`���l��
   0xA  �ȏ� I2C

�G���[�R�[�h
   (-1) �`���l�������݂��Ȃ� or size ���͈͊O�B
   (-2 �` ) �f�o�C�X�ŗL�̃G���[�R�[�h

JTAG/SPI�}�X�^ (�g�����[�h2 �R�}���h)

    ( 'j' �R�}���h�ŁA�g�����[�h 2 �ɓ���K�v������܂��B )

�p�P�b�g�̌`��

    +-----------+-----+-----+-----------+-----------+    +-----------+
    | COMMONAD  |FLAGS|BITS |   BYTES   |   DATA 1  |... |   DATA N  |
    +-----------+-----+-----+-----------+-----------+    +-----------+
         8         5     3       8

���̃t�H�[�}�b�g�ő���M�Ƃ����Ƃ肵�܂��B

FLAGS : bit4 RECIEVE  �t���O
        bit3 TMS_HIGH �t���O 
        bit2 TDI_HIGH �t���O 
        bit1 USE_DELAY �t���O
            : JTAG �]���� TCK �� ON/OFF ���� delay ��}�����܂��B
        bit0 0 ����`

�f�[�^�� (bit �P��)

	BYTES * 8 + BITS
	( �f�[�^ �o�C�g�� N = BYTES + (BITS + 7)/8 )


�f�[�^�t�H�[�}�b�g

	MSB first �� bit-stream
	�ő� 511 bit

'r'  Request Recieved Data
        �f�[�^�� = 0
        �f�[�^ 0 �o�C�g

	�L�^���� TDO Stream �� �󂯎��v��
        TDO Stream �́ACOMMAND �� 'R' ���ݒ肳��� �����Ă��܂��B
 
's'  Set Port
        �f�[�^�� = 1 �o�C�g 

        �f�[�^ bit7    TDI �l
               bit6    TMS �l
               bit5    TCK �l
               bit3-0  �f�B���C�l 

'd'  Put TDI Stream
        �f�[�^�� = TDI Stream �r�b�g��
       
        TMS_HIGH �t���O �ɂ���� TMS ��ݒ肵�A
        TDI Stream �� �^�[�Q�b�g�� ����܂��B
        
        RECIEVE �t���O�� 1 �̏ꍇ�ATDO Stream �� �L�^���܂��B

'D'  Put TDI+TMS Stream
        �f�[�^�� = TDI+TMS Stream �r�b�g��
       
        TMS_HIGH �t���O �ɂ���� TMS ��ݒ肵�A
        TDI+TMS Stream �� �^�[�Q�b�g�� ����܂��B
        bitmap �́ATDI TMS TDI TMS �̏��Ƀf�[�^���Z�b�g���܂��B

        RECIEVE �t���O�� 1 �̏ꍇ�ATDO Stream �� �L�^���܂��B

'c'  Get TDO Stream
        �f�[�^�� = TDO Stream �r�b�g��
        �f�[�^ 0 �o�C�g

        TMS_HIGH �t���O �� TDI_HIGH �t���O �ɂ���� TMS,TDI ��ݒ肵�A
        �Œ�l�� TDI Stream �� �^�[�Q�b�g�� ����ATDO Stream �� �L�^���܂��B

---

�g�����[�h 2 �v���O���~���O����


������

   (1) 'S' �R�}���h�� �v���O���}�����擾�B

	"SERJTAG" �܂��́A"USB910" ���`�F�b�N

   (2) 'V' �R�}���h �𔭍s���܂��B

	"24" �Ȃǂ� 2 �o�C�g�̏�񂪕ԋp�����B

   (3) 'j' �R�}���h �� �g�����[�h(2)�ɂ���B

	"Y" ���ԋp���ꂽ��A�g�����[�h(2)���T�|�[�g�B
	"?" �̏ꍇ�́A�g�����[�h(2)���T�|�[�g���Ă��Ȃ��B

��Ԃ��킩��Ȃ��Ƃ��̍ď�����

   (1) 'V' �R�}���h 70�� ���s���A��M�f�[�^���t���b�V��(�S���̂Ă�)�B

        �ő�f�[�^���M���́A3+64�o�C�g�Ȃ̂ŁA'V' �R�}���h�𑗂�Â���΁A
        ��Ԃ��m�肵�܂��B

   (2) ��� �������菇���s���B


  

