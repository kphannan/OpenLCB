
serjtag は、USB910(A) の jtag 機能 と i2c マスタ機能を取り出して、
ATtiny2313 に移植したものです。

USB910 の jtag 機能は、jtag にしては遅すぎるために、
秋月の ＦＴ２４５ＲＬ　ＵＳＢパラレル変換モジュール(K-1799)
を使う、高速版という位置づけで 作りました。

いまのところ、この jtag 機能を サポートしているソフトウェアはありません
が、xilprg という ソフトを改造すれば、Xilinx のデバイスへの書き込みが
できるようになります。

xilprg http://sourceforge.net/projects/xilprg

    xilprg is a Xilinx FPGA/PROM/CPLD JTAG programmer tool for Win32/Linux.
    Supports Parallel III cable, Digilent USB. cblsrv is a Open-Source 
    CableServer for Xilinx Impact.


プロトコル


一般コマンド

'S' : get Software
   * SERJTAG を返します。
'V' : get software Version
   * 拡張モードの場合、拡張モードを リセットします。
'v' : get hardware Version
   * 拡張モードの場合、拡張モードを リセットします。
'e' : 拡張モード(1)にします。
'j' : 拡張モード(2)にします。

拡張モード(1) コマンド

's' chan size 

   chan (1バイト) チャネル 
   size (1バイト) 1-8 の範囲 
   戻り
      バイト数 (マイナスの場合はエラー)


'r' chan size

   chan (1バイト) チャネル 
   size (1バイト) 1-8 の範囲 
   戻り
       バイト数 (マイナスの場合はエラー)
       受信したバイト数 

チャネル
   0xA  以上 I2C

エラーコード
   (-1) チャネルが存在しない or size が範囲外。
   (-2 ～ ) デバイス固有のエラーコード

JTAG/SPIマスタ (拡張モード2 コマンド)

    ( 'j' コマンドで、拡張モード 2 に入る必要があります。 )

パケットの形式

    +-----------+-----+-----+-----------+-----------+    +-----------+
    | COMMONAD  |FLAGS|BITS |   BYTES   |   DATA 1  |... |   DATA N  |
    +-----------+-----+-----+-----------+-----------+    +-----------+
         8         5     3       8

このフォーマットで送受信ともやりとりします。

FLAGS : bit4 RECIEVE  フラグ
        bit3 TMS_HIGH フラグ 
        bit2 TDI_HIGH フラグ 
        bit1 USE_DELAY フラグ
            : JTAG 転送時 TCK の ON/OFF 時に delay を挿入します。
        bit0 0 未定義

データ長 (bit 単位)

	BYTES * 8 + BITS
	( データ バイト数 N = BYTES + (BITS + 7)/8 )


データフォーマット

	MSB first の bit-stream
	最大 511 bit

'r'  Request Recieved Data
        データ長 = 0
        データ 0 バイト

	記録した TDO Stream の 受け取り要求
        TDO Stream は、COMMAND に 'R' が設定されて 送られてきます。
 
's'  Set Port
        データ長 = 1 バイト 

        データ bit7    TDI 値
               bit6    TMS 値
               bit5    TCK 値
               bit3-0  ディレイ値 

'd'  Put TDI Stream
        データ長 = TDI Stream ビット数
       
        TMS_HIGH フラグ によって TMS を設定し、
        TDI Stream を ターゲットに 送ります。
        
        RECIEVE フラグが 1 の場合、TDO Stream を 記録します。

'D'  Put TDI+TMS Stream
        データ長 = TDI+TMS Stream ビット数
       
        TMS_HIGH フラグ によって TMS を設定し、
        TDI+TMS Stream を ターゲットに 送ります。
        bitmap は、TDI TMS TDI TMS の順にデータをセットします。

        RECIEVE フラグが 1 の場合、TDO Stream を 記録します。

'c'  Get TDO Stream
        データ長 = TDO Stream ビット数
        データ 0 バイト

        TMS_HIGH フラグ と TDI_HIGH フラグ によって TMS,TDI を設定し、
        固定値の TDI Stream を ターゲットに 送り、TDO Stream を 記録します。

---

拡張モード 2 プログラミングメモ


初期化

   (1) 'S' コマンドで プログラマ名を取得。

	"SERJTAG" または、"USB910" をチェック

   (2) 'V' コマンド を発行します。

	"24" などと 2 バイトの情報が返却される。

   (3) 'j' コマンド で 拡張モード(2)にする。

	"Y" が返却されたら、拡張モード(2)をサポート。
	"?" の場合は、拡張モード(2)をサポートしていない。

状態がわからないときの再初期化

   (1) 'V' コマンド 70回 発行し、受信データをフラッシュ(全部捨てる)。

        最大データ送信長は、3+64バイトなので、'V' コマンドを送りつづければ、
        状態が確定します。

   (2) 上の 初期化手順を行う。


  

