      *****************************************************************
      * ＫＥＩＫＡ：経過マスタ ［ＲＬ＝１２８BYTE］
      *****************************************************************
      * SYSYTEM NAME:柔 FOR WINNDOWS                                  *
      * LANGUAGE PG :COBOL97 FOR WINDOWS                              *
      * PRODUCT DATE:1997.11.15                                       *
      * AUTHOR      :上田  育正                                       *
      * PURPOSE     :当面、区分コードは、０１（負傷経過）で固定       *
      *              経過コード毎の、経過コメント・記号を管理         *
      *****************************************************************
       01 レコード.
         03 レコードキー.
      *****************************************************************
      * 負傷経過：０１
           05 区分コード                     PIC 9(2).
      *****************************************************************
           05 経過コード                     PIC 9(4).
         03 レコードデータ.
           05 経過コメント                   PIC N(20).
           05 経過コメント２                 PIC N(20).
      *****************************************************************
      * 軽快　良好　やや良好　やや悪化　悪化
           05 経過略称                       PIC N(6).
      *****************************************************************
      * 軽快：☆　良好：◎　やや良好：○　やや悪化：△　悪化：×
           05 経過記号                       PIC N.
      *****************************************************************
           05 FILLER                         PIC X(28).
