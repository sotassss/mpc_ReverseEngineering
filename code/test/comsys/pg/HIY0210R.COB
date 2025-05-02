000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             HIY0210R.
000060 AUTHOR.                 山田 浩之
000070*
000080*----------------------------------------------------------------*
000090*         費用額計算プログラム　レセプト発行［健康保険用］（柔）
000100*
000110* 2001年01月から老人保険の改正の為一部修正 修正個所は[*0101]
000120* 2002年10月から老人保険の改正の為修正[*0210]
000130*         MED = 
000140*----------------------------------------------------------------*
000150* 患者１人の月のレセプト項目の算出。小計（長期逓減含む小計）。
000160* 費用額（小計の縦計）。負担金。請求額を求める。
000170* 
000180* 平成14年10月～用
000530*/ 入金額初期値 0:一部負担金 1:０円/140821
      */施術者番号の対応/151229～160112
      */初期化項目追加/160221
      */日毎部位毎に料金が発生しない場合は実日数に含めない/20160804
000190*----------------------------------------------------------------*
000200 DATE-WRITTEN.           1997-12-21
000210 DATE-COMPILED.          1997-12-21
000220*----------------------------------------------------------------*
000230* 修正履歴
000240* 19981210 自費診療計算ができるように修正
000250******************************************************************
000260*            ENVIRONMENT         DIVISION                        *
000270******************************************************************
000280 ENVIRONMENT             DIVISION.
000290 CONFIGURATION           SECTION.
000300 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000310 OBJECT-COMPUTER.        FMV-DESKPOWER.
000320 SPECIAL-NAMES.          CONSOLE  IS  CONS
000330                         SYSERR   IS  MSGBOX.
000340 INPUT-OUTPUT            SECTION.
000350 FILE-CONTROL.
000360     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  施記－施術和暦年月日
000400                                                          施記－患者コード
000410                             ALTERNATE RECORD KEY     IS  施記－患者コード
000420                                                          施記－施術和暦年月日
000430                             FILE STATUS              IS  状態キー
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  料金マスタ      ASSIGN      TO        RYOUKINL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  料－区分コード
000490                                                          料－部位コード
000500                                                          料－開始和暦年月
000510                             FILE STATUS              IS  状態キー
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  計算マスタ      ASSIGN      TO        KEISANL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  計－制御区分
000570                                                          計－開始和暦年月
000580                             FILE STATUS              IS  状態キー
000590                             LOCK        MODE         IS  AUTOMATIC.
000600*     SELECT  計算ＰＧマスタ  ASSIGN      TO        KEISAPGL
000610*                             ORGANIZATION             IS  INDEXED
000620*                             ACCESS MODE              IS  DYNAMIC
000630*                             RECORD KEY               IS  計Ｐ－区分コード
000640*                                                          計Ｐ－開始和暦年月
000650*                             FILE STATUS              IS  状態キー
000660*                             LOCK        MODE         IS  AUTOMATIC.
000670     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000680                             ORGANIZATION             IS  INDEXED
000690                             ACCESS MODE              IS  DYNAMIC
000700                             RECORD KEY               IS  受－施術和暦年月
000710                                                          受－患者コード
000720                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000730                                                          受－患者カナ
000740                                                          受－患者コード
000750                             ALTERNATE RECORD KEY     IS  受－患者コード
000760                                                          受－施術和暦年月
000770                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000780                                                          受－保険種別
000790                                                          受－保険者番号
000800                                                          受－患者コード
000810                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000820                                                          受－公費種別
000830                                                          受－費用負担者番号
000840                                                          受－患者コード
000850                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000860                                                          受－助成種別
000870                                                          受－費用負担者番号助成
000880                                                          受－患者コード
000890                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000900                                                          受－施術和暦年月
000910                                                          受－患者コード
000920                             FILE STATUS              IS  状態キー
000930                             LOCK        MODE         IS  AUTOMATIC.
000931*
000932     SELECT  参照受診者情報Ｆ    ASSIGN      TO        JUSINJL
000933                             ORGANIZATION             IS  INDEXED
000934                             ACCESS MODE              IS  DYNAMIC
000935                             RECORD KEY               IS  参照受－施術和暦年月
000936                                                          参照受－患者コード
000937                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
000938                                                          参照受－患者カナ
000939                                                          参照受－患者コード
000940                             ALTERNATE RECORD KEY     IS  参照受－患者コード
000941                                                          参照受－施術和暦年月
000942                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
000943                                                          参照受－保険種別
000944                                                          参照受－保険者番号
000945                                                          参照受－患者コード
000946                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
000947                                                          参照受－公費種別
000948                                                          参照受－費用負担者番号
000949                                                          参照受－患者コード
000950                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
000951                                                          参照受－助成種別
000952                                                          参照受－費用負担者番号助成
000953                                                          参照受－患者コード
000954                             ALTERNATE RECORD KEY     IS  参照受－請求和暦年月
000955                                                          参照受－施術和暦年月
000956                                                          参照受－患者コード
000957                             FILE STATUS              IS  状態キー
000958                             LOCK        MODE         IS  AUTOMATIC.
000959**
000962     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000963                             ORGANIZATION             IS  INDEXED
000964                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  負－施術和暦年月
000980                                                          負－患者コード
000990                             ALTERNATE RECORD KEY     IS  負－患者コード
001000                                                          負－施術和暦年月
001010                             FILE STATUS              IS  状態キー
001020                             LOCK        MODE         IS  AUTOMATIC.
001030     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
001040                             ORGANIZATION             IS  INDEXED
001050                             ACCESS MODE              IS  DYNAMIC
001060                             RECORD KEY               IS  制－制御区分
001070                             FILE STATUS              IS  状態キー
001080                             LOCK        MODE         IS  AUTOMATIC.
001160     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  元－元号区分
001200                             FILE STATUS              IS  状態キー
001210                             LOCK        MODE         IS  AUTOMATIC.
001220*/0101老人負担区分
001230     SELECT  老人負担区分マスタ    ASSIGN      TO      ROUJINHL
001240                             ORGANIZATION             IS  INDEXED
001250                             ACCESS MODE              IS  DYNAMIC
001260                             RECORD KEY               IS  老負－保険種別
001270                                                          老負－開始和暦年月
001280                             FILE STATUS              IS  状態キー
001290                             LOCK        MODE         IS  AUTOMATIC.
001300     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001310                             ORGANIZATION             IS  INDEXED
001320                             ACCESS MODE              IS  DYNAMIC
001330                             RECORD KEY               IS  市－公費種別
001340                                                          市－市町村番号
001350                             ALTERNATE RECORD KEY     IS  市－公費種別
001360                                                          市－市町村名称
001370                                                          市－市町村番号
001380                             FILE STATUS              IS  状態キー
001390                             LOCK        MODE         IS  AUTOMATIC.
001400*
001420     SELECT  会計データＦ    ASSIGN      TO        KAIKEIL
001430                             ORGANIZATION             IS  INDEXED
001440                             ACCESS MODE              IS  DYNAMIC
001450                             RECORD KEY               IS  会－施術和暦年月日
001460                                                          会－患者コード
001470                             ALTERNATE RECORD KEY     IS  会－患者コード
001480                                                          会－施術和暦年月日
001490                             FILE STATUS              IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
001510*
001520     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  レセ－施術和暦年月
001560                                                          レセ－患者コード
001570                                                          レセ－レセ種別
001580                             ALTERNATE RECORD KEY     IS  レセ－患者コード
001590                                                          レセ－施術和暦年月
001600                                                          レセ－レセ種別
001610                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001620                                                          レセ－施術和暦年月
001630                                                          レセ－患者コード
001640                                                          レセ－レセ種別
001650                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001660                                                          レセ－レセ種別
001670                                                          レセ－請求保険者番号
001680                                                          レセ－患者コード
001690                                                          レセ－施術和暦年月
001700                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001710                                                          レセ－請求保険者番号
001720                                                          レセ－患者コード
001730                                                          レセ－レセ種別
001740                                                          レセ－施術和暦年月
001750                             FILE STATUS              IS  状態キー
001760                             LOCK        MODE         IS  AUTOMATIC.
001770**
001780     SELECT  保険者特別負担マスタ   ASSIGN      TO        HOKENTKL
001790                                    ORGANIZATION          IS  INDEXED
001800                                    ACCESS MODE           IS  DYNAMIC
001810                                    RECORD KEY            IS  保特－保険種別
001820                                                              保特－保険者番号
001830                                                              保特－開始和暦年月
001840                                    FILE STATUS           IS  状態キー
001850                                    LOCK        MODE      IS  AUTOMATIC.
001860**
001861     SELECT  会計領収Ｆ      ASSIGN      TO        RYOSYUL
001862                             ORGANIZATION             IS  INDEXED
001863                             ACCESS MODE              IS  DYNAMIC
001864                             RECORD KEY               IS  領－施術区分
001865                                                          領－会計領収区分
001866                                                          領－施術和暦年月日
001867                                                          領－患者コード
001868                             ALTERNATE RECORD KEY     IS  領－施術区分
001869                                                          領－患者コード
001870                                                          領－会計領収区分
001871                                                          領－施術和暦年月日
001872                             FILE STATUS              IS  状態キー
001873                             LOCK        MODE         IS  AUTOMATIC.
001874*
000720     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS 施情－施術所番号
000760                             FILE STATUS              IS  状態キー
000770                             LOCK        MODE         IS  AUTOMATIC.
001875*--------------------------------------------------------------------------------------*
001876*--------------------------------------------------------------------------------------*
001877* EXTERNAL用
001878*
001880     SELECT  ＥＸ保険者マスタ    ASSIGN      TO        HOKENSL
001881                             ORGANIZATION             IS  INDEXED
001882                             ACCESS MODE              IS  DYNAMIC
001883                             RECORD KEY               IS  ＥＸ保－保険種別
001884                                                          ＥＸ保－保険者番号
001885                             ALTERNATE RECORD KEY     IS  ＥＸ保－保険種別
001886                                                          ＥＸ保－保険者名称
001887                                                          ＥＸ保－保険者番号
001888                             FILE STATUS              IS  状態キー
001889                             LOCK        MODE         IS  AUTOMATIC.
001890     SELECT  ＥＸ負担率マスタ    ASSIGN      TO        HUTANRIL
001891                             ORGANIZATION             IS  INDEXED
001892                             ACCESS MODE              IS  DYNAMIC
001893                             RECORD KEY               IS  ＥＸ負率－保険種別
001894                                                          ＥＸ負率－開始和暦年月
001895                             FILE STATUS              IS  状態キー
001896                             LOCK        MODE         IS  AUTOMATIC.
001897     SELECT  ＥＸ受診者情報Ｆ    ASSIGN      TO        JUSINJL
001898                             ORGANIZATION             IS  INDEXED
001899                             ACCESS MODE              IS  DYNAMIC
001900                             RECORD KEY               IS  ＥＸ受－施術和暦年月
001901                                                          ＥＸ受－患者コード
001902                             ALTERNATE RECORD KEY     IS  ＥＸ受－施術和暦年月
001903                                                          ＥＸ受－患者カナ
001904                                                          ＥＸ受－患者コード
001905                             ALTERNATE RECORD KEY     IS  ＥＸ受－患者コード
001906                                                          ＥＸ受－施術和暦年月
001907                             ALTERNATE RECORD KEY     IS  ＥＸ受－施術和暦年月
001908                                                          ＥＸ受－保険種別
001909                                                          ＥＸ受－保険者番号
001910                                                          ＥＸ受－患者コード
001911                             ALTERNATE RECORD KEY     IS  ＥＸ受－施術和暦年月
001912                                                          ＥＸ受－公費種別
001913                                                          ＥＸ受－費用負担者番号
001914                                                          ＥＸ受－患者コード
001915                             ALTERNATE RECORD KEY     IS  ＥＸ受－施術和暦年月
001916                                                          ＥＸ受－助成種別
001917                                                          ＥＸ受－費用負担者番号助成
001918                                                          ＥＸ受－患者コード
001919                             ALTERNATE RECORD KEY     IS  ＥＸ受－請求和暦年月
001920                                                          ＥＸ受－施術和暦年月
001921                                                          ＥＸ受－患者コード
001922                             FILE STATUS              IS  状態キー
001923                             LOCK        MODE         IS  AUTOMATIC.
001926     SELECT  ＥＸ施術記録Ｆ      ASSIGN      TO        SEKIROKL
001927                             ORGANIZATION             IS  INDEXED
001928                             ACCESS MODE              IS  DYNAMIC
001929                             RECORD KEY               IS  ＥＸ施記－施術和暦年月日
001930                                                          ＥＸ施記－患者コード
001931                             ALTERNATE RECORD KEY     IS  ＥＸ施記－患者コード
001932                                                          ＥＸ施記－施術和暦年月日
001933                             FILE STATUS              IS  状態キー
001934                             LOCK        MODE         IS  AUTOMATIC.
001939     SELECT  ＥＸ料金マスタ      ASSIGN      TO        RYOUKINL
001940                             ORGANIZATION             IS  INDEXED
001941                             ACCESS MODE              IS  DYNAMIC
001942                             RECORD KEY               IS  ＥＸ料－区分コード
001943                                                          ＥＸ料－部位コード
001944                                                          ＥＸ料－開始和暦年月
001945                             FILE STATUS              IS  状態キー
001946                             LOCK        MODE         IS  AUTOMATIC.
001947     SELECT  ＥＸ市町村マスタ    ASSIGN      TO        SITYOSNL
001948                             ORGANIZATION             IS  INDEXED
001949                             ACCESS MODE              IS  DYNAMIC
001950                             RECORD KEY               IS  ＥＸ市－公費種別
001951                                                          ＥＸ市－市町村番号
001952                             ALTERNATE RECORD KEY     IS  ＥＸ市－公費種別
001953                                                          ＥＸ市－市町村名称
001954                                                          ＥＸ市－市町村番号
001955                             FILE STATUS              IS  状態キー
001956                             LOCK        MODE         IS  AUTOMATIC.
001957     SELECT  ＥＸ計算マスタ      ASSIGN      TO        KEISANL
001958                             ORGANIZATION             IS  INDEXED
001959                             ACCESS MODE              IS  DYNAMIC
001960                             RECORD KEY               IS  ＥＸ計－制御区分
001961                                                          ＥＸ計－開始和暦年月
001962                             FILE STATUS              IS  状態キー
001963                             LOCK        MODE         IS  AUTOMATIC.
001966     SELECT  ＥＸ老人負担区分マスタ    ASSIGN      TO      ROUJINHL
001967                             ORGANIZATION             IS  INDEXED
001968                             ACCESS MODE              IS  DYNAMIC
001969                             RECORD KEY               IS  ＥＸ老負－保険種別
001970                                                          ＥＸ老負－開始和暦年月
001971                             FILE STATUS              IS  状態キー
001972                             LOCK        MODE         IS  AUTOMATIC.
001978     SELECT  ＥＸ制御情報マスタ  ASSIGN      TO        SEIGYOL
001979                             ORGANIZATION             IS  INDEXED
001980                             ACCESS MODE              IS  DYNAMIC
001981                             RECORD KEY               IS  ＥＸ制－制御区分
001982                             FILE STATUS              IS  状態キー
001983                             LOCK        MODE         IS  AUTOMATIC.
001998     SELECT  ＥＸ保険者特別負担マスタ   ASSIGN      TO        HOKENTKL
001999                                    ORGANIZATION          IS  INDEXED
002000                                    ACCESS MODE           IS  DYNAMIC
002001                                    RECORD KEY            IS  ＥＸ保特－保険種別
002002                                                              ＥＸ保特－保険者番号
002003                                                              ＥＸ保特－開始和暦年月
002004                                    FILE STATUS           IS  状態キー
002005                                    LOCK        MODE      IS  AUTOMATIC.
000250     SELECT  確認施術記録Ｆ      ASSIGN      TO        SEKIROKL
000260                             ORGANIZATION             IS  INDEXED
000270                             ACCESS MODE              IS  DYNAMIC
000280                             RECORD KEY           IS 確認施記－施術和暦年月日
000290                                                     確認施記－患者コード
000300                             ALTERNATE RECORD KEY IS 確認施記－患者コード
000310                                                     確認施記－施術和暦年月日
000320                             FILE STATUS              IS  状態キー
000330                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT  参照レセプトＦ      ASSIGN      TO        RECEPTL
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  参照レセ－施術和暦年月
001560                                                          参照レセ－患者コード
001570                                                          参照レセ－レセ種別
001580                             ALTERNATE RECORD KEY     IS  参照レセ－患者コード
001590                                                          参照レセ－施術和暦年月
001600                                                          参照レセ－レセ種別
001610                             ALTERNATE RECORD KEY     IS  参照レセ－請求和暦年月
001620                                                          参照レセ－施術和暦年月
001630                                                          参照レセ－患者コード
001640                                                          参照レセ－レセ種別
001650                             ALTERNATE RECORD KEY     IS  参照レセ－請求和暦年月
001660                                                          参照レセ－レセ種別
001670                                                          参照レセ－請求保険者番号
001680                                                          参照レセ－患者コード
001690                                                          参照レセ－施術和暦年月
001700                             ALTERNATE RECORD KEY     IS  参照レセ－請求和暦年月
001710                                                          参照レセ－請求保険者番号
001720                                                          参照レセ－患者コード
001730                                                          参照レセ－レセ種別
001740                                                          参照レセ－施術和暦年月
001750                             FILE STATUS              IS  状態キー
001760                             LOCK        MODE         IS  AUTOMATIC.
000962     SELECT  参照負傷データＦ    ASSIGN      TO        HUSYOUL
000963                             ORGANIZATION             IS  INDEXED
000964                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  参照負－施術和暦年月
000980                                                          参照負－患者コード
000990                             ALTERNATE RECORD KEY     IS  参照負－患者コード
001000                                                          参照負－施術和暦年月
001010                             FILE STATUS              IS  状態キー
001020                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT  枝番参照レセプトＦ      ASSIGN      TO        RECEPTL
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  枝番参照レセ－施術和暦年月
001560                                                          枝番参照レセ－患者コード
001570                                                          枝番参照レセ－レセ種別
001580                             ALTERNATE RECORD KEY     IS  枝番参照レセ－患者コード
001590                                                          枝番参照レセ－施術和暦年月
001600                                                          枝番参照レセ－レセ種別
001610                             ALTERNATE RECORD KEY     IS  枝番参照レセ－請求和暦年月
001620                                                          枝番参照レセ－施術和暦年月
001630                                                          枝番参照レセ－患者コード
001640                                                          枝番参照レセ－レセ種別
001650                             ALTERNATE RECORD KEY     IS  枝番参照レセ－請求和暦年月
001660                                                          枝番参照レセ－レセ種別
001670                                                          枝番参照レセ－請求保険者番号
001680                                                          枝番参照レセ－患者コード
001690                                                          枝番参照レセ－施術和暦年月
001700                             ALTERNATE RECORD KEY     IS  枝番参照レセ－請求和暦年月
001710                                                          枝番参照レセ－請求保険者番号
001720                                                          枝番参照レセ－患者コード
001730                                                          枝番参照レセ－レセ種別
001740                                                          枝番参照レセ－施術和暦年月
001750                             FILE STATUS              IS  状態キー
001760                             LOCK        MODE         IS  AUTOMATIC.
000962     SELECT  枝番参照負傷データＦ    ASSIGN      TO        HUSYOUL
000963                             ORGANIZATION             IS  INDEXED
000964                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  枝番参照負－施術和暦年月
000980                                                          枝番参照負－患者コード
000990                             ALTERNATE RECORD KEY     IS  枝番参照負－患者コード
001000                                                          枝番参照負－施術和暦年月
001010                             FILE STATUS              IS  状態キー
001020                             LOCK        MODE         IS  AUTOMATIC.
000931*
000932     SELECT  長頻参照受診者情報Ｆ    ASSIGN      TO        JUSINJL
000933                             ORGANIZATION             IS  INDEXED
000934                             ACCESS MODE              IS  DYNAMIC
000935                             RECORD KEY               IS  長頻受－施術和暦年月
000936                                                          長頻受－患者コード
000937                             ALTERNATE RECORD KEY     IS  長頻受－施術和暦年月
000938                                                          長頻受－患者カナ
000939                                                          長頻受－患者コード
000940                             ALTERNATE RECORD KEY     IS  長頻受－患者コード
000941                                                          長頻受－施術和暦年月
000942                             ALTERNATE RECORD KEY     IS  長頻受－施術和暦年月
000943                                                          長頻受－保険種別
000944                                                          長頻受－保険者番号
000945                                                          長頻受－患者コード
000946                             ALTERNATE RECORD KEY     IS  長頻受－施術和暦年月
000947                                                          長頻受－公費種別
000948                                                          長頻受－費用負担者番号
000949                                                          長頻受－患者コード
000950                             ALTERNATE RECORD KEY     IS  長頻受－施術和暦年月
000951                                                          長頻受－助成種別
000952                                                          長頻受－費用負担者番号助成
000953                                                          長頻受－患者コード
000954                             ALTERNATE RECORD KEY     IS  長頻受－請求和暦年月
000955                                                          長頻受－施術和暦年月
000956                                                          長頻受－患者コード
000957                             FILE STATUS              IS  状態キー
000958                             LOCK        MODE         IS  AUTOMATIC.
000370*
001360     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\yawobj\TEMP\W0210L.DAT"
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS                   IS  DYNAMIC
001390                             RECORD      KEY          IS  
000200                                                          作１－患者番号
000330                                                          作１－部位コード
000390                                                          作１－負傷和暦年月日
000490                                                          作１－開始和暦年月日
000140                                                          作１－施術和暦年月
001580                                                          作１－保険者番号
001490                             FILE        STATUS       IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
000959**
001770**
002006**
002010*--------------------------------------------------------------------------------------*
002011*
002012******************************************************************
002013*                      DATA DIVISION                             *
002014******************************************************************
002015 DATA                    DIVISION.
002016 FILE                    SECTION.
002017*                           ［ＲＬ＝  ２５６］
002018 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002019     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002020*                           ［ＲＬ＝  ３２０］
002021 FD  料金マスタ          BLOCK   CONTAINS   1   RECORDS.
002022     COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
002023     COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
002024     COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
002025     COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
002026     COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
002027     COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
002030     COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
002040*                           ［ＲＬ＝  ２５６］
002050 FD  計算マスタ          BLOCK   CONTAINS   1   RECORDS.
002060     COPY KEISAN          OF  XFDLIB  JOINING   計   AS  PREFIX.
002070     COPY KEISANA         OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
002080**                           ［ＲＬ＝  １２８］
002090* FD  計算ＰＧマスタ       BLOCK   CONTAINS   1   RECORDS.
002100*     COPY KEISAPG    OF  XFDLIB  JOINING   計Ｐ AS  PREFIX.
002110*                           ［ＲＬ＝  ３２０］
002120 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002130     COPY JUSINJ     OF  XFDLIB  JOINING   受   AS  PREFIX.
002131*
002132 FD  参照受診者情報Ｆ    BLOCK   CONTAINS   1   RECORDS.
002133     COPY JUSINJ     OF  XFDLIB  JOINING   参照受   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002160     COPY HUSYOU     OF  XFDLIB  JOINING   負   AS  PREFIX.
002170*                           ［ＲＬ＝  ２５６］
002180 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002190     COPY SEIGYO     OF  XFDLIB  JOINING   制     AS  PREFIX.
           COPY SEIGYO02   OF  XFDLIB  JOINING   制０２ AS  PREFIX.
           COPY SEIGYO01   OF  XFDLIB  JOINING   制０１ AS  PREFIX.
002200*                           ［ＲＬ＝  １２８］
002210 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002220     COPY GENGOU     OF  XFDLIB  JOINING   元   AS  PREFIX.
002230*
002260*/0101老人負担区分
002270 FD  老人負担区分マスタ      BLOCK   CONTAINS   1   RECORDS.
002280     COPY ROUJINH    OF  XFDLIB  JOINING   老負   AS  PREFIX.
002290*                           ［ＲＬ＝  ２５６］
002300 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002310     COPY SITYOSN    OF  XFDLIB  JOINING   市   AS  PREFIX.
002320*
002330 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
002340     COPY KAIKEI     OF  XFDLIB  JOINING   会   AS  PREFIX.
002350*
002360 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
002370     COPY RECEPT     OF  XFDLIB  JOINING   レセ   AS  PREFIX.
002380*
002390 FD  保険者特別負担マスタ  BLOCK   CONTAINS   1   RECORDS.
002400     COPY HOKENTK    OF  XFDLIB  JOINING   保特 AS PREFIX.
002401*
002402 FD  会計領収Ｆ     BLOCK   CONTAINS   1   RECORDS.
002403     COPY RYOSYU     OF  XFDLIB  JOINING   領  AS  PREFIX.
002410*
001760 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002411*--------------------------------------------------------------------------------------*
002412*--------------------------------------------------------------------------------------*
002413* EXTERNAL用
002414*
002418 FD  ＥＸ保険者マスタ IS EXTERNAL       BLOCK   CONTAINS   1   RECORDS.
002419     COPY HOKENS     OF  XFDLIB  JOINING   ＥＸ保   AS  PREFIX.
002420*
002421 FD  ＥＸ負担率マスタ IS EXTERNAL       BLOCK   CONTAINS   1   RECORDS.
002422     COPY HUTANRI    OF  XFDLIB  JOINING   ＥＸ負率 AS  PREFIX.
002423*
002424 FD  ＥＸ受診者情報Ｆ IS EXTERNAL       BLOCK   CONTAINS   1   RECORDS.
002425     COPY JUSINJ     OF  XFDLIB  JOINING   ＥＸ受   AS  PREFIX.
002426*
002430 FD  ＥＸ施術記録Ｆ  IS EXTERNAL        BLOCK   CONTAINS   1   RECORDS.
002431     COPY SEKIROK    OF  XFDLIB  JOINING   ＥＸ施記 AS  PREFIX.
002432*
002433 FD  ＥＸ料金マスタ  IS EXTERNAL        BLOCK   CONTAINS   1   RECORDS.
002434     COPY RYOUKIN    OF  XFDLIB  JOINING ＥＸ料     AS  PREFIX.
002435     COPY RYOUKNA    OF  XFDLIB  JOINING ＥＸ料Ａ   AS  PREFIX.
002436     COPY RYOUKNB    OF  XFDLIB  JOINING ＥＸ料Ｂ   AS  PREFIX.
002437     COPY RYOUKNC    OF  XFDLIB  JOINING ＥＸ料Ｃ   AS  PREFIX.
002438     COPY RYOUKND    OF  XFDLIB  JOINING ＥＸ料Ｄ   AS  PREFIX.
002439     COPY RYOUKNE    OF  XFDLIB  JOINING ＥＸ料Ｅ   AS  PREFIX.
002440     COPY RYOUKNF    OF  XFDLIB  JOINING ＥＸ料Ｆ   AS  PREFIX.
002441*
002442 FD  ＥＸ計算マスタ  IS EXTERNAL        BLOCK   CONTAINS   1   RECORDS.
002443     COPY KEISAN     OF  XFDLIB  JOINING   ＥＸ計   AS  PREFIX.
002444     COPY KEISANA    OF  XFDLIB  JOINING   ＥＸ計Ａ AS  PREFIX.
002445*
002446 FD  ＥＸ市町村マスタ IS EXTERNAL       BLOCK   CONTAINS   1   RECORDS.
002447     COPY SITYOSN    OF  XFDLIB  JOINING   ＥＸ市   AS  PREFIX.
002448*
002449 FD  ＥＸ制御情報マスタ IS EXTERNAL         BLOCK   CONTAINS   1   RECORDS.
002450     COPY SEIGYO     OF  XFDLIB  JOINING   ＥＸ制   AS  PREFIX.
002451*
002452 FD  ＥＸ老人負担区分マスタ IS EXTERNAL     BLOCK   CONTAINS   1   RECORDS.
002453     COPY ROUJINH    OF  XFDLIB  JOINING   ＥＸ老負   AS  PREFIX.
002454*
002455 FD  ＥＸ保険者特別負担マスタ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002456     COPY HOKENTK    OF  XFDLIB  JOINING   ＥＸ保特 AS PREFIX.
002457*
001340 FD  確認施術記録Ｆ  BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEKIROK    OF  XFDLIB  JOINING   確認施記 AS  PREFIX.
002350*
002360 FD  参照レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
002370     COPY RECEPT     OF  XFDLIB  JOINING   参照レセ   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  参照負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002160     COPY HUSYOU     OF  XFDLIB  JOINING   参照負   AS  PREFIX.
001513*
002360 FD  枝番参照レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
002370     COPY RECEPT     OF  XFDLIB  JOINING   枝番参照レセ   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  枝番参照負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002160     COPY HUSYOU     OF  XFDLIB  JOINING   枝番参照負   AS  PREFIX.
002131*
002132 FD  長頻参照受診者情報Ｆ    BLOCK   CONTAINS   1   RECORDS.
002133     COPY JUSINJ     OF  XFDLIB  JOINING   長頻受   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
001350*
001870 FD  作業ファイル１ RECORD  CONTAINS 128 CHARACTERS.
001880 01  作１－レコード.
001890   03  作１－レコードキー.
000200     05 作１－患者番号                     PIC 9(6).
000330     05 作１－部位コード.
000340       07 作１－負傷種別                   PIC 9(2).
000350       07 作１－部位                       PIC 9(2).
000360       07 作１－左右区分                   PIC 9.
000370       07 作１－負傷位置番号               PIC 9(2).
000380     05 作１－負傷和暦年月日.
000390       07 作１－負傷和暦年月.
000400         09 作１－負傷和暦                 PIC 9.
000410         09 作１－負傷年月.
000420           11 作１－負傷年                 PIC 9(2).
000430           11 作１－負傷月                 PIC 9(2).
000440       07 作１－負傷日                     PIC 9(2).
000480     05 作１－開始和暦年月日.
000490       07 作１－開始和暦年月.
000500         09 作１－開始和暦                 PIC 9.
000510         09 作１－開始年月.
000520           11 作１－開始年                 PIC 9(2).
000530           11 作１－開始月                 PIC 9(2).
000540       07 作１－開始日                     PIC 9(2).
000140     05 作１－施術和暦年月.
000150       07 作１－施術和暦                   PIC 9.
000160       07 作１－施術年月.
000170         09 作１－施術年                   PIC 9(2).
000180         09 作１－施術月                   PIC 9(2).
001580     05 作１－保険者番号.
001590       07 作１－法別番号                   PIC X(2).
001600       07 作１－保番.
001610         09 作１－都道府県番号             PIC X(2).
001620         09 作１－保険番号                 PIC X(3).
001630         09 作１－検証番号                 PIC X.
001640         09 FILLER                         PIC X(2).
001890   03  作１－レコードデータ.
000690     05 作１－部位実日数                   PIC 9(2).
002464*----------------------------------------------------------------*
002465******************************************************************
002466*                WORKING-STORAGE SECTION                         *
002467******************************************************************
002468 WORKING-STORAGE         SECTION.
002470 01 キー入力                           PIC X    VALUE SPACE.
002480 01 状態キー                           PIC X(2) VALUE SPACE.
002490 01 終了フラグ                         PIC X(3) VALUE SPACE.
002500 01 終了フラグ２                       PIC X(3) VALUE SPACE.
002510 01 書込フラグ                         PIC X(4) VALUE SPACE.
002520 01 書込フラグ２                       PIC X(4) VALUE SPACE.
002530 01 書込許可フラグ                     PIC X(3) VALUE SPACE.
002540 01 年月フラグ                         PIC X(3) VALUE SPACE.
002550 01 追加フラグ                         PIC X(3) VALUE SPACE.
002560 01 行桁フラグ                         PIC X(3) VALUE SPACE.
002570 01 項目フラグ                         PIC X(3) VALUE SPACE.
002580 01 長期フラグ                         PIC X(3) VALUE SPACE.
002590 01 往療加算フラグ                     PIC X(3) VALUE SPACE.
002600 01 保険者番号Ｗ                       PIC X(6) VALUE SPACE.
002610 01 ファイル名Ｗ                       PIC N(10) VALUE SPACE.
002620 01 エラー内容Ｗ                       PIC N(18) VALUE SPACE.
002630 01 会員名Ｗ                           PIC N(16) VALUE SPACE.
002640 01 画面名称Ｗ                         PIC N(2) VALUE SPACE.
002650 01 負傷ＣＮＴ                         PIC 9(2)    VALUE ZERO.
002660 01 部位カウンタ                       PIC 9(2)    VALUE ZERO.
002670 01 部位カウンタ２                     PIC 9(2)    VALUE ZERO.
002680 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
002690 01 部位ＣＮＴ２                       PIC 9(2) VALUE ZERO.
002690 01 部位ＣＮＴ３                       PIC 9(2) VALUE ZERO.
002700 01 多部位ＣＮＴ                       PIC 9(2) VALUE ZERO.
002710 01 最大登録部位数Ｗ                   PIC 9(2) VALUE ZERO.
002720 01 請求移動キー                       PIC X(4) VALUE SPACE.
002730 01 画面移動キー                       PIC X(4) VALUE SPACE.
002740 01 処理移動キー                       PIC X(4) VALUE SPACE.
002750 01 再入力キーＷ                       PIC X(4) VALUE SPACE.
002760 01 入力状態Ｗ                         PIC X(4) VALUE SPACE.
002770 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002780 01 行Ｗ                               PIC 9(4) VALUE ZERO.
002790 01 桁Ｗ                               PIC 9(4) VALUE ZERO.
002800 01 前和暦Ｗ                           PIC 9 VALUE ZERO.
002810 01 カレント和暦Ｗ                     PIC 9 VALUE ZERO.
002820 01 入金額Ｗ                           PIC 9(6) VALUE ZERO.
002830 01 初検請求区分Ｗ                     PIC 9 VALUE ZERO.
002840 01 転帰区分Ｗ                         PIC 9 VALUE ZERO.
002850 01 保険種別Ｗ                         PIC 9(2) VALUE ZERO.
002860 01 公費種別Ｗ                         PIC 9(2) VALUE ZERO.
002870 01 助成種別Ｗ                         PIC 9(2) VALUE ZERO.
002880 01 助成種別１Ｗ                       PIC 9(2) VALUE ZERO.
002890 01 助成種別２Ｗ                       PIC 9(2) VALUE ZERO.
002900 01 老人負担回数Ｗ                     PIC 9(2) VALUE ZERO.
002910 01 助成負担計算区分Ｗ                 PIC 9 VALUE ZERO.
002920 01 助成負担金免除Ｗ                   PIC 9    VALUE ZERO.
002930*
002940 01 助成負担回数Ｗ                     PIC 9(2) VALUE ZERO.
002950*
002960 01 実行キーＷ                         PIC X(3) VALUE SPACE.
002970 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
002980*
002990*/老人保険改正の対応0101
003000 01 老人負担区分Ｗ.
003010*/定率の上限廃止、定額制廃止によりコメント↓↓↓
003020*         03 老人負担計算区分Ｗ               PIC 9    VALUE ZERO.
003030*/４１端数区分
003040*/０：１０円、１の位を四捨五入
003050*/１：１円、小数点以下を含めて足し込んだ合計の小数点以下を切り上げ。
003060   03 ４１端数区分Ｗ                   PIC 9    VALUE ZERO.
003070*
003080*/老人保険改正の対応0101
003090*       01 老人負担額上限ＡＷ                 PIC 9(5) VALUE ZERO.
003100*/４１老に準ずる助成の対応
003110 01 負担区分Ｗ                         PIC 9(2) VALUE ZERO.
003120*
003130 01 初検時間外Ｗ                       PIC 9 VALUE ZERO.
003140 01 初検休日Ｗ                         PIC 9 VALUE ZERO.
003150 01 初検深夜Ｗ                         PIC 9 VALUE ZERO.
003160*
003170 01 往療夜間Ｗ                         PIC 9 VALUE ZERO.
003180 01 往療難路Ｗ                         PIC 9 VALUE ZERO.
003190 01 往療暴風雨Ｗ                       PIC 9 VALUE ZERO.
003200 01 往療時間外Ｗ                       PIC 9 VALUE ZERO.
003210*
003220 01 金属副子大Ｗ                       PIC 9 VALUE ZERO.
003230 01 金属副子中Ｗ                       PIC 9 VALUE ZERO.
003240 01 金属副子小Ｗ                       PIC 9 VALUE ZERO.
      */金属副子合計回数/20180613
       01 金属副子回数Ｗ                     PIC 9(2) VALUE ZERO.
003250*
003260 01 往療被除数Ｗ                       PIC 9(3) VALUE ZERO.
003270 01 往療除数Ｗ                         PIC 9(3) VALUE ZERO.
003280 01 往療商Ｗ                           PIC 9(3) VALUE ZERO.
003290 01 往療剰余Ｗ                         PIC 9(3) VALUE ZERO.
003300 01 往療距離Ｗ                         PIC 9(3)V9 VALUE ZERO.
003300 01 月往療距離Ｗ                       PIC 9(3)V9 VALUE ZERO.
003310 01 往療加算率Ｗ                       PIC 9(3) VALUE ZERO.
003320*
003330 01 初検料Ｗ                           PIC 9(4) VALUE ZERO.
003340 01 初検加算料Ｗ                       PIC 9(5) VALUE ZERO.
003350 01 再検料Ｗ                           PIC 9(4) VALUE ZERO.
003360 01 往療料Ｗ                           PIC 9(6) VALUE ZERO.
003370 01 往療加算料Ｗ                       PIC 9(6) VALUE ZERO.
003380 01 後療料Ｗ                           PIC 9(5) VALUE ZERO.
003390 01 冷罨法料Ｗ                         PIC 9(4) VALUE ZERO.
003400 01 温罨法料Ｗ                         PIC 9(4) VALUE ZERO.
003410 01 電療料Ｗ                           PIC 9(4) VALUE ZERO.
003420*
003430 01 部位日計多部位Ｗ１                 PIC 9(6) VALUE ZERO.
003440 01 部位日計多部位Ｗ２                 PIC 9(6) VALUE ZERO.
003450 01 部位日計多部位Ｗ３                 PIC 9(6) VALUE ZERO.
003460 01 部位日計多部位Ｗ３１               PIC 9(6) VALUE ZERO.
003470 01 部位日計多部位Ｗ４                 PIC 9(6) VALUE ZERO.
003480 01 部位日計多部位Ｗ４１               PIC 9(6) VALUE ZERO.
003490 01 部位日計多部位Ｗ５                 PIC 9(6) VALUE ZERO.
003500 01 部位日計多部位Ｗ５１               PIC 9(6) VALUE ZERO.
003510 01 部位日計多部位Ｗ５２               PIC 9(6) VALUE ZERO.
003520 01 部位日計多部位Ｗ５３               PIC 9(6) VALUE ZERO.
      */６部位～/120807
003510 01 部位日計多部位Ｗ６２               PIC 9(6) VALUE ZERO.
003520 01 部位日計多部位Ｗ６３               PIC 9(6) VALUE ZERO.
003510 01 部位日計多部位Ｗ７２               PIC 9(6) VALUE ZERO.
003520 01 部位日計多部位Ｗ７３               PIC 9(6) VALUE ZERO.
003530*
003540 01 部位日計長期Ｗ１                   PIC 9(6) VALUE ZERO.
003550 01 部位日計長期Ｗ２                   PIC 9(6) VALUE ZERO.
003560 01 部位日計長期Ｗ３                   PIC 9(6) VALUE ZERO.
003570 01 部位日計長期Ｗ３１                 PIC 9(6) VALUE ZERO.
003580 01 部位日計長期Ｗ４                   PIC 9(6) VALUE ZERO.
003590 01 部位日計長期Ｗ４１                 PIC 9(6) VALUE ZERO.
003600 01 部位日計長期Ｗ４２                 PIC 9(6) VALUE ZERO.
003610 01 部位日計長期Ｗ５                   PIC 9(6) VALUE ZERO.
003620 01 部位日計長期Ｗ５１                 PIC 9(6) VALUE ZERO.
003630 01 部位日計長期Ｗ５２                 PIC 9(6) VALUE ZERO.
003640 01 部位日計長期Ｗ５３                 PIC 9(6) VALUE ZERO.
      */６部位～/120807
003630 01 部位日計長期Ｗ６２                 PIC 9(6) VALUE ZERO.
003640 01 部位日計長期Ｗ６３                 PIC 9(6) VALUE ZERO.
003630 01 部位日計長期Ｗ７２                 PIC 9(6) VALUE ZERO.
003640 01 部位日計長期Ｗ７３                 PIC 9(6) VALUE ZERO.
003650*
003660 01 部位日計Ｗ１                       PIC 9(6) VALUE ZERO.
003670 01 部位日計Ｗ２                       PIC 9(6) VALUE ZERO.
003680 01 部位日計Ｗ３                       PIC 9(6) VALUE ZERO.
003690 01 部位日計Ｗ３１                     PIC 9(6) VALUE ZERO.
003700 01 部位日計Ｗ４                       PIC 9(6) VALUE ZERO.
003710 01 部位日計Ｗ４１                     PIC 9(6) VALUE ZERO.
003720 01 部位日計Ｗ４２                     PIC 9(6) VALUE ZERO.
003730 01 部位日計Ｗ５                       PIC 9(6) VALUE ZERO.
003740 01 部位日計Ｗ５１                     PIC 9(6) VALUE ZERO.
003750 01 部位日計Ｗ５２                     PIC 9(6) VALUE ZERO.
003760 01 部位日計Ｗ５３                     PIC 9(6) VALUE ZERO.
      */６部位～/120807
003750 01 部位日計Ｗ６２                     PIC 9(6) VALUE ZERO.
003760 01 部位日計Ｗ６３                     PIC 9(6) VALUE ZERO.
003750 01 部位日計Ｗ７２                     PIC 9(6) VALUE ZERO.
003760 01 部位日計Ｗ７３                     PIC 9(6) VALUE ZERO.
003770*
003780 01 部位合計多部位Ｗ１                 PIC 9(6) VALUE ZERO.
003790 01 部位合計多部位Ｗ２                 PIC 9(6) VALUE ZERO.
003800 01 部位合計多部位Ｗ３                 PIC 9(6) VALUE ZERO.
003810 01 部位合計多部位Ｗ３１               PIC 9(6) VALUE ZERO.
003820 01 部位合計多部位Ｗ４                 PIC 9(6) VALUE ZERO.
003830 01 部位合計多部位Ｗ４１               PIC 9(6) VALUE ZERO.
003840 01 部位合計多部位Ｗ５                 PIC 9(6) VALUE ZERO.
003850 01 部位合計多部位Ｗ５１               PIC 9(6) VALUE ZERO.
003860 01 部位合計多部位Ｗ５２               PIC 9(6) VALUE ZERO.
003870 01 部位合計多部位Ｗ５３               PIC 9(6) VALUE ZERO.
      */６部位～/120804
003860 01 部位合計多部位Ｗ６２               PIC 9(6) VALUE ZERO.
003870 01 部位合計多部位Ｗ６３               PIC 9(6) VALUE ZERO.
003860 01 部位合計多部位Ｗ７２               PIC 9(6) VALUE ZERO.
003870 01 部位合計多部位Ｗ７３               PIC 9(6) VALUE ZERO.
003880*
003890 01 部位合計長期Ｗ１                   PIC 9(6) VALUE ZERO.
003900 01 部位合計長期Ｗ２                   PIC 9(6) VALUE ZERO.
003910 01 部位合計長期Ｗ３                   PIC 9(6) VALUE ZERO.
003920 01 部位合計長期Ｗ３１                 PIC 9(6) VALUE ZERO.
003930 01 部位合計長期Ｗ４                   PIC 9(6) VALUE ZERO.
003940 01 部位合計長期Ｗ４１                 PIC 9(6) VALUE ZERO.
003950 01 部位合計長期Ｗ４２                 PIC 9(6) VALUE ZERO.
003960 01 部位合計長期Ｗ５                   PIC 9(6) VALUE ZERO.
003970 01 部位合計長期Ｗ５１                 PIC 9(6) VALUE ZERO.
003980 01 部位合計長期Ｗ５２                 PIC 9(6) VALUE ZERO.
003990 01 部位合計長期Ｗ５３                 PIC 9(6) VALUE ZERO.
      */６部位～/120804
003980 01 部位合計長期Ｗ６２                 PIC 9(6) VALUE ZERO.
003990 01 部位合計長期Ｗ６３                 PIC 9(6) VALUE ZERO.
003980 01 部位合計長期Ｗ７２                 PIC 9(6) VALUE ZERO.
003990 01 部位合計長期Ｗ７３                 PIC 9(6) VALUE ZERO.
004000*
004010 01 部位合計Ｗ１                       PIC 9(6) VALUE ZERO.
004020 01 部位合計Ｗ２                       PIC 9(6) VALUE ZERO.
004030 01 部位合計Ｗ３                       PIC 9(6) VALUE ZERO.
004040 01 部位合計Ｗ３１                     PIC 9(6) VALUE ZERO.
004050 01 部位合計Ｗ４                       PIC 9(6) VALUE ZERO.
004060 01 部位合計Ｗ４１                     PIC 9(6) VALUE ZERO.
004070 01 部位合計Ｗ４２                     PIC 9(6) VALUE ZERO.
004080 01 部位合計Ｗ５                       PIC 9(6) VALUE ZERO.
004090 01 部位合計Ｗ５１                     PIC 9(6) VALUE ZERO.
004100 01 部位合計Ｗ５２                     PIC 9(6) VALUE ZERO.
004110 01 部位合計Ｗ５３                     PIC 9(6) VALUE ZERO.
      */６部位～/120804
004100 01 部位合計Ｗ６２                     PIC 9(6) VALUE ZERO.
004110 01 部位合計Ｗ６３                     PIC 9(6) VALUE ZERO.
004100 01 部位合計Ｗ７２                     PIC 9(6) VALUE ZERO.
004110 01 部位合計Ｗ７３                     PIC 9(6) VALUE ZERO.
004120*
004130 01 初回処置ＳＷ.
004140   03 初回処置ＯＷ OCCURS 9.
004150     05 初回処置料ＯＷ                 PIC 9(5) VALUE ZERO.
004160*
004170*
004180 01 後療単価ＳＷ.
004190   03 後療単価ＯＷ１ OCCURS 9.
004200*     05 後療単価ＯＷ                   PIC 9(4) OCCURS 9 VALUE ZERO.
004210     05 後療単価ＯＷ                   PIC 9(4) VALUE ZERO.
004220*
004230 01 後療料１００ＳＷ.
004240   03 後療料１００Ｗ１ OCCURS 9.
004250     05 後療料１００Ｗ                 PIC 9(5) OCCURS 9 VALUE ZERO.
004260*
004270 01 電療料１００ＳＷ.
004280   03 電療料１００Ｗ１ OCCURS 9.
004290     05 電療料１００Ｗ                 PIC 9(5) OCCURS 9 VALUE ZERO.
004300*
004310 01 冷罨法料１００ＳＷ.
004320   03 冷罨法料１００Ｗ１ OCCURS 9.
004330     05 冷罨法料１００Ｗ               PIC 9(5) OCCURS 9 VALUE ZERO.
004340*
004350 01 温罨法料１００ＳＷ.
004360   03 温罨法料１００Ｗ１ OCCURS 9.
004370     05 温罨法料１００Ｗ               PIC 9(5) OCCURS 9 VALUE ZERO.
004380*
004390 01 初検料ＧＷ                         PIC 9(5) VALUE ZERO.
004400 01 初検加算料ＧＷ                     PIC 9(6) VALUE ZERO.
004410 01 初回処置料ＧＷ                     PIC 9(6) VALUE ZERO.
004420 01 再検料ＧＷ                         PIC 9(5) VALUE ZERO.
004430 01 往療料ＧＷ                         PIC 9(6) VALUE ZERO.
004440 01 往療加算料ＧＷ                     PIC 9(6) VALUE ZERO.
004450 01 往療回数ＧＷ                       PIC 9(2) VALUE ZERO.
004460 01 後療料ＧＷ                         PIC 9(6) VALUE ZERO.
004470 01 冷罨法料ＧＷ                       PIC 9(4) VALUE ZERO.
004480 01 温罨法料ＧＷ                       PIC 9(4) VALUE ZERO.
004490 01 電療料ＧＷ                         PIC 9(4) VALUE ZERO.
004500 01 金属副子料ＧＷ                     PIC 9(5) VALUE ZERO.
004510 01 施術情報提供料ＧＷ                 PIC 9(5) VALUE ZERO.
004520*
004530 01 負担額ＧＷ                         PIC 9(5) VALUE ZERO.
004540 01 費用額老人ＧＷ                     PIC 9(6) VALUE ZERO.
004550 01 負担額老人ＧＷ                     PIC 9(5) VALUE ZERO.
004560 01 請求額老人ＧＷ                     PIC 9(6) VALUE ZERO.
004570 01 負担額助成ＧＷ                     PIC 9(6) VALUE ZERO.
004580 01 請求額助成ＧＷ                     PIC 9(6) VALUE ZERO.
004590*
004600*/計算ＰＧマスタを使用しない
004610* 01 費用額ＰＧＷ                       PIC X(8) VALUE SPACE.
004620* 01 負担額ＰＧＷ                       PIC X(8) VALUE SPACE.
004630* 01 負担額ＰＧ老人Ｗ                   PIC X(8) VALUE SPACE.
004640* 01 負担額ＰＧ助成Ｗ                   PIC X(8) VALUE SPACE.
004650*
004660 01 後療料ＴＷ                         PIC 9(5) VALUE ZERO.
004670 01 冷罨法料ＴＷ                       PIC 9(4) VALUE ZERO.
004680 01 温罨法料ＴＷ                       PIC 9(4) VALUE ZERO.
004690 01 電療料ＴＷ                         PIC 9(4) VALUE ZERO.
004700 01 金属副子料Ｗ                       PIC 9(4) VALUE ZERO.
004710 01 初回処置料Ｗ                       PIC 9(5) VALUE ZERO.
004720 01 施術情報提供料Ｗ                   PIC 9(4) VALUE ZERO.
004730*
004740 01 後療料Ｗ１                         PIC 9(5) VALUE ZERO.
004750 01 冷罨法料Ｗ１                       PIC 9(4) VALUE ZERO.
004760 01 温罨法料Ｗ１                       PIC 9(4) VALUE ZERO.
004770 01 電療料Ｗ１                         PIC 9(4) VALUE ZERO.
004780*
004790 01 西暦年Ｗ                           PIC 9(4) VALUE ZERO.
004800 01 西暦年差Ｗ                         PIC 9(4) VALUE ZERO.
004810 01 開始年月絶対値Ｗ                   PIC 9(6) VALUE ZERO.
004820 01 施術年月絶対値Ｗ                   PIC 9(6) VALUE ZERO.
004830 01 施術年月差Ｗ                       PIC 9(4) VALUE ZERO.
004840*
004850 01 部位数Ｗ                           PIC 9(2) VALUE ZERO.
004860 01 継続部位数Ｗ                       PIC 9(2) VALUE ZERO.
004870*
004880* 計算情報ワーク
004890* 基本
004900 01 往療基本距離ＫＷ                   PIC 9(2) VALUE ZERO.
004910 01 後療可能算定数ＫＷ                 PIC 9(2) VALUE ZERO.
004920*
004930 01 逓減率ＳＫＷ.
004940   03 逓減率ＫＷ  OCCURS 9.
004950     05 多部位逓減率ＫＷ               PIC 9(3) VALUE ZERO.
004960*
004970 01 部位逓減ＳＷ.
004980   03 部位逓減Ｗ  OCCURS 9.
004990     05 部位逓減情報Ｗ                 PIC 9(2) VALUE ZERO.
005000*
005010* 01 長期逓減ＳＷ.
005020*   03 長期区分Ｗ１ OCCURS 9.
005030*     05 長期区分Ｗ                     PIC 9(2) OCCURS 9 VALUE ZERO.
005040*
005050 01 長期区分ＳＷ.
005060    03 長期区分Ｗ                      PIC 9(2) OCCURS 9 VALUE ZERO.
005070*
005080 01 往療加算率中間Ｗ                   PIC 9V9(3) VALUE ZERO.
005090*
005100 01 長期逓減率中間ＫＷ                 PIC 9V9(3) VALUE ZERO.
005110 01 長期逓減率ＫＷ                     PIC 9(3) VALUE ZERO.
005120 01 長期逓減月境界日ＫＷ               PIC 9(2) VALUE ZERO.
005130 01 長期逓減月数ＫＷ                   PIC 9(2) VALUE ZERO.
005140 01 老人負担回数ＫＷ                   PIC 9(2) VALUE ZERO.
005150 01 逓減率中間結果Ｗ                   PIC 9V9(3) VALUE ZERO.
       01 長期頻回逓減率ＫＷ                 PIC 9(3) VALUE ZERO.
005160* 料金情報ワーク
005170* 基本
005180 01 初検料ＡＷ                         PIC 9(4) VALUE ZERO.
005190 01 時間外ＡＷ                         PIC 9(4) VALUE ZERO.
005200 01 休日ＡＷ                           PIC 9(4) VALUE ZERO.
005210 01 深夜ＡＷ                           PIC 9(4) VALUE ZERO.
005220 01 往療基本料金ＡＷ                   PIC 9(4) VALUE ZERO.
005230 01 往療追加料金ＡＷ                   PIC 9(4) VALUE ZERO.
005240 01 往療加算率ＡＷ                     PIC 9(3) VALUE ZERO.
005250 01 往療深夜加算率ＡＷ                 PIC 9(3) VALUE ZERO.
005260 01 往療難路加算率ＡＷ                 PIC 9(3) VALUE ZERO.
005270 01 往療暴風雨加算率ＡＷ               PIC 9(3) VALUE ZERO.
005280 01 再検料ＡＷ                         PIC 9(4) VALUE ZERO.
005290 01 冷罨法料ＡＷ                       PIC 9(4) VALUE ZERO.
005300 01 温罨法料ＡＷ                       PIC 9(4) VALUE ZERO.
005310 01 電療料ＡＷ                         PIC 9(4) VALUE ZERO.
005320 01 大ＡＷ                             PIC 9(4) VALUE ZERO.
005330 01 中ＡＷ                             PIC 9(4) VALUE ZERO.
005340 01 小ＡＷ                             PIC 9(4) VALUE ZERO.
005350 01 施術情報提供料ＡＷ                 PIC 9(4) VALUE ZERO.
005360*/初検時相談料/080527
005370 01 初検時相談料ＧＷ                   PIC 9(5) VALUE ZERO.
005380 01 初検時相談料Ｗ                     PIC 9(4) VALUE ZERO.
005390 01 初検時相談料ＡＷ                   PIC 9(4) VALUE ZERO.
005400* 負傷別
005410 01 拘縮後療料ＢＷ                     PIC 9(5) VALUE ZERO.
005420 01 後療料ＢＷ                         PIC 9(5) VALUE ZERO.
005430 01 整復料ＢＷ                         PIC 9(5) VALUE ZERO.
005440*
005450 01 部位コードＷ.
005460    03 負傷種別Ｗ                      PIC 9(2).
005470    03 部位Ｗ                          PIC 9(2).
005480    03 左右区分Ｗ                      PIC 9.
005490    03 負傷位置番号Ｗ                  PIC 9(2).
005500*
005510 01 部位コードＣＷ.
005520    03 負傷種別ＣＷ                    PIC 9(2).
005530    03 部位ＣＷ                        PIC 9(2).
005540*
005550 01 患者コードＷ.
005560     03 患者番号Ｗ                     PIC 9(6).
005570     03 枝番Ｗ                         PIC X.
005580*
005590 01 患者コードＣＷ.
005600     03 患者番号ＣＷ                   PIC 9(6).
005610     03 枝番ＣＷ                       PIC X.
005620*
005630 01 施術和暦年月日ＣＷ.
005640   03 施術和暦年月ＣＷ.
005650     05 施術和暦ＣＷ                   PIC 9.
005660     05 施術年月ＣＷ.
005670        07 施術年ＣＷ                  PIC 9(2).
005680        07 施術月ＣＷ                  PIC 9(2).
005690   03 施術日ＣＷ                       PIC 9(2).
005700*
005710 01 施術和暦年月日Ｗ.
005720   03 施術和暦年月Ｗ.
005730     05 施術和暦Ｗ                     PIC 9.
005740     05 施術年月Ｗ.
005750        07 施術年Ｗ                    PIC 9(2).
005760        07 施術月Ｗ                    PIC 9(2).
005770   03 施術日Ｗ                         PIC 9(2).
005780*
005790 01 開始和暦年月日Ｗ.
005800   03 開始和暦年月Ｗ.
005810     05 開始和暦Ｗ                     PIC 9.
005820     05 開始年月Ｗ.
005830        07 開始年Ｗ                    PIC 9(2).
005840        07 開始月Ｗ                    PIC 9(2).
005850   03 開始日Ｗ                         PIC 9(2).
005860*
005870 01 終了和暦年月日Ｗ.
005880   03 終了和暦年月Ｗ.
005890     05 終了和暦Ｗ                     PIC 9.
005900     05 終了年月Ｗ.
005910        07 終了年Ｗ                    PIC 9(2).
005920        07 終了月Ｗ                    PIC 9(2).
005930   03 終了日Ｗ                         PIC 9(2).
005940*
005950* 部位毎の多部位逓減部位毎のワーク
005960* １部位目
005970 01 後療回数１ＲＷ                     PIC 9(2) VALUE ZERO.
005980 01 後療料１ＲＷ                       PIC 9(5) VALUE ZERO.
005990 01 冷罨法回数１ＲＷ                   PIC 9(2) VALUE ZERO.
006000 01 冷罨法料１ＲＷ                     PIC 9(4) VALUE ZERO.
006010 01 温罨法回数１ＲＷ                   PIC 9(2) VALUE ZERO.
006020 01 温罨法料１ＲＷ                     PIC 9(4) VALUE ZERO.
006030 01 電療回数１ＲＷ                     PIC 9(2) VALUE ZERO.
006040 01 電療料１ＲＷ                       PIC 9(4) VALUE ZERO.
006050 01 小計１ＲＷ                         PIC 9(6) VALUE ZERO.
006060* ２部位目
006070 01 後療回数２ＲＷ                     PIC 9(2) VALUE ZERO.
006080 01 後療料２ＲＷ                       PIC 9(5) VALUE ZERO.
006090 01 冷罨法回数２ＲＷ                   PIC 9(2) VALUE ZERO.
006100 01 冷罨法料２ＲＷ                     PIC 9(4) VALUE ZERO.
006110 01 温罨法回数２ＲＷ                   PIC 9(2) VALUE ZERO.
006120 01 温罨法料２ＲＷ                     PIC 9(4) VALUE ZERO.
006130 01 電療回数２ＲＷ                     PIC 9(2) VALUE ZERO.
006140 01 電療料２ＲＷ                       PIC 9(4) VALUE ZERO.
006150 01 小計２ＲＷ                         PIC 9(6) VALUE ZERO.
006160* ３部位目
006170 01 後療回数３ＲＷ                     PIC 9(2) VALUE ZERO.
006180 01 後療料３ＲＷ                       PIC 9(5) VALUE ZERO.
006190 01 冷罨法回数３ＲＷ                   PIC 9(2) VALUE ZERO.
006200 01 冷罨法料３ＲＷ                     PIC 9(4) VALUE ZERO.
006210 01 温罨法回数３ＲＷ                   PIC 9(2) VALUE ZERO.
006220 01 温罨法料３ＲＷ                     PIC 9(4) VALUE ZERO.
006230 01 電療回数３ＲＷ                     PIC 9(2) VALUE ZERO.
006240 01 電療料３ＲＷ                       PIC 9(4) VALUE ZERO.
006250 01 小計３ＲＷ                         PIC 9(6) VALUE ZERO.
006260* ３部位目（繰上り）
006270 01 後療回数３１ＲＷ                   PIC 9(2) VALUE ZERO.
006280 01 後療料３１ＲＷ                     PIC 9(5) VALUE ZERO.
006290 01 冷罨法回数３１ＲＷ                 PIC 9(2) VALUE ZERO.
006300 01 冷罨法料３１ＲＷ                   PIC 9(4) VALUE ZERO.
006310 01 温罨法回数３１ＲＷ                 PIC 9(2) VALUE ZERO.
006320 01 温罨法料３１ＲＷ                   PIC 9(4) VALUE ZERO.
006330 01 電療回数３１ＲＷ                   PIC 9(2) VALUE ZERO.
006340 01 電療料３１ＲＷ                     PIC 9(4) VALUE ZERO.
006350 01 小計３１ＲＷ                       PIC 9(6) VALUE ZERO.
006360 01 逓減開始和暦年月日３１ＲＷ.
006370   03 逓減開始和暦年月３１ＲＷ.
006380     05 逓減開始和暦３１ＲＷ           PIC 9 VALUE ZERO.
006390     05 逓減開始年月３１ＲＷ.
006400       07 逓減開始年３１ＲＷ           PIC 9(2) VALUE ZERO.
006410       07 逓減開始月３１ＲＷ           PIC 9(2) VALUE ZERO.
006420   03 逓減開始日３１ＲＷ               PIC 9(2) VALUE ZERO.
006430* ４部位目
006440 01 後療回数４ＲＷ                     PIC 9(2) VALUE ZERO.
006450 01 後療料４ＲＷ                       PIC 9(5) VALUE ZERO.
006460 01 冷罨法回数４ＲＷ                   PIC 9(2) VALUE ZERO.
006470 01 冷罨法料４ＲＷ                     PIC 9(4) VALUE ZERO.
006480 01 温罨法回数４ＲＷ                   PIC 9(2) VALUE ZERO.
006490 01 温罨法料４ＲＷ                     PIC 9(4) VALUE ZERO.
006500 01 電療回数４ＲＷ                     PIC 9(2) VALUE ZERO.
006510 01 電療料４ＲＷ                       PIC 9(4) VALUE ZERO.
006520 01 小計４ＲＷ                         PIC 9(6) VALUE ZERO.
006530* ４部位目（繰上り１）
006540 01 後療回数４１ＲＷ                   PIC 9(2) VALUE ZERO.
006550 01 後療料４１ＲＷ                     PIC 9(5) VALUE ZERO.
006560 01 冷罨法回数４１ＲＷ                 PIC 9(2) VALUE ZERO.
006570 01 冷罨法料４１ＲＷ                   PIC 9(4) VALUE ZERO.
006580 01 温罨法回数４１ＲＷ                 PIC 9(2) VALUE ZERO.
006590 01 温罨法料４１ＲＷ                   PIC 9(4) VALUE ZERO.
006600 01 電療回数４１ＲＷ                   PIC 9(2) VALUE ZERO.
006610 01 電療料４１ＲＷ                     PIC 9(4) VALUE ZERO.
006620 01 小計４１ＲＷ                       PIC 9(6) VALUE ZERO.
006630 01 逓減開始和暦年月日４１ＲＷ.
006640   03 逓減開始和暦年月４１ＲＷ.
006650     05 逓減開始和暦４１ＲＷ           PIC 9 VALUE ZERO.
006660     05 逓減開始年月４１ＲＷ.
006670       07 逓減開始年４１ＲＷ           PIC 9(2) VALUE ZERO.
006680       07 逓減開始月４１ＲＷ           PIC 9(2) VALUE ZERO.
006690   03 逓減開始日４１ＲＷ               PIC 9(2) VALUE ZERO.
006700* ４部位目（繰上り２）
006710 01 後療回数４２ＲＷ                   PIC 9(2) VALUE ZERO.
006720 01 後療料４２ＲＷ                     PIC 9(5) VALUE ZERO.
006730 01 冷罨法回数４２ＲＷ                 PIC 9(2) VALUE ZERO.
006740 01 冷罨法料４２ＲＷ                   PIC 9(4) VALUE ZERO.
006750 01 温罨法回数４２ＲＷ                 PIC 9(2) VALUE ZERO.
006760 01 温罨法料４２ＲＷ                   PIC 9(4) VALUE ZERO.
006770 01 電療回数４２ＲＷ                   PIC 9(2) VALUE ZERO.
006780 01 電療料４２ＲＷ                     PIC 9(4) VALUE ZERO.
006790 01 小計４２ＲＷ                       PIC 9(6) VALUE ZERO.
006800 01 逓減開始和暦年月日４２ＲＷ.
006810   03 逓減開始和暦年月４２ＲＷ.
006820     05 逓減開始和暦４２ＲＷ           PIC 9 VALUE ZERO.
006830     05 逓減開始年月４２ＲＷ.
006840       07 逓減開始年４２ＲＷ           PIC 9(2) VALUE ZERO.
006850       07 逓減開始月４２ＲＷ           PIC 9(2) VALUE ZERO.
006860   03 逓減開始日４２ＲＷ               PIC 9(2) VALUE ZERO.
006870* ５部位目
006880 01 後療回数５ＲＷ                     PIC 9(2) VALUE ZERO.
006890 01 後療料５ＲＷ                       PIC 9(5) VALUE ZERO.
006900 01 冷罨法回数５ＲＷ                   PIC 9(2) VALUE ZERO.
006910 01 冷罨法料５ＲＷ                     PIC 9(4) VALUE ZERO.
006920 01 温罨法回数５ＲＷ                   PIC 9(2) VALUE ZERO.
006930 01 温罨法料５ＲＷ                     PIC 9(4) VALUE ZERO.
006940 01 電療回数５ＲＷ                     PIC 9(2) VALUE ZERO.
006950 01 電療料５ＲＷ                       PIC 9(4) VALUE ZERO.
006960 01 小計５ＲＷ                         PIC 9(6) VALUE ZERO.
006970* ５部位目（繰上り１）
006980 01 後療回数５１ＲＷ                   PIC 9(2) VALUE ZERO.
006990 01 後療料５１ＲＷ                     PIC 9(5) VALUE ZERO.
007000 01 冷罨法回数５１ＲＷ                 PIC 9(2) VALUE ZERO.
007010 01 冷罨法料５１ＲＷ                   PIC 9(4) VALUE ZERO.
007020 01 温罨法回数５１ＲＷ                 PIC 9(2) VALUE ZERO.
007030 01 温罨法料５１ＲＷ                   PIC 9(4) VALUE ZERO.
007040 01 電療回数５１ＲＷ                   PIC 9(2) VALUE ZERO.
007050 01 電療料５１ＲＷ                     PIC 9(4) VALUE ZERO.
007060 01 小計５１ＲＷ                       PIC 9(6) VALUE ZERO.
007070 01 逓減開始和暦年月日５１ＲＷ.
007080   03 逓減開始和暦年月５１ＲＷ.
007090     05 逓減開始和暦５１ＲＷ           PIC 9 VALUE ZERO.
007100     05 逓減開始年月５１ＲＷ.
007110       07 逓減開始年５１ＲＷ           PIC 9(2) VALUE ZERO.
007120       07 逓減開始月５１ＲＷ           PIC 9(2) VALUE ZERO.
007130   03 逓減開始日５１ＲＷ               PIC 9(2) VALUE ZERO.
007140* ５部位目（繰上り２）
007150 01 後療回数５２ＲＷ                   PIC 9(2) VALUE ZERO.
007160 01 後療料５２ＲＷ                     PIC 9(5) VALUE ZERO.
007170 01 冷罨法回数５２ＲＷ                 PIC 9(2) VALUE ZERO.
007180 01 冷罨法料５２ＲＷ                   PIC 9(4) VALUE ZERO.
007190 01 温罨法回数５２ＲＷ                 PIC 9(2) VALUE ZERO.
007200 01 温罨法料５２ＲＷ                   PIC 9(4) VALUE ZERO.
007210 01 電療回数５２ＲＷ                   PIC 9(2) VALUE ZERO.
007220 01 電療料５２ＲＷ                     PIC 9(4) VALUE ZERO.
007230 01 小計５２ＲＷ                       PIC 9(6) VALUE ZERO.
007240 01 逓減開始和暦年月日５２ＲＷ.
007250   03 逓減開始和暦年月５２ＲＷ.
007260     05 逓減開始和暦５２ＲＷ           PIC 9 VALUE ZERO.
007270     05 逓減開始年月５２ＲＷ.
007280       07 逓減開始年５２ＲＷ           PIC 9(2) VALUE ZERO.
007290       07 逓減開始月５２ＲＷ           PIC 9(2) VALUE ZERO.
007300   03 逓減開始日５２ＲＷ               PIC 9(2) VALUE ZERO.
007310* ５部位目（繰上り３）
007320 01 後療回数５３ＲＷ                   PIC 9(2) VALUE ZERO.
007330 01 後療料５３ＲＷ                     PIC 9(5) VALUE ZERO.
007340 01 冷罨法回数５３ＲＷ                 PIC 9(2) VALUE ZERO.
007350 01 冷罨法料５３ＲＷ                   PIC 9(4) VALUE ZERO.
007360 01 温罨法回数５３ＲＷ                 PIC 9(2) VALUE ZERO.
007370 01 温罨法料５３ＲＷ                   PIC 9(4) VALUE ZERO.
007380 01 電療回数５３ＲＷ                   PIC 9(2) VALUE ZERO.
007390 01 電療料５３ＲＷ                     PIC 9(4) VALUE ZERO.
007400 01 小計５３ＲＷ                       PIC 9(6) VALUE ZERO.
007410 01 逓減開始和暦年月日５３ＲＷ.
007420   03 逓減開始和暦年月５３ＲＷ.
007430     05 逓減開始和暦５３ＲＷ           PIC 9 VALUE ZERO.
007440     05 逓減開始年月５３ＲＷ.
007450       07 逓減開始年５３ＲＷ           PIC 9(2) VALUE ZERO.
007460       07 逓減開始月５３ＲＷ           PIC 9(2) VALUE ZERO.
007470   03 逓減開始日５３ＲＷ               PIC 9(2) VALUE ZERO.
      */６部位/120804
007140* ６部位目（繰上り２）
007150 01 後療回数６２ＲＷ                   PIC 9(2) VALUE ZERO.
007160 01 後療料６２ＲＷ                     PIC 9(5) VALUE ZERO.
007170 01 冷罨法回数６２ＲＷ                 PIC 9(2) VALUE ZERO.
007180 01 冷罨法料６２ＲＷ                   PIC 9(4) VALUE ZERO.
007190 01 温罨法回数６２ＲＷ                 PIC 9(2) VALUE ZERO.
007200 01 温罨法料６２ＲＷ                   PIC 9(4) VALUE ZERO.
007210 01 電療回数６２ＲＷ                   PIC 9(2) VALUE ZERO.
007220 01 電療料６２ＲＷ                     PIC 9(4) VALUE ZERO.
007230 01 小計６２ＲＷ                       PIC 9(6) VALUE ZERO.
007240 01 逓減開始和暦年月日６２ＲＷ.
007250   03 逓減開始和暦年月６２ＲＷ.
007260     05 逓減開始和暦６２ＲＷ           PIC 9 VALUE ZERO.
007270     05 逓減開始年月６２ＲＷ.
007280       07 逓減開始年６２ＲＷ           PIC 9(2) VALUE ZERO.
007290       07 逓減開始月６２ＲＷ           PIC 9(2) VALUE ZERO.
007300   03 逓減開始日６２ＲＷ               PIC 9(2) VALUE ZERO.
007310* ６部位目（繰上り３）
007320 01 後療回数６３ＲＷ                   PIC 9(2) VALUE ZERO.
007330 01 後療料６３ＲＷ                     PIC 9(5) VALUE ZERO.
007340 01 冷罨法回数６３ＲＷ                 PIC 9(2) VALUE ZERO.
007350 01 冷罨法料６３ＲＷ                   PIC 9(4) VALUE ZERO.
007360 01 温罨法回数６３ＲＷ                 PIC 9(2) VALUE ZERO.
007370 01 温罨法料６３ＲＷ                   PIC 9(4) VALUE ZERO.
007380 01 電療回数６３ＲＷ                   PIC 9(2) VALUE ZERO.
007390 01 電療料６３ＲＷ                     PIC 9(4) VALUE ZERO.
007400 01 小計６３ＲＷ                       PIC 9(6) VALUE ZERO.
007410 01 逓減開始和暦年月日６３ＲＷ.
007420   03 逓減開始和暦年月６３ＲＷ.
007430     05 逓減開始和暦６３ＲＷ           PIC 9 VALUE ZERO.
007440     05 逓減開始年月６３ＲＷ.
007450       07 逓減開始年６３ＲＷ           PIC 9(2) VALUE ZERO.
007460       07 逓減開始月６３ＲＷ           PIC 9(2) VALUE ZERO.
007470   03 逓減開始日６３ＲＷ               PIC 9(2) VALUE ZERO.
      */７部位/120804
007140* ７部位目（繰上り２）
007150 01 後療回数７２ＲＷ                   PIC 9(2) VALUE ZERO.
007160 01 後療料７２ＲＷ                     PIC 9(5) VALUE ZERO.
007170 01 冷罨法回数７２ＲＷ                 PIC 9(2) VALUE ZERO.
007180 01 冷罨法料７２ＲＷ                   PIC 9(4) VALUE ZERO.
007190 01 温罨法回数７２ＲＷ                 PIC 9(2) VALUE ZERO.
007200 01 温罨法料７２ＲＷ                   PIC 9(4) VALUE ZERO.
007210 01 電療回数７２ＲＷ                   PIC 9(2) VALUE ZERO.
007220 01 電療料７２ＲＷ                     PIC 9(4) VALUE ZERO.
007230 01 小計７２ＲＷ                       PIC 9(6) VALUE ZERO.
007240 01 逓減開始和暦年月日７２ＲＷ.
007250   03 逓減開始和暦年月７２ＲＷ.
007260     05 逓減開始和暦７２ＲＷ           PIC 9 VALUE ZERO.
007270     05 逓減開始年月７２ＲＷ.
007280       07 逓減開始年７２ＲＷ           PIC 9(2) VALUE ZERO.
007290       07 逓減開始月７２ＲＷ           PIC 9(2) VALUE ZERO.
007300   03 逓減開始日７２ＲＷ               PIC 9(2) VALUE ZERO.
007310* ７部位目（繰上り３）
007320 01 後療回数７３ＲＷ                   PIC 9(2) VALUE ZERO.
007330 01 後療料７３ＲＷ                     PIC 9(5) VALUE ZERO.
007340 01 冷罨法回数７３ＲＷ                 PIC 9(2) VALUE ZERO.
007350 01 冷罨法料７３ＲＷ                   PIC 9(4) VALUE ZERO.
007360 01 温罨法回数７３ＲＷ                 PIC 9(2) VALUE ZERO.
007370 01 温罨法料７３ＲＷ                   PIC 9(4) VALUE ZERO.
007380 01 電療回数７３ＲＷ                   PIC 9(2) VALUE ZERO.
007390 01 電療料７３ＲＷ                     PIC 9(4) VALUE ZERO.
007400 01 小計７３ＲＷ                       PIC 9(6) VALUE ZERO.
007410 01 逓減開始和暦年月日７３ＲＷ.
007420   03 逓減開始和暦年月７３ＲＷ.
007430     05 逓減開始和暦７３ＲＷ           PIC 9 VALUE ZERO.
007440     05 逓減開始年月７３ＲＷ.
007450       07 逓減開始年７３ＲＷ           PIC 9(2) VALUE ZERO.
007460       07 逓減開始月７３ＲＷ           PIC 9(2) VALUE ZERO.
007470   03 逓減開始日７３ＲＷ               PIC 9(2) VALUE ZERO.
007480*
007490* 部位毎の多部位逓減部位毎のワーク（日計用）
007500* １部位目
007510 01 後療料１ＰＷ                       PIC 9(5) VALUE ZERO.
007520 01 冷罨法料１ＰＷ                     PIC 9(4) VALUE ZERO.
007530 01 温罨法料１ＰＷ                     PIC 9(4) VALUE ZERO.
007540 01 電療料１ＰＷ                       PIC 9(4) VALUE ZERO.
007550 01 小計１ＰＷ                         PIC 9(6) VALUE ZERO.
007560* ２部位目
007570 01 後療料２ＰＷ                       PIC 9(5) VALUE ZERO.
007580 01 冷罨法料２ＰＷ                     PIC 9(4) VALUE ZERO.
007590 01 温罨法料２ＰＷ                     PIC 9(4) VALUE ZERO.
007600 01 電療料２ＰＷ                       PIC 9(4) VALUE ZERO.
007610* ３部位目
007620 01 後療料３ＰＷ                       PIC 9(5) VALUE ZERO.
007630 01 冷罨法料３ＰＷ                     PIC 9(4) VALUE ZERO.
007640 01 温罨法料３ＰＷ                     PIC 9(4) VALUE ZERO.
007650 01 電療料３ＰＷ                       PIC 9(4) VALUE ZERO.
007660* ３部位目（繰上り）
007670 01 後療料３１ＰＷ                     PIC 9(5) VALUE ZERO.
007680 01 冷罨法料３１ＰＷ                   PIC 9(4) VALUE ZERO.
007690 01 温罨法料３１ＰＷ                   PIC 9(4) VALUE ZERO.
007700 01 電療料３１ＰＷ                     PIC 9(4) VALUE ZERO.
007710* ４部位目
007720 01 後療料４ＰＷ                       PIC 9(5) VALUE ZERO.
007730 01 冷罨法料４ＰＷ                     PIC 9(4) VALUE ZERO.
007740 01 温罨法料４ＰＷ                     PIC 9(4) VALUE ZERO.
007750 01 電療料４ＰＷ                       PIC 9(4) VALUE ZERO.
007760* ４部位目（繰上り１）
007770 01 後療料４１ＰＷ                     PIC 9(5) VALUE ZERO.
007780 01 冷罨法料４１ＰＷ                   PIC 9(4) VALUE ZERO.
007790 01 温罨法料４１ＰＷ                   PIC 9(4) VALUE ZERO.
007800 01 電療料４１ＰＷ                     PIC 9(4) VALUE ZERO.
007810* ４部位目（繰上り２）
007820 01 後療料４２ＰＷ                     PIC 9(5) VALUE ZERO.
007830 01 冷罨法料４２ＰＷ                   PIC 9(4) VALUE ZERO.
007840 01 温罨法料４２ＰＷ                   PIC 9(4) VALUE ZERO.
007850 01 電療料４２ＰＷ                     PIC 9(4) VALUE ZERO.
007860* ５部位目
007870 01 後療料５ＰＷ                       PIC 9(5) VALUE ZERO.
007880 01 冷罨法料５ＰＷ                     PIC 9(4) VALUE ZERO.
007890 01 温罨法料５ＰＷ                     PIC 9(4) VALUE ZERO.
007900 01 電療料５ＰＷ                       PIC 9(4) VALUE ZERO.
007910* ５部位目（繰上り１）
007920 01 後療料５１ＰＷ                     PIC 9(5) VALUE ZERO.
007930 01 冷罨法料５１ＰＷ                   PIC 9(4) VALUE ZERO.
007940 01 温罨法料５１ＰＷ                   PIC 9(4) VALUE ZERO.
007950 01 電療料５１ＰＷ                     PIC 9(4) VALUE ZERO.
007960* ５部位目（繰上り２）
007970 01 後療料５２ＰＷ                     PIC 9(5) VALUE ZERO.
007980 01 冷罨法料５２ＰＷ                   PIC 9(4) VALUE ZERO.
007990 01 温罨法料５２ＰＷ                   PIC 9(4) VALUE ZERO.
008000 01 電療料５２ＰＷ                     PIC 9(4) VALUE ZERO.
008010* ５部位目（繰上り３）
008020 01 後療料５３ＰＷ                     PIC 9(5) VALUE ZERO.
008030 01 冷罨法料５３ＰＷ                   PIC 9(4) VALUE ZERO.
008040 01 温罨法料５３ＰＷ                   PIC 9(4) VALUE ZERO.
008050 01 電療料５３ＰＷ                     PIC 9(4) VALUE ZERO.
      */６部位/120807
007960* ６部位目（繰上り２）
007970 01 後療料６２ＰＷ                     PIC 9(5) VALUE ZERO.
007980 01 冷罨法料６２ＰＷ                   PIC 9(4) VALUE ZERO.
007990 01 温罨法料６２ＰＷ                   PIC 9(4) VALUE ZERO.
008000 01 電療料６２ＰＷ                     PIC 9(4) VALUE ZERO.
008010* ６部位目（繰上り３）
008020 01 後療料６３ＰＷ                     PIC 9(5) VALUE ZERO.
008030 01 冷罨法料６３ＰＷ                   PIC 9(4) VALUE ZERO.
008040 01 温罨法料６３ＰＷ                   PIC 9(4) VALUE ZERO.
008050 01 電療料６３ＰＷ                     PIC 9(4) VALUE ZERO.
      */７部位/120807
007960* ７部位目（繰上り２）
007970 01 後療料７２ＰＷ                     PIC 9(5) VALUE ZERO.
007980 01 冷罨法料７２ＰＷ                   PIC 9(4) VALUE ZERO.
007990 01 温罨法料７２ＰＷ                   PIC 9(4) VALUE ZERO.
008000 01 電療料７２ＰＷ                     PIC 9(4) VALUE ZERO.
008010* ７部位目（繰上り３）
008020 01 後療料７３ＰＷ                     PIC 9(5) VALUE ZERO.
008030 01 冷罨法料７３ＰＷ                   PIC 9(4) VALUE ZERO.
008040 01 温罨法料７３ＰＷ                   PIC 9(4) VALUE ZERO.
008050 01 電療料７３ＰＷ                     PIC 9(4) VALUE ZERO.
008060*
008070 01 負傷種別略称Ｗ.
008080    03 負傷種別略称Ｗ１                PIC N(4).
008090    03 FILLER                          PIC N(2).
008100 01 元号名称Ｗ.
008110    03 元号名称Ｗ１                    PIC N(2).
008120    03 FILLER                          PIC N(4).
008130 01 転帰区分略称Ｗ.
008140    03 転帰区分略称Ｗ１                PIC N(4).
008150    03 FILLER                          PIC N(2).
008160*
008170 01  保険者番号Ｗ２.
008180     03  保番１Ｗ                      PIC X(2).
008190     03  保番２Ｗ                      PIC X(8).
008200* 費用負担者番号（市町村番号）ＷＯＲＫ
008210 01 費用負担者番号Ｗ.
008220     03  法別番号Ｗ                    PIC X(2).
008230     03  助保険Ｗ.
008240          05  府県Ｗ                   PIC X(2).
008250          05  助番Ｗ                   PIC X     OCCURS 3.
008260          05  助ＣＤＷ                 PIC X.
008270     03  FILLER                        PIC X(2).
008280*/エラー表示の修正
008290 01 エラー表示Ｆ                       PIC 9(1)  VALUE ZERO.
008300*/老人保険改正の対応0101
008310 01 終了フラグ３                       PIC X(3) VALUE SPACE.
008310 01 終了フラグ４                       PIC X(3) VALUE SPACE.
008320 01 枝番フラグ                         PIC X(3) VALUE SPACE.
008330 01 初診療年月日ＣＷ                   PIC X(7) VALUE SPACE.
008340 01 公費種別Ｗ１                       PIC 9(2) VALUE ZERO.
008350 01 助成種別Ｗ１                       PIC 9(2) VALUE ZERO.
008360 01 公費種別Ｗ２                       PIC 9(2) VALUE ZERO.
008370 01 助成種別Ｗ２                       PIC 9(2) VALUE ZERO.
008380*
008390 01 枝番累計金額Ｗ                     PIC 9(5) VALUE ZERO.
008400* 01 枝番累計金額１Ｗ                   PIC 9(5) VALUE ZERO.
008410*/枝番計算時の元のデータを保存する為のワーク
008420 01 枝番時元データ.
008430    03  保険種別枝Ｗ                   PIC 9(2)  VALUE ZERO.
008440    03  保険者番号枝Ｗ                 PIC X(10) VALUE SPACE.
008450    03  公費種別枝Ｗ                   PIC 9(2)  VALUE ZERO.
008460    03  市町村番号老人枝Ｗ             PIC X(10) VALUE SPACE.
008470    03  助成種別枝Ｗ                   PIC 9(2)  VALUE ZERO.
008480    03  市町村番号助成枝Ｗ             PIC X(10) VALUE ZERO.
008490*/0210 ２７助成で助成の負担額を退避しておく
008500 01 負担額助成ＧＷ２                   PIC 9(6) VALUE ZERO.
008510*追加↓↓↓
008520 01 カウンタ                           PIC 9(3) VALUE ZERO.
008530 01 存在フラグ                         PIC 9(1) VALUE ZERO.
008540*/日毎用ワーク
008550 01 初検時間外ＮＷ                     PIC 9 VALUE ZERO.
008560 01 初検休日ＮＷ                       PIC 9 VALUE ZERO.
008570 01 初検深夜ＮＷ                       PIC 9 VALUE ZERO.
008580 01 往療夜間ＮＷ                       PIC 9 VALUE ZERO.
008590 01 往療難路ＮＷ                       PIC 9 VALUE ZERO.
008600 01 往療暴風雨ＮＷ                     PIC 9 VALUE ZERO.
008610 01 往療時間外ＮＷ                     PIC 9 VALUE ZERO.
008620 01 金属副子大ＮＷ                     PIC 9 VALUE ZERO.
008630 01 金属副子中ＮＷ                     PIC 9 VALUE ZERO.
008640 01 金属副子小ＮＷ                     PIC 9 VALUE ZERO.
008650*/日毎部位毎ワーク
008660 01 初回処置ＮＯＷ.
008670    03 初回処置料ＮＯＷ                PIC 9(5) OCCURS 9 VALUE ZERO.
008680* １部位目
008690 01 後療料１ＮＲＷ                     PIC 9(5) VALUE ZERO.
008700 01 冷罨法料１ＮＲＷ                   PIC 9(4) VALUE ZERO.
008710 01 温罨法料１ＮＲＷ                   PIC 9(4) VALUE ZERO.
008720 01 電療料１ＮＲＷ                     PIC 9(4) VALUE ZERO.
008730* ２部位目
008740 01 後療料２ＮＲＷ                     PIC 9(5) VALUE ZERO.
008750 01 冷罨法料２ＮＲＷ                   PIC 9(4) VALUE ZERO.
008760 01 温罨法料２ＮＲＷ                   PIC 9(4) VALUE ZERO.
008770 01 電療料２ＮＲＷ                     PIC 9(4) VALUE ZERO.
008780* ３部位目逓減
008790 01 後療料３ＮＲＷ                     PIC 9(5) VALUE ZERO.
008800 01 冷罨法料３ＮＲＷ                   PIC 9(4) VALUE ZERO.
008810 01 温罨法料３ＮＲＷ                   PIC 9(4) VALUE ZERO.
008820 01 電療料３ＮＲＷ                     PIC 9(4) VALUE ZERO.
008830* ３部位目（繰上り）
008840 01 後療料３１ＮＲＷ                   PIC 9(5) VALUE ZERO.
008850 01 冷罨法料３１ＮＲＷ                 PIC 9(4) VALUE ZERO.
008860 01 温罨法料３１ＮＲＷ                 PIC 9(4) VALUE ZERO.
008870 01 電療料３１ＮＲＷ                   PIC 9(4) VALUE ZERO.
008880* ４部位目
008890 01 後療料４ＮＲＷ                     PIC 9(5) VALUE ZERO.
008900 01 冷罨法料４ＮＲＷ                   PIC 9(4) VALUE ZERO.
008910 01 温罨法料４ＮＲＷ                   PIC 9(4) VALUE ZERO.
008920 01 電療料４ＮＲＷ                     PIC 9(4) VALUE ZERO.
008930* ４部位目（繰上り１）
008940 01 後療料４１ＮＲＷ                   PIC 9(5) VALUE ZERO.
008950 01 冷罨法料４１ＮＲＷ                 PIC 9(4) VALUE ZERO.
008960 01 温罨法料４１ＮＲＷ                 PIC 9(4) VALUE ZERO.
008970 01 電療料４１ＮＲＷ                   PIC 9(4) VALUE ZERO.
008980* ４部位目（繰上り２）
008990 01 後療料４２ＮＲＷ                   PIC 9(5) VALUE ZERO.
009000 01 冷罨法料４２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009010 01 温罨法料４２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009020 01 電療料４２ＮＲＷ                   PIC 9(4) VALUE ZERO.
009030* ５部位目
009040 01 後療料５ＮＲＷ                     PIC 9(5) VALUE ZERO.
009050 01 冷罨法料５ＮＲＷ                   PIC 9(4) VALUE ZERO.
009060 01 温罨法料５ＮＲＷ                   PIC 9(4) VALUE ZERO.
009070 01 電療料５ＮＲＷ                     PIC 9(4) VALUE ZERO.
009080* ５部位目（繰上り１）
009090 01 後療料５１ＮＲＷ                   PIC 9(5) VALUE ZERO.
009100 01 冷罨法料５１ＮＲＷ                 PIC 9(4) VALUE ZERO.
009110 01 温罨法料５１ＮＲＷ                 PIC 9(4) VALUE ZERO.
009120 01 電療料５１ＮＲＷ                   PIC 9(4) VALUE ZERO.
009130* ５部位目（繰上り２）
009140 01 後療料５２ＮＲＷ                   PIC 9(5) VALUE ZERO.
009150 01 冷罨法料５２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009160 01 温罨法料５２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009170 01 電療料５２ＮＲＷ                   PIC 9(4) VALUE ZERO.
009180* ５部位目（繰上り３）
009190 01 後療料５３ＮＲＷ                   PIC 9(5) VALUE ZERO.
009200 01 冷罨法料５３ＮＲＷ                 PIC 9(4) VALUE ZERO.
009210 01 温罨法料５３ＮＲＷ                 PIC 9(4) VALUE ZERO.
009220 01 電療料５３ＮＲＷ                   PIC 9(4) VALUE ZERO.
      */６部位～/120804
009130* ６部位目（繰上り２）
009140 01 後療料６２ＮＲＷ                   PIC 9(5) VALUE ZERO.
009150 01 冷罨法料６２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009160 01 温罨法料６２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009170 01 電療料６２ＮＲＷ                   PIC 9(4) VALUE ZERO.
009180* ６部位目（繰上り３）
009190 01 後療料６３ＮＲＷ                   PIC 9(5) VALUE ZERO.
009200 01 冷罨法料６３ＮＲＷ                 PIC 9(4) VALUE ZERO.
009210 01 温罨法料６３ＮＲＷ                 PIC 9(4) VALUE ZERO.
009220 01 電療料６３ＮＲＷ                   PIC 9(4) VALUE ZERO.
009130* ７部位目（繰上り２）
009140 01 後療料７２ＮＲＷ                   PIC 9(5) VALUE ZERO.
009150 01 冷罨法料７２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009160 01 温罨法料７２ＮＲＷ                 PIC 9(4) VALUE ZERO.
009170 01 電療料７２ＮＲＷ                   PIC 9(4) VALUE ZERO.
009180* ７部位目（繰上り３）
009190 01 後療料７３ＮＲＷ                   PIC 9(5) VALUE ZERO.
009200 01 冷罨法料７３ＮＲＷ                 PIC 9(4) VALUE ZERO.
009210 01 温罨法料７３ＮＲＷ                 PIC 9(4) VALUE ZERO.
009220 01 電療料７３ＮＲＷ                   PIC 9(4) VALUE ZERO.
009230
009240 01 レセプト種類Ｗ                     PIC X(4) VALUE SPACE.
009250 01 助成カウンタ                       PIC 9     VALUE ZERO.
009260 01 更新フラグ                         PIC X(3)  VALUE SPACE.
009270* 初検日退避用
009280 01 初検年月日ＷＴ.
009290    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
009300    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
009310    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
009320    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
009330* キーレセ種別用
009340 01 レセ種別Ｗ                         PIC 9(2)  VALUE ZERO.
009350 01 新規チェックＷ                     PIC N     VALUE SPACE.
009360 01 継続チェックＷ                     PIC N     VALUE SPACE.
009370 01 初検フラグ                         PIC X(3)  VALUE SPACE.
009380 01 部位データ.
009381     03 部位実日数Ｗ          PIC 9(2) OCCURS 7  VALUE ZERO.
009390 01 実日数Ｗ                           PIC 9(2)  VALUE ZERO.
009400*
009410*追加↑↑↑
009420*
009421**
009422 01 会計領収無しフラグ                 PIC X(3) VALUE SPACE.
009426***
009427*
009428 01 計算拡張情報Ｗ.
009429   03 本体償還払い区分Ｗ            PIC 9 VALUE ZERO.
009430   03 助成償還払い区分Ｗ            PIC 9 VALUE ZERO.
009431   03 本体まとめ区分Ｗ              PIC 9 VALUE ZERO.
009432 01 枝番負担累計額Ｗ                PIC 9(6) VALUE ZERO.
009434*
      */助成時の本体分の窓口金/120906
       01 本体一部負担金Ｗ                PIC 9(6) VALUE ZERO.
000530*/ 入金額初期値 0:一部負担金 1:０円/140821
000540 01 入金額初期値Ｗ                  PIC 9    VALUE ZERO.
009435*
      */施術者番号/151229
       01 退避施術者番号Ｗ.
          05 施術者番号Ｗ                 PIC 9(3) OCCURS 31 VALUE ZERO.
          05 繰越フラグ                   PIC 9(1) OCCURS 31 VALUE ZERO.
       01 複数施術者使用区分Ｗ            PIC 9(1) VALUE ZERO.
       01 前回施術者番号Ｗ                PIC 9(3) VALUE ZERO.
      */入金額を退避してクリアした後に戻す/160625
       01 入金退避Ｗ.
          03 退避入金額Ｗ                 PIC 9(6) OCCURS 31 VALUE ZERO.
          03 退避自費額Ｗ                 PIC 9(6) OCCURS 31 VALUE ZERO.
      */負担区分２２(大阪府平成30年4月～)
004210 01 計算限度額Ｗ                       PIC 9(5) VALUE ZERO.
002790 01 負担一定金額Ｗ                     PIC 9(6) VALUE ZERO.








      */平成30年6月1日より↓↓↓/20180424
      *
       01 部位カウンタ３                     PIC 9(2) VALUE ZERO.
       01 部位カウンタ４                     PIC 9(2) VALUE ZERO.
       01 金属副子ＣＮＴ                     PIC 9(1) VALUE ZERO.
       01 部位コード２Ｗ.
          03 負傷種別２Ｗ                    PIC 9(2) VALUE ZERO.
          03 部位２Ｗ                        PIC 9(2) VALUE ZERO.
          03 左右区分２Ｗ                    PIC 9    VALUE ZERO.
          03 負傷位置番号２Ｗ                PIC 9(2) VALUE ZERO.
      *
       01 施術和暦年月日２Ｗ.
         03 施術和暦年月２Ｗ.
           05 施術和暦２Ｗ                   PIC 9    VALUE ZERO.
           05 施術年月２Ｗ.
              07 施術年２Ｗ                  PIC 9(2) VALUE ZERO.
              07 施術月２Ｗ                  PIC 9(2) VALUE ZERO.
         03 施術日２Ｗ                       PIC 9(2) VALUE ZERO.
      *
       01 開始和暦年月日２Ｗ.
         03 開始和暦年月２Ｗ.
           05 開始和暦２Ｗ                   PIC 9    VALUE ZERO.
           05 開始年月２Ｗ.
              07 開始年２Ｗ                  PIC 9(2) VALUE ZERO.
              07 開始月２Ｗ                  PIC 9(2) VALUE ZERO.
         03 開始日２Ｗ                       PIC 9(2) VALUE ZERO.
      *運動後療料
       01 運動療法料ＡＷ                     PIC 9(4) VALUE ZERO.
       01 運動療法回数Ｗ                     PIC 9    VALUE ZERO.
       01 運動療法料Ｗ                       PIC 9(4) VALUE ZERO.
       01 運動療法料ＧＷ                     PIC 9(5) VALUE ZERO.
       01 運動後療回数Ｗ                     PIC 9(2) VALUE ZERO.
       01 運動後療Ｗ.
           03 運動月上限回数Ｗ               PIC 9(2) OCCURS 7 VALUE ZERO.
           03 運動算定日Ｗ                   PIC 9(2) OCCURS 5 VALUE ZERO.
      *明細書発行加算料
       01 明細書発行加算料ＡＷ               PIC 9(4) VALUE ZERO.
       01 明細書発行加算料Ｗ                 PIC 9(4) VALUE ZERO.
       01 明細書発行加算料ＧＷ               PIC 9(5) VALUE ZERO.
       01 明細書発行加算日Ｗ                 PIC 9(2) VALUE ZERO.
      *
001540 01 剰余Ｗ                             PIC 9(4) VALUE ZERO.
002000*
002010 01 計算和暦年月Ｗ.
002020     03 計算和暦Ｗ                     PIC 9.
002030     03 計算年月Ｗ.
002040        05 計算年Ｗ                    PIC 9(2).
002050        05 計算月Ｗ                    PIC 9(2).
002060     03 計算日Ｗ                       PIC 9(2).
002070*
002080 01 絶対値Ｗ                           PIC 9(6) VALUE ZERO.
002090 01 絶対値月Ｗ                         PIC 9(3) VALUE ZERO.
002100 01 絶対値月Ｗ２                       PIC 9(3) VALUE ZERO.
002110 01 負傷経過日Ｗ                       PIC 9(4) VALUE ZERO.
002120* 01 西暦年Ｗ                           PIC 9(4) VALUE ZERO.
002130* 01 西暦年差Ｗ                         PIC 9(2) VALUE ZERO.
002140 01 西暦年差２Ｗ                       PIC 9(2) VALUE ZERO.
002150 01 閏年回数Ｗ                         PIC 9(2) VALUE ZERO.
002160 01 通常年回数Ｗ                       PIC 9(2) VALUE ZERO.
002170 01 通常年絶対値Ｗ                     PIC 9(6) VALUE ZERO.
002180 01 閏年絶対値Ｗ                       PIC 9(6) VALUE ZERO.
002190 01 年絶対値Ｗ                         PIC 9(6) VALUE ZERO.
002200 01 負傷絶対値Ｗ                       PIC 9(6) VALUE ZERO.
002210 01 施術絶対値Ｗ                       PIC 9(6) VALUE ZERO.
002220 01 加算年Ｗ                           PIC 9(4) VALUE ZERO.
001410*
001420 01 遅延フラグ                         PIC X(3) VALUE SPACE.
001430 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
001440 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
       01 金属副子算定Ｆ                     PIC X(3) VALUE SPACE.
      */平成30年6月1日より↑↑↑/20180424
       01 明細書発行加算回数Ｗ               PIC 9(2) VALUE ZERO.
      *
009438**
009441*******
009442* 01 画面制御.
009450*     03 定義体名                       PIC X(8) VALUE SPACE.
009460*     03 項目群名                       PIC X(8) VALUE SPACE.
009470*     03 項目群名Ｒ  REDEFINES  項目群名.
009480*         05 項目群Ｒ名                 PIC X(4).
009490*         05 項目群番号                 PIC 9(2).
009500*         05 FILLER                     PIC X(2).
009510*     03 処理種別                       PIC X(2) VALUE SPACE.
009520*     03 アテンション                   PIC X(4) VALUE SPACE.
009530*     03 機能キー                       PIC X(4) VALUE SPACE.
009540*     03 拡張制御.
009550*         05 端末制御                   PIC X(4) VALUE SPACE.
009560*         05 詳細制御                   PIC X(2) VALUE SPACE.
009570*     03 通知情報                       PIC X(2) VALUE SPACE.
009580*     03 ユニット名.
009590*         05 FILLER                     PIC X(2)  VALUE SPACE.
009600*         05 端末番号                   PIC 9(3)  VALUE ZERO.
009610*         05 FILLER                     PIC X(4)  VALUE SPACE.
009620*     03 メッセージＷ                   PIC N(20) VALUE SPACE.
009630* 日付ＷＯＲＫ
009640 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009650 01 計算機和暦年Ｗ                     PIC 9(2) VALUE ZERO.
009660*
009670 01 計算機西暦.
009680    03 計算機西暦年                    PIC 9(4).
009690    03 計算機西暦月日                  PIC 9(4).
009700 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009710    03 計算機世紀                      PIC 9(2).
009720    03 計算機日付                      PIC 9(6).
009730    03 計算機日付Ｒ REDEFINES 計算機日付.
009740       05 計算機年月                   PIC 9(4).
009750       05 計算機年月Ｒ REDEFINES 計算機年月.
009760         07 計算機年                   PIC 9(2).
009770         07 計算機月                   PIC 9(2).
009780       05 計算機日                     PIC 9(2).
      */令和6年10月改定対応↓↓↓*/20240708
       01 長期頻回計算Ｗ.
           03 継続月数Ｗ            OCCURS 7 PIC 9(2) VALUE ZERO.
           03 部位継続月数Ｗ        OCCURS 7 PIC 9(2) VALUE ZERO.
      * 01 判定終了和暦年月Ｗ                 PIC 9(5) VALUE ZERO.
       01 判定終了和暦年月Ｗ.
002020     03 判定終了和暦Ｗ                 PIC 9    VALUE ZERO.
002030     03 判定終了年月Ｗ.
002040        05 判定終了年Ｗ                PIC 9(2) VALUE ZERO.
002050        05 判定終了月Ｗ                PIC 9(2) VALUE ZERO.
       01 終了フラグ５                       PIC X(3) VALUE SPACE.
       01 終了フラグ６                       PIC X(3) VALUE SPACE.
       01 終了フラグ７                       PIC X(3) VALUE SPACE.
004310*/前回通院月判定
004320 01 計算年月日３Ｗ.
004330    03 計算和暦３Ｗ                    PIC 9(1)  VALUE ZERO.
004340    03 計算年３Ｗ                      PIC S9(2) VALUE ZERO.
004350    03 計算月３Ｗ                      PIC S9(2) VALUE ZERO.
004360    03 計算日３Ｗ                      PIC S9(2) VALUE ZERO.
004370 01 開始年月日３Ｗ.
          03 開始和暦年月３Ｗ.
004380        05 開始和暦３Ｗ                PIC 9(1)  VALUE ZERO.
004390        05 開始年３Ｗ                  PIC 9(2)  VALUE ZERO.
004400        05 開始月３Ｗ                  PIC 9(2)  VALUE ZERO.
004410    03 開始日３Ｗ                      PIC 9(2)  VALUE ZERO.
004420    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
004430 01 終了年月日３Ｗ.
          03 終了和暦年月３Ｗ.
004440        05 終了和暦３Ｗ                PIC 9(1)  VALUE ZERO.
004450        05 終了年３Ｗ                  PIC 9(2)  VALUE ZERO.
004460        05 終了月３Ｗ                  PIC 9(2)  VALUE ZERO.
004470    03 終了日３Ｗ                      PIC 9(2)  VALUE ZERO.
004480    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
004510 01 前月フラグ                         PIC X(3)  VALUE SPACE.
      */明細発行加算の変更和暦年月/20240724
000420 01 明細変更和暦年月Ｗ.
000430    03 明細変更和暦Ｗ                  PIC 9     VALUE ZERO.
000440    03 明細変更年月Ｗ.
000450        05 明細変更年Ｗ                PIC 9(2)  VALUE ZERO.
000460        05 明細変更月Ｗ                PIC 9(2)  VALUE ZERO.
       01 枝番部位実日数Ｗ                   PIC 9(2)  VALUE ZERO.

       01 継続開始施術和暦年月Ｗ             PIC 9(5)  VALUE ZERO.
       01 前月部位継続月数Ｗ                 PIC 9(2)  VALUE ZERO.
004640 01 協会コードＷ                       PIC 9(2)  VALUE ZERO.
      */令和6年10月改定対応↑↑↑*/20240708

009790*
009800* ＦＯＲＭ（画面定義体）の展開
009810*
009820* ファイル識別名
009830*****************************************************************
009840*                          連結項目                             *
009850*****************************************************************
009860*
009870*    月毎の料金
009880***********************
009890 01 連料－料金１ IS EXTERNAL.
009900   03 連料－初検.
009910      05 連料－負担率                 PIC 9(3).
009920      05 連料－初検料                 PIC 9(5).
009930      05 連料－初検加算.
009940        07 連料－初検時間外           PIC 9.
009950        07 連料－初検休日             PIC 9.
009960        07 連料－初検深夜             PIC 9.
009970      05 連料－初検加算料             PIC 9(5).
009980   03 連料－再検料                    PIC 9(5).
009990   03 連料－往療.
010000      05 連料－往療距離               PIC 9(2)V9.
010010      05 連料－往療回数               PIC 9(2).
010020      05 連料－往療料                 PIC 9(6).
010030      05 連料－往療加算料             PIC 9(5).
010040      05 連料－往療加算.
010050        07 連料－往療夜間             PIC 9.
010060        07 連料－往療難路             PIC 9.
010070        07 連料－往療暴風雨           PIC 9.
010080        07 連料－往療時間外           PIC 9.
010090   03 連料－金属副子加算料            PIC 9(5).
010100   03 連料－金属副子数.
010110      05 連料－大                     PIC 9.
010120      05 連料－中                     PIC 9.
010130      05 連料－小                     PIC 9.
010140   03 連料－施術情報提供料            PIC 9(5).
010150   03 連料－小計                      PIC 9(7).
010160   03 連料－費用額                    PIC 9(6).
010170   03 連料－一部負担金                PIC 9(6).
010180   03 連料－請求額                    PIC 9(6).
010190   03 連料－給付割合                  PIC 9(3).
010200   03 連料－受給者負担額              PIC 9(6).
010210   03 連料－助成請求金額              PIC 9(6).
010220*
010230   03 連料－受給者負担額内訳   OCCURS  10.
010240      05 連料－当日分負担額           PIC 9(4).
010250*
010260*
010270* 負傷部位毎の料金
010280***********************
010290 01 連料－料金２ IS EXTERNAL.
010300   03 連料－初回処置    OCCURS   9.
010310      05 連料－初回処置料             PIC 9(5).
010320   03 連料－初回処置料計              PIC 9(6).
010330*
010340* 逓減毎の料金
010350***********************
010360 01 連料－料金３ IS EXTERNAL.
010370**********
010380* １部位 *
010390**********
010400   03 連料－部位１.
010410      05 連料－後療１.
010420         07 連料－後療単価１          PIC 9(4).
010430         07 連料－後療回数１          PIC 9(2).
010440         07 連料－後療料１            PIC 9(5).
010450      05 連料－冷罨法１.
010460         07 連料－冷罨法回数１        PIC 9(2).
010470         07 連料－冷罨法料１          PIC 9(4).
010480      05 連料－温罨法１.
010490         07 連料－温罨法回数１        PIC 9(2).
010500         07 連料－温罨法料１          PIC 9(4).
010510      05 連料－電療１.
010520         07 連料－電療回数１          PIC 9(2).
010530         07 連料－電療料１            PIC 9(4).
010540      05 連料－小計１                 PIC 9(6).
010550      05 連料－長期逓減率１           PIC 9(3).
010560      05 連料－長期込小計１           PIC 9(6).
010570**********
010580* ２部位 *
010590**********
010600   03 連料－部位２.
010610      05 連料－後療２.
010620         07 連料－後療単価２          PIC 9(4).
010630         07 連料－後療回数２          PIC 9(2).
010640         07 連料－後療料２            PIC 9(5).
010650      05 連料－冷罨法２.
010660         07 連料－冷罨法回数２        PIC 9(2).
010670         07 連料－冷罨法料２          PIC 9(4).
010680      05 連料－温罨法２.
010690         07 連料－温罨法回数２        PIC 9(2).
010700         07 連料－温罨法料２          PIC 9(4).
010710      05 連料－電療２.
010720         07 連料－電療回数２          PIC 9(2).
010730         07 連料－電療料２            PIC 9(4).
010740      05 連料－小計２                 PIC 9(6).
010750      05 連料－長期逓減率２           PIC 9(3).
010760      05 連料－長期込小計２           PIC 9(6).
010770******************
010780* ３部位／８割 *
010790******************
010800   03 連料－部位３８.
010810      05 連料－後療３８.
010820         07 連料－後療単価３８          PIC 9(4).
010830         07 連料－後療回数３８          PIC 9(2).
010840         07 連料－後療料３８            PIC 9(5).
010850      05 連料－冷罨法３８.
010860         07 連料－冷罨法回数３８        PIC 9(2).
010870         07 連料－冷罨法料３８          PIC 9(4).
010880      05 連料－温罨法３８.
010890         07 連料－温罨法回数３８        PIC 9(2).
010900         07 連料－温罨法料３８          PIC 9(4).
010910      05 連料－電療３８.
010920         07 連料－電療回数３８          PIC 9(2).
010930         07 連料－電療料３８            PIC 9(4).
010940      05 連料－小計３８                 PIC 9(6).
010950      05 連料－多部位込小計３８         PIC 9(6).
010960      05 連料－長期逓減率３８           PIC 9(3).
010970      05 連料－長期込小計３８           PIC 9(6).
010980******************
010990* ３部位／１０割 *
011000******************
011010   03 連料－部位３０.
011020      05 連料－逓減開始月日３０.
011030         07 連料－逓減開始月３０        PIC 9(2).
011040         07 連料－逓減開始日３０        PIC 9(2).
011050      05 連料－後療３０.
011060         07 連料－後療単価３０          PIC 9(4).
011070         07 連料－後療回数３０          PIC 9(2).
011080         07 連料－後療料３０            PIC 9(5).
011090      05 連料－冷罨法３０.
011100         07 連料－冷罨法回数３０        PIC 9(2).
011110         07 連料－冷罨法料３０          PIC 9(4).
011120      05 連料－温罨法３０.
011130         07 連料－温罨法回数３０        PIC 9(2).
011140         07 連料－温罨法料３０          PIC 9(4).
011150      05 連料－電療３０.
011160         07 連料－電療回数３０          PIC 9(2).
011170         07 連料－電療料３０            PIC 9(4).
011180      05 連料－小計３０                 PIC 9(6).
011190      05 連料－長期逓減率３０           PIC 9(3).
011200      05 連料－長期込小計３０           PIC 9(6).
011210****************
011220* ４部位／５割 *
011230****************
011240   03 連料－部位４５.
011250      05 連料－後療４５.
011260         07 連料－後療単価４５          PIC 9(4).
011270         07 連料－後療回数４５          PIC 9(2).
011280         07 連料－後療料４５            PIC 9(5).
011290      05 連料－冷罨法４５.
011300         07 連料－冷罨法回数４５        PIC 9(2).
011310         07 連料－冷罨法料４５          PIC 9(4).
011320      05 連料－温罨法４５.
011330         07 連料－温罨法回数４５        PIC 9(2).
011340         07 連料－温罨法料４５          PIC 9(4).
011350      05 連料－電療４５.
011360         07 連料－電療回数４５          PIC 9(2).
011370         07 連料－電療料４５            PIC 9(4).
011380      05 連料－小計４５                 PIC 9(6).
011390      05 連料－多部位込小計４５         PIC 9(6).
011400      05 連料－長期逓減率４５           PIC 9(3).
011410      05 連料－長期込小計４５           PIC 9(6).
011420****************
011430* ４部位／８割 *
011440****************
011450   03 連料－部位４８.
011460      05 連料－逓減開始月日４８.
011470         07 連料－逓減開始月４８        PIC 9(2).
011480         07 連料－逓減開始日４８        PIC 9(2).
011490      05 連料－後療４８.
011500         07 連料－後療単価４８          PIC 9(4).
011510         07 連料－後療回数４８          PIC 9(2).
011520         07 連料－後療料４８            PIC 9(5).
011530      05 連料－冷罨法４８.
011540         07 連料－冷罨法回数４８        PIC 9(2).
011550         07 連料－冷罨法料４８          PIC 9(4).
011560      05 連料－温罨法４８.
011570         07 連料－温罨法回数４８        PIC 9(2).
011580         07 連料－温罨法料４８          PIC 9(4).
011590      05 連料－電療４８.
011600         07 連料－電療回数４８          PIC 9(2).
011610         07 連料－電療料４８            PIC 9(4).
011620      05 連料－小計４８                 PIC 9(6).
011630      05 連料－多部位込小計４８         PIC 9(6).
011640      05 連料－長期逓減率４８           PIC 9(3).
011650      05 連料－長期込小計４８           PIC 9(6).
011660******************
011670* ４部位／１０割 *
011680******************
011690   03 連料－部位４０.
011700      05 連料－逓減開始月日４０.
011710         07 連料－逓減開始月４０        PIC 9(2).
011720         07 連料－逓減開始日４０        PIC 9(2).
011730      05 連料－後療４０.
011740         07 連料－後療単価４０          PIC 9(4).
011750         07 連料－後療回数４０          PIC 9(2).
011760         07 連料－後療料４０            PIC 9(5).
011770      05 連料－冷罨法４０.
011780         07 連料－冷罨法回数４０        PIC 9(2).
011790         07 連料－冷罨法料４０          PIC 9(4).
011800      05 連料－温罨法４０.
011810         07 連料－温罨法回数４０        PIC 9(2).
011820         07 連料－温罨法料４０          PIC 9(4).
011830      05 連料－電療４０.
011840         07 連料－電療回数４０          PIC 9(2).
011850         07 連料－電療料４０            PIC 9(4).
011860      05 連料－小計４０                 PIC 9(6).
011870      05 連料－長期逓減率４０           PIC 9(3).
011880      05 連料－長期込小計４０           PIC 9(6).
011890********************
011900* ５部位／２．５割 *
011910********************
011920   03 連料－部位５２.
011930      05 連料－後療５２.
011940         07 連料－後療単価５２          PIC 9(4).
011950         07 連料－後療回数５２          PIC 9(2).
011960         07 連料－後療料５２            PIC 9(5).
011970      05 連料－冷罨法５２.
011980         07 連料－冷罨法回数５２        PIC 9(2).
011990         07 連料－冷罨法料５２          PIC 9(4).
012000      05 連料－温罨法５２.
012010         07 連料－温罨法回数５２        PIC 9(2).
012020         07 連料－温罨法料５２          PIC 9(4).
012030      05 連料－電療５２.
012040         07 連料－電療回数５２          PIC 9(2).
012050         07 連料－電療料５２            PIC 9(4).
012060      05 連料－小計５２                 PIC 9(6).
012070      05 連料－多部位込小計５２         PIC 9(6).
012080      05 連料－長期逓減率５２           PIC 9(3).
012090      05 連料－長期込小計５２           PIC 9(6).
012100****************
012110* ５部位／５割 *
012120****************
012130   03 連料－部位５５.
012140      05 連料－逓減開始月日５５.
012150         07 連料－逓減開始月５５        PIC 9(2).
012160         07 連料－逓減開始日５５        PIC 9(2).
012170      05 連料－後療５５.
012180         07 連料－後療単価５５          PIC 9(4).
012190         07 連料－後療回数５５          PIC 9(2).
012200         07 連料－後療料５５            PIC 9(5).
012210      05 連料－冷罨法５５.
012220         07 連料－冷罨法回数５５        PIC 9(2).
012230         07 連料－冷罨法料５５          PIC 9(4).
012240      05 連料－温罨法５５.
012250         07 連料－温罨法回数５５        PIC 9(2).
012260         07 連料－温罨法料５５          PIC 9(4).
012270      05 連料－電療５５.
012280         07 連料－電療回数５５          PIC 9(2).
012290         07 連料－電療料５５            PIC 9(4).
012300      05 連料－小計５５                 PIC 9(6).
012310      05 連料－多部位込小計５５         PIC 9(6).
012320      05 連料－長期逓減率５５           PIC 9(3).
012330      05 連料－長期込小計５５           PIC 9(6).
012340****************
012350* ５部位／８割 *
012360****************
012370   03 連料－部位５８.
012380      05 連料－逓減開始月日５８.
012390         07 連料－逓減開始月５８        PIC 9(2).
012400         07 連料－逓減開始日５８        PIC 9(2).
012410      05 連料－後療５８.
012420         07 連料－後療単価５８          PIC 9(4).
012430         07 連料－後療回数５８          PIC 9(2).
012440         07 連料－後療料５８            PIC 9(5).
012450      05 連料－冷罨法５８.
012460         07 連料－冷罨法回数５８        PIC 9(2).
012470         07 連料－冷罨法料５８          PIC 9(4).
012480      05 連料－温罨法５８.
012490         07 連料－温罨法回数５８        PIC 9(2).
012500         07 連料－温罨法料５８          PIC 9(4).
012510      05 連料－電療５８.
012520         07 連料－電療回数５８          PIC 9(2).
012530         07 連料－電療料５８            PIC 9(4).
012540      05 連料－小計５８                 PIC 9(6).
012550      05 連料－多部位込小計５８         PIC 9(6).
012560      05 連料－長期逓減率５８           PIC 9(3).
012570      05 連料－長期込小計５８           PIC 9(6).
012580******************
012590* ５部位／１０割 *
012600******************
012610   03 連料－部位５０.
012620      05 連料－逓減開始月日５０.
012630         07 連料－逓減開始月５０        PIC 9(2).
012640         07 連料－逓減開始日５０        PIC 9(2).
012650      05 連料－後療５０.
012660         07 連料－後療単価５０          PIC 9(4).
012670         07 連料－後療回数５０          PIC 9(2).
012680         07 連料－後療料５０            PIC 9(5).
012690      05 連料－冷罨法５０.
012700         07 連料－冷罨法回数５０        PIC 9(2).
012710         07 連料－冷罨法料５０          PIC 9(4).
012720      05 連料－温罨法５０.
012730         07 連料－温罨法回数５０        PIC 9(2).
012740         07 連料－温罨法料５０          PIC 9(4).
012750      05 連料－電療５０.
012760         07 連料－電療回数５０          PIC 9(2).
012770         07 連料－電療料５０            PIC 9(4).
012780      05 連料－小計５０                 PIC 9(6).
012790      05 連料－長期逓減率５０           PIC 9(3).
012800      05 連料－長期込小計５０           PIC 9(6).
      */６，７部位目を追加。連結項目としては使わないがここに記載します
       01 連料６部位７部位追加項目.
      */６部位/120804
012340****************
012350* ６部位／７割 *
012360****************
012370   03 連料－部位６８.
012380      05 連料－逓減開始月日６８.
012390         07 連料－逓減開始月６８        PIC 9(2).
012400         07 連料－逓減開始日６８        PIC 9(2).
012410      05 連料－後療６８.
012420         07 連料－後療単価６８          PIC 9(4).
012430         07 連料－後療回数６８          PIC 9(2).
012440         07 連料－後療料６８            PIC 9(5).
012450      05 連料－冷罨法６８.
012460         07 連料－冷罨法回数６８        PIC 9(2).
012470         07 連料－冷罨法料６８          PIC 9(4).
012480      05 連料－温罨法６８.
012490         07 連料－温罨法回数６８        PIC 9(2).
012500         07 連料－温罨法料６８          PIC 9(4).
012510      05 連料－電療６８.
012520         07 連料－電療回数６８          PIC 9(2).
012530         07 連料－電療料６８            PIC 9(4).
012540      05 連料－小計６８                 PIC 9(6).
012550      05 連料－多部位込小計６８         PIC 9(6).
012560      05 連料－長期逓減率６８           PIC 9(3).
012570      05 連料－長期込小計６８           PIC 9(6).
012580******************
012590* ６部位／１０割 *
012600******************
012610   03 連料－部位６０.
012620      05 連料－逓減開始月日６０.
012630         07 連料－逓減開始月６０        PIC 9(2).
012640         07 連料－逓減開始日６０        PIC 9(2).
012650      05 連料－後療６０.
012660         07 連料－後療単価６０          PIC 9(4).
012670         07 連料－後療回数６０          PIC 9(2).
012680         07 連料－後療料６０            PIC 9(5).
012690      05 連料－冷罨法６０.
012700         07 連料－冷罨法回数６０        PIC 9(2).
012710         07 連料－冷罨法料６０          PIC 9(4).
012720      05 連料－温罨法６０.
012730         07 連料－温罨法回数６０        PIC 9(2).
012740         07 連料－温罨法料６０          PIC 9(4).
012750      05 連料－電療６０.
012760         07 連料－電療回数６０          PIC 9(2).
012770         07 連料－電療料６０            PIC 9(4).
012780      05 連料－小計６０                 PIC 9(6).
012790      05 連料－長期逓減率６０           PIC 9(3).
012800      05 連料－長期込小計６０           PIC 9(6).
      */７部位/120804
012340****************
012350* ７部位／７割 *
012360****************
012370   03 連料－部位７８.
012380      05 連料－逓減開始月日７８.
012390         07 連料－逓減開始月７８        PIC 9(2).
012400         07 連料－逓減開始日７８        PIC 9(2).
012410      05 連料－後療７８.
012420         07 連料－後療単価７８          PIC 9(4).
012430         07 連料－後療回数７８          PIC 9(2).
012440         07 連料－後療料７８            PIC 9(5).
012450      05 連料－冷罨法７８.
012460         07 連料－冷罨法回数７８        PIC 9(2).
012470         07 連料－冷罨法料７８          PIC 9(4).
012480      05 連料－温罨法７８.
012490         07 連料－温罨法回数７８        PIC 9(2).
012500         07 連料－温罨法料７８          PIC 9(4).
012510      05 連料－電療７８.
012520         07 連料－電療回数７８          PIC 9(2).
012530         07 連料－電療料７８            PIC 9(4).
012540      05 連料－小計７８                 PIC 9(6).
012550      05 連料－多部位込小計７８         PIC 9(6).
012560      05 連料－長期逓減率７８           PIC 9(3).
012570      05 連料－長期込小計７８           PIC 9(6).
012580******************
012590* ７部位／１０割 *
012600******************
012610   03 連料－部位７０.
012620      05 連料－逓減開始月日７０.
012630         07 連料－逓減開始月７０        PIC 9(2).
012640         07 連料－逓減開始日７０        PIC 9(2).
012650      05 連料－後療７０.
012660         07 連料－後療単価７０          PIC 9(4).
012670         07 連料－後療回数７０          PIC 9(2).
012680         07 連料－後療料７０            PIC 9(5).
012690      05 連料－冷罨法７０.
012700         07 連料－冷罨法回数７０        PIC 9(2).
012710         07 連料－冷罨法料７０          PIC 9(4).
012720      05 連料－温罨法７０.
012730         07 連料－温罨法回数７０        PIC 9(2).
012740         07 連料－温罨法料７０          PIC 9(4).
012750      05 連料－電療７０.
012760         07 連料－電療回数７０          PIC 9(2).
012770         07 連料－電療料７０            PIC 9(4).
012780      05 連料－小計７０                 PIC 9(6).
012790      05 連料－長期逓減率７０           PIC 9(3).
012800      05 連料－長期込小計７０           PIC 9(6).
      *
012810*初検時相談料/080527
012820 01 連料－初検時相談料 IS EXTERNAL      PIC 9(4).
      *運動後療料/180503
       01 連料－運動後療     IS EXTERNAL.
          03 連料－運動後療回数               PIC 9.
          03 連料－運動後療単価               PIC 9(4).
          03 連料－運動後療料                 PIC 9(4).
      *金属副子/180613
       01 連料－金属副子     IS EXTERNAL.
          03 連料－金属副子回数               PIC 9(2).
      *明細書発行加算料/20220704
       01 連料－明細書発行   IS EXTERNAL.
          03 連料－明細書発行加算料           PIC 9(4).
          03 連料－明細書発行加算日           PIC 9(2).
012830*
012840 01 連－共通キー IS EXTERNAL.
012850    03  連－カレント和暦               PIC 9.
012860*
012870 01 連計－キー IS EXTERNAL.
012880    03  連計－負担金計算区分           PIC 9.
012890    03  連計－本人家族区分             PIC 9.
012900    03  連計－保険種別                 PIC 9(2).
012910    03  連計－保険者番号               PIC X(10).
012920    03  連計－公費種別                 PIC 9(2).
012930    03  連計－市町村番号老人           PIC X(10).
012940    03  連計－助成種別                 PIC 9(2).
012950    03  連計－市町村番号助成           PIC X(10).
012960    03  連計－患者コード.
012970      05  連計－患者番号               PIC 9(6).
012980      05  連計－枝番                   PIC X.
012990    03  連計－施術和暦年月日.
013000      05  連計－施術和暦年月.
013010        07  連計－施術和暦             PIC 9.
013020        07  連計－施術年月.
013030          09  連計－施術年             PIC 9(2).
013040          09  連計－施術月             PIC 9(2).
013050      05  連計－施術日                 PIC 9(2).
013060    03  連計－費用額                   PIC 9(6).
013070    03  連計－負担額                   PIC 9(6).
013080    03  連計－請求額                   PIC 9(6).
013090    03  連計－費用額老人               PIC 9(6).
013100    03  連計－負担額老人               PIC 9(6).
013110    03  連計－請求額老人               PIC 9(6).
013120    03  連計－費用額助成               PIC 9(6).
013130    03  連計－負担額助成               PIC 9(5).
013140    03  連計－請求額助成               PIC 9(5).
013150    03  連計－実日数                   PIC 9(2).
013160    03  連計－負担率                   PIC 9(3).
013170    03  連計－負担率区分               PIC 9.
013180*
013181*-------------------------------------*
013183 01 連計日計窓口－キー IS EXTERNAL.
013203    03  連計日計窓口－負担額           PIC 9(6).
013211*-------------------------------------*
013214*
013215*
013217 01 連計Ｐ－キー IS EXTERNAL.
013218    03 連計Ｐ－費用額ＰＧ              PIC X(8).
013219    03 連計Ｐ－負担額ＰＧ              PIC X(8).
013220    03 連計Ｐ－負担額ＰＧ老人          PIC X(8).
013230    03 連計Ｐ－負担額ＰＧ助成          PIC X(8).
013240*
013250 01 連Ｏ－キー IS EXTERNAL.
013260    03 連Ｏ－負担率                    PIC 9(2).
013270*
013280 01 連レ－キー IS EXTERNAL.
013290    03 連レ－保険種別                  PIC 9(2).
013300*
013310* 確認メッセージ用 (２行)
013320 01 連メ７－キー IS EXTERNAL.
013330    03  連メ７－メッセージ１             PIC X(40).
013340    03  連メ７－メッセージ２             PIC X(40).
013350*
013360*老人保険改正の対応0101
013370*当月の累計金額と回数
013380 01 連老負－キー IS EXTERNAL.
013390    03 連老負－老人負担回数              PIC 9(2).
013400    03 連老負－老人負担額累計            PIC 9(5)V9(3).
013410    03 連老負－老人最終負担日            PIC 9(2).
013420    03 連老負－老人負担額端数            PIC 9(4).
013430    03 連老負－老人負担回数１            PIC 9(2).
013440    03 連老負－老人負担額累計１          PIC 9(5)V9(3).
013450    03 連老負－老人最終負担日１          PIC 9(2).
013460    03 連老負－老人負担額端数１          PIC 9(4).
013470*/老人負担額計算用、月計と日計を区別するフラグ。０：月計。１：日計。
013480 01 連老計算－キー IS EXTERNAL.
013490    03 連老計算－月日Ｆ                  PIC 9.
013500    03 連老計算－枝番フラグ              PIC 9.
013510*/負担金の累計を０：する、１：しない。
013520    03 連老計算－累計フラグ              PIC 9.
013530***/４１老人の月計の合計
013540**    03 連老計算－４１老人月計            PIC 9(5)V9(3).
013550*助成負担区分16の対応。HUJ--で、HIY--R,Gからコールされた場合とそれ以外の振り分け。
013560 01 連計－月計算フラグ                     PIC 9 IS EXTERNAL.
013570**
013580********************
013590* メッセージ表示キー *
013600********************
013610 01 連メ－キー IS EXTERNAL.
013620    03  連メ－メッセージ                 PIC N(20).
013630*
013640 01 連率－負担率取得キー IS EXTERNAL.
013650    03 連率－施術和暦年月.
013660       05 連率－施術和暦               PIC 9.
013670       05 連率－施術年月.
013680          07 連率－施術年              PIC 9(2).
013690          07 連率－施術月              PIC 9(2).
013700    03 連率－患者コード.
013710       05 連率－患者番号               PIC 9(6).
013720       05 連率－枝番                   PIC X.
013730    03 連率－実際負担率                PIC 9(3).
013740    03 連率－実際本体負担率            PIC 9(3).
013750    03 連率－健保負担率                PIC 9(3).
013760    03 連率－２７老負担率              PIC 9(3).
013770    03 連率－助成負担率                PIC 9(3).
013780    03 連率－特別用負担率              PIC 9(3).
013790*
013791*---------------------------------------------------------*
013792* 助成月途中用
013793 01 連計算助成月途中－キー IS EXTERNAL.
013794   03 連計算助成月途中－日計区分           PIC 9.
013795   03 連計算助成月途中－助成月途中開始日   PIC 9(2).
013796   03 連計算助成月途中－助成のみ処理区分   PIC 9.
013797*---------------------------------------------------------*
013798 01 連計算助成累計額－負担額助成       PIC 9(6) IS EXTERNAL.
013799 01 連計算助成累計額－枝番負担累計額   PIC 9(6) IS EXTERNAL.
013801*---------------------------------------------------------*
013802*
013803*
      */負担区分２２(大阪府平成30年4月～)
004887 01 連計算助成累計額－負担額助成１０   PIC 9(6) IS EXTERNAL.
      *
013804******************************************************************
013810*                      PROCEDURE  DIVISION                       *
013820******************************************************************
013830 PROCEDURE               DIVISION.
HILO  *     DISPLAY "210-----------------"
013840************
013850*           *
013860* 初期処理   *
013870*           *
013880************
013890*****************************************************************
013900* 老人保険を含む健康保険の場合は、レセプトを発行しない。
013910     IF (( 連レ－保険種別 >= 01 AND 連レ－保険種別 <= 04 ) OR
013920         ( 連レ－保険種別 >= 06 AND 連レ－保険種別 <= 09 )) AND
013930         ( 連計－公費種別  = 05 )
013940        EXIT PROGRAM
013950     END-IF.
013960*****************************************************************
013970*
013980* 以下よりＰＧ開始
013990     PERFORM 初期化.
014000     PERFORM 累計項目初期化.
014010     PERFORM 基本料金取得.
014020     PERFORM 計算情報取得.
014030     PERFORM 制御情報取得.
014040*/0101老人負担区分
014050     PERFORM 老人負担区分取得.
014060*     PERFORM 計算ＰＧ取得.
014070     PERFORM 長期逓減区分取得.
014071     PERFORM 受診者情報取得.
014072**
014081     PERFORM レセＦ固有情報取得.
014082**
014090* 助成の対応
014100     IF 連計－助成種別 NOT = ZERO
014110         IF (助成負担計算区分Ｗ NOT = ZERO) AND
014120            (助成負担金免除Ｗ   NOT = 1   )
014130             PERFORM 助成負担区分取得
014131**
014132             IF ( 受－枝番作成時枝番 NOT = SPACE ) AND ( 負担区分Ｗ = 05 OR 16 )
014134                IF 連計算助成月途中－助成月途中開始日 = ZERO
014136                   PERFORM 枝番負担累計額計算
014137                END-IF
014141             END-IF
014142**
014143         END-IF
014150     END-IF.
014160     PERFORM 連結項目初期化.
      */施術者番号/151230
           PERFORM 施術所情報取得.
      */運動後療料/20180526
           PERFORM 運動上限算定.
014161*
014175*
014180************
014190*           *
014200* 主処理     *
014210*           *
014220************
014230*
      */負傷登録が無い場合計算しない/131212
           PERFORM 負傷データ取得
      */複数施術者区分２の場合前回の施術者を繰り越す為レコードを作る/151230
      *     IF 部位数Ｗ NOT = ZERO
           IF (部位数Ｗ NOT = ZERO) OR (複数施術者使用区分Ｗ = 2)
014240         MOVE 連計－施術和暦 TO 施記－施術和暦 施術和暦ＣＷ
014250         MOVE 連計－施術年   TO 施記－施術年   施術年ＣＷ
014260         MOVE 連計－施術月   TO 施記－施術月   施術月ＣＷ
014261*
014262*---------------------------------------------------------------------------------------*
014263* 助成月途中変更考慮
014266         IF ( 連計算助成月途中－助成のみ処理区分 NOT = 1 )
014267             MOVE 1   TO 施記－施術日   施術日ＣＷ
014269         ELSE
014271             MOVE 連計算助成月途中－助成月途中開始日  TO 施記－施術日   施術日ＣＷ
014272         END-IF
014273*---------------------------------------------------------------------------------------*
014274*
014280         MOVE 連計－患者番号 TO 施記－患者番号 患者コードＣＷ
014290         MOVE 連計－枝番     TO 施記－枝番     枝番ＣＷ
014300*
014310         START 施術記録Ｆ KEY IS >= 施記－患者コード 施記－施術和暦年月日
014320         END-START
014330*
014340         IF 状態キー = "00"
014351*
014352*---------------------------------------------------------------------------------------*
014353*        / 助成月途中変更以外 /
014355             IF ( 連計算助成月途中－助成のみ処理区分 NOT = 1 ) 
014356                 PERFORM 当月会計ファイル削除
014360             END-IF
014361*---------------------------------------------------------------------------------------*
014362*
      */レセプト往療距離が正しく転記されない/170224
                   MOVE ZERO TO 月往療距離Ｗ
014370*             MOVE "YES" TO 往療加算フラグ
014380             MOVE 1     TO 老人負担回数Ｗ
014390             MOVE 1     TO 助成負担回数Ｗ
014400             MOVE SPACE TO 終了フラグ
014410             PERFORM 施術記録Ｆ読込
014411             IF ( 施記－患者コード    = 患者コードＣＷ   ) AND
014412                ( 施記－施術和暦年月  = 施術和暦年月ＣＷ ) AND
014413                ( 終了フラグ          = SPACE )
014420                PERFORM 施術日セット
014421             END-IF
014430*
014440             PERFORM UNTIL ( 施記－患者コード   NOT = 患者コードＣＷ   ) OR
014450                           ( 施記－施術和暦年月 NOT = 施術和暦年月ＣＷ ) OR
014460                           ( 終了フラグ         NOT = SPACE )
014470*/老人負担金額の計算の改正に伴い変更(下でセットすると不都合)0101
014480                 PERFORM 施術日セット
014490*
014500                 PERFORM 日計項目初期化
014510*
014520                 PERFORM 初検料算定
014530                 PERFORM 再検料算定
014540                 PERFORM 往療料算定
HILO  *                     DISPLAY "210-1 明細書発行加算料算定"  施記－明細書発行加算区分
014550                 PERFORM 後療料算定
014560                 PERFORM その他算定
014570                 PERFORM 逓減開始日取得
      */平成30年6月1日より仕様変更/20180429
                       IF 施記－施術和暦年月 >= 43006
                           PERFORM 運動療法料算定
                       END-IF
      */令和4年10月より明細書発行加算料/20220704
030780                 IF 連計－施術和暦年月 >= 50410
      */明細書発行加算。過去年月のフラグを変更しない。/20240725
                           IF 連計－施術和暦年月 >= 明細変更和暦年月Ｗ
                               PERFORM 明細書発行加算料算定
      */明細書発行加算。過去年月のフラグを変更しない↓↓↓/20240725
                           ELSE
                               MOVE ZERO TO 明細書発行加算料Ｗ 
                               IF 施記－明細書発行加算区分 = 1
                                   MOVE 明細書発行加算料ＡＷ TO 明細書発行加算料Ｗ 
      */20241010↓↓↓
                                   PERFORM 明細書発行加算料算定
      *                             MOVE レセ－明細書発行加算日 TO 明細書発行加算日Ｗ MOVE レセ－明細書発行加算料 TO 明細書発行加算料Ｗ 
      */20241010↑↑↑
                               END-IF
                           END-IF
      */明細書発行加算。過去年月のフラグを変更しない↑↑↑/20240725
                       END-IF
014580*
014590* 部位関連以外の項目を累計しておく。
014600                 PERFORM 累計処理
014610*
014611*
014612***
014613                 INITIALIZE 連計日計窓口－キー
014614                 MOVE ZERO TO 連計－負担金計算区分
014615                 MOVE 1    TO 連計算助成月途中－日計区分
014616***
014620********************
014630* 老人負担額の計算 *
014640********************
014650* ２７老人・４１老人については、日毎に負担額の計算が必要
014660*
014670                 EVALUATE 連レ－保険種別 ALSO 公費種別Ｗ
014680* ４１老人
014690                 WHEN 51         ALSO ZERO
014700*/４１端数区分が１の時は日々の計算は必要ない。
014710*******                 IF ４１端数区分Ｗ NOT = 1
014720                         PERFORM 日計処理
014730                         PERFORM 健保負担額計算
014740                         PERFORM 老人負担額計算
014790*******                 END-IF
014800* 助成
014810                 WHEN 50 THRU 60 ALSO ZERO
014839                     PERFORM 日計処理
      */負担区分10を指定負担率（１０円単位）に変更/20191126
014840*                     IF 負担区分Ｗ = 09 OR 10 
014840                     IF 負担区分Ｗ = 09
014850                          PERFORM 健保負担額計算
014860                          PERFORM 老人負担額計算
014980                     ELSE
014990                         IF (助成負担計算区分Ｗ NOT = ZERO) AND
015000                            (助成負担金免除Ｗ   NOT = 1   )
015020                             PERFORM 助成負担額計算
015070                         END-IF
015080                     END-IF
015090* ２７助成
015100*/0210の改正で老人の日別計算は必要ないが助成の計算の為に日毎にコール。老人分の負担額は後で改めて計算する。
015110                 WHEN 50         ALSO 05
015120                 WHEN 52 THRU 60 ALSO 05
015130                     PERFORM 日計処理
015140                     PERFORM 老人助成負担額計算
015150***                 COMPUTE 老人負担回数Ｗ = 老人負担回数Ｗ + 1
015160                 WHEN OTHER
015170*********
015180                 PERFORM 日計処理
015190                 IF 受－保険種別 = 05
015200                     PERFORM 老人負担額計算
015210                 ELSE
015220                     PERFORM 健保負担額計算
015230                 END-IF
015240*********
015260                 END-EVALUATE
015270*
015280*
015292*---------------------------------------------------------------------------------------*
015293*            / 助成月途中変更以外 /
015294                 IF ( 連計算助成月途中－助成のみ処理区分 NOT = 1 ) 
015296                     PERFORM 会計データＦ書込
015297                 END-IF
015298*---------------------------------------------------------------------------------------*
015299*
015320*
015330                 PERFORM 施術記録Ｆ読込
015340*/老人負担金額の計算の改正に伴い変更(ここでセットすると不都合)0101
015350*/助成負担の初検日と定率制の上限金額を超える日の判定にしか使っていない。
015360*                 PERFORM 施術日セット
015370*
015380             END-PERFORM
015390         END-IF
015400*
015401*------------------------------------------------------------------------------*
015402*
015411         INITIALIZE 連計日計窓口－キー
015412         MOVE ZERO TO 連計－負担金計算区分
015413         MOVE ZERO TO 連計算助成月途中－日計区分
015420*


      */令和6年10月改定対応↓↓↓*/20240712
015440* 部位毎の継続月数を算出
      */令和6年9月は継続月数のみ書き込む
      *         IF 連計－施術和暦年月 >= 50610
               IF 連計－施術和暦年月 >= 50609
050530             PERFORM 部位実日数セット
050531             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 7
050532                 MOVE 部位実日数Ｗ(部位ＣＮＴ) TO レセ－部位実日数(部位ＣＮＴ)
050533             END-PERFORM
                   PERFORM 部位継続月数判定
               END-IF
      */令和6年10月改定対応↑↑↑*/20240712
015430*****************************************************************
015440* 多部位関連項目の部位毎１００％の合計から逓減率、長期逓減率を乗じた
015450* 金額を部位毎に算出する。
015460*****************************************************************
015470*
015480         PERFORM 多部位合計処理
015490*
015500* 各部位毎の（１００％の金額に）逓減率（多部位・長期）を
015510* 乗けた金額と部位以外項目を加算する。
015520*
015530         COMPUTE 連計－費用額 = 部位合計長期Ｗ１ + 部位合計長期Ｗ２ +
015540                                部位合計長期Ｗ３ + 部位合計長期Ｗ３１ +
015550                                部位合計長期Ｗ４ + 部位合計長期Ｗ４１ +
015560                                部位合計長期Ｗ４２ + 部位合計長期Ｗ５ + 
015570                                部位合計長期Ｗ５１ + 部位合計長期Ｗ５２ + 
015580                                部位合計長期Ｗ５３ + 初検料ＧＷ + 初検加算料ＧＷ +
015590                                再検料ＧＷ + 往療料ＧＷ + 往療加算料ＧＷ +
015600                                金属副子料ＧＷ + 初回処置料ＧＷ + 施術情報提供料ＧＷ +
015610                                初検時相談料ＧＷ + 運動療法料ＧＷ +
      */６部位～/120804
                                      部位合計長期Ｗ６２ + 部位合計長期Ｗ６３ +
                                      部位合計長期Ｗ７２ + 部位合計長期Ｗ７３ +
                                      明細書発行加算料ＧＷ
hilo  *         DISPLAY "210-3-1 連計－費用額=" 連計－費用額 "-------------------------------"
hilo  *         DISPLAY "210-1 連計－費用額=" 連計－費用額 
      *                 " 部位合計長期Ｗ４ "    部位合計長期Ｗ４  
      *                 " 部位合計長期Ｗ４１ "  部位合計長期Ｗ４１
      *                 " 部位合計長期Ｗ４２ "  部位合計長期Ｗ４２
015620*
015630         EVALUATE 連レ－保険種別
015640         WHEN 01 THRU 04
015641         WHEN 06 THRU 09
015650         WHEN 85 THRU 91
015660             PERFORM 健保負担額計算
015670             MOVE 連計－費用額 TO 連料－費用額
015680             MOVE 連計－負担額 TO 連料－一部負担金
015690             COMPUTE 連料－請求額 = 連計－費用額 - 連計－負担額
015700* ２７老人
015710         WHEN 05
015720*/施術日が無い場合もあるのでデータチェックを追加する0211
015730*             PERFORM データチェック
015740*             IF 実行キーＷ = "YES"
015750*                 PERFORM 老人負担額計算
015760*                 MOVE 連計－費用額     TO 連料－費用額
015770*                 MOVE 連計－負担額老人 TO 連料－一部負担金
015780*                 COMPUTE 連料－請求額 = 連計－費用額 - 連計－負担額老人
015790*             END-IF
015791*
015792*     連計－費用額 NOT = ZERO に変更 2011.02.10
015794             IF 連計－費用額 NOT = ZERO
015795                PERFORM 老人負担額計算
015796                MOVE 連計－費用額     TO 連料－費用額
015797                MOVE 連計－負担額老人 TO 連料－一部負担金
015798                COMPUTE 連料－請求額 = 連計－費用額 - 連計－負担額老人
015799             END-IF
015800*
015803*     ４１老人
015810         WHEN 51
015820             PERFORM 健保負担額計算
015830             MOVE 連計－費用額   TO 連料－費用額
015840             MOVE 連計－負担額   TO 連料－一部負担金
015850             MOVE 連計－請求額   TO 連料－請求額
015860*/資格証明の対応/*４１老人と４１に準ずる助成で)
015870*/２７老人でなく((国保＆国保組合以外)または退職)＆資格証明区分が１の時は全額患者負担
015880             PERFORM 受診者情報取得
015890             IF (((受－保険種別 = 01)  AND (受－保険者番号(3:1) NOT = "3")) OR
015900                 ( 受－保険種別 = 08)) AND
015910                 ( 受－資格証明区分 = 1)
015920                 MOVE 連計－負担額 TO 連料－受給者負担額
015930                 MOVE ZERO         TO 連料－助成請求金額
015940             ELSE
015950*/４１端数区分が１の時のみ月の合計から負担額を計算する。
015960                 IF ４１端数区分Ｗ = 1
015970                     PERFORM 老人負担額計算
015980*/４１の負担額が健保の負担額を超えないように調整
015990                     IF 連計－負担額助成 > 連計－負担額
016000                         MOVE 連計－負担額 TO 連計－負担額助成
016010                     END-IF
016020                     MOVE 連計－負担額助成 TO 連料－受給者負担額
016030                     IF 連計－負担額 > 連料－受給者負担額
016040                         COMPUTE 連料－助成請求金額 = 連計－負担額 - 連料－受給者負担額
016050                     ELSE
016060                         MOVE ZERO TO 連料－助成請求金額
016070                     END-IF
016080                 ELSE
016090*/負担額の月計は１円単位は小数点以下を含めた日々の合計の小数点以下を切り上げ
016100*/１０円単位は日々の負担額の合計(１０円単位は小数点以下は入らない)。
016110                     IF 連老負－老人負担額累計(6:3) NOT = ZERO
016120                         MOVE ZERO TO 連老負－老人負担額累計(6:3)
016130                         COMPUTE 連老負－老人負担額累計 = 連老負－老人負担額累計 + 1
016140                     ELSE
016150                         MOVE ZERO TO 連老負－老人負担額累計(6:3)
016160                     END-IF
016170*/４１の負担額が健保の負担額を超えないように調整
016180                     IF 連老負－老人負担額累計 > 連計－負担額
016190                         MOVE 連計－負担額 TO 連老負－老人負担額累計
016200                     END-IF
016210                     MOVE 連老負－老人負担額累計 TO 負担額助成ＧＷ
016220*    /日々の計算の積み上げの為、健保の負担額と助成の負担額は合わない。0101
016230*    /健保の負担額＞助成の負担額、の場合の助成の請求額はゼロを転記する。
016240                     MOVE 負担額助成ＧＷ TO 連料－受給者負担額
016250                     IF 連計－負担額 > 負担額助成ＧＷ
016260                         COMPUTE 連料－助成請求金額 = 連計－負担額 - 負担額助成ＧＷ
016270                     ELSE
016280                         MOVE ZERO TO 連料－助成請求金額
016290                     END-IF
016300                 END-IF
016310/    資格証明の対応/*４１老人と４１に準ずる助成で)
016320             END-IF
016330         WHEN 50
016340         WHEN 52 THRU 60
016350* ２７助成
016360             IF 公費種別Ｗ = 05
016370                 MOVE 負担額助成ＧＷ TO 負担額助成ＧＷ２
016380*                 PERFORM 老人負担額計算
016390*/２７老人＋４１に準ずる助成の対応/071005
016400                 IF 負担区分Ｗ = 09 OR 10
016410                     PERFORM 老人負担額計算
016420                     MOVE 連計－負担額助成 TO 連料－受給者負担額
016430                     MOVE 連計－請求額助成 TO 連料－助成請求金額
016440                 ELSE
016450                     PERFORM 老人助成負担額計算
016460                 END-IF
016470*                 PERFORM 老人助成負担額計算
016480                 MOVE 連計－費用額     TO 連料－費用額
016490                 MOVE 連計－負担額老人 TO 連料－一部負担金
016500                 COMPUTE 連料－請求額 = 連計－費用額 - 連計－負担額老人
016510*/２７老人＋４１に準ずる助成の対応/071005
016520*/後期高齢＋指定負担率の対応/080523
016540                 IF (負担区分Ｗ = 09 OR 10 OR 15 OR 16 OR 05) OR
      */負担区分24&負担金免除2追加180821
                          (負担区分Ｗ = 24 AND 助成負担金免除Ｗ = 2)
016550**                 CONTINUE
016560*/後期高齢＋指定負担率の対応/080523
016570*/16:追加/091014*/24追加180821
016580                     IF 負担区分Ｗ = 15 OR 16 OR 24
016590                         MOVE 連計－負担額助成 TO 連料－受給者負担額
016600                         COMPUTE 連料－助成請求金額 = 連計－負担額老人 - 連料－受給者負担額
016610                     END-IF
016611                     IF 負担区分Ｗ = 05
016612                         MOVE 連計－負担額助成 TO 連料－受給者負担額
016613                         MOVE 連計－請求額助成 TO 連料－助成請求金額
016617                     END-IF
016620                 ELSE
016630*/２７助成で助成負担額が老人負担額を超えないように補正/0501
016640                     IF 負担額助成ＧＷ２ > 連計－負担額老人
016650                          MOVE 連計－負担額老人 TO 負担額助成ＧＷ２
016660                     END-IF
016670*/日々の計算で足し込んでおいた値を転記する0210
016680                     MOVE 負担額助成ＧＷ２ TO 連料－受給者負担額
016690                     COMPUTE 連料－助成請求金額 = 連計－負担額老人 - 負担額助成ＧＷ２
016700*/老人保険の改正0210↑↑
016710                 END-IF
016720* 助成
016730             ELSE
016740                 PERFORM 健保負担額計算
016750*/0708東京都マル子の対応191001より
      */負担区分24&負担金免除2追加180821
016770*                 EVALUATE 負担区分Ｗ
016770                 EVALUATE 負担区分Ｗ ALSO 助成負担金免除Ｗ
016771                 WHEN 05 ALSO ANY
016772                 WHEN 15 ALSO ANY
016773                 WHEN 16 ALSO ANY
016773                 WHEN 24 ALSO 2
016780                     PERFORM 助成負担額計算
016781*
016782                     MOVE 連計－費用額 TO 連料－費用額
016783                     MOVE 連計－負担額 TO 連料－一部負担金
016784                     COMPUTE 連料－請求額 = 連計－費用額 - 連計－負担額
016786                     IF 連計－負担額助成 > 連計－負担額
016787                         MOVE 連計－負担額 TO 連計－負担額助成
016788                     END-IF
016789                     MOVE 連計－負担額助成 TO 連料－受給者負担額
016790                     IF 連計－負担額 > 連料－受給者負担額
016791                         COMPUTE 連料－助成請求金額 = 連計－負担額 - 連料－受給者負担額
016792                     ELSE
016793                         MOVE ZERO TO 連料－助成請求金額
016794                     END-IF
016799*
016807*/４１老に準ずる助成の対応
016808                 WHEN 09 ALSO ANY
      */負担区分10を指定負担率（１０円単位）に変更/20191126
016809*                 WHEN 10 ALSO ANY
016820                     MOVE 連計－費用額   TO 連料－費用額
016830                     MOVE 連計－負担額   TO 連料－一部負担金
016840                     MOVE 連計－請求額   TO 連料－請求額
016850*/資格証明の対応/*４１老人と４１に準ずる助成で)
016860*/２７老人でなく((国保＆国保組合以外)または退職)＆資格証明区分が１の時は全額患者負担
016870                     PERFORM 受診者情報取得
016880                     IF (((受－保険種別 = 01)  AND (受－保険者番号(3:1) NOT = "3")) OR
016890                         ( 受－保険種別 = 08)) AND
016900                         ( 受－資格証明区分 = 1)
016910                         MOVE 連計－負担額 TO 連料－受給者負担額
016920                         MOVE ZERO         TO 連料－助成請求金額
016930                     ELSE
016940*
016950*/４１端数区分が１の時のみ月の合計から負担額を計算する。0210
016960                         IF ４１端数区分Ｗ = 1
016970                             PERFORM 老人負担額計算
016980*/４１の負担額が健保の負担額を超えないように調整
016990                             IF 連計－負担額助成 > 連計－負担額
017000                                 MOVE 連計－負担額 TO 連計－負担額助成
017010                             END-IF
017020                             MOVE 連計－負担額助成 TO 連料－受給者負担額
017030                             IF 連計－負担額 > 連料－受給者負担額
017040                                 COMPUTE 連料－助成請求金額 = 連計－負担額 - 連料－受給者負担額
017050                             ELSE
017060                                 MOVE ZERO TO 連料－助成請求金額
017070                             END-IF
017080                         ELSE
017090*/負担額の月計は１円単位は小数点以下を含めた日々の合計の小数点以下を切り上げ
017100*/１０円単位は日々の負担額の合計(１０円単位は小数点以下は入らない)。
017110                             IF 連老負－老人負担額累計(6:3) NOT = ZERO
017120                                 MOVE ZERO TO 連老負－老人負担額累計(6:3)
017130                                 COMPUTE 連老負－老人負担額累計 = 連老負－老人負担額累計 + 1
017140                             ELSE
017150                                 MOVE ZERO TO 連老負－老人負担額累計(6:3)
017160                             END-IF
017170*/４１の負担額が健保の負担額を超えないように調整
017180                             IF 連老負－老人負担額累計 > 連計－負担額
017190                                 MOVE 連計－負担額 TO 連老負－老人負担額累計
017200                             END-IF
017210                             MOVE 連老負－老人負担額累計 TO 負担額助成ＧＷ
017220*
017230*/日々の計算の積み上げの為、健保の負担額と助成の負担額は合わない。0101
017240*/健保の負担額＞助成の負担額、の場合の助成の請求額はゼロを転記する。
017250                             MOVE 負担額助成ＧＷ TO 連料－受給者負担額
017260                             IF 連計－負担額 > 負担額助成ＧＷ
017270                                 COMPUTE 連料－助成請求金額 = 連計－負担額 
017280                                                            - 負担額助成ＧＷ
017290                             ELSE
017300                                 MOVE ZERO TO 連料－助成請求金額
017310                             END-IF
017320                         END-IF
017330*/資格証明の対応/*４１老人と４１に準ずる助成で)
017340                     END-IF
017341                 WHEN OTHER
017360                     MOVE 連計－費用額 TO 連料－費用額
017370                     MOVE 連計－負担額 TO 連料－一部負担金
017380                     COMPUTE 連料－請求額 = 連計－費用額 - 連計－負担額
017390*/助成の負担額が健保の負担額を超えないように調整
017400                     IF 負担額助成ＧＷ > 連計－負担額
017410                         MOVE 連計－負担額 TO 負担額助成ＧＷ
017420                     END-IF
017430                     MOVE 負担額助成ＧＷ TO 連料－受給者負担額
017440                     COMPUTE 連料－助成請求金額 = 連計－負担額 - 負担額助成ＧＷ
017450                 END-EVALUATE
017460             END-IF
017470         WHEN OTHER
017480             CONTINUE
017490         END-EVALUATE
017500*
017510         PERFORM 連結項目セット
017520* 全柔のみの機能（ＯＣＲコードの一部）
017530         PERFORM 負担率ＯＣＲセット
017540*
017550*/平成１２年６月より５部位目の後療料は算定しない
017560*/５部位目の多部位逓減率で判定する
017570         IF 多部位逓減率ＫＷ(5) = ZERO
017580             MOVE ZERO TO 連料－部位５２
017590             INITIALIZE   連料－部位５２
017600         END-IF
017610*
017620*/レセプトファイル作成
017630         PERFORM レセプトファイル作成
017631*
017632*
      */負傷部位がない場合はファイルをクリアする/131212
           ELSE
014355         IF ( 連計算助成月途中－助成のみ処理区分 NOT = 1 ) 
014356             PERFORM 当月会計ファイル削除
      */施術者番号/151230
                   PERFORM 会計施術者番号書込
014360         END-IF
018390         PERFORM 連結項目セット
018420         PERFORM レセプトファイル作成
           END-IF.
017650************
017660*           *
017670* 終了処理   *
017680*           *
017690************
017700*
017710     PERFORM 終了処理.
017720     MOVE 1 TO PROGRAM-STATUS.
017730     EXIT PROGRAM.
017740*
017750*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
017760*================================================================*
017770 初期化 SECTION.
017780*
      */運動後療更新の為/20180526
017790     OPEN INPUT 施術記録Ｆ.
      *     OPEN I-O 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
017820     OPEN INPUT 料金マスタ.
017830         MOVE NC"料金" TO ファイル名Ｗ.
017840         PERFORM オープンチェック.
017850     OPEN INPUT 計算マスタ.
017860         MOVE NC"計算" TO ファイル名Ｗ.
017870         PERFORM オープンチェック.
017880*     OPEN INPUT 計算ＰＧマスタ.
017890*         MOVE NC"計Ｐ" TO ファイル名Ｗ.
017900*         PERFORM オープンチェック.
017901*
017910     OPEN I-O 受診者情報Ｆ.
017920         MOVE NC"受診" TO ファイル名Ｗ.
017930         PERFORM オープンチェック.
017931*
017940     OPEN INPUT 負傷データＦ.
017950         MOVE NC"負傷" TO ファイル名Ｗ.
017960         PERFORM オープンチェック.
017970     OPEN INPUT 制御情報マスタ.
017980         MOVE NC"制御" TO ファイル名Ｗ.
017990         PERFORM オープンチェック.
018000     OPEN INPUT 元号マスタ.
018010         MOVE NC"元号" TO ファイル名Ｗ.
018020         PERFORM オープンチェック.
018060*/0101老人負担区分
018070     OPEN INPUT 老人負担区分マスタ.
018080         MOVE NC"老人負担" TO ファイル名Ｗ.
018090         PERFORM オープンチェック.
018100     OPEN INPUT 市町村マスタ.
018110         MOVE NC"名称" TO ファイル名Ｗ.
018120         PERFORM オープンチェック.
018130*
018142     OPEN INPUT 会計領収Ｆ.
018143         MOVE NC"会計領収" TO ファイル名Ｗ.
018144         PERFORM オープンチェック.
018150     OPEN I-O 会計データＦ.
018160         MOVE NC"会計" TO ファイル名Ｗ.
018170         PERFORM オープンチェック.
018180     OPEN I-O レセプトＦ.
018190         MOVE NC"レセ" TO ファイル名Ｗ.
018200         PERFORM オープンチェック.
018210********
018220     OPEN INPUT 保険者特別負担マスタ.
018230         MOVE NC"保特別" TO ファイル名Ｗ.
018240         PERFORM オープンチェック.
006770     OPEN INPUT 施術所情報マスタ
006780         MOVE NC"施情" TO ファイル名Ｗ.
006790         PERFORM オープンチェック.
006650     OPEN INPUT 確認施術記録Ｆ.
006660         MOVE NC"確認施記" TO ファイル名Ｗ.
006670         PERFORM オープンチェック.
018250*
018180     OPEN INPUT 参照レセプトＦ.
018190         MOVE NC"参照レセ" TO ファイル名Ｗ.
018200         PERFORM オープンチェック.
017940     OPEN INPUT 参照負傷データＦ.
017950         MOVE NC"参照負傷" TO ファイル名Ｗ.
017960         PERFORM オープンチェック.
018180     OPEN INPUT 枝番参照レセプトＦ.
018190         MOVE NC"枝番参照レセ" TO ファイル名Ｗ.
018200         PERFORM オープンチェック.
017940     OPEN INPUT 枝番参照負傷データＦ.
017950         MOVE NC"枝番参照負傷" TO ファイル名Ｗ.
017960         PERFORM オープンチェック.
018261*-------------------------------------------------------------------------------*
018262* EXTERNAL用
018263*
018265     OPEN INPUT ＥＸ保険者マスタ.
018266         MOVE NC"ＥＸ保険者" TO ファイル名Ｗ.
018267         PERFORM オープンチェック.
018268     OPEN INPUT ＥＸ負担率マスタ.
018269         MOVE NC"ＥＸ負担率" TO ファイル名Ｗ.
018270         PERFORM オープンチェック.
018271     OPEN INPUT ＥＸ受診者情報Ｆ.
018272         MOVE NC"ＥＸ受診者" TO ファイル名Ｗ.
018273         PERFORM オープンチェック.
018274     OPEN INPUT ＥＸ施術記録Ｆ.
018275         MOVE NC"ＥＸ施術記録" TO ファイル名Ｗ.
018276         PERFORM オープンチェック.
018277     OPEN INPUT ＥＸ料金マスタ.
018278         MOVE NC"ＥＸ料金マスタ" TO ファイル名Ｗ.
018279         PERFORM オープンチェック.
018280     OPEN INPUT ＥＸ計算マスタ.
018281         MOVE NC"ＥＸ計算マスタ" TO ファイル名Ｗ.
018282         PERFORM オープンチェック.
018283     OPEN INPUT ＥＸ市町村マスタ.
018284         MOVE NC"ＥＸ市町村マスタ" TO ファイル名Ｗ.
018285         PERFORM オープンチェック.
018286     OPEN INPUT ＥＸ制御情報マスタ.
018287         MOVE NC"ＥＸ制御情報" TO ファイル名Ｗ.
018288         PERFORM オープンチェック.
018289     OPEN INPUT ＥＸ老人負担区分マスタ.
018290         MOVE NC"ＥＸ老人負担区分" TO ファイル名Ｗ.
018291         PERFORM オープンチェック.
018292     OPEN INPUT ＥＸ保険者特別負担マスタ.
018293         MOVE NC"ＥＸ保険者特別負担" TO ファイル名Ｗ.
018294         PERFORM オープンチェック.
018311*
018312*-------------------------------------------------------------------------------*
056157     OPEN INPUT 長頻参照受診者情報Ｆ.
056158         MOVE NC"参照受診" TO ファイル名Ｗ.
056159         PERFORM オープンチェック.
008320*     OPEN OUTPUT 作業ファイル１.
008330*         MOVE NC"作１" TO ファイル名Ｗ.
008340*         PERFORM オープンチェック.
018313*
018314     MOVE ZERO TO 連計－費用額 連計－負担額 連計－請求額
018315                  連計－費用額老人 連計－負担額老人 連計－請求額老人
018316                  連計－費用額助成 連計－負担額助成 連計－請求額助成.
018317*
018320     MOVE SPACE TO 連料－料金１.
018330     MOVE SPACE TO 連料－料金２.
018340     MOVE SPACE TO 連料－料金３.
           MOVE SPACE TO 連料６部位７部位追加項目.
018350     MOVE ZERO  TO 連料－初検時相談料.
           MOVE ZERO  TO 連料－運動後療.
           MOVE ZERO  TO 連料－明細書発行.
018360     INITIALIZE    連料－料金１.
018370     INITIALIZE    連料－料金２.
018380     INITIALIZE    連料－料金３.
           INITIALIZE    連料６部位７部位追加項目.
018390     INITIALIZE    連料－初検時相談料.
           INITIALIZE    連料－運動後療.
           INITIALIZE    連料－明細書発行.
018400*
018410     MOVE 連計－保険種別 TO 保険種別Ｗ.
018420     MOVE 連計－公費種別 TO 公費種別Ｗ.
018430     MOVE 連計－助成種別 TO 助成種別Ｗ.
018440*/初期化必要
018450     MOVE ZERO           TO 連老計算－累計フラグ.
018460* DEBUG
018470*     DISPLAY NC"保険種別Ｗ" 保険種別Ｗ UPON MSGBOX.
018480*     DISPLAY NC"公費種別Ｗ" 公費種別Ｗ UPON MSGBOX.
018490*     DISPLAY NC"助成種別Ｗ" 助成種別Ｗ UPON MSGBOX.
018500*
018505*
      */施術者番号/160102
           INITIALIZE 退避施術者番号Ｗ.

018510*================================================================*
018520 オープンチェック SECTION.
018530*
018540     IF 状態キー  NOT =  "00"
018550         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
018560         DISPLAY NC"状態キー：" 状態キー           UPON CONS
018570         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
018580*-----------------------------------------*
018590         CALL "actcshm"  WITH C LINKAGE
018600*-----------------------------------------*
018610         ACCEPT  キー入力 FROM CONS
018620         PERFORM ファイル閉鎖
018630         MOVE ZERO TO PROGRAM-STATUS
018640         EXIT PROGRAM.
018650*================================================================*
018660 ファイル閉鎖 SECTION.
018670*
018680     CLOSE 施術記録Ｆ    料金マスタ    計算マスタ      
018690           受診者情報Ｆ  負傷データＦ  制御情報マスタ
018700           元号マスタ    市町村マスタ
018710*     計算ＰＧマスタ
018720*/0101老人負担区分
018730           老人負担区分マスタ 確認施術記録Ｆ.
018740     CLOSE 会計データＦ レセプトＦ 会計領収Ｆ 保険者特別負担マスタ 施術所情報マスタ.
018741*
018743*-------------------------------------------------------------------------------*
018744* EXTERNAL用
018746     CLOSE ＥＸ保険者マスタ ＥＸ負担率マスタ   ＥＸ受診者情報Ｆ
018749           ＥＸ施術記録Ｆ   ＥＸ料金マスタ     ＥＸ計算マスタ
018750           ＥＸ市町村マスタ ＥＸ制御情報マスタ ＥＸ老人負担区分マスタ
018766           ＥＸ保険者特別負担マスタ.
      *
           CLOSE 参照レセプトＦ 参照負傷データＦ 枝番参照レセプトＦ 枝番参照負傷データＦ 
                 長頻参照受診者情報Ｆ.
018767*
018783*================================================================*
018784 終了処理 SECTION.
018785*
018786     PERFORM ファイル閉鎖.
018790*================================================================*
018800 エラー表示 SECTION.
018810*
018820     DISPLAY ファイル名Ｗ NC"ファイル書込エラー"   UPON CONS.
018830     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
018840     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
018850     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
018860*-----------------------------------------*
018870     CALL "actcshm"  WITH C LINKAGE.
018880*-----------------------------------------*
018890     ACCEPT  キー入力 FROM CONS.
018900     PERFORM ファイル閉鎖.
018910     MOVE ZERO TO PROGRAM-STATUS
018920     EXIT PROGRAM.
018930*================================================================*
018940 エラー表示Ｒ SECTION.
018950*
018960     DISPLAY ファイル名Ｗ NC"ファイル読込エラー"   UPON CONS.
018970     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
018980     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
018990     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019000*-----------------------------------------*
019010     CALL "actcshm"  WITH C LINKAGE.
019020*-----------------------------------------*
019030     ACCEPT  キー入力 FROM CONS.
019040     PERFORM ファイル閉鎖.
019050     MOVE ZERO TO PROGRAM-STATUS
019060     EXIT PROGRAM.
019070*================================================================*
019080 エラー表示その他 SECTION.
019090*
019100     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
019110     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019120*-----------------------------------------*
019130     CALL "actcshm"  WITH C LINKAGE.
019140*-----------------------------------------*
019150     ACCEPT  キー入力 FROM CONS.
019160     PERFORM ファイル閉鎖.
019170     EXIT PROGRAM.
019180*================================================================*
019190 施術記録Ｆ読込 SECTION.
019200*
019210     READ 施術記録Ｆ NEXT
019220     AT END
019230         MOVE "YES" TO 終了フラグ
019240     END-READ.
019250*
019260*================================================================*
019270 初検料算定 SECTION.
019280*
019290     MOVE ZERO TO 初検料Ｗ.
019300     MOVE ZERO TO 初検加算料Ｗ.
019310     MOVE ZERO TO 初検時相談料Ｗ.
019320*
019330*/日毎項目の初期化/
019340     MOVE ZERO TO  初検時間外ＮＷ 初検休日ＮＷ 初検深夜ＮＷ.
019350
019360*
019370     IF ( 施記－診療区分     = 2 ) AND
019380        ( 初検請求区分Ｗ NOT = 1 ) AND
019390        ( 施記－初検料請求区分  = 1 )
019400         COMPUTE 初検料Ｗ = 初検料Ｗ + 初検料ＡＷ
019410
019420* 初検加算
019430         EVALUATE 施記－初検加算
019440         WHEN ZERO
019450             CONTINUE
019460         WHEN 1
019470             MOVE 1 TO 初検時間外Ｗ 初検時間外ＮＷ
019480             COMPUTE 初検加算料Ｗ = 初検加算料Ｗ + 時間外ＡＷ
019490         WHEN 2
019500             MOVE 1 TO 初検休日Ｗ 初検休日ＮＷ
019510             COMPUTE 初検加算料Ｗ = 初検加算料Ｗ + 休日ＡＷ
019520         WHEN 3
019530             MOVE 1 TO 初検深夜Ｗ 初検深夜ＮＷ
019540             COMPUTE 初検加算料Ｗ = 初検加算料Ｗ + 深夜ＡＷ
019550         WHEN OTHER
019560             CONTINUE
019570         END-EVALUATE
019580     END-IF.
019590* 初検時相談料 /080527
019600     IF (施記－診療区分 = 2 ) AND (施記－初検時相談料区分 NOT = 1)
019610         COMPUTE 初検時相談料Ｗ = 初検時相談料Ｗ + 初検時相談料ＡＷ
019620     END-IF.
019630*================================================================*
019640 再検料算定 SECTION.
019650*
019660     MOVE ZERO TO 再検料Ｗ.
019670*
019680     IF 施記－再検料請求  = 1
019690         COMPUTE 再検料Ｗ = 再検料Ｗ + 再検料ＡＷ
019700     END-IF.
019710*
019720*================================================================*
019730 往療料算定 SECTION.
019740******************************************************************
019750* 往療加算は、端数を含めた金額を基本とし、計算する。
019760* 100/100の場合は、100を加算料（夜間・暴風雨）の分として計算する。
019770******************************************************************
019780*
019790     MOVE ZERO TO 往療料Ｗ.
019800     MOVE ZERO TO 往療加算料Ｗ.
019810*/日毎項目の初期化
019820     MOVE ZERO TO 往療夜間ＮＷ 往療難路ＮＷ 往療暴風雨ＮＷ 往療時間外ＮＷ.
      */初期化項目追加/160221
           MOVE ZERO TO 往療距離Ｗ.
019830
019840*
019850     IF 施記－往療距離 NOT = ZERO
019860* 整数にするため、１０を掛ける
019870         MOVE 施記－往療距離 TO 往療距離Ｗ
      */レセプト往療距離が正しく転記されない/170224
                                      月往療距離Ｗ
019880*/18年6月より8kmを超える往療料は同額↓↓↓060601/060522
019890         IF (連計－施術和暦年月 >= 41806) AND (施記－往療距離 >= 6.1)
019900             COMPUTE 往療被除数Ｗ = 6.1 * 10
019880*/令和2年6月より4kmを超える往療料は同額↓↓↓/20200428
019890             IF (連計－施術和暦年月 >= 50206) AND (施記－往療距離 >= 4.1)
019900                 COMPUTE 往療被除数Ｗ = 4.1 * 10
019930             END-IF
019880*/令和2年6月より4kmを超える往療料は同額↑↑↑/20200428
019910         ELSE
019920             COMPUTE 往療被除数Ｗ = 施記－往療距離 * 10
019930         END-IF
019940*/18年6月より8kmを超える往療料は同額↑↑↑↓１行コメント060601/060522
019950*         COMPUTE 往療被除数Ｗ = 施記－往療距離 * 10
019960         COMPUTE 往療除数Ｗ   = 往療基本距離ＫＷ * 10
019970         DIVIDE 往療除数Ｗ INTO 往療被除数Ｗ GIVING 往療商Ｗ
019980                           REMAINDER 往療剰余Ｗ
019990         COMPUTE 往療料Ｗ = 往療基本料金ＡＷ +
020000                          ( 往療追加料金ＡＷ * ( 往療商Ｗ - 1 ) )
020010         IF 往療剰余Ｗ > ZERO
020020             COMPUTE 往療料Ｗ = 往療料Ｗ + 往療追加料金ＡＷ
020030         END-IF
020040* 往療加算（夜間・暴風雨等）
020050* レセプトの場合、距離の加算は、往療料に含め、往療加算は、夜間・暴風雨の
020060* 加算のみとする
020070* 往療加算は、月１回（社団のみ）
020080*         IF 往療加算フラグ NOT = SPACE
020090             IF 施記－往療加算 NOT = ZERO
020100                 COMPUTE 往療加算料Ｗ = 往療料Ｗ * ( 往療加算率ＡＷ / 100 )
020110                 COMPUTE 往療加算率Ｗ     = 往療加算率ＡＷ - 100
020120                 COMPUTE 往療加算率中間Ｗ = 往療加算率Ｗ / 100
020130                 COMPUTE 往療加算料Ｗ     = 往療料Ｗ * 往療加算率中間Ｗ
020140*
020150                 EVALUATE 施記－往療加算
020160                 WHEN 1
020170                     MOVE 1 TO 往療夜間Ｗ   往療夜間ＮＷ
020180                 WHEN 2
020190                     MOVE 1 TO 往療難路Ｗ   往療難路ＮＷ
020200                 WHEN 3
020210                     MOVE 1 TO 往療暴風雨Ｗ 往療暴風雨ＮＷ
020220                 WHEN 4
020230                     MOVE 1 TO 往療時間外Ｗ 往療時間外ＮＷ
020240                 WHEN OTHER
020250                     CONTINUE
020260                 END-EVALUATE
020270*
020280             END-IF
020290*             MOVE SPACE TO 往療加算フラグ
020300*         END-IF
020310*
020320         COMPUTE 往療回数ＧＷ = 往療回数ＧＷ + 1
020330*
020340     END-IF.
020350*
020360*================================================================*
020370 基本料金取得 SECTION.
020380*
020390*
020400     MOVE 01             TO 料－区分コード.
020410     MOVE ZEROS          TO 料－負傷種別.
020420     MOVE ZEROS          TO 料－部位.
020430     MOVE ZEROS          TO 料－左右区分.
020440     MOVE ZEROS          TO 料－負傷位置番号.
020450     MOVE 連計－施術和暦 TO 料－開始和暦 施術和暦ＣＷ.
020460     MOVE 連計－施術年   TO 料－開始年   施術年ＣＷ.
020470     MOVE 連計－施術月   TO 料－開始月   施術月ＣＷ.
020480*
020490     START 料金マスタ KEY IS <= 料－区分コード 料－部位コード 料－開始和暦年月 REVERSED
020500     END-START.
020510*
020520     IF 状態キー = "00"
020530         READ 料金マスタ NEXT
020540         AT END
020550*/エラー表示の修正
020560             PERFORM コンソールエラー確認
020570             DISPLAY "施術年月に対応した料金がみつかりません"
020580                     " 受診者№=" 連計－患者コード
020590                     " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
020591*-----------------------------------------*
020592             CALL "actcshm"  WITH C LINKAGE
020593*-----------------------------------------*
020600             PERFORM 終了処理
020610             MOVE ZERO TO PROGRAM-STATUS
020620             EXIT PROGRAM
020630         NOT AT END
020640*
020650             IF ( 施術和暦年月ＣＷ >= 料Ａ－開始和暦年月 ) AND
020660                ( 施術和暦年月ＣＷ <= 料Ａ－終了和暦年月 )
020670                 MOVE 料Ａ－初検料           TO 初検料ＡＷ             
020680                 MOVE 料Ａ－時間外           TO 時間外ＡＷ     
020690                 MOVE 料Ａ－休日             TO 休日ＡＷ       
020700                 MOVE 料Ａ－深夜             TO 深夜ＡＷ       
020710                 MOVE 料Ａ－往療基本料金     TO 往療基本料金ＡＷ       
020720                 MOVE 料Ａ－往療追加料金     TO 往療追加料金ＡＷ       
020730                 MOVE 料Ａ－往療加算率       TO 往療加算率ＡＷ         
020740                 MOVE 料Ａ－再検料           TO 再検料ＡＷ             
020750                 MOVE 料Ａ－冷罨法料         TO 冷罨法料ＡＷ           
020760                 MOVE 料Ａ－温罨法料         TO 温罨法料ＡＷ           
020770                 MOVE 料Ａ－電療料           TO 電療料ＡＷ             
020780                 MOVE 料Ａ－大               TO 大ＡＷ             
020790                 MOVE 料Ａ－中               TO 中ＡＷ             
020800                 MOVE 料Ａ－小               TO 小ＡＷ             
020810                 MOVE 料Ａ－施術情報提供料   TO 施術情報提供料ＡＷ     
020820                 MOVE 料Ａ－初検時相談料     TO 初検時相談料ＡＷ
                       MOVE 料Ａ－運動後療料       TO 運動療法料ＡＷ
                       MOVE 料Ａ－明細書発行加算料 TO 明細書発行加算料ＡＷ
020830             ELSE
020840*/エラー表示の修正
020850                 PERFORM コンソールエラー確認
020860                 DISPLAY "施術年月に対応した料金がみつかりません"
020870                         " 受診者№=" 連計－患者コード
020880                         " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
020890*                 DISPLAY NC"施術年月に対応した料金がみつかりません" UPON MSGBOX
020891*-----------------------------------------*
020892                 CALL "actcshm"  WITH C LINKAGE
020893*-----------------------------------------*
020900                 PERFORM 終了処理
020910                 MOVE ZERO TO PROGRAM-STATUS
020920                 EXIT PROGRAM
020930             END-IF
020940         END-READ
020950     END-IF.
020960*
020970*================================================================*
020980 負傷別料金取得 SECTION.
020990*
021000     MOVE 02               TO 料－区分コード.
021010     MOVE 負傷種別Ｗ       TO 料－負傷種別.
021020     MOVE 部位Ｗ           TO 料－部位.
021030     MOVE 左右区分Ｗ       TO 料－左右区分.
021040     MOVE 負傷位置番号Ｗ   TO 料－負傷位置番号.
021050     MOVE 連計－施術和暦   TO 料－開始和暦 施術和暦ＣＷ.
021060     MOVE 連計－施術年     TO 料－開始年   施術年ＣＷ.
021070     MOVE 連計－施術月     TO 料－開始月   施術月ＣＷ.
021080*
021090     START 料金マスタ KEY IS <= 料－区分コード 料－部位コード 料－開始和暦年月 REVERSED
021100     END-START.
021110*
021120     IF 状態キー = "00"
021130         READ 料金マスタ NEXT
021140         AT END
021150*/エラー表示の修正
021160             PERFORM コンソールエラー確認
021170             DISPLAY "施術年月に対応した料金がみつかりません"
021180                     " 受診者№=" 連計－患者コード
021190                     " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
021191*-----------------------------------------*
021192             CALL "actcshm"  WITH C LINKAGE
021193*-----------------------------------------*
021200             PERFORM 終了処理
021210             MOVE ZERO TO PROGRAM-STATUS
021220             EXIT PROGRAM
021230         NOT AT END
021240*/レコードのチェックを追加0107
021250             IF ( 料－区分コード    NOT = 02             ) OR
021260                ( 料－負傷種別      NOT = 負傷種別Ｗ     ) OR
021270                ( 料－部位          NOT = 部位Ｗ         ) OR
021280                ( 料－左右区分      NOT = 左右区分Ｗ     ) OR
021290                ( 料－負傷位置番号  NOT = 負傷位置番号Ｗ )   
021300                  PERFORM コンソールエラー確認
021310                  DISPLAY "料金がみつかりません１"
021320                          " 受診者№="   連計－患者コード
021330                          " 施術年月="   連計－施術年 連計－施術月
021340                          " 部位コード=" 部位コードＷ UPON CONS
021341*-----------------------------------------*
021342                  CALL "actcshm"  WITH C LINKAGE
021343*-----------------------------------------*
021350             ELSE
021360                 IF ( 施術和暦年月ＣＷ >= 料Ｂ－開始和暦年月 ) AND
021370                    ( 施術和暦年月ＣＷ <= 料Ｂ－終了和暦年月 )
021380                     MOVE 料Ｂ－拘縮後療料 TO 拘縮後療料ＢＷ
021390                     MOVE 料Ｂ－後療料     TO 後療料ＢＷ    
021400                     MOVE 料Ｂ－整復料     TO 整復料ＢＷ    
021410*    
021420                 ELSE
021430*    /エラー表示の修正
021440                     PERFORM コンソールエラー確認
021450                     DISPLAY "施術年月に対応した料金がみつかりません"
021460                             " 受診者№=" 連計－患者コード
021470                             " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
021471*-----------------------------------------*
021472                     CALL "actcshm"  WITH C LINKAGE
021473*-----------------------------------------*
021480                     PERFORM 終了処理
021490                     MOVE ZERO TO PROGRAM-STATUS
021500                     EXIT PROGRAM
021510                 END-IF
021520             END-IF
021530         END-READ
021540     END-IF.
021550*
021560*================================================================*
021570 捻挫等後療料取得 SECTION.
021580*
021590     MOVE 02               TO 料－区分コード.
021600     MOVE 負傷種別Ｗ       TO 料－負傷種別.
021610     MOVE ZERO             TO 料－部位.
021620     MOVE ZERO             TO 料－左右区分.
021630     MOVE ZERO             TO 料－負傷位置番号.
021640     MOVE 連計－施術和暦   TO 料－開始和暦 施術和暦ＣＷ.
021650     MOVE 連計－施術年     TO 料－開始年   施術年ＣＷ.
021660     MOVE 連計－施術月     TO 料－開始月   施術月ＣＷ.
021670*
021680     START 料金マスタ KEY IS <= 料－区分コード 料－部位コード 料－開始和暦年月 REVERSED
021690     END-START.
021700*
021710     IF 状態キー = "00"
021720         READ 料金マスタ NEXT
021730         AT END
021740*/エラー表示の修正
021750             PERFORM コンソールエラー確認
021760             DISPLAY "施術年月に対応した料金がみつかりません"
021770                     " 受診者№=" 連計－患者コード
021780                     " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
021781*-----------------------------------------*
021782             CALL "actcshm"  WITH C LINKAGE
021783*-----------------------------------------*
021790             PERFORM 終了処理
021800             MOVE ZERO TO PROGRAM-STATUS
021810             EXIT PROGRAM
021820         NOT AT END
021830             IF ( 施術和暦年月ＣＷ >= 料Ｂ－開始和暦年月 ) AND
021840                ( 施術和暦年月ＣＷ <= 料Ｂ－終了和暦年月 )
021850                 MOVE 料Ｂ－後療料     TO 後療料ＢＷ    
021860                 MOVE 料Ｂ－整復料     TO 整復料ＢＷ    
021870             ELSE
021880*/エラー表示の修正
021890                 PERFORM コンソールエラー確認
021900                 DISPLAY "施術年月に対応した料金がみつかりません"
021910                         " 受診者№=" 連計－患者コード
021920                         " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
021921*-----------------------------------------*
021922                 CALL "actcshm"  WITH C LINKAGE
021923*-----------------------------------------*
021930                 PERFORM 終了処理
021940                 MOVE ZERO TO PROGRAM-STATUS
021950                 EXIT PROGRAM
021960             END-IF
021970         END-READ
021980     END-IF.
021990*
022000*================================================================*
022010 捻挫等整復料取得 SECTION.
022020*
022030     MOVE 02               TO 料－区分コード.
022040     MOVE 負傷種別Ｗ       TO 料－負傷種別.
022050     MOVE ZERO             TO 料－部位.
022060     MOVE ZERO             TO 料－左右区分.
022070     MOVE ZERO             TO 料－負傷位置番号.
022080     MOVE 連計－施術和暦   TO 料－開始和暦 施術和暦ＣＷ.
022090     MOVE 連計－施術年     TO 料－開始年   施術年ＣＷ.
022100     MOVE 連計－施術月     TO 料－開始月   施術月ＣＷ.
022110*
022120     START 料金マスタ KEY IS <= 料－区分コード 料－部位コード 料－開始和暦年月 REVERSED
022130     END-START.
022140*
022150     IF 状態キー = "00"
022160         READ 料金マスタ NEXT
022170         AT END
022180*/エラー表示の修正
022190             PERFORM コンソールエラー確認
022200             DISPLAY "施術年月に対応した料金がみつかりません"
022210                     " 受診者№=" 連計－患者コード
022220                     " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
022221*-----------------------------------------*
022222             CALL "actcshm"  WITH C LINKAGE
022223*-----------------------------------------*
022230             PERFORM 終了処理
022240             MOVE ZERO TO PROGRAM-STATUS
022250             EXIT PROGRAM
022260         NOT AT END
022270             IF ( 施術和暦年月ＣＷ >= 料Ｂ－開始和暦年月 ) AND
022280                ( 施術和暦年月ＣＷ <= 料Ｂ－終了和暦年月 )
022290                 MOVE 料Ｂ－整復料     TO 整復料ＢＷ    
022300             ELSE
022310*/エラー表示の修正
022320                 PERFORM コンソールエラー確認
022330                 DISPLAY "施術年月に対応した料金がみつかりません"
022340                         " 受診者№=" 連計－患者コード
022350                         " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
022351*-----------------------------------------*
022352                 CALL "actcshm"  WITH C LINKAGE
022353*-----------------------------------------*
022360                 PERFORM 終了処理
022370                 MOVE ZERO TO PROGRAM-STATUS
022380                 EXIT PROGRAM
022390             END-IF
022400         END-READ
022410     END-IF.
022420*
022430*================================================================*
022440 計算情報取得 SECTION.
022450*
022460* 制御区分：０１　健康保険
022470     MOVE 01             TO 計－制御区分.
022480     MOVE 連計－施術和暦 TO 計－開始和暦 施術和暦ＣＷ.
022490     MOVE 連計－施術年   TO 計－開始年   施術年ＣＷ.
022500     MOVE 連計－施術月   TO 計－開始月   施術月ＣＷ.
022510*
022520     START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月 REVERSED
022530     END-START.
022540*
022550     IF 状態キー = "00"
022560         READ 計算マスタ NEXT
022570         AT END
022580*/エラー表示の修正
022590             PERFORM コンソールエラー確認
022600             DISPLAY "施術年月に対応した料金がみつかりません"
022610                     " 受診者№=" 連計－患者コード
022620                     " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
022621*-----------------------------------------*
022622             CALL "actcshm"  WITH C LINKAGE
022623*-----------------------------------------*
022630             PERFORM 終了処理
022640             MOVE ZERO TO PROGRAM-STATUS
022650             EXIT PROGRAM
022660         NOT AT END
022670             IF ( 施術和暦年月ＣＷ >= 計Ａ－開始和暦年月 ) AND
022680                ( 施術和暦年月ＣＷ <= 計Ａ－終了和暦年月 )
022690                 MOVE 計Ａ－往療基本距離     TO 往療基本距離ＫＷ
022700                 MOVE 計Ａ－後療可能算定数   TO 後療可能算定数ＫＷ
022710                 PERFORM VARYING 多部位ＣＮＴ FROM 1 BY 1
022720                         UNTIL 多部位ＣＮＴ > 後療可能算定数ＫＷ
022730                     MOVE 計Ａ－多部位逓減率(多部位ＣＮＴ) TO 多部位逓減率ＫＷ(多部位ＣＮＴ)
022740                 END-PERFORM
022750                 MOVE 計Ａ－長期逓減率       TO 長期逓減率ＫＷ
022760                 COMPUTE 長期逓減率中間ＫＷ   = 長期逓減率ＫＷ / 100
022770                 MOVE 計Ａ－長期逓減月境界日 TO 長期逓減月境界日ＫＷ
022780                 MOVE 計Ａ－長期逓減月数     TO 長期逓減月数ＫＷ
022790                 MOVE 計Ａ－老人負担回数     TO 老人負担回数ＫＷ
                       MOVE 計Ａ－長期頻回逓減率   TO 長期頻回逓減率ＫＷ
022800             ELSE
022810*/エラー表示の修正
022820                 PERFORM コンソールエラー確認
022830                 DISPLAY "施術年月に対応した料金がみつかりません"
022840                         " 受診者№=" 連計－患者コード
022850                         " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
022851*-----------------------------------------*
022852                 CALL "actcshm"  WITH C LINKAGE
022853*-----------------------------------------*
022860                 PERFORM 終了処理
022870                 MOVE ZERO TO PROGRAM-STATUS
022880                 EXIT PROGRAM
022890             END-IF
022900         END-READ
022910     END-IF.
022920*
022930     PERFORM 初検請求区分取得.
022940*
022950*================================================================*
022960 後療料算定 SECTION.
022970****************************************************************
022980* 逓減が関係する：後療料・冷罨法料・温罨法料を含む
022990* 継続以外（治癒・中止・転医等）で算定の繰上り有り（５部位まで）
023000* 治療中の部位の治癒等のケース。
023010* 後療料・罨法料・電療料を含む
023020****************************************************************
023030*
023040* 継続部位数Ｗのセット
023050     PERFORM 継続部位数セット.
023060*
023070     MOVE ZERO TO 後療料Ｗ 冷罨法料Ｗ 温罨法料Ｗ 電療料Ｗ.
023080
023090*/日毎項目の初期化
023100     MOVE ZERO TO 後療料１ＮＲＷ   冷罨法料１ＮＲＷ   温罨法料１ＮＲＷ   電療料１ＮＲＷ
023110                  後療料２ＮＲＷ   冷罨法料２ＮＲＷ   温罨法料２ＮＲＷ   電療料２ＮＲＷ
023120                  後療料３ＮＲＷ   冷罨法料３ＮＲＷ   温罨法料３ＮＲＷ   電療料３ＮＲＷ
023130                  後療料３１ＮＲＷ 冷罨法料３１ＮＲＷ 温罨法料３１ＮＲＷ 電療料３１ＮＲＷ
023140                  後療料４ＮＲＷ   冷罨法料４ＮＲＷ   温罨法料４ＮＲＷ   電療料４ＮＲＷ
023150                  後療料４１ＮＲＷ 冷罨法料４１ＮＲＷ 温罨法料４１ＮＲＷ 電療料４１ＮＲＷ
023160                  後療料４２ＮＲＷ 冷罨法料４２ＮＲＷ 温罨法料４２ＮＲＷ 電療料４２ＮＲＷ
023170                  後療料５ＮＲＷ   冷罨法料５ＮＲＷ   温罨法料５ＮＲＷ   電療料５ＮＲＷ
023180                  後療料５１ＮＲＷ 冷罨法料５１ＮＲＷ 温罨法料５１ＮＲＷ 電療料５１ＮＲＷ
023190                  後療料５２ＮＲＷ 冷罨法料５２ＮＲＷ 温罨法料５２ＮＲＷ 電療料５２ＮＲＷ
023200                  後療料５３ＮＲＷ 冷罨法料５３ＮＲＷ 温罨法料５３ＮＲＷ 電療料５３ＮＲＷ
      */６部位～/120804
023190                  後療料６２ＮＲＷ 冷罨法料６２ＮＲＷ 温罨法料６２ＮＲＷ 電療料６２ＮＲＷ
023190                  後療料６３ＮＲＷ 冷罨法料６３ＮＲＷ 温罨法料６３ＮＲＷ 電療料６３ＮＲＷ
023200                  後療料７２ＮＲＷ 冷罨法料７２ＮＲＷ 温罨法料７２ＮＲＷ 電療料７２ＮＲＷ
023200                  後療料７３ＮＲＷ 冷罨法料７３ＮＲＷ 温罨法料７３ＮＲＷ 電療料７３ＮＲＷ.
023210*
023220*     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
023230*             UNTIL   部位ＣＮＴ > 最大登録部位数Ｗ
023240*         PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1
023250*                 UNTIL   部位ＣＮＴ２ > 最大登録部位数Ｗ
023260*             MOVE ZERO TO 長期区分Ｗ(部位ＣＮＴ, 部位ＣＮＴ２)
023270*         END-PERFORM
023280*     END-PERFORM.
023290*
023300* 継続の部位のみ算定する
023310* 継続の部位のみ部位カウンタをカウントＵＰする
023320     MOVE 1 TO 部位カウンタ
023330*
023340* 部位カウンタは、継続部位数との比較のためと
023350* 逓減率の配列位置取得のための変数
023360     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
023370             UNTIL ( 部位カウンタ > 継続部位数Ｗ   ) OR
      */６７部位対応コメント/120803
023380*                   ( 部位カウンタ > 後療可能算定数ＫＷ ) OR 
023390                   ( 部位カウンタ２ > 部位数Ｗ )
023400*
023410* 負－終了和暦年月が未入力の場合は、ALL 9 をセットする
023420         PERFORM 開始終了日取得
023430*         PERFORM 長期逓減処理
023440*
023450         IF ( 開始和暦年月日Ｗ <= 施記－施術和暦年月日 ) AND
023460            ( 終了和暦年月日Ｗ >= 施記－施術和暦年月日 )
023470*            ( 転帰区分Ｗ = 9 )
023480*
023490             MOVE 施記－負傷種別(部位カウンタ２)     TO 負傷種別Ｗ
023500             MOVE 施記－部位(部位カウンタ２)         TO 部位Ｗ
023510             MOVE 施記－左右区分(部位カウンタ２)     TO 左右区分Ｗ
023520             MOVE 施記－負傷位置番号(部位カウンタ２) TO 負傷位置番号Ｗ
023530             IF 負傷種別Ｗ = 01 OR 02 OR 03
023540                 PERFORM 捻挫等後療料取得
023550             ELSE
023560                 PERFORM 負傷別料金取得
023570             END-IF
023580* 後療単価の退避
023590             IF 負傷種別Ｗ = 07 OR 08
023600*                 MOVE 拘縮後療料ＢＷ TO 後療単価ＯＷ(部位カウンタ, 部位カウンタ２)
023610                 MOVE 拘縮後療料ＢＷ TO 後療単価ＯＷ(部位カウンタ２)
023620             ELSE
023630*                 MOVE 後療料ＢＷ     TO 後療単価ＯＷ(部位カウンタ, 部位カウンタ２)
023640                 MOVE 後療料ＢＷ     TO 後療単価ＯＷ(部位カウンタ２)
023650             END-IF
023660* 後療の時のみ算定
023670             IF ( 施記－後療料請求区分(部位カウンタ２)   = 1 ) AND 
023680                ( 施記－整復施療区分(部位カウンタ２) NOT = 1 )
023690                 IF ( 施記－医師拘縮区分(部位カウンタ２) = 1 ) AND
023700                    ( 負傷種別Ｗ = 07 OR 08                  )
023710* レセプトの多部位逓減項目の出力の為、１００％の金額を部位毎合計する。
023720                     MOVE 拘縮後療料ＢＷ TO 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
023730                     PERFORM 後療多部位項目取得
023740                     PERFORM 後療多部位累計
023750                 ELSE
023760                     MOVE 後療料ＢＷ TO 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
023770                     PERFORM 後療多部位項目取得
023780                     PERFORM 後療多部位累計
023790                 END-IF
023800             ELSE IF ( 施記－後療料請求区分(部位カウンタ２)   = 1 ) AND 
023810                     ( 施記－整復施療区分(部位カウンタ２)     = 1 )
023820*/エラー表示の修正
023830                      PERFORM コンソールエラー確認
023840                      DISPLAY "整復料・後療料は同時には請求できません"
023850                              " 受診者№=" 連計－患者コード
023860                              " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
023870*                      DISPLAY NC"整復料・後療料は同時に請求できません" 施記－患者コード UPON MSGBOX
023871*-----------------------------------------*
023872                      CALL "actcshm"  WITH C LINKAGE
023873*-----------------------------------------*
023880                  END-IF
023890             END-IF
023900*
023910*
023920             EVALUATE 施記－罨法区分(部位カウンタ２)
023930             WHEN 1
023940                 MOVE 冷罨法料ＡＷ TO 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
023950                 PERFORM 冷罨法多部位項目取得
023960                 PERFORM 冷罨法多部位累計
023970*
023980             WHEN 2
023990                 MOVE 温罨法料ＡＷ TO 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
024000                 PERFORM 温罨法多部位項目取得
024010                 PERFORM 温罨法多部位累計
024020             WHEN OTHER
024030                 CONTINUE
024040             END-EVALUATE
024050*
024060*
024070             IF 施記－電療区分(部位カウンタ２) = 1
024080                 MOVE 電療料ＡＷ TO 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
024090                 PERFORM 電療多部位項目取得
024100                 PERFORM 電療多部位累計
024110             END-IF
024120*
024130*
024140             COMPUTE 部位カウンタ = 部位カウンタ + 1
024150*
024160         END-IF
024170     END-PERFORM.
024180*
024190*================================================================*
024200 年月絶対値計算 SECTION.
024210*
024220* 1989年1月を基準とし、当年までの絶対値を計算
024230     MOVE SPACE TO 長期フラグ.
024240*
024250* 和暦から西暦を換算
024260     MOVE 開始和暦Ｗ TO 元－元号区分.
024270     READ 元号マスタ
024280     INVALID KEY
024290         MOVE NC"元号" TO ファイル名Ｗ
024300     NOT INVALID KEY
024310         COMPUTE 西暦年Ｗ   = 元－開始西暦年 + 開始年Ｗ
024320         COMPUTE 西暦年差Ｗ = 西暦年Ｗ - 1989
024330     END-READ.
024340*
024350* 当月か翌月かの判定
024360     IF 開始日Ｗ > 長期逓減月境界日ＫＷ
024370         COMPUTE 開始年月絶対値Ｗ = (西暦年差Ｗ * 12) + ( 開始月Ｗ + 1 )
024380     ELSE
024390         COMPUTE 開始年月絶対値Ｗ = (西暦年差Ｗ * 12) + 開始月Ｗ
024400     END-IF.
024410*
024420     MOVE 連計－施術和暦 TO 元－元号区分.
024430     READ 元号マスタ
024440     INVALID KEY
024450         MOVE NC"元号" TO ファイル名Ｗ
024460     NOT INVALID KEY
024470         COMPUTE 西暦年Ｗ   = 元－開始西暦年 + 連計－施術年
024480         COMPUTE 西暦年差Ｗ = 西暦年Ｗ - 1989
024490     END-READ.
024500* 施術日は、境界日の判定の必要なし？
024510* 当月か翌月かの判定
024520*     IF 施記－施術日 > 長期逓減月境界日ＫＷ
024530*         COMPUTE 施術年月絶対値Ｗ = (西暦年差Ｗ * 12) + ( 連計－施術月 + 1 )
024540*     ELSE
024550         COMPUTE 施術年月絶対値Ｗ = (西暦年差Ｗ * 12) + 連計－施術月
024560*     END-IF.
024570*
024580     COMPUTE 施術年月差Ｗ = 施術年月絶対値Ｗ - 開始年月絶対値Ｗ.
024590*
024600     IF 施術年月差Ｗ >= 長期逓減月数ＫＷ
024610         MOVE "YES" TO 長期フラグ
024620     END-IF.
024630*

      **/令和6年10月改定対応↓↓↓*/20240708
      **/初回月は１を記載
      *     COMPUTE 継続月数Ｗ(部位カウンタ２) = 施術年月差Ｗ + 1.
HILO  *     DISPLAY "610-1 年月絶対値計算 " 部位カウンタ２ " " 施術年月差Ｗ " " レセ－部位実日数(部位カウンタ２) " " 部位実日数Ｗ(部位カウンタ２).

024640*================================================================*
024650 その他算定 SECTION.
024660****************************************************************
024670* 副子・初回処置・情報提供料
024680* 初回処置については、６部位目以降も算定あり。
024690****************************************************************
024700*
024710     MOVE ZERO TO 金属副子料Ｗ.
024720     MOVE ZERO TO 初回処置料Ｗ.
024730     MOVE ZERO TO 施術情報提供料Ｗ.
           MOVE SPACE TO 金属副子算定Ｆ.
024740
024750
024760*/日毎項目の初期化
024770     MOVE ZERO TO 金属副子大ＮＷ 金属副子中ＮＷ 金属副子小ＮＷ 初回処置ＮＯＷ.
024780
024790
024800*
024810* 継続の部位のみ算定する
024820* 継続の部位のみ部位カウンタをカウントＵＰする
024830* 初回処置のみ後療可能算定数以上に算定可能
024840     MOVE 1 TO 部位カウンタ
024850     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
      */金属副子を算定後に開始日終了日を変えて算定出来なくなった場合にクリアする為に
      */継続部位数を超えても処理を止めないように変更する↓↓↓/20180710
024860             UNTIL ( 部位カウンタ２ > 部位数Ｗ )
024860*             UNTIL ( 部位カウンタ２ > 部位数Ｗ ) OR
024870*                   ( 部位カウンタ > 継続部位数Ｗ )
      */↑↑↑/20180710
024880*
024890* 負－終了和暦年月が未入力の場合は、ALL 9 をセットする
024900         PERFORM 開始終了日取得
024910*
HILO  *     DISPLAY "その他算定-1 " 施記－施術和暦年月日 " 部位" 部位カウンタ２ " "  開始和暦年月日Ｗ " "  終了和暦年月日Ｗ
024920         IF ( 開始和暦年月日Ｗ <= 施記－施術和暦年月日 ) AND
024930            ( 終了和暦年月日Ｗ >= 施記－施術和暦年月日 )
024940*            ( 転帰区分Ｗ = 9 )
024950*
024960             MOVE 施記－負傷種別(部位カウンタ２)     TO 負傷種別Ｗ
024970             MOVE 施記－部位(部位カウンタ２)         TO 部位Ｗ
024980             MOVE 施記－左右区分(部位カウンタ２)     TO 左右区分Ｗ
024990             MOVE 施記－負傷位置番号(部位カウンタ２) TO 負傷位置番号Ｗ
025000             IF 負傷種別Ｗ = 01 OR 02 OR 03
025010                 PERFORM 捻挫等整復料取得
025020             ELSE
025030                 PERFORM 負傷別料金取得
025040             END-IF
025050*
025060* 初回処置料
025070*
025080             IF 施記－整復施療区分(部位カウンタ２) = 1
025090                 COMPUTE 初回処置料Ｗ = 初回処置料Ｗ + 整復料ＢＷ
025100                 COMPUTE 初回処置料ＯＷ(部位カウンタ２) = 初回処置料ＯＷ(部位カウンタ２) + 整復料ＢＷ
025110********
025120                 MOVE 整復料ＢＷ TO 初回処置料ＮＯＷ(部位カウンタ２)
025130********
025140
025150
025160
025170             END-IF
025180*
      */金属副子情報提供は全部位算定可/180427
025190*             IF 部位カウンタ <= 後療可能算定数ＫＷ
      */平成30年6月1日より仕様変更/20180427
                   IF 連計－施術和暦年月 < 43006
025200* 金属副子（脱臼・骨折・不全骨折）
025210                 EVALUATE 施記－金属副子区分(部位カウンタ２)
025220                 WHEN 1
025230*/仕様変更。金属副子は同じ種類を複数取れる。IF文をコメントに。0307
025240*                     IF 金属副子大Ｗ = ZERO
025250                         IF ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
025260                            ( 連計－施術和暦年月日 = 開始和暦年月日Ｗ   )
025270                             MOVE 1 TO 金属副子大Ｗ 金属副子大ＮＷ
025280                             COMPUTE 金属副子料Ｗ = 金属副子料Ｗ + 大ＡＷ
025290                         END-IF
025300*                     END-IF
025310                 WHEN 2
025320*/仕様変更。金属副子は同じ種類を複数取れる。IF文をコメントに。0307
025330*                     IF 金属副子中Ｗ = ZERO
025340                         IF ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
025350                            ( 連計－施術和暦年月日 = 開始和暦年月日Ｗ   )
025360                             MOVE 1 TO 金属副子中Ｗ 金属副子中ＮＷ
025370                             COMPUTE 金属副子料Ｗ = 金属副子料Ｗ + 中ＡＷ
025380                         END-IF
025390*                     END-IF
025400                 WHEN 3
025410*/仕様変更。金属副子は同じ種類を複数取れる。IF文をコメントに。0307
025420*                     IF 金属副子小Ｗ = ZERO
025430                         IF ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
025440                            ( 連計－施術和暦年月日 = 開始和暦年月日Ｗ   )
025450                             MOVE 1 TO 金属副子小Ｗ 金属副子小ＮＷ
025460                             COMPUTE 金属副子料Ｗ = 金属副子料Ｗ + 小ＡＷ
025470                         END-IF
025480*                     END-IF
025490                 WHEN OTHER
025500                     CONTINUE
025510                 END-EVALUATE
                   ELSE
      *金属副子（脱臼・骨折・不全骨折）
HILO  *     DISPLAY "その他算定 " 施記－施術和暦年月日 " 部位" 部位カウンタ２ " 金属区分" 施記－金属副子区分(部位カウンタ２) " 負傷種別" 施記－負傷種別(部位カウンタ２)
                       MOVE SPACE TO 金属副子算定Ｆ
                       EVALUATE 施記－金属副子区分(部位カウンタ２)
                       WHEN 1
                           IF ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 )
                               PERFORM 金属副子回数判定
                               IF 金属副子ＣＮＴ < 3
                                   COMPUTE 金属副子大Ｗ   = 金属副子大Ｗ   + 1
                                   COMPUTE 金属副子大ＮＷ = 金属副子大ＮＷ + 1
                                   COMPUTE 金属副子料Ｗ   = 金属副子料Ｗ   + 大ＡＷ
                                   COMPUTE 金属副子回数Ｗ = 金属副子回数Ｗ + 1
                                   MOVE "YES" TO 金属副子算定Ｆ
                               END-IF
                           END-IF
                       WHEN 2
                           IF ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 )
                               PERFORM 金属副子回数判定
                               IF 金属副子ＣＮＴ < 3
                                   COMPUTE 金属副子中Ｗ   = 金属副子中Ｗ   + 1
                                   COMPUTE 金属副子中ＮＷ = 金属副子中ＮＷ + 1
                                   COMPUTE 金属副子料Ｗ   = 金属副子料Ｗ   + 中ＡＷ
                                   COMPUTE 金属副子回数Ｗ = 金属副子回数Ｗ + 1
                                   MOVE "YES" TO 金属副子算定Ｆ
                               END-IF
                           END-IF
                       WHEN 3
                           IF ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 )
                               PERFORM 金属副子回数判定
                               IF 金属副子ＣＮＴ < 3
                                   COMPUTE 金属副子小Ｗ   = 金属副子小Ｗ   + 1
                                   COMPUTE 金属副子小ＮＷ = 金属副子小ＮＷ + 1
                                   COMPUTE 金属副子料Ｗ   = 金属副子料Ｗ   + 小ＡＷ
                                   COMPUTE 金属副子回数Ｗ = 金属副子回数Ｗ + 1
                                   MOVE "YES" TO 金属副子算定Ｆ
                               END-IF
                           END-IF
                       WHEN OTHER
                           CONTINUE
                       END-EVALUATE
      */金属副子算定出来ない場合にクリアする
                       IF (施記－金属副子区分(部位カウンタ２) = 1 OR 2 OR 3) AND
                          (金属副子算定Ｆ = SPACE)
                           PERFORM 金属副子区分クリア
                       END-IF
                   END-IF
025520* 施術情報提供料
025530* 骨折・不全骨折・脱臼の場合のみ
025540                 IF ( 施記－情報提供区分(部位カウンタ２) = 1 ) AND
025550                    ( 施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
025560                    ( 開始和暦年月日Ｗ = 施記－施術和暦年月日 )
025570                     COMPUTE 施術情報提供料Ｗ = 施術情報提供料Ｗ + 施術情報提供料ＡＷ
025580                 END-IF
      */金属副子情報提供は全部位算定可/180427
025590*             END-IF
025600*
025610             COMPUTE 部位カウンタ = 部位カウンタ + 1
025620*
      */金属副子算定後に開始終了日を変更しその日に算定不可になった場合はクリアする↓↓↓/20180628
               ELSE
      */平成30年6月1日より仕様変更/20180427
                   IF (連計－施術和暦年月 >= 43006) AND
                      (施記－金属副子区分(部位カウンタ２) = 1 OR 2 OR 3) AND
                      (金属副子算定Ｆ = SPACE)
HILO  *                 DISPLAY 施記－施術和暦年月日 " ("  部位カウンタ２ ") クリア１"
                       PERFORM 金属副子区分クリア
                   END-IF
      */金属副子算定後に開始終了日を変更しその日に算定不可になった場合はクリアする↑↑↑/20180628
025630         END-IF
025640     END-PERFORM.
      **/金属副子算定後に開始終了日を変更しその日に算定不可になった場合はクリアする↓↓↓/20180628
      **/平成30年6月1日より仕様変更/20180427
      *     IF (連計－施術和暦年月 >= 43006) AND
      *        (施記－金属副子区分(部位カウンタ２) = 1 OR 2 OR 3) AND
      *        (金属副子算定Ｆ = SPACE)
HILO  *         DISPLAY 施記－施術和暦年月日 " ("  部位カウンタ２ ") クリア２"
      *         PERFORM 金属副子区分クリア
      *     END-IF.
      **/金属副子算定後に開始終了日を変更しその日に算定不可になった場合はクリアする↑↑↑/20180628
025650*
025660*================================================================*
025670 開始終了日取得 SECTION.
025680*
025690     MOVE 連計－施術和暦 TO 負－施術和暦.
025700     MOVE 連計－施術年   TO 負－施術年.
025710     MOVE 連計－施術月   TO 負－施術月.
025720     MOVE 連計－患者番号 TO 負－患者番号.
025730     MOVE 連計－枝番     TO 負－枝番.
025740     READ 負傷データＦ
025750     INVALID KEY
025760*/エラー表示の修正
025770         PERFORM コンソールエラー確認
025780         DISPLAY "指定の受信者の負傷データがありません"
025790                 " 受診者№=" 連計－患者コード
025800                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
025810*-----------------------------------------*
025820         CALL "actcshm"  WITH C LINKAGE
025830*-----------------------------------------*
025840*         ACCEPT  キー入力 FROM CONS
025850         PERFORM ファイル閉鎖
025860         MOVE ZERO TO PROGRAM-STATUS
025870         EXIT PROGRAM
025880     NOT INVALID KEY
025890         MOVE 負－転帰区分(部位カウンタ２)       TO 転帰区分Ｗ
025900         MOVE 負－開始和暦年月日(部位カウンタ２) TO 開始和暦年月日Ｗ
025910*
025920         IF ( 負－終了和暦(部位カウンタ２) = ZERO ) OR
025930            ( 負－終了年(部位カウンタ２)   = ZERO ) OR
025940            ( 負－終了月(部位カウンタ２)   = ZERO ) OR
025950            ( 負－終了日(部位カウンタ２)   = ZERO )
025960             MOVE "9999999" TO 終了和暦年月日Ｗ
025970         ELSE
025980             MOVE 負－終了和暦年月日(部位カウンタ２) TO 終了和暦年月日Ｗ
025990         END-IF
026000     END-READ.
026010*
026020*================================================================*
026030 継続部位数セット SECTION.
026040*
026050     MOVE ZERO TO 継続部位数Ｗ.
026060*
026070     MOVE 連計－施術和暦 TO 負－施術和暦.
026080     MOVE 連計－施術年   TO 負－施術年.
026090     MOVE 連計－施術月   TO 負－施術月.
026100     MOVE 連計－患者番号 TO 負－患者番号.
026110     MOVE 連計－枝番     TO 負－枝番.
026120     READ 負傷データＦ
026130     INVALID KEY
026140*/エラー表示の修正
026150         PERFORM コンソールエラー確認
026160         DISPLAY "指定の受信者の負傷データがありません"
026170                 " 受診者№=" 連計－患者コード
026180                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
026190*-----------------------------------------*
026200         CALL "actcshm"  WITH C LINKAGE
026210*-----------------------------------------*
026220*         ACCEPT  キー入力 FROM CONS
026230         PERFORM ファイル閉鎖
026240         MOVE ZERO TO PROGRAM-STATUS
026250         EXIT PROGRAM
026260     NOT INVALID KEY
026270         MOVE 負－部位数 TO 部位数Ｗ
026280         MOVE 連計－施術和暦 TO  施術和暦ＣＷ
026290         MOVE 連計－施術年   TO  施術年ＣＷ
026300         MOVE 連計－施術月   TO  施術月ＣＷ
026310         MOVE 施記－施術日   TO  施術日ＣＷ
026320         PERFORM VARYING 部位カウンタ FROM 1 BY 1
026330                 UNTIL 部位カウンタ > 部位数Ｗ
026340*
026350             IF ( 負－終了和暦(部位カウンタ) = ZERO ) OR
026360                ( 負－終了年(部位カウンタ)   = ZERO ) OR
026370                ( 負－終了月(部位カウンタ)   = ZERO ) OR
026380                ( 負－終了日(部位カウンタ)   = ZERO )
026390                 MOVE "9999999" TO 終了和暦年月日Ｗ
026400             ELSE
026410                 MOVE 負－終了和暦年月日(部位カウンタ) TO 終了和暦年月日Ｗ
026420             END-IF
026430*
026440             IF ( 負－開始和暦年月日(部位カウンタ) <= 施記－施術和暦年月日 ) AND
026450                ( 終了和暦年月日Ｗ                 >= 施記－施術和暦年月日 )
026460*                ( 負－転帰区分(部位カウンタ) = 9 )
026470                 COMPUTE 継続部位数Ｗ = 継続部位数Ｗ + 1
026480                 MOVE 部位カウンタ TO 部位逓減情報Ｗ(継続部位数Ｗ)
026490             END-IF
026500         END-PERFORM
026510     END-READ.
026520*
026530*================================================================*
026540 累計処理 SECTION.
026550*
026560     COMPUTE 初検料ＧＷ         = 初検料ＧＷ     + 初検料Ｗ.
026570     COMPUTE 初検加算料ＧＷ     = 初検加算料ＧＷ + 初検加算料Ｗ.
026580     COMPUTE 再検料ＧＷ         = 再検料ＧＷ     + 再検料Ｗ.
026590     COMPUTE 往療料ＧＷ         = 往療料ＧＷ     + 往療料Ｗ.
026600     COMPUTE 往療加算料ＧＷ     = 往療加算料ＧＷ + 往療加算料Ｗ.
026610     COMPUTE 初検時相談料ＧＷ   = 初検時相談料ＧＷ + 初検時相談料Ｗ.
026620*
026630*     COMPUTE 後療料ＧＷ         = 後療料ＧＷ     + 後療料Ｗ.
026640*     COMPUTE 冷罨法料ＧＷ       = 冷罨法料ＧＷ   + 冷罨法料Ｗ.
026650*     COMPUTE 温罨法料ＧＷ       = 温罨法料ＧＷ   + 温罨法料Ｗ.
026660*     COMPUTE 電療料ＧＷ         = 電療料ＧＷ     + 電療料Ｗ.
026670*
026680     COMPUTE 金属副子料ＧＷ     = 金属副子料ＧＷ + 金属副子料Ｗ.
026690     COMPUTE 初回処置料ＧＷ     = 初回処置料ＧＷ + 初回処置料Ｗ.
026700     COMPUTE 施術情報提供料ＧＷ = 施術情報提供料ＧＷ + 施術情報提供料Ｗ.
           COMPUTE 運動療法料ＧＷ     = 運動療法料ＧＷ + 運動療法料Ｗ.
HILO  *     DISPLAY 施記－施術和暦年月日 " 運動療法料Ｗ " 運動療法料Ｗ.
026610     COMPUTE 明細書発行加算料ＧＷ = 明細書発行加算料ＧＷ + 明細書発行加算料Ｗ.
026710*
026720*================================================================*
026730 後療多部位累計 SECTION.
026740*
026750* １００％の金額を部位毎累計する。
026760* 逓減率は、部位毎の合計金額に対し、乗じる。
026770*
026780     EVALUATE 部位カウンタ ALSO 部位カウンタ２
026790     WHEN 1 ALSO 1
026800         COMPUTE 後療回数１ＲＷ = 後療回数１ＲＷ + 1
026810         COMPUTE 後療料１ＲＷ   = 後療料１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
026820         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料１ＮＲＷ
026830*
026840     WHEN 1 ALSO 2
026850     WHEN 2 ALSO 2
026860         COMPUTE 後療回数２ＲＷ = 後療回数２ＲＷ + 1
026870         COMPUTE 後療料２ＲＷ   = 後療料２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
026880         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料２ＮＲＷ
026890*
026900     WHEN 1 ALSO 3
026910     WHEN 2 ALSO 3
026920*         IF 後療回数３１ＲＷ = ZERO
026930*             MOVE 施記－施術和暦 TO 逓減開始和暦３１ＲＷ
026940*             MOVE 施記－施術年   TO 逓減開始年３１ＲＷ
026950*             MOVE 施記－施術月   TO 逓減開始月３１ＲＷ
026960*             MOVE 施記－施術日   TO 逓減開始日３１ＲＷ
026970*         END-IF
026980         COMPUTE 後療回数３１ＲＷ = 後療回数３１ＲＷ + 1
026990         COMPUTE 後療料３１ＲＷ   = 後療料３１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027000         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料３１ＮＲＷ
027010
027020     WHEN 3 ALSO 3
027030         COMPUTE 後療回数３ＲＷ = 後療回数３ＲＷ + 1
027040         COMPUTE 後療料３ＲＷ = 後療料３ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027050
027060         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料３ＮＲＷ
027070*
027080     WHEN 1 ALSO 4
027090     WHEN 2 ALSO 4
027100*         IF 後療回数４２ＲＷ = ZERO
027110*             MOVE 施記－施術和暦 TO 逓減開始和暦４２ＲＷ
027120*             MOVE 施記－施術年   TO 逓減開始年４２ＲＷ
027130*             MOVE 施記－施術月   TO 逓減開始月４２ＲＷ
027140*             MOVE 施記－施術日   TO 逓減開始日４２ＲＷ
027150*         END-IF
027160         COMPUTE 後療回数４２ＲＷ = 後療回数４２ＲＷ + 1
027170         COMPUTE 後療料４２ＲＷ   = 後療料４２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027180         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４２ＮＲＷ
027190     WHEN 3 ALSO 4
027200*         IF 後療回数４１ＲＷ = ZERO
027210*             MOVE 施記－施術和暦 TO 逓減開始和暦４１ＲＷ
027220*             MOVE 施記－施術年   TO 逓減開始年４１ＲＷ
027230*             MOVE 施記－施術月   TO 逓減開始月４１ＲＷ
027240*             MOVE 施記－施術日   TO 逓減開始日４１ＲＷ
027250*         END-IF
027260         COMPUTE 後療回数４１ＲＷ = 後療回数４１ＲＷ + 1
027270         COMPUTE 後療料４１ＲＷ   = 後療料４１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027280         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４１ＮＲＷ
027290     WHEN 4 ALSO 4
027300         COMPUTE 後療回数４ＲＷ = 後療回数４ＲＷ + 1
027310         COMPUTE 後療料４ＲＷ   = 後療料４ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027320         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４ＮＲＷ
027330*
027340     WHEN 1 ALSO 5
027350     WHEN 2 ALSO 5
027360*         IF 後療回数５３ＲＷ = ZERO
027370*             MOVE 施記－施術和暦 TO 逓減開始和暦５３ＲＷ
027380*             MOVE 施記－施術年   TO 逓減開始年５３ＲＷ
027390*             MOVE 施記－施術月   TO 逓減開始月５３ＲＷ
027400*             MOVE 施記－施術日   TO 逓減開始日５３ＲＷ
027410*         END-IF
027420         COMPUTE 後療回数５３ＲＷ = 後療回数５３ＲＷ + 1
027430         COMPUTE 後療料５３ＲＷ   = 後療料５３ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027440         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５３ＮＲＷ
027450     WHEN 3 ALSO 5
027460*         IF 後療回数５２ＲＷ = ZERO
027470*             MOVE 施記－施術和暦 TO 逓減開始和暦５２ＲＷ
027480*             MOVE 施記－施術年   TO 逓減開始年５２ＲＷ
027490*             MOVE 施記－施術月   TO 逓減開始月５２ＲＷ
027500*             MOVE 施記－施術日   TO 逓減開始日５２ＲＷ
027510*         END-IF
027520         COMPUTE 後療回数５２ＲＷ = 後療回数５２ＲＷ + 1
027530         COMPUTE 後療料５２ＲＷ   = 後療料５２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027540         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５２ＮＲＷ
027550     WHEN 4 ALSO 5
027560*         IF 後療回数５１ＲＷ = ZERO
027570*             MOVE 施記－施術和暦 TO 逓減開始和暦５１ＲＷ
027580*             MOVE 施記－施術年   TO 逓減開始年５１ＲＷ
027590*             MOVE 施記－施術月   TO 逓減開始月５１ＲＷ
027600*             MOVE 施記－施術日   TO 逓減開始日５１ＲＷ
027610*         END-IF
027620         COMPUTE 後療回数５１ＲＷ = 後療回数５１ＲＷ + 1
027630         COMPUTE 後療料５１ＲＷ   = 後療料５１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027640         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５１ＮＲＷ
027650     WHEN 5 ALSO 5
027660         COMPUTE 後療回数５ＲＷ = 後療回数５ＲＷ + 1
027670         COMPUTE 後療料５ＲＷ = 後療料５ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027680         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５ＮＲＷ
      */６部位/120804
027340     WHEN 1 ALSO 6
027350     WHEN 2 ALSO 6
027420         COMPUTE 後療回数６３ＲＷ = 後療回数６３ＲＷ + 1
027430         COMPUTE 後療料６３ＲＷ   = 後療料６３ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027440         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料６３ＮＲＷ
027450     WHEN 3 ALSO 6
027520         COMPUTE 後療回数６２ＲＷ = 後療回数６２ＲＷ + 1
027530         COMPUTE 後療料６２ＲＷ   = 後療料６２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027540         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料６２ＮＲＷ
      */７部位/120804
027340     WHEN 1 ALSO 7
027350     WHEN 2 ALSO 7
027420         COMPUTE 後療回数７３ＲＷ = 後療回数７３ＲＷ + 1
027430         COMPUTE 後療料７３ＲＷ   = 後療料７３ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027440         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料７３ＮＲＷ
027450     WHEN 3 ALSO 7
027520         COMPUTE 後療回数７２ＲＷ = 後療回数７２ＲＷ + 1
027530         COMPUTE 後療料７２ＲＷ   = 後療料７２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027540         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料７２ＮＲＷ
027690     WHEN OTHER
027700         CONTINUE
027710     END-EVALUATE.
027720*
027730*================================================================*
027740 冷罨法多部位累計 SECTION.
027750*
027760     EVALUATE 部位カウンタ ALSO 部位カウンタ２
027770     WHEN 1 ALSO 1
027780         COMPUTE 冷罨法回数１ＲＷ = 冷罨法回数１ＲＷ + 1
027790         COMPUTE 冷罨法料１ＲＷ   = 冷罨法料１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027800         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料１ＮＲＷ
027810*
027820     WHEN 1 ALSO 2
027830     WHEN 2 ALSO 2
027840         COMPUTE 冷罨法回数２ＲＷ = 冷罨法回数２ＲＷ + 1
027850         COMPUTE 冷罨法料２ＲＷ   = 冷罨法料２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027860         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料２ＮＲＷ
027870*
027880     WHEN 1 ALSO 3
027890     WHEN 2 ALSO 3
027900         COMPUTE 冷罨法回数３１ＲＷ = 冷罨法回数３１ＲＷ + 1
027910         COMPUTE 冷罨法料３１ＲＷ   = 冷罨法料３１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027920         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料３１ＮＲＷ
027930     WHEN 3 ALSO 3
027940         COMPUTE 冷罨法回数３ＲＷ = 冷罨法回数３ＲＷ + 1
027950         COMPUTE 冷罨法料３ＲＷ = 冷罨法料３ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027960         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料３ＮＲＷ
027970*
027980     WHEN 1 ALSO 4
027990     WHEN 2 ALSO 4
028000         COMPUTE 冷罨法回数４２ＲＷ = 冷罨法回数４２ＲＷ + 1
028010         COMPUTE 冷罨法料４２ＲＷ   = 冷罨法料４２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028020         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４２ＮＲＷ
028030     WHEN 3 ALSO 4
028040         COMPUTE 冷罨法回数４１ＲＷ = 冷罨法回数４１ＲＷ + 1
028050         COMPUTE 冷罨法料４１ＲＷ   = 冷罨法料４１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028060         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４１ＮＲＷ
028070     WHEN 4 ALSO 4
028080         COMPUTE 冷罨法回数４ＲＷ = 冷罨法回数４ＲＷ + 1
028090         COMPUTE 冷罨法料４ＲＷ   = 冷罨法料４ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028100         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４ＮＲＷ
028110*
028120     WHEN 1 ALSO 5
028130     WHEN 2 ALSO 5
028140         COMPUTE 冷罨法回数５３ＲＷ = 冷罨法回数５３ＲＷ + 1
028150         COMPUTE 冷罨法料５３ＲＷ   = 冷罨法料５３ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028160         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５３ＮＲＷ
028170     WHEN 3 ALSO 5
028180         COMPUTE 冷罨法回数５２ＲＷ = 冷罨法回数５２ＲＷ + 1
028190         COMPUTE 冷罨法料５２ＲＷ   = 冷罨法料５２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028200         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５２ＮＲＷ
028210     WHEN 4 ALSO 5
028220         COMPUTE 冷罨法回数５１ＲＷ = 冷罨法回数５１ＲＷ + 1
028230         COMPUTE 冷罨法料５１ＲＷ   = 冷罨法料５１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028240         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５１ＮＲＷ
028250     WHEN 5 ALSO 5
028260         COMPUTE 冷罨法回数５ＲＷ = 冷罨法回数５ＲＷ + 1
028270         COMPUTE 冷罨法料５ＲＷ = 冷罨法料５ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028280         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５ＮＲＷ
      */６部位/120804
028120     WHEN 1 ALSO 6
028130     WHEN 2 ALSO 6
028140         COMPUTE 冷罨法回数６３ＲＷ = 冷罨法回数６３ＲＷ + 1
028150         COMPUTE 冷罨法料６３ＲＷ   = 冷罨法料６３ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028160         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料６３ＮＲＷ
028170     WHEN 3 ALSO 6
028180         COMPUTE 冷罨法回数６２ＲＷ = 冷罨法回数６２ＲＷ + 1
028190         COMPUTE 冷罨法料６２ＲＷ   = 冷罨法料６２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028200         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料６２ＮＲＷ
      */７部位/120804
028120     WHEN 1 ALSO 7
028130     WHEN 2 ALSO 7
028140         COMPUTE 冷罨法回数７３ＲＷ = 冷罨法回数７３ＲＷ + 1
028150         COMPUTE 冷罨法料７３ＲＷ   = 冷罨法料７３ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028160         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料７３ＮＲＷ
028170     WHEN 3 ALSO 7
028180         COMPUTE 冷罨法回数７２ＲＷ = 冷罨法回数７２ＲＷ + 1
028190         COMPUTE 冷罨法料７２ＲＷ   = 冷罨法料７２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028200         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料７２ＮＲＷ
028290     WHEN OTHER
028300         CONTINUE
028310     END-EVALUATE.
028320*
028330*================================================================*
028340 温罨法多部位累計 SECTION.
028350*
028360     EVALUATE 部位カウンタ ALSO 部位カウンタ２
028370     WHEN 1 ALSO 1
028380         COMPUTE 温罨法回数１ＲＷ = 温罨法回数１ＲＷ + 1
028390         COMPUTE 温罨法料１ＲＷ   = 温罨法料１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028400         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料１ＮＲＷ
028410*
028420     WHEN 1 ALSO 2
028430     WHEN 2 ALSO 2
028440         COMPUTE 温罨法回数２ＲＷ = 温罨法回数２ＲＷ + 1
028450         COMPUTE 温罨法料２ＲＷ   = 温罨法料２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028460         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料２ＮＲＷ
028470*
028480     WHEN 1 ALSO 3
028490     WHEN 2 ALSO 3
028500         COMPUTE 温罨法回数３１ＲＷ = 温罨法回数３１ＲＷ + 1
028510         COMPUTE 温罨法料３１ＲＷ   = 温罨法料３１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028520         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料３１ＮＲＷ
028530     WHEN 3 ALSO 3
028540         COMPUTE 温罨法回数３ＲＷ = 温罨法回数３ＲＷ + 1
028550         COMPUTE 温罨法料３ＲＷ = 温罨法料３ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028560         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料３ＮＲＷ
028570*
028580     WHEN 1 ALSO 4
028590     WHEN 2 ALSO 4
028600         COMPUTE 温罨法回数４２ＲＷ = 温罨法回数４２ＲＷ + 1
028610         COMPUTE 温罨法料４２ＲＷ   = 温罨法料４２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028620         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４２ＮＲＷ
028630     WHEN 3 ALSO 4
028640         COMPUTE 温罨法回数４１ＲＷ = 温罨法回数４１ＲＷ + 1
028650         COMPUTE 温罨法料４１ＲＷ   = 温罨法料４１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028660         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４１ＮＲＷ
028670     WHEN 4 ALSO 4
028680         COMPUTE 温罨法回数４ＲＷ = 温罨法回数４ＲＷ + 1
028690         COMPUTE 温罨法料４ＲＷ   = 温罨法料４ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028700         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４ＮＲＷ
028710*
028720     WHEN 1 ALSO 5
028730     WHEN 2 ALSO 5
028740         COMPUTE 温罨法回数５３ＲＷ = 温罨法回数５３ＲＷ + 1
028750         COMPUTE 温罨法料５３ＲＷ   = 温罨法料５３ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028760         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５３ＮＲＷ
028770     WHEN 3 ALSO 5
028780         COMPUTE 温罨法回数５２ＲＷ = 温罨法回数５２ＲＷ + 1
028790         COMPUTE 温罨法料５２ＲＷ   = 温罨法料５２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028800         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５２ＮＲＷ
028810     WHEN 4 ALSO 5
028820         COMPUTE 温罨法回数５１ＲＷ = 温罨法回数５１ＲＷ + 1
028830         COMPUTE 温罨法料５１ＲＷ   = 温罨法料５１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028840         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５１ＮＲＷ
028850     WHEN 5 ALSO 5
028860         COMPUTE 温罨法回数５ＲＷ = 温罨法回数５ＲＷ + 1
028870         COMPUTE 温罨法料５ＲＷ = 温罨法料５ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028880         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５ＮＲＷ
      */６部位/120804
028720     WHEN 1 ALSO 6
028730     WHEN 2 ALSO 6
028740         COMPUTE 温罨法回数６３ＲＷ = 温罨法回数６３ＲＷ + 1
028750         COMPUTE 温罨法料６３ＲＷ   = 温罨法料６３ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028760         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料６３ＮＲＷ
028770     WHEN 3 ALSO 6
028780         COMPUTE 温罨法回数６２ＲＷ = 温罨法回数６２ＲＷ + 1
028790         COMPUTE 温罨法料６２ＲＷ   = 温罨法料６２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028800         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料６２ＮＲＷ
      */７部位/120804
028720     WHEN 1 ALSO 7
028730     WHEN 2 ALSO 7
028740         COMPUTE 温罨法回数７３ＲＷ = 温罨法回数７３ＲＷ + 1
028750         COMPUTE 温罨法料７３ＲＷ   = 温罨法料７３ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028760         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料７３ＮＲＷ
028770     WHEN 3 ALSO 7
028780         COMPUTE 温罨法回数７２ＲＷ = 温罨法回数７２ＲＷ + 1
028790         COMPUTE 温罨法料７２ＲＷ   = 温罨法料７２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
028800         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料７２ＮＲＷ
028890     WHEN OTHER
028900         CONTINUE
028910     END-EVALUATE.
028920*
028930*================================================================*
028940 電療多部位累計 SECTION.
028950*
028960     EVALUATE 部位カウンタ ALSO 部位カウンタ２
028970     WHEN 1 ALSO 1
028980         COMPUTE 電療回数１ＲＷ = 電療回数１ＲＷ + 1
028990         COMPUTE 電療料１ＲＷ   = 電療料１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029000         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料１ＮＲＷ
029010*
029020     WHEN 1 ALSO 2
029030     WHEN 2 ALSO 2
029040         COMPUTE 電療回数２ＲＷ = 電療回数２ＲＷ + 1
029050         COMPUTE 電療料２ＲＷ   = 電療料２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029060         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料２ＮＲＷ
029070*
029080     WHEN 1 ALSO 3
029090     WHEN 2 ALSO 3
029100         COMPUTE 電療回数３１ＲＷ = 電療回数３１ＲＷ + 1
029110         COMPUTE 電療料３１ＲＷ   = 電療料３１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029120         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料３１ＮＲＷ
029130     WHEN 3 ALSO 3
029140         COMPUTE 電療回数３ＲＷ = 電療回数３ＲＷ + 1
029150         COMPUTE 電療料３ＲＷ = 電療料３ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029160         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料３ＮＲＷ
029170*
029180     WHEN 1 ALSO 4
029190     WHEN 2 ALSO 4
029200         COMPUTE 電療回数４２ＲＷ = 電療回数４２ＲＷ + 1
029210         COMPUTE 電療料４２ＲＷ   = 電療料４２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029220         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４２ＮＲＷ
029230     WHEN 3 ALSO 4
029240         COMPUTE 電療回数４１ＲＷ = 電療回数４１ＲＷ + 1
029250         COMPUTE 電療料４１ＲＷ   = 電療料４１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029260         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４１ＮＲＷ
029270     WHEN 4 ALSO 4
029280         COMPUTE 電療回数４ＲＷ = 電療回数４ＲＷ + 1
029290         COMPUTE 電療料４ＲＷ   = 電療料４ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029300         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４ＮＲＷ
029310*
029320     WHEN 1 ALSO 5
029330     WHEN 2 ALSO 5
029340         COMPUTE 電療回数５３ＲＷ = 電療回数５３ＲＷ + 1
029350         COMPUTE 電療料５３ＲＷ   = 電療料５３ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029360         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５３ＮＲＷ
029370     WHEN 3 ALSO 5
029380         COMPUTE 電療回数５２ＲＷ = 電療回数５２ＲＷ + 1
029390         COMPUTE 電療料５２ＲＷ   = 電療料５２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029400         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５２ＮＲＷ
029410     WHEN 4 ALSO 5
029420         COMPUTE 電療回数５１ＲＷ = 電療回数５１ＲＷ + 1
029430         COMPUTE 電療料５１ＲＷ   = 電療料５１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029440         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５１ＮＲＷ
029450     WHEN 5 ALSO 5
029460         COMPUTE 電療回数５ＲＷ = 電療回数５ＲＷ + 1
029470         COMPUTE 電療料５ＲＷ = 電療料５ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029480         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５ＮＲＷ
      */６部位/120804
029320     WHEN 1 ALSO 6
029330     WHEN 2 ALSO 6
029340         COMPUTE 電療回数６３ＲＷ = 電療回数６３ＲＷ + 1
029350         COMPUTE 電療料６３ＲＷ   = 電療料６３ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029360         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料６３ＮＲＷ
029370     WHEN 3 ALSO 6
029380         COMPUTE 電療回数６２ＲＷ = 電療回数６２ＲＷ + 1
029390         COMPUTE 電療料６２ＲＷ   = 電療料６２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029400         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料６２ＮＲＷ
      */７部位/120804
029320     WHEN 1 ALSO 7
029330     WHEN 2 ALSO 7
029340         COMPUTE 電療回数７３ＲＷ = 電療回数７３ＲＷ + 1
029350         COMPUTE 電療料７３ＲＷ   = 電療料７３ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029360         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料７３ＮＲＷ
029370     WHEN 3 ALSO 7
029380         COMPUTE 電療回数７２ＲＷ = 電療回数７２ＲＷ + 1
029390         COMPUTE 電療料７２ＲＷ   = 電療料７２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029400         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料７２ＮＲＷ
029490     WHEN OTHER
029500         CONTINUE
029510     END-EVALUATE.
029520*
029530*================================================================*
029540 累計項目初期化 SECTION.
029550*
029560     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
029570            UNTIL   部位ＣＮＴ > 最大登録部位数Ｗ
029580         MOVE ZERO TO 初回処置料ＯＷ(部位ＣＮＴ)
029590     END-PERFORM.
029600*
029610     MOVE ZERO TO 初検料ＧＷ 初検加算料ＧＷ
029620                  再検料ＧＷ 往療料ＧＷ 往療加算料ＧＷ 往療回数ＧＷ
029630                  後療料ＧＷ 冷罨法料ＧＷ 温罨法料ＧＷ 電療料ＧＷ
029640                  金属副子料ＧＷ
029650                  初回処置料ＧＷ 施術情報提供料ＧＷ
029660                  費用額老人ＧＷ 負担額老人ＧＷ 負担額助成ＧＷ 負担額ＧＷ
029670                  初検時相談料ＧＷ 運動療法料ＧＷ 運動療法回数Ｗ 明細書発行加算料Ｗ 明細書発行加算日Ｗ.
029680*
029690     MOVE ZERO TO 後療料１ＲＷ 後療料２ＲＷ
029700                  後療料３ＲＷ 後療料３１ＲＷ
029710                  後療料４ＲＷ 後療料４１ＲＷ 後療料４２ＲＷ
029720                  後療料５ＲＷ 後療料５１ＲＷ 後療料５２ＲＷ 後療料５３ＲＷ
      */６部位～/120804
                        後療料６２ＲＷ 後療料６３ＲＷ 後療料７２ＲＷ 後療料７３ＲＷ.
029730     MOVE ZERO TO 冷罨法料１ＲＷ 冷罨法料２ＲＷ
029740                  冷罨法料３ＲＷ 冷罨法料３１ＲＷ
029750                  冷罨法料４ＲＷ 冷罨法料４１ＲＷ 冷罨法料４２ＲＷ
029760                  冷罨法料５ＲＷ 冷罨法料５１ＲＷ 冷罨法料５２ＲＷ 冷罨法料５３ＲＷ
      */６部位～/120804
                        冷罨法料６２ＲＷ 冷罨法料６３ＲＷ 冷罨法料７２ＲＷ 冷罨法料７３ＲＷ.
029770     MOVE ZERO TO 温罨法料１ＲＷ 温罨法料２ＲＷ
029780                  温罨法料３ＲＷ 温罨法料３１ＲＷ
029790                  温罨法料４ＲＷ 温罨法料４１ＲＷ 温罨法料４２ＲＷ
029800                  温罨法料５ＲＷ 温罨法料５１ＲＷ 温罨法料５２ＲＷ 温罨法料５３ＲＷ
      */６部位～/120804
                        温罨法料６２ＲＷ 温罨法料６３ＲＷ 温罨法料７２ＲＷ 温罨法料７３ＲＷ.
029810     MOVE ZERO TO 電療料１ＲＷ 電療料２ＲＷ
029820                  電療料３ＲＷ 電療料３１ＲＷ
029830                  電療料４ＲＷ 電療料４１ＲＷ 電療料４２ＲＷ
029840                  電療料５ＲＷ 電療料５１ＲＷ 電療料５２ＲＷ 電療料５３ＲＷ
      */６部位～/120804
                        電療料６２ＲＷ 電療料６３ＲＷ 電療料７２ＲＷ 電療料７３ＲＷ.
029850*
029860     MOVE ZERO TO 後療回数１ＲＷ 後療回数２ＲＷ
029870                  後療回数３ＲＷ 後療回数３１ＲＷ
029880                  後療回数４ＲＷ 後療回数４１ＲＷ 後療回数４２ＲＷ
029890                  後療回数５ＲＷ 後療回数５１ＲＷ 後療回数５２ＲＷ 後療回数５３ＲＷ.
029900     MOVE ZERO TO 冷罨法回数１ＲＷ 冷罨法回数２ＲＷ
029910                  冷罨法回数３ＲＷ 冷罨法回数３１ＲＷ
029920                  冷罨法回数４ＲＷ 冷罨法回数４１ＲＷ 冷罨法回数４２ＲＷ
029930                  冷罨法回数５ＲＷ 冷罨法回数５１ＲＷ 冷罨法回数５２ＲＷ 冷罨法回数５３ＲＷ.
029940     MOVE ZERO TO 温罨法回数１ＲＷ 温罨法回数２ＲＷ
029950                  温罨法回数３ＲＷ 温罨法回数３１ＲＷ
029960                  温罨法回数４ＲＷ 温罨法回数４１ＲＷ 温罨法回数４２ＲＷ
029970                  温罨法回数５ＲＷ 温罨法回数５１ＲＷ 温罨法回数５２ＲＷ 温罨法回数５３ＲＷ.
029980     MOVE ZERO TO 電療回数１ＲＷ 電療回数２ＲＷ
029990                  電療回数３ＲＷ 電療回数３１ＲＷ
030000                  電療回数４ＲＷ 電療回数４１ＲＷ 電療回数４２ＲＷ
030010                  電療回数５ＲＷ 電療回数５１ＲＷ 電療回数５２ＲＷ 電療回数５３ＲＷ.
030020*
030030     MOVE ZERO TO 初検時間外Ｗ 初検休日Ｗ 初検深夜Ｗ 
030040                  往療夜間Ｗ 往療難路Ｗ 往療暴風雨Ｗ 往療時間外Ｗ
030050                  金属副子大Ｗ 金属副子中Ｗ 金属副子小Ｗ 金属副子回数Ｗ.
030060*
030070*老人保険改正の対応0101
030080*当月の累計金額と回数
030090*     MOVE ZERO TO 連老負－キー.
030100*     INITIALIZE   連老負－キー.
030110*枝番の累計金額0101
030120*     MOVE ZERO TO 枝番累計金額Ｗ.
030131*
030132     MOVE ZERO TO 枝番負担累計額Ｗ.
030133     MOVE ZERO TO 連計算助成累計額－枝番負担累計額.
030135*
      */負担区分２２(大阪府平成30年4月～)
           MOVE ZERO TO 連計算助成累計額－負担額助成１０.
030140*
030150*================================================================*
030160 健保負担額計算 SECTION.
030170*
030180****************************************************************
030190* 負担金計算方法の指定  負担金計算区分  費用・請求額   負担額  *
030200* 　　　　　　　　　　　　　　０　　　　　１円単位    １円単位 *
030210*              　　　　 　　　１　　　　１０円単位             *
030220*              　　　　　　　 ２　　　　　１円単位  １０円単位 *
030230****************************************************************
030240*     MOVE ZERO TO 連計－負担金計算区分
030241*
           MOVE ZERO TO 本体一部負担金Ｗ.
030243*
030250*
030260*/負担額計算ＰＧをコール
030270     EVALUATE TRUE
030280*     WHEN (連計－施術和暦年月 >= 40909) AND (連計－施術和暦年月 <= 41409)
030290*         CALL   "HUT9709"
030300*         CANCEL "HUT9709"
030310     WHEN (連計－施術和暦年月 >= 41410) AND (連計－施術和暦年月 <= 99999)
030320         CALL   "HUT0210"
030330         CANCEL "HUT0210"
030340     WHEN OTHER
030350         DISPLAY "施術日に合った計算ＰＧがありません" 
030360                 " 受診者№="   連計－患者コード
030370                 " 施術年月日=" 連計－施術年 連計－施術月 連計－施術日 UPON CONS
030371*-----------------------------------------*
030372         CALL "actcshm"  WITH C LINKAGE
030373*-----------------------------------------*
030380     END-EVALUATE
030390     IF PROGRAM-STATUS = ZERO
030400*/エラー表示の修正
      */保険証画面からコンソールエラー表示を削除する↓↓↓/120928
030410*         PERFORM コンソールエラー確認
030420*         DISPLAY "負担額の取得に失敗しました"
030430*                 " 受診者№=" 連計－患者コード
030440*                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
030450**-----------------------------------------*
030460*         CALL "actcshm"  WITH C LINKAGE
030470**-----------------------------------------*
030480*         ACCEPT  キー入力 FROM CONS
               MOVE  "負担額の取得に失敗しました" TO 連メ７－メッセージ１
               MOVE  SPACE                        TO 連メ７－メッセージ２
      */コンソール画面に年月患者コードを表示する/130601
               STRING "受診者№="       DELIMITED BY SIZE
                       連計－患者コード DELIMITED BY SIZE
                     " 施術年月="       DELIMITED BY SIZE
                       連計－施術年     DELIMITED BY SIZE
                       連計－施術月     DELIMITED BY SIZE
                  INTO 連メ７－メッセージ２
               END-STRING
               CALL   "MSG007"
               CANCEL "MSG007"
      */保険証画面からコンソールエラー表示を削除する↑↑↑/120928
030490         PERFORM ファイル閉鎖
030500         EXIT PROGRAM
030510     END-IF.
      */本体分の窓口金/120911
           MOVE 連計日計窓口－負担額 TO 本体一部負担金Ｗ.
030520*================================================================*
030530 老人負担額計算 SECTION.
030540*
030550* 一日毎の計算を累計する
030560****************************************************************
030570* 負担金計算方法の指定  負担金計算区分  費用・請求額   負担額  *
030580* 　　　　　　　　　　　　　　０　　　　　１円単位    １円単位 *
030590*              　　　　 　　　１　　　　１０円単位             *
030600*              　　　　　　　 ２　　　　　１円単位  １０円単位 *
030610****************************************************************
030611*
030613*
030620*（健保＋）２７老人
030630     EVALUATE TRUE
030640     WHEN (( 保険種別Ｗ >= 01   ) AND ( 保険種別Ｗ <= 09 )) AND
030650           ( 公費種別Ｗ  = 05   ) AND
030660           ( 助成種別Ｗ  = ZERO )
030670         MOVE ZERO TO 連老計算－月日Ｆ
030680*/負担額計算ＰＧをコール
030690         EVALUATE TRUE
030700*         WHEN (連計－施術和暦年月 >= 40909) AND (連計－施術和暦年月 <= 41212)
030710*             CALL   "HUR9709"
030720*             CANCEL "HUR9709"
030730*         WHEN (連計－施術和暦年月 >= 41301) AND (連計－施術和暦年月 <= 41409)
030740*             CALL   "HUR0101"
030750*             CANCEL "HUR0101"
030760         WHEN (連計－施術和暦年月 >= 41410) AND (連計－施術和暦年月 <= 99999)
030770             CALL   "HUR0210"
030780             CANCEL "HUR0210"
030790         WHEN OTHER
030800             DISPLAY "施術日に合った計算ＰＧがありません" 
030810                     " 受診者№="   連計－患者コード
030820                     " 施術年月日=" 連計－施術年 連計－施術月 連計－施術日 UPON CONS
030821*-----------------------------------------*
030822             CALL "actcshm"  WITH C LINKAGE
030823*-----------------------------------------*
030830         END-EVALUATE
030840         IF PROGRAM-STATUS NOT = ZERO
030850* 未入金累計額と加算するためワーク変数に退避
030860*
030870             COMPUTE 費用額老人ＧＷ = 費用額老人ＧＷ + 連計－費用額老人
030880             COMPUTE 負担額老人ＧＷ = 負担額老人ＧＷ + 連計－負担額老人
030890             COMPUTE 請求額老人ＧＷ = 請求額老人ＧＷ + 連計－請求額老人
030900         END-IF
030910*（健保＋）４１老人
030920     WHEN (( 保険種別Ｗ >= 01   ) AND ( 保険種別Ｗ <= 09 )) AND
030930           ( 公費種別Ｗ  = ZERO ) AND
030940           ( 助成種別Ｗ  = 51 )
030950*/４１老に準ずる助成の対応
030960     WHEN (( 保険種別Ｗ    >= 01  ) AND ( 保険種別Ｗ <= 09 )) AND
030970*/２７老人＋４１に準ずる助成の対応 １行コメント/071005
030980*           ( 公費種別Ｗ     = ZERO) AND 
030990           ((助成種別Ｗ NOT = ZERO) AND (負担区分Ｗ = 09 OR 10))
031000         MOVE ZERO TO 連老計算－月日Ｆ
031010*/負担額計算ＰＧをコール
031020         EVALUATE TRUE
031030*         WHEN (連計－施術和暦年月 >= 40909) AND (連計－施術和暦年月 <= 41212)
031040*             CALL   "HUR9709"
031050*             CANCEL "HUR9709"
031060*         WHEN (連計－施術和暦年月 >= 41301) AND (連計－施術和暦年月 <= 41409)
031070*             CALL   "HUR0101"
031080*             CANCEL "HUR0101"
031090         WHEN (連計－施術和暦年月 >= 41410) AND (連計－施術和暦年月 <= 99999)
031100             CALL   "HUR0210"
031110             CANCEL "HUR0210"
031120         WHEN OTHER
031130             DISPLAY "施術日に合った計算ＰＧがありません" 
031140                     " 受診者№="   連計－患者コード
031150                     " 施術年月日=" 連計－施術年 連計－施術月 連計－施術日 UPON CONS
031151*-----------------------------------------*
031152             CALL "actcshm"  WITH C LINKAGE
031153*-----------------------------------------*
031160         END-EVALUATE
031170         IF PROGRAM-STATUS NOT = ZERO
031180             COMPUTE 負担額ＧＷ     = 負担額ＧＷ     + 連計－負担額
031190             COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 連計－負担額助成
031200         END-IF
031210     WHEN OTHER
031220*/エラー表示の修正
031230         PERFORM コンソールエラー確認
031240         DISPLAY "計算条件がみつかりません１"
031250                 " 受診者№=" 連計－患者コード
031260                 " 保険種別=" 連計－保険種別
031270                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
031280*-----------------------------------------*
031290         CALL "actcshm"  WITH C LINKAGE
031300*-----------------------------------------*
031310*         ACCEPT  キー入力 FROM CONS
031320         PERFORM ファイル閉鎖
031330         EXIT PROGRAM
031340     END-EVALUATE.
031350*
031360*================================================================*
031370 助成負担額計算 SECTION.
031380****************************************************************
031390* 負担金計算方法の指定  負担金計算区分    健保      その他     *
031400* 　　　　　　　　　　　　　　０　　　　　１円単位    １円単位 *
031410*              　　　　 　　　１　　　　１０円単位             *
031420*              　　　　　　　 ２　　　　　１円単位  １０円単位 *
031430****************************************************************
031431*
           MOVE ZERO TO 本体一部負担金Ｗ.
031440*
031450* 健保＋助成
031460* 　　（生保・母子・身障・被爆・乳幼児・４１老人・その他助成）
031470     EVALUATE TRUE
031480     WHEN (( 保険種別Ｗ >= 01 ) AND ( 保険種別Ｗ <= 09 )) AND
031490           ( 公費種別Ｗ = ZERO  ) AND
031500          (( 助成種別Ｗ >= 52 ) AND ( 助成種別Ｗ <= 60 ))
031510* 19990201
031520     WHEN (( 保険種別Ｗ >= 01 ) AND ( 保険種別Ｗ <= 09 )) AND
031530           ( 公費種別Ｗ = ZERO  ) AND
031540           ( 助成種別Ｗ >= 50 )
031550     WHEN ( 保険種別Ｗ = ZERO ) AND
031560          ( 公費種別Ｗ = ZERO ) AND
031570          ( 助成種別Ｗ = 50   )
031580*
031590*         MOVE ZERO TO 連計－負担金計算区分
031600*
031610*/NG負担区分13はレセの負担金も10円単位で計算する。/100302
031620* NG        IF 負担区分Ｗ = 13
031630* NG            MOVE 2 TO 連計－負担金計算区分
031640* NG        END-IF
031650*/負担額計算ＰＧをコール
031660         EVALUATE TRUE
031670*         WHEN (連計－施術和暦年月 >= 40909) AND (連計－施術和暦年月 <= 41409)
031680*             CALL   "HUT9709"
031690*             CANCEL "HUT9709"
031700         WHEN (連計－施術和暦年月 >= 41410) AND (連計－施術和暦年月 <= 99999)
031710             CALL   "HUT0210"
031720             CANCEL "HUT0210"
031730         WHEN OTHER
031740             DISPLAY "施術日に合った計算ＰＧがありません" 
031750                     " 受診者№="   連計－患者コード
031760                     " 施術年月日=" 連計－施術年 連計－施術月 連計－施術日 UPON CONS
031761*-----------------------------------------*
031762             CALL "actcshm"  WITH C LINKAGE
031763*-----------------------------------------*
031770         END-EVALUATE
      */助成時の本体分の窓口金/120906
               MOVE 連計日計窓口－負担額 TO 本体一部負担金Ｗ
031780         IF PROGRAM-STATUS NOT = ZERO
031790*/負担区分16の対応。月計算ＰＧからコールされた場合だけ一定金額以下の計算をする/091014
031800             MOVE 1    TO 連計－月計算フラグ
031810*/負担額計算ＰＧをコール
031820             EVALUATE TRUE
031830             WHEN (連計－施術和暦年月 >= 40909) AND (連計－施術和暦年月 <= 99999)
031831*
031832***
031833                 MOVE 負担額助成ＧＷ TO 連計算助成累計額－負担額助成
031834***
031835*                / 月計で負担区分Ｗ = 05 OR 16 の特別処理 /
031836                 IF ( 連計算助成月途中－日計区分 = ZERO ) AND ( 負担区分Ｗ = 05 OR 16 )
031838                    MOVE 枝番負担累計額Ｗ TO 連計算助成累計額－枝番負担累計額
031843                 END-IF
031844****
031851***
031852                 CALL   "HUJ9709"
031853                 CANCEL "HUJ9709"
031860             WHEN OTHER
031870                 DISPLAY "施術日に合った計算ＰＧがありません" " 受診者№="   連計－患者コード " 施術年月日=" 連計－施術年 連計－施術月 連計－施術日 UPON CONS
031872*-----------------------------------------*
031873                 CALL "actcshm"  WITH C LINKAGE
031874*-----------------------------------------*
031880             END-EVALUATE
031890             MOVE ZERO TO 連計－月計算フラグ
031900             IF PROGRAM-STATUS NOT = ZERO
031910                 COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 連計－負担額助成
      */負担区分２２(大阪府平成30年4月～)↓↓↓/20180307
                       IF 負担区分Ｗ = 22
      */レセプト3000円未満で窓口金3000円を超えた場合、後から3000円を超えたら負担額助成を合わせる↓↓↓
      */負担額助成の累計が限度額未満で窓口金が限度額に達した場合で、その後も通院して負担額が増えた場合は負担額助成を調整する？
                           IF (負担額助成ＧＷ < 計算限度額Ｗ) AND
                              (連計算助成累計額－負担額助成１０ >= 計算限度額Ｗ) AND
                              (連計－負担額助成 = ZERO) AND
                              (連計－負担額 NOT = ZERO)
                               IF 連計－負担額 < 負担一定金額Ｗ
                                   COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 連計－負担額
                               ELSE
                                   COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 負担一定金額Ｗ
                               END-IF
      */レセプト3000円未満で窓口金3000円を超えた場合、後から3000円を超えたら負担額助成を合わせる↑↑↑
                           END-IF
                           IF 負担額助成ＧＷ > 計算限度額Ｗ
                               MOVE 計算限度額Ｗ TO 負担額助成ＧＷ
                           END-IF
                       END-IF
      */負担区分２２(大阪府平成30年4月～)↑↑↑/20180307
031920             END-IF
031930         END-IF
031940* その他
031950     WHEN OTHER
031960*/エラー表示の修正
031970         PERFORM コンソールエラー確認
031980         DISPLAY "計算条件がみつかりません２"
031990                 " 受診者№=" 連計－患者コード
032000                 " 保険種別=" 連計－保険種別
032010                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
032020*-----------------------------------------*
032030         CALL "actcshm"  WITH C LINKAGE
032040*-----------------------------------------*
032050*         ACCEPT  キー入力 FROM CONS
032060         PERFORM ファイル閉鎖
032070         EXIT PROGRAM
032080     END-EVALUATE.
032090*
032100*================================================================*
032110 老人助成負担額計算 SECTION.
032120****************************************************************
032130* 負担金計算方法の指定  負担金計算区分    健保      その他     *
032140* 　　　　　　　　　　　　　　０　　　　　１円単位    １円単位 *
032150*              　　　　 　　　１　　　　１０円単位             *
032160*              　　　　　　　 ２　　　　　１円単位  １０円単位 *
032170****************************************************************
032171*
           MOVE ZERO TO 本体一部負担金Ｗ.
032175*
032180*２７老人＋助成
032190     EVALUATE TRUE
032200     WHEN (( 保険種別Ｗ >= 01   ) AND ( 保険種別Ｗ <= 09 )) AND
032210           ( 公費種別Ｗ  = 05   ) AND
032220           ( 助成種別Ｗ  NOT = ZERO )
032230*         MOVE ZERO TO 連計－負担金計算区分
032240         MOVE ZERO TO 連老計算－月日Ｆ
032250*/負担区分16の対応。月計算ＰＧからコールされた場合だけ一定金額以下の計算をする/091015
032260         MOVE 1    TO 連計－月計算フラグ
032270*/負担額計算ＰＧをコール
032280         EVALUATE TRUE
032290*         WHEN (連計－施術和暦年月 >= 40909) AND (連計－施術和暦年月 <= 41212)
032300*             CALL   "HUR9709"
032310*             CANCEL "HUR9709"
032320*         WHEN (連計－施術和暦年月 >= 41301) AND (連計－施術和暦年月 <= 41409)
032330*             CALL   "HUR0101"
032340*             CANCEL "HUR0101"
032350         WHEN (連計－施術和暦年月 >= 41410) AND (連計－施術和暦年月 <= 99999)
032351***
032352             MOVE 負担額助成ＧＷ TO 連計算助成累計額－負担額助成
032353***
032355*            / 月計で負担区分Ｗ = 05 OR 16 の特別処理 /
032356             IF ( 連計算助成月途中－日計区分 = ZERO ) AND ( 負担区分Ｗ = 05 OR 16 )
032357                MOVE 枝番負担累計額Ｗ TO 連計算助成累計額－枝番負担累計額
032358             END-IF
032360****
032361***
032362             CALL   "HUR0210"
032370             CANCEL "HUR0210"
032380         WHEN OTHER
032390             DISPLAY "施術日に合った計算ＰＧがありません" 
032400                     " 受診者№="   連計－患者コード
032410                     " 施術年月日=" 連計－施術年 連計－施術月 連計－施術日 UPON CONS
032411*-----------------------------------------*
032412             CALL "actcshm"  WITH C LINKAGE
032413*-----------------------------------------*
032420         END-EVALUATE
032430         MOVE ZERO TO 連計－月計算フラグ
      */後期＋助成時の本体分の窓口金/120907
               COMPUTE 本体一部負担金Ｗ ROUNDED = 連計－負担額老人 / 10
               COMPUTE 本体一部負担金Ｗ         = 本体一部負担金Ｗ * 10
032440         IF PROGRAM-STATUS NOT = ZERO
032450* 未入金累計額と加算するためワーク変数に退避
032460             COMPUTE 費用額老人ＧＷ = 費用額老人ＧＷ + 連計－費用額老人
032470             COMPUTE 負担額老人ＧＷ = 負担額老人ＧＷ + 連計－負担額老人
032480             COMPUTE 請求額老人ＧＷ = 請求額老人ＧＷ + 連計－請求額老人
032490             COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 連計－負担額助成
032500             COMPUTE 請求額助成ＧＷ = 請求額助成ＧＷ + 連計－請求額助成
      */負担区分２２(大阪府平成30年4月～)↓↓↓/20180307
                   IF 負担区分Ｗ = 22
      */レセプト3000円未満で窓口金3000円を超えた場合、後から3000円を超えたら負担額助成を合わせる？↓↓↓
      */負担額助成の累計が限度額未満で窓口金が限度額に達した場合で、その後も通院して負担額が増えた場合は負担額助成を調整する？
                       IF (負担額助成ＧＷ < 計算限度額Ｗ) AND
                          (連計算助成累計額－負担額助成１０ >= 計算限度額Ｗ) AND
                          (連計－負担額助成     = ZERO) AND
                          (連計－負担額老人 NOT = ZERO)
                           IF 連計－負担額老人  < 負担一定金額Ｗ
                               COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 連計－負担額老人
                           ELSE
                               COMPUTE 負担額助成ＧＷ = 負担額助成ＧＷ + 負担一定金額Ｗ
                           END-IF
      */レセプト3000円未満で窓口金3000円を超えた場合、後から3000円を超えたら負担額助成を合わせる？↑↑↑
                       END-IF
                       IF 負担額助成ＧＷ > 計算限度額Ｗ
                           MOVE 計算限度額Ｗ TO 負担額助成ＧＷ
                       END-IF
                   END-IF
      */負担区分２２(大阪府平成30年4月～)↑↑↑/20180307
HILO  *             DISPLAY "210-11 " 連計－施術和暦年月日 " " 負担額助成ＧＷ " " 連計－負担額助成 " " 連計－負担額 " " 負担一定金額Ｗ
032510         END-IF
032520     WHEN OTHER
032530*/エラー表示の修正
032540         PERFORM コンソールエラー確認
032550         DISPLAY "計算条件がみつかりません３"
032560                 " 受診者№=" 連計－患者コード
032570                 " 保険種別=" 連計－保険種別
032580                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
032590*-----------------------------------------*
032600         CALL "actcshm"  WITH C LINKAGE
032610*-----------------------------------------*
032620*         ACCEPT  キー入力 FROM CONS
032630         PERFORM ファイル閉鎖
032640         EXIT PROGRAM
032650     END-EVALUATE.
032660*
032670*================================================================*
032680 連結項目セット SECTION.
032690*
032700* 費用額・負担額・請求額は、主処理にセット
032710*
032720* 初期化ルーチンにて初期化済
032730*
032740     MOVE 初検料ＧＷ     TO 連料－初検料.
032750     MOVE 初検加算料ＧＷ TO 連料－初検加算料.
032760     MOVE 再検料ＧＷ     TO 連料－再検料.
      */レセプト往療距離が正しく転記されない/170224
032770*     MOVE 往療距離Ｗ     TO 連料－往療距離.
032770     MOVE 月往療距離Ｗ     TO 連料－往療距離.
032780     MOVE 往療回数ＧＷ   TO 連料－往療回数.
032790     MOVE 往療料ＧＷ     TO 連料－往療料.
032800     MOVE 往療加算料ＧＷ TO 連料－往療加算料.
032810     MOVE 金属副子料ＧＷ TO 連料－金属副子加算料.
032820     MOVE 施術情報提供料ＧＷ TO 連料－施術情報提供料.
032830     MOVE 初検時相談料ＧＷ   TO 連料－初検時相談料.
032840*
032850     MOVE 初検時間外Ｗ   TO 連料－初検時間外.
032860     MOVE 初検休日Ｗ     TO 連料－初検休日.  
032870     MOVE 初検深夜Ｗ     TO 連料－初検深夜.  
032880     MOVE 往療夜間Ｗ     TO 連料－往療夜間.  
032890     MOVE 往療難路Ｗ     TO 連料－往療難路.  
032900     MOVE 往療暴風雨Ｗ   TO 連料－往療暴風雨.
032910     MOVE 往療時間外Ｗ   TO 連料－往療時間外.
032920*
032930     MOVE 金属副子大Ｗ   TO 連料－大.
032940     MOVE 金属副子中Ｗ   TO 連料－中.
032950     MOVE 金属副子小Ｗ   TO 連料－小.
           MOVE 金属副子回数Ｗ TO 連料－金属副子回数.
      *
           IF 運動療法回数Ｗ > ZERO
               MOVE 運動療法回数Ｗ   TO 連料－運動後療回数
               MOVE 運動療法料ＡＷ   TO 連料－運動後療単価
               MOVE 運動療法料ＧＷ   TO 連料－運動後療料
           END-IF.
           MOVE 明細書発行加算料ＧＷ TO 連料－明細書発行加算料.
           MOVE 明細書発行加算日Ｗ   TO 連料－明細書発行加算日.
032960*
032970     COMPUTE 連料－小計 = 初検料ＧＷ + 初検加算料ＧＷ +
032980                          再検料ＧＷ + 往療料ＧＷ +
032990                          往療加算料ＧＷ + 金属副子料ＧＷ +
033000                          施術情報提供料ＧＷ +
033010                          初検時相談料ＧＷ
033020*
033030     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
033040             UNTIL   部位ＣＮＴ > 部位数Ｗ 
033050         MOVE 初回処置料ＯＷ(部位ＣＮＴ) TO 連料－初回処置料(部位ＣＮＴ)
033060         COMPUTE 連料－初回処置料計 = 連料－初回処置料計 + 初回処置料ＯＷ(部位ＣＮＴ)
033070     END-PERFORM.
033080*
033090**********
033100* １部位 *
033110**********
033120     IF 部位合計Ｗ１ > ZERO
033130         IF 後療回数１ＲＷ > ZERO
033140*             MOVE 後療単価ＯＷ(1, 1) TO 連料－後療単価１
033150             MOVE 後療単価ＯＷ(1)    TO 連料－後療単価１
033160             MOVE 後療回数１ＲＷ     TO 連料－後療回数１
033170             MOVE 後療料１ＲＷ       TO 連料－後療料１
033180         END-IF
033190         MOVE 冷罨法回数１ＲＷ   TO 連料－冷罨法回数１
033200         MOVE 冷罨法料１ＲＷ     TO 連料－冷罨法料１
033210         MOVE 温罨法回数１ＲＷ   TO 連料－温罨法回数１
033220         MOVE 温罨法料１ＲＷ     TO 連料－温罨法料１
033230         MOVE 電療回数１ＲＷ     TO 連料－電療回数１
033240         MOVE 電療料１ＲＷ       TO 連料－電療料１
033250* 上記１００％の金額を加算する
033260         MOVE 部位合計Ｗ１       TO 連料－小計１
033270         IF 長期区分Ｗ(1) > ZERO
033280             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率１
033290         END-IF
033300         MOVE 部位合計長期Ｗ１   TO 連料－長期込小計１
033310     END-IF.
033320**********
033330* ２部位 *
033340**********
033350     IF 部位合計Ｗ２ > ZERO
033360         IF 後療回数２ＲＷ > ZERO
033370*             MOVE 後療単価ＯＷ(1, 2) TO 連料－後療単価２
033380*             MOVE 後療単価ＯＷ(2, 2) TO 連料－後療単価２
033390             MOVE 後療単価ＯＷ(2)    TO 連料－後療単価２
033400             MOVE 後療回数２ＲＷ     TO 連料－後療回数２
033410             MOVE 後療料２ＲＷ       TO 連料－後療料２
033420         END-IF
033430         MOVE 冷罨法回数２ＲＷ   TO 連料－冷罨法回数２
033440         MOVE 冷罨法料２ＲＷ     TO 連料－冷罨法料２
033450         MOVE 温罨法回数２ＲＷ   TO 連料－温罨法回数２
033460         MOVE 温罨法料２ＲＷ     TO 連料－温罨法料２
033470         MOVE 電療回数２ＲＷ     TO 連料－電療回数２
033480         MOVE 電療料２ＲＷ       TO 連料－電療料２
033490* 上記１００％の金額を加算する
033500         MOVE 部位合計Ｗ２       TO 連料－小計２
033510         IF 長期区分Ｗ(2) > ZERO
033520             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率２
033530         END-IF
033540         MOVE 部位合計長期Ｗ２   TO 連料－長期込小計２
033550     END-IF.
033560******************
033570* ３部位／８割 *
033580******************
033590     IF 部位合計Ｗ３ > ZERO
033600         IF 後療回数３ＲＷ > ZERO
033610*             MOVE 後療単価ＯＷ(3, 3) TO 連料－後療単価３８
033620             MOVE 後療単価ＯＷ(3)    TO 連料－後療単価３８
033630             MOVE 後療回数３ＲＷ     TO 連料－後療回数３８
033640             MOVE 後療料３ＲＷ       TO 連料－後療料３８
033650         END-IF
033660         MOVE 冷罨法回数３ＲＷ   TO 連料－冷罨法回数３８
033670         MOVE 冷罨法料３ＲＷ     TO 連料－冷罨法料３８
033680         MOVE 温罨法回数３ＲＷ   TO 連料－温罨法回数３８
033690         MOVE 温罨法料３ＲＷ     TO 連料－温罨法料３８
033700         MOVE 電療回数３ＲＷ     TO 連料－電療回数３８
033710         MOVE 電療料３ＲＷ       TO 連料－電療料３８
033720* 上記１００％の金額を加算する
033730         MOVE 部位合計Ｗ３       TO 連料－小計３８
033740         MOVE 部位合計多部位Ｗ３   TO 連料－多部位込小計３８
033750         IF 長期区分Ｗ(3) > ZERO
033760             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率３８
033770         END-IF
033780         MOVE 部位合計長期Ｗ３   TO 連料－長期込小計３８
HILO  *         DISPLAY "210-5 " 部位合計長期Ｗ３
033790     END-IF.
033800******************
033810* ３部位／１０割 *
033820******************
033830     IF 部位合計Ｗ３１ > ZERO
033840         MOVE 逓減開始月３１ＲＷ TO 連料－逓減開始月３０
033850         MOVE 逓減開始日３１ＲＷ TO 連料－逓減開始日３０
033860         IF 後療回数３１ＲＷ > ZERO
033870*             MOVE 後療単価ＯＷ(1, 3) TO 連料－後療単価３０
033880*             MOVE 後療単価ＯＷ(2, 3) TO 連料－後療単価３０
033890             MOVE 後療単価ＯＷ(3)  TO 連料－後療単価３０
033900             MOVE 後療回数３１ＲＷ TO 連料－後療回数３０
033910             MOVE 後療料３１ＲＷ   TO 連料－後療料３０
033920         END-IF
033930         MOVE 冷罨法回数３１ＲＷ   TO 連料－冷罨法回数３０
033940         MOVE 冷罨法料３１ＲＷ     TO 連料－冷罨法料３０
033950         MOVE 温罨法回数３１ＲＷ   TO 連料－温罨法回数３０
033960         MOVE 温罨法料３１ＲＷ     TO 連料－温罨法料３０
033970         MOVE 電療回数３１ＲＷ     TO 連料－電療回数３０
033980         MOVE 電療料３１ＲＷ       TO 連料－電療料３０
033990* 上記１００％の金額を加算する
034000         MOVE 部位合計Ｗ３１       TO 連料－小計３０
034010         IF 長期区分Ｗ(3) > ZERO
034020             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率３０
034030         END-IF
034040         MOVE 部位合計長期Ｗ３１   TO 連料－長期込小計３０
034050     END-IF.
034060****************
034070* ４部位／５割 *
034080****************
034081*/４部位目０割。多部位込小計がゼロの時、明細を転記しない。/100518
034082     IF 部位合計多部位Ｗ４ > ZERO
034083         IF 部位合計Ｗ４ > ZERO
034084             IF 後療回数４ＲＷ > ZERO
034085*                 MOVE 後療単価ＯＷ(4, 4) TO 連料－後療単価４５
034086                 MOVE 後療単価ＯＷ(4)    TO 連料－後療単価４５
034087                 MOVE 後療回数４ＲＷ     TO 連料－後療回数４５
034088                 MOVE 後療料４ＲＷ       TO 連料－後療料４５
034089             END-IF
034090             MOVE 冷罨法回数４ＲＷ   TO 連料－冷罨法回数４５
034091             MOVE 冷罨法料４ＲＷ     TO 連料－冷罨法料４５
034092             MOVE 温罨法回数４ＲＷ   TO 連料－温罨法回数４５
034093             MOVE 温罨法料４ＲＷ     TO 連料－温罨法料４５
034094             MOVE 電療回数４ＲＷ     TO 連料－電療回数４５
034095             MOVE 電療料４ＲＷ       TO 連料－電療料４５
034096*     上記１００％の金額を加算する
034097             MOVE 部位合計Ｗ４       TO 連料－小計４５
034098             MOVE 部位合計多部位Ｗ４ TO 連料－多部位込小計４５
034099             IF 長期区分Ｗ(4) > ZERO
034100                 MOVE 長期逓減率ＫＷ TO 連料－長期逓減率４５
034101             END-IF
034102             MOVE 部位合計長期Ｗ４   TO 連料－長期込小計４５
034103         END-IF
034104     END-IF.
034105*
034300****************
034310* ４部位／８割 *
034320****************
034330     IF 部位合計Ｗ４１ > ZERO
034340         MOVE 逓減開始月４１ＲＷ TO 連料－逓減開始月４８
034350         MOVE 逓減開始日４１ＲＷ TO 連料－逓減開始日４８
034360         IF 後療回数４１ＲＷ > ZERO
034370*             MOVE 後療単価ＯＷ(3, 4) TO 連料－後療単価４８
034380             MOVE 後療単価ＯＷ(4)      TO 連料－後療単価４８
034390             MOVE 後療回数４１ＲＷ     TO 連料－後療回数４８
034400             MOVE 後療料４１ＲＷ       TO 連料－後療料４８
034410         END-IF
034420         MOVE 冷罨法回数４１ＲＷ   TO 連料－冷罨法回数４８
034430         MOVE 冷罨法料４１ＲＷ     TO 連料－冷罨法料４８
034440         MOVE 温罨法回数４１ＲＷ   TO 連料－温罨法回数４８
034450         MOVE 温罨法料４１ＲＷ     TO 連料－温罨法料４８
034460         MOVE 電療回数４１ＲＷ     TO 連料－電療回数４８
034470         MOVE 電療料４１ＲＷ       TO 連料－電療料４８
034480* 上記１００％の金額を加算する
034490         MOVE 部位合計Ｗ４１       TO 連料－小計４８
034500         MOVE 部位合計多部位Ｗ４１   TO 連料－多部位込小計４８
034510         IF 長期区分Ｗ(4) > ZERO
034520             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率４８
034530         END-IF
034540         MOVE 部位合計長期Ｗ４１   TO 連料－長期込小計４８
034550     END-IF.
034560******************
034570* ４部位／１０割 *
034580******************
034590     IF 部位合計Ｗ４２ > ZERO
034600         MOVE 逓減開始月４２ＲＷ TO 連料－逓減開始月４０
034610         MOVE 逓減開始日４２ＲＷ TO 連料－逓減開始日４０
034620         IF 後療回数４２ＲＷ > ZERO
034630*             MOVE 後療単価ＯＷ(1, 4) TO 連料－後療単価４０
034640*             MOVE 後療単価ＯＷ(2, 4) TO 連料－後療単価４０
034650             MOVE 後療単価ＯＷ(4)      TO 連料－後療単価４０
034660             MOVE 後療回数４２ＲＷ     TO 連料－後療回数４０
034670             MOVE 後療料４２ＲＷ       TO 連料－後療料４０
034680         END-IF
034690         MOVE 冷罨法回数４２ＲＷ   TO 連料－冷罨法回数４０
034700         MOVE 冷罨法料４２ＲＷ     TO 連料－冷罨法料４０
034710         MOVE 温罨法回数４２ＲＷ   TO 連料－温罨法回数４０
034720         MOVE 温罨法料４２ＲＷ     TO 連料－温罨法料４０
034730         MOVE 電療回数４２ＲＷ     TO 連料－電療回数４０
034740         MOVE 電療料４２ＲＷ       TO 連料－電療料４０
034750* 上記１００％の金額を加算する
034760         MOVE 部位合計Ｗ４２       TO 連料－小計４０
034770         IF 長期区分Ｗ(4) > ZERO
034780             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率４０
034790         END-IF
034800         MOVE 部位合計長期Ｗ４２   TO 連料－長期込小計４０
034810     END-IF.
034820********************
034830* ５部位／２．５割 *
034840********************
034841*/５部位目０割。多部位込小計がゼロの時、明細を転記しない。/100518
034842     IF 部位合計多部位Ｗ５ > ZERO
034843         IF 部位合計Ｗ５ > ZERO
034844             IF 後療回数５ＲＷ > ZERO
034845*                 MOVE 後療単価ＯＷ(5, 5) TO 連料－後療単価５２
034846                 MOVE 後療単価ＯＷ(5)    TO 連料－後療単価５２
034847                 MOVE 後療回数５ＲＷ     TO 連料－後療回数５２
034848                 MOVE 後療料５ＲＷ       TO 連料－後療料５２
034849             END-IF
034850             MOVE 冷罨法回数５ＲＷ   TO 連料－冷罨法回数５２
034851             MOVE 冷罨法料５ＲＷ     TO 連料－冷罨法料５２
034852             MOVE 温罨法回数５ＲＷ   TO 連料－温罨法回数５２
034853             MOVE 温罨法料５ＲＷ     TO 連料－温罨法料５２
034854             MOVE 電療回数５ＲＷ     TO 連料－電療回数５２
034855             MOVE 電療料５ＲＷ       TO 連料－電療料５２
034856*     上記１００％の金額を加算する
034857             MOVE 部位合計Ｗ５       TO 連料－小計５２
034858             MOVE 部位合計多部位Ｗ５ TO 連料－多部位込小計５２
034859             IF 長期区分Ｗ(5) > ZERO
034860                 MOVE 長期逓減率ＫＷ TO 連料－長期逓減率５２
034861             END-IF
034862             MOVE 部位合計長期Ｗ５   TO 連料－長期込小計５２
034863         END-IF
034864     END-IF.
034865*
035060****************
035070* ５部位／５割 *
035080****************
035081*/４部位目０割。多部位込小計がゼロの時、明細を転記しない。/100518
035082     IF 部位合計多部位Ｗ５１ > ZERO
035083         IF 部位合計Ｗ５１ > ZERO
035084             MOVE 逓減開始月５１ＲＷ TO 連料－逓減開始月５５
035085             MOVE 逓減開始日５１ＲＷ TO 連料－逓減開始日５５
035086             IF 後療回数５１ＲＷ > ZERO
035087*                 MOVE 後療単価ＯＷ(4, 5) TO 連料－後療単価５５
035088                 MOVE 後療単価ＯＷ(5)      TO 連料－後療単価５５
035089                 MOVE 後療回数５１ＲＷ     TO 連料－後療回数５５
035090                 MOVE 後療料５１ＲＷ       TO 連料－後療料５５
035091             END-IF
035092             MOVE 冷罨法回数５１ＲＷ   TO 連料－冷罨法回数５５
035093             MOVE 冷罨法料５１ＲＷ     TO 連料－冷罨法料５５
035094             MOVE 温罨法回数５１ＲＷ   TO 連料－温罨法回数５５
035095             MOVE 温罨法料５１ＲＷ     TO 連料－温罨法料５５
035096             MOVE 電療回数５１ＲＷ     TO 連料－電療回数５５
035097             MOVE 電療料５１ＲＷ       TO 連料－電療料５５
035098*     上記１００％の金額を加算する
035099             MOVE 部位合計Ｗ５１       TO 連料－小計５５
035100             MOVE 部位合計多部位Ｗ５１ TO 連料－多部位込小計５５
035101             IF 長期区分Ｗ(5) > ZERO
035102                 MOVE 長期逓減率ＫＷ   TO 連料－長期逓減率５５
035103             END-IF
035104             MOVE 部位合計長期Ｗ５１   TO 連料－長期込小計５５
035105         END-IF
035106     END-IF.
035107*
035320****************
035330* ５部位／８割 *
035340****************
035350     IF 部位合計Ｗ５２ > ZERO
035360         MOVE 逓減開始月５２ＲＷ TO 連料－逓減開始月５８
035370         MOVE 逓減開始日５２ＲＷ TO 連料－逓減開始日５８
035380         IF 後療回数５２ＲＷ > ZERO
035390*             MOVE 後療単価ＯＷ(3, 5) TO 連料－後療単価５８
035400             MOVE 後療単価ＯＷ(5)    TO 連料－後療単価５８
035410             MOVE 後療回数５２ＲＷ   TO 連料－後療回数５８
035420             MOVE 後療料５２ＲＷ     TO 連料－後療料５８
035430         END-IF
035440         MOVE 冷罨法回数５２ＲＷ   TO 連料－冷罨法回数５８
035450         MOVE 冷罨法料５２ＲＷ     TO 連料－冷罨法料５８
035460         MOVE 温罨法回数５２ＲＷ   TO 連料－温罨法回数５８
035470         MOVE 温罨法料５２ＲＷ     TO 連料－温罨法料５８
035480         MOVE 電療回数５２ＲＷ     TO 連料－電療回数５８
035490         MOVE 電療料５２ＲＷ       TO 連料－電療料５８
035500* 上記１００％の金額を加算する
035510         MOVE 部位合計Ｗ５２       TO 連料－小計５８
035520         MOVE 部位合計多部位Ｗ５２   TO 連料－多部位込小計５８
035530         IF 長期区分Ｗ(5) > ZERO
035540             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率５８
035550         END-IF
035560         MOVE 部位合計長期Ｗ５２   TO 連料－長期込小計５８
035570     END-IF.
035580******************
035590* ５部位／１０割 *
035600******************
035610     IF 部位合計Ｗ５３ > ZERO
035620         MOVE 逓減開始月５３ＲＷ TO 連料－逓減開始月５０
035630         MOVE 逓減開始日５３ＲＷ TO 連料－逓減開始日５０
035640         IF 後療回数５３ＲＷ > ZERO
035650*             MOVE 後療単価ＯＷ(1, 5) TO 連料－後療単価５０
035660*             MOVE 後療単価ＯＷ(2, 5) TO 連料－後療単価５０
035670             MOVE 後療単価ＯＷ(5)      TO 連料－後療単価５０
035680             MOVE 後療回数５３ＲＷ     TO 連料－後療回数５０
035690             MOVE 後療料５３ＲＷ       TO 連料－後療料５０
035700         END-IF
035710         MOVE 冷罨法回数５３ＲＷ   TO 連料－冷罨法回数５０
035720         MOVE 冷罨法料５３ＲＷ     TO 連料－冷罨法料５０
035730         MOVE 温罨法回数５３ＲＷ   TO 連料－温罨法回数５０
035740         MOVE 温罨法料５３ＲＷ     TO 連料－温罨法料５０
035750         MOVE 電療回数５３ＲＷ     TO 連料－電療回数５０
035760         MOVE 電療料５３ＲＷ       TO 連料－電療料５０
035770* 上記１００％の金額を加算する
035780         MOVE 部位合計Ｗ５３       TO 連料－小計５０
035790         IF 長期区分Ｗ(5) > ZERO
035800             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率５０
035810         END-IF
035820         MOVE 部位合計長期Ｗ５３   TO 連料－長期込小計５０
035830     END-IF.
      */６部位/120804
035320****************
035330* ６部位／７割 *
035340****************
035350     IF 部位合計Ｗ６２ > ZERO
035360         MOVE 逓減開始月６２ＲＷ TO 連料－逓減開始月６８
035370         MOVE 逓減開始日６２ＲＷ TO 連料－逓減開始日６８
035380         IF 後療回数６２ＲＷ > ZERO
035390*             MOVE 後療単価ＯＷ(3, 5) TO 連料－後療単価６８
035400             MOVE 後療単価ＯＷ(6)    TO 連料－後療単価６８
035410             MOVE 後療回数６２ＲＷ   TO 連料－後療回数６８
035420             MOVE 後療料６２ＲＷ     TO 連料－後療料６８
035430         END-IF
035440         MOVE 冷罨法回数６２ＲＷ   TO 連料－冷罨法回数６８
035450         MOVE 冷罨法料６２ＲＷ     TO 連料－冷罨法料６８
035460         MOVE 温罨法回数６２ＲＷ   TO 連料－温罨法回数６８
035470         MOVE 温罨法料６２ＲＷ     TO 連料－温罨法料６８
035480         MOVE 電療回数６２ＲＷ     TO 連料－電療回数６８
035490         MOVE 電療料６２ＲＷ       TO 連料－電療料６８
035500* 上記１００％の金額を加算する
035510         MOVE 部位合計Ｗ６２       TO 連料－小計６８
035520         MOVE 部位合計多部位Ｗ６２   TO 連料－多部位込小計６８
035530         IF 長期区分Ｗ(6) > ZERO
035540             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率６８
035550         END-IF
035560         MOVE 部位合計長期Ｗ６２   TO 連料－長期込小計６８
035570     END-IF.
035580******************
035590* ６部位／１０割 *
035600******************
035610     IF 部位合計Ｗ６３ > ZERO
035620         MOVE 逓減開始月６３ＲＷ TO 連料－逓減開始月６０
035630         MOVE 逓減開始日６３ＲＷ TO 連料－逓減開始日６０
035640         IF 後療回数６３ＲＷ > ZERO
035650*             MOVE 後療単価ＯＷ(1, 5) TO 連料－後療単価６０
035660*             MOVE 後療単価ＯＷ(2, 5) TO 連料－後療単価６０
035670             MOVE 後療単価ＯＷ(6)      TO 連料－後療単価６０
035680             MOVE 後療回数６３ＲＷ     TO 連料－後療回数６０
035690             MOVE 後療料６３ＲＷ       TO 連料－後療料６０
035700         END-IF
035710         MOVE 冷罨法回数６３ＲＷ   TO 連料－冷罨法回数６０
035720         MOVE 冷罨法料６３ＲＷ     TO 連料－冷罨法料６０
035730         MOVE 温罨法回数６３ＲＷ   TO 連料－温罨法回数６０
035740         MOVE 温罨法料６３ＲＷ     TO 連料－温罨法料６０
035750         MOVE 電療回数６３ＲＷ     TO 連料－電療回数６０
035760         MOVE 電療料６３ＲＷ       TO 連料－電療料６０
035770* 上記１００％の金額を加算する
035780         MOVE 部位合計Ｗ６３       TO 連料－小計６０
035790         IF 長期区分Ｗ(6) > ZERO
035800             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率６０
035810         END-IF
035820         MOVE 部位合計長期Ｗ６３   TO 連料－長期込小計６０
035830     END-IF.
      */７部位/120804
035320****************
035330* ７部位／７割 *
035340****************
035350     IF 部位合計Ｗ７２ > ZERO
035360         MOVE 逓減開始月７２ＲＷ TO 連料－逓減開始月７８
035370         MOVE 逓減開始日７２ＲＷ TO 連料－逓減開始日７８
035380         IF 後療回数７２ＲＷ > ZERO
035390*             MOVE 後療単価ＯＷ(3, 5) TO 連料－後療単価７８
035400             MOVE 後療単価ＯＷ(7)    TO 連料－後療単価７８
035410             MOVE 後療回数７２ＲＷ   TO 連料－後療回数７８
035420             MOVE 後療料７２ＲＷ     TO 連料－後療料７８
035430         END-IF
035440         MOVE 冷罨法回数７２ＲＷ   TO 連料－冷罨法回数７８
035450         MOVE 冷罨法料７２ＲＷ     TO 連料－冷罨法料７８
035460         MOVE 温罨法回数７２ＲＷ   TO 連料－温罨法回数７８
035470         MOVE 温罨法料７２ＲＷ     TO 連料－温罨法料７８
035480         MOVE 電療回数７２ＲＷ     TO 連料－電療回数７８
035490         MOVE 電療料７２ＲＷ       TO 連料－電療料７８
035500* 上記１００％の金額を加算する
035510         MOVE 部位合計Ｗ７２       TO 連料－小計７８
035520         MOVE 部位合計多部位Ｗ７２   TO 連料－多部位込小計７８
035530         IF 長期区分Ｗ(7) > ZERO
035540             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率７８
035550         END-IF
035560         MOVE 部位合計長期Ｗ７２   TO 連料－長期込小計７８
035570     END-IF.
035580******************
035590* ７部位／１０割 *
035600******************
035610     IF 部位合計Ｗ７３ > ZERO
035620         MOVE 逓減開始月７３ＲＷ TO 連料－逓減開始月７０
035630         MOVE 逓減開始日７３ＲＷ TO 連料－逓減開始日７０
035640         IF 後療回数７３ＲＷ > ZERO
035650*             MOVE 後療単価ＯＷ(1, 5) TO 連料－後療単価７０
035660*             MOVE 後療単価ＯＷ(2, 5) TO 連料－後療単価７０
035670             MOVE 後療単価ＯＷ(7)      TO 連料－後療単価７０
035680             MOVE 後療回数７３ＲＷ     TO 連料－後療回数７０
035690             MOVE 後療料７３ＲＷ       TO 連料－後療料７０
035700         END-IF
035710         MOVE 冷罨法回数７３ＲＷ   TO 連料－冷罨法回数７０
035720         MOVE 冷罨法料７３ＲＷ     TO 連料－冷罨法料７０
035730         MOVE 温罨法回数７３ＲＷ   TO 連料－温罨法回数７０
035740         MOVE 温罨法料７３ＲＷ     TO 連料－温罨法料７０
035750         MOVE 電療回数７３ＲＷ     TO 連料－電療回数７０
035760         MOVE 電療料７３ＲＷ       TO 連料－電療料７０
035770* 上記１００％の金額を加算する
035780         MOVE 部位合計Ｗ７３       TO 連料－小計７０
035790         IF 長期区分Ｗ(7) > ZERO
035800             MOVE 長期逓減率ＫＷ     TO 連料－長期逓減率７０
035810         END-IF
035820         MOVE 部位合計長期Ｗ７３   TO 連料－長期込小計７０
035830     END-IF.
035840*
035850*================================================================*
035860 制御情報取得 SECTION.
035870*
      */明細書発行加算。過去年月のフラグを変更しない/20240725
035880     MOVE 01 TO 制０１－制御区分.
035890     READ 制御情報マスタ
035900     NOT INVALID KEY
035910         MOVE 制０１－明細変更和暦年月 TO 明細変更和暦年月Ｗ
035930     END-READ.
      *
      */入金額初期値 0:一部負担金 1:０円/140821
035880     MOVE 02 TO 制０２－制御区分.
035890     READ 制御情報マスタ
035900     NOT INVALID KEY
035910         MOVE 制０２－入金額初期値 TO 入金額初期値Ｗ
035930     END-READ.
      *
035880     MOVE ZERO TO 制－制御区分.
035890     READ 制御情報マスタ
035900     NOT INVALID KEY
035910         MOVE 制－最大登録部位数   TO 最大登録部位数Ｗ
035920         MOVE 制－助成負担計算区分 TO 助成負担計算区分Ｗ
015330         MOVE 制－協会コード       TO 協会コードＷ
035930     END-READ.
035940*
035950*================================================================*
035960 多部位合計処理 SECTION.
035970*
035980     COMPUTE 部位合計Ｗ１ = 後療料１ＲＷ + 冷罨法料１ＲＷ +
035990                            温罨法料１ＲＷ + 電療料１ＲＷ.
036000     COMPUTE 部位合計Ｗ２ = 後療料２ＲＷ + 冷罨法料２ＲＷ +
036010                            温罨法料２ＲＷ + 電療料２ＲＷ.
036020     COMPUTE 部位合計Ｗ３ = 後療料３ＲＷ + 冷罨法料３ＲＷ +
036030                            温罨法料３ＲＷ + 電療料３ＲＷ.
036040     COMPUTE 部位合計Ｗ３１ = 後療料３１ＲＷ + 冷罨法料３１ＲＷ +
036050                            温罨法料３１ＲＷ + 電療料３１ＲＷ.
036060     COMPUTE 部位合計Ｗ４ = 後療料４ＲＷ + 冷罨法料４ＲＷ +
036070                            温罨法料４ＲＷ + 電療料４ＲＷ.
036080     COMPUTE 部位合計Ｗ４１ = 後療料４１ＲＷ + 冷罨法料４１ＲＷ +
036090                            温罨法料４１ＲＷ + 電療料４１ＲＷ.
036100     COMPUTE 部位合計Ｗ４２ = 後療料４２ＲＷ + 冷罨法料４２ＲＷ +
036110                            温罨法料４２ＲＷ + 電療料４２ＲＷ.
036120     COMPUTE 部位合計Ｗ５ = 後療料５ＲＷ + 冷罨法料５ＲＷ +
036130                            温罨法料５ＲＷ + 電療料５ＲＷ.
036140     COMPUTE 部位合計Ｗ５１ = 後療料５１ＲＷ + 冷罨法料５１ＲＷ +
036150                            温罨法料５１ＲＷ + 電療料５１ＲＷ.
036160     COMPUTE 部位合計Ｗ５２ = 後療料５２ＲＷ + 冷罨法料５２ＲＷ +
036170                            温罨法料５２ＲＷ + 電療料５２ＲＷ.
036180     COMPUTE 部位合計Ｗ５３ = 後療料５３ＲＷ + 冷罨法料５３ＲＷ +
036190                            温罨法料５３ＲＷ + 電療料５３ＲＷ.
      */６部位/120804
036160     COMPUTE 部位合計Ｗ６２ = 後療料６２ＲＷ + 冷罨法料６２ＲＷ +
036170                            温罨法料６２ＲＷ + 電療料６２ＲＷ.
036180     COMPUTE 部位合計Ｗ６３ = 後療料６３ＲＷ + 冷罨法料６３ＲＷ +
036190                            温罨法料６３ＲＷ + 電療料６３ＲＷ.
      */７部位/120804
036160     COMPUTE 部位合計Ｗ７２ = 後療料７２ＲＷ + 冷罨法料７２ＲＷ +
036170                            温罨法料７２ＲＷ + 電療料７２ＲＷ.
036180     COMPUTE 部位合計Ｗ７３ = 後療料７３ＲＷ + 冷罨法料７３ＲＷ +
036190                            温罨法料７３ＲＷ + 電療料７３ＲＷ.
036200*
036210* 多部位逓減算定
036220* ３部位目逓減
036230     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
036240     COMPUTE 部位合計多部位Ｗ３ ROUNDED = 部位合計Ｗ３ * 逓減率中間結果Ｗ
036250*
036260* ４部位目逓減　５０％
036270     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
036280     COMPUTE 部位合計多部位Ｗ４ ROUNDED = 部位合計Ｗ４ * 逓減率中間結果Ｗ
036290*
036300* ４部位目逓減　８０％
036310     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
036320     COMPUTE 部位合計多部位Ｗ４１ ROUNDED = 部位合計Ｗ４１ * 逓減率中間結果Ｗ
036330*
036340* ５部位目逓減　２５％
036350     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(5) / 100
036360     COMPUTE 部位合計多部位Ｗ５ ROUNDED = 部位合計Ｗ５ * 逓減率中間結果Ｗ
036370*
036380* ５部位目逓減　５０％
036390     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
036400     COMPUTE 部位合計多部位Ｗ５１ ROUNDED = 部位合計Ｗ５１ * 逓減率中間結果Ｗ
036410*
036420* ５部位目逓減　８０％
036430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
036440     COMPUTE 部位合計多部位Ｗ５２ ROUNDED = 部位合計Ｗ５２ * 逓減率中間結果Ｗ
      */修正漏れ？/120706
036420* ５部位目逓減　１００％
036430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(2) / 100
036440     COMPUTE 部位合計多部位Ｗ５３ ROUNDED = 部位合計Ｗ５３ * 逓減率中間結果Ｗ
      */６部位/120804
036420* ６部位目逓減　８０％
036430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
036440     COMPUTE 部位合計多部位Ｗ６２ ROUNDED = 部位合計Ｗ６２ * 逓減率中間結果Ｗ
036420* ６部位目逓減　１００％
036430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(2) / 100
036440     COMPUTE 部位合計多部位Ｗ６３ ROUNDED = 部位合計Ｗ６３ * 逓減率中間結果Ｗ
      */７部位/120804
036420* ７部位目逓減　８０％
036430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
036440     COMPUTE 部位合計多部位Ｗ７２ ROUNDED = 部位合計Ｗ７２ * 逓減率中間結果Ｗ
036420* ７部位目逓減　１００％
036430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(2) / 100
036440     COMPUTE 部位合計多部位Ｗ７３ ROUNDED = 部位合計Ｗ７３ * 逓減率中間結果Ｗ
036450*
036460*
036470* 長期逓減含む部位毎の合計を計算
036480*
036490* １部位　１００％
036500*
036510     IF 長期区分Ｗ(1) = 1
036520         COMPUTE 部位合計長期Ｗ１ ROUNDED = 部位合計Ｗ１ * 長期逓減率中間ＫＷ
036530     ELSE IF 長期区分Ｗ(1) = ZERO
036540              MOVE 部位合計Ｗ１ TO 部位合計長期Ｗ１
036550          END-IF
036560     END-IF.
036570*
036580* ２部位　１００％
036590     IF 長期区分Ｗ(2) = 1
036600         COMPUTE 部位合計長期Ｗ２ ROUNDED = 部位合計Ｗ２ * 長期逓減率中間ＫＷ
036610     ELSE IF 長期区分Ｗ(2) = ZERO
036620              MOVE 部位合計Ｗ２ TO 部位合計長期Ｗ２
036630          END-IF
036640     END-IF.
036650*
036660* ３部位　１００％
036670     IF 長期区分Ｗ(3) = 1
036680         COMPUTE 部位合計長期Ｗ３１ ROUNDED = 部位合計Ｗ３１ * 長期逓減率中間ＫＷ
036690     ELSE IF 長期区分Ｗ(3) = ZERO
036700              MOVE 部位合計Ｗ３１ TO 部位合計長期Ｗ３１
036710     END-IF.
036720*
036730* ３部位　８０％
036740     IF 長期区分Ｗ(3) = 1
036750         COMPUTE 部位合計長期Ｗ３ ROUNDED = 部位合計多部位Ｗ３ * 長期逓減率中間ＫＷ
036760     ELSE IF 長期区分Ｗ(3) = ZERO
036770             MOVE 部位合計多部位Ｗ３ TO 部位合計長期Ｗ３
036780          END-IF
036790     END-IF.
036800*
036810* ４部位　１００％
036820     IF 長期区分Ｗ(4) = 1
036830         COMPUTE 部位合計長期Ｗ４２ ROUNDED = 部位合計Ｗ４２ * 長期逓減率中間ＫＷ
036840     ELSE IF 長期区分Ｗ(4) = ZERO
036850              MOVE 部位合計Ｗ４２ TO 部位合計長期Ｗ４２
036860          END-IF
036870     END-IF.
036880*
036890* ４部位　８０％
036900     IF 長期区分Ｗ(4) = 1
036910         COMPUTE 部位合計長期Ｗ４１ ROUNDED = 部位合計多部位Ｗ４１ * 長期逓減率中間ＫＷ
036920     ELSE IF 長期区分Ｗ(4) = ZERO
036930              MOVE 部位合計多部位Ｗ４１ TO 部位合計長期Ｗ４１
036940          END-IF
036950     END-IF
036960*
036970* ４部位　５０％
036980     IF 長期区分Ｗ(4) = 1
036990         COMPUTE 部位合計長期Ｗ４ ROUNDED = 部位合計多部位Ｗ４ * 長期逓減率中間ＫＷ
037000     ELSE IF 長期区分Ｗ(4) = ZERO
037010              MOVE 部位合計多部位Ｗ４ TO 部位合計長期Ｗ４
037020          END-IF
037030     END-IF.
037040*
037050* ５部位　１００％
037060*
037070     IF 長期区分Ｗ(5) = 1
037080         COMPUTE 部位合計長期Ｗ５３ ROUNDED = 部位合計多部位Ｗ５３ * 長期逓減率中間ＫＷ
037090     ELSE IF 長期区分Ｗ(5) = ZERO
037100              MOVE 部位合計Ｗ５３ TO 部位合計長期Ｗ５３
037110          END-IF
037120     END-IF.
037130*
037140* ５部位　８０％
037150     IF 長期区分Ｗ(5) = 1
037160         COMPUTE 部位合計長期Ｗ５２ ROUNDED = 部位合計多部位Ｗ５２ * 長期逓減率中間ＫＷ
037170     ELSE IF 長期区分Ｗ(5) = ZERO
037180              MOVE 部位合計多部位Ｗ５２ TO 部位合計長期Ｗ５２
037190          END-IF
037200     END-IF.
037210*
037220* ５部位　５０％
037230     IF 長期区分Ｗ(5) = 1
037240         COMPUTE 部位合計長期Ｗ５１ ROUNDED = 部位合計多部位Ｗ５１ * 長期逓減率中間ＫＷ
037250     ELSE IF 長期区分Ｗ(5) = ZERO
037260              MOVE 部位合計多部位Ｗ５１ TO 部位合計長期Ｗ５１
037270          END-IF
037280     END-IF.
037290*
037300* ５部位　２５％
037310     IF 長期区分Ｗ(5) = 1
037320         COMPUTE 部位合計長期Ｗ５ ROUNDED = 部位合計多部位Ｗ５ * 長期逓減率中間ＫＷ
037330     ELSE IF 長期区分Ｗ(5) = ZERO
037340              MOVE 部位合計多部位Ｗ５ TO 部位合計長期Ｗ５
037350          END-IF
037360     END-IF.
      */６部位/120804
037050* ６部位　１００％
037060*
037070     IF 長期区分Ｗ(6) = 1
037080         COMPUTE 部位合計長期Ｗ６３ ROUNDED = 部位合計多部位Ｗ６３ * 長期逓減率中間ＫＷ
037090     ELSE IF 長期区分Ｗ(6) = ZERO
037100              MOVE 部位合計Ｗ６３ TO 部位合計長期Ｗ６３
037110          END-IF
037120     END-IF.
037130*
037140* ６部位　７０％
037150     IF 長期区分Ｗ(6) = 1
037160         COMPUTE 部位合計長期Ｗ６２ ROUNDED = 部位合計多部位Ｗ６２ * 長期逓減率中間ＫＷ
037170     ELSE IF 長期区分Ｗ(6) = ZERO
037180              MOVE 部位合計多部位Ｗ６２ TO 部位合計長期Ｗ６２
037190          END-IF
037200     END-IF.
      */７部位/120804
037050* ７部位　１００％
037060*
037070     IF 長期区分Ｗ(7) = 1
037080         COMPUTE 部位合計長期Ｗ７３ ROUNDED = 部位合計多部位Ｗ７３ * 長期逓減率中間ＫＷ
037090     ELSE IF 長期区分Ｗ(7) = ZERO
037100              MOVE 部位合計Ｗ７３ TO 部位合計長期Ｗ７３
037110          END-IF
037120     END-IF.
037130*
037140* ７部位　７０％
037150     IF 長期区分Ｗ(7) = 1
037160         COMPUTE 部位合計長期Ｗ７２ ROUNDED = 部位合計多部位Ｗ７２ * 長期逓減率中間ＫＷ
037170     ELSE IF 長期区分Ｗ(7) = ZERO
037180              MOVE 部位合計多部位Ｗ７２ TO 部位合計長期Ｗ７２
037190          END-IF
037200     END-IF.
037370*



      */令和6年10月改定対応↓↓↓*/20240712
           IF 連計－施術和暦年月 >= 50610
036510         IF (長期区分Ｗ(1) = 1) AND (部位継続月数Ｗ(1) >= 6)
036520             COMPUTE 部位合計長期Ｗ１ ROUNDED = 部位合計Ｗ１ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(2) = 1) AND (部位継続月数Ｗ(2) >= 6)
036520             COMPUTE 部位合計長期Ｗ２ ROUNDED = 部位合計Ｗ２ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(3) = 1) AND (部位継続月数Ｗ(3) >= 6)
036520*             COMPUTE 部位合計長期Ｗ３ ROUNDED = 部位合計Ｗ３ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ３ ROUNDED = 部位合計多部位Ｗ３ *  長期頻回逓減率ＫＷ / 100
HILO  *         DISPLAY "210-4 " 部位合計長期Ｗ３ "'" 部位合計多部位Ｗ３  "'" 長期頻回逓減率ＫＷ
               END-IF
036510         IF (長期区分Ｗ(3) = 1) AND (部位継続月数Ｗ(3) >= 6)
036520             COMPUTE 部位合計長期Ｗ３１ ROUNDED = 部位合計Ｗ３１ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(4) = 1) AND (部位継続月数Ｗ(4) >= 6)
036520*             COMPUTE 部位合計長期Ｗ４ ROUNDED = 部位合計Ｗ４ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ４ ROUNDED = 部位合計多部位Ｗ４ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(4) = 1) AND (部位継続月数Ｗ(4) >= 6)
036520*             COMPUTE 部位合計長期Ｗ４１ ROUNDED = 部位合計Ｗ４１ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ４１ ROUNDED = 部位合計多部位Ｗ４１ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(4) = 1) AND (部位継続月数Ｗ(4) >= 6)
036520             COMPUTE 部位合計長期Ｗ４２ ROUNDED = 部位合計Ｗ４２ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(5) = 1) AND (部位継続月数Ｗ(5) >= 6)
036520*             COMPUTE 部位合計長期Ｗ５ ROUNDED = 部位合計Ｗ５ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ５ ROUNDED = 部位合計多部位Ｗ５ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(5) = 1) AND (部位継続月数Ｗ(5) >= 6)
036520             COMPUTE 部位合計長期Ｗ５１ ROUNDED = 部位合計Ｗ５１ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ５１ ROUNDED = 部位合計多部位Ｗ５１ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(5) = 1) AND (部位継続月数Ｗ(5) >= 6)
036520*             COMPUTE 部位合計長期Ｗ５２ ROUNDED = 部位合計Ｗ５２ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ５２ ROUNDED = 部位合計多部位Ｗ５２ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(5) = 1) AND (部位継続月数Ｗ(5) >= 6)
036520*             COMPUTE 部位合計長期Ｗ５３ ROUNDED = 部位合計Ｗ５３ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ５３ ROUNDED = 部位合計多部位Ｗ５３ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(6) = 1) AND (部位継続月数Ｗ(6) >= 6)
036520*             COMPUTE 部位合計長期Ｗ６２ ROUNDED = 部位合計Ｗ６２ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ６２ ROUNDED = 部位合計多部位Ｗ６２ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(6) = 1) AND (部位継続月数Ｗ(6) >= 6)
036520*             COMPUTE 部位合計長期Ｗ６３ ROUNDED = 部位合計Ｗ６３ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ６３ ROUNDED = 部位合計多部位Ｗ６３ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(7) = 1) AND (部位継続月数Ｗ(7) >= 6)
036520*             COMPUTE 部位合計長期Ｗ７２ ROUNDED = 部位合計Ｗ７２ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ７２ ROUNDED = 部位合計多部位Ｗ７２ *  長期頻回逓減率ＫＷ / 100
               END-IF
036510         IF (長期区分Ｗ(7) = 1) AND (部位継続月数Ｗ(7) >= 6)
036520*             COMPUTE 部位合計長期Ｗ７３ ROUNDED = 部位合計Ｗ７３ *  長期頻回逓減率ＫＷ / 100
036520             COMPUTE 部位合計長期Ｗ７３ ROUNDED = 部位合計多部位Ｗ７３ *  長期頻回逓減率ＫＷ / 100
               END-IF

           END-IF.
      */令和6年10月改定対応↑↑↑*/20240712





037380*================================================================*
037390* 計算ＰＧ取得 SECTION.
037400**
037410*************************
037420** 計算プログラムの取得 *
037430*************************
037440** 費用額計算するため、区分コードをセット
037450*     EVALUATE 保険種別Ｗ
037460** 健康保険
037470*     WHEN ZERO
037480** 　　　　／＊　生保単独　＊／
037490*         IF 助成種別Ｗ = 50 AND 公費種別Ｗ = ZERO
037500*             MOVE 01 TO 計Ｐ－区分コード
037510*         ELSE
037520**/エラー表示の修正
037530*             PERFORM コンソールエラー確認
037540*             DISPLAY "保険種別が不正です" 
037550*                     " 受診者№=" 連計－患者コード
037560*                     " 保険種別=" 連計－保険種別
037570*                     " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
037580*             PERFORM エラー終了
037590*         END-IF
037600** 健保・自費
037610*     WHEN 01 THRU 09
037620** 19981210
037630*     WHEN 90
037640*         MOVE 01 TO 計Ｐ－区分コード
037650** 労災保険
037660*     WHEN 70
037670*         MOVE 02 TO 計Ｐ－区分コード
037680** 自賠責保険
037690*     WHEN 80
037700*         MOVE 03 TO 計Ｐ－区分コード
037710*     WHEN OTHER
037720**/エラー表示の修正
037730*         PERFORM コンソールエラー確認
037740*         DISPLAY "保険種別が不正です" 
037750*                 " 受診者№=" 連計－患者コード
037760*                 " 保険種別=" 連計－保険種別
037770*                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
037780*         PERFORM エラー終了
037790*     END-EVALUATE.
037800**
037810*     MOVE 連計－施術和暦 TO 計Ｐ－開始和暦 施術和暦ＣＷ.
037820*     MOVE 連計－施術年   TO 計Ｐ－開始年   施術年ＣＷ.
037830*     MOVE 連計－施術月   TO 計Ｐ－開始月   施術月ＣＷ.
037840**
037850*     START 計算ＰＧマスタ KEY IS <= 計Ｐ－区分コード 計Ｐ－開始和暦年月 REVERSED
037860*     END-START.
037870**
037880*     IF 状態キー = "00"
037890*         READ 計算ＰＧマスタ NEXT
037900*         AT END
037910**/エラー表示の修正
037920*             PERFORM コンソールエラー確認
037930*             DISPLAY "施術日に合った計算ＰＧがありません"
037940*                     " 受診者№="   連計－患者コード
037950*                     " 施術年月日=" 連計－施術年
037960*                       連計－施術月 連計－施術日 UPON CONS
037970**             DISPLAY NC"施術日に合った計算ＰＧがありません" UPON MSGBOX
037980*             PERFORM ファイル閉鎖
037990*             EXIT PROGRAM
038000*         NOT AT END
038010*             IF ( 計Ｐ－開始和暦年月 <= 施術和暦年月ＣＷ ) AND
038020*                ( 計Ｐ－終了和暦年月 >= 施術和暦年月ＣＷ )
038030*                 MOVE 計Ｐ－費用額ＰＧ     TO 費用額ＰＧＷ     
038040*                 MOVE 計Ｐ－負担額ＰＧ     TO 負担額ＰＧＷ     
038050*                 MOVE 計Ｐ－負担額ＰＧ老人 TO 負担額ＰＧ老人Ｗ
038060*                 MOVE 計Ｐ－負担額ＰＧ助成 TO 負担額ＰＧ助成Ｗ 
038070**
038080*             ELSE
038090**/エラー表示の修正
038100*                 PERFORM コンソールエラー確認
038110*                 DISPLAY "施術日に合った計算ＰＧがありません"
038120*                         " 受診者№="   連計－患者コード
038130*                         " 施術年月日=" 連計－施術年
038140*                           連計－施術月 連計－施術日 UPON CONS
038150*                 PERFORM ファイル閉鎖
038160*                 EXIT PROGRAM
038170*             END-IF
038180*         END-READ
038190*     END-IF.
038200*================================================================*
038210 負担率ＯＣＲセット SECTION.
038220* 親保険の負担率をセット　全柔用
038230
038240     EVALUATE 連レ－保険種別
038250     WHEN 01 THRU 04
038260     WHEN 06 THRU 09
038270         MOVE 連Ｏ－負担率 TO 連料－負担率
038280     WHEN OTHER
038290         MOVE ZERO         TO 連料－負担率
038300     END-EVALUATE.
038310*
038320*================================================================*
038330 施術日セット SECTION.
038340*
038350* 助成負担ＰＧに施術日を連結する
038360     MOVE 施記－施術日 TO 連計－施術日.
038370*
038380*================================================================*
038390 初検請求区分取得 SECTION.
038400*
038410* 初検料・再検料を請求するかのフラグ取得
038420     MOVE 連計－施術和暦 TO 負－施術和暦. 
038430     MOVE 連計－施術年   TO 負－施術年.   
038440     MOVE 連計－施術月   TO 負－施術月.   
038450     MOVE 連計－患者番号 TO 負－患者番号. 
038460     MOVE 連計－枝番     TO 負－枝番.     
038470     READ 負傷データＦ
038480     INVALID KEY
038490         DISPLAY NC"該当月の情報がありません" UPON CONS
038500         DISPLAY NC"施術和暦" 連計－施術和暦  UPON CONS
038510         DISPLAY NC"施術年"   連計－施術年    UPON CONS 
038520         DISPLAY NC"施術月"   連計－施術月    UPON CONS 
038530         DISPLAY NC"患者番号" 連計－患者番号  UPON CONS
038540         DISPLAY NC"枝番"     連計－枝番      UPON CONS 
038550         MOVE NC"負傷" TO ファイル名Ｗ
038560         PERFORM エラー表示Ｒ
038570     NOT INVALID KEY
038580         MOVE 負－初検請求区分 TO 初検請求区分Ｗ
038590     END-READ.
038600*
038610*================================================================*
038620 長期逓減区分取得 SECTION.
038630*
038640     MOVE 連計－施術和暦 TO 負－施術和暦.
038650     MOVE 連計－施術年   TO 負－施術年.
038660     MOVE 連計－施術月   TO 負－施術月.
038670     MOVE 連計－患者番号 TO 負－患者番号.
038680     MOVE 連計－枝番     TO 負－枝番.
038690     READ 負傷データＦ
038700     INVALID KEY
038710*/エラー表示の修正
038720         PERFORM コンソールエラー確認
038730         DISPLAY "指定の受信者の負傷データがありません" 
038740                 " 受診者№="   連計－患者コード
038750                 " 施術年月日=" 連計－施術年 連計－施術月 UPON CONS
038760*-----------------------------------------*
038770         CALL "actcshm"  WITH C LINKAGE
038780*-----------------------------------------*
038790*         ACCEPT  キー入力 FROM CONS
038800         PERFORM ファイル閉鎖
038810         MOVE ZERO TO PROGRAM-STATUS
038820         EXIT PROGRAM
038830     NOT INVALID KEY
038840         MOVE 負－部位数 TO 部位数Ｗ
038850     END-READ.
038860*
038870     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
038880               UNTIL 部位カウンタ２ > 部位数Ｗ
038890         MOVE ZERO TO 長期区分Ｗ(部位カウンタ２)
038900     END-PERFORM.
038910*
038920     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
038930              UNTIL  部位カウンタ２ > 部位数Ｗ
038940*
038950* 負－終了和暦年月が未入力の場合は、ALL 9 をセットする
038960* ０１：捻挫　０２：打撲　０３：挫傷のみ長期逓減あり
038970* ０４：脱臼も追加0211
038980*         IF 負－負傷種別(部位カウンタ２) = 01 OR 02 OR 03
038990         IF 負－負傷種別(部位カウンタ２) = 01 OR 02 OR 03 OR 04
039000             PERFORM 開始終了日取得
039010             PERFORM 年月絶対値計算
039020             IF 長期フラグ NOT = SPACE 
039030                 MOVE 1 TO 長期区分Ｗ(部位カウンタ２)
039040             END-IF
039050         END-IF
039060     END-PERFORM.
039070*
039080*================================================================*
039090 後療多部位項目取得 SECTION.
039100*
039110* １００％の金額を部位毎累計する。
039120* 逓減率は、部位毎の合計金額に対し、乗じる。
039130*
039140     EVALUATE 部位カウンタ ALSO 部位カウンタ２
039150     WHEN 1 ALSO 1
039160         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料１ＰＷ
039170*
039180     WHEN 1 ALSO 2
039190     WHEN 2 ALSO 2
039200         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料２ＰＷ
039210*
039220     WHEN 1 ALSO 3
039230     WHEN 2 ALSO 3
039240         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料３１ＰＷ
039250     WHEN 3 ALSO 3
039260         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料３ＰＷ
039270*
039280     WHEN 1 ALSO 4
039290     WHEN 2 ALSO 4
039300         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４２ＰＷ
039310     WHEN 3 ALSO 4
039320         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４１ＰＷ
039330     WHEN 4 ALSO 4
039340         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４ＰＷ
039350*
039360     WHEN 1 ALSO 5
039370     WHEN 2 ALSO 5
039380         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５３ＰＷ
039390     WHEN 3 ALSO 5
039400         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５２ＰＷ
039410     WHEN 4 ALSO 5
039420         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５１ＰＷ
039430     WHEN 5 ALSO 5
039440         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５ＰＷ
      */６部位/120807
039360     WHEN 1 ALSO 6
039370     WHEN 2 ALSO 6
039380         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料６３ＰＷ
039390     WHEN 3 ALSO 6
039400         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料６２ＰＷ
      */７部位/120807
039360     WHEN 1 ALSO 7
039370     WHEN 2 ALSO 7
039380         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料７３ＰＷ
039390     WHEN 3 ALSO 7
039400         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料７２ＰＷ
039450     WHEN OTHER
039460         CONTINUE
039470     END-EVALUATE.
039480*
039490*================================================================*
039500 冷罨法多部位項目取得 SECTION.
039510*
039520     EVALUATE 部位カウンタ ALSO 部位カウンタ２
039530     WHEN 1 ALSO 1
039540         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料１ＰＷ
039550*
039560     WHEN 1 ALSO 2
039570     WHEN 2 ALSO 2
039580         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料２ＰＷ
039590*
039600     WHEN 1 ALSO 3
039610     WHEN 2 ALSO 3
039620         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料３１ＰＷ
039630     WHEN 3 ALSO 3
039640         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料３ＰＷ
039650*
039660     WHEN 1 ALSO 4
039670     WHEN 2 ALSO 4
039680         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４２ＰＷ
039690     WHEN 3 ALSO 4
039700         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４１ＰＷ
039710     WHEN 4 ALSO 4
039720         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４ＰＷ
039730*
039740     WHEN 1 ALSO 5
039750     WHEN 2 ALSO 5
039760         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５３ＰＷ
039770     WHEN 3 ALSO 5
039780         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５２ＰＷ
039790     WHEN 4 ALSO 5
039800         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５１ＰＷ
039810     WHEN 5 ALSO 5
039820         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５ＰＷ
      */６部位/120807
039740     WHEN 1 ALSO 6
039750     WHEN 2 ALSO 6
039760         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料６３ＰＷ
039770     WHEN 3 ALSO 6
039780         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料６２ＰＷ
      */７部位/120807
039740     WHEN 1 ALSO 7
039750     WHEN 2 ALSO 7
039760         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料７３ＰＷ
039770     WHEN 3 ALSO 7
039780         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料７２ＰＷ
039830     WHEN OTHER
039840         CONTINUE
039850     END-EVALUATE.
039860*
039870*================================================================*
039880 温罨法多部位項目取得 SECTION.
039890*
039900     EVALUATE 部位カウンタ ALSO 部位カウンタ２
039910     WHEN 1 ALSO 1
039920         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料１ＰＷ
039930*
039940     WHEN 1 ALSO 2
039950     WHEN 2 ALSO 2
039960         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料２ＰＷ
039970*
039980     WHEN 1 ALSO 3
039990     WHEN 2 ALSO 3
040000         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料３１ＰＷ
040010     WHEN 3 ALSO 3
040020         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料３ＰＷ
040030*
040040     WHEN 1 ALSO 4
040050     WHEN 2 ALSO 4
040060         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４２ＰＷ
040070     WHEN 3 ALSO 4
040080         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４１ＰＷ
040090     WHEN 4 ALSO 4
040100         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４ＰＷ
040110*
040120     WHEN 1 ALSO 5
040130     WHEN 2 ALSO 5
040140         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５３ＰＷ
040150     WHEN 3 ALSO 5
040160         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５２ＰＷ
040170     WHEN 4 ALSO 5
040180         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５１ＰＷ
040190     WHEN 5 ALSO 5
040200         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５ＰＷ
      */６部位/120807
040120     WHEN 1 ALSO 6
040130     WHEN 2 ALSO 6
040140         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料６３ＰＷ
040150     WHEN 3 ALSO 6
040160         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料６２ＰＷ
      */７部位/120807
040120     WHEN 1 ALSO 7
040130     WHEN 2 ALSO 7
040140         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料７３ＰＷ
040150     WHEN 3 ALSO 7
040160         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料７２ＰＷ
040210     WHEN OTHER
040220         CONTINUE
040230     END-EVALUATE.
040240*
040250*================================================================*
040260 電療多部位項目取得 SECTION.
040270*
040280     EVALUATE 部位カウンタ ALSO 部位カウンタ２
040290     WHEN 1 ALSO 1
040300         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料１ＰＷ
040310*
040320     WHEN 1 ALSO 2
040330     WHEN 2 ALSO 2
040340         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料２ＰＷ
040350*
040360     WHEN 1 ALSO 3
040370     WHEN 2 ALSO 3
040380         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料３１ＰＷ
040390     WHEN 3 ALSO 3
040400         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料３ＰＷ
040410*
040420     WHEN 1 ALSO 4
040430     WHEN 2 ALSO 4
040440         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４２ＰＷ
040450     WHEN 3 ALSO 4
040460         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４１ＰＷ
040470     WHEN 4 ALSO 4
040480         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４ＰＷ
040490*
040500     WHEN 1 ALSO 5
040510     WHEN 2 ALSO 5
040520         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５３ＰＷ
040530     WHEN 3 ALSO 5
040540         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５２ＰＷ
040550     WHEN 4 ALSO 5
040560         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５１ＰＷ
040570     WHEN 5 ALSO 5
040580         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５ＰＷ
      */６部位/120807
040500     WHEN 1 ALSO 6
040510     WHEN 2 ALSO 6
040520         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料６３ＰＷ
040530     WHEN 3 ALSO 6
040540         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料６２ＰＷ
      */７部位/120807
040500     WHEN 1 ALSO 7
040510     WHEN 2 ALSO 7
040520         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料７３ＰＷ
040530     WHEN 3 ALSO 7
040540         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料７２ＰＷ
040590     WHEN OTHER
040600         CONTINUE
040610     END-EVALUATE.
040620*
040630*================================================================*
040640 多部位日計処理 SECTION.
040650*
040660     COMPUTE 部位日計Ｗ１ = 後療料１ＰＷ + 冷罨法料１ＰＷ +
040670                            温罨法料１ＰＷ + 電療料１ＰＷ.
040680     COMPUTE 部位日計Ｗ２ = 後療料２ＰＷ + 冷罨法料２ＰＷ +
040690                            温罨法料２ＰＷ + 電療料２ＰＷ.
040700     COMPUTE 部位日計Ｗ３ = 後療料３ＰＷ + 冷罨法料３ＰＷ +
040710                            温罨法料３ＰＷ + 電療料３ＰＷ.
040720     COMPUTE 部位日計Ｗ３１ = 後療料３１ＰＷ + 冷罨法料３１ＰＷ +
040730                            温罨法料３１ＰＷ + 電療料３１ＰＷ.
040740     COMPUTE 部位日計Ｗ４ = 後療料４ＰＷ + 冷罨法料４ＰＷ +
040750                            温罨法料４ＰＷ + 電療料４ＰＷ.
040760     COMPUTE 部位日計Ｗ４１ = 後療料４１ＰＷ + 冷罨法料４１ＰＷ +
040770                            温罨法料４１ＰＷ + 電療料４１ＰＷ.
040780     COMPUTE 部位日計Ｗ４２ = 後療料４２ＰＷ + 冷罨法料４２ＰＷ +
040790                            温罨法料４２ＰＷ + 電療料４２ＰＷ.
040800     COMPUTE 部位日計Ｗ５ = 後療料５ＰＷ + 冷罨法料５ＰＷ +
040810                            温罨法料５ＰＷ + 電療料５ＰＷ.
040820     COMPUTE 部位日計Ｗ５１ = 後療料５１ＰＷ + 冷罨法料５１ＰＷ +
040830                            温罨法料５１ＰＷ + 電療料５１ＰＷ.
040840     COMPUTE 部位日計Ｗ５２ = 後療料５２ＰＷ + 冷罨法料５２ＰＷ +
040850                            温罨法料５２ＰＷ + 電療料５２ＰＷ.
040860     COMPUTE 部位日計Ｗ５３ = 後療料５３ＰＷ + 冷罨法料５３ＰＷ +
040870                            温罨法料５３ＰＷ + 電療料５３ＰＷ.
      */６部位/120807
040840     COMPUTE 部位日計Ｗ６２ = 後療料６２ＰＷ + 冷罨法料６２ＰＷ +
040850                            温罨法料６２ＰＷ + 電療料６２ＰＷ.
040860     COMPUTE 部位日計Ｗ６３ = 後療料６３ＰＷ + 冷罨法料６３ＰＷ +
040870                            温罨法料６３ＰＷ + 電療料６３ＰＷ.
      */７部位/120807
040840     COMPUTE 部位日計Ｗ７２ = 後療料７２ＰＷ + 冷罨法料７２ＰＷ +
040850                            温罨法料７２ＰＷ + 電療料７２ＰＷ.
040860     COMPUTE 部位日計Ｗ７３ = 後療料７３ＰＷ + 冷罨法料７３ＰＷ +
040870                            温罨法料７３ＰＷ + 電療料７３ＰＷ.
040880*
040890* 多部位逓減算定
040900* ３部位目逓減
040910     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
040920     COMPUTE 部位日計多部位Ｗ３ ROUNDED = 部位日計Ｗ３ * 逓減率中間結果Ｗ
040930*
040940* ４部位目逓減　５０％
040950     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
040960     COMPUTE 部位日計多部位Ｗ４ ROUNDED = 部位日計Ｗ４ * 逓減率中間結果Ｗ
040970*
040980* ４部位目逓減　８０％
040990     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
041000     COMPUTE 部位日計多部位Ｗ４１ ROUNDED = 部位日計Ｗ４１ * 逓減率中間結果Ｗ
041010*
041020* ５部位目逓減　２５％
041030     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(5) / 100
041040     COMPUTE 部位日計多部位Ｗ５ ROUNDED = 部位日計Ｗ５ * 逓減率中間結果Ｗ
041050*
041060* ５部位目逓減　５０％
041070     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
041080     COMPUTE 部位日計多部位Ｗ５１ ROUNDED = 部位日計Ｗ５１ * 逓減率中間結果Ｗ
041090*
041100* ５部位目逓減　８０％
041110     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
041120     COMPUTE 部位日計多部位Ｗ５２ ROUNDED = 部位日計Ｗ５２ * 逓減率中間結果Ｗ
      */修正漏れ？/120706
041100* ５部位目逓減　１００％
041110     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(2) / 100
041120     COMPUTE 部位日計多部位Ｗ５３ ROUNDED = 部位日計Ｗ５３ * 逓減率中間結果Ｗ
      */６部位/120807
041100* ６部位目逓減　７０％
041110     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
041120     COMPUTE 部位日計多部位Ｗ６２ ROUNDED = 部位日計Ｗ６２ * 逓減率中間結果Ｗ
041100* ６部位目逓減　１００％
041110     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(2) / 100
041120     COMPUTE 部位日計多部位Ｗ６３ ROUNDED = 部位日計Ｗ６３ * 逓減率中間結果Ｗ
      */７部位/120807
041100* ７部位目逓減　７０％
041110     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
041120     COMPUTE 部位日計多部位Ｗ７２ ROUNDED = 部位日計Ｗ７２ * 逓減率中間結果Ｗ
041100* ７部位目逓減　１００％
041110     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(2) / 100
041120     COMPUTE 部位日計多部位Ｗ７３ ROUNDED = 部位日計Ｗ７３ * 逓減率中間結果Ｗ
041130*
041140*
041150* 長期逓減含む部位毎の日計を計算
041160*
041170* １部位　１００％
041180* DEBUG
041190*     DISPLAY NC"長期区分Ｗ１部位目" 長期区分Ｗ(1, 1) UPON MSGBOX.
041200*
041210     IF 長期区分Ｗ(1) = 1
041220         COMPUTE 部位日計長期Ｗ１ ROUNDED = 部位日計Ｗ１ * 長期逓減率中間ＫＷ
041230     ELSE IF 長期区分Ｗ(1) = ZERO
041240              MOVE 部位日計Ｗ１ TO 部位日計長期Ｗ１
041250          END-IF
041260     END-IF.
041270*
041280* ２部位　１００％
041290     IF 長期区分Ｗ(2) = 1
041300         COMPUTE 部位日計長期Ｗ２ ROUNDED = 部位日計Ｗ２ * 長期逓減率中間ＫＷ
041310     ELSE IF 長期区分Ｗ(2) = ZERO
041320              MOVE 部位日計Ｗ２ TO 部位日計長期Ｗ２
041330          END-IF
041340     END-IF.
041350*
041360* ３部位　１００％
041370     IF 長期区分Ｗ(3) = 1
041380         COMPUTE 部位日計長期Ｗ３１ ROUNDED = 部位日計Ｗ３１ * 長期逓減率中間ＫＷ
041390     ELSE IF 長期区分Ｗ(3) = ZERO
041400              MOVE 部位日計Ｗ３１ TO 部位日計長期Ｗ３１
041410     END-IF.
041420*
041430* ３部位　８０％
041440     IF 長期区分Ｗ(3) = 1
041450         COMPUTE 部位日計長期Ｗ３ ROUNDED = 部位日計多部位Ｗ３ * 長期逓減率中間ＫＷ
041460     ELSE IF 長期区分Ｗ(3) = ZERO
041470             MOVE 部位日計多部位Ｗ３ TO 部位日計長期Ｗ３
041480          END-IF
041490     END-IF.
041500*
041510* ４部位　１００％
041520     IF 長期区分Ｗ(4) = 1
041530         COMPUTE 部位日計長期Ｗ４２ ROUNDED = 部位日計Ｗ４２ * 長期逓減率中間ＫＷ
041540     ELSE IF 長期区分Ｗ(4) = ZERO
041550              MOVE 部位日計Ｗ４２ TO 部位日計長期Ｗ４２
041560          END-IF
041570     END-IF.
041580*
041590* ４部位　８０％
041600     IF 長期区分Ｗ(4) = 1
041610         COMPUTE 部位日計長期Ｗ４１ ROUNDED = 部位日計多部位Ｗ４１ * 長期逓減率中間ＫＷ
041620     ELSE IF 長期区分Ｗ(4) = ZERO
041630              MOVE 部位日計多部位Ｗ４１ TO 部位日計長期Ｗ４１
041640          END-IF
041650     END-IF
041660*
041670* ４部位　５０％
041680     IF 長期区分Ｗ(4) = 1
041690         COMPUTE 部位日計長期Ｗ４ ROUNDED = 部位日計多部位Ｗ４ * 長期逓減率中間ＫＷ
041700     ELSE IF 長期区分Ｗ(4) = ZERO
041710              MOVE 部位日計多部位Ｗ４ TO 部位日計長期Ｗ４
041720          END-IF
041730     END-IF.
041740*
041750* ５部位　１００％
041760*
041770     IF 長期区分Ｗ(5) = 1
041780         COMPUTE 部位日計長期Ｗ５３ ROUNDED = 部位日計多部位Ｗ５３ * 長期逓減率中間ＫＷ
041790     ELSE IF 長期区分Ｗ(5) = ZERO
041800              MOVE 部位日計Ｗ５３ TO 部位日計長期Ｗ５３
041810          END-IF
041820     END-IF.
041830*
041840* ５部位　８０％
041850     IF 長期区分Ｗ(5) = 1
041860         COMPUTE 部位日計長期Ｗ５２ ROUNDED = 部位日計多部位Ｗ５２ * 長期逓減率中間ＫＷ
041870     ELSE IF 長期区分Ｗ(5) = ZERO
041880              MOVE 部位日計多部位Ｗ５２ TO 部位日計長期Ｗ５２
041890          END-IF
041900     END-IF.
041910*
041920* ５部位　５０％
041930     IF 長期区分Ｗ(5) = 1
041940         COMPUTE 部位日計長期Ｗ５１ ROUNDED = 部位日計多部位Ｗ５１ * 長期逓減率中間ＫＷ
041950     ELSE IF 長期区分Ｗ(5) = ZERO
041960              MOVE 部位日計多部位Ｗ５１ TO 部位日計長期Ｗ５１
041970          END-IF
041980     END-IF.
041990*
042000* ５部位　２５％
042010     IF 長期区分Ｗ(5) = 1
042020         COMPUTE 部位日計長期Ｗ５ ROUNDED = 部位日計多部位Ｗ５ * 長期逓減率中間ＫＷ
042030     ELSE IF 長期区分Ｗ(5) = ZERO
042040              MOVE 部位日計多部位Ｗ５ TO 部位日計長期Ｗ５
042050          END-IF
042060     END-IF.
      */６部位/120807
041750* ６部位　１００％
041760*
041770     IF 長期区分Ｗ(6) = 1
041780         COMPUTE 部位日計長期Ｗ６３ ROUNDED = 部位日計多部位Ｗ６３ * 長期逓減率中間ＫＷ
041790     ELSE IF 長期区分Ｗ(6) = ZERO
041800              MOVE 部位日計Ｗ６３ TO 部位日計長期Ｗ６３
041810          END-IF
041820     END-IF.
041830*
041840* ６部位　７０％
041850     IF 長期区分Ｗ(6) = 1
041860         COMPUTE 部位日計長期Ｗ６２ ROUNDED = 部位日計多部位Ｗ６２ * 長期逓減率中間ＫＷ
041870     ELSE IF 長期区分Ｗ(6) = ZERO
041880              MOVE 部位日計多部位Ｗ６２ TO 部位日計長期Ｗ６２
041890          END-IF
041900     END-IF.
041910*
      */７部位/120807
041750* ７部位　１００％
041760*
041770     IF 長期区分Ｗ(6) = 1
041780         COMPUTE 部位日計長期Ｗ７３ ROUNDED = 部位日計多部位Ｗ７３ * 長期逓減率中間ＫＷ
041790     ELSE IF 長期区分Ｗ(6) = ZERO
041800              MOVE 部位日計Ｗ７３ TO 部位日計長期Ｗ７３
041810          END-IF
041820     END-IF.
041830*
041840* ７部位　７０％
041850     IF 長期区分Ｗ(6) = 1
041860         COMPUTE 部位日計長期Ｗ７２ ROUNDED = 部位日計多部位Ｗ７２ * 長期逓減率中間ＫＷ
041870     ELSE IF 長期区分Ｗ(6) = ZERO
041880              MOVE 部位日計多部位Ｗ７２ TO 部位日計長期Ｗ７２
041890          END-IF
041900     END-IF.
041910*
042070*
042080*================================================================*
042090 日計処理 SECTION.
042100*
042110     PERFORM 多部位日計処理.
042120     COMPUTE 連計－費用額 = 部位日計長期Ｗ１ + 部位日計長期Ｗ２ +
042130                            部位日計長期Ｗ３ + 部位日計長期Ｗ３１ +
042140                            部位日計長期Ｗ４ + 部位日計長期Ｗ４１ +
042150                            部位日計長期Ｗ４２ + 部位日計長期Ｗ５ + 
042160                            部位日計長期Ｗ５１ + 部位日計長期Ｗ５２ + 
042170                            部位日計長期Ｗ５３ + 初検料Ｗ + 初検加算料Ｗ +
042180                            再検料Ｗ + 往療料Ｗ + 往療加算料Ｗ +
042190                            金属副子料Ｗ + 初回処置料Ｗ + 施術情報提供料Ｗ +
042200                            初検時相談料Ｗ + 運動療法料Ｗ +
      */６部位～/120807
                                  部位日計長期Ｗ６２ + 部位日計長期Ｗ６３ +
                                  部位日計長期Ｗ７２ + 部位日計長期Ｗ７３ + 
                                  明細書発行加算料Ｗ.
042210*
042220*================================================================*
042230 日計項目初期化 SECTION.
042240*
042250     MOVE ZERO TO 後療料１ＰＷ 後療料２ＰＷ
042260                  後療料３ＰＷ 後療料３１ＰＷ
042270                  後療料４ＰＷ 後療料４１ＰＷ 後療料４２ＰＷ
042280                  後療料５ＰＷ 後療料５１ＰＷ 後療料５２ＰＷ 後療料５３ＰＷ
      */６部位～/120807
                        後療料６２ＰＷ 後療料６３ＰＷ 後療料７２ＰＷ 後療料７３ＰＷ.
042290     MOVE ZERO TO 冷罨法料１ＰＷ 冷罨法料２ＰＷ
042300                  冷罨法料３ＰＷ 冷罨法料３１ＰＷ
042310                  冷罨法料４ＰＷ 冷罨法料４１ＰＷ 冷罨法料４２ＰＷ
042320                  冷罨法料５ＰＷ 冷罨法料５１ＰＷ 冷罨法料５２ＰＷ 冷罨法料５３ＰＷ
      */６部位～/120807
                        冷罨法料６２ＰＷ 冷罨法料６３ＰＷ 冷罨法料７２ＰＷ 冷罨法料７３ＰＷ.
042330     MOVE ZERO TO 温罨法料１ＰＷ 温罨法料２ＰＷ
042340                  温罨法料３ＰＷ 温罨法料３１ＰＷ
042350                  温罨法料４ＰＷ 温罨法料４１ＰＷ 温罨法料４２ＰＷ
042360                  温罨法料５ＰＷ 温罨法料５１ＰＷ 温罨法料５２ＰＷ 温罨法料５３ＰＷ
      */６部位～/120807
                        温罨法料６２ＰＷ 温罨法料６３ＰＷ 温罨法料７２ＰＷ 温罨法料７３ＰＷ.
042370     MOVE ZERO TO 電療料１ＰＷ 電療料２ＰＷ
042380                  電療料３ＰＷ 電療料３１ＰＷ
042390                  電療料４ＰＷ 電療料４１ＰＷ 電療料４２ＰＷ
042400                  電療料５ＰＷ 電療料５１ＰＷ 電療料５２ＰＷ 電療料５３ＰＷ
      */６部位～/120807
                        電療料６２ＰＷ 電療料６３ＰＷ 電療料７２ＰＷ 電療料７３ＰＷ.
042410*
042420*================================================================*
042430 逓減開始日取得 SECTION.
042440*
042450* 継続部位数Ｗのセット
042460     PERFORM 継続部位数セット.
042470*
042480* 継続の部位のみ算定する
042490* 継続の部位のみ部位カウンタをカウントＵＰする
042500     MOVE 1 TO 部位カウンタ
042510*
042520* 部位カウンタは、継続部位数との比較のためと
042530* 逓減率の配列位置取得のための変数
042540     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
042550             UNTIL ( 部位カウンタ > 継続部位数Ｗ   ) OR
042560                   ( 部位カウンタ > 後療可能算定数ＫＷ ) OR 
042570                   ( 部位カウンタ２ > 部位数Ｗ )
042580*
042590* 負－終了和暦年月が未入力の場合は、ALL 9 をセットする
042600         PERFORM 開始終了日取得
042610*
042620         IF ( 開始和暦年月日Ｗ <= 施記－施術和暦年月日 ) AND
042630            ( 終了和暦年月日Ｗ >= 施記－施術和暦年月日 )
042640*
042650             EVALUATE 部位カウンタ ALSO 部位カウンタ２
042660*
042670             WHEN 1 ALSO 3
042680             WHEN 2 ALSO 3
042690                 IF (逓減開始和暦３１ＲＷ = ZERO ) AND
042700                    (逓減開始年３１ＲＷ   = ZERO ) AND
042710                    (逓減開始月３１ＲＷ   = ZERO ) AND
042720                    (逓減開始日３１ＲＷ   = ZERO )
042730                     MOVE 施記－施術和暦 TO 逓減開始和暦３１ＲＷ
042740                     MOVE 施記－施術年   TO 逓減開始年３１ＲＷ
042750                     MOVE 施記－施術月   TO 逓減開始月３１ＲＷ
042760                     MOVE 施記－施術日   TO 逓減開始日３１ＲＷ
042770                 END-IF
042780*
042790             WHEN 1 ALSO 4
042800             WHEN 2 ALSO 4
042810                 IF (逓減開始和暦４２ＲＷ = ZERO ) AND
042820                    (逓減開始年４２ＲＷ   = ZERO ) AND
042830                    (逓減開始月４２ＲＷ   = ZERO ) AND
042840                    (逓減開始日４２ＲＷ   = ZERO )
042850                     MOVE 施記－施術和暦 TO 逓減開始和暦４２ＲＷ
042860                     MOVE 施記－施術年   TO 逓減開始年４２ＲＷ
042870                     MOVE 施記－施術月   TO 逓減開始月４２ＲＷ
042880                     MOVE 施記－施術日   TO 逓減開始日４２ＲＷ
042890                 END-IF
042900             WHEN 3 ALSO 4
042910                 IF (逓減開始和暦４１ＲＷ = ZERO ) AND
042920                    (逓減開始年４１ＲＷ   = ZERO ) AND
042930                    (逓減開始月４１ＲＷ   = ZERO ) AND
042940                    (逓減開始日４１ＲＷ   = ZERO )
042950                     MOVE 施記－施術和暦 TO 逓減開始和暦４１ＲＷ
042960                     MOVE 施記－施術年   TO 逓減開始年４１ＲＷ
042970                     MOVE 施記－施術月   TO 逓減開始月４１ＲＷ
042980                     MOVE 施記－施術日   TO 逓減開始日４１ＲＷ
042990                 END-IF
043000*
043010             WHEN 1 ALSO 5
043020             WHEN 2 ALSO 5
043030                 IF (逓減開始和暦５３ＲＷ = ZERO ) AND
043040                    (逓減開始年５３ＲＷ   = ZERO ) AND
043050                    (逓減開始月５３ＲＷ   = ZERO ) AND
043060                    (逓減開始日５３ＲＷ   = ZERO )
043070                     MOVE 施記－施術和暦 TO 逓減開始和暦５３ＲＷ
043080                     MOVE 施記－施術年   TO 逓減開始年５３ＲＷ
043090                     MOVE 施記－施術月   TO 逓減開始月５３ＲＷ
043100                     MOVE 施記－施術日   TO 逓減開始日５３ＲＷ
043110                 END-IF
043120             WHEN 3 ALSO 5
043130                 IF (逓減開始和暦５２ＲＷ = ZERO ) AND
043140                    (逓減開始年５２ＲＷ   = ZERO ) AND
043150                    (逓減開始月５２ＲＷ   = ZERO ) AND
043160                    (逓減開始日５２ＲＷ   = ZERO )
043170                     MOVE 施記－施術和暦 TO 逓減開始和暦５２ＲＷ
043180                     MOVE 施記－施術年   TO 逓減開始年５２ＲＷ
043190                     MOVE 施記－施術月   TO 逓減開始月５２ＲＷ
043200                     MOVE 施記－施術日   TO 逓減開始日５２ＲＷ
043210                 END-IF
043220             WHEN 4 ALSO 5
043230                 IF (逓減開始和暦５１ＲＷ = ZERO ) AND
043240                    (逓減開始年５１ＲＷ   = ZERO ) AND
043250                    (逓減開始月５１ＲＷ   = ZERO ) AND
043260                    (逓減開始日５１ＲＷ   = ZERO )
043270                     MOVE 施記－施術和暦 TO 逓減開始和暦５１ＲＷ
043280                     MOVE 施記－施術年   TO 逓減開始年５１ＲＷ
043290                     MOVE 施記－施術月   TO 逓減開始月５１ＲＷ
043300                     MOVE 施記－施術日   TO 逓減開始日５１ＲＷ
043310                 END-IF
      */６部位/120804
043010             WHEN 1 ALSO 6
043020             WHEN 2 ALSO 6
043030                 IF (逓減開始和暦６３ＲＷ = ZERO ) AND
043040                    (逓減開始年６３ＲＷ   = ZERO ) AND
043050                    (逓減開始月６３ＲＷ   = ZERO ) AND
043060                    (逓減開始日６３ＲＷ   = ZERO )
043070                     MOVE 施記－施術和暦 TO 逓減開始和暦６３ＲＷ
043080                     MOVE 施記－施術年   TO 逓減開始年６３ＲＷ
043090                     MOVE 施記－施術月   TO 逓減開始月６３ＲＷ
043100                     MOVE 施記－施術日   TO 逓減開始日６３ＲＷ
043110                 END-IF
043120             WHEN 3 ALSO 6
043130                 IF (逓減開始和暦６２ＲＷ = ZERO ) AND
043140                    (逓減開始年６２ＲＷ   = ZERO ) AND
043150                    (逓減開始月６２ＲＷ   = ZERO ) AND
043160                    (逓減開始日６２ＲＷ   = ZERO )
043170                     MOVE 施記－施術和暦 TO 逓減開始和暦６２ＲＷ
043180                     MOVE 施記－施術年   TO 逓減開始年６２ＲＷ
043190                     MOVE 施記－施術月   TO 逓減開始月６２ＲＷ
043200                     MOVE 施記－施術日   TO 逓減開始日６２ＲＷ
043210                 END-IF
      */７部位/120804
043010             WHEN 1 ALSO 7
043020             WHEN 2 ALSO 7
043030                 IF (逓減開始和暦７３ＲＷ = ZERO ) AND
043040                    (逓減開始年７３ＲＷ   = ZERO ) AND
043050                    (逓減開始月７３ＲＷ   = ZERO ) AND
043060                    (逓減開始日７３ＲＷ   = ZERO )
043070                     MOVE 施記－施術和暦 TO 逓減開始和暦７３ＲＷ
043080                     MOVE 施記－施術年   TO 逓減開始年７３ＲＷ
043090                     MOVE 施記－施術月   TO 逓減開始月７３ＲＷ
043100                     MOVE 施記－施術日   TO 逓減開始日７３ＲＷ
043110                 END-IF
043120             WHEN 3 ALSO 7
043130                 IF (逓減開始和暦７２ＲＷ = ZERO ) AND
043140                    (逓減開始年７２ＲＷ   = ZERO ) AND
043150                    (逓減開始月７２ＲＷ   = ZERO ) AND
043160                    (逓減開始日７２ＲＷ   = ZERO )
043170                     MOVE 施記－施術和暦 TO 逓減開始和暦７２ＲＷ
043180                     MOVE 施記－施術年   TO 逓減開始年７２ＲＷ
043190                     MOVE 施記－施術月   TO 逓減開始月７２ＲＷ
043200                     MOVE 施記－施術日   TO 逓減開始日７２ＲＷ
043210                 END-IF
043320             WHEN OTHER
043330                 CONTINUE
043340             END-EVALUATE
043350*
043360             COMPUTE 部位カウンタ = 部位カウンタ + 1
043370*
043380         END-IF
043390     END-PERFORM.
043400*
043410*================================================================*
043420 エラー終了 SECTION.
043430*
043440     PERFORM ファイル閉鎖.
043450     MOVE ZERO TO PROGRAM-STATUS.
043460     EXIT PROGRAM.
043470*================================================================*
043480 コンソールエラー確認 SECTION.
043490*
043500     IF エラー表示Ｆ = ZERO
043510         MOVE  "エラーが発生したのでコンソール画面の内容" TO 連メ７－メッセージ１
043520         MOVE  "を確認し、修正してください。"             TO 連メ７－メッセージ２
043530         CALL   "MSG007"
043540         CANCEL "MSG007"
043550     END-IF.
043560     MOVE 1 TO エラー表示Ｆ.
043570*================================================================*
043580* 受診者情報Ｆ読込 SECTION.
043590*
043600*     READ 受診者情報Ｆ NEXT
043610*     AT END
043620*         MOVE "YES" TO 終了フラグ３
043630*     END-READ.
043640*================================================================*
043650 老人負担区分取得 SECTION.
043660*
043670     MOVE 05             TO 老負－保険種別.
043680     MOVE 連計－施術和暦 TO 老負－開始和暦.
043690     MOVE 連計－施術年   TO 老負－開始年.
043700     MOVE 連計－施術月   TO 老負－開始月.
043710*
043720     START 老人負担区分マスタ KEY IS <= 老負－保険種別
043730                                        老負－開始和暦年月
043740                                        REVERSED
043750     END-START.
043760     IF 状態キー = "00"
043770         READ 老人負担区分マスタ NEXT
043780         AT END
043790             DISPLAY "施術年月に対応した老人負担区分がありません"
043800                     " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
043810             MOVE 1 TO ４１端数区分Ｗ
043820         NOT AT END
043830*/0210定率の上限廃止、定額制廃止によりコメント
043840*                   MOVE 老負－負担区分     TO 老人負担計算区分Ｗ
043850             MOVE 老負－４１端数区分 TO ４１端数区分Ｗ
043860         END-READ
043870     ELSE
043880         DISPLAY "施術年月に対応した老人負担区分がありません"
043890                 " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
043900         MOVE 1 TO ４１端数区分Ｗ
043910     END-IF.
043920*/0210老人改正の対応でレコードを読めなかった場合は４１端数区分１で計算するように変更。
043930*================================================================*
043940 連結項目初期化 SECTION.
043950*
043960     INITIALIZE   連老計算－キー.
043970     MOVE ZERO TO 連老計算－キー.
043980     INITIALIZE   連老負－キー.
043990     MOVE ZERO TO 連老負－キー.
044000*================================================================*
044010 受診者情報取得 SECTION.
044020*
044030     MOVE 連計－施術和暦     TO 受－施術和暦.
044040     MOVE 連計－施術年       TO 受－施術年  .
044050     MOVE 連計－施術月       TO 受－施術月  .
044060     MOVE 連計－患者番号     TO 受－患者番号.
044070     MOVE 連計－枝番         TO 受－枝番    .
044080     READ 受診者情報Ｆ
044090     INVALID KEY
044091         MOVE SPACE   TO 受－レコード
044092         INITIALIZE      受－レコード
044100         MOVE ZERO               TO 助成負担金免除Ｗ
044110     NOT INVALID KEY
044120         MOVE 受－助成負担金免除 TO 助成負担金免除Ｗ
044121*
044135     END-READ.
044136*
044140*================================================================*
044150 助成負担区分取得 SECTION.
044160*
044170* 初期化
044180     MOVE ZERO  TO 負担区分Ｗ.
044190     MOVE 連計－助成種別       TO 市－公費種別.
044200     MOVE 連計－市町村番号助成 TO 市－市町村番号.
044210     READ 市町村マスタ
044220     INVALID KEY
      */保険証画面からコンソールエラー表示を削除する↓↓↓/120928
044230*         PERFORM コンソールエラー確認
044240*         DISPLAY "市町村番号が登録されていません"
044250*                 " 受診者№="   連計－患者コード
044260*                 " 施術年月="   連計－施術年 連計－施術月
044270*                 " 助成種別="   連計－助成種別
044280*                 " 市町村番号=" 連計－市町村番号助成 UPON CONS
044290**-----------------------------------------*
044300*         CALL "actcshm"  WITH C LINKAGE
044310**-----------------------------------------*
               MOVE  "市町村番号が登録されていません" TO 連メ７－メッセージ１
               MOVE  SPACE                            TO 連メ７－メッセージ２
               CALL   "MSG007"
               CANCEL "MSG007"
      */保険証画面からコンソールエラー表示を削除する↑↑↑/120928
044320*         ACCEPT  キー入力 FROM CONS
044330         PERFORM ファイル閉鎖
044340         MOVE ZERO TO PROGRAM-STATUS
044350         EXIT PROGRAM
044360     NOT INVALID KEY
044370         MOVE 連計－助成種別       TO 保特－保険種別
044380         MOVE 連計－市町村番号助成 TO 保特－保険者番号
044390         MOVE 連計－施術和暦       TO 保特－開始和暦
044400         MOVE 連計－施術年         TO 保特－開始年
044410         MOVE 連計－施術月         TO 保特－開始月
044420         START 保険者特別負担マスタ KEY IS <= 保特－保険種別 
044430                                              保特－保険者番号
044440                                              保特－開始和暦年月
044450                                              REVERSED
044460         END-START
044470         IF 状態キー = "00" 
044480             READ 保険者特別負担マスタ NEXT
044490             NOT AT END
044500                 IF ( 保特－保険種別   = 連計－助成種別 ) AND
044510                    ( 保特－保険者番号 = 連計－市町村番号助成 )
044520                    MOVE 保特－負担区分       TO 負担区分Ｗ
044530*                    MOVE 保特－負担率１       TO 助成特別負担率１Ｗ
044540*                    MOVE 保特－負担率２       TO 助成特別負担率２Ｗ
044550*                    MOVE 保特－負担一定金額   TO 助成特別負担一定金額Ｗ
044560*                    MOVE 保特－事務手数料     TO 助成特別事務手数料Ｗ
044570*                    MOVE 保特－助成負担回数   TO 助成特別助成負担回数Ｗ
044580*                    MOVE 保特－助成負担回数２ TO 助成特別助成負担回数２Ｗ
      */負担区分２２(大阪府平成30年4月～)/20180307
010390                    MOVE 保特－負担一定金額 TO 負担一定金額Ｗ
018252                    EVALUATE 受－助成負担金免除
018253                    WHEN ZERO
018254                        MOVE 保特－限度額１ TO 計算限度額Ｗ
018255                    WHEN 1
018256                        MOVE ZERO           TO 計算限度額Ｗ
018257                    WHEN 8
018258                        MOVE 保特－限度額２ TO 計算限度額Ｗ
018259                    WHEN 9
018260                        MOVE 保特－限度額３ TO 計算限度額Ｗ
018261                    WHEN OTHER
018262                        MOVE 保特－限度額１ TO 計算限度額Ｗ
018263                    END-EVALUATE
      *
044590                 END-IF
044600             END-READ
044610         END-IF
044620     END-READ.
044630*================================================================*
044640 データチェック SECTION.
044650*
044660     MOVE SPACE  TO 実行キーＷ.
044670* *****************************************************************
044680* * 負傷部位有無チェック：部位数 = 0 の場合データ作成対象としない *
044690* *****************************************************************
044700     MOVE 連計－施術和暦   TO 負－施術和暦.
044710     MOVE 連計－施術年     TO 負－施術年.
044720     MOVE 連計－施術月     TO 負－施術月.
044730     MOVE 連計－患者番号   TO 負－患者番号.
044740     MOVE 連計－枝番       TO 負－枝番.
044750     READ 負傷データＦ
044760     INVALID KEY
044770         MOVE SPACE  TO 実行キーＷ
044780     NOT INVALID KEY
044790         IF 負－部位数 NOT = ZERO
044800*        *************************************************************
044810*        * 施術記録チェック：通院数 = 0 の場合データ作成対象としない *
044820*        *************************************************************
044830             MOVE 負－患者番号  TO 施記－患者番号
044840             MOVE 負－枝番      TO 施記－枝番
044850             MOVE 負－施術和暦  TO 施記－施術和暦
044860             MOVE 負－施術年    TO 施記－施術年
044870             MOVE 負－施術月    TO 施記－施術月
044880             MOVE ZERO          TO 施記－施術日
044890             START 施術記録Ｆ   KEY IS >= 施記－患者コード
044900                                          施記－施術和暦年月日
044910             END-START
044920             IF 状態キー = "00"
044930                 MOVE SPACE TO 終了フラグ
044940                 MOVE SPACE TO 施術記録有Ｗ
044950                 PERFORM 施術記録Ｆ読込
044960                 PERFORM UNTIL ( 終了フラグ           = "YES"          ) OR
044970                               ( 施記－患者コード NOT = 負－患者コード ) OR
044980                               ( 施記－施術和暦   NOT = 負－施術和暦   ) OR
044990                               ( 施記－施術年     NOT = 負－施術年     ) OR
045000                               ( 施記－施術月     NOT = 負－施術月     ) OR
045010                               ( 施術記録有Ｗ         = "YES"          )
045020                     MOVE "YES"  TO 施術記録有Ｗ
045030                     MOVE "YES"  TO 実行キーＷ
045040                 END-PERFORM
045050             ELSE
045060                 MOVE SPACE  TO 実行キーＷ
045070             END-IF
045080         ELSE
045090*             MOVE  NC"データがありません。" TO 連メ－メッセージ
045100*             CALL   "MSG001"
045110*             CANCEL "MSG001"
045120             MOVE SPACE  TO 実行キーＷ
045130         END-IF
045140     END-READ.
045150*
045160*================================================================*
045170 当月会計ファイル削除 SECTION.
045180*
      */入金額を退避してクリアした後に戻す/160625
           INITIALIZE 入金退避Ｗ.
045190     MOVE 連計－患者コード     TO 会－患者コード    .
045200     MOVE 連計－施術和暦       TO 会－施術和暦      .
045210     MOVE 連計－施術年         TO 会－施術年        .
045220     MOVE 連計－施術月         TO 会－施術月        .
045230     MOVE ZERO                 TO 会－施術日        .
045240     START 会計データＦ KEY IS >= 会－患者コード 会－施術和暦年月日
045250     END-START.
045260     IF 状態キー = "00"
045270         MOVE SPACE TO 終了フラグ
045280         PERFORM 会計データＦ読込
045290         PERFORM UNTIL (連計－患者コード NOT = 会－患者コード) OR
045300                       (連計－施術和暦   NOT = 会－施術和暦  ) OR
045310                       (連計－施術年     NOT = 会－施術年    ) OR
045320                       (連計－施術月     NOT = 会－施術月    ) OR
045330                       (終了フラグ       NOT = SPACE         )
      */施術者番号/160102
                   MOVE 会－施術者番号 TO  施術者番号Ｗ(会－施術日)
                   IF (会－費用額 > ZERO) OR (部位数Ｗ = ZERO)
                       MOVE 1 TO 繰越フラグ(会－施術日)
                   END-IF
      */入金額を退避する/160625
                   MOVE 会－入金額 TO 退避入金額Ｗ(会－施術日)
                   MOVE 会－自費額 TO 退避自費額Ｗ(会－施術日)
      *
045340             DELETE 会計データＦ
045350             INVALID KEY
045360                 MOVE NC"会計" TO ファイル名Ｗ
045370                 PERFORM エラー表示Ｄ
045380             END-DELETE
045390             PERFORM 会計データＦ読込
045400         END-PERFORM
045410     END-IF.
      */部位が無い場合だけ入金と自費額を設定する。部位がある場合は別の場所で設定している為↓↓↓/160625
           IF 部位数Ｗ = ZERO
046102         PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
                   IF 退避入金額Ｗ(カウンタ) NOT = ZERO
                       MOVE SPACE TO 会－レコード
                       INITIALIZE    会－レコード
045190                 MOVE 連計－患者コード       TO 会－患者コード    
045200                 MOVE 連計－施術和暦         TO 会－施術和暦      
045210                 MOVE 連計－施術年           TO 会－施術年        
045220                 MOVE 連計－施術月           TO 会－施術月        
045230                 MOVE カウンタ               TO 会－施術日        
      */             */退避していた値を戻す
                       MOVE 退避入金額Ｗ(カウンタ) TO 会－入金額
                       MOVE 退避自費額Ｗ(カウンタ) TO 会－自費額
      *
      */退避した施術者番号を戻す/20230227
                       MOVE 施術者番号Ｗ(カウンタ) TO  会－施術者番号
      *
048090                 WRITE 会－レコード
048100                 INVALID KEY
048110                     MOVE NC"会１" TO ファイル名Ｗ
048120                     PERFORM エラー表示
048130                     PERFORM 終了処理
048140                     EXIT PROGRAM
048150                 END-WRITE
                   END-IF
046104         END-PERFORM
           END-IF.
      */部位が無い場合だけ入金と自費額を設定する。部位がある場合は別の場所で設定している為↑↑↑/160625
045420*
045430*================================================================*
045440 会計データＦ読込 SECTION.
045450*
045460     READ 会計データＦ NEXT
045470     AT END
045480         MOVE SPACE TO 会－レコード
045490         MOVE "YES" TO 終了フラグ
045500     END-READ.
045510*================================================================*
045520 会計データＦ書込 SECTION.
045530*
      */複数施術者区分２の場合前回の施術者を繰り越す/151229
           IF 複数施術者使用区分Ｗ = 2
               PERFORM 前回施術者取得
           END-IF.
045540     MOVE SPACE TO 会－レコード.
045550     INITIALIZE    会－レコード.
045560     MOVE ZERO  TO 存在フラグ.
045570     MOVE 連計－患者コード     TO 会－患者コード.
045580     MOVE 連計－施術和暦       TO 会－施術和暦.
045590     MOVE 連計－施術年         TO 会－施術年.
045600     MOVE 連計－施術月         TO 会－施術月.
045610     MOVE 連計－施術日         TO 会－施術日.
045620*     MOVE 連料－負担率         TO 会－負担率.
045630*
045640     MOVE 初検料Ｗ             TO 会－初検料.
045650     MOVE 初検時間外ＮＷ       TO 会－時間外.
045660     MOVE 初検休日ＮＷ         TO 会－休日.
045670     MOVE 初検深夜ＮＷ         TO 会－深夜.
045680     MOVE 初検加算料Ｗ         TO 会－初検加算料.
045690     MOVE 再検料Ｗ             TO 会－再検料.
045700*
045710     MOVE 往療距離Ｗ           TO 会－往療距離.
045720     IF 往療距離Ｗ NOT = ZERO
045730         MOVE 1                TO 会－往療回数
045740     END-IF.
045750     MOVE 往療料Ｗ             TO 会－往療料.
045760     MOVE 往療加算料Ｗ         TO 会－往療加算料.
045770     MOVE 往療夜間ＮＷ         TO 会－夜間.
045780     MOVE 往療難路ＮＷ         TO 会－難路.
045790     MOVE 往療暴風雨ＮＷ       TO 会－暴風雨雪.
045800*     MOVE 往療時間外ＮＷ       TO 会－時間外.
045810     MOVE 金属副子料Ｗ         TO 会－金属副子加算料.
045820     MOVE 金属副子大ＮＷ       TO 会－大.
045830     MOVE 金属副子中ＮＷ       TO 会－中.
045840     MOVE 金属副子小ＮＷ       TO 会－小.
045850     MOVE 施術情報提供料Ｗ     TO 会－施術情報提供料.
      */運動後療料/20180507
           MOVE 運動療法料Ｗ         TO 会－運動後療料.
      */明細書発行加算料/20220704
           MOVE 明細書発行加算料Ｗ   TO 会－明細書発行加算料.
HILO  *     DISPLAY "210-1 " 会－施術和暦年月日 " " 会－運動後療料
045860     COMPUTE 会－小計 = 初検料Ｗ + 初検加算料Ｗ + 再検料Ｗ + 往療料Ｗ +
045870             往療加算料Ｗ + 金属副子料Ｗ + 施術情報提供料Ｗ + 初検時相談料Ｗ.
045880*
045890***
045891     MOVE 連計－費用額         TO 会－費用額.
045892*
045902*
045912*
045922* 10円単位の新規連結項目からセット
045923*    03  連計日計窓口－負担額           PIC 9(6).
045932*
045957*    / 助成は、負担額助成をセット /
045958*     IF 受－助成種別 NOT = ZERO
045959*        MOVE 連計－負担額助成     TO 会－一部負担金
045960*     ELSE
045961*        IF 受－公費種別 = 05
045962*            MOVE 連計－負担額老人 TO 会－一部負担金
045963*        ELSE
045964*            MOVE 連計－負担額     TO 会－一部負担金
045965*        END-IF
045967*     END-IF.
045968*
045969**
045970     MOVE 連計日計窓口－負担額    TO 会－一部負担金  会－例外一部負担金.
      */助成時の本体分の窓口金/120906
           MOVE 本体一部負担金Ｗ        TO 会－本体一部負担金.
045971**
045975*
045976*---------------------------------------------------------------------------------------*
045977* ※ 上記後、入金額は、会計領収Fにない時、一部負担金セット
045978     MOVE SPACE TO 会計領収無しフラグ.
045979     MOVE ZERO               TO 領－施術区分.
045980* 1:日、2:月、3:年
045981     MOVE 1                  TO 領－会計領収区分.
045982     MOVE 施記－施術和暦     TO 領－施術和暦.
045983     MOVE 施記－施術年       TO 領－施術年.
045984     MOVE 施記－施術月       TO 領－施術月.
045985     MOVE 施記－施術日       TO 領－施術日.
045986     MOVE 施記－患者番号     TO 領－患者番号.
045987     MOVE 施記－枝番         TO 領－枝番.
045988     READ 会計領収Ｆ
045989     INVALID KEY
045990        MOVE 会－一部負担金  TO 会－入金額
045991        MOVE ZERO            TO 会－自費額
045992        MOVE "YES" TO 会計領収無しフラグ
000530*/ 入金額初期値 0:一部負担金 1:０円/140821
      */入金処理をしていない場合はゼロにする/140821
000540        IF 入金額初期値Ｗ = 1
045990            MOVE ZERO TO 会－入金額
              END-IF
045993     NOT INVALID KEY
045994        MOVE 領－入金額      TO 会－入金額
045995        MOVE 領－自費額      TO 会－自費額
046002     END-READ.
046003*
046004*---------------------------------------------------------------------------------------*
046009*    / 1円単位で計算の通常項目 /
046010     IF 受－助成種別 NOT = ZERO
046011           MOVE 連計－負担額助成 TO 会－助成計算負担額  会－例外助成計算負担額
046012     ELSE
046013        IF 受－公費種別 = 05
046014            MOVE 連計－負担額老人 TO 会－助成計算負担額  会－例外助成計算負担額
046015        ELSE
046016            MOVE 連計－負担額     TO 会－助成計算負担額  会－例外助成計算負担額
046017        END-IF
046018     END-IF.
046020*---------------------------------------------------------------------------------------*
046021*
046024*---------------------------------------------------------------------------------------*
046040*---------------------------------------------------------------------------*
046041*** 上記後、償還払い・本体まとめの金額を算出し、セットし直す。
046042* Ｈ日－例外一部負担金とＨ日－例外助成計算負担額は、通常（償還でない）値がセットされている。
046043*
046044     IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 ) OR ( 本体まとめ区分Ｗ = 1 )
046045           IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
046046              IF 受－助成種別 = ZERO
046047*                /本体のみ償還/
046048                 IF 本体償還払い区分Ｗ = 1
046049                    MOVE 会－費用額 TO 会－一部負担金 会－助成計算負担額
046050                 END-IF
046051              ELSE
046052                 EVALUATE TRUE
046053*                /本体償還、助成通常/
046054                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
046055                    COMPUTE 会－一部負担金 =  ( 会－費用額 - 連計－負担額 ) + 連計－負担額助成
046056                    MOVE 会－一部負担金  TO 会－助成計算負担額
046057*                /本体通常、助成償還/
046058                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
046059                    MOVE 連計－負担額  TO 会－一部負担金 会－助成計算負担額
046060*                /本体償還、助成償還/
046061                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
046062                    MOVE 会－費用額  TO 会－一部負担金 会－助成計算負担額
046063                 END-EVALUATE
046064              END-IF
046065           ELSE
046066*             / 本体まとめのみ /
      */本体まとめ時一部負担金はクリアしない/140108
046067*              MOVE ZERO  TO 会－一部負担金 会－助成計算負担額
046067              MOVE ZERO  TO 会－助成計算負担額
046068           END-IF
046069**
046071* 　　　　 入金額は、会計領収Fにない時、一部負担金セット
046074           IF 会計領収無しフラグ = "YES"
046076              MOVE 会－一部負担金  TO 会－入金額
046077              MOVE ZERO            TO 会－自費額
046081           END-IF
046082*
046083     END-IF.
046084*---------------------------------------------------------------------------------------*
046086*    / 被災者 /
046087     IF 受－資格証明区分 = 9
046088        MOVE ZERO  TO 会－一部負担金 会－助成計算負担額
046089        IF 会計領収無しフラグ = "YES"
046090            MOVE ZERO  TO 会－入金額
046091            MOVE ZERO  TO 会－自費額
046092        END-IF
046093     END-IF.
046094*
046095*---------------------------------------------------------------------------------------*
046098**
046099**
046100*
046101*日毎レコードに入れる意味なし？
046102*     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 10
046103*        MOVE 連料－当日分負担額(カウンタ) TO 会－当日分負担額(カウンタ)
046104*     END-PERFORM.
046105*
046106* 負傷部位毎の料金
046107     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 7
046108         MOVE 初回処置料ＮＯＷ(カウンタ) TO 会－初回処置料(カウンタ)
046109         COMPUTE 会－初回処置料合計 = 会－初回処置料合計 + 初回処置料ＮＯＷ(カウンタ)
046110     END-PERFORM.
046111*
046112*/部位毎に逓減した金額を計算する
046120     PERFORM 逓減計算.
046130
046140*
046150* 逓減毎の料金
046160* １部位目
046170     MOVE 後療料１ＮＲＷ         TO 会－後療料１.
046180     MOVE 冷罨法料１ＮＲＷ       TO 会－冷罨法料１.
046190     MOVE 温罨法料１ＮＲＷ       TO 会－温罨法料１.
046200     MOVE 電療料１ＮＲＷ         TO 会－電療料１.
046210*     MOVE 連料－後療単価１       TO 会－後療単価１.
046220*     MOVE 連料－後療回数１       TO 会－後療回数１.
046230*     MOVE 連料－冷罨法回数１     TO 会－冷罨法回数１.
046240*     MOVE 連料－温罨法回数１     TO 会－温罨法回数１.
046250*     MOVE 連料－電療回数１       TO 会－電療回数１.
046260*     MOVE 連料－小計１           TO 会－小計１.
046270*     MOVE 連料－長期逓減率１     TO 会－長期逓減率１.
046280*     MOVE 連料－長期込小計１     TO 会－長期込小計１.
046290**********
046300* ２部位 *
046310**********
046320* ２部位目
046330     MOVE 後療料２ＮＲＷ         TO 会－後療料２.
046340     MOVE 冷罨法料２ＮＲＷ       TO 会－冷罨法料２.
046350     MOVE 温罨法料２ＮＲＷ       TO 会－温罨法料２.
046360     MOVE 電療料２ＮＲＷ         TO 会－電療料２.
046370*     MOVE 連料－後療単価２       TO 会－後療単価２.
046380*     MOVE 連料－後療回数２       TO 会－後療回数２.
046390*     MOVE 連料－冷罨法回数２     TO 会－冷罨法回数２.
046400*     MOVE 連料－温罨法回数２     TO 会－温罨法回数２.
046410*     MOVE 連料－電療回数２       TO 会－電療回数２.
046420*     MOVE 連料－小計２           TO 会－小計２.
046430*     MOVE 連料－長期逓減率２     TO 会－長期逓減率２.
046440*     MOVE 連料－長期込小計２     TO 会－長期込小計２.
046450******************                           
046460* ３部位／８割 *                             
046470******************                           
046480* ３部位目逓減
046490     MOVE 後療料３ＮＲＷ         TO 会－後療料３８.
046500     MOVE 冷罨法料３ＮＲＷ       TO 会－冷罨法料３８.
046510     MOVE 温罨法料３ＮＲＷ       TO 会－温罨法料３８.
046520     MOVE 電療料３ＮＲＷ         TO 会－電療料３８.
046530*     MOVE 連料－後療単価３８     TO 会－後療単価３８.
046540*     MOVE 連料－後療回数３８     TO 会－後療回数３８.
046550*     MOVE 連料－冷罨法回数３８   TO 会－冷罨法回数３８.
046560*     MOVE 連料－温罨法回数３８   TO 会－温罨法回数３８.
046570*     MOVE 連料－電療回数３８     TO 会－電療回数３８.
046580*     MOVE 連料－小計３８         TO 会－小計３８.
046590*     MOVE 連料－多部位込小計３８ TO 会－多部位込小計３８.
046600*     MOVE 連料－長期逓減率３８   TO 会－長期逓減率３８.
046610*     MOVE 連料－長期込小計３８   TO 会－長期込小計３８.
046620******************                           
046630* ３部位／１０割 *                           
046640******************                           
046650* ３部位目（繰上り）
046660     MOVE 後療料３１ＮＲＷ       TO 会－後療料３０.
046670     MOVE 冷罨法料３１ＮＲＷ     TO 会－冷罨法料３０.
046680     MOVE 温罨法料３１ＮＲＷ     TO 会－温罨法料３０.
046690     MOVE 電療料３１ＮＲＷ       TO 会－電療料３０.
046700*     MOVE 連料－逓減開始月３０   TO 会－逓減開始月３０.
046710*     MOVE 連料－逓減開始日３０   TO 会－逓減開始日３０.
046720*     MOVE 連料－後療単価３０     TO 会－後療単価３０.
046730*     MOVE 連料－後療回数３０     TO 会－後療回数３０.
046740*     MOVE 連料－冷罨法回数３０   TO 会－冷罨法回数３０.
046750*     MOVE 連料－温罨法回数３０   TO 会－温罨法回数３０.
046760*     MOVE 連料－電療回数３０     TO 会－電療回数３０.
046770*     MOVE 連料－小計３０         TO 会－小計３０.
046780*     MOVE 連料－長期逓減率３０   TO 会－長期逓減率３０.
046790*     MOVE 連料－長期込小計３０   TO 会－長期込小計３０.
046800****************                             
046810* ４部位／５割 *                             
046820****************                             
046830* ４部位目
046840     MOVE 後療料４ＮＲＷ         TO 会－後療料４５.
046850     MOVE 冷罨法料４ＮＲＷ       TO 会－冷罨法料４５.
046860     MOVE 温罨法料４ＮＲＷ       TO 会－温罨法料４５.
046870     MOVE 電療料４ＮＲＷ         TO 会－電療料４５.
046880*     MOVE 連料－後療単価４５     TO 会－後療単価４５.
046890*     MOVE 連料－後療回数４５     TO 会－後療回数４５.
046900*     MOVE 連料－冷罨法回数４５   TO 会－冷罨法回数４５.
046910*     MOVE 連料－温罨法回数４５   TO 会－温罨法回数４５.
046920*     MOVE 連料－電療回数４５     TO 会－電療回数４５.
046930*     MOVE 連料－小計４５         TO 会－小計４５.
046940*     MOVE 連料－多部位込小計４５ TO 会－多部位込小計４５.
046950*     MOVE 連料－長期逓減率４５   TO 会－長期逓減率４５.
046960*     MOVE 連料－長期込小計４５   TO 会－長期込小計４５.
046970****************                             
046980* ４部位／８割 *                             
046990****************                             
047000* ４部位目（繰上り１）
047010     MOVE 後療料４１ＮＲＷ       TO 会－後療料４８.
047020     MOVE 冷罨法料４１ＮＲＷ     TO 会－冷罨法料４８.
047030     MOVE 温罨法料４１ＮＲＷ     TO 会－温罨法料４８.
047040     MOVE 電療料４１ＮＲＷ       TO 会－電療料４８.
047050*     MOVE 連料－逓減開始月４８   TO 会－逓減開始月４８.
047060*     MOVE 連料－逓減開始日４８   TO 会－逓減開始日４８.
047070*     MOVE 連料－後療回数４８     TO 会－後療回数４８.
047080*     MOVE 連料－冷罨法回数４８   TO 会－冷罨法回数４８.
047090*     MOVE 連料－温罨法回数４８   TO 会－温罨法回数４８.
047100*     MOVE 連料－電療回数４８     TO 会－電療回数４８.
047110*     MOVE 連料－小計４８         TO 会－小計４８.
047120*     MOVE 連料－多部位込小計４８ TO 会－多部位込小計４８.
047130*     MOVE 連料－長期逓減率４８   TO 会－長期逓減率４８.
047140*     MOVE 連料－長期込小計４８   TO 会－長期込小計４８.
047150******************                           
047160* ４部位／１０割 *                           
047170******************                           
047180* ４部位目（繰上り２）
047190     MOVE 後療料４２ＮＲＷ       TO 会－後療料４０.
047200     MOVE 冷罨法料４２ＮＲＷ     TO 会－冷罨法料４０.
047210     MOVE 温罨法料４２ＮＲＷ     TO 会－温罨法料４０.
047220     MOVE 電療料４２ＮＲＷ       TO 会－電療料４０.
047230*     MOVE 連料－逓減開始月４０   TO 会－逓減開始月４０.
047240*     MOVE 連料－逓減開始日４０   TO 会－逓減開始日４０.
047250*     MOVE 連料－後療単価４０     TO 会－後療単価４０.
047260*     MOVE 連料－後療回数４０     TO 会－後療回数４０.
047270*     MOVE 連料－冷罨法回数４０   TO 会－冷罨法回数４０.
047280*     MOVE 連料－温罨法回数４０   TO 会－温罨法回数４０.
047290*     MOVE 連料－電療回数４０     TO 会－電療回数４０.
047300*     MOVE 連料－小計４０         TO 会－小計４０.
047310*     MOVE 連料－長期逓減率４０   TO 会－長期逓減率４０.
047320*     MOVE 連料－長期込小計４０   TO 会－長期込小計４０.
047330********************
047340* ５部位／２．５割 *
047350********************
047360* ５部位目
047370     MOVE 後療料５ＮＲＷ         TO 会－後療料５２.
047380     MOVE 冷罨法料５ＮＲＷ       TO 会－冷罨法料５２.
047390     MOVE 温罨法料５ＮＲＷ       TO 会－温罨法料５２.
047400     MOVE 電療料５ＮＲＷ         TO 会－電療料５２.
047410*     MOVE 連料－後療単価５２     TO 会－後療単価５２.
047420*     MOVE 連料－後療回数５２     TO 会－後療回数５２.
047430*     MOVE 連料－冷罨法回数５２   TO 会－冷罨法回数５２.
047440*     MOVE 連料－温罨法回数５２   TO 会－温罨法回数５２.
047450*     MOVE 連料－電療回数５２     TO 会－電療回数５２.
047460*     MOVE 連料－小計５２         TO 会－小計５２.
047470*     MOVE 連料－多部位込小計５２ TO 会－多部位込小計５２.
047480*     MOVE 連料－長期逓減率５２   TO 会－長期逓減率５２.
047490*     MOVE 連料－長期込小計５２   TO 会－長期込小計５２.
047500****************                             
047510* ５部位／５割 *                             
047520****************                             
047530* ５部位目（繰上り１）
047540     MOVE 後療料５１ＮＲＷ       TO 会－後療料５５.
047550     MOVE 冷罨法料５１ＮＲＷ     TO 会－冷罨法料５５.
047560     MOVE 温罨法料５１ＮＲＷ     TO 会－温罨法料５５.
047570     MOVE 電療料５１ＮＲＷ       TO 会－電療料５５.
047580*     MOVE 連料－逓減開始月５５   TO 会－逓減開始月５５.
047590*     MOVE 連料－逓減開始日５５   TO 会－逓減開始日５５.
047600*     MOVE 連料－後療単価５５     TO 会－後療単価５５.
047610*     MOVE 連料－後療回数５５     TO 会－後療回数５５.
047620*     MOVE 連料－冷罨法回数５５   TO 会－冷罨法回数５５.
047630*     MOVE 連料－温罨法回数５５   TO 会－温罨法回数５５.
047640*     MOVE 連料－電療回数５５     TO 会－電療回数５５.
047650*     MOVE 連料－小計５５         TO 会－小計５５.
047660*     MOVE 連料－多部位込小計５５ TO 会－多部位込小計５５.
047670*     MOVE 連料－長期逓減率５５   TO 会－長期逓減率５５.
047680*     MOVE 連料－長期込小計５５   TO 会－長期込小計５５.
047690****************                             
047700* ５部位／８割 *                             
047710****************                             
047720* ５部位目（繰上り２）
047730     MOVE 後療料５２ＮＲＷ       TO 会－後療料５８.
047740     MOVE 冷罨法料５２ＮＲＷ     TO 会－冷罨法料５８.
047750     MOVE 温罨法料５２ＮＲＷ     TO 会－温罨法料５８.
047760     MOVE 電療料５２ＮＲＷ       TO 会－電療料５８.
047770*     MOVE 連料－逓減開始月５８   TO 会－逓減開始月５８.
047780*     MOVE 連料－逓減開始日５８   TO 会－逓減開始日５８.
047790*     MOVE 連料－後療単価５８     TO 会－後療単価５８.
047800*     MOVE 連料－後療回数５８     TO 会－後療回数５８.
047810*     MOVE 連料－冷罨法回数５８   TO 会－冷罨法回数５８.
047820*     MOVE 連料－温罨法回数５８   TO 会－温罨法回数５８.
047830*     MOVE 連料－電療回数５８     TO 会－電療回数５８.
047840*     MOVE 連料－小計５８         TO 会－小計５８.
047850*     MOVE 連料－多部位込小計５８ TO 会－多部位込小計５８.
047860*     MOVE 連料－長期逓減率５８   TO 会－長期逓減率５８.
047870*     MOVE 連料－長期込小計５８   TO 会－長期込小計５８.
047880******************                           
047890* ５部位／１０割 *                           
047900******************                           
047910* ５部位目（繰上り３）
047920     MOVE 後療料５３ＮＲＷ       TO 会－後療料５０.
047930     MOVE 冷罨法料５３ＮＲＷ     TO 会－冷罨法料５０.
047940     MOVE 温罨法料５３ＮＲＷ     TO 会－温罨法料５０.
047950     MOVE 電療料５３ＮＲＷ       TO 会－電療料５０.
047960*     MOVE 連料－逓減開始月５０   TO 会－逓減開始月５０.
047970*     MOVE 連料－逓減開始日５０   TO 会－逓減開始日５０.
047980*     MOVE 連料－後療単価５０     TO 会－後療単価５０.
047990*     MOVE 連料－後療回数５０     TO 会－後療回数５０.
048000*     MOVE 連料－冷罨法回数５０   TO 会－冷罨法回数５０.
048010*     MOVE 連料－温罨法回数５０   TO 会－温罨法回数５０.
048020*     MOVE 連料－電療回数５０     TO 会－電療回数５０.
048030*     MOVE 連料－小計５０         TO 会－小計５０.
048040*     MOVE 連料－長期逓減率５０   TO 会－長期逓減率５０.
048050*     MOVE 連料－長期込小計５０   TO 会－長期込小計５０.
      */６部位/120804
047690****************
047700* ６部位／８割 *
047710****************
047720* ６部位目（繰上り２）
047730     MOVE 後療料６２ＮＲＷ       TO 会－後療料６８.
047740     MOVE 冷罨法料６２ＮＲＷ     TO 会－冷罨法料６８.
047750     MOVE 温罨法料６２ＮＲＷ     TO 会－温罨法料６８.
047760     MOVE 電療料６２ＮＲＷ       TO 会－電療料６８.
047880******************
047890* ６部位／１０割 *
047900******************
047910* ６部位目（繰上り３）
047920     MOVE 後療料６３ＮＲＷ       TO 会－後療料６０.
047930     MOVE 冷罨法料６３ＮＲＷ     TO 会－冷罨法料６０.
047940     MOVE 温罨法料６３ＮＲＷ     TO 会－温罨法料６０.
047950     MOVE 電療料６３ＮＲＷ       TO 会－電療料６０.
      */７部位/120804
047690****************
047700* ７部位／８割 *
047710****************
047720* ７部位目（繰上り２）
047730     MOVE 後療料７２ＮＲＷ       TO 会－後療料７８.
047740     MOVE 冷罨法料７２ＮＲＷ     TO 会－冷罨法料７８.
047750     MOVE 温罨法料７２ＮＲＷ     TO 会－温罨法料７８.
047760     MOVE 電療料７２ＮＲＷ       TO 会－電療料７８.
047880******************
047890* ７部位／１０割 *
047900******************
047910* ７部位目（繰上り３）
047920     MOVE 後療料７３ＮＲＷ       TO 会－後療料７０.
047930     MOVE 冷罨法料７３ＮＲＷ     TO 会－冷罨法料７０.
047940     MOVE 温罨法料７３ＮＲＷ     TO 会－温罨法料７０.
047950     MOVE 電療料７３ＮＲＷ       TO 会－電療料７０.
DEBUG************************************* debug **********************************************
DEBUG *     IF (部位日計長期Ｗ６２ NOT = ZERO) OR (部位日計長期Ｗ６３ NOT = ZERO) OR
DEBUG *        (部位日計長期Ｗ７２ NOT = ZERO) OR (部位日計長期Ｗ７３ NOT = ZERO)
DEBUG *         DISPLAY 施記－施術日 "日-------------------" END-IF
DEBUG *     IF 部位日計長期Ｗ６２ NOT = ZERO DISPLAY "６  70% 後 " 会－後療料６８ "冷 " 会－冷罨法料６８ "温 " 会－温罨法料６８ "電 "  会－電療料６８  END-IF.
DEBUG *     IF 部位日計長期Ｗ６３ NOT = ZERO DISPLAY "６ 100% 後 " 会－後療料６０ "冷 " 会－冷罨法料６０ "温 " 会－温罨法料６０ "電 "  会－電療料６０  END-IF.
DEBUG *     IF 部位日計長期Ｗ７２ NOT = ZERO DISPLAY "７  70% 後 " 会－後療料７８ "冷 " 会－冷罨法料７８ "温 " 会－温罨法料７８ "電 "  会－電療料７８  END-IF.
DEBUG *     IF 部位日計長期Ｗ７３ NOT = ZERO DISPLAY "７ 100% 後 " 会－後療料７０ "冷 " 会－冷罨法料７０ "温 " 会－温罨法料７０ "電 "  会－電療料７０  END-IF.
DEBUG************************************* debug **********************************************
      *
048060*初検時相談料
048070     MOVE 初検時相談料Ｗ     TO 会－初検時相談料.
048080*
      */施術者番号/160112
           MOVE 施術者番号Ｗ(会－施術日) TO  会－施術者番号
           IF (複数施術者使用区分Ｗ = 2) AND (繰越フラグ(会－施術日) = ZERO)
               MOVE 前回施術者番号Ｗ TO 会－施術者番号
           END-IF.
048090     WRITE 会－レコード
048100     INVALID KEY
      */部位が無い場合は別の場所で書き込み済みの為CONTINUE↓↓↓/20230227
               IF 状態キー = "22"
                   CONTINUE
               ELSE
      */部位が無い場合は別の場所で書き込み済みの為CONTINUE↑↑↑/20230227
048110         MOVE NC"会２" TO ファイル名Ｗ
048120         PERFORM エラー表示
048130         PERFORM 終了処理
048140         EXIT PROGRAM
      */部位が無い場合は別の場所で書き込み済みの為CONTINUE/20230227
               END-IF
048150     END-WRITE.
048151*
048160*================================================================*
048170 エラー表示Ｄ SECTION.
048180*
048190     DISPLAY ファイル名Ｗ NC"ファイル削除エラー"   UPON CONS.
048200     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
048210     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
048220     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
048230*-----------------------------------------*
048240     CALL "actcshm"  WITH C LINKAGE.
048250*-----------------------------------------*
048260     ACCEPT  キー入力 FROM CONS.
048270     PERFORM ファイル閉鎖.
048280     EXIT PROGRAM.
048290*================================================================*
048300 逓減計算 SECTION.
048310*
048320*/ 後療料、冷罨法、温罨法、電療料に多部位逓減と長期逓減を考慮する
048330*/ それぞれの項目に逓減された金額をセットする
048340* 多部位逓減算定
048350* ３部位目逓減
048360     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
048370     COMPUTE 後療料３ＮＲＷ   ROUNDED = 後療料３ＮＲＷ   * 逓減率中間結果Ｗ
048380     COMPUTE 冷罨法料３ＮＲＷ ROUNDED = 冷罨法料３ＮＲＷ * 逓減率中間結果Ｗ
048390     COMPUTE 温罨法料３ＮＲＷ ROUNDED = 温罨法料３ＮＲＷ * 逓減率中間結果Ｗ
048400     COMPUTE 電療料３ＮＲＷ   ROUNDED = 電療料３ＮＲＷ   * 逓減率中間結果Ｗ
048410*
048420* ４部位目逓減　５０％
048430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
048440     COMPUTE 後療料４ＮＲＷ   ROUNDED = 後療料４ＮＲＷ   * 逓減率中間結果Ｗ
048450     COMPUTE 冷罨法料４ＮＲＷ ROUNDED = 冷罨法料４ＮＲＷ * 逓減率中間結果Ｗ
048460     COMPUTE 温罨法料４ＮＲＷ ROUNDED = 温罨法料４ＮＲＷ * 逓減率中間結果Ｗ
048470     COMPUTE 電療料４ＮＲＷ   ROUNDED = 電療料４ＮＲＷ   * 逓減率中間結果Ｗ
048480*
048490* ４部位目逓減　８０％
048500     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
048510     COMPUTE 後療料４１ＮＲＷ   ROUNDED = 後療料４１ＮＲＷ   * 逓減率中間結果Ｗ
048520     COMPUTE 冷罨法料４１ＮＲＷ ROUNDED = 冷罨法料４１ＮＲＷ * 逓減率中間結果Ｗ
048530     COMPUTE 温罨法料４１ＮＲＷ ROUNDED = 温罨法料４１ＮＲＷ * 逓減率中間結果Ｗ
048540     COMPUTE 電療料４１ＮＲＷ   ROUNDED = 電療料４１ＮＲＷ   * 逓減率中間結果Ｗ
048550*
048560* ５部位目逓減　２５％
048570     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(5) / 100
048580     COMPUTE 後療料５ＮＲＷ   ROUNDED = 後療料５ＮＲＷ   * 逓減率中間結果Ｗ
048590     COMPUTE 冷罨法料５ＮＲＷ ROUNDED = 冷罨法料５ＮＲＷ * 逓減率中間結果Ｗ
048600     COMPUTE 温罨法料５ＮＲＷ ROUNDED = 温罨法料５ＮＲＷ * 逓減率中間結果Ｗ
048610     COMPUTE 電療料５ＮＲＷ   ROUNDED = 電療料５ＮＲＷ   * 逓減率中間結果Ｗ
048620*
048630* ５部位目逓減　５０％
048640     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
048650     COMPUTE 後療料５１ＮＲＷ   ROUNDED = 後療料５１ＮＲＷ   * 逓減率中間結果Ｗ
048660     COMPUTE 冷罨法料５１ＮＲＷ ROUNDED = 冷罨法料５１ＮＲＷ * 逓減率中間結果Ｗ
048670     COMPUTE 温罨法料５１ＮＲＷ ROUNDED = 温罨法料５１ＮＲＷ * 逓減率中間結果Ｗ
048680     COMPUTE 電療料５１ＮＲＷ   ROUNDED = 電療料５１ＮＲＷ   * 逓減率中間結果Ｗ
048690*
048700* ５部位目逓減　８０％
048710     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
048720     COMPUTE 後療料５２ＮＲＷ   ROUNDED = 後療料５２ＮＲＷ   * 逓減率中間結果Ｗ
048730     COMPUTE 冷罨法料５２ＮＲＷ ROUNDED = 冷罨法料５２ＮＲＷ * 逓減率中間結果Ｗ
048740     COMPUTE 温罨法料５２ＮＲＷ ROUNDED = 温罨法料５２ＮＲＷ * 逓減率中間結果Ｗ
048750     COMPUTE 電療料５２ＮＲＷ   ROUNDED = 電療料５２ＮＲＷ   * 逓減率中間結果Ｗ
      */６部位/120807
048700* ６部位目逓減　８０％
048710     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
048720     COMPUTE 後療料６２ＮＲＷ   ROUNDED = 後療料６２ＮＲＷ   * 逓減率中間結果Ｗ
048730     COMPUTE 冷罨法料６２ＮＲＷ ROUNDED = 冷罨法料６２ＮＲＷ * 逓減率中間結果Ｗ
048740     COMPUTE 温罨法料６２ＮＲＷ ROUNDED = 温罨法料６２ＮＲＷ * 逓減率中間結果Ｗ
048750     COMPUTE 電療料６２ＮＲＷ   ROUNDED = 電療料６２ＮＲＷ   * 逓減率中間結果Ｗ
      */７部位/120807
048700* ７部位目逓減　８０％
048710     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
048720     COMPUTE 後療料７２ＮＲＷ   ROUNDED = 後療料７２ＮＲＷ   * 逓減率中間結果Ｗ
048730     COMPUTE 冷罨法料７２ＮＲＷ ROUNDED = 冷罨法料７２ＮＲＷ * 逓減率中間結果Ｗ
048740     COMPUTE 温罨法料７２ＮＲＷ ROUNDED = 温罨法料７２ＮＲＷ * 逓減率中間結果Ｗ
048750     COMPUTE 電療料７２ＮＲＷ   ROUNDED = 電療料７２ＮＲＷ   * 逓減率中間結果Ｗ
048760*
048770*
048780* 長期逓減含む部位毎の合計を計算
048790*
048800* １部位　１００％
048810* DEBUG
048820*     DISPLAY NC"長期区分Ｗ１部位目" 長期区分Ｗ(1, 1) UPON MSGBOX.
048830*
048840     IF 長期区分Ｗ(1) = 1
048850         COMPUTE 後療料１ＮＲＷ   ROUNDED = 後療料１ＮＲＷ   * 長期逓減率中間ＫＷ
048860         COMPUTE 冷罨法料１ＮＲＷ ROUNDED = 冷罨法料１ＮＲＷ * 長期逓減率中間ＫＷ
048870         COMPUTE 温罨法料１ＮＲＷ ROUNDED = 温罨法料１ＮＲＷ * 長期逓減率中間ＫＷ
048880         COMPUTE 電療料１ＮＲＷ   ROUNDED = 電療料１ＮＲＷ   * 長期逓減率中間ＫＷ
048890     END-IF.
048900*
048910* ２部位　１００％
048920     IF 長期区分Ｗ(2) = 1
048930         COMPUTE 後療料２ＮＲＷ   ROUNDED = 後療料２ＮＲＷ   * 長期逓減率中間ＫＷ
048940         COMPUTE 冷罨法料２ＮＲＷ ROUNDED = 冷罨法料２ＮＲＷ * 長期逓減率中間ＫＷ
048950         COMPUTE 温罨法料２ＮＲＷ ROUNDED = 温罨法料２ＮＲＷ * 長期逓減率中間ＫＷ
048960         COMPUTE 電療料２ＮＲＷ   ROUNDED = 電療料２ＮＲＷ   * 長期逓減率中間ＫＷ
048970     END-IF.
048980*
048990* ３部位　１００％
049000     IF 長期区分Ｗ(3) = 1
049010         COMPUTE 後療料３１ＮＲＷ   ROUNDED = 後療料３１ＮＲＷ   * 長期逓減率中間ＫＷ
049020         COMPUTE 冷罨法料３１ＮＲＷ ROUNDED = 冷罨法料３１ＮＲＷ * 長期逓減率中間ＫＷ
049030         COMPUTE 温罨法料３１ＮＲＷ ROUNDED = 温罨法料３１ＮＲＷ * 長期逓減率中間ＫＷ
049040         COMPUTE 電療料３１ＮＲＷ   ROUNDED = 電療料３１ＮＲＷ   * 長期逓減率中間ＫＷ
049050     END-IF.
049060*
049070* ３部位　８０％
049080     IF 長期区分Ｗ(3) = 1
049090         COMPUTE 後療料３ＮＲＷ   ROUNDED = 後療料３ＮＲＷ   * 長期逓減率中間ＫＷ
049100         COMPUTE 冷罨法料３ＮＲＷ ROUNDED = 冷罨法料３ＮＲＷ * 長期逓減率中間ＫＷ
049110         COMPUTE 温罨法料３ＮＲＷ ROUNDED = 温罨法料３ＮＲＷ * 長期逓減率中間ＫＷ
049120         COMPUTE 電療料３ＮＲＷ   ROUNDED = 電療料３ＮＲＷ   * 長期逓減率中間ＫＷ
049130     END-IF.
049140*
049150* ４部位　１００％
049160     IF 長期区分Ｗ(4) = 1
049170         COMPUTE 後療料４２ＮＲＷ   ROUNDED = 後療料４２ＮＲＷ   * 長期逓減率中間ＫＷ
049180         COMPUTE 冷罨法料４２ＮＲＷ ROUNDED = 冷罨法料４２ＮＲＷ * 長期逓減率中間ＫＷ
049190         COMPUTE 温罨法料４２ＮＲＷ ROUNDED = 温罨法料４２ＮＲＷ * 長期逓減率中間ＫＷ
049200         COMPUTE 電療料４２ＮＲＷ   ROUNDED = 電療料４２ＮＲＷ   * 長期逓減率中間ＫＷ
049210     END-IF.
049220*
049230* ４部位　８０％
049240     IF 長期区分Ｗ(4) = 1
049250         COMPUTE 後療料４１ＮＲＷ   ROUNDED = 後療料４１ＮＲＷ   * 長期逓減率中間ＫＷ
049260         COMPUTE 冷罨法料４１ＮＲＷ ROUNDED = 冷罨法料４１ＮＲＷ * 長期逓減率中間ＫＷ
049270         COMPUTE 温罨法料４１ＮＲＷ ROUNDED = 温罨法料４１ＮＲＷ * 長期逓減率中間ＫＷ
049280         COMPUTE 電療料４１ＮＲＷ   ROUNDED = 電療料４１ＮＲＷ   * 長期逓減率中間ＫＷ
049290     END-IF
049300*
049310* ４部位　５０％
049320     IF 長期区分Ｗ(4) = 1
049330         COMPUTE 後療料４ＮＲＷ   ROUNDED = 後療料４ＮＲＷ   * 長期逓減率中間ＫＷ
049340         COMPUTE 冷罨法料４ＮＲＷ ROUNDED = 冷罨法料４ＮＲＷ * 長期逓減率中間ＫＷ
049350         COMPUTE 温罨法料４ＮＲＷ ROUNDED = 温罨法料４ＮＲＷ * 長期逓減率中間ＫＷ
049360         COMPUTE 電療料４ＮＲＷ   ROUNDED = 電療料４ＮＲＷ   * 長期逓減率中間ＫＷ
049370     END-IF.
049380*
049390* ５部位　１００％
049400*
049410     IF 長期区分Ｗ(5) = 1
049420         COMPUTE 後療料５３ＮＲＷ   ROUNDED = 後療料５３ＮＲＷ   * 長期逓減率中間ＫＷ
049430         COMPUTE 冷罨法料５３ＮＲＷ ROUNDED = 冷罨法料５３ＮＲＷ * 長期逓減率中間ＫＷ
049440         COMPUTE 温罨法料５３ＮＲＷ ROUNDED = 温罨法料５３ＮＲＷ * 長期逓減率中間ＫＷ
049450         COMPUTE 電療料５３ＮＲＷ   ROUNDED = 電療料５３ＮＲＷ   * 長期逓減率中間ＫＷ
049460     END-IF.
049470*
049480* ５部位　８０％
049490     IF 長期区分Ｗ(5) = 1
049500         COMPUTE 後療料５２ＮＲＷ   ROUNDED = 後療料５２ＮＲＷ   * 長期逓減率中間ＫＷ
049510         COMPUTE 冷罨法料５２ＮＲＷ ROUNDED = 冷罨法料５２ＮＲＷ * 長期逓減率中間ＫＷ
049520         COMPUTE 温罨法料５２ＮＲＷ ROUNDED = 温罨法料５２ＮＲＷ * 長期逓減率中間ＫＷ
049530         COMPUTE 電療料５２ＮＲＷ   ROUNDED = 電療料５２ＮＲＷ   * 長期逓減率中間ＫＷ
049540     END-IF.
049550*
049560* ５部位　５０％
049570     IF 長期区分Ｗ(5) = 1
049580         COMPUTE 後療料５１ＮＲＷ   ROUNDED = 後療料５１ＮＲＷ   * 長期逓減率中間ＫＷ
049590         COMPUTE 冷罨法料５１ＮＲＷ ROUNDED = 冷罨法料５１ＮＲＷ * 長期逓減率中間ＫＷ
049600         COMPUTE 温罨法料５１ＮＲＷ ROUNDED = 温罨法料５１ＮＲＷ * 長期逓減率中間ＫＷ
049610         COMPUTE 電療料５１ＮＲＷ   ROUNDED = 電療料５１ＮＲＷ   * 長期逓減率中間ＫＷ
049620     END-IF.
049630*
049640* ５部位　２５％
049650     IF 長期区分Ｗ(5) = 1
049660         COMPUTE 後療料５ＮＲＷ   ROUNDED = 後療料５ＮＲＷ   * 長期逓減率中間ＫＷ
049670         COMPUTE 冷罨法料５ＮＲＷ ROUNDED = 冷罨法料５ＮＲＷ * 長期逓減率中間ＫＷ
049680         COMPUTE 温罨法料５ＮＲＷ ROUNDED = 温罨法料５ＮＲＷ * 長期逓減率中間ＫＷ
049690         COMPUTE 電療料５ＮＲＷ   ROUNDED = 電療料５ＮＲＷ   * 長期逓減率中間ＫＷ
049700     END-IF.
049710*
      */６部位/120807
049390* ６部位　１００％
049400*
049410     IF 長期区分Ｗ(6) = 1
049420         COMPUTE 後療料６３ＮＲＷ   ROUNDED = 後療料６３ＮＲＷ   * 長期逓減率中間ＫＷ
049430         COMPUTE 冷罨法料６３ＮＲＷ ROUNDED = 冷罨法料６３ＮＲＷ * 長期逓減率中間ＫＷ
049440         COMPUTE 温罨法料６３ＮＲＷ ROUNDED = 温罨法料６３ＮＲＷ * 長期逓減率中間ＫＷ
049450         COMPUTE 電療料６３ＮＲＷ   ROUNDED = 電療料６３ＮＲＷ   * 長期逓減率中間ＫＷ
049460     END-IF.
049470*
049480* ６部位　７０％
049490     IF 長期区分Ｗ(6) = 1
049500         COMPUTE 後療料６２ＮＲＷ   ROUNDED = 後療料６２ＮＲＷ   * 長期逓減率中間ＫＷ
049510         COMPUTE 冷罨法料６２ＮＲＷ ROUNDED = 冷罨法料６２ＮＲＷ * 長期逓減率中間ＫＷ
049520         COMPUTE 温罨法料６２ＮＲＷ ROUNDED = 温罨法料６２ＮＲＷ * 長期逓減率中間ＫＷ
049530         COMPUTE 電療料６２ＮＲＷ   ROUNDED = 電療料６２ＮＲＷ   * 長期逓減率中間ＫＷ
049540     END-IF.
049550*
      */７部位/120807
049390* ７部位　１００％
049400*
049410     IF 長期区分Ｗ(7) = 1
049420         COMPUTE 後療料７３ＮＲＷ   ROUNDED = 後療料７３ＮＲＷ   * 長期逓減率中間ＫＷ
049430         COMPUTE 冷罨法料７３ＮＲＷ ROUNDED = 冷罨法料７３ＮＲＷ * 長期逓減率中間ＫＷ
049440         COMPUTE 温罨法料７３ＮＲＷ ROUNDED = 温罨法料７３ＮＲＷ * 長期逓減率中間ＫＷ
049450         COMPUTE 電療料７３ＮＲＷ   ROUNDED = 電療料７３ＮＲＷ   * 長期逓減率中間ＫＷ
049460     END-IF.
049470*
049480* ７部位　７０％
049490     IF 長期区分Ｗ(7) = 1
049500         COMPUTE 後療料７２ＮＲＷ   ROUNDED = 後療料７２ＮＲＷ   * 長期逓減率中間ＫＷ
049510         COMPUTE 冷罨法料７２ＮＲＷ ROUNDED = 冷罨法料７２ＮＲＷ * 長期逓減率中間ＫＷ
049520         COMPUTE 温罨法料７２ＮＲＷ ROUNDED = 温罨法料７２ＮＲＷ * 長期逓減率中間ＫＷ
049530         COMPUTE 電療料７２ＮＲＷ   ROUNDED = 電療料７２ＮＲＷ   * 長期逓減率中間ＫＷ
049540     END-IF.
049550*
049712*================================================================*
049713*================================================================*
049714 レセＦ固有情報取得 SECTION.
049715*
049716     INITIALIZE 計算拡張情報Ｗ.
049717*
049718     MOVE ZERO TO 助成カウンタ.
049719     PERFORM キー用レセ種別取得.
049730*
049731     MOVE 連計－施術和暦      TO レセ－施術和暦.
049732     MOVE 連計－施術年        TO レセ－施術年.
049733     MOVE 連計－施術月        TO レセ－施術月.
049734     MOVE 連計－患者番号      TO レセ－患者番号.
049735     MOVE 連計－枝番          TO レセ－枝番.
049736*     / キー用レセ種別で取得 /
049737     MOVE レセ種別Ｗ          TO レセ－レセ種別.
049738**
049750     READ レセプトＦ
049751     NOT INVALID KEY
049752         MOVE レセ－償還払い区分     TO 本体償還払い区分Ｗ
049753         MOVE レセ－本体まとめ区分   TO 本体まとめ区分Ｗ
049759     END-READ.
049760**
049761**
049762     IF 受－助成種別 NOT = ZERO
049763        MOVE 連計－施術和暦      TO レセ－施術和暦
049764        MOVE 連計－施術年        TO レセ－施術年
049765        MOVE 連計－施術月        TO レセ－施術月
049766        MOVE 連計－患者番号      TO レセ－患者番号
049767        MOVE 連計－枝番          TO レセ－枝番
049774        MOVE 3                   TO レセ－レセ種別
049775        READ レセプトＦ
049776        NOT INVALID KEY
049777            MOVE レセ－償還払い区分  TO 助成償還払い区分Ｗ
049778        END-READ
049779     END-IF.
049787*
049789*================================================================*
049790*================================================================*
049791 レセプトファイル作成 SECTION.
049792*
049793     MOVE ZERO TO 助成カウンタ.
049794     PERFORM キー用レセ種別取得.
049795***     PERFORM レコード存在チェック.
049796     PERFORM 初検日以前のデータ判定.
049800*
049881***
049883     MOVE 連計－施術和暦      TO レセ－施術和暦.
049884     MOVE 連計－施術年        TO レセ－施術年.
049885     MOVE 連計－施術月        TO レセ－施術月.
049887     MOVE 連計－患者番号      TO レセ－患者番号.
049888     MOVE 連計－枝番          TO レセ－枝番.
049890*     / キー用レセ種別で取得 /
049891     MOVE レセ種別Ｗ          TO レセ－レセ種別.
049892     READ レセプトＦ
049893     INVALID KEY
049896         DISPLAY NC"該当月のレコードがありません。" UPON CONS
049897         DISPLAY NC"施術和暦：" 連計－施術和暦  UPON CONS
049898         DISPLAY NC"施術年："   連計－施術年    UPON CONS 
049899         DISPLAY NC"施術月："   連計－施術月    UPON CONS 
049900         DISPLAY NC"患者番号：" 連計－患者番号  UPON CONS
049901         DISPLAY NC"枝番："     連計－枝番      UPON CONS 
049902         DISPLAY NC"レセ種別：" レセ種別Ｗ      UPON CONS 
049903         MOVE NC"レセプトＦ" TO ファイル名Ｗ
049904         PERFORM エラー表示Ｒ
049907     NOT INVALID KEY
049909*
049910*---------------------------------------------------------------------------------------*
049911*        / 助成月途中変更以外 /
049912         IF ( 連計算助成月途中－助成のみ処理区分 NOT = 1 ) 
049916             PERFORM 一般レコードセット
049917             PERFORM ファイル書込
049919             PERFORM 受診者柔整通院あり更新
049921         END-IF
049922*---------------------------------------------------------------------------------------*
049923*
049928     END-READ.
049929**
049930*    / 助成の時は、２件作成 /
049931     IF 受－助成種別 NOT = ZERO
049932             MOVE 1  TO 助成カウンタ
049940             PERFORM キー用レセ種別取得
049941*
049942             MOVE 連計－施術和暦      TO レセ－施術和暦
049943             MOVE 連計－施術年        TO レセ－施術年
049944             MOVE 連計－施術月        TO レセ－施術月
049946             MOVE 連計－患者番号      TO レセ－患者番号
049947             MOVE 連計－枝番          TO レセ－枝番
049950             MOVE レセ種別Ｗ          TO レセ－レセ種別
049951             READ レセプトＦ
049952             INVALID KEY
049953                 DISPLAY NC"該当月のレコードがありません。" UPON CONS
049954                 DISPLAY NC"施術和暦：" 連計－施術和暦  UPON CONS
049955                 DISPLAY NC"施術年："   連計－施術年    UPON CONS 
049956                 DISPLAY NC"施術月："   連計－施術月    UPON CONS 
049957                 DISPLAY NC"患者番号：" 連計－患者番号  UPON CONS
049958                 DISPLAY NC"枝番："     連計－枝番      UPON CONS 
049959                 DISPLAY NC"レセ種別：" レセ種別Ｗ      UPON CONS 
049960                 MOVE NC"レセプトＦ" TO ファイル名Ｗ
049961                 PERFORM エラー表示Ｒ
049962             NOT INVALID KEY
049963                PERFORM 一般レコードセット
049966*---------------------------------------------------------------------------------------*
049967*               / 助成月途中変更時 /
049968                IF ( 連計算助成月途中－助成のみ処理区分 = 1 )
049973                    MOVE 1  TO  レセ－助成月途中対象
049976                END-IF
049977*---------------------------------------------------------------------------------------*
049983*
049985                PERFORM ファイル書込
049986            END-READ
049987*
049988     END-IF.
049990*
049991*================================================================*
050000 ファイル書込 SECTION.
050010*
050150     REWRITE レセ－レコード.
050151     IF 状態キー NOT = "00"
050152        MOVE NC"レセ更新" TO ファイル名Ｗ
050153        PERFORM エラー表示
050154     END-IF.
DEBUG************************************ debug **********************************************
HILO  *     DISPLAY "レセプトＦ書込 " レセ－レコードキー "|" レセ－運動算定日(1) "'" レセ－運動算定日(2) "'" レセ－運動算定日(3) "'" レセ－運動算定日(4) "'" レセ－運動算定日(5) "'"
DEBUG *     DISPLAY 受－患者氏名(1:20) "-------------------------"
DEBUG *     DISPLAY "             まとめた金額 | まとめる前の金額"
DEBUG *     DISPLAY "レセ－合計         " レセ－合計
DEBUG *     DISPLAY "レセ－一部負担金   " レセ－一部負担金   "  " レセ－印刷レセ負担額       " 印刷レセ負担額       "
DEBUG *     DISPLAY "レセ－請求金額     " レセ－請求金額     "  " レセ－印刷レセ請求額       " 印刷レセ請求額       "
DEBUG *     DISPLAY " "                                                                  
DEBUG *     DISPLAY "レセ－受給者負担額 " レセ－受給者負担額 "  " レセ－印刷レセ受給者負担額 " 印刷レセ受給者負担額 "
DEBUG *     DISPLAY "レセ－助成請求金額 " レセ－助成請求金額 "  " レセ－印刷レセ助成請求金額 " 印刷レセ助成請求金額 ".
DEBUG************************************ debug **********************************************
050155*
050160*================================================================*
050170 一般レコードセット SECTION.
050190*
050211*
050212*    0：なし、1：まとめる
050213*     05 本体枝番まとめ区分                 PIC 9.
050214*     05 例外本体枝番まとめ単独金額合計欄.
050215*        07 例外本体枝番まとめ単独費用額    PIC 9(6).
050216*        07 例外本体枝番まとめ単独負担額    PIC 9(6).
050217*        07 例外本体枝番まとめ単独請求額    PIC 9(6).
050218*
050219*
050461********************
050462* 受診者情報セット *
050463********************
050464**
050465***** 以下はセットなし
050466*     MOVE 連計－施術和暦      TO レセ－施術和暦.
050467*     MOVE 連計－施術年        TO レセ－施術年.
050468*     MOVE 連計－施術月        TO レセ－施術月.
050469*
050470*     MOVE 連計－患者番号      TO レセ－患者番号.
050471*     MOVE 連計－枝番          TO レセ－枝番.
050472*
050473*  / キー用レセ種別で取得 /
050474*     MOVE レセ種別Ｗ          TO レセ－レセ種別.
050475*
050476*     MOVE 受－請求和暦        TO レセ－請求和暦.
050477*     MOVE 受－請求年          TO レセ－請求年.
050478*     MOVE 受－請求月          TO レセ－請求月.
050479*     MOVE 受－請求区分        TO レセ－請求区分.
050480*
050481**
050482*   / 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
050483*
050484*     EVALUATE レセ－レセ種別
050485*     WHEN 1
050486*        MOVE 受－保険者番号         TO レセ－請求保険者番号
050487*     WHEN 2
050488*        MOVE 受－費用負担者番号     TO レセ－請求保険者番号
050489*     WHEN 3
050490*        MOVE 受－費用負担者番号助成 TO レセ－請求保険者番号
050491*     WHEN OTHER
050492*        MOVE SPACE                  TO レセ－請求保険者番号
050493*     END-EVALUATE.
050494*
050495*        07 負傷データ   OCCURS   7.
050496*          09 部位名称１                PIC N(10).
050497*          09 部位名称２                PIC N(10).
050498*
050499*
050500*
050501******
050502*
050503*---------------------------------------------------------------------------*
050504*
050505**    / ファイルの金額欄初期化 /
050506      INITIALIZE レセ－合計欄.
050507      INITIALIZE レセ－印刷レセ金額合計欄.
050508      INITIALIZE レセ－例外本体枝番まとめ単独金額合計欄.
050509      INITIALIZE レセ－料金１.
050510      INITIALIZE レセ－部位１.
050511      INITIALIZE レセ－部位２.
050512      INITIALIZE レセ－部位３８.
050513      INITIALIZE レセ－部位３０.
050514      INITIALIZE レセ－部位４５.
050515      INITIALIZE レセ－部位４８.
050516      INITIALIZE レセ－部位４０.
050517      INITIALIZE レセ－部位５２.
050518      INITIALIZE レセ－部位５５.
050519      INITIALIZE レセ－部位５８.
050520      INITIALIZE レセ－部位５０.
050522*
050523      MOVE ZERO TO レセ－助成月途中対象.
050524*
050526*---------------------------------------------------------------------------*
050527***
050528* 実日数
050529* 部位実日数
050530     PERFORM 部位実日数セット.
050531     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 7
050532         MOVE 部位実日数Ｗ(部位カウンタ) TO レセ－部位実日数(部位カウンタ)
      */令和6年10月改定対応*/20240712
      *         MOVE 部位継続月数Ｗ(部位カウンタ) TO レセ－部位継続月数(部位カウンタ)
050533     END-PERFORM.
050534     MOVE 実日数Ｗ    TO  レセ－レセ実日数.
      */令和6年10月改定対応↓↓↓*/20240712
      *     IF 連計－施術和暦年月 >= 50610
      *         PERFORM 部位継続月数判定
      *     END-IF.
      */令和6年10月改定対応↑↑↑*/20240712
050535*
050540* 新規/継続 チェック
050550     IF 連料－初検料  NOT = ZERO
050560         MOVE NC"○"   TO 新規チェックＷ
050570     ELSE
050580         MOVE NC"○"   TO 継続チェックＷ
050590     END-IF.
050600*   / 1:新規,2:継続,3:両方 /
050610     IF ( 新規チェックＷ = NC"○" ) AND ( 継続チェックＷ = NC"○" )
050620         MOVE 3  TO レセ－レセ請求区分
050630     ELSE
050640        IF 新規チェックＷ = NC"○" 
050650           MOVE 1  TO レセ－レセ請求区分
050660        ELSE
050670           MOVE 2  TO レセ－レセ請求区分
050680        END-IF
050690     END-IF.
050700*************************************************************
050710* 料金データセット(健保・老人・助成） *
050720*************************************************************
050730* 初検
050740     MOVE 連料－初検料         TO  レセ－初検料.
050750     MOVE 連料－初検加算料     TO  レセ－初検加算料.
050760     IF 連料－初検時間外 = 1
050770         MOVE 1   TO レセ－時間外
050780     END-IF.
050790     IF 連料－初検休日 = 1
050800         MOVE 1   TO レセ－休日
050810     END-IF.
050820     IF 連料－初検深夜 = 1
050830         MOVE 1   TO レセ－深夜
050840     END-IF.
050850*
050860     MOVE 連料－初検時相談料   TO レセ－初検時相談料.
050870*---------------------------------------------------------*
050880*
050890     MOVE 連料－再検料         TO  レセ－再検料.
050900* 往療
050910     MOVE 連料－往療距離       TO  レセ－往療距離.
050920     MOVE 連料－往療回数       TO  レセ－往療回数.
050930     MOVE 連料－往療料         TO  レセ－往療料.
050940     MOVE 連料－往療加算料     TO  レセ－往療加算料.
050950     IF 連料－往療夜間 = 1
050960         MOVE 1   TO レセ－夜間
050970     END-IF.
050980     IF 連料－往療暴風雨 = 1
050990         MOVE 1   TO レセ－暴風雨雪
051000     END-IF.
051010     IF 連料－往療難路 = 1
051020         MOVE 1   TO レセ－難路
051030     END-IF.
051040*
051050* 金属など
051060     IF 連料－大 = 1
051070         MOVE 1   TO レセ－大
051080     END-IF.
051090     IF 連料－中 = 1
051100         MOVE 1   TO レセ－中
051110     END-IF.
051120     IF 連料－小 = 1
051130         MOVE 1   TO レセ－小
051140     END-IF.
      */20180607
051060     MOVE 連料－大 TO レセ－大.
051090     MOVE 連料－中 TO レセ－中.
051120     MOVE 連料－小 TO レセ－小.
      */20180613
           MOVE 連料－金属副子回数 TO レセ－金属副子回数.
051150*
051160     MOVE 連料－金属副子加算料         TO  レセ－金属副子加算料.
051170     MOVE 連料－施術情報提供料         TO  レセ－施術情報提供料.
051180     MOVE 連料－小計                   TO  レセ－小計.
      *運動後療料/180503
           MOVE 連料－運動後療回数           TO  レセ－運動後療回数.
           MOVE 連料－運動後療単価           TO  レセ－運動後療単価.
           MOVE 連料－運動後療料             TO  レセ－運動後療料  .
      *明細書発行加算料/20220705
           MOVE 連料－明細書発行加算料       TO  レセ－明細書発行加算料  .
           MOVE 連料－明細書発行加算日       TO  レセ－明細書発行加算日.
051190********************
051200* 初回処置料セット *
051210********************
051211     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 7
051212         MOVE ZERO TO レセ－初回処置料(部位ＣＮＴ)
051213     END-PERFORM.
051220     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
051230             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
051240         MOVE 連料－初回処置料(部位ＣＮＴ) TO レセ－初回処置料(部位ＣＮＴ)
051250     END-PERFORM.
051260     MOVE 連料－初回処置料計           TO レセ－初回処置料合計.
051270*
051280***********
051290* １部位 *
051300***********
051310     MOVE 連料－後療単価１             TO レセ－後療単価１.
051320     MOVE 連料－後療回数１             TO レセ－後療回数１.
051330     MOVE 連料－後療料１               TO レセ－後療料１.
051340     MOVE 連料－冷罨法回数１           TO レセ－冷罨法回数１.
051350     MOVE 連料－冷罨法料１             TO レセ－冷罨法料１.
051360     MOVE 連料－温罨法回数１           TO レセ－温罨法回数１.
051370     MOVE 連料－温罨法料１             TO レセ－温罨法料１.
051380     MOVE 連料－電療回数１             TO レセ－電療回数１.
051390     MOVE 連料－電療料１               TO レセ－電療料１.
051400     MOVE 連料－小計１                 TO レセ－小計１.
051410     MOVE 連料－長期逓減率１           TO レセ－長期逓減率１.
051420     MOVE 連料－長期込小計１           TO レセ－長期込小計１.
051430***********
051440* ２部位 *
051450***********
051460     MOVE 連料－後療単価２             TO レセ－後療単価２.
051470     MOVE 連料－後療回数２             TO レセ－後療回数２.
051480     MOVE 連料－後療料２               TO レセ－後療料２.
051490     MOVE 連料－冷罨法回数２           TO レセ－冷罨法回数２.
051500     MOVE 連料－冷罨法料２             TO レセ－冷罨法料２.
051510     MOVE 連料－温罨法回数２           TO レセ－温罨法回数２.
051520     MOVE 連料－温罨法料２             TO レセ－温罨法料２.
051530     MOVE 連料－電療回数２             TO レセ－電療回数２.
051540     MOVE 連料－電療料２               TO レセ－電療料２.
051550     MOVE 連料－小計２                 TO レセ－小計２.
051560     MOVE 連料－長期逓減率２           TO レセ－長期逓減率２.
051570     MOVE 連料－長期込小計２           TO レセ－長期込小計２.
051580*****************
051590* ３部位／８割 *
051600*****************
051610     MOVE 連料－後療単価３８           TO レセ－後療単価３８.
051620     MOVE 連料－後療回数３８           TO レセ－後療回数３８.
051630     MOVE 連料－後療料３８             TO レセ－後療料３８.
051640     MOVE 連料－冷罨法回数３８         TO レセ－冷罨法回数３８.
051650     MOVE 連料－冷罨法料３８           TO レセ－冷罨法料３８.
051660     MOVE 連料－温罨法回数３８         TO レセ－温罨法回数３８.
051670     MOVE 連料－温罨法料３８           TO レセ－温罨法料３８.
051680     MOVE 連料－電療回数３８           TO レセ－電療回数３８.
051690     MOVE 連料－電療料３８             TO レセ－電療料３８.
051700     MOVE 連料－小計３８               TO レセ－小計３８.
051710     MOVE 連料－多部位込小計３８       TO レセ－多部位込小計３８.
051720     MOVE 連料－長期逓減率３８         TO レセ－長期逓減率３８.
051730     MOVE 連料－長期込小計３８         TO レセ－長期込小計３８.
051740*****************
051750* ３部位／10割 *
051760*****************
051770     MOVE 連料－逓減開始月３０         TO レセ－逓減開始月３０.
051780     MOVE 連料－逓減開始日３０         TO レセ－逓減開始日３０.
051790     MOVE 連料－後療単価３０           TO レセ－後療単価３０.
051800     MOVE 連料－後療回数３０           TO レセ－後療回数３０.
051810     MOVE 連料－後療料３０             TO レセ－後療料３０.
051820     MOVE 連料－冷罨法回数３０         TO レセ－冷罨法回数３０.
051830     MOVE 連料－冷罨法料３０           TO レセ－冷罨法料３０.
051840     MOVE 連料－温罨法回数３０         TO レセ－温罨法回数３０.
051850     MOVE 連料－温罨法料３０           TO レセ－温罨法料３０.
051860     MOVE 連料－電療回数３０           TO レセ－電療回数３０.
051870     MOVE 連料－電療料３０             TO レセ－電療料３０.
051880     MOVE 連料－小計３０               TO レセ－小計３０.
051890     MOVE 連料－長期逓減率３０         TO レセ－長期逓減率３０.
051900     MOVE 連料－長期込小計３０         TO レセ－長期込小計３０.
051910*****************
051920* ４部位／５割 *
051930*****************
051940     MOVE 連料－後療単価４５           TO レセ－後療単価４５.
051950     MOVE 連料－後療回数４５           TO レセ－後療回数４５.
051960     MOVE 連料－後療料４５             TO レセ－後療料４５.
051970     MOVE 連料－冷罨法回数４５         TO レセ－冷罨法回数４５.
051980     MOVE 連料－冷罨法料４５           TO レセ－冷罨法料４５.
051990     MOVE 連料－温罨法回数４５         TO レセ－温罨法回数４５.
052000     MOVE 連料－温罨法料４５           TO レセ－温罨法料４５.
052010     MOVE 連料－電療回数４５           TO レセ－電療回数４５.
052020     MOVE 連料－電療料４５             TO レセ－電療料４５.
052030     MOVE 連料－小計４５               TO レセ－小計４５.
052040     MOVE 連料－多部位込小計４５       TO レセ－多部位込小計４５.
052050     MOVE 連料－長期逓減率４５         TO レセ－長期逓減率４５.
052060     MOVE 連料－長期込小計４５         TO レセ－長期込小計４５.
052070*****************
052080* ４部位／８割 *
052090*****************
052100     MOVE 連料－逓減開始月４８         TO レセ－逓減開始月４８.
052110     MOVE 連料－逓減開始日４８         TO レセ－逓減開始日４８.
052120     MOVE 連料－後療単価４８           TO レセ－後療単価４８.
052130     MOVE 連料－後療回数４８           TO レセ－後療回数４８.
052140     MOVE 連料－後療料４８             TO レセ－後療料４８.
052150     MOVE 連料－冷罨法回数４８         TO レセ－冷罨法回数４８.
052160     MOVE 連料－冷罨法料４８           TO レセ－冷罨法料４８.
052170     MOVE 連料－温罨法回数４８         TO レセ－温罨法回数４８.
052180     MOVE 連料－温罨法料４８           TO レセ－温罨法料４８.
052190     MOVE 連料－電療回数４８           TO レセ－電療回数４８.
052200     MOVE 連料－電療料４８             TO レセ－電療料４８.
052210     MOVE 連料－小計４８               TO レセ－小計４８.
052220     MOVE 連料－多部位込小計４８       TO レセ－多部位込小計４８.
052230     MOVE 連料－長期逓減率４８         TO レセ－長期逓減率４８.
052240     MOVE 連料－長期込小計４８         TO レセ－長期込小計４８.
052250*****************
052260* ４部位／10割 *
052270*****************
052280     MOVE 連料－逓減開始月４０         TO レセ－逓減開始月４０.
052290     MOVE 連料－逓減開始日４０         TO レセ－逓減開始日４０.
052300     MOVE 連料－後療単価４０           TO レセ－後療単価４０.
052310     MOVE 連料－後療回数４０           TO レセ－後療回数４０.
052320     MOVE 連料－後療料４０             TO レセ－後療料４０.
052330     MOVE 連料－冷罨法回数４０         TO レセ－冷罨法回数４０.
052340     MOVE 連料－冷罨法料４０           TO レセ－冷罨法料４０.
052350     MOVE 連料－温罨法回数４０         TO レセ－温罨法回数４０.
052360     MOVE 連料－温罨法料４０           TO レセ－温罨法料４０.
052370     MOVE 連料－電療回数４０           TO レセ－電療回数４０.
052380     MOVE 連料－電療料４０             TO レセ－電療料４０.
052390     MOVE 連料－小計４０               TO レセ－小計４０.
052400     MOVE 連料－長期逓減率４０         TO レセ－長期逓減率４０.
052410     MOVE 連料－長期込小計４０         TO レセ－長期込小計４０.
052420******************
052430* ５部位／2.5割 *
052440******************
052450     MOVE 連料－後療単価５２           TO レセ－後療単価５２.
052460     MOVE 連料－後療回数５２           TO レセ－後療回数５２.
052470     MOVE 連料－後療料５２             TO レセ－後療料５２.
052480     MOVE 連料－冷罨法回数５２         TO レセ－冷罨法回数５２.
052490     MOVE 連料－冷罨法料５２           TO レセ－冷罨法料５２.
052500     MOVE 連料－温罨法回数５２         TO レセ－温罨法回数５２.
052510     MOVE 連料－温罨法料５２           TO レセ－温罨法料５２.
052520     MOVE 連料－電療回数５２           TO レセ－電療回数５２.
052530     MOVE 連料－電療料５２             TO レセ－電療料５２.
052540     MOVE 連料－小計５２               TO レセ－小計５２.
052550     MOVE 連料－多部位込小計５２       TO レセ－多部位込小計５２.
052560     MOVE 連料－長期逓減率５２         TO レセ－長期逓減率５２.
052570     MOVE 連料－長期込小計５２         TO レセ－長期込小計５２.
052580*****************
052590* ５部位／５割 *
052600*****************
052610     MOVE 連料－逓減開始月５５         TO レセ－逓減開始月５５.
052620     MOVE 連料－逓減開始日５５         TO レセ－逓減開始日５５.
052630     MOVE 連料－後療単価５５           TO レセ－後療単価５５.
052640     MOVE 連料－後療回数５５           TO レセ－後療回数５５.
052650     MOVE 連料－後療料５５             TO レセ－後療料５５.
052660     MOVE 連料－冷罨法回数５５         TO レセ－冷罨法回数５５.
052670     MOVE 連料－冷罨法料５５           TO レセ－冷罨法料５５.
052680     MOVE 連料－温罨法回数５５         TO レセ－温罨法回数５５.
052690     MOVE 連料－温罨法料５５           TO レセ－温罨法料５５.
052700     MOVE 連料－電療回数５５           TO レセ－電療回数５５.
052710     MOVE 連料－電療料５５             TO レセ－電療料５５.
052720     MOVE 連料－小計５５               TO レセ－小計５５.
052730     MOVE 連料－多部位込小計５５       TO レセ－多部位込小計５５.
052740     MOVE 連料－長期逓減率５５         TO レセ－長期逓減率５５.
052750     MOVE 連料－長期込小計５５         TO レセ－長期込小計５５.
052760*****************
052770* ５部位／８割 *
052780*****************
052790     MOVE 連料－逓減開始月５８         TO レセ－逓減開始月５８.
052800     MOVE 連料－逓減開始日５８         TO レセ－逓減開始日５８.
052810     MOVE 連料－後療単価５８           TO レセ－後療単価５８.
052820     MOVE 連料－後療回数５８           TO レセ－後療回数５８.
052830     MOVE 連料－後療料５８             TO レセ－後療料５８.
052840     MOVE 連料－冷罨法回数５８         TO レセ－冷罨法回数５８.
052850     MOVE 連料－冷罨法料５８           TO レセ－冷罨法料５８.
052860     MOVE 連料－温罨法回数５８         TO レセ－温罨法回数５８.
052870     MOVE 連料－温罨法料５８           TO レセ－温罨法料５８.
052880     MOVE 連料－電療回数５８           TO レセ－電療回数５８.
052890     MOVE 連料－電療料５８             TO レセ－電療料５８.
052900     MOVE 連料－小計５８               TO レセ－小計５８.
052910     MOVE 連料－多部位込小計５８       TO レセ－多部位込小計５８.
052920     MOVE 連料－長期逓減率５８         TO レセ－長期逓減率５８.
052930     MOVE 連料－長期込小計５８         TO レセ－長期込小計５８.
052940*****************
052950* ５部位／10割 *
052960*****************
052970     MOVE 連料－逓減開始月５０         TO レセ－逓減開始月５０.
052980     MOVE 連料－逓減開始日５０         TO レセ－逓減開始日５０.
052990     MOVE 連料－後療単価５０           TO レセ－後療単価５０.
053000     MOVE 連料－後療回数５０           TO レセ－後療回数５０.
053010     MOVE 連料－後療料５０             TO レセ－後療料５０.
053020     MOVE 連料－冷罨法回数５０         TO レセ－冷罨法回数５０.
053030     MOVE 連料－冷罨法料５０           TO レセ－冷罨法料５０.
053040     MOVE 連料－温罨法回数５０         TO レセ－温罨法回数５０.
053050     MOVE 連料－温罨法料５０           TO レセ－温罨法料５０.
053060     MOVE 連料－電療回数５０           TO レセ－電療回数５０.
053070     MOVE 連料－電療料５０             TO レセ－電療料５０.
053080     MOVE 連料－小計５０               TO レセ－小計５０.
053090     MOVE 連料－長期逓減率５０         TO レセ－長期逓減率５０.
053100     MOVE 連料－長期込小計５０         TO レセ－長期込小計５０.
      */６部位/120804
052760*****************
052770* ６部位／7割 *
052780*****************
052790     MOVE 連料－逓減開始月６８         TO レセ－逓減開始月６８.
052800     MOVE 連料－逓減開始日６８         TO レセ－逓減開始日６８.
052810     MOVE 連料－後療単価６８           TO レセ－後療単価６８.
052820     MOVE 連料－後療回数６８           TO レセ－後療回数６８.
052830     MOVE 連料－後療料６８             TO レセ－後療料６８.
052840     MOVE 連料－冷罨法回数６８         TO レセ－冷罨法回数６８.
052850     MOVE 連料－冷罨法料６８           TO レセ－冷罨法料６８.
052860     MOVE 連料－温罨法回数６８         TO レセ－温罨法回数６８.
052870     MOVE 連料－温罨法料６８           TO レセ－温罨法料６８.
052880     MOVE 連料－電療回数６８           TO レセ－電療回数６８.
052890     MOVE 連料－電療料６８             TO レセ－電療料６８.
052900     MOVE 連料－小計６８               TO レセ－小計６８.
052910     MOVE 連料－多部位込小計６８       TO レセ－多部位込小計６８.
052920     MOVE 連料－長期逓減率６８         TO レセ－長期逓減率６８.
052930     MOVE 連料－長期込小計６８         TO レセ－長期込小計６８.
052940*****************
052950* ６部位／10割 *
052960*****************
052970     MOVE 連料－逓減開始月６０         TO レセ－逓減開始月６０.
052980     MOVE 連料－逓減開始日６０         TO レセ－逓減開始日６０.
052990     MOVE 連料－後療単価６０           TO レセ－後療単価６０.
053000     MOVE 連料－後療回数６０           TO レセ－後療回数６０.
053010     MOVE 連料－後療料６０             TO レセ－後療料６０.
053020     MOVE 連料－冷罨法回数６０         TO レセ－冷罨法回数６０.
053030     MOVE 連料－冷罨法料６０           TO レセ－冷罨法料６０.
053040     MOVE 連料－温罨法回数６０         TO レセ－温罨法回数６０.
053050     MOVE 連料－温罨法料６０           TO レセ－温罨法料６０.
053060     MOVE 連料－電療回数６０           TO レセ－電療回数６０.
053070     MOVE 連料－電療料６０             TO レセ－電療料６０.
053080     MOVE 連料－小計６０               TO レセ－小計６０.
053090     MOVE 連料－長期逓減率６０         TO レセ－長期逓減率６０.
053100     MOVE 連料－長期込小計６０         TO レセ－長期込小計６０.
      */７部位/120804
052760*****************
052770* ７部位／7割 *
052780*****************
052790     MOVE 連料－逓減開始月７８         TO レセ－逓減開始月７８.
052800     MOVE 連料－逓減開始日７８         TO レセ－逓減開始日７８.
052810     MOVE 連料－後療単価７８           TO レセ－後療単価７８.
052820     MOVE 連料－後療回数７８           TO レセ－後療回数７８.
052830     MOVE 連料－後療料７８             TO レセ－後療料７８.
052840     MOVE 連料－冷罨法回数７８         TO レセ－冷罨法回数７８.
052850     MOVE 連料－冷罨法料７８           TO レセ－冷罨法料７８.
052860     MOVE 連料－温罨法回数７８         TO レセ－温罨法回数７８.
052870     MOVE 連料－温罨法料７８           TO レセ－温罨法料７８.
052880     MOVE 連料－電療回数７８           TO レセ－電療回数７８.
052890     MOVE 連料－電療料７８             TO レセ－電療料７８.
052900     MOVE 連料－小計７８               TO レセ－小計７８.
052910     MOVE 連料－多部位込小計７８       TO レセ－多部位込小計７８.
052920     MOVE 連料－長期逓減率７８         TO レセ－長期逓減率７８.
052930     MOVE 連料－長期込小計７８         TO レセ－長期込小計７８.
052940*****************
052950* ７部位／10割 *
052960*****************
052970     MOVE 連料－逓減開始月７０         TO レセ－逓減開始月７０.
052980     MOVE 連料－逓減開始日７０         TO レセ－逓減開始日７０.
052990     MOVE 連料－後療単価７０           TO レセ－後療単価７０.
053000     MOVE 連料－後療回数７０           TO レセ－後療回数７０.
053010     MOVE 連料－後療料７０             TO レセ－後療料７０.
053020     MOVE 連料－冷罨法回数７０         TO レセ－冷罨法回数７０.
053030     MOVE 連料－冷罨法料７０           TO レセ－冷罨法料７０.
053040     MOVE 連料－温罨法回数７０         TO レセ－温罨法回数７０.
053050     MOVE 連料－温罨法料７０           TO レセ－温罨法料７０.
053060     MOVE 連料－電療回数７０           TO レセ－電療回数７０.
053070     MOVE 連料－電療料７０             TO レセ－電療料７０.
053080     MOVE 連料－小計７０               TO レセ－小計７０.
053090     MOVE 連料－長期逓減率７０         TO レセ－長期逓減率７０.
053100     MOVE 連料－長期込小計７０         TO レセ－長期込小計７０.

      */令和6年10月改定対応↓↓↓*/20240712
      */令和6年9月は継続月数のみ書き込む
           IF 連計－施術和暦年月 >= 50609
               PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
                   MOVE 部位継続月数Ｗ(部位ＣＮＴ) TO レセ－部位継続月数(部位ＣＮＴ)
               END-PERFORM
           END-IF


           IF 連計－施術和暦年月 >= 50610
      *
               MOVE ZERO TO レセ－長期頻回逓減率１   レセ－長期頻回逓減率２   レセ－長期頻回逓減率３８
                            レセ－長期頻回逓減率３０ レセ－長期頻回逓減率４８ レセ－長期頻回逓減率４０
                            レセ－長期頻回逓減率５８ レセ－長期頻回逓減率５０ レセ－長期頻回逓減率６８
                            レセ－長期頻回逓減率６０ レセ－長期頻回逓減率７８ レセ－長期頻回逓減率７０
      *
               PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
                   MOVE 部位継続月数Ｗ(部位ＣＮＴ) TO レセ－部位継続月数(部位ＣＮＴ)
               END-PERFORM
      *
               IF (レセ－小計１ > ZERO) AND (レセ－部位継続月数(1) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率１
                   COMPUTE レセ－長期込小計１ ROUNDED = レセ－小計１ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計１ TO 部位合計長期Ｗ１
               END-IF
               IF (レセ－小計２ > ZERO) AND (レセ－部位継続月数(2) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率２  
                   COMPUTE レセ－長期込小計２ ROUNDED = レセ－小計２ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計２ TO 部位合計長期Ｗ２
               END-IF
               IF (レセ－小計３８ > ZERO) AND (レセ－部位継続月数(3) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率３８
      *             COMPUTE レセ－長期込小計３８ ROUNDED = レセ－小計３８ * 長期頻回逓減率ＫＷ / 100
                   COMPUTE レセ－長期込小計３８ ROUNDED = レセ－多部位込小計３８ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計３８ TO 部位合計長期Ｗ３
               END-IF
               IF (レセ－小計３０ > ZERO) AND (レセ－部位継続月数(3) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率３０
                   COMPUTE レセ－長期込小計３０ ROUNDED = レセ－小計３０ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計３０ TO 部位合計長期Ｗ３１
               END-IF
               IF (レセ－小計４８ > ZERO) AND (レセ－部位継続月数(4) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率４８
      *             COMPUTE レセ－長期込小計４８ ROUNDED = レセ－小計４８ * 長期頻回逓減率ＫＷ / 100
                   COMPUTE レセ－長期込小計４８ ROUNDED = レセ－多部位込小計４８ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計４８ TO 部位合計長期Ｗ４１
               END-IF
               IF (レセ－小計４０ > ZERO) AND (レセ－部位継続月数(4) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率４０
                   COMPUTE レセ－長期込小計４０ ROUNDED = レセ－小計４０ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計４０ TO 部位合計長期Ｗ４２
               END-IF
               IF (レセ－小計５８ > ZERO) AND (レセ－部位継続月数(5) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率５８
      *             COMPUTE レセ－長期込小計５８ ROUNDED = レセ－小計５８ * 長期頻回逓減率ＫＷ / 100
                   COMPUTE レセ－長期込小計５８ ROUNDED = レセ－多部位込小計５８ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計５８ TO 部位合計長期Ｗ５２
               END-IF
               IF (レセ－小計５０ > ZERO) AND (レセ－部位継続月数(5) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率５０
                   COMPUTE レセ－長期込小計５０ ROUNDED = レセ－小計５０ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計５０ TO 部位合計長期Ｗ５３
               END-IF
               IF (レセ－小計６８ > ZERO) AND (レセ－部位継続月数(6) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率６８
      *             COMPUTE レセ－長期込小計６８ ROUNDED = レセ－小計６８ * 長期頻回逓減率ＫＷ / 100
                   COMPUTE レセ－長期込小計６８ ROUNDED = レセ－多部位込小計６８ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計６８ TO 部位合計長期Ｗ６２
               END-IF
               IF (レセ－小計６０ > ZERO) AND (レセ－部位継続月数(6) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率６０
                   COMPUTE レセ－長期込小計６０ ROUNDED = レセ－小計６０ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計６０ TO 部位合計長期Ｗ６３
               END-IF
               IF (レセ－小計７８ > ZERO) AND (レセ－部位継続月数(7) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率７８
      *             COMPUTE レセ－長期込小計７８ ROUNDED = レセ－小計７８ * 長期頻回逓減率ＫＷ / 100
                   COMPUTE レセ－長期込小計７８ ROUNDED = レセ－多部位込小計７８ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計７８ TO 部位合計長期Ｗ７２
               END-IF
               IF (レセ－小計７０ > ZERO) AND (レセ－部位継続月数(7) >= 6)
                   MOVE 長期頻回逓減率ＫＷ   TO レセ－長期頻回逓減率７０
                   COMPUTE レセ－長期込小計７０ ROUNDED = レセ－小計７０ * 長期頻回逓減率ＫＷ / 100
      *             MOVE レセ－長期込小計７０ TO 部位合計長期Ｗ７３
               END-IF

HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率１  "  レセ－長期頻回逓減率１  
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率２  "  レセ－長期頻回逓減率２  
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率３８"  レセ－長期頻回逓減率３８
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率３０"  レセ－長期頻回逓減率３０
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率４８"  レセ－長期頻回逓減率４８
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率４０"  レセ－長期頻回逓減率４０
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率５８"  レセ－長期頻回逓減率５８
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率５０"  レセ－長期頻回逓減率５０
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率６８"  レセ－長期頻回逓減率６８
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率６０"  レセ－長期頻回逓減率６０
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率７８"  レセ－長期頻回逓減率７８
HILO  *         DISPLAY "10-3-4 レセ－長期頻回逓減率７０"  レセ－長期頻回逓減率７０

           END-IF.
DEBUG************************************ debug **********************************************
DEBUG *     DISPLAY " "
DEBUG *     DISPLAY 受－患者コード 受－患者氏名(1:15) "------------------------------------------------"
DEBUG *     DISPLAY "                 後              冷         温         電        合計   多部位計 長期"
DEBUG *     DISPLAY "６  70% "
DEBUG *             レセ－逓減開始月６８ "月" レセ－逓減開始日６８ "日"
DEBUG *         "(" レセ－後療単価６８   "x"  レセ－後療回数６８ "=" レセ－後療料６８ ") "
DEBUG *         "(" レセ－冷罨法回数６８ "回" レセ－冷罨法料６８ ") "
DEBUG *         "(" レセ－温罨法回数６８ "回" レセ－温罨法料６８ ") "
DEBUG *         "(" レセ－電療回数６８   "回" レセ－電療料６８   ") "
DEBUG *             レセ－小計６８       " "  レセ－多部位込小計６８  "   x"  
DEBUG *             レセ－長期逓減率６８ "="  レセ－長期込小計６８ 
DEBUG *     DISPLAY "６ 100% "
DEBUG *             レセ－逓減開始月６０ "月" レセ－逓減開始日６０ "日"
DEBUG *         "(" レセ－後療単価６０   "x"  レセ－後療回数６０ "=" レセ－後療料６０ ") "
DEBUG *         "(" レセ－冷罨法回数６０ "回" レセ－冷罨法料６０ ") "
DEBUG *         "(" レセ－温罨法回数６０ "回" レセ－温罨法料６０ ") "
DEBUG *         "(" レセ－電療回数６０   "回" レセ－電療料６０   ") "
DEBUG *             レセ－小計６０ "          x"
DEBUG *             レセ－長期逓減率６０ "="  レセ－長期込小計６０
DEBUG *     DISPLAY "７  70% "
DEBUG *             レセ－逓減開始月７８ "月" レセ－逓減開始日７８ "日"
DEBUG *         "(" レセ－後療単価７８   "x"  レセ－後療回数７８ "=" レセ－後療料７８ ") "
DEBUG *         "(" レセ－冷罨法回数７８ "回" レセ－冷罨法料７８ ") "
DEBUG *         "(" レセ－温罨法回数７８ "回" レセ－温罨法料７８ ") "
DEBUG *         "(" レセ－電療回数７８   "回" レセ－電療料７８   ") "
DEBUG *             レセ－小計７８       " "  レセ－多部位込小計７８  "   x"  
DEBUG *             レセ－長期逓減率７８ "="  レセ－長期込小計７８ 
DEBUG *     DISPLAY "７ 100% "
DEBUG *             レセ－逓減開始月７０ "月" レセ－逓減開始日７０ "日"
DEBUG *         "(" レセ－後療単価７０   "x"  レセ－後療回数７０ "=" レセ－後療料７０ ") "
DEBUG *         "(" レセ－冷罨法回数７０ "回" レセ－冷罨法料７０ ") "
DEBUG *         "(" レセ－温罨法回数７０ "回" レセ－温罨法料７０ ") "
DEBUG *         "(" レセ－電療回数７０   "回" レセ－電療料７０   ") "
DEBUG *             レセ－小計７０ "          x"
DEBUG *             レセ－長期逓減率７０ "="  レセ－長期込小計７０ 
DEBUG************************************ debug *************************************************
053110*
053120************
053130* 合計欄 *
053140************
053150     MOVE 連料－費用額                 TO レセ－合計.
053160     MOVE 連料－一部負担金             TO レセ－一部負担金.
053170     MOVE 連料－請求額                 TO レセ－請求金額.
053180     MOVE 連料－受給者負担額           TO レセ－受給者負担額.
053190     MOVE 連料－助成請求金額           TO レセ－助成請求金額.
053200*
053210* 負担率
053220* 14/10～ 老人対応  
053230     IF ( レセ－レセ種別 = 2 OR 3 ) AND
053240        ( 受－公費種別 NOT = ZERO )
053250*        IF 施術和暦年月ＷＲ >= 41410
053260           PERFORM 負担率取得１４１０
053270*        END-IF
053280     ELSE
053290        COMPUTE レセ－負担割合 = 連計－負担率 / 10
053300     END-IF.
053310*
053320     COMPUTE レセ－給付割合 = 10 - レセ－負担割合.
053330*
053340     MOVE 連料－一部負担金             TO レセ－印刷レセ負担額      .
053350     MOVE 連料－請求額                 TO レセ－印刷レセ請求額      .
053360     MOVE 連料－受給者負担額           TO レセ－印刷レセ受給者負担額.
053370     MOVE 連料－助成請求金額           TO レセ－印刷レセ助成請求金額.
053371*
053381***
053382*---------------------------------------------------------------------------------------*
053383**※--- 償還払い・本体まとめ時----------------------------------------------------------*
053384*** 上記後（最後に）、償還払い・本体まとめの金額を算出し、セットし直す。
053385     IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 ) OR ( 本体まとめ区分Ｗ = 1 )
053386*****
053387**
053388*       IF 助成カウンタ = ZERO
053394*           IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
053395*              IF 受－助成種別 = ZERO
053396*                /本体のみ償還/
053397*                 IF 本体償還払い区分Ｗ = 1
053398*                    MOVE レセ－合計  TO レセ－一部負担金
053399*                    MOVE ZERO        TO レセ－請求金額
053400*                 END-IF
053401*              ELSE
053402*                 EVALUATE TRUE
053403*                /本体償還、助成通常/
053404*                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
053405*                    COMPUTE レセ－一部負担金 =  レセ－合計 - 連料－一部負担金
053406*                    MOVE ZERO  TO レセ－請求金額
053407*                /本体通常、助成償還/
053408*                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
053409*                    CONTINUE
053410*                /本体償還、助成償還/
053411*                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
053412*                    COMPUTE レセ－一部負担金 =  レセ－合計 - 連料－一部負担金
053413*                    MOVE ZERO  TO レセ－請求金額
053414*                 END-EVALUATE
053415*              END-IF
053416*           ELSE
053417*             / 本体まとめのみ /
053418*              MOVE ZERO            TO レセ－一部負担金
053419*              MOVE レセ－合計      TO レセ－請求金額
053421*              MOVE ZERO    TO  レセ－受給者負担額
053422*              MOVE ZERO    TO  レセ－助成請求金額
053423*              MOVE ZERO    TO  レセ－印刷レセ受給者負担額
053424*              MOVE ZERO    TO  レセ－印刷レセ助成請求金額
053427*           END-IF
053428*       ELSE
053429*      / 助成レコード時 /
053430*           IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
053431*              IF 受－助成種別 NOT = ZERO
053432*                 EVALUATE TRUE
053433*                /本体償還、助成通常/
053434*                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
053435*                    CONTINUE
053436*                /本体通常、助成償還/
053437*                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
053438*                    MOVE ZERO    TO  レセ－受給者負担額
053439*                    MOVE ZERO    TO  レセ－助成請求金額
053440*                /本体償還、助成償還/
053441*                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
053442*                    MOVE レセ－印刷レセ負担額  TO レセ－受給者負担額
053443*                    MOVE ZERO                  TO レセ－助成請求金額
053444*                 END-EVALUATE
053445*              END-IF
053446*           ELSE
053447*             / 本体まとめのみ /
053448*              MOVE ZERO            TO レセ－一部負担金
053449*              MOVE レセ－合計      TO レセ－請求金額
053450*              MOVE ZERO    TO  レセ－受給者負担額
053451*              MOVE ZERO    TO  レセ－助成請求金額
053452*              MOVE ZERO    TO  レセ－印刷レセ受給者負担額
053453*              MOVE ZERO    TO  レセ－印刷レセ助成請求金額
053454*           END-IF
053455*       END-IF
053456***
053460        IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
053461              IF 受－助成種別 = ZERO
053462*                /本体のみ償還/
053463                 IF 本体償還払い区分Ｗ = 1
053464                    MOVE レセ－合計  TO レセ－一部負担金
053465                    MOVE ZERO        TO レセ－請求金額
053466                 END-IF
053467              ELSE
053468                 EVALUATE TRUE
053469*                /本体償還、助成通常/
053470                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
053471                    COMPUTE レセ－一部負担金 =  レセ－合計 - 連料－一部負担金
053472                    MOVE ZERO  TO レセ－請求金額
053473*                /本体通常、助成償還/
053474                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
053476                    MOVE ZERO    TO  レセ－受給者負担額
053477                    MOVE ZERO    TO  レセ－助成請求金額
053481*                /本体償還、助成償還/
053482                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
053483                    COMPUTE レセ－一部負担金 =  レセ－合計 - 連料－一部負担金
053484                    MOVE ZERO  TO レセ－請求金額
053486                    MOVE レセ－印刷レセ負担額  TO レセ－受給者負担額
053487                    MOVE ZERO                  TO レセ－助成請求金額
053491                 END-EVALUATE
053492              END-IF
053493        ELSE
053494*             / 本体まとめのみ /
      */まとめる助成に負担がある場合に対応↓↓↓/130409
053495*              MOVE ZERO            TO レセ－一部負担金
053496*              MOVE レセ－合計      TO レセ－請求金額
                    MOVE レセ－受給者負担額 TO レセ－一部負担金
                    COMPUTE レセ－請求金額 = レセ－合計 - レセ－一部負担金
      */まとめる助成に負担がある場合に対応↑↑↑/130409
      */まとめる助成は本体分にまとまっている為クリアする(入金管理等帳票に含まれる為)↓↓↓/170618
      **/↑の修正漏れ？クリアしないように？↓↓↓/170316
053497              MOVE ZERO    TO  レセ－受給者負担額
053498              MOVE ZERO    TO  レセ－助成請求金額
      */↑の修正漏れ？クリアしないように？↑↑↑/170316
      */まとめる助成は本体分にまとまっている為クリアする(入金管理等帳票に含まれる為)↑↑↑/170618
      */県固有書類でまとめる前の助成分の金額を使用する為クリアしない/120906
053499*              MOVE ZERO    TO  レセ－印刷レセ受給者負担額
053500*              MOVE ZERO    TO  レセ－印刷レセ助成請求金額
053501        END-IF
053506*
053507     END-IF.
053508*---------------------------------------------------------------------------------------*
053509*---------------------------------------------------------------------------------------*
053510*    / 被災者 /
053511     IF 受－資格証明区分 = 9
      */レセプトには０割負担、１０割請求で記載する/120502
053530*        MOVE ZERO         TO レセ－一部負担金
053531*        MOVE レセ－合計   TO レセ－請求金額
053532*        MOVE ZERO         TO レセ－受給者負担額
053533*        MOVE ZERO         TO レセ－助成請求金額
053534*        MOVE ZERO         TO レセ－印刷レセ受給者負担額
053535*        MOVE ZERO         TO レセ－印刷レセ助成請求金額
      */レセまとめと仕様を合わせる為変更↓↓↓/120911
021780*        MOVE ZERO         TO レセ－印刷レセ負担額
021790*        MOVE レセ－合計   TO レセ－印刷レセ請求額
021780        MOVE ZERO         TO レセ－一部負担金
021790        MOVE レセ－合計   TO レセ－請求金額
      */レセまとめと仕様を合わせる為変更↑↑↑/120911
053585     END-IF.
053586*
053587*---------------------------------------------------------------------------------------*
053588*
      */運動算定日/20180526
           MOVE 運動算定日Ｗ(1) TO レセ－運動算定日(1).
           MOVE 運動算定日Ｗ(2) TO レセ－運動算定日(2).
           MOVE 運動算定日Ｗ(3) TO レセ－運動算定日(3).
           MOVE 運動算定日Ｗ(4) TO レセ－運動算定日(4).
           MOVE 運動算定日Ｗ(5) TO レセ－運動算定日(5).
053589*================================================================*
053590*================================================================*
053591*================================================================*
053592 キー用レセ種別取得 SECTION.
053593*
053594* / レセプトＦのキー：レセ種別を決定する /
053595*
053596*   / 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
053597*
053598     MOVE ZERO TO レセ種別Ｗ.
053599*
053600     IF 受－保険種別 = 91
053601        MOVE  8   TO レセ種別Ｗ
053602     ELSE
053604        IF 受－助成種別 NOT = ZERO
053605           IF 助成カウンタ = ZERO
053606              IF 受－公費種別 = ZERO
053607                 MOVE  1   TO レセ種別Ｗ
053608              ELSE
053609                 MOVE  2   TO レセ種別Ｗ
053610              END-IF
053611           ELSE
053612              MOVE  3   TO レセ種別Ｗ
053613           END-IF
053614        ELSE
053615           IF 受－公費種別 NOT = ZERO
053616              MOVE  2   TO レセ種別Ｗ
053617           ELSE
053618              EVALUATE 受－保険種別
053619              WHEN 70
053620                  MOVE  4   TO レセ種別Ｗ
053621              WHEN 80
053622                  MOVE  5   TO レセ種別Ｗ
053623              WHEN 90
053630                  MOVE  6   TO レセ種別Ｗ
053640              WHEN 85
053650                  MOVE  7   TO レセ種別Ｗ
053660              WHEN 91
053670                  MOVE  8   TO レセ種別Ｗ
053671              WHEN OTHER
053672                  MOVE  1   TO レセ種別Ｗ
053680              END-EVALUATE
053681           END-IF
053682        END-IF
053683     END-IF.
053690*
053700*================================================================*
054220*================================================================*
054450*================================================================*
054460 初検日以前のデータ判定 SECTION.
054470*
054480*********************************************************************************
054490*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
054500*  継続にもチェックする。(新規と継続の両方)
054510*********************************************************************************
054520*
054530** 最初の初検日を取得
054540     MOVE SPACE                 TO 初検フラグ.
054550     MOVE 連計－患者番号        TO 施記－患者番号.
054560     MOVE 連計－枝番            TO 施記－枝番.
054570     MOVE 連計－施術和暦        TO 施記－施術和暦.
054580     MOVE 連計－施術年          TO 施記－施術年.
054590     MOVE 連計－施術月          TO 施記－施術月.
054600     MOVE ZERO                  TO 施記－施術日.
054610     START 施術記録Ｆ   KEY IS >= 施記－患者コード
054620                                  施記－施術和暦年月日
054630     END-START.
054640     IF 状態キー = "00"
054650         MOVE ZERO  TO 初検和暦ＷＴ
054660         MOVE ZERO  TO 初検年ＷＴ
054670         MOVE ZERO  TO 初検月ＷＴ
054680         MOVE ZERO  TO 初検日ＷＴ
054690         MOVE SPACE TO 終了フラグ
054700         PERFORM 施術記録Ｆ読込
054710         PERFORM UNTIL ( 終了フラグ           = "YES"           ) OR
054720                       ( 施記－患者コード NOT = 連計－患者コード) OR
054730                       ( 施記－施術和暦   NOT = 連計－施術和暦  ) OR
054740                       ( 施記－施術年     NOT = 連計－施術年    ) OR
054750                       ( 施記－施術月     NOT = 連計－施術月    ) 
054760*
054770*         /  最初の初検日を取得
054780               IF ( 施記－診療区分 = 2 ) AND ( 初検フラグ = SPACE )
054790                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
054800                   MOVE 施記－施術年             TO 初検年ＷＴ
054810                   MOVE 施記－施術月             TO 初検月ＷＴ
054820                   MOVE 施記－施術日             TO 初検日ＷＴ
054830                   MOVE "YES"                    TO 初検フラグ
054840                   MOVE "YES"                    TO 終了フラグ
054850               END-IF
054860               PERFORM 施術記録Ｆ読込
054870         END-PERFORM
054880     END-IF.
054890*
054900* 初検日以前のデータ判定
054910     IF 初検フラグ = "YES"
054920        MOVE 連計－患者番号        TO 施記－患者番号
054930        MOVE 連計－枝番            TO 施記－枝番
054940        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
054950        MOVE 初検年ＷＴ            TO 施記－施術年
054960        MOVE 初検月ＷＴ            TO 施記－施術月
054970        MOVE 初検日ＷＴ            TO 施記－施術日
054980        START 施術記録Ｆ   KEY IS <  施記－患者コード
054990                                     施記－施術和暦年月日
055000                                     REVERSED
055010        END-START
055020        IF 状態キー = "00"
055030           MOVE SPACE  TO 終了フラグ
055040           PERFORM 施術記録Ｆ読込
055050           IF ( 終了フラグ      = SPACE         ) AND
055060              ( 施記－患者番号  = 連計－患者番号) AND
055070              ( 施記－枝番      = 連計－枝番    ) AND
055080              ( 施記－施術和暦  = 初検和暦ＷＴ  ) AND
055090              ( 施記－施術年    = 初検年ＷＴ    ) AND
055100              ( 施記－施術月    = 初検月ＷＴ    )
055110*  初検日以前の当月中に施術記録レコードがあった時
055120                IF 継続チェックＷ = SPACE
055130                   MOVE NC"○"    TO 継続チェックＷ
055140                END-IF
055150           END-IF
055160         END-IF
055170     END-IF.
055180*
055190*================================================================*
055200 負担率取得１４１０ SECTION.
055210*
055220* 平成14/10～
055230     MOVE SPACE TO 連率－負担率取得キー.
055240     INITIALIZE 連率－負担率取得キー.
055250     MOVE 連計－施術和暦年月 TO 連率－施術和暦年月.
055260     MOVE 連計－患者コード   TO 連率－患者コード.
055270*
055280     CALL   "HUTANRIT".
055290     CANCEL "HUTANRIT".
055300*
055310** / 老人レセの時は以下
055320     COMPUTE レセ－負担割合 = 連率－２７老負担率 / 10.
055330*
055340*================================================================*
055789*================================================================*
055790 部位実日数セット SECTION.
055791*
055792     MOVE ZERO      TO  実日数Ｗ.
055794*
055795     MOVE 連計－施術和暦 TO 負－施術和暦. 
055796     MOVE 連計－施術年   TO 負－施術年.   
055797     MOVE 連計－施術月   TO 負－施術月.   
055798     MOVE 連計－患者番号 TO 負－患者番号. 
055799     MOVE 連計－枝番     TO 負－枝番.     
055800     READ 負傷データＦ
055801     INVALID KEY
055802         MOVE SPACE   TO 負－レコード
055803         INITIALIZE      負－レコード
055804     END-READ.
055805**
055806     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 7
055807         MOVE ZERO TO 部位実日数Ｗ(部位カウンタ)
055808     END-PERFORM.
055809*
055810     MOVE 連計－患者番号        TO 施記－患者番号.
055820     MOVE 連計－枝番            TO 施記－枝番.
055830     MOVE 連計－施術和暦        TO 施記－施術和暦.
055840     MOVE 連計－施術年          TO 施記－施術年.
055850     MOVE 連計－施術月          TO 施記－施術月.
055862*
055863*---------------------------------------------------------------------------------------*
055864* 助成月途中変更
055865     IF ( 連計算助成月途中－助成のみ処理区分 NOT = 1 ) 
055866         MOVE ZERO              TO 施記－施術日
055868     ELSE
055869         MOVE 連計算助成月途中－助成月途中開始日  TO 施記－施術日
055871     END-IF.
055872*---------------------------------------------------------------------------------------*
055873*
055877     START 施術記録Ｆ   KEY IS >= 施記－患者コード
055880                                  施記－施術和暦年月日
055890     END-START.
055900     IF 状態キー = "00"
055910         MOVE SPACE TO 終了フラグ
055920         PERFORM 施術記録Ｆ読込
055971         PERFORM UNTIL ( 施記－施術和暦   NOT = 連計－施術和暦 ) OR
055972                       ( 施記－施術年     NOT = 連計－施術年   ) OR
055973                       ( 施記－施術月     NOT = 連計－施術月   ) OR
055974                       ( 施記－患者コード NOT = 連計－患者コード ) OR
055975                       ( 終了フラグ       NOT = SPACE )
055976*
055977             COMPUTE 実日数Ｗ = 実日数Ｗ + 1
055980*
055990             PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 負－部位数
056000                 IF 負－開始和暦年月日(部位カウンタ) <= 施記－施術和暦年月日 
056010                     IF (負－終了和暦年月日(部位カウンタ) >= 施記－施術和暦年月日) OR
056020                        (負－転帰区分      (部位カウンタ) = 9)
      */日毎部位毎に料金が発生しない場合は実日数に含めない↓↓↓/20160804
                               IF (施記－整復施療区分  (部位カウンタ) = ZERO) AND
                                  (施記－罨法区分      (部位カウンタ) = ZERO) AND
                                  (施記－電療区分      (部位カウンタ) = ZERO) AND
                                  (施記－金属副子区分  (部位カウンタ) = ZERO) AND
                                  (施記－情報提供区分  (部位カウンタ) = ZERO) AND
                                  (施記－後療料請求区分(部位カウンタ) = ZERO) AND
      */無病の場合は料金が発生しない場合も実日数に含める/20170515
                                  (負－負傷種別        (部位カウンタ) NOT = 09)
                                   CONTINUE
                               ELSE
      */日毎部位毎に料金が発生しない場合は実日数に含めない↑↑↑/20160804
056030                            COMPUTE 部位実日数Ｗ(部位カウンタ) = 部位実日数Ｗ(部位カウンタ) + 1
                               END-IF
056040                     END-IF
056050                 END-IF
056060             END-PERFORM
056070             PERFORM 施術記録Ｆ読込
056080         END-PERFORM
056090     END-IF.
HILO  *     DISPLAY "210-2 " 部位実日数Ｗ(1) " " 部位カウンタ "---------------------------"
056092*
056093** 上記後
056094*----------------------------------------------------------------------------*
056095     IF レセ－請求区分 = 9
056096        MOVE ZERO TO レセ－請求対象区分
056097     ELSE
056098        IF ( 実日数Ｗ NOT = ZERO ) AND ( 負－部位数 NOT = ZERO )
056099           MOVE 1    TO レセ－請求対象区分
056100        ELSE
056101           MOVE ZERO TO レセ－請求対象区分
056102        END-IF
056103     END-IF.
056104*----------------------------------------------------------------------------*
056105*
056106*================================================================*
056107 受診者柔整通院あり更新 SECTION.
056111*
056112* 実日数ありなし→受診者情報Ｆの柔整有区分を更新する。
056113* 0：柔整通院なし 1：柔整通院あり
056114*     05 柔整有区分                     PIC 9.
056116*
056119     MOVE 連計－施術和暦     TO 受－施術和暦.
056120     MOVE 連計－施術年       TO 受－施術年.
056121     MOVE 連計－施術月       TO 受－施術月.
056122     MOVE 連計－患者番号     TO 受－患者番号.
056123     MOVE 連計－枝番         TO 受－枝番.
056125     READ 受診者情報Ｆ
056127     NOT INVALID KEY
056129*
056130         IF 実日数Ｗ NOT = ZERO
056132            MOVE 1    TO 受－柔整有区分
056133         ELSE
056134            MOVE ZERO TO 受－柔整有区分
056136         END-IF
056137*
056140         REWRITE 受－レコード
056141         IF 状態キー NOT = "00"
056142            MOVE NC"受診者更新" TO ファイル名Ｗ
056143            PERFORM エラー表示
056144         END-IF
056145     END-READ.
056146*
056147*================================================================*
056148*================================================================*
056149 枝番負担累計額計算 SECTION.
056150*
056151*  受－枝番作成時枝番で、参照受診者情報Ｆを読み、同じ助成なら、
056152*  会－一部負担金を集計して、負担額助成ＧＷにセットしておく。
056153*
056156*
056157     OPEN INPUT 参照受診者情報Ｆ.
056158         MOVE NC"参照受診" TO ファイル名Ｗ.
056159         PERFORM オープンチェック.
056160*
056161     MOVE 連計－施術和暦        TO 参照受－施術和暦.
056162     MOVE 連計－施術年          TO 参照受－施術年.
056163     MOVE 連計－施術月          TO 参照受－施術月.
056164     MOVE 連計－患者番号        TO 参照受－患者番号.
056165*    / 受－枝番作成時枝番 /
056166     MOVE 受－枝番作成時枝番    TO 参照受－枝番.
056167*
056168     READ 参照受診者情報Ｆ
056169     NOT INVALID KEY
056170         IF ( 参照受－助成種別           = 受－助成種別 ) AND
056171            ( 参照受－費用負担者番号助成 = 受－費用負担者番号助成 ) AND
056172            ( 参照受－受益者番号助成     = 受－受益者番号助成 ) AND
056173            ( 参照受－助成負担金免除     = 受－助成負担金免除 )
056174*
056176               MOVE 参照受－患者コード     TO 会－患者コード
056177               MOVE 参照受－施術和暦       TO 会－施術和暦
056178               MOVE 参照受－施術年         TO 会－施術年
056179               MOVE 参照受－施術月         TO 会－施術月
056181*              / 助成月途中対応 /
056183               IF 参照受－助成月途中開始日 NOT = ZERO
056184                  MOVE 参照受－助成月途中開始日 TO 会－施術日
056185               ELSE
056187                  MOVE ZERO                     TO 会－施術日
056188               END-IF
056189*
056191               START 会計データＦ KEY IS >= 会－患者コード 会－施術和暦年月日
056192               END-START
056193               IF 状態キー = "00"
056194                   MOVE SPACE TO 終了フラグ
056195                   PERFORM 会計データＦ読込
056196                   PERFORM UNTIL ( 参照受－患者コード NOT = 会－患者コード ) OR
056197                                 ( 参照受－施術和暦   NOT = 会－施術和暦   ) OR
056198                                 ( 参照受－施術年     NOT = 会－施術年     ) OR
056199                                 ( 参照受－施術月     NOT = 会－施術月     ) OR
056200                                 ( 終了フラグ         NOT = SPACE            )
056201*
056202                       COMPUTE 枝番負担累計額Ｗ  = 枝番負担累計額Ｗ + 会－一部負担金
056203*
056204                       PERFORM 会計データＦ読込
056205                   END-PERFORM
056206*
056207***                / 負担額助成ＧＷに累計セット /
056208                   MOVE 枝番負担累計額Ｗ TO 負担額助成ＧＷ
056211***
056212               END-IF
056213*
056214         END-IF
056215     END-READ.
056216*
056217     CLOSE 参照受診者情報Ｆ.
      *================================================================*
056218*
       負傷データ取得 SECTION.
      *
           MOVE 連計－施術和暦 TO 負－施術和暦 .
           MOVE 連計－施術年   TO 負－施術年   .
           MOVE 連計－施術月   TO 負－施術月   .
           MOVE 連計－患者番号 TO 負－患者番号 .
           MOVE 連計－枝番     TO 負－枝番     .
           READ 負傷データＦ
           INVALID KEY
               DISPLAY NC"該当月の情報がありません" UPON CONS
               DISPLAY NC"施術和暦" 連計－施術和暦  UPON CONS
               DISPLAY NC"施術年"   連計－施術年    UPON CONS
               DISPLAY NC"施術月"   連計－施術月    UPON CONS
               DISPLAY NC"患者番号" 連計－患者番号  UPON CONS
               DISPLAY NC"枝番"     連計－枝番      UPON CONS
               MOVE NC"負傷" TO ファイル名Ｗ
               PERFORM エラー表示Ｒ
           NOT INVALID KEY
               MOVE 負－部位数 TO 部位数Ｗ
           END-READ.
056219*================================================================*
       会計施術者番号書込 SECTION.
      *
      */削除時に退避しておいた施術者番号を元に戻す
055990     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
               IF 施術者番号Ｗ(カウンタ) NOT = ZERO
045540             MOVE SPACE TO 会－レコード
045550             INITIALIZE    会－レコード
045570             MOVE 連計－患者コード     TO 会－患者コード
045580             MOVE 連計－施術和暦       TO 会－施術和暦
045590             MOVE 連計－施術年         TO 会－施術年
045600             MOVE 連計－施術月         TO 会－施術月
                   MOVE カウンタ TO 会－施術日
                   MOVE 施術者番号Ｗ(カウンタ) TO  会－施術者番号
048090             WRITE 会－レコード
048100             INVALID KEY
      */部位が無い場合は別の場所で書き込み済みの為CONTINUE↓↓↓/20230227
                       IF 状態キー = "22"
                           CONTINUE
                       ELSE
      */部位が無い場合は別の場所で書き込み済みの為CONTINUE↑↑↑/20230227
048110                 MOVE NC"会３" TO ファイル名Ｗ
048120                 PERFORM エラー表示
048130                 PERFORM 終了処理
048140                 EXIT PROGRAM
      */部位が無い場合は別の場所で書き込み済みの為CONTINUE/20230227
                       END-IF
048150             END-WRITE
               END-IF
           END-PERFORM.
056220*================================================================*
009510 施術所情報取得 SECTION.
009520*
009530     MOVE ZERO  TO 施情－施術所番号.
009540     READ 施術所情報マスタ
009550     INVALID KEY
009560          MOVE  NC"施術所情報マスタに登録後、実行して下さい" TO 連メ－メッセージ
009570          CALL   "MSG001"
009580          CANCEL "MSG001"
009590          PERFORM ファイル閉鎖
009600          MOVE 99 TO PROGRAM-STATUS
009610          EXIT PROGRAM
009620*     
009621     NOT INVALID KEY
009622          MOVE 施情－複数施術者使用区分 TO 複数施術者使用区分Ｗ
009623
009624     END-READ.
056221*================================================================*
       前回施術者取得 SECTION.
      *
           MOVE ZERO  TO 前回施術者番号Ｗ.
045540     MOVE SPACE TO 会－レコード.
045550     INITIALIZE    会－レコード.
045560     MOVE ZERO  TO 存在フラグ.
045570     MOVE 連計－患者コード     TO 会－患者コード.
045580     MOVE 連計－施術和暦       TO 会－施術和暦.
045590     MOVE 連計－施術年         TO 会－施術年.
045600     MOVE 連計－施術月         TO 会－施術月.
045610     MOVE 連計－施術日         TO 会－施術日.
045240     START 会計データＦ KEY IS < 会－患者コード 会－施術和暦年月日 REVERSED
045250     END-START.
045260     IF 状態キー = "00"
               IF 会－患者コード = 連計－患者コード
045280             PERFORM 会計データＦ読込
                   MOVE 会－施術者番号 TO 前回施術者番号Ｗ
               END-IF
           END-IF.
056221*================================================================*
       金属副子回数判定 SECTION.
      *
HILO  *     DISPLAY "金属副子回数判定 " 部位カウンタ２ " ---------------"
      *該当部位の金属副子の回数を数える。(部位コードと開始日が同じ部位を該当部位とする)
008580     MOVE 連計－施術和暦   TO 負－施術和暦.
008590     MOVE 連計－施術年     TO 負－施術年.
008600     MOVE 連計－施術月     TO 負－施術月.
008610     MOVE 連計－患者番号   TO 負－患者番号.
008620     MOVE 連計－枝番       TO 負－枝番.
008630     READ 負傷データＦ
008640     INVALID KEY
               MOVE NC"負傷" TO ファイル名Ｗ
               PERFORM エラー表示Ｒ
008660     NOT INVALID KEY
               MOVE 負－部位コード    (部位カウンタ２) TO 部位コードＷ
               MOVE 負－開始和暦年月日(部位カウンタ２) TO 開始和暦年月日２Ｗ
               MOVE 部位カウンタ２                     TO 部位カウンタ３
               MOVE SPACE TO 終了フラグ２
               MOVE ZERO  TO 金属副子ＣＮＴ
               MOVE 連計－患者コード     TO 施記－患者コード
               MOVE 連計－施術和暦年月日 TO 施記－施術和暦年月日 施術和暦年月日２Ｗ
008770         START 施術記録Ｆ   KEY IS < 施記－患者コード
008780                                     施記－施術和暦年月日
                                           REVERSED
008790         END-START
008800         IF 状態キー = "00"
008810             MOVE SPACE TO 終了フラグ２
008830             PERFORM 施術記録Ｆ読込２
                   PERFORM UNTIL (連計－患者コード NOT = 施記－患者コード  ) OR 
                                 (施記－施術和暦年月日 < 開始和暦年月日２Ｗ) OR
                                 (終了フラグ２     NOT = SPACE)
                       IF 施記－施術和暦年月 NOT = 施術和暦年月２Ｗ
                           PERFORM 金属月跨処理
                           MOVE 施記－施術和暦年月日 TO 施術和暦年月日２Ｗ
                       END-IF
                       IF 部位カウンタ３ NOT = ZERO
                           IF 施記－金属副子区分(部位カウンタ３) NOT = ZERO
                               COMPUTE 金属副子ＣＮＴ = 金属副子ＣＮＴ + 1
                           END-IF
                       END-IF
008830                 PERFORM 施術記録Ｆ読込２
                   END-PERFORM
               END-IF
           END-READ.
      *
           MOVE 連計－患者コード     TO 施記－患者コード.
           MOVE 連計－施術和暦年月日 TO 施記－施術和暦年月日.
008770     START 施術記録Ｆ   KEY IS >= 施記－患者コード
008780                                  施記－施術和暦年月日
008790     END-START.
008800     IF 状態キー = "00"
008810         MOVE SPACE TO 終了フラグ
008830         PERFORM 施術記録Ｆ読込２
           END-IF.
HILO  *     DISPLAY "金属副子回数 " 金属副子ＣＮＴ.
009040*================================================================*
       施術記録Ｆ読込２ SECTION.
      *
           READ 施術記録Ｆ NEXT
           AT END
               MOVE "YES" TO 終了フラグ２
           END-READ.
      *
      *================================================================*
       金属月跨処理 SECTION.
      *
HILO  *     DISPLAY "金属月跨処理"
008580     MOVE 施記－施術和暦   TO 負－施術和暦.
008590     MOVE 施記－施術年     TO 負－施術年.
008600     MOVE 施記－施術月     TO 負－施術月.
008610     MOVE 施記－患者番号   TO 負－患者番号.
008620     MOVE 施記－枝番       TO 負－枝番.
008630     READ 負傷データＦ
008640     INVALID KEY
               MOVE NC"負傷" TO ファイル名Ｗ
               PERFORM エラー表示Ｒ
008660     NOT INVALID KEY
           MOVE ZERO TO 部位カウンタ３
               PERFORM VARYING 部位カウンタ４ FROM 1 BY 1 UNTIL  部位カウンタ４ > 負－部位数
                   IF (負－部位コード    (部位カウンタ４) = 部位コードＷ      ) AND
                      (負－開始和暦年月日(部位カウンタ４) = 開始和暦年月日２Ｗ)
                       MOVE 部位カウンタ４ TO 部位カウンタ３
                   END-IF
               END-PERFORM
           END-READ.
      *================================================================*
       旧運動療法料算定 SECTION.
      *
           MOVE ZERO TO 運動療法料Ｗ.
      *
           MOVE 連計－施術和暦 TO 負－施術和暦.
           MOVE 連計－施術年   TO 負－施術年.
           MOVE 連計－施術月   TO 負－施術月.
           MOVE 連計－患者番号 TO 負－患者番号.
           MOVE 連計－枝番     TO 負－枝番.
           READ 負傷データＦ
           INVALID KEY
               PERFORM コンソールエラー確認
               DISPLAY "指定の受信者の負傷データがありません"
                       " 受診者№=" 連計－患者コード
                       " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
               PERFORM ファイル閉鎖
               MOVE ZERO TO PROGRAM-STATUS
               EXIT PROGRAM
           NOT INVALID KEY
               PERFORM VARYING 部位カウンタ４ FROM 1 BY 1 UNTIL  部位カウンタ４ > 負－部位数
                   IF ( 施記－負傷種別(部位カウンタ４) = 04 OR 05 OR 06 OR 07 OR 08)
                       IF (施記－施術和暦年月日 >= 負－開始和暦年月日(部位カウンタ４)) AND
                          (施記－施術和暦年月日 <= 負－終了和暦年月日(部位カウンタ４) OR (負－終了和暦年月日(部位カウンタ４) = ZERO))
                           IF (施記－運動療法区分 = 1) AND (施記－診療区分 = 1)
                               COMPUTE 運動療法回数Ｗ = 運動療法回数Ｗ + 1
                               MOVE 運動療法料ＡＷ TO 運動療法料Ｗ
HILO  *                         DISPLAY 施記－施術和暦年月日 "運動 Ｏ"
                           END-IF
                           MOVE 負－部位数 TO 部位カウンタ４
HILO  *                 ELSE    DISPLAY 施記－施術和暦年月日 "運動 Ｘ"
                       END-IF
                   END-IF
               END-PERFORM
           END-READ.
      *================================================================*
       運動療法料算定 SECTION.
      *
           MOVE ZERO TO 運動療法料Ｗ.
      *
           IF 施記－運動療法区分 = 1
               MOVE 連計－施術和暦 TO 負－施術和暦
               MOVE 連計－施術年   TO 負－施術年
               MOVE 連計－施術月   TO 負－施術月
               MOVE 連計－患者番号 TO 負－患者番号
               MOVE 連計－枝番     TO 負－枝番
               READ 負傷データＦ
               INVALID KEY
                   PERFORM コンソールエラー確認
                   DISPLAY "指定の受信者の負傷データがありません"
                           " 受診者№=" 連計－患者コード
                           " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
                   PERFORM ファイル閉鎖
                   MOVE ZERO TO PROGRAM-STATUS
                   EXIT PROGRAM
               NOT INVALID KEY
                   PERFORM VARYING 部位カウンタ４ FROM 1 BY 1 UNTIL  部位カウンタ４ > 負－部位数
                       IF ( 施記－負傷種別(部位カウンタ４) = 04 OR 05 OR 06 OR 07 OR 08) AND
                          ( 施記－後療料請求区分(部位カウンタ４) = 1 ) AND
                          ((負－負傷和暦年月(部位カウンタ４)     = 連計－施術和暦年月) AND (負－負傷日(部位カウンタ４) < 16) OR 
                          ( 負－負傷和暦年月(部位カウンタ４) NOT = 連計－施術和暦年月))
                           PERFORM 経過日数判定
                           IF 負傷経過日Ｗ > 15 
HILO  *                         DISPLAY "施術日" 施記－施術日 " 部位" 部位カウンタ４ " 上限" 運動月上限回数Ｗ(部位カウンタ４) " 経過" 負傷経過日Ｗ
                               IF 運動後療回数Ｗ < 運動月上限回数Ｗ(部位カウンタ４)
                                   COMPUTE 運動療法回数Ｗ = 運動療法回数Ｗ + 1
                                   MOVE 運動療法料ＡＷ TO 運動療法料Ｗ
                                   COMPUTE 運動後療回数Ｗ = 運動後療回数Ｗ + 1
                                   MOVE 施記－施術日 TO 運動算定日Ｗ(運動後療回数Ｗ)
                                   MOVE 負－部位数 TO 部位カウンタ４
                               END-IF
                           END-IF
                        END-IF
                   END-PERFORM
               END-READ
      */算定出来ない日は運動療法区分をクリアする
               IF 運動療法料Ｗ = ZERO
                   MOVE ZERO TO 施記－運動療法区分
                   PERFORM 施術記録Ｆ書込
HILO  *             DISPLAY 施記－施術和暦年月日 "運動 Ｘ" ELSE DISPLAY 施記－施術和暦年月日 "運動 Ｏ"
               END-IF
           END-IF.
      *================================================================*
       経過日数判定 SECTION.
      *
      */負傷日からの経過日数を判定する
006390     MOVE 負－負傷和暦(部位カウンタ４) TO 計算和暦Ｗ.
006400     MOVE 負－負傷年  (部位カウンタ４) TO 計算年Ｗ.
006410     MOVE 負－負傷月  (部位カウンタ４) TO 計算月Ｗ.
006420     MOVE 負－負傷日  (部位カウンタ４) TO 計算日Ｗ.
006430     PERFORM 西暦年取得.
006440     PERFORM 日付絶対値計算.
006450     MOVE 絶対値Ｗ                 TO 負傷絶対値Ｗ.
006460*
006470     MOVE 施記－施術和暦           TO 計算和暦Ｗ.
006480     MOVE 施記－施術年             TO 計算年Ｗ.
006490     MOVE 施記－施術月             TO 計算月Ｗ.
006500     MOVE 施記－施術日             TO 計算日Ｗ.
006510     PERFORM 西暦年取得.
006520     PERFORM 日付絶対値計算.
006530     MOVE 絶対値Ｗ                 TO 施術絶対値Ｗ.
006540*
006550     COMPUTE 負傷経過日Ｗ = 施術絶対値Ｗ - ( 負傷絶対値Ｗ - 1 ).
      *================================================================*
007540*================================================================*
007550 西暦年取得 SECTION.
007560*
007570     MOVE 計算和暦Ｗ TO 元－元号区分.
007580     READ 元号マスタ
007590     INVALID KEY
007600         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007610         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007620                                                  UPON CONS
007630         ACCEPT  キー入力 FROM CONS
007640         PERFORM 終了処理
007650         EXIT PROGRAM
007660     NOT INVALID KEY
007670         COMPUTE 西暦年Ｗ = 元－開始西暦年 + 計算年Ｗ - 1
007680     END-READ.
007690*
007700*================================================================*
007710 日付絶対値計算 SECTION.
007720*
007730** 1989年を基準とし、当年まで閏年が何回あるかを計算
007740** 元号マスタの西暦開始年確認。
007750*     COMPUTE 西暦年差Ｗ   = 西暦年Ｗ - 1989 - 1.
007760*     DIVIDE 4 INTO 西暦年差Ｗ GIVING 閏年回数Ｗ
007770*                            REMAINDER 剰余Ｗ
007780*     END-DIVIDE.
007790*/基準年1989年から当年までの閏年の回数を計算(当年は含まない)
007800*/始めのうるう年の1992年から数える
007810     COMPUTE 西暦年差Ｗ   = 西暦年Ｗ - 1989 - 1.
007820     MOVE 1992 TO 加算年Ｗ.
007830     MOVE ZERO TO 閏年回数Ｗ.
007840     PERFORM UNTIL 西暦年Ｗ <= 加算年Ｗ
007850         COMPUTE 加算年Ｗ   = 加算年Ｗ   + 4
007860         COMPUTE 閏年回数Ｗ = 閏年回数Ｗ + 1
007870     END-PERFORM.
007880*
007890     COMPUTE 通常年回数Ｗ     = 西暦年差Ｗ     - 閏年回数Ｗ.
007900     COMPUTE 通常年絶対値Ｗ   = 通常年回数Ｗ   * 365.
007910     COMPUTE 閏年絶対値Ｗ     = 閏年回数Ｗ     * 366.
007920     COMPUTE 年絶対値Ｗ       = 通常年絶対値Ｗ + 閏年絶対値Ｗ.
007930* 当年がうるう年かどうかを調べる（２月の末日計算）
007940*     COMPUTE 西暦年差２Ｗ     = 西暦年Ｗ + 1.
007950*     DIVIDE 4 INTO 西暦年差２Ｗ GIVING 閏年回数Ｗ
007960*                                REMAINDER 剰余Ｗ
007970*     END-DIVIDE.
007980     DIVIDE 4 INTO 西暦年Ｗ GIVING 閏年回数Ｗ
007990                            REMAINDER 剰余Ｗ
008000     END-DIVIDE.
008010*
008020     MOVE ZERO TO 絶対値月Ｗ.
008030*
008040* 前月までの絶対値を累計
008050     COMPUTE 絶対値月Ｗ２ = 計算月Ｗ * 30.
008060*
008070     EVALUATE 計算月Ｗ
008080     WHEN 3
008090         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ - 1
008100     WHEN 2
008110     WHEN 6
008120     WHEN 7
008130         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 1
008140     WHEN 8
008150         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 2
008160     WHEN 9
008170     WHEN 10
008180         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 3
008190     WHEN 11
008200     WHEN 12
008210         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 4
008220     WHEN OTHER
008230         MOVE 絶対値月Ｗ２ TO 絶対値月Ｗ
008240     END-EVALUATE.
008250*
008260     IF ( 剰余Ｗ = ZERO ) AND
008270        ( 計算月Ｗ > 2  )
008280         COMPUTE 絶対値月Ｗ = 絶対値月Ｗ + 1
008290     END-IF.
008300*
008310     COMPUTE 絶対値Ｗ   = 年絶対値Ｗ + 絶対値月Ｗ + 計算日Ｗ.
008320*
008330*================================================================*
       運動上限算定 SECTION.
      *
           MOVE ZERO TO 運動後療Ｗ.
           INITIALIZE   運動後療Ｗ.
           MOVE 連計－施術和暦 TO 負－施術和暦.
           MOVE 連計－施術年   TO 負－施術年.
           MOVE 連計－施術月   TO 負－施術月.
           MOVE 連計－患者番号 TO 負－患者番号.
           MOVE 連計－枝番     TO 負－枝番.
           READ 負傷データＦ
           INVALID KEY
               PERFORM コンソールエラー確認
               DISPLAY "指定の受信者の負傷データがありません"
                       " 受診者№=" 連計－患者コード
                       " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
               PERFORM ファイル閉鎖
               MOVE ZERO TO PROGRAM-STATUS
               EXIT PROGRAM
           NOT INVALID KEY
               PERFORM VARYING 部位カウンタ４ FROM 1 BY 1 UNTIL  部位カウンタ４ > 負－部位数
                   MOVE 負－施術和暦 TO 施記－施術和暦 施術和暦ＣＷ
                   MOVE 負－施術年   TO 施記－施術年   施術年ＣＷ
                   MOVE 負－施術月   TO 施記－施術月   施術月ＣＷ
                   MOVE 99           TO 施記－施術日   施術日ＣＷ
                   MOVE 負－患者番号 TO 施記－患者番号 患者コードＣＷ
                   MOVE 負－枝番     TO 施記－枝番     枝番ＣＷ
                   START 施術記録Ｆ KEY IS <= 施記－患者コード 施記－施術和暦年月日 REVERSED
                   END-START
                   IF 状態キー = "00"
                       MOVE SPACE TO 終了フラグ
                       PERFORM 施術記録Ｆ読込
                       PERFORM UNTIL ( 施記－患者コード   NOT = 患者コードＣＷ   ) OR
                                     ( 施記－施術和暦年月 NOT = 施術和暦年月ＣＷ ) OR
                                     ( 終了フラグ         NOT = SPACE )
                           IF ( 施記－負傷種別      (部位カウンタ４) = 04 OR 05 OR 06 OR 07 OR 08) AND
                              ( 施記－後療料請求区分(部位カウンタ４) = 1)
                               IF 施記－施術日 < 16
                                   MOVE 2 TO 運動月上限回数Ｗ(部位カウンタ４)
                               ELSE
                                   MOVE 5 TO 運動月上限回数Ｗ(部位カウンタ４)
                               END-IF
                               MOVE "YES" TO 終了フラグ
                           END-IF
                           PERFORM 施術記録Ｆ読込
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-READ.
HILO  *     PERFORM VARYING 部位カウンタ４ FROM 1 BY 1 UNTIL  部位カウンタ４ > 負－部位数
HILO  *     DISPLAY "上限(" 部位カウンタ４ ") " 運動月上限回数Ｗ(部位カウンタ４) END-PERFORM.
008330*================================================================*
       施術記録Ｆ書込 SECTION.
      *
HILO  *     DISPLAY "R-1 クリア"
           MOVE 施記－施術和暦年月日 TO 施術和暦年月日Ｗ.
           MOVE 施記－患者コード     TO 患者コードＷ.
           CLOSE 施術記録Ｆ.
025850     PERFORM 遅延処理.
      */運動後療更新の為/20180526
           OPEN I-O 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
           MOVE 施術和暦年月日Ｗ TO 施記－施術和暦年月日
           MOVE 患者コードＷ     TO 施記－患者コード 
           READ 施術記録Ｆ
008840     INVALID KEY
008850         MOVE NC"施記" TO ファイル名Ｗ
008860         PERFORM エラー表示Ｒ
           END-READ.
           MOVE ZERO TO 施記－運動療法区分.
025850     PERFORM 遅延処理.
      *
008760     WRITE 施記－レコード
008770     END-WRITE.
008780*
008790     EVALUATE 状態キー
008800     WHEN "00"
008810         CONTINUE
008820     WHEN "22"
008830         REWRITE 施記－レコード
008840         INVALID KEY
008850             MOVE NC"施記" TO ファイル名Ｗ
008860             PERFORM エラー表示
008870         END-REWRITE
008880     WHEN OTHER
008890         MOVE NC"施記" TO ファイル名Ｗ
008900         PERFORM エラー表示
008910     END-EVALUATE.
008920*
           CLOSE 施術記録Ｆ.
025850     PERFORM 遅延処理.
      */運動後療更新の為/20180526
           OPEN INPUT 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
           MOVE 施術和暦年月日Ｗ TO 施記－施術和暦年月日.
           MOVE 患者コードＷ     TO 施記－患者コード.
014310     START 施術記録Ｆ KEY IS >= 施記－患者コード 施記－施術和暦年月日
014320     END-START.
014330*
014340     IF 状態キー = "00"
014410         PERFORM 施術記録Ｆ読込
           END-IF.
008930*================================================================*
025880 遅延処理 SECTION.
025890*
025900     PERFORM VARYING 遅延カウンタ FROM 1 BY 1
025910             UNTIL 遅延カウンタ > 遅延回数Ｗ
025920         MOVE "YES" TO 遅延フラグ
025930     END-PERFORM.
025940*
025950*================================================================*
       金属副子区分クリア SECTION.
      *
HILO  *     DISPLAY "R-1 クリア"
           MOVE 施記－施術和暦年月日 TO 施術和暦年月日Ｗ.
           MOVE 施記－患者コード     TO 患者コードＷ.
           CLOSE 施術記録Ｆ.
025850     PERFORM 遅延処理.
      */運動後療更新の為/20180526
           OPEN I-O 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
           MOVE 施術和暦年月日Ｗ TO 施記－施術和暦年月日
           MOVE 患者コードＷ     TO 施記－患者コード 
           READ 施術記録Ｆ
008840     INVALID KEY
008850         MOVE NC"施記" TO ファイル名Ｗ
008860         PERFORM エラー表示Ｒ
           END-READ.
           MOVE ZERO TO 施記－金属副子区分(部位カウンタ２).
025850     PERFORM 遅延処理.
      *
008760     WRITE 施記－レコード
008770     END-WRITE.
008780*
008790     EVALUATE 状態キー
008800     WHEN "00"
008810         CONTINUE
008820     WHEN "22"
008830         REWRITE 施記－レコード
008840         INVALID KEY
008850             MOVE NC"施記" TO ファイル名Ｗ
008860             PERFORM エラー表示
008870         END-REWRITE
008880     WHEN OTHER
008890         MOVE NC"施記" TO ファイル名Ｗ
008900         PERFORM エラー表示
008910     END-EVALUATE.
008920*
           CLOSE 施術記録Ｆ.
025850     PERFORM 遅延処理.
      */運動後療更新の為/20180526
           OPEN INPUT 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
           MOVE 施術和暦年月日Ｗ TO 施記－施術和暦年月日.
           MOVE 患者コードＷ     TO 施記－患者コード.
014310     START 施術記録Ｆ KEY IS >= 施記－患者コード 施記－施術和暦年月日
014320     END-START.
014330*
014340     IF 状態キー = "00"
014410         PERFORM 施術記録Ｆ読込
           END-IF.
025950**================================================================*
      * 旧明細書発行加算料算定 SECTION.
      **
019310**     MOVE ZERO TO 明細書発行加算料Ｗ.
019370**     IF  施記－明細書発行加算区分 = 1
019400**         COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
      **     END-IF.
019310*     MOVE ZERO TO 明細書発行加算料Ｗ.
      **
019370*     IF  施記－明細書発行加算区分 = 1
030780*         IF 連計－施術和暦年月日 >= 5040601
033730*             MOVE 連計－施術和暦 TO ＥＸ施記－施術和暦
033740*             MOVE 連計－施術年   TO ＥＸ施記－施術年
033750*             MOVE 連計－施術月   TO ＥＸ施記－施術月
033760*             MOVE 連計－施術日   TO ＥＸ施記－施術日
033770*             MOVE 連計－患者番号 TO ＥＸ施記－患者番号
033780*             MOVE 連計－枝番     TO ＥＸ施記－枝番
033790*             START ＥＸ施術記録Ｆ KEY IS < ＥＸ施記－患者コード
033800*                                           ＥＸ施記－施術和暦年月日
033810*                                           REVERSED
033820*             END-START
033830**
033840*             IF 状態キー = "00"
033850*                 READ ＥＸ施術記録Ｆ NEXT
033860*                 AT END
033900*                     MOVE 1 TO 施記－明細書発行加算区分
019400*                     COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
033860*                 NOT AT END
033880*                     IF (連計－患者コード NOT = ＥＸ施記－患者コード) OR
033890*                        (連計－施術和暦   NOT = ＥＸ施記－施術和暦  ) OR
033890*                        (連計－施術年     NOT = ＥＸ施記－施術年    ) OR
033890*                        (連計－施術月     NOT = ＥＸ施記－施術月    )
033900*                         MOVE 1    TO 施記－明細書発行加算区分
019400*                         COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
      *                     ELSE
033900*                         MOVE ZERO TO 施記－明細書発行加算区分
033910*                     END-IF
033920*                 END-READ
033930*             END-IF
      * 
HILO  **/毎日算定する場合
033900**             MOVE 1    TO 施記－明細書発行加算区分
      *         ELSE
033900*             MOVE ZERO TO 施記－明細書発行加算区分
033930*         END-IF
      *         IF 明細書発行加算料Ｗ = ZERO
033900*             MOVE ZERO TO 施記－明細書発行加算区分
HILO  **             DISPLAY "210-1 " 施記－レコードキー
      *             PERFORM 明細書発行加算区分クリア
HILO  **             DISPLAY 施記－施術和暦年月日 "明細 Ｘ" ELSE DISPLAY 施記－施術和暦年月日 "明細 Ｏ"
      *         END-IF
033930*     END-IF.
025950**================================================================*
008330*================================================================*
       明細書発行加算区分クリア SECTION.
      *
HILO  *     DISPLAY "R-1 クリア " 施記－施術和暦年月日
           MOVE 施記－施術和暦年月日 TO 施術和暦年月日Ｗ.
           MOVE 施記－患者コード     TO 患者コードＷ.
           CLOSE 施術記録Ｆ.
025850     PERFORM 遅延処理.
      */運動後療更新の為/20180526
           OPEN I-O 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
           MOVE 施術和暦年月日Ｗ TO 施記－施術和暦年月日
           MOVE 患者コードＷ     TO 施記－患者コード 
           READ 施術記録Ｆ
008840     INVALID KEY
008850         MOVE NC"施記" TO ファイル名Ｗ
008860         PERFORM エラー表示Ｒ
           END-READ.
033900     MOVE ZERO TO 施記－明細書発行加算区分.
025850     PERFORM 遅延処理.
      *
008760     WRITE 施記－レコード
008770     END-WRITE.
008780*
008790     EVALUATE 状態キー
008800     WHEN "00"
008810         CONTINUE
008820     WHEN "22"
008830         REWRITE 施記－レコード
008840         INVALID KEY
008850             MOVE NC"施記" TO ファイル名Ｗ
008860             PERFORM エラー表示
008870         END-REWRITE
008880     WHEN OTHER
008890         MOVE NC"施記" TO ファイル名Ｗ
008900         PERFORM エラー表示
008910     END-EVALUATE.
008920*
           CLOSE 施術記録Ｆ.
025850     PERFORM 遅延処理.
      *
           OPEN INPUT 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
           MOVE 施術和暦年月日Ｗ TO 施記－施術和暦年月日.
           MOVE 患者コードＷ     TO 施記－患者コード.
014310     START 施術記録Ｆ KEY IS >= 施記－患者コード 施記－施術和暦年月日
014320     END-START.
014330*
014340     IF 状態キー = "00"
014410         PERFORM 施術記録Ｆ読込
           END-IF.
008930*================================================================*
025950**================================================================*
      * 旧明細書発行加算料算定 SECTION.
      **
019310**     MOVE ZERO TO 明細書発行加算料Ｗ.
019370**     IF  施記－明細書発行加算区分 = 1
019400**         COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
      **     END-IF.
019310*     MOVE ZERO TO 明細書発行加算料Ｗ.
      **
019370*     IF  施記－明細書発行加算区分 = 1
030780*         IF 連計－施術和暦年月日 >= 5041001
033730*             MOVE 連計－施術和暦 TO 確認施記－施術和暦
033740*             MOVE 連計－施術年   TO 確認施記－施術年
033750*             MOVE 連計－施術月   TO 確認施記－施術月
033760*             MOVE 連計－施術日   TO 確認施記－施術日
033770*             MOVE 連計－患者番号 TO 確認施記－患者番号
033780*             MOVE 連計－枝番     TO 確認施記－枝番
033790*             START 確認施術記録Ｆ KEY IS < 確認施記－患者コード
033800*                                           確認施記－施術和暦年月日
033810*                                           REVERSED
033820*             END-START
033830**
033840*             IF 状態キー = "00"
019230*                 MOVE SPACE TO 終了フラグ３
019230*                 MOVE ZERO  TO 明細書発行加算回数Ｗ
033850*                 PERFORM 確認施術記録Ｆ読込
033880*                 PERFORM UNTIL (連計－患者コード NOT = 確認施記－患者コード) OR
033890*                               (連計－施術和暦   NOT = 確認施記－施術和暦  ) OR
033890*                               (連計－施術年     NOT = 確認施記－施術年    ) OR
033890*                               (連計－施術月     NOT = 確認施記－施術月    ) OR
      *                               (終了フラグ３     NOT = SPACE)
019370*                     IF 確認施記－明細書発行加算区分 = 1
HILO  **                         DISPLAY "210-3 " 確認施記－レコードキー
019230*                         MOVE "YES" TO 終了フラグ３
      *                         COMPUTE 明細書発行加算回数Ｗ = 明細書発行加算回数Ｗ + 1
      *                     END-IF
033850*                     PERFORM 確認施術記録Ｆ読込
      *                END-PERFORM
HILO  **                DISPLAY "明細書発行加算回数Ｗ " 明細書発行加算回数Ｗ
      **/当月初回のみ算定
      *                IF 明細書発行加算回数Ｗ = ZERO
019400*                    COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
      *                    MOVE 施記－施術日 TO 明細書発行加算日Ｗ
      *                END-IF
033930*             END-IF
033930*         END-IF
      *         IF 明細書発行加算料Ｗ = ZERO
033900*             MOVE ZERO TO 施記－明細書発行加算区分
HILO  **             DISPLAY "210-1 " 施記－レコードキー
      *             PERFORM 明細書発行加算区分クリア
HILO  **             DISPLAY 施記－施術和暦年月日 "明細 Ｘ2" ELSE DISPLAY 施記－施術和暦年月日 "明細 Ｏ"
      *         END-IF
033930*     END-IF.
025950**================================================================*
025950*================================================================*
019190 確認施術記録Ｆ読込 SECTION.
019200*
019210     READ 確認施術記録Ｆ NEXT
019220     AT END
019230         MOVE "YES" TO 終了フラグ３
019240     END-READ.
019250*
019260*================================================================*
       明細書発行加算料算定 SECTION.
      *
HILO  *     DISPLAY "210-2 明細書発行加算料算定 " 連計－施術和暦年月日 " " 施記－明細書発行加算区分
056157     OPEN INPUT 参照受診者情報Ｆ.
056158         MOVE NC"参照受診" TO ファイル名Ｗ.
056159         PERFORM オープンチェック.
019310     MOVE ZERO TO 明細書発行加算料Ｗ.
      *
019370     IF 施記－明細書発行加算区分 = 1
030780         IF 連計－施術和暦年月日 >= 5041001
017390             MOVE 連計－施術和暦 TO 参照受－施術和暦
017400             MOVE 連計－施術年   TO 参照受－施術年
017410             MOVE 連計－施術月   TO 参照受－施術月
017420             MOVE 連計－患者番号 TO 参照受－患者番号
017440             MOVE HIGH-VALUE     TO 参照受－枝番
017480* 
017490             START 参照受診者情報Ｆ KEY IS < 参照受－施術和暦年月 参照受－患者コード REVERSED
017500             END-START
017510             IF 状態キー = "00"
                       MOVE SPACE TO 終了フラグ４
                       PERFORM 参照受診者情報Ｆ読込
                       PERFORM UNTIL (連計－施術和暦 NOT = 参照受－施術和暦) OR
                                     (連計－施術年月 NOT = 参照受－施術年月) OR
                                     (連計－患者番号 NOT = 参照受－患者番号) OR
                                     (終了フラグ４   NOT = SPACE           )
HILO  *                 DISPLAY "210-3 " 参照受－レコードキー
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↓↓↓/160706
                           IF 受－枝番 NOT = "x" AND "h" AND "a" AND "r"
033730                         MOVE 連計－施術和暦 TO 確認施記－施術和暦
033740                         MOVE 連計－施術年   TO 確認施記－施術年
033750                         MOVE 連計－施術月   TO 確認施記－施術月
033760                         MOVE 連計－施術日   TO 確認施記－施術日
033770                         MOVE 連計－患者番号 TO 確認施記－患者番号
033780                         MOVE 参照受－枝番   TO 確認施記－枝番
033790                         START 確認施術記録Ｆ KEY IS < 確認施記－患者コード
033800                                                       確認施記－施術和暦年月日
033810                                                       REVERSED
033820                         END-START
033830*
033840                         IF 状態キー = "00"
019230                             MOVE SPACE TO 終了フラグ３
019230                             MOVE ZERO  TO 明細書発行加算回数Ｗ 明細書発行加算料Ｗ
033850                             PERFORM 確認施術記録Ｆ読込
HILO  *                         DISPLAY "210-7 参照受－" 参照受－レコードキー " " 確認施記－レコードキー
033880*                             PERFORM UNTIL (連計－患者コード NOT = 確認施記－患者コード) OR
033880                             PERFORM UNTIL (参照受－患者コード NOT = 確認施記－患者コード) OR
033890                                           (連計－施術和暦     NOT = 確認施記－施術和暦  ) OR
033890                                           (連計－施術年       NOT = 確認施記－施術年    ) OR
033890                                           (連計－施術月       NOT = 確認施記－施術月    ) OR
                                                 (終了フラグ３       NOT = SPACE)
019370                                 IF 確認施記－明細書発行加算区分 = 1
019230                                     MOVE "YES" TO 終了フラグ３
                                           COMPUTE 明細書発行加算回数Ｗ = 明細書発行加算回数Ｗ + 1
HILO  *                                     DISPLAY "210-9 " 確認施記－レコードキー " " 明細書発行加算回数Ｗ
                                       END-IF
033850                                 PERFORM 確認施術記録Ｆ読込
                                  END-PERFORM
      */当月初回のみ算定
                                  IF 明細書発行加算回数Ｗ = ZERO
019400                                COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
                                      MOVE 施記－施術日 TO 明細書発行加算日Ｗ
                                  END-IF
      */患者コード1番初回月の対応↓↓↓/20221108
                               ELSE
                                   IF 状態キー = "23"
                                       IF (明細書発行加算回数Ｗ = ZERO) AND (明細書発行加算料Ｗ = ZERO)
019400                                     COMPUTE 明細書発行加算料Ｗ = 明細書発行加算料Ｗ + 明細書発行加算料ＡＷ
                                           MOVE 施記－施術日 TO 明細書発行加算日Ｗ
                                       END-IF
                                   END-IF
      */患者コード1番初回月の対応↑↑↑/20221108
033930                         END-IF
                           END-IF
HILO  *                     DISPLAY "210-10 明細書発行加算料Ｗ " 明細書発行加算料Ｗ
                           IF 明細書発行加算料Ｗ = ZERO
033900                         MOVE ZERO TO 施記－明細書発行加算区分
HILO  *                         DISPLAY "210-1 クリア " 施記－レコードキー
                               PERFORM 明細書発行加算区分クリア
HILO  *                         DISPLAY 施記－施術和暦年月日 "明細 Ｘ2" ELSE DISPLAY 施記－施術和暦年月日 "明細 Ｏ"
                           END-IF
      *
                           PERFORM 参照受診者情報Ｆ読込
                       END-PERFORM
                   END-IF
      *
033930     END-IF.
056157     CLOSE 参照受診者情報Ｆ.
025950*================================================================*
017890 参照受診者情報Ｆ読込 SECTION.
017900*
017910     READ 参照受診者情報Ｆ NEXT
017920     AT END
017930         MOVE "YES" TO 終了フラグ４
017940     END-READ.
025950*================================================================*
       参照負傷データＦ読込 SECTION.
045450*
045460     READ 参照負傷データＦ NEXT
045470     AT END
045480         MOVE SPACE TO 参照負－レコード
045490         MOVE "YES" TO 終了フラグ５
045500     END-READ.
025950*================================================================*
017710 前月判定 SECTION.
017720* 

HILO  *     DISPLAY "工事中 GETUJIK 参照".

017730*** 読み込んだ負傷データの年月が、前月かどうか判定 (年月の差が 1 か?)
017740      MOVE  SPACE  TO  前月フラグ.
017750      INITIALIZE 計算年月日３Ｗ.
017750*      INITIALIZE  計算年月日３Ｗ 開始年月日３Ｗ 終了年月日３Ｗ.
017760***
017770*      MOVE 連月－施術和暦  TO 終了和暦３Ｗ.
017780*      MOVE 連月－施術年    TO 終了年３Ｗ.
017790*      MOVE 連月－施術月    TO 終了月３Ｗ.
017800*      MOVE 負－施術和暦    TO 開始和暦３Ｗ.
017810*      MOVE 負－施術年      TO 開始年３Ｗ.
017820*      MOVE 負－施術月      TO 開始月３Ｗ.
017830*
017840      EVALUATE TRUE
017850       WHEN (開始和暦３Ｗ = 終了和暦３Ｗ) AND (開始年３Ｗ = 終了年３Ｗ)
017860            PERFORM  前月比較月
017870       WHEN (開始和暦３Ｗ = 終了和暦３Ｗ) AND (開始年３Ｗ NOT = 終了年３Ｗ)
017880            PERFORM  前月比較年
017890       WHEN  開始和暦３Ｗ NOT = 終了和暦３Ｗ 
017900            PERFORM  前月比較元号
017910      END-EVALUATE.
017920*
017930*--- 元号日付チェック ---*
017940*****      IF 計算月Ｗ = 1
017950*****         MOVE  "YES"  TO  前月フラグ
017960*****         PERFORM データチェック
017970******      END-IF.
017980      IF ( 計算月３Ｗ = 1 )
017990         MOVE  "YES"  TO  前月フラグ
018000         IF ( 開始和暦３Ｗ NOT = 終了和暦３Ｗ )
018010            MOVE "YES"  TO  実行キーＷ
018020         ELSE
018030            PERFORM データチェック
018040         END-IF
018050      END-IF.
018060*
018070*================================================================*
018080 前月比較月  SECTION.
018090*
018100     IF  終了月３Ｗ >  開始月３Ｗ
018110         COMPUTE 計算月３Ｗ = 終了月３Ｗ - 開始月３Ｗ
018120     ELSE
018130        MOVE ZERO TO 計算月３Ｗ
018140     END-IF.
018150*
018160*================================================================*
018170 前月比較年  SECTION.
018180*
018190     IF  終了年３Ｗ >  開始年３Ｗ
018200         COMPUTE 計算年３Ｗ = 終了年３Ｗ - 開始年３Ｗ
018210         COMPUTE 計算月３Ｗ = (計算年３Ｗ * 12 + 終了月３Ｗ) - 開始月３Ｗ
018220     ELSE
018230        MOVE ZERO TO 計算月３Ｗ
018240     END-IF.
018250*
018260*================================================================*
018270 前月比較元号  SECTION.
018280*
018290     MOVE 開始和暦３Ｗ TO 元－元号区分.
018300     READ 元号マスタ
018310     NOT INVALID KEY
018320         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
018330     END-READ.
018340     MOVE 終了和暦３Ｗ TO 元－元号区分.
018350     READ 元号マスタ
018360     NOT INVALID KEY
018370         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
018380     END-READ.
018390**
018400     IF (開始西暦年Ｗ NOT = ZERO) AND (終了西暦年Ｗ NOT = ZERO)
018410        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年３Ｗ - 1
018420        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年３Ｗ - 1
018430*
018440        IF 終了西暦年Ｗ =  開始西暦年Ｗ
018450           PERFORM  前月比較月
018460*--- 元号日付チェック ---*
018470           IF ( 計算月３Ｗ = ZERO )
018480              MOVE 1 TO 計算月３Ｗ
018490           END-IF
018500*
018510        ELSE
018520           IF  終了西暦年Ｗ >  開始西暦年Ｗ
018530               COMPUTE 計算年３Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
018540               COMPUTE 計算月３Ｗ = (計算年Ｗ * 12 + 終了月３Ｗ) - 開始月３Ｗ
018550           ELSE
018560               MOVE ZERO TO 計算月３Ｗ
018570           END-IF
018580        END-IF
018590     ELSE
018600        MOVE ZERO TO 計算月３Ｗ
018610     END-IF.
018620*
018630**================================================================*
      * 部位継続月数判定１ SECTION.
      **レセファイルに転記するヴァージョン
hilo  *     DISPLAY "210-5 部位継続月数判定 " 負－患者コード "--------------"
      **/継続月数
      *     IF 連計－施術和暦年月 >= 50610
      **         MOVE ZERO TO 部位継続月数Ｗ(部位ＣＮＴ)
      *         INITIALIZE 長期頻回計算Ｗ
050531*         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
hilo  **             DISPLAY "210-6 部位コード(" 部位ＣＮＴ ") " 負－部位コード(部位ＣＮＴ) " 負傷日" 負－負傷和暦年月日(部位ＣＮＴ) " 開始日" 負－開始和暦年月日(部位ＣＮＴ)
hilo  *             DISPLAY "210-6 " 部位ＣＮＴ "部位目---------------------"
      *             MOVE 負－初検和暦年月 TO 判定終了和暦年月Ｗ
      *             MOVE SPACE TO 終了フラグ５
055795*             MOVE 負－レコードキー TO 参照負－レコードキー 
044420*             START 参照負傷データＦ KEY IS <= 参照負－患者コード
001000*                                              参照負－施術和暦年月
044450*                                              REVERSED
044460*             END-START
045260*             IF 状態キー = "00"
045270*                 MOVE SPACE TO 終了フラグ５
045280*                 PERFORM 参照負傷データＦ読込
      *                 PERFORM UNTIL (判定終了和暦年月Ｗ > 参照負－施術和暦年月) OR 
      *                               (負－患者コード NOT = 参照負－患者コード  ) OR
      *                               (終了フラグ５   NOT = SPACE)
      **/                  */同一部位を探してから処理
050531*                     PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
      *                         IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
      *                            (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND 
      *                            (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２))
hilo  **                             DISPLAY "210-7 部位コード(" 部位ＣＮＴ ") " 参照負－部位コード(部位ＣＮＴ) " 負傷日" 参照負－負傷和暦年月日(部位ＣＮＴ) " 開始日" 参照負－開始和暦年月日(部位ＣＮＴ)
049942*                             MOVE 参照負－施術和暦年月 TO 参照レセ－施術和暦年月
049946*                             MOVE 参照負－患者コード   TO 参照レセ－患者コード
049950*                             MOVE 01                   TO 参照レセ－レセ種別
049951*                             READ 参照レセプトＦ
049952*                             NOT INVALID KEY
      **/                              */当月データは保存前の為レコードセットしたデータで判定する
      *                                 IF 負－施術和暦年月 = 参照負－施術和暦年月
      *                                     MOVE レセ－レコードデータ TO 参照レセ－レコードデータ
      *                                 END-IF
      *                                 IF 参照レセ－部位実日数(部位ＣＮＴ２) >= 10
      *                                     COMPUTE 部位継続月数Ｗ(部位ＣＮＴ) = 部位継続月数Ｗ(部位ＣＮＴ) + 1
HILO  *                                 DISPLAY "年月" 参照レセ－施術和暦年月 " 実日数(" 部位ＣＮＴ２ ")=" 参照レセ－部位実日数(部位ＣＮＴ２) " 部位継続月数Ｗ(" 部位ＣＮＴ ")「" 部位継続月数Ｗ(部位ＣＮＴ) "」対象  ＋１"
      *                                 ELSE
      *                                     MOVE "YES" TO 終了フラグ５
HILO  *                                 DISPLAY "年月" 参照レセ－施術和暦年月 " 実日数(" 部位ＣＮＴ２ ")=" 参照レセ－部位実日数(部位ＣＮＴ２) " 部位継続月数Ｗ(" 部位ＣＮＴ ")「" 部位継続月数Ｗ(部位ＣＮＴ) "」対象外終了"
      *                                 END-IF
      *                             END-READ
      *                             MOVE 9 TO 部位ＣＮＴ２
      *                         END-IF
      *                     END-PERFORM
045280*                     PERFORM 参照負傷データＦ読込
017640*                     IF (終了フラグ５ = SPACE) AND (参照負－患者コード = 負－患者コード)
      *                        MOVE 参照レセ－施術和暦年月 TO 終了和暦年月３Ｗ
      *                        MOVE 参照負－施術和暦年月   TO 開始和暦年月３Ｗ
017660*                        PERFORM 前月判定
      *                        IF 前月フラグ = SPACE
      *                            MOVE "YES" TO 終了フラグ５
      *                        END-IF
017670*                     END-IF
      *                 END-PERFORM
      *             END-IF
050533*         END-PERFORM
050533*     END-IF.
      **
      *     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
HILO  *         DISPLAY 部位継続月数Ｗ(部位ＣＮＴ)
      *         MOVE 部位継続月数Ｗ(部位ＣＮＴ) TO レセ－部位継続月数(部位ＣＮＴ)
      *     END-PERFORM.
      **
      **/前月のレセ－部位継続月数(部位ＣＮＴ)が６以上なら実日数にかかわらず＋１をセットする
055795*     MOVE 負－レコードキー TO 参照負－レコードキー 
044420*     START 参照負傷データＦ KEY IS < 参照負－患者コード
001000*                                     参照負－施術和暦年月
044450*                                     REVERSED
044460*     END-START
045260*     IF 状態キー = "00"
045270*         MOVE SPACE TO 終了フラグ５
045280*         PERFORM 参照負傷データＦ読込
017640*         IF (終了フラグ５ = SPACE) AND (参照負－患者コード = 負－患者コード)
      *            MOVE 負－施術和暦年月     TO 終了和暦年月３Ｗ
      *            MOVE 参照負－施術和暦年月 TO 開始和暦年月３Ｗ
017660*            PERFORM 前月判定
      *            IF 前月フラグ = "YES"
      **/              */同一部位を探してから処理
050531*                 PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
050531*                     PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
      *                         IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
      *                            (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND 
      *                            (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２))
049942*                             MOVE 参照負－施術和暦年月 TO 参照レセ－施術和暦年月
049946*                             MOVE 参照負－患者コード   TO 参照レセ－患者コード
049950*                             MOVE 01                   TO 参照レセ－レセ種別
049951*                             READ 参照レセプトＦ
049952*                             NOT INVALID KEY
HILO  *                                 DISPLAY "210-2-6 前月の部位継続月数(" 部位ＣＮＴ２ ")=" 参照レセ－部位継続月数(部位ＣＮＴ２)
      *                                 IF 参照レセ－部位継続月数(部位ＣＮＴ２) >= 6
      *                                     COMPUTE レセ－部位継続月数(部位ＣＮＴ) = 参照レセ－部位継続月数(部位ＣＮＴ) + 1
      *                                 END-IF
      *                             END-READ
      *                         END-IF
      *                     END-PERFORM
      *                 END-PERFORM
      *             END-IF
017670*         END-IF
      *     END-IF.
      **
HILO  *     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
HILO  *         DISPLAY "210-8-1 レセ－部位継続月数(" 部位ＣＮＴ ")=" レセ－部位継続月数(部位ＣＮＴ)
HILO  *     END-PERFORM.
025950*================================================================*
018630*================================================================*
       部位継続月数判定２ SECTION.
      *
      *ワークにセットする
hilo       DISPLAY "210-5 部位継続月数判定 " 負－患者コード " " 受－レコードキー " " 受－保険者番号 "------------------------"
      */継続月数
      *     IF 連計－施術和暦年月 >= 50610
      *         MOVE ZERO TO 部位継続月数Ｗ(部位ＣＮＴ)
               INITIALIZE 長期頻回計算Ｗ
050531         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
hilo  *             DISPLAY "210-6 部位コード(" 部位ＣＮＴ ") " 負－部位コード(部位ＣＮＴ) " 負傷日" 負－負傷和暦年月日(部位ＣＮＴ) " 開始日" 負－開始和暦年月日(部位ＣＮＴ)
hilo  *             DISPLAY "210-6 " 部位ＣＮＴ "部位目--------------------------------------------"
hilo               DISPLAY "210-7 " 負－負傷種別(部位ＣＮＴ)
                 IF 負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03
                   MOVE 負－初検和暦年月 TO 判定終了和暦年月Ｗ
024360             IF 負－初検日 > 長期逓減月境界日ＫＷ
024360                 IF 判定終了月Ｗ NOT = 12
024370                     COMPUTE 判定終了月Ｗ = 判定終了月Ｗ + 1
024380                 ELSE
024390                     COMPUTE 判定終了年Ｗ = 判定終了年Ｗ + 1
024390                     MOVE 01 TO 判定終了月Ｗ
018340                     MOVE 判定終了和暦Ｗ TO 元－元号区分
018350                     READ 元号マスタ
018360                     NOT INVALID KEY
      */20241107
018370*                         IF 判定終了和暦年月Ｗ > 元－正式終了和暦年月
018370                         IF (判定終了和暦年月Ｗ > 元－正式終了和暦年月) AND
018370                            (元－正式終了和暦年月 NOT = ZERO)
                                   COMPUTE 判定終了和暦Ｗ = 判定終了和暦Ｗ + 1
                                   MOVE 1 TO 判定終了年Ｗ
                               END-IF
018380                     END-READ
024400                 END-IF
024400             END-IF
                   MOVE SPACE TO 終了フラグ５
055795             MOVE 負－レコードキー TO 参照負－レコードキー 
      */枝番を考慮する/20241107
                   MOVE SPACE TO 参照負－枝番
044420             START 参照負傷データＦ KEY IS <= 参照負－患者コード
001000                                              参照負－施術和暦年月
044450                                              REVERSED
044460             END-START
045260             IF 状態キー = "00"
045270                 MOVE SPACE TO 終了フラグ５
045280                 PERFORM 参照負傷データＦ読込
                       PERFORM UNTIL (判定終了和暦年月Ｗ > 参照負－施術和暦年月) OR 
      */20241107
      *                               (負－患者コード NOT = 参照負－患者コード  ) OR
                                     (負－患者番号   NOT = 参照負－患者番号  ) OR
                                     (終了フラグ５   NOT = SPACE)
056167*
                           MOVE 参照負－レコードキー TO 長頻受－レコードキー
056168                     READ 長頻参照受診者情報Ｆ
056169                     INVALID KEY MOVE SPACE TO 長頻受－レコード END-READ
HILO                       DISPLAY "210-8 " 受－施術和暦年月 受－患者コード     " "     受－患者氏名(1:10) " "     受－保険者番号 " " 
                                        長頻受－施術和暦年月 長頻受－患者コード " " 長頻受－患者氏名(1:10) " " 長頻受－保険者番号


      */                  */同一部位を探してから処理
050531                     PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
                               IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
                                  (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND 
                                  (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２))
                               AND (受－保険者番号 = 長頻受－保険者番号) *>20241117


hilo  *                             DISPLAY "210-7 部位コード(" 部位ＣＮＴ ") " 参照負－部位コード(部位ＣＮＴ) " 負傷日" 参照負－負傷和暦年月日(部位ＣＮＴ) " 開始日" 参照負－開始和暦年月日(部位ＣＮＴ)
049942                             MOVE 参照負－施術和暦年月 TO 参照レセ－施術和暦年月
049946                             MOVE 参照負－患者コード   TO 参照レセ－患者コード
046013                             IF 受－公費種別 = 05
049950                                 MOVE 02               TO 参照レセ－レセ種別
                                   ELSE
049950                                 MOVE 01               TO 参照レセ－レセ種別
                                   END-IF
049951                             READ 参照レセプトＦ
049952                             NOT INVALID KEY
      */                              */当月データは保存前の為レコードセットしたデータで判定する
                                       IF 負－施術和暦年月 = 参照負－施術和暦年月
                                           MOVE レセ－レコードデータ TO 参照レセ－レコードデータ
                                       END-IF
      */枝番を考慮する/20241107↓↓↓
                                       PERFORM 枝番確認
      *                                 IF 参照レセ－部位実日数(部位ＣＮＴ２) >= 10
                                       IF 枝番部位実日数Ｗ >= 10
      */枝番を考慮する/20241107↑↑↑
                                           COMPUTE 部位継続月数Ｗ(部位ＣＮＴ) = 部位継続月数Ｗ(部位ＣＮＴ) + 1
HILO  *                                    DISPLAY "年月" 参照レセ－施術和暦年月 " 実日数(" 部位ＣＮＴ２ ")=" 参照レセ－部位実日数(部位ＣＮＴ２) " 部位継続月数Ｗ(" 部位ＣＮＴ ")「" 部位継続月数Ｗ(部位ＣＮＴ) "」対象  ＋１"
                                       ELSE
                                           MOVE "YES" TO 終了フラグ５
HILO  *                                    DISPLAY "年月" 参照レセ－施術和暦年月 " 実日数(" 部位ＣＮＴ２ ")=" 参照レセ－部位実日数(部位ＣＮＴ２) " 部位継続月数Ｗ(" 部位ＣＮＴ ")「" 部位継続月数Ｗ(部位ＣＮＴ) "」対象外終了"
                                       END-IF
                                   END-READ
                                   MOVE 9 TO 部位ＣＮＴ２
                               END-IF
                           END-PERFORM
045280                     PERFORM 参照負傷データＦ読込
017640                     IF (終了フラグ５ = SPACE) AND (参照負－患者コード = 負－患者コード)
                              MOVE 参照レセ－施術和暦年月 TO 終了和暦年月３Ｗ
                              MOVE 参照負－施術和暦年月   TO 開始和暦年月３Ｗ
017660                        PERFORM 前月判定
                              IF 前月フラグ = SPACE
                                  MOVE "YES" TO 終了フラグ５
                              END-IF
017670                     END-IF
                       END-PERFORM
                   END-IF
                 END-IF
050533         END-PERFORM
050533*     END-IF.
      *
      *     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
HILO  **         DISPLAY 部位継続月数Ｗ(部位ＣＮＴ)
      *         MOVE 部位継続月数Ｗ(部位ＣＮＴ) TO レセ－部位継続月数(部位ＣＮＴ)
      *     END-PERFORM.
      *
      */前月のレセ－部位継続月数(部位ＣＮＴ)が６以上なら実日数にかかわらず＋１をセットする
055795     MOVE 負－レコードキー TO 参照負－レコードキー 
044420     START 参照負傷データＦ KEY IS < 参照負－患者コード
001000                                     参照負－施術和暦年月
044450                                     REVERSED
044460     END-START
045260     IF 状態キー = "00"
045270         MOVE SPACE TO 終了フラグ５
045280         PERFORM 参照負傷データＦ読込
017640         IF (終了フラグ５ = SPACE) AND (参照負－患者コード = 負－患者コード)
                  MOVE 負－施術和暦年月     TO 終了和暦年月３Ｗ
                  MOVE 参照負－施術和暦年月 TO 開始和暦年月３Ｗ
017660            PERFORM 前月判定
                  IF 前月フラグ = "YES"
      */              */同一部位を探してから処理
050531                 PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
050531                     PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
                               IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
                                  (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND 
                                  (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２))
049942                             MOVE 参照負－施術和暦年月 TO 参照レセ－施術和暦年月
049946                             MOVE 参照負－患者コード   TO 参照レセ－患者コード
049950                             MOVE 01                   TO 参照レセ－レセ種別
049951                             READ 参照レセプトＦ
049952                             NOT INVALID KEY
                                       IF 参照レセ－部位継続月数(部位ＣＮＴ２) >= 5
HILO  *                                     DISPLAY "210-2-6 前月の部位継続月数(" 部位ＣＮＴ２ ")=" 参照レセ－部位継続月数(部位ＣＮＴ２)
                                           COMPUTE レセ－部位継続月数(部位ＣＮＴ) = 参照レセ－部位継続月数(部位ＣＮＴ) + 1
                                           COMPUTE 部位継続月数Ｗ(部位ＣＮＴ)     = 参照レセ－部位継続月数(部位ＣＮＴ) + 1
                                       END-IF
                                   END-READ
                               END-IF
                           END-PERFORM
                       END-PERFORM
                   END-IF
017670         END-IF
           END-IF.
      *
HILO  *     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
HILO  *         DISPLAY "210-8-2 レセ－部位継続月数(" 部位ＣＮＴ ")=" レセ－部位継続月数(部位ＣＮＴ)
HILO  *         DISPLAY "210-8-2 部位継続月数(" 部位ＣＮＴ ")=" 部位継続月数Ｗ(部位ＣＮＴ)
HILO  *     END-PERFORM.
025950*================================================================*
       枝番確認 SECTION.
      *
           MOVE ZERO TO 枝番部位実日数Ｗ.
HILO***       DISPLAY "枝番確認 " 参照負－レコードキー  "(" 部位ＣＮＴ ") " 参照レセ－レコードキー
           MOVE 参照レセ－レコードキー TO 枝番参照レセ－レコードキー
           MOVE SPACE                  TO 枝番参照レセ－枝番
014310     START 枝番参照レセプトＦ KEY IS >=
001580                               枝番参照レセ－患者コード
001590                               枝番参照レセ－施術和暦年月
001600                               枝番参照レセ－レセ種別
014320     END-START.
014340     IF 状態キー = "00"
               MOVE SPACE TO 終了フラグ６
               PERFORM 枝番参照レセプトＦ読込
HILO***           DISPLAY "210-2 " 枝番参照レセ－レコードキー
               PERFORM UNTIL (参照レセ－患者番号     NOT = 枝番参照レセ－患者番号) OR 
      *                       (参照レセ－施術和暦年月 NOT = 枝番参照レセ－施術和暦年月) OR 
                             (終了フラグ６       NOT = SPACE)
                   IF (枝番参照レセ－レセ種別 = 1 OR 2) AND
                      (参照レセ－施術和暦年月 = 枝番参照レセ－施術和暦年月)  
000140                 MOVE 枝番参照レセ－施術和暦年月 TO 枝番参照負－施術和暦年月
000190                 MOVE 枝番参照レセ－患者コード   TO 枝番参照負－患者コード
                       READ 枝番参照負傷データＦ
                       NOT INVALID KEY


      */20241117
                           MOVE 枝番参照負－レコードキー TO 長頻受－レコードキー
056168                     READ 長頻参照受診者情報Ｆ
056169                     INVALID KEY MOVE SPACE TO 長頻受－レコード END-READ


050531                     PERFORM VARYING 部位ＣＮＴ３ FROM 1 BY 1 UNTIL 部位ＣＮＴ３ > 参照負－部位数

HILO  *                     DISPLAY "-5 " 負－レコードキー " " 枝番参照負－レコードキー
HILO                        DISPLAY "-9 " 受－患者コード " " 受－保険者番号 " " 長頻受－患者コード " " 長頻受－保険者番号 *>20241117


                               IF (負－部位コード    (部位ＣＮＴ) = 枝番参照負－部位コード    (部位ＣＮＴ３)) AND
                                  (負－負傷和暦年月日(部位ＣＮＴ) = 枝番参照負－負傷和暦年月日(部位ＣＮＴ３)) AND 
                                  (負－開始和暦年月日(部位ＣＮＴ) = 枝番参照負－開始和暦年月日(部位ＣＮＴ３))
                               AND (受－保険者番号 = 長頻受－保険者番号) *>20241117

HILO                        DISPLAY "-10 " 部位実日数Ｗ(部位ＣＮＴ３)


                                   IF (連計－施術和暦年月 = 枝番参照レセ－施術和暦年月) AND
                                      (連計－患者コード   = 枝番参照レセ－患者コード  )
050532                                 MOVE 部位実日数Ｗ(部位ＣＮＴ３) TO 枝番参照レセ－部位実日数(部位ＣＮＴ３)

HILO                        DISPLAY "-11 "

                                   END-IF
HILO**                               DISPLAY "210-1 " 枝番参照レセ－レコードキー " 部位ＣＮＴ３(" 部位ＣＮＴ３ ")=" 枝番参照レセ－部位実日数 (部位ＣＮＴ３)
                                   COMPUTE 枝番部位実日数Ｗ = 枝番部位実日数Ｗ + 枝番参照レセ－部位実日数 (部位ＣＮＴ３)
                               END-IF
                           END-PERFORM
                       END-READ
                   END-IF
                   PERFORM 枝番参照レセプトＦ読込
HILO  *         DISPLAY "210-3 " 枝番参照レセ－レコードキー
               END-PERFORM
           END-IF.
HILO***       DISPLAY "210-4 " 枝番部位実日数Ｗ.
025950*================================================================*
       枝番参照レセプトＦ読込 SECTION.
045450*
045460     READ 枝番参照レセプトＦ NEXT
045470     AT END
045490         MOVE "YES" TO 終了フラグ６
045500     END-READ.
025950*================================================================*
018630*================================================================*
       部位継続月数判定 SECTION.
      *
HILO  *     DISPLAY "部位継続月数判定------------------------------"
008320     OPEN OUTPUT 作業ファイル１.
008330         MOVE NC"作１" TO ファイル名Ｗ.
008340         PERFORM オープンチェック.
056157     OPEN INPUT 参照受診者情報Ｆ.
056158         MOVE NC"参照受診" TO ファイル名Ｗ.
056159         PERFORM オープンチェック.
      */最小の負傷年月
           MOVE ALL "9" TO 継続開始施術和暦年月Ｗ
050531     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
               IF 継続開始施術和暦年月Ｗ > 負－負傷和暦年月(部位ＣＮＴ)
                   MOVE 負－負傷和暦年月(部位ＣＮＴ) TO 継続開始施術和暦年月Ｗ
               END-IF
           END-PERFORM.
HILO***       DISPLAY "210-1 " 継続開始施術和暦年月Ｗ.
012960     MOVE 連計－患者コード   TO 参照受－患者コード.
           MOVE HIGH-VALUE         TO 参照受－枝番.
013000     MOVE 連計－施術和暦年月 TO 参照受－施術和暦年月.
044420     START 参照受診者情報Ｆ KEY IS <= 参照受－施術和暦年月
000936                                      参照受－患者コード
044450                                       REVERSED
044460     END-START
045260     IF 状態キー = "00"
               MOVE SPACE TO 終了フラグ４
               PERFORM 参照受診者情報Ｆ読込
               PERFORM UNTIL (参照受－施術和暦年月 < 継続開始施術和暦年月Ｗ) OR (終了フラグ４ NOT = SPACE)
                   IF (連計－患者番号 = 参照受－患者番号) AND (参照受－保険種別 >= 1) AND (参照受－保険種別 <= 9)
HILO***                   DISPLAY "210-2 " 参照受－レコードキー
                       PERFORM 作業ファイル１作成
                   END-IF
                   PERFORM 参照受診者情報Ｆ読込
               END-PERFORM
           END-IF.
      *
HILO  *     DISPLAY "デバッグ--------------------------------------------------"
      *            MOVE HIGH-VALUE TO 作１－レコードキー
044420*            START 作業ファイル１ KEY IS <= 作１－患者番号 作１－部位コード 作１－負傷和暦年月日
000490*            作１－開始和暦年月日 作１－施術和暦年月 作１－保険者番号 REVERSED
044460*             END-START
045260*             IF 状態キー = "00"
      *                 MOVE SPACE TO 終了フラグ７
      *                 PERFORM 作業ファイル１読込
      *                 PERFORM UNTIL (終了フラグ７ NOT = SPACE)
HILO  *                     DISPLAY "210-2-5 " 作１－患者番号 " " 作１－部位コード " " 作１－負傷和暦年月日 " " 
      *                     作１－開始和暦年月日 " " 作１－施術和暦年月 "/" 作１－保険者番号 "/" 作１－部位実日数
      *                 PERFORM 作業ファイル１読込
      *                 END-PERFORM
      *             END-IF
HILO  *     DISPLAY "デバッグ--------------------------------------------------"

HILO  *     DISPLAY "カウント------------------------------"
      */継続月数のカウント
050531     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
      */脱臼も対象/20250128
      *        IF 負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03
              IF 負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03 OR 04
000200            MOVE 負－患者番号                   TO 作１－患者番号
000330            MOVE 負－部位コード    (部位ＣＮＴ) TO 作１－部位コード
000390            MOVE 負－負傷和暦年月日(部位ＣＮＴ) TO 作１－負傷和暦年月日
000490            MOVE 負－開始和暦年月日(部位ＣＮＴ) TO 作１－開始和暦年月日
000140            MOVE 負－施術和暦年月               TO 作１－施術和暦年月
012960            MOVE 負－患者コード                 TO 参照受－患者コード
013000            MOVE 負－施術和暦年月               TO 参照受－施術和暦年月
                  READ 参照受診者情報Ｆ
                  NOT INVALID KEY
001580                MOVE 参照受－保険者番号         TO 作１－保険者番号
                  END-READ
      *
044420            START 作業ファイル１ KEY IS <= 
000200                                           作１－患者番号
000330                                           作１－部位コード
000390                                           作１－負傷和暦年月日
000490                                           作１－開始和暦年月日
000140                                           作１－施術和暦年月
001580                                           作１－保険者番号
044450                                           REVERSED
044460             END-START
HILO  *             DISPLAY "210-2-3 " 状態キー
045260             IF 状態キー = "00"
                       MOVE SPACE TO 終了フラグ７
                       PERFORM 作業ファイル１読込
                       PERFORM UNTIL (負－患者番号                   NOT = 作１－患者番号      ) OR
000330                               (負－部位コード    (部位ＣＮＴ) NOT = 作１－部位コード    ) OR
000390                               (負－負傷和暦年月日(部位ＣＮＴ) NOT = 作１－負傷和暦年月日) OR
000490                               (負－開始和暦年月日(部位ＣＮＴ) NOT = 作１－開始和暦年月日) OR
000140*                               (負－施術和暦年月               NOT = 作１－施術和暦年月  ) OR
001580*                               (参照受－保険者番号             NOT = 作１－保険者番号    ) OR
                                     (終了フラグ７                   NOT = SPACE               )
001580*                     IF (参照受－保険者番号 = 作１－保険者番号) *>20241119 保険者番号毎集計しない
                               IF 作１－部位実日数 >= 10
                                   COMPUTE 部位継続月数Ｗ(部位ＣＮＴ) = 部位継続月数Ｗ(部位ＣＮＴ) + 1
                                   IF (作１－開始和暦年月 = 作１－施術和暦年月) AND (作１－開始日 >= 16)
                                       COMPUTE 部位継続月数Ｗ(部位ＣＮＴ) = 部位継続月数Ｗ(部位ＣＮＴ) - 1
                                       MOVE "YES" TO 終了フラグ７
                                   END-IF
                               ELSE
                                   MOVE "YES" TO 終了フラグ７
                               END-IF
      *                     END-IF *>20241119 保険者番号毎集計しない
                           PERFORM 作業ファイル１読込
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE 参照受診者情報Ｆ 作業ファイル１.
      *
      */当該継続月数が５ヶ月を超えている施術については６と記載し以降１０回未満でも継続月数を記載
           MOVE ZERO TO 前月部位継続月数Ｗ.
           IF 受－施術月 = 1
               MOVE 12 TO 参照レセ－施術月
               COMPUTE 参照レセ－施術年 = 受－施術年 - 1
           ELSE
               MOVE 受－施術年 TO 参照レセ－施術年
               COMPUTE 参照レセ－施術月 = 受－施術月 - 1
           END-IF
018340     MOVE 受－施術和暦 TO 元－元号区分 参照レセ－施術和暦
018350     READ 元号マスタ
018360     NOT INVALID KEY
               IF 元－正式開始和暦年月 > 受－施術和暦年月
                   COMPUTE 元－元号区分 = 元－元号区分 - 1
018350             READ 元号マスタ
018360             NOT INVALID KEY
                       MOVE 元－正式終了和暦年月 TO 参照レセ－施術和暦年月
                   END-READ
               END-IF
           END-READ
      *
049946     MOVE 受－患者コード   TO 参照レセ－患者コード
046013     IF 受－公費種別 = 05
049950         MOVE 02               TO 参照レセ－レセ種別
           ELSE
049950         MOVE 01               TO 参照レセ－レセ種別
           END-IF
      *
049951     READ 参照レセプトＦ
049952     NOT INVALID KEY
000140         MOVE 参照レセ－施術和暦年月 TO 参照負－施術和暦年月
000190         MOVE 参照レセ－患者コード   TO 参照負－患者コード
               READ 参照負傷データＦ
               NOT INVALID KEY
050531             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
      */脱臼も対象/20250128
      *                 IF 負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03
                       IF 負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03 OR 04
                           MOVE ZERO TO 前月部位継続月数Ｗ
050531                     PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
000330                         IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
000390                            (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND
000490                            (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２)) 
                                   MOVE 参照レセ－部位継続月数(部位ＣＮＴ２) TO 前月部位継続月数Ｗ
                                   MOVE 9 TO 部位ＣＮＴ２
                               END-IF
                           END-PERFORM
                           IF 前月部位継続月数Ｗ >= 5
                               COMPUTE 部位継続月数Ｗ(部位ＣＮＴ) = 前月部位継続月数Ｗ + 1
                           END-IF
                       END-IF
                   END-PERFORM
               END-READ
           END-READ.
      */例外処理
      */日整静岡・令和柔整は枝番を合算しない
           IF 協会コードＷ = 02 OR 37
050531         PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 負－部位数
                   IF 部位継続月数Ｗ  (部位ＣＮＴ２) < 6
                   IF レセ－部位実日数(部位ＣＮＴ２) < 10
                       MOVE ZERO TO 部位継続月数Ｗ(部位ＣＮＴ２)
                   END-IF
                   END-IF
               END-PERFORM
           END-IF.
018630*================================================================*
       作業ファイル１読込 SECTION.
      *
045450*
045460     READ 作業ファイル１ NEXT
045470     AT END
045490         MOVE "YES" TO 終了フラグ７
045500     END-READ.
018630*================================================================*
       作業ファイル１作成 SECTION.
      *
           CLOSE 作業ファイル１.
008320     OPEN I-O 作業ファイル１.
008330         MOVE NC"作１" TO ファイル名Ｗ.
008340         PERFORM オープンチェック.
      */レコードセット
000200     MOVE 参照受－患者コード TO 作１－患者番号.
      */保険者番号ごとの集計はしない
001580*     MOVE 参照受－保険者番号 TO 作１－保険者番号.
           MOVE SPACE TO 作１－保険者番号.
      *
000140     MOVE 参照受－施術和暦年月 TO 参照負－施術和暦年月
000190     MOVE 参照受－患者コード   TO 参照負－患者コード
           READ 参照負傷データＦ
           NOT INVALID KEY
050531         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 参照負－部位数
000330             MOVE 参照負－部位コード    (部位ＣＮＴ) TO 作１－部位コード
000390             MOVE 参照負－負傷和暦年月日(部位ＣＮＴ) TO 作１－負傷和暦年月日
000490             MOVE 参照負－開始和暦年月日(部位ＣＮＴ) TO 作１－開始和暦年月日
000140             MOVE 参照負－施術和暦年月               TO 作１－施術和暦年月
      *
049942             MOVE 参照負－施術和暦年月 TO 参照レセ－施術和暦年月
049946             MOVE 参照負－患者コード   TO 参照レセ－患者コード
046013             IF 参照受－公費種別 = 05
049950                 MOVE 02               TO 参照レセ－レセ種別
                   ELSE
049950                 MOVE 01               TO 参照レセ－レセ種別
                   END-IF
049951             READ 参照レセプトＦ
049952             NOT INVALID KEY
                       READ 作業ファイル１
                       INVALID KEY
000690                     MOVE 参照レセ－部位実日数(部位ＣＮＴ) TO 作１－部位実日数
                           IF (連計－施術和暦年月 = 参照レセ－施術和暦年月) AND
                              (連計－患者コード   = 参照レセ－患者コード  )
                              MOVE 部位実日数Ｗ(部位ＣＮＴ) TO 作１－部位実日数
                           END-IF
                           WRITE 作１－レコード
005657                     IF 状態キー  NOT =  "00"
                               MOVE NC"Ｈレセ詳細" TO ファイル名Ｗ
                               PERFORM エラー表示
                           END-IF
                       NOT INVALID KEY
000690                     COMPUTE 作１－部位実日数 = 作１－部位実日数 + 参照レセ－部位実日数(部位ＣＮＴ)
                           REWRITE 作１－レコード
005657                     IF 状態キー  NOT =  "00"
                               MOVE NC"Ｈレセ詳細" TO ファイル名Ｗ
                               PERFORM エラー表示
                           END-IF
                       END-READ
HILO***                   DISPLAY "210-3   " 作１－部位コード 作１－保険者番号 作１－施術和暦年月 " " 作１－部位実日数
                   END-READ
               END-PERFORM
           END-READ.
      *     CLOSE 作業ファイル１.
018630*================================================================*
      * 前月レセプトＦ確認 SECTION.
      **
HILO  *     DISPLAY "210-4-1 前月レセプトＦ確認----------------------"
      *     MOVE ZERO TO 前月部位継続月数Ｗ.
      *     IF 受－施術月 = 1
      *         MOVE 12 TO 受－施術月
      *         COMPUTE 受－施術年 = 受－施術年 - 1
      *     ELSE
      *         COMPUTE 受－施術月 = 受－施術月 - 1
      *     END-IF
018340*     MOVE 受－施術和暦 TO 元－元号区分
018350*     READ 元号マスタ
018360*     NOT INVALID KEY
      *         IF 元－正式開始和暦年月 > 受－施術和暦年月
      *             COMPUTE 元－元号区分 = 元－元号区分 - 1
018350*             READ 元号マスタ
018360*             NOT INVALID KEY
      *                 MOVE 元－正式終了和暦年月 TO 受－施術和暦年月
      *             END-READ
      *         END-IF
      *     END-READ
      **
049942*     MOVE 受－施術和暦年月 TO 参照レセ－施術和暦年月
049946*     MOVE 受－患者コード   TO 参照レセ－患者コード
046013*     IF 受－公費種別 = 05
049950*         MOVE 02               TO 参照レセ－レセ種別
      *     ELSE
049950*         MOVE 01               TO 参照レセ－レセ種別
      *     END-IF
HILO  *     DISPLAY "参照レセ－レコードキー " 参照レセ－レコードキー
049951*     READ 参照レセプトＦ
049952*     NOT INVALID KEY
000140*         MOVE レセ－施術和暦年月 TO 参照負－施術和暦年月
000190*         MOVE レセ－患者コード   TO 参照負－患者コード
      *         READ 参照負傷データＦ
      *         NOT INVALID KEY
050531*             PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
000330*                 IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
000390*                    (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND
000490*                    (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２)) 
      *                     MOVE 参照レセ－部位継続月数(部位ＣＮＴ２) TO 前月部位継続月数Ｗ
HILO  *     DISPLAY "210-4-2 前月部位継続月数Ｗ=" 前月部位継続月数Ｗ "/" 部位ＣＮＴ 部位ＣＮＴ２
      *                     MOVE 9 TO 部位ＣＮＴ２
      *                 END-IF
      *             END-PERFORM
      *         END-READ
      *     END-READ.
018630**================================================================*
018630**================================================================*
      * 前月レセプトＦ確認 SECTION.
      **
050531*             PERFORM VARYING 部位ＣＮＴ２ FROM 1 BY 1 UNTIL 部位ＣＮＴ２ > 参照負－部位数
000330*                 IF (負－部位コード    (部位ＣＮＴ) = 参照負－部位コード    (部位ＣＮＴ２)) AND
000390*                    (負－負傷和暦年月日(部位ＣＮＴ) = 参照負－負傷和暦年月日(部位ＣＮＴ２)) AND
000490*                    (負－開始和暦年月日(部位ＣＮＴ) = 参照負－開始和暦年月日(部位ＣＮＴ２)) 
      *                     MOVE 参照レセ－部位継続月数(部位ＣＮＴ２) TO 前月部位継続月数Ｗ
HILO  *     DISPLAY "210-4-2 前月部位継続月数Ｗ=" 前月部位継続月数Ｗ "/" 部位ＣＮＴ 部位ＣＮＴ２
      *                     MOVE 9 TO 部位ＣＮＴ２
      *                 END-IF
      *             END-PERFORM
018630*================================================================*
056222******************************************************************
056223 END PROGRAM HIY0210R.
056224******************************************************************
