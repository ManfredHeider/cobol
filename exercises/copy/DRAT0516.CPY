      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0516.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0516.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:01:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0516.
000020        07 DTASATZ                PICTURE X(2000).
000030        07 DTA-SST-DEF-FILLER REDEFINES DTASATZ.
000040           09 DTA-SST-DEF.
000050              11 VS-A-FILLER.
000060                 13 VS-A.
000070                    15 DTA-VORSATZ.
000080                       17 SATZ-KZ PICTURE X.
000090                       17 BABS    PICTURE X(27).
000100                       17 BERST   PICTURE 9(8).
000110                       17 BERST-R REDEFINES BERST.
000120                          19 BERST-TT
000130                                  PICTURE 99.
000140                          19 BERST-MM
000150                                  PICTURE 99.
000160                          19 BERST-JJJJ
000170                                  PICTURE 9(4).
000180                       17 BSTAND  PICTURE 9(8).
000190                       17 BSTAND-R REDEFINES BSTAND.
000200                          19 BSTAND-TT
000210                                  PICTURE 99.
000220                          19 BSTAND-MM
000230                                  PICTURE 99.
000240                          19 BSTAND-JJJJ
000250                                  PICTURE 9(4).
000260                       17 BEMPF   PICTURE X(27).
000270                       17 BINHALT PICTURE X(20).
000280                       17 WRGKZN  PICTURE XXX.
000290                    15 DTA-VORSATZ-ALLG REDEFINES DTA-VORSATZ.
000300                       17 SATZ-KZN
000310                                  PICTURE X.
000320                       17 DTABSENDER
000330                                  PICTURE X(27).
000340                       17 DTERSTELLUNG
000350                                  PICTURE 9(8).
000360                       17 DTSTAND PICTURE 9(8).
000370                       17 DTEMPFAENGER
000380                                  PICTURE X(27).
000390                       17 DTINHALT
000400                                  PICTURE X(20).
000410                       17 DTWAEHRUNGSKZN
000420                                  PICTURE XXX.
000430                 13 FILLER        PICTURE X(52).
000440              11 VS-B-FILLER REDEFINES VS-A-FILLER.
000450                 13 VS-B.
000460                    15 SATZ-KZ    PICTURE X.
000470                    15 BABS       PICTURE X(27).
000480                    15 BERST      PICTURE 9(6).
000490                    15 BERST-R REDEFINES BERST.
000500                       17 TAG     PICTURE 99.
000510                       17 MON     PICTURE 99.
000520                       17 JHR     PICTURE 99.
000530                    15 BSTAND     PICTURE 9(6).
000540                    15 BSTAND-R REDEFINES BSTAND.
000550                       17 TAG     PICTURE 99.
000560                       17 MON     PICTURE 99.
000570                       17 JHR     PICTURE 99.
000580                    15 BEMPF      PICTURE X(27).
000590                    15 BINHALT    PICTURE X(20).
000600                 13 FILLER        PICTURE X(59).
000610              11 VS-CE-FILLER REDEFINES VS-A-FILLER.
000620                 13 VS-CE.
000630                    15 SATZ-KZ    PICTURE X.
000640                    15 SATZTXTH   PICTURE X(45).
000650                    15 SATZTXTH-RED-PROVISION REDEFINES SATZTXTH.
000660                       17 FILLER  PICTURE X(13).
000670                       17 DTABSENDER
000680                                  PICTURE X(11).
000690                       17 FILLER  PICTURE X.
000700                       17 DTINHALT
000710                                  PICTURE X(20).
000720                    15 DTERSTELLUNG
000730                                  PICTURE S9(9)  PACKED-DECIMAL.
000740                    15 KOPKDNR    PICTURE S9(11) PACKED-DECIMAL.
000750                 13 FILLER        PICTURE X(89).
000760              11 VS-CA-FILLER REDEFINES VS-A-FILLER.
000770                 13 VS-CA.
000780                    15 SATZ-KZ    PICTURE X.
000790                    15 SATZTXH1   PICTURE X(12).
000800                    15 KOPAKDNR   PICTURE S9(11) PACKED-DECIMAL.
000810                    15 KONZKZN    PICTURE X.
000820                    15 SATZTXH2   PICTURE X(15).
000830                    15 DTADATV    PICTURE S9(9)  PACKED-DECIMAL.
000840                    15 DTADATB    PICTURE S9(9)  PACKED-DECIMAL.
000850                 13 FILLER        PICTURE X(101).
000860              11 VS-D REDEFINES VS-A-FILLER.
000870                 13 SSAZDTA       PICTURE X.
000880                 13 SBCHART       PICTURE X.
000890                 13 TDTN          PICTURE X.
000900                 13 IBNKEMP       PICTURE S9(9)  PACKED-DECIMAL.
000910                 13 TUNU1         PICTURE S9(9)  PACKED-DECIMAL.
000920                 13 TDTNAMABS     PICTURE X(27).
000930                 13 DERS          PICTURE S9(7)  PACKED-DECIMAL.
000940                 13 TUNU2         PICTURE 9(4).
000950                 13 SKTOABS       PICTURE S9(11) PACKED-DECIMAL.
000960                 13 TUNU3         PICTURE 9(10).
000970                 13 REST          OCCURS 82 TIMES
000980                                  PICTURE X.
000990              11 VS-E REDEFINES VS-A-FILLER.
001000                 13 A2-DTA-SA-A   PICTURE X.
001010                 13 A3-GUTSCHR-KZN
001020                                  PICTURE XX.
001030                 13 A4-BLZ-DTA-ABS
001040                                  PICTURE S9(9)  PACKED-DECIMAL.
001050                 13 A5-BLZ-DTA    PICTURE S9(9)  PACKED-DECIMAL.
001060                 13 A6-NAME-DTA-ABS
001070                                  PICTURE X(27).
001080                 13 A7-DTA-ERSTELLDAT
001090                                  PICTURE S9(7)  PACKED-DECIMAL.
001100                 13 A8            PICTURE X(4).
001110                 13 A9-KTONR-ABS  PICTURE S9(11) PACKED-DECIMAL.
001120                 13 A10-REFERENZNR
001130                                  PICTURE 9(10).
001140                 13 A11           OCCURS 81 TIMES
001150                                  PICTURE X.
001160                 13 A12-WRG-KZN   PICTURE X.
001170              11 VS-F-FILLER REDEFINES VS-A-FILLER.
001180                 13 VS-F.
001190                    15 DTA-VORSATZ-ALLG-AVBG.
001200                       17 SATZ-KZN
001210                                  PICTURE X.
001220                       17 DTABSENDER
001230                                  PICTURE X(27).
001240                       17 DTERSTELLUNG
001250                                  PICTURE 9(8).
001260                       17 DTSTAND PICTURE 9(8).
001270                       17 DTEMPFAENGER
001280                                  PICTURE X(27).
001290                       17 DTINHALT
001300                                  PICTURE X(20).
001310                       17 DTWAEHRUNGSKZN
001320                                  PICTURE XXX.
001330                    15 DTA-VORSATZ-AVBG REDEFINES
001340          DTA-VORSATZ-ALLG-AVBG.
001350                       17 SATZ-KZ PICTURE X.
001360                       17 BABS    PICTURE X(27).
001370                       17 BERST   PICTURE 9(8).
001380                       17 BERST-R REDEFINES BERST.
001390                          19 BERST-JJJJ
001400                                  PICTURE 9(4).
001410                          19 BERST-MM
001420                                  PICTURE 99.
001430                          19 BERST-TT
001440                                  PICTURE 99.
001450                       17 BSTAND  PICTURE 9(8).
001460                       17 BSTAND-R REDEFINES BSTAND.
001470                          19 BSTAND-JJJJ
001480                                  PICTURE 9(4).
001490                          19 BSTAND-MM
001500                                  PICTURE 99.
001510                          19 BSTAND-TT
001520                                  PICTURE 99.
001530                       17 BEMPF   PICTURE X(27).
001540                       17 BINHALT PICTURE X(20).
001550                       17 WRGKZN  PICTURE XXX.
001560                 13 FILLER        PICTURE X(52).
001570              11 NS-A-FILLER REDEFINES VS-A-FILLER.
001580                 13 NS-A.
001590                    15 SATZ-KZ    PICTURE X.
001600                    15 FSAETZE    PICTURE S9(7)  PACKED-DECIMAL.
001610                    15 FSUMME     OCCURS 5 TIMES
001620                                  PICTURE S9(15)V99
001630                                                 PACKED-DECIMAL.
001640                 13 FILLER        PICTURE X(96).
001650              11 NS-B-FILLER REDEFINES VS-A-FILLER.
001660                 13 NS-B.
001670                    15 SATZ-KZ    PICTURE X.
001680                    15 FILLER     PICTURE 9(17).
001690                    15 FSAETZE    PICTURE 9(7).
001700                    15 FSUMME1    OCCURS 2 TIMES
001710                                  PICTURE S9(15)V99.
001720                    15 FSUMME2    OCCURS 5 TIMES
001730                                  PICTURE S9(11)V99.
001740                 13 FILLER        PICTURE X(22).
001750              11 NS-CE-FILLER REDEFINES VS-A-FILLER.
001760                 13 NS-CE.
001770                    15 SATZ-KZ    PICTURE X.
001780                    15 SATZTXTS   PICTURE X(37).
001790                    15 DTADTVON   PICTURE S9(9)  PACKED-DECIMAL.
001800                    15 DTADTBIS   PICTURE S9(9)  PACKED-DECIMAL.
001810                    15 DTSTAND REDEFINES DTADTBIS
001820                                  PICTURE S9(9)  PACKED-DECIMAL.
001830                    15 FSAETZE    PICTURE S9(11) PACKED-DECIMAL.
001840                    15 PRUEFSUM   PICTURE S9(15)V99
001850                                                 PACKED-DECIMAL.
001860                 13 FILLER        PICTURE X(83).
001870              11 NS-CA-FILLER REDEFINES VS-A-FILLER.
001880                 13 NS-CA.
001890                    15 SATZ-KZ    PICTURE X.
001900                    15 SATZTXTS   PICTURE X(25).
001910                    15 FSAETZE    PICTURE S9(11) PACKED-DECIMAL.
001920                    15 PRUEFSUM   PICTURE S9(15)V99
001930                                                 PACKED-DECIMAL.
001940                 13 FILLER        PICTURE X(105).
001950              11 NS-D REDEFINES VS-A-FILLER.
001960                 13 SSAZDTA       PICTURE X.
001970                 13 TDUM1         PICTURE X(5).
001980                 13 ANUMSAZ       PICTURE S9(7)  PACKED-DECIMAL.
001990                 13 BBCHSUM       PICTURE S9(11)V99
002000                                                 PACKED-DECIMAL.
002010                 13 SKTOVNESUM    PICTURE S9(17) PACKED-DECIMAL.
002020                 13 IBNKVNESUM    PICTURE S9(17) PACKED-DECIMAL.
002030                 13 BUEB          PICTURE S9(11)V99
002040                                                 PACKED-DECIMAL.
002050                 13 REST          OCCURS 104 TIMES
002060                                  PICTURE X.
002070              11 NS-E REDEFINES VS-A-FILLER.
002080                 13 E2-DTA-SA-E   PICTURE X.
002090                 13 E3            PICTURE X(5).
002100                 13 E4-ANZ-SAETZE PICTURE S9(7)  PACKED-DECIMAL.
002110                 13 E5-KONTR-BETR PICTURE S9(11)V99
002120                                                 PACKED-DECIMAL.
002130                 13 E6-KONTR-BKTONR
002140                                  PICTURE S9(17) PACKED-DECIMAL.
002150                 13 E7-KONTR-BLZ  PICTURE S9(17) PACKED-DECIMAL.
002160                 13 E8-KONTR-BETR-EURO
002170                                  PICTURE S9(11)V99
002180                                                 PACKED-DECIMAL.
002190                 13 E9            OCCURS 104 TIMES
002200                                  PICTURE X.
002210              11 NS-F-FILLER REDEFINES VS-A-FILLER.
002220                 13 NS-F.
002230                    15 SATZ-KZ    PICTURE X.
002240                    15 FSAETZE    PICTURE 9(7).
002250                    15 FSUMME     OCCURS 5 TIMES
002260                                  PICTURE 9(15)V99.
002270                 13 FILLER        PICTURE X(53).
002280           09 FILLER              PICTURE X(1854).
