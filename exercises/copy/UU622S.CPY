      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU622S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU622S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   May 14 1996 07:35:54   T13EX14
      *AFKD0091 Badenia neue Datenfelder
      *
      *   Rev 1.0   Nov 04 1995 10:03:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    07.07.93 16:08:45
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER VERAENDERUNGSDATEI                         *
      *    -----------------------------------                         *
      *                                                                *
      *    NAME:    UU622S                                             *
      *                                                                *
      *    VARIABLE SATZLAENGE               ERSTELLT AM 20.04.90      *
      *             MINIMUM  61 BYTES        HECKER                    *
      *             MAXIMUM 720 BYTES                                  *
      *                                                                *
      *             DUMMYSATZ                   ==>    61 B            *
      *             KGES = 10    KUNR = 0       ==>   607 B            *
      *                          KUNR > 0       ==>   374 B            *
      *             KGES = 20                   ==>   720 B            *
      *             KGES = 40, 41               ==>   705 B            *
      *             KGES = 50    KUNR = 0       ==>   699 B            *
      *                          KUNR > 0       ==>   466 B            *
      *             KGES = 70                   ==>   482 B            *
      *             KGES = 80                   ==>   612 B            *
      *                                                                *
      *    BEI DIESER DATEI HANDELT ES SICH UM EINE VSAM-DATEI MIT     *
      *    ORGANISATION KSDS.                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    GEAENDERT:                                 HECKER  22.10.90 *
      *    ROLAND IN RECHTSSCHUTZ UMBENANNT;                           *
      *    REDEFINITION AUF KVART1 FUER ADVOCARD                       *
      *                                                                *
      *    GEAENDERT:                                 KUHN    22.03.91 *
      *    KVPRZ MIT REDEFINE AUF KARBANR FUER                         *
      *    PRUEFZIFFER VOFUE.                                          *
      *                                                                *
      *    GEAENDERT:                                 KUHN    30.01.92 *
      *    KDATVERTR IN KAENDDIR UNBENANNT. DAS                        *
      *    FELD BEINHALTET DAS DATUM DER LETZTEN                       *
      *    DIREKTIONS-AENDERUNG.                                       *
      *                                                                *
      *     GEAENDERT:                                BORK    14.05.93 *
      *     WEGEN UMSTELLUNG AUF NEUE, 5-STELLIGE PLZ:                 *
      *     KPOSTL2, KPOSTL3 IM VARIABLEN TEIL VON AML ALS COMP-3 FELD *
      *     REDEFINIERT, DAMIT DIE NEUE PLZ EINGESTELLT WERDEN KANN.   *
      *     KPOSTLZ IM VAR. TEIL BANKEN ERWEITERT, DA NOCH GENUG FILLER*
      *                                                                *
      * !!!!DIESE EINFACHE LÖSUNG WURDE GEWÄHLT, WEIL FÜR KOMFORTABLERE*
      *     LÖSUNGEN KEINE ZEIT WAR!!!!!                               *
      *                                                                *
      *     AFKD0091                                                   *
      *     GEAENDERT:                            W.SCMIDT    14.05.96 *
      *     NEUE DATENFELDER BADENIA                                   *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  AM-LEBEN.
           10  AEND-KEY.
               20 VBNR                     PIC 9(7).
               20 IDNR                     PIC 9(7).
               20 GES                      PIC 99.
               20 VNR                      PIC 9(15).
               20 UNR                      PIC 99.
               20 AEND-DAT                 PIC 9(6).
               20 AEND-KZ                  PIC 9.
           10  AM-LEBEN1.
               20  KSA                     PIC 99.
               20  KKEY.
                   30  KGES                PIC 99.
                   30  KVNR                PIC 9(15).
                   30  KUNR                PIC 99.
               20  KMATCHC                 PIC X(17).
               20  KVBDAT                  PIC 9(7)          COMP-3.
               20  KBEARBDAT               PIC 9(7)          COMP-3.
               20  KALT-KEY.
                   30 KVBNR                PIC 9(7).
                   30 KIDNR                PIC 9(7).
               20  KIDDAT                  PIC 9(7)          COMP-3.
               20  KVB-HIST      OCCURS 3.
                   30 KVBDAT-HIST          PIC 9(7)          COMP-3.
                   30 KBEARBDAT-HIST       PIC 9(7)          COMP-3.
                   30 KVBNR-HIST           PIC 9(7)          COMP-3.
               20  KID-HIST      OCCURS 3.
                   30 KIDDAT-HIST          PIC 9(7)          COMP-3.
                   30 KIDNR-HIST           PIC 9(7)          COMP-3.
      *
      *****    FIX-TEIL
      *
               20  KFIXTEIL.
                   30  KANREDE             PIC X.
                   30  KNAME               PIC X(60).
                   30  KSTR                PIC X(26).
                   30  KAUSL               PIC XXX.
                   30  KPLZ                PIC X(5).
                   30  KORT                PIC X(21).
                   30  KGEBDAT             PIC 9(7)           COMP-3.
                   30  KBERUF              PIC 999.
                   30  KAENDDIR            PIC 9(7)           COMP-3.
                   30  KZAHLW              PIC X.
                   30  KMAHNKZ             PIC 9.
                   30  KMAHNDAT            PIC 9(7)           COMP-3.
                   30  KVERTR.
                       40  KVBEG           PIC 9(7)           COMP-3.
                       40  KVABL           PIC 9(7)           COMP-3.
                   30  KVAEND              PIC 9(7)           COMP-3.
                   30  KPROVHAFT.
                       40  KPRVON          PIC 9(4).
                       40  KPRBIS          PIC 9(4).
                   30  KVZUST              PIC 9.
                   30  KUPDAT              PIC 9(7)           COMP-3.
                   30  KUPKZ               PIC 9.
                   30  KDIR                PIC 999.
                   30  KVORT               PIC X(22).
                   30  KVSTR               PIC X(36).
                   30  KINFO               PIC X(7).
                   30  KAVM                PIC 9(7)           COMP-3.
                   30  KAVM-AEND-DAT       PIC 9(7)           COMP-3.
                   30  KAVM-KZ             PIC X.
                   30  KVBNR-KZ            PIC X.
                   30  KAVM-BA-KZ          PIC 9.
      *
      *****    VARIABLER TEIL AM-LEBEN
      *
               20  VARIABEL-AML.
                   30  KPSTAT1             PIC 9.
                   30  KIART               PIC 99.
                   30  KRBTG               PIC S9(5)V99       COMP-3.
                   30  KTARIF-AML          PIC X(10).
                   30  KBTG                PIC 9(4).
                   30  KVSR                PIC S9(9)          COMP-3.
                   30  KERSCHW             PIC XX.
                   30  KVERF               PIC XX.
                   30  KVZBETR             PIC S9(7)V99       COMP-3.
                   30  KPSTAT2             PIC X.
                   30  KNAM2               PIC X(34).
                   30  KGEB2               PIC 9(7)           COMP-3.
                   30  KPOSTL2             PIC X(4).
140593             30  FILLER   REDEFINES  KPOSTL2.
140593                 35  KPOSTL2KZ       PIC X(01).
140593                 35  KPOSTL2-NEU     PIC 9(05)          COMP-3.
                   30  KWORT2              PIC X(21).
                   30  KSTRA2              PIC X(25).
                   30  KBER2               PIC 999            COMP-3.
                   30  KPSTAT3             PIC X.
                   30  KNAM3               PIC X(34).
                   30  KGEB3               PIC 9(7)           COMP-3.
                   30  KPOSTL3             PIC X(4).
140593             30  FILLER   REDEFINES  KPOSTL3.
140593                 35  KPOSTL3KZ       PIC X(01).
140593                 35  KPOSTL3-NEU     PIC 9(05)          COMP-3.
                   30  KWORT3              PIC X(21).
                   30  KSTRA3              PIC X(25).
                   30  KBER3               PIC 999            COMP-3.
                   30  KANR1               PIC X.
                   30  KPSTAT4             PIC X.
                   30  KARBANR             PIC X.
                   30  KVPRZ   REDEFINES  KARBANR PIC X.
                   30  KARBGEB             PIC X(20).
                   30  KARBANSCHR          PIC X(33).
                   30  KDYNVSU             PIC S9(18)         COMP-3.
                   30  KDYNBTR             PIC S9(16)V99      COMP-3.
                   30  KDYNRBTR            PIC S9(16)V99      COMP-3.
                   30  KDYNVZ              PIC S9(16)V99      COMP-3.
                   30  KBA-KZ              PIC 9.
                   30  KBA-DAT             PIC 9(7)           COMP-3.
      *
      *****         BESTANDSAETZE AM-SACH
      *
       01  AMS.
           10  FILLER                      PIC X(40).
           10  AMS1.
               20  FILLER                  PIC X(362).
      *
      *****         VARIABLER TEIL AM-SACH
      *
               20  VARIABEL-AMS            PIC X(205).
      *
      *****   REDEFINITIONEN DER EINZELNEN VERS.-ZWEIGE
      *
      *
      *****         S A C H - V E R S .
      *
               20  AMS-SACH  REDEFINES  VARIABEL-AMS.
                   30  SACH-DATEN       OCCURS 4.
                       40  S-ZWG           PIC 99.
                       40  S-ZWGT          PIC X(3).
                       40  S-BTG           PIC 9(7)V99        COMP-3.
                       40  S-VGEF          PIC X(21).
                       40  S-VUMF          PIC X(3).
                       40  S-VSU           PIC 9(9)           COMP-3.
                       40  S-HFI           PIC XX.
                       40  S-SB            PIC XX.
                       40  S-VHB           PIC X(5).
                       40  S-GN            PIC XX.
                       40  S-KZW           PIC X.
                   30  S-STERN             PIC X.
      *
      *****     BAUWESEN-, SCHWACHSTROM-, MASCHINEN- U. MBU-VERS.
      *
               20  AMS-MBS  REDEFINES  VARIABEL-AMS.
                   30  MBS-ZWG             PIC 99.
                   30  MBS-ZWGT            PIC X(3).
                   30  MBS-AVB          OCCURS 5.
                       40  MBS-AVBKZ       PIC 99.
                   30  MBS-VSU             PIC 9(5)           COMP-3.
                   30  MBS-TAB          OCCURS 4.
                       40  MBS-ART         PIC XX.
                       40  MBS-LEISTG      PIC XX.
                       40  MBS-BTG         PIC 9(7)           COMP-3.
                   30  MBS-KZW             PIC X.
                   30  MBS-STERN           PIC X.
                   30  MBS-FILLER          PIC X(153).
      *
      *****         T I E R - V E R S .
      *
               20  AMS-TIER  REDEFINES  VARIABEL-AMS.
                   30  TI-ZWG              PIC 99.
                   30  TI-ZWGT             PIC X(3).
                   30  TI-KZ               PIC X.
                   30  TI-BTG              PIC 9(5)           COMP-3.
                   30  TI-TAB           OCCURS 7.
                       40  TI-ART          PIC 99.
                       40  TI-GAT          PIC 99.
                       40  TI-VZW          PIC 99.
                   30  TI-KZW              PIC X.
                   30  TI-STERN            PIC X.
                   30  TI-FILLER           PIC X(152).
      *
      *****         TRANSPORT-, LUFTUNFALL-VERS.
      *
               20  AMS-TLU  REDEFINES  VARIABEL-AMS.
                   30  TLU-ZWG             PIC 99.
                   30  TLU-ZWGT            PIC X(3).
                   30  TLU-STAT         OCCURS 20.
                       40  TLU-STKZ        PIC 9(3)           COMP-3.
                   30  TLU-KZW             PIC X.
                   30  TLU-PREG            PIC X.
                   30  TLU-STERN           PIC X.
                   30  TLU-FILLER          PIC X(157).
      *
      *****         K F Z - , M O P E D - V E R S.
      *
               20  AMS-KFZ  REDEFINES  VARIABEL-AMS.
                   30  KFZ-ZWG             PIC 99.
                   30  KFZ-ZWGT            PIC X(3).
                   30  KFZ-RUHE            PIC X.
                   30  KFZ-AKZ             PIC X(12).
                   30  KFZ-ZUJAHR          PIC 99.
                   30  KFZ-WKZ             PIC 9(3)           COMP-3.
                   30  KFZ-DSH             PIC XX.
                   30  KFZ-BTGH            PIC 9(5)           COMP-3.
                   30  KFZ-RBKLH           PIC X(3).
                   30  KFZ-DSF             PIC XX.
                   30  KFZ-BTGF            PIC 9(5)           COMP-3.
                   30  KFZ-RBKLF           PIC X(3).
                   30  KFZ-DSU             PIC XX.
                   30  KFZ-BTGU            PIC 9(5)V99        COMP-3.
                   30  KFZ-FNRNAT          PIC X(3).
                   30  KFZ-TARB            PIC X(3).
                   30  KFZ-STERN           PIC X.
                   30  KFZ-FILLER          PIC X(154).
      *
      *****         H A F T P F L I C H T - V E R S .
      *
               20  AMS-HAFT  REDEFINES  VARIABEL-AMS.
                   30  H-ZWG               PIC 99.
                   30  H-ZWGT              PIC X(3).
                   30  H-BTG               PIC 9(7)V99        COMP-3.
                   30  H-WAGN1             PIC 9(7)           COMP-3.
                   30  H-WAGN2             PIC 9(7)           COMP-3.
                   30  H-VSUP              PIC 9(5)           COMP-3.
                   30  H-VSUS              PIC 9(5)           COMP-3.
                   30  H-VSUV              PIC 9(5)           COMP-3.
                   30  H-VSUE              PIC 9(5)           COMP-3.
                   30  H-WAGNMENG          PIC 9(7)           COMP-3.
                   30  H-WAGNTEXT          PIC X(6).
                   30  H-KZW               PIC X.
                   30  H-STERN             PIC X.
                   30  H-FILLER            PIC X(163).
      *
      *****         U N F A L L - V E R S .
      *
               20  AMS-UNFALL  REDEFINES  VARIABEL-AMS.
                   30  U-ZWG               PIC 99.
                   30  U-ZWGT              PIC X(3).
                   30  U-BTG               PIC 9(7)V99        COMP-3.
                   30  U-KZBG              PIC X.
                   30  U-DATEN          OCCURS 5.
                       40  U-GBJAHR        PIC 99.
                       40  U-UMFANG        PIC 9.
                       40  U-TART          PIC XX.
                       40  U-VSUTOD        PIC 9(7)           COMP-3.
                       40  U-VSUINV        PIC 9(7)           COMP-3.
                       40  U-VSUKTG        PIC 9(3)           COMP-3.
                       40  U-VSUTG         PIC 9(3)           COMP-3.
                       40  U-VSUGG         PIC 9(3)           COMP-3.
                       40  U-VSUHK         PIC 9(3)           COMP-3.
                   30  U-STERN             PIC X.
                   30  U-FILLER            PIC X(88).
      *
      *****         BESTANDSAETZE CKV
      *
       01  CKV.
           10  FILLER                      PIC X(40).
           10  CKV1.
               20  FILLER                  PIC X(362).
      *
      *****         VARIABLER TEIL CKV
      *
               20  VARIABEL-CKV.
                   30  KVNAME              PIC X(10).
                   30  KGEBD               PIC 9(7)           COMP-3.
                   30  KGESCHL             PIC 9.
                   30  KVBEGP              PIC 9(4).
                   30  KVABLP              PIC 9(4).
                   30  KGS                 PIC XXX.
                   30  KLAST               PIC X.
                   30  KTARIFTAB           OCCURS 10.
                       40  KZUGV           PIC 9(4).
                       40  KTARIF          PIC X(8).
                       40  KFORM           PIC X.
                       40  KBEITR          PIC S9(5)V99       COMP-3.
                       40  KSBET           PIC S9(5)          COMP-3.
                       40  KLEIST          PIC S9(5)          COMP-3.
                       40  KABGV           PIC 9(4).
      *
      *****         BESTANDSAETZE BADENIA
      *
       01  BADENIA.
           10  FILLER                      PIC X(40).
           10  BADENIA1.
               20  FILLER                  PIC X(362).
      *
      *****    VARIABLER TEIL BADENIA
      *
               20  VARIABEL-BADENIA.
                   30  KPRODB              PIC 99.
                   30  KLAUFZ              PIC XX.
                   30  KVSUM               PIC S9(7)          COMP-3.
                   30  KGHABEN             PIC S9(7)V99       COMP-3.
                   30  KBEITRAG            PIC S9(5)V99       COMP-3.
                   30  KDARL               PIC S9(9)          COMP-3.
                   30  KKONTOST            PIC X.
                   30  KBWZAHL             PIC S99V9(5)       COMP-3.
140596*            30  KARBGEBER.
140596*                40  KFIRMA          PIC X(20).
140596*                40  KFSITZ          PIC X(33).
140596             30  KTARIF-T3           PIC X(01).
140596             30  KZINS-ALLG          PIC S9(3)V99       COMP-3.
140596             30  KZINS-DARLEHEN      PIC S9(3)V99       COMP-3.
140596             30  KDISAGIO            PIC X(1).
140596             30  KFREI-BETRAG        PIC S9(5)V99       COMP-3.
140596             30  KVFREI-BETRAG       PIC S9(5)V99       COMP-3.
140596             30  KFEHL-BETRAG        PIC S9(7)          COMP-3.
140596             30  KENDE-ST-LAUFZEIT   PIC S9(9)          COMP-3.
140596             30  KUMS-TABELLE.
140596                 35 KUMS-TAB         OCCURS 5.
140596                    40 KUMS-BETRAG   PIC S9(7)V99       COMP-3.
140596                    40 KUMS-SCHL     PIC S9(3)          COMP-3.
140596                    40 KUMS-DATUM    PIC S9(7)          COMP-3.
140596             30  KDATUM-WOHN         PIC S9(9)          COMP-3.
140596             30  KDATUM-ZUTEIL       PIC S9(9)          COMP-3.
140596             30  KBLAND              PIC X(2).
140596             30  KFEHL-BETRAG-DATUM  PIC S9(9)          COMP-3.

      *****        BESTANDSAETZE ROLAND
      *
       01  RECHTSSCHUTZ.
           10  FILLER                      PIC X(40).
           10  RECHTSSCHUTZ1.
               20  FILLER                  PIC X(362).
      *
      *****    VARIABLER TEIL RECHTSSCHUTZ
      *
               20  VARIABEL-RECHTSSCHUTZ.
                   30  KSPARTE             PIC X.
                   30  KLASTR              PIC X.
                   30  KBAKKZ              PIC X.
                   30  KFAEL               PIC XX.
                   30  KPRAEMIE            PIC 9(9)V99        COMP-3.
                   30  KVSU                PIC 9(7)           COMP-3.
                   30  KVARTTB1            OCCURS 5.
                       40 KWAG1            PIC X.
                       40 KLBAPRO1         PIC S9(5)          COMP-3.
                       40 KLBADAT1         PIC 9(7)           COMP-3.
                       40 KVART1           PIC XXX.
                       40 KVART1-ADV REDEFINES KVART1  PIC 9(5) COMP-3.
                       40 KVTEXT1          PIC X(25).
                   30  KVARTTB2            OCCURS 5.
                       40 KVART2           PIC XXX.
      *
      *****        BESTANDSAETZE BHI DIT III HYPO
      *
       01  BANKEN.
           10  FILLER                      PIC X(40).
           10  BANKEN1.
               20  FILLER                  PIC X(362).
      *
      *****    VARIABLER TEIL DIT III HYPO BHI
      *
               20  VARIABEL-BANK.
                   30  KPRODTAB            OCCURS 16.
                       40  KPROD           PIC XX.
                       40  KSUMME          PIC S9(9)           COMP-3.
                       40  KKTO            PIC S9(6)V999       COMP-3.
                   30  KPOSTEMPF.
                       40  KPOSTNAME       PIC X(36).
                       40  KPOSTZUS        PIC X(21).
                       40  KPOSTSTR        PIC X(25).
140593                 40  KPOSTPLZ        PIC X(6).
                       40  KPOSTORT        PIC X(25).
      *
      *****        FOLGESAETZE CKV
      *
       01  FOLGE-CKV.
           10  FILLER                      PIC X(40).
           10  FOLGE-CKV1.
               20  FILLER                  PIC X(124).
               20  FOLGE-CKV-TEIL.
                   25  KFUPDAT             PIC 9(7)           COMP-3.
                   25  FCKV.
                       30  KFUPKZ              PIC 9.
                       30  KFVNAME             PIC X(10).
                       30  KFGEBD              PIC 9(7)       COMP-3.
                       30  KFGESCHL            PIC 9.
                       30  KFVBEGP             PIC 9(4).
                       30  KFVABLP             PIC 9(4).
                       30  KFGS                PIC XXX.
                       30  KFLAST              PIC X.
                       30  KFTARIFTB           OCCURS 10.
                           40  KFZUGV          PIC 9(4).
                           40  KFTARIF         PIC X(8).
                           40  KFFORM          PIC X.
                           40  KFBEITR         PIC S9(5)V99      COMP-3.
                           40  KFSBET          PIC S9(5)         COMP-3.
                           40  KFLEIST         PIC S9(5)         COMP-3.
                           40  KFABGV          PIC 9(4).
      *
      *****     F O L G E - S A E T Z E   A M - S A C H
      *
       01  FOLGE-AMS.
           10  FILLER                      PIC X(40).
           10  FOLGE-AMS1.
               20  FILLER                  PIC X(124).
               20  AMSF-UPDAT              PIC 9(7)           COMP-3.
               20  AMSF-UPKZ               PIC 9.
               20  AMSF-ZWEIG              PIC X(205).
      *
      *****         S A C H - V E R S .         >>> FOLGE <<<
      *
               20  AMSF-SACH  REDEFINES  AMSF-ZWEIG.
                   30  SACHF-DATEN      OCCURS 4.
                       40  SF-ZWG          PIC 99.
                       40  SF-ZWGT         PIC X(3).
                       40  SF-BTG          PIC 9(7)V99        COMP-3.
                       40  SF-VGEF         PIC X(21).
                       40  SF-VUMF         PIC X(3).
                       40  SF-VSU          PIC 9(9)           COMP-3.
                       40  SF-HFI          PIC XX.
                       40  SF-SB           PIC XX.
                       40  SF-VHB          PIC X(5).
                       40  SF-GN           PIC XX.
                       40  SF-KZW          PIC X.
                   30  SF-STERN            PIC X.
      *
      *****       H A F T P F L I C H T - V E R S .    >>> FOLGE <<<
      *
               20  AMSF-HAFT  REDEFINES  AMSF-ZWEIG.
                   30  HF-ZWG              PIC 99.
                   30  HF-ZWGT             PIC X(3).
                   30  HF-BTG              PIC 9(7)V99        COMP-3.
                   30  HF-WAGN1            PIC 9(7)           COMP-3.
                   30  HF-WAGN2            PIC 9(7)           COMP-3.
                   30  HF-VSUP             PIC 9(5)           COMP-3.
                   30  HF-VSUS             PIC 9(5)           COMP-3.
                   30  HF-VSUV             PIC 9(5)           COMP-3.
                   30  HF-VSUE             PIC 9(5)           COMP-3.
                   30  HF-WAGNMENG         PIC 9(7)           COMP-3.
                   30  HF-WAGNTEXT         PIC X(6).
                   30  HF-KZW              PIC X.
                   30  HF-STERN            PIC X.
                   30  HF-FILLER           PIC X(163).
      *
      *****        U N F A L L - V E R S .          >>> FOLGE <<<
      *
               20  AMSF-UNFALL  REDEFINES  AMSF-ZWEIG.
                   30  UF-ZWG              PIC 99.
                   30  UF-ZWGT             PIC X(3).
                   30  UF-BTG              PIC 9(7)V99        COMP-3.
                   30  UF-KZBG             PIC X.
                   30  UF-DATEN         OCCURS 5.
                       40  UF-GBJAHR       PIC 99.
                       40  UF-UMFANG       PIC 9.
                       40  UF-TART         PIC XX.
                       40  UF-VSUTOD       PIC 9(7)           COMP-3.
                       40  UF-VSUINV       PIC 9(7)           COMP-3.
                       40  UF-VSUKTG       PIC 9(3)           COMP-3.
                       40  UF-VSUTG        PIC 9(3)           COMP-3.
                       40  UF-VSUGG        PIC 9(3)           COMP-3.
                       40  UF-VSUHK        PIC 9(3)           COMP-3.
                   30  UF-STERN            PIC X.
                   30  UF-FILLER           PIC X(88).
      *
      *****        DUMMY-SATZ
      *
       01  DUMMY-SATZ.
           10  FILLER                 PIC X(61).

