      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU614S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU614S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   May 14 1996 07:34:44   T13EX14
      *AFKD0091 Badenia neue Datenfelder
      *
      *   Rev 1.0   Nov 04 1995 10:03:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    20.01.94 16:45:07
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER GEZOGENE KUNDENSATZ-DATEI U.UU1614/U.UU1615*
      *                GEZOGENER KUNDENSATZ * KI *                     *
      *    ---------------------------------------------------         *
      *                                                                *
      *    JEDES DATUM IN DIESEM SATZ IST IN DER FORM 'JJMMTT'         *
      *    GESPEICHERT.                                                *
      *                                                                *
      *    NAME :        UU614S                                        *
      *    SATZLAENGE :  VARIABEL            ERSTELLT    18.05.90      *
      *                                                                *
      *             MINIMUM 346 BYTES                                  *
      *             MAXIMUM 692 BYTES                                  *
      *                                                                *
      *     AMS       : KGES = 10     KUNR = 0    => 579 BYTES         *
      *                               KUNR > 0    => 346 BYTES         *
      *     AM-LEBEN  : KGES = 02                 => 692 BYTES         *
      *     VOFUE     : KGES = 03                 => 692 BYTES         *
      *     BANKEN                                                     *
      *       DIT     : KGES = 40(DIT)            => 677 BYTES         *
      *       III     : KGES = 41(III)            => 677 BYTES         *
      *       INVEST  : KGES = 46(INVEST)         => 677 BYTES         *
      *       IMMO    : KGES = 47(IMMO)           => 677 BYTES         *
      *     CKV       : KGES = 50     KUNR = 0    => 671 BYTES         *
      *                               KUNR > 0    => 438 BYTES         *
      *     BADENIA   : KGES = 70   - alt -       => 454 BYTES         *
      *     BADENIA   : KGES = 70   - neu -       => 498 BYTES         *
      *     RECHTSSCHUTZ                                               *
      *       ROLAND  : KGES = 80                 => 584 BYTES         *
      *       ADVOCARD: KGES = 81                 => 584 BYTES         *
      *       DMB     : KGES = 82                 => 584 BYTES         *
      *                                                                *
      *----------------------------------------------------------------*
      *    BEI DIESER DATEI HANDELT ES SICH UM EINE VSAM-DATEI ESDS    *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *     GEAENDERT:                               HECKER  23.10.90  *
      *     ROLAND IN RECHTSSCHUTZ UMBENANNT;                          *
      *     REDEFINITION AUF KVART1 FUER ADVOCARD                      *
      *                                                                *
      *     GEAENDERT:                               KUHN    29.11.90  *
      *     REDEFINE AUF KARBANR WG. PRUEFZIFFER                       *
      *     VNR VOFUE                                                  *
      *                                                                *
      *     GEAENDERT:                               KUHN    30.01.92  *
      *     KDATVERTR IN KAENDDIR UNBENANNT.                           *
      *     DATUM DER LETZTEN DIREKTIONSAENDERUNG.                     *
      *                                                                *
      *     GEAENDERT:                         VON DER GOLTZ 11.08.93  *
      *     IN DEM FELD 'KVSTR' WERDEN FELDER FUER FLEXIBLE-PC-        *
      *     STRUKTUREN GESPEICHERT. DIESE FELDER WERDEN IM PROGRAMM    *
      *     UU253 GESPEICHERT. SIE WERDEN DANN IN DEM SORT NACH UU253  *
      *     UND IN UU254 VERARBEITET. (KVSTR WIRD NICHT GEBRAUCHT.)    *
      *       KPCNR-BETR, PCNR WO DIE AUSGELAGERTE STRUKTUR EIGENTLICH *
      *                 HINGEHOERT.                                    *
      *       KBVBNR-AUS, VBNR DES BETREUERS, DER MIT SEINER STRUKTUR  *
      *                 AUSGELAGERT WIRD.                              *
      *                                                                *
      *     GEAENDERT:                         VON DER GOLTZ 20.01.94  *
      *     DIE 2 FELDER FUER FLEXIBLE-PC-STRUKTUREN WERDEN AUS 'KVSTR'*
      *     RAUSGENOMMEN UND IN DEM MATCHCODE-FELD GESPEICHERT.        *
      *     GRUND:  MC-FELD LIEGT NOCH IM FIXTEIL DER FOLGESAETZE VON  *
      *     CKV UND AMV, WIRD ABER NICHT FUER DEN PC VERARBEITET.      *
      *                                                                *
      *     AFKD0091                                                   *
      *     GEAENDERT:                         W.Schmidt     14.05.96  *
      *     Neue Felder für Badenia eingebaut. Neue Satzlänge 493 Bytes*
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  AM-LEBEN.
           05  K-PCNR                      PIC 9(5).
           05  K-AEND-DAT                  PIC 9(6).
           05  K-AEND-KZ                   PIC 9.
           05  AM-LEBEN1.
               10  KSA                     PIC 99.
               10  KKEY.
                   20  KGES                PIC 99.
                   20  KVNR                PIC 9(15).
                   20  KUNR                PIC 99.
               10  KMATCHC                 PIC X(17).
200194         10  KFL-PC-STRUKTUR  REDEFINES KMATCHC.
200194             20  KPCNR-BETR          PIC 9(05).
200194             20  KBVBNR-AUS          PIC 9(07).
200194             20  KFL-FILLER          PIC X(05).
               10  KVBDAT                  PIC 9(7)          COMP-3.
               10  KBEARBDAT               PIC 9(7)          COMP-3.
               10  KALT-KEY.
                   20 KVBNR                PIC 9(7).
                   20 KIDNR                PIC 9(7).
               10  KIDDAT                  PIC 9(7)          COMP-3.
               10  KVB-HIST      OCCURS 3.
                   20 KVBDAT-HIST          PIC 9(7)          COMP-3.
                   20 KBEARBDAT-HIST       PIC 9(7)          COMP-3.
                   20 KVBNR-HIST           PIC 9(7)          COMP-3.
               10  KID-HIST      OCCURS 3.
                   20 KIDDAT-HIST          PIC 9(7)          COMP-3.
                   20 KIDNR-HIST           PIC 9(7)          COMP-3.
      *
      *****    FIX-TEIL
      *
               10  KFIXTEIL.
                   20  KANREDE             PIC X.
                   20  KNAME               PIC X(60).
                   20  KSTR                PIC X(26).
                   20  KAUSL               PIC XXX.
                   20  KPLZ                PIC X(5).
                   20  KORT                PIC X(21).
                   20  KGEBDAT             PIC 9(7)           COMP-3.
                   20  KBERUF              PIC 999.
                   20  KAENDDIR            PIC 9(7)           COMP-3.
                   20  KZAHLW              PIC X.
                   20  KMAHNKZ             PIC 9.
                   20  KMAHNDAT            PIC 9(7)           COMP-3.
                   20  KVERTR.
                       30  KVBEG           PIC 9(7)           COMP-3.
                       30  KVABL           PIC 9(7)           COMP-3.
                   20  KVAEND              PIC 9(7)           COMP-3.
                   20  KPROVHAFT.
                       30  KPRVON          PIC 9(4).
                       30  KPRBIS          PIC 9(4).
                   20  KVZUST              PIC 9.
                   20  KUPDAT              PIC 9(7)           COMP-3.
                   20  KUPKZ               PIC 9.
                   20  KDIR                PIC 999.
                   20  KVORT               PIC X(22).
110893*            20  KVSTR               PIC X(36).
200194             20  KVSTR               PIC X(36).
200194*            20  KFL-PC-STRUKTUR.
200194*                30  KPCNR-BETR      PIC 9(05).
200194*                30  KBVBNR-AUS      PIC 9(07).
200194*                30  FILLER          PIC X(24).
                   20  KINFO               PIC X(7).
                   20  KAVM                PIC 9(7)           COMP-3.
                   20  KAVM-AEND-DAT       PIC 9(7)           COMP-3.
                   20  KAVM-KZ             PIC X.
                   20  KVBNR-KZ            PIC X.
                   20  KAVM-BA-KZ          PIC 9.
      *
      *****    VARIABLER TEIL AM-LEBEN
      *
               10  VARIABEL-AML.
                   20  KPSTAT1             PIC 9.
                   20  KIART               PIC 99.
                   20  KRBTG               PIC S9(5)V99       COMP-3.
                   20  KTARIF-AML          PIC X(10).
                   20  KBTG                PIC 9(4).
                   20  KVSR                PIC S9(9)          COMP-3.
                   20  KERSCHW             PIC XX.
                   20  KVERF               PIC XX.
                   20  KVZBETR             PIC S9(7)V99       COMP-3.
                   20  KPSTAT2             PIC X.
                   20  KNAM2               PIC X(34).
                   20  KGEB2               PIC 9(7)           COMP-3.
                   20  KPOSTL2             PIC X(4).
                   20  KWORT2              PIC X(21).
                   20  KSTRA2              PIC X(25).
                   20  KBER2               PIC 999            COMP-3.
                   20  KPSTAT3             PIC X.
                   20  KNAM3               PIC X(34).
                   20  KGEB3               PIC 9(7)           COMP-3.
                   20  KPOSTL3             PIC X(4).
                   20  KWORT3              PIC X(21).
                   20  KSTRA3              PIC X(25).
                   20  KBER3               PIC 999            COMP-3.
                   20  KANR1               PIC X.
                   20  KPSTAT4             PIC X.
                   20  KARBANR             PIC X.
                   20  KVPRZ   REDEFINES   KARBANR    PIC X.
                   20  KARBGEB             PIC X(20).
                   20  KARBANSCHR          PIC X(33).
                   20  KDYNVSU             PIC S9(18)         COMP-3.
                   20  KDYNBTR             PIC S9(16)V99      COMP-3.
                   20  KDYNRBTR            PIC S9(16)V99      COMP-3.
                   20  KDYNVZ              PIC S9(16)V99      COMP-3.
                   20  KBA-KZ              PIC 9.
                   20  KBA-DAT             PIC 9(7)           COMP-3.
      *
      *****         BESTANDSAETZE AM-SACH
      *
       01  AMS.
           05  FILLER                      PIC X(12).
           05  AMS1.
               10  FILLER                  PIC X(362).
      *
      *****         VARIABLER TEIL AM-SACH
      *
               10  VARIABEL-AMS            PIC X(205).
      *
      *****   REDEFINITIONEN DER EINZELNEN VERS.-ZWEIGE
      *
      *
      *****         S A C H - V E R S .
      *
               10  AMS-SACH  REDEFINES  VARIABEL-AMS.
                   20  SACH-DATEN       OCCURS 4.
                       30  S-ZWG           PIC 99.
                       30  S-ZWGT          PIC X(3).
                       30  S-BTG           PIC 9(7)V99        COMP-3.
                       30  S-VGEF          PIC X(21).
                       30  S-VUMF          PIC X(3).
                       30  S-VSU           PIC 9(9)           COMP-3.
                       30  S-HFI           PIC XX.
                       30  S-SB            PIC XX.
                       30  S-VHB           PIC X(5).
                       30  S-GN            PIC XX.
                       30  S-KZW           PIC X.
                   20  S-STERN             PIC X.
      *
      *****     BAUWESEN-, SCHWACHSTROM-, MASCHINEN- U. MBU-VERS.
      *
               10  AMS-MBS  REDEFINES  VARIABEL-AMS.
                   20  MBS-ZWG             PIC 99.
                   20  MBS-ZWGT            PIC X(3).
                   20  MBS-AVB          OCCURS 5.
                       30  MBS-AVBKZ       PIC 99.
                   20  MBS-VSU             PIC 9(5)           COMP-3.
                   20  MBS-TAB          OCCURS 4.
                       30  MBS-ART         PIC XX.
                       30  MBS-LEISTG      PIC XX.
                       30  MBS-BTG         PIC 9(7)           COMP-3.
                   20  MBS-KZW             PIC X.
                   20  MBS-STERN           PIC X.
                   20  MBS-FILLER          PIC X(153).
      *
      *****         T I E R - V E R S .
      *
               10  AMS-TIER  REDEFINES  VARIABEL-AMS.
                   20  TI-ZWG              PIC 99.
                   20  TI-ZWGT             PIC X(3).
                   20  TI-KZ               PIC X.
                   20  TI-BTG              PIC 9(5)           COMP-3.
                   20  TI-TAB           OCCURS 7.
                       30  TI-ART          PIC 99.
                       30  TI-GAT          PIC 99.
                       30  TI-VZW          PIC 99.
                   20  TI-KZW              PIC X.
                   20  TI-STERN            PIC X.
                   20  TI-FILLER           PIC X(152).
      *
      *****         TRANSPORT-, LUFTUNFALL-VERS.
      *
               10  AMS-TLU  REDEFINES  VARIABEL-AMS.
                   20  TLU-ZWG             PIC 99.
                   20  TLU-ZWGT            PIC X(3).
                   20  TLU-STAT         OCCURS 20.
                       30  TLU-STKZ        PIC 9(3)           COMP-3.
                   20  TLU-KZW             PIC X.
                   20  TLU-PREG            PIC X.
                   20  TLU-STERN           PIC X.
                   20  TLU-FILLER          PIC X(157).
      *
      *****         K F Z - , M O P E D - V E R S.
      *
               10  AMS-KFZ  REDEFINES  VARIABEL-AMS.
                   20  KFZ-ZWG             PIC 99.
                   20  KFZ-ZWGT            PIC X(3).
                   20  KFZ-RUHE            PIC X.
                   20  KFZ-AKZ             PIC X(12).
                   20  KFZ-ZUJAHR          PIC 99.
                   20  KFZ-WKZ             PIC 9(3)           COMP-3.
                   20  KFZ-DSH             PIC XX.
                   20  KFZ-BTGH            PIC 9(5)           COMP-3.
                   20  KFZ-RBKLH           PIC X(3).
                   20  KFZ-DSF             PIC XX.
                   20  KFZ-BTGF            PIC 9(5)           COMP-3.
                   20  KFZ-RBKLF           PIC X(3).
                   20  KFZ-DSU             PIC XX.
                   20  KFZ-BTGU            PIC 9(5)V99        COMP-3.
                   20  KFZ-FNRNAT          PIC X(3).
                   20  KFZ-TARB            PIC X(3).
                   20  KFZ-STERN           PIC X.
                   20  KFZ-FILLER          PIC X(154).
      *
      *****         H A F T P F L I C H T - V E R S .
      *
               10  AMS-HAFT  REDEFINES  VARIABEL-AMS.
                   20  H-ZWG               PIC 99.
                   20  H-ZWGT              PIC X(3).
                   20  H-BTG               PIC 9(7)V99        COMP-3.
                   20  H-WAGN1             PIC 9(7)           COMP-3.
                   20  H-WAGN2             PIC 9(7)           COMP-3.
                   20  H-VSUP              PIC 9(5)           COMP-3.
                   20  H-VSUS              PIC 9(5)           COMP-3.
                   20  H-VSUV              PIC 9(5)           COMP-3.
                   20  H-VSUE              PIC 9(5)           COMP-3.
                   20  H-WAGNMENG          PIC 9(7)           COMP-3.
                   20  H-WAGNTEXT          PIC X(6).
                   20  H-KZW               PIC X.
                   20  H-STERN             PIC X.
                   20  H-FILLER            PIC X(163).
      *
      *****         U N F A L L - V E R S .
      *
               10  AMS-UNFALL  REDEFINES  VARIABEL-AMS.
                   20  U-ZWG               PIC 99.
                   20  U-ZWGT              PIC X(3).
                   20  U-BTG               PIC 9(7)V99        COMP-3.
                   20  U-KZBG              PIC X.
                   20  U-DATEN          OCCURS 5.
                       30  U-GBJAHR        PIC 99.
                       30  U-UMFANG        PIC 9.
                       30  U-TART          PIC XX.
                       30  U-VSUTOD        PIC 9(7)           COMP-3.
                       30  U-VSUINV        PIC 9(7)           COMP-3.
                       30  U-VSUKTG        PIC 9(3)           COMP-3.
                       30  U-VSUTG         PIC 9(3)           COMP-3.
                       30  U-VSUGG         PIC 9(3)           COMP-3.
                       30  U-VSUHK         PIC 9(3)           COMP-3.
                   20  U-STERN             PIC X.
                   20  U-FILLER            PIC X(88).
      *
      *****         BESTANDSAETZE CKV
      *
       01  CKV.
           05  FILLER                      PIC X(12).
           05  CKV1.
               10  FILLER                  PIC X(362).
      *
      *****         VARIABLER TEIL CKV
      *
               10  VARIABEL-CKV.
                   20  KVNAME              PIC X(10).
                   20  KGEBD               PIC 9(7)           COMP-3.
                   20  KGESCHL             PIC 9.
                   20  KVBEGP              PIC 9(4).
                   20  KVABLP              PIC 9(4).
                   20  KGS                 PIC XXX.
                   20  KLAST               PIC X.
                   20  KTARIFTAB           OCCURS 10.
                       30  KZUGV           PIC 9(4).
                       30  KTARIF          PIC X(8).
                       30  KFORM           PIC X.
                       30  KBEITR          PIC S9(5)V99       COMP-3.
                       30  KSBET           PIC S9(5)          COMP-3.
                       30  KLEIST          PIC S9(5)          COMP-3.
                       30  KABGV           PIC 9(4).
      *
      *****         BESTANDSAETZE BADENIA
      *
       01  BADENIA.
           05  FILLER                      PIC X(12).
           05  BADENIA1.
               10  FILLER                  PIC X(362).
      *
      *****    VARIABLER TEIL BADENIA
      *
               10  VARIABEL-BADENIA.
                   20  KPRODB              PIC 99.
                   20  KLAUFZ              PIC XX.
                   20  KVSUM               PIC S9(7)          COMP-3.
                   20  KGHABEN             PIC S9(7)V99       COMP-3.
                   20  KBEITRAG            PIC S9(5)V99       COMP-3.
                   20  KDARL               PIC S9(9)          COMP-3.
                   20  KKONTOST            PIC X.
                   20  KBWZAHL             PIC S99V9(5)       COMP-3.
140596*            20  KARBGEBER.
140596*                30  KFIRMA          PIC X(20).
140596*                30  KFSITZ          PIC X(33).
140596             20  KTARIF-T3           PIC X(01).
140596             20  KZINS-ALLG          PIC S9(3)V99       COMP-3.
140596             20  KZINS-DARLEHEN      PIC S9(3)V99       COMP-3.
140596             20  KDISAGIO            PIC X(1).
140596             20  KFREI-BETRAG        PIC S9(5)V99       COMP-3.
140596             20  KVFREI-BETRAG       PIC S9(5)V99       COMP-3.
140596             20  KFEHL-BETRAG        PIC S9(7)          COMP-3.
140596             20  KENDE-ST-LAUFZEIT   PIC S9(9)          COMP-3.
140596             20  KUMS-TABELLE.
140596                 25 KUMS-TAB         OCCURS 5.
140596                    30 KUMS-BETRAG   PIC S9(7)V99       COMP-3.
140596                    30 KUMS-SCHL     PIC S9(3)          COMP-3.
140596                    30 KUMS-DATUM    PIC S9(7)          COMP-3.
140596             20  KDATUM-WOHN         PIC S9(9)          COMP-3.
140596             20  KDATUM-ZUTEIL       PIC S9(9)          COMP-3.
140596             20  KBLAND              PIC X(2).
140596             20  KFEHL-BETRAG-DATUM  PIC S9(09)         COMP-3.
      *
      *****        BESTANDSAETZE RECHTSSCHUTZ
      *
       01  RECHTSSCHUTZ.
           05  FILLER                      PIC X(12).
           05  RECHTSSCHUTZ1.
               10  FILLER                  PIC X(362).
      *
      *****    VARIABLER TEIL RECHTSSCHUTZ
      *
               10  VARIABEL-RECHTSSCHUTZ.
                   20  KSPARTE             PIC X.
                   20  KLASTR              PIC X.
                   20  KBAKKZ              PIC X.
                   20  KFAEL               PIC XX.
                   20  KPRAEMIE            PIC 9(9)V99        COMP-3.
                   20  KVSU                PIC 9(7)           COMP-3.
                   20  KVARTTB1            OCCURS 5.
                       30 KWAG1            PIC X.
                       30 KLBAPRO1         PIC S9(5)          COMP-3.
                       30 KLBADAT1         PIC 9(7)           COMP-3.
                       30 KVART1           PIC XXX.
                       30 KVART1-ADV REDEFINES KVART1 PIC 9(5) COMP-3.
                       30 KVTEXT1          PIC X(25).
                   20  KVARTTB2            OCCURS 5.
                       30 KVART2           PIC XXX.
      *
      *****        BESTANDSAETZE DIT III
      *
       01  BANKEN.
           05  FILLER                      PIC X(12).
           05  BANKEN1.
               10  FILLER                  PIC X(362).
      *
      *****    VARIABLER TEIL DIT III
      *
               10  VARIABEL-BANK.
                   20  KPRODTAB            OCCURS 16.
                       30  KPROD           PIC XX.
                       30  KSUMME          PIC S9(9)           COMP-3.
                       30  KKTO            PIC S9(6)V999       COMP-3.
                   20  KPOSTEMPF.
                       30  KPOSTNAME       PIC X(36).
                       30  KPOSTZUS        PIC X(21).
                       30  KPOSTSTR        PIC X(25).
                       30  KPOSTPLZ        PIC X(4).
                       30  KPOSTORT        PIC X(25).
      *
      *****        FOLGESAETZE CKV
      *
       01  FOLGE-CKV.
           05  FILLER                      PIC X(12).
           05  FOLGE-CKV1.
               10  FILLER                  PIC X(124).
               10  FOLGE-CKV-TEIL.
                   20  KFUPDAT             PIC 9(7)           COMP-3.
                   20  FCKV.
                       25  KFUPKZ              PIC 9.
                       25  KFVNAME             PIC X(10).
                       25  KFGEBD              PIC 9(7)       COMP-3.
                       25  KFGESCHL            PIC 9.
                       25  KFVBEGP             PIC 9(4).
                       25  KFVABLP             PIC 9(4).
                       25  KFGS                PIC XXX.
                       25  KFLAST              PIC X.
                       25  KFTARIFTB           OCCURS 10.
                           30  KFZUGV          PIC 9(4).
                           30  KFTARIF         PIC X(8).
                           30  KFFORM          PIC X.
                           30  KFBEITR         PIC S9(5)V99      COMP-3.
                           30  KFSBET          PIC S9(5)         COMP-3.
                           30  KFLEIST         PIC S9(5)         COMP-3.
                           30  KFABGV          PIC 9(4).
      *
      *****     F O L G E - S A E T Z E   A M - S A C H
      *
       01  FOLGE-AMS.
           05  FILLER                      PIC X(12).
           05  FOLGE-AMS1.
               10  FILLER                  PIC X(124).
               10  AMSF-UPDAT              PIC 9(7)           COMP-3.
               10  AMSF-UPKZ               PIC 9.
               10  AMSF-ZWEIG              PIC X(205).
      *
      *****         S A C H - V E R S .         >>> FOLGE <<<
      *
               10  AMSF-SACH  REDEFINES  AMSF-ZWEIG.
                   20  SACHF-DATEN      OCCURS 4.
                       30  SF-ZWG          PIC 99.
                       30  SF-ZWGT         PIC X(3).
                       30  SF-BTG          PIC 9(7)V99        COMP-3.
                       30  SF-VGEF         PIC X(21).
                       30  SF-VUMF         PIC X(3).
                       30  SF-VSU          PIC 9(9)           COMP-3.
                       30  SF-HFI          PIC XX.
                       30  SF-SB           PIC XX.
                       30  SF-VHB          PIC X(5).
                       30  SF-GN           PIC XX.
                       30  SF-KZW          PIC X.
                   20  SF-STERN            PIC X.
      *
      *****       H A F T P F L I C H T - V E R S .    >>> FOLGE <<<
      *
               10  AMSF-HAFT  REDEFINES  AMSF-ZWEIG.
                   20  HF-ZWG              PIC 99.
                   20  HF-ZWGT             PIC X(3).
                   20  HF-BTG              PIC 9(7)V99        COMP-3.
                   20  HF-WAGN1            PIC 9(7)           COMP-3.
                   20  HF-WAGN2            PIC 9(7)           COMP-3.
                   20  HF-VSUP             PIC 9(5)           COMP-3.
                   20  HF-VSUS             PIC 9(5)           COMP-3.
                   20  HF-VSUV             PIC 9(5)           COMP-3.
                   20  HF-VSUE             PIC 9(5)           COMP-3.
                   20  HF-WAGNMENG         PIC 9(7)           COMP-3.
                   20  HF-WAGNTEXT         PIC X(6).
                   20  HF-KZW              PIC X.
                   20  HF-STERN            PIC X.
                   20  HF-FILLER           PIC X(163).
      *
      *****        U N F A L L - V E R S .          >>> FOLGE <<<
      *
               10  AMSF-UNFALL  REDEFINES  AMSF-ZWEIG.
                   20  UF-ZWG              PIC 99.
                   20  UF-ZWGT             PIC X(3).
                   20  UF-BTG              PIC 9(7)V99        COMP-3.
                   20  UF-KZBG             PIC X.
                   20  UF-DATEN         OCCURS 5.
                       30  UF-GBJAHR       PIC 99.
                       30  UF-UMFANG       PIC 9.
                       30  UF-TART         PIC XX.
                       30  UF-VSUTOD       PIC 9(7)           COMP-3.
                       30  UF-VSUINV       PIC 9(7)           COMP-3.
                       30  UF-VSUKTG       PIC 9(3)           COMP-3.
                       30  UF-VSUTG        PIC 9(3)           COMP-3.
                       30  UF-VSUGG        PIC 9(3)           COMP-3.
                       30  UF-VSUHK        PIC 9(3)           COMP-3.
                   20  UF-STERN            PIC X.
                   20  UF-FILLER           PIC X(88).


