      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1126.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1126.CPv  $
      *
      *   Rev 2.6   Jan 18 2005 15:49:32   t13ex27
      *AKI00448: Neue AM-Leben Tarife nach AEG
      *
      *   Rev 2.5   Jul 12 2003 17:27:38   t13ae44
      *aki00327
      *
      *   Rev 2.4   Jan 17 2003 17:17:02   t13ae44
      *AKI00303 - KoKü
      *
      *   Rev 2.3   Jul 11 2002 19:54:36   t13ae44
      *AKI00274 - DB 24 neu
      *
      *   Rev 2.2   11 Sep 2001 12:12:50   t13ex39
      *FKI00505 KDNR+Personenname
      *
      *   Rev 2.1   31 Jan 2001 10:38:30   T13EX49
      *AKI00068 - erstmalige
      *Produktionsübergabe
      *
      *   Rev 2.0   07 Jun 2000 17:08:18   P13SP20
      *Archiverstellung
      *
      *--------------------  L o g b u c h  ---------------------------*
      *DATUM   !Auftrag ! Änderung                          !Name      *
      *--------!--------!-----------------------------------!----------*
      *19.09.00!AKI00068! Neuerstellung                     !P. Pohl   *
      *----------------------------------------------------------------*
      *07.09.01!FKI00505! KDNR aud 10 Stellen               !WHO       *
      *        !        ! erweitert                         !          *
      *----------------------------------------------------------------*
      *08.07.02!AKI00274! Z07-FOND-STAND neu;               !R. Biehl  *
      *        !        ! versch. Feldlaengenaenderungen    !          *
      *--------!--------!-----------------------------------!----------*
      *20.11.02!AKI00303! KOMMBETR neu (bei allen Ges.);    !R. Biehl  *
      *        !        ! versch. Feldlaengenanpassungen    !          *
      *        !        ! wegen neuem Feld KOMMBETR         !          *
      *--------!--------!-----------------------------------!----------*
      *AKI00448!17.11.04! Z04-TARIF eine Stelle vergrößert  !I. Frenzel*
      *        !        !                                   !          *
       01  WS-ZEILE.
      *    05  VORS                PIC X       VALUE SPACE.
      *    05  INH                 PIC X(132)  VALUE SPACE.
           05  INH                 PIC X(133)  VALUE SPACE.
       01  WS-Z01       REDEFINES  WS-ZEILE.
           05  FILLER              PIC X(97).
           05  Z01-INF1.
               07  Z01-DIR         PIC 999.
               07  Z01-TRENN       PIC X(3).
               07  Z01-VBNR        PIC 9(6).
               07  Z01-TRENN-VBNR  PIC X.
               07  Z01-PRZ-VBNR    PIC X.
               07  FILLER          PIC X(22).
           05  Z01-INF2  REDEFINES     Z01-INF1.
070901*        07  Z01-KDNR        PIC 9(7).
070901         07  Z01-KDNR        PIC Z(10).
070901*        07  Z01-TRENN-KDNR  PIC X.
      ****     07  Z01-PRZ-KDNR    PIC 99.
070901*        07  Z01-PRZ-KDNR    PIC X(2).
               07  FILLER          PIC X(2).
               07  Z01-BLTEXT      PIC X(3).
               07  Z01-BLATT       PIC ZZ9.
               07  FILLER          PIC X(18).
       01  WS-Z01A      REDEFINES  WS-ZEILE.
           05  FILLER              PIC X(2).
           05  Z01-TEXT            PIC X(20).
           05  FILLER              PIC X(111).
       01  WS-Z02        REDEFINES     WS-ZEILE.
           05  FILLER              PIC XX.
           05  Z02-INF0.
               07  FILLER          PIC X(98).
      *        07  Z02-DAT         PIC X(8).
      *        07  FILLER          PIC X(25).
               07  Z02-DAT         PIC X(10).
               07  FILLER          PIC X(23).
           05  Z02-INF1 REDEFINES  Z02-INF0.
               07  Z02-NAME        PIC X(38).
               07  FILLER          PIC XX.
               07  Z02-STR         PIC X(26).
               07  FILLER          PIC X(13).
      *        07  Z02-GEBD        PIC X(8).
      *        07  FILLER          PIC X(44).
               07  Z02-GEBD        PIC X(10).
               07  FILLER          PIC X(42).
           05  Z02-INF2 REDEFINES  Z02-INF0.
               07  FILLER          PIC X(36).
               07  Z02-AUSL        PIC X(3).
               07  FILLER          PIC X.
               07  Z02-PLZ         PIC X(5).
               07  FILLER          PIC XX.
               07  Z02-ORT         PIC X(21).
               07  FILLER          PIC X(29).
               07  Z02-ANZPKT      PIC X(3).
               07  FILLER          PIC X.
               07  Z02-STERN       PIC X.
               07  FILLER          PIC X(29).
       01  WS-Z03        REDEFINES     WS-ZEILE.
           05  FILLER              PIC XX.
           05  Z03-T1.
               07  Z03-TEXT        PIC X(17).
      *        07  FILLER          PIC X(05).
      *    05  Z03-T2    REDEFINES     Z03-T1      PIC X(22).
      *    05  FILLER              PIC X(55).
               07  Z03-DIT         PIC X(10).
080702         07  FILLER          PIC X(6).
080702*    05  Z03-T2    REDEFINES     Z03-T1      PIC X(27).
080702     05  Z03-T2    REDEFINES     Z03-T1      PIC X(33).
080702*    05  FILLER              PIC X(50).
080702     05  FILLER              PIC X(44).
           05  Z03-TEX             PIC X(12).
           05  FILLER              PIC X(42).
      *                                                      *
      *****             A M - L E B E N                  *****
      *                                                      *
       01  WS-Z04        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X.
           05  Z04-BW              PIC X.
           05  Z04-VSNR.
               07  Z04-VNR         PIC X(16).
201102*        07  FILLER          PIC XX.
201102         07  Z04-KOMMBETR    PIC X(3).
           05  Z04-DYN   REDEFINES     Z04-VSNR.
               07  FILLER          PIC X(6).
               07  Z04-DYNVON      PIC X(3).
               07  Z04-DYNBIS      PIC Z9.
               07  Z04-DYNTEXT     PIC X(6).
               07  FILLER          PIC X.
171104*    05  Z04-TARIF           PIC X(10).
171104     05  Z04-TARIF           PIC X(11).
201102*    05  FILLER              PIC X(3).
201102*    05  FILLER              PIC X(2).
171104     05  FILLER              PIC X(1).
           05  Z04-BEG             PIC 9(6).
           05  FILLER              PIC XX.
           05  Z04-ABL             PIC 9(6).
           05  FILLER              PIC X.
      *    05  Z04-BTRFR           PIC X(4).
           05  Z04-BTRFR           PIC X(5).
           05  Z04-ZAHLWEISE REDEFINES Z04-BTRFR.
      *        07  Z04-INKA        PIC X.
               07  Z04-INKA        PIC XX.
               07  FILLER          PIC X.
               07  Z04-ZAHLW       PIC XX.
      *    05  FILLER              PIC XX.
           05  FILLER              PIC X.
           05  Z04-BEITR           PIC Z(4)9,99-.
           05  FILLER              PIC X.
           05  Z04-SUM.
               07  FILLER          PIC X.
               07  Z04-S1          PIC Z.ZZZ.ZZ9-.
           05  Z04-SUM1  REDEFINES     Z04-SUM.
               07  Z04-S2          PIC ZZZ.ZZ9,99-.
           05  FILLER              PIC XX.
           05  Z04-DAR             PIC Z.ZZZ.ZZ9,99-  BLANK WHEN ZERO.
           05  FILLER              PIC XX.
           05  Z04-BA              PIC XX.
           05  FILLER              PIC X.
           05  Z04-BADAT           PIC 99.99.99.
           05  FILLER              PIC X(20).
       01  WS-Z05        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(8).
           05  Z05-TEXT            PIC X(12).
           05  Z05-NAME            PIC X(24).
           05  FILLER              PIC XX.
           05  Z05-STR             PIC X(22).
           05  FILLER              PIC X(5).
           05  Z05-PLZ             PIC X(5).
           05  FILLER              PIC XX.
           05  Z05-ORT             PIC X(21).
           05  FILLER              PIC XX.
      *    05  Z05-GEBD            PIC 99.99.99.
      *    05  Z05-GEBD            PIC Z9.99.9999 blank when zero.
           05  Z05-GEBD            PIC X(10).
      *    05  FILLER              PIC X(22).
           05  FILLER              PIC X(20).
      *                                                      *
      *****           B A N K E N                        *****
      *                                                      *
       01  WS-Z07        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X.
           05  Z07-BW              PIC X.
      *    05  Z07-VNRD            PIC X(12).
020603     05  Z07-VNRD            PIC X(13).
201102     05  Z07-KOMMBETR        PIC X(3).

           05  Z07-FOND.
201102             10  Z07-FOND-DIT        PIC X(15).
                   10  FILLER              PIC X(01).
020603*            10  Z07-BEG-DAT         PIC X(06).
020603             10  Z07-BEG-DAT         PIC X(10).
020603*            10  FILLER              PIC X(03).
020603             10  FILLER              PIC X(01).
020603             10  Z07-ABL-DAT         PIC X(11).
080702             10  Z07-ANT             PIC X(9).
                   10  Z07-BETR-ANT        PIC ZZZ.ZZ9,999-.
020603*            10  FILLER              PIC X(8).
020603             10  FILLER              PIC X(3).
030702             10  Z07-WERT            PIC X(10).
                   10  Z07-BETR-WERT       PIC ZZZ.ZZZ.ZZ9-.
080702             10  FILLER              PIC X(1).
080702             10  Z07-FOND-STAND      PIC X(15).
080702*            10  FILLER              PIC X(23).
020603             10  FILLER              PIC X(15).

020603     05  Z07-NICHT-FOND REDEFINES Z07-FOND.
020603             10  Z07-NICHT-FOND-TEXT PIC X(31).
020603             10  FILLER              PIC X(01).
020603             10  Z07-NICHT-FOND-BEG  PIC X(11).
020603             10  FILLER              PIC X(01).
020603             10  Z07-NICHT-FOND-ABL  PIC X(11).
020603             10  FILLER              PIC X(01).
020603             10  Z07-NICHT-FOND-SUM-TEXT PIC X(19).
020603             10  Z07-N-F-SUM REDEFINES Z07-NICHT-FOND-SUM-TEXT.
020603                 15 Z07-NICHT-FOND-SUM PIC ZZZ.ZZZ.ZZZ.ZZ9,99-
020603                                           BLANK WHEN ZERO.
020603             10  FILLER              PIC X(01).
020603             10  Z07-NICHT-FOND-LIM-TEXT PIC X(19).
020603             10  Z07-N-F-LIM REDEFINES Z07-NICHT-FOND-LIM-TEXT.
020603                 15 Z07-NICHT-FOND-LIM PIC ZZZ.ZZZ.ZZZ.ZZ9,99-
020603                                           BLANK WHEN ZERO.
020603             10  FILLER              PIC X(20).
      *                                                      *
      *****                   C K V                      *****
      *                                                      *
       01  WS-Z08        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X.
           05  Z08-BW              PIC X.
           05  Z08-VNRC            PIC X(8).
           05  WS-Z08-CKV1.
201102         07  Z08-KOMMBETR    PIC X(3).
               07  FILLER          PIC X(20).
               07  Z08-BEG         PIC 9(6).
               07  FILLER          PIC XX.
               07  Z08-ABL         PIC 9(6)             BLANK WHEN ZERO.
               07  FILLER          PIC XX.
               07  Z08-LAST        PIC X(3).
               07  FILLER          PIC X(32).
               07  Z08-TEXT-GS     PIC X(4).
               07  Z08-GS          PIC X(3).
               07  FILLER          PIC X(42).
           05  WS-Z08-CKV2  REDEFINES  WS-Z08-CKV1.
               07  FILLER          PIC X(6).
               07  Z08-TEXT        PIC X(4).
               07  Z08-VNAME       PIC X(10).
               07  FILLER          PIC X(4).
      *        07  Z08-PBEG        PIC 99.99.
               07  Z08-PBEG        PIC X(5).
               07  FILLER          PIC X(3).
      *        07  Z08-PABL        PIC 99.99.
               07  Z08-PABL        PIC X(5).
               07  FILLER          PIC X(4).
               07  Z08-TEX1        PIC X(9).
      *        07  Z08-GEBD        PIC X(8).
      *        07  FILLER          PIC X(3).
               07  Z08-GEBD        PIC Z9.99.9999 blank when zero.
               07  FILLER          PIC X(1).
               07  Z08-TEX2        PIC X(25).
               07  FILLER          PIC X(37).
           05  WS-Z08-CKV3  REDEFINES  WS-Z08-CKV1.
               07  FILLER          PIC X(10).
               07  Z08-TAR         PIC X(8).
               07  FILLER          PIC X(6).
      *        07  Z08-ZUGV        PIC 99.99.
               07  Z08-ZUGV        PIC X(5).
               07  FILLER          PIC X(3).
      *        07  Z08-ABGV        PIC 99.99.
               07  Z08-ABGV        PIC X(5).
               07  FILLER          PIC X(7).
               07  Z08-BEITR       PIC Z(4)9,99-.
               07  FILLER          PIC X.
               07  Z08-TEXT-BEITR  PIC X(8).
               07  FILLER          PIC X.
               07  Z08-SB          PIC ZZ.ZZ9-     BLANK WHEN ZERO.
               07  FILLER          PIC X(6).
               07  Z08-VSL         PIC ZZ.ZZ9-     BLANK WHEN ZERO.
               07  FILLER          PIC X(40).
      *                                                      *
      *****                B A D E N I A                 *****
      *                                                      *
       01  WS-Z10        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X.
           05  Z10-BW              PIC X.
           05  Z10-VNRBAD          PIC X(12).
           05  Z10-VNRBAD1  REDEFINES Z10-VNRBAD.
               07  Z10-VNRB1       PIC X(11).
               07  Z10-VNRB2       PIC X.
201102*    05  FILLER              PIC X(6).
201102     05  FILLER              PIC X.
201102     05  Z10-KOMMBETR        PIC X(3).
201102     05  FILLER              PIC X(2).
           05  Z10-PROD            PIC X(12).
           05  FILLER              PIC X.
           05  Z10-BEG             PIC 9(6).
           05  FILLER              PIC XX.
           05  Z10-ABL             PIC 9(6)        BLANK WHEN ZERO.
      **   05  FILLER              PIC X(3).
      **   05  Z10-ZW              PIC X.
           05  FILLER              PIC X(4).
           05  Z10-ZW              PIC XX.
           05  FILLER              PIC X(1).
           05  Z10-BTR             PIC Z(4)9,99-   BLANK WHEN ZERO.
           05  FILLER              PIC XX.
           05  Z10-VSUM            PIC Z.ZZZ.ZZ9-.
           05  FILLER              PIC XX.
           05  Z10-DARL            PIC ZZZ.ZZZ.ZZ9- BLANK WHEN ZERO.
           05  FILLER              PIC X(44).
       01  WS-Z11        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z11-TEX1            PIC X(14).
           05  Z11-KONTO           PIC X.
           05  FILLER              PIC X(11).
      *    05  FILLER              PIC X(8).
           05  Z11-TEX2            PIC X(10).
           05  Z11-GUTH            PIC Z.ZZZ.ZZ9,99-.
      *    05  FILLER              PIC X(5).
      *    05  Z11-TEX3            PIC X(10).
           05  FILLER              PIC X(4).
           05  Z11-TEX3            PIC X(11).
           05  FILLER              PIC X(1).
           05  Z11-DARL            PIC ZZZ.ZZZ.ZZ9- BLANK WHEN ZERO.
      *    05  FILLER              PIC X(7).
      *    05  Z11-TEX3            PIC X(16).
      *    05  Z11-BEWZ            PIC Z9,99999-.
      *    05  FILLER              PIC X(34).
      *    05  FILLER              PIC X(35).
       01  WS-Z11A       REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z11A-TEX1           PIC X(13).
           05  FILLER              PIC X(02).
           05  Z11A-FEHL-BETRAG    PIC Z.ZZZ.ZZ9-   BLANK WHEN ZERO.
           05  FILLER              PIC X(01).
           05  Z11A-TEX2           PIC X(14).
           05  FILLER              PIC X(01).
           05  Z11A-BEWZ           PIC ZZ9,99999-.
      *    05  FILLER              PIC X(01).
           05  FILLER              PIC X(02).
           05  Z11A-TEX3           PIC X(11).
           05  FILLER              PIC X(02).
           05  Z11A-DATUM-ZUTEIL   PIC Z9.99.9999   BLANK WHEN ZERO.
       01  WS-Z11B        REDEFINES    WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z11B-TEX1           PIC X(13).
           05  FILLER              PIC X(02).
           05  Z11B-FREI-BETRAG    PIC ZZ.ZZ9,99-   BLANK WHEN ZERO.
           05  FILLER              PIC X(01).
           05  Z11B-TEX2           PIC X(38).
           05  FILLER              PIC X(04).
           05  Z11B-VFREI-BETRAG   PIC Z(04)9,99-   BLANK WHEN ZERO.
       01  WS-Z11C        REDEFINES    WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z11C-TEX1           PIC X(13).
           05  FILLER              PIC X(01).
           05  Z11C-DATUM-WOHN     PIC Z9.99.9999 BLANK WHEN ZERO.
           05  FILLER              PIC X(02).
           05  Z11C-TEX2           PIC X(38).
           05  FILLER              PIC X(02).
           05  Z11C-ENDE-ST-LAUFZ  PIC Z9.99.9999 BLANK WHEN ZERO.
       01  WS-Z11V       REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z11V-TEX1           PIC X(13).
           05  FILLER              PIC X(05).
           05  Z11V-ZINS-NOMINAL   PIC ZZ9,99-    BLANK WHEN ZERO.
           05  FILLER              PIC X(01).
           05  Z11V-TEX2           PIC X(14).
           05  FILLER              PIC X(05).
           05  Z11V-DARLEHEN-ZINS  PIC ZZ9,99-    BLANK WHEN ZERO.
           05  FILLER              PIC X(01).
           05  Z11V-TEX3           PIC X(11).
           05  FILLER              PIC X(02).
      *    05  Z11V-DISAGIO        PIC X(01).
           05  Z11V-DISAGIO        PIC ZZ9,99-    BLANK WHEN ZERO.

      *                                                      *
      *****           R E C H T S S C H U T Z            *****
      *                                                      *
       01  WS-Z12        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X.
           05  Z12-BW              PIC X.
           05  Z12-VNRR            PIC X(16).
201102*    05  FILLER              PIC XX.
201102     05  Z12-KOMMBETR        PIC X(3).
           05  Z12-SPARTE          PIC X(12).
           05  FILLER              PIC X.
           05  Z12-ROL1.
               10  Z12-BEG             PIC 9(6).
201102*        10  FILLER              PIC XX.
201102         10  FILLER              PIC X.
               10  Z12-ABL             PIC 9(6)         BLANK WHEN ZERO.
      **       10  FILLER              PIC X(3).
      **       10  Z12-ZAHLW           PIC X.
               10  FILLER              PIC X(4).
               10  Z12-ZAHLW           PIC XX.
               10  FILLER              PIC X(1).
               10  Z12-BEITR           PIC Z(4)9,99.
               10  FILLER              PIC X.
               10  Z12-VSU             PIC ZZZ.ZZZ.ZZ9  BLANK WHEN ZERO.
               10  FILLER              PIC X(6).
               10  Z12-VTEXT1          PIC X(25).
               10  FILLER              PIC X(28).
           05  Z12-ROL2  REDEFINES     Z12-ROL1.
201102*        10  FILLER              PIC X(16).
201102         10  FILLER              PIC X(15).
               10  Z12-LAST            PIC X(3).
               10  FILLER              PIC X(3).
               10  Z12-LBATEXT         PIC X(3).
               10  Z12-LBADAT          PIC 99.99    BLANK WHEN ZERO.
               10  FILLER              PIC X(17).
               10  Z12-VTEXT2          PIC X(25).
               10  FILLER              PIC X(28).
           05  Z12-ROL3  REDEFINES     Z12-ROL1.
201102*        10  FILLER              PIC X(47).
201102         10  FILLER              PIC X(46).
               10  Z12-VTEXT           PIC X(25).
               10  FILLER              PIC X(28).
           05  Z12-ROL4  REDEFINES     Z12-ROL1.
201102*        10  FILLER              PIC X(47).
201102         10  FILLER              PIC X(46).
               10  Z12-VART     OCCURS 5.
                   15  Z12-VA          PIC X(3).
                   15  FILLER          PIC X.
               10  FILLER              PIC X(33).
      *                                                      *
      *****          ANSCHRIFT PRO VERTRAG               *****
      *                                                      *
       01  WS-Z14        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(8).
           05  Z14-TEXT            PIC X(12).
           05  Z14-NAME            PIC X(24).
           05  FILLER              PIC XX.
           05  Z14-STR             PIC X(22).
           05  FILLER              PIC X.
           05  Z14-AUSL            PIC X(3).
           05  FILLER              PIC X.
      *    05  Z14-PLZ             PIC X(5).
           05  Z14-PLZ             PIC X(6).
      *    05  FILLER              PIC XX.
           05  FILLER              PIC X.
           05  Z14-ORT             PIC X(21).
           05  FILLER              PIC XX.
      *    05  Z14-GEBD            PIC 99.99.99.
           05  Z14-GEBD            PIC Z9.99.9999 blank when zero.
      *                                                      *
      *****              A M - S A C H                   *****
      *                                                      *
      *****           SACH-VERSICHERUNGEN                *****
      *                                                      *
       01  WS-Z15        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X.
           05  Z15-BW              PIC X.
           05  Z15-VNR             PIC X(14).
201102*    05  FILLER              PIC X(4).
201102     05  Z15-KOMMBETR        PIC X(3).
201102     05  FILLER              PIC X.
           05  Z15-TAR             PIC XXX.
           05  FILLER              PIC X(10).
           05  Z15-BEG             PIC 9(6).
           05  FILLER              PIC XX.
           05  Z15-ABL             PIC 9(6).
      **   05  FILLER              PIC X(3).
      **   05  Z15-ZAHLW           PIC X.
           05  FILLER              PIC X(4).
           05  Z15-ZAHLW           PIC XX.
           05  FILLER              PIC X(1).
           05  Z15-BTR             PIC Z(4)9,99.
           05  FILLER              PIC X.
           05  Z15-VSU             PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC XXX.
           05  Z15-ANP             PIC X(4).
           05  FILLER              PIC XX.
           05  Z15-VUMF            PIC X(21).
           05  FILLER              PIC X(29).
      *                                                      *
      *****             UNFALL VERSICHERUNG              *****
      *                                                      *
       01  WS-Z16        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(67).
           05  Z16-TEXT            PIC X(28).
           05  Z16-DATEN REDEFINES     Z16-TEXT.
               07  Z16-GJ          PIC XX.
               07  FILLER          PIC X.
               07  Z16-TOD         PIC ZZZ9.
               07  Z16-ST1         PIC X.
               07  Z16-INV         PIC ZZZ9.
               07  Z16-ST2         PIC X.
               07  Z16-KTG         PIC ZZ9.
               07  Z16-ST3         PIC X.
               07  Z16-TG          PIC ZZ9.
               07  Z16-ST4         PIC X.
               07  Z16-GG          PIC ZZ9.
               07  Z16-ST5         PIC X.
               07  Z16-HK          PIC ZZ9.
           05  FILLER              PIC X(38).
      *                                                      *
      *****          HAFTPFLICHT VERSICHERUNG            *****
      *                                                      *
       01  WS-Z17        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(67).
           05  Z17-TEXT            PIC X(19).
           05  FILLER              PIC X(46).
       01  WS-Z18        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(34).
           05  Z18-TEX1            PIC X(13).
           05  Z18-WAGM            PIC Z(6)9.
           05  FILLER              PIC X.
           05  Z18-WAGT            PIC X(6).
           05  FILLER              PIC X(6).
           05  Z18-VSUP            PIC ZZZ9.
           05  Z18-ST1             PIC X.
           05  Z18-VSUS            PIC ZZZ9.
           05  Z18-ST2             PIC X.
           05  Z18-VSUV            PIC ZZZ9.
           05  Z18-ST3             PIC X.
           05  Z18-VSUE            PIC ZZZ9.
           05  FILLER              PIC X(47).
      *                                                      *
      *****             K F Z   VERSICHERUNG             *****
      *                                                      *
       01  WS-Z19        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z19-TAR             PIC XXX.
           05  FILLER              PIC X.
           05  Z19-WKZ             PIC 999         BLANK WHEN ZERO.
           05  FILLER              PIC X.
           05  Z19-RUHE            PIC X(4).
           05  FILLER              PIC X(35).
           05  Z19-TEX1            PIC XXX.
           05  Z19-KZ              PIC XX.
           05  Z19-TEX2            PIC X(5).
           05  Z19-BTR             PIC Z(4)9,99.
           05  FILLER              PIC XX.
           05  Z19-TEX3            PIC X(5).
      **** 05  Z19-JA              PIC XXX.
           05  Z19-JA              PIC 9(3).
           05  Z19-TEX4            PIC XX.
           05  FILLER              PIC X(36).
       01  WS-Z20        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z20-AKZ             PIC X(12).
           05  FILLER              PIC X.
           05  Z20-TEX1            PIC X(6).
      *    05  Z20-ZJ              PIC 99       BLANK WHEN ZERO.
           05  Z20-ZJ              PIC 99.
           05  FILLER              PIC X(92).
      *                                                      *
      *****     BAUWESEN- MASCHINEN VERSICHERUNG         *****
      *                                                      *
       01  WS-Z21        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(63).
           05  Z21-VSU             PIC ZZZ.ZZZ.ZZ9  BLANK WHEN ZERO.
           05  FILLER              PIC X(59).
      *                                                      *
      *****     TRANSPORT- LUFTUNFALL VERSICHERUNG       *****
      *                                                      *
       01  WS-Z22        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z22-TEX             PIC X(4).
           05  FILLER              PIC XX.
           05  Z22-TAB             OCCURS 10.
               07  Z22-SKZ         PIC XXX.
               07  FILLER          PIC X.
           05  FILLER              PIC X(67).
      *                                                      *
      *****               TIER VERSICHERUNG              *****
      *                                                      *
       01  WS-Z23        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(20).
           05  Z23-HINW            PIC X(16).
           05  FILLER              PIC X(97).
      *                                                                *
      *****  HINWEISZEILE FUER NICHT MEHR ANGELIEFERTE VERTRAEGE  ******
      *                                                                *
       01  WS-Z24        REDEFINES     WS-ZEILE.
           05  FILLER              PIC XX.
           05  Z24-HINW            PIC X(90).
           05  FILLER              PIC X(41).
      *                                                                *
      *****  ZEILE FUER DATENSCHUTZKLAUSEL                          ****
      *                                                                *
       01  WS-Z26        REDEFINES     WS-ZEILE.
           05  FILLER              PIC X(8).
           05  Z26-TEX             PIC X(50).
           05  FILLER              PIC X(75).
      *                                                                *
