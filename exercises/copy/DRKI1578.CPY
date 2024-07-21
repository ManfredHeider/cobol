      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1578.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1578.CPv  $
      *
      *   Rev 2.2   Oct 10 2003 13:49:26   t13ex48
      *FKI00780 Korrektur falscher Feldlängen im BA-FTP-Satz
      *
      *   Rev 2.1   Sep 29 2003 11:09:04   t13ex27
      *AKI00366 - Neue Besuchsauftrags-Art
      *
      *   Rev 2.0   May 19 2003 10:00:52   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00780!10.10.03!    ! Laenge von PLZ/Ort geae.  !H. Sebbach*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  BA-SATZ.
           05  BA-SA                      PIC X(001).
           05  BA-PC-NR                   PIC 9(005).
           05  BA-VSNR.
               10  BA-SPARTE              PIC 9(001).
               10  BA-NUMMER              PIC 9(007).
               10  BA-PRZ                 PIC 9(002).
           05  BA-ABS-DAT                 PIC 9(006).
           05  FILLER REDEFINES  BA-ABS-DAT.
               10  BA-ABS-DAT-JJ          PIC 99.
               10  BA-ABS-DAT-MM          PIC 99.
               10  BA-ABS-DAT-TT          PIC 99.
           05  BA-SKZ                     PIC 9           .
           05  BA-SKZ-DAT                 PIC 9(6)        .
           05  FILLER REDEFINES  BA-SKZ-DAT.
               10  BA-SKZ-DAT-JJ          PIC 99.
               10  BA-SKZ-DAT-MM          PIC 99.
               10  BA-SKZ-DAT-TT          PIC 99.
           05  BA-BA-ART                  PIC X           .
           05  BA-MANR.
               10  BA-ORGGS               PIC 999         .
               10  BA-AGT                 PIC 9999        .
           05 FILLER REDEFINES  BA-MANR.
               10  BA-ORG                 PIC 9.
               10  BA-GS                  PIC 99.
               10  FILLER                 PIC X(4).
           05  BA-MA-NAME                 PIC X(20)       .
           05  BA-PIF                     PIC X(15)       .
           05  BA-PROVZEIT.
               10  BA-PROV-VON            PIC 9(4)        .
               10  FILLER REDEFINES  BA-PROV-VON.
                   15  BA-PROV-VON-JJ     PIC 99.
                   15  BA-PROV-VON-MM     PIC 99.
               10  BA-PROV-BIS            PIC 9(4)        .
               10  FILLER REDEFINES  BA-PROV-BIS.
                   15  BA-PROV-BIS-JJ     PIC 99.
                   15  BA-PROV-BIS-MM     PIC 99.
           05  BA-KUE-DAT                 PIC 9(6)        .
           05  FILLER REDEFINES  BA-KUE-DAT.
               10  BA-KUE-DAT-JJ          PIC 99.
               10  BA-KUE-DAT-MM          PIC 99.
               10  BA-KUE-DAT-TT          PIC 99.
           05  BA-STOGRUND                PIC X(20)       .
           05  BA-RUE-DAT                 PIC 9(6)        .
           05  FILLER REDEFINES  BA-RUE-DAT.
               10  BA-RUE-DAT-JJ          PIC 99.
               10  BA-RUE-DAT-MM          PIC 99.
               10  BA-RUE-DAT-TT          PIC 99.
           05  BA-LDYN.
               10  BA-LDYN-NR             PIC 99          .
               10  BA-LDYN-DAT            PIC 9(6)        .
               10  FILLER REDEFINES  BA-LDYN-DAT.
                   15  BA-LDYN-DAT-JJ     PIC 99.
                   15  BA-LDYN-DAT-MM     PIC 99.
                   15  BA-LDYN-DAT-TT     PIC 99.
           05  BA-VN.
               10  BA-VN-NAME             PIC X(39)       .
               10  BA-VN-STR              PIC X(25)       .
101003*        10  BA-VN-PLZ              PIC X(9)        .
101003         10  BA-VN-PLZ              PIC X(6)        .
101003*        10  BA-VN-ORT              PIC X(26)       .
101003         10  BA-VN-ORT              PIC X(29)       .
               10  BA-VN-GEBDAT           PIC 9(6)        .
               10  FILLER REDEFINES  BA-VN-GEBDAT.
                   15  BA-VN-GEBDAT-JJ    PIC 99.
                   15  BA-VN-GEBDAT-MM    PIC 99.
                   15  BA-VN-GEBDAT-TT    PIC 99.
           05  BA-AG.
               10  BA-AG-NAME             PIC X(39)       .
               10  BA-AG-STR              PIC X(25)       .
101003*        10  BA-AG-PLZ              PIC X(6)        .
101003         10  BA-AG-PLZ              PIC X(9)        .
101003*        10  BA-AG-ORT              PIC X(26)       .
101003         10  BA-AG-ORT              PIC X(29)       .
           05  BA-ABT                     PIC X           .
           05  BA-LZE.
               10  BA-LZE-DAT             PIC 9(6)        .
               10  FILLER REDEFINES  BA-LZE-DAT.
                   15  BA-LZE-DAT-JJ      PIC 99.
                   15  BA-LZE-DAT-MM      PIC 99.
                   15  BA-LZE-DAT-TT      PIC 99.
               10  BA-LZE-BTG             PIC S9(7)V99     COMP-3.
           05  BA-INKST                   PIC 9(4)        .
           05  FILLER REDEFINES  BA-INKST.
               10  BA-INKST-JJ            PIC 99.
               10  BA-INKST-MM            PIC 99.
           05  BA-BTGRUECK                PIC S9(7)V99     COMP-3.
           05  BA-BANKVERB.
               10  BA-KTONR               PIC 9(10)       .
               10  BA-BLZ                 PIC 9(8)        .
           05  BA-MAHN-DAT                PIC 9(4)        .
           05  FILLER REDEFINES  BA-MAHN-DAT.
               10  BA-MAHN-DAT-JJ         PIC 99.
               10  BA-MAHN-DAT-MM         PIC 99.
           05  BA-TARIF                   PIC X(12)       .
           05  BA-VSU                     PIC S9(9)V99     COMP-3.
           05  BA-BEG                     PIC 9(4)        .
           05  FILLER REDEFINES  BA-BEG.
               10  BA-BEG-JJ              PIC 99.
               10  BA-BEG-MM              PIC 99.
           05  BA-ABL                     PIC 9(4)        .
           05  FILLER REDEFINES  BA-ABL.
               10  BA-ABL-JJ              PIC 99.
               10  BA-ABL-MM              PIC 99.
           05  BA-BTG                     PIC S9(7)V99     COMP-3.
           05  BA-ZAHLW                   PIC X(12)       .
           05  BA-INKART                  PIC X(12)       .
           05  BA-BFR                     PIC S9(9)V99     COMP-3.
           05  BA-RKW                     PIC S9(9)V99     COMP-3.
           05  BA-SE                      PIC S9(9)V99     COMP-3.
           05  BA-GEWGUT                  PIC S9(9)V99     COMP-3.
           05  BA-BWSE                    PIC S9(9)V99     COMP-3.
           05  BA-VZMOEGL                 PIC S9(9)V99     COMP-3.
           05  BA-VZBETR                  PIC S9(9)V99     COMP-3.
           05  BA-VZZINS                  PIC S9(7)V99     COMP-3.
           05  BA-ZINSSATZ                PIC S9(3)V99     COMP-3.
           05  BA-VERFUEG                 PIC X(12)       .
           05  BA-RZBTG                   PIC S9(7)V99     COMP-3.
           05  BA-BETR-KZ                 PIC 9           .
           05  BA-VBNR-ALT                PIC 9(6)        .
           05  BA-GEWVA                   PIC 9           .
           05  BA-ABLBZ                   PIC 9(4)        .
           05  FILLER REDEFINES  BA-ABLBZ.
               10  BA-ABLBZ-MM            PIC 99.
               10  BA-ABLBZ-JJ            PIC 99.
           05  BA-BETREFF                 PIC X           .
           05  BA-RSGRP                   PIC X(6)        .
           05  FILLER REDEFINES  BA-RSGRP.
               10  BA-RSGRP1-2            PIC X(2).
               10  BA-RSGRP3-5            PIC X(3).
               10  BA-RSGRP6              PIC X.
           05  BA-URSPRUNG                PIC X           .
           05  BA-BEITRAG-MIT-DYN         PIC S9(9)V99 COMP-3.
           05  BA-DYNABLKZN               PIC 9(002).
           05  BA-BTGVERF                 PIC X(001).
           05  BA-ERHPROZKZN              PIC 9(002).
           05  BA-BLZ-PRODGEBER           PIC 9(008).
           05  BA-KTONR-PRODGEBER         PIC 9(010).
      *01  FILLER REDEFINES  BA-SATZ.
      *    05  BA-KENNUNG                 PIC X(8).
      *    05  FILLER                     PIC X(528).
