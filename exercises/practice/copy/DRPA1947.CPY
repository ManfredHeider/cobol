      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  01.07.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1947.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1947.CPv  $
      *
      *   Rev 2.2   Jul 07 2020 15:33:42   t13ae37
      *FPA01775: Abend beim Aufruf einer EVS-Notiz 
      *
      *   Rev 2.1   Jul 03 2020 07:11:34   t13ex67
      *FPA01775: Abend beim Aufruf einer EVS-Notiz
      *
      *   Rev 2.0   Jul 01 2020 08:40:58   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1947
      *    --------------------------------------------------------
      *    EVS-NOTIZ (HISTORISCH, VOR DEM 26.06.2020 ERSTELLT)
      *    --------------------------------------------------------
      *    ERSTELLT: STORCH
      *    DATUM:    01.07.2020
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA01775!01.07.20!2.01! NEU                       ! STORCH   *
      * FPA01775!07.07.20!2.02! LÄNGE AUF 23200 ANGEPASST ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN ZUR EVS-NOTIZ; DIE LÄNGE    *
      *    DER NOTIZ BETRÄGT 23.200 BYTES.                           *
      *                                                              *
      *    ===================================================       *
      *     BEI LÄNGENÄNDERUNG AUCH COPY-STRECKE DRPA0977 UND        *
      *     DRPA1049, SOWIE PROGRAMM UPPA0103 ANPASSEN!              *
      *    ===================================================       *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1947-NZPA0011.
           05 DRPA1947-ZEILE-1.
              10 FILLER                             PIC  X(018)
                                            VALUE 'NZPA0011 EVS-NOTIZ'.
              10 DRPA1947-FILLER-ZL-1               PIC  X(062).
      *
           05 DRPA1947-ZEILE-2.
              10 DRPA1947-A-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA1947-B-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA1947-C-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA1947-D-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA1947-E-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA1947-F-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA1947-FILLER-ZL-2               PIC  X(074).
      *
      *
           05 DRPA1947-A-NOTIZ.
              10 DRPA1947-A-EINMAL.
                 15 DRPA1947-A-EVS-VNR              PIC  X(015).
                 15 DRPA1947-A-EVS-PGEB-GES         PIC S9(004) BINARY.
                 15 DRPA1947-A-EVS-PRINTE           PIC S9(015)
                                                        PACKED-DECIMAL.
                 15 DRPA1947-A-EVS-AVERM            PIC S9(009) BINARY.
                 15 DRPA1947-A-KNR-EVS-ERM-AVERM    PIC S9(004) BINARY.
                 15 DRPA1947-A-EE-VT-ENTSCHEIDUNG   PIC  X(001).
              10 DRPA1947-A-MAX-USED-VVNR           PIC S9(004) BINARY.
              10 DRPA1947-A-MAX-USED-VVNR-RED REDEFINES
                 DRPA1947-A-MAX-USED-VVNR           PIC  X(002).
              10 DRPA1947-A-VVNR-TABELLE            OCCURS 10
                                             INDEXED BY DRPA1947-A-IX1.
                 15 DRPA1947-A-PRINTE               PIC S9(015)
                                                        PACKED-DECIMAL.
                 15 DRPA1947-A-VVGES                PIC S9(004) BINARY.
                 15 DRPA1947-A-VVNR                 PIC  X(015).
                 15 DRPA1947-A-KD-BETR              PIC S9(009) BINARY.
                 15 DRPA1947-A-KNR-EVS-ERM-KDBETR   PIC S9(004) BINARY.
      *
      *
           05 DRPA1947-B-NOTIZ.
              10 DRPA1947-B-MAX-USED-PVPT           PIC S9(004) BINARY.
              10 DRPA1947-B-MAX-USED-PVPT-RED REDEFINES
                 DRPA1947-B-MAX-USED-PVPT           PIC  X(002).
              10 DRPA1947-B-PVPT-TABELLE            OCCURS 40
                                                    INDEXED BY
                                                        DRPA1947-B-IX1.
                 15 DRPA1947-B-PVPT-EINMAL.
                    20 DRPA1947-B-PROD-PROD-SCHL    PIC S9(004) BINARY.
                    20 DRPA1947-B-PVPT-LAUFZEIT     PIC S9(004) BINARY.
                    20 DRPA1947-B-PRKO-KOND-NR-EE   PIC S9(004) BINARY.
                    20 DRPA1947-B-EEP-KOND-NOT-FOUND
                                                    PIC  X(001).
                    20 DRPA1947-B-SUM-JAHRES-NT-PRAEM
                                                    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA1947-B-EVS-JAHRES-NT-PRAEM
                                                    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA1947-B-EVS-ERHOEHUNG     PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA1947-B-EVS-ERHOEHUNG-PROZ
                                                    PIC S9(003)V99
                                                        PACKED-DECIMAL.
                    20 DRPA1947-B-EE-PR-ENTSCHEIDUNG
                                                    PIC  X(001).
                 15 DRPA1947-B-MAX-USED-JNP         PIC S9(004) BINARY.
                 15 DRPA1947-B-MAX-USED-JNP-RED REDEFINES
                    DRPA1947-B-MAX-USED-JNP         PIC  X(002).
                 15 DRPA1947-B-JNP-TABELLE          OCCURS 10
                                             INDEXED BY DRPA1947-B-IX2.
                    20 DRPA1947-B-VVNR              PIC  X(015).
                    20 DRPA1947-B-JAHRES-NT-PRAEMIE PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA1947-B-KNR-WAEHRUNG      PIC S9(004) BINARY.
      *
      *
           05 DRPA1947-C-NOTIZ.
              10 DRPA1947-C-MAX-USED-PVPT           PIC S9(004) BINARY.
              10 DRPA1947-C-MAX-USED-PVPT-RED REDEFINES
                 DRPA1947-C-MAX-USED-PVPT           PIC  X(002).
              10 DRPA1947-C-PVPT-TABELLE            OCCURS 100
                                             INDEXED BY DRPA1947-C-IX1.
                 15 DRPA1947-C-PROD-PROD-SCHL       PIC S9(004) BINARY.
                 15 DRPA1947-C-PVPT-VT-PRIO         PIC S9(004) BINARY.
                 15 DRPA1947-C-VVNR                 PIC  X(015).
                 15 DRPA1947-C-PVPT-PPFL-BEGINN     PIC  X(010).
                 15 DRPA1947-C-PVPT-PPFL-ENDE       PIC  X(010).
                 15 DRPA1947-C-PVPT-BEWERT-BETRAG   PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-C-KNR-WAEHRUNG         PIC S9(004) BINARY.
                 15 DRPA1947-C-BEREINIGUNGS-KZ      PIC  X(001).
                 15 DRPA1947-C-KNR-EVS-BER-PVPT     PIC S9(004) BINARY.
      *
      *
           05 DRPA1947-D-NOTIZ.
              10 DRPA1947-D-MAX-USED-PVPT           PIC S9(004) BINARY.
              10 DRPA1947-D-MAX-USED-PVPT-RED REDEFINES
                 DRPA1947-D-MAX-USED-PVPT           PIC  X(002).
              10 DRPA1947-D-PVPT-TABELLE            OCCURS 40
                                             INDEXED BY DRPA1947-D-IX1.
                 15 DRPA1947-D-PROD-PROD-SCHL       PIC S9(004) BINARY.
                 15 DRPA1947-D-PVPT-VT-PRIO         PIC S9(004) BINARY.
                 15 DRPA1947-D-PVPT-PPFL-BEGINN     PIC  X(010).
                 15 DRPA1947-D-PVPT-PPFL-ENDE       PIC  X(010).
                 15 DRPA1947-D-PVPT-BEWERT-BETRAG   PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-D-KNR-WAEHRUNG         PIC S9(004) BINARY.
      *
      *
           05 DRPA1947-E-NOTIZ.
              10 DRPA1947-E-MAX-USED-VVNR           PIC S9(004) BINARY.
              10 DRPA1947-E-MAX-USED-VVNR-RED REDEFINES
                 DRPA1947-E-MAX-USED-VVNR           PIC  X(002).
              10 DRPA1947-E-VVNR-TABELLE            OCCURS 10
                                             INDEXED BY DRPA1947-E-IX1.
                 15 DRPA1947-E-VVNR-EINMAL.
                    20 DRPA1947-E-VVNR              PIC  X(015).
                    20 DRPA1947-E-RUECKBELASTUNG-KZ PIC  X(001).
                 15 DRPA1947-E-MAX-USED-PROD        PIC S9(004) BINARY.
                 15 DRPA1947-E-MAX-USED-PROD-RED REDEFINES
                    DRPA1947-E-MAX-USED-PROD        PIC  X(002).
                 15 DRPA1947-E-PROD-TABELLE         OCCURS 15
                                             INDEXED BY DRPA1947-E-IX2.
                    20 DRPA1947-E-PROD-PROD-SCHL    PIC S9(004) BINARY.
                    20 DRPA1947-E-RUECKBEL-BEGINN   PIC  X(010).
                    20 DRPA1947-E-RUECKBEL-ENDE     PIC  X(010).
                    20 DRPA1947-E-GSMT-PROV         PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA1947-E-KNR-WAEHRUNG      PIC S9(004) BINARY.
      *
      *
           05 DRPA1947-F-NOTIZ.
              10 DRPA1947-F-MAX-USED-PROD           PIC S9(004) BINARY.
              10 DRPA1947-F-MAX-USED-PROD-RED REDEFINES
                 DRPA1947-F-MAX-USED-PROD           PIC  X(002).
              10 DRPA1947-F-PROD-TABELLE            OCCURS 15
                                             INDEXED BY DRPA1947-F-IX1.
                 15 DRPA1947-F-PROD-PROD-SCHL       PIC S9(004) BINARY.
                 15 DRPA1947-F-JAHRES-NT-PRAEMIE    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-F-KNR-ZAHLWEISE        PIC S9(004) BINARY.
                 15 DRPA1947-F-PRKO-GSP-STZ         PIC S9(003)V999
                                                        PACKED-DECIMAL.
                 15 DRPA1947-F-GSMT-PROV-1          PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-F-GSMT-PROV-2          PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-F-PG-GSMT-PROV         PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-F-GSMT-PROV-RUECK      PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA1947-F-ABWEICHUNG           PIC S9(009)V99
                                                        PACKED-DECIMAL.
           05 DRPA1947-FILLER                       PIC  X(426).
070720     05 DRPA1947-FILLER                       PIC  X(200).
 

