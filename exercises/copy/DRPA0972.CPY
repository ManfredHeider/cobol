      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0972.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0972.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:29:08   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 27 1999 11:39:32   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0972                                      *
      *    --------------------------------------------------------   *
      *    EVS-NOTIZ                                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    18.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.06.98! 01 ! NEU                       ! WOLFF    *
      * AFPA012 !03.07.98! 01 ! PGEB-GES IN BL.A EINGEBAUT! FOLTZ     *
      *         !        !    ! NOTIZ-ID-ZEILE = FILLER   !          *
      * AFPA0012!27.08.98!    ! KOMPRIMIERUNG EVS-NOTIZ   ! WOLFF    *
      * APA02028!17.02.20!2.1 ! GES-UEBERGR EVS           ! SCHEIDEL *
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
       01  DRPA0972-NZPA0011.
           05 DRPA0972-ZEILE-1.
              10 FILLER                             PIC  X(018)
                                            VALUE 'NZPA0011 EVS-NOTIZ'.
              10 DRPA0972-FILLER-ZL-1               PIC  X(062).
      *
           05 DRPA0972-ZEILE-2.
              10 DRPA0972-A-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA0972-B-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA0972-C-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA0972-D-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA0972-E-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA0972-F-NOTIZ-GEFUELLT          PIC  X(001).
              10 DRPA0972-FILLER-ZL-2               PIC  X(074).
      *
      *
           05 DRPA0972-A-NOTIZ.
              10 DRPA0972-A-EINMAL.
                 15 DRPA0972-A-EVS-VNR              PIC  X(015).
                 15 DRPA0972-A-EVS-PGEB-GES         PIC S9(004) BINARY.
                 15 DRPA0972-A-EVS-PRINTE           PIC S9(015)
                                                        PACKED-DECIMAL.
                 15 DRPA0972-A-EVS-AVERM            PIC S9(009) BINARY.
                 15 DRPA0972-A-KNR-EVS-ERM-AVERM    PIC S9(004) BINARY.
                 15 DRPA0972-A-EE-VT-ENTSCHEIDUNG   PIC  X(001).
              10 DRPA0972-A-MAX-USED-VVNR           PIC S9(004) BINARY.
              10 DRPA0972-A-MAX-USED-VVNR-RED REDEFINES
                 DRPA0972-A-MAX-USED-VVNR           PIC  X(002).
              10 DRPA0972-A-VVNR-TABELLE            OCCURS 10
                                             INDEXED BY DRPA0972-A-IX1.
                 15 DRPA0972-A-PRINTE               PIC S9(015)
                                                        PACKED-DECIMAL.
170220           15 DRPA0972-A-VVGES                PIC S9(004) BINARY.
                 15 DRPA0972-A-VVNR                 PIC  X(015).
                 15 DRPA0972-A-KD-BETR              PIC S9(009) BINARY.
                 15 DRPA0972-A-KNR-EVS-ERM-KDBETR   PIC S9(004) BINARY.
      *
      *
           05 DRPA0972-B-NOTIZ.
              10 DRPA0972-B-MAX-USED-PVPT           PIC S9(004) BINARY.
              10 DRPA0972-B-MAX-USED-PVPT-RED REDEFINES
                 DRPA0972-B-MAX-USED-PVPT           PIC  X(002).
              10 DRPA0972-B-PVPT-TABELLE            OCCURS 40
                                                    INDEXED BY
                                                        DRPA0972-B-IX1.
                 15 DRPA0972-B-PVPT-EINMAL.
                    20 DRPA0972-B-PROD-PROD-SCHL    PIC S9(004) BINARY.
                    20 DRPA0972-B-PVPT-LAUFZEIT     PIC S9(004) BINARY.
                    20 DRPA0972-B-PRKO-KOND-NR-EE   PIC S9(004) BINARY.
                    20 DRPA0972-B-EEP-KOND-NOT-FOUND
                                                    PIC  X(001).
                    20 DRPA0972-B-SUM-JAHRES-NT-PRAEM
                                                    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA0972-B-EVS-JAHRES-NT-PRAEM
                                                    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA0972-B-EVS-ERHOEHUNG     PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA0972-B-EVS-ERHOEHUNG-PROZ
                                                    PIC S9(003)V99
                                                        PACKED-DECIMAL.
                    20 DRPA0972-B-EE-PR-ENTSCHEIDUNG
                                                    PIC  X(001).
                 15 DRPA0972-B-MAX-USED-JNP         PIC S9(004) BINARY.
                 15 DRPA0972-B-MAX-USED-JNP-RED REDEFINES
                    DRPA0972-B-MAX-USED-JNP         PIC  X(002).
                 15 DRPA0972-B-JNP-TABELLE          OCCURS 10
                                             INDEXED BY DRPA0972-B-IX2.
                    20 DRPA0972-B-VVNR              PIC  X(015).
                    20 DRPA0972-B-JAHRES-NT-PRAEMIE PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA0972-B-KNR-WAEHRUNG      PIC S9(004) BINARY.
      *
      *
           05 DRPA0972-C-NOTIZ.
              10 DRPA0972-C-MAX-USED-PVPT           PIC S9(004) BINARY.
              10 DRPA0972-C-MAX-USED-PVPT-RED REDEFINES
                 DRPA0972-C-MAX-USED-PVPT           PIC  X(002).
              10 DRPA0972-C-PVPT-TABELLE            OCCURS 100
                                             INDEXED BY DRPA0972-C-IX1.
                 15 DRPA0972-C-PROD-PROD-SCHL       PIC S9(004) BINARY.
                 15 DRPA0972-C-PVPT-VT-PRIO         PIC S9(004) BINARY.
170220           15 DRPA0972-C-VVGES                PIC S9(004) BINARY.
                 15 DRPA0972-C-VVNR                 PIC  X(015).
                 15 DRPA0972-C-PVPT-PPFL-BEGINN     PIC  X(010).
                 15 DRPA0972-C-PVPT-PPFL-ENDE       PIC  X(010).
                 15 DRPA0972-C-PVPT-BEWERT-BETRAG   PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-C-KNR-WAEHRUNG         PIC S9(004) BINARY.
                 15 DRPA0972-C-BEREINIGUNGS-KZ      PIC  X(001).
                 15 DRPA0972-C-KNR-EVS-BER-PVPT     PIC S9(004) BINARY.
      *
      *
           05 DRPA0972-D-NOTIZ.
              10 DRPA0972-D-MAX-USED-PVPT           PIC S9(004) BINARY.
              10 DRPA0972-D-MAX-USED-PVPT-RED REDEFINES
                 DRPA0972-D-MAX-USED-PVPT           PIC  X(002).
              10 DRPA0972-D-PVPT-TABELLE            OCCURS 40
                                             INDEXED BY DRPA0972-D-IX1.
                 15 DRPA0972-D-PROD-PROD-SCHL       PIC S9(004) BINARY.
                 15 DRPA0972-D-PVPT-VT-PRIO         PIC S9(004) BINARY.
                 15 DRPA0972-D-PVPT-PPFL-BEGINN     PIC  X(010).
                 15 DRPA0972-D-PVPT-PPFL-ENDE       PIC  X(010).
                 15 DRPA0972-D-PVPT-BEWERT-BETRAG   PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-D-KNR-WAEHRUNG         PIC S9(004) BINARY.
      *
      *
           05 DRPA0972-E-NOTIZ.
              10 DRPA0972-E-MAX-USED-VVNR           PIC S9(004) BINARY.
              10 DRPA0972-E-MAX-USED-VVNR-RED REDEFINES
                 DRPA0972-E-MAX-USED-VVNR           PIC  X(002).
              10 DRPA0972-E-VVNR-TABELLE            OCCURS 10
                                             INDEXED BY DRPA0972-E-IX1.
                 15 DRPA0972-E-VVNR-EINMAL.
                    20 DRPA0972-E-VVNR              PIC  X(015).
                    20 DRPA0972-E-RUECKBELASTUNG-KZ PIC  X(001).
                 15 DRPA0972-E-MAX-USED-PROD        PIC S9(004) BINARY.
                 15 DRPA0972-E-MAX-USED-PROD-RED REDEFINES
                    DRPA0972-E-MAX-USED-PROD        PIC  X(002).
                 15 DRPA0972-E-PROD-TABELLE         OCCURS 15
                                             INDEXED BY DRPA0972-E-IX2.
                    20 DRPA0972-E-PROD-PROD-SCHL    PIC S9(004) BINARY.
                    20 DRPA0972-E-RUECKBEL-BEGINN   PIC  X(010).
                    20 DRPA0972-E-RUECKBEL-ENDE     PIC  X(010).
                    20 DRPA0972-E-GSMT-PROV         PIC S9(009)V99
                                                        PACKED-DECIMAL.
                    20 DRPA0972-E-KNR-WAEHRUNG      PIC S9(004) BINARY.
      *
      *
           05 DRPA0972-F-NOTIZ.
              10 DRPA0972-F-MAX-USED-PROD           PIC S9(004) BINARY.
              10 DRPA0972-F-MAX-USED-PROD-RED REDEFINES
                 DRPA0972-F-MAX-USED-PROD           PIC  X(002).
              10 DRPA0972-F-PROD-TABELLE            OCCURS 15
                                             INDEXED BY DRPA0972-F-IX1.
                 15 DRPA0972-F-PROD-PROD-SCHL       PIC S9(004) BINARY.
                 15 DRPA0972-F-JAHRES-NT-PRAEMIE    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-F-KNR-ZAHLWEISE        PIC S9(004) BINARY.
                 15 DRPA0972-F-PRKO-GSP-STZ         PIC S9(003)V999
                                                        PACKED-DECIMAL.
                 15 DRPA0972-F-GSMT-PROV-1          PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-F-GSMT-PROV-2          PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-F-PG-GSMT-PROV         PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-F-GSMT-PROV-RUECK      PIC S9(009)V99
                                                        PACKED-DECIMAL.
                 15 DRPA0972-F-ABWEICHUNG           PIC S9(009)V99
                                                        PACKED-DECIMAL.
           05 DRPA0972-FILLER                       PIC  X(426).

