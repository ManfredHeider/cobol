      *****************************************************************
      *    COPY-MEMBER: DRVB1359                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0356                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THURMANN                                         *
      *    DATUM:    24.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-4870 |24.08.22| NEU                           | THURMANN  *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1359 - ERMITTELN SATZ AUS TABELLE WTAUS       *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1359.
          05 PI-DRVB1359-DATEN.
             10 PI-DRVB1359-MA-ST-NR          PIC S9(009) BINARY.
             10 PI-DRVB1359-WTAUS-ANSPR-VOM   PIC  X(010).
             10 PI-DRVB1359-KNR-ANSPR-GRUND   PIC S9(004) BINARY.

       01 PO-DRVB1359.
          05 PO-DRVB1359-RC                   PIC  X(003).
             88 PO-DRVB1359-RC-OK                  VALUE 'OK '.
             88 PO-DRVB1359-RC-NOT-FOUND           VALUE 'NFD'.
          05 PO-DRVB1359-DATEN.
             10 PO-DRVB1359-NI-VIVB0356.
                15 NI-DRVB1359-MA-ST-NR            PIC S9(004) BINARY.
                15 NI-DRVB1359-WTAUS-ANSPR-VOM     PIC S9(004) BINARY.
                15 NI-DRVB1359-KNR-ANSPR-GRUND     PIC S9(004) BINARY.
                15 NI-DRVB1359-KTY-ANSPR-GRUND     PIC S9(004) BINARY.
                15 NI-DRVB1359-WTAUS-LAST-UPDATE   PIC S9(004) BINARY.
                15 NI-DRVB1359-WTAUS-ANSPR-BETRAG  PIC S9(004) BINARY.
                15 NI-DRVB1359-KTY-ANSPR-STATUS    PIC S9(004) BINARY.
                15 NI-DRVB1359-KNR-ANSPR-STATUS    PIC S9(004) BINARY.
             10 PO-DRVB1359-DCL-VIVB0356.
                15 PO-DRVB1359-MA-ST-NR            PIC S9(009) BINARY.
                15 PO-DRVB1359-WTAUS-ANSPR-VOM     PIC  X(010).
                15 PO-DRVB1359-KNR-ANSPR-GRUND     PIC S9(004) BINARY.
                15 PO-DRVB1359-KTY-ANSPR-GRUND     PIC S9(004) BINARY.
                15 PO-DRVB1359-WTAUS-LAST-UPDATE   PIC  X(026).
                15 PO-DRVB1359-WTAUS-ANSPR-BETRAG
                                            PIC S9(009)V9(002) COMP-3.
                15 PO-DRVB1359-KTY-ANSPR-STATUS    PIC S9(004) BINARY.
                15 PO-DRVB1359-KNR-ANSPR-STATUS    PIC S9(004) BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1359   --------------------------*
      *---------------------------------------------------------------*