      *****************************************************************
      *    COPY-MEMBER: DRVB1314                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0143                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THI / TTH                                        *
      *    DATUM:    22.06.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-1742 !22.06.21! NEU                           ! THI / TTH *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1314 - ERMITTELN SATZ AUS TABELLE BWKS        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1314.
          05 PI-DRVB1314-DATEN.
             10 PI-DRVB1314-BWKS-BETR-NR       PIC S9(09)       BINARY.
             10 PI-DRVB1314-BWKS-UNTST-NR      PIC S9(09)       BINARY.
             10 PI-DRVB1314-BWKS-DAT-VON       PIC  X(10).
             10 PI-DRVB1314-BWKS-ZUSTRUK-TYP   PIC S9(04)       BINARY.
             10 PI-DRVB1314-BWKS-STAT          PIC S9(04)       BINARY.

       01 PO-DRVB1314.
         03 PO-DRVB1314-RC                     PIC  X(03).
             88 PO-DRVB1314-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1314-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1314-DATEN.
          05 PO-DRVB1314-NI-VIVB0143.
             10 NI-DRVB1314-BWKS-BETR-NR       PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-UNTST-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-DAT-BIS       PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-DAT-ERSTELL   PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-DAT-ENDE      PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-HERKUNFT      PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-EBENE         PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-ABZUGS-ART    PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-DIR-NEU       PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-DIR-ALT       PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-BETR-ALT      PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-ZUSTRUK-TYP   PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-STAT          PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-STAT-DATUM    PIC S9(04)       BINARY.
             10 NI-DRVB1314-BWKS-LAST-UPDATE   PIC S9(04)       BINARY.
          05 PO-DRVB1314-DCL-VIVB0143.
             10 PO-DRVB1314-BWKS-BETR-NR       PIC S9(09)       BINARY.
             10 PO-DRVB1314-BWKS-UNTST-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1314-BWKS-DAT-VON       PIC  X(10).
             10 PO-DRVB1314-BWKS-DAT-BIS       PIC  X(10).
             10 PO-DRVB1314-BWKS-DAT-ERSTELL   PIC  X(10).
             10 PO-DRVB1314-BWKS-DAT-ENDE      PIC  X(10).
             10 PO-DRVB1314-BWKS-HERKUNFT      PIC  X(01).
             10 PO-DRVB1314-BWKS-EBENE         PIC S9(04)       BINARY.
             10 PO-DRVB1314-BWKS-ABZUGS-ART    PIC S9(04)       BINARY.
             10 PO-DRVB1314-BWKS-DIR-NEU       PIC S9(04)       BINARY.
             10 PO-DRVB1314-BWKS-DIR-ALT       PIC S9(04)       BINARY.
             10 PO-DRVB1314-BWKS-BETR-ALT      PIC S9(09)       BINARY.
             10 PO-DRVB1314-BWKS-ZUSTRUK-TYP   PIC S9(04)       BINARY.
             10 PO-DRVB1314-BWKS-STAT          PIC S9(04)       BINARY.
             10 PO-DRVB1314-BWKS-STAT-DATUM    PIC  X(10).
             10 PO-DRVB1314-BWKS-LAST-UPDATE   PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1314   --------------------------*
      *---------------------------------------------------------------*
