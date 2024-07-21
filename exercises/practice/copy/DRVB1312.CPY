      *****************************************************************
      *    COPY-MEMBER: DRVB1312                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0144                   *
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
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1312 - ERMITTELN SATZ AUS TABELLE BWUEB       *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1312.
          05 PI-DRVB1312-DATEN.
             10 PI-DRVB1312-BWUEB-BETR-ALT     PIC S9(09)       BINARY.
             10 PI-DRVB1312-BWUEB-UEBH-NR      PIC S9(09)       BINARY.
             10 PI-DRVB1312-BWUEB-DAT-VON      PIC  X(10).
             10 PI-DRVB1312-BWUEB-STAT         PIC S9(04)       BINARY.

       01 PO-DRVB1312.
         03 PO-DRVB1312-RC                     PIC  X(03).
             88 PO-DRVB1312-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1312-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1312-DATEN.
          05 PO-DRVB1312-NI-VIVB0144.
             10 NI-DRVB1312-BWUEB-BETR-ALT     PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-UEBH-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-DAT-BIS      PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-DAT-ERSTELL  PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-DAT-ENDE     PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-HERKUNFT     PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-BETR-NEU     PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-STAT         PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-STAT-DATUM   PIC S9(04)       BINARY.
             10 NI-DRVB1312-BWUEB-LAST-UPDATE  PIC S9(04)       BINARY.
          05 PO-DRVB1312-DCL-VIVB0144.
             10 PO-DRVB1312-BWUEB-BETR-ALT     PIC S9(09)       BINARY.
             10 PO-DRVB1312-BWUEB-UEBH-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1312-BWUEB-DAT-VON      PIC  X(10).
             10 PO-DRVB1312-BWUEB-DAT-BIS      PIC  X(10).
             10 PO-DRVB1312-BWUEB-DAT-ERSTELL  PIC  X(10).
             10 PO-DRVB1312-BWUEB-DAT-ENDE     PIC  X(10).
             10 PO-DRVB1312-BWUEB-HERKUNFT     PIC  X(01).
             10 PO-DRVB1312-BWUEB-BETR-NEU     PIC S9(09)       BINARY.
             10 PO-DRVB1312-BWUEB-STAT         PIC S9(04)       BINARY.
             10 PO-DRVB1312-BWUEB-STAT-DATUM   PIC  X(10).
             10 PO-DRVB1312-BWUEB-LAST-UPDATE  PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1312   --------------------------*
      *---------------------------------------------------------------*
