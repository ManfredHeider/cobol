      *****************************************************************
      *    COPY-MEMBER: DRVB1223                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0326                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    23.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580|23.09.19|2.1 | NEU                       | TTH       *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1223 - ERMITTELN SATZ AUS TABELLE SWBAS       *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1223.
          05 PI-DRVB1223-DATEN.
             10 PI-DRVB1223-SWBAS-BETR-NR      PIC S9(09)       BINARY.
             10 PI-DRVB1223-SWBAS-UNTST-NR     PIC S9(09)       BINARY.
             10 PI-DRVB1223-SWBAS-DAT-WECHSEL  PIC  X(10).
             10 PI-DRVB1223-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 PI-DRVB1223-KNR-KORR-TYP       PIC S9(04)       BINARY.

       01 PO-DRVB1223.
         03 PO-DRVB1223-RC                     PIC  X(03).
             88 PO-DRVB1223-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1223-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1223-DATEN.
          05 PO-DRVB1223-NI-VIVB0326.
             10 NI-DRVB1223-SWBAS-BETR-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-UNTST-NR     PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-DAT-WECHSEL  PIC S9(04)       BINARY.
             10 NI-DRVB1223-KTY-UST-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1223-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-KORR-AB      PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-KORR-BIS     PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-KORR-ENDE    PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-BASIS-AB     PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-BASIS-BIS    PIC S9(04)       BINARY.
             10 NI-DRVB1223-KTY-ZUSTRUK-TYP    PIC S9(04)       BINARY.
             10 NI-DRVB1223-KNR-ZUSTRUK-TYP    PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-DIR-NEU      PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-EBENE        PIC S9(04)       BINARY.
             10 NI-DRVB1223-KTY-KORR-TYP       PIC S9(04)       BINARY.
             10 NI-DRVB1223-KNR-KORR-TYP       PIC S9(04)       BINARY.
             10 NI-DRVB1223-KNR-KORR-DAT       PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-EH-BASIS-12  PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-ABR-MONATE   PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-EH-MONWERT   PIC S9(04)       BINARY.
             10 NI-DRVB1223-SWBAS-LAST-UPDATE  PIC S9(04)       BINARY.
          05 PO-DRVB1223-DCL-VIVB0326.
             10 PO-DRVB1223-SWBAS-BETR-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1223-SWBAS-UNTST-NR     PIC S9(09)       BINARY.
             10 PO-DRVB1223-SWBAS-DAT-WECHSEL  PIC  X(10).
             10 PO-DRVB1223-KTY-UST-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1223-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1223-SWBAS-KORR-AB      PIC  X(10).
             10 PO-DRVB1223-SWBAS-KORR-BIS     PIC  X(10).
             10 PO-DRVB1223-SWBAS-KORR-ENDE    PIC  X(10).
             10 PO-DRVB1223-SWBAS-BASIS-AB     PIC  X(10).
             10 PO-DRVB1223-SWBAS-BASIS-BIS    PIC  X(10).
             10 PO-DRVB1223-KTY-ZUSTRUK-TYP    PIC S9(04)       BINARY.
             10 PO-DRVB1223-KNR-ZUSTRUK-TYP    PIC S9(04)       BINARY.
             10 PO-DRVB1223-SWBAS-DIR-NEU      PIC S9(04)       BINARY.
             10 PO-DRVB1223-SWBAS-EBENE        PIC S9(04)       BINARY.
             10 PO-DRVB1223-KTY-KORR-TYP       PIC S9(04)       BINARY.
             10 PO-DRVB1223-KNR-KORR-TYP       PIC S9(04)       BINARY.
             10 PO-DRVB1223-KNR-KORR-DAT       PIC  X(10).
             10 PO-DRVB1223-SWBAS-EH-BASIS-12  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1223-SWBAS-ABR-MONATE   PIC S9(04)       BINARY.
             10 PO-DRVB1223-SWBAS-EH-MONWERT   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1223-SWBAS-LAST-UPDATE  PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1223   --------------------------*
      *---------------------------------------------------------------*







