      *****************************************************************
      *    COPY-MEMBER: DRVB1021                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0313                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    11.07.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00964!11.07.11!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1021 - ERMITTELN SATZ AUS TABELLE DETAILDW    *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1021.
          05 PI-DRVB1021-DATEN.
             10 PI-DRVB1021-DETAILDW-BETR      PIC S9(09)       BINARY.
             10 PI-DRVB1021-DETAILDW-UNTST     PIC S9(09)       BINARY.
             10 PI-DRVB1021-DETAILDW-JAHR      PIC S9(04)       BINARY.
             10 PI-DRVB1021-DETAILDW-MONAT     PIC  X(10).
             10 PI-DRVB1021-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 PI-DRVB1021-KNR-WB-TYP         PIC S9(04)       BINARY.

       01 PO-DRVB1021.
         03 PO-DRVB1021-RC                     PIC  X(03).
             88 PO-DRVB1021-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1021-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1021-DATEN.
          05 PO-DRVB1021-NI-VIVB0313.
             10 NI-DRVB1021-DETAILDW-BETR      PIC S9(04)       BINARY.
             10 NI-DRVB1021-DETAILDW-UNTST     PIC S9(04)       BINARY.
             10 NI-DRVB1021-DETAILDW-JAHR      PIC S9(04)       BINARY.
             10 NI-DRVB1021-DETAILDW-MONAT     PIC S9(04)       BINARY.
             10 NI-DRVB1021-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1021-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB1021-DETAILDW-LAST-UP   PIC S9(04)       BINARY.
             10 NI-DRVB1021-DETAILDW-EH        PIC S9(04)       BINARY.
             10 NI-DRVB1021-DETAILDW-BIS       PIC S9(04)       BINARY.
          05 PO-DRVB1021-DCL-VIVB0313.
             10 PO-DRVB1021-DETAILDW-BETR      PIC S9(09)       BINARY.
             10 PO-DRVB1021-DETAILDW-UNTST     PIC S9(09)       BINARY.
             10 PO-DRVB1021-DETAILDW-JAHR      PIC S9(04)       BINARY.
             10 PO-DRVB1021-DETAILDW-MONAT     PIC  X(10).
             10 PO-DRVB1021-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1021-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB1021-DETAILDW-LAST-UP   PIC  X(26).
             10 PO-DRVB1021-DETAILDW-EH        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1021-DETAILDW-BIS       PIC  X(10).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1021   --------------------------*
      *---------------------------------------------------------------*
