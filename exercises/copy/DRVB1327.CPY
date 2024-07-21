      *****************************************************************
      *    COPY-MEMBER: DRVB1327                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0137                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    02.12.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-3566|02.12.21|NEU                             | T13AE30   *
      *         |        |                                |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1327 - ERMITTELT SUMMIERTE EH EINES ABSCHLUSS-*
      *                        VERMITTLERS IN EINEM VORGEGEBENEN      *
      *                        ZEITRAUM (ANALOG FUNKTION DRVB1295     * 
      *                        IM ZXVB0137)                           *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1327.
          05 PI-DRVB1327-DATEN.
             10 PI-DRVB1327-BETR-NR            PIC S9(09)       BINARY. 
             10 PI-DRVB1327-UNTST-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1327-AVERM-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1327-EHBASISB-JAHR-VON  PIC  X(04).
             10 PI-DRVB1327-EHBASISB-MONAT-VON PIC  X(02).
             10 PI-DRVB1327-EHBASISB-JAHR-BIS  PIC  X(04).
             10 PI-DRVB1327-EHBASISB-MONAT-BIS PIC  X(02).
 
       01 PO-DRVB1327.
         03 PO-DRVB1327-RC                     PIC  X(03).
             88 PO-DRVB1327-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1327-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1327-DATEN.
          05 PO-DRVB1327-NI-VIVB0137.
             10 NI-DRVB1327-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1327-AVERM              PIC S9(04)       BINARY.
             10 NI-DRVB1327-EHBASISB-AVERM     PIC S9(04)       BINARY.
          05 PO-DRVB1327-DCL-VIVB0137.
             10 PO-DRVB1327-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1327-AVERM              PIC S9(09)       BINARY.
             10 PO-DRVB1327-EHBASISB-AVERM     PIC S9(10)V9(01) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1327   --------------------------*
      *---------------------------------------------------------------*
