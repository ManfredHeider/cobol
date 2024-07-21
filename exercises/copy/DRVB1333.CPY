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
      * AUFTRAG | DATUM  |AENDERUNG                       | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-3566|02.12.21|NEU                             | T13AE30   *
      *         |        |                                |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1333 - ERMITTELT SUMMIERTE FOLGE-EH EINES     *
      *                        ÜBERHOLDENDEN VB DIE SOWOHL BEI IHM,   *
      *                        ALS AUCH SEINEM ALTEN BETREUER         * 
      *                        VORHANDEN SIND.                        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1333.
          05 PI-DRVB1333-DATEN.
             10 PI-DRVB1333-BETR-ALT-NR        PIC S9(09)       BINARY. 
             10 PI-DRVB1333-UNTST-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1333-EHBASISB-JAHR-VON  PIC  X(04).
             10 PI-DRVB1333-EHBASISB-MONAT-VON PIC  X(02).
             10 PI-DRVB1333-EHBASISB-JAHR-BIS  PIC  X(04).
             10 PI-DRVB1333-EHBASISB-MONAT-BIS PIC  X(02).
 
       01 PO-DRVB1333.
         03 PO-DRVB1333-RC                     PIC  X(03).
             88 PO-DRVB1333-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1333-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1333-DATEN.
          05 PO-DRVB1333-NI-VIVB0137.
             10 NI-DRVB1333-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1333-UNTST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1333-FOLGE-EH-UEBERH    PIC S9(04)       BINARY.
          05 PO-DRVB1333-DCL-VIVB0137.
             10 PO-DRVB1333-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1333-UNTST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1333-FOLGE-EH-UEBERH    PIC S9(10)V9(01) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1333   --------------------------*
      *---------------------------------------------------------------*
