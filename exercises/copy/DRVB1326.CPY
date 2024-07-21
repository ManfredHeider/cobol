      *****************************************************************
      *    COPY-MEMBER: DRVB1326                                      * 
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0137                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    20.10.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-3566|20.10.21| NEUE FUNKTION DRVB1326         | T13AE30  *  
      *         |        |                                |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1326 - ERMITTELN EH FÜR ZUSCHLAG              * 
      *                        FÜR EINEN ZEITRAUM AUS EHBASISB        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1326.
          05 PI-DRVB1326-UFKT-CODE             PIC  9(02).
          05 PI-DRVB1326-DATEN.
             10 PI-DRVB1326-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1326-UNTST-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1326-EHBASISB-JAHR-VON  PIC  X(04).
             10 PI-DRVB1326-EHBASISB-MONAT-VON PIC  X(02).
             10 PI-DRVB1326-EHBASISB-JAHR-BIS  PIC  X(04).
             10 PI-DRVB1326-EHBASISB-MONAT-BIS PIC  X(02).
 
       01 PO-DRVB1326.
         03 PO-DRVB1326-RC                     PIC  X(03).
             88 PO-DRVB1326-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1326-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRVB1326-RC-EOF                  VALUE 'EOF'.
         03 PO-DRVB1326-DATEN.
          05 PO-DRVB1326-NI-ZXVB0137.
             10 NI-DRVB1326-MA-ST-NR           PIC S9(04)       BINARY. 
             10 NI-DRVB1326-UEBERHOL-VB        PIC S9(04)       BINARY. 
             10 NI-DRVB1326-EHBASISB-VON       PIC S9(04)       BINARY. 
             10 NI-DRVB1326-EHBASISB-BIS       PIC S9(04)       BINARY. 
             10 NI-DRVB1326-EHBASISB-EH        PIC S9(04)       BINARY. 
             10 NI-DRVB1326-BWUEB-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1326-BWUEB-DAT-BIS      PIC S9(04)       BINARY.
             10 NI-DRVB1326-BWUEB-DAT-ERSTELL  PIC S9(04)       BINARY.
             10 NI-DRVB1326-BWUEB-ZUR-MONAT    PIC S9(04)       BINARY.
          05 PO-DRVB1326-ZXVB0137.
             10 PO-DRVB1326-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PO-DRVB1326-UEBERHOL-VB        PIC S9(09)       BINARY. 
             10 PO-DRVB1326-EHBASISB-VON       PIC  X(10).
             10 PO-DRVB1326-EHBASISB-BIS       PIC  X(10).
             10 PO-DRVB1326-EHBASISB-EH        PIC S9(09)V9(01) COMP-3. 
             10 PO-DRVB1326-BWUEB-DAT-VON      PIC  X(10).
             10 PO-DRVB1326-BWUEB-DAT-BIS      PIC  X(10). 
             10 PO-DRVB1326-BWUEB-DAT-ERSTELL  PIC  X(10).
             10 PO-DRVB1326-BWUEB-ZUR-MONAT    PIC  9(03).

      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1326   --------------------------* 
      *---------------------------------------------------------------*
