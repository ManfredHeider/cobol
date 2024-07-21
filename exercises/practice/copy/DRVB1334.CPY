      *****************************************************************
      *    COPY-MEMBER: DRVB1334                                      * 
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
      * VLA-3566|20.10.21| NEUE FUNKTION DRVB1334         | T13AE30  *  
      *         |        |                                |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1334 - ERMITTELN VB, DIE EINEM VB AKTUELL     * 
      *                        UNTERSTELLT SIND BZW. IN DER           *
      *                        VERGANGENHEIT WAREN FÜR ZU-/ABSCHLAG   *
      *                        BERECHNUNG IN BEFÖRDERUNGSVORAUSSCHAU  *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1334.
          05 PI-DRVB1334-UFKT-CODE             PIC  9(02).
          05 PI-DRVB1334-DATEN.
             10 PI-DRVB1334-MA-ST-NR           PIC S9(09)       BINARY. 
 
       01 PO-DRVB1334.
         03 PO-DRVB1334-RC                     PIC  X(03).
             88 PO-DRVB1334-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1334-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRVB1334-RC-EOF                  VALUE 'EOF'.
         03 PO-DRVB1334-DATEN.
          05 PO-DRVB1334-NI-ZXVB0137.
             10 NI-DRVB1334-MA-ST-NR           PIC S9(04)       BINARY. 
             10 NI-DRVB1334-UNTST-NR           PIC S9(04)       BINARY. 
          05 PO-DRVB1334-ZXVB0137.
             10 PO-DRVB1334-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PO-DRVB1334-UNTST-NR           PIC S9(09)       BINARY. 

      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1334   --------------------------* 
      *---------------------------------------------------------------*
