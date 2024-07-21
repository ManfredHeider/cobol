      *****************************************************************
      *    COPY-MEMBER: DRVB1324                                      * 
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
      * VLA-3566|20.10.21| NEUE FUNKTION DRVB1324         | T13AE30  *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1324 - ERM. EH ZUSTRUKTURIERTE DIREKT UNTERST *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1324.
          05 PI-DRVB1324-UFKT-CODE             PIC  9(02).
          05 PI-DRVB1324-DATEN.
             10 PI-DRVB1324-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1324-EHBASISB-JAHR-VON  PIC  X(04).
             10 PI-DRVB1324-EHBASISB-MONAT-VON PIC  X(02).
             10 PI-DRVB1324-EHBASISB-JAHR-BIS  PIC  X(04).
             10 PI-DRVB1324-EHBASISB-MONAT-BIS PIC  X(02).
             10 PI-DRVB1324-UNTST-NR           PIC S9(09)       BINARY. 

       01 PO-DRVB1324.
         03 PO-DRVB1324-RC                     PIC  X(03).
             88 PO-DRVB1324-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1324-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRVB1324-RC-EOF                  VALUE 'EOF'.
         03 PO-DRVB1324-DATEN.
          05 PO-DRVB1324-NI-ZXVB0137.
             10 NI-DRVB1324-MA-ST-NR           PIC S9(04)       BINARY. 
             10 NI-DRVB1324-AVERM              PIC S9(04)       BINARY. 
             10 NI-DRVB1324-BWKS-DAT-VON       PIC S9(04)       BINARY. 
             10 NI-DRVB1324-EHBASISB-VON       PIC S9(04)       BINARY. 
             10 NI-DRVB1324-EHBASISB-BIS       PIC S9(04)       BINARY. 
             10 NI-DRVB1324-EHBASISB-EH        PIC S9(04)       BINARY. 
             10 NI-DRVB1324-NGEW-VON           PIC S9(04)       BINARY. 
             10 NI-DRVB1324-NGEW-BIS           PIC S9(04)       BINARY. 
             10 NI-DRVB1324-GEW-VON            PIC S9(04)       BINARY. 
             10 NI-DRVB1324-GEW-BIS            PIC S9(04)       BINARY. 
             10 NI-DRVB1324-BWKS-DAT-ERSTELL   PIC S9(04)       BINARY. 
             10 NI-DRVB1324-BWKS-EBENE         PIC S9(04)       BINARY. 
             10 NI-DRVB1324-BWKS-ABZUGS-ART    PIC S9(04)       BINARY. 
             10 NI-DRVB1324-BWKS-ABZ-MONAT     PIC S9(04)       BINARY. 
          05 PO-DRVB1324-ZXVB0137.
             10 PO-DRVB1324-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PO-DRVB1324-AVERM              PIC S9(09)       BINARY. 
             10 PO-DRVB1324-BWKS-DAT-VON       PIC  X(10).
             10 PO-DRVB1324-EHBASISB-VON       PIC  X(10).
             10 PO-DRVB1324-EHBASISB-BIS       PIC  X(10).
             10 PO-DRVB1324-EHBASISB-EH        PIC S9(09)V9(01) COMP-3. 
             10 PO-DRVB1324-NGEW-VON           PIC  X(10).
             10 PO-DRVB1324-NGEW-BIS           PIC  X(10).
             10 PO-DRVB1324-GEW-VON            PIC  X(10).
             10 PO-DRVB1324-GEW-BIS            PIC  X(10).
             10 PO-DRVB1324-BWKS-DAT-ERSTELL   PIC  X(10).
             10 PO-DRVB1324-BWKS-EBENE         PIC  X(01).
             10 PO-DRVB1324-BWKS-ABZUGS-ART    PIC  X(01).
             10 PO-DRVB1324-BWKS-ABZ-MONAT     PIC  9(03).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1324   --------------------------* 
      *---------------------------------------------------------------*
