      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VERSIONMANAGER
      *            DATUM      :  31.08.2020
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2020 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRVB1286.CPY  $
      *    $REVISION:   2.0  $
      *
      *    $LOG:   L:/MF/DRVB1286.CPV  $
      *
      *   REV 2.0   AUG 31 2020 14:14:54   U130213
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1286                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0137                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    31.08.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-1057|31.08.20|2.1 | NEU                       | HITZEMANN *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1286 - ERMITTELN EH PRO AVERM AUS EHBASISB    *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1286.
          05 PI-DRVB1286-UFKT-CODE             PIC  9(02).
          05 PI-DRVB1286-DATEN.
             10 PI-DRVB1286-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1286-EHBASISB-MONAT-VON PIC  X(10).
             10 PI-DRVB1286-EHBASISB-MONAT-BIS PIC  X(10).

       01 PO-DRVB1286.
         03 PO-DRVB1286-RC                     PIC  X(03).
             88 PO-DRVB1286-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1286-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRVB1286-RC-EOF                  VALUE 'EOF'.
         03 PO-DRVB1286-DATEN.
          05 PO-DRVB1286-NI-VIVB0137.
             10 NI-DRVB1286-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1286-AVERM              PIC S9(04)       BINARY.
             10 NI-DRVB1286-EHBASISB-EH        PIC S9(04)       BINARY.
             10 NI-DRVB1286-EIGEN-KZ           PIC S9(04)       BINARY.
          05 PO-DRVB1286-DCL-VIVB0137.
             10 PO-DRVB1286-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1286-AVERM              PIC S9(09)       BINARY.
             10 PO-DRVB1286-EHBASISB-EH        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1286-EIGEN-KZ           PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1286   --------------------------*
      *---------------------------------------------------------------*




