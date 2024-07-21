F     *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.10.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1295.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1295.CPv  $
      *
      *   Rev 2.2   Dec 21 2020 17:34:54   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.1   Nov 03 2020 16:07:22   u130213
      *AVB01642: KS2021: Eh-Ermittlung Allf.1 und Allf.2
      *
      *   Rev 2.0   Oct 27 2020 11:15:26   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1295                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LIST-OZP ZXVB0137                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    27.10.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-1263|27.10.20|2.0 | NEU                       | HITZEMANN *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1295 - ERMITTELN EH FÜR GEMEINSAME AVERM      *
      *                        VON BETREUER UND DIREKTEN UNTERSTELLTEN*
      *                        FÜR EINEN ZEITRAUM AUS EHBASISB        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1295.
          05 PI-DRVB1295-UFKT-CODE             PIC  9(02).
          05 PI-DRVB1295-DATEN.
             10 PI-DRVB1295-BETR-NR            PIC S9(09)       BINARY.
             10 PI-DRVB1295-EHBASISB-JAHR-VON  PIC  X(04).
             10 PI-DRVB1295-EHBASISB-MONAT-VON PIC  X(02).
             10 PI-DRVB1295-EHBASISB-JAHR-BIS  PIC  X(04).
             10 PI-DRVB1295-EHBASISB-MONAT-BIS PIC  X(02).
             10 PI-DRVB1295-UNTST-NR           PIC S9(09)       BINARY.
 
       01 PO-DRVB1295.
         03 PO-DRVB1295-RC                     PIC  X(03).
             88 PO-DRVB1295-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1295-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRVB1295-RC-EOF                  VALUE 'EOF'.
         03 PO-DRVB1295-DATEN.
          05 PO-DRVB1295-NI-VIVB0137.
             10 NI-DRVB1295-BETR-NR            PIC S9(04)       BINARY.
             10 NI-DRVB1295-UNTST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1295-AVERM              PIC S9(04)       BINARY.
             10 NI-DRVB1295-EHBASISB-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1295-EHBASISB-BIS       PIC S9(04)       BINARY.
             10 NI-DRVB1295-EHBASISB-EH        PIC S9(04)       BINARY.
          05 PO-DRVB1295-DCL-VIVB0137.
             10 PO-DRVB1295-BETR-NR            PIC S9(09)       BINARY.
             10 PO-DRVB1295-UNTST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1295-AVERM              PIC S9(09)       BINARY.
             10 PO-DRVB1295-EHBASISB-VON       PIC  X(10).
             10 PO-DRVB1295-EHBASISB-BIS       PIC  X(10).
             10 PO-DRVB1295-EHBASISB-EH        PIC S9(09)V9(01) COMP-3.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1295   --------------------------*
      *---------------------------------------------------------------*

