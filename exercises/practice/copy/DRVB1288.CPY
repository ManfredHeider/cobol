      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1288.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1288.CPv  $
      *
      *   Rev 2.2   Dec 21 2020 17:34:20   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.1   Oct 06 2020 16:46:42   u130213
      *AVB01642: KS2021: Eh-Ermittlung Allf.1 und Allf.2
      *
      *   Rev 2.0   Sep 02 2020 07:55:02   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1288                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0137                   *
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
      *    FUNKTION DRVB1288 - ERMITTELN SATZ AUS TABELLE EHBASISB    *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1288.
          05 PI-DRVB1288-DATEN.
             10 PI-DRVB1288-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1288-EHBASISB-MONAT     PIC  X(10).
             10 PI-DRVB1288-AVERM              PIC S9(09)       BINARY.
             10 PI-DRVB1288-PROD-ART           PIC S9(04)       BINARY.
             10 PI-DRVB1288-EIGEN-KZ           PIC  X(01).
             10 PI-DRVB1288-STRUK-BER-KZ       PIC  X(01).
 
       01 PO-DRVB1288.
         03 PO-DRVB1288-RC                     PIC  X(03).
             88 PO-DRVB1288-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1288-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1288-DATEN.
          05 PO-DRVB1288-NI-VIVB0137.
             10 NI-DRVB1288-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1288-EHBASISB-MONAT     PIC S9(04)       BINARY.
             10 NI-DRVB1288-AVERM              PIC S9(04)       BINARY.
             10 NI-DRVB1288-PROD-ART           PIC S9(04)       BINARY.
             10 NI-DRVB1288-EIGEN-KZ           PIC S9(04)       BINARY.
             10 NI-DRVB1288-STRUK-BER-KZ       PIC S9(04)       BINARY.
             10 NI-DRVB1288-EHBASISB-EH        PIC S9(04)       BINARY.
             10 NI-DRVB1288-LAST-UPDATE        PIC S9(04)       BINARY.
          05 PO-DRVB1288-DCL-VIVB0137.
             10 PO-DRVB1288-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1288-EHBASISB-MONAT     PIC  X(10).
             10 PO-DRVB1288-AVERM              PIC S9(09)       BINARY.
             10 PO-DRVB1288-PROD-ART           PIC S9(04)       BINARY.
             10 PO-DRVB1288-EIGEN-KZ           PIC  X(01).
             10 PO-DRVB1288-STRUK-BER-KZ       PIC  X(01).
             10 PO-DRVB1288-EHBASISB-EH        PIC S9(10)V9(01) COMP-3.
             10 PO-DRVB1288-LAST-UPDATE        PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1288   --------------------------*
      *---------------------------------------------------------------*


