      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.01.2021
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1306.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1306.CPv  $
      *
      *   Rev 2.1   Feb 23 2021 08:27:42   t13ae56
      *AVB01644: PGM - Anpassung DAP für KS2021
      *
      *   Rev 2.0   26 Jan 2021 13:50:30   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1306                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0121                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    26.01.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-532 !26.01.21!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1306 - PRÜFEN LETZTE BEFÖRDERUNG              *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1306.
          05 PI-DRVB1306-DATEN.
             10 PI-DRVB1306-MA-ST-NR           PIC S9(09)       BINARY.
 
       01 PO-DRVB1306.
         03 PO-DRVB1306-RC                     PIC  X(03).
             88 PO-DRVB1306-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1306-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1306-DATEN.
          05 PO-DRVB1306-MS-NR                 PIC S9(04)       BINARY.
          05 PO-DRVB1306-BEFINFO-DAT-VON       PIC  X(10).
          05 PO-DRVB1306-KNR-BEFBED-TYP        PIC S9(04)       BINARY.
          05 PO-DRVB1306-E-PR-ALT              PIC  X(01).
          05 PO-DRVB1306-S-PR-ALT              PIC  X(01).
          05 PO-DRVB1306-E-PR-07               PIC  X(01).
          05 PO-DRVB1306-S-PR-07               PIC  X(01).
          05 PO-DRVB1306-E-PR-11               PIC  X(01).
          05 PO-DRVB1306-S-PR-11               PIC  X(01).
          05 PO-DRVB1306-E-PR-15               PIC  X(01).
          05 PO-DRVB1306-S-PR-15               PIC  X(01).
          05 PO-DRVB1306-E-PR-21               PIC  X(01).
          05 PO-DRVB1306-S-PR-21               PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1306   --------------------------*
      *---------------------------------------------------------------*

