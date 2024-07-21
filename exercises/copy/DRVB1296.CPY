      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1296.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1296.CPv  $
      *
      *   Rev 2.1   Jan 07 2021 11:49:38   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.0   Nov 20 2020 10:46:56   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1296                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0009                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.11.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-532 !20.11.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1296 - ERMITTELN SATZ AUS TABELLE MS (D-VIEW) *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1296.
          05 PI-DRVB1296-DATEN.
             10 PI-DRVB1296-MS-NR              PIC S9(04)       COMP.
             10 PI-DRVB1296-KNR-GESELL-TYP     PIC S9(04)       COMP.
             10 PI-DRVB1296-KNR-BEFBED-TYP     PIC S9(04)       COMP.
 
       01 PO-DRVB1296.
         03 PO-DRVB1296-RC                     PIC  X(03).
             88 PO-DRVB1296-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1296-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1296-DATEN.
          05 PO-DRVB1296-NI-VIVBD009.
             10 NI-DRVB1296-ELE                PIC S9(04)       COMP
                                               OCCURS 37 TIMES.
          05 PO-DRVB1296-DCL-VIVBD009.
             10 PO-DRVB1296-TUPEL-VIEW-D       PIC  X(167).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1296   --------------------------*
      *---------------------------------------------------------------*

