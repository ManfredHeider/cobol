      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  23.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1299.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1299.CPv  $
      *
      *   Rev 2.1   Jan 07 2021 11:50:50   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.0   Nov 23 2020 16:34:28   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1299                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0044                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.11.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-532 !23.11.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1299 - PRÜFEN PFLICHTSEMINAR FÜR STUFE        *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1299.
          05 PI-DRVB1299-DATEN.
             10 PI-DRVB1299-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PI-DRVB1299-MS-NR              PIC S9(04)       BINARY.
             10 PI-DRVB1299-VBP-NR             PIC S9(04)       BINARY.
             10 PI-DRVB1299-KNR-SSTS-TYP       PIC S9(04)       BINARY.
 
       01 PO-DRVB1299.
         03 PO-DRVB1299-RC                     PIC  X(03).
             88 PO-DRVB1299-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1299-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1299-DATEN.
          05 PO-DRVB1299-SEMINAR-PFLICHT       PIC  X(01).
             88 PO-DRVB1299-SEMINAR-PFLICHT-JA      VALUE 'J'.
             88 PO-DRVB1299-SEMINAR-PFLICHT-NO      VALUE 'N'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1299   --------------------------*
      *---------------------------------------------------------------*

