      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  01.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1303.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1303.CPv  $
      *
      *   Rev 2.1   Dec 04 2020 13:13:50   t13ae56
      *ACV00226: PGM - Beförderungsschreiben austauschen
      *
      *   Rev 2.0   Dec 01 2020 09:18:36   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1303                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0132                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.12.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-935 !01.12.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1303 - PRÜFEN GUTSCHEINCODE FÜR VB-NUMMER     *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1303.
          05 PI-DRVB1303-DATEN.
             10 PI-DRVB1303-GUTSC-ST-NR        PIC S9(09)       BINARY.
             10 PI-DRVB1303-KNR-GUTSC-TYP      PIC S9(04)       BINARY.
 
       01 PO-DRVB1303.
         03 PO-DRVB1303-RC                     PIC  X(03).
             88 PO-DRVB1303-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1303-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1303-DATEN.
          05 PO-DRVB1303-GUTSC-VORHANDEN       PIC  X(01).
             88 PO-DRVB1303-GUTSC-VORH-JA           VALUE 'J'.
             88 PO-DRVB1303-GUTSC-VORH-NEIN         VALUE 'N'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1303   --------------------------*
      *---------------------------------------------------------------*

