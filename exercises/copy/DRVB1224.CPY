      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1224.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1224.CPv  $
      *
      *   Rev 2.2   Jan 14 2020 15:28:12   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.1   Aug 27 2019 11:55:24   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Jul 30 2019 08:02:36   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1224                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0234                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    31.07.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !31.07.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO UPRO UEBERGEBEN WERDEN --> FUNKTIONEN-COPY             *
      *                                                               *
      *****************************************************************
 
       01  PI-ZWVB0234.
           05  PI-ZWVB0234-FUNKTION            PIC  X(008).
               88 PI-ZWVB0234-DRVB1225              VALUE 'DRVB1225'.
               88 PI-ZWVB0234-DRVB1228              VALUE 'DRVB1228'.
               88 PI-ZWVB0234-DRVB1229              VALUE 'DRVB1229'.
               88 PI-ZWVB0234-DRVB1230              VALUE 'DRVB1230'.
               88 PI-ZWVB0234-DRVB1233              VALUE 'DRVB1233'.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1224   --------------------------*
      *---------------------------------------------------------------*


