      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1218.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1218.CPv  $
      *
      *   Rev 2.2   Dec 04 2020 13:13:28   t13ae56
      *ACV00226: PGM - Beförderungsschreiben austauschen
      *
      *   Rev 2.1   Jun 28 2019 12:32:36   t13ae56
      *AVB01570: PGM - Gutscheincode
      *
      *   Rev 2.0   Jun 07 2019 15:10:12   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1218                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0132                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-998 !07.06.19!2.1 ! NEU                       ! KLEEMEIER *
011220* VLA-935 !01.12.20!2.2 ! DRVB1303 HINZU            ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO UPRO UEBERGEBEN WERDEN --> FUNKTIONEN-COPY             *
      *                                                               *
      *****************************************************************
 
       01  PI-ZWVB0132.
           05  PI-ZWVB0132-FUNKTION            PIC  X(008).
               88 PI-ZWVB0132-DRVB1219              VALUE 'DRVB1219'.
011220         88 PI-ZWVB0132-DRVB1303              VALUE 'DRVB1303'.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1218   --------------------------*
      *---------------------------------------------------------------*

