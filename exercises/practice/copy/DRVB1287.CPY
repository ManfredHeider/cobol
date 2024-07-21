      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1287.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1287.CPv  $
      *
      *   Rev 2.2   Dec 21 2020 17:34:14   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.1   Oct 06 2020 16:46:36   u130213
      *AVB01642: KS2021: Eh-Ermittlung Allf.1 und Allf.2
      *
      *   Rev 2.0   Sep 02 2020 07:55:00   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1287                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0137                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    31.08.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-1057!31.08.20!2.1 ! NEU                       ! HITZEMANN *
      * VLA-3566!02.12.21!NEUE FUNKTIONEN DRVB1327DRVB1333! T13AE30   *
      *         !        !                                !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO UPRO UEBERGEBEN WERDEN --> FUNKTIONEN-COPY             *
      *                                                               *
      *****************************************************************
      *
       01  PI-ZWVB0137.
           05  PI-ZWVB0137-FUNKTION            PIC  X(008).
               88 PI-ZWVB0137-DRVB1288              VALUE 'DRVB1288'.
               88 PI-ZWVB0137-DRVB1327              VALUE 'DRVB1327'.
               88 PI-ZWVB0137-DRVB1333              VALUE 'DRVB1333'.
      *
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1287   --------------------------*
      *---------------------------------------------------------------*

