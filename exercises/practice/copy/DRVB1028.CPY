      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1028.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1028.CPv  $
      *
      *   Rev 2.2   Sep 01 2011 11:47:32   t13ae14
      *ASL00390: Videosonderbonus Anpassung PMRW0018
      *
      *   Rev 2.1   Aug 19 2011 14:11:28   t13ae14
      *ASL00390: Videosonderbonus FTP und Load in die Tabelle VIDEO
      *A, Anpassung PMRW0018
      *
      *   Rev 2.0   Aug 12 2011 11:16:54   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB1028                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0210                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    12.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00390!12.08.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *                                                               *
      *****************************************************************

       01  PI-ZWVB0210.
           05  PI-ZWVB0210-FUNKTION            PIC  X(008).
               88 PI-ZWVB0210-DRVB1027              VALUE 'DRVB1027'.

      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1028   --------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
