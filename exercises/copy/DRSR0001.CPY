      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSR0001.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRSR0001.CPv  $
      *
      *   Rev 2.3   Oct 28 2004 13:56:00   t13ex22
      *ASR00001: PGM - CTV-Parameterbeschaffung Online
      *
      *   Rev 2.2   Oct 28 2004 12:10:10   t13ex22
      *ASR00001: PGM - CTV-Parameterbeschaffung Online
      *
      *   Rev 2.1   Oct 26 2004 08:07:34   t13ex43
      *ASR00001: PGM - CTV-Parameterbeschaffung
      *
      *   Rev 2.0   Oct 19 2004 12:56:08   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRSR0001                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPSR0003                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.10.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASR00001!19.10.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-UPSR0003.
           05  PI-UPSR0003-FUNKTION            PIC  X(008).
               88 PI-UPSR0003-DRSR0002              VALUE 'DRSR0002'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
