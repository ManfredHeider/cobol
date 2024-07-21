      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1015.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1015.CPv  $
      *
      *   Rev 2.2   Feb 06 2012 16:52:04   t13ex43
      *AVB01195: PGM - Prüfung in VB45 'Orgabetreuer für Einzeldire
      *ktion'
      *
      *   Rev 2.1   May 09 2011 10:00:44   t13ex43
      *AVB01168: PGM - Pflege Orga-Betreuer (VB47)
      *
      *   Rev 2.0   Apr 28 2011 14:50:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1015                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0099                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01168!02.05.11!2.1 ! NEU                       ! KLEEMEIER *
060212* AVB01195!06.02.12!2.2 ! DRVB1045 HINZU            ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *                                                               *
      *****************************************************************

       01  PI-ZWVB0099.
           05  PI-ZWVB0099-FUNKTION            PIC  X(008).
               88 PI-ZWVB0099-DRVB1016              VALUE 'DRVB1016'.
060212         88 PI-ZWVB0099-DRVB1045              VALUE 'DRVB1045'.

      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1015   --------------------------*
      *---------------------------------------------------------------*
