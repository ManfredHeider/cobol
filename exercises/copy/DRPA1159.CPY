      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1159.CPY  $
      *    $Revision:   1.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1159.CPv  $
      *
      *   Rev 1.3   Apr 21 2008 13:24:36   t13ex21
      *FPA01015: Advocard: Korrektur Buchungen
      *
      *   Rev 1.2   Feb 18 2002 14:25:28   t13ex43
      *FPA00352 : Korrekturbuchung für halbe Einheiten
      *
      *   Rev 1.1   Nov 26 2001 16:32:26   t13ex21
      *FPA00293 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:02:00   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1159                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJPA0018                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    24.10.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00293!24.10.01!    ! NEU                       ! STORCH   *
      * FPA00352!11.02.02!1.02! DRPA1523 HINZU            ! KLEEMEIER*
      * FPA01015!09.04.08!2.01! DRPA1676 HINZU            ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZJPA0018.
           05  PI-ZJPA0018-FUNKTION            PIC  X(008).
               88 PI-ZJPA0018-DRPA1158              VALUE 'DRPA1158'.
110202         88 PI-ZJPA0018-DRPA1523              VALUE 'DRPA1523'.
090408         88 PI-ZJPA0018-DRPA1676              VALUE 'DRPA1676'.
      *
      ******* ENDE COPY-MEMBER **************************************
