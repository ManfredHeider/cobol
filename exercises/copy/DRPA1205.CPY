      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1205.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1205.CPv  $
      *
      *   Rev 2.2   Jul 13 1999 10:27:10   T13AE18
      *apa00012
      *
      *   Rev 2.1   Apr 13 1999 10:32:02   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B
      *
      *   Rev 2.0   Mar 30 1999 08:44:00   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1205                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0114                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA00012!02.07.99!2.2 ! NEUE Funktion DRPA1276    ! Bohlinger*
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
       01  PI-UPPA0114.
           05  PI-UPPA0114-FUNKTION            PIC  X(008).
               88 PI-UPPA0114-DRPA1206              VALUE 'DRPA1206'.
020799         88 PI-UPPA0114-DRPA1276              VALUE 'DRPA1276'.
      *
      ******* ENDE COPY-MEMBER **************************************
