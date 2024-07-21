      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0422.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0422.CPv  $
      *
      *   Rev 2.0   May 25 1999 13:13:24   T13AE18
      *APA00012
      *
      *   Rev 1.1   Mar 25 1999 12:08:42   T13AE18
      *APA0012 neue Funktion DRPA1179
      *
      *   Rev 1.0   23 Jul 1997 12:34:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0422                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0051                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    23.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.10.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!02.03.99!1.2 ! NEUE FUNKTION DRPA1179    ! BOHLINGER*
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
       01  PI-UPPA0051.
           05  PI-UPPA0051-FUNKTION            PIC  X(008).
               88 PI-UPPA0051-DRPA0423              VALUE 'DRPA0423'.
020399         88 PI-UPPA0051-DRPA1179              VALUE 'DRPA1179'.
      *
      ******* ENDE COPY-MEMBER **************************************
