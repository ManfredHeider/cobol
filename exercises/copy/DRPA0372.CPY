      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0372.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0372.CPv  $
      *
      *   Rev 2.4   Sep 27 2016 09:31:46   t13ae46
      *APA02280: Dialoganpassungen PG14
      *
      *   Rev 2.3   Jan 07 2011 11:29:34   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.2   Jun 10 2005 09:48:24   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.1   Aug 31 1999 14:05:58   T13EX21
      *APA00053 - Neue Funktion DRPA1298
      *
      *
      *   Rev 2.0   Feb 27 1999 14:28:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:23:40   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0372                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJPA0010                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.07.97!    ! NEU                       ! D.WILHELM*
      * APA00053!24.08.99!    ! NEUE FUNKTION DRPA1298    ! STORCH   *
      * APA00353!02.06.05! 2.2! NEUE FUNKTIONEN           ! SCHEIDEL *
      * APA01129!07.01.11! 2.3! NEUE FUNKTIONEN PRODFILTER! RAHNER   *
      * APA02280!04.07.16! 2.4! NEUE FUNKTIONEN           ! WANG     *
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
       01  PI-ZJPA0010.
           05  PI-ZJPA0010-FUNKTION            PIC  X(008).
               88 PI-ZJPA0010-DRPA0373              VALUE 'DRPA0373'.
               88 PI-ZJPA0010-DRPA0441              VALUE 'DRPA0441'.
               88 PI-ZJPA0010-DRPA0442              VALUE 'DRPA0442'.
               88 PI-ZJPA0010-DRPA0527              VALUE 'DRPA0527'.
240899         88 PI-ZJPA0010-DRPA1298              VALUE 'DRPA1298'.
020605         88 PI-ZJPA0010-DRPA1646              VALUE 'DRPA1646'.
020605         88 PI-ZJPA0010-DRPA1647              VALUE 'DRPA1647'.
020605         88 PI-ZJPA0010-DRPA1648              VALUE 'DRPA1648'.
020605         88 PI-ZJPA0010-DRPA1649              VALUE 'DRPA1649'.
020605         88 PI-ZJPA0010-DRPA1650              VALUE 'DRPA1650'.
020605         88 PI-ZJPA0010-DRPA1651              VALUE 'DRPA1651'.
070111         88 PI-ZJPA0010-DRPA1662              VALUE 'DRPA1662'.
070111         88 PI-ZJPA0010-DRPA1719              VALUE 'DRPA1719'.
070111         88 PI-ZJPA0010-DRPA1722              VALUE 'DRPA1722'.
040716         88 PI-ZJPA0010-DRPA1847              VALUE 'DRPA1847'.
040716         88 PI-ZJPA0010-DRPA1848              VALUE 'DRPA1848'.
040716         88 PI-ZJPA0010-DRPA1849              VALUE 'DRPA1849'.
040716         88 PI-ZJPA0010-DRPA1852              VALUE 'DRPA1852'.
040716         88 PI-ZJPA0010-DRPA1853              VALUE 'DRPA1853'.
040716         88 PI-ZJPA0010-DRPA1855              VALUE 'DRPA1855'.
040716         88 PI-ZJPA0010-DRPA1856              VALUE 'DRPA1856'.
040716         88 PI-ZJPA0010-DRPA1857              VALUE 'DRPA1857'.
040716         88 PI-ZJPA0010-DRPA1858              VALUE 'DRPA1858'.
      *
      ******* ENDE COPY-MEMBER **************************************

