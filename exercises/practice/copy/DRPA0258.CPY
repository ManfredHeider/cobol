      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0258.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA0258.CPv  $
      *
      *   Rev 2.9   Dec 10 2018 13:40:56   t13ae37
      *APA02749: Einführung GC&C - Ersatz für Ventilproduktschlüsse
      *l 0091/0093 - Sourcen ABU
      *
      *   Rev 2.8   Apr 04 2018 15:35:02   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.7   Jan 03 2017 08:48:02   t13ae37
      *APA02347: BKK-Linde als eigenständige Gesellschaft
      *
      *   Rev 2.6   Jan 05 2012 14:24:40   t13ex25
      *APA01284: Ges. 95- maschinelle DTA- Übernahme
      *
      *   Rev 2.5   Dec 10 2007 08:35:00   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.4   Aug 21 2002 09:20:16   t13ae46
      *APA00206: PEG Generali Sach und Wiener Allianz Sach
      *
      *   Rev 2.3   20 Aug 2001 09:29:44   t13ex21
      *APA00133 - Neue Funktion
      *
      *   Rev 2.2   04 Jul 2001 11:32:18   t13ex43
      *APA00133 : PEG Advocard
      *
      *   Rev 2.1   13 Mar 2001 16:28:50   T13EX25
      *APA00131; PEG Central
      *
      *   Rev 2.0   Feb 27 1999 14:28:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:31:34   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:24:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0258                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UWPA0015                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00131!01.03.01! 02 ! NEUE FUNKTION DRPA1475    ! JÄGER    *
      * APA00133!03.07.01!2.02! NEUE FUNKTION DRPA1500    ! KLEEMEIER*
      * APA00133!07.08.01!2.03! NEUE FUNKTION DRPA1504    ! STORCH   *
      * APA00206!18.07.02!2.04! NEUE FUNKTION DRPA1550    ! WANG     *
      *         !        !    !               DRPA1544    !          *
      * APA00622!17.07.07!2.05! NEUE FUNKTIONEN DRPA1666  ! STORCH   *
      *         !        !    ! UND DRPA1672              !          *
      * APA01284!29.11.11!2.06! Neue Funktion DRPA1727    ! Jäger    *
      * APA02347!29.11.16!2.07! Neue Funktion DRPA1863    ! Buch     *
      * APA02579!16.03.18!2.08! NEUE FUNKTION DRPA1884    ! WANG     *
      * APA02749!26.11.18!2.09! NEUE FUNKTION DRPA1893    ! BUCH     *
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
       01  PI-UWPA0015.
           05  PI-UWPA0015-FUNKTION            PIC  X(008).
               88 PI-UWPA0015-DRPA0259              VALUE 'DRPA0259'.
               88 PI-UWPA0015-DRPA1475              VALUE 'DRPA1475'.
               88 PI-UWPA0015-DRPA1500              VALUE 'DRPA1500'.
               88 PI-UWPA0015-DRPA1504              VALUE 'DRPA1504'.
               88 PI-UWPA0015-DRPA1550              VALUE 'DRPA1550'.
               88 PI-UWPA0015-DRPA1544              VALUE 'DRPA1544'.
               88 PI-UWPA0015-DRPA1666              VALUE 'DRPA1666'.
               88 PI-UWPA0015-DRPA1672              VALUE 'DRPA1672'.
291111         88 PI-UWPA0015-DRPA1727              VALUE 'DRPA1727'.
291116         88 PI-UWPA0015-DRPA1863              VALUE 'DRPA1863'.
160318         88 PI-UWPA0015-DRPA1884              VALUE 'DRPA1884'.
261118         88 PI-UWPA0015-DRPA1893              VALUE 'DRPA1893'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

