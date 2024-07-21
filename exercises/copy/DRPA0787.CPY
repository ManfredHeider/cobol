      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0787.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0787.CPv  $
      *
      *   Rev 2.3   Oct 19 2005 12:01:44   t13ex44
      *APA00388: Erweiterung SA815 (Satzlänge/3-stellige Prod-Schlü
      *ssel)
      *
      *   Rev 2.2   Nov 26 2001 16:35:16   t13ex21
      *FPA00293 - Neue Funktion
      *
      *   Rev 2.1   Nov 05 1999 12:02:08   T13EX21
      *FPA00107 - Neue Funktion DRPA0745
      *
      *   Rev 2.0   May 26 1999 13:15:52   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:47:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0787                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZFPA0002                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    10.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.01.98! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * APA00012!25.08.98! 02 ! NEUE FUNKTIONEN FÜR LIST, ! PARENT   *
      *         !        !    ! SELECT PDPT-PDPE          !          *
      *         !        !    !                           !          *
      * APA00012! 1.09.98! 2.1! NEUE FUNKTION FÜR LIST    ! PARENT   *
      *         !        !    ! NACH EINFÜGEDATUM SORTIERT!          *
      *         !        !    !                           !          *
      * APA00012!15.09.98! 2.2! NEUE FUNKTION COMMIT      ! PARENT   *
      *         !        !    !                           !          *
      * APA00012!08.02.99! 2.3! NEUE FUNKTION DRPA1164    ! LÜTTGES  *
      *         !        !    !                           !          *
      * FPA00107!05.11.99! 2.4! NEUE FUNKTION DRPA0745    ! STORCH   *
      *         !        !    !                           !          *
      * FPA00293!30.10.01! 2.5! NEUE FUNKTION DRPA1160    ! STORCH   *
      * APA00388!04.10.05! 2.3! NEUE FUNKTION DRPA1654    ! SCHEIDEL *
      *         !        !    !                           !          *
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
       01  PI-ZFPA0002.
           05  PI-ZFPA0002-FUNKTION            PIC  X(008).
               88 PI-ZFPA0002-DRPA0788              VALUE 'DRPA0788'.
               88 PI-ZFPA0002-DRPA0961              VALUE 'DRPA0961'.
250898         88 PI-ZFPA0002-DRPA1053              VALUE 'DRPA1053'.
250898         88 PI-ZFPA0002-DRPA1054              VALUE 'DRPA1054'.
010998         88 PI-ZFPA0002-DRPA1055              VALUE 'DRPA1055'.
150998         88 PI-ZFPA0002-DRAT0228-COM          VALUE 'DRAT0228'.
080299         88 PI-ZFPA0002-DRPA1164              VALUE 'DRPA1164'.
051199         88 PI-ZFPA0002-DRPA0745              VALUE 'DRPA0745'.
301001         88 PI-ZFPA0002-DRPA1160              VALUE 'DRPA1160'.
041005         88 PI-ZFPA0002-DRPA1654-SEL-F-I      VALUE 'DRPA1654'.
      *
      ******* ENDE COPY-MEMBER **************************************
