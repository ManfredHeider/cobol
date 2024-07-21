      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1307.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:\MF\CPY\DRPA1307.CPv  $
      *
      *   Rev 2.7   17 May 2001 13:29:40   t13ex43
      *APA00132 : Check Batchbearbeitungsfunktion
      *
      *   Rev 2.6   24 Apr 2001 11:16:40   t13ex21
      *APA0096 - Neue Funktion DRPA1492
      *
      *   Rev 2.5   07 Sep 2000 14:48:40   T13EX23
      *APA00096 - Neue Funktion DRPA1402
      *
      *   Rev 2.4   22 May 2000 17:27:28   T13EX21
      *APA00090 - Neue Funktion
      *
      *   Rev 2.3   15 May 2000 08:49:28   T13EX21
      *APA00090 - Neue Funktion DRPA1381
      *
      *   Rev 2.2   Mar 16 2000 16:20:02   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.1   Feb 22 2000 08:42:46   T13AE18
      *APA00080
      *
      *   Rev 2.0   Nov 29 1999 13:55:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1307                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0027 KONFIGURATIONS-SB      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    03.12.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!03.12.99! 01 ! NEU                       ! BOHLINGER*
      * APA00067!14.03.00! 02 ! ANPASSUNG FÜR BADENIA     ! WILKENS  *
      * APA00090!11.05.00! 03 ! NEUE FUNKTION DRPA1381    ! STORCH   *
      * APA00090!18.05.00! 04 ! NEUE FUNKTION DRPA1386    ! STORCH   *
      * APA00096!19.07.00! 05 ! NEUE FUNKTION DRPA1402    ! GRAY     *
      *         !23.04.01!2.06! NEUE FUNKTION DRPA1492    ! STORCH   *
      * APA00132!15.05.01!2.07! NEUE FUNKTION DRPA1493    ! KLEEMEIER*
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
       01  PI-UPPA0027.
           05  PI-UPPA0027-FUNKTION            PIC  X(008).
               88 PI-UPPA0027-DRPA1308              VALUE 'DRPA1308'.
               88 PI-UPPA0027-DRPA1348              VALUE 'DRPA1348'.
110500         88 PI-UPPA0027-DRPA1381              VALUE 'DRPA1381'.
180500         88 PI-UPPA0027-DRPA1386              VALUE 'DRPA1386'.
190700         88 PI-UPPA0027-DRPA1402              VALUE 'DRPA1402'.
230401         88 PI-UPPA0027-DRPA1492              VALUE 'DRPA1492'.
150501         88 PI-UPPA0027-DRPA1493              VALUE 'DRPA1493'.
      *
      ******* ENDE COPY-MEMBER **************************************
