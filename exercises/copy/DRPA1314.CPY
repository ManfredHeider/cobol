      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1314.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1314.CPv  $
      *
      *   Rev 2.2   23 May 2001 13:51:08   t13ex21
      *APA00096 - Neue Funktion
      *
      *   Rev 2.1   Jan 04 2000 13:11:48   T13AE18
      *APA00068
      *
      *
      *   Rev 2.0   Nov 29 1999 13:55:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1314                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0078 'TS-QUEUE - SB '       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                        *
      *    DATUM:    03.01.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!03.01.00! 01 ! NEU                       ! BOHLINGER*
      * APA00096!19.10.00!2.02! NEUE FUNKTION             ! STORCH   *
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
       01  PI-UPPA0078.
           05  PI-UPPA0078-FUNKTION            PIC  X(008).
               88 PI-UPPA0078-DRPA1315              VALUE 'DRPA1315'.
191000         88 PI-UPPA0078-DRPA1426              VALUE 'DRPA1426'.
      *
      ******* ENDE COPY-MEMBER **************************************
