      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1417.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1417.CPv  $
      *
      *   Rev 2.1   07 Sep 2000 14:56:58   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER: DRPA1417                                      *
      *   --------------------------------------------------------   *
      *   FUNKTIONEN DES BUFFER-OZP'S ZYPA0036                       *
      *   ( BISHER BENUTZT VON PGPA0051 IM DIALOG PA02               *
      *     - DAISY GESELLSCHAFTABHÄNGIGE DATEN ERMITTELN)           *
      *   -------------------------------------------------------    *
      *   ERSTELLT: GRAY                                             *
      *   DATUM:    08.08.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!08.08.00! NEU                            ! GRAY     *
      *         !        !                                !          *
      *         !        !                                !          *
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
       01  PI-ZYPA0036.
           05  PI-ZYPA0036-FUNKTION            PIC  X(008).
      *----ERGEBNISMENGE BEREITSTELLEN
               88 PI-ZYPA0036-DRPA1418              VALUE 'DRPA1418'.
      *
      ******* ENDE COPY-MEMBER **************************************
