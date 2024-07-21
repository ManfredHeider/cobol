      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0256.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRAT0256.CPv  $
      *
      *   Rev 2.3   Feb 29 2000 08:39:26   T13EX13
      *APA00082: Währungsprüfung PEG
      *
      *   Rev 2.2   Jan 26 2000 16:58:48   T13EX13
      *APA00082 - Währungskennzeichen mit Datum
      *prüfen
      *
      *
      *   Rev 2.1   Apr 06 1999 11:33:18   T13AE34
      *AAT00005-Euro-Swicht
      *
      *   Rev 2.0   Mar 01 1999 14:48:52   T13AE34
      *NT-Umstellung
      *
      *   Rev 1.1   06 Jul 1998 09:26:38   T13AE34
      *AAT00002 - Euro
      *
      *   Rev 1.0   27 Feb 1998 10:16:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0256                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO UPAT0023              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. HEIDER                                        *
      *    DATUM:    12.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00002!12.06.98!    ! NEU                       ! HEIDER   *
      * AAT00005!17.02.98!1.2 ! DRAT0283 - EURO-SWITCH    ! HEIDER   *
250100* APA00082!25.01.00!1.3 ! DRAT0309 - WÄHRUNG MIT    ! WILKENS  *
250100*         !        !    ! DATUM PRÜFEN              !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-UPAT0023.
           05  PI-UPAT0023-FUNKTION            PIC  X(008).
               88 PI-UPAT0023-DRAT0257              VALUE 'DRAT0257'.
               88 PI-UPAT0023-DRAT0258              VALUE 'DRAT0258'.
               88 PI-UPAT0023-DRAT0259              VALUE 'DRAT0259'.
               88 PI-UPAT0023-DRAT0260              VALUE 'DRAT0260'.
               88 PI-UPAT0023-DRAT0283              VALUE 'DRAT0283'.
               88 PI-UPAT0023-DRAT0291              VALUE 'DRAT0291'.
250100         88 PI-UPAT0023-DRAT0309              VALUE 'DRAT0309'.
      *
      ******* ENDE COPY-MEMBER **************************************
