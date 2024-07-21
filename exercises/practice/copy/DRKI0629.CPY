      * P13SP99   Stand: 06/12/98    18:02:28
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0629.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0629.CPv  $
      *
      *   Rev 2.2   Nov 24 2010 10:49:12   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich Ticket-
      *DTA Teil 2
      *
      *   Rev 2.1   Feb 27 2007 12:47:16   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Apr 16 1999 13:19:06   T13EX19
      *aki00028
      *
      *   Rev 1.0   25 Aug 1998 09:25:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0629  FUNKTIONSAUFRUFE FUER              *
      *                             JOZP ZJKI0013;                   *
      *                           VERTRAGSDATEN ERMITTELN            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    17.09.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.09.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * AKI00557!05.02.07!2.1 ! Neue Funktion             !I. Frenzel*
      * AKI00966!20.09.10!2.2 ! Neue Funktion             !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZJKI0013.
           05 PI-ZJKI0013-FUNKTION            PIC X(008).
              88 PI-ZJKI0013-DRKI0630-LIST        VALUE 'DRKI0630'.
050207        88 PI-ZJKI0013-DRKI2016-LIST        VALUE 'DRKI2016'.
200910        88 PI-ZJKI0013-DRKI2429-LIST        VALUE 'DRKI2429'.
200910        88 PI-ZJKI0013-DRKI2431-SELECT      VALUE 'DRKI2431'.
