      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0069.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DROK0069.CPv  $
      *
      *   Rev 2.6   Jan 23 2014 10:48:36   t13ex27
      *AKI01063: Wartungsauftrag-Ticketverarbeitung-Kundenlöschung
      *
      *   Rev 2.5   Jan 19 2010 18:44:52   t13ex51
      *AKI00899: Reaktivierung postalische Prüfung
      *
      *   Rev 2.4   May 14 2007 10:58:12   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Teil 2
      *
      *   Rev 2.3   05 Apr 2007 17:10:54   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB
      *
      *   Rev 2.2   05 Apr 2007 15:43:34   t13ex27
      *AKI00557: Ticketverfahren AD/ID und Kunde-VB-Beziehung und A
      *dressänderung
      *
      *   Rev 2.1   Apr 04 2007 15:55:44   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Adressaenderungen
      *
      *   Rev 2.0   Feb 07 2007 15:28:46   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * Funktionskonstanten fuer LIST-OZP ZXOK0012 (HISTADRESSAENDERG)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!07.02.07! 01 ! NEUERSTELLUNG             !H. Sebbach*
      * --------!--------!----!---------------------------!----------*
      *         !05.04.07!2.2 ! neu Funktion              !I. Frenzel*
      * --------!--------!----!---------------------------!----------*
250407*         !25.04.07!2.3 ! neu Funktion              !H. Sebbach*
      * --------!--------!----!---------------------------!----------*
140110* AKI00899!14.01.10!2.3 ! neu Funktion              !HRO       *
      * AKI01063!05.12.13!2.6 ! Vertragslöschung          ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXOK0012.
           05 PI-ZXOK0012-FUNKTION            PIC X(008).
              88 PI-ZXOK0012-DROK0070-LIST        VALUE 'DROK0070'.
              88 PI-ZXOK0012-DROK0082-LIST        VALUE 'DROK0082'.
050407        88 PI-ZXOK0012-DROK0084-LIST        VALUE 'DROK0084'.
250407        88 PI-ZXOK0012-DROK0088-LIST        VALUE 'DROK0088'.
140110        88 PI-ZXOK0012-DROK0110-LIST        VALUE 'DROK0110'.
051213        88 PI-ZXOK0012-DROK0085-LIST        VALUE 'DROK0085'.
      ******************************************************************


