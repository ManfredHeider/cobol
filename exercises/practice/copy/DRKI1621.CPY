      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1621.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI1621.CPv  $
      *
      *   Rev 2.13   Jan 11 2017 18:21:06   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle
      *
      *   Rev 2.12   Feb 24 2016 14:59:36   t13ex27
      *AKI01409: Abgleich-Kundenübertragungsart berücksichtigen
      *
      *   Rev 2.11   Feb 24 2016 14:46:28   t13ex27
      *AKI01409: Abgleich-Kundenübertragungsart berücksichtigen
      *
      *   Rev 2.10   08 Nov 2007 13:25:30   t13ex27
      *AKL00664: Allfinanz: Internetanfragen
      *
      *   Rev 2.9   Sep 14 2006 10:49:04   t13ex48
      *AKI00531: Kundenbetreuungsvereinbarung PGM für KÜ
      *
      *   Rev 2.8   Feb 02 2006 12:12:48   t13ex27
      *AKI00543: Internetadressen - Umkreissuche
      *
      *   Rev 2.7   Feb 02 2006 11:38:48   t13ex27
      *AKI00543: Internetadressen - Umkreissuche
      *
      *   Rev 2.6   Feb 02 2006 11:31:14   t13ex27
      *AKI00543: Internetadressen - Umkreissuche
      *
      *   Rev 2.5   Feb 02 2006 11:29:30   t13ex27
      *AKI00543: Internetadressen - Umkreissuche
      *
      *   Rev 2.3   Aug 22 2005 08:58:54   t13ex27
      *AKI00453: Elektronische Verarbeitung von Adress-Daten aus de
      *m Internet
      *
      *   Rev 2.2   Jan 10 2005 10:32:12   t13ex27
      *AKI00449: Problematische Bestandskunden in der KoKÜ
      *
      *   Rev 2.1   Mar 24 2004 10:19:44   t13ex27
      *AKI00393 - KoKÜ auf Anfrage
      *
      *   Rev 2.0   Dec 01 2003 11:34:52   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00393!01.12.03!2.1 ! NEU                       ! I.Frenzel*
      * AKI00449!18.10.04!2.2 ! neue Funktion             ! I.Frenzel*
      * AKI00453!02.05.05!2.3 ! neue Funktionen           ! I.Frenzel*
      * AKI00543!24.01.06!2.4 ! neue Funktionen           ! I.Frenzel*
160606* AKI00531!16.06.06!2.4 ! neue Funktion             ! HSE      *
081107* AKI00662!26.06.07!2.10! neue Funktionen           ! I.Frenzel*
      * AKI01461!11.01.17!2.13! Funktionen entfernt       ! I.Frenzel*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   Funktions-Copy für JOIN-OZP ZJKI0023                       *
      *                                                              *
      ****************************************************************
       01  PI-ZJKI0023.
           05  PI-ZJKI0023-FUNKTION            PIC  X(008).
               88 PI-ZJKI0023-DRKI1734              VALUE 'DRKI1734'.
160606         88 PI-ZJKI0023-DRKI1921              VALUE 'DRKI1921'.
      *
      ******* ENDE COPY-MEMBER DRKI1621 *****************************



