      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0292.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/mf/CPY/DRKI0292.CPv  $
      *
      *   Rev 2.19   Oct 11 2019 09:59:30   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer 
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.18   May 06 2019 13:47:06   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.17   Nov 06 2015 12:29:32   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.16   Nov 27 2014 09:45:24   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa ZTKI0315
      *
      *   Rev 2.15   Sep 29 2010 07:59:10   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.14   Sep 23 2010 12:21:44   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.13   Sep 23 2010 09:56:00   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.12   Sep 23 2010 09:40:24   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.11   Sep 23 2010 09:21:06   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.10   Nov 02 2009 08:33:12   t13ex39
      *AKI00844: DWS-Datenspiegel für DWS-Vorsorgeprodukte
      *
      *   Rev 2.9   Oct 27 2009 12:37:42   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.8   Oct 27 2009 12:06:24   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.7   Aug 12 2009 16:11:34   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 1.0   07 Jul 1998 11:50:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0292                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0315                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.04.2019 / 07:33:50 / T13EX27
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
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
       01  PI-ZTKI0315.
           05  PI-ZTKI0315-FUNKTION              PIC  X(008).


             88   PI-ZTKI0315-DRKI0293     VALUE 'DRKI0293'.
             88   PI-ZTKI0315-DRKI0294     VALUE 'DRKI0294'.
             88   PI-ZTKI0315-DRKI0295     VALUE 'DRKI0295'.
             88   PI-ZTKI0315-DRKI0296     VALUE 'DRKI0296'.
             88   PI-ZTKI0315-DRKI0383     VALUE 'DRKI0383'.
             88   PI-ZTKI0315-DRKI1128     VALUE 'DRKI1128'.
             88   PI-ZTKI0315-DRKI1274     VALUE 'DRKI1274'.
             88   PI-ZTKI0315-DRKI1493     VALUE 'DRKI1493'.
             88   PI-ZTKI0315-DRKI1494     VALUE 'DRKI1494'.
             88   PI-ZTKI0315-DRKI1542     VALUE 'DRKI1542'.
             88   PI-ZTKI0315-DRKI1543     VALUE 'DRKI1543'.
             88   PI-ZTKI0315-DRKI1544     VALUE 'DRKI1544'.
             88   PI-ZTKI0315-DRKI1546     VALUE 'DRKI1546'.
             88   PI-ZTKI0315-DRKI2240     VALUE 'DRKI2240'.
             88   PI-ZTKI0315-DRKI2348     VALUE 'DRKI2348'.
             88   PI-ZTKI0315-DRKI2349     VALUE 'DRKI2349'.
             88   PI-ZTKI0315-DRKI2350     VALUE 'DRKI2350'.
             88   PI-ZTKI0315-DRKI2425     VALUE 'DRKI2425'.
             88   PI-ZTKI0315-DRKI2426     VALUE 'DRKI2426'.
             88   PI-ZTKI0315-DRKI2938     VALUE 'DRKI2938'.
             88   PI-ZTKI0315-DRKI2939     VALUE 'DRKI2939'.
             88   PI-ZTKI0315-DRKI2940     VALUE 'DRKI2940'.
             88   PI-ZTKI0315-DRKI2941     VALUE 'DRKI2941'.
             88   PI-ZTKI0315-DRKI3045     VALUE 'DRKI3045'.

      *
      ******* ENDE COPY-MEMBER **************************************


