      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0023.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/mf/CPY/DRAN0023.CPv  $
      *
      *   Rev 2.19   Nov 30 2020 12:15:36   t13ae34
      *AAN01492: Projekt Testoptimierung im eingereichten Geschäft 
      *2020 - AP3: Testautomatisierung
      *
      *   Rev 2.18   May 17 2019 10:45:28   t13ae23
      *AAN01419: Optimierung Auftrag AN205100 - capping killer
      *
      *   Rev 2.17   May 07 2019 11:32:38   t13ae23
      *AAN01419: Optimierung Auftrag AN205100 - capping killer
      *
      *   Rev 2.16   Jul 11 2018 09:30:46   t13ae34
      *FAN01134: In den Antragsdialogen wird zur Produktprüfung und
      * zur Einheitenberechnung fälschlicherweise das Tagesdatum bz
      *tum und nicht das fachliche Datum (Strukturerfassungsdatum b
      *
      *   Rev 2.15   Jul 09 2018 10:49:56   t13ae34
      *FAN01134: In den Antragsdialogen wird zur Produktprüfung und
      * zur Einheitenberechnung fälschlicherweise das Tagesdatum bz
      *tum und nicht das fachliche Datum (Strukturerfassungsdatum b
      *
      *   Rev 2.14   Jan 22 2018 19:33:36   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.13   Oct 13 2016 14:02:18   t13ae34
      *AAN01292: Elektra Rundlauf für Anträge
      *
      *   Rev 2.12   Jul 04 2016 10:05:32   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.11   Aug 21 2015 12:02:52   t13ae34
      *AAN01219: Zuordnung von Verträgen anhand ihrer Herkunft - Fu
      *nktion für KI
      *
      *   Rev 2.10   24 Jan 2007 15:45:28   t13ex57
      *AAN00697: Gewerbeberechtigung bei Antragserfassung DVAG-Wien
      *
      *   Rev 2.9   Jul 15 2005 11:45:14   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.8   Jul 15 2005 10:18:16   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.7   Jul 01 2005 17:14:16   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.6   Jul 01 2005 17:05:24   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.5   Jul 01 2005 16:56:40   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.4   Jul 01 2005 16:35:46   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.3   Mar 18 2005 10:32:56   t13ae34
      *AKI00479: Funktion: eingereichte Einheiten zu VB in best. Ze
      *itraum
      *
      *   Rev 2.2   11 Sep 2000 12:23:50   T13AE34
      *AAN00279-konzessionspflichtiges Geschäft
      *
      *   Rev 2.1   10 Jul 2000 15:18:30   T13AE34
      *AVB00487-Hat VB einger. Geschäft im
      *Jahr JHJJ?
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:34:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0023                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO UPAN0010              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. HEIDER                                        *
      *    DATUM:    05.07.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00487!05.07.00!2.1 ! NEU                       ! HEIDER   *
      * AAN00279!07.09.00!2.2 ! DRAN0025                  ! HEIDER   *
      * AKI00479!15.03.05!2.3 ! DRAN0107                  ! HEIDER   *
      * AAN00582!01.07.05!2.4 ! DRAN0109                  ! HEIDER   *
      * AAN00697!26.10.06!2.19! Gewerbeberechtig DVAG Wien! Boll     *
      * AAN01219!19.08.15!2.11! DRAN0304                  ! HEIDER   *
      * AAN01269!01.07.16!2.12! DRAN0310                  ! HEIDER   *
      * AAN01292!13.10.16!2.13! DRAN0311                  ! HEIDER   *
      * AAN01355!22.01.18!2.14! DRAN0321 DRAN0322         ! HEIDER   *
      * FAN01134!09.07:18!2.15! DRAN0332                  ! HEIDER   *
      * FAN01134!09.07:18!2.15! DRAN0001 STATT DRAN0332   ! HEIDER   *
      * AAN01419!06.05.19!2.17! DRAN0332 NEU              ! RAHNER   *
      * AAN01492!24.11.20!2.18! DRAN0374                  ! HEIDER   *
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
       01  PI-UPAN0010.
           05  PI-UPAN0010-FUNKTION            PIC  X(008).
               88 PI-UPAN0010-DRAN0024              VALUE 'DRAN0024'.
               88 PI-UPAN0010-DRAN0025              VALUE 'DRAN0025'.
               88 PI-UPAN0010-DRAN0107              VALUE 'DRAN0107'.
010705         88 PI-UPAN0010-DRAN0109              VALUE 'DRAN0109'.
261006         88 PI-UPAN0010-DRAN0146              VALUE 'DRAN0146'.
190815         88 PI-UPAN0010-DRAN0304              VALUE 'DRAN0304'.
010716         88 PI-UPAN0010-DRAN0310              VALUE 'DRAN0310'.
131016         88 PI-UPAN0010-DRAN0311              VALUE 'DRAN0311'.
220118         88 PI-UPAN0010-DRAN0321              VALUE 'DRAN0321'.
220118         88 PI-UPAN0010-DRAN0322              VALUE 'DRAN0322'.
090718         88 PI-UPAN0010-DRAN0001              VALUE 'DRAN0001'.
060519         88 PI-UPAN0010-DRAN0334              VALUE 'DRAN0334'.
241120         88 PI-UPAN0010-DRAN0374              VALUE 'DRAN0374'.
      *
      ******* ENDE COPY-MEMBER **************************************




