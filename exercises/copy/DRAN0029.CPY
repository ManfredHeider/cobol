      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0029.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRAN0029.CPv  $
      *
      *   Rev 2.11   Dec 18 2018 11:19:40   t13ae23
      *FAN01150: DVAG-Wien EGES: Geburtsdatum für Kunden 2 - 5 mit 
      *inkorrekter Jahreszahl
      *
      *   Rev 2.10   Dec 07 2018 10:15:28   t13ae34
      *AAN01403: Papieranträge Deutsche Bank und Fondsdepotbank: Es
      * sollen zu allen Anträgen Host-Antragsdaten (DFÜ-Daten) gesc
      *rden
      *
      *   Rev 2.9   Dec 07 2018 10:08:50   t13ae34
      *AAN01403: Papieranträge Deutsche Bank und Fondsdepotbank: Es
      * sollen zu allen Anträgen Host-Antragsdaten (DFÜ-Daten) gesc
      *rden
      *
      *   Rev 2.8   08 Sep 2006 09:39:48   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.7   Feb 01 2005 15:19:16   t13ae34
      *AAN00544: DB2+Batch: PoA Optimierung Advocard
      *
      *   Rev 2.6   Aug 06 2004 08:35:32   t13ae23
      *AAN00455: Elektronischer Antrag DWS - Programme -
      *
      *   Rev 2.5   Jul 25 2003 09:03:54   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.4   Mar 06 2003 16:54:14   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.3   Jun 03 2002 18:12:38   t13ae34
      *AAN00357 - POA-AMV
      *
      *   Rev 2.2   Apr 23 2002 16:59:46   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.1   27 Feb 2001 11:49:50   T13AE34
      *AAN00268-POA Antrag mit FAN00288
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0029                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO UPAN0011              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. HEIDER                                        *
      *    DATUM:    27.11.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!27.11.00!2.1 ! NEU                       ! HEIDER   *
      * AAN00357!25.03.02!2.2 ! POA-FUNKTIONEN            ! HEIDER   *
      * AAN00357!24.05.02!2.3 ! POA-FUNKTION DRAN0059     ! HEIDER   *
      * AAN00409!07.03.03!2.4 ! VIA-FUNKTIONEN            ! RAHNER   *
      * AAN00422!25.07.03!2.5 ! ADVOCARD-FUNKTIONEN       ! RAHNER   *
      * AAN00455!06.08.04!2.6 ! DWS-FUNKTIONEN            ! RAHNER   *
      * AAN00544!01.02.05!2.7 ! ENTFERNEN FUNKTIONEN      ! HEIDER   *
      * AAN00685!08.09.06!2.8 ! UMSTELLEN AML AUF BLACK-  !          *
      *         !        !    ! BOX-VERFAHREN             ! RAHNER   *
      * AAN01403!05.12.18!2.9 ! DRAN0349 DRAN0352         ! HEIDER   *
      * FAN01150!18.12.18!2.11! C+L WG. UWAN0001          ! RAHNER   *
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
       01  PI-UPAN0011.
           05  PI-UPAN0011-FUNKTION            PIC  X(008).
               88 PI-UPAN0011-DRAN0030              VALUE 'DRAN0030'.
               88 PI-UPAN0011-DRAN0031              VALUE 'DRAN0031'.
               88 PI-UPAN0011-DRAN0032              VALUE 'DRAN0032'.
               88 PI-UPAN0011-DRAN0066              VALUE 'DRAN0066'.
               88 PI-UPAN0011-DRAN0067              VALUE 'DRAN0067'.
               88 PI-UPAN0011-DRAN0068              VALUE 'DRAN0068'.
               88 PI-UPAN0011-DRAN0094              VALUE 'DRAN0094'.
               88 PI-UPAN0011-DRAN0095              VALUE 'DRAN0095'.
               88 PI-UPAN0011-DRAN0096              VALUE 'DRAN0096'.
051218         88 PI-UPAN0011-DRAN0349              VALUE 'DRAN0349'.
051218         88 PI-UPAN0011-DRAN0352              VALUE 'DRAN0352'.
      *
      ******* ENDE COPY-MEMBER **************************************



