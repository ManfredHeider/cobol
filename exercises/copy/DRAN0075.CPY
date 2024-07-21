      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0075.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAN0075.CPv  $
      *
      *   Rev 2.9   Dec 18 2015 08:56:54   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.8   Oct 21 2015 14:38:48   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.7   Oct 24 2013 11:00:38   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.6   23 Nov 2007 11:01:02   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.5   01 Sep 2006 09:46:36   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.4   31 Aug 2006 09:43:08   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.3   Dec 30 2003 08:51:38   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.2   Oct 31 2003 10:49:52   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.1   Jul 25 2003 09:05:24   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:15:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0075  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00422!31.10.03!2.2 ! VERGRÖßERN BLACKBOX       ! RAHNER   *
      * AAN00422!30.12.03!2.3 !AKTIVIEREN ÄNDERUNGEN VOM  !          *
      *         !        !    !31.10.2003                 ! RAHNER   *
      * AAN00684!31.08.06!2.4 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AAN00730!30.11.07!2.6 ! ALLFINANZ                 ! RAHNER   *
      * AAN01073!24.10.13!2.7 ! SEPA                      ! RAHNER   *
      * AAN01223!08.12.15!2.9 ! BEGINN/ABLAUF ÄNDERBAR    ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0075                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ADVOCARD TBAN0005          *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0075.
           05 PI-DRAN0075-ADVC-KEY.
               10 PI-DRAN0075-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0075-ADVC-PIN        PIC S9(9) COMP.
      *
       01  PO-DRAN0075.
           05 PO-DRAN0075-RC                   PIC  X(003).
             88 PO-DRAN0075-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0075-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0075-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0075-ADVC-DATEN.
               10 PO-DRAN0075-ADVC-PIN         PIC S9(9) COMP.
               10 PO-DRAN0075-ADVC-LAST-UPDATE PIC X(26).
               10 PO-DRAN0075-ADVC-KENNUNG     PIC X(8).
      *
      *    ADVC-DATEN AUS PC-DATEN
           COPY DRAN0083.
      ****** ENDE COPY-MEMBER ****************************************


