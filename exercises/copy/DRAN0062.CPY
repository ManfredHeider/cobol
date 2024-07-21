      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0062.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0062.CPv  $
      *
      *   Rev 2.4   27 Dec 2007 11:39:02   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.3   01 Sep 2006 09:45:20   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:42:52   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:54:18   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:14:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0062  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03 2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00684!31.08.06!2.2 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AAN00730!28.12.07!2.4 ! ALLFINANZ                 ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0062                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE VIA-BADENIA TBAN0004       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0062.
           05 PI-DRAN0062-VIA-KEY.
               10 PI-DRAN0062-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0062-VIA-PIN         PIC S9(9) COMP.
      *
       01  PO-DRAN0062.
           05 PO-DRAN0062-RC                   PIC  X(003).
             88 PO-DRAN0062-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0062-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0062-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0062-VIA-DATEN.
               10 PO-DRAN0038-VIA-PIN         PIC S9(9) COMP.
               10 PO-DRAN0038-VIA-PIN2        PIC S9(9) COMP.
               10 PO-DRAN0062-VIA-LAST-UPDATE PIC X(26).
               10 PO-DRAN0062-VIA-KENNUNG     PIC X(8).

      *
      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0063.
      ****** ENDE COPY-MEMBER ****************************************

