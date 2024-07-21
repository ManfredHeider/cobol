      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0069.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0069.CPv  $
      *
      *   Rev 2.3   27 Dec 2007 11:39:20   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.2   01 Sep 2006 09:46:12   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:54:26   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:15:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0069  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00730!28.12.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0069                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE VIA TBAN0004               *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0069.
           05 PI-DRAN0069-VIA-KEY.
               10 PI-DRAN0069-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0069-VIA-HVANTNUM    PIC S9(17) COMP-3.
      *
       01  PO-DRAN0069.
           05 PO-DRAN0069-RC                   PIC  X(003).
             88 PO-DRAN0069-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0069-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0069-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0069-VIA-DATEN.
               10 PO-DRAN0069-VIA-PIN         PIC S9(9) COMP.
               10 PO-DRAN0069-VIA-PIN2        PIC S9(9) COMP.
               10 PO-DRAN0069-VIA-LAST-UPDATE PIC X(26).
               10 PO-DRAN0069-VIA-KENNUNG     PIC X(8).
      *    PZM-DATEN AUS PC-DATEN
           COPY DRAN0063.
      ****** ENDE COPY-MEMBER ****************************************

