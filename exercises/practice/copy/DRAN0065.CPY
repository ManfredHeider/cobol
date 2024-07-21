      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0065.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0065.CPv  $
      *
      *   Rev 2.3   27 Dec 2007 11:39:10   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.2   01 Sep 2006 09:45:40   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:54:20   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:14:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0065  FUNKTION SELECT ÜBER GES, PIN, AVERM
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.02!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00730!28.12.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0065                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE VIA-BADENIA TBAN0004       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0065.
           05 PI-DRAN0065-VIA-KEY.
               10 PI-DRAN0065-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0065-VIA-PIN         PIC S9(9) COMP.
               10 PI-DRAN0065-VIA-VDABVERM    PIC S9(9) COMP.
      *
       01  PO-DRAN0065.
           05 PO-DRAN0065-RC                   PIC  X(003).
             88 PO-DRAN0065-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0065-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0065-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0065-VIA-DATEN.
               10 PO-DRAN0065-VIA-LAST-UPDATE PIC X(26).
               10 PO-DRAN0065-VIA-KENNUNG     PIC X(8).

      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0063.

      ****** ENDE COPY-MEMBER ****************************************

