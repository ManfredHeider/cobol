      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0121.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0121.CPv  $
      *
      *   Rev 2.5   Dec 14 2011 16:58:08   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.4   26 Oct 2006 08:52:56   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   24 Oct 2006 13:52:18   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Sep 26 2006 10:01:00   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   15 Sep 2006 16:21:54   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:25:40   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0121  FUNKTION INSERT STANDARD-OZP
      *                 FÜR TBAN0008
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!12.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01033!        !2.2 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0121                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE ANTA TBAN0008              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0121.
          05 PI-DRAN0121-DATEN.
             10 PI-DRAN0121-ANTRAG-DATEN.
                15 PI-DRAN0121-ANTRAG-KOPF.
                   COPY DRAN0141.
                15 PI-DRAN0121-ANTRAG-PE-STRUKTUR.
                   COPY DRAN0142.

       01 PO-DRAN0121.
          05 PO-DRAN0121-RC                         PIC  X(003).
             88 PO-DRAN0121-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0121-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ***************************************
