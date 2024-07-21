      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0125.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0125.CPv  $
      *
      *   Rev 2.5   Dec 14 2011 16:58:30   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.4   26 Oct 2006 08:53:56   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   24 Oct 2006 13:53:38   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Sep 26 2006 10:17:40   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   15 Sep 2006 16:22:38   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:26:22   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0125  FUNKTION SELECT ÜBER GES, VORSCHNR *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!14.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01033!        !2.5 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0125                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANTA TBAN0008              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0125.
           05 ANTA-KEY.
               10 PGEB-GES                       PIC S9(4)   COMP.
               10 ANTA-VORSCHLAG-NR              PIC X(17).
      *
       01  PO-DRAN0125.
           05 PO-DRAN0125-RC                     PIC  X(003).
             88 PO-DRAN0125-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0125-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRAN0125-RC-KEY-NOT-UNIQUE         VALUE 'KNU'.
      *
      *
          05 PO-DRAN0125-ANTRAG-DATEN.
             15 PO-DRAN0125-ANTRAG-KOPF.
                COPY DRAN0141.
             15 ANTA-LAST-UPDATE                 PIC  X(026).
             15 ANTA-KENNUNG                     PIC  X(008).
             15 PO-DRAN0125-ANTRAG-PE-STRUKTUR.
                COPY DRAN0142.

      ****** ENDE COPY-MEMBER ****************************************

