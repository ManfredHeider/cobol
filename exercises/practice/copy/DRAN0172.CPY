      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0172.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0172.CPv  $
      *
      *   Rev 2.1   10 Nov 2006 13:05:48   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   09 Nov 2006 16:35:52   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0172  FUNKTION SELECT STANDARD-OZP
      *                 FÜR TBAN0011
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!10.11.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0172                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANTQ TBAN0011              *
      *                                                              *
      ****************************************************************
      *    COPY-MEMBER: DRAN0172  FUNKTION SELECT                    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!14.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0172                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANTQ TBAN0009              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0172.
          05 ANTQ-KEY.
             10 PI-DRAN0172-PGEB-GES              PIC S9(4) COMP.
             10 PI-DRAN0172-PROD-PROD-SCHL        PIC S9(4) COMP.
             10 PI-DRAN0172-ANTQ-PLAUSI           PIC S9(4) COMP.
             10 PI-DRAN0172-ANTQ-GUELTIG-AB       PIC X(10).
      *
       01 PO-DRAN0172.
          05 PO-DRAN0172-RC                       PIC  X(003).
             88 PO-DRAN0172-RC-OK                      VALUE 'OK '.
             88 PO-DRAN0172-RC-NOT-FOUND               VALUE 'NFD'.
             88 PO-DRAN0172-RC-KEY-NOT-UNIQUE          VALUE 'KNU'.
      *
          05 PO-DRAN0172-DATEN.
              10 PO-DRAN0172-PGEB-GES             PIC S9(4) COMP.
              10 PO-DRAN0172-PROD-PROD-SCHL       PIC S9(4) COMP.
              10 PO-DRAN0172-ANTQ-PLAUSI          PIC S9(4) COMP.
              10 PO-DRAN0172-ANTQ-GUELTIG-AB      PIC X(10).
              10 PO-DRAN0172-ANTQ-GUELTIG-BIS     PIC X(10).
              10 PO-DRAN0172-ANTQ-WERT-NUM        PIC S9(9)V9(3) COMP-3.
              10 PO-DRAN0172-ANTQ-WERT-ALPHANUM   PIC X(30).
              10 PO-DRAN0172-ANTQ-LAST-UPDATE     PIC X(26).
              10 PO-DRAN0172-ANTQ-KENNUNG         PIC X(8).

      ****** ENDE COPY-MEMBER ****************************************
