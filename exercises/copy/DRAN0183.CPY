      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0183.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0183.CPv  $
      *
      *   Rev 2.1   22 Dec 2006 23:25:02   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   19 Dec 2006 16:09:20   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0183  FUNKTION SELECT STANDARD-OZP
      *                 FÜR TBAN0011 -> AKTUELLER EINTRAG
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!19.12.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0183                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  AKTUELLER EINTRAG <<<   TABELLE ANTQ TBAN0011*
      *                                                              *
      ****************************************************************
      *    COPY-MEMBER: DRAN0183  FUNKTION SELECT                    *
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
      *    FÜR DIE FUNKTION DRAN0183                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANTQ TBAN0009              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0183.
          05 ANTQ-KEY.
             10 PI-DRAN0183-PGEB-GES              PIC S9(4) COMP.
             10 PI-DRAN0183-PROD-PROD-SCHL        PIC S9(4) COMP.
             10 PI-DRAN0183-ANTQ-PLAUSI           PIC S9(4) COMP.
      *
       01 PO-DRAN0183.
          05 PO-DRAN0183-RC                       PIC  X(003).
             88 PO-DRAN0183-RC-OK                      VALUE 'OK '.
             88 PO-DRAN0183-RC-NOT-FOUND               VALUE 'NFD'.
             88 PO-DRAN0183-RC-KEY-NOT-UNIQUE          VALUE 'KNU'.
      *
          05 PO-DRAN0183-DATEN.
              10 PO-DRAN0183-PGEB-GES             PIC S9(4) COMP.
              10 PO-DRAN0183-PROD-PROD-SCHL       PIC S9(4) COMP.
              10 PO-DRAN0183-ANTQ-PLAUSI          PIC S9(4) COMP.
              10 PO-DRAN0183-ANTQ-GUELTIG-AB      PIC X(10).
              10 PO-DRAN0183-ANTQ-GUELTIG-BIS     PIC X(10).
              10 PO-DRAN0183-ANTQ-WERT-NUM        PIC S9(9)V9(3) COMP-3.
              10 PO-DRAN0183-ANTQ-WERT-ALPHANUM   PIC X(30).
              10 PO-DRAN0183-ANTQ-LAST-UPDATE     PIC X(26).
              10 PO-DRAN0183-ANTQ-KENNUNG         PIC X(8).

      ****** ENDE COPY-MEMBER ****************************************
