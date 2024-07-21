      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0170.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0170.CPv  $
      *
      *   Rev 2.1   10 Nov 2006 13:00:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   09 Nov 2006 16:28:56   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0170  FUNKTION UPDATE STANDARD-OZP
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
      *    FÜR DIE FUNKTION DRAN0170                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE ANTQ TBAN0011              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0170.
           05 PI-DRAN0170-LAST-UPDATE             PIC X(26).
           05 ANTQ-KEY.
              10 PI-DRAN0170-PGEB-GES             PIC S9(4) COMP.
              10 PI-DRAN0170-PROD-PROD-SCHL       PIC S9(4) COMP.
              10 PI-DRAN0170-ANTQ-PLAUSI          PIC S9(4) COMP.
              10 PI-DRAN0170-ANTQ-GUELTIG-AB      PIC X(10).

           05 ANTQ-DATEN.
              10 PI-DRAN0170-ANTQ-GUELTIG-BIS     PIC X(10).
              10 PI-DRAN0170-ANTQ-WERT-NUM        PIC S9(9)V9(3) COMP-3.
              10 PI-DRAN0170-ANTQ-WERT-ALPHANUM   PIC X(30).
              10 PI-DRAN0170-ANTQ-KENNUNG         PIC X(8).
      *
      *
       01 PO-DRAN0170.
          05 PO-DRAN0170-RC                       PIC  X(003).
             88 PO-DRAN0170-RC-OK                      VALUE 'OK '.
             88 PO-DRAN0170-RC-CHANGED                 VALUE 'CHA'.
             88 PO-DRAN0170-RC-NOT-FOUND               VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
