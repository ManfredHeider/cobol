      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0169.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0169.CPv  $
      *
      *   Rev 2.1   10 Nov 2006 12:57:54   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   09 Nov 2006 16:25:20   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0169  FUNKTION INSERT STANDARD-OZP
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
      *    FÜR DIE FUNKTION DRAN0169                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE ANTQ TBAN0011              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0169.
           05 PI-DRAN0169-PGEB-GES                PIC S9(4) COMP.
           05 PI-DRAN0169-PROD-PROD-SCHL          PIC S9(4) COMP.
           05 PI-DRAN0169-ANTQ-PLAUSI             PIC S9(4) COMP.
           05 PI-DRAN0169-ANTQ-GUELTIG-AB         PIC X(10).
           05 PI-DRAN0169-ANTQ-GUELTIG-BIS        PIC X(10).
           05 PI-DRAN0169-ANTQ-WERT-NUM           PIC S9(9)V9(3) COMP-3.
           05 PI-DRAN0169-ANTQ-WERT-ALPHANUM      PIC X(30).
           05 PI-DRAN0169-ANTQ-KENNUNG            PIC X(8).
      *
       01  PO-DRAN0169.
           05 PO-DRAN0169-RC                      PIC X(03).
             88 PO-DRAN0169-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0169-RC-DUPLICATE-VALUE        VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ***************************************
