      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0129.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0129.CPv  $
      *
      *   Rev 2.2   22 Dec 2006 20:24:46   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 10:34:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:27:04   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0129  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!22.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!22.12.06!2.2 ! PK ÄNDERUNG               ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0123                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE ANTP TBAN0009              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0129.
           05 PI-DRAN0129-LAST-UPDATE         PIC  X(026).
           05 ANTP-KEY.
              10 PGEB-GES                     PIC S9(4) COMP.
              10 ANTP-NR                      PIC S9(15)V COMP-3.
              10 PROD-PROD-SCHL               PIC S9(4) COMP.
              10 ANTP-LFD-NR                  PIC S9(4) COMP.
      *
       01  PO-DRAN0129.
           05 PO-DRAN0129-RC                  PIC  X(003).
              88 PO-DRAN0129-RC-OK                  VALUE 'OK '.
              88 PO-DRAN0129-RC-CHANGED             VALUE 'CHA'.
              88 PO-DRAN0129-RC-NOT-FOUND           VALUE 'NFD'.

