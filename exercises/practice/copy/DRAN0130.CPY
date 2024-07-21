      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0130.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0130.CPv  $
      *
      *   Rev 2.2   22 Dec 2006 20:27:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 10:38:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:27:14   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0130  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!22.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
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
      *    >>>  DELETE ALLE PROD ZU ANTRAG <<<  TABELLE ANTP TBAN0009*
      *                                                              *
      ****************************************************************
       01  PI-DRAN0130.
           05 PI-DRAN0130-LAST-UPDATE         PIC  X(026).
           05 ANTP-KEY.
              10 PGEB-GES                     PIC S9(4) COMP.
              10 ANTP-NR                      PIC S9(15)V COMP-3.
      *
       01  PO-DRAN0130.
           05 PO-DRAN0130-RC                  PIC  X(003).
              88 PO-DRAN0130-RC-OK                  VALUE 'OK '.
              88 PO-DRAN0130-RC-NOT-FOUND           VALUE 'NFD'.

