      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.12.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0312.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0312.CPv  $
      *
      *   Rev 2.2   Jan 03 2017 16:21:40   t13ae34
      *AAN01304: Antragsdatum
      *
      *   Rev 2.1   Dec 23 2016 14:09:20   t13ae34
      *AAN01304: Erfassung eAnträge zum Monatsschluss
      *
      *   Rev 2.0   Dec 22 2016 16:34:30   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0312 FUNKTION                             *
      *                 ERMITTELN WORKFLOW ANTRAG PE-STRUKTUR         *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN01304!23.12.16!2.1 ! NEU                       ! HEIDER    *
      * AAN01304!03.01.17!2.2 ! ANTRAGSDATUM              ! HEIDER    *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
       01 PI-DRAN0312.
          05 ANTA-KEY.
             10 PGEB-GES                         PIC S9(4)   COMP.
             10 ANTA-AVERM                       PIC S9(9)   COMP.
             10 ANTA-STRUK-DAT                   PIC X(10).
             10 ANTA-STATISTIK-DAT               PIC X(10).
             10 ANTA-AN-AUFN-DAT                 PIC X(10).
      *
       01 PO-DRAN0312.
      *      RC PE-STRUKTUR
          05 PO-DRAN0312-RC                      PIC  X(003).
             88 PO-DRAN0312-RC-PE-S-OK                VALUE 'PEO'.
             88 PO-DRAN0312-RC-PE-S-NOK               VALUE 'PEN'.
      *


