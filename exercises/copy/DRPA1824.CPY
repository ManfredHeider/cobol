      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1824.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1824.CPv  $
      *
      *   Rev 2.1   Jul 03 2015 10:38:02   T13AE37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.0   Jul 03 2015 09:17:08   T13AE37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!03.07.15!2.1 ! NEUERSTELLUNG             ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1824                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1824.
          05 PI-DRPA1824-PROGRAMM            PIC  X(008).
          05 PI-DRPA1824-UFKT                PIC  9(002).
          05 PI-DRPA1824-DATEN.
             10 PI-DRPA1824-PGEB-GES         PIC S9(4) USAGE COMP.
             10 PI-DRPA1824-PDPT-ID          PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1824-PABR-VBNR        PIC S9(9) USAGE COMP.
             10 PI-DRPA1824-PABR-ABRECH-NR   PIC X(15).
             10 PI-DRPA1824-PDPT-ID-KORR     PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1824-PABR-LAST-UPDATE PIC X(26).
             10 PI-DRPA1824-PABR-KENNUNG     PIC X(8).
      *
       01 PO-DRPA1824.
          05 PO-DRPA1824-RC                  PIC  X(003).
             88 PO-DRPA1824-RC-OK             VALUE 'OK '.
             88 PO-DRPA1824-RC-LEER           VALUE 'LER'.
             88 PO-DRPA1824-RC-EOF            VALUE 'EOF'.
      *
      ***** ENDE-COPY-MEMBER *****************************************

