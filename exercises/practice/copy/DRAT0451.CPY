      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0451.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0451.CPv  $
      *
      *   Rev 2.2   Feb 24 2009 16:32:38   t13ae49
      *AKI00779: Masch. DTA-Kontrolle Badenia Programme
      *
      *   Rev 2.1   Jan 28 2009 10:42:54   t13ex14
      *AKI00779: Masch. DTA-Kontrolle Badenia täglich
      *
      *   Rev 2.0   Jan 26 2009 11:25:40   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0451                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0451                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Misgeld                                          *
      *    DATUM:    26-Jan-2009                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00779 !26.01.09!    ! NEU                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKIX889: ERGEBNISMENGE _XXX_ BEREITSTELLEN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0451.
           05  PI-DRAT0451-UFKT                PIC  9(02).

           05  PI-DRAT0451-KEY.
               10 PI-DRAT0451-DTAMANDANTNR     PIC S9(9) COMP.
               10 PI-DRAT0451-DTAPARTGESID     PIC S9(4) COMP.
               10 PI-DRAT0451-DTAGRUPPE        PIC S9(4) COMP.
               10 PI-DRAT0451-DTAGDGSTATUS     PIC X(1).
      *
       01  PO-DRAT0451.
           05  PO-DRAT0451-RC                  PIC  X(003).
               88 PO-DRAT0451-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0451-RC-EOF                       VALUE 'EOF'.
               88 PO-DRAT0451-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRAT0451-DATEN.
               10 PO-DRAT0451-DTADATEI                 PIC X(36).
               10 PO-DRAT0451-DTAERSTERSATZ.
                   15 PO-DRAT0451-DTAERSTERSATZ-LEN    PIC S9(4) COMP.
                   15 PO-DRAT0451-DTAERSTERSATZ-TEXT   PIC X(1000).
               10 PO-DRAT0451-DTAGEN                   PIC X(8).
               10 PO-DRAT0451-DTAJOBNAME               PIC X(8).
               10 PO-DRAT0451-DTAKATALOGDATUM          PIC X(10).
               10 PO-DRAT0451-DTAPARTNERDATUM          PIC X(10).
               10 PO-DRAT0451-DTASATZANZAHL            PIC S9(9) COMP.
               10 PO-DRAT0451-DTALFN                   PIC S9(9) COMP.
               10 PO-DRAT0451-DTADATEINUMMER           PIC S9(4) COMP.
