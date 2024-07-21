      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0458.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0458.CPv  $
      *
      *   Rev 2.1   Feb 24 2009 16:32:58   t13ae49
      *AKI00779: Masch. DTA-Kontrolle Badenia Programme
      *
      *   Rev 2.0   Feb 03 2009 14:51:14   t13ae49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0458                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0451                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WEHLITZ                                          *
      *    DATUM:    21-Jan-2009                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00779!21.01.09!    ! NEU                       ! WEHLITZ  *
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
       01  PI-DRAT0458.
           05  PI-DRAT0458-UFKT                PIC  9(02).

           05  PI-DRAT0458-KEY.
               10 PI-DRAT0458-DTAMANDANTNR     PIC S9(9) COMP.
               10 PI-DRAT0458-DTAPARTGESID     PIC S9(4) COMP.
               10 PI-DRAT0458-DTAGRUPPE        PIC S9(4) COMP.
               10 PI-DRAT0458-DTAPARTNERDATUM  PIC X(10).
      *
       01  PO-DRAT0458.
           05  PO-DRAT0458-RC                  PIC  X(003).
               88 PO-DRAT0458-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0458-RC-EOF                       VALUE 'EOF'.
               88 PO-DRAT0458-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRAT0458-DATEN.
               10 PO-DRAT0458-DTADATEI                 PIC X(36).
               10 PO-DRAT0458-DTAERSTERSATZ.
                   15 PO-DRAT0458-DTAERSTERSATZ-LEN    PIC S9(4) COMP.
                   15 PO-DRAT0458-DTAERSTERSATZ-TEXT   PIC X(1000).
               10 PO-DRAT0458-DTAGEN                   PIC X(8).
               10 PO-DRAT0458-DTAJOBNAME               PIC X(8).
               10 PO-DRAT0458-DTAKATALOGDATUM          PIC X(10).
               10 PO-DRAT0458-DTASATZANZAHL            PIC S9(9) COMP.
               10 PO-DRAT0458-DTALFN                   PIC S9(9) COMP.
               10 PO-DRAT0458-DTADATEINUMMER           PIC S9(4) COMP.
               10 PO-DRAT0458-DTAGDGSTATUS             PIC X(1).
