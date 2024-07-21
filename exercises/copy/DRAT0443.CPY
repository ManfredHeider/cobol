      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0443.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0443.CPv  $
      *
      *   Rev 2.1   Nov 17 2008 14:30:40   t13ex21
      *APA00809: SF21-CPD-Konten Erweiterung
      *
      *   Rev 2.0   Sep 24 2008 10:42:16   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0443                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0443                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    24.09.2008                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
       01  PI-DRAT0443.
           05  PI-DRAT0443-UFKT                PIC  9(02).
           05  PI-DRAT0443-PROGRAMM            PIC  X(08).
           05  PI-DRAT0443-AUSWAHL-KRITERIEN.
               10 PI-DRAT0443-PGEB-GES         PIC S9(04) COMP.
      *
       01  PO-DRAT0443.
           05  PO-DRAT0443-RC                  PIC  X(03).
               88 PO-DRAT0443-RC-OK                 VALUE 'OK '.
               88 PO-DRAT0443-RC-EOF                VALUE 'EOF'.
               88 PO-DRAT0443-RC-LEER               VALUE 'LER'.
           05  PO-DRAT0443-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRAT0443-ERGEBNISZEILE.
               10 KNR-VERTRIEB-GES             PIC S9(004) BINARY.
               10 PGVG-GUELT-AB                PIC  X(026).
               10 PGVG-GUELT-BIS               PIC  X(010).
               10 KTY-GES                      PIC S9(004) BINARY.
               10 KTY-VERTRIEB-GES             PIC S9(004) BINARY.
               10 PGVG-KONTO                   PIC  X(010).
               10 PGVG-EDV-GKTO-EIN            PIC S9(003)V COMP-3.
               10 PGVG-EDV-GKTO-AUS            PIC S9(003)V COMP-3.
               10 PGVG-LAST-UPDATE             PIC  X(026).
               10 PGVG-KENNUNG                 PIC  X(008).
      *
      ******* ENDE COPY-MEMBER **************************************
