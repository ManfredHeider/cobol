      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2841.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2841.CPv  $
      *
      *   Rev 2.1   Feb 18 2014 14:22:34   t13ex77
      *AKI01274: Prüfung der DeuBa-DTAs
      *
      *   Rev 2.0   Jan 27 2014 13:34:38   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2841.
           05  PI-DRKI2841-PROGRAMM            PIC  X(008).
           05  PI-DRKI2841-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2841-DATEN.
             10 PI-DRKI2841-VERTRAGSID         PIC  X(013).
      ***
       01 PO-DRKI2841.
           05 PO-DRKI2841-RC                   PIC  X(003).
             88 PO-DRKI2841-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2841-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2841-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2841-DATEN.
              10 PO-DRKI2841-VERTRAGSNUMMER      PIC X(15).
              10 PO-DRKI2841-GHBGHALFDNR         PIC S9(4)  COMP.
              10 PO-DRKI2841-FONDWERTPAPKNR      PIC X(6).
              10 PO-DRKI2841-FONDISIN            PIC X(12).
              10 PO-DRKI2841-GHBGUTHABENANT      PIC S9(6)V9(4) COMP-3.
              10 PO-DRKI2841-GHBBETRAGLETZTBUCH  PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI2841-GHBBUCHUNGSDATUM    PIC X(10).
              10 PO-DRKI2841-GHBKZFINHLETZTBUID  PIC S9(4)  COMP.
              10 PO-DRKI2841-GHBSTEUBETRLETZTBU  PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI2841-LETZTEAENDERUNG     PIC X(26).
              10 PO-DRKI2841-GHBNETTOBETRAG      PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI2841-GHBGUTHABPRODLFDNR  PIC S9(4)  COMP.
              10 PO-DRKI2841-GHBBUCHUNGLFDNR     PIC S9(4)  COMP.
              10 PO-DRKI2841-FONDWERTPAPKZ       PIC X(01).
