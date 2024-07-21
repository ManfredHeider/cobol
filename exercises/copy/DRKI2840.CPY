      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2840.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2840.CPv  $
      *
      *   Rev 2.1   Feb 18 2014 14:22:30   t13ex77
      *AKI01274: Prüfung der DeuBa-DTAs
      *
      *   Rev 2.0   Jan 27 2014 13:34:32   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2840.
           05  PI-DRKI2840-PROGRAMM            PIC  X(008).
           05  PI-DRKI2840-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2840-DATEN.
             10 PI-DRKI2840-GESELLSCHAFT       PIC  9(002).
             10 PI-DRKI2840-MANDANTNR          PIC  9(007).
      ***
       01 PO-DRKI2840.
           05 PO-DRKI2840-RC                   PIC  X(003).
             88 PO-DRKI2840-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2840-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2840-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2840-DATEN.
              10 PO-DRKI2840-VERTRAGSNUMMER      PIC X(15).
              10 PO-DRKI2840-GHBGHALFDNR         PIC S9(4)  COMP.
              10 PO-DRKI2840-FONDWERTPAPKNR      PIC X(6).
              10 PO-DRKI2840-FONDISIN            PIC X(12).
              10 PO-DRKI2840-GHBGUTHABENANT      PIC S9(6)V9(4) COMP-3.
              10 PO-DRKI2840-GHBBETRAGLETZTBUCH  PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI2840-GHBBUCHUNGSDATUM    PIC X(10).
              10 PO-DRKI2840-GHBKZFINHLETZTBUID  PIC S9(4)  COMP.
              10 PO-DRKI2840-GHBSTEUBETRLETZTBU  PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI2840-LETZTEAENDERUNG     PIC X(26).
              10 PO-DRKI2840-GHBNETTOBETRAG      PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI2840-GHBGUTHABPRODLFDNR  PIC S9(4)  COMP.
              10 PO-DRKI2840-GHBBUCHUNGLFDNR     PIC S9(4)  COMP.
              10 PO-DRKI2840-FONDWERTPAPKZ       PIC X(01).
