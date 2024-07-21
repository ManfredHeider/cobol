      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/03/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0460.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0460.CPv  $
      *
      *   Rev 2.1   Apr 03 2013 11:24:24   t13ex73
      *AKI01211: Anpassung der Produkt- und Tarifnamen für Badenia-
      *Produkte
      *
      *   Rev 2.0   Mar 6 2013 15:37:36   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BADPRODTARIFBEZ.
           10 PTBPRODSCHL                   PIC  X(0002).
           10 PTBTARIFVARIANTE              PIC  X(0001).
           10 PTBWOHNPSPARKEY               PIC S9(4) COMP.
           10 PTBPRODUKTTEXT                PIC  X(0050).
           10 NIL-PTBPRODUKTTEXT            PIC  X(01).
           10 PTBTARIFTEXT                  PIC  X(0050).
           10 NIL-PTBTARIFTEXT              PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
