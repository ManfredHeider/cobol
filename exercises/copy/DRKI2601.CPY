      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2601.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2601.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:06:50   t13ex27
      *AKI01135: Kundendaten physisch löschen-UP für Vertragsdaten
      *
      *   Rev 2.0   Apr 27 2012 11:29:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2601.
           05  PI-DRKI2601-PROGRAMM            PIC  X(008).
           05  PI-DRKI2601-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2601-DATEN.
             10 PI-DRKI2601-VERVERTRAGSID      PIC X(13).
      ***
       01 PO-DRKI2601.
           05 PO-DRKI2601-RC                   PIC  X(003).
             88 PO-DRKI2601-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2601-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2601-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2601-DATEN.
              10 PO-DRKI2601-ADRESSEID         PIC  X(013).

