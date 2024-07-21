      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0110.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0110.CPv  $
      *
      *   Rev 2.1   Jan 19 2010 18:44:44   t13ex51
      *AKI00899: Reaktivierung postalische Prüfung
      *
      *   Rev 2.0   Jan 14 2010 16:04:38   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DROK0110.
           05  PI-DROK0110-PROGRAMM            PIC  X(008).
           05  PI-DROK0110-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0110-DATEN.
             10 PI-DROK0110-HADADRESSEALTID     PIC X(13).
      ***
       01 PO-DROK0110.
           05 PO-DROK0110-RC                    PIC  X(003).
             88 PO-DROK0110-RC-OK                        VALUE 'OK '.
             88 PO-DROK0110-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0110-RC-EOF                       VALUE 'EOF'.
           05 PO-DROK0110-DATEN.
             10 PO-DROK0110-HADPERSONENID        PIC X(13).
             10 PO-DROK0110-HADVERTRAGSID        PIC X(13).
             10 PO-DROK0110-HADADRESSEALTID      PIC X(13).
             10 PO-DROK0110-HADLFDNR             PIC S9(04) COMP.
             10 PO-DROK0110-HADLANDID            PIC X(03).
             10 PO-DROK0110-HADORT               PIC X(30).
             10 PO-DROK0110-HADPLZ               PIC X(15).
             10 PO-DROK0110-HADSTRHAUSNUM        PIC X(45).
             10 PO-DROK0110-HADORTSTEIL          PIC X(30).
             10 PO-DROK0110-HADVBID              PIC X(13).
             10 PO-DROK0110-HADAENDGRUNDID       PIC S9(04) COMP.
             10 PO-DROK0110-HADTICKETID          PIC S9(18) COMP-3.
             10 PO-DROK0110-HADDTAERLEDIGT       PIC X(01).
             10 PO-DROK0110-HADDTAERLDATUM       PIC X(10).
             10 PO-DROK0110-HADGEAENDERTAM       PIC X(10).
             10 PO-DROK0110-HADSTATUS            PIC S9(04) COMP.
             10 PO-DROK0110-LETZTEAENDERUNG      PIC X(10).
      *
      ******** Ende Copy-Member DROK0110 *****************************
      *

