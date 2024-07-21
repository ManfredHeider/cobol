      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2604.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2604.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 16:06:36   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-PERSONADR
      *ESSBEZ
      *
      *   Rev 2.0   May 02 2012 14:21:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2604.
           05  PI-DRKI2604-PROGRAMM            PIC  X(008).
           05  PI-DRKI2604-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2604-DATEN.
             10 PI-DRKI2604-KPEPERSONENID   PIC X(13).
      ***
       01 PO-DRKI2604.
           05 PO-DRKI2604-RC                    PIC  X(003).
             88 PO-DRKI2604-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2604-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2604-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2604-DATEN.
             10 PO-DRKI2604-KPEVERTRAGSID       PIC X(13).
             10 PO-DRKI2604-KPEPERSONLFDNR      PIC S9(4) USAGE COMP.
             10 PO-DRKI2604-KPEPRODUKTID        PIC S9(4) USAGE COMP.
             10 PO-DRKI2604-KPEADRESSEID        PIC X(13).
             10 PO-DRKI2604-KPEBEGINNDATUM      PIC X(10).
             10 PO-DRKI2604-KPEENDEDATUM        PIC X(10).
             10 PO-DRKI2604-KPEPERSONENNUMMER   PIC S9(4) USAGE COMP.
             10 PO-DRKI2604-KPEGESCHSTID-KNR    PIC S9(4) USAGE COMP.

