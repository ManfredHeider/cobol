      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2539.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2539.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 16:06:26   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-PERSONADR
      *ESSBEZ
      *
      *   Rev 2.0   Nov 03 2011 17:22:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2539.
           05  PI-DRKI2539-PROGRAMM            PIC  X(008).
           05  PI-DRKI2539-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2539-DATEN.
             10 PI-DRKI2539-KPEADRESSEID   PIC X(13).
      ***
       01 PO-DRKI2539.
           05 PO-DRKI2539-RC                    PIC  X(003).
             88 PO-DRKI2539-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2539-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2539-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2539-DATEN.
             10 PO-DRKI2539-KPEVERTRAGSID       PIC X(13).
             10 PO-DRKI2539-KPEPERSONLFDNR      PIC S9(4) USAGE COMP.
             10 PO-DRKI2539-KPEPRODUKTID        PIC S9(4) USAGE COMP.
             10 PO-DRKI2539-KPEPERSONENID       PIC X(13).
             10 PO-DRKI2539-KPEBEGINNDATUM      PIC X(10).
             10 PO-DRKI2539-KPEENDEDATUM        PIC X(10).
             10 PO-DRKI2539-KPEPERSONENNUMMER   PIC S9(4) USAGE COMP.
             10 PO-DRKI2539-KPEGESCHSTID-KNR    PIC S9(4) USAGE COMP.

