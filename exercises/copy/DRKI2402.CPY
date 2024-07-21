      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.05.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2402.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2402.CPv  $
      *
      *   Rev 2.1   Sep 20 2012 10:28:04   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung: VN-Wechsel
      *
      *   Rev 2.0   May 04 2010 17:06:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2402.
           05  PI-DRKI2402-KEY.
             10 PI-DRKI2402-KPEVERTRAGSID   PIC X(13).
             10 PI-DRKI2402-KPEPERSONENID   PIC X(13).
      ***
       01 PO-DRKI2402.
           05 PO-DRKI2402-RC                    PIC  X(003).
               88 PO-DRKI2402-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2402-RC-NOT-FOUND                 VALUE 'NFD'.

           05 PO-DRKI2402-DATEN.
             10 PO-DRKI2402-KPEPERSONLFDNR    PIC S9(4) COMP.
             10 PO-DRKI2402-KPEPRODUKTID      PIC S9(4) COMP.
             10 PO-DRKI2402-KPEANREDE-KNR     PIC S9(4) COMP.
             10 PO-DRKI2402-KPENACHNAME       PIC X(30).
             10 PO-DRKI2402-KPEVORNAME        PIC X(30).
             10 PO-DRKI2402-KPEGEBDATUM       PIC X(10).
             10 PO-DRKI2402-KPEADRESSEID      PIC X(13).
             10 PO-DRKI2402-KPEBEGINNDATUM    PIC X(10).
             10 PO-DRKI2402-KPEENDEDATUM      PIC X(10).
             10 PO-DRKI2402-KPEPERSONENNUMMER PIC S9(4) COMP.
             10 PO-DRKI2402-KPEGESCHSTID-KNR  PIC S9(4) COMP.

