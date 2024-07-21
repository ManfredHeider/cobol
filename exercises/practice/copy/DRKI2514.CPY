      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2514.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2514.CPv  $
      *
      *   Rev 2.1   Sep 21 2011 10:39:06   t13ex27
      *FKI00911: Verträge löschen-LV
      *
      *   Rev 2.0   Jul 14 2011 10:12:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2514.
           05  PI-DRKI2514-PROGRAMM            PIC  X(008).
           05  PI-DRKI2514-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2514-DATEN.
             10 PI-DRKI2514-LPEVERTRAGSID      PIC X(13).
      ***
       01 PO-DRKI2514.
           05 PO-DRKI2514-RC                   PIC  X(003).
             88 PO-DRKI2514-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2514-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2514-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2514-DATEN.
              10 PO-DRKI2514-LPEPERSROLLEKZ    PIC  X(001).
              10 PO-DRKI2514-LPENACHNAME       PIC  X(030).
              10 PO-DRKI2514-LPEVORNAME        PIC  X(030).
              10 PO-DRKI2514-LPEGEBDATUM       PIC  X(010).
              10 PO-DRKI2514-LPEADRESSEID      PIC  X(013).
              10 PO-DRKI2514-LPEANREDE-KNR     PIC S9(004) USAGE COMP.
