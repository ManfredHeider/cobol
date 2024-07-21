      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1971.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1971.CPv  $
      *
      *   Rev 2.1   27 Mar 2007 10:30:44   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   Oct 25 2006 09:44:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  PI-DRKI1971.
           05  PI-DATEN.
               10  PI-DRKI1971-OTTICKETID             PIC 9(018) COMP-3.
               10  PI-DRKI1971-OTERSTELLT             PIC X(026).
               10  PI-DRKI1971-OTERSTELLER            PIC X(030).
               10  PI-DRKI1971-OTAENDGRUND-KTY        PIC S9(4)  COMP.
               10  PI-DRKI1971-OTAENDGRUND-KNR        PIC S9(4)  COMP.

       01  PO-DRKI1971.
           05  PO-DRKI1971-RC               PIC  X(003).
               88 PO-DRKI1971-RC-OK              VALUE 'OK '.
               88 PO-DRKI1971-RC-NOK             VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI1971-OTFEHLER-KNR        PIC S9(4)  COMP.

