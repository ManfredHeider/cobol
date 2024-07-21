      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0445.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0445.CPv  $
      *
      *   Rev 2.1   Jan 28 2009 16:08:16   t13ex51
      *AKI00772: Reisegepäckversicherung Aktualisierung
      *
      *   Rev 2.0   Dec 22 2008 16:33:52   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  PI-DRAT0445.
           05  PI-DRAT0445-KEY.
             10 PI-DRAT0445-KTY               PIC S9(4) COMP.
             10 PI-DRAT0445-BEZEICHNUNG       PIC x(100).
      *
       01  PO-DRAT0445.
           05  PO-DRAT0445-RC                 PIC  X(003).
               88 PO-DRAT0445-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0445-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0445-DATEN.
             10 PO-DRAT0445-KNR               PIC S9(15)V USAGE COMP-3.
