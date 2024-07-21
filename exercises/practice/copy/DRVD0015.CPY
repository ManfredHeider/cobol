      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0015.CPv  $
      *
      *   Rev 2.1   Nov 23 2004 16:48:56   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 22 2004 09:27:24   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!16.06.04!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Daten aus Vertrag und VertragRolleBez und KundenInteressent
      *
      * Satzlänge 320 Byte
      *
       01 DRVD0015.

           05  DRVD0015-Satzart                 PIC X(01).
           05  DRVD0015-VERTRAGSID              PIC X(13).

           05  DRVD0015-GHPPRODUKTID            PIC 9(05).
           05  DRVD0015-GHPGUTHABPRODLFDNR      PIC 9(05).
           05  DRVD0015-GHPEIGENVERMITTELT      PIC X(01).
           05  FILLER                           PIC X(03).
           05  DRVD0015-GHPABSCHLUSSVBNR        PIC X(07).
           05  DRVD0015-GHPABSCHLUSSVBNR-NI     PIC X(01).
           05  DRVD0015-GHPOFFNUNGSDATUM        PIC X(10).
           05  DRVD0015-GHPOFFNUNGSDATUM-NI     PIC X(01).
           05  DRVD0015-GHPSCHLIESSDATUM        PIC X(10).
           05  DRVD0015-GHPSCHLIESSDATUM-NI     PIC X(01).

           05  DRVD0015-GHA-Felder.
               10  DRVD0015-GHAGUTHABENANT      PIC S9(6)V9(4) COMP-3.
               10  DRVD0015-GHAGUTHABENANT-NI   PIC X(01).
               10  DRVD0015-GHAGUTHABEN         PIC S9(9)V9(2) COMP-3.
               10  DRVD0015-GHAGUTHABEN-NI      PIC X(01).
               10  DRVD0015-GHAFONDID           PIC S9(4) COMP.
               10  FILLER                       PIC X(54).

           05  DRVD0015-DBP-Felder REDEFINES DRVD0015-GHA-Felder.
               10  DRVD0015-DBPLFDNR            PIC 9(05).
               10  DRVD0015-DBPKARTENART        PIC X(03).
               10  DRVD0015-DBPDARLBETRAG       PIC S9(12)V9(2) COMP-3.
               10  DRVD0015-DBPSALDO            PIC S9(12)V9(2) COMP-3.
               10  FILLER                       PIC X(46).

      *----------- Ende DRVD0015 ---------------------------

