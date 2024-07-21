      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0098.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0098.CPv  $
      *
      *   Rev 2.1   Dec 03 2003 09:30:42   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *   Rev 2.0   Oct 22 2003 10:51:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-NEUKUNDE.
           10 NKDPERSONENID               PIC X(13).
           10 NKDLFDNR                    PIC S9(4) COMP.
           10 NKDKUNDENNUMMER             PIC S9(9) COMP.
           10 NKDVBNR                     PIC S9(9) COMP.
           10 NKDDIREKTION                PIC S9(4) COMP.
           10 NKDPARTGESELLSCHID          PIC S9(4) COMP.
           10 NKDVERTRAGSNUMMER           PIC X(15).
           10 NKDTAGESDATUM               PIC X(010).
           10 NKDKNDSTATUSIDALT           PIC X(1).
           10 NKDKNDSTATUSIDNEU           PIC X(1).
           10 NKDBWTUNGSKZDVAG            PIC X(2).
           10 NKDBWTUNGSKZVB              PIC X(2).
           10 NIL-NKDBWTUNGSKZVB          PIC X(01).
           10 NKDKOMMISARISCHKZ           PIC X(1).
           10 NIL-NKDKOMMISARISCHKZ       PIC X(01).
           10 NKDPGMNAME                  PIC X(8).
           10 NKDPRODUKT                  PIC X(10).
           10 NIL-NKDPRODUKT              PIC X(01).
           10 NKDEREIGNISKZ               PIC S9(4) COMP.
           10 LETZTEAENDERUNG             PIC X(026).
           10 NKDKUZUPERSONENID           PIC X(13).
           10 NIL-NKDKUZUPERSONENID       PIC X(01).
      *****************************************************************
