      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0098.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0098.CPv  $
      *
      *   Rev 2.2   Dec 03 2003 09:29:38   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *   Rev 2.1   Oct 27 2003 16:20:24   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *   Rev 2.0   Oct 22 2003 10:51:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-NEUKUNDE-GESAMT.
         10 EA-NI-NEUKUNDE-TAB.
           20 EA-NI-NEUKUNDE-ELE PIC X OCCURS 18 TIMES.
         10 EA-NI-NEUKUNDE
                 REDEFINES EA-NI-NEUKUNDE-TAB.
           20 EA-NI-NKDPERSONENID            PIC X.
           20 EA-NI-NKDLFDNR                 PIC X.
           20 EA-NI-NKDKUNDENNUMMER          PIC X.
           20 EA-NI-NKDVBNR                  PIC X.
           20 EA-NI-NKDDIREKTION             PIC X.
           20 EA-NI-NKDPARTGESELLSCHID       PIC X.
           20 EA-NI-NKDVERTRAGSNUMMER        PIC X.
           20 EA-NI-NKDTAGESDATUM            PIC X.
           20 EA-NI-NKDKNDSTATUSIDALT        PIC X.
           20 EA-NI-NKDKNDSTATUSIDNEU        PIC X.
           20 EA-NI-NKDBWTUNGSKZDVAG         PIC X.
           20 EA-NI-NKDBWTUNGSKZVB           PIC X.
           20 EA-NI-NKDKOMMISARISCHKZ        PIC X.
           20 EA-NI-NKDPGMNAME               PIC X.
           20 EA-NI-NKDPRODUKT               PIC X.
           20 EA-NI-NKDEREIGNISKZ            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-NKDKUZUPERSONENID        PIC X.
         10 EA-DCL-NEUKUNDE.
           20 EA-NKDPERSONENID               PIC X(13).
           20 EA-NKDLFDNR                    PIC -----9.
           20 EA-NKDKUNDENNUMMER             PIC ----------9.
           20 EA-NKDVBNR                     PIC ----------9.
           20 EA-NKDDIREKTION                PIC -----9.
           20 EA-NKDPARTGESELLSCHID          PIC -----9.
           20 EA-NKDVERTRAGSNUMMER           PIC X(15).
           20 EA-NKDTAGESDATUM               PIC X(010).
           20 EA-NKDKNDSTATUSIDALT           PIC X(1).
           20 EA-NKDKNDSTATUSIDNEU           PIC X(1).
           20 EA-NKDBWTUNGSKZDVAG            PIC X(2).
           20 EA-NKDBWTUNGSKZVB              PIC X(2).
           20 EA-NKDKOMMISARISCHKZ           PIC X(1).
           20 EA-NKDPGMNAME                  PIC X(8).
           20 EA-NKDPRODUKT                  PIC X(10).
           20 EA-NKDEREIGNISKZ               PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-NKDKUZUPERSONENID           PIC X(13).
      *****************************************************************
