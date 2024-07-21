      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0455.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0455.CPv  $
      *
      *   Rev 2.3   Jun 13 2013 13:07:06   t13ex27
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.1   Feb 27 2013 14:00:20   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:27:44   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BTGZAHLERVTG.
           10 BZVVERTRAGSID                 PIC  X(0013).
           10 BZVLFDNR                      PIC S9(4) COMP.
           10 BZVKNDPERSONENID              PIC  X(0013).
           10 NIL-BZVKNDPERSONENID          PIC  X(01).
           10 BZVNACHNAME                   PIC  X(0030).
           10 NIL-BZVNACHNAME               PIC  X(01).
           10 BZVVORNAME                    PIC  X(0030).
           10 NIL-BZVVORNAME                PIC  X(01).
           10 BZVGEBDATUM                   PIC  X(010).
           10 NIL-BZVGEBDATUM               PIC  X(01).
           10 BZVADRESSEID                  PIC  X(0013).
           10 NIL-BZVADRESSEID              PIC  X(01).
           10 BZVANREDE-KTY                 PIC S9(4) COMP.
           10 NIL-BZVANREDE-KTY             PIC  X(01).
           10 BZVANREDE-KNR                 PIC S9(4) COMP.
           10 NIL-BZVANREDE-KNR             PIC  X(01).
           10 BZVIBAN                       PIC  X(0034).
           10 NIL-BZVIBAN                   PIC  X(01).
           10 BZVBIC                        PIC  X(0011).
           10 NIL-BZVBIC                    PIC  X(01).
           10 BZVABWBTGZAHLER               PIC  X(0001).
           10 BZVMANDATREFNR.
             49 BZVMANDATREFNR-LEN          PIC S9(4) COMP.
             49 BZVMANDATREFNR-TEXT         PIC X(35).
           10 NIL-BZVMANDATREFNR            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
