      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0459.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0459.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:48:04   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 14:00:34   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:55:24   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BTGZAHLERSPARPLAN.
           10 BZSVERTRAGSID                 PIC  X(0013).
           10 BZSGHPLFDNR                   PIC S9(4) COMP.
           10 BZSGHZFONDSID                 PIC S9(4) COMP.
           10 BZSGHZLFDNR                   PIC S9(4) COMP.
           10 BZSLFDNR                      PIC S9(4) COMP.
           10 BZSKNDPERSONENID              PIC  X(0013).
           10 NIL-BZSKNDPERSONENID          PIC  X(01).
           10 BZSNACHNAME                   PIC  X(0030).
           10 NIL-BZSNACHNAME               PIC  X(01).
           10 BZSVORNAME                    PIC  X(0030).
           10 NIL-BZSVORNAME                PIC  X(01).
           10 BZSGEBDATUM                   PIC  X(010).
           10 NIL-BZSGEBDATUM               PIC  X(01).
           10 BZSADRESSEID                  PIC  X(0013).
           10 NIL-BZSADRESSEID              PIC  X(01).
           10 BZSANREDE-KTY                 PIC S9(4) COMP.
           10 NIL-BZSANREDE-KTY             PIC  X(01).
           10 BZSANREDE-KNR                 PIC S9(4) COMP.
           10 NIL-BZSANREDE-KNR             PIC  X(01).
           10 BZSIBAN                       PIC  X(0034).
           10 NIL-BZSIBAN                   PIC  X(01).
           10 BZSBIC                        PIC  X(0011).
           10 NIL-BZSBIC                    PIC  X(01).
           10 BZSABWBTGZAHLER               PIC  X(0001).
           10 NIL-BZSABWBTGZAHLER           PIC  X(01).
           10 BZSMANDATREFNR.
             49 BZSMANDATREFNR-LEN          PIC S9(4) COMP.
             49 BZSMANDATREFNR-TEXT         PIC X(35).
           10 NIL-BZSMANDATREFNR            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
