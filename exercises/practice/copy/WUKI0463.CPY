      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/07/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0463.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0463.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:49:28   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:29:16   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BTGZAHLERBANKINGP.
           10 BZBDBPVERTRAGSID              PIC  X(0013).
           10 BZBDBPGHPLFDNR                PIC S9(4) COMP.
           10 BZBDBPLFDNR                   PIC S9(4) COMP.
           10 BZBLFDNR                      PIC S9(4) COMP.
           10 BZBKNDPERSONENID              PIC  X(0013).
           10 NIL-BZBKNDPERSONENID          PIC  X(01).
           10 BZBNACHNAME                   PIC  X(0030).
           10 NIL-BZBNACHNAME               PIC  X(01).
           10 BZBVORNAME                    PIC  X(0030).
           10 NIL-BZBVORNAME                PIC  X(01).
           10 BZBGEBDATUM                   PIC  X(010).
           10 NIL-BZBGEBDATUM               PIC  X(01).
           10 BZBADRESSEID                  PIC  X(0013).
           10 NIL-BZBADRESSEID              PIC  X(01).
           10 BZBANREDE-KTY                 PIC S9(4) COMP.
           10 NIL-BZBANREDE-KTY             PIC  X(01).
           10 BZBANREDE-KNR                 PIC S9(4) COMP.
           10 NIL-BZBANREDE-KNR             PIC  X(01).
           10 BZBIBAN                       PIC  X(0034).
           10 NIL-BZBIBAN                   PIC  X(01).
           10 BZBBIC                        PIC  X(0011).
           10 NIL-BZBBIC                    PIC  X(01).
           10 BZBABWBTGZAHLER               PIC  X(0001).
           10 NIL-BZBABWBTGZAHLER           PIC  X(01).
           10 BZBMANDATREFNR.
             49 BZBMANDATREFNR-LEN          PIC S9(4) COMP.
             49 BZBMANDATREFNR-TEXT         PIC X(35).
           10 NIL-BZBMANDATREFNR            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
