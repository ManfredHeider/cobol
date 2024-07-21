      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/07/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0464.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0464.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:49:24   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:33:44   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BTGZAHLERKREDITK.
           10 BZKVERTRAGSID                 PIC  X(0013).
           10 BZKGHPLFDNR                   PIC S9(4) COMP.
           10 BZKDBPLFDNR                   PIC S9(4) COMP.
           10 BZKLFDNR                      PIC S9(4) COMP.
           10 BZKKNDPERSONENID              PIC  X(0013).
           10 NIL-BZKKNDPERSONENID          PIC  X(01).
           10 BZKNACHNAME                   PIC  X(0030).
           10 NIL-BZKNACHNAME               PIC  X(01).
           10 BZKVORNAME                    PIC  X(0030).
           10 NIL-BZKVORNAME                PIC  X(01).
           10 BZKGEBDATUM                   PIC  X(010).
           10 NIL-BZKGEBDATUM               PIC  X(01).
           10 BZKADRESSEID                  PIC  X(0013).
           10 NIL-BZKADRESSEID              PIC  X(01).
           10 BZKANREDE-KTY                 PIC S9(4) COMP.
           10 NIL-BZKANREDE-KTY             PIC  X(01).
           10 BZKANREDE-KNR                 PIC S9(4) COMP.
           10 NIL-BZKANREDE-KNR             PIC  X(01).
           10 BZKIBAN                       PIC  X(0034).
           10 NIL-BZKIBAN                   PIC  X(01).
           10 BZKBIC                        PIC  X(0011).
           10 NIL-BZKBIC                    PIC  X(01).
           10 BZKABWBTGZAHLER               PIC  X(0001).
           10 NIL-BZKABWBTGZAHLER           PIC  X(01).
           10 BZKMANDATREFNR.
             49 BZKMANDATREFNR-LEN          PIC S9(4) COMP.
             49 BZKMANDATREFNR-TEXT         PIC X(35).
           10 NIL-BZKMANDATREFNR            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
