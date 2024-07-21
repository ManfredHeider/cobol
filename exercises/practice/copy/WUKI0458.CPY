      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/02/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0458.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0458.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:47:58   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 14:00:26   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs f}r SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:45:20   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BTGZAHLERDEPOT.
           10 BZDVERTRAGSID                 PIC  X(0013).
           10 BZDGHPLFDNR                   PIC S9(4) COMP.
           10 BZDLFDNR                      PIC S9(4) COMP.
           10 BZDKNDPERSONENID              PIC  X(0013).
           10 NIL-BZDKNDPERSONENID          PIC  X(01).
           10 BZDNACHNAME                   PIC  X(0030).
           10 NIL-BZDNACHNAME               PIC  X(01).
           10 BZDVORNAME                    PIC  X(0030).
           10 NIL-BZDVORNAME                PIC  X(01).
           10 BZDGEBDATUM                   PIC  X(010).
           10 NIL-BZDGEBDATUM               PIC  X(01).
           10 BZDADRESSEID                  PIC  X(0013).
           10 NIL-BZDADRESSEID              PIC  X(01).
           10 BZDANREDE-KTY                 PIC S9(4) COMP.
           10 NIL-BZDANREDE-KTY             PIC  X(01).
           10 BZDANREDE-KNR                 PIC S9(4) COMP.
           10 NIL-BZDANREDE-KNR             PIC  X(01).
           10 BZDIBAN                       PIC  X(0034).
           10 NIL-BZDIBAN                   PIC  X(01).
           10 BZDBIC                        PIC  X(0011).
           10 NIL-BZDBIC                    PIC  X(01).
           10 BZDABWBTGZAHLER               PIC  X(0001).
           10 NIL-BZDABWBTGZAHLER           PIC  X(01).
           10 BZDMANDATREFNR.
             49 BZDMANDATREFNR-LEN          PIC S9(4) COMP.
             49 BZDMANDATREFNR-TEXT         PIC X(35).
           10 NIL-BZDMANDATREFNR            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
