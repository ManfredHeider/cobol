      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0505.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0505.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:23:04   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:53:02   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 29 2015 07:50:10   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0505.
           10 KVPVERTRAGSID                 PIC  X(0013).
           10 KVPLFDNR                      PIC S9(4) COMP.
           10 KVPPRODUKTID                  PIC S9(4) COMP.
           10 KVPANREDE-KTY                 PIC S9(4) COMP.
           10 KVPANREDE-KNR                 PIC S9(4) COMP.
           10 KVPNACHNAME                   PIC  X(0030).
           10 NIL-KVPNACHNAME               PIC  X(01).
           10 KVPVORNAME                    PIC  X(0030).
           10 NIL-KVPVORNAME                PIC  X(01).
           10 KVPGEBDATUM                   PIC  X(010).
           10 NIL-KVPGEBDATUM               PIC  X(01).
           10 KVPBEGINNDATUM                PIC  X(010).
           10 NIL-KVPBEGINNDATUM            PIC  X(01).
           10 KVPENDEDATUM                  PIC  X(010).
           10 NIL-KVPENDEDATUM              PIC  X(01).
           10 KVPVORVENDDATUM               PIC  X(010).
           10 NIL-KVPVORVENDDATUM           PIC  X(01).
           10 KVPVORVGES-KTY                PIC S9(4) COMP.
           10 NIL-KVPVORVGES-KTY            PIC  X(01).
           10 KVPVORVGES-KNR                PIC S9(15)V COMP-3.
           10 NIL-KVPVORVGES-KNR            PIC  X(01).
           10 KVPVORVKUENDKZ                PIC  X(0001).
           10 NIL-KVPVORVKUENDKZ            PIC  X(01).
           10 KVPVERSART                    PIC  X(0001).
           10 NIL-KVPVERSART                PIC  X(01).
           10 KVPABRECHKZ                   PIC  X(0001).
           10 NIL-KVPABRECHKZ               PIC  X(01).
           10 KVPZUSRAHMEN                  PIC  X(0001).
           10 NIL-KVPZUSRAHMEN              PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).

