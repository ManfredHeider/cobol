      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05/08/16
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0424.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0424.CPv  $
      *
      *   Rev 2.2   Sep 07 2017 07:40:10   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. OZP
      *
      *   Rev 2.1   Sep 19 2016 07:33:50   t13ae31
      *AKI01458: SUH Namensvorwort bei Versicherten Personen OZP
      *
      *   Rev 2.0   Aug 5 2016 10:24:30   T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0424.
           10 UVPVERTRAGSID                 PIC  X(0013).
           10 UVPVETLFDNR                   PIC S9(4) COMP.
           10 UVPPUFVPLFDNR                 PIC S9(4) COMP.
           10 UVPEINZVTGHOSTID              PIC  X(0015).
           10 UVPVERSPERSONLFD              PIC S9(4) COMP.
           10 UVPVPNAME                     PIC  X(0025).
           10 UVPVPBERUF                    PIC  X(0021).
           10 UVPVPGEBURTSDAT               PIC  X(010).
           10 NIL-UVPVPGEBURTSDAT           PIC  X(01).
           10 UVPVPGEFAHRENGRP              PIC  X(0001).
           10 UVPVPBEITRAG                  PIC S9(7)V9(2) COMP-3.
           10 UVPTARIFART                   PIC  X(0030).
           10 UVPDYNAMIKTEXT                PIC  X(0005).
           10 UVPDYNAMIKPROZ                PIC S9(2)V9(2) COMP-3.
           10 UVPDYNABLJAHR                 PIC S9(8)V COMP-3.
           10 UVPREGULIERUNGKZ              PIC  X(0002).
           10 UVPRUECKGEWSUMKZ              PIC  X(0002).
           10 UVPPRRUECKGEWKZ               PIC  X(0002).
           10 UVPFREIZDECKKZ                PIC  X(0002).
           10 UVPANZVERPERGRP               PIC S9(9) COMP.
           10 UVPVPGESCHL-KTY               PIC S9(4) COMP.
           10 UVPVPGESCHL-KNR               PIC S9(15)V COMP-3.
           10 UVPPRFREISTKIZKZ              PIC  X(0002).
           10 UVPPRAEMICHEKIZ               PIC S9(7)V9(2) COMP-3.
           10 UVPVPPARTNHOSTID              PIC  X(0017).
           10 UVPDIFFDECKKZ                 PIC  X(0002).
           10 UVPZIELGRUPPE                 PIC  X(0030).
           10 UVPGESELLSCHAFT               PIC  X(0050).
           10 UVPVERSNUMMER                 PIC  X(0040).
           10 UVPUMSTELLUNGDAT              PIC  X(010).
           10 NIL-UVPUMSTELLUNGDAT          PIC  X(01).
           10 UVPKUENDIGUNGKZ               PIC  X(0002).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 UVPNACHNAME                   PIC  X(0040).
           10 NIL-UVPNACHNAME               PIC  X(01).
           10 UVPVORNAME                    PIC  X(0030).
           10 NIL-UVPVORNAME                PIC  X(01).
           10 UVPBERUF-KTY                  PIC S9(4) COMP.
           10 NIL-UVPBERUF-KTY              PIC  X(01).
           10 UVPBERUF-KNR                  PIC S9(15)V COMP-3.
           10 NIL-UVPBERUF-KNR              PIC  X(01).

