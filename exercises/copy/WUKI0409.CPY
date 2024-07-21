      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0409.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0409.CPv  $
      *
      *   Rev 2.3   Nov 24 2010 10:33:12   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:51:36   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:59:16   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Aug 16 2010 16:24:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-HAFTPFLICHTVERT.
           10 HPVVERTRAGSID                 PIC  X(0013).
           10 HPVVETLFDNR                   PIC S9(4) COMP.
           10 HPVEINZVTGHOSTID              PIC  X(0015).
           10 HPVBEDINGUNG                  PIC  X(0016).
           10 HPVRISIKO                     PIC  X(0050).
           10 HPVKLAUSELTARIF               PIC  X(0080).
           10 HPVZUSATZINFO                 PIC  X(0100).
           10 HPVRANSCHRIFTPLZ              PIC  X(0006).
           10 HPVRANSCHRIFTSTR              PIC  X(0040).
           10 HPVRANSCHRIFTORT              PIC  X(0030).
           10 HPVRANSCHRIFTLKZ              PIC S9(9) COMP.
           10 HPVMITVERSPERSON              PIC  X(0035).
           10 HPVRANSCHRHAUSNR              PIC  X(0015).
           10 HPVREGULIERUNGKZ              PIC  X(0002).
           10 HPVMINDBEITRKZ                PIC  X(0002).
           10 HPVLFREIVONDAT                PIC  X(010).
           10 NIL-HPVLFREIVONDAT            PIC  X(01).
           10 HPVLFREIBISDAT                PIC  X(010).
           10 NIL-HPVLFREIBISDAT            PIC  X(01).
           10 HPVSELBSTBKZ                  PIC  X(0002).
           10 HPVSELBSTBPROZ                PIC S9(2)V9(2) COMP-3.
           10 HPVSELBSTBFEST                PIC S9(9) COMP.
           10 HPVSELBSTBTEXT                PIC  X(0050).
           10 HPVAUSLSCHADKZ                PIC  X(0002).
           10 HPVVERWSCHADKZ                PIC  X(0002).
           10 HPVPRODUKT                    PIC  X(0030).
           10 HPVNAMELTERNVJKI              PIC  X(0045).
           10 HPVNAMVJKINDERN               PIC  X(0045).
           10 HPVABWEICHENDEVP              PIC  X(0035).
           10 HPVMAILINGAKTION              PIC  X(0070).
           10 HPVDIFFDECKKZ                 PIC  X(0002).
           10 HPVUMSTELLUNGDAT              PIC  X(010).
           10 NIL-HPVUMSTELLUNGDAT          PIC  X(01).
           10 HPVGESELLSCHAFT               PIC  X(0050).
           10 HPVVERSNUMMER                 PIC  X(0040).
           10 HPVKUENDIGUNGKZ               PIC  X(0002).
           10 HPVZIELGRUPPE                 PIC  X(0030).
           10 LETZTEAENDERUNG               PIC  X(026).
