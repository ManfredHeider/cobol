      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0410.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0410.CPv  $
      *
      *   Rev 2.4   Sep 19 2016 07:41:50   t13ae31
      *AKI01460: SUH SMART-HOME OZP TCKI0410 Hausrat
      *
      *   Rev 2.3   Nov 24 2010 10:33:22   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:51:42   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:59:22   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Aug 16 2010 16:24:54   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-HAUSRATVERTRAG.
           10 HRVVERTRAGSID                 PIC  X(0013).
           10 HRVVETLFDNR                   PIC S9(4) COMP.
           10 HRVEINZVTGHOSTID              PIC  X(0015).
           10 HRVRANSCHRIFTPLZ              PIC  X(0006).
           10 HRVRANSCHRIFTSTR              PIC  X(0040).
           10 HRVRANSCHRIFTORT              PIC  X(0030).
           10 HRVRANSCHRIFTLKZ              PIC S9(9) COMP.
           10 HRVBEDINGUNG                  PIC  X(0016).
           10 HRVRANSCHRHAUSNR              PIC  X(0015).
           10 HRVKLAUSELTARIF               PIC  X(0080).
           10 HRVZUSATZINFO                 PIC  X(0100).
           10 HRVLEIFREIVONDAT              PIC  X(010).
           10 NIL-HRVLEIFREIVONDAT          PIC  X(01).
           10 HRVLEIFREIBISDAT              PIC  X(010).
           10 NIL-HRVLEIFREIBISDAT          PIC  X(01).
           10 HRVSELBSTBKZ                  PIC  X(0002).
           10 HRVSELBSTBPROZ                PIC S9(2)V9(2) COMP-3.
           10 HRVSELBSTBFEST                PIC S9(9) COMP.
           10 HRVSELBSTBTEXT                PIC  X(0050).
           10 HRVNUTZUNG-KTY                PIC S9(4) COMP.
           10 HRVNUTZUNG-KNR                PIC S9(15)V COMP-3.
           10 HRVVERSSUMME                  PIC S9(9) COMP.
           10 HRVWOHNFLAECHE                PIC S9(9) COMP.
           10 HRVUNTVERSVERZKZ              PIC  X(0002).
           10 HRVGEBAEUTYP-KTY              PIC S9(4) COMP.
           10 HRVGEBAEUTYP-KNR              PIC S9(15)V COMP-3.
           10 HRVEIVERHAEL-KTY              PIC S9(4) COMP.
           10 HRVEIVERHAEL-KNR              PIC S9(15)V COMP-3.
           10 HRVTARIFZONE                  PIC  X(0004).
           10 HRVBAUARTKL-KTY               PIC S9(4) COMP.
           10 HRVBAUARTKL-KNR               PIC S9(15)V COMP-3.
           10 HRVALARMANLAGEKZ              PIC  X(0002).
           10 HRVFEUERKZ                    PIC  X(0002).
           10 HRVSTURMKZ                    PIC  X(0002).
           10 HRVHAGELKZ                    PIC  X(0002).
           10 HRVEINBRDIEBSTKZ              PIC  X(0002).
           10 HRVGLASKZ                     PIC  X(0002).
           10 HRVLEITUNGSWASKZ              PIC  X(0002).
           10 HRVELEMSCHKZ                  PIC  X(0002).
           10 HRVBEZUGSPRAEMIE              PIC S9(3)V9(3) COMP-3.
           10 HRVLEISTUNGSTEXT              PIC  X(0040).
           10 HRVSUMANPKLAUSEL              PIC  X(0032).
           10 HRVPRODUKT                    PIC  X(0030).
           10 HRVMAILINGAKTION              PIC  X(0070).
           10 HRVDIFFDECKKZ                 PIC  X(0002).
           10 HRVUMSTELLUNGDAT              PIC  X(010).
           10 NIL-HRVUMSTELLUNGDAT          PIC  X(01).
           10 HRVGESELLSCHAFT               PIC  X(0050).
           10 HRVVERSNUMMER                 PIC  X(0040).
           10 HRVKUENDIGUNGKZ               PIC  X(0002).
           10 HRVZIELGRUPPE                 PIC  X(0030).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 HRVSMHPAKET                   PIC  X(0002).
           10 NIL-HRVSMHPAKET               PIC  X(01).
           10 HRVSMHDATUM                   PIC  X(010).
           10 NIL-HRVSMHDATUM               PIC  X(01).
           10 HRVSMHTRACKID                 PIC  X(0060).
           10 NIL-HRVSMHTRACKID             PIC  X(01).
           10 HRVSMHCONTROLZ                PIC  X(0060).
           10 NIL-HRVSMHCONTROLZ            PIC  X(01).

