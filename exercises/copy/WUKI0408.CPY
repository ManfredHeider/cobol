      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0408.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0408.CPv  $
      *
      *   Rev 2.3   Nov 24 2010 10:33:04   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:51:32   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:59:12   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Aug 16 2010 16:24:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-GLASVERTRAG.
           10 GLVVERTRAGSID                 PIC  X(0013).
           10 GLVVETLFDNR                   PIC S9(4) COMP.
           10 GLVEINZVTGHOSTID              PIC  X(0015).
           10 GLVRANSCHRIFTPLZ              PIC  X(0006).
           10 GLVRANSCHRIFTSTR              PIC  X(0040).
           10 GLVRANSCHRIFTORT              PIC  X(0030).
           10 GLVRANSCHRHAUSNR              PIC  X(0015).
           10 GLVRANSCHRIFTLKZ              PIC S9(9) COMP.
           10 GLVKLAUSELTARIF               PIC  X(0080).
           10 GLVBEDINGUNG                  PIC  X(0016).
           10 GLVGEBAEUTYP-KTY              PIC S9(4) COMP.
           10 GLVGEBAEUTYP-KNR              PIC S9(15)V COMP-3.
           10 GLVRISIKO                     PIC  X(0050).
           10 GLVMINBEITRAGKZ               PIC  X(0002).
           10 GLVSBPROZENT                  PIC S9(2)V9(2) COMP-3.
           10 GLVSBFEST                     PIC S9(9) COMP.
           10 GLVZUSATZINFO                 PIC  X(0100).
           10 GLVLFREIVONDAT                PIC  X(010).
           10 NIL-GLVLFREIVONDAT            PIC  X(01).
           10 GLVLFREIBISDAT                PIC  X(010).
           10 NIL-GLVLFREIBISDAT            PIC  X(01).
           10 GLVMAILINGAKTION              PIC  X(0070).
           10 GLVDIFDECKKZ                  PIC  X(0002).
           10 GLVUMSTELLUNGDAT              PIC  X(010).
           10 NIL-GLVUMSTELLUNGDAT          PIC  X(01).
           10 GLVGESELLSCHAFT               PIC  X(0050).
           10 GLVVERSNUMMER                 PIC  X(0040).
           10 GLVKUENDIGUNGKZ               PIC  X(0002).
           10 LETZTEAENDERUNG               PIC  X(026).
