      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0411.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0411.CPv  $
      *
      *   Rev 2.3   Nov 24 2010 10:33:30   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:51:48   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:59:28   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Aug 16 2010 16:25:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-UNFALLVERTRAG.
           10 UNFVERTRAGSID                 PIC  X(0013).
           10 UNFVETLFDNR                   PIC S9(4) COMP.
           10 UNFEINZVTGHOSTID              PIC  X(0015).
           10 UNFBEDINGUNG                  PIC  X(0016).
           10 UNFRISIKO                     PIC  X(0050).
           10 UNFKLAUSELTARIF               PIC  X(0080).
           10 UNFZUSATZINFO                 PIC  X(0100).
           10 UNFANZVERSPERSON              PIC S9(9) COMP.
           10 UNFLEISTFREIVDAT              PIC  X(010).
           10 NIL-UNFLEISTFREIVDAT          PIC  X(01).
           10 UNFLEISTFREIBDAT              PIC  X(010).
           10 NIL-UNFLEISTFREIBDAT          PIC  X(01).
           10 UNFSELBSTBKZ                  PIC  X(0002).
           10 UNFSELBSTBPROZ                PIC S9(2)V9(2) COMP-3.
           10 UNFSELBSTBFEST                PIC S9(9) COMP.
           10 UNFSELBSTBTEXT                PIC  X(0050).
           10 UNFMINDBEITRAGKZ              PIC  X(0002).
           10 UNFMAILINGAKTION              PIC  X(0070).
           10 UNFUMSTELLUNGDAT              PIC  X(010).
           10 NIL-UNFUMSTELLUNGDAT          PIC  X(01).
           10 UNFGESELLSCHAFT               PIC  X(0050).
           10 UNFVERSNUMMER                 PIC  X(0040).
           10 UNFKUENDIGUNGKZ               PIC  X(0002).
           10 LETZTEAENDERUNG               PIC  X(026).
