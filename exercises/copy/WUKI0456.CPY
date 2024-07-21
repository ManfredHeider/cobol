      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19/11/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0456.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0456.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:45:40   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Nov 19 2012 10:30:42   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0456.
           10 CKVTARIFKEY                   PIC S9(4) COMP.
           10 CKVTARIFBEZ                   PIC  X(0008).
           10 CKVTARIFNAME.
             49 CKVTARIFNAME-LEN            PIC S9(4) COMP.
             49 CKVTARIFNAME-TEXT           PIC X(30).
           10 CKVTARIFVERSION               PIC  X(0007).
           10 CKVTARIFART                   PIC  X(0001).
           10 CKVTARIFLFDNR                 PIC S9(4) COMP.
           10 CKVTARIFTEXT.
             49 CKVTARIFTEXT-LEN            PIC S9(4) COMP.
             49 CKVTARIFTEXT-TEXT           PIC X(500).
           10 LETZTEAENDERUNG               PIC  X(0026).

