      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  13.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0395.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WDKI0395.CPv  $
      *
      *   Rev 2.4   Oct 12 2006 12:17:40   t13ex27
      *AKI00624: DB-Änderung für Fix 93 EA-Programme
      *
      *   Rev 2.3   Jul 24 2006 09:32:38   t13ex27
      *AKI00604: DB-Änderung für Fix 90 - EA-Programme
      *
      *   Rev 2.2   May 27 2003 11:39:24   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.1   Apr 07 2003 10:12:48   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.0   Mar 13 2003 08:25:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-DBPRODUKT-GESAMT.
         10 EA-NI-DBPRODUKT-TAB.
           20 EA-NI-DBPRODUKT-ELE PIC X OCCURS 30 TIMES.
         10 EA-NI-DBPRODUKT
                 REDEFINES EA-NI-DBPRODUKT-TAB.
           20 EA-NI-DBPVERTRAGSID            PIC X.
           20 EA-NI-DBPGUTHABPRODLFDNR       PIC X.
           20 EA-NI-DBPLFDNR                 PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-DBPWAEHRSCHL             PIC X.
           20 EA-NI-DBPBILANZSCHL            PIC X.
           20 EA-NI-DBPEXTLIMIT              PIC X.
           20 EA-NI-DBPEXTLIMITWKZ           PIC X.
           20 EA-NI-DBPSALDO                 PIC X.
           20 EA-NI-DBPSALDOWKZ              PIC X.
           20 EA-NI-DBPKARTENART             PIC X.
           20 EA-NI-DBANZKARTEN              PIC X.
           20 EA-NI-DBPKREDITKARTENNR        PIC X.
           20 EA-NI-DBPHAUPTKARTENNR         PIC X.
           20 EA-NI-DBPVERTRHAUPTKART        PIC X.
           20 EA-NI-DBPABRECHKONTO           PIC X.
           20 EA-NI-DBPBLZABRECHKONTO        PIC X.
           20 EA-NI-DBPFILIALNR              PIC X.
           20 EA-NI-DBPKONTOART              PIC X.
           20 EA-NI-DBPVERTRAGSENDE          PIC X.
           20 EA-NI-DBPDARLBETRAG            PIC X.
           20 EA-NI-DBPDARLBETRAGWKZ         PIC X.
           20 EA-NI-DBPBLZKUNDEN             PIC X.
           20 EA-NI-DBPKONDSCHL              PIC X.
           20 EA-NI-DBPSOLLZINS              PIC X.
           20 EA-NI-DBPUEBERZZINS            PIC X.
           20 EA-NI-DBPHABENZINS             PIC X.
           20 EA-NI-DBPMAHNSTATUS            PIC X.
           20 EA-NI-DBPZAHLUNGSWEISEID       PIC X.
           20 EA-NI-DBPRATENHOEHE            PIC X.
         10 EA-DCL-DBPRODUKT.
           20 EA-DBPVERTRAGSID               PIC X(13).
           20 EA-DBPGUTHABPRODLFDNR          PIC -----9.
           20 EA-DBPLFDNR                    PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-DBPWAEHRSCHL                PIC -----9.
           20 EA-DBPBILANZSCHL               PIC -----9.
           20 EA-DBPEXTLIMIT                 PIC ------------9.99.
           20 EA-DBPEXTLIMITWKZ              PIC X(1).
           20 EA-DBPSALDO                    PIC ------------9.99.
           20 EA-DBPSALDOWKZ                 PIC X(1).
           20 EA-DBPKARTENART                PIC X(3).
           20 EA-DBANZKARTEN                 PIC -----9.
           20 EA-DBPKREDITKARTENNR           PIC X(16).
           20 EA-DBPHAUPTKARTENNR            PIC X(16).
           20 EA-DBPVERTRHAUPTKART           PIC X(15).
           20 EA-DBPABRECHKONTO              PIC X(10).
           20 EA-DBPBLZABRECHKONTO           PIC X(8).
           20 EA-DBPFILIALNR                 PIC -----9.
           20 EA-DBPKONTOART                 PIC X(3).
           20 EA-DBPVERTRAGSENDE             PIC X(010).
           20 EA-DBPDARLBETRAG               PIC ------------9.99.
           20 EA-DBPDARLBETRAGWKZ            PIC X(1).
           20 EA-DBPBLZKUNDEN                PIC X(8).
           20 EA-DBPKONDSCHL                 PIC X(2).
           20 EA-DBPSOLLZINS                 PIC --9.9999.
           20 EA-DBPUEBERZZINS               PIC --9.9999.
           20 EA-DBPHABENZINS                PIC --9.9999.
           20 EA-DBPMAHNSTATUS               PIC X(1).
           20 EA-DBPZAHLUNGSWEISEID          PIC -----9.
           20 EA-DBPRATENHOEHE               PIC ------------9.99.
      *****************************************************************
