      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09/12/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0466.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0466.CPv  $
      *
      *   Rev 2.3   Nov 06 2015 12:27:38   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens   OZP ZTKI0466
      *
      *   Rev 2.2   Apr 16 2014 17:12:30   t13ex19
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 23 2014 15:36:14   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Dec 9 2013 16:32:50   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BANKKREDITKARTE.
           10 KKAVERTRAGSID                 PIC  X(0013).
           10 KKAPRODUKTLFDNR               PIC S9(4) COMP.
           10 KKALFDNR                      PIC S9(4) COMP.
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 KKAPRODUKTID                  PIC S9(4) COMP.
           10 KKAKARTENART                  PIC  X(0003).
           10 NIL-KKAKARTENART              PIC  X(01).
           10 KKAKARTENNR                   PIC  X(0016).
           10 NIL-KKAKARTENNR               PIC  X(01).
           10 KKAHAUPTKARTENNR              PIC  X(0016).
           10 NIL-KKAHAUPTKARTENNR          PIC  X(01).
           10 KKAVNRHAUPTKARTE              PIC  X(0015).
           10 NIL-KKAVNRHAUPTKARTE          PIC  X(01).
           10 KKAABRECHKTONR                PIC  X(0010).
           10 NIL-KKAABRECHKTONR            PIC  X(01).
           10 KKABLZABRECHKTO               PIC  X(0008).
           10 NIL-KKABLZABRECHKTO           PIC  X(01).
           10 KKAFILNRABRECHKTO             PIC S9(4) COMP.
           10 NIL-KKAFILNRABRECHKTO         PIC  X(01).
           10 KKAMAHNSTATUS                 PIC  X(0001).
           10 NIL-KKAMAHNSTATUS             PIC  X(01).
           10 KKAOEFFNUNGSDAT               PIC  X(010).
           10 NIL-KKAOEFFNUNGSDAT           PIC  X(01).
           10 KKAEIGENVERMKZ                PIC  X(0001).
           10 KKAABSCHLUSSVBNR              PIC S9(9) COMP.
           10 NIL-KKAABSCHLUSSVBNR          PIC  X(01).
           10 KKAPROVISIONKZ                PIC  X(0001).
           10 NIL-KKAPROVISIONKZ            PIC  X(01).

