      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0451.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0451.CPv  $
      *
      *   Rev 2.4   Oct 22 2018 08:14:22   t13ae31
      *AKI01630: CKV Bordero neues Feld OZP
      *
      *   Rev 2.3   Nov 26 2012 15:28:36   T13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.2   Oct 19 2012 13:40:16   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:28:10   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 16:41:56   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0451.
           10 VPSVERTRAGSID                 PIC  X(0013).
           10 VPSPERSONENNR                 PIC S9(4) COMP.
           10 VPSPERSERHOEHPRZ              PIC S9(7)V9(2) COMP-3.
           10 NIL-VPSPERSERHOEHPRZ          PIC  X(01).
           10 VPSPERSRUECKSTLG              PIC S9(7)V9(2) COMP-3.
           10 NIL-VPSPERSRUECKSTLG          PIC  X(01).
           10 VPSPERSBEITRRUEK              PIC S9(7)V9(2) COMP-3.
           10 NIL-VPSPERSBEITRRUEK          PIC  X(01).
           10 VPSKUNDBINDPERS               PIC  X(0001).
           10 NIL-VPSKUNDBINDPERS           PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 VPSBEITRVORTEIL               PIC S9(7)V9(2) COMP-3.
           10 NIL-VPSBEITRVORTEIL           PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).

