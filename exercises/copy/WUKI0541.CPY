      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20/05/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0541.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0541.CPv  $
      *
      *   Rev 2.2   Jul 07 2020 12:33:14   t13ex78
      *AKI01780: Standard OZP G90EVERTRAG
      *
      *   Rev 2.1   Jun 12 2020 11:44:44   t13ex78
      *AKI01780: Standard OZP G90EVERTRAG
      *
      *   Rev 2.0   May 20 2020 08:51:01   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0541.
           10 G90EVERTRAGSNR                PIC  X(0015).
           10 G90EMANDANT                   PIC S9(9) COMP.
           10 G90EZUSTAND                   PIC  X(0001).
           10 G90EABSCHLVBNR                PIC S9(9) COMP.
           10 G90EPRODUKTART                PIC S9(1)V COMP-3.
           10 G90EGEBNICHTANG               PIC S9(1)V9(2) COMP-3.
           10 G90EFACTGEBPROZ               PIC S9(1)V9(2) COMP-3.
           10 G90EFACTANKRAHMEN             PIC S9(10)V9(2) COMP-3.
           10 G90EVERTRAGBEGINN             PIC  X(0008).
           10 G90EVERTRAGENDE               PIC  X(0008).
           10 G90EEINRICHTGEB               PIC S9(8)V9(2) COMP-3.
           10 G90EIBAN                      PIC  X(0034).
           10 G90EINFOSPERREKZN             PIC  X(0001).
           10 G90EVONDATJAHR                PIC  X(0008).
           10 G90EBISDATJAHR                PIC  X(0008).
           10 G90ERECHNUNGJAHR              PIC S9(8)V COMP-3.
           10 G90EUMSATZJAHR                PIC S9(8)V9(2) COMP-3.
           10 G90EVONDATMONAT               PIC  X(0008).
           10 G90EBISDATMONAT               PIC  X(0008).
           10 G90ERECHNUNGMONAT             PIC S9(8)V COMP-3.
           10 G90EUMSATZMONAT               PIC S9(8)V9(2) COMP-3.
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).

