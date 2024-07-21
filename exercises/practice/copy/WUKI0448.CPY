      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26/06/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0448.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0448.CPv  $
      *
      *   Rev 2.2   Sep 03 2012 10:16:24   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.1   Aug 15 2012 13:04:18   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.0   Jun 26 2012 11:14:42   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VBUNGUELTIGAV.
           10 VBUVERTRAGSNR                 PIC  X(0015).
           10 VBULFDNR                      PIC S9(4) COMP.
           10 VBUVBNRGELIEF                 PIC S9(9) COMP.
           10 NIL-VBUVBNRGELIEF             PIC  X(01).
           10 VBUVRBVBNR                    PIC S9(9) COMP.
           10 NIL-VBUVRBVBNR                PIC  X(01).
           10 VBUPARTGESID                  PIC S9(4) COMP.
           10 VBUMANDANTNR                  PIC S9(9) COMP.
           10 VBUABLEHNKZ                   PIC  X(0001).
           10 NIL-VBUABLEHNKZ               PIC  X(01).
           10 VBUSTRUKTURSTUFE              PIC S9(9) COMP.
           10 NIL-VBUSTRUKTURSTUFE          PIC  X(01).
           10 VBUAUSTRITTSDAT               PIC  X(010).
           10 NIL-VBUAUSTRITTSDAT           PIC  X(01).
           10 VBUAKTDAT                     PIC  X(010).
           10 VBUAUSWABLEHNKZ               PIC  X(0001).
           10 NIL-VBUAUSWABLEHNKZ           PIC  X(01).
           10 VBUAUSWSTRUKTSTUFE            PIC S9(9) COMP.
           10 NIL-VBUAUSWSTRUKTSTUFE        PIC  X(01).
           10 VBUAUSWAUSTRTTDAT             PIC  X(010).
           10 NIL-VBUAUSWAUSTRTTDAT         PIC  X(01).
           10 VBUAUSWDAT                    PIC  X(010).
           10 NIL-VBUAUSWDAT                PIC  X(01).
           10 VBUSTATUS                     PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
