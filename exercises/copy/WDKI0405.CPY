      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0405.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WDKI0405.CPv  $
      *
      *   Rev 2.1   Oct 12 2006 11:57:20   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE EA-Programme
      *
      *   Rev 2.0   Apr 27 2006 15:58:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-FSAEIGENVERTRAEGE-GESAMT.
         10 EA-NI-FSAEIGENVERTRAEGE-TAB.
           20 EA-NI-FSAEIGENVERTRAEGE-ELE PIC X OCCURS 12 TIMES.
         10 EA-NI-FSAEIGENVERTRAEGE
                 REDEFINES EA-NI-FSAEIGENVERTRAEGE-TAB.
           20 EA-NI-FSAVERTRAGSID            PIC X.
           20 EA-NI-FSAPERSONENID            PIC X.
           20 EA-NI-FSAPARTNERGESID          PIC X.
           20 EA-NI-FSAGEMFREIBETRAG         PIC X.
           20 EA-NI-FSAFREIBETRAGVERBR       PIC X.
           20 EA-NI-FSAWAEHRUNGSKZ           PIC X.
           20 EA-NI-FSAANZDEPFREISTBET       PIC X.
           20 EA-NI-FSAZINSABSCHSTEUER       PIC X.
           20 EA-NI-FSAFREIBETRAGABL         PIC X.
           20 EA-NI-FSASTUECKZINS            PIC X.
           20 EA-NI-FSAGUELTIGAB             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FSAEIGENVERTRAEGE.
           20 EA-FSAVERTRAGSID               PIC X(13).
           20 EA-FSAPERSONENID               PIC X(13).
           20 EA-FSAPARTNERGESID             PIC -----9.
           20 EA-FSAGEMFREIBETRAG            PIC ---------9.99.
           20 EA-FSAFREIBETRAGVERBR          PIC ---------9.99.
           20 EA-FSAWAEHRUNGSKZ              PIC X(1).
           20 EA-FSAANZDEPFREISTBET          PIC -----9.
           20 EA-FSAZINSABSCHSTEUER          PIC ---------9.99.
           20 EA-FSAFREIBETRAGABL            PIC X(010).
           20 EA-FSASTUECKZINS               PIC ---------9.99.
           20 EA-FSAGUELTIGAB                PIC X(010).
           20 EA-LETZTEAENDERUNG             PIC X(026).
      *****************************************************************
