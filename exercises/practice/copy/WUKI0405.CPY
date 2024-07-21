      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0405.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0405.CPv  $
      *
      *   Rev 2.3   Jun 28 2011 09:55:46   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.2   Sep 21 2009 14:33:42   t13ex27
      *AKI00876: DB-Änderungen für Rel. 4.62-FSAEIGENVERTRAEGE
      *
      *   Rev 2.1   Oct 12 2006 13:32:58   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE-PMKI0079
      *
      *   Rev 2.0   Apr 27 2006 16:01:04   t13ex27
      *Archiverstellung
      *
*     *---------------------------------------------------------------*
        05  WU-FSAEIGENVERTRAEGE.
           10 FSAVERTRAGSID                 PIC  X(0013).
           10 FSAPERSONENID                 PIC  X(0013).
           10 FSAPARTNERGESID               PIC S9(4) COMP.
           10 FSAGEMFREIBETRAG              PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAGEMFREIBETRAG          PIC  X(01).
           10 FSAFREIBETRAGVERBR            PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAFREIBETRAGVERBR        PIC  X(01).
           10 FSAWAEHRUNGSKZ                PIC  X(0001).
           10 FSAANZDEPFREISTBET            PIC S9(4) COMP.
           10 NIL-FSAANZDEPFREISTBET        PIC  X(01).
           10 FSAZINSABSCHSTEUER            PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAZINSABSCHSTEUER        PIC  X(01).
           10 FSAFREIBETRAGABL              PIC  X(010).
           10 NIL-FSAFREIBETRAGABL          PIC  X(01).
           10 FSASTUECKZINS                 PIC S9(9)V9(2) COMP-3.
           10 NIL-FSASTUECKZINS             PIC  X(01).
           10 FSAGUELTIGAB                  PIC  X(010).
           10 NIL-FSAGUELTIGAB              PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 FSAKZSTAUSL                   PIC  X(0001).
           10 NIL-FSAKZSTAUSL               PIC  X(01).
           10 FSANVGUELTIGBIS               PIC  X(010).
           10 NIL-FSANVGUELTIGBIS           PIC  X(01).
           10 FSAVERBRFREIVORJ              PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAVERBRFREIVORJ          PIC  X(01).
           10 FSASUMERTRAG                  PIC S9(9)V9(2) COMP-3.
           10 NIL-FSASUMERTRAG              PIC  X(01).
           10 FSASUMKEST                    PIC S9(9)V9(2) COMP-3.
           10 NIL-FSASUMKEST                PIC  X(01).
           10 FSASUMSOLI                    PIC S9(9)V9(2) COMP-3.
           10 NIL-FSASUMSOLI                PIC  X(01).
           10 FSASUMKIST                    PIC S9(9)V9(2) COMP-3.
           10 NIL-FSASUMKIST                PIC  X(01).
           10 FSAVERLUSTAKTIEN              PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAVERLUSTAKTIEN          PIC  X(01).
           10 FSAVERLUSTSONST               PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAVERLUSTSONST           PIC  X(01).
           10 FSAAUSLQSTEUER                PIC S9(9)V9(2) COMP-3.
           10 NIL-FSAAUSLQSTEUER            PIC  X(01).
           10 FSAKZVERBAKTIEN               PIC  X(0001).
           10 NIL-FSAKZVERBAKTIEN           PIC  X(01).
           10 FSAKZVERBSONST                PIC  X(0001).
           10 NIL-FSAKZVERBSONST            PIC  X(01).
           10 FSAKONFESSION1                PIC  X(0002).
           10 NIL-FSAKONFESSION1            PIC  X(01).
           10 FSAKONFESSION2                PIC  X(0002).
           10 NIL-FSAKONFESSION2            PIC  X(01).
           10 FSAKISTSATZ1                  PIC S9(3)V9(2) COMP-3.
           10 NIL-FSAKISTSATZ1              PIC  X(01).
           10 FSAKISTSATZ2                  PIC S9(3)V9(2) COMP-3.
           10 NIL-FSAKISTSATZ2              PIC  X(01).
           10 FSAKISTEINBEHALT              PIC  X(010).
           10 NIL-FSAKISTEINBEHALT          PIC  X(01).
           10 FSAAUFTKONF1                  PIC S9(3)V9(2) COMP-3.
           10 NIL-FSAAUFTKONF1              PIC  X(01).
           10 FSAAUFTKONF2                  PIC S9(3)V9(2) COMP-3.
           10 NIL-FSAAUFTKONF2              PIC  X(01).
           10 FSAFREISTAUFTRNR              PIC  X(0030).
           10 NIL-FSAFREISTAUFTRNR          PIC  X(01).
