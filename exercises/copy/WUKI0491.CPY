      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22/02/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0491.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0491.CPv  $
      *
      *   Rev 2.2   Dec 15 2020 15:19:16   t13ae31
      *AKI01812: G21 Leben Basis Viridium DB2
      *
      *   Rev 2.1   Apr 13 2018 10:14:58   t13ex19
      *AKI01570: FW - Generali-Partnergesellschaften - Basisdaten-V
      *ertrag
      *
      *   Rev 2.0   Feb 22 2018 15:42:02   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0491.
           10 GENVGESELLSCHID               PIC  X(0002).
           10 GENVVNR                       PIC  X(0015).
           10 GENVEZPNR                     PIC S9(4) COMP.
           10 GENVVERARB-KZN                PIC  X(0001).
           10 GENVAENDKZ                    PIC  X(0001).
           10 GENVVBNR                      PIC S9(9) COMP.
           10 GENVBEGINN                    PIC  X(0008).
           10 GENVABLAUF                    PIC  X(0008).
           10 GENVZUSTAND                   PIC S9(4) COMP.
           10 GENVBEITRAG                   PIC S9(8)V9(2) COMP-3.
           10 GENVNETTOBEITRAG              PIC S9(8)V9(2) COMP-3.
           10 GENVZAHLWEISE                 PIC  X(0002).
           10 GENVSPARTE                    PIC  X(0003).
           10 GENVPRODUKT                   PIC  X(0050).
           10 GENVTARIF                     PIC  X(0050).
           10 GENVSUMME                     PIC S9(8)V9(2) COMP-3.
           10 GENVAMTLKENNZ                 PIC  X(0012).
           10 GENVWAGNISKZ                  PIC S9(4) COMP.
           10 GENVGUTHABEN                  PIC S9(8)V9(2) COMP-3.
           10 GENVAKTWERT                   PIC S9(8)V9(2) COMP-3.
           10 GENVRENTENBEZUG               PIC  X(0001).
           10 GENVVORRENTE                  PIC S9(6)V COMP-3.
           10 GENVGARRENTE                  PIC S9(6)V COMP-3.
           10 GENVSTAND                     PIC  X(0008).
           10 GENVGZWEIG                    PIC  X(0002).
           10 GENVFLAECHE                   PIC S9(4) COMP.
           10 GENVPRNUMMER                  PIC S9(9) COMP.
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 GENVMIGKZN                    PIC  X(0001).

