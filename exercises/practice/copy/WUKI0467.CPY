      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/05/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0467.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0467.CPv  $
      *
      *   Rev 2.3   May 27 2014 13:06:18   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467 SWZ
      *
      *   Rev 2.2   May 19 2014 15:27:46   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.1   May 16 2014 11:26:32   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.0   May 14 2014 07:22:08   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-OIFKUENDIGUNG.
           10 OIFVERTRAGSID                 PIC  X(0013).
           10 OIFGUTHABPRODLFDNR            PIC S9(4) COMP.
           10 OIFFONDID                     PIC S9(4) COMP.
           10 OIFGHALFDNR                   PIC S9(4) COMP.
           10 OIFDATRUECKZAHLUNG            PIC  X(010).
           10 OIFANTEILE                    PIC S9(9)V9(4) COMP-3.
           10 NIL-OIFANTEILE                PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
