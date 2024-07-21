      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0207.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WDKI0207.CPv  $
      *
      *   Rev 2.1   Apr 26 2006 15:47:58   t13ex27
      *AKI00549: DB-Änderung für Fix 84
      *
      *   Rev 2.0   Jan 12 2006 14:28:16   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-NZMAGAZIN-GESAMT.
         10 EA-NI-NZMAGAZIN-TAB.
           20 EA-NI-NZMAGAZIN-ELE PIC X OCCURS 24 TIMES.
         10 EA-NI-NZMAGAZIN
                 REDEFINES EA-NI-NZMAGAZIN-TAB.
           20 EA-NI-NZMJAHR                  PIC X.
           20 EA-NI-NZMHEFTNUMMER            PIC X.
           20 EA-NI-NZMBESTELLERID           PIC X.
           20 EA-NI-NZMBESTELLERROLLE        PIC X.
           20 EA-NI-NZMKUNDENID              PIC X.
           20 EA-NI-NZMUZKZ                  PIC X.
           20 EA-NI-NZMLABELVORNAME          PIC X.
           20 EA-NI-NZMLABELNACHNAME         PIC X.
           20 EA-NI-NZMLABELSTRHAUSNUM       PIC X.
           20 EA-NI-NZMLABELPLZ              PIC X.
           20 EA-NI-NZMLABELORT              PIC X.
           20 EA-NI-NZMALTVORNAME            PIC X.
           20 EA-NI-NZMALTNACHNAME           PIC X.
           20 EA-NI-NZMALTSTRHAUSNUM         PIC X.
           20 EA-NI-NZMALTPLZ                PIC X.
           20 EA-NI-NZMALTORT                PIC X.
           20 EA-NI-NZMNEUVORNAME            PIC X.
           20 EA-NI-NZMNEUNACHNAME           PIC X.
           20 EA-NI-NZMNEUSTRHAUSNUM         PIC X.
           20 EA-NI-NZMNEUPLZ                PIC X.
           20 EA-NI-NZMNEUORT                PIC X.
           20 EA-NI-NZMKLASSIFIKATOREN       PIC X.
           20 EA-NI-NZMPROZENT               PIC X.
           20 EA-NI-NZMERLEDIGT              PIC X.
         10 EA-DCL-NZMAGAZIN.
           20 EA-NZMJAHR                     PIC -----9.
           20 EA-NZMHEFTNUMMER               PIC -----9.
           20 EA-NZMBESTELLERID              PIC X(13).
           20 EA-NZMBESTELLERROLLE           PIC X(1).
           20 EA-NZMKUNDENID                 PIC X(13).
           20 EA-NZMUZKZ                     PIC -----9.
           20 EA-NZMLABELVORNAME             PIC X(30).
           20 EA-NZMLABELNACHNAME            PIC X(30).
           20 EA-NZMLABELSTRHAUSNUM          PIC X(45).
           20 EA-NZMLABELPLZ                 PIC X(15).
           20 EA-NZMLABELORT                 PIC X(30).
           20 EA-NZMALTVORNAME               PIC X(30).
           20 EA-NZMALTNACHNAME              PIC X(30).
           20 EA-NZMALTSTRHAUSNUM            PIC X(45).
           20 EA-NZMALTPLZ                   PIC X(15).
           20 EA-NZMALTORT                   PIC X(30).
           20 EA-NZMNEUVORNAME               PIC X(30).
           20 EA-NZMNEUNACHNAME              PIC X(30).
           20 EA-NZMNEUSTRHAUSNUM            PIC X(45).
           20 EA-NZMNEUPLZ                   PIC X(15).
           20 EA-NZMNEUORT                   PIC X(30).
           20 EA-NZMKLASSIFIKATOREN          PIC X(16).
           20 EA-NZMPROZENT                  PIC X(24).
           20 EA-NZMERLEDIGT                 PIC X(1).
      *****************************************************************
