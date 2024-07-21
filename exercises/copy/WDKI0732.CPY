      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0732.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/WDKI0732.CPv  $
      *
      *   Rev 2.1   May 27 2003 11:39:28   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   May 14 2003 15:19:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELPUBLICPOOL-GESAMT.
         10 EA-NI-SELPUBLICPOOL-TAB.
           20 EA-NI-SELPUBLICPOOL-ELE PIC X OCCURS 6 TIMES.
         10 EA-NI-SELPUBLICPOOL
                 REDEFINES EA-NI-SELPUBLICPOOL-TAB.
           20 EA-NI-SPPID                    PIC X.
           20 EA-NI-SPPACTORNAME             PIC X.
           20 EA-NI-SPPNAME                  PIC X.
           20 EA-NI-SPPHOSTID                PIC X.
           20 EA-NI-SPPCREATEDATE            PIC X.
           20 EA-NI-SPPACTORNUM              PIC X.
         10 EA-DCL-SELPUBLICPOOL.
           20 EA-SPPID                       PIC X(13).
           20 EA-SPPACTORNAME                PIC X(60).
           20 EA-SPPNAME                     PIC X(60).
           20 EA-SPPHOSTID                   PIC ----------9.
           20 EA-SPPCREATEDATE               PIC X(010).
           20 EA-SPPACTORNUM                 PIC ----------9.
      *****************************************************************
