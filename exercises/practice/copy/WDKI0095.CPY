      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0095.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0095.CPv  $
      *
      *   Rev 2.2   Oct 18 2004 09:43:08   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.1   Oct 15 2002 08:15:44   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.0   Sep 30 2002 12:41:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-KKVORSCHLAG-GESAMT.
         10 EA-NI-KKVORSCHLAG-TAB.
           20 EA-NI-KKVORSCHLAG-ELE PIC X OCCURS 19 TIMES.
         10 EA-NI-KKVORSCHLAG
                 REDEFINES EA-NI-KKVORSCHLAG-TAB.
           20 EA-NI-KKVVLID                  PIC X.
           20 EA-NI-KKVLFDNR                 PIC X.
           20 EA-NI-KKVEMPFVBNR              PIC X.
           20 EA-NI-KKVKDNR                  PIC X.
           20 EA-NI-KKVKDANREDE              PIC X.
           20 EA-NI-KKVKDGEBDAT              PIC X.
           20 EA-NI-KKVKDPLZ                 PIC X.
           20 EA-NI-KKVKDORT                 PIC X.
           20 EA-NI-KKVKDSTRASSE             PIC X.
           20 EA-NI-KKVVBALTVBNR             PIC X.
           20 EA-NI-KKVVBALTAUSDAT           PIC X.
           20 EA-NI-KKVKZMFB                 PIC X.
           20 EA-NI-KKVAKZVBID               PIC X.
           20 EA-NI-KKAKZVBROLLE             PIC X.
           20 EA-NI-KKVVSTATUS               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-KKVKDNAME                PIC X.
           20 EA-NI-KKVVBALTNAME             PIC X.
           20 EA-NI-KKVABLEHNUNGSGRUND       PIC X.
         10 EA-DCL-KKVORSCHLAG.
           20 EA-KKVVLID                     PIC X(13).
           20 EA-KKVLFDNR                    PIC -----9.
           20 EA-KKVEMPFVBNR                 PIC ----------9.
           20 EA-KKVKDNR                     PIC ----------9.
           20 EA-KKVKDANREDE                 PIC -----9.
           20 EA-KKVKDGEBDAT                 PIC X(010).
           20 EA-KKVKDPLZ                    PIC X(15).
           20 EA-KKVKDORT                    PIC X(30).
           20 EA-KKVKDSTRASSE                PIC X(45).
           20 EA-KKVVBALTVBNR                PIC ----------9.
           20 EA-KKVVBALTAUSDAT              PIC X(010).
           20 EA-KKVKZMFB                    PIC X(1).
           20 EA-KKVAKZVBID                  PIC X(13).
           20 EA-KKAKZVBROLLE                PIC X(1).
           20 EA-KKVVSTATUS                  PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-KKVKDNAME.
              49 EA-KKVKDNAME-LEN            PIC -----9.
              49 EA-KKVKDNAME-TEXT           PIC X(61).
           20 EA-KKVVBALTNAME.
              49 EA-KKVVBALTNAME-LEN         PIC -----9.
              49 EA-KKVVBALTNAME-TEXT        PIC X(61).
           20 EA-KKVABLEHNUNGSGRUND          PIC X(60).
      *****************************************************************
      *****************************************************************
