      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0045.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\WDKI0045.CPv  $
      *
      *   Rev 2.2   04 Sep 2001 14:16:16   t13ex27
      *AKI00219
      *
      *   Rev 2.1   Apr 16 1999 10:04:56   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSPERSBEZIEHUNG-GESAMT.
         10 EA-NI-PERSPERSBEZIEHUNG-TAB.
           20 EA-NI-PERSPERSBEZIEHUNG-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-PERSPERSBEZIEHUNG
                 REDEFINES EA-NI-PERSPERSBEZIEHUNG-TAB.
           20 EA-NI-PPBEZPERSONENID1         PIC X.
           20 EA-NI-PPBEZPERSONENID2         PIC X.
           20 EA-NI-PPBEZBEZART              PIC X.
           20 EA-NI-PPBEZINVBEZART           PIC X.
           20 EA-NI-PPBEZGUELTIGVON          PIC X.
           20 EA-NI-PPBEZGUELTIGBIS          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-PPBEZWERT                PIC X.
         10 EA-DCL-PERSPERSBEZIEHUNG.
           20 EA-PPBEZPERSONENID1            PIC X(13).
           20 EA-PPBEZPERSONENID2            PIC X(13).
           20 EA-PPBEZBEZART                 PIC -----9    .
           20 EA-PPBEZINVBEZART              PIC -----9    .
           20 EA-PPBEZGUELTIGVON             PIC X(10).
           20 EA-PPBEZGUELTIGBIS             PIC X(10).
           20 EA-LETZTEAENDERUNG             PIC X(26).
           20 EA-PPBEZWERT                   PIC ------9.99       .
      ******************************************************************
