      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0033.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0033.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:42   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-MIETE-GESAMT.
         10 EA-NI-MIETE-TAB.
           20 EA-NI-MIETE-ELE PIC X
                  OCCURS 12 TIMES.
         10 EA-NI-MIETE
                 REDEFINES EA-NI-MIETE-TAB.
           20 EA-NI-MIETEPERSONENID          PIC X.
           20 EA-NI-MIETEPERSONENROLLE       PIC X.
           20 EA-NI-MIETELAUFENDENUM         PIC X.
           20 EA-NI-MIETESTEIGERUNG          PIC X.
           20 EA-NI-MIETEMONKALTMIETE        PIC X.
           20 EA-NI-MIETWMONKMIETWKZ         PIC X.
           20 EA-NI-MIETEMONNEBKOSTEN        PIC X.
           20 EA-NI-MIETEMONNEBKWKZ          PIC X.
           20 EA-NI-MIETEWOHNFLAECHE         PIC X.
           20 EA-NI-MIETEWOHNGZUSCH          PIC X.
           20 EA-NI-MIETEWOHNGZWKZ           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-MIETE.
           20 EA-MIETEPERSONENID             PIC X(13).
           20 EA-MIETEPERSONENROLLE          PIC X(01).
           20 EA-MIETELAUFENDENUM            PIC --------9 .
           20 EA-MIETESTEIGERUNG             PIC ---9.99          .
           20 EA-MIETEMONKALTMIETE           PIC ------9.99       .
           20 EA-MIETWMONKMIETWKZ            PIC X(01).
           20 EA-MIETEMONNEBKOSTEN           PIC ------9.99       .
           20 EA-MIETEMONNEBKWKZ             PIC X(01).
           20 EA-MIETEWOHNFLAECHE            PIC ---9.99          .
           20 EA-MIETEWOHNGZUSCH             PIC ------9.99       .
           20 EA-MIETEWOHNGZWKZ              PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
