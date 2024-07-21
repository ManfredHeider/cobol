      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0331.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0331.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:10:16   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:34   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-LEBENTEILTYP-GESAMT.
         10 EA-NI-LEBENTEILTYP-TAB.
           20 EA-NI-LEBENTEILTYP-ELE PIC X
                  OCCURS 14 TIMES.
         10 EA-NI-LEBENTEILTYP
                 REDEFINES EA-NI-LEBENTEILTYP-TAB.
           20 EA-NI-LTTVERTRAGSID            PIC X.
           20 EA-NI-LTTVERSTARLFDNR          PIC X.
           20 EA-NI-LTTTTNR                  PIC X.
           20 EA-NI-LTTPROVHAFTZVON          PIC X.
           20 EA-NI-LTTPROVHAFTZBIS          PIC X.
           20 EA-NI-LTTABSCHLUSSVERM         PIC X.
           20 EA-NI-LTTDYNPROZ               PIC X.
           20 EA-NI-LTTVERSDAUER             PIC X.
           20 EA-NI-LTTVERTRAGSABLAUF        PIC X.
           20 EA-NI-LTTVERTRAGSBEGINN        PIC X.
           20 EA-NI-LTTBEITRZAHLDAUER        PIC X.
           20 EA-NI-LTTDYNAMIKKZ             PIC X.
           20 EA-NI-LTTDYNLTGRVID            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-LEBENTEILTYP.
           20 EA-LTTVERTRAGSID               PIC X(13).
           20 EA-LTTVERSTARLFDNR             PIC -----9    .
           20 EA-LTTTTNR                     PIC -----9    .
           20 EA-LTTPROVHAFTZVON             PIC X(10).
           20 EA-LTTPROVHAFTZBIS             PIC X(10).
           20 EA-LTTABSCHLUSSVERM            PIC --------9 .
           20 EA-LTTDYNPROZ                  PIC ---9.99          .
           20 EA-LTTVERSDAUER                PIC X(2).
           20 EA-LTTVERTRAGSABLAUF           PIC X(10).
           20 EA-LTTVERTRAGSBEGINN           PIC X(10).
           20 EA-LTTBEITRZAHLDAUER           PIC X(10).
           20 EA-LTTDYNAMIKKZ                PIC X(01).
           20 EA-LTTDYNLTGRVID               PIC -----9    .
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
