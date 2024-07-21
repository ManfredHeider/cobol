      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0329.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0329.CPv  $
      *
      *   Rev 2.3   Dec 12 2005 07:41:02   t13ex27
      *AKI00533: DB-Änderung für Fix 82 EA-Programme und WDKI's
      *
      *   Rev 2.2   Apr 05 2000 14:02:38   T13AE38
      *AKI00095
      *
      *
      *   Rev 2.1   Apr 16 1999 10:07:48   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:34   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-LEBENTEILLEISTUNG-GESAMT.
         10 EA-NI-LEBENTEILLEISTUNG-TAB.
           20 EA-NI-LEBENTEILLEISTUNG-ELE PIC X OCCURS 12 TIMES.
         10 EA-NI-LEBENTEILLEISTUNG
                 REDEFINES EA-NI-LEBENTEILLEISTUNG-TAB.
           20 EA-NI-LTLVERTRAGSID            PIC X.
           20 EA-NI-LTLVERSTARLFDNR          PIC X.
           20 EA-NI-LTLLAUFENDENR            PIC X.
           20 EA-NI-LTLAUSZAHLBETRAG         PIC X.
           20 EA-NI-LTLAUSZBETRAGWKZ         PIC X.
           20 EA-NI-LTLAUSZAHLZEITPKT        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-LTLVERRENTKAPBETR        PIC X.
           20 EA-NI-LTLVERRENTKAPBWKZ        PIC X.
           20 EA-NI-LTLRENTEGARZEIT          PIC X.
           20 EA-NI-LTLAUSZDECKKAPBETR       PIC X.
           20 EA-NI-LTLAUSZDECKKAPWKZ        PIC X.
         10 EA-DCL-LEBENTEILLEISTUNG.
           20 EA-LTLVERTRAGSID               PIC X(13).
           20 EA-LTLVERSTARLFDNR             PIC -----9.
           20 EA-LTLLAUFENDENR               PIC -----9.
           20 EA-LTLAUSZAHLBETRAG            PIC ---------9.99.
           20 EA-LTLAUSZBETRAGWKZ            PIC X(1).
           20 EA-LTLAUSZAHLZEITPKT           PIC X(010).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-LTLVERRENTKAPBETR           PIC ---------9.99.
           20 EA-LTLVERRENTKAPBWKZ           PIC X(1).
           20 EA-LTLRENTEGARZEIT             PIC -----9.
           20 EA-LTLAUSZDECKKAPBETR          PIC ---------9.99.
           20 EA-LTLAUSZDECKKAPWKZ           PIC X(1).
      *****************************************************************

