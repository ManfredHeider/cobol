      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0023.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0023.CPv  $
      *
      *   Rev 2.2   Apr 07 2003 10:12:34   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.1   Apr 16 1999 10:04:26   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HAUSWOHNEIGENTUM-GESAMT.
         10 EA-NI-HAUSWOHNEIGENTUM-TAB.
           20 EA-NI-HAUSWOHNEIGENTUM-ELE PIC X
                  OCCURS 26 TIMES.
         10 EA-NI-HAUSWOHNEIGENTUM
                 REDEFINES EA-NI-HAUSWOHNEIGENTUM-TAB.
           20 EA-NI-HWEPERSONENID            PIC X.
           20 EA-NI-HWEPERSONENROLLE         PIC X.
           20 EA-NI-HWELAUFENDENUMMER        PIC X.
           20 EA-NI-HWEEIGTUMARTID           PIC X.
           20 EA-NI-HWEEIGTUMNARTID          PIC X.
           20 EA-NI-HWEABSCHRFUERANL         PIC X.
           20 EA-NI-HWEBAUKINDERGELD         PIC X.
           20 EA-NI-HWEBAUKINDGWKZ           PIC X.
           20 EA-NI-HWEBAUKINDGELDSEIT       PIC X.
           20 EA-NI-HWEEIGENTUMSEIT          PIC X.
           20 EA-NI-HWEMONNEBENKOSTEN        PIC X.
           20 EA-NI-HWEMONNEBKOSTWKZ         PIC X.
           20 EA-NI-HWEMONERTRAG             PIC X.
           20 EA-NI-HWEMONERTRAGWKZ          PIC X.
           20 EA-NI-HWEOBJEKTWERT            PIC X.
           20 EA-NI-HWEOBJKETWERTWKZ         PIC X.
           20 EA-NI-HWESELBSTGENFLAE         PIC X.
           20 EA-NI-HWESONDAUSGABEN          PIC X.
           20 EA-NI-HWESONDAUSGWKZ           PIC X.
           20 EA-NI-HWEWERBKOSTEN            PIC X.
           20 EA-NI-HWEWERBKOSTENWKZ         PIC X.
           20 EA-NI-HWEEIGENHEIMZULAGE       PIC X.
           20 EA-NI-HWEEIGHEIMZULWKZ         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-HWEKAUFBAUKOSTEN         PIC X.
           20 EA-NI-HWEKAUFBAUKOSTWKZ        PIC X.
         10 EA-DCL-HAUSWOHNEIGENTUM.
           20 EA-HWEPERSONENID               PIC X(13).
           20 EA-HWEPERSONENROLLE            PIC X(1).
           20 EA-HWELAUFENDENUMMER           PIC ----------9.
           20 EA-HWEEIGTUMARTID              PIC X(3).
           20 EA-HWEEIGTUMNARTID             PIC X(3).
           20 EA-HWEABSCHRFUERANL            PIC X(30).
           20 EA-HWEBAUKINDERGELD            PIC ---------9.99.
           20 EA-HWEBAUKINDGWKZ              PIC X(1).
           20 EA-HWEBAUKINDGELDSEIT          PIC X(010).
           20 EA-HWEEIGENTUMSEIT             PIC X(010).
           20 EA-HWEMONNEBENKOSTEN           PIC ---------9.99.
           20 EA-HWEMONNEBKOSTWKZ            PIC X(1).
           20 EA-HWEMONERTRAG                PIC ---------9.99.
           20 EA-HWEMONERTRAGWKZ             PIC X(1).
           20 EA-HWEOBJEKTWERT               PIC ---------9.99.
           20 EA-HWEOBJKETWERTWKZ            PIC X(1).
           20 EA-HWESELBSTGENFLAE            PIC ----9.99.
           20 EA-HWESONDAUSGABEN             PIC ---------9.99.
           20 EA-HWESONDAUSGWKZ              PIC X(1).
           20 EA-HWEWERBKOSTEN               PIC ---------9.99.
           20 EA-HWEWERBKOSTENWKZ            PIC X(1).
           20 EA-HWEEIGENHEIMZULAGE          PIC ---------9.99.
           20 EA-HWEEIGHEIMZULWKZ            PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-HWEKAUFBAUKOSTEN            PIC ---------9.99.
           20 EA-HWEKAUFBAUKOSTWKZ           PIC X(1).
      *****************************************************************
