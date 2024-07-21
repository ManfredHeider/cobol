      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wdki0365.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\wdki0365.cpv  $
      *
      *   Rev 2.2   10 May 2000 08:59:20   T13AE38
      *AKI00106
      *
      *   Rev 2.1   Apr 16 1999 10:11:10   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:50   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00106 !13.04.00!    !-WABWAGNISID IST           !S. JOST   *
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05 EA-WAGNISBEZ-GESAMT.
         10 EA-NI-WAGNISBEZ-TAB.
           20 EA-NI-WAGNISBEZ-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-WAGNISBEZ
                 REDEFINES EA-NI-WAGNISBEZ-TAB.
           20 EA-NI-WABVERTRAGSID            PIC X.
           20 EA-NI-WABVERSTARIFLFDNR        PIC X.
           20 EA-NI-WABHPFLGRZUSATZKZ        PIC X.
           20 EA-NI-WABWAGNISID              PIC X.
           20 EA-NI-WABWAGNISLFDNR           PIC X.
           20 EA-NI-WABWAGNISTEXT            PIC X.
           20 EA-NI-WABWAGNISKZ              PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-WAGNISBEZ.
           20 EA-WABVERTRAGSID               PIC X(13).
           20 EA-WABVERSTARIFLFDNR           PIC -----9    .
           20 EA-WABHPFLGRZUSATZKZ           PIC X(01).
130400*    20 EA-WABWAGNISID                 PIC -----9    .
130400     20 EA-WABWAGNISID                 PIC --------9 .
           20 EA-WABWAGNISLFDNR              PIC -----9    .
           20 EA-WABWAGNISTEXT.
              49 EA-WABWAGNISTEXT-LEN        PIC -----9    .
              49 EA-WABWAGNISTEXT-TEXT       PIC X(25).
           20 EA-WABWAGNISKZ                 PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
