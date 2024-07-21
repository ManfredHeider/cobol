      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0344.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\WDKI0344.CPv  $
      *
      *   Rev 2.3   12 May 2000 10:53:08   T13AE38
      *AKI00095
      *
      *   Rev 2.2   10 May 2000 08:59:14   T13AE38
      *AKI00106
      *
      *   Rev 2.1   Apr 16 1999 10:10:30   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:40   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00106 !13.04.00!    !-VBBVERSBEDINGUNGID        !S. JOST   *
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       05 EA-VERSBEDINGUNGBEZ-GESAMT.
         10 EA-NI-VERSBEDINGUNGBEZ-TAB.
           20 EA-NI-VERSBEDINGUNGBEZ-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-VERSBEDINGUNGBEZ
                 REDEFINES EA-NI-VERSBEDINGUNGBEZ-TAB.
           20 EA-NI-VBBVERTRAGSID            PIC X.
           20 EA-NI-VBBVERSTARLFDNR          PIC X.
           20 EA-NI-VBBVERSBEDINGUNGID       PIC X.
           20 EA-NI-VBBVERSBEDINGLFDNR       PIC X.
           20 EA-NI-VBBVEBPARTNERGES         PIC X.
           20 EA-NI-VBBVERSBEDINGUNG         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERSBEDINGUNGBEZ.
           20 EA-VBBVERTRAGSID               PIC X(13).
           20 EA-VBBVERSTARLFDNR             PIC -----9    .
130400*    20 EA-VBBVERSBEDINGUNGID          PIC -----9    .
130400     20 EA-VBBVERSBEDINGUNGID          PIC --------9 .
           20 EA-VBBVERSBEDINGLFDNR          PIC -----9    .
           20 EA-VBBVEBPARTNERGES            PIC -----9    .
           20 EA-VBBVERSBEDINGUNG.
              49 EA-VBBVERSBEDINGUNG-LEN     PIC -----9    .
              49 EA-VBBVERSBEDINGUNG-TEXT    PIC X(10).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
