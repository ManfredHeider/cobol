      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0315.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/WDKI0315.CPv  $
      *
      *   Rev 2.8   Oct 12 2006 12:15:40   t13ex27
      *AKI00624: DB-Änderung für Fix 93 EA-Programme
      *
      *   Rev 2.7   Oct 18 2004 09:43:32   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.6   Apr 07 2003 10:12:44   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.5   Dec 16 2002 12:03:32   t13ex27
      *AKI00321 - DB-Änderung für Fixpack 31
      *
      *   Rev 2.4   Oct 15 2002 08:15:52   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.3   Jun 03 2002 09:32:32   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.2   12 Sep 2000 16:21:42   T13EX27
      *AKI00149 DB-Änderung für Fixpack 16
      *
      *   Rev 2.1   Apr 16 1999 10:07:26   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:30   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00308!30.09.02!2.1 ! Fixpack 27                ! IFR      *
      * -------------------------------------------------------------*
      * AKI00321!28.11.02!2.2 ! Fixpack 31                ! IFR      *
      * -------------------------------------------------------------*
      * AKI00624!19.09.06!2.8 ! Fixpack 93                ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05 EA-GUTHABENPRODUKT-GESAMT.
         10 EA-NI-GUTHABENPRODUKT-TAB.
           20 EA-NI-GUTHABENPRODUKT-ELE PIC X OCCURS 33 TIMES.
         10 EA-NI-GUTHABENPRODUKT
                 REDEFINES EA-NI-GUTHABENPRODUKT-TAB.
           20 EA-NI-GHPVERTRAGSID            PIC X.
           20 EA-NI-GHPGUTHABPRODLFDNR       PIC X.
           20 EA-NI-GHPPRODUKTID             PIC X.
           20 EA-NI-GHPANLAGENAME            PIC X.
           20 EA-NI-GHPSCHLAUSZGESCH         PIC X.
           20 EA-NI-GHPSCHLAUSZGWKZ          PIC X.
           20 EA-NI-GHPZINSSATZ              PIC X.
           20 EA-NI-GHPINFOWPERREKZ          PIC X.
           20 EA-NI-GHPINFOSPERREDATUM       PIC X.
           20 EA-NI-GHPVERFUEGUNGID          PIC X.
           20 EA-NI-GHPGUTHABENANT           PIC X.
           20 EA-NI-GHPGUTHABENWERT          PIC X.
           20 EA-NI-GHPGUTHWERTWKZ           PIC X.
           20 EA-NI-GHPGUTHABEN              PIC X.
           20 EA-NI-GHPGUTHABENWKZ           PIC X.
           20 EA-NI-GHPSUBTYPE               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-GHPRISIKOKLASSE          PIC X.
           20 EA-NI-GHPEIGENAUFWAND          PIC X.
           20 EA-NI-GHPFAXREVERS             PIC X.
           20 EA-NI-GHPTELEFONREVERS         PIC X.
           20 EA-NI-GHPONLINEREVERS          PIC X.
           20 EA-NI-GHPZWISCHENGEWINN        PIC X.
           20 EA-NI-GHPZWISCHENGEWWKZ        PIC X.
           20 EA-NI-GHPPERFLFDJAHR           PIC X.
           20 EA-NI-GHPPERFGESAMT            PIC X.
           20 EA-NI-GHPEIGENVERMITTELT       PIC X.
           20 EA-NI-GHPOFFNUNGSDATUM         PIC X.
           20 EA-NI-GHPSCHLIESSDATUM         PIC X.
           20 EA-NI-GHPANLAGEZIEL            PIC X.
           20 EA-NI-GHPABSCHLUSSVBNR         PIC X.
           20 EA-NI-GHPDEPOTART              PIC X.
           20 EA-NI-GHPPERFDAT               PIC X.
         10 EA-DCL-GUTHABENPRODUKT.
           20 EA-GHPVERTRAGSID               PIC X(13).
           20 EA-GHPGUTHABPRODLFDNR          PIC -----9.
           20 EA-GHPPRODUKTID                PIC -----9.
           20 EA-GHPANLAGENAME.
              49 EA-GHPANLAGENAME-LEN        PIC -----9.
              49 EA-GHPANLAGENAME-TEXT       PIC X(15).
           20 EA-GHPSCHLAUSZGESCH            PIC ---------9.99.
           20 EA-GHPSCHLAUSZGWKZ             PIC X(1).
           20 EA-GHPZINSSATZ                 PIC ---9.99.
           20 EA-GHPINFOWPERREKZ             PIC X(1).
           20 EA-GHPINFOSPERREDATUM          PIC X(010).
           20 EA-GHPVERFUEGUNGID             PIC X(2).
           20 EA-GHPGUTHABENANT              PIC ------9.999.
           20 EA-GHPGUTHABENWERT             PIC ---------9.99.
           20 EA-GHPGUTHWERTWKZ              PIC X(1).
           20 EA-GHPGUTHABEN                 PIC ---------9.99.
           20 EA-GHPGUTHABENWKZ              PIC X(1).
           20 EA-GHPSUBTYPE                  PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-GHPRISIKOKLASSE             PIC -----9.
           20 EA-GHPEIGENAUFWAND             PIC ---------9.99.
           20 EA-GHPFAXREVERS                PIC X(1).
           20 EA-GHPTELEFONREVERS            PIC X(1).
           20 EA-GHPONLINEREVERS             PIC X(1).
           20 EA-GHPZWISCHENGEWINN           PIC ---------9.99.
           20 EA-GHPZWISCHENGEWWKZ           PIC X(1).
           20 EA-GHPPERFLFDJAHR              PIC ------------9.99999.
           20 EA-GHPPERFGESAMT               PIC ------------9.99999.
           20 EA-GHPEIGENVERMITTELT          PIC X(1).
           20 EA-GHPOFFNUNGSDATUM            PIC X(010).
           20 EA-GHPSCHLIESSDATUM            PIC X(010).
           20 EA-GHPANLAGEZIEL               PIC X(1).
           20 EA-GHPABSCHLUSSVBNR            PIC ----------9.
           20 EA-GHPDEPOTART                 PIC X(1).
           20 EA-GHPPERFDAT                  PIC X(010).
      *****************************************************************
