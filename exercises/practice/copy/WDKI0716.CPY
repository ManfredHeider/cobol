      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0716.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WDKI0716.CPv  $
      *
      *   Rev 2.4   May 24 2004 10:54:18   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.3   Oct 27 2003 16:20:44   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *   Rev 2.2   Jun 03 2002 09:32:40   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:14:08   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELANFRAGE-GESAMT.
         10 EA-NI-SELANFRAGE-TAB.
           20 EA-NI-SELANFRAGE-ELE PIC X OCCURS 12 TIMES.
         10 EA-NI-SELANFRAGE
                 REDEFINES EA-NI-SELANFRAGE-TAB.
           20 EA-NI-SANID                    PIC X.
           20 EA-NI-SANERSTELLPID            PIC X.
           20 EA-NI-SANERSTELLPROLLE         PIC X.
           20 EA-NI-SANERSTELLDATUM          PIC X.
           20 EA-NI-SANBEZEICHNUNG           PIC X.
           20 EA-NI-SANAUSWERTUNGTYP         PIC X.
           20 EA-NI-SANVERSION               PIC X.
           20 EA-NI-SANSELTYPE               PIC X.
           20 EA-NI-SANSERVICE               PIC X.
           20 EA-NI-SANSERVICENAME           PIC X.
           20 EA-NI-SANOBJECTTYPE            PIC X.
           20 EA-NI-SANORDER                 PIC X.
         10 EA-DCL-SELANFRAGE.
           20 EA-SANID                       PIC X(13).
           20 EA-SANERSTELLPID               PIC X(13).
           20 EA-SANERSTELLPROLLE            PIC X(1).
           20 EA-SANERSTELLDATUM             PIC X(010).
           20 EA-SANBEZEICHNUNG              PIC X(50).
           20 EA-SANAUSWERTUNGTYP.
              49 EA-SANAUSWERTUNGTYP-LEN     PIC -----9.
              49 EA-SANAUSWERTUNGTYP-TEXT    PIC X(30).
           20 EA-SANVERSION                  PIC -----9.
           20 EA-SANSELTYPE                  PIC X(1).
           20 EA-SANSERVICE                  PIC X(80).
           20 EA-SANSERVICENAME              PIC X(80).
           20 EA-SANOBJECTTYPE               PIC X(80).
           20 EA-SANORDER                    PIC ----------9.
      *****************************************************************

