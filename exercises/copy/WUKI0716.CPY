      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0716.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/WUKI0716.CPv  $
      *
      *   Rev 2.8   May 24 2004 11:45:54   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.7   Feb 02 2004 08:38:00   t13ex27
      *AKI00373 - DB-Änderung FIX 54
      *
      *   Rev 2.6   Jun 11 2003 13:14:30   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-SELANFRAGE.
           10 SANID                       PIC X(13).
           10 SANERSTELLPID               PIC X(13).
           10 SANERSTELLPROLLE            PIC X(1).
           10 SANERSTELLDATUM             PIC X(010).
           10 SANBEZEICHNUNG              PIC X(50).
           10 SANAUSWERTUNGTYP.
              49 SANAUSWERTUNGTYP-LEN          PIC S9(4) COMP.
              49 SANAUSWERTUNGTYP-TEXT         PIC X(30).
           10 SANVERSION                  PIC S9(4) COMP.
           10 SANSELTYPE                  PIC X(1).
           10 NIL-SANSELTYPE              PIC X(01).
           10 SANSERVICE                  PIC X(80).
           10 SANSERVICENAME              PIC X(80).
           10 SANOBJECTTYPE               PIC X(80).
           10 SANORDER                    PIC S9(9) COMP.
           10 NIL-SANORDER                PIC X(01).
      *****************************************************************
