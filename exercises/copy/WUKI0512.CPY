      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0512.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0512.CPv  $
      *
      *   Rev 2.1   Sep 08 2003 16:29:20   t13ex27
      *AKI00360 - DB-Änderung für Fix 46
      *
      *   Rev 2.0   Aug 11 2003 10:05:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-DFUESTATISTIK.
           10 VBID                        PIC X(13).
           10 VBROLLE                     PIC X(1).
           10 STARTZEITFTP                PIC X(026).
           10 ENDEZEITFTP                 PIC X(026).
           10 UEBERTRANZBYTES             PIC S9(9) COMP.
           10 PROVIDER                    PIC X(32).
           10 WORKSTATIONID               PIC S9(9) COMP.
           10 NIL-WORKSTATIONID           PIC X(01).
           10 KIVERSION                   PIC X(15).
           10 STATUSID                    PIC S9(4) COMP.
           10 STARTZEIT                   PIC X(026).
           10 ENDEZEIT                    PIC X(026).
           10 MAXANZBYTES                 PIC S9(9) COMP.
           10 UEBERTRDATEI.
              49 UEBERTRDATEI-LEN              PIC S9(4) COMP.
              49 UEBERTRDATEI-TEXT             PIC X(128).
      *****************************************************************
