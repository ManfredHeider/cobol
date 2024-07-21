      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.07.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0512.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WDKI0512.CPv  $
      *
      *   Rev 2.1   Aug 11 2003 11:13:18   t13ex27
      *AKI00360 - DB-Änderung für Fix 46 und
      *           neues Adressverfahren
      *
      *   Rev 2.0   Jul 30 2003 12:35:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-DFUESTATISTIK-GESAMT.
         10 EA-NI-DFUESTATISTIK-TAB.
           20 EA-NI-DFUESTATISTIK-ELE PIC X OCCURS 13 TIMES.
         10 EA-NI-DFUESTATISTIK
                 REDEFINES EA-NI-DFUESTATISTIK-TAB.
           20 EA-NI-VBID                     PIC X.
           20 EA-NI-VBROLLE                  PIC X.
           20 EA-NI-STARTZEITFTP             PIC X.
           20 EA-NI-ENDEZEITFTP              PIC X.
           20 EA-NI-UEBERTRANZBYTES          PIC X.
           20 EA-NI-PROVIDER                 PIC X.
           20 EA-NI-WORKSTATIONID            PIC X.
           20 EA-NI-KIVERSION                PIC X.
           20 EA-NI-STATUSID                 PIC X.
           20 EA-NI-STARTZEIT                PIC X.
           20 EA-NI-ENDEZEIT                 PIC X.
           20 EA-NI-MAXANZBYTES              PIC X.
           20 EA-NI-UEBERTRDATEI             PIC X.
         10 EA-DCL-DFUESTATISTIK.
           20 EA-VBID                        PIC X(13).
           20 EA-VBROLLE                     PIC X(1).
           20 EA-STARTZEITFTP                PIC X(026).
           20 EA-ENDEZEITFTP                 PIC X(026).
           20 EA-UEBERTRANZBYTES             PIC ----------9.
           20 EA-PROVIDER                    PIC X(32).
           20 EA-WORKSTATIONID               PIC ----------9.
           20 EA-KIVERSION                   PIC X(15).
           20 EA-STATUSID                    PIC -----9.
           20 EA-STARTZEIT                   PIC X(026).
           20 EA-ENDEZEIT                    PIC X(026).
           20 EA-MAXANZBYTES                 PIC ----------9.
           20 EA-UEBERTRDATEI.
              49 EA-UEBERTRDATEI-LEN         PIC -----9.
              49 EA-UEBERTRDATEI-TEXT        PIC X(128).
      *****************************************************************
