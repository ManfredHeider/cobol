      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0704.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0704.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:56   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-STARWRITERDOKUMENT-GESAMT.
         10 EA-NI-STARWRITERDOKUMENT-TAB.
           20 EA-NI-STARWRITERDOKUMENT-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-STARWRITERDOKUMENT
                 REDEFINES EA-NI-STARWRITERDOKUMENT-TAB.
           20 EA-NI-STARWRITERDOCID          PIC X.
           20 EA-NI-DOKUMENTART              PIC X.
           20 EA-NI-DOKUMENTDATEI            PIC X.
           20 EA-NI-DOKUMENTTITEL            PIC X.
           20 EA-NI-DOKUMENTBEMERKUNG        PIC X.
         10 EA-DCL-STARWRITERDOKUMENT.
           20 EA-STARWRITERDOCID             PIC X(13).
           20 EA-DOKUMENTART                 PIC X(01).
           20 EA-DOKUMENTDATEI               PIC X(50).
           20 EA-DOKUMENTTITEL               PIC X(128).
           20 EA-DOKUMENTBEMERKUNG.
              49 EA-DOKUMENTBEMERKUNG-LEN    PIC -----9    .
              49 EA-DOKUMENTBEMERKUNG-TEXT   PIC X(3000).
      ******************************************************************
