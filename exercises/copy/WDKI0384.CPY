      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0384.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0384.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:26   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATOBJEKTE-GESAMT.
         10 EA-NI-FINDATOBJEKTE-TAB.
           20 EA-NI-FINDATOBJEKTE-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-FINDATOBJEKTE
                 REDEFINES EA-NI-FINDATOBJEKTE-TAB.
           20 EA-NI-FDOMAPPERID              PIC X.
           20 EA-NI-FDOWERT                  PIC X.
           20 EA-NI-FDOASPECT                PIC X.
           20 EA-NI-FDOMETHOD                PIC X.
           20 EA-NI-FDOCLASS                 PIC X.
           20 EA-NI-FDOOBJECTID              PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATOBJEKTE.
           20 EA-FDOMAPPERID                 PIC X(13).
           20 EA-FDOWERT.
              49 EA-FDOWERT-LEN              PIC -----9    .
              49 EA-FDOWERT-TEXT             PIC X(30).
           20 EA-FDOASPECT.
              49 EA-FDOASPECT-LEN            PIC -----9    .
              49 EA-FDOASPECT-TEXT           PIC X(80).
           20 EA-FDOMETHOD.
              49 EA-FDOMETHOD-LEN            PIC -----9    .
              49 EA-FDOMETHOD-TEXT           PIC X(20).
           20 EA-FDOCLASS.
              49 EA-FDOCLASS-LEN             PIC -----9    .
              49 EA-FDOCLASS-TEXT            PIC X(20).
           20 EA-FDOOBJECTID                 PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
