      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0721.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0721.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:18   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELFREIGABE-GESAMT.
         10 EA-NI-SELFREIGABE-TAB.
           20 EA-NI-SELFREIGABE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-SELFREIGABE
                 REDEFINES EA-NI-SELFREIGABE-TAB.
           20 EA-NI-SFRANFRAGID              PIC X.
           20 EA-NI-SFRERSTELLDATUM          PIC X.
           20 EA-NI-SFRPARAMFREIGDATUM       PIC X.
         10 EA-DCL-SELFREIGABE.
           20 EA-SFRANFRAGID                 PIC X(13).
           20 EA-SFRERSTELLDATUM             PIC X(10).
           20 EA-SFRPARAMFREIGDATUM          PIC X(10).
      ******************************************************************
