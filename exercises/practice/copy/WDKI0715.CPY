      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0715.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0715.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:06   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VPDOKUMENT-GESAMT.
         10 EA-NI-VPDOKUMENT-TAB.
           20 EA-NI-VPDOKUMENT-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-VPDOKUMENT
                 REDEFINES EA-NI-VPDOKUMENT-TAB.
           20 EA-NI-VPDOKUMENTID             PIC X.
           20 EA-NI-VPDESCRIPTION            PIC X.
         10 EA-DCL-VPDOKUMENT.
           20 EA-VPDOKUMENTID                PIC X(13).
           20 EA-VPDESCRIPTION.
              49 EA-VPDESCRIPTION-LEN        PIC -----9    .
              49 EA-VPDESCRIPTION-TEXT       PIC X(3900).
      ******************************************************************
