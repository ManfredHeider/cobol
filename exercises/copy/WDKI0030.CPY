      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0030.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:38   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KOMMUNIKATION-GESAMT.
         10 EA-NI-KOMMUNIKATION-TAB.
           20 EA-NI-KOMMUNIKATION-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-KOMMUNIKATION
                 REDEFINES EA-NI-KOMMUNIKATION-TAB.
           20 EA-NI-KOMMPERSONENID           PIC X.
           20 EA-NI-KOMMTELARTID             PIC X.
           20 EA-NI-KOMMLAUFENDENUMMER       PIC X.
           20 EA-NI-KOMMNUMMER               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KOMMUNIKATION.
           20 EA-KOMMPERSONENID              PIC X(13).
           20 EA-KOMMTELARTID                PIC -----9    .
           20 EA-KOMMLAUFENDENUMMER          PIC --------9 .
           20 EA-KOMMNUMMER.
              49 EA-KOMMNUMMER-LEN           PIC -----9    .
              49 EA-KOMMNUMMER-TEXT          PIC X(128).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
