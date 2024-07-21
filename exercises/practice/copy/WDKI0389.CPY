      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0389.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0389.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:57:02   I13PC19
      * 
      *
      *   Rev 2.0   Apr 16 1999 10:41:42   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATWERTTYP-GESAMT.
         10 EA-NI-FINDATWERTTYP-TAB.
           20 EA-NI-FINDATWERTTYP-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-FINDATWERTTYP
                 REDEFINES EA-NI-FINDATWERTTYP-TAB.
           20 EA-NI-FDWTTYPID                PIC X.
           20 EA-NI-FDWTBEZEICHNUNG          PIC X.
         10 EA-DCL-FINDATWERTTYP.
           20 EA-FDWTTYPID                   PIC -----9    .
           20 EA-FDWTBEZEICHNUNG.
              49 EA-FDWTBEZEICHNUNG-LEN      PIC -----9    .
              49 EA-FDWTBEZEICHNUNG-TEXT     PIC X(100).
      ******************************************************************
