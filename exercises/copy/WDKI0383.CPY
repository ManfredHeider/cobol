      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0383.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0383.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:22   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATMAPPING-GESAMT.
         10 EA-NI-FINDATMAPPING-TAB.
           20 EA-NI-FINDATMAPPING-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-FINDATMAPPING
                 REDEFINES EA-NI-FINDATMAPPING-TAB.
           20 EA-NI-FDMFELDID                PIC X.
           20 EA-NI-FDMMAPPINGID             PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATMAPPING.
           20 EA-FDMFELDID                   PIC X(13).
           20 EA-FDMMAPPINGID                PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
