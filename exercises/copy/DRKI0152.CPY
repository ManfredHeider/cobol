      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0152.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0152.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:16   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Direktversicherer table.  This array is then passed back
      *   to the calling program.
      *   ...........................................................
       01  DRKI0152-max-direktvers-id      pic s9(4) comp value 6.
       01  DRKI0152-arr-direktvers-id.
           05  DRKI0152-arr-direktvers-id-occ occurs 6.
      *   *Eigentliche Daten (Direktversicherungs-ID)
300398**       10  DRKI0152-arr-id         pic s9(4) comp.
               10  DRKI0152-arr-id         pic x.
      *   *Laufende Nummer
           05  DRKI0152-cur-direktvers-id  pic s9(4) comp.
