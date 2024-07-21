      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0154.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0154.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:22   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Beitragszustand table.  This array is then passed back
      *   to the calling program.
      *   ...........................................................
       01  DRKI0154-max-bzustand-id     pic s9(4) comp value 20.
       01  DRKI0154-arr-bzustand-id.
           05  DRKI0154-arr-bzustand-id-occ occurs 20.
      *   *Eigentliche Daten (Beitragszustands-ID)
               10  DRKI0154-arr-id      pic s9(4) comp.
      *   *Laufende Nummer
           05  DRKI0154-cur-bzustand-id pic s9(4) comp.
