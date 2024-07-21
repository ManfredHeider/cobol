      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0155.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0155.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:24   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Produktschlüssel table.  This array is then passed back
      *   to the calling program.
      *   ...........................................................
       01  DRKI0155-max-prod-schl          pic s9(4) comp value 200.
       01  DRKI0155-arr-prod-schl.
           05  DRKI0155-arr-prod-schl-occ occurs 200.
      *   *Eigentliche Daten (Produktschlüssel-ID)
               10  DRKI0155-arr-id         pic s9(4) comp.
      *   *Laufende Nummer
           05  DRKI0155-cur-prod-schl      pic s9(4) comp.
