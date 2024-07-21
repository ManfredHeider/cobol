      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0151.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0151.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:24   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Kfzfahrzeugstyp table.  This array is then passed back to the
      *   calling program.
      *   ...........................................................
       01  DRKI0151-max-kfztyp-id pic s9(4) comp value 300.
       01  DRKI0151-arr-kfztyp-id.
           05  DRKI0151-arr-kfztyp-id-occ occurs 300.
               10  DRKI0151-arr-code   pic s9(4) comp.
           05  DRKI0151-cur-kfztyp-id pic s9(4) comp.
