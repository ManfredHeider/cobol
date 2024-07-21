      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0139.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0139.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:12   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the zahlungsweise
      *   table.  This array is then passed back to the calling
      *   program.
      *   ...........................................................
       01  DRKI0139-max-zahlungsweise pic s9(4) comp value 20.
       01  DRKI0139-arr-zahlungsweise.
           05  DRKI0139-arr-zahlungsweise-occ occurs 20.
               10  DRKI0139-arr-id pic s9(4) comp.
           05  DRKI0139-cur-zahlungsweise pic s9(4) comp.
