      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0150.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0150.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:22   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Kfzleistungart table.  This array is then passed back to the
      *   calling program.
      *   ...........................................................
       01  DRKI0150-max-kfzleistart-id pic s9(4) comp value 10.
       01  DRKI0150-arr-kfzleistart-id.
           05  DRKI0150-arr-kfzleistart-occ occurs 10.
               10  DRKI0150-arr-code   pic s9(4) comp.
           05  DRKI0150-cur-kfzleistart-id pic s9(4) comp.
