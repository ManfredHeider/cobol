      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0142.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0142.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   19 May 1998 15:15:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.1   19 May 1998 09:34:04   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the Verslei-
      *   stung table.  This array is then passed back to the calling
      *   program.
      *   ...........................................................
       01  DRKI0142-max-versleist        pic s9(4) comp value 33.
       01  DRKI0142-arr-versleist.
           05  DRKI0142-arr-versleist-occ occurs 33.
               10  DRKI0142-arr-id       pic s9(4) comp.
           05  DRKI0142-cur-versleist    pic s9(4) comp.
