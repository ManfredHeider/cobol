      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0141.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0141.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:14   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the personenid selected
      *   from the verstarifrolle tabelle (by vertragsid and rolle)
      *   table.  This array is then passed back to the calling
      *   program.
      *   ...........................................................
       01  DRKI0141-max-vtrpersonenid pic s9(4) comp value 50.
       01  DRKI0141-arr-vtrpersonenid-all.
           05  DRKI0141-arr-vtrpersonenid-occ occurs 50.
               10  DRKI0141-arr-vtrpersonenid pic x(13).
           05  DRKI0141-cur-vtrpersonenid pic s9(4) comp.
