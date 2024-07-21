      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0146.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0146.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:14   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the Klausel
      *   table.  This array is then passed back to the calling
      *   program.
      *   ...........................................................
       01  DRKI0146-max-klausel     pic s9(4) comp value 150.
       01  DRKI0146-arr-klausel.
           05  DRKI0146-arr-klausel-occ occurs 150.
               10  DRKI0146-arr-code   pic s9(4) comp.
           05  DRKI0146-cur-klausel    pic s9(4) comp.
