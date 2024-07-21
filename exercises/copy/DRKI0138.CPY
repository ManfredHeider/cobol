      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0138.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0138.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:08   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the Zahlart
      *   table.  This array is then passed back to the calling
      *   program.
      *   ...........................................................
       01  DRKI0138-max-zahlart     pic s9(4) comp value 20.
       01  DRKI0138-arr-zahlart.
           05  DRKI0138-arr-zahlart-occ occurs 20.
               10  DRKI0138-arr-id pic s9(4) comp.
           05  DRKI0138-cur-zahlart    pic s9(4) comp.
