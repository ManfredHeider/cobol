      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0144.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0144.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:08   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the Gefah
      *   table.  This array is then passed back to the calling
      *   program.
      *   ...........................................................
       01  DRKI0144-max-umfang      pic s9(4) comp value 250.
       01  DRKI0144-arr-umfang.
           05  DRKI0144-arr-umfang-occ occurs 250.
               10  DRKI0144-arr-id     pic s9(4) comp.
           05  DRKI0144-cur-umfang     pic s9(4) comp.
