      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0145.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0145.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:12   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Bedingung table. This array is then passed back to the
      *   calling program.
      *   ...........................................................
       01  DRKI0145-max-bedingung   pic s9(4) comp value 100.
       01  DRKI0145-arr-bedingung.
           05  DRKI0145-arr-bedingung-occ occurs 100.
               10  DRKI0145-arr-id     pic s9(4) comp.
           05  DRKI0145-cur-bedingung  pic s9(4) comp.
