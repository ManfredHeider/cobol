      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0148.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0148.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:18   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Wohngebaeudeart table.This array is then passed back to the
      *   calling program.
      *   ...........................................................
       01  DRKI0148-max-gebart      pic s9(4) comp value 20.
       01  DRKI0148-arr-gebart.
           05  DRKI0148-arr-gebart-occ occurs 20.
               10  DRKI0148-arr-code   pic s9(4) comp.
           05  DRKI0148-cur-gebart     pic s9(4) comp.
