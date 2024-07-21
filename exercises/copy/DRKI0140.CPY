      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0140.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0140.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 May 1998 10:00:26   P13SP99
      *aki00001 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:51:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the          eise
      *   Vertragszustand table.  This array is then passed back to the
      *   calling program.
      *   ...........................................................
       01  DRKI0140-max-zustand pic s9(4) comp value 20.
       01  DRKI0140-arr-zustand.
           05  DRKI0140-arr-zustand-occ occurs 20.
               10  DRKI0140-arr-id pic s9(4) comp.
           05  DRKI0140-cur-zustand pic s9(4) comp.
