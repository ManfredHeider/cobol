      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0106.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0106.CPv  $
      *
      *   Rev 2.1   Jul 03 2007 16:55:04   t13ex50
      *AKI00666: Einführung in MFE
      *
      *   Rev 2.0   Feb 27 1999 12:52:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:33:58   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:47:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Wagnis table.  This array is then passed back to the
      *   calling program.
      *   ...........................................................
       01  drki0106-max-wagnis-id   pic s9(4) comp value 1500.
       01  drki0106-arr-wagnis-id.
           05  drki0106-arr-wagnis-id-occ occurs 1500.
               10  drki0106-arr-code   pic s9(4) comp.
           05  drki0106-cur-wagnis-id  pic s9(4) comp.
