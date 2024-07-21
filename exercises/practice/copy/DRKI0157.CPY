      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0157.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0157.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   26 Aug 1998 14:05:50   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   19 May 1998 15:15:48   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
240898*  24.08.98 MTA : maxwert auf 200 vergrößert
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the ID-values of the
      *   Vertragsart table.  This array is then passed back
      *   to the calling program.
      *   ...........................................................
240898 01  DRKI0157-max-vertragsart        pic s9(4) comp value 200.
       01  DRKI0157-arr-vertragsart.
240898     05  DRKI0157-arr-vertragsart-occ occurs 200.
      *   *Eigentliche Daten (Vertragsart-ID)
               10  DRKI0157-arr-id         pic s9(4) comp.
      *   *Laufende Nummer
           05  DRKI0157-cur-vertragsart    pic s9(4) comp.
