      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0117.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0117.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 May 1998 16:35:42   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * Ein Array der Kundennummer Tabelle
      *****************************************************************
      *
       01  drki0117-gesamt.

           05  drki0117-kdnr-list-arr.

               10  drki0117-kdnr-list-occ        occurs 30.

                     15 drki0117-kdnr-GRUPPE PIC S9(4) COMP.
                     15 drki0117-kdnr-ID-AKTUELL PIC S9(9) COMP.
                     15 drki0117-kdnr-ID-BIS PIC S9(9) COMP.
                     15 drki0117-kdnr-ID-aktuell-neu PIC S9(9) COMP.
                     15 drki0117-kdnr-ID-ermittl-von PIC S9(9) COMP.

           05  drki0117-curidx                 pic s9(4) comp.
           05  drki0117-max-kdnr-list          pic s9(4) comp
                                                value 30.
