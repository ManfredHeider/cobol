      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0135.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0135.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 May 1998 17:51:12   P13SP99
      *akib0001 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *****************************************************************
      * This is an array of candidate names for client search
      *   used for Aktualisierung.
      *****************************************************************

       01  DRKI0135-gesamt.

           05  DRKI0135-name-list-arr.

               10  DRKI0135-name-list-occ       occurs 30.

                   15 DRKI0135-personenid       pic x(13).
                   15 DRKI0135-personsuchname   pic x(10).
                   15 DRKI0135-personsuchvorname pic x(10).

           05  DRKI0135-curidx                  pic s9(4) comp.
           05  DRKI0135-max-name-list           pic s9(4) comp
                                                value 30.
