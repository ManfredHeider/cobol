      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0118.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0118.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 May 1998 16:35:50   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * Ein Array der Partnergesellschaf Tabelle
      *****************************************************************

       01  drki0118-gesamt.

           05  drki0118-pges-list-arr.

               10  drki0118-pges-list-occ        occurs 32.

                     20  drki0118-pges-id PIC S9(4) COMP.
                     20  drki0118-pges-GRUPPE PIC S9(4) COMP.
                     20  drki0118-pges-NAME PIC X(15).
                     20  drki0118-pges-anz-ersetzt pic s9(9) comp.

           05  drki0118-curidx                 pic s9(4) comp.
           05  drki0118-max-pges-list          pic s9(4) comp
                                                value 32.
