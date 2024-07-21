      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0133.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0133.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:34:06   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   This array is used to store all the values of the land
      *   table.  This array is then passed back to the calling
      *   program.   If there will be more than 340 entries, then
      *   DWKI0003 must be modified to increase the DATA-AREA portion
      *   to accomodate more than 1k.
      *   ...........................................................
      *   *Maximale Anzahl an Array-Einträgen
       01  DRKI0133-max-land        pic s9(4) comp value 340.
       01  DRKI0133-arr-land.
           05  DRKI0133-arr-land-occ occurs 340.
      *   *Eigentliche Daten
               10  DRKI0133-arr-landkz pic x(3).
      *   *Korrespondierender Zähler
           05  DRKI0133-cur-land    pic s9(4) comp.
