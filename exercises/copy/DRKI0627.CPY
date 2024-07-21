      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0627.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0627.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   22 Sep 1998 12:12:56   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   18 Sep 1998 11:15:06   T13EX20
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   Dies Array enthält 498 SmallintEinträge, die für IDs
      *   einer Supporttabelle.  Dies Array wird zum Aufrufenden
      *   Programm weitergegeben.                (Insgesamt 1000 bytes)
      *                                           MTA 18.09.98
      *   ...........................................................
       01  DRKI0627-max-id                 pic s9(4) comp value 498.
       01  DRKI0627-arr-werte.
           05  DRKI0627-arr-werte-occ                    occurs 498.
      *   *Eigentliche Daten
               10  DRKI0627-arr-id         pic s9(4) comp.
      *   *Laufende Nummer
           05  DRKI0627-cur-id             pic s9(4) comp.
