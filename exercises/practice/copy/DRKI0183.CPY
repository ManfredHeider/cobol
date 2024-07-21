      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0183.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0183.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   02 Jun 1998 14:07:34   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:55:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   ...........................................................
      *   DIES IST EIN BEREICH ZUM ABGLEICH DER SUPPORTTABELLEN
      *   KVTARIFART UND KVGESCHAEFTSST (326 UND 379)
      *   ...........................................................
       01  DRKI0183-MAX-ID               PIC S9(4) COMP VALUE 100.
       01  DRKI0183-ARR-WERTE.
           05  DRKI0183-ARR-WERTE-OCC OCCURS 100.
               10  DRKI0183-ARR-ID       PIC S9(4) COMP.
               10  DRKI0183-CUR-TEXT         PIC X(30).
